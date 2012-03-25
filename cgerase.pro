; docformat = 'rst'
;
; NAME:
;   cgErase
;
; PURPOSE:
;   Provides a device-independent and color-model-independent way to erase a graphics
;   window with a particular color.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc. All rights reserved.           ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
;
;+
;   Provides a device-independent and color-model-independent way to erase a graphics
;   window with a particular color.
;
; :Categories:
;    Graphics
;    
; :Params:
;    background_color: in, optional, type=string/integer/long, default='white'
;         The color to use in erasing the graphics window. Default is "white."
;         Color names are those used with cgColor.
;       
; :Keywords:
;     color: in, optional, type=string/integer/long, default='white'
;         An alternative way to specify the color to use in erasing the graphics window.
;         Color names are those used with cgColor. This parameter is used in
;         preference to the background_color parameter.
;     layout: in, optional, type=intarr(3)
;         This keyword specifies a grid with a graphics window and determines where the
;         graphic should appear. The syntax of LAYOUT is three numbers: [ncolumns, nrows, location].
;         The grid is determined by the number of columns (ncolumns) by the number of 
;         rows (nrows). The location of the graphic is determined by the third number. The
;         grid numbering starts in the upper left (1) and goes sequentually by column and then
;         by row. If this keyword is used, only this portion of the window is erased.
;     window: in, optional, type=boolean, default=0
;         Set this keyword to erase the current cgWindow application. "Erasing" in
;         this case means removing all the current commands.
;          
; :Examples:
;    Used to "erase" various things::
;       IDL> cgErase
;       IDL> cgErase, 'gray'
;       IDL> cgErase, COLOR='charcoal'
;       
;       IDL> cgPlot, cgDemoData(1), /Window
;       IDL> cgErase, /Window
;       
;       IDL> cgPlot, cgDemoData(17), Layout=[2,2,1]
;       IDL> cgPlot, cgDemoData(17), Layout=[2,2,4]
;       IDL> cgErase, Layout=[2,2,1]
;       
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: david@idlcoyote.com
;           Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 12 November 2010. DWF.
;        Modified so that input variables are not changed. 18 Nov 2010. DWF.
;        Got my color selection algorithm right. COLOR keyword takes precedence
;          over the parameter. 18 Nov 2010. DWF.
;        Modified to erase in decomposed color, if possible.
;        In some cases, I was turning BYTE values to strings without converting to 
;            INTEGERS first. 30 Dec 2010. DWF.   
;        Added WINDOW keyword. 26 Jan 2011. DWF. 
;        Added LAYOUT keyword. 1 Feb 2011. DWF.    
;        Modified error handler to restore the entry decomposition state if there is an error. 17 March 2011. DWF
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO cgErase, background_color, COLOR=color, LAYOUT=layout, WINDOW=window

    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /Cancel
       IF N_Elements(currentState) NE 0 THEN SetDecomposedState, currentState
       RETURN
    ENDIF

    ; Are we erasing an cgWindow application?
    IF Keyword_Set(window) THEN BEGIN
    
        currentID = cgQuery(COUNT=wincnt, OBJECTREF=currentObj, /Current)
        IF wincnt GT 0 THEN BEGIN
            currentObj -> DeleteCommand, /All
            currentObj -> ExecuteCommands
        ENDIF
        RETURN
        
    ENDIF

    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Get a color for erasing.
    IF N_Elements(background_color) EQ 0 THEN thisColor = 'white' ELSE thisColor = background_color
    IF N_Elements(color) NE 0 THEN thisColor = color 
    IF Size(thisColor, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN thisColor = Byte(thisColor)
    IF Size(thisColor, /TYPE) LE 2 THEN thisColor = StrTrim(Fix(thisColor),2)

    ; Get the current color vectors.
    TVLCT, rr, gg, bb, /Get
    
    ; Do this in decomposed color, if possible.
    SetDecomposedState, 1, CURRENTSTATE=currentState
    
    IF Size(thisColor, /TNAME) EQ 'STRING' THEN thisColor = cgColor(thisColor)
    
    ; Set up the layout, if necessary.
    IF N_Elements(layout) NE 0 THEN BEGIN
       thisMulti = !P.Multi
       totalPlots = layout[0]*layout[1]
       !P.Multi = [0,layout[0], layout[1], 0, 0]
       IF layout[2] EQ 1 THEN BEGIN
            noerase = 1
            !P.Multi[0] = 0
       ENDIF ELSE BEGIN
            !P.Multi[0] = totalPlots - layout[2] + 1
       ENDELSE
       
       ; Draw an invisible plot in this space.
       Plot, [0,1], XStyle=4, YStyle=4, /NoErase
       
       ; Fill the plot area with the color.
       x = !X.Region
       y = !Y.Region
       PolyFill, [x[0], x[0], x[1], x[1]], $
                 [y[0], y[1], y[1], y[0]], /FIll, $
                 Color=thisColor, /Normal
       !P.Multi = thisMulti
    ENDIF ELSE Erase, thisColor

    
    ; Clean up.
    SetDecomposedState, currentState
    IF !D.Name NE 'Z' THEN TVLCT, rr, gg, bb
   
END
