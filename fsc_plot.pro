; docformat = 'rst'
;
; NAME:
;   FSC_Plot
;
; PURPOSE:
;   The purpose of FSC_Plot is to create a wrapper for the traditional IDL graphics
;   command, Plot. The primary purpose of this is to create plot commands that work
;   and look identically both on the display and in PostScript files.
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
; :Description:
;   The purpose of FSC_Plot is to create a wrapper for the traditional IDL graphics
;   command, Plot. The primary purpose of this is to create plot commands that work
;   and look identically both on the display and in PostScript files.
;
; :Categories:
;    Graphics
;    
; :Params:
;    x: in, required, type=any
;         If X is provided without Y, a vector representing the dependent values to be 
;         plotted If both X and Y are provided, X is the independent parameter and 
;         Y is the dependent parameter to be plotted.
;    y: in, optional, type=any
;         A vector representing the dependent values to be plotted.
;       
; :Keywords:
;     aspect: in, optional, type=float, default=none
;        Set this keyword to a floating point ratio that represents the aspect ratio 
;        (ysize/xsize) of the resulting plot. The plot position may change as a result
;        of setting this keyword. Note that ASPECT cannot be used when plotting with
;        !P.MULTI.
;     axiscolor: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the axis color. By default, 'black'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     axescolor: in, optional, type=string/integer
;        Provisions for bad spellers.
;     background: in, optional, type=string/integer, default='white'
;        If this keyword is a string, the name of the background color. By default, 'white'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     color: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the data color. By default, 'black'.
;        Color names are those used with FSC_Color. Otherwise, the keyword is assumed 
;        to be a color index into the current color table.
;     isotropic: in, optional, type=boolean, default=0
;         A short-hand way of setting the ASPECT keyword to 1.
;     nodata: in, optional, type=boolean, default=0
;         Set this keyword to draw axes, but no data.
;     noerase: in, optional, type=boolean, default=0
;         Set this keyword to draw the plot without erasing the display first.
;     overplot: in, optional, type=boolean, default=0
;         Set this keyword if you wish to overplot data on an already exisiting set of
;         axes. It is like calling the IDL OPLOT command.
;     position: in, optional, type=vector
;         The usual four-element position vector for the Plot comamnd. Only monitored and
;         possibly set if the ASPECT keyword is used.
;     psym: in, optional, type=integer
;         Any normal IDL PSYM values, plus and value supported by the Coyote Library
;         routine SYMCAT. An integer between 0 and 46.
;     symcolor: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the symbol color. By default, 'black'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     _extra: in, optional, type=any
;        Any keyword appropriate for the IDL Plot command is allowed in the program.
;
; :Examples:
;    Use as you would use the IDL PLOT command::
;       FSC_Plot, Findgen(11)
;       FSC_Plot, Findgen(11), Aspect=1.0
;       FSC_Plot, Findgen(11), Color='olive', AxisColor='red', Thick=2
;       FSC_Plot, Findgen(11), Color='blue', SymColor='red', PSym=-16
;       
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: davidf@dfanning.com
;           Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; :History:
;     Change History::
;        Written, 12 November 2010. DWF.
;        Added SYMCOLOR keyword, and allow all 46 symbols from SYMCAT. 15 November 2010. DWF.
;        Added NODATA keyword. 15 November 2010. DWF.
;        Now setting decomposition state by calling SetDecomposedState. 16 November 2010. DWF.
;        Final color table restoration skipped in Z-graphics buffer. 17 November 2010. DWF.
;        Fixed a problem with overplotting with symbols. 17 November 2010. DWF.
;        Background keyword now applies in PostScript file as well. 17 November 2010. DWF.
;        Many changes after BACKGROUND changes to get !P.MULTI working again! 18 November 2010. DWF.
;        Fixed a small problem with the OVERPLOT keyword. 18 Nov 2010. DWF.
;        Changes so that color inputs don't change type. 23 Nov 2010. DWF.
;        
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO FSC_Plot, x, y, $
    ASPECT=aspect, $
    AXISCOLOR=saxiscolor, $
    AXESCOLOR=saxescolor, $
    BACKGROUND=sbackground, $
    COLOR=scolor, $
    ISOTROPIC=isotropic, $
    NODATA=nodata, $
    NOERASE=noerase, $
    OVERPLOT=overplot, $
    POSITION=position, $
    PSYM=psym, $
    SYMCOLOR=ssymcolor, $
    _Extra=extra
    
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Check parameters.
    IF N_Params() EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: FSC_Plot, x, y'
        RETURN
    ENDIF
    
    ; Sort out which is the dependent and which is independent data.
    CASE N_Params() OF
      
       1: BEGIN
       dep = x
       indep = Findgen(N_Elements(dep))
       ENDCASE
    
       2: BEGIN
       dep = y
       indep = x
       ENDCASE
    
    ENDCASE
   
    ; Get the current color table vectors.
    TVLCT, rr, gg, bb, /GET
    
    ; Check the keywords.
    IF N_Elements(sbackground) EQ 0 THEN background = 'WHITE' ELSE background = sbackground
    
    ; Choose an axis color.
    IF N_Elements(saxisColor) EQ 0 AND N_Elements(saxescolor) NE 0 THEN saxiscolor = saxescolor
    IF N_Elements(saxiscolor) EQ 0 THEN BEGIN
       IF (Size(background, /TNAME) EQ 'STRING') && (StrUpCase(background) EQ 'WHITE') THEN BEGIN
            IF !P.Multi[0] EQ 0 THEN saxisColor = 'BLACK'
       ENDIF
       IF N_Elements(saxiscolor) EQ 0 THEN BEGIN
           IF !D.Name EQ 'PS' THEN BEGIN
                saxisColor = 'BLACK' 
           ENDIF ELSE BEGIN
                IF (!D.Window GE 0) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
                    pixel = TVRead(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF Total(pixel) EQ 765 THEN saxisColor = 'BLACK'
                    IF Total(pixel) EQ 0 THEN saxisColor = 'WHITE'
                    IF N_Elements(saxisColor) EQ 0 THEN saxisColor = 'OPPOSITE'
                ENDIF ELSE saxisColor = 'OPPOSITE'
           ENDELSE
       ENDIF
    ENDIF
    IF N_Elements(saxisColor) EQ 0 THEN axisColor = !P.Color ELSE axisColor = saxisColor
    
    ; Choose a color.
    IF N_Elements(sColor) EQ 0 THEN BEGIN
       IF (Size(background, /TNAME) EQ 'STRING') && (StrUpCase(background) EQ 'WHITE') THEN BEGIN
            IF !P.Multi[0] EQ 0 THEN sColor = 'BLACK'
       ENDIF
       IF N_Elements(sColor) EQ 0 THEN BEGIN
           IF !D.Name EQ 'PS' THEN BEGIN
                sColor = 'BLACK' 
           ENDIF ELSE BEGIN
                IF (!D.Window GE 0) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
                    pixel = TVRead(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF Total(pixel) EQ 765 THEN sColor = 'BLACK'
                    IF Total(pixel) EQ 0 THEN sColor = 'WHITE'
                    IF N_Elements(sColor) EQ 0 THEN sColor = 'OPPOSITE'
                ENDIF ELSE sColor = 'OPPOSITE'
           ENDELSE
       ENDIF
    ENDIF
    IF N_Elements(sColor) EQ 0 THEN color = !P.Color ELSE  color = sColor
    
    IF N_Elements(ssymcolor) EQ 0 THEN symcolor = color ELSE symcolor = ssymcolor
    IF Keyword_Set(isotropic) THEN aspect = 1.0
    noerase = Keyword_Set(noerase)
    IF N_Elements(psym) EQ 0 THEN psym = 0
    IF (N_Elements(aspect) NE 0) AND (Total(!P.MULTI) EQ 0) THEN BEGIN
        position = Aspect(aspect)
    ENDIF
           
     ; Load the drawing colors, if needed.
    IF Size(axiscolor, /TNAME) EQ 'STRING' THEN $
        axiscolor = FSC_Color(axiscolor, DECOMPOSED=0, 254)
    IF Size(color, /TNAME) EQ 'STRING' THEN $
        color = FSC_Color(color, DECOMPOSED=0, 253)
    IF Size(background, /TNAME) EQ 'STRING' THEN $
        background = FSC_Color(background, DECOMPOSED=0, 252)
    IF Size(symcolor, /TNAME) EQ 'STRING' THEN $
        symcolor = FSC_Color(symcolor, DECOMPOSED=0, 251)
    
    ; Do you need a PostScript background color? Lot's of problems here!
    ; Basically, I MUST draw a plot to advance !P.MULTI. But, drawing a
    ; plot of any sort erases the background color. So, I have to draw a 
    ; plot, store the new system variables, then draw my background, etc.
    ; I have tried LOTS of options. This is the only one that worked.
    IF !D.Name EQ 'PS' THEN BEGIN
       IF ~noerase THEN BEGIN
       
           ; I only have to do this, if this is the first plot.
           IF !P.MULTI[0] EQ 0 THEN BEGIN
           
                IF Keyword_Set(overplot) NE 1 THEN BEGIN
                
                    ; Save the current system variables. Will need to restore later.
                    bangx = !X
                    bangy = !Y
                    bangp = !P
                    
                    ; Draw the plot that doesn't draw anything.
                    Plot, indep, dep, POSITION=position, /NODATA, _STRICT_EXTRA=extra  
                    
                    ; Save the "after plot" system variables. Will use later. 
                    afterx = !X
                    aftery = !Y
                    afterp = !P     
                    
                    ; Draw the background color and set the variables you will need later.
                    PS_Background, background
                    psnodraw = 1
                    tempNoErase = 1
                    
                    ; Restore the original system variables so that it is as if you didn't
                    ; draw the invisible plot.
                    !X = bangx
                    !Y = bangy
                    !P = bangp
                
                ENDIF
            ENDIF ELSE tempNoErase = noerase
        ENDIF ELSE tempNoErase = noerase
     ENDIF ELSE tempNoErase = noerase
 
     ; Going to have to do all of this in indexed color.
    SetDecomposedState, 0, CURRENTSTATE=currentState
    
    ; Draw the plot.
    IF Keyword_Set(overplot) THEN BEGIN
       IF psym LE 0 THEN OPlot, indep, dep, COLOR=color, _STRICT_EXTRA=extra
    ENDIF ELSE BEGIN
      Plot, indep, dep, BACKGROUND=background, COLOR=axiscolor, $
            POSITION=position, /NODATA, NOERASE=tempNoErase, _STRICT_EXTRA=extra
        IF psym LE 0 THEN BEGIN
           IF ~Keyword_Set(nodata) THEN OPlot, indep, dep, COLOR=color, _EXTRA=extra  
        ENDIF  
    ENDELSE
    IF Abs(psym) GT 0 THEN BEGIN
        IF ~Keyword_Set(nodata) THEN OPlot, indep, dep, COLOR=symcolor, $
            PSYM=SymCat(Abs(psym)), _EXTRA=extra
    ENDIF 
         
    ; If this is the first plot in PS, then we have to make it appear that we have
    ; drawn a plot, even though we haven't.
    IF N_Elements(psnodraw) EQ 1 THEN BEGIN
        !X = afterX
        !Y = afterY
        !P = afterP
    ENDIF

    ; Restore the decomposed color state if you can.
    IF currentState THEN SetDecomposedState, 1
    
    ; Restore the color table. Can't do this for the Z-buffer or
    ; the snap shot will be incorrect.
    IF (!D.Name NE 'Z') THEN TVLCT, rr, gg, bb
    
END
    