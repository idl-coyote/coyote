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
;     nodata: in, optional, type=boolian, default=0
;         Set this keyword to draw axes, but no data.
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
;        
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO FSC_Plot, x, y, $
    ASPECT=aspect, $
    AXISCOLOR=axiscolor, $
    AXESCOLOR=axescolor, $
    BACKGROUND=background, $
    COLOR=color, $
    ISOTROPIC=isotropic, $
    NODATA=nodata, $
    OVERPLOT=overplot, $
    POSITION=position, $
    PSYM=psym, $
    SYMCOLOR=symcolor, $
    _Extra=extra
    
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
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
    IF N_Elements(background) EQ 0 THEN background = 'white'
    IF (N_Elements(axescolor) EQ 0) AND (N_Elements(axiscolor) EQ 0) THEN BEGIN
       axiscolor = 'black'
    ENDIF
    IF N_Elements(axescolor) NE 0 THEN axiscolor = axescolor
    IF N_Elements(color) EQ 0 THEN color = 'black'
    IF N_Elements(symcolor) EQ 0 THEN symcolor = 'black'
    IF Keyword_Set(isotropic) THEN aspect = 1.0
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
    
    ; Going to have to do all of this in indexed color.
    SetDecomposedState, 0, CURRENTSTATE=currentState
    
    ; Draw the plot.
    IF Keyword_Set(overplot) THEN BEGIN
       OPLOT, indep, dep, COLOR=color, _STRICT_EXTRA=extra
    ENDIF ELSE BEGIN
        Plot, indep, dep, BACKGROUND=background, COLOR=axiscolor, $
            POSITION=position, /NODATA, _STRICT_EXTRA=extra
        IF PSYM LE 0 THEN BEGIN
            IF ~Keyword_Set(nodata) THEN OPLOT, indep, dep, COLOR=color, _EXTRA=extra  
        ENDIF  
        IF Abs(psym) GT 0 THEN BEGIN
            IF ~Keyword_Set(nodata) THEN OPLOT, indep, dep, COLOR=symcolor, $
                   PSYM=SymCat(Abs(psym)), _EXTRA=extra
        ENDIF 
    ENDELSE
         
    ; Restore the decomposed color state if you can.
    IF currentState THEN SetDecomposedState, 1
    
    ; Restore the color table.
    TVLCT, rr, gg, bb
    
END
    