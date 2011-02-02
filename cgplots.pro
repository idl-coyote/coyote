; docformat = 'rst'
;
; NAME:
;   cgPlotS
;
; PURPOSE:
;   The purpose of cgPlotS is to create a wrapper for the traditional IDL graphics
;   command, PlotS. The primary purpose of this is to create plot commands that work
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
;   The purpose of cgPlotS is to create a wrapper for the traditional IDL graphics
;   command, PlotS. The primary purpose of this is to create plot commands that work
;   and look identically both on the display and in PostScript files.
;
; :Categories:
;    Graphics
;    
; :Params:
;    X: in, required, type=any
;         A vector or scalar argument providing the X components of the points to be
;         drawn or connected. May be a 2xN or 3xN array, if Y and Z parameters are
;         not used.
;    Y: in, optional, type=any
;         A vector or scalar argument providing the Y components of the points to be
;         drawn or connected.
;    Z: in, optional, type=any
;         A vector or scalar argument providing the Z components of the points to be
;         drawn or connected.
;         
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        Set this keyword to add the command to an cgWindow display.
;     color: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the data color. By default, 'black'.
;        Color names are those used with cgColor. Otherwise, the keyword is assumed 
;        to be a color index into the current color table. May be a vector of the same
;        length as X.
;     psym: in, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine SYMCAT. An integer between 0 and 46. 
;     symcolor: in, optional, type=string/integer/vector, default=COLOR
;        If this keyword is a string, the name of the symbol color. By default, same as COLOR.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;        May be a vector of the same length as X.
;     symsize: in, optional, type=float/vector, default=1.0
;        A scalar or vector of symbol sizes. Default is 1.0. May be a vector of the same 
;        length as X.
;     window: in, optional, type=boolean, default=0
;         Set this keyword to add the command to the current cgWindow application.
;     _extra: in, optional, type=any
;        Any keywords supported by the PLOTS command are allowed.
;         
; :Examples:
;    Use like the IDL PLOTS command::
;       IDL> cgPlot, Findgen(11)
;       IDL> cgPlotS, !X.CRange, [5,5], LINESTYLE=2, THICK=2, COLOR='red'
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
;        Added SYMCOLOR keyword. PSYM accepts all values from SYMCAT. SYMCOLOR and SYMSIZE
;           keywords can be vectors the size of x. 15 November 2010. DWF
;        Added ability to support COLOR keyword as a vector the size of x. 15 November 2010. DWF
;        Now setting decomposition state by calling SetDecomposedState. 16 November 2010. DWF.
;        Final color table restoration skipped in Z-graphics buffer. 17 November 2010. DWF.
;        Changes so that color variables don't change type. 23 Nov 2010. DWF.
;        Modified to use decomposed color, if possible. 24 Dec 2010. DWF.
;        Whoops! Programming is like herding cats! 29 Dec 2010. DWF.
;        In some cases, I was turning BYTE values to strings without converting to 
;            INTEGERS first. 30 Dec 2010. DWF.        
;        Moved setting to decomposed color before color selection process to avoid PostScript
;             background problems when passed 24-bit color integers. 12 Jan 2011. DWF.  
;        Added WINDOW keyword. 24 Jan 2011. DWF. 
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO cgPlotS, x, y, z, $
    ADDCMD=addcmd, $
    COLOR=scolor, $
    PSYM=psym, $
    SYMCOLOR=ssymcolor, $
    SYMSIZE=symsize, $
    WINDOW=window, $
    _EXTRA=extra

    Compile_Opt idl2

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF Keyword_Set(dataSwitch) THEN x = Temporar(y)
        RETURN
    ENDIF
    
    ; Did the user pass parameters?
    IF N_Params() EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: cgPlotS, x, y, [z]'
        RETURN
    ENDIF
    
    ; Should this be added to a resizeable graphics window?
    IF (Keyword_Set(window) OR Keyword_Set(addcmd)) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        cgWindow, 'cgPlotS', x, y, z, $
            COLOR=scolor, $
            PSYM=psym, $
            SYMCOLOR=ssymcolor, $
            SYMSIZE=symsize, $
            ADDCMD=1, $
            _EXTRA=extra
            
         RETURN
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; The PLOTS command requires at least two positional parameters.
    ; If we only have one, then we will fake it, as we do in a line
    ; plot.
    n_params = N_Params()
    IF (n_params EQ 1) && Size(x, /N_DIMENSIONS) EQ 1 THEN BEGIN
        temp = x
        x = Indgen(N_Elements(x))
        y = Temporary(temp)
        n_params = 2
        dataSwitch = 1
    ENDIF
    
    ; Going to draw the lines in decomposed color, if possible
    SetDecomposedState, 1, CurrentState=currentState
    
    ; Choose a color.
    IF N_Elements(sColor) EQ 0 THEN BEGIN
           IF !D.Name EQ 'PS' THEN BEGIN
                sColor = 'OPPOSITE' 
           ENDIF ELSE BEGIN
                IF (!D.Window GE 0) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
                    pixel = TVRead(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF (Total(pixel) EQ 765) THEN sColor = 'BLACK'
                    IF (Total(pixel) EQ 0) THEN sColor = 'WHITE'
                    IF N_Elements(sColor) EQ 0 THEN sColor = 'OPPOSITE'
                ENDIF ELSE sColor = 'OPPOSITE'
           ENDELSE
     ENDIF
    IF N_Elements(sColor) EQ 0 THEN color = !P.Color ELSE  color = sColor
    IF Size(color, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN color = Byte(color)
    IF Size(color, /TYPE) LE 2 THEN color = StrTrim(Fix(color),2)
    
    ; Check parameters and keywords.
    IF N_Elements(psym) EQ 0 THEN psym = 0
    IF N_Elements(ssymcolor) EQ 0 THEN symcolor = color ELSE symcolor = ssymcolor
    IF Size(symcolor, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN symcolor = Byte(symcolor)
    IF Size(symcolor, /TYPE) LE 2 THEN symcolor = StrTrim(Fix(symcolor),2)
    IF N_Elements(symsize) EQ 0 THEN symsize = 1.0
   
    ; Be sure the vectors are the right length.
    CASE n_params OF
        1: xsize = N_Elements(x[0,*])
        ELSE: xsize = N_Elements(x)
    ENDCASE
    IF N_Elements(color) GT 1 THEN BEGIN
       IF N_Elements(color) NE xsize THEN $
          Message, 'COLOR vector must contain the same number of elements as the data.'
    ENDIF
    IF N_Elements(symcolor) GT 1 THEN BEGIN
       IF N_Elements(symcolor) NE xsize THEN $
          Message, 'SYMCOLOR vector must contain the same number of elements as the data.'
    ENDIF
    IF N_Elements(symsize) GT 1 THEN BEGIN
       IF N_Elements(symsize) NE xsize THEN $
          Message, 'SYMSIZE vector must contain the same number of elements as the data.'
    ENDIF
    IF N_Elements(psym) GT 1 THEN Message, 'PSYM value must be a scalar value.'
    
   ; Get current color table vectors.
   TVLCT, rr, gg, bb, /Get

   ; Draw the line or symbol.
   IF N_Elements(color) EQ 1 THEN BEGIN
   
       ; Load a color, if needed.
       IF Size(color, /TNAME) EQ 'STRING' THEN color = cgColor(color)
       CASE n_params OF
            1: IF psym[0] LE 0 THEN PlotS, x, Color=color, _STRICT_EXTRA=extra
            2: IF psym[0] LE 0 THEN PlotS, x, y, Color=color, _STRICT_EXTRA=extra
            3: IF psym[0] LE 0 THEN PlotS, x, y, z, Color=color, _STRICT_EXTRA=extra
       ENDCASE   
       
   ENDIF ELSE BEGIN
   
        FOR j=0,xsize-2 DO BEGIN
            thisColor = color[j]
            CASE n_params OF
                1: IF psym[0] LE 0 THEN BEGIN
                       PlotS, [x[0,j],x[0,j+1]], [x[1,j],x[1,j+1]], [x[2,j],x[2,j+1]], $
                           Color=thisColor, _STRICT_EXTRA=extra
                   END
                2: IF psym[0] LE 0 THEN BEGIN
                       PlotS, [x[j],x[j+1]], [y[j], y[j+1]], $
                            Color=thisColor, _STRICT_EXTRA=extra
                   ENDIF
                3: IF psym[0] LE 0 THEN BEGIN
                        PlotS, [x[j],x[j+1]], [y[j], y[j+1]], [z[j], z[j+1]], $
                            Color=thisColor, _STRICT_EXTRA=extra
                   ENDIF
            ENDCASE
        ENDFOR
   
   ENDELSE
   
   ; Draw the symbol, if required.
   IF Abs(psym) GT 0 THEN BEGIN
      
      FOR j=0,N_Elements(x)-1 DO BEGIN
      
          ; Get information about the symbol you are drawing.
          IF N_Elements(symcolor) GT 1 THEN thisColor = symcolor[j] ELSE thisColor = symcolor
          IF Size(thisColor, /TNAME) EQ 'STRING' THEN thisColor = cgColor(thisColor)
          IF N_Elements(symsize) GT 1 THEN thisSize = symsize[j] ELSE thisSize = symsize
          CASE n_params OF
              
                1: BEGIN
                   PlotS, x[*,j], COLOR=thisColor, PSYM=SymCat(Abs(psym)), $
                      SYMSIZE=thisSize, _STRICT_EXTRA=extra
                   END
                   
                2: BEGIN
                  PlotS, x[j], y[j], COLOR=thisColor, PSYM=SymCat(Abs(psym)), $
                       SYMSIZE=thisSize, _STRICT_EXTRA=extra
                   END
                   
                3: BEGIN
                   PlotS, x[j], y[j], z[j], COLOR=thisColor, PSYM=SymCat(Abs(psym)), $
                       SYMSIZE=thisSize, _STRICT_EXTRA=extra
                   END
                       
          ENDCASE  
           
       ENDFOR
       
   ENDIF 
   
   ; Restore the decomposed state if you can.
   SetDecomposedState, currentState
   
   ; Restore the color table. Can't do this for the Z-buffer or
   ; the snap shot will be incorrect.
   IF (!D.Name NE 'Z') THEN TVLCT, rr, gg, bb
   
   ; If you switched data, switch it back.
   IF Keyword_Set(dataSwitch) THEN x = Temporary(y)
   
END