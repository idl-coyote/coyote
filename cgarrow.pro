; docformat = 'rst'
;
; NAME:
;   cgArrow
;
; PURPOSE:
;   Provides a device-independent and color-model-independent way of drawing an arrow
;   in a specified color.
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
; Provides a device-independent and color-model-independent way of drawing an arrow
; in a specified color.
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics
;    
; :Params:
;    x0: in, required, type=integer/float
;         The x location of the blunt end of the arrow. May be a vector. Assumes
;         device coordinates.
;    x1: in, required, type=integer/float
;         The x location of the sharp end of the arrow. May be a vector. Assumes
;         device coordinates.
;    y0: in, required, type=integer/float
;         The y location of the blunt end of the arrow. May be a vector. Assumes
;         device coordinates.
;    y1: in, required, type=integer/float
;         The y location of the sharp end of the arrow. May be a vector. Assumes
;         device coordinates.
;       
; :Keywords:
;     color: in, optional, type=string/integer/long, default='white'
;         An alternative way to specify the color to use in erasing the graphics window.
;         Color names are those used with cgColor. This parameter is used in
;         preference to the background_color parameter.
;     data: in, optional, type=boolean, default=0
;          Set this keyword of the arrow locations are in data coordinates.
;          Device coordinates are assumed.
;     hsize: in, optional, type=float
;         The size of the the arrow head. By default 1/64th the width
;         of the device. (!D.X_SIZE / 64.)
;     hthick: in, optional, type=float, default=1.0
;         The thickness of the line drawing the arrowhead. 
;     linestyle: in, optional, type=integer, default=0
;         The line style of the arrow. Line style integers as in PLOT.
;     normal: in, optional, type=boolean, default=0
;          Set this keyword of the arrow locations are in normalized coordinates.
;          Device coordinates are assumed.
;     solid: in, optional, type=boolean, default=0
;          Set this keyword to draw solid, filled arrows.
;     thick: in, optional, type=float, default=1.0
;         The thickness of the line drawing the shaft of the arrow. 
;     window: in, optional, type=boolean, default=0
;         Set this keyword to add the command to an cgWindow application.
;         
; :Examples:
;    Used to draw arrows::
;       IDL> cgArrow, 50, 50, 100, 100, /Solid
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
;        Written, 23 November 2010. DWF. Based on old Arrow routine in IDL.
;        Added Window keyword 24 January 2011. DWF.
;        Modified error handler to restore the entry decomposition state if there is an error. 17 March 2011. DWF
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO cgArrow, x0, y0, x1, y1, $
    COLOR = scolor, $
    DATA = data, $
    HSIZE = hsize, $
    HTHICK = hthick, $
    LINESTYLE=linestyle, $
    NORMAL = normal, $
    SOLID = solid, $
    THICK = thick, $
    WINDOW=window, $
    _REF_EXTRA=extra

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(currentState) NE 0 THEN SetDecomposedState, currentState
        RETURN
    ENDIF
    
    ; Should this be added to a resizeable graphics window?
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        void = cgQuery(COUNT=wincnt)
        IF wincnt EQ 0 THEN cgWindow
        cgWindow, 'cgArrow', x0, y0, x1, y1, $
            COLOR = scolor, $
            DATA = data, $
            HSIZE = hsize, $
            HTHICK = hthick, $
            LINESTYLE=linestyle, $
            NORMAL = normal, $
            SOLID = solid, $
            THICK = thick, $
            _EXTRA=extra
            
         RETURN
    ENDIF
    
    ; Get the current color table vectors.
    TVLCT, rr, gg, bb, /Get
    
    ; Set up keyword parameters.
    IF N_Elements(thick) EQ 0 THEN thick = 1.
    IF N_Elements(hthick) EQ 0 THEN hthick = thick
    
    ; Choose a color.
    IF N_Elements(sColor) EQ 0 THEN BEGIN
           IF !D.Name EQ 'PS' THEN BEGIN
                sColor = 'OPPOSITE' 
           ENDIF ELSE BEGIN
                IF (!D.Window GE 0) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
                    pixel = cgSnapshot(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF (Total(pixel) EQ 765) THEN sColor = 'BLACK'
                    IF (Total(pixel) EQ 0) THEN sColor = 'WHITE'
                    IF N_Elements(sColor) EQ 0 THEN sColor = 'OPPOSITE'
                ENDIF ELSE sColor = 'OPPOSITE'
           ENDELSE
    ENDIF
    IF N_Elements(sColor) EQ 0 THEN color = !P.Color ELSE color = sColor
    IF Size(color, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN color = Byte(color)
    IF Size(color, /TYPE) LE 2 THEN color = StrTrim(Fix(color),2)
    
    ; Head size in device units
    IF N_Elements(hsize) EQ 0 $
        THEN arrowsize = !D.X_Size/50. * (hthick/2. > 1) $
        ELSE arrowsize = Float(hsize)
    
    ; If arrowsize GT 15, THEN use 20% arrow. Otherwise use 30%.
    IF arrowsize LT 15 THEN BEGIN
       mcost = -0.866D
       sint = 0.500D
       msint = -sint
    ENDIF ELSE BEGIN
       mcost = - 0.939693D
       sint = 0.342020D
       msint = -sint
    ENDELSE
    
    ; Process each arrow in the vector.
    FOR i = 0L, N_Elements(x0)-1 DO BEGIN   
    
       CASE 1 OF
            Keyword_Set(data):   p = Convert_Coord([x0[i],x1[i]],[y0[i],y1[i]], /DATA, /TO_DEVICE)
            Keyword_Set(normal): p = Convert_Coord([x0[i],x1[i]],[y0[i],y1[i]], /NORMAL, /TO_DEVICE)
            ELSE:                p = [[x0[i], y0[i]],[x1[i], y1[i]]]
       ENDCASE
    
       xp0 = p[0,0]
       xp1 = p[0,1]
       yp0 = p[1,0]
       yp1 = p[1,1]
    
       dx = xp1 - xp0
       dy = yp1 - yp0
       
       ; The length.
       zz = SQRT(dx^2d + dy^2d)
    
       IF zz GT 0 THEN BEGIN
         dx = dx/zz     ; Cos th
         dy = dy/zz     ; Sin th
       ENDIF ELSE BEGIN
         dx = 1.
         dy = 0.
         zz = 1.
       ENDELSE
       IF arrowsize gt 0 $
            THEN a = arrowsize $  ;a = length of head
            ELSE a = -zz * arrowsize
    
       xxp0 = xp1 + a * (dx*mcost - dy * msint)
       yyp0 = yp1 + a * (dx*msint + dy * mcost)
       xxp1 = xp1 + a * (dx*mcost - dy * sint)
       yyp1 = yp1 + a * (dx*sint  + dy * mcost)
     
       SetDecomposedState, 1, CURRENT=currentState
       IF Keyword_Set(solid) THEN BEGIN   ;Use polyfill?
         b = a * mcost*.9d ; End of arrow shaft (Fudge to force join)
         cgPlotS, [xp0, xp1+b*dx], [yp0, yp1+b*dy], /DEVICE, $
            COLOR=color, THICK=thick, LINESTYLE=linestyle, _Extra=extra
         cgColorFill, [xxp0, xxp1, xp1, xxp0], [yyp0, yyp1, yp1, yyp0], $
            /DEVICE, COLOR = color
       ENDIF ELSE BEGIN
         cgPlotS, [xp0, xp1], [yp0, yp1], /DEVICE, COLOR=color, THICK=thick, $
             LINESTYLE=linestyle, _Extra=extra
         cgPlotS, [xxp0,xp1,xxp1],[yyp0,yp1,yyp1], /DEVICE, COLOR=color, $
            THICK=hthick, LINESTYLE=linestyle, _Extra=extra
       ENDELSE
       SetDecomposedState, currentState
       ENDFOR
       
       ; Restore the input colors.
       IF !D.NAME NE 'Z' THEN TVLCT, rr, gg, bb
END
