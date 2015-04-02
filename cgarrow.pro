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
;    y0: in, required, type=integer/float
;         The y location of the blunt end of the arrow. May be a vector. Assumes
;         device coordinates.
;    x1: in, required, type=integer/float
;         The x location of the sharp end of the arrow. May be a vector. Assumes
;         device coordinates.
;    y1: in, required, type=integer/float
;         The y location of the sharp end of the arrow. May be a vector. Assumes
;         device coordinates.
;       
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;         An alternative way to set the `Window` keyword.
;     clip: in, optional, type=boolean, default=0
;         Set the keyword to clip arrow output to the clipping rectangle specified
;         in the `CRect` keyword. See the documentation for the IDL graphics 
;         keyword NOCLIP.
;     color: in, optional, type=string/integer/long, default='white'
;         An alternative way to specify the color to use in erasing the graphics window.
;         Color names are those used with cgColor. This parameter is used in
;         preference to the background_color parameter.
;     crect: in, optional, type=float
;          A four-element array giving the clipping rectangle [xo, yo, x1, y1]. The
;          default clipping rectangle is the plot area. See the documenation for the
;          IDL graphics keyword CLIP.
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
;        Added the ADDCMD keyword to make the interface more consistent with other Coyote Grapics routines. 18 April 2013. DWF.
;        Modified to allow a vector of colors to be added with the COLOR keyword. 23 February 2015. DWF.
;        Algorithm modified slightly to get the program a little closer to the machine to speed up
;           vector drawing when there are lots of vectors. 23 February 2015. DWF.
;        
; :Copyright:
;     Copyright (c) 2010-2015, Fanning Software Consulting, Inc.
;-
PRO cgArrow, x0, y0, x1, y1, $
    ADDCMD=addcmd, $
    CLIP=clipit, $
    COLOR = scolor, $
    CRECT=crect, $
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
        void = cgErrorMsg()
        IF N_Elements(currentState) NE 0 THEN cgSetColorState, currentState
        RETURN
    ENDIF
    
    ; Use ADDCMD as an alternative way to set the WINDOW keyword.
    IF Keyword_Set(addcmd) THEN window = 1
    
    ; Should this be added to a resizeable graphics window?
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        void = cgQuery(COUNT=wincnt)
        IF wincnt EQ 0 THEN cgWindow
        cgWindow, 'cgArrow', x0, y0, x1, y1, $
            CLIP=clipit, $
            COLOR = scolor, $
            CRECT=crect, $
            DATA = data, $
            HSIZE = hsize, $
            HTHICK = hthick, $
            LINESTYLE=linestyle, $
            NORMAL = normal, $
            SOLID = solid, $
            THICK = thick, $
            ADDCMD=1, $
            _EXTRA=extra
            
         RETURN
    ENDIF
    
    ; Get the current color table vectors.
    TVLCT, rr, gg, bb, /Get
    
    ; Set up keyword parameters.
    IF N_Elements(thick) EQ 0 THEN thick = 1.
    IF N_Elements(hthick) EQ 0 THEN hthick = thick
    
    ; Strange manipulations to get IDL graphics keyword straight.
    noclip = 1 - Keyword_Set(clipit)
    IF N_Elements(crect) NE 0 THEN clip =crect
    
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
    IF Size(color, /TYPE) EQ 3 THEN IF cgGetColorState() EQ 0 THEN color = Byte(color)
    IF Size(color, /TYPE) LE 2 THEN color = StrTrim(Fix(color),2)
    
    ; Make sure colors are using 24-bit values.
    cgSetColorState, 1, CURRENT=currentState
    IF N_Elements(color) NE N_Elements(x0) $
        THEN thisColor = Replicate(cgColor(color), N_Elements(x0)) $
        ELSE thisColor = cgColor(color)
    
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
       
       aColor = thisColor[i]
    
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
     
       IF Keyword_Set(solid) THEN BEGIN   ;Use polyfill?
         b = a * mcost*.9d ; End of arrow shaft (Fudge to force join)
         Plots, [xp0, xp1+b*dx], [yp0, yp1+b*dy], /DEVICE, $
            COLOR=aColor, THICK=thick, LINESTYLE=linestyle, $
            NOCLIP=noclip, CLIP=clip, _Extra=extra
         Polyfill, [xxp0, xxp1, xp1, xxp0], [yyp0, yyp1, yp1, yyp0], $
            /DEVICE, COLOR=aColor, NOCLIP=noclip, CLIP=clip, _Extra=extra
       ENDIF ELSE BEGIN
         Plots, [xp0, xp1], [yp0, yp1], /DEVICE, COLOR=aColor, THICK=thick, $
             LINESTYLE=linestyle, NOCLIP=noclip, CLIP=clip,_Extra=extra
         Polyfill, [xxp0,xp1,xxp1],[yyp0,yp1,yyp1], /DEVICE, COLOR=aColor, $
            THICK=hthick, LINESTYLE=linestyle, $
             NOCLIP=noclip, CLIP=clip,_Extra=extra
       ENDELSE
       
    ENDFOR
       
    ; Restore color state.
    cgSetColorState, currentState

    ; Restore the input colors.
    IF !D.NAME NE 'Z' THEN TVLCT, rr, gg, bb
END
