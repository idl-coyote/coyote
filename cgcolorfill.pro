; docformat = 'rst'
;
; NAME:
;   cgColorFill
;
; PURPOSE:
;   Provides a device-independent and color-model-independent way to fill a polygon
;   with a particular color.
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
; Provides a device-independent and color-model-independent way to fill a polygon
; with a particular color. This is a wrapper to the PolyFill command in IDL.
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics
;    
; :Params:
;     x: in, required, type=number
;         A vector argument providing the X coordinates of the points to be connected. 
;         The vector must contain at least three elements. If only one argument is 
;         specified, X must be an array of either two or three vectors (i.e., (2,*) 
;         or (3,*)). In this special case, the vector X[0,*] specifies the X values, 
;         X[1,*] specifies Y, and X[2,*] contain the Z values.       
;     y: in, required, type=number
;         A vector argument providing the Y coordinates of the points to be connected. 
;         Y must contain at least three elements.
;     z: in, optional, type=number
;         An optional vector argument providing the Z coordinates of the points to be 
;         connected. Z must contain at least three elements.
;
; :Keywords:
;     color: in, optional, type=string/byte/integer/long, default='rose'
;         The name of the fill color. Color names are those used with cgColor. 
;         This value can also be a long integer or an index into the current color
;         table.
;     device: in, optional, type=boolean, default=0
;         Set to indicate the polygon vertices are in device coordinates.
;     normal: in, optional, type=boolean, default=0
;         Set to indicate the polygon vertices are in normalized coordinates.
;     position: in, optional, type=float
;         Set to the normal four-element normalized position array for locating 
;         a rectangular region in a graphics window. If this keyword is used, the
;         x and y parameters are constructed from this position.
;     window: in, optional, type=boolean, default=0
;         Set this keyword to add the command to the current cgWindow application.
;     _ref_extra: in, optional, type=appropriate
;         Any other keywords to the IDL POLYFILL command may be used.
;     
;          
; :Examples:
;    Used like the IDL Polyfill command::
;       IDL> cgColorFill, [0.25, 0.25, 0.75, 0.75, 0.25], [0.25, 0.75, 0.75, 0.25, 0.25], $
;                 /NORMAL, COLOR='blue'
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 24 December 2010. DWF.
;        In some cases, I was turning BYTE values to strings without converting to 
;            INTEGERS first. 30 Dec 2010. DWF.        
;        Moved setting to decomposed color before color selection process to avoid PostScript
;             background problems when passed 24-bit color integers. 12 Jan 2011. DWF.   
;        Added WINDOW keyword. 24 Jan 2011. DWF.
;        Modified error handler to restore the entry decomposition state if there is an error. 17 March 2011. DWF
;        Modified to use cgDefaultColor for default color selection. 24 Dec 2011. DWF.
;        Added a POSITION keyword to allow setting the color position in a graphics window. 24 Jan 2013. DWF.
;
; :Copyright:
;     Copyright (c) 2010-2013, Fanning Software Consulting, Inc.
;-
PRO cgColorFill, x, y, z, $
    COLOR=color, $
    NORMAL=normal, $
    DEVICE=device, $
    POSITION=position, $
    WINDOW=window, $
     _REF_EXTRA=extra

    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(currentState) NE 0 THEN SetDecomposedState, currentState
        RETURN
    ENDIF

    ; Did user pass parameters?
    IF (N_Params() EQ 0) && (N_Elements(position) EQ 0) THEN BEGIN
        Print, 'USE SYNTAX: cgColorFill, x, y, [z]'
        RETURN
    ENDIF
    
    ; Should this be added to a resizeable graphics window?
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        cgWindow, 'cgColorFill', x, y, z, $
            COLOR=color, $
            NORMAL=normal, $
            DEVICE=device, $
            POSITION=position, $
            ADDCMD=1, $
            _EXTRA=extra
            
         RETURN
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; We are going to draw in decomposed color, if possible.
    SetDecomposedState, 1, Current=currentState

    ; Use position to set up vectors?
    IF N_Elements(position) NE 0 THEN BEGIN
       p = position
       x = [p[0], p[0], p[2], p[2], p[0]]
       y = [p[1], p[3], p[3], p[1], p[1]]
       normal = 1
    ENDIF
    
    ; If current state is "indexed color" and colors are represented as long integers then "fix" them.
    IF (currentState EQ 0) THEN BEGIN
       IF Size(color, /TNAME) EQ 'LONG' THEN color = Fix(color)
    ENDIF
    
    ; Need a color?
    thisColor = cgDefaultColor(color, DEFAULT='rose')

    ; Get the current color vectors.
    TVLCT, rr, gg, bb, /Get
    
    ; Fill the polygon.
    IF Size(thisColor, /TNAME) EQ 'STRING' THEN thisColor = cgColor(thisColor)
    CASE N_Elements(z) OF
        0: PolyFill, x, y, COLOR=thisColor, NORMAL=normal, DEVICE=device, _STRICT_EXTRA=extra
        ELSE: PolyFill, x, y, z, COLOR=thisColor, NORMAL=normal, DEVICE=device, _STRICT_EXTRA=extra
    ENDCASE
    
    ; Clean up.
    SetDecomposedState, currentState
    IF !D.Name NE 'Z' THEN TVLCT, rr, gg, bb
   
END
