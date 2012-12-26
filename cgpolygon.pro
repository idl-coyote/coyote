; docformat = 'rst'
;
; NAME:
;   cgPolygon
;
; PURPOSE:
;   Provides a device-independent and color-model-independent way to draw a 
;   filled or unfilled polygon.
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
;   Provides a device-independent and color-model-independent way to draw a 
;   filled or unfilled polygon.
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
;     addcmd: in, optional, type=boolean, default=0
;        Set this keyword to add the command to an cgWindow. Setting this keyword
;        automatically sets the WINDOW keyword, but the command does not erase the
;        graphics window as it would normally.
;     color: in, optional, type=string, default='rose'
;         The name of the polygon color. Color names are those used with cgColor. 
;         This value can also be a long integer or an index into the current color
;         table.
;     device: in, optional, type=boolean, default=0
;         Set to indicate the polygon vertices are in device coordinates.
;     fcolor: in, optional, type=string/integer/long
;         The name of the polygon fill color. Color names are those used with cgColor. 
;         This value can also be a long integer or an index into the current color
;         table. The default is the same as `Color`.
;     fill: in, optional, type=boolean, default=0
;         Set this keyword to draw a filled polygon, rather than an open polygon.
;     normal: in, optional, type=boolean, default=0
;         Set to indicate the polygon vertices are in normalized coordinates.
;     position: in, optional, type=fltarr(4)
;         Set this keyword to a four-element position array of normalized coordinates
;         to fill a rectangular area on the graphics display. The normal input parameters
;         are ignored if this keyword is set.
;     window: in, optional, type=boolean, default=0
;         Set this keyword to add the command to the current cgWindow application.
;     _ref_extra: in, optional, type=appropriate
;         Any other keywords to the IDL POLYFILL command may be used.
;     
;          
; :Examples:
;    Used like the IDL Plots or Polyfill commands::
;       IDL> cgPolygon, [0.25, 0.25, 0.75, 0.75, 0.25], [0.25, 0.75, 0.75, 0.25, 0.25], $
;                 /NORMAL, COLOR='blue'
;       IDL> cgPolygon, [0.25, 0.25, 0.75, 0.75, 0.25], [0.25, 0.75, 0.75, 0.25, 0.25], $
;                 /NORMAL, COLOR='blue', FCOLOR='rose', /FILL
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
;        Written, 26 March 2012. David Fanning.
;        Added Position keyword to allow a fill of a particular portion of the graphics 
;            window. 20 Apr 2012. DWF.
;        Added AddCmd keyword. 25 Oct 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
PRO cgPolygon, x, y, z, $
    ADDCMD=addcmd, $
    COLOR=color, $
    FCOLOR=fcolor, $
    FILL=fill, $
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
    IF (N_Params() EQ 0) AND (N_Elements(position) EQ 0) THEN BEGIN
        Print, 'USE SYNTAX: cgColorFill, x, y, [z]'
        RETURN
    ENDIF
    
    ; Do they want this plot in a resizeable graphics window?
    IF Keyword_Set(addcmd) THEN window = 1
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
        IF wincnt EQ 0 THEN replaceCmd = 0 ELSE replaceCmd=1
        
        ; If adding a command, have to do this differently.
        IF Keyword_Set(addcmd) THEN BEGIN
           cgWindow, 'cgColorFill', x, y, z, $
              COLOR=color, $
              FCOLOR=fcolor, $
              FILL=fill, $
              NORMAL=normal, $
              DEVICE=device, $
              POSITION=position, $
              ADDCMD=1, $
              _EXTRA=extra
                
            RETURN
            
        ENDIF ELSE BEGIN
        
           ; Otherwise, we are just replacing the commands in a new or existing window.
           cgWindow, 'cgColorFill', x, y, z, $
              COLOR=color, $
              FCOLOR=fcolor, $
              FILL=fill, $
              NORMAL=normal, $
              DEVICE=device, $
              POSITION=position, $
              REPLACECMD=replaceCmd, $
              _Extra=extra
            
           RETURN
         
         ENDELSE
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; We are going to draw in decomposed color, if possible.
    SetDecomposedState, 1, Current=currentState
       
    ; If current state is "indexed color" and colors are represented as long integers then "fix" them.
    IF (currentState EQ 0) THEN BEGIN
      IF Size(color, /TNAME) EQ 'LONG' THEN color = Fix(color)
      IF Size(fcolor, /TNAME) EQ 'LONG' THEN fcolor = Fix(fcolor)
    ENDIF
    
    ; Need a color?
    thisColor = cgDefaultColor(color, DEFAULT='rose')
    IF N_Elements(fColor) EQ 0 THEN BEGIN
       fillColor = thisColor 
    ENDIF ELSE BEGIN
       fillColor = cgDefaultColor(fcolor)
    ENDELSE
    
    ; Get the current color vectors.
    TVLCT, rr, gg, bb, /Get
    
    ; Do you need a window?
    IF ((!D.Flags AND 256) NE 0) && (!D.Window LT 0) THEN cgDisplay
    
    ; Are the colors strings? 
    IF Size(thisColor, /TNAME) EQ 'STRING' THEN thisColor = cgColor(thisColor)
    IF Size(fillColor, /TNAME) EQ 'STRING' THEN fillColor = cgColor(fillColor)
    
    ; Are we filling a POSITION in the window?
    IF N_Elements(position) NE 0 THEN BEGIN
       p = position
       x = [p[0], p[0], p[2], p[2], p[0]]
       y = [p[1], p[3], p[3], p[1], p[1]]
       normal = 1
       device = 0
    ENDIF
    
    ; Fill the polygon.
    CASE N_Elements(z) OF
        0: BEGIN
              IF Keyword_Set(fill) THEN BEGIN
                 PolyFill, x, y, COLOR=fillColor, NORMAL=normal, DEVICE=device, _EXTRA=extra
                 PlotS, x, y, COLOR=thisColor, NORMAL=normal, DEVICE=device, _EXTRA=extra
              ENDIF ELSE BEGIN
                 PlotS, x, y, COLOR=thisColor, NORMAL=normal, DEVICE=device, _EXTRA=extra
              ENDELSE
           END
        ELSE: BEGIN
              IF Keyword_Set(fill) THEN BEGIN
                 PolyFill, x, y, z, COLOR=fillColor, NORMAL=normal, DEVICE=device, _EXTRA=extra
                 PlotS, x, y, z, COLOR=thisColor, NORMAL=normal, DEVICE=device, _EXTRA=extra
              ENDIF ELSE BEGIN
                 PlotS, x, y, z, COLOR=thisColor, NORMAL=normal, DEVICE=device, _EXTRA=extra
              ENDELSE
           END
    ENDCASE
    
    ; Clean up.
    SetDecomposedState, currentState
    IF !D.Name NE 'Z' THEN TVLCT, rr, gg, bb
   
END
