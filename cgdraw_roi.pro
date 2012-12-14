; docformat = 'rst'
;
; NAME:
;   cgDraw_ROI
;
; PURPOSE:
;   This procedure draws a region or group of regions, defined by the IDLanROI or
;   IDLanROIGroup objects, on a direct graphics device. The type of ROI drawn is
;   based on the TYPE property of a given IDLanROI object. These can be points,
;   polylines, or filled polygons.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2012, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; This procedure draws a region or group of regions, defined by the IDLanROI or
; IDLanROIGroup objects, on a direct graphics device. The type of ROI drawn is
; based on the TYPE property of a given IDLanROI object. These can be points,
; polylines, or filled polygons. This procedure is a Coyote Graphics wrapper for
; the built-in IDL procedure DRAW_ROI.
;
; :Categories:
;    Graphics
;    
; :Params:
;     roi: in, required, type=object
;         The input ROI object. Must be either an IDLanROI object or an IDLanROIGroup
;         object (if you wish to draw multiple ROIs at the same time).       
;
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        Set this keyword to add the command to an cgWindow. Setting this keyword
;        automatically sets the WINDOW keyword, but the command does not erase the
;        graphics window as it would normally.
;     color: in, optional, type=string, default='opposite'
;         The name of the polygon color. Color names are those used with cgColor. 
;         This value can also be a long integer or a byte or short integer index 
;         into the current color table.
;     device: in, optional, type=boolean, default=0
;         Set to indicate the polygon vertices are in device coordinates, rather than data coordinates.
;     linestyle: in, optional, type=integer, default=0
;         Set this keyword to the normal linestyle graphics keyword values. The default is a solid line.
;     normal: in, optional, type=boolean, default=0
;         Set to indicate the polygon vertices are in normalized coordinates, rather than data coordinates.
;     psym: in, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46. May also be specified as a
;        symbol names. See `cgSymCat` for details.
;     symsize: in, optional, type=float/vector, default=1.0
;        A scalar or vector of symbol sizes. Default is 1.0. May be a vector of the same 
;        length as X.
;     t3d: in, optional, type=boolean, default=0
;        Set this keyword to use the 3D coordinate system established by !P.T.
;     thick: in, optional, type=float, default=1.0
;     window: in, optional, type=boolean, default=0
;         Set this keyword to add the command to the current cgWindow application.
;     _ref_extra: in, optional
;         Any `DRAW_ROI <http://www.exelisvis.com/docs/DRAW_ROI_Procedure.html>` keyword 
;         not defined here may be used
;     
;          
; :Examples:
;    Download `cgDraw_ROI_Example <http://www.idlcoyote.com/tip_examples/cgdraw_roi_example.pro>'
;    for an example program illustrating how cgDraw_ROI can be used.
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
;        Written, 11 November 2012. David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
PRO cgDraw_ROI, roi, $
    ADDCMD=addcmd, $
    COLOR=color, $
    DEVICE=device, $
    LINESTYLE=linestyle, $
    NORMAL=normal, $
    PSYM=psym, $
    SYMSIZE=symsize, $
    T3D=t3d, $
    THICK=thick, $
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

    ; Did user pass a parameter?
    IF (N_Params() EQ 0) THEN BEGIN
        Print, 'USE SYNTAX: cgDraw_ROI, thisROI
        RETURN
    ENDIF
    
    ; Do they want this plot in a resizeable graphics window?
    IF Keyword_Set(addcmd) THEN window = 1
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
        IF wincnt EQ 0 THEN replaceCmd = 0 ELSE replaceCmd=1
        
        ; If adding a command, have to do this differently.
        IF Keyword_Set(addcmd) THEN BEGIN
           cgWindow, 'cgDraw_ROI', roi, $
              COLOR=color, $
              DEVICE=device, $
              LINESTYLE=linestyle, $
              NORMAL=normal, $
              PSYM=psym, $
              SYMSIZE=symsize, $
              T3D=t3d, $
              THICK=thick, $
              ADDCMD=1, $
              _EXTRA=extra
                
            RETURN
            
        ENDIF ELSE BEGIN
        
           ; Otherwise, we are just replacing the commands in a new or existing window.
           cgWindow, 'cgDraw_ROI', roi, $
              COLOR=color, $
              DEVICE=device, $
              LINESTYLE=linestyle, $
              NORMAL=normal, $
              PSYM=psym, $
              SYMSIZE=symsize, $
              T3D=t3d, $
              THICK=thick, $
              REPLACECMD=replaceCmd, $
              _Extra=extra
            
           RETURN
         
         ENDELSE
    ENDIF
    
  ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Do this in decomposed color, if possible.
    SetDecomposedState, 1, CURRENTSTATE=currentState
    
    ; Need a color?
    thisColor = cgDefaultColor(color, DEFAULT='opposite')
    
    ; Get the current color vectors.
    TVLCT, rr, gg, bb, /Get
    
    ; Do you need a window?
    IF ((!D.Flags AND 256) NE 0) && (!D.Window LT 0) THEN cgDisplay
    
    ; Is the color a string? 
    IF Size(thisColor, /TNAME) EQ 'STRING' THEN thisColor = cgColor(thisColor)
    IF N_Elements(psym) EQ 0 THEN psym = 0
        
    ; Draw the ROI
    Draw_ROI, roi, $
      COLOR=thisColor, $
      DEVICE=device, $
      LINESTYLE=linestyle, $
      NORMAL=normal, $
      PSYM=cgSymCat(psym), $
      SYMSIZE=symsize, $
      T3D=t3d, $
      THICK=thick, $
      _Extra=extra
    
    ; Clean up.
    SetDecomposedState, currentState
    IF !D.Name NE 'Z' THEN TVLCT, rr, gg, bb
   
END
