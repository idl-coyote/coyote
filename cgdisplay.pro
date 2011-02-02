; docformat = 'rst'
;
; NAME:
;   cgDisplay
;
; PURPOSE:
;   The purpose of cgDisplay is to open a graphics window on the display, or in the
;   PostScript device, or in the Z-graphics buffer, depending upon the current graphics
;   device. In PostScript a window of the proper aspect ratio is created with PSWindow.
;   Using cgDisplay to open "windows" will allow you to more easily write device-independent
;   IDL programs.
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
;   The purpose of cgDisplay is to open a graphics window on the display, or in the
;   PostScript device, or in the Z-graphics buffer, depending upon the current graphics
;   device. In PostScript a window of the proper aspect ratio is created with PSWindow.
;   Using cgDisplay to open "windows" will allow you to more easily write device-independent
;   IDL programs.
;
; :Categories:
;    Graphics
;    
; :Params:
;    xsize: in, optional, type=integer, default=640
;         The X size of the graphics window created. By default, 640.
;    ysize: in, optional, type=integer, default=512
;         The Y size of the graphics window created. By default, 512.
;         
; :Keywords:
;    color: in, optional, type=string/integer, default='white'
;        If this keyword is a string, the name of the data color. By default, 'white'.
;        Color names are those used with cgColor. Otherwise, the keyword is assumed 
;        to be a color index into the current color table. The color is not used if
;        the "window" is opened in PostScript on the Z-graphics buffer.
;    free: in, optional, type=boolean, default=0
;         Set this keyword to open a window with a free or unused window index number.
;         This keyword applied only to graphics windows created on the computer display.
;    wid: in, optional, type=integer, default=0
;         The window index number of the IDL graphics window to create.
;    xsize: in, optional, type=integer, default=640
;         The X size of the graphics window created. By default, 640. The XSIZE parameter 
;         is used in preference to the XSIZE keyword value.
;    ysize: in, optional, type=integer, default=512
;         The Y size of the graphics window created. By default, 512. The YSIZE parameter 
;         is used in preference to the YSIZE keyword value.
;     _extra: in, optional, type=any
;         Any keywords supported by the WINDOW command are allowed.
;         
; :Examples:
;    Use like the IDL WINDOW command::
;       IDL> cgDisplay, XSIZE=500 YSIZE=400
;       IDL> cgDisplay, 500, 500, WID=1, COLOR='gray'
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
;        Written, 15 November 2010. DWF.
;        Changes so that color variables don't change type. 23 Nov 2010. DWF.
;        Moved the window index argument to the WID keyword. 9 Dec 2010. DWF.
;        Modified to produce a window in PostScript and the Z-buffer, too. 15 Dec 2010. DWF.
;        Added the FREE keyword. 3 January 2011. DWF.
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO cgDisplay, pxsize, pysize, $
    COLOR=scolor, $
    FREE=free, $
    WID=windowIndex, $
    XSIZE=xsize, $
    YSIZE=ysize, $
    _EXTRA=extra

    Compile_Opt idl2

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Check parameters and keywords.
    free = Keyword_Set(free)
    IF N_Elements(scolor) EQ 0 THEN color = 'white' ELSE color = scolor
    IF N_Elements(windowIndex) EQ 0 THEN windowIndex = 0
    IF N_Elements(xsize) EQ 0 THEN xsize = 640
    IF N_Elements(ysize) EQ 0 THEN ysize = 512
    IF N_Elements(pxsize) EQ 0 THEN pxsize = xsize
    IF N_Elements(pysize) EQ 0 THEN pysize = ysize
    
    IF (!D.Flags AND 256) NE 0 THEN BEGIN
        Window, windowIndex, XSIZE=pxsize, YSIZE=pysize, FREE=free, _STRICT_EXTRA=extra
        
        ; cgErase will take care of sorting out what kind of "color" indicator
        ; we are using. No need to do it here.
        cgErase, color   
    ENDIF ELSE BEGIN
        CASE !D.Name OF
            'PS': Device, _Extra=PSWindow(AspectRatio=Float(pysize)/pxsize)
            'Z': Device, Set_Resolution=[pxsize,pysize]
            ELSE:
        ENDCASE
    ENDELSE
END