; docformat = 'rst'
;
; NAME:
;   FSC_Display
;
; PURPOSE:
;   The purpose of FSC_Display is to open a graphics window on graphics devices that
;   support windows. Using FSC_Display to open a graphics window means you no longer
;   have to "protect" your Window command in devices, like the PostScript device, that
;   do not support windows. Basically, a smarter Window command.
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
;   The purpose of FSC_Display is to open a graphics window on graphics devices that
;   support windows. Using FSC_Display to open a graphics window means you no longer
;   have to "protect" your Window command in devices, like the PostScript device, that
;   do not support windows. Basically, a smarter Window command.
;
; :Categories:
;    Graphics
;    
; :Params:
;    windowindex: in, optional, type=integer, default=0
;         The window index number of the graphics window you would like to create.
;    xsize: in, optional, type=integer, default=640
;         The X size of the graphics window created. By default, 640.
;    ysize: in, optional, type=integer, default=512
;         The Y size of the graphics window created. By default, 512.
;         
; :Keywords:
;     color: in, optional, type=string/integer, default='white'
;        If this keyword is a string, the name of the data color. By default, 'white'.
;        Color names are those used with FSC_Color. Otherwise, the keyword is assumed 
;        to be a color index into the current color table.
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
;       IDL> FSC_Display, /Free, XSIZE=500 YSIZE=400
;       IDL> FSC_Display, 1, 500, 500, COLOR='gray'
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
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO FSC_Display, windowIndex, pxsize, pysize, $
    COLOR=color, $
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
    
    ; Check parameters and keywords.
    IF N_Elements(color) EQ 0 THEN color = 'white'
    IF N_Elements(windowIndex) EQ 0 THEN windowIndex = 0
    IF N_Elements(xsize) EQ 0 THEN xsize = 640
    IF N_Elements(ysize) EQ 0 THEN ysize = 512
    IF N_Elements(pxsize) EQ 0 THEN pxsize = xsize
    IF N_Elements(pysize) EQ 0 THEN pysize = ysize
    IF windowIndex GT 127 THEN BEGIN
        pysize = pxsize
        pxsize = windowIndex
        windowIndex = 0
    ENDIF
    
    IF (!D.Flags AND 256) NE 0 THEN BEGIN
        Window, windowIndex, XSIZE=pxsize, YSIZE=pysize, _STRICT_EXTRA=extra
        FSC_Erase, color   
    ENDIF
END