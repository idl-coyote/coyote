; docformat = 'rst'
;
; NAME:
;   cgBlendImage
;
; PURPOSE:
;   This alpha blends two 24-bit images and displays them with the cgImage command.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2011, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; This alpha blends two 24-bit images and displays them with the cgImage command.
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics
;    
; :Params:
;    foregroundImage: in, required, type=truecolor
;         A 24-bit foreground image with the same dimensions as the background image.
;    backgroundImage: in, required, type=truecolor    
;         A 24-bit background image with the same dimensions as the foreground image.
;
; :Keywords:
;    alpha: in, optional, type=float, default=0.5
;         A number between 0 and 1 that indicates the percentage of the foreground image
;         to alpha-blend into the background image. For example, ALPHA=0.2 will give
;         a weigth of 20% to the foreground image pixels and 80% to the background image
;         pixels. 
;   window: in, optional, type=intarr
;         A one-, two-, or three-element array giving the window index numbers for
;         the foreground, background, and display windows, repectively. If one-element,
;         the foreground image is retrieved from that window and displayed in the current
;         window. If two elements, the foreground and background images are obtained from
;         the first two elements, respectively, and a new third display window is opened.
;         This is the same for three elements, except the third element is used to specify
;         the display window.
;    _ref_extra: in, optional
;         Any keyword appropriate for the cgImage command is also accepted by keyword
;         inheritance.
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
;        Written by: David W. Fanning, 26 May 2009.
;        Added the WINDOW keyword. 16 February 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2009-2012, Fanning Software Consulting, Inc.
;-
PRO cgBlendImage, foreGroundImage, backGroundImage, $
   ALPHA=alpha, $
   WINDOW=window, $
   _REF_EXTRA=extra

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Handle the window keyword
    IF N_Elements(window) NE 0 THEN BEGIN
       
       ; Save the current graphics window. Be aware that this may be -1.
       thisWindow = !D.Window
       
       ; How many elements are in the WINDOW keyword?
       CASE N_Elements(window) OF
       
           1: BEGIN
              WSet, window[0]
              foreGroundImage = cgSnapshot()
              IF thisWindow GE 0 THEN BEGIN
                  WSet, thisWindow 
              ENDIF ELSE BEGIN
                  cgDisplay, /Force, /Match, /Free
              ENDELSE
              backGroundImage = cgSnapshot()
              END
              
           2: BEGIN
              WSet, window[0]
              foreGroundImage = cgSnapshot()
              WSet, window[1]
              backGroundImage = cgSnapshot()
              cgDisplay, /Force, /Match, /Free
              END
              
           3: BEGIN
              WSet, window[0]
              foreGroundImage = cgSnapshot()
              WSet, window[1]
              backGroundImage = cgSnapshot()
              IF WindowAvailable(window[2]) THEN BEGIN
                  WSet, window[2] 
              ENDIF ELSE BEGIN
                  cgDisplay, /Force, /Match, /Free
              ENDELSE
              END

            ELSE: Message, 'WINDOW keyword must contain no more than three elements.'
       ENDCASE
    
    ENDIF
    
    ; Need a foreground and a background image.
    IF (N_Elements(foregroundImage) EQ 0) || (N_Elements(backgroundImage) EQ 0) $
        THEN Message, 'Usage:  "BlendedImage, foreGroundImage, backGroundImage"'
    
    ; Condition alpha blending value.
    IF N_Elements(alpha) EQ 0 THEN alpha = 0.5
    alpha = 0.0 > alpha < 1.0
    
    ; Do input image dimensions match?
    IF Total( Size(foregroundImage, /DIMENSIONS) EQ Size(backgroundImage, /DIMENSIONS) ) NE 3 $
        THEN Message, 'Dimensions and/or size of input images do not match.'
        
    ; Output the blended image.
    cgImage,  (foreGroundImage * alpha) + (1 - alpha) * backGroundImage, _STRICT_EXTRA=extra
            
END
