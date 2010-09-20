;+
; NAME:
;       CENTERTLB
;
; PURPOSE:
;
;       This is a utility routine to position a widget program
;       on the display at an arbitrary location. By default the
;       widget is centered on the display.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:

;       Utilities
;
; CALLING SEQUENCE:
;
;       CenterTLB, tlb, [x, y, /NOCENTER, /DEVICE]
;
; REQUIRED INPUTS:
;
;       tlb: The top-level base identifier of the widget program. This must
;       be a valid widget ID.
;
; OPTIONAL INPUTS:
;
;       x:  Set this equal to a normalized position for the center
;       of the widget as measured from the left-hand side of the screen.
;       The default value is 0.5 (the center)  Setting this equal to 1.0
;       places the widget at the far right-hand side of the screen.
;
;       y:  Set this equal to a normalized position for the center
;       of the widget as measured from the bottom of the screen.
;       The default value is 0.5 (the center) Setting this equal to 1.0
;       places the widget at the top of the screen.
;
; KEYWORDS:
;
;      DEVICE:  Normally, the x and y parameters are specified in normalized
;      coordinates. If this keyword is set, they are taken to be in DEVICE
;      coordinates.
;
;      NOCENTER:  By default, the center of the widget is positioned at the
;      location specified by the x and y parameters.  If NOCENTER is set
;      to a non-zero value, then the upper left corner of the widget
;      is postioned at the specifed location.
;
; PROCEDURE:
;
;       The program should be called after all the widgets have
;       been created, but just before the widget hierarchy is realized.
;       It uses the top-level base geometry along with the display size
;       to calculate offsets for the top-level base that will center the
;       top-level base on the display.
;
; COMMENT:
;       Regardless of the values set for x, y and NOCENTER, the widget
;       is not permitted to run off the display.
;
; MODIFICATION HISTORY:
;
;       Written by:  Dick Jackson, 12 Dec 98.
;       Modified to use device-independent Get_Screen_Size
;            function. 31 Jan 2000. DWF.
;       Added x, y, NOCENTER and run-off protection. 26 Jan 2001. BT.
;       Added a maximum value of 1280 for X screen size. This helps
;            center the widget on a single monitor when using dual
;            monitor settings with some graphics cards. 3 Feb 2003. DWF.
;       Added DEVICE keyword. 4 January 2006. DWF.
;
;-
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
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
PRO CenterTLB, tlb, x, y, NoCenter=nocenter, Device=device

On_Error, 2

IF Widget_Info(tlb, /Valid_ID) EQ 0 THEN Message, 'First parameter must be a valid widget ID.'

IF N_Elements(x) EQ 0 THEN xc = 0.5 ELSE xc = Float(x[0])
IF N_Elements(y) EQ 0 THEN yc = 0.5 ELSE yc = Float(y[0])
center = 1 - Keyword_Set(nocenter)

screenSize = Get_Screen_Size()
IF screenSize[0] GT 2000 THEN screenSize[0] = screenSize[0]/2 ; Dual monitors.
IF ~Keyword_Set(device) THEN BEGIN ; Normalized coordinates
   xCenter = screenSize[0] * xc
   yCenter = screenSize[1] * yc
ENDIF ELSE BEGIN ; Device coordinates
   xCenter = xc
   yCenter = yc
ENDELSE

; Get the screen sizes of the TLB. Divide by 2.
geom = Widget_Info(tlb, /Geometry)
xHalfSize = geom.Scr_XSize / 2
yHalfSize = geom.Scr_YSize / 2

; Are you centering, or placing upper-left corner?
IF center THEN BEGIN
   XOffset = 0 > (xCenter - xHalfSize) < (screenSize[0] - geom.Scr_Xsize)
   YOffset = 0 > (yCenter - yHalfSize) < (screenSize[1] - geom.Scr_Ysize)
ENDIF ELSE BEGIN
   XOffset = xcenter
   YOffset = ycenter
ENDELSE

; Set the offsets.
Widget_Control, tlb, XOffset=XOffset, YOffset=YOffset
END
