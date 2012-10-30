; docformat = 'rst'
;
; NAME:
;   cgCenterTLB
;
; PURPOSE:
;   This is a utility routine to position a widget program on the display at an arbitrary 
;   location. By default the widget is centered on the display.
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
; This is a utility routine to position a widget program on the display at an arbitrary 
; location. By default the widget is centered on the display.
;
; :Categories:
;    Utilities
;    
; :Params:
;    tlb: in, required, type=long
;       The top-level base identifier of the widget program. This must
;       be a valid widget ID.
;    x: in, optional, type=integer
;       Set this equal to a normalized position for the center
;       of the widget as measured from the left-hand side of the screen.
;       The default value is 0.5 (the center)  Setting this equal to 1.0
;       places the widget at the far right-hand side of the screen.
;    y: in, optional, type=integer
;       Set this equal to a normalized position for the center
;       of the widget as measured from the bottom of the screen.
;       The default value is 0.5 (the center) Setting this equal to 1.0
;       places the widget at the top of the screen.
;       
; :Keywords:
;    centerontlb: in, optional, type=long
;      If provided, the center of the widget is positioned at 
;      the center of the widget whose ID is provided here.
;    device: in, optional, type=boolean, default=0
;       Normally, the x and y parameters are specified in normalized
;       coordinates. If this keyword is set, they are taken to be in device
;       coordinates.
;    nocenter: in, optional, type=boolean, default=0
;       By default, the center of the widget is positioned at the
;       location specified by the x and y parameters.  If `NoCenter` is set
;       to a non-zero value, then the upper left corner of the widget
;       is postioned at the specifed location.
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
;    Change History::
;       Written by:  Dick Jackson, 12 Dec 98.
;       Modified to use device-independent Get_Screen_Size
;            function. 31 Jan 2000. DWF.
;       Added x, y, NOCENTER and run-off protection. 26 Jan 2001. BT.
;       Added a maximum value of 1280 for X screen size. This helps
;            center the widget on a single monitor when using dual
;            monitor settings with some graphics cards. 3 Feb 2003. DWF.
;       Added DEVICE keyword. 4 January 2006. DWF.
;       Added CenterOnTLB keyword. 7 March 2011. DJ.
;       Renamed cgCenterTLB and retired the previous CenterTLB program. 29 Oct 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
PRO cgCenterTLB, tlb, x, y, $
    CenterOnTLB=wCenterOnTLB, $
    Device=device, $
    NoCenter=nocenter 
   
    On_Error, 2
    
    IF Widget_Info(tlb, /Valid_ID) EQ 0 THEN Message, 'First parameter must be a valid widget ID.'
    
    IF N_Elements(x) EQ 0 THEN xc = 0.5 ELSE xc = Float(x[0])
    IF N_Elements(y) EQ 0 THEN yc = 0.5 ELSE yc = Float(y[0])
    center = 1 - Keyword_Set(nocenter)
    
    ; Get the screen size of the primary monitor.
    screenSize = GetPrimaryScreenSize(/Exclude_Taskbar)
    IF N_Elements(wCenterOnTLB) EQ 1 && $
       Widget_Info(wCenterOnTLB, /Valid_ID) THEN BEGIN
       wCenterOnTLBGeom = Widget_Info(wCenterOnTLB, /Geometry)
       xCenter = wCenterOnTLBGeom.xOffset + wCenterOnTLBGeom.scr_xSize / 2
       yCenter = wCenterOnTLBGeom.yOffset + wCenterOnTLBGeom.scr_ySize / 2
    ENDIF ELSE IF ~Keyword_Set(device) THEN BEGIN ; Normalized coordinates
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
       xOffset = 0 > (xCenter - xHalfSize) < (screenSize[0] - geom.Scr_Xsize)
       yOffset = 0 > (yCenter - yHalfSize) < (screenSize[1] - geom.Scr_Ysize)
    ENDIF ELSE BEGIN
       xOffset = xcenter
       yOffset = ycenter
    ENDELSE
    
    ; Set the offsets.
    Widget_Control, tlb, XOffset=xOffset, YOffset=yOffset
    
END
