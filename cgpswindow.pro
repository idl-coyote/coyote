; docformat = 'rst'
;
; NAME:
;   cgPSWindow
;
; PURPOSE:
;   This function is used to calculate the size of a PostScript window that has the same 
;   aspect ratio (ratio of height to width) as the current display graphics window. It 
;   creates the largest possible PostScript output window with the desired aspect ratio. 
;   This assures that PostScript output looks similar, if not identical, to normal 
;   graphics output on the display.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2014, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; This function is used to calculate the size of a PostScript window that has the same 
; aspect ratio (ratio of height to width) as the current display graphics window. It 
; creates the largest possible PostScript output window with the desired aspect ratio. 
; This assures that PostScript output looks similar, if not identical, to normal 
; graphics output on the display.
;
; :Categories:
;    Utilities, Graphics
;  
; :Examples:
;    To create a PostScript output window with the same aspect
;    ratio as the curently active display window, type::
;
;       pageInfo = cgPSWINDOW()
;       SET_PLOT, 'PS'
;       DEVICE, _Extra=pageInfo
;
;    To configure the PRINTER device::
;
;       pageInfo = cgPSWINDOW(/Printer, Fudge=0.25)
;       SET_PLOT, 'PRINTER'
;       DEVICE, _Extra=pageInfo
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
;       Written by: David W. Fanning, November 1996.
;       Fixed a bug in which the YOFFSET was calculated incorrectly
;          in Landscape mode. 12 Feb 97.
;       Took out a line of code that wasn't being used. 14 Mar 97.
;       Added correct units keyword to return structure. 29 JUN 98. DWF
;       Fixed a bug in how landscape offsets were calculated. 19 JUL 99. DWF.
;       Fixed a bug in the way margins were used to conform to my
;          original conception of the program. 19 JUL 99. DWF.
;       Added Landscape and Portrait fields to the return structure. 19 JUL 99. DWF.
;       Added PageSize keyword, changed MARGIN keyword, and completely
;          rewrote most of the intenal code. 9 FEB 2000. DWF.
;       Fixed a bug in how I calculated the aspect ratio. 1 MAR 2000. DWF.
;       Added PRINTER keyword to return proper offset values for the
;          PRINTER device, where the offset location is not rotated. 1 MAR 2000. DWF.
;       Added PRINTER fudge factors to take into account that printer offsets are
;          calculated from the printable area of the paper, rather than the corner
;          of the paper. 8 AUG 2000. DWF.
;       Changed the default margin to 0.05 from 0.15. 29 Nov 2004, DWF.
;       Added EUROPEAN keyword and set LANDSCAPE mode if window wider than higher
;           as the default if LANDSCAPE is not set. 13 Dec 2010. DWF.
;       Added ASPECTRATIO keyword to allow user-specified window aspect ratio. 13 Dec 2010. DWF.
;       Depreciated EUROPEAN keyword in favor of METRIC. 31 Jan 2011. DWF.
;       Now setting LANDSCAPE=0 if aspect GT 1 and not set otherwise. 19 Feb 2013. DWF.
;       Renamed cgPSWIndow from PSWindow. 10 Feb 2014. DWF.
;       Added SANE_OFFSETS keyword. 10 Feb 2014. DWF.
;
; :Copyright:
;     Copyright (c) 1996-2014, Fanning Software Consulting, Inc.
;-

;+
; This function produces a position vector in the output window, given
; a desired aspect ratio and window size.
; 
; :Returns:
;    A four-element vector that can be used as the normal "position" vector
;    to locate graphics in display output.
; 
; :Params:
;    aspectRatio: in, optional, type=float, default=1.0
;       The desired aspect ratio of the output "window" on the output device.
;       The aspect ratio is calculated as the ratio height/width.
;
; :Keywords:
;    margin: in, optional, type=float, default=0.15
;       An optional margin to calculate around the edge of the output window.
;       A number between 0.0 and 0.35.
;    windowaspect: in, optional, type=float
;       The aspect ratio of the "window" the graphics output will be displayed in.
;-
FUNCTION cgPSWINDOW_ASPECT, aspectRatio, $
    MARGIN=margin, $
    WINDOWASPECT=windowAspect

    ON_ERROR, 1
    
    ; Check for aspect ratio parameter and possibilities.
    IF N_PARAMS() EQ 0 THEN aspectRatio = 1.0
    IF aspectRatio EQ 0 THEN BEGIN
       MESSAGE, 'Aspect Ratio of 0. Changing to 1...', /Informational
       aspectRatio = 1.0
    ENDIF
    
    dataType = SIZE(aspectRatio, /TNAME)
    IF dataType NE 'FLOAT' THEN BEGIN
       MESSAGE, 'Aspect Ratio is not a FLOAT. Take care...', /Informational
       aspectRatio = Float(aspectRatio)
    ENDIF
    
    ; Check for margins.
    IF N_ELEMENTS(margin) EQ 0 THEN margin = 0.15
    
    ; Error checking.
    IF margin LT 0 OR margin GE 0.35 THEN $
       MESSAGE, 'The MARGIN keyword value must be between 0.0 and 0.35.'
    
    ; Calculate the aspect ratio of the current window.
    IF N_Elements(windowAspect) EQ 0 THEN windowAspect = FLOAT(!D.Y_VSIZE) / !D.X_VSIZE
    
    ; Calculate normalized positions in window.
    IF (aspectRatio LE windowAspect) THEN BEGIN
       xstart = margin
       ystart = 0.5 - (0.5 - margin) * (aspectRatio / windowAspect)
       xend = 1.0 - margin
       yend = 0.5 + (0.5 - margin) * (aspectRatio / windowAspect)
    ENDIF ELSE BEGIN
       xstart = 0.5 - (0.5 - margin) * (windowAspect / aspectRatio)
       ystart = margin
       xend = 0.5 + (0.5 - margin) * (windowAspect / aspectRatio)
       yend = 1.0 - margin
    ENDELSE
    
    ; Calculate a window position in normalized coordinates.
    position = [xstart, ystart, xend, yend]
    
    ; Return the position.
    RETURN, position
    
END ; ----------------------------------------------------------------------------------


;+
; This function returns a keyword structure that can be used to configure
; the PostScript device for graphics output.
;
; :Returns:
;    The output value is a named structure defined like this::
;
;       pageInfo = {CGPSWINDOW_STRUCT, XSIZE:0.0, YSIZE:0.0, $
;          XOFSET:0.0, YOFFSET:0.0, INCHES:0, PORTRAIT:0, LANDSCAPE:0}
;
;    The units of the four size fields are inches unless the CM or METRIC keywords
;    are set. The output can be used to immediately configure the PostScript
;    or Printer device, like this::
;
;       Set_Plot, 'PS' ; or 'PRINTER'
;       Device, _Extra=pageInfo
;
; :Keywords:
;    aspectRatio: in, optional, type=float, default=1.0
;       The desired aspect ratio of the output "window" on the output device.
;       The aspect ratio is calculated as the ratio height/width.
;    cm: in, optional, type=boolean, default=0
;        Set this keyword to return sizes and offsets in centimeters instead of inches.
;    european: in, optional, type=boolean, default=0
;        A depreciated keyword. Use `Metric` instead.
;    fudge: in, optional, type=float
;       A quick way to set symetrical XFUDGE and YFUDGE factors.
;       If this keyword is set to a value, XFUDGE and YFUDGE keywords are
;       set to the same value. Fudge factors are used only with some
;       printers and generally only when output is being sent to the
;       PRINTER device. See the description of the `XFudge` and `YFudge`
;       keywords for additional information.
;    landscape: in, optional, type=boolean, default=0
;        Set this keyword to return sizes and offsets in landscape mode.
;    margin: in, optional, type=float, default=0.15
;       An optional margin to calculate around the edge of the output window.
;       A number between 0.0 and 0.35.
;    metric: in, optional, type=boolean, default=0
;        Set this keyword to change the `Pagesize` to A4 and set the `CM` keyword for
;        metric or European measurements.
;    pagesize: in, optional, type=string, default='LETTER'
;       Set this keyword to a string indicating the type of PostScript page size you want. 
;       Allowed values are "LETTER", "LEGAL", and "A4". 
;    printer: in, optional, type=boolean, default=0
;       Set this keyword to produce keyword values appropriate for the PRINTER device.
;    sane_offsets: in, optional, type=boolean, default=0.0
;        Configuring the PostScript device when in Landscape mode is pretty much insane.
;        The problem is the PostScript page rotates and the X and Y offsets change
;        directions, although the X and Y sizes do not. Nevertheless, this is how it
;        is done, so normally these insane, mixed up values are returned so they can be
;        passed directly to the PostScript device. Coyote Graphics routines, however, 
;        reply on cgPS_Config to configure the PostScript device, and this routine uses
;        sane offset values, which are *always* calculated from the lower-left corner of 
;        the display window, no matter how the page is rotated. If you are passing these
;        values into cgPS_Config, you want to set this keyword.
;    xfudge: in, optional, type=float
;       Printers calculate the offset point from the printable
;       edge of the paper (sometimes), rather from the corner of the paper.
;       For example, on my Lexmark printer, both X and Y offsets are
;       calculated from a point 0.25 inches in from the edge. This keyword
;       allows you to set a "fudge" factor that will be subtracted from
;       the XOFFSET that is returned to the user. This allows you to create
;       output that is centered on the page. The fudge factor should be in
;       the same units as the returned size and offset values.
;    yfudge: in, optional, type=float
;       Printers calculate the offset point from the printable
;       edge of the paper (sometimes), rather from the corner of the paper.
;       For example, on my Lexmark printer, both X and Y offsets are
;       calculated from a point 0.25 inches in from the edge. This keyword
;       allows you to set a "fudge" factor that will be subtracted from
;       the YOFFSET that is returned to the user. This allows you to create
;       output that is centered on the page. The fudge factor should be in
;       the same units as the returned size and offset values.
;-
FUNCTION cgPSWINDOW, $
    ASPECTRATIO=aspectRatio, $
    CM=cm, $
    EUROPEAN=european, $
    FUDGE=fudge, $
    LANDSCAPE=landscape, $
    MARGIN=margin, $
    METRIC=metric, $
    PAGESIZE=pagesize, $
    PRINTER=printer, $
    SANE_OFFSETS=sane_offsets, $
    XFUDGE=xfudge, $
    YFUDGE=yfudge
    
    Compile_Opt idl2
    
    On_Error, 2 ; Return to caller.

    ; Depreciated keywords.
    IF N_Elements(metric) EQ 0 THEN metric = Keyword_Set(european) ELSE metric = Keyword_Set(metric)

    ; Set up default values and check keywords.
    IF Keyword_Set(metric) THEN BEGIN
        cm = 1
        pagesize = 'A4'
    ENDIF
    IF N_Elements(landscape) EQ 0 THEN BEGIN
        IF N_Elements(aspectRatio) NE 0 THEN BEGIN
            IF aspectRatio LT 1.0 THEN landscape = 1 ELSE landscape = 0
        ENDIF ELSE BEGIN
            IF !D.Y_VSIZE LT !D.X_VSIZE THEN landscape = 1
        ENDELSE
    ENDIF
    landscape = Keyword_Set(landscape)
    cm = Keyword_Set(cm)
    printer = Keyword_Set(printer)
    
    ; The PRINTER device uses sane offsets.
    IF printer THEN sane_offsets = 1
    
    inches = 1 ; Work in inches until later.
    
    ; Set up printer fudge factors, if necessary.
    IF N_Elements(fudge) NE 0 THEN BEGIN
       xfudge = fudge
       yfudge = fudge
    ENDIF
    IF N_Elements(xfudge) EQ 0 THEN xfudge = 0.0
    IF N_Elements(yfudge) EQ 0 THEN yfudge = 0.0
    
    ; Get the page size.
    IF N_Elements(pagesize) EQ 0 THEN pagesize = 'LETTER' $
       ELSE pagesize = StrUpCase(pagesize)
    CASE pagesize OF
       'LETTER': BEGIN
          shortside = 8.5
          longside = 11.0
          ENDCASE
       'LEGAL': BEGIN
          shortside = 8.5
          longside = 14.0
          ENDCASE
        'A4': BEGIN
          shortside = 8.27
          longside = 11.7
          ENDCASE
        ELSE: BEGIN
          Message, 'Unknown page size. Using LETTER...', /Informational
          shortside = 8.5
          longside = 11.0
          ENDCASE
    ENDCASE
    
    ; Need measurements in centimeters?
    IF KEYWORD_SET(cm) THEN BEGIN
          shortside = shortside * 2.54
          longside = longside * 2.54
          inches = 0
    ENDIF
    
    ; Determine the margin of the window on the page.
    IF N_ELEMENTS(margin) EQ 0 THEN margin=0.05
    
    ; Get the aspect ratio of the current display window if needed. Aspect ratio
    ; is ratio of ysize/xsize.
    IF N_Elements(aspectRatio) EQ 0 THEN aspectRatio = FLOAT(!D.Y_VSIZE) / !D.X_VSIZE
    
    ; Get the aspect ratio of the page.
    IF Keyword_Set(landscape) THEN pAspectRatio = shortside / longside $
       ELSE pAspectRatio = longside / shortside
    
    ; Get the position on the page for this window.
    pos = cgPSWindow_Aspect(aspectRatio, Margin=margin, WindowAspect=pAspectRatio)
    
    ; Convert normalized position coordinates to size units.
    IF KEYWORD_SET(landscape) THEN BEGIN
       IF printer THEN BEGIN
          xsize = (pos[2] - pos[0]) * longside
          ysize = (pos[3] - pos[1]) * shortside
          yoffset = pos[1] * shortside - yfudge
          xoffset = pos[0] * longside - xfudge
          landscape = 1
          portrait = 0
       ENDIF ELSE BEGIN
          xsize = (pos[2] - pos[0]) * longside
          ysize = (pos[3] - pos[1]) * shortside
          xoffset = pos[1] * shortside
          yoffset = longside - (pos[0] * longside)
          
          ; Do you need sane offsets? These are the offsets as cgPS_Config expects them.
          IF Keyword_Set(sane_offsets) THEN BEGIN
              temp = xoffset
              xoffset = longside-yoffset
              yoffset = temp
          ENDIF
          landscape = 1
          portrait = 0
       ENDELSE
    ENDIF ELSE BEGIN
       xsize = (pos[2] - pos[0]) * shortside
       ysize = (pos[3] - pos[1]) * longside
       IF printer THEN BEGIN
          xoffset = pos[0] * shortside - xfudge
          yoffset = pos[1] * longside - yfudge
       ENDIF ELSE BEGIN
          xoffset = pos[0] * shortside
          yoffset = pos[1] * longside
       ENDELSE
       landscape = 0
       portrait = 1
    ENDELSE
    
    
    ; Return the proper DEVICE data structure.
    RETURN, {CGPSWINDOW_STRUCT, XSIZE:xsize, YSIZE:ysize, $
       XOFFSET:xoffset, YOFFSET:yoffset, INCHES:inches, $
       PORTRAIT:portrait, LANDSCAPE:landscape}
    
END  ; ----------------------------------------------------------------------------------
