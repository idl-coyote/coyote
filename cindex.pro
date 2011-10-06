;+
; NAME:
;       CIndex
;
; PURPOSE:
;       This is a program for viewing the current colors in the
;       colortable with their index numbers overlayed on each color.
;       On 24-bit systems you must click the cursor in the graphics window
;       to see the colors in the current color table.
;
; AUTHOR:
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY: Graphics
;
; CALLING SEQUENCE:  CIndex
;
; INPUTS:   None.
;
; INPUT KEYWORDS:   
;
;  BREWER:     If this keyword is set, the BREWER colors will be loaded with the
;              Change Colors button. (Assuming the brewer color table file, fsc_brewer.tbl,
;              has been installed.
;
; OUTPUTS:  None
;
; OPTIONAL OUTPUTS:  None
;
; OUTPUT KEYWORDS:
;
;   NOTIFYID:   A two-element array containing the Change Colors button widget
;               identifier and the identifier of the top-level base widget. This
;               array is meant to be sent to an XCOLORS routine via its NOTIFYID
;               keyword. This will allow instant updating of the CINDEX interface.
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:   None
;
; RESTRICTIONS:   Reqires XCOLORS and cgImage from the Coyote Library:
;
;                     http://www.idlcoyote.com/programs/xcolors.pro
;                     http://www.idlcoyote.com/programs/cgImage.pro
;
; PROCEDURE:
;
;  Draws a 31x25 set of small rectangles in 256 different colors.
;  Writes the color index number on top of each rectangle.
;
; MODIFICATION HISTORY:  Written by David Fanning, May 1995
;
;  Widgetized and made it work in 24-bit color. Colors are
;     updated by clicking in window. 22 Oct 98. DWF
;  Replace POLYFILL with TV command to avoid underflow error in
;     Z-buffer. 8 March 99. DWF
;  Fixed a problem with 24-bit devices with color decomposition ON. 15 Feb 2000. DWF.
;  Added the NOTIFYID keyword, 15 Dec 2005. DWF.
;  Added BREWER keyword, 19 May 2008. DWF.
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
PRO CIndex_Colors, event

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; What kind of event is this?

thisEvent = Tag_Names(event, /Structure_Name)
CASE thisEvent OF

   'WIDGET_BUTTON': XColors, Group_Leader=event.top, $
      NotifyID=[event.id, event.top], BREWER = info.brewer

   'XCOLORS_LOAD': BEGIN
      Device, Get_Visual_Depth=thisDepth
      IF thisDepth GT 8 THEN BEGIN
         thisID = !D.Window
         WSet, info.wid
         TV, info.snap
         WSet, thisID
      ENDIF

      ENDCASE
ENDCASE
Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;---------------------------------------------------------------------



PRO CIndex_Event, event
IF event.type NE 1 THEN RETURN
Widget_Control, event.top, Get_UValue=info, /No_Copy
thisID = !D.Window
WSet, info.wid

   ; Use cgImage if you can. If not, use TV.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   Device, Decomposed=0
   TV, info.snap
   GOTO, skip_cgImage
ENDIF

cgImage, info.snap

skip_cgImage:

WSet, thisID
Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;---------------------------------------------------------------------



PRO CIndex, NOTIFYID=notifyID, BREWER=brewer
oldWindowID = !D.Window
thisDevice = !D.Name
Set_Plot, 'Z'
Device, Set_Resolution=[496,400]

   ; Set the starting index for the polygons.

xindex = 0
yindex = 0

   ; Start drawing. There are 16 rows and 16 columns of colors.

FOR i=0,15 DO BEGIN

    y = [yindex, yindex+25, yindex+25, yindex, yindex]
    yindex = yindex+25
    xindex = 0

    FOR j=0,15 DO BEGIN

        x = [xindex, xindex, xindex+31, xindex+31, xindex]
        color = j+(i*16)

           ; Draw the polygon in a specfic color.

        ;Polyfill, x, y, /Device, Color=color
        TV, Replicate(color,31,25), j*31, i*25 ; To avoid bug in Z-buffer.
        output = StrTrim(j+(i*16), 2)

           ; Draw the index number in the "opposite" color.

        XYOutS, xindex+8, yindex-15, output, Color=Byte(255-color), $
           /Device, Charsize=0.75

           ; Reset the xindex number.

        xindex = xindex+31

    ENDFOR

ENDFOR

   ; Take a snapshot of the Z-Buffer.

snap = TVRD()

Set_Plot, thisDevice

tlb = Widget_Base(Title='Color Index Numbers', $
   TLB_Frame_Attr=1, MBar=menuID)
controlID = Widget_Button(menuID, Value='Change Colors')
xcolorsID = Widget_Button(controlID, Value='XColors', $
   Event_Pro='CIndex_Colors')
notifyID = [xcolorsID, tlb] ; Widgets to notify if colors change.
drawID = Widget_Draw(tlb, XSize=496, YSize=400, /Button_Events)

   ; Center the top-level base.

Device, Get_Screen_Size=screenSize
xCenter = screenSize(0) / 2
yCenter = screenSize(1) / 2

geom = Widget_Info(tlb, /Geometry)
xHalfSize = geom.Scr_XSize / 2
yHalfSize = geom.Scr_YSize / 2

Widget_Control, tlb, XOffset = xCenter-xHalfSize, $
   YOffset = yCenter-yHalfSize

   ; Realize the top-level base.

Widget_Control, tlb, /Realize
Widget_Control, drawID, Get_Value=wid
WSet, wid

   ; Use cgImage if you can. If not, use TV.

Catch, theError
IF theError NE 0 THEN BEGIN
   Device, Decomposed=0
   TV, snap
   GOTO, skip_cgImage
ENDIF

cgImage, snap

Catch, /Cancel
Skip_cgImage:

info = {snap:snap, wid:wid, brewer:Keyword_Set(brewer)}
Widget_Control, tlb, Set_UValue=info, /No_Copy
WSet, oldWindowID
XManager, 'cindex', tlb, /No_Block
END
