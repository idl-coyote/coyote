;+
; NAME:
;       ZPLOT
;
; PURPOSE:
;
;       The purpose of this program is to display a line plot in a resizeable
;       graphics window which can be zoomed by drawing an "arrow box" on top
;       of it. To return to the un-zoomed plot, click and release anywhere in the window.
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;      Widgets.
;
; CALLING SEQUENCE:
;
;      ZPlot, x, y
;
; OPTIONAL INPUTS:
;
;      x: If only one positional parameter, this is assumed to be the
;         dependent data. If there are two positional parameters, this
;         is assumed to be the independent data in accordance with the
;         PLOT command.
;
;      y: The dependent data, if the X parameter is present.
;
; KEYWORD PARAMETERS:
;
;       Any valid PLOT keyword can be used with this program. In additon,
;       the following keywords are defined specifically.
;
;       GROUP_LEADER: This keyword is used to assign a group leader to this
;                 program. This program will be destroyed when the group
;                 leader is destroyed. Use this keyword if you are calling
;                 ZIMAGE from another widget program.
;
;       XLOG: Set this keyword if you wish the X axis to be logarthmic style.
;
;       YLOG: Set this keyword if you wish the Y axis to be logarthmic style.
;
;       ZOOM_XSIZE: The initial X size of the plot window. Default is 400 pixels.
;
;       ZOOM_YSIZE: The initial Y size of the plot window. Default is 400 pixels.
;
; COMMON BLOCKS:
;
;       None.
;
; SIDE EFFECTS:
;
;       Drawing colors are loaded into the color table.
;
; RESTRICTIONS:
;
;       Zooms only along the X axis.
;       Requires FSC_Color from the Coyote Library.
;
; PROCEDURE:
;
;       Click and drag the cursor to create an "arrow box". The plot
;       is zoomed into the X coordinates of the box, when released.
;       To restore unzoomed plot, click and release anywhere in the
;       window.
;
; EXAMPLE:
;
;        To display an plot you can zoom into, type:
;
;        ZPLOT
;
; MODIFICATION HISTORY:
;
;        Written by: David Fanning, 15 February 2000.
;        Modified the original rubberband box to be an "arrow box". 1 April 2000. DWF.
;        Added the ability to produce log style plots. 20 November 2001. DWF.
;        Replaced all references to GETCOLOR with FSC_COLOR. 1 March 2006. DWF.
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
PRO ZPLOT_CLEANUP, tlb

   ; The purpose of this module is to delete the pixmap window
   ; and perform other cleanup chores when the program ZPLOT is
   ; destroyed.

Widget_Control, tlb, Get_UValue=info
IF N_Elements(info) NE 0 THEN BEGIN
   WDelete, info.pixIndex
   Ptr_Free, info.extraKeywords
ENDIF
END ; of ZPLOT_CLEANUP **********************************************************



PRO ZPLOT_RESIZE, event

; This event handler reponds to TLB re-size events.

 Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Update window sizes.

info.zxsize = event.x
info.zysize = event.y

   ; Destroy old pixmap and create a new one.

WDelete, info.pixIndex
Window, XSize=event.x, YSize=event.y, /Free, /Pixmap
info.pixIndex = !D. Window

   ; Resize draw widget.

Widget_Control, info.drawID, Draw_XSize=(event.x > 200), Draw_YSize=(event.y > 150)

   ;  Set up colors for line plot.

backColor = FSC_Color('charcoal', !D.Table_Size-2)
dataColor = FSC_Color('yellow', !D.Table_Size-3)
axisColor = FSC_Color('green', !D.Table_Size-4)

   ; Draw the plot in both windows.

Plot, info.indep, info.dep, XRange=info.xrange, XStyle=info.xstyle, YRange=info.yrange, $
   Background=backColor, Color=axisColor, /NoData, _Extra=*info.extraKeywords
OPlot, info.indep, info.dep, Color=dataColor

WSet, info.drawIndex
Plot, info.indep, info.dep, XRange=info.xrange, XStyle=1, YRange=info.yrange, $
   Background=backColor, Color=axisColor, /NoData, _Extra=*info.extraKeywords
OPlot, info.indep, info.dep, Color=dataColor

   ; Update system parameters.

info.x = !X
info.y = !Y
info.p = !P

Widget_Control, event.top, Set_UValue=info, /No_Copy


END ; of ZPLOT_RESIZE **********************************************************



PRO ZPLOT_PROCESS_EVENTS, event

   ; This event handler ONLY responds to button down events from the
   ; draw widget. If it gets a DOWN event, it does three things: (1) sets
   ; the static and dynamic corners of the arrow box, (2) changes the
   ; event handler for the draw widget to ZPLOT_DRAWBOX and turns on MOTION
   ; events, and (3) update the user's color table vectors.

possibleEventTypes = [ 'DOWN', 'UP', 'MOTION', 'SCROLL' ]
thisEvent = possibleEventTypes(event.type)
IF thisEvent NE 'DOWN' THEN RETURN

    ; Must be DOWN event to get here, so get info structure.

 Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Set the static corners of the arrow box to current
    ; cursor location.

 info.xs = event.x
 info.ys = event.y

    ; Change the event handler for the draw widget and turn MOTION
    ; events ON.

 Widget_Control, event.id, Event_Pro='ZPLOT_DRAWBOX', $
    Draw_Motion_Events=1

   ; Put the info structure back into its storage location.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ; of ZPLOT_PROCESS_EVENTS *****************************************************



PRO ZPLOT_DRAWBOX, event

   ; This event handler continuously draws and erases the arrow box until it
   ; receives an UP event from the draw widget. Then it turns draw widget motion
   ; events OFF and changes the event handler for the draw widget back to
   ; ZPLOT_PROCESS_EVENTS.

   ; Get the info structure out of the top-level base.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ;  Set up colors for line plot.

backColor = FSC_Color('charcoal', !D.Table_Size-2)
dataColor = FSC_Color('yellow', !D.Table_Size-3)
axisColor = FSC_Color('green', !D.Table_Size-4)
boxColor = FSC_Color('beige', !D.Table_Size-5)

   ; What type of an event is this?

possibleEventTypes = [ 'DOWN', 'UP', 'MOTION', 'SCROLL' ]
thisEvent = possibleEventTypes(event.type)

IF thisEvent EQ 'UP' THEN BEGIN

      ; If this is an UP event, you need to erase the zoombox, restore
      ; the user's color table, turn motion events OFF, set the
      ; draw widget's event handler back to ZPLOT_PROCESS_EVENTS, and
      ; draw the "zoomed" plot in both the draw widget and the pixmap.

      ; Erase the arrow box one final time by copying the plot from the pixmap.

   WSet, info.drawIndex
   Device, Copy = [0, 0, info.zxsize, info.zysize, 0, 0, info.pixIndex]

      ; Turn motion events off and redirect the events to ZPLOT_PROCESS_EVENTS.

    Widget_Control, event.id, Draw_Motion_Events=0, $
       Event_Pro='ZPLOT_PROCESS_EVENTS'

      ; Draw the "zoomed" plot. Start by getting the new limits to the plot
      ; (i.e., the LAST zoom box outline).

   x = [info.xs, event.x]
   y = [info.ys, event.y]

      ; Make sure the x values are ordered as [min, max].

   IF info.xs GT event.x THEN x = [event.x, info.xs]

      ; Don't want exact style if we are drawing entire plot.

   IF info.xs EQ event.x THEN xstyle = 0 ELSE xstyle = 1
   info.xstyle = xstyle

      ; Restore plot system variables.

   !X = info.x
   !Y = info.y
   !P = info.p

      ; Convert the x device coordinates to data coordinates.

   coords = Convert_Coord(x, y, /Device, /To_Data)

      ; Make sure the x coordinates are within the data boundaries of the plot.

   IF info.xlog THEN BEGIN
      x1 = 10^!X.CRange(0) > coords(0,0) < 10^!X.CRange(1)
      x2 = 10^!X.CRange(0) > coords(0,1) < 10^!X.CRange(1)
   ENDIF ELSE BEGIN
      x1 = !X.CRange(0) > coords(0,0) < !X.CRange(1)
      x2 = !X.CRange(0) > coords(0,1) < !X.CRange(1)
   ENDELSE
   info.xrange = [x1,x2]

      ; Draw the "zoomed" plot in both the draw widget and the pixmap.

   yDataRange = [Min(info.dep),Max(info.dep)]
   Plot, info.indep, info.dep, XRange=[x1,x2], XStyle=xstyle, YRange=info.yrange, $
      Background=backColor, Color=axisColor, /NoData, _Extra=*info.extraKeywords, $
      XLog=info.xlog, YLog=info.ylog
   OPlot, info.indep, info.dep, Color=dataColor

   WSet, info.pixIndex
   Plot, info.indep, info.dep, XRange=[x1,x2], XStyle=xstyle, YRange=info.yrange, $
      Background=backColor, Color=axisColor, /NoData, _Extra=*info.extraKeywords, $
      XLog=info.xlog, YLog=info.ylog
   OPlot, info.indep, info.dep, Color=dataColor

      ; Update the plot system variables.

   info.x = !X
   info.y = !Y
   info.p = !P

      ; Put the info structure back into its storage location and then, out of here!

   Widget_Control, event.top, Set_UValue=info, /No_Copy
   RETURN
ENDIF ; thisEvent = UP


   ; Most of the action in this event handler occurs here while we are waiting
   ; for an UP event to occur. As long as we don't get it, keep erasing the
   ; old zoom box and drawing a new one.

   ; Erase the old zoom box.

WSet, info.drawIndex
Device, Copy = [0, 0, info.zxsize, info.zysize, 0, 0, info.pixIndex]

   ; Update the dynamic corner of the zoom box to the current cursor location.

 info.xd = event.x
 info.yd = event.y

   ; Restore plot system variables.

!X = info.x
!Y = info.y
!P = info.p

   ; Convert the x device coordinates to data coordinates.

x = [info.xs, event.x]
y = [info.ys, event.y]
coords = Convert_Coord(x, y, /Device, /To_Data)

   ; Make sure the x coordinates are within the data boundaries of the plot.

IF info.xlog THEN BEGIN
   x1 = 10^!X.CRange(0) > coords(0,0) < 10^!X.CRange(1)
   x2 = 10^!X.CRange(0) > coords(0,1) < 10^!X.CRange(1)
ENDIF ELSE BEGIN
   x1 = !X.CRange(0) > coords(0,0) < !X.CRange(1)
   x2 = !X.CRange(0) > coords(0,1) < !X.CRange(1)
ENDELSE

IF info.ylog THEN BEGIN
   y1 = 10^!Y.CRange(0) > coords(1,0) < 10^!Y.CRange(1)
   y2 = 10^!Y.CRange(0) > coords(1,1) < 10^!Y.CRange(1)
ENDIF ELSE BEGIN
   y1 = !Y.CRange(0) > coords(1,0) < !Y.CRange(1)
   y2 = !Y.CRange(0) > coords(1,1) < !Y.CRange(1)
ENDELSE

   ; Draw the arrow box.

Arrow, x1, y1, x2, y1, /Data, Color=boxColor, /Solid, HSize=12
IF info.ylog THEN BEGIN
   PlotS, [x1, x1], [10^!Y.CRange[0], 10^!Y.CRange[1]], Color=boxColor
   PlotS, [x2, x2], [10^!Y.CRange[0], 10^!Y.CRange[1]], Color=boxColor
ENDIF ELSE BEGIN
   PlotS, [x1, x1], [!Y.CRange[0], !Y.CRange[1]], Color=boxColor
   PlotS, [x2, x2], [!Y.CRange[0], !Y.CRange[1]], Color=boxColor
ENDELSE

   ; Put the info structure back into its storage location.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ; of ZPLOT_DRAWBOX ******************************************************************



PRO ZPLOT, x, y, Zoom_XSize=zxsize, Zoom_YSize=zysize, $
   Group_Leader=group, XLog=xlog, YLog=ylog, _Extra=extra

   ; This procedure allows the user to "zoom" into the data plot
   ; by drawing an arrow box around the part of the data to zoom into.
   ; The arrow box will be drawn and erased by using a pixmap and
   ; the "Device Copy" technique.

   ; On an error condition, return to the main level of IDL.

On_Error, 1

   ; Was data passed into the procedure? If not, create some.

CASE N_Params() OF

   0: BEGIN
   dep = Findgen(101)
   dep = Sin(dep/5) / Exp(dep/50)
   indep = Findgen(101)
   ENDCASE

   1: BEGIN
   dep = x
   indep = Findgen(N_Elements(dep))
   ENDCASE

   2: BEGIN
   dep = y
   indep = x
   ENDCASE

ENDCASE

   ;  Set up colors for line plot.

backColor = FSC_Color('charcoal', !D.Table_Size-2)
dataColor = FSC_Color('yellow', !D.Table_Size-3)
axisColor = FSC_Color('green', !D.Table_Size-4)

   ; Check for keywords. Set defaults if necessary.

IF N_Elements(zxsize) EQ 0 THEN zxsize = 400
IF N_Elements(zysize) EQ 0 THEN zysize = 400
xlog = Keyword_Set(xlog)
ylog = Keyword_Set(ylog)

   ; Create a top-level base for this program. No resizing of this base.

tlb = Widget_Base(Title='ZPlot Window', TLB_Size_Events=1)

   ; The only thing in the top-level base is a draw widget.
   ; The draw widget will have its own event handler.

drawID = Widget_Draw(tlb, XSize=zxsize, YSize=zysize, $
   Button_Events=1, Event_Pro='ZPLOT_PROCESS_EVENTS')

   ; Realize the program.

Widget_Control, tlb, /Realize

   ; Get the window index number of the draw widget.
   ; Make the draw widget the current graphics window
   ; and draw the plot of the data in it. Make the
   ; X data range exactly fit the data.

Widget_Control, drawID, Get_Value=drawIndex
WSet, drawIndex
xrange = [Min(indep), Max(indep)]
yrange=[Min(dep),Max(dep)]

Plot, indep, dep, XRange=xrange, YRange=yrange, XLog=xlog, YLog=ylog, $
   Background=backColor, Color=axisColor, /NoData, _Extra=extra
OPlot, indep, dep, Color=dataColor

  ; Create a pixmap window the same size as the draw widget window.
  ; Store its window index number in a local variable. Draw the same
  ; plot you just put in the draw widget in the pixmap window.

Window, /Free, XSize=zxsize, YSize=zysize, /Pixmap
pixIndex = !D.Window
Plot, indep, dep, XRange=xrange, YRange=yrange, XLog=xlog, YLog=ylog, $
   Background=backColor, Color=axisColor, /NoData, _Extra=extra
OPlot, indep, dep, Color=dataColor

IF N_Elements(extra) EQ 0 THEN extraKeywords = Ptr_New(/Allocate_Heap) ELSE extraKeywords = Ptr_New(extra)

   ; Create an info structure to hold information required by the program.

info = { $
   dep:dep, $                       ; The dependent data to be plotted.
   indep:indep, $                   ; The independent data to be plotted.
   zxsize:zxsize, $                 ; The X size of the draw widget.
   zysize:zysize, $                 ; The Y size of the draw widget.
   drawID:drawID, $                 ; The widget identifier of the draw widget.
   drawIndex:drawIndex, $           ; The draw window index number.
   pixIndex:pixIndex, $             ; The pixmap window index number.
   xrange:xrange, $                 ; The current X range of the plot.
   yrange:yrange, $                 ; The current Y range of the plot.
   xlog:xlog, $                     ; A flag for X logarithmic axis style.
   ylog:ylog, $                     ; A flag for Y logarithmic axis style.
   xstyle:0, $                      ; The setting for the XStyle keyword.
   xs:0, $                          ; X static corner of the zoom box.
   ys:0, $                          ; Y static corner of the zoom box.
   xd:0, $                          ; X dynamic corner of the zoom box.
   yd:0, $                          ; Y dynamic corner of the zoom box.
   x:!X, $                          ; The !X system variable after plot.
   y:!Y, $                          ; The !Y system variable after plot.
   p:!P, $                          ; The !P system variable after plot.
   extraKeywords:extraKeywords }    ; Extra plot keywords pointer.

   ; Store the info structure in the user value of the top-level base.

Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Register this program and set up the event loop.

XManager, 'ZPLOT', tlb, Cleanup='ZPLOT_Cleanup', Group_Leader=group, /No_Block, $
   Event_Handler='ZPLOT_Resize'
END ; of ZPLOT ****************************************************************************
