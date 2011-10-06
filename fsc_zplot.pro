; docformat = 'rst'
;
; NAME:
;   FSC_ZPlot
;
; PURPOSE:
;   Allows the user to interactively zoom into a line plot. Use the left mouse button to 
;   draw a arrow on the plot. Use the right mouse button to zoom all the way back out.
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
PRO FSC_ZPlot_Cleanup, tlb

    ; The purpose of this module is to delete the pixmap window
    ; and perform other cleanup chores when the program FSC_ZPlot is
    ; destroyed.

    Widget_Control, tlb, Get_UValue=info
    IF N_Elements(info) NE 0 THEN BEGIN
       WDelete, info.pixIndex
       Ptr_Free, info.extraKeywords
    ENDIF
END ; -----------------------------------------------------------------------



PRO FSC_ZPlot_Resize, event

    ; This event handler reponds to TLB re-size events.
    
     Widget_Control, event.top, Get_UValue=info, /No_Copy
    
     ; Update window sizes.
     info.xsize = event.x
     info.ysize = event.y
    
     ; Destroy old pixmap and create a new one.
     WDelete, info.pixIndex
     Window, XSize=event.x, YSize=event.y, /Free, /Pixmap
     info.pixIndex = !D. Window
    
     ; Resize draw widget.
     Widget_Control, info.drawID, Draw_XSize=(event.x > 200), Draw_YSize=(event.y > 150)
    
     ; Draw the plot in both windows.
     cgPlot, info.indep, info.dep, $
        AXISCOLOR=info.axiscolor, $
        BACKGROUND=info.background, $
        COLOR=info.color, $
        SYMCOLOR=info.symcolor, $
        XLOG=info.xlog, $
        YLOG=info.ylog, $
        XStyle=info.xstyle, $
        XRange=info.xrange, $
        YRange=info.yrange, $
        _EXTRA=*info.extraKeywords
    
     WSet, info.drawIndex
     cgPlot, info.indep, info.dep, $
        AXISCOLOR=info.axiscolor, $
        BACKGROUND=info.background, $
        COLOR=info.color, $
        SYMCOLOR=info.symcolor, $
        XLOG=info.xlog, $
        YLOG=info.ylog, $
        XStyle=info.xstyle, $
        XRange=info.xrange, $
        YRange=info.yrange, $
        _EXTRA=*info.extraKeywords
      
     ; Update system parameters.
     info.x = !X
     info.y = !Y
     info.p = !P
    
     Widget_Control, event.top, Set_UValue=info, /No_Copy
    
END ; -----------------------------------------------------------------------



PRO FSC_ZPlot_Events, event

   ; This event handler ONLY responds to button down events from the
   ; draw widget. If it gets a DOWN event, it does three things: (1) sets
   ; the static and dynamic corners of the arrow box, (2) changes the
   ; event handler for the draw widget to FSC_ZPlot_Drawbox and turns on MOTION
   ; events, and (3) update the user's color table vectors.

    possibleEventTypes = [ 'DOWN', 'UP', 'MOTION', 'SCROLL' ]
    thisEvent = possibleEventTypes[event.type]
    IF thisEvent NE 'DOWN' THEN RETURN

    ; Must be DOWN event to get here, so get info structure.
    Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Set the static corners of the arrow box to current
    ; cursor location.
    info.xs = event.x
    info.ys = event.y

    ; Change the event handler for the draw widget and turn MOTION
    ; events ON.
    Widget_Control, event.id, Event_Pro='FSC_ZPlot_Drawbox', $
        Draw_Motion_Events=1

    ; Put the info structure back into its storage location.
    Widget_Control, event.top, Set_UValue=info, /No_Copy
    
END ; -----------------------------------------------------------------------


PRO FSC_ZPlot_Drawbox, event

   ; This event handler continuously draws and erases the arrow box until it
   ; receives an UP event from the draw widget. Then it turns draw widget motion
   ; events OFF and changes the event handler for the draw widget back to
   ; FSC_ZPlot_Events.

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
         
        ; Turn motion events off.
        Widget_Control, event.id, /CLEAR_EVENTS
        Widget_Control, event.id, Draw_Motion_Events=0, $
             Event_Pro='FSC_ZPlot_Events'   
        
        ; Put the info structure back.
        IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
        RETURN
    ENDIF
    
    ; Get the info structure out of the top-level base.      
    Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; What type of an event is this?
    possibleEventTypes = [ 'DOWN', 'UP', 'MOTION', 'SCROLL' ]
    thisEvent = possibleEventTypes[event.type]

    IF thisEvent EQ 'UP' THEN BEGIN

     ; If this is an UP event, you need to erase the zoombox, restore
     ; the user's color table, turn motion events OFF, set the
     ; draw widget's event handler back to FSC_ZPlot_Events, and
     ; draw the "zoomed" plot in both the draw widget and the pixmap.

     ; Erase the arrow box one final time by copying the plot from the pixmap.
     WSet, info.drawIndex
     Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]

     ; Turn motion events off and redirect the events to FSC_ZPlot_Events.
     Widget_Control, event.id, Draw_Motion_Events=0, $
        Event_Pro='FSC_ZPlot_Events'

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
     ; Draw the plot in both windows.
     cgPlot, info.indep, info.dep, $
        AXISCOLOR=info.axiscolor, $
        BACKGROUND=info.background, $
        COLOR=info.color, $
        SYMCOLOR=info.symcolor, $
        XLOG=info.xlog, $
        YLOG=info.ylog, $
        XStyle=info.xstyle, $
        XRange=[x1,x2], $
        YRange=info.yrange, $
        _EXTRA=*info.extraKeywords

     WSet, info.pixIndex
     cgPlot, info.indep, info.dep, $
        AXISCOLOR=info.axiscolor, $
        BACKGROUND=info.background, $
        COLOR=info.color, $
        SYMCOLOR=info.symcolor, $
        XLOG=info.xlog, $
        YLOG=info.ylog, $
        XStyle=info.xstyle, $
        XRange=[x1,x2], $
        YRange=info.yrange, $
        _EXTRA=*info.extraKeywords

      ; Update the plot system variables.
      info.x = !X
      info.y = !Y
      info.p = !P
      
      ; Clear all events waiting in the queue.
      Widget_Control, info.drawID, /CLEAR_EVENTS

      ; Put the info structure back into its storage location and then, out of here!
      Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN

    ENDIF ; thisEvent = UP


   ; Most of the action in this event handler occurs here while we are waiting
   ; for an UP event to occur. As long as we don't get it, keep erasing the
   ; old zoom box and drawing a new one.

   ; Erase the old zoom box.
   WSet, info.drawIndex
   Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]

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
    cgArrow, x1, y1, x2, y1, /Data, Color=info.arrowColor, /Solid, HSize=12, THICK=2
    IF info.ylog THEN BEGIN
       cgPlotS, [x1, x1], [10^!Y.CRange[0], 10^!Y.CRange[1]], $
            Color=info.arrowColor, THICK=2
       cgPlotS, [x2, x2], [10^!Y.CRange[0], 10^!Y.CRange[1]], $
            Color=info.arrowColor, THICK=2
    ENDIF ELSE BEGIN
       cgPlotS, [x1, x1], [!Y.CRange[0], !Y.CRange[1]], $
            Color=info.arrowColor, THICK=2
       cgPlotS, [x2, x2], [!Y.CRange[0], !Y.CRange[1]], $
            Color=info.arrowColor, THICK=2
    ENDELSE

    ; Put the info structure back into its storage location.
    Widget_Control, event.top, Set_UValue=info, /No_Copy
    
END ; -----------------------------------------------------------------------


;+
; :Description:
;   Allows the user to interactively zoom into a line plot. Use the left mouse button to 
;   draw a arrow on the plot. Use the right mouse button to zoom all the way back out.
;
; :Categories:
;    Graphics
;    
;    x: in, required, type=any
;         If X is provided without Y, a vector representing the dependent values to be 
;         plotted If both X and Y are provided, X is the independent parameter and 
;         Y is the dependent parameter to be plotted.
;    y: in, optional, type=any
;         A vector representing the dependent values to be plotted.
;       
; :Keywords:
;     arrowcolor: in, optional, type=string/integer, default='red8'
;        If this keyword is a string, the name of the zoom arrow color. By default, 'red8'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     axiscolor: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the axis color. By default, 'black'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     axescolor: in, optional, type=string/integer
;        Provisions for bad spellers.
;     background: in, optional, type=string/integer, default='white'
;        If this keyword is a string, the name of the background color. By default, 'white'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     color: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the data color. By default, 'black'.
;        Color names are those used with cgColor. Otherwise, the keyword is assumed 
;        to be a color index into the current color table.
;     group_leader: in, optional, type=long
;         The Group Leader widget identifier for this widget program.
;     symcolor: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the symbol color. By default, 'black'.
;        Color names are those used with cgColor. Otherwise, the keyword is assumed 
;        to be a color index into the current color table.
;     xlog: in, optional, type=integer, default=0
;         Set this keyword if you want the X axis to be a log axis.
;     xsize: in, optional, type=integer, default=640
;         The X size of the initial plot window.
;     ylog: in, optional, type=integer, default=0
;         Set this keyword if you want the Y axis to be a log axis.
;     ysize: in, optional, type=integer, default=512    
;         The Y size of the initial plot window.
;     _extra: in, optional
;         Any keywords appropriate for the cgPlot command.
;          
; :Examples:
;    Code examples::
;       IDL> Zplot, cgDemoData(1)
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
;        Written, 23 November 2010. DWF.
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO FSC_ZPlot, x, y, $
    ArrowColor=sarrowColor, $
    AxisColor=saxisColor, $
    Background=sbackground, $
    Color=scolor, $
    Group_Leader=group, $
    SymColor=ssymcolor, $
    XLog=xlog, $
    XSize=xsize, $
    YSize=ysize, $
    YLog=ylog, $
    _Extra=extra

    Compile_Opt idl2
    
    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

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

    ; Check for keywords. Set defaults if necessary.
    IF N_Elements(sbackground) EQ 0 THEN background = 'white' ELSE background = sbackground
    IF (N_Elements(saxescolor) EQ 0) AND (N_Elements(saxiscolor) EQ 0) THEN BEGIN
       saxiscolor = 'black'
    ENDIF
    IF N_Elements(saxescolor) NE 0 THEN axiscolor = saxescolor ELSE axiscolor = saxiscolor
    IF N_Elements(scolor) EQ 0 THEN color = 'black' ELSE color = scolor
    IF N_Elements(ssymcolor) EQ 0 THEN symcolor = 'black' ELSE symcolor = ssymcolor
    IF N_Elements(sarrowColor) EQ 0 THEN arrowColor = 'red8' ELSE arrowColor = sarrowColor
    IF N_Elements(xsize) EQ 0 THEN xsize = 620
    IF N_Elements(ysize) EQ 0 THEN ysize = 512
    xlog = Keyword_Set(xlog)
    ylog = Keyword_Set(ylog)

    ; Create a top-level base for this program. No resizing of this base.
    tlb = Widget_Base(Title='ZPlot Window', TLB_Size_Events=1)

    ; The only thing in the top-level base is a draw widget.
    ; The draw widget will have its own event handler.
    drawID = Widget_Draw(tlb, XSize=xsize, YSize=ysize, $
       Button_Events=1, Event_Pro='FSC_ZPlot_Events')
    
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
    
    cgPlot, indep, dep, $
        AXISCOLOR=axiscolor, $
        BACKGROUND=background, $
        COLOR=color, $
        SYMCOLOR=symcolor, $
        XLOG=xlog, $
        YLOG=ylog, $
        _EXTRA=extra
    
    ; Create a pixmap window the same size as the draw widget window.
    ; Store its window index number in a local variable. Draw the same
    ; plot you just put in the draw widget in the pixmap window.
    Window, /Free, XSize=xsize, YSize=ysize, /Pixmap
    pixIndex = !D.Window
    cgPlot, indep, dep, $
        AXISCOLOR=axiscolor, $
        BACKGROUND=background, $
        COLOR=color, $
        SYMCOLOR=symcolor, $
        XLOG=xlog, $
        YLOG=ylog, $
        _EXTRA=extra
    
    IF N_Elements(extra) EQ 0 THEN extraKeywords = Ptr_New(/Allocate_Heap) ELSE extraKeywords = Ptr_New(extra)
    
       ; Create an info structure to hold information required by the program.
    
    info = { $
       dep:dep, $                       ; The dependent data to be plotted.
       indep:indep, $                   ; The independent data to be plotted.
       xsize:xsize, $                   ; The X size of the draw widget.
       ysize:ysize, $                   ; The Y size of the draw widget.
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
       axiscolor: axiscolor, $
       background: background, $
       color: color, $
       symcolor: symcolor, $
       arrowColor: arrowColor, $
       extraKeywords:extraKeywords }    ; Extra plot keywords pointer.
    
     ; Store the info structure in the user value of the top-level base.
     Widget_Control, tlb, Set_UValue=info, /No_Copy
    
     ; Register this program and set up the event loop.
     XManager, 'FSC_ZPlot', tlb, Cleanup='FSC_ZPlot_Cleanup', Group_Leader=group, /No_Block, $
        Event_Handler='FSC_ZPlot_Resize'
       
END ; -----------------------------------------------------------------------


