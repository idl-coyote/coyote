; docformat = 'rst'
;
; NAME:
;   cgZPlot__Define
;
; PURPOSE:
;   This program creates a "zoomable" line plot in an interactive window. The user can
;   zoom into or out of the plot. Once a plot is zoomed, the user can then pan the plot
;   in both the X and Y directions. See the operating instructions for how to interact
;   with the line plot.
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
;+
;  This program creates a "zoomable" line plot in an interactive, resizable window. The user 
;  can zoom into or out of the plot. Once a plot is zoomed, the user can then pan the plot
;  in both the X and Y directions. See the operating instructions for how to interact
;  with the line plot.
;  
;  Operating Instructions--
;  
;  Use the LEFT mouse button to zoom the plot and the RIGHT mouse button to pan the plot.
;  
;  If you click and drag inside the plot axes, you will create a rubber band box. Select the
;  portion of data you wish to zoom into. The zoom will occur in both the X and Y directions.
;  If you wish to zoom the plot all the way back out, simply click and release the LEFT mouse
;  button inside the plot axes without moving the mouse.
;  
;  Once you are zoomed into a plot, you can adjust the zoom by clicking the LEFT mouse button
;  outside the plot axes. If you click the mouse below the plot, you will cause the X axis to
;  zoom out of the plot by the zoomFactor amount (normally 5% of the current range of the axis).
;  If you wish to zoom the X axis into the plot, simply click above in the region of the window
;  above the plot. Click below the plot to zoom out, above the plot to zoom in. Similarly, you 
;  can adjust the zoom on the Y axis. Clicking to the left of the plot zooms the Y axis out, 
;  while clicking to the right of the plot zooms the Y axis in.
;  
;  If you are zoomed into the plot, you can pan to different locations in the plot by using
;  the RIGHT mouse button. Hold and drag the RIGHT mouse button inside the plot axes. The
;  entire plot will pan in both the X and Y directions.
;  
;  File output requires that ImageMagick and GhostScript be installed on your machine. Note
;  that exact axis scaling is always in effect.
;
;  The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
;  to be installed on your machine.
;        
; :Categories:
;    Graphics
;    
; :Examples:
;    Plot examples::
;       cgZPlot, cgDemodata(1), PSYM=2, Color='dodger blue'
;       
;    To put this in your own widget program::
;        tlb = Widget_Base(Title='My Program')
;        cgZPlot, cgDemodata(1), PSYM=2, Color='dodger blue', Parent=tlb
;        Widget_Control, tlb, /Realize
;        Widget_Control, 'myprogram', tlb, /NoBlock
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
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;     
; :History:
;     Change History::
;        Written 16 May 2012, by David W. Fanning.
;        Added UNDO capability arbitrarily set to 50 items. 17 May 2012. DWF.
;        Added a REDO capability and the ability to adjust the Y range (via a button) so that 
;           you can see the actual data Y range of all the data in the X range of a particular
;           view of the data. 21 May 2012. DWF.
;        Added a PARENT keyword and changed the algorithm slightly so that this
;           interactive widget functionality can be incorporated into your own
;           widget programs. 21 may 2012. DWF.
;        Added compile options idl2 to all modules. Fixed a typo for REDO button. 14 June 2012. DWF.
;        Separated the object code from the driver code for easier inheritance. 14 June 2012. DWF.
;        Removed the POLAR keyword, which can't be used in a zoom plot. 15 June 2012. DWF.
;        Added a persistent output save directory.  30 June 2012. DWF.
;        Added an ERASE method to erase the current display. 10 July 2012. DWF.
;        Added a LABEL keyword to add a label instead of a title to a plot. 13 July 2012. DWF.
;        Added the ability to include overplot objects in the zoom window. 17 July 2012. DWF.
;        Added a Destroy method and now remove widget GUI in CLEANUP method. 2 Oct 2012. DWF.
;-

;+
; This is the initialization method of the cgZPlot object. In general, any keyword appropriate
; for the cgPlot command can be used with this program.
; 
; :Params:
;    x: in, required, type=any
;         If X is provided without Y, a vector representing the dependent values to be 
;         plotted If both X and Y are provided, X is the independent parameter and 
;         Y is the dependent parameter to be plotted.
;    y: in, optional, type=any
;         A vector representing the dependent values to be plotted.
;       
; :Keywords:
;     aspect: in, optional, type=float, default=none
;        Set this keyword to a floating point ratio that represents the aspect ratio 
;        (ysize/xsize) of the resulting plot. The plot position may change as a result
;        of setting this keyword. Note that `Aspect` cannot be used when plotting with
;        !P.MULTI.
;     label: in, optional, type=string
;        A label is similar to a plot title, but it is aligned to the left edge
;        of the plot and is written in hardware fonts. Use of the label keyword
;        will suppress the plot title.
;     legends: in, optional, type=object
;         A single cgLegendItem object, or an array of cgLegendItem objects that will be
;         drawn on the plot as a legend.
;     max_value: in, optional, type=float
;        Set this keyword to the maximum value to plot. Any values greater than this 
;        value are treated as missing.
;     min_value: in, optional, type=float
;        Set this keyword to the minimu value to plot. Any values smaller than this 
;        value are treated as missing.
;     oplots: in, optional, type=object
;         A single cgOverPlot object, or an array of cgOverPlot objects that will be
;         overplot on the axes set up by the original data.
;     parent: in, optional, type=long
;        The identifer of the parent widget for this program's draw widget. If not
;        provided, the program will create it's own top-level base widget as a parent.
;     xlog: in, optional, type=boolean, default=0
;        Set this keyword to use a logarithmic X axis
;     xrange: in, optional, type=double
;        Set this keyword to a two-element array giving the X data range of the plot.
;     xsize: in, optional, type=int, default=640
;        The X size of the program's draw widget.
;     ylog: in, optional, type=boolean, default=0
;        Set this keyword to use a logarithmic Y axis
;     ynozero: in, optional, type=boolean, default=0
;        Set this keyword to use allow the Y axis to start at a value other than zero.
;     yrange: in, optional, type=double
;         Set this keyword to a two-element array giving the Y data range of the plot.
;     ysize: in, optional, type=int, default=512
;         The Y size of the program's draw widget.
;     zoomfactor: in, optional, type=float
;         Set this keyword to a number between 0.01 and 0.25. This affects the amount
;         of zooming when the X axis and Y axis are zoomed with the LEFT mouse button.
;         The default value is 0.05 or five percent of the current axis range on each
;         end of the axis, resulting in a 10 percent change in the axis length.
;     _ref_extra: in, optional, type=any
;        Any keyword appropriate for the IDL Plot or Coyote Graphic cgPlot command is 
;        allowed in the program. Note that this is not the same as saying it is a good
;        idea to use every one of the these keywords. Use good judgement.
;-
FUNCTION cgZPlot::INIT, x, y, $
    ASPECT=aspect, $
    DRAWID=drawid, $
    LABEL=label, $
    LEGENDS=legends, $
    MAX_VALUE=max_value, $
    MIN_VALUE=min_value, $
    OPLOTS=oplots, $
    PARENT=parent, $
    XLOG=xlog, $
    XRANGE=xrange, $
    XSIZE=xsize, $
    YLOG=ylog, $
    YRANGE=yrange, $
    YNOZERO=ynozero, $
    YSIZE=ysize, $
    ZOOMFACTOR=zoomfactor, $
    _REF_EXTRA=extra
    
    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN, 0
    ENDIF
    
    ; Set parameters and arguments.
    IF N_Elements(zoomfactor) EQ 0 THEN zoomfactor = 0.05 ELSE zoomfactor = 0.01 > zoomfactor < 0.25
    
    ; Call the superclass object INIT method.
    IF ~self -> cgGraphicsKeywords::INIT(_STRICT_EXTRA=extra) THEN RETURN, 0   
    
    ; Sort out which is the dependent and which is independent data.
    CASE N_Params() OF
      
       1: BEGIN
       dep = x
       indep = Findgen(N_Elements(dep))
       ENDCASE
    
       2: BEGIN
       dep = y
       indep = x
       ENDCASE
    
    ENDCASE
    
    ; If either of these input vectors are scalars, make them vectors.
    IF N_Elements(dep) EQ 1 THEN dep = [dep]
    IF N_Elements(indep) EQ 1 THEN indep = [indep]
    
    ; Allocate heap for variables.
    self.indep = Ptr_New(/Allocate_Heap)
    self.dep = Ptr_New(/Allocate_Heap)
    self.aspect = Ptr_New(/Allocate_Heap)
    self.max_value = Ptr_New(/Allocate_Heap)
    self.min_value = Ptr_New(/Allocate_Heap)
    self.xlog = Ptr_New(/Allocate_Heap)
    self.ylog = Ptr_New(/Allocate_Heap)
    self.ynozero = Ptr_New(/Allocate_Heap)
    self.undoList = Obj_New('LinkedList')
    self.redoList = Obj_New('LinkedList')
    IF N_Elements(oplots) NE 0 THEN self.oplots = Ptr_New(oplots) ELSE self.oplots = Ptr_New(/ALLOCATE_HEAP)
    IF N_Elements(legends) NE 0 THEN self.legends = Ptr_New(legends) ELSE self.legends = Ptr_New(/ALLOCATE_HEAP)
    
    self -> SetProperty, $
        INDEP=indep, $
        DEP=dep, $
        ASPECT=aspect, $
        MAX_VALUE=max_value, $
        MIN_VALUE=min_value, $
        XLOG=xlog, $
        XRANGE=xrange, $
        YLOG=ylog, $
        YRANGE=yrange, $
        YNOZERO=ynozero
        
    ; Set the draw widget size.
    IF N_Elements(xsize) EQ 0 THEN self.xsize = 640 ELSE self.xsize = xsize
    IF N_Elements(ysize) EQ 0 THEN self.ysize = 512 ELSE self.ysize = ysize
    
    ; Do you need to create your own TLB, or will you be using someone else's?
    IF N_Elements(parent) EQ 0 THEN BEGIN
        self.tlb = Widget_Base(Title='Zoom/Pan Plot', TLB_SIZE_EVENTS=1, $
           UVALUE={method:'TLB_RESIZE_EVENTS', object:self}, MBar=menuID)
           
        ; Menu items.
        fileID = Widget_Button(menuID, Value='File')
        output = Widget_Button(fileID, Value='Save As...', /Menu)
        button = Widget_Button(output, Value='BMP File', UVALUE={method:'FileOutput', object:self})
        button = Widget_Button(output, Value='EPS File', UVALUE={method:'FileOutput', object:self})
        button = Widget_Button(output, Value='GIF File', UVALUE={method:'FileOutput', object:self})
        button = Widget_Button(output, Value='JPEG File', UVALUE={method:'FileOutput', object:self})
        button = Widget_Button(output, Value='PDF File', UVALUE={method:'FileOutput', object:self})
        button = Widget_Button(output, Value='PS File', UVALUE={method:'FileOutput', object:self})
        button = Widget_Button(output, Value='PNG File', UVALUE={method:'FileOutput', object:self})
        button = Widget_Button(output, Value='TIFF File', UVALUE={method:'FileOutput', object:self})
        
        button =  Widget_Button(fileID, Value='Undo', ACCELERATOR="Ctrl+U", $
            UVALUE={method:'Undo', object:self}, /Separator)
        button =  Widget_Button(fileID, Value='Redo', ACCELERATOR="Ctrl+R", $
            UVALUE={method:'Redo', object:self})
            
        button = Widget_Button(fileID, Value='Adjust Range to Data Viewed', ACCELERATOR="Ctrl+A", $
            UVALUE={method:'AdjustRange', object:self}, /Separator)
        
        button =  Widget_Button(fileID, Value='Quit', UVALUE={method:'Quit', object:self}, /Separator)
    ENDIF ELSE self.tlb = parent
    
    ; Create the draw widget and pixmap. These are the essential elements of this object
    ; and should work in any parent widget.
    retain = (StrUpCase(!Version.OS_Family) EQ 'UNIX') ? 2 : 1
    self.drawID = Widget_Draw(self.tlb, XSize=self.xsize, YSize=self.ysize, $
       UVALUE={method:'BUTTON_EVENTS', object:self}, $
       RETAIN=retain, Button_Events=1, $
       NOTIFY_REALIZE='cgzplot_notify_realize', $
       EVENT_PRO='cgZplot_Events')
    Window, /Pixmap, /Free, XSize=self.xsize, YSize=self.ysize
    self.pixmapID = !D.Window
    
    ; Set object properties.
    self.drag = 0
    IF N_Elements(label) NE 0 THEN self.label = label
    IF N_Elements(xrange) EQ 0 THEN xrange = [Min(indep), Max(indep)]
    IF N_Elements(yrange) EQ 0 THEN yrange = [Min(dep), Max(dep)*1.05]
    self.orig_xrange = xrange
    self.orig_yrange = yrange
    self.zoomfactor = zoomfactor
    *self.xlog = Keyword_Set(xlog)
    *self.ylog = Keyword_Set(ylog)
    
    ; Must do exact axis scaling for smooth operation.
    IF N_Elements(*self.xstyle) NE 0 THEN *self.xstyle = *self.xstyle && 1 ELSE *self.xstyle = 1
    IF N_Elements(*self.ystyle) NE 0 THEN *self.ystyle = *self.ystyle && 1 ELSE *self.ystyle = 1
    
    ; Realize the widget and get it going, if you created the TLB.
    IF N_Elements(parent) EQ 0 THEN BEGIN
        Widget_Control, self.tlb, /Realize
        XManager, 'cgzplot', self.tlb, /No_Block, Event_Handler='cgZPlot_Events', $
           Cleanup='cgZPlot_Cleanup'
    ENDIF
    
    RETURN, 1
END

;+
; The clean-up method for the object. When the object is destroyed, 
; this method will free the object's pointers.
;-
PRO cgZPlot::CLEANUP

    Compile_Opt idl2
    
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
   
    
    Ptr_Free, self.indep
    Ptr_Free, self.dep
    Ptr_Free, self.aspect
    Ptr_Free, self.max_value
    Ptr_Free, self.min_value
    Ptr_Free, self.xlog
    Ptr_Free, self.ylog
    Ptr_Free, self.ynozero
    Obj_Destroy, self.undoList
    Obj_Destroy, self.redoList
    WDelete, self.pixmapID
    
    ; Get rid of the overplot objects, if any.
    IF Ptr_Valid(self.oplots) THEN BEGIN
      FOR j=0,N_Elements(*self.oplots)-1 DO BEGIN
         Obj_Destroy, (*self.oplots)[j]
      ENDFOR
      Ptr_Free, self.oplots
    ENDIF

    ; Get rid of the legend objects, if any.
    IF Ptr_Valid(self.legends) THEN BEGIN
      FOR j=0,N_Elements(*self.legends)-1 DO BEGIN
         Obj_Destroy, (*self.legends)[j]
      ENDFOR
      Ptr_Free, self.legends
    ENDIF
    
    ; Call the superclass CLEANUP method.
    self -> cgGraphicsKeywords::CLEANUP
    
    ; If you have a valid TLB, destroy that, too.
    IF Widget_Info(self.tlb, /VALID_ID) THEN Widget_Control, self.tlb, /Destroy

END


;+
; This method adds a cgLegendItem object or array of objects to the plot. The
; legend objects are drawn on the plot after the orginal plot is drawn.
;
; :Params:
;    legendobject: in, optional, type=object
;        A cgLegendItem object, or an array of cgLegendItem objects that should be drawn
;        when the Draw method is called. The legend objects will be destroyed when this
;        object is destroyed.
;        
; :Keywords:
;     clear: in, optional, type=boolean, default=0
;        If this keyword is set, the overplot list is cleared before the new overplot objects
;        are added. Otherwise, the overplot object or objects is added to the end of the list
;        already present.
;     draw: in, optional, type=boolean, default=0
;        If this keyword is set, the object calls its Draw method after the overplot objects are added.
;     index: in, optional, type=integer, default=0
;         Used only if the `REPLACE` keyword is set. Specifies the replacement index.
;     replace: in, optional, type=boolean, default=0
;        If this keyword is set, the new object replaces a current object, in the object
;        array at the `INDEX` location.
;-
PRO cgZPlot::AddLegends, legendObject, $
   CLEAR=clear, $
   DRAW=draw, $
   INDEX=index, $
   REPLACE=replace

    Compile_Opt idl2
    
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
  
   ; Clear all current legends, if needed.
   IF Keyword_Set(clear) THEN BEGIN
   
        ; Get rid of the overplot objects, if any.
        IF Ptr_Valid(self.legends) THEN BEGIN
           FOR j=0,N_Elements(*self.legends)-1 DO BEGIN
             Obj_Destroy, (*self.legends)[j]
           ENDFOR
           Ptr_Free, self.legends
         ENDIF

   ENDIF
   
   ; Are we replacing an object in the current list?
   ; If so, do it and return.
   IF Keyword_Set(replace) THEN BEGIN
   
       IF N_Elements(index) EQ 0 THEN index = 0
       Obj_Destroy, (*self.legends)[index]
       (*self.legends)[index] = legendObject
        RETURN
        
   ENDIF
   
   ; If nothing to add, return.
   IF N_Elements(legendObject) EQ 0 THEN BEGIN
       IF Ptr_Valid(self.legends) EQ 0 THEN self.legends = Ptr_New(/ALLOCATE_HEAP)
       RETURN
   ENDIF
   
   ; Otherwise, add the legend objects.
   IF Ptr_Valid(self.legends) THEN BEGIN
       *self.legends = [*self.legends, legendObject]
   ENDIF ELSE BEGIN
       self.legends = Ptr_New(legendObject)
   ENDELSE
   help, *self.legends
   ; Draw the object?
   IF Keyword_Set(draw) THEN self -> Draw
   
END

;+
; This method adds a cgOverplot object or array of objects to the plot. The
; overplot objects are drawn on the plot after the orginal plot is drawn.
;
; :Params:
;    oplotobject: in, optional, type=object
;        A cgOverPlot object, or an array of cgOverplot objects that should be overplot
;        when the Draw method is called. The overplot objects will be destroyed when this
;        object is destroyed.
;        
; :Keywords:
;     clear: in, optional, type=boolean, default=0
;        If this keyword is set, the overplot list is cleared before the new overplot objects
;        are added. Otherwise, the overplot object or objects is added to the end of the list
;        already present.
;     draw: in, optional, type=boolean, default=0
;        If this keyword is set, the object calls its Draw method after the overplot objects are added.
;     index: in, optional, type=integer, default=0
;         Used only if the `REPLACE` keyword is set. Specifies the replacement index.
;     replace: in, optional, type=boolean, default=0
;        If this keyword is set, the new object replaces a current object, in the object
;        array at the `INDEX` location.
;-
PRO cgZPlot::AddOverplots, oplotObject, $
   CLEAR=clear, $
   DRAW=draw, $
   INDEX=index, $
   REPLACE=replace

    Compile_Opt idl2
    
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
  
   ; Are we replacing an object in the current list?
   ; If so, do it and return.
   IF Keyword_Set(replace) THEN BEGIN
   
       IF N_Elements(index) EQ 0 THEN index = 0
       Obj_Destroy, (*self.oplots)[index]
       (*self.oplots)[index] = oplotObject
        RETURN
        
   ENDIF
   
   ; Clear all current overplots, if needed.
   IF Keyword_Set(clear) THEN BEGIN
   
        ; Get rid of the overplot objects, if any.
        IF Ptr_Valid(self.oplots) THEN BEGIN
           FOR j=0,N_Elements(*self.oplots)-1 DO BEGIN
             Obj_Destroy, (*self.oplots)[j]
           ENDFOR
           Ptr_Free, self.oplots
         ENDIF

   ENDIF
   
   ; If nothing to add, return.
   IF N_Elements(oplotObject) EQ 0 THEN BEGIN
       IF Ptr_Valid(self.oplots) EQ 0 THEN self.oplots = Ptr_New(/ALLOCATE_HEAP)
       RETURN
   ENDIF
   
   ; Otherwise, add the overplot objects.
   IF Ptr_Valid(self.oplots) THEN BEGIN
       *self.oplots = [*self.oplots, oplotObject]
   ENDIF ELSE BEGIN
       self.oplots = Ptr_New(oplotObject)
   ENDELSE
   
   ; Draw the object?
   IF Keyword_Set(draw) THEN self -> Draw
   
END



;+
; This event handler will adjust the data Y range of the line plot to include all
; of the data in the current data X range, even if that data is currently not being
; displayed. 
; 
; :Params:
; 
;    event: in, optional, type=structure
;        The event structure passed by the window manager. Not used in this event handler.
;-
PRO cgZPlot::AdjustRange, event

    Compile_Opt idl2
    
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
      
   ; Get the current X range of the data display.
   xrange = *self.xrange
   
   ; Locate the end points of that data range in the original data vector.
   endpoints = 0 > Value_Locate(*self.indep, xrange) < (N_Elements(*self.indep)-1)
   
   ; Find the corresponding points in the dependent data vector, and calculate the
   ; Y range from those.
   points = (*self.dep)[endpoints[0]:endpoints[1]]
   minrange = Min(points, Max=maxrange)
   fudge = Abs(maxrange-minrange) * 0.05
   *self.yrange = [minrange-fudge, maxrange+fudge]
   
   ; Redraw the plot.
   self -> Draw
END


;+
; Button down events are processed in this event handler method. Depending
; on which button is pressed and where the button is pressed in the graphics
; window, this method will either handle or dispatch the event to the appropriate 
; event handler.
; 
; :Params:
; 
;    event: in, required, type=structure
;        The event structure passed by the window manager.
;-
PRO cgZPlot::Button_Events, event

    Compile_Opt idl2
    
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Only interested in button down events.
   IF event.type NE 0 THEN RETURN
   
   ; Left or right button determines the "mode" of the draw widget.
   ; Left button (1) is zoom, right button (4) is pan. Everything else
   ; is ignored.
   CASE event.press OF
      1: mode = 0
      4: mode = 1
      ELSE: RETURN
   ENDCASE
   
   ; Where the click is determines what we do next. If the click is below bottom the X axis,
   ; we will zoom the X axis out. If the click is above the top X axis, we will zoom the X axis 
   ; in. If the click is to the left of the Y axis we will zoom the Y axis out, if the click is
   ; to the right of the Y axis we will zoom the Y axis in.
   IF ~self -> InsidePlot(event.x, event.y) THEN BEGIN
      
      ; Need to do exact axis scaling to do smooth panning and zooming.
      IF N_Elements(*self.xstyle) NE 0 THEN *self.xstyle = *self.xstyle && 1 ELSE *self.xstyle = 1
      IF N_Elements(*self.ystyle) NE 0 THEN *self.ystyle = *self.ystyle && 1 ELSE *self.ystyle = 1
      
      ; Convert the click to normalized coordinates.
      !X = self.bangX
      !Y = self.bangY
      !P = self.bangP
      WSet, self.wid
      xy = Convert_Coord(event.x, event.y, /Device, /To_Normal)
      xn = xy[0]
      yn = xy[1]
      zf = self.zoomfactor
      
      ; Click is below bottom X axis.
      IF (xn GT !X.Window[0]) && (xn LT !X.Window[1]) && (yn LT !Y.Window[0]) THEN BEGIN
          distance = Abs((*self.xrange)[1] - (*self.xrange)[0]) * zf
          (*self.xrange)[0] = ((*self.xrange)[0] - distance) > self.orig_xrange[0]
          (*self.xrange)[1] = ((*self.xrange)[1] + distance) < self.orig_xrange[1]
      ENDIF
      
      ; Click is above the top X axis.
      IF (xn GT !X.Window[0]) && (xn LT !X.Window[1]) && (yn GT !Y.Window[1]) THEN BEGIN
          distance = Abs((*self.xrange)[1] - (*self.xrange)[0]) * zf
          (*self.xrange)[0] = ((*self.xrange)[0] + distance) > self.orig_xrange[0]
          (*self.xrange)[1] = ((*self.xrange)[1] - distance) < self.orig_xrange[1]
          IF (*self.xrange)[0] GE (*self.xrange)[1] THEN (*self.xrange)[0] = (*self.xrange)[1] - distance
      ENDIF

      ; Click is to left of left Y axis.
      IF (yn GT !Y.Window[0]) && (yn LT !Y.Window[1]) && (xn LT !X.Window[0]) THEN BEGIN
          distance = Abs((*self.yrange)[1] - (*self.yrange)[0]) * zf
          (*self.yrange)[0] = ((*self.yrange)[0] - distance) > self.orig_yrange[0]
          (*self.yrange)[1] = ((*self.yrange)[1] + distance) < self.orig_yrange[1]
      ENDIF
      
      ; Click is to right of right Y axis.
      IF (yn GT !Y.Window[0]) && (yn LT !Y.Window[1]) && (xn GT !X.Window[1]) THEN BEGIN
          distance = Abs((*self.yrange)[1] - (*self.yrange)[0]) * zf
          (*self.yrange)[0] = ((*self.yrange)[0] + distance) > self.orig_yrange[0]
          (*self.yrange)[1] = ((*self.yrange)[1] - distance) < self.orig_yrange[1]
          IF (*self.yrange)[0] GE (*self.yrange)[1] THEN (*self.yrange)[0] = (*self.yrange)[1] - distance
      ENDIF
      
      self -> Draw
      RETURN
      
   ENDIF
   
   ; Store the current click location.
   self.x0 = event.x
   self.y0 = event.y
   
   ; Send the event to the proper event handler, depending on the mode.
   Widget_Control, self.drawID, /Clear_Events
   CASE mode OF
       0: BEGIN ; Zooming
          Widget_Control, self.drawID, Set_UValue={method:'Zoom_Events', object:self}
          END
       1: BEGIN ; Panning
          Widget_Control, self.drawID, Set_UValue={method:'Pan_Events', object:self}
          self. drag = 1
          END
   ENDCASE
   
   ; Turn motion events on for this widget.
   Widget_Control, self.drawID, DRAW_MOTION_EVENTS=1
     
END

;+
; This method copies the contents of the pixmap into the display window.
;-
PRO cgZPlot::CopyPixmap

    Compile_Opt idl2
    
    WSet, self.wid
    Device, Copy=[0, 0, self.xsize, self.ysize, 0, 0, self.pixmapID]

END   

;+
; This method destroys the object and the GUI, if it still exists.
;-
PRO cgZplot::Destroy
   Obj_Destroy, self
END

;+
; This is the standard drawing method for the object. For smooth operation,
; the graphics are pixmap buffered. The plot is drawn into the pixmap, then
; copied to the draw widget window.
;-
PRO cgZPlot::Draw

    Compile_Opt idl2
    
    ; Standard error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
    ENDIF
   
    WSet, self.pixmapID
    cgErase
    
   
    ; Draw the plot itself.
    self -> DrawPlot
           
    ; Save the plot system variables.
    self.bangX = !X
    self.bangY = !Y
    self.bangP = !P
   
    ; Make sure we are drawing into the right window.
    WSet, self.wid
    Device, Copy=[0, 0, self.xsize, self.ysize, 0, 0, self.pixmapID]
    
    ; We need to save the current position of the plot in the window,
    ; so we can determine if clicks are inside or outside this position.
    self.current_position = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
    
    ; Save the current configuration on the undoList.
    IF self.drag EQ 0 THEN self -> UndoList
    
END

;+
; This method simply gets the keywords it needs and draws the line plot.
; It was created primarily so the OUTPUT keyword could be used with the
; cgPlot command, since all the file output infrastruction has been built
; into that command.
; 
; :Keywords:
;    
;     output: in, optional, type=string
;         The name of the output file. File type is determined by the file extension.
;-
PRO cgZPlot::DrawPlot, OUTPUT=output

    Compile_Opt idl2
    
    ; Standard error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
    ENDIF

   ; Get the graphics keywords.
   self -> cgGraphicsKeywords::GetProperty, $
        AXISCOLOR=axiscolor, $
        BACKGROUND=background, $
        CHARSIZE=charsize, $
        CHARTHICK=charthick, $
        CLIP=clip, $
        COLOR=color, $
        DATA=data, $
        DEVICE=device, $
        NORMAL=normal, $
        FONT=font, $
        NOCLIP=noclip, $
        NODATA=nodata, $
        NOERASE=noerase, $
        POSITION=position, $
        PSYM=psym, $
        SUBTITLE=subtitle, $
        SYMSIZE=symsize, $
        T3D=t3d, $
        THICK=thick, $
        TICKLEN=ticklen, $
        TITLE=title, $
        
        XCHARSIZE=xcharsize, $
        XGRIDSTYLE=xgridstyle, $
        XMARGIN=xmargin, $
        XMINOR=xminor, $
        XRANGE=xrange, $
        XSTYLE=xstyle, $
        XTHICK=xthick, $
        XTICK_GET=xtick_get, $
        XTICKFORMAT=xtickformat, $
        XTICKINTERVAL=xtickinterval, $
        XTICKLAYOUT=xticklayout, $
        XTICKLEN=xticklen, $
        XTICKNAME=xtickname, $
        XTICKS=xticks, $
        XTICKUNITS=xtickunits, $
        XTICKV=xtickv, $
        XTITLE=xtitle, $
        
        YCHARSIZE=ycharsize, $
        YGRIDSTYLE=ygridstyle, $
        YMARGIN=ymargin, $
        YMINOR=yminor, $
        YRANGE=yrange, $
        YSTYLE=ystyle, $
        YTHICK=ythick, $
        YTICK_GET=ytick_get, $
        YTICKFORMAT=ytickformat, $
        YTICKINTERVAL=ytickinterval, $
        YTICKLAYOUT=yticklayout, $
        YTICKLEN=yticklen, $
        YTICKNAME=ytickname, $
        YTICKS=yticks, $
        YTICKUNITS=ytickunits, $
        YTICKV=ytickv, $
        YTITLE=ytitle, $
       
        ZCHARSIZE=zcharsize, $
        ZGRIDSTYLE=zgridstyle, $
        ZMARGIN=zmargin, $
        ZMINOR=zminor, $
        ZRANGE=zrange, $
        ZSTYLE=zstyle, $
        ZTHICK=zthick, $
        ZTICK_GET=ztick_get, $
        ZTICKFORMAT=ztickformat, $
        ZTICKINTERVAL=ztickinterval, $
        ZTICKLAYOUT=zticklayout, $
        ZTICKLEN=zticklen, $
        ZTICKNAME=ztickname, $
        ZTICKS=zticks, $
        ZTICKUNITS=ztickunits, $
        ZTICKV=ztickv, $
        ZTITLE=ztitle, $
        ZVALUE=zvalue
        
   ; Draw the plot.
   cgPlot, *self.indep, *self.dep, $
        OUTPUT=output, $
        ASPECT=*self.aspect, $
        LABEL=self.label, $
        MAX_VALUE=*self.max_value, $
        MIN_VALUE=*self.min_value, $
        XLOG=*self.xlog, $
        YLOG=*self.ylog, $
        YNOZERO=*self.ynozero, $
        AXISCOLOR=axiscolor, $
        BACKGROUND=background, $
        CHARSIZE=charsize, $
        CHARTHICK=charthick, $
        CLIP=clip, $
        COLOR=color, $
        DATA=data, $
        DEVICE=device, $
        NORMAL=normal, $
        FONT=font, $
        LEGENDS=*self.legends, $
        NOCLIP=noclip, $
        NODATA=nodata, $
        NOERASE=noerase, $
        OPLOTS=*self.oplots, $
        POSITION=position, $
        PSYM=psym, $
        SUBTITLE=subtitle, $
        SYMSIZE=symsize, $
        T3D=t3d, $
        THICK=thick, $
        TICKLEN=ticklen, $
        TITLE=title, $
        XCHARSIZE=xcharsize, $
        XGRIDSTYLE=xgridstyle, $
        XMARGIN=xmargin, $
        XMINOR=xminor, $
        XRANGE=xrange, $
        XSTYLE=xstyle, $
        XTHICK=xthick, $
        XTICK_GET=xtick_get, $
        XTICKFORMAT=xtickformat, $
        XTICKINTERVAL=xtickinterval, $
        XTICKLAYOUT=xticklayout, $
        XTICKLEN=xticklen, $
        XTICKNAME=xtickname, $
        XTICKS=xticks, $
        XTICKUNITS=xtickunits, $
        XTICKV=xtickv, $
        XTITLE=xtitle, $
        YCHARSIZE=ycharsize, $
        YGRIDSTYLE=ygridstyle, $
        YMARGIN=ymargin, $
        YMINOR=yminor, $
        YRANGE=yrange, $
        YSTYLE=ystyle, $
        YTHICK=ythick, $
        YTICK_GET=ytick_get, $
        YTICKFORMAT=ytickformat, $
        YTICKINTERVAL=ytickinterval, $
        YTICKLAYOUT=yticklayout, $
        YTICKLEN=yticklen, $
        YTICKNAME=ytickname, $
        YTICKS=yticks, $
        YTICKUNITS=ytickunits, $
        YTICKV=ytickv, $
        YTITLE=ytitle, $
        ZCHARSIZE=zcharsize, $
        ZGRIDSTYLE=zgridstyle, $
        ZMARGIN=zmargin, $
        ZMINOR=zminor, $
        ZRANGE=zrange, $
        ZSTYLE=zstyle, $
        ZTHICK=zthick, $
        ZTICK_GET=ztick_get, $
        ZTICKFORMAT=ztickformat, $
        ZTICKINTERVAL=ztickinterval, $
        ZTICKLAYOUT=zticklayout, $
        ZTICKLEN=zticklen, $
        ZTICKNAME=ztickname, $
        ZTICKS=zticks, $
        ZTICKUNITS=ztickunits, $
        ZTICKV=ztickv, $
        ZTITLE=ztitle, $
        ZVALUE=zvalue
        
END


;+
; This method simply erases the display.
;-
PRO cgZPlot::Erase

    Compile_Opt idl2
    
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
 
    WSet, self.pixmapID
    cgErase
    self -> CopyPixmap
    
END



;+
; This event handler method allows the plot in the graphics window to be output
; to a file.
; 
; :Params:
; 
;    event: in, required, type=structure
;        The event structure passed by the window manager.
;-
PRO cgZPlot::FileOutput, event

    Compile_Opt idl2
    
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
   
   ; What kind of output does the user want?
   Widget_Control, event.id, Get_Value=fileType
   CASE StrUpCase(fileType) OF
      'BMP FILE':  filename = 'cgzplot.bmp'
      'GIF FILE':  filename = 'cgzplot.gif'
      'EPS FILE':  filename = 'cgzplot.eps'
      'JPEG FILE': filename = 'cgzplot.jpg'
      'PDF FILE':  filename = 'cgzplot.pdf'
      'PNG FILE':  filename = 'cgzplot.png'
      'PS FILE':   filename = 'cgzplot.ps'
      'TIFF FILE': filename = 'cgzplot.tif'
   ENDCASE
   
   IF self.saveDir NE "" THEN thisDir = self.saveDir ELSE CD, CURRENT=thisDir
   
   filename = cgPickfile(PATH=thisDir, File=filename, GET_PATH=saveDir, $
       Title='Save Plot As...', /Write)
   IF filename EQ "" THEN RETURN
   
   self.saveDir = saveDir
   self -> DrawPlot, OUTPUT=filename
   
   
END

;+
; The properties of the object (keywords) are retrieved with this method.
; 
; :Keywords:
;     aspect: out, optional, type=float
;        A value that represents the aspect ratio (ysize/xsize) of the resulting plot. 
;     label: out, optional, type=string
;         The label that is used for the zoom plot.
;     legends: out, optional, type=object
;         The current legend objects, if there are any. If not, a null object.
;     max_value: out, optional, type=float
;         The maximum value to plot. 
;     min_value: out, optional, type=float
;         The minimum value to plot. 
;     oplots: out, optional, type=object
;         The current overplot objects, if there are any. If not, a null object.
;     undolist: out, optional, type=objref
;         The LinkedList object that maintains the undo list.
;     xlog: out, optional, type=boolean
;         Set if a logarithmic X axis is used in the plot.
;     ylog: out, optional, type=boolean
;         Set if a logarithmic Y axis is used in the plot.
;     ynozero: out, optional, type=boolean
;         Set if this property of the plot is set.
;     zoomfactor: out, optional, type=float
;         Set to the current zoom factor.
;     _extra: out, optional, type=any
;        Any keyword appropriate for the IDL Plot or Coyote Graphic cgPlot command is 
;        allowed in the program.
;-
PRO cgZPlot::GetProperty, $
        DATA_X=indep, $
        DATA_Y=dep, $
        ASPECT=aspect, $
        LABEL=label, $
        LEGENDS=legends, $
        MAX_VALUE=max_value, $
        MIN_VALUE=min_value, $
        OPLOTS=oplots, $
        UNDOLIST=undolist, $
        XLOG=xlog, $
        YLOG=ylog, $
        YNOZERO=ynozero, $
        ZOOMFACTOR=zoomfactor, $
        _EXTRA=extra
        
    Compile_Opt idl2
    
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
   
   
    IF Arg_Present(indep) NE 0 THEN IF N_Elements(*self.indep) NE 0 THEN indep = *self.indep
    IF Arg_Present(dep) NE 0 THEN IF N_Elements(*self.dep) NE 0 THEN dep = *self.dep
    IF Arg_Present(aspect) NE 0 THEN IF N_Elements(*self.aspect) NE 0 THEN aspect = *self.aspect
    IF Arg_Present(label) NE 0 THEN IF N_Elements(self.label) NE 0 THEN label = self.label
    IF Arg_Present(legends) NE 0 THEN IF Ptr_Valid(self.legends) $
        THEN legends = *self.legends $
        ELSE legends = Obj_New()
    IF Arg_Present(max_value) NE 0 THEN IF N_Elements(*self.max_value) NE 0 THEN max_value = *self.max_value
    IF Arg_Present(min_value) NE 0 THEN IF N_Elements(*self.min_value) NE 0 THEN min_value = *self.min_value
    IF Arg_Present(oplots) NE 0 THEN IF Ptr_Valid(self.oplots) $
        THEN oplots = *self.oplots $
        ELSE oplots = Obj_New()
    IF Arg_Present(xlog) NE 0 THEN IF N_Elements(*self.xlog) NE 0 THEN xlog = *self.xlog
    IF Arg_Present(ylog) NE 0 THEN IF N_Elements(*self.ylog) NE 0 THEN ylog = *self.ylog
    IF Arg_Present(ynozero) NE 0 THEN IF N_Elements(*self.ynozero) NE 0 THEN ynozero = *self.ynozero
    IF Arg_Present(zoomfactor) NE 0 THEN zoomfactor = self.zoomfactor
    
    ; Get superclass properties.
    IF N_Elements(extra) NE 0 THEN self -> cgGraphicsKeywords::GetProperty, _REF_STRICT_EXTRA=extra
END

;+
; This  method  simply determines if a button click is inside (returns 1)
; or outside (returns 0) the plot boundaries, determined by the plot axes.
; 
; :Params:
;    x: in, required, type=int
;        The X location of the button click in device coordinates.
;    y: in, required, type=int
;        The Y location of the button click in device coordinates.
;-
FUNCTION cgZPlot::InsidePlot, x, y

    Compile_Opt idl2
    
    On_Error, 2
    
    ; Convert device coordinate location to normalized coordinate location.
    ; Make sure you have the right system variables and window open.
    bangx = !X
    bangy = !Y
    bangp = !P
    !X = self.bangx
    !Y = self.bangy
    !P = self.bangp
    WSet, self.pixmapID
    
    ; Do the conversion.
    xy = Convert_Coord(x, y, /Device, /To_Normal)
    xpt = xy[0]
    ypt = xy[1]
    
    ; Restore the system variables.
    !X = bangx
    !Y = bangy
    !P = bangp
    
    ; Is this within the plot boundaries?
    p = self.current_position
    IF (xpt LT p[0]) || (xpt gt p[2]) || (ypt LT p[1]) || (ypt GT p[3]) THEN RETURN, 0 ELSE RETURN, 1
    
END


;+
; The purpose of this method is to draw the initial line plot in the draw widget.
; 
;-
PRO cgZPlot::Notify_Realize

   Compile_Opt idl2
   
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF

    Widget_Control, self.drawID, Get_Value=wid
    self.wid = wid
    
     ; Draw the initial plot.
    self -> Draw
    
END


;+
; This event handler method responds to panning events until it gets a button UP event.
; 
; :Params:
; 
;    event: in, required, type=structure
;        The event structure passed by the window manager.
;-
PRO cgZPlot::Pan_Events, event


   Compile_Opt idl2
   
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
   
    ; Is this a button UP event? If it is, then we are done here.
    IF event.type EQ 1 THEN BEGIN
    
      ; Turn motion events off for this widget and clear all subsequent events.
      ; Restore the original event handler.
      Widget_Control, self.drawID, Draw_Motion_Events=0
      Widget_Control, self.drawID, /Clear_Events
      Widget_Control, self.drawID, Set_UValue={method:'Button_Events', object:self}
      self.drag = 0
    ENDIF
    
    ; Determine the end points of the pan and draw the plot. We are panning only in
    ; the X direction. The Y axes will reflect ALL the data points in the X range.
    ; Deterime the distance traveled in the XRange since you were last here.
    xpts = [self.x0, event.x]
    ypts = [self.y0, event.y]
    
    ; Locate these points in the data coordinate space.
    !X = self.bangX
    !Y = self.bangY
    !P = self.bangP
    WSet, self.wid
    xd = Convert_Coord(xpts, ypts, /Device, /To_Data)
    xdistance = (xd[0,0] - xd[0,1])
    ydistance = (xd[1,0] - xd[1,1])
    IF *self.xlog THEN BEGIN
        *self.xrange = (*self.xrange + xdistance) > 1e-6
    ENDIF ELSE BEGIN
        *self.xrange = (*self.xrange + xdistance)
    ENDELSE
    IF *self.ylog THEN BEGIN
        *self.yrange = (*self.yrange + ydistance) > 1e-6
    ENDIF ELSE BEGIN
        *self.yrange = (*self.yrange + ydistance)
    ENDELSE
    
    ; Update the static pan location.
    self.x0 = event.x
    self.y0 = event.y
    
    ; Draw the plot.
    self -> Draw
    
END


;+
; This method resizes the draw widget.
; 
; :Params:
; 
;    xsize: in, required, type=integer
;        The requested X size of the draw widget.
;    ysize: in, required, type=integer
;        The requested Y size of the draw widget.
;        
; :Keywords:
; 
;     draw: in, optional, type=boolean, default=0
;        If this keyword is set, the DRAW method is called after the widget is resized.
;-
PRO cgZPlot::ResizeDrawWidget, xsize, ysize, DRAW=draw

   Compile_Opt idl2
   
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Delete and recreate the pixmap at the right size. 
   WDelete, self.pixmapID
   Window, /Pixmap, /Free, XSize=xsize, YSize=ysize
   self.pixmapID = !D.Window
   
   ; Make the draw widget the right size.
   Widget_Control, self.drawID, Draw_XSize=xsize, Draw_YSize=ysize
   self.xsize = xsize
   self.ysize = ysize

    ; Draw the plot?
    IF Keyword_Set(draw) THEN self -> Draw
    
END


;+
; This method allow plot keywords to be set to appropriate values.
; 
; :Keywords:
;
;     aspect: in, optional, type=float, default=none
;        Set this keyword to a floating point ratio that represents the aspect ratio 
;        (ysize/xsize) of the resulting plot. The plot position may change as a result
;        of setting this keyword. Note that `Aspect` cannot be used when plotting with
;        !P.MULTI.
;     dep: in, optional, type=any
;         The dependent data to plot.
;     draw: in, optional, type=boolean, default=0
;         Set this keyword if you would like to immediately draw the plot after properties are set.
;     indep: in, optional, type=any
;         The independent data to plot.
;     label: in, optional, type=string
;         A label is similar to a plot title, but it is aligned to the left edge
;         of the plot and is written in hardware fonts. Use of the label keyword
;         will suppress the plot title.
;     legends: in, optional, type=object
;         A single cgLegendItem object, or an array of cgLegendItem objects that will be
;         drawn on the plot as a legend.
;     max_value: in, optional, type=float
;        Set this keyword to the maximum value to plot. Any values greater than this 
;        value are treated as missing.
;     min_value: in, optional, type=float
;        Set this keyword to the minimu value to plot. Any values smaller than this 
;        value are treated as missing.
;     oplots: in, optional, type=object
;         A single cgOverPlot object, or an array of cgOverPlot objects that will be
;         overplot on the axes set up by the original data.
;     xlog: in, optional, type=boolean, default=0
;         Set this keyword to use a logarithmic X axis
;     ylog: in, optional, type=boolean, default=0
;         Set this keyword to use a logarithmic Y axis
;     ynozero: in, optional, type=boolean, default=0
;         Set this keyword to use allow the Y axis to start at a value other than zero.
;     zoomfactor: in, optional, type=float
;         Set this keyword to a number between 0.01 and 0.25. This affects the amount
;         of zooming when the X axis and Y axis are zoomed with the LEFT mouse button.
;         The default value is 0.05 or five percent of the current axis range on each
;         end of the axis, resulting in a 10 percent change in the axis length.
;     _extra: in, optional, type=any
;        Any keyword appropriate for the IDL Plot or Coyote Graphic cgPlot command is 
;        allowed in the program.
;-
PRO cgZPlot::SetProperty, $
        ASPECT=aspect, $
        DEP=dep, $
        DRAW=draw, $
        INDEP=indep, $
        LABEL=label, $
        LEGENDS=legends, $
        MAX_VALUE=max_value, $
        MIN_VALUE=min_value, $
        OPLOTS=oplots, $
        XLOG=xlog, $
        YLOG=ylog, $
        YNOZERO=ynozero, $
        ZOOMFACTOR=zoomfactor, $
        _EXTRA=extra
        
   Compile_Opt idl2
   
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
   
    
    IF N_Elements(indep) NE 0 THEN *self.indep = indep
    IF N_Elements(dep) NE 0 THEN *self.dep = dep
    IF N_Elements(aspect) NE 0 THEN *self.aspect = aspect
    IF N_Elements(label) NE 0 THEN self.label = label
    IF N_Elements(legends) NE 0 THEN self -> AddLegends, legends, /Clear
    IF N_Elements(max_value) NE 0 THEN *self.max_value = max_value
    IF N_Elements(min_value) NE 0 THEN *self.min_value = min_value
    IF N_Elements(oplots) NE 0 THEN self -> AddOverplots, oplots, /Clear
    IF N_Elements(xlog) NE 0 THEN *self.xlog = Keyword_Set(xlog)
    IF N_Elements(ylog) NE 0 THEN *self.ylog = Keyword_Set(ylog)
    IF N_Elements(ynozero) NE 0 THEN *self.ynozero = Keyword_Set(ynozero)
    IF N_Elements(zoomfactor) NE 0 THEN self.zoomfactor = zoomfactor
    
    ; Superclass keywords.
    IF N_Elements(extra) NE 0 THEN self -> cgGraphicsKeywords::SetProperty, _STRICT_EXTRA=extra
    
    ; Need to draw the plot?
    IF Keyword_Set(draw) THEN self -> Draw
        
END

;+
; This event handler method destroys the widget program.
; 
; :Params:
;    event: in, required, type=structure
;        The event structure passed by the window manager.
;-
PRO cgZPlot::Quit, event

   Widget_Control, event.top, /Destroy

END


;+
; This method performs the REDO action and restores the plot to
; it's previous condition.
; 
; :Params:
;    event: in, optional, type=structure
;        The event structure passed by the window manager. Not used in this method.
;-
PRO cgZPlot::Redo, event

   Compile_Opt idl2
   
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Get the list count. Return if there is nothing in the list
   listCnt = self.redoList -> Get_Count()
   IF listCnt EQ 0 THEN BEGIN
      void = Dialog_Message('Nothing to REDO')
      RETURN
   ENDIF
   
   ; Retrieve the last item on this list.
   item = self.redoList -> Get_Item()
   
   ; Remove the last item from the list.
   IF listCnt GT 1 THEN self.redoList -> Delete

   ; Update the range variables and draw the plot.
   IF listCnt GE 1 THEN BEGIN
     *self.xrange = item.xrange
     *self.yrange = item.yrange
     self -> Draw
   ENDIF
   
END


;
;+
; This event handler method resizes the graphics window.
; 
; :Params:
;    event: in, required, type=structure
;        The event structure passed by the window manager.
;-
PRO cgZPlot::TLB_Resize_Events, event

   Compile_Opt idl2
   
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
   
   WDelete, self.pixmapID
   Window, /Pixmap, /Free, XSize=event.x, YSize=event.y
   self.pixmapID = !D.Window
   Widget_Control, self.drawID, Draw_XSize=event.x, Draw_YSize=event.y
   self.xsize = event.x
   self.ysize = event.y
   self -> Draw
   
END


;+
; This method performs the UNDO action and restores the plot to
; it's previous condition.
; 
; :Params:
;    event: in, optional, type=structure
;        The event structure passed by the window manager. Not used in this method.
;-
PRO cgZPlot::Undo, event

   Compile_Opt idl2
   
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Get the list count. Return if there is nothing in the list
   listCnt = self.undoList -> Get_Count()
   IF listCnt EQ 0 THEN RETURN
   
   ; Retrieve the last item from the list, and add it to the REDO list.
   item = self.undoList -> Get_Item()
   self.redoList -> Add, item
   
   ; Remove the last item from the list.
   IF listCnt GT 1 THEN self.undoList -> Delete
   
   ; Get the last item on the list.
   item = self.undoList -> Get_Item()
   
   ; Now remove this item from the list, too, if it is not the last one
   IF listCnt GT 1 THEN self.undoList -> Delete
   
   ; Set the ranges and redraw the plot.
   IF listCnt GE 1 THEN BEGIN
     *self.xrange = item.xrange
     *self.yrange = item.yrange
     self -> Draw
   ENDIF
   
END


;+
; This method maintains the UNDO list. The list has a maximum undo capacity of 50.
;-
PRO cgZPlot::UndoList

   Compile_Opt idl2
   
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Prepare the structure that will be stored on the list.
   IF N_Elements(*self.xrange) EQ 0 THEN *self.xrange = self.orig_xrange
   IF N_Elements(*self.yrange) EQ 0 THEN *self.yrange = self.orig_yrange
   undoStruct = {xrange:*self.xrange, yrange:*self.yrange}
   
   ; How many items are on the list already? If 50, delete the first
   ; item on the list.
   listCnt = self.undoList -> Get_Count()
   IF listCnt GE 50 THEN self.undoList -> Delete, 0
   
   ; Add the item to the list.
   self.undoList -> Add, undoStruct

END


;+
; This event handler method allows the user to create a rubber-band box for zooming
; into the line plot.
; 
; :Params:
;    event: in, required, type=structure
;        The event structure passed by the window manager.
;-
PRO cgZPlot::Zoom_Events, event

    ; Is this a button UP event? If it is, all the action is done here.
    ; If it is not, then it must be a MOTION event, and we simply draw
    ; and erase the zoom box.
    
   Compile_Opt idl2
   
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
   
    ; What kind of event is this?
    CASE event.type OF
        1: BEGIN ; button UP event
        
           ; Turn motion events off for this widget and clear all subsequent events.
           ; Restore the original event handler.
           Widget_Control, self.drawID, Draw_Motion_Events=0
           Widget_Control, self.drawID, /Clear_Events
           Widget_Control, self.drawID, Set_UValue={method:'Button_Events', object:self}
           self -> CopyPixmap
           
           xtest = [self.x0, event.x]
           ytest = [self.y0, event.y]
           x = [Min(xtest), Max(xtest)]
           y = [Min(ytest), Max(ytest)]
           
           ; Make sure the up event is inside the plot.
           !X = self.bangX
           !Y = self.bangY
           !P = self.bangP
           xy = Convert_Coord(x, y, /Device, /To_Normal)
           p = self.current_position
           xn = p[0] > xy[0,*] < p[2]
           yn = p[1] > xy[1,*] < p[3]
           
           ; Convert these normalized coordinates to data coordinates.
           xy = Convert_Coord(xn, yn, /Normal, /To_Data)
           xd = Reform(xy[0,*])
           yd = Reform(xy[1,*])
           
           ; The range depends on whether you are using log axes or not.
           IF *self.xlog THEN BEGIN
              x = 10^!X.CRange[0] > xd < 10^!X.CRange[1]
           ENDIF ELSE BEGIN
              x = !X.CRange[0] > xd < !X.CRange[1]
           ENDELSE
           IF *self.ylog THEN BEGIN
              y = 10^!Y.CRange[0] > yd < 10^!Y.CRange[1]
           ENDIF ELSE BEGIN
              y = !Y.CRange[0] > yd < !Y.CRange[1]
           ENDELSE
           *self.xrange = x
           *self.yrange = y
            
           ; Draw the plot.
           self -> Draw
           END
           
        2: BEGIN ; motion event
        
           ; Erase whatever is currently on the display.
           self -> CopyPixmap
           
           ; Draw the new box.
           x = [self.x0, self.x0, event.x, event.x, self.x0]
           y = [self.y0, event.y, event.y, self.y0, self.y0]
           PlotS, x, y, Color=cgColor('NAVY'), Thick=2, /Device
           END
        ELSE: 
    ENDCASE

END


;+
; This is the main event handler for the program. All events come here
; to be distributed to the appropriate event handler method according
; to instructions packed into the UVALUE of any widget generating an
; event.
; 
; :Params:
;    event: in, required, type=structure
;        The event structure passed by the window manager.
;-
PRO cgZPlot_Events, event

   Compile_Opt idl2
   
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
   
    Widget_Control, event.id, Get_UValue=instructions
    Call_Method, instructions.method, instructions.object, event

END

;+
; This is the cleanup routine for the widget. Its function is to destroy
; the underlying program object.
; 
; :Params:
;    tlb: in, required, type=int
;        The widget identifier of the top-level base widget that just died.
;-
PRO cgZPlot_Cleanup, tlb

   Widget_Control, tlb, Get_UValue=instr
   Obj_Destroy, instr.object
   
END

;+
; This is the realize notify routine for the widget. Its function call the
; Realize_Notify method to draw the initial plot in the display window.
; 
; :Params:
;    id: in, required, type=int
;        The widget identifier of the widget that has been realized.
;-
PRO cgZPlot_Notify_Realize, id

   Widget_Control, id, Get_UValue=instructions
   Call_Method, 'Notify_Realize', instructions.object
   
END
;+
; The object class definition.
; 
; :Params:
;    class: out, optional, type=struct
;        The class definition object. Often helpful for obtaining fields of the object structure.
;-
PRO cgZPlot__Define, class

   Compile_Opt idl2
   
   class = { cgZPLOT, $
             INHERITS cgGraphicsKeywords, $
             ASPECT: Ptr_New(), $
             MAX_VALUE: Ptr_New(), $
             MIN_VALUE: Ptr_New(), $
             NSUM: Ptr_New(), $
             XLOG: Ptr_New(), $
             YLOG: Ptr_New(), $
             YNOZERO: Ptr_New(), $
             
             orig_xrange: DblArr(2), $
             orig_yrange: DblArr(2), $
             current_position: DblArr(4), $
             zoomFactor: 0.0, $
             undoList: Obj_New(), $
             redoList: Obj_New(), $
             oplots: Ptr_New(), $      ; A pointer to a cgOverPlot object or array of cgOverPlot objects.
             legends: Ptr_New(), $     ; A pointer to a cgLegendItem object or array of cgLegendItem objects.
             label: "", $              ; The plot label. Suppress TITLE if present.
             savedir: "", $            ; The output directory where files are saved.
             drag: 0B, $               ; A flag that tells me if I am panning or not. Necessary for UNDO.
             
             indep: Ptr_New(), $
             dep: Ptr_New(), $
             
             tlb: 0L, $
             drawID: 0L, $
             pixmapID: 0L, $
             wid: 0L, $
             xsize: 0L, $
             ysize: 0L, $
             x0: 0L, $
             y0: 0L,$
             x1: 0L, $
             y1: 0L, $
             mode: 0B, $   0 is zoom plot, 1 is pan plot.
             bangx: !X, $
             bangy: !Y, $
             bangp: !P $
            }

END

