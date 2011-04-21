; docformat = 'rst'
;
; NAME:
;   FSC_ZImage
;
; PURPOSE:
;   Allows the user to interactively zoom into an image. Program controls are available
;   by right-clicking in the full-sized image window. Zoom factors from 2x to 16x are
;   available. Use the left mouse button to draw a box on the full-sized image to locate
;   the region of the image to zoom.
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
PRO FSC_ZImage_ZoomDied, zoomID

    ; Come here when the zoom window dies. Basically, you 
    ; want to erase the zoom box in the full-size window.
  
    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message(/Quiet)
         
        ; Put the info structure back.
        IF N_Elements(info) NE 0 THEN Widget_Control, tlb, Set_UValue=info, /No_Copy
        RETURN
    ENDIF

    ; Get the TLB of the full-sized window.
    Widget_Control, zoomID, GET_UVALUE=tlb
    
    ; If that base is gone, disappear!
    IF Widget_Info(tlb, /VALID_ID) EQ 0 THEN RETURN
    
    ; Get the information you need to redisplaythe image.
    Widget_Control, tlb, Get_UValue=info, /No_Copy
    
    ; Redisplay the image.
    WSet, info.drawIndex
    cgImage, info.image, BOTTOM=info.bottom, TOP=info.top, MINVALUE=info.minvalue, $
             MAXValue=info.maxValue, SCALE=info.scale, NOINTERP=info.nointerp
    WSet, info.pixIndex
    Device, Copy=[0, 0, info.xsize, info.ysize, 0, 0, info.drawIndex]
    
    ; Store the info structure.
    Widget_Control, tlb, Set_UValue=info, /No_Copy

END ; ----------------------------------------------------------------------


PRO FSC_ZImage_BoxColor, event

    ; Come here to change the selector box color.
  
    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message(/Quiet)
         
        ; Put the info structure back.
        IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
        RETURN
    ENDIF

    ; Get the information you need to redisplaythe image.
    Widget_Control, event.top, Get_UValue=info, /No_Copy
    
    boxcolor = PickColorName(info.boxColor)
    info.boxColor = boxColor
    
    ; Redisplay the image.
    WSet, info.drawIndex
    cgImage, info.image, BOTTOM=info.bottom, TOP=info.top, MINVALUE=info.minvalue, $
             MAXValue=info.maxValue, SCALE=info.scale, NOINTERP=info.nointerp
    WSet, info.pixIndex
    Device, Copy=[0, 0, info.xsize, info.ysize, 0, 0, info.drawIndex]
    
    ; Store the info structure.
    Widget_Control, event.top, Set_UValue=info, /No_Copy

END ; ----------------------------------------------------------------------


PRO FSC_ZImage_LoadColors, event

    ; Come here to load colors or to respond to color loading events.

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message(/Quiet)
         
        ; Put the info structure back.
        IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
        RETURN
    ENDIF

    Widget_Control, event.top, Get_UValue=info, /No_Copy
    
    ; What kind of event is this?
    thisEvent = Tag_Names(event, /Structure)
    
    ; Do the right thing.
    CASE thisEvent OF
    
        'WIDGET_BUTTON': BEGIN
            TVLCT, info.r, info.g, info.b, info.bottom
            XColors, Group=event.top, NColors = info.ncolors, $
                Bottom=info.bottom, NotifyID=[event.id, event.top], $
                Title='ZImage Colors (' + StrTrim(info.drawIndex,2) + ')'
            Widget_Control, info.controlID, Map=0
            info.map = 0
            END
            
        'XCOLORS_LOAD':BEGIN
    
                ; Extract the new color table vectors from XCOLORS.
    
            info.r = event.r(info.bottom:info.bottom+info.ncolors-1)
            info.g = event.g(info.bottom:info.bottom+info.ncolors-1)
            info.b = event.b(info.bottom:info.bottom+info.ncolors-1)
    
            ; Redisplay the image.
            WSet, info.drawIndex
            cgImage, info.image, BOTTOM=info.bottom, TOP=info.top, MINVALUE=info.minvalue, $
                MAXValue=info.maxValue, SCALE=info.scale, NOINTERP=info.nointerp

            WSet, info.pixIndex
            Device, Copy=[0, 0, info.xsize, info.ysize, 0, 0, info.drawIndex]
    
            ; Is a zoom window open? If so, redisplay it as well.
            IF Widget_Info(info.zoomDrawID, /Valid_ID) THEN BEGIN
               WSet, info.zoomWindowID
               IF Ptr_Valid(info.zoomedImage) THEN BEGIN
                  cgImage, *info.zoomedImage, BOTTOM=info.bottom, TOP=info.top, MINVALUE=info.minvalue, $
                      MAXValue=info.maxValue, SCALE=info.scale, NOINTERP=1
               ENDIF
            ENDIF
    
            END
    ENDCASE
    Widget_Control, event.top, Set_UValue=info, /No_Copy
END ; ----------------------------------------------------------------------





PRO FSC_ZImage_Quit, event
    Widget_Control, event.top, /Destroy
END ; ----------------------------------------------------------------------





PRO FSC_ZImage_Cleanup, tlb

   ; The purpose of this program is to delete the pixmap window
   ; when the program FSC_ZImage is destroyed. Get the info structure,
   ; which holds the pixmap window index number and delete the window.

    Widget_Control, tlb, Get_UValue=info, /No_Copy
    IF N_Elements(info) NE 0 THEN BEGIN
        WDelete, info.pixIndex
        Ptr_Free, info.zoomedImage
    ENDIF
END ; ----------------------------------------------------------------------


PRO FSC_ZImage_Factor, event

   ; The purpose of this event handler is to set the zoom factor.

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message(/Quiet)
         
        ; Put the info structure back.
        IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
        RETURN
    ENDIF
    
    Widget_Control, event.top, Get_UValue=info, /No_Copy
    Widget_Control, event.id, Get_UValue=factor
    info.zoomfactor = factor[event.index]
    Widget_Control, info.controlID, Map=0
    info.map = 0
    Widget_Control, event.top, Set_UValue=info, /No_Copy
END ; ----------------------------------------------------------------------





PRO FSC_ZImage_DrawEvents, event

   ; This event handler continuously draws and erases the zoom box until it
   ; receives an UP event from the draw widget. Then it turns draw widget
   ; motion events OFF.

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message(/Quiet)
         
        ; Turn motion events off.
        Widget_Control, event.id, Draw_Motion_Events=0        
        
        ; Put the info structure back.
        IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
        RETURN
    ENDIF

   ; Get the info structure out of the top-level base.
    Widget_Control, event.top, Get_UValue=info, /No_Copy
    
   ; What type of an event is this?
    possibleEventTypes = [ 'DOWN', 'UP', 'MOTION', 'SCROLL' ]
    thisEvent = possibleEventTypes[event.type]
    buttons = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']
    
    CASE thisEvent OF
    
       'DOWN': BEGIN
    
       ; Is this the left or right button?
       ; If RIGHT, then map or unmap controls.
       buttonPressed = buttons[event.press]
       IF buttonPressed EQ 'RIGHT' THEN BEGIN
          IF info.map EQ 1 THEN BEGIN
             Widget_Control, info.controlID, Map=0
             info.map = 0
          ENDIF ELSE BEGIN
             Widget_Control, info.controlID, Map=1
             info.map = 1
          ENDELSE
          Widget_Control, event.top, Set_UValue=info, /No_Copy
          RETURN
       ENDIF

      ; Set the static corners of the box to current
      ; cursor location.
      info.xs = event.x
      info.ys = event.y

      ; Turn draw MOTION events ON.
      Widget_Control, event.id, Draw_Motion_Events=1
    
      ENDCASE
    
      'UP': BEGIN
    
       ; Is this the left or right button?
       ; If RIGHT, then do nothing.
       buttonReleased = buttons[event.release]
       IF buttonReleased EQ 'RIGHT' THEN BEGIN
          Widget_Control, event.top, Set_UValue=info, /No_Copy
          RETURN
       ENDIF

      ; If this is an UP event, you need to erase the zoombox, turn motion events OFF, and
      ; draw the "zoomed" plot in both the draw widget and the pixmap.


      ; Turn motion events off.
      Widget_Control, event.id, Draw_Motion_Events=0

      ; Draw the "zoomed" image. Start by getting the LAST zoom
      ; box outline. These are indices into image array.
      event.x = 0 > event.x < (info.xsize - 1)
      event.y = 0 > event.y < (info.ysize - 1)
      x = [info.xs, event.x]
      y = [info.ys, event.y]
      
      ; Make sure the user didn't just click in the window.
      IF info.xs EQ event.x OR info.ys EQ event.y THEN BEGIN
      
          ; Erase the zoombox.
          WSet, info.drawIndex
          TVLCT, info.r, info.g, info.b
          
          ; Copy from the pximap.
          Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]
          Widget_Control, event.top, Set_UValue=info, /No_Copy
          RETURN
          
      ENDIF

      ; Make sure the x and y values are ordered as [min, max].
      IF info.xs GT event.x THEN x = [event.x, info.xs]
      IF info.ys GT event.y THEN y = [event.y, info.ys]
      
      ; Make sure these are in image pixel coordinates, not just
      ; window pixel coordinates.
      xvec = Scale_Vector(Indgen(info.xsize), 0, !D.X_Size-1)
      yvec = Scale_Vector(Indgen(info.ysize), 0, !D.Y_Size-1)
      x = Value_Locate(xvec, x)
      y = Value_Locate(yvec, y)

      ; Set the zoom factor and determine the new X and Y
      ; sizes of the Zoom Window.
      zoomXSize = (x[1] - x[0] + 1) * info.zoomFactor
      zoomYSize = (y[1] - y[0] + 1) * info.zoomFactor

      ; Subset the image, and apply the zoom factor to it.
      CASE info.trueIndex OF
          -1: imageSubset = info.image[x[0]:x[1], y[0]:y[1]]
           0: imageSubset = info.image[*, x[0]:x[1], y[0]:y[1]]
           1: imageSubset = info.image[x[0]:x[1], *, y[0]:y[1]]
           2: imageSubset = info.image[x[0]:x[1], y[0]:y[1], *]
      ENDCASE
      
      zoomedImage = FSC_Resize_Image(imageSubset, zoomXSize, zoomYSize, Interp=0)
      IF Ptr_Valid(info.zoomedImage) $
        THEN *info.zoomedImage = zoomedImage $
        ELSE info.zoomedImage = Ptr_New(zoomedImage, /No_Copy)

      ; If the Zoom Window exists, make it the proper size and load
      ; the zoomed image into it. If it does not exists, create it.
      IF Widget_Info(info.zoomDrawID, /Valid_ID) THEN BEGIN

         ; If the new zoomed image needs scroll bars, or the window has
         ; scroll bars, destroy it and recreate it.
         dims = Image_Dimensions(*info.zoomedimage, XSIZE=ixsize, YSIZE=iysize)
         IF (ixsize GT info.maxSize) OR (iysize GT info.maxSize) OR (info.hasScrollBars) THEN BEGIN
         
             ; Get offset positions for the non-existing zoom window.
             Widget_Control, info.zoomDrawID, TLB_Get_Offset=offsets
             xpos = offsets[0] 
             ypos = offsets[1]
             
             Widget_Control, info.zoomDrawID, /Destroy
             
             ; Calculate a window size. Maximum window size is 800.
             dims = Image_Dimensions(*info.zoomedimage, XSIZE=ixsize, YSIZE=iysize)
             aspect = Float(ixsize)/iysize
             MAXSIZE = 800
             IF ixsize GT MAXSIZE OR iysize GT MAXSIZE THEN BEGIN
                 x_scroll_size = MAXSIZE < ixsize
                 y_scroll_size = MAXSIZE < iysize
                 info.hasScrollBars = 1
                 
                 ; Make sure window is not off the display.
                 maxwinsize = MaxWindowSize()
                 IF (xpos + x_scroll_size) GT maxwinsize[0] THEN $
                    xpos = maxwinsize[0] - x_scroll_size
                 IF (ypos + y_scroll_size) GT maxwinsize[1] THEN $
                    ypos = maxwinsize[1] - y_scroll_size
             ENDIF ELSE info.hasScrollBars = 0
             
             ; Zoom window does not exist. Create it.
             zoomtlb = Widget_Base(Title='Zoomed Image', Group=event.top, $
                 XOffset=xpos, YOffset=ypos, KILL_NOTIFY='FSC_ZImage_ZoomDied', $
                 UVALUE=event.top, X_Scroll_Size=x_scroll_size, Y_Scroll_Size=y_scroll_size)
             zoomdraw = Widget_Draw(zoomtlb, XSize=zoomXSize, YSize=zoomYSize)
             Widget_Control, zoomtlb, /Realize
             Widget_Control, zoomdraw, Get_Value=windowID
             info.zoomDrawID = zoomdraw
             info.zoomWindowID = windowID
             WSet, windowID
             IF Ptr_Valid(info.zoomedImage) THEN BEGIN
                cgImage, *info.zoomedImage, BOTTOM=info.bottom, TOP=info.top, MINVALUE=info.minvalue, $
                    MAXValue=info.maxValue, SCALE=info.scale, NOINTERP=info.nointerp
             ENDIF
     
          ENDIF ELSE BEGIN
         
         ; Zoomed window exists. Make it correct size and load image.
         Widget_Control, info.zoomDrawID, XSize=zoomXSize, YSize=zoomYSize
         WSet, info.zoomWindowID
         IF Ptr_Valid(info.zoomedImage) THEN BEGIN
            cgImage, *info.zoomedImage, BOTTOM=info.bottom, TOP=info.top, MINVALUE=info.minvalue, $
                MAXValue=info.maxValue, SCALE=info.scale, NOINTERP=info.nointerp
         ENDIF
         
         ENDELSE
      ENDIF ELSE BEGIN

         ; Get offset positions for the non-existing zoom window.
         Widget_Control, event.top, TLB_Get_Size=sizes, TLB_Get_Offset=offsets
         xpos = sizes[0] + offsets[0] + 20
         ypos = offsets[1] + 40
         
         ; Calculate a window size. Maximum window size is 800.
         dims = Image_Dimensions(*info.zoomedimage, XSIZE=ixsize, YSIZE=iysize)
         aspect = Float(ixsize)/iysize
         MAXSIZE = 800
         IF ixsize GT MAXSIZE OR iysize GT MAXSIZE THEN BEGIN
             x_scroll_size = MAXSIZE < ixsize
             y_scroll_size = MAXSIZE < iysize
             info.hasScrollBars = 1
                 
             ; Make sure window is not off the display.
             maxwinsize = MaxWindowSize()
             IF (xpos + x_scroll_size) GT maxwinsize[0] THEN $
                xpos = maxwinsize[0] - x_scroll_size
             IF (ypos + y_scroll_size) GT maxwinsize[1] THEN $
                ypos = maxwinsize[1] - y_scroll_size
         ENDIF ELSE info.hasScrollBars = 0
         
         ; Zoom window does not exist. Create it.
         zoomtlb = Widget_Base(Title='Zoomed Image', Group=event.top, $
             XOffset=xpos, YOffset=ypos, KILL_NOTIFY='FSC_ZImage_ZoomDied', $
             UVALUE=event.top, X_Scroll_Size=x_scroll_size, Y_Scroll_Size=y_scroll_size)
         zoomdraw = Widget_Draw(zoomtlb, XSize=zoomXSize, YSize=zoomYSize)
         Widget_Control, zoomtlb, /Realize
         Widget_Control, zoomdraw, Get_Value=windowID
         info.zoomDrawID = zoomdraw
         info.zoomWindowID = windowID
         WSet, windowID
         IF Ptr_Valid(info.zoomedImage) THEN BEGIN
            cgImage, *info.zoomedImage, BOTTOM=info.bottom, TOP=info.top, MINVALUE=info.minvalue, $
                MAXValue=info.maxValue, SCALE=info.scale, NOINTERP=info.nointerp
         ENDIF
         
      ENDELSE

      ; If the controls were mapped, unmap them.
      IF info.map EQ 1 THEN BEGIN
          Widget_Control, info.controlID, Map=0
          info.map = 0
      ENDIF
    
      ENDCASE

    'MOTION': BEGIN

    ; Most of the action in this event handler occurs here while we are waiting
    ; for an UP event to occur. As long as we don't get it, keep erasing the
    ; old zoom box and drawing a new one.

    ; Erase the old zoom box.
    WSet, info.drawIndex
    TVLCT, info.r, info.g, info.b, info.bottom
    Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]

    ; Update the dynamic corner of the zoom box to the current cursor location.
    info.xd = event.x
    info.yd = event.y

    ; Draw the zoom box. 
    Device, Get_Decomposed=theState
    Device, Decomposed=1
    PlotS, [info.xs, info.xs, info.xd, info.xd, info.xs], $
       [info.ys, info.yd, info.yd, info.ys, info.ys], $
       /Device, Color=cgColor(info.boxcolor)
    Device, Decomposed=theState
       
    ENDCASE

ENDCASE

   ; Put the info structure back into its storage location.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ; ----------------------------------------------------------------------


;+
; :Description:
;   Allows the user to interactively zoom into an image. Program controls are available
;   by right-clicking in the full-sized image window. Zoom factors from 2x to 16x are
;   available. Use the left mouse button to draw a box on the full-sized image to locate
;   the region of the image to zoom.
;
; :Categories:
;    Graphics
;    
; :Params:
;    image: in, required, type=any
;        A 2D or true-color image of any normal data type. If not a BYTE array,
;        cgImage keywords for proper image scaling must be used to provide image
;        scaling parameters.
;        
;       
; :Keywords:
;     bottom: in, optional, type=byte, default=0
;         As defined for the cgImage command.
;     boxcolor: in, optional, type=string, default='red8'
;         The name of a color to use for the rubberband selection box.
;         Color names are those used with cgColor. 
;     group_leader: in, optional, type=long
;         The Group Leader widget identifier for this widget program.
;     maxvalue: in, optional, type=byte, default=0
;         As defined for the cgImage command.
;     minvalue: in, optional, type=byte, default=0
;         As defined for the cgImage command.
;     ncolors: in, optional, type=byte, default=0
;         As defined for the cgImage command.
;     nointerpolate: in, optional, type=byte, default=0
;         As defined for the cgImage command.
;     scale: in, optional, type=byte, default=0
;         As defined for the cgImage command.
;     top: in, optional, type=byte, default=0
;         As defined for the cgImage command.
;          
; :Examples:
;    Code examples::
;       IDL> image = cgDemoData(7)
;       IDL> FSC_ZImage, image ; 2D image
;       IDL> image = cgDemoData(16)
;       IDL> FSC_ZImage, image ; True-Color image
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
;        Written, 23 November 2010. DWF.
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO FSC_ZImage, image, $
    BOTTOM=bottom, $
    BOXCOLOR=sboxcolor, $
    GROUP_LEADER=group_leader, $
    MAXVALUE=maxvalue, $
    MINVALUE=minvalue, $
    NCOLORS=ncolors, $
    NOINTERPOLATE=nointerp, $
    SCALE=scale, $
    TOP=top
    

    Compile_Opt idl2
    
    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    ; Was an image passed into the procedure?
    ; If not, find one in the IDL examples/data directory.
    IF N_Params() EQ 0 THEN BEGIN
       image = ImageSelect(FILENAME='marsglobe.jpg', CANCEL=cancelled, /EXAMPLES)
       IF cancelled THEN RETURN
    ENDIF
    
    ; Just make sure nothing undefined got passed in.
    IF N_Elements(image) EQ 0 THEN Message, 'An image parameter is required.

    ; Check for keywords. 
    IF N_Elements(sboxcolor) EQ 0 THEN boxcolor = 'red8' ELSE boxcolor = sboxcolor
    IF N_Elements(factor) EQ 0 THEN factor = 4
    nointerp = Keyword_Set(nointerp)
    
    ; Get image size.
    dims = Image_Dimensions(image, XSize=ixsize, YSize=iysize, $
        XIndex=xindex, YIndex=yindex, TrueIndex=trueindex)
    
    ; Calculate a window size. Maximum window size is 800.
     aspect = Float(ixsize)/iysize
     MAXSIZE = 800
     IF ixsize GT MAXSIZE OR iysize GT MAXSIZE THEN BEGIN
         IF ixsize NE iysize THEN BEGIN
            aspect = Float(ysize) / ixsize
            IF aspect LT 1 THEN BEGIN
               xsize = MAXSIZE
               ysize = (MAXSIZE * aspect) < MAXSIZE
            ENDIF ELSE BEGIN
               ysize = MAXSIZE
               xsize = (MAXSIZE / aspect) < MAXSIZE
            ENDELSE
         ENDIF ELSE BEGIN
            ysize = MAXSIZE
            xsize = MAXSIZE
         ENDELSE
     ENDIF ELSE BEGIN
        xsize = ixsize
        ysize = iysize
     ENDELSE

    
    ; Create a top-level base for this program. No resizing of this base.
    tlb = Widget_Base(TLB_Frame_Attr=1)

    ; Create two bases. One for controls and the other for the
    ; draw widget. Leave the control base unmapped for now.
    controlID = Widget_Base(tlb, Map=0, Column=1)
    factorString = ['2x', '3x', '4x', '5x', '6x', '7x', '8x', '12x', '16x']
    factors = [Indgen(7) + 2, 12, 16]
    zoomfactor = Widget_DropList(controlID, Value=factorString, $
       Event_Pro='FSC_ZImage_Factor', UValue=factors, Title='Zoom Factor')
    IF trueindex EQ -1 THEN BEGIN
        colors = Widget_Button(controlID, Value='Load Image Colors', Event_Pro='FSC_ZImage_LoadColors')
    ENDIF
    void = Widget_Button(controlID, Value='Change Selection Box Color', Event_Pro='FSC_ZImage_BoxColor')
    quitter = Widget_Button(controlID, Value='Exit Program', $
       Event_Pro='FSC_ZImage_Quit')
    
    drawbase = Widget_Base(tlb, Map=1)
    draw = Widget_Draw(drawbase, XSize=xsize, YSize=ysize, $
       Button_Events=1, Event_Pro='FSC_ZImage_DrawEvents')

    ; Realize the program.
    Widget_Control, tlb, /Realize
    
    ; Set the initial default zoom factor.
    Widget_Control, zoomfactor, SET_DROPLIST_SELECT=2
    
    ; Get the window index number of the draw widget.
    ; Make the draw widget the current graphics window
    ; and display the image in it.
    Widget_Control, draw, Get_Value=drawIndex
    WSet, drawIndex
    cgImage, image, BOTTOM=bottom, TOP=top, MINVALUE=minvalue, $
       MAXValue=maxValue, NCOLOR=ncolors, SCALE=scale, NOINTERP=nointerp

    ; Set the title of the window.
    Widget_Control, tlb, TLB_Set_Title='Full Size Image (' + StrTrim(drawIndex,2) + ') -- ' + $
       'Right Click for Controls.'

    ; Create a pixmap window the same size as the draw widget window.
    ; Store its window index number in a local variable. Display the
    ; image you just put in the draw widget in the pixmap window.
    Window, /Free, XSize=xsize, YSize=ysize, /Pixmap
    pixIndex = !D.Window
    cgImage, image, BOTTOM=bottom, TOP=top, MINVALUE=minvalue, $
       MAXValue=maxValue, NCOLOR=ncolors, SCALE=scale, NOINTERP=nointerp

   ; Get color vectors for this application.
   TVLCT, r, g, b, /Get

   ; Create an info structure to hold information required by the program.
   
    info = { $
       image:image, $               ; The original image.
       zoomedimage:Ptr_New(), $     ; The scaled and resized subimage.
       xsize:ixsize, $              ; The x size of the image.
       ysize:iysize, $              ; The y size of the image.
       drawIndex:drawIndex, $       ; The draw window index number.
       pixIndex:pixIndex, $         ; The pixmap window index number.
       ncolors:ncolors, $           ; The number of colors for the image.
       bottom:bottom, $             ; The bottom color index.
       top:top, $                   ; The TOP keyword for scaling the image.
       maxvalue:maxvalue, $         ; The MAXVALUE keyword for scaling the image.
       minvalue:minvalue, $         ; The MINVALUE keyword for scaling the image.
       scale:scale, $               ; The SCALE keyword for scaling the image.
       boxcolor:boxcolor, $         ; The name of the drawing color.
       xs:0, $                      ; X static corner of the zoom box.
       ys:0, $                      ; Y static corner of the zoom box.
       xd:0, $                      ; X dynamic corner of the zoom box.
       yd:0, $                      ; Y dynamic corner of the zoom box.
       zoomDrawID:-1L, $            ; Zoomed image draw widget ID.
       zoomWindowID:-1, $           ; Zoomed image window index number.
       r:r, $                       ; The red color vector.
       g:g, $                       ; The green color vector.
       b:b, $                       ; The blue color vector.
       zoomfactor:factor, $         ; The initial zoom factor.
       nointerp:nointerp, $         ; A flag to select nearest neighbor or bilinear resampling.
       map:0, $                     ; A flag to tell if the controls are mapped.
       xindex:xindex, $             ; The X size index.
       yindex:yindex, $             ; The Y size index.
       trueIndex:trueIndex, $       ; The "true-color" index. 0 if image is 2D.
       MAXSIZE:maxsize, $           ; The maximum window size.
       hasScrollBars: 0, $          ; A flag indicating the zoom window has scroll bars.
       controlID:controlID}         ; The identifier of the control base to map.

    ; Store the info structure in the user value of the top-level base.
    Widget_Control, tlb, Set_UValue=info, /No_Copy

    ; Register this program and set up the event loop.
    XManager, 'fsc_zimage', tlb, Cleanup='FSC_ZImage_Cleanup', Group_Leader=group, /No_Block
    
END ; ----------------------------------------------------------------------


