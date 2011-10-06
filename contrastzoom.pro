;+
; NAME:
;       CONTRASTZOOM
;
; PURPOSE:
;
;       The purpose of this program is to demonstrate how to
;       zoom an image "in place" and how to window and level
;       (set "contrast and brightness") an image using object
;       graphics functionality. The exercise involves using
;       multiple views in an object graphics scene, and being
;       able to interact with different views in different ways.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:
;
;       Widgets, Object Graphics.
;
; CALLING SEQUENCE:
;
;       ContrastZoom, image
;
; REQUIRED INPUTS:
;
;       None. The image "mr_knee.dcm" from the examples/data directory
;       is used if no data is supplied in call.
;
; OPTIONAL INPUTS
;
;       image: A 2D image array of any data type.
;
; OPTIONAL KEYWORD PARAMETERS:
;
;       COLORTABLE: The number of a color table to use as the image palette.
;       Color table 0 (grayscale) is used as a default.

;       GROUP_LEADER: The group leader for this program. When the group leader
;       is destroyed, this program will be destroyed.
;
; COMMON BLOCKS:
;
;       None.
;
; SIDE EFFECTS:
;
;       None.
;
; RESTRICTIONS:
;
;       None. The Coyote Library program VCOLORBAR is included.
;
; EXAMPLE:
;
;       To use this program with your 8-bit image data and a red-temperature
;       color scale, type:
;
;          IDL> ContrastZoom, image, Colortable=3
;
; NOTES:
;
;       The left image is used to "zoom" into a portion of the image.
;       The aspect ratio of the sub-image is always preserved. To see
;       the entire image, click and release the mouse button in this
;       window.
;
;       The center image is used to adjust the contrast and brightness
;       (sometimes called the "window" and "level" of the image. Click and
;       drag the mouse vertically to set contrast. Click and drag the mouse
;       horizontally to set brightness. To return to original values (25%
;       contrast and 75% brightness), click and release in the center image.
;
;       The color bars shows the image values of the image.
;
; MODIFICATION HISTORY:
;
;       Written by David Fanning, 18 November 2001.
;       Added second colorbar to show the relationship of the clamped
;          colors to the overall image values. 19 November 2001. DWF.
;-
;
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
PRO ContrastZoom_VColorBar::Clamp, datarange

; This method clamps the data to a particular data range.

self->GetProperty, Range=currentRange

thisclamp = Bytscl(datarange, Max=currentRange[1], Min=currentRange[0])
bar = BytScl(Replicate(1B,10) # Bindgen(self.ncolors), Min=thisclamp[0], Max=thisclamp[1])
self.thisImage->SetProperty, Data=bar
END
;-------------------------------------------------------------------------



FUNCTION ContrastZoom_VColorBar::INIT, Position=position, $
    NColors=ncolors, Title=title, Palette=palette, $
    Major=major, Minor=minor, Range=range, Color=color, $
    _Extra=extra, Name=name

   ; Catch possible errors.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Dialog_Message(!Error_State.Msg)
   Message, !Error_State.Msg, /Informational
   RETURN, 0
ENDIF

   ; Initialize model superclass.

IF (self->IDLgrModel::Init(_EXTRA=extra) NE 1) THEN RETURN, 0

    ; Define default values for keywords, if necessary.

IF N_Elements(name) EQ 0 THEN name=''
IF N_Elements(color) EQ 0 THEN self.color = [255,255,255] $
   ELSE self.color = color
thisFont = Obj_New('IDLgrFont', 'Helvetica', Size=8.0)
self.thisFont = thisFont
IF N_Elements(title) EQ 0 THEN title=''

thisTitle = Obj_New('IDLgrText', title, Color=self.color, $
    Font=thisFont, Recompute_Dimensions=2, /Enable_Formatting)

IF N_Elements(ncolors) EQ 0 THEN self.ncolors = 256 $
   ELSE self.ncolors = ncolors
IF N_Elements(palette) EQ 0 THEN BEGIN
    red = (green = (blue = BIndGen(self.ncolors)))
    self.palette = Obj_New('IDLgrPalette', red, green, blue)
ENDIF ELSE self.palette = palette
IF N_Elements(range) EQ 0 THEN self.range = [0, self.ncolors] $
   ELSE self.range = range
IF N_Elements(major) EQ 0 THEN self.major = 5 $
   ELSE self.major = major
IF N_Elements(minor) EQ 0 THEN self.minor = 4 $
   ELSE self.minor = minor
IF N_Elements(position) EQ 0 THEN self.position = [0.90, 0.10, 0.95, 0.90] $
   ELSE self.position = position

    ; Create the colorbar image. Get its size.

bar = REPLICATE(1B,10) # BINDGEN(self.ncolors)
s = SIZE(bar, /Dimensions)
xsize = s[0]
ysize = s[1]

    ; Create the colorbar image object. Add palette to it.

thisImage = Obj_New('IDLgrImage', bar, Palette=self.palette)
xs = FSC_Normalize([0,xsize], Position=[0,1.])
ys = FSC_Normalize([0,ysize], Position=[0,1.])
thisImage->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys

   ; Create a polygon object. Add the image as a texture map. We do
   ; this so the image can rotate in 3D space.

thisPolygon = Obj_New('IDLgrPolygon', [0, 1, 1, 0], [0, 0, 1, 1], [0,0,0,0], $
   Texture_Map=thisImage, Texture_Coord = [[0,0], [1,0], [1,1], [0,1]], color=[255,255,255])

    ; Scale the polygon into the correct position.

xs = FSC_Normalize([0,1], Position=[self.position(0), self.position(2)])
ys = FSC_Normalize([0,1], Position=[self.position(1), self.position(3)])
thispolygon->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys

    ; Create scale factors to position the axes.

longScale = FSC_Normalize(self.range, Position=[self.position(1), self.position(3)])
shortScale = FSC_Normalize([0,1], Position=[self.position(0), self.position(2)])

    ; Create the colorbar axes. 1000 indicates this location ignored.

shortAxis1 = Obj_New("IDLgrAxis", 0, Color=self.color, Ticklen=0.025, $
    Major=1, Range=[0,1], /NoText, /Exact, XCoord_Conv=shortScale,  $
    Location=[1000, self.position(1), 0.001])
shortAxis2 = Obj_New("IDLgrAxis", 0, Color=self.color, Ticklen=0.025, $0.001
    Major=1, Range=[0,1], /NoText, /Exact, XCoord_Conv=shortScale,  $
    Location=[1000, self.position(3), 0.001], TickDir=1)

textAxis = Obj_New("IDLgrAxis", 1, Color=self.color, Ticklen=0.025, $
    Major=self.major, Minor=self.minor, Title=thisTitle, Range=self.range, /Exact, $
    YCoord_Conv=longScale, Location=[self.position(0), 1000, 0.001])
textAxis->GetProperty, TickText=thisText
thisText->SetProperty, Font=self.thisFont, Recompute_Dimensions=2

longAxis2 = Obj_New("IDLgrAxis", 1, Color=self.color, /NoText, Ticklen=0.025, $
    Major=self.major, Minor=self.minor, Range=self.range, TickDir=1, $
    YCoord_Conv=longScale, Location=[self.position(2), 1000, 0.001], /Exact)

    ; Add the parts to the colorbar model.

self->Add, shortAxis1
self->Add, shortAxis2
self->Add, textAxis
self->Add, longAxis2
self->Add, thisPolygon

   ; Assign the name.

self->IDLgrModel::SetProperty, Name=name, Select_Target=1

    ; Create a container object and put the objects into it.

thisContainer = Obj_New('IDL_Container')
thisContainer->Add, thisFont
thisContainer->Add, thisImage
thisContainer->Add, thisText
thisContainer->Add, thisTitle
thisContainer->Add, self.palette
thisContainer->Add, textAxis
thisContainer->Add, shortAxis1
thisContainer->Add, shortAxis2
thisContainer->Add, longAxis2

    ; Update the SELF structure.

self.thisImage = thisImage
self.thisFont = thisFont
self.thisText = thisText
self.textAxis = textAxis
self.shortAxis1 = shortAxis1
self.shortAxis2 = shortAxis2
self.longAxis2 = longAxis2
self.thisContainer = thisContainer
self.thisTitle = thisTitle

RETURN, 1
END
;-------------------------------------------------------------------------



PRO ContrastZoom_VColorBar::Cleanup

    ; Lifecycle method to clean itself up.

Obj_Destroy, self.thisContainer
self->IDLgrMODEL::Cleanup
END
;-------------------------------------------------------------------------



PRO ContrastZoom_VColorBar::GetProperty, Position=position, Text=text, $
    Title=title, Palette=palette, Major=major, Minor=minor, $
    Range=range, Color=color, Name=name, $
    Transform=transform, _Ref_Extra=extra

    ; Get the properties of the colorbar.

IF Arg_Present(position) THEN position = self.position
IF Arg_Present(text) THEN text = self.thisText
IF Arg_Present(title) THEN self.thisTitle->GetProperty, Strings=title
IF Arg_Present(palette) THEN palette = self.palette
IF Arg_Present(major) THEN major = self.major
IF Arg_Present(minor) THEN minor = self.minor
IF Arg_Present(range) THEN range = self.range
IF Arg_Present(color) THEN color = self.color
IF Arg_Present(name) THEN self->IDLgrMODEL::GetProperty, Name=name
IF Arg_Present(transform) THEN self->IDLgrMODEL::GetProperty, Transform=transform
IF Arg_Present(extra) THEN self->IDLgrMODEL::GetProperty, _Extra=extra

END
;-------------------------------------------------------------------------



PRO ContrastZoom_VColorBar::SetProperty, Position=position, $
    Title=title, Palette=palette, Major=major, Minor=minor, $
    Range=range, Color=color, Name=name, Transform=transform, _Extra=extra

    ; Set properties of the colorbar.

IF N_Elements(position) NE 0 THEN BEGIN
    self.position = position

        ; Find the size of the image.

    self.thisImage->GetProperty, Data=image
    s = Size(image)
    xsize = s(1)
    ysize = s(2)
    xs = FSC_Normalize([0,xsize], Position=[position(0), position(2)])
    ys = FSC_Normalize([0,ysize], Position=[position(1), position(3)])
    self.thisImage->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys

        ; Create new scale factors to position the axes.

    longScale = FSC_Normalize(self.range, $
       Position=[self.position(1), self.position(3)])
    shortScale = FSC_Normalize([0,1], $
       Position=[self.position(0), self.position(2)])

        ; Position the axes. 1000 indicates this position ignored.

    self.textaxis->SetProperty, YCoord_Conv=longScale, $
       Location=[self.position(0), 1000, 0]
    self.longaxis2->SetProperty, YCoord_Conv=longScale, $
       Location=[self.position(2), 1000, 0]
    self.shortAxis1->SetProperty, XCoord_Conv=shortScale, $
       Location=[1000, self.position(1), 0]
    self.shortAxis2->SetProperty, XCoord_Conv=shortScale, $
       Location=[1000, self.position(3), 0]

ENDIF
IF N_Elements(title) NE 0 THEN self.thisTitle->SetProperty, Strings=title
IF N_Elements(transform) NE 0 THEN self->IDLgrMODEL::SetProperty, Transform=transform
IF N_Elements(palette) NE 0 THEN BEGIN
    self.palette = palette
    self.thisImage->SetProperty, Palette=palette
ENDIF
IF N_Elements(major) NE 0 THEN BEGIN
    self.major = major
    self.textAxis->SetProperty, Major=major
    self.longAxis2->SetProperty, Major=major
END
IF N_Elements(minor) NE 0 THEN BEGIN
    self.minor = minor
    self.textAxis->SetProperty, Minor=minor
    self.longAxis2->SetProperty, Minor=minor
END
IF N_Elements(range) NE 0 THEN BEGIN
    self.range = range
    longScale = FSC_Normalize(range, $
       Position=[self.position(1), self.position(3)])
    self.textAxis->SetProperty, Range=range, YCoord_Conv=longScale
    self.longAxis2->SetProperty, Range=range, YCoord_Conv=longScale
ENDIF
IF N_Elements(color) NE 0 THEN BEGIN
    self.color = color
    self.textAxis->SetProperty, Color=color
    self.longAxis2->SetProperty, Color=color
    self.shortAxis1->SetProperty, Color=color
    self.shortAxis2->SetProperty, Color=color
    self.thisText->SetProperty, Color=color
ENDIF
IF N_Elements(name) NE 0 THEN self->IDLgrMODEL::SetProperty, Name=name
IF N_Elements(extra) NE 0 THEN self->IDLgrMODEL::SetProperty, _Extra=extra
END
;-------------------------------------------------------------------------



PRO ContrastZoom_VColorBar__Define

; For details on how this colorbar object works, see the VCOLORBAR__DEFINE.PRO
; program in the Coyote Library.
;
;   http://www.idlcoyote.com/programs/vcolorbar__define.pro

colorbar = { ContrastZoom_VColorBar, $
             INHERITS IDLgrMODEL, $      ; Inherits the Model Object.
             Position:FltArr(4), $       ; The position of the colorbar.
             Palette:Obj_New(), $        ; The colorbar palette.
             thisImage:Obj_New(), $      ; The colorbar image.
             imageModel:Obj_New(), $     ; The colorbar image model.
             thisContainer:Obj_New(), $  ; Container for cleaning up.
             thisFont:Obj_New(), $       ; The annotation font object.
             thisText:Obj_New(), $       ; The bar annotation text object.
             thisTitle: Obj_New(), $     ; The title of the colorbar.
             textAxis:Obj_New(), $       ; The axis containing annotation.
             shortAxis1:Obj_New(), $     ; A short axis.
             shortAxis2:Obj_New(), $     ; A second short axis.
             longAxis2:Obj_New(), $      ; The other long axis.
             NColors:0, $                ; The number of colors in the bar.
             Major:0, $                  ; Number of major axis intervals.
             Minor:0, $                  ; Number of minor axis intervals.
             Color:BytArr(3), $          ; Color of axes and annotation.
             Range:FltArr(2) }           ; The range of the colorbar axis.

END
;-------------------------------------------------------------------------



FUNCTION ContrastZoom_Aspect, aspectRatio, MARGIN=margin, WindowAspect=wAspectRatio

; This function calculates the correct aspect ratios for positioning
; objects in windows.

ON_ERROR, 2

   ; Check for aspect ratio parameter and possibilities.

IF N_PARAMS() EQ 0 THEN aspectRatio = 1.0

IF aspectRatio EQ 0 THEN BEGIN
   MESSAGE, 'Aspect Ratio of 0. Changing to 1...', /Informational
   aspectRatio = 1.0
ENDIF

s = SIZE(aspectRatio)
IF s(s(0)+1) NE 4 THEN $
   MESSAGE, 'Aspect Ratio is not a FLOAT. Take care...', /Informational

   ; Check for margins.

IF N_ELEMENTS(margin) EQ 0 THEN margin = 0.15

   ; Error checking.

IF margin LT 0 OR margin GE 0.5 THEN $
   MESSAGE, 'The MARGIN keyword value must be between 0.0 and 0.5.'

   ; Calculate the aspect ratio of the current window.

IF N_Elements(wAspectRatio) EQ 0 THEN wAspectRatio = FLOAT(!D.Y_VSIZE) / !D.X_VSIZE

   ; Calculate normalized positions in window.

IF (aspectRatio LE wAspectRatio) THEN BEGIN
   xstart = margin
   ystart = 0.5 - (0.5 - margin) * (aspectRatio / wAspectRatio)
   xend = 1.0 - margin
   yend = 0.5 + (0.5 - margin) * (aspectRatio / wAspectRatio)
ENDIF ELSE BEGIN
   xstart = 0.5 - (0.5 - margin) * (wAspectRatio / aspectRatio)
   ystart = margin
   xend = 0.5 + (0.5 - margin) * (wAspectRatio / aspectRatio)
   yend = 1.0 - margin
ENDELSE

position = [xstart, ystart, xend, yend]

RETURN, position
END
;-------------------------------------------------------------------------



PRO ContrastZoom_Resize, event

; This event handler responds to resize events.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Resize the draw widget. This is the proper way to do this
    ; in object graphics, but it does not always work in UNIX
    ; versions of IDL. If it doesn't work for you, comment the
    ; first line out and try the second. The second line is more
    ; portable, but not exactly the proper "object" way. :-(

info.theWindow->SetProperty, Dimensions=[event.x, event.y-20]
;Widget_Control, info.drawID, Draw_XSize=event.x, Draw_YSize=event.y-20


   ; Update the aspect ratios and re-position the images
   ; in the window.

sz = Size(*info.subimage, /Dimensions)
imageAspect = Float(sz[1]) / sz[0]
info.theWindow->GetProperty, Dimensions=dims
windowAspect = (450./info.window_ysize * dims[1]) / (300./info.window_xsize * dims[0])
pos = ContrastZoom_Aspect(imageAspect, WindowAspect=windowAspect, Margin=0)
info.zoomImage->GetProperty, XRange=xrange, YRange=yrange
xs = FSC_Normalize(xrange, Position=[pos(0), pos(2)])
ys = FSC_Normalize(yrange, Position=[pos(1), pos(3)])
info.zoomImage->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys
info.theBox->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys

sc = Size(*info.image, /Dimensions)
imageAspect = Float(sc[1]) / sc[0]
info.theWindow->GetProperty, Dimensions=dims
windowAspect = (450./info.window_ysize * dims[1]) / (300./info.window_xsize * dims[0])
pos = ContrastZoom_Aspect(imageAspect, WindowAspect=windowAspect, Margin=0)
info.contrastImage->GetProperty, XRange=xrange, YRange=yrange
xs = FSC_Normalize(xrange, Position=[pos(0), pos(2)])
ys = FSC_Normalize(yrange, Position=[pos(1), pos(3)])
info.contrastImage->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys

   ; Draw the scene.

info.theWindow->Draw, info.theScene
Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;-------------------------------------------------------------------------



PRO ContrastZoom_DistinguishEvents, event

; This event handler responds to all draw widget events.

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message(/Traceback)
   Widget_Control, event.top, Set_UValue=info, /No_Copy
   RETURN
ENDIF

   ; Get the info structure.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Which view object are we dealing with. Find out by getting
   ; the UVALUE from the view obect?

thisView = info.theWindow->Select(info.theScene, [event.x, event.y], Dimensions=[1,1])
thisView = thisView[0]
IF Obj_Valid(thisView) EQ 0 THEN BEGIN
   possibleEventTypes = [ 'DOWN', 'UP', 'MOTION', 'SCROLL', 'EXPOSE' ]
   thisEvent = possibleEventTypes(event.type)
   IF thisEvent EQ 'EXPOSE' THEN info.theWindow->Draw, info.theScene
   Widget_Control, event.top, Set_UValue=info, /No_Copy
   RETURN
ENDIF
thisView->GetProperty, UValue=selectWindow

   ; What happens depends upon which view the event comes from.

CASE selectWindow OF

   'ZOOMWINDOW': BEGIN ; You are trying to zoom into a region in the left-hand image.

         ; Make sure ContrastWindow events don't show up here.

      IF info.currentMode EQ 'CONTRASTWINDOW' THEN BEGIN
         Widget_Control, event.top, Set_UValue=info, /No_Copy
         RETURN
      ENDIF

         ; Find the point in the coordinates of the image in the window.

      hit = info.theWindow->Pickdata(thisView, info.zoomImage, [event.x, event.y], xyz)
      xpt = Floor(xyz[0])
      ypt = Floor(xyz[1])

      possibleEventTypes = [ 'DOWN', 'UP', 'MOTION', 'SCROLL', 'EXPOSE' ]
      thisEvent = possibleEventTypes(event.type)

      CASE thisEvent OF

         'DOWN': BEGIN

               ; While the coordinates are in the image coordinate system, it is
               ; possible that they are *outside* the actual coordinates of the
               ; image. Make sure they don't exceed the size of the image.
               ; What you do with the point depends on what kind of event it is.

            IF xpt LT 0 OR xpt GT (info.zxsize-1) THEN BEGIN
               Widget_Control, event.top, Set_UValue=info, /No_Copy
               RETURN
            ENDIF
            IF ypt LT 0 OR ypt GT (info.zysize-1) THEN BEGIN
               Widget_Control, event.top, Set_UValue=info, /No_Copy
               RETURN
            ENDIF

               ; Set the static corners of the box to current
               ; cursor location. See the current mode so dragging
               ; outside the image can't cause problems.

            info.xs = xpt
            info.ys = ypt
            info.currentMode = "ZOOMWINDOW"

               ; Change the event handler for the draw widget and turn MOTION
               ; events ON.

            Widget_Control, event.id, Draw_Motion_Events=1

               ; Initialize and hide the polyline object.

            box = FltArr(2,5)
            box[0,*] = Replicate(info.xs, 5)
            box[1,*] = Replicate(info.ys, 5)
            info.theBox->SetProperty, Data=box, Hide=0

            END ; of DOWN event

        'UP': BEGIN

              ; It is possible to get an UP event without a previous DOWN event. (For
              ; example, the user starts the box outside the draw widget window.) If this
              ; occurs, info.xs and info.ys will be negative. Check for this and return,
              ; if necessary.

           IF info.xs EQ -1 OR info.ys EQ -1 THEN BEGIN
              Widget_Control, event.top, Set_UValue=info, /No_Copy
              RETURN
           ENDIF

              ; If this is an UP event, you need to erase the zoombox, turn motion events
              ; OFF, and draw the "zoomed" plot.

              ; Turn motion events off. Set the current mode to NULL.

           Widget_Control, event.id, Draw_Motion_Events=0
           Widget_Control, event.id, /Clear_Events
           info.currentMode = ""

              ; Draw the "zoomed" image. Start by getting the LAST zoom
              ; box outline. These are indices into image array.

              xpt = 0 > xpt < (info.zxsize)
              ypt = 0 > ypt < (info.zysize)

              x = [info.xs, xpt]
              y = [info.ys, ypt]

                 ; If the static point and the dynamic point are the same,
                 ; zoom all the way out.

           IF Abs(info.xs - xpt) LT 2 AND Abs(info.ys-ypt) LT 2 THEN BEGIN
              s = Size(*info.image, /Dimensions)
              info.zxsize = s[0]
              info.zysize = s[1]
              contrast = info.contrast
              brightness = info.brightness
              level = (1-brightness/100.)*(info.maxVal - info.minVal) + info.minVal
              width = (1-contrast/100.)*(info.maxVal - info.minVal)

                 ; Calculate display minimum and maximum.

              displayMax = (level + (width / 2))
              displayMin = (level - (width / 2))

              IF displayMax GT info.maxval THEN BEGIN
                 difference = Abs(displayMax - info.maxval)
                 displayMax = displayMax - difference
                 displayMin = displayMin - difference
              ENDIF
              IF displayMin LT info.minval THEN BEGIN
                 difference = Abs(info.minval - displayMin)
                 displayMin = displayMin + difference
                 displayMax = displayMax + difference
              ENDIF

                     ; Display the image.

              info.zoomImage->SetProperty, Data=BytScl(*info.image, Min=info.minval > $
                  displayMin, Max=displayMax < info.maxval), Dimensions=s
              *info.subimage = *info.image
              imageAspect = Float(s[1]) / s[0]
              info.theWindow->GetProperty, Dimensions=dims
              windowAspect = (450./info.window_ysize * dims[1]) / (300./info.window_xsize * dims[0])
              pos = ContrastZoom_Aspect(imageAspect, WindowAspect=windowAspect, Margin=0)
              info.zoomImage->GetProperty, XRange=xrange, YRange=yrange
              xs = FSC_Normalize(xrange, Position=[pos(0), pos(2)])
              ys = FSC_Normalize(yrange, Position=[pos(1), pos(3)])

              info.zoomImage->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys
              info.theBox->SetProperty, Hide=1, XCoord_Conv=xs, YCoord_Conv=ys
              info.theWindow->Draw, info.theScene
              info.xs = 0
              info.ys = 0
              info.xd = s[0]
              info.yd = s[1]
              Widget_Control, event.top, Set_UValue=info, /No_Copy
              RETURN
           ENDIF

              ; Make sure the x and y values are ordered as [min, max].

           IF info.xs GT xpt THEN x = [xpt, info.xs]
           IF info.ys GT ypt THEN y = [ypt, info.ys]

              ; Subset the image.


           *info.subimage = (*info.subimage)(x[0]:x[1] < (info.zxsize-1), y[0]:y[1] < (info.zysize-1))
           zoomedImage = *info.subimage

              ; Update the zoomed image data and draw it.

           s = Size(zoomedImage, /Dimensions)
           info.zxsize = s[0]
           info.zysize = s[1]
           contrast = info.contrast
           brightness = info.brightness
           level = (1-brightness/100.)*(info.maxVal - info.minVal) + info.minVal
           width = (1-contrast/100.)*(info.maxVal - info.minVal)

              ; Calculate display minimum and maximum.

           displayMax = (level + (width / 2))
           displayMin = (level - (width / 2))

           IF displayMax GT info.maxval THEN BEGIN
              difference = Abs(displayMax - info.maxval)
              displayMax = displayMax - difference
              displayMin = displayMin - difference
           ENDIF
           IF displayMin LT info.minval THEN BEGIN
              difference = Abs(info.minval - displayMin)
              displayMin = displayMin + difference
              displayMax = displayMax + difference
           ENDIF

              ; Display the image after positioning it appropriately
              ; in the window so that it maintains the aspect ratio of the
              ; sub-sampled image. Apply the positioning to the image and
              ; to the zoom box.

           info.zoomImage->SetProperty, Data=BytScl(*info.subimage, Min=info.minval > $
               displayMin, Max=displayMax < info.maxval), Dimensions=s
           imageAspect = Float(s[1]) / s[0]
           info.theWindow->GetProperty, Dimensions=dims
           windowAspect = (450./info.window_ysize * dims[1]) / (300./info.window_xsize * dims[0])
           pos = ContrastZoom_Aspect(imageAspect, WindowAspect=windowAspect, Margin=0)
           info.zoomImage->GetProperty, XRange=xrange, YRange=yrange
           xs = FSC_Normalize(xrange, Position=[pos(0), pos(2)])
           ys = FSC_Normalize(yrange, Position=[pos(1), pos(3)])
           info.zoomImage->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys
           info.theBox->SetProperty, Hide=1, XCoord_Conv=xs, YCoord_Conv=ys

              ; Clear any motion events that may have occurred.

           Widget_Control, event.id, /Clear_Events

              ; Set the static values to negative values.

           info.xs = -1
           info.ys = -1

           END ; of UP event

         'MOTION': BEGIN

               ; Get the dynamic corner of the box.

             info.xd = 0 > xpt < (info.zxsize)
             info.yd = 0 > ypt < (info.zysize)

               ; Re-configure the box coordinates.

            box = FltArr(2,5)
            box[0,*] = [info.xs, info.xd, info.xd, info.xs, info.xs]
            box[1,*] = [info.ys, info.ys, info.yd, info.yd, info.ys]

               ; Draw the new zoom box.

           info.theBox->SetProperty, Data=box

            END ; of MOTION event.

         ELSE:

     ENDCASE

  END ; of ZOOMWINDOW processing.

  'CONTRASTWINDOW': BEGIN ; You are trying to window and level the image.

        ; Make sure no zooming event sneak in here by accident.

     IF info.currentMode EQ 'ZOOMWINDOW' THEN BEGIN
        Widget_Control, event.top, Set_UValue=info, /No_Copy
        RETURN
     ENDIF


        ; Find the point in the coordinates of the image in the window.

     hit = info.theWindow->Pickdata(thisView, info.zoomImage, [event.x, event.y], xyz)
     xpt = Floor(xyz[0])
     ypt = Floor(xyz[1])

        ; What happens next depends on the type of event this is.

     possibleEventTypes = [ 'DOWN', 'UP', 'MOTION', 'SCROLL', 'EXPOSE' ]
     thisEvent = possibleEventTypes(event.type)

     CASE thisEvent OF

     'DOWN': BEGIN

           ; Set the initial (x,y) point. Turn motion events on.

        info.x1 = xpt
        info.y1 = ypt
        Widget_Control, info.drawID, Draw_Motion_Events=1
        info.currentMode = "CONTRASTWINDOW"
        END

      'UP': BEGIN

            ; Turn motion events off. Clear any motion events that might have queued.
            ; Reset the current mode.

         Widget_Control, info.drawID, Draw_Motion_Events=0
         Widget_Control, info.drawID, /Clear_Events
         info.currentMode = ""

            ; If the static point and the dynamic point are the same,
            ; reset the level and width.

         IF Abs(info.x1 - xpt) LT 2 AND Abs(info.y1-ypt) LT 2 THEN BEGIN
            contrast = 0
            brightness = 100
            level = (1-brightness/100.)*(info.maxVal - info.minVal) + info.minVal
            width = (1-contrast/100.)*(info.maxVal - info.minVal)

             ; Calculate display minimum and maximum.

            displayMax = (level + (width / 2))
            displayMin = (level - (width / 2))

            IF displayMax GT info.maxval THEN BEGIN
               difference = Abs(displayMax - info.maxval)
               displayMax = displayMax - difference
               displayMin = displayMin - difference
            ENDIF
            IF displayMin LT info.minval THEN BEGIN
               difference = Abs(info.minval - displayMin)
               displayMin = displayMin + difference
              displayMax = displayMax + difference
             ENDIF

             ; Display the image.

            info.contrastImage->SetProperty, Data=BytScl(*info.image, Min=info.minval > $
               displayMin, Max=displayMax < info.maxval)
            info.zoomImage->SetProperty, Data=BytScl(*info.subimage, Min=info.minval > $
               displayMin, Max=displayMax < info.maxval)
            info.colorbar->Clamp, [info.minval > displayMin, displayMax < info.maxval]
            info.colorbar2->SetProperty, Range=[info.minval > displayMin, displayMax < info.maxval]

               ; Update the current contrast and brightness values.

            info.contrast = contrast
            info.brightness = brightness
            info.theWindow->Draw, info.theScene
            Widget_Control, event.top, Set_UValue=info, /No_Copy
            RETURN

           ENDIF

               ; Calculate new contrast, brightness, level, and width parameters.

           contrast = 0 > ((info.y1 - ypt) * info.cstep + info.contrast) < 99
           brightness = 0 > ((info.x1 - xpt) * info.bstep + info.brightness) < 100
           level = (1-brightness/100.)*(info.maxVal - info.minVal) + info.minVal
           width = (1-contrast/100.)*(info.maxVal - info.minVal)

               ; Calculate display minimum and maximum.

           displayMax = (level + (width / 2))
           displayMin = (level - (width / 2))

           IF displayMax GT info.maxval THEN BEGIN
              difference = Abs(displayMax - info.maxval)
              displayMax = displayMax - difference
              displayMin = displayMin - difference
           ENDIF
           IF displayMin LT info.minval THEN BEGIN
              difference = Abs(info.minval - displayMin)
              displayMin = displayMin + difference
              displayMax = displayMax + difference
           ENDIF

              ; Display the image.

           info.contrastImage->SetProperty, Data=BytScl(*info.image, Min=info.minval > $
               displayMin, Max=displayMax < info.maxval)
           info.zoomImage->SetProperty, Data=BytScl(*info.subimage, Min=info.minval > $
               displayMin, Max=displayMax < info.maxval)
           info.colorbar->Clamp, [info.minval > displayMin, displayMax < info.maxval]
           info.colorbar2->SetProperty, Range=[info.minval > displayMin, displayMax < info.maxval]

              ; Update the current contrast and brightness values.

           info.contrast = contrast
           info.brightness = brightness
           END

         'MOTION': BEGIN

               ; Calculate new contrast, brightness, level, and width parameters.
               ; Restrict the width to 5 percent of the image range.

            contrast = 0 > ((info.y1 - ypt) * info.cstep + info.contrast) < 99
            brightness = 0 > ((info.x1 - xpt) * info.bstep + info.brightness) < 100
            level = (1-brightness/100.)*(info.maxVal - info.minVal) + info.minVal
            width = (1-contrast/100.)*(info.maxVal - info.minVal)

               ; Calculate display minimum and maximum.

            displayMax = (level + (width / 2))
            displayMin = (level - (width / 2))

            IF displayMax GT info.maxval THEN BEGIN
               difference = Abs(displayMax - info.maxval)
               displayMax = displayMax - difference
               displayMin = displayMin - difference
            ENDIF
            IF displayMin LT info.minval THEN BEGIN
               difference = Abs(info.minval - displayMin)
               displayMin = displayMin + difference
               displayMax = displayMax + difference
            ENDIF

               ; Display the image.

            info.contrastImage->SetProperty, Data=BytScl(*info.image, Min=info.minval > $
               displayMin, Max=displayMax < info.maxval)
            info.zoomImage->SetProperty, Data=BytScl(*info.subimage, Min=info.minval > $
               displayMin, Max=displayMax < info.maxval)
            info.colorbar->Clamp, [info.minval > displayMin, displayMax < info.maxval]
            info.colorbar2->SetProperty, Range=[info.minval > displayMin, displayMax < info.maxval]

            END

         ELSE:

         ENDCASE

      END ; of ContrastWindow processing.

   ELSE:


ENDCASE

   ; Draw the scene.

info.theWindow->Draw, info.theScene

   ; Store the info structure.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END;-------------------------------------------------------------------------------------



PRO ContrastZoom_Cleanup, tlb

   ; This is the clean-up procedure for the program.

Widget_Control, tlb, Get_UValue=info, /No_Copy
IF N_Elements(info) EQ 0 THEN RETURN

Obj_Destroy, info.theContainer
Ptr_Free, info.image
Ptr_Free, info.subimage

END;-------------------------------------------------------------------------------------


PRO ContrastZoom, image, Colortable=colortable, Group_Leader=group_leader

On_Error, 2

   ; Find an image, if needed.

IF N_Elements(image) EQ 0 THEN BEGIN
   filename = Filepath(Subdir=['examples','data'], 'rbcells.jpg')
   Read_JPEG, filename, image
   image = Reverse(image,2) ; Image is upside down.
ENDIF

   ; Only 2D images can be used.

IF Size(image, /N_Dimensions) NE 2 THEN Message, 'Image must be 2D. Returning...'

   ; Get a colortable if needed.

IF N_Elements(colortable) EQ 0 THEN colortable = 0 ELSE colortable = 0 > colortable < 41

   ; Create a color palette and load the colortable.

thePalette = Obj_New('IDLgrPalette')
thePalette->LoadCT, colortable

   ; Create a zoom box for zoom rubberbanding.

theBox = Obj_New('IDLgrPolyline', Hide=1, Color=[255,255,255])

   ; Create contrast and zoom image objects.

dims = Size(image, /Dimensions)
zxsize = dims[0]
zysize = dims[1]
zoomImage = Obj_New('IDLgrImage', image, Palette=thePalette, Dimensions=dims)
contrastImage = Obj_New('IDLgrImage', BytScl(image), Palette=thePalette, Dimensions=dims)

   ; Create a color bar.

colorbar = Obj_New('ContrastZoom_VColorBar', Palette=thePalette, Range=[Min(image), Max(image)], $
   Position=[0.7, 0.1, 0.95, 0.95], Title='Image Values')
colorbar2 = Obj_New('ContrastZoom_VColorBar', Palette=thePalette, Range=[Min(image), Max(image)], $
   Position=[0.7, 0.1, 0.95, 0.95], Title='Displayed Values', Major=8)

   ; Create the scene, views, and models for the object heirarchy. The dimensions
   ; and location are in normalized units, but device units are used to calculate
   ; the values. The numbers window_xsize and window_ysize refer to the X size and Y
   ; size of the initial draw widget that will be created later. User values will be
   ; used to identify which events are associated with which view.

window_xsize = 925
window_ysize = 500

theScene = Obj_New('IDLgrScene', Color=[125, 125, 125])
zoomView = Obj_New('IDLgrView', Color=[125, 125, 125], Viewplane_Rect=[0,0,1,1], $
   Location=[25./window_xsize, 25./window_ysize], Dimensions=[300./window_xsize, 450./window_ysize], Units=3, UValue='ZOOMWINDOW')
contrastView = Obj_New('IDLgrView', Color=[125, 125, 125], Viewplane_Rect=[0,0,1,1], $
   Location=[350./window_xsize, 25./window_ysize], Dimensions=[300./window_xsize, 450./window_ysize], Units=3, UValue='CONTRASTWINDOW')
colorbarView = Obj_New('IDLgrView', Color=[125, 125, 125], Viewplane_Rect=[-0.2,0,1.2,1], $
   Location=[675./window_xsize, 25./window_ysize], Dimensions=[75./window_xsize, 450./window_ysize], Units=3, UValue='COLORBARWINDOW')
colorbar2View = Obj_New('IDLgrView', Color=[125, 125, 125], Viewplane_Rect=[-0.2,0,1.2,1], $
   Location=[775./window_xsize, 25./window_ysize], Dimensions=[75./window_xsize, 450./window_ysize], Units=3, UValue='COLORBAR2WINDOW')

theScene->Add, zoomView
theScene->Add, contrastView
theScene->Add, colorbarView
theScene->Add, colorbar2View

zoomModel = Obj_New('IDLgrModel')
contrastModel = Obj_New('IDLgrModel')
colorbarModel = Obj_New('IDLgrModel')
colorbar2Model = Obj_New('IDLgrModel')

zoomView->Add, zoomModel
contrastView->Add, contrastModel
colorbarView->Add, colorbarModel
colorbar2View->Add, colorbar2Model

zoomModel->Add, zoomImage
zoomModel->Add, theBox
contrastModel->Add, contrastImage
colorbarModel->Add, colorbar
colorbar2Model->Add, colorbar2

   ; We need to scale the image into the view. Start by
   ; getting the image range. This is the same for both
   ; the zoom and contrast image.

zoomImage->GetProperty, XRange=xrange, YRange=yrange

    ; Calculate the aspect ratios (height/width) for the image
    ; and for the display window. Scale the images and the zoom box.

s = Size(image, /Dimensions)
imageAspect = Float(s[1]) / s[0]
windowAspect = Float(450) / 300
pos = ContrastZoom_Aspect(imageAspect, WindowAspect=windowAspect, Margin=0)
xs = FSC_Normalize(xrange, Position=[pos(0), pos(2)])
ys = FSC_Normalize(yrange, Position=[pos(1), pos(3)])
print, 'pos: ', pos
zoomImage->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys
contrastImage->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys
theBox->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys

   ; Set up initial parameters. Contrast and brightness values
   ; go from 0 to 100. Start with 25% contrast and 75% brightness.

maxval = Max(image, Min=minVal)
contrast = 25
brightness = 75

   ; Calculate window level and width from contrast/brightness values.

level = (1-brightness/100.)*(maxVal - minVal) + minVal
width = (1-contrast/100.)*(maxVal - minVal)

   ; Calculate display minimum and maximum.

displayMax = (level + (width / 2))
displayMin = (level - (width / 2))

IF displayMax GT maxval THEN BEGIN
   difference = Abs(displayMax - maxval)
   displayMax = displayMax - difference
   displayMin = displayMin - difference
ENDIF
IF displayMin LT minval THEN BEGIN
   difference = Abs(minval - displayMin)
   displayMin = displayMin + difference
   displayMax = displayMax + difference
ENDIF

   ; Update the contrast and zoom image after changes.

contrastImage->SetProperty, Data=BytScl(image, Min=displayMin, Max=displayMax)
zoomImage->SetProperty, Data=BytScl(image, Min=displayMin, Max=displayMax)
colorbar2->SetProperty, Range=[displayMin, displayMax]

   ; Create the widgets for the program. Set software rendering for the draw
   ; widget, or rubberbanding may be slow.

tlb = Widget_Base(Title='Contrast/Zoom Object Graphics Example', /Base_Align_Center, $
   Column=1, /TLB_Size_Events)
drawID = Widget_Draw(tlb, XSize=window_xsize, YSize=window_ysize, Button_Events=1, $
   Graphics_Level=2, Event_Pro='ContrastZoom_DistinguishEvents', Expose_Events=1, $
   Renderer=1)
label = Widget_Label(tlb, Value='Left image allows ZOOMING. Center image allows WINDOWING. ' + $
   'Click and drag. Click and release in window restores original view.')
Widget_Control, tlb, /Realize

   ; Get the window object and draw the scene.

Widget_Control, drawID, Get_Value=theWindow
theWindow->Draw, theScene

   ; Create a container object to aid in proper object clean-up.

theContainer = Obj_New('IDL_Container')
theContainer->Add, thePalette
theContainer->Add, zoomView
theContainer->Add, contrastView
theContainer->Add, colorbarView
theContainer->Add, colorbar2View
theContainer->Add, theScene

   ; Create the info structure.

info = { zoomImage:zoomImage, $            ; The zoom image object.
         contrastImage: contrastImage, $   ; The contrast image object.
         image:Ptr_New(image), $           ; A pointer to the original image.
         subimage: Ptr_New(image), $       ; A pointer to the current image subset.
         zoomView:zoomView, $              ; The zoom view object.
         contrastView:contrastView, $      ; The contrast view object.
         colorbar:colorbar, $              ; The color bar object.
         colorbar2:colorbar2, $            ; The second color bar.
         colorbarView:colorbarView, $      ; The color bar view.
         theContainer:theContainer, $      ; The container object.
         theWindow:theWindow, $            ; The window object.
         theScene:theScene, $              ; The scene to be displayed.
         drawID:drawID, $                  ; The draw widget identifier.
         thebox: thebox, $                 ; The zoom box object.
         currentMode: "", $                ; The current mode the window is operating in.
         x1:-1L, $                         ; Locations in the window for contrast/brightness operations.
         x2:-1L, $
         y1:-1L, $
         y2:-1L, $
         zxsize:zxsize, $                  ; The X size of the zoom image.
         zysize:zysize, $                  ; The Y size of the zoom image.
         contrast:contrast, $              ; The current contrast value.
         brightness:brightness, $          ; The current brightness value.
         bstep:zxsize/512., $              ; The amount of brighness change for one pixel movement.
         cstep:zysize/512., $              ; The amount of contrast change for one pixel movement.
         maxVal:maxVal, $
         minVal:minVal, $
         window_xsize:window_xsize, $      ; The original X size of the draw widget.
         window_ysize:window_ysize, $      ; The original Y size of the draw widget.
         xs:-1L, $                         ; Locations in the window for zooming operations.
         ys:-1L, $
         xd:zxsize-1, $
         yd:zysize-1 }

Widget_Control, tlb, Set_UValue=info, /No_Copy
XManager, 'contrastzoom', tlb, /No_Block, Cleanup='ContrastZoom_Cleanup', $
   Event_Handler='Contrastzoom_Resize', Group_Leader=group_leader

END
