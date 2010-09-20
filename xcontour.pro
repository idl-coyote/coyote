;+
; NAME:
;       XCONTOUR
;
; PURPOSE:
;       The purpose of this program is to demonstrate how to
;       create a contour plot with axes and a title in the
;       new IDL 5 object graphics.
;
; AUTHOR:
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;       Widgets, IDL 5 Object Graphics.
;
; CALLING SEQUENCE:
;       XCONTOUR, data, x, y
;
; REQUIRED INPUTS:
;       None. Fake data will be used if no data is supplied in call.
;
; OPTIONAL INPUTS
;
;       data: A 2D array of surface data.
;
;       x: A vector of X data values.
;
;       y: A vector of Y data values.
;
; OPTIONAL KEYWORD PARAMETERS:
;
;       CBARTITLE: A string used as the title of the colorbar.
;
;       _EXTRA: This keyword collects otherwise undefined keywords that are
;       passed to the old IDL contour command. Most of the keywords will
;       have absolutely no effect.
;
;       GROUP_LEADER: The group leader for this program. When the group leader
;       is destroyed, this program will be destroyed.
;
;       NLEVELS: The number of equally spaced contour intervals to draw.
;       Default is 10. Note that contour levels are acutally calculated,
;       since the NLEVELS keyword to the contour object does not always
;       result in the correct number of contour levels.
;
;       TITLE: A string used as the title of the plot.
;
;       XTITLE: A string used as the X title of the plot.
;
;       YTITLE: A string used as the Y title of the plot.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;
;       Requires FSC_NORMALIZE  from the Coyote library.
;
;          http://www.dfanning.com/programs/fsc_normalize.pro
;
; EXAMPLE:
;       To use this program with your 2D data, type:
;
;        IDL> XContour, data
;
; MODIFICATION HISTORY:
;       Written by David Fanning, 9 June 97.
;       Added a colorbar to the plot. 19 June 97, DWF.
;       Modified the way VCOLORBAR was called. 14 July 97. DWF.
;       Fixed cleanup procedure to clean up ALL objects. 12 Feb 98. DWF.
;       Changed IDLgrContainer to IDL_Container to fix 5.1 problems. 20 May 98. DWF.
;       Modified to use the IDLgrColorbar object. 20 Sept 98. DWF.
;       Added the ability to do a filled contour. 27 Sept 98. DWF.
;       Fixed a bug in the way the data was scaled into the view. 9 May 99. DWF.
;       Fixed a bug in the filled contours. 11 May 99. DWF.
;       Added a line to make sure 256 colors are available in Z buffer. 19 Dec 99. DWF.
;       Fixed a small bug where the X and Y vectors weren't sent to IDLgrContour. 19 Sept 2000. DWF.
;       Added CBARTITLE keyword and fixed a small memory leak. Updated VCOLORBAR code. 8 Dec 2000. DWF.
;       Fixed a minor problem with the Colorbar. Removed GIF support for IDL 5.4 and higher. 27 Mar 2001. DWF.
;       Removed NORMALIZE from the source code. 29 November 2005. DWF.
;       Renamed NORMALIZE to FSC_NORMALIZE to avoid numerous naming conflicts. 17 October 2008. DWF.
;       Made the draw widget render in software to eliminate a window read-through problem
;          (http://www.dfanning.com/ographics_tips/snapshot.html). 30 December 2008. DWF.
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
FUNCTION VColorBar::INIT, Position=position, $
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
    Font=thisFont, Recompute_Dimensions=2)

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



PRO VColorBar::Cleanup

    ; Lifecycle method to clean itself up.

Obj_Destroy, self.thisContainer
self->IDLgrMODEL::Cleanup
END
;-------------------------------------------------------------------------



PRO VColorBar::GetProperty, Position=position, Text=text, $
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



PRO VColorBar::SetProperty, Position=position, $
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



PRO VColorBar__Define

colorbar = { VCOLORBAR, $
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



PRO XContour_Cleanup, tlb

    ; Come here when program dies. Free all created objects.

Widget_Control, tlb, Get_UValue=info
IF N_Elements(info) NE 0 THEN Obj_Destroy, info.thisContainer
END
;-------------------------------------------------------------------



PRO XContour_Draw_Events, event

    ; Draw widget expose events are handled here.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Redraw the view.

info.thisWindow->Draw, info.thisView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO XContour_Properties, event

     ; Event handler to set graphic properties.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; What property is wanted?

Widget_Control, event.id, Get_UValue=newProperty
CASE newProperty OF

       ; Appearance of the contour plot.

   'FILLED': BEGIN
      info.thisContour->SetProperty, Fill=1
      END
   'UNFILLED': info.thisContour->SetProperty, Fill=0

       ; Background color.

   'BBLACK': info.thisView->SetProperty, Color=[0,0,0]
   'BWHITE': info.thisView->SetProperty, Color=[255,255,255]
   'BCHARCOAL': info.thisView->SetProperty, Color=[80,80,80]

       ; Axes colors.

   'ABLACK': BEGIN
      info.xAxis1->SetProperty, Color=[0,0,0]
      info.yAxis1->SetProperty, Color=[0,0,0]
      info.xAxis2->SetProperty, Color=[0,0,0]
      info.yAxis2->SetProperty, Color=[0,0,0]
      info.thisColorbar->SetProperty, Color=[0,0,0]
      END
   'AWHITE': BEGIN
      info.xAxis1->SetProperty,Color=[255,255,255]
      info.yAxis1->SetProperty,Color=[255,255,255]
      info.xAxis2->SetProperty,Color=[255,255,255]
      info.yAxis2->SetProperty,Color=[255,255,255]
      info.thisColorbar->SetProperty, Color=[255,255,255]
      END
   'AGREEN': BEGIN
      info.xAxis1->SetProperty,Color=[0,255,0]
      info.yAxis1->SetProperty,Color=[0,255,0]
      info.xAxis2->SetProperty,Color=[0,255,0]
      info.yAxis2->SetProperty,Color=[0,255,0]
      info.thisColorbar->SetProperty, Color=[0,255,0]
      END
   'AYELLOW': BEGIN
      info.xAxis1->SetProperty,Color=[255,255,0]
      info.yAxis1->SetProperty,Color=[255,255,0]
      info.xAxis2->SetProperty,Color=[255,255,0]
      info.yAxis2->SetProperty,Color=[255,255,0]
      info.thisColorbar->SetProperty, Color=[255,255,0]
      END

       ; Title colors.

   'TBLACK': info.plotTitle->SetProperty, Color=[0,0,0]
   'TWHITE': info.plotTitle->SetProperty, Color=[255,255,255]
   'TGREEN': info.plotTitle->SetProperty, Color=[0,255,0]
   'TYELLOW': info.plotTitle->SetProperty, Color=[255,255,0]

      ; Color schemes.

   'B/W': BEGIN
      info.thisView->SetProperty, Color=[255,255,255]
      info.xAxis1->SetProperty, Color=[0,0,0]
      info.yAxis1->SetProperty, Color=[0,0,0]
      info.xAxis2->SetProperty, Color=[0,0,0]
      info.yAxis2->SetProperty, Color=[0,0,0]
      info.plotTitle->SetProperty, Color=[0,0,0]
      info.thisColorbar->SetProperty, Color=[0,0,0]
      END
   'W/B': BEGIN
      info.thisView->SetProperty, Color=[0,0,0]
      info.xAxis1->SetProperty,Color=[255,255,255]
      info.yAxis1->SetProperty,Color=[255,255,255]
      info.xAxis2->SetProperty,Color=[255,255,255]
      info.yAxis2->SetProperty,Color=[255,255,255]
      info.plotTitle->SetProperty, Color=[255,255,255]
      info.thisColorbar->SetProperty, Color=[255,255,255]
      END
   'ORIGINAL_COLORS': BEGIN
      info.thisView->SetProperty, Color=[80,80,80]
      info.xAxis1->SetProperty,Color=[255,255,0]
      info.yAxis1->SetProperty,Color=[255,255,0]
      info.xAxis2->SetProperty,Color=[255,255,0]
      info.yAxis2->SetProperty,Color=[255,255,0]
      info.plotTitle->SetProperty, Color=[0,255,0]
      info.thisColorbar->SetProperty, Color=[255,255,0]
      END

ENDCASE

    ; Redraw the graphic.

info.thisWindow->Draw, info.thisView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO XContour_Output, event

   ; This event handler creates GIF and JPEG files.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get a snapshop of window contents. (TVRD equivalent.)

info.thisWindow->GetProperty, Image_Data=snapshot

   ; JPEG or GIF file wanted?

Widget_Control, event.id, GET_UValue=whichFileType
CASE whichFileType OF

   'GIF': BEGIN

         ; Because we are using RGB color for this model, we have
         ; a 3-m-n array. Use Color_Quan to create a 2D image and
         ; appropriate color tables for the GIF file.

      image2D = Color_Quan(snapshot, 1, r, g, b)
      filename = Dialog_Pickfile(/Write, File='idl.gif')
      IF filename NE '' THEN Write_GIF, filename, image2d, r, g, b
      END

   'JPEG': BEGIN

      filename = Dialog_Pickfile(/Write, File='idl.jpg')
      IF filename NE '' THEN Write_JPEG, filename, snapshot, True=1
      END

   'TIFF': BEGIN

      filename = Dialog_Pickfile(/Write, File='idl.tif')
      IF filename NE '' THEN Write_TIFF, filename, Reverse(snapshot,3)
      END
ENDCASE

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------


PRO XContour_Exit, event

   ; Exit the program.

Widget_Control, event.top, /Destroy
END
;-------------------------------------------------------------------



PRO XContour_Printing, event

   ; PostScript printing and printer setup handled here.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Which button?

Widget_Control, event.id, Get_UValue=ButtonValue
CASE buttonValue OF
   'PRINT': BEGIN
      result = Dialog_PrintJob(info.thisPrinter)
      IF result EQ 1 THEN BEGIN
         info.thisPrinter->Draw, info.thisView
         info.thisPrinter->NewDocument
      ENDIF
      END
   'SETUP': BEGIN
      result = Dialog_PrinterSetup(info.thisPrinter)
      IF result EQ 1 THEN BEGIN
         info.thisPrinter->Draw, info.thisView
         info.thisPrinter->NewDocument
      ENDIF
      END
ENDCASE

   ; Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO XContour_Resize, event

     ; The only events generated by this simple program are resize
     ; events, which are handled here.

     ; Get the info structure.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Resize the draw widget.

info.thisWindow->SetProperty, Dimension=[event.x, event.y]

    ; Redisplay the graphic.

info.thisWindow->Draw, info.thisView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO XContour, data, x, y, _Extra=extra, XTitle=xtitle, $
   YTitle=ytitle, NLevels=nlevels, Title=plotTitle, $
   Group_Leader=groupLeader, CBarTitle=cbartitle

    ; Check for keywords. Define default values

IF N_Elements(xtitle) EQ 0 THEN xtitle = 'X Axis'
IF N_Elements(ytitle) EQ 0 THEN ytitle = 'Y Axis'
IF N_Elements(nlevels) EQ 0 THEN nlevels = 10
IF N_Elements(plotTitle) EQ 0 THEN plotTitle = 'Pretty Good Contour Plot'
IF N_Elements(cbartitle) EQ 0 THEN cbartitle = 'Meters'

    ; Need some data.

Catch, error
IF error NE 0 THEN BEGIN  ; Can't find LoadData.
   data = Beselj(Shift(Dist(81), 20, 20)/2.0)
   x = Findgen(81)
   y = Findgen(81)
   IF !Error NE -154 THEN Print, !Err_String
ENDIF

IF N_Elements(data) EQ 0 THEN BEGIN
   data = LoadData(2)
ENDIF

s = Size(data)

IF s(0) NE 2 THEN Message,'Must pass 2D argument. Using fake data.'
IF N_Elements(x) EQ 0 THEN x = Findgen(s(1))
IF N_Elements(y) EQ 0 THEN y = Findgen(s(2))


CreateView:
Catch, /Cancel

    ; Create a view. Use RGB color. Charcoal background.
    ; The viewplane rectangle extends from 0 to 1 in XY directions.
    ; The chosen viewplane uses the old IDL "normalized" window. This
    ; is to make it easy to use the path information (xyData, which is
    ; returned in normalized coordinates) from the Contour command
    ; later. I won't have to worry about scaling.

thisView = OBJ_NEW('IDLgrView', Color=[80,80,80], Viewplane_Rect=[0,0,1,1])

    ; Create a model and add it to the view.

thisModel = OBJ_NEW('IDLgrModel')
thisView->Add, thisModel

    ; Colors are the only way to distinguish contour intervals.
    ; Here I load a color table and use colors from the table
    ; for the different contour lines. Colors are loaded in the
    ; Z buffer because I want to be sure I have 256 colors.

thisDevice = !D.Name
Set_Plot, 'Z'
Device, Set_Colors=256
LoadCT, 33, /Silent
TVLCT, red, green, blue, /Get
Set_Plot, thisDevice

    ; Calculate the contour levels. (NLEVELS doesn't actually divide
    ; the data into this number of equally space intervals.)

step = (Max(data) - Min(data)) / nlevels
levels = Indgen(nlevels) * step + Min(data)

    ; Calculate the contour colors.

colorStep = Fix(255/Float(nlevels))
cLevelColor = BIndGen(nlevels) * colorStep
contourColors = BIndGen(3, nlevels)
FOR j=0,nlevels-1 DO contourColors[*,j] = [red(cLevelColor[j]), $
   green(cLevelColor[j]), blue(cLevelColor[j])]

    ; Create a contour plot object. Have to add an extra level
    ; to the contour levels to get the top color to plot. Bug?

thisContour = Obj_New('IDLgrContour', data, x, y, GeomZ=-0.01, $
   Color=[255, 255, 0], /Planar, C_Color=contourColors, $
   C_Value=[levels, Max(data)])

    ; Scale the contour plot into the view.

thisContour->GetProperty, XRange=xrange, YRange=yrange
xs = FSC_Normalize(xrange, Position=[0.2,0.75])
ys = FSC_Normalize(yrange, Position=[0.2,0.85])
thiscontour->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys

    ; Add the contour plot to the model.

thisModel->Add, thisContour

    ; Create a vertical colorbar for the plot. This example uses
    ; the IDL-supplied colorbar, but I don't recommend it, as I don't
    ; think it is very well written. You might better use HCOLORBAR or
    ; VCOLORBAR, which you can download from the Coyote Library. Code
    ; for VCOLORBAR is included. You will have to remove the appropriate
    ; code to use it.


    ; Code for using VCOLORBAR.

 cbarPalette = Obj_New('IDLgrPalette', red, green, blue)
 thisColorbar = Obj_New('VColorbar', Range=[Min(data), Max(data)], $
   Position=[0.93, 0.2, 0.98, 0.85], Color=[255,255,0], $
   Palette=cbarPalette, Title=cbarTitle, Major=nlevels+1)

    ; Create the tick labels for the colorbar. Remove the next eight lines
    ; of code if using VCOLORBAR.

; Went back to using VCOLORBAR, due to an error in IDLgrColorbar introduced
; in IDL 5.4.


;cbarTicksValues = String([levels, Max(data)], Format='(I4)')
;cbarTicks = Obj_New('IDLgrText', cbarTicksValues, Color=[255,255,0], Font=cbarFont)

;cbarPalette = Obj_New('IDLgrPalette', contourColors[0,*],  contourColors[1,*],  contourColors[2,*])
;thisColorbar = Obj_New('IDLgrColorbar', Palette=cbarPalette, $
;   Dimensions=[40, 255], XCoord_Conv=FSC_Normalize([0,39], Position=[0.93,0.98]), $
;   YCoord_Conv=FSC_Normalize([0,255], Position=[0.2, 0.85]),$
;   Title=cbarTitle, Major=N_Elements(levels) + 1, Color=[255,255,0], TickLen=5, $
;   TickText=cbarTicks, /Show_Axis, /Show_Outline) ; End of RSI-supplied colorbar code.

thisModel->Add, thisColorbar

    ; Create titles for the axes and plot. Color them yellow.

xTitle = Obj_New('IDLgrText', xtitle, Color=[255,255,0])
yTitle = Obj_New('IDLgrText', ytitle, Color=[255,255,0])

    ; Create a plot title. Add it to model.

plotTitle = Obj_New('IDLgrText', plotTitle, Color=[0,255,0], $
   Alignment=0.5, Location=[0.475, 0.9, 0.0])
thisModel->Add, plotTitle

    ; Create box axes for the contour plot. Color them yellow.
    ; Notice the large values in the Location keyword. These
    ; are values that are not used.

xrange = [Min(x), Max(x)]
yrange = [Min(y), Max(y)]
zrange = [Min(data), Max(data)]
xs = FSC_Normalize(xrange, Position=[0.2,0.75])
ys = FSC_Normalize(yrange, Position=[0.2,0.85])

xAxis1 = Obj_New("IDLgrAxis", 0, Color=[255,255,0], Ticklen=0.05, $
   Minor=4, Title=xtitle, Range=xrange, XCoord_Conv=xs, $
   Location=[1000, 0.2, 0.0], /Exact)
 xAxis2 = Obj_New("IDLgrAxis", 0, Color=[255,255,0], Ticklen=0.05, $
   Minor=4, /NoText, Range=xrange, XCoord_Conv=xs, $
   Location=[1000, 0.85, 0.0], TickDir=1, /Exact)

yAxis1 = Obj_New("IDLgrAxis", 1, Color=[255,255,0], Ticklen=0.05, $
   Minor=4, Title=ytitle, Range=yrange, YCoord_Conv=ys, $
   Location=[0.2, 1000, 0.0], /Exact)
yAxis2 = Obj_New("IDLgrAxis", 1, Color=[255,255,0], Ticklen=0.05, $
   Minor=4, /NoText, Range=yrange, YCoord_Conv=ys, $
   Location=[0.75, 1000, 0.0], TickDir=1, /Exact)

    ; Add the axes to the model.

thisModel->Add, xAxis1
thisModel->Add, xAxis2
thisModel->Add, yAxis1
thisModel->Add, yAxis2

   ; Check for availability of GIF files.

thisVersion = Float(!Version.Release)
IF thisVersion LT 5.4 THEN haveGif = 1 ELSE haveGIF = 0

    ; Create the widgets to view the contour. Set expose events
    ; on the draw widget so that it refreshes itself whenever necessary.

tlb = Widget_Base(Title='Resizeable Window Contour Example', Column=1, $
   TLB_Size_Events=1, MBar=menubase)
drawID = Widget_Draw(tlb, XSize=400, YSize=400, Graphics_Level=2, $
   Expose_Events=1, Event_Pro='XContour_Draw_Events', Renderer=1)

    ; Create FILE menu buttons for printing and exiting.

filer = Widget_Button(menubase, Value='File', /Menu)
pnt = Widget_Button(filer, Value='Print', $
   Event_Pro='XContour_Printing', UValue='PRINT')
pntset = Widget_Button(filer, Value='Print Setup', $
   Event_Pro='XContour_Printing', UValue='SETUP')
quitter = Widget_Button(filer, /Separator, Value='Exit', $
   Event_Pro='XContour_Exit')

   ; Create PROPERTIES menu buttons for surface properties.

properties = Widget_Button(menubase, Value='Properties', /Menu)

   ; Appearance

appear = Widget_Button(properties, Value='Appearance', /Menu)
dummy = Widget_Button(appear, Value='Filled Contours', $
   Event_Pro='XContour_Properties', UValue='FILLED')
dummy = Widget_Button(appear, Value='Contour Lines', $
   Event_Pro='XContour_Properties', UValue='UNFILLED')

   ; Background Color

bcolor = Widget_Button(properties, Value='Background Color', /Menu)
dummy = Widget_Button(bcolor, Value='Black', $
   Event_Pro='XContour_Properties', UValue='BBLACK')
dummy = Widget_Button(bcolor, Value='White', $
   Event_Pro='XContour_Properties', UValue='BWHITE')
dummy = Widget_Button(bcolor, Value='Charcoal', $
   Event_Pro='XContour_Properties', UValue='BCHARCOAL')

   ; Axes Color

acolor = Widget_Button(properties, Value='Axes Color', /Menu)
dummy = Widget_Button(acolor, Value='Black', $
   Event_Pro='XContour_Properties', UValue='ABLACK')
dummy = Widget_Button(acolor, Value='White', $
   Event_Pro='XContour_Properties', UValue='AWHITE')
dummy = Widget_Button(acolor, Value='Yellow', $
   Event_Pro='XContour_Properties', UValue='AYELLOW')
dummy = Widget_Button(acolor, Value='Green', $
   Event_Pro='XContour_Properties', UValue='AGREEN')

   ; Title Color

tcolor = Widget_Button(properties, Value='Title Color', /Menu)
dummy = Widget_Button(tcolor, Value='Black', $
   Event_Pro='XContour_Properties', UValue='TBLACK')
dummy = Widget_Button(tcolor, Value='White', $
   Event_Pro='XContour_Properties', UValue='TWHITE')
dummy = Widget_Button(tcolor, Value='Yellow', $
   Event_Pro='XContour_Properties', UValue='TYELLOW')
dummy = Widget_Button(tcolor, Value='Green', $
   Event_Pro='XContour_Properties', UValue='TGREEN')

   ; Color Schemes.

scolor = Widget_Button(properties, Value='Color Schemes', /Menu)
dummy = Widget_Button(scolor, Value='Black on White', $
   Event_Pro='XContour_Properties', UValue='B/W')
dummy = Widget_Button(scolor, Value='White on Black', $
   Event_Pro='XContour_Properties', UValue='W/B')
dummy = Widget_Button(scolor, Value='Original Colors', $
   Event_Pro='XContour_Properties', UValue='ORIGINAL_COLORS')

   ; Create OUTPUT menu buttons for formatted output files.

output = Widget_Button(menubase, Value='Output')
IF havegif THEN gif = Widget_Button(output, Value='GIF File', $
   UValue='GIF', Event_Pro='XContour_Output')
jpeg = Widget_Button(output, Value='JPEG File', $
   UValue='JPEG', Event_Pro='XContour_Output')
tiff = Widget_Button(output, Value='TIFF File', $
   UValue='TIFF', Event_Pro='XContour_Output')

Widget_Control, tlb, /Realize

    ; Get the window destination object.

Widget_Control, drawID, Get_Value=thisWindow

    ; Draw the view in the window.

thisWindow->Draw, thisView

   ; Get a printer object for this graphic.

thisPrinter = Obj_New('IDLgrPrinter')

   ; Create a container object to hold all the other
   ; objects. This will make it easy to free all the
   ; objects when we are finished with the program.

thisContainer = Obj_New('IDL_Container')

   ; Add created objects to the container.

thisContainer->Add, thisView
thisContainer->Add, thisPrinter
thisContainer->Add, xAxis1
thisContainer->Add, xAxis2
thisContainer->Add, yAxis1
thisContainer->Add, yAxis2
thisContainer->Add, plotTitle
thisContainer->Add, thisContour
thisContainer->Add, xTitle
thisContainer->Add, yTitle
thisContainer->Add, thisColorbar


   ; If you use VCOLORBAR, comment out the next three lines
   ; and uncomment the fourth.

;thisContainer->Add, cbarFont
;thisContainer->Add, cbarTitle
;thisContainer->Add, cbarTicks
 thisContainer->Add, cbarPalette

   ; Create an INFO structure to hold needed program information.

info = { thisContainer:thisContainer, $    ; The object container.
         thisWindow:thisWindow, $          ; The window object.
         thisPrinter:thisPrinter, $        ; The printer object.
         thisModel:thisModel, $            ; The model object.
         thisContour:thisContour, $        ; The contour object.
         thisColorbar:thisColorbar, $      ; The colorbar object.
         xAxis1:xAxis1, $                  ; The X Axis object.
         yAxis1:yAxis1, $                  ; The Y Axis object.
         xAxis2:xAxis2, $                  ; The X Axis object.
         yAxis2:yAxis2, $                  ; The Y Axis object.
         plotTitle:plotTitle, $            ; The plot title object.
         thisView:thisView }               ; The view object.

   ; Store the info structure in the UValue of the TLB.

Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Call XManager. Set a cleanup routine so the objects
   ; can be freed upon exit from this program.

XManager, 'XContour', tlb, Cleanup='XContour_Cleanup', /No_Block, $
   Event_Handler='XContour_Resize', Group_Leader=groupLeader
END
;-------------------------------------------------------------------