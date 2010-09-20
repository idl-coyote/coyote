;+
; NAME:
;       XIMAGE
;
; PURPOSE:
;
;       The purpose of this program is to demonstrate how to
;       create a image plot with axes,a title, and the ability
;       to select a location and image value using object graphics.
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
;
;       Widgets, Object Graphics.
;
; CALLING SEQUENCE:
;
;       XImage, image
;
; REQUIRED INPUTS:
;
;       None. The image "worldelv.dat" from the examples/data directory
;       is used if no data is supplied in call.
;
; OPTIONAL INPUTS
;
;       image: An 8-bit or 24-bit image.
;
; OPTIONAL KEYWORD PARAMETERS:
;
;       COLORTABLE: The number of a color table to use as the image palette.
;       Color table 0 (grayscale) is used as a default. (Keyword ignored if
;       a 24-bit image is used.
;
;       GROUP_LEADER: The group leader for this program. When the group leader
;       is destroyed, this program will be destroyed.
;
;       KEEP_ASPECT_RATIO: Set this keyword if you wish the aspect ratio
;       of the image to be preserved as the graphics display window is resized.
;
;       TITLE: A string used as the title of the plot.
;
;       XRANGE: A two-element array specifying the X axis range.
;
;       XSIZE: The initial X window size. Default is 400 pixels.
;
;       XTITLE: A string used as the X title of the plot.
;
;       YRANGE: A two-element array specifying the Y axis range.
;
;       YSIZE: The initial Y window size. Default is 400 pixels.
;
;       YTITLE: A string used as the Y title of the plot.
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
;       Requires the XColors and Normalize programs from the Coyote library.
;
;          http://www.dfanning.com/programs/xcolors.pro
;          http://www.dfanning.com/programs/normalize.pro
;
; EXAMPLE:
;
;       To use this program with your 8-bit or 24-bit image data, type:
;
;        IDL> XImage, image
;
; MODIFICATION HISTORY:
;
;       Written by David Fanning, 13 June 97.
;       Added Keep_Apect_Ratio keyword and Zoom buttons. DWF 15 JUNE 97.
;       Improved font handling and color support. DWF 4 OCT 97.
;       Fixed memory leakage from improper object cleanup. 12 FEB 98. DWF
;       Changed IDLgrContainer to IDL_Container to fix 5.1 problems. 20 May 98. DWF.
;       Modified program to show how image values can be selected. 8 May 2000. DWF.
;       Made several modifications to resize event handler. Much improved. 2 June 2000. DWF.
;       Completely updated program to reflect current (IDL 5.5) capabilities. 12 Nov 2001. DWF.
;       Removed NORMALIZE from the source code. 29 November 2005. DWF.
;       Renamed NORMALIZE to FSC_NORMALIZE to avoid numerous naming conflicts. 17 October 2008. DWF.
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
FUNCTION XImage_Aspect, aspectRatio, MARGIN=margin, WindowAspect=wAspectRatio

; This function calculates the correct aspect ratios for printing.

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



PRO XImage_Zoom_Button_Event, event

     ; Event handler to perform window zooming.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; What kind of zooming is wanted?

Widget_Control, event.id, Get_UValue=zoomIt
CASE zoomIt OF

    'ZOOM_IN': BEGIN
        info.plotView->GetProperty, Viewplane_Rect=thisRect
        thisRect(0) = (thisRect(0) + 0.05) < thisRect(2)
        thisRect(1) = (thisRect(1) + 0.05) < thisRect(3)
        thisRect(2) = (thisRect(2) - 0.1) > thisRect(0)
        thisRect(3) = (thisRect(3) - 0.1) > thisRect(1)
        info.plotView->SetProperty, Viewplane_Rect=thisRect
        END

    'ZOOM_OUT': BEGIN
        info.plotView->GetProperty, Viewplane_Rect=thisRect
        thisRect(0) = thisRect(0) - 0.05
        thisRect(1) = thisRect(1) - 0.05
        thisRect(2) = thisRect(2) + 0.1
        thisRect(3) = thisRect(3) + 0.1
        info.plotView->SetProperty, Viewplane_Rect=thisRect
        END

ENDCASE

    ; Redisplay the view.

info.thisWindow->Draw, info.plotView

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO XImage_Processing, event

     ; Event handler to perform image processing options.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; What processing is wanted?

Widget_Control, event.id, Get_UValue=thisOperation
CASE thisOperation OF

    'SOBEL': data=Sobel(*info.processPtr)
    'ROBERTS': data=Roberts(*info.processPtr)
    'BOXCAR': data=Smooth(*info.processPtr,5)
    'MEDIAN': data=Median(*info.processPtr,5)
    'ORIGINAL': data=*info.imagePtr

ENDCASE

   ; Update image and processed image data.

info.thisImage->SetProperty, Data=BytScl(data)
*info.processPtr = data

    ; Redisplay the view.

info.thisWindow->Draw, info.plotView

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO XImage_Output, event

   ; This event handler creates output files.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get a snapshop of window contents. (TVRD equivalent.)

Wait, 0.5 ; To allow menu to disappear.
info.thisWindow->GetProperty, Image_Data=snapshot

   ; What kind of file is wanted?

Widget_Control, event.id, GET_UValue=whichFileType
CASE whichFileType OF

   'GIF': BEGIN

         ; Because we are using RGB color for this model, we have
         ; a 3-m-n array. Use Color_Quan to create a 2D image and
         ; appropriate color tables for the GIF file.

      image2D = Color_Quan(snapshot, 1, r, g, b)
      filename = Dialog_Pickfile(/Write, File='ximage.gif')
      IF filename NE '' THEN Write_GIF, filename, image2d, r, g, b
      END

   'BMP': BEGIN

         ; Because we are using RGB color for this model, we have
         ; a 3-m-n array. Use Color_Quan to create a 2D image and
         ; appropriate color tables for the GIF file.

      image2D = Color_Quan(snapshot, 1, r, g, b)
      filename = Dialog_Pickfile(/Write, File='ximage.bmp')
      IF filename NE '' THEN Write_BMP, filename, image2d, r, g, b
      END

   'PNG': BEGIN

         ; Because we are using RGB color for this model, we have
         ; a 3-m-n array. Use Color_Quan to create a 2D image and
         ; appropriate color tables for the GIF file.

      image2D = Color_Quan(snapshot, 1, r, g, b)
      filename = Dialog_Pickfile(/Write, File='ximage.png')
      IF filename NE '' THEN Write_PNG, filename, image2d, r, g, b
      END

   'PICT': BEGIN

         ; Because we are using RGB color for this model, we have
         ; a 3-m-n array. Use Color_Quan to create a 2D image and
         ; appropriate color tables for the GIF file.

      image2D = Color_Quan(snapshot, 1, r, g, b)
      filename = Dialog_Pickfile(/Write, File='ximage.pict')
      IF filename NE '' THEN Write_PICT, filename, image2d, r, g, b
      END

   'JPEG': BEGIN

      filename = Dialog_Pickfile(/Write, File='ximage.jpg')
      IF filename NE '' THEN Write_JPEG, filename, snapshot, True=1
      END


   'TIFF': BEGIN

      filename = Dialog_Pickfile(/Write, File='ximage.tif')
      IF filename NE '' THEN BEGIN

         ; TIFF files should have their Y direction reversed for
         ; compatibility with most other software.

         Write_TIFF, filename, Reverse(snapshot,3)
      ENDIF
      END

ENDCASE

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO XImage_Exit, event

   ; Exit the program via the EXIT button.
   ; The XIMAGE_CLEANUP procedure will be called automatically.

Widget_Control, event.top, /Destroy
END
;-------------------------------------------------------------------



PRO XImage_Printing, event

   ; PostScript printing and printer setup handled here.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Which button?

Widget_Control, event.id, Get_UValue=ButtonValue
CASE buttonValue OF

   'PRINT': BEGIN
      result = Dialog_PrintJob(info.thisPrinter)
      IF result EQ 1 THEN BEGIN

            ; I want the output on the page to have the same aspect ratio
            ; (ratio of height to width) as I see in the display window.

         info.thisWindow->GetProperty, Dimensions=wdims
         info.thisPrinter->GetProperty, Dimensions=pdims
         plotAspect = Float(wdims[1]) / wdims[0]
         windowAspect = Float(pdims[1]) / pdims[0]
         position = XImage_Aspect(plotAspect, WindowAspect=windowAspect, Margin=0.075)
         info.plotView->SetProperty, Dimensions=[position[2]-position[0], position[3]-position[1]], $
            Location=[position[0], position[1]], Units=3

            ; Print it. May take a little time. Alert the user.

         Widget_Control, Hourglass=1
         info.thisPrinter->Draw, info.plotView
         info.thisPrinter->NewDocument
         Widget_Control, Hourglass=0

            ; Put things back the way you found them.

         info.plotView->SetProperty, Location=[0,0], Dimensions=[0,0]

      ENDIF
      END

   'SETUP': BEGIN
      result = Dialog_PrinterSetup(info.thisPrinter)
      IF result EQ 1 THEN BEGIN

            ; I want the output on the page to have the same aspect ratio
            ; (ratio of height to width) as I see in the display window.

         info.thisWindow->GetProperty, Dimensions=wdims
         info.thisPrinter->GetProperty, Dimensions=pdims
         plotAspect = Float(wdims[1]) / wdims[0]
         windowAspect = Float(pdims[1]) / pdims[0]
         position = XImage_Aspect(plotAspect, WindowAspect=windowAspect, Margin=0.075)
         info.plotView->SetProperty, Dimensions=[position[2]-position[0], position[3]-position[1]], $
            Location=[position[0], position[1]], Units=3

            ; Print it. May take a little time. Alert the user.

         Widget_Control, Hourglass=1
         info.thisPrinter->Draw, info.plotView
         info.thisPrinter->NewDocument
         Widget_Control, Hourglass=0

            ; Put things back the way you found them.

         info.plotView->SetProperty, Location=[0,0], Dimensions=[0,0]

      ENDIF

      END

ENDCASE

   ; Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO XImage_Image_Colors, event

; This event handler changes color tables.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; What kind of event is this?

thisEvent = Tag_Names(event, /Structure_Name)
CASE thisEvent OF

   "WIDGET_BUTTON": BEGIN
      TVLCT, info.r, info.g, info.b
      XColors, Group_Leader=event.top, NotifyID=[event.id, event.top]
      ENDCASE

   "XCOLORS_LOAD": BEGIN
      info.r = event.r
      info.g = event.g
      info.b = event.b
      IF Obj_Valid(info.thisPalette) THEN info.thisPalette->SetProperty, $
         Red=event.r, Green=event.g, Blue=event.b
      ENDCASE

ENDCASE

   ; Draw the graphic display.

info.thisWindow->Draw, info.plotView
Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------------------------



PRO XIMAGE_CLEANUP, id

    ; Come here when the widget dies. Free all the program
    ; objects, pointers, pixmaps, etc. and release memory.

Widget_Control, id, Get_UValue=info
IF N_Elements(info) NE 0 THEN BEGIN
   Obj_Destroy, info.thisContainer
   Ptr_Free, info.imagePtr
   Ptr_Free, info.processPtr
ENDIF
END
;---------------------------------------------------------------------



PRO XImage_DrawWidget_Event, event

    ; This event handler handles draw widget events, including
    ; image value selection events.

Widget_Control, event.top, Get_UValue=info, /No_Copy

possibleEvents = ['DOWN', 'UP', 'MOTION', 'SCROLL', 'EXPOSE']
thisEvent = possibleEvents[event.type]
CASE thisEvent OF
   'DOWN': Widget_Control, event.id, Draw_Motion_Events=1
   'UP': Widget_Control, event.id, Draw_Motion_Events=0
   'EXPOSE': info.thisWindow->Draw, info.plotView
   ELSE:
ENDCASE

IF thisEvent NE 'EXPOSE' THEN BEGIN

   ; Calculate the image value at the current location. If 24-bit image, skip writing the value,
   ; since this make no sense for a 24-bit image.

   hit = info.thisWindow->Pickdata(info.plotView, info.thisImage, [event.x, event.y], xyz)
   xpt = Floor(xyz[0])
   ypt = Floor(xyz[1])
   IF xpt LT 0 OR xpt GT (info.xsize-1) OR ypt LT 0 OR ypt GT (info.ysize-1) THEN BEGIN
      Widget_Control, info.xvalueID, Set_Value=-999
      Widget_Control, info.yvalueID, Set_Value=-999
      IF info.truecolor EQ 0 THEN $
         Widget_Control, info.valueID, Set_Value='-9999.0'
   ENDIF ELSE BEGIN
      Widget_Control, info.xvalueID, Set_Value=xpt
      Widget_Control, info.yvalueID, Set_Value=ypt
      IF info.truecolor EQ 0 THEN $
         Widget_Control, info.valueID, Set_Value=Float((*info.processPtr)[xpt, ypt])
   ENDELSE

ENDIF

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO XImage_Resize, event

    ; This is main event handler for the TLB. It handles resize events.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Resize the draw widget.

info.thisWindow->SetProperty, Dimension=[event.x > 400, event.y-info.basesize]

    ; Keep the aspect ratio of the graphic?

IF info.keepAspect THEN BEGIN

        ; Get the new viewplane rectangle.

    waspect = (event.y-info.basesize) / Float(event.x > 400)
    IF waspect GE 1.0 THEN BEGIN
       ylen = 1.0 * waspect
       xlen = 1.0
       ystart = (-ylen/2.0) + 0.5 ; Center it.
       xstart = 0.0
    ENDIF ELSE BEGIN
       xlen = 1.0 / waspect
       ylen = 1.0
       xstart = (-xlen/2.0) + 0.5 ; Center it.
       ystart = 0.0
    ENDELSE

        ; Reset the viewplane rectangle.

    info.plotView->SetProperty,Viewplane_Rect=[xstart, ystart, xlen, ylen]

ENDIF

   ; Redisplay the graphic.

info.thisWindow->Draw, info.plotView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;---------------------------------------------------------------------



PRO XIMAGE, image, XRange=xrange, YRange=yrange, $
    Keep_Aspect=keepAspect, XSize=wxsize, YSize=wysize, $
    Colortable=colortable, XTitle=xtitle, YTitle=ytitle, $
    Group_Leader=group, Title=plottitle, TrueColor=trueColor

    ; Check for parameters. If no parameters, load world elevation data.

IF N_Params() EQ 0 THEN BEGIN
   filename = Filepath(SubDir=['examples', 'data'], 'worldelv.dat')
   OpenR, lun, filename, /Get_LUN
   image = BytArr(360,360)
   yrange = [-90, 90]
   xrange = [0, 360]
   plottitle='World Elevation'
   xtitle = 'Longitude'
   ytitle='Latitude'
   colortable = 33
   ReadU, lun, image
   Free_Lun, lun
ENDIF

   ; Get dimensions of image data. Set up INTERLEAVE variable.

s = SIZE(image)
IF s(0) LT 2 THEN Message, 'Must pass a 2D or 3D image data set.'
IF s(0) EQ 2 THEN BEGIN
   xsize = s(1)
   ysize = s(2)
   interleave = 0
   IF N_Elements(trueColor) EQ 0 THEN trueColor = 0
ENDIF
IF s(0) EQ 3 THEN BEGIN
   sizes = [s(1), s(2), s(3)]
   interleave = WHERE(sizes EQ 3)
   interleave = interleave(0)
   IF N_Elements(trueColor) EQ 0 THEN trueColor = 1
   IF interleave LT 0 THEN $
      Message, 'Image does not appear to be a 24-bit image. Returning...'
   CASE interleave OF
      0: BEGIN
         xsize = s(2)
         ysize = s(3)
         END
      1: BEGIN
         xsize = s(1)
         ysize = s(3)
         END
      2: BEGIN
         xsize = s(1)
         ysize = s(2)
         END
   ENDCASE
ENDIF

   ; Check for keyword parameters. Define default values if necessary.

IF N_Elements(xrange) EQ 0 THEN xrange = [0,xsize]
IF N_Elements(yrange) EQ 0 THEN yrange = [0,ysize]
IF N_Elements(wxsize) EQ 0 THEN wxsize = 400
IF N_Elements(wysize) EQ 0 THEN wysize = 400
IF N_Elements(xtitle) EQ 0 THEN xtitle = ''
IF N_Elements(ytitle) EQ 0 THEN ytitle = ''
IF N_Elements(plotTitle) EQ 0 THEN plotTitle=''
IF N_Elements(colortable) EQ 0 THEN colortable = 0
keepAspect = Keyword_Set(keepAspect)

    ; Calculate the aspect ratios (height/width) for the image
    ; and for the display window.

imageAspect = Float(ysize) / xsize
windowAspect = Float(wysize) / wxsize

   ; If this is an 8-bit image, we will need a color palette.

IF truecolor EQ 0 THEN BEGIN
   thisPalette = Obj_New('IDLgrPalette')
   thisPalette->LoadCT, colortable
   thisPalette->GetProperty, Red=r, Blue=b, Green=g
ENDIF ELSE BEGIN
   thisPalette = Obj_New()
   TVLCT, r, g, b, /Get
ENDELSE

   ; Define other drawing colors.

antiqueWhite = [250, 235, 215]
navyBlue = [0, 0, 128]
firebrickRed = [178, 34, 34]

    ; Create the image object.

imagePtr = Ptr_New(image)
processPtr = Ptr_New(image)
thisImage = Obj_New('IDLgrImage', BytScl(*imagePtr), $
   Dimensions=[xsize,ysize], Interleave=interleave, $
   Palette=thisPalette)

    ; Create scaling parameters for the image. I get
    ; position coordinates for a normalized window from
    ; my XImage_Aspect function. Then use my FSC_Normalize
    ; function to create scaling factors for the image.

pos = XImage_Aspect(imageAspect, WindowAspect=windowAspect, Margin=0.125)
xs = FSC_Normalize([0,xsize], Position=[pos(0), pos(2)])
ys = FSC_Normalize([0,ysize], Position=[pos(1), pos(3)])
thisImage->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys

    ; Note that XCoord_Conv and YCoord_Conv are broken in IDL 5.0
    ; for the image object, I must put the image in its own model
    ; and scale the model appropriately if I am running there.
    ; The code looks like this:

;imageModel = Obj_New('IDLgrModel')
;imageModel->Scale, (pos(2)-pos(0))/xsize, (pos(3)-pos(1))/ysize, 1
;imageModel->Translate, pos(0), pos(1), 1
;imageModel->Add, thisImage

    ; Set up font objects for the axes titles.

helvetica10pt = Obj_New('IDLgrFont', 'Helvetica', Size=10)
helvetica8pt = Obj_New('IDLgrFont', 'Helvetica', Size=8)

    ; Create title objects for the axes. Color them yellow.

xTitle = Obj_New('IDLgrText', xtitle, Recompute_Dimensions=1, Font=helvetica8pt)
yTitle = Obj_New('IDLgrText', ytitle, Recompute_Dimensions=1, Font=helvetica8pt)

    ; Create a plot title object. I want the title centered just
    ; above the upper X Axis.

plotTitle = Obj_New('IDLgrText', plotTitle, Color=NavyBlue, $
   Location=[0.5, pos(3)+0.05, 0.0], Alignment=0.5, Font=helvetica10pt, $
   Recompute_Dimensions=1)

    ; Set up scaling for the axes. Notice that we use the axis
    ; range here, not the size of the image, as before.

pos = XImage_Aspect(imageAspect, WindowAspect=windowAspect, Margin=0.125)
xs = FSC_Normalize(xrange, Position=[pos(0), pos(2)])
ys = FSC_Normalize(yrange, Position=[pos(1), pos(3)])

    ; Create the four axis objects (box axes). Make the titles
    ; with helvetica 10 point fonts.

xAxis1 = Obj_New("IDLgrAxis", 0, Color=firebrickRed, Ticklen=0.025, $
    Minor=4, Range=xrange, /Exact, Title=xTitle, XCoord_Conv=xs,  $
    Location=[1000, pos(1), 0.125])
xAxis1->GetProperty, Ticktext=xAxisText
xAxisText->SetProperty, Font=helvetica8pt, Recompute_Dimensions=2

xAxis2 = Obj_New("IDLgrAxis", 0, Color=firebrickRed, Ticklen=0.025, $
    Minor=4, /NoText, Range=xrange, TickDir=1, /Exact, XCoord_Conv=xs, $
    Location=[1000, pos(3), 0.125])

yAxis1 = Obj_New("IDLgrAxis", 1, Color=firebrickRed, Ticklen=0.025, $
    Minor=4, Title=ytitle, Range=yrange, /Exact, YCoord_conv=ys, $
    Location=[pos(0) ,1000, 0.125])
yAxis1->GetProperty, Ticktext=yAxisText
yAxisText->SetProperty, Font=helvetica8pt, Recompute_Dimensions=2

yAxis2 = Obj_New("IDLgrAxis", 1, Color=firebrickRed, Ticklen=0.025, $
    Minor=4, /NoText, Range=yrange, TickDir=1, /Exact, YCoord_conv=ys, $
    Location=[pos(2), 1000, 0.125])

    ; Create a plot model and add axes, image, title to it.

plotModel = Obj_New('IDLgrModel', Select=1)
plotModel->Add, thisImage
plotModel->Add, xAxis1
plotModel->Add, xAxis2
plotModel->Add, yAxis1
plotModel->Add, yAxis2
plotModel->Add, plotTitle

    ; Create a plot view. Add the model to the view. Notice
    ; the view is created to give space around the region
    ; where the "action" in the plot takes place. The extra
    ; space has to be large enough to accomodate axis annotation.

viewRect = [0.0, 0.0, 1.0, 1.0]
plotView = Obj_New('IDLgrView', Viewplane_Rect=viewRect, $
   Location=[0,0], Color=antiqueWhite)
plotView->Add, plotModel

    ; Create the widgets for this program.

tlb = Widget_Base(Title='Resizeable Image Example', $
   MBar=menubase, TLB_Size_Events=1, Column=1, Base_Align_Center=1)

   ; Create the draw widget. RETAIN=0 is necessary to generate
   ; EXPOSE events.

drawID = Widget_Draw(tlb, XSize=wxsize, YSize=wysize, $
   Graphics_Level=2, Expose_Events=1, Retain=0, $
   Event_Pro='XImage_DrawWidget_Event', Button_Events=1)

   ; Create image value and location widgets.

bottomBase = Widget_Base(tlb, Colum=1, /Base_Align_Center)
labelBase = Widget_Base(bottomBase, Row=1)
dummy = Widget_Label(labelBase, Value='Click (and drag) inside image for Image Value')
valueBase = Widget_Base(bottomBase, Row=1)
xvalueID = CW_Field(valueBase, Title='X Location:', Value=-999, /Integer, XSize=6)
yvalueID = CW_Field(valueBase, Title='  Y Location:', Value=-999, /Integer, XSize=6)
IF trueColor THEN BEGIN
   valueID =  CW_Field(valueBase, Title='  Image Value:', Value='Not Available', XSize=12)
ENDIF ELSE BEGIN
   valueID =  CW_Field(valueBase, Title='  Image Value:', Value='-9999.0', XSize=12, /Float)
ENDELSE

    ; Create FILE menu buttons for printing and exiting.

filer = Widget_Button(menubase, Value='File', /Menu)

   ; Create OUTPUT menu buttons for formatted output files.

output = Widget_Button(filer, Value='Save As...', /Menu)

   ; Check for availability of GIF files.

thisVersion = Float(!Version.Release)
IF thisVersion LT 5.4 THEN haveGif = 1 ELSE haveGIF = 0

IF havegif THEN b = Widget_Button(output, Value='GIF File', $
   UValue='GIF', Event_Pro='XImage_Output')
button = Widget_Button(output, Value='JPEG File', $
   UValue='JPEG', Event_Pro='XImage_Output')
button = Widget_Button(output, Value='TIFF File', $
   UValue='TIFF', Event_Pro='XImage_Output')
button = Widget_Button(output, Value='PNG File', $
   UValue='PNG', Event_Pro='XImage_Output')
button = Widget_Button(output, Value='PICT File', $
   UValue='PICT', Event_Pro='XImage_Output')
button = Widget_Button(output, Value='BMP File', $
   UValue='BMP', Event_Pro='XImage_Output')

dummy = Widget_Button(filer, Value='Print', $
   Event_Pro='XImage_Printing', UValue='PRINT')
dummy = Widget_Button(filer, Value='Print Setup', $
   Event_Pro='XImage_Printing', UValue='SETUP')
dummy = Widget_Button(filer, /Separator, Value='Quit', $
   Event_Pro='XImage_Exit')

IF trueColor EQ 0 THEN BEGIN

   ; Create COLORS menu buttons for changing colors.

   colorID = Widget_Button(menubase, Value='Colors', /Menu)

   ; Image Colors

   imagecolors = Widget_Button(colorID, Value='Image Colors...', $
      Event_Pro='XImage_Image_Colors')

ENDIF


   ; Create IMAGE PROCESSING menu buttons.

processing = Widget_Button(menubase, Menu=1, $
   Value='Processing')
edge = Widget_Button(processing, Menu=1, $
   Value='Edge Enhancement')
dummy = Widget_Button(edge, Value='Sobel', UValue='SOBEL', $
   Event_Pro='XImage_Processing')
dummy = Widget_Button(edge, Value='Roberts', UValue='ROBERTS', $
   Event_Pro='XImage_Processing')
smoother = Widget_Button(processing, Menu=1, $
   Value='Image Smoothing')
dummy = Widget_Button(smoother, Value='Boxcar', UValue='BOXCAR', $
   Event_Pro='XImage_Processing')
dummy = Widget_Button(smoother, Value='Median', UValue='MEDIAN', $
   Event_Pro='XImage_Processing')
dummy = Widget_Button(processing, Value='Original Image', $
   Event_Pro='XImage_Processing', UValue='ORIGINAL')

    ; Zoom in and out buttons.

zoomit = Widget_Button(menubase, Value='Zoom')

zInButton = Widget_Button(zoomit, Value='Zoom In', $
   Event_Pro='XImage_Zoom_Button_Event', UValue='ZOOM_IN')

zOutButton = Widget_Button(zoomit, Value='Zoom Out', $
   Event_Pro='XImage_Zoom_Button_Event', UValue='ZOOM_OUT')

    ; Realize the widgets and get the window object.

Widget_Control, tlb, /Realize
Widget_Control, drawID, Get_Value=thisWindow

   ; Find out the size of the base below the image
   ; so you can keep it in the face of window resizing.

geom = Widget_Info(bottomBase, /Geometry)
basesize = geom.scr_ysize

   ; Load the palette into the window. This will cause the
   ; image to be output through the palette always, even
   ; when displayed on 24-bit displays.

thisWindow->SetProperty, Palette=thisPalette

   ; Draw the graphic in the window.

thisWindow->Draw, plotView

   ; Get a printer object for this graphic.

thisPrinter = Obj_New('IDLgrPrinter')

   ; Create a container object to hold all the other
   ; objects. This will make it easy to free all the
   ; objects when we are finished with the program.
   ; Make a container to hold all the objects you created.

thisContainer = Obj_New('IDL_Container')
thisContainer->Add, thisWindow
thisContainer->Add, plotView
thisContainer->Add, thisPrinter
thisContainer->Add, xTitle
thisContainer->Add, yTitle
thisContainer->Add, thisPalette
thisContainer->Add, helvetica10pt
thisContainer->Add, helvetica8pt
thisContainer->Add, xaxis1
thisContainer->Add, xaxis2
thisContainer->Add, yaxis1
thisContainer->Add, yaxis2
thisContainer->Add, plotTitle

s = Size(*imagePtr, /Dimensions)

    ; Create an info structure to hold program information.

info = { thisContainer:thisContainer, $  ; The container object.
         thisPalette:thisPalette, $      ; The palette for INDEXED color.
         thisWindow:thisWindow, $        ; The window object.
         plotView:plotView, $            ; The view that will be rendered.
         thisPrinter:thisPrinter, $      ; The printer object.
         thisImage:thisImage, $          ; The image object.
         processPtr:processPtr, $        ; The pointer to the processed image.
         imagePtr:imagePtr, $            ; The pointer to the original image.
         xsize:xsize, $                  ; The X size of the image
         ysize:ysize, $                  ; The Y size of the image.
         plotTitle:plotTitle, $          ; The plot title.
         viewRect:viewRect, $            ; The original viewplane rectangle.
         xAxis1:xAxis1, $                ; Bottom X axis.
         xAxis2:xAxis2, $                ; Top X axis
         yAxis1:yAxis1, $                ; Left Y axis.
         yAxis2:yAxis2, $                ; Right Y axis.
         r:r, $                          ; The red color table values.
         g:g, $                          ; The green color table values.
         b:b, $                          ; The blue color table values.
         antiqueWhite:antiqueWhite, $    ; The background color.
         navyBlue:navyBlue, $            ; The axes color.
         firebrickRed:firebrickRed, $    ; The annotation color.
         truecolor:truecolor, $          ; A flag indicating a 24-bit image.
         xvalueID:xvalueID, $            ; X location widget ID.
         yvalueID:yvalueID, $            ; Y location widget ID.
         valueID:valueID, $              ; Image value widget ID.
         basesize:basesize, $            ; The height of the label base.
         keepAspect:keepAspect, $        ; The image keep aspect flag.
         drawID:drawID }                 ; The draw widget ID.

Widget_Control, tlb, Set_UValue=info, /No_Copy

XManager, 'ximage', tlb, Cleanup='XImage_Cleanup', $
   Group_Leader=group, /No_Block, Event_Handler='XImage_Resize'
END