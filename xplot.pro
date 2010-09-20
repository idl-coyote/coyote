;+
; NAME:
;       XPLOT
;
; PURPOSE:
;       The purpose of this program is to demonstrate how to
;       create a line plot with axes and a title in the
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
;       XPlot, x, y
;
; REQUIRED INPUTS:
;       x: A vector of input values used as the dependent data.
;
; OPTIONAL INPUTS
;       y: A vector of input values used as the dependent data.
;          If both x and y parameters are present, x is the independent data.
;
; OPTIONAL KEYWORD PARAMETERS:
;
;       COLORPRINT: This keyword is allowed for historical reasons, but does
;       nothing. Color printing is now available from the Print menu at all
;       times.
;
;       EXACT: Set this keyword to a one- or two-element array to set exact axis
;       scaling for the axes. If Exact is a one-element array, both axes are
;       set to the same value. If Exact is a two-element array, the first
;       elements sets the X axis property and the second element sets the Y
;       axis property. For example, to set the X axis to exact scaling and
;       the Y axis to normal scaling, type:
;
;           IDL> x = Findgen(10)
;           IDL> XPlot, x, Sin(x), Exact=[1,0], XRange=[0,8.5]
;
;       _EXTRA: This keyword collects otherwise undefined keywords that are
;       passed to new Plot command. To some extent these are similar to the
;       old IDL Plot command. For example: Linestyle=2, Thick=3,
;       XRange=[-100,100], etc.
;
;       GROUP_LEADER: The group leader for this program. When the group leader
;       is destroyed, this program will be destroyed.
;
;       LANDSCAPE: Set this keyword if you are printing in landscape mode. The
;       default is Portrait mode. The Landscape keyword on the PRINTER object
;       is set, but not all printers will honor this keyword setting. If yours
;       does not, set Landscape mode in the Printer Setup dialog.
;
;       POSITION: A four-element array of the form [x0, y0, x1, y1] for locating
;       the axes of the plot in the display window. The coordinates are in
;       "normalized" units, which means the extent of the window is from 0.0 to 1.0.
;       The default value of POSITION is [0.15, 0.15, 0.925, 0.925].
;
;       PSYM: The index of a plotting symbol to use on the plot. Integers 0-7
;       are valid values.
;
;       SYMSIZE: Sets the size of the symbols. By default, symbols are sized
;       so that they are 0.015 percent of the axis range.
;
;       VECTOR: Set this keyword if you want the printed output to be in
;       vector (as opposed to bitmap) form. This is faster, but not as accurate.
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
;       Requires FSC_NORMALIZE from the Coyote Library.
;
;         http://www.dfanning.com/programs/fsc_normalize.pro
;
; EXAMPLE:
;       To use this program, pass a 1D vector or vectors, like this:
;
;        IDL> XPlot, RandomU(seed, 11) * 9, YRange=[0, 10]
;
; MODIFICATION HISTORY:
;       Written by David W. Fanning, 13 June 97.
;       Modified axis font handling. 17 Sept 97. DWF.
;       Was not destroying all objects on exit. 12 Feb 98. DWF.
;       Changed IDLgrContainer to IDL_Container to fix 5.1 problems. 20 May 98. DWF.
;       Fixed a bug in the way symbols were (NOT!) sized. 11 May 99. DWF.
;       Added non-exact axis scaling. 12 May 99. DWF.
;       Fixed bug that changed data when calling with single parameter. 13 May DWF.
;       Added VECTOR, LANDSCAPE and COLORPRINT keywords and improved printing
;          capabilities. 16 Feb 2000. DWF.
;       Modified the EXACT keyword to accept values for X and Y axes
;          independently. 10 May 2000. DWF.
;       Updated for IDL 5.4. 13 June 2001. DWF.
;       Added EPS output via Clipboard object. 19 May 2002. DWF.
;       Added additional color schemes. 22 May 2002. DWF.
;       Added the ability to do color printing from the PRINT menu. 22 May 2002.
;       Removed COLORPRINT keyword. Keyword is allowed, but does nothing. 22 May 2002.
;       Added POSITION keyword so you can position plot in window. 1 August 2002. DWF.
;       Removed NORMALIZE from source code. 29 Nov 2005. DWF.
;       Fixed a problem with the plot title not appearing. 2 Dec 2005. DWF.
;       Slightly adjusted plot position and removed VECTOR keyword when producing
;          PostScript output to avoid losing axis characters in output. 27 Feb 2008. DWF.
;       Modification to PostScript Encapsulated output, allowed vector output again. 28 July 2008. DWF.
;       Renamed NORMALIZE to FSC_NORMALIZE to avoid numerous naming conflicts. 17 October 2008. DWF.
;       Fixed a placement problem with the plot title. 21 December 2008. DWF.
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
FUNCTION XPlot_Aspect, aspectRatio, MARGIN=margin, WindowAspect=wAspectRatio

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



PRO XPlot_Output, event

   ; This event handler creates GIF and JPEG files.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get a snapshop of window contents. (TVRD equivalent.)

info.thisWindow->GetProperty, Image_Data=snapshot

   ; What kind of file is wanted?

Widget_Control, event.id, GET_UValue=whichFileType
CASE whichFileType OF

   'GIF': BEGIN

         ; Because we are using RGB color for this model, we have
         ; a 3-m-n array. Use Color_Quan to create a 2D image and
         ; appropriate color tables for the GIF file.

      image2D = Color_Quan(snapshot, 1, r, g, b)
      filename = Dialog_Pickfile(/Write, File='xplot.gif')
      IF filename NE '' THEN Write_GIF, filename, image2d, r, g, b
      END

   'JPEG': BEGIN

      filename = Dialog_Pickfile(/Write, File='xplot.jpg')
      IF filename NE '' THEN Write_JPEG, filename, snapshot, True=1
      END


   'TIFF': BEGIN

      filename = Dialog_Pickfile(/Write, File='xplot.tif')
      IF filename NE '' THEN BEGIN

         ; TIFF files should have their Y direction reversed for
         ; compatibility with most other software.

         Write_TIFF, filename, Reverse(snapshot,3)
      ENDIF
      END

      ; Encapsulated PostScript output here.

   ELSE: BEGIN
      IF whichFileType NE 'NORMAL' THEN filename = Dialog_Pickfile(/Write, File='xplot.eps') ELSE $
         filename = 'xplot.normal.ps'
      IF filename NE '' THEN BEGIN
         CASE whichFileType OF
            'EPS-72' : BEGIN
                resolution = [2.54/72, 2.54/72]
                viewDimensions = 72./72
                END
            'EPS-150': BEGIN
                resolution = [2.54/150, 2.54/150]
                viewDimensions = 150/72.
                END
            'EPS-300': BEGIN
                resolution = [2.54/300, 2.54/300]
                viewDimensions = 300/72.
                END
            'EPS-600': BEGIN
                resolution = [2.54/600, 2.54/600]
                viewDimensions = 600/72.
                END
            'EPS-1200': BEGIN
                resolution = [2.54/1200, 2.54/1200]
                viewDimensions = 600/72.
                END
                
         ENDCASE
         info.thisWindow->GetProperty, Dimensions=theDimensions, Units=theUnits
         clipboard = Obj_New('IDLgrClipboard', Dimensions=viewDimensions*theDimensions, $
            Resolution=resolution, Units=theUnits, Quality=2)
            
         ; Removed VECTOR keyword to avoid losing axis characters in 
         ; PostScript output when using exponential values.
         clipboard->Draw,info.plotView, Filename=filename, /PostScript, /Vector
         Obj_Destroy, clipboard

      ENDIF
      END

ENDCASE

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO XPlot_Exit, event

   ; Exit the program.

Widget_Control, event.top, /Destroy
END
;-------------------------------------------------------------------



PRO XPlot_Printing, event

   ; Printer output handled here.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Does the user really want to print?

print_it = Dialog_PrinterSetup(info.thisPrinter)
IF NOT print_it THEN BEGIN
   Widget_Control, event.top, Set_UValue=info, /No_Copy
   RETURN
ENDIF

   ; Need black and white printing?

Widget_Control, event.id, Get_UValue=outputType
IF outputType EQ 'B&W' THEN BEGIN

      ; Find out the current colors of all the objects.

   info.plotView->GetProperty, Color=backgroundColor
   info.thisPlot->GetProperty, Color=plotColor
   info.xAxis1->GetProperty, Color=axisColor
   info.thisSymbol->GetProperty, Color=symbolColor

      ; Change colors to black and white for printing.

   info.plotView->SetProperty, Color=[255,255,255]
   info.thisPlot->SetProperty, Color=[0,0,0]
   info.xAxis1->SetProperty, Color=[0,0,0]
   info.yAxis1->SetProperty, Color=[0,0,0]
   info.xAxis2->SetProperty, Color=[0,0,0]
   info.yAxis2->SetProperty, Color=[0,0,0]
   info.thisSymbol->SetProperty, Color=[0,0,0]
   info.plotTitle->SetProperty, Color=[0,0,0]
ENDIF

   ; I want the output on the page to have the same aspect ratio
   ; (ratio of height to width) as I see in the display window.

info.thisWindow->GetProperty, Dimensions=wdims
info.thisPrinter->GetProperty, Dimensions=pdims
plotAspect = Float(wdims[1]) / wdims[0]
windowAspect = Float(pdims[1]) / pdims[0]
position = XPlot_Aspect(plotAspect, WindowAspect=windowAspect, Margin=0)
info.plotView->SetProperty, Dimensions=[position[2]-position[0], position[3]-position[1]], $
   Location=[position[0], position[1]], Units=3

   ; Print it. May take a little time. Alert the user.

Widget_Control, Hourglass=1
info.thisPrinter->Draw, info.plotView, Vector=info.vector
info.thisPrinter->NewDocument
Widget_Control, Hourglass=0

   ; Put everything back the way it was if you are doing black and white printing.

IF outputType EQ 'B&W' THEN BEGIN
   info.plotView->SetProperty, Color=backgroundColor
   info.thisPlot->SetProperty, Color=plotColor
   info.xAxis1->SetProperty, Color=axisColor
   info.yAxis1->SetProperty, Color=axisColor
   info.xAxis2->SetProperty, Color=axisColor
   info.yAxis2->SetProperty, Color=axisColor
   info.thisSymbol->SetProperty, Color=symbolColor
   info.plotTitle->SetProperty, Color=axisColor
ENDIF

   ; Restore the plot view properties.

info.PlotView->SetProperty, Location=[0,0], Dimensions=[0,0]

   ; Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO Xplot_Cleanup, id

    ; Come here when the widget dies. Free all the program
    ; objects, pointers, pixmaps, etc. and release memory.

Widget_Control, id, Get_UValue=info
IF N_Elements(info) NE 0 THEN Obj_Destroy, info.thisContainer
END
;---------------------------------------------------------------------



PRO XPlot_Draw_Widget_Events, event

    ; This event handler handles draw widget expose events.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Draw the graphic.

info.thisWindow->Draw, info.plotView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;------------------------------------------------------------------------



PRO XPlot_Resize_Events, event

    ; This event handler handles TLB resize events.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Resize the draw widget.

info.thisWindow->SetProperty, Dimension=[event.x, event.y]

    ; Redisplay the graphic.

info.thisWindow->Draw, info.plotView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;------------------------------------------------------------------------



PRO XPlot_ColorSchemes, event

    ; This event handler applies different color schemets.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Get the requested this color scheme.

Widget_Control, event.id, Get_Value=thisScheme

    ; Change it.

CASE thisScheme OF
   'Original Colors': BEGIN
      info.plotView->SetProperty, Color=[80,80,80]
      info.plotTitle->SetProperty, Color=[255, 255, 0]
      info.thisSymbol->SetProperty, Color=[0, 255, 0]
      info.thisPlot->SetProperty, Color=[0, 255, 0]
      info.xaxis1->SetProperty, Color=[255, 255, 0]
      info.xaxis2->SetProperty, Color=[255, 255, 0]
      info.yaxis1->SetProperty, Color=[255, 255, 0]
      info.yaxis2->SetProperty, Color=[255, 255, 0]
      END
   'Black and White': BEGIN
      info.plotView->SetProperty, Color=[255,255,255]
      info.thisPlot->SetProperty, Color=[0,0,0]
      info.xAxis1->SetProperty, Color=[0,0,0]
      info.yAxis1->SetProperty, Color=[0,0,0]
      info.xAxis2->SetProperty, Color=[0,0,0]
      info.yAxis2->SetProperty, Color=[0,0,0]
      info.thisSymbol->SetProperty, Color=[0,0,0]
      info.plotTitle->SetProperty, Color=[0,0,0]
      END
   'White Background': BEGIN
      info.plotView->SetProperty, Color=[255,255,255]
      info.thisPlot->SetProperty, Color=[139,69,19]
      info.xAxis1->SetProperty, Color=[0,100,0]
      info.yAxis1->SetProperty, Color=[0,100,0]
      info.xAxis2->SetProperty, Color=[0,100,0]
      info.yAxis2->SetProperty, Color=[0,100,0]
      info.thisSymbol->SetProperty, Color=[255, 99,71]
      info.plotTitle->SetProperty, Color=[0,0,128]
      END
ENDCASE

    ; Redisplay the graphic.

info.thisWindow->Draw, info.plotView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;------------------------------------------------------------------------


PRO XPlot_Linestyle, event

    ; This event handler handles linesytle change events.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Get the requested linestyle.

Widget_Control, event.id, Get_UValue=thisLineStyle

    ; Change it.

info.thisPlot->SetProperty, LineStyle=thisLineStyle

    ; Redisplay the graphic.

info.thisWindow->Draw, info.plotView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;------------------------------------------------------------------------



PRO XPlot_Symbol, event

    ; This event handler handles symbol change events.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Get the requested symbol.

Widget_Control, event.id, Get_UValue=thisSymbol

    ; Change it.

info.thisPlot->GetProperty, Symbol=symbolObject
symbolObject->SetProperty, Data=thisSymbol

    ; Redisplay the graphic.

info.thisWindow->Draw, info.plotView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;------------------------------------------------------------------------



PRO XPlot_SymbolSize, event

    ; This event handler handles symbol size change events.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Get the requested symbol size.

Widget_Control, event.id, Get_UValue=thisSize

    ; Change it.

info.thisPlot->GetProperty, Symbol=symbolObject
symbolObject->SetProperty, Size=info.symbolSize * thisSize

    ; Redisplay the graphic.

info.thisWindow->Draw, info.plotView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;------------------------------------------------------------------------


PRO XPlot, xx, yy, _Extra=extra, PSym=psym, Title=title, SymSize=symSize, $
   Group_Leader=group, XTitle=xtitle, YTitle=ytitle, Exact=exact, $
   ColorPrint=colorprint, Vector=vector, Landscape=landscape, $
   Position=position

   ; New printer functionality requires IDL 5.3 or higher.

IF Float(!Version.Release) LT 5.3 THEN BEGIN
   ok = Dialog_Message('Program functionality requires IDL 5.3 or higher. Returning...')
   RETURN
ENDIF

    ; Check to be sure at least one parameter is present.

np =  N_Params()
CASE np OF
    0: BEGIN
       Print, 'Using fake data in XPLOT...'
       y = FIndGen(101)
       y = Sin(y/5) / Exp(y/50)
       x = IndGen(N_Elements(y))
       xtitle = 'Time'
       ytitle = 'Signal Stength'
       END
    1: BEGIN
       y = xx
       x = IndGen(N_Elements(y))
       END
    ELSE:
ENDCASE

   ; Make sure no data parameters change.

IF N_Elements(x) EQ 0 THEN x = xx
IF N_Elements(y) EQ 0 THEN y = yy

    ; Check keyword parameters.

IF N_Elements(psym) EQ 0 THEN psym = 0
IF N_Elements(position) EQ 0 THEN position = [0.2, 0.2, 0.95, 0.95]
IF N_Elements(title) EQ 0 THEN title = ''
IF N_Elements(symsize) EQ 0 THEN symsize = 1.0
IF N_Elements(xtitle) EQ 0 THEN xtitle = ''
IF N_Elements(ytitle) EQ 0 THEN ytitle = ''
colorprint = Keyword_Set(colorprint)
vector = Keyword_Set(vector)
landscape = Keyword_Set(landscape)
CASE N_Elements(exact) OF
   0: exact = [0,0]
   1: exact = Replicate(exact, 2)
   2:
   ELSE: BEGIN
      ok = Dialog_Message('Exact keyword contains too many elements. Returning...')
      RETURN
      ENDCASE
ENDCASE

   ; Create title objects for the axes. Color them yellow.

xTitle = Obj_New('IDLgrText', xtitle, Color=[255,255,0])
yTitle = Obj_New('IDLgrText', ytitle, Color=[255,255,0])

    ; Make a symbol object. Color symbols cyan.

thisSymbol = Obj_New('IDLgrSymbol', psym, Color=[0, 255, 255])

    ; Make a font object.

helvetica10pt = Obj_New('IDLgrFont', 'Helvetica', Size=10)

    ; Create a plot object. The plot will be in the coordinate
    ; space 0->1. The view will be in the range -0.35->1.25 so
    ; that the plot axis annotation will be visable. Make the plot
    ; a green color.

thisPlot = Obj_New("IDLgrPLOT", x, y, _Extra=extra, $
   Color=[0,255,0], Symbol=thisSymbol, Thick=2)

    ; Get the data ranges from the Plot Object.

thisPlot->GetProperty, XRange=xrange, YRange=yrange

    ; Create plot box style axes. Make the axes yellow.
    ; The large values in the LOCATION keyword indicates which
    ; values are NOT used. The axes text is set to Helvetica
    ; 10 point font.

xAxis1 = Obj_New("IDLgrAxis", 0, Color=[255,255,0], Ticklen=0.025, $
    Minor=4, Range=xrange, Title=xtitle, $
    Location=[1000, position[1] ,0], Exact=exact[0])
xAxis1->GetProperty, Ticktext=xAxisText
xAxisText->SetProperty, Font=helvetica10pt

xAxis2 = Obj_New("IDLgrAxis", 0, Color=[255,255,0], Ticklen=0.025, $
    Minor=4, /NoText, Range=xrange, TickDir=1, $
    Location=[1000, position[3], 0], Exact=exact[0])

yAxis1 = Obj_New("IDLgrAxis", 1, Color=[255,255,0], Ticklen=0.025, $
    Minor=4, Title=ytitle, Range=yrange, $
    Location=[position[0], 1000, 0], Exact=exact[1])
yAxis1->GetProperty, Ticktext=yAxisText
yAxisText->SetProperty, Font=helvetica10pt

yAxis2 = Obj_New("IDLgrAxis", 1, Color=[255,255,0], Ticklen=0.025, $
    Minor=4, /NoText, Range=yrange, TickDir=1, $
    Location=[position[2], 1000, 0], Exact=exact[1])

    ; Because we may not be using exact axis ranging, the axes
    ; may extend further than the xrange and yrange. Get the
    ; actual axis range so that the plot, etc. can be scaled
    ; appropriately.

xAxis1->GetProperty, CRange=xrange
yAxis1->GetProperty, CRange=yrange

    ; Set up the scaling so that the axes for the plot and the
    ; plot data extends from 0->1 in the X and Y directions.

xs = FSC_Normalize(xrange, Position=[position[0], position[2]])
ys = FSC_Normalize(yrange, Position=[position[1], position[3]])

    ; Scale the plot data and axes into 0->1.

thisPlot->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys
xAxis1->SetProperty, XCoord_Conv=xs
xAxis2->SetProperty, XCoord_Conv=xs
yAxis1->SetProperty, YCoord_Conv=ys
yAxis2->SetProperty, YCoord_Conv=ys

    ; Size the symbols appropriately for the plot.

xSymSize = (xrange[1] - xrange[0]) * 0.015 * symSize
ySymSize = (yrange[1] - yrange[0]) * 0.015 * symSize
IF Obj_Valid(thisSymbol) THEN thisSymbol->SetProperty, Size=[xSymSize, ySymSize]

  ; Create a plot title. Center it at a location above the plot.

helvetica14pt = Obj_New('IDLgrFont', 'Helvetica', Size=14)
plotTitle = Obj_New('IDLgrText', title, Color=[255,255,0], $
   Location=[0.55, 0.98, 0.0], Alignment=0.5, Font=helvetica14pt, $
   ENABLE_FORMATTING=1)

    ; Create a plot model and add axes, plot, and plot title to it.

plotModel = Obj_New('IDLgrModel')
plotModel->Add, thisPlot
plotModel->Add, xAxis1
plotModel->Add, xAxis2
plotModel->Add, yAxis1
plotModel->Add, yAxis2
plotModel->Add, plotTitle

    ; Create a view and add the plot model to it. Notice that the view
    ; is larger than the 0->1 plot area to accomodate axis annotation.
    ; The view will have a gray background.

plotView = Obj_New('IDLgrView', Viewplane_Rect=[0.0, 0.0, 1.0, 1.05], $
   Location=[0,0], Color=[80,80,80])
plotView->Add, plotModel

   ; Check for availability of GIF files.

thisVersion = Float(!Version.Release)
IF thisVersion LT 5.4 THEN haveGif = 1 ELSE haveGIF = 0

    ; Create the widgets for this program.

tlb = Widget_Base(Column=1, Title='Resizeable Line Plot Example', $
   TLB_Size_Events=1, MBar=menubase)

    ; Create FILE menu buttons for printing and exiting.

filer = Widget_Button(menubase, Value='File', /Menu)
printButton = Widget_Button(filer, Value='Print', $
   Event_Pro='XPlot_Printing', UValue='PRINT', /Menu)
b = Widget_Button(printButton, Value='Color', $
   Event_Pro='XPlot_Printing', UValue='COLOR')
b = Widget_Button(printButton, Value='Black and White', $
   Event_Pro='XPlot_Printing', UValue='B&W')

b = Widget_Button(filer, /Separator, Value='Exit', $
   Event_Pro='XPlot_Exit')

    ; Create OUTPUT menu buttons for formatted output files.

output = Widget_Button(menubase, Value='Output')
IF havegif THEN b = Widget_Button(output, Value='GIF File', $
   UValue='GIF', Event_Pro='XPlot_Output')
b = Widget_Button(output, Value='JPEG File', $
   UValue='JPEG', Event_Pro='XPlot_Output')
b = Widget_Button(output, Value='TIFF File', $
   UValue='TIFF', Event_Pro='XPlot_Output')
epsbutton = Widget_Button(output, Value='Encapsulated PostScript File', $
   UValue='EPS', Event_Pro='XPlot_Output', /Menu)
b = Widget_Button(epsbutton, Value='72 DPI Resolution', $
   UValue='EPS-72', Event_Pro='XPlot_Output')
b = Widget_Button(epsbutton, Value='150  DPI Resolution', $
   UValue='EPS-150', Event_Pro='XPlot_Output')
b = Widget_Button(epsbutton, Value='300 DPI Resolution', $
   UValue='EPS-300', Event_Pro='XPlot_Output')
b = Widget_Button(epsbutton, Value='600 DPI Resolution', $
   UValue='EPS-600', Event_Pro='XPlot_Output')
b = Widget_Button(epsbutton, Value='1200 DPI Resolution', $
   UValue='EPS-1200', Event_Pro='XPlot_Output')

    ; Create PROPERTIES menu buttons for plot properties.

propertiesID = Widget_Button(menubase, Value='Properties')

linestyleID = Widget_Button(propertiesID, Value='Line Style', $
   Event_Pro='XPlot_Linestyle', /Menu)
b = Widget_Button(linestyleID, Value='Solid', UValue=0)
b = Widget_Button(linestyleID, Value='Dot', UValue=1)
b = Widget_Button(linestyleID, Value='Dash', UValue=2)
b = Widget_Button(linestyleID, Value='Dash Dot', UValue=3)
b = Widget_Button(linestyleID, Value='Dash Dot Dot Dot', UValue=4)
b = Widget_Button(linestyleID, Value='Long Dash', UValue=5)
b = Widget_Button(linestyleID, Value='No Line', UValue=6)

symbolID = Widget_Button(propertiesID, Value='Symbol', $
   Event_Pro='XPlot_Symbol', /Menu)
b = Widget_Button(symbolID, Value='No Symbol', UValue=0)
b = Widget_Button(symbolID, Value='Plus Sign', UValue=1)
b = Widget_Button(symbolID, Value='Asterisk', UValue=2)
b = Widget_Button(symbolID, Value='Period', UValue=3)
b = Widget_Button(symbolID, Value='Diamond', UValue=4)
b = Widget_Button(symbolID, Value='Triangle', UValue=5)
b = Widget_Button(symbolID, Value='Square', UValue=6)
b = Widget_Button(symbolID, Value='X', UValue=7)

symbolSizeID = Widget_Button(propertiesID, Value='Symbol Size', $
   Event_Pro='XPlot_SymbolSize', /Menu)
b = Widget_Button(symbolSizeID, Value='0.25', UValue=0.25)
b = Widget_Button(symbolSizeID, Value='0.50', UValue=0.50)
b = Widget_Button(symbolSizeID, Value='0.75', UValue=0.75)
b = Widget_Button(symbolSizeID, Value='1.00', UValue=1.00)
b = Widget_Button(symbolSizeID, Value='1.25', UValue=1.25)
b = Widget_Button(symbolSizeID, Value='1.50', UValue=1.50)
b = Widget_Button(symbolSizeID, Value='1.75', UValue=1.75)
b = Widget_Button(symbolSizeID, Value='2.00', UValue=2.00)

    ; Create COLOR SCHEMES menu buttons for color options.

schemesID = Widget_Button(menubase, Value='Color Schemes')
b = Widget_Button(schemesID, Value='Original Colors', $
   Event_Pro='XPlot_ColorSchemes')
b = Widget_Button(schemesID, Value='Black and White', $
   Event_Pro='XPlot_ColorSchemes')
b = Widget_Button(schemesID, Value='White Background', $
   Event_Pro='XPlot_ColorSchemes')

    ; Create the draw widget. Use RGB color model. Be sure to set
    ; the Expose_Events keyword so that graphics are redisplayed
    ; properly. (This implies that RETAIN=0. The Graphics_Level
    ; keyword makes it a window object.

drawID = Widget_Draw(tlb, XSize=400, YSize=400, Color_Model=0, $
   Graphics_Level=2, Expose_Events=1, Retain=0, Renderer=1, $
   Event_Pro='XPlot_Draw_Widget_Events')

    ; Realize the widgets and get the window object.

Widget_Control, tlb, /Realize
Widget_Control, drawID, Get_Value=thisWindow

    ; Display the plot in the window.

thisWindow->Draw, plotView

   ; Get a printer object for this graphic.

thisPrinter = Obj_New('IDLgrPrinter', Landscape=landscape)

   ; Create a container object to hold all the other
   ; objects. This will make it easy to free all the
   ; objects when we are finished with the program.

thisContainer = Obj_New('IDL_Container')
thisContainer->Add, thisWindow
thisContainer->Add, plotView
thisContainer->Add, thisPrinter
thisContainer->Add, helvetica10pt
thisContainer->Add, helvetica14pt
thisContainer->Add, xaxis1
thisContainer->Add, xaxis2
thisContainer->Add, yaxis1
thisContainer->Add, yaxis2
thisContainer->Add, plotTitle
thisContainer->Add, xTitle
thisContainer->Add, yTitle
thisContainer->Add, thisSymbol

   ; Create an info structure to hold program information.

info = { thisContainer:thisContainer, $     ; The container object.
         thisPlot:thisPlot, $               ; The plot object.
         symbolSize:[xSymSize, ySymSize], $ ; The default symbol size.
         thisWindow:thisWindow, $           ; The window object.
         thisSymbol:thisSymbol, $           ; The symbol object.
         plotTitle:plotTitle, $             ; The plot title object.
         xTitle:xTitle, $                   ; The X axis title object.
         yTitle:yTitle, $                   ; The Y axis title object.
         plotView:plotView, $               ; The view that will be rendered.
         xaxis1:xaxis1, $                   ; The X axis object.
         xaxis2:xaxis2, $                   ; The X axis object.
         yaxis1:yaxis1, $                   ; The Y axis object.
         yaxis2:yaxis2, $                   ; The Y axis object.
         colorprint:colorprint, $           ; A flag for color printing.
         vector:vector, $                   ; A flag for vector printing.
         landscape:landscape, $             ; A flag for landscape printing.
         thisPrinter:thisPrinter }          ; The printer object.

    ; Put the info stucture in the TLB.

Widget_Control, tlb, Set_UValue=info, /No_Copy

    ; Start the event loop.

XManager, 'xplot', tlb, Cleanup='XPlot_Cleanup', Group_Leader=group, $
   Event_Handler='XPlot_Resize_Events', /No_Block

END
;------------------------------------------------------------------------


