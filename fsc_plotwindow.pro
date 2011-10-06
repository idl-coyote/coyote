;+
; NAME:
;   FSC_PLOTWINDOW
;
; PURPOSE:
;
;   The purpose of this compound widget is to create a resizeable
;   "plot window" inside a larger "page window". I'm not sure it
;   has any value except as a utility routine for the PostScript
;   configuration object FSC_PSCONFIG__DEFINE, but it's a neat
;   program anyway. :-)
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: david@idlcoyote.com
;   Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; CATEGORY:
;
;   Utility routine for FSC_PSCONFIG__DEFINE.
;
; CALLING SEQUENCE:
;
;   plotwindowObject = CW_PlotWindow(parent)
;
; REQUIRED INPUT PARAMETERS:
;
;   parent - The parent base widget of this compound widget.
;
; RETURN VALUE:
;
;   plotwindowObject - The object reference of the compound widget.
;
; KEYWORDS:
;
;   COLOR - If set, display the window in "color". This is the default on 24-bit devices.
;   DEBUG - Set this keyword to turn traceback error handling on in the error handling code.
;   EVENT_PRO - The event procedure for the widget. Required for events to be generated. Otherwise, all events are handled internally.
;   LANDSCAPE - If set, display the page in landscape mode. Otherwise the page is display in portrait mode.
;   PAGESIZE - The "pagesize" of the widget. Possible values are: "LETTER", "LEDGER", "LEGAL", "A4", and "DISPLAY".
;   UNITS - A string indicating INCHES or CENTIMETER units. DEVICE units represented by a null string, "".
;   UVALUE - A user value for the caller of this program.
;   WINDOWCOLOR - A three-element array specifying the background window color (RGB).
;   WINDOWSIZE - The size of the "window" on the page. A four-element array of normalized coordinates in the form [x0, y0, x1, y1].
;
; EVENT STRUCTURE:
;
;   The event structure that is returned from this compound widget is defined like this,
;   where the sizes and offsets locate the target "window" on the page in normalized units:
;
;      event = {ID:0L, TOP:0L, HANDLER:0L, XSize:0.0, YSize:0.0, XOffset:0.0, YOffset:0.0}
;
; MODIFICATIONS:
;
;   Written by David Fanning, 31 January 2000.
;   Fixed a small bug that prevented it working on Macintosh computers. 26 Sept 2000. DWF.
;   Added a "DISPLAY" page size, so the program can be used to position
;      plots and other graphics in a display window. The "page area" will
;      have the same aspect ratio is the current graphics window. 17 March 2001. DWF.
;   Changed some of the tolerances for "closeness" from 0.1 to 0.025 to allow smaller
;      sizing for colorbars and other small objects. 6 July 2005. DWF.
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

FUNCTION FSC_PLOTWINDOW_Error_Message, theMessage, Traceback=traceback, NoName=noName

On_Error, 2

   ; Check for presence and type of message.

IF N_Elements(theMessage) EQ 0 THEN theMessage = !Error_State.Msg
s = Size(theMessage)
messageType = s[s[0]+1]
IF messageType NE 7 THEN BEGIN
   Message, "The message parameter must be a string."
ENDIF

   ; Get the call stack and the calling routine's name.

Help, Calls=callStack
callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]

   ; Are widgets supported? Doesn't matter in IDL 5.3 and higher.

widgetsSupported = ((!D.Flags AND 65536L) NE 0) OR Float(!Version.Release) GE 5.3
IF widgetsSupported THEN BEGIN
   IF Keyword_Set(noName) THEN answer = Dialog_Message(theMessage) ELSE BEGIN
      IF StrUpCase(callingRoutine) EQ "$MAIN$" THEN answer = Dialog_Message(theMessage) ELSE $
         answer = Dialog_Message(StrUpCase(callingRoutine) + ": " + theMessage)
   ENDELSE
ENDIF ELSE BEGIN
      Message, theMessage, /Continue, /NoPrint, /NoName, /NoPrefix
      Print, '%' + callingRoutine + ': ' + theMessage
      answer = 'OK'
ENDELSE

   ; Provide traceback information if requested.

IF Keyword_Set(traceback) THEN BEGIN
   Help, /Last_Message, Output=traceback
   FOR j=0,N_Elements(traceback)-1 DO Print, traceback[j]
ENDIF

RETURN, answer
END ;----------------------------------------------------------------------------------



FUNCTION FSC_PLOTWINDOW_Normalize, range, Position=position

On_Error, 1
IF N_Params() EQ 0 THEN Message, 'Please pass range vector as argument.'

IF (N_Elements(position) EQ 0) THEN position = [0.0, 1.0] ELSE $
    position=Float(position)
range = Float(range)

scale = [((position[0]*range[1])-(position[1]*range[0])) / $
    (range[1]-range[0]), (position[1]-position[0])/(range[1]-range[0])]

RETURN, scale
END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW::Refresh
self.theWindow->Draw, self.theView
END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW::SetWindowColor, theColor

; Set the background color of the view.

self.theView->SetProperty, Color=theColor
self.theWindow->Draw, self.theView
END ;----------------------------------------------------------------------------------



FUNCTION FSC_PLOTWINDOW::GetUValue

; Get the user value of the compound widget.

parent = Widget_Info(self.drawID, /Parent)
Widget_Control, parent, Get_UValue=theValue
RETURN, theValue
END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW::SetColor, on_off

; Sets the color of the plot window. Different colors
; depending on depth of visual class.

IF on_off THEN BEGIN
   self.theBackground->SetProperty, Color=[255,255,150]
   self.thePlot->SetProperty, Color=[255, 0, 0]
ENDIF ELSE BEGIN
   Device, Get_Visual_Depth=theDepth
   IF theDepth GT 8 THEN backColor = [210, 200, 180] ELSE backColor = [220, 220, 220]
   self.theBackground->SetProperty, Color=backColor
   self.thePlot->SetProperty, Color=[0, 0, 0]
ENDELSE

   ; Draw the view.

IF Obj_Valid(self.theWindow) THEN self.theWindow->Draw, self.theView
END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW::SetUnits, units

; Sets the type of units to report in.

possibleUnits = ["INCHES", "CENTIMETERS", ""]
units = StrUpCase(units)
index = WHERE(possibleUnits EQ units, count)
IF count EQ 0 THEN BEGIN
   ok = Dialog_Message('Unknown units: ' + units + '. Returning...')
   RETURN
ENDIF
self.units = units
END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW::GetWindowLocation, xsize, ysize, xoffset, yoffset

; Returns the current size and offsets of the plot window in
; units of inches or centimeters, as appropriate.

xsize = self.xlength
ysize = self.ylength
xoffset = self.x1
yoffset = self.y1

factor = Float(self.pixels_per_inch)

CASE self.units OF
   'INCHES': BEGIN
      xsize_in_pixels = (xsize * self.xsize)
      ysize_in_pixels = (ysize * self.ysize)
      xoff_in_pixels = (xoffset * self.xsize)
      yoff_in_pixels = (yoffset * self.ysize)
      xsize = (xsize_in_pixels / factor)
      ysize = (ysize_in_pixels / factor)
      xoffset = (xoff_in_pixels / factor)
      yoffset = (yoff_in_pixels / factor)
      ENDCASE
   'CENTIMETERS': BEGIN
      xsize_in_pixels = (xsize * self.xsize)
      ysize_in_pixels = (ysize * self.ysize)
      xoff_in_pixels = (xoffset * self.xsize)
      yoff_in_pixels = (yoffset * self.ysize)
      xsize = (xsize_in_pixels / factor * 2.54)
      ysize = (ysize_in_pixels / factor * 2.54)
      xoffset = (xoff_in_pixels / factor * 2.54)
      yoffset = (yoff_in_pixels / factor * 2.54)
      ENDCASE
   "" :
   ELSE: ok = Dialog_Message('Unknown units: ' + self.units + '. Returning...')
ENDCASE

END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW::SetWindowLocation, xsize, ysize, xoffset, yoffset

; Sets the location of the plot window in the view.
; Converts inches or centimeters to pixels.

factor = Float(self.pixels_per_inch)

CASE self.units OF
   'INCHES': BEGIN
      xsize_in_pixels = (xsize * factor)
      ysize_in_pixels = (ysize * factor)
      xoff_in_pixels = (xoffset * factor)
      yoff_in_pixels = (yoffset * factor)
      x1 = 0.0 > (xoff_in_pixels / self.xsize) < 0.85
      y1 = 0.0 > (yoff_in_pixels / self.ysize) < 0.85
      x2 = (x1 + (xsize_in_pixels / self.xsize)) < 1.0
      y2 = (y1 + (ysize_in_pixels / self.ysize)) < 1.0
      ENDCASE
   'CENTIMETERS': BEGIN
      xsize_in_pixels = (xsize * factor / 2.54)
      ysize_in_pixels = (ysize * factor / 2.54)
      xoff_in_pixels = (xoffset * factor / 2.54)
      yoff_in_pixels = (yoffset * factor / 2.54)
      x1 = 0.0 > (xoff_in_pixels / self.xsize) < 0.85
      y1 = 0.0 > (yoff_in_pixels / self.ysize) < 0.85
      x2 = (x1 + (xsize_in_pixels / self.xsize)) < 1.0
      y2 = (y1 + (ysize_in_pixels / self.ysize)) < 1.0
      ENDCASE
   "" :
   ELSE: ok = Dialog_Message('Unknown units: ' + self.units + '. Returning...')
ENDCASE

self.xlength = x2 - x1
self.ylength = y2 - y1
self.x1 = x1
self.x2 = x2
self.y1 = y1
self.y2 = y2
self->SetWindowSize, [x1, y1, x2, y2]

END ;----------------------------------------------------------------------------------

PRO FSC_PLOTWINDOW::SetWindowSize, position

; Set the size of the plot window. Axes have to be
; rescaled appropriately.

IF N_Elements(position) EQ 0 THEN position = [0.2, 0.2, 0.8, 0.8]

x1 = position[0]
y1 = position[1]
x2 = position[2]
y2 = position[3]

new = FltArr(2,5)
new[0,*] = [x1, x1, x2, x2, x1]
new[1,*] = [y1, y2, y2, y1, y1]

   ; Modify the graphics objects.

self.theBackground->SetProperty, Data=new
xs = FSC_PlotWindow_Normalize([0,100], Position=[new[0,0]+0.05, new[0,2]-0.05])
ys = FSC_PlotWindow_Normalize([-1,1], Position=[new[1,0]+0.05, new[1,1]-0.05])

self.theXAxis->SetProperty, XCoord_Conv=xs, Location=[1000, new[1,0]+0.05, 0]
self.theYAxis->SetProperty, YCoord_Conv=ys, Location=[new[0,0]+.05, 1000, 0]
self.thePlot->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys
self.x1 = new[0,0]
self.x2 = new[0,2]
self.y1 = new[1,0]
self.y2 = new[1,1]
self.xlength = self.x2 - self.x1
self.ylength = self.y2 - self.y1

   ; Draw the view after resetting the transformation matrix.

self.theModel->Reset
self.theWindow->Draw, self.theView
END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW::SetPageSize, pagesize, Landscape=landscape, TLB=tlb

; Sets the page size of the window.

; 200 pixels = 11 inches.
; Letter 8.5 x 11.
; Ledger 11 x 17.
; Legal 8.5 x 14.
; A4 8.27 x 11.7.

STANDARD_SIZE = self.pixels_per_inch * 11.0

IF N_Elements(pagesize) EQ 0 THEN pagesize = "LETTER"
pagesize = StrUpCase(pagesize)
IF N_Elements(tlb) EQ 0 THEN tlb = self.base

CASE pagesize OF
   'LETTER': BEGIN
         xsize = STANDARD_SIZE * (8.5/11.0)
         ysize = STANDARD_SIZE
      ENDCASE
   'LEDGER': BEGIN
         xsize = STANDARD_SIZE
         ysize = STANDARD_SIZE * (17.0/11.0)
      ENDCASE
   'LEGAL': BEGIN
         xsize = STANDARD_SIZE * (8.5/11.0)
         ysize = STANDARD_SIZE * (14.0/11.0)
      ENDCASE
   'A4': BEGIN
         xsize = STANDARD_SIZE * (8.27/11.0)
         ysize = STANDARD_SIZE * (11.7/11.0)
      ENDCASE
   'DISPLAY': BEGIN
         xsize = 300
         ratio = Float(!D.Y_Size) / !D.X_Size
         ysize = 300 * ratio
      ENDCASE
   ELSE: BEGIN
         ok = Dialog_Message('Unknown page size: ' + pagesize + '. Returning...')
         RETURN
      ENDCASE
ENDCASE

IF Keyword_Set(landscape) THEN BEGIN
   temp = xsize
   xsize = ysize
   ysize = temp
ENDIF

self.xsize = ROUND(xsize)
self.ysize = ROUND(ysize)
self.pagesize = pagesize

   ; Bugs in X windows implementation of window resizing necessitate
   ; destroying and re-creating the draw widget window.

IF !D.Name NE 'X' THEN BEGIN

   self.theWindow->SetProperty, Dimensions=[xsize, ysize]

ENDIF ELSE BEGIN

   Widget_Control, tlb, Update=0
   Widget_Control, self.drawID, Kill_Notify=""
   Widget_Control, self.drawID, /Destroy
   self.drawID = Widget_Draw(self.base, Button_Events=1, Expose_Events=1, $
      Retain=0, Graphics_Level=2, XSize=xsize, YSize=ysize, $
      Event_Pro='FSC_PLOTWINDOW_Events', UValue=self, Renderer=1, $
      Kill_Notify='FSC_PLOTWINDOW_Kill_Notify', Motion_Events=1)
   Widget_Control, self.drawID, /Realize
   Widget_Control, self.drawID, Get_Value=theWindow
   self.theWindow = theWindow
   Widget_Control, tlb, Update=1

ENDELSE

self.thewindow->Draw, self.theView

END ;----------------------------------------------------------------------------------



FUNCTION FSC_PLOTWINDOW::GetPagePixels, pagesize, Landscape=landscape

; Given the page size or type, returns the xsize and ysize
; of the draw widget in pixels.

; 198 pixels = 11 inches.
; Letter 8.5 x 11.
; Ledger 11 x 17.
; Legal 8.5 x 14.
; A4 8.27 x 11.7.

STANDARD_SIZE = self.pixels_per_inch * 11.0

IF N_Elements(pagesize) EQ 0 THEN pagesize = "LETTER"
pagesize = StrUpCase(pagesize)

CASE pagesize OF
   'LETTER': BEGIN
         xsize = STANDARD_SIZE * (8.5/11.0)
         ysize = STANDARD_SIZE
      ENDCASE
   'LEDGER': BEGIN
         xsize = STANDARD_SIZE
         ysize = STANDARD_SIZE * (17.0/11.0)
      ENDCASE
   'LEGAL': BEGIN
         xsize = STANDARD_SIZE * (8.5/11.0)
         ysize = STANDARD_SIZE * (14.0/11.0)
      ENDCASE
   'A4': BEGIN
         xsize = STANDARD_SIZE * (8.27/11.0)
         ysize = STANDARD_SIZE * (11.7/11.0)
      ENDCASE
   'DISPLAY': BEGIN
         xsize = 300
         ratio = Float(!D.Y_Size) / !D.X_Size
         ; Check for *very* strange windows.
         IF ratio LT 0.2 THEN ratio = 1.0
         IF ratio GT 5 THEN ratio = 1.0
         ysize = 300 * ratio
      ENDCASE
ENDCASE

IF Keyword_Set(landscape) THEN BEGIN
   temp = xsize
   xsize = ysize
   ysize = temp
ENDIF

RETURN, [ROUND(xsize), ROUND(ysize)]

END ;----------------------------------------------------------------------------------



FUNCTION FSC_PLOTWINDOW::GetPosition

; Returns the current position of the plot window in
; the view. Normalized coordinates.

RETURN, [self.x1, self.y1, self.x2, self.y2]
END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW::Resize, event, direction

; Performs window resizing, depending upon which of eight
; possible directions you can move the window.

IF direction EQ 'NONE' OR direction EQ 'MOVE' THEN BEGIN
   RETURN
ENDIF

   ; Calculate movement in normalized coordinates. Update start coords.

deltax = (event.x - self.currentX) / self.xsize
deltay = (event.y - self.currentY) / self.ysize
self.currentX = event.x
self.currentY = event.y

CASE direction OF
   'SE': BEGIN
         x1 = self.x1
         x2 = (self.x2 + deltax) < 1.0
         y1 = (self.y1 + deltay) > 0.0
         y2 = self.y2
         IF x2 LE x1 + 0.025 THEN x2 = x1 + 0.025
         IF y1 GE y2 - 0.025 THEN y1 = y2 - 0.025
         END
   'E' : BEGIN
         x1 = self.x1
         x2 = (self.x2 + deltax) < 1.0
         y1 = self.y1
         y2 = self.y2
         IF x2 LE x1 + 0.025 THEN x2 = x1 + 0.1
         END
   'SW': BEGIN
         x1 = (self.x1 + deltax) > 0.0
         x2 = self.x2
         y1 = (self.y1 + deltay) > 0.0
         y2 = self.y2
         IF x1 GE x2 - 0.025 THEN x1 = x2 - 0.025
         IF y1 GE y2 - 0.025 THEN y1 = y2 - 0.025
         END
   'S' : BEGIN
         x1 = self.x1
         x2 = self.x2
         y1 = (self.y1 + deltay) > 0.0
         y2 = self.y2
         IF y1 GE y2 - 0.025 THEN y1 = y2 - 0.025
         END
   'N' : BEGIN
         x1 = self.x1
         x2 = self.x2
         y1 = self.y1
         y2 = (self.y2  + deltay) < 1.0
         IF y2 LE y1 + 0.025 THEN y2 = y1 + 0.025
         END
   'NE': BEGIN
         x1 = self.x1
         x2 = (self.x2 + deltax) < 1.0
         y1 = self.y1
         y2 = (self.y2 + deltay) < 1.0
         IF x2 LE x1 + 0.025 THEN x2 = x1 + 0.025
         IF y2 LE y1 + 0.025 THEN y2 = y1 + 0.025
         END
   'W' : BEGIN
         x1 = (self.x1  + deltax) > 0.0
         x2 = self.x2
         y1 = self.y1
         y2 = self.y2
         IF x1 GE x2 - 0.025 THEN x1 = x2 - 0.025
         END
   'NW': BEGIN
         x1 = (self.x1 + deltax) > 0.0
         x2 = self.x2
         y1 = self.y1
         y2 = (self.y2 + deltay) < 1.0
         IF x1 GE x2 - 0.025 THEN x1 = x2 - 0.025
         IF y2 LE y1 + 0.025 THEN y2 = y1 + 0.025

         END
ENDCASE

new = FltArr(2,5)
new[0,*] = [x1, x1, x2, x2, x1]
new[1,*] = [y1, y2, y2, y1, y1]

   ; Modify the graphics objects.

self.theBackground->SetProperty, Data=new

xs = FSC_PlotWindow_Normalize([0,100], Position=[new[0,0]+0.05, new[0,2]-0.05]) > 10e-6
ys = FSC_PlotWindow_Normalize([-1,1], Position=[new[1,0]+0.05, new[1,1]-0.05]) > 10e-6

self.theXAxis->SetProperty, XCoord_Conv=xs, Location=[1000, new[1,0]+0.05, 0]
self.theYAxis->SetProperty, YCoord_Conv=ys, Location=[new[0,0]+.05, 1000, 0]
self.thePlot->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys
self.x1 = new[0,0]
self.x2 = new[0,2]
self.y1 = new[1,0]
self.y2 = new[1,1]
self.xlength = self.x2 - self.x1
self.ylength = self.y2 - self.y1

   ; Draw the view after resetting the transformation matrix.

self.theModel->Reset
self.theWindow->Draw, self.theView

END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW::CenterPlot, event

   ; Centers the plot in the window.

new = FltArr(2,5)
self.x1 = (1.0 - self.xlength) / 2.0
self.x2 = self.x1 + self.xlength
self.y1 = (1.0 - self.ylength) / 2.0
self.y2 = self.y1 + self.ylength

new[0,*] = [self.x1, self.x1, self.x2, self.x2, self.x1]
new[1,*] = [self.y1, self.y2, self.y2, self.y1, self.y1]

   ; Modify the graphics objects.

self.theBackground->SetProperty, Data=new

xs = FSC_PlotWindow_Normalize([0,100], Position=[new[0,0]+0.05, new[0,2]-0.05])
ys = FSC_PlotWindow_Normalize([-1,1], Position=[new[1,0]+0.05, new[1,1]-0.05])

self.theXAxis->SetProperty, XCoord_Conv=xs, Location=[1000, new[1,0]+0.05, 0]
self.theYAxis->SetProperty, YCoord_Conv=ys, Location=[new[0,0]+.05, 1000, 0]
self.thePlot->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys
self.x1 = new[0,0]
self.x2 = new[0,2]
self.y1 = new[1,0]
self.y2 = new[1,1]

   ; Draw the view after resetting the transformation matrix.

self.theModel->Reset
self.theWindow->Draw, self.theView

END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW::MovePlot, event

   ; Calculate movement in normalized coordinates. Update start coords.

deltax = (event.x - self.currentX) / self.xsize
deltay = (event.y - self.currentY) / self.ysize
self.currentX = event.x
self.currentY = event.y

   ; Translate the selected model.

self.theModel->Translate, deltax, deltay, 0

   ; Get the current transformation matrix.

matrix = self.theModel->GetCTM()

   ; Get the vertices of the polygon object.

self.theBackground->GetProperty, Data=vertices

   ; Apply the transformation matrix to the vertices.

new = FltArr(2,5)
FOR j=0,4 DO BEGIN
   v = [vertices[*, j], 0.0, 1.0]
   new[*, j] = (matrix ## v)[0:1]
ENDFOR

IF new[0,0] LT 0.0 THEN new[0,*] = [0.0, 0.0, self.xlength, self.xlength, 0.0]
IF new[0,2] GT 1.0 THEN new[0,*] = [1.0-self.xlength, 1.0-self.xlength, 1.0, 1.0, 1.0-self.xlength]
IF new[1,0] LT 0.0 THEN new[1,*] = [0.0, self.ylength, self.ylength, 0.0, 0.0]
IF new[1,1] GT 1.0 THEN new[1,*] = [1.0-self.ylength, 1.0, 1.0, 1.0-self.ylength, 1.0-self.ylength]

   ; Modify the graphics objects.

self.theBackground->SetProperty, Data=new

xs = FSC_PlotWindow_Normalize([0,100], Position=[new[0,0]+0.05, new[0,2]-0.05])
ys = FSC_PlotWindow_Normalize([-1,1], Position=[new[1,0]+0.05, new[1,1]-0.05])

self.theXAxis->SetProperty, XCoord_Conv=xs, Location=[1000, new[1,0]+0.05, 0]
self.theYAxis->SetProperty, YCoord_Conv=ys, Location=[new[0,0]+.05, 1000, 0]
self.thePlot->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys
self.x1 = new[0,0]
self.x2 = new[0,2]
self.y1 = new[1,0]
self.y2 = new[1,1]

   ; Draw the view after resetting the transformation matrix.

self.theModel->Reset
self.theWindow->Draw, self.theView

END ;----------------------------------------------------------------------------------



FUNCTION FSC_PLOTWINDOW::WhichButtonReleased, event

; Determines which button was used.

buttons = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']
RETURN, buttons[event.release]
END ;----------------------------------------------------------------------------------



FUNCTION FSC_PLOTWINDOW::WhichButtonPressed, event

; Determines which button was used.

buttons = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']
RETURN, buttons[event.press]
END ;----------------------------------------------------------------------------------



FUNCTION FSC_PLOTWINDOW::InTarget, x, y, DIRECTION=direction

; Given a location of the cursor in the window returns the
; target location and the direction the window should be
; resized in. Have to be within 2.5% of window edge to be
; in moveable target.

x = x / self.xsize
y = y / self.ysize

xtest = -1
IF x GE self.x1 AND x LE self.x1 + 0.025 THEN xtest = 0
IF x GT self.x1 + 0.025 AND x LT self.x2 - 0.025 THEN xtest = 1
IF x GE self.x2 - 0.025 AND x LE self.x2 THEN xtest = 2
ytest = -1
IF y GE self.y1 AND y LE self.y1 + 0.1 THEN ytest = 0
IF y GT self.y1 + 0.025 AND y LT self.y2 - 0.025 THEN ytest = 1
IF y GE self.y2 - 0.025 AND y LE self.y2 THEN ytest = 2

IF xtest EQ -1 OR ytest EQ -1 THEN BEGIN
   retVal = 'ORIGINAL'
   direction = 'NONE'
ENDIF
IF xtest EQ 0 AND ytest EQ  0 THEN BEGIN
   retVal = 'SIZE_SW'
   direction = 'SW'
ENDIF
IF xtest EQ 0 AND ytest EQ  1 THEN BEGIN
    retVal = 'SIZE_EW'
   direction = 'W'
ENDIF
IF xtest EQ 0 AND ytest EQ  2 THEN BEGIN
    retVal = 'SIZE_NW'
   direction = 'NW'
ENDIF
IF xtest EQ 1 AND ytest EQ  0 THEN BEGIN
    retVal = 'SIZE_NS'
   direction = 'S'
ENDIF
IF xtest EQ 1 AND ytest EQ  1 THEN BEGIN
    retVal = 'MOVE'
   direction = 'MOVE'
ENDIF
IF xtest EQ 1 AND ytest EQ  2 THEN BEGIN
    retVal = 'SIZE_NS'
   direction = 'N'
ENDIF
IF xtest EQ 2 AND ytest EQ  0 THEN BEGIN
    retVal = 'SIZE_SE'
   direction = 'SE'
ENDIF
IF xtest EQ 2 AND ytest EQ  1 THEN BEGIN
    retVal = 'SIZE_EW'
   direction = 'E'
ENDIF
IF xtest EQ 2 AND ytest EQ  2 THEN BEGIN
    retVal = 'SIZE_NE'
   direction = 'NE'
ENDIF

RETURN, retVal
END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW::Realize

Widget_Control, self.drawID, Get_Value=theWindow
self.theWindow = theWindow
END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW_Notify_Realize, drawID
Widget_Control, drawID, Get_UValue=self
self->Realize
END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW::Process_Events, event

; Handles draw widget events.

possibleEvents = ['DOWN', 'UP', 'MOTION', 'SCROLL', 'EXPOSE']
thisEvent = possibleEvents[event.type]

   ; Are we inside a target? If so, change cursor.

target = self->InTarget(event.x, event.y, Direction=direction)
self.theWindow->SetCurrentCursor, target


   ; What kind of event is this?

CASE thisEvent OF
   'EXPOSE': self.theWindow->Draw, self.theView
   'UP': BEGIN
         thisButton = self->WhichButtonReleased(event)
         Widget_Control, event.id, Clear_Events=1
         CASE self.action OF
            'MOVE': BEGIN
               self.currentX = -1
               self.currentY = -1
               self.action = ""
               self->GetWindowLocation, xsize, ysize, xoffset, yoffset
               IF self.event_pro NE "" THEN BEGIN
                  thisEvent = {ID:event.id, TOP:event.top, HANDLER:event.handler, $
                     xsize:xsize, ysize:ysize, xoffset:xoffset, yoffset:yoffset}
                  Call_Procedure, self.event_pro, thisEvent
               ENDIF
               ENDCASE
            'ORIGINAL':
            'RESIZE': BEGIN
               self.currentX = -1
               self.currentY = -1
               self.action = ""
               self.direction = "NONE"
               self->GetWindowLocation, xsize, ysize, xoffset, yoffset
               IF self.event_pro NE "" THEN BEGIN
                  thisEvent = {ID:event.id, TOP:event.top, HANDLER:event.handler, $
                     xsize:xsize, ysize:ysize, xoffset:xoffset, yoffset:yoffset}
                  Call_Procedure, self.event_pro, thisEvent
               ENDIF
               ENDCASE
            ELSE:
         ENDCASE
         ENDCASE
   'DOWN': BEGIN
         thisButton = self->WhichButtonPressed(event)
         IF thisButton EQ 'MIDDLE' THEN BEGIN
            self->CenterPlot, event
            self->GetWindowLocation, xsize, ysize, xoffset, yoffset
            IF self.event_pro NE "" THEN BEGIN
                  thisEvent = {ID:event.id, TOP:event.top, HANDLER:event.handler, $
                     xsize:xsize, ysize:ysize, xoffset:xoffset, yoffset:yoffset}
                  Call_Procedure, self.event_pro, thisEvent
            ENDIF
            RETURN
         ENDIF
         CASE target OF
            'ORIGINAL': RETURN
            'MOVE': BEGIN
               self.currentX = event.x
               self.currentY = event.y
               self.action = 'MOVE'
               ENDCASE
            ELSE: BEGIN
               self.currentX = event.x
               self.currentY = event.y
               self.action = 'RESIZE'
               self.direction = direction
               ENDCASE
         ENDCASE
         ENDCASE
   'MOTION': BEGIN
         CASE self.action OF
            'MOVE': self->MovePlot, event
            'RESIZE': self->Resize, event, self.direction
            ELSE:
         ENDCASE
         ENDCASE
ENDCASE

END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW_Events, event

; Widget event handler. Calls event handler method.

Widget_Control, event.id, Get_UValue=self
self->Process_Events, event
END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW_Kill_Notify, drawID

; Destroy the plot window object when the window is destroyed.

Widget_Control, drawID, Get_UValue=self
Obj_Destroy, self
END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW::CLEANUP
Obj_Destroy, self.theModel
Obj_Destroy, self.theWindow
Obj_Destroy, self.theView
Obj_Destroy, self.plotModel
END ;----------------------------------------------------------------------------------



FUNCTION FSC_PLOTWINDOW::INIT, $
   parent, $                  ; The parent base widget of this compound widget. Required.
   PageSize=pagesize, $       ; The "pagesize" of the widget. Possible values are: "LETTER", "LEDGER", "LEGAL", "A4", and "DISPLAY".
   WindowSize=windowsize, $   ; The size of the "window" on the page. A four-element array of normalized coordinates in the form [x0, y0, x1, y1].
   Event_Pro=event_pro, $     ; The event procedure for the widget.
   Units=units, $             ; A string indicating INCHES or CENTIMETER units. DEVICE units represented by a null string, "".
   UValue=uvalue, $           ; A user value for the caller of this program.
   Landscape=landscape, $     ; If set, display the page in landscape mode. Otherwise the page is display in portrait mode.
   Color=color, $             ; If set, display the window in "color". This is the default on 24-bit devices.
   Debug=debug, $             ; Set this keyword to turn traceback error handling on in the error handling code.
   WindowColor=windowColor    ; A three-element array specifying the background window color (RGB).

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PlotWindow_Error_Message(Traceback=Keyword_Set(debug))
   RETURN, 0
ENDIF

IF N_Elements(pagesize) EQ 0 THEN pageSize = 'LETTER'
IF N_Elements(windowsize) EQ 0 THEN windowsize = [0.2, 0.2, 0.8, 0.8]
IF N_Elements(event_pro) EQ 0 THEN event_pro = ""
IF N_Elements(units) EQ 0 THEN units=""
IF N_Elements(uvalue) EQ 0 THEN uvalue=""
IF N_Elements(landscape) EQ 0 THEN landscape = 0
Device, Get_Visual_Depth=theDepth
self.pixels_per_inch = 18
sizes = self->GetPagePixels(pageSize, Landscape=landscape)
xsize = sizes[0]
ysize = sizes[1]
IF N_Elements(windowColor) EQ 0 THEN $
   IF theDepth GT 8 THEN windowColor = [60, 140, 140] ELSE windowColor = [80, 80, 80]

IF N_Elements(color) EQ 0 THEN BEGIN
   Device, Get_Visual_Depth=theDepth
   IF theDepth GT 8 THEN color = 1 ELSE color = 0
ENDIF ELSE color = Keyword_Set(color)

x1 = windowsize[0]
x2 = windowsize[2]
y1 = windowsize[1]
y2 = windowsize[3]

data = Findgen(101)
data = Sin(data/5) / Exp(data/20)

self.thePlot = Obj_New('IDLgrPlot', data, Color=[0,0,0])
self.thePlot->SetProperty, XCoord_Conv=FSC_PlotWindow_Normalize([0,100], $
   Position=[x1+0.05, x2-0.05]) > 10e-6, $
   YCoord_Conv=FSC_PlotWindow_Normalize([-1,1], Position=[y1+0.05, y2-0.05]) > 10e-6

self.theXAxis = Obj_New('IDLgrAxis', 0, Color=[0,0,0], Ticklen=0.025, $
   Minor=4, Range=[0,100], Location=[1000, y1+0.05 , 0.0], /NoText)
self.theXAxis->SetProperty, XCoord_Conv=FSC_PlotWindow_Normalize([0,100], $
   Position=[x1+0.05, x2-0.05]) > 10e-6

self.theYAxis = Obj_New('IDLgrAxis', 1, Color=[0,0,0], Ticklen=0.025, $
   Minor=4, Range=[-1,1], Location=[x1+0.05, 1000, 0.0], /NoText)
self.theYAxis->SetProperty, YCoord_Conv=FSC_PlotWindow_Normalize([-1,1], $
   Position=[y1+0.05, y2-0.05]) > 10e-6

self.plotModel = Obj_New('IDLgrModel')

IF theDepth GT 8 THEN theColor = [210, 200, 180] ELSE theColor = [255,255,255]
self.theBackground = Obj_New('IDLgrPolygon', Color=theColor, $
   [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1])

self.theModel = Obj_New('IDLgrModel', Select_Target=1)
self.theModel->Add, self.theBackground

self.plotModel->Add, self.theXAxis
self.plotModel->Add, self.theYAxis
self.plotModel->Add, self.thePlot
self.plotModel->Translate, 0, 0, 0.1

self.theView = Obj_New('IDLgrView', Color=windowColor,  Viewplane_Rect=[0,0,1,1])
self.theView->Add, self.theModel
self.theView->Add, self.plotModel

self.base = Widget_Base(parent, UValue=uvalue)
self.drawID = Widget_Draw(self.base, Button_Events=1, Expose_Events=1, $
   Retain=0, Graphics_Level=2, XSize=xsize, YSize=ysize, $
   Event_Pro='FSC_PLOTWINDOW_Events', UValue=self, Renderer=1, $
   Kill_Notify='FSC_PLOTWINDOW_Kill_Notify', Motion_Events=1, $
   Notify_Realize='FSC_PLOTWINDOW_Notify_Realize')

self.pagesize = pagesize
self.landscape = Keyword_Set(landscape)
self.x1 = x1
self.x2 = x2
self.y1 = y1
self.y2 = y2
self.xsize = Float(xsize)
self.ysize = Float(ysize)
self.currentX = -1
self.currentY = -1
self.xlength = x2 - x1
self.ylength = y2 - y1
self.units = units
self.event_pro = event_pro
self.color = color
self->SetColor, color
RETURN, 1
END ;----------------------------------------------------------------------------------



PRO FSC_PLOTWINDOW__DEFINE

   struct = { FSC_PLOTWINDOW,          $ ; The object class name.
              action:"",               $ ; A flag to indicate whether you are moving or resizing the plot window.
              base: 0L,                $ ; The top-level base of the compound widget.
              color:0,                 $ ; A flag to indicate if the window is in "color" or not.
              currentX:0,              $ ; The starting X location for resizing.
              currentY:0,              $ ; The starting Y location for resizing.
              direction:"",            $ ; The direction of resizing movement.
              event_pro:"",            $ ; The name of an event handler to call.
              drawID:0L,               $ ; The draw widget identifier.
              pageSize:"",             $ ; A string to indicate the "type" of page: Letter, A4, etc.
              pixels_per_inch:0,       $ ; The pixel per inch scaling. 18 pixels/inch hardcoded.
              landscape: 0,            $ ; A flag to indicate the window should be in landscape mode.
              plotModel:Obj_New(),     $ ; The model for the plot and axes objects.
              theBackground:Obj_New(), $ ; The background filled polygon.
              thePlot:Obj_New(),       $ ; The plot object.
              theModel:Obj_New(),      $ ; The model for the background object.
              theView:Obj_New(),       $ ; The object view.
              theWindow:Obj_New(),     $ ; The object window.
              theXAxis:Obj_New(),      $ ; The X axis object.
              theYAxis:Obj_New(),      $ ; The Y axis object.
              units:"",                $ ; A string indicating INCHES or CENTIMETER units.
              x1:0.0,                  $ ; The x location of lower-left corner of plot window. (Normalized coordinates.)
              x2:0.0,                  $ ; The x location of upper-right corner of plot window. (Normalized coordinates.)
              xlength:0.0,             $ ; The length of the plot window in x. (Normalized coordinates.)
              xsize:0.0,               $ ; The current X size of the draw widget. (Device coordinates.)
              y1:0.0,                  $ ; The y location of lower-left corner of plot window. (Normalized coordinates.)
              y2:0.0,                  $ ; The yx location of upper-right corner of plot window. (Normalized coordinates.)
              ylength:0.0,             $ ; The length of the plot window in y. (Normalized coordinates.)
              ysize:0.0                $ ; The current X size of the draw widget. (Device coordinates.)
            }
END ;----------------------------------------------------------------------------------



FUNCTION FSC_PLOTWINDOW, parent, PageSize=pagesize, WindowSize=windowsize, $
   Event_Pro=event_pro, Units=units, UValue=uvalue, Landscape=landscape, $
   Color=color

; A wrapper function so this object can be treated as a compound
; widget function.

IF N_Elements(pagesize) EQ 0 THEN pageSize = 'LETTER'
IF N_Elements(windowsize) EQ 0 THEN windowsize = [0.15, 0.15, 0.85, 0.85]
IF N_Elements(event_pro) EQ 0 THEN event_pro = ""
IF N_Elements(units) EQ 0 THEN units=""
color = Keyword_Set(color)
landscape = Keyword_Set(landscape)
IF N_Elements(parent) EQ 0 THEN BEGIN
   ok = Dialog_Message('Parent parameter is required. Returning...')
   RETURN, Obj_New()
ENDIF

RETURN, Obj_New('FSC_PLOTWINDOW', parent, PageSize=pagesize, WindowSize=windowsize, $
   Event_Pro=event_pro, Units=units, UValue=uvalue, Landscape=landscape, Color=color)
END ;----------------------------------------------------------------------------------
