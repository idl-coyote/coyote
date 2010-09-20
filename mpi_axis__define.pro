;+
; NAME:
;       MPI_AXIS__DEFINE
;
; PURPOSE:
;
;       This is a compound widget program for interactively adjusting and keeping track
;       of keywords appropriate for configuing axis properties.
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
;       Graphics
;
; CALLING SEQUENCE:
;
;       xAxisObjext = Obj_New("MPI_AXIS", /XAxis)
;       xAxisID = xAxisObject->GUI(baseWidgetID)
;
; INPUT PARAMETERS:
;
;       None.
;
; INPUT KEYWORDS (Sent to the INIT method. The same keywords can be set with the SETPROPERTY method of the object.):
;
;       AUTOKEYWORDS - An anonymous struture of keywords that are passed to the AutoRange function.
;
;       AUTORANGE - The name of a function that can return axis range information as 2-element array.
;
;       CHARSIZE - The character size used for the axis. By default, 1.0.
;
;       EXACT - Set to indicate exact axis range scaling. (Can also be set with the STYLE keyword.)
;
;       EXTEND - Set to indicate extended axis range. (Can also be set with the STYLE keyword.)
;
;       GRIDSTYLE - The style used for drawing grid lines.
;
;       HIDE - Set to indicate hidden axis style. (Can also be set with the STYLE keyword.)
;
;       LOG - Set to indicate logarithmic axis.
;
;       MARGIN - The axis margin. (Currently unimplemented.)
;
;       MINOR - The number of minor tick marks between the major tick marks on the axis.
;
;       NAME = A user-defined "name" for the object.
;
;       NOBOX - Set to inhibit box-style axis. (Can also be set with the STYLE keyword.)
;
;       NOZERO - Set to indicate NO_ZERO axis style. (Can also be set with the STYLE keyword.)
;
;       RANGE - The axis range as a two-element array, [minrange, maxrange].
;
;       STYLE - The axis style. A 32-bit value whose bits select certain properties. See the
;          on-line documentation for the !X.STYLE system variable for more information. These
;          style properties can be set in a more natural way with other keywords.
;
;       THICK - The thickness of the axis. By default, 1.0.
;
;       TICKFORMAT - The format to use with tick marks. May be name of procedure.
;
;       TICKINTERVAL - The interval to space tick marks for first-level axis. (Currently not implemented.)
;
;       TICKLAYOUT - The type of tick layout desired. (Currently not implemented.)
;
;       TICKLEN - The length of the ticks on the axis. By default, 0.0. (Note that changing this
;          value will cause the Plot TICKLEN value to be ignored for the axis.)
;
;       TICKNAME - The string names associated with each tick mark. (Currently not implemented.)
;
;       TICKS - The number of major tick intervals.
;
;       TICKUNITS - The units to use for tick labeling. (Currently not implemented.)
;
;       TICKV - A vector of tick values. (Currently not implemented.)
;
;       TITLE - The axis title.
;
;       XAXIS - Set to indicate an X axis object. This is the default.
;
;       YAXIS - Set to indicate a Y axis object.
;
;       ZAXIS - Set to indicate a Z axis.
;
; METHOD PROCEDURES:
;
;      GUI - This procedure method displays a graphical user interface that allows the user
;            to change the axis configuration parameters.
;
;            PARAMETERS:
;
;                parent - The parent of the compound widget.
;
;            KEYWORDS:
;
;                EVENT_PRO - The specified event handler procedure.
;                EVENT_FUNC - The specified event handler function.
;                ONLY_STYLE - If set, display only style parameters in the GUI.
;                ONLY_TICK - If set, display only tick parameters in the GUI.
;                SHORT_FORM - Normally, all the axis properties are displayed in the GUI. Setting
;                     this keyword places the Tick and Style properties behind buttons on the interface.
;                UVALUE - The user value of the compound widget.
;
;      SETPROPERTY - This procedure can be used to set the properties of the axis
;               configuration object without using the graphical user interface. The
;               keywords are identical to those used in the INIT method, above.
;
; METHOD FUNCTIONS:
;
;      GETKEYWORDS - This function method contains no arguments or keywords. It returns a
;            structure, with fields equivalent to PLOT keywords for setting axis properties.
;            The idea is that these keywords can be passed directly to the PLOT command using
;            the keyword inheritance mechanism via the _EXTRA keyword to the plot command. ished with it.
;
; PROGRAM NOTES:
;
;      Required Programs: The following programs are required to reside in your !PATH. They can be
;         obtained from the Coyote Library:
;
;                     http://www.dfanning.com/programs/cw_spacer.pro
;                     http://www.dfanning.com/programs/error_message.pro
;                     http://www.dfanning.com/programs/fsc_droplist.pro
;                     http://www.dfanning.com/programs/fsc_field.pro
;
; EXAMPLE:
;
;       A heavily documented program, named MPI_PLOT, is supplied with this program.
;       This program not only explains how to use the MPI_PLOTCONFIG__DEFINE and the
;       MPI_AXIS__DEFINE programs, it can be used as a wrapper program for the PLOT command
;       that you can use with your own data. The program can be downloaded here:
;
;                     http://www.dfanning.com/programs/mpi_plot.pro
;
; MODIFICATION HISTORY:
;
;       Written by David Fanning, March 2001.
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
FUNCTION MPI_Axis_FindTLB, startID

; This function traces up the widget hierarcy to find the top-level base.


Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   RETURN, 0L
ENDIF

FORWARD_FUNCTION MPI_Axis_FindTLB

parent = Widget_Info(startID, /Parent)
IF parent EQ 0 THEN RETURN, startID ELSE parent = MPI_Axis_FindTLB(parent)
RETURN, parent
END ;----------------------------------------------------------------------------



PRO MPI_Axis_CenterTLB, tlb

; Center the top-level base on the display.

screenSize = Get_Screen_Size()
IF screenSize[0] GT 2000 THEN screenSize[0] = screenSize[0]/2 ; Dual monitors.
xCenter = screenSize(0) / 2
yCenter = screenSize(1) / 2

geom = Widget_Info(tlb, /Geometry)
xHalfSize = geom.Scr_XSize / 2
yHalfSize = geom.Scr_YSize / 2

Widget_Control, tlb, XOffset = xCenter-xHalfSize, $
   YOffset = yCenter-yHalfSize

END ;---------------------------------------------------------------------------



FUNCTION MPI_Axis_Error_Message, theMessage, Traceback=traceback, NoName=noName, _Extra=extra

On_Error, 2

   ; Check for presence and type of message.

IF N_Elements(theMessage) EQ 0 THEN theMessage = !Error_State.Msg
s = Size(theMessage)
messageType = s[s[0]+1]
IF messageType NE 7 THEN BEGIN
   Message, "The message parameter must be a string.", _Extra=extra
ENDIF

   ; Get the call stack and the calling routine's name.

Help, Calls=callStack
callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]

   ; Are widgets supported? Doesn't matter in IDL 5.3 and higher.

widgetsSupported = ((!D.Flags AND 65536L) NE 0) OR Float(!Version.Release) GE 5.3
IF widgetsSupported THEN BEGIN
   IF Keyword_Set(noName) THEN answer = Dialog_Message(theMessage, _Extra=extra) ELSE BEGIN
      IF StrUpCase(callingRoutine) EQ "$MAIN$" THEN answer = Dialog_Message(theMessage, _Extra=extra) ELSE $
         answer = Dialog_Message(StrUpCase(callingRoutine) + ": " + theMessage, _Extra=extra)
   ENDELSE
ENDIF ELSE BEGIN
      Message, theMessage, /Continue, /NoPrint, /NoName, /NoPrefix, _Extra=extra
      Print, '%' + callingRoutine + ': ' + theMessage
      answer = 'OK'
ENDELSE

   ; Provide traceback information if requested.

IF Keyword_Set(traceback) THEN BEGIN
   Help, /Last_Message, Output=traceback
   Print,''
   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
   Print, ''
   FOR j=0,N_Elements(traceback)-1 DO Print, "     " + traceback[j]
ENDIF

RETURN, answer
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO MPI_Axis::GetProperty, Name=name

   ; This method returns axis object properties. Currently used only
   ; to obtain the "name" of the object in a way that is consistent
   ; with other objects.

name = self.name
END ;-----------------------------------------------------------------------------------------------------------------------------




PRO MPI_Axis::ResizeTLB

; This method sizes the compound widgets TLB to the size of the parent.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN
ENDIF

top_tlb = MPI_Axis_FindTLB(self.parent)
Widget_Control, top_tlb, Update=0
pinfo = Widget_Info(self.parent, /Geometry)
Widget_Control, self.tlb, Scr_XSize=pinfo.Scr_XSize - 12
Widget_Control, top_tlb, Update=1
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::GetTLB

; This method returns the top-level base of the compound widget.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

RETURN, self.tlb
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::GetKeywords

; This method constructs a structure containing fields with values
; set to the current state of the object.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self->UpdateObject
struct = Create_Struct( $
   self.axistype + 'charsize', self.charsize, $
   self.axistype + 'gridstyle', 0 > self.gridstyle, $
   self.axistype + 'minor', self.minor, $
   self.axistype + 'range', self.range, $
   self.axistype + 'style', self.style, $
   self.axistype + 'thick', self.thick, $
   self.axistype + 'tickformat', self.tickformat, $
   self.axistype + 'ticklen', self.ticklen, $
   self.axistype + 'ticks', self.ticks, $
   self.axistype + 'title', self.title, $
   self.axistype + 'log', self.type )

RETURN, struct
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO MPI_Axis::UpdateObject

; This method simply gathers all the information in the graphical
; user interface (that doesn't update itself) and updates the self
; object. If fields are undefined, they are set to their default values.


   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN
ENDIF

IF Obj_Valid(self.lowRangeObj) NE 0 THEN BEGIN
   IF Finite(self.lowRangeObj->Get_Value()) EQ 0 THEN BEGIN
      value = 0.0
      self.lowRangeObj->Set_Value, value
   ENDIF ELSE value = self.lowRangeObj->Get_Value()
self.range[0] = value
ENDIF

IF Obj_Valid(self.highRangeObj) NE 0 THEN BEGIN
   IF Finite(self.highRangeObj->Get_Value()) EQ 0 THEN BEGIN
      value = 1.0
      self.highRangeObj->Set_Value, value
   ENDIF ELSE value = self.highRangeObj->Get_Value()
self.range[1] = value
ENDIF

IF Obj_Valid(self.titleObj) NE 0 THEN BEGIN
   IF N_Elements(self.titleObj->Get_Value()) EQ 0 THEN BEGIN
      value = ""
      self.titleObj->Set_Value, value
   ENDIF ELSE value = self.titleObj->Get_Value()
self.title = value
ENDIF

IF Obj_Valid(self.majorObj) NE 0 THEN BEGIN
   IF Finite(self.majorObj->Get_Value()) EQ 0 THEN BEGIN
      value = 0
      self.majorObj->Set_Value, value
   ENDIF ELSE value = self.majorObj->Get_Value()
self.ticks = value
ENDIF

IF Obj_Valid(self.minorObj) NE 0 THEN BEGIN
   IF Finite(self.minorObj->Get_Value()) EQ 0 THEN BEGIN
      value = 0
      self.minorObj->Set_Value, value
   ENDIF ELSE value = self.minorObj->Get_Value()
self.minor = value
ENDIF

IF Obj_Valid(self.ticklenObj) NE 0 THEN BEGIN
   IF Finite(self.ticklenObj->Get_Value()) EQ 0 THEN BEGIN
      value = 0.025
      self.ticklenObj->Set_Value, value
   ENDIF ELSE value = self.ticklenObj->Get_Value()
self.ticklen = value
ENDIF

IF Obj_Valid(self.ticksizeObj) NE 0 THEN BEGIN
   IF Finite(self.ticksizeObj->Get_Value()) EQ 0 THEN BEGIN
      value = 1.0
      self.ticksizeObj->Set_Value, value
   ENDIF ELSE value = self.ticksizeObj->Get_Value()
self.charsize = value
ENDIF

IF Obj_Valid(self.thickObj) NE 0 THEN BEGIN
   IF Finite(self.thickObj->Get_Value()) EQ 0 THEN BEGIN
      value = 1.0
      self.thickObj->Set_Value, value
   ENDIF ELSE value = self.thickObj->Get_Value()
self.thick = value
ENDIF

IF Obj_Valid(self.tickformatObj) NE 0 THEN BEGIN
   IF N_Elements(self.tickformatObj->Get_Value()) EQ 0 THEN BEGIN
      value = ""
      self.tickformatObj->Set_Value, value
   ENDIF ELSE value = self.tickformatObj->Get_Value()
self.tickformat = value
ENDIF

END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::CheckEvent, event

; This event handler method checks to see if an event
; should be passed on. The only way to get events passed
; on to another event handler is to have defined either
; an event hander procedure or an event handler function
; via the EVENT_PRO or EVENT_FUNC keywords, respectively.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

event.top = MPI_Axis_FindTLB(self.parent)
event.id = self.tlb
event.handler = 0L

   ; Make sure all values are current by updating.

self->UpdateObject

thisEvent = { MPI_AXIS_EVENT, $
              ID: self.tlb, $
              TOP: MPI_Axis_FindTLB(self.parent), $
              HANDLER: 0L, $
              SELF:self }

IF self.event_pro NE "" THEN BEGIN
   Call_Procedure, self.event_pro, thisEvent
   thisEvent = 0
ENDIF

IF self.event_func NE "" THEN BEGIN
   ok = Call_Function(self.event_func, thisEvent)
   thisEvent = 0
ENDIF

RETURN, 0
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::LowRange, event

; This event handler method sets the lower axis range.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

   ; Is the value defined? If not, return.

IF N_Elements(*event.value) EQ 0 THEN BEGIN
   self->UpdateObject
   RETURN, 0
ENDIF

self.range[0] = *event.value

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::HighRange, event

; This event handler method sets the upper axis range.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

   ; Is the value defined? If not, return.

IF N_Elements(*event.value) EQ 0 THEN BEGIN
   self->UpdateObject
   RETURN, 0
ENDIF

self.range[1] = *event.value

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::ResetRange, event

; This event handler method calls the autorange function, if available.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

IF N_Elements(*self.autokeywords) EQ 0 THEN BEGIN
   range = Call_Function(self.autorange)
ENDIF ELSE BEGIN
   range = Call_Function(self.autorange, _Extra=*self.autokeywords)
ENDELSE

self.range = range
IF Obj_Valid(self.lowRangeObj) THEN self.lowRangeObj->Set_Value, range[0]
IF Obj_Valid(self.highRangeObj) THEN self.highRangeObj->Set_Value, range[1]

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::AxisTitle, event

; This event handler method sets the axis title.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self.title = *event.value

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::AxisThickness, event

; This event handler method sets the axis thickness.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

   ; Is the value defined? If not, return.

IF N_Elements(*event.value) EQ 0 THEN BEGIN
   self->UpdateObject
   RETURN, 0
ENDIF

self.thick = *event.value
RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::MajorTicks, event

; This event handler method sets the major tick values.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

IF N_Elements(*event.value) EQ 0 THEN BEGIN
   self->UpdateObject
   RETURN, 0
ENDIF

self.ticks = *event.value

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::TickLength, event

; This event handler method sets the tick length.
; Confined between -0.1 and 1.0.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

   ; Is the value defined? If not, return.

IF N_Elements(*event.value) EQ 0 THEN BEGIN
   self->UpdateObject
   RETURN, 0
ENDIF

self.ticklen = -0.1 > *event.value < 1.0
IF *event.value LT -0.1 OR *event.value GT 1.0 THEN $
   event.object->Set_Value, self.ticklen

self.oldgridlen = self.ticklen

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::CharSize, event

; This event handler method sets the axis character size.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

   ; Is the value defined? If not, return.

IF N_Elements(*event.value) EQ 0 THEN BEGIN
   self->UpdateObject
   RETURN, 0
ENDIF

self.charsize = *event.value

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::MinorTicks, event

; This event handler method sets the minor tick values.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

   ; Is the value defined? If not, return.

IF N_Elements(*event.value) EQ 0 THEN BEGIN
   self->UpdateObject
   RETURN, 0
ENDIF

self.minor = *event.value

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::TickFormat, event

; This event handler method sets the tick format.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

theFormat = *event.value
IF *event.value EQ "" THEN RETURN, self->CheckEvent(event)

stringLen = StrLen(theFormat)
change = 0
IF StrMid(theFormat, 0, 1) NE "(" THEN BEGIN
   theFormat = "(" + theFormat
   stringLen = StrLen(theFormat)
   change = 1
ENDIF
IF StrMid(theFormat, stringLen-1, 1) NE ")" THEN BEGIN
   theFormat = theFormat + ")"
   change = 1
ENDIF
IF change THEN event.object->Set_Value, theFormat

self.tickformat = theFormat

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::GridStyle, event

; This event handler method sets the axis grid style.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

IF StrUpCase(*event.selection) EQ 'NONE' THEN BEGIN
   self.ticklen = self.oldgridlen
   self.ticklenObj->SetSensitive, 1
   self.ticklenObj->Set_Value, self.ticklen
ENDIF ELSE BEGIN
   self.oldgridlen = self.ticklenObj->Get_Value()
   self.ticklenObj->Set_Value, 1.0
   self.ticklen = 1.0
   self.ticklenObj->SetSensitive, 0
ENDELSE

self.gridstyle = event.index - 1
RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::SetExactStyle, event

; This event handler method sets the axis style characteristics.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

IF event.select THEN self.style = self.style + 1 ELSE self.style = self.style - 1
RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::SetExtendStyle, event

; This event handler method sets the axis style characteristics.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

IF event.select THEN self.style = self.style + 2 ELSE self.style = self.style - 2
RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::SetHideStyle, event

; This event handler method sets the axis style characteristics.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

IF event.select THEN self.style = self.style + 4 ELSE self.style = self.style - 4
RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::SetBoxStyle, event

; This event handler method sets the axis style characteristics.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

IF event.select THEN self.style = self.style + 8 ELSE self.style = self.style - 8
RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::SetZeroStyle, event

; This event handler method sets the axis style characteristics.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

IF event.select THEN self.style = self.style + 16 ELSE self.style = self.style - 16
RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::SetLogStyle, event

; This event handler method sets the axis style characteristics.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self.type = event.select

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::TickOptions, event

; This event handler method responds to the "Tick Options..." button.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

IF XRegistered(self.axistype +'-Axis Tick Properties') GT 0 THEN RETURN, 0

tlb = Widget_Base(Column=1, Frame=1, Title=self.axistype +'-Axis Tick Properties', $
   Group_Leader=self.tlb, Base_Align_Center=1)
   label = Widget_Label(tlb, Value=self.axistype +'-Axis Tick Properties');, Font=self.labelfont)
   containerbase = Widget_Base(tlb, Column=1, Frame=1)
   row1 = Widget_Base(containerbase, Row=1)
      majorID = FSC_Field(row1, Title='Major: ', Value=self.ticks, Labelsize=self.labelplus + 40, $
         UValue={Object:self, Method:'MAJORTICKS'}, /CR_Only, XSize=6, $
         Event_Func='MPI_AXIS_WIDGET_EVENTS', Object=majorObj, Digits=2, /Positive, /Label_Right)
      minorID = FSC_Field(row1, Title='Minor: ', Value=self.minor, /Positive, $
         UValue={Object:self, Method:'MINORTICKS'}, /CR_Only, XSize=6, Labelsize=self.labelplus + 55, $
         Event_Func='MPI_AXIS_WIDGET_EVENTS', Object=minorObj, Digits=2, /Label_Right)
   row2 = Widget_Base(containerbase, Row=1)
      ticklenID = FSC_Field(row2, Title='Length: ', Value=self.ticklen, Decimal=3, XSize=6, Labelsize=self.labelplus + 40, $
         UValue={Object:self, Method:'TICKLENGTH'}, /CR_Only, $
         Event_Func='MPI_AXIS_WIDGET_EVENTS', Object=ticklenObj, /Label_Right)
      ticksizeID = FSC_Field(row2, Title='Size: ', Value=self.charsize, $
         UValue={Object:self, Method:'CHARSIZE'}, /CR_Only, Decimal=2, XSize=6, Labelsize=self.labelplus + 55, $
         Event_Func='MPI_AXIS_WIDGET_EVENTS', Object=ticksizeObj, /Label_Right)
   row3 = Widget_Base(containerbase, Row=1)
      lenID = FSC_Field(row3, Title='Format: ', Value=self.tickformat, Labelsize=self.labelplus + 40, $
         UValue={Object:self, Method:'TICKFORMAT'}, /CR_Only, XSize=6, $
         Event_Func='MPI_AXIS_WIDGET_EVENTS', Object=tickformatObj, /Label_Right)
      thickID = FSC_Field(row3, Title='Thickness: ', Value=self.thick, Decimal=2, $
         UValue={Object:self, Method:'AXISTHICKNESS'}, /CR_Only, /Positive, Format='(F5.2)', $
         Event_Func='MPI_AXIS_WIDGET_EVENTS', Object=thickObj, XSize=6, Labelsize=self.labelplus + 55, /Label_Right)
   row4 = Widget_Base(containerbase, Row=1)
               styles = ['None','Solid', 'Dotted', 'Dashed', 'Dash Dot', 'Dash Dot Dot', 'Long Dash']
      gridStyleObj = FSC_Droplist(row4, Value=styles, Spaces=[0,1], Event_Func='MPI_AXIS_WIDGET_EVENTS', $
         Title='Grid Style: ', UValue={Object:self, Method:'GRIDSTYLE'}, Index=self.gridstyle+1)
      spacer = CW_Spacer(row4, 1)

         ; Leave this for implementation in future versions.

      ;button = Widget_Button(row4, Value='More...',UValue={Object:self, Method:'MORETICKSTUFF'}, $
         ;Event_Func='MPI_AXIS_WIDGET_EVENTS')

   buttonbase = Widget_Base(tlb, Row=1)
   button = Widget_Button(buttonbase, Value='Close', UValue={Object:self, Method:'DISMISSDIALOG'}, $
         Event_Func='MPI_AXIS_WIDGET_EVENTS')
   button = Widget_Button(buttonbase, Value='Accept', UValue={Object:self, Method:'ACCEPTTICKOPTIONS'}, $
         Event_Func='MPI_AXIS_WIDGET_EVENTS')

MPI_Axis_CenterTLB, tlb
Widget_Control, tlb, /Realize

IF Obj_Valid(majorObj) NE 0 THEN self.majorObj = majorObj
IF Obj_Valid(minorObj) NE 0 THEN self.minorObj = minorObj
IF Obj_Valid(ticklenObj) NE 0 THEN self.ticklenObj = ticklenObj
IF Obj_Valid(ticksizeObj) NE 0 THEN self.ticksizeObj = ticksizeObj
IF Obj_Valid(thickObj) NE 0 THEN self.thickObj = thickObj
IF Obj_Valid(tickformatObj) NE 0 THEN self.tickformatObj = tickformatObj
IF Obj_Valid(gridStyleObj) NE 0 THEN self.gridStyleObj = gridStyleObj

XManager,self.axistype +'-Axis Tick Properties', tlb, /No_Block
RETURN, 0
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::DismissDialog, event

; This event handler method simply destroys the top-level base of the dialog.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

Widget_Control, event.top, /Destroy

RETURN, 0
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::AcceptTickOptions, event

; This event handler method updates the object and destroys the top-level base.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self->UpdateObject
Widget_Control, event.top, /Destroy

   ; It is possible that someone may want to know
   ; that the widget has been updated. Call CHECKEVENT
   ; to find out.

ok = self->CheckEvent(event)
RETURN, 0
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::StyleOptions, event

; This event handler method responds to the "Style Options..." button.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

IF XRegistered(self.axistype +'-Axis Style Properties') GT 0 THEN RETURN, 0

tlb = Widget_Base(Column=1, Title=self.axistype +'-Axis Style Properties', $
   Group_Leader=self.tlb, Base_Align_Center=1)

    label = Widget_Label(tlb, Value=self.axistype +'-Axis Style Properties');, Font=self.labelfont)
    containerbase = Widget_Base(tlb, Column=1, Frame=1)

    stylebaseID = Widget_Base(containerbase, Column=1, /Nonexclusive)
      axisOnOffID = Widget_Button(stylebaseID, Value='Hide Axis', $
         UValue={Object:self, Method:'SETHIDESTYLE'}, $
         Event_Func='MPI_AXIS_WIDGET_EVENTS')
      IF (self.style AND 4) NE 0 THEN Widget_Control, axisOnOffID, /Set_Button
       exactID = Widget_Button(stylebaseID, Value='Exact Axis Range', $
          UValue={Object:self, Method:'SETEXACTSTYLE'}, $
          Event_Func='MPI_AXIS_WIDGET_EVENTS')
       IF (self.style AND 1) NE 0 THEN Widget_Control, exactID, /Set_Button
       extendID = Widget_Button(stylebaseID, Value='Extend Axis Range', $
          UValue={Object:self, Method:'SETEXTENDSTYLE'}, $
          Event_Func='MPI_AXIS_WIDGET_EVENTS')
       IF (self.style AND 2) NE 0 THEN Widget_Control, extendID, /Set_Button
       boxOnOffID = Widget_Button(stylebaseID, Value='Box Axis Off', $
          UValue={Object:self, Method:'SETBOXSTYLE'}, $
          Event_Func='MPI_AXIS_WIDGET_EVENTS')
       IF (self.style AND 8) NE 0 THEN Widget_Control, boxOnOffID, /Set_Button
       logID = Widget_Button(stylebaseID, Value='Logarithmic Axis', $
          UValue={Object:self, Method:'SETLOGSTYLE'}, $
          Event_Func='MPI_AXIS_WIDGET_EVENTS')
       IF self.type THEN Widget_Control, logID, /Set_Button
       IF self.axistype EQ 'Y' THEN BEGIN
          nozeroID = Widget_Button(stylebaseID, Value='Suppress Zero', $
             UValue={Object:self, Method:'SETZEROSTYLE'}, $
             Event_Func='MPI_AXIS_WIDGET_EVENTS')
          IF (self.style AND 16) NE 0 THEN Widget_Control, nozeroID, /Set_Button
       ENDIF

   buttonbase = Widget_Base(tlb, Row=1)
   button = Widget_Button(buttonbase, Value='Close', UValue={Object:self, Method:'DISMISSDIALOG'}, $
         Event_Func='MPI_AXIS_WIDGET_EVENTS')
   button = Widget_Button(buttonbase, Value='Accept', UValue={Object:self, Method:'DISMISSDIALOG'}, $
         Event_Func='MPI_AXIS_WIDGET_EVENTS')

MPI_Axis_CenterTLB, tlb
Widget_Control, tlb, /Realize

XManager, self.axistype +'-Axis Style Properties', tlb, /No_Block
RETURN, 0
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis_Widget_Events, event

; The main event handler for the compound widget. It reacts
; to "messages" in the UValue of the text widget.
; The message indicates which object method to call. A message
; consists of an object method and the self object reference.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = Dialog_Message('Specified functionality currently  NOT IMPLEMENTED.')
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

Widget_Control, event.ID, Get_UValue=theMessage
return_event = Call_Method(theMessage.method, theMessage.object, event)

RETURN, return_event
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::GUI, $
  parent, $                      ; The parent of the compound widget.
  Defaultfont=defaultfont, $     ; The name of a font to use as the default font.
  Event_Pro=event_pro, $         ; The specified event handler procedure.
  Event_Func=event_func, $       ; The specified event handler function.
  LabelFont=labelfont, $         ; The name of a font to use for program labels.
  Only_Style=only_style, $       ; A style-only presentation style.
  Only_Tick=only_tick, $         ; A tick-only presentation style.
  Short_Form=short_form, $       ; A short-form presentation style.
  UValue=uvalue                  ; The user value of the compound widget.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

IF N_Elements(parent) EQ 0 THEN Message, /NoName, "Parent widget identifier required."
IF N_Elements(event_pro) EQ 0 THEN event_pro = ""
IF N_Elements(event_func) EQ 0 THEN event_func = ""
IF N_Elements(uvalue) EQ 0 THEN uvalue = ""

   ; Check for GUI presentation style.

presentation = 'FULL'
IF Keyword_Set(only_style) THEN presentation = 'STYLE'
IF Keyword_Set(only_tick) THEN presentation = 'TICK'
IF Keyword_Set(short_form) THEN presentation = 'SHORT'

   ; Set widget font names.

;thisOS = StrUpCase(!Version.os_family)
;CASE thisOS OF
;   'WINDOWS': BEGIN
;      IF N_Elements(labelfont) EQ 0 THEN self.labelfont = 'Times*Bold'
;      IF N_Elements(defaultfont) EQ 0 THEN self.defaultfont = 'MS Sans Serif*10'
;   END
;   'MACOS': BEGIN
;      IF N_Elements(labelfont) EQ 0 THEN self.labelfont = 'Times*Bold'
;      IF N_Elements(defaultfont) EQ 0 THEN self.defaultfont = 'Times*10'
;   END
;   ELSE: BEGIN
;      IF N_Elements(labelfont) EQ 0 THEN self.labelfont = '-*-times-bold-r-*-*-12-*'
;      IF N_Elements(defaultfont) EQ 0 THEN self.defaultfont = '-*-times-medium-r-*-*-12-*'
;   END
;ENDCASE
;Widget_Control, Default_Font=self.defaultfont


tlb = Widget_Base(parent, $
   Column=1, $
   Base_Align_Center=1, $
   Pro_Set_Value='MPI_AXIS_SETVALUE', $
   Func_Get_Value='MPI_AXIS_GETVALUE', $
   Event_Func='MPI_AXIS_WIDGET_EVENTS', $
   Frame=1, $
   Notify_Realize='MPI_AXIS_NOTIFY_REALIZE', $
   UValue=uvalue)

;; Build widget depending on presentation style.

   IF presentation EQ 'FULL' OR presentation EQ 'SHORT' THEN BEGIN

      majorbase = Widget_Base(tlb, Column=1,  UValue=self, Base_Align_Center=1)
      label = Widget_Label(majorbase, Value=self.axistype + '-Axis');, Font=self.labelfont)
      layoutbase = Widget_Base(majorbase, Row=1)

      row2base = Widget_Base(layoutbase, Column=1)
      col1base = Widget_Base(row2base, Row=1)
         lowRangeID = FSC_Field(col1base, Title='Range: ', Value=self.range[0], $
            /Float, Object=lowRangeObj, UValue={Object:self, Method:'LOWRANGE'}, /CR_Only, $
            Event_Func='MPI_AXIS_WIDGET_EVENTS', XSize=10, Labelsize=self.labelplus + 63, /Label_Left)
         highRangeID = FSC_Field(col1base, Title=' ', Value=self.range[1], $
            /Float, Object=highRangeObj, UValue={Object:self, Method:'HIGHRANGE'}, /CR_Only, $
            Event_Func='MPI_AXIS_WIDGET_EVENTS', XSize=10, Labelsize=5, /Label_Left)

            ; Need Auto Range button if autorange function is available.

         IF self.autorange NE "" THEN resetrangeID = Widget_Button(col1base, Value='Reset range', $
           UValue={Object:self, Method:'RESETRANGE'})

         titleID = FSC_Field(row2base, Title='Axis Title: ', Value=self.title, $
            UValue={Object:self, Method:'AXISTITLE'}, /CR_Only, XSize=35, $
            Event_Func='MPI_AXIS_WIDGET_EVENTS', Object=titleObj, Labelsize=self.labelplus + 63, /Label_Left)

      ENDIF

      ;; Insert buttons for child dialogs for short style.

      IF presentation EQ 'SHORT' THEN BEGIN
         row3base = Widget_Base(layoutbase, Column=1)

         buttonbase = Widget_Base(majorbase, Row=1)
         tickoptionsID = Widget_Button(buttonbase, $
                                       Value=self.axistype + "-Axis Tick Options...", $
                                       UValue={Object:self, Method:'TICKOPTIONS'}, $
                                       Event_Func='MPI_AXIS_WIDGET_EVENTS')
         styleoptionsID = Widget_Button(buttonbase, $
                                        Value=self.axistype + "-Axis Style Options...", $
                                        UValue={Object:self, Method:'STYLEOPTIONS'}, $
                                        Event_Func='MPI_AXIS_WIDGET_EVENTS')

      ENDIF

      IF presentation NE 'SHORT' THEN $
         row4base = Widget_Base(tlb, Row=1, UValue=self)


      IF presentation EQ 'FULL' OR presentation EQ 'TICK' THEN BEGIN

         tickbase = Widget_Base(row4base, Column=1, Frame=1)
         labeltext = 'Tick Properties'
         IF presentation NE 'FULL' THEN labeltext = self.axistype +'-Axis '+labeltext
         label = Widget_Label(tickbase, $
                              Value=labeltext);, Font=self.labelfont)

         row1 = Widget_Base(tickbase, Row=1)
         majorID = FSC_Field(row1, Title='Major: ', $
                             Value=self.ticks, Labelsize=self.labelplus + 40, $
                             UValue={Object:self, Method:'MAJORTICKS'}, /CR_Only, XSize=6, $
                             Event_Func='MPI_AXIS_WIDGET_EVENTS', Object=majorObj, $
                             Digits=2, /Positive, /Label_Right)
         minorID = FSC_Field(row1, Title='Minor: ', Value=self.minor, /Positive, $
                             UValue={Object:self, Method:'MINORTICKS'}, /CR_Only, XSize=6, $
                             Labelsize=self.labelplus + 55, $
                             Event_Func='MPI_AXIS_WIDGET_EVENTS', Object=minorObj, $
                             Digits=2, /Label_Right)
         row2 = Widget_Base(tickbase, Row=1)
         ticklenID = FSC_Field(row2, Title='Length: ', Value=self.ticklen, Decimal=3, $
                               XSize=6, Labelsize=self.labelplus + 40, $
                               UValue={Object:self, Method:'TICKLENGTH'}, /CR_Only, $
                               Event_Func='MPI_AXIS_WIDGET_EVENTS', Object=ticklenObj, $
                               /Label_Right)

         ticksizeID = FSC_Field(row2, Title='Size: ', Value=self.charsize, $
                                UValue={Object:self, Method:'CHARSIZE'}, /CR_Only, $
                                Decimal=2, XSize=6, Labelsize=self.labelplus + 55, $
                                Event_Func='MPI_AXIS_WIDGET_EVENTS', Object=ticksizeObj, $
                                /Label_Right)
         row3 = Widget_Base(tickbase, Row=1)
         lenID = FSC_Field(row3, Title='Format: ', Value=self.tickformat, $
                           Labelsize=self.labelplus + 40, $
                           UValue={Object:self, Method:'TICKFORMAT'}, /CR_Only, XSize=6, $
                           Event_Func='MPI_AXIS_WIDGET_EVENTS', Object=tickformatObj, $
                           /Label_Right)
         thickID = FSC_Field(row3, Title='Thickness: ', Value=self.thick, Decimal=2, $
                             UValue={Object:self, Method:'AXISTHICKNESS'}, /CR_Only, $
                             /Positive, Format='(F5.2)', $
                             Event_Func='MPI_AXIS_WIDGET_EVENTS', Object=thickObj, XSize=6, $
                             Labelsize=self.labelplus + 55, /Label_Right)

         row4 = Widget_Base(tickbase, Row=1)
         styles = ['None', 'Solid', 'Dotted', 'Dashed', 'Dash Dot', $
                   'Dash Dot Dot', 'Long Dash']
         gridStyleObj = FSC_Droplist(row4, Value=styles, Spaces=[0,1], $
                                     Title='Grid Style: ', $
                                     UValue={Object:self, Method:'GRIDSTYLE'}, $
                                     Index=self.gridstyle+1)
         spacer = CW_Spacer(row4, 1)
         button = Widget_Button(row4, Value='More...', $
                                UValue={Object:self, Method:'MORETICKSTUFF'}, $
                                Event_Func='MPI_AXIS_WIDGET_EVENTS')

      ENDIF

      IF presentation EQ 'FULL' OR presentation EQ 'STYLE' THEN BEGIN
         stylebase = Widget_Base(row4base, Column=1, Frame=1)
         labeltext = 'Style Properties'
         IF presentation NE 'FULL' THEN labeltext = self.axistype +'-Axis '+labeltext
         label = Widget_Label(stylebase, Value=labeltext);, Font=self.labelfont)

         stylebaseID = Widget_Base(stylebase, Column=1, /Nonexclusive)
         axisOnOffID = Widget_Button(stylebaseID, Value='Hide Axis', $
                                     UValue={Object:self, Method:'SETHIDESTYLE'}, $
                                     Event_Func='MPI_AXIS_WIDGET_EVENTS')
         IF (self.style AND 4) NE 0 THEN Widget_Control, axisOnOffID, /Set_Button
         exactID = Widget_Button(stylebaseID, Value='Autoscale Axis', $
                                 UValue={Object:self, Method:'SETEXACTSTYLE'}, $
                                 Event_Func='MPI_AXIS_WIDGET_EVENTS')
         IF (self.style AND 1) EQ 0 THEN Widget_Control, exactID, /Set_Button
         extendID = Widget_Button(stylebaseID, Value='Extend Axis Range', $
                                  UValue={Object:self, Method:'SETEXTENDSTYLE'}, $
                                  Event_Func='MPI_AXIS_WIDGET_EVENTS')
         IF (self.style AND 2) NE 0 THEN Widget_Control, extendID, /Set_Button
         boxOnOffID = Widget_Button(stylebaseID, Value='Box Axis Off', $
                                    UValue={Object:self, Method:'SETBOXSTYLE'}, $
                                    Event_Func='MPI_AXIS_WIDGET_EVENTS')
         IF (self.style AND 8) NE 0 THEN Widget_Control, boxOnOffID, /Set_Button
         logID = Widget_Button(stylebaseID, Value='Logarithmic Axis', $
                               UValue={Object:self, Method:'SETLOGSTYLE'}, $
                               Event_Func='MPI_AXIS_WIDGET_EVENTS')
         IF self.type THEN Widget_Control, logID, /Set_Button
         IF self.axistype EQ 'Y' THEN BEGIN
            nozeroID = Widget_Button(stylebaseID, Value='Suppress Zero', $
                                     UValue={Object:self, Method:'SETZEROSTYLE'}, $
                                     Event_Func='MPI_AXIS_WIDGET_EVENTS')
            IF (self.style AND 16) NE 0 THEN Widget_Control, nozeroID, /Set_Button
         ENDIF
      ENDIF

   ; Populate object.

self.presentation = presentation
self.event_pro = event_pro
self.event_func = event_func
self.parent = parent
self.tlb = tlb
IF Obj_Valid(lowRangeObj) NE 0 THEN self.lowRangeObj = lowRangeObj
IF Obj_Valid(highRangeObj) NE 0 THEN self.highRangeObj = highRangeObj
IF Obj_Valid(titleObj) NE 0 THEN self.titleObj = titleObj
IF Obj_Valid(majorObj) NE 0 THEN self.majorObj = majorObj
IF Obj_Valid(minorObj) NE 0 THEN self.minorObj = minorObj
IF Obj_Valid(ticklenObj) NE 0 THEN self.ticklenObj = ticklenObj
IF Obj_Valid(ticksizeObj) NE 0 THEN self.ticksizeObj = ticksizeObj
IF Obj_Valid(thickObj) NE 0 THEN self.thickObj = thickObj
IF Obj_Valid(tickformatObj) NE 0 THEN self.tickformatObj = tickformatObj
IF Obj_Valid(gridStyleObj) NE 0 THEN self.gridStyleObj = gridStyleObj

RETURN, tlb
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO MPI_Axis::SetProperty, $
  AutoKeywords=autokeywords, $   ; An anonymous struture of keywords that are passed to the AutoRange function.
  AutoRange=autorange, $         ; The name of a function that can return axis range information as 2-element array.
  Charsize=charsize, $           ; The character size used for the axis. By default: 1.0.
  Defaultfont=defaultfont, $     ; The name of a font to use as the default font.
  Exact=exact, $                 ; Set to indicate exact axis range.
  Extend=extend, $               ; Set to indicate extended axis range.
  Gridstyle=gridstyle, $         ; The style used for drawing grid lines.
  Hide=hide, $                   ; Set to indicate hidden axis style.
  LabelFont=labelfont, $         ; The name of a font to use for program labels.
  Log=log, $                     ; The type (linear or logarithmic) of the axis.
  Margin=margin, $               ; The axis margin.
  Minor=minor, $                 ; The number of minor tick marks on the axis.
  Name=name, $                   ; A user-defined "name" for the object.
  NoBox=nobox, $                 ; Set to inhibit box-style axis.
  NoZero=nozero, $               ; Set to indicate NO_ZERO axis style.
  Range=range, $                 ; The axis range.
  Style=style, $                 ; The axis style.
  Thick=thick, $                 ; The thickness of the axis.
  Tickformat=tickformat, $       ; The format to use with tick marks. May be name of procedure.
  Tickinterval=tickinterval, $   ; The interval to space tick marks for first-level axis.
  Ticklayout=ticklayout, $       ; The type of tick layout desired.
  Ticklen=ticklen, $             ; The length of the ticks on the axis.
  Tickname=tickname, $           ; The string names associated with each tick mark.
  Ticks=ticks, $                 ; The number of major tick intervals.
  Tickunits=tickunits, $         ; The units to use for tick labeling.
  Tickv=tickv, $                 ; A vector of tick values.
  Title=title, $                 ; The axis title.
  XAxis=xaxis, $                 ; Set to indicate an X axis.
  YAxis=yaxis, $                 ; Set to indicate a Y axis.
  ZAxis=zaxis                    ; Set to indicate a Z axis.

; This method allows the user to set properties of the object.
; Note that no updating of the object occurs when the GUI method
; is applied.

   ; Check for keywords and parameters.

IF N_Elements(autokeywords) NE 0 THEN *self.autokeywords = autokeywords
IF N_Elements(autorange) NE 0 THEN self.autorange = autorange
IF N_Elements(charsize) NE 0 THEN self.charsize = charsize
IF N_Elements(defaultfont) NE 0 THEN self.defaultfont = defaultfont
IF N_Elements(gridstyle) NE 0 THEN self.gridstyle = -1 > gridstyle < 6
IF N_Elements(labelfont) NE 0 THEN self.labelfont = labelfont
IF N_Elements(log) NE 0 THEN self.type = Keyword_Set(log)
IF N_Elements(margin) NE 0 THEN self.margin = margin
IF N_Elements(minor) NE 0 THEN self.minor = minor
IF N_Elements(name) NE 0 THEN self.name = name
IF N_Elements(range) NE 0 THEN self.range = range
IF N_Elements(style) NE 0 THEN self.style = style
IF N_Elements(thick) NE 0 THEN self.thick = thick
IF N_Elements(tickformat) NE 0 THEN self.tickformat = tickformat
IF N_Elements(tickinterval) NE 0 THEN self.tickinterval = tickinterval
IF N_Elements(ticklayout) NE 0 THEN self.ticklayout = ticklayout
IF N_Elements(ticklen) NE 0 THEN self.ticklen = ticklen
IF N_Elements(tickname) NE 0 THEN *self.tickname = tickname
IF N_Elements(ticks) NE 0 THEN self.ticks = ticks
IF N_Elements(tickunits) NE 0 THEN *self.tickunits = tickunits
IF N_Elements(tickv) NE 0 THEN *self.tickv = tickv
IF N_Elements(title) NE 0 THEN self.title = title
IF Keyword_Set(xaxis) THEN self.axistype = 'X'
IF Keyword_Set(yaxis) THEN self.axistype = 'Y'
IF Keyword_Set(zaxis) THEN self.axistype = 'Z'


   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN
ENDIF

IF N_Elements(exact) NE 0 THEN BEGIN
   IF Keyword_Set(exact) THEN BEGIN
      IF (self.style AND 1) NE 1 THEN self.style = self.style + 1
   ENDIF ELSE BEGIN
      IF (self.style AND 1) EQ 1 THEN self.style = self.style - 1
   ENDELSE
ENDIF
IF N_Elements(extend) NE 0 THEN BEGIN
   IF Keyword_Set(extend) THEN BEGIN
      IF (self.style AND 2) NE 2 THEN self.style = self.style + 2
   ENDIF ELSE BEGIN
      IF (self.style AND 2) EQ 2 THEN self.style = self.style - 2
   ENDELSE
ENDIF
IF N_Elements(hide) NE 0 THEN BEGIN
   IF Keyword_Set(hide) THEN BEGIN
      IF (self.style AND 4) NE 4 THEN self.style = self.style + 4
   ENDIF ELSE BEGIN
      IF (self.style AND 4) EQ 4 THEN self.style = self.style - 4
   ENDELSE
ENDIF
IF N_Elements(nobox) NE 0 THEN BEGIN
   IF Keyword_Set(nobox) THEN BEGIN
      IF (self.style AND 8) NE 8 THEN self.style = self.style + 8
   ENDIF ELSE BEGIN
      IF (self.style AND 8) EQ 8 THEN self.style = self.style - 8
   ENDELSE
ENDIF
IF N_Elements(nozero) NE 0 THEN BEGIN
   IF Keyword_Set(nozero) THEN BEGIN
      IF (self.style AND 16) NE 16 THEN self.style = self.style + 16
   ENDIF ELSE BEGIN
      IF (self.style AND 16) EQ 16 THEN self.style = self.style - 16
   ENDELSE
ENDIF

END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis::Init, $

  AutoKeywords=autokeywords, $   ; An anonymous struture of keywords that are passed to the AutoRange function.
  AutoRange=autorange, $         ; The name of a function that can return axis range information as 2-element array.
  Charsize=charsize, $           ; The character size used for the axis. By default: 1.0.
  Exact=exact, $                 ; Set to indicate exact axis range.
  Extend=extend, $               ; Set to indicate extended axis range.
  Gridstyle=gridstyle, $         ; The style used for drawing grid lines.
  Hide=hide, $                   ; Set to indicate hidden axis style.
  Log=log, $                     ; The type (linear or logarithmic) of the axis.
  Margin=margin, $               ; The axis margin.
  Minor=minor, $                 ; The number of minor tick marks on the axis.
  Name=name, $                   ; A user-defined "name" for the object.
  NoBox=nobox, $                 ; Set to inhibit box-style axis.
  NoZero=nozero, $               ; Set to indicate NO_ZERO axis style.
  Range=range, $                 ; The axis range.
  Style=style, $                 ; The axis style.
  Thick=thick, $                 ; The thickness of the axis.
  Tickformat=tickformat, $       ; The format to use with tick marks. May be name of procedure.
  Tickinterval=tickinterval, $   ; The interval to space tick marks for first-level axis.
  Ticklayout=ticklayout, $       ; The type of tick layout desired.
  Ticklen=ticklen, $             ; The length of the ticks on the axis.
  Tickname=tickname, $           ; The string names associated with each tick mark.
  Ticks=ticks, $                 ; The number of major tick intervals.
  Tickunits=tickunits, $         ; The units to use for tick labeling.
  Tickv=tickv, $                 ; A vector of tick values.
  Title=title, $                 ; The axis title.
  XAxis=xaxis, $                 ; Set to indicate an X axis.
  YAxis=yaxis, $                 ; Set to indicate a Y axis.
  ZAxis=zaxis                    ; Set to indicate a Z axis.


; This method performs the initialization of the object.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_Axis_Error_Message(/Traceback)
   RETURN, 0
ENDIF

   ; Check for keywords and parameters.

IF N_Elements(autorange) EQ 0 THEN autorange = ""
IF N_Elements(charsize) EQ 0 THEN BEGIN
   IF !P.Charsize GT 0 THEN charsize = !P.Charsize ELSE charsize = 1.0
ENDIF
IF N_Elements(gridstyle) EQ 0 THEN gridstyle = -1 ELSE gridstyle = -1 > gridstyle < 6
log = Keyword_Set(log)
IF N_Elements(minor) EQ 0 THEN minor=0
IF N_Elements(name) EQ 0 THEN name = ""

   ; What kind of axis is this?

axistype = "X"
IF Keyword_Set(xaxis) THEN axistype = 'X'
IF Keyword_Set(yaxis) THEN axistype = 'Y'
IF Keyword_Set(zaxis) THEN axistype = 'Z'
IF N_Elements(title) EQ 0 THEN title = axistype + '-Axis'

IF N_Elements(range) EQ 0 THEN range = [0.0,1.0]
IF N_Elements(delta) EQ 0 THEN delta = (range[1] - range[0]) / 100.0

   ; Check for axis style parameters.

IF N_Elements(style) EQ 0 THEN BEGIN
   style = 0
   IF Keyword_Set(exact) THEN style = style + 1
   IF Keyword_Set(extend) THEN style = style + 2
   IF Keyword_Set(hide) THEN style = style + 4
   IF Keyword_Set(nobox) THEN style = style + 8
   IF Keyword_Set(nozero) THEN style = style + 16
ENDIF

   ; Set the style parameters from the other keywords.

IF N_Elements(exact) NE 0 THEN BEGIN
   IF Keyword_Set(exact) THEN BEGIN
      IF (self.style AND 1) NE 1 THEN self.style = self.style + 1
   ENDIF ELSE BEGIN
      IF (self.style AND 1) EQ 1 THEN self.style = self.style - 1
   ENDELSE
ENDIF
IF N_Elements(extend) NE 0 THEN BEGIN
   IF Keyword_Set(extend) THEN BEGIN
      IF (self.style AND 2) NE 2 THEN self.style = self.style + 2
   ENDIF ELSE BEGIN
      IF (self.style AND 2) EQ 2 THEN self.style = self.style - 2
   ENDELSE
ENDIF
IF N_Elements(hide) NE 0 THEN BEGIN
   IF Keyword_Set(hide) THEN BEGIN
      IF (self.style AND 4) NE 4 THEN self.style = self.style + 4
   ENDIF ELSE BEGIN
      IF (self.style AND 4) EQ 4 THEN self.style = self.style - 4
   ENDELSE
ENDIF
IF N_Elements(nobox) NE 0 THEN BEGIN
   IF Keyword_Set(nobox) THEN BEGIN
      IF (self.style AND 8) NE 8 THEN self.style = self.style + 8
   ENDIF ELSE BEGIN
      IF (self.style AND 8) EQ 8 THEN self.style = self.style - 8
   ENDELSE
ENDIF
IF N_Elements(nozero) NE 0 THEN BEGIN
   IF Keyword_Set(nozero) THEN BEGIN
      IF (self.style AND 16) NE 16 THEN self.style = self.style + 16
   ENDIF ELSE BEGIN
      IF (self.style AND 16) EQ 16 THEN self.style = self.style - 16
   ENDELSE
ENDIF

IF N_Elements(thick) EQ 0 THEN thick = 1.0
IF N_Elements(tickformat) EQ 0 THEN tickformat = "" ELSE BEGIN
   stringLen = StrLen(tickformat)
   IF StrMid(tickformat, 0, 1) NE "(" THEN BEGIN
      tickformat = "(" + tickformat
      stringLen = StrLen(tickformat)
   ENDIF
   IF StrMid(tickformat, stringLen-1, 1) NE ")" THEN BEGIN
      tickformat = tickformat + ")"
   ENDIF
ENDELSE
IF N_Elements(tickinterval) EQ 0 THEN tickinterval = 0
IF N_Elements(ticklayout) EQ 0 THEN ticklayout = 0
IF N_Elements(ticklen) EQ 0 THEN ticklen = 0.0
IF N_Elements(tickname) EQ 0 THEN tickname = StrArr(60)
IF N_Elements(ticks) EQ 0 THEN ticks = 0
IF N_Elements(tickunits) EQ 0 THEN tickunits = StrArr(10)
IF N_Elements(tickv) EQ 0 THEN tickv = DblArr(60)

   ; Populate the object.

self.autokeywords = Ptr_New(autokeywords)
self.autorange = autorange
self.charsize = charsize
self.gridstyle = gridstyle
self.type = log
self.minor = minor
self.range = range
self.style = style
self.thick = thick
self.tickformat = tickformat
self.tickinterval = tickinterval
self.ticklayout = ticklayout
self.ticklen = ticklen
self.tickname = Ptr_New(tickname)
self.ticks = ticks
self.tickunits = Ptr_New(tickunits)
self.tickv = Ptr_New(tickv)
self.title = title
self.type = log

self.axistype = axistype
self.name = name

CASE StrUpCase(!Version.OS_Family) OF
   'WINDOWS': self.labelplus = 0
   'MACOS': self.labelplus = 0
   ELSE: self.labelplus = 10
ENDCASE

RETURN, 1
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO MPI_Axis::Cleanup

; This method performs cleanup for the object.

Ptr_Free, self.tickname
Ptr_Free, self.tickunits
Ptr_Free, self.tickv
Ptr_Free, self.autokeywords
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO MPI_Axis_Notify_Realize, tlb
childID = Widget_Info(tlb, /Child)
Widget_Control, childID, Get_UValue=self
Widget_Control, tlb, Update=0
self->ResizeTLB
Widget_Control, tlb, Update=1
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_Axis_GetValue, id

; The is the function that returns the "value" of the compound widget.
; In this case, the "value" is the current state structure.

child = Widget_Info(id, /Child)
Widget_Control, child, Get_UValue=self
self->UpdateObject
stateStruct = self->GetKeywords()
RETURN, stateStruct
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO MPI_Axis_SetValue, value

; This is the procedure that sets the "value" of the compound widget.
; At the moment, I don't know what it would mean to "set" the value,
; so I am leaving this unimplemented.

ok = Dialog_Message('Setting the value of the compound widget is undefined. Returning...')
END ;-----------------------------------------------------------------------------------------------------------------------------


PRO MPI_Axis__Define

; This procedure defines the MPE_AXIS object class.

   struct = { MPI_AXIS, $                ; The object class name.

                ; The following fields refer to actual ![XYZ] values.

              charsize: 0.0, $           ; The character size used for the axis. By default: 1.0.
              gridstyle: 0L, $           ; The style used for drawing grid lines.
              margin: LonArr(2), $       ; The axis margin.
              minor: 0L, $               ; The number of minor tick marks on the axis.
              range: DblArr(2), $        ; The axis range.
              style: 0, $                ; The axis style.
              thick: 0.0, $              ; The thickness of the axis.
              tickformat: "", $          ; The format to use with tick marks. May be name of procedure.
              tickinterval: 0L, $        ; The interval to space tick marks for first-level axis.
              ticklayout: 0, $           ; The type of tick layout desired.
              ticklen: 0.0, $            ; The length of the ticks on the axis.
              tickname: Ptr_New(), $     ; The string names associated with each tick mark.
              ticks: 0, $                ; The number of major tick intervals.
              tickunits: Ptr_New(), $    ; The units to use for tick labeling.
              tickv: Ptr_New(), $        ; A vector of tick values.
              title: "", $               ; The axis title.
              type: 0L, $                ; The axis type. 0 is normal, 1 is log.

                ; The following fields refer to values needed to manipulate the object.

              autokeywords: Ptr_New(), $ ; An anonymous structure of keywords to pass to the AutoRange function.
              autorange: "", $           ; The name of a function to return axis range as two-element array.
              name: "", $                ; A user-defined "name" for the object.
              axistype: "", $            ; Indicates orientation of axis (i.e, X, Y, or Z).
              presentation: "", $        ; A switch that indicates which values to return to user.
              oldgridlen: 0.0, $         ; Storage for the old tick length when grids turned on.
              defaultfont: "", $         ; The default widget font.
              labelfont: "", $           ; The label widget font.
              labelplus: 0L, $           ; A fudge factor for increasing the label sizes on non-windows machines.

               ; The following are fields needed for widget actions.

              event_pro: "", $           ; The event handler procedure name.
              event_func: "", $          ; The event handler function name.

               ; The following fields are widget identifiers.

              tlb: 0L, $                 ; The top-level base of the compound widget.
              parent: 0L, $              ; The parent of the top-level base.
              axisOnOffID: 0L, $         ; The axis ON/OFF button style widget identifier.
              exactID: 0L, $             ; The exact axis style widget identifier.
              extendID: 0L, $            ; The extended axis style widget identifier.
              boxOnOffID: 0L, $          ; The box ON/OFF axis style widget identifier.
              logID: 0L, $               ; The logarithmic axis style widget identifier.
              nozeroID: 0L, $            ; The Y no zero axis style widget identifier.
              lowrangeObj: Obj_New(), $  ; The low range object reference.
              highrangeObj: Obj_New(), $ ; The high range object reference.
              titleObj: Obj_New(), $     ; The title object reference.
              majorObj: Obj_New(), $     ; The major tick interval object reference.
              minorObj: Obj_New(), $     ; The minor ticks object reference.
              ticklenObj: Obj_New(), $   ; The tick length object reference.
              ticksizeObj: Obj_New(), $  ; The tick size object reference.
              thickObj: Obj_New(), $     ; The thickness object reference.
              tickformatObj: Obj_New(), $; The tick format object reference.
              gridstyleObj: Obj_New() $  ; The gridstyle object reference.
             }
END ;-----------------------------------------------------------------------------------------------------------------------------



