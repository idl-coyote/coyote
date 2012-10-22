;+
; NAME:
;       CW_DRAWCOLOR
;
; PURPOSE:
;
;       This compound widget is used to place a label or color name next
;       to a color patch. Clicking on the color patch allows the user
;       to select another color
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
;       Graphics
;
; CALLING SEQUENCE:
;
;       colorpatchID = CW_DrawColor(parent)
;
; REQUIRED INPUTS:
;
;       parent - The identifier of a parent base widget.
;
; OUTPUTS:
;
;       colorpatchID - The widget identifier of the top-level base of this compound widget
;
; INPUT KEYWORDS:
;
;   COLOR - The name of the color to be displayed. Color names come from cgPickColorName.
;   COLUMN - Set this keyword to stack widgets in a column. Default is in a row.
;   EVENT_FUNC - The name of an event handler function for this compound widget.
;   EVENT_PRO -The name of an event handler procedure for this compound widget.
;   INDEX - An index number where the color should be loaded. !D.Table_Size-2, by default.
;   FILENAME - An optional input to cgPickColorName specifying different
;              colors.  See the cgPickColorName documenation for the file format.
;   LABEL_LEFT - Set this keyword to have the label text aligned on the left of the label. Default is to center.
;   LABEL_RIGHT - Set this keyword to have the label text aligned on the right of the label. Default is to center.
;   LABELSIZE - This is the X size of the label widget (containing the label) in device coordinates. Default is natural size.
;   LABELTEXT - This is the text on the label. Example, "Background Color", etc.
;   TITLE - This is the title on the cgPickColorName program that allows the user to select another color.
;   UVALUE - A user value for the widget.
;   XSIZE - The xsize (in pixel units) of the color patch. By default, 20.
;   YSIZE - The xsize (in pixel units) of the color patch. By default, 20.
;
; OUTPUT KEYWORDS:
;
;   OBJECT - The object reference. Use this to call methods, etc.
;
; OBJECT PROCEDURE METHODS:
;
;   Set_Value -- this method takes one argument, the new color name.
;               It will change the color of the widget if it has
;               already been realized.
;
;   Get_Value -- this method returns the color name the widget is displaying
;
; DEPENDENCIES:
;
;       Reqires cgColor and cgPickColorName from the Coyote Library:
;
;                     http://www.idlcoyote.com/programs/cgColor.pro
;                     http://www.idlcoyote.com/programs/cgPickColorName.pro
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, March 2001.
;       Fixed a problem with self object cleanup. 7 March 2006. DWF.
;       Allow addition to already realized widget hierarchies, October 2007. L. Anderson.
;       Added set_value and get_value methods to the widget can be
;            updated after being realized. October 2007. L. Anderson.
;       Added option to pass filename on to cgPickColorName. October
;            2007. L. Anderson
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
FUNCTION CW_Drawcolor_FindTLB, startID

; This function traces up the widget hierarcy to find the top-level base.


Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   RETURN, 0L
ENDIF

FORWARD_FUNCTION CW_Drawcolor_FindTLB

parent = Widget_Info(startID, /Parent)
IF parent EQ 0 THEN RETURN, startID ELSE parent = CW_Drawcolor_FindTLB(parent)
RETURN, parent
END ;----------------------------------------------------------------------------



FUNCTION CW_Drawcolor_Error_Message, theMessage, Traceback=traceback, NoName=noName, _Extra=extra

;  The standard event handler function.

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
callingRoutine = (Str_Sep(StrCompress(callStack[1])," "))[0]

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



FUNCTION CW_DrawColor::GetTLB

; This method function returns the identifier of the top-level base widget.

RETURN, self.tlb
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO CW_DrawColor::Realize

; This method initializes the draw widget and fills it with a color.

Widget_Control, self.drawID, Get_Value=wid
self.wid = wid
currentwindow = !D.Window
WSet, wid
PolyFill, [0, 0, 1, 1, 0], [0, 1, 1, 0, 0], /Normal, Color=cgColor(self.color, self.index)
IF currentwindow NE -1 THEN WSet, currentwindow
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO CW_DrawColor_Realize, drawID
; This procedure initializes the compound widget when it is realized.

Widget_Control, drawID, Get_UValue=selfObj
selfObj->Realize
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION CW_DrawColor_Pickcolor, event

; This event handler responds to events in the color patch.

   ; Only interested in button down events.

IF event.type NE 0 THEN RETURN, 0

   ; Get a new color. The event structure will be modified by the
   ; PickColor method.

Widget_Control, event.id, Get_UValue=selfObj
event = selfObj->PickColor()

   ; Return the modified event structure.

RETURN, event
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION CW_DrawColor::Pickcolor

; This object method allows the user to pick another color.
; Upon successfully picking the color, an event is sent to
; a specified event handler, or it is simply passed up the
; widget hierarchy in the normal fashion.

   ; Get the current color.

currentColor = self.color

   ; Allow the user to pick another color name.
IF self.filename NE "" THEN BEGIN
    self.color = cgPickColorName(self.color, Group_Leader=self.tlb, Title=self.title, Filename = self.filename)
ENDIF ELSE BEGIN
    self.color = cgPickColorName(self.color, Group_Leader=self.tlb, Title=self.title)
ENDELSE
IF currentColor EQ self.color THEN RETURN, 0

   ; Create an event structure for this compound widget.

event = { CW_DRAWCOLOR_EVENT, $
          ID: self.tlb, $
          TOP: CW_Drawcolor_FindTLB(self.parent), $
          HANDLER: 0L, $
          COLOR: self.color, $
          INDEX: self.index }

   ; Load the new color in the color patch.

WSet, self.wid
PolyFill, [0,0,1,1,0], [0,1,1,0,0], /Normal, Color=cgColor(self.color, self.index)

   ; If an event handler is specified, call it.

IF self.event_pro NE "" THEN BEGIN
   Call_Procedure, self.event_pro, event
   event = 0
ENDIF

IF self.event_func NE "" THEN BEGIN
   ok = Call_Function(self.event_func, event)
   event = 0
ENDIF

RETURN, event
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO CW_DrawColor_Set_Value, id, newcolor

   ; Get self reference

drawID = Widget_Info(id, /child)
Widget_Control, drawID, Get_UValue = self

   ; Call method

Call_Method, 'Set_Value', self, newcolor

END ;-----------------------------------------------------------------------------------------------------------------------------



PRO CW_DrawColor::Set_Value, newcolor

   ; Check if the color is defined in the cgColor program

IF N_Elements(newcolor) EQ 0 THEN Message, /NoName, 'Please pass a color name.'
newcolor = StrCompress(StrTrim(newcolor, 2))
IF where(StrUpCase(newcolor) EQ StrUpCase(cgColor(/Names))) EQ -1 THEN newColor = 'White'

   ; Store the value

self.color = newColor

   ; Load the new color in the color patch.

realized = Widget_Info(self.tlb, /Realize) ; Check if realized already
IF realized THEN BEGIN
    WSet, self.wid
    PolyFill, [0,0,1,1,0], [0,1,1,0,0], /Normal, Color=cgColor(self.color, self.index)
ENDIF

END
;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION CW_DrawColor_Get_Value, id

   ; Get self reference

drawID = Widget_Info(id, /child)
Widget_Control, drawID, Get_UValue = self

   ; Call method

RETURN, Call_Method('Get_Value', self)

END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION CW_DrawColor::Get_Value

RETURN, self.color
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION CW_DRAWCOLOR::Init, $

; The INIT method for the object. Builds the widget hierarchy.

   parent, $                   ; The parent widget identifier for this compound widget.
   Color=color, $              ; The name of the color to be displayed. Color names come from cgPickColorName.
   Column=column, $            ; Set this keyword to stack widgets in a column. Default is in a row.
   Event_Func=event_func, $    ; The name of an event handler function for this compound widget.
   Event_Pro=event_pro, $      ; The name of an event handler procedure for this compound widget.
   Filename=filename, $        ; An optional input to cgPickColorName specifying different colors.
   Index=index, $              ; An index number where the color should be loaded. !D.Table_Size-2, by default.
   Label_Left=label_left, $    ; Set this keyword to have the label text aligned on the left of the label. Default is to center.
   Label_Right=label_right, $  ; Set this keyword to have the label text aligned on the right of the label. Default is to center.
   LabelSize=labelsize, $      ; This is the X size of the label widget (containing the label) in device coordinates. Default is natural size.
   LabelText=label, $          ; This is the text on the label. Example, "Background Color", etc.
   Title=title, $              ; This is the title on the cgPickColorName program that allows the user to select another color.
   UValue=uvalue, $            ; A user value for the widget.
   XSize=xsize, $              ; The xsize (in pixel units) of the color patch. By default, 20.
   YSize=ysize                 ; The xsize (in pixel units) of the color patch. By default, 20.

   ; Error handling.
Catch, theError
IF theError NE 0 THEN BEGIN
   ok = CW_Drawcolor_Error_Message(/Traceback)
   RETURN, 0
ENDIF

   ; Check keywords and parameters.

IF N_Elements(parent) EQ 0 THEN Message, /NoName, 'Parent widget ID required.'
IF N_Elements(color) EQ 0 THEN color = 'WHITE'
IF N_Elements(event_func) EQ 0 THEN event_func = ""
IF N_Elements(event_pro) EQ 0 THEN event_pro = ""
IF N_Elements(index) EQ 0 THEN index = !D.Table_Size-2
IF N_Elements(filename) EQ 0 THEN filename = ""
IF N_Elements(label) EQ 0 THEN label = "Color"
IF N_Elements(labelsize) EQ 0 THEN labelsize = 0
IF N_Elements(title) EQ 0 THEN title=""
IF N_Elements(uvalue) EQ 0 THEN uvalue = ""
IF N_Elements(xsize) EQ 0 THEN xsize = 20
IF N_Elements(ysize) EQ 0 THEN ysize = 20
label_left = Keyword_Set(label_left)
label_right = Keyword_Set(label_right)
column = Keyword_Set(column)
IF column THEN row = 0 ELSE row = 1

   ; Create the widgets.

tlb = Widget_Base(parent, $
   Pro_Set_Value='CW_Drawcolor_Set_Value', $
   Func_Get_Value='CW_Drawcolor_Get_Value', $
   UValue=uvalue, $
   Base_Align_Center=1 )

base = Widget_Base(tlb, Row=row, Column=column, UValue=self, $
   Base_Align_Center=1)

labelID = Widget_Label(base, Value=label, Scr_XSize=labelsize, $
   Align_Left=label_left, Align_Right=label_right)

drawID = Widget_Draw(base, XSize=xsize, YSize=xsize, Kill_Notify='CW_Drawcolor_Kill_Notify', $
                     UValue=self, Event_Func='CW_Drawcolor_PickColor', Button_Events=1, /Align_Center)

   ; Populate the object.

self.parent = parent
self.tlb = tlb
self.labelID = labelID
self.drawID = drawid
self.color = color
self.index = index
self.filename = filename
self.title = title

; Adding a notify_realize link here allows the block of code above to
; run, regardless of whether the parent is already realized
Widget_Control, drawID, Notify_Realize='CW_Drawcolor_Realize'

; Have to realize widget forcibly if parent is already realized
IF Widget_Info(parent, /Realized) THEN CW_Drawcolor_Realize, drawID

RETURN, 1
END ;-------------------------------------------------------------------------------------



PRO CW_DRAWCOLOR_Kill_Notify, drawID

; Come here when draw widget dies. Get UValue (self) and destroy object.
Widget_Control, drawID, Get_UValue=self
Obj_Destroy, self

END ;-------------------------------------------------------------------------------------


PRO CW_DRAWCOLOR::Cleanup

; Nothing to clean up in this Cleanup method.

END ;-------------------------------------------------------------------------------------


PRO CW_DRAWCOLOR__DEFINE

; The class definition of the CW_DRAWCOLOR object.

   struct = { CW_DRAWCOLOR, $    ; The class name.
              parent: 0L, $      ; The identifier of the parent widget.
              event_pro:"", $    ; The name of an event handler procedure.
              event_func: "", $  ; The name of an event handler function.
              filename: "", $    ; The filename for colors to use in cgPickColorName
              tlb: 0L, $         ; The top-level base widget of this compound widget.
              labelID: 0L, $     ; The identifier of the label widget.
              drawID: 0L, $      ; The identifier of the draw widget.
              wid: 0L, $         ; The window index number of the draw widget.
              index: 0L, $       ; The color table index number where color is loaded.
              title: "", $       ; The title of the cgPickColorName program.
              color: "" }        ; The name of the color displayed in the color patch.

END ;-------------------------------------------------------------------------------------



FUNCTION CW_DRAWCOLOR, $

; A wrapper function for the CW_DRAWCOLOR object.

   parent, $                   ; The parent widget identifier for this compound widget.
   Color=color, $              ; The name of the color to be displayed. Color names come from cgPickColorName.
   Column=column, $            ; Set this keyword to stack widgets in a column. Default is in a row.
   Event_Func=event_func, $    ; The name of an event handler function for this compound widget.
   Event_Pro=event_pro, $      ; The name of an event handler procedure for this compound widget.
   Index=index, $              ; An index number where the color should be loaded. !D.Table_Size-2, by default.
   Filename=filename, $        ; An optional input to cgPickColorName specifying different colors.
   Label_Left=label_left, $    ; Set this keyword to have the label text aligned on the left of the label. Default is to center.
   Label_Right=label_right, $  ; Set this keyword to have the label text aligned on the right of the label. Default is to center.
   LabelSize=labelsize, $      ; This is the X size of the label widget (containing the label) in device coordinates. Default is natural size.
   LabelText=label, $          ; This is the text on the label. Example, "Background Color", etc.
   Object=object, $            ; The object reference for the CW_DRAWCOLOR object. (Output keyword.)
   Title=title, $              ; This is the title on the cgPickColorName program that allows the user to select another color.
   UValue=uvalue, $            ; A user value for the widget.
   XSize=xsize, $              ; The xsize (in pixel units) of the color patch. By default, 20.
   YSize=ysize                 ; The xsize (in pixel units) of the color patch. By default, 20.

object = Obj_New("CW_DRAWCOLOR", $
   parent, $                   ; The parent widget identifier for this compound widget.
   Color=color, $              ; The name of the color to be displayed. Color names come from cgPickColorName.
   Column=column, $            ; Set this keyword to stack widgets in a column. Default is in a row.
   Event_Func=event_func, $    ; The name of an event handler function for this compound widget.
   Event_Pro=event_pro, $      ; The name of an event handler procedure for this compound widget.
   Index=index, $              ; An index number where the color should be loaded. !D.Table_Size-2, by default.
   Filename=filename, $        ; An optional input to cgPickColorName specifying different colors.
   Label_Left=label_left, $    ; Set this keyword to have the label text aligned on the left of the label. Default is to center.
   Label_Right=label_right, $  ; Set this keyword to have the label text aligned on the right of the label. Default is to center.
   LabelSize=labelsize, $      ; This is the X size of the label widget (containing the label) in device coordinates. Default is natural size.
   LabelText=label, $          ; This is the text on the label. Example, "Background Color", etc.
   Title=title, $              ; This is the title on the cgPickColorName program that allows the user to select another color.
   UValue=uvalue, $            ; A user value for the widget.
   XSize=xsize, $              ; The xsize (in pixel units) of the color patch. By default, 20.
   YSize=ysize)                ; The xsize (in pixel units) of the color patch. By default, 20.


   ; Return the TLB of the compound widget.

IF Obj_Valid(object) THEN RETURN, object->GetTLB() ELSE RETURN, -1L
END ;-------------------------------------------------------------------------------------
