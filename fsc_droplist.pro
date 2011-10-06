;+
; NAME:
;   FSC_DROPLIST
;
; PURPOSE:
;
;   The purpose of this compound widget is to provide an alternative
;   to the DROPLIST widget offered in the IDL distribution. What has
;   always annoyed me about a droplist is that you can't get the current
;   "value" of a droplist easily. This compound widget makes this and
;   other tasks much easier.
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
;   General programming.
;
; CALLING SEQUENCE:
;
;   droplistObj = FSC_Droplist(parent, Title='Animals: ", Value=['Dog'. 'Cat', 'Coyote'], Index=2)
;
;   The return value of the FSC_Droplist (droplistObj in this example) is
;   an object reference. Interaction with the droplist will occur through
;   object methods.
;
; INPUT PARAMETERS:
;
;   parent -- The parent widget ID of the compound widget. Required.
;
; INPUT KEYWORDS:
;
; Any keyword that is appropriate for the Widget_Droplist function can be used.
; In addition, these keywords are explicitly defined.
;
;   EVENT_FUNC -- Set this keyword to the name of an Event Handler Function.
;   EVENT_PRO -- Set this keyword to the name of an Event Handler Procedure.
;   FORMAT -- A format specifier for the "format" of the values in the droplist.
;   INDEX -- The index number of the current selection.
;   SPACES -- A two-element array that indicates the number of blank spaces to be added
;             to the the beginning and end of the formatted values. If a single number
;             is provided, this number of blank spaces is added to both the beginning
;             and the end of the value.
;   TITLE -- The title of the droplist widget.
;   UNAME -- The user name of the droplist widget. (Only available in IDL 5.2 and higher.)
;   UVALUE -- The normal "user value" of the droplist.
;   VALUE -- An array of the droplist "selections". May be any data type.
;
; COMMON BLOCKS:
;
;   None.
;
; DEPENDENCIES:
;
;   Requires ERROR_MESSAGE from the Coyote Library..
;
; EVENT STRUCTURE:
;
;   An event is returned each time the droplist value is changed. The event structure
;   is defined like this:
;
;   event = { FSC_DROPLIST_EVENT, $ ; The name of the event structure.
;             ID: 0L, $             ; The ID of the compound widget's top-level base.
;             TOP: 0L, $            ; The widget ID of the top-level base of the hierarchy.
;             HANDLER: 0L, $        ; The event handler ID. Filled out by IDL.
;             INDEX: 0L, $          ; The index number of the current selection.
;             SELECTION:Ptr_New() $ ; A pointer to the current selection "value".
;             SELF:Obj_New() }      ; The object reference of the compound widget.
;
; PUBLIC OBJECT METHODS:
;
;   GetID -- A function with no arguments that returns the widget identifier
;      of the droplist widget.
;
;      droplistID = droplistObj->GetID()
;
;   GetIndex -- A function with no arguments that returns the index
;      number of the current droplist selection.
;
;      currentIndex = droplistObj->GetIndex()
;
;   GetSelection -- A function with no arguments that returns the current
;      droplist selection.
;
;      currentSelection = droplistObj->GetSelection()
;
;   GetUValue -- A function with no arguments that returns the "user value"
;      of the compound widget i.e., the value set with the UVALUE keyword).
;
;      myUValue = droplistObj->GetUValue()
;
;   GetValues -- A function with no arguments that returns the "values" or
;      "selections" for the droplist.
;
;      possibleSelections = droplistObj->GetValues()
;
;   Resize -- A procedure that sets the X screen size of the droplist. It is
;      defined like this:
;
;      PRO Resize, newSize, ParentSize=parentSize
;
;      The "newSize" keyword is the new X screen size. If this argument is
;      missing, the screen X size of the compound widget's parent is used.
;      The parentSize keyword is an output keyword that returns the X screen
;      size of the compound widget's parent.
;
;      droplistObj->Resize, 400
;
;      Note that not all devices (e.g., X Windows devices) support droplist resizing.
;
;   SetIndex -- A procedure that sets the current droplist selection based on
;      the given index. This is equivalent to Widget_Control, droplistID, Set_Droplist_Select=newIndex
;
;      droplistObj->SetIndex, newIndex
;
;   SetSelection -- Whereas a regular droplist widget can only be set by index
;      number, this compound widget can also be set by a "selection". The new selection
;      can be any data type and corresponds to one of the "values" of the droplist.
;
;      droplistObj->SetSelection, newSelection
;
;   SetValues -- Sets the possible selections of the droplist widget. The CurrentIndex keyword
;      will allow the current index of the selection to be changed to:
;
;      newChoices = ['dog', 'cat', 'coyote']
;      droplistObj->SetValues, newChoices, CurrentIndex=2
;
;
; EXAMPLE:
;
;   An example program is provided at the end of the FSC_DROPLIST code. To run it,
;   type these commands:
;
;      IDL> .Compile FSC_DROPLIST
;      IDL> Example
;
; MODIFICATION HISTORY:
;
;   Written by: David W Fanning, 17 Jan 2000. DWF.
;   Added FORMAT and SPACES keywords 28 April 2000. DWF.
;   Fixed a small problem with event processing when the EVENT_FUNC keyword
;      was used. 29 Dec 2000. DWF.
;   Attached the UNAME value to the TLB of the compound widget instead
;      of to the droplist widget itself. 11 Jan 2001. DWF.
;   Fixed a problem when the droplist was part of a modal widget and used the
;      EVENT_PRO keyword. 27 Oct 2003. DWF.
;   Added a SetValue method for setting all the values in the droplist at once. 12 Nov 2004. DWF.
;   Fixed type on line 346/ 6 Feb 2008. DWF.
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
PRO FSC_Droplist_Kill_Notify, droplistID

; This procedure is called automatically when the droplist is destroy. The
; purpose is to destroy the self object so there are no memory leaks.

Widget_Control,droplistID, Get_UValue=self
Obj_Destroy, self
END ;--------------------------------------------------------------------------


FUNCTION FSC_Droplist::EventHandler, event

; This is the event handler method of the object. The purpose is to set the
; object's current selection and index number. If an event handler has
; been specified, a FSC_DROPLIST event is sent to the proper event handler.

   ; Set the current selection and index number.

*self.selection = (*self.value)[event.index]
self.index = event.index

   ; If we need to send an event package the event up. Include both
   ; the index number (what normal droplist events produce), the current
   ; selection, and the self object reference.

thisEvent = {FSC_Droplist_EVENT, ID:self.tlb, TOP:event.top, HANDLER:event.handler, $
 INDEX:self.index, SELECTION:self.selection, SELF:self}

   ; Send the event if requested. If it is requested, no events returned.

IF self.event_pro NE "" THEN BEGIN
   Call_Procedure, self.event_pro, thisEvent
   thisEvent = 0
   IF Obj_Valid(self) EQ 0 THEN RETURN, thisEvent ; Modal widget destroyed.
ENDIF

IF self.event_func NE "" THEN BEGIN
   ok = Call_Function(self.event_func, thisEvent)
   thisEvent = 0
ENDIF

RETURN, thisEvent
END ;--------------------------------------------------------------------------



FUNCTION FSC_Droplist_Events, event

; This is the droplist event handler. The purpose of this procedure is to
; get the self object reference and call the object's own event method.
; The event handler method will return 0 if an event handler procedure or
; function has been called. Otherwise, it will return the event.

Widget_Control, event.id, Get_UValue=self
theEvent = self->EventHandler(event)
RETURN, theEvent
END ;--------------------------------------------------------------------------



PRO FSC_Droplist::Sensitive, value

; This method makes the droplist sensitive (value=1) or insensitive (value=0).

Widget_Control, self.droplistID, Sensitive=value
END ;--------------------------------------------------------------------------



PRO FSC_Droplist::Resize, newSize,  ParentSize=parentSize

; This method resizes the droplist. If the variable "newSize" is not provided,
; the new size is set by the parent widget's X screen size. This makes it possible
; to have the droplist sized to fit it's parent base widget.

   ; Get the parent widget's geometry and X screen size.

parentGeometry = Widget_Info(self.parent, /Geometry)
parentSize = parentGeometry.scr_xsize

   ; Has a size been provided? In not, use the parent's X screen size.

IF N_Elements(newSize) EQ 0 THEN BEGIN
   newSize = parentGeometry.scr_xsize
ENDIF

   ; Resize the droplist widget.

Widget_Control, self.droplistID, Scr_XSize=newSize
END ;--------------------------------------------------------------------------



PRO FSC_Droplist::SetSelection, selection

; This method sets the current selection of the droplist by means of
; the selection "value". With a normal droplist you must set the selection
; by specifying an index number.

   ; Find the selection in the value. Return it's index number.

IF N_Elements(selection) EQ 0 THEN selection = (*self.value)[0]
index = Where(StrUpCase( Strtrim(*self.value,2) ) EQ StrUpCase( StrTrim(selection,2) ), count)

   ; If you can't find the selection, print an error message.

IF count EQ 0 THEN BEGIN
   uname = Widget_Info(self.droplistID,/UName)
   ok = Error_Message('No item with name "' + uname + ':' + $
      StrTrim(selection,2) + '" exists. Returning...')
   RETURN
ENDIF

   ; Set the current index and selection for the object.

index = 0 > index < (N_Elements(*self.value) - 1)
self.index = index
*self.selection = selection

   ; Set the current selection for the droplist widget.

Widget_Control, self.droplistID, Set_Droplist_Select=self.index
END ;--------------------------------------------------------------------------



PRO FSC_Droplist::SetIndex, index

; This method sets the current selection by means of its index number.

   ; Make sure the index number is there and valid.

IF N_Elements(index) EQ 0 THEN index = 0
self.index = 0 > index < (N_Elements(*self.value) - 1)

   ; Set the current selection.

*self.selection = (*self.value)[index]

   ; Set the selection on the droplist widget.

Widget_Control, self.droplistID, Set_Droplist_Select=self.index

END ;--------------------------------------------------------------------------



PRO FSC_Droplist::SetValues, theValues, CurrentIndex=currentIndex

; This method allows all the values in the droplist to be changed simultaneously.

  IF Ptr_Valid(self.value) THEN *self.value = theValues ELSE self.value = Ptr_New(theValues)
  Widget_Control, self.droplistID, Set_Value=*self.value

  IF N_Elements(currentIndex) EQ 0 THEN currentIndex = self.index ELSE self.index = currentIndex < (N_Elements(theValues)-1)
  IF currentIndex GT (N_Elements(*self.value)-1) THEN BEGIN
     currentIndex = 0
     self.index = currentIndex
  ENDIF
  IF Ptr_Valid(self.selection) THEN *self.selection = (*self.value)[self.index] ELSE $
    self.selection = Ptr_New((*self.value)[self.index])
  Widget_Control, self.droplistID, Set_Droplist_Select=self.index
END ;--------------------------------------------------------------------------



FUNCTION FSC_Droplist::GetIndex

; This method returns the index number of the current selection.

RETURN, self.index
END ;--------------------------------------------------------------------------



FUNCTION FSC_Droplist::GetSelection

; This method returns the "value" of the current selection.

RETURN, *self.selection
END ;--------------------------------------------------------------------------



FUNCTION FSC_Droplist::GetValues

; This method returns the current "values" or "selections" of the droplist.

RETURN, *self.value
END ;--------------------------------------------------------------------------



FUNCTION FSC_Droplist::GetUValue

; This method returns the "user value" of the droplist.

RETURN, *self.storage
END ;--------------------------------------------------------------------------



FUNCTION FSC_Droplist::GetID

; This method returns the droplist widget identifier.

RETURN, self.droplistID
END ;--------------------------------------------------------------------------



PRO FSC_Droplist::CLEANUP

; This is the object's cleanup method. Free's up all the pointers used in the
; object.

Ptr_Free, self.value
Ptr_Free, self.selection
Ptr_Free, self.storage
END ;--------------------------------------------------------------------------



FUNCTION FSC_Droplist::INIT, $
   parent, $                    ; The widget ID of the droplist's parent widget. Required.
   debug=debug, $               ; Turns full error handling on for INIT catch error handler.
   Event_Func=event_func, $     ; The name of an event handler function.
   Event_Pro=event_pro, $       ; The name of an event handler procedure.
   _Extra=extra, $              ; A keyword that allows any droplist keyword to be used in initialization.
   Format=format, $             ; The format of the "value" of the droplist.
   Index=index, $               ; The index number of the current selection.
   Spaces=spaces, $             ; Number of blank spaces to add to formatted "values".
   Title=title, $               ; The text that goes in the TITLE of the droplist.
   UName=uname, $               ; A user name. Left for the user of the cw_droplist program.
   UValue=storage, $            ; A user value. Left for the user of the cw_droplist program.
   Value=value                  ; The "value" or selections of the droplist. May be any data type.

   ; Catch any errors.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message(!Error_State.Msg, Traceback=Keyword_Set(debug))
   RETURN, 0
ENDIF

; This is the object's initialization method.

IF N_Elements(parent) EQ 0 THEN BEGIN
   ok = Error_Message('Parent parameter must be provided. Returning...')
   RETURN, 0
ENDIF

   ; Check for presence of keywords.

IF N_Elements(title) EQ 0 THEN title = 'Selection: '
IF N_Elements(value) EQ 0 THEN value = ['Dog', 'Cat', 'Coyote']
IF N_Elements(storage) EQ 0 THEN storage = ""
IF N_Elements(index) EQ 0 THEN index = 0
IF N_Elements(event_pro) EQ 0 THEN event_pro = ""
IF N_Elements(event_func) EQ 0 THEN event_func = ""
IF N_Elements(uname) EQ 0 THEN uname = ""
IF N_Elements(spaces) EQ 0 THEN BEGIN
   forwardSpace = ""
   trailingSpace = ""
ENDIF ELSE BEGIN
   IF N_Elements(spaces) EQ 2 THEN BEGIN
      IF spaces[0] LE 0 THEN forwardSpace = "" ELSE forwardSpace = String(Replicate(32B,spaces[0]))
      IF spaces[1] LE 0 THEN trailingSpace = "" ELSE trailingSpace = String(Replicate(32B,spaces[1]))
   ENDIF ELSE BEGIN
      IF spaces[0] LE 0 THEN forwardSpace = "" ELSE forwardSpace = String(Replicate(32B,spaces[0]))
      IF spaces[0] LE 0 THEN trailingSpace = "" ELSE trailingSpace = String(Replicate(32B,spaces[0]))
   ENDELSE
ENDELSE
index = 0 > index < (N_Elements(value) - 1)

   ; Create the droplist widget. UName only available for IDL 5.2 and higher.

IF Float(!Version.Release) GE 5.2 THEN BEGIN

   IF Keyword_Set(format) THEN theValue = String(value, Format=format) ELSE theValue = StrTrim(value,2)
   self.tlb = Widget_Base(parent, UValue=storage, UName=uname)
   self.droplistID = Widget_Droplist(self.tlb, $
         Title=title, $
         Value=forwardSpace + theValue + trailingSpace, $
         _Extra=extra, $
         Kill_Notify='FSC_Droplist_Kill_Notify', $
         Event_Func='FSC_Droplist_Events', $
         UValue=self)

 ENDIF ELSE BEGIN

   IF Keyword_Set(format) THEN theValue = String(value, Format=format) ELSE theValue = StrTrim(value,2)
   self.tlb = Widget_Base(parent, UValue=storage)
   self.droplistID = Widget_Droplist(self.tlb, $
         Title=title, $
         Value=forwardSpace + theValue + trailingSpace, $
         _Extra=extra, $
         Kill_Notify='FSC_Droplist_Kill_Notify', $
         Event_Func='FSC_Droplist_Events', $
         UValue=self)

ENDELSE

   ; Set the current selection on the droplist.

selection = Value[index]
Widget_Control, self.droplistID, Set_Droplist_Select=index

   ; Populate the object.

self.storage = Ptr_New(storage)
self.index = index
self.title = title
self.parent = parent
self.selection = Ptr_New(selection)
self.event_func = event_func
self.event_pro = event_pro
self.value = Ptr_New(value)

RETURN, 1
END ;--------------------------------------------------------------------------



PRO FSC_Droplist__DEFINE

   struct = { FSC_Droplist, $          ; The FSC_DROPLIST object class definition.
              title:"", $              ; The droplist title.
              value:Ptr_New(), $       ; The values or selections on the droplist.
              parent:0L, $             ; The widget identifer of the parent widget.
              tlb:0L, $                ; The top-level base of the compound widget. .
              droplistID:0L, $         ; The droplist widget identifier.
              selection:Ptr_New(), $   ; The current droplist selection.
              storage:Ptr_New(), $     ; The storage locaton for the "user value".
              event_pro:"", $          ; The name of an event handler procedure.
              event_func:"", $         ; The name of an event handler function.
              index:0 $                ; The index number of the current selecton.
            }
END ;--------------------------------------------------------------------------



FUNCTION FSC_Droplist, parent, Title=title, Value=value, _Extra=extra

   ; This is the compound widget function call. It's purpose is to
   ; create the compound widget object and return it.

RETURN, Obj_New('FSC_Droplist', parent, Title=title, Value=value, _Extra=extra)
END ;--------------------------------------------------------------------------



PRO Example_Events, event
Widget_Control, event.top, Get_UValue=droplists
thisEvent = Tag_Names(event, /Structure_Name)
CASE thisEvent OF
   'FSC_DROPLIST_EVENT': BEGIN
      thisDroplist = event.self->GetUValue()
      Print, ""
      Print, thisDroplist + ' Selection: ', *event.selection
      Print, thisDroplist + ' Index Number: ', event.index
      ENDCASE
   'WIDGET_BUTTON': BEGIN
      Widget_Control, event.id, Get_UValue=thisValue
      CASE thisValue OF ; Set the droplists by "selection".
         'SET_DROPLIST1': droplists[0]->SetSelection, 'Coyote'
         'SET_DROPLIST2': droplists[1]->SetSelection, 99.8
         ELSE: Widget_Control, event.top, /Destroy
      ENDCASE
      ENDCASE
ENDCASE
END ;---------------------------------------------------------------------------


PRO Example

; Droplist 1 is filled with string selections.
; Droplist 2 is filled with number selections.

tlb = Widget_Base(Title='FSC_Droplist Example', Column=1)
animals = ['Dog', 'Cat', 'Opposum', 'Coyote']
numbers = [45.6, 18.3, 21.5, 99.8]

   ; Create the droplists.

droplist1 = FSC_Droplist(tlb, Value=animals, Index=3, UValue='DROPLIST 1')
droplist2 = FSC_Droplist(tlb, Value=numbers, Index=1, UValue='DROPLIST 2', $
   Format='(F5.2)', Spaces=[2,0], Debug=1)

   ; Other widgets.

button = Widget_Button(tlb, Value='Set Droplist 1 to "Coyote"', UValue='SET_DROPLIST1')
button = Widget_Button(tlb, Value='Set Droplist 2 to 99.8', UValue='SET_DROPLIST2')
button = Widget_Button(tlb, Value='Quit', UValue='QUIT')

   ; Resize the droplists.

droplist1->Resize, 200
droplist2->Resize, 200

Widget_Control, tlb, /Realize, Set_UValue=[droplist1, droplist2]
XManager, 'Example', tlb, Event_Handler='Example_Events', /No_Block

; Test of SetValues method:
;      Wait, 2
;      newChoices = ['dog', 'cat', 'coyote']
;      droplist1->SetValues, newChoices, CurrentIndex=2

END ;---------------------------------------------------------------------------
