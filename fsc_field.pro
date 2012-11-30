;+
; NAME:
;   FSC_FIELD
;
; PURPOSE:
;
;   The purpose of this compound widget is to provide an alternative
;   to the CW_FIELD widget offered in the IDL distribution. One weakness
;   of the CW_FIELD compound widget is that the text widgets do not
;   look editable to the users on Windows platforms. This program
;   corrects that deficiency and adds some features that I think
;   will be helpful. For example, you can now assign an event handler
;   to the compound widget, ask for positive numbers only, and limit
;   the number of digits in a number, or the number of digits to the
;   right of a decimal point. The program is written as a widget object,
;   which allows the user to call object methods directly, affording
;   even more flexibility in use. This program replaces the earlier
;   programs FSC_INPUTFIELD and COYOTE_FIELD.
;
;   The program consists of a label widget next to a one-line text widget.
;   The "value" of the compound widget is shown in the text widget. If the
;   value is a number, it will not be possible (generally) to type
;   alphanumeric values in the text widget. String values behave like
;   strings in any one-line text widget.
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
; TYPICAL CALLING SEQUENCE:
;
;   fieldID = FSC_FIELD(parent, Title="X Size:", Value=256, Object=fieldObject, Digits=3)
;
; INPUT PARAMETERS:
;
;   parent -- The parent widget ID of the compound widget. Required.
;
; INPUT KEYWORDS:
;
;   COLUMN        Set this keyword to have the Label widget above the Text widget.
;                 The default is to have the Label widget in a row with the Text widget.
;
;   CR_ONLY       Set this keyword if you only want Carriage Return events returned to
;                 your event handler. If this keyword is not set, all events are returned.
;                 Setting this keyword has no effect unless either the EVENT_PRO or
;                 EVENT_FUNC keyword is used.
;
;   DECIMAL       Set this keyword to the number of digits to the right of the decimal
;                 point in floating point or double precision numbers. Ignored for STRING values.
;
;   DIGITS        Set this keyword to the number of digits permitted in integer numbers.
;
;   EVENT_FUNC    Set this keyword to the name of an event handler function. If this
;                 keyword is undefined and the Event_Pro keyword is undefined,
;                 all compound widget events are handled internally and not
;                 passed on to the parent widget.
;
;   EVENT_PRO     Set this keyword to the name of an event handler procedure. If this
;                 keyword is undefined and the Event_Func keyword is undefined,
;                 all compound widget events are handled internally and not
;                 passed on to the parent widget.
;
;   FIELDFONT     The font name for the text in the text widget.
;
;   FRAME         Set this keyword to put a frame around the compound widget.
;
;   FOCUS_EVENTS  Set this keyword to enable event generation for keyboard focus
;                 events. Ignored unless EVENT_FUNC or EVENT_PRO keywords are specified.
;
;   HIGHLIGHT     Set this keyword to highlight the existing text if the widget gain
;                 the keyboard focus. This keyword MUST be set for tabbing to work naturally
;                 in IDL 6.2 and higher.
;
;   LABEL_LEFT    Set this keyword to align the text on the label to the left.
;
;   LABEL_RIGHT   Set this keyword to align the text on the label to the right.
;
;   LABELFONT     The font name for the text in the label widget.
;
;   LABELSIZE     The X screen size of the label widget.
;
;   NAME          A string containing the name of the object. The default is ''.
;
;   NOEDIT        Set this keyword to allow no user editing of the input text widget.
;
;   NONSENSITIVE  Set this keyword to make the input text widget non-sensitive.
;
;   POSITIVE      Set this keyword if you want only positive numbers allowed.
;
;   SCR_XSIZE     The X screen size of the compound widget.
;
;   SCR_YSIZE     The Y screen size of the compound widget.
;
;   TITLE         The string text placed on the label widget.
;
;   UNDEFINED     Set this keyword to the value to use for "undefined" values. If
;                 not set, then !Value.F_NAN is used for numerical fields and a
;                 NULL string is used for string fields. This applies to values
;                 obtained with the GET_VALUE method or the GET_VALUE function.
;
;   UVALUE        A user value for any purpose.
;
;   VALUE         The "value" of the compound widget. Any type of integer, floating, or string
;                 variable is allowed. The data "type" is determined automatically from the
;                 value supplied with this keyword. Be sure you set the type appropriately for
;                 your intended use of the value.
;
;   XSIZE         The X size of the text widget in the usual character units.
;
; OUTPUT KEYWORDS:
;
;   OBJECT        Set this keyword to a named variable to receive the compound widget's
;                 object reference. This is required if you wish to call methods on the object.
;                 Note that the object reference is also available in the event structure
;                 generated by the widget object. Note that the object reference will be
;                 necessary if you want to get or set values in the compound widget.
;
; COMMON BLOCKS:
;
;   None.
;
; RESTRICTIONS:
;
;   Requires cgDblToStr from the Coyote Library:
;      http://www.idlcoyote.com/programs/cgdbltostr.pro
;
; EVENT STRUCTURE:
;
;   All events are handled internally unless either the Event_Pro or Event_Func
;   keywords are used to assign an event handler to the compound widget. By
;   default all events generated by the text widget are passed to the assigned
;   event handler. If you wish to receive only Carriage Return events, set the
;   CR_Only keyword.
;
;   event = { FSC_FIELD_EVENT, $   ; The name of the event structure.
;             ID: 0L, $            ; The ID of the compound widget's top-level base.
;             TOP: 0L, $           ; The widget ID of the top-level base of the hierarchy.
;             HANDLER: 0L, $       ; The event handler ID. Filled out by IDL.
;             OBJECT: Obj_New(), $ ; The "self" object reference. Provided so you can call methods.
;             VALUE: Ptr_New(), $  ; A pointer to the widget value.
;             TYPE:""              ; A string indicating the type of data in the VALUE field.
;           }
;
;   Note that if the field is "empty", the VALUE will be a pointer
;   to an undefined variable. You should check this value before you
;   use it. You code will look something like this:
;
;     IF N_Elements(*event.value) EQ 0 THEN $
;         Print, 'Current Value UNDEFINED.' ELSE $
;         Print, 'Current Value: ', *event.value
;
; GETTING and SETTING VALUES:
;
;   Almost all the properties of the widget can be obtained or set via
;   the object's GetProperty and SetProperty methods (described below).
;   Traditional compound widgets have the ability to get and set the "value"
;   of the compound widget identifier (e.g., fieldID in the calling
;   sequence above). Unfortunately, it is impossible to retreive a variable
;   in this way when the variable is undefined. In practical terms, this
;   means that the undefined variable must be set to *something*. You can
;   determine what that something is with the UNDEFINED keyword, or I will set
;   it to !VALUES.F_NAN for numerical fields and to the null string for string
;   fields. In any case, you will have to check for undefined variables before
;   you try to do something with the value. For a numerical field, the code
;   might look something like this:
;
;      fieldID = FSC_FIELD(parent, Title="X Size:", Value=256, Object=fieldObject, Digits=3)
;      currentValue = fieldObject->Get_Value()
;      IF Finite(currentValue) EQ 0 THEN Print, 'Value is Undefined' ELSE Print, currentValue
;
;   Additional examples are provided in the numerical example fields in Example Program below.
;
;   Setting the value of the compound widget is the same as calling the Set_Value
;   method on the object reference. In other words, these two statements are equivalent.
;
;        fieldObject->Set_Value, 45.4
;        Widget_Control, fieldID, Set_Value=45.4
;
;   The data type of the value is determined from the value itself. Be sure you set it appropriately.
;
; OBJECT PROCEDURE METHODS:
;
;   GetProperty -- This method allows various properties of the widget to be
;       returned via output keywords. The keywords that are available are:
;
;       CR_Only -- A flag, if set, means only report carriage return events.
;       DataType -- The data type of the field variable.
;       Decimal -- Set this keyword to the number of digits to the right of the decimal
;              point in FLOATVALUE and DOUBLEVALUE numbers.
;       Digits -- Set this keyword to the number of digits permitted in INTERGERVALUE and LONGVALUE numbers.
;       Event_Func -- The name of the event handler function.
;       Event_Pro -- The name of the event handler function.
;       Has_Focus -- Set to 1 if the text widget currently has the keyboard focus.
;       Highlight -- The highlight flag.
;       NoEdit -- The NoEdit flag.
;       NonSensitive -- The NonSensitive flag.
;       Undefined -- The "value" of any undefined value.
;       UValue -- The user value assigned to the compound widget.
;       Value -- The "value" of the compound widget.
;     Name -- A scalar string name of the object.
;
;   Resize -- This method allows you to resize the compound widget's text field.
;        The value parameter is an X screen size for the entire widget. The text
;        widget is sized by using the value obtained from this value minus the
;        X screen size of the label widget.
;
;          objectRef->Resize, screen_xsize_value
;
;   Set_Value -- This method allows you to set the "value" of the field. It takes
;       one positional parameter, which is the value.
;
;          objectRef->Set_Value, 5
;
;   SetProperty -- This method allows various properties of the widget to be
;       set via input keywords. The keywords that are available are:
;
;       CR_Only -- Set this keyword if you only want Carriage Return events.
;       Decimal -- Set this keyword to the number of digits to the right of the decimal
;              point in FLOAT and DOUBLE numbers.
;       Digits -- Set this keyword to the number of digits permitted in INTERGER and LONG numbers.
;       Event_Func -- Set this keyword to the name of an Event Function.
;       Event_Pro -- Set this keyword to the name of an Event Procedure.
;       Highlight -- Set this keyword to highlight the existing text
;                    when the widget gets the keyboard focus
;       LabelSize --  The X screen size of the Label Widget.
;       Name -- A scalar string name of the object. (default = '')
;       NoEdit -- Set this keyword to make the text widget uneditable
;       NonSensitive -- Set this keyword to make the widget nonsensitive
;       Scr_XSize -- The X screen size of the text widget.
;       Scr_YSize -- The Y screen size of the text widget.
;       Title -- The text to go on the Label Widget.
;       UValue -- A user value for any purpose.
;       Value -- The "value" of the compound widget.
;       XSize -- The X size of the Text Widget.
;
;   SetTabNext -- This method allows you to specify which field to go to when a TAB character
;      is typed in the text widget. See the Example program below for an example of how to
;      use this method.
;
; OBJECT FUNCTIONS METHODS:
;
;      Get_Value -- Returns the "value" of the field. No parameters. Will be undefined
;          if a "number" field is blank. Should be checked before using:
;
;          IF N_Elements(objectRef->Get_Value()) NE 0 THEN Print, Value is: ', objectRef->Get_Value()
;
;      GetID -- Returns the widget identifier of the compound widget's top-level base.
;         (The first child of the parent widget.) No parameters.
;
;      GetLabelSize -- Returns the X screen size of the label widget. No parameters.
;
;      GetTextID -- Returns the widget identifier of the compound widget's text widget.
;         No parameters.
;
;      GetTextSize -- Returns the X screen size of the text widget. No parameters.
;
; PRIVATE OBJECT METHODS:
;
;   Although there is really no such thing as a "private" method in IDL's
;   object implementation, some methods are used internally and not meant to
;   be acessed publicly. Here are a few of those methods. I list them because
;   it may be these private methods are ones you wish to override in subclassed
;   objects.
;
;      MoveTab -- This method moves the focus to the widget identified in the "next" field,
;        which must be set with the SetTabNext method. No parameters. Called automatically
;        when a TAB character is typed in the text widget.
;
;      Text_Events -- The main event handler method for the compound widget. All
;        text widget events are processed here.
;
;      ReturnValue -- This function method accepts a string input value and converts
;        it to the type of data requested by the user.
;
;      Validate -- This function method examines all text input and removes unwanted
;        characters, depending upon the requested data type for the field. It makes it
;        impossible, for example, to type alphanumeric characters in an INTEGER field.
;
; EXAMPLE:
;
;   An example program is provided at the end of the FSC_FIELD code. To run it,
;   type these commands:
;
;      IDL> .Compile FSC_Field
;      IDL> Example
;
; MODIFICATION HISTORY:
;
;   Written by: David W. Fanning, 18 October 2000. Based heavily on an earlier
;      FSC_INPUTFIELD program and new ideas about the best way to write
;      widget objects.
;   Added LABEL_LEFT, LABEL_RIGHT, and UNDEFINED keywords. 29 Dec 2000. DWF.
;   Modified the way the value is returned in the GET_VALUE method and the
;      GET_VALUE function. Modified Example program to demonstrate. 30 Dec 2000. DWF.
;   Added NOEDIT and NONSENSITIVE keywords, with corresponding SETEDIT and SETSENNSITIVE
;      methods. 19 Jan 2001. DWF.
;   Actually followed through with the changes I _said_" I made 29 Dec 2000. (Don't ask....) 13 June 2001. DWF.
;   Added GetTextSize and GetLabelSize methods for obtaining the X screen
;      size of the text and label widgets, respectively. 21 July 2001. DWF.
;   Fixed a problem in SetProperty method where I was setting self.xsize, which doesn't exist. 24 April 2002. DWF.
;   Small modification to the SetEdit method. 6 August 2003. DWF.
;   Added Highlight keyword. Ported Focus_Events keyword from
;      fsc_inputfield.pro. Updated documentation. 17 November
;      2004. DWF and Benjamin Hornberger
;   Added Has_Focus keyword to the GetProperty method. 18 November
;      2004. Benjamin Hornberger
;   Fixed bug in GetProperty method (set value to *self.undefined if
;      *self.value is undefined. 24 Feb 2004. Benjamin Hornberger
;   Modified FOCUS_EVENTS keyword handling so that *all* focus events are now
;      passed to specified event handlers. Check event.select to see if the
;      widget is gaining or losing focus. 10 August 2005. DWF.
;   Added new tabbing functionality, introduced in IDL 6.2. To use tabbing
;      functionality natually, the HIGHTLIGHT keywords must be set.
;      See included EXAMPLE program for details. 10 August 2005. DWF.
;   Added functionality to covert double precision values to strings properly. 30 Nov 2005. DWF.
;   Set the default fonts to be the current widget font, rather than the default widget font. 4 Oct 2008. DWF.
;   Fixed a problem with validating a float or double value when it was written with
;      exponential notation. 2 April 2010. DWF.
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

FUNCTION FSC_Field_WidgetFont, DEFAULT=default

   ; Build a small widget to determine the current 
   ; and default widget fonts.
   
   base = Widget_Base(MAP=0)
   button = Widget_Button(base, Value='TEST')
   
   ; Checking before realization gives default font.
   defaultFont = Widget_Info(button, /FONTNAME)
   
   ; Checking after realization gives current font.
   Widget_Control, base, /REALIZE
   currentFont = Widget_Info(button, /FONTNAME)
   
   ; Clean up.
   Widget_Control, base, /DESTROY

   IF Keyword_Set(default) THEN $
        RETURN, defaultFont ELSE $
        RETURN, currentFont
    
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION FSC_Field_Error_Message, theMessage, Traceback=traceback, NoName=noName, _Extra=extra

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



FUNCTION FSC_Field::GetLabelSize

; This method returns the X screen size of the label widget.

geom = Widget_Info(self.labelID, /Geometry)
RETURN, geom.scr_xsize
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION FSC_Field::GetTextSize

; This method returns the X screen size of the text widget.

geom = Widget_Info(self.textID, /Geometry)
RETURN, geom.scr_xsize
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO FSC_Field::MoveTab

   ; If the TABNEXT field has a valid widget, will set the
   ; focus to this widget when a TAB event occurs in the widget.

IF  NOT Widget_Info(self.tabnext, /Valid_ID) THEN RETURN
Widget_Control, self.tabnext, /Input_Focus
Widget_Control, self.tabnext, Get_Value=theText
theText = theText[0]
Widget_Control, self.tabnext, Set_Text_Select=[0,StrLen(theText)]
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO FSC_Field::SetTabNext, nextID

; This method assigns a text id the TABNEXT field of
; the object. This is required for the MOVETAB method.

self.tabnext = nextID
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION FSC_Field::GetTextID

; This method returns the ID of the text widget of the compound widget.

RETURN, self.textID
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO FSC_Field::Resize, newsize

; This method resizes the widget by making the text widget fit the new size.

l = Widget_Info(self.labelID, /Geometry)
Widget_Control, self.textID, Scr_XSize = (newsize - l.scr_xsize)

   ; Set the text widget sensitivitiy.


Widget_Control, self.textID, Sensitive=1-Keyword_Set(self.nonsensitive)

END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION FSC_Field::GetID

; This method returns the ID of the top-level base of the compound widget.

RETURN, self.tlb
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION FSC_Field::Geometry

; This method returns the geometry of the compound widget.

RETURN, Widget_Info(self.tlb,/Geometry)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION FSC_Field::Get_Value

; This method returns the actual value of the compound widget.

IF N_Elements(*self.theValue) EQ 0 THEN RETURN, *self.undefined ELSE RETURN, *self.theValue
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO FSC_Field::Set_Value, value

; This method sets the value of the compound widget.

   ; Error Handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_Field_Error_Message(/Traceback)
   RETURN
ENDIF

dataType = Size(value, /TNAME)
CASE dataType OF
   'BYTE'   : BEGIN
      genType = 'INTEGER'
      dataType = 'INT'
      positive = 1
      value = Fix(value)
      Message, 'BYTE data not supported. Value will be converted to INT.', /Informational
      END
   'INT'    : genType = 'INTEGER'
   'LONG'   : genType = 'INTEGER'
   'LONG64' : genType = 'INTEGER'
   'UINT'   : genType = 'UNSIGNED'
   'ULONG'  : genType = 'UNSIGNED'
   'ULONG64': genType = 'UNSIGNED'
   'FLOAT'  : genType = 'FLOAT'
   'DOUBLE' : genType = 'DOUBLE'
   'STRING' : genType = 'STRING'
   ELSE     : Message, 'Data type ' + dataType + ' is not supported. Returning.'
ENDCASE
self.dataType = dataType
self.gentype = genType

IF self.gentype EQ 'DOUBLE' THEN theText = cgDblToStr(value) ELSE theText = StrTrim(value,2)
theText = self->Validate(theText)

   ; Load the value in the widget.

Widget_Control, self.textID, Set_Value=theText, Set_Text_Select=[StrLen(theText),0]
self.theText = theText

   ; Set the actual value of the compound widget.

*self.theValue = self->ReturnValue(theText)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION FSC_Field::Validate, value

; This function eliminates illegal characters from a string that represents
; a number. The return value is a properly formatted string that can be turned into
; an INT, LONG, FLOAT, or DOUBLE value. This is a "private" method.
;
; + 43B
; - 45B
; . 46B
; 0 - 9 48B -57B
; 'eEdD' [101B, 69B, 100B, 68B]

   ; Error Handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_Field_Error_Message(/Traceback)
   testValue = self->ReturnValue(value)
   IF String(testValue) NE 'NULLVALUE' THEN numCheck = Finite(testValue) ELSE numCheck = 1
   IF numCheck THEN BEGIN
      RETURN, retValue
   ENDIF ELSE BEGIN
      ok = Dialog_Message('The requested number is not representable.')
      RETURN, ""
   ENDELSE
ENDIF

   ; A null string should be returned at once.

IF N_Elements(value) EQ 0 THEN value = ""
value = value[0]
IF value EQ "" THEN RETURN, String(value)

   ; No leading or trailing blank characters to evaluate.

value = StrTrim(value, 2)

   ; A string value should be returned at once. Nothing to check.

IF StrUpCase(self.gentype) EQ 'STRING' THEN RETURN, String(value)

   ; Check integers and longs. A "-" or "+" in the first character is allowed. Otherwise,
   ; only number between 0 and 9, or 43B to 57B.

IF self.gentype EQ 'INTEGER' THEN BEGIN

   returnValue = Ptr_New(/Allocate_Heap)
   asBytes = Byte(value)

   IF self.positive THEN BEGIN
      IF (asBytes[0] EQ 43B) OR $
         (asBytes[0] GE 48B AND asBytes[0] LE 57B) THEN *returnValue = [asBytes[0]]
   ENDIF ELSE BEGIN
      IF (asBytes[0] EQ 45B) OR (asBytes[0] EQ 43B) OR $
         (asBytes[0] GE 48B AND asBytes[0] LE 57B) THEN *returnValue = [asBytes[0]]
   ENDELSE
   length = StrLen(asBytes)
   IF length EQ 1 THEN BEGIN
      IF N_Elements(*returnValue) EQ 0 THEN  *returnValue = [32B] ELSE $
            *returnValue = [asBytes[0]]
   ENDIF ELSE BEGIN
      FOR j=1,length-1 DO BEGIN
         IF (asBytes[j] GE 48B AND asBytes[j] LE 57B) THEN BEGIN
            IF N_Elements(*returnValue) EQ 0 THEN  *returnValue = [asBytes[j]] ELSE $
               *returnValue = [*returnValue, asBytes[j]]
         ENDIF
      ENDFOR
  ENDELSE
  IF N_Elements(*returnValue) NE 0 THEN retValue = String(*returnValue) ELSE retValue = ""
  Ptr_Free, returnValue

      ; Check for digit restrictions.

  IF self.digits GT 0 THEN BEGIN
      retValue = StrTrim(retValue, 2)
      IF StrMid(retValue, 0, 1) EQ "-" THEN digits = self.digits + 1 ELSE digits = self.digits
      retValue = StrMid(retValue, 0, digits)
  ENDIF

  RETURN, retValue

ENDIF

   ; Check unsigned data types.

IF self.gentype EQ 'UNSIGNED' THEN BEGIN

   returnValue = Ptr_New(/Allocate_Heap)
   asBytes = Byte(value)

   IF self.positive THEN BEGIN
      IF (asBytes[0] GE 48B AND asBytes[0] LE 57B) THEN *returnValue = [asBytes[0]]
   ENDIF ELSE BEGIN
      IF (asBytes[0] GE 48B AND asBytes[0] LE 57B) THEN *returnValue = [asBytes[0]]
   ENDELSE
   length = StrLen(asBytes)
   IF length EQ 1 THEN BEGIN
      IF N_Elements(*returnValue) EQ 0 THEN  *returnValue = [32B] ELSE $
            *returnValue = [asBytes[0]]
   ENDIF ELSE BEGIN
      FOR j=1,length-1 DO BEGIN
         IF (asBytes[j] GE 48B AND asBytes[j] LE 57B) THEN BEGIN
            IF N_Elements(*returnValue) EQ 0 THEN  *returnValue = [asBytes[j]] ELSE $
               *returnValue = [*returnValue, asBytes[j]]
         ENDIF
      ENDFOR
  ENDELSE
  IF N_Elements(*returnValue) NE 0 THEN retValue = String(*returnValue) ELSE retValue = ""
  Ptr_Free, returnValue

      ; Check for digit restrictions.

  IF self.digits GT 0 THEN BEGIN
      retValue = StrTrim(retValue, 2)
      digits = self.digits
      retValue = StrMid(retValue, 0, digits)
  ENDIF

  RETURN, retValue

ENDIF

   ; Check floating values. (+,-) in first character or after 'eEdD'.
   ; Only numbers, signs, decimal points, and 'eEdD' allowed.

IF (self.gentype EQ 'FLOAT') OR (self.gentype EQ 'DOUBLE') THEN BEGIN
   returnValue = Ptr_New(/Allocate_Heap)
   asBytes = Byte(value)
   IF self.positive THEN BEGIN
      IF (asBytes[0] EQ 43B) OR $
         (asBytes[0] GE 48B AND asBytes[0] LE 57B) OR $
         (asBytes[0] EQ 46B) THEN *returnValue = [asBytes[0]]
      IF (asBytes[0] EQ 46B) THEN haveDecimal = 1 ELSE haveDecimal = 0
   ENDIF ELSE BEGIN
      IF (asBytes[0] EQ 45B) OR (asBytes[0] EQ 43B) OR $
         (asBytes[0] GE 48B AND asBytes[0] LE 57B) OR $
         (asBytes[0] EQ 46B) THEN *returnValue = [asBytes[0]]
      IF (asBytes[0] EQ 46B) THEN haveDecimal = 1 ELSE haveDecimal = 0
   ENDELSE
   haveExponent = 0
   length = StrLen(asBytes)
   prevByte = asBytes[0]
   exponents = Byte('eEdD')
   IF length EQ 1 THEN BEGIN
      IF N_Elements(*returnValue) EQ 0 THEN  *returnValue = [32B] ELSE $
            *returnValue = [asBytes[0]]
   ENDIF ELSE BEGIN
      FOR j=1,length-1 DO BEGIN
         IF (asBytes[j] GE 48B AND asBytes[j] LE 57B) THEN BEGIN
            IF N_Elements(*returnValue) EQ 0 THEN  *returnValue = [asBytes[j]] ELSE $
               *returnValue = [*returnValue, asBytes[j]]
            prevByte = asBytes[j]
         ENDIF ELSE BEGIN

            ; What kind of thing is it?

            IF (asBytes[j] EQ 46B) THEN BEGIN ; A decimal point.
               IF haveDecimal EQ 0 THEN BEGIN
                  *returnValue = [*returnValue, asBytes[j]]
                  haveDecimal = 1
                  prevByte = asBytes[j]
               ENDIF
            ENDIF

            IF (asBytes[j] EQ 45B) OR (asBytes[j] EQ 43B) THEN BEGIN ; A + or - sign.
               index = Where(exponents EQ prevByte, count)
               IF count EQ 1 AND haveExponent THEN BEGIN
                  *returnValue = [*returnValue, asBytes[j]]
                  haveDecimal = 1
                  prevByte = asBytes[j]
               ENDIF
            ENDIF

            index = Where(exponents EQ asBytes[j], count)
            IF count EQ 1 AND haveExponent EQ 0 THEN BEGIN ; An exponent
               *returnValue = [*returnValue, asBytes[j]]
               haveExponent = 1
               prevByte = asBytes[j]
            ENDIF
         ENDELSE
      ENDFOR
   ENDELSE
      IF N_Elements(*returnValue) NE 0 THEN BEGIN

      retValue = String(*returnValue)
      retValue = StrTrim(retValue, 2)

               ; Check for decimal restrictions

      IF self.decimal GE 0 THEN BEGIN
         theDecimalPt = StrPos(retValue, '.')
         IF theDecimalPt NE -1 THEN retValue = StrMid(retValue, 0, theDecimalPt + self.decimal + 1)
      ENDIF

   ENDIF ELSE retValue = ""
   Ptr_Free, returnValue

      ; Is this a representable number?

   testValue = self->ReturnValue(retValue)
   IF String(testValue) NE 'NULLVALUE' THEN numCheck = Finite(testValue) ELSE numCheck = 1
   IF numCheck THEN BEGIN
      RETURN, retValue
   ENDIF ELSE BEGIN
      ok = Dialog_Message('The requested number is not representable.')
      RETURN, ""
   ENDELSE
ENDIF


END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION FSC_Field::ReturnValue, inputValue

; This method takes a string and turns it into a number,
; depending upon the current data type of the compound widget.
; This is a "private" method. For numbers, if the input value
; is a null string, then an undefined variable is returned.

   ; Error Handling.

ON_IOERROR, CatchIt

CASE self.datatype OF
   'BYTE': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
      retValue = 'NULLVALUE' ELSE retValue = Fix(inputValue)
   'INT': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
      retValue = 'NULLVALUE' ELSE retValue = Fix(inputValue)
   'LONG': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
      retValue = 'NULLVALUE' ELSE retValue = Long(inputValue)
   'LONG64': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
      retValue = 'NULLVALUE' ELSE retValue = Long64(inputValue)
   'UINT': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
      retValue = 'NULLVALUE' ELSE retValue = UInt(inputValue)
   'ULONG': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
      retValue = 'NULLVALUE' ELSE retValue = ULong(inputValue)
   'ULONG64': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
      retValue = 'NULLVALUE' ELSE retValue = ULong64(inputValue)
   'FLOAT' : IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
      retValue = 'NULLVALUE' ELSE retValue = Float(inputValue)
   'DOUBLE': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
      retValue = 'NULLVALUE' ELSE retValue = Double(inputValue)
   'STRING' : retValue = inputValue
ENDCASE

RETURN, retValue

CatchIt:
   retValue = 'NULLVALUE'
   RETURN, retValue
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION FSC_Field::TextEvents, event

; The event handler method for the text widget of the compound widget.

   ; Error Handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_Field_Error_Message(/Traceback)
   RETURN, 0
ENDIF

   ; Get the previous text, the current cursor location in the text widget,
   ; and indicate this is not a Carriage Return event.

previousText = self.theText
textLocation = Widget_Info(event.id, /Text_Select)
cr_event = 0

; Handle keyboard focus events.
IF Tag_Names(event, /structure_name) EQ 'WIDGET_KBRD_FOCUS' THEN BEGIN
    IF event.enter THEN BEGIN
        self.has_focus = 1L
        IF self.highlight THEN BEGIN
            Widget_Control, self.textID, get_value=text
            text = text[0]
            Widget_Control, self.textID, set_text_select=[0, Strlen(text)]
        ENDIF

        IF self.focus THEN BEGIN

              ; Get the current contents of text widget. Validate it.

            Widget_Control, self.textID, Get_Value=newText
            newText = newText[0]
            validText = self->Validate(newText)

               ; Load the valid text.

           self.theText = validText
           testValue  = self->ReturnValue(validText)
           IF String(testValue) EQ "NULLVALUE" THEN BEGIN
               Ptr_Free, self.theValue
               self.theValue = Ptr_New(/Allocate_Heap)
           ENDIF ELSE *self.theValue = testValue

           ; Send the keyboard focus events if there is an event handler to accept them.
           IF self.event_func NE "" THEN BEGIN
              thisEvent = {FSC_Field_Event, self.tlb, event.top, 0L, self.theValue, self.dataType, self}
              RETURN, thisEvent
           ENDIF

           IF self.event_pro NE "" THEN BEGIN
              thisEvent = {FSC_Field_Event, self.tlb, event.top, 0L, self.theValue, self.dataType, self}
              RETURN, thisEvent
           ENDIF

       ENDIF

       RETURN, 0

    ENDIF ELSE BEGIN

        self.has_focus = 0L
        Widget_Control, self.textID, set_text_select=[0]

        IF self.focus THEN BEGIN

              ; Get the current contents of text widget. Validate it.

            Widget_Control, self.textID, Get_Value=newText
            newText = newText[0]
            validText = self->Validate(newText)

               ; Load the valid text.

           self.theText = validText
           testValue  = self->ReturnValue(validText)
           IF String(testValue) EQ "NULLVALUE" THEN BEGIN
               Ptr_Free, self.theValue
               self.theValue = Ptr_New(/Allocate_Heap)
           ENDIF ELSE *self.theValue = testValue

           ; Send the keyboard focus events if there is an event handler to accept them.
           IF self.event_func NE "" THEN BEGIN
              thisEvent = {FSC_Field_Event, self.tlb, event.top, 0L, self.theValue, self.dataType, self}
              RETURN, thisEvent
           ENDIF

           IF self.event_pro NE "" THEN BEGIN
              thisEvent = {FSC_Field_Event, self.tlb, event.top, 0L, self.theValue, self.dataType, self}
              RETURN, thisEvent
           ENDIF

       ENDIF

    ENDELSE

   RETURN, 0

ENDIF

   ; What kind of event is this?

possibleTypes = ['INSERT SINGLE CHARACTER', 'INSERT MULTIPLE CHARACTERS', 'DELETE TEXT', 'SELECT TEXT']
thisType = possibleTypes[event.type]

   ; Branch on event type.

CASE thisType OF

   'INSERT SINGLE CHARACTER': BEGIN

            ; If the character is a TAB see if there is something to do.

         IF event.ch EQ 9B THEN BEGIN
            self->MoveTab
            RETURN, 0
         ENDIF

            ; Get the current contents of text widget. Validate it.

         Widget_Control, self.textID, Get_Value=newText
         newText = newText[0]
         validText = self->Validate(newText)

            ; If it is valid, leave it alone. If not, go back to previous text.

         IF validText NE newText THEN BEGIN

            Widget_Control, self.textID, Set_Value=previousText, Set_Text_Select=[textLocation[0]-1,0]

         ENDIF ELSE BEGIN

            self.theText = validText
            testValue  = self->ReturnValue(validText)
            IF String(testValue) EQ "NULLVALUE" THEN BEGIN
               Ptr_Free, self.theValue
               self.theValue = Ptr_New(/Allocate_Heap)
            ENDIF ELSE *self.theValue = testValue

         ENDELSE

            ; Is this a Carriage Return event?

         IF event.ch EQ 10B then cr_event = 1
      ENDCASE

   'INSERT MULTIPLE CHARACTERS': BEGIN

            ; Same as above, but for all the characters you are inserting.

         Widget_Control, self.textID, Get_Value=newText
         newText = newText[0]
         validText = self->Validate(newText)
         IF validText NE newText THEN BEGIN
            Widget_Control, self.textID, Set_Value=previousText, Set_Text_Select=[textLocation[0]-1,0]
         ENDIF ELSE BEGIN
            self.theText = validText
            testValue  = self->ReturnValue(validText)
            IF String(testValue) EQ "NULLVALUE" THEN BEGIN
               Ptr_Free, self.theValue
               self.theValue = Ptr_New(/Allocate_Heap)
            ENDIF ELSE *self.theValue = testValue
         ENDELSE
      ENDCASE

   'DELETE TEXT': BEGIN

            ; Get the current contents of text widget. Validate it.

         Widget_Control, self.textID, Get_Value=newText
         newText = newText[0]
         validText = self->Validate(newText)

            ; Load the valid text.

        Widget_Control, self.textID, Set_Value=validText, Set_Text_Select=[textLocation[0],0]
        self.theText = validText
        testValue  = self->ReturnValue(validText)
        IF String(testValue) EQ "NULLVALUE" THEN BEGIN
            Ptr_Free, self.theValue
            self.theValue = Ptr_New(/Allocate_Heap)
        ENDIF ELSE *self.theValue = testValue

      ENDCASE

   'SELECT TEXT': ; Nothing to do.

ENDCASE

   ; Do you report all events, or only Carriage Return events?

IF self.cr_only THEN BEGIN

   IF self.event_func NE "" THEN BEGIN
      thisEvent = {FSC_Field_Event, self.tlb, event.top, 0L, self.theValue, self.dataType, self}
      IF cr_event THEN RETURN, thisEvent
   ENDIF

   IF self.event_pro NE "" THEN BEGIN
      thisEvent = {FSC_Field_Event, self.tlb, event.top, 0L, self.theValue, self.dataType, self}
      IF cr_event THEN RETURN, thisEvent
   ENDIF

ENDIF ELSE BEGIN

   IF self.event_func NE "" THEN BEGIN
      thisEvent = {FSC_Field_Event, self.tlb, event.top, 0L, self.theValue, self.dataType, self}
      RETURN, thisEvent
   ENDIF

   IF self.event_pro NE "" THEN BEGIN
      thisEvent = {FSC_Field_Event, self.tlb, event.top, 0L, self.theValue, self.dataType, self}
      RETURN, thisEvent
   ENDIF

ENDELSE

RETURN, 0
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO FSC_Field::GetProperty, $
;
; This method allows you to obtain various properties of the compound widget via output keywords.
;
   CR_Only=cr_only, $               ; Set this keyword if you only want Carriage Return events.
   DataType=datatype, $             ; The datatype of the compound widget.
   Decimal=decimal, $               ; The number of digits to the right of the decimal point in FLOAT numbers.
   Digits=digits, $                 ; The number of digits permitted in INTERGERVALUE and LONGVALUE numbers.
   Event_Func=event_func, $         ; Set this keyword to the name of an Event Function.
   Event_Pro=event_pro, $           ; Set this keyword to the name of an Event Procedure.
   Focus_Events=focus_events, $     ; Set this keyword to inquire whether or not events are reported when the keyboard focus is lost.
   Has_Focus=has_focus, $           ; Set this keyword to inquire whether of not the widget has the keyboard focus currently.
   Highlight=highlight, $           ; The highlight flag.
   Name=Name, $                     ; The name of the object.
   NoEdit=noedit, $                 ; Setting this keywords makes the text widget non-editable.
   NonSensitive=nonsensitive, $     ; Setting this keywords makes the text widget non-sensitive.
   Undefined=undefined, $           ; The "value" of any undefined value.
   UValue=uvalue, $                 ; A user value for any purpose.
   Value=value                      ; The "value" of the compound widget.

   ; Error Handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_Field_Error_Message(/Traceback)
   RETURN
ENDIF

   ; Get the properties.

cr_only = self.cr_only
datatype = self.datatype
decimal = self.decimal
digits = self.digits
event_func = self.event_func
event_pro = self.event_pro
focus_events = self.focus
has_focus = self.has_focus
highlight = self.highlight
name = self.name
noedit = self.noedit
nonsensitive = self.nonsensitive
Widget_Control, self.tlb, Get_UValue=uvalue
IF N_Elements(*self.theValue) EQ 0 THEN value = *self.undefined ELSE value = *self.thevalue
undefined = *self.undefined

END ;--------------------------------------------------------------------------------------------------------------



PRO FSC_Field::SetProperty, $
;
; This method allows you to set various properties of the compound widget.
;
   CR_Only=cr_only, $               ; Set this keyword if you only want Carriage Return events.
   Decimal=decimal, $               ; Set this keyword to the number of digits to the right of the decimal point in FLOAT values..
   Digits=digits, $                 ; Set this keyword to the number of digits permitted in INTEGER values.
   Event_Func=event_func, $         ; Set this keyword to the name of an Event Function.
   Event_Pro=event_pro, $           ; Set this keyword to the name of an Event Procedure.
   Focus_Events=focus_events, $     ; Set this keyword to turn on or off event reporting when the keyboard focus is lost.
   Highlight=highlight, $           ; Set this keyword to highlight the text when it gets keyboard focus.
   LabelSize=labelsize, $           ; The X screen size of the Label Widget.
   Name=name, $                     ; A scalar string name for the object.
   NoEdit=noedit, $                 ; Setting this keywords makes the text widget non-editable.
   NonSensitive=nonsensitive, $     ; Setting this keywords makes the text widget non-sensitive.
   Scr_XSize=scr_xsize, $           ; The X screen size of the text widget.
   Scr_YSize=scr_ysize, $           ; The Y screen size of the text widget.
   Title=title, $                   ; The text to go on the Label Widget.
   Undefined=undefinded, $          ; Set to "value" of undefined value.
   UValue=uvalue, $                 ; A user value for any purpose.
   Value=value, $                   ; The "value" of the compound widget.
   XSize=xsize                      ; The X size of the Text Widget.

   ; Error Handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_Field_Error_Message(/Traceback)
   RETURN
ENDIF

   ; Set the properties, if needed.

IF N_Elements(cr_only) NE 0 THEN self.cr_only = cr_only
IF Keyword_Set(decimal)THEN self.decimal = decimal
IF Keyword_Set(digits)THEN self.digits = digits
IF N_Elements(event_func) NE 0 THEN self.event_func = event_func
IF N_Elements(event_pro) NE 0 THEN self.event_pro = event_pro
IF N_Elements(focus_events) NE 0 THEN self.focus = keyword_set(focus)
IF N_Elements(highlight) NE 0 THEN self.highlight = Keyword_Set(highlight)
IF N_Elements(labelsize) NE 0 THEN BEGIN
   Widget_Control, self.labelID, XSize=labelsize
ENDIF
IF N_Elements(scr_xsize) NE 0 THEN BEGIN
   Widget_Control, self.textID, Scr_XSize=scr_xsize
ENDIF
IF N_Elements(scr_ysize) NE 0 THEN BEGIN
   Widget_Control, self.textID, Scr_YSize=scr_ysize
ENDIF
IF N_Elements(title) NE 0 THEN Widget_Control, self.labelID, Set_Value=title
IF N_Elements(uvalue) NE 0 THEN Widget_Control, self.tlb, Set_UValue=uvalue
If N_Elements(name) NE 0 Then self.name = String(name[0])
IF N_Elements(xsize) NE 0 THEN BEGIN
   Widget_Control, self.textID, XSize=xsize
ENDIF
IF N_Elements(noedit) THEN BEGIN
   self.noedit = Keyword_Set(noedit)
   Widget_Control, self.textID, Editable=1-self.noedit
ENDIF
IF N_Elements(nonsensitive) THEN BEGIN
   self.nonsensitive = Keyword_Set(nonsensitive)
   Widget_Control, self.textID, Sensitive=1-self.nonsensitive
ENDIF

   ; Set up the value.

IF N_Elements(value) NE 0 THEN BEGIN

   ; Set up data type and general type.

   dataType = Size(value, /TNAME)
   CASE dataType OF
      'BYTE'   : BEGIN
         genType = 'INTEGER'
         dataType = 'INT'
         positive = 1
         *self.theValue = Fix(*self.theValue)
         Message, 'BYTE data not supported. Value will be converted to INT.', /Informational
         END
      'INT'    : genType = 'INTEGER'
      'LONG'   : genType = 'INTEGER'
      'LONG64' : genType = 'INTEGER'
      'UINT'   : genType = 'UNSIGNED'
      'ULONG'  : genType = 'UNSIGNED'
      'ULONG64': genType = 'UNSIGNED'
      'FLOAT'  : genType = 'FLOAT'
      'DOUBLE' : genType = 'DOUBLE'
      'STRING' : genType = 'STRING'
      ELSE     : BEGIN
         Ptr_Free, self.theValue
         self.theValue = Ptr_New(/Allocate_Heap)
         Message, 'Data type ' + dataType + ' is not supported. Returning.', /NoName
         END
   ENDCASE
   CASE Size(value, /TNAME) OF
       'FLOAT': self.theText = StrTrim(String(value, FORMAT='(F0)'),2)
       'DOUBLE': self.theText = StrTrim(String(value, FORMAT='(D0)'),2)
       ELSE: self.theText = StrTrim(value, 2)
   ENDCASE
   *self.theValue = self->ReturnValue(self.theText)
   Widget_Control, self.textID, Set_Value=self.theText
   self.dataType = datatype
   self.genType = genType
   IF N_Elements(undefined) EQ 0 THEN BEGIN
   IF genType EQ 'STRING' THEN undefined = "" ELSE undefined = !VALUES.F_NAN
ENDIF

ENDIF

END ;--------------------------------------------------------------------------------------------------------------



PRO FSC_Field::SetEdit, editvalue

   ; Error Handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_Field_Error_Message(/Traceback)
   RETURN
ENDIF

IF N_Elements(editvalue) NE 0 THEN $
   Widget_Control, self.textID, Editable=Keyword_Set(editvalue)
END ;--------------------------------------------------------------------------------------------------------------


PRO FSC_Field::SetSensitive, value

   ; Error Handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_Field_Error_Message(/Traceback)
   RETURN
ENDIF

IF N_Elements(value) EQ 0 THEN value = 1
self.nonsensitive = 1-value
Widget_Control, self.textID, Sensitive=value
END ;--------------------------------------------------------------------------------------------------------------



FUNCTION FSC_Field::INIT, $         ; The compound widget FSC_Field INIT method..
   parent, $                        ; The parent widget. Required for all compound widgets.
   Column=column, $                 ; Set this keyword to have Label above Text Widget.
   CR_Only=cr_only, $               ; Set this keyword if you only want Carriage Return events.
   Decimal=decimal, $               ; Set this keyword to the number of digits to the right of the decimal point in FLOAT.
   Digits=digits, $                 ; Set this keyword to the number of digits permitted in INTEGER values.
   Event_Func=event_func, $         ; Set this keyword to the name of an Event Function.
   Event_Pro=event_pro, $           ; Set this keyword to the name of an Event Procedure.
   _Extra=extra, $                  ; Passes along extra keywords to the text widget.
   FieldFont=fieldfont, $           ; The font name for the text in the Text Widget.
   Focus_Events=focus_events, $     ; Set this keyword to enable event reporting when the keyboard focus is lost or gained.
   Frame=frame, $                   ; Set this keyword to put a frame around the compound widget.
   Highlight=highlight, $           ; If this keyword is set, the text is highlighted.
   Label_Left=label_left, $         ; Set this keyword to align the label to the left of the label.
   Label_Right=label_right, $       ; Set this keyword to align the labe to the right of the label.
   LabelFont=labelfont, $           ; The font name for the text in the Label Widget.
   LabelSize=labelsize, $           ; The X screen size of the Label Widget.
   Name=name, $                     ; A scalar string name for the object.
   NoEdit=noedit, $                 ; Setting this keywords makes the text widget non-editable.
   NonSensitive=nonsensitive, $     ; Setting this keywords makes the text widget non-sensitive.
   Positive=positive, $             ; Set this keyword to indicate only positive numbers allowed in the field.
   Scr_XSize=scr_xsize, $           ; The X screen size of the text widget.
   Scr_YSize=scr_ysize, $           ; The Y screen size of the text widget.
   Title=title, $                   ; The text to go on the Label Widget.
   Undefined=undefined, $           ; Set to the value for "undefined" field values.
   UValue=uvalue, $                 ; A user value for any purpose.
   Value=value, $                   ; The "value" of the compound widget.
   XSize=xsize                      ; The X size of the Text Widget.

   ; Error Handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_Field_Error_Message(/Traceback)
   RETURN, 0
ENDIF

   ; A parent is required.

IF N_Elements(parent) EQ 0 THEN BEGIN
   Message, 'A PARENT argument is required. Returning...', /NoName
ENDIF

   ; Check keyword values.

IF N_Elements(column) EQ 0 THEN column = 0
IF N_Elements(digits) EQ 0 THEN digits = 0 ELSE digits = Fix(digits)
IF N_Elements(decimal) EQ 0 THEN decimal = -1 ELSE decimal = Fix(decimal)
IF N_Elements(event_func) EQ 0 THEN event_func = ""
IF N_Elements(event_pro) EQ 0 THEN event_pro = ""

IF N_Elements(fieldfont) EQ 0 THEN fieldfont = FSC_Field_WidgetFont()
IF N_Elements(frame) EQ 0 THEN frame = 0
IF N_Elements(labelfont) EQ 0 THEN labelfont = FSC_Field_WidgetFont()
IF N_Elements(labelsize) EQ 0 THEN labelsize = 0
IF N_Elements(name) EQ 0 THEN name = ""
noedit = Keyword_Set(noedit)
nonsensitive = Keyword_Set(nonsensitive)
positive = Keyword_Set(positive)
IF N_Elements(scr_xsize) EQ 0 THEN scr_xsize = 0
IF N_Elements(scr_ysize) EQ 0 THEN scr_ysize = 0
IF N_Elements(title) EQ 0 THEN title = "Input Value: "
IF N_Elements(uvalue) EQ 0 THEN uvalue = ""
IF N_Elements(value) EQ 0 THEN value = ""
IF N_Elements(xsize) EQ 0 THEN xsize = 0

   ; What data type are we looking for?

dataType = Size(value, /TNAME)
CASE dataType OF
   'BYTE'   : BEGIN
      genType = 'INTEGER'
      dataType = 'INT'
      positive = 1
      value = Fix(value)
      Message, 'BYTE data not supported. Value will be converted to INT.', /Informational
      END
   'INT'    : genType = 'INTEGER'
   'LONG'   : genType = 'INTEGER'
   'LONG64' : genType = 'INTEGER'
   'UINT'   : genType = 'UNSIGNED'
   'ULONG'  : genType = 'UNSIGNED'
   'ULONG64': genType = 'UNSIGNED'
   'FLOAT'  : genType = 'FLOAT'
   'DOUBLE' : genType = 'DOUBLE'
   'STRING' : genType = 'STRING'
   ELSE     : Message, 'Data type ' + dataType + ' is not supported. Returning.', /NoName
ENDCASE

IF N_Elements(undefined) EQ 0 THEN BEGIN
   IF genType EQ 'STRING' THEN undefined = "" ELSE undefined = !VALUES.F_NAN
ENDIF

   ; Populate the object.

self.cr_only = Keyword_Set(cr_only)
self.datatype = datatype
self.decimal = decimal
self.digits = digits
self.focus = Keyword_Set(focus_events)
self.highlight = Keyword_Set(highlight)
self.gentype = genType
self.parent = parent
self.event_pro = event_pro
self.event_func = event_func
self.positive = positive
self.undefined = Ptr_New(undefined)
self.noedit = noedit
self.nonsensitive = nonsensitive
If N_Elements(name) NE 0 Then self.name = String(name[0])
IF Keyword_Set(column) THEN row = 0 ELSE row = 1

   ; Validate the input value.

IF self.gentype EQ 'DOUBLE' THEN value = cgDblToStr(value) ELSE value = StrTrim(value,2)
value = self->Validate(value)
self.theText = value

   ; Create the widgets.

self.tlb = Widget_Base( parent, $  ; The top-level base of the compound widget.
   Frame=frame, $
   Row=row, $
   Column=Keyword_Set(column), $
   Base_Align_Center=1, $
   UName=name, $
   UValue=uvalue, $
   Event_Pro=event_pro, $
   Func_Get_Value='FSC_Field_Get_Compound_Widget_Value', $
   Pro_Set_Value='FSC_Field_Set_Compound_Widget_Value', $
   Event_Func=event_func )

; Update for tabbing in IDL 6.2.
IF Float(!Version.Release) GT 6.1 THEN BEGIN
   Widget_Control, self.tlb, TAB_MODE=1
ENDIF

self.labelID = Widget_Label( self.tlb, Value=title, $ ; The Label Widget.
  Scr_XSize=labelsize, UValue=self, Align_Left=Keyword_Set(label_left), Align_Right=Keyword_Set(label_right))

self.textID = Widget_Text( self.tlb, $  ; The Text Widget.
   Value=value, $
   XSize=xsize, $
   YSize=1, $
   Scr_XSize=scr_xsize, $
   Scr_YSize=scr_ysize, $
   sensitive=1-self.nonsensitive, $
   Font=fieldfont, $
   All_Events=1, $
   _Extra=extra, $
   kbrd_focus_events=1, $
   Event_Func='FSC_Field_Event_Handler', $
   UValue={Method:"TextEvents", Object:self}, $
   Kill_Notify='FSC_Field_Kill_Notify', $
   Editable=1-noedit )

self.theValue = Ptr_New(self->ReturnValue(value))

   ; If CR_ONLY or FOCUS_EVENTS are turned on and EVENT_PRO and EVENT_FUNC keywords are
   ; unspecified, issue a warning message to the command log.

IF self.cr_only AND (self.event_pro EQ "" AND self.event_func EQ "") THEN $
   Message, /Information, 'WARNING: There is no specified event handler for carriage return events for this widget. Events will be swallowed.'
IF self.focus AND (self.event_pro EQ "" AND self.event_func EQ "") THEN $
   Message, /Information, 'WARNING: There is no specified event handler for keyboard focus events for this widget. Events will be swallowed.'

RETURN, 1
END ;--------------------------------------------------------------------------------------------------------------



PRO FSC_Field::CLEANUP

; This method makes sure there are not pointers left on the heap.

Ptr_Free, self.theValue
Ptr_Free, self.undefined
END ;--------------------------------------------------------------------------------------------------------------



FUNCTION FSC_Field_Event_Handler, event

; The main event handler for the compound widget. It reacts
; to "messages" in the UValue of the text widget.
; The message indicates which object method to call. A message
; consists of an object method and the self object reference.

Widget_Control, event.ID, Get_UValue=theMessage
event = Call_Method(theMessage.method, theMessage.object, event)

RETURN, event
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO FSC_Field_Event__Define

; The FSC_Field Event Structure. Sent only if EVENT_PRO or EVENT_FUNC keywords
; have defined an event handler for the top-level base of the compound widget.

   event = { FSC_Field_Event, $   ; The name of the event structure.
             ID: 0L, $            ; The ID of the compound widget's top-level base.
             TOP: 0L, $           ; The widget ID of the top-level base of the hierarchy.
             HANDLER: 0L, $       ; The event handler ID. Filled out by IDL.
             Value: Ptr_New(), $  ; A pointer to the widget value.
             Type: "", $          ; A string indicating the type of data in the VALUE field.
             Object: Obj_New()}   ; The "self" object.


END ;-----------------------------------------------------------------------------------------------------------------------------



PRO FSC_Field_Set_Compound_Widget_Value, tlb, value

; This utilty routine is invoked when the user tries to set
; the value of the compound widget using the base widget
; identifier of the top-level base of the compound widget.
; The self object is located, and the Set_Value method is called
; on the object.

firstChildID = Widget_Info(tlb, /Child)
Widget_Control, firstChildID, Get_UValue=self
self->Set_Value, value
END ;--------------------------------------------------------------------------------------------------------------



FUNCTION FSC_Field_Get_Compound_Widget_Value, tlb

; This utilty routine is invoked when the user tries to get
; the value of the compound widget using the base widget
; identifier of the top-level base of the compound widget.
; The self object is located, and the Get_Value method is called
; on the object.

firstChildID = Widget_Info(tlb, /Child)
Widget_Control, firstChildID, Get_UValue=self
IF N_Elements(self->Get_Value()) EQ 0 THEN RETURN, *self.undefined ELSE RETURN, self->Get_Value()
END ;--------------------------------------------------------------------------------------------------------------



PRO FSC_Field_Kill_Notify, textID

; This widget call-back procedure makes sure the self object is
; destroyed when the widget is destroyed.

Widget_Control, textID, Get_UValue=message
Obj_Destroy, message.object
END ;--------------------------------------------------------------------------------------------------------------



PRO FSC_Field__Define

   objectClass = { FSC_FIELD, $             ; The object class name.
                   parent: 0L, $            ; The parent widget ID.
                   tlb: 0L, $               ; The top-level base of the compound widget.
                   labelID: 0L, $           ; The label widget ID.
                   textID: 0L, $            ; The text widget ID.
                   theText: "", $           ; The actual text in the text widget.
                   theValue: Ptr_New(), $   ; The actual "value" of the text in the text widget. :-)
                   event_func: "", $        ; The name of the specified event handler function.
                   event_pro: "", $         ; The name of the specified event handler procedrue
                   cr_only: 0L, $           ; A flag meaning send only carriage return events.
                   focus: 0L, $             ; A flag to indicate that events should be reported if the keyboard focus is lost
                   has_focus: 0L, $         ; A flag to indicate that the widget has the keyboard focus currently.
                   highlight: 0L, $         ; A flag for highlighting.
                   tabnext: 0L, $           ; The identifier of a widget to receive the cursor focus if a TAB character is detected.
                   decimal: 0, $            ; The number of decimals points in FLOAT and DOUBLE numbers.
                   digits: 0, $             ; The number of digits in INT and LONG numbers.
                   positive: 0, $           ; A flag meaning only positive numbers allowed.
                   datatype: "",$           ; The type of data to be returned from the text widget.
                   gentype: "", $           ; The "general" type of data: INTEGER, UNSIGNED, FLOAT, or STRING.
                   undefined: Ptr_New(), $  ; The "undefined" value. Used in Get_Value methods, etc.
                   noedit: 0L, $            ; A flag indicating whether text widget is editable (0) or not (1).
                   nonsensitive: 0L, $      ; A flag indicating whether text widget is sensitive (0) or not (1).
                   name:"" $                ; a scalar string name for the object
                  }

END ;--------------------------------------------------------------------------------------------------------------



FUNCTION FSC_FIELD, $               ; The compound widget FSC_Field.
   parent, $                        ; The parent widget. Required for all compound widgets.
   Column=column, $                 ; Set this keyword to have Label above Text Widget.
   CR_Only=cr_only, $               ; Set this keyword if you only want Carriage Return events.
   Decimal=decimal, $               ; Set this keyword to the number of digits to the right of the decimal point in FLOAT numbers.
   Digits=digits, $                 ; Set this keyword to the number of digits permitted in INTEGER numbers.
   Event_Func=event_func, $         ; Set this keyword to the name of an Event Function.
   Event_Pro=event_pro, $           ; Set this keyword to the name of an Event Procedure.
   _Extra=extra, $                  ; Passes along extra keywords to the text widget.
   FieldFont=fieldfont, $           ; The font name for the text in the Text Widget.
   Focus_Events=focus_events, $     ; Set this keyword to enable event reporting when the keyboard focus is lost.
   Frame=frame, $                   ; Set this keyword to put a frame around the compound widget.
   Highlight=highlight, $           ; If this keyword is set, the text is highlighted.
   Label_Left=label_left, $         ; Set this keyword to align the label to the left of the label.
   Label_Right=label_right, $       ; Set this keyword to align the labe to the right of the label.
   LabelFont=labelfont, $           ; The font name for the text in the Label Widget.
   LabelSize=labelsize, $           ; The X screen size of the Label Widget.
   Name=name, $                     ; The name of the object.
   NoEdit=noedit, $                 ; Setting this keywords makes the text widget non-editable.
   NonSensitive=nonsensitive, $     ; Setting this keywords makes the text widget non-sensitive.
   Object = obj, $                  ; An output keyword that contains the object reference.
   Positive=positive, $             ; Set this keyword to indicate only positive numbers allowed in the field.
   Scr_XSize=scr_xsize, $           ; The X screen size of the text widget.
   Scr_YSize=scr_ysize, $           ; The Y screen size of the text widget.
   Title=title, $                   ; The text to go on the Label Widget.
   Undefined=undefined, $           ; Set to the value for "undefined" field values.
   UValue=uvalue, $                 ; A user value for any purpose.
   Value=value, $                   ; The "value" of the compound widget.
   XSize=xsize                      ; The X size of the Text Widget.

obj = Obj_New("FSC_FIELD", $
   parent, $                        ; The parent widget. Required for all compound widgets.
   Column=column, $                 ; Set this keyword to have Label above Text Widget.
   CR_Only=cr_only, $               ; Set this keyword if you only want Carriage Return events.
   Decimal=decimal, $               ; Set this keyword to the number of digits to the right of the decimal point in FLOAT numbers.
   Digits=digits, $                 ; Set this keyword to the number of digits permitted in INTEGER numbers.
   Event_Func=event_func, $         ; Set this keyword to the name of an Event Function.
   Event_Pro=event_pro, $           ; Set this keyword to the name of an Event Procedure.
   _Extra=extra, $                  ; Passes along extra keywords to the text widget.
   FieldFont=fieldfont, $           ; The font name for the text in the Text Widget.
   Focus_events=focus_events, $     ; Set this keyword to enable event reporting when the keyboard focus is lost.
   Frame=frame, $                   ; Set this keyword to put a frame around the compound widget.
   Highlight=highlight, $           ; If this keyword is set, the text is highlighted.
   Label_Left=label_left, $         ; Set this keyword to align the label to the left of the label.
   Label_Right=label_right, $       ; Set this keyword to align the labe to the right of the label.
   LabelFont=labelfont, $           ; The font name for the text in the Label Widget.
   LabelSize=labelsize, $           ; The X screen size of the Label Widget.
   Name=name, $                     ; The name of the object.
   NoEdit=noedit, $                 ; Setting this keywords makes the text widget non-editable.
   NonSensitive=nonsensitive, $     ; Setting this keywords makes the text widget non-sensitive.
   Positive=positive, $             ; Set this keyword to indicate only positive numbers allowed in the field.
   Scr_XSize=scr_xsize, $           ; The X screen size of the text widget.
   Scr_YSize=scr_ysize, $           ; The Y screen size of the text widget.
   Title=title, $                   ; The text to go on the Label Widget.
   Undefined=undefined, $           ; Set to the value for "undefined" field values.
   UValue=uvalue, $                 ; A user value for any purpose.
   Value=value, $                   ; The "value" of the compound widget.
   XSize=xsize)                     ; The X size of the Text Widget.

IF Obj_Valid(obj) THEN RETURN, obj->GetID() ELSE RETURN, -1L
END ;--------------------------------------------------------------------------------------------------------------



PRO Example_Event, event


; An example event handler for FSC_Field.

Widget_Control, event.top, Get_UValue=info
Widget_Control, event.id, Get_UValue=thisEvent


; Not interested in losing keyboard focus events.
theName = Tag_Names(event, /Structure_Name)
IF theName EQ 'WIDGET_KBRD_EVENT' THEN BEGIN
   IF event.type EQ 0 THEN RETURN
ENDIF

; What kind of event is this?
CASE thisEvent OF
   'Field 1 Event': BEGIN

      ; Demonstate various ways to test if the value from this field is undefined.

      Print, ''
      IF N_Elements(*event.value) EQ 0 THEN Print, 'Field 1 Value is Undefined from Event Structure' ELSE $
         Print, 'Field 1 Value from Event Structure: ', *event.value

      Widget_Control, info.field1id, Get_Value=theValue
      IF Finite(theValue) EQ 0 THEN Print, 'Field 1 Value is Undefined and Assigned Value: ', theValue ELSE $
         Print, 'Field 1 Value via Widget Get_Value Function: ', theValue


      IF Finite(info.field1->Get_Value()) EQ 0 THEN Print, 'Field 1 Value is Undefined from Object Get_Value Function' ELSE $
         Print, 'Field 1 Value via Object Get_Value Function: ', info.field1->Get_Value()

      END
   'Field 2 Event': BEGIN
      Print, ''
      IF N_Elements(*event.value) EQ 0 THEN Print, 'Field 2 Value is Undefined' ELSE $
         Print, 'Field 2 Value: ', *event.value

      theValue = info.field2->Get_Value()
      IF theValue EQ -9999.0 THEN Print, 'Field 2 Value is Undefined and Assigned Value: ', theValue ELSE $
         Print, 'Field 2 Value via Object Get_Value Function: ', theValue

      END

   'Print It': BEGIN
      theValue =info.field3->Get_Value()
      Print, ''
      Print, 'Field 3 Value: ', theValue
      END
   'Set It': BEGIN
      info.field3->Set_Value, 'Coyote Rules!'
      END
   'Quit': Widget_Control, event.top, /Destroy
   'ChangeToUInt': BEGIN
      info.field1->SetProperty, Title='Unsigned:', Value=UINT(RandomU(seed, 1)*100)
      END
   'ChangeToFloat': BEGIN
      info.field1->SetProperty, Title='Float:', Value=RandomU(seed, 1)*100
      END
   'ChangeToString': BEGIN
      info.field1->SetProperty, Title='String:', Value='Coyote Jules'
      END
   'PrintFloat': BEGIN
      IF N_Elements(info.field2->Get_Value()) EQ 0 THEN Print, 'Field 2 Value is Undefined' ELSE $
         Print, 'Field 2 Value: ', info.field2->Get_Value()
      END
   'LabelSize': BEGIN
      Widget_Control, event.top, Update=0
      currentSize = info.labelsize
      info.field1->SetProperty, LabelSize=info.labelsize
      info.field2->SetProperty, LabelSize=info.labelsize
      info.field3->SetProperty, LabelSize=info.labelsize
      IF currentsize EQ 75 THEN info.labelsize = 50 ELSE info.labelsize = 75
      Widget_Control, event.top, Update=1
      END
   'MakeEditable': BEGIN
      Widget_Control, event.id, Get_Value=buttonValue
      CASE buttonValue OF
         'Make String Field Editable': BEGIN
            info.field3->SetEdit, 1
            Widget_Control, event.id, Set_Value='Make String Field Non-Editable'
            END
         'Make String Field Non-Editable': BEGIN
            info.field3->SetEdit, 0
            Widget_Control, event.id, Set_Value='Make String Field Editable'
            END
      ENDCASE
      END
   'MakeSensitive': BEGIN
      Widget_Control, event.id, Get_Value=buttonValue
      CASE buttonValue OF
         'Make String Field Sensitive': BEGIN
            info.field3->SetSensitive, 1
            Widget_Control, event.id, Set_Value='Make String Field Non-Sensitive'
            END
         'Make String Field Non-Sensitive': BEGIN
            info.field3->SetSensitive, 0
            Widget_Control, event.id, Set_Value='Make String Field Sensitive'
            END
      ENDCASE
      END
ENDCASE

IF Widget_Info(event.top, /Valid_ID) THEN Widget_Control, event.top, Set_UValue=info
END ;--------------------------------------------------------------------------------------------------------------



PRO Example, field1, field2, field3

; An example program to exercise some of the features of FSC_FIELD.

tlb = Widget_Base(Column=1)
button = Widget_Button(tlb, Value='Change First Field to Float', $
   UValue='ChangeToFloat')
button = Widget_Button(tlb, Value='Change First Field to String', $
   UValue='ChangeToString')
button = Widget_Button(tlb, Value='Change First Field to Unsigned Integer', $
   UValue='ChangeToUInt')

   ; Create an integer field, no more than 4 digits in the value.

field1id = FSC_FIELD(tlb, Title='Integer:', LabelSize=50, Digits=4, Object=field1, $
   Value=5, UValue='Field 1 Event', Event_Pro='Example_Event', /CR_Only, /Highlight)

   ; Create a floating point field. Only two decimal points to the right of the decimal.
   ; Set the Undefined value to -9999.0.

field = FSC_FIELD(tlb, Title='Double:', LabelSize=50, Value=-123456789.1234567891234D, Object=field2, Undefined=-9999.0, $
   /CR_Only, UValue='Field 2 Event', Event_Pro='Example_Event', Decimal=8, /Highlight)

   ; Create a string field.

field = FSC_FIELD(tlb, Title='String:', LabelSize=50, Value='Coyote Rules!', Object=field3, /Nonsensitive, /Highlight)

   ; Set up TABing between fields.

;field1->SetTabNext, field2->GetTextID()
;field2->SetTabNext, field3->GetTextID()
;field3->SetTabNext, field1->GetTextID()

button = Widget_Button(tlb, Value='Print Value of String', UValue="Print It")
button = Widget_Button(tlb, Value='Set Value of String', UValue='Set It')
button = Widget_Button(tlb, Value='Change Size of Labels', UValue='LabelSize')
button = Widget_Button(tlb, Value='Print Floating Value', UValue='PrintFloat')
button = Widget_Button(tlb, Value='Make String Field Editable', UValue='MakeEditable')
button = Widget_Button(tlb, Value='Make String Field Sensitive', UValue='MakeSensitive')
button = Widget_Button(tlb, Value='Quit', UValue='Quit')
Widget_Control, tlb, /Realize, Set_UValue={field1:field1, field2:field2, field3:field3, field1id:field1id, labelsize:75}
XManager, 'example', tlb, /No_Block
END
