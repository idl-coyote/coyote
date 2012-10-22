;+
; NAME:
;   FSC_COLORSELECT
;
; PURPOSE:
;
;   The purpose of this compound widget is to provide a means for selecting
;   a new color or color table in a widget program. The program consists of
;   a label, a non-editable text widget, and a button to make a color or
;   color table selection interactively. (See the example program.)
;
; AUTHOR:
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
;   objectRef = FSC_COLORSELECT(parent, Title='Annotate Color ", Color='red')
;
; INPUT PARAMETERS:
;
;   parent -- The parent widget ID of the compound widget. Required.
;
; INPUT KEYWORDS:
;
;   BREWER:        Set if you want Brewer color tables, rather than the normal IDL color tables.
;                  This requires the file fsc_brewer.tbl to be in your IDL path.
;                  
;   CFONT:         Set to the name of a font to display the color name in.
;   
;   COLOR:         The name of the color to be displayed in the widget. If color
;                  is not used, or if it is set to a null string, then the widget
;                  will allow selection of a color table, instead of a color name.
;                  
;   CT_INDEX:      The color table index number you wish to use. The actual name of the
;                  color table will be displayed in the widget.
;                    
;   EVENT_FUNC:   Set this keyword to the name of an Event Function. If this
;                 keyword is undefined and the Event_Pro keyword is undefined,
;                 all compound widget events are handled internally and not
;                 passed on to the parent widget.
;                 
;   EVENT_PRO:    Set this keyword to the name of an Event Procedure. If this
;                 keyword is undefined and the Event_Func keyword is undefined,
;                 all compound widget events are handled internally and not
;                 passed on to the parent widget.
;                 
;   FRAME:        Set this keyword to put a frame around the compound widget.
;   
;   LABELALIGN:  Set this keyword to align label text. [0-center (default), 1-left, 2-right].
;   
;   LABELFONT:   The font name for the text in the Label Widget.
;   
;   LABELSIZE:   The X screen size of the Label Widget.
;   
;   NAME:        A scalar string name of the object. (Default = ''.)
;   
;   SCR_XSIZE:   The X screen size of the compound widget.
;   
;   SCR_YSIZE:   The Y screen size of the compound widget.
;   
;   STYLE:       The "style" of the text in the Text Widget. A value of 0 (the default)
;                capitalizes the first letter in the name. A style of 1 uses all lowercase.
;                A style of 2 uses all uppercase.
;                
;   TITLE:       The text to go on the Label Widget.
;   
;   UVALUE:      A user value for any purpose.
;   
;   XSIZE:       The X size of the Text Widget.
;
;   In addition, any keyword defined for WIDGET_TEXT, but not defined here (e.g., SENSITIVE), is
;   passed along without inspection to the text widget. Use of "extra" keywords is discouraged.
;
; COMMON BLOCKS:
;
;   None.
;
; RESTRICTIONS:
;
;   None.
;
; EVENT STRUCTURE:
;
;   All events are handled internally unless either the Event_Pro or Event_Func
;   keywords are used to assign an event handler to the compound widget. 
;
;   event = { FSC_ColorSelect_Event, $; The name of the event structure.
;             ID: 0L, $              ; The ID of the compound widget's top-level base.
;             TOP: 0L, $             ; The widget ID of the top-level base of the hierarchy.
;             HANDLER: 0L, $         ; The event handler ID. Filled out by IDL.
;             Color: "", $           ; The name of the current color or color table.
;             Index: 0L, $           ; The color table index selected if color tables are used.
;             Brewer: 0L, $          ; A flag that indicated Brewer color tables are being used.
;             NColors: 0L, $         ; The number of colors used for the color table.
;             ObjRef:Obj_New()}      ; The "self" object.
;
; GETTING and SETTING VALUES:
;
;   Almost all the properties of the widget can be obtained or set via
;   the object's GetProperty and SetProperty methods (described below).
;   But since traditional compound widgets have the ability to get and
;   set the value of the compound widget, this capability is implemented
;   as special methods: Get_Color/Set_Color and Get_Color_Index/Set_Color_Index.
;
;   To get the color of the widget, do this: color = objectRef->Get_Color()
;   To set the color of the widget, do this: objectRef->Set_Color, 'blue'.
;   Valid colors are those returned by FSC_Color. Getting and setting the
;   color table index number works similarly.
;
; EXAMPLE:
;
;   An example program is provided at the end of the FSC_COLORSELECT code. To run it,
;   type these commands:
;
;      IDL> .Compile FSC_COLORSELECT
;      IDL> Example
;
; DEPENDENCIES:
;
;   Requires the Coyote Library:
;     http://www.idlcoyote.com/programs/coyoteprograms.zip
;
; MODIFICATION HISTORY:
;
;   Written by: David W. Fanning, 26 JULY 2010.
;-
;******************************************************************************************;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc.                                ;
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
FUNCTION FSC_COLORSELECT_WidgetFont, DEFAULT=default

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




FUNCTION FSC_COLORSELECT::GetTextID

    ; This method returns the ID of the text widget of the compound widget.
    
    RETURN, self.textID
    
END ;-----------------------------------------------------------------------------------------------------------------------------


PRO FSC_COLORSELECT::Resize, newsize

    ; This method resizes the widget by making the text widget fit the new size.
    
    l = Widget_Info(self.labelID, /Geometry)
    b = Widget_Info(self.cbuttonID, /Geometry)
    Widget_Control, self.textID, Scr_XSize = (newsize - (l.scr_xsize + b.scr_xsize))
    
END ;-----------------------------------------------------------------------------------------------------------------------------


FUNCTION FSC_COLORSELECT::GetID

    ; This method returns the ID of the top-level base of the compound widget.
    
    RETURN, self.tlb
    
END ;-----------------------------------------------------------------------------------------------------------------------------


FUNCTION FSC_COLORSELECT::GetLabelSize

; This method returns the X screen size of the label widget.

geom = Widget_Info(self.labelID, /Geometry)
RETURN, geom.scr_xsize
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION FSC_COLORSELECT::GetTextSize

    ; This method returns the X screen size of the text widget.
    
    geom = Widget_Info(self.textID, /Geometry)
    RETURN, geom.scr_xsize
    
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION FSC_ColorSelect::Geometry

    ; This method returns the geometry of the compound widget.
    
    RETURN, Widget_Info(self.tlb,/Geometry)
    
END ;-----------------------------------------------------------------------------------------------------------------------------


FUNCTION FSC_ColorSelect::Get_Color

    ; This method returns the text in the Text Widget of the compound widget
    
    Widget_Control, self.textID, Get_Value=theValue
    theValue = theValue[0]
   
    RETURN, theValue
    
END ;-----------------------------------------------------------------------------------------------------------------------------


FUNCTION FSC_ColorSelect::Get_Color_Index, BREWER=brewer

    ; This method returns the current color table index of the widget. 
    ; Optionally, you can determine if this index is to a brewer color
    ; table or not.
    
    brewer=self.brewer
   
    RETURN, self.ct_index
    
END ;-----------------------------------------------------------------------------------------------------------------------------


PRO FSC_ColorSelect::Set_Color, color

; This method sets the value of the compound widget.

   ; Error Handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
       void = Error_Message()
       RETURN
   ENDIF
   
   ; Must be a string.
   IF Size(color, /TNAME) NE 'STRING' THEN Message, 'Input color must be a string.'
   
   ; Can you find this color in cgColor?
   colors = cgColor(/NAMES)
   index = Where(colors EQ StrCompress(color, /REMOVE_ALL), count)
   IF count EQ 0 THEN Message, 'The color ' + StrUpCase(color) + ' is not a valid color name.'

   ; Load the color in the widget.
   Widget_Control, self.textID, Set_Value=color
   self.color = color

END ;-----------------------------------------------------------------------------------------------------------------------------


PRO FSC_ColorSelect::Set_Color_Index, colorIndex, BREWER=brewer, TABLENAME=tableName

    ; This method sets the color table index number of the widget.
    ; Optionally, can set the brewer color tables. The TABLENAME
    ; keyword will return the color table name associated with the
    ; colorIndex number.

   ; Error Handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
       void = Error_Message()
       RETURN
   ENDIF
   
   ; Check parameters.
   IF N_Elements(colorIndex) EQ 0 THEN Message, 'Must pass a color table index number.'
   IF N_Elements(brewer) EQ 0 THEN brewer = self.brewer ELSE brewer = Keyword_Set(brewer)
   
   ; Get the color table names.
   thisDevice = !D.NAME
   Set_Plot, 'Z'
   TVLCT, r, g, b, /GET
   cgLoadCT, colorIndex, GET_NAMES=ctNames, BREWER=brewer
   tableName = ctNames[colorIndex]
   TVLCT, r, g, b
   Set_Plot, thisDevice
    
    ; Set the appropriate values.
    Widget_Control, self.textID, Set_Value=tableName
    Widget_Control, self.textID, Set_Text_Select=[StrLen(tableName)]
    self.ct_index = colorIndex
    self.brewer = brewer

END ;-----------------------------------------------------------------------------------------------------------------------------



PRO FSC_ColorSelect_Event__Define

; The FSC_ColorSelect_Event Structure. Sent only if EVENT_PRO or EVENT_FUNC keywords
; have defined an event handler for the top-level base of the compound widget.

   event = { FSC_ColorSelect_Event, $; The name of the event structure.
             ID: 0L, $              ; The ID of the compound widget's top-level base.
             TOP: 0L, $             ; The widget ID of the top-level base of the hierarchy.
             HANDLER: 0L, $         ; The event handler ID. Filled out by IDL.
             Color: "", $           ; The name of the current color or color table.
             Index: 0L, $           ; The color table index selected if color tables are used.
             Brewer: 0L, $          ; A flag that indicated Brewer color tables are being used.
             NColors: 0L, $         ; The number of colors used for the color table.
             ObjRef:Obj_New()}      ; The "self" object.


END ;-----------------------------------------------------------------------------------------------------------------------------


FUNCTION FSC_ColorSelect::PickColorEvents, event

    ; Error Handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /Cancel
       void = Error_Message()
       RETURN, 0
    ENDIF

    IF self.ct_ncolors GT 1 THEN BEGIN
        
        XCOLORS, /MODAL, CANCEL=cancelled, GROUP_LEADER=self.tlb, INDEX=self.ct_index, $
            NCOLORS=self.ct_ncolors, COLORINFO=cinfo, BREWER=self.brewer

        thisDevice = !D.NAME
        Set_Plot, 'Z'
        TVLCT, r, g, b, /GET
        cgLoadCT, cinfo.index, GET_NAMES=ctNames, BREWER=cinfo.brewer
        tableName = ctNames[cinfo.index]
        TVLCT, r, g, b
        Set_Plot, thisDevice
    
        Widget_Control, self.textID, Set_Value=tableName
        Widget_Control, self.textID, Set_Text_Select=[StrLen(tableName)]
        self.ct_index = cinfo.index
        self.brewer = cinfo.brewer

        event = { FSC_COLORSELECT_EVENT, $
            self.tlb, event.top, 0L, tableName, cinfo.index, $
            cinfo.brewer, self.ct_ncolors, self}        
    ENDIF ELSE BEGIN
        color = cgPickColorName(self.color, CANCEL=cancelled, GROUP_LEADER=self.tlb)
        IF cancelled THEN RETURN, 0
        
        ; Style the color name.
        IF color NE "" THEN BEGIN
            CASE self.style OF
               0: thisColor = CapFirstLetter(StrLowCase(color))
               1: thisColor = StrLowCase(color)
               2: thisColor = StrUpCase(color)
            ENDCASE
        ENDIF ELSE thisColor = ""
    
        self.color = color
        Widget_Control, self.textID, Set_Value=thisColor
        Widget_Control, self.textID, Set_Text_Select=[StrLen(thisColor)]
        event = { FSC_COLORSELECT_EVENT, $
            self.tlb, event.top, 0L, StrUpCase(color), self.ct_index, $
            self.brewer, self.ct_ncolors, self}
    ENDELSE
    
    IF self.event_pro EQ "" AND self.event_func EQ "" THEN event = 0
    
   RETURN, event
END

FUNCTION FSC_ColorSelect_Event_Handler, event

    ; The main event handler for the compound widget. It reacts
    ; to "messages" in the UValue of the text widget.
    ; The message indicates which object method to call. A message
    ; consists of an object method and the self object reference.
    
    Widget_Control, event.ID, Get_UValue=theMessage
    
    event = Call_Method(theMessage.method, theMessage.object, event)
    
    RETURN, event
    
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO FSC_ColorSelect::GetProperty, $
;
; This method allows you to obtain various properties of the compound widget via output keywords.
;
   BREWER=brewer, $                 ; A flag that indicates a brewer color table.
   COLOR=color, $                   ; Get the "value" of the compound widget.
   CT_INDEX=ct_index, $             ; The color table index number.
   EVENT_FUNC=event_func, $         ; Get the name of an Event Function.
   EVENT_PRO=event_pro, $           ; Get the name of an Event Procedure.
   ID=id, $                         ; The widget identifier of the compound widget.
   NAME=name, $                     ; Get the name of the object.
   TEXTID=textid, $                 ; The ID of the text widget.
   UVALUE=uvalue                    ; Get the user value of this object.

   ; Error Handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
       void = Error_Message()
       RETURN
   ENDIF

    ; Get the properties.
    brewer = self.brewer
    color = self.color
    ct_index = self.ct_index
    event_func = self.event_func
    event_pro = self.event_pro
    id = self -> GetID()
    uvalue = *self.uvalue
    name = self.name
    textid = self -> GetTextID()
    
END ;--------------------------------------------------------------------------------------------------------------



PRO FSC_ColorSelect::SetProperty, $

   ; This method allows you to set various properties of the compound widget.

   BREWER=brewer, $
   COLOR=color, $                   ; The name of a color for the compound widget.
   CT_INDEX=ct_index, $             ; The color table index number.
   EVENT_FUNC=event_func, $         ; Set this keyword to the name of an Event Function.
   EVENT_PRO=event_pro, $           ; Set this keyword to the name of an Event Procedure.
   LABELSIZE=labelsize, $           ; The X screen size of the Label Widget.
   NAME=name, $                     ; A scalar string name for the object.
   SCR_XSIZE=scr_xsize, $           ; The X screen size of the text widget.
   SCR_YSIZE=scr_ysize, $           ; The Y screen size of the text widget.
   SENSITIVE=sensitive, $           ; Set to 1 to make the widget sensitive, and to 0 to make it insensitive.
   STYLE=style, $                   ; The formating style for the text widget.
   TITLE=title, $                   ; The text to go on the Label Widget.
   UVALUE=uvalue, $                 ; A user value for any purpose.
   XSIZE=xsize                      ; The X size of the Text Widget.

   ; Error Handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
       void = Error_Message()
       RETURN
    ENDIF
    
    ; Set the properties, if needed.
    IF N_Elements(brewer) NE 0 THEN self.brewer = Keyword_Set(brewer)
    IF N_Elements(color) NE 0 THEN self.color = color
    IF N_Elements(ct_index) NE 0 THEN self.ct_index = ct_index
    IF N_Elements(event_func) NE 0 THEN self.event_func = event_func
    IF N_Elements(event_pro) NE 0 THEN self.event_pro = event_pro
    IF N_Elements(labelsize) NE 0 THEN BEGIN
       Widget_Control, self.labelID, XSize=labelsize
    ENDIF
    IF N_Elements(scr_xsize) NE 0 THEN BEGIN
       self.scr_xsize = scr_xsize
       Widget_Control, self.textID, Scr_XSize=scr_xsize
    ENDIF
    IF N_Elements(scr_ysize) NE 0 THEN BEGIN
       self.scr_ysize = scr_ysize
       Widget_Control, self.textID, Scr_YSize=scr_ysize
    ENDIF
    IF N_Elements(sensitive) NE 0 THEN Widget_Control, self.textID, Sensitive=sensitive
    IF N_Elements(style) NE 0 THEN self.style = 0 > style < 2
    IF N_Elements(title) NE 0 THEN Widget_Control, self.labelID, Set_Value=title
    IF N_Elements(uvalue) NE 0 THEN *self.uvalue = uvalue
    IF N_elements(name) NE 0 THEN self.name = String(name[0])
    IF N_Elements(xsize) NE 0 THEN Widget_Control, self.textID, XSize=xsize

END ;--------------------------------------------------------------------------------------------------------------



FUNCTION FSC_ColorSelect::INIT, $   ; The compound widget FSC_ColorSelect INIT method..
   parent, $                        ; The parent widget. Required for all compound widgets.
   Brewer=brewer, $                 ; A brewer color table, if set.
   Button_Text=button_text, $       ; The value of the selection button.
   CFont=cfont, $                   ; The font name for the text in the Text Widget.
   Color=color, $                   ; The color name.
   CT_Index=ct_index, $             ; The index of the current color table.
   CT_NColors=ct_ncolors, $         ; The number of colors in the color table. $               
   Event_Func=event_func, $         ; Set this keyword to the name of an Event Function.
   Event_Pro=event_pro, $           ; Set this keyword to the name of an Event Procedure.
   _Extra=extra, $                  ; Passes along extra keywords to the text widget.
   Frame=frame, $                   ; Set this keyword to put a frame around the compound widget.
   LabelAlign=labelalign, $         ; Set this keyword to align label text.
   LabelFont=labelfont, $           ; The font name for the text in the Label Widget.
   LabelSize=labelsize, $           ; The X screen size of the Label Widget.
   Name=name, $                     ; A scalar string name for the object.
   Scr_XSize=scr_xsize, $           ; The X screen size of the text widget.
   Scr_YSize=scr_ysize, $           ; The Y screen size of the text widget.
   Style=style, $                   ; The style of the color name: 0 (cap first letter), 1 (lowcase) 2 (uppercase)
   Title=title, $                   ; The text to go on the Label Widget.
   UValue=uvalue, $                 ; A user value for any purpose.
   XSize=xsize                      ; The X size of the Text Widget.

   ; Error Handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
       void = Error_Message()
       RETURN, 0
    ENDIF
    
    ; A parent is required.
    IF N_Elements(parent) EQ 0 THEN BEGIN
       Message, 'A PARENT argument is required. Returning...', /Informational
       RETURN, -1L
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Check keyword values. Properties of the widget are determined by
    ; whether the COLOR parameter is a null string or not.
    IF N_Elements(color) EQ 0 THEN color = ""
    IF N_Elements(ct_index) EQ 0 THEN ct_index = 0
    brewer = Keyword_Set(brewer)
    IF N_Elements(ct_ncolors) EQ 0 THEN ct_ncolors = (color EQ "") ? 256 : 1
    IF N_Elements(event_func) EQ 0 THEN event_func = ""
    IF N_Elements(event_pro) EQ 0 THEN event_pro = ""
    IF N_Elements(cfont) EQ 0 THEN cfont = FSC_ColorSelect_WidgetFont()
    IF N_Elements(frame) EQ 0 THEN frame = 0
    IF N_Elements(labelalign) EQ 0 THEN labelalign = 0
    IF N_Elements(labelfont) EQ 0 THEN labelfont = FSC_ColorSelect_WidgetFont()
    IF N_Elements(labelsize) EQ 0 THEN labelsize = 0
    IF N_Elements(style) EQ 0 THEN style = 0
    IF N_Elements(title) EQ 0 THEN title = "Input Value: "
    IF N_Elements(uvalue) EQ 0 THEN uvalue = ""
    IF N_Elements(color) EQ 0 THEN color = "Red"
    IF N_Elements(xsize) EQ 0 THEN xsize = 0
    
    ; Populate the object.
    self.color = color
    self.ct_index = ct_index
    self.brewer = brewer
    self.ct_ncolors = 0 > ct_ncolors < 256
    self.parent = parent
    self.event_pro = event_pro
    self.event_func = event_func
    self.style = style
    self.uvalue = Ptr_New(uvalue)
    If N_Elements(name) NE 0 Then self.name = String(name[0])
    
    ; Create the widgets.
    self.tlb = Widget_Base( parent, $  ; The top-level base of the compound widget.
       Frame=frame, $
       Row=row, $
       Row=1, $
       Base_Align_Center=1, $
       UValue=uvalue, $
       Event_Pro=event_pro, $
       Event_Func=event_func )
    
    self.labelID = Widget_Label( self.tlb, Value=title, Font=labelfont, $ ; The Label Widget.
      Scr_XSize=labelsize, Scr_YSize=scr_ysize, /Dynamic_Resize, $
      Align_Center=(labelalign eq 0)?1:0, $
      Align_Left=(labelalign EQ 1)?1:0, $
      Align_Right=(labelalign EQ 2)?1:0)
      
   
      
    ; Style the color name.
    IF color NE "" THEN BEGIN
        CASE style OF
           0: thisColor = CapFirstLetter(StrLowCase(color))
           1: thisColor = StrLowCase(color)
           2: thisColor = StrUpCase(color)
        ENDCASE
    ENDIF ELSE BEGIN
        thisDevice = !D.NAME
        Set_Plot, 'Z'
        TVLCT, r, g, b, /GET
        cgLoadCT, ct_index, GET_NAMES=ctNames, BREWER=brewer
        thisColor = ctNames[ct_index]
        TVLCT, r, g, b
        Set_Plot, thisDevice
    ENDELSE
    
    self.textID = Widget_Text( self.tlb, $  ; The Text Widget.
       Value=thisColor, $
       XSize=xsize, $
       YSize=1, $
       Font=colorfont, $
       All_Events=0, $
       _Extra=extra, $
       Scr_YSize=scr_ysize, $
       Event_Func='FSC_ColorSelect_Event_Handler', $
       UValue={Method:"TextEvents", Object:self}, $
       Kill_Notify='FSC_ColorSelect_Kill_Notify', $
       Editable=0 )
       
    ; Get the name of the selection button.
    IF N_Elements(button_text) EQ 0 THEN BEGIN
        IF color EQ "" THEN BEGIN
            button_text = 'Select Color Table' 
        ENDIF ELSE BEGIN
            button_text = 'Select Color'
        ENDELSE
    ENDIF
    
    ; Selection button.
    self.cbuttonID = Widget_Button( self.tlb, $
       Value=button_text, Font=labelfont, $
       Event_Func='FSC_ColorSelect_Event_Handler', $
       UValue={Method:"PickColorEvents", Object:self}, $
       Kill_Notify='FSC_ColorSelect_Kill_Notify')
    
   ; If screen sizes are set, adjust the text widget size.
   IF N_Elements(scr_xsize) NE 0 THEN BEGIN
      tlbg = Widget_Info(self.tlb, /Geometry)
      textg = Widget_Info(self.textID, /Geometry)
      labelg = Widget_info(self.labelID, /Geometry)
      plussize = textg.scr_xsize + labelg.scr_xsize
      stuff = (tlbg.xpad*2) + tlbg.space
      IF plussize GT scr_xsize THEN $
         Widget_Control, self.textID, Scr_XSize=textg.scr_xsize-(plussize-scr_xsize)-stuff ELSE $
         Widget_Control, self.textID, Scr_XSize=textg.scr_xsize+(scr_xsize - plussize)-stuff
   ENDIF
        
   RETURN, 1
   
END ;--------------------------------------------------------------------------------------------------------------



PRO FSC_ColorSelect_Kill_Notify, textID

    ; This widget call-back procedure makes sure the self object is
    ; destroyed when the widget is destroyed.
    
    Widget_Control, textID, Get_UValue=message
    Obj_Destroy, message.object
    
END ;--------------------------------------------------------------------------------------------------------------



PRO FSC_ColorSelect::CLEANUP

    ; This method makes sure there are not pointers left on the heap.
    
    Ptr_Free, self.uvalue
    
END ;--------------------------------------------------------------------------------------------------------------



PRO FSC_ColorSelect__Define

   objectClass = { FSC_ColorSelect, $       ; The object class name.
                   parent: 0L, $            ; The parent widget ID.
                   tlb: 0L, $               ; The top-level base of the compound widget.
                   cbuttonID: 0L, $         ; The identifier of the Select Color button.
                   color: "", $             ; The name of the color.
                   ct_index: 0L, $          ; The color table index number.
                   ct_ncolors: 0L, $        ; The number of colors in XCOLORS.
                   labelID: 0L, $           ; The label widget ID.
                   textID: 0L, $            ; The text widget ID.
                   brewer: 0L, $            ; Use Brewer color tables.
                   event_func: "", $        ; The name of the specified event handler function.
                   event_pro: "", $         ; The name of the specified event handler procedrue
                   style: 0L, $             ; The style of the color name.
                   uvalue: Ptr_New(), $     ; The user value of the compound widget.
                   name: "" $               ; A scalar string name for the object
                  }

  END ;--------------------------------------------------------------------------------------------------------------


FUNCTION FSC_ColorSelect, $          ; The compound widget FSC_ColorSelect.
   parent, $                        ; The parent widget. Required for all compound widgets.
   Brewer=brewer, $                 ; A brewer color table, if set.
   Button_Text=button_text, $       ; The value of the selection button.
   CFont=cfont, $                   ; The font name for the text in the Text Widget.
   Color=color, $                   ; The color name.
   CT_Index=ct_index, $             ; The index of the current color table.
   CT_NColors=ct_ncolors, $         ; The number of colors in the color table. $               
   Event_Func=event_func, $         ; Set this keyword to the name of an Event Function.
   Event_Pro=event_pro, $           ; Set this keyword to the name of an Event Procedure.
   _Extra=extra, $                  ; Passes along extra keywords to the text widget.
   Frame=frame, $                   ; Set this keyword to put a frame around the compound widget.
   LabelAlign=labelalign, $         ; Set this keyword to align label text.
   LabelFont=labelfont, $           ; The font name for the text in the Label Widget.
   LabelSize=labelsize, $           ; The X screen size of the Label Widget.
   Name=name, $                     ; A scalar string name for the object.
   Scr_XSize=scr_xsize, $           ; The X screen size of the text widget.
   Scr_YSize=scr_ysize, $           ; The Y screen size of the text widget.
   Style=style, $                   ; The style of the color name: 0 (cap first letter), 1 (lowcase) 2 (uppercase)
   Title=title, $                   ; The text to go on the Label Widget.
   UValue=uvalue, $                 ; A user value for any purpose.
   XSize=xsize                      ; The X size of the Text Widget.

RETURN, Obj_New("FSC_ColorSelect", $
   parent, $                        ; The parent widget. Required for all compound widgets.
   Brewer=brewer, $                 ; A brewer color table, if set.
   Button_Text=button_text, $       ; The value of the selection button.
   CFont=cfont, $                   ; The font name for the text in the Text Widget.
   Color=color, $                   ; The color name.
   CT_Index=ct_index, $             ; The index of the current color table.
   CT_NColors=ct_ncolors, $         ; The number of colors in the color table. $               
   Event_Func=event_func, $         ; Set this keyword to the name of an Event Function.
   Event_Pro=event_pro, $           ; Set this keyword to the name of an Event Procedure.
   _Extra=extra, $                  ; Passes along extra keywords to the text widget.
   Frame=frame, $                   ; Set this keyword to put a frame around the compound widget.
   LabelAlign=labelalign, $         ; Set this keyword to align label text.
   LabelFont=labelfont, $           ; The font name for the text in the Label Widget.
   LabelSize=labelsize, $           ; The X screen size of the Label Widget.
   Name=name, $                     ; A scalar string name for the object.
   Scr_XSize=scr_xsize, $           ; The X screen size of the text widget.
   Scr_YSize=scr_ysize, $           ; The Y screen size of the text widget.
   Style=style, $                   ; The style of the color name: 0 (cap first letter), 1 (lowcase) 2 (uppercase)
   Title=title, $                   ; The text to go on the Label Widget.
   UValue=uvalue, $                 ; A user value for any purpose.
   XSize=xsize)                     ; The X size of the Text Widget.
END ;----------------------------------------------------------------------------


PRO Example_Event, event
    Help,  event, /Structure
END ;----------------------------------------------------------------------------


PRO Example
    ; An example program to exercise some of the features of FSC_ColorSelect.
    tlb = Widget_Base(Column=1)
    field1 = FSC_COLORSELECT(tlb, Title='Color:', LabelSize=75, Color='Red', $
        LabelAlign=1, Event_Pro='Example_Event')
    field2 = FSC_COLORSELECT(tlb, Title='ColorTable:', LabelSize=75, $
        LabelAlign=1, CT_INDEX=4, /BREWER, Event_Pro='Example_Event')
    Widget_Control, tlb, /Realize
    XManager, 'example', tlb, /NO_BLOCK
END ;----------------------------------------------------------------------------
