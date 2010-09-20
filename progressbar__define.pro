;*****************************************************************************************************
;+
; NAME:
;       PROGRESSBAR__DEFINE
;
; PURPOSE:
;
;       Creates a simple progress bar for indicating the progess of a looping
;       operation in IDL.
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

;       Utilities
;
; CALLING SEQUENCE:
;
;       progressBar = Obj_New("PROGRESSBAR")
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       BACKGROUND:    The name of the background color of the progress bar. By default, "black".
;
;       COLOR:         The name of the color for the progress bar. By default: "red".
;
;       Possible color names are those defined by FSC_COLOR:
;
;                Almond   Antique White      Aquamarine           Beige          Bisque           Black
;                  Blue     Blue Violet           Brown       Burlywood        Charcoal      Chartreuse
;             Chocolate           Coral        Cornsilk            Cyan  Dark Goldenrod       Dark Gray
;            Dark Green      Dark Khaki     Dark Orchid     Dark Salmon       Deep Pink     Dodger Blue
;             Firebrick    Forest Green            Gold       Goldenrod            Gray           Green
;          Green Yellow        Honeydew        Hot Pink      Indian Red           Ivory           Khaki
;              Lavender      Lawn Green     Light Coral      Light Cyan      Light Gray    Light Salmon
;          Light Yellow      Lime Green           Linen         Magenta          Maroon     Medium Gray
;         Medium Orchid        Moccasin            Navy           Olive      Olive Drab          Orange
;            Orange Red          Orchid  Pale Goldenrod      Pale Green          Papaya            Peru
;                  Pink            Plum     Powder Blue          Purple             Red            Rose
;            Rosy Brown      Royal Blue    Saddle Brown          Salmon     Sandy Brown       Sea Green
;              Seashell          Sienna        Sky Blue      Slate Gray            Snow    Spring Green
;            Steel Blue             Tan         Thistle          Tomato       Turquoise          Violet
;            Violet Red           Wheat           White          Yellow
;
;       FAST_LOOP:     Set this keyword if what you are doing in the loop doesn't involve
;                      any color operations and you want the progress bar to update as fast
;                      as possible. With this keyword set, the program will eliminate extra
;                      calls to FSC_COLOR, which can be slow if you are calling it, say,
;                      10,000 times!
;
;       GROUP_LEADER:  The group leader for the progress bar.
;
;       NOCANCEL:      Set this keyword to eliminate the CANCEL button from the progres bar.
;
;       PERCENT:       The initial percent on the progress bar. Used only if the START keyword is
;                      also set.
;
;       START:         Set this keyword if you wish to call the START method immediately upon initialization.
;
;       TEXT:          The textual message that goes above the progress bar. By default:
;                      "Operation in progress..."
;
;       TITLE:         The title of the progress bar window. By default: "Progress Bar".
;
;       XSIZE:         The X size of the progress bar itself. By default, 150 pixels.
;
;       YSIZE:         The Y size of the progress bar itself. By default, 10 pixels.

;
; PROCEDURE:
;
;       The user is responsible for starting, updating, checking for CANCEL events, and
;       destroying the progress indicator. The sequence of commands might look
;       like this:
;
;          progressBar = Obj_New("PROGRESSBAR")
;          progressBar -> Start
;          FOR j=0,9 DO BEGIN
;             IF progressBar -> CheckCancel() THEN BEGIN
;                ok = Dialog_Message('The user cancelled operation.')
;                RETURN
;             ENDIF
;             Wait, 0.5  ; Would probably be doing something ELSE here!
;             progressBar -> Update, (j+1)*10
;          ENDFOR
;          progressBar -> Destroy
;
;       See the example program at the end of this file for a working example of code.
;
; METHODS:
;
;       CHECKCANCEL: This function method returns a 1 if the user has clicked
;           the CANCEL button. Otherwise, it returns a 0.
;
;          cancelled = progressBar -> CheckCancel()
;          IF cancelled THEN progressBar->Destroy
;
;       DESTROY: Destroys the progress bar widgets as well as the object.
;
;          progressBar -> Destroy
;
;       GETPROPERTY: Gets certain properties of the object.
;
;          progressBar -> GetProperty, Color=currentColor
;
;       SETPROPERTY: Allows the user to set certain properties of the object.
;
;          progressBar -> SetProperty, Color='green'
;
;       START: Puts the progress bar on the display and enables it to receive events.
;
;          progressBar -> Start
;
;       UPDATE: Updates the progress bar. Requires on argument, a number between 0
;          and 100 that indicates the percent of progress bar that should be filled
;          with a color. Can optional specify TEXT that is displayed above the progress
;          bar.
;
;          progressBar -> Update, 50
;          progressBar -> Update, 50, Text='Operation 50% completed...'
;
; EXAMPLE:
;
;       See the example program at the bottom of this file.
;
; RESTRICTIONS:
;
;       Note that the progress bar cannot be run as a MODAL widget program and
;       still capture CANCEL button events. Thus, the user *may* be able to generate events
;       in the calling program while this progress bar is in operation.
;
; DEPENDENCIES:
;
;       This program requires FSC_COLOR from the Coyote Library:
;
;          http://www.dfanning.com/programs/fsc_color.pro
;
; MODIFICATION HISTORY:
;
;       Written by:  David W. Fanning, 19 September 2002.
;       Added TEXT keyword to Update method. 12 Nov 2002. DWF.
;       Added FAST_LOOP keyword. 19 Dec 2002. DWF.
;       Fixed a problem in where I was checking for CANCEL button event. 2 April 2003. DWF.
;       Removed the XMANAGER call in the START method, since it wasn't needed. 7 October 2003. DWF.
;       General maintenance updates. Added START keyword to INIT method to allow immediate
;          starting upon initialization. Added better error handling and checking. 10 October 2003. DWF.
;       Added ACCEPT button and CheckButton method. Modified Example program to demonstrate new
;          functionality. 1 December 2003. DWF.
;       Added XOFFSET and YOFFSET keywords. 4 March 2004. DWF.
;       Removed obsolete STR_SEP and replaced with STRSPLIT. 27 Oct 2004. DWF.
;       Added keyword keyword TITLE to Update method. 23 Feb 2005. Benjamin Hornberger
;       Added BACKGROUND keyword. 22 Dec 2006. DWF.
;       Added RETAIN=1 keyword to Widget_Draw command. 9 Dec 2007. DWF.
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
;
; NAME:
;       PROGRESSBAR_Cleanup
;
; PURPOSE:
;
;       This procedure makes sure the object is destroyed when the progress bar is destroyed.
;
;*****************************************************************************************************
PRO PROGRESSBAR_Cleanup, tlb

   Widget_Control, tlb, Get_UValue=self
   Obj_Destroy, self

END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR_Error_Message
;
; PURPOSE:
;
;       This function is the standard ERROR_MESSAGE error handling functionality.
;
;*****************************************************************************************************
FUNCTION PROGRESSBAR_Error_Message, theMessage, Error=error, Informational=information, $
   Traceback=traceback, NoName=noname, Title=title, _Extra=extra

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

      ; Are widgets supported?

   widgetsSupported = ((!D.Flags AND 65536L) NE 0)
   IF widgetsSupported THEN BEGIN

         ; If this is an error produced with the MESSAGE command, it is a trapped
         ; error and will have the name "IDL_M_USER_ERR".

      IF !ERROR_STATE.NAME EQ "IDL_M_USER_ERR" THEN BEGIN

         IF N_Elements(title) EQ 0 THEN title = 'Trapped Error'

            ; If the message has the name of the calling routine in it,
            ; it should be stripped out. Can you find a colon in the string?

         ; Is the calling routine an object method? If so, special processing
         ; is required. Object methods will have two colons together.

         doublecolon = StrPos(theMessage, "::")
         IF doublecolon NE -1 THEN BEGIN

            prefix = StrMid(theMessage, 0, doublecolon+2)
            submessage = StrMid(theMessage, doublecolon+2)
            colon = StrPos(submessage, ":")
            IF colon NE -1 THEN BEGIN

                  ; Extract the text up to the colon. Is this the same as
                  ; the callingRoutine? If so, strip it.

               IF StrMid(theMessage, 0, colon+StrLen(prefix)) EQ callingRoutine THEN $
                  theMessage = StrMid(theMessage, colon+1+StrLen(prefix))
            ENDIF
         ENDIF ELSE BEGIN

            colon = StrPos(theMessage, ":")
            IF colon NE -1 THEN BEGIN

                  ; Extract the text up to the colon. Is this the same as
                  ; the callingRoutine? If so, strip it.

               IF StrMid(theMessage, 0, colon) EQ callingRoutine THEN $
                  theMessage = StrMid(theMessage, colon+1)
            ENDIF

         ENDELSE


            ; Add the calling routine's name, unless NONAME is set.

         IF Keyword_Set(noname) THEN BEGIN
            answer = Dialog_Message(theMessage, Title=title, _Extra=extra, $
               Error=error, Information=information)
         ENDIF ELSE BEGIN
            answer = Dialog_Message(StrUpCase(callingRoutine) + ": " + $
               theMessage, Title=title, _Extra=extra, $
               Error=error, Information=information)
         ENDELSE

      ENDIF ELSE BEGIN

            ; Otherwise, this is an IDL system error.

         IF N_Elements(title) EQ 0 THEN title = 'System Error'

         IF StrUpCase(callingRoutine) EQ "$MAIN$" THEN $
            answer = Dialog_Message(theMessage, _Extra=extra, Title=title, $
               Error=error, Information=information) ELSE $
         IF Keyword_Set(noname) THEN BEGIN
            answer = Dialog_Message(theMessage, _Extra=extra, Title=title, $
               Error=error, Information=information)
         ENDIF ELSE BEGIN
            answer = Dialog_Message(StrUpCase(callingRoutine) + "--> " + $
               theMessage, _Extra=extra, Title=title, $
               Error=error, Information=information)
         ENDELSE
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
END


PRO PROGRESSBAR_Event, event

; This is the event handler for the program. It simply sets the CANCEL
; flag if this is a button event.

   Widget_Control, event.top, Get_UValue=self
   thisEvent = Tag_Names(event, /Structure_Name)
   IF thisEvent EQ 'WIDGET_BUTTON' THEN self -> SetProperty, Cancel=1
END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::CheckButton
;
; PURPOSE:
;
;       Returns a 1 if a button was selected. Returns 0 othewise.
;
; SYNTAX:
;
;       check = progressbar -> CheckButton()
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       ACCEPT:   If the Accept button was selected, this keyword is set to one.
;
;       CANCEL:   If the Cancel button was selected, this keyword is set to one.
;
;*****************************************************************************************************
FUNCTION PROGRESSBAR::CheckButton, ACCEPT=accept, CANCEL=cancel

; This method checks for a button event. It returns 1
; if an event has occurred and 0 otherwise.


   accept = 0
   cancel = 0
   retValue = 0

   ; Check for a Cancel button event.
   IF Widget_Info(self.cancelID, /Valid_ID) THEN BEGIN
      event = Widget_Event(self.cancelID, /NoWait)
      name = Tag_Names(event, /Structure_Name)
      IF name EQ 'WIDGET_BUTTON' THEN BEGIN
         Widget_Control, event.id, Get_Value=buttonValue
         IF buttonValue EQ 'Cancel' THEN BEGIN
            self.cancelFlag = 1
            cancel = 1
            retValue = 1
         ENDIF
      ENDIF
   ENDIF

   IF Widget_Info(self.acceptID, /Valid_ID) THEN BEGIN
      event = Widget_Event(self.acceptID, /NoWait)
      name = Tag_Names(event, /Structure_Name)
      IF name EQ 'WIDGET_BUTTON' THEN BEGIN
         Widget_Control, event.id, Get_Value=buttonValue
         IF buttonValue EQ 'Accept' THEN BEGIN
            accept = 1
            retValue = 1
         ENDIF
      ENDIF
   ENDIF


   RETURN, retValue
END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::CheckCancel
;
; PURPOSE:
;
;       Returns a 1 if the user selected the CANCEL button. Returns 0 othewise.
;
; SYNTAX:
;
;       check = progressbar -> CheckCancel()
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       None.
;
;*****************************************************************************************************
FUNCTION PROGRESSBAR::CheckCancel

; This method checks for a CANCEL Button event. It returns 1
; if an event has occurred and 0 otherwise.

      ; Check for a CANCEL button event.

   IF Widget_Info(self.cancelID, /Valid_ID) THEN BEGIN
      event = Widget_Event(self.cancelID, /NoWait)
      name = Tag_Names(event, /Structure_Name)
      IF name EQ 'WIDGET_BUTTON' THEN self.cancelFlag = 1
   ENDIF

   RETURN, self.cancelFlag
END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::Destroy
;
; PURPOSE:
;
;       Destroys both the widget hierarchy and the object.
;
; SYNTAX:
;
;      progressbar -> Destroy
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       None.
;
;*****************************************************************************************************
PRO PROGRESSBAR::Destroy

; This method takes the widget off the display and destroys the self object.

      ; Restore the old !P.Color.

   TVLCT, self.r, self.g, self.b, (255 < self.oldcolor)

      ; Destroy the object.

   Widget_Control, self.tlb, Destroy=1
   Obj_Destroy, self

END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::GETPROPERTY
;
; PURPOSE:
;
;       Allows user to get various progress bar properties.
;
; SYNTAX:
;
;       progressbar -> GetProperty, Color=currentColor
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       BACKGROUND: The name of the background color of the progress bar.
;
;       COLOR:      The name of the color for the progress bar.
;
;       FAST_LOOP:  The value of the current "fast loop" flag.
;
;       PERCENT:    The current percent amount on the progress bar.
;
;       TEXT:       The textual message that goes above the progress bar.
;
;       TLB_ID:     The widget ID of the progressbar window's top-level base.
;
;*****************************************************************************************************
PRO PROGRESSBAR::GetProperty, COLOR=color, FAST_LOOP=fast_loop, TEXT=text, $
                              TLB_ID=tlb_id, PERCENT=percent, BACKGROUND=background

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = ProgressBar_Error_Message(Traceback=1)
      RETURN
   ENDIF

   IF Arg_Present(background) THEN background = self.background
   IF Arg_Present(color) THEN color = self.color
   IF Arg_Present(fast_loop) THEN fast_loop = self.fast
   IF Arg_Present(percent) THEN percent = self.percent
   IF Arg_Present(text) THEN BEGIN
      IF Widget_Info(self.labelID, /Valid_ID) THEN BEGIN
         Widget_Control, self.labelID, Get_Value=text
         text = text[0]
      ENDIF ELSE text = ""
  ENDIF
  IF Arg_Present(tlb_id) THEN tlb_id = self.tlb

END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::SETPROPERTY
;
; PURPOSE:
;
;       Allows user to set various progress bar properties.
;
; SYNTAX:
;
;       progressbar -> SetProperty, Color='yellow'
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       BACKGROUND:    The name of the background color of the progress bar. By default, "black".
;
;       CANCEL:        Set this keyword to set the cancelFlag field in the self object.
;
;       COLOR:         The name of the color for the progress bar. By default: "red".
;
;       Possible color names are those defined by FSC_COLOR:
;
;                Almond   Antique White      Aquamarine           Beige          Bisque           Black
;                  Blue     Blue Violet           Brown       Burlywood        Charcoal      Chartreuse
;             Chocolate           Coral        Cornsilk            Cyan  Dark Goldenrod       Dark Gray
;            Dark Green      Dark Khaki     Dark Orchid     Dark Salmon       Deep Pink     Dodger Blue
;             Firebrick    Forest Green            Gold       Goldenrod            Gray           Green
;          Green Yellow        Honeydew        Hot Pink      Indian Red           Ivory           Khaki
;              Lavender      Lawn Green     Light Coral      Light Cyan      Light Gray    Light Salmon
;          Light Yellow      Lime Green           Linen         Magenta          Maroon     Medium Gray
;         Medium Orchid        Moccasin            Navy           Olive      Olive Drab          Orange
;            Orange Red          Orchid  Pale Goldenrod      Pale Green          Papaya            Peru
;                  Pink            Plum     Powder Blue          Purple             Red            Rose
;            Rosy Brown      Royal Blue    Saddle Brown          Salmon     Sandy Brown       Sea Green
;              Seashell          Sienna        Sky Blue      Slate Gray            Snow    Spring Green
;            Steel Blue             Tan         Thistle          Tomato       Turquoise          Violet
;            Violet Red           Wheat           White          Yellow
;
;       FAST_LOOP:     Set this keyword to one to allow "fast looping". Set to 0 to turn it off.
;
;       TEXT:          The textual message that goes above the progress bar.
;
;       TITLE:         The title of the progress bar window. By default: "Progress Bar".
;
;       XOFFSET:       The X offset for the progress bar when it appears.
;
;       YOFFSET:       The Y offset of the progress bar when it appears.
;
;*****************************************************************************************************
PRO PROGRESSBAR::SetProperty, $
   BACKGROUND=background, $
   CANCEL=cancel, $
   COLOR=color, $
   FAST_LOOP=fast_loop, $
   TEXT=text, $
   TITLE=title, $
   XOFFSET=xoffset, $
   YOFFSET=yoffset

   ; Error handling.

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = ProgressBar_Error_Message(Traceback=1)
      RETURN
   ENDIF

   IF N_Elements(background) NE 0 THEN BEGIN
      self.background = background
      IF self.fast THEN TVLCT, FSC_Color(self.background, /Triple), self.colorindex-1
   ENDIF
   IF N_Elements(cancel) NE 0 THEN self.cancelFlag = Keyword_Set(cancel)
   IF N_Elements(fast_loop) NE 0 THEN self.fast = fast_loop
   IF N_Elements(color) NE 0 THEN BEGIN
      self.color = color
      IF self.fast THEN TVLCT, FSC_Color(self.color, /Triple), self.colorindex
   ENDIF
   IF N_Elements(text) NE 0 THEN BEGIN
      self.text = text
      IF Widget_Info(self.labelID, /Valid_ID) THEN Widget_Control, self.labelID, Set_Value=text
   ENDIF
   IF N_Elements(title) NE 0 THEN BEGIN
      self.title = title
      IF Widget_Info(self.tlb, /Valid_ID) THEN Widget_Control, self.tlb, TLB_Set_Title=title
   ENDIF
   IF (N_Elements(xoffset) NE 0) OR (N_Elements(yoffset) NE 0) THEN BEGIN
      Widget_Control, self.tlb, XOffset=xoffset, YOffset=yoffset
   ENDIF
END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::START
;
; PURPOSE:
;
;       Puts the progress bar on the display.
;
; SYNTAX:
;
;       progressbar -> Start
;
; ARGUMENTS:
;
;       initialPercent -- The initial percentage that should be on the progress bar. By default, 0.
;
; KEYWORDS:
;
;       None.
;
;*****************************************************************************************************
PRO PROGRESSBAR::Start, initialPercent

      ; Error handling.

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = ProgressBar_Error_Message(Traceback=1)
      RETURN
   ENDIF

      ; Save the old !P.Color.

   self.oldcolor = !P.Color
   TVLCT, r, g, b, /Get
   self.r = r[self.oldcolor < 255]
   self.g = g[self.oldcolor < 255]
   self.b = b[self.oldcolor < 255]

      ; Do we want fast looping?

   IF self.fast THEN BEGIN
      self.colorindex = (!D.Table_Size-1) < !P.Color
      TVLCT, FSC_Color(self.color, /Triple), self.colorindex
      TVLCT, FSC_Color(self.background, /Triple), !P.Background
   ENDIF

      ; Find the window index number of any open display window.

   thisWindow = !D.Window

      ; Realize the widget.

   Widget_Control, self.tlb, /Realize, Map=1

      ; Get the window index number of the draw widget.

   Widget_Control, self.drawID, Get_Value=wid
   self.wid = wid
   Erase, Color=FSC_Color(self.background, !P.Background)

      ; Back to the open display window.

   IF thisWindow GE 0 THEN WSet, thisWindow

      ; Do you need a starting update?

   IF N_Elements(initialPercent) NE 0 THEN self -> Update, initialPercent

END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::Update
;
; PURPOSE:
;
;       Updates the progress bar
;
; SYNTAX:
;
;       progressbar -> Update, percent
;
; ARGUMENTS:
;
;       percent -- A value between 0 and 100 that represents the percentage of the progress
;                  bar that should be colored.
;
; KEYWORDS:
;
;       Text -- The message text which will be displayed above the
;               bar.
;
;       Title -- The title of the progressbar window to be updated
;
;*****************************************************************************************************
PRO PROGRESSBAR::Update, percent, Text=theText, Title=theTitle

; This method updates the display. PERCENT should be a value between 0 and 100.
; The text will be substituted for the message text.

   Catch, theError
   IF theError NE 0 THEN BEGIN
       Catch, /Cancel

         ; Catch a WSET error silently.

       IF !Error_State.Code EQ -386 THEN RETURN
       ok = Progressbar_Error_Message(Traceback=1)
       RETURN
   ENDIF

   percent = 0 > percent < 100
   self.percent = percent

      ; Update the progress box.

   thisWindow = !D.Window
   WSet, self.wid
   x1 = 0
   y1 = 0
   x2 = Round(self.xsize  * (percent/100.0))
   y2 = self.ysize
   IF N_Elements(theText) NE 0 THEN Widget_Control, self.labelID, Set_Value=theText
   IF N_Elements(theTitle) NE 0 THEN BEGIN
       self.title = theTitle
       IF Widget_Info(self.tlb, /Valid_ID) THEN Widget_Control, self.tlb, TLB_Set_Title=theTitle
   ENDIF
   IF self.fast THEN BEGIN
      Polyfill, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=self.colorindex
      PlotS, [0, x2-1], [0, 0], /Device, Color=self.colorindex
   ENDIF ELSE BEGIN
      Polyfill, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=FSC_Color(self.color, !P.Color)
      PlotS, [0, x2-1], [0, 0], /Device, Color=FSC_Color(self.color, !P.Color)
   ENDELSE
   IF thisWindow GE 0 AND thisWindow NE self.wid THEN WSet, thisWindow

END


;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::CLEANUP
;
; PURPOSE:
;
;       Nothing to do in this cleanup routine.
;
;*****************************************************************************************************
PRO PROGRESSBAR::CLEANUP
END


;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::INIT
;
; PURPOSE:
;
;       Implements a progress bar widget functionality.
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       ACCEPT:        If this keyword is set, and "Accept" button is created. When "ACCEPT"
;                      is discovered, a BREAK command is issued to get out of the loop.
;
;       BACKGROUND:    The name of the background color of the progress bar. By default, "black".
;
;       COLOR:         The name of the color for the progress bar. By default: "red".
;
;       Possible color names are those defined by FSC_COLOR:
;
;                Almond   Antique White      Aquamarine           Beige          Bisque           Black
;                  Blue     Blue Violet           Brown       Burlywood        Charcoal      Chartreuse
;             Chocolate           Coral        Cornsilk            Cyan  Dark Goldenrod       Dark Gray
;            Dark Green      Dark Khaki     Dark Orchid     Dark Salmon       Deep Pink     Dodger Blue
;             Firebrick    Forest Green            Gold       Goldenrod            Gray           Green
;          Green Yellow        Honeydew        Hot Pink      Indian Red           Ivory           Khaki
;              Lavender      Lawn Green     Light Coral      Light Cyan      Light Gray    Light Salmon
;          Light Yellow      Lime Green           Linen         Magenta          Maroon     Medium Gray
;         Medium Orchid        Moccasin            Navy           Olive      Olive Drab          Orange
;            Orange Red          Orchid  Pale Goldenrod      Pale Green          Papaya            Peru
;                  Pink            Plum     Powder Blue          Purple             Red            Rose
;            Rosy Brown      Royal Blue    Saddle Brown          Salmon     Sandy Brown       Sea Green
;              Seashell          Sienna        Sky Blue      Slate Gray            Snow    Spring Green
;            Steel Blue             Tan         Thistle          Tomato       Turquoise          Violet
;            Violet Red           Wheat           White          Yellow
;
;       GROUP_LEADER:  The group leader for the progress bar.
;
;       NOCANCEL:      Set this keyword to eliminate the CANCEL button from the progres bar.
;
;       PERCENT:       The initial percent on the progress bar. Used only if the START keyword is
;                      also set.
;
;       START:         Set this keyword if you wish to call the START method immediately upon initialization.
;
;       TEXT:          The textual message that goes above the progress bar. By default:
;                      "Operation in progress..."
;
;       TITLE:         The title of the progress bar window. By default: "Progress Bar".
;
;       XOFFSET:       The X offset for the progress bar when it appears.
;
;       XSIZE:         The X size of the progress bar itself. By default, 150 pixels.
;
;       YOFFSET:       The Y offset of the progress bar when it appears.
;
;       YSIZE:         The Y size of the progress bar itself. By default, 10 pixels.
;
;*****************************************************************************************************
FUNCTION PROGRESSBAR::INIT, $
              ACCEPT=accept, $             ; Set this keyword to get an Accept button.
              BACKGROUND=background, $     ; The name of the background color of the progress bar.
              COLOR=color, $               ; The name of the color of the progress bar.
              FAST_LOOP=fast_loop, $       ; The user plans to use the progress bar in a fast loop.
              GROUP_LEADER=group_leader, $ ; The identifier of the group leader widget.
              NOCANCEL=noCancel, $         ; Set this keyword to leave off the CANCEL button.
              PERCENT=percent, $           ; Initial percent of the progress bar. (Only recognized if START used.)
              START=start, $               ; Set this keyword if you wish to call the START method from INIT.
              TEXT=text, $                 ; The message text to be written over the progress bar.
              TITLE=title, $               ; The title of the top-level base widget.
              XOFFSET=xoffset, $           ; The X offset of the progress bar.
              XSIZE=xsize, $               ; The X size of the progress bar.
              YOFFSET=yoffset, $           ; The Y offset of the progress bar.
              YSIZE=ysize                  ; The Y size of the progress bar.

   ; Error handling.

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = ProgressBar_Error_Message(Traceback=1)
      RETURN, 0
   ENDIF

   ; Check keywords.

   IF N_Elements(background) EQ 0 THEN self.background = "black" ELSE self.background = background
   IF N_Elements(color) EQ 0 THEN self.color = "red" ELSE self.color = color
   self.fast = Keyword_Set(fast_loop)
   IF N_Elements(text) EQ 0 THEN text = "Operation in progress..."
   IF N_Elements(title) EQ 0 THEN title = "Progress Bar"
   IF N_Elements(xsize) EQ 0 THEN self.xsize = 150 ELSE self.xsize = xsize
   IF N_Elements(ysize) EQ 0 THEN self.ysize = 10 ELSE self.ysize = ysize

   ; Create the widgets for the progress bar.

   self.tlb = Widget_Base(Title=title, Column=1, Base_Align_Center=1, $
      Map=0, Group_Leader=group_leader, TLB_FRAME_ATTR=11)
   self.labelID = Widget_Label(self.tlb, Value=text, /Dynamic_Resize)
   self.drawID = Widget_Draw(self.tlb, XSize=self.xsize, YSize=self.ysize, RETAIN=1)
   Widget_Control, self.tlb, Set_UValue=self

   IF Keyword_Set(accept) THEN BEGIN
      buttonBase = Widget_Base(self.tlb,Row=1)
      self.acceptID = Widget_Button(buttonBase, Value='Accept')
   ENDIF ELSE self.acceptID = -1L
   IF NOT Keyword_Set(nocancel) THEN BEGIN
      IF N_Elements(buttonBase) EQ 0 THEN buttonBase = Widget_Base(self.tlb,Row=1)
      self.cancelID = Widget_Button(buttonBase, Value='Cancel')
   ENDIF ELSE self.cancelID = -1L

   ; Center the top-level base if offsets are not used. Othersize, use offsets.
   IF (N_Elements(xoffset) NE 0) OR (N_Elements(yoffset) NE 0) THEN BEGIN
      IF N_Elements(xoffset) EQ 0 THEN xoffset=0
      IF N_Elements(yoffset) EQ 0 THEN yoffset=0
      Widget_Control, self.tlb, XOFFSET=xoffset, YOFFSET=yoffset

   ENDIF ELSE BEGIN
      Device, Get_Screen_Size=screenSize
      IF screenSize[0] GT 2000 THEN screenSize[0] = screenSize[0]/2 ; Dual monitors.
      xCenter = screenSize(0) / 2
      yCenter = screenSize(1) / 2

      geom = Widget_Info(self.tlb, /Geometry)
      xHalfSize = geom.Scr_XSize / 2
      yHalfSize = geom.Scr_YSize / 2

      Widget_Control, self.tlb, XOffset = xCenter-xHalfSize, $
         YOffset = yCenter-yHalfSize
   ENDELSE

   ; Start it up?
   IF Keyword_Set(start) THEN self -> Start, percent

   RETURN, 1
END


;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR CLASS DEFINITION
;
; PURPOSE:
;
;       This is the PROGRESSBAR object's structure definition code.
;
;*****************************************************************************************************
PRO PROGRESSBAR__DEFINE

   struct = { PROGRESSBAR, $      ; The object class name.
              background: "", $   ; The name of the background color of the progress bar.
              cancelFlag: 0L, $   ; A flag to indicate that the CANCEL button was clicked.
              cancelID: 0L, $     ; The identifier of the CANCEL button.
              acceptID: 0L, $     ; The identifier of the ACCEPT button.
              color: "", $        ; The name of the color of the progress bar.
              colorindex: 0L, $   ; The color index number (set by a call to FSC_COLOR).
              drawID: 0L, $       ; The identifier of the draw widget.
              fast: 0L, $         ; A "fast loop" flag.
              labelID: 0L, $      ; The identifier of the label widget.
              oldcolor: 0L, $     ; The color index of !P.Color.
              percent: 0.0, $     ; A number between 0 and 100.
              r: 0B, $            ; The r value of !P.Color.
              g: 0B, $            ; The g value of !P.Color.
              b: 0B, $            ; The b value of !P.Color.
              text: "", $         ; The text message to be written over the progress bar.
              title: "", $        ; The title of the top-level base widget.
              tlb: 0L, $          ; The identifier of the top-level base.
              wid: 0L, $          ; The window index number of the draw widget.
              xsize: 0L, $        ; The XSize of the progress bar.
              ysize: 0L $         ; The YSize of the progress bar.
            }
END


;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR EXAMPLE PROGRAM
;
; PURPOSE:
;
;       This is the PROGRESSBAR example program that demonstrates how to use the progress bar.
;
;*****************************************************************************************************
;PRO Progressbar_Example_Event, event
;
;; Respond to program button events.
;
;Widget_Control, event.id, Get_Value=buttonValue
;
;CASE buttonValue OF
;
;   'Start Loop (Normal)': BEGIN
;
;         ; Create the progress bar.
;
;      progressbar = Obj_New('progressbar', Color='red', Text='Loop Iteration 0')
;
;         ; Place the progress bar on the display.
;
;      progressbar -> Start
;
;         ; Start the loop.
;
;      count = 0
;      FOR j=0, 1000 DO BEGIN
;
;          IF j MOD 100 EQ 0 THEN BEGIN ; Update the progess bar every 100 times through loop.
;
;               ; Did the user try to cancel the progress bar?
;
;            IF progressbar->CheckCancel() THEN BEGIN
;               ok = Dialog_Message('User cancelled operation.') ; Other cleanup, etc. here.
;               progressbar -> Destroy ; Destroy the progress bar.
;               RETURN
;            ENDIF
;
;               ; If user didn't cancel, update the progress bar. Update value
;               ; must be between 0 and 100.
;
;            progressbar -> Update, (count * 10.0), Text='Loop Iteration ' + StrTrim(j,2)
;            count = count + 1
;          ENDIF
;
;          Wait, 0.01 ; This is where you would do something useful.
;      ENDFOR
;
;         ; Destroy the progress bar when you are finished with it.
;
;      progressbar -> Destroy
;      ENDCASE
;
;   'Start Loop (Accept)': BEGIN
;
;         ; Create the progress bar.
;
;      progressbar = Obj_New('progressbar', Color='sky blue', background='yellow', Text='Loop Iteration 0', /Accept)
;
;         ; Place the progress bar on the display.
;
;      progressbar -> Start
;
;         ; Start the loop.
;
;      count = 0
;      FOR j=0, 1000 DO BEGIN
;
;          IF j MOD 100 EQ 0 THEN BEGIN ; Update the progess bar every 100 times through loop.
;
;               ; Did the user try to cancel the progress bar or did the user Accept?
;
;            IF progressBar -> CheckButton(Accept=acceptButton) THEN BEGIN
;
;               IF acceptButton THEN BEGIN
;
;                  progressbar -> Update, (count * 10.0), Text='Loop Iteration ' + StrTrim(j,2)
;                  ok = Dialog_Message('Final loop count is: '+ StrTrim(j,2))
;                  BREAK
;
;               ENDIF ELSE BEGIN
;
;               ok = Dialog_Message('User cancelled operation.') ; Other cleanup, etc. here.
;               progressbar -> Destroy ; Destroy the progress bar.
;               RETURN
;               ENDELSE
;
;            ENDIF
;
;               ; If user didn't cancel, update the progress bar. Update value
;               ; must be between 0 and 100.
;
;            progressbar -> Update, (count * 10.0), Text='Loop Iteration ' + StrTrim(j,2)
;            count = count + 1
;          ENDIF
;
;          Wait, 0.01 ; This is where you would do something useful.
;      ENDFOR
;
;         ; Destroy the progress bar when you are finished with it.
;
;      progressbar -> Destroy
;      ENDCASE
;
;   'Quit': Widget_Control, event.top, /Destroy
;
;ENDCASE
;
;END
;
;
;PRO Progressbar_Example
;tlb = Widget_Base(Column=1, Xoffset=200, Yoffset=200)
;button = Widget_Button(tlb, Value='Start Loop (Normal)')
;button = Widget_Button(tlb, Value='Start Loop (Accept)')
;quiter = Widget_Button(tlb, Value='Quit')
;Widget_Control, tlb, /Realize
;XManager, 'progressbar_example', tlb, /No_Block
;END