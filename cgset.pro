; docformat = 'rst'
;
; NAME:
;   cgSet
;
; PURPOSE:
;   Allows the user to select the cgWindow application to be the "current" application.
;   Selection can be made based on window index number, widget identifier, object reference,
;   or window title.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2011, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
;+
;   Allows the user to select the cgWindow application to be the "current" application.
;   Selection can be made based on window index number, widget identifier, object reference,
;   or window title.
;
; :Categories:
;    Graphics
;    
; :Params:
;    selection: in, optional, type=varies
;       Normally, a window index number of an cgWindow application. But, the selection
;       can be a widget identifier, an object reference, or a window title, depending on
;       which keywords are set. The cgWindow matching the selection is made the "current"
;       cgWindow and the application is moved forward on the display.
;       
; :Keywords:
;     display: in, optional, type=boolean, default=0
;         If this keyword is set, the selection is made the "current" application,
;         and then the graphics window of the application is made the current graphics
;         window. If there is no selection, then the current cgWindow graphics window
;         is made the current graphics window.
;     object: in, optional, type=boolean
;         If this keyword is set, the selection is assumed to be an object reference.
;     title: in, optional, type=boolean
;         If this keyword is set, the selection is assumed to be a window title. All
;         matching is done in uppercase characters.
;     widgetid: in, optional, type=boolean
;         If this keyword is set, the selection is assumed to be a widget identifier.
;          
; :Examples:
;    Used with query routine::
;       IDL> wids = cgQuery(TITLE=titles, COUNT=count)
;       IDL> index = Where(StrUpCase(titles) EQ 'PLOT WINDOW', tcnt)
;       IDL> IF tcnt GT 0 THEN cgSet, wids[index]
;       IDL> cgWindow, 'Oplot', thisData, /AddCmd
;       IDL> cgSet ; Bring current window forwad on display
;       
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: david@idlcoyote.com
;           Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 23 January 2011. DWF.
;        If selection match isn't provided, as like WShow to bring the current 
;           window forward on display. 26 Jan 2011. DWF.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
PRO cgSet, selection, DISPLAY=display, OBJECT=object, WIDGETID=widgetID, TITLE=title

   Compile_Opt idl2
    
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        ok = Dialog_Message(!Error_State.MSG)
        RETURN
   ENDIF
   
   ; If there is no selection match, then act like WSHOW an bring the window
   ; forward on the display.
   IF N_Elements(selection) EQ 0 THEN BEGIN
      cgShow
      IF Keyword_Set(display) THEN WSet, cgQuery(/CURRENT)
      RETURN
   ENDIF
   
   ; Try to do the right thing here.
   IF Size(selection, /TNAME) EQ 'OBJREF' THEN object = 1
   IF Size(selection, /TNAME) EQ 'STRING' THEN title = 1
   
   ; Get the values you need.
   wid = cgQuery(WIDGETID=tlb, OBJECT=objref, TITLE=titles, COUNT=count)
   IF count EQ 0 THEN BEGIN
        Message, 'There are no cgWindow objects currently on the display.', /INFORMATIONAL
        RETURN
   ENDIF
   
   ; Get the window list.
   list = !FSC_Window_List
   
   ; Decide what to do based on the type of match.
   CASE 1 OF
   
        Keyword_Set(widgetID): BEGIN
            index = Where(tlb EQ selection, selectCount)
            IF selectCount EQ 0 THEN Message, 'No cgWindow matches the selection criteria.'
            END
            
        Keyword_Set(object): BEGIN
            index = Where(objref EQ selection, selectCount)
            IF selectCount EQ 0 THEN Message, 'No cgWindow matches the selection criteria.'
            END
            
        Keyword_Set(title): BEGIN
            index = Where(StrUpCase(titles) EQ StrUpCase(selection), selectCount)
            IF selectCount EQ 0 THEN Message, 'No cgWindow matches the selection criteria.'
            END

        ELSE: BEGIN
            index = Where(wid EQ selection, selectCount)
            IF selectCount EQ 0 THEN Message, 'No cgWindow matches the selection criteria.'
            END
   
   ENDCASE
   
   ; Make sure the index is a scalar.
   index = index[0]
   
   ; Move the window forward on the display.
   Widget_Control, tlb[index], /Show
   
   ; Move the matched node to the end of the list.
   list -> Move_Node, index
   
   ; Make this window the current graphics window?
   IF Keyword_Set(display) THEN WSet, cgQuery(/CURRENT)
   
END
