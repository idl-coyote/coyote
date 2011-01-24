; docformat = 'rst'
;
; NAME:
;   FSC_WSet
;
; PURPOSE:
;   Allows the user to select the FSC_Window application to be the "current" application.
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
; :Description:
;   Allows the user to select the FSC_Window application to be the "current" application.
;   Selection can be made based on window index number, widget identifier, object reference,
;   or window title.
;
; :Categories:
;    Graphics
;    
; :Params:
;    selection: in, required, type=varies
;       Normally, a window index number of an FSC_Window application. But, the selection
;       can be a widget identifier, an object reference, or a window title, depending on
;       which keywords are set. The FSC_Window matching the selection is made the "current"
;       FSC_Window and the application is moved forward on the display.
;       
; :Keywords:
;     object: in, optional, type=boolean
;         If this keyword is set, the selection is assumed to be an object reference.
;     title: in, optional, type=boolean
;         If this keyword is set, the selection is assumed to be a window title. All
;         matching is done in uppercase characters.
;     widgetid: in, optional, type=boolean
;         If this keyword is set, the selection is assumed to be a widget identifier.
;          
; :Examples:
;    Used as a query routine::
;       IDL> wids = FSC_QueryWin(TITLE=titles, COUNT=count)
;       IDL> index = Where(StrUpCase(titles) EQ 'PLOT WINDOW', tcnt)
;       IDL> IF tcnt GT 0 THEN FSC_WSet, wids[index]
;       IDL> FSC_Window, 'Oplot', thisData, /AddCmd
;       
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: davidf@dfanning.com
;           Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; :History:
;     Change History::
;        Written, 23 January 2011. DWF.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
PRO FSC_WSet, selectMatch, OBJECT=object, WIDGETID=widgetID, TITLE=title

   Compile_Opt idl2
    
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        ok = Dialog_Message(!Error_State.MSG)
        RETURN
   ENDIF
   
   ; Check the match criteria.
   IF N_Elements(selectMatch) EQ 0 THEN Message, 'Must provide a window index number for selecting.'
   IF Size(selectMatch, /TNAME) EQ 'OBJREF' THEN object = 1
   IF Size(selectMatch, /TNAME) EQ 'STRING' THEN title = 1
   
   ; Get the values you need.
   wid = FSC_QueryWin(WIDGETID=tlb, OBJECT=objref, TITLE=titles, COUNT=count)
   IF count EQ 0 THEN BEGIN
        Message, 'There are no FSC_Window objects currently on the display.', /INFORMATIONAL
        RETURN
   ENDIF
   
   ; Get the window list.
   list = !FSC_Window_List
   
   ; Decide what to do based on the type of match.
   CASE 1 OF
   
        Keyword_Set(widgetID): BEGIN
            index = Where(tlb EQ selectMatch, selectCount)
            IF selectCount EQ 0 THEN Message, 'No FSC_Window matches the selection criteria.'
            END
            
        Keyword_Set(object): BEGIN
            index = Where(objref EQ selectMatch, selectCount)
            IF selectCount EQ 0 THEN Message, 'No FSC_Window matches the selection criteria.'
            END
            
        Keyword_Set(title): BEGIN
            index = Where(StrUpCase(titles) EQ StrUpCase(selectMatch), selectCount)
            IF selectCount EQ 0 THEN Message, 'No FSC_Window matches the selection criteria.'
            END

        ELSE: BEGIN
            index = Where(wid EQ selectMatch, selectCount)
            IF selectCount EQ 0 THEN Message, 'No FSC_Window matches the selection criteria.'
            END
   
   ENDCASE
   
   ; Make sure the index is a scalar.
   index = index[0]
   
   ; Move the window forward on the display.
   Widget_Control, tlb[index], /Show
   
   ; Move the matched node to the end of the list.
   list -> Move_Node, index
   
END