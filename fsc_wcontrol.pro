; docformat = 'rst'
;
; NAME:
;   FSC_WControl
;
; PURPOSE:
;   Allows the user to set various properties of an FSC_Window object. This is essentially
;   a wrapper to the FSC_Window SetProperty method.
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
;   Allows the user to set various properties of an FSC_Window object. This is essentially
;   a wrapper to the FSC_Window SetProperty method.
;
; :Categories:
;    Graphics
;    
; :Params:
;    selection: in, required, type=varies
;       Normally, a window index number of an FSC_Window application. But, the selection
;       can be a widget identifier, an object reference, or a window title, depending on
;       which keywords are set. The FSC_Window matching the selection has its properties set.
;       
; :Keywords:
;     all: in, optional, type=boolean
;         This keyword applies only to keywords that manipulate commands in the command
;         list (e.g., DeleteCmd). It will select all the commands in the command list to
;         apply the action to.
;     background: in, optional, type=string
;         The background color of the window. Only use if the ERASEIT property is also set.
;     cmdindex: in, optional, type=integer
;         This keyword applies only to keywords that manipulate commands in the command
;         list (e.g., DeleteCmd). It specifies the command index number of the command 
;         for which the action is desired.
;     colorpalette: in, optional, type=BytArr(N,3)
;         Use this keyword to pass in an N-by-3 (or 3-by-N) byte array containing the
;         R, G, and B vectors of a color table. It is probably easier to use CTLOAD or
;         XCOLORS to load color tables for the window, but this is provided as another option.
;     delay: in, optional, type=float
;         Set this keyword to the amount of "delay" you want between commands in the command list.
;     deletecmd: in, optional, type=boolean
;          Set this keyword to delete a command in the FSC_Window. The keywords cmdIndex and All
;          are used in deleting the specified command.
;     eraseit: in, optional, type=boolean
;         If this property is set, the FSC_Window erases with the background color before
;         displaying the commands in the window's command list.
;     multi: in, optional, type=Intarr(5)
;         Set this keyword to the !P.MULTI setting you want to use for the window.
;         !P.MULTI is set to this setting before command execution, and set back to
;         it's default value when the commands are finished executing.
;     object: in, optional, type=boolean
;         If this keyword is set, the selection is assumed to be an object reference.
;     title: in, optional, type=boolean
;         If this keyword is set, the selection is assumed to be a window title. All
;         matching is done in uppercase characters.
;     update: in, optional, type=boolean
;         Set this keyword if you want the window commands to be immediately executed 
;         after the property change.
;     widgetid: in, optional, type=boolean
;         If this keyword is set, the selection is assumed to be a widget identifier.
;          
; :Examples:
;    Used to set FSC_Window properties::
;       IDL> FSC_WControl, Background='gray', EraseIt=1
;       IDL> FSC_WControl, Multi=[0,2,2]
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
;        Written, 28 January 2011. DWF.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
PRO FSC_WControl, selection, $
    ALL=all, $
    BACKGROUND=background, $
    CMDINDEX=cmdIndex, $
    COLORPALETTE=colorPalette, $
    DELAY=delay, $
    DELETECMD = deleteCmd, $
    ERASEIT=eraseit, $
    LISTCMD=listCmd, $
    MULTI=multi, $
    OBJECT=object, $
    TITLE=title, $
    UPDATE=update, $
    WIDGETID=widgetID
    
   Compile_Opt idl2
    
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
   ENDIF
   
   ; If there is no selection match, use the current window. If there
   ; is no current window, create one.
   IF N_Elements(selection) EQ 0 THEN BEGIN
        selection = FSC_QueryWin(/CURRENT, COUNT=count)
        IF count EQ 0 THEN BEGIN
            FSC_Window
            selection = FSC_QueryWin(/CURRENT, COUNT=count)
        ENDIF
   ENDIF
   
   ; Try to do the right thing here.
   IF Size(selection, /TNAME) EQ 'OBJREF' THEN object = 1
   IF Size(selection, /TNAME) EQ 'STRING' THEN title = 1
   
   ; Get the values you need.
   wid = FSC_QueryWin(WIDGETID=tlb, OBJECT=objref, TITLE=titles, COUNT=count)
   IF count EQ 0 THEN Message, 'There are no FSC_Windows currently on the display.', /Infomational
   
   ; Get the window list.
   list = !FSC_Window_List
   
   ; Decide what to do based on the type of match.
   CASE 1 OF
   
        Keyword_Set(widgetID): BEGIN
            index = Where(tlb EQ selection, selectCount)
            IF selectCount EQ 0 THEN $
                Message, 'No FSC_Window matches the selection criteria.', /Infomational
            END
            
        Keyword_Set(object): BEGIN
            index = Where(objref EQ selection, selectCount)
            IF selectCount EQ 0 THEN $
                Message, 'No FSC_Window matches the selection criteria.', /Infomational
            END
            
        Keyword_Set(title): BEGIN
            index = Where(StrUpCase(titles) EQ StrUpCase(selection), selectCount)
            IF selectCount EQ 0 THEN $
                Message, 'No FSC_Window matches the selection criteria.', /Infomational
            END

        ELSE: BEGIN
            index = Where(wid EQ selection, selectCount)
            IF selectCount EQ 0 THEN $
                Message, 'No FSC_Window matches the selection criteria.', /Infomational
            END
   
   ENDCASE
   
   ; Make sure the index is a scalar.
   index = index[0]
   
   ; Are you deleting commands?
   IF N_Elements(deleteCmd) NE 0 THEN BEGIN
        objref[index] -> DeleteCommand, cmdIndex, ALL=Keyword_Set(all)
   ENDIF

   ; Are you listing the commands?
   IF N_Elements(listCmd) NE 0 THEN BEGIN
        objref[index] -> ListCommand, cmdIndex
   ENDIF
   
   ; Set the properties of the window.
   objref[index] -> SetProperty, $
        BACKGROUND=background, $
        DELAY=delay, $
        ERASEIT=eraseit, $
        COLORPALETTE=colorPalette, $
        MULTI=multi, $
        UPDATE=update
    
    
END 