; docformat = 'rst'
;
; NAME:
;   FSC_QueryWin
;
; PURPOSE:
;   Provides information about any FSC_Window applications currently on the display. Returns
;   the window index numbers of any FSC_Window applications current on the display.
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
;   Provides information about any FSC_Window applications currently on the display. Returns
;   the window index numbers of any FSC_Window applications current on the display.
;
; :Categories:
;    Graphics
;    
; :Params:
;    none
;       
; :Keywords:
;     current: in, optional, type=boolean
;         If set, the current FSC_Window application information is returned in the result
;         of the function and in the information keywords.
;     count: out, optional, type=long
;         The number of FSC_Window applications currently on the display.
;     objectref: out, optional, type=object
;         A vector of FSC_CMDWINDOW object references for each FSC_Window application currently 
;         on the display.
;     title: out, optional, type=string
;         A vector of window titles for each FSC_Window application currently on the display.
;     widgetID: out, optional, type=long
;         A vector of widget identifiers of the top-level base widget for each FSC_Window
;         application currently on the display.
;          
; :Return Value:
;      windowIndexID: out, type=long
;          An array of window index numbers for each FSC_Window application currently on the display.
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
FUNCTION FSC_QueryWin, $
    COUNT=count, $
    CURRENT=current, $
    OBJECTREF=objectRef, $
    TITLE=title, $
    WIDGETID=widgetID

    ; Are there FSC_Window applications around?
    DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
    
    ; If it doesn't exist, or it is invalid, leave.
    IF ~exists THEN BEGIN
        count = 0
        RETURN, -1
    ENDIF ELSE BEGIN
        IF ~Obj_Valid(!FSC_WINDOW_LIST) THEN BEGIN
            count = 0
            RETURN, -1
        ENDIF   
    ENDELSE
    
    ; Get the window list and find out how many windows there are.
    list = !FSC_WINDOW_LIST
    count = list -> Get_Count()
    
    ; Make arrays to hold the values.
    widgetID = LonArr(count)
    objectRef = ObjArr(count)
    title = StrArr(count)
    windowIndex = IntArr(count)
    
    ; Fill them up.
    FOR j=0,count-1 DO BEGIN
        thisItem = list -> Get_Item(j, /DEREFERENCE)
        widgetID[j] = thisItem.tlb
        objectRef[j] = thisItem.windowobj
        title[j] = thisItem.title
        windowIndex[j] = thisItem.wid
    ENDFOR
    
    ; Return just the current values if the CURRENT keyword is set.
    IF Keyword_Set(current) THEN BEGIN
        IF (count-1) GE 0 THEN BEGIN
            windowIndex = windowIndex[count-1]
            widgetID = widgetID[count-1]
            objectRef = objectRef[count-1]
            title = title[count-1] 
        ENDIF ELSE BEGIN
            windowIndex = -1
            widgetID = -1
            objectRef = Obj_New()
            title = ""
        ENDELSE
    ENDIF

    ; Make sure scalar is returned if just one element.
    IF count EQ 1 THEN BEGIN
        widgetID = widgetID[0]
        objectRef = objectRef[0]
        title = title[0]
        windowIndex = windowIndex[0]
    ENDIF
    
    RETURN, windowIndex
    
END