; docformat = 'rst'
;
; NAME:
;   cgQuery
;
; PURPOSE:
;   Provides information about any cgWindow applications currently on the display. Returns
;   the window index numbers of any cgWindow applications current on the display.
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
;   Provides information about any cgWindow applications currently on the display. Returns
;   the window index numbers of any cgWindow applications current on the display.
;
; :Categories:
;    Graphics
;    
; :Keywords:
;     count: out, optional, type=long
;         The number of cgWindow applications currently on the display.
;     current: in, optional, type=boolean
;         If set, the current cgWindow application information is returned in the result
;         of the function and in the information keywords.
;     dimensions: out, optional, type=integer
;         The dimensions of the ctWindow application, [xdim, ydim, n].
;     objectref: out, optional, type=object
;         A vector of FSC_CMDWINDOW object references for each cgWindow application currently 
;         on the display.
;     title: out, optional, type=string
;         A vector of window titles for each cgWindow application currently on the display.
;     widgetID: out, optional, type=long
;         A vector of widget identifiers of the top-level base widget for each cgWindow
;         application currently on the display.
;          
; :Returns:
;      windowIndexID: out, type=long
;          An array of window index numbers for each cgWindow application currently on the display.
;          
; :Examples:
;    Used as a query routine::
;       IDL> wids = cgQuery(TITLE=titles, COUNT=count)
;       IDL> index = Where(StrUpCase(titles) EQ 'PLOT WINDOW', tcnt)
;       IDL> IF tcnt GT 0 THEN cgSet, wids[index]
;       IDL> cgWindow, 'Oplot', thisData, /AddCmd
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
;        Added DIMENSIONS keyword to return current dimensions of cgWindows. 24 Feb 2011. DWF.
;        Made sure this program only returns information on devices that support windows. 20 July 2011. DWF.
;        
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
FUNCTION cgQuery, $
    COUNT=count, $
    CURRENT=current, $
    DIMENSIONS=dimensions, $
    OBJECTREF=objectRef, $
    TITLE=title, $
    WIDGETID=widgetID
    
    ; This can only be done in devices that support windows.
    IF ~((!D.Flags AND 256) NE 0) THEN BEGIN
        count = 0
        RETURN, -1
    ENDIF

    ; Are there cgWindow applications around?
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
    dimensions = IntArr(2, count)
    
    ; Fill them up.
    thisWindow = !D.Window
    FOR j=0,count-1 DO BEGIN
        thisItem = list -> Get_Item(j, /DEREFERENCE)
        
        ; Make sure this is a valid widget.
        IF Widget_Info(thisItem.tlb, /VALID_ID) THEN BEGIN
           widgetID[j] = thisItem.tlb
        ENDIF ELSE BEGIN
           widgetID[j] = -1
           CONTINUE
        ENDELSE
        objectRef[j] = thisItem.windowobj
        title[j] = thisItem.title
        windowIndex[j] = thisItem.wid
        dimensions[*,j] = [!D.X_Size, !D.Y_Size]
    ENDFOR
    
    ; IF you have bad indices, clean things up.
    badIndices = Where(widgetID EQ -1, badCount, COMPLEMENT=goodIndices, NCOMPLEMENT=count)
    IF badCount GT 0 THEN BEGIN
       widgetID = widgetID[goodIndices]
       objectRef[j] = objectRef[goodIndices]
       title[j] =  title[goodIndices]
       windowIndex[j] =  windowIndex[goodIndices]
       dimensions[*,j] =  dimensions[*,goodIndices]
       FOR k=0,badCount-1 DO list -> Destroy, badIndices[k], /Destroy
    ENDIF
    IF (thisWindow GE 0) && WindowAvailable(thisWindow) THEN WSet, thisWindow ELSE WSet, -1
    
    ; Return just the current values if the CURRENT keyword is set.
    IF Keyword_Set(current) THEN BEGIN
        IF (count-1) GE 0 THEN BEGIN
            windowIndex = windowIndex[count-1]
            widgetID = widgetID[count-1]
            objectRef = objectRef[count-1]
            title = title[count-1] 
            dimensions = dimensions[*,count-1]
        ENDIF ELSE BEGIN
            windowIndex = -1
            widgetID = -1
            objectRef = Obj_New()
            title = ""
            dimensions = [0,0]
        ENDELSE
    ENDIF

    ; Make sure scalar is returned if just one element.
    IF count EQ 1 THEN BEGIN
        widgetID = widgetID[0]
        objectRef = objectRef[0]
        title = title[0]
        windowIndex = windowIndex[0]
        dimensions = dimensions[*,0]
    ENDIF
    
    
    RETURN, windowIndex
    
END
