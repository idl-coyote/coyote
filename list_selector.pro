;+
; NAME:
;   LIST_SELECTOR
;
; PURPOSE:
;
;   The purpose of this function is to implement a pop-up dialog widget
;   for the purpose of selecting "names". Names can be names of variables,
;   names of files, etc. Any string array can be used.
;
; CALLING SEQUENCE:
;
;   selectedNames = List_Selector(theNames)
;
; ARGUMENTS:
;
;   theNames:       A string array of potential "names" that can be selected.
;
; KEYWORDS:
;
;   ALL:            Set this keyword if you wish all the names to be selected
;                   initially.
;
;   CANCEL:         An output keyword set to 1 if the user cancels or quits the
;                   program without hitting the Accept button. Set to 0 if a proper
;                   selection was made and the use hits the Accept button.
;                    
;   COUNT:          An output keyword containing the number of elements in the return array.
;
;   GROUP_LEADER:   The widget identifier of a widget who will be the group leader
;                   for this dialog. Passing a group leader is the *only* way to
;                   assure the dialog will be a MODAL dialog (as opposed to a blocking
;                   dialog). A GROUP_LEADER is required if you will be using this
;                   function in an IDL Virtual Machine application.
;                   
;   LABEL:          A string that will be placed on a label above the selections.
;                   If not used, no label is used in the program.
;                   
;   LIST_COUNTER:   If this keyword is set, a number is associated and displayed with 
;                   each list item, starting with the number 1.
;                   
;   TITLE:          A string that is used for the title of the dialog window. If
;                   undefined, then "Selection Widget" is used.
;                   
;   SELECTED_INDICES: An output vector of the selected indices from theNames array.
;
; RETURN VALUE:
;
;   selectedNames:  Typically, an array of selected names. If there is only one item
;                   in the selection, the variable will be a scalar string.
;
; EXAMPLE:
;
;   See the List_Selector_Test procedure below. I use the program to allow the
;   user to select the names of scientific data sets in an HDF file for further
;   reading and processing.
;
; MODIFICATION HISTORY:
;
;   Written by David W. Fanning, 11 January 2009, based on Name_Selector program.
;   Added "Accept on Double-Click" functionality. 14 January 2009. DWF.
;   Added LIST_COUNTER keyword. 25 May 2009. DWF.
;   Well, basically a RE-DO of yesterday's work, although done correctly today. 26 May 2009. DWF.
;   Fixed a problem when the user double-clicks an item in the list. 8 August 2009. DWF.
;   Double clicks are a problem with UNIX machines because <CR> sets event.clicks = 2
;      prematurely. Removed double-click functionality from all but Windows machines. 9 Feb 2012. DWF.
;
;-
;******************************************************************************************;
;  Copyright (c) 2009-2012, by Fanning Software Consulting, Inc.                           ;
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
PRO List_Selector_Button_Events, event

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   Widget_Control, event.id, GET_UVALUE=buttonValue
   Widget_Control, event.top, GET_UVALUE=info
   
   CASE buttonValue OF
   
      'SELECT_ALL': Widget_Control, (*info).listID, SET_LIST_SELECT=Indgen(N_Elements((*info).theNames))
      
      'DESELECT_ALL': Widget_Control, (*info).listID, SET_LIST_SELECT=N_Elements((*info).theNames)
               
      'DISMISS': Widget_Control, event.top, /Destroy
         
      'ACCEPT': BEGIN
         indices = Widget_Info((*info).listID, LIST_SELECT=1)
         IF indices[0] EQ -1 THEN count = 0 ELSE count = N_Elements(indices)
         IF count GT 0 THEN BEGIN
            *(*info).selectedNamePtr = {cancel:0, theNames:((*info).theNames)[indices], $
                           selectedIndices:indices}
         ENDIF ELSE BEGIN
              void = Dialog_Message('No items were selected. Returning...')
              RETURN
         ENDELSE
         Widget_Control, event.top, /Destroy
         END
         
      ELSE:
         
   ENDCASE
END ;--------------------------------------------------------------------------------


PRO List_Selector_Click_Events, event

    ; Events coming here do nothing whatsoever unless event.clicks EQ 2.
    ; UNIX machines set event.clicks = 2 whenever the Carriage Return is selected,
    ; which precludes me from using this functionality in anything other than 
    ; Windows machines.
    IF (event.clicks NE 2) || (StrUpCase(!Version.OS_Family) NE 'WINDOWS') THEN RETURN

    ; Act as if you had selected this item.
    Widget_Control, event.top, GET_UVALUE=info
    indices = event.index
    IF indices[0] EQ -1 THEN count = 0 ELSE count = N_Elements(indices)
    IF count GT 0 THEN BEGIN
        *(*info).selectedNamePtr = {cancel:0, theNames:((*info).theNames)[indices], $
                           selectedIndices:indices}
    ENDIF 
    Widget_Control, event.top, /Destroy
    
END ;--------------------------------------------------------------------------------



PRO List_Selector_Size_Events, event

;  This event handler handles the size events from the TLB.

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   Widget_Control, event.top, GET_UVALUE=info
   Widget_Control, (*info).listID, SCR_XSIZE=event.x > (*info).xmin, $
        SCR_YSIZE=(event.y - (*info).yoffset)
        
END ;--------------------------------------------------------------------------------


PRO List_Selector_Test, MORENAMES=morenames, LIST_COUNTER=list_counter, SELECTED_INDICES=selected_indices


     names = ['dog', 'cow', 'coyote', 'pig', 'elephant', 'donkey', 'aligator', 'croc', $
            'goat', 'snake', 'possum', 'bird', 'eagle', 'hamster']
     IF Keyword_Set(morenames) THEN names = [names, names, names]
     names = List_Selector(names, LABEL='Select Animals for Processing', CANCEL=cancelled, $
          LIST_COUNTER=Keyword_Set(list_counter), SELECTED_INDICES=selected_indices)
     IF ~cancelled THEN BEGIN
         Print, 'Names: ', names
         Print, 'Selected Indices: ', selected_indices
     ENDIF
END ;--------------------------------------------------------------------------------


FUNCTION List_Selector, theNames, $
    ALL=all, $
    CANCEL=cancel, $
    COUNT=count, $
    LABEL=label, $
    LIST_COUNTER=list_counter, $
    NUMCOLS=numcols, $
    SELECTED_INDICES=selected_indices, $
    TITLE=title, $
    GROUP_LEADER=group_leader

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN, ""
   ENDIF  
   
   ; Check arguments and keywords.
   IF N_Elements(theNames) EQ 0 THEN Message, 'Must pass string arguments to select.'
   numNames = N_Elements(theNames)
   IF Keyword_Set(list_counter) THEN BEGIN
        CASE 1 OF
            numNames LT 10: numbers = String(IndGen(numNames)+1, Format='(I1)')
            numNames LT 100: numbers = String(IndGen(numNames)+1, Format='(I2.2)')
            numNames LT 1000: numbers = String(IndGen(numNames)+1, Format='(I3.3)')
            ELSE: numbers = String(IndGen(numNames)+1, Format='(I4.4)')
        ENDCASE
        _theNames = numbers + ' ' + theNames
   ENDIF ELSE _theNames = theNames
   
   all = Keyword_Set(all)
   SetDefaultValue, title, 'Selection Widget'
   
   ; Define TLB as a modal widget, if the group leader is present. Otherwise,
   ; hope like hell it blocks on the XMANAGER command line! :-(
   IF N_Elements(group_leader) NE 0 THEN BEGIN
      tlb = Widget_Base(Title=title, COLUMN=1, GROUP_LEADER=group_leader, MODAL=1, $
            /BASE_ALIGN_CENTER, /TLB_SIZE_EVENTS)   
   ENDIF ELSE BEGIN
      tlb = Widget_Base(Title=title, COLUMN=1, /BASE_ALIGN_CENTER, /TLB_SIZE_EVENTS)
   ENDELSE
   IF N_Elements(label) NE 0 THEN labelID = Widget_Label(tlb, Value=label)
   
   ; Appearance fudges for different machines.
   IF !D.NAME EQ 'WIN' THEN sizeFudge = 14 ELSE sizeFudge = 18
   
   ; Calculate the size of the selection widget based on how many names you have.
   ysize = 30 < N_Elements(theNames)
   listID = Widget_List(tlb, VALUE=_theNames, YSIZE=ysize, /MULTIPLE, EVENT_PRO='List_Selector_Click_Events')
   buttonBase = Widget_Base(tlb, ROW=1, XPAD=0, YPAD=0, EVENT_PRO='List_Selector_Button_Events')
   button = Widget_Button(buttonBase, Value='Dismiss', UVALUE='DISMISS')
   button = Widget_Button(buttonBase, Value='Deselect All', UVALUE='DESELECT_ALL')
   button = Widget_Button(buttonBase, Value='Select All', UVALUE='SELECT_ALL')
   button = Widget_Button(buttonBase, Value='Accept', UVALUE='ACCEPT')
   bgeo = Widget_Info(buttonBase, /GEOMETRY)
   
   ; Set all the buttons to selected?
   IF all THEN BEGIN
      FOR j=0,numNames-1 DO Widget_Control, listID, SET_LIST_SELECT=Indgen(N_Elements(theStrings))
   ENDIF
   
   ; Display pop-up widgets in the center of the display.
   cgCenterTLB, tlb
   
   ; Need sizes for sizing the TLB, if required.
   IF N_Elements(labelID) NE 0 THEN BEGIN
        labelGeo = Widget_Info(labelID, /GEOMETRY)
        label_y = labelGeo.scr_ysize
   ENDIF ELSE label_y = 0.0
   butGeo = Widget_Info(buttonBase, /GEOMETRY)
   yoffset = label_y + butGeo.scr_ysize
   xmin = butGeo.scr_xsize
   
   ; Minimum size for selection base.
   sbgeo = Widget_Info(listID, /GEOMETRY)
   IF sbgeo.scr_xsize LT xmin THEN BEGIN
      Widget_Control, listID, SCR_XSIZE=xmin
   ENDIF
   Widget_Control, tlb, /Realize
   
   ; Set up pointers for passing info around the program and for storing results.
   selectedNamePtr = Ptr_New({cancel:1})
   info = Ptr_New({listID:listID, selectedNamePtr:selectedNamePtr, $
        theNames:theNames, yoffset:yoffset, xmin:xmin}, /NO_COPY)
   Widget_Control, tlb, SET_UVALUE=info
   
   ; This *should* block the IDL command line, but if you called this from
   ; a blocking widget program, you could run though this block with disasterous
   ; results. This is a *common* problem with Virtual Machine applications, which
   ; by definition block the IDL command line. Maybe you should have used a GROUP_LEADER.
   XManager, 'name_selector', tlb, EVENT_HANDLER='List_Selector_Size_Events'
   
   ; Return here after block is released. Set the CANCEL flag, get the names, free the pointers.
   cancel = (*selectedNamePtr).cancel
   IF cancel EQ 0 THEN BEGIN
      selectedNames    = (*selectedNamePtr).theNames 
      selected_indices = (*selectedNamePtr).selectedIndices
   ENDIF ELSE selectedNames = ""
   IF N_Elements(selected_indices) EQ 1 THEN selected_indices = selected_indices[0]
   Ptr_Free, selectedNamePtr, info
   
   ; Count them.
   count = N_Elements(selectedNames)
   
   ; Return the selected names.
   IF N_Elements(selectedNames) EQ 1 THEN RETURN, selectedNames[0] ELSE RETURN, selectedNames

END ; ----------------------------------------------------------------------------------