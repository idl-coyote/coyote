;+
; NAME:
;   NAME_SELECTOR
;
; PURPOSE:
;
;   The purpose of this function is to implement a pop-up dialog widget
;   for the purpose of selecting "names". Names can be names of variables,
;   names of files, etc. Any string array can be used.
;
; CALLING SEQUENCE:
;
;   selectedNames = Name_Selector(theNames)
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
;   NUMCOLS:        The number of columns to organize the string array in. The default
;                   is to use one column per approximately 20 strings.
;                   
;   TITLE:          A string that is used for the title of the dialog window. If
;                   undefined, then "Selection Widget" is used.
;
; RETURN VALUE:
;
;   selectedNames:  Typically, an array of selected names. If there is only one item
;                   in the selection, the variable will be a scalar string.
;
; EXAMPLE:
;
;   See the Name_Selector_Test procedure below. I use the program to allow the
;   user to select the names of scientific data sets in an HDF file for further
;   reading and processing.
;
; MODIFICATION HISTORY:
;
;   Written by David W. Fanning, 21 December 2008.
;   Added a COUNT keyword. DWF. 6 January 2009.
;
;-
;******************************************************************************************;
;  Copyright (c) 2008-2009, by Fanning Software Consulting, Inc.                           ;
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
PRO Name_Selector_Button_Events, event

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   Widget_Control, event.id, GET_UVALUE=buttonValue
   Widget_Control, event.top, GET_UVALUE=info
   
   CASE buttonValue OF
   
      'SELECT_ALL': BEGIN
         FOR j=0,N_Elements((*info).listbuttons)-1 DO BEGIN
            Widget_Control, (*info).listbuttons[j], SET_BUTTON=1
         ENDFOR
         END
         
      'DESELECT_ALL': BEGIN
         FOR j=0,N_Elements((*info).listbuttons)-1 DO BEGIN
            Widget_Control, (*info).listbuttons[j], SET_BUTTON=0
         ENDFOR
         END
         
      'DISMISS': BEGIN
         Widget_Control, event.top, /Destroy
         END
         
      'ACCEPT': BEGIN
         buttonsOn = IntArr(N_Elements((*info).listbuttons))
         FOR j=0,N_Elements((*info).listbuttons)-1 DO BEGIN
            buttonsOn[j] = Widget_Info((*info).listbuttons[j], /BUTTON_SET)
         ENDFOR
         index = Where(buttonsOn EQ 1, count)
         IF count GT 0 THEN BEGIN
            *(*info).selectedNamePtr = {cancel:0, theNames:((*info).theNames)[index]}
         ENDIF ELSE BEGIN
              void = Dialog_Message('No items were selected. Returning...')
              RETURN
         ENDELSE
         Widget_Control, event.top, /Destroy
         END
         
      ELSE:
         
   ENDCASE
END ;--------------------------------------------------------------------------------


PRO Name_Selector_Null_Events, event
; Events coming here do nothing whatsoever.
END ;--------------------------------------------------------------------------------



PRO Name_Selector_Size_Events, event

;  This event handler handles the size events from the TLB.

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   Widget_Control, event.top, GET_UVALUE=info

   Widget_Control, (*info).selectionBase, XSIZE=event.x > (*info).xmin, $
        YSIZE=(event.y - (*info).yoffset)
        
END ;--------------------------------------------------------------------------------


PRO Name_Selector_Test, MORENAMES=morenames, NUMCOLS=numcols

    names = ['dog', 'cow', 'coyote', 'pig', 'elephant', 'donkey', 'aligator', 'croc', $
            'goat', 'snake', 'possum', 'bird', 'eagle', 'hamster']
     IF Keyword_Set(morenames) THEN names = [names, names, names]
     names = name_selector(names, LABEL='Select Animals for Processing', $
         CANCEL=cancelled, NUMCOLS=numcols)
     IF ~cancelled THEN Print, 'Names: ', names
END ;--------------------------------------------------------------------------------


FUNCTION Name_Selector, theNames, $
    ALL=all, $
    CANCEL=cancel, $
    COUNT=count, $
    LABEL=label, $
    NUMCOLS=numcols, $
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
   IF numNames GT 20 THEN BEGIN
       IF N_Elements(numcols) EQ 0 THEN numcols = (numNames/20)+1
       selectionBase = Widget_Base(tlb, COLUMN=numcols, /NONEXCLUSIVE, /FRAME, $
          Y_SCROLL_SIZE=( ((numNames/numcols) + 1) * (!D.Y_CH_SIZE + sizeFudge) )< 600, $
          X_SCROLL_SIZE=( ( (Max(StrLen(theNames))*!D.X_CH_SIZE ) + 50) * (numNames/((numNames/numcols) + 1)) ) < 1000, $
          EVENT_PRO='Name_Selector_Null_Events')
   ENDIF ELSE BEGIN
       IF N_Elements(numcols) EQ 0 THEN numcols = 1
       selectionBase = Widget_Base(tlb, COLUMN=numcols, /NONEXCLUSIVE, /FRAME, $
          Y_SCROLL_SIZE=(((numNames/numcols) + 1) * (!D.Y_CH_SIZE + sizeFudge)) < 600, $
          X_SCROLL_SIZE=( ( (Max(StrLen(theNames))*!D.X_CH_SIZE ) + 50) * (numNames/((numNames/numcols) + 1)) ) < 1000, $
          EVENT_PRO='Name_Selector_Button_Events')
   ENDELSE
   listbuttons = LonArr(numNames)
   FOR j=0, numNames-1 DO BEGIN
      listbuttons[j] = Widget_Button(selectionBase, Value=theNames[j], $
         UVALUE=STRUPCASE(theNames[j]))
   ENDFOR   
   buttonBase = Widget_Base(tlb, ROW=1, XPAD=0, YPAD=0, EVENT_PRO='Name_Selector_Button_Events')
   button = Widget_Button(buttonBase, Value='Dismiss', UVALUE='DISMISS')
   button = Widget_Button(buttonBase, Value='Deselect All', UVALUE='DESELECT_ALL')
   button = Widget_Button(buttonBase, Value='Select All', UVALUE='SELECT_ALL')
   button = Widget_Button(buttonBase, Value='Accept', UVALUE='ACCEPT')
   
   ; Set all the buttons to selected?
   IF all THEN BEGIN
      FOR j=0,numNames-1 DO Widget_Control, listbuttons[j], SET_BUTTON=1
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
   sbgeo = Widget_Info(selectionBase, /GEOMETRY)
   IF sbgeo.scr_xsize LT xmin THEN BEGIN
      Widget_Control, selectionBase, SCR_XSIZE=xmin
   ENDIF
   Widget_Control, tlb, /Realize
   
   ; Set up pointers for passing info around the program and for storing results.
   selectedNamePtr = Ptr_New({cancel:1})
   info = Ptr_New({listbuttons:listbuttons, selectedNamePtr:selectedNamePtr, $
        theNames:theNames, yoffset:yoffset, xmin:xmin, selectionBase:selectionBase}, /NO_COPY)
   Widget_Control, tlb, SET_UVALUE=info
   
   ; This *should* block the IDL command line, but if you called this from
   ; a blocking widget program, you could run though this block with disasterous
   ; results. This is a *common* problem with Virtual Machine applications, which
   ; by definition block the IDL command line. Maybe you should have used a GROUP_LEADER.
   XManager, 'name_selector', tlb, EVENT_HANDLER='Name_Selector_Size_Events'
   
   ; Return here after block is released. Set the CANCEL flag, get the names, free the pointers.
   cancel = (*selectedNamePtr).cancel
   IF cancel EQ 0 THEN selectedNames = (*selectedNamePtr).theNames ELSE selectedNames = ""
   Ptr_Free, selectedNamePtr, info
   
   ; Count them.
   count = N_Elements(selectedNames)
   
   ; Return the selected names.
   IF N_Elements(selectedNames) EQ 1 THEN RETURN, selectedNames[0] ELSE RETURN, selectedNames

END ; ----------------------------------------------------------------------------------