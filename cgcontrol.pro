; docformat = 'rst'
;
; NAME:
;   cgControl
;
; PURPOSE:
;   Allows the user to set various properties of an cgWindow object. This is essentially
;   a wrapper to the cgWindow SetProperty method.
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
;   Allows the user to set various properties of an cgWindow object. This is essentially
;   a wrapper to the cgWindow SetProperty method.
;
; :Categories:
;    Graphics
;    
; :Params:
;    selection: in, required, type=varies
;       Normally, a window index number of an cgWindow application. But, the selection
;       can be a widget identifier, an object reference, or a window title, depending on
;       which keywords are set. The cgWindow matching the selection has its properties set.
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
;     delay: in, optional, type=float
;         Set this keyword to the amount of "delay" you want between commands in the command list.
;     deletecmd: in, optional, type=boolean
;          Set this keyword to delete a command in the cgWindow. The keywords cmdIndex and All
;          are used in deleting the specified command.
;     destroy: in, optional, type=boolean
;          Set this keyword to destroy the cgWindow program. This keyword should not be used
;          with other keywords.
;     eraseit: in, optional, type=boolean
;         If this property is set, the cgWindow erases with the background color before
;         displaying the commands in the window's command list.
;     execute: in, optional, type=boolean
;         Set this keyword to exectute the commands in the window's command list. Use UPDATE if you
;         want commands executed after changing their properties. This command should not be used
;         with other keywwords. It's primary purpose is to execute commands after then have been
;         silently loaded into the window.
;     im_transparent: in, optional, type=boolean, default=0
;         Set this keyword to allow ImageMagick to create transparent backgrounds when it
;         makes raster image files from PostScript output.
;     im_density: in, optional, type=integer, default=300
;         Set this keyword to the sampling density when ImageMagick creates raster image
;         file from PostScript outout.
;     im_options: in, optional, type=string, default=""
;         Set this keyword to any ImageMagick options you would like to pass along to the
;         ImageMagick convert command when creating raster image files from PostScript output.
;     im_resize: in, optional, type=integer, default=25
;         Set this keyword to percentage that the raster image file created my ImageMagick
;         from PostScript output should be resized.
;     multi: in, optional, type=Intarr(5)
;         Set this keyword to the !P.MULTI setting you want to use for the window.
;         !P.MULTI is set to this setting before command execution, and set back to
;         it's default value when the commands are finished executing.
;     object: in, optional, type=boolean
;         If this keyword is set, the selection is assumed to be an object reference.
;     palette: in, optional, type=BytArr(N,3)
;         Use this keyword to pass in an N-by-3 (or 3-by-N) byte array containing the
;         R, G, and B vectors of a color table. It is probably easier to use CTLOAD or
;         XCOLORS to load color tables for the window, but this is provided as another option.
;     ps_delete: in, optional, type=boolean, default=1
;         Set this keyword to zero if you want to keep the PostScript output ImageMagick creates
;         when making raster file output.
;     ps_encapsulated: in, optional, type=boolean, default=0
;          Set this keyword to configure PSCONFIG to produce encapsulated PostScript output by default.
;     ps_metric: in, optional, type=boolean, default=0
;          Set this keyword to configure PSCONFIG to use metric values and A4 page size in its interface.
;     title: in, optional, type=boolean
;         If this keyword is set, the selection is assumed to be a window title. All
;         matching is done in uppercase characters.
;     update: in, optional, type=boolean, default=1
;         Set this keyword to zero if you do not want the updates to be done immediately
;         after the properties are changed.
;     widgetid: in, optional, type=boolean
;         If this keyword is set, the selection is assumed to be a widget identifier.
;     xomargin: in, optional, type=intarr(2)
;         Sets the !X.OMargin system variable when multiple plots are displayed in the window.
;     yomargin: in, optional, type=intarr(2)
;         Sets the !Y.OMargin system variable when multiple plots are displayed in the window.
;          
; :Examples:
;    Used to set cgWindow properties::
;       IDL> cgControl, Background='gray', EraseIt=1
;       IDL> cgControl, Multi=[0,2,2]
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
PRO cgControl, selection, $
    ALL=all, $                                    ; Apply the command operation to all the commands (i.e., DeleteCMD)
    BACKGROUND=background, $                      ; Sets the background color of the window
    CMDINDEX=cmdIndex, $                          ; Apply the command operation to this command only.
    PALETTE=palette, $                            ; The color palette (color vectors) associated with this window.
    DELAY=delay, $                                ; Set the delay between command execution.
    DELETECMD=deleteCmd, $                        ; Delete a command. If ALL is set, delete all commands.
    DESTROY=destroy, $                            ; Destroy the window. Should be called alone or with the ALL keyword.
    ERASEIT=eraseit, $                            ; Set the erase feature of the window.
    EXECUTE=execute, $                            ; Execute the commands immediately. Should be called alone. Use UPDATE otherwise.
    LISTCMD=listCmd, $                            ; List a command or ALL commands.
    MULTI=multi, $                                ; Set the multiple property of the window. Identical to !P.Multi.
    OBJECT=object, $                              ; If this keyword is set, the selection is an object reference.
    TITLE=title, $                                ; If this keyword is set, the selection is the title.
    UPDATE=update, $                              ; If this keyword is set, the commands are immediately executed after properties are set.
    WIDGETID=widgetID, $                          ; If this keyword is set, the selection is a widget ID.
    XOMARGIN=xomargin, $                          ; Change the !X.OMargin setting for the winow.
    YOMARGIN=yomargin, $                          ; Change the !Y.OMargin setting for the window.
    IM_TRANSPARENT=im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
    IM_DENSITY=im_density, $                      ; Sets the density parameter on ImageMagick convert command.
    IM_RESIZE=im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
    IM_OPTIONS=im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
    PS_DELETE=ps_delete, $                        ; Delete the PostScript file when making IM files.
    PS_METRIC=ps_metric, $                        ; Select metric measurements in PostScript output.
    PS_ENCAPSULATED=ps_encapsulated, $            ; Create Encapsulated PostScript output.
    PS_FONT=ps_font, $                            ; Select the font for PostScript output.
    PS_CHARSIZE=ps_charsize, $                    ; Select the character size for PostScript output.
    PS_SCALE_FACTOR=ps_scale_factor, $            ; Select the scale factor for PostScript output.
    PS_TT_FONT=ps_tt_font                         ; Select the true-type font to use for PostScript output.
    
   Compile_Opt idl2
    
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
   ENDIF
   
   ; Always update, unless told otherwise.
   IF N_Elements(update) EQ 0 THEN update = 1
   
   ; If you are updating, then turn execute off, otherwise you will
   ; draw the graphics twice.
   IF Keyword_Set(update) THEN execute=0
   
   ; If there is no selection match, use the current window. If there
   ; is no current window, create one.
   IF N_Elements(selection) EQ 0 THEN BEGIN
        selection = cgQuery(/CURRENT, COUNT=count)
        IF count EQ 0 THEN BEGIN
            cgWindow
            selection = cgQuery(/CURRENT, COUNT=count)
        ENDIF
   ENDIF
   
   ; Try to do the right thing here.
   IF Size(selection, /TNAME) EQ 'OBJREF' THEN object = 1
   IF Size(selection, /TNAME) EQ 'STRING' THEN title = 1
   
   ; Get the values you need.
   wid = cgQuery(WIDGETID=tlb, OBJECT=objref, TITLE=titles, COUNT=count)
   IF count EQ 0 THEN Message, 'There are no cgWindows currently on the display.', /Infomational
   
   ; Get the window list.
   list = !FSC_Window_List
   
   ; Decide what to do based on the type of match.
   CASE 1 OF
   
        Keyword_Set(widgetID): BEGIN
            index = Where(tlb EQ selection, selectCount)
            IF selectCount EQ 0 THEN $
                Message, 'No cgWindow matches the selection criteria.', /Infomational
            END
            
        Keyword_Set(object): BEGIN
            index = Where(objref EQ selection, selectCount)
            IF selectCount EQ 0 THEN $
                Message, 'No cgWindow matches the selection criteria.', /Infomational
            END
            
        Keyword_Set(title): BEGIN
            index = Where(StrUpCase(titles) EQ StrUpCase(selection), selectCount)
            IF selectCount EQ 0 THEN $
                Message, 'No cgWindow matches the selection criteria.', /Infomational
            END

        ELSE: BEGIN
            index = Where(wid EQ selection, selectCount)
            IF selectCount EQ 0 THEN $
                Message, 'No cgWindow matches the selection criteria.', /Infomational
            END
   
   ENDCASE
   
   ; Make sure the index is a scalar.
   index = index[0]
   
   ; Are you deleting commands?
   IF N_Elements(deleteCmd) NE 0 THEN BEGIN
        IF Obj_Valid(objref[index]) THEN objref[index] -> DeleteCommand, cmdIndex, ALL=Keyword_Set(all)
   ENDIF

   ; Are you listing the commands?
   IF N_Elements(listCmd) NE 0 THEN BEGIN
        IF Obj_Valid(objref[index]) THEN objref[index] -> ListCommand, cmdIndex
   ENDIF
   
   ; Are you destroying the window?
   IF Keyword_Set(destroy) THEN BEGIN
       IF Keyword_Set(all) THEN BEGIN
            FOR j=0,count-1 DO IF Widget_Info(wid[j], /Valid_ID) THEN Widget_Control, wid[j], /Destroy
       ENDIF ELSE BEGIN
            IF Widget_Info(wid[index], /Valid_ID) THEN Widget_Control, wid[index], /Destroy
       ENDELSE
       RETURN
   ENDIF
   
   ; Set the properties of the window.
   IF Obj_Valid(objref[index]) THEN objref[index] -> SetProperty, $
        BACKGROUND=background, $
        DELAY=delay, $
        ERASEIT=eraseit, $
        PALETTE=palette, $
        MULTI=multi, $
        UPDATE=update, $
        XOMARGIN=xomargin, $                            ; Change the !X.OMargin setting for the winow.
        YOMARGIN=yomargin, $                            ; Change the !Y.OMargin setting for the window.
        IM_TRANSPARENT = im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
        IM_DENSITY = im_density, $                      ; Sets the density parameter on ImageMagick convert command.
        IM_RESIZE = im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
        IM_OPTIONS = im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
        PS_DELETE = ps_delete, $                        ; Delete the PostScript file when making IM files.
        PS_METRIC = ps_metric, $                        ; Select metric measurements in PostScript output.
        PS_ENCAPSULATED = ps_encapsulated, $            ; Create encapsulated PostScript output.
        PS_FONT=ps_font, $                              ; Select the font for PostScript output.
        PS_CHARSIZE=ps_charsize, $                      ; Select the character size for PostScript output.
        PS_SCALE_FACTOR=ps_scale_factor, $              ; Select the scale factor for PostScript output.
        PS_TT_FONT=ps_tt_font                           ; Select the true-type font to use for PostScript output.
        
   ; Are you executing commands immediately?
   IF Keyword_Set(execute) THEN BEGIN
        IF Obj_Valid(objref[index]) THEN objref[index] -> ExecuteCommands
   ENDIF
   
     
    
END 