; docformat = 'rst'
;
; NAME:
;   cgWindow
;
; PURPOSE:
;   Creates a resizeable graphics window for IDL traditional commands (Plot, Contour, 
;   Surface, etc. or for Coyote Graphics routines, cgPlot, cgContour, cgSurf, etc.). 
;   In addition, the window contents can be saved as PostScript files or as raster image 
;   files. If ImageMagick is installed on your machine, the raster image files can be 
;   created in very high quality from PostScript files.
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
;+
; :Description:
;     Adds a command object of class IDL_WINDOW_COMMAND to the command list 
;     maintained by the window.
;
; :Params:
;     command: in, required, type=object
;         A command object of class IDL_WINDOW_COMMAND.
;-
PRO FSC_CmdWindow::AddCommand, command,  INDEX=index

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    ; If the command is a valid object, add it to the command list.
    IF Obj_Valid(command) THEN self.cmds -> Add, command, index, /Before

END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;     Provides a programmatic way to create a PostScript file from the window.
;     Call by setting the CREATE_PS keyword with cgControl.
;
; :Params:
;     filename:  The name of the PostScript file.
;-
PRO FSC_CmdWindow::AutoPostScriptFile, filename

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Make this window the current graphics windows.
    currentWindow = !D.Window
    WSet, self.wid

    IF N_Elements(filename) EQ 0 THEN filename='cgwindow.ps'

    ; Allow the user to configure the PostScript file.
    PS_Start, GUI=0, $
        FILENAME=filename, $
        EUROPEAN=self.ps_metric, $
        ENCAPSULATED=self.ps_encapsulated, $
        SCALE_FACTOR=self.ps_scale_factor, $
        CHARSIZE=self.ps_charsize, $
        FONT=self.ps_font, $
        QUIET=self.ps_quiet, $
        TT_FONT=self.ps_tt_font
    
    ; Execute the graphics commands.
    self -> ExecuteCommands
    
    ; Clean up.
    PS_End

    ; Set the window index number back.
    IF WindowAvailable(curentWindow) THEN WSet, currentWindow ELSE WSet, -1

END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;     Provides a programmatic way to create a raster file from the window.
;     Call by setting the create_png, etc. keyword with cgControl.
;
; :Params:
;     filetype:  The type of raster file (e.g., PNG, JPEG, etc.)
;     filename:  The name of the output file.
;-
PRO FSC_CmdWindow::AutoRasterFile, filetype, filename

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Make this window the current graphics windows.
    currentWindow = !D.Window
    WSet, self.wid

    IF N_Elements(filetype) EQ 0 then filetype = 'PNG'
    IF N_Elements(filename) EQ 0 THEN filename = 'cgwindow.' + StrLowCase(filetype)

    ; Strip the extension off the filename.
    outname = FSC_Base_Filename(filename, DIRECTORY=dirName)
    
    ; Put it back together without an extension.
    outputFilename = Filepath(ROOT_DIR=dirName, outname)
    
    ; What kind of raster file. The type will be set with cgControl, IM_RASTER=type.
    CASE self.im_raster OF
      ; Normal raster.
      0: BEGIN
         void = cgSnapshot(TYPE=filetype, FILENAME=outputFilename, /NODIALOG)
         END

      ; Raster via ImageMagick
      1: BEGIN

           ; Create a PostScript file first.
           PS_Start, $
                FILENAME=outputFilename + '.ps', $
                EUROPEAN=self.ps_metric, $
                SCALE_FACTOR=self.ps_scale_factor, $
                CHARSIZE=self.ps_charsize, $
                FONT=self.ps_font, $
                QUIET=self.ps_quiet, $
                TT_FONT=self.ps_tt_font
           
           ; Draw the graphics.
           self -> ExecuteCommands
           
           ; Close the file and convert to proper file type.
           CASE filetype OF
                'BMP':  PS_END, /BMP, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options
                'GIF':  PS_END, /GIF, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options
                'JPEG': PS_END, /JPEG, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options
                'PNG':  PS_END, /PNG,  DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options
                'TIFF': PS_END, /TIFF, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options
           ENDCASE
         END

    ENDCASE

    ; Set the window index number back.
    IF WindowAvailable(curentWindow) THEN WSet, currentWindow ELSE WSet, -1
END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;     Sends the window commands to a PostScript file.
;
; :Params:
;     event: in, required, type=structure
;         An event structure.
;-
PRO FSC_CmdWindow::CreatePostScriptFile, event

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Make this window the current graphics windows.
    currentWindow = !D.Window
    WSet, self.wid

    ; Allow the user to configure the PostScript file.
    PS_Start, /GUI, $
        CANCEL=cancelled, $
        EUROPEAN=self.ps_metric, $
        ENCAPSULATED=self.ps_encapsulated, $
        SCALE_FACTOR=self.ps_scale_factor, $
        CHARSIZE=self.ps_charsize, $
        FONT=self.ps_font, $
        QUIET=self.ps_quiet, $
        TT_FONT=self.ps_tt_font
    IF cancelled THEN RETURN
    
    ; Execute the graphics commands.
    self -> ExecuteCommands
    
    ; Clean up.
    PS_End
    
    ; Set the window index number back.
    IF WindowAvailable(curentWindow) THEN WSet, currentWindow ELSE WSet, -1

END ;----------------------------------------------------------------------------------------------------------------

;+
; :Description:
;     Deletes a command object from the command list maintained by the window.
;
; :Params:
;     cmdIndex: in, optional, type=integer
;         The zero-based index of the command to delete. If undefined, the
;         index of the last command in the window is used.
;
; :Keywords:
;     all: in, optional, type=boolean
;         If set, all the commands in the command list are deleted.
;-
PRO FSC_CmdWindow::DeleteCommand, cmdIndex, ALL=all

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    ; Get the command count.
    count = self.cmds -> Get_Count()
    
    ; Delete all the commands? Is so, delete and RETURN.
    IF Keyword_Set(all) THEN BEGIN
        self.cmds -> Delete_Nodes, /DESTROY
        self -> ExecuteCommands
        RETURN
    ENDIF
    
    ; Need a command index?
    IF N_Elements(cmdIndex) EQ 0 THEN BEGIN
        cmdIndex = count - 1
    ENDIF 
    
    ; Do we have a command with this command number?
    IF cmdIndex GT (count-1) THEN Message, 'A command with index ' + StrTrim(cmdIndex,2) + ' does not exist.'
    
    IF cmdIndex GE 0 THEN BEGIN
        self.cmds -> Delete, cmdIndex, /Destroy
        self -> ExecuteCommands
    ENDIF ELSE Message, 'A negative command index number is not allowed.'
    
END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;     All widget events come here and are dispatched to the proper object method.
;
; :Params:
;     event: in, required, type=structure
;         An event structure.
;-
PRO FSC_CmdWindow_Dispatch_Events, event

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    ; Get the window object.
    Widget_Control, event.top, GET_UVALUE=self
    
    ; Get the event type. If the type is an object, then
    ; this must be a resize event from the TLB.
    Widget_Control, event.id, GET_UVALUE=eventType
    IF Obj_Valid(eventType) THEN eventType = 'TLB_RESIZE'
    
    ; Dispatch the events to the proper method.
    CASE eventType OF   
        'TLB_RESIZE': self -> Resize, event
        'POSTSCRIPT': self -> CreatePostscriptFile, event
        'SAVECOMMANDS': self -> SaveCommands
        'RESTORECOMMANDS': self -> RestoreCommands
        'QUIT': Obj_Destroy, self
        ELSE: self -> SaveAsRaster, event
    ENDCASE
    
END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;     This method executes the commands on the command list.
;-
PRO FSC_CmdWindow::ExecuteCommands, KEYWORDS=keywords

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        !P.Multi = thisMulti
        !X.OMargin = thisXOmargin
        !Y.OMargin = thisYOmargin
        IF N_Elements(rr) NE 0 THEN TVLCT, rr, gg, bb
        IF (!D.Flags AND 256) NE 0 THEN WSet, -1
        RETURN
    ENDIF
    
    ; Make sure you are suppose to be executing these commands.
    IF Keyword_Set(self.noExecuteCommands) THEN RETURN
    
    ; Store the current !P.MULTI state.
    thisMulti = !P.Multi
    thisXOmargin = !X.OMargin
    thisYOmargin = !Y.OMargin

    ; Make this window the current graphics window.
    IF (!D.Flags AND 256) NE 0 THEN BEGIN
        currentWindow = !D.Window
        WSet, self.wid
    ENDIF
    
    ; Get the current color table vectors so they can be restored.
    TVLCT, rr, gg, bb, /GET
    
    ; Load the color vectors.
    TVLCT, *self.r, *self.g, *self.b
    
    ; Erase the window.
    IF self.eraseit THEN cgErase, *self.background 
    
    ; Are we doing multiple commands?
    IF Total(self.pmulti) NE 0 THEN !P.Multi = self.pmulti

    ; How many commands are there?
    n_cmds = self.cmds -> Get_Count()
    
    ; Issue an informative message if there are no commands to execute.
    ;IF n_cmds EQ 0 THEN Message, 'There are currently no graphics commands to execute.', /INFORMATIONAL
    
    ; Execute the commands.
    FOR j=0,n_cmds-1 DO BEGIN
        thisCmdObj = self.cmds -> Get_Item(j, /DEREFERENCE)
        
        ; Execute the command. This will update output keywords, possibly.
        thisCmdObj -> Draw, SUCCESS=success, KEYWORDS=keywords
        
        ; Update the keyword structure. I had hoped that by doing this would have
        ; updated output keyword values, but this doesn't work because Call_Procedure
        ; does not pass keywords by reference (using keyword inheritance) but, by value).
        ; Thus, it is currently NOT possible to obtain output keyword values from commands
        ; entered into cgWindow.
        IF success && (N_Elements(keywords) NE 0) THEN thisCmdObj -> UpdateKeywordStruct, keywords

        ; Did you successfully draw this command?
        IF ~success THEN BEGIN
        
            thisCmdObj -> List
            answer = Dialog_Message('Problem executing the command shown ' + $
                                    'in the console output. Delete command?', /QUESTION)
            IF StrUpCase(answer) EQ 'YES' THEN BEGIN
                self -> DeleteCommand, j
                !P.Multi = thisMulti
                !X.OMargin = thisXOmargin
                !Y.OMargin = thisYOmargin
                IF N_Elements(rr) NE 0 THEN TVLCT, rr, gg, bb
                IF (!D.Flags AND 256) NE 0 THEN WSet, -1
                RETURN
            ENDIF
        
        ENDIF
        
        ; Need to delay?
        IF self.delay NE 0 THEN Wait, self.delay
    ENDFOR
    
    ; If there are no commands, then just erase the window.
    IF n_cmds EQ 0 THEN cgErase, *self.background 
    
    ; Restore the colors in effect when we entered.
    TVLCT, rr, gg, bb
    
    ; Set the !P.Multi and outside margin system variables back to its original values.
    !P.Multi = thisMulti
    !X.OMargin = thisXOmargin
    !Y.OMargin = thisYOmargin
    
    ; Reset the current graphics window, if possible.
    IF (!D.Flags AND 256) NE 0 THEN BEGIN
       IF (currentWindow GE 0) && WindowAvailable(currentWindow) THEN BEGIN
            WSet, currentWindow
       ENDIF ELSE WSet, -1
    ENDIF

END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;     This method retrieves properties from the object.
;-
FUNCTION FSC_CmdWindow::GetCommandKeyword, keyword, cmdIndex, SUCCESS=success

    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        void = Error_Message()
        success = 0
        RETURN, success
    ENDIF
    
    ; Assume failure.
    success = 0

    IF N_Params() NE 2 THEN BEGIN
        Print, 'Useage: keywordValue = obj -> GetCommandKeyword(keywordName, commandIndex)'
        RETURN, ""
    ENDIF
    IF Size(keyword, /TNAME) NE 'STRING' THEN Message, 'Keyword name must be a string variable.'
    
    ; Make sure command index is in the range.
    cmdIndex = 0 > cmdIndex < ((self.cmds -> Get_Count()) - 1)
    
    ; Get the keywords for the proper command.
    cmdObject = self.cmds -> Get_Item(cmdIndex, /DEREFERENCE)
    keywordStruct = cmdObject -> Get_Keywords(HAS_KEYWORDS=has_keywords)
    
    ; Does this command have keywords?
    retValue = ""
    IF has_keywords THEN BEGIN
        tagNames = Tag_Names(keywordStruct)
        keyIndex = Where(tagNames EQ StrUpCase(keyword), nameCount)
        IF nameCount GT 0 THEN BEGIN
            retvalue = keywordStruct.(nameCount)
            success = 1
        ENDIF ELSE Message, 'This command has no keyword named: ' + keyword
    ENDIF ELSE Message, 'This command has no keywords.'

    RETURN, retValue
END ;----------------------------------------------------------------------------------------------------------------


;
;+
; :Description:
;     This method retrieves properties from the object.
;-
PRO FSC_CmdWindow::GetProperty, $
    BACKGROUND=background, $
    COMMANDS=commands, $
    DELAY=delay, $
    ERASEIT=eraseit, $
    NOEXECUTECOMMANDS=noExecuteCommands, $ ; Set if you want commands to execute commands.
    MULTI=multi, $
    PALETTE=palette, $
    TLB=tlb, $
    WID=wid, $
    XOMARGIN=xomargin, $
    YOMARGIN=yomargin, $
    IM_TRANSPARENT=im_transparent, $  ; Sets the "alpha" keyword on ImageMagick convert command.
    IM_DENSITY=im_density, $                      ; Sets the density parameter on ImageMagick convert command.
    IM_RESIZE=im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
    IM_OPTIONS=im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
    IM_RASTER=im_raster, $                        ; Sets whether to generate raster files via ImageMagick
    PS_DELETE=ps_delete, $
    PS_ENCAPSULATED=ps_encapsulated, $
    PS_METRIC=ps_metric, $
    PS_FONT=ps_font, $                            ; Select the font for PostScript output.
    PS_CHARSIZE=ps_charsize, $                    ; Select the character size for PostScript output.
    PS_SCALE_FACTOR=ps_scale_factor, $            ; Select the scale factor for PostScript output.
    PS_TT_FONT=ps_tt_font, $                       ; Select the true-type font to use for PostScript output.
    _EXTRA=extra
    
    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    ; Window properties.
    IF Arg_Present(background) THEN background = *self.background
    IF Arg_Present(palette) THEN BEGIN
        len = N_Elements(*self.r)
        palette = BytArr(len,3)
        palette[*,0] = *self.r
        palette[*,1] = *self.g
        palette[*,2] = *self.b
    ENDIF
    IF Arg_Present(commands) THEN commands = self.cmds
    IF Arg_Present(delay) THEN delay = self.delay
    IF Arg_Present(eraseit) THEN eraseit = self.eraseit
    IF Arg_Present(multi) THEN multi = self.pmulti
    IF Arg_Present(tlb) THEN tlb = self.tlb
    IF Arg_Present(wid) THEN wid = self.wid
    IF Arg_Present(xomargin) THEN xomargin = self.xomargin
    IF Arg_Present(yomargin) THEN yomargin = self.yomargin
    
     ; PostScript properties.
     ps_charsize = self.ps_charsize
     ps_delete = self.ps_delete
     ps_encapsulated = self.ps_encapsulated
     ps_metric = self.ps_metric
     ps_font = self.ps_font
     ps_scale_factor = self.ps_scale_factor
     ps_tt_font = self.ps_tt_font
     
     ; ImageMagick properties.
     im_transparent = self.im_transparent
     im_density = self.im_density
     im_options = self.im_options
     im_resize = self.im_resize
     im_raster = self.im_raster
     
     noExecuteCommands = self.noExecuteCommands
     
    ; You may have gotten keywords you don't know what to deal with. Inform
    ; the user there.
    IF N_Elements(extra) NE 0 THEN BEGIN
       tags = Tag_Names(extra)
       FOR j=0,N_Elements(tags)-1 DO BEGIN
          Message, 'The following property could NOT be obtained in the GetProperty method: ' + tags[j], /Informational
       ENDFOR
    ENDIF
END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;     This method invalidates a widget ID. It is used for restored
;     visualizations, so that the current window doesn't get inadvertenly
;     destroyed by a widget identifier from an old program.
;-
PRO FSC_CmdWindow::InvalidateWidgetID
    self.tlb = -1
END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;     This method lists the command indicated by the command index. In the
;     absence of the command index, all commands are listed.
;-
PRO FSC_CmdWindow::ListCommand, cmdIndex, CREATECOMMANDSTRUCT=createCommandStruct

    Compile_Opt idl2

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    ; Need a command structure?
    createCommandStruct = Keyword_Set(createCommandStruct)

    ; How many commands are there?
    count = self.cmds -> Get_Count()

    IF N_Elements(cmdIndex) EQ 0 THEN BEGIN
        FOR j = 0, count-1 DO BEGIN
            thisCmdObj = self.cmds -> Get_Item(j, /DEREFERENCE)

            ; Preface the commands with their index number.
            thisCmdObj -> List, StrTrim(j,2) + '.'

            ; Create the command struct
            IF createCommandStruct THEN thisCmdObj -> CreateCommandStruct, 'cmd' + StrTrim(j,2)
        ENDFOR
    ENDIF ELSE BEGIN
        IF cmdIndex LT (count-1) THEN BEGIN
            thisCmdObj = self.cmds -> Get_Item(cmdIndex, /DEREFERENCE)

            ; Preface the commands with their index number.
            thisCmdObj -> List, StrTrim(cmdIndex,2) + '.'

            ; Create the command struct
            IF createCommandStruct THEN thisCmdObj -> CreateCommandStruct, 'cmd' + StrTrim(cmdIndex,2)
        ENDIF ELSE Message, 'The command index is out of range of the number of commands.'
    ENDELSE

END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;     This method loads color table vectors into the program.
;     The XCOLORS_DATA keyword is required to get color vector
;     information from XCOLORS.
;-
PRO FSC_CmdWindow::LoadColors, r, g, b, XCOLORS_DATA=colorData

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    ; Load the vectors
    IF N_Elements(r) NE 0 THEN *self.r = r
    IF N_Elements(g) NE 0 THEN *self.g = g
    IF N_Elements(b) NE 0 THEN *self.b = b
    
    IF N_Elements(colorData) NE 0 THEN BEGIN
       *self.r = colorData.r
       *self.g = colorData.g
       *self.b = colorData.b
    ENDIF
    
    ; Execute the commands.
    self -> ExecuteCommands
    
END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;     This method replaces a command in the command list. If cmdImdex is missing,
;     replace all the commands in the command list.
;-
PRO FSC_CmdWindow::ReplaceCommand, command, cmdIndex, MULTI=multi

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    ; If cmdIndex is missing, remove all the current commands with this one.
    IF N_Elements(cmdIndex) EQ 0 THEN BEGIN
        self.cmds -> Delete, /ALL, /Destroy
        IF N_Elements(multi) NE 0 THEN BEGIN
            self.pmulti = multi ; Reset !P.Multi to new value.
        ENDIF ELSE BEGIN
            self.pmulti = Intarr(5) ; Reset !P.Multi to zero.
        ENDELSE
        self.cmds -> Add, command
    ENDIF ELSE BEGIN
    
        ; Get the old command first, so you can destroy it properly.
        oldcmd = self.cmds -> Get_Item(cmdIndex, /DEREFERENCE)
        self.cmds -> Replace_Item, cmdIndex, command
        Obj_Destroy, oldcmd
    ENDELSE
    
END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;     This method resizes the graphics window and executes the commands again.
;-
PRO FSC_CmdWindow::Resize, event

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    Widget_Control, self.drawID, DRAW_XSIZE=event.x, DRAW_YSIZE=event.y
    self -> ExecuteCommands
    
END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;     This method saves the graphics window as a raster image file.
;-
PRO FSC_CmdWindow::SaveAsRaster, event

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    ; On going in here for down event.
    IF event.select NE 1 THEN RETURN
    
    Widget_Control, event.ID, Get_UValue=buttonValue
    
    ; Determine if this is normal raster (o) or ImageMagick raster (1).
    IF StrMid(buttonValue, 0, 6) EQ 'RASTER' THEN BEGIN
        fileType = StrMid(buttonValue, 7)
        rasterType = 0 
    ENDIF ELSE BEGIN
        filetype = StrMid(buttonValue, 12)
        rasterType = 1
    ENDELSE
    
    ; Make this window the current graphics windows.
    currentWindow = !D.Window
    WSet, self.wid

    ; Get a filename from the user.
    CASE filetype OF
       'BMP':  filename = FSC_Pickfile(FILE='cgwindow.bmp', /WRITE, TITLE='Select an Output File...')
       'GIF':  filename = FSC_Pickfile(FILE='cgwindow.gif', /WRITE, TITLE='Select an Output File...')
       'JPEG': filename = FSC_Pickfile(FILE='cgwindow.jpg', /WRITE, TITLE='Select an Output File...')
       'PNG':  filename = FSC_Pickfile(FILE='cgwindow.png', /WRITE, TITLE='Select an Output File...')
       'TIFF': filename = FSC_Pickfile(FILE='cgwindow.tif', /WRITE, TITLE='Select an Output File...')
    ENDCASE
    IF filename EQ "" THEN RETURN
    root_name = FSC_Base_Filename(filename, DIRECTORY=dirName)
    outname = Filepath(ROOT_DIR=dirname, root_name)
    
    ; What kind of raster file.
    CASE rasterType OF
    
        ; Normal raster.
        0: BEGIN
           void = cgSnapshot(TYPE=fileType, FILENAME=outname, /NODIALOG)
           END
           
        ; Raster via ImageMagick.
        1: BEGIN
        
           ; Create a PostScript file first.
           thisname = outname + '.ps'
           PS_Start, $
                FILENAME=thisname, $
                EUROPEAN=self.ps_metric, $
                SCALE_FACTOR=self.ps_scale_factor, $
                CHARSIZE=self.ps_charsize, $
                FONT=self.ps_font, $
                QUIET=self.ps_quiet, $
                TT_FONT=self.ps_tt_font
           
           ; Draw the graphics.
           self -> ExecuteCommands
           
           ; Close the file and convert to proper file type.
           CASE filetype OF
                'BMP':  PS_END, /BMP, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options
                'GIF':  PS_END, /GIF, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options
                'JPEG': PS_END, /JPEG, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options
                'PNG':  PS_END, /PNG,  DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options
                'TIFF': PS_END, /TIFF, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options
           ENDCASE
        
           END
    
    ENDCASE
    
    ; Set the window index number back.
    IF WindowAvailable(curentWindow) THEN WSet, currentWindow ELSE WSet, -1
END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;     This method restores the commands.
;-
PRO FSC_CmdWindow::RestoreCommands, filename

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    ; Need a file name?
    IF N_Elements(filename) EQ 0 THEN BEGIN
        filename = FSC_Pickfile(Title='Restore Coyote Graphics Commands...', $
            FILTER='*.cgs')
        IF filename EQ "" THEN RETURN
    ENDIF
    
    ; Does the file exist?
    fileTest = File_Test(filename, /READ)
    IF ~fileTest THEN BEGIN
        text = 'The file (' + filename + ') cannot be located. Returning.'
        void = Dialog_Message(text)
        RETURN
    ENDIF
    
    ; Restore the file. The variables "cg_commands" and "cg_window" are restored.
    Restore, FILE=filename, /Relaxed_Structure_Assignment
    
    ; Make this object the current object.
    cgSet, self.wid
    
    ; Erase the current commands in the window. If this is not
    ; done memory will leak.
    cgErase, self.wid, /Window
    
    ; Copy the commands from the restored command list to this command list.
    oldCommands = cg_commands -> Get_Item(/DEREFERENCE, /ALL)
    self.cmds -> Delete, /ALL
    FOR j=0,N_Elements(oldCommands)-1 DO BEGIN
        thisCmd = oldCommands[j] -> Copy()
        self.cmds -> Add, thisCmd
    ENDFOR
        
    ; Copy properties from old window object to new.
    cg_window -> GetProperty, $
        BACKGROUND=background, $       ; The background color of the window.
        DELAY=delay, $                 ; The delay between command execution.
        ERASEIT=eraseit, $             ; Set the erase flag for the window
        PALETTE=palette, $             ; Change window color table vectors.
        MULTI=multi, $                 ; Change the !P.MULTI setting for the window.
        XOMARGIN=xomargin, $           ; Change the !X.OMargin setting for the winow.
        YOMARGIN=yomargin, $           ; Change the !Y.OMargin setting for the window.
        IM_TRANSPARENT=im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
        IM_DENSITY=im_density, $                      ; Sets the density parameter on ImageMagick convert command.
        IM_RESIZE=im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
        IM_OPTIONS=im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
        IM_RASTER=im_raster, $                        ; Sets whether to use ImageMagick to create raster files.
        PS_DELETE=ps_delete, $                        ; Delete the PostScript file when making IM raster files.
        PS_METRIC=ps_metric, $                        ; Select metric measurements in PostScript output.
        PS_ENCAPSULATED=ps_encapsulated, $            ; Select encapusulated PostScript output.
        PS_FONT=ps_font, $                            ; Select the font for PostScript output.
        PS_CHARSIZE=ps_charsize, $                    ; Select the character size for PostScript output.
        PS_SCALE_FACTOR=ps_scale_factor, $            ; Select the scale factor for PostScript output.
        PS_TT_FONT=ps_tt_font                         ; Select the true-type font to use for PostScript output.
        
    self -> SetProperty, $
        BACKGROUND=background, $       ; The background color of the window.
        DELAY=delay, $                 ; The delay between command execution.
        ERASEIT=eraseit, $             ; Set the erase flag for the window
        PALETTE=palette, $             ; Change window color table vectors.
        MULTI=multi, $                 ; Change the !P.MULTI setting for the window.
        XOMARGIN=xomargin, $           ; Change the !X.OMargin setting for the winow.
        YOMARGIN=yomargin, $           ; Change the !Y.OMargin setting for the window.
        IM_TRANSPARENT=im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
        IM_DENSITY=im_density, $                      ; Sets the density parameter on ImageMagick convert command.
        IM_RESIZE=im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
        IM_OPTIONS=im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
        IM_RASTER=im_raster, $                        ; Sets whether to use ImageMagick to create raster files.
        PS_DELETE=ps_delete, $                        ; Delete the PostScript file when making IM raster files.
        PS_METRIC=ps_metric, $                        ; Select metric measurements in PostScript output.
        PS_ENCAPSULATED=ps_encapsulated, $            ; Select encapusulated PostScript output.
        PS_FONT=ps_font, $                            ; Select the font for PostScript output.
        PS_CHARSIZE=ps_charsize, $                    ; Select the character size for PostScript output.
        PS_SCALE_FACTOR=ps_scale_factor, $            ; Select the scale factor for PostScript output.
        PS_TT_FONT=ps_tt_font                         ; Select the true-type font to use for PostScript output.
        
        
    Obj_Destroy, cg_window
    
    ; Execute the new commands.
    self -> ExecuteCommands

END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;     This method saves the commands.
;-
PRO FSC_CmdWindow::SaveCommands, filename

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Need a file name.
    IF N_Elements(filename) EQ 0 THEN BEGIN
       filename = FSC_Pickfile(FILE='commands.cgs', $
            TITLE='Save Coyote Graphics Commands...', /Write)
       IF filename EQ "" THEN RETURN
    ENDIF
    
    ; Copy the commands.
    cg_commands = self.cmds
    cg_window = self
    
    ; Save them in the file.
    Save, cg_commands, cg_window, FILE=filename
   
END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;     This method sets properties of the window object.
;-
PRO FSC_CmdWindow::SetProperty, $
    BACKGROUND=background, $       ; The background color of the window.
    DELAY=delay, $                 ; The delay between command execution.
    ERASEIT=eraseit, $             ; Set the erase flag for the window
    PALETTE=palette, $             ; Change window color table vectors.
    NOEXECUTECOMMANDS=noExecuteCommands, $ ; Set if you want commands to execute commands.
    MULTI=multi, $                 ; Change the !P.MULTI setting for the window.
    XOMARGIN=xomargin, $           ; Change the !X.OMargin setting for the winow.
    YOMARGIN=yomargin, $           ; Change the !Y.OMargin setting for the window.
    UPDATE=update, $               ; Set if you want the commands to be updated after property change.
    IM_TRANSPARENT=im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
    IM_DENSITY=im_density, $                      ; Sets the density parameter on ImageMagick convert command.
    IM_RESIZE=im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
    IM_OPTIONS=im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
    IM_RASTER=im_raster, $                        ; Sets whether to use ImageMagick to create raster files.
    PS_DELETE=ps_delete, $                        ; Delete the PostScript file when making IM raster files.
    PS_METRIC=ps_metric, $                        ; Select metric measurements in PostScript output.
    PS_ENCAPSULATED=ps_encapsulated, $            ; Select encapusulated PostScript output.
    PS_FONT=ps_font, $                            ; Select the font for PostScript output.
    PS_CHARSIZE=ps_charsize, $                    ; Select the character size for PostScript output.
    PS_QUIET=ps_quiet, $                          ; Select the QUIET keyword for PS_Start.
    PS_SCALE_FACTOR=ps_scale_factor, $            ; Select the scale factor for PostScript output.
    PS_TT_FONT=ps_tt_font, $                      ; Select the true-type font to use for PostScript output.
    _EXTRA=extra
    
    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    IF N_Elements(background) NE 0 THEN BEGIN
        IF Ptr_Valid(self.background) $
            THEN *self.background = background $
            ELSE self.background = Ptr_New(background)
    ENDIF 
    IF N_Elements(palette) NE 0 THEN BEGIN
        IF Size(palette, /N_DIMENSIONS) NE 2 THEN Message, 'Color palette is not a 3xN array.'
        dims = Size(palette, /DIMENSIONS)
        threeIndex = Where(dims EQ 3)
        IF ((threeIndex)[0] LT 0) THEN Message, 'Color palette is not a 3xN array.'
        IF threeIndex[0] EQ 0 THEN palette = Transpose(palette)
        *self.r = palette[*,0]
        *self.g = palette[*,1]
        *self.b = palette[*,2]
    ENDIF   
    IF N_Elements(delay) NE 0 THEN self.delay = delay
    IF N_Elements(eraseit) NE 0 THEN self.eraseit = Keyword_Set(eraseit)
    IF N_Elements(noExecuteCommands) NE 0 THEN self.noexecutecommands = Keyword_Set(noExecuteCommands)
    IF N_Elements(multi) NE 0 THEN BEGIN
        IF (N_Elements(multi) EQ 1) && (multi EQ 0) THEN multi = IntArr(5)
        FOR j=0,N_Elements(multi)-1 DO self.pmulti[j] = multi[j]
    ENDIF
    IF N_Elements(xomargin) NE 0 THEN self.xomargin = xomargin
    IF N_Elements(yomargin) NE 0 THEN self.yomargin = yomargin
    IF N_Elements(im_transparent) NE 0 THEN self.im_transparent = im_transparent
    IF N_Elements(im_density) NE 0 THEN self.im_density = im_density
    IF N_Elements(im_resize) NE 0 THEN self.im_resize = im_resize
    IF N_Elements(im_options) NE 0 THEN self.im_options = im_options
    IF N_Elements(im_raster) NE 0 then self.im_raster = im_raster
    IF N_Elements(ps_delete) NE 0 THEN self.ps_delete = ps_delete
    IF N_Elements(ps_metric) NE 0 THEN self.ps_metric = ps_metric
    IF N_Elements(ps_encapsulated) NE 0 THEN self.ps_encapsulated = ps_encapsulated
    IF N_Elements(ps_charsize) NE 0 THEN self.ps_charsize = ps_charsize
    IF N_Elements(ps_font) NE 0 THEN self.ps_font = ps_font
    IF N_Elements(ps_quiet) NE 0 THEN self.ps_quiet = ps_quiet
    IF N_Elements(ps_scale_factor) NE 0 THEN self.ps_scale_factor = ps_scale_factor
    IF N_Elements(ps_tt_font) NE 0 THEN self.ps_tt_font = ps_tt_font
    
    ; You may have gotten keywords you don't know what to deal with. Inform
    ; the user there.
    IF N_Elements(extra) NE 0 THEN BEGIN
       tags = Tag_Names(extra)
       FOR j=0,N_Elements(tags)-1 DO BEGIN
          Message, 'The following property could NOT be set in the SetProperty method: ' + tags[j], /Informational
       ENDFOR
    ENDIF
    
    ; Update now?
    IF Keyword_Set(update) THEN self -> ExecuteCommands
END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_CmdWindow_Cleanup, tlb
    Widget_Control, tlb, Get_UValue=self
    Obj_Destroy, self
END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;     This method initializes the object that is at the heart of cgWindow.
;     It takes most of the same arguments as cgWindow.
;-
FUNCTION FSC_CmdWindow::Init, $
   command, $                       ; The graphics "command" to execute.
   p1, p2, p3, $                    ; The three allowed positional parameters.
   _Extra = extra, $                ; Any extra keywords. Usually the "command" keywords.
   Group_Leader = group_leader, $   ; The group leader of the cgWindow program.
   AddCmd=addcmd, $                 ; Set this keyword to add a command to the interface.
   CmdDelay=cmdDelay, $             ; Set this keyword to a value to "wait" before executing the next command.
   Method=method, $                 ; If set, will use CALL_METHOD instead of CALL_PROCEDURE to execute command.
   ReplaceCmd=replacecmd, $         ; Replace the current command and execute in the current window.
   WEraseIt = Weraseit, $           ; Set this keyword to erase the display before executing the command.
   WMulti = wmulti, $               ; Set this in the same way !P.Multi is used.
   WOXMargin = woxmargin, $         ; Set the !X.OMargin. A two element array.
   WOYMargin = woymargin, $         ; Set the !Y.OMargin. A two element array
   WXSize = wxsize, $               ; The X size of the cgWindow graphics window in pixels. By default: 400.
   WYSize = wysize, $               ; The Y size of the cgWindow graphics window in pixels. By default: 400.
   WTitle = wtitle, $               ; The window title.
   WXPos = wxpos, $                 ; The X offset of the window on the display. The window is centered if not set.
   WYPos = wypos, $                 ; The Y offset of the window on the display. The window is centered if not set.
   WBackground = wbackground        ; The background color. Set to !P.Background by default.

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN, 0
    ENDIF
   
    ; Check keywords.
    method = Keyword_Set(method)
    
    ; Get the global defaults.
    cgWindow_GetDefs, $
       Background = d_background, $                      ; The background color. 
       Delay = d_delay, $                                ; The amount of delay between command execution.
       EraseIt = d_eraseit, $                            ; Set this keyword to erase the display before executing the commands.
       Multi = d_multi, $                                ; Set this in the same way !P.Multi is used.   
       XOMargin = d_xomargin, $                          ; Set the !X.OMargin. A two element array.
       YOMargin = d_yomargin, $                          ; Set the !Y.OMargin. A two element array
       XSize = d_xsize, $                                ; The X size of the cgWindow graphics window.
       YSize = d_ysize, $                                ; The Y size of the cgWindow graphics window.
       Title = d_title, $                                ; The window title.
       XPos = d_xpos, $                                  ; The X offset of the window on the display.
       YPos = d_ypos, $                                  ; The Y offset of the window on the display. 
       Palette = d_palette, $                            ; The color table palette to use for the window.
       
       ; ImageMagick Properties.
       IM_Transparent = d_im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
       IM_Density = d_im_density, $                      ; Sets the density parameter on ImageMagick convert command.
       IM_Raster = d_im_raster, $                        ; Create raster files via ImageMagick.
       IM_Resize = d_im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
       IM_Options = d_im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
       
       ; PostScript properties.
       PS_Delete = d_ps_delete, $                        ; Delete PS file when making IM raster.
       PS_Metric = d_ps_metric, $                        ; Select metric measurements in PostScript output.
       PS_Encapsulated = d_ps_encapsulated, $            ; Create Encapsulated PostScript output.    
       PS_FONT = d_ps_font, $                            ; Select the font for PostScript output.
       PS_CHARSIZE = d_ps_charsize, $                    ; Select the character size for PostScript output.
       PS_QUIET = d_ps_quiet, $                          ; Select the QUIET keyword for PS_Start.
       PS_SCALE_FACTOR = d_ps_scale_factor, $            ; Select the scale factor for PostScript output.
       PS_TT_FONT = d_ps_tt_font                         ; Select the true-type font to use for PostScript output.
        
    ; If method is set, the first positional parameter must be present,
    ; and it must be a valid object reference.
    IF method THEN BEGIN
        IF N_Elements(p1) EQ 0 THEN $
            Message, 'The first positional parameter must be present to make a method call.'
        IF ~Obj_Valid(p1) THEN $
            Message, 'The first positional parameter must be a valid object reference when making method calls.'
    ENDIF
    IF N_Elements(wxsize) EQ 0 THEN xsize = d_xsize ELSE xsize = wxsize
    IF N_Elements(wysize) EQ 0 THEN ysize = d_ysize ELSE ysize = wysize
    IF N_Elements(wxpos) EQ 0 THEN xpos = d_xpos ELSE xpos = wxpos
    IF N_Elements(wypos) EQ 0 THEN ypos = d_ypos ELSE ypos = wypos
    IF N_Elements(wbackground) EQ 0 THEN BEGIN
        wbackground = d_background
        IF N_Elements(command) EQ 0 THEN weraseit = 1
        IF (N_Elements(command) NE 0) && CoyoteGraphic(command) THEN weraseit = 1
    ENDIF ELSE BEGIN
        weraseit = 1
    ENDELSE
    IF N_Elements(weraseIt) EQ 0 THEN weraseIt = d_eraseit ELSE weraseIt = Keyword_Set(weraseIt)
    
    ; The commands will be placed in a linked list for execution.
    self.cmds = Obj_New('LinkedList')
    IF Obj_Valid(self.cmds) EQ 0 THEN Message, 'Failed to make the LinkedList for the commands.'
    
    ; Add a command, if you have one. Otherwise, just make the window.
    IF (N_Elements(command) NE 0) THEN BEGIN
        thisCommand = Obj_New('FSC_Window_Command', COMMAND=command, $
                P1=p1, P2=p2, P3=p3, KEYWORDS=extra, TYPE=method)
        IF Obj_Valid(thisCommand) THEN self.cmds -> Add, thisCommand ELSE Message, 'Failed to make command object.'
    ENDIF 
    
    ; If there is a palette, use it. Otherwise, use the current color table vectors.
    IF Total(d_palette) NE 0 THEN BEGIN
        IF Size(d_palette, /N_DIMENSIONS) NE 2 THEN Message, 'Color palette is not a 3xN array.'
        dims = Size(d_palette, /DIMENSIONS)
        threeIndex = Where(dims EQ 3)
        IF ((threeIndex)[0] LT 0) THEN Message, 'Color palette is not a 3xN array.'
        IF threeIndex[0] EQ 0 THEN d_palette = Transpose(d_palette)
        self.r = Ptr_New(d_palette[*,0])
        self.g = Ptr_New(d_palette[*,1])
        self.b = Ptr_New(d_palette[*,2])
    ENDIF ELSE BEGIN
        TVLCT, rr, gg, bb, /Get
        self.r = Ptr_New(rr)
        self.g = Ptr_New(gg)
        self.b = Ptr_New(bb)
    ENDELSE
    
    ; Create the widgets for the program.
    self.tlb = Widget_Base(/TLB_SIZE_EVENTS, MBar=menuID)
    
    fileID = Widget_Button(menuID, Value='File')
    saveID = Widget_Button(fileID, Value='Save Window As...', /MENU)
    button = Widget_Button(saveID, Value='PostScript File', UVALUE='POSTSCRIPT')
    raster = Widget_Button(saveID, Value='Raster Image File', /MENU)
    
    button = Widget_Button(raster, Value='BMP', UVALUE='RASTER_BMP')
    button = Widget_Button(raster, Value='GIF', UVALUE='RASTER_GIF')
    button = Widget_Button(raster, Value='JPEG', UVALUE='RASTER_JPEG')
    button = Widget_Button(raster, Value='PNG', UVALUE='RASTER_PNG')
    button = Widget_Button(raster, Value='TIFF', UVALUE='RASTER_TIFF')
    
    ; If you can find ImageMagick on this machine, you can convert to better
    ; looking raster files.
    IF HasImageMagick() EQ 1 THEN BEGIN
        imraster = Widget_Button(saveID, Value='Raster Image File via ImageMagick', /MENU)
        button = Widget_Button(imraster, Value='BMP', UVALUE='IMAGEMAGICK_BMP')
        button = Widget_Button(imraster, Value='GIF', UVALUE='IMAGEMAGICK_GIF')
        button = Widget_Button(imraster, Value='JPEG', UVALUE='IMAGEMAGICK_JPEG')
        button = Widget_Button(imraster, Value='PNG', UVALUE='IMAGEMAGICK_PNG')
        button = Widget_Button(imraster, Value='TIFF', UVALUE='IMAGEMAGICK_TIFF')
    ENDIF
    
    button = Widget_Button(fileID, Value='Save Current Visualization', /Separator, UVALUE='SAVECOMMANDS')
    button = Widget_Button(fileID, Value='Restore Visualization', UVALUE='RESTORECOMMANDS')
    button = Widget_Button(fileID, Value='Quit', /Separator, UVALUE='QUIT')
    
    ; Create draw widget. UNIX versions of IDL have a bug in which creating
    ; a draw widget as the very first window in an IDL session causes both
    ; !P.Background and !P.Color to be set to white. I know, it's odd. But
    ; doing this little trick fixes the problem.
    tempBackground = !P.Background
    tempColor = !P.Color
    retain = (StrUpCase(!Version.OS_Family) EQ 'UNIX') ? 2 : 1
    self.drawID = Widget_Draw(self.tlb, XSIZE=xsize, YSize=ysize, RETAIN=retain) 
    !P.Background = Temporary(tempBackground)
    !P.Color = Temporary(tempColor)
    
    ; Do we need to center the widget?
    IF (xpos EQ -1) AND (ypos EQ -1) THEN BEGIN
        DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
        IF ~exists THEN BEGIN
           xpos = 5
           ypos = 5
        ENDIF ELSE BEGIN
            IF Obj_Valid(!FSC_WINDOW_LIST) THEN BEGIN
                count = !FSC_WINDOW_LIST -> Get_Count()
                xpos = 5 + (30*count)
                ypos = 5 + (25*count)
            ENDIF ELSE BEGIN
                xpos = 5
                ypos = 5           
            ENDELSE
        ENDELSE
        CenterTLB, self.tlb, xpos, ypos, /NOCENTER, /DEVICE
    ENDIF ELSE BEGIN
        CenterTLB, self.tlb, xpos, ypos, /NOCENTER, /DEVICE
    ENDELSE
    
    ; Display the widget and get window index number.
    currentWindow = !D.Window
    Widget_Control, self.tlb, /Realize
    Widget_Control, self.drawID, Get_Value=wid
    self.wid = wid
    
    IF N_Elements(wtitle) EQ 0 THEN BEGIN
        wtitle = d_title
        wtitle = wtitle + ' (' + StrTrim(wid,2) + ')'
    ENDIF
    Widget_Control, self.tlb, TLB_Set_Title=wtitle

    ; Load object properties.
    self.background = Ptr_New(wbackground)
    IF N_Elements(cmdDelay) NE 0 THEN self.delay = cmdDelay ELSE self.delay = d_delay
    self.eraseIt = weraseIt
    IF N_Elements(wmulti) NE 0 THEN BEGIN
       FOR j=0,N_Elements(wmulti)-1 DO self.pmulti[j] = wmulti[j]
    ENDIF ELSE self.pmulti = d_multi
    IF N_Elements(wxomargin) NE 0 THEN self.xomargin = wxomargin ELSE self.xomargin = d_xomargin
    IF N_Elements(wyomargin) NE 0 THEN self.yomargin = wyomargin ELSE self.yomargin = d_yomargin
    self.im_transparent = d_im_transparent
    self.im_density = d_im_density
    self.im_options = d_im_options
    self.im_raster = d_im_raster
    self.im_resize = d_im_resize
    self.ps_delete = d_ps_delete
    self.ps_encapsulated = d_ps_encapsulated
    self.ps_metric = d_ps_metric
    self.ps_charsize = d_ps_charsize
    self.ps_font = d_ps_font
    self.ps_quiet = d_ps_quiet
    self.ps_scale_factor = d_ps_scale_factor
    self.ps_tt_font = d_ps_tt_font

    ; Execute the commands.
    self -> ExecuteCommands 
    
    ; Get it running.
    WIDGET_CONTROL, /MANAGED, self.tlb
    XManager, 'cgwindow', self.tlb, /No_Block, $
        Event_Handler='FSC_CmdWindow_Dispatch_Events', $
        Cleanup = 'FSC_CmdWindow_Cleanup', $
        Group_Leader=group_leader
    
    ; Store the self reference in the UVALUE of the TLB.
    Widget_Control, self.tlb, SET_UValue=self
    
    ; Each instance of cgWindow will store evidence of its
    ; existance in a linked list.
    DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
    IF ~exists THEN BEGIN
        fsc_window_list = Obj_New("LinkedList")
        DefSysV, '!FSC_WINDOW_LIST', fsc_window_list 
        fsc_window_list -> Add, {FSC_WINDOW_ID, self.tlb, wid, wtitle, self}
    ENDIF ELSE BEGIN
        IF Obj_Valid(!FSC_WINDOW_LIST) THEN BEGIN
            !FSC_WINDOW_LIST -> Add, {FSC_WINDOW_ID, self.tlb, wid, wtitle, self}
        ENDIF ELSE BEGIN
            !FSC_WINDOW_LIST = Obj_New('LinkedList')
            !FSC_WINDOW_LIST-> Add, {FSC_WINDOW_ID, self.tlb, wid, wtitle, self}
        ENDELSE
    ENDELSE
    
    ; Restore the current graphics window, if you can.
    IF (currentWindow GE 0) && WindowAvailable(currentWindow) THEN BEGIN
       WSet, currentWindow
    ENDIF ELSE WSet, -1
    RETURN, 1

END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_CmdWindow::Cleanup

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    ; Free any pointers.
    Ptr_Free, self.background
    Ptr_Free, self.r
    Ptr_Free, self.g
    Ptr_Free, self.b
    
    ; Destroy the command objects.
    IF Obj_Valid(self.cmds) THEN BEGIN
        count = self.cmds -> Get_Count()
        FOR j=0,count-1 DO Obj_Destroy, self.cmds -> Get_Item(j, /DEREFERENCE)
        Obj_Destroy, self.cmds
    ENDIF
    
    ; You have to remove yourself from the list of valid cgWindows.
    theList = !FSC_WINDOW_LIST
    IF Obj_Valid(theList) THEN BEGIN
        structs = theList -> Get_Item(/ALL, /DEREFERENCE)
        index = Where(structs.windowObj[*] EQ self, count)
        IF count GT 0 THEN theList -> Delete, index[0]
    ENDIF 
    
    ; If the list doesn't have any more cgWindows objects in it,
    ; delete the list so it doesn't waste memory. 
    IF (theList -> Get_Count() EQ 0) THEN Obj_Destroy, theList
    
    ; If your widget ID is valid, destroy the widget program.
    IF Widget_Info(self.tlb, /VALID_ID) THEN Widget_Control, self.tlb, /Destroy
    
END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_CmdWindow__Define, class

    class = { FSC_CMDWINDOW, $
              tlb: 0L, $                    ; The identifier of the top-level base widget.
              cmds: Obj_New(), $            ; A linkedlist object containing the graphics commands.
              wid: 0L, $                    ; The window index number of the graphics window.
              drawid: 0L, $                 ; The identifier of the draw widget.
              
              ; cgWindow parameters
              background: Ptr_New(), $      ; The background color.
              delay: 0.0, $                 ; The command delay.
              eraseit: 0B, $                ; Do we need to erase the display.
              noExecuteCommands: 0B, $      ; Set to stop command execution (e.g.,for loading commands)
              pmulti: LonArr(5), $          ; Identical to !P.Multi.
              xomargin: FltArr(2), $        ; Identical to !X.OMargin
              yomargin: FltArr(2), $        ; Identical to !Y.OMargin
              r: Ptr_New(), $               ; The red color table vector.
              g: Ptr_New(), $               ; The green color table vector.
              b: Ptr_New(), $               ; The blue color table vector.
              
              ; PostScript options.
              ps_delete: 0L, $              ; Delete the PS file when making IM image file.
              ps_encapsulated: 0L, $        ; Encapsulated PostScript
              ps_metric: 0L, $              ; Metric measurements in PostScript.
              ps_charsize: 0.0, $           ; The character size to use for PostScript output.
              ps_font: 0, $                 ; The PostScript font to use.
              ps_quiet: 0, $                ; Select the QUIET keyword for PS_Start.
              ps_scale_factor: 0, $         ; The PostScript scale factor.
              ps_tt_font: "", $             ; The name of a true-type font to use for PostScript output.

              ; ImageMagick output parameters.
              im_transparent: 0B, $         ; Sets the "alpha" keyword on ImageMagick convert command.
              im_density: 0L, $             ; Sets the density parameter on ImageMagick convert command.
              im_resize: 0L, $              ; Sets the resize parameter on ImageMagick convert command.
              im_options: "", $             ; Sets extra ImageMagick options on the ImageMagick convert command.
              im_raster: 0L $               ; Create raster files via ImageMagick
            }
            
END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_Window_Command::CreateCommandStruct, structName, Quiet=quiet

    Compile_Opt idl2

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    ; Struct variable name
    IF N_Elements(structName) EQ 0 THEN structName='cmd'

    cmdString = self.command
    cmdStruct = Create_Struct('Command', cmdString, 'nparams', self.nparams, 'type', self.type)
    CASE self.nparams OF
        0:
        1: cmdStruct = Create_Struct(cmdStruct, 'p1', *self.p1)
        2: cmdStruct = Create_Struct(cmdStruct, 'p1', *self.p1, 'p2', *self.p2)
        3: cmdStruct = Create_Struct(cmdStruct, 'p1', *self.p1, 'p2', *self.p2, 'p3', *self.p3)
    ENDCASE
    IF Ptr_Valid(self.keywords) THEN BEGIN
        cmdStruct = Create_Struct(cmdStruct, 'keywords', *self.keywords)
    ENDIF

    ; Copy the variable to the MAIN level
    (Scope_VarFetch(structName, /Enter, Level=1)) = Temporary(cmdStruct)
    IF NOT Keyword_Set(quiet) THEN $
        PRINT, 'Created command struct variable ', structName, ' in IDL $MAIN level.'

END ;----------------------------------------------------------------------------------------------------------------


FUNCTION FSC_Window_Command::Copy

    IF Ptr_Valid(self.keywords) THEN BEGIN
    
        CASE self.nparams OF
           0: copyObj = Obj_New('FSC_Window_Command', COMMAND=self.command, KEYWORDS=*self.keywords, TYPE=self.type)
           1: copyObj = Obj_New('FSC_Window_Command', COMMAND=self.command, P1=*self.p1, KEYWORDS=*self.keywords, TYPE=self.type)
           2: copyObj = Obj_New('FSC_Window_Command', COMMAND=self.command, P1=*self.p1, P2=*self.p2, KEYWORDS=*self.keywords, TYPE=self.type)
           3: copyObj = Obj_New('FSC_Window_Command', COMMAND=self.command, P1=*self.p1, P2=*self.p2, P3=*self.p3, KEYWORDS=*self.keywords, TYPE=self.type)
        ENDCASE
        
    ENDIF ELSE BEGIN
    
        CASE self.nparams OF
           0: copyObj = Obj_New('FSC_Window_Command', COMMAND=self.command, TYPE=self.type)
           1: copyObj = Obj_New('FSC_Window_Command', COMMAND=self.command, P1=*self.p1, TYPE=self.type)
           2: copyObj = Obj_New('FSC_Window_Command', COMMAND=self.command, P1=*self.p1, P2=*self.p2, TYPE=self.type)
           3: copyObj = Obj_New('FSC_Window_Command', COMMAND=self.command, P1=*self.p1, p2=*self.p2, P3=*self.p3, TYPE=self.type)
        ENDCASE
    
    ENDELSE
    
    RETURN, copyObj
    
END ;----------------------------------------------------------------------------------------------------------------


FUNCTION FSC_Window_Command::Get_Keywords, HAS_KEYWORDS=has_keywords
    IF Ptr_Valid(self.keywords) THEN BEGIN
        has_keywords = 1
        RETURN, *self.keywords 
    ENDIF ELSE BEGIN
        has_keywords = 0
        RETURN, Ptr_New()
    ENDELSE
END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_Window_Command::Draw, SUCCESS=success, KEYWORDS=keywords

    Compile_Opt idl2
    
    ; Can't really catch CALL_PROCEDURE errors, so I'm not sure what this is doing here.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        ;void = Error_Message()
        success = 0
        RETURN
    ENDIF
    
    ; It seems it is not possible to catch errors generated by CALL_PROCEDURE,
    ; so I have to fake it out. I reset the !ERROR_STATE structure at the beginning
    ; and assume failure. After I get through the code, I check to see if the
    ; !ERROR_STATE.MSG field is still a null string. If it is, I assume we successfully
    ; executed the command and change the success flag.
    success = 0
    Message, /RESET

    ; What kind of command is this?
    CASE self.type OF 
    
        ; Command calls a procedure.
        0: BEGIN
        
             IF Ptr_Valid(self.keywords) THEN BEGIN
                 CASE self.nparams OF
                     0: Call_Procedure, self.command, _Extra=*self.keywords
                     1: Call_Procedure, self.command, *self.p1, _Extra=*self.keywords
                     2: Call_Procedure, self.command, *self.p1, *self.p2, _Extra=*self.keywords
                     3: Call_Procedure, self.command, *self.p1, *self.p2, *self.p3, _Extra=*self.keywords
                 ENDCASE
                 keywords = *self.keywords
             ENDIF ELSE BEGIN
                 CASE self.nparams OF
                     0: Call_Procedure, self.command
                     1: Call_Procedure, self.command, *self.p1
                     2: Call_Procedure, self.command, *self.p1, *self.p2
                     3: Call_Procedure, self.command, *self.p1, *self.p2, *self.p3
                 ENDCASE
             ENDELSE
             
             END
             
        ; Command calls a method.
        1: BEGIN

             IF Ptr_Valid(self.keywords) THEN BEGIN
                 CASE self.nparams OF
                     0: Message, 'Call_Method requires at least one positional parameter.'
                     1: Call_Method, self.command, *self.p1, _Extra=*self.keywords
                     2: Call_Method, self.command, *self.p1, *self.p2, _Extra=*self.keywords
                     3: Call_Method, self.command, *self.p1, *self.p2, *self.p3, _Extra=*self.keywords
                 ENDCASE
                 keywords = *self.keywords
             ENDIF ELSE BEGIN
                 CASE self.nparams OF
                     0: Message, 'Call_Method requires at least one positional parameter.'
                     1: Call_Method, self.command, *self.p1
                     2: Call_Method, self.command, *self.p1, *self.p2
                     3: Call_Method, self.command, *self.p1, *self.p2, *self.p3
                 ENDCASE
             ENDELSE
             
           END
    
    ENDCASE
    
    ; If nothing has been put into the message field, we much have executed the command
    ; successfully.
    IF !Error_State.MSG EQ "" THEN success = 1 

    ; For some reason, CALL_PROCEDURE does not flush the graphics buffer on UNIX machines.
    ; We have to do it ourself to get the program to resize correctly on UNIX machines.
    EMPTY
    
END ;----------------------------------------------------------------------------------------------------------------



PRO FSC_Window_Command::List, prefix

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    cmdString = self.command
    CASE self.nparams OF
        0:
        1: cmdString = cmdString + ', p1'
        2: cmdString = cmdString + ', p1, p2'
        3: cmdString = cmdString + ', p1, p2, p3'
    ENDCASE
    IF Ptr_Valid(self.keywords) THEN BEGIN
        tags = Tag_Names(*self.keywords)
        FOR j=0,N_Elements(tags)-1 DO BEGIN
            cmdString = cmdString + ', ' + tags[j] + '=value'
        ENDFOR
    ENDIF
    
    IF N_Elements(prefix) NE 0 THEN prefix = '   ' + prefix + ' ' ELSE prefix = '   '
    
    Print, prefix + cmdString
END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_Window_Command::UpdateKeywordStruct, keywords
    
    Compile_Opt idl2

    On_Error, 2
    
    ; If the keyword pointer is invalid, out of here.
    IF ~Ptr_Valid(self.keywords) THEN RETURN
    
    ; Update the structure.
    IF N_Elements(keywords) NE 0 THEN *self.keywords = keywords
    
END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_Window_Command::Cleanup
    Ptr_Free, self.p1
    Ptr_Free, self.p2
    Ptr_Free, self.p3
    Ptr_Free, self.keywords
END ;----------------------------------------------------------------------------------------------------------------


FUNCTION FSC_Window_Command::INIT, $
    COMMAND=command, $
    P1=p1, P2=p2, P3=p3, $
    KEYWORDS=keywords, $
    TYPE=type
    
    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN, 0
    ENDIF

    self.command = command
    IF N_Elements(p1) NE 0 THEN self.p1 = Ptr_New(p1)
    IF N_Elements(p2) NE 0 THEN self.p2 = Ptr_New(p2)
    IF N_Elements(p3) NE 0 THEN self.p3 = Ptr_New(p3)
    IF N_Elements(keywords) NE 0 THEN self.keywords = Ptr_New(keywords)
    self.type = type
    self.nparams = (N_Elements(p1) NE 0) + (N_Elements(p2) NE 0) + (N_Elements(p3) NE 0)
    RETURN, 1
    
END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_Window_Command__Define

   ; The definition of the command object.
   class = { FSC_Window_Command, $
              command: "", $         ; The command to execute.
              p1: Ptr_New(), $       ; The first parameter.
              p2: Ptr_New(), $       ; The second parameter.
              p3: Ptr_New(), $       ; The third parameter.
              nparams: 0, $          ; The number of parameters.
              keywords: Ptr_New(), $ ; The command keywords.
              type: 0 $              ; =0 call_procedure =1 call_method
            }
END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_Window_ID__Define

   struct = { FSC_WINDOW_ID, $
                 tlb: 0L, $
                 wid: 0L, $
                 title: "", $
                 windowObj: Obj_New() $
             }
END ;----------------------------------------------------------------------------------------------------------------


;+
; :Description:
;   Creates a resizeable graphics window for IDL traditional commands (Plot, Contour, 
;   Surface, etc. or for Coyote Graphics routines, cgPlot, cgContour, cgSurf, etc.). 
;   In addition, the window contents can be saved as PostScript files or as raster image 
;   files. If ImageMagick is installed on your machine, the raster image files can be 
;   created in very high quality from PostScript files.
;
; :Categories:
;    Graphics
;    
; :Params:
;    command: in, required, type=string
;       The graphics procedure command to be executed. This parameter
;       must be a string and the the command must be a procedure. Examples
;       are 'Surface', 'Contour', 'Plot', 'cgPlot', cgContour, etc.
;    p1: in, optional, type=any
;       The first positional parameter appropriate for the graphics command.
;    p2: in, optional, type=any
;       The second positional parameter appropriate for the graphics command.
;    p3: in, optional, type=any
;       The third positional parameter appropriate for the graphics command.
;       
; :Keywords:
;    addcmd: in, optional, type=boolean, default=0
;       Set this keyword to add an additional graphics command to an cgWindow.
;       The command is added to the last created cgWindow, unless the WinID
;       keyword is used to select another cgWindow. Adding a command causes
;       all the commands in the window to be immediately executed. If this is
;       not behavior you desire, use the LOADCMD keyword instead. If CMDINDEX
;       is used to select a command index, the new command is added before
;       the command currently occuping that index in the command list.
;    cmddelay: in, optional, type=float
;       If this keyword is set to a value other than zero, there will be a 
;       delay of this many seconds between command execution. This will permit
;       "movies" of command sequences to be displayed.
;    cmdindex: in, optional, type=integer
;       This keyword is used to select which command in an cgWindow to act on
;       when the AllCmd, DeleteCmd, LoadCmd and ReplaceCmd keywords are used. 
;       See the descriptions of these keywords for details on what happens when 
;       CmdIndex is missing.
;    deletecmd: in, optional, type=boolean, default=0
;       Set this keyword to delete a graphics command from an cgWindow.
;       If CmdIndex is undefined the last command entered into the window is
;       deleted. It is not possible to delete the last command in the window.
;       Use WinID to identify the cgWindow you are interested in. If WinID 
;       is undefined, the last cgWindow created is used.
;    executecmd: in, optional, type=boolean, default=0
;       Set this keyword to immediate execute all the commands in an cgWindow.
;       Normally, this is used after commands have been loaded with LOADCMD.
;    listcmd: in, optional, type=boolean, default=0
;       If this keyword is set, the commands currently in the cgWindow are
;       listed. Use WinID to identify the cgWindow you are interested in.
;       If WinID is undefined, the last cgWindow created is used.
;    loadcmd: in, optional, type=boolean, default=0
;       Set this keyword to add an additional graphics command to an cgWindow.
;       The command is added to the last created cgWindow, unless the WinID
;       keyword is used to select another cgWindow. Loaded commands are not
;       automatically executed. Set the EXECUTECMD keyword at the end of loading
;       to execute the loaded commands. If CMDINDEX is used to select a command 
;       index, the new command is loaded before the command currently occuping 
;       that index in the command list.
;    replacecmd: in, optional, type=boolean, default=0
;       Set this keyword to replace a graphics command from an cgWindow.
;       If CmdIndex is undefined, *all* commands in the window are replaced. Use 
;       WinID to identify the cgWindow you are interested in. If WinID is 
;       undefined, the last cgWindow created is used for the replacement.
;    group_leader: in, optional
;       The identifier of a widget to serve as a group leader for this program.
;       If the group leader is destroyed, this program is also destroyed. Used
;       when calling this program from another widget program.
;    method: in, optional, type=boolean, default=0
;       Set this keyword if the command is an object method call rather than a 
;       procedure call. If this keyword is set, the first positional parameter, p1,
;       must be present and must be a valid object reference.
;    wbackground: in, optional, type=varies, default=!P.Background
;       The background color of the window. Specifying a background color 
;       automatically sets the WErase keyword.
;    werase: in, optional, type=boolean, default=0
;       Set this keyword to cause the window to be erased before graphics commands 
;       are drawn. This may need to be set, for example, to display images.
;    winid: in, optional, type=integer
;       Use this keyword to select the window cgWindow identifier (the number between
;       the parentheses in the title bar of cgWindow). The AddCmd, ReplaceCmd, ListCmd,
;       and DeleteCmd keywords will all apply to the commands in the last cgWindow
;       created unless this keyword is used to select another cgWindow to apply the 
;       commands to.
;    wmulti: in, optional, type=intarr(5)
;        Set this keyword in exactly the same way you would set the !P.Multi keyword.
;        It will allow you to display multi-plots in the cgWindow graphics window.
;    wobject: out, optional, type=object
;       cgWindow creates a FSC_CmdWindow object. This object reference is returned
;       if this keyword is present.
;    wxpos: in, optional, type=integer, default=5
;       The x offset in device coordinates of the cgWindow from the upper-left corner of the display.
;    wypos: in, optional, type=integer, default=5
;       The y offset in device coordinates of the cgWindow from the upper-left corner of the display.
;    wxsize: in, optional, type=integer, default=640
;       The x size in device coordinates of the graphics window.
;    wysize: in, optional, type=integer, default=5
;       The y size in device coordinates of the the graphics window.
;    wtitle: in, opetional, type=string, default='Resizeable Graphics Window'
;       The title of the graphics window. A window index number is appended to the
;       title so multiple cgWindow programs can be selected.
;          
; :Examples:
;    Test code::
;       data = Loaddata(17)
;       cgWindow, 'cgPlot', data, COLOR='red'
;       cgWindow, 'cgPlot', data, PSYM=2, /Overplot, COLOR='dodger blue', /AddCmd
;       cgWIndow, 'cgPlot', Loaddata(17), color='olive', linestyle = 2, /Overplot, /AddCmd
;       cgWindow, /ListCmd
;       cgWindow, 'cgPlot', data, COLOR='purple', /ReplaceCMD, CMDINDEX=0
;       
;       Additional examples can be found here:
;       
;          http://www.idlcoyote.com/graphics_tips/cgwindow.html
;           
; :Notes:
;    Notes on using the program::
;    
;       The program is designed to work with any IDL traditional graphics routine
;       that is a procedure and includes no more than three positional parameters.
;       Any number of keywords can be used to specify properties of the graphical
;       output. Any number of graphics commands can be "added" the the cgWindow.
;       Simply use the ADDCMD keyword to add commands.
;       
;       If your program does not load its own color tables, the color tables in
;       effect when cgWindow is first called are used to display the graphics
;       commands.
;    
;       To create PostScript output from within cgWindow, your graphics program
;       has to be written in such a way that it can work properly in the PostScript
;       device. This means there are no Window commands, WSet commands, and the like
;       that are not allowed in the PostScript device. Such commands are allowed in 
;       programs, of course, if they are "protected". Usually such protection looks 
;       like this:
;       
;          IF (!D.Flags AND 256) NE 0 THEN Window, ...
;          
;        cgDisplay is a good program for opening graphics "windows", because such
;        PostScript protection is built into the program. In a PostScript device,
;        cgDisplay produces a "window" with the same aspect ratio as the current
;        dislay graphics window, which is an aid in producing PostScript output that
;        looks like the same output in the display window.
;        
;        Much better looking raster files can be created from the cgWindow contents,
;        if the raster files are created by converting PostScript files to the raster 
;        file. If the ImageMagick "convert" command can be found on your machine, you
;        will have the option to create raster files using this method. I *highly*
;        recommend doing so, as fonts and other plot annotation will be of much higher
;        quality using this method.
;        
;        cgWindow has been designed to work with other Coyote Graphics routines: cgPlot,
;        cgContour, cgSurf, and so on, although I expect it to work with any IDL
;        traditional graphics routine, if the routine is well written.
;        
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: davidf@idlcoyote.com
;           Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 17 January 2011. DWF.
;        Fixed a problem with the example code, and added EMPTY to end of Draw method
;           to force UNIX machines to empty the graphics buffer after CALL_PROCEDURE. 20 Jan 2011. DWF.
;        Improved documentation and error handling. 19 Jan 2011. DWF.
;        More improved error handling and messages. 26 Jan 2011. DWF.
;        Made changes to accommodate the new cgControl routine. 27 Jan 2011. DWF.
;        Added WXOMARGIN and WYOMARGIN keywords. 28 Jan 2011. DWF.
;        Numerous changes leading up to official release. 4 Feb 2011. DWF.
;        Added workaround for UNIX bug for draw widget creation. 5 Feb 2011. DWF.
;        Corrected a window aspect ratio problem with PostScript output by making the
;           window the current window before calling PS_Start. 17 Feb 2011. DWF.
;        Added machinery for programmatically generating raster files. 18 Feb 2011. Jeremy Bailin.
;        Problem with restoring visualizations fixed. 6 March 2011. DWF.
;        Fixed a problem with CALL_METHOD, which requires one positional parameter. 8 March 2011. DWF.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
PRO cgWindow, $
   command, $                       ; The graphics "command" to execute.
   p1, p2, p3, $                    ; The three allowed positional parameters.
   _Extra = extra, $                ; Any extra keywords. Usually the "command" keywords.
   Group_Leader = group_leader, $   ; The group leader of the cgWindow program.
   Method=method, $                 ; If set, will use CALL_METHOD instead of CALL_PROCEDURE to execute command.
   WBackground = wbackground, $     ; The background color. Set to !P.Background by default.
   WErase = weraseit, $             ; Set this keyword to erase the display before executing the command.
   WMulti = wmulti, $               ; Set this in the same way !P.Multi is used.   
   WOXMargin = woxmargin, $         ; Set the !X.OMargin. A two element array.
   WOYMargin = woymargin, $         ; Set the !Y.OMargin. A two element array
   WXSize = wxsize, $               ; The X size of the cgWindow graphics window in pixels. By default: 640.
   WYSize = wysize, $               ; The Y size of the cgWindow graphics window in pixels. By default: 512.
   WTitle = wtitle, $               ; The window title.
   WXPos = wxpos, $                 ; The X offset of the window on the display. The window is tiled if not set.
   WYPos = wypos, $                 ; The Y offset of the window on the display. The window is tiled if not set.
   
   AddCmd=addcmd, $                 ; Set this keyword to add a command to the interface and immediate execute commands.
   CmdDelay=cmdDelay, $             ; Set this keyword to a value to "wait" before executing the next command.
   CmdIndex=cmdIndex, $             ; Set this keyword to identify the index of the command to manipulate.
   DeleteCmd=deletecmd, $           ; Set the keyword to delete a command.
   ExecuteCmd=executecmd, $         ; Set this keyword to execute the commands in the window.
   ListCmd=listCmd, $               ; Set this keyword to list the commands in the window.
   LoadCmd=loadCmd, $               ; Set this keyword to load commands in the window, but not execute them.
   ReplaceCmd=replacecmd, $         ; Set this keyword to replace a command in the window.
   WinID=winid, $                   ; Set this keyword to select an cgWindow.
   WObject=wobject                  ; The FSC_CMDWindow object. A return value.

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message(Traceback=0)
        RETURN
    ENDIF
    
    ; Did the user want to execute the commands in the window?
    IF N_Elements(executeCmd) NE 0 THEN BEGIN
   
      ; Does the self object exist somewhere?
      DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
      IF exists THEN BEGIN
           theList = !FSC_WINDOW_LIST
           IF Obj_Valid(theList) THEN BEGIN
                structs = theList -> Get_Item(/ALL, /DEREFERENCE)
                IF Size(structs, /TNAME) EQ 'POINTER' THEN RETURN
                IF N_Elements(winID) EQ 0 THEN BEGIN
                    winID = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN winID = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgWindow with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[winID]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    thisWindowStruct.windowObj -> ExecuteCommands
                ENDIF ELSE BEGIN
                    Message, 'The cgWindow referred to does not exist.'
                ENDELSE
                RETURN
           ENDIF ELSE Message, 'A valid cgWindow does not exist to execute commands in.'
       ENDIF ELSE Message, 'An cgWindow object not exist to execute commands in.'
    ENDIF
    
    ; Did the user want to list the commands in a cgWindow?
    IF N_Elements(listCmd) NE 0 THEN BEGIN
   
      ; Does the self object exist somewhere?
      DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
      IF exists THEN BEGIN
           theList = !FSC_WINDOW_LIST
           IF Obj_Valid(theList) THEN BEGIN
                structs = theList -> Get_Item(/ALL, /DEREFERENCE)
                IF Size(structs, /TNAME) EQ 'POINTER' THEN RETURN
                IF N_Elements(winID) EQ 0 THEN BEGIN
                    winID = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN winID = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgWindow with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[winID]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    thisWindowStruct.windowObj -> ListCommand
                ENDIF ELSE BEGIN
                    Message, 'The cgWindow referred to does not exist.'
                ENDELSE
                RETURN
           ENDIF ELSE Message, 'A valid cgWindow does not exist to list commands from.'
       ENDIF ELSE Message, 'An cgWindow object not exist to list commands from.'
    ENDIF
    
    ; Did the user want to delete a command in the window?
    IF N_Elements(deleteCmd) NE 0 THEN BEGIN
   
      ; Does the self object exist somewhere?
      DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
      IF exists THEN BEGIN
           theList = !FSC_WINDOW_LIST
           IF Obj_Valid(theList) THEN BEGIN
                structs = theList -> Get_Item(/ALL, /DEREFERENCE)
                IF Size(structs, /TNAME) EQ 'POINTER' THEN RETURN
                IF N_Elements(winID) EQ 0 THEN BEGIN
                    winID = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN winID = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgWindow with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[winID]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    ; If the cmdIndex is undefined, the last entered command is deleted.
                    thisWindowStruct.windowObj -> DeleteCommand, cmdIndex
                ENDIF ELSE BEGIN
                    Message, 'The cgWindow referred to does not exist.'
                ENDELSE
                RETURN
           ENDIF ELSE Message, 'The cgWindow object is not a valid window object.'
       ENDIF ELSE Message, 'An cgWindow object not exist to add a command to.'
   ENDIF

   ; Did the user want to replace a command or commands in the window?
   IF Keyword_Set(replaceCmd) THEN BEGIN
      
      ; Must have a command to replace current command with.
      IF N_Elements(command) EQ 0 THEN Message, 'No replacement command has been specified.'

      ; Does the self object exist somewhere?
      DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
      IF exists THEN BEGIN
           theList = !FSC_WINDOW_LIST
           IF Obj_Valid(theList) THEN BEGIN
                structs = theList -> Get_Item(/ALL, /DEREFERENCE)
                IF Size(structs, /TNAME) EQ 'POINTER' THEN RETURN
                IF N_Elements(winID) EQ 0 THEN BEGIN
                    winID = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN winID = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgWindow with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[winID]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    
                    ; Check command components.
                    IF Size(command, /TNAME) NE 'STRING' THEN $
                        Message, 'The first positional argument must be a command string.'
                    IF N_Params() GT 4 THEN $
                        Message, 'The maximum number of positional command parameters allowed is three.'
                    newCommand = Obj_New('FSC_Window_Command', COMMAND=command, $
                        P1=p1, P2=p2, P3=p3, KEYWORDS=extra, TYPE=Keyword_Set(method))
                        
                    ; If the cmdIndex is undefined, ALL current commands in the window are replaced.
                    thisWindowStruct.windowObj -> ReplaceCommand, newCommand, cmdIndex, MULTI=wmulti
                    thisWindowStruct.windowObj -> ExecuteCommands
                ENDIF ELSE BEGIN
                    Message, 'The cgWindow referred to does not exist.'
                ENDELSE
                RETURN
           ENDIF ELSE Message, 'A valid cgWindow does not exist to replace commands with.'
       ENDIF ELSE Message, 'An cgWindow does not exist to replace commands with.'
    
    ENDIF 
   
   ; Did the user want to load commands without executing them?
   IF Keyword_Set(loadCmd) THEN BEGIN

      ; Does the self object exist somewhere?
      DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
      IF exists THEN BEGIN
           theList = !FSC_WINDOW_LIST
           IF Obj_Valid(theList) THEN BEGIN
                structs = theList -> Get_Item(/ALL, /DEREFERENCE)
                IF Size(structs, /TNAME) EQ 'POINTER' THEN RETURN
                IF N_Elements(winID) EQ 0 THEN BEGIN
                    winID = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN winID = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgWindow with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[winID]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    newCommand = Obj_New('FSC_Window_Command', COMMAND=command, $
                        P1=p1, P2=p2, P3=p3, KEYWORDS=extra, TYPE=Keyword_Set(method))
                    thisWindowStruct.windowObj -> AddCommand, newCommand, INDEX=cmdIndex
                ENDIF ELSE BEGIN
                    Message, 'The cgWindow referred to does not exist.'
                ENDELSE
                RETURN
           ENDIF ELSE Message, 'A valid cgWindow does not exist to add a command to.'
       ENDIF ELSE Message, 'An cgWindow does not exist to add a command to.'
    
    ENDIF 
   
   ; Did the user want to add a command to the window?
   IF Keyword_Set(addCmd) THEN BEGIN

      ; Does the self object exist somewhere?
      DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
      IF exists THEN BEGIN
           theList = !FSC_WINDOW_LIST
           IF Obj_Valid(theList) THEN BEGIN
                structs = theList -> Get_Item(/ALL, /DEREFERENCE)
                IF Size(structs, /TNAME) EQ 'POINTER' THEN RETURN
                IF N_Elements(winID) EQ 0 THEN BEGIN
                    winID = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN winID = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgWindow with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[winID]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    newCommand = Obj_New('FSC_Window_Command', COMMAND=command, $
                        P1=p1, P2=p2, P3=p3, KEYWORDS=extra, TYPE=Keyword_Set(method))
                    thisWindowStruct.windowObj -> AddCommand, newCommand, INDEX=cmdIndex
                    thisWindowStruct.windowObj -> ExecuteCommands
                ENDIF ELSE BEGIN
                    Message, 'The cgWindow referred to does not exist.'
                ENDELSE
                RETURN
           ENDIF ELSE Message, 'A valid cgWindow does not exist to add a command to.'
       ENDIF ELSE Message, 'An cgWindow does not exist to add a command to.'
    
    ENDIF 
   
   ; Otherwise, make the command object.
   wobject = Obj_New('FSC_CmdWindow', $
       command, $                       ; The graphics "command" to execute.
       p1, p2, p3, $                    ; The three allowed positional parameters.
       _Extra = extra, $                ; Any extra keywords. Usually the "command" keywords.
       CmdDelay = cmdDelay, $           ; The amount of time to "wait" between commands.
       Group_Leader = group_leader, $   ; The group leader of the cgWindow program.
       Method=method, $                 ; If set, will use CALL_METHOD instead of CALL_PROCEDURE to execute command.
       WBackground = wbackground, $     ; The background color. Not used unless set.
       WMulti = wmulti, $               ; Set this in the same way !P.Multi is used.
       WErase = weraseit, $             ; Set this keyword to erase the display before executing the command.
       WXSize = wxsize, $               ; The X size of the cgWindow graphics window in pixels. By default: 400.
       WYSize = wysize, $               ; The Y size of the cgWindow graphics window in pixels. By default: 400.
       WTitle = wtitle, $               ; The window title.
       WXPos = wxpos, $                 ; The X offset of the window on the display. The window is centered if not set.
       WYPos = wypos )                  ; The Y offset of the window on the display. The window is centered if not set.
       
END
