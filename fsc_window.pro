; docformat = 'rst'
;
; NAME:
;   FSC_Window
;
; PURPOSE:
;   Creates a resizeable graphics window for IDL traditional commands (Plot, Contour, 
;   Surface, etc. or for Coyote Graphics routines, FSC_Plot, FSC_Contour, FSC_Surf, etc.). 
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
;
;+
; :Description:
;   Creates a resizeable graphics window for IDL traditional commands (Plot, Contour, 
;   Surface, etc. or for Coyote Graphics routines, FSC_Plot, FSC_Contour, FSC_Surf, etc.). 
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
;       are 'Surface', 'Contour', 'Plot', 'FSC_Plot', FSC_Contour, etc.
;    p1: in, optional, type=any
;       The first positional parameter appropriate for the graphics command.
;    p2: in, optional, type=any
;       The second positional parameter appropriate for the graphics command.
;    p3: in, optional, type=any
;       The third positional parameter appropriate for the graphics command.
;       
; :Keywords:
;    addcmd: in, optional, type=boolean, default=0
;       Set this keyword to add an additional graphics command to an FSC_Window.
;       The command is added to the last created FSC_Window, unless the WinID
;       keyword is used to select another FSC_Window.
;    cmdindex: in, optional, type=integer
;       This keyword is used to select which command in an FSC_Window to act on
;       when the DeleteCmd or ReplaceCmd keywords are used. See the descriptions
;       of these keywords for details on what happens when CmdIndex is missing.
;    deletecmd: in, optional, type=boolean, default=0
;       Set this keyword to delete a graphics command from an FSC_Window.
;       If CmdIndex is undefined the last command entered into the window is
;       deleted. It is not possible to delete the last command in the window.
;       Use WinID to identify the FSC_Window you are interested in. If WinID 
;       is undefined, the last FSC_Window created is used.
;    listcmd: in, optional, type=boolean, default=0
;       If this keyword is set, the commands currently in the FSC_Window are
;       listed. Use WinID to identify the FSC_Window you are interested in.
;       If WinID is undefined, the last FSC_Window created is used.
;    replacecmd: in, optional, type=boolean, default=0
;       Set this keyword to replace a graphics command from an FSC_Window.
;       If CmdIndex is undefined, *all* commands in the window are replaced. Use 
;       WinID to identify the FSC_Window you are interested in. If WinID is 
;       undefined, the last FSC_Window created is used for the replacement.
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
;       Use this keyword to select the window FSC_Window identifier (the number between
;       the parentheses in the title bar of FSC_Window). The AddCmd, ReplaceCmd, ListCmd,
;       and DeleteCmd keywords will all apply to the commands in the last FSC_Window
;       created unless this keyword is used to select another FSC_Window to apply the 
;       commands to.
;    wmulti: in, optional, type=intarr(5)
;        Set this keyword in exactly the same way you would set the !P.Multi keyword.
;        It will allow you to display multi-plots in the FSC_Window graphics window.
;    wobject: out, optional, type=object
;       FSC_Window creates a FSC_CmdWindow object. This object reference is returned
;       if this keyword is present.
;    wxpos: in, optional, type=integer, default=5
;       The x offset in device coordinates of the FSC_Window from the upper-left corner of the display.
;    wypos: in, optional, type=integer, default=5
;       The y offset in device coordinates of the FSC_Window from the upper-left corner of the display.
;    wxsize: in, optional, type=integer, default=640
;       The x size in device coordinates of the graphics window.
;    wysize: in, optional, type=integer, default=5
;       The y size in device coordinates of the the graphics window.
;    wtitle: in, opetional, type=string, default='Resizeable Graphics Window'
;       The title of the graphics window. A window index number is appended to the
;       title so multiple FSC_Window programs can be selected.
;          
; :Examples:
;    Test code::
;       data = Loaddata(17)
;       FSC_Window, 'FSC_Plot', data, COLOR='red'
;       FSC_Window, 'FSC_Plot', data, PSYM=2, /Overplot, COLOR='dodger blue', /AddCmd
;       FSC_WIndow, 'FSC_Plot', Loaddata(17), color='olive', linestyle = 2, /Overplot, /AddCmd
;       FSC_Window, /ListCmd
;       FSC_Window, 'FSC_Plot', data, PSYM=-4, COLOR='purple', /ReplaceCMD, CMDINDEX=1
;       
;       Additional examples can be found here:
;       
;          http://www.idlcoyote.com/graphics_tips/fsc_window.html
;           
; :Notes:
;    Notes on using the program::
;    
;       The program is designed to work with any IDL traditional graphics routine
;       that is a procedure and includes no more than three positional parameters.
;       Any number of keywords can be used to specify properties of the graphical
;       output. Any number of graphics commands can be "added" the the FSC_Window.
;       Simply use the ADDCMD keyword to add commands.
;       
;       If your program does not load its own color tables, the color tables in
;       effect when FSC_Window is first called are used to display the graphics
;       commands.
;    
;       To create PostScript output from within FSC_Window, your graphics program
;       has to be written in such a way that it can work properly in the PostScript
;       device. This means there are no Window commands, WSet commands, and the like
;       that are not allowed in the PostScript device. Such commands are allowed in 
;       programs, of course, if they are "protected". Usually such protection looks 
;       like this:
;       
;          IF (!D.Flags AND 256) NE 0 THEN Window, ...
;          
;        FSC_Display is a good program for opening graphics "windows", because such
;        PostScript protection is built into the program. In a PostScript device,
;        FSC_Display produces a "window" with the same aspect ratio as the current
;        dislay graphics window, which is an aid in producing PostScript output that
;        looks like the same output in the display window.
;        
;        Much better looking raster files can be created from the FSC_Window contents,
;        if the raster files are created by converting PostScript files to the raster 
;        file. If the ImageMagick "convert" command can be found on your machine, you
;        will have the option to create raster files using this method. I *highly*
;        recommend doing so, as fonts and other plot annotation will be of much higher
;        quality using this method.
;        
;        FSC_Window has been designed to work with other Coyote Graphics routines: FSC_Plot,
;        FSC_Contour, FSC_Surf, and so on, although I expect it to work with any IDL
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
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
PRO FSC_CmdWindow::ListCommand, command

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    count = self.cmds -> Get_Count()
    FOR j = 0, count-1 DO BEGIN
        thisCmdObj = self.cmds -> Get_Item(j, /DEREFERENCE)
        thisCmdObj -> List, StrTrim(j,2) + '.'
    ENDFOR

END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_CmdWindow::AddCommand, command

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    self.cmds -> Add, command

END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_CmdWindow::DeleteCommand, cmdIndex

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
    
    ; Need a command index?
    IF N_Elements(cmdIndex) EQ 0 THEN BEGIN
        cmdIndex = count - 1
    ENDIF 
    
    ; Do we have a command with this command number?
    IF cmdIndex GT (count-1) THEN Message, 'A command with index ' + StrTrim(cmdIndex,2) + ' does not exist.'
    
    IF cmdIndex GT 0 THEN BEGIN
        self.cmds -> Delete, cmdIndex, /Destroy
        self -> ExecuteCommands
    ENDIF ELSE Message, 'The last command in FSC_Window cannot be deleted. Use ReplaceCmd instead.'
    
END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_CmdWindow::ReplaceCommand, command, cmdIndex

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    IF N_Elements(cmdIndex) EQ 0 THEN BEGIN
        self.cmds -> Delete, /ALL, /Destroy
        self.pmulti = IntArr(5)
        self.cmds -> Add, command
    ENDIF ELSE BEGIN
        oldcmd = self.cmds -> Get_Item(cmdIndex, /DEREFERENCE)
        self.cmds -> Replace_Item, cmdIndex, command
        Obj_Destroy, oldcmd
    ENDELSE
    
END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_CmdWindow::CreatePostScriptFile, event

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    PS_Start, /GUI, CANCEL=cancelled
    IF cancelled THEN RETURN
    
    self -> ExecuteCommands
    PS_End

END ;----------------------------------------------------------------------------------------------------------------


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
    
    CASE filetype OF
       'BMP':  filename = FSC_Pickfile(FILE='fsc_window.bmp', /WRITE, TITLE='Select an Output File...')
       'GIF':  filename = FSC_Pickfile(FILE='fsc_window.gif', /WRITE, TITLE='Select an Output File...')
       'JPEG': filename = FSC_Pickfile(FILE='fsc_window.jpg', /WRITE, TITLE='Select an Output File...')
       'PNG':  filename = FSC_Pickfile(FILE='fsc_window.png', /WRITE, TITLE='Select an Output File...')
       'TIFF': filename = FSC_Pickfile(FILE='fsc_window.tif', /WRITE, TITLE='Select an Output File...')
    ENDCASE
    IF filename EQ "" THEN RETURN
    root_name = FSC_Base_Filename(filename, DIRECTORY=dirName)
    outname = Filepath(ROOT_DIR=dirname, root_name)
    
    ; What kind of raster file.
    CASE rasterType OF
    
        ; Normal raster.
        0: BEGIN
           WSet, self.wid
           void = TVRead(TYPE=fileType, FILENAME=outname, /NODIALOG)
           END
           
        1: BEGIN
           
           thisname = outname + '.ps'
           PS_Start, FILENAME=thisname
           self -> ExecuteCommands
           
           CASE filetype OF
                'BMP':  PS_END, /BMP,  /DELETE_PS
                'GIF':  PS_END, /GIF,  /DELETE_PS
                'JPEG': PS_END, /JPEG, /DELETE_PS
                'PNG':  PS_END, /PNG,  /DELETE_PS
                'TIFF': PS_END, /TIFF, /DELETE_PS
           ENDCASE
        
           END
    
    ENDCASE
END ;----------------------------------------------------------------------------------------------------------------


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


PRO FSC_CmdWindow_Dispatch_Events, event

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    Widget_Control, event.top, GET_UVALUE=self
    Widget_Control, event.id, GET_UVALUE=eventType
    IF Obj_Valid(eventType) THEN eventType = 'TLB_RESIZE'
    
    CASE eventType OF 
        
        'TLB_RESIZE': self -> Resize, event
        'POSTSCRIPT': self -> CreatePostscriptFile, event
        ELSE: self -> SaveAsRaster, event
    
    ENDCASE
    
END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_CmdWindow::ExecuteCommands

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        !P.Multi = thisMulti
        RETURN
    ENDIF
    
    ; Store the current !P.MULTI state.
    thisMulti = !P.Multi

    ; Make this window the current graphics window.
    IF (!D.Flags AND 256) NE 0 THEN BEGIN
        currentWindow = !D.Window
        WSet, self.wid
    ENDIF
    
    ; Get the current color table vectors so they can be restored.
    TVLCT, rr, gg, bb, /GET
    
    ; Load the color vectors.
    TVLCT, self.r, self.g, self.b
    
    ; Erase the window.
    IF self.eraseit THEN FSC_Erase, *self.background 
    
    ; Are we doing multiple commands?
    IF Total(self.pmulti) NE 0 THEN !P.Multi = self.pmulti
        
    ; How many commands are there?
    n_cmds = self.cmds -> Get_Count()
    
    ; Execute the commands.
    FOR j=0,n_cmds-1 DO BEGIN
        thisCmdObj = self.cmds -> Get_Item(j, /DEREFERENCE)
        thisCmdObj -> Draw
    ENDFOR
    
    ; Restore the colors in effect when we entered.
    TVLCT, rr, gg, bb
    
    ; Set the !P.Multi system variable back to its original values.
    !P.Multi = thisMulti
    
    ; Reset the current graphics window, if possible.
    IF (!D.Flags AND 256) NE 0 THEN BEGIN
       IF (currentWindow GE 0) && WindowAvailable(currentWindow) THEN BEGIN
            WSet, currentWindow
       ENDIF ELSE WSet, -1
    ENDIF

END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_CmdWindow_Cleanup, tlb
    Widget_Control, tlb, Get_UValue=self
    Obj_Destroy, self
END ;----------------------------------------------------------------------------------------------------------------


FUNCTION FSC_CmdWindow::Init, $
   command, $                       ; The graphics "command" to execute.
   p1, p2, p3, $                    ; The three allowed positional parameters.
   _Extra = extra, $                ; Any extra keywords. Usually the "command" keywords.
   Group_Leader = group_leader, $   ; The group leader of the FSC_Window program.
   AddCmd=addcmd, $                 ; Set this keyword to add a command to the interface.
   Method=method, $                 ; If set, will use CALL_METHOD instead of CALL_PROCEDURE to execute command.
   ReplaceCmd=replacecmd, $         ; Replace the current command and execute in the current window.
   WEraseIt = Weraseit, $           ; Set this keyword to erase the display before executing the command.
   WMulti = wmulti, $               ; Set this in the same way !P.Multi is used.
   WXSize = wxsize, $               ; The X size of the FSC_Window graphics window in pixels. By default: 400.
   WYSize = wysize, $               ; The Y size of the FSC_Window graphics window in pixels. By default: 400.
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
    
    ; If method is set, the first positional parameter must be present,
    ; and it must be a valid object reference.
    IF method THEN BEGIN
        IF N_Elements(p1) EQ 0 THEN $
            Message, 'The first positional parameter must be present to make a method call.'
        IF ~Obj_Valid(p1) THEN $
            Message, 'The first positional parameter must be a valid object reference when making method calls.'
    ENDIF
    IF N_Elements(wxsize) EQ 0 THEN xsize = 640 ELSE xsize = wxsize
    IF N_Elements(wysize) EQ 0 THEN ysize = 512 ELSE ysize = wysize
    IF N_Elements(wxpos) EQ 0 THEN xpos = -1 ELSE xpos = wxpos
    IF N_Elements(wypos) EQ 0 THEN ypos = -1 ELSE ypos = wypos
    IF N_Elements(wbackground) EQ 0 THEN BEGIN
        background=!P.Background 
    ENDIF ELSE BEGIN
        background = wbackground
        eraseit = 1
    ENDELSE
    IF N_Elements(eraseIt) EQ 0 THEN eraseIt = Keyword_Set(weraseit)

    ; The commands will be placed in a linked list for execution.
    self.cmds = Obj_New('LinkedList')
    IF Obj_Valid(self.cmds) EQ 0 THEN Message, 'Failed to make the LinkedList for the commands.'
    thisCommand = Obj_New('FSC_Window_Command', COMMAND=command, $
            P1=p1, P2=p2, P3=p3, KEYWORDS=extra, TYPE=method)
    IF Obj_Valid(thisCommand) THEN self.cmds -> Add, thisCommand ELSE Message, 'Failed to make command object.'
    
    ; Store the current color table vectors
    TVLCT, rr, gg, bb, /Get
    self.r = rr
    self.g = gg
    self.b = bb
    
    ; Create the widgets for the program.
    self.tlb = Widget_Base(/TLB_SIZE_EVENTS, MBar=menuID)
    
    saveID = Widget_Button(menuID, Value='Save As...')
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
    
    ; Create draw widget.
    retain = (StrUpCase(!Version.OS_Family) EQ 'UNIX') ? 2 : 1
    self.drawID = Widget_Draw(self.tlb, XSIZE=xsize, YSize=ysize, RETAIN=retain) 
    
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
    
    IF N_Elements(wtitle) EQ 0 THEN wtitle = "Resizeable Graphics Window"
    wtitle = wtitle + ' (' + StrTrim(wid,2) + ')'
    Widget_Control, self.tlb, TLB_Set_Title=wtitle

    ; Load object properties.
    self.background = Ptr_New(background)
    self.eraseIt = eraseIt
    IF N_Elements(wmulti) NE 0 THEN BEGIN
       FOR j=0,N_Elements(wmulti)-1 DO self.pmulti[j] = wmulti[j]
    ENDIF

    ; Execute the commands.
    self -> ExecuteCommands
    
    ; Get it running.
    WIDGET_CONTROL, /MANAGED, self.tlb
    XManager, 'fsc_window', self.tlb, /No_Block, $
        Event_Handler='FSC_CmdWindow_Dispatch_Events', $
        Cleanup = 'FSC_CmdWindow_Cleanup', $
        Group_Leader=group_leader
    
    ; Store the self reference in the UVALUE of the TLB.
    Widget_Control, self.tlb, SET_UValue=self
    
    ; Each instance of FSC_Window will store evidence of its
    ; existance in a linked list.
    DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
    IF ~exists THEN BEGIN
        fsc_window_list = Obj_New("LinkedList")
        DefSysV, '!FSC_WINDOW_LIST', fsc_window_list 
        fsc_window_list -> Add, {FSC_WINDOW_ID, self.tlb, wid, self}
    ENDIF ELSE BEGIN
        IF Obj_Valid(!FSC_WINDOW_LIST) THEN BEGIN
            !FSC_WINDOW_LIST -> Add, {FSC_WINDOW_ID, self.tlb, wid, self}
        ENDIF ELSE BEGIN
            !FSC_WINDOW_LIST = Obj_New('LinkedList')
            !FSC_WINDOW_LIST-> Add, {FSC_WINDOW_ID, self.tlb, wid, self}
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

    Ptr_Free, self.background
    count = self.cmds -> Get_Count()
    FOR j=0,count-1 DO Obj_Destroy, self.cmds -> Get_Item(j, /DEREFERENCE)
    Obj_Destroy, self.cmds
    
    ; You have to remove yourself from the list of valid FSC_Windows.
    theList = !FSC_WINDOW_LIST
    IF Obj_Valid(theList) THEN BEGIN
        structs = theList -> Get_Item(/ALL, /DEREFERENCE)
        index = Where(structs.windowObj[*] EQ self, count)
        IF count GT 0 THEN theList -> Delete, index[0]
    ENDIF 
    
    ; If the list doesn't have any more FSC_Windows objects in it,
    ; delete the list so it doesn't waste memory.
    IF theList -> Get_Count() EQ 0 THEN Obj_Destroy, theList
    
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


PRO FSC_Window_Command::Draw

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
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
                     0: Call_Method, self.command, _Extra=*self.keywords
                     1: Call_Method, self.command, *self.p1, _Extra=*self.keywords
                     2: Call_Method, self.command, *self.p1, *self.p2, _Extra=*self.keywords
                     3: Call_Method, self.command, *self.p1, *self.p2, *self.p3, _Extra=*self.keywords
                 ENDCASE
             ENDIF ELSE BEGIN
                 CASE self.nparams OF
                     0: Call_Method, self.command
                     1: Call_Method, self.command, *self.p1
                     2: Call_Method, self.command, *self.p1, *self.p2
                     3: Call_Method, self.command, *self.p1, *self.p2, *self.p3
                 ENDCASE
             ENDELSE
             
           END
    
    ENDCASE
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
                 windowObj: Obj_New() $
             }
END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_CmdWindow__Define, class

    class = { FSC_CMDWINDOW, $
              tlb: 0L, $                    ; The identifier of the top-level base widget.
              r: BytArr(256), $             ; The red color table vector.
              g: BytArr(256), $             ; The green color table vector.
              b: BytArr(256), $             ; The blue color table vector.
              pmulti: LonArr(5), $          ; Identical to !P.Multi.
              cmds: Obj_New(), $            ; A linkedlist object containing the graphics commands.
              background: Ptr_New(), $      ; The background color.
              eraseit: 0B, $                ; Do we need to erase the display.
              wid: 0L, $                    ; The window index number of the graphics window.
              drawid: 0L $                  ; The identifier of the draw widget.
            }
            
END ;----------------------------------------------------------------------------------------------------------------


PRO FSC_Window, $
   command, $                       ; The graphics "command" to execute.
   p1, p2, p3, $                    ; The three allowed positional parameters.
   _Extra = extra, $                ; Any extra keywords. Usually the "command" keywords.
   Group_Leader = group_leader, $   ; The group leader of the FSC_Window program.
   Method=method, $                 ; If set, will use CALL_METHOD instead of CALL_PROCEDURE to execute command.
   WBackground = wbackground, $     ; The background color. Set to !P.Background by default.
   WErase = weraseit, $             ; Set this keyword to erase the display before executing the command.
   WMulti = wmulti, $               ; Set this in the same way !P.Multi is used.   
   WXSize = wxsize, $               ; The X size of the FSC_Window graphics window in pixels. By default: 400.
   WYSize = wysize, $               ; The Y size of the FSC_Window graphics window in pixels. By default: 400.
   WTitle = wtitle, $               ; The window title.
   WXPos = wxpos, $                 ; The X offset of the window on the display. The window is centered if not set.
   WYPos = wypos, $                 ; The Y offset of the window on the display. The window is centered if not set.
   
   AddCmd=addcmd, $                 ; Set this keyword to add a command to the interface.
   CmdIndex=cmdIndex, $             ; Set this keyword to identify the index of the command to manipulate.
   DeleteCmd=deletecmd, $           ; Set the keyword to delete a command.
   ListCmd=listCmd, $               ; Set this keyword to list the commands in the window.
   ReplaceCmd=replacecmd, $         ; Set this keyword to replace a command in the window.
   WinID=winid, $                   ; Set this keyword to select an FSC_Window.
   WObject=wobject                  ; The FSC_CMDWindow object. A return value.

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
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
                        Message, 'Cannot find an FSC_Window with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[winID]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    thisWindowStruct.windowObj -> ListCommand
                ENDIF ELSE BEGIN
                    Message, 'The specified FSC_Window object is not a valid window object.'
                ENDELSE
                RETURN
           ENDIF ELSE Message, 'The FSC_Window object is not a valid window object.'
       ENDIF ELSE Message, 'An FSC_Window object not exist to add a command to.'
   ENDIF
    
    
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
                        Message, 'Cannot find an FSC_Window with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[winID]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                
                    ; If the cmdIndex is undefined, the last entered command is deleted.
                    ; It is impossible to delete all commands from the window.
                    thisWindowStruct.windowObj -> DeleteCommand, cmdIndex
                ENDIF ELSE BEGIN
                    Message, 'The specified FSC_Window object is not a valid window object.'
                ENDELSE
                RETURN
           ENDIF ELSE Message, 'The FSC_Window object is not a valid window object.'
       ENDIF ELSE Message, 'An FSC_Window object not exist to add a command to.'
   ENDIF


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
                        Message, 'Cannot find an FSC_Window with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[winID]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    newCommand = Obj_New('FSC_Window_Command', COMMAND=command, $
                        P1=p1, P2=p2, P3=p3, KEYWORDS=extra, TYPE=Keyword_Set(method))
                        
                    ; If the cmdIndex is undefined, ALL current commands in the window are replaced.
                    thisWindowStruct.windowObj -> ReplaceCommand, newCommand, cmdIndex
                    thisWindowStruct.windowObj -> ExecuteCommands
                ENDIF ELSE BEGIN
                    Message, 'The specified FSC_Window object is not a valid window object.'
                ENDELSE
                RETURN
           ENDIF ELSE Message, 'The FSC_Window object is not a valid window object.'
       ENDIF ELSE Message, 'An FSC_Window does not exist to add a command to.'
    
    ENDIF 
   
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
                        Message, 'Cannot find an FSC_Window with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[winID]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    newCommand = Obj_New('FSC_Window_Command', COMMAND=command, $
                        P1=p1, P2=p2, P3=p3, KEYWORDS=extra, TYPE=Keyword_Set(method))
                    thisWindowStruct.windowObj -> AddCommand, newCommand
                    thisWindowStruct.windowObj -> ExecuteCommands
                ENDIF ELSE BEGIN
                    Message, 'The specified FSC_Window object is not a valid window object.'
                ENDELSE
                RETURN
           ENDIF ELSE Message, 'The FSC_Window object is not a valid window object.'
       ENDIF ELSE Message, 'An FSC_Window does not exist to add a command to.'
    
    ENDIF 
   
   ; Othersize, make the command object.
   wobject = Obj_New('FSC_CmdWindow', $
       command, $                       ; The graphics "command" to execute.
       p1, p2, p3, $                    ; The three allowed positional parameters.
       _Extra = extra, $                ; Any extra keywords. Usually the "command" keywords.
       Group_Leader = group_leader, $   ; The group leader of the FSC_Window program.
       Method=method, $                 ; If set, will use CALL_METHOD instead of CALL_PROCEDURE to execute command.
       WBackground = wbackground, $     ; The background color. Not used unless set.
       WMulti = wmulti, $               ; Set this in the same way !P.Multi is used.
       WErase = weraseit, $             ; Set this keyword to erase the display before executing the command.
       WXSize = wxsize, $               ; The X size of the FSC_Window graphics window in pixels. By default: 400.
       WYSize = wysize, $               ; The Y size of the FSC_Window graphics window in pixels. By default: 400.
       WTitle = wtitle, $               ; The window title.
       WXPos = wxpos, $                 ; The X offset of the window on the display. The window is centered if not set.
       WYPos = wypos )                  ; The Y offset of the window on the display. The window is centered if not set.
       
END