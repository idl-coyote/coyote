; docformat = 'rst'
;
; NAME:
;   cgCmdWindow
;
; PURPOSE:
;   Creates a "command" window for IDL traditional commands (Plot, Contour, 
;   Surface, etc. or for Coyote Graphics routines, cgPlot, cgContour, cgSurf, etc.). 
;   In addition, the window contents can be saved as PostScript files or as raster image 
;   files. If ImageMagick is installed on your machine, the raster image files can be 
;   created in very high quality from PostScript files. This program can be used in 
;   place of a normal draw widget or as a stand-alone program.
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
;  Creates a "command" window for IDL traditional commands (Plot, Contour, 
;  Surface, etc. or for Coyote Graphics routines, cgPlot, cgContour, cgSurf, etc.). 
;  In addition, the window contents can be saved as PostScript files or as raster image 
;  files. If ImageMagick is installed on your machine, the raster image files can be 
;  created in very high quality from PostScript files. This program can be used in 
;  place of a normal draw widget or as a stand-alone program.
;
;  The program is designed to work with any IDL traditional graphics routine
;  that is a procedure and includes no more than four positional parameters.
;  Any number of keywords can be used to specify properties of the graphical
;  output. Any number of graphics commands can be "added" the the window.
;  Simply use the AddCommand method to add commands.
;  
;  If your program does not load its own color tables, the color tables in
;  effect when this program is initiated are used to display the graphics
;  commands.
;    
;  To create PostScript output from within the program, your graphics program
;  has to be written in such a way that it can work properly in the PostScript
;  device. This means there are no Window commands, WSet commands, and the like
;  that are not allowed in the PostScript device. Such commands are allowed in 
;  programs, of course, if they are "protected". Usually such protection looks 
;  like this::
;  
;     IF (!D.Flags AND 256) NE 0 THEN Window, ...
;     
;  The Coyote Graphics program `cgDisplay` is a good program for opening graphics 
;  "windows", because such PostScript protection is built into the program. In a PostScript 
;  device, cgDisplay produces a "window" with the same aspect ratio as the current
;  display graphics window, which is an aid in producing PostScript output that
;  looks like the same output in the display window.
;   
;  Much better looking raster files can be created from the graphics window contents,
;  if the raster files are created by converting PostScript files to the raster 
;  file. If the ImageMagick "convert" command can be found on your machine, you
;  will have the option to create raster files using this method. I *highly*
;  recommend doing so, as fonts and other plot annotation will be of much higher
;  quality using this method.
;   
;  This program has been designed to work with other Coyote Graphics routines: `cgPlot`,
;  `cgContour`, `cgSurf`, and so on, although I expect it to work with any IDL
;  traditional graphics routine, if the routine is well written.
;        
; :Categories:
;    Graphics
;    
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;     
; :History:
;     Change History::
;        Separated from old cgWindow program 22 Jan 2012, by David W. Fanning.
;        Fixed a typo in PackageCommand method that prevented extra keywords from
;           being collectd. 26 Jan 2012. DWF.
;        Fixed a problem with MULTI keyword. 31 Jan 2012. DWF.
;        Added a COPY method. 7 Feb 2012. DWF.
;        Fixed a problem with input filenames in the AutoRasterFile method. 22 Feb 2012. DWF.
;        Modified Draw_Widget creation routine to account for events that were added in 
;           different versions of IDL. 23 Feb 2012. DWF.
;        Added ability to use IM_WIDTH keyword to set the width of raster file output
;           created with ImageMagick. 3 April 2012. DWF.
;        Forgot to specify the GROUP_LEADER when calling PS_START. Caused PSConfig to
;           run though its block when cgWindow was called from a blocking widget program.
;           5 June 2012. DWF.
;        Added the ability to save the file name and directory of the last output file, so
;            that subsequent file saves can use that last name and directory as a starting
;            default filename. 11 July 2012. DWF.
;        In decompling cgWindow from cgCmdWindow, I accidentally named the WASPECT keyword ASPECT. Restored
;             original name in this version. 13 July 2012. DWF.
;        I added a new method, ReplaceEscapeSequences, that can evaluate escape sequences in
;             string parameters and keywords to call the appropriate cgSymbol value at run-time.
;             This eliminates the need for alternate keywords. 27 July 2012. DWF.
;        Added WDestroyObjects keyword to destroy objects parameters, if needed. 11 November 2012. DWF.
;        Not adding IM_WIDTH parameter from cgWindow_GetDefs. 19 November 2012. DWF.
;        Modified ReplaceEscapeSequence method to use cgCheckForSymbols. 24 November 2012. DWF.
;        Modified to allow keywords to turn off messages from PS_START and PS_END with keywords. 27 November 2012. DWF.
;        The output filename was not specified correctly when making PDF file automatically. Fixed. 2 Dec 2012. DWF.
;-


;+
; This method initializes the object that is at the heart of the Coyote Graphics system.
; It allows you to both configure the draw widget for events and to load a command into
; the command window.
; 
; :Params:
;    parent: in, optional
;       The parent base widget for this draw widget object. If not defined,
;       the program will create its own top-level base widget.
;       
; :Keywords:
;    altps_Keywords: in, optional, type=string
;       A structure containing alternative keyword names (as tags) and values for
;       those keywords to be used when the current device is the PostScript device.
;       See http://www.idlcoyote.com/cg_tips/kwexpressions.php and the examples
;       below for details on how to use this keyword.
;    altps_Params: in, optional, type=IntArr(3)
;       A structure containing alternative parameter values to be used when 
;       the current device is the PostScript device. Structure names are restricted
;       to the names "P1", "P2", "P3" and "P4"to correspond to the equivalent positional
;       parameter. See http://www.idlcoyote.com/cg_tips/kwexpressions.php and the 
;       examples below for details on how to use this keyword.
;    background: in, optional, type=varies, default=!P.Background
;       The background color of the window. Specifying a background color 
;       automatically sets the WErase keyword.
;    button_events: in, optional, type=boolean
;       Set this keyword to turn button events on for the draw widget.
;    command: in, required, type=string
;       The graphics procedure command to be executed. This parameter
;       must be a string and the the command must be a procedure. Examples
;       are 'Surface', 'Contour', 'Plot', 'cgPlot', cgContour, etc.
;    cmddelay: in, optional, type=float
;       If this keyword is set to a value other than zero, there will be a 
;       delay of this many seconds between command execution. This will permit
;       "movies" of command sequences to be displayed.
;    drop_events: in, optional, type=boolean
;       Set this keyword to turn drop events on for the draw widget.
;    eraseit: in, optional, type=boolean, default=0
;       Set this keyword to cause the window to be erased before graphics commands 
;       are drawn. This may need to be set, for example, to display images.
;    event_handler: in, optional, type=string
;        The name of an event handler procedure to accept events from
;        the draw widget. Write the event handler procedure with two
;        positional parameters. The first will be the event structure
;        returned from the draw widget. The second will be the cgCmdWindow
;        object reference (i.e., self) to allow you to manipulate the
;        command window.
;    group_leader: in, optional
;       The identifier of a widget to serve as a group leader for this program.
;       If the group leader is destroyed, this program is also destroyed. Used
;       when calling this program from another widget program.
;    keyboard_events: in, optional, type=boolean
;       Set this keyword to turn keyboard events on for the draw widget.
;    method: in, optional, type=boolean, default=0
;       Set this keyword if the command is an object method call rather than a 
;       procedure call. If this keyword is set, the first positional parameter, p1,
;       must be present and must be a valid object reference.
;    motion_events: in, optional, type=boolean
;       Set this keyword to turn motion events on for the draw widget.
;    multi: in, optional, type=intarr(5)
;        Set this keyword in exactly the same way you would set the !P.Multi keyword.
;        It will allow you to display multi-plots in the cgWindow graphics window.
;    oxmargin: in, optional, type=float
;       A two-element array indicating the left and right X outside margins for the
;       graphical display. Used only when doing multiple plots with `WMulti`.
;    oymargin: in, optional, type=float
;       A two-element array indicating the bottom and top Y outside margins for the
;       graphical display. Used only when doing multiple plots with `WMulti`.
;    p1: in, optional, type=any
;       The first positional parameter appropriate for the graphics command.
;    p2: in, optional, type=any
;       The second positional parameter appropriate for the graphics command.
;    p3: in, optional, type=any
;       The third positional parameter appropriate for the graphics command.
;    p4: in, optional, type=any
;       The fourth positional parameter appropriate for the graphics command.
;    replacecmd: in, optional, type=boolean, default=0
;       Set this keyword to replace a graphics command from an cgWindow.
;       If CmdIndex is undefined, *all* commands in the window are replaced. Use 
;       WinID to identify the cgWindow you are interested in. If WinID is 
;       undefined, the last cgWindow created is used for the replacement.
;    storage: in, optional, type=any
;       Any user-defined IDL variable will be stored in the object storage location.
;       Defined here for convenience. Same as `Storage` keyword for the SetProperty method.
;    tracking_events: in, optional, type=boolean
;       Set this keyword to turn tracking events on for the draw widget.
;    waspect: in, optional, type=float, default=normal
;       Set this keyword to the aspect ratio you would like the window to have.
;       The aspect ratio is calculated as (ysize/xsize). Must be a float value.
;       If this keyword is set, the window will maintain this aspect ratio,
;       even when it is resized.
;    wdestroyobjects: in, optional, type=boolean, default=0
;       If this keyword is set, and any of the input parameters p1-p4 is an object,
;       the object parameter will be destroyed when the window is destroyed.
;    wheel_events: in, optional, type=boolean
;       Set this keyword to turn wheel events on for the draw widget.
;    wtitle: in, optional, type=string, default='Resizeable Graphics Window'
;       The title of the graphics window if the program creates its own top-level
;       base widget. A window index number is appended to the title so multiple cgWindow 
;       programs can be selected. Also used to register the widget program and as the
;       title of the object when it is stored.
;    wxpos: in, optional, type=integer, default=5
;       The x offset in device coordinates of the window from the upper-left corner of 
;       the display, if the program creates its own top-level base. Otherwise, this keyword
;       is ignored.
;    wypos: in, optional, type=integer, default=5
;       The y offset in device coordinates of the window from the upper-left corner of 
;       the display, if the program creates its own top-level base. Otherwise, this keyword
;       is ignored.
;    wxsize: in, optional, type=integer, default=640
;       The x size in device coordinates of the graphics window.
;    wysize: in, optional, type=integer, default=512
;       The y size in device coordinates of the the graphics window.
;    _extra: in, optional
;       The "extra" keywords for the command that is being added to the window.
;-
FUNCTION cgCmdWindow::Init, parent, $
   AltPS_Keywords=altps_Keywords, $ ; A structure of PostScript alternative keywords and values.
   AltPS_Params=altps_Params, $     ; A structure of PostScript alternative parameters and values. 
   Background = background, $       ; The background color. Set to !P.Background by default.
   Button_Events=button_events, $   ; Set this keyword to allow button events in the draw widget.
   CmdDelay=cmdDelay, $             ; Set this keyword to a value to "wait" before executing the next command.
   Command=command, $               ; The graphics "command" to execute.
   Drop_Events=drop_events, $       ; Set this keyword to allow drop events in the draw widget.
   EraseIt = eraseit, $             ; Set this keyword to erase the display before executing the command.
   Event_Handler=event_handler, $   ; Set this keyword to the name of an event handler procedure to handle draw widget events.
   Group_Leader = group_leader, $   ; The group leader of the cgWindow program.
   Keyboard_Events=keyboard_events, $ ; Set this keyword to allow keyboard events in the draw widget.
   Method=method, $                 ; If set, will use CALL_METHOD instead of CALL_PROCEDURE to execute command.
   Motion_Events=motion_events, $   ; Set this keyword to allow motion events in the draw widget.
   Multi = multi, $                 ; Set this in the same way !P.Multi is used.
   OXMargin = xomargin, $           ; Set the !X.OMargin. A two element array.
   OYMargin = yomargin, $           ; Set the !Y.OMargin. A two element array
   P1=p1, $                         ; The first postitional parameter in a graphics command loaded for display.
   P2=p2, $                         ; The sedond postitional parameter in a graphics command loaded for display.
   P3=p3, $                         ; The third postitional parameter in a graphics command loaded for display.
   P4=p4, $                         ; The fourth postitional parameter in a graphics command loaded for display. 
   ReplaceCmd=replacecmd, $         ; Replace the current command and execute in the current window.
   Storage=storage, $               ; A storage pointer location. Used like a user value in a widget.
   Tracking_Events=tracking_events, $ ; Set this keyword to allow tracking events in the draw widget.
   WAspect = waspect, $             ; Set the window aspect ratio to this value.
   WDestroyObjects=wdestroyobjects, $ ; Set this keyword to destroy object parameters upon exit.
   Wheel_Events=wheel_events, $     ; Set this keyword to allow wheel events in the draw widget.
   WTitle = title, $                ; The window title.
   WXPos = xpos, $                  ; The X offset of the window on the display. The window is centered if not set.
   WXSize = xsize, $                ; The X size of the cgWindow graphics window in pixels. By default: 400.
   WYPos = ypos, $                  ; The Y offset of the window on the display. The window is centered if not set.
   WYSize = ysize, $                ; The Y size of the cgWindow graphics window in pixels. By default: 400.
   _Extra = extra                   ; Any extra keywords. Usually the "command" keywords.

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
       AdjustSize = d_adjustsize, $                      ; Adjust charsize to window size.
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
       Aspect = d_aspect, $                              ; The aspect ratio of the window.
       Palette = d_palette, $                            ; The color table palette to use for the window.
       
       ; PDF properties.
       PDF_Unix_Convert_Cmd = d_pdf_unix_convert_cmd, $  ; Command to convert PS to PDF.
       PDF_Path = d_pdf_path, $                          ; The path to the Ghostscript conversion command.
   
       ; ImageMagick Properties.
       IM_Transparent = d_im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
       IM_Density = d_im_density, $                      ; Sets the density parameter on ImageMagick convert command.
       IM_Raster = d_im_raster, $                        ; Create raster files via ImageMagick.
       IM_Resize = d_im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
       IM_Options = d_im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
       IM_Width = d_im_width, $                          ; The width of the raster file output.
        
       ; PostScript properties.
       PS_Decomposed = d_ps_decomposed, $                ; Sets the PostScript color mode.
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
            Message, 'The parameter P1 must be present to make a method call.'
        IF ~Obj_Valid(p1) THEN $
            Message, 'The parameter P1 must be a valid object reference when making method calls.'
    ENDIF
    SetDefaultValue, waspect, d_aspect 
    SetDefaultValue, xsize, d_xsize 
    SetDefaultValue, ysize, d_ysize 
    SetDefaultValue, xpos, d_xpos 
    SetDefaultValue, ypos, d_ypos 
    SetDefaultValue, button_events, 0, /Boolean
    SetDefaultValue, drop_events, 0, /Boolean
    SetDefaultValue, keyboard_events, 0, /Boolean
    SetDefaultValue, motion_events, 0, /Boolean
    SetDefaultValue, tracking_events, 0, /Boolean
    SetDefaultValue, wheel_events, 0, /Boolean
    IF N_Elements(storage) NE 0 THEN self.storage = Ptr_New(storage)
    IF N_Elements(background) EQ 0 THEN BEGIN
        background = d_background
        IF N_Elements(command) EQ 0 THEN eraseit = 1
        IF (N_Elements(command) NE 0) && cgCoyoteGraphic(command) THEN eraseit = 1
    ENDIF ELSE BEGIN
        eraseit = 1
    ENDELSE
    IF N_Elements(eraseIt) EQ 0 THEN eraseIt = d_eraseit ELSE eraseIt = Keyword_Set(eraseIt)

    ; The commands will be placed in a linked list for execution.
    self.cmds = Obj_New('LinkedList')
    IF Obj_Valid(self.cmds) EQ 0 THEN Message, 'Failed to make the LinkedList for the commands.'
    
    ; Add a command, if you have one. Otherwise, just make the window.
    IF (N_Elements(command) NE 0) THEN BEGIN
        thisCommand = Obj_New('cgWindow_Command', COMMAND=command, $
                P1=p1, P2=p2, P3=p3, P4=p4, KEYWORDS=extra, TYPE=method, $
                ALTPS_KEYWORDS=altps_Keywords, ALTPS_PARAMS=altps_Params, $
                DESTROYOBJECTS=Keyword_Set(wdestroyobjects))
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
    IF N_Elements(parent) NE 0 THEN BEGIN
       self.tlb = parent
       createParent = 0
    ENDIF ELSE BEGIN    
       self.tlb = Widget_Base(/TLB_SIZE_EVENTS, MBar=menuID, UNAME='TLB_RESIZE')
       createParent = 1
    ENDELSE
    
    ; Create buttons if you created the parent widget.
    IF createParent THEN BEGIN
    
        fileID = Widget_Button(menuID, Value='File')
        saveID = Widget_Button(fileID, Value='Save Window As...', /MENU)
        button = Widget_Button(saveID, Value='PostScript File', UNAME='POSTSCRIPT')
        button = Widget_Button(saveID, Value='PDF File', UNAME='PDF')
        raster = Widget_Button(saveID, Value='Raster Image File', /MENU)
        
        button = Widget_Button(raster, Value='BMP', UNAME='RASTER_BMP')
        button = Widget_Button(raster, Value='GIF', UNAME='RASTER_GIF')
        button = Widget_Button(raster, Value='JPEG', UNAME='RASTER_JPEG')
        button = Widget_Button(raster, Value='PNG', UNAME='RASTER_PNG')
        button = Widget_Button(raster, Value='TIFF', UNAME='RASTER_TIFF')
        
        ; If you can find ImageMagick on this machine, you can convert to better
        ; looking raster files.
        IF cgHasImageMagick() EQ 1 THEN BEGIN
            imraster = Widget_Button(saveID, Value='Raster Image File via ImageMagick', /MENU)
            button = Widget_Button(imraster, Value='BMP', UNAME='IMAGEMAGICK_BMP')
            button = Widget_Button(imraster, Value='GIF', UNAME='IMAGEMAGICK_GIF')
            button = Widget_Button(imraster, Value='JPEG', UNAME='IMAGEMAGICK_JPEG')
            button = Widget_Button(imraster, Value='PNG', UNAME='IMAGEMAGICK_PNG')
            button = Widget_Button(imraster, Value='TIFF', UNAME='IMAGEMAGICK_TIFF')
        ENDIF
        
        button = Widget_Button(fileID, Value='Save Current Visualization', /Separator, UNAME='SAVECOMMANDS')
        button = Widget_Button(fileID, Value='Restore Visualization', UNAME='RESTORECOMMANDS')
        button = Widget_Button(fileID, Value='Quit', /Separator, UNAME='QUIT')
    
    ENDIF
    
    ; Check to see if you have to create a window with a particular aspect ratio.
    ; If so, your xsize and ysize values will need to be adjusted.
    IF waspect NE 0 THEN BEGIN
         IF waspect GE 1 THEN BEGIN
            xsize = Round(ysize / waspect)
         ENDIF ELSE BEGIN
            ysize = Round(xsize * waspect)
         ENDELSE
     ENDIF
    
    ; Create draw widget. UNIX versions of IDL have a bug in which creating
    ; a draw widget as the very first window in an IDL session causes both
    ; !P.Background and !P.Color to be set to white. I know, it's odd. But
    ; doing this little trick fixes the problem.
    tempBackground = !P.Background
    tempColor = !P.Color
    retain = (StrUpCase(!Version.OS_Family) EQ 'UNIX') ? 2 : 1
    IF ~createParent THEN BEGIN
        notify_realize = 'cgCmdWindowDrawRealize'
        kill_notify = 'cgCmdWindowKillNotify'
    ENDIF
    self.drawID = Widget_Draw(self.tlb, XSIZE=xsize, YSize=ysize, RETAIN=retain, $
       UVALUE=self, Notify_Realize=notify_realize, $
       UNAME='DRAW_WIDGET', Kill_Notify=kill_notify, $
       BUTTON_EVENTS=button_events, $
       KEYBOARD_EVENTS=keyboard_events, $
       MOTION_EVENTS=motion_events, $
       TRACKING_EVENTS=tracking_events) 
    release = Float(!Version.Release) 
    IF (N_Elements(wheel_events) NE 0) && (release GE 6.2) THEN $
       Widget_Control, self.drawID, DRAW_WHEEL_EVENTS=wheel_events
    IF (N_Elements(drop_events) NE 0) && (release GE 6.3) THEN $
       Widget_Control, self.drawID, SET_DROP_EVENTS=drop_events
       
    
    !P.Background = Temporary(tempBackground)
    !P.Color = Temporary(tempColor)
    
    ; Do we need to center the widget? Only do this if you created the parent.
    IF (xpos EQ -1) && (ypos EQ -1) && createParent THEN BEGIN
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
        cgCenterTLB, self.tlb, xpos, ypos, /NOCENTER, /DEVICE
    ENDIF ELSE BEGIN
        IF createParent THEN cgCenterTLB, self.tlb, xpos, ypos, /NOCENTER, /DEVICE
    ENDELSE
    
    ; Display the widget and get window index number.
    currentWindow = !D.Window
    IF createParent THEN BEGIN 
        Widget_Control, self.tlb, /Realize
        Widget_Control, self.drawID, Get_Value=wid
        self.wid = wid
        WSet, wid
        IF N_Elements(title) EQ 0 THEN BEGIN
            title = d_title
            title = title + ' (' + StrTrim(wid,2) + ')'
        ENDIF
        Widget_Control, self.tlb, TLB_Set_Title=title
    ENDIF ELSE BEGIN
        IF N_Elements(title) EQ 0 THEN title = "cgCmdWindow"
    ENDELSE
    self.title = title
    

    ; Load object properties.
    self.background = Ptr_New(background)
    IF N_Elements(cmdDelay) NE 0 THEN self.delay = cmdDelay ELSE self.delay = d_delay
    self.eraseIt = eraseIt
    IF N_Elements(multi) NE 0 THEN BEGIN
       FOR j=0,N_Elements(multi)-1 DO self.pmulti[j] = multi[j]
    ENDIF ELSE self.pmulti = d_multi
    IF N_Elements(xomargin) NE 0 THEN self.xomargin = xomargin ELSE self.xomargin = d_xomargin
    IF N_Elements(yomargin) NE 0 THEN self.yomargin = yomargin ELSE self.yomargin = d_yomargin
    self.adjustsize = d_adjustsize
    self.createParent = createParent
    self.destroyObjects = Keyword_Set(wdestroyobjects)
    self.im_transparent = d_im_transparent
    self.im_density = d_im_density
    self.im_options = d_im_options
    self.im_raster = d_im_raster
    self.im_resize = d_im_resize
    self.im_width = d_im_width
    self.msysvar = Ptr_New(/Allocate_Heap)
    self.pdf_unix_convert_cmd = d_pdf_unix_convert_cmd
    self.pdf_path = d_pdf_path
    self.ps_decomposed = d_ps_decomposed
    self.ps_delete = d_ps_delete
    self.ps_encapsulated = d_ps_encapsulated
    self.ps_metric = d_ps_metric
    self.ps_charsize = d_ps_charsize
    self.ps_font = d_ps_font
    self.ps_quiet = d_ps_quiet
    self.ps_scale_factor = d_ps_scale_factor
    self.ps_tt_font = d_ps_tt_font
    self.waspect = waspect

    IF createParent THEN BEGIN
    
        ; Get it running.
        WIDGET_CONTROL, /MANAGED, self.tlb
        XManager, 'cgwindow', self.tlb, /No_Block, $
            Event_Handler='cgCmdWindow_Dispatch_Events', $
            Cleanup = 'cgCmdWindow_Cleanup', $
            Group_Leader=group_leader
            
        ; Store the self object in UVALUE of TLB. This is so it can be cleaned up properly.
        Widget_Control, self.tlb, SET_UValue=self
        
        ; Store the object reference on the external linked list.
        self -> StoreObjectReference
        
        ; Execute the commands
        self -> ExecuteCommands
        
    ENDIF
    
    ; Restore the current graphics window, if you can.
    IF (currentWindow GE 0) && WindowAvailable(currentWindow) THEN BEGIN
       WSet, currentWindow
    ENDIF ELSE WSet, -1
    RETURN, 1

END ;----------------------------------------------------------------------------------------------------------------

;+
; This program is called when the draw widget is realized. The purpose is to find
; and set the window index number in the program, store the object reference on
; an external linked list so it can found again later, and to execute the loaded
; commands.
; 
; :Params:
;     drawID: in, required
;         The widget identifier of the draw widget.
;-
PRO cgCmdWindowDrawRealize, drawID

    Widget_Control, drawID, Get_UVALUE=theObject, Get_VALUE=wid
    theObject -> SetProperty, WID=wid
    
    ; Store the object reference on the external linked list.
    theObject -> StoreObjectReference
    
    ; Execute the commands
    theObject -> ExecuteCommands
    
END



;+
; This program is called when the draw widget is destroyed. The purpose
; is to clean up the object.
; 
; :Params:
;     drawID: in, required
;         The widget identifier of the draw widget.
;-
PRO cgCmdWindowKillNotify, drawID

    Widget_Control, drawID, GET_UVALUE=theObject
    Obj_Destroy, theObject
    
END

;
;
;+
; The cleanup method for the cgCmdWindow object.
;-
PRO cgCmdWindow::Cleanup

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
    Ptr_Free, self.msysvar
    Ptr_Free, self.storage
    
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
    IF ((theList -> Get_Count()) EQ 0) THEN Obj_Destroy, theList
    
    ; If your widget ID is valid, destroy the widget program.
    IF self.createParent && Widget_Info(self.tlb, /VALID_ID) THEN Widget_Control, self.tlb, /Destroy
    
END ;----------------------------------------------------------------------------------------------------------------


;+
; This procedure builds a command structure and saves the structure
; to the main IDL level. Its purpose is to allow the user to work
; with restored commands independently of the window interface.
;
; :Params:
;     structname: in, optional, type=string, default="cmd"
;         The name of the command structure to be saved at the 
;         main IDL level.
;
; :Keywords:
;     quiet: in, optional, type=boolean
;         If set, the message indicated where the variable was saved
;         and what its name is is not printed in the IDL command window.
;-
PRO cgWindow_Command::CreateCommandStruct, structName, Quiet=quiet

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
        4: cmdStruct = Create_Struct(cmdStruct, 'p1', *self.p1, 'p2', *self.p2, 'p3', *self.p3, 'p4', *self.p4)
    ENDCASE
    IF Ptr_Valid(self.keywords) THEN BEGIN
        cmdStruct = Create_Struct(cmdStruct, 'keywords', *self.keywords)
    ENDIF

    ; Copy the variable to the MAIN level
    (Scope_VarFetch(structName, /Enter, Level=1)) = Temporary(cmdStruct)
    IF NOT Keyword_Set(quiet) THEN $
        PRINT, 'Created command struct variable ', structName, ' in IDL $MAIN level.'

END ;----------------------------------------------------------------------------------------------------------------


;+
; This procedure makes and returns a copy of a command object.
;-
FUNCTION cgWindow_Command::Copy

    IF Ptr_Valid(self.keywords) THEN BEGIN
    
        CASE self.nparams OF
           0: copyObj = Obj_New('cgWindow_Command', COMMAND=self.command, $
                KEYWORDS=*self.keywords, TYPE=self.type, $
                DESTROYOBECTS=self.destroyObjects)
           1: copyObj = Obj_New('cgWindow_Command', COMMAND=self.command, P1=*self.p1, $
                KEYWORDS=*self.keywords, TYPE=self.type, $
                DESTROYOBECTS=self.destroyObjects)
           2: copyObj = Obj_New('cgWindow_Command', COMMAND=self.command, P1=*self.p1, P2=*self.p2, $
                KEYWORDS=*self.keywords, TYPE=self.type, $
                DESTROYOBECTS=self.destroyObjects)
           3: copyObj = Obj_New('cgWindow_Command', COMMAND=self.command, P1=*self.p1, P2=*self.p2, $
                P3=*self.p3, KEYWORDS=*self.keywords, TYPE=self.type, $
                DESTROYOBECTS=self.destroyObjects)
           4: copyObj = Obj_New('cgWindow_Command', COMMAND=self.command, P1=*self.p1, P2=*self.p2, $
                P3=*self.p3, P4=*self.p4, KEYWORDS=*self.keywords, TYPE=self.type, $
                DESTROYOBECTS=self.destroyObjects)
        ENDCASE
        
    ENDIF ELSE BEGIN
    
        CASE self.nparams OF
           0: copyObj = Obj_New('cgWindow_Command', COMMAND=self.command, $
                TYPE=self.type, DESTROYOBECTS=self.destroyObjects)
           1: copyObj = Obj_New('cgWindow_Command', COMMAND=self.command, P1=*self.p1, $
                 TYPE=self.type, DESTROYOBECTS=self.destroyObjects)
           2: copyObj = Obj_New('cgWindow_Command', COMMAND=self.command, P1=*self.p1, P2=*self.p2, $
                 TYPE=self.type, DESTROYOBECTS=self.destroyObjects)
           3: copyObj = Obj_New('cgWindow_Command', COMMAND=self.command, P1=*self.p1, p2=*self.p2, P3=*self.p3, $
                 TYPE=self.type, DESTROYOBECTS=self.destroyObjects)
           4: copyObj = Obj_New('cgWindow_Command', COMMAND=self.command, P1=*self.p1, p2=*self.p2, P3=*self.p3, P4=*self.p4, $
                 TYPE=self.type, DESTROYOBECTS=self.destroyObjects)
        ENDCASE
    
    ENDELSE
    
    RETURN, copyObj
    
END ;----------------------------------------------------------------------------------------------------------------


;+
; This procedure returns the keyword in a command object, if there
; are any keywords.
;
; :Keywords:
;     has_keywords: out, optional, type=boolean
;         If set to 1, the function returned keywords. If set to 0,
;         there were no keywords in the command to return.
;-
FUNCTION cgWindow_Command::Get_Keywords, HAS_KEYWORDS=has_keywords
    IF Ptr_Valid(self.keywords) THEN BEGIN
        has_keywords = 1
        RETURN, *self.keywords 
    ENDIF ELSE BEGIN
        has_keywords = 0
        RETURN, Ptr_New()
    ENDELSE
END ;----------------------------------------------------------------------------------------------------------------


PRO cgWindow_Command::Draw, SUCCESS=success, KEYWORDS=keywords

    Compile_Opt idl2
    
    ; Can't really catch CALL_PROCEDURE errors, so I'm not sure what this is doing here.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
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
    
    ; Do we have alternative keywords?
    IF Ptr_Valid(self.altps_Keywords) THEN BEGIN
       keywords = self -> EvaluateKeywords(*self.keywords, SUCCESS=success)
       IF success EQ 0 THEN Ptr_Free, self.keywords
    ENDIF ELSE BEGIN
        IF Ptr_Valid(self.keywords) THEN BEGIN
            keywords = *self.keywords
            keywords = self -> ReplaceEscapeSequences(keywords)
        ENDIF
    ENDELSE
    
    ; Do we have alternative parameters?
    IF Ptr_Valid(self.altps_params) THEN BEGIN
    
        altTags = Tag_Names(*self.altps_params)
        IF (Where(altTags EQ 'P1') NE -1) THEN BEGIN
            IF (!D.Name EQ 'PS') $
                THEN p1 = (*self.altps_params).p1 $
                ELSE IF Ptr_Valid(self.p1) THEN p1 = *self.p1
        ENDIF ELSE IF Ptr_Valid(self.p1) THEN p1 = *self.p1
        IF (Where(altTags EQ 'P2') NE -1) THEN BEGIN
            IF (!D.Name EQ 'PS') $
                THEN p2 = (*self.altps_params).p2 $
                ELSE IF Ptr_Valid(self.p2) THEN p2 = *self.p2
        ENDIF ELSE IF Ptr_Valid(self.p2) THEN p2 = *self.p2
        IF (Where(altTags EQ 'P3') NE -1) THEN BEGIN
            IF (!D.Name EQ 'PS') $
                THEN p3 = (*self.altps_params).p3 $
                ELSE IF Ptr_Valid(self.p3) THEN p3 = *self.p3
        ENDIF ELSE IF Ptr_Valid(self.p3) THEN p3 = *self.p3
        IF (Where(altTags EQ 'P4') NE -1) THEN BEGIN
            IF (!D.Name EQ 'PS') $
                THEN p4 = (*self.altps_params).p4 $
                ELSE IF Ptr_Valid(self.p4) THEN p4 = *self.p4
        ENDIF ELSE IF Ptr_Valid(self.p4) THEN p4 = *self.p4
    
    ENDIF ELSE BEGIN
        IF Ptr_Valid(self.p1) THEN p1 = *self.p1
        IF Ptr_Valid(self.p2) THEN p2 = *self.p2
        IF Ptr_Valid(self.p3) THEN p3 = *self.p3
        IF Ptr_Valid(self.p4) THEN p4 = *self.p4
    ENDELSE
    
    ; Remove escape sequences, if you need to.
    IF Size(p1, /TNAME) EQ 'STRING' THEN p1 = self -> ReplaceEscapeSequences(p1)
    IF Size(p2, /TNAME) EQ 'STRING' THEN p2 = self -> ReplaceEscapeSequences(p2)
    IF Size(p3, /TNAME) EQ 'STRING' THEN p3 = self -> ReplaceEscapeSequences(p3)
    IF Size(p4, /TNAME) EQ 'STRING' THEN p4 = self -> ReplaceEscapeSequences(p4)

    ; What kind of command is this?
    CASE self.type OF 
    
        ; Command calls a procedure.
        0: BEGIN
        
             IF Ptr_Valid(self.keywords) THEN BEGIN
                 CASE self.nparams OF
                     0: Call_Procedure, self.command, _Extra=keywords
                     1: Call_Procedure, self.command, p1, _Extra=keywords
                     2: Call_Procedure, self.command, p1, p2, _Extra=keywords
                     3: Call_Procedure, self.command, p1, p2, p3, _Extra=keywords
                     4: Call_Procedure, self.command, p1, p2, p3, p4, _Extra=keywords
                 ENDCASE
             ENDIF ELSE BEGIN
                 CASE self.nparams OF
                     0: Call_Procedure, self.command
                     1: Call_Procedure, self.command, p1
                     2: Call_Procedure, self.command, p1, p2
                     3: Call_Procedure, self.command, p1, p2, p3
                     4: Call_Procedure, self.command, p1, p2, p3, p4
                 ENDCASE
             ENDELSE
             
             END
             
        ; Command calls a method.
        1: BEGIN

             IF Ptr_Valid(self.keywords) THEN BEGIN
                 CASE self.nparams OF
                     0: Message, 'Call_Method requires at least one positional parameter.'
                     1: Call_Method, self.command, p1, _Extra=keywords
                     2: Call_Method, self.command, p1, p2, _Extra=keywords
                     3: Call_Method, self.command, p1, p2, p3, _Extra=keywords
                     4: Call_Method, self.command, p1, p2, p3, p4, _Extra=keywords
                 ENDCASE
             ENDIF ELSE BEGIN
                 CASE self.nparams OF
                     0: Message, 'Call_Method requires at least one positional parameter.'
                     1: Call_Method, self.command, p1
                     2: Call_Method, self.command, p1, p2
                     3: Call_Method, self.command, p1, p2, p3
                     4: Call_Method, self.command, p1, p2, p3, p4
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



;+
; This method replaces stored keyword values with alternative keyword values, if they
; are available. The return variable is a list of the keywords with the
; alternative values substituted for the stored values.
;     
; :Params:
;    keywords: in, required, type=structure
;       The list of input keywords.
;       
; :Keywords:
;     success: out, optional, type=boolean
;         If the command executed successfully, return a 1. Otherwise
;         return a 0.
;-
FUNCTION cgWindow_Command::EvaluateKeywords, keywords, SUCCESS=success

  ; Error handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     success = 0
     void = Error_Message()
     IF N_Elements(keywords) NE 0 THEN RETURN, keywords ELSE RETURN, 0
  ENDIF
  
  Compile_Opt idl2
  
  ; Assume success.
  success = 1
  
  ; Need to have keywords.
  IF N_Elements(keywords) EQ 0 THEN $
      Message, 'A keyword structure must be passed to the method.'

  ; Get the keyword names.
  tags = Tag_Names(keywords)
  
  ; Find the keywords to be evaluated in this keyword structure.
  evalTags = Tag_Names(*self.altps_keywords)
  theseTags = Ptr_New(/Allocate_Heap)
  FOR j=0,N_Elements(evalTags)-1 DO BEGIN
     index = Where(tags EQ evalTags[j], count)
     IF (count EQ 1) THEN BEGIN
        IF N_Elements(*theseTags) NE 0 THEN *theseTags = [*theseTags, index] ELSE *theseTags = [index] 
     ENDIF ELSE IF count GT 1 THEN Message, 'Ambiguous keywords to evaluate.'
  ENDFOR
  
  ; Evaluate the keywords and, if necessary, replace the keyword value
  ; with the evaluated value. The evaluated keyword value must initially be a 
  ; STRING that can be evaluated with the EXECUTE command.
  FOR j=0,N_Elements(tags)-1 DO BEGIN
    index = Where(*theseTags EQ j, count)
    IF count EQ 0 THEN BEGIN
      IF N_Elements(mkeys) EQ 0 THEN BEGIN
        mkeys = Create_Struct(tags[j], keywords.(j)) 
      ENDIF ELSE BEGIN
        mkeys = Create_Struct(mkeys, tags[j], keywords.(j))
      ENDELSE
    ENDIF ELSE BEGIN
    
      ; If this is PostScript, use the alternative keyword value.
      tagValue = (!D.Name EQ 'PS') ? (*self.altps_keywords).(index) : keywords.(j)
      IF N_Elements(mkeys) EQ 0 THEN BEGIN
        mkeys = Create_Struct(tags[j], tagValue)
      ENDIF ELSE BEGIN
        mkeys = Create_Struct(mkeys, tags[j], tagValue)
      ENDELSE
    ENDELSE
    
  ENDFOR
  
  ; Evaluate string keywords for escape sequences (e.g, $\mu$).
  ;mkeys = cgWindow_Command -> ReplaceEscapeSequences(mkeys)
 
  Ptr_Free, theseTags
  RETURN, mkeys
END ;----------------------------------------------------------------------------------------------------------------


;+
; This method lists the current command by printing a representation
; of the command in the command log window.
;     
; :Params:
;    prefix: in, optional, type=string
;       A prefix for the printed command (e.g., an index number, etc.).
;       
;-
PRO cgWindow_Command::List, prefix

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
        4: cmdString = cmdString + ', p1, p2, p3, p4'
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

;+
; This method searches for escape sequences in text keywords and replaces
; them if necessary with the particular token from cgSymbol. To create an
; micrometer symbol, for example, you might specify an XTITLE keyword like
; this: XTITLE='Length ($\mu$M)'. The symbol name should be proceeded by
; a "$\" and ended with a "$".
; 
; :Params:
;     aString: in, required
;        Either a scalar or array of strings, or a structure of keyword parameters. 
;        If a structure, then the ReplaceEscapeSequences method is called recursively.
;-
FUNCTION cgWindow_Command::ReplaceEscapeSequences, aString

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(aString) EQ 0 THEN RETURN, "" ELSE RETURN, aString
    ENDIF
    
    ; Must have a parameter.
    IF N_Elements(aString) EQ 0 THEN Message, 'Must pass a string or structure of keywords.'
    
    ; What kind of thing is the parameter?
    type = Size(aString, /TNAME)
    
    ; If this is a structure of keyword parameters, sort though and call all string
    ; parameters recursively.
    IF type EQ "STRUCT" THEN BEGIN
    
        tags = Tag_Names(aString)
        FOR j=0,N_Elements(tags)-1 DO BEGIN
           type = Size(aString.(j), /TNAME)
           IF (type EQ 'STRING') || (type EQ 'STRUCT') || (type EQ 'POINTER') THEN BEGIN
              aString.(j) = self -> ReplaceEscapeSequences(aString.(j))
           ENDIF
        ENDFOR
        RETURN, aString
        
    ENDIF
    
    ; Is this a pointer type?
    IF type EQ 'POINTER' THEN BEGIN
    
       IF Size(*aString, /TNAME) EQ 'STRING' THEN *aString = self -> ReplaceEscapeSequences(*aString)
    
    ENDIF
    
    ; If this is a string, then do your thing.
    IF type EQ 'STRING' THEN BEGIN
    
        FOR j=0,N_Elements(aString)-1 DO BEGIN
            thisString = aString[j]
            thisString = cgCheckForSymbols(thisString)
            aString[j] = thisString
        ENDFOR
        
        RETURN, aString
        
    ENDIF

END ;----------------------------------------------------------------------------------------------------------------



;+
; The clean-up routine for the command object.
;-
PRO cgWindow_Command::Cleanup

    ; Need to destroy parameter objects?
    IF self.destroyobjects THEN BEGIN
       IF Ptr_Valid(self.p1) && Obj_Valid(*self.p1) THEN Obj_Destroy, *self.p1
       IF Ptr_Valid(self.p2) && Obj_Valid(*self.p2) THEN Obj_Destroy, *self.p2
       IF Ptr_Valid(self.p3) && Obj_Valid(*self.p3) THEN Obj_Destroy, *self.p3
       IF Ptr_Valid(self.p4) && Obj_Valid(*self.p4) THEN Obj_Destroy, *self.p4
    ENDIF
    Ptr_Free, self.p1
    Ptr_Free, self.p2
    Ptr_Free, self.p3
    Ptr_Free, self.p4
    Ptr_Free, self.keywords
    Ptr_Free, self.altps_keywords
    Ptr_Free, self.altps_params
END ;----------------------------------------------------------------------------------------------------------------


;+
; The initialization routine for the command object.
; 
; :Keywords:
;   altps_keywords: in, optional, type=structure
;       A structure of alternative keyword names and values to be used
;       when the PostScript device is the current graphics device.
;   altps_params: in, optional, type=structure
;       A structure of alternative parameters and values to be used
;       when the PostScript device is the current graphics device.
;   command: in, required, type=string
;       The command that is being stored in the command object.
;   destroyobjects: in, optional, type=boolean, default=0
;       If this keyword is set, and any of the input parameters p1-p4 is an object,
;       the object parameter will be destroyed when the window is destroyed.
;   keywords: in, optional, type=structure
;       A structure containing keyword:value pairs to be executed
;       with the command.
;   P1: in, optional, type=varies
;       The first positional parameter of the command being stored
;       in the structure.
;   P2: in, optional, type=varies
;       The second positional parameter of the command being stored
;       in the structure.
;   P3: in, optional, type=varies
;       The third positional parameter of the command being stored
;       in the structure.
;   P4: in, optional, type=varies
;       The fourth positional parameter of the command being stored
;       in the structure.
;   type: in, optional, type=integer, default=0
;       The type of command. 0 indicates a procedure. 1 indicates an object method.
;-
FUNCTION cgWindow_Command::INIT, $
    ALTPS_KEYWORDS=altps_keywords, $
    ALTPS_PARAMS=altps_params, $
    COMMAND=command, $
    DESTROYOBJECTS=destroyObjects, $
    KEYWORDS=keywords, $
    P1=p1, P2=p2, P3=p3, P4=p4, $
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
    IF N_Elements(p4) NE 0 THEN self.p4 = Ptr_New(p4)
    IF N_Elements(keywords) NE 0 THEN self.keywords = Ptr_New(keywords)
    IF N_Elements(altps_keywords) NE 0 THEN self.altps_keywords = Ptr_New(altps_keywords)
    IF N_Elements(altps_params) NE 0 THEN self.altps_params = Ptr_New(altps_params)
    self.destroyobjects = Keyword_Set(destroyObjects)
    self.type = Keyword_Set(type)
    self.nparams = (N_Elements(p1) NE 0) + (N_Elements(p2) NE 0) + (N_Elements(p3) NE 0) + (N_Elements(p4) NE 0)
    RETURN, 1
    
END ;----------------------------------------------------------------------------------------------------------------

;+
; The structure definition module for structure that is stored
; on the window manager list.
;-
PRO cgWindow_ID__Define

   struct = { cgWINDOW_ID, $
                 tlb: 0L, $
                 wid: 0L, $
                 title: "", $
                 windowObj: Obj_New() $
             }
END ;----------------------------------------------------------------------------------------------------------------

;+
; The clean-up method for the cgCmdWindow object.
;-
PRO cgCmdWindow_Cleanup, tlb
    Widget_Control, tlb, Get_UValue=self
    IF Obj_Valid(self) THEN Obj_Destroy, self
END ;----------------------------------------------------------------------------------------------------------------


;+
; The class definition module for the cgWindow_Command object.
;-
PRO cgWindow_Command__Define

   ; The definition of the command object.
   class = { cgWindow_Command, $
              command: "", $             ; The command to execute.
              p1: Ptr_New(), $           ; The first parameter.
              p2: Ptr_New(), $           ; The second parameter.
              p3: Ptr_New(), $           ; The third parameter.
              p4: Ptr_New(), $           ; The fourth parameter.
              nparams: 0, $              ; The number of parameters.
              keywords: Ptr_New(), $     ; The command keywords.
              destroyobjects: 0B, $      ; A flag to destroy parameter objects upon exit.
              altps_keywords: Ptr_New(), $ ; Structure of keywords to evaluate at run-time.
              altps_params: Ptr_New(), $  ; Structure of parameters to evaluate at run-time.
              type: 0 $                  ; =0 call_procedure =1 call_method
            }
END ;----------------------------------------------------------------------------------------------------------------

;+
; Adds a command object of class cgWINDOW_COMMAND to the command list 
; maintained by the window.
;
; :Params:
;     command: in, required, type=object
;         A command object of class cgWINDOW_COMMAND.
;         
; :Keywords:
;     index: in, optional, type=integer
;         The index number of where the command should be added in the command list.
;         The command is added to the end of the command list by default.
;-
PRO cgCmdWindow::AddCommand, command,  INDEX=index

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
; Provides a programmatic way to create a PostScript file from the window.
; Call by setting the CREATE_PS keyword with cgControl.
;
; :Params:
;     filename:  in, required, type=string
;         The name of the PostScript file to generate.
;-
PRO cgCmdWindow::AutoPostScriptFile, filename

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        
        ; Close the PostScript file.
        PS_END, /NoFix     

        ; Set the window index number back.
        IF N_Elements(currentWindow) NE 0 THEN BEGIN
            IF WindowAvailable(currentWindow) THEN WSet, currentWindow ELSE WSet, -1
        ENDIF
        
        RETURN
    ENDIF
    
    ; Make this window the current graphics windows.
    currentWindow = !D.Window
    WSet, self.wid

    IF N_Elements(filename) EQ 0 THEN filename='cgwindow.ps'

    ; Allow the user to configure the PostScript file.
    PS_Start, GUI=0, $
        FILENAME=filename, $
        DECOMPOSED=self.ps_decomposed, $
        EUROPEAN=self.ps_metric, $
        ENCAPSULATED=self.ps_encapsulated, $
        GROUP_LEADER=self.tlb, $
        SCALE_FACTOR=self.ps_scale_factor, $
        CHARSIZE=self.ps_charsize, $
        FONT=self.ps_font, $
        QUIET=self.ps_quiet, $
        TT_FONT=self.ps_tt_font
    
    ; Execute the graphics commands.
    self -> ExecuteCommands
    
    ; Clean up.
    PS_End, NOMESSAGE=self.ps_quiet

    ; Set the window index number back.
    IF WindowAvailable(currentWindow) THEN WSet, currentWindow ELSE WSet, -1

END ;----------------------------------------------------------------------------------------------------------------


;+
; Provides a programmatic way to create a raster file from the window.
; Call by setting the create_png, etc. keyword with cgControl.
;
; :Params:
;     filetype:  in, required, type=string
;         The type of raster file (e.g., PNG, JPEG, etc.).
;     filename:  in, required, type=string
;         The name of the output file.
;-
PRO cgCmdWindow::AutoRasterFile, filetype, filename

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        
        ; Close the PostScript file.
        PS_END, /NoFix     

        ; Set the window index number back.
        IF N_Elements(currentWindow) NE 0 THEN BEGIN
            IF WindowAvailable(currentWindow) THEN WSet, currentWindow ELSE WSet, -1
        ENDIF
        
        RETURN
    ENDIF
    
    ; Make this window the current graphics windows.
    currentWindow = !D.Window
    WSet, self.wid

    IF N_Elements(filetype) EQ 0 then filetype = 'PNG'
    IF N_Elements(filename) EQ 0 THEN filename = 'cgwindow.' + StrLowCase(filetype)
    IF StrUpCase(filetype) EQ 'PDF' THEN rastertype = -1 ELSE rastertype = self.im_raster
    
    ; Strip the extension off the filename.
    outname = cgRootName(filename, DIRECTORY=dirName)
    
    ; Put it back together without an extension.
    outputFilename = Filepath(ROOT_DIR=dirName, outname)
    
    ; What kind of raster file.
    CASE rasterType OF
    
        ; PDF File.
       -1: BEGIN
       
           thisname = outputFilename + '.ps'
           outname = outputFilename + '.pdf'
           PS_Start, $
                DECOMPOSED=self.ps_decomposed, $
                FILENAME=thisname, $
                GROUP_LEADER=self.tlb, $
                METRIC=self.ps_metric, $
                KEYWORDS=keywords, $ ; Returned PSConfig keywords.
                SCALE_FACTOR=self.ps_scale_factor, $
                CHARSIZE=self.ps_charsize, $
                FONT=self.ps_font, $
                QUIET=1, $
                TT_FONT=self.ps_tt_font
                           
           ; Draw the graphics.
           self -> ExecuteCommands
           
           ; Close the file and make a PDF file.
           PS_End
           cgPS2PDF, thisname, outname, DELETE_PS=self.ps_delete, /SILENT, SUCCESS=success, $
              UNIX_CONVERT_CMD=self.pdf_unix_convert_cmd, GS_PATH=self.pdf_path
           IF ~success THEN BEGIN
              Message, 'Unable to create PDF file. See cgPS2PDF documentation.'
           ENDIF ELSE BEGIN
              IF ~self.ps_quiet THEN Print, 'PDF output will be created here: ' + outname
           ENDELSE
           END
    
        ; Normal raster.
        0: BEGIN
           void = cgSnapshot(TYPE=fileType, FILENAME=outputFilename, /NODIALOG)
           Print, 'Output file is located here: ' + outputFilename 
           END
           
        ; Raster via ImageMagick.
        1: BEGIN
        
           ; Create a PostScript file first.
           thisname = outputFilename + '.ps'
           PS_Start, $
                DECOMPOSED=self.ps_decomposed, $
                FILENAME=thisname, $
                GROUP_LEADER=self.tlb, $
                METRIC=self.ps_metric, $
                KEYWORDS=keywords, $ ; Returned PSConfig keywords.
                SCALE_FACTOR=self.ps_scale_factor, $
                CHARSIZE=self.ps_charsize, $
                FONT=self.ps_font, $
                QUIET=1, $
                TT_FONT=self.ps_tt_font
                
           ; Cannot successfully convert encapsulated landscape file to raster.
           ; Limitation of ImageMagick, and specifically, GhostScript, which does
           ; the conversion.
           IF keywords.encapsulated && keywords.landscape THEN BEGIN
                Message, 'ImageMagick cannot successfully convert an encapsulated ' + $
                         'PostScript file in landscape mode to a raster file. Returning...'
           ENDIF
           
           ; Draw the graphics.
           self -> ExecuteCommands

           ; Close the file and convert to proper file type.
            CASE filetype OF
                'BMP':  PS_END, /BMP, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width, NOMESSAGE=self.ps_quiet
                'GIF':  PS_END, /GIF, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width, NOMESSAGE=self.ps_quiet
                'JPEG': PS_END, /JPEG, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width, NOMESSAGE=self.ps_quiet
                'PNG':  PS_END, /PNG,  DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width, NOMESSAGE=self.ps_quiet
                'TIFF': PS_END, /TIFF, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width, NOMESSAGE=self.ps_quiet
           ENDCASE
           ;IF ~self.ps_quiet THEN Print, 'Output file is located here: ' + outfilename
           END
    
    ENDCASE

    ; Set the window index number back.
    IF WindowAvailable(currentWindow) THEN WSet, currentWindow ELSE WSet, -1
END ;----------------------------------------------------------------------------------------------------------------


;+
; This method copies the contents of the draw widget to the current graphics window using
; the DEVICE COPY method. The DEVICE COPY command looks like this::
;
;    DEVICE, COPY=[origin[0], origin[1], extent[0], extent[1], destination[0], destination[1], self.wid]
;
; If the IMAGE keyword is used, the window contents are stored in a band-interleaved image variable
; and the window contents are not copied into the current graphics window.
;
; :Keywords:
;    destination: in, optional, type=intarr(2)
;        A two-element array specifying the device coordinates of the lower-left
;        corner of the copied region in the destination window. By default, [0,0].
;    extent: in, optional, type=intarr(2)
;       A two-element array specifying the number of columns and rows to copy.
;       If missing, the entire draw widget window is copied. By default, [!D.X_Size, !D.Y_Size].
;    image: out, optional, type=byte
;       Set this keyword to a named IDL variable that returns a copy of the draw
;       widget contents as a band interleaved (MxNx3) image. If this keyword is set
;       nothing is copied from the window.
;    origin: in, optional, type=intarr(2)
;       A two-element array specifying the device coordinates of the lower-left
;       corner of region in the draw widget window to be copied. By default, [0,0].
;-
PRO cgCmdWindow::Copy, $
   DESTINATION=dest, $
   EXTENT=extent,             $
   IMAGE=image,               $
   ORIGIN=origin

   Compile_Opt idl2
    
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
   ENDIF
    
   ; Get the window ID of the window we are copying to.
   destination = !D.Window
   
   ; Make the draw widget the current graphics window.
   WSet, self.wid

   ; Check the input parameters
   IF (N_Elements(origin) NE 2) THEN origin = [0, 0]
   IF (N_Elements(extent) NE 2) THEN extent = [!D.X_Size, !D.Y_Size]
   IF (N_Elements(dest  ) NE 2) THEN dest   = [0, 0]

   ; If we're using an output variable, capture the current window
   IF Arg_Present(image) THEN BEGIN
      image = cgSnapshot(origin[0], origin[1], extent[0], extent[1], TRUE=3)
      IF (destination NE -1) THEN WSet, destination
   ENDIF ELSE BEGIN
      IF (destination NE -1) THEN WSet, destination ELSE Message, 'No current window to copy into.'
      DEVICE, COPY=[origin[0], origin[1], extent[0], extent[1], dest[0], dest[1], self.wid]
   ENDELSE
   
   
END


;+
; This method allows you to turn draw widget events on and off and to set
; the name of an event handler procedure to accept these widget events.
; An event handler procedure must be defined to get the events processed
; correctly. 
; 
; :Keywords:
;     button_events: in, optional, type=boolean
;         Set this keyword to turn button events on for the draw widget.
;     drop_events: in, optional, type=boolean
;         Set this keyword to turn drop events on for the draw widget.
;     event_handler: in, optional, type=string
;         The name of an event handler procedure to accept events from
;         the draw widget. Write the event handler procedure with two
;         positional parameters. The first will be the event structure
;         returned from the draw widget. The second will be the cgCmdWindow
;         object reference (i.e., self) to allow you to manipulate the
;         command window.
;     keyboard_events: in, optional, type=boolean
;         Set this keyword to turn keyboard events on for the draw widget.
;     motion_events: in, optional, type=boolean
;         Set this keyword to turn motion events on for the draw widget.
;     storage: in, optional, type=any
;         Any user-defined IDL variable will be stored in the object storage location.
;         Defined here for convenience. Same as `Storage` keyword for the SetProperty method.
;     tracking_events: in, optional, type=boolean
;         Set this keyword to turn tracking events on for the draw widget.
;     wheel_events: in, optional, type=boolean
;         Set this keyword to turn wheel events on for the draw widget.
;-
PRO cgCmdWindow::DrawWidgetConfig, $
   Button_Events=button_events, $
   Drop_Events=drop_events, $
   Event_Handler=event_handler, $
   Keyboard_Events=keyboard_events, $
   Motion_Events=motion_events, $
   Storage=storage, $
   Tracking_Events=tracking_events, $
   Wheel_Events=wheel_events

   IF N_Elements(button_events) NE 0 THEN $
      Widget_Control, self.drawID, Draw_Button_Events=Keyword_Set(button_events)
   IF N_Elements(drop_events) NE 0 THEN $
      Widget_Control, self.drawID, Draw_Drop_Events=Keyword_Set(drop_events)
   IF N_Elements(event_handler) NE 0 THEN self.event_handler = event_handler
   IF N_Elements(keyboard_events) NE 0 THEN $
      Widget_Control, self.drawID, Draw_Keyboard_Events=Keyword_Set(keyboard_events)
   IF N_Elements(motion_events) NE 0 THEN $
      Widget_Control, self.drawID, Draw_Motion_Events=Keyword_Set(motion_events)
   IF N_Elements(storage) NE 0 THEN self -> SetProperty, Storage=storage
   IF N_Elements(tracking_events) NE 0 THEN $
      Widget_Control, self.drawID, Draw_Tracking_Events=Keyword_Set(tracking_events)
   IF N_Elements(wheel_events) NE 0 THEN $
      Widget_Control, self.drawID, Draw_Wheel_Events=Keyword_Set(wheel_events)
   
END



;+
; This event handler processes draw widget events and sends the events
; to a user-defined event handler, if such and event handler exists.
; The event handler must be defined to accept two positional parameters.
; The first is the event structure returned by the draw widget, and the
; second is the cgCmdWindow object reference.
; 
; :Params:
;     event: in, required, type=structure
;         An event structure.
;-
PRO cgCmdWindow::DrawWidgetEvents, event

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Make the draw widget the current graphics window.
    WSet, self.wid

    ; Pass the event on to the event handler, if there is one.
    IF self.event_handler NE "" THEN BEGIN
       Call_Procedure, self.event_handler, event
    ENDIF ELSE Message, 'No event handler has been specified for draw widget events from cgCmdWindow.'
    
END



;+
; Sends the window commands to a PostScript file.
;
; :Params:
;     event: in, required, type=structure
;         An event structure.
;-
PRO cgCmdWindow::CreatePostScriptFile, event

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        
        ; Close the PostScript file.
        PS_END, /NoFix     

        ; Set the window index number back.
        IF N_Elements(currentWindow) NE 0 THEN BEGIN
            IF WindowAvailable(currentWindow) THEN WSet, currentWindow ELSE WSet, -1
        ENDIF
        
        RETURN
    ENDIF
    
    ; Make this window the current graphics windows
    currentWindow = !D.Window
    WSet, self.wid
    
    ; Construct a file name, if you have one.
    ext = self.ps_encapsulated ? '.eps' : '.ps'
    IF self.lastWriteFile NE "" THEN BEGIN
        filename = Filepath(ROOT_DIR=self.lastWriteDir, self.lastWriteFile + ext)
    ENDIF ELSE BEGIN
         CD, CURRENT=thisDir
         filename = Filepath(ROOT_DIR=thisDir, 'cgwindow' + ext)
    ENDELSE

    ; Allow the user to configure the PostScript file.
    PS_Start, /GUI, $
        CANCEL=cancelled, $
        CHARSIZE=self.ps_charsize, $
        DECOMPOSED=self.ps_decomposed, $
        EUROPEAN=self.ps_metric, $
        ENCAPSULATED=self.ps_encapsulated, $
        FILENAME=filename, $
        FONT=self.ps_font, $
        GROUP_LEADER=self.tlb, $
        KEYWORDS=keywords, $
        SCALE_FACTOR=self.ps_scale_factor, $
        QUIET=self.ps_quiet, $
        TT_FONT=self.ps_tt_font
    IF cancelled THEN RETURN
    
    ; Save the name of the last output file.
    self.lastWriteDir = File_DirName(keywords.filename)
    self.lastWriteFile = cgRootName(keywords.filename)
    
    ; Execute the graphics commands.
    self -> ExecuteCommands
    
    ; Clean up.
    PS_End
    
    ; Set the window index number back.
    IF WindowAvailable(currentWindow) THEN WSet, currentWindow ELSE WSet, -1

END ;----------------------------------------------------------------------------------------------------------------

;+
; Deletes a command object from the command list maintained by the window.
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
PRO cgCmdWindow::DeleteCommand, cmdIndex, ALL=all

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
; All widget events come here and are dispatched to the proper object method.
;
; :Params:
;     event: in, required, type=structure
;         An event structure.
;-
PRO cgCmdWindow_Dispatch_Events, event

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
    eventType = Widget_Info(event.id, /UNAME)
    
    ; Dispatch the events to the proper method.
    CASE eventType OF   
        'DRAW_WIDGET': self -> DrawWidgetEvents, event
        'IMAGEMAGICK_BMP': self -> SaveAsRaster, event
        'IMAGEMAGICK_GIF': self -> SaveAsRaster, event
        'IMAGEMAGICK_JPEG': self -> SaveAsRaster, event
        'IMAGEMAGICK_PNG': self -> SaveAsRaster, event
        'IMAGEMAGICK_TIFF': self -> SaveAsRaster, event
        'POSTSCRIPT': self -> CreatePostscriptFile, event
        'PDF': self -> SaveAsRaster, event
        'RASTER_BMP': self -> SaveAsRaster, event
        'RASTER_GIF': self -> SaveAsRaster, event
        'RASTER_JPEG': self -> SaveAsRaster, event
        'RASTER_PNG': self -> SaveAsRaster, event
        'RASTER_TIFF': self -> SaveAsRaster, event
        'RESTORECOMMANDS': self -> RestoreCommands
        'SAVECOMMANDS': self -> SaveCommands
        'TLB_RESIZE': self -> Resize, event.x, event.y
        'QUIT': Obj_Destroy, self
        ELSE: Message, 'Unknown type of event: ' + eventtype + '. Cannot dispatch properly.'
    ENDCASE
    
END ;----------------------------------------------------------------------------------------------------------------


;+
; This method executes the commands on the command list.
;-
PRO cgCmdWindow::ExecuteCommands

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
    IF Total(self.xomargin) NE 0 THEN !X.OMargin = self.xomargin
    IF Total(self.yomargin) NE 0 THEN !Y.OMargin = self.yomargin

    ; How many commands are there?
    n_cmds = self.cmds -> Get_Count()
    
    ; Execute the commands.
    FOR j=0,n_cmds-1 DO BEGIN
        thisCmdObj = self.cmds -> Get_Item(j, /DEREFERENCE)
        
        ; Execute the command. 
        thisCmdObj -> Draw, SUCCESS=success
        
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
    
    ; Save the current data coordinate system.
    self -> SaveDataCoords
    
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
; This method retrieves the value of a keyword from a particular command,
; if it can.
;     
; :Params:
;     keyword: in, required, type=string
;        The name of the keyword you would like to retrieve from the command.
;     cmdindex: in, required, type=integer
;        The index number of the command you wish to retrieve the keyword from.
;        
; :Keywords:
;     success: out, optional, type=boolean
;         If the program can successfully get the keyword, this variable will
;         contain the value 1. Otherwise, it will contain the value 0.
;-
FUNCTION cgCmdWindow::GetCommandKeyword, keyword, cmdIndex, SUCCESS=success

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


;+
; This method retrieves properties from the object.
; 
; :Keywords:
;     adjustsize: out, optional, type=boolean
;         Set this keyword to adjust default character size to the display window size.
;     background: out, optional, type=string
;         The background color of the window. Only use if the ERASEIT property is also set.
;     commands: out, optional
;         A list of the commands stored in the window.
;     delay: out, optional, type=float
;         Set this keyword to the amount of "delay" you want between commands in the command list.
;     dimensions: out, optional, type=intarr(2)
;         Set this keyword to a two-element array giving the xsize and ysize
;         of the draw widget.
;     eraseit: out, optional, type=boolean
;         If this property is set, the cgWindow erases with the background color before
;         displaying the commands in the window's command list.
;     im_density: out, optional, type=integer, default=300
;         Set this keyword to the sampling density when ImageMagick creates raster image
;         file from PostScript outout.
;     im_options: out, optional, type=string, default=""
;         Set this keyword to any ImageMagick options you would like to pass along to the
;         ImageMagick convert command when creating raster image files from PostScript output.
;     im_resize: out, optional, type=integer, default=25
;         Set this keyword to percentage that the raster image file created my ImageMagick
;         from PostScript output should be resized.
;     im_raster: out, optional, type=boolean, default=1
;         Set this keyword to zero to create raster files using the create_png etc. keywords
;         directly, instead of via ImageMagick.
;     im_transparent: out, optional, type=boolean, default=0
;         Set this keyword to allow ImageMagick to create transparent backgrounds when it
;         makes raster image files from PostScript output.
;     multi: out, optional, type=Intarr(5)
;         Set this keyword to the !P.MULTI setting you want to use for the window.
;         !P.MULTI is set to this setting before command execution, and set back to
;         it's default value when the commands are finished executing.
;     noexecutecommands: out, optional, type=boolean, default=0
;         Set this keyword to 1 to prevent the window from executing the commands and to
;         0 if you want the window to execute the commands.
;     palette: out, optional, type=byte
;         Use this keyword to pass in an N-by-3 (or 3-by-N) byte array containing the
;         R, G, and B vectors of a color table. It is probably easier to use cgLoadCT or
;         XCOLORS to load color tables for the window, but this is provided as another option.
;     pdf_path: out, optional, type=string
;         Set this keyword to the name of the path to the Ghostscript command for converting PS to PDF.
;     pdf_unix_convert_cmd: out, optional, type=string
;         Set this keyword to the name of an alternative UNIX command to convert PostScript to PDF.
;     ps_charsize: out, optional, type=float
;         The PostScript character size.
;     ps_decomposed: out, optional, type=boolean, default=0
;         Set this keyword to zero to set the PostScript color mode to indexed color and to
;         one to set the PostScript color mode to decomposed color.
;     ps_delete: out, optional, type=boolean, default=1
;         Set this keyword to zero if you want to keep the PostScript output ImageMagick creates
;         when making raster file output.
;     ps_encapsulated: out, optional, type=boolean, default=0
;         Set this keyword to configure PSCONFIG to produce encapsulated PostScript output by default.
;     ps_font: out, optional, type=integer
;         Set this keyword to the type of font you want to use in PostScript output. It sets the 
;         FONT keyword on the PSConfig command. Normally, 0 (hardware fonts) or 1 (true-type fonts).
;     ps_metric: out, optional, type=boolean, default=0
;         Set this keyword to configure PSCONFIG to use metric values and A4 page size in its interface.
;     ps_quiet: out, optional, type=boolean, default=0
;         Set this keyword to set the QUIET keyword on PS_Start.
;     ps_scale_factor: out, optional, type=float
;         Set his keyword to the PostScript scale factor you wish to use in creating PostScript output.
;     ps_tt_font: out, optional, type=string
;         Set this keyword to the name of a true-type font to use in creating PostScript output.
;     storage: out, optional
;         An IDL variable that was defined and stored here by the program's user .
;     tlb: out, optional, type=long
;         The widget identifier of the top-level base widget.
;     wid: out, optional, type=integer
;         The window index number of the draw widget.
;     xomargin: out, optional, type=intarr(2)
;         Sets the !X.OMargin system variable when multiple plots are displayed in the window.
;     yomargin: out, optional, type=intarr(2)
;         Sets the !Y.OMargin system variable when multiple plots are displayed in the window.
;-
PRO cgCmdWindow::GetProperty, $
    ADJUSTSIZE=adjustsize, $
    BACKGROUND=background, $
    COMMANDS=commands, $
    DELAY=delay, $
    DIMENSIONS=dimensions, $
    ERASEIT=eraseit, $
    IM_DENSITY=im_density, $                      ; Sets the density parameter on ImageMagick convert command.
    IM_RESIZE=im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
    IM_OPTIONS=im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
    IM_RASTER=im_raster, $                        ; Sets whether to generate raster files via ImageMagick
    IM_TRANSPARENT=im_transparent, $  ; Sets the "alpha" keyword on ImageMagick convert command.
    NOEXECUTECOMMANDS=noExecuteCommands, $ ; Set if you want commands to execute commands.
    MULTI=multi, $
    PALETTE=palette, $
    PDF_PATH=pdf_path, $                          ; The path to the Ghostscript conversion command.
    PDF_UNIX_CONVERT_CMD=pdf_unix_convert_cmd, $  ; Command to convert PS to PDF.
    PS_CHARSIZE=ps_charsize, $                    ; Select the character size for PostScript output.
    PS_DECOMPOSED=ps_decomposed, $
    PS_DELETE=ps_delete, $
    PS_ENCAPSULATED=ps_encapsulated, $
    PS_FONT=ps_font, $                            ; Select the font for PostScript output.
    PS_METRIC=ps_metric, $
    PS_SCALE_FACTOR=ps_scale_factor, $            ; Select the scale factor for PostScript output.
    PS_QUIET=ps_quiet, $
    PS_TT_FONT=ps_tt_font, $                      ; Select the true-type font to use for PostScript output.
    TLB=tlb, $
    STORAGE=storage, $
    WID=wid, $
    XOMARGIN=xomargin, $
    YOMARGIN=yomargin, $
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
    IF Arg_Present(adjustsize) THEN adjustsize = self.adjustsize
    IF Arg_Present(background) THEN background = *self.background
    IF Arg_Present(palette) THEN BEGIN
        len = N_Elements(*self.r)
        palette = BytArr(len,3)
        palette[*,0] = *self.r
        palette[*,1] = *self.g
        palette[*,2] = *self.b
    ENDIF
    IF Arg_Present(commands) THEN commands = self.cmds
    IF Arg_Present(dimensions) THEN BEGIN
        thisWindow = !D.Window
        WSet, self.wid
        dimensions = [!D.X_Size, !D.Y_Size]
        IF thisWindow GE 0 THEN WSet, thisWindow
    ENDIF
    IF Arg_Present(delay) THEN delay = self.delay
    IF Arg_Present(eraseit) THEN eraseit = self.eraseit
    IF Arg_Present(multi) THEN multi = self.pmulti
    IF Arg_Present(tlb) THEN tlb = self.tlb
    IF Arg_Present(storage) THEN IF Ptr_Valid(self.storage) THEN storage = *self.storage
    IF Arg_Present(wid) THEN wid = self.wid
    IF Arg_Present(xomargin) THEN xomargin = self.xomargin
    IF Arg_Present(yomargin) THEN yomargin = self.yomargin
    
     ; PDF properties.
     pdf_unix_convert_cmd = self.pdf_unix_convert_cmd
     pdf_path = self.pdf_path
    
     ; PostScript properties.
     ps_charsize = self.ps_charsize
     ps_decomposed = self.ps_decomposed
     ps_delete = self.ps_delete
     ps_encapsulated = self.ps_encapsulated
     ps_metric = self.ps_metric
     ps_font = self.ps_font
     ps_quiet = self.ps_quiet
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
; :Private:
;     This method invalidates a widget ID. It is used for restored
;     visualizations, so that the current window doesn't get inadvertenly
;     destroyed by a widget identifier from an old program.
;-
PRO cgCmdWindow::InvalidateWidgetID
    self.tlb = -1
END ;----------------------------------------------------------------------------------------------------------------


;+
; This method lists the command indicated by the command index. In the
; absence of the command index, all commands are listed.
;
; :Params:
;      cmdindex: in, optional, type=index
;         The index number of the command you wish to list. If not supplied,
;         all the commands in the window are listed.
; :Keywords:
;       createcommandstruct: out, optional, type=structure
;          Set to a named variable to return a command structure of this command.
;-
PRO cgCmdWindow::ListCommand, cmdIndex, CREATECOMMANDSTRUCT=createCommandStruct

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
; This method loads color table vectors into the program.
; The XCOLORS_DATA keyword is required to get color vector
; information from XCOLORS.
;  
; :Params:
;     r: in, optional, type=bytarr(256)
;        The red color vector.
;     g: in, optional, type=bytarr(256)
;        The green color vector.
;     b: in, optional, type=bytarr(256)
;        The blue color vector.
;        
; :Keywords:
;      xcolors_data: in, optional, type=structure
;         The information XColors sends to an object when colors are changed
;         in XColors.
; 
;-
PRO cgCmdWindow::LoadColors, r, g, b, XCOLORS_DATA=colorData

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
; This method creates PostScript, PDF, BMP, GIF, JPEG, PNG, and TIFF file output
; from the pixmap window contents. The method assumes ImageMagick and Ghostscript
; are installed correctly.
;
; :Params:
;    filename: in, optional, type=string, default='idl.ps'
;        The name of the output file. The type of file is determined from the
;        file name extension.
;-
PRO cgCmdWindow::Output, filename

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Need a filename?
    IF N_Elements(filename) EQ 0 THEN filename = 'idl.ps'
    
    ; The type of file is determined by the filename extension.
    rootname = cgRootName(filename, DIRECTORY=dir, EXTENSION=ext)
    CASE StrUpCase(ext) OF
       'PS':   self -> AutoPostScriptFile, filename
       'EPS':  self -> AutoPostScriptFile, filename
       'PDF':  self -> AutoRasterFile, 'PDF', filename
       'BMP':  self -> AutoRasterFile, 'BMP', filename
       'GIF':  self -> AutoRasterFile, 'GIF', filename
       'JPG':  self -> AutoRasterFile, 'JPEG', filename
       'JPEG': self -> AutoRasterFile, 'JPEG', filename
       'PNG':  self -> AutoRasterFile, 'PNG', filename
       'TIF':  self -> AutoRasterFile, 'TIFF', filename
       'TIFF': self -> AutoRasterFile, 'TIFF', filename
       ELSE: Message, 'Unknown file type: ' + StrUpCase(ext) + '.'
    ENDCASE
    
 END



;+
; Packages the command up into a command object that can be added to the window
; or used to replace commands that are already in the window.
; 
; :Returns:
;    The command, packaged as a cgWindow_Command object, is returned.
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
;    p4: in, optional, type=any
;       The fourth positional parameter appropriate for the graphics command.
;       
; :Keywords:
;    addcmd: in, optional, type=boolean, default=0
;       Set this keyword to add the packaged command to the command list.
;    altps_Keywords: in, optional, type=string
;       A structure containing alternative keyword names (as tags) and values for
;       those keywords to be used when the current device is the PostScript device.
;       See http://www.idlcoyote.com/cg_tips/kwexpressions.php and the examples
;       below for details on how to use this keyword.
;    altps_Params: in, optional, type=IntArr(3)
;       A structure containing alternative parameter values to be used when 
;       the current device is the PostScript device. Structure names are restricted
;       to the names "P1", "P2", "P3" and "P4" to correspond to the equivalent positional
;       parameter. See http://www.idlcoyote.com/cg_tips/kwexpressions.php and the 
;       examples below for details on how to use this keyword.
;    cmdindex: in, optional, type=integer
;       The command list index. This is used when adding or replacing a command. If the
;       command index is undefined when replacing a command, all the commands in the command
;       list are replaced with the command that is being added. If the command index is 
;       undefined when adding a command, the command is added to the end of the command list.
;    execute: in, optional, type=boolean, default=0
;       Set this keyword is you want to execute the commands in the command list as soon as
;       the current command is added to the list. Otherwise, no command execution takes place.
;    method: in, optional, type=boolean, default=0
;       Set this keyword if the command is an object method call rather than a 
;       procedure call. If this keyword is set, the first positional parameter, p1,
;       must be present and must be a valid object reference.
;    multi: in, optional, type=intarr(5)
;       A replacement value to assign to the self.pmulti value. It is
;       a way to reset multiple command plotting to single command plotting
;       for a new single command. Like calling: object -> SetPropery, MULTI=multi. Used only if
;       `ReplaceCmd` is set.
;    replacecmd: in, optional, type=boolean, default=0
;       Set this keyword to replace a graphics command with this packaged command on the command list
;       If `CmdIndex` is undefined, *all* the commands on the command list are replaced. 
;    _extra: in, optional
;       Any extra keywords are collected by keyword inheritance for the command structure.
;- 
FUNCTION cgCmdWindow::PackageCommand, command, p1, p2, p3, p4, $
   AddCmd=addCmd, $                 ; Add the command to the command list
   AltPS_Keywords=altps_Keywords, $ ; A structure of PostScript alternative keywords and values.
   AltPS_Params=altps_Params, $     ; A structure of PostScript alternative parameters and values. 
   CmdIndex=cmdIndex, $             ; The location of the command in the command list.
   DestroyObjects=destroyobjects, $ ; Set this keyword to destroy parameter objects on exit.
   Execute=execute, $               ; Execute the commands in the window, if this keyword set.
   Method=method, $                 ; A flag that indicates a method call.
   Multi=multi, $                   ; If you are replacing all commands, you may want to change the way they are displayed.
   ReplaceCmd=replaceCmd, $         ; Set this keyword to replace one or all commands in the command list.
    _Extra=extra                    ; Extra keywords to the command.
    
    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN, Obj_New()
    ENDIF

   newCommand = Obj_New('cgWindow_Command', COMMAND=command, $
      P1=p1, P2=p2, P3=p3, P4=p4, KEYWORDS=extra, AltPS_Keywords=altps_Keywords, $
      AltPS_Params=altps_Params, TYPE=Keyword_Set(method), DESTROYOBECTS=Keyword_Set(destroyObjects))
                        
    ; Replace command? If the cmdIndex is undefined, ALL commands are replaced.
    IF Keyword_Set(replaceCmd) THEN self -> ReplaceCommand, newCommand, cmdIndex, Multi=multi
      
    ; Need to add a command?
    IF Keyword_Set(addCmd) THEN self -> AddCommand, newCommand, INDEX=cmdIndex
   
    ; Execute the commands?
    IF Keyword_Set(execute) THEN self -> ExecuteCommands
    
    ; Return the packaged command.
    RETURN, newCommand
    
END


;+
; This method replaces a command in the command list. If cmdIndex is missing,
; all the commands in the command list are replaced by this command.
;
; :Params:
;    command: in, required, type=object
;       The new command object. 
;    cmdindex: in, optional, type=integer
;       The index number of the "command" to replace. If absent, then
;       all the current commands are replaces with this new command.
; :Keywords:
;     multi: in, optional, type=intarr(5)
;        A replacement value to assign to the self.pmulti value. It is
;        a way to reset multiple command plotting to single command plotting
;        for a new single command. Like calling: object -> SetPropery, MULTI=multi.
;-
PRO cgCmdWindow::ReplaceCommand, command, cmdIndex, MULTI=multi

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
        self.cmds -> Replace_Item, command, cmdIndex
        Obj_Destroy, oldcmd
    ENDELSE
    
END ;----------------------------------------------------------------------------------------------------------------


;+
; This method resizes the graphics window and executes the commands again.
; 
; :Params:
;     x: in, required, type=integer
;        The new X size of the draw widget in device coordinates.
;     y: in, required, type=integer
;        The new Y size of the draw widget in device coordinates.
;-
PRO cgCmdWindow::Resize, x, y

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Make sure we have both sizes.
    IF N_Params() NE 2 THEN Message, 'Resize method requires both an X and Y size be specified.'
    
    ; Do you need to maintain the aspect ratio of the window?
    IF self.waspect NE 0 THEN BEGIN
         IF self.waspect GE 1 THEN BEGIN
             ysize = y
             xsize = Round(y / self.waspect)
         ENDIF ELSE BEGIN
             xsize = x
             ysize = Round(x * self.waspect)         
         ENDELSE
    ENDIF ELSE BEGIN
        xsize = x
        ysize = y
    ENDELSE

    Widget_Control, self.drawID, DRAW_XSIZE=xsize, DRAW_YSIZE=ysize
    self -> ExecuteCommands
    
END ;----------------------------------------------------------------------------------------------------------------


;+
; This event handler method saves the graphics window as a raster image file.
; PDF files also pass through here.
; 
; :Params:
;     event: in, required, type=structure
;        The event structure.
;-
PRO cgCmdWindow::SaveAsRaster, event

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        
        ; Close the PostScript file.
        PS_END, /NoFix     

        ; Set the window index number back.
        IF N_Elements(currentWindow) NE 0 THEN BEGIN
            IF WindowAvailable(currentWindow) THEN WSet, currentWindow ELSE WSet, -1
        ENDIF
        
        RETURN
    ENDIF

    ; Only going in here for down event.
    IF event.select NE 1 THEN RETURN
    
    buttonValue = Widget_Info(event.id, /UNAME)
    
    ; Is this a PDF file or a raster file?
    IF buttonValue EQ 'PDF' THEN BEGIN
       rasterType = -1
       filetype = 'PDF'
    ENDIF ELSE BEGIN
    
        ; Determine if this is normal raster (0) or ImageMagick raster (1).
        IF StrMid(buttonValue, 0, 6) EQ 'RASTER' THEN BEGIN
            fileType = StrMid(buttonValue, 7)
            rasterType = 0 
        ENDIF ELSE BEGIN
            filetype = StrMid(buttonValue, 12)
            rasterType = 1
        ENDELSE
        
    ENDELSE
    
    ; Make this window the current graphics windows.
    currentWindow = !D.Window
    WSet, self.wid
    
    ; Construct a file name, if you have one.
    CASE filetype OF
       'BMP':  ext = '.bmp'
       'GIF':  ext = '.gif'
       'JPEG': ext = '.jpg'
       'PDF':  ext = '.pdf'
       'PNG':  ext = '.png'
       'TIFF': ext = '.tif'
    ENDCASE
    IF self.lastWriteFile NE "" THEN BEGIN
        filename = Filepath(ROOT_DIR=self.lastWriteDir, self.lastWriteFile + ext)
    ENDIF ELSE BEGIN
         CD, CURRENT=thisDir
         filename = Filepath(ROOT_DIR=thisDir, 'cgwindow' + ext)
    ENDELSE
    
    ; Get a filename from the user.
    CASE filetype OF
       'BMP':  filename = cgPickfile(FILE=filename, /WRITE, TITLE='Select an Output File...')
       'GIF':  filename = cgPickfile(FILE=filename, /WRITE, TITLE='Select an Output File...')
       'JPEG': filename = cgPickfile(FILE=filename, /WRITE, TITLE='Select an Output File...')
       'PDF':  filename = cgPickfile(FILE=filename, /WRITE, TITLE='Select an Output File...')
       'PNG':  filename = cgPickfile(FILE=filename, /WRITE, TITLE='Select an Output File...')
       'TIFF': filename = cgPickfile(FILE=filename, /WRITE, TITLE='Select an Output File...')
    ENDCASE
    IF filename EQ "" THEN RETURN
    
    ; Parset the name.
    root_name = cgRootName(filename, DIRECTORY=dirName)
    outname = Filepath(ROOT_DIR=dirname, root_name)
    
    ; Save this name.
    self.lastWriteFile = root_name
    self.lastWriteDir = dirName
    
    ; What kind of raster file.
    CASE rasterType OF
    
        ; PDF File.
       -1: BEGIN
       
           thisname = outname + '.ps'
           outname = outname + '.pdf'
           PS_Start, $
                DECOMPOSED=self.ps_decomposed, $
                FILENAME=thisname, $
                GROUP_LEADER=self.tlb, $
                METRIC=self.ps_metric, $
                KEYWORDS=keywords, $ ; Returned PSConfig keywords.
                SCALE_FACTOR=self.ps_scale_factor, $
                CHARSIZE=self.ps_charsize, $
                FONT=self.ps_font, $
                QUIET=self.ps_quiet, $
                TT_FONT=self.ps_tt_font
                           
           ; Draw the graphics.
           self -> ExecuteCommands
           
           ; Close the file and make a PDF file.
           PS_End
           cgPS2PDF, thisname, outname, DELETE_PS=self.ps_delete, /SILENT, SUCCESS=success, $
              UNIX_CONVERT_CMD=self.pdf_unix_convert_cmd, GS_PATH=self.pdf_path
           IF ~success THEN BEGIN
              Message, 'Unable to create PDF file. See cgPS2PDF documentation.'
           ENDIF ELSE BEGIN
              IF ~self.ps_quiet THEN Print, 'PDF output will be created here: ' + outname
           ENDELSE
           END
    
        ; Normal raster.
        0: BEGIN
           void = cgSnapshot(TYPE=fileType, FILENAME=outname, /NODIALOG)
           IF ~self.ps_quiet THEN Print, 'Output file located here: ' + outname 
           END
           
        ; Raster via ImageMagick.
        1: BEGIN
        
           ; Create a PostScript file first.
           thisname = outname + '.ps'
           PS_Start, $
                DECOMPOSED=self.ps_decomposed, $
                FILENAME=thisname, $
                GROUP_LEADER=self.tlb, $
                METRIC=self.ps_metric, $
                KEYWORDS=keywords, $ ; Returned PSConfig keywords.
                SCALE_FACTOR=self.ps_scale_factor, $
                CHARSIZE=self.ps_charsize, $
                FONT=self.ps_font, $
                QUIET=1, $
                TT_FONT=self.ps_tt_font
                
           ; Cannot successfully convert encapsulated landscape file to raster.
           ; Limitation of ImageMagick, and specifically, GhostScript, which does
           ; the conversion.
           IF keywords.encapsulated && keywords.landscape THEN BEGIN
                Message, 'ImageMagick cannot successfully convert an encapsulated ' + $
                         'PostScript file in landscape mode to a raster file. Returning...'
           ENDIF
           
           ; Draw the graphics.
           self -> ExecuteCommands
           
           ; Close the file and convert to proper file type.
           CASE filetype OF
                'BMP':  PS_END, /BMP, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width
                'GIF':  PS_END, /GIF, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width
                'JPEG': PS_END, /JPEG, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width
                'PNG':  PS_END, /PNG,  DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width
                'TIFF': PS_END, /TIFF, DELETE_PS=self.ps_delete, $
                            ALLOW_TRANSPARENT=self.im_transparent, $
                            DENSITY=self.im_density, RESIZE=self.im_resize, $
                            IM_OPTIONS=self.im_options, OUTFILENAME=outfilename, $
                            WIDTH=self.im_width
           ENDCASE
           IF ~self.ps_quiet THEN Print, 'Output will be created here: ' + outfilename
           END
    
    ENDCASE
    
    ; Set the window index number back.
    IF WindowAvailable(currentWindow) THEN WSet, currentWindow ELSE WSet, -1
END ;----------------------------------------------------------------------------------------------------------------


;+
; This method restores the commands from a specified file and loads them
; into the window.
;
; :Params:
;    filename: in, required, type=string
;       The name of the file containing the cgWindow commands that were previously saved.
;-
PRO cgCmdWindow::RestoreCommands, filename

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
        filename = cgPickfile(Title='Restore Coyote Graphics Commands...', $
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
        
        
    ; The widget ID of the restored object must be invalidated,
    ; or the current object (with that widget ID) will be destroyed.
    cg_window -> InvalidateWidgetID
    Obj_Destroy, cg_window
    
    ; Execute the new commands.
    self -> ExecuteCommands

END ;----------------------------------------------------------------------------------------------------------------


;+
; This method restores the current data coordinate system by restoring the system variable
; state from the last command execution.
;-
PRO cgCmdWindow::RestoreDataCoords

   !MAP = *self.msysvar
   !P = self.psysvar
   !X = self.xsysvar
   !Y = self.ysysvar
   !Z = self.zsysvar
   
END


;+
; This method saves the commands from a cgWindow into a form that can be
; e-mailed to colleagues or restored to a cgWindow at some later time.
;
; :Params:
;    filename: in, optional, type=string, default='commands.cgs'
;       The name of the file where the cgWindow commands are to be saved.
;-
PRO cgCmdWindow::SaveCommands, filename

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
       filename = cgPickfile(FILE='commands.cgs', $
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
; This method saves the current data coordinate system by saving all the plotting and mapping
; system variables after the commands have been executed in the program.
;-
PRO cgCmdWindow::SaveDataCoords

   *self.msysvar = !Map
   self.psysvar = !P
   self.xsysvar = !X
   self.ysysvar = !Y
   self.zsysvar = !Z
   
END


;+
; This method sets properties of the window object. 
; 
; :Keywords:
;     adjustsize: in, optional, type=boolean
;        Set this keyword to adjust default character size to the display window size.
;     background: in, optional, type=string
;        The background color of the window. Only use if the ERASEIT property is also set.
;     delay: in, optional, type=float
;        Set this keyword to the amount of "delay" you want between commands in the command list.
;     dimensions: in, optional, type=intarr(2)
;        Set this keyword to a two-element array giving the xsize and ysize
;        of the draw widget.
;     eraseit: in, optional, type=boolean
;        If this property is set, the cgWindow erases with the background color before
;        displaying the commands in the window's command list.
;     im_density: in, optional, type=integer, default=300
;        Set this keyword to the sampling density when ImageMagick creates raster image
;        file from PostScript outout.
;     im_options: in, optional, type=string, default=""
;        Set this keyword to any ImageMagick options you would like to pass along to the
;        ImageMagick convert command when creating raster image files from PostScript output.
;     im_resize: in, optional, type=integer, default=25
;        Set this keyword to percentage that the raster image file created my ImageMagick
;        from PostScript output should be resized.
;     im_raster: in, optional, type=boolean, default=1
;        Set this keyword to zero to create raster files using the create_png etc. keywords
;        directly, instead of via ImageMagick.
;     im_transparent: in, optional, type=boolean, default=0
;        Set this keyword to allow ImageMagick to create transparent backgrounds when it
;        makes raster image files from PostScript output.
;     multi: in, optional, type=Intarr(5)
;        Set this keyword to the !P.MULTI setting you want to use for the window.
;        !P.MULTI is set to this setting before command execution, and set back to
;        it's default value when the commands are finished executing.
;     noexecutecommands: in, optional, type=boolean, default=0
;        Set this keyword to 1 to prevent the window from executing the commands and to
;        0 if you want the window to execute the commands.
;     palette: in, optional, type=byte
;        Use this keyword to pass in an N-by-3 (or 3-by-N) byte array containing the
;        R, G, and B vectors of a color table. It is probably easier to use cgLoadCT or
;        XCOLORS to load color tables for the window, but this is provided as another option.
;     pdf_path: out, optional, type=string
;        Set this keyword to the name of the path to the Ghostscript command for converting PS to PDF.
;     pdf_unix_convert_cmd: out, optional, type=string
;        Set this keyword to the name of an alternative UNIX command to convert PostScript to PDF.
;     ps_charsize: in, optional, type=float
;        The PostScript character size.
;     ps_decomposed: in, optional, type=boolean, default=0
;        Set this keyword to zero to set the PostScript color mode to indexed color and to
;        one to set the PostScript color mode to decomposed color.
;     ps_delete: in, optional, type=boolean, default=1
;        Set this keyword to zero if you want to keep the PostScript output ImageMagick creates
;        when making raster file output.
;     ps_encapsulated: in, optional, type=boolean, default=0
;        Set this keyword to configure PSCONFIG to produce encapsulated PostScript output by default.
;     ps_font: in, optional, type=integer
;        Set this keyword to the type of font you want to use in PostScript output. It sets the 
;        FONT keyword on the PSConfig command. Normally, 0 (hardware fonts) or 1 (true-type fonts).
;     ps_metric: in, optional, type=boolean, default=0
;        Set this keyword to configure PSCONFIG to use metric values and A4 page size in its interface.
;     ps_quiet: in, optional, type=boolean, default=0
;        Set this keyword to set the QUIET keyword on PS_Start.
;     ps_scale_factor: in, optional, type=float
;        Set his keyword to the PostScript scale factor you wish to use in creating PostScript output.
;     ps_tt_font: in, optional, type=string
;        Set this keyword to the name of a true-type font to use in creating PostScript output.
;     storage: in, optional
;        A storage location for any kind of IDL variable. This functions like a user value in
;        a widget program. 
;     update: in, optional, type=boolean, default=1
;        Set this keyword to zero if you do not want the updates to be done immediately
;        after the properties are changed.
;     wid: in, optional, type=integer
;        The draw window index number. Not to be used by the user. Used by the draw widget
;        Notify_Realize callback routine when the draw widget is realized.
;     xomargin: in, optional, type=intarr(2)
;        Sets the !X.OMargin system variable when multiple plots are displayed in the window.
;     yomargin: in, optional, type=intarr(2)
;        Sets the !Y.OMargin system variable when multiple plots are displayed in the window.
;          
;-
PRO cgCmdWindow::SetProperty, $
    ADJUSTSIZE=adjustsize, $       ; Adjust the default charsize to match display size.
    BACKGROUND=background, $       ; The background color of the window.
    DELAY=delay, $                 ; The delay between command execution.
    DIMENSIONS=dimensions, $       ; Set the dimensions of the draw widget.
    ERASEIT=eraseit, $             ; Set the erase flag for the window
    IM_DENSITY=im_density, $                      ; Sets the density parameter on ImageMagick convert command.
    IM_OPTIONS=im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
    IM_RASTER=im_raster, $                        ; Sets whether to use ImageMagick to create raster files.
    IM_RESIZE=im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
    IM_TRANSPARENT=im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
    IM_WIDTH = im_width, $                        ; Sets the final width of the raster files create with ImageMagick.
    PDF_UNIX_CONVERT_CMD=pdf_unix_convert_cmd, $  ; Command to convert PS to PDF.
    PDF_PATH=pdf_path, $                          ; The path to the Ghostscript conversion command.
    PALETTE=palette, $             ; Change window color table vectors.
    PS_CHARSIZE=ps_charsize, $                    ; Select the character size for PostScript output.
    PS_DECOMPOSED=ps_decomposed, $                ; Sets the PostScript color mode.
    PS_DELETE=ps_delete, $                        ; Delete the PostScript file when making IM raster files.
    PS_ENCAPSULATED=ps_encapsulated, $            ; Select encapusulated PostScript output.
    PS_FONT=ps_font, $                            ; Select the font for PostScript output.
    PS_METRIC=ps_metric, $                        ; Select metric measurements in PostScript output.
    PS_QUIET=ps_quiet, $                          ; Select the QUIET keyword for PS_Start.
    PS_SCALE_FACTOR=ps_scale_factor, $            ; Select the scale factor for PostScript output.
    PS_TT_FONT=ps_tt_font, $                      ; Select the true-type font to use for PostScript output.
    NOEXECUTECOMMANDS=noExecuteCommands, $ ; Set if you don't want the window to execute commands.
    MULTI=multi, $                 ; Change the !P.MULTI setting for the window.
    STORAGE=storage, $             ; A pointer location for storing any IDL variable, like a UVALUE.
    XOMARGIN=xomargin, $           ; Change the !X.OMargin setting for the window.
    YOMARGIN=yomargin, $           ; Change the !Y.OMargin setting for the window.
    UPDATE=update, $               ; Set if you want the commands to be updated after property change.
    WID=wid, $                     ; The window index number. Used in draw widget notify realize event.
    _EXTRA=extra
    
    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    IF N_Elements(adjustsize) NE 0 THEN self.adjustsize = Keyword_Set(adjustsize)

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
    IF N_Elements(dimensions) NE 0 THEN BEGIN
       IF N_Elements(dimensions) EQ 1 THEN dimensions = [dimensions, dimensions]
       Widget_Control, self.drawID, DRAW_XSIZE=dimensions[0], DRAW_YSIZE=dimensions[1]
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
    IF N_Elements(im_width) NE 0 then self.im_width = im_width
    IF N_Elements(pdf_unix_convert_cmd) NE 0 THEN self.pdf_unix_convert_cmd = pdf_unix_convert_cmd
    IF N_Elements(pdf_path) NE 0 THEN self.pdf_path = pdf_path
    IF N_Elements(ps_decomposed) NE 0 THEN self.ps_decomposed = ps_decomposed
    IF N_Elements(ps_delete) NE 0 THEN self.ps_delete = ps_delete
    IF N_Elements(ps_metric) NE 0 THEN self.ps_metric = ps_metric
    IF N_Elements(ps_encapsulated) NE 0 THEN self.ps_encapsulated = ps_encapsulated
    IF N_Elements(ps_charsize) NE 0 THEN self.ps_charsize = ps_charsize
    IF N_Elements(ps_font) NE 0 THEN self.ps_font = ps_font
    IF N_Elements(ps_quiet) NE 0 THEN self.ps_quiet = ps_quiet
    IF N_Elements(ps_scale_factor) NE 0 THEN self.ps_scale_factor = ps_scale_factor
    IF N_Elements(ps_tt_font) NE 0 THEN self.ps_tt_font = ps_tt_font
    IF N_Elements(storage) NE 0 THEN BEGIN
       IF Ptr_Valid(self.storage) THEN *self.storage = storage ELSE self.storage = Ptr_New(storage)
    ENDIF
    
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
    
    ; Update the draw widget index number. Should only be called from the draw widget notify realize procedure.
    IF N_Elements(wid) NE 0 THEN self.wid = wid
END ;----------------------------------------------------------------------------------------------------------------


;+
; This method saves the window object reference in a linked-list system variable.
;-
PRO cgCmdWindow::StoreObjectReference

    ; Each instance of cgWindow will store evidence of its
    ; existance in a linked list.
    DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
    IF ~exists THEN BEGIN
        fsc_window_list = Obj_New("LinkedList")
        DefSysV, '!FSC_WINDOW_LIST', fsc_window_list 
        fsc_window_list -> Add, {cgWINDOW_ID, self.tlb, self.wid, self.title, self}
    ENDIF ELSE BEGIN
        IF Obj_Valid(!FSC_WINDOW_LIST) THEN BEGIN
            !FSC_WINDOW_LIST -> Add, {cgWINDOW_ID, self.tlb, self.wid, self.title, self}
        ENDIF ELSE BEGIN
            !FSC_WINDOW_LIST = Obj_New('LinkedList')
            !FSC_WINDOW_LIST-> Add, {cgWINDOW_ID, self.tlb, self.wid, self.title, self}
        ENDELSE
    ENDELSE
    
END



;+
; The definition module for the cgCmdWindow object
; 
; :Params:
;    class: out, optional, type=struct
;        The object class structure definition. Occasionally useful.
;-
PRO cgCmdWindow__Define, class

    class = { cgCMDWINDOW, $
              tlb: 0L, $                    ; The identifier of the top-level base widget.
              cmds: Obj_New(), $            ; A linkedlist object containing the graphics commands.
              wid: 0L, $                    ; The window index number of the graphics window.
              drawid: 0L, $                 ; The identifier of the draw widget.
              destroyObjects: 0B, $         ; If set, destroy object parameters upon exit.
              event_handler: "", $          ; The name of an event handler that will receive draw widget events.
              storage: Ptr_New(), $         ; Holder for user information, like UVALUE.
              title: "", $                  ; The "title" of the object when it is stored.
              createParent: 0B, $           ; Flag that indicates whether this object created its own parent widget.
              lastWriteFile: "", $          ; The name of the last file written.
              lastWriteDir: "", $           ; The name of the last directory written to.
              
              ; cgWindow parameters
              adjustsize: 0B, $             ; Adjust character size to display window.
              background: Ptr_New(), $      ; The background color.
              delay: 0.0, $                 ; The command delay.
              eraseit: 0B, $                ; Do we need to erase the display.
              noExecuteCommands: 0B, $      ; Set to stop command execution (e.g.,for loading commands)
              pmulti: LonArr(5), $          ; Identical to !P.Multi.
              xomargin: FltArr(2), $        ; Identical to !X.OMargin
              yomargin: FltArr(2), $        ; Identical to !Y.OMargin
              waspect: 0.0, $               ; The aspect ratio of the window.
              r: Ptr_New(), $               ; The red color table vector.
              g: Ptr_New(), $               ; The green color table vector.
              b: Ptr_New(), $               ; The blue color table vector.
              
              ; Data coordinate fields.
              psysvar: !P, $                ; The !P system variable.
              xsysvar: !X, $                ; The !X system variable.
              ysysvar: !Y, $                ; The !Y system variable.
              zsysvar: !Z, $                ; The !Z system variable.
              msysvar: Ptr_New(), $         ; The !MAP system variable.
              
              ; PostScript options.
              ps_decomposed: 0L, $          ; Sets the PostScript color mode.
              ps_delete: 0L, $              ; Delete the PS file when making IM image file.
              ps_encapsulated: 0L, $        ; Encapsulated PostScript
              ps_metric: 0L, $              ; Metric measurements in PostScript.
              ps_charsize: 0.0, $           ; The character size to use for PostScript output.
              ps_font: 0, $                 ; The PostScript font to use.
              ps_quiet: 0, $                ; Select the QUIET keyword for PS_Start.
              ps_scale_factor: 0, $         ; The PostScript scale factor.
              ps_tt_font: "", $             ; The name of a true-type font to use for PostScript output.
              
              ; PDF options.
              pdf_unix_convert_cmd: "", $   ; The name of an alternative UNIX command to convert PS to PDF.
              pdf_path: "", $               ; The name of the path to a Ghostscript conversion command.

              ; ImageMagick output parameters.
              im_transparent: 0B, $         ; Sets the "alpha" keyword on ImageMagick convert command.
              im_density: 0L, $             ; Sets the density parameter on ImageMagick convert command.
              im_resize: 0L, $              ; Sets the resize parameter on ImageMagick convert command.
              im_options: "", $             ; Sets extra ImageMagick options on the ImageMagick convert command.
              im_raster: 0L, $              ; Create raster files via ImageMagick
              im_width: 0L $                ; Sets the width of the final raster output with ImageMagick
            }
            
END ;----------------------------------------------------------------------------------------------------------------




