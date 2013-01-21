; docformat = 'rst'
;
; NAME:
;   cgPixmapWindow
;
; PURPOSE:
;   This is the "object" behind cgPixmap. It is a subclassed cgCmdWindow object
;   and behaves almost identically, except that normally the draw widget at the
;   heart of the window is invisible. That is, it is a pixmap.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2012, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; This is the "object" behind cgPixmap. It is a subclassed cgCmdWindow object
; and behaves almost identically, except that normally the draw widget at the
; heart of the window is invisible. That is, it is a pixmap.
; 
; Default properties of the object can be controled with cgWindow_SetDefs and cgControl,
; as with cgWindow. Be sure to delete the pixmap object when you are done with it, by
; using (for example) cgDelete.
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
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;     
; :History:
;     Change History::
;        Created from cgCmdWindow, 7 February 2012. DWF.
;        Problem with the ASPECT keyword, which should have been named WASPECT. 7 Oct 2012. DWF.
;        Added WDestroyObjects keyword to destroy objects parameters, if needed. 11 November 2012. DWF.
;        Confusion trying to change WBackground keyword to Background. Changed it back. Identical to ASPECT
;           problem described above. 18 Jan 2013. DWF.
;-

;+
; This method initializes the pixmap object.
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
;    command: in, required, type=string
;       The graphics procedure command to be executed. This parameter
;       must be a string and the the command must be a procedure. Examples
;       are 'Surface', 'Contour', 'Plot', 'cgPlot', cgContour, etc.
;    eraseit: in, optional, type=boolean, default=0
;       Set this keyword to cause the window to be erased before graphics commands 
;       are drawn. This may need to be set, for example, to display images.
;    group_leader: in, optional
;       The identifier of a widget to serve as a group leader for this program.
;       If the group leader is destroyed, this program is also destroyed. Used
;       when calling this program from another widget program.
;    method: in, optional, type=boolean, default=0
;       Set this keyword if the command is an object method call rather than a 
;       procedure call. If this keyword is set, the first positional parameter, p1,
;       must be present and must be a valid object reference.
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
;    waspect: in, optional, type=float, default=normal
;       Set this keyword to the aspect ratio you would like the window to have.
;       The aspect ratio is calculated as (ysize/xsize). Must be a float value.
;       If this keyword is set, the window will maintain this aspect ratio,
;       even when it is resized.
;    wbackground: in, optional, type=varies, default=!P.Background
;       The background color of the window. Specifying a background color 
;       automatically sets the WErase keyword.
;    wdestroyobjects: in, optional, type=boolean, default=0
;       If this keyword is set, and any of the input parameters p1-p4 is an object,
;       the object parameter will be destroyed when the window is destroyed.
;    wxsize: in, optional, type=integer, default=640
;       The x size in device coordinates of the graphics window.
;    wysize: in, optional, type=integer, default=512
;       The y size in device coordinates of the the graphics window.
;    _extra: in, optional
;       The "extra" keywords for the command that is being added to the window.
;-
FUNCTION cgPixmapWindow::INIT, parent, $
   AltPS_Keywords=altps_Keywords, $ ; A structure of PostScript alternative keywords and values.
   AltPS_Params=altps_Params, $     ; A structure of PostScript alternative parameters and values. 
   Command=command, $               ; The graphics "command" to execute.
   EraseIt = eraseit, $             ; Set this keyword to erase the display before executing the command.
   Group_Leader = group_leader, $   ; The group leader of the cgWindow program.
   Method=method, $                 ; If set, will use CALL_METHOD instead of CALL_PROCEDURE to execute command.
   Multi = multi, $                 ; Set this in the same way !P.Multi is used.
   OXMargin = oxmargin, $           ; Set the !X.OMargin. A two element array.
   OYMargin = oymargin, $           ; Set the !Y.OMargin. A two element array
   P1=p1, $                         ; The first postitional parameter in a graphics command loaded for display.
   P2=p2, $                         ; The sedond postitional parameter in a graphics command loaded for display.
   P3=p3, $                         ; The third postitional parameter in a graphics command loaded for display.
   P4=p4, $                         ; The fourth postitional parameter in a graphics command loaded for display. 
   ReplaceCmd=replacecmd, $         ; Replace the current command and execute in the current window.
   Storage=storage, $               ; A storage pointer location. Used like a user value in a widget.
   Visible=visible, $               ; Set this keyword to make the pixmap visible.
   WAspect = waspect, $             ; Set the window aspect ratio to this value.
   WBackground = wbackground, $     ; The background color. Set to !P.Background by default.
   WDestroyObjects=wdestroyobjects, $ ; Set this keyword to destroy object parameters upon exit.
   WXSize = xsize, $                ; The X size of the cgWindow graphics window in pixels. By default: 400.
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
    visible = Keyword_Set(visible)
    currentWindow = !D.Window
    
    ; Create the widgets for the program.
    IF N_Elements(parent) NE 0 THEN BEGIN
       self.tlb = parent
       Widget_Control, self.tlb, Map=0
    ENDIF ELSE BEGIN    
       self.tlb = Widget_Base(Map=0, /TLB_SIZE_EVENTS, UNAME='TLB_RESIZE')
    ENDELSE
   
    ; Get the global defaults.
    cgWindow_GetDefs, $
       AdjustSize = d_adjustsize, $                      ; Adjust charsize to window size.
       Aspect = d_aspect, $                              ; The aspect ratio of the window.
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
       
       ; PDF properties.
       PDF_Unix_Convert_Cmd = d_pdf_unix_convert_cmd, $  ; Command to convert PS to PDF.
       PDF_Path = d_pdf_path, $                          ; The path to the Ghostscript conversion command.
   
       ; ImageMagick Properties.
       IM_Transparent = d_im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
       IM_Density = d_im_density, $                      ; Sets the density parameter on ImageMagick convert command.
       IM_Raster = d_im_raster, $                        ; Create raster files via ImageMagick.
       IM_Resize = d_im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
       IM_Options = d_im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
       IM_Width = d_im_width, $                          ; Set the width of ImageMagick output.
       
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
    SetDefaultValue, xsize, d_xsize 
    SetDefaultValue, ysize, d_ysize 
    SetDefaultValue, xpos, d_xpos 
    SetDefaultValue, ypos, d_ypos 
    SetDefaultValue, button_events, 0, /Boolean
    SetDefaultValue, drop_events, 0, /Boolean
    SetDefaultValue, keyboard_events, 0, /Boolean
    SetDefaultValue, motion_events, 0, /Boolean
    SetDefaultValue, tracking_events, 0, /Boolean
    SetDefaultValue, waspect, d_aspect 
    SetDefaultValue, wheel_events, 0, /Boolean
    IF N_Elements(storage) NE 0 THEN self.storage = Ptr_New(storage)
    IF N_Elements(wbackground) EQ 0 THEN BEGIN
        wbackground = d_background
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
    notify_realize = 'cgCmdWindowDrawRealize'
    kill_notify = 'cgCmdWindowKillNotify'
    self.drawID = Widget_Draw(self.tlb, XSIZE=xsize, YSize=ysize, RETAIN=retain, $
       UVALUE=self, UNAME='DRAW_WIDGET', Kill_Notify=kill_notify, NOTIFY_REALIZE=notify_realize) 
    !P.Background = Temporary(tempBackground)
    !P.Color = Temporary(tempColor)
    
    ; Load object properties.
    self.background = Ptr_New(wbackground)
    IF N_Elements(cmdDelay) NE 0 THEN self.delay = cmdDelay ELSE self.delay = d_delay
    self.eraseIt = eraseIt
    IF N_Elements(multi) NE 0 THEN BEGIN
       FOR j=0,N_Elements(multi)-1 DO self.pmulti[j] = multi[j]
    ENDIF ELSE self.pmulti = d_multi
    IF N_Elements(wxomargin) NE 0 THEN self.xomargin = xomargin ELSE self.xomargin = d_xomargin
    IF N_Elements(wyomargin) NE 0 THEN self.yomargin = yomargin ELSE self.yomargin = d_yomargin
    self.adjustsize = d_adjustsize
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
    self.visible = visible
    self.waspect = waspect

    ; Restore the current graphics window, if you can.
    IF (currentWindow GE 0) && WindowAvailable(currentWindow) THEN BEGIN
       WSet, currentWindow
    ENDIF ELSE WSet, -1
    
    ; Make the pixmap visible, if needed.
    IF visible THEN Widget_Control, self.tlb, MAP=1
    Widget_Control, self.tlb, /Realize, Set_UValue=self
    XManager, 'cgwindow', self.tlb, /No_Block, $
            Event_Handler='cgCmdWindow_Dispatch_Events', $
            Cleanup = 'cgCmdWindow_Cleanup', $
            Group_Leader=group_leader
 
    RETURN, 1
END
  

;+
; This method allows you to set the properties of the object. Most properties are
; passed along to the superclass method.
;
; :Keywords:
;     visible: in, optional, type=boolean
;        Set this keyword to make the pixmap visible.
;     _extra: in, optional
;        Any keywords appropriate for the SetProperty method of the superclass object.
;-
PRO cgPixmapWindow::SetProperty, VISIBLE=visible, _EXTRA=extra

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    IF N_Elements(visible) NE 0 THEN Widget_Control, self.tlb, MAP=Keyword_Set(visible)
    
    IF N_Elements(extra) NE 0 THEN self -> cgCmdWindow::SetProperty, _STRICT_EXTRA=extra
    
END


;+
; The definition module for the cgPixmapWindow object
; 
; :Params:
;    class: out, optional, type=struct
;        The object class structure definition. Occasionally useful.
;-
PRO cgPixmapWindow__Define, class

   class = {cgPixmapWindow, $
            INHERITS cgCmdWindow, $
            visible: 0L }             ; Set this to make the pixmap visible.
            
END