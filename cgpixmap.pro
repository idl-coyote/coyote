; docformat = 'rst'
;
; NAME:
;   cgPixmap
;
; PURPOSE:
;   Creates an invisible graphics window for IDL traditional commands (Plot, Contour, 
;   Surface, etc. or for Coyote Graphics routines, cgPlot, cgContour, cgSurf, etc.). 
;   In addition, the window contents can be saved as PostScript files or as raster image 
;   files. If ImageMagick is installed on your machine, the raster image files can be 
;   created in very high quality from PostScript files. This window is identical to
;   cgWindow, except that it doesn't appear on the display, unless you want it to.
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
;  Creates an invisible graphics window for IDL traditional commands (Plot, Contour, 
;  Surface, etc. or for Coyote Graphics routines, cgPlot, cgContour, cgSurf, etc.). 
;  In addition, the window contents can be saved as PostScript files or as raster image 
;  files. If ImageMagick is installed on your machine, the raster image files can be 
;  created in very high quality from PostScript files. This window is identical to
;  cgWindow, except that it doesn't appear on the display unless you want it to.
;
;  The program is designed to work with any IDL traditional graphics routine
;  that is a procedure and includes no more than three positional parameters.
;  Any number of keywords can be used to specify properties of the graphical
;  output. Any number of graphics commands can be "added" the the cgWindow.
;  Simply use the `AddCmd` keyword to add commands.
;  
;  If your program does not load its own color tables, the color tables in
;  effect when cgWindow is first called are used to display the graphics
;  commands.
;    
;  To create PostScript output from within cgWindow, your graphics program
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
;  Much better looking raster files can be created from the cgWindow contents,
;  if the raster files are created by converting PostScript files to the raster 
;  file. If the ImageMagick "convert" command can be found on your machine, you
;  will have the option to create raster files using this method. I *highly*
;  recommend doing so, as fonts and other plot annotation will be of much higher
;  quality using this method.
;   
;  cgWindow has been designed to work with other Coyote Graphics routines: `cgPlot`,
;  `cgContour`, `cgSurf`, and so on, although I expect it to work with any IDL
;  traditional graphics routine, if the routine is well written.
;        
;  Default properties of the object can be controled with cgWindow_SetDefs and cgControl,
;  as with cgWindow. Be sure to delete the pixmap object when you are done with it, by
;  using (for example) cgDelete.
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
;       Set this keyword to add an additional graphics command to an cgWindow.
;       The command is added to the last created cgWindow, unless the WinID
;       keyword is used to select another cgWindow. Adding a command causes
;       all the commands in the window to be immediately executed. If this is
;       not behavior you desire, use the LOADCMD keyword instead. If CMDINDEX
;       is used to select a command index, the new command is added before
;       the command currently occuping that index in the command list.
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
;    cmddelay: in, optional, type=float
;       Ignored. Used only for compatibility with cgWindow.
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
;    group_leader: in, optional
;       The identifier of a widget to serve as a group leader for this program.
;       If the group leader is destroyed, this program is also destroyed. Used
;       when calling this program from another widget program.
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
;    method: in, optional, type=boolean, default=0
;       Set this keyword if the command is an object method call rather than a 
;       procedure call. If this keyword is set, the first positional parameter, p1,
;       must be present and must be a valid object reference.
;    replacecmd: in, optional, type=boolean, default=0
;       Set this keyword to replace a graphics command from an cgWindow.
;       If CmdIndex is undefined, *all* commands in the window are replaced. Use 
;       WinID to identify the cgWindow you are interested in. If WinID is 
;       undefined, the last cgWindow created is used for the replacement.
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
;    woxmargin: in, optional, type=float
;       A two-element array indicating the left and right X outside margins for the
;       graphical display. Used only when doing multiple plots with `WMulti`.
;    woymargin: in, optional, type=float
;       A two-element array indicating the bottom and top Y outside margins for the
;       graphical display. Used only when doing multiple plots with `WMulti`.
;    wxpos: in, optional, type=integer, default=5
;       Ignored. Used only for compatibility with cgWindow.
;    wypos: in, optional, type=integer, default=5
;       Ignored. Used only for compatibility with cgWindow.
;    wxsize: in, optional, type=integer, default=640
;       The x size in device coordinates of the graphics window.
;    wysize: in, optional, type=integer, default=5
;       The y size in device coordinates of the the graphics window.
;    wtitle: in, optional, type=string, default='Resizeable Graphics Window'
;       Ignored. Used only for compatibility with cgWindow.
;          
; :Examples:
;    Test code::
;       data = cgDemoData(17)
;       pixmap = cgPixmap()
;       cgPlot, data, COLOR='red', /Window
;       cgPlot, data, PSYM=2, /Overplot, COLOR='dodger blue', /AddCmd
;       cgPlot, cgDemoData(17), color='olive', linestyle = 2, /Overplot, /AddCmd
;       pixmap = cgPixmap(/ListCmd)
;       pixmap = cgPixmap('cgPlot', data, COLOR='purple', /ReplaceCMD, CMDINDEX=0)
;       pixmap -> Output, 'myplot.png'
;       
;    Example using different keyword parameters for the display and PostScript output::
;    
;       pixmap = cgPixmap('cgPlot', cgDemoData(1), $
;             THICK=1.0, XTITLE='Distance (' + cgGreek('mu') + 'm)', $
;             ALTPS_KEYWORDS={THICK:4.0, XTITLE:'Distance (' + cgGreek('mu', /PS) + 'm)'})
;           
;    Example using different positional parameters::
;    
;       pixmap = cgPixmap('cgText', 0.20, 0.85, /Normal, 'Line of Text', ALIGNMENT=0.0, $
;             ALTPS_KEYWORDS={ALIGNMENT:1.0}, ALTPS_PARAMS={P1:0.88})
;             
;    Example copying pixmap contents to a normal IDL graphics window::
;    
;       Window
;       pixmap -> Copy
;           
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;     
; :History:
;     Change History::
;        Written, 7 February 2012, based on cgWindow, by David W. Fanning.
;        Added WDESTROYOBJECTS keyword. 11 Nov 2012. DWF.
;-
FUNCTION cgPixmap, $
   command, $                       ; The graphics "command" to execute.
   p1, p2, p3, p4, $                ; The four allowed positional parameters.
   AddCmd=addcmd, $                 ; Set this keyword to add a command to the interface and immediate execute commands.
   AltPS_Keywords=altps_Keywords, $ ; A structure of PostScript alternative keywords and values.
   AltPS_Params=altps_Params, $     ; A structure of PostScript alternative parameters and values. Fields 
                                    ; should be "P1", "P2", "P3" or "P4".
   CmdDelay=cmdDelay, $             ; Set this keyword to a value to "wait" before executing the next command.
   CmdIndex=cmdIndex, $             ; Set this keyword to identify the index of the command to manipulate.
   DeleteCmd=deletecmd, $           ; Set the keyword to delete a command.
   ExecuteCmd=executecmd, $         ; Set this keyword to execute the commands in the window.
   Group_Leader = group_leader, $   ; The group leader of the cgPixmap program.
   ListCmd=listCmd, $               ; Set this keyword to list the commands in the window.
   LoadCmd=loadCmd, $               ; Set this keyword to load commands in the window, but not execute them.
   Method=method, $                 ; If set, will use CALL_METHOD instead of CALL_PROCEDURE to execute command.
   ReplaceCmd=replacecmd, $         ; Set this keyword to replace a command in the window.
   WAspect = waspect, $             ; Set the window aspect ratio to this value.
   WBackground = wbackground, $     ; The background color. Set to !P.Background by default.
   WDestroyObjects=wdestroyobjects, $ ; Set this keyword to destroy object parameters upon exit.
   WErase = weraseit, $             ; Set this keyword to erase the display before executing the command.
   WinID=winid, $                   ; Set this keyword to select an cgPixmap.
   WMulti = wmulti, $               ; Set this in the same way !P.Multi is used.   
   WObject=wobject, $               ; The FSC_CMDWindow object. A return value.
   WOXMargin = woxmargin, $         ; Set the !X.OMargin. A two element array.
   WOYMargin = woymargin, $         ; Set the !Y.OMargin. A two element array
   WXPos = wxpos, $                 ; The X offset of the window on the display. The window is tiled if not set.
   WXSize = wxsize, $               ; The X size of the cgPixmap graphics window in pixels. By default: 640.
   WYPos = wypos, $                 ; The Y offset of the window on the display. The window is tiled if not set.
   WYSize = wysize, $               ; The Y size of the cgPixmap graphics window in pixels. By default: 512.
   WTitle = wtitle, $               ; The window title.
    _Extra = extra                   ; Any extra keywords. Usually the "command" keywords.

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message(Traceback=0)
        RETURN, Obj_New()
    ENDIF

    ; Did the user want to execute the commands in the window?
    IF N_Elements(executeCmd) NE 0 THEN BEGIN
   
      ; Does the self object exist somewhere?
      DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
      IF exists THEN BEGIN
           theList = !FSC_WINDOW_LIST
           IF Obj_Valid(theList) THEN BEGIN
                structs = theList -> Get_Item(/ALL, /DEREFERENCE)
                IF Size(structs, /TNAME) EQ 'POINTER' THEN RETURN, Obj_New()
                IF N_Elements(winID) EQ 0 THEN BEGIN
                    winID = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN winID = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgPixmap with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[winID]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    thisWindowStruct.windowObj -> ExecuteCommands
                ENDIF ELSE BEGIN
                    Message, 'The cgPixmap referred to does not exist.'
                ENDELSE
                RETURN, thisWindowStruct.windowObj
           ENDIF ELSE Message, 'A valid cgPixmap does not exist to execute commands in.'
       ENDIF ELSE Message, 'A cgPixmap object not exist to execute commands in.'
    ENDIF
    
    ; Did the user want to list the commands in a cgPixmap?
    IF N_Elements(listCmd) NE 0 THEN BEGIN
   
      ; Does the self object exist somewhere?
      DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
      IF exists THEN BEGIN
           theList = !FSC_WINDOW_LIST
           IF Obj_Valid(theList) THEN BEGIN
                structs = theList -> Get_Item(/ALL, /DEREFERENCE)
                IF Size(structs, /TNAME) EQ 'POINTER' THEN RETURN, Obj_New()
                IF N_Elements(winID) EQ 0 THEN BEGIN
                    winID = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN winID = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgPixmap with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[winID]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    thisWindowStruct.windowObj -> ListCommand
                ENDIF ELSE BEGIN
                    Message, 'The cgPixmap referred to does not exist.'
                ENDELSE
                RETURN, thisWindowStruct.windowObj
           ENDIF ELSE Message, 'A valid cgPixmap does not exist to list commands from.'
       ENDIF ELSE Message, 'A cgPixmap object not exist to list commands from.'
    ENDIF
    
    ; Did the user want to delete a command in the window?
    IF N_Elements(deleteCmd) NE 0 THEN BEGIN
   
      ; Does the self object exist somewhere?
      DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
      IF exists THEN BEGIN
           theList = !FSC_WINDOW_LIST
           IF Obj_Valid(theList) THEN BEGIN
                structs = theList -> Get_Item(/ALL, /DEREFERENCE)
                IF Size(structs, /TNAME) EQ 'POINTER' THEN RETURN, Obj_New()
                IF N_Elements(winID) EQ 0 THEN BEGIN
                    winID = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN winID = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgPixmap with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[winID]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    ; If the cmdIndex is undefined, the last entered command is deleted.
                    thisWindowStruct.windowObj -> DeleteCommand, cmdIndex
                ENDIF ELSE BEGIN
                    Message, 'The cgPixmap referred to does not exist.'
                ENDELSE
                RETURN, thisWindowStruct.windowObj
           ENDIF ELSE Message, 'The cgPixmap object is not a valid window object.'
       ENDIF ELSE Message, 'A cgPixmap object not exist to add a command to.'
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
                IF Size(structs, /TNAME) EQ 'POINTER' THEN RETURN, Obj_New()
                IF N_Elements(winID) EQ 0 THEN BEGIN
                    winID = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN winID = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgPixmap with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[winID]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    
                    ; Check command components.
                    IF Size(command, /TNAME) NE 'STRING' THEN $
                        Message, 'The first positional argument must be a command string.'
                    IF N_Params() GT 5 THEN $
                        Message, 'The maximum number of positional command parameters allowed is four.'
                    newCommand = Obj_New('cgWindow_Command', COMMAND=command, $
                        P1=p1, P2=p2, P3=p3, P4=p4, KEYWORDS=extra, AltPS_Keywords=altps_Keywords, $
                        AltPS_Params=altps_Params, TYPE=Keyword_Set(method), $
                        DESTROYOBJECTS=Keyword_Set(wdestroyObjects))
                        
                    ; If the cmdIndex is undefined, ALL current commands in the window are replaced.
                    thisWindowStruct.windowObj -> ReplaceCommand, newCommand, cmdIndex, MULTI=wmulti
                    thisWindowStruct.windowObj -> ExecuteCommands
                ENDIF ELSE BEGIN
                    Message, 'The cgPixmap referred to does not exist.'
                ENDELSE
                RETURN, thisWindowStruct.windowObj
           ENDIF ELSE Message, 'A valid cgPixmap does not exist to replace commands with.'
       ENDIF ELSE Message, 'A cgPixmap does not exist to replace commands with.'
    
    ENDIF 
   
   ; Did the user want to load commands without executing them?
   IF Keyword_Set(loadCmd) THEN BEGIN

      ; Does the self object exist somewhere?
      DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
      IF exists THEN BEGIN
           theList = !FSC_WINDOW_LIST
           IF Obj_Valid(theList) THEN BEGIN
                structs = theList -> Get_Item(/ALL, /DEREFERENCE)
                IF Size(structs, /TNAME) EQ 'POINTER' THEN RETURN, Obj_New()
                IF N_Elements(winID) EQ 0 THEN BEGIN
                    winID = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN winID = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgPixmap with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[winID]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    newCommand = Obj_New('cgWindow_Command', COMMAND=command, $
                        P1=p1, P2=p2, P3=p3, P4=p4, KEYWORDS=extra, AltPS_Keywords=altps_Keywords, $
                        AltPS_Params=altps_Params, TYPE=Keyword_Set(method), $
                        DESTROYOBJECTS=Keyword_Set(wdestroyObjects))
                    thisWindowStruct.windowObj -> AddCommand, newCommand, INDEX=cmdIndex
                ENDIF ELSE BEGIN
                    Message, 'The cgPixmap referred to does not exist.'
                ENDELSE
                RETURN, thisWindowStruct.windowObj
           ENDIF ELSE Message, 'A valid cgPixmap does not exist to add a command to.'
       ENDIF ELSE Message, 'A cgPixmap does not exist to add a command to.'
    
    ENDIF 
   
   ; Did the user want to add a command to the window?
   IF Keyword_Set(addCmd) THEN BEGIN

      ; Does the self object exist somewhere?
      DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
      IF exists THEN BEGIN
           theList = !FSC_WINDOW_LIST
           IF Obj_Valid(theList) THEN BEGIN
                structs = theList -> Get_Item(/ALL, /DEREFERENCE)
                IF Size(structs, /TNAME) EQ 'POINTER' THEN RETURN, Obj_New()
                IF N_Elements(winID) EQ 0 THEN BEGIN
                    winID = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN winID = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgPixmap with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[winID]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    newCommand = Obj_New('cgWindow_Command', COMMAND=command, $
                        P1=p1, P2=p2, P3=p3, P4=p4, KEYWORDS=extra, AltPS_Keywords=altps_Keywords, $
                        AltPS_Params=altps_Params, TYPE=Keyword_Set(method), $
                        DESTROYOBJECTS=Keyword_Set(wdestroyObjects))
                    thisWindowStruct.windowObj -> AddCommand, newCommand, INDEX=cmdIndex
                    thisWindowStruct.windowObj -> ExecuteCommands
                ENDIF ELSE BEGIN
                    Message, 'The cgPixmap referred to does not exist.'
                ENDELSE
                RETURN, thisWindowStruct.windowObj
           ENDIF ELSE Message, 'A valid cgPixmap does not exist to add a command to.'
       ENDIF ELSE Message, 'A cgPixmap does not exist to add a command to.'
    
    ENDIF 
   
   ; Otherwise, make the command object.
   wobject = Obj_New('cgPixmapWindow', $
       Command=command, $               ; The graphics "command" to execute.
       P1=p1, P2=p2, P3=p3, P4=p4, $    ; The four allowed positional parameters.
       _Extra = extra, $                ; Any extra keywords. The "command" keywords.
       AltPS_Keywords=altps_Keywords, $ ; A structure of PostScript alternative keywords and values.
       AltPS_Params=altps_Params, $     ; A structure of PostScript alternative parameters and values. Fields 
                                        ; should be "P1", "P2", "P3" or "P4".
       Group_Leader = group_leader, $   ; The group leader of the cgPixmap program.
       Method=method, $                 ; If set, will use CALL_METHOD instead of CALL_PROCEDURE to execute command.
       Aspect = waspect, $              ; Set the window aspect ratio to this value.
       Background = wbackground, $      ; The background color. Not used unless set.
       Multi = wmulti, $                ; Set this in the same way !P.Multi is used.
       Erase = weraseit, $              ; Set this keyword to erase the display before executing the command.
       WDestroyObjects=wdestroyobjects, $ ; Set this keyword to destroy object parameters upon exit.
       WXSize = wxsize, $               ; The X size of the cgPixmap graphics window in pixels. By default: 400.
       WYSize = wysize )                ; The Y size of the cgPixmap graphics window in pixels. By default: 400.
        
   ; Return the object, if it is valid.
   IF Obj_Valid(wobject) THEN RETURN, wobject ELSE RETURN, Obj_New()
  
END
