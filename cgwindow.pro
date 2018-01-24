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
;  Creates a resizeable graphics window for IDL traditional commands (Plot, Contour, 
;  Surface, etc. or for Coyote Graphics routines, `cgPlot`, `cgContour`, `cgSurf`, etc.). 
;  In addition, the window contents can be saved as PostScript, PDF, or raster image 
;  files. If ImageMagick is installed on your machine, the raster image files can be 
;  created in very high quality from PostScript files.
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
;-


;+
;   Creates a resizeable graphics window for IDL traditional commands (Plot, Contour, 
;   Surface, etc. or for Coyote Graphics routines, cgPlot, cgContour, cgSurf, etc.). 
;   In addition, the window contents can be saved as PostScript files or as raster image 
;   files. If ImageMagick is installed on your machine, the raster image files can be 
;   created in very high quality from PostScript files.
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
;       Use this keyword to select a previously open and existing cgWindow identifier 
;       (the number between the parentheses in the title bar of cgWindow). The AddCmd, 
;       ReplaceCmd, ListCmd, and DeleteCmd keywords all apply to the "current" cgWindow
;       unless this keyword is used to select another existing cgWindow to apply the 
;       commands to.
;    wmulti: in, optional, type=intarr(5)
;        Set this keyword in exactly the same way you would set the !P.Multi keyword.
;        It will allow you to display multi-plots in the cgWindow graphics window.
;    wobject: out, optional, type=object
;       cgWindow creates a cgCmdWindow object. This object reference is returned
;       if this keyword is present.
;    woxmargin: in, optional, type=float
;       A two-element array indicating the left and right X outside margins for the
;       graphical display. Used only when doing multiple plots with `WMulti`.
;    woymargin: in, optional, type=float
;       A two-element array indicating the bottom and top Y outside margins for the
;       graphical display. Used only when doing multiple plots with `WMulti`.
;    wxpos: in, optional, type=integer, default=5
;       The x offset in device coordinates of the cgWindow from the upper-left corner of the display.
;    wypos: in, optional, type=integer, default=5
;       The y offset in device coordinates of the cgWindow from the upper-left corner of the display.
;    wxsize: in, optional, type=integer, default=640
;       The x size in device coordinates of the graphics window.
;    wysize: in, optional, type=integer, default=5
;       The y size in device coordinates of the the graphics window.
;    wtitle: in, optional, type=string, default='Resizeable Graphics Window'
;       The title of the graphics window. 
;          
; :Examples:
;    Test code::
;       data = cgDemoData(17)
;       cgWindow, 'cgPlot', data, COLOR='red'
;       cgWindow, 'cgPlot', data, PSYM=2, /Overplot, COLOR='dodger blue', /AddCmd
;       cgWIndow, 'cgPlot', cgDemoData(17), color='olive', linestyle = 2, /Overplot, /AddCmd
;       cgWindow, /ListCmd
;       cgWindow, 'cgPlot', data, COLOR='purple', /ReplaceCMD, CMDINDEX=0
;       
;    Additional examples can be found here::
;       
;        http://www.idlcoyote.com/graphics_tips/cgwindow.html
;          
;    Example using different keyword parameters for the display and PostScript output::
;    
;        IDL> cgPlot, cgDemoData(1), /WINDOW, $
;             THICK=1.0, XTITLE='Distance (' + cgGreek('mu') + 'm)', $
;             ALTPS_KEYWORDS={THICK:4.0, XTITLE:'Distance (' + cgGreek('mu', /PS) + 'm)'}
;           
;    Example using different positional parameters::
;    
;        IDL> cgText, 0.20, 0.85, /Normal, 'Line of Text', ALIGNMENT=0.0, $
;             ALTPS_KEYWORDS={ALIGNMENT:1.0}, ALTPS_PARAMS={P1:0.88}, /ADDCMD
;           
;    Additional examples can be found here::
;    
;        http://www.idlcoyote.com/cg_tips/kwexpressions.php
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
;           window the current window before calling cgPS_Open. 17 Feb 2011. DWF.
;        Added machinery for programmatically generating raster files. 18 Feb 2011. Jeremy Bailin.
;        Problem with restoring visualizations fixed. 6 March 2011. DWF.
;        Fixed a problem with CALL_METHOD, which requires one positional parameter. 8 March 2011. DWF.
;        Added the ability to set and unset adjustable text size in the window. 24 April 2011. DWF.
;        Fixed a problem in the ReplaceCommand method that had input parameters reversed. 6 May 2011. DWF.
;        Added the ability to set the dimensions of the draw widget programmatically. 14 June 2011.
;        Added the keywords EvalKeywords and EvalParams to allow evaluation of command parameters and
;            keywords at run-time. See http://www.idlcoyote.com/cg_tips/kwexpressions.php for
;            additional details and explanations of how these keywords should be used. 2 Aug 2011.
;        Problem dereferencing a null pointer in DRAW method fixed. 3 Aug 2011. DWF.
;        Changes to handle inability to create raster files from PS encapsulated files in 
;           landscape mode. 26 Aug 2011. DWF.
;        Added ability to set PostScript color mode. 30 Aug 2011. DWF.
;        The method I was using for evaluating keyword and argument parameters at run-time
;            was just WAY too complicated and difficult to use. I have eliminated this
;            method (along with the EvalKeywords and EvalParams) in favor of a method that
;            allows the user to supply alternative values to use in the PostScript device.
;            This uses keywords AltPS_Keywords and AltPS_Params to collect these alternative
;            arguments in structures that can be used at run-time to supply alternative values.
;            As before, this is explained in detail at http://www.idlcoyote.com/cg_tips/kwexpressions.php.
;            1 Sept 2011. DWF.
;         Missed a couple of places to set decomposition color mode. 7 Sept 2011. DWF.
;         Fixed a problem with improper filename when creating raster file vis
;             Imagemagick via cgControl. 10 Oct 2011. DWF.
;         Added WASPECT keyword to allow window aspect ratio to be set. 9 Nov 2011. DWF.   
;         Added PDF file to the Save As menu. Requires Ghostscript to be installed on some machines. 6 Dec 2011. DWF.
;         Added modifications to allow PDF files to be programmatically created from cgControl. 11 Dec 2011. DWF.
;         Added the ability to specify a fourth positional parameter. 6 Jan 2012. DWF.
;         Separated off the cgCmdWindow part of the code to make an object-widget draw widget. 19 Jan 2012. DWF.
;         Fixed a small type with the Outside Margin keywords that was preventing these from being used. 19 April 2012. DWF.
;         In decompling cgWindow from cgCmdWindow, I accidentally named the WASPECT keyword ASPECT. Restored
;             original name in this version. 13 July 2012. DWF.
;         Added WDestroyObjects keyword to destroy objects parameters, if needed. 11 November 2012. DWF.
;         Restored the WMULTI keyword win calling cgCmdWindow. 11 Feb 2013. DWF.
;         Fixed a problem in which the WinID keyword was being redefined internally. 18 June 2015. DWF.
;-
PRO cgWindow, $
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
   Group_Leader = group_leader, $   ; The group leader of the cgWindow program.
   ListCmd=listCmd, $               ; Set this keyword to list the commands in the window.
   LoadCmd=loadCmd, $               ; Set this keyword to load commands in the window, but not execute them.
   Method=method, $                 ; If set, will use CALL_METHOD instead of CALL_PROCEDURE to execute command.
   ReplaceCmd=replacecmd, $         ; Set this keyword to replace a command in the window.
   WAspect = waspect, $             ; Set the window aspect ratio to this value.
   WBackground = wbackground, $     ; The background color. Set to !P.Background by default.
   WDestroyObjects=wdestroyobjects, $ ; Set this keyword to destroy object parameters upon exit.
   WErase = weraseit, $             ; Set this keyword to erase the display before executing the command.
   WinID=winid, $                   ; Set this keyword to select an cgWindow.
   WMulti = wmulti, $               ; Set this in the same way !P.Multi is used.   
   WObject=wobject, $               ; The cgCMDWindow object. A return value.
   WOXMargin = woxmargin, $         ; Set the !X.OMargin. A two element array.
   WOYMargin = woymargin, $         ; Set the !Y.OMargin. A two element array
   WXPos = wxpos, $                 ; The X offset of the window on the display. The window is tiled if not set.
   WXSize = wxsize, $               ; The X size of the cgWindow graphics window in pixels. By default: 640.
   WYPos = wypos, $                 ; The Y offset of the window on the display. The window is tiled if not set.
   WYSize = wysize, $               ; The Y size of the cgWindow graphics window in pixels. By default: 512.
   WTitle = wtitle, $               ; The window title.
    _Extra = extra                   ; Any extra keywords. Usually the "command" keywords.

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg(Traceback=0)
        RETURN
    ENDIF
    destroyObjects = Keyword_Set(wdestroyobjects)

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
                    windowDisplay = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN windowDisplay = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgWindow with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[windowDisplay]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    thisWindowStruct.windowObj -> ExecuteCommands
                ENDIF ELSE BEGIN
                    Message, 'The cgWindow referred to does not exist.'
                ENDELSE
                RETURN
           ENDIF ELSE Message, 'A valid cgWindow does not exist to execute commands in.'
       ENDIF ELSE Message, 'A cgWindow object not exist to execute commands in.'
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
                    windowDisplay = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN windowDisplay = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgWindow with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[windowDisplay]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    thisWindowStruct.windowObj -> ListCommand
                ENDIF ELSE BEGIN
                    Message, 'The cgWindow referred to does not exist.'
                ENDELSE
                RETURN
           ENDIF ELSE Message, 'A valid cgWindow does not exist to list commands from.'
       ENDIF ELSE Message, 'A cgWindow object not exist to list commands from.'
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
                    windowDisplay = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN windowDisplay = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgWindow with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[windowDisplay]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    ; If the cmdIndex is undefined, the last entered command is deleted.
                    thisWindowStruct.windowObj -> DeleteCommand, cmdIndex
                ENDIF ELSE BEGIN
                    Message, 'The cgWindow referred to does not exist.'
                ENDELSE
                RETURN
           ENDIF ELSE Message, 'The cgWindow object is not a valid window object.'
       ENDIF ELSE Message, 'A cgWindow object not exist to add a command to.'
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
                    windowDisplay = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN windowDisplay = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgWindow with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[windowDisplay]
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
                    Message, 'The cgWindow referred to does not exist.'
                ENDELSE
                RETURN
           ENDIF ELSE Message, 'A valid cgWindow does not exist to replace commands with.'
       ENDIF ELSE Message, 'A cgWindow does not exist to replace commands with.'
    
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
                    windowDisplay = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN windowDisplay = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgWindow with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[windowDisplay]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    newCommand = Obj_New('cgWindow_Command', COMMAND=command, $
                        P1=p1, P2=p2, P3=p3, P4=p4, KEYWORDS=extra, AltPS_Keywords=altps_Keywords, $
                        AltPS_Params=altps_Params, TYPE=Keyword_Set(method), $
                        DESTROYOBJECTS=Keyword_Set(wdestroyObjects))
                    thisWindowStruct.windowObj -> AddCommand, newCommand, INDEX=cmdIndex
                ENDIF ELSE BEGIN
                    Message, 'The cgWindow referred to does not exist.'
                ENDELSE
                RETURN
           ENDIF ELSE Message, 'A valid cgWindow does not exist to add a command to.'
       ENDIF ELSE Message, 'A cgWindow does not exist to add a command to.'
    
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
                    windowDisplay = N_Elements(structs) - 1
                ENDIF ELSE BEGIN
                    index = Where(structs.wid[*] EQ winID, count)
                    IF count GT 0 THEN windowDisplay = index[0] ELSE BEGIN
                        Message, 'Cannot find an cgWindow with window index ' + StrTrim(winID, 2) + '.'
                    ENDELSE
                ENDELSE
                thisWindowStruct = structs[windowDisplay]
                IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
                    newCommand = Obj_New('cgWindow_Command', COMMAND=command, $
                        P1=p1, P2=p2, P3=p3, P4=p4, KEYWORDS=extra, AltPS_Keywords=altps_Keywords, $
                        AltPS_Params=altps_Params, TYPE=Keyword_Set(method), $
                        DESTROYOBJECTS=Keyword_Set(wdestroyObjects))
                    thisWindowStruct.windowObj -> AddCommand, newCommand, INDEX=cmdIndex
                    thisWindowStruct.windowObj -> ExecuteCommands
                ENDIF ELSE BEGIN
                    Message, 'The cgWindow referred to does not exist.'
                ENDELSE
                RETURN
           ENDIF ELSE Message, 'A valid cgWindow does not exist to add a command to.'
       ENDIF ELSE Message, 'A cgWindow does not exist to add a command to.'
    
    ENDIF 
   
   ; Otherwise, make the command object.
   wobject = Obj_New('cgCmdWindow', $
       Command=command, $               ; The graphics "command" to execute.
       P1=p1, P2=p2, P3=p3, P4=p4, $    ; The four allowed positional parameters.
       _Extra = extra, $                ; Any extra keywords. The "command" keywords.
       CmdDelay = cmdDelay, $           ; The amount of time to "wait" between commands.
       AltPS_Keywords=altps_Keywords, $ ; A structure of PostScript alternative keywords and values.
       AltPS_Params=altps_Params, $     ; A structure of PostScript alternative parameters and values. Fields 
                                        ; should be "P1", "P2", "P3" or "P4".
       Group_Leader = group_leader, $   ; The group leader of the cgWindow program.
       Method=method, $                 ; If set, will use CALL_METHOD instead of CALL_PROCEDURE to execute command.
       Background = wbackground, $      ; The background color. Not used unless set.
       Erase = weraseit, $              ; Set this keyword to erase the display before executing the command.
       OXMargin = woxMargin, $          ; The X outside margin.
       OYMargin = woymargin, $          ; The Y outside margin.
       WAspect = waspect, $             ; Set the window aspect ratio to this value.
       WDestroyObjects=wdestroyobjects, $ ; Set this keyword to destroy object parameters upon exit.
       WMulti = wmulti, $               ; Set this in the same way !P.Multi is used.
       WXSize = wxsize, $               ; The X size of the cgWindow graphics window in pixels. By default: 400.
       WYSize = wysize, $               ; The Y size of the cgWindow graphics window in pixels. By default: 400.
       WTitle = wtitle, $               ; The window title.
       WXPos = wxpos, $                 ; The X offset of the window on the display. The window is centered if not set.
       WYPos = wypos )                  ; The Y offset of the window on the display. The window is centered if not set.
        
END
