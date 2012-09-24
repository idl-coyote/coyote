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
;     adjustsize: in, optional, type=boolean
;         Set this keyword to adjust default character size to the display window size.
;     aspect: in, optional, type=boolean
;         Set this keyword to the desired aspect ratio (ysize/xsize) of the window.
;     background: in, optional, type=string
;         The background color of the window. Only use if the ERASEIT property is also set.
;     cmdindex: in, optional, type=integer
;         This keyword applies only to keywords that manipulate commands in the command
;         list (e.g., DeleteCmd). It specifies the command index number of the command 
;         for which the action is desired.
;     create_bmp: in, optional, type='string', default='cgwindow.bmp'
;          Set this keyword to the name of a bitmap file to create automatically from the window.
;          Using this keyword is a way to create a bitmap file programmatically from a cgWindow application.
;          The raster file will be created via ImageMagick if im_raster has been set (default).
;          Depreciated now in favor of the `Output` keyword.
;     create_gif: in, optional, type='string', default='cgwindow.gif'
;          Set this keyword to the name of a GIF file to create automatically from the window.
;          Using this keyword is a way to create a GIF file programmatically from a cgWindow application.
;          The raster file will be created via ImageMagick if im_raster has been set (default).
;          Depreciated now in favor of the `Output` keyword.
;     create_jpeg: in, optional, type='string', default='cgwindow.jpeg'
;          Set this keyword to the name of a JPEG file to create automatically from the window.
;          Using this keyword is a way to create a JPEG file programmatically from a cgWindow application.
;          The raster file will be created via ImageMagick if im_raster has been set (default).
;          Depreciated now in favor of the `Output` keyword.
;     create_pdf: in, optional, type='string', default='cgwindow.pdf'
;          Set this keyword to the name of a PNG file to create automatically from the window.
;          Using this keyword is a way to create a PDF file programmatically from a cgWindow application.
;          The PDF file will be created via the Coyote Graphics program cgPS2PDF.
;          Depreciated now in favor of the `Output` keyword.
;     create_png: in, optional, type='string', default='cgwindow.png'
;          Set this keyword to the name of a PNG file to create automatically from the window.
;          Using this keyword is a way to create a PNG file programmatically from a cgWindow application.
;          The raster file will be created via ImageMagick if im_raster has been set (default).
;     create_ps: in, optional, type='string', default='cgwindow.ps'
;          Set this keyword to the name of a PostScript file to create automatically from the window.
;          Using this keyword is a way to create a PostScript file programmatically from a cgWindow application.
;          Depreciated now in favor of the `Output` keyword.
;     create_tiff: in, optional, type='string', default='cgwindow.tiff'
;          Set this keyword to the name of a TIFF file to create automatically from the window.
;          Using this keyword is a way to create a TIFF file programmatically from a cgWindow application.
;          The raster file will be created via ImageMagick if im_raster has been set (default).
;          Depreciated now in favor of the `Output` keyword.
;     delay: in, optional, type=float
;         Set this keyword to the amount of "delay" you want between commands in the command list.
;     deletecmd: in, optional, type=boolean
;          Set this keyword to delete a command in the cgWindow. The keywords cmdIndex and All
;          are used in deleting the specified command.
;     destroy: in, optional, type=boolean
;          Set this keyword to destroy the cgWindow program. This keyword should not be used
;          with other keywords.
;     dimensions: in, optional, type=intarr(2)
;          Set this keyword to a two-element array giving the xsize and ysize
;          of the draw widget.
;     eraseit: in, optional, type=boolean
;         If this property is set, the cgWindow erases with the background color before
;         displaying the commands in the window's command list.
;     execute: in, optional, type=boolean
;         Set this keyword to 1 to exectute the commands in the window's command list. 
;         Set this keyword to 0 to prevent command excution. This is useful, for example,
;         if you want to load commands without having them be executed immediately.
;     get_keycmdindex: in, optional, type=integer
;         Set this value to the number of the command (zero-based) for which you want to
;         obtain the keyword value. If not provided, the first command (command 0) is searched.
;     get_keyword: in, optional, type=string
;         The name of the keyword whose value you want to return in get_keyvalue. The name must
;         be spelled EXACTLY as you used the keyword, except that case does not matter. The string
;         is converted to UPPERCASE to locate the proper keyword. Although it was my intention to use
;         this to retrieve output keyword values, this is not possible using cgWindow due to the way
;         Call_Procedure and keyword inheritance work.
;     get_keyvalue: out, optional, type=any
;         The value of the keyword specified in get_keyword. If the keyword cannot be found, this
;         value will be undefined. You MUST check for this before using the return variable in your program.
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
;     im_raster: in, optional, type=boolean, default=1
;         Set this keyword to zero to create raster files using the create_png etc. keywords
;         directly, instead of via ImageMagick.
;     im_width: in, optional, type=integer
;        Set this keyword to the width of the output raster file in pixel units. The height of the raster
;        file is set to preserve the aspect ratio of the output image. Applies only to raster images (eg
;        PNG, JPEG, TIFF, etc.) created from PostScript files with ImageMagick.
;     multi: in, optional, type=Intarr(5)
;         Set this keyword to the !P.MULTI setting you want to use for the window.
;         !P.MULTI is set to this setting before command execution, and set back to
;         it's default value when the commands are finished executing.
;     object: in, optional, type=boolean
;         If this keyword is set, the selection is assumed to be an object reference.
;     output: in, optional, type=string
;         This keyword should be set to the name of an output file. It is a short-hand way of
;         specifying the CREATE_*** keywords. The type of file is taken from the file name
;         extension.
;     palette: in, optional, type=byte
;         Use this keyword to pass in an N-by-3 (or 3-by-N) byte array containing the
;         R, G, and B vectors of a color table. It is probably easier to use cgLoadCT or
;         XCOLORS to load color tables for the window, but this is provided as another option.
;     pdf_path: out, optional, type=string
;         Set this keyword to the name of the path to the Ghostscript command for converting PS to PDF.
;     pdf_unix_convert_cmd: out, optional, type=string
;         Set this keyword to the name of an alternative UNIX command to convert PostScript to PDF.
;     ps_charsize: in, optional, type=float
;         The PostScript character size.
;     ps_decomposed: in, optional, type=boolean, default=0
;         Set this keyword to zero to set the PostScript color mode to indexed color and to
;         one to set the PostScript color mode to decomposed color.
;     ps_delete: in, optional, type=boolean, default=1
;         Set this keyword to zero if you want to keep the PostScript output ImageMagick creates
;         when making raster file output.
;     ps_encapsulated: in, optional, type=boolean, default=0
;          Set this keyword to configure PSCONFIG to produce encapsulated PostScript output by default.
;     ps_font: in, optional, type=integer
;        Set this keyword to the type of font you want to use in PostScript output. It sets the 
;        FONT keyword on the PSConfig command. Normally, 0 (hardware fonts) or 1 (true-type fonts).
;     ps_metric: in, optional, type=boolean, default=0
;          Set this keyword to configure PSCONFIG to use metric values and A4 page size in its interface.
;     ps_quiet: in, optional, type=boolean, default=0
;          Set this keyword to set the QUIET keyword on PS_Start.
;     ps_scale_factor: in, optional, type=float
;          Set his keyword to the PostScript scale factor you wish to use in creating PostScript output.
;     ps_tt_font: in, optional, type=string
;        Set this keyword to the name of a true-type font to use in creating PostScript output.
;     restore_visualization: in, optional, type=string
;          Set this keyword to the name of a visualization save file to restore.
;     save_visualization: in, optional, type=string, default='graphic.cgs'
;          Set this keyword to the name of a file where the visualization should be saved.
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
;           E-mail: david@idlcoyote.com
;           Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; :History:
;     Change History::
;     Written, 28 January 2011. DWF.
;     Added CREATE_PS keyword. 16 Feb 2011. DWF.
;     Added PS_QUIET, GET_KEYCMDINDEX, GET_KEYWORD, and GET_KEYVALUE keywords. 17 Feb 2011. DWF.
;     Added im_raster and the create_... raster options. 18 Feb 2011. Jeremy Bailin
;     Added the ability to set and unset adjustable text size in 
;        cgWindow with ADJUSTSIZE keyword. 24 April 2011. DWF.
;     Added the ability to set the dimensions of the draw widget programmatically. 14 June 2011.
;     Added PS_DECOMPOSED keyword to set the PostScript color mode. 30 Aug 2011. DWF.
;     Added SAVE_VISUALIZATION and RESTORE_VISUALIZATION keywords. 15 Sept 2011. DWF.
;     Added ASPECT keyword to control window aspect ratio. 9 Nov 2011. DWF.
;     Added CREATE_PDF, PDF_UNIX_CONVERT_CMD, and PDF_PATH keywords. 11 Dec 2011. DWF.
;     Added IM_WIDTH keyword. 3 April 2012. DWF.
;     Added the OUTPUT keyword. 3 April 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2011-2012, Fanning Software Consulting, Inc.
;-
PRO cgControl, selection, $
    ADJUSTSIZE=adjustsize, $                      ; Adjusts text size to fit display window size.
    ASPECT=aspect, $                              ; Sets the aspect ratio of the window.
    ALL=all, $                                    ; Apply the command operation to all the commands (i.e., DeleteCMD)
    BACKGROUND=background, $                      ; Sets the background color of the window
    CMDINDEX=cmdIndex, $                          ; Apply the command operation to this command only.
    CREATE_BMP=create_bmp, $                      ; Set this to the name of a bitmap file that is produced from the commands in the window.
    CREATE_GIF=create_gif, $                      ; Set this to the name of a GIF file that is produced from the commands in the window.
    CREATE_JPEG=create_jpeg, $                    ; Set this to the name of a JPEG file that is produced from the commands in the window.
    CREATE_PDF=create_pdf, $                      ; Set this to the name of a PDF file that is produced from the commands in the window.
    CREATE_PNG=create_png, $                      ; Set this to the name of a PNG file that is produced from the commands in the window.
    CREATE_PS=create_ps, $                        ; Set this to the name of a PostScript file that is produced from the commands in the window.
    CREATE_TIFF=create_tiff, $                    ; Set this to the name of a TIFF file that is produced from the commands in the window.
    PALETTE=palette, $                            ; The color palette (color vectors) associated with this window.
    DELAY=delay, $                                ; Set the delay between command execution.
    DELETECMD=deleteCmd, $                        ; Delete a command. If ALL is set, delete all commands.
    DESTROY=destroy, $                            ; Destroy the window. Should be called alone or with the ALL keyword.
    DIMENSIONS=dimensions, $                      ; The dimensions of the draw widget.
    ERASEIT=eraseit, $                            ; Set the erase feature of the window.
    EXECUTE=execute, $                            ; Execute the commands immediately. 
    GET_KEYCMDINDEX = get_keycmdindex, $          ; Gets the keyword value out of this command. Counting starts at 0.
    GET_KEYWORD=get_keyword, $                    ; Get this keyword value.
    GET_KEYVALUE=get_keyvalue, $                  ; Returns the keyword value. Will be undefined if the keyword is not found.
    LISTCMD=listCmd, $                            ; List a command or ALL commands.
    MULTI=multi, $                                ; Set the multiple property of the window. Identical to !P.Multi.
    OBJECT=object, $                              ; If this keyword is set, the selection is an object reference.
    OUTPUT=output, $                              ; The name of the output file.
    TITLE=title, $                                ; If this keyword is set, the selection is the title.
    UPDATE=update, $                              ; If this keyword is set, the commands are immediately executed after properties are set.
    WIDGETID=widgetID, $                          ; If this keyword is set, the selection is a widget ID.
    XOMARGIN=xomargin, $                          ; Change the !X.OMargin setting for the winow.
    YOMARGIN=yomargin, $                          ; Change the !Y.OMargin setting for the window.
    IM_DENSITY=im_density, $                      ; Sets the density parameter on ImageMagick convert command.
    IM_RESIZE=im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
    IM_OPTIONS=im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
    IM_RASTER=im_raster, $                        ; Sets whether to generate raster files via ImageMagick.
    IM_TRANSPARENT=im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
    IM_WIDTH=im_width, $                          ; Set the width of raster file output from PostScript files.
    PDF_UNIX_CONVERT_CMD=pdf_unix_convert_cmd, $  ; Command to convert PS to PDF.
    PDF_PATH=pdf_path, $                          ; The path to the Ghostscript conversion command.
    PS_CHARSIZE=ps_charsize, $                    ; Select the character size for PostScript output.
    PS_DECOMPOSED=ps_decomposed, $                ; Sets the PostScript color mode.
    PS_DELETE=ps_delete, $                        ; Delete the PostScript file when making IM files.
    PS_ENCAPSULATED=ps_encapsulated, $            ; Create Encapsulated PostScript output.
    PS_FONT=ps_font, $                            ; Select the font for PostScript output.
    PS_METRIC=ps_metric, $                        ; Select metric measurements in PostScript output.
    PS_QUIET=ps_quiet, $                          ; Select the QUIET keyword on PS_Start.
    PS_SCALE_FACTOR=ps_scale_factor, $            ; Select the scale factor for PostScript output.
    PS_TT_FONT=ps_tt_font, $                      ; Select the true-type font to use for PostScript output.
   RESTORE_VISUALIZATION=restore_visualization, $; Set this keyword to the name of a file containing a visualization to restore.
    SAVE_VISUALIZATION=save_visualization         ; Set this keyword to the name of a file where the visualization is to be saved.
    
   Compile_Opt idl2
    
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
   ENDIF
   
   ; Make sure there is something here to control.
   void = cgQuery(COUNT=count)
   IF count EQ 0 THEN RETURN
   
   ; Handle the OUTPUT keyword first, since it sets other keywords.
   IF N_Elements(output) NE 0 THEN BEGIN
   
        ; Start with a fresh slate.
        Undefine, create_bmp
        Undefine, create_gif
        Undefine, create_jpeg
        Undefine, create_pdf
        Undefine, create_png
        Undefine, create_ps
        Undefine, create_tiff
        
        ; Parse the filename to find a file extension.
        rootName = cgRootName(output, DIRECTORY=thisDir, EXTENSION=ext)
        extension = StrUpCase(ext)

        typeOfOutput = ['BMP','EPS', 'GIF','JPEG','JPG','PDF', 'PNG','PS', 'TIFF', 'TIF']
        void = Where(typeOfOutput EQ extension, count)
        IF count EQ 0 THEN Message, 'Cannot output to a file with a "' + ext + '" extension.'
       
        ; Set things up.
        CASE extension OF
          'BMP':  create_bmp = output
          'EPS':  create_ps = output
          'GIF':  create_gif = output
          'JPEG': create_jpeg = output
          'JPG':  create_jpeg = output
          'PDF':  create_pdf = output
          'PNG':  create_png = output
          'PS':   create_ps = output
          'TIFF': create_tiff = output
          'TIF':  create_tiff = output
        ENDCASE
    
   ENDIF
   
   ; Always update, unless told otherwise.
   IF N_Elements(update) EQ 0 THEN update = 1
      
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
   IF count EQ 0 THEN Message, 'There are no cgWindows currently on the display.', /Informational
   
   ; Get the window list.
   list = !FSC_Window_List
   
   ; Decide what to do based on the type of match.
   CASE 1 OF
   
        Keyword_Set(widgetID): BEGIN
            index = Where(tlb EQ selection, selectCount)
            IF selectCount EQ 0 THEN $
                Message, 'No cgWindow matches the selection criteria.', /Informational
            END
            
        Keyword_Set(object): BEGIN
            index = Where(objref EQ selection, selectCount)
            IF selectCount EQ 0 THEN $
                Message, 'No cgWindow matches the selection criteria.', /Infomational
            END
            
        Keyword_Set(title): BEGIN
            index = Where(StrUpCase(titles) EQ StrUpCase(selection), selectCount)
            IF selectCount EQ 0 THEN $
                Message, 'No cgWindow matches the selection criteria.', /Informational
            END

        ELSE: BEGIN
            index = Where(wid EQ selection, selectCount)
            IF selectCount EQ 0 THEN $
                Message, 'No cgWindow matches the selection criteria.', /Informational
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
            FOR j=0,count-1 DO IF Widget_Info(tlb[j], /Valid_ID) THEN Widget_Control, tlb[j], /Destroy
       ENDIF ELSE BEGIN
            IF Widget_Info(tlb[index], /Valid_ID) THEN Widget_Control, tlb[index], /Destroy
       ENDELSE
       RETURN
   ENDIF
   
   ; Are you executing commands? Has to be done before
   ; setting properties.
   IF N_Elements(execute) NE 0 THEN BEGIN
       IF Keyword_Set(execute) THEN BEGIN
            objref[index] -> SetProperty, NoExecuteCommands = 0
       ENDIF ELSE BEGIN
            objref[index] -> SetProperty, NoExecuteCommands = 1
       ENDELSE
   ENDIF
   
   ; Set the properties of the window.
   IF Obj_Valid(objref[index]) THEN objref[index] -> SetProperty, $
        ADJUSTSIZE=adjustsize, $
        ASPECT=aspect, $
        BACKGROUND=background, $
        DELAY=delay, $
        DIMENSIONS=dimensions, $
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
        IM_RASTER = im_raster, $                        ; Sets whether to create raster files via ImageMagick.
        IM_WIDTH = im_width, $                          ; Sets the final width of the raster files create with ImageMagick.
        PDF_UNIX_CONVERT_CMD=pdf_unix_convert_cmd, $    ; Command to convert PS to PDF.
        PDF_PATH=pdf_path, $                            ; The path to the Ghostscript conversion command.
        PS_CHARSIZE=ps_charsize, $                      ; Select the character size for PostScript output.
        PS_DECOMPOSED = ps_decomposed, $                ; Sets the PostScript color mode.
        PS_DELETE = ps_delete, $                        ; Delete the PostScript file when making IM files.
        PS_ENCAPSULATED = ps_encapsulated, $            ; Create encapsulated PostScript output.
        PS_FONT=ps_font, $                              ; Select the font for PostScript output.
        PS_METRIC = ps_metric, $                        ; Select metric measurements in PostScript output.
        PS_QUIET=ps_quiet, $                            ; Setlect the QUIET keywords for PS_Start.
        PS_SCALE_FACTOR=ps_scale_factor, $              ; Select the scale factor for PostScript output.
        PS_TT_FONT=ps_tt_font                           ; Select the true-type font to use for PostScript output.
        
   ; Are you executing commands?
   IF N_Elements(execute) NE 0 THEN BEGIN
       IF Keyword_Set(execute) THEN BEGIN
             IF ~update THEN IF Obj_Valid(objref[index]) THEN objref[index] -> ExecuteCommands
       ENDIF 
   ENDIF
   
   ; Are you creating a PostScript file?
   IF N_Elements(create_ps) NE 0 THEN BEGIN
   
       typeName = Size(create_ps, /TNAME)
       CASE 1 OF
          typeName EQ 'STRING': BEGIN
            filename = create_ps
            END
          (typeName EQ 'INT') && (create_ps[0] EQ 1): BEGIN
               filename = 'cgwindow.ps'
            END
          ELSE: Message, 'Incorrect input to CREATE_PS keyword.'
       ENDCASE
       
       IF Obj_Valid(objref[index]) THEN objref[index] -> AutoPostScriptFile, filename 
   ENDIF

   ; Are you creating a PDF file?
   IF N_Elements(create_pdf) NE 0 THEN BEGIN
   
       typeName = Size(create_pdf, /TNAME)
       CASE 1 OF
          typeName EQ 'STRING': BEGIN
            filename = create_pdf
            END
          (typeName EQ 'INT') && (create_pdf[0] EQ 1): BEGIN
               filename = 'cgwindow.pdf'
            END
          ELSE: Message, 'Incorrect input to CREATE_PDF keyword.'
       ENDCASE
       
       IF Obj_Valid(objref[index]) THEN objref[index] -> AutoRasterFile, 'PDF', filename 
   ENDIF
    
   ; Are you creating a bitmap file?
   IF N_Elements(create_bmp) NE 0 THEN BEGIN
   
       typeName = Size(create_bmp, /TNAME)
       CASE 1 OF
          typeName EQ 'STRING': BEGIN
            filename = create_bmp
            END
          (typeName EQ 'INT') && (create_bmp[0] EQ 1): BEGIN
               filename = 'cgwindow.bmp'
            END
          ELSE: Message, 'Incorrect input to CREATE_BMP keyword.'
       ENDCASE
       
       IF Obj_Valid(objref[index]) THEN objref[index] -> AutoRasterFile, 'BMP', filename 
   ENDIF
    
   ; Are you creating a GIF file?
   IF N_Elements(create_gif) NE 0 THEN BEGIN
   
       typeName = Size(create_gif, /TNAME)
       CASE 1 OF
          typeName EQ 'STRING': BEGIN
            filename = create_gif
            END
          (typeName EQ 'INT') && (create_gif[0] EQ 1): BEGIN
               filename = 'cgwindow.gif'
            END
          ELSE: Message, 'Incorrect input to CREATE_GIF keyword.'
       ENDCASE
       
       IF Obj_Valid(objref[index]) THEN objref[index] -> AutoRasterFile, 'GIF', filename 
   ENDIF
    
   ; Are you creating a JPEG file?
   IF N_Elements(create_jpeg) NE 0 THEN BEGIN
   
       typeName = Size(create_jpeg, /TNAME)
       CASE 1 OF
          typeName EQ 'STRING': BEGIN
            filename = create_jpeg
            END
          (typeName EQ 'INT') && (create_jpeg[0] EQ 1): BEGIN
               filename = 'cgwindow.jpeg'
            END
          ELSE: Message, 'Incorrect input to CREATE_JPEG keyword.'
       ENDCASE
       
       IF Obj_Valid(objref[index]) THEN objref[index] -> AutoRasterFile, 'JPEG', filename 
   ENDIF
    
   ; Are you creating a PNG file?
   IF N_Elements(create_png) NE 0 THEN BEGIN
   
       typeName = Size(create_png, /TNAME)
       CASE 1 OF
          typeName EQ 'STRING': BEGIN
            filename = create_png
            END
          (typeName EQ 'INT') && (create_png[0] EQ 1): BEGIN
               filename = 'cgwindow.png'
            END
          ELSE: Message, 'Incorrect input to CREATE_PNG keyword.'
       ENDCASE
       
       IF Obj_Valid(objref[index]) THEN objref[index] -> AutoRasterFile, 'PNG', filename 
   ENDIF
    
   ; Are you creating a TIFF file?
   IF N_Elements(create_tiff) NE 0 THEN BEGIN
   
       typeName = Size(create_tiff, /TNAME)
       CASE 1 OF
          typeName EQ 'STRING': BEGIN
            filename = create_tiff
            END
          (typeName EQ 'INT') && (create_tiff[0] EQ 1): BEGIN
               filename = 'cgwindow.tiff'
            END
          ELSE: Message, 'Incorrect input to CREATE_TIFF keyword.'
       ENDCASE
       
       IF Obj_Valid(objref[index]) THEN objref[index] -> AutoRasterFile, 'TIFF', filename 
   ENDIF
   
   ; Are you saving a visualization?
   IF N_Elements(save_visualization) NE 0 THEN BEGIN
   
       ; Get a filename if this is not a string.
       IF Size(save_visualization, /TNAME) EQ 'STRING' $
           THEN filename = save_visualization $
           ELSE filename = 'graphics.cgs'
           
       ; Save the visualization to a file.
       IF Obj_Valid(objref[index]) THEN objref[index] -> SaveCommands, filename
   ENDIF
    
   ; Are you restoring a visualization?
   IF N_Elements(restore_visualization) NE 0 THEN BEGIN
   
       ; Get a filename if this is not a string.
       IF Size(restore_visualization, /TNAME) EQ 'STRING' $
           THEN filename = restore_visualization $
           ELSE BEGIN
                filename = cgPickfile()
           ENDELSE
           
       ; Does this file exist?
       IF File_Test(filename) EQ 0 THEN Message, 'Cannot find the file: ' + filename
           
       IF filename NE "" THEN BEGIN
       
           ; Restore the visualization.
           IF Obj_Valid(objref[index]) THEN objref[index] -> RestoreCommands, filename
           
        ENDIF
        
   ENDIF

   ; Need a keyword?
   IF N_Elements(get_keyword) NE 0 THEN BEGIN
      IF Size(get_keyword, /TNAME) NE 'STRING' THEN Message, 'Keyword name must be a string variable.'
      IF Obj_Valid(objref[index]) THEN BEGIN
      
            ; Look for the keyword in the first command, unless told otherwise.
            IF N_Elements(get_keycmdindex) EQ 0 THEN get_keycmdindex = 0
            keywordValue = objref[index] -> GetCommandKeyword(get_keyword, get_keycmdindex, SUCCESS=success)
            IF success THEN get_keyvalue = keywordValue
      ENDIF
   ENDIF
END 
