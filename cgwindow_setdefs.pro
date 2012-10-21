; docformat = 'rst'
;
; NAME:
;   cgWindow_SetDefs
;
; PURPOSE:
;   Allows the user to set global defaults for resizeable cgWindow programs.
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
;   Allows the user to set global defaults for resizeable cgWindow programs.
;
; :Categories:
;    Graphics
;    
; :Keywords:
;     adjustsize: in, optional, type=boolean, default=0
;         Set this keyword to have the default text size adjusted to fit the size of the 
;         display window.
;     aspect: in, optional, type=float, default=0.0
;         Set the aspect ratio of the window. If set to 0, the normal "default" window
;         aspect ratio is used and nothing special is done when the window is resize.
;         If aspect is not 0, then the window is confined to this aspect ratio.
;     background: in, optional, type=string
;         The background color of the window. Only use if the ERASEIT property is also set.
;     delay: in, optional, type=float, default=0
;         Set this keyword to the amount of delay desired between command execution. 0
;     eraseit: in, optional, type=boolean
;         If this property is set, the cgWindow erases with the background color before
;         displaying the commands in the window's command list.
;     im_density: in, optional, type=integer, default=300
;         Set this keyword to the sampling density when ImageMagick creates raster image
;         file from PostScript outout.
;     im_png8: in, optional, type=boolean, default=0
;         Set this keyword to create an 8-bit PNG file rather than the normal 24-bit PNG file.
;     im_options: in, optional, type=string, default=""
;         Set this keyword to any ImageMagick options you would like to pass along to the
;         ImageMagick convert command when creating raster image files from PostScript output.
;     im_raster: in, optional, type=boolean, default=1 (if ImageMagick is found)
;         When raster files are created programmatically, this keyword determines if the
;         raster file is created directly in IDL (value =0) or is created from a PostScript
;         intermediate file via ImageMagick (value =1). The default is via ImageMagick if the
;         convert program can be found on the machine running the program.
;     im_resize: in, optional, type=integer, default=25
;         Set this keyword to percentage that the raster image file created my ImageMagick
;         from PostScript output should be resized.
;     im_transparent: in, optional, type=boolean, default=0
;         Set this keyword to allow ImageMagick to create transparent backgrounds when it
;         makes raster image files from PostScript output.
;     im_width: in, optional, type=integer
;        Set this keyword to the width of the output raster file in pixel units. The height of the raster
;        file is set to preserve the aspect ratio of the output image. Applies only to raster images (eg
;        PNG, JPEG, TIFF, etc.) created from PostScript files with ImageMagick.
;     multi: in, optional, type=Intarr(5)
;         Set this keyword to the !P.MULTI setting you want to use for the window.
;         !P.MULTI is set to this setting before command execution, and set back to
;         it's default value when the commands are finished executing.
;     palette: in, optional, type=byte
;         Use this keyword to pass in an N-by-3 (or 3-by-N) byte array containing the
;         R, G, and B vectors of a color table. It is probably easier to use cgLoadCT or
;         XCOLORS to load color tables for the window, but this is provided as another option.
;     pdf_path: out, optional, type=string
;         Set this keyword to the name of the path to the Ghostscript command for converting PS to PDF.
;     pdf_unix_convert_cmd: out, optional, type=string
;         Set this keyword to the name of an alternative UNIX command to convert PostScript to PDF.
;     ps_charsize: in, optional, type=float, default=0.0
;         Set this value to the !P.Charsize value to use when creating PostScript output. This
;         value is not used if !P.Charsize is set to anything other than 0.0.
;     ps_decomposed: in, optional, type=boolean
;         If set, use decomposed color in the PostScript device.
;     ps_delete: in, optional, type=boolean, default=1
;         Set this keyword to zero if you want to keep the PostScript output ImageMagick creates
;         when making raster file output.
;     ps_encapsulated: in, optional, type=boolean, default=0
;          Set this keyword to configure PSCONFIG to produce encapsulated PostScript output by default.
;     ps_font: in, optional, type=integer, default=0
;          Set this to the !P.Font value to use for creating PostScript files.
;     ps_metric: in, optional, type=boolean, default=0
;          Set this keyword to configure PSCONFIG to use metric values and A4 page size in its interface.
;     ps_quiet: in, optional, type=boolean, default=0
;          Set this keyword to suppress output messages from PS_Start and PS_End.
;     ps_scale_factor: in, optional, type=float, default=1.0
;          Set this keyword to the PostScript scale factor you want to use for PostScript output.
;     ps_tt_font: in, optional, type=string, default="Helvetica"
;          Set this keyword to the name of the PostScript true-type font to use for PostScript output.
;          Not used, unless !P.Font=1.
;     reset: in, optional, type=boolean, default=0
;         Set this keyword to reset all values to their default values.
;     title: in, optional, type=boolean
;         If this keyword is set, the selection is assumed to be a window title. All
;         matching is done in uppercase characters.
;     xomargin: in, optional, type=intarr(2)
;         Set this keyword to set !X.OMargin to this value for multiplots.
;     xpos: in, optional, type=integer
;         Set this keyword to the X offset of the window from the upper-left corner of the display.
;     xsize: in, optional, type=integer, default=640
;         Set this keyword to the starting X size of the window.
;     yomargin: in, optional, type=intarr(2)
;         Set this keyword to set !Y.OMargin to this value for multiplots.
;     ypos: in, optional, type=integer
;         Set this keyword to the Y offset of the window from the upper-left corner of the display.
;     ysize: in, optional, type=integer, default=512
;         Set this keyword to the starting Y size of the window.
;          
; :Examples:
;    Used to set cgWindow global properties::
;       IDL> cgLoadCT, 5, RGB_TABLE=palette
;       IDL> cgWindow_SetDefs, PALETTE=palette, $
;               ERASEIT=1, XSIZE=800, YSIZE=400, XPOS=100, YPOS=200, $
;               PS_ENCAPSULATED=1, PS_METRIC=1
;       IDL> cgImage, cgDemoData(7), /WINDOW, MARGIN=0.1
;       
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: david@idlcoyote.com
;           Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 29 January 2011. DWF.
;        Small bug fixes, and addition of PS_QUIET keyword. 17 Feb 2011. DWF.
;        Added Raster_IM, 18 February 2011. Jeremy Bailin.
;        Added the ability to set and unset adjustable text size in 
;          cgWindow with ADJUSTSIZE keyword. 24 April 2011. DWF.
;        Added PS_DECOMPOSED keyword to set the PostScript color mode. 30 Aug 2011. DWF.
;        Added ASPECT keyword to allow getting/setting of window aspect ratio. 18 Nov 2011. DWF.
;        Added PDF_UNIX_CONVERT_CMD and PDF_PATH keywords. 7 Dec 2011. DWF.
;        Added IM_WIDTH keyword. 3 April 2012. DWF.
;        Added IM_PNG8 keyword. 3 April 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
PRO cgWindow_SetDefs, $
   AdjustSize = adjustsize, $                      ; Adjusts text size to fit display window size.
   Aspect = aspect, $                              ; Set the aspect ratio of the window.
   Background = background, $                      ; The background color. 
   Delay = delay, $                                ; The delay between command execution.
   EraseIt = eraseit, $                            ; Set this keyword to erase the display before executing the commands.
   IM_Density = im_density, $                      ; Sets the density parameter on ImageMagick convert command.
   IM_PNG8 = im_png8, $                            ; Set this keyword to create an 8-bit PNG file, rather than 24-bit.
   IM_Options = im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
   IM_Raster = im_raster, $                        ; Sets thee raster via ImageMagick setting.
   IM_Resize = im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
   IM_Transparent = im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
   IM_Width=im_width, $                            ; Set the width of raster file output from PostScript files.
   Multi = multi, $                                ; Set this in the same way !P.Multi is used.   
   Palette = palette, $                            ; The color table palette to use for the window.
   PDF_Unix_Convert_Cmd = pdf_unix_convert_cmd, $  ; Command to convert PS to PDF.
   PDF_Path = pdf_path, $                          ; The path to the Ghostscript conversion command.
   PS_Decomposed = ps_decomposed, $                ; If set, use decomposed color in PostScript.
   PS_Delete = ps_delete, $                        ; Delete the PostScript file when making IM files.
   PS_Metric = ps_metric, $                        ; Select metric measurements in PostScript output.
   PS_Encapsulated = ps_encapsulated, $            ; Create Encapsulated PostScript output.
   PS_FONT=ps_font, $                              ; Select the font for PostScript output.
   PS_CHARSIZE=ps_charsize, $                      ; Select the character size for PostScript output.
   PS_QUIET=ps_quiet, $                            ; Select the QUIET keyword for PS_START.
   PS_SCALE_FACTOR=ps_scale_factor, $              ; Select the scale factor for PostScript output.
   PS_TT_FONT=ps_tt_font, $                           ; Select the true-type font to use for PostScript output.
   Reset=reset, $                                  ; Reset to original values. 
   XOMargin = xomargin, $                          ; Set the !X.OMargin. A two element array.
   YOMargin = yomargin, $                          ; Set the !Y.OMargin. A two element array
   XSize = xsize, $                                ; The X size of the cgWindow graphics window.
   YSize = ysize, $                                ; The Y size of the cgWindow graphics window.
   Title = title, $                                ; The window title.
   XPos = xpos, $                                  ; The X offset of the window on the display.
   YPos = ypos                                     ; The Y offset of the window on the display. 
      
   Compile_Opt idl2
   
   ; Return to caller on error.
   ON_Error, 2
   
   ; Does the defaults structure exist? If not, you have to create it.
   DefSysV, '!FSC_WINDOW_DEFAULTS', EXISTS=exists
   IF ~exists || Keyword_Set(reset) THEN BEGIN
   
        ; Check the various keywords. Create defaults.
        IF N_Elements(adjustsize) EQ 0 THEN adjustsize = 0
        IF N_Elements(aspect) EQ 0 THEN aspect = 0.0
        IF N_Elements(background) EQ 0 THEN background = 'white'
        IF N_Elements(delay) EQ 0 THEN delay = 0
        IF N_Elements(eraseit) EQ 0 THEN eraseit = 0
        IF N_Elements(multi) EQ 0 THEN multi = Intarr(5)
        IF N_Elements(xomargin) EQ 0 THEN xomargin = FltArr(2)
        IF N_Elements(yomargin) EQ 0 THEN yomargin = FltArr(2)
        IF N_Elements(xsize) EQ 0 THEN xsize = 640
        IF N_Elements(ysize) EQ 0 THEN ysize = 512
        IF N_Elements(title) EQ 0 THEN title = 'Resizeable Graphics Window'
        IF N_Elements(xpos) EQ 0 THEN xpos = -1
        IF N_Elements(ypos) EQ 0 THEN ypos = -1
        IF N_Elements(palette) EQ 0 THEN palette = BytArr(256,3)
        IF N_Elements(im_density) EQ 0 THEN im_density = 300
        im_png8 = Keyword_Set(im_png8)
        IF N_Elements(im_options) EQ 0 THEN im_options = ""
        IF N_Elements(im_raster) EQ 0 THEN BEGIN
            IF cgHasImageMagick() THEN im_raster = 1 ELSE im_raster = 0
        ENDIF
        IF N_Elements(im_resize) EQ 0 THEN im_resize = 25
        IF N_Elements(im_transparent) EQ 0 THEN im_transparent = 0
        IF N_Elements(im_width) EQ 0 THEN im_width = 0
        IF N_Elements(pdf_unix_convert_cmd) EQ 0 THEN pdf_unix_convert_cmd = ""
        IF N_Elements(pdf_path) EQ 0 THEN pdf_path = ""
        IF N_Elements(ps_charsize) EQ 0 THEN ps_charsize = 0.0
        IF N_Elements(ps_encapsulated) EQ 0 THEN ps_encapsulated = 0
        IF N_Elements(ps_font) EQ 0 THEN ps_font = 0
        IF N_Elements(ps_decomposed) EQ 0 THEN ps_decomposed = 0 ; Index mode by default.
        IF N_Elements(ps_delete) EQ 0 THEN ps_delete = 1
        IF N_Elements(ps_metric) EQ 0 THEN ps_metric = 0
        IF N_Elements(ps_quiet) EQ 0 THEN ps_quiet = 0
        IF N_Elements(ps_scale_factor) EQ 0 THEN ps_scale_factor = 1.0
        IF N_Elements(ps_tt_font) EQ 0 THEN ps_tt_font = 'Helvetica'

        ; Define the default structure.
        fsc_window_defaults = { _$_FSC_WINDOW_DEFAULTS, $
           Adjustsize:adjustsize, $                      ; Adjust text size.
           Aspect:aspect, $                              ; The window aspect ratio.
           Background:background, $                      ; The background color. 
           Delay: delay, $                               ; Set this keyword to the delay between command execution.
           EraseIt:eraseit, $                            ; Set this keyword to erase the display before executing the commands.
           Multi:multi, $                                ; Set this in the same way !P.Multi is used.   
           xomargin:xomargin, $                          ; Set the !X.OMargin. A two element array.
           yomargin:yomargin, $                          ; Set the !Y.OMargin. A two element array
           XSize:xsize, $                                ; The X size of the cgWindow graphics window.
           YSize:ysize, $                                ; The Y size of the cgWindow graphics window.
           Title:title, $                                ; The window title.
           XPos:xpos, $                                  ; The X offset of the window on the display.
           YPos:ypos, $                                  ; The Y offset of the window on the display. 
           Palette:palette, $                            ; The color table palette to use for the window.
           IM_Density:im_density, $                      ; Sets the density parameter on ImageMagick convert command.
           IM_PNG8:im_png8, $                            ; If set create 8-bit rather than 24-bit PNG files.
           IM_Options:im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
           IM_Raster:im_raster, $                        ; Sets the raster via ImageMagick setting.
           IM_Resize:im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
           IM_Transparent:im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
           IM_Width:im_width, $                          ; Sets the width of raster output on raster files created with ImageMagick.
           PDF_UNIX_Convert_Cmd: pdf_unix_convert_cmd, $ ; Sets the PDF alternative conversion command.
           PDF_Path: pdf_path, $                         ; Set the path to the PDF conversion command.
           PS_Decomposed:ps_decomposed, $                ; Sets the PostScript color mode.
           PS_Delete:ps_delete, $                        ; Delete the PostScript file when making IM files.
           PS_Metric:ps_metric, $                        ; Select metric measurements in PostScript output.
           PS_Encapsulated:ps_encapsulated, $            ; Create Encapsulated PostScript output.
           PS_Charsize:ps_charsize, $                    ; PostScript character size.
           PS_Font:ps_font, $                            ; PostScript font to use.
           PS_Quiet:ps_quiet, $                          ; PostScript QUIET keyword on PS_Start.
           PS_Scale_Factor:ps_scale_factor, $            ; PostScript scale_factor
           PS_TT_Font:ps_tt_font }                       ; PostScript true-type font.
           
        IF Keyword_Set(reset) THEN BEGIN
            !FSC_WINDOW_DEFAULTS = fsc_window_defaults
        ENDIF ELSE BEGIN
            DefSysV, '!FSC_WINDOW_DEFAULTS', fsc_window_defaults 
        ENDELSE
   ENDIF ELSE BEGIN
    
        IF N_Elements(adjustsize) NE 0 THEN !FSC_WINDOW_DEFAULTS.adjustsize = adjustsize
        IF N_Elements(aspect) NE 0 THEN !FSC_WINDOW_DEFAULTS.aspect = aspect
        IF N_Elements(background) NE 0 THEN !FSC_WINDOW_DEFAULTS.background = background
        IF N_Elements(delay) NE 0 THEN !FSC_WINDOW_DEFAULTS.delay = delay
        IF N_Elements(eraseit) NE 0 THEN !FSC_WINDOW_DEFAULTS.eraseit = eraseit
        IF N_Elements(multi) NE 0 THEN !FSC_WINDOW_DEFAULTS.multi = multi
        IF N_Elements(xomargin) NE 0 THEN !FSC_WINDOW_DEFAULTS.xomargin = xomargin
        IF N_Elements(yomargin) NE 0 THEN !FSC_WINDOW_DEFAULTS.yomargin = oymargin
        IF N_Elements(xsize) NE 0 THEN !FSC_WINDOW_DEFAULTS.xsize = xsize
        IF N_Elements(ysize) NE 0 THEN !FSC_WINDOW_DEFAULTS.ysize = ysize
        IF N_Elements(title) NE 0 THEN !FSC_WINDOW_DEFAULTS.title = title
        IF N_Elements(xpos) NE 0 THEN !FSC_WINDOW_DEFAULTS.xpos = xpos
        IF N_Elements(ypos) NE 0 THEN !FSC_WINDOW_DEFAULTS.ypos = ypos
        IF N_Elements(palette) NE 0 THEN !FSC_WINDOW_DEFAULTS.palette = palette
        IF N_Elements(im_density) NE 0 THEN !FSC_WINDOW_DEFAULTS.im_density = im_density
        IF N_Elements(im_png8) NE 0 THEN !FSC_WINDOW_DEFAULTS.im_png8 = im_png8
        IF N_Elements(im_options) NE 0 THEN !FSC_WINDOW_DEFAULTS.im_options = im_options
        IF N_Elements(im_raster) NE 0 THEN !FSC_WINDOW_DEFAULTS.im_raster = im_raster
        IF N_Elements(im_resize) NE 0 THEN !FSC_WINDOW_DEFAULTS.im_resize = im_resize
        IF N_Elements(im_transparent) NE 0 THEN !FSC_WINDOW_DEFAULTS.im_transparent = Keyword_Set(im_transparent)
        IF N_Elements(im_width) NE 0 THEN !FSC_WINDOW_DEFAULTS.im_width = im_width
        IF N_Elements(raster_im) NE 0 then !FSC_WINDOW_DEFAULTS.raster_im = raster_im
        IF N_Elements(pdf_unix_convert_cmd) NE 0 THEN !FSC_WINDOW_DEFAULTS.pdf_unix_convert_cmd = pdf_unix_convert_cmd
        IF N_Elements(pdf_path) NE 0 THEN !FSC_WINDOW_DEFAULTS.pdf_path = pdf_path
        IF N_Elements(ps_decomposed) NE 0 THEN !FSC_WINDOW_DEFAULTS.ps_decomposed = Keyword_Set(ps_decomposed)
        IF N_Elements(ps_delete) NE 0 THEN !FSC_WINDOW_DEFAULTS.ps_delete = Keyword_Set(ps_delete)
        IF N_Elements(ps_metric) NE 0 THEN !FSC_WINDOW_DEFAULTS.ps_metric = Keyword_Set(ps_metric)
        IF N_Elements(ps_encapsulated) NE 0 THEN !FSC_WINDOW_DEFAULTS.ps_encapsulated = Keyword_Set(ps_encapsulated)
        IF N_Elements(ps_charsize) NE 0 THEN !FSC_WINDOW_DEFAULTS.ps_charsize = ps_charsize
        IF N_Elements(ps_font) NE 0 THEN !FSC_WINDOW_DEFAULTS.ps_font = ps_font
        IF N_Elements(ps_quiet) NE 0 THEN !FSC_WINDOW_DEFAULTS.ps_quiet = ps_quiet
        IF N_Elements(ps_scale_factor) NE 0 THEN !FSC_WINDOW_DEFAULTS.ps_scale_factor = ps_scale_factor
        IF N_Elements(ps_tt_font) NE 0 THEN !FSC_WINDOW_DEFAULTS.ps_tt_font = ps_tt_font
        
   ENDELSE
   
END
