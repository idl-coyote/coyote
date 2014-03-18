; docformat = 'rst'
;
; NAME:
;   cgPS_Open
;
; PURPOSE:
;    The purpose of cgPS_Open and cgPS_Close is to make it easy to set-up
;    and close a PostScript file. These programs are used extensively
;    in all Coyote Graphics routines.
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
; The purpose of cgPS_Open and `cgPS_Close` is to make it easy to set-up
; and close a PostScript file. These programs are used extensively
; in all Coyote Graphics routines.
;
; If `ImageMagick  <http://www.imagemagick.org/script/index.php>` is installed 
; on your computer, you can easily convert PostScript output to GIF, JPEG, PNG, and TIFF
; raster output. If `Ghostscript <http://www.ghostscript.com/download/>` is installed
; you can convert PostScript output to PDF files. See the appropriate keywords to
; `cgPS_Close`.
; 
; When cgPS_Open is called, the current graphics device is set to "PS" (the PostScript 
; device). When cgPS_Close is called the current graphics device is returned to the device
; in effect when cgPS_Open was called.
; 
; cgPS_Open uses the current display window as a template for the Postscript
; file. Thus, if the display window is wider than it is higher, output is
; in Landscape mode. To set the size of the PostScript "window" yourself, be
; sure to set the NOMATCH keyword to 1.
;       
; To display surface plots correctly the FONT keyword should be set to 1.
; Otherwise, the default font is 0, or hardware fonts when outputting to 
; PostScript.
;
; You can easily configure any modifications you like for your PostScript output
; by setting fields in the plot and axis system variables (!P, !X, !Y, and !Z).
; The modifications currently made by default in this program are these::
;
;     !P.Thick = 3
;     !P.CharThick = 3
;     !X.Thick = 3
;     !Y.Thick = 3
;     !Z.Thick = 3
;     !P.Font = 0
;          
; The !P.Charsize variable is set differently on Windows computers, and depending
; on whether !P.MULTI is being used. On Windows the default is 1.25, or 1.00 for
; multiple plots. On other computers, the default is 1.5, or 1.25 for multiple plots.
; If true-type fonts are being used (FONT=1), the default is 1.5, or 1.25 for 
; multiple plots.
;
; The cgPS_Open program contains the common block, _$FSC_PS_START_. See the FSC_PS_SETUP__DEFINE 
; program in the Coyote Library for its definition.
; 
; :Categories:
;    Utilities, Graphics
;    
; :Params:
;     filename: in, optional, type=string, default='idl.ps'
;        The name of the PostScript file to be created. This can also be the name of a raster 
;        file (e.g., PNG, JPEG, TIFF, PDF, etc.) that you would like to have created from a PostScript 
;        intermediate file. This requires that ImageMagick is installed correctly on your 
;        machine. If you choose this kind of filename, the intermediate PostScript file is
;        automatically deleted.
;    
; :Keywords:
;     cancel: out, optional, type=boolean, default=0
;         An output keyword that is set to 1 if the user cancelled from
;         PS_Config. Otherwise, set to 0.
;     charsize: in, optional, type=float
;         If this keyword is set, the !P.Charsize variable is set to this value until cgPS_Close is called.
;     default_thickness: in, optional, type=integer, default=3
;         Sets the following system variables to this value while creating PostScript output:
;         !P.Thick, !P.CharThick, !X.Thick, !Y.Thick, !Z.Thick. These variables are returned to
;         their original values by `cgPS_Close`. A system variable is set to this value only if it 
;         currently contains the IDL default value of 0.0. If it is set to anything else, this 
;         default thickness value is ignored.
;     dejavusans: in, optional, type=boolean, default=0
;         Set this keyword to select the DejaVuSans true-type font for PostScript output.
;         This option is ONLY available in IDL 8.2 or higher and/or you have installed the
;         DejaVuSans true-type font in your font directory.
;     filename: in, optional, type=string, default='idl.ps'
;         The name of the PostScript file created. An alternative, and older, way of setting
;         the `filename` parameter.
;     font: in, optional, type=integer, default=0                
;         Set this to the type of font you want. A -1 selects Hershey fonts, a 0 selects hardware 
;         fonts (Helvetica, normally), and a 1 selects a True-Type font. Set to 0 by default.
;     encapsulated: in, optional, type=boolean, default=0
;         Set this keyword to produce encapsulated PostScript output.
;     gui: in, optional, type=boolean, default=0
;         The default behavior is to use cgPS_Config to configure the PostScript device silently. 
;         If you wish to allow the user to interatively configure the PostScript device, set this
;         keyword.
;     keywords: out, optional, type=structure                
;         This output keyword contains the keyword structure returned from PS_Config.
;     landscape: in, optional, type=boolean, default=0
;         Set this keyword to produce landscape PostScript output.
;     nomatch: in, optional, type=boolean, default=0                
;         Normally, cgPS_Open will try to "match" the aspect ratio of the PostScript file "window" 
;         to the current display window. If this keyword is set, then this doesn't occur, giving 
;         the user the option of specifying the size and offsets of the PostScript window directly 
;         though appropriate keywords.
;     quiet: in, optional, type=boolean, default=0
;         If set, informational messages are not set. 
;     scale_factor: in, optional, type=float, default=1.0
;         Set this to the PostScript scale factor. By default: 1.
;     tt_font: in, optional, type=string
;         The name of a true-type font to use. Using this keyword sets `Font` to 1.
;     _ref_extra: in, optional
;         Any keyword appropriate for the PostScript configuration program cgPS_Config, from
;         the Coyote Library can be used with cgPS_Open.
;              
; :Examples:
;    To create a line plot in a PostScript file named lineplot.ps and
;    also create a PNG file named lineplot.png for display in a browser,
;    type these commands::
;
;        cgPS_Open, 'lineplot.ps'
;        cgPlot, Findgen(11), COLOR='navy', /NODATA, XTITLE='Time', YTITLE='Signal'
;        cgPlot, Findgen(11), COLOR='indian red', /OVERPLOT
;        cgPlot, Findgen(11), COLOR='olive', PSYM=2, /OVERPLOT
;        cgPS_Close, /PNG
;        
;    Or, alternatively, without also creating a PostScript file, like this. Simply pass
;    cgPS_Open the name of the raster file you want to create::
;    
;        cgPS_Open, 'lineplot.png'
;        cgPlot, Findgen(11), COLOR='navy', /NODATA, XTITLE='Time', YTITLE='Signal'
;        cgPlot, Findgen(11), COLOR='indian red', /OVERPLOT
;        cgPlot, Findgen(11), COLOR='olive', PSYM=2, /OVERPLOT
;        cgPS_Close
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
;       Written by: David W. Fanning, 20 May 2008.
;       Slight modification to allow filenames with spaces in them.
;       Added NoMatch keyword. 17 March 2009. DWF.
;       Added a number of keywords to make these commands more configurable. 19 March 2009. DWF.
;       Only set thickness system variables if starting system variables are set to their
;           default values (namely, 0). This allows users to set their own system variables
;           before they call PS_START, rather than after. 23 March  2009. DWF.
;       Moved PS_END to its own file to allow the IDLExBr_Assistant to work properly. 7 April 2009. DWF.
;       Modified to allow PostScript page type to be stored for future processing with cgFixPS. 9 August 2009. DWF.
;       Added NoFix keyword to PS_END calls to repair previous, but unused set-ups. 1 Nov 2010. DWF.
;       Added Charsize keyword to PS_START. 14 Nov 2010. DWF.
;       Changed the way default character sizes are set. 19 Nov 2010. DWF.
;       Added CANCEL and KEYWORDS output keywords. 16 Jan 2011. DWF.
;       Changes to handle inability to create raster files from PS encapsulated files in 
;           landscape mode. 26 Aug 2011. DWF.
;       The SCALE_FACTOR is called at the time the PostScript file is opened to avoid problems
;           with the bounding box not being set to the correct values. 26 October 2011. DWF.
;       Created a DEFAULT_THICKNESS keyword to set the default thicknesses of PostScript 
;           system variables. 14 Dec 2011. DWF.
;       Moved the true-type font set-up to *after* changing the graphics device to PostScript. 10 Jan 2012. DWF.
;       Added DejaVuSans keyword to allow this true-type font to be used in PostScript Output. 21 Dec 2012. DWF.
;       Modified so that the PostScript device can keep a consistent interface when using True-Type
;          fonts. Requires using cgSet_TTFont to select True-Type fonts. 22 May 2013. DWF.
;       Changed name to cgPS_Open from PS_Start. 4 November 2013. DWF. Retired PS_Start.
;       Added ability to specify the name of the output raster file desired as the filename. If this is done,
;          and ImageMagick is installed, the PostScript intermediate file is deleted and the raster file is
;          created automatically without setting a raster output keyword on cgPS_Close. 29 Nov 2013. DWF.
;       Moved the check for Charsize to after setting to the PostScript device. 14 Jan 2014. DWF.
;       The program wasn't picking up default values from cgWindow_GetDefs. 22 Jan 2014. DWF.
;       Modified the program so that the PostScript file location is printed only if the PostScript file 
;          is being retrained. 17 March 2014. DWF.
;       
; :Copyright:
;     Copyright (c) 2008-2014, Fanning Software Consulting, Inc.
;-
PRO cgPS_Open, filename, $
    CANCEL=cancelled, $
    CHARSIZE=charsize, $
    DEFAULT_THICKNESS=default_thickness, $
    DEJAVUSANS=dejavusans, $
    FILENAME=ps_filename, $
    FONT=font , $
    ENCAPSULATED=encapsulated, $
    GUI=gui, $
    KEYWORDS=keywords, $
    LANDSCAPE=landscape, $
    NOMATCH=nomatch, $
    QUIET=quiet, $
    SCALE_FACTOR=scale_factor, $
    TT_FONT=tt_font, $
   _REF_EXTRA=extra
 
   COMMON _$FSC_PS_START_, ps_struct
   
    ; Define the PS structure.
    IF N_Elements(ps_struct) EQ 0 THEN ps_struct = {cgPS_SETUP}
       
   ; Handle the filename parameter and keywords. This is necessary because "filename" can come
   ; from both an input parameter and a keyword parameter (historical reasons).
   IF N_Elements(filename) EQ 0 THEN filename = 'idl.ps'
   IF N_Elements(ps_filename) EQ 0 THEN ps_filename = filename 
   
   ; Get the file extension. This will tell you what kind of raster file you need to make, if any.
   rootname = cgRootName(ps_filename, DIRECTORY=directory, EXTENSION=extension)
   print_ps_location = 1
   CASE StrUpCase(extension) OF
       'PS': 
       'EPS':
       '': ps_filename = Filepath(ROOT_DIR=directory, rootname + '.ps')
       ELSE: BEGIN
          ps_filename = Filepath(ROOT_DIR=directory, rootname + '.ps')
          
          ; If ImageMagick is installed, the we can create the raster file directly,
          ; and we can delete the intermediate PostScript file.
          IF cgHasImageMagick() THEN BEGIN
             ps_struct.rasterFileType = extension
             print_ps_location = 0
          END
          END
   ENDCASE
      
   ; Need DejaVuSans fonts?
   IF Keyword_Set(dejavusans) && (Float(!Version.Release) GE 8.2) THEN BEGIN
      tt_font = 'DejaVuSans'
      font = 1
   ENDIF
   
   ; Save the current True-Type font before entering the PostScript device.
   ; Necessary for restoring it later.
   cgWindow_GetDefs, PS_TT_FONT=ps_tt_font
   ps_struct.tt_font_old = ps_tt_font
   
   ; Get the default font for PostScript output.
   IF N_Elements(font) EQ 0 THEN cgWindow_GetDefs, PS_FONT=font
   ps_struct.font = font
   
   ; Set up the true-type font for PostScript, if needed.
   IF (N_Elements(tt_font) EQ 0) AND (font EQ 1) THEN cgWindow_GetDefs, PS_TT_FONT=tt_font
   IF N_Elements(tt_font) NE 0 THEN BEGIN
        ps_struct.tt_font = tt_font
        font = 1
   ENDIF
   
   gui = Keyword_Set(gui)
   
   ; Get the default QUIET flag, if not set here.
   IF N_Elements(quiet) EQ 0 THEN cgWindow_GetDefs, PS_QUIET=quiet
   quiet = Keyword_Set(quiet)
   
   ; Get the default ENCAPSULATED flag, if not set here.
   IF N_Elements(encapsulated) EQ 0 THEN cgWindow_GetDefs, PS_ENCAPSULATED=encapsulated
   encapsulated = Keyword_Set(encapsulated)
   
   ; Handle keywords appropriately.
   SetDefaultValue, default_thickness, 3
   landscape = Keyword_Set(landscape)
   IF encapsulated THEN landscape = 0
   
   ; Get the default scale_factor.
   IF N_Elements(scale_factor) EQ 0 THEN cgWindow_GetDefs, PS_SCALE_FACTOR=scale_factor
   SetDefaultValue, scale_factor, 1.0

   ; If the setup flag is on, then we have to close the previous
   ; start command before we can continue.
   IF ps_struct.setup EQ 1 THEN cgPS_Close, /NoFix, /NoMessage
   
   ; Save current setup information in the PS_STRUCT structure.
   ps_struct.setup = 1
   ps_struct.currentDevice = !D.Name
   ps_struct.p = !P
   ps_struct.x = !X
   ps_struct.y = !Y
   ps_struct.z = !Z
   
   ; Change any parameters you feel like changing.
   IF ps_struct.p.thick EQ 0 THEN !P.Thick = default_thickness
   IF ps_struct.p.charthick EQ 0 THEN !P.Charthick = default_thickness
   IF ps_struct.x.thick EQ 0 THEN !X.Thick = default_thickness
   IF ps_struct.y.thick EQ 0 THEN !Y.Thick = default_thickness
   IF ps_struct.z.thick EQ 0 THEN !Z.Thick = default_thickness
   
   ; Set the true-type font.
   thisWindow = !D.Window
   IF thisWindow EQ -1 AND ((!D.Flags AND 256) NE 0) THEN BEGIN
        Window, /FREE, /PIXMAP
        pixmap = !D.Window
   ENDIF
   !P.Font = font 
   IF N_Elements(pixmap) NE 0 THEN WDelete, pixmap

   ; Configure the PostScript Device
   cancelled = 0
   IF ~Keyword_Set(nomatch) THEN BEGIN
      IF !D.X_Size GT !D.Y_Size THEN landscape = 1 ELSE landscape = 0
      IF Keyword_Set(encapsulated) THEN landscape = 0
      sizes = cgPSWindow(_Extra=extra, LANDSCAPE=landscape, /SANE_OFFSETS)
      keywords = cgPS_Config(_Strict_Extra=extra, INCHES=sizes.inches, XSIZE=sizes.xsize, YSIZE=sizes.ysize, $
         XOFFSET=sizes.xoffset, YOFFSET=sizes.yoffset, Cancel=cancelled, NOGUI=(~gui), $
         LANDSCAPE=sizes.landscape, ENCAPSULATED=encapsulated, FILENAME=ps_filename[0])
   ENDIF ELSE BEGIN
      keywords = cgPS_Config(_Strict_Extra=extra, ENCAPSULATED=encapsulated, $
          LANDSCAPE=landscape, CANCEL=cancelled, NOGUI=(~gui), FILENAME=ps_filename[0])
   ENDELSE
   IF cancelled THEN BEGIN
        cgPS_Close, /NoFix, /NoMessage
        RETURN
   ENDIF
   
   ; Let them know where the output will be.
   IF ~quiet THEN BEGIN
      IF print_ps_location THEN Print, 'PostScript output will be created here: ', keywords.filename
   ENDIF
   
   Set_Plot, 'PS'
   Device, _EXTRA=keywords, SCALE_FACTOR=scale_factor
   IF N_Elements(tt_font) NE 0 THEN Device, Set_Font=tt_font, /TT_Font
   
   ; Determine the character size.
   IF ps_struct.p.charsize EQ 0 THEN BEGIN
       IF N_Elements(charsize) EQ 0 THEN BEGIN
           !P.Charsize = cgDefCharsize(FONT=font)
       ENDIF ELSE !P.Charsize = charsize
   ENDIF ELSE BEGIN
       IF N_Elements(charsize) NE 0 THEN !P.Charsize = charsize
   ENDELSE

   ; Store filename and other pertinent information.
   ps_struct.filename = keywords.filename
   ps_struct.encapsulated = keywords.encapsulated
   ps_struct.landscape = Fix(keywords.landscape)
   ps_struct.pagetype = keywords.pagetype
   ps_struct.quiet = Fix(quiet)
 
END
