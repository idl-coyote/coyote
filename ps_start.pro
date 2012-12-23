; docformat = 'rst'
;
; NAME:
;   PS_START
;
; PURPOSE:
;    The purpose of PS_START and PS_END is to make it easy to set-up
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
; The purpose of PS_START and PS_END is to make it easy to set-up
; and close a PostScript file. These programs are used extensively
; in all Coyote Graphics routines.
;
; If `ImageMagick  <http://www.imagemagick.org/script/index.php>` is installed 
; on your computer, you can easily convert PostScript output to GIF, JPEG, PNG, and TIFF
; raster output. If `Ghostscript <http://www.ghostscript.com/download/>` is installed
; you can convert PostScript output to PDF files. See the appropriate keywords to
; PS_END.
; 
; When PS_START is called, the current graphics device is set to "PS" (the PostScript 
; device). When PS_END is called the current graphics device is returned to the device
; in effect when PS_START was called.
; 
; PS_Start uses the current display window as a template for the Postscript
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
; The PS_Start program contains the common block, _$FSC_PS_START_. See the FSC_PS_SETUP__DEFINE 
; program in the Coyote Library for its definition.
; 
; :Categories:
;    Utilities, Graphics
;    
; :Params:
;     filename: in, optional, type=string, default='idl.ps'
;        The name of the PostScript file created.
;    
; :Keywords:
;     cancel: out, optional, type=boolean, default=0
;         An output keyword that is set to 1 if the user cancelled from
;         PS_Config. Otherwise, set to 0.
;     charsize: in, optional, type=float
;         If this keyword is set, the !P.Charsize variable is set to this value until PS_END is called.
;     default_thickness: in, optional, type=integer, default=3
;         Sets the following system variables to this value while creating PostScript output:
;         !P.Thick, !P.CharThick, !X.Thick, !Y.Thick, !Z.Thick. These variables are returned to
;         their original values by `PS_End`. A system variable is set to this value only if it 
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
;         The default behavior is to use PSCONFIG to configure the PostScript device silently. 
;         If you wish to allow the user to interatively configure the PostScript device, set this
;         keyword.
;     keywords: out, optional, type=structure                
;         This output keyword contains the keyword structure returned from PS_Config.
;     landscape: in, optional, type=boolean, default=0
;         Set this keyword to produce landscape PostScript output.
;     nomatch: in, optional, type=boolean, default=0                
;         Normally, PS_Start will try to "match" the aspect ratio of the PostScript file "window" 
;         to the current display window. If this keyword is set, then this doesn't occur, giving 
;         the user the option of specifying the size and offsets of the PostScript window directly 
;         though appropriate keywords.
;     quiet: in, optional, type=boolean, default=0
;         If set, informational messages are not set. 
;     scale_factor: in, optional, type=float, default=1.0
;         Set this to the PostScript scale factor. By default: 1.
;     tt_font: in, optional, type=string, default="Helvetica"
;         The name of a true-type font to use if FONT=1.
;     _ref_extra: in, optional
;         Any keyword appropriate for the PostScript configuration program PSConfig, from
;         the Coyote Library can be used with PS_Start.
;              
; :Examples:
;    To create a line plot in a PostScript file named lineplot.ps and
;    also create a PNG file named lineplot.png for display in a browser,
;    type these commands::
;
;        PS_Start, FILENAME='lineplot.ps'
;        cgPlot, Findgen(11), COLOR='navy', /NODATA, XTITLE='Time', YTITLE='Signal'
;        cgPlot, Findgen(11), COLOR='indian red', /OVERPLOT
;        cgPlot, Findgen(11), COLOR='olive', PSYM=2, /OVERPLOT
;        PS_End, /PNG
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
;       
; :Copyright:
;     Copyright (c) 2008-2012, Fanning Software Consulting, Inc.
;-
PRO PS_START, filename, $
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
   
   ; Handle the filename parameter and keywords.
   IF N_Elements(filename) EQ 0 THEN filename = 'idl.ps'
   IF N_Elements(ps_filename) EQ 0 THEN ps_filename = filename 
      
   ; Need DejaVuSans fonts?
   IF Keyword_Set(dejavusans) && (Float(!Version.Release) GE 8.2) THEN BEGIN
      tt_font = 'DejaVuSans'
      font = 1
   ENDIF
   
   ; Define the PS structure.
   IF N_Elements(ps_struct) EQ 0 THEN ps_struct = {FSC_PS_SETUP}
   
   ; PostScript hardware fonts by default.
   SetDefaultValue, font, 0
   ps_struct.font = font
   
   ; Use Helvetica True-Type font by default.
   SetDefaultValue, tt_font, 'Helvetica'
   ps_struct.tt_font = tt_font
   
   gui = Keyword_Set(gui)
   quiet = Keyword_Set(quiet)
   
   ; Handle encapsulated and landscape keywords appropriately.
   SetDefaultValue, default_thickness, 3
   encapsulated = Keyword_Set(encapsulated)
   landscape = Keyword_Set(landscape)
   IF encapsulated THEN landscape = 0
   SetDefaultValue, scale_factor, 1

   ; If the setup flag is on, then we have to close the previous
   ; start command before we can continue.
   IF ps_struct.setup EQ 1 THEN PS_END, /NoFix, /NoMessage
   
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
   IF ps_struct.p.charsize EQ 0 THEN BEGIN
        IF N_Elements(charsize) EQ 0 THEN BEGIN
            !P.Charsize = cgDefCharsize(FONT=font)
        ENDIF ELSE !P.Charsize = charsize
   ENDIF ELSE BEGIN
        IF N_Elements(charsize) NE 0 THEN !P.Charsize = charsize
   ENDELSE
   IF ps_struct.x.thick EQ 0 THEN !X.Thick = default_thickness
   IF ps_struct.y.thick EQ 0 THEN !Y.Thick = default_thickness
   IF ps_struct.z.thick EQ 0 THEN !Z.Thick = default_thickness
   
   ; Set the true-type font.
   thisWindow = !D.Window
   IF thisWindow EQ -1 AND ((!D.Flags AND 256) NE 0)THEN BEGIN
        Window, /FREE, /PIXMAP
        pixmap = !D.Window
   ENDIF
   !P.Font = font 
   Device, Set_Font=tt_font, /TT_FONT
   IF N_Elements(pixmap) NE 0 THEN WDelete, pixmap

   ; Configure the PostScript Device
   cancelled = 0
   IF ~Keyword_Set(nomatch) THEN BEGIN
      IF !D.X_Size GT !D.Y_Size THEN landscape = 1 ELSE landscape = 0
      IF Keyword_Set(encapsulated) THEN landscape = 0
      sizes = PSWindow(_Extra=extra, LANDSCAPE=landscape)
      keywords = PSConfig(_Strict_Extra=extra, INCHES=sizes.inches, XSIZE=sizes.xsize, YSIZE=sizes.ysize, $
         XOFFSET=sizes.xoffset, YOFFSET=sizes.yoffset, Cancel=cancelled, NOGUI=(~gui), $
         LANDSCAPE=sizes.landscape, ENCAPSULATED=encapsulated, FILENAME=ps_filename[0])
   ENDIF ELSE BEGIN
      keywords = PSConfig(_Strict_Extra=extra, ENCAPSULATED=encapsulated, $
          LANDSCAPE=landscape, CANCEL=cancelled, NOGUI=(~gui), FILENAME=ps_filename[0])
   ENDELSE
   IF cancelled THEN BEGIN
        PS_END, /NoFix, /NoMessage
        RETURN
   ENDIF
   
   ; Let them know where the output will be.
   IF ~quiet THEN Print, 'PostScript output will be created here: ', keywords.filename
   
   Set_Plot, 'PS'
   Device, Set_Font=tt_font, /TT_FONT
   Device, _EXTRA=keywords, SCALE_FACTOR=scale_factor
   
   ; Store filename and other pertinent information.
   ps_struct.filename = keywords.filename
   ps_struct.encapsulated = keywords.encapsulated
   ps_struct.landscape = Fix(keywords.landscape)
   ps_struct.pagetype = keywords.pagetype
   ps_struct.quiet = Fix(quiet)
   
END
