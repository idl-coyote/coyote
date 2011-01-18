;+
; NAME:
;   PS_START
;
; PURPOSE:
;
;    The purpose of PS_START and PS_END is to make it easy to set-up
;    for and close a PostScript file. The programs work in close conjunction
;    with PSCONFIG, another program from the Coyote Library.
;
;    If ImageMagick  (http://www.imagemagick.org/script/index.php) is installed 
;    on your computer, you can easily convert PostScript output to JPEG, PNG, and TIFF
;    image output. (See the keywords to PS_END.)
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;       Graphics, File Output, PostScript
;
; CALLING SEQUENCE:
;
;       PS_START
;       Various graphics commands here...
;       PS_END
;
; KEYWORD PARAMETERS FOR PS_START:
; 
;       CANCEL:       An output keyword that is set to 1 if the user cancelled from
;                     PS_Config. Otherwise, set to 0.
; 
;       CHARSIZE:     If this keyword is set, the !P.Charsize variable is set to this
;                     value until PS_END is called.
;                     
;       FONT:         Set this to the type of font you want. A -1 selects Hershey 
;                     fonts, a 0 selects hardware fonts (Helvetica, normally), and
;                     a 1 selects a True-Type font. Set to 0 by default.
;
;       GUI:          The default behavior is to use PSCONFIG to configure the
;                     PostScript device silently. If you wish to allow the user
;                     to interatively configure the PostScript device, set this
;                     keyword.
;                     
;       KEYWORDS:     This output keyword contains the keyword structure returned 
;                     from PS_Config.
;                     
;       NOMATCH:      Normally, PS_Start will try to "match" the aspect ratio of the
;                     PostScript file "window" to the current display window. If this
;                     keyword is set, then this doesn't occur, giving the user the
;                     option of specifying the size and offsets of the PostScript 
;                     window directly though appropriate keywords.
;
;       QUIET:        If set, informational messages are not set. 
;
;       SCALE_FACTOR: Set this to the PostScript scale factor. By default: 1.
;
;       TT_FONT:      The name of a true-type font to use if FONT=1. By default, "HELVETICA".
;
;       Any keyword supported by PSCONFIG can be used to configure the PostScript device.
;       Common keywords would include FILENAME, XSIZE, YSIZE, XOFFSET, YOFFSET, etc. See
;       the PSCONFIG documentation for details. If size keywords are used, you *must* set
;       the NOMATCH keyword.
;
; COMMON BLOCKS:
;
;       _$FSC_PS_START_   Contains the PS_STRUCT structure for communication between
;                         PS_START and PS_END.
;
; SIDE EFFECTS:
;
;       When PS_START is called, the current graphics device is set to "PS" (the PostScript 
;       device). When PS_END is called the current graphics device is returned to the device
;       in effect when PS_START was called.
;       
;       PS_Start uses the current display window as a template for the Postscript
;       file. Thus, if the display window is wider than it is higher, output is
;       in Landscape mode. To set the size of the PostScript "window" yourself, be
;       sure to set the NOMATCH keyword to 1.
;       
;       To display surface plots correctly the FONT keyword should be set to 1.
;       Otherwise, the default font is 0, or hardware fonts when outputting to 
;       PostScript.
;
; RESTRICTIONS:
;
;       Requires numerous programs from the Coyote Library. To convert PostScript files
;       to PNG, JPEG, GIF, and TIFF files requires ImageMagick be installed on your
;       computer and configured correctly. You can download Coyote Library programs here:
;
;             http://www.dfanning.com/programs/coyoteprograms.zip
;
;       ImageMagick can be found here:
;
;              http://www.imagemagick.org/script/index.php
;              
;       NOTE: In this version of PS_START, I have added the "alpha" option to the ImageMagick
;       command. This is a relatively recent addition to the convert command. (Added in ImageMagick
;       version 6.3.4). Your version of ImageMagick may not allow it. If so, simply comment out the
;       alpha command in the code below (on or about line 242), or uncomment it, as the case may be. 
;
; EXAMPLE:
;
;       To create a line plot in a PostScript file named lineplot.ps and
;       also create a PNG file named lineplot.png for display in a browser,
;       type these commands.
;
;       PS_Start, FILENAME='lineplot.ps'
;       Plot, Findgen(11), COLOR=FSC_Color('navy'), /NODATA, XTITLE='Time', YTITLE='Signal'
;       OPlot, Findgen(11), COLOR=FSC_Color('indian red')
;       OPlot, Findgen(11), COLOR=FSC_Color('olive'), PSYM=2
;       PS_End, /PNG
;
; NOTES:
;
;       You can easily configure any modifications you like for your PostScript output
;       by setting fields in the plot and axis system variables (!P, !X, !Y, and !Z).
;       The modifications currently made by default in this program are these:
;
;          !P.Thick = 2
;          !P.CharThick = 2
;          !X.Thick = 2
;          !Y.Thick = 2
;          !Z.Thick = 2
;          !P.Font = 0
;          
;        The !P.Charsize variable is set differently on Windows computers, and depending
;        on whether !P.MULTI is being used. On Windows the default is 1.25, or 1.00 for
;        multiple plots. On other computers, the default is 1.5, or 1.25 for multiple plots.
;        If true-type fonts are being used (FONT=1), the default is 1.5, or 1.25 for 
;        multiple plots.
;
; MODIFICATION HISTORY:
;
;       Written by: David W. Fanning, 20 May 2008.
;       Slight modification to allow filenames with spaces in them.
;       Added NoMatch keyword. 17 March 2009. DWF.
;       Added a number of keywords to make these commands more configurable. 19 March 2009. DWF.
;       Only set thickness system variables if starting system variables are set to their
;           default values (namely, 0). This allows users to set their own system variables
;           before they call PS_START, rather than after. 23 March  2009. DWF.
;       Moved PS_END to its own file to allow the IDLExBr_Assistant to work properly. 7 April 2009. DWF.
;       Modified to allow PostScript page type to be stored for future processing with FixPS. 9 August 2009. DWF.
;       Added NoFix keyword to PS_END calls to repair previous, but unused set-ups. 1 Nov 2010. DWF.
;       Added Charsize keyword to PS_START. 14 Nov 2010. DWF.
;       Changed the way default character sizes are set. 19 Nov 2010. DWF.
;       Added CANCEL and KEYWORDS output keywords. 16 Jan 2011. DWF.
;-
;
;******************************************************************************************;
;  Copyright (c) 2008 - 2010, by Fanning Software Consulting, Inc.                         ;
;  All rights reserved.                                                                    ;
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
PRO PS_START, $
    CANCEL=cancelled, $
    CHARSIZE=charsize, $
    FONT=font , $
    GUI=gui, $
    KEYWORDS=keywords, $
    NOMATCH=nomatch, $
    QUIET=quiet, $
    SCALE_FACTOR=scale_factor, $
    TT_FONT=tt_font, $
   _EXTRA=extra
 
   COMMON _$FSC_PS_START_, ps_struct
   
   ; PostScript hardware fonts by default.
   SetDefaultValue, font, 0
   
   ; Use Helvetica True-Type font by default.
   IF font EQ 1 THEN SetDefaultValue, tt_font, 'Helvetica'
   
   gui = Keyword_Set(gui)
   quiet = Keyword_Set(quiet)

   ; Define the PS structure.
   IF N_Elements(ps_struct) EQ 0 THEN ps_struct = {FSC_PS_SETUP}
   
   ; If the setup flag is on, then we have to close the previous
   ; start command before we can continue.
   IF ps_struct.setup EQ 1 THEN PS_END, /NoFix
   
   ; Save current setup information in the PS_STRUCT structure.
   ps_struct.setup = 1
   ps_struct.currentDevice = !D.Name
   ps_struct.p = !P
   ps_struct.x = !X
   ps_struct.y = !Y
   ps_struct.z = !Z
   
   ; Change any parameters you feel like changing.
   IF ps_struct.p.thick EQ 0 THEN !P.Thick = 2
   IF ps_struct.p.charthick EQ 0 THEN !P.Charthick = 2
   IF ps_struct.p.charsize EQ 0 THEN BEGIN
        IF N_Elements(charsize) EQ 0 THEN BEGIN
            !P.Charsize = FSC_DefCharsize(FONT=font)
        ENDIF ELSE !P.Charsize = charsize
   ENDIF ELSE BEGIN
        IF N_Elements(charsize) NE 0 THEN !P.Charsize = charsize
   ENDELSE
   IF ps_struct.x.thick EQ 0 THEN !X.Thick = 2
   IF ps_struct.y.thick EQ 0 THEN !Y.Thick = 2
   IF ps_struct.z.thick EQ 0 THEN !Z.Thick = 2
   
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
      sizes = PSWindow(_Extra=extra, LANDSCAPE=landscape)
      keywords = PSConfig(_Extra=extra, INCHES=sizes.inches, XSIZE=sizes.xsize, YSIZE=sizes.ysize, $
         XOFFSET=sizes.xoffset, YOFFSET=sizes.yoffset, Cancel=cancelled, NOGUI=(~gui), LANDSCAPE=sizes.landscape)
   ENDIF ELSE BEGIN
      keywords = PSConfig(_Extra=extra, Cancel=cancelled, NOGUI=(~gui))
   ENDELSE
   IF cancelled THEN BEGIN
        PS_END, /NoFix
        RETURN
   ENDIF

   ; Let them know where the output will be.
   IF ~quiet THEN Print, 'PostScript output will be created here: ', keywords.filename
   
   Set_Plot, 'PS'
   Device, _EXTRA=keywords
   
   ; What about scale factor?
   IF N_Elements(scale_factor) NE 0 THEN $
        DEVICE, SCALE_FACTOR=scale_factor ELSE $
        DEVICE, SCALE_FACTOR=1
   
   ; Store filename.
   ps_struct.filename = keywords.filename
   ps_struct.landscape = Fix(keywords.landscape)
   ps_struct.pagetype = keywords.pagetype
   ps_struct.quiet = Fix(quiet)
   
END
