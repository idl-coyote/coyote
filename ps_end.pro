; docformat = 'rst'
;
; NAME:
;   PS_END
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
; you can convert PostScript output to PDF files. See the appropriate keywords below.
; 
; When PS_START is called, the current graphics device is set to "PS" (the PostScript 
; device). When PS_END is called the current graphics device is returned to the device
; in effect when PS_START was called.

; :Categories:
;    Utilities, Graphics
;    
; :Keywords:
;     allow_transparent: in, optional, type=boolean, default=0
;         To make the background of some image files white, rather than transparent,
;         you have to set the "-alpha off" string in the ImageMagick call. This
;         string is automatically added to the ImageMagick call unless this keyword
;         is set, in which case the string is not added and the image background will
;         be transparent.  
;     delete_ps: in, optional, type=boolean, default=0            
;        Setting this keyword will delete the PostScript file that is used as the intermediate
;        file in the conversion to other file types.
;     density: in, optional, type=integer, default=300
;        The horizontal and vertical density (in dots per inch, DPI) of the image when the PostScript file
;        is converted to a raster format by ImageMagick. 
;     gif: in, optional, type=boolean, default=0                 
;        Set this keyword to convert the PostScript output file to a GIF image. Requires ImageMagick.
;     im_options: in, optional, type=string, default=""
;        A string of ImageMagick "convert" options that can be passed to the ImageMagick convert 
;        command. No error checking occurs with this string.
;     jpeg: in, optional, type=boolean, default=0                 
;        Set this keyword to convert the PostScript output file to a JPEG image. Requires ImageMagick.
;     nofix: in, optional, type=boolean, default=0  
;        If this keyword is set, then the FixPS program to fix IDL landscape
;        PostScript files is not called.
;     nomessage: in, optional, type=boolean, default=0                  
;        If this keyword is set, then no error messages are issued. The keyword is used primarily 
;        to allow PS_END to reset the internal structure without a lot of ruckus.  
;     outfilename: out, optional, type=string
;        The name of the output filename created by the program.             
;     pdf: in, optional, type=boolean, default=0                 
;        Set this keyword to convert the PostScript output file to a PDF file. Requires Ghostscript.
;     png: in, optional, type=boolean, default=0                 
;        Set this keyword to convert the PostScript output file to a PNG image. Requires ImageMagick.
;     resize: in, optional, type=integer, default=25
;        If an image is being created from the PostScript file, it is often resized by some 
;        amount. You can use this keyword to change the value (e.g, RESIZE=100).
;        The value is passed on to resize argument as a percentage in the ImageMagick call.
;     showcmd: in, optional, type=boolean, default=0
;        Set this command to show the command used to do any PostScript coversions.
;     tiff: in, optional, type=boolean, default=0                 
;        Set this keyword to convert the PostScript output file to a TIFF image. Requires ImageMagick.
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
;       Reordered ImageMagick commands to put them in the proper sequence to get "alpha" switch to work. 23 April 2009. DWF.
;       Put the switches *ahead* of the PostScript file name. Now resizing works and default size reduction
;           returned to 25%. 23 April 2009. DWF.
;       Still having a devil of a time getting the ImageMagick "convert" command right. Fonts
;           have become a problem. Now trying a "flatten" option in the command. 12 May 2009. DWF.
;       If the PostScript file is in Landscape mode, it is now "fixed" with FixPS to allow it
;           to be displayed right-side up in PostScript viewers. 8 August 2009. DWF.
;       Fixed a problem in not checking the GIF keyword properly. 4 December 2009. DWF.
;       Added NOFIX keyword to the program. 1 November 2010. DWF.
;       Added better handing of errors coming from FIXPS after update to FIXPS. 15 November 2010. DWF.
;       Added DELETE_PS keyword. 16 Jan 2011. DWF.
;       Better protection of code from not finding ImageMagick. 17 Jan 2011. DWF.
;       Collecting result of SPAWN command. Only printing if QUIET=0. 16 Feb 2011. DWF.
;       Changes to handle inability to create raster files from PS encapsulated files in 
;           landscape mode. Added NOMESSAGE keyword. 26 Aug 2011. DWF.
;        Added PDF keyword. Requires Ghostscript to use. 6 Dec 2011. DWF.
;        Added SHOWCMD keyword. 9 Dec 2011. DWF.
;        Added OUTFILENAME keyword. 11 Dec 2011. DWF.
;
; :Copyright:
;     Copyright (c) 2008-2011, Fanning Software Consulting, Inc.
;-
PRO PS_END, $
    ALLOW_TRANSPARENT=allow_transparent, $
    BMP=bmp, $
    DELETE_PS=delete_ps, $
    DENSITY=density, $
    IM_OPTIONS=im_options, $
    GIF=gif, $
    JPEG=jpeg, $
    NOFIX=nofix, $
    NOMESSAGE=nomessage, $
    OUTFILENAME=outfilename, $
    PDF=pdf, $
    PNG=png, $
    RESIZE=resize, $
    SHOWCMD=showcmd, $
    TIFF=tiff
            

   COMMON _$FSC_PS_START_, ps_struct
   
   Catch, theError
   IF theError NE 0 THEN BEGIN
   
       Catch, /CANCEL
   
       ; Issue an error message, unless messages are turned off.
       IF ~Keyword_Set(nomessage) THEN void = Error_Message()
   
       ; Clean up.
       IF ps_struct.currentDevice NE "" THEN Set_Plot, ps_struct.currentDevice
       !P = ps_struct.p
       !X = ps_struct.x
       !Y = ps_struct.y
       !Z = ps_struct.z
       ps_struct.setup = 0
       ps_struct.currentDevice = ""
       ps_struct.filename = ""
       ps_struct.convert = ""
       
       RETURN
       
   ENDIF
   
   ; Close the PostScript file, if this is PostScript device.
   IF !D.Name EQ 'PS' THEN Device, /CLOSE_FILE
   ps_filename = ps_struct.filename
   outfilename = ps_filename
   showcmd = Keyword_Set(showcmd)
   
   ; If the file is in landscape mode, then fix it so that the plot
   ; is right-side up.
   IF ps_struct.landscape THEN BEGIN
        IF ~Keyword_Set(nofix) THEN BEGIN
            FixPS, ps_struct.filename, PAGETYPE=ps_struct.pagetype, SUCCESS=success, QUIET=1
            IF success EQ 0 THEN Print, 'Encountered problem fixing landscape PostScript file. Proceeding...'
        ENDIF
   ENDIF
   
   ; Need to convert with ImageMagick?
   allow_transparent = Keyword_Set(allow_transparent)
   IF Keyword_Set(bmp) THEN ps_struct.convert = 'BMP'
   IF Keyword_Set(gif) THEN ps_struct.convert = 'GIF'
   IF Keyword_Set(pdf) THEN ps_struct.convert = 'PDF'
   IF Keyword_Set(png) THEN ps_struct.convert = 'PNG'
   IF Keyword_Set(jpeg) THEN ps_struct.convert = 'JPEG'
   IF Keyword_Set(tiff) THEN ps_struct.convert = 'TIFF'
   SetDefaultValue, density, 300
   SetDefaultValue, resize, 25
   
   IF ps_struct.convert NE "" THEN BEGIN

        basename = FSC_Base_Filename(ps_struct.filename, DIRECTORY=theDir, EXTENSION=theExtension)
        CASE 1 OF
            ps_struct.convert EQ 'GIF':  outfilename = Filepath(ROOT_DIR=theDir, basename + '.gif')
            ps_struct.convert EQ 'PDF':  outfilename = Filepath(ROOT_DIR=theDir, basename + '.pdf')
            ps_struct.convert EQ 'PNG':  outfilename = Filepath(ROOT_DIR=theDir, basename + '.png')
            ps_struct.convert EQ 'JPEG': outfilename = Filepath(ROOT_DIR=theDir, basename + '.jpg')
            ps_struct.convert EQ 'TIFF': outfilename = Filepath(ROOT_DIR=theDir, basename + '.tif')
        ENDCASE
        IF N_Elements(outfilename) NE "" THEN BEGIN
        
            ; ImageMagick is required for this section of the code.
            available = HasImageMagick(Version=version)
            IF available && ~(ps_struct.convert EQ 'PDF') THEN BEGIN
            
                ; Cannot successfully convert encapsulated landscape file to raster.
                ; Limitation of ImageMagick, and specifically, GhostScript, which does
                ; the conversion. 
                IF  ps_struct.encapsulated &&  ps_struct.landscape THEN BEGIN
                    Message, 'ImageMagick cannot convert an encapsulated ' + $
                             'PostScript file in landscape mode to a raster file. '
                ENDIF
                
                vp = StrSplit(version, '.', /EXTRACT)
                vp[2] = StrMid(vp[2],0,1)
                version_number = Fix(vp[0]) * 100 + Fix(vp[1])*10 + vp[2]
                IF version_number GT 634 THEN allowAlphaCmd = 1 ELSE allowAlphaCmd = 0
            
                ; Set up for various ImageMagick convert options.
                IF allowAlphaCmd THEN alpha_cmd =  allow_transparent ? '' : ' -alpha off' 
                density_cmd = ' -density ' + StrTrim(density,2)
                resize_cmd =  ' -resize '+ StrCompress(resize, /REMOVE_ALL)+'%'
                
                ; Start ImageMagick convert command.
                cmd = 'convert'
                
                ; Add various command options.
                IF N_Elements(alpha_cmd) NE 0 THEN cmd = cmd + alpha_cmd
                IF N_Elements(density_cmd) NE 0 THEN cmd = cmd + density_cmd
                
                 ; Add the input filename.
                cmd = cmd +  ' "' + ps_struct.filename + '"' 
                
                IF N_Elements(resize_cmd) NE 0 THEN cmd = cmd + resize_cmd
                cmd = cmd +  ' -flatten '
                IF N_Elements(im_options) NE 0 THEN BEGIN
                    IF StrMid(im_options, 0, 1) NE " " THEN im_options = " " + im_options
                    cmd = cmd + im_options
                ENDIF
                
                ; If the landscape mode is set, rotate by 90 to allow the 
                ; resulting file to be in landscape mode.
                IF ps_struct.landscape THEN cmd = cmd + ' -rotate 90'
                
                ; Add the output filename. Make sure PNG files are 24-bit images.
                IF ps_struct.convert EQ 'PNG' $
                    THEN cmd = cmd + ' "' + 'PNG24:' +outfilename + '"' $
                    ELSE cmd = cmd + ' "' + outfilename + '"'
                IF ~ps_struct.quiet THEN BEGIN
                    IF showcmd THEN Print, 'ImageMagick CONVERT command: ',  cmd
                ENDIF
                SPAWN, cmd, result, err_result
                
                IF ~ps_struct.quiet THEN BEGIN
                    IF N_Elements(result) NE 0 THEN BEGIN
                        FOR k=0,N_Elements(result)-1 DO Print, result[k]
                    ENDIF
                    IF N_Elements(err_result) NE 0 THEN BEGIN
                        FOR k=0,N_Elements(err_result)-1 DO Print, err_result[k]
                    ENDIF ELSE Print, 'Output file located here: + outfilename
                ENDIF
                
                ; Have you been asked to delete the PostScript file?
                IF Keyword_Set(delete_ps) THEN BEGIN
                    IF outfilename NE ps_filename THEN File_Delete, ps_filename
                ENDIF
            ENDIF ELSE BEGIN
                IF ps_struct.convert EQ 'PDF' THEN BEGIN
                    cgPS2PDF, ps_struct.filename, outfilename, $
                        DELETE_PS=delete_ps, PAGETYPE=ps_struct.pagetype, SILENT=1, SUCCESS=success
                    IF success THEN BEGIN
                       IF ~ps_struct.quiet THEN Print, 'PDF Output File: ' + outfilename
                    ENDIF ELSE Print, 'Encountered problem creating PDF file. Proceeding...'
                ENDIF ELSE BEGIN
                    Message, 'ImageMagick could not be found. No conversion to raster was possible.'
                ENDELSE
            ENDELSE
        ENDIF
        
   ENDIF
   
   ; Clean up.
   IF ps_struct.currentDevice NE "" THEN Set_Plot, ps_struct.currentDevice
   !P = ps_struct.p
   !X = ps_struct.x
   !Y = ps_struct.y
   !Z = ps_struct.z
   ps_struct.setup = 0
   ps_struct.currentDevice = ""
   ps_struct.filename = ""
   ps_struct.convert = ""

END ;---------------------------------------------------------------



