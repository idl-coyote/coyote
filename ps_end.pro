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
;     bmp: in, optional, type=boolean, default=0                 
;        Set this keyword to convert the PostScript output file to a BMP image. Requires ImageMagick.
;     delete_ps: in, optional, type=boolean, default=0            
;        Setting this keyword will delete the PostScript file that is used as the intermediate
;        file in the conversion to other file types.
;     density: in, optional, type=integer, default=300
;        The horizontal and vertical density (in dots per inch, DPI) of the image when the PostScript file
;        is converted to a raster format by ImageMagick. 
;     filetype: in, optional, type='string', default=""
;        This keyword provides a generic way of setting the `BMP`, `GIF`, `JPEG`, `PNG`, and `TIFF` 
;        keywords. Set this keyword to the type of file output desired, and the correct "output"
;        keyword will be set.
;     gif: in, optional, type=boolean, default=0                 
;        Set this keyword to convert the PostScript output file to a GIF image. Requires ImageMagick.
;     gs_path: in, optional, type=string
;        This program assumes that UNIX users can access Ghostscript with the "gs"
;        command. It assumes WINDOWS users have installed Ghostscript in either
;        the C:\gs or C:\Program Files\gs directories. If either of these assumptions
;        is incorrect, you can specify the directory where the Ghostscript executable
;        resides with this keyword. (The Windows 32-bit executable is named gswin32c.exe
;        and the 64-bit executable is named gswin64c.exe.) Passed directly to cgPS2PDF.
;     im_options: in, optional, type=string, default=""
;        A string of ImageMagick "convert" options that can be passed to the ImageMagick convert 
;        command. No error checking occurs with this string.
;     jpeg: in, optional, type=boolean, default=0                 
;        Set this keyword to convert the PostScript output file to a JPEG image. Requires ImageMagick.
;     nofix: in, optional, type=boolean, default=0  
;        If this keyword is set, then the cgFixPS program to fix IDL landscape
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
;        Normally, 24-bit PNG files are created. However, if the IM_PNG8 keyword is set with
;        cgWindow_SetDefs, then PS_End will create an 8-bit PNG file instead.
;     resize: in, optional, type=integer, default=25
;        If an image is being created from the PostScript file, it is often resized by some 
;        amount. You can use this keyword to change the value (e.g, RESIZE=100).
;        The value is passed on to resize argument as a percentage in the ImageMagick call.
;     showcmd: in, optional, type=boolean, default=0
;        Set this command to show the command used to do any PostScript coversions.
;     tiff: in, optional, type=boolean, default=0                 
;        Set this keyword to convert the PostScript output file to a TIFF image. Requires ImageMagick.
;     unix_convert_cmd: in, optional, type=string
;         There are a number of commands on UNIX machines for converting PostScript files
;         to PDF files. This program assumes you are using Ghostscript to do the conversion
;         for you. The Ghostscript command on most UNIX machines is "gs", which is used if
;         this keyword is undefined. However, if you would prefer to use another program to do
;         the conversion for you, you can specify the name of the command here. For example,
;         "pstopdf" or "epstopdf". In creating the actual command, this command will be
;         separated by a space from the input file name. In other words, if the alternative
;         conversion command was "pstopdf", the actual command would be "pstopdf" + " " + ps_file.
;         Any output filename is ignored. This command does not apply to Macintosh or Windows 
;         computers. Passed directly to cgPS2PDF.
;     width: in, optional, type=integer
;         Set the keyword to the final pixel width of the output raster image. Applies
;         only to raster image file output (e.g., JPEG, PNG, TIFF, etc.). The height of
;         the image is chosen to preserve the image aspect ratio.
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
;       If the PostScript file is in Landscape mode, it is now "fixed" with cgFixPS to allow it
;           to be displayed right-side up in PostScript viewers. 8 August 2009. DWF.
;       Fixed a problem in not checking the GIF keyword properly. 4 December 2009. DWF.
;       Added NOFIX keyword to the program. 1 November 2010. DWF.
;       Added better handing of errors coming from cgFixPS after update to FIXPS. 15 November 2010. DWF.
;       Added DELETE_PS keyword. 16 Jan 2011. DWF.
;       Better protection of code from not finding ImageMagick. 17 Jan 2011. DWF.
;       Collecting result of SPAWN command. Only printing if QUIET=0. 16 Feb 2011. DWF.
;       Changes to handle inability to create raster files from PS encapsulated files in 
;           landscape mode. Added NOMESSAGE keyword. 26 Aug 2011. DWF.
;        Added PDF keyword. Requires Ghostscript to use. 6 Dec 2011. DWF.
;        Added SHOWCMD keyword. 9 Dec 2011. DWF.
;        Added OUTFILENAME keyword. 11 Dec 2011. DWF.
;        Just realized a BMP case is missing from one of the CASE statements. 12 Dec 2011. DWF.
;        Added GS_PATH and UNIX_CONVERT_CMD keywords to support PDF output. 14 Dec 2011. DWF.
;        Add the WIDTH keyword. 3 April 2012. DWF.
;        Added a check for IM_PNG8 keyword, using cgWindow_GetDefs to see if an 8-bit or 24-bit
;           PNG file should be created. 3 April 2012. DWF.
;        Modified the ImageMagick commands that resizes the image to a particular width. Necessary
;           to accommodate PNG8 file output. Using ImageMagick 6.7.2-9. 4 April 2012. DWF.
;        Added FILETYPE keyword to provide a generic way of creating raster file output. 30 August 2012. DWF.
;        Modified to use the cgPS2Raster program so code doesn't have to be maintained in 
;            two places. 15 Oct 2012. DWF.
;        Added a check for ImageMagick and an informational message for raster operations. 4 Nov 2012. DWF.
;        Fixed a problem in which the NOMESSAGE keyword was not getting passed along to cgPS2Raster. 5 Nov 2012. DWF.
;        Fixed a problem where I was not passing the PORTRAIT keyword to cgPS2Raster properly. 22 Jan 2013. DWF.
;
; :Copyright:
;     Copyright (c) 2008-2012, Fanning Software Consulting, Inc.
;-
PRO PS_END, $
    ALLOW_TRANSPARENT=allow_transparent, $
    BMP=bmp, $
    DELETE_PS=delete_ps, $
    DENSITY=density, $
    IM_OPTIONS=im_options, $
    FILETYPE=filetype, $
    GIF=gif, $
    GS_PATH=gs_path, $
    JPEG=jpeg, $
    NOFIX=nofix, $
    NOMESSAGE=nomessage, $
    OUTFILENAME=outfilename, $
    PDF=pdf, $
    PNG=png, $
    RESIZE=resize, $
    SHOWCMD=showcmd, $
    TIFF=tiff, $
    UNIX_CONVERT_CMD=unix_convert_cmd, $
    WIDTH=width
            

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
   xsize = !D.X_Size
   ysize = !D.Y_Size
   ps_filename = ps_struct.filename
   showcmd = Keyword_Set(showcmd)
   
   ; If the file is in landscape mode, then fix it so that the plot
   ; is right-side up.
   IF ps_struct.landscape THEN BEGIN
        IF ~Keyword_Set(nofix) THEN BEGIN
            cgFixPS, ps_struct.filename, PAGETYPE=ps_struct.pagetype, SUCCESS=success, QUIET=1
            IF success EQ 0 THEN Print, 'Encountered problem fixing landscape PostScript file. Proceeding...'
        ENDIF
        portrait = 0
   ENDIF ELSE portrait = 1
   
   ; Need to convert the PostScript to a raster file?
   needRaster = 0
   allow_transparent = Keyword_Set(allow_transparent)
   IF N_Elements(filetype) EQ 0 THEN filetype = ""
   CASE StrUpCase(filetype) OF
       'BMP': bmp = 1
       'GIF': gif = 1
       'PDF': pdf = 1
       'PNG': png = 1
       'JPEG': jpeg = 1
       'JPG': jpeg = 1
       'TIFF': tiff = 1
       'TIF': tiff = 1
       "": 
       ELSE: BEGIN
           void = Dialog_Message('File type ' + StrUpCase(filetype) + ' invalid. No raster created.')
           needRaster = 0
           END
   ENDCASE
   IF Keyword_Set(bmp) THEN needRaster = 1
   IF Keyword_Set(gif) THEN needRaster = 1
   IF Keyword_Set(pdf) THEN needRaster = 1
   IF Keyword_Set(png) THEN needRaster = 1
   IF Keyword_Set(jpeg) THEN needRaster = 1
   IF Keyword_Set(tiff) THEN needRaster = 1
   SetDefaultValue, density, 300
   SetDefaultValue, resize, 25
   IF needRaster THEN BEGIN
       IF cgHasImageMagick() THEN BEGIN
       cgPS2Raster, ps_filename, raster_filename, $
          ALLOW_TRANSPARENT=allow_transparent, $
          BMP=bmp, $
          DELETE_PS=delete_ps, $
          DENSITY=density, $
          IM_OPTIONS=im_options, $
          FILETYPE=filetype, $
          GIF=gif, $
          JPEG=jpeg, $
          OUTFILENAME=outfilename, $
          PDF=pdf, $
          PNG=png, $
          PORTRAIT=portrait, $
          RESIZE=resize, $
          SHOWCMD=showcmd, $
          SILENT=Keyword_Set(nomessage), $
          SUCCESS=success, $
          TIFF=tiff, $
          WIDTH=width
       ENDIF ELSE BEGIN
           Print, ''
           Print, 'Message from the PS_End Program:'
           Print, '   The requested raster operation cannot be completed unless ImageMagick is installed.'
           delete_ps = 0
           Print, '   The requested PostScript file has been saved: ' + ps_filename + '.'
           Print, '   Please see http://www.idlcoyote.com/graphics_tips/weboutput.php for details'
           Print, '   about converting PostScript intermediate files to raster files via ImageMagick.
           void = Dialog_Message('PS_End: ImageMagick must be installed to complete raster operation.')
       ENDELSE
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



