; docformat = 'rst'
;
; NAME:
;   cgPS2Raster
;
; PURPOSE:
;    The purpose of this program is to convert a PostScript file to a high
;    resolution raster file, using the ImageMagick convert command to do the
;    conversion.
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
; The purpose of this program is to convert a PostScript file to a high
; resolution raster file, using the ImageMagick convert command to do the
; conversion. `ImageMagick  <http://www.imagemagick.org/script/index.php>` must 
; be installed on your computer.
; 
; Note that one restriction the ImageMagick convert command imposes is that it
; cannot convert encapsulated PostScript files that are in landscape mode to
; raster files. These raster files will be clipped at one end of the file. If you
; wish to do the conversion properly, make sure encapsulated landscape plots are
; in portrait mode.
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
; 
; :Categories:
;    Utilities, Graphics
;    
; :Params:
;     ps_filename: in, required, type=string
;         The name of the input PostScript file that is being converted to a raster file.
;         If not provided, the user will be asked to select a file.
;     raster_filename: in, required, type=string
;         The name of the output raster file that is being converted from the PostScript file.
;         If not provided, the output filename is created from the input PostScript file name.
;         Note, this is the preferred way to create the output filename, since the OUTPUTNAME
;         keyword has been depreciated as an input keyword.
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
;     im_options: in, optional, type=string, default=""
;        A string of ImageMagick "convert" options that can be passed to the ImageMagick convert 
;        command. No error checking occurs with this string.
;     jpeg: in, optional, type=boolean, default=0  
;        Set this keyword to convert the PostScript output file to a JPEG image. Requires ImageMagick.
;     outfilename: out, optional, type=string
;        On exit, the name of the output file that was created.
;     pdf: in, optional, type=boolean, default=0                 
;        Set this keyword to convert the PostScript output file to a PDF file. Requires Ghostscript.
;     png: in, optional, type=boolean, default=0                 
;        Set this keyword to convert the PostScript output file to a PNG image. Requires ImageMagick.
;     portrait: in, optional, type=boolean, default=0
;        Set this keyword to indicate the PostScript file is in portrait mode. Otherwise, landscape
;        mode is assumed.    
;     resize: in, optional, type=integer, default=25
;        If an image is being created from the PostScript file, it is often resized by some 
;        amount. You can use this keyword to change the value (e.g, RESIZE=100).
;        The value is passed on to resize argument as a percentage in the ImageMagick call.
;     showcmd: in, optional, type=boolean, default=0
;        Set this command to show the command used to do any PostScript coversions.
;     silent: in, optional, type=boolean, default=0
;        Set this keyword to suppress output from the file. Is this keyword is set, be
;        sure to check the `Success` keyword on exit.
;     success: out, optional, type=boolean
;         On exit, this keyword is set to 1 if the program successfully managed to create a 
;         raster file. It will be set to 0 otherwise.
;     tiff: in, optional, type=boolean, default=0                 
;        Set this keyword to convert the PostScript output file to a TIFF image. Requires ImageMagick.
;     width: in, optional, type=integer
;        Set this keyword to set the resulting width of the raster file. The height of the
;        raster will be such as to preserve the aspect ratio of the starting image.
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
;       Written by: David W. Fanning, 12 December 2011
;       Added the ability to check to see if 8-bit or 24-bit PNG files should be created. 3 April 2012. DWF.
;       Modified the ImageMagick commands that resizes the image to a particular width. Necessary
;          to accommodate PNG8 file output. Using ImageMagick 6.7.2-9. 4 April 2012. DWF.
;       Added FILETYPE keyword. 13 October 2012. DWF.
;       Apparently Macs can't handle the version number, so I have removed the version number
;           check for Macs. 13 October 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
PRO cgPS2Raster, ps_filename, raster_filename, $
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
    SILENT=silent, $
    SUCCESS=success, $
    TIFF=tiff, $
    WIDTH=width

   Compile_Opt idl2
   
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      IF ~Keyword_Set(silent) THEN void = Error_Message()
      success = 0
      RETURN
   ENDIF

   ; Assume failure. Sigh...
   success = 0
   
   ; Need a file?
   IF N_Elements(ps_filename) EQ 0 THEN BEGIN
      ps_filename = cgPickfile(Filter=['*.ps','*.eps'], Title='Select a PostScript file.')
      IF ps_filename EQ "" THEN RETURN
   ENDIF

   ; ImageMagick parameters. Assume we will convert to PNG file, unless told otherwise
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
           success = 0
           RETURN
           END
   ENDCASE
   IF Keyword_Set(pdf) THEN filetype = 'PDF'
   IF Keyword_Set(bmp) THEN filetype = 'BMP'
   IF Keyword_Set(gif) THEN filetype = 'GIF'
   IF Keyword_Set(png) THEN filetype = 'PNG'
   IF Keyword_Set(jpeg) THEN filetype = 'JPEG'
   IF Keyword_Set(tiff) THEN filetype = 'TIFF'
   SetDefaultValue, allow_transparent, 0, /BOOLEAN
   SetDefaultValue, density, 300
   SetDefaultValue, portrait, 0, /BOOLEAN
   SetDefaultValue, resize, 25
   SetDefaultValue, showcmd, 0, /BOOLEAN
   SetDefaultValue, silent, 0, /BOOLEAN
   SetDefaultValue, width, 0
   IF portrait THEN BEGIN
      xsize = 11
      ysize = 8.5
   ENDIF ELSE BEGIN
      xsize = 8.5
      ysize = 11
   ENDELSE
   
   ; Construct an output filename, if needed. First, look to see if the raster_filename
   ; positional parameter is being used to specify the output filename.
   IF (N_Elements(raster_filename) NE 0) && (N_Elements(outfilename) EQ 0) THEN BEGIN
       outfilename = raster_filename
   ENDIF
   basename = cgRootName(ps_filename, DIRECTORY=theDir, EXTENSION=theExtension)
   IF theDir EQ "" THEN CD, CURRENT=theDir
   IF N_Elements(outfilename) EQ 0 THEN BEGIN
       CASE 1 OF
          filetype EQ 'BMP':  outfilename = Filepath(ROOT_DIR=theDir, basename + '.bmp')
          filetype EQ 'GIF':  outfilename = Filepath(ROOT_DIR=theDir, basename + '.gif')
          filetype EQ 'JPEG': outfilename = Filepath(ROOT_DIR=theDir, basename + '.jpg')
          filetype EQ 'PDF':  outfilename = Filepath(ROOT_DIR=theDir, basename + '.pdf')
          filetype EQ 'PNG':  outfilename = Filepath(ROOT_DIR=theDir, basename + '.png')
          filetype EQ 'TIFF': outfilename = Filepath(ROOT_DIR=theDir, basename + '.tif')
       ENDCASE
   ENDIF
   
   ; Make sure the outfilename is an absolute file path.
   IF File_BaseName(outfilename) EQ outfilename THEN outfilename = Filepath(ROOT_DIR=theDir, outfilename)
   
   ; If you have an output filename, then do it.
   IF N_Elements(outfilename) NE "" THEN BEGIN
   
      ; PDF files handled a bit differently.
      IF filetype EQ 'PDF' THEN BEGIN
          cgPS2PDF, ps_filename, outfilename, DELETE_PS=delete_ps, SHOWCMD=showcmd, $
              SILENT=silent, SUCCESS=success
          RETURN
      ENDIF
        
      ; ImageMagick is required for this section of the code.
      IF StrUpCase(!Version.OS) EQ 'DARWIN' THEN BEGIN
          available = cgHasImageMagick()
          doVersionTest = 0
      ENDIF ELSE BEGIN
          available = cgHasImageMagick(Version=version)
          doVersionTest = 1
      ENDELSE
      IF available THEN BEGIN
            
          ; Some functionality depends on the ImageMagick version. Check the version here.
          IF doVersionTest THEN BEGIN
             vp = StrSplit(version, '.', /EXTRACT)
             vp[2] = StrMid(vp[2],0,1)
             version_number = Fix(vp[0]) * 100 + Fix(vp[1])*10 + vp[2]
             IF version_number GT 634 THEN allowAlphaCmd = 1 ELSE allowAlphaCmd = 0
          ENDIF ELSE allowAlphaCmd = 1
            
          ; Set up for various ImageMagick convert options.
          IF allowAlphaCmd THEN alpha_cmd =  allow_transparent ? '' : ' -alpha off' 
          density_cmd = ' -density ' + StrTrim(density,2)
          
          ; Normally, we just resize by a precentage, unless a specific width
          ; has been specified.
          IF (N_Elements(width) EQ 0) || (width LE 0) THEN BEGIN
               resize_cmd =  ' -resize '+ StrCompress(resize, /REMOVE_ALL)+'%'
           ENDIF ELSE BEGIN
                
           ; Getting the width correct has to be done in an extremely non-intuitive way.
           ; I wonder if this will be changed in different versions of ImageMagick?
           height = Ceil(width*ysize/Float(xsize))
           IF ~portrait THEN BEGIN
                resize_cmd = ' -resize ' + StrCompress(height, /REMOVE_ALL) + 'x' + StrCompress(width, /REMOVE_ALL)                   
           ENDIF ELSE BEGIN
                resize_cmd = ' -resize ' + StrCompress(width, /REMOVE_ALL) + 'x' + StrCompress(height, /REMOVE_ALL)
                     ENDELSE
           ENDELSE
                
          ; Start ImageMagick convert command.
          cmd = 'convert'
                
          ; Add various command options.
          IF N_Elements(alpha_cmd) NE 0 THEN cmd = cmd + alpha_cmd
          IF N_Elements(density_cmd) NE 0 THEN cmd = cmd + density_cmd
                
          ; Add the input filename.
          cmd = cmd +  ' "' + ps_filename + '"' 
                
          IF N_Elements(resize_cmd) NE 0 THEN cmd = cmd + resize_cmd
          cmd = cmd +  ' -flatten '
          IF N_Elements(im_options) NE 0 THEN BEGIN
              IF StrMid(im_options, 0, 1) NE " " THEN im_options = " " + im_options
              cmd = cmd + im_options
          ENDIF
                
          ; If in landscape mode, rotate by 90 to allow the 
          ; resulting file to be in landscape mode.
          IF (1-portrait) THEN cmd = cmd + ' -rotate 90'
                
                ; Add the output filename and check for PNG output.
                IF Keyword_Set(png) THEN BEGIN
                
                    ; Check to see whether 8-bit or 24-bit PNG files should be created.
                    cgWindow_GetDefs, IM_PNG8=png8
                    IF png8 THEN BEGIN
                       cmd = cmd + ' "' + 'PNG8:' +outfilename + '"' 
                    ENDIF ELSE BEGIN
                       cmd = cmd + ' "' + 'PNG24:' +outfilename + '"' 
                    ENDELSE
                ENDIF ELSE cmd = cmd + ' "' + outfilename + '"'
          IF ~silent THEN BEGIN
              IF showcmd THEN Print, 'ImageMagick CONVERT command: ',  cmd
          ENDIF
          SPAWN, cmd, result, err_result
                
          IF ~silent THEN BEGIN
              IF err_result[0] NE "" THEN BEGIN
                  FOR k=0,N_Elements(err_result)-1 DO Print, err_result[k]
              ENDIF ELSE Print, 'Output file located here: ' + outfilename
          ENDIF
                
          ; Have you been asked to delete the PostScript file?
          IF Keyword_Set(delete_ps) THEN BEGIN
              IF outfilename NE ps_filename THEN File_Delete, ps_filename
          ENDIF
      ENDIF ELSE Message, 'ImageMagick cannot be found on this machine.'
      
   ENDIF
        
END