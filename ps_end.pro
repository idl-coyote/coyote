;+
; NAME:
;   PS_END
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
; KEYWORD PARAMETERS FOR PS_END:
;
;      All keywords for PS_END require that ImageMagick is installed on your computer
;      and is configured correctly. Image conversion is done by spawning a "convert" 
;      command to ImageMagick.
;
;       ALLOW_TRANSPARENT: To make the background of some image files white, rather than transparent,
;                     you have to set the "-alpha off" string in the ImageMagick call. This
;                     string is automatically added to the ImageMagick call unless this keyword
;                     is set, in which case the string is not added and the image background will
;                     be transparent.  (See RESTRICTIONS note below for more information.) 
;
;       DENSITY:      The horizontal and vertical density of the image when the PostScript file
;                     is converted to an image format by ImageMagick. By default, 300. Use this
;                     keyword to set another value.
;                     
;       GIF:          Set this keyword to convert the PostScript output file to a GIF image.
;
;       JPEG:         Set this keyword to convert the PostScript output file to a JPEG image.
;       
;       NOFIX:        If this keyword is set, then the FixPS program to fix IDL landscape
;                     PostScript files is not called.
;
;       PNG:          Set this keyword to convert the PostScript output file to a PNG image.
;
;       IM_OPTIONS:   A string of ImageMagick "convert" options that can be passed to
;                     the  ImageMagick convert command. By default, a null string. No
;                     error checking occurs with this string.
;
;       RESIZE:       If an image is being created from the PostScript file, it is often 
;                     resized by some amount. The default value is 25 percent. You can use 
;                     this keyword to change the value to some other amount (e.g, RESIZE=100).
;                     The value is passed on to resize argument as a percentage in the 
;                     ImageMagick call.
;
;       TIFF:         Set this keyword to convert the PostScript output file to a TIFF image.
;       
;       The convert command looks like this. You can modify it in the code if you want it to
;       do something else for you.
;       
;          convert inputPostScriptFilename -resize 50% -density 300 -alpha off outputImageFilename 
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
; RESTRICTIONS:
;
;       Requires numerous programs from the Coyote Library. To convert PostScript files
;       to PNG, JPEG, and TIFF files requires ImageMagick be installed on your
;       computer and configured correctly. You can download Coyote Library programs here:
;
;             http://www.dfanning.com/programs/coyoteprograms.zip
;
;       ImageMagick can be found here:
;
;              http://www.imagemagick.org/script/index.php
;              
;       NOTE: In this version of PS_END, I have added the "alpha" option to the ImageMagick
;       command. This is a relatively recent addition to the convert command. (Added in ImageMagick
;       version 6.3.4). I believe PS_END can discover which version of ImageMagick you are useing
;       and act accordingly, but if it can't, set this alpha option to your liking. 
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
;          !X.Thick = 2
;          !Y.Thick = 2
;          !Z.Thick = 2
;          !P.Font = 1
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
;       Reordered ImageMagick commands to put them in the proper sequence to get "alpha" switch to work. 23 April 2009. DWF.
;       Put the switches *ahead* of the PostScript file name. Now resizing works and default size reduction
;           returned to 25%. 23 April 2009. DWF.
;       Still having a devil of a time getting the ImageMagick "convert" command right. Fonts
;           have become a problem. Now trying a "flatten" option in the command. 12 May 2009. DWF.
;       If the PostScript file is in Landscape mode, it is now "fixed" with FixPS to allow it
;           to be displayed right-side up in PostScript viewers. 8 August 2009. DWF.
;       Fixed a problem in not checking the GIF keyword properly. 4 December 2009. DWF.
;       Added NOFIX keyword to the program. 1 November 2010. DWF.
;- 
;
;******************************************************************************************;
;  Copyright (c) 2008-2010, by Fanning Software Consulting, Inc.                           ;
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
PRO PS_END, $
    ALLOW_TRANSPARENT=allow_transparent, $
    DENSITY=density, $
    IM_OPTIONS=im_options, $
    GIF=gif, $
    JPEG=jpeg, $
    NOFIX=nofix, $
    PNG=png, $
    RESIZE=resize, $
    TIFF=tiff
            

   COMMON _$FSC_PS_START_, ps_struct
   
   ON_ERROR, 2 ; Return to caller.
   
   ; Close the PostScript file, if this is PostScript device.
   IF !D.Name EQ 'PS' THEN Device, /CLOSE_FILE
   
   ; If the file is in landscape mode, then fix it so that the plot
   ; is right-side up.
   IF ps_struct.landscape THEN BEGIN
        IF ~Keyword_Set(nofix) THEN FixPS, ps_struct.filename, PAGETYPE=ps_struct.pagetype
   ENDIF
   
   ; Need to convert with ImageMagick?
   allow_transparent = Keyword_Set(allow_transparent)
   IF Keyword_Set(gif) THEN ps_struct.convert = 'GIF'
   IF Keyword_Set(png) THEN ps_struct.convert = 'PNG'
   IF Keyword_Set(jpeg) THEN ps_struct.convert = 'JPEG'
   IF Keyword_Set(tiff) THEN ps_struct.convert = 'TIFF'
   SetDefaultValue, density, 300
   SetDefaultValue, resize, 25
   
   IF ps_struct.convert NE "" THEN BEGIN

        basename = FSC_Base_Filename(ps_struct.filename, DIRECTORY=theDir, EXTENSION=theExtension)
        CASE 1 OF
            ps_struct.convert EQ 'GIF':  outfilename = Filepath(ROOT_DIR=theDir, basename + '.gif')
            ps_struct.convert EQ 'PNG':  outfilename = Filepath(ROOT_DIR=theDir, basename + '.png')
            ps_struct.convert EQ 'JPEG': outfilename = Filepath(ROOT_DIR=theDir, basename + '.jpg')
            ps_struct.convert EQ 'TIFF': outfilename = Filepath(ROOT_DIR=theDir, basename + '.tif')
        ENDCASE
        IF N_Elements(outfilename) NE "" THEN BEGIN
        
            ; Find out what version of ImageMagick you are using.
            Spawn, 'convert -version', result
            parts = StrSplit(result[0], /EXTRACT)
            version = parts[2]
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
            IF ~ps_struct.quiet THEN Print, 'ImageMagick CONVERT command: ',  cmd
            SPAWN, cmd
        ENDIF
        
   ENDIF
   
   ; Clean up.
   Set_Plot, ps_struct.currentDevice
   !P = ps_struct.p
   !X = ps_struct.x
   !Y = ps_struct.y
   !Z = ps_struct.z
   ps_struct.setup = 0
   ps_struct.currentDevice = ""
   ps_struct.filename = ""
   ps_struct.convert = ""

END ;---------------------------------------------------------------



