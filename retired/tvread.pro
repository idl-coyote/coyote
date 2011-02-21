;+
; NAME:
;       TVREAD
;
; PURPOSE:
;
;       To get accurate screen dumps with the IDL command TVRD on 24-bit
;       PC and Macintosh computers, you have to be sure to set color
;       decomposition on. This program adds that capability automatically.
;       In addition, the program will optionally write BMP, GIF, JPEG,
;       PICT, PNG, and TIFF color image files of the screen dump.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Graphics
;
; CALLING SEQUENCE:
;
;       image = TVREAD(xstart, ystart, ncols, nrows)
;
;       The returned image will be a 2D image on 8-bit systems and
;       a 24-bit pixel interleaved true-color image on 24-bit systems.
;       A -1 will be returned if a file output keyword is used (e.g., JPEG, TIFF, etc.).
;
; OPTIONAL INPUTS:
;
;       XSTART -- The starting column index.  By default, 0.
;
;       YSTART -- The starting row index. By default, 0.
;
;       NCOLS -- The number of columns to read. By default, !D.X_Size - XSTART
;
;       NROWS -- The number of rows to read. By default, !D.Y_Size - YSTART.
;
; KEYWORD PARAMETERS:
;
;       BMP -- Set this keyword to write the screen dump as a color BMP file.
;
;       CANCEL -- An output keyword set to 1 if the user cancels out of a
;          filename dialog. Set to 0 otherwise.
;
;       COLORS -- If a 24-bit image has to be quantized, this will set the number
;          of colors in the output image. Set to 256 by default. Applies to BMP,
;          GIF, PICT, and PNG formats written from 24-bit displays.(See the
;          COLOR_QUAN documentation for details.)
;
;       CUBE -- If this keyword is set to a value between 2 and 6 the color
;          quantization will use a cubic method of quantization. Applies to BMP,
;          GIF, PICT, and PNG formats written from 24-bit displays.(See the
;          COLOR_QUAN documentation for details.)
;
;       DITHER -- If this keyword is set the quantized image will be dithered.
;          Applies to BMP, GIF, PICT, and PNG formats written from 24-bit displays.
;          (See the COLOR_QUAN documentation for details.)
;
;       FILENAME -- The base name of the output file. (No file extensions;
;           they will be added automatically.) This name may be changed by the user.
;
;              image = TVREAD(Filename='myfile', /JPEG)
;
;           No file will be written unless a file output keyword is used
;           (e.g., JPEG, TIFF, etc.) in the call. By default the FILENAME is
;           set to "idl". The file extension will be set automatically to match
;           the type of file created.
;
;       GIF -- Set this keyword to write the screen dump as a color GIF file.
;
;       JPEG -- Set this keyword to write the screen dump as a color JPEG file.
;
;       NODIALOG -- Set this keyword if you wish to avoid the DIALOG_PICKFILE
;           dialog that asks you to name the output file. This keyword should be
;           set, for example, if you are processing screens in batch mode.
;
;       ORDER -- Set this keyword to determine the image order for reading the
;           display. Corresponds to !Order and set to such as the default.
;           
;       OVERWRITE_PROMPT -- Set this keyword if you would like to get a prompt
;           if you are overwriting a file. This applies only to operations with
;           DIALOG_PICKFILE.
;
;       PICT -- Set this keyword to write the screen dump as a color PICT file.
;
;       PNG -- Set this keyword to write the screen dump as a color PNG file.
;
;       TIFF -- Set this keyword to write the screen dump as a color TIFF file.
;
;       TRUE -- Set this keyword to the type of interleaving you want. 1 = Pixel
;           interleaved, 2 = row interleaved, 3 = band interleaved.
;
;       TYPE -- Can be set to the type of file to write. Use this instead of
;           setting BMP, GIF, JPEG, PICT, PNG, or TIFF keywords: TYPE='JPEG'. The
;           primary purpose of this is to make event handlers easier to write.
;
;       QUALITY -- This keyword sets the amount of compression for JPEG images.
;           It should be set to a value between 0 and 100. It is set to 75 by default.
;           (See the WRITE_JPEG documentation for details.)
;
;       WID -- The index number of the window to read from. The current graphics window
;           (!D.Window) is selected by default. An error is issued if no windows are
;           currently open on a device that supports windows.
;
;       _EXTRA -- Any keywords that are appropriate for the WRITE_*** routines are
;           also accepted via keyword inheritance.
;
; COMMON BLOCKS:
;
;       None
;
; RESTRICTIONS:   Requires IDL 5.2 and higher.
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 9 AUG 2000.
;       Added changes to make the program more device independent. 16 SEP 2000. DWF.
;       Removed GIF file support for IDL 5.4 and above. 18 JAN 2001. DWF.
;       Added NODIALOG keyword. 28 MAR 2001. DWF.
;       Added an output CANCEL keyword. 29 AUG 2001. DWF.
;       Added ERROR_MESSAGE code to file. 17 DEC 2001. DWF.
;       Added ORDER keyword. 25 March 2002. DWF.
;       Now create 24-bit PNG files if reading from a 24-bit display. 11 May 2002. DWF.
;       Now create 24-bit BMP files if reading from a 24-bit display. 23 May 2002. DWF.
;       Removed obsolete STR_SEP and replaced with STRSPLIT. 27 Oct 2004. DWF.
;       Unleashed the GIF code for IDL 6.2 and above. 10 Nov 2005. DWF.
;       Rolled back previous change to IDL 6.1. 24 Jan 2006. DWF.
;       Fixed a problem in which 16-bit displays were treated as 24-bit displays,
;         and as a result could not produce WHITE colors. Now all 16-bit displays
;         are treated as 8-bit displays. It is an ugly trade-off. 24 Jan 2006. DWF.
;       Added TYPE keyword 27 Sep 2006. DWF.
;       Updated program to work with 24-bit Z-buffer in IDL 6.4. 11 June 2007. DWF.
;       Added OVERWRITE_PROMPT keyword. 2 Oct 2008. DWF.
;       Found an extra line of code that wrote PNG files twice! Removed. 10 Nov 2010. DWF.
;       Replaced TVREAD_ERROR_MESSAGE with ERROR_MESSAGE. 25 Nov 2010. DWF.
;-
;
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
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
FUNCTION TVREAD, xstart, ystart, ncols, nrows, $
   BMP=bmp, $
   Cancel=cancel, $
   Colors=colors, $
   Cube=cube, $
   Dither=dither, $
   _Extra=extra, $
   Filename=filename, $
   GIF=gif, $
   JPEG=jpeg, $
   NoDialog=nodialog, $
   Order=order, $
   Overwrite_Prompt=overwrite_prompt, $
   PICT=pict, $
   PNG=png, $
   TIFF=tiff, $
   True=true, $
   TYPE=type, $
   Quality=quality, $
   WID=wid

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message()
   IF N_Elements(thisWindow) EQ 0 THEN RETURN, -1
   IF thisWindow GE 0 THEN WSet, thisWindow
   RETURN, -1
ENDIF

cancel = 0

   ; Check for availability of GIF files.

thisVersion = Float(!Version.Release)
IF (thisVersion LT 5.3) OR (thisVersion GE 6.1) THEN haveGif = 1 ELSE haveGIF = 0

   ; Go to correct window.

IF N_Elements(wid) EQ 0 THEN wid =!D.Window
thisWindow = !D.Window
IF (!D.Flags AND 256) NE 0 THEN WSet, wid

   ; Check keywords and parameters. Define values if necessary.

IF N_Elements(xstart) EQ 0 THEN xstart = 0
IF N_Elements(ystart) EQ 0 THEN ystart = 0
IF N_Elements(ncols) EQ 0 THEN ncols = !D.X_Size - xstart
IF N_Elements(nrows) EQ 0 THEN nrows = !D.Y_Size - ystart
IF N_Elements(order) EQ 0 THEN order = !Order
IF N_Elements(true) EQ 0 THEN true = 1
dialog = 1 - Keyword_Set(nodialog)

   ; Do you want to write an image file instead of
   ; capturing an image?
IF N_Elements(type) NE 0 THEN BEGIN
   CASE StrUpCase(type) OF
      'BMP': bmp = 1
      'GIF': gif = 1
      'JPEG': jpeg = 1
      'JPG': jpeg = 1
      'PICT': pict = 1
      'PNG': png = 1
      'TIFF': tiff = 1
      'TIF': tif = 1
      ELSE: Message, 'Cannot write a file of type: ' + StrUpCase(type) + '.'
   ENDCASE
ENDIF
writeImage = 0
fileType = ""
extention = ""
IF Keyword_Set(bmp)THEN BEGIN
   writeImage = 1
   fileType = 'BMP'
   extension = 'bmp'
ENDIF
IF Keyword_Set(gif) THEN BEGIN
   IF havegif THEN BEGIN
      writeImage = 1
      fileType = 'GIF'
      extension = 'gif'
    ENDIF ELSE BEGIN
       ok = Dialog_Message('GIF files not supported in this IDL version. Replacing with JPEG.')
       writeImage = 1
      fileType = 'JPEG'
      extension = 'jpg'
   ENDELSE
ENDIF
IF Keyword_Set(jpeg) THEN BEGIN
   writeImage = 1
   fileType = 'JPEG'
   extension = 'jpg'
ENDIF
IF Keyword_Set(PICT) THEN BEGIN
   writeImage = 1
   fileType = 'PICT'
   extension = 'pict'
ENDIF
IF Keyword_Set(png) THEN BEGIN
   writeImage = 1
   fileType = 'PNG'
   extension = 'png'
ENDIF
IF Keyword_Set(tiff) THEN BEGIN
   writeImage = 1
   fileType = 'TIFF'
   extension = 'tif'
ENDIF

IF N_Elements(colors) EQ 0 THEN colors = 256
IF N_Elements(quality) EQ 0 THEN quality = 75
dither = Keyword_Set(dither)

   ; On 24-bit displays, make sure color decomposition is ON.

IF (!D.Flags AND 256) NE 0 THEN BEGIN
   Device, Get_Decomposed=theDecomposedState, Get_Visual_Depth=theDepth
   IF theDepth GT 8 THEN BEGIN
      Device, Decomposed=1
      IF theDepth EQ 24 THEN truecolor = true ELSE truecolor = 0
   ENDIF ELSE truecolor = 0
   IF thisWindow LT 0 THEN $
      Message, 'No currently open windows. Returning.', /NoName
ENDIF ELSE BEGIN
   truecolor = 0
   theDepth = 8
ENDELSE

; Fix for 24-bit Z-buffer.
IF (Float(!Version.Release) GE 6.4) AND (!D.NAME EQ 'Z') THEN BEGIN
   Device, Get_Decomposed=theDecomposedState, Get_Pixel_Depth=theDepth
   IF theDepth EQ 24 THEN truecolor = true ELSE truecolor = 0
ENDIF


   ; Get the screen dump. 2D image on 8-bit displays. 3D image on 24-bit displays.

image = TVRD(xstart, ystart, ncols, nrows, True=truecolor, Order=order)

   ; Need to set color decomposition back?

IF theDepth GT 8 THEN Device, Decomposed=theDecomposedState

   ; If we need to write an image, do it here.

IF writeImage THEN BEGIN

      ; Get the name of the output file.

   IF N_Elements(filename) EQ 0 THEN BEGIN
      filename = 'idl.' + StrLowCase(extension)
   ENDIF ELSE BEGIN
      filename = filename + "." + StrLowCase(extension)
   ENDELSE
   IF dialog THEN filename = Dialog_Pickfile(/Write, File=filename, OVERWRITE_PROMPT=Keyword_Set(overwrite_prompt))

   IF filename EQ "" THEN BEGIN
      cancel = 1
      RETURN, image
   ENDIF

      ; Write the file.

   CASE fileType OF

      'BMP': BEGIN
         IF truecolor THEN BEGIN
            ; BMP files assume blue, green, red planes.
            temp = image[0,*,*]
            image[0,*,*] = image[2,*,*]
            image[2,*,*] = temp
            Write_BMP, filename, image, _Extra=extra
         ENDIF ELSE BEGIN
            TVLCT, r, g, b, /Get
            Write_BMP, filename, image, r, g, b, _Extra=extra
         ENDELSE
         END

      'GIF': BEGIN
         IF truecolor THEN BEGIN
            CASE Keyword_Set(cube) OF
               0: image2D = Color_Quan(image, 1, r, g, b, Colors=colors, Dither=dither)
               1: image2D = Color_Quan(image, 1, r, g, b, Cube=2 > cube < 6)
            ENDCASE
         ENDIF ELSE BEGIN
            TVLCT, r, g, b, /Get
            image2D = image
         ENDELSE
         Write_GIF, filename, image2D, r, g, b, _Extra=extra
         END

      'JPEG': BEGIN
         IF truecolor THEN BEGIN
            image3D = image
         ENDIF ELSE BEGIN
            s = Size(image, /Dimensions)
            image3D = BytArr(3, s[0], s[1])
            TVLCT, r, g, b, /Get
            image3D[0,*,*] = r[image]
            image3D[1,*,*] = g[image]
            image3D[2,*,*] = b[image]
         ENDELSE
         Write_JPEG, filename, image3D, True=1, Quality=quality, _Extra=extra
         END

      'PICT': BEGIN
         IF truecolor THEN BEGIN
            CASE Keyword_Set(cube) OF
               0: image2D = Color_Quan(image, 1, r, g, b, Colors=colors, Dither=dither)
               1: image2D = Color_Quan(image, 1, r, g, b, Cube=2 > cube < 6)
            ENDCASE
         ENDIF ELSE BEGIN
            TVLCT, r, g, b, /Get
            image2D = image
         ENDELSE
         Write_PICT, filename, image2D, r, g, b
         END

      'PNG': BEGIN
         IF truecolor THEN BEGIN
            Write_PNG, filename, image, _Extra=extra
         ENDIF ELSE BEGIN
            TVLCT, r, g, b, /Get
            image2D = image
            Write_PNG, filename, image2D, r, g, b, _Extra=extra
         ENDELSE
         END

      'TIFF': BEGIN
         IF truecolor THEN BEGIN
            image3D = Reverse(image,3)
         ENDIF ELSE BEGIN
            s = Size(image, /Dimensions)
            image3D = BytArr(3, s[0], s[1])
            TVLCT, r, g, b, /Get
            image3D[0,*,*] = r[image]
            image3D[1,*,*] = g[image]
            image3D[2,*,*] = b[image]
            image3D = Reverse(Temporary(image3D), 3)
         ENDELSE
         Write_TIFF, filename, image3D, 1, _Extra=extra
         END
   ENDCASE
   RETURN, -1
ENDIF

   ; Return the screen dump image.

RETURN, image
END ;-------------------------------------------------------------------------------