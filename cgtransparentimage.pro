; docformat = 'rst'
;
; NAME:
;   cgTransparentImage
;
; PURPOSE:
; Creates a semi-transparent image for display. Optionally, a  "missing" 
; color or color index (for 2D images) can be set to a completely transparent value.
; The transparent image can be saved as a transparent PNG file.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2012, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; Creates a semi-transparent image for display. Optionally, a  "missing" 
; color or color index (for 2D images) can be set to a completely transparent value.
; The transparent image can be saved as a transparent PNG file.
;
; :Categories:
;    Graphics
;    
; :Params:
;    image: in, optional, type=byte
;       A 2D image or a 24-bit image with or without an alpha channel. If an alpha
;       channel is present, it will be modified by the program.
;       
; :Keywords:
;     brewer: in, optional, type=boolean, default=0
;         This keyword is used only if the `CTIndex` keyword is used to select a color table number.
;         Setting this keyword allows Brewer color tables to be used.
;         
;     ctindex: in, optional, type=integer
;         The index number of a color table. The `Brewer` and `Reverse` keywords will be checked
;         to see how to load the color table into the `Palette` keyword. This keyword will take
;         precidence over any colors that are loaded with the `Palette` keyword. 
;         
;     missing_value: in, optional, type=various
;        The "color" of a pixel that will be treated as a "missing" color or value.
;        Any pixels in the image with this color value will be set completely
;        transparent. If `Color` is a string, use cgColor to obtain a color triple. 
;        If `Color` is a non-strint scalar, this value is taken to be the missing color index
;        in a 2D image. Otherwise, this is assumed to be a color triple that indicates
;        the "missing" color or value in the output image. The alpha channel in the output image
;        is set to 0 for the "missing" color, which makes this value completely transparent.
;        
;     filename: in, optional, type=string
;         The name of an image file that can be read with READ_IMAGE. Used to
;         select the input image if the `image` parameter is not used.
;        
;     nogui: in, optional, type=boolean, default=0
;         Set this keyword if you do not wish to have the user confirm the name of
;         the output PNG file selected with the `PNGFile` keyword.
;         
;     palette: in, optional, type=byte
;         Set this keyword to a 3x256 or 256x3 byte array containing the RGB color 
;         vectors to be loaded before the transparent image is created. Such vectors can be 
;         obtained, for example, from cgLoadCT with the RGB_TABLE keyword::
;               
;                IDL> cgLoadCT, 4, /BREWER, /REVERSE, RGB_TABLE=palette
;                IDL> tImage = cgTransparentImage( cgDemoData(7), PALETTE=palette)
;                
;         The default is to use whatever colors are loaded in the current hardware color table.
;         A palette applies only to 2D input images.
;         
;     pngfile: in, optional
;         If this keyword is set, the output image is written as a transparent
;         PNG file in 4xMxN format. Optionally, this keyword can be set to the
;         name of the output PNG file. The user will have the option of confirming
;         the name of the output file, unless the `NoGUI` keyword is set.
;         
;     reverse: in, optional, type=boolean, default=0
;         Set this keyword to reverse the color table vectors selected with the `CTIndex` keyword.

;     transparent: in, optional, type=integer, default=50
;         The percentage of transparency desired in the output image. A number 
;         between 0 and 100.
;        
;     wset: in, optional, type=long
;        If image parameter is not present, make the window indicated with
;        this keyword the current graphics window for obtaining an image parameter.
;        If not passed, and no image is present, then the current graphics window is 
;        used to create the image.
;
; :Examples:
;    To create and display a transparent image::
;       cgDisplay, WID=0
;       cgImage, cgDemoData(5), CTIndex=0, /Interp
;       timage = cgTransparentImage(MISSING_VALUE='black', TRANSPARENT=50)
;       cgDisplay, WID=1
;       cgImage, cgDemoData(7), CTIndex=22
;       cgImage, timage
;
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 23 October 2012 to replace Make_Transparent_Image, which was retired. DWF.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgTransparentImage, image, $
  BREWER=brewer, $
  CTINDEX=ctindex, $
  MISSING_VALUE=missing_value, $
  FILENAME=filename, $
  NOGUI=nogui, $
  PALETTE=palette, $
  PNGFILE=pngfile, $
  REVERSE=reverse, $
  TRANSPARENT=transparent, $
  WSET=wset
  
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN 
        Catch, /CANCEL
        void = Error_Message()
        RETURN, image
    ENDIF
    
    ; Check for keyword values.
    IF N_Elements(transparent) EQ 0 THEN transparent = 50
    nogui = Keyword_Set(nogui)

    ; Make sure the transparent value is between 0 and 100 initially, and between 0 and 1 finally.
    transparent = (0 > transparent < 100) / 100.0
        
    ; Did the user pass an image? If not, read an image from the display.
    IF N_Elements(image) EQ 0 THEN BEGIN
    
        ; Did the user specify a filename?
        IF N_Elements(filename) NE 0 THEN BEGIN
            image = Read_Image(filename, r, g, b)
            IF N_Elements(r) NE 0 THEN palette = [[r],[g],[b]]
        ENDIF 
        IF N_Elements(image) EQ 0 THEN BEGIN
            IF (!D.Flags AND 256) EQ 0 THEN Message, 'Current device does not support windows.'
            IF N_Elements(wset) NE 0 THEN BEGIN
                thisWindow = !D.Window
                WSet, wset
            ENDIF
            image = cgSnapshot(TRUE=3)
            IF N_Elements(thisWindow) NE 0 THEN WSet, thisWindow
        ENDIF
    ENDIF
    
    ; What kind of image are we dealing with here?
    ndims = Size(image, /N_DIMENSIONS)
    CASE ndims OF
        2: BEGIN

           ; Use a color table to create a color palette?
           IF N_Elements(ctindex) NE 0 THEN BEGIN
              cgLoadCT, ctindex, Reverse=reverse, Brewer=brewer, RGB_TABLE=palette
           ENDIF
 
           IF N_Elements(palette) NE 0 THEN BEGIN
              IF (Size(palette, /DIMENSIONS))[0] EQ 3 THEN BEGIN
                 r = Reform(palette[0,*])
                 g = Reform(palette[1,*])
                 b = Reform(palette[2,*])
              ENDIF ELSE BEGIN
                 r = palette[*,0]
                 g = palette[*,1]
                 b = palette[*,2]
              ENDELSE
           ENDIF ELSE BEGIN
              TVLCT, r, g, b, /Get
           ENDELSE
           red = r[image]
           grn = g[image]
           blu = b[image]
           alpha = Byte(image) * 0 + (255 * (1.0 - transparent))
           
           ; Handle missing color index here.
           IF (N_Elements(missing_value) EQ 1) && (Size(missing_value, /TNAME) NE 'STRING') THEN BEGIN
              missingIndices = Where(image EQ missing_value, missingCnt)
              IF missingCnt GT 0 THEN alpha[missingIndices] = 0B
           ENDIF
           END
           
        3: BEGIN
           s = Size(image, /DIMENSIONS)
           index = Where(s EQ 3, count)
           IF count GT 0 THEN BEGIN
                CASE index OF
                   0: aImage = Transpose(image, [1,2,0])
                   1: aImage = Transpose(image, [0,2,1])
                   ELSE: aImage = image
                ENDCASE
                red = aImage[*,*,0]
                grn = aImage[*,*,1]
                blu = aImage[*,*,2]
                s = Size(aImage, /DIMENSIONS)
                alpha = BytArr(s[0], s[1]) + (255 * (1.0 - transparent))
           ENDIF
           index = Where(s EQ 4, count)
           IF count GT 0 THEN BEGIN
                CASE index OF
                   0: aImage = Transpose(image, [1,2,0])
                   1: aImage = Transpose(image, [0,2,1])
                   ELSE: aImage = image
                ENDCASE
                red = aImage[*,*,0]
                grn = aImage[*,*,1]
                blu = aImage[*,*,2]
                s = Size(aImage, /DIMENSIONS)
                alpha = BytArr(s[0], s[1]) + (255 * (1.0 - transparent))    
           ENDIF
           END
                      
        ELSE: Message, 'Input variable does not appear to be an image.'
    ENDCASE
    
    ; Did the user pass a color? 
    CASE N_Elements(missing_value) OF
        0: 
        1: BEGIN
           IF Size(missing_value, /TNAME) EQ 'STRING' THEN BEGIN
               missingColor = cgColor(missing_value, /TRIPLE)
           ENDIF
           END
        2: Message, 'The COLOR keyword contains unexpected values.'
        3: missingColor = missing_value
        ELSE: Message, 'The COLOR keyword must be a color triple.'
    ENDCASE
    
    ; Find the missing color in the image, if it is defined and this is not a 2D image.
    IF (N_Elements(missingColor) NE 0) && (Size(image, /N_DIMENSIONS) NE 2) THEN BEGIN
      indices = Where((red EQ missingColor[0]) AND (grn EQ missingColor[1]) $
                  AND (blu EQ missingColor[2]), count)
      IF count GT 0 THEN alpha[indices] = 0
    ENDIF
    
    ; Create the transparent image.
    transparentImage = [ [[red]], [[grn]], [[blu]], [[alpha]] ]
    transparentImage = Transpose(transparentImage, [2,0,1])
    
    ; Write this image to a file?
    IF Keyword_Set(pngfile) THEN BEGIN
        
        IF Size(pngfile, /TNAME) EQ 'STRING' THEN outfilename = pngfile ELSE outfilename = 'transparent.png'
        IF noGUI THEN BEGIN
            Write_PNG, outfilename, transparentImage
        ENDIF ELSE BEGIN
            outfilename = Dialog_Pickfile(Title='Select PNG File For Output...', $
                FILE=outfilename, FILTER='*.png')
            IF outfilename NE "" THEN BEGIN
               Write_PNG, outfilename, transparentImage
            ENDIF
        ENDELSE
        
    ENDIF
    
    ; Return the transparent image.
    RETURN, transparentImage
    
END
