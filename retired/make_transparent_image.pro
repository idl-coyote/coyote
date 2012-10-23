; docformat = 'rst'
;
; NAME:
;   Make_Transparent_Image
;
; PURPOSE:
;   Creates a transparent image from an input image or from the current display window.
;   The transparent color is set with the COLOR keyword.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; :Description:
;   Creates a transparent image from an input image or from the current display window.
;   The transparent color is set with the COLOR keyword.
;
; :Categories:
;    Graphics
;    
; :Params:
;    image: in, optional, type=byte
;       A 2D image or a 24-bit image with or without an alpha channel.
;       
; :Keywords:
;     color: in, optional, type=various
;        If COLOR is a string, use cgColor to obtain a color triple. If
;        COLOR is a scalar, replicate the value to obtain a color triple.
;        Othersize, expect a color triple to indicate the "transparent"
;        color in the output image. The alpha channel in the output image
;        is set to 0 for the transparent color. If this keyword is not used,
;        the color of the pixels in the lower-left [0,0] corner of the image
;        is used instead.
;        
;     filename: in, optional, type=string
;         If the SAVE_PNG keyword is used this keyword will specify the name
;         of the PNG file to create. If a filename is provided, no user dialog
;         will be displayed. If a filename is not provided, the user will be
;         prompted for the name of the output file.
;        
;     save_png: in, optional, type=boolean
;         If this keyword is set, the resulting transparent image will
;         be written as a PNG image file.
;        
;     wset: in, optional, type=long
;        If image parameter is not passed, make the window indicated with
;        this keyword the current graphics window for obtaining an image parameter.
;        If not passed, the current graphics window is used.
;
; :Examples:
;    To create and display a transparent image::
;       cgDisplay, WID=0
;       cgLoadCT, 0
;       cgImage, cgDemoData(5)
;       timage = Make_Transparent_Image(COLOR='black')
;       cgDisplay, WID=1
;       cgLoadCT, 22
;       cgImage, cgDemoData(7)
;       cgImage, timage, Position=[0.2, 0.2, 0.8, 0.8]
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
;        Written, 4 November 2010. DWF.
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
FUNCTION Make_Transparent_Image, image, COLOR=color, $
    FILENAME=filename, SAVE_PNG=save_png, WSET=wset

    Catch, theError
    IF theError NE 0 THEN BEGIN 
        Catch, /CANCEL
        void = Error_Message()
        RETURN, image
    ENDIF
    
    ; Did the user pass an image? If not, read an image from the display.
    IF N_Elements(image) EQ 0 THEN BEGIN
        IF (!D.Flags AND 256) EQ 0 THEN Message, 'Current device does not support windows.'
        IF N_Elements(wset) NE 0 THEN BEGIN
            thisWindow = !D.Window
            WSet, wset
        ENDIF
        image = cgSnapshot(TRUE=3)
        IF N_Elements(thisWindow) NE 0 THEN WSet, thisWindow
    ENDIF
    
    ; What kind of image are we dealing with here?
    ndims = Size(image, /N_DIMENSIONS)
    CASE ndims OF
        2: BEGIN
           TVLCT, r, g, b, /GET
           red = r[image]
           grn = g[image]
           blu = b[image]
           alpha = Byte(image) * 0 + 255B
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
                alpha = BytArr(s[0], s[1]) + 255B
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
                alpha = aImage[*,*,3]           
           ENDIF
           END
                      
        ELSE: Message, 'Input variable does not appear to be an image.'
    ENDCASE
    
    ; Did the user pass a color? If not, use the color of the pixel in the
    ; lower-left corner [0,0] of the image.
    CASE N_Elements(color) OF
        0: thisColor = [red[0,0], grn[0,0], blu[0,0]]
        1: BEGIN
           CASE Size(color, /TNAME) OF
                'STRING': thisColor = cgColor(color, /TRIPLE)
                ELSE: thisColor = Replicate(color, 3)
           ENDCASE
           END
        2: Message, 'The COLOR keyword contains unexpected values.'
        3: thisColor = color
        ELSE: Message, 'The COLOR keyword must be a color triple.'
    ENDCASE
    
    ; Find the color in the image.
    indices = Where((red EQ thisColor[0]) AND (grn EQ thisColor[1]) AND (blu EQ thisColor[2]), count)
    IF count GT 0 THEN alpha[indices] = 0
    
    ; Create the transparent image.
    transparentImage = [ [[red]], [[grn]], [[blu]], [[alpha]] ]
    
    ; Write this image to a file?
    IF Keyword_Set(save_png) THEN BEGIN
    
        IF N_Elements(filename) NE 0 THEN BEGIN
            Write_PNG, filename, Transpose(transparentImage, [2,0,1])
        ENDIF ELSE BEGIN
            filename = Dialog_Pickfile(Title='Select PNG File For Output...', $
                FILE='transparent.png', FILTER='*.png')
            IF filename NE "" THEN BEGIN
               Write_PNG, filename, Transpose(transparentImage, [2,0,1])
            ENDIF
        ENDELSE
        
    ENDIF
    
    ; Return the transparent image.
    RETURN, transparentImage
    
END
