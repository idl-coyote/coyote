; docformat = 'rst'
;
; NAME:
;   cgImgScl
;
; PURPOSE:
;   This function scales an image using the same keywords and scaling available in 
;   cgImage and cgStretch.
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
;+
; This function scales an image using the same keywords and scaling available in 
; cgImage and cgStretch. Set the `Stretch` keyword for the types of image scaling
; or stretching available.
; 
; :Categories:
;    Graphics, Utilities
;    
; :Returns:
;     Returns a scaled image.
; 
; :Params:
;    image: in, required
;       The input image that is to be scaled. Only 2D images can be scaled, although
;       true-color images can be resized.
;    xsize: in, optional
;       The output X size of the image.
;    ysize: in, optional
;       The output Y size of the image.
;   
; :Keywords:
;    bottom: in, optional, type=integer, default=0
;         If the `SCALE` keyword is set, the image is scaled before display so that all 
;         displayed pixels have values greater than or equal to `BOTTOM` and less than 
;         or equal to `TOP`.
;    beta: in, optional, type=float, default=3.0
;         The beta factor in a Hyperpolic Sine stretch.
;    clip: in, optional, type=float, default=2
;         A number between 0 and 50 that indicates the percentage of pixels to clip
;         off either end of the image histogram before performing a linear stretch.
;    exclude: in, optional, type=numeric
;         The value to exclude in a standard deviation stretch.
;    exponent: in, optional, type=float, default=4.0
;         The logarithm exponent in a logarithmic stretch.
;    gamma: in, optional, type=float, default=1.5
;         The gamma factor in a gamma stretch.
;    interpolate: in, optional, type=boolean, default=0
;         Set this keyword to interpolate with bilinear interpolation the display image as it 
;         is sized to its final position in the display window. Interpolation will potentially 
;         create image values that do not exist in the original image. The default is to do no
;         interpolation, so that image values to not change upon resizing. Interpolation can
;         result in smoother looking final images.
;    maxvalue: in, optional, type=varies
;         If this value is defined, the data is linearly scaled between `MINVALUE`
;         and `MAXVALUE`. `MAXVALUE` is set to `MAX`(image) by default. Setting this 
;         keyword to a value automatically sets `SCALE` to 1. If the maximum value of the 
;         image is greater than 255, this keyword is defined and SCALE=1.
;    mean: in, optional, type=float, default=0.5
;         The mean factor in a logarithmic stretch.
;    minus_one: in, optional, type=boolean, default=0
;         The value of this keyword is passed along to the cgResizeImage
;         command. It prevents cgResizeImage from adding an extra row and
;         column to the resulting array, which can be a problem with
;         small image arrays. 
;    minvalue: in, optional, type=varies
;         If this value is defined, the data is linearly scaled between MINVALUE
;         and `MAXVALUE`. `MINVALUE` is set to `MIN`(image) by default. Setting this 
;         keyword to a value automatically sets SCALE=1. If the minimum value of the 
;         image is less than 0, this keyword is defined and SCALE=1.
;    missing_index: in, optional, type=integer, default=255
;         The index of the missing color in the final byte scaled image.
;    missing_value: in, optional, type=integer
;         The number that represents the missing value in the image.
;    multiplier: in, optional, type=float
;         The multiplication factor in a standard deviation stretch. The standard deviation
;         is multiplied by this factor to produce the thresholds for a linear stretch.
;    ncolors: in, optional, type=integer, default=256
;         If this keyword is supplied, the `TOP` keyword is ignored and the TOP keyword 
;         is set equal to  NCOLORS-1. This keyword is provided to make cgImgScl easier 
;         to use with the color-loading programs such as cgLOADCT::
;
;              cgLoadCT, 5, NColors=100, Bottom=100
;              scaled = cgImgScl(image, NColors=100, Bottom=100)
;                  
;         Setting this keyword to a value automatically sets SCALE=1 and STRETCH='LINEAR', if not
;         defined otherwise. Available only with 2D images.
;    negative: in, optional, type=boolean, default=0
;         Set this keyword if you want to display the image with a negative or reverse stretch.
;    scale: in, optional, type=boolean, default=0
;         Set this keyword to byte scale the image before display. If this keyword is not set, 
;         the image is not scaled before display. This keyword will be set automatically by using
;         any of the keywords normally associated with byte scaling an image.
;    stretch: in, optional, type=integer/string, default=0
;         The type of scaling performed prior to display. May be specified as a number 
;         or as a string (e.g, 3 or "Log"). Available only with 2D images. If  Min(image)
;         is less than zero or Max(image) is greater than 255, then the default value for
;         stretch is 1.
;
;         Number   Type of Stretch::
;             0         None           No scaling whatsoever is done.
;             1         Linear         scaled = BytScl(image, MIN=minValue, MAX=maxValue)
;             2         Linear 2%      A histogram stretch, with a percentage of pixels clipped at both the top and bottom
;             3         Gamma          scaled = GmaScl(image, MIN=minValue, MAX=maxValue, Gamma=gamma)
;             4         Log            scaled = LogScl(image, MIN=minValue, MAX=maxValue, Mean=mean, Exponent=exponent)
;             5         Asinh          scaled = AsinhScl(image, MIN=minValue, MAX=maxValue, Beta=beta)
;             6         SquareRoot     A linear stretch of the square root histogram of the image values.
;             7         Equalization   A linear stretch of the histogram equalized image histogram.
;             8         Gaussian       A Gaussian normal function is applied to the image histogram.
;             9         MODIS          Scaling done in the differential manner of the MODIS Rapid Response Team
;                                      and implemented in the Coyote Library routine ScaleModis.
;             10        StdDev         Standard deviation stretch. scaled = SDevScl(image, MULTIPLIER=2).
;    sigma: in, optional, type=float, default=1.0
;         The sigma scale factor in a Gaussian stretch.
;    top: in, optional, type=integer, default=255
;         If the `SCALE` keyword is set, the image is scaled before display so that all 
;         displayed pixels have values greater than or equal to `BOTTOM` and less than 
;         or equal to `TOP`.
;         
; :Examples:
;    Used to for image display::
;       IDL> scaledImage = cgImgScl(image, Stretch=4, Exponent=2)
;       IDL> cgImage, scaledImage[0:100, 200:400], /Keep_Aspect
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
;       Written by David W. Fanning, 21 September 2012. 
;       Only define MISSING_INDEX value if needed. Change to support TRANSPARENT keyword
;          in cgImage. 18 October 2012. DWF.
;       Only 2D images can be scaled. 18 October 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.

;-
FUNCTION cgImgScl, image, xsize, ysize, $
   BOTTOM=bottom, $
   BETA=beta, $
   CLIP=clip, $
   EXCLUDE=exclude, $
   EXPONENT=exponent, $
   GAMMA=gamma, $
   INTERPOLATE=interpolate, $
   MAXVALUE=maxvalue, $
   MEAN=mean, $
   MINUS_ONE=minus_one, $
   MINVALUE=minvalue, $
   MISSING_INDEX=missing_index, $
   MISSING_VALUE=missing_value, $
   MULTIPLIER=multiplier, $
   NCOLORS=ncolors, $
   NEGATIVE=negative, $
   SCALE=scale, $
   STRETCH=stretch, $
   SIGMA=sigma, $
   TOP=top

   Compile_Opt idl2
   
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /Cancel
       ok = Error_Message()
       RETURN, image
    ENDIF
    
    ; Did the user want to scale the image?
    ; If either MIN or MAX are set, this implies SCALE=1.
    ; If min LT 0 or max GT 255, this implies SCALE=1.
    ; If NCOLORS is used, this implies SCALE=1 and STRETCH='linear', if STRETCH is undefined.
    IF N_Elements(min) EQ 0 THEN min = Min(image, /NAN) ELSE scale = 1
    IF N_Elements(max) EQ 0 THEN max = Max(image, /NAN) ELSE scale = 1
    IF (min LT 0) OR (max GT 255) THEN scale = 1
    IF N_Elements(top) EQ 0 THEN top = (N_Elements(missing_value) NE 0) ? !D.TABLE_SIZE - 2 : !D.TABLE_SIZE - 1
    IF N_Elements(bottom) EQ 0 THEN bottom = 0B
    IF N_Elements(ncolors) NE 0 THEN BEGIN
        top = (N_Elements(missing_value) NE 0) ? (ncolors - 2) < 255 : (ncolors - 1)
        scale = 1
     ENDIF
    
    ncolors = top-bottom+1
    negative = Keyword_Set(negative)
    scale = Keyword_Set(scale)
    IF scale THEN BEGIN
       IF N_Elements(stretch) EQ 0 THEN stretch = 1
    ENDIF 
    SetDefaultValue, beta, 3.0
    SetDefaultValue, clip, 2
    SetDefaultValue, exponent, 4.0
    SetDefaultValue, gamma, 1.5 
    SetDefaultValue, mean, 1.0
    SetDefaultValue, negative, 0
    SetDefaultValue, sigma, 1.0
    SetDefaultValue, stretch, 0
    
    ; Missing index only defined if needed. Change to support transparent images in cgImage.
    IF N_Elements(missing_value) NE 0 THEN IF N_Elements(missing_index) EQ 0 THEN missing_index = 255
    
    ; Make sure you can specify the type of stretch with a string name.
    IF Size(stretch, /TNAME) EQ 'STRING' THEN BEGIN
        stretches = ['None', 'Linear', 'Clip', 'Gamma', 'Log', 'ASinh', $
              'SquareRoot', 'Equalization', 'Gaussian', 'MODIS', 'StdDev']
       index = Where(StrUpCase(stretch) EQ StrUpCase(stretches), count)
       IF count GT 0 THEN stretch=index ELSE Message, 'Cannot find stretch: ' + StrUpCase(stretch)
    ENDIF
    IF stretch NE 0 THEN scale = 1
    
    ; Can't stretch true-color images.
    IF Size(image, /N_DIMENSIONS) NE 2 THEN scale = 0
    
   ; I would like to avoid making a copy of the image, if possible.
   ; If nothing needs to be done, just return the image. Only 2D images
   ; can proceed.
   IF (N_Elements(xsize) EQ 0) && $
      (N_Elements(missing_value) EQ 0) && $
      ~Keyword_Set(scale) THEN RETURN, image
      
   ; Is there a missing value to worry about? We can only worry
   ; about missing values with 2D image arrays.
   ndims = Size(image, /N_DIMENSIONS)
   IF (N_Elements(missing_value) NE 0) && (ndims EQ 2) THEN BEGIN
   
       ; Get the image type.
       imageType = Size(image, /TNAME)
       
       ; Create a temporary image variable. If you will be scaling the data,
       ; make the image a float if it is not float or double already.
       CASE imageType OF
          'FLOAT': tempImage = image
          'DOUBLE': tempImage = image
          ELSE: tempImage = Float(image) 
       ENDCASE
       
       ; The missing value may be the symbol for NAN.
       IF Finite(missing_value) THEN BEGIN
           missingIndices = Where(tempImage EQ missing_value, missingCnt)
       ENDIF ELSE BEGIN
           missingIndices = Where(Finite(tempImage) EQ 0, missingCnt)
       ENDELSE
       
       ; Set the missing indices to the correct NAN value.
       IF imageType EQ 'DOUBLE' THEN BEGIN
           IF missingCnt GT 0 THEN tempImage[missingIndices] = !Values.D_NAN
       ENDIF ELSE BEGIN
           IF missingCnt GT 0 THEN tempImage[missingIndices] = !Values.F_NAN
       ENDELSE
   
   ENDIF
   
   ; Do you need scaling?
   IF Keyword_Set(scale) || (stretch NE 0) THEN BEGIN
   
      ; Create a temporary image, if you don't already have one.
      IF N_Elements(tempImage) EQ 0 THEN BEGIN
          imageType = Size(image, /TNAME)
          CASE imageType OF
              'FLOAT': tempimage = image
              'DOUBLE': tempimage = image
              ELSE: tempImage = Float(image) 
          ENDCASE
       ENDIF
      
      ; Select the particular kind of stretch you want to do. Unfortunately, we
      ; can still cause underflow error messages when doing stretch, despite best
      ; attempts to prevent this. Turn these messages off here.
      curExcept = !Except
      !Except = 0      
       CASE stretch OF
       
;             0         None           No scaling whatsoever is done.
;             1         Linear         scaled = BytScl(image, MIN=minValue, MAX=maxValue)
;             2         Clip           A histogram stretch, with a percentage of pixels clipped at both the top and bottom
;             3         Gamma          scaled = GmaScl(image, MIN=minValue, MAX=maxValue, Gamma=gamma)
;             4         Log            scaled = LogScl(image, MIN=minValue, MAX=maxValue, Mean=mean, Exponent=exponent)
;             5         Asinh          scaled = AsinhScl(image, MIN=minValue, MAX=maxValue, Beta=beta)
;             6         SquareRoot     A linear stretch of the square root histogram of the image values.
;             7         Equalization   A linear stretch of the histogram equalized image histogram.
;             8         Gaussian       A Gaussian normal function is applied to the image histogram.
;             9         MODIS          Scaling done in the differential manner of the MODIS Rapid Response Team
;             10        StdDev         A standard deviation stretch.

          0: ; No stretch at all. 
       
          1: BEGIN ; Linear stretch.
             tempImage = BytScl(tempImage, Max=maxvalue, Min=minvalue, /NAN, TOP=top) + bottom
             IF negative THEN tempImage = Byte(top) - tempImage
             END
    
          2: BEGIN ; Histogram clip stretch.
             tempImage = ClipScl(tempImage, clip, OMIN=bottom, OMAX=top, NEGATIVE=negative)
             END

          3: BEGIN ; Gamma log scale stretch.
             tempImage = GmaScl(tempImage, Max=maxvalue, Min=minvalue, $
                       Gamma=gamma, Negative=negative, OMAX=top, OMIN=bottom)
             END
    
          4: BEGIN ; Log scale stretch.
             tempImage =  LogScl(tempImage, Max=maxvalue, Min=minvalue, $
                       Mean=mean, Exponent=exponent, Negative=negative, $
                       OMIN=bottom, OMAX=top)
             END
    
          5: BEGIN ; Hyperpolic sine stretch.
             tempImage = ASinhScl(tempImage, Max=maxvalue, Min=minvalue, $
                      BETA=beta, Negative=negative, OMAX=top, OMIN=bottom)
             END
               
    
          6: BEGIN ; Square Root stretch.
             tempImage = BytScl(SQRT(tempImage), Max=maxvalue, Min=minvalue, /NAN, TOP=top) + bottom
             IF negative THEN tempImage = Byte(top) - tempImage
             END
    
          7: BEGIN ; Histogram equalization stretch.
             IF (top EQ 255) && (bottom EQ 0) THEN BEGIN
                 tempImage = Hist_Equal(tempImage, MaxV=maxvalue, MinV=minvalue)
             ENDIF ELSE BEGIN
                 tempImage = Bytscl(Float(Hist_Equal(tempImage, MaxV=maxvalue, MinV=minvalue)), /NAN, TOP=top) + bottom
             ENDELSE
             IF negative THEN tempImage = Byte(top) - tempImage
             END
    
          8: BEGIN ; Gaussian stretch.
             tempImage = GaussScl(tempImage, Max=maxvalue, Min=minvalue, $
                       Sigma=sigma, Negative=negative, OMIN=bottom, OMAX=top)
             END
         
          9: BEGIN ; MODIS image stretch.
             tempImage = ScaleModis(tempImage)
             END
             
          10: BEGIN ; Standard deviation stretch.
              tempImage = SDevScl(tempImage, MULTIPLIER=multiplier, EXCLUDE=exclude, $
                   Negative=negative, OMAX=top, OMIN=bottom)
              END
               
            ELSE: Message, 'Unknown scaling index.'
            
       ENDCASE
       
       ; Clear the math error register and turn normal error checking on.
       void = Check_Math()
       !Except = curExcept
       
   ENDIF
   
   ; After scaling, you may need to replace missing values with the
   ; missing index.
   IF N_Elements(missingCnt) NE 0 THEN BEGIN
      IF missingCnt GT 0 THEN tempImage[missingIndices] = missing_index
   ENDIF
   
   ; If you created a temporary image, then return that.
   ; Otherwise you can return the original image, modified
   ; to the appropriate size.
   IF N_Elements(tempImage) EQ 0 THEN BEGIN
       IF (N_Elements(xsize) EQ 0) THEN BEGIN
           RETURN, image
       ENDIF ELSE BEGIN
           RETURN, cgResizeImage(image, xsize, ysize, $
                    INTERP=interpolate, MINUS_ONE=minus_one)
       ENDELSE
   ENDIF ELSE BEGIN
       IF (N_Elements(xsize) EQ 0) THEN BEGIN
           RETURN, tempImage
       ENDIF ELSE BEGIN
           RETURN, cgResizeImage(tempImage, xsize, ysize, $
                    INTERP=interpolate, MINUS_ONE=minus_one)
       ENDELSE
   ENDELSE
END