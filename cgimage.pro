; docformat = 'rst'
;
; PURPOSE:
;     The purpose of cgIMAGE is to create a device-independent TV command
;     with the power and functionality to be used in sophisticated graphics
;     programs, as well as at the IDL command line. It can be thought of as 
;     a "smart" TV command.
;
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
; The purpose of cgIMAGE is to create a device-independent TV command
; with the power and functionality to be used in sophisticated graphics
; programs, as well as at the IDL command line. It can be thought of as 
; a "smart" TV command.
;   
; There is a common block in cgImage that is defined as follows upon exiting
; this command::
; 
;    COMMON FSC_$CGIMAGE, $
;       _cgimage_xsize, $    ; The X size of the image.
;       _cgimage_ysize, $    ; The Y size of the imge.
;       _cgimage_winxsize, $ ; The X size of the window displaying the image.
;       _cgimage_winysize, $ ; The Y size of the window displaying the image.
;       _cgimage_position, $ ; The final position of the image in the window.
;       _cgimage_winID, $    ; The window index number of the window displaying the image.
;       _cgimage_current     ; Set to 1 if a call to cgImage is made.
;
; :Categories:
;    Graphics
;       
; :Examples:
;     To display an image with a contour plot on top of it, type::
;
;        filename = FILEPATH(SUBDIR=['examples','data'], 'worldelv.dat')
;        image = BYTARR(360,360)
;        OPENR, lun, filename, /GET_LUN
;        READU, lun, image
;        FREE_LUN, lun
;
;        thisPostion = [0.1, 0.1, 0.9, 0.9]
;        cgIMAGE, image, POSITION=thisPosition, /KEEP_ASPECT_RATIO
;        CONTOUR, image, POSITION=thisPosition, /NOERASE, XSTYLE=1, $
;            YSTYLE=1, XRANGE=[0,360], YRANGE=[0,360], NLEVELS=10
;
;     To display four images in a window without spacing between them::
;
;        !P.Multi=[0,2,2]
;        cgImage, image, Margin=0
;        cgImage, image, Margin=0
;        cgImage, image, Margin=0
;        cgImage, image, Margin=0
;        !P.Multi = 0
;
;     To display four image in a window with associated color bars::
;
;        !P.Multi=[0,2,2]
;        p = [0.02, 0.3, 0.98, 0.98]
;        LoadCT, 0
;        cgImage, image, Position=p
;        cgColorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;        p = [0.02, 0.3, 0.98, 0.98]
;        LoadCT, 2
;        cgImage, image, Position=p
;        cgColorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;        p = [0.02, 0.3, 0.98, 0.98]
;        LoadCT, 3
;        cgImage, image, Position=p
;        cgColorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;        p = [0.02, 0.3, 0.98, 0.98]
;        LoadCT, 5
;        cgImage, image, Position=p
;        cgColorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;        !P.Multi =0
;     
;     To set a missing value to -32767 and the color white and do
;     a 3% histogram clip of the image::
;     
;        cgLoadCT, 4, /Brewer, NColors=254
;        TVLCT, palette, /Get
;        cgImage, image, Missing_Value=-32767, Missing_Color='white', Stretch='Clip', Clip=3
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;        David W. Fanning 
;        1645 Sheely Drive
;        Fort Collins, CO 80526 USA
;        Phone: 970-221-0438
;        E-mail: david@idlcoyote.com
;        Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; :History:
;    Modification History::
;      Written by: David W. Fanning, from modifications to TVIMAGE. 3 Feb 2011.
;      8 Feb 2011. Added OPOSITION keyword. DWF.
;      27 Feb 2011. Added keywords to make cgImage more compatible with TVImage calls. DWF.
;      Color table vectors must be obtained AFTER loading the color palette. 6 March 2011. DWF.
;      I have been convinced (conversations with Wayne Landsman) that if the 
;         CENTER keyword is set, the MINUS_ONE keyword is not needed, since 
;         it was created to solve the same problem. So, I have changed the 
;         default setting of MINUS_ONE to 0. 14 March 2011. DWF.
;       Corrected a problem with restoring color tables if a PALETTE is used. 31 March 2011. DWF.
;       Whoops! Documented a CHARSIZE keyword, but forgot to define it. 7 July 2011.
;       Damnation! I did the same thing with the FONT keyword! 25 July 2011.
;       And now a TITLE keyword! What the devil is going on!? 29 Aug 2011.
;       Very slight modifications to image size and start position so that the image is
;          positioned completely inside the axes. 30 Sept 2011. DWF.
;       Fitting the image inside the axes causes image matching problems (and lines!) in
;          other programs, so I've decided to only do positioning inside axes when the
;          user asks for this capability by setting the new FIT_INSIDE keyword. 16 Nov 2011. DWF.
;       Problem with transparent images with alpha channels caused by changes in the TVImage->cgImage
;          transition. Added AlphaFGPosition keyword to address issues. Cleaned up the
;          code and improved the internal documentation. 22 Nov 2011. DWF.
;       Added the ability to stretch 2D image arrays in various ways before display. 1 Dec 2011.
;       Added the ability to handle missing data in 2D arrays before display. 1 Dec 2011.
;       Added a DISPLAY keyword to display the image in windows with the image aspect ratio. 2 Dec 2011.
;       Added the ability to send the output directly to a file via the OUTPUT keyword. 9 Dec 2011, DWF.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
;
;+
; This routine prepares a transparent image (an image with an alpha channel)
; for display.
; 
; :Returns:
;     Returns an image that has been blended with the background image and is
;     suitable for display with cgImage.
; 
; :Params:
;    image: in, required
;       The input image that is being prepared for display. It will contain
;       an alpha channel.
;    alphaBackgroundImage: in, required
;       The background image. The foregroundimage, image, will be blended
;       with the background image.
;   
; :Keywords:
;    alphabgposition: in, required, type=fltarr
;       The normalized position in the window where the background 
;       image is to be located.
;    alphafgposition: in, required, type=fltarr
;       The normalized position in the window where the foreground 
;       image is to be located.
;    tv: in, optional, type=boolean, default=0
;       If this keyword is set, the alpha channel is removed from the
;       input image, because we cannot display an image with an alpha
;       channel if the cgImage command is acting like a smarter IDL 
;       TV command.
;-
FUNCTION cgImage_Prepare_Alpha, image, alphaBackgroundImage, $
    ALPHABGPOSITION=alphabgpos, $
    ALPHAFGPOSITION=alphafgpos, $    
    TV=tv

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /Cancel
       ok = Error_Message()
       IF Ptr_Valid(ptr) THEN BEGIN
            image = Temporary(*ptr)
            Ptr_Free, ptr
       ENDIF
       IF N_Elements(thisDevice) NE 0 THEN Set_Plot, thisDevice
       RETURN, image
    ENDIF

    ; Prepare an alpha image, if needed.
    index = Where(Size(image,/DIMENSIONS) EQ 4)
    CASE index OF
         0: aImage = Transpose(image, [1,2,0])
         1: aImage = Transpose(image, [0,2,1])
         ELSE: aImage = image
    ENDCASE
       
    ; Separate the alpha channel.
    alpha_channel = aImage[*,*,3]
       
    ; If this is acting like a TV command, then there is no alpha channel.
    ; Exit now.
    IF Keyword_Set(tv) THEN RETURN, aImage[*,*,0:2]
    
    ; If this version of IDL is 6.4 or older, we can't do this.
    thisRelease = Float(!Version.Release)
    IF thisRelease LT 6.5 THEN BEGIN
       Message, 'IDL 6.5 or higher required to correctly display alpha images.', /INFORMATIONAL
       RETURN, aImage[*,*,0:2]
    ENDIF
                    
    ; Some alpha channels are screwy. Just ignore those and return now.
    IF MIN(alpha_channel) EQ MAX(alpha_channel) THEN RETURN, aImage[*,*,0:2]
       
    ; Now we have an alpha channel.
    alpha_channel = Scale_Vector(alpha_channel, 0.0, 1.0)
    foregndImage = aImage[*,*,0:2]
           
    ; Get the size and dimensions of the background image.
    ndim = Size(alphaBackgroundImage, /N_DIMENSIONS)
    CASE ndim OF
        2: BEGIN
           TVLCT, r, g, b, /GET
           s = Size(alphaBackgroundImage, /DIMENSIONS)
           bImage = BytArr(s[0], s[1], 3)
           bImage[*,*,0] = r[alphaBackgroundImage]
           bImage[*,*,1] = g[alphaBackgroundImage]
           bImage[*,*,2] = b[alphaBackgroundImage]
           END
        3: BEGIN
           index = Where(Size(alphaBackgroundImage,/DIMENSIONS) EQ 3)
           CASE index OF
              0: bImage = Transpose(alphaBackgroundImage, [1,2,0])
              1: bImage = Transpose(alphaBackgroundImage, [0,2,1])
              ELSE: bImage = alphaBackgroundImage
           ENDCASE
           END
      ELSE: Message, 'Unexpected dimensions of the background image.'
    ENDCASE
    
    ; Now that we have a background image, display that in
    ; the Z-Graphics buffer.
    sb = Size(bImage, /DIMENSIONS)
    sf = Size(foregndImage, /DIMENSIONS)
    thisDevice = !D.Name
    Set_Plot, 'Z'
    Device, Get_Decomposed=theState
    Device, Set_Resolution=sb[0:1], Decomposed=1, Set_Pixel_Depth=24
   
    IF N_Elements(alphabgpos) EQ 0 THEN BEGIN
        cgImage, bImage
    ENDIF ELSE BEGIN
        cgImage, bImage, Position=alphabgpos
    ENDELSE
    
    ; Calculate the parameters for taking a snapshot of the
    ; relevant portion of the window.
    xstart = alphafgpos[0]*sb[0]
    cols = Round((alphafgpos[2] - alphafgpos[0]) * sb[0])
    ystart = alphafgpos[1]*sb[1]
    rows = Round((alphafgpos[3] - alphafgpos[1]) * sb[1])
            
    ; Take a snapshot
    bsnap = TVRD(xstart, ystart, cols, rows, TRUE=3)
            
    ; Get the size of the snapshot.
    sb = Size(bsnap, /DIMENSIONS)
    Device, Decomposed=theState
    Set_Plot, thisDevice
            
     ; Make the foreground image the right size.
     foregndImage = FSC_Resize_Image(foregndImage, cols, rows)
     alpha = FSC_Resize_Image(alpha_channel, sb[0], sb[1], /INTERPOLATE)
     alpha = Rebin(alpha, sb[0], sb[1], 3)
            
     ; Blend the two images in the location of the POSITION.
     blendImage = foregndImage*alpha + (1 - alpha)*bsnap  
     
     ; Now put this blended portion back into the background image.
     outimage = bimage
     outimage[xstart:xstart+cols-1, ystart:ystart+rows-1, *] = blendImage
            
     ; Put the dimensions back the way they came in.   
     index = Where(Size(foregndImage,/DIMENSIONS) EQ 3)
     CASE index OF
        0: outImage = Transpose(outImage, [2,0,1])
        1: outImage = Transpose(outImage, [1,0,2])
        ELSE: outImage = outImage
     ENDCASE
    
     RETURN, outimage
END
;--------------------------------------------------------------------------


;+
; This routine scales or otherwise prepares an image to be displayed.
; 
; :Returns:
;     Returns an image that can be displaye properly.
; 
; :Params:
;    image: in, required
;       The input image that is being prepared for display. 
;    xsize: in, optional
;       The output X size of the image.
;    ysize: in, optional
;       The output Y size of the image.
;   
; :Keywords:
;    bottom: in, optional, type=integer, default=0
;         If the SCALE keyword is set, the image is scaled before display so that all 
;         displayed pixels have values greater than or equal to BOTTOM and less than 
;         or equal to TOP.
;    beta: in, optional, type=float, default=3.0
;         The beta factor in a Hyperpolic Sine stretch.
;    clip: in, optional, type=float, default=2
;         A number between 0 and 50 that indicates the percentage of pixels to clip
;         off either end of the image histogram before performing a linear stretch.
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
;         If this value is defined, the data is linearly scaled between MINVALUE
;         and MAXVALUE. MAXVALUE is set to MAX(image) by default. Setting this 
;         keyword to a value automatically sets `SCALE` to 1. If the maximum value of the 
;         image is greater than 255, this keyword is defined and SCALE=1.
;    mean: in, optional, type=float, default=0.5
;         The mean factor in a logarithmic stretch.
;    minus_one: in, optional, type=boolean, default=0
;         The value of this keyword is passed along to the FSC_RESIZE_IMAGE
;         command. It prevents FSC_RESIZE_IMAGE from adding an extra row and
;         column to the resulting array, which can be a problem with
;         small image arrays. 
;    minvalue: in, optional, type=varies
;         If this value is defined, the data is linearly scaled between MINVALUE
;         and `MAXVALUE`. MINVALUE is set to MIN(image) by default. Setting this 
;         keyword to a value automatically sets SCALE=1. If the minimum value of the 
;         image is less than 0, this keyword is defined and SCALE=1.
;    missing_index: in, optional, type=integer, default=255
;         The index of the missing color in the final byte scaled image.
;    missing_value: in, optional, type=integer
;         The number that represents the missing value in the image.
;    negative: in, optional, type=boolean, default=0
;         Set this keyword if you want to display the image with a negative or reverse stretch.
;    scale: in, optional, type=boolean, default=0
;         Set this keyword to byte scale the image before display. If this keyword is not set, 
;         the image is not scaled before display. This keyword will be set automatically by using
;         any of the keywords normally associated with byte scaling an image.
;    stretch: in, optional, type=integer/string, default=1
;         The type of scaling performed prior to display. 
;         May be specified as a number or as a string (e.g, 3 or "Log").
;
;           Number   Type of Stretch
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
;                                      and implemented in the Coyote Library routine ScaleModis.
;    sigma: in, optional, type=float, default=1.0
;         The sigma scale factor in a Gaussian stretch.
;    top: in, optional, type=integer, default=255
;         If the SCALE keyword is set, the image is scaled before display so that all 
;         displayed pixels have values greater than or equal to BOTTOM and less than 
;         or equal to TOP.
;-
FUNCTION cgImage_Prepare_Output, image, xsize, ysize, $
   BOTTOM=bottom, $
   BETA=beta, $
   CLIP=clip, $
   EXPONENT=exponent, $
   GAMMA=gamma, $
   INTERPOLATE=interpolate, $
   MAXVALUE=maxvalue, $
   MEAN=mean, $
   MINUS_ONE=minus_one, $
   MINVALUE=minvalue, $
   MISSING_INDEX=missing_index, $
   MISSING_VALUE=missing_value, $
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
    
   ; I would like to avoid making a copy of the image, if possible.
   ; If nothing needs to be done, just return the image.
   IF (N_Elements(xsize) EQ 0) && $
      (N_Elements(missing_value) EQ 0) && $
      ~scale THEN RETURN, image
      
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
             tempImage = BytScl(Hist_Equal(tempImage), Max=maxvalue, Min=minvalue, /NAN, TOP=top) + bottom
             IF negative THEN tempImage = Byte(top) - tempImage
             END
    
          8: BEGIN ; Gaussian stretch.
             tempImage = GaussScl(tempImage, Max=maxvalue, Min=minvalue, $
                       Sigma=sigma, Negative=negative, OMIN=bottom, OMAX=top)
             END
         
          9: BEGIN ; MODIS image stretch.
             tempImage = ScaleModis(tempImage)
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
           RETURN, FSC_Resize_Image(image, xsize, ysize, $
                    INTERP=interpolate, MINUS_ONE=minus_one)
       ENDELSE
   ENDIF ELSE BEGIN
       IF (N_Elements(xsize) EQ 0) THEN BEGIN
           RETURN, tempImage
       ENDIF ELSE BEGIN
           RETURN, FSC_Resize_Image(tempImage, xsize, ysize, $
                    INTERP=interpolate, MINUS_ONE=minus_one)
       ENDELSE
   ENDELSE
END


;+
; The purpose of this program is to create a TV command that works the way
; the TV command would be expected to work if it was written today, rather
; than 25 years ago. In other words, it knows the difference between an
; 8-bit device and a 24-bit device, it honors the POSITION keyword like 
; other graphics commands in IDL, it honors the !P.MULTI value, like other
; graphics commands in IDL, it works seamlessly with both 8-bit and 24-bit
; images. In addition to other modern features, this program can also 
; display images that contain an alpha channel.
; 
; Also, two-dimensional image arrays can be manipulated, stretched,
; and scaled directly with keywords to cgImage. These keywords do not
; work with alpha channel images, or if the TV keyword is used with
; cgImage.
; 
; :Params:
;    image:  in, required, type=various
;        An 8-bit (MxN), 24-bit (e.g., MxNx3), or a 24-bit + alpha channel
;        (e.g., MxNx4) image  to display.
;    x: in, optional, type=integer
;        The X position of the lower-left corner of the image in device
;        coordinates. This parameter is only recognized if the TV keyword 
;        is set. If the Y position is not used, X is taken to be the image
;        "position" in the window. See the TV command documenation for details. 
;    y: in, optional, type=integer      
;        The Y position of the lower-left corner of the image in device
;        coordinates. This parameter is only recognized if the TV keyword 
;        is set.
;        
; :Keywords:
;    addcmd: in, optional, type=boolean, default=0
;         Set this keyword to add this command to an already open cgWindow to
;         be executed when the window is resized. If the DISPLAY keyword is also
;         set, this keyword will act as if the WINDOW keyword were set.
;    alphabackgroundimage: in, optional, type=varies
;         Normally, when a image with an alpha channel is displayed, the image is 
;         blended with whatever is currently in the display window. This means, the 
;         program has to obtain that background image. This is not a problem on devices 
;         (e.g., WIN, X, Z) that allow this kind of operation, but it is on devices 
;         (e.g., the PostScript device, PS) that do not. To get around this problem, 
;         you can pass the background image to the cgImage program. This background image
;         will be blended with the alpha channel image you wish to display. If an alpha 
;         channel image is displayed on a device in which there is no way to obtain the 
;         background image, and this keyword is not used to pass a background image, then
;         the alpha channel image will be blended with a white background image.
;         This keyword is only used if an alpha channel image is passed to the 
;         program via the IMAGE parameter. The AlphaBackgroundImage does not need
;         to have the same dimensions as the alpha channel image. The background image
;         can be either a 2D image or a 24-bit image.
;    alphabgposition: in, optional, type=fltarr(4)
;         The normalized position where the background image is to be displayed in the
;         graphics window. By default, the position is calculated to position the image 
;         so that it covers the entire graphics window, [0.0, 0.0, 1.0, 1.0].
;    alphafgposition: in, optional, type=fltarr(4)
;         The normalized position where the foreground image (the image parameter) is to 
;         be displayed in the graphics window. By default, the position is calculated to 
;         position the image so that it covers the entire graphics window, [0.0, 0.0, 1.0, 1.0].
;    axis: in, optional, type=boolean, default=0
;         A misspelled version of the AXES keyword. Provided as a service to people whose
;         fingers have minds of their own.
;    axes: in, optional, type=boolean, default=0
;         Set this keyword to display the image with axes surrounding the image. If the POSITION
;         keyword is not used, a MARGIN of 0.1 is used to allow the axes to show. If you wish
;         to make the image fit entirely inside the axes, set the `FIT_INSIDE` keyword. Otherwise,
;         the axis sit on top of the image data.
;    axkeywords: in, optional, type=structure
;         A structure of AXIS keywords and values that can be used to configure the axes
;         in whatever way the user desires. Many of the most often used axis keywords are available 
;         as cgImage keywords.
;    background: in, optional, type=string, default='white'
;         The name of the background color for the image display. Unlike the TV command in IDL,
;         the cgImage command will erase the display before executing the command like other
;         fundamental graphics commands (e.g., Plot, Contour, Surface) in IDL.
;    beta: in, optional, type=float, default=3.0
;         The beta factor in a Hyperpolic Sine stretch. Available only with 2D images.
;    bottom: in, optional, type=integer, default=0
;         If the SCALE keyword is set, the image is scaled before display so that all 
;         displayed pixels have values greater than or equal to BOTTOM and less than 
;         or equal to TOP. Available only with 2D images.
;    charsize: in, optional, type=float
;         Sets the character size. Used only if the AXES keyword is also set. By default, 
;         the value from cgDefCharsize().
;    clip: in, optional, type=float, default=2
;         A number between 0 and 50 that indicates the percentage of pixels to clip
;         off either end of the image histogram before performing a linear stretch.
;         Available only with 2D images.
;    color: in, optional, type=string, default='opposite'
;         The name of the color in which to draw the axes. Used only if the `AXES` keyword is set.
;    display: in, optional, type=boolean, default=0
;         If this keyword is set, a new display window is created (with cgDisplay) that has the
;         same aspect ratio as the image. The image is displayed in that window. If the WINDOW
;         keyword is also set, a new cgWindow is created with the WASPECT keyword set to the image
;         aspect ratio, and the image is displayed in that new cgwindow. 
;    erase: in, optional, type=boolean, default=1
;         An obsolete keyword. Used only for compatibility with the earlier TVImage command. The
;         default for cgImage is to always erase the graphics display before displaying the image
;         unless told otherwise by setting the `NOERASE` keyword. This makes cgImage consistent with
;         other IDL graphics commands.
;    exponent: in, optional, type=float, default=4.0
;         The logarithm exponent in a logarithmic stretch. Available only with 2D images.
;    fit_inside: in, optional, type=boolean, default=0
;         When the AXES keyword is set, the default is to position the axes on top of the image
;         using the POSITION. However, if this keyword is set, the axes are positioned at POSITION
;         and the image is sized so that it fits entirely inside the axes.
;    font: in, optional, type=integer
;         This keyword selects the font used for axis and title display. The default is to use
;         the value of !P.Font.
;    gamma: in, optional, type=float, default=1.5
;         The gamma factor in a gamma stretch. Available only with 2D images.
;    interpolate: in, optional, type=boolean, default=0
;         Set this keyword to interpolate with bilinear interpolation the display image as it 
;         is sized to its final position in the display window. Interpolation will potentially 
;         create image values that do not exist in the original image. The default is to do no
;         interpolation, so that image values to not change upon resizing. Interpolation can
;         result in smoother looking final images.
;    keep_aspect_ratio: in, optional, type=boolean, default=0
;         By default, the output image is resized into the `POSITION` in the graphics window.
;         This can result in a distortion of the image aspect ratio (the Y size of the image
;         divided by the X size of the image). Setting this keyword will preserve the original
;         aspect ratio of the image in the output display window. In effect, the image will
;         be placed in the window `POSITION` in a way that preserves its aspect ratio. The
;         actual final location of the image in the display window can be obtained via the 
;         `OPOSITION` keyword.
;    layout: in, optional, type=intarr
;         This keyword specifies a grid with a graphics window and determines 
;         where the graphic should appear. The syntax of LAYOUT is a 3-element 
;         array: [ncolumns, nrows, location]. The grid is determined by the 
;         number of columns (ncolumns) by the number of rows (nrows). The location 
;         of the graphic is determined by the third number. The grid numbering 
;         starts in the upper left (1) and goes sequentually by column and then
;         by row. Note that using the LAYOUT keyword automatically sets the NOERASE 
;         keyword to 1.
;    margin: in, optional, type=float, default=0.0
;         A single value, expressed as a normalized coordinate, that
;         can easily be used to calculate a position in the window.
;         The margin is used to calculate a `POSITION` that gives
;         the image an equal margin around the edge of the window.
;         The margin must be a number in the range 0.0 to 0.333. This
;         keyword is ignored if the `POSITION` or `OVERPLOT` keywords are
;         used. It is also ignored when cgImage is executed in a
;         multi-plot window, EXCEPT if it's value is zero. In this
;         special case, the image will be drawn into its position in
;         the multi-plot window with no margins whatsoever. (The
;         default is to have a slight margin about the image to separate
;         it from other images or graphics. The default margin is 0.05.)
;    maxvalue: in, optional, type=varies
;         If this value is defined, the data is linearly scaled between MINVALUE
;         and MAXVALUE. MAXVALUE is set to MAX(image) by default. Setting this 
;         keyword to a value automatically sets `SCALE` to 1. If the maximum value of the 
;         image is greater than 255, this keyword is defined and SCALE=1.
;    mean: in, optional, type=float, default=0.5
;         The mean factor in a logarithmic stretch. Available only with 2D images.
;    minus_one: in, optional, type=boolean, default=0
;         The value of this keyword is passed along to the FSC_RESIZE_IMAGE
;         command. It prevents FSC_RESIZE_IMAGE from adding an extra row and
;         column to the resulting array, which can be a problem with
;         small image arrays. 
;    minvalue: in, optional, type=varies
;         If this value is defined, the data is linearly scaled between MINVALUE
;         and `MAXVALUE`. MINVALUE is set to MIN(image) by default. Setting this 
;         keyword to a value automatically sets SCALE=1. If the minimum value of the 
;         image is less than 0, this keyword is defined and SCALE=1.
;    missing_color: in, optional, type=string, default='white'
;         The color name of the missing value. Available only with 2D images.
;    missing_index: in, optional, type=integer, default=255 
;         The index of the missing color in the final byte scaled image. Available only with 2D images.
;    missing_value: in, optional, type=integer
;         The number that represents the missing value in the image. Available only with 2D images.
;    multimargin: in, optional, type=varies
;         Sometimes, when displaying multiple images with !P.Multi, you
;         want the images to be slightly smaller than the position created
;         by !P.Multi so you can add, for example, a colorbar or an annotation
;         to the image. This keyword can be used to adjust the image position
;         by a small margin. A four-element array, the margins apply to the 
;         [bottom, left, top, right] of the image position. So, to
;         leave room at the top of an image for a color bar, you might
;         type this::
;               
;             cgImage, image, MultiMargin=[0, 0, 4, 0]
;                  
;         This keyword applies *only* to images displayed with !P.Multi, and if
;         passed a scalar value, will use the same value for all four positions.
;    ncolors: in, optional, type=integer, default=256
;         If this keyword is supplied, the `TOP` keyword is ignored and the TOP keyword 
;         is set equal to  NCOLORS-1. This keyword is provided to make cgImage easier 
;         to use with the color-loading programs such as cgLOADCT::
;
;              cgLoadCT, 5, NColors=100, Bottom=100
;              cgImage, image, NColors=100, Bottom=100
;                  
;         Setting this keyword to a value automatically sets SCALE=1. Available only with 2D images.
;    negative: in, optional, type=boolean, default=0
;         Set this keyword if you want to display the image with a negative or reverse stretch.
;         Available only with 2D images.
;    noerase: in, optional, type=boolean, default=0
;         Set this keyword to prevent the command from first erasing the graphics
;         display before displaying the image.
;    nointerpolation: in, optional, type=boolean, default=0
;         This is an obsolete keyword that is included here only for compatibility with
;         the older TVImage command. Use the `INTERPOLATE` keyword.
;    normal: in, optional, type=boolean, default=0
;         Setting this keyword means image position coordinates x and y are interpreted 
;         as being in normalized coordinates. This keyword is only valid if the TV 
;         keyword is set.
;    oposition: out, optional, type=float
;         Set this keyword to a named variable to obtain the actual position in the
;         graphics window where the image was located. The output position may be
;         different from the input `POSITION`, especially if the `KEEP_ASPECT_RATIO`
;         keyword is set. Note that the output position is also stored in a 
;         FSC_$CGIMAGE common block so that other programs (e.g., cgMap, cgImageInfo, 
;         etc.) can take advantage of this information.
;     outfilename: in, optional, type=string
;        If the `Output` keyword is set, the user will be asked to supply an output
;        filename, unless this keyword is set to a non-null string. In that case, the
;        value of this keyword will be used as the filename and there will be no dialog
;        presented to the user.
;     output: in, optional, type=string, default=""
;        Set this keyword to the type of output desired. Possible values are these::
;            
;            'PS'   - PostScript file
;            'EPS'  - Encapsulated PostScript file
;            'PDF'  - PDF file
;            'BMP'  - BMP raster file
;            'GIF'  - GIF raster file
;            'JPEG' - JPEG raster file
;            'PNG'  - PNG raster file
;            'TIFF' - TIFF raster file
;            
;        Or, you can simply set this keyword to the name of the output file, and the type of
;        file desired will be determined by the file extension. If you use this option, the
;        user will not be prompted to supply the name of the output file.
;            
;        All raster file output is created through PostScript intermediate files (the
;        PostScript files will be deleted), so ImageMagick and Ghostview MUST be installed 
;        to produce anything other than PostScript output. (See cgPS2PDF and PS_END for 
;        details.) And also note that you should NOT use this keyword when doing multiple 
;        plots. The keyword is to be used as a convenient way to get PostScript or raster 
;        output for a single graphics command.
;    overplot: in, optional, type=boolean, default=0
;         Setting this keyword causes the POSITION keyword to be ignored
;         and the image is positioned in the location established by the
;         last graphics command.
;    palette: in, optional, type=byte
;         Set this keyword to a 3x256 or 256x3 byte array containing the RGB color 
;         vectors to be loaded before the image is displayed. Such vectors can be 
;         obtained, for example, from cgLoadCT with the RGB_TABLE keyword::
;               
;                cgLoadCT, 4, /BREWER, /REVERSE, RGB_TABLE=palette
;                cgImage, cgDemoData(7), PALETTE=palette
;    position: in, optional, type=float
;         The location of the image in the output window. This is a four-element 
;         floating array of normalized coordinates of the type given by !P.POSITION 
;         or the POSITION keyword to other IDL graphics commands. The form is [x0, y0, x1, y1].
;         The default is [0.0, 0.0, 1.0, 1.0]. Note that this keyword is ALSO an output 
;         keyword. That is to say, upon return from cgImage this keyword (if passed by 
;         reference) contains the actual position in the window where the image was 
;         displayed. This may be different from the input values if the KEEP_ASPECT_RATIO
;         keyword is set, or if you are using cgImage with the POSITION keyword when !P.MULTI 
;         is set to something other than a single plot. Note that the POSITION keyword should 
;         not, normally, be used when displaying multiple images with !P.MULTI. If it is used,
;         its meaning differs slightly from its normal meaning. !P.MULTI is responsible for 
;         calculating the position of graphics in the display window. Normally, it would be a 
;         mistake to use a POSITION graphics keyword on a graphics command that was being drawn with
;         !P.MULTI. But in this special case, cgImage will use the POSITION coordinates to calculate 
;         an image position in the actual position calculated for the image by !P.MULTI. The main 
;         purpose of this functionality is to allow the user to display images along with
;         color bars when using !P.MULTI. 
;    quiet: in, optional, type=boolean, default=0
;         There are situations when you would prefer that cgIMAGE does not advertise itself by 
;         filling out the FSC_$CGIMAGE common block. For example, if you are using cgImage to 
;         draw a color bar, it would not be necessary. Setting this keyword means that cgImage 
;         just goes quietly about it's business without bothering anyone else.    
;    save: in, optional, type=boolean, default=0
;         Set this to cause a data coordinate system to be established for the image. The XRANGE 
;         and YRANGE keyword values will be used to establish a data coordinate system coincident 
;         with the final image position. Setting the `AXES` keyword automatically sets SAVE=1.
;    scale: in, optional, type=boolean, default=0
;         Set this keyword to byte scale the image before display. If this keyword is not set, 
;         the image is not scaled before display. This keyword will be set automatically by using
;         any of the keywords normally associated with byte scaling an image. Available only with 
;         2D images. If set, STRETCH is set to 1, unless it is set to another value.
;    stretch: in, optional, type=integer/string, default=1
;         The type of scaling performed prior to display. May be specified as a number 
;         or as a string (e.g, 3 or "Log"). Available only with 2D images.
;
;           Number   Type of Stretch
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
;                                      and implemented in the Coyote Library routine ScaleModis.
;    sigma: in, optional, type=float, default=1.0
;         The sigma scale factor in a Gaussian stretch. Available only with 2D images.
;    title: in, optional, type=string, default=""
;         Set this keyword to the title of the image plot. No title will appear with the
;         image unless the `AXES` keyword is also set.
;    top: in, optional, type=integer, default=255
;         If the SCALE keyword is set, the image is scaled before display so that all 
;         displayed pixels have values greater than or equal to BOTTOM and less than 
;         or equal to TOP. Available only with 2D images.
;    tv: in, optional, type=boolean, default=0
;         Setting this keyword makes the cgImage command work much like the brain-dead
;         TV command except that it will get colors right on all output devices. Most of
;         the cgImage keywords are ignored if this keyword is set.
;    window: in, optional, type=boolean, default=0
;         Set this keyword to replace all the commands in a current cgWindow or to
;         create a new cgWindow for displaying this command. If the DISPLAY keyword is
;         also set, a new cgWindow will be created.
;    xrange: in, optional, type=fltarr(2)
;         A two element array giving the X range of the image. By default set to
;         [0, size of image in X].
;    xtitle: in, optional, type=string, default=""
;         The X title of the image plot. Used only if `AXES` is set.
;    yrange: in, optional, type=fltarr(2)
;         A two element array giving the Y range of the image. By default set to
;         [0, size of image in Y].
;    ytitle: in, optional, type=string, default=""
;         The Y title of the image plot. Used only if `AXES` is set.
;    _ref_extra: in, optional, type=varies
;         Any keywords defined for the TV command can be used. This applies only
;         if the TV keyword is set.
;-           
PRO cgImage, image, x, y, $
   ADDCMD=addcmd, $
   ALPHABACKGROUNDIMAGE=alphaBackgroundImage, $
   ALPHABGPOSITION=alphabgpos, $
   ALPHAFGPOSITION=alphafgpos, $
   AXIS=axis, $
   AXES=axes, $
   AXKEYWORDS=axkeywords, $
   BACKGROUND=background, $
   BOTTOM=bottom, $
   BETA=beta, $
   CHARSIZE=charsize, $
   CLIP=clip, $
   COLOR=color, $
   DISPLAY=display, $      ; Make sure this keyword is NOT is the list of keywords passed to cgWindow.
   ERASE=obsolete_erase, $ ; Added for compatibility with TVIMAGE.
   EXPONENT=exponent, $
   FIT_INSIDE=fit_inside, $
   FONT=font, $
   GAMMA=gamma, $
   INTERPOLATE=interp, $
   KEEP_ASPECT_RATIO=keep_aspect, $
   LAYOUT=layout, $
   MARGIN=margin, $
   MAXVALUE=max, $
   MEAN=mean, $
   MISSING_COLOR=missing_color, $
   MISSING_INDEX=missing_index, $
   MISSING_VALUE=missing_value, $
   NEGATIVE=negative, $
   MINUS_ONE=minusOne, $
   MINVALUE=min, $
   MULTIMARGIN=multimargin, $
   NCOLORS=ncolors, $
   NOERASE=noerase, $
   NOINTERPOLATION=obsolete_nointerpolation, $ ; Added for compatibility with TVIMAGE.
   NORMAL=normal, $
   OUTFILENAME=outfilename, $
   OUTPUT=output, $
   OPOSITION=oposition, $
   OVERPLOT=overplot, $
   PALETTE=palette, $
   POSITION=position, $
   QUIET=quiet, $
   SAVE=save, $
   SCALE=scale, $
   SIGMA=sigma, $
   STRETCH=stretch, $
   TITLE=title, $
   TOP=top, $
   TV=tv, $
   WINDOW=window, $
   XRANGE=plotxrange, $
   XTITLE=plotxtitle, $
   YRANGE=plotyrange, $
   YTITLE=plotytitle, $
   _REF_EXTRA=extra

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /Cancel
       ok = Error_Message()
       IF N_Elements(thisMulti) NE 0 THEN !P.Multi = thisMulti
       RETURN
    ENDIF
    
    ; Check parameters.
    IF N_Params() EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: cgImage, image'
        RETURN
    ENDIF
    
    ; Handle obsolete keywords.
    IF N_Elements(obsolete_erase) NE 0 THEN noerase = 1 - obsolete_erase
    IF N_Elements(obsolete_nointerpolation) NE 0 THEN interp = 1 - obsolete_nointerpolation
    
    ; Set up a common block as input to cgImageInfo.
    COMMON FSC_$CGIMAGE, _cgimage_xsize, $    ; The X size of the image.
                         _cgimage_ysize, $    ; The Y size of the imge.
                         _cgimage_winxsize, $ ; The X size of the window displaying the image.
                         _cgimage_winysize, $ ; The Y size of the window displaying the image.
                         _cgimage_position, $ ; The final position of the image in the window.
                         _cgimage_winID, $    ; The window index number of the window displaying the image.
                         _cgimage_current     ; Set to 1 if a call to cgImage is made.
    
    ; Add the command to cgWindow?
    IF Keyword_Set(addcmd) THEN BEGIN
        noerase = 1
        window = 1
    ENDIF
    
    ; Do we want to display the image in a window with the proper aspect ratio?
    IF Keyword_Set(display) THEN BEGIN
    
         ; Are we making a cgWindow?
         IF Keyword_Set(window) THEN BEGIN
             dims = Image_Dimensions(image, XSIZE=xsize, YSIZE=ysize)
             imgaspect = Float(ysize) / xsize
             cgWindow, WASPECT=imgaspect
             addcmd = 0
             noerase = 0
         ENDIF ELSE BEGIN
             IF ~Keyword_Set(addcmd) THEN cgDisplay, /Free, ASPECT=image
         ENDELSE
    
    ENDIF
    
    ; If we want a cgWindow and we can make windows in this device, do so now.
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
        IF wincnt EQ 0 THEN replaceCmd = 0 ELSE replaceCmd=1
        
        ; If you are using a layout, you can't ever erase.
        IF N_Elements(layout) NE 0 THEN noerase = 1
        
        ; If we are adding a command, we have to do something different.
        IF Keyword_Set(addcmd) THEN BEGIN
            cgWindow, 'cgImage', image, x, y, $
               ALPHABACKGROUNDIMAGE=alphaBackgroundImage, $
               ALPHABGPOSITION=alphabgpos, $
               ALPHAFGPOSITION=alphafgpos, $
               AXIS=axis, $
               AXES=axes, $
               AXKEYWORDS=axkeywords, $
               BACKGROUND=background, $
               BOTTOM=bottom, $
               BETA=beta, $
               CHARSIZE=charsize, $
               CLIP=clip, $
               COLOR=color, $
               ERASE=obsolete_erase, $ ; Added for compatibility with TVIMAGE.
               EXPONENT=exponent, $
               FIT_INSIDE=fit_inside, $
               FONT=font, $
               GAMMA=gamma, $
               INTERPOLATE=interp, $
               KEEP_ASPECT_RATIO=keep_aspect, $
               LAYOUT=layout, $
               MARGIN=margin, $
               MAXVALUE=max, $
               MEAN=mean, $
               MISSING_COLOR=missing_color, $
               MISSING_INDEX=missing_index, $
               MISSING_VALUE=missing_value, $
               NEGATIVE=negative, $
               MINUS_ONE=minusOne, $
               MINVALUE=min, $
               MULTIMARGIN=multimargin, $
               NCOLORS=ncolors, $
               NOERASE=noerase, $
               NOINTERPOLATION=obsolete_nointerpolation, $ ; Added for compatibility with TVIMAGE.
               NORMAL=normal, $
               OPOSITION=oposition, $
               OVERPLOT=overplot, $
               PALETTE=palette, $
               POSITION=position, $
               QUIET=quiet, $
               SAVE=save, $
               SCALE=scale, $
               SIGMA=sigma, $
               STRETCH=stretch, $
               TITLE=title, $
               TOP=top, $
               TV=tv, $
               XRANGE=plotxrange, $
               XTITLE=plotxtitle, $
               YRANGE=plotyrange, $
               YTITLE=plotytitle, $
               ADDCMD=1, $
               _EXTRA=extra
                 RETURN
        ENDIF
        
        ; Otherwise, we are replacing the commands in a new or existing window.
        cgWindow, 'cgImage', image, x, y, $
               ALPHABACKGROUNDIMAGE=alphaBackgroundImage, $
               ALPHABGPOSITION=alphabgpos, $
               ALPHAFGPOSITION=alphafgpos, $
               AXIS=axis, $
               AXES=axes, $
               AXKEYWORDS=axkeywords, $
               BACKGROUND=background, $
               BOTTOM=bottom, $
               BETA=beta, $
               CHARSIZE=charsize, $
               CLIP=clip, $
               COLOR=color, $
               ERASE=obsolete_erase, $ ; Added for compatibility with TVIMAGE.
               EXPONENT=exponent, $
               FIT_INSIDE=fit_inside, $
               FONT=font, $
               GAMMA=gamma, $
               INTERPOLATE=interp, $
               KEEP_ASPECT_RATIO=keep_aspect, $
               LAYOUT=layout, $
               MARGIN=margin, $
               MAXVALUE=max, $
               MEAN=mean, $
               MISSING_COLOR=missing_color, $
               MISSING_INDEX=missing_index, $
               MISSING_VALUE=missing_value, $
               NEGATIVE=negative, $
               MINUS_ONE=minusOne, $
               MINVALUE=min, $
               MULTIMARGIN=multimargin, $
               NCOLORS=ncolors, $
               NOERASE=noerase, $
               NOINTERPOLATION=obsolete_nointerpolation, $ ; Added for compatibility with TVIMAGE.
               NORMAL=normal, $
               OPOSITION=oposition, $
               OVERPLOT=overplot, $
               PALETTE=palette, $
               POSITION=position, $
               QUIET=quiet, $
               SAVE=save, $
               SCALE=scale, $
               SIGMA=sigma, $
               STRETCH=stretch, $
               TITLE=title, $
               TOP=top, $
               TV=tv, $
               XRANGE=plotxrange, $
               XTITLE=plotxtitle, $
               YRANGE=plotyrange, $
               REPLACECMD=replacecmd, $
               _EXTRA=extra
             RETURN
    ENDIF
    
    ; Are we doing some kind of output?
    IF (N_Elements(output) NE 0) && (output NE "") THEN BEGIN
    
       ; If the output string has a dot character, then this must be a
       ; filename, and we will determine the type of file from the filename extension.
       IF StrPos(output, '.') NE -1 THEN BEGIN
             root_name = FSC_Base_Filename(output, DIRECTORY=theDir, EXTENSION=ext)
             IF theDir EQ "" THEN CD, CURRENT=theDir
             outfilename = output
             outputSelection = StrUpCase(ext)
       ENDIF
    
       IF N_Elements(outputSelection) EQ 0 THEN outputSelection = StrUpCase(output)
       typeOfOutput = ['PS','EPS','PDF','BMP','GIF','JPEG','JPG','PNG','TIFF', 'TIF']
       void = Where(typeOfOutput EQ outputSelection, count)
       IF count EQ 0 THEN Message, 'Cannot find ' + outputSelection + ' in allowed output types.'
       
       ; Set things up.
       CASE outputSelection OF
          'PS': BEGIN
              ext = '.ps'
              delete_ps = 0
              END    
          'EPS': BEGIN
              ext = '.eps'
              encapsulated = 1
              delete_ps = 0
              END
          'PDF': BEGIN
              ext = '.pdf'
              pdf_flag = 1
              delete_ps = 1
              END     
          'BMP': BEGIN
              ext = '.bmp'
              bmp_flag = 1
              delete_ps = 1
              END      
          'GIF': BEGIN
              ext = '.gif'
              gif_flag = 1
              delete_ps = 1
              END
          'JPEG': BEGIN
              ext = '.jpg'
              jpeg_flag = 1
              delete_ps = 1
              END      
          'JPG': BEGIN
              ext = '.jpg'
              jpeg_flag = 1
              delete_ps = 1
              END
          'PNG': BEGIN
              ext = '.png'
              png_flag = 1
              delete_ps = 1
              END      
          'TIFF': BEGIN
              ext = '.tif'
              tiff_flag = 1
              delete_ps = 1
              END
          'TIF': BEGIN
              ext = '.tif'
              tiff_flag = 1
              delete_ps = 1
              END    
       ENDCASE
              
       ; Do you need a filename?
       IF ( (N_Elements(outfilename) EQ 0) || (outfilename EQ "") ) THEN BEGIN 
            filename = 'cgplot' + ext
            outfilename = cgPickfile(FILE=filename, TITLE='Select Output File Name...', $
                FILTER=ext, /WRITE)
            IF outfilename EQ "" THEN RETURN
       ENDIF
       
       ; We need to know the root name of the file, because we have to make a PostScript
       ; file of the same name. At least we do if the type is not PS or EPS.
       IF (outputSelection NE 'PS') && (outputSelection NE 'EPS') THEN BEGIN
           root_name = FSC_Base_Filename(outfilename, DIRECTORY=theDir)
           IF theDir EQ "" THEN CD, CURRENT=theDir
           ps_filename = Filepath(ROOT_DIR=theDir, root_name + '.ps')
       ENDIF ELSE ps_filename = outfilename
       
       ; Set up the PostScript device.
       PS_Start, FILENAME=ps_filename, ENCAPSULATED=encapsulated, QUIET=1
    
    ENDIF
   
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Which release of IDL is this?
    thisRelease = Float(!Version.Release)
        
    ; Pay attention to !P.Noerase in setting the NOERASE kewyord. This must be
    ; done BEFORE checking the LAYOUT properties.
    IF !P.NoErase NE 0 THEN noerase = !P.NoErase ELSE noerase = Keyword_Set(noerase)
    keep_aspect = Keyword_Set(keep_aspect)
    
    ; Set up the layout, if necessary.
    IF N_Elements(layout) NE 0 THEN BEGIN
       thisMulti = !P.Multi
       totalPlots = layout[0]*layout[1]
       !P.Multi = [0,layout[0], layout[1], 0, 0]
       IF layout[2] EQ 1 THEN BEGIN
            noerase = 1
            overplot = 0
            !P.Multi[0] = 0
       ENDIF ELSE BEGIN
            !P.Multi[0] = totalPlots - layout[2] + 1
       ENDELSE
    ENDIF

    ; Character size has to be determined *after* the layout has been decided.
    IF N_Elements(font) EQ 0 THEN font = !P.Font
    IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharSize(FONT=font)
    IF N_Elements(color) EQ 0 THEN acolorname = 'opposite' ELSE acolorname = color
    interp = Keyword_Set(interp)
    
    ; Doing multiple plots?
    IF Total(!P.Multi) GT 0 THEN multi = 1 ELSE multi = 0
    
    ; Check for image parameter and keywords.
    IF N_Elements(image) EQ 0 THEN MESSAGE, 'You must pass a valid image argument.'
    
    ; Did the user want to scale the image?
    ; If either MIN or MAX are set, this implies SCALE=1.
    ; If min LT 0 or max GT 255, this implies SCALE=1.
    ; If NCOLORS is used, this implies SCALE=1.
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
    SetDefaultValue, alphabgpos, [0.0, 0.0, 1.0, 1.0]
    SetDefaultValue, alphafgpos, [0.0, 0.0, 1.0, 1.0]
    SetDefaultValue, beta, 3.0
    SetDefaultValue, clip, 2
    SetDefaultValue, exponent, 4.0
    SetDefaultValue, gamma, 1.5 
    SetDefaultValue, mean, 1.0
    SetDefaultValue, missing_index, 255
    SetDefaultValue, negative, 0
    SetDefaultValue, sigma, 1.0
    SetDefaultValue, stretch, 0
    
    ; Make sure you can specify the type of stretch with a string name.
    IF Size(stretch, /TNAME) EQ 'STRING' THEN BEGIN
        stretches = ['None', 'Linear', 'Clip', 'Gamma', 'Log', 'ASinh', $
              'SquareRoot', 'Equalization', 'Gaussian', 'MODIS']
       
       index = Where(StrUpCase(stretch) EQ StrUpCase(stretches), count)
       IF count GT 0 THEN stretch=index ELSE Message, 'Cannot find stretch: ' + StrUpCase(stretch)
    ENDIF
    IF stretch NE 0 THEN scale = 1
    
    ; Check for mis-spelling of AXES as AXIS.
    IF Keyword_Set(axis) THEN axes = 1    
    axes = Keyword_Set(axes)
    
    ; If you want axes, then save the coordinate system, unless s
    ; pecifically asked not to.
    IF axes THEN IF N_Elements(save) EQ 0 THEN save = 1
    
    ; If axes are set and MARGIN and POSITION are NOT set and you are NOT
    ; doing multiplots, then set a normal "plot" margin.
    IF Keyword_Set(axes) AND ((N_Elements(margin) EQ 0) AND (N_Elements(position) EQ 0) $
        AND (multi EQ 0)) THEN margin = 0.1
    
    ; Check other keywords.
    interp = Keyword_Set(interp)
    IF N_Elements(minusOne) EQ 0 THEN minusOne = 0
    minusOne = Keyword_Set(minusOne)
        
    IF N_Elements(background) EQ 0 THEN background = 'white'
    IF Size(background, /TNAME) EQ 'STRING' THEN BEGIN
        IF StrUpCase(background) EQ 'WHITE' THEN BEGIN
           IF N_Elements(acolorname) EQ 0 THEN acolorname = 'black 
        ENDIF 
    ENDIF
    noerase = Keyword_Set(noerase) ; Don't change, used in PS output.
    
    ; Choose an axis color.
    IF N_Elements(acolorname) EQ 0 THEN BEGIN
           IF !D.Name EQ 'PS' THEN BEGIN
                acolorname = 'OPPOSITE' 
           ENDIF ELSE BEGIN
                IF (!D.Window GE 0) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
                    pixel = cgSnapshot(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF (Total(pixel) EQ 765) THEN acolorname = 'BLACK'
                    IF (Total(pixel) EQ 0) THEN acolorname = 'WHITE'
                    IF N_Elements(acolorname) EQ 0 THEN acolorname = 'OPPOSITE'
                ENDIF ELSE acolorname = 'OPPOSITE'
           ENDELSE
     ENDIF
    IF N_Elements(acolorname) EQ 0 THEN acolor = !P.Color ELSE acolor = acolorname
    IF Size(acolor, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN acolor = Byte(color)
    IF Size(acolor, /TYPE) LE 2 THEN acolor = StrTrim(Fix(acolor),2)
 
    ; Load the color palette if you are using one.
    IF N_Elements(palette) NE 0 THEN BEGIN
        IF Size(palette, /N_DIMENSIONS) NE 2 THEN Message, 'Color palette is not a 3xN array.'
        dims = Size(palette, /DIMENSIONS)
        threeIndex = Where(dims EQ 3)
        IF ((threeIndex)[0] LT 0) THEN Message, 'Color palette is not a 3xN array.'
        IF threeIndex[0] EQ 0 THEN palette = Transpose(palette)
        TVLCT, p_red, p_grn, p_blu, /Get ; Save the color vectors before loading the palette.
        TVLCT, palette
    ENDIF
    
    ; If you have a missing color, load it at the missing color index.
    IF N_Elements(missing_color) NE 0 THEN TVLCT, cgColor(missing_color, /Triple), missing_index
    
    ; Before you do anything, get the current color table vectors
    ; so they can be restored later. Must do AFTER loading a palette!
    TVLCT, rr, gg, bb, /Get
    
    ; If this is an image with an alpha channel, and there is no alphachannel background image
    ; supplied, you will have to take a snapshot of the current window right now before you
    ; erase the window. If the smallest image dimension is a 4, then we will assume this is
    ; an image with an alpha channel.
    IF Min(Size(image, /DIMENSIONS)) EQ 4 THEN BEGIN
    
       ; We can get the background image on devices that support windows.
       IF (!D.Flags AND 256) NE 0 THEN BEGIN
           IF N_Elements(alphabackgroundImage) EQ 0 THEN BEGIN
               alphabackgroundImage = cgSnapshot()
           ENDIF
       ENDIF ELSE BEGIN
           IF N_Elements(alphabackgroundImage) EQ 0 THEN BEGIN
               ; Otherwise, blend this with a white image.
               alphabackgroundImage = BytArr(100,100) + 255B
           ENDIF
       ENDELSE
    ENDIF
    
    ; Do you need to erase the window before image display?
    IF ~Keyword_Set(noerase) && (!P.MULTI[0] EQ 0) && (N_Elements(layout) EQ 0) THEN BEGIN
         IF (!D.Flags AND 256) NE 0 THEN BEGIN
            cgErase, background
         ENDIF ELSE BEGIN
            IF (!D.NAME EQ 'Z') THEN BEGIN
                cgErase, background
            ENDIF
            
            ; Do you need a PostScript background color? Lot's of problems here!
            ; Basically, I MUST draw a plot to advance !P.MULTI. But, drawing a
            ; plot of any sort erases the background color. So, I have to draw a 
            ; plot, store the new system variables, then draw my background, etc.
            ; I have tried LOTS of options. This is the only one that worked.
            IF !D.Name EQ 'PS' THEN BEGIN
               IF ~noerase THEN BEGIN
               
                   ; I only have to do this, if this is the first plot.
                   IF !P.MULTI[0] EQ 0 THEN BEGIN
                   
                        ; Save the current system variables. Will need to restore later.
                        bangx = !X
                        bangy = !Y
                        bangp = !P
                        
                        ; Draw the plot that doesn't draw anything.
                        Plot, [0], POSITION=position, /NODATA, XSTYLE=4, YSTYLE=4, ZSTYLE=4
                        
                        ; Save the "after plot" system variables. Will use later. 
                        afterx = !X
                        aftery = !Y
                        afterp = !P     
                        
                        ; Draw the background color and set the variables you will need later.
                        PS_Background, background
                        psnodraw = 1
                        tempNoErase = 1
                        
                        ; Restore the original system variables so that it is as if you didn't
                        ; draw the invisible plot.
                        !X = bangx
                        !Y = bangy
                        !P = bangp
                        TVLCT, rr, gg, bb
                    ENDIF ELSE tempNoErase = noerase
                ENDIF ELSE tempNoErase = noerase
             ENDIF ELSE tempNoErase = noerase
         ENDELSE
    ENDIF
    
    ; Need a margin around the plot?
    IF (N_Elements(margin) GT 0) THEN BEGIN
       IF Keyword_Set(margin) EQ 0 THEN BEGIN
          IF N_Elements(multimargin) EQ 0 THEN multimargin=[0., 0., 0., 0.] 
       ENDIF ELSE BEGIN
          IF N_Elements(multimargin) EQ 0 THEN multimargin=[1., 1., 1., 1.]
       ENDELSE
       IF margin[0] EQ 1 THEN margin = 0.075  ; Comes from /MARGIN
    ENDIF 
    
    ; Make sure the multimargin has four elements.
    IF N_Elements(multimargin) EQ 0 THEN multimargin = [0., 0., 0., 0.] 
    IF N_Elements(multimargin) EQ 1 THEN multimargin = [multimargin, multimargin, multimargin, multimargin]
    IF N_Elements(multimargin) NE 4 THEN Message, 'The keyword MULTIMARGIN must be a four-element array.'
    
    ; Check image size.
    s = Size(image)
    IF s[0] LT 2 OR s[0] GT 3 THEN $
       MESSAGE, 'Argument does not appear to be an image. Returning...'
    alphaImage = 0
    
    ; Allow 24-bit images and 2D images that are sent in as 3D
    ; arrays where one dimension is a 1. 24-bit images can have an
    ; alpha channel.
    IF s[0] EQ 3 THEN BEGIN
    
       ; We are going to fake doing something with the alpha channel here.
       i = Where(s[1:3] EQ 3, threeCnt)
       i = Where(s[1:3] EQ 4, fourCnt)
       IF threeCnt EQ 0 AND fourCnt NE 0 THEN BEGIN
            s[i+1] = 3
            alphaImage = 1
       ENDIF ELSE alphaImage = 0
       
       ; Now handle normal 24-bit images and suspect 2D images.
       IF (s[1] NE 3L) AND (s[2] NE 3L) AND (s[3] NE 3L) THEN BEGIN
          IF (s[1] NE 1L) AND (s[2] NE 1L) AND (s[3] NE 1L) THEN BEGIN
             MESSAGE, 'Argument does not appear to be a 24-bit image. Returning...'
          ENDIF ELSE BEGIN
             IF s[1] EQ 1 THEN single = 1
             IF s[2] EQ 1 THEN single = 2
             IF s[3] EQ 1 THEN single = 3
             CASE single OF
                1: image = Reform(image, s[2], s[3])
                2: image = Reform(image, s[1], s[3])
                3: image = Reform(image, s[1], s[2])
             ENDCASE
             s = Size(image)
          ENDELSE
       ENDIF
    ENDIF ELSE s = Size(image)
    
    ; If a window is not open, open one, otherwise in X devices you get incorrect
    ; window size information the first time you call cgImage.
    IF (!D.FLAGS AND 256) NE 0 THEN IF (!D.Window EQ -1) THEN cgDisplay
    
    ; Check for position and overplot keywords.
    IF N_Elements(position) EQ 0 THEN BEGIN
       IF Keyword_Set(multi) AND (Keyword_Set(overplot) NE 1) THEN BEGIN
          ; Draw the invisible plot to get plot position.
          IF Size(background, /TNAME) EQ 'STRING' THEN background = cgColor(background)
          Plot, Findgen(11), XStyle=4, YStyle=4, /NoData, Background=background, $
             XMargin=multimargin[[1,3]], YMargin=multimargin[[0,2]], $
             NOERASE=N_Elements(layout) EQ 0 ? tempNoErase : 1
          position = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
          TVLCT, rr, gg, bb
       ENDIF ELSE BEGIN
          IF Keyword_Set(overplot) THEN BEGIN
             position = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
          ENDIF ELSE position = [0.0, 0.0, 1.0, 1.0]
       ENDELSE
    ENDIF ELSE BEGIN
       IF Keyword_Set(multi) AND (Keyword_Set(overplot) NE 1)THEN BEGIN
          ; Draw the invisible plot to get plot position.
          IF Size(background, /TNAME) EQ 'STRING' THEN background = cgColor(background)
          Plot, Findgen(11), XStyle=4, YStyle=4, /NoData, Background=background, $
              XMargin=multimargin[[1,3]], YMargin=multimargin[[0,2]], $
             NOERASE=N_Elements(layout) EQ 0 ? tempNoErase : 1
          TVLCT, rr, gg, bb
          ; Use position coordinates to indicate position in this set of coordinates.
          xrange = !X.Window[1] - !X.Window[0]
          xstart = !X.Window[0] + position[0]*xrange
          xend = xrange * (position[2] - position[0]) + xstart
    
          yrange = !Y.Window[1] - !Y.Window[0]
          ystart = !Y.Window[0] + position[1]*yrange
          yend = yrange * (position[3] - position[1]) + ystart
    
          ; New position based on !P.MULTI position.
          position = [xstart, ystart, xend, yend]
    
       ENDIF ELSE BEGIN
          IF Keyword_Set(overplot) THEN BEGIN
             position = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
          ENDIF ELSE position = Float(position)
       ENDELSE
    ENDELSE
    
    ; Check for margin keyword.
    IF (Keyword_Set(multi) EQ 0) AND (Keyword_Set(overplot) EQ 0) THEN BEGIN
       IF N_Elements(margin) NE 0 THEN BEGIN
               margin = 0.0 > margin < 0.33
               position = [position[0] + margin, position[1] + margin, $
                           position[2] - margin, position[3] - margin]
       ENDIF
    ENDIF
    
    ; 2D image.
    IF s[0] EQ 2 THEN BEGIN
    
       imgXsize = FLOAT(s[1])
       imgYsize = FLOAT(s[2])
       true = 0
    
       ; Decomposed color off if device supports it.
       CASE  StrUpCase(!D.NAME) OF
            'X': BEGIN
                Device, Get_Visual_Depth=thisDepth
                IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
                Device, Decomposed=0
                ENDCASE
            'WIN': BEGIN
    
                Device, Get_Visual_Depth=thisDepth
                IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
                Device, Decomposed=0
                ENDCASE
            'MAC': BEGIN
                Device, Get_Visual_Depth=thisDepth
                IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
                Device, Decomposed=0
                ENDCASE
            'Z': BEGIN
                ; Fix for 24-bit Z-buffer.
                IF (thisRelease GE 6.4) THEN BEGIN
                   Device, Get_Decomposed=thisDecomposed, Get_Pixel_Depth=thisDepth
                   Device, Decomposed=0
                ENDIF ELSE thisDepth = 8
                ENDCASE
            'PS': BEGIN
                IF (thisRelease GE 7.1) THEN BEGIN
                   thisDecomposed = DecomposedColor(Depth=thisDepth)
                   Device, Decomposed=0
                ENDIF ELSE thisDepth = 8
                ENDCASE
            ELSE: thisDepth = 8
       ENDCASE
    
    ENDIF
    
    ; 3D image.
    IF s[0] EQ 3 THEN BEGIN
    
      ; What kind of pixel interleaving?
      IF s[1] EQ 3 THEN true = 1 ; Pixel interleaved
      IF s[2] EQ 3 THEN true = 2 ; Row interleaved
      IF s[3] EQ 3 THEN true = 3 ; Band interleaved
    
       ; Decomposed color on if device supports it.
       CASE StrUpCase(!D.NAME) OF
          'X': BEGIN
             Device, Get_Visual_Depth=thisDepth
             IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
             IF thisDepth GT 8 THEN Device, Decomposed=1
             ENDCASE
          'WIN': BEGIN
             Device, Get_Visual_Depth=thisDepth
             IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
             IF thisDepth GT 8 THEN Device, Decomposed=1
             ENDCASE
          'MAC': BEGIN
             Device, Get_Visual_Depth=thisDepth
             IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
             IF thisDepth GT 8 THEN Device, Decomposed=1
             ENDCASE
          'Z': BEGIN
             ; Fix for 24-bit Z-buffer.
             IF (Float(!Version.Release) GE 6.4) THEN BEGIN
                Device, DECOMPOSED=1, Set_Pixel_Depth=24
                thisDepth = 24
             ENDIF ELSE thisDepth = 8
             ENDCASE
          'PS': BEGIN
             IF (Float(!Version.Release) GE 7.1) THEN BEGIN
                   thisDecomposed = DecomposedColor(Depth=thisDepth)
                   TVLCT, r, g, b, /GET
                   LoadCT, 0, /Silent
                   Device, DECOMPOSED=1, BITS_PER_PIXEL=8, COLOR=1
                   TVLCT, r, g, b
             ENDIF ELSE thisDepth = 8
             ENDCASE
          
          ELSE: thisDepth = 8
       ENDCASE
    
       CASE true OF
          1: BEGIN
             imgXsize = FLOAT(s[2])
             imgYsize = FLOAT(s[3])
             ENDCASE
          2: BEGIN
             imgXsize = FLOAT(s[1])
             imgYsize = FLOAT(s[3])
             ENDCASE
          3: BEGIN
             imgXsize = FLOAT(s[1])
             imgYsize = FLOAT(s[2])
             ENDCASE
       ENDCASE
    
    ENDIF
    
    ; Check for TV keyword. If present, then act like a TV command.
    IF Keyword_Set(tv) THEN BEGIN
    
       IF N_Params() GE 3 OR N_Params() EQ 1 THEN BEGIN
         IF N_Elements(x) EQ 0 THEN x = 0
         IF N_Elements(y) EQ 0 THEN y = 0
         IF Keyword_Set(normal) THEN BEGIN
            IF alphaImage THEN BEGIN
               outImage = cgImage_Prepare_Alpha(image, alphaBackgroundImage, $
                    TV=1, ALPHABGPOSITION=alphapos, ALPHAFGPOSITION=alphafgpos)
               TV, outImage, x, y, True=3, _STRICT_EXTRA=extra, /Normal
            ENDIF ELSE BEGIN
               CASE scale OF
                    0: TV, image, x, y, True=true, _STRICT_EXTRA=extra, /Normal 
                    1: TV, BytScl(image, Top=top, Max=max, Min=min) + bottom, $
                           x, y, True=true, _STRICT_EXTRA=extra, /Normal
                ENDCASE
            ENDELSE
         ENDIF ELSE BEGIN
            IF alphaImage THEN BEGIN
               outImage = cgImage_Prepare_Alpha(image, alphaBackgroundImage, $
                    TV=1, ALPHABGPOSITION=alphapos, ALPHAFGPOSITION=alphafgpos)
               TV, outImage, x, y, True=3, _STRICT_EXTRA=extra, /Device
            ENDIF ELSE BEGIN
               CASE scale OF
                   0: TV, image, x, y, True=true, _STRICT_EXTRA=extra, /Device
                   1: TV, BytScl(image, Top=top, Max=max, Min=min) + bottom, $
                           x, y, True=true, _STRICT_EXTRA=extra, /Device
                ENDCASE
            ENDELSE
         ENDELSE
       ENDIF ELSE BEGIN
         IF N_Params() EQ 2 THEN BEGIN
            IF Keyword_Set(normal) THEN BEGIN
                IF alphaImage THEN BEGIN
                   outImage = cgImage_Prepare_Alpha(image, alphaBackgroundImage, $
                       TV=1, ALPHABGPOSITION=alphapos, ALPHAFGPOSITION=alphafgpos)
                   TV, outImage, x,  True=3, _STRICT_EXTRA=extra, /Normal
                ENDIF ELSE BEGIN
                   CASE scale OF 
                        0: TV, image, x, True=true, _STRICT_EXTRA=extra, /Normal
                        1: TV, BytScl(image, Top=top, Max=max, Min=min) + bottom, $
                                x, True=true, _STRICT_EXTRA=extra, /Normal
                   ENDCASE
                ENDELSE
             ENDIF ELSE BEGIN
                IF alphaImage THEN BEGIN
                   outImage = cgImage_Prepare_Alpha(image, alphaBackgroundImage, $
                        TV=1, ALPHABGPOSITION=alphabgpos, ALPHAFGPOSITION=alphafgpos)
                   TV, outImage, x,  True=3, _STRICT_EXTRA=extra, /Device
                ENDIF ELSE BEGIN
                   CASE scale OF 
                        0: TV, image, x, True=true, _STRICT_EXTRA=extra, /Device
                        1: TV, BytScl(image, Top=top, Max=max, Min=min) + bottom, $
                                x, True=true, _STRICT_EXTRA=extra, /Device
                   ENDCASE
                ENDELSE
             ENDELSE
         ENDIF
       ENDELSE
       GoTo, restoreDecomposed
    
    ENDIF
    
    ; Maintain aspect ratio (ratio of height to width)?
    IF KEYWORD_SET(keep_aspect) THEN BEGIN
    
       ; Find aspect ratio of image.
       ratio = FLOAT(imgYsize) / imgXSize
    
       ; Find the proposed size of the image in pixels without aspect
       ; considerations.
       xpixSize = (position(2) - position(0)) * !D.X_VSize
       ypixSize = (position(3) - position(1)) * !D.Y_VSize
    
       ; Try to fit the image width. If you can't maintain
       ; the aspect ratio, fit the image height.
       trialX = xpixSize
       trialY = trialX * ratio
       IF trialY GT ypixSize THEN BEGIN
          trialY = ypixSize
          trialX = trialY / ratio
       ENDIF
    
       ; Recalculate the position of the image in the window.
       position[0] = (((xpixSize - trialX) / 2.0) / !D.X_VSize) + position[0]
       position[2] = position[0] + (trialX/FLOAT(!D.X_VSize))
       position[1] = (((ypixSize - trialY) / 2.0) / !D.Y_VSize)  + position[1]
       position[3] = position[1] + (trialY/FLOAT(!D.Y_VSize))
    
    ENDIF
    
    ; Set the output position.
    oposition = position
    
    ; Calculate the image size and start locations. The plus and minus
    ; factor values are designed to keep the image completely inside the axes.
    ; In other words, if you draw the axes first, then put the image in
    ; the display window, the axes should remain visible and not be covered
    ; up by the image. Do this only if the user requests it with the FIT_INSIDE
    ; keyword.
    IF Keyword_Set(fit_inside) THEN factor = 1 ELSE factor = 0
    xsize = Ceil((position[2] - position[0]) * !D.X_VSIZE) - factor
    ysize = Ceil((position[3] - position[1]) * !D.Y_VSIZE) - factor
    xstart = Round(position[0] * !D.X_VSIZE) + factor
    ystart = Round(position[1] * !D.Y_VSIZE) + factor
    
    ; Display the image. Sizing different for scalable pixels devices.
    IF (!D.Flags AND 1) NE 0 THEN BEGIN
    
       ; Need a gray-scale color table if this is a true
       ; color image.
       IF true GT 0 THEN LOADCT, 0, /Silent
       IF alphaImage THEN BEGIN
           outImage = cgImage_Prepare_Alpha(image, alphaBackgroundImage, $
                       ALPHABGPOSITION=alphabgpos, ALPHAFGPOSITION=alphafgpos)
           TV, outImage, xstart, ystart, XSIZE=xsize, YSIZE=ysize, _STRICT_EXTRA=extra, True=3
       ENDIF ELSE BEGIN
           TV, cgImage_Prepare_Output(image, $
                       BOTTOM=bottom, $
                       BETA=beta, $
                       CLIP=clip, $
                       EXPONENT=exponent, $
                       GAMMA=gamma, $
                       INTERPOLATE=interpolate, $
                       MAXVALUE=maxvalue, $
                       MEAN=mean, $
                       MINUS_ONE=minus_one, $
                       MINVALUE=minvalue, $
                       MISSING_INDEX=missing_index, $
                       MISSING_VALUE=missing_value, $
                       NEGATIVE=negative, $
                       SCALE=scale, $
                       STRETCH=stretch, $
                       SIGMA=sigma, $
                       TOP=top), xstart, ystart, XSIZE=xsize, $
                       YSIZE=ysize, _STRICT_EXTRA=extra, True=true
       ENDELSE
    ENDIF ELSE BEGIN ; All other devices.
    
       CASE true OF
          0: BEGIN
               TV, cgImage_Prepare_Output(image, xsize, ysize, $
                       BOTTOM=bottom, $
                       BETA=beta, $
                       CLIP=clip, $
                       EXPONENT=exponent, $
                       GAMMA=gamma, $
                       INTERPOLATE=interpolate, $
                       MAXVALUE=maxvalue, $
                       MEAN=mean, $
                       MINUS_ONE=minus_one, $
                       MINVALUE=minvalue, $
                       MISSING_INDEX=missing_index, $
                       MISSING_VALUE=missing_value, $
                       NEGATIVE=negative, $
                       SCALE=scale, $
                       STRETCH=stretch, $
                       SIGMA=sigma, $
                       TOP=top), xstart, ystart, _STRICT_EXTRA=extra
             END
          1: IF thisDepth GT 8 THEN BEGIN
                IF alphaImage THEN BEGIN
                    outImage = cgImage_Prepare_Alpha(image, alphaBackgroundImage, $
                       ALPHABGPOSITION=alphabgpos, ALPHAFGPOSITION=alphafgpos)
                    TV, FSC_Resize_Image(outImage, xsize, ysize, INTERP=interp, $
                       MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=3
                ENDIF ELSE BEGIN
                    CASE scale OF
                        0: TV, FSC_Resize_Image(image, xsize, ysize, INTERP=interp, $
                                MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=1
                        1: TV, BYTSCL(FSC_Resize_Image(image, xsize, ysize, $
                                INTERP=interp, MINUS_ONE=minusOne), Top=top-bottom, $
                                Max=max, Min=min) + bottom, xstart, ystart, _STRICT_EXTRA=extra, True=1
                     ENDCASE
                ENDELSE
             ENDIF ELSE BEGIN
                IF alphaImage THEN BEGIN
                    outImage = cgImage_Prepare_Alpha(image, alphaBackgroundImage, $
                       ALPHABGPOSITION=alphabgpos, ALPHAFGPOSITION=alphafgpos)
                    image2d = Color_Quan(outImage, 3, r, g, b, _EXTRA=extra)               
                ENDIF ELSE BEGIN
                    image2d = Color_Quan(image, 1, r, g, b, _EXTRA=extra)   
                ENDELSE                
                TVLCT, r, g, b
                TV, FSC_Resize_Image(image2d, xsize, ysize, INTERP=0, $
                   MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=0
             ENDELSE
          2: IF thisDepth GT 8 THEN BEGIN
                IF alphaImage THEN BEGIN
                    outImage = cgImage_Prepare_Alpha(image, alphaBackgroundImage, $
                       ALPHABGPOSITION=alphabgpos, ALPHAFGPOSITION=alphafgpos)
                    TV, FSC_Resize_Image(outImage, xsize, ysize, INTERP=interp, $
                       MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=3
                ENDIF ELSE BEGIN
                    CASE scale OF
                        0: TV, FSC_Resize_Image(image, xsize, ysize, INTERP=interp, $
                                MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=2
                        1: TV, BYTSCL(FSC_Resize_Image(image, xsize, ysize, $
                                INTERP=interp, MINUS_ONE=minusOne), Top=top-bottom, Max=max, $
                                Min=min) + bottom, xstart, ystart, _STRICT_EXTRA=extra, True=2
                    ENDCASE
                ENDELSE
             ENDIF ELSE BEGIN
                IF alphaImage THEN BEGIN
                    outImage = cgImage_Prepare_Alpha(image, alphaBackgroundImage, ALPHABGPOSITION=alphapos)
                    image2d = Color_Quan(outImage, 3, r, g, b, _EXTRA=extra)               
                ENDIF ELSE BEGIN
                    image2d = Color_Quan(image, 2, r, g, b, _EXTRA=extra)
                ENDELSE                
                TVLCT, r, g, b
                TV, FSC_Resize_Image(image2d, xsize, ysize, INTERP=0, $
                   MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=0
             ENDELSE
          3: IF thisDepth GT 8 THEN BEGIN
                IF alphaImage THEN BEGIN
                    outImage = cgImage_Prepare_Alpha(image, alphaBackgroundImage, $
                       ALPHABGPOSITION=alphabgpos, ALPHAFGPOSITION=alphafgpos)
                    TV, FSC_Resize_Image(outImage, xsize, ysize, INTERP=interp, $
                       MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=3
                ENDIF ELSE BEGIN
                    CASE scale OF
                        0: TV, FSC_Resize_Image(image, xsize, ysize, INTERP=interp, $
                                MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=3
                        1: TV, BYTSCL(FSC_Resize_Image(image, xsize, ysize, $
                                INTERP=interp, MINUS_ONE=minusOne), Top=top-bottom, Max=max, $
                                Min=min) + bottom, xstart, ystart, _STRICT_EXTRA=extra, True=3
                    ENDCASE
                ENDELSE
             ENDIF ELSE BEGIN
                IF alphaImage THEN BEGIN
                    outImage = cgImage_Prepare_Alpha(image, alphaBackgroundImage, $
                       ALPHABGPOSITION=alphabgpos, ALPHAFGPOSITION=alphafgpos)
                    image2d = Color_Quan(outImage, 3, r, g, b, _EXTRA=extra)               
                ENDIF ELSE BEGIN
                    image2d = Color_Quan(image, 3, r, g, b, _EXTRA=extra)
                ENDELSE
                TVLCT, r, g, b
                TV, FSC_Resize_Image(image2d, xsize, ysize, INTERP=0, $
                   MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=0
             ENDELSE
      ENDCASE
    ENDELSE
        
    ; Restore Decomposed state if necessary.
    RestoreDecomposed:
    
    ; If this is the first plot in PS, then we have to make it appear that we have
    ; drawn a plot, even though we haven't.
    IF N_Elements(psnodraw) EQ 1 THEN BEGIN
        !X = afterX
        !Y = afterY
        !P = afterP
    ENDIF
     
    CASE StrUpCase(!D.NAME) OF
       'X': BEGIN
          IF thisRelease GE 5.2 THEN Device, Decomposed=thisDecomposed
          ENDCASE
       'WIN': BEGIN
          IF thisRelease GE 5.2 THEN Device, Decomposed=thisDecomposed
          ENDCASE
       'MAC': BEGIN
          IF thisRelease GE 5.2 THEN BEGIN
             Device, Decomposed=thisDecomposed
    
             ; Here is a hack that fixes a longstanding Mac problem with
             ; color tables after changing the decomposed state.
             TV, [0]
          ENDIF
          ENDCASE
       'Z': BEGIN
          IF thisRelease GE 6.4 THEN Device, Decomposed=thisDecomposed
          ENDCASE
       'PS': BEGIN
          IF thisRelease GE 7.1 THEN BEGIN
              Device, DECOMPOSED=thisDecomposed
              IF N_Elements(r) NE 0 THEN TVLCT, r, g, b
          ENDIF
          ENDCASE
       ELSE:
    ENDCASE

    ; Set up common block parameters, but only if device supports windows.
    ; And only if the QUIET flag is not turned on.
    IF ~Keyword_Set(quiet) THEN BEGIN
        IF (!D.FLAGS AND 256) NE 0 THEN BEGIN
            _cgimage_xsize = imgXsize
            _cgimage_ysize = imgYsize
            _cgimage_winID = !D.Window
            _cgimage_winxsize = !D.X_Size
            _cgimage_winysize = !D.Y_Size
            _cgimage_position = position
            _cgimage_current = 1
        ENDIF 
    ENDIF
    
    ; Save plot system variables.
    bangp = !P
    bangx = !X
    bangy = !Y
     
    ; Need a data range?
    IF N_Elements(plotxrange) EQ 0 THEN plotxrange = [0, imgXsize]
    IF N_Elements(plotyrange) EQ 0 THEN plotyrange = [0, imgYsize]
    
    ; If the user wanted axes, draw them now.
    IF axes THEN BEGIN
    
        cgPLOT, [0], FONT=font, /NODATA, /NOERASE, XRANGE=plotxrange, YRANGE=plotyrange, $
            XSTYLE=1, YSTYLE=1, POSITION=position, AXISCOLOR=acolor, $
            XTITLE=plotxtitle, YTITLE=plotytitle, TITLE=title, CHARSIZE=charsize, $
            _STRICT_EXTRA=axkeywords
            
    ENDIF ELSE BEGIN
    
        ; If you are saving the data coordinate space, draw invisible axes.
        IF Keyword_Set(save) THEN BEGIN
            PLOT, [0], /NODATA, /NOERASE, XRANGE=plotxrange, YRANGE=plotyrange, $
                XSTYLE=5, YSTYLE=5, POSITION=position, _STRICT_EXTRA=axkeywords
        ENDIF
    
    ENDELSE

    ; Clean up after yourself.
    IF (!D.Name NE 'Z') THEN BEGIN
        TVLCT, rr, gg, bb
        ; If you loaded a color palette, restore the before color vectors.
        IF N_Elements(p_red) NE 0 THEN TVLCT, p_red, p_grn, p_blu
    ENDIF
    IF ~Keyword_Set(save) THEN BEGIN
        !P = bangp
        !X = bangx
        !Y = bangy
    ENDIF

    ; Clean up if you are using a layout.
    IF N_Elements(layout) NE 0 THEN !P.Multi = thisMulti

    ; Are we producing output? If so, we need to clean up here.
    IF (N_Elements(output) NE 0) && (output NE "") THEN BEGIN
    
        ; Close the PostScript file and create whatever output is needed.
        PS_END, DELETE_PS=delete_ps, $
             BMP=bmp_flag, $
             GIF=gif_flag, $
             JPEG=jpeg_flag, $
             PDF=pdf_flag, $
             PNG=png_flag, $
             TIFF=tiff_flag
         basename = File_Basename(outfilename)
         dirname = File_Dirname(outfilename)
         IF dirname EQ "." THEN CD, CURRENT=dirname
         Print, 'Output File: ' + Filepath(ROOT_DIR=dirname, basename)
    ENDIF
    
END
