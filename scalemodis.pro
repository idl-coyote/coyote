;+
; NAME:
;  SCALEMODIS
;
; PURPOSE:
;
;  MODIS corrected reflectance images often appear drab when initially processed
;  and displayed on a computer using BYTSCL. In fact, the resulting true-color 
;  images look nothing like the images you can find on the MODIS Rapid Response
;  web page (http://rapidfire.sci.gsfc.nasa.gov/gallery/). After poking around on
;  the Internet for awhile, I discovered that the Rapid Response Team doesn't use
;  BYTSCL to prepare the images. Rather, they selectively scale portions of the
;  reflectance image, using a piecewise scaling with different slopes. This program
;  implements this Rapid Response Team algorithm.
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: david@idlcoyote.com
;   Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; CATEGORY:
;
;  Graphics
;
; CALLING SEQUENCE:
;
;  scaledBand = ScaleModis(red, green, blue)
;
; ARGUMENTS:
;
;  red:           A two-dimensional array representing the corrected reflectance
;                 values of a MODIS image. This is a required parameter. If the
;                 green and blue parameters are also used, this parameter will 
;                 represent the red band of a RGB 24-bit scaled image that is returned.
;                 
;  green:         If the three parameters--red, green, and blue--are present, the returned
;                 array is a 24-bit true-color image, scaled appropriately. This parameter
;                 is used as the green band in such an image. The parameter is a two-dimensional
;                 array of corrected reflectance values.
;             
;  blue:          If the three parameters--red, green, and blue--are present, the returned
;                 array is a 24-bit true-color image, scaled appropriately. This parameter
;                 is used as the blue band in such an image. The parameter is a two-dimensional
;                 array of corrected reflectance values.
;                 
; KEYWORD PARAMETERS:
;
;  RANGE:         A two-dimensional array that the input bands are first scaled into, prior to
;                 the differential scaling using the MODIS Rapid Response algorithm. The default
;                 input range is [-0.01, 1.10]. These values will be used to set the MIN and MAX
;                 keywords for the BYTSCL command in the initial scaling of the input bands.
;
;  CLOUD:         The MODIS Rapid Response team uses a slightly different scaling algorithm when
;                 the idea is to emphasize clouds in a MODIS scene. Set this keyword to use the
;                 alternate cloud scaling algorithm.
;
; OUTPUTS:
; 
;  scaledBand:    If a single 2D array is passed as the argument, then scaledBand is the scaled
;                 2D output array. If all three arguments are passed to the program, then scaledBand
;                 is a scaled 24-bit image that represents a true-color or false color representation
;                 of the three input bands.
;                 
; MODIFICATION HISTORY:
;
;  Written by: David W. Fanning, July 2009, using the IDL programs MODIS_FALSE_COLOR and
;     and SCALE_IMAGE for inspiration. I found these programs on the Internet when poking  
;     around MODIS web pages. I suspect, but I am not sure, these programs were originally  
;     written by Liam Gumley.
;  Minor changes to the ScaleIt function to be sure partitioning is done correctly. 5 Aug 2009. DWF.
;-
;
;******************************************************************************************;
;  Copyright (c) 2009, by Fanning Software Consulting, Inc.                                ;
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
FUNCTION ScaleModis_ScaleIt, image, input, output

    ; This function performs the actual piecewise scaling and returns
    ; the differentially scaled image.
    
    ON_Error, 2 ; Return to caller.
    
    ; Create the output array.
    scaled = image * 0B
    
    ; Check the input vector lengths to be sure they are the same.
    inum = N_Elements(input)
    onum = N_Elements(output)
    IF inum NE onum THEN Message, 'Scaling vectors must be the same length.'
    
    ; Partition the image into input values.
    h = Histogram(Value_Locate(input[1:inum-2], image) + 2, $
       MIN=0, MAX=255, REVERSE_INDICES=ri)
    
    ; Differentially scale the image.
    FOR index=0,N_Elements(input)-2 DO BEGIN
        x1 = input[index]
        x2 = input[index+1]
        y1 = output[index]
        y2 = output[index+1]
        
        ; Find slope and intercept for scaling.
        m = (y2 - y1) / Float((x2 - x1))
        b = y2 - (m * x2)
 
        ; Get the indices you need to scale.
        IF ri[index] NE ri[index+1] THEN BEGIN
            indices = ri[ri[index]:ri[index+1]-1]
        
            ; Scale the image.
            scaled[indices] = Byte(m * image[indices] + b)
        ENDIF
    ENDFOR
    
    RETURN, scaled
END ; ---------------------------------------------------------------------------------------


FUNCTION ScaleModis, red, grn, blu, RANGE=range, CLOUD=cloud

    ; Error handling. Return to caller with error condition.
    On_Error, 2
    
    ; Check keywords.
    IF N_Elements(range) EQ 0 THEN range = [-0.01, 1.10]
    IF N_Elements(range) EQ 1 THEN Message, 'RANGE must be a two-element array.'
    cloud = Keyword_Set(cloud)
    
    ; Set up input and output vectors for Rapid Response scaling. Image pixels
    ; between adjacent values in the input vector are linearly scaled to the corresponding
    ; values in the output vector. In other words, pixels with values between 0 and 30 in
    ; the byte scaled input image are scaled into the range 0 to 110 in the output image, and 
    ; so on. Each portion of the input image is scaled differently.
    IF cloud THEN BEGIN
        input  = [0, 25,  55, 100, 255]
        output = [0, 90, 140, 175, 255]
    ENDIF ELSE BEGIN
        input  = [0,  30,  60, 120, 190, 255]
        output = [0, 110, 160, 210, 240, 255]
    ENDELSE
    
    ; Proceed differently, depending upon the the number of positional parameters.
    CASE N_Params() OF
    
        0: Message, 'Must pass a MODIS corrected reflectance 2D image as an argument.'
        1: BEGIN
              ndims = Size(red, /N_DIMENSIONS)
              IF ndims NE 2 THEN Message, 'Input image must be a 2D array.'
              scaled = ScaleModis_ScaleIt(BytScl(red, MIN=range[0], MAX=range[1]), input, output)
           END
        2: Message, 'Either 1 or 3 positional arguments are required in this function.'
        3: BEGIN
              ndims = Size(red, /N_DIMENSIONS)
              IF ndims NE 2 THEN Message, 'Input image must be a 2D array.'
              dims = Size(red, /DIMENSIONS)
              IF Total(dims EQ Size(grn, /DIMENSIONS)) NE 2 THEN Message, 'Input images must have the same dimensions'
              IF Total(dims EQ Size(blu, /DIMENSIONS)) NE 2 THEN Message, 'Input images must have the same dimensions'
              scaled = BytArr(dims[0], dims[1], 3)
              scaled[0,0,0] = ScaleModis_ScaleIt(BytScl(red, MIN=range[0], MAX=range[1]), input, output)
              scaled[0,0,1] = ScaleModis_ScaleIt(BytScl(grn, MIN=range[0], MAX=range[1]), input, output)
              scaled[0,0,2] = ScaleModis_ScaleIt(BytScl(blu, MIN=range[0], MAX=range[1]), input, output)
           END
    
    ENDCASE

    RETURN, scaled
END ; ---------------------------------------------------------------------------------------

