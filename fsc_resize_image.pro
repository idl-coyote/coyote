; docformat = 'rst'
;
; NAME:
;   FSC_Resize_Image
;
; PURPOSE:
;   Provides a CONGRID like resizing of images, although it performs this service
;   correctly for both 2D and 3D images. Pixel locations do not change in the output
;   image, since the center of the pixel is used for interpolation purposes, rather than
;   the lower-left corner of the pixel. Unlike CONGRID, 3D images can use nearest neighbor
;   interpolation as well as bilinear interpolation.
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
;   Provides a CONGRID like resizing of images, although it performs this service
;   correctly for both 2D and 3D images. Pixel locations do not change in the output
;   image, since the center of the pixel is used for interpolation purposes, rather than
;   the lower-left corner of the pixel. Unlike CONGRID, 3D images can use nearest neighbor
;   interpolation as well as bilinear interpolation.
;
; :Categories:
;    Utilities
;    
; :Params:
;    image: in, required, type=any
;         The image variable to resize. Must be a 2D or 3D image. If a 3D image,
;         one of the image dimensions must be a 3.
;         
;    cols: in, required, type=integer
;          The number of columns (i.e., XSIZE) in the output image.
;          
;    rows: in, required, type=integer
;          The number of rows (i.e., YSIZE) in the output image.
;                 
; :Keywords:
;     interpolate: in, optional, type=boolean, default=0
;         Is set, bilinear interpolation is used to resize the image. Otherwise,
;         nearest neighbor sampling is used instead.
;      minus_one: in, optional, type=boolean, default=1
;          Identical to CONGRID MINUS_ONE keyword. Default is set to 1 to avoid
;          extrapolating one column or row beyond the bounds of the input image.
;          
; :Examples:
;    Used in a similar fashion to the Congrid command, but for images::
;       IDL> bigEarth = FSC_Resize_Image(LoadData(7), 720, 720)
;       IDL> bigRose  = FSC_Resize_Image(LoadData(16), 681, 447)
;       
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: davidf@dfanning.com
;           Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; :History:
;     Change History::
;        Written, 20 November 2010. DWF.
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
FUNCTION FSC_Resize_Image, image, cols, rows, $
    INTERPOLATE=interp, $
    MINUS_ONE=minus_one

    Compile_Opt idl2
    
    ; Return to the caller on an error.
    On_Error, 2   
    
    ; Check image parameter for size. Only 2D and 3D images allowed.
    ndim = SIZE(image, /N_DIMENSIONS)
    dims = SIZE(image, /DIMENSIONS)
    IF ((ndim LT 2) OR (ndim GT 3)) THEN $
      Message, 'Input image must have two or three dimensions.'
    
    ; Check for keywords. Default for minus_one is 1.
    interp = Keyword_Set(interp)
    IF N_Elements(minus_one) EQ 0 THEN minus_one = 1
    m1 = Keyword_Set(minus_one)
    
    ; 2D images are immediately passed to IDL's Congrid command, with 
    ; the CENTER keyword set.
    IF ndim EQ 2 THEN BEGIN
        RETURN, Congrid(image, cols, rows, CENTER=1, MINUS_ONE=minus_one, $
                     INTERP=interp)
    ENDIF
    
    ; 24-bit images are handled differently. The "3" dimension of a 24-bit
    ; image should not be interpolated.
    offset = 0.5 ; To center the pixel location (i.e., CENTER=1 for Congrid)
    index3 = Where(dims EQ 3)
    CASE index3 OF
    
        0: BEGIN
           srx = [0,1,2]
           sry = Float(dims[1] - m1) / (cols - m1) *(Findgen(cols) + offset) - offset
           srz = Float(dims[2] - m1) / (rows - m1) *(Findgen(rows) + offset) - offset
           END
           
        1: BEGIN
           srx = Float(dims[0] - m1) / (cols - m1) *(Findgen(cols) + offset) - offset
           sry = [0,1,2]
           srz = Float(dims[2] - m1) / (rows - m1) *(Findgen(rows) + offset) - offset
           END
           
        2: BEGIN
           srx = Float(dims[0] - m1) / (cols - m1) *(Findgen(cols) + offset) - offset
           sry = Float(dims[1] - m1) / (rows - m1) *(Findgen(rows) + offset) - offset
           srz = [0,1,2]
           END
           
    ENDCASE
    
    ; Do the interpolation. Preserve nearest neighbor sampling, if required.
    IF interp THEN BEGIN
          RETURN, Interpolate(image, srx, sry, srz, /GRID)
    ENDIF ELSE BEGIN
          RETURN, Interpolate(image, Round(srx), Round(sry), Round(srz), /GRID)
    ENDELSE
    
END ;--------------------------------------------------------------------------
