; docformat = 'rst'
;
; NAME:
;   cgCompressScl
;
; PURPOSE:
;   This is a utility routine to perform a compression transformation
;   on an image. For exponent values greater than 1.0, the upper and
;   lower values of the image are compressed and centered on the mean.
;   Larger exponent values provide steeper compression. See pages 68-70 in 
;   _Digital Image Processing with MATLAB_ by Gonzales, Wood, and Eddins. 
;   The function is used to improve contrast in images.
;   
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2015, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; This is a utility routine to perform a compression transformation
; on an image. For exponent values greater than 1.0, the upper and
; lower values of the image are compressed and centered on the mean.
; Larger exponent values provide steeper compression. See pages 68-70 in 
; _Digital Image Processing with MATLAB_ by Gonzales, Wood, and Eddins. 
; The function is used to improve contrast in images. The equation being
; implemented before scaling between OMIN and OMAX is as follows::
; 
;     output = 1.0D / ((1.0D + (mean / (Temporary(input) > 1e-16))^exponent) > (1e-16))
;
; :Categories:
;    Image Processing
;
; :Returns:
;     A byte scaled image is returned.
;
; :Params:
;    image: in, required
;       The image to be scaled. Written for 2D images, but arrays of any size are treated alike.
;
; :Keywords:
;     exponent: in, optional, type=float, default=4.0
;         The exponent in the compression transformation. By default, 4.0.
;
;     max: in, optional
;          Any value in the input image greater than this value is set to this value
;          before scaling.
;          
;     mean: in, optional, type=float, default=0.5
;          Values on either side of the mean will be compressed by the log. The value is a 
;          normalized value between 0.0 and 1.0. 
;          
;     min: in, optional
;          Any value in the input image less than this value is set to this value
;          before scaling.
;
;     negative, in, optional, type=boolean, default=0
;          If set, the "negative" of the result is returned.
;
;     omax: in, optional, type=byte, default=255
;          The output image is scaled between OMIN and OMAX.
;
;     omin: in, optional, type=byte, default=0
;          The output image is scaled between OMIN and OMAX.
;
; :Examples:
;    Examples of compression stretching::
;       cgLoadCT, 0                                              ; Gray-scale colors.
;       image = cgDemoData(22)                                   ; Load image.
;       cgImage, image                                           ; No contrast.
;       cgImage, cgCompressScl(image)                            ; Improved contrast.
;       cgImage, cgCompressScl(image, Exponent=10, Mean=0.65)    ; Even more contrast.
;       cgImage, cgCompressScl(image, /Negative, Exponent=5)     ; A negative image.
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
;       Written by:  David W. Fanning, 20 February 2006.
;       Fixed a problem with output scaling. 1 July 2009. DWF (with input from Bo Milvang-Jensen).
;       Renamed cgCompressScl from LogScl when I discoverd LogScl implemented the wrong scaling equation.
;          See the documentation in the retired LogScl program for details. 27 March 2015. DWF.
;
; :Copyright:
;     Copyright (c) 2006-2015, Fanning Software Consulting, Inc.
;-
FUNCTION cgCompressScl, image, $
   EXPONENT=exponent, $
   MEAN=mean, $
   NEGATIVE=negative, $
   MAX=maxValue, $
   MIN=minValue, $
   OMAX=maxOut, $
   OMIN=minOut

   ; Return to caller on error.
   On_Error, 2
   
   ; Check arguments.
   IF N_Elements(image) EQ 0 THEN Message, 'Must pass IMAGE argument.'

   ; Check for underflow of values near 0. Yuck!
   curExcept = !Except
   !Except = 0
   i = Where(image GT -1e-35 AND image LT 1e-35, count)
   IF count GT 0 THEN image[i] = 0.0
   void = Check_Math()
   !Except = curExcept

   output = Double(image)

   ; Check keywords.
   IF N_Elements(exponent) EQ 0 THEN exponent = 4.0
   IF N_Elements(mean) EQ 0 THEN mean = 0.5 ELSE mean = 0.0 > mean < 1.0
   IF N_Elements(maxOut) EQ 0 THEN maxOut = 255B ELSE maxout = 0 > Byte(maxOut) < 255
   IF N_Elements(minOut) EQ 0 THEN minOut = 0B ELSE minOut = 0 > Byte(minOut) < 255
   IF minOut GE maxout THEN Message, 'OMIN must be less than OMAX.'

   ; Exponent must be greater than 0.
   exponent = 1.0e-6 > exponent

   ; Perform initial scaling of the image into 0 to 1.
   output = cgScaleVector(Temporary(output), 0.0D, 1.0D, MaxValue=maxValue, $
      MinValue=minValue, /NAN, Double=1)

   ; Too damn many floating underflow warnings, no matter WHAT I do! :-(
   thisExcept = !Except
   !Except = 0

   ; Compression scaling.
   output = 1.0D / ((1.0D + (mean / (Temporary(output) > 1e-16))^exponent) > (1e-16))

   ; Scale to image coordinates.
   output = cgScaleVector(Temporary(output), minOut, maxOut, MinValue=0.0D, MaxValue=1.0D, /NAN, Double=1)

   ; Clear math errors.
   void = Check_Math()
   !Except = thisExcept

   ; Does the user want the negative result?
   IF Keyword_Set(negative) THEN RETURN, BYTE(maxout - Round(output) + minOut) $
      ELSE RETURN, BYTE(Round(output))

END
