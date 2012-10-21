;+
; NAME:
;       CLIPSCL
;
; PURPOSE:
;
;       This is a utility routine to perform linear scaling (similar to BYTSCL)
;       on image arrays. If differs from BYTSCL only in that a user-specified
;       percentage of pixels can be clipped from the image histogram, prior to
;       scaling. By default, two percent of the pixels are clipped. Clipping
;       occurs at both ends of the image histogram.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:
;
;       Utilities
;
; CALLING SEQUENCE:
;
;       scaledImage = CLIPSCL(image, clipPercent)
;
; ARGUMENTS:
;
;       image:         The image to be scaled. Written for 2D images, but arrays
;                      of any size are treated alike.
;
;       clipPercent:   The percent of image clipping. Optional argument is set
;                      to 2 by default. Must be value between 0 and 49. Clipping
;                      occurs from both ends of image histogram, so a clip of 2
;                      linearly scales approximately 96% of the image histogram.
;                      Clipping percents are approximations only, and depend
;                      entirely on the distribution of pixels in the image. For
;                      interactive scaling, see cgStretch.
;
; INPUT KEYWORDS:
;
;
;       NEGATIVE:      If set, the "negative" of the result is returned.
;
;       OMAX:          The output image is scaled between OMIN and OMAX. The
;                      default value is 255.
;
;       OMIN:          The output image is scaled between OMIN and OMAX. The
;                      default value is 0.
; OUTPUT KEYWORDS:
;
;
;       THRESHOLD:     A two-element array containing the image thresholds for clipping.
;
; RETURN VALUE:
;
;       scaledImage:   The output, scaled into the range OMIN to OMAX. A byte array.
;
; COMMON BLOCKS:
;       None.
;
; EXAMPLES:
;
;       LoadCT, 0                                            ; Gray-scale colors.
;       image = cgDemoData(22)                                 ; Load image.
;       TV, ClipScl(image, 4)
;
; RESTRICTIONS:
;
;     Requires SCALE_VECTOR from the Coyote Library:
;
;        http://www.idlcoyote.com/programs/scale_vector.pro
;
; MODIFICATION HISTORY:
;
;       Written by:  David W. Fanning, 6 September 2007.
;       Not sure what this program was doing, but not what I thought. I've reworked
;          the algorithm to scale the data appropriately. 25 Oct 2011. DWF.
;-
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
FUNCTION ClipScl, image, clip, $
   NEGATIVE=negative, $
   OMAX=maxOut, $
   OMIN=minOut, $
   THRESHOLD=threshold

   ; Return to caller on error.
   ;On_Error, 2
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN, vector
   ENDIF

   ; Check arguments.
   IF N_Elements(image) EQ 0 THEN Message, 'Must pass IMAGE argument.'
   IF N_Elements(clip) EQ 0 THEN clip = 2 ELSE clip = 0 > clip < 48

   ; Check for underflow of values near 0. Yuck!
   curExcept = !Except
   !Except = 0
   i = Where(image GT -1e-35 AND image LT 1e-35, count)
   IF count GT 0 THEN image[i] = 0.0
   void = Check_Math()
   !Except = curExcept

   output = image

   ; Check keywords.
   IF N_Elements(maxOut) EQ 0 THEN maxOut = 255B ELSE maxout = 0 > Byte(maxOut) < 255
   IF N_Elements(minOut) EQ 0 THEN minOut = 0B ELSE minOut = 0 > Byte(minOut) < 255
   IF minOut GE maxout THEN Message, 'OMIN must be less than OMAX.'

   ; Calculate binsize.
   maxr = Max(image, MIN=minr, /NAN)
   range = maxr - minr
   IF Size(image, /TName) EQ 'BYTE' THEN binsize = 1.0 ELSE binsize = range / 1000.
   IF Size(image, /TName) NE Size(binsize, /TName) THEN image = Convert_To_Type(image, Size(binsize, /TName))
   h = Histogram(image, BINSIZE=binsize, OMIN=omin, OMAX=omax, /NAN)
   n = N_Elements(image)
   cumTotal = Total(h, /CUMULATIVE)
   minIndex = Value_Locate(cumTotal, n * (clip/100.))
   IF minIndex EQ -1 THEN minIndex = 0
   WHILE cumTotal[minIndex] EQ cumTotal[minIndex + 1] DO BEGIN
        minIndex = minIndex + 1
   ENDWHILE
   minThresh = minIndex * binsize + omin

   ; Not all files can be clipped appropriately. If maxIndex
   ; is -1 or N_Elements(cumTotal), or maxIndex=minIndex then 
   ; just byte scale the image and get out of here.
   maxIndex  = Value_Locate(cumTotal, n * ((100-clip)/100.))
   IF (maxIndex EQ -1) || (maxIndex EQ N_Elements(cumTotal)) || (maxIndex EQ minIndex) THEN BEGIN
       threshold = [minr, maxr]
       Message, 'Image histogram could not be clipped successfully. Image is byte scaled.', /Informational
       IF Keyword_Set(negative) THEN RETURN, 255B - BytScl(image, /NAN) ELSE RETURN, BytScl(image, /NAN)
   ENDIF
   
   ; If you are still here, try to clip the histogram.
   WHILE cumTotal[maxIndex] EQ cumTotal[maxIndex - 1] DO BEGIN
       maxIndex = maxIndex - 1
   ENDWHILE
   maxThresh = maxIndex * binsize + omin

   ; Save the thresholds.
   threshold = [minThresh, maxThresh]
   
   ; Scale the data.
   output = Scale_Vector(Temporary(output), MIN=threshold[0], MAX=threshold [1], minOut, maxOut)

   IF Keyword_Set(negative) THEN RETURN, 0B > (maxout - output + minOut) < 255B $
      ELSE RETURN, output

END
