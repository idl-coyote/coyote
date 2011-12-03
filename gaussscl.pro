;+
; NAME:
;       GAUSSSCL
;
; PURPOSE:
;
;       This is a utility routine to perform a gaussian gray-level pixel
;       transformation stretch on a image.
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
;       scaledImage = GAUSSSCL(image)
;
; ARGUMENTS:
;
;       image:         The image to be scaled. Written for 2D images, but arrays
;                      of any size are treated alike.
;
; KEYWORDS:
;
;       SIGMA:         The sigma value or width of the Gaussian
;                      function. Set to 1 by default.
;
;
;       MAX:           Any value in the input image greater than this value is
;                      set to this value before scaling.
;
;       MIN:           Any value in the input image less than this value is
;                      set to this value before scaling.
;
;       NEGATIVE:      If set, the "negative" of the result is returned.
;
;       OMAX:          The output image is scaled between OMIN and OMAX. The
;                      default value is 255.
;
;       OMIN:          The output image is scaled between OMIN and OMAX. The
;                      default value is 0.
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
;       image = cgDemoData(11)                                 ; Load image.
;       TV, GaussScl(image)
;
; RESTRICTIONS:
;
;     Requires SCALE_VECTOR from the Coyote Library:
;
;        http://www.idlcoyote.com/programs/scale_vector.pro
;
; MODIFICATION HISTORY:
;
;       Written by:  David W. Fanning, 5 September 2007.
;       Now setting NAN keyword on all MIN and MAX functions. 2 Dec 2011. DWF.
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
FUNCTION GaussScl, image, $
   SIGMA=sigma, $
   MAX=imageMax, $
   MIN=imageMin, $
   NEGATIVE=negative, $
   OMAX=maxOut, $
   OMIN=minOut

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

   ; Check for underflow of values near 0. Yuck!
   curExcept = !Except
   !Except = 0
   i = Where(image GT -1e-35 AND image LT 1e-35, count)
   IF count GT 0 THEN image[i] = 0.0
   void = Check_Math()
   !Except = curExcept

   output = image

   ; Check keywords.
   IF N_Elements(imageMax) EQ 0 THEN imageMax = Max(output, /NAN)
   IF N_Elements(imageMin) EQ 0 THEN imageMin = Min(output, /NAN)
   IF N_Elements(maxOut) EQ 0 THEN maxOut = 255B ELSE maxout = 0 > Byte(maxOut) < 255
   IF N_Elements(minOut) EQ 0 THEN minOut = 0B ELSE minOut = 0 > Byte(minOut) < 255
   IF minOut GE maxout THEN Message, 'OMIN must be less than OMAX.'
   IF N_Elements(sigma) EQ 0 THEN sigma = 1 ELSE sigma = sigma > 0.25

   ; Perform initial scaling of the image.
   output = Scale_Vector(Temporary(output), 0.0D, 1.0D, MinValue=imageMin, MaxValue=imageMax, /NAN, Double=1)

   ; Perform Gaussian scaling.
   output = Scale_Vector(Temporary(output), -!pi, !pi)
   f = (1/(sigma*sqrt(2*!dpi)))*Exp(-(Temporary(output)^2/(2*sigma^2)))
   output = Scale_Vector(Temporary(f), minOut, maxOut)

   ; Does the user want the negative result?
   IF Keyword_Set(negative) THEN RETURN, BYTE(0B > (maxout - Round(output) + minOut) < 255B) $
      ELSE RETURN, BYTE(0B > Round(output) < 255B)

END
