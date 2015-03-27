; docformat = 'rst'
;
; NAME:
;   cgGaussScl
;
; PURPOSE:
;   This is a utility routine to perform a gaussian gray-level pixel transformation 
;   stretch on an image.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 20150, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; This is a utility routine to perform a gaussian gray-level pixel transformation 
; stretch on an image.
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
;     max: in, optional
;          Any value in the input image greater than this value is set to this value 
;          before scaling.
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
;     sigma: in, optional, type=float, default=1.0
;         The sigma value or width of the Gaussian function. 
;
; :Examples:
;     Display a Gaussian scaled image::
;       cgLoadCT, 0              ; Gray-scale colors.
;       image = cgDemoData(11)   ; Load image.
;       cgImage, cgGaussScl(image)
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
;       Written by:  David W. Fanning, 5 September 2007.
;       Now setting NAN keyword on all MIN and MAX functions. 2 Dec 2011. DWF.
;       Renamed cgGaussScl from the retired GaussScl. 26 March 2015. DWF.
;
; :Copyright:
;     Copyright (c) 2007-2015, Fanning Software Consulting, Inc.
;-
FUNCTION cgGaussScl, image, $
   SIGMA=sigma, $
   MAX=imageMax, $
   MIN=imageMin, $
   NEGATIVE=negative, $
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

   output = image

   ; Check keywords.
   IF N_Elements(imageMax) EQ 0 THEN imageMax = Max(output, /NAN)
   IF N_Elements(imageMin) EQ 0 THEN imageMin = Min(output, /NAN)
   IF N_Elements(maxOut) EQ 0 THEN maxOut = 255B ELSE maxout = 0 > Byte(maxOut) < 255
   IF N_Elements(minOut) EQ 0 THEN minOut = 0B ELSE minOut = 0 > Byte(minOut) < 255
   IF minOut GE maxout THEN Message, 'OMIN must be less than OMAX.'
   IF N_Elements(sigma) EQ 0 THEN sigma = 1 ELSE sigma = sigma > 0.25

   ; Perform initial scaling of the image.
   output = cgScaleVector(Temporary(output), 0.0D, 1.0D, MinValue=imageMin, MaxValue=imageMax, /NAN, Double=1)

   ; Perform Gaussian scaling.
   output = cgScaleVector(Temporary(output), -!pi, !pi)
   f = (1/(sigma*sqrt(2*!dpi)))*Exp(-(Temporary(output)^2/(2*sigma^2)))
   output = cgScaleVector(Temporary(f), minOut, maxOut)

   ; Does the user want the negative result?
   IF Keyword_Set(negative) THEN RETURN, BYTE(0B > (maxout - Round(output) + minOut) < 255B) $
      ELSE RETURN, BYTE(0B > Round(output) < 255B)

END
