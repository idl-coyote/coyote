; docformat = 'rst'
;
; NAME:
;   SDEVSCL
;
; PURPOSE:
;    This is a utility routine to perform standard deviation scaling
;    on image arrays. The user defines a multiple of the standard
;    deviation and this is used with the standard deviation of the
;    pixels in the image to create a threshold for linear scaling.
;    Use the EXCLUDE keyword to exclude a particular value from
;    the standard deviation calculation.
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
; This is a utility routine to perform standard deviation scaling
; on image arrays. The user defines a multiple of the standard
; deviation and this is used with the standard deviation of the
; pixels in the image to create a threshold for linear scaling.
; Use the EXCLUDE keyword to exclude a particular value from
; the standard deviation calculation.
; 
; :Categories:
;    Utilities, Graphics
;    
; :Params:
;     image: in, required, type=numeric
;        The image array that is to be scaled.
;      
; :Keywords:
;     exclude: in, optional, type=numeric
;         Set this keyword to a value in the image array that is to be excluded from the
;         standard deviation calculation. Normally, this would be the backgroud value of
;         the image, if there is a background.
;     multiplier: in, optional, type=float, default=2.0
;        The standard deviation of the image pixels is computed and then
;        multiplied by the multiplier factor to produce upper and lower
;        thresholds for the linear scaling of the image by subtracting
;        or adding this value to the mean value of the image. The image
;        is linearly scaled between these two threshold values. 
;     negative: in, optional, type=boolean
;          Set this keyword to return the "negative" or reverse of the image scaling.
;     omax: in, optional, type=byte, default=255
;          Normally, the image is scaled into the range of 0 to 255. Setting the
;          OMIN and OMAX keywords can change this scaling.
;     omin: in, optional, type=byte, default=0
;          Normally, the image is scaled into the range of 0 to 255. Setting the
;          OMIN and OMAX keywords can change this scaling.
;     threshold: out, optional, type=float
;          A two-element array that contains the minimum and maximum thresholds, respectively,
;          that were calculated for the scaling.
;              
; :Examples:
;     To display an image with standard deviation scaling::
;        image = cgDemoData(5)
;        cgDisplay, 256*3, 256
;        !P.Multi = [0,3,1]
;        cgImage, image
;        cgImage, SDevScl(image)
;        cgImage, SDevScl(image, Exclude=0)
;        !P.Multi = 0
;       
; :Author:
;     FANNING SOFTWARE CONSULTING::
;        David W. Fanning 
;        1645 Sheely Drive
;        Fort Collins, CO 80526 USA
;        Phone: 970-221-0438
;        E-mail: david@idlcoyote.com
;        Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written by: David W. Fanning, 5 June 2012.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION SDevScl, image, $
   EXCLUDE=exclude, $
   MULTIPLIER=multiplier, $
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
      RETURN, image
   ENDIF

   ; Check arguments.
   IF N_Elements(image) EQ 0 THEN Message, 'Must pass IMAGE argument.'
   IF N_Elements(multiplier) EQ 0 THEN multiplier = 2.0

   ; Check for underflow of values near 0. Yuck!
   curExcept = !Except
   !Except = 0
   i = Where(image GT -1e-35 AND image LT 1e-35, count)
   IF count GT 0 THEN image[i] = 0.0
   void = Check_Math()
   !Except = curExcept

   IF Size(image, /TNAME) NE 'DOUBLE' THEN output = Float(image)

   ; Check keywords.
   IF N_Elements(maxOut) EQ 0 THEN maxOut = 255B ELSE maxout = 0 > Byte(maxOut) < 255
   IF N_Elements(minOut) EQ 0 THEN minOut = 0B ELSE minOut = 0 > Byte(minOut) < 255
   IF minOut GE maxout THEN Message, 'OMIN must be less than OMAX.'
   
   ; Did you want to exclude a value?
   IF N_Elements(exclude) NE 0 THEN BEGIN
     indices = Where(image EQ exclude[0], count)
     IF count GT 0 THEN output[indices] = !Values.F_NAN
   ENDIF

   ; Calculate the standard deviation of the image.
   stddev = StdDev(output, /DOUBLE, /NAN)
   
   ; Calculate the thresholds.
   minThresh = Mean(output, /DOUBLE, /NAN) - (multiplier * stddev)
   maxThresh = Mean(output, /DOUBLE, /NAN) + (multiplier * stddev)

   ; Save the thresholds.
   threshold = [minThresh, maxThresh]

   ; Scale the data.
   output = Scale_Vector(Temporary(output), MIN=threshold[0], MAX=threshold [1], minOut, maxOut)

   IF Keyword_Set(negative) THEN RETURN, 0B > (maxout - Byte(output) + minOut) < 255B $
      ELSE RETURN, Byte(output)

END
