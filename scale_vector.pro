;+
; NAME:
;       SCALE_VECTOR
;
; PURPOSE:
;
;       This is a utility routine to scale the elements of a vector
;       (or an array) into a given data range. The processed vector
;       [MINVALUE > vector < MAXVECTOR] is scaled into the data range
;       given by MINRANGE and MAXRANGE.
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
;       scaledVector = SCALE_VECTOR(vector, [minRange], [maxRange], [MINVALUE=minvalue], [MAXVALUE=maxvalue])
;
; INPUT POSITIONAL PARAMETERS:
;
;       vector:   The vector (or array) to be scaled. Required.
;       minRange: The minimum value of the scaled vector. Set to 0 by default. Optional.
;       maxRange: The maximum value of the scaled vector. Set to 1 by default. Optional.

;       Note that it is the processed vector [MINVALUE > vector < MAXVALUE] that is
;       scaled between minRange and maxRange. See the MINVALUE and MAXVALUE keywords below.
;
; INPUT KEYWORD PARAMETERS:
;
;       DOUBLE:        Set this keyword to perform scaling in double precision.
;                      Otherwise, scaling is done in floating point precision.
;
;       MAXVALUE:      MAXVALUE is set equal to (vector < MAXVALUE) prior to scaling.
;                      The default value is MAXVALUE = Max(vector).
;
;       MINVALUE:      MINVALUE is set equal to (vector > MAXVALUE) prior to scaling.
;                      The default value is MINXVALUE = Min(vector).
;
;       NAN:           Set this keyword to enable not-a-number checking. NANs
;                      in vector will be ignored.
;
;       PRESERVE_TYPE: Set this keyword to preserve the input data type in the output.
;
; RETURN VALUE:
;
;       scaledVector: The vector (or array) values scaled into the data range.
;
; COMMON BLOCKS:
;       None.
;
; EXAMPLES:
;
;       x = [3, 5, 0, 10]
;       xscaled = SCALE_VECTOR(x, -50, 50)
;       Print, xscaled
;          -20.0000     0.000000     -50.0000      50.0000

;       Suppose your image has a minimum value of -1.7 and a maximum value = 2.5.
;       You wish to scale this data into the range 0 to 255, but you want to use
;       a diverging color table. Thus, you want to make sure value 0.0 is scaled to 128.
;       You proceed like this:
;
;       scaledImage = SCALE_VECTOR(image, 0, 255, MINVALUE=-2.5, MAXVALUE=2.5)
;
; RESTRICTIONS:
;
;     Requires the following programs from the Coyote Library:
;
;        http://www.idlcoyote.com/programs/convert_to_type.pro
;        http://www.idlcoyote.com/programs/fpufix.pro
;
; MODIFICATION HISTORY:
;
;       Written by:  David W. Fanning, 12 Dec 1998.
;       Added MAXVALUE and MINVALUE keywords. 5 Dec 1999. DWF.
;       Added NAN keyword. 18 Sept 2000. DWF.
;       Removed check that made minRange less than maxRange to allow ranges to be
;          reversed on axes, etc. 28 Dec 2003. DWF.
;       Added PRESERVE_TYPE and DOUBLE keywords. 19 February 2006. DWF.
;       Added FPUFIX to cut down on floating underflow errors. 11 March 2006. DWF.
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
FUNCTION Scale_Vector, vector, minRange, maxRange, $
   MAXVALUE=vectorMax, MINVALUE=vectorMin, NAN=nan, $
   PRESERVE_TYPE=preserve_type, DOUBLE=double

   ; Return to caller on error.
   ;On_Error, 2
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN, vector
   ENDIF

   ; Check positional parameters.
   CASE N_Params() OF
      0: Message, 'Incorrect number of arguments.'
      1: BEGIN
         IF Keyword_Set(double) THEN BEGIN
            minRange = 0.0D
            maxRange = 1.0D
         ENDIF ELSE BEGIN
            minRange = 0.0
            maxRange = 1.0
         ENDELSE
         ENDCASE
      2: BEGIN
         IF Keyword_Set(double) THEN maxRange = 1.0D > (minRange + 0.0001D) ELSE $
            maxRange = 1.0 > (minRange + 0.0001)
         ENDCASE
      ELSE:
   ENDCASE

   ; If input data type is DOUBLE and DOUBLE keyword is not set, then set it.
   IF Size(FPUFIX(vector), /TNAME) EQ 'DOUBLE' AND N_Elements(double) EQ 0 THEN double = 1

   ; Make sure we are working with at least floating point numbers.
   IF Keyword_Set(double) THEN minRange = DOUBLE( minRange ) ELSE minRange = FLOAT( minRange )
   IF Keyword_Set(double) THEN maxRange = DOUBLE( maxRange ) ELSE maxRange = FLOAT( maxRange )

   ; Make sure we have a valid range.
   IF maxRange EQ minRange THEN Message, 'Range max and min are coincidental'

   ; Check keyword parameters.
   IF Keyword_Set(double) THEN BEGIN
      IF N_Elements(vectorMin) EQ 0 THEN vectorMin = Double( Min(FPUFIX(vector), NAN=1) ) $
         ELSE vectorMin = Double(vectorMin)
      IF N_Elements(vectorMax) EQ 0 THEN vectorMax = DOUBLE( Max(FPUFIX(vector), NAN=1) ) $
         ELSE vectorMax = DOUBLE( vectorMax )
   ENDIF ELSE BEGIN
      IF N_Elements(vectorMin) EQ 0 THEN vectorMin = FLOAT( Min(FPUFIX(vector), NAN=1) ) $
         ELSE vectorMin = FLOAT( vectorMin )
      IF N_Elements(vectorMax) EQ 0 THEN vectorMax = FLOAT( Max(FPUFIX(vector), NAN=Keyword_Set(nan)) ) $
         ELSE vectorMax = FLOAT( vectorMax )
   ENDELSE

   ; Trim vector before scaling.
   index = Where(Finite(vector) EQ 1, count)
   IF count NE 0 THEN BEGIN
      IF Keyword_Set(double) THEN trimVector = Double(vector) ELSE trimVector = Float(vector)
      trimVector[index]  =  vectorMin >  vector[index] < vectorMax
   ENDIF ELSE BEGIN
      IF Keyword_Set(double) THEN trimVector = vectorMin > Double(vector) < vectorMax ELSE $
         trimVector = vectorMin > Float(vector) < vectorMax
   ENDELSE

   ; Calculate the scaling factors.
   scaleFactor = [((minRange * vectorMax)-(maxRange * vectorMin)) / $
       (vectorMax - vectorMin), (maxRange - minRange) / (vectorMax - vectorMin)]

   ; Clear math errors.
   void = Check_Math()

   ; Return the scaled vector.
   IF Keyword_Set(preserve_type) THEN BEGIN
      RETURN, FPUFIX(Convert_To_Type(trimVector * scaleFactor[1] + scaleFactor[0], Size(vector, /TNAME)))
   ENDIF ELSE BEGIN
      RETURN, FPUFIX(trimVector * scaleFactor[1] + scaleFactor[0])
   ENDELSE

END ;-------------------------------------------------------------------------
