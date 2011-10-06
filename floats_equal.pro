;+
; NAME:
;       FLOATS_EQUAL
;
; PURPOSE:
;
;       The purpose of this function is to compare two floating-point values or
;       arrays to determine if the values or arrays are equal. Arrays are equal
;       if they have the same number of elements, and each element is equal.
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
;       result = FLOATS_EQUAL(array_1, array_2)
;
; ARGUMENTS:
;
;       array_1        Any single or double precision value or array. Required parameter.
;
;       array_2        Any single or double precision value or array. Required parameter.
;
; KEYWORDS:
;
;       ULP            UNIT in the LAST PLACE. It is the gap or difference between two
;                      floating point numbers in the last digit that can distinguish the
;                      two numbers. Must be a positive integer. Set to 1 by default. Set
;                      to a larger value if you suspect accumulative round-off errors
;                      in your arrays.
;
; RETURN VALUE:
;
;       result         Set to 1 if the arrays are equal, which means that the arrays have
;                      the same number of elements and each element is equal to the same
;                      element in the other array. Set to 0 if the arrays are not equal.
; COMMON BLOCKS:
;       None.
;
; EXAMPLE:
;
;       IDL> a = Findgen(11)
;       IDL> b = Findgen(11)
;       IDL> Print, Floats_Equal(a,b)
;             1
;       IDL> b[4] = b[4] + 0.0001
;       IDL> Print, Floats_Equal(a,b)
;             0
;
; RESTRICTIONS:
;
;       None.
;
; MODIFICATION HISTORY:
;
;       Written by:  David W. Fanning, 29 August 2007.
;       Fixed a problem when using large numbers with the TOTAL command
;          by setting the INTEGER keyword. 22 June 2011. DWF.
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
FUNCTION FLOATS_EQUAL, array_1, array_2, ULP=ulp

       ; Error handling. Return to caller on error.
       ON_ERROR, 2
       IF N_Params() NE 2 THEN Message, 'Must pass two arrays or values to compare.'
       ; Are we comparing double precision values?
       IF Size(array_1, /TNAME) EQ 'DOUBLE' OR Size(array_2, /TNAME) EQ 'DOUBLE' THEN $
           double = 1 ELSE double = 0
           
        ; Check keyword.
       IF N_Elements(ulp) EQ 0 THEN ulp = 1.0D ELSE ulp = ROUND(ABS(ulp))
       
       ; Arrays not equal if they are not the same length.
       IF N_Elements(array_1) NE N_Elements(array_2) THEN RETURN, 0
       ; Choose a number "sufficiently close" to zero for comparison.
       epsilon = (MACHAR(DOUBLE=double)).eps
       NUMBER = (Abs(array_1) > Abs(array_2)) * epsilon * ulp
       ; Compare the arrays.
       IF Total(Abs(array_1 - array_2) LE NUMBER, /INTEGER) EQ N_Elements(array_1) THEN $
           RETURN, 1 ELSE RETURN, 0
   END
