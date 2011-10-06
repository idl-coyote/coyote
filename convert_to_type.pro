;+
; NAME:
;       CONVERT_TO_TYPE
;
; PURPOSE:
;
;       Converts its input argument to a specified data type.
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
;       result = Convert_To_Type(input, type)
;
; INPUT_PARAMETERS:
;
;       input:          The input data to be converted.
;       type:           The data type. Accepts values as given by Size(var, /TNAME) or Size(var, /TYPE).
;                       If converting to integer types, values are truncated (similar to FLOOR keyword below),
;                       unless keywords are set.
;
; OUTPUT_PARAMETERS:
;
;      result:          The input data is converted to specified data type.
;
; KEYWORDS:
;
;     CEILING:          If set and converting to an integer type, the CEIL function is applied before conversion.
;
;     FLOOR:            If set and converting to an integer type, the FLOOR function is applied before conversion.
;
;     ROUND:            If set and converting to an integer type, the ROUND function is applied before conversion.
;
;
; RESTRICTIONS:
;
;     Data types STRUCT, POINTER, and OBJREF are not allowed.
;
; MODIFICATION HISTORY:
;
;     Written by David W. Fanning, 19 February 2006.
;     Typo had "UNIT" instead of "UINT". 23 February 2009. DWF.
;     Added CEILING, FLOOR, and ROUND keywords. 1 April 2009. DWF.
;     Modified so that the "type" variable is not changed by the program. 5 May 2009. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2008-2009, by Fanning Software Consulting, Inc.                           ;
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
FUNCTION Convert_To_Type, input, theType, $
    CEILING=ceiling, $
    FLOOR=floor, $
    ROUND=round

   ; Return to caller on error.
   On_Error, 2
   
   ; Two positional parameters are required.
   IF N_Params() NE 2 THEN Message, 'Two input parameters (INPUT and TYPE) are required.'

   ; If type is a string, turn it into a number.
   IF Size(theType, /TNAME) EQ 'STRING' THEN BEGIN

      type = StrUpCase(theType[0])
      CASE type OF
         'BYTE': type = 1
         'INT': type = 2
         'LONG': type = 3
         'FLOAT': type = 4
         'DOUBLE': type = 5
         'COMPLEX': type = 6
         'STRING': type = 7
         'DCOMPLEX': type = 9
         'UINT': type = 12
         'ULONG': type = 13
         'LONG64': type = 14
         'ULONG64': type = 15
         ELSE: Message, 'Unable to convert input to type: ' + StrUpCase(theType)
      ENDCASE

   ENDIF ELSE BEGIN

      ; Only certain kinds of data conversions can occur.
      type = theType[0]
      CASE 1 OF
         (type LT 1): Message, 'Unable to convert input to UNDEFINED data type.'
         (type EQ 8): Message, 'Unable to convert input to STRUCTURE data type.'
         (type EQ 10): Message, 'Unable to convert input to POINTER data type.'
         (type EQ 11): Message, 'Unable to convert input to OBJECT data type.'
         (type GT 15): Message, 'Unable to convert undefined data type: ', StrTrim(theType) + '.'
         ELSE:
      ENDCASE
   ENDELSE

   ; Do the conversion.
   CASE type OF
      1: BEGIN
         CASE 1 OF
            Keyword_Set(ceiling): output = BYTE(CEIL(input))
            Keyword_Set(round): output = BYTE(ROUND(input))
            Keyword_Set(floor): output = BYTE(FLOOR(input))
            ELSE: output = BYTE(input)
         ENDCASE
         END
      2: BEGIN
         CASE 1 OF
            Keyword_Set(ceiling): output = FIX(CEIL(input))
            Keyword_Set(round): output = FIX(ROUND(input))
            Keyword_Set(floor): output = FIX(FLOOR(input))
            ELSE: output = FIX(input)
         ENDCASE
         END
      3: BEGIN
         CASE 1 OF
            Keyword_Set(ceiling): output = LONG(CEIL(input))
            Keyword_Set(round): output = LONG(ROUND(input))
            Keyword_Set(floor): output = LONG(FLOOR(input))
            ELSE: output = LONG(input)
         ENDCASE
         END
      4: output = FLOAT(input)
      5: output = DOUBLE(input)
      6: output = COMPLEX(input)
      7: output = STRING(input)
      9: output = DCOMPLEX(input)
      12: BEGIN
         CASE 1 OF
            Keyword_Set(ceiling): output = UINT(CEIL(input))
            Keyword_Set(round): output = UINT(ROUND(input))
            Keyword_Set(floor): output = UINT(FLOOR(input))
            ELSE: output = UINT(input)
         ENDCASE
         END
      13: BEGIN
         CASE 1 OF
            Keyword_Set(ceiling): output = ULONG(CEIL(input))
            Keyword_Set(round): output = ULONG(ROUND(input))
            Keyword_Set(floor): output = ULONG(FLOOR(input))
            ELSE: output = ULONG(input)
         ENDCASE
         END
      14: BEGIN
         CASE 1 OF
            Keyword_Set(ceiling): output = LONG64(CEIL(input))
            Keyword_Set(round): output = LONG64(ROUND(input))
            Keyword_Set(floor): output = LONG64(FLOOR(input))
            ELSE: output = LONG64(input)
         ENDCASE
         END
      15: BEGIN
         CASE 1 OF
            Keyword_Set(ceiling): output = ULONG64(CEIL(input))
            Keyword_Set(round): output = ULONG64(ROUND(input))
            Keyword_Set(floor): output = ULONG64(FLOOR(input))
            ELSE: output = ULONG64(input)
         ENDCASE
         END
   ENDCASE

   RETURN, output

END
