;+
; NAME:
;  BINARY
;
; PURPOSE:
;
;   This function is used to display a binary representation of byte,
;   integer, and long integer values.
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
;   Utilities
;
; CALLING SEQUENCE:
;
;   output = Binary(theNumber)
;
; RETURN VALUE:
;
;   output:        A string array of 0s and 1s to be printed (normally), in a
;                  binary representation of the number. The number is represented with
;                  the highest bits on the left and the lowest bits on the right,
;                  when printed with the PRINT command.
;
; ARGUMENTS:
;
;  theNumber:      The number for which the user wants a binary representation.
;                  It must be BYTE, INT, or LONG.
;
; KEYWORDRS:
;
;  COLOR:          If this keyword is set, the binary representation always
;                  contains 24 bits of output.
;
;  SEPARATE:       If this keyword is set, the output is separated with space
;                  between each group of eight bits.
;
; EXAMPLE:
;
;  IDL> Print, Binary(24B)
;          0 0 0 1 1 0 0 0
;  IDL> Print, Binary(24L)
;          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0
;  IDL> Print, Binary(24L, /COLOR)
;          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0
;  IDL> Print, Binary(24L, /COLOR, /SEPARATE)
;          0 0 0 0 0 0 0 0    0 0 0 0 0 0 0 0    0 0 0 1 1 0 0 0
;
; MODIFICATION HISTORY:
;
;  Written by: David W. Fanning, November 10, 2007.
;  Fixed a problem with error handling. 13 March 2008. DWF.
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
FUNCTION BINARY, number, COLOR=color, SEPARATE=separate

   ON_ERROR, 2

   ; What kind of number is this?
   thisType = SIZE(number, /Type)

   CASE thisType OF

      1: BEGIN ; Byte value

         bin = STRARR(8)
         FOR j=0,7 DO BEGIN
            powerOfTwo = 2L^j
            IF (LONG(number) AND powerOfTwo) EQ powerOfTwo THEN $
               bin(j) = '1' ELSE bin(j) = '0'
         ENDFOR
         IF Keyword_Set(color) THEN bin = [bin, STRARR(16)+'0']
         ENDCASE

      2: BEGIN ; Integer value.

         bin = STRARR(16)
         FOR j=0,15 DO BEGIN
            powerOfTwo = 2L^j
            IF (LONG(number) AND powerOfTwo) EQ powerOfTwo THEN $
               bin(j) = '1' ELSE bin(j) = '0'
         ENDFOR
         IF Keyword_Set(color) THEN bin = [bin, STRARR(8)+'0']
         ENDCASE

      3: BEGIN ; Long integer value.

         number = LONG(number)
         bin = STRARR(32)
         FOR j=0,31 DO BEGIN
            powerOfTwo = 2L^j
            IF (LONG(number) AND powerOfTwo) EQ powerOfTwo THEN $
               bin(j) = '1' ELSE bin(j) = '0'
         ENDFOR
         IF Keyword_Set(color) THEN bin = bin[0:23]

         ENDCASE

      ELSE: Message, 'Only BYTE, INTEGER, and LONG values allowed.'

   ENDCASE

   ; Do we need to separate in groups of 8?
   IF Keyword_Set(separate) THEN BEGIN
      CASE N_Elements(bin) OF
         8:
         16: bin = [bin[0:7], '  ', bin[8:15]]
         24: bin = [bin[0:7], '  ', bin[8:15], '  ', bin[16:23]]
         32: bin = [bin[0:7], '  ', bin[8:15], '  ', bin[16:23], '  ', bin[24:31]]
      ENDCASE
   ENDIF

   ; Reverse the array, so highest bits are on left and lowest bits are on right.
   RETURN, Reverse(bin)

END
