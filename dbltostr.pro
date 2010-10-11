;+
; NAME:
;       DBLTOSTR
;
; PURPOSE:
;
;       This is a program for converting a double precision numerical value
;       to a string. It was originally offered by BioPhys on the IDL newsgroup.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning,  Ph.D.
;       1645 Sheely Drive
;       Fort Collins,  CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Utility
;
; CALLING SEQUENCE:
;
;       stringValue  =  DblToStr(value)
;
; INPUTS:
;
;       value - A double-precision or floating point value to be converted to a string.
;
; OUTPUTS:
;
;       stringValue - The converted string value.
;
; KEYWORDS:
;
;       None.
;
; RESTRICTIONS:
;
;       Assumes 14 significant digits of precision.
;
; MODIFICATION HISTORY:
;
;       Written by BioPhys and offered to the IDL newsgroup,  7 November 2005.
;       Slightly modified and renamed by David Fanning,  30 November,  2005.
;-
;
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
FUNCTION DBLTOSTR,  value

   ; Error handling.
   On_Error,  2
   IF N_Elements(value) EQ 0 THEN Message,  'Double precision or floaing value must be passed to the function.'

   ; Get the data type.
   theType = Size(value, /Type)
   IF theType NE 4 AND theType NE 5 THEN BEGIN
       value = Double(value)
       theType = 5
   ENDIF

   ; Data extension.
   typeExt = theType EQ 4 ? 'e' : 'd'

   ; Create a string, using the full-widtet G format.
   rawstr = StrTrim(String(value, Format = '(g)'), 2)

   ; Extract the sign from the string and remove it for the moment.
   sign = StrMid(rawstr, 0, 1) EQ '-' ? '-' : ''
   rawstr = sign EQ '' ? rawstr:StrMid(rawstr, 1)

   ; Is there an exponent in the string? If so, remove that for the moment.
   epos = StrPos(rawstr, 'e')
   indx = epos gt -1 ? StrMid(rawstr, epos+1) : ''
   rawstr = indx EQ '' ? rawstr:StrMid(rawstr, 0, epos)

   ; Find the position of the decimal point.
   dpos = StrPos(rawstr, '.')

   ; Rounding process (assumes 14 significant digits).
   outstr = StrArr(15)
   FOR i = 0, 14 DO outstr[i] = StrMid(rawstr, i, 1)
   aux = Fix(StrMid(rawstr, 16, 1)) GE 5?1:0
   FOR i = 14,  0,  -1 DO BEGIN
      IF i NE dpos then BEGIN
         sumstr = StrTrim(String(aux+fix(outstr[i])), 2)
         sumlen = StrLen(sumstr)
         outstr[i] = StrMid(sumstr, sumlen-1, 1)
         aux = sumlen EQ 1 ? 0 : 1
      ENDIF
   ENDFOR

   ; Throw away '0's at the end.
   ii = 14
   WHILE outstr[ii] EQ '0' DO BEGIN
      ii = ii-1
   ENDWHILE

   ; Reconstruct the string.
   saux = aux NE 0 ? '1' : ''
   outstr = sign + saux + StrJoin(outstr[0:ii]) + typeExt + indx

   ; Return it.
   RETURN, outstr

END

