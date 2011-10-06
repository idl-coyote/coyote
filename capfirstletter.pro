;+
; NAME:
;       CAPFIRSTLETTER
;
; PURPOSE:
;
;       Given a string, separates the parts by white space, commas,
;       semi-colons, or colons. Each part has the first letter capitalized.
;       The returned string has the capitalized parts separated by a space.
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

;       Utilities
;
; CALLING SEQUENCE:
;
;       capitalizedString = CatFirstLetter(theString)
;
; AUGUMENTS:
;
;       theString:         The input string.
;
; RETURN_VALUE:
;
;      capitalizedString:  The capitalized output string. There is a space between parts
;                          (words) of the input string.
;
; KEYWORDS:
;
;     None.
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 29 July 2005.
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
FUNCTION CapFirstLetter, theString

   Compile_Opt idl2

   ; Return to caller on error.
   On_Error, 2

   ; Separate string into parts.
   parts = StrSplit(theString, " ,;:", /Extract, /Preserve_Null)

   ; Capitalize the first letter of each part.
   FOR j=0, N_Elements(parts)-1 DO BEGIN
      parts[j] = StrLowCase(parts[j])
      firstLetter = StrUpCase(StrMid(parts[j], 0, 1))
      temp = parts[j]
      StrPut, temp, firstLetter, 0
      parts[j] = temp
   ENDFOR

   ; Join the parts together with a space.
   RETURN, StrJoin(parts, " ")
END
