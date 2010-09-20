;+
; NAME:
;       COLOR24
;
; PURPOSE:
;
;       The purpose of this function is to convert a RGB color triple
;       into the equivalent 24-bit long integer. The 24-bit integer
;       can be decomposed into the appropriate color by interpreting
;       the lowest 8 bits as red, the middle 8 bits as green, and the
;       highest 8 bits as blue.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;       Graphics, Color Specification.
;
; CALLING SEQUENCE:
;
;       color = COLOR24(rgb_triple)
;
; INPUTS:
;
;       RGB_TRIPLE: A three-element column or row array representing
;       a color triple. Or an N-by-three element array of color triples.
;       The values of the elements must be between 0 and 255.
;
; KEYWORD PARAMETERS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       None.
;
; EXAMPLE:
;
;       To convert the color triple for the color YELLOW,
;       (255, 255, 0), to the hexadecimal value '00FFFF'x
;       or the decimal number 65535, type:
;
;       color = COLOR24([255, 255, 0])
;
;       This routine was written to be used with device-independent
;       color programs like GETCOLOR.
;
; MODIFICATION HISTORY:
;
;       Written by:  David Fanning, 3 February 96.
;       Completely revised the algorithm to accept color arrays. 19 October 2000. DWF.
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
FUNCTION COLOR24, color

   ; This FUNCTION accepts a [red, green, blue] triple that
   ; describes a particular color and returns a 24-bit long
   ; integer that is equivalent to (can be decomposed into)
   ; that color. The triple can be either a row or column
   ; vector of 3 elements or it can be an N-by-3 array of
   ; color triples.

ON_ERROR, 2

s = Size(color)

IF s[0] EQ 1 THEN BEGIN
   IF s[1] NE 3 THEN Message, 'Input color parameter must be a 3-element vector.'
   RETURN, color[0] + (color[1] * 2L^8) + (color[2] * 2L^16)
ENDIF ELSE BEGIN
   IF s[2] GT 3 THEN Message, 'Input color parameter must be an N-by-3 array.'
   RETURN, color[*,0] + (color[*,1] * 2L^8) + (color[*,2] * 2L^16)
ENDELSE

END ;--------------------------------------------------------------------------------------------
