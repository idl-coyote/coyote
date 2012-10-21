; docformat = 'rst'
;
; NAME:
;   cgColor24
;
; PURPOSE:
;   The purpose of this function is to convert a RGB color triple
;   into the equivalent 24-bit long integer. The 24-bit integer
;   can be decomposed into the appropriate color by interpreting
;   the lowest 8 bits as red, the middle 8 bits as green, and the
;   highest 8 bits as blue.
;
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
; The purpose of this function is to convert a RGB color triple
; into the equivalent 24-bit long integer. The 24-bit integer
; can be decomposed into the appropriate color by interpreting
; the lowest 8 bits as red, the middle 8 bits as green, and the
; highest 8 bits as blue. This routine was written to be used with 
; device-independent color programs like `cgColor`.
; 
; :Categories:
;    Graphics, Utilities
;    
; :Returns:
;    A 24-bit long integer that can be decomposed into a color triple value.
;    
; :Params:
;    color: in, required
;       A three-element column or row array representing a color triple. Or an 
;       N-by-three element array of color triples. The values of the elements 
;       must be between 0 and 255.
;
; :Examples:
;    To convert the color triple for the color YELLOW, (255, 255, 0), to the 
;    hexadecimal value '00FFFF'x or the decimal number 65535, type::
;    
;       color = COLOR24([255, 255, 0])
;
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written by:  David Fanning, 3 February 96.
;        Completely revised the algorithm to accept color arrays. 19 October 2000. DWF.
;            
; :Copyright:
;     Copyright (c) 1996-2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgCOLOR24, color

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
