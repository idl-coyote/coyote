; docformat = 'rst'
;
; NAME:
;   ColorsAreIdentical
;
; PURPOSE:
;   Returns a 1 if the two input colors refer to the same color, otherwise returns a 0.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; :Description:
;   Returns a 1 if the two input colors refer to the same color, otherwise returns a 0.
;
; :Categories:
;    Graphics Utility
;    
; :Params:
;    color_1: in, required, type=string/integer/long
;         The first color to compare for "equality".
;    color_2: in, required, type=string/integer/long
;         The second color to compare for "equality".
;       
; :Keywords:
;     None.
;          
; :Examples:
;    Used to compare if two different colors are the same color::
;       IDL> Print, ColorsAreIdentical('white', cgColor('white'))
;       IDL> Print, ColorsAreIdentical(252, !P.Color)
;       IDL> Print, ColorsAreIdentical('white', '255')
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
;        Written, 24 December 2010. DWF.
;        Fixed a typo when first color is INTEGER and second color is STRING. 3 Jan 2011. DWF.
;        Added error handling for out of bounds color values. 25 May 2011. DWF.
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
FUNCTION ColorsAreIdentical, color_1, color_2

    On_Error, 2

    answer = 0
    IF N_Params() NE 2 THEN Message, 'Two positional parameters (colors) are required.'
    IF N_Elements(color_1) EQ 0 THEN Message, 'Color 1 is undefined.'
    IF N_Elements(color_2) EQ 0 THEN Message, 'Color 2 is undefined.'
    
    ; If the color is a "number" string, turn it back into a number.
    ; If all the bytes are between 48 and 57, then this string is a "number".
    IF Size(color_1, /TNAME) EQ 'STRING' THEN BEGIN
        bytecheck = Byte(StrUpCase(color_1))
        i = Where(bytecheck LT 48, lessthan)
        i = Where(bytecheck GT 57, greaterthan)
        IF (lessthan + greaterthan) EQ 0 THEN c1 = Fix(color_1)
    ENDIF
    IF N_Elements(c1) EQ 0 THEN c1 = color_1
    
    ; Colors have to be between 0 and 255.
    IF Size(c1, /TYPE) LE 2 THEN BEGIN
        IF (c1 LT 0) || (c1 GT 255) THEN BEGIN
            msg = 'Color value of ' + StrTrim(c1,2) + ' is outside expected color range of 0 to 255.'
            Message, msg
        ENDIF
    ENDIF
    
    IF Size(color_2, /TNAME) EQ 'STRING' THEN BEGIN
        bytecheck = Byte(StrUpCase(color_2))
        i = Where(bytecheck LT 48, lessthan)
        i = Where(bytecheck GT 57, greaterthan)
        IF (lessthan + greaterthan) EQ 0 THEN c2 = Fix(color_2)
    ENDIF
    IF N_Elements(c2) EQ 0 THEN c2 = color_2
    
    ; Colors have to be between 0 and 255.
    IF Size(c2, /TYPE) LE 2 THEN BEGIN
        IF (c2 LT 0) || (c2 GT 255) THEN BEGIN
            msg = 'Color value of ' + StrTrim(c2,2) + ' is outside expected color range of 0 to 255.'
            Message, msg
        ENDIF
    ENDIF
    
    ; If the colors are the same type, compare them directly
    IF Size(c1, /TYPE) EQ Size(c2, /TYPE) THEN BEGIN
        IF Size(c1, /TNAME) EQ 'STRING' THEN BEGIN
            IF StrUpCase(c1) EQ StrUpCase(c2) THEN answer = 1 ELSE answer = 0
        ENDIF ELSE BEGIN
            IF c1 EQ c2 THEN answer = 1 ELSE answer = 0
        ENDELSE
     ENDIF ELSE BEGIN
    
        ; Get colors so we can restore later.
        TVLCT, r, g, b, /GET
        
        ; Check different purtubations of data type.
        CASE 1 OF
        
           ; First color a STRING, second color an INTEGER.
           (Size(c1, /TYPE) EQ 7) AND (Size(c2, /TYPE) LE 2): BEGIN
                v1 = Transpose(cgColor(c1, /TRIPLE))
                v2 = [r[c2], g[c2], b[c2]]
                answer = Array_Equal(v1, v2)
                END
 
           ; First color a STRING, second color a LONG.
           (Size(c1, /TYPE) EQ 7) AND (Size(c2, /TYPE) EQ 3): BEGIN
                answer = Array_Equal(cgColor(c1, /DECOMPOSED), c2)
                END
             
           ; First color an INTEGER, second color a STRING.    
           (Size(c1, /TYPE) LE 2) AND (Size(c2, /TYPE) EQ 7): BEGIN
                v1 = Transpose(cgColor(c2, /TRIPLE))
                v2 = [r[c1], g[c1], b[c1]]
                answer = Array_Equal(v1, v2)
                END

           ; First color a LONG, second color a STRING.
           (Size(c1, /TYPE) EQ 3) AND (Size(c2, /TYPE) EQ 7): BEGIN
                answer = Array_Equal(cgColor(c2, /DECOMPOSED), c1)
                END

            ; First color a LONG and second color an INTEGER.
           (Size(c1, /TYPE) EQ 3) AND (Size(c2, /TYPE) LE 2): BEGIN
                answer = Array_Equal(c1, cgColor24([r[c2], g[c2], b[c2]]))
                END

           ; First color an INTEGER, second color a LONG.
           (Size(c1, /TYPE) LE 2) AND (Size(c2, /TYPE) EQ 3): BEGIN
                answer = Array_Equal(c2, cgColor24([r[c1], g[c1], b[c1]]))
                END

           ELSE: Message, 'Colors do not meet type expectations. Unsure how to proceed.'
        
        ENDCASE
        
        ; Restore the color vectors, in case they were changed.
        TVLCT, r, g, b
    ENDELSE

    RETURN, answer
END
