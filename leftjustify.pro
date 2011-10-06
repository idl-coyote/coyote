;+
; NAME:
;       LEFTJUSTIFY
;
; PURPOSE:
;
;       This is a utility routine to create a string that is left-justified with
;       respect to a string width.
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
;       justifiedString = LeftJustify(string, width)
;
; AUGUMENTS:
;
;       string:      The string that is to be left-justified. If not supplied, a null
;                    string is returned and no error is issued.
;                    
;       width:       The final width of the left-justified string. The width must be 
;                    longer than the length of the string or an error results. Default: 50.
;
; KEYWORDS:
;
;       None.
;      
; RETURN VALUE:
; 
;       justifiedString: A string of WIDTH, with the string left-justified and the rest of the string
;                     filled with blank characters. 
;
; MODIFICATION HISTORY:
;
;       Written by:  David W. Fanning, 26 January 2009.
;
;-
;******************************************************************************************;
;  Copyright (c) 2009, by Fanning Software Consulting, Inc.                                ;
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
FUNCTION LeftJustify, theString, theWidth

    ON_ERROR, 2 ; Return to caller.

    ; Check parameters.
    IF N_Elements(theString) EQ 0 THEN RETURN, ""
    IF N_Elements(theWidth) EQ 0 THEN theWidth = 50
    
    ; Make sure the string is shorter than the width.
    IF StrLen(theString) GT theWidth THEN Message, 'Requested width is shorter than string.'
    
    ; Left justify the string.
    justifiedString = String(BytArr(theWidth) + 32B)
    StrPut, justifiedString, theString
    RETURN, justifiedString
    
END
