; docformat = 'rst'
;
; NAME:
;   cgPSDims
;
; PURPOSE:
;   The purpose of this function is to return the dimensions of the bounding
;   box of a PostScript file.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2014, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; The purpose of this function is to return the dimensions of the bounding
; box of a PostScript file.
;
; :Categories:
;    Utility
;
; :Params:
;    filename: in, required, type=string
;         The name of a PostScript file from which the bounding box information will be 
;         obtained.
;
; :Keywords:
;    success: out, optional, type=boolean
;         This keyword will be set to 1 if the program is successful and to 0 otherwise.
;
; :Examples:
;    Here is how to use this program::
;       IDL> ps_dims = cgPSDims('myoutput.ps')
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
;        Written, 15 January 2014 by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2014, Fanning Software Consulting, Inc.
;-
FUNCTION cgPSDims, filename, SUCCESS=success

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        success = 0
        IF N_Elements(lun) NE 0 THEN Free_Lun, lun
        RETURN, [0,0]
    ENDIF
    
    ; Must have a filename to proceed.
    IF N_Elements(filename) EQ 0 THEN BEGIN
      filename = Dialog_Pickfile(FILTER=['*.ps', '*.eps'], TITLE='Select a PostScript file to obtain its dimensions.')
      IF filename EQ "" THEN Message, 'A PostScript file name is required to continue.'
     ENDIF
    
    ; Assume success.
    success = 1

    ; Move along in the file until the end of the comment section.
    line = ""
    count = 0
    target = "void"
    buffer = StrArr(100)
    
    OpenR, lun, filename, /GET_LUN
    WHILE target NE '%%EndComments' DO BEGIN
        ReadF, lun, line
        buffer[count] = line
        target = StrMid(line, 0, 13)
        count = count + 1
        IF count MOD 100 EQ 0 THEN buffer = [buffer, StrArr(100)]
    ENDWHILE
    Free_Lun, lun
    
    ; Find the line containing the bounding box in the comment section and
    ; extract the dimension of the PostScript file.
    buffer = buffer[0:count-2]
    FOR j=0,N_Elements(buffer)-1 DO BEGIN
        test = StrMid(buffer[j], 0, 14)
        IF test NE '%%BoundingBox:' THEN Continue
        values = IntArr(4)
        ReadS, StrMid(buffer[j], 15), values
        dims = [values[2]-values[0], values[3]-values[1]]
        Break
    ENDFOR
    
    RETURN, dims
    
END