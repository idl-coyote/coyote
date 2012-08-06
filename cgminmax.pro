; docformat = 'rst'
;
; NAME:
;   cgMinMax
;
; PURPOSE:
; 
;   Calculates and prints the minimum and maximum of a variable.
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
;+
; :Description:
;    Prints the minimum and maximum of an IDL variable.
; 
; :Categories:
;    Utility
;
; :Params:
;    variable: in, required, type=any
;       The variable whose minimum and maximum you wish to know.
;       
; :Keywords:
;     NAN: in, optional, type=boolean
;       Set this keyword to ignore NANs in the variable. Default: 0.
;     TEXT: in, optional, type=string
;       Prepend this string to the output of MinMax. Default: "MinMax: ".
;       
; :Examples:
;   The MinMax routine gives the range of the variable::
;     IDL> a = Findgen(11)
;     IDL> MinMax, a, TEXT='Variable A:'
;     Variable A:   0    11
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
;     Written, 6 August 2012.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
PRO cgMinMax, variable, NAN=nan, TEXT=text

  Compile_Opt idl2
  
  ; Did user provide a text string? Does it end in a blank character?
  IF N_Elements(text) EQ 0 THEN BEGIN
    text = "MinMax: " 
  ENDIF ELSE BEGIN
    ; Add a blank character at the end of the text if it is not already there.
    IF (text NE "") OR (StrMid(text, 0, 1, /REVERSE) NE " ") THEN text = text + " "
  ENDELSE
  
  ; Print, the output.
  IF N_Elements(variable) NE 0 THEN BEGIN
     Print, text,  Min(variable, NAN=Keyword_Set(nan)), $
                   Max(variable, NAN=Keyword_Set(nan)) 
  ENDIF ELSE BEGIN
     ok = Dialog_Message('Please pass MINMAX a variable to examine.')
  ENDELSE
END