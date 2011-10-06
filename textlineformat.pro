;+
; NAME:
;       TextLineFormat
;
; PURPOSE:
;
;       This is a utility program for taking a line of text and shortening
;       it to a defined maximum length. The result of the function is a string
;       array in which no line of text in the string array is longer than the maximum
;       length. The text is broken into "words" by white space. The algorithm is
;       modified slightly if there are LF (line feeds) in the text, or if any single
;       word in the text is too large to fit on a line.
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
;       formattedText = TextLineFormat(theText)
;
; INPUTS:
;
;       theText:   The line of text that is to be formatted.
;
; KEYWORDS:
;
;       LENGTH:    The maximum line length allowed in the resulting text array.
;                  Set to 60 characters by default. Lines greater than length
;                  can be permitted if Line Feeds (ASCII 10B) are found
;                  in the text or single words are too large to fit on a line.
;
; MODIFICATION HISTORY:
;
;       Written by David Fanning, June 2005.
;       Fixed a small problem with cumulative total not counting spaces between
;          words. Changed the default size to 60. DWF. 18 August 2005.
;       Added check for LF in text to accommodate reading netCDF file attributes. 
;           If LF are present, I break on these, and return. 15 Feb 2008. DWF.
;       Better handling of lines with no white space in them for breaking. 23 March 2009. DWF.
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
FUNCTION TEXTLINEFORMAT, theText, LENGTH=length

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      ok = Error_Message()
      RETURN, theText
   ENDIF

   ; Check arguments.
   IF N_Elements(theText) EQ 0 THEN Message, 'A line of text to format is required.'
   IF Size(theText, /N_Dimensions) NE 0 THEN Message, 'The text line argument must be a scalar variable.'
   IF N_Elements(length) EQ 0 THEN length = 60

   ; Make sure the text is not shorter than the maximum line length.
   IF StrLen(theText) LE length THEN RETURN, theText
   
   ; Are there any Line Feeds (ASCII value 10) in the text? If so, I am going to 
   ; assume the user did this intentionally, and split on these and return.
   testSplit = StrSplit(theText, String(10B), /Preserve_Null, /Extract)
   IF N_Elements(testSplit) GT 1 THEN BEGIN
        RETURN, testSplit
   ENDIF
   
   ; Set up formatted array and maxStrLen variable.
   formattedText = [theText]
   maxStrLen = StrLen(theText)

   ; Do until length of all strings are right.
   WHILE maxStrLen GT length DO BEGIN

      lastIndex = N_Elements(formattedText)-1

      ; Split the string at white space. First, get the length of each sub-string.
      void = StrSplit(formattedText[lastIndex], /Preserve_Null, Length=len)
      p = StrSplit(formattedText[lastIndex], /Preserve_Null, /Extract)
      
      ; If there is no white space. Just return.
      IF N_Elements(p) EQ 1 THEN RETURN, formattedText

      ; Join the lines back together, increasing the size of the formatted
      ; text array by one each time.
      cumLen = Total(len+1, /Cumulative)
      cumLen[N_Elements(cumLen)-1] = cumLen[N_Elements(cumLen)-1] - 1
      index = Value_Locate(cumLen, length)
      IF index GE 0 THEN BEGIN
          formattedText[lastIndex] = StrJoin(p[0:index],' ')
          formattedText = [formattedText, StrJoin(p[index+1:*], ' ')]
      ENDIF
      
      ; The word might be too big to fit on the line. If so, it goes on
      ; the next line, and I continue.
      IF index LT 0 THEN BEGIN
          formattedText[lastIndex] = p[0]
          formattedText = [formattedText, StrJoin(p[1:*], ' ')]
      ENDIF
      
      maxStrLen = StrLen(formattedText[lastIndex + 1])

   ENDWHILE

   RETURN, formattedText

END
