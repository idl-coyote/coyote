; docformat = 'rst'
;
; NAME:
;   cgNumber_Formatter
;
; PURPOSE:
;   This is a utility routine format a number into a string. It is used primarily for 
;   formatting numbers that will appear in text widgets in widget programs.
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
; This is a utility routine format a number into a string. It is used primarily for 
; formatting numbers that will appear in text widgets in widget programs.
;
; :Categories:
;    Utilities
;    
; :Returns:
;    A string with the formatted number or numbers is returned.
;    
; :Params:
;    number: in, required
;       The number to be turned into a string. May be any data type except complex, 
;       double complex, pointer or object. May be an array of numbers.
;       
; :Keywords:
;    decimals: in, optional, type=integer, default=2
;        Set this keyword to the number of decimal places to be included to the right of 
;        the decimal point in floats and doubles. Set to 2 by default. In numbers with
;        absolute values between 0 and 1, the number of decimals is equivalent to the
;        number of significant digits. See the example below.
;         
; :Examples:
;    Here is how to use this program::
;        IDL> array = [45.123456, -30.123456, !Values.F_Nan, !Values.F_Infinity, $
;                      0.00123456, -0.00123456, 0.0123456, 0.123456, 1.234567]
;        IDL> FOR j=0,N_Elements(array)-1 DO Print, Number_Formatter(array[j], Decimals=4)
;        45.1235
;        -30.1235
;        NaN
;        Inf
;        0.001235
;        -0.001235
;        0.01235
;        0.1235
;        1.2346
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
;        Written, 8 November 2012 by David W. Fanning from the retired Number_Formatter program.
;        The algorithm I was using for numbers between 0 and 1 used a LT in the Where function to
;            find an index. It appears this should have been a LE, instead. 11 Jan 2013. DWF.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgNumber_Formatter, number, DECIMALS=decimals

   On_Error, 2


   IF N_Elements(number) EQ 0 THEN Message, 'A number must be passed as an argument to the function.'
   IF N_Elements(decimals) EQ 0 THEN decimals = 2
   
   ; If the number is a byte, convert it to an integer and return it.
   IF Size(number[0], /TNAME) EQ 'BYTE' THEN RETURN, StrTrim(String(Fix(number), Format='(I3)'),2)

   ; If the number is a string, trim it and return it directly.
   IF Size(number[0], /TNAME) EQ 'STRING' THEN RETURN, StrTrim(number,2)

   ; Number can be an array. Handle that here.
   numElements = N_Elements(number)
   retValue = StrArr(numElements)
   FOR j=0,numElements-1 DO BEGIN
   
      ; Is the number a NaN?
      IF ~Finite(number[j]) THEN BEGIN
         IF Finite(number[j], /NAN) THEN retValue[j] = 'NaN'
         IF Finite(number[j], /Infinity) THEN retValue[j] = 'Inf'
      ENDIF
      IF ~Finite(number[j]) THEN CONTINUE
      
      ; Is this a number between zero and 1? Then the number of decimals should
      ; be adjusted to save the number of significant decimals in the fractional part of this
      ; number. This works as long as the number of zeros in front of the first
      ; significant digits doesn't exceed the number of decimals points asked for
      ; and the "G" formatting doesn't push into exponetial territory.
      IF (Abs(number[j]) GT 0.0001) && (Abs(number[j]) LT 1.0) THEN BEGIN
          savedecimals = decimals
          exponents = 1. / 10.^Indgen(12)
          index = Where(exponents LE Abs(number[j]), count)
          IF count EQ 0 THEN thisIndex = 11 ELSE thisindex = index[0]
          decimals = ((thisindex-1) + decimals)
      ENDIF
      
      ; Is the number a negative value? Deal only with positive values, until the end.
      IF number[j] LT 0.0 THEN minus = 1 ELSE minus = 0
      theNumber = Abs(number[j])

      ; Do the appropriate thing.
      CASE Size(theNumber, /TNAME) OF

         'INT': retValue[j] = StrTrim(String(theNumber),2)

         'LONG': retValue[j] = StrTrim(String(theNumber),2)

         'FLOAT': BEGIN

            ; Format the number with G format.
            aString = StrTrim(String(theNumber, Format='(G)'), 2)

            ; Split the number into a whole part and a fractional part.
            parts = StrSplit(aString, '.', /Extract)
            IF N_Elements(parts) EQ 1 THEN parts = [parts, '0']
            parts[1] = StrTrim(parts[1],2)

            ; Does the fractional part have an E or a D in it?
            loc = StRegEx(parts[1], '[DE]')
            CASE loc OF

               -1: BEGIN ; No exponent.

                   ; Round to the number of decimals you want.
                   fracpart = StrTrim(Round(Double('1.' + parts[1]) * (10LL^decimals), /L64), 2)
                   IF StrMid(fracpart,0,1) EQ '2' THEN BEGIN
                     parts[0] = StrTrim(Long64(parts[0]) + 1, 2)
                   ENDIF
                   parts[1] = StrMid(fracpart, 1)
                   retValue[j] = StrTrim(parts[0],2) + '.' + parts[1]
                   END

               ELSE: BEGIN

                   ; Divide the fractional part of the number up into parts.
                   ; Treat p[0] as you treated parts[1] above. Then put it all
                   ; back together.
                   p = StrSplit(parts[1], '[DdEe]', /RegEx, /Extract)

                   ; Round to the number of decimals you want.
                   fracpart = StrTrim(Round(Double('1.' + p[0]) * (10LL^decimals), /L64), 2)
                   IF StrMid(fracpart,0,1) EQ '2' THEN BEGIN
                     parts[0] = StrTrim(Long64(parts[0]) + 1, 2)
                   ENDIF
                   p[0] = StrMid(fracpart, 1)

                   ; Get the exponent sign and exponent part.
                   expSign = StrMid(p[1],0,1)
                   expPart = StrMid(p[1],1)

                   ; Trim zeros in exponent.
                   firstChar = StrMid(expPart, 0, 1)
                   WHILE firstChar EQ '0' DO BEGIN
                     expPart = StrMid(expPart, 1)
                     firstChar = StrMid(expPart, 0, 1)
                   ENDWHILE

                   ; Put it all back together.
                   retValue[j] = StrTrim(parts[0],2) + '.' + p[0] + StrLowCase(StrMid(parts[1],loc,1)) + expSign + expPart
                   END

            ENDCASE

            ; If you changed the number of decimals you were using, put it back.
            IF N_Elements(savedecimals) NE 0 THEN decimals = savedecimals
            END

         'DOUBLE': BEGIN
            ; Format the number with G format.
            aString = StrTrim(String(theNumber, Format='(G)'), 2)

            ; Split the number into a whole part and a fractional part.
            parts = StrSplit(aString, '.', /Extract)
            IF N_Elements(parts) EQ 1 THEN parts = [parts, '0']
            parts[1] = StrTrim(parts[1],2)

            ; Does the fractional part have an E or a D in it?
            loc = StRegEx(parts[1], '[DE]')
            CASE loc OF

               -1: BEGIN ; No exponent.

                   ; Round to the number of decimals you want.
                   fracpart = StrTrim(Round(Double('1.' + parts[1]) * (10LL^decimals), /L64), 2)
                   IF StrMid(fracpart,0,1) EQ '2' THEN BEGIN
                     parts[0] = StrTrim(Long64(parts[0]) + 1, 2)
                   ENDIF
                   parts[1] = StrMid(fracpart, 1)
                   retValue[j] = StrTrim(parts[0],2) + '.' + parts[1]
                   END

               ELSE: BEGIN

                   ; Divide the fractional part of the number up into parts.
                   ; Treat p[0] as you treated parts[1] above. Then put it all
                   ; back together.
                   p = StrSplit(parts[1], '[DdEe]', /RegEx, /Extract)

                   ; Round to the number of decimals you want.
                   fracpart = StrTrim(Round(Double('1.' + p[0]) * (10LL^decimals), /L64), 2)
                   IF StrMid(fracpart,0,1) EQ '2' THEN BEGIN
                     parts[0] = StrTrim(Long64(parts[0]) + 1, 2)
                   ENDIF
                   p[0] = StrMid(fracpart, 1)

                   ; Get the exponent sign and exponent part.
                   expSign = StrMid(p[1],0,1)
                   expPart = StrMid(p[1],1)

                   ; Trim zeros in exponent.
                   firstChar = StrMid(expPart, 0, 1)
                   WHILE firstChar EQ '0' DO BEGIN
                     expPart = StrMid(expPart, 1)
                     firstChar = StrMid(expPart, 0, 1)
                   ENDWHILE

                   ; Put it all back together.
                   retValue[j] = StrTrim(parts[0],2) + '.' + p[0] + StrLowCase(StrMid(parts[1],loc,1)) + expSign + expPart
                   END

            ENDCASE

            END

         'UINT': retValue[j] = StrTrim(String(theNumber),2)

         'ULONG': retValue[j] = StrTrim(String(theNumber),2)

         'LONG64': retValue[j] = StrTrim(String(theNumber),2)

         'ULONG64': retValue[j] = StrTrim(String(theNumber),2)

         ELSE: Message, 'Cannot format a number of this type: ' + Size(theNumber, /TNAME) + '.'

      ENDCASE

      ; Need a minus sign?
      IF minus THEN retValue[j] = '-' + retValue[j]

   ENDFOR

   IF N_Elements(retValue) EQ 1 THEN RETURN, retValue[0] ELSE RETURN, retValue

END ;----------------------------------------------------------------------------------------
