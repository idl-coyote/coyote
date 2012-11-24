; docformat = 'rst'
;
; NAME:
;   cgCheckForSymbols
;
; PURPOSE:
;   Checks a string for symbols that should be revolved with cgSymbol.
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
; The purpose of this funciton is to check a string for symbols, encased in escape
; characters, that should be revolved with cgSymbol. The cgSymbol name will appear
; with the characters "$\" prepended to the name, and the character "$" appended. All
; Greek characters and other symbols supported by cgSymbol are allowed. Also,
; subscripts and superscripts are allowed::
;     $\upTEXT$ : Raise the text in TEXT to superscript level and reduce character size by 62%.
;     $\extTEXT$ : Raise the text in TEXT to superscript level and reduce character size by 44%.
;     $\downTEXT$ : Lower the text in TEXT to subscript level and reduce character size by 62%.
;     $\subTEXT$ : Lower the text in TEXT to subscript level and reduce character size by 44%.
;     
;
; :Categories:
;    Utilities
;    
; :Returns:
;    The modified string with the cgSymbol code embedded in place of the
;    escaped symbol name.
;    
; :Params:
;    astring: in, required, type=string
;       The string that should be searched for cgSymbol values.
;          
; :Examples:
;    To create a plot that uses the Greek mu character on the X axis and
;    the Angstrom squared symbol on the Y axis::
;    
;       cgPlot, cgDemoData(1), XTitle='Length ($\mu$M)', YTitle='Distance ($\Angstrom$$\up2$)'
;
;    The program has been modified to accept TexToIDL tokens. They must be preceed by
;    a "\tex" prefix. For example, to draw a right arrow between 5 and 3, you would
;    construct the embedded string like this::
;
;         aString = '5 $\tex\rightarrow$ 3'
;         cgText, 0.5, 0.5, /Normal, Align=0.5, Charsize=3.0, aString
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
;        Written by David W. Fanning, 27 July 2012.
;        Modified to check for superscript and subscript codes. 9 November 2012. DWF.
;        Modified to allow the user to use the TexToIDL program from embedded codes.
;            To use a right arrow, for example, aString = '5 $\tex\rightarrow$ 3'
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgCheckForSymbols, aString

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(aString) EQ 0 THEN RETURN, "" ELSE RETURN, aString
    ENDIF
    
    ; Must have a parameter.
    IF N_Elements(aString) EQ 0 THEN Message, 'Must pass a string.'
    
    ; What kind of thing is the parameter?
    type = Size(aString, /TNAME)
    
    
    ; If this is a string, then do your thing.
    IF type EQ 'STRING' THEN BEGIN
    
        FOR j=0,N_Elements(aString)-1 DO BEGIN
            thisString = aString[j]
            
            ; Can you find an escape sequence (i.e., "{\") in this string?
            locstart = StrPos(thisString, '$\')
            IF locStart NE -1 THEN BEGIN
            
               finalLoc = StrPos(StrMid(thisString, locstart+2), '$')
               IF finalLoc NE -1 THEN BEGIN
                  token = StrMid(thisString, locStart+2, finalLoc)
                  strToReplace = StrMid(thisString, locStart, finalLoc+3)
                  
                  ; Special handling for superscripts and subscripts.
                  CASE 1 OF
                      StrUpCase(StrMid(strToReplace, 2, 2)) EQ 'UP': BEGIN
                         replaceStr = '!U' + StrMid(strToReplace,4,StrLen(strToReplace)-5) + '!N'
                         newString = StrMid(thisString, 0, locstart) + replaceStr + StrMid(thisString, locstart+3+StrLen(token))
                         END
                      StrUpCase(StrMid(strToReplace, 2, 4)) EQ 'DOWN': BEGIN
                         replaceStr = '!D' + StrMid(strToReplace,6,StrLen(strToReplace)-7) + '!N'
                         newString = StrMid(thisString, 0, locstart) + replaceStr + StrMid(thisString, locstart+3+StrLen(token))
                         END
                      StrUpCase(StrMid(strToReplace, 2, 3)) EQ 'EXP': BEGIN
                         replaceStr = '!E' + StrMid(strToReplace,5,StrLen(strToReplace)-6) + '!N'
                         newString = StrMid(thisString, 0, locstart) + replaceStr + StrMid(thisString, locstart+3+StrLen(token))
                         END
                      StrUpCase(StrMid(strToReplace, 2, 3)) EQ 'SUB': BEGIN
                         replaceStr = '!I' + StrMid(strToReplace,5,StrLen(strToReplace)-6) + '!N'
                         newString = StrMid(thisString, 0, locstart) + replaceStr + StrMid(thisString, locstart+3+StrLen(token))
                         END

                      ; Provide a mechanism whereby people can use TexToIDL.
                      StrUpCase(StrMid(strToReplace, 2, 3)) EQ 'TEX': BEGIN
                         replaceStr = Call_Function('TexToIDL', StrMid(token,3), FONT=!P.FONT)
                         newString = StrMid(thisString, 0, locstart) + replaceStr + StrMid(thisString, locstart+3+StrLen(token))
                         END
                     ELSE: newString = StrMid(thisString, 0, locstart) + cgSymbol(token) + StrMid(thisString, locstart+3+StrLen(token))

                  ENDCASE
                  
                  thisString = cgCheckForSymbols(newString)
               ENDIF
               
            ENDIF
            
            aString[j] = thisString
         
        ENDFOR
        
        RETURN, aString
        
    ENDIF

END ;----------------------------------------------------------------------------------------------------------------
