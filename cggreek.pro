;+
; NAME:
;  cgGREEK
;
; PURPOSE:
;
;   This function provides a device-independent way to ask for a Greek letter as
;   a string that can be included, for example, in a plot title. It uses the Greek
;   simplex font (!4) when used with Hershey fonts, and the Symbol font (!9) when
;   used with PostScript or True-Type fonts. Selects the type of Greek character to 
;   return based on value of !P.FONT. Updated now to return the UNICODE values for 
;   Greek characters for those fonts that support them (Macintosh?).
;   
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: david@idlcoyote.com
;   Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; CATEGORY:
;
;   Graphics, Utilities
;
; CALLING SEQUENCE:
;
;   greekString = cgGreek(greekLetter)
;
; RETURN VALUE:
;
;   greekString    A string that represents the Greek letter.
;
; ARGUMENTS:
;
;  greekLetter:    The name of the Greek letter desired. A string. Default: 'alpha'.
;                  Valid string names are the 24 characters of the Greek alphabet.
;                     alpha        nu
;                     beta         xi
;                     gamma        omicron
;                     delta        pi
;                     epsilon      rho
;                     zeta         sigma
;                     eta          tau
;                     theta        upsilon
;                     iota         phi
;                     kappa        chi
;                     lambda       psi
;                     mu           omega
;                    
;                   Note that if the first letter of the name is capitalized, this is
;                   the equivalent of setting the CAPITAL keyword. 
;
; KEYWORDRS:
;
;  CAPTIAL:        If this keyword is set, the captial Greek letter is returned rather 
;                  than the lowercase Greek letter. An alternative way of capitalizing
;                  the letter is to make the first letter of the name an uppercase letter.
;                  
;  EXAMPLE:        If this keyword is set, the names of the Greek characters and their
;                  symbols are written out in the current graphics window.
;                  
;  PS:             Normally, the PostScript version of the greek letter is returned if
;                  the current device is PostScript and !P.Font is 0 or 1. But, the 
;                  PostScript version of the greek letter can be obtained at any time
;                  and in any device, by setting this keyword.
;                                    
;  UNICODE:        If this keyword is set, the function returns the Unicode for the Greek
;                  letter.
;
; EXAMPLE:
;
;  Lowercase PSI:
;  
;     IDL> Plot, findgen(11), XTitle='This title contains ' + $
;           cgGreek('psi') + ' as a Greek letter' 
;
;  Uppercase PSI:
;  
;     IDL> Plot, findgen(11), XTitle='This title contains ' + $
;           cgGreek('Psi') + ' as a Greek letter' 
; NOTES:
; 
;  See the following article for additional information: 
;  
;       http://www.idlcoyote.com/ps_tips/greeksym.html
;       
; RESTRICTIONS:
; 
;  For this program to work correctly on your graphics display, you should be using
;  Hershey fonts (!P.Font=-1). It will work correctly in PostScript with either 
;  hardware fonts (!P.Font=0) or True-Type fonts (!P.Font=1).
;  
; MODIFICATION HISTORY:
;
;  Written by: David W. Fanning, 9 January 2010.
;  An alternative way to get an uppercase letter is to make the first letter of
;     the Greek name uppercase. (Maarten Sneep's suggestion!) 11 Jan 2010. DWF
;  Had the wrong value for the PostScript version of Phi. 26 January 2010. DWF
;  Added UNICODE keyword and values for Greek characters. 11 June 2010. DWF.
;  Changed the branching from !D.NAME EQ 'PS' to !P.FONT NE -1. (This is actually
;      what the documentation says, and what I intended.) 13 Dec 2010. DWF.
;  I don't think the last change did quite want I wanted. More tweaking to make
;      this more responsive to being in a PostScript file. 31 July 2011. DWF.
;  Added PS keyword so the return value is the PostScript file. This is for
;      convenience only, as the return value will be the PostScript value if
;      the current graphics device is PS and FONT is not equal to -1. 30 Aug 2011. DWF.
;  Retired Greek and replaced by cgGreek. 23 Dec 2012. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc.                                ;
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
Forward_Function cgGreek

PRO cgGreek_Example, UNICODE=unicode, PS=ps

    Compile_Opt hidden
    
    ; The 24 Greek letters.
    letter = [  'alpha', 'beta', 'gamma', 'delta', 'epsilon',  'zeta', $
                'eta', 'theta', 'iota', 'kappa', 'lambda', 'mu', $
                'nu', 'xi', 'omicron', 'pi', 'rho', 'sigma', 'tau', $
                'upsilon', 'phi', 'chi', 'psi', 'omega' ]
    
    ; Output positions.
    x = [0.25, 0.6]
    y = Reverse((Indgen(12) + 1) * (1.0 / 13))
    
    ; Need PostScript output?
    IF Keyword_Set(ps) || (!D.Name EQ 'PS') THEN cgPS_Open
    
    ; Create a window.
    cgDisplay, 600, 500
    
    ; Output the letters.
    FOR j=0,11 DO BEGIN
        cgText, x[0], y[j], letter[j] + ': ' + $
            cgGreek(letter[j], UNICODE=unicode) + cgGreek(letter[j], /CAPITAL, UNICODE=unicode), $
            /NORMAL, CHARSIZE=1.5
        cgText, x[1], y[j], letter[j+12] + ': ' + $
            cgGreek(letter[j+12], UNICODE=unicode) + cgGreek(letter[j+12], /CAPITAL, UNICODE=unicode), $
            /NORMAL, CHARSIZE=1.5
    ENDFOR
    
    ; Clean up PostScript, if needed.
    IF Keyword_Set(ps) || (!D.Name EQ 'PS') THEN cgPS_Close

    ; Restore the users window.
    IF N_Elements(thisWindow) NE 0 THEN BEGIN
       IF thisWindow GE 0 THEN WSet, thisWindow
    ENDIF
    
END ; --------------------------------------------------------------------------------------


FUNCTION cgGreek, letter, CAPITAL=capital, EXAMPLE=example, PS=PS, UNICODE=unicode

    Compile_Opt idl2
    
    ; Return to caller on error.
    ON_Error, 2
    
    ; Do you wish to see an example?
    IF Keyword_Set(example) THEN BEGIN
        cgGreek_Example, UNICODE=unicode, PS=ps
        RETURN, ""
    ENDIF

    ; A symbol name is required.
    IF N_Elements(letter) EQ 0 THEN BEGIN
       Print, 'Syntax: Greek("theLetter")
       RETURN, ""
    ENDIF
    
    ; Return quietly if the Greek letter is a null string.
    IF letter  EQ "" THEN RETURN, ""
    
     ; Set up PostScript device for working with Greek letters.
    IF !D.Name EQ 'PS' THEN Device, ISOLATIN1=1
    
    ; Check keywords.
    IF N_Elements(letter) EQ 0 THEN letter = 'alpha'
    capital = Keyword_Set(capital)
    
    ; If the first letter of the "letter" variable is uppercase
    ; the user wants an uppercase Greek letter.
    firstLetter = StrMid(letter, 0, 1)
    IF firstLetter EQ StrUpCase(firstLetter) THEN capital = 1
    
    IF Keyword_Set(unicode) THEN BEGIN
    
        CASE StrLowCase(letter) OF
            'alpha':   greekLetter = (capital) ? '!Z(0391)' : '!Z(03B1)'
            'beta':    greekLetter = (capital) ? '!Z(0392)' : '!Z(03B2)'
            'gamma':   greekLetter = (capital) ? '!Z(0393)' : '!Z(03B3)'
            'delta':   greekLetter = (capital) ? '!Z(0394)' : '!Z(03B4)'
            'epsilon': greekLetter = (capital) ? '!Z(0395)' : '!Z(03B5)'
            'zeta':    greekLetter = (capital) ? '!Z(0396)' : '!Z(03B6)'
            'eta':     greekLetter = (capital) ? '!Z(0397)' : '!Z(03B7)'
            'theta':   greekLetter = (capital) ? '!Z(0398)' : '!Z(03B8)'
            'iota':    greekLetter = (capital) ? '!Z(0399)' : '!Z(03B9)'
            'kappa':   greekLetter = (capital) ? '!Z(039A)' : '!Z(03BA)'
            'lambda':  greekLetter = (capital) ? '!Z(039B)' : '!Z(03BB)'
            'mu':      greekLetter = (capital) ? '!Z(039C)' : '!Z(03BC)'
            'nu':      greekLetter = (capital) ? '!Z(039D)' : '!Z(03BD)'
            'xi':      greekLetter = (capital) ? '!Z(039E)' : '!Z(03BE)'
            'omicron': greekLetter = (capital) ? '!Z(039F)' : '!Z(03BF)'
            'pi':      greekLetter = (capital) ? '!Z(03A0)' : '!Z(03C0)'
            'rho':     greekLetter = (capital) ? '!Z(03A1)' : '!Z(03C1)'
            'sigma':   greekLetter = (capital) ? '!Z(03A3)' : '!Z(03C3)'
            'tau':     greekLetter = (capital) ? '!Z(03A4)' : '!Z(03C4)'
            'upsilon': greekLetter = (capital) ? '!Z(03A5)' : '!Z(03C5)'
            'phi':     greekLetter = (capital) ? '!Z(03A6)' : '!Z(03C6)'
            'chi':     greekLetter = (capital) ? '!Z(03A7)' : '!Z(03C7)'
            'psi':     greekLetter = (capital) ? '!Z(03A8)' : '!Z(03C8)'
            'omega':   greekLetter = (capital) ? '!Z(03A9)' : '!Z(03C9)'
            ELSE: Message, 'The greek letter ' + letter + ' is unrecognized.'
       ENDCASE
           
       ; Return the UNICODE greek letter.
       RETURN, greekLetter
       
    ENDIF
    
    IF ((!D.Name EQ 'PS') && (!P.FONT NE -1)) || Keyword_Set(PS)  THEN BEGIN
    
        ; Make sure ISOLATIN1 encoding is turned on.
        IF (!D.Name EQ 'PS') THEN DEVICE, /ISOLATIN1
        
        CASE StrLowCase(letter) OF
            'alpha':   greekLetter = (capital) ? '!9A!X' : '!9a!X'
            'beta':    greekLetter = (capital) ? '!9B!X' : '!9b!X'
            'gamma':   greekLetter = (capital) ? '!9G!X' : '!9g!X'
            'delta':   greekLetter = (capital) ? '!9D!X' : '!9d!X'
            'epsilon': greekLetter = (capital) ? '!9E!X' : '!9e!X'
            'zeta':    greekLetter = (capital) ? '!9Z!X' : '!9z!X'
            'eta':     greekLetter = (capital) ? '!9H!X' : '!9h!X'
            'theta':   greekLetter = (capital) ? '!9Q!X' : '!9q!X'
            'iota':    greekLetter = (capital) ? '!9I!X' : '!9i!X'
            'kappa':   greekLetter = (capital) ? '!9K!X' : '!9k!X'
            'lambda':  greekLetter = (capital) ? '!9L!X' : '!9l!X'
            'mu':      greekLetter = (capital) ? '!9M!X' : '!9m!X'
            'nu':      greekLetter = (capital) ? '!9N!X' : '!9n!X'
            'xi':      greekLetter = (capital) ? '!9X!X' : '!9x!X'
            'omicron': greekLetter = (capital) ? '!9O!X' : '!9o!X'
            'pi':      greekLetter = (capital) ? '!9P!X' : '!9p!X'
            'rho':     greekLetter = (capital) ? '!9R!X' : '!9r!X'
            'sigma':   greekLetter = (capital) ? '!9S!X' : '!9s!X'
            'tau':     greekLetter = (capital) ? '!9T!X' : '!9t!X'
            'upsilon': greekLetter = (capital) ? '!9U!X' : '!9u!X'
            'phi':     greekLetter = (capital) ? '!9F!X' : '!9j!X'
            'chi':     greekLetter = (capital) ? '!9C!X' : '!9c!X'
            'psi':     greekLetter = (capital) ? '!9Y!X' : '!9y!X'
            'omega':   greekLetter = (capital) ? '!9W!X' : '!9w!X'
            ELSE: Message, 'The greek letter ' + letter + ' is unrecognized.'
        ENDCASE
    
    ENDIF ELSE BEGIN
    
        CASE StrLowCase(letter) OF
            'alpha':   greekLetter = (capital) ? '!4A!X' : '!4a!X'
            'beta':    greekLetter = (capital) ? '!4B!X' : '!4b!X'
            'gamma':   greekLetter = (capital) ? '!4C!X' : '!4cX'
            'delta':   greekLetter = (capital) ? '!4D!X' : '!4d!X'
            'epsilon': greekLetter = (capital) ? '!4E!X' : '!4e!X'
            'zeta':    greekLetter = (capital) ? '!4F!X' : '!4f!X'
            'eta':     greekLetter = (capital) ? '!4G!X' : '!4g!X'
            'theta':   greekLetter = (capital) ? '!4H!X' : '!4h!X'
            'iota':    greekLetter = (capital) ? '!4I!X' : '!4i!X'
            'kappa':   greekLetter = (capital) ? '!4J!X' : '!4j!X'
            'lambda':  greekLetter = (capital) ? '!4K!X' : '!4k!X'
            'mu':      greekLetter = (capital) ? '!4L!X' : '!4l!X'
            'nu':      greekLetter = (capital) ? '!4M!X' : '!4m!X'
            'xi':      greekLetter = (capital) ? '!4N!X' : '!4n!X'
            'omicron': greekLetter = (capital) ? '!4O!X' : '!4o!X'
            'pi':      greekLetter = (capital) ? '!4P!X' : '!4p!X'
            'rho':     greekLetter = (capital) ? '!4Q!X' : '!4q!X'
            'sigma':   greekLetter = (capital) ? '!4R!X' : '!4r!X'
            'tau':     greekLetter = (capital) ? '!4S!X' : '!4s!X'
            'upsilon': greekLetter = (capital) ? '!4T!X' : '!4t!X'
            'phi':     greekLetter = (capital) ? '!4U!X' : '!4u!X'
            'chi':     greekLetter = (capital) ? '!4V!X' : '!4v!X'
            'psi':     greekLetter = (capital) ? '!4W!X' : '!4w!X'
            'omega':   greekLetter = (capital) ? '!4X!X' : '!4x!X'
            ELSE: Message, 'The greek letter ' + letter + ' is unrecognized.'      
        ENDCASE
        
      ENDELSE

    RETURN, greekLetter
    
END
