; docformat = 'rst'
;
; NAME:
;   cgSymbol
;
; PURPOSE:
; This function provides a device-independent way to ask for commonly-used
; symbols (e.g., less than or equal to, Angstrom, degree symbol, etc.),
; including all 24 Greek symbols in upper and lower case, to display with text.
; 
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2011, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; This function provides a device-independent way to ask for commonly-used
; symbols (e.g., less than or equal to, Angstrom, degree symbol, subscripts, superscripts, etc.),
; including all 24 Greek symbols in upper and lower case, to display with text.
;
; For this program to work correctly on your graphics display, you should be using
; Hershey fonts (!P.Font=-1). It will work correctly in PostScript with either 
; hardware fonts (!P.Font=0) or True-Type fonts (!P.Font=1).
; 
; `Greek Symbols <http://www.idlcoyote.com/ps_tips/greeksym.html>` are created by
; calling the Coyote Library routine `cgGreek <http://www.idlcoyote.com/programs/cggreek.pro>' 
; from this program.
; 
; .. image:: cgsymbol.png 
; 
; Normally, rather than calling cgSymbol, the symbols are embedded in Coyote Graphics
; text that are used for axis annotation and so forth. See `Embedding Symbols in Coyote
; Graphics Output <http://www.idlcoyote.com/cg_tips/embedsymbols.php>`. Embedded subscripts
; and superscripts are implemented like this::
;     $\upTEXT$ : Raise the text in TEXT to superscript level and reduce character size by 62%.
;     $\extTEXT$ : Raise the text in TEXT to superscript level and reduce character size by 44%.
;     $\downTEXT$ : Lower the text in TEXT to subscript level and reduce character size by 62%.
;     $\subTEXT$ : Lower the text in TEXT to subscript level and reduce character size by 44%.
;
; :Categories:
;    Graphics
;    
; :Returns:
;    A string variable that represents the requested symbol and can be used
;    in a textual context.
;    
; :Examples:
;     To create a lowercase Greek psi symbol::
;  
;        IDL> cgPlot, findgen(11), XTitle='This title contains ' + $
;                 cgSymbol('psi') + ' as a Greek letter' 
;        IDL> cgPlot, findgen(11), XTitle='This title contains $\psi$ as a Greek letter'
;
;     To create an Angstrom sign::
;  
;        IDL> cgPlot, findgen(11), XTitle='This title contains (' + $
;                cgSymbol('Angstrom') +  ') an Angstrom sign.' 
;        IDL> cgPlot, findgen(11), XTitle='This title contains ($\Angstrom$) an Angstrom sign.'
;       
;     To create subscripts or superscripts::
;  
;        IDL> cgPlot, findgen(11), XTitle='E=mc$\up2$' 
;        IDL> cgPlot, findgen(11), XTitle='H$\sub2$O'
;        IDL> cgPlot, findgen(11), XTitle='H$\upSuper$MT $\Omega$$\subSubscript$', Charsize=2.0
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
;        Written by: David W. Fanning, 2 September 2011. 
;        Added plus-minus symbol. 2 Nov 2011. DWF.
;        Added "up", "down", "exp" "sub" and "n" symbols for subscripting and superscripting. 9 Nov 2012. DWF.
;        Added "division" and "times" signs. 24 Nov 2012. DWF.
;        Updated UNICODE values to display capital letters correctly. 23 Dec 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
;+
; Displays the symbols and their names in a graphics window.
; 
; :Keywords:
;    ps: in, optional, type=boolean, default=0                                
;        Normally, the PostScript version of the symbol is returned automatically if
;        the current device is PostScript and !P.Font is 0 or 1. But, the PostScript version 
;        of the symbol can be obtained at any time and in any device, by setting this keyword.
;    unicode: in, optional, type=boolean, default=0                                
;        If this keyword is set, the function returns the Unicode value for the symbol.
; 
;-
PRO cgSymbol_Example, PS=ps, UNICODE=unicode

    Forward_Function cgSymbol

    Compile_Opt hidden
    
    ; The symbols..
    symbol = [  'alpha', 'beta', 'gamma', 'delta', 'epsilon',  'zeta', $
                'eta', 'theta', 'iota', 'kappa', 'lambda', 'mu', $
                'nu', 'xi', 'omicron', 'pi', 'rho', 'sigma', 'tau', $
                'upsilon', 'phi', 'chi', 'psi', 'omega', 'leq', 'geq', $
                'neq', 'deg', '+-', 'equiv', 'prime', 'angstrom', 'sun', $
                'varphi', 'infinity', 'copyright' ]
    
    ; Output positions.
    x = [0.15, 0.4, 0.65]
    y = Reverse((Indgen(13) + 1) * (1.0 / 14))
    
    ; Need PostScript output?
    IF Keyword_Set(ps) || (!D.Name EQ 'PS') THEN PS_Start
    
    ; Create a window.
    cgDisplay, 800, 500
    
    ; Output the symbols.
    FOR j=0,11 DO BEGIN
        cgText, x[0], y[j], symbol[j] + ': ' + $
            cgSymbol(symbol[j], UNICODE=unicode) + cgSymbol(symbol[j], /CAPITAL, UNICODE=unicode), $
            /NORMAL, CHARSIZE=1.5
        cgText, x[1], y[j], symbol[j+12] + ': ' + $
            cgSymbol(symbol[j+12], UNICODE=unicode) + cgSymbol(symbol[j+12], /CAPITAL, UNICODE=unicode), $
            /NORMAL, CHARSIZE=1.5
        IF j NE 12 THEN cgText, x[2], y[j], symbol[j+24] + ': ' + $
            cgSymbol(symbol[j+24], UNICODE=unicode), $
            /NORMAL, CHARSIZE=1.5
    ENDFOR
    
    ; Clean up PostScript, if needed.
    IF Keyword_Set(ps) || (!D.Name EQ 'PS') THEN PS_End
    
    ; Restore the users window.
    IF N_Elements(thisWindow) NE 0 THEN BEGIN
       IF thisWindow GE 0 THEN WSet, thisWindow
    ENDIF
    
END ; --------------------------------------------------------------------------------------


;+
; This function provides a device-independent way to ask for commonly-used
; symbols (e.g., less than or equal to, Angstrom, degree symbol, etc.),
; including all 24 Greek symbols in upper and lower case, to display with text.
;
; :Params:
;    symbol: in, required, type=string, default="alpha"
;       The name of the symbol desired as a string. Valid string names are the 24 
;       characters of the Greek alphabet, plus commonly used graphical symbols::
;       
;          alpha        nu        leq
;          beta         xi        geq
;          gamma        omicron   neg
;          delta        pi        deg
;          epsilon      rho       equiv
;          zeta         sigma     prime
;          eta          tau       angstrom
;          theta        upsilon   sun
;          iota         phi       varphi
;          kappa        chi       infinity
;          lambda       psi       copyright
;          mu           omega
;                    
;       Note that if the first letter of the name is capitalized, this is
;       the equivalent of setting the `Capital` keyword. Only Greek characters
;       use this method of selecting symbols.
;       
; :Keywords:
;    capital: in, optional, type=boolean, default=0
;        If this keyword is set, the captial Greek letter is returned rather than the lowercase 
;        Greek letter. An alternative way of capitalizing the letter is to make the first letter 
;        of the name an uppercase letter.
;    example: in, optional, type=boolean, default=0             
;        If this keyword is set, the names of symbols and the symbols themselves are written 
;        out in the current graphics window.
;    ps: in, optional, type=boolean, default=0                                
;        Normally, the PostScript version of the symbol is returned automatically if
;        the current device is PostScript and !P.Font is 0 or 1. But, the PostScript version 
;        of the symbol can be obtained at any time and in any device, by setting this keyword.
;    unicode: in, optional, type=boolean, default=0                                
;        If this keyword is set, the function returns the Unicode value for the symbol.
;          
;-
FUNCTION cgSymbol, symbol, CAPITAL=capital, EXAMPLE=example, PS=PS, UNICODE=unicode

    Compile_Opt idl2
    
    ; Return to caller on error.
    ON_Error, 2
    
    ; A common block to communicate with PS_Start.
    COMMON _$FSC_PS_START_, ps_struct
    
    ; Do you wish to see an example?
    IF Keyword_Set(example) THEN BEGIN
        cgSymbol_Example, UNICODE=unicode, PS=ps
        RETURN, ""
    ENDIF

    ; A symbol name is required.
    IF N_Elements(symbol) EQ 0 THEN BEGIN
       Print, 'Syntax: cgSymbol("theSymbol")
       RETURN, ""
    ENDIF
    
    ; Return quietly if the symbol is a null string.
    IF symbol EQ "" THEN RETURN, ""
    
    ; Set up PostScript device for working with Greek letters.
    IF !D.Name EQ 'PS' THEN Device, ISOLATIN1=1
    
    ; Check keywords.
    capital = Keyword_Set(capital)
    
    ; If the first letter of the "symbol" variable is uppercase
    ; the user wants an uppercase symbol, if available.
    firstLetter = StrMid(symbol, 0, 1)
    IF firstLetter EQ StrUpCase(firstLetter) THEN capital = 1
    
    IF Keyword_Set(unicode) THEN BEGIN

        CASE StrLowCase(symbol) OF
            'alpha':   retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'beta':    retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'gamma':   retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'delta':   retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'epsilon': retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'zeta':    retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'eta':     retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'theta':   retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'iota':    retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'kappa':   retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'lambda':  retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'mu':      retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'nu':      retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'xi':      retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'omicron': retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'pi':      retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'rho':     retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'sigma':   retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'tau':     retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'upsilon': retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'phi':     retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'chi':     retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'psi':     retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'omega':   retSymbol = cgGreek(symbol, /UNICODE, CAPITAL=capital)
            'leq':     retSymbol = '!Z(2266)'
            'geq':     retSymbol = '!Z(2267)'
            'neq':     retSymbol = '!Z(2260)'
            'deg':     retSymbol = '!Z(00B0)'
            '+-':      retSymbol = '!Z(00B1)'
            'equiv':   retSymbol = '!Z(2261)'
            'prime':   retSymbol = '!Z(2232)'
            'angstrom':retSymbol = '!Z(00C5)'
            'sun':     retSymbol = '!Z(2609)'
            'varphi':  retSymbol = '!Z(03D5)'
            'infinity':retSymbol = '!Z(221E)'
            'copyright': retSymbol = '!Z(00A9)'
            'up':      retSymbol = '!U'
            'down':    retSymbol = '!D'
            'exp':     retSymbol = '!E'
            'sub':     retSymbol = '!I'
            'n':       retSymbol = '!N'
            'div':     retSymbol = '!Z(00F7)'
            'times':   retSymbol = '!Z(00D7)'
            ELSE: Message, 'The symbol ' + symbol + ' is unrecognized.'
       ENDCASE
           
       ; Return the UNICODE letter.
       RETURN, retSymbol
       
    ENDIF
    
    IF ((!D.Name EQ 'PS') && (!P.FONT NE -1)) || Keyword_Set(PS)  THEN BEGIN
    
        ; Make sure ISOLATIN1 encoding is turned on.
        IF (!D.Name EQ 'PS') THEN DEVICE, /ISOLATIN1
        
        CASE StrLowCase(symbol) OF
            'alpha':   retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'beta':    retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'gamma':   retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'delta':   retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'epsilon': retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'zeta':    retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'eta':     retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'theta':   retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'iota':    retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'kappa':   retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'lambda':  retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'mu':      retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'nu':      retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'xi':      retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'omicron': retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'pi':      retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'rho':     retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'sigma':   retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'tau':     retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'upsilon': retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'phi':     retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'chi':     retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'psi':     retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'omega':   retSymbol = cgGreek(symbol, CAPITAL=capital, /PS)
            'leq':     retSymbol = '!9' + String("243B) + '!X'
            'geq':     retSymbol = '!9' + String("263B) + '!X'
            'neq':     retSymbol = '!9' + String("271B) + '!X' 
            'deg':     retSymbol = '!9' + String("260B) + '!X'
            '+-':      retSymbol = '!9' + String("261B) + '!X'
            'equiv':   retSymbol = '!9' + String("272B) + '!X'
            'prime':   retSymbol = '!9' + String("242B) + '!X'
            'angstrom':retSymbol = '!3' + String("305B) + '!X'
            'sun':     BEGIN
                       IF (ps_struct.font EQ 1) && (StrUpCase(ps_struct.tt_font) EQ 'DEJAVUSANS') THEN BEGIN
                          retSymbol = '!Z(2609)'
                       ENDIF ELSE BEGIN
                         thisDevice = !D.Name
                         Set_Plot, 'PS'
                         Device, /AVANTGARDE, ISOLATIN1=0, /BOOK, FONT_INDEX = 20
                         retSymbol = '!20!S!DO!R!I ' + string(183b) + '!X!N'
                         Set_Plot, thisDevice
                       ENDELSE
                       END
            'varphi':  retSymbol = '!9' + String("152B) + '!X'
            'infinity':retSymbol = '!9' + String("245B) + '!X'
            'copyright':retSymbol = '!9' + String("323B) + '!X'
            'up':      retSymbol = '!U'
            'down':    retSymbol = '!D'
            'exp':     retSymbol = '!E'
            'sub':     retSymbol = '!I'
            'n':       retSymbol = '!N'
            'div':     retSymbol = '!9' + String("270B) + '!X'
            'times':   retSymbol = '!9' + String("264B) + '!X'
           ELSE: Message, 'The symbol ' + symbol + ' is unrecognized.'
        ENDCASE
    
    ENDIF ELSE BEGIN
    
        CASE StrLowCase(symbol) OF
            'alpha':   retSymbol = cgGreek(symbol, CAPITAL=capital)
            'beta':    retSymbol = cgGreek(symbol, CAPITAL=capital)
            'gamma':   retSymbol = cgGreek(symbol, CAPITAL=capital)
            'delta':   retSymbol = cgGreek(symbol, CAPITAL=capital)
            'epsilon': retSymbol = cgGreek(symbol, CAPITAL=capital)
            'zeta':    retSymbol = cgGreek(symbol, CAPITAL=capital)
            'eta':     retSymbol = cgGreek(symbol, CAPITAL=capital)
            'theta':   retSymbol = cgGreek(symbol, CAPITAL=capital)
            'iota':    retSymbol = cgGreek(symbol, CAPITAL=capital)
            'kappa':   retSymbol = cgGreek(symbol, CAPITAL=capital)
            'lambda':  retSymbol = cgGreek(symbol, CAPITAL=capital)
            'mu':      retSymbol = cgGreek(symbol, CAPITAL=capital)
            'nu':      retSymbol = cgGreek(symbol, CAPITAL=capital)
            'xi':      retSymbol = cgGreek(symbol, CAPITAL=capital)
            'omicron': retSymbol = cgGreek(symbol, CAPITAL=capital)
            'pi':      retSymbol = cgGreek(symbol, CAPITAL=capital)
            'rho':     retSymbol = cgGreek(symbol, CAPITAL=capital)
            'sigma':   retSymbol = cgGreek(symbol, CAPITAL=capital)
            'tau':     retSymbol = cgGreek(symbol, CAPITAL=capital)
            'upsilon': retSymbol = cgGreek(symbol, CAPITAL=capital)
            'phi':     retSymbol = cgGreek(symbol, CAPITAL=capital)
            'chi':     retSymbol = cgGreek(symbol, CAPITAL=capital)
            'psi':     retSymbol = cgGreek(symbol, CAPITAL=capital)
            'omega':   retSymbol = cgGreek(symbol, CAPITAL=capital)
            'leq':     retSymbol = '!9' + String("154B) + '!X'
            'geq':     retSymbol = '!9' + String("142B) + '!X'
            'neq':     retSymbol = '!9' + String("75B) + '!X' 
            'deg':     retSymbol = '!9' + String("45B) + '!X'
            '+-':      retSymbol = '!9' + String("53B) + '!X'
            'equiv':   retSymbol = '!9' + String("72B) + '!X'
            'prime':   retSymbol = '!9' + String("140B) + '!X'
            'angstrom':retSymbol = '!3' + String("305B) + '!X'
            'sun':     retSymbol = '!D!9n!N!X'
            'varphi':  retSymbol = '!9' + String("120B) + '!X'
            'infinity':retSymbol = '!9' + String("44B) + '!X'
            'copyright':retSymbol = '!3' + String("251B) + '!X'
            'up':      retSymbol = '!U'
            'down':    retSymbol = '!D'
            'exp':     retSymbol = '!E'
            'sub':     retSymbol = '!I'
            'n':       retSymbol = '!N'
            'div':     retSymbol = '!9' + String("57B) + '!X'
            'times':   retSymbol = '!9' + String("130B) + '!X'
            ELSE: Message, 'The symbol ' + symbol + ' is unrecognized.'      
        ENDCASE
        
      ENDELSE

    RETURN, retSymbol
    
END
