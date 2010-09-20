;+
; NAME:
;       SYMCAT
;
; PURPOSE:
;
;       This function provides a symbol catalog for specifying a number of plotting symbols.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Graphics
;
; CALLING SEQUENCE:
;
;       Plot, findgen(11), PSYM=SYMCAT(theSymbol)
;
;       To connect symbols with lines, use a negative value for the PSYM keyword:
;
;       Plot, findgen(11), PSYM=-SYMCAT(theSymbol)
;
; INPUTS:
;
;       theSymbol:    The number of the symbol you wish to use. Possible values are:
;
;       0  : No symbol.
;       1  : Plus sign.
;       2  : Asterisk.
;       3  : Dot (period).
;       4  : Open diamond.
;       5  : Open upward triangle.
;       6  : Open square.
;       7  : X.
;       8  : Defined by the user with USERSYM.
;       9  : Open circle.
;      10  : Histogram style plot.
;      11  : Open downward triangle.
;      12  : Open rightfacing triangle.
;      13  : Open leftfacing triangle.
;      14  : Filled diamond.
;      15  : Filled square.
;      16  : Filled circle.
;      17  : Filled upward triangle.
;      18  : Filled downward triangle.
;      19  : Filled rightfacing triangle.
;      20  : Filled leftfacing triangle.
;      21  : Hourglass.
;      22  : Filled Hourglass.
;      23  : Bowtie.
;      24  : Filled bowtie.
;      25  : Standing Bar.
;      26  : Filled Standing Bar.
;      27  : Laying Bar.
;      28  : Filled Laying Bar.
;      29  : Hat up.
;      30  : Hat down.
;      31  : Hat right.
;      32  : Hat down.
;      33  : Big cross.
;      34  : Filled big cross.
;      35  : Circle with plus.
;      36  : Circle with X.
;      37  : Upper half circle.
;      38  : Filled upper half circle.
;      39  : Lower half circle.
;      40  : Filled lower half circle.
;      41  : Left half circle.
;      42  : Filled left half circle.
;      43  : Right half circle.
;      44  : Filled right half circle.
;      45  : Star.
;      46  : Filled star.
;
; RETURN VALUE:
;
;       The return value is whatever is appropriate for passing along
;       to the PSYM keyword of (for example) a PLOT or OPLOT command.
;       For the vast majority of the symbols, the return value is 8.
;
; KEYWORDS:
; 
;       COLOR:  Set this keyword to the color (index or 24-bit value) of
;               the color desired. For example:
;               
;               Plot, Findgen(11), COLOR=FSC_COLOR('yellow'), PSYM=-SYMCAT(4, COLOR=FSC_COLOR('green'))
;
;       THICK:  Set this keyword to the thickness desired. Default is 1. 
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 2 September 2006. Loosely based on the
;          program MC_SYMBOL introduced on the IDL newsgroup 1 September 2006,
;          and MPI_PLOTCONFIG__DEFINE from the Coyote Library.
;       2 October 2006. DWF. Modified to allow PSYM=8 and PSYM=10 to have
;          their normal meanings. This shifted previous symbols by two values
;          from their old meanings. For example, an hourglass went from symbol
;          number 19 to 21.
;       Whoops! Added two symbols but forgot to change limits to allow for them. 4 May 2007. DWF.
;       Added THICK keyword. 21 Aug 2007. DWF.
;       Added COLOR keyword and made THICK keyword apply to all symbols. 11 Nov 2008. DWF.
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
FUNCTION SymCat, theSymbol, THICK=thick, COLOR=color

   On_Error, 2

   ; Error checking.
   IF N_Elements(theSymbol) EQ 0 THEN RETURN, 0
   IF N_Elements(thick) EQ 0 THEN thick = 1
   IF (theSymbol LT 0) OR (theSymbol GT 46) THEN Message, 'Symbol number out of defined range.'
   theSymbol = Fix(theSymbol)

   ; Define helper variables for creating circles.
   phi = Findgen(36) * (!PI * 2 / 36.)
   phi = [ phi, phi(0) ]

   ; Use user defined symbol by default.
   result = 8

   CASE theSymbol OF

       0  : result = 0                                                                               ; No symbol.
;       1  : result = 1   
       1  : UserSym, [1, -1, 0, 0, 0], [0, 0, 0, -1, 1], THICK=thick, COLOR=color                    ; Plus sign.
       2  : result = 2                                                                               ; Asterisk.
       3  : result = 3                                                                               ; Dot (period).
       4  : UserSym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], THICK=thick, COLOR=color                ; Open diamond.
       5  : UserSym, [ -1, 0, 1, -1 ], [ -1, 1, -1, -1 ], THICK=thick, COLOR=color                   ; Open upward triangle.
       6  : UserSym, [ -1, 1, 1, -1, -1 ], [ 1, 1, -1, -1, 1 ], THICK=thick, COLOR=color             ; Open square.
       7  : result = 7                                                                               ; X.
       8  :                                                                                          ; User defined with USERSYM.
       9  : UserSym, cos(phi), sin(phi), THICK=thick, COLOR=color                                    ; Open circle.
      10  : result = 10                                                                              ; Histogram type plot.
      11  : UserSYm, [ -1, 0, 1, -1 ], [ 1, -1, 1, 1 ], THICK=thick, COLOR=color                     ; Open downward facing triangle
      12  : UserSym, [ -1, 1, -1, -1 ], [1, 0, -1, 1 ], THICK=thick, COLOR=color                     ; Open rightfacing triangle.
      13  : UserSym, [ 1, -1, 1, 1 ], [1, 0, -1, 1 ], THICK=thick, COLOR=color                       ; Open leftfacing triangle.
      14  : UserSym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], /Fill, THICK=thick, COLOR=color         ; Filled diamond.
      15  : UserSym, [ -1, 1, 1, -1, -1 ], [ 1, 1, -1, -1, 1 ], /Fill, THICK=thick, COLOR=color      ; Filled square.
      16  : UserSym, Cos(phi), Sin(phi), /Fill, THICK=thick, COLOR=color                             ; Filled circle.
      17  : UserSym, [ -1, 0, 1, -1 ], [ -1, 1, -1, -1 ], /Fill, THICK=thick, COLOR=color            ; Filled upward triangle.
      18  : UserSym, [ -1, 0, 1, -1 ], [  1, -1, 1, 1 ], /Fill, THICK=thick, COLOR=color             ; Filled downward triangle.
      19  : UserSym, [ -1, 1, -1, -1 ], [1, 0, -1, 1 ], /Fill, THICK=thick, COLOR=color              ; Filled rightfacing triangle.
      20  : UserSym, [ 1, -1, 1, 1 ], [1, 0, -1, 1 ], /Fill, THICK=thick, COLOR=color                ; Filled leftfacing triangle.
      21  : UserSym, [-1, 1,-1,1,-1], [-1,-1, 1,1,-1], THICK=thick, COLOR=color                      ; Hourglass.
      22  : UserSym, [-1, 1,-1,1,-1], [-1,-1, 1,1,-1], /Fill, THICK=thick, COLOR=color               ; Filled Hourglass.
      23  : UserSym, [-1,-1, 1,1,-1], [-1, 1,-1,1,-1], THICK=thick, COLOR=color                      ; Bowtie.
      24  : UserSym, [-1,-1, 1,1,-1], [-1, 1,-1,1,-1], /Fill, THICK=thick, COLOR=color               ; Filled bowtie.
      25  : UserSym, [-0.5,-0.5, 0.5, 0.5,-0.5], [-1, 1, 1,-1,-1], THICK=thick, COLOR=color          ; Standing Bar.
      26  : UserSym, [-0.5,-0.5, 0.5, 0.5,-0.5], [-1, 1, 1,-1,-1], /Fill, THICK=thick, COLOR=color   ; Filled Standing Bar.
      27  : UserSym, [-1,-1, 1, 1,-1], [-0.5, 0.5, 0.5,-0.5,-0.5], THICK=thick, COLOR=color          ; Laying Bar.
      28  : UserSym, [-1,-1, 1, 1,-1], [-0.5, 0.5, 0.5,-0.5,-0.5], /Fill, THICK=thick, COLOR=color   ; Filled Laying Bar.
      29  : UserSym, [-1, -0.5, -0.5, 0.5, 0.5, 1, -1], [-0.7, -0.7, 0.7, 0.7, -0.7, -0.7, -0.7], THICK=thick, COLOR=color ; Hat up.
      30  : UserSym, [-1, -0.5, -0.5, 0.5, 0.5, 1, -1], [0.7, 0.7, -0.7, -0.7, 0.7, 0.7, 0.7], THICK=thick, COLOR=color    ; Hat down.
      31  : UserSym, [-0.7, -0.7, 0.7, 0.7, -0.7, -0.7, -0.7], [-1, -0.5, -0.5, 0.5, 0.5, 1, -1], THICK=thick, COLOR=color ; Hat right.
      32  : UserSym, [0.7, 0.7, -0.7, -0.7, 0.7, 0.7, 0.7], [-1, -0.5, -0.5, 0.5, 0.5, 1, -1], THICK=thick, COLOR=color    ; Hat down.
      33  : UserSym, [1, 0.3, 0.3, -0.3, -0.3, -1, -1, -0.3, -0.3, 0.3, 0.3, 1, 1], $
                     [0.3, 0.3, 1, 1, 0.3, 0.3, -0.3, -0.3, -1, -1, -0.3, -0.3, 0.3], THICK=thick, COLOR=color             ; Big cross.
      34  : UserSym, [1, 0.3, 0.3, -0.3, -0.3, -1, -1, -0.3, -0.3, 0.3, 0.3, 1, 1], $
                     [0.3, 0.3, 1, 1, 0.3, 0.3, -0.3, -0.3, -1, -1, -0.3, -0.3, 0.3], /Fill , THICK=thick, COLOR=color     ; Filled big cross.
      35  : UserSym, [1,.866, .707, .500, 0,-.500,-.707,-.866,-1, $
                      -.866,-.707,-.500, 0, .500, .707, .866, 1, -1, 0, 0, 0], $
                     [0,.500, .707, .866, 1, .866, .707, .500, 0, $
                      -.500,-.707,-.866,-1,-.866,-.707,-.500, 0, 0, 0, 1, -1], THICK=thick, COLOR=color                    ; Circle with plus.
      36  : UserSym, [1,.866, .707, .500, 0,-.500,-.707,-.866,-1, $
                      -.866,-.707,-.500, 0, .500, .707, .866, 1, $
                      .866,.707,-.707,    0, .707,-.707], $
                     [0,.500, .707, .866, 1, .866, .707, .500, 0, $
                      -.500,-.707,-.866,-1,-.866,-.707,-.500, 0, $
                      .500,.707,-.707,    0,-.707, .707]   , THICK=thick, COLOR=color                                      ; Circle with X.
      37  : UserSym, [Cos(phi[0:18]), Cos(phi[0])], [Sin(phi[0:18]), Sin(phi[0])]-0.5, THICK=thick, COLOR=color            ; Upper half circle.
      38  : UserSym, [Cos(phi[0:18]), Cos(phi[0])], [Sin(phi[0:18]), Sin(phi[0])]-0.5, /Fill, THICK=thick, COLOR=color     ; Filled upper half circle.
      39  : UserSym, [Cos(phi[18:36]), Cos(phi[18])], [Sin(phi[18:36]), Sin(phi[18])]+0.5, THICK=thick, COLOR=color        ; Lower half circle.
      40  : UserSym, [Cos(phi[18:36]), Cos(phi[18])], [Sin(phi[18:36]), Sin(phi[18])]+0.5, /Fill, THICK=thick, COLOR=color ; Filled lower half circle.
      41  : UserSym, [Cos(phi[9:27]), Cos(phi[9])]+0.5, [Sin(phi[9:27]), Sin(phi[9])], THICK=thick, COLOR=color            ; Left half circle.
      42  : UserSym, [Cos(phi[9:27]), Cos(phi[9])]+0.5, [Sin(phi[9:27]), Sin(phi[9])], /Fill, THICK=thick, COLOR=color     ; Filled left half circle.
      43  : UserSym, [Cos(phi[27:36]), Cos(phi[0:9]), Cos(phi[27])]-0.5, $
                     [Sin(phi[27:36]), Sin(phi[0:9]), Sin(phi[27])], THICK=thick, COLOR=color                              ; Right half circle.
      44  : UserSym, [Cos(phi[27:36]), Cos(phi[0:9]), Cos(phi[27])]-0.5, $
                     [Sin(phi[27:36]), Sin(phi[0:9]), Sin(phi[27])], /Fill, THICK=thick, COLOR=color                       ; Filled right half circle.
      45  : UserSym, [-1,-.33, 0,.33,1, .33, 0,-.33,-1], $
                     [ 0, .33, 1,.33,0,-.33,-1,-.33, 0], THICK=thick, COLOR=color                                          ; Star.
      46  : UserSym, [-1,-.33, 0,.33,1, .33, 0,-.33,-1], $
                     [ 0, .33, 1,.33,0,-.33,-1,-.33, 0], /Fill, THICK=thick, COLOR=color                                   ; Filled star.

   ENDCASE

   RETURN, result
END ;-----------------------------------------------------------------------------------------------------------------------------



