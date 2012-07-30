; docformat = 'rst'
;
; NAME:
;   cgSymCat
;
; PURPOSE:
;   This function provides a symbol catalog for specifying a number of plotting symbols.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 20o6-2012, by Fanning Software Consulting, Inc. All rights reserved.      ;
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
; This function provides a symbol catalog for specifying a number of plotting symbols.
;
; :Categories:
;    Graphics
;    
; :Params:
;    theInSymbol: in, required, type=varies
;       The number (or name) of the symbol you wish to use. Possible values are::
;          0  : No symbol.                         NONE
;          1  : Plus sign.                         PLUSSIGN
;          2  : Asterisk.                          ASTERISK
;          3  : Dot (period).                      DOT
;          4  : Open diamond.                      OPENDIAMOND
;          5  : Open upward triangle.              OPENUPTRIANGLE
;          6  : Open square.                       OPENSQUARE
;          7  : X.                                 X
;          8  : Defined by the user with USERSYM.  USERSYM
;          9  : Open circle.                       OPENCIRCLE
;         10  : Histogram style plot.              HISTOGRAM
;         11  : Open downward triangle.            OPENDOWNTRIANGLE
;         12  : Open rightfacing triangle.         OPENRIGHTTRIANGLE
;         13  : Open leftfacing triangle.          OPENLEFTTRIANGLE
;         14  : Filled diamond.                    FILLEDDIAMOND
;         15  : Filled square.                     FILLEDSQUARE
;         16  : Filled circle.                     FILLEDCIRCLE
;         17  : Filled upward triangle.            FILLEDUPTRIANGLE
;         18  : Filled downward triangle.          FILLEDDOWNTRIANGLE
;         19  : Filled rightfacing triangle.       FILLEDRIGHTTRIANGLE
;         20  : Filled leftfacing triangle.        FILLEDLEFTTRIANGLE
;         21  : Hourglass.                         HOURGLASS
;         22  : Filled Hourglass.                  FILLEDHOURGLASS
;         23  : Bowtie.                            BOWTIE
;         24  : Filled bowtie.                     FILLEDBOWTIE
;         25  : Standing Bar.                      STANDINGBAR
;         26  : Filled Standing Bar.               FILLEDSTANDINGBAR
;         27  : Laying Bar.                        LAYINGBAR
;         28  : Filled Laying Bar.                 FILLEDLAYINGBAR
;         29  : Hat up.                            HATUP
;         30  : Hat down.                          HATDOWN
;         31  : Hat right.                         HATRIGHT
;         32  : Hat down.                          HATDOWN
;         33  : Big cross.                         BIGCROSS
;         34  : Filled big cross.                  FILLEDBIGCROSS
;         35  : Circle with plus.                  CIRCLEWITHPLUS
;         36  : Circle with X.                     CIRCLEWITHX
;         37  : Upper half circle.                 UPPERHALFCIRCLE
;         38  : Filled upper half circle.          FILLEDUPPERHALFCIRCLE
;         39  : Lower half circle.                 LOWERHALFCIRCLE
;         40  : Filled lower half circle.          FILLEDLOWERHALFCIRCLE
;         41  : Left half circle.                  LEFTHALFCIRCLE
;         42  : Filled left half circle.           FILLEDLEFTHALFCIRCLE
;         43  : Right half circle.                 RIGHTHALFCIRCLE
;         44  : Filled right half circle.          FILLEDRIGHTHALFCIRCLE
;         45  : Star.                              STAR
;         46  : Filled star.                       FILLEDSTAR
;       
; :Keywords:
;     color: in, optional, type=varies
;         Set this keyword to the color (color table index, 24-bit value, or color name) 
;         of the color desired. 
;     names: in, optional, type=boolean, default=0
;         If this keyword is set, the function returns the names of the symbols allowed.
;         The index number of the name is its symbol number.
;     thick: in, optional, type=float, default=1.0
;         Set this keyword to the thickness desired. Default is 1. 
;          
; :Examples:
; 
;    To use with normal IDL routines::
;       Plot, findgen(11), PSYM=SYMCAT(theSymbol)
;
;    To connect symbols with lines, use a negative value for the PSYM keyword::
;       Plot, findgen(11), PSYM=-SYMCAT(theSymbol)
;       
;    To use with Coyote Library routines, you can call SYMCAT indirectly::
;       cgPlot, findgne(11), PSYM=16, SYMCOLOR='dodger blue'
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
;    Change History::
;       Written by David W. Fanning, 2 September 2006. Loosely based on the
;          program MC_SYMBOL introduced on the IDL newsgroup 1 September 2006,
;          and MPI_PLOTCONFIG__DEFINE from the Coyote Library.
;       Modified to allow PSYM=8 and PSYM=10 to have their normal meanings. This shifted 
;       previous symbols by two values from their old meanings. For example, an hourglass 
;       went from symbol number 19 to 21. 2 October 2006. DWF. 
;       Whoops! Added two symbols but forgot to change limits to allow for them. 4 May 2007. DWF.
;       Added THICK keyword. 21 Aug 2007. DWF.
;       Added COLOR keyword and made THICK keyword apply to all symbols. 11 Nov 2008. DWF.
;       If you are using USERSYM, the "color" can get "stuck" on the last color used to draw
;          a symbol. To "unstick" the color,  you have to call USERSYM without a color keyword
;          before calling it again with a color keyword. This is, without a doubt, the strangest
;          bug I've ever seen in IDL! 20 June 2012. DWF.
;       Changed the way I "unstick" the color from calling USERSYM to calling SYMCAT. This
;          avoids a problem in changing the symbol when SYMCAT is used (unnecessarily) to pass
;          a symbol into Coyote Graphics programs. 9 July 2012. DWF.
;       Added the ability (via Jack Saba) to ask for symbols by name. 17 July 2012. DWF.
;       Added the ability to ask for the color by name. 17 July 2012. DWF.
;       Renamed to cgSYMCAT from SYMCAT. 17 July 2012. DWF.
;       The previous problem with USERSYM colors "sticking" turned out to be a problem
;           with cgGraphicsKeywords, so I removed previous "fix". 18 July 2012. DWF.
;       Modified the program to allow a "negative" string to be used as the symbol name
;           (e.g., PSYM="-star"). 30 July 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2006-2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgSymCat, theInSymbol, COLOR=color, NAMES=names, THICK=thick

   On_Error, 2
   
   IF Keyword_Set(names) THEN BEGIN
      names = [ $
         'None', $
         'Plus Sign', $
         'Asterisk', $
         'Dot', $
         'Open Diamond', $
         'Open Up Triangle', $
         'Open Square', $
         'X', $
         'UserSym', $
         'Open Circle', $
         'Histogram', $
         'Open Down Triangle', $
         'Open Right Triangle', $
         'Open Left Triangle', $
         'Filled Diamod', $
         'Filled Square', $
         'Filled Circle', $
         'Filled Up Triangle', $
         'Filled Down Triangle', $
         'Filled Right Triangle', $
         'Filled Left Triangle', $
         'Hourglass', $
         'Filled Hourglass', $
         'Bowtie', $
         'Filled Bowtie', $
         'Standing Bar', $
         'Filled Standing Bar', $
         'Laying Bar', $
         'Filled Laying Bar', $
         'Hat Up', $
         'Hat Down', $
         'Hat Right', $
         'Hat Left', $
         'Big Cross', $
         'Filled Big Cross', $
         'Circle Plus', $
         'Circle X', $
         'Upper Half Circle', $
         'Fillled Upper Half Circle', $
         'Lower Half Circle', $
         'Filled Lower Half Circle', $
         'Left Half Circle', $
         'Filled Left Half Circle', $
         'Right Half Circle', $
         'Filled Right Half Circle', $
         'Star', $
         'Filled Star' ]
       
       RETURN, names
   ENDIF
   
   ; Is the symbol input as a string?
   IF ( SIZE(theInSymbol,/TNAME) EQ 'STRING' ) THEN BEGIN
   
      ; Is the first character a minus sign?
      IF (StrMid(theInSymbol,0,1) EQ "-") THEN BEGIN
          aMinusString = 1
          theInSymbol = StrMid(theInSymbol,1)
      ENDIF
   
      ; Aliases defined here.
      IF StrUpCase(StrCompress(theInSymbol, /REMOVE_ALL)) EQ 'OPENUPWARDTRIANGLE' THEN theInSymbol = 'OPENUPTRIANGLE'
      IF StrUpCase(StrCompress(theInSymbol, /REMOVE_ALL)) EQ 'OPENDOWNWARDTRIANGLE' THEN theInSymbol = 'OPENDOWNTRIANGLE'
      IF StrUpCase(StrCompress(theInSymbol, /REMOVE_ALL)) EQ 'OPENRIGHTWARDTRIANGLE' THEN theInSymbol = 'OPENRIGHTTRIANGLE'
      IF StrUpCase(StrCompress(theInSymbol, /REMOVE_ALL)) EQ 'OPENLEFTWARDTRIANGLE' THEN theInSymbol = 'OPENLEFTTRIANGLE'
      IF StrUpCase(StrCompress(theInSymbol, /REMOVE_ALL)) EQ 'FILLEDUPWARDTRIANGLE' THEN theInSymbol = 'FILLEDUPTRIANGLE'
      IF StrUpCase(StrCompress(theInSymbol, /REMOVE_ALL)) EQ 'FILLEDDOWNWARDTRIANGLE' THEN theInSymbol = 'FILLEDDOWNTRIANGLE'
      IF StrUpCase(StrCompress(theInSymbol, /REMOVE_ALL)) EQ 'FILLEDDOWNWARDTRIANGLE' THEN theInSymbol = 'FILLEDDOWNTRIANGLE'
      IF StrUpCase(StrCompress(theInSymbol, /REMOVE_ALL)) EQ 'FILLEDLEFTWARDTRIANGLE' THEN theInSymbol = 'FILLEDLEFTTRIANGLE'
      
      CASE StrUpCase(StrCompress(theInSymbol, /REMOVE_ALL)) OF
         '':                         theSymbol = 0
         'NONE':                     theSymbol = 0
         'PLUSSIGN':                 theSymbol = 1
         'ASTERISK':                 theSymbol = 2
         'DOT':                      theSymbol = 3
         'OPENDIAMOND':              theSymbol = 4
         'OPENUPTRIANGLE':           theSymbol = 5
         'OPENSQUARE':               theSymbol = 6
         'X':                        theSymbol = 7
         'USERSYM':                  theSymbol = 8
         'OPENCIRCLE':               theSymbol = 9
         'HISTOGRAM':                theSymbol = 10
         'OPENDOWNTRIANGLE':         theSymbol = 11
         'OPENRIGHTTRIANGLE':        theSymbol = 12
         'OPENLEFTTRIANGLE':         theSymbol = 13
         'FILLEDDIAMOND':            theSymbol = 14
         'FILLEDSQUARE':             theSymbol = 15
         'FILLEDCIRCLE':             theSymbol = 16
         'FILLEDUPTRIANGLE':         theSymbol = 17
         'FILLEDDOWNTRIANGLE':       theSymbol = 18
         'FILLEDRIGHTTRIANGLE':      theSymbol = 19
         'FILLEDLEFTTRIANGLE':       theSymbol = 20
         'HOURGLASS':                theSymbol = 21
         'FILLEDHOURGLASS':          theSymbol = 22
         'BOWTIE':                   theSymbol = 23
         'FILLEDBOWTIE':             theSymbol = 24
         'STANDINGBAR':              theSymbol = 25
         'FILLEDSTANDINGBAR':        theSymbol = 26
         'LAYINGBAR':                theSymbol = 27
         'FILLEDLAYINGBAR':          theSymbol = 28
         'HATUP':                    theSymbol = 29
         'HATDOWN':                  theSymbol = 30
         'HATRIGHT':                 theSymbol = 31
         'HATLEFT':                  theSymbol = 32
         'BIGCROSS':                 theSymbol = 33
         'FILLEDBIGCROSS':           theSymbol = 34
         'CIRCLEPLUS':               theSymbol = 35
         'CIRCLEX':                  theSymbol = 36
         'UPPERHALFCIRCLE':          theSymbol = 37
         'FILLEDUPPERHALFCIRCLE':    theSymbol = 38
         'LOWERHALFCIRCLE':          theSymbol = 39
         'FILLEDLOWERHALFCIRCLE':    theSymbol = 40
         'LEFTHALFCIRCLE':           theSymbol = 41
         'FILLEDLEFTHALFCIRCLE':     theSymbol = 42
         'RIGHTHALFCIRCLE':          theSymbol = 43
         'FILLEDRIGHTHALFCIRCLE':    theSymbol = 44
         'STAR':                     theSymbol = 45
         'FILLEDSTAR':               theSymbol = 46
         ELSE: Message, 'Symbol not defined.'
      ENDCASE
            
   ENDIF ELSE theSymbol = theInSymbol[0]

   ; Error checking.
   IF N_Elements(theSymbol) EQ 0 THEN RETURN, 0
   IF N_Elements(thick) EQ 0 THEN thick = (!P.Thick EQ 0) ? 1 : !P.Thick
   IF (Abs(theSymbol) LT 0) OR (Abs(theSymbol) GT 46) THEN Message, 'Symbol number out of defined range.'
   theFinalSymbol = Fix(Abs(theSymbol))
   
   ; Do you have a color?
   IF N_Elements(color) NE 0 THEN BEGIN
   
      ; Is color a string?
      IF Size(color, /TNAME) EQ 'STRING' THEN thisColor = cgColor(color) ELSE thisColor = color
      
   ENDIF
   
   ; Define helper variables for creating circles.
   phi = Findgen(36) * (!PI * 2 / 36.)
   phi = [ phi, phi(0) ]

   ; Use user defined symbol by default.
   result = 8

   CASE theFinalSymbol OF

       0  : result = 0                                                                               ; No symbol.
       1  : UserSym, [1, -1, 0, 0, 0], [0, 0, 0, -1, 1], THICK=thick, COLOR=thisColor                    ; Plus sign.
       2  : result = 2                                                                               ; Asterisk.
       3  : result = 3                                                                               ; Dot (period).
       4  : UserSym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], THICK=thick, COLOR=thisColor                ; Open diamond.
       5  : UserSym, [ -1, 0, 1, -1 ], [ -1, 1, -1, -1 ], THICK=thick, COLOR=thisColor                   ; Open upward triangle.
       6  : UserSym, [ -1, 1, 1, -1, -1 ], [ 1, 1, -1, -1, 1 ], THICK=thick, COLOR=thisColor             ; Open square.
       7  : result = 7                                                                               ; X.
       8  :                                                                                          ; User defined with USERSYM.
       9  : UserSym, cos(phi), sin(phi), THICK=thick, COLOR=thisColor                                    ; Open circle.
      10  : result = 10                                                                              ; Histogram type plot.
      11  : UserSYm, [ -1, 0, 1, -1 ], [ 1, -1, 1, 1 ], THICK=thick, COLOR=thisColor                     ; Open downward facing triangle
      12  : UserSym, [ -1, 1, -1, -1 ], [1, 0, -1, 1 ], THICK=thick, COLOR=thisColor                     ; Open rightfacing triangle.
      13  : UserSym, [ 1, -1, 1, 1 ], [1, 0, -1, 1 ], THICK=thick, COLOR=thisColor                       ; Open leftfacing triangle.
      14  : UserSym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], /Fill, THICK=thick, COLOR=thisColor         ; Filled diamond.
      15  : UserSym, [ -1, 1, 1, -1, -1 ], [ 1, 1, -1, -1, 1 ], /Fill, THICK=thick, COLOR=thisColor      ; Filled square.
      16  : UserSym, Cos(phi), Sin(phi), /Fill, THICK=thick, COLOR=thisColor                             ; Filled circle.
      17  : UserSym, [ -1, 0, 1, -1 ], [ -1, 1, -1, -1 ], /Fill, THICK=thick, COLOR=thisColor            ; Filled upward triangle.
      18  : UserSym, [ -1, 0, 1, -1 ], [  1, -1, 1, 1 ], /Fill, THICK=thick, COLOR=thisColor             ; Filled downward triangle.
      19  : UserSym, [ -1, 1, -1, -1 ], [1, 0, -1, 1 ], /Fill, THICK=thick, COLOR=thisColor              ; Filled rightfacing triangle.
      20  : UserSym, [ 1, -1, 1, 1 ], [1, 0, -1, 1 ], /Fill, THICK=thick, COLOR=thisColor                ; Filled leftfacing triangle.
      21  : UserSym, [-1, 1,-1,1,-1], [-1,-1, 1,1,-1], THICK=thick, COLOR=thisColor                      ; Hourglass.
      22  : UserSym, [-1, 1,-1,1,-1], [-1,-1, 1,1,-1], /Fill, THICK=thick, COLOR=thisColor               ; Filled Hourglass.
      23  : UserSym, [-1,-1, 1,1,-1], [-1, 1,-1,1,-1], THICK=thick, COLOR=thisColor                      ; Bowtie.
      24  : UserSym, [-1,-1, 1,1,-1], [-1, 1,-1,1,-1], /Fill, THICK=thick, COLOR=thisColor               ; Filled bowtie.
      25  : UserSym, [-0.5,-0.5, 0.5, 0.5,-0.5], [-1, 1, 1,-1,-1], THICK=thick, COLOR=thisColor          ; Standing Bar.
      26  : UserSym, [-0.5,-0.5, 0.5, 0.5,-0.5], [-1, 1, 1,-1,-1], /Fill, THICK=thick, COLOR=thisColor   ; Filled Standing Bar.
      27  : UserSym, [-1,-1, 1, 1,-1], [-0.5, 0.5, 0.5,-0.5,-0.5], THICK=thick, COLOR=thisColor          ; Laying Bar.
      28  : UserSym, [-1,-1, 1, 1,-1], [-0.5, 0.5, 0.5,-0.5,-0.5], /Fill, THICK=thick, COLOR=thisColor   ; Filled Laying Bar.
      29  : UserSym, [-1, -0.5, -0.5, 0.5, 0.5, 1, -1], [-0.7, -0.7, 0.7, 0.7, -0.7, -0.7, -0.7], THICK=thick, COLOR=thisColor ; Hat up.
      30  : UserSym, [-1, -0.5, -0.5, 0.5, 0.5, 1, -1], [0.7, 0.7, -0.7, -0.7, 0.7, 0.7, 0.7], THICK=thick, COLOR=thisColor    ; Hat down.
      31  : UserSym, [-0.7, -0.7, 0.7, 0.7, -0.7, -0.7, -0.7], [-1, -0.5, -0.5, 0.5, 0.5, 1, -1], THICK=thick, COLOR=thisColor ; Hat right.
      32  : UserSym, [0.7, 0.7, -0.7, -0.7, 0.7, 0.7, 0.7], [-1, -0.5, -0.5, 0.5, 0.5, 1, -1], THICK=thick, COLOR=thisColor    ; Hat down.
      33  : UserSym, [1, 0.3, 0.3, -0.3, -0.3, -1, -1, -0.3, -0.3, 0.3, 0.3, 1, 1], $
                     [0.3, 0.3, 1, 1, 0.3, 0.3, -0.3, -0.3, -1, -1, -0.3, -0.3, 0.3], THICK=thick, COLOR=thisColor             ; Big cross.
      34  : UserSym, [1, 0.3, 0.3, -0.3, -0.3, -1, -1, -0.3, -0.3, 0.3, 0.3, 1, 1], $
                     [0.3, 0.3, 1, 1, 0.3, 0.3, -0.3, -0.3, -1, -1, -0.3, -0.3, 0.3], /Fill , THICK=thick, COLOR=thisColor     ; Filled big cross.
      35  : UserSym, [1,.866, .707, .500, 0,-.500,-.707,-.866,-1, $
                      -.866,-.707,-.500, 0, .500, .707, .866, 1, -1, 0, 0, 0], $
                     [0,.500, .707, .866, 1, .866, .707, .500, 0, $
                      -.500,-.707,-.866,-1,-.866,-.707,-.500, 0, 0, 0, 1, -1], THICK=thick, COLOR=thisColor                    ; Circle with plus.
      36  : UserSym, [1,.866, .707, .500, 0,-.500,-.707,-.866,-1, $
                      -.866,-.707,-.500, 0, .500, .707, .866, 1, $
                      .866,.707,-.707,    0, .707,-.707], $
                     [0,.500, .707, .866, 1, .866, .707, .500, 0, $
                      -.500,-.707,-.866,-1,-.866,-.707,-.500, 0, $
                      .500,.707,-.707,    0,-.707, .707]   , THICK=thick, COLOR=thisColor                                      ; Circle with X.
      37  : UserSym, [Cos(phi[0:18]), Cos(phi[0])], [Sin(phi[0:18]), Sin(phi[0])]-0.5, THICK=thick, COLOR=thisColor            ; Upper half circle.
      38  : UserSym, [Cos(phi[0:18]), Cos(phi[0])], [Sin(phi[0:18]), Sin(phi[0])]-0.5, /Fill, THICK=thick, COLOR=thisColor     ; Filled upper half circle.
      39  : UserSym, [Cos(phi[18:36]), Cos(phi[18])], [Sin(phi[18:36]), Sin(phi[18])]+0.5, THICK=thick, COLOR=thisColor        ; Lower half circle.
      40  : UserSym, [Cos(phi[18:36]), Cos(phi[18])], [Sin(phi[18:36]), Sin(phi[18])]+0.5, /Fill, THICK=thick, COLOR=thisColor ; Filled lower half circle.
      41  : UserSym, [Cos(phi[9:27]), Cos(phi[9])]+0.5, [Sin(phi[9:27]), Sin(phi[9])], THICK=thick, COLOR=thisColor            ; Left half circle.
      42  : UserSym, [Cos(phi[9:27]), Cos(phi[9])]+0.5, [Sin(phi[9:27]), Sin(phi[9])], /Fill, THICK=thick, COLOR=thisColor     ; Filled left half circle.
      43  : UserSym, [Cos(phi[27:36]), Cos(phi[0:9]), Cos(phi[27])]-0.5, $
                     [Sin(phi[27:36]), Sin(phi[0:9]), Sin(phi[27])], THICK=thick, COLOR=thisColor                              ; Right half circle.
      44  : UserSym, [Cos(phi[27:36]), Cos(phi[0:9]), Cos(phi[27])]-0.5, $
                     [Sin(phi[27:36]), Sin(phi[0:9]), Sin(phi[27])], /Fill, THICK=thick, COLOR=thisColor                       ; Filled right half circle.
      45  : UserSym, [-1,-.33, 0,.33,1, .33, 0,-.33,-1], $
                     [ 0, .33, 1,.33,0,-.33,-1,-.33, 0], THICK=thick, COLOR=thisColor                                          ; Star.
      46  : UserSym, [-1,-.33, 0,.33,1, .33, 0,-.33,-1], $
                     [ 0, .33, 1,.33,0,-.33,-1,-.33, 0], /Fill, THICK=thick, COLOR=thisColor                                   ; Filled star.
      ELSE: Message, 'Cannot resolve the symbol. Please specify an integer between 0 and 46.'
   ENDCASE

   ; If this is a minus string, we have to modify the result and fix the input.
   IF Keyword_Set(aMinusString) THEN BEGIN
      result = -result
      theInSymbol[0] = '-' + theInSymbol
   ENDIF
   
   RETURN, result
END ;-----------------------------------------------------------------------------------------------------------------------------



