;+
; NAME:
;       ColorButtonBitmap
;
; PURPOSE:
;
;       The purpose of this program is to create a 24-bit bitmap that can be used to
;       create a colored widget button.
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
;       Widget Programming
;
; CALLING SEQUENCE:
;
;       bitmap = ColorButtonBitmap(theText)
;       button = Widget_Button(tlb, Value=bitmap)
;
; REQUIRED INPUTS:
;
;       theText - The text you wish to have on the button.
;
; OUTPUTS:
;
;       bitmap - A 3xMxN byte array, representing a 24-bit image that is used
;                as a button value.
;
; OPTIONAL KEYWORDS:
;
;       BGCOLOR - The name of the background color. For example, 'Yellow', 'Tan', etc.
;                 The name must be compatible with names appropriate for cgColor.
;
;       FGCOLOR - The name of the foreground color. For example, 'Navy', 'Black', etc.
;                 The name must be compatible with names appropriate for cgColor.
;
;
; DEPENDENCIES:
;
;       Reqires cgColor from the Coyote Library:
;
;                     http://www.idlcoyote.com/programs/cgColor.pro
;
; EXAMPLE:
;
;       tlb = Widget_Base(/Row, /Exclusive)
;       button1 = Widget_Button(tlb, Value=ColorButtonBitmap('Button 1')) ; Normal button.
;       button2 = Widget_Button(tlb, Value=ColorButtonBitmap('Button 2', FGCOLOR='YELLOW', BGCOLOR='NAVY'))
;       button3 = Widget_Button(tlb, Value=ColorButtonBitmap('Button 3', BGCOLOR='YELLOW', FGCOLOR='NAVY'))
;       Widget_Control, tlb, /Realize
;
; MODIFICATION HISTORY:
;
;       Written by David Fanning, May 25, 2007 based on code named BitmapForButtonText supplied to the IDL
;       newsgroup by Dick Jackson: http://www.idlcoyote.com/tip_examples/bitmapforbuttontext.pro.
;       Fixed a problem with foreground and background colors that caused them to work correctly only
;           when color decomposition is on--as it should be :-). 6 May 2009.
;-
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
FUNCTION ColorButtonBitmap, theText, BGColor=bgcolor, FGColor=fgcolor

    ;; Error handling. Return to caller.
    ON_ERROR, 2

    ;; Check parameters, define default colors.
    IF N_Elements(theText) EQ 0 THEN theText = 'Button'
    IF N_Elements(bgcolor) EQ 0 THEN bgcolor = 'background'
    IF N_Elements(fgcolor) EQ 0 THEN fgcolor = 'opposite'

    ;; Get the current hardware font.
    wTLB = Widget_Base()
    wBtn = Widget_Button(wTLB)
    font = Widget_Info(wBtn, /FontName)
    Widget_Control, wTLB, /Destroy

    ;;    Find how high the bitmap needs to be for ascenders and descenders
    ;;    (highest and lowest points of characters) in this font
    Window, XSize=100, YSize=100, /Pixmap, /Free, Retain=2
    Device, Set_Font=font
    y0 = 15
    XYOutS, 0, y0, 'Ay', /Device, Font=0    ; Test with high and low letters
    bwWindow = TVRD()                       ; Black background, white text
    WDelete, !D.Window
    IF !Order EQ 1 THEN bwWindow = Reverse(bwWindow, 2) ; Handle !Order=1
    whRowUsed = Where(Max(bwWindow, Dimension=1) NE 0)
    minY = Min(whRowUsed, Max=maxY)

    ;;    Calculate sizes and starting position
    border = 2                              ; Width of border around text
    xSize = (Get_Screen_Size())[0]          ; Maximum width of button text
    ySize = (maxY-minY+1) + border*2
    x0 = border
    y0 = border+(y0-minY)

    ;;    Make window, draw text, read back
    Window, XSize=xSize, YSize=ySize, /Pixmap, /Free, Retain=2
    Erase, cgColor(bgcolor)
    blankRGB = TVRD(True=3)
    XYOutS, x0, y0, theText, /Device, Font=0, $
            Color=cgColor(fgcolor)
    textRGB = TVRD(True=3)
    WDelete, !D.Window
    text2D = Total(textRGB NE blankRGB, 3)
    whereX = Where(Total(text2D, 2) NE 0, nWhereX)

    ;;    Prepare result
    IF nWhereX EQ 0 THEN $                  ; Nothing visible: Return one column
       result = blankRGB[0, *, *] $         ;
    ELSE $                                  ; Else return width used plus
                                            ; border (plus two extra pixels
                                            ; to make Windows button look OK)
       result = textRGB[0:(whereX[nWhereX-1]+border+2) < (xSize-1), *, *]

    ;;    Compensate for reversal if !Order is 1
    IF !Order EQ 1 THEN result = Reverse(result, 2)

    Return, result

END
