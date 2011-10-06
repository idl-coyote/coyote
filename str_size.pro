;+
; NAME:
;  STR_SIZE
;
; PURPOSE:
;
;  The purpose of this function is to return the proper
;  character size to make a specified string a specifed
;  width in a window. The width is specified in normalized
;  coordinates. The function is extremely useful for sizing
;  strings and labels in resizeable graphics windows.
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
;  Graphics Programs, Widgets.
;
; CALLING SEQUENCE:
;
;  thisCharSize = STR_SIZE(thisSting, targetWidth)
;
; INPUTS:
;
;  thisString:  This is the string that you want to make a specifed
;     target size or width.
;
; OPTIONAL INPUTS:
;
;  targetWidth:  This is the target width of the string in normalized
;     coordinates in the current graphics window. The character
;     size of the string (returned as thisCharSize) will be
;     calculated to get the string width as close as possible to
;     the target width. The default is 0.25.
;
; KEYWORD PARAMETERS:
;
;  INITSIZE:  This is the initial size of the string. Default is 1.0.
;
;  STEP:   This is the amount the string size will change in each step
;     of the interative process of calculating the string size.
;     The default value is 0.05.
;
;  XPOS:   X position of the output test string. This can be
;     used on the Postscript device, where no pixmap windows are
;     available and where therefore the test strings would appear on
;     the printable area. Default is 0.5 on most devices. If !D.NAME
;     is PS, the default is 2.0 to draw the test string out of the
;     drawable window area.
;
;  YPOS:   Y position of the output test string. This can be
;     used on the Postscript device, where no pixmap windows are
;     available and where therefore the test strings would appear on
;     the printable area. Default is 0.5 on most devices. If !D.NAME
;     is PS, the default is 2.0 to draw the test string out of the
;     drawable window area.
;
; OUTPUTS:
;
;  thisCharSize:  This is the size the specified string should be set
;     to if you want to produce output of the specified target
;     width. The value is in standard character size units where
;     1.0 is the standard character size.
;
; EXAMPLE:
;
;  To make the string "Happy Holidays" take up 30% of the width of
;  the current graphics window, type this:
;
;      XYOUTS, 0.5, 0.5, ALIGN=0.5, "Happy Holidays", $
;        CHARSIZE=STR_SIZE("Happy Holidays", 0.3)
;
; MODIFICATION HISTORY:
;
;  Written by: David Fanning, 17 DEC 96.
;  Added a scaling factor to take into account the aspect ratio
;     of the window in determing the character size. 28 Oct 97. DWF
;  Added check to be sure hardware fonts are not selected. 29 April 2000. DWF.
;  Added a pixmap to get proper scaling in skinny windows. 16 May 2000. DWF.
;  Forgot I can't do pixmaps in all devices. :-( Fixed. 7 Aug 2000. DWF.
;  Added support of PostScript at behest of Benjamin Hornberger. 11 November 2004. DWF.
;  Cleaned up the code a bit. 28 Feb 2011. DWF.
;  Fixed non-square window algorithm to reflect my original intentions. 10 June 2011.
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
FUNCTION STR_SIZE, theString, targetWidth, $
   INITSIZE=initsize, $
   STEP=step, $
   XPOS=xpos, $
   YPOS=ypos

    ON_ERROR, 2
    
    ; No hardware fonts.
    thisFont = !P.Font
    IF (thisFont EQ 0) AND (!D.NAME NE 'PS') THEN !P.Font = -1
    
    ; Check positional parameters.
    np = N_PARAMS()
    CASE np OF
       0: MESSAGE, 'One string parameter is required.'
       1: targetWidth = 0.25
       ELSE:
    ENDCASE
    
    ; Check keywords. Assign default values.
    IF N_ELEMENTS(initsize) EQ 0 THEN initsize = 1.0
    IF N_ELEMENTS(step) EQ 0 THEN step = 0.05
    IF N_ELEMENTS(xpos) EQ 0 THEN IF !D.NAME NE 'PS' THEN xpos = 0.5 ELSE xpos = 2.0
    IF N_ELEMENTS(ypos) EQ 0 THEN IF !D.NAME NE 'PS' THEN ypos = 0.5 ELSE ypos = 2.0
    
    ; Save the current window index number.
    currentWindow = !D.Window
    
    ; This is the algorithm that makes this work. Reverse size of
    ; window if is window is taller than wider.
    IF((!D.Flags AND 256) NE 0) THEN BEGIN
        IF !D.X_Size LE !D.Y_Size THEN BEGIN
           Window, /Pixmap, /Free, XSize=!D.X_Size, YSize=!D.Y_Size
        ENDIF ELSE BEGIN
           Window, /Pixmap, /Free, XSize=!D.Y_Size, YSize=!D.X_Size
        ENDELSE
        pixID = !D.Window
    ENDIF

    ; Calculate a trial width.
    strTrialSize = initsize
    XYOUTS, xpos, ypos, ALIGN=0.5, theString, WIDTH=thisWidth, $
          CHARSIZE=-strTrialSize, /NORMAL

    ; Three possibilities for comparison.
    CASE 1 OF
    
        ; Initial size is perfect.
        (thisWidth EQ targetWidth): BEGIN
           theSize = strTrialSize 
           END
    
       ; Initial size is too big.
       (thisWidth GT targetWidth): BEGIN
          REPEAT BEGIN
             XYOUTS, xpos, ypos, ALIGN=0.5, theString, WIDTH=thisWidth, $
                CHARSIZE=-strTrialSize, /NORMAL
             strTrialSize = strTrialSize - step
           ENDREP UNTIL thisWidth LE targetWidth
           theSize = strTrialSize
           END
        
       ; Initial size is too small.
       (thisWidth LT targetWidth): BEGIN
           REPEAT BEGIN
             XYOUTS, xpos, ypos, ALIGN=0.5, theString, WIDTH=thisWidth, $
                CHARSIZE=-strTrialSize, /NORMAL
             strTrialSize = strTrialSize + step
           ENDREP UNTIL thisWidth GT targetWidth
           strTrialSize = strTrialSize - (step/2.0) ; Need a value slightly smaller than target.
           theSize = strTrialSize
           END
    
    ENDCASE
    
    ; Cleanup.
    !P.Font = thisFont
    IF currentWindow NE -1 THEN WSet, currentWindow
    IF N_Elements(pixID) NE 0 THEN WDelete, pixID
    
    RETURN, theSize
END
