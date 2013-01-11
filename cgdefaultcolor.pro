; docformat = 'rst'
;
; NAME:
;   cgDefaultColor
;
; PURPOSE:
;   The purpose of this function is to choose a default color for Coyote Graphics routines.
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
; The purpose of this function is to choose a default color for Coyote Graphics routines.
; 
; :Categories:
;    Graphics
;    
; :Returns:
;    Returns a scalar or vector (depends on the type of the input color) of color 
;    names (strings) to be used as the "color" in Coyote Graphics routines. If the
;    MODE is 1 and the inputColor is of type LONG, then the return value is an array
;    or scalar of type LONG.
; 
; :Params:
;    inputcolour: in, optional
;        The input color. May be undefined, a byte, integer, long, or string. If the
;        device is in indexed color mode at the time the request is made, then all,
;        byte, integer, and long values will be treated as indices into the current
;        color table. The variable may be a vector.
;        
; :Keywords:
;     background: in, optional, type=boolean
;        If this keyword is set, the color is treated as a background color. Otherwise,
;        it is treated as a drawing color.
;     default: in, optional
;         A color of any type allowed as the `inputColour`. Used if the `inputColour` is undefined.
;     mode: in, optional, type=boolean
;         The color mode. A 0 mean indexed color mode. A 1 means decomposed color mode.
;         If not supplied in the call, the color mode is determined at run-time with `GetDecomposedState`.
;         The color mode is *always* determined at run time if the current graphics device 
;         is the PostScript device.
;     traditional: in, optional, type=boolean, default=0
;         Set this keyword if you are using the traditional color scheme of white foreground
;         and black background. If this keyword is set, and the current graphics device is
;         the PostScript device, the colors will be reversed, in the traditional IDL graphics
;         way.
;         
; :Examples:
;    Use as a device independent way to get a color::
;       background = cgDefaultColor(bColor, /Background)
;       color = cgDefaultColor(bColor)
;       cgPlot, cgDemoData, Background=background, Color=color
;       
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;        David W. Fanning 
;        1645 Sheely Drive
;        Fort Collins, CO 80526 USA
;        Phone: 970-221-0438
;        E-mail: david@idlcoyote.com
;        Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 24 December 2011. David W. Fanning.
;        Modified to make sure a LONG integer in indexed color mode is in the range 0-255. 10 Jan 2012. DWF.
;        Modified to make sure MODE is always determined at run-time for PostScript device. 14 Jan 2012. DWF.
;        Allow other data types to be treated as color table index numbers, as long as they are in the
;           range 0 to 255, and the MODE indicates indexed color. 7 March 2012. DWF.
;        Modified so that the variable MODE will not change in the calling program program. 8 March 2012. DWF.
;        Made FOR loop counter a LONG integer. 3 July 2012. DWF.
;        More work on getting the MODE correct to handle LONG integers properly. Now, in PostScript, we get
;            the current mode, except if we have been told what mode to be in. 21 July 2012. DWF.
;        For numerical values less than 256, in indexed color state, I now return the values
;              directly to the user. This should significantly speed up many Coyote Graphics
;              processes. 14 December 2012. DWF.
;        Modified to return byte and integer values as LONG decomposed integers, if decomposed
;              mode is currently in effect. 14 December 2012. DWF.
;        Now setting all background colors to WHITE, not BACKGROUND. 16 Dec 2012. DWF.
;        Whoops! Forgot one of the background colors in the Traditional section. 11 Jan 2013. DWF.
;        
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
FUNCTION cgDefaultColor, inputColour, $
    BACKGROUND=background, $
    DEFAULT=default, $
    MODE=mode, $
    TRADITIONAL=traditional
    
    ; Return to the caller on error.
    On_Error, 2
    
    ; Default values and variables needed for the program.
    background = Keyword_Set(background)
    IF N_Elements(inputColour) NE 0 THEN inputColor = inputColour
    
    ; Getting the mode correct is critical for handling LONG integers correctly.
    ; If we are doing this in PostScript, then we have to get the mode while we
    ; are in the PostScript file (since we try to draw in DECOMPOSED mode always).
    ; But, if we are TOLD what mode to use, we have to honor that, even in PostScript.
    IF ((N_Elements(mode) EQ 0) || (!D.Name EQ 'PS')) THEN BEGIN
       thisMode = GetDecomposedState()
       IF (N_Elements(mode) NE 0) THEN thisMode = Keyword_Set(mode)
    ENDIF ELSE thisMode = Keyword_Set(mode)
    traditional = Keyword_Set(traditional)
    thisDevice = !D.Name
    
    ; Is the color undefined? If so, is there a default color to assign?
    IF N_Elements(inputColor) EQ 0 THEN BEGIN
        IF N_Elements(default) NE 0 THEN inputColor = default
    ENDIF
    
    ; Is the input color still undefined?
    IF (N_Elements(inputColor) EQ 0) THEN BEGIN
       IF background THEN BEGIN
       
          IF traditional THEN BEGIN
             CASE thisDevice OF
                 'PS': inputColor = 'WHITE'
                 'Z': inputColor = 'BLACK'
                 ELSE: inputColor = 'BLACK'
             ENDCASE
          ENDIF ELSE BEGIN
              CASE thisDevice OF
                 'PS': inputColor = 'WHITE'
                 'Z': inputColor = 'WHITE'
                 ELSE: inputColor = 'WHITE'
             ENDCASE
          ENDELSE
       
       ENDIF ELSE BEGIN
       
          IF traditional THEN BEGIN
              CASE thisDevice OF
                 'PS': inputColor = 'BLACK'
                 'Z': inputColor = 'WHITE'
                 ELSE: inputColor = 'WHITE'
             ENDCASE
          ENDIF ELSE BEGIN
              CASE thisDevice OF
                 'PS': inputColor = 'BLACK'
                 'Z': inputColor = 'BLACK'
                 ELSE: inputColor = 'BLACK'
             ENDCASE
          ENDELSE

       ENDELSE
       
    ENDIF
    
    ; If we get here, the input color is defined as *something*.
    ; This is the crux of the problem. What does this color mean?
    ; If it is a byte or integer value, we assume this is an index
    ; into the current color table. We do the same thing with a long
    ; integer if the MODE is 0, or indexed color, and the value is 
    ; between 0 and 255. If it is a string, we can return it directly.
    thisType = Size(inputcolor, /TNAME)
    TVLCT, r, g, b, /GET
    IF (thisType EQ 'BYTE') && (thisMode EQ 0) THEN BEGIN
        RETURN, inputColor 
    ENDIF 
    IF (thisType EQ 'BYTE') && (thisMode EQ 1) THEN BEGIN
        ncolors = N_Elements(inputColor)
        colors = LonArr(ncolors)
        FOR j=0,ncolors-1 DO colors[j] = cgColor24([r[inputColor[j]], g[inputColor[j]], b[inputColor[j]]]) 
        IF N_Elements(colors) EQ 1 THEN RETURN, colors[0] ELSE RETURN, colors
    ENDIF 
    IF (thisType EQ 'INT') && (thisMode EQ 0) THEN BEGIN
        RETURN, inputColor 
    ENDIF 
    IF (thisType EQ 'INT') && (thisMode EQ 1) THEN BEGIN
        ncolors = N_Elements(inputColor)
        colors = LonArr(ncolors)
        FOR j=0,ncolors-1 DO colors[j] = cgColor24([r[inputColor[j]], g[inputColor[j]], b[inputColor[j]]]) 
        IF N_Elements(colors) EQ 1 THEN RETURN, colors[0] ELSE RETURN, colors
    ENDIF 
    IF thisType EQ 'LONG' && (thisMode EQ 0) THEN BEGIN
        RETURN, inputColor 
    ENDIF
    IF thisType EQ 'LONG' && (thisMode EQ 1) THEN BEGIN
        theColors = LonArr(N_Elements(inputColor))    
    ENDIF 
    IF N_Elements(theColors) EQ 0 THEN theColors = StrArr(N_Elements(inputColor))

    ; Fill the color return array.
    FOR j=0L,N_Elements(theColors)-1 DO BEGIN
        thisColor = inputColor[j]
        CASE thisType OF
        
            'STRING': theColors[j] = StrTrim(thisColor,2)
            'LONG': BEGIN
                IF (thisMode EQ 0) && ( (thisColor GE 0) && (thisColor LT 256) ) THEN BEGIN
                    theColors[j] = StrTrim(thisColor, 2)
                ENDIF ELSE BEGIN
                   IF (thisMode EQ 0) && ( (thisColor LT 0) || (thisColor GT 255) ) THEN BEGIN
                       Message, 'Value of LONG integer ' + StrTrim(thisColor,2) + ' is out of indexed color range.'
                   ENDIF ELSE BEGIN
                      theColors[j] = thisColor
                   ENDELSE
                ENDELSE
                END
            ELSE: BEGIN
              ; I feel like an enabler of bad programming practices. Sigh...
              IF (thisColor GE 0) && (thisColor LE 255) && (thisMode EQ 0) THEN BEGIN
                  theColors[j] = StrTrim(Fix(thisColor),2)
              ENDIF ELSE BEGIN
                  Message, 'Cannot determine a color from a value of ' + $
                            StrTrim(thisColor,2) + ' of data type ' + thisType + '.'
              ENDELSE
              END
        ENDCASE
        
    ENDFOR
    
    ; Return the colors after making sure to return a scalar if there is only one element.
    IF N_Elements(theColors) EQ 1 THEN theColors = theColors[0]
    RETURN, theColors
    
END