; docformat = 'rst'
;
; NAME:
;   cgText
;
; PURPOSE:
;   Provides a device-independent and color-model-independent way to write text into
;   a graphics window. It is a wrapper to the XYOUTS command.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; :Description:
;   Provides a device-independent and color-model-independent way to write text into
;   a graphics window. It is a wrapper to the XYOUTS command.
;
; :Categories:
;    Graphics
;    
; :Params:
;    xloc: in, required, type=depends
;       The X location of the text. 
;    yloc: in, required, type=depends
;       The Y location of the text. 
;    text: in, optional, type=string
;        The text to output. By default, the calling sequence of the program.
;       
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        Set this keyword to add the command to an cgWindow. Setting this keyword
;        automatically sets the WINDOW keyword, but the command does not erase the
;        graphics window as it would normally.
;     alignment: in, optional, type=integer, default=0
;         Set this keyword to indicate the alignment of the text with respect to the
;         x and y location. 0 is left aligned, 0.5 is centered, and 1.0 is right aligned.
;         The alignment is set to 0.5 if PLACE is set and ALIGNMENT is unspecified. 
;         Otherwise, the default is 0.
;     charsize: in, optional, type=float, default=cgDefCharSize()
;         The character size for axes annotations. Uses cgDefCharSize to select default
;         character size, unless !P.Charsize is set, in which case !P.Charsize is always used.
;     color: in, optional, type=string/integer/long
;         The color of the text. Color names are those used with cgColor. By default,
;         "black", unless the upper-right hand pixel in the display is black, then "white".
;     data: in, optional, type=boolean
;         Set this keyword to indicate xloc and yloc are in data coordinates. Data coordinates
;         are the default, unless DEVICE or NORMAL is set.
;     device: in, optional, type=boolean
;         Set this keyword to indicate xloc and yloc are in device coordinates.
;     font: in, optional, type=integer, default=!P.Font
;         The type of font desired. By default, !P.Font.
;     normal: in, optional, type=boolean
;         Set this keyword to indicate xloc and yloc are in normalized coordinates.
;     outloc: out, optional, type=various
;         Only used if PLACE is set, this is a two-element array containing the xloc and yloc
;         of the cursor position in the window.
;     place: in, optional, type=boolean
;          Set this keyword if you wish to click the cursor in the graphics window to place
;          the text. If this keyword is set, you do not need to specify the xloc and yloc
;          positional parameters. The first positional parameter is assumed to be the text.
;          The clicked location will be returned in the OUTLOC variable. If the ALIGNMENT
;          keyword is not set, it will be set to 0.5 to set "center" as the default placement
;          alignment.
;     tt_font: in, optional, type=string
;         The true-type font to use for the text. Only used if FONT=1.
;     width: out, optional, type=float
;         Set this keyword to a named variable in which to return the width of the text string, 
;         in normalized coordinate units.
;     window: in, optional, type=boolean
;         Set this keyword to add the command to the in the current cgWindow application.
;     _extra: in, optional
;          Any keywords appropriate for the XYOUTS command.
;     
;          
; :Examples:
;    Used like the IDL XYOUTS command::
;       IDL> cgText, 0.5, 0.5, 'This is sample text', ALIGNMENT=0.5, /NORMAL
;       IDL> cgText, /PLACE, 'Use the cursor to locate this text', COLOR='dodger blue'
;       
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: davidf@dfanning.com
;           Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; :History:
;     Change History::
;        Written, 19 November 2010. DWF.
;        Corrected a problem with setting text color and added PLACE and OUTLOC 
;            keywords. 25 Nov 2010. DWF.
;        Humm, that text color problem got reset to the old way! Sheesh! Fixed. 9 Dec 2010. DWF.
;        Modified the way the default color is selected. Drawing with DECOMPOSED 
;             if possible. 30 Dec 2010. DWF.
;        Keywords collected with _REF_EXTRA so WIDTH can be returned. Added WIDTH keyword. 6 Jan 2011. DWF.
;        Moved setting to decomposed color before color selection process to avoid PostScript
;             background problems when passed 24-bit color integers. 12 Jan 2011. DWF.   
;        Added Window keyword. 24 Jan 2011. DWF.
;        Added ability to return WIDTH from resizeable graphics windows and added ADDCMD 
;              keyword. 24 Feb 2011. DWF.
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO cgText, xloc, yloc, text, $
    ADDCMD=addcmd, $
    ALIGNMENT=alignment, $
    CHARSIZE=charsize, $
    COLOR=scolor, $
    DATA=data, $
    DEVICE=device, $
    FONT=font, $
    NORMAL=normal, $
    OUTLOC=outloc, $
    PLACE=place, $
    TT_FONT=tt_font, $
    WIDTH=width, $
    WINDOW=window, $
    _REF_EXTRA=extra
    
    Compile_Opt idl2
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Did the user pass parameters?
    IF N_Params() EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: cgText, xlocation, ylocation, text'
        RETURN
    ENDIF
    
    ; Should this be added to a resizeable graphics window?
    IF (Keyword_Set(window) || Keyword_Set(addcmd)) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        void = cgQuery(COUNT=wincnt)
        IF (wincnt EQ 0) && Keyword_Set(window) THEN cgWindow
        cgWindow, 'cgText', xloc, yloc, text, $
            ALIGNMENT=alignment, $
            CHARSIZE=charsize, $
            COLOR=scolor, $
            DATA=data, $
            DEVICE=device, $
            FONT=font, $
            NORMAL=normal, $
            OUTLOC=outloc, $
            PLACE=place, $
            TT_FONT=tt_font, $
            WIDTH=width, $
            ADDCMD=1, $
            _EXTRA=extra
            
         ; You might want to get the width of the window back.
         IF Arg_Present(width) THEN BEGIN
            void = cgQuery(DIMENSIONS=dims, /CURRENT)
            Window, /PIXMAP, XSIZE=dims[0], YSIZE=dims[1]
            IF N_Elements(font) EQ 0 THEN font = !P.FONT
            IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharSize(FONT=font)
            XYOUTS, 0.5, 0.5, text, /NORMAL, WIDTH=width, CHARSIZE=charsize
            WDelete, !D.Window
         ENDIF
            
         RETURN
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    IF Keyword_Set(place) THEN BEGIN
    
        ; Make sure this device is appropriate for a CURSOR command.
        IF ((!D.Flags AND 256) EQ 0) THEN $
            Message, 'Cannot use the PLACE keyword with the current graphics device.'
            
        ; There must be a window open.
        IF !D.Window LT 0 THEN $
            Message, 'There is no current graphics window open.'
            
        ; Print some instructions.
        Print, ""
        Print, 'Click in current graphics window (window index' + $
            StrTrim(!D.Window,2) + ') to place text.'
    
        ; The text string is the only positional parameter.
        textStr = xloc
        Cursor, x, y, /DOWN, DEVICE=device, NORMAL=normal, DATA=data
        outloc = [x, y]
        IF N_Elements(alignment) EQ 0 THEN alignment = 0.5
    
    ENDIF ELSE BEGIN
    
        ; All three positional parameters are required.
        IF N_Params() NE 3 THEN Message, 'cgText must be called with three positional parameters.'
  
        ; If the text is specified as the first parameter, move things around.
        IF Size(xloc, /TNAME) EQ 'STRING' THEN BEGIN
            temp = xloc
            x = yloc
            y = text
            textStr = temp
        ENDIF ELSE BEGIN
            x = xloc
            y = yloc
            textStr = text
        ENDELSE
        
    ENDELSE
    
    
    ; Check keywords.
    IF N_Elements(font) EQ 0 THEN font = !P.FONT
    IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharSize(FONT=font)
    IF N_Elements(tt_font) NE 0 THEN BEGIN
        IF font EQ 1 THEN BEGIN
            Device, Set_Font=tt_font, /TT_FONT
        ENDIF
    ENDIF

    ; Write the text. Do this in Decomposed color, if possible.
    SetDecomposedState, 1, CURRENTSTATE=currentState

    ; Get the input color table.
    TVLCT, rr, gg, bb, /Get

    ; Choose a color.
    IF N_Elements(sColor) EQ 0 THEN BEGIN
           IF !D.Name EQ 'PS' THEN BEGIN
                sColor = 'OPPOSITE' 
           ENDIF ELSE BEGIN
                IF (!D.Window GE 0) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
                    pixel = cgSnapshot(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF (Total(pixel) EQ 765) THEN sColor = 'BLACK'
                    IF (Total(pixel) EQ 0) THEN sColor = 'WHITE'
                    IF N_Elements(sColor) EQ 0 THEN sColor = 'OPPOSITE'
                ENDIF ELSE sColor = 'OPPOSITE'
           ENDELSE
     ENDIF
    IF N_Elements(sColor) EQ 0 THEN color = !P.Color ELSE  color = sColor
    IF Size(color, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN color = Byte(color)
    IF Size(color, /TYPE) LE 2 THEN color = StrTrim(Fix(color),2)
     
    ; Draw the text.
    IF Size(color, /TNAME) EQ 'STRING' THEN thisColor = cgColor(color) ELSE thisColor = color
    XYOutS, x, y, textStr, CHARSIZE=charsize, COLOR=thisColor, FONT=font, ALIGNMENT=alignment, $
        DATA=data, DEVICE=device, NORMAL=normal, WIDTH=width, _STRICT_EXTRA=extra
   SetDecomposedState, currentState
   
   ; Restore the color tables.
   IF (!D.Name NE 'Z') THEN TVLCT, rr, gg, bb
   
END