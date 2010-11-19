; docformat = 'rst'
;
; NAME:
;   FSC_Text
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
;     color: in, optional, type=string/integer/long
;         The color of the text. Color names are those used with FSC_Color. By default,
;         "black", unless the upper-right hand pixel in the display is black, then "white".
;     font: in, optional, type=integer
;         The type of font desired. By default, !P.Font.
;     tt_font: in, optional, type=string
;         The true-type font to use for the text. Only used if FONT=1.
;     _extra: in, optional
;          Any keywords appropriate for the XYOUTS command.
;     
;          
; :Examples:
;    Used like the IDL XYOUTS command::
;       IDL> FSC_Text, 0.5, 0.5, 'This is sample text', ALIGNMENT=0.5, /NORMAL
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
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO FSC_Text, xloc, yloc, text, $
    COLOR=color, $
    FONT=font, $
    TT_FONT=tt_font, $
    _EXTRA=extra
    
    Compile_Opt idl2
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; All three positional parameters are required.
    IF N_Params() NE 3 THEN Message, 'FSC_Text must be called with three positional parameters.'
    
    ; If the text is specified as the first parameter, move things around.
    IF Size(xloc, /TNAME) EQ 'STRING' THEN BEGIN
        temp = xloc
        xloc = yloc
        yloc = text
        text = temp
    ENDIF
    
    ; Check keywords.
    IF N_Elements(font) EQ 0 THEN font = !P.FONT
    IF N_Elements(tt_font) NE 0 THEN BEGIN
        IF font EQ 1 THEN BEGIN
            Device, Set_Font=tt_font, /TT_FONT
        ENDIF
    ENDIF

    ; Get the input color table.
    TVLCT, rr, gg, bb, /Get

    ; Choose a default color
    IF (!D.Name EQ 'PS') AND N_Elements(color) EQ 0 THEN BEGIN
        color = 'black'
    ENDIF ELSE BEGIN
        IF N_Elements(color) EQ 0 THEN BEGIN
            IF (!D.Window GE 0) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
                pixel = TVRead(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                IF N_ELEMENTS(color) EQ 0 THEN BEGIN
                    IF Total(pixel) EQ 765 THEN color = 'black'
                    IF Total(pixel) EQ 0 THEN color = 'white'
                    IF N_Elements(color) EQ 0 THEN color = 'opposite'
                ENDIF 
            ENDIF
        ENDIF ELSE color = !P.Color
    ENDELSE 
    IF N_Elements(color) EQ 0 THEN color = !P.Color
     
    ; Write the text.
    IF Size(color, /TNAME) EQ 'STRING' THEN thisColor = FSC_Color(color) ELSE thisColor = color
    XYOutS, xloc, yloc, text, COLOR=thisColor, FONT=font, _STRICT_EXTRA=extra
   
   ; Restore the color tables.
   IF (!D.Name NE 'Z') THEN TVLCT, rr, gg, bb
   
END