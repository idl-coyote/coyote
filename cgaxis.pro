; docformat = 'rst'
;
; NAME:
;   cgAxis
;
; PURPOSE:
;   Provides a device-independent and color-model-independent way to draw an axis into
;   a graphics window. It is a wrapper to the AXIS command.
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
;   Provides a device-independent and color-model-independent way to draw an axis into
;   a graphics window. It is a wrapper to the AXIS command.
;
; :Categories:
;    Graphics
;    
; :Params:
;    xloc: in, optional, type=depends
;       The X location of the axis. 
;    yloc: in, optional, type=depends
;       The Y location of the axis. 
;    zloc: in, optional, type=depends
;       The Z location of the axis. 
;       
; :Keywords:
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
;     save: in, optional, type=boolean
;         Set this keyword to save the scaling parameters set by the axis for subsequent use.
;     title: in, optional, type=string, default=""
;         The title or annotation that appears on the axis.
;     window: in, optional, type=boolean
;         Set this keyword to add the command to the in the current cgWindow application.
;     xaxis: in, optional, type=integer, default=0
;         If set to 0, the axis is drawn under the plot with the tick marks pointing up; if set 
;         to 1, the axis is drawn on top of the plot with the tick marks pointing down.
;     xlog: in, optional, type=boolean, default=0
;         Set this keyword to specify a logarithmic axis type.
;     xtitle: in, optional, type=string
;         An alternative way to set the `Title` keyword for X axes. Use `Title` instead.
;     yaxis: in, optional, type=integer, default=0
;         If set to 0, the axis is drawn under the plot with the tick marks pointing up; if set 
;         to 1, the axis is drawn on top of the plot with the tick marks pointing down.
;     ylog: in, optional, type=boolean, default=0
;         Set this keyword to specify a logarithmic axis type.
;     ynozero: in, optional, type=boolean, default=0
;         Set this keyword to prevent the Y axis from starting at 0.
;     ytitle: in, optional, type=string
;         An alternative way to set the `Title` keyword for Y axes. Use `Title` instead.
;     zaxis: in, optional, type=integer, default=0
;         Set to 0-3 to position the Z axis in various locatons. See the AXIS documentation.
;     zlog: in, optional, type=boolean, default=0
;         Set this keyword to specify a logarithmic axis type.
;     ztitle: in, optional, type=string
;         An alternative way to set the `Title` keyword for Z axes. Use `Title` instead.
;     _ref_extra: in, optional
;          Any keywords appropriate for the XYOUTS command.
;     
;          
; :Examples:
;    Used like the IDL AXIS command::
;       IDL> cgPlot, cgDemoData(1), YStyle=8, Position=[0.1, 0.1, 0.85, 0.9], /Window
;       IDL> cgAxis, /YAxis, Color='red', YRange=[-500, 500], /Save, /Window
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
;        Written, 25 Janauray 2011. DWF.
;        Modified error handler to restore the entry decomposition state if there is an error. 17 March 2011. DWF
;        Modifed the way I am handling brain dead AXIS command. 30 May 2011. DWF.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
PRO cgAxis, xloc, yloc, zloc, $
            CHARSIZE=charsize, $
            COLOR=scolor, $
            DATA=data, $
            DEVICE=device, $
            FONT=font, $
            NORMAL=normal, $
            SAVE=save, $
            TITLE=title, $
            XAXIS=xaxis, $
            XLOG=xlog, $
            XTITLE=xtitle, $
            YAXIS=yaxis, $
            YLOG=ylog, $
            YTITLE=ytitle, $
            YNOZERO=ynozero, $
            ZAXIS=zaxis, $
            ZLOG=zlog, $
            ZTITLE=ztitle, $
            WINDOW=window, $
            _REF_EXTRA=extra    
    Compile_Opt idl2
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(currentState) NE 0 THEN SetDecomposedState, currentState
        RETURN
    ENDIF
        
    ; Should this be added to a resizeable graphics window?
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        void = cgQuery(COUNT=wincnt)
        IF wincnt EQ 0 THEN cgWindow
        cgWindow, 'cgAxis', xloc, yloc, zloc, $
            CHARSIZE=charsize, $
            COLOR=scolor, $
            DATA=data, $
            DEVICE=device, $
            FONT=font, $
            NORMAL=normal, $
            SAVE=save, $
            TITLE=title, $
            XAXIS=xaxis, $
            XLOG=xlog, $
            XTITLE=xtitle, $
            YAXIS=yaxis, $
            YLOG=ylog, $
            YTITLE=ytitle, $
            YNOZERO=ynozero, $
            ZAXIS=zaxis, $
            ZLOG=zlog, $
            ZTITLE=ztitle, $
            ADDCMD=1, $
            _EXTRA=extra
            
         RETURN
    ENDIF
        
    ; Did the user specify a title with the TITLE keyword?
    IF (N_Elements(xtitle) EQ 0) && (N_Elements(title) NE 0) THEN xtitle = title
    IF (N_Elements(ytitle) EQ 0) && (N_Elements(title) NE 0) THEN ytitle = title
    IF (N_Elements(ztitle) EQ 0) && (N_Elements(title) NE 0) THEN ztitle = title
    
    ; Do you need to reverse the Y title?
    IF (N_Elements(ytitle) NE 0) && Keyword_Set(yreverse) THEN BEGIN
        reverseTitle = ytitle
        ytitle = ""
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Check keywords.
    IF N_Elements(font) EQ 0 THEN font = !P.FONT
    IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharSize(FONT=font)

    ; Draw the axis. Do this in Decomposed color, if possible.
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
     
    ; Draw the axis.
    IF Size(color, /TNAME) EQ 'STRING' THEN thisColor = cgColor(color) ELSE thisColor = color
    
    ; Axis command appears to be brain dead. Doesn't act like any other command
    ; in IDL. Have to do this according to number of positional parameters. Sigh...
    CASE N_Params() OF
      0: Axis, $
            CHARSIZE=charsize, $
            COLOR=thisColor, $
            DATA=data, $
            DEVICE=device, $
            FONT=font, $
            NORMAL=normal, $
            SAVE=save, $
            XAXIS=xaxis, $
            XLOG=xlog, $
            XTITLE=xtitle, $
            YAXIS=yaxis, $
            YLOG=ylog, $
            YTITLE=ytitle, $
            YNOZERO=ynozero, $
            ZAXIS=zaxis, $
            ZLOG=zlog, $
            ZTITLE=ztitle, $
            _STRICT_EXTRA=extra
      1: Axis, xloc, $
            CHARSIZE=charsize, $
            COLOR=thisColor, $
            DATA=data, $
            DEVICE=device, $
            FONT=font, $
            NORMAL=normal, $
            SAVE=save, $
            XAXIS=xaxis, $
            XLOG=xlog, $
            XTITLE=xtitle, $
            YAXIS=yaxis, $
            YLOG=ylog, $
            YTITLE=ytitle, $
            YNOZERO=ynozero, $
            ZAXIS=zaxis, $
            ZLOG=zlog, $
            ZTITLE=ztitle, $
            _STRICT_EXTRA=extra
      2: Axis, xloc, yloc, $
            CHARSIZE=charsize, $
            COLOR=thisColor, $
            DATA=data, $
            DEVICE=device, $
            FONT=font, $
            NORMAL=normal, $
            SAVE=save, $
            XAXIS=xaxis, $
            XLOG=xlog, $
            XTITLE=xtitle, $
            YAXIS=yaxis, $
            YLOG=ylog, $
            YTITLE=ytitle, $
            YNOZERO=ynozero, $
            ZAXIS=zaxis, $
            ZLOG=zlog, $
            ZTITLE=ztitle, $
            _STRICT_EXTRA=extra
      3: Axis, xloc, yloc, zloc, $
            CHARSIZE=charsize, $
            COLOR=thisColor, $
            DATA=data, $
            DEVICE=device, $
            FONT=font, $
            NORMAL=normal, $
            SAVE=save, $
            XAXIS=xaxis, $
            XLOG=xlog, $
            XTITLE=xtitle, $
            YAXIS=yaxis, $
            YLOG=ylog, $
            YTITLE=ytitle, $
            YNOZERO=ynozero, $
            ZAXIS=zaxis, $
            ZLOG=zlog, $
            ZTITLE=ztitle, $
            _STRICT_EXTRA=extra
    ENDCASE
    SetDecomposedState, currentState
   
   ; Restore the color tables.
   IF (!D.Name NE 'Z') THEN TVLCT, rr, gg, bb
   
END
