; docformat = 'rst'
;
; NAME:
;   cgLegend
;
; PURPOSE:
;   The purpose of this program is to a create simple legend that can be drawn in a 
;   graphics window.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2013, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
;+
;   The purpose of this program is to a create simple legend that can be drawn in a 
;   graphics window. Users needing more complex legends may prefer to use AL_Legend from
;   the NASA Astronomy IDL Library. This program has a more limited set of functionality,
;   especially in regard to placement options. This program is a wrapper for the
;   cgLegendItem object.
;
; :Categories:
;    Graphics
;
; :Examples:
;    A plot with a simple legend::
;        cgDisplay, 800, 450
;        cgPlot, cgDemoData(17), PSym=-6, SymColor='red7', Position=[0.15, 0.15, 0.7, 0.9]
;        cgOPlot, cgDemoData(17), PSym=-15, SymColor='blu7'
;        cgLegend, SymColors=['red7', 'blu7'], PSyms=[6,15], Symsize=1.5, Location=[0.725, 0.9], $
;           Titles=['May 27', 'June 27'], Length=0.075, /Box, VSpace=2.75, /Background, BG_Color='rose'
;
;    Same as the previous example, but in a resizeable graphics window::
;        cgWindow, WXSize=800, WYSize=450
;        cgPlot, cgDemoData(17), PSym=-6, SymColor='red7', Position=[0.15, 0.15, 0.7, 0.9], /AddCmd
;        cgOPlot, cgDemoData(17), PSym=-15, SymColor='blu7', /AddCmd
;        cgLegend, SymColors=['red7', 'blu7'], PSyms=[6,15], Symsize=1.5, Location=[0.725, 0.9], $
;           Titles=['May 27', 'June 27'], Length=0.075, /Box, VSpace=2.75, /Background, $
;           BG_Color='rose', /AddCmd
;           
; :Keywords:
;     addcmd: in, optional, type=boolean,default=0
;        If this keyword is set, the object is added to the resizeable graphics
;        window, cgWindow. The DRAW method of the object is called in cgWindow.
;     background: out, optional, type=boolean, default=0
;        Set this keyword to draw a colored background for the legend.
;     bg_color: out, optional, type=string, default="white"
;        The name of the background color.
;     box: in, optional, type=boolean, default=0
;        Set this keyword to draw a box around the legend items.
;     bx_color: in, optional, type=varies, default="black"
;        The color of the box drawn around the legend items.
;     bx_thick: in, optional, type=float
;        The thickness of the line used to draw the box around the legend items.
;        If not set, use !P.Thick at drawing time.
;     center_sym: in, optional, type=boolean
;        Set this keyword to place a single symbol in the center of the line. The default
;        is to draw a symbol at each endpoint of the line.
;     charsize: in, optional, type=float
;        The character size for the legend text. Uses cgDefCharsize by default.
;        Ignored if using hardware fonts on the display.
;     colors: in, optional, type=varies, default="black"
;        The name of the data color. This is the color of each data line. May be an array.
;     data: in, optional, type=boolean, default=0
;        If set the values specified by the `Location` keyword are taken to be in data
;        coordinate space.
;     hardware: in, optional, type=boolean
;        Set this keyword if you want to output the legend text in a hardware font.
;     length: in, optional, type=float, default=0.075
;        The length of the line connecting symbols in the legend, in normalized
;        coordinates (0 to 1 in the graphics window). Set this equal to 0.0 if
;        you wish to only plot symbols in the legend.
;     linestyles: in, optional, type=integer
;        The line style for drawing each line. May be an array.
;     location: in, optional, type=fltarr
;        A two-element vector giving the X and Y location of upper-left corner of the legend
;        (or legend box, if the `Box` keyword is set) in normalized coordinates. If the `Data`
;        keyword is set, the locations are taken to be in data coordinate space.
;     psyms: in, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46. May be an array.
;     symcolors: in, optional, type=varies
;        The name of the symbol color. By default, the same as the `COLOR` keyword. May be an array.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     symthick: in, optional, type=float
;        The thickness of the symbol. If not set, use !P.Thick at drawing time.
;     tcolors: in, optional, type=varies, default="black"
;        The `Title` color. May be an array.
;     thick: in, optional, type=float
;        The thickness of the line. If not set, use !P.Thick at drawing time.
;     titles: in, optional, type=string/strarr, default='Plot Item'
;        The "title" or text for each legend item.
;     tt_font: in, optional, type=string
;        The name of a true-type font to use for the legend text.
;     visible: in, optional, type=boolean, default=1
;        Set this keyword to determine if the legend should be drawn (visible=1), or
;        if the legend should not be drawn (visible=0).
;     vspace: in, optional, type=float, default=1.5
;         A scale factor for vertical spacing between legend items. This number is multiplied by
;         `Charsize` to determine vertical spacing.
;     window: in, optional, type=boolean, default=0
;         If this keyword is set, the object replaces any commands in a current
;         cgWindow or it opens a new cgWindow and adds itself to it.
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
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;
; :History:
;     Change History::
;        Written 5 Dec 2013. David W Fanning.
;-
PRO cgLegend, $
    ADDCMD=addcmd, $
    BACKGROUND=background, $
    BG_COLOR=bg_color, $
    BOX=box, $
    BX_COLOR=bx_color, $
    BX_THICK=bx_thick, $
    CENTER_SYM=center_sym, $
    CHARSIZE=charsize, $
    COLORS=colors, $
    DATA=data, $
    HARDWARE=hardware, $
    LENGTH=length, $
    LINESTYLES=linestyles, $
    LOCATION=location, $
    PSYMS=psyms, $
    SYMCOLORS=symcolors, $
    SYMSIZE=symsize, $
    SYMTHICK=symthick, $
    TCOLORS=tcolors, $
    THICK=thick, $
    TITLES=titles, $
    TT_FONT=tt_font, $
    VISIBLE=visible, $
    VSPACE=vspace, $
    WINDOW=window
    
    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ; Create a cgLegendItem object.
    object = Obj_New('cgLegendItem', $
        ADDCMD=addcmd, $
        BACKGROUND=background, $
        BG_COLOR=bg_color, $
        BOX=box, $
        BX_COLOR=bx_color, $
        BX_THICK=bx_thick, $
        CENTER_SYM=center_sym, $
        CHARSIZE=charsize, $
        COLORS=colors, $
        DATA=data, $
        DRAW=1, $
        HARDWARE=hardware, $
        LENGTH=length, $
        LINESTYLES=linestyles, $
        LOCATION=location, $
        PSYMS=psyms, $
        SYMCOLORS=symcolors, $
        SYMSIZE=symsize, $
        SYMTHICK=symthick, $
        TCOLORS=tcolors, $
        THICK=thick, $
        TITLES=titles, $
        TT_FONT=tt_font, $
        VISIBLE=visible, $
        VSPACE=vspace, $
        WINDOW=window)
        
    
    ; If you haven't added this to a cgWindow, destroy the object.
    IF ~Keyword_Set(addcmd) && ~Keyword_Set(window) THEN Obj_Destroy, object
    
END