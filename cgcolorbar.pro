; docformat = 'rst'
;
; NAME:
;   cgColorbar
;
; PURPOSE:
;   The purpose of this routine is to add a color bar to the current graphics window.
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
; The purpose of this routine is to add a color bar to the current graphics window.
; 
; .. image:: cgcolorbar2.png
; 
; .. image:: cgcolorbar4.png
; 
; .. image:: cgcolorbar3.png
; 
; .. image:: cgcolorbar1.png
; 
; .. image:: cgcolorbar5.png
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics
;    
; :Keywords:
;    addcmd: in, optional, type=boolean, default=0
;       Set this keyword to add the command to the resizeable graphics window cgWindow.
;    annotatecolor: in, optional, type=string, default="opposite"
;       The name of the "annotation color" to use. The names are those for
;       cgCOLOR. If this keyword is used, the annotation color is loaded after
;       the color bar is displayed. This keyword is provided to maintain backward 
;       compatibility, but also to solve the potential problem of an extra line showing up
;       in the color bar when the COLOR keyword is used in indexed color mode. In other words,
;       use ANNOTATECOLOR in place of COLOR for complete color model independent results.
;    bottom: in, optional, type=integer, default=0
;       The lowest color index of the colors to be loaded in the color bar.
;    brewer: in, optional, type=boolean, default=0
;         This keyword is used only if the `CTIndex` keyword is used to select a color table number.
;         Setting this keyword allows Brewer color tables to be used.
;    charpercent: in, optional, type=float, default=0.85                 
;       A value from 0.0 go 1.0 that is multiplied by the CHARSIZE to produce
;       the character size for the color bar. This value is only used if CHARSIZE is 
;       undefined. This keyword is primarily useful for using color bars in resizeable 
;       graphics windows (cgWindow).
;    charsize: in, optional, type=float
;       The character size of the color bar annotations. Default is cgDefCharsize()*charPercent.
;    clamp: in, optional, type=float
;        A two-element array in data units. The color bar is clamped to these
;        two values. This is mostly of interest if you are "window-leveling"
;        an image. The clamp is set to the "window" of the color bar.
;        Normally, when you are doing this, you would like the colors outside
;        the "window" to be set to a neutral color. Use the NEUTRALINDEX keyword
;        to set the netural color index in the color bar. (See the Examples section
;        for more information.)
;    color: in, optional, type=string
;        The name of the color to use for color bar annotations. Ignored unless passed 
;        the name of a cgColor color. The default value is to use the ANNOTATECOLOR.
;    ctindex: in, optional, type=integer
;         The index number of a color table. The `Brewer` and `Reverse` keywords will be checked
;         to see how to load the color table into the `Palette` keyword. This keyword will take
;         precidence over any colors that are loaded with the `Palette` keyword. 
;    discrete: in, optional, type=boolean, default=0
;         Set this keyword to configure certain properties of the color bar to make
;         discrete color blocks for the color bar. This works best if you are using
;         a handful of colors in the color bar (e.g, 8-16).
;    divisions: in, optional, type=integer
;         The number of divisions to divide the bar into. There will
;         be (divisions + 1) annotations. The default is 0 if using the
;         default color bar formatting, which allows the plot command to 
;         determine how many divisions to make. Otherwise, if you are specifying
;         some other format for the tick labels, the default number of divisions
;         is six.
;    fit: in, optional, type=boolean, default=0
;       If this keyword is set, the colorbar "fits" itself to the normalized
;       coordinates of the last graphics command executed. In other words, for
;       a horizontal color bar, postition[[0,2]] = !X.Window, and for a vertical
;       color bar, position[[1,3]] = !Y.Window. Other positions are adjusted
;       to put the colorbar "reasonably" close to the plot. The fit many not always
;       be accurate. If you are fitting to an image, be sure to set the SAVE keyword
;       on cgImage to establish a data coordinate system.
;    font: in, optional, type=integer, default=!P.Font
;       Sets the font of the annotation. Hershey: -1, Hardware:0, True-Type: 1.
;    format: in, optional, type=string, default=""
;       The format of the color bar annotations. Default is "". Note that the
;       formatting behaviour can change, depending up values for the keywords
;       `RANGE` and `DIVISIONS`. If you prefer to let the IDL Plot command determine
;       how the color bar labels are formatted, set the format to a null string and
;       set the `DIVISIONS` keyword to 0. Note the difference in these two commands::
;       
;           cgColorbar, Range=[18,125], Position=[0.1, 0.8, 0.9, 0.85]
;           cgColorbar, Range=[18,125], Position=[0.1, 0.7, 0.9, 0.75], Divisions=0
;           
;    invertcolors: in, optional, type=boolean, default=0
;       Setting this keyword inverts the colors in the color bar.
;    maxrange: in, optional
;       The maximum data value for the color bar annotation. Default is NCOLORS.
;    minrange: in, optional, type=float, default=0.0
;       The minimum data value for the bar annotation. 
;    minor: in, optional, type=integer, default=2
;       The number of minor tick divisions. 
;    ncolors: in, optional, type=integer, default=256
;       This is the number of colors in the color bar.
;    neutralindex: in, optional, type=integer   
;       This is the color index to use for color bar values outside the
;       clamping range when clamping the color bar with the CLAMP keyword.
;       If this keyword is absent, the highest color table value is used
;       for low range values and the lowest color table value is used
;       for high range values, in order to provide contrast with the
;       clamped region. (See the Examples section for more information.)
;    nodisplay: in, optional
;       This keyword is obsolete and is no longer used.
;    oob_factor: in, optional, type=float, default=1.0
;       The default is to make the length of the out-of-bounds triangle the
;       same distance as the height (or width, in the case of a vertical
;       color bar) of the color bar. If you would prefer a shorted triangle length, 
;       set this keyword to a value less than zero (e.g., 0.5). If you prefer a 
;       longer length, set this keyword to a value greater than zero. The "standard"
;       length will be multiplied by this value.
;    oob_high: in, optional, type=string
;       The name of an out-of-bounds high color. This color will be represented
;       by a triangle on the right or top of the color bar. If the color is
;       a string byte value (e.g., "215"), then this color in the current color
;       table is used. The color can also be a three-element color triple 
;       (e.g., [240, 200, 65]). Note, you can CANNOT use a long integer as
;       a color table index number with this keyword. If you want to use a 
;       color table index number, be sure the number is a short integer, byte
;       value, or a string (e.g, OOB_HIGH=200S, OOB_HIGH=200B, or OOB_HIGH='200').
;    oob_low: in, optional, type=string
;       The name of an out-of-bounds low color. This color will be represented
;       by a triangle on the left or bottom of the color bar. If the color is
;       a string byte value (e.g., "215"), then this color in the current color
;       table is used. The color can also be a three-element color triple 
;       (e.g., [240, 200, 65]). Note, you can CANNOT use a long integer as
;       a color table index number with this keyword. If you want to use a 
;       color table index number, be sure the number is a short integer, byte
;       value, or a string (e.g, OOB_HIGH=200S, OOB_HIGH=200B, or OOB_HIGH='200').
;    palette: in, optional, type=byte
;       A color palette containing the RGB color vectors to use for the color
;       bar. The program will sample NCOLORS from the color palette. 
;    position: in, optional, type=float          
;       A four-element array of normalized coordinates in the same
;       form as the POSITION keyword on a plot. Default is[0.88, 0.10, 0.95, 0.90] 
;       for a vertical bar and [0.10, 0.88, 0.90, 0.95] for a horizontal bar.
;       See the FIT keyword, also.
;    range: in, optional, type=float
;       A two-element vector of the form [min, max]. Provides an alternative 
;       and faster way way of setting the MINRANGE and MAXRANGE keywords.
;    reverse: in, optional, type=boolean, default=0
;       An alternative keyword name (one I can actually remember!) for the INVERTCOLORS keyword.
;       It reverses the colors in the color bar.
;    right: in, optional, type=boolean, default=0   
;       This puts the labels on the right-hand side of a vertical color bar. It applies 
;       only to vertical color bars.
;    tickinterval: in, optional, type=float
;       Set this keyword to the interval spacing of major tick marks. Use this keyword in
;       place of XTickInterval or YTickInterval keywords.
;    ticklen: in, optional, type=float, default=0.25
;       Set this keyword to the major tick length desired. Default is 0.25. Setting this 
;       keyword to a value greater than or equal to 0.5 will result in major tick marks 
;       extending the width of the color bar. Note that setting this keyword to 0.3 or
;       greater will result in minor tick mark lengths being set to 0.01, which is almost 
;       too small to be seen. All direct graphics tick marks act in this (strange!) way.
;    ticknames: in, optional, type=string                 
;       A string array of names or values for the color bar tick marks.
;    title: in, optional, type=string, default=""
;       This is title for the color bar. The default is to have no title.
;    tcharsize: in, optional, type=float
;       The title size. By default, the same as `Charsize`. Note that this keyword is
;       ignored for vertical color bars unless the title location (`TLocation`) is on
;       the opposite side of the color bar from the color bar labels. This is a consequence
;       of being upable to determine the length of color bar labels programmatically in this
;       orientation.
;    tlocation: in, optional, type=string
;       The title location, which allows the user to set the title location independently 
;       of the colorbar labels. May be "TOP" or "BOTTOM" for horizontal color bars, and
;       "LEFT" or "RIGHT" for vertical color bars.
;    top: in, optional, type=boolean, default=0
;       This puts the labels on top of the bar rather than under it. The keyword only 
;       applies if a horizontal color bar is rendered.
;    vertical: in, optional, type=boolean, default=0
;       Setting this keyword give a vertical color bar. The default is a horizontal color bar.
;    window: in, optional, type=boolean, default=0               
;       Set this keyword to display the plot in a resizeable graphics window (cgWindow).
;    xlog: in, optional, type=boolean, default=0
;       Set this keyword to use logarithmic scaling for the colorbar data range.
;    xtickinterval: in, optional, type=float
;       This keyword is trapped, but unused. Please use the`TickInterval` keyword instead.
;    xtitle: in, optional, type=string
;        This keyword is ignored. Use the `Title` keyword to set a title for the color bar.
;    ylog: in, optional, type=boolean, default=0
;       Set this keyword to use logarithmic scaling for the colorbar data range.
;    ytickinterval: in, optional, type=float
;       This keyword is trapped, but unused. Please use the`TickInterval` keyword instead.
;    ytitle: in, optional, type=string
;        This keyword is ignored. Use the `Title` keyword to set a title for the color bar.
;    _ref_extra: in, optional
;         Any keyword appropriate for the PLOT and AXIS commands is also accepted by keyword
;         inheritance.
;    
; :Examples:
;    To display a horizontal color bar above a contour plot, type::
;
;       cgLOADCT, 5, NCOLORS=100
;       cgCONTOUR, DIST(31,41), POSITION=[0.15, 0.15, 0.95, 0.75], $
;          C_COLORS=INDGEN(25)*4, NLEVELS=25
;       cgCOLORBAR, NCOLORS=100, POSITION=[0.15, 0.85, 0.95, 0.90]
;       
;    Example using the `Clamp` and `NeutralIndex` keywords::
;       
;       cgLOADCT, 33, NCOLORS=254
;       TVLCT, cgCOLOR('gray', /TRIPLE), 255
;       cgCOLORBAR, NCOLORS=254, NEUTRALINDEX=255, RANGE=[0,1500], $
;           DIVISIONS=8, CLAMP=[400, 800]
;           
;    Additional examples can be found in the article `Adding a Color Bar <http://www.idlcoyote.com/color_tips/colorbar.html>`.
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
;       Written by: David W. Fanning, 4 February 2011, as a direct descendant of cgColorbar.
;       Program developement stopped on cgColorbar as of this date, and this program has
;       become a part of the Coyote Graphics System.
;       Added FIT keyword. 28 Feb 2011. DWF
;       Made default character size cgDefCharsize*0.85. 28 Feb 2011. DWF.
;       Modified error handler to restore the entry decomposition state if there is an error. 17 March 2011. DWF
;       Added CHARPERCENT keyword 18 March 2011. DWF.
;       Added XTITLE and YTITLE keywords, which do nothing except prevent these keywords
;          from being used inadvertently. 27 May 2011. DWF.
;       Fixed a problem with assigning the color with the ANNOTATECOLOR keyword in the
;          Z-graphics buffer. 30 Aug 2011. DWF.
;       Changed the default DIVISIONS to 0 and the default FORMAT to "". 2 Sept 2011. DWF.
;       Added code that will force MINRANGE and MAXRANGE values to be scalars. 5 Sept 2011. DWF.
;       Problem with division by zero when FORMAT is not default value. Now, if format
;          is the default value, then default is DIVISIONS=0, else DIVISIONS=6.
;       Documented the TICKLEN keyword and set the default tick length to 0.25. 3 Oct 2011. DWF.
;       Added the OOB_FACTOR, OOB_HIGH and OOB_LOW keywords. 5 Dec 2011. DWF.
;       Added DISCRETE keyword. 7 Dec 2011. DWF.
;       Changed the way the top axis was drawn, and had a problem with EXTRA keywords. Fixed. 20 Dec 2011. DWF.
;       Modified to use cgDefaultColor for default color selection. 24 Dec 2011. DWF.
;       Fixed a problem with color palettes by defining NCOLORS according to the number of colors
;          in the palette. 19 March 2012. DWF.
;       Set the maximum number of divisions at 59 to recognize the IDL plot limit for tick marks. 19 March 2012. DWF.
;       Modifications to the FIT algorithm to make sure the color bar is completely inside
;           the graphics window. Also fixed mis-spelled variable name. 20 March 2012. DWF.
;       Added TickInterval, XTickInterval and YTickInterval keywords to accommodate interval 
;           spacing of major tick marks. 21 July 2012. DWF.
;       Added the ability to use escape characters in plot titles to specify cgSymbol symbols. 27 July 2012. DWF.
;       Added TLOCATION and TCHARSIZE keywords. 20 September 2012. DWF.
;       Implemented a fix that will allow the user to specify a tick formatting function name 
;          with the FORMAT keyword. 21 September 2012. DWF.
;       Fixed a problem in which setting the RANGE keyword gave different results, depending upon whether
;          a FORMAT keyword was used or not. This change will affect the default color bar labeling
;          *if* the user specifies a range. If you prefer the old labeling behavior, simiply set
;          the `Divisions` keyword to 0. 16 Oct 2012. DWF.
;       Added CTINDEX, and BREWER keywords to make loading a color table palette easier. 20 October 2012. DWF.
;       Fixed a strange interaction between TickInterval and the Format keywords. 5 Nov 2012. DWF.
;       
; :Copyright:
;     Copyright (c) 2008-2012, Fanning Software Consulting, Inc.
;-
PRO cgColorbar, $
    ADDCMD=addcmd, $
    ANNOTATECOLOR=annotatecolor, $
    BOTTOM=bottom, $
    BREWER=brewer, $
    CHARPERCENT=charpercent, $
    CHARSIZE=charsize, $
    CLAMP=clamp, $
    COLOR=color, $
    CTINDEX=ctindex, $
    DISCRETE=discrete, $
    DIVISIONS=divisions, $
    FIT=fit, $
    FONT=font, $
    FORMAT=format, $
    INVERTCOLORS=invertcolors, $
    MAXRANGE=maxrange, $
    MINOR=minor, $
    MINRANGE=minrange, $
    NCOLORS=ncolors, $
    NEUTRALINDEX=neutralIndex, $
    NODISPLAY=nodisplay, $
    OOB_FACTOR=oob_factor, $
    OOB_HIGH=oob_high, $
    OOB_LOW=oob_low, $
    PALETTE=palette, $
    POSITION=position, $
    RANGE=range, $
    REVERSE=reverse, $
    RIGHT=right, $
    TLOCATION=tlocation, $
    TCHARSIZE=tcharsize, $
    TICKINTERVAL=tickinterval, $
    TICKLEN=ticklen, $
    TICKNAMES=ticknames, $
    TITLE=title, $
    TOP=top, $
    VERTICAL=vertical, $
    XLOG=xlog, $
    XTICKINTERVAL=xtickinterval, $
    XTITLE=xtitle, $ ; Ignored.
    YLOG=ylog, $
    YTICKINTERVAL=ytickinterval, $
    YTITLE=ytitle, $ ; Ignored
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
    IF (Keyword_Set(window) OR Keyword_Set(addcmd)) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        IF Keyword_Set(addcmd) THEN window = 0
        void = cgQuery(COUNT=wincnt)
        IF wincnt EQ 0 THEN cgWindow
        cgWindow, 'cgColorbar', $
            ANNOTATECOLOR=annotatecolor, $
            BOTTOM=bottom, $
            BREWER=brewer, $
            CHARPERCENT=charpercent, $
            CHARSIZE=charsize, $
            CLAMP=clamp, $
            COLOR=color, $
            CTINDEX=ctindex, $
            DISCRETE=discrete, $
            DIVISIONS=divisions, $
            FIT=fit, $
            FONT=font, $
            FORMAT=format, $
            INVERTCOLORS=invertcolors, $
            MAXRANGE=maxrange, $
            MINOR=minor, $
            MINRANGE=minrange, $
            NCOLORS=ncolors, $
            NEUTRALINDEX=neutralIndex, $
            NODISPLAY=nodisplay, $
            OOB_FACTOR=oob_factor, $
            OOB_HIGH=oob_high, $
            OOB_LOW=oob_low, $
            PALETTE=palette, $
            POSITION=position, $
            RANGE=range, $
            REVERSE=reverse, $
            RIGHT=right, $
            TLOCATION=tlocation, $
            TCHARSIZE=tcharsize, $
            TICKINTERVAL=tickinterval, $
            TICKLEN=ticklen, $
            TICKNAMES=ticknames, $
            TITLE=title, $
            TOP=top, $
            VERTICAL=vertical, $
            XLOG=xlog, $
            XTICKINTERVAL=xtickinterval, $
            XTITLE=xtitle, $ ; Ignored.
            YLOG=ylog, $
            YTICKINTERVAL=ytickinterval, $
            YTITLE=ytitle, $
            REPLACECMD=Keyword_Set(window), $
            ADDCMD=Keyword_Set(addcmd), $
             _EXTRA=extra

            
         RETURN
    ENDIF
    
    ; Get the current color table vectors. 
    TVLCT, r, g, b, /GET
    
    ; Default values.
    IF (N_Elements(charPercent) EQ 0) $
        THEN charPercent = 0.85 $
        ELSE charPercent = 0.0 > charPercent < 1.0
        
    ; Did you specify a color table index?
    IF N_Elements(ctindex) NE 0 THEN BEGIN
        cgLoadCT, ctindex, Brewer=brewer, RGB_TABLE=palette
    ENDIF

    ; If you have a palette, load the colors now. Otherwise whatever colors
    ; are in the current color table will be used.
    IF N_Elements(palette) NE 0 THEN BEGIN
        IF Size(palette, /N_DIMENSIONS) NE 2 THEN Message, 'Color palette is not a 3xN array.'
        dims = Size(palette, /DIMENSIONS)
        threeIndex = Where(dims EQ 3)
        IF ((threeIndex)[0] LT 0) THEN Message, 'Color palette is not a 3xN array.'
        IF threeIndex[0] EQ 0 THEN BEGIN
            palette = Transpose(palette)
            npalColors = dims[1]
        ENDIF ELSE npalColors = dims[0]
        IF N_Elements(ncolors) EQ 0 THEN ncolors = npalColors
;        IF ncolors NE npalColors THEN $
;           Message, 'The number of colors in the color palette does not match NCOLORS.'
        TVLCT, palette
        TVLCT, rr, gg, bb, /Get
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Save the current plot state.
    bang_p = !P
    bang_x = !X
    bang_Y = !Y
    bang_Z = !Z
    bang_Map = !Map

    ; Are scalable pixels available on the device?
    IF (!D.Flags AND 1) NE 0 THEN scalablePixels = 1 ELSE scalablePixels = 0

    ; Which release of IDL is this?
    thisRelease = Float(!Version.Release)

    ; Check and define keywords.
    SetDefaultValue, ncolors, 256
    SetDefaultValue, bottom, 0B
    SetDefaultValue, charsize, cgDefCharsize() * charPercent
    SetDefaultValue, tcharsize, charsize
    SetDefaultValue, format, ""
    IF N_Elements(nodisplay) EQ 0 THEN nodisplay = 1
    minrange = (N_ELEMENTS(minrange) EQ 0) ? 0. : Float(minrange[0])
    maxrange = (N_ELEMENTS(maxrange) EQ 0) ? Float(ncolors) : Float(maxrange[0])
    SetDefaultValue, ticklen, 0.25
    SetDefaultValue, minor, 2
    IF N_ELEMENTS(range) NE 0 THEN BEGIN
       minrange = Float(range[0])
       maxrange = Float(range[1])
    ENDIF
    SetDefaultValue, font, !P.Font
    SetDefaultValue, title, ""
    IF N_Elements(title) NE "" THEN title = cgCheckForSymbols(title)
    SetDefaultValue, oob_factor, 1.0
    xlog = Keyword_Set(xlog)
    ylog = Keyword_Set(ylog)
    
    ; Deal with tick intervals, if you have them.
    IF Keyword_Set(vertical) THEN BEGIN
       IF (N_Elements(tlocation) EQ 0) THEN tlocation = Keyword_Set(right) ? 'RIGHT' : 'LEFT'
       IF (N_Elements(yTickInterval) NE 0) && (ylog EQ 0) THEN BEGIN
           IF N_Elements(tickInterval) EQ 0 THEN tickInterval = yTickInterval
       ENDIF
    ENDIF ELSE BEGIN
       IF (N_Elements(tlocation) EQ 0) THEN tlocation = Keyword_Set(top) ? 'TOP' : 'BOTTOM'
       IF (N_Elements(xTickInterval) NE 0) && (xlog EQ 0) THEN BEGIN
           IF N_Elements(tickInterval) EQ 0 THEN tickInterval = xTickInterval    
       ENDIF
    ENDELSE
    
    ; Now handle DIVISIONS properly.
    IF N_Elements(divisions) EQ 0 THEN BEGIN
       IF (format EQ "") THEN BEGIN
           IF N_Elements(range) NE 0 THEN BEGIN
              IF N_Elements(tickInterval) NE 0 $
                  THEN divisions = Abs(maxrange - minrange) / tickInterval $
                  ELSE divisions = 6
           ENDIF ELSE divisions = 0 
       ENDIF ELSE BEGIN
           ; You can't have both DIVISONS and a tick interval at the same time,
           ; so the following value will be disgarded soon if you have a tick interval
           ; defined.
           IF N_Elements(tickInterval) NE 0 $
               THEN divisions = Abs(maxrange - minrange) / tickInterval $
               ELSE divisions = 6
       ENDELSE
    ENDIF
    divisions = divisions < 59 ; Limit to the PLOT command.
    
    ; If needed create a window first, so the drawing
    ; colors are correct for the window you want to draw into.
    IF ((!D.Flags AND 256) NE 0) && (!D.Window LT 0) THEN cgDisplay
    
    ; If the user asked for discrete colors, set some keywwords appropriately.
    ; This really should not be used for more than 16 or colors, but I don't
    ; want to limit it for the user. The maximum value is 59.
    IF Keyword_Set(discrete) THEN BEGIN
       divisions = ncolors < 59
       ticklen = 1.0
       minor = 0
    ENDIF

    ; You can't have a format set *and* use ticknames.
    IF N_ELEMENTS(ticknames) NE 0 THEN format = ""
    
    ; If the format is NOT null and is is the name of a tick formating function, then we have
    ; to handle things differently.
    IF (format NE "") && (StrPos(format, '(') EQ -1) THEN BEGIN
    
       IF minrange LT maxrange THEN BEGIN
           step = (maxrange - minrange) / divisions
           levels = minrange > (Indgen(divisions+1) * step + minrange) < maxrange
           nlevels = N_Elements(levels)
           ticknames = StrArr(nlevels)
           FOR j=0,nlevels-1 DO BEGIN
               ticknames[j] = Call_Function(format, 0, j, levels[j])
           ENDFOR
           format = "" ; No formats allowed in PLOT call now that we have ticknames.
        ENDIF ELSE BEGIN
           step = (minrange - maxrange) / divisions
           levels = maxrange > (Indgen(divisions+1) * step + maxrange) < minrange
           levels = Reverse(levels)
           nlevels = N_Elements(levels)
           ticknames = StrArr(nlevels)
           FOR j=0,nlevels-1 DO BEGIN
               ticknames[j] = Call_Function(format, 0, j, levels[j])
           ENDFOR
           format = "" ; No formats allowed in PLOT call now that we have ticknames.
       ENDELSE
    
    ENDIF ELSE BEGIN

        ; If the format is NOT null, then format the ticknames yourself.
        ; Can't assume minrange is less than maxrange.
        IF (xlog XOR ylog) EQ 0 THEN BEGIN
            IF format NE "" THEN BEGIN
               IF minrange LT maxrange THEN BEGIN
                   IF N_Elements(tickinterval) EQ 0 THEN BEGIN
                      step = (maxrange - minrange) / divisions
                      levels = minrange > (Indgen(divisions+1) * step + minrange) < maxrange
                      IF StrPos(StrLowCase(format), 'i') NE -1 THEN levels = Round(levels)
                      ticknames = String(levels, Format=format)
                      format = "" ; No formats allowed in PLOT call now that we have ticknames.
                   ENDIF
               ENDIF ELSE BEGIN
                   IF N_Elements(tickinterval) EQ 0 THEN BEGIN
                       step = (minrange - maxrange) / divisions
                       levels = maxrange > (Indgen(divisions+1) * step + maxrange) < minrange
                       levels = Reverse(levels)
                       IF StrPos(StrLowCase(format), 'i') NE -1 THEN levels = Round(levels)
                       ticknames = String(levels, Format=format)
                       format = "" ; No formats allowed in PLOT call now that we have ticknames.
                   ENDIF
               ENDELSE
            ENDIF
        ENDIF
        
    ENDELSE
    
    IF KEYWORD_SET(vertical) THEN BEGIN
       bar = REPLICATE(1B,20) # BINDGEN(ncolors)
       IF Keyword_Set(invertcolors) THEN bar = Reverse(bar, 2)
       IF N_ELEMENTS(position) EQ 0 THEN BEGIN
          IF Keyword_Set(right) THEN BEGIN
             position = [0.83, 0.1, 0.90, 0.9]
          ENDIF ELSE BEGIN
             position = [0.88, 0.1, 0.95, 0.9]
          ENDELSE
       ENDIF ELSE BEGIN
          IF position[2]-position[0] GT position[3]-position[1] THEN BEGIN
             position = [position[1], position[0], position[3], position[2]]
          ENDIF
          IF position[0] GE position[2] THEN Message, "Position coordinates can't be reconciled."
          IF position[1] GE position[3] THEN Message, "Position coordinates can't be reconciled."
       ENDELSE
       IF Keyword_Set(fit) THEN BEGIN
            position[[1,3]] = !Y.Window
            distance = position[2] - position[0]
            IF Keyword_Set(right) THEN BEGIN
                position[0] = !X.Window[1] + ((4*!D.X_CH_SIZE*charsize) / !D.X_Size)
            ENDIF ELSE BEGIN
                position[0] = !X.Window[1] + ((10*!D.X_CH_SIZE*charsize) / !D.X_Size)
            ENDELSE
            position[2] = position[0] + distance
            IF position[2] GT 1.0 THEN BEGIN
              diff = position[2]-1.0
              position[2] = 1.0 - 0.015
              IF (position[2] - position[0]) LT 0.015 THEN position[0] = (position[2] < position[0])-0.015
            ENDIF
       ENDIF
    ENDIF ELSE BEGIN
       bar = BINDGEN(ncolors) # REPLICATE(1B, 20)
       IF Keyword_Set(invertcolors) THEN bar = Reverse(bar, 1)
       IF N_ELEMENTS(position) EQ 0 THEN BEGIN
          IF Keyword_Set(top) THEN BEGIN
             position = [0.1, 0.82, 0.9, 0.90]
          ENDIF ELSE BEGIN
             position = [0.1, 0.88, 0.9, 0.95]
          ENDELSE
       ENDIF ELSE BEGIN
          IF position[3]-position[1] GT position[2]-position[0] THEN BEGIN
             position = [position[1], position[0], position[3], position[2]]
          ENDIF
          IF position[0] GE position[2] THEN Message, "Position coordinates can't be reconciled."
          IF position[1] GE position[3] THEN Message, "Position coordinates can't be reconciled."
       ENDELSE
       IF Keyword_Set(fit) THEN BEGIN
            position[[0,2]] = !X.Window
            distance = (position[3] - position[1])
            position[1] = !Y.Window[1] + ((4*!D.Y_CH_SIZE*charsize) / !D.Y_Size)
            position[3] = position[1] + distance
            IF position[3] GT 1.0 THEN BEGIN
              diff = position[3]-1.0
              position[3] = 1.0 - 0.015
              IF (position[3] - position[1]) LT 0.015 THEN position[1] = (position[3] < position[1])-0.015
            ENDIF
       ENDIF
     ENDELSE
     
     ; Adjust the positions if you have OOB colors.
     IF (N_Elements(oob_high) NE 0) || (N_Elements(oob_low) NE 0) THEN BEGIN
         IF Keyword_Set(vertical) THEN BEGIN
            length = (position[2]-position[0]) * oob_factor
            IF (N_Elements(oob_high) NE 0) THEN position[3] = position[3] - length
            IF (N_Elements(oob_low) NE 0) THEN position[1] = position[1] + length
         ENDIF ELSE BEGIN
            length = (position[3]-position[1]) * oob_factor
            IF (N_Elements(oob_high) NE 0) THEN position[2] = position[2] - length
            IF (N_Elements(oob_low) NE 0) THEN position[0] = position[0] + length
         ENDELSE
     ENDIF

     ; Scale the color bar.
     IF N_Elements(clamp) NE 0 THEN BEGIN
        IF N_Elements(clamp) NE 2 THEN Message, 'The CLAMP keyword must be a two-element array.'
        byterange = BytScl(clamp, minrange, maxrange)
        tempbar = BYTSCL(bar, TOP=(ncolors-1) < (255-bottom)) + bottom   
        bar = BYTSCL(bar, TOP=(ncolors-1) < (255-bottom), MIN=byterange[0], MAX=byterange[1]) + bottom 
        IF N_Elements(neutralIndex) EQ 0 THEN BEGIN
            neutralBottom = (ncolors-1) < (255-bottom)
            neutralTop = bottom
        ENDIF ELSE BEGIN
            neutralBottom = neutralIndex
            neutralTop = neutralIndex
        ENDELSE
        i = Where(tempbar LT byterange[0], count)
        IF count GT 0 THEN bar[i] = neutralBottom
        i = Where(tempbar GT byterange[1], count)
        IF count GT 0 THEN bar[i] = neutralTop
        
         
     ENDIF ELSE BEGIN
        bar = BYTSCL(bar, TOP=(ncolors-1) < (255-bottom)) + bottom
     ENDELSE

     IF Keyword_Set(reverse) THEN BEGIN
       IF Keyword_Set(vertical) THEN bar = Reverse(bar,2) ELSE bar = Reverse(bar,1)
     ENDIF

    ; Get starting locations in NORMAL coordinates.
    xstart = position[0]
    ystart = position[1]

    ; Get the size of the bar in NORMAL coordinates.
    xsize = (position[2] - position[0])
    ysize = (position[3] - position[1])

       
    ; Let's do this in decomposed color, if possible.
    SetDecomposedState, 1, CURRENTSTATE=currentState
       
    ; Display the color bar in the window. Sizing is
    ; different for PostScript and regular display.
    IF scalablePixels THEN BEGIN

       ; Display the color bar.
       SetDecomposedState, 0
       TV, bar, xstart, ystart, XSIZE=xsize, YSIZE=ysize, /Normal
       SetDecomposedState, 1 

    ENDIF ELSE BEGIN

       bar = CONGRID(bar, CEIL(xsize*!D.X_VSize), CEIL(ysize*!D.Y_VSize))

       ; Display the color bar.
       SetDecomposedState, 0
       TV, bar, xstart, ystart, /Normal
       SetDecomposedState, 1

    ENDELSE
   
    ; Get the current colortable.
    TVLCT, rr, gg, bb, /GET
    
    ; You can't specify both DIVISIONS and a tick interval, so fix that here.
    IF N_Elements(tickInterval) NE 0 THEN divisions = 0
        
    ; Annotate the color bar.
    annotateColor = cgDefaultColor(annotateColor, DEFAULT=color)
    
    ; If color is undefined, use the annotate color.
    color = cgDefaultColor(color, DEFAULT=annotateColor)
    
    IF Size(annotateColor, /TNAME) EQ 'STRING' THEN annotateColor = cgColor(annotateColor)
    IF Size(color, /TNAME) EQ 'STRING' THEN color = cgColor(color)
    
    IF KEYWORD_SET(vertical) THEN BEGIN

       IF KEYWORD_SET(right) THEN BEGIN

          IF StrUpCase(tlocation) EQ 'LEFT' THEN BEGIN
              PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=1, $
                 YTICKS=divisions, XSTYLE=1, YSTYLE=9, XTITLE="", YTITLE="", $
                 POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
                 XTICKFORMAT='(A1)', YTICKFORMAT='(A1)', YMINOR=minor, _STRICT_EXTRA=extra, $
                 YTICKNAME=ticknames, FONT=font, YLOG=ylog, YTICKINTERVAL=tickinterval
    
              AXIS, YAXIS=1, YRANGE=[minrange, maxrange], YTICKFORMAT=format, YTICKS=divisions, $
                 YTICKLEN=ticklen, YSTYLE=1, COLOR=color, CHARSIZE=charsize, XTITLE="", $
                 FONT=font, YTITLE="", _STRICT_EXTRA=extra, YMINOR=minor, YTICKNAME=ticknames, $
                 YLOG=ylog, YTICKINTERVAL=tickinterval, YTICK_GET=ticks
                 
              truecharsize = Float(!D.X_CH_SIZE * tcharsize) / !D.X_SIZE
              yloc = (position[3] - position[1]) / 2.0 + position[1]
              xloc = position[0] - (1.5 * truecharsize)
              XYOUTS, xloc, yloc, title, /NORMAL, COLOR=color, $
                ALIGNMENT=0.5, FONT=font, CHARSIZE=tcharsize, ORIENTATION=-270
          ENDIF ELSE BEGIN
              PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=1, $
                 YTICKS=divisions, XSTYLE=1, YSTYLE=9, XTITLE="", YTITLE="", $
                 POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
                 XTICKFORMAT='(A1)', YTICKFORMAT='(A1)', YMINOR=minor, _STRICT_EXTRA=extra, $
                 YTICKNAME=ticknames, FONT=font, YLOG=ylog, YTICKINTERVAL=tickinterval
    
              AXIS, YAXIS=1, YRANGE=[minrange, maxrange], YTICKFORMAT=format, YTICKS=divisions, $
                 YTICKLEN=ticklen, YSTYLE=1, COLOR=color, CHARSIZE=charsize, XTITLE="", $
                 FONT=font, YTITLE=title, _STRICT_EXTRA=extra, YMINOR=minor, YTICKNAME=ticknames, $
                 YLOG=ylog, YTICKINTERVAL=tickinterval, YTICK_GET=ticks
          ENDELSE

       ENDIF ELSE BEGIN

          IF StrUpCase(tlocation) EQ 'RIGHT' THEN BEGIN
              PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=1,  $
                 YTICKS=divisions, YSTYLE=9, XSTYLE=1, YTITLE="", $
                 POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
                 XTICKFORMAT='(A1)', YTICKFORMAT=format, YMinor=minor, _STRICT_EXTRA=extra, $
                 YTICKNAME=ticknames, YLOG=ylog, YTICKLEN=ticklen, FONT=font, XTITLE="", $
                 YTICKINTERVAL=tickinterval
    
              AXIS, YAXIS=1, YRANGE=[minrange, maxrange], YTICKFORMAT='(A1)', YTICKS=divisions, $
                 YTICKLEN=0.001, YSTYLE=1, COLOR=color, CHARSIZE=charsize, XTITLE="", $
                 FONT=font, YTITLE="", _STRICT_EXTRA=extra, YMINOR=minor, YTICKNAME="", YLOG=ylog, $
                 YTICKINTERVAL=tickinterval
                 
              truecharsize = Float(!D.X_CH_SIZE * tcharsize) / !D.X_SIZE
              yloc = (position[3] - position[1]) / 2.0 + position[1]
              xloc = position[2] + (2.0 * truecharsize)
              XYOUTS, xloc, yloc, title, /NORMAL, COLOR=color, $
                ALIGNMENT=0.5, FONT=font, CHARSIZE=tcharsize, ORIENTATION=-270
          ENDIF ELSE BEGIN
              PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=1,  $
                 YTICKS=divisions, YSTYLE=9, XSTYLE=1, YTITLE=title, $
                 POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
                 XTICKFORMAT='(A1)', YTICKFORMAT=format, YMinor=minor, _STRICT_EXTRA=extra, $
                 YTICKNAME=ticknames, YLOG=ylog, YTICKLEN=ticklen, FONT=font, XTITLE="", $
                 YTICKINTERVAL=tickinterval
    
              AXIS, YAXIS=1, YRANGE=[minrange, maxrange], YTICKFORMAT='(A1)', YTICKS=divisions, $
                 YTICKLEN=0.001, YSTYLE=1, COLOR=color, CHARSIZE=charsize, XTITLE="", $
                 FONT=font, YTITLE="", _STRICT_EXTRA=extra, YMINOR=minor, YTICKNAME="", YLOG=ylog, $
                 YTICKINTERVAL=tickinterval
          ENDELSE

       ENDELSE

    ENDIF ELSE BEGIN

       IF KEYWORD_SET(top) THEN BEGIN

          PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=divisions, $
             YTICKS=1, XSTYLE=9, YSTYLE=1, $
             POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
             YTICKFORMAT='(A1)', XTICKFORMAT='(A1)', XTICKLEN=0.01, $
             XRANGE=[minrange, maxrange], FONT=font, XMINOR=minor, _STRICT_EXTRA=extra, $
             XTICKNAME=ticknames, XLOG=xlog, XTITLE="", YTITLE="", XTICKINTERVAL=tickInterval

          AXIS, XTICKS=divisions, XSTYLE=1, COLOR=color, CHARSIZE=charsize, $
             XTICKFORMAT=format, XTICKLEN=ticklen, XRANGE=[minrange, maxrange], XAXIS=1, $
             FONT=font, XTITLE="", _STRICT_EXTRA=extra, XMINOR=minor, $
             XTICKNAME=ticknames, XLOG=xlog, YTITLE="", XTICKINTERVAL=tickInterval
             
          IF title NE "" THEN BEGIN
             xloc = (position[2] - position[0]) / 2.0 + position[0]
             CASE StrUpCase(tlocation) OF
                'TOP': BEGIN
                     truecharsize = Float(!D.Y_CH_SIZE * charsize) / !D.Y_SIZE
                     yloc = position[3] + (2.25 * truecharsize)
                     END
                'BOTTOM': BEGIN
                      truecharsize = Float(!D.Y_CH_SIZE * tcharsize) / !D.Y_SIZE
                      yloc = position[1] - (1.5 * truecharsize)
                      END
                'RIGHT': Message, 'Illegal specification for title position: ' + StrUpCase(tlocation)
                'LEFT': Message, 'Illegal specification for title position: ' + StrUpCase(tlocation)
                ELSE: Message, 'Illegal specification for title position: ' + StrUpCase(tlocation)
             ENDCASE
             XYOUTS, xloc, yloc, title, /NORMAL, COLOR=color, $
                ALIGNMENT=0.5, FONT=font, CHARSIZE=tcharsize
          ENDIF

       ENDIF ELSE BEGIN

          PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=divisions, $
             YTICKS=1, XSTYLE=9, YSTYLE=1, TITLE="", $
             POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
             YTICKFORMAT='(A1)', XTICKFORMAT=format, XTICKLEN=ticklen, $
             XRANGE=[minrange, maxrange], FONT=font, XMinor=minor, _STRICT_EXTRA=extra, $
             XTICKNAME=ticknames, XLOG=xlog, XTITLE="", YTITLE="", XTICKINTERVAL=tickInterval

          AXIS, XTICKS=divisions, XSTYLE=1, COLOR=color, CHARSIZE=charsize, $
             XTICKFORMAT='(A1)', XTICKLEN=0.001, XRANGE=[minrange, maxrange], XAXIS=1, $
             FONT=font, XTITLE="", XCHARSIZE=charsize, XMINOR=minor, $
             XTICKNAME="", XLOG=xlog, YTITLE="", XTICKINTERVAL=tickInterval

          IF title NE "" THEN BEGIN
             xloc = (position[2] - position[0]) / 2.0 + position[0]
             CASE StrUpCase(tlocation) OF
                'TOP': BEGIN
                     truecharsize = Float(!D.Y_CH_SIZE * charsize) / !D.Y_SIZE
                     yloc = position[3] + (0.75 * truecharsize)
                     END
                'BOTTOM': BEGIN
                      truecharsize = Float(!D.Y_CH_SIZE * charsize) / !D.Y_SIZE
                      yloc = position[1] - (2.00 * truecharsize) - $
                           (Float(!D.Y_CH_SIZE * tcharsize) / !D.Y_SIZE)
                      END
                'RIGHT': Message, 'Illegal specification for title position: ' + StrUpCase(tlocation)
                'LEFT': Message, 'Illegal specification for title position: ' + StrUpCase(tlocation)
               ELSE: Message, 'Illegal specification for title position: ' + StrUpCase(tlocation)
             ENDCASE
             XYOUTS, xloc, yloc, title, /NORMAL, COLOR=color, $
                ALIGNMENT=0.5, FONT=font, CHARSIZE=tcharsize
          ENDIF

        ENDELSE

    ENDELSE

    ; If you have OOB colors, draw them now.
    IF (N_Elements(oob_high) NE 0) || (N_Elements(oob_low) NE 0) THEN BEGIN
         p = position
         IF Keyword_Set(vertical) THEN BEGIN
            IF (N_Elements(oob_high) NE 0) THEN BEGIN
               phalf = (p[2]-p[0])/2.0 + p[0]
               pdist = (p[2]-p[0]) * oob_factor
               PolyFill, [p[0], phalf, p[2], p[0]], $
                         [p[3], pdist+p[3], p[3], p[3]], /Normal, Color=cgColor(oob_high)
               PlotS, [p[0], phalf, p[2], p[0]], $
                      [p[3], pdist+p[3], p[3], p[3]], /Normal, Color=color
            ENDIF
            IF (N_Elements(oob_low) NE 0) THEN BEGIN
               phalf = (p[2]-p[0])/2.0 + p[0]
               pdist = (p[2]-p[0]) * oob_factor
               PolyFill, [p[0], phalf, p[2], p[0]], $
                         [p[1], p[1]-pdist, p[1], p[1]], /Normal, Color=cgColor(oob_low)
               PlotS, [p[0], phalf, p[2], p[0]], $
                      [p[1], p[1]-pdist, p[1], p[1]], /Normal, Color=color            
            END
         ENDIF ELSE BEGIN
            IF (N_Elements(oob_high) NE 0) THEN BEGIN
               phalf = (p[3]-p[1])/2.0 + p[1]
               pdist = (p[3]-p[1]) * oob_factor
               PolyFill, [p[2], p[2]+pdist, p[2], p[2]], $
                         [p[1], phalf, p[3], p[1]], /Normal, Color=cgColor(oob_high)
               PlotS, [p[2], p[2]+pdist, p[2], p[2]], $
                      [p[1], +phalf, p[3], p[1]], /Normal, Color=color            
            ENDIF
            IF (N_Elements(oob_low) NE 0) THEN BEGIN
               phalf = (p[3]-p[1])/2.0 + p[1]
               pdist = (p[3]-p[1]) * oob_factor
               PolyFill, [p[0], p[0]-pdist, p[0], p[0]], $
                         [p[1], phalf, p[3], p[1]], /Normal, Color=cgColor(oob_low)
               PlotS, [p[0], p[0]-pdist, p[0], p[0]], $
                      [p[1], +phalf, p[3], p[1]], /Normal, Color=color            
            END
         ENDELSE
    ENDIF

    ; Restore the color state.
    SetDecomposedState, currentState

    ; Restore the previous plot and map system variables.
    !P = bang_p
    !X = bang_x
    !Y = bang_y
    !Z = bang_z
    !Map = bang_map
    
    ; Set the current colors back.
    IF !D.Name NE 'Z' THEN TVLCT, r, g, b
END
