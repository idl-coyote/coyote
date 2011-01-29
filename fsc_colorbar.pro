;+
; NAME:
;   FSC_COLORBAR
;   
;      Note: The name of the routine has been changed from COLORBAR
;      on 25 Sept 2010 to avoid conflicts with an IDL 8.0 routine of the
;      same name. See the article "IDL 8 Name Conflicts" here:
;       
;           http://www.dfanning.com/ng_tips/idl8_name_conflicts.html
;
; PURPOSE:
;
;       The purpose of this routine is to add a color bar to the current
;       graphics window.
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;       Graphics, Widgets.
;
; CALLING SEQUENCE:
;
;       FSC_COLORBAR
;
; INPUTS:
;
;       None.
;
; KEYWORD PARAMETERS:
;
;       ADDCMD:       Set this keyword to add the FSC_Colorbar command to the current FSC_Window
;                     command list. 
;               
;       ANNOTATECOLOR: The name of the "annotation color" to use. The names are those for
;                     FSC_COLOR, and using the keyword implies that FSC_COLOR is also in
;                     your !PATH. If this keyword is used, the annotation color is loaded
;                     *after* the color bar is displayed. The color will be represented
;                     as theColor = FSC_COLOR(ANNOTATECOLOR). This keyword is provide
;                     to maintain backward compatibility, but also to solve the problem of
;                     an extra line in the color bar when this kind of syntax is used in
;                     conjunction with the indexed (DEVICE, DECOMPOSED=0) model is used:
;
;                          LoadCT, 33
;                          TVImage, image
;                          FSC_Colorbar, Color=FSC_Color('firebrick')
;
;                     The proper syntax for device-independent color is like this:
;
;                          LoadCT, 33
;                          TVImage, image
;                          FSC_Colorbar, AnnotateColor='firebrick', Color=255
;                          
;                    Set the Modification History note for 13 November 2010 for additional
;                    information about default values.
;
;       BOTTOM:       The lowest color index of the colors to be loaded in
;                     the bar.
;
;       CHARSIZE:     The character size of the color bar annotations. Default is !P.Charsize.
;       
;       CLAMP:        A two-element array in data units. The color bar is clamped to these
;                     two values. This is mostly of interest if you are "window-leveling"
;                     an image. The clamp is set to the "window" of the color bar.
;                     Normally, when you are doing this, you would like the colors outside
;                     the "window" to be set to a neutral color. Use the NEUTRALINDEX keyword
;                     to set the netural color index in the color bar. (See the Example section
;                     for more information.
;
;       COLOR:        The color index of the bar outline and characters. Default
;                     is !P.Color. To display the color bar in a device indpendent
;                     way, you should use the ANNOTATECOLOR keyword instead of this keyword.
;                     If this keyword is a string color name, then ANNOTATECOLOR=color.
;
;       DIVISIONS:    The number of divisions to divide the bar into. There will
;                     be (divisions + 1) annotations. The default is 6.
;
;       FONT:         Sets the font of the annotation. Hershey: -1, Hardware:0, True-Type: 1.
;
;       FORMAT:       The format of the bar annotations. Default is '(I0)'.
;
;       INVERTCOLORS: Setting this keyword inverts the colors in the color bar.
;
;       MAXRANGE:     The maximum data value for the bar annotation. Default is
;                     NCOLORS.
;
;       MINRANGE:     The minimum data value for the bar annotation. Default is 0.
;
;       MINOR:        The number of minor tick divisions. Default is 2.
;
;       NCOLORS:      This is the number of colors in the color bar.
;       
;       NEUTRALINDEX: This is the color index to use for color bar values outside the
;                     clamping range when clamping the color bar with the CLAMP keyword.
;                     If this keyword is absent, the highest color table value is used
;                     for low range values and the lowest color table value is used
;                     for high range values, in order to provide contrast with the
;                     clamped region. See the Example section for more information.
;
;       NODISPLAY:    FSC_COLORBAR uses FSC_COLOR to specify some of it colors. Normally, 
;                     FSC_COLOR loads "system" colors as part of its palette of colors.
;                     In order to do so, it has to create an IDL widget, which in turn 
;                     has to make a connection to the windowing system. If your program 
;                     is being run without a window connection, then this program will 
;                     fail. If you can live without the system colors (and most people 
;                     don't even know they are there, to tell you the truth), then setting 
;                     this keyword will keep them from being loaded, and you can run
;                     FSC_COLORBAR without a display. As of 19 Oct 2010, set to 1  by default.
;
;       POSITION:     A four-element array of normalized coordinates in the same
;                     form as the POSITION keyword on a plot. Default is
;                     [0.88, 0.10, 0.95, 0.90] for a vertical bar and
;                     [0.10, 0.88, 0.90, 0.95] for a horizontal bar.
;
;       RANGE:        A two-element vector of the form [min, max]. Provides an
;                     alternative way of setting the MINRANGE and MAXRANGE keywords.
;
;       REVERSE:      Setting this keyword reverses the colors in the colorbar.
;
;       RIGHT:        This puts the labels on the right-hand side of a vertical
;                     color bar. It applies only to vertical color bars.
;
;       TICKNAMES:    A string array of names or values for the tick marks.
;
;       TITLE:        This is title for the color bar. The default is to have
;                     no title.
;
;       TOP:          This puts the labels on top of the bar rather than under it.
;                     The keyword only applies if a horizontal color bar is rendered.
;
;       VERTICAL:     Setting this keyword give a vertical color bar. The default
;                     is a horizontal color bar.
;                     
;       WINDOW:       Set this keyword if you want to add the FSC_Colorbar command to
;                     the current FSC_Window application.
;
; COMMON BLOCKS:
;
;       None.
;
; SIDE EFFECTS:
;
;       Color bar is drawn in the current graphics window.
;
; RESTRICTIONS:
;
;       The number of colors available on the graphics display device (not the
;       PostScript device) is used unless the NCOLORS keyword is used.
;
;       Requires the FSC_COLOR program from the Coyote Library:
;
;          http://www.dfanning.com/programs/fsc_color.pro
;
; EXAMPLE:
;
;       To display a horizontal color bar above a contour plot, type:
;
;       LOADCT, 5, NCOLORS=100
;       CONTOUR, DIST(31,41), POSITION=[0.15, 0.15, 0.95, 0.75], $
;          C_COLORS=INDGEN(25)*4, NLEVELS=25
;       FSC_COLORBAR, NCOLORS=100, POSITION=[0.15, 0.85, 0.95, 0.90]
;       
;       Example using the CLAMP and NEUTRALINDEX keywords.
;       
;       LOADCT, 33, NCOLORS=254
;       TVLCT, FSC_COLOR('gray', /TRIPLE), 255
;       FSC_COLORBAR, NCOLORS=254, NEUTRALINDEX=255, RANGE=[0,1500], $
;           DIVISIONS=8, CLAMP=[400, 800]
;
; MODIFICATION HISTORY:
;
;       Written by: David W. Fanning, 10 JUNE 96.
;       10/27/96: Added the ability to send output to PostScript. DWF
;       11/4/96: Substantially rewritten to go to screen or PostScript
;           file without having to know much about the PostScript device
;           or even what the current graphics device is. DWF
;       1/27/97: Added the RIGHT and TOP keywords. Also modified the
;            way the TITLE keyword works. DWF
;       7/15/97: Fixed a problem some machines have with plots that have
;            no valid data range in them. DWF
;       12/5/98: Fixed a problem in how the colorbar image is created that
;            seemed to tickle a bug in some versions of IDL. DWF.
;       1/12/99: Fixed a problem caused by RSI fixing a bug in IDL 5.2. Sigh... DWF.
;       3/30/99: Modified a few of the defaults. DWF.
;       3/30/99: Used NORMAL rather than DEVICE coords for positioning bar. DWF.
;       3/30/99: Added the RANGE keyword. DWF.
;       3/30/99: Added FONT keyword. DWF
;       5/6/99: Many modifications to defaults. DWF.
;       5/6/99: Removed PSCOLOR keyword. DWF.
;       5/6/99: Improved error handling on position coordinates. DWF.
;       5/6/99. Added MINOR keyword. DWF.
;       5/6/99: Set Device, Decomposed=0 if necessary. DWF.
;       2/9/99: Fixed a problem caused by setting BOTTOM keyword, but not NCOLORS. DWF.
;       8/17/99. Fixed a problem with ambiguous MIN and MINOR keywords. DWF
;       8/25/99. I think I *finally* got the BOTTOM/NCOLORS thing sorted out. :-( DWF.
;       10/10/99. Modified the program so that current plot and map coordinates are
;            saved and restored after the colorbar is drawn. DWF.
;       3/18/00. Moved a block of code to prevent a problem with color decomposition. DWF.
;       4/28/00. Made !P.Font default value for FONT keyword. DWF.
;       9/26/00. Made the code more general for scalable pixel devices. DWF.
;       1/16/01. Added INVERTCOLORS keyword. DWF.
;       5/11/04. Added TICKNAME keyword. DWF.
;       9/29/05. Added REVERSE keywords, which does the *exact* same thing as
;           INVERTCOLORS, but I can never remember the latter keyword name. DWF.
;       1/2/07. Added ANNOTATECOLOR keyword. DWF.
;       4/14/07. Changed the default FORMAT to I0. DWF.
;       5/1/07. Unexpected consequence of default format change is colorbar annotations
;           no longer match contour plot levels. Changed to explicit formating of
;           colorbar axis labels before PLOT command. DWF.
;       5/25/07. Previous change has unanticipated effect on color bars using
;           logarithmic scaling, which is not really supported, but I have an
;           article on my web page describing how to do it: http://www.dfanning.com/graphics_tips/logcb.html.
;           Thus, I've fixed the program to accommodate log scaling, while still not OFFICIALLY
;           supporting it. DWF.
;       10/3/07. Method used to calculate TICKNAMES produces incorrect values in certain cases when
;           the min and max range values are integers. Now force range values to be floats. DWF.
;       10/17/07. Accidentaly use of INTERP keyword in CONGRID results in wrong bar values for
;           low NCOLORS numbers when INVERTCOLORS or REVERSE keyword is used. Removed INTERP keyword. DWF.
;       11/10/07. Finished fixing program to accommodate log scaling in ALL possible permutations. DWF.
;       8 Feb 2008. Added CRONJOB keyword and decided to use month names when I write the date. DWF.
;       8 Feb 2008. Renamed CRONJOB to NODISPLAY to better reflect its purpose. DWF.
;      21 May 2008. Changed the default CHARSIZE to !P.CHARSIZE from 1.0. DWF.
;      30 Oct 2008. Fixed a problem with the FONT keyword not being recognized in certain
;            configurations.
;      9 Nov 2009. Fixed typo in title of vertical colorbar. DWF.
;      25 Sep 2010. Renamed FSC_Colorbar from Colorbar to avoid conflict with ITTVIS introduced
;             colorbar.pro function in IDL 8.0. DWF.
;      19 Oct 2010. Made changes so that axes titles are not picked up in _EXTRA and displayed
;             on the color bar. Also changed all _EXTRA passes to _STRICT_EXTRA to report
;             mis-spelled and/or inappropriate keywords. DWF.
;      19 Oct 2010. Changed the default value of NODISPLAY keyword to 1, since FSC_Color
;             no longer looks for system colors anyway. NODISPLAY is a depreciated keyword
;             and does nothing if using the latest version of FSC_Color. DWF.
;      13 Nov 2010. Samples the upper-right hand pixel on the display if it can. It this pixel is 
;             "white" or the current device is PostScript, sets the ANNOTATECOLOR keyword
;             to "black" if it of the COLOR keyword is not currently set to some other color. 
;             If the pixel is "black" then ANNOTATECOLOR is set to "white". DWF.
;      19 Nov 2010. Fixed a small problem when choosing an AnnnotateColor. DWF.
;      29 Nov 2010. Added CLAMP and NEUTRALINDEX keywords, updated ability to set
;             color model with SetDecomposedState command. DWF.
;      1 Dec 2010. Set COLOR=1 and BITS_PER_PIXEL=8 in PostScript device. Tired of getting
;             e-mails that "your damn colorbar doesn't work!". DWF.
;      6 Dec 2010. I was always setting COLOR to ANNOTATECOLOR. I should only do that if
;             COLOR is undefined. DWF.
;      24 Jan 2011. Added WINDOW keyword. DWF.
;      29 Jan 2011. Added ADDCMD keyword. DWF.
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
PRO FSC_COLORBAR, $
    ADDCMD=addcmd, $
    ANNOTATECOLOR=annotatecolor, $
    BOTTOM=bottom, $
    CHARSIZE=charsize, $
    CLAMP=clamp, $
    COLOR=color, $
    DIVISIONS=divisions, $
    FONT=font, $
    FORMAT=format, $
    INVERTCOLORS=invertcolors, $
    MAXRANGE=maxrange, $
    MINOR=minor, $
    MINRANGE=minrange, $
    NCOLORS=ncolors, $
    NEUTRALINDEX=neutralIndex, $
    NODISPLAY=nodisplay, $
    POSITION=position, $
    RANGE=range, $
    REVERSE=reverse, $
    RIGHT=right, $
    TICKLEN=ticklen, $
    TICKNAMES=ticknames, $
    TITLE=title, $
    TOP=top, $
    VERTICAL=vertical, $
    XLOG=xlog, $
    YLOG=ylog, $
    WINDOW=window, $
    _EXTRA=extra

    Compile_Opt idl2

    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    ; Should this be added to a resizeable graphics window?
    IF (Keyword_Set(window) OR Keyword_Set(addcmd)) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        IF Keyword_Set(addcmd) THEN window = 0
        void = FSC_QueryWin(COUNT=wincnt)
        IF wincnt EQ 0 THEN FSC_Window
        FSC_Window, 'FSC_Colorbar', $
            ANNOTATECOLOR=annotatecolor, $
            BOTTOM=bottom, $
            CHARSIZE=charsize, $
            CLAMP=clamp, $
            COLOR=color, $
            DIVISIONS=divisions, $
            FONT=font, $
            FORMAT=format, $
            INVERTCOLORS=invertcolors, $
            MAXRANGE=maxrange, $
            MINOR=minor, $
            MINRANGE=minrange, $
            NCOLORS=ncolors, $
            NEUTRALINDEX=neutralIndex, $
            NODISPLAY=nodisplay, $
            POSITION=position, $
            RANGE=range, $
            REVERSE=reverse, $
            RIGHT=right, $
            TICKLEN=ticklen, $
            TICKNAMES=ticknames, $
            TITLE=title, $
            TOP=top, $
            VERTICAL=vertical, $
            XLOG=xlog, $
            YLOG=ylog, $
            REPLACECMD=Keyword_Set(window), $
            ADDCMD=Keyword_Set(addcmd), $
             _EXTRA=extra

            
         RETURN
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
    IF N_ELEMENTS(ncolors) EQ 0 THEN ncolors = 256
    IF N_ELEMENTS(bottom) EQ 0 THEN bottom = 0B
    IF N_ELEMENTS(charsize) EQ 0 THEN charsize = !P.Charsize
    IF N_ELEMENTS(format) EQ 0 THEN format = '(I0)'
    IF N_Elements(nodisplay) EQ 0 THEN nodisplay = 1
    minrange = (N_ELEMENTS(minrange) EQ 0) ? 0. : Float(minrange)
    maxrange = (N_ELEMENTS(maxrange) EQ 0) ? Float(ncolors) : Float(maxrange)
    IF N_ELEMENTS(ticklen) EQ 0 THEN ticklen = 0.2
    IF N_ELEMENTS(minor) EQ 0 THEN minor = 2
    IF N_ELEMENTS(range) NE 0 THEN BEGIN
       minrange = Float(range[0])
       maxrange = Float(range[1])
    ENDIF
    IF N_ELEMENTS(divisions) EQ 0 THEN divisions = 6
    IF N_ELEMENTS(font) EQ 0 THEN font = !P.Font
    IF N_ELEMENTS(title) EQ 0 THEN title = ''
    xlog = Keyword_Set(xlog)
    ylog = Keyword_Set(ylog)

    ; You can't have a format set *and* use ticknames.
    IF N_ELEMENTS(ticknames) NE 0 THEN format = ""

    ; If the format is NOT null, then format the ticknames yourself.
    ; Can't assume minrange is less than maxrange.
    IF (xlog XOR ylog) EQ 0 THEN BEGIN
        IF format NE "" THEN BEGIN
           IF minrange LT maxrange THEN BEGIN
               step = (maxrange - minrange) / divisions
               levels = minrange > (Indgen(divisions+1) * step + minrange) < maxrange
               IF StrPos(StrLowCase(format), 'i') NE -1 THEN levels = Round(levels)
               ticknames = String(levels, Format=format)
               format = "" ; No formats allowed in PLOT call now that we have ticknames.
           ENDIF ELSE BEGIN
               step = (minrange - maxrange) / divisions
               levels = maxrange > (Indgen(divisions+1) * step + maxrange) < minrange
               levels = Reverse(levels)
               IF StrPos(StrLowCase(format), 'i') NE -1 THEN levels = Round(levels)
               ticknames = String(levels, Format=format)
               format = "" ; No formats allowed in PLOT call now that we have ticknames.
           ENDELSE
        ENDIF
    ENDIF

    IF KEYWORD_SET(vertical) THEN BEGIN
       bar = REPLICATE(1B,20) # BINDGEN(ncolors)
       IF Keyword_Set(invertcolors) THEN bar = Reverse(bar, 2)
       IF N_ELEMENTS(position) EQ 0 THEN BEGIN
          position = [0.88, 0.1, 0.95, 0.9]
       ENDIF ELSE BEGIN
          IF position[2]-position[0] GT position[3]-position[1] THEN BEGIN
             position = [position[1], position[0], position[3], position[2]]
          ENDIF
          IF position[0] GE position[2] THEN Message, "Position coordinates can't be reconciled."
          IF position[1] GE position[3] THEN Message, "Position coordinates can't be reconciled."
       ENDELSE
    ENDIF ELSE BEGIN
       bar = BINDGEN(ncolors) # REPLICATE(1B, 20)
       IF Keyword_Set(invertcolors) THEN bar = Reverse(bar, 1)
       IF N_ELEMENTS(position) EQ 0 THEN BEGIN
          position = [0.1, 0.88, 0.9, 0.95]
       ENDIF ELSE BEGIN
          IF position[3]-position[1] GT position[2]-position[0] THEN BEGIN
             position = [position[1], position[0], position[3], position[2]]
          ENDIF
          IF position[0] GE position[2] THEN Message, "Position coordinates can't be reconciled."
          IF position[1] GE position[3] THEN Message, "Position coordinates can't be reconciled."
       ENDELSE
     ENDELSE

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

       
    ; Decomposed color off if device supports it.
    SetDecomposedState, 0, CURRENTSTATE=currentState
       
    ; Display the color bar in the window. Sizing is
    ; different for PostScript and regular display.
    IF scalablePixels THEN BEGIN

       ; Display the color bar.
       TV, bar, xstart, ystart, XSIZE=xsize, YSIZE=ysize, /Normal

    ENDIF ELSE BEGIN

       bar = CONGRID(bar, CEIL(xsize*!D.X_VSize), CEIL(ysize*!D.Y_VSize))

       ; Display the color bar.
       TV, bar, xstart, ystart, /Normal

   ENDELSE
   
   ; Restore the decomposed state if needed.
   IF currentState THEN SetDecomposedState, 1

    ; Get the current colortable.
    TVLCT, rr, gg, bb, /GET
    
    ; Annotate the color bar.
    IF (!D.Name EQ 'PS') AND N_Elements(annotateColor) EQ 0 THEN BEGIN
        annotateColor = 'black'
    ENDIF ELSE BEGIN
        IF N_Elements(annotateColor) EQ 0 THEN BEGIN
            IF (!D.Window GE 0) AND ~scalablePixels THEN BEGIN
                pixel = TVRead(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                IF N_ELEMENTS(color) EQ 0 THEN BEGIN
                    IF Total(pixel) EQ 765 THEN annotateColor = 'black'
                    IF Total(pixel) EQ 0 THEN annotateColor = 'white'
                    IF N_Elements(annotateColor) EQ 0 THEN annotateColor = 'opposite'
                ENDIF ELSE BEGIN
                     IF Size(color, /TNAME) EQ 'STRING' THEN annotateColor = color
                ENDELSE
            ENDIF ELSE annotateColor = 'opposite'
        ENDIF 
    ENDELSE
    IF N_Elements(annotateColor) EQ 0 THEN annotateColor = 'opposite'
    
    ; If color is undefined, use the annotate color.
    IF N_Elements(color) EQ 0 THEN BEGIN
        IF Size(annotateColor, /TNAME) EQ 'STRING' THEN BEGIN
            color = FSC_Color(annotateColor)
        ENDIF ELSE BEGIN
            color = annotateColor
        ENDELSE
    ENDIF 
    
    IF KEYWORD_SET(vertical) THEN BEGIN

       IF KEYWORD_SET(right) THEN BEGIN

          PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=1, $
             YTICKS=divisions, XSTYLE=1, YSTYLE=9, XTITLE="", YTITLE="", $
             POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
             XTICKFORMAT='(A1)', YTICKFORMAT='(A1)', YMINOR=minor, _STRICT_EXTRA=extra, $
             YTICKNAME=ticknames, FONT=font, YLOG=ylog

          AXIS, YAXIS=1, YRANGE=[minrange, maxrange], YTICKFORMAT=format, YTICKS=divisions, $
             YTICKLEN=ticklen, YSTYLE=1, COLOR=color, CHARSIZE=charsize, XTITLE="", $
             FONT=font, YTITLE=title, _STRICT_EXTRA=extra, YMINOR=minor, YTICKNAME=ticknames, YLOG=ylog

       ENDIF ELSE BEGIN

          PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=1,  $
             YTICKS=divisions, YSTYLE=1, XSTYLE=1, YTITLE=title, $
             POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
             XTICKFORMAT='(A1)', YTICKFORMAT=format, YMinor=minor, _STRICT_EXTRA=extra, $
             YTICKNAME=ticknames, YLOG=ylog, YTICKLEN=ticklen, FONT=font, XTITLE=""

       ENDELSE

    ENDIF ELSE BEGIN

       IF KEYWORD_SET(top) THEN BEGIN

          PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=divisions, $
             YTICKS=1, XSTYLE=9, YSTYLE=1, $
             POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
             YTICKFORMAT='(A1)', XTICKFORMAT='(A1)', XTICKLEN=ticklen, $
             XRANGE=[minrange, maxrange], FONT=font, XMINOR=minor, _STRICT_EXTRA=extra, $
             XTICKNAME=ticknames, XLOG=xlog, XTITLE="", YTITLE=""

          AXIS, XTICKS=divisions, XSTYLE=1, COLOR=color, CHARSIZE=charsize, $
             XTICKFORMAT=format, XTICKLEN=ticklen, XRANGE=[minrange, maxrange], XAXIS=1, $
             FONT=font, XTITLE=title, _STRICT_EXTRA=extra, XCHARSIZE=charsize, XMINOR=minor, $
             XTICKNAME=ticknames, XLOG=xlog, YTITLE=""

       ENDIF ELSE BEGIN

          PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=divisions, $
             YTICKS=1, XSTYLE=1, YSTYLE=1, TITLE=title, $
             POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
             YTICKFORMAT='(A1)', XTICKFORMAT=format, XTICKLEN=ticklen, $
             XRANGE=[minrange, maxrange], FONT=font, XMinor=minor, _STRICT_EXTRA=extra, $
             XTICKNAME=ticknames, XLOG=xlog, XTITLE="", YTITLE=""

        ENDELSE

    ENDELSE

    ; Restore the previous plot and map system variables.
    !P = bang_p
    !X = bang_x
    !Y = bang_y
    !Z = bang_z
    !Map = bang_map
    
    ; Set the current colors back.
    IF !D.Name NE 'Z' THEN TVLCT, rr, gg, bb
END