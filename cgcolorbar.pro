;+
; NAME:
;   cgCOLORBAR
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
;       cgColorbar
;
; INPUTS:
;
;       None.
;
; KEYWORD PARAMETERS:
;
;       ADDCMD:       Set this keyword to add the FSC_Colorbar command to the current cgWindow
;                     command list. 
;               
;       ANNOTATECOLOR: The name of the "annotation color" to use. The names are those for
;                     cgCOLOR, and using the keyword implies that cgCOLOR is also in
;                     your !PATH. If this keyword is used, the annotation color is loaded
;                     *after* the color bar is displayed. The color will be represented
;                     as theColor = cgCOLOR(ANNOTATECOLOR). This keyword is provide
;                     to maintain backward compatibility, but also to solve the problem of
;                     an extra line in the color bar when this kind of syntax is used in
;                     conjunction with the indexed (DEVICE, DECOMPOSED=0) model is used:
;
;                          LoadCT, 33
;                          TVImage, image
;                          FSC_Colorbar, Color=cgColor('firebrick')
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
;       NODISPLAY:    FSC_COLORBAR uses cgCOLOR to specify some of it colors. Normally, 
;                     cgCOLOR loads "system" colors as part of its palette of colors.
;                     In order to do so, it has to create an IDL widget, which in turn 
;                     has to make a connection to the windowing system. If your program 
;                     is being run without a window connection, then this program will 
;                     fail. If you can live without the system colors (and most people 
;                     don't even know they are there, to tell you the truth), then setting 
;                     this keyword will keep them from being loaded, and you can run
;                     cgCOLORBAR without a display. As of 19 Oct 2010, set to 1  by default.
;
;       PALETTE:      A color palette containing the RGB color vectors to use for the color
;                     bar. The program will sample NCOLORS from the color palette. 
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
;                     the current cgWindow application.
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
;       Requires the cgCOLOR program from the Coyote Library:
;
;          http://www.dfanning.com/programs/cgcolor.pro
;
; EXAMPLE:
;
;       To display a horizontal color bar above a contour plot, type:
;
;       LOADCT, 5, NCOLORS=100
;       CONTOUR, DIST(31,41), POSITION=[0.15, 0.15, 0.95, 0.75], $
;          C_COLORS=INDGEN(25)*4, NLEVELS=25
;       cgCOLORBAR, NCOLORS=100, POSITION=[0.15, 0.85, 0.95, 0.90]
;       
;       Example using the CLAMP and NEUTRALINDEX keywords.
;       
;       LOADCT, 33, NCOLORS=254
;       TVLCT, cgCOLOR('gray', /TRIPLE), 255
;       cgCOLORBAR, NCOLORS=254, NEUTRALINDEX=255, RANGE=[0,1500], $
;           DIVISIONS=8, CLAMP=[400, 800]
;
; MODIFICATION HISTORY:
;
;       Written by: David W. Fanning, 4 February 2011, as a direct descendant of FSC_Colorbar.
;       Program developement stopped on FSC_Colorbar as of this date, and this program has
;       become a part of the Coyote Graphics System.
;
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
PRO cgColorbar, $
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
    PALETTE=palette, $
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
    _REF_EXTRA=extra
    
    FSC_COLORBAR, $
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
        PALETTE=palette, $
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
    
END