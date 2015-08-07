; docformat = 'rst'
;
; NAME:
;   cgPlot
;
; PURPOSE:
;   The purpose of cgPlot is to create a wrapper for the traditional IDL graphics
;   command, Plot. The primary purpose of this is to create plot commands that work
;   and look identically both on the display and in PostScript files.
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
; The purpose of cgPlot is to create a wrapper for the traditional IDL graphics
; command, Plot. The primary purpose of this is to create plot commands that work
; and look identically both on the display and in PostScript files.
; 
; Program default colors will depend on the IDL graphics window. If no windows are currently
; open when the program is called, cgDisplay is used to create a window.
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics
;    
; :Params:
;    x: in, required, type=any
;         If X is provided without Y, a vector representing the dependent values to be 
;         plotted If both X and Y are provided, X is the independent parameter and 
;         Y is the dependent parameter to be plotted.
;    y: in, optional, type=any
;         A vector representing the dependent values to be plotted.
;       
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        Set this keyword to add the command to the resizeable graphics window cgWindow.
;     aspect: in, optional, type=float, default=none
;        Set this keyword to a floating point ratio that represents the aspect ratio 
;        (ysize/xsize) of the resulting plot. The plot position may change as a result
;        of setting this keyword. Note that `Aspect` cannot be used when plotting with
;        !P.MULTI.
;     axiscolor: in, optional, type=string/integer, default='opposite'
;        If this keyword is a string, the name of the axis color. 
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     axescolor: in, optional, type=string/integer
;        Provisions for bad spellers.
;     background: in, optional, type=string/integer, default='background'
;        If this keyword is a string, the name of the background color. 
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     charsize: in, optional, type=float, default=cgDefCharSize()
;        The character size for axes annotations. Uses cgDefCharSize to select default
;        character size, unless !P.Charsize is set, in which case !P.Charsize is always used.
;     color: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the data color. By default, 'black'.
;        Color names are those used with cgColor. Otherwise, the keyword is assumed 
;        to be a color index into the current color table.
;        ERR_COLOR=serr_color, $
;     err_clip: in, optional, type=boolean, default=0
;         Set this keyword to cause error bars to be clipped to the borders of the plot.
;         The default is to draw error bars, even if outside the plot.
;     err_color: in, optional, type=varies
;         The color error bars should be drawn in. The default is to use the `Color` keyword.
;     err_thick:, in, optional, type=integer
;         The thickness of the line for drawing the error bars. By default, !P.Thick.
;     err_width: in, optional, type=float
;         The width of the end lines on error bars in normalized coordinates. By default, the
;         width is one percent of the width of the axes length in the appropriate dimension.
;     err_xhigh: in, optional
;         The high error values that should be added to the independent or X data values.
;     err_xlow: in, optional
;         The low error values that should be subtracted from the independent or X data values.
;     err_yhigh: in, optional
;         The high error values that should be added to the dependent or Y data values.
;     err_ylow: in, optional
;         The low error values that should be subtracted from the dependent or Y data values.
;     font: in, optional, type=integer, default=!P.Font
;        The type of font desired for axis annotation.
;     isotropic: in, optional, type=boolean, default=0
;        Maintain the same scale on both axes.
;     label: in, optional, type=string
;        A label is similar to a plot title, but it is aligned to the left edge
;        of the plot and is written in hardware fonts. Use of the label keyword
;        will suppress the plot title.
;     layout: in, optional, type=intarr(3)
;        This keyword specifies a grid with a graphics window and determines where the
;        graphic should appear. The syntax of LAYOUT is three numbers: [ncolumns, nrows, location].
;        The grid is determined by the number of columns (ncolumns) by the number of 
;        rows (nrows). The location of the graphic is determined by the third number. The
;        grid numbering starts in the upper left (1) and goes sequentually by column and then
;        by row.
;     legends: in, optional, type=object
;        One or more cgLegendItem objects that are to be drawn on the plot.
;     map_object: in, optional, type=object
;        If you are drawing on a map projection set up with Map_Proj_Init
;        and using projected meter space, rather than lat/lon space, then you can use this
;        keyword to provide a cgMap object that will allow you to convert the dependent and independent
;        parameters from longitude and latitude values, respectively, to projected meter space
;        before drawing. 
;     nodata: in, optional, type=boolean, default=0
;        Set this keyword to draw axes, but no data.
;     noerase: in, optional, type=boolean, default=0
;        Set this keyword to draw the plot without erasing the display first.
;     oplots: in, optional, type=object
;        A single cgOverPlot object, or an array of cgOverPlot objects that will be
;        overplot on the axes set up by the original data. The user will be responsible
;        for destroying the objects. The cgPlot program will simply draw the objects.
;     outfilename: in, optional, type=string
;        If the `Output` keyword is set, the user will be asked to supply an output
;        filename, unless this keyword is set to a non-null string. In that case, the
;        value of this keyword will be used as the filename and there will be no dialog
;        presented to the user.
;     output: in, optional, type=string, default=""
;        Set this keyword to the type of output desired. Possible values are these::
;            
;            'PS'   - PostScript file
;            'EPS'  - Encapsulated PostScript file
;            'PDF'  - PDF file
;            'BMP'  - BMP raster file
;            'GIF'  - GIF raster file
;            'JPEG' - JPEG raster file
;            'PNG'  - PNG raster file
;            'TIFF' - TIFF raster file
;            
;        Or, you can simply set this keyword to the name of the output file, and the type of
;        file desired will be determined by the file extension. If you use this option, the
;        user will not be prompted to supply the name of the output file.  
;            
;        All raster file output is created through PostScript intermediate files (the
;        PostScript files will be deleted), so ImageMagick and Ghostview MUST be installed 
;        to produce anything other than PostScript output. (See cgPS2PDF and cgPS_Close for 
;        details.) And also note that you should NOT use this keyword when doing multiple 
;        plots. The keyword is to be used as a convenient way to get PostScript or raster 
;        output for a single graphics command. Output parameters can be set with cgWindow_SetDefs.
;     overplot: in, optional, type=boolean, default=0
;        Set this keyword if you wish to overplot data on an already exisiting set of
;        axes. It is like calling the IDL OPLOT command.
;     position: in, optional, type=vector
;        The usual four-element position vector for the Plot comamnd. Only monitored and
;        possibly set if the `Aspect` keyword is used.
;     psym: in, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46. This may also be set to the
;        "name" of a symbol, such as returned from Print, cgSymCat(/Names).
;     symcolor: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the symbol color. By default, 'black'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     title: in, optional, type=string
;         The title of the plot.
;     traditional: in, optional, type=boolean, default=0
;        If this keyword is set, the traditional color scheme of a black background for
;        graphics windows on the display is used and PostScript files always use a white background.
;     window: in, optional, type=boolean, default=0
;        Set this keyword to replace all the commands in a current cgWindow or to
;        create a new cgWindow for displaying this command.
;     xmargin: in, optional
;         Set this keyword to a two-element array giving the left and right sides of the plot window
;         in units of character size. Use of this keyword is greatly discouraged and is included here
;         strictly to make code backward compatable. Use the `Position` keyword instead. This 
;         keyword value is only used for the default positioning of a single plot, if no other
;         method of specifying the plot position is used.
;     xrange: in, optional
;         Set this keyword to a two-element vector setting the X axis range for the plot.
;         If this keyword is used, and the `XStyle` keyword is NOT used, then XSTYLE is set to 1.
;     xstyle: in, optional, type=integer
;         This keyword is a bit map that allows a variety of axis options, depending upon which bit
;         is set. Bits are set by adding the following values together when setting the value of
;         the keyword::
;            Value    Description
;              0      Allow axis autoscaling.
;              1      Turn axis autoscaling off, force exact axis range.
;              2      Extend axis range.
;              4      Suppress entire axis.
;              8      Suppress box style axis. Draw only main axis.
;         To suppress box axis style and force exact axis range, for example, set the keyword to 8+1=9::
;             cgPlot, cgDemoData(1), XRange=[15,78], XStyle=9
;     xtitle: in, optional, type=string
;         The X title of the plot.
;     ymargin: in, optional
;         Set this keyword to a two-element array giving the bottom and top sides of the plot window
;         in units of character size. Use of this keyword is greatly discouraged and is included here
;         strictly to make code backward compatable. Use the `Position` keyword instead. This
;         keyword value is only used for the default positioning of a single plot, if no other
;         method of specifying the plot position is used.
;     yrange: in, optional
;         Set this keyword to a two-element vector setting the Y axis range for the plot.
;         If this keyword is used, and the `YStyle` keyword is NOT used, then YSTYLE is set to 1.
;     ystyle: in, optional, type=integer
;         This keyword is a bit map that allows a variety of axis options, depending upon which bit
;         is set. Bits are set by adding the following values together when setting the value of
;         the keyword::
;            Value    Description
;              0      Allow axis autoscaling.
;              1      Turn axis autoscaling off, force exact axis range.
;              2      Extend axis range.
;              4      Suppress entire axis.
;              8      Suppress box style axis. Draw only main axis.
;             16      Inhibt setting the Y axis minimum value to 0.
;         To suppress box axis style and force exact axis range, for example, set the keyword to 8+1=9::
;             cgPlot, cgDemoData(1), YRange=[15,28], YStyle=9
;     ytitle: in, optional, type=string
;         The Y title of the plot.
;     _ref_extra: in, optional, type=any
;        Any `IDL Plot keyword <http://www.exelisvis.com/docs/PLOT_Procedure.html>` 
;        not defined here is allowed in the program.
;
; :Examples:
;    Use as you would use the IDL PLOT command::
;       cgPlot, Findgen(11)
;       cgPlot, Findgen(11), Aspect=1.0
;       cgPlot, Findgen(11), Color='olive', AxisColor='red', Thick=2
;       cgPlot, Findgen(11), Color='blue', SymColor='red', PSym=-16
;       
;     Example using error bars::
;       data = Congrid(cgDemoData(1), 15)
;       seed = -5L
;       time = cgScaleVector(Findgen(N_Elements(data)), 1, 9)
;       high_yerror = RandomU(seed, N_Elements(data)) * 5 > 0.5
;       low_yerror = RandomU(seed, N_Elements(data)) * 4 > 0.25
;       high_xerror = RandomU(seed, N_Elements(data)) * 0.75 > 0.1
;       low_xerror = RandomU(seed, N_Elements(data))  * 0.75 > 0.1
;       xtitle = 'Time'
;       ytitle = 'Signal Strength'
;       title = 'Error Bar Plot'
;       position = [0.125, 0.125, 0.9, 0.925]
;       thick = (!D.Name EQ 'PS') ? 3 : 1
;       cgDisplay, 600, 500, Title='Errorbar Plot'
;       cgPlot, time, data, Color='red5', PSym=-16, $
;           SymSize=1.0, Thick=thick, Title=title, XTitle=xtitle, YTitle=ytitle, $
;           Position=position, YStyle=1, $
;           ERR_XLow=low_xerror, ERR_XHigh=high_xerror, ERR_CLIP=1, $
;           ERR_YLow=low_yerror, ERR_YHigh=high_yerror, ERR_Color='blu5'
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
;        Written, 12 November 2010. DWF.
;        Added SYMCOLOR keyword, and allow all 46 symbols from cgSYMCAT. 15 November 2010. DWF.
;        Added NODATA keyword. 15 November 2010. DWF.
;        Now setting decomposition state by calling cgSetColorState. 16 November 2010. DWF.
;        Final color table restoration skipped in Z-graphics buffer. 17 November 2010. DWF.
;        Fixed a problem with overplotting with symbols. 17 November 2010. DWF.
;        Background keyword now applies in PostScript file as well. 17 November 2010. DWF.
;        Many changes after BACKGROUND changes to get !P.MULTI working again! 18 November 2010. DWF.
;        Fixed a small problem with the OVERPLOT keyword. 18 Nov 2010. DWF.
;        Changes so that color inputs don't change type. 23 Nov 2010. DWF.
;        Added WINDOW keyword to allow graphic to be displayed in a resizable graphics window. 8 Dec 2010. DWF
;        Modifications to allow cgPlot to be drop-in replacement for old PLOT commands in 
;            indexed color mode. 24 Dec 2010. DWF.
;        Previous changes introduced problems with OVERPLOT that have now been fixed. 28 Dec 2010. DWF.
;        Set NOERASE keyword from !P.NoErase system variable when appropriate. 28 Dec 2010. DWF.
;        Additional problems with NOERASE discovered and solved. 29 Dec 2010. DWF.
;        In some cases, I was turning BYTE values to strings without converting to 
;            INTEGERS first. 30 Dec 2010. DWF.  
;         Selecting character size now with cgDefCharSize. 11 Jan 2011. DWF.   
;         Moved setting to decomposed color before color selection process to avoid PostScript
;             background problems when passed 24-bit color integers. 12 Jan 2011. DWF. 
;         Changed _EXTRA to _REF_EXTRA on procedure definition statement to be able to return
;             plot keywords such as XGET_TICKS. 13 Jan 2011. DWF.  
;         Added SYMSIZE keyword. 16 Jan 2011. DWF.
;         Fixed a problem in which I assumed the background color was a string. 18 Jan 2011. DWF.  
;         Added ADDCMD keyword. 26 Jan 2011. DWF.
;         Added LAYOUT keyword. 28 Jan 2011. DWF.
;         Made a modification that allows THICK and COLOR keywords apply to symbols, too. 24 Feb 2011. DWF.
;         Modified error handler to restore the entry decomposition state if there is an error. 17 March 2011. DWF
;         Somehow I had gotten independent and dependent data reversed in the code. Put right. 16 May 2011. DWF.
;         Allowed ASPECT (and /ISOTROPIC) to take into account input POSITION. 15 June 2011. Jeremy Bailin.
;         Updated the BACKGROUND color selection from lessons learned in 27 Oct 2011 cgContour 
;             corrections. 27 Oct 2011. DWF.
;         Added the ability to send the output directly to a file via the OUTPUT keyword. 9 Dec 2011, DWF.
;         PostScript, PDF, and Imagemagick parameters can now be tailored with cgWindow_SetDefs. 14 Dec 2011. DWF.
;         Modified to use cgDefaultColor for default color selection. 24 Dec 2011. DWF.
;         Over-zealous use of _STRICT_EXTRA when overplotting resulted in errors. Now use _EXTRA. 1 Jan 2012. DWF.
;         Changes to allow better default colors, based on changes to cgColor and cgDefaultColor. 1 Feb 2012. DWF.
;         Now allowing the user to draw in the "background" color, if the COLOR or AXISCOLOR is "BACKGROUND". 19 March 2012. DWF.
;         Scalar input parameters are changed to 1-element vectors to avoid annoying error messages from PLOT. 6 April 2012. DWF.
;         Added a LABEL keyword. 12 July 2012. DWF.
;         Yikes! Bad choice of variable names in LABEL work yesterday has severe consequences. Changed names. 13 July 2012. DWF.
;         Added OPLOTS keyword to allow cgOverplot objects. 18 July 2012. DWF.
;         Added the ability to specify a symbol name with the PSYM keyword. 19 Juyl 2012. DWF.
;         Added the ability to use escape characters in plot titles to specify cgSymbol symbols. 27 July 2012. DWF.
;         Fixed an interaction with the LABEL keyword that prevented a Title from appearing. 2 Oct 2012. DWF.
;         Modified the way default colors are selected when the background color is "white". 4 Dec 2012. DWF.
;         Still trying to accommodate users who incorrectly specify LONG integers while using INDEXED color. 26 Dec 2012. DWF.
;         Modified code that checks to see if COLOR and AXISCOLOR keywords are the same as BACKGROUND and changes them.
;              This precludes drawing in background color on non-white backgrounds. Now only change the
;              colors if it is possible to draw a background color. 12 Feb 2013. DWF.
;         Problem using symbol names (e.g., 'opencircle') in cgWindows is fixed. 10 May 2013. DWF.
;         Changed the meaning of ISOTROPIC to its true meaning of keeping the same scale on both axes. 21 June 2013. DWF.
;         Added XRANGE, XSTYLE, YRANGE, and YSTYLE keywords. This allows exact axis scaling if the XRANGE or YRANGE
;             keywords are used without setting the XSTYLE or YSTYLE keywords, which is more intuitive. 15 July 2013. DWF.
;         Added error bar plotting capability via ERR_* keywords. 10 December 2013. DWF.
;         Added Map_Object keyword to allow plotting values in longitude/latitude to be converted to XY projected meter
;             space automatically. 10 December 2013. DWF.
;         Data could sometimes be drawn when OVERPLOT and NODATA keywords were both set. Fixed. 16 Dec 2013. DWF.
;         Added ERR_CLIP keyword. 31 Jan 2014. DWF.
;         Added a sanity check for inappropriate use of the ISOTROPIC keyword. 6 Feb 2014. DWF.
;         Problem with using ISOTROPIC keyword without POSITION keyword fixed. 21 Jun 2014. DWF.
;         Fixed incorrect use of MARGIN keyword in cgAspect. 1 Sep 2014. DWF.
;         Fixed a problem with plotting with a map coordinate object. I had reversed independent and
;              dependent data vectors. 2 Sept 2014. DWF.
;         Modified the error bar plotting section of the code to accommodate zoomable plots (eg., cgZPlot). 
;              Error bars are drawn only on points inside the axes range. 30 Sep 2014. DWF.
;         Further modified error bar plotting to account for possible log axes on the plot. 24 Nov 2014. DWF.
;         Added XMargin and YMargin keywords against my better judgement. 15 December 2014. DWF.
;         Now taking default values for Title, XTitle, YTitle, XStyle, and YStyle from corresponding
;              system variables. 15 December 2014. DWF.
;         
; :Copyright:
;     Copyright (c) 2010-2014, Fanning Software Consulting, Inc.
;-
PRO cgPlot, x, y, $
    ADDCMD=addcmd, $
    ASPECT=aspect, $
    AXISCOLOR=saxiscolor, $
    AXESCOLOR=saxescolor, $
    BACKGROUND=sbackground, $
    CHARSIZE=charsize, $
    COLOR=scolor, $
    ERR_CLIP=err_clip, $
    ERR_COLOR=serr_color, $
    ERR_THICK=err_thick, $
    ERR_WIDTH=err_width, $
    ERR_XHIGH=err_xhigh, $
    ERR_XLOW=err_xlow, $
    ERR_YHIGH=err_yhigh, $
    ERR_YLOW=err_ylow, $
    FONT=font, $
    ISOTROPIC=isotropic, $
    LABEL=label, $
    LAYOUT=layout, $
    LEGENDS=legends, $
    MAP_OBJECT=map_object, $
    NODATA=nodata, $
    NOERASE=noerase, $
    OPLOTS=oplots, $
    OUTFILENAME=outfilename, $
    OUTPUT=output, $
    OVERPLOT=overplot, $
    POSITION=position, $
    PSYM=psymIn, $
    SYMCOLOR=ssymcolor, $
    SYMSIZE=symsize, $
    TITLE=title, $
    TRADITIONAL=traditional, $
    WINDOW=window, $
    XMARGIN=xmargin, $
    XRANGE=xrange, $
    XSTYLE=xstyle, $
    XTITLE=xtitle, $
    YMARGIN=ymargin, $
    YRANGE=yrange, $
    YSTYLE=ystyle, $
    YTITLE=ytitle, $
    _REF_EXTRA=extra
    
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        IF N_Elements(thisMulti) NE 0 THEN !P.Multi = thisMulti
        IF N_Elements(currentState) NE 0 THEN cgSetColorState, currentState
        IF (N_Elements(output) NE 0) THEN cgPS_Close, /NOFIX
        RETURN
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Check parameters.
    IF N_Params() EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: cgPlot, x, y'
        RETURN
    ENDIF
    
    ; Pay attention to !P.Noerase in setting the NOERASE kewyord. This must be
    ; done BEFORE checking the LAYOUT properties.
    IF !P.NoErase NE 0 THEN noerase = !P.NoErase ELSE noerase = Keyword_Set(noerase)
    
    ; Do they want this plot in a resizeable graphics window?
    IF Keyword_Set(addcmd) THEN window = 1
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        ; If you are using a layout, you can't ever erase.
        IF N_Elements(layout) NE 0 THEN noerase = 1
        
        ; Special treatment for overplotting or adding a command.
        IF Keyword_Set(overplot) OR Keyword_Set(addcmd) THEN BEGIN
            cgWindow, 'cgPlot', x, y, $
                ASPECT=aspect, $
                AXISCOLOR=saxiscolor, $
                AXESCOLOR=saxescolor, $
                BACKGROUND=sbackground, $
                CHARSIZE=charsize, $
                COLOR=scolor, $
                ERR_CLIP=err_clip, $
                ERR_COLOR=serr_color, $
                ERR_THICK=err_thick, $
                ERR_WIDTH=err_width, $
                ERR_XHIGH=err_xhigh, $
                ERR_XLOW=err_xlow, $
                ERR_YHIGH=err_yhigh, $
                ERR_YLOW=err_ylow, $
                FONT=font, $
                ISOTROPIC=isotropic, $
                LABEL=label, $
                LAYOUT=layout, $
                LEGENDS=legends, $
                MAP_OBJECT=map_object, $
                NODATA=nodata, $
                NOERASE=noerase, $
                OPLOTS=oplots, $
                OVERPLOT=overplot, $
                POSITION=position, $
                PSYM=psymIn, $
                SYMCOLOR=ssymcolor, $
                SYMSIZE=symsize, $
                TITLE=title, $
                TRADITIONAL=traditional, $
                XMARGIN=xmargin, $
                XRANGE=xrange, $
                XSTYLE=xstyle, $
                XTITLE=xtitle, $
                YMARGIN=ymargin, $
                YRANGE=yrange, $
                YSTYLE=ystyle, $
                YTITLE=ytitle, $
                ADDCMD=1, $
                _Extra=extra
             RETURN
       ENDIF
        
        ; Open a new window or replace the current commands, as required.
        currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
        IF wincnt EQ 0 THEN replaceCmd = 0 ELSE replaceCmd=1
        cgWindow, 'cgPlot', x, y, $
            ASPECT=aspect, $
            AXISCOLOR=saxiscolor, $
            AXESCOLOR=saxescolor, $
            BACKGROUND=sbackground, $
            CHARSIZE=charsize, $
            COLOR=scolor, $
            ERR_CLIP=err_clip, $
            ERR_COLOR=serr_color, $
            ERR_THICK=err_thick, $
            ERR_WIDTH=err_width, $
            ERR_XHIGH=err_xhigh, $
            ERR_XLOW=err_xlow, $
            ERR_YHIGH=err_yhigh, $
            ERR_YLOW=err_ylow, $
            FONT=font, $
            ISOTROPIC=isotropic, $
            LABEL=label, $
            LAYOUT=layout, $
            LEGENDS=legends, $
            MAP_OBJECT=map_object, $
            NODATA=nodata, $
            NOERASE=noerase, $
            OPLOTS=oplots, $
            OVERPLOT=overplot, $
            POSITION=position, $
            PSYM=psymIn, $
            SYMCOLOR=ssymcolor, $
            SYMSIZE=symsize, $
            TITLE=title, $
            TRADITIONAL=traditional, $
            XRANGE=xrange, $
            XSTYLE=xstyle, $
            XTITLE=xtitle, $
            YMARGIN=ymargin, $
            YRANGE=yrange, $
            YSTYLE=ystyle, $
            YTITLE=ytitle, $
            REPLACECMD=replaceCmd, $
            _Extra=extra
            
         RETURN
    ENDIF
    
    ; Sort out which is the dependent and which is independent data.
    CASE N_Params() OF
      
       1: BEGIN
       _dep = x
       _indep = Findgen(N_Elements(x))
       ENDCASE
    
       2: BEGIN
       _dep = y
       _indep = x
       ENDCASE
    
    ENDCASE
    
    ; If either of these input vectors are scalars, make them vectors.
    IF N_Elements(_dep) EQ 1 THEN _dep = [_dep]
    IF N_Elements(_indep) EQ 1 THEN _indep = [_indep]
    
    ; If you have a map coordinate object, do the conversion to XY projected meter space here.
    IF (N_Elements(map_object) NE 0) && Obj_Valid(map_object) THEN BEGIN
        
        xy = map_object -> Forward(_indep, _dep, /NoForwardFix)
        dep = Reform(xy[1,*])
        indep = Reform(xy[0,*])
        
        IF (N_Elements(err_ylow) NE 0) THEN BEGIN
            xy = map_object -> Forward(_indep, _dep-err_ylow, /NoForwardFix)
            err_ylow = Reform(xy[1,*]) - dep
        ENDIF
        
        IF (N_Elements(err_yhigh) NE 0) THEN BEGIN
            xy = map_object -> Forward(_indep, _dep+err_yhigh, /NoForwardFix)
            err_yhigh = Reform(xy[1,*]) - dep
        ENDIF

        IF (N_Elements(err_xlow) NE 0) THEN BEGIN
            xy = map_object -> Forward(_indep-err_xlow, _dep, /NoForwardFix)
            err_xlow = Reform(xy[0,*]) - indep
        ENDIF

        IF (N_Elements(err_xhigh) NE 0) THEN BEGIN
            xy = map_object -> Forward(_indep+err_xhigh, _dep, /NoForwardFix)
            err_xhigh = Reform(xy[0,*]) - indep
        ENDIF

    ENDIF ELSE BEGIN
        dep = _dep
        indep = _indep
    ENDELSE
    
    
    ; Check to see if psymIn is a string. If so, covert it here.
    IF N_Elements(psymIn) NE 0 THEN BEGIN
        IF Size(psymIn, /TNAME) EQ 'STRING' THEN BEGIN
            names = cgSymCat(/Names)
            index = Where(STRUPCASE(StrCompress(names, /REMOVE_ALL)) EQ STRUPCASE(StrCompress(psymIN, /REMOVE_ALL)), count)
            IF count GT 0 THEN psym = index[0] ELSE Message, 'Cannot resolve the PSYM value: ' + psymIn
        ENDIF ELSE psym = psymIn
    ENDIF
    
    ; Are we doing some kind of output?
    IF (N_Elements(output) NE 0) && (output NE "") THEN BEGIN
    
       ; If the output string has a dot character, then this must be a
       ; filename, and we will determine the type of file from the filename extension.
       IF StrPos(output, '.') NE -1 THEN BEGIN
             root_name = cgRootName(output, DIRECTORY=theDir, EXTENSION=ext)
             IF theDir EQ "" THEN CD, CURRENT=theDir
             outfilename = output
             outputSelection = StrUpCase(ext)
       ENDIF
    
       IF N_Elements(outputSelection) EQ 0 THEN outputSelection = StrUpCase(output)
       typeOfOutput = ['PS','EPS','PDF','BMP','GIF','JPEG','JPG','PNG','TIFF', 'TIF']
       void = Where(typeOfOutput EQ outputSelection, count)
       IF count EQ 0 THEN Message, 'Cannot find ' + outputSelection + ' in allowed output types.'
       
       ; Set things up.
       CASE outputSelection OF
          'PS': BEGIN
              ext = '.ps'
              delete_ps = 0
              END    
          'EPS': BEGIN
              ext = '.eps'
              encapsulated = 1
              delete_ps = 0
              END
          'PDF': BEGIN
              ext = '.pdf'
              pdf_flag = 1
              delete_ps = 1
              END     
          'BMP': BEGIN
              ext = '.bmp'
              bmp_flag = 1
              delete_ps = 1
              END      
          'GIF': BEGIN
              ext = '.gif'
              gif_flag = 1
              delete_ps = 1
              END
          'JPEG': BEGIN
              ext = '.jpg'
              jpeg_flag = 1
              delete_ps = 1
              END      
          'JPG': BEGIN
              ext = '.jpg'
              jpeg_flag = 1
              delete_ps = 1
              END
          'PNG': BEGIN
              ext = '.png'
              png_flag = 1
              delete_ps = 1
              END      
          'TIFF': BEGIN
              ext = '.tif'
              tiff_flag = 1
              delete_ps = 1
              END
          'TIF': BEGIN
              ext = '.tif'
              tiff_flag = 1
              delete_ps = 1
              END    
       ENDCASE
       
       ; Do you need a filename?
       IF ( (N_Elements(outfilename) EQ 0) || (outfilename EQ "") ) THEN BEGIN 
            filename = 'cgplot' + ext
            outfilename = cgPickfile(FILE=filename, TITLE='Select Output File Name...', $
                FILTER=ext, /WRITE)
            IF outfilename EQ "" THEN RETURN
       ENDIF
       
       ; We need to know the root name of the file, because we have to make a PostScript
       ; file of the same name. At least we do if the type is not PS or EPS.
       IF (outputSelection NE 'PS') && (outputSelection NE 'EPS') THEN BEGIN
           root_name = cgRootName(outfilename, DIRECTORY=theDir)
           IF theDir EQ "" THEN CD, CURRENT=theDir
           ps_filename = Filepath(ROOT_DIR=theDir, root_name + '.ps')
       ENDIF ELSE ps_filename = outfilename
       
       ; Get the output default values.
       cgWindow_GetDefs, $
         PS_Charsize = ps_charsize, $          ; The PostScript character size.
         PS_FONT = ps_font, $                  ; Select the font for PostScript output.
         PS_Decomposed = ps_decomposed, $      ; Sets the PostScript color mode.
         PS_Delete = ps_delete, $              ; Delete PS file when making IM raster.
         PS_Metric = ps_metric, $              ; Select metric measurements in PostScript output.
         PS_Scale_factor = ps_scale_factor, $  ; Select the scale factor for PostScript output.
         PS_TT_Font = ps_tt_font               ; Select the true-type font to use for PostScript output.   
       
       ; Set up the PostScript device.
       cgPS_Open, $
          CHARSIZE=ps_charsize, $
          DECOMPOSED=ps_decomposed, $
          FILENAME=ps_filename, $
          FONT=ps_font , $
          ENCAPSULATED=encapsulated, $
          METRIC=ps_metric, $
          SCALE_FACTOR=ps_scale_factor, $
          TT_FONT=ps_tt_font, $
          QUIET=1
    
    
    ENDIF
   
    ; Get the current color table vectors.
    TVLCT, rr, gg, bb, /GET
    
    ; Going to do this in decomposed color, if possible.
    cgSetColorState, 1, CURRENTSTATE=currentState
    
    ; If current state is "indexed color" and colors are represented as long integers then "fix" them.
    IF (currentState EQ 0) THEN BEGIN
      IF Size(sbackground, /TNAME) EQ 'LONG' THEN sbackground = Fix(sbackground)
      IF Size(saxiscolor, /TNAME) EQ 'LONG' THEN saxiscolor = Fix(saxiscolor)
      IF Size(saxescolor, /TNAME) EQ 'LONG' THEN saxescolor = Fix(saxescolor)
      IF Size(serr_color, /TNAME) EQ 'LONG' THEN serr_color = Fix(serr_color)
      IF Size(scolor, /TNAME) EQ 'LONG' THEN scolor = Fix(scolor)
      IF Size(ssymcolor, /TNAME) EQ 'LONG' THEN ssymcolor = Fix(ssymcolor)
    ENDIF
    
    ; Set up the layout, if necessary.
    IF N_Elements(layout) NE 0 THEN BEGIN
       thisMulti = !P.Multi
       totalPlots = layout[0]*layout[1]
       !P.Multi = [0,layout[0], layout[1], 0, 0]
       IF layout[2] EQ 1 THEN BEGIN
            noerase = 1
            !P.Multi[0] = 0
       ENDIF ELSE BEGIN
            !P.Multi[0] = totalPlots - layout[2] + 1
       ENDELSE
    ENDIF

    ; Check the keywords.
    SetDefaultValue, title, !P.Title
    SetDefaultValue, xtitle, !X.Title
    SetDefaultValue, ytitle, !Y.Title
    SetDefaultValue, xstyle, !X.Style
    SetDefaultValue, ystyle, !Y.Style
    IF N_Elements(xrange) NE 0 THEN BEGIN
       IF cgBitGet(xstyle, 0) NE 1 THEN xstyle = xstyle + 1 
    ENDIF
    IF N_Elements(yrange) NE 0 THEN BEGIN
        IF cgBitGet(ystyle, 0) NE 1 THEN ystyle = ystyle + 1 
    ENDIF
   
    title = cgCheckForSymbols(title)
    xtitle = cgCheckForSymbols(xtitle)
    ytitle = cgCheckForSymbols(ytitle)
    IF (N_Elements(label) NE 0) && (label NE "") THEN title = ""
    traditional = Keyword_Set(traditional)
    background = cgDefaultColor(sbackground, /BACKGROUND, TRADITIONAL=traditional)
    IF Size(background, /TNAME) EQ 'STRING' && (StrUpCase(background[0]) EQ 'WHITE') THEN BEGIN
       IF (N_Elements(saxisColor) EQ 0) && (N_Elements(saxesColor) NE 0) THEN saxisColor = saxesColor
       axisColor = cgDefaultColor(saxisColor, DEFAULT='black', TRADITIONAL=traditional)
       color = cgDefaultColor(sColor, DEFAULT='black', TRADITIONAL=traditional)
    ENDIF ELSE BEGIN
       IF (N_Elements(saxisColor) EQ 0) && (N_Elements(saxesColor) NE 0) THEN saxisColor = saxesColor
       axisColor = cgDefaultColor(saxisColor, TRADITIONAL=traditional)
       color = cgDefaultColor(sColor, DEFAULT=axisColor, TRADITIONAL=traditional)
    ENDELSE

    ; If color is the same as background, do something. Since this precludes drawing the 
    ; background color (perhaps you want to "erase" something), I offer an exception. If the
    ; COLOR is "Background", I am going to assume you know what you are doing!
    IF ColorsAreIdentical(background, color) THEN BEGIN
        IF ((!D.Flags AND 256) NE 0) THEN BEGIN
           IF (!P.Multi[0] EQ 0) && (~Keyword_Set(overplot) && ~noerase) THEN cgErase, background
        ENDIF
        IF (Size(color, /TNAME) EQ 'STRING') THEN BEGIN
            IF (StrUpCase(color) NE 'BACKGROUND') THEN BEGIN
                IF ~noerase && ~Keyword_Set(overplot) THEN color = 'OPPOSITE'
            ENDIF
        ENDIF ELSE BEGIN
            IF ~noerase && ~Keyword_Set(overplot) THEN color = 'OPPOSITE'
        ENDELSE
    ENDIF
    IF ColorsAreIdentical(background, axiscolor) THEN BEGIN
        IF ((!D.Flags AND 256) NE 0) THEN BEGIN
           IF (!P.Multi[0] EQ 0) && (~Keyword_Set(overplot) && ~noerase) THEN cgErase, background
        ENDIF
        IF (Size(axiscolor, /TNAME) EQ 'STRING') THEN BEGIN
           IF (StrUpCase(axiscolor) NE 'BACKGROUND') THEN BEGIN
               IF ~noerase && ~Keyword_Set(overplot) THEN axiscolor = 'OPPOSITE'
           ENDIF
        ENDIF ELSE BEGIN
             IF ~noerase && ~Keyword_Set(overplot) THEN axiscolor = 'OPPOSITE'
        ENDELSE
    ENDIF
    symcolor = cgDefaultColor(ssymcolor, DEFAULT=color, TRADITIONAL=traditional)
    err_color = cgDefaultColor(serr_color, DEFAULT=color, TRADITIONAL=traditional)
    
    ; Character size has to be determined *after* the layout has been decided.
    IF N_Elements(font) EQ 0 THEN font = !P.Font
    IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharSize(FONT=font)
    
    ; Other keywords.
    IF N_Elements(symsize) EQ 0 THEN symsize = 1.0
    IF Keyword_Set(isotropic) THEN BEGIN
        IF N_Elements(yrange) EQ 0 THEN BEGIN
           yscaleTest = Max(dep, /NaN) - Min(dep, /NaN)
        ENDIF ELSE BEGIN
           yscaleTest = Max(yrange, /NaN) - Min(yrange, /NaN)    
        ENDELSE
        IF N_Elements(xrange) EQ 0 THEN BEGIN
           xscaleTest = Max(indep, /NaN) - Min(indep, /NaN)
        ENDIF ELSE BEGIN
           xscaleTest = Max(xrange, /NaN) - Min(xrange, /NaN)            
        ENDELSE
        aspect = Double(yscaleTest)/xscaleTest
        
        ; Do a sanity check.
        IF (aspect LT 1e-2) || (aspect GT 100) THEN $
            Message, 'Axes ranges are incompatible with the ISOTROPIC keyword. Try using ASPECT.'
        xscale = xscaleTest
        yscale = yscaleTest
        xstyle=1
        ystyle=1
    ENDIF
    IF N_Elements(psym) EQ 0 THEN psym = 0
    IF (N_Elements(aspect) NE 0) AND (Total(!P.MULTI) EQ 0) THEN BEGIN
    
        ; If position is set, then fit the plot into those bounds.
        IF (N_Elements(position) GT 0) THEN BEGIN
          trial_position = cgAspect(aspect, /Single_Plot)
          trial_width = trial_position[2]-trial_position[0]
          trial_height = trial_position[3]-trial_position[1]
          pos_width = position[2]-position[0]
          pos_height = position[3]-position[1]

          ; Same logic as cgImage: try to fit image width, then if you can't get the right aspect
          ; ratio, fit the image height instead.
          fit_ratio = pos_width / trial_width
          IF trial_height * fit_ratio GT pos_height THEN $
             fit_ratio = pos_height / trial_height

          ; new width and height
          trial_width *= fit_ratio
          trial_height *= fit_ratio

          ; calculate position vector based on trial_width and trial_height
          position[0] += 0.5*(pos_width - trial_width)
          position[2] -= 0.5*(pos_width - trial_width)
          position[1] += 0.5*(pos_height - trial_height)
          position[3] -= 0.5*(pos_height - trial_height)
        ENDIF ELSE position=cgAspect(aspect, /SINGLE_PLOT)   ; if position isn't set, just use output of Aspect
        
    ENDIF
    
    ; If you get here with no position defined, and no layout, and no !P.Multi and no nothing,
    ; then for God's sake, define a reasonable position in the window!
    IF (N_Elements(position) EQ 0) && (Total(!P.Position) EQ 0) && (N_Elements(layout) EQ 0) && (Total(!P.Multi) LE 0) THEN BEGIN       

        ; If [XY]Margins are defined, use cgLayout to figure out a position.
        IF (N_Elements(xmargin) NE 0) || (N_Elements(ymargin) NE 0) THEN BEGIN
            position = cgLayout([1,1], OXMargin=xmargin, OYMargin=ymargin)
        ENDIF ELSE BEGIN
            position = [0.125, 0.125, 0.925, 0.9] 
        ENDELSE
        
    ENDIF
           
    ; Do you need a PostScript background color? Lot's of problems here!
    ; Basically, I MUST draw a plot to advance !P.MULTI. But, drawing a
    ; plot of any sort erases the background color. So, I have to draw a 
    ; plot, store the new system variables, then draw my background, etc.
    ; I have tried LOTS of options. This is the only one that worked.
    IF !D.Name EQ 'PS' THEN BEGIN
         IF ~noerase THEN BEGIN
       
           ; I only have to do this, if this is the first plot.
           IF !P.MULTI[0] EQ 0 THEN BEGIN
           
                IF Keyword_Set(overplot) NE 1 THEN BEGIN
                
                    ; Save the current system variables. Will need to restore later.
                    bangx = !X
                    bangy = !Y
                    bangp = !P
                    
                    ; Draw the plot that doesn't draw anything.
                    Plot, indep, dep, POSITION=position, CHARSIZE=charsize, /NODATA, $
                        FONT=font, XRANGE=xrange, XSTYLE=xstyle, YRANGE=yrange, YSTYLE=ystyle, $
                        _STRICT_EXTRA=extra  
                    
                    ; Save the "after plot" system variables. Will use later. 
                    afterx = !X
                    aftery = !Y
                    afterp = !P     
                    
                    ; Draw the background color and set the variables you will need later.
                    PS_Background, background
                    psnodraw = 1
                    tempNoErase = 1
                    
                    ; Restore the original system variables so that it is as if you didn't
                    ; draw the invisible plot.
                    !X = bangx
                    !Y = bangy
                    !P = bangp
                
                ENDIF
            ENDIF ELSE tempNoErase = noerase
        ENDIF ELSE tempNoErase = noerase
     ENDIF ELSE tempNoErase = noerase
 
    
    ; Load the drawing colors. If needed create a window first, so the drawing
    ; colors are correct for the window you want to draw into.
    IF ((!D.Flags AND 256) NE 0) && (!D.Window LT 0) THEN cgDisplay
    IF Size(axiscolor, /TNAME) EQ 'STRING' THEN axiscolor = cgColor(axiscolor)
    IF Size(color, /TNAME) EQ 'STRING' THEN color = cgColor(color)
    IF Size(background, /TNAME) EQ 'STRING' THEN background = cgColor(background)
    IF Size(err_color, /TNAME) EQ 'STRING' THEN err_color = cgColor(err_color)
    IF Size(symcolor, /TNAME) EQ 'STRING' THEN symcolor = cgColor(symcolor)
    
    ; Draw the plot.
    IF Keyword_Set(overplot) THEN BEGIN
       IF psym LE 0 THEN IF ~Keyword_Set(nodata) THEN OPlot, indep, dep, COLOR=color, _EXTRA=extra
    ENDIF ELSE BEGIN
      Plot, indep, dep, BACKGROUND=background, COLOR=axiscolor, CHARSIZE=charsize, $
            POSITION=position, /NODATA, NOERASE=tempNoErase, FONT=font, TITLE=title, $
            XTITLE=xtitle, YTITLE=ytitle, XRANGE=xrange, YRANGE=yrange, $
            XSTYLE=xstyle, YSTYLE=ystyle, _STRICT_EXTRA=extra
        IF psym LE 0 THEN BEGIN
           IF ~Keyword_Set(nodata) THEN OPlot, indep, dep, COLOR=color, _EXTRA=extra  
        ENDIF  
    ENDELSE
    IF N_Elements(xrange) EQ 0 THEN xrange = !X.CRange
    IF N_Elements(yrange) EQ 0 THEN yrange = !Y.CRange
    IF Abs(psym) GT 0 THEN BEGIN
        asymbol = cgSymCat(Abs(psym), COLOR=symcolor, _Extra=extra)
        IF ~Keyword_Set(nodata) THEN OPlot, indep, dep, COLOR=symcolor, $
            PSYM=asymbol, SYMSIZE=symsize, _EXTRA=extra
    ENDIF 
    
    ; Do you have overplot objects to plot?
    IF N_Elements(oplots) NE 0 THEN BEGIN
        FOR j=0,N_Elements(oplots)-1 DO BEGIN
           thisObject = oplots[j]
           IF Obj_Valid(thisObject) THEN thisObject -> Draw
        ENDFOR
    ENDIF
    
    ; Do you have legend objects to draw?
    IF N_Elements(legends) NE 0 THEN BEGIN
        FOR j=0,N_Elements(legends)-1 DO BEGIN
           thisObject = legends[j]
           IF Obj_Valid(thisObject) THEN thisObject -> Draw
        ENDFOR
    ENDIF

    ; Need a label on the plot?
    IF N_Elements(label) NE 0 THEN BEGIN
        xx = !X.Window[0]
        yy = !Y.Window[1] + 0.015
        labelfont = (!D.Name EQ 'PS') ? 1 : 0
        cgText, xx, yy, /NORMAL, label, FONT=labelfont, COLOR=axiscolor
    ENDIF
    
    ; Do you have error bars to draw?
    errTotal = Total(N_Elements(err_xhigh) + N_Elements(err_xlow) + $
                     N_Elements(err_yhigh) + N_Elements(err_ylow))
    IF errTotal GT 0 THEN BEGIN
        
        ; Draw X errors.
        IF (N_Elements(err_xhigh) NE 0) || (N_Elements(err_xlow) NE 0) THEN BEGIN
            
            IF N_Elements(err_width) EQ 0 THEN BEGIN
                yerr_width = 0.01 * (!Y.Window[1] - !Y.Window[0]) 
            ENDIF ELSE BEGIN
                yerr_width = err_width
            ENDELSE
            
            ; X high error bars.
            IF (N_Elements(err_xhigh) NE 0) THEN BEGIN
                xhigh = indep + err_xhigh
                IF !X.Type EQ 1 THEN BEGIN ; Log X axis
                   indices = Where(indep GE 10^xrange[0] AND indep LE 10^xrange[1], cnt)
                ENDIF ELSE BEGIN
                   indices = Where(indep GE xrange[0] AND indep LE xrange[1], cnt)
                ENDELSE
                IF cnt GT 0 && cnt NE N_Elements(xhigh) THEN BEGIN
                    indep_ = indep[indices]
                    dep_ = dep[indices]
                    xhigh = xhigh[indices]
                ENDIF ELSE BEGIN
                    indep_ = indep
                    dep_ = dep
                ENDELSE
                FOR j=0,N_Elements(xhigh)-1 DO BEGIN
                    PlotS, [indep_[j], xhigh[j]], [dep_[j], dep_[j]], Color=err_color, Thick=err_thick, $
                        NoClip=1-Keyword_Set(err_clip)
                    nCoord = Convert_Coord(xhigh[j], dep_[j], /Data, /To_Normal)
                    PlotS, [nCoord[0], nCoord[0]], [nCoord[1]+yerr_width, nCoord[1]-yerr_width], $
                        /Normal, Color=err_color, Thick=err_thick, NoClip=1-Keyword_Set(err_clip)
                ENDFOR
            ENDIF
            
            ; X low error bars.
            IF (N_Elements(err_xlow) NE 0) THEN BEGIN
                xlow = indep - err_xlow
                IF !X.Type EQ 1 THEN BEGIN ; Log X axis
                    indices = Where(indep GE 10^xrange[0] AND indep LE 10^xrange[1], cnt)
                ENDIF ELSE BEGIN
                    indices = Where(indep GE xrange[0] AND indep LE xrange[1], cnt)
                ENDELSE
                IF cnt GT 0 && cnt NE N_Elements(xlow) THEN BEGIN
                    indep_ = indep[indices]
                    dep_ = dep[indices]
                    xlow = xlow[indices]
                ENDIF ELSE BEGIN
                    indep_ = indep
                    dep_ = dep
                ENDELSE
                FOR j=0,N_Elements(xlow)-1 DO BEGIN
                    PlotS, [indep_[j], xlow[j]], [dep_[j], dep_[j]], Color=err_color, Thick=err_thick, $
                    NoClip=1-Keyword_Set(err_clip)
                    nCoord = Convert_Coord(xlow[j], dep_[j], /Data, /To_Normal)
                    PlotS, [nCoord[0], nCoord[0]], [nCoord[1]+yerr_width, nCoord[1]-yerr_width], $
                        /Normal, Color=err_color, Thick=err_thick, NoClip=1-Keyword_Set(err_clip)
                ENDFOR
            ENDIF

        ENDIF
        
        ; Draw y errors.
        IF (N_Elements(err_yhigh) NE 0) || (N_Elements(err_ylow) NE 0) THEN BEGIN
        
            IF N_Elements(err_width) EQ 0 THEN BEGIN
                xerr_width = 0.01 * (!X.Window[1] - !X.Window[0])
            ENDIF ELSE BEGIN
                xerr_width = err_width
            ENDELSE
            
            ; Y high error bars.
            IF (N_Elements(err_yhigh) NE 0) THEN BEGIN
                yhigh = dep + err_yhigh
                IF !Y.Type EQ 1 THEN BEGIN ; Log Y axis
                    indices = Where(dep GE 10^yrange[0] AND dep LE 10^yrange[1], cnt)
                ENDIF ELSE BEGIN
                    indices = Where(dep GE yrange[0] AND dep LE yrange[1], cnt)
                ENDELSE
                IF cnt GT 0 && cnt NE N_Elements(yhigh) THEN BEGIN
                    indep_ = indep[indices]
                    dep_ = dep[indices]
                    yhigh = yhigh[indices]
                ENDIF ELSE BEGIN
                    indep_ = indep
                    dep_ = dep
                ENDELSE
                FOR j=0,N_Elements(yhigh)-1 DO BEGIN
                    PlotS, [indep_[j], indep_[j]], [yhigh[j], dep_[j]], Color=err_color, Thick=err_thick, $
                        NoClip=1-Keyword_Set(err_clip)
                    nCoord = Convert_Coord(indep_[j], yhigh[j], /Data, /To_Normal)
                    PlotS, [nCoord[0]-xerr_width, nCoord[0]+xerr_width], [nCoord[1], nCoord[1]], $
                        /Normal, Color=err_color, Thick=err_thick, NoClip=1-Keyword_Set(err_clip)
                ENDFOR
            ENDIF
            
            ; Y low error bars.
            IF (N_Elements(err_ylow) NE 0) THEN BEGIN
                ylow = dep - err_ylow
                IF !Y.Type EQ 1 THEN BEGIN ; Log Y axis
                    indices = Where(dep GE 10^yrange[0] AND dep LE 10^yrange[1], cnt)
                ENDIF ELSE BEGIN
                    indices = Where(dep GE yrange[0] AND dep LE yrange[1], cnt)
                ENDELSE
                IF cnt GT 0 && cnt NE N_Elements(yhigh) THEN BEGIN
                    indep_ = indep[indices]
                    dep_ = dep[indices]
                    ylow = ylow[indices]
                ENDIF ELSE BEGIN
                    indep_ = indep
                    dep_ = dep
                ENDELSE
                FOR j=0,N_Elements(ylow)-1 DO BEGIN
                    PlotS, [indep_[j], indep_[j]], [ylow[j], dep_[j]], Color=err_color, Thick=err_thick, $
                        NoClip=1-Keyword_Set(err_clip)
                    nCoord = Convert_Coord(indep_[j], ylow[j], /Data, /To_Normal)
                    PlotS, [nCoord[0]-xerr_width, nCoord[0]+xerr_width], [nCoord[1], nCoord[1]], $
                        /Normal, Color=err_color, Thick=err_thick, NoClip=1-Keyword_Set(err_clip)
                ENDFOR
            ENDIF
            
        ENDIF
    ENDIF
         
    ; If this is the first plot in PS, then we have to make it appear that we have
    ; drawn a plot, even though we haven't.
    IF N_Elements(psnodraw) EQ 1 THEN BEGIN
        !X = afterX
        !Y = afterY
        !P = afterP
    ENDIF
    
    ; Are we producing output? If so, we need to clean up here.
    IF (N_Elements(output) NE 0) && (output NE "") THEN BEGIN
    
       ; Get the output default values.
       cgWindow_GetDefs, $
           IM_Density = im_density, $                      ; Sets the density parameter on ImageMagick convert command.
           IM_Options = im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
           IM_Resize = im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
           IM_Transparent = im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
           IM_Width = im_width, $                          ; Sets the width of raster file output created with ImageMagick.
           PDF_Unix_Convert_Cmd = pdf_unix_convert_cmd, $  ; Command to convert PS to PDF.
           PDF_Path = pdf_path                             ; The path to the Ghostscript conversion command.
    
        ; Close the PostScript file and create whatever output is needed.
        cgPS_Close, DELETE_PS=delete_ps, $
             ALLOW_TRANSPARENT=im_transparent, $
             BMP=bmp_flag, $
             DENSITY=im_density, $
             GIF=gif_flag, $
             GS_PATH=pdf_path, $
             IM_OPTIONS=im_options, $
             JPEG=jpeg_flag, $
             PDF=pdf_flag, $
             PNG=png_flag, $
             RESIZE=im_resize, $
             TIFF=tiff_flag, $
             UNIX_CONVERT_CMD=pdf_unix_convert_cmd, $
             WIDTH=im_width

         basename = File_Basename(outfilename)
         dirname = File_Dirname(outfilename)
         IF dirname EQ "." THEN CD, CURRENT=dirname
         Print, 'Output File: ' + Filepath(ROOT_DIR=dirname, basename)
    ENDIF
    
    ; Restore the decomposed color state if you can.
    cgSetColorState, currentState
    
    ; Restore the color table. Can't do this for the Z-buffer or
    ; the snap shot will be incorrect.
    IF (!D.Name NE 'Z') THEN TVLCT, rr, gg, bb
    
    ; Clean up if you are using a layout.
    IF N_Elements(layout) NE 0 THEN !P.Multi = thisMulti
    
END
    
