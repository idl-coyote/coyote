; docformat = 'rst'
;
; NAME:
;   cgContour
;
; PURPOSE:
;   The purpose of cgContour is to create a wrapper for the traditional IDL graphics
;   command, Contour. The Contour command has a number of deficiencies that make it
;   difficult to use in a modern computing environment. cgContour corrects these
;   deficiencies and allows the user to produce traditional contour plots in a device
;   and machine independent manner.
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
; The purpose of cgContour is to create a wrapper for the traditional IDL graphics
; command, Contour. The Contour command has a number of deficiencies that make it
; difficult to use in a modern computing environment. cgContour corrects these
; deficiencies and allows the user to produce traditional contour plots in a device
; and machine independent manner.
; 
; .. image:: cgcontour.png
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics
;    
; :Params:
;    data: in, required, type=any
;         A one- or two-dimensional array containing the values that make 
;         up the contour surface.
;    x: in, optional, type=any
;         A vector or two-dimensional array specifying the X coordinates for
;         the contour surface.
;    y: in, optional, type=any
;         A vector or two-dimensional array specifying the Y coordinates for
;         the contour surface.
;       
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        Set this keyword to add the command to an cgWindow. Setting this keyword
;        automatically sets the WINDOW keyword, but the command does not erase the
;        graphics window as it would normally.
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
;     c_annotation: in, optional, type=string
;        The label to be drawn on each contour. Normally contours are labeled with their
;        value. This vector of strings may substitute for those values.
;     c_colors: in, optional, type=integer/string vector
;        Set to the index values of the contour colors or to named colors. Must contain
;        the same number of colors as the number of requested contour levels.
;     c_labels: in, optional, type=integer
;        A vector that specifies which contour levels to label. If used, the LABEL
;        keyword is ignored.
;     c_charsize: in, optional, type=float
;        The character size of the annotations used on the contour lines themselves.
;        By default, 75% of `Charsize`.
;     cell_fill: in, optional, type=boolean, default=0
;        Set to indicate filled contours should be created using the "cell fill" method.
;        This keyword should always be set if displaying filled contours on map projections
;        or if missing data is present in the data you are contouring.
;     charsize: in, optional, type=float, default=cgDefCharSize()
;        The character size for axes annotations. Uses cgDefCharSize to select default
;        character size, unless !P.Charsize is set, in which case !P.Charsize is always used.
;     color: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the data color. By default, same as AXISCOLOR.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     font: in, optional, type=integer, default=!P.Font
;        The type of font desired for axis annotation.
;     fill: in, optional, type=boolean, default=0
;        Set to indicate filled contours should be created.
;     irregular: in, optional, type=boolean
;        If this keyword is set, the data, x, and y input parameters are taken to be
;        irregularly gridded data, the the data is gridded for use in the contour plot
;        using the Triangulate and Trigrid method. The resolution of the gridded output
;        is set by the RESOLUTION keyword.
;     label: in, optional, type=integer, default=1
;        An number that tells how to label contour levels. A 0 means
;        no contour levels are labelled. A 1 (the default) means all contour levels are
;        labelled. A 2 means label every 2nd contour level is labelled. A 3 means every 
;        3rd contour level is labelled, and so on.
;     layout: in, optional, type=intarr(3)
;        This keyword specifies a grid with a graphics window and determines where the
;        graphic should appear. The syntax of LAYOUT is three numbers: [ncolumns, nrows, location].
;        The grid is determined by the number of columns (ncolumns) by the number of 
;        rows (nrows). The location of the graphic is determined by the third number. The
;        grid numbering starts in the upper left (1) and goes sequentually by column and then
;        by row.
;     levels: in, optional, type=any
;        A vector of data levels to contour. If used, NLEVELS is ignored. If missing, 
;        NLEVELS is used to construct N equally-spaced contour levels.
;     map_object: in, optional, type=object
;        If you are overplotting (OVERPLOT=1) on a map projection set up with Map_Proj_Init
;        and using projected meter space, rather than lat/lon space, then you can use this
;        keyword to provide a cgMap object that will allow you to convert the `x` and `y`
;        grid parameters from longitude and latitude, respectively, to projected meter space
;        before the contour is displayed. Note, you MUST pass the `x` and `y` grid parameters 
;        to cgContour if you are overplotting on a map projection. There is no checking to
;        be sure these parameters are in the correct longitude and latitude range, respectively.
;     missingvalue: in, optional, type=any
;        Use this keyword to identify any missing data in the input data values.
;     noclip: in, optional, type=boolean, default=0
;        Normally, the plot is clipped to the axes boundaries. Setting this keyword prevents
;        such clipping. Normally, this keyword is only used when there is a problem displaying
;        contour plots in 3D space.
;     nlevels: in, optional, type=integer, default=6
;        If the Contour plot LEVELS keyword is not used, this keyword will produce this
;        number of equally spaced contour intervals. Unlike the Contour NLEVELS keyword,
;        this keyword actually works!
;     noerase: in, optional, type=boolean, default=0
;        Set this keyword to prevent the window from erasing the contents before displaying
;        the contour plot.
;     olevels: out, optional
;        Set to a named variable to return the actual contour levels used in the program.
;        Unfortunately, output variables cannot be returned if the cgContour command is
;        being executed in a cgWindow.
;     onimage: in, optional, type=boolean, default=0
;        If this keyword is set, and an image has been display previously with cgImage,
;        then the contour plot will determine the location of the image in the display
;        window and overplot itself onto that image.
;     outcolor: in, optional, type=string, default='charcoal'
;        The color of the contour lines when the `Outline` keyword is used.
;     outfilename: in, optional, type=string
;        If the `Output` keyword is set, the user will be asked to supply an output
;        filename, unless this keyword is set to a non-null string. In that case, the
;        value of this keyword will be used as the filename and there will be no dialog
;        presented to the user.
;     outline: in, optional, type=boolean, default=0
;        This keyword applies only if the `Fill` keyword is set. It will draw the
;        contour lines on top of the filled contour. It draws the outline in the `OutColor`.
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
;        to produce anything other than PostScript output. (See cgPS2PDF and PS_END for 
;        details.) And also note that you should NOT use this keyword when doing multiple 
;        plots. The keyword is to be used as a convenient way to get PostScript or raster 
;        output for a single graphics command. Output parameters can be set with cgWindow_SetDefs.
;     overplot: in, optional, type=boolean, default=0
;        Set this keyword to overplot the contours onto a previously established
;        data coordinate system.
;     palette: in, optional, type=byte
;        A (256x3) color palette containing the RGB color vectors to use for coloring contours.
;        Contour colors will be sampled from the color table palette into the number 
;        of contour levels required. If the palette is NOT 256 elements in length, then
;        it is assumed that the length corresponds to the number of levels to be contoured.
;     position: in, optional, type=float
;        Set this keyword to a four-element [x0,y0,x1,y1] array giving the contour plot
;        position in normalized coordinates. 
;     resolution: in, optional, type=integer array, default=[41\,41]
;        If the IRREGULAR keyword is set, this keyword specifies the X and Y resolution
;        in a two element integer array of the final gridded data that is sent to the 
;        contour plot.
;     t3d: in, optional, type=boolean, default=0
;        Set this keyword to use the 3D axis rotation matrix in !P.T3D.
;     title: in, optional, type=string
;        Set this keyword to the title of the plot.
;     traditional: in, optional, type=boolean, default=0
;         If this keyword is set, the traditional color scheme of a black background for
;         graphics windows on the display is used and PostScript files always use a white background.
;     window: in, optional, type=boolean, default=0
;        Set this keyword to replace all the commands in the current cgWindow or to
;        create a new cgWindow, if one doesn't currenly exist, for displaying this command.
;        To create a new cgWindow if one currenly exists, use the `cgWindow` command
;     xstyle: in, optional, type=integer, default=1
;        If unused in the program, set to 1 to force exact axis scaling.
;     xthick: in, optional, type=integer, default=1
;        The thickness of the X axis annotations.
;     xticklen: in, optional, type=float, default=0.025
;        The length of the X tick marks. Set to a negative value to create outward
;        facing tick marks.
;     xticks: in, optional, type=integer
;        The number of tick intervals on the X axis.
;     xtickv: in, optional, type=string
;        A vector of tick values to use with the tick marks. See IDL documentation for
;        graphics keywords for additional information.
;     xtitle: in, optional, type=string
;        Set this keyword to the X title of the plot.
;     ystyle: in, optional, type=integer, default=1
;        If unused in the program, set to 1 to force exact axis scaling.
;     ythick: in, optional, type=integer, default=1
;        The thickness of the Y axis annotations.
;     yticklen: in, optional, type=float, default=0.025
;        The length of the Y tick marks. Set to a negative value to create outward
;        facing tick marks.
;     yticks: in, optional, type=integer
;        The number of tick intervals on the Y axis.
;     ytickv: in, optional, type=string
;        A vector of tick values to use with the tick marks. See IDL documentation for
;        graphics keywords for additional information.
;     ytitle: in, optional, type=string
;        Set this keyword to the Y title of the plot.
;     _ref_extra: in, optional, type=any
;        Any keyword appropriate for the `IDL Contour command <http://www.exelisvis.com/docs/CONTOUR_Procedure.html>` 
;        is allowed in the program.
;
; :Examples:
;    Use as you would use the IDL CONTOUR command::
;       data = dist(51)
;       cgContour, data
;       LoadCT, 33
;       cgContour, data, /FILL
;       cgContour, data, /OVERPLOT
;       
;    If you wish to overplot on top of an image, use the ONIMAGE keyword, rather
;    than the OVERPLOT keyword:
;       cgImage, data, /SCALE, XRANGE=[-10, 10], YRANGE=[-5,5], /AXES
;       cgContour, data, /ONIMAGE
;          
;       
;       See `Device Independent Contour Plots <http://www.idlcoyote.com/graphics_tips/cgcontour.html>` 
;       for additional examples.
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
;        Written, 11 November 2010. DWF.
;        Restored the CELL_FILL keyword, which had been accidentally removed in
;           the earlier version. 12 November 2010. DWF.
;        Add the ability to specify the contour colors as color names. 16 November 2010. DWF.
;        Now setting decomposition state by calling SetDecomposedState. 16 November 2010. DWF.
;        Final color table restoration skipped in Z-graphics buffer. 17 November 2010. DWF.
;        Background keyword now applies in PostScript file as well. 17 November 2010. DWF.
;        Many changes after BACKGROUND changes to get !P.MULTI working again! 18 November 2010. DWF.
;        Fixed a small problem with the OVERPLOT keyword. 18 Nov 2010. DWF.
;        Changes so that color variables don't change type. 23 Nov 2010. DWF.
;        Added WINDOW keyword to allow graphic to be displayed in a resizable graphics window. 8 Dec 2010. DWF
;        Modifications to allow cgContour to be drop-in replacement for old Contour commands in 
;            indexed color mode. 24 Dec 2010. DWF.
;        Previous changes introduced problems with OVERPLOT that have now been fixed. 28 Dec 2010. DWF.
;        Set NOERASE keyword from !P.NoErase system variable when appropriate. 28 Dec 2010. DWF.
;        Additional problems with NOERASE discovered and solved. 29 Dec 2010. DWF.
;        Change to DECOMPOSED color was using incorrect color tables. 29 Dec 2010. DWF.
;        In some cases, I was turning BYTE values to strings without converting to 
;            INTEGERS first. 30 Dec 2010. DWF.
;        Still working on getting contour colors to work in decomposed color mode in all 
;             circumstances. 2 Jan 2011. DWF.
;        Fixed problem with FILL when no contour colors (C_COLORS) are specified. 3 Jan 2011. DWF.
;        Fixed a problem that preventing output keyword (e.g., PATH_INFO) from being returned properly. 
;             3 Jan 2011. DWF.
;        Fixed a problem calculating NLEVELS when LEVELS keyword was used instead. 3 Jan 2011. DWF.
;        TVLCT commands protected from NULL device. 4 Jan 2011. DWF.
;        Fixed a no color problem when CELL_FILL was set. 11 Jan 2011. DWF.
;        Fixed a problem with overlaying filled contours with /OVERPLOT. 11 Jan 2011. DWF.
;        Selecting character size now with cgDefCharSize. 11 Jan 2011. DWF.      
;        Moved setting to decomposed color before color selection process to avoid PostScript
;             background problems when passed 24-bit color integers. 12 Jan 2011. DWF.   
;        Fixed a problem in which I assumed the background color was a string. 18 Jan 2011. DWF.  
;        Added ADDCMD keyword. 26 Jan 2011. DWF.
;        Added LAYOUT keyword. 28 Jan 2011. DWF.
;        Added PALETTE keyword. 4 Feb 2011. DWF.
;        Color table vectors must be obtained AFTER loading the color palette. 6 March 2011. DWF.
;        Modified error handler to restore the entry decomposition state if there is an error. 17 March 2011. DWF
;        Modifications to allow palettes of less than 256 elememts in length to be used. 1 April 2011. DWF.
;        Modifications to repair axes and tickmarks when creating filled contour plots. 28 May 2011. DWF.
;        Whoops! Last fix shouldn't apply to OVERPLOTTING. Fixed. 22 June 2011. DWF.
;        Still more work to get axes overplotting to work correct. 5 July 2011. DWF.
;        Added an ONIMAGE keyword that allows the contours to be overplotted on top of an image that
;           has been displayed with cgImage. This requires that the SAVE keyword is set in the 
;           cgImage call.
;        Improved error handling. 26 Aug 2011. DWF.
;        Got the data type correct in the part of the code that creates levels. 6 Sept 2011. DWF.
;        Small change to allow cgWindow to set the current graphics window if it is the only
;           window on the display. 15 Sept 2011. DWF.
;        Had to add XTICKV, YTICKV, XTICKS, and YTICKS keywords to get repaired axes to work
;            properly on filled contour plots. There may be other keywords needed, but I am 
;            going to add them on an as-needed basis. 30 Sept 2011. DWF.
;        Other keywords WERE needed! I added XTICKLEN and YTICKLEN keywords to the repaired axes
;            code. 3 Oct 2011. DWF.
;        Change from 15 Sept 2011 forgot to include the possibility of pixmap windows. Algorithm
;            made more robust. 27 Oct 2011. DWF.
;        There was a problem with axes when plotting contours in 3D that has been fixed. 18 Nov 2011. DWF.
;        Added OLEVELS keyword. 7 Dec 2011. DWF.
;        Added OUTLINE and OUTCOLOR keywords. 8 Dec 2011. DWF.
;        Modified the way the axes are drawn when given a negative tick length. 9 Dec 2011. DWF.
;        Added the ability to send the output directly to a file via the OUTPUT keyword. 9 Dec 2011, DWF.
;        PostScript, PDF, and Imagemagick parameters can now be tailored with cgWindow_SetDefs. 14 Dec 2001. DWF.
;        Made sure the OUTLINE keyword works with CELL_FILL, too. 16 Dec 2011. DWF.
;        Modified to use cgDefaultColor for default color selection. 24 Dec 2011. DWF.
;        Added MAP_OBJECT keyword. 28 Dec 2011. DWF.
;        Changes to allow better default colors, based on changes to cgColor and cgDefaultColor. 1 Feb 2012. DWF.
;        Axis repair for filled contour plots (done with AXIS) results in incorrect tick labeling with
;            date/time axes. Replaced repair code with actual Contour command. 9 March 2012. DWF.
;        Fixed a problem with color palettes by defining NLEVELS according to the number of colors
;            in the palette. 19 March 2012. DWF.
;        Now allowing the user to draw in the "background" color, if the COLOR or AXISCOLOR is "BACKGROUND". 19 March 2012. DWF.
;        The axis repair change on 9 March was not working in multi plots because the plot was already
;            advanced. Added a fix to make sure the repair is to the correct multi plot. 20 April 2012. DWF.           
;        Added an ASPECT keyword to maintain the program aspect ratio. 12 July 2012. DWF.
;        Added the ability to use escape characters in plot titles to specify cgSymbol symbols. 27 July 2012. DWF.
;        Added C_ANNOTATION keyword. 10 Nov 2012. DWF.
;        Modified the way default colors are selected when the background color is "white". 4 Dec 2012. DWF.
;        Making more effort to set the CELL_FILL keyword instead of FILL if filling contours on maps. 7 Jan 2013. DWF.
;        
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO cgContour, data, x, y, $
    ADDCMD=addcmd, $
    ASPECT=aspect, $
    AXISCOLOR=saxiscolor, $
    AXESCOLOR=saxescolor, $
    BACKGROUND=sbackground, $
    C_ANNOTATION=c_annotation, $
    C_CHARSIZE=c_charsize, $
    C_COLORS=c_colors, $
    C_LABELS=c_labels, $
    CELL_FILL=cell_fill, $
    CHARSIZE=charsize, $
    COLOR=scolor, $
    FILL=fill, $
    FONT=font, $
    IRREGULAR=irregular, $
    LABEL=label, $
    LAYOUT=layout, $
    LEVELS=levels, $
    NLEVELS=nlevels, $
    NOCLIP=noclip, $
    NOERASE=noerase, $
    MAP_OBJECT=map_object, $
    MISSINGVALUE=missingvalue, $
    OLEVELS=olevels, $
    ONIMAGE=onImage, $
    OUTCOLOR=outcolor, $
    OUTFILENAME=outfilename, $
    OUTLINE=outline, $
    OUTPUT=output, $
    OVERPLOT=overplot, $
    PALETTE=palette, $
    POSITION=position, $
    RESOLUTION=resolution, $
    T3D=t3d, $
    TITLE=title, $
    TRADITIONAL=traditional, $
    WINDOW=window, $
    XSTYLE=xstyle, $
    XTHICK=xthick, $
    XTICKLEN=xticklen, $
    XTICKV=xtickv, $
    XTICKS=xticks, $
    XTITLE=xtitle, $
    YSTYLE=ystyle, $
    YTHICK=ythick, $
    YTICKLEN=yticklen, $
    YTICKV=ytickv, $
    YTICKS=yticks, $
    YTITLE=ytitle, $
    ZVALUE=zvalue, $
    _REF_EXTRA=extra
    
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(thisMulti) NE 0 THEN !P.Multi = thisMulti
        IF (!D.Name NE "NULL") && (N_Elements(rr) NE 0) THEN TVLCT, rr, gg, bb
        IF N_Elements(currentState) NE 0 THEN SetDecomposedState, currentState
        RETURN
    ENDIF
    
    ; Check parameters.
    IF N_Elements(data) EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: cgContour, data, x, y, NLEVELS=10'
        RETURN
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Check parameters.
    IF N_Params() EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: cgContour, data, x, y'
        RETURN
    ENDIF

    ; Do they want this plot in a resizeable graphics window?
    IF Keyword_Set(addcmd) THEN window = 1
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        ; If you are using a layout, you can't ever erase.
        IF N_Elements(layout) NE 0 THEN noerase = 1
            
        IF Keyword_Set(overplot) OR Keyword_Set(addcmd) THEN BEGIN
            cgWindow, 'cgContour', data, x, y, $
                ASPECT=aspect, $
                AXISCOLOR=saxiscolor, $
                AXESCOLOR=saxescolor, $
                BACKGROUND=sbackground, $
                C_ANNOTATION=c_annotation, $
                C_CHARSIZE=c_charsize, $
                C_COLORS=c_colors, $
                C_LABELS=c_labels, $
                CELL_FILL=cell_fill, $
                CHARSIZE=charsize, $
                COLOR=scolor, $
                FONT=font, $
                FILL=fill, $
                IRREGULAR=irregular, $
                LABEL=label, $
                LAYOUT=layout, $
                LEVELS=levels, $
                NLEVELS=nlevels, $
                NOCLIP=noclip, $
                NOERASE=noerase, $
                MAP_OBJECT=map_object, $
                MISSINGVALUE=missingvalue, $
                OLEVELS=olevels, $
                ONIMAGE=onimage, $
                OUTCOLOR=outcolor, $
                OUTLINE=outline, $
                OVERPLOT=overplot, $
                PALETTE=palette, $
                POSITION=position, $
                RESOLUTION=resolution, $
                T3D=t3d, $
                TITLE=title, $
                TRADITIONAL=traditional, $
                XSTYLE=xstyle, $
                XTHICK=xthick, $
                XTICKLEN=xticklen, $
                XTICKV=xtickv, $
                XTICKS=xticks, $
                XTITLE=xtitle, $
                YSTYLE=ystyle, $
                YTHICK=ythick, $
                YTICKLEN=yticklen, $
                YTICKV=ytickv, $
                YTICKS=yticks, $
                YTITLE=ytitle, $
                ZVALUE=zvalue, $
                ADDCMD=1, $
                _Extra=extra
             RETURN
       ENDIF
        
        currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
        IF wincnt EQ 0 THEN replaceCmd = 0 ELSE replaceCmd=1
        cgWindow, 'cgContour', data, x, y, $
            ASPECT=aspect, $
            AXISCOLOR=saxiscolor, $
            AXESCOLOR=saxescolor, $
            BACKGROUND=sbackground, $
            C_ANNOTATION=c_annotation, $
            C_CHARSIZE=c_charsize, $
            C_COLORS=c_colors, $
            C_LABELS=c_labels, $
            CELL_FILL=cell_fill, $
            CHARSIZE=charsize, $
            COLOR=scolor, $
            FONT=font, $
            FILL=fill, $
            IRREGULAR=irregular, $
            LABEL=label, $
            LAYOUT=layout, $
            LEVELS=levels, $
            NLEVELS=nlevels, $
            NOCLIP=noclip, $
            NOERASE=noerase, $
            MAP_OBJECT=map_object, $
            MISSINGVALUE=missingvalue, $
            OLEVELS=olevels, $
            ONIMAGE=onimage, $
            OUTCOLOR=outcolor, $
            OUTLINE=outline, $
            OVERPLOT=overplot, $
            PALETTE=palette, $
            POSITION=position, $
            RESOLUTION=resolution, $
            T3D=t3d, $
            TITLE=title, $
            TRADITIONAL=traditional, $
            XSTYLE=xstyle, $
            XTHICK=xthick, $
            XTICKLEN=xticklen, $
            XTICKV=xtickv, $
            XTICKS=xticks, $
            XTITLE=xtitle, $
            YSTYLE=ystyle, $
            YTHICK=ythick, $
            YTICKLEN=yticklen, $
            YTICKV=ytickv, $
            YTICKS=yticks, $
            YTITLE=ytitle, $
            ZVALUE=zvalue, $
            REPLACECMD=replaceCmd, $
            _Extra=extra
            
         RETURN
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
       PS_Start, $
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
    
    ; If you supplied annotation, check it for embedded symbols.
    IF N_Elements(c_annotation) NE 0 THEN BEGIN
        FOR j=0,N_Elements(c_annotation)-1 DO c_annotation[j] = cgCheckForSymbols(c_annotation[j])
    ENDIF
   
    IF (N_Elements(aspect) NE 0) AND (Total(!P.MULTI) EQ 0) THEN BEGIN
    
        ; If position is set, then fit the plot into those bounds.
        IF (N_Elements(position) GT 0) THEN BEGIN
          trial_position = Aspect(aspect, margin=0.)
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
        ENDIF ELSE position=Aspect(aspect)   ; if position isn't set, just use output of Aspect
        
    ENDIF
    
    ; If you want to overplot on an image, set the OVERPLOT keyword.
    IF Keyword_Set(onImage) THEN overplot = 1
    
    ; Going to have to do all of this in decomposed color, if possible.
    SetDecomposedState, 1, CURRENTSTATE=currentState
    
    ; If current state is "indexed color" and colors are represented as long integers then "fix" them.
    IF (currentState EQ 0) THEN BEGIN
      IF Size(sbackground, /TNAME) EQ 'LONG' THEN sbackground = Fix(sbackground)
      IF Size(saxiscolor, /TNAME) EQ 'LONG' THEN saxiscolor = Fix(saxiscolor)
      IF Size(saxescolor, /TNAME) EQ 'LONG' THEN saxescolor = Fix(saxescolor)
      IF Size(scolor, /TNAME) EQ 'LONG' THEN scolor = Fix(scolor)
    ENDIF
    
    ; Pay attention to !P.Noerase in setting the NOERASE kewyord. This must be
    ; done BEFORE checking the LAYOUT properties.
    IF !P.NoErase NE 0 THEN noerase = !P.NoErase ELSE noerase = Keyword_Set(noerase)
    
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

    ; Character size has to be determined *after* the layout has been decided.
    IF N_Elements(font) EQ 0 THEN font = !P.Font
    IF Keyword_Set(t3d) && (!D.Name EQ 'PS') THEN font = -1
    IF N_Elements(title) EQ 0 THEN title = "" ELSE title = cgCheckForSymbols(title)
    IF N_Elements(xtitle) EQ 0 THEN xtitle = "" ELSE xtitle = cgCheckForSymbols(xtitle)
    IF N_Elements(ytitle) EQ 0 THEN ytitle = "" ELSE ytitle = cgCheckForSymbols(ytitle)
    IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharSize(FONT=font)
    IF N_Elements(c_charsize) EQ 0 THEN c_charsize = charsize * 0.75
    
    ; Handle data properly.
    ndims = Size(data, /N_DIMENSIONS)
    s = Size(data, /DIMENSIONS)
    CASE ndims OF
        1: BEGIN
           IF N_Elements(x) EQ 0 THEN BEGIN
               IF Keyword_Set(onImage) THEN BEGIN
                   CASE !X.TYPE OF
                      0: xgrid = Scale_Vector(Indgen(s[0]), !X.CRange[0], !X.CRange[1])
                      1: xgrid = Scale_Vector(Indgen(s[0]), 10^!X.CRange[0], 10^!X.CRange[1])
                      3: Message, 'Must supply LONGITUDE vector when overplotting on map projections'
                   ENDCASE
               ENDIF ELSE BEGIN
                  xgrid = Indgen(s[0])
               ENDELSE
           ENDIF ELSE xgrid = x
           IF N_Elements(y) EQ 0 THEN BEGIN
               IF Keyword_Set(onImage) THEN BEGIN
                   CASE !Y.TYPE OF
                      0: ygrid = Scale_Vector(Indgen(s[1]), !Y.CRange[0], !Y.CRange[1])
                      1: ygrid = Scale_Vector(Indgen(s[1]), 10^!Y.CRange[0], 10^!Y.CRange[1])
                      3: Message, 'Must supply LATITUDE vector when overplotting on map projections'
                   ENDCASE
               ENDIF ELSE BEGIN
                  ygrid = Indgen(s[1])
               ENDELSE
           ENDIF ELSE ygrid = y
           END
        2: BEGIN
           IF N_Elements(x) EQ 0 THEN BEGIN
               IF Keyword_Set(onImage) THEN BEGIN
                   CASE !X.TYPE OF
                      0: xgrid = Scale_Vector(Indgen(s[0]), !X.CRange[0], !X.CRange[1])
                      1: xgrid = Scale_Vector(Indgen(s[0]), 10^!X.CRange[0], 10^!X.CRange[1])
                      3: Message, 'Must supply LONGITUDE vector when overplotting on map projections'
                   ENDCASE
               ENDIF ELSE BEGIN
                  xgrid = Indgen(s[0])
               ENDELSE
           ENDIF ELSE xgrid = x
           IF N_Elements(y) EQ 0 THEN BEGIN
               IF Keyword_Set(onImage) THEN BEGIN
                   CASE !Y.TYPE OF
                      0: ygrid = Scale_Vector(Indgen(s[1]), !Y.CRange[0], !Y.CRange[1])
                      1: ygrid = Scale_Vector(Indgen(s[1]), 10^!Y.CRange[0], 10^!Y.CRange[1])
                      3: Message, 'Must supply LATITUDE vector when overplotting on map projections'
                   ENDCASE
               ENDIF ELSE BEGIN
                  ygrid = Indgen(s[1])
               ENDELSE
           ENDIF ELSE ygrid = y
           END
        ELSE: Message, 'Contour data must be 1D or 2D.'
    ENDCASE
    
    ; Get the current color table vectors. The NULL business was put here at
    ; the request of Wayne Landsman in support of NASA Astronomy Library. It
    ; is important for programs NASA runs.
    IF (!D.Name NE 'NULL') THEN BEGIN
         
        ; If you have a palette, load the colors now. Otherwise whatever colors
        ; are in the current color table will be used. If you are using a palette,
        ; you should NOT use C_COLORS, so I undefine it.
        IF N_Elements(palette) NE 0 THEN BEGIN
            IF Size(palette, /N_DIMENSIONS) NE 2 THEN Message, 'Color palette is not a 3xN array.'
            dims = Size(palette, /DIMENSIONS)
            threeIndex = Where(dims EQ 3)
            IF ((threeIndex)[0] LT 0) THEN Message, 'Color palette is not a 3xN array.'
            IF threeIndex[0] EQ 0 THEN palette = Transpose(palette)
            TVLCT, p_red, p_grn, p_blu, /Get ; Save the color vectors before loading the palette.
            TVLCT, palette
            
            ; Set up contour colors for the palette. If you passed contour colors,
            ; then I assume these are indices into the color palette.
            IF N_Elements(c_colors) NE 0 THEN con_colors = cgColor24(palette[c_colors,*])
            
            ; If the palette contains fewer colors than the color table, then I assume
            ; the palette is just for contour colors.
            IF (N_Elements(c_colors) EQ 0) && ((N_Elements(palette)/3) LT 256) THEN BEGIN
                con_colors = cgColor24(palette)
            ENDIF
            
            ; If the number of contour colors is less than 256, and LEVELS and NLEVELS are
            ; both undefined, then let's define NLEVELS to be equal to the number of contour
            ; colors in the color palette.
            dims = Size(palette, /DIMENSIONS) ; First dimension is now the number of colors.
            IF (dims[0] LT 256) && (N_Elements(levels) EQ 0) && (N_Elements(nlevels) EQ 0) THEN BEGIN
               nlevels = dims[0]
            ENDIF
        ENDIF

       ; Get the color table vectors. Must do AFTER loading the palette, or
       ; PostScript can't be produced properly.
       TVLCT, rr, gg, bb, /GET
    ENDIF
    
    ; Check the color keywords.
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

    ; If color is the same as background, do something. Since this precludes drawing the the
    ; background color (perhaps you want to "erase" something), I offer an exception. If the
    ; COLOR is "Background", I am going to assume you know what you are doing!
    IF ColorsAreIdentical(background, color) THEN BEGIN
        IF ((!D.Flags AND 256) NE 0) THEN BEGIN
           IF (!P.Multi[0] EQ 0) && (~Keyword_Set(overplot) && ~noerase) THEN cgErase, background
        ENDIF
        IF (Size(color, /TNAME) EQ 'STRING') THEN BEGIN
            IF (StrUpCase(color) NE 'BACKGROUND') THEN color = 'OPPOSITE'
        ENDIF ELSE color = 'OPPOSITE'
    ENDIF
    IF ColorsAreIdentical(background, axiscolor) THEN BEGIN
        IF ((!D.Flags AND 256) NE 0) THEN BEGIN
           IF (!P.Multi[0] EQ 0) && (~Keyword_Set(overplot) && ~noerase) THEN cgErase, background
        ENDIF
        IF (Size(axiscolor, /TNAME) EQ 'STRING') THEN BEGIN
           IF (StrUpCase(axiscolor) NE 'BACKGROUND') THEN axiscolor = 'OPPOSITE'
        ENDIF ELSE axiscolor = 'OPPOSITE'
    ENDIF
    
    ; Default values for keywords.
    fill = Keyword_Set(fill)
    
    ; Really need CELL_FILL instead of FILL if you are overplotting onto a map projection.
    IF fill && ((N_Elements(map_object) NE 0) || (!X.Type EQ 3)) && Keyword_Set(overplot) THEN BEGIN
        fill = 0
        cell_fill = 1
    ENDIF
    cell_fill = Keyword_Set(cell_fill)
    irregular = Keyword_Set(irregular)
    SetDefaultValue, label, 1
    SetDefaultValue, resolution, [41,41]
    SetDefaultValue, outcolor, 'charcoal'
    outline = Keyword_Set(outline)
    IF N_Elements(nlevels) EQ 0 THEN BEGIN
        IF N_Elements(levels) EQ 0 THEN nlevels = 6 ELSE nlevels = N_Elements(levels)
    ENDIF    
    t3d = Keyword_Set(t3d)
    IF t3d THEN BEGIN
        IF N_Elements(zvalue) EQ 0 THEN zvalue = 0
        IF N_Elements(noclip) EQ 0 THEN noclip = 1
    ENDIF
    noclip = Keyword_Set(noclip)
    SetDefaultValue, xstyle, 1
    SetDefaultValue, ystyle, 1
    IF N_Elements(missingvalue) NE 0 THEN BEGIN
        IF  (Size(data, /TNAME) NE 'FLOAT') $
        AND (Size(data, /TNAME) NE 'DOUBLE') $
        THEN contourData = Float(data) ELSE contourData = data
        missingIndices = Where(contourData EQ missingValue[0], missingCount)
        IF missingCount GT 0 THEN BEGIN
            contourData[missingIndices] = !Values.F_NAN
            IF Keyword_Set(fill) THEN BEGIN
               fill = 0
               cell_fill = 1
            ENDIF
        ENDIF
    ENDIF ELSE contourData = data
    
    ; Handle gridding of irregular data.
    IF irregular THEN BEGIN
        Triangulate, xgrid, ygrid, triangles
        contourData = Trigrid(xgrid, ygrid, contourData, triangles, $
            NX=resolution[0], NY=resolution[1], $
            XGRID=xgrid, YGRID=ygrid, MISSING=!Values.F_NAN)
        IF Keyword_Set(fill) THEN BEGIN
           fill = 0
           cell_fill = 1
        ENDIF
     ENDIF
  
    ; Do you need to calculate levels.
    IF N_Elements(levels) EQ 0 THEN BEGIN
        minData = Min(contourData, /NAN, MAX=maxData)
        IF Size(minData, /TYPE) EQ 2 THEN minData = Float(minData)     ;Avoid 16 bit integer overflow
        levels = ((maxData - minData) / Float(nlevels)) * Indgen(nlevels) + minData
    ENDIF
    
    ; Need to make sure contour colors are integers if they are indices. 
    IF N_Elements(c_colors) NE 0 THEN BEGIN
        IF Size(c_colors, /TNAME) NE 'STRING' THEN BEGIN
           IF (Size(c_colors, /TYPE) EQ 3) && (Max(c_colors) LE 255) THEN BEGIN
                con_colors = Fix(c_colors)
           ENDIF ELSE BEGIN
                IF N_Elements(palette) NE 0 THEN BEGIN
                    TVLCT, palette
                    IF (!D.Name NE 'NULL') THEN TVLCT, rrr, ggg, bbb, /Get
                    rrr = Congrid(rrr, nlevels)
                    ggg = Congrid(ggg, nlevels)
                    bbb = Congrid(bbb, nlevels)
                    IF (!D.Name NE 'NULL') THEN TVLCT, rrr, ggg, bbb, 1
                    IF N_Elements(con_colors) EQ 0 THEN con_colors = StrTrim(Indgen(nlevels)+1,2)
                ENDIF ELSE BEGIN
                    con_colors = c_colors
                ENDELSE
           ENDELSE
        ENDIF ELSE con_colors = c_colors
    ENDIF ELSE BEGIN
        IF Keyword_Set(fill) OR Keyword_Set(cell_fill) THEN BEGIN
            IF N_Elements(palette) NE 0 THEN IF (!D.Name NE 'NULL') THEN TVLCT, palette
            IF (!D.Name NE 'NULL') THEN TVLCT, rrr, ggg, bbb, /Get
            rrr = Congrid(rrr, nlevels)
            ggg = Congrid(ggg, nlevels)
            bbb = Congrid(bbb, nlevels)
            IF (!D.Name NE 'NULL') THEN TVLCT, rrr, ggg, bbb, 1
            IF N_Elements(con_colors) EQ 0 THEN con_colors = StrTrim(Indgen(nlevels)+1,2)
        ENDIF ELSE BEGIN
            con_colors = Replicate(color, nlevels)
        ENDELSE
        IF Size(con_colors, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN con_colors = Byte(con_colors)
        IF Size(con_colors, /TYPE) LE 2 THEN con_colors = StrTrim(Fix(c_colors),2)
    ENDELSE

    ; Set up the appropriate contour labeling. Only can do if C_LABELS not passed in.
    IF N_Elements(c_labels) EQ 0 THEN BEGIN
        indices = Indgen(N_Elements(levels))
        IF label EQ 0 THEN BEGIN
           c_labels = Replicate(0,N_Elements(levels))
        ENDIF ELSE c_labels = Reverse((indices MOD label) EQ 0)
    ENDIF

    ; Load the drawing colors. If needed create a window first, so the drawing
    ; colors are correct for the window you want to draw into.
    IF ((!D.Flags AND 256) NE 0) && (!D.Window LT 0) THEN cgDisplay
    IF Size(axiscolor, /TNAME) EQ 'STRING' THEN axiscolor = cgColor(axiscolor)
    IF Size(color, /TNAME) EQ 'STRING' THEN color = cgColor(color)
    IF Size(background, /TNAME) EQ 'STRING' THEN background = cgColor(background)
    IF (Size(con_colors, /TYPE) LE 2) && (Size(con_colors, /TYPE) NE 0) THEN con_colors = StrTrim(Fix(con_colors),2)
    IF Size(con_colors, /TNAME) EQ 'STRING' THEN con_colors = cgColor(con_colors)
    
    ; Do you need a PostScript background color? Lot's of problems here!
    ; Basically, I MUST draw a plot to advance !P.MULTI. But, drawing a
    ; plot of any sort erases the background color. So, I have to draw a 
    ; plot, store the new system variables, then draw my background, etc.
    ; I have tried LOTS of options. This is the only one that worked.
    IF !D.Name EQ 'PS' THEN BEGIN
       IF Keyword_Set(noerase) EQ 0 THEN BEGIN
       
           ; I only have to do this, if this is the first plot.
           IF !P.MULTI[0] EQ 0 THEN BEGIN
           
                IF Keyword_Set(overplot) NE 1 THEN BEGIN

                    ; Save the current system variables. Will need to restore later.
                    bangx = !X
                    bangy = !Y
                    bangp = !P
 
                    ; Make sure axis are turned off. I don't really want to draw anything,
                    ; just advance !P.MULTI or "erase" the display for the next plot.
                    IF BitGet(xstyle, 2) NE 1 THEN xxstyle = xstyle + 4 ELSE xxstyle = xstyle
                    IF BitGet(ystyle, 2) NE 1 THEN yystyle = xstyle + 4 ELSE yystyle = ystyle
                    
                    ; Draw the plot that doesn't draw anything.
                     Contour, contourData, xgrid, ygrid, COLOR=axiscolor, CHARSIZE=charsize, $
                        BACKGROUND=background, LEVELS=levels, XSTYLE=xstyle, YSTYLE=xstyle, $
                        POSITION=position, _STRICT_EXTRA=extra, T3D=t3d, XTHICK=xthick, YTHICK=ythick, $
                        FONT=font, /NODATA, C_CHARSIZE=c_charsize, XTICKV=xtickv, XTICKS=xticks, $
                        YTICKV=ytickv, YTICKS=yticks, ZVALUE=zvalue, NOCLIP=noclip
                    
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
     
    ; Storing these system variable is *required* to make !P.MULTI work correctly
    ; when doing filled contours. Do not delete!
    bangx = !X
    bangy = !Y
    bangp = !P
    
    ; If you are not overploting, draw the contour plot now. Only the axes are
    ; drawn here, no data.
    IF Keyword_Set(overplot) EQ 0 THEN BEGIN

        Contour, contourData, xgrid, ygrid, COLOR=axiscolor, CHARSIZE=charsize, $
            BACKGROUND=background, LEVELS=levels, XSTYLE=xstyle, YSTYLE=ystyle, $
            POSITION=position, _STRICT_EXTRA=extra, T3D=t3d, /NODATA, NOERASE=tempNoErase, $
            XTHICK=xthick, YTHICK=ythick, FONT=font, C_CHARSIZE=c_charsize, $
            XTICKLEN=xticklen, YTICKLEN=yticklen, XTICKV=xtickv, XTICKS=xticks, $
            YTICKV=ytickv, YTICKS=yticks, ZVALUE=zvalue, NOCLIP=noclip, $
            TITLE=title, XTITLE=xtitle, YTITLE=ytitle
                    
    ENDIF
    
    ; This is where we actually draw the data. Check to see if we have a map object and need to
    ; convert the X and Y grid parameters from longitude/latitude to projected meter space
    ; before we do the drawing.
    IF (Keyword_Set(overplot) && (N_Elements(map_object) NE 0)) THEN BEGIN
    
        ; Do we have a valid map object?
        IF ~Obj_Isa(map_object, 'cgMap') THEN Message, 'The map object is not valid or not the correct type.'
        
        ; Are the X and Y grids the same size? If so, it makes things easy.
        IF (N_Elements(xgrid) EQ N_Elements(ygrid)) THEN BEGIN
           dims = Size(xgrid, /DIMENSIONS)
           n_dims = Size(xgrid, /N_DIMENSIONS)
           IF n_dims EQ 2 THEN BEGIN
              xgrid = Reform(xgrid, dims[0]*dims[1])
              ygrid = Reform(ygrid, dims[0]*dims[1])
           ENDIF
           xy = map_object -> Forward(xgrid, ygrid)
           xgrid = xy[0,*]
           ygrid = xy[1,*]
           IF n_dims EQ 2 THEN BEGIN
              xgrid = Reform(xgrid, dims[0], dims[1])
              ygrid = Reform(ygrid, dims[0], dims[1])
           ENDIF
        ENDIF ELSE BEGIN
           xsize = N_Elements(xgrid)
           ysize = N_Elements(ygrid)
           xgrid = Reform(Rebin(xgrid, xsize, ysize), xsize*ysize)
           ygrid = Reform(Rebin(Reform(ygrid, 1, ysize), xsize, ysize), xsize*ysize)
           xy = map_object -> Forward(xgrid, ygrid)
           xgrid = Reform(xy[0,*], xsize, ysize)
           ygrid = Reform(xy[1,*], xsize, ysize)
        ENDELSE
        
    ENDIF

    ; The actual levels are returned in the OLEVELS keyword.
    olevels = levels
    
    ; Draw the data on the axes.
    Contour, contourData, xgrid, ygrid, FILL=fill, CELL_FILL=cell_fill, COLOR=color, $
       LEVELS=levels, C_Labels=c_labels, C_COLORS=con_colors, XTHICK=xthick, YTHICK=ythick, $
       POSITION=position, XSTYLE=xstyle, YSTYLE=ystyle, _STRICT_EXTRA=extra, T3D=t3d, CHARSIZE=charsize, $
       FONT=font, /OVERPLOT, C_CHARSIZE=c_charsize, XTICKLEN=xticklen, YTICKLEN=yticklen, $
       XTICKV=xtickv, XTICKS=xticks, YTICKV=ytickv, YTICKS=yticks, ZVALUE=zvalue, NOCLIP=noclip, $
       C_ANNOTATION=c_annotation
       
    ; If this is a filled contour plot, and the OUTLINE keyword is set, then draw the contour
    ; outlines over the top of the data. 
    IF (fill || cell_fill) && outline THEN BEGIN
        Contour, contourData, xgrid, ygrid, COLOR=cgColor(outcolor), $
           LEVELS=levels, C_Labels=c_labels, XTHICK=xthick, YTHICK=ythick, $
           _STRICT_EXTRA=extra, T3D=t3d, CHARSIZE=charsize, $
           FONT=font, /OVERPLOT, C_CHARSIZE=c_charsize, XTICKLEN=xticklen, YTICKLEN=yticklen, $
           XTICKV=xtickv, XTICKS=xticks, YTICKV=ytickv, YTICKS=yticks, ZVALUE=zvalue, NOCLIP=noclip
    ENDIF
        
    ; If this is the first plot in PS, then we have to make it appear that we have
    ; drawn a plot, even though we haven't.
    IF N_Elements(psnodraw) EQ 1 THEN BEGIN
        !X = afterX
        !Y = afterY
        !P = afterP
    ENDIF
        
    ; If we filled the contour plot, we need to repair the axes. But, in multiple plot we
    ; have already advanced to the next position. We need to restore the position, do our
    ; repair work, and then advance to the next position again.
    IF (~Keyword_Set(overplot)) && (Keyword_Set(fill) || Keyword_Set(cell_fill)) THEN BEGIN 
        IF Total(!P.Multi) NE 0 THEN BEGIN
           fillx = !X
           filly = !Y
           fillp = !P
           !X = bangx
           !Y = bangy
           !P = bangP
        ENDIF
        Contour, contourData, xgrid, ygrid, COLOR=axiscolor, CHARSIZE=charsize, $
            BACKGROUND=background, LEVELS=levels, XSTYLE=xstyle, YSTYLE=ystyle, $
            POSITION=position, _STRICT_EXTRA=extra, T3D=t3d, /NODATA, $
            XTHICK=xthick, YTHICK=ythick, FONT=font, C_CHARSIZE=c_charsize, $
            XTICKLEN=xticklen, YTICKLEN=yticklen, XTICKV=xtickv, XTICKS=xticks, $
            YTICKV=ytickv, YTICKS=yticks, ZVALUE=zvalue, NOCLIP=noclip, /NOERASE
        IF Total(!P.Multi) NE 0 THEN BEGIN
           !X = fillx
           !Y = filly
           !P = fillp
        ENDIF
     ENDIF
    
    ; Restore the decomposed color state if you can.
    SetDecomposedState, currentState
    
    ; Restore the color table. Can't do this for the Z-buffer or
    ; the snap shot will be incorrect.
    IF (!D.Name NE 'Z') AND (!D.Name NE 'NULL') THEN BEGIN
        TVLCT, rr, gg, bb
        ; If you loaded a color palette, restore the before color vectors.
        IF N_Elements(p_red) NE 0 THEN TVLCT, p_red, p_grn, p_blu
    ENDIF
     
    ; Clean up if you are using a layout.
    IF N_Elements(layout) NE 0 THEN !P.Multi = thisMulti

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
        PS_END, DELETE_PS=delete_ps, $
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
    
END
    
