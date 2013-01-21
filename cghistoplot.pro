; docformat = 'rst'
;
; NAME:
;   cgHistoplot
;
; PURPOSE:
;   This program is used to draw a histogram plot in an IDL direct graphics window..
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
; This program is used to draw a histogram plot in an IDL direct graphics window..
; 
; .. image:: cghistoplot.png
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics
;    
; :Params:
;    data: in, required, 
;       The data from which the histogram is created.
;       
; :Keywords:
;    addcmd: in, optional, type=boolean, default=0
;       Set this keyword to add the command to the resizeable graphics window cgWindow.
;    axiscolorname: in, optional, type=string, default='opposite'                     
;       The name of the axis color. All color names are derived from cgColor.
;    backcolorname: in, optional, type=string, default='background'
;       The name of the background color. All color names are derived from cgColor.
;    binsize: in, optional
;       The binsize of the histogram. By default, Scott's Choice of bin size for histograms is used::
;                         
;           binsize = (3.5 * StdDev(data)) / N_Elements(data)^(0.3333)
;                            
;       If BINSIZE in not defined, and NBINS is defined, the BINSIZE is calcuated as::
;                         
;            binsize = (Max(dataToHistogram) - Min(dataToHistogram)) / (NBINS -1)
;                             
;       While it is pointed out in the HISTOGRAM documentation, it is extremely
;       important that the BINSIZE be of the same data type as the data you are going to
;       calculate the histogram of. If it is not VERY strange things can happen. I've
;       tried to protect you from most of the bad things, but I don't have a high confidence
;       level that I have done it for every situation. If you see something that "just don't
;       look right", I would check first to see if your data types match. That might solve
;       all your problems.
;    charsize: in, optional, type=float
;       The character size of the annotations. Default set by calling cgDefCharSize().
;    color: in, optional, type=string
;       Used as a shorthand way of setting the `DataColorName` keyword or, if the `FillPolygon`
;       keyword is set, the `PolyColor` keyword. Only used if those other two keywords are not
;       set.
;    datacolorname: in, optional, type=string, default="indian red"
;       The name of the data color for drawing the histogram outlines.
;    filename: in, optional, type=string
;       The name of a color name file to use with cgCOLOR.
;    fillpolygon: in, optional, type=boolean, default=0
;       Set this keyword to fill the histogram polygons with the `POLYCOLOR`.
;    frequency: in, optional, type=boolean, default=0
;       If this keyword is set, the relative frequency is plotted on the Y axis,
;       rather than the histogram density.
;    histdata: out, optional
;       The output value of the internal HISTOGRAM command.
;    l64: in, optional, type=boolean, default=0                       
;       If set, the return value of HISTOGRAM are 64-bit integers, rather than
;       the default 32-bit integers.
;    log: in, optional, type=boolean, default=0
;       Set this keyword if you wish the histogram count to be represented on a logarithmic scale.
;    layout: in, optional, type=integer
;       This keyword specifies a grid with a graphics window and determines 
;       where the graphic should appear. The syntax of LAYOUT is a 3-element 
;       array: [ncolumns, nrows, location]. The grid is determined by the 
;       number of columns (ncolumns) by the number of rows (nrows). The location 
;       of the graphic is determined by the third number. The grid numbering 
;       starts in the upper left (1) and goes sequentually by column and then by row.
;    line_fill: in, optional, type=boolean, default=0           
;       If set, the polygons are filled with lines instead of solid color. If
;       this keyword is set, the following keywords can also be used: `ORIENTATION`,
;       `PATTERN`, `POLYCOLOR`, and `SPACING`.
;    locations: out, optional
;       Starting locations of each bin. (See the HISTOGRAM documentation for details.)
;    maxinput: in, optional
;       The maximum value to use in calculating input histogram. Equivalent to the MAX keyword
;       in the HISTOGRAM documentation.
;    max_value: in, optional
;       The maximum Y data value to represent on graphics plot. Default: Max(data) * 1.05.
;    mininput: in, optional
;       The minimum value to use in calculating input histogram. Equivalent to the MIN keyword
;       in the HISTOGRAM documentation.
;    min_value: in, optional
;       The minimum Y data value to represent on graphics plot. Default: 0.
;    missing: in, optional
;       The value that should be represented as "missing" and not used in the histogram.
;       Be aware that if the input data is not of type "float" or "double" that the input
;       data will be converted to floating point prior to calculating the histogram.
;    nan: in, optional, type=boolean, default=0   
;       If set, ignore NAN values in calculating and plotting histogram.
;    nbins: in, optional, type=integer
;       The number of output bins in the histogram. Meaning is slightly different from
;       meaning in the HISTOGRAM command. Used only to calculate BINSIZE when BINSIZE is
;       not specified. In this case, binsize = rangeofData/(nbins-1).
;    omax: out, optional
;       The maximum output value used to construct the histogram. (See HISTOGRAM documentation.)
;    omin: out, optional
;       The minimum output value used to construct the histogram. (See HISTOGRAM documentation.)
;    oprobability: in, optional, type=boolean, default=0
;       Set this keyword if you want to overplot the cumulative probability on the plot.
;    oplot: in, optional, type=boolean, default=0
;       Set this keyword if you want to overplot the histogram on already established axes.
;    orientation: in, optional, type=float, default=0.0
;       The orientation (rotations) of the lines used to fill the polygons if `LINE_FILL` is set.
;       (See POLYFILL documentation.)
;    outfilename: in, optional, type=string
;       If the `Output` keyword is set, the user will be asked to supply an output
;       filename, unless this keyword is set to a non-null string. In that case, the
;       value of this keyword will be used as the filename and there will be no dialog
;       presented to the user.
;    outline: in, optional, type=boolean, default=0   
;       Set this keyword if you wish to draw only the outline of the histogram plot,
;       in a manner similar to setting PSYM=10 on a PLOT command.
;    output: in, optional, type=string, default=""
;       Set this keyword to the type of output desired. Possible values are these::
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
;       Or, you can simply set this keyword to the name of the output file, and the type of
;       file desired will be determined by the file extension. If you use this option, the
;       user will not be prompted to supply the name of the output file.
;            
;       All raster file output is created through PostScript intermediate files (the
;       PostScript files will be deleted), so ImageMagick and Ghostview MUST be installed 
;       to produce anything other than PostScript output. (See cgPS2PDF and PS_END for 
;       details.) And also note that you should NOT use this keyword when doing multiple 
;       plots. The keyword is to be used as a convenient way to get PostScript or raster 
;       output for a single graphics command. Output parameters can be set with cgWindow_SetDefs.
;    pattern: in, optional
;       The fill pattern for the polygons if the `FILLPOLYGON` keyword is set. (See POLYFILL documentation.)
;    polycolor: in, optional, type=string, default="rose"
;       The name of the polygon fill color if the `FILLPOLYGON` keyword is set.
;    position: in, optional, type=fltarr
;       The position of the plot axes in normalized data coordinates, [x0,y0,x1,y1].
;    probability_function: out, optional, type=float
;       The total cummulative probability of the histogram plot, scaled from 0 to 1.
;    probcolorname: in, optional, type=string, default="blue"                      
;       The name of the probability color for overplotting the cumulative probability
;       on the plot. 
;    probthick: in, optional, type=float, default=1.0
;       The thickness of the probability line drawn on the plot.
;    reverse_indices: out, optional
;       The list of reverse indices returned from the HISTOGRAM command. (See HISTOGRAM documentation.)
;    rotate: in, optional, type=boolean, default=0                     
;       Set this keyword to cause the histogram bins to be drawn from left to right, rather 
;       than from bottom to top.
;    smooth: in, optional, type=integer, default=0
;       Set this keyword to an odd positive integer to smooth the histogram output before plotting.
;       The integer will set the width of a smoothing box to be applied to the histogram data with
;       the Smooth function.
;    spacing: in, optional
;       The spacing of fill line if the 'LINE_FILL` keyword is set. (See POLYFILL documentation.)
;    thick: in, optional, type=integer, default=1   
;       Set this keyword to a value greater than 1 to draw thicker axes and lines.
;    title: in, optional, type=string
;        Set this keyword to the plot title.
;    window: in, optional, type=boolean, default=0
;        Set this keyword to replace all the commands in the current cgWindow or to
;        create a new cgWindow, if one doesn't currenly exist, for displaying this command.
;        To create a new cgWindow if one currenly exists, use the `cgWindow` command
;    xtitle: in, optional, type=string, default="Relative Frequency"
;        The X title of the histogram plot.
;    ytitle: in, optional, type=string, default="Histogram Density"
;        The Y title of the histogram plot.
;    _ref_extra: in, optional
;         Any additional PLOT commands are passed via keyword inheritance.
;          
; :Examples:
;    Some of the ways cgHistogram can be used::
;    
;       cgHistoplot, Dist(256), BINSIZE=5.0
;       cgHistoplot, Fix(RandomU(seed, 200)*20), POLYCOLOR=['charcoal', 'steel blue'], /FILLPOLYGON
;       cgHistoplot, Fix(RandomU(seed, 200)*20), POLYCOLOR=['navy', 'forest green'], /LINE_FILL, ORIENTATION=[45,-45]
;       
;    Additional examples can be found here::
;    
;        http://www.idlcoyote.com/graphics_tips/histoplot.php
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
;    Change History::
;       Written by:  David W. Fanning, 14 November 2007.
;       Modified to work with !P.MULTI. 20 Nov 2007. DWF.
;       Slight problem with extra space at the right end of the plot resolved. 20 Nov 2007. DWF.
;       Added FILE and NOLINES keywords. 24 Nov 2007. DWF.
;       Added additional HISTOGRAM access via keywords. 24 Nov 2007. DWF.
;       Fixed a small problem with FILLPOLY keyword. 26 Nov 2007. DWF.
;       Fixed a small problem with the OVERPLOTTED histogram being slightly offset. 12 Jan 2009. DWF
;       Major reconstructive surgery on the actual drawing part of the program. As far as I can
;          tell, all functionality is the same, but without drawing problems evidenced before. 14 March 2009. DWF.
;       A small problem with the way I was specifying ranges caused the bars to be slightly offset. 23 March 2009. DWF.
;       I fixed a small problem with BINSIZE and HISTOGRAM data type matching (see the Note), and I also
;          fixed a small problem with the range calculations when byte data is passed in. 1 April 2009. DWF.
;       I removed a NOLINES keyword, which was no longer being used. 1 April 2009. DWF.
;       MIN_VALUE and MAX_VALUE keywords now work again, thanks to Josiah Schwab. 22 April 2009. DWF.
;       Changed default POLYFILL color to "ROSE". 22 April 2009. DWF.
;       Having problems with binsize selection when data to histogram is an integer type. Fixed. 8 June 2009. DWF.
;       When the input array has a considerable number of NANs, the Histogram command complains with an error
;           "Array has too many elements." This happens even when the NAN keyword is set for the Histogram
;           command. So, I now screen for NANs before I process the histogram. 8 June 2009. DWF.
;       Added MISSING keyword to represent missing data. 18 July 2009. DWF.
;       Adding the MISSING keyword exposed a problem I had in restoring the original input data
;           to its original values when there were NANs and MISSING values. Fixed now by making
;           a copy of the data to work on internally. Everything else is too complex. 20 July 2009. DWF.
;       Yikes! Bad error in calculating start and end of histogram plot when overplotting fixed. 4 Sept 2009. DWF.
;       Added needed XSTYLE=1 to AXIS command to match Plot axis labelling. 19 Oct 2009. DWF.
;       Added a THICK keyword. 9 November 2009. DWF.
;       Added an OUTLINE keyword so only the outline of the histogram is plotted. 3 December 2009. DWF.
;       I was trying to be a good citizen by reloading the input color table when I exited
;            the program. But, of course, that makes it impossible to use the program in
;            the Z-buffer. Fixed by being less of a good citizen. 23 July 2010. DWF.
;       Because of the way I was dealing with NANs and MISSING data, the reverse indices were
;            inaccurate when they were returned, if there was NANs or MISSING data in the
;            input array. The data is now being handled correctly in all cases. This requires
;            that missing data must be set to !VALUES.F_NAN prior to calculating the histogram.
;            This means the data MUST be converted to floats for this operation. Since I am
;            always working on a *copy* of the data when this occurs, it should not affect
;            user input data. Also, I scan all input floating point and double data for NANs,
;            and if found, and the NAN keyword is not set, I issue a warning and set the NAN
;            keyword. This is a change in behavior. 1 October 2010. DWF.
;       Fixed a problem when specifying more than one POLYCOLOR. I made a change to the program
;            and forgot to propogate it everywhere. 4 October 2010. DWF.
;       Default axis color name changed from "Navy" to "Black". 28 October 2010. DWF.
;       Fixed a problem with restoring color tables in PostScript. 24 Nov 2010. DWF.
;       Added OPROBABILITY, PROBCOLOR, and PROBABILITY keywords. 24 Nov 2010. DWF.
;       Changed the way I find a default axis color. 3 Dec 2010. DWF.
;       Expanded search for "integers" from in BINSIZE calculation from DataType LE 3 
;             to include DataType GE 12, too. 8 Dec 2010. DWF.
;       Added WINDOW keyword. 24 Jan 2011. DWF.
;       Added ADDCMD keyword. 26 Jan 2011. DWF.
;       Added LAYOUT keyword. 28 Jan 2011. DWF.
;       Added CHARSIZE keyword. 2 Feb 2011. DWF.
;       Added YTITLE keyword. 9 May 2011. DWF.
;       Worked around a PLOT problem when setting the X axis range that caused the Y axis
;          range to be corrupted. 19 May 2011. DWF.
;       Added the ROTATE keyword. 18 Aug 2011. DWF.
;       I was calculating and displaying the cumulative probability distribution function
;           incorrectly. Now changed to what I think is the correct result. 8 Nov 2011. DWF.
;       Added the ability to send the output directly to a file via the OUTPUT keyword. 9 Dec 2011, DWF.
;       PostScript, PDF, and Imagemagick parameters can now be tailored with cgWindow_SetDefs. 14 Dec 2001. DWF.
;       I had a problem with OVERPLOTs being slightly offset because I was calculating the xrange
;           and yrange, rather than taking them from !X.CRange and !Y.CRange. 17 Dec 2011. DWF.
;       Modified to use cgDefaultColor for default color selection. 24 Dec 2011. DWF.
;       Incomplete implementation of new color selection scheme, fixed. 30 Dec 2011. DWF.
;       The change of 17 Dec 2011 was incorrect, as I misunderstood the problem. Restored original. 30 Dec 2011. DWF.
;       Changes to allow better default colors, based on changes to cgColor and cgDefaultColor. 1 Feb 2012. DWF.
;       Added a SMOOTH keyword. 26 April 2012. DWF.
;       Small fix (CR missing!) to allow overplotting in cgWindow. 26 April 2012. DWF.
;       The Outline keyword was incorrectly drawing the last histogram bin outline. Fixed. 26 April 2012. DWF.
;       Added POSITION and PROBTHICK keywords to set the plot position and the thickness of the cumulative
;          probability line, respectively. 25 May 2012. DWF.
;       If the cumulative probability option (keyword OPROBABILITY) is set, a second axis is drawn indicating
;          the cumulative probablity from 0 to 1. 25 May 2012. DWF.
;        Whoops! Don't want to set default position unless Total(!P.MULTI) equals zero. 25 May 2012. DWF.
;        More work on getting the cumulative probability to be correctly plotted. 30 May 2012. DWF.
;        More whoops! Setting POSITION now interfering with LAYOUT keyword. More fixes to restore LAYOUT. 26 July 2012. DWF.
;        Aaauuughhh! Typo introduced in yesterday's fix before I saved final version. 27 July 2012. DWF.
;        Added the ability to use escape characters in plot titles to specify cgSymbol symbols. 27 July 2012. DWF.
;        Mis-spelled "probability" in one section of the code. Fixed. 31 July 2012. DWF.
;        Added COLOR keyword. 19 Sept 2012. DWF.
;        Now restoring previous plot parameters after drawing cumulative probability axis, so as not
;           to interfere with subsequent overplotting. 27 Sept 2012. DWF.
;        Changed the way the "ystart" variable is set on log plots. 21 Jan 2013. DWF.
;         
; :Copyright:
;     Copyright (c) 2007-2012, Fanning Software Consulting, Inc.
;-
PRO cgHistoplot, $                  ; The program name.
   data, $                          ; The data to draw a histogram of.
   ADDCMD=addcmd, $                 ; Add this command to an cgWindow.
   AXISCOLORNAME=axisColorName, $   ; The axis color.
   BACKCOLORNAME=backcolorName, $   ; The background color.
   BINSIZE=binsize, $               ; The histogram bin size.
   CHARSIZE=charsize, $
   COLOR=color, $
   DATACOLORNAME=datacolorName, $   ; The data color.
   FILENAME=file, $                 ; For specifying a color name file.
   FILLPOLYGON=fillpolygon, $       ; Set if you want filled polygons
   FREQUENCY=frequency, $           ; Plot relative frequency, rather than density.
   HISTDATA=histdata, $
   L64=l64, $                       ; Input for HISTOGRAM.
   LAYOUT=layout, $                 ; Select the grid layout.
   LINE_FILL=line_fill, $           ; Set if you want line-filled polygons.
   LOCATIONS=locations, $
   LOG=log, $
   MAXINPUT=maxinput, $             ; The maximum value to HISTOGRAM.
   MAX_VALUE=max_value, $           ; The maximum value to plot.
   MIN_VALUE=min_value, $           ; The minimum value to plot.
   MININPUT=mininput, $             ; The minimum value to HISTOGRAM.
   MISSING=missing, $               ; The value that indicates "missing" data to be excluded from the histgram.
   NAN=nan, $                       ; Check for NAN.
   NBINS=nbins, $                   ; The number of bins to display.
   NOERASE=noerase, $               ; Set this keyword to avoid erasing when plot is drawn.
   OMAX=omax, $
   OMIN=omin, $
   OPLOT=overplot, $                ; Set if you want overplotting.
   OPROBABILITY=oprob, $            ; Overplot the cummulative probability distribution.
   ORIENTATION=orientation, $       ; The orientation of the lines.
   OUTFILENAME=outfilename, $       ; The name of the output file.
   OUTLINE=outline, $               ; Set this keyword if you wish to draw only the outline of the plot.
   OUTPUT=output, $                 ; The type of output file desired.
   PATTERN=pattern, $               ; The fill pattern.
   POLYCOLOR=polycolorname, $       ; The name of the polygon draw/fill color.
   POSITION=position, $             ; The position of the plot in the window in normalized coordinates.
   PROBABILITY_FUNCTION=probability, $
   PROBCOLORNAME=probColorName, $   ; The color for the probability plot, if it is used. By default, "blue".
   PROBTHICK=probthick, $           ; The thickness of the probability line.
   REVERSE_INDICES=ri, $
   ROTATE=rotate, $                 ; Rotate plot so histogram bars are drawn left to right.
   SMOOTH=smooth, $                 ; Run a smoothing filter of this width over the histogram data before plotting.
   SPACING=spacing, $               ; The spacing of filled lines.
   THICK=thick, $                   ; Set to draw thicker lines and axes.
   TITLE=title, $                   ; The plot title.
   WINDOW=window, $                 ; Display this in an cgWindow.
   XTITLE=xtitle, $                 ; The X title.
   YTITLE=ytitle, $                 ; The Y title.
    _REF_EXTRA=extra                ; For passing extra keywords.
    
   Compile_Opt idl2

   ; Catch any error in the cgHistoplot program.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(!Error_State.Msg + '. Returning...')
      IF N_Elements(nancount) EQ 0 THEN BEGIN
            IF N_Elements(_data) NE 0 THEN data = Temporary(_data)
      ENDIF ELSE BEGIN
            IF nancount EQ 0 THEN BEGIN
                IF N_Elements(_data) NE 0 THEN data = Temporary(_data)
            ENDIF
      ENDELSE
      IF N_Elements(thisMulti) NE 0 THEN !P.Multi = thisMulti
      RETURN
   ENDIF

    ; Should this be added to a resizeable graphics window?
    IF Keyword_Set(addcmd) THEN window = 1
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        ; If you are using a layout, you can't ever erase.
        IF N_Elements(layout) NE 0 THEN noerase = 1
        
        ; Have to do something different if we are overplotting or adding a command.
        IF Keyword_Set(overplot) OR Keyword_Set(addcmd) THEN BEGIN
            cgWindow, 'cgHistoplot', $          ; The program name.
               data, $                          ; The data to draw a histogram of.
               AXISCOLORNAME=axisColorName, $   ; The axis color.
               BACKCOLORNAME=backcolorName, $   ; The background color.
               CHARSIZE=charsize, $
               COLOR=color, $
               DATACOLORNAME=datacolorName, $   ; The data color.
               _EXTRA=extra, $                  ; For passing extra keywords.
               FILENAME=file, $                 ; For specifying a color name file.
               FREQUENCY=frequency, $           ; Plot relative frequency, rather than density.
               LAYOUT=layout, $
               LOG=log, $
               MAX_VALUE=max_value, $           ; The maximum value to plot.
               MIN_VALUE=min_value, $           ; The minimum value to plot.
               MISSING=missing, $               ; The value that indicates "missing" data to be excluded from the histgram.
               NOERASE=noerase, $               ; Set this keyword to avoid erasing when plot is drawn. 
               OPLOT=overplot, $
               OPROBABILITY=oprob, $            ; Overplot the cummulative probability distribution.
               OUTLINE=outline, $               ; Set this keyword if you wish to draw only the outline of the plot.
               POSITION=position, $             ; The position of the plot in the window in normalized coordinates.
               PROBCOLORNAME=probColorName, $   ; The color for the probability plot, if it is used. By default, "blue".
               PROBTHICK=probthick, $           ; The thickness of the probability line.
               ROTATE=rotate, $
               SMOOTH=smooth, $
               THICK=thick, $                   ; Set to draw thicker lines and axes.
               TITLE=title, $
               XTITLE=xtitle, $
               YTITLE=ytitle, $                 ; The Y title.
               ;
               ; POLYFILL KEYWORDS
               ;
               FILLPOLYGON=fillpolygon, $       ; Set if you want filled polygons
               LINE_FILL=line_fill, $           ; Set if you want line-filled polygons.
               ORIENTATION=orientation, $       ; The orientation of the lines.
               PATTERN=pattern, $               ; The fill pattern.
               POLYCOLOR=polycolorname, $           ; The name of the polygon draw/fill color.
               SPACING=spacing, $               ; The spacing of filled lines.
               ;
               ; HISTOGRAM OUTPUT KEYWORDS
               ;
               HISTDATA=histdata, $
               LOCATIONS=locations, $
               OMAX=omax, $
               OMIN=omin, $
               PROBABILITY_FUNCTION=probability, $
               REVERSE_INDICES=ri, $
               ;
               ; HISTOGRAM INPUT KEYWORDS
               ;
               BINSIZE=binsize, $               ; The histogram bin size.
               L64=l64, $                       ; Input for HISTOGRAM.
               MAXINPUT=maxinput, $             ; The maximum value to HISTOGRAM.
               MININPUT=mininput, $             ; The minimum value to HISTOGRAM.
               NAN=nan, $                       ; Check for NAN.
               NBINS=nbins, $                   ; The number of bins to display.
               ADDCMD=1
            RETURN
        ENDIF 
        
        ; Otherwise, we are just replacing the commands in a new or existing window.
            void = cgQuery(COUNT=wincnt)
            IF wincnt EQ 0 THEN replaceCmd=0 ELSE replaceCmd=1
            cgWindow, 'cgHistoplot', $          ; The program name.
               data, $               ; The data to draw a histogram of.
               AXISCOLORNAME=axisColorName, $   ; The axis color.
               BACKCOLORNAME=backcolorName, $   ; The background color.
               CHARSIZE=charsize, $
               COLOR=color, $
               DATACOLORNAME=datacolorName, $   ; The data color.
               _EXTRA=extra, $                  ; For passing extra keywords.
               FILENAME=file, $                 ; For specifying a color name file.
               FREQUENCY=frequency, $           ; Plot relative frequency, rather than density.
               LAYOUT=layout, $
               LOG=log, $
               MAX_VALUE=max_value, $           ; The maximum value to plot.
               MIN_VALUE=min_value, $           ; The minimum value to plot.
               MISSING=missing, $               ; The value that indicates "missing" data to be excluded from the histgram.
               NOERASE=noerase, $               ; Set this keyword to avoid erasing when plot is drawn.               OPLOT=overplot, $                ; Set if you want overplotting.
               OPLOT=overplot, $                ; Set if you want overplotting.
               OPROBABILITY=oprob, $            ; Overplot the cummulative probability distribution.
               OUTLINE=outline, $               ; Set this keyword if you wish to draw only the outline of the plot.
               POSITION=position, $             ; The position of the plot in the window in normalized coordinates.
               PROBCOLORNAME=probColorName, $   ; The color for the probability plot, if it is used. By default, "blue".
               PROBTHICK=probthick, $           ; The thickness of the probability line.
               ROTATE=rotate, $
               SMOOTH=smooth, $
               THICK=thick, $                   ; Set to draw thicker lines and axes.
               TITLE=title, $
               XTITLE=xtitle, $
               YTITLE=ytitle, $                 ; The Y title.
               ;
               ; POLYFILL KEYWORDS
               ;
               FILLPOLYGON=fillpolygon, $       ; Set if you want filled polygons
               LINE_FILL=line_fill, $           ; Set if you want line-filled polygons.
               ORIENTATION=orientation, $       ; The orientation of the lines.
               PATTERN=pattern, $               ; The fill pattern.
               POLYCOLOR=polycolorname, $           ; The name of the polygon draw/fill color.
               SPACING=spacing, $               ; The spacing of filled lines.
               ;
               ; HISTOGRAM OUTPUT KEYWORDS
               ;
               HISTDATA=histdata, $
               LOCATIONS=locations, $
               OMAX=omax, $
               OMIN=omin, $
               PROBABILITY_FUNCTION=probability, $
               REVERSE_INDICES=ri, $
               ;
               ; HISTOGRAM INPUT KEYWORDS
               ;
               BINSIZE=binsize, $               ; The histogram bin size.
               L64=l64, $                       ; Input for HISTOGRAM.
               MAXINPUT=maxinput, $             ; The maximum value to HISTOGRAM.
               MININPUT=mininput, $             ; The minimum value to HISTOGRAM.
               NAN=nan, $                       ; Check for NAN.
               NBINS=nbins, $                   ; The number of bins to display.
               REPLACECMD=replaceCmd
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
   
   ; Set up PostScript device for working with colors.
   IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
   ; Check for parameters.
   IF N_Elements(color) NE 0 THEN BEGIN
       IF Keyword_Set(fillpolygon) && (N_Elements(polycolorname) EQ 0) THEN polycolorname = color
       IF ~Keyword_Set(fillpolygon) && (N_Elements(datacolorname) EQ 0) THEN datacolorname = color
   ENDIF
   IF N_Elements(data) EQ 0 THEN Message, 'Must pass data to histogram.'
   IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharSize()
   IF N_Elements(smooth) NE 0 THEN BEGIN
     IF (smooth MOD 2) NE 0 THEN smooth = smooth + 1
   ENDIF
   IF N_Elements(position) EQ 0 && (Total(!P.Multi) LE 0) && (Total(!P.Position) EQ 0.0) THEN BEGIN
       IF Keyword_Set(oprob) THEN BEGIN
          IF Keyword_Set(rotate) THEN BEGIN
             position = [0.125, 0.125, 0.925, 0.875]
          ENDIF ELSE BEGIN
             position = [0.175, 0.125, 0.875, 0.925]
          ENDELSE
       ENDIF ELSE BEGIN
          IF Keyword_Set(rotate) THEN BEGIN
             position = [0.125, 0.15, 0.925, 0.925]
          ENDIF ELSE BEGIN
             position = [0.175, 0.125, 0.925, 0.875]
          ENDELSE
       ENDELSE
   ENDIF
   
   ; What kind of data are we doing a HISTOGRAM on?
   dataType = Size(data, /TYPE)
      
   ; Check the data for NANs and alert the user if the NAN keyword is not set.
   IF dataType EQ 4 OR datatype EQ 5 THEN BEGIN
        goodIndices = Where(Finite(data), count, NCOMPLEMENT=nancount, COMPLEMENT=nanIndices)
        IF nancount GT 0 THEN BEGIN
           IF ~Keyword_Set(nan) THEN BEGIN
               Message, 'NANs found in the data. NAN keyword is set to 1.', /INFORMATIONAL
               nan = 1
           ENDIF
        ENDIF 
   ENDIF 
   
   ; The only sensible way to proceed is to make a copy of the data. Otherwise, I'll have
   ; a devil of a time putting it back together again at the end. There is a bug in
   ; HISTOGRAM when using BYTE data, so convert that here
   IF N_Elements(_data) EQ 0 THEN BEGIN
      IF Size(data, /TNAME) EQ 'BYTE' THEN BEGIN
          _data = Fix(data) 
       ENDIF ELSE BEGIN
          _data = data
       ENDELSE
   ENDIF
   
   ; If you have any "missing" data, then the data needs to be converted to float
   ; and the missing data set to F_NAN.
   IF N_Elements(missing) NE 0 THEN BEGIN
      missingIndices = Where(_data EQ missing, missingCount)
      IF missingCount GT 0 THEN BEGIN
         CASE datatype OF
            4: _data[missingIndices] = !Values.F_NAN
            5: _data[missingIndices] = !Values.D_NAN
            ELSE: BEGIN
                _data = Float(_data)
                dataType = 4
                _data[missingIndices] = !Values.F_NAN
                END
         ENDCASE
         nan = 1
      ENDIF ELSE BEGIN
        IF missingCount EQ N_Elements(_data) THEN $
            Message, 'All values are "missing"!'
      ENDELSE
   ENDIF
   
   ; Check for histogram keywords.
   IF N_Elements(binsize) EQ 0 THEN BEGIN
      range = Max(_data, /NAN) - Min(_data, /NAN)
      IF N_Elements(nbins) EQ 0 THEN BEGIN  ; Scott's Choice
         binsize = (3.5D * StdDev(_data, /NAN))/N_Elements(_data)^(1./3.0D) 
         IF (dataType LE 3) OR (dataType GE 12) THEN binsize = Round(binsize) > 1
         binsize = Convert_To_Type(binsize, dataType)
      ENDIF ELSE BEGIN
         binsize = range / (nbins -1)
         IF dataType LE 3 THEN binsize = Round(binsize) > 1
         binsize = Convert_To_Type(binsize, dataType)
      ENDELSE
   ENDIF ELSE BEGIN
       IF Size(binsize, /TYPE) NE dataType THEN BEGIN
          IF dataType LE 3 THEN binsize = Round(binsize) > 1
          binsize = Convert_To_Type(binsize, dataType)
       ENDIF
   ENDELSE

   ; Check for keywords.
   IF N_Elements(backColorName) EQ 0 THEN backColorName = "background"
   IF N_Elements(dataColorName) EQ 0 THEN dataColorName = "Indian Red"
   SetDefaultValue, title, ""
   
    ; Set up the layout, if necessary.
    IF N_Elements(layout) NE 0 THEN BEGIN
       IF N_Elements(position) NE 0 THEN Undefine, position
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

   ; Choose an axis color.
   IF N_Elements(axisColorName) EQ 0 AND N_Elements(saxescolor) NE 0 THEN axisColorName = saxescolor
   axisColorName = cgDefaultColor(axisColorName, DEFAULT='opposite')
   IF N_Elements(polycolorname) EQ 0 THEN polycolorname = "Rose"
   IF N_Elements(probColorname) EQ 0 THEN probColorname = "Blue"
   frequency = Keyword_Set(frequency)
   line_fill = Keyword_Set(line_fill)
   IF line_fill THEN fillpolygon = 1
   log = Keyword_Set(log)
   fillpolygon = Keyword_Set(fillpolygon)
   IF fillpolygon THEN BEGIN
      IF N_Elements(orientation) EQ 0 THEN orientation = 0
      IF N_Elements(spacing) EQ 0 THEN spacing = 0
   ENDIF
   IF N_Elements(mininput) EQ 0 THEN mininput = Min(_data, NAN=nan)
   IF N_Elements(maxinput) EQ 0 THEN maxinput = Max(_data, NAN=nan)
   IF N_Elements(thick) EQ 0 THEN thick = 1.0

   ; Do this in decomposed color, if possible.
   SetDecomposedState, 1, CURRENT=currentState
   
   ; Load plot colors.
   TVLCT, r, g, b, /GET
   
   ; If needed create a window first, so the drawing
   ; colors are correct for the window you want to draw into.
   IF ((!D.Flags AND 256) NE 0) && (!D.Window LT 0) && ~Keyword_Set(overplot) THEN cgDisplay
   axisColor = cgColor(axisColorName, FILE=file)
   dataColor = cgColor(datacolorName, FILE=file)
   backColor = cgColor(backColorName, FILE=file)
   polyColor = cgColor(polyColorName, FILE=file)
   probColor = cgColor(probColorName, FILE=file)

   ; Set up some labels.
   IF frequency THEN BEGIN
      IF Keyword_Set(rotate) THEN BEGIN
          IF N_Elements(xtitle) EQ 0 THEN xtitle = 'Relative Frequency'
          xtickformat = '(F6.4)'
      ENDIF ELSE BEGIN
          IF N_Elements(ytitle) EQ 0 THEN ytitle = 'Relative Frequency'
          ytickformat = '(F6.4)'
      ENDELSE
   ENDIF ELSE BEGIN
      IF Keyword_Set(rotate) THEN BEGIN
          IF N_Elements(xtitle) EQ 0 THEN xtitle = 'Histogram Density'
          xtickformat = '(I)'
      ENDIF ELSE BEGIN
          IF N_Elements(ytitle) EQ 0 THEN ytitle = 'Histogram Density'
          ytickformat = '(I)'
      ENDELSE
   ENDELSE
   
   ; Check for symbols in titles.
   IF N_Elements(title) NE 0 THEN title = cgCheckForSymbols(title)
   IF N_Elements(xtitle) NE 0 THEN xtitle = cgCheckForSymbols(xtitle)
   IF N_Elements(ytitle) NE 0 THEN ytitle = cgCheckForSymbols(ytitle)
   
   
   ; Calculate the histogram.
    histdata = Histogram(_data, $
      BINSIZE=binsize, $
      L64=l64, $
      MAX=maxinput, $
      MIN=mininput, $
      NAN=nan, $
      LOCATIONS=locations, $
      OMAX=omax, $
      OMIN=omin, $
      REVERSE_INDICES=ri)

   ; Do you need to smooth the data?
   IF N_Elements(smooth) NE 0 THEN histdata = Smooth(histdata, smooth)
   
   ; Are you plotting the frequency rather than the count?
   IF frequency THEN histdata = Float(histdata)/N_Elements(_data)
   
   ; Need a probability distribution?
   IF Arg_Present(probability) OR Keyword_Set(oprob) THEN BEGIN
       cumTotal = Total(histData, /CUMULATIVE)
       probability = Total(Double(histdata)/Total(Double(histdata)), /CUMULATIVE)
   ENDIF

   ; Calculate the range of the plot output.
   IF N_Elements(min_value) EQ 0 THEN BEGIN
      IF Keyword_Set(log) THEN min_value = 1e-3 ELSE min_value = 0
   ENDIF
   IF N_Elements(max_value) EQ 0 THEN BEGIN
      IF Keyword_Set(log) $
        THEN max_value = Max(histData) * 1.25 $
        ELSE max_value = Max(histData) * 1.05
   ENDIF
   IF Keyword_Set(rotate) THEN BEGIN
       xmin = min_value
       xmax = max_value
       ymin = Double(omin) - binsize
       ymax = Double(omax) + (binsize * 1.5)
   ENDIF ELSE BEGIN
       ymin = min_value
       ymax = max_value
       xmin = Double(omin) - binsize
       xmax = Double(omax) + (binsize * 1.5)
   ENDELSE
   
   ; Save the current system variables, if doing multiple plots.
   IF Total(!P.MULTI) NE 0 THEN BEGIN
      bangp = !P
      bangx = !X
      bangy = !Y
      bangmap = !MAP
   ENDIF
   
   
   ; Unless we are overplotting, draw the plot to establish a data coordinate system.
   ; Don't actually display anything yet, because we may have to repair damage caused
   ; by polygon filling.
   xrange = [xmin, xmax]
   yrange = [ymin, ymax]
   IF ~Keyword_Set(overplot) THEN BEGIN
       IF Keyword_Set(rotate) THEN BEGIN
          Plot, [0,0], xrange=xrange, yrange=yrange, $             
             Background=backColor, $
             Color=axisColor, $                       ; The color of the axes.
             Charsize=charsize, $
             XLOG=log, $
             NoData=1, $                              ; Draw the axes only. No data.
             NOERASE=noerase, $
             POSITION=position, $
             TITLE=title, $
             XTHICK=thick, $                          ; Axes thicker, if needed.
             YTHICK=thick, $
             XStyle=5, $                              ; Exact axis scaling. No autoscaled axes.
             YMinor=0, $                              ; No minor tick mark on X axis.
             YStyle=5, $                              ; Exact axis scaling. No autoscaled axes.
             XTickformat='(A1)', $                    ; No format. Nothing drawn
             YTickformat='(A1)', $                    ; No format. Nothing drawn
             _Strict_Extra=extra                      ; Pass any extra PLOT keywords.
       ENDIF ELSE BEGIN
          Plot, [0,0], xrange=xrange, yrange=yrange, $             
             Background=backColor, $
             Color=axisColor, $                       ; The color of the axes.
             Charsize=charsize, $
             YLOG=log, $
             NoData=1, $                              ; Draw the axes only. No data.
             NOERASE=noerase, $
             POSITION=position, $
             TITLE=title, $
             XTHICK=thick, $                          ; Axes thicker, if needed.
             YTHICK=thick, $
             XStyle=5, $                              ; Exact axis scaling. No autoscaled axes.
             YMinor=0, $                              ; No minor tick mark on X axis.
             YStyle=5, $                              ; Exact axis scaling. No autoscaled axes.
             XTickformat='(A1)', $                    ; No format. Nothing drawn
             YTickformat='(A1)', $                    ; No format. Nothing drawn
             _Strict_Extra=extra                      ; Pass any extra PLOT keywords.
        ENDELSE
   ENDIF

   ; Save the after-plot system variables, if doing multiple plots.
   ; You will need it to advance the plots in !P.MULTI, since you draw
   ; the plots with NOERASE.
   IF Total(!P.MULTI) NE 0 THEN BEGIN
       bangAfterp = !P
       bangAfterx = !X
       bangAftery = !Y
       bangAftermap = !MAP
   ENDIF

   ; Do we need to have things be filled?
   IF Keyword_Set(fillpolygon) THEN BEGIN

       ncolors = N_Elements(polycolor)

      ; Are we line filling?
      IF line_fill THEN BEGIN

         norient = N_Elements(orientation)
         nspace = N_Elements(spacing)
         step = (xrange[1] - xrange[0]) / (binsize + 1)
         IF Keyword_Set(rotate) THEN BEGIN
            start = yrange[0] + binsize
         ENDIF ELSE BEGIN
            start = xrange[0] + binsize
         ENDELSE

         endpt = start + binsize
         FOR j=0,N_Elements(histdata)-1 DO BEGIN
            IF Keyword_Set(rotate) THEN BEGIN
               y = [start, start, endpt, endpt, start]
               IF log THEN BEGIN
                  x = [1, histdata[j], histdata[j], 1, 1]
               ENDIF ELSE BEGIN
                  x = [0, histdata[j], histdata[j], 0, 0]
               ENDELSE
            ENDIF ELSE BEGIN
               x = [start, start, endpt, endpt, start]
               IF log THEN BEGIN
                  y = [1, histdata[j], histdata[j], 1, 1]
               ENDIF ELSE BEGIN
                  y = [0, histdata[j], histdata[j], 0, 0]
               ENDELSE
            ENDELSE
            fillcolor = polycolor[j MOD ncolors]
            orient = orientation[j MOD norient]
            space = spacing[j MOD nspace]
            PolyFill, x, y, COLOR=fillColor, /LINE_FILL, ORIENTATION=orient, $
               PATTERN=pattern, SPACING=space, NOCLIP=0
            start = start + binsize
            endpt = start + binsize
         ENDFOR

      ENDIF ELSE BEGIN ; Normal polygon color fill.

         step = (xrange[1] - xrange[0]) / (binsize + 1)
         IF Keyword_Set(rotate) THEN BEGIN
            start = yrange[0] + binsize
         ENDIF ELSE BEGIN
            start = xrange[0] + binsize
         ENDELSE
         endpt = start + binsize
         FOR j=0,N_Elements(histdata)-1 DO BEGIN
            IF Keyword_Set(rotate) THEN BEGIN
               y = [start, start, endpt, endpt, start]
               IF log THEN BEGIN
                   x = [!X.CRange[0], histdata[j], histdata[j], !X.CRange[0], !X.CRange[0]]
               ENDIF ELSE BEGIN
                   x = [0, histdata[j], histdata[j], 0, 0]
               ENDELSE
            ENDIF ELSE BEGIN
               x = [start, start, endpt, endpt, start]
               IF log THEN BEGIN
                  y = [!Y.CRange[0], histdata[j], histdata[j], !Y.CRange[0], !Y.CRange[0]]
               ENDIF ELSE BEGIN
                  y = [0, histdata[j], histdata[j], 0, 0]
               ENDELSE
            ENDELSE
            fillcolor = polycolor[j MOD ncolors]
            PolyFill, x, y, COLOR=fillColor, NOCLIP=0
            start = start + binsize
            endpt = start + binsize
         ENDFOR

      ENDELSE
   ENDIF
      
   ; Restore the pre-plot system variables.
   IF Total(!P.MULTI) NE 0 THEN BEGIN
       !P = bangp
       !X = bangx
       !Y = bangy
       !MAP = bangmap
    ENDIF

   IF ~Keyword_Set(overplot) THEN BEGIN
       xrange = [xmin, xmax]
       yrange = [ymin, ymax]
       IF Keyword_Set(oprob) THEN BEGIN
          IF Keyword_Set(rotate) THEN BEGIN
             xstyle = 9
             ystyle = 9
          ENDIF ELSE BEGIN
             xstyle = 9
             ystyle = 9
          ENDELSE
       ENDIF ELSE BEGIN
          IF Keyword_Set(rotate) THEN BEGIN
             xstyle = 1
             ystyle = 9
          ENDIF ELSE BEGIN
             xstyle = 9
             ystyle = 1
          ENDELSE
       ENDELSE
       IF Keyword_Set(rotate) THEN BEGIN
       Plot, [0,0], xrange=xrange, yrange=yrange, $             
             Background=backColor, $
             Charsize=charsize, $
             Color=axisColor, $                       ; The color of the axes.
             POSITION=position, $
             XLOG=log, $
             NoData=1, $                              ; Draw the axes only. No data.
             XThick=thick, $  
             YThick=thick, $
             YStyle=ystyle, $                              ; Exact axis scaling. No autoscaled axes.
             XMinor=1, $                              ; No minor tick mark on X axis.
             XStyle=xstyle, $                              ; Exact axis scaling. No autoscaled axes.
             XTickformat=xtickformat, $               ; Y Tickformat
             YTickformat=ytickformat, $
             XTickV=tickV, $
             XTicks = ticks, $
             XTitle=xtitle, $                         ; Y Title
             YTitle=ytitle, $
             NoErase=1, $
             YTicklen=-0.025, $
             _Strict_Extra=extra                      ; Pass any extra PLOT keywords.
       ENDIF ELSE BEGIN
       Plot, [0,0], xrange=xrange, yrange=yrange, $             
             Background=backColor, $
             Charsize=charsize, $
             Color=axisColor, $                       ; The color of the axes.
             POSITION=position, $
             YLOG=log, $
             NoData=1, $                              ; Draw the axes only. No data.
             XThick=thick, $  
             YThick=thick, $
             XStyle=xstyle, $                              ; Exact axis scaling. No autoscaled axes.
             YMinor=1, $                              ; No minor tick mark on X axis.
             YStyle=ystyle, $                              ; Exact axis scaling. No autoscaled axes.
             XTickformat=xtickformat, $               ; Y Tickformat
             YTickformat=ytickformat, $
             YTickV=tickv, $
             YTicks = ticks, $
             XTitle=xtitle, $                         ; Y Title
             YTitle=ytitle, $
             NoErase=1, $
             XTicklen=-0.025, $
             _Strict_Extra=extra                      ; Pass any extra PLOT keywords.
        ENDELSE
             
        ; Repair the damage caused by polygon filling.
        IF Keyword_Set(rotate) THEN BEGIN
            IF log THEN BEGIN
                Axis, 10^!X.CRange[1], 10^!Y.CRange[0], YAXIS=1, YTickformat='(A1)', YMINOR=1, $
                    COLOR=axisColor, YSTYLE=1, YTHICK=thick, CHARSIZE=charsize, XLOG=1
            ENDIF ELSE BEGIN
                Axis, !X.CRange[1], !Y.CRange[0], YAXIS=1, YTickformat='(A1)', YMINOR=1, $
                    COLOR=axisColor, YSTYLE=1, YTHICK=thick, CHARSIZE=charsize
            ENDELSE
        ENDIF ELSE BEGIN
            IF log THEN BEGIN
            Axis, 10^!X.CRange[0], 10^!Y.CRange[1], XAXIS=1, XTickformat='(A1)', XMINOR=1, $
                COLOR=axisColor, XSTYLE=1, XTHICK=thick, CHARSIZE=charsize, /YLOG
            ENDIF ELSE BEGIN
            Axis, !X.CRange[0], !Y.CRange[1], XAXIS=1, XTickformat='(A1)', XMINOR=1, $
                COLOR=axisColor, XSTYLE=1, XTHICK=thick, CHARSIZE=charsize
            ENDELSE
        ENDELSE

   ENDIF
    
    step = (xrange[1] - xrange[0]) / (binsize + 1)
    IF Keyword_Set(rotate) THEN BEGIN
        start = yrange[0] + binsize
    ENDIF ELSE BEGIN
        start = xrange[0] + binsize
    ENDELSE
    endpt = start + binsize
    IF log THEN BEGIN
       IF Keyword_Set(rotate) THEN ystart = 10^!X.CRange[0] ELSE ystart = 10^!Y.CRange[0] 
    ENDIF ELSE ystart = 0
    jend = N_Elements(histdata)-1
    FOR j=0,jend DO BEGIN
        IF Keyword_Set(outline) THEN BEGIN
           IF Keyword_Set(rotate) THEN BEGIN
               Plots, [ystart, histdata[j]], [start, start], COLOR=dataColor, THICK=thick, NOCLIP=0
               Plots, [histdata[j], histdata[j]], [start, endpt], COLOR=dataColor, THICK=thick, NOCLIP=0
               IF j EQ jend THEN BEGIN
                  Plots, [histdata[j], xrange[0]], [endpt, endpt], COLOR=dataColor, THICK=thick, NOCLIP=0
               ENDIF
           ENDIF ELSE BEGIN
               Plots, [start, start], [ystart, histdata[j]], COLOR=dataColor, THICK=thick, NOCLIP=0
               Plots, [start, endpt], [histdata[j], histdata[j]], COLOR=dataColor, THICK=thick, NOCLIP=0
               IF j EQ jend THEN BEGIN
                  Plots, [endpt, endpt], [yrange[0], histdata[j]], COLOR=dataColor, THICK=thick, NOCLIP=0
               ENDIF
           ENDELSE
           start = start + binsize
           endpt = start + binsize
           ystart = histdata[j]
        ENDIF ELSE BEGIN
           x = [start, start, endpt, endpt, start]
           y = [ystart, histdata[j], histdata[j], ystart, ystart]
           IF Keyword_Set(rotate) THEN BEGIN
              PLOTS, y, x, COLOR=dataColor, NOCLIP=0, THICK=thick
           ENDIF ELSE BEGIN
              PLOTS, x, y, COLOR=dataColor, NOCLIP=0, THICK=thick
           ENDELSE
           start = start + binsize
           endpt = start + binsize
        ENDELSE
    ENDFOR
   
   ; Need to overplot probability function?
   IF Keyword_Set(oprob) THEN BEGIN
   
        ; Save the current axes system variable so they can be restored after
        ; the axis is drawn.
        xsave = !X
        ysave = !Y
        
        ; If you are plotting the probability plot, label the axes appropriately.
        IF Keyword_Set(overplot) THEN style = 5 ELSE style = 1
        IF Keyword_Set(rotate) THEN BEGIN
            Axis, !X.CRange[0], !Y.CRange[1], XAXIS=1, COLOR=axisColor, /SAVE, $
                 XSTYLE=style, XTHICK=thick, CHARSIZE=charsize, XRANGE=[0.0,1.0005], XTITLE='Cumulative Probability'
        ENDIF ELSE BEGIN
            Axis, !X.CRange[1], !Y.CRange[0], YAXIS=1, YMINOR=1, COLOR=axisColor, /SAVE, $
                 YSTYLE=style, YTHICK=thick, CHARSIZE=charsize, YRANGE=[0.0,1.0005], YTITLE='Cumulative Probability'
        ENDELSE
        IF N_Elements(probthick) EQ 0 THEN probthick = (!D.Name NE 'PS') ? 1.0 : 3.0
        IF Keyword_Set(rotate) THEN BEGIN
            proby = locations + (binsize/2.0)
            Oplot, probability, proby, COLOR=probcolor, THICK=probthick;, PSYM=2
        ENDIF ELSE BEGIN
            probx = locations + (binsize/2.0)
            Oplot, probx, probability, COLOR=probcolor, THICK=probthick;, PSYM=2
        ENDELSE
        
        ; Restore the axes system variables.
        !X = xsave
        !Y = ysave
        
   ENDIF

   ; Advance the plot for !P.Multi purposes.
   IF Total(!P.MULTI) NE 0 THEN BEGIN
       !P = bangAfterp 
       !X = bangAfterx 
       !Y = bangAftery
       !MAP = bangAftermap
   ENDIF

   ; Clean up. But you really can't do this in the Z-buffer. 
   IF !D.Name NE 'Z' THEN TVLCT, r, g, b
   
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
    
    ; Clean up.
    SetDecomposedState, currentState
END
