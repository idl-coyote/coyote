;+
; NAME:
;       HISTOPLOT
;
; PURPOSE:
;
;       This program is used to draw a histogram in an IDL direct graphics window.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Graphics
;
; CALLING SEQUENCE:
;
;      HistoPlot, dataToHistogram
;
; ARGUMENTS:
;
;       dataToHistogram:  The data from which the histogram is created.
;
; INPUT KEYWORDS:
;
;       AXISCOLORNAME:    The name of the axis color. Default: "Navy". (All color names
;                         derived from FSC_COLOR.)
;
;       BACKCOLORNAME:    The name of the background color. Default: "White".
;
;       BINSIZE:          The binsize of the histogram. By default, Scott's Choice of
;                         bin size for histograms is used:
;                         
;                            binsize = (3.5 * StdDev(data)) / N_Elements(data)^(0.3333)
;                            
;                         If BINSIZE in not defined, and NBINS is defined, the BINSIZE is
;                         calcuated as:
;                         
;                             binsize = (Max(dataToHistogram) - Min(dataToHistogram)) / (NBINS -1)
;
;       DATACOLORNAME:    The name of the data color for drawing the histogram outlines.
;                         Default: "Indian Red".
;
;       FILE:             The name of a color name file to use with FSC_COLOR.
;
;       FILLPOLYGON:      Set this keyword to fill the histogram polygons. If this keyword
;                         is set, the following keyword can also be used.
;
;                         POLYCOLOR:    The name, or vector of names, of polygon colors.
;                                       If a vector, the names are cycled though, as needed.
;                                       Defaults to 'ROSE'.
;
;       FREQUENCY:        If this keyword is set, the relative frequency is plotted on the Y axis,
;                         rather than the histogram density.
;                         
;       L64:              If set, the return value of HISTOGRAM are 64-bit integers, rather than
;                         the default 32-bit integers.
;
;       LINE_FILL:        If set, the polygons are filled with lines instead of solid color. If
;                         this keyword is set, the following keywords can also be used.
;
;                         ORIENTATION:  The orientation of the lines in line-filled polygons in degrees.
;                         PATTERN:      Set to rectangular array of pixel giving fill pattern.
;                         POLYCOLOR:    The name, or vector of names, of line colors.
;                                       If a vector, the names are cycled though, as needed.
;                         SPACING:      The spacing, in centimeters, between parallel lines.
;
;       MAXINPUT:         The maximum value to use in calculating input histogram. Equivalent to MAX keyword
;                         in HISTOGRAM documentation.
;
;       MAX_VALUE:        The maximum Y data value to represent on graphics plot. Default: Max(histdataToPlot) * 1.05
;
;       MININPUT:         The minimum value to use in calculating input histogram. Equivalent to MIN keyword
;                         in HISTOGRAM documentation.
;
;       MIN_VALUE:        The minimum Y data value to represent on graphics plot. Default: 0.
;
;       MISSING:          The value that should be represented as "missing" and not used in the histogram.
;       
;       NAN:              If set, ignore NAN values in calculating and plotting histogram.
;
;       NBINS:            The number of output bins in the histogram. Meaning is slightly different from
;                         meaning in the HISTOGRAM command. Used only to calculate BINSIZE when BINSIZE is
;                         not specified. In this case, binsize = rangeofData/(nbins-1).
;
;
;       OPLOT:            Set this keyword if you want to overplot data on already established axes.
;       
;       OUTLINE:          Set this keyword if you wish to draw only the outline of the histogram plot,
;                         in a manner similar to setting PSYM=10 on a PLOT command.
;       
;       THICK:            Set this keyword to a value greater than 1 to draw thicker axes and lines.
;
;       The user may also enter any other keywords suitable for the PLOT and POLYFILL commands in IDL.
;
; OUTPUT KEYWORDS:
;
;       HISTDATA:         The output value of the internal HISTOGRAM command.
;
;       LOCATIONS:        Starting locations of each bin. (See HISTOGRAM documentation.)
;
;       OMAX:             The maximum output value used to construct the histogram. (See HISTOGRAM documentation.)
;
;       OMIN:             The minimum output value used to construct the histogram. (See HISTOGRAM documentation.)
;
;       REVERSE_INDICES:  List of reverse indices. (See HISTOGRAM documentation.)
;
; EXAMPLES:
;
;      IDL> Histoplot, Dist(256)
;      IDL> Histoplot, Fix(RandomU(seed, 200)*20), POLYCOLOR=['charcoal', 'steel blue'], /FILLPOLYGON
;      IDL> Histoplot, Fix(RandomU(seed, 200)*20), POLYCOLOR=['navy', 'forest green'], /LINE_FILL, ORIENTATION=[45,-45]
;
; REQUIRES:
;
;     Requires at least these programs from the Coyote Library:
;
;        http://www.dfanning.com/programs/convert_to_type.pro
;        http://www.dfanning.com/programs/error_message.pro
;        http://www.dfanning.com/programs/fsc_color.pro
;        
; NOTE:
; 
;       While it is pointed out in the HISTOGRAM documentation, it is extremely
;       important that the BINSIZE be of the same data type as the data you are going to
;       calculate the histogram of. If it is not VERY strange things can happen. I've
;       tried to protect you from most of the bad things, but I don't have a high confidence
;       level that I have done it for every situation. If you see something that "just don't
;       look right", I would check first to see if your data types match. That might solve
;       all the problems. :-)
;
; MODIFICATION HISTORY:
;
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
;-
;******************************************************************************************;
;  Copyright (c) 2007-2010, by Fanning Software Consulting, Inc.                           ;
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
PRO HistoPlot , $                   ; The program name.
   dataToHistogram, $               ; The data to draw a histogram of.
   AXISCOLORNAME=axisColorName, $   ; The axis color.
   BACKCOLORNAME=backcolorName, $   ; The background color.
   DATACOLORNAME=datacolorName, $   ; The data color.
   _REF_EXTRA=extra, $              ; For passing extra keywords.
   FILE=file, $                     ; For specifying a color name file.
   FREQUENCY=frequency, $           ; Plot relative frequency, rather than density.
   MAX_VALUE=max_value, $           ; The maximum value to plot.
   MIN_VALUE=min_value, $           ; The minimum value to plot.
   MISSING=missing, $               ; The value that indicates "missing" data to be excluded from the histgram.
   OPLOT=overplot, $                ; Set if you want overplotting.
   OUTLINE=outline, $               ; Set this keyword if you wish to draw only the outline of the plot.
   THICK=thick, $                   ; Set to draw thicker lines and axes.
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
   REVERSE_INDICES=ri, $
   ;
   ; HISTOGRAM INPUT KEYWORDS
   ;
   BINSIZE=binsize, $               ; The histogram bin size.
   L64=l64, $                       ; Input for HISTOGRAM.
   MAXINPUT=maxinput, $             ; The maximum value to HISTOGRAM.
   MININPUT=mininput, $             ; The minimum value to HISTOGRAM.
   NAN=nan, $                       ; Check for NAN.
   NBINS=nbins                      ; The number of bins to display.


   ; Catch any error in the HistoPlot program.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(!Error_State.Msg + '. Returning...')
      IF N_Elements(nancount) EQ 0 THEN BEGIN
            IF N_Elements(_dataToHistogram) NE 0 THEN dataToHistogram = Temporary(_dataToHistogram)
      ENDIF ELSE BEGIN
            IF nancount EQ 0 THEN BEGIN
                IF N_Elements(_dataToHistogram) NE 0 THEN dataToHistogram = Temporary(_dataToHistogram)
            ENDIF
      ENDELSE
      RETURN
   ENDIF

   ; Check for positional parameter.
   IF N_Elements(dataToHistogram) EQ 0 THEN Message, 'Must pass data to histogram.'
   
   ; What kind of data are we doing a HISTOGRAM on?
   dataType = Size(dataToHistogram, /TYPE)
      
   ; Check the data for NANs and remove them if any are found.
   IF dataType EQ 4 OR datatype EQ 5 THEN BEGIN
        goodIndices = Where(Finite(dataToHistogram), count, NCOMPLEMENT=nancount, COMPLEMENT=nanIndices)
        IF nancount GT 0 THEN BEGIN
           _dataToHistogram = dataToHistogram[goodIndices]
        ENDIF 
   ENDIF 
   
   ; The only sensible way to proceed is to make a copy of the data. Otherwise, I'll have
   ; a devil of a time putting it back together again at the end.
   IF N_Elements(_dataToHistogram) EQ 0 THEN _dataToHistogram = dataToHistogram
   
   ; If you have any "missing" data, remove those now.
   IF N_Elements(missing) NE 0 THEN BEGIN
      nonMissingIndices = Where(_dataToHistogram NE missing, nonMissingCount)
      IF nonMissingCount NE 0 THEN _dataToHistogram = (Temporary(_dataToHistogram))[nonMissingIndices] $
                    ELSE Message, 'There is no non-missing data.'
   ENDIF
   
   ; Did someone pass the number of bins?
   IF N_Elements(nbins) NE 0 THEN theseBins = DOUBLE(nbins)

   ; Check for histogram keywords.
   IF N_Elements(binsize) EQ 0 THEN BEGIN
      range = Max(_dataToHistogram, /NAN) - Min(_dataToHistogram, /NAN)
      IF N_Elements(nbins) EQ 0 THEN BEGIN  ; Scott's Choice
         binsize = (3.5D * StdDev(_dataToHistogram, /NAN))/N_Elements(_dataToHistogram)^(1./3.0D) 
         IF dataType LE 3 THEN binsize = Round(binsize) > 1
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
   IF N_Elements(dataColorName) EQ 0 THEN dataColorName = "Indian Red"
   IF N_Elements(axisColorName) EQ 0 THEN axisColorName = "Navy"
   IF N_Elements(backColorName) EQ 0 THEN backColorName = "White"
   IF N_Elements(polycolorname) EQ 0 THEN polycolorname = "Rose"
   frequency = Keyword_Set(frequency)
   line_fill = Keyword_Set(line_fill)
   IF line_fill THEN fillpolygon = 1
   fillpolygon = Keyword_Set(fillpolygon)
   IF fillpolygon THEN BEGIN
      IF N_Elements(orientation) EQ 0 THEN orientation = 0
      IF N_Elements(spacing) EQ 0 THEN spacing = 0
   ENDIF
   IF N_Elements(mininput) EQ 0 THEN mininput = Min(_dataToHistogram)
   IF N_Elements(maxinput) EQ 0 THEN maxinput = Max(_dataToHistogram)
   IF N_Elements(thick) EQ 0 THEN thick = 1.0

   ; Load plot colors.
   TVLCT, r, g, b, /GET
   axisColor = FSC_Color(axisColorName, FILE=file)
   dataColor = FSC_Color(datacolorName, FILE=file)
   backColor = FSC_Color(backColorName, FILE=file)
   polyColor = FSC_Color(polyColorName, FILE=file)

   ; Set up some labels.
   IF frequency THEN BEGIN
      ytitle = 'Relative Frequency'
      ytickformat = '(F6.4)'
   ENDIF ELSE BEGIN
      ytitle = 'Histogram Density'
      ytickformat = '(I)'
   ENDELSE

  ; Calculate the histogram.
   histdata = Histogram(_dataToHistogram, $
      BINSIZE=binsize, $
      L64=l64, $
      MAX=maxinput, $
      MIN=mininput, $
      NAN=nan, $
      LOCATIONS=locations, $
      OMAX=omax, $
      OMIN=omin, $
      REVERSE_INDICES=ri)
   IF frequency THEN histdata = Float(histdata)/N_Elements(_dataToHistogram)

   ; Calculate the range of the plot output.
;   ymin = 0
;   ymax = Max(histData)
;   ymax = ymax + (ymax * 0.05)
   IF N_Elements(min_value) EQ 0 THEN min_value = 0
   IF N_Elements(max_value) EQ 0 THEN max_value = Max(histData) * 1.05
   ymin = min_value
   ymax = max_value
   xmin = Double(omin) - binsize
   xmax = Double(omax) + (binsize * 2)

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
       Plot, xrange, yrange, $             
             Background=backColor, $
             Color=axisColor, $                       ; The color of the axes.
             NoData=1, $                              ; Draw the axes only. No data.
             XTHICK=thick, $                          ; Axes thicker, if needed.
             YTHICK=thick, $
             XStyle=5, $                              ; Exact axis scaling. No autoscaled axes.
             YMinor=0, $                              ; No minor tick mark on X axis.
             YStyle=5, $                              ; Exact axis scaling. No autoscaled axes.
             XTickformat='(A1)', $                    ; No format. Nothing drawn
             YTickformat='(A1)', $                    ; No format. Nothing drawn
             _Strict_Extra=extra                      ; Pass any extra PLOT keywords.
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
         start = xrange[0] + binsize
         endpt = start + binsize

         FOR j=0,N_Elements(histdata)-1 DO BEGIN
            x = [start, start, endpt, endpt, start]
            y = [0, histdata[j], histdata[j], 0, 0]
            fillcolor = polycolor[j MOD ncolors]
            orient = orientation[j MOD norient]
            space = spacing[j MOD nspace]
            PolyFill, x, y, COLOR=polyColor, /LINE_FILL, ORIENTATION=orient, $
               PATTERN=pattern, SPACING=space, NOCLIP=0
            start = start + binsize
            endpt = start + binsize
         ENDFOR

      ENDIF ELSE BEGIN ; Normal polygon color fill.

         step = (xrange[1] - xrange[0]) / (binsize + 1)
         start = xrange[0] + binsize
         endpt = start + binsize
         FOR j=0,N_Elements(histdata)-1 DO BEGIN
            x = [start, start, endpt, endpt, start]
            y = [0, histdata[j], histdata[j], 0, 0]
            fillcolor = polycolor[j MOD ncolors]
            PolyFill, x, y, COLOR=polyColor, NOCLIP=0
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
       Plot, xrange, yrange, $             
             Background=backColor, $
             Color=axisColor, $                       ; The color of the axes.
             NoData=1, $                              ; Draw the axes only. No data.
             XThick=thick, $  
             YThick=thick, $
             XStyle=9, $                              ; Exact axis scaling. No autoscaled axes.
             YMinor=1, $                              ; No minor tick mark on X axis.
             YStyle=1, $                              ; Exact axis scaling. No autoscaled axes.
             YTickformat=ytickformat, $               ; Y Tickformat
             YTitle=ytitle, $                         ; Y Title
             NoErase=1, $
             XTicklen=-0.025, $
             _Strict_Extra=extra                      ; Pass any extra PLOT keywords.
             
        Axis, !X.CRange[0], !Y.CRange[1], XAXIS=1, XTickformat='(A1)', XMINOR=1, $
            COLOR=axisColor, XSTYLE=1, XTHICK=thick
    ENDIF
    step = (xrange[1] - xrange[0]) / (binsize + 1)
    start = xrange[0] + binsize
    endpt = start + binsize
    ystart = 0
    jend = N_Elements(histdata)-1
    FOR j=0,jend DO BEGIN
        IF Keyword_Set(outline) THEN BEGIN
           PLOTS, [start, start], [ystart, histdata[j]], COLOR=dataColor, THICK=thick, NOCLIP=0
           PLOTS, [start, endpt], [histdata[j], histdata[j]], COLOR=dataColor, THICK=thick, NOCLIP=0
           IF j EQ jend THEN $
              Plots, [endpt, endpt], [histdata[j], 0], COLOR=dataColor, THICK=thick, NOCLIP=0
           start = start + binsize
           endpt = start + binsize
           ystart = histdata[j]
        ENDIF ELSE BEGIN
           x = [start, start, endpt, endpt, start]
           y = [0, histdata[j], histdata[j], 0, 0]
           PLOTS, x, y, COLOR=dataColor, NOCLIP=0, THICK=thick
           start = start + binsize
           endpt = start + binsize
        ENDELSE
    ENDFOR
   
   ; Advance the plot for !P.Multi purposes.
   IF Total(!P.MULTI) NE 0 THEN BEGIN
       !P = bangAfterp 
       !X = bangAfterx 
       !Y = bangAftery
       !MAP = bangAftermap
   ENDIF

   ; Clean up. But you really can't do this in PostScript or the Z-buffer. Causes
   ; total havoc in the Z-buffer, in particular.
   CASE !D.Name OF
     'Z':
     'PS':
     ELSE: TVLCT, r, g, b
   ENDCASE
END
