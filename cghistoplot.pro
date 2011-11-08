
;+
; NAME:
;       cgHISTOPLOT
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
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:
;
;       Graphics
;
; CALLING SEQUENCE:
;
;      cgHistoplot, dataToHistogram
;
; ARGUMENTS:
;
;       dataToHistogram:  The data from which the histogram is created.
;
; INPUT KEYWORDS:
;
;       ADDCMD:           Set this keyword to add the command to an FSC_Window
;                         command list. Setting this keyword automatically sets
;                         the WINDOW keyword.
;                         
;       AXISCOLORNAME:    The name of the axis color. Default: "Black". (All color names
;                         derived from cgCOLOR.)
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
;       CHARSIZE:         The character size. Default set by calling cgDefCharSize().
;
;       DATACOLORNAME:    The name of the data color for drawing the histogram outlines.
;                         Default: "Indian Red".
;
;       FILE:             The name of a color name file to use with cgCOLOR.
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
;       LAYOUT:           This keyword specifies a grid with a graphics window and determines 
;                         where the graphic should appear. The syntax of LAYOUT is a 3-element 
;                         array: [ncolumns, nrows, location]. The grid is determined by the 
;                         number of columns (ncolumns) by the number of rows (nrows). The location 
;                         of the graphic is determined by the third number. The grid numbering 
;                         starts in the upper left (1) and goes sequentually by column and then
;                         by row.
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
;                         Be aware that if the input data is not of type "float" or "double" that the input
;                         data will be converted to floating point prior to calculating the histogram.
;       
;       NAN:              If set, ignore NAN values in calculating and plotting histogram.
;
;       NBINS:            The number of output bins in the histogram. Meaning is slightly different from
;                         meaning in the HISTOGRAM command. Used only to calculate BINSIZE when BINSIZE is
;                         not specified. In this case, binsize = rangeofData/(nbins-1).
;
;       OPROBABILITY:     Set this keyword if you want to overplot the cumulative probability on the plot.
;       
;       OPLOT:            Set this keyword if you want to overplot data on already established axes.
;       
;       OUTLINE:          Set this keyword if you wish to draw only the outline of the histogram plot,
;                         in a manner similar to setting PSYM=10 on a PLOT command.
;                         
;       PROBCOLORNAME:    The name of the probability color for overplotting the cumulative probability
;                         on the plot.  Default: "Blue".
;                         
;       ROTATE:           Set this keyword to cause the histogram bins to be drawn from left
;                         to right, rather than from bottom to top.
;       
;       THICK:            Set this keyword to a value greater than 1 to draw thicker axes and lines.
;       
;       WINDOW:           Set this keyword to display the plot in a resizeable cgWindow program.
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
;       PROBABILITY:      The total cummulative probability of the histogram plot, scaled from 0 to 1.
;
;       REVERSE_INDICES:  List of reverse indices. (See HISTOGRAM documentation.)
;
; EXAMPLES:
;
;      IDL> cgHistoplot, Dist(256)
;      IDL> cgHistoplot, Fix(RandomU(seed, 200)*20), POLYCOLOR=['charcoal', 'steel blue'], /FILLPOLYGON
;      IDL> cgHistoplot, Fix(RandomU(seed, 200)*20), POLYCOLOR=['navy', 'forest green'], /LINE_FILL, ORIENTATION=[45,-45]
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
;-
;******************************************************************************************;
;  Copyright (c) 2007-2011, by Fanning Software Consulting, Inc.                           ;
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
PRO cgHistoplot, $                    ; The program name.
   dataToHistogram, $               ; The data to draw a histogram of.
   ADDCMD=addcmd, $                 ; Add this command to an cgWindow.
   AXISCOLORNAME=axisColorName, $   ; The axis color.
   BACKCOLORNAME=backcolorName, $   ; The background color.
   CHARSIZE=charsize, $
   DATACOLORNAME=datacolorName, $   ; The data color.
   _REF_EXTRA=extra, $              ; For passing extra keywords.
   FILE=file, $                     ; For specifying a color name file.
   FREQUENCY=frequency, $           ; Plot relative frequency, rather than density.
   LAYOUT=layout, $                 ; Select the grid layout.
   MAX_VALUE=max_value, $           ; The maximum value to plot.
   MIN_VALUE=min_value, $           ; The minimum value to plot.
   NOERASE=noerase, $               ; Set this keyword to avoid erasing when plot is drawn.
   MISSING=missing, $               ; The value that indicates "missing" data to be excluded from the histgram.
   OPLOT=overplot, $                ; Set if you want overplotting.
   OPROBABILITY=oprob, $            ; Overplot the cummulative probability distribution.
   OUTLINE=outline, $               ; Set this keyword if you wish to draw only the outline of the plot.
   PROBCOLORNAME=probColorName, $   ; The color for the probability plot, if it is used. By default, "blue".
   ROTATE=rotate, $                 ; Rotate plot so histogram bars are drawn left to right.
   THICK=thick, $                   ; Set to draw thicker lines and axes.
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
   
   WINDOW=window                    ; Display this in an cgWindow.


   ; Catch any error in the cgHistoplot program.
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
               dataToHistogram, $               ; The data to draw a histogram of.
               AXISCOLORNAME=axisColorName, $   ; The axis color.
               BACKCOLORNAME=backcolorName, $   ; The background color.
               CHARSIZE=charsize, $
               DATACOLORNAME=datacolorName, $   ; The data color.
               _EXTRA=extra, $                  ; For passing extra keywords.
               FILE=file, $                     ; For specifying a color name file.
               FREQUENCY=frequency, $           ; Plot relative frequency, rather than density.
               LAYOUT=layout, $
               MAX_VALUE=max_value, $           ; The maximum value to plot.
               MIN_VALUE=min_value, $           ; The minimum value to plot.
               MISSING=missing, $               ; The value that indicates "missing" data to be excluded from the histgram.
               NOERASE=noerase, $               ; Set this keyword to avoid erasing when plot is drawn.               OPLOT=overplot, $                ; Set if you want overplotting.
               OPROBABILITY=oprob, $            ; Overplot the cummulative probability distribution.
               OUTLINE=outline, $               ; Set this keyword if you wish to draw only the outline of the plot.
               PROBCOLORNAME=probColorName, $   ; The color for the probability plot, if it is used. By default, "blue".
               ROTATE=rotate, $
               THICK=thick, $                   ; Set to draw thicker lines and axes.
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
               dataToHistogram, $               ; The data to draw a histogram of.
               AXISCOLORNAME=axisColorName, $   ; The axis color.
               BACKCOLORNAME=backcolorName, $   ; The background color.
               CHARSIZE=charsize, $
               DATACOLORNAME=datacolorName, $   ; The data color.
               _EXTRA=extra, $                  ; For passing extra keywords.
               FILE=file, $                     ; For specifying a color name file.
               FREQUENCY=frequency, $           ; Plot relative frequency, rather than density.
               LAYOUT=layout, $
               MAX_VALUE=max_value, $           ; The maximum value to plot.
               MIN_VALUE=min_value, $           ; The minimum value to plot.
               MISSING=missing, $               ; The value that indicates "missing" data to be excluded from the histgram.
               NOERASE=noerase, $               ; Set this keyword to avoid erasing when plot is drawn.               OPLOT=overplot, $                ; Set if you want overplotting.
               OPLOT=overplot, $                ; Set if you want overplotting.
               OPROBABILITY=oprob, $            ; Overplot the cummulative probability distribution.
               OUTLINE=outline, $               ; Set this keyword if you wish to draw only the outline of the plot.
               PROBCOLORNAME=probColorName, $   ; The color for the probability plot, if it is used. By default, "blue".
               ROTATE=rotate, $
               THICK=thick, $                   ; Set to draw thicker lines and axes.
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
    
   ; Set up PostScript device for working with colors.
   IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
   ; Check for positional parameter.
   IF N_Elements(dataToHistogram) EQ 0 THEN Message, 'Must pass data to histogram.'
   IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharSize()
   
   ; What kind of data are we doing a HISTOGRAM on?
   dataType = Size(dataToHistogram, /TYPE)
      
   ; Check the data for NANs and alert the user if the NAN keyword is not set.
   IF dataType EQ 4 OR datatype EQ 5 THEN BEGIN
        goodIndices = Where(Finite(dataToHistogram), count, NCOMPLEMENT=nancount, COMPLEMENT=nanIndices)
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
   IF N_Elements(_dataToHistogram) EQ 0 THEN BEGIN
      IF Size(dataToHistogram, /TNAME) EQ 'BYTE' THEN BEGIN
          _dataToHistogram = Fix(dataToHistogram) 
       ENDIF ELSE BEGIN
          _dataToHistogram = dataToHistogram
       ENDELSE
   ENDIF
   
   ; If you have any "missing" data, then the data needs to be converted to float
   ; and the missing data set to F_NAN.
   IF N_Elements(missing) NE 0 THEN BEGIN
      missingIndices = Where(_dataToHistogram EQ missing, missingCount)
      IF missingCount GT 0 THEN BEGIN
         CASE datatype OF
            4: _dataToHistogram[missingIndices] = !Values.F_NAN
            5: _dataToHistogram[missingIndices] = !Values.D_NAN
            ELSE: BEGIN
                _dataToHistogram = Float(_dataToHistogram)
                dataType = 4
                _dataToHistogram[missingIndices] = !Values.F_NAN
                END
         ENDCASE
         nan = 1
      ENDIF ELSE BEGIN
        IF missingCount EQ N_Elements(_dataToHistogram) THEN $
            Message, 'All values are "missing"!'
      ENDELSE
   ENDIF
   
   ; Check for histogram keywords.
   IF N_Elements(binsize) EQ 0 THEN BEGIN
      range = Max(_dataToHistogram, /NAN) - Min(_dataToHistogram, /NAN)
      IF N_Elements(nbins) EQ 0 THEN BEGIN  ; Scott's Choice
         binsize = (3.5D * StdDev(_dataToHistogram, /NAN))/N_Elements(_dataToHistogram)^(1./3.0D) 
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
   IF N_Elements(backColorName) EQ 0 THEN backColorName = "White"
   IF N_Elements(dataColorName) EQ 0 THEN dataColorName = "Indian Red"
   
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

   ; Choose an axis color.
   IF N_Elements(axisColorName) EQ 0 AND N_Elements(saxescolor) NE 0 THEN axisColorName = saxescolor
   IF N_Elements(axisColorName) EQ 0 THEN BEGIN
       IF (Size(backColorName, /TNAME) EQ 'STRING') && (StrUpCase(backColorName) EQ 'WHITE') THEN BEGIN
            IF !P.Multi[0] EQ 0 THEN axisColorName = 'BLACK'
       ENDIF
       IF N_Elements(axisColorName) EQ 0 THEN BEGIN
           IF !D.Name EQ 'PS' THEN BEGIN
                axisColorName = 'BLACK' 
           ENDIF ELSE BEGIN
                IF (!D.Window GE 0) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
                    pixel = cgSnapshot(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF Total(pixel) EQ 765 THEN axisColorName = 'BLACK'
                    IF Total(pixel) EQ 0 THEN axisColorName = 'WHITE'
                    IF N_Elements(axisColorName) EQ 0 THEN axisColorName = 'OPPOSITE'
                ENDIF ELSE axisColorName = 'OPPOSITE'
           ENDELSE
       ENDIF
   ENDIF
   IF N_Elements(axisColorName) EQ 0 THEN axisColor = !P.Color ELSE axisColor = axisColorName
    
   IF N_Elements(polycolorname) EQ 0 THEN polycolorname = "Rose"
   IF N_Elements(probColorname) EQ 0 THEN probColorname = "Blue"
   frequency = Keyword_Set(frequency)
   line_fill = Keyword_Set(line_fill)
   IF line_fill THEN fillpolygon = 1
   fillpolygon = Keyword_Set(fillpolygon)
   IF fillpolygon THEN BEGIN
      IF N_Elements(orientation) EQ 0 THEN orientation = 0
      IF N_Elements(spacing) EQ 0 THEN spacing = 0
   ENDIF
   IF N_Elements(mininput) EQ 0 THEN mininput = Min(_dataToHistogram, NAN=nan)
   IF N_Elements(maxinput) EQ 0 THEN maxinput = Max(_dataToHistogram, NAN=nan)
   IF N_Elements(thick) EQ 0 THEN thick = 1.0

   ; Load plot colors.
   TVLCT, r, g, b, /GET
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
   
   ; Need a probability distribution?
   IF Arg_Present(probablity) OR Keyword_Set(oprob) THEN BEGIN
       cumTotal = Total(histData, /CUMULATIVE)
       probability = Total(Double(histdata)/Total(Double(histdata)), /CUMULATIVE)
   ENDIF

   ; Calculate the range of the plot output.
   IF N_Elements(min_value) EQ 0 THEN min_value = 0
   IF N_Elements(max_value) EQ 0 THEN max_value = Max(histData) * 1.05
   IF Keyword_Set(rotate) THEN BEGIN
       xmin = min_value
       xmax = max_value
       ymin = Double(omin) - binsize
       ymax = Double(omax) + (binsize * 2)
   ENDIF ELSE BEGIN
       ymin = min_value
       ymax = max_value
       xmin = Double(omin) - binsize
       xmax = Double(omax) + (binsize * 2)
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
       Plot, [0,0], xrange=xrange, yrange=yrange, $             
             Background=backColor, $
             Color=axisColor, $                       ; The color of the axes.
             Charsize=charsize, $
             NoData=1, $                              ; Draw the axes only. No data.
             NOERASE=noerase, $
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
         IF Keyword_Set(rotate) THEN BEGIN
            start = yrange[0] + binsize
         ENDIF ELSE BEGIN
            start = xrange[0] + binsize
         ENDELSE

         endpt = start + binsize

         FOR j=0,N_Elements(histdata)-1 DO BEGIN
            IF Keyword_Set(rotate) THEN BEGIN
               y = [start, start, endpt, endpt, start]
               x = [0, histdata[j], histdata[j], 0, 0]
            ENDIF ELSE BEGIN
               x = [start, start, endpt, endpt, start]
               y = [0, histdata[j], histdata[j], 0, 0]
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
               x = [0, histdata[j], histdata[j], 0, 0]
            ENDIF ELSE BEGIN
               x = [start, start, endpt, endpt, start]
               y = [0, histdata[j], histdata[j], 0, 0]
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
       IF Keyword_Set(rotate) THEN BEGIN
       Plot, [0,0], xrange=xrange, yrange=yrange, $             
             Background=backColor, $
             Charsize=charsize, $
             Color=axisColor, $                       ; The color of the axes.
             NoData=1, $                              ; Draw the axes only. No data.
             XThick=thick, $  
             YThick=thick, $
             YStyle=9, $                              ; Exact axis scaling. No autoscaled axes.
             XMinor=1, $                              ; No minor tick mark on X axis.
             XStyle=1, $                              ; Exact axis scaling. No autoscaled axes.
             XTickformat=xtickformat, $               ; Y Tickformat
             YTickformat=ytickformat, $
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
             NoData=1, $                              ; Draw the axes only. No data.
             XThick=thick, $  
             YThick=thick, $
             XStyle=9, $                              ; Exact axis scaling. No autoscaled axes.
             YMinor=1, $                              ; No minor tick mark on X axis.
             YStyle=1, $                              ; Exact axis scaling. No autoscaled axes.
             XTickformat=xtickformat, $               ; Y Tickformat
             YTickformat=ytickformat, $
             XTitle=xtitle, $                         ; Y Title
             YTitle=ytitle, $
             NoErase=1, $
             XTicklen=-0.025, $
             _Strict_Extra=extra                      ; Pass any extra PLOT keywords.
        ENDELSE
             
        IF Keyword_Set(rotate) THEN BEGIN
            Axis, !X.CRange[1], !Y.CRange[0], YAXIS=1, YTickformat='(A1)', YMINOR=1, $
                COLOR=axisColor, YSTYLE=1, YTHICK=thick, CHARSIZE=charsize
        ENDIF ELSE BEGIN
            Axis, !X.CRange[0], !Y.CRange[1], XAXIS=1, XTickformat='(A1)', XMINOR=1, $
                COLOR=axisColor, XSTYLE=1, XTHICK=thick, CHARSIZE=charsize
        ENDELSE
    ENDIF
    
    step = (xrange[1] - xrange[0]) / (binsize + 1)
    IF Keyword_Set(rotate) THEN BEGIN
        start = yrange[0] + binsize
    ENDIF ELSE BEGIN
        start = xrange[0] + binsize
    ENDELSE
    endpt = start + binsize
    ystart = 0
    jend = N_Elements(histdata)-1
    FOR j=0,jend DO BEGIN
        IF Keyword_Set(outline) THEN BEGIN
           IF Keyword_Set(rotate) THEN BEGIN
               PLOTS, [ystart, histdata[j]], [start, start], COLOR=dataColor, THICK=thick, NOCLIP=0
               PLOTS, [histdata[j], histdata[j]], [start, endpt], COLOR=dataColor, THICK=thick, NOCLIP=0
               IF j EQ jend THEN $
                  Plots, [histdata[j], 0], [endpt, endpt], COLOR=dataColor, THICK=thick, NOCLIP=0
           ENDIF ELSE BEGIN
               PLOTS, [start, start], [ystart, histdata[j]], COLOR=dataColor, THICK=thick, NOCLIP=0
               PLOTS, [start, endpt], [histdata[j], histdata[j]], COLOR=dataColor, THICK=thick, NOCLIP=0
               IF j EQ jend THEN $
                  Plots, [endpt, endpt], [histdata[j], 0], COLOR=dataColor, THICK=thick, NOCLIP=0
           ENDELSE
           start = start + binsize
           endpt = start + binsize
           ystart = histdata[j]
        ENDIF ELSE BEGIN
           x = [start, start, endpt, endpt, start]
           y = [0, histdata[j], histdata[j], 0, 0]
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
        IF Keyword_Set(rotate) THEN BEGIN
            probx = Scale_Vector(probability, !X.CRange[0], !X.CRange[1], MIN=0, MAX=1)
            IF Keyword_Set(oplot) THEN bsize = 0 ELSE bsize = binsize
            proby = Scale_Vector(Findgen(N_Elements(probx)), !Y.CRange[0] + bsize, !Y.CRange[1] - bsize)
            Oplot, probx, proby, COLOR=probcolor
        ENDIF ELSE BEGIN
            proby = Scale_Vector(probability, !Y.CRange[0], !Y.CRange[1], MIN=0, MAX=1)
            IF Keyword_Set(oplot) THEN bsize = 0 ELSE bsize = binsize
            probx = Scale_Vector(Findgen(N_Elements(proby)), !X.CRange[0] + bsize, !X.CRange[1] - bsize)
            Oplot, probx, proby, COLOR=probcolor
        ENDELSE
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

END
