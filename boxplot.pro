;+
; NAME:
;       BOXPLOT
;
; PURPOSE:
;
;       This is graphics routine to display a box plot, also known as a box and
;       whisker plot, in IDL direct graphics. The box encloses the interquartile
;       range (IQR), defined at IQR75-IQR25. The whiskers extend out to the maximum
;       or minimum value of the data, or to the 1.5 times either the IQR75 or IQR25,
;       if there is data beyond this range. Outliers are identified with small circles.
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
;       Boxplot, data
;
; REQUIRED INPUTS:
;
;       data:    A two-dimensional array. The data for each box plot will be in
;                the columns of the data array. There will be one box plot drawn 
;                for each column in the data array. The maximum column size is 28.
;                
;                As an alternative, data can be a pointer array, in which case
;                there will be one box plot drawn for each valid pointer in the array.
;
; INPUT KEYWORDS:
;
;      AXES_COLOR:  A string color name, as appropriate for the FSC_COLOR program.
;                   By default, the same as the COLOR keyword. Used only if OVERPLOT 
;                   keyword is not set.
;                   
;      BACKGROUND_COLOR: A string color name, as appropriate for the FSC_COLOR program.
;                   By default, 'white'. Used only if OVERPLOT keyword is not set.
;
;      BOXCOLOR:    If FILLBOXES is set, the IQR box is filled with this color. By default, "ROSE".
;                   
;      COLOR:       A string color name, as appropriate for the FSC_COLOR program.
;                   By default, 'charcoal'. The boxplot will be drawn in this color.
;
;      FILLBOXES:   Set this keyword to fill the IQR box with a color, specified by BOXCOLOR.
;                   
;      LABELS:      A string array of the same length as the number of columns of data.
;                   The boxplots will be labeled with these labels along the X axis.
;                   Used only if OVERPLOT keyword is not set.
;                   
;      MISSING_DATA_VALUE: Set this keyword to a value that will be used to identify missing data.
;                   Missing data is not used in the calculations of the box plot.
;                   
;      OVERPLOT:    If this keyword is set, the boxplots will be overdrawn on the current
;                   set of axes. The X axis will be presumed to be scaled from 0 to 1 more
;                   than the number of columns in data.
;                   
;      ROTATE:      Set to a value between -90 and 90 degree. The labels will be rotated this
;                   amount. Positive values rotate in CCW fashion, negative values in CW fashion.
;                   
;      Any other keywords (e.g., POSITION, XTITLE, YTITLE, etc.) that are appropriate for 
;      the PLOT command can be used with this procedure.
;
; OUTPUT KEYWORDS:
;
;      STATS:      Set this to a named variable that will return an array of structures
;                  for each of the columns of data. The structure will be defined as
;                  this:
;
;                      struct = { Median:0.0D, Mean: 0.0D, Min:0.0D, Max:0.0D, $
;                                 Q25:0.0D, Q75:0.0D, IQR:0.0D, SDEV:0.0D, N:0L }
;
;                  Where "mean" is the median value of the data, "Q25" and "Q75" are the 25th percent
;                  quartile and 75th percent quartile of the data, repectively, "IRG" is the
;                  Interquartile Range, SDEV is the standard deviation, and N is the number of points
;                  used to construct the box plot.
;      
; REQUIRES:
;
;       Several program from the Coyote Library (http://www.dfanning.com/documents/programs.html)
;       are required. Among them are these:
;       
;       ERROR_MESSAGE (http://www.dfanning.com/programs/error_message.pro)
;       FSC_COLOR (http://www.dfanning.com/programs/fsc_color.pro)
;       SYMCAT (http://www.dfanning.com/programs/symcat.pro)
;
; EXAMPLE:
; 
;       Here is an example, using data from the Michaelson-Morley speed of light experiment,
;       in which they made five experiments of 20 measurements of the speed of light each.
;       The data can be downloaded from here:
;       
;          http://www.dfanning.com/misc/mm_data.dat
;          
;       Here are the IDL commands to read the data and produce a box plot of it.
;       
;           OpenR, 1, 'mm_data.dat'
;           header = Strarr(2)
;           Readf, 1, header
;           data = Intarr(5, 20)
;           Readf, 1, data
;           Close, 1
;           Boxplot, data, XTITLE='Experiment Number', YTITLE='Speed of Light'
;           
;       An article about his program can be found here:
;       
;            http://www.dfanning.com/graphics_tips/box_whisker.html
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 4 March 2009.
;       Added STATS keyword to return data statistics. 5 March 2009. DWF.
;       Added MISSING_DATA_VALUE keyword to identify missing values. 14 March 2009. DWF.
;       Removed limitation of LABELS array having no more than 28 elements. 14 March 2009. DWF.
;       Made it possible to pass a pointer array containing the data, if desired. 14 March 2009. DWF.
;       Added ROTATE keyword to rotate labels. 16 March 2009. DWF.
;       Added several modifications to guard against ill-formed data in the BoxPlot_Draw
;          procedure. 23 March 2009. DWF.
;       Added keywords FILLBOXES and BOXCOLOR. 24 March 2009. DWF.
;       Redefined the STATS structure to include MEAN and to store values as doubles. 25 March 2009. DWF.
;       Fixed in a bug that resulted in incorrect behavior when the MISSING_DATA_VALUE keyword
;          was used. 8 April 2009. DWF.
;       Fixed a typo that didn't allow a single column vector to be displayed as a box plot. 17 May 2009. DWF.
;       Now allow a single row vector to be passed into program and displayed. 20 May 2009. DWF.
;       Added NOCLIP=0 keyword to PLOTS command when drawing outliers. 15 July 2009. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2009, by Fanning Software Consulting, Inc.                                ;
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
   FUNCTION BoxPlot_Prepare_Data, data, missing_data_value
   
      ; If there is no missing_data_value, then just return the data.
      IF N_Elements(missing_data_value) NE 0 THEN BEGIN
      
         ; If the missing_data_value is a number (as opposed to NAN), then we will find
         ; the missing values and change them to !VALUES.F_NAN for futher processing.
         ; If the missing_data_value is a NAN, then we just return the data, since it
         ; is already set up the way it needs to be. We *may* have to convert the data
         ; to floating type to do this.
         IF Finite(missing_data_value) EQ 1 THEN BEGIN
             dataTypeName = Size(missing_data_value, /TNAME)
             CASE dataTypeName OF
             
                'FLOAT': BEGIN
                    epsilon = (MACHAR()).eps
                    indices = Where( Abs(data - missing_data_value) LE epsilon, count)
                    END
                    
                'DOUBLE': BEGIN
                    epsilon = (MACHAR(DOUBLE=1)).eps
                    indices = Where( Abs(data - missing_data_value) LE epsilon, count)
                    END
             
                ELSE: BEGIN
                    indices = Where(data EQ missing_data_value, count)
                    END
             
             ENDCASE
             thisData = data
              
             ; If you found indices, then convert to !VALUES.F_NAN
             IF count GT 0 THEN BEGIN
                    CASE Size(data, /TNAME) OF
                        'FLOAT': thisData[indices] = !VALUES.F_NAN
                        'DOUBLE': thisData[indices] = !VALUES.D_NAN
                        ELSE: BEGIN
                            thisData = Float(data)
                            thisData[indices] = !VALUES.F_NAN
                            END
                    ENDCASE
             ENDIF
             
             RETURN, thisData
         ENDIF ELSE RETURN, data
     ENDIF ELSE RETURN, data
      
   END ; ---------------------------------------------------------------------------------
   
   
   PRO BoxPlot_Draw, thisdata, $
        BOXCOLOR=boxcolor, $
        COLOR=color, $
        FILLBOXES=fillboxes, $
        WIDTH=width, $
        XLOCATION=xlocation, $
        STATS=stats

      On_Error, 1

      ; Check the parameters.
      IF N_Elements(thisdata) EQ 0 THEN Message, 'Data vector is undefined.'
      IF N_Elements(boxcolor) EQ 0 THEN boxcolor = 'ROSE'
      IF N_Elements(color) EQ 0 THEN color = 'WHITE'
      fillboxes = Keyword_Set(fillboxes)
      IF N_Elements(xlocation) EQ 0 THEN xlocation = (!X.CRange[1] - !X.Crange[0]) / 2 + Min(!X.CRange)
      IF N_Elements(width) EQ 0 THEN width = (!X.CRange[1] - !X.Crange[0]) * 0.05
      data = thisData
      
      ; Only work with numbers, no NANs.
      indices = Where(Finite(data) EQ 1, count)
      IF count NE N_Elements(data) THEN data = data[indices]
            
      ; Find min, max, mean, and median values.
      minData = MIN(data, MAX=maxData, /NAN)
      stats.min = minData
      stats.max = maxData
      stats.mean = Mean(data, /NAN, /DOUBLE)
      stats.median = Median(data, /EVEN, /DOUBLE)
      
      ; How many points are going to be used to draw the box plot?
      ; Make sure you have some.
      stats.n = N_Elements(data)
      IF stats.n EQ 0 THEN RETURN
      
      ; If the min is equal to the max, then draw a line and out of here.
      IF (stats.min EQ stats.max) THEN BEGIN
        stats.q25 = stats.median
        stats.q75 = stats.median
        stats.iqr = 0
        stats.sdev = 0
        halfwidth = width / 2.0
        x1 = xlocation - halfwidth
        x2 = xlocation + halfwidth
        PLOTS, [x1, x2], [stats.median, stats.median], COLOR=FSC_Color(color)
        RETURN
      ENDIF
      
      ; Sort the data.
      sortedData = data[Sort(data)]
      IF N_Elements(sortedData) MOD 2 EQ 0 THEN BEGIN
         index = N_Elements(sortedData)/2
         medianData = (sortedData[index-1] + sortedData[index]) / 2.0
         lowerGroup = sortedData[0:index-1]
         higherGroup = sortedData[index:N_Elements(data)-1]
      ENDIF ELSE BEGIN ; The middle point belongs to both upper and lower quartiles.
         index = N_Elements(sortedData)/2
         medianData = sortedData[index]
         lowerGroup = sortedData[0:index]
         higherGroup = sortedData[index:N_Elements(data)-1]
      ENDELSE
      stats.median = medianData

      ; Find the quartiles.
      quartile_25 = Median(lowerGroup, /EVEN)
      quartile_75 = Median(higherGroup, /EVEN) 
      stats.q25 = quartile_25
      stats.q75 = quartile_75
          
      ; Calculate IQR
      iqr = quartile_75 - quartile_25
      stats.iqr = iqr
      stats.sdev = StDDev(data, /NAN, /DOUBLE)

      ; Color decomposition on, if allowed.
      IF (!D.Flags AND 256) NE 0 THEN BEGIN
         Device, Get_Visual_Depth=theDepth
         IF theDepth GE 24 THEN Device, Decomposed=1, Get_Decomposed=theState
      ENDIF
       
      ; Draw the box.
      halfwidth = width / 2.0
      x1 = xlocation - halfwidth
      x2 = xlocation + halfwidth
      y1 = quartile_25
      y2 = quartile_75
      IF fillboxes THEN POLYFILL, [x1,x1,x2,x2,x1], [y1,y2,y2,y1,y1], COLOR=FSC_Color(boxcolor)
      PLOTS, [x1,x1,x2,x2,x1], [y1,y2,y2,y1,y1], COLOR=FSC_Color(color)
      PLOTS, [x1, x2], [medianData, medianData], COLOR=FSC_Color(color)
      
      ; Are there any data greater than 1.5*iqr
      imax = Where(data GT quartile_75 + (1.5 * iqr), maxcount)
      IF maxcount EQ 0 THEN BEGIN
        top = maxData 
      ENDIF ELSE BEGIN
            index = Value_Locate(sortedData, quartile_75 + (1.5 * iqr))
            top = sortedData[0 > (index) < (N_Elements(data)-1)]
      ENDELSE
      
      ; Are there any data less than 1.5*iqr
      imin = Where(data LT quartile_25 - (1.5 * iqr), mincount)
      IF mincount EQ 0 THEN BEGIN
            bottom = minData 
      ENDIF ELSE BEGIN
            index = Value_Locate(sortedData, quartile_25 - (1.5 * iqr))
            bottom = sortedData[0 > (index+1) < (N_Elements(data)-1)]
      ENDELSE
      
      ; Draw the whiskers.
      PLOTS, [xlocation, xlocation], [quartile_75, top], COLOR=FSC_Color(color)
      PLOTS, [xlocation, xlocation], [quartile_25, bottom], COLOR=FSC_Color(color)
      PLOTS, [xlocation - (halfwidth*0.5), xlocation + (halfwidth*0.5)], $
             [top, top], COLOR=FSC_Color(color)
      PLOTS, [xlocation - (halfwidth*0.5), xlocation + (halfwidth*0.5)], $
             [bottom, bottom], COLOR=FSC_Color(color)
      
      ; Draw outliners if there are any.
      IF maxcount GT 0 THEN BEGIN
         FOR j=0,maxcount-1 DO PLOTS, xlocation, data[imax[j]], $
            PSYM=SymCat(9), COLOR=FSC_Color(color), NOCLIP=0
      ENDIF
      IF mincount GT 0 THEN BEGIN
         FOR j=0,mincount-1 DO PLOTS, xlocation, data[imin[j]], $
            PSYM=SymCat(9), COLOR=FSC_Color(color), NOCLIP=0
      ENDIF
      
      IF N_Elements(theState) NE 0 THEN Device, Decomposed=theState
   END ;-----------------------------------------------------------------------------------------------------
   

   PRO BoxPlot, data, $
        AXES_COLOR=axes_color, $
        BACKGROUND_COLOR=background_color, $
        BOXCOLOR=boxcolor, $
        COLOR=color, $
        FILLBOXES=fillboxes, $
        LABELS=labels, $
        MISSING_DATA_VALUE=missing_data_value, $
        OVERPLOT=overplot, $
        ROTATE=rotate, $
        STATS=stats, $
        _EXTRA=extra
        
      ; Error handling.
      Catch, theError
      IF theError NE 0 THEN BEGIN
         Catch, /CANCEL
         void = Error_Message()
         IF N_Elements(theState) NE 0 THEN Device, Decomposed=theState
         RETURN
      ENDIF
      
      ; Arguments and keywords.
      IF N_Params() EQ 0 THEN Message, 'A data array must be passed into BoxPlot.'
      IF N_Elements(color) EQ 0 THEN color = 'Charcoal'
      IF N_Elements(axes_color) EQ 0 THEN axes_color = color
      IF N_Elements(background_color) EQ 0 THEN backgroud_color = 'white'
      IF N_Elements(boxcolor) EQ 0 THEN boxcolor = 'rose'
      fillboxes = Keyword_Set(fillboxes)
      IF N_Elements(rotate) EQ 0 THEN rotate = 0
      rotate = -90 > Fix(rotate) < 90
      overplot = Keyword_Set(overplot)
      isRowVector = 0
      
      passedDataType = Size(data, /TNAME)
      
      ; How many box plots are there?
      IF passedDataType EQ 'POINTER' THEN BEGIN
          numbox = N_Elements(data)
          *data[0] = Boxplot_Prepare_Data(*data[0], missing_data_value)
          minData = Min(*data[0], Max=maxData, /NAN)
          IF numbox GT 1 THEN BEGIN
              FOR j=1,numbox-1 DO BEGIN
                 *data[j] = Boxplot_Prepare_Data(*data[j], missing_data_value)
                 minptr = Min(*data[j], Max=maxptr, /NAN)
                 minData = Min([mindata, minptr], /NAN)
                 maxData = Max([maxdata, maxptr], /NAN)
              ENDFOR    
          ENDIF
      ENDIF ELSE BEGIN
          ndims = Size(data, /N_DIMENSIONS)
          IF ndims EQ 1 THEN BEGIN
            numbox = 1 
            IF N_Elements(data) GT 1 THEN isRowVector = 1 ELSE Message, 'Input data must be a vector.'
          ENDIF ELSE BEGIN
              IF ndims GT 2 THEN Message, 'Cannot work with multi-dimensional data.'
              s = Size(data, /DIMENSIONS)
              numbox = s[0]
          ENDELSE
          thisData = Boxplot_Prepare_Data(data, missing_data_value)
          minData = Min(thisData, Max=maxData, /NAN)
      ENDELSE  

      ; If you are not overplotting, then draw a plot for the box plots.
      IF ~overplot THEN BEGIN
         rr = maxData - minData
         minData = minData - (0.05 * Abs(rr))
         maxData = maxData + (0.05 * Abs(rr))
         yrange = [minData, maxData]
         xrange = [0, numbox + 1]
         IF N_Elements(labels) EQ 0 THEN BEGIN
            plotlabels = ['  ', StrCompress(String(Indgen(numbox) + 1), /REMOVE_ALL), '  ']
         ENDIF ELSE BEGIN
            plotlabels = ['  ', labels, '  ']
         ENDELSE
         IF (!D.Flags AND 256) NE 0 THEN BEGIN
            Device, Get_Visual_Depth=theDepth
            IF theDepth GE 24 THEN Device, Decomposed=1, Get_Decomposed=theState
         ENDIF
         Plot, xrange, yrange, /NODATA, _STRICT_EXTRA=extra, $
            XMINOR=1, XTICKS=numbox+1, YSTYLE=1, BACKGROUND=FSC_Color(background_color), $
            COLOR=FSC_Color(axes_color), XTICK_GET=xloc, XTICKFORMAT='(A1)'
            
         ; Put the labels on the plots.
         CASE 1 OF
            (rotate EQ 0): alignment = 0.5
            (rotate GT 0) AND (rotate LE 45): alignment = 1.0
            (rotate LT 0) AND (rotate GE -45): alignment = 0.0
            (rotate EQ 90): alignment = 1.0
            (rotate EQ -90): alignment = 0.0
            ELSE: alignment = 0.5
         ENDCASE
         FOR j=1,numbox DO BEGIN
             xy = Convert_Coord(xloc[j], !Y.CRange[0], /DATA, /TO_NORMAL)
             XYOUTS, xy[0], xy[1] - 0.025, /NORMAL, plotlabels[j], $
                ALIGNMENT=alignment, COLOR=FSC_Color(axes_color), $
                ORIENTATION=rotate
         ENDFOR
         IF N_Elements(theState) NE 0 THEN Device, Decomposed=theState
      ENDIF
      
      ; Draw the boxes.
      width = ((!X.CRange[1] - !X.Crange[0]) / (numbox+2.0)) * 0.9
      s = { Median:0.0D, Mean: 0.0D, Min:0.0D, Max:0.0D, $
           Q25:0.0D, Q75:0.0D, IQR:0.0D, SDEV:0.0D, N:0L }
      IF Arg_Present(stats) THEN stats = Replicate(s, numbox)
      FOR j=1,numbox DO BEGIN
          IF passedDataType EQ 'POINTER' THEN BEGIN
             dataToBox = BoxPlot_Prepare_Data(*data[j-1], missing_data_value)       
          ENDIF ELSE BEGIN
             IF numBox GE 1 THEN BEGIN
                IF isRowVector THEN dataToBox = thisData ELSE dataToBox = Reform(thisData[j-1,*])
             ENDIF
          ENDELSE
          BoxPlot_Draw, dataToBox, COLOR=color, BOXCOLOR=boxcolor, FILLBOXES=fillboxes, $
             WIDTH=width, XLOCATION=j, STATS=s
          IF Arg_Present(stats) THEN stats[j-1] = s
      ENDFOR
      
   END ;-----------------------------------------------------------------------------------------------------
