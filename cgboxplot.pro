; docformat = 'rst'
;
; NAME:
;   cgBoxPlot
;
; PURPOSE:
;   This is graphics routine to display a box plot, also known as a box and
;   whisker plot, in IDL direct graphics. The box encloses the interquartile
;   range (IQR), defined at IQR75-IQR25. The whiskers extend out to the maximum
;   or minimum value of the data, or to the 1.5 times either the IQR75 or IQR25,
;   if there is data beyond this range. Outliers are identified with small circles.
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
; This is graphics routine to display a box plot, also known as a box and
; whisker plot, in IDL direct graphics. The box encloses the interquartile
; range (IQR), defined at IQR75-IQR25. The whiskers extend out to the maximum
; or minimum value of the data, or to the 1.5 times either the IQR75 or IQR25,
; if there is data beyond this range. Outliers are identified with small circles.
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics
;    
; :Examples:
;    Here is an example, using data from the Michaelson-Morley speed of light experiment,
;    in which they made five experiments of 20 measurements of the speed of light each.
;    The data can be downloaded from here::
;       
;       http://www.idlcoyote.com/misc/mm_data.dat
;          
;    Here are the IDL commands to read the data and produce a box plot of it::
;       
;        OpenR, 1, Find_Resource_File('mm_data.dat')
;        header = Strarr(2)
;        Readf, 1, header
;        data = Intarr(5, 20)
;        Readf, 1, data
;        Close, 1
;        cgBoxPlot, data, XTITLE='Experiment Number', YTITLE='Speed of Light'
;        
;    Here is an example that produces a low, medium, and high box for each of
;    six experiments and plots them::
;    
;         data = fltarr(18, 40)
;         index = indgen(6)*3
;         for j=0,5 do data[index[j],*] = Randomu(seed, 40)*6
;         index = index+1
;         for j=0,5 do data[index[j],*] = Randomu(seed, 40)*10
;         index = index+1
;         for j=0,5 do data[index[j],*] = Randomu(seed, 40)*15
;         cgPlot, [0,1], /nodata, yrange=[0,16], xrange=[0,19], $
;            xtickformat='(A1)', ytitle='Gc(mms-1)', YStyle=1
;         index = indgen(6)*3
;         width = ((!X.CRange[1] - !X.Crange[0]) / (20)) * 0.75
;         cgBoxPlot, data[index, *],/overplot, XLOCATION=index+1, WIDTH=width, $
;            BOXCOLOR='rose', /FILLBOX
;         cgBoxPlot, data[index+1, *],/overplot, XLOCATION=index+2, WIDTH=width, $
;            BOXCOLOR='pale green', /FILLBOX
;         cgBoxPlot, data[index+2, *],/overplot, XLOCATION=index+3, WIDTH=width, $
;            BOXCOLOR='goldenrod', /FILLBOX
;         labels = ['AAA', 'BBB', 'CCC', 'DDD', 'EEE', 'FFF']
;         for j=0,5 do cgText, (index+2)[j], -1, labels[j], Alignment=0.5
;           
; .. image:: cgboxplot.png 
;
;    An article about his program can be found here::
;       
;         http://www.idlcoyote.com/graphics_tips/box_whisker.html
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
;        Written by David W. Fanning, 4 March 2009.
;        Added STATS keyword to return data statistics. 5 March 2009. DWF.
;        Added MISSING_DATA_VALUE keyword to identify missing values. 14 March 2009. DWF.
;        Removed limitation of LABELS array having no more than 28 elements. 14 March 2009. DWF.
;        Made it possible to pass a pointer array containing the data, if desired. 14 March 2009. DWF.
;        Added ROTATE keyword to rotate labels. 16 March 2009. DWF.
;        Added several modifications to guard against ill-formed data in the cgBoxPlot_Draw
;          procedure. 23 March 2009. DWF.
;        Added keywords FILLBOXES and BOXCOLOR. 24 March 2009. DWF.
;        Redefined the STATS structure to include MEAN and to store values as doubles. 25 March 2009. DWF.
;        Fixed in a bug that resulted in incorrect behavior when the MISSING_DATA_VALUE keyword
;          was used. 8 April 2009. DWF.
;        Fixed a typo that didn't allow a single column vector to be displayed as a box plot. 17 May 2009. DWF.
;        Now allow a single row vector to be passed into program and displayed. 20 May 2009. DWF.
;        Added NOCLIP=0 keyword to PLOTS command when drawing outliers. 15 July 2009. DWF.
;        Minor adjustment of the X axis label position. 28 October 2010. DWF.
;        Add the ability to change the label character size and thickness via the normal
;          XCHARSIZE and XTHICK keywords you would use for a plot. 3 Dec 2010. DWF.
;        Fixed a couple of typos, added ADDCMD, CHARSIZE, LAYOUT and WINDOW keywords. 2 Feb 2011. DWF.
;        Added the ability to send the output directly to a file via the OUTPUT keyword. 9 Dec 2011, DWF.
;        PostScript, PDF, and Imagemagick parameters can now be tailored with cgWindow_SetDefs. 14 Dec 2011. DWF.
;        Added XLOCATION and WIDTH keywords. 5 June 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2009, Fanning Software Consulting, Inc.
;-
;
;+
; This function prepares the data for display by removing any
; missing data values from further consideration.
; 
; :Params:
;     data: in, required
;        The data to be prepared.
;     missing_data_value: in, required
;        The missing data value that should be found and removed from the data.
;-
FUNCTION cgBoxPlot_Prepare_Data, data, missing_data_value
   
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
   
;+
; Draws the box plot in the display window.
;
; :Params:
;    thisdata: in, required
;       The data to be draw as a box plot.
;       
;  :Keywords:
;    boxcolor: in, optional, type='string', default='rose'
;       If FILLBOXES is set, the IQR box is filled with this color. 
;    color: in, optional, type=string, default='charcoal'              
;       A string color name, as appropriate for the cgColor program. The boxplot 
;       will be drawn in this color.
;    fillboxes: in, optional, type=boolean, default=0
;       Set this keyword to fill the IQR box with a color, specified by BOXCOLOR.
;    stats: in, optional
;       Set this to a named variable that will return an array of structures
;       for each of the columns of data. The statistics are calculated in this
;       routine.
;    width: in, optional
;        The width of the box.
;    xlocation: in, optional
;        The x starting location of the box.
;     
;-
   PRO cgBoxPlot_Draw, thisdata, $
        BOXCOLOR=boxcolor, $
        COLOR=color, $
        FILLBOXES=fillboxes, $
        STATS=stats, $
        WIDTH=width, $
        XLOCATION=xlocation

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
        PLOTS, [x1, x2], [stats.median, stats.median], COLOR=cgColor(color)
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
      IF fillboxes THEN POLYFILL, [x1,x1,x2,x2,x1], [y1,y2,y2,y1,y1], COLOR=cgColor(boxcolor)
      PLOTS, [x1,x1,x2,x2,x1], [y1,y2,y2,y1,y1], COLOR=cgColor(color)
      PLOTS, [x1, x2], [medianData, medianData], COLOR=cgColor(color)
      
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
      PLOTS, [xlocation, xlocation], [quartile_75, top], COLOR=cgColor(color)
      PLOTS, [xlocation, xlocation], [quartile_25, bottom], COLOR=cgColor(color)
      PLOTS, [xlocation - (halfwidth*0.5), xlocation + (halfwidth*0.5)], $
             [top, top], COLOR=cgColor(color)
      PLOTS, [xlocation - (halfwidth*0.5), xlocation + (halfwidth*0.5)], $
             [bottom, bottom], COLOR=cgColor(color)
      
      ; Draw outliners if there are any.
      IF maxcount GT 0 THEN BEGIN
         FOR j=0,maxcount-1 DO PLOTS, xlocation, data[imax[j]], $
            PSYM=cgSymCat(9), COLOR=cgColor(color), NOCLIP=0
      ENDIF
      IF mincount GT 0 THEN BEGIN
         FOR j=0,mincount-1 DO PLOTS, xlocation, data[imin[j]], $
            PSYM=cgSymCat(9), COLOR=cgColor(color), NOCLIP=0
      ENDIF
      
      IF N_Elements(theState) NE 0 THEN Device, Decomposed=theState
   END ;-----------------------------------------------------------------------------------------------------
   
;+
;   This is graphics routine to display a box plot, also known as a box and
;   whisker plot, in IDL direct graphics. The box encloses the interquartile
;   range (IQR), defined at IQR75-IQR25. The whiskers extend out to the maximum
;   or minimum value of the data, or to the 1.5 times either the IQR75 or IQR25,
;   if there is data beyond this range. Outliers are identified with small circles.

; :Params:
;    data: in, required
;       A two-dimensional array. The data for each box plot will be in
;       the columns of the data array. There will be one box plot drawn 
;       for each column in the data array. The maximum column size is 28.
;       As an alternative, data can be a pointer array, in which case
;       there will be one box plot drawn for each valid pointer in the array.
;
; :Keywords:
;    addcmd: in, optional, type=boolean, default=0
;       Set this keyword to add the command to the resizeable graphics window cgWindow.
;    axes_color: in, optional, type=string
;       A string color name, as appropriate for the cgCOLOR program.
;       By default, the same as the COLOR keyword. Used only if OVERPLOT 
;       keyword is not set.
;    background_color: in, optional, type=string, default='background'     
;       A string color name, as appropriate for the cgColor program.
;       Used only if OVERPLOT keyword is not set.
;    boxcolor: in, optional, type='string', default='rose'
;       If FILLBOXES is set, the IQR box is filled with this color. 
;    charsize: in, optional, type=float
;       Set this to the character size to use on the plot. If undefined, uses
;       the value of cgDefCharsize().
;    color: in, optional, type=string, default='opposite'              
;       A string color name, as appropriate for the cgColor program. The boxplot 
;       will be drawn in this color.
;    fillboxes: in, optional, type=boolean, default=0
;       Set this keyword to fill the IQR box with a color, specified by BOXCOLOR.
;    labels: in, optional, type=string               
;       A string array of the same length as the number of columns of data.
;       The boxplots will be labeled with these labels along the X axis.
;       Used only if OVERPLOT keyword is not set.
;    layout: in, optional, type=intarr(3)
;       This keyword specifies a grid with a graphics window and determines where the
;       graphic should appear. The syntax of LAYOUT is three numbers: [ncolumns, nrows, location].
;       The grid is determined by the number of columns (ncolumns) by the number of 
;       rows (nrows). The location of the graphic is determined by the third number. The
;       grid numbering starts in the upper left (1) and goes sequentually by column and then
;       by row.
;    missing_data_value: in, optional
;       Set this keyword to a value that will be used to identify missing data.
;       Missing data is not used in the calculations of the box plot.
;    outfilename: in, optional, type=string
;       If the `Output` keyword is set, the user will be asked to supply an output
;       filename, unless this keyword is set to a non-null string. In that case, the
;       value of this keyword will be used as the filename and there will be no dialog
;       presented to the user.
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
;    overplot: in, optional, type=boolean, default=0              
;       If this keyword is set, the boxplots will be overdrawn on the current
;       set of axes. The X axis will be presumed to be scaled from 0 to 1 more
;       than the number of columns in data.
;    rotate: in, optional, type=float, default=0.0               
;       Set to a value between -90 and 90 degree. The labels will be rotated this
;       amount. Positive values rotate in CCW fashion, negative values in CW fashion.
;    stats: out, optional
;       Set this to a named variable that will return an array of structures
;       for each of the columns of data. The structure will be defined as
;       this:
;
;           struct = { Median:0.0D, Mean: 0.0D, Min:0.0D, Max:0.0D, $
;                      Q25:0.0D, Q75:0.0D, IQR:0.0D, SDEV:0.0D, N:0L }
;
;       Where "mean" is the median value of the data, "Q25" and "Q75" are the 25th percent
;       quartile and 75th percent quartile of the data, repectively, "IRG" is the
;       Interquartile Range, SDEV is the standard deviation, and N is the number of points
;       used to construct the box plot.
;    width: in, optional, type=float
;        The "width" of each box plot in data units. The default is calculated from
;        the X axis range and the number of boxes to draw on the plot like this:
;        ((!X.CRange[1] - !X.Crange[0]) / (numbox+2.0)) * 0.9.
;    window: in, optional, type=boolean, default=0               
;       Set this keyword to display the plot in a resizeable graphics window (cgWindow).
;    xcharsize: in, optional, type=float, default=1.0
;       The size of the X axis labels.
;    xlocation: in, optional, type=integer
;       The X location where the data should be plotted. Can be an array the save size as 
;       the first dimension of data. Normally, this is an integer from 1 to the number of
;       boxplots that are on the final plot.
;    xthick: in, optional, type=integer, default=1
;       The thickness of the X axis labels.
;    _ref_extra: in, optional
;         Any keyword appropriate for the cgPlot command is also accepted by keyword
;         inheritance.
;-
   PRO cgBoxPlot, data, $
        ADDCMD=addcmd, $
        AXES_COLOR=axes_color, $
        BACKGROUND_COLOR=background_color, $
        BOXCOLOR=boxcolor, $
        CHARSIZE=charsize, $
        COLOR=color, $
        FILLBOXES=fillboxes, $
        LABELS=labels, $
        LAYOUT=layout, $
        MISSING_DATA_VALUE=missing_data_value, $
        OUTFILENAME=outfilename, $
        OUTPUT=output, $
        OVERPLOT=overplot, $
        ROTATE=rotate, $
        STATS=stats, $
        XCHARSIZE=xcharsize, $
        XLOCATION=xlocation, $
        XTHICK=xthick, $
        WIDTH=width, $
        WINDOW=window, $
        _REF_EXTRA=extra
        
      ; Error handling.
      Catch, theError
      IF theError NE 0 THEN BEGIN
         Catch, /CANCEL
         void = Error_Message()
         IF N_Elements(theState) NE 0 THEN Device, Decomposed=theState
         IF N_Elements(thisMulti) NE 0 THEN !P.Multi = thisMulti
         RETURN
      ENDIF
      
      ; Check parameters.
      IF N_Params() EQ 0 THEN BEGIN
          Print, 'USE SYNTAX: cgBoxPlot, data'
          RETURN
      ENDIF
      
    ; Do they want this plot in a resizeable graphics window?
    IF Keyword_Set(addcmd) THEN window = 1
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        ; If you are using a layout, you can't ever erase.
        IF N_Elements(layout) NE 0 THEN noerase = 1
            
        IF Keyword_Set(overplot) OR Keyword_Set(addcmd) THEN BEGIN
            cgWindow, 'cgBoxPlot', data, $
                AXES_COLOR=axes_color, $
                BACKGROUND_COLOR=background_color, $
                BOXCOLOR=boxcolor, $
                CHARSIZE=charsize, $
                COLOR=color, $
                FILLBOXES=fillboxes, $
                LABELS=labels, $
                MISSING_DATA_VALUE=missing_data_value, $
                OVERPLOT=overplot, $
                ROTATE=rotate, $
                STATS=stats, $
                XCHARSIZE=xcharsize, $
                XLOCATION=xlocation, $
                XTHICK=xthick, $
                WIDTH=width, $
                ADDCMD=1, $
                _EXTRA=extra
             RETURN
       ENDIF
        
        currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
        IF wincnt EQ 0 THEN replaceCmd = 0 ELSE replaceCmd=1
        cgWindow, 'cgBoxPlot', data, $
                AXES_COLOR=axes_color, $
                BACKGROUND_COLOR=background_color, $
                BOXCOLOR=boxcolor, $
                CHARSIZE=charsize, $
                COLOR=color, $
                FILLBOXES=fillboxes, $
                LABELS=labels, $
                MISSING_DATA_VALUE=missing_data_value, $
                OVERPLOT=overplot, $
                ROTATE=rotate, $
                STATS=stats, $
                XCHARSIZE=xcharsize, $
                XLOCATION=xlocation, $
                XTHICK=xthick, $
                WIDTH=width, $
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

      ; Arguments and keywords.
      IF N_Elements(color) EQ 0 THEN color = 'opposite'
      IF N_Elements(axes_color) EQ 0 THEN axes_color = color
      IF N_Elements(background_color) EQ 0 THEN background_color = 'background'
      IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharsize()
      IF N_Elements(xcharsize) EQ 0 THEN xcharsize = charsize * 0.75
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
          *data[0] = cgBoxPlot_Prepare_Data(*data[0], missing_data_value)
          minData = Min(*data[0], Max=maxData, /NAN)
          IF numbox GT 1 THEN BEGIN
              FOR j=1,numbox-1 DO BEGIN
                 *data[j] = cgBoxPlot_Prepare_Data(*data[j], missing_data_value)
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
          thisData = cgBoxPlot_Prepare_Data(data, missing_data_value)
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
         IF ((!D.Flags AND 256) NE 0) && (!D.Window LT 0) THEN cgDisplay
         Plot, xrange, yrange, /NODATA, _STRICT_EXTRA=extra, $
            XMINOR=1, XTICKS=numbox+1, YSTYLE=1, BACKGROUND=cgColor(background_color), $
            COLOR=cgColor(axes_color), XTICK_GET=xloc, XTICKFORMAT='(A1)', $
            XCHARSIZE=xcharsize, XTHICK=xthick, CHARSIZE=charsize
            
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
             chary = !D.Y_CH_SIZE / Float(!D.Y_Size) * charsize
             XYOUTS, xy[0], xy[1] - (1.5 * chary), /NORMAL, plotlabels[j], $
                ALIGNMENT=alignment, COLOR=cgColor(axes_color), $
                ORIENTATION=rotate, CHARSIZE=charsize, CHARTHICK=xthick
         ENDFOR
         IF N_Elements(theState) NE 0 THEN Device, Decomposed=theState
      ENDIF
      
      ; Draw the boxes.
      IF N_Elements(width) EQ 0 THEN width = ((!X.CRange[1] - !X.Crange[0]) / (numbox+2.0)) * 0.9
      s = { Median:0.0D, Mean: 0.0D, Min:0.0D, Max:0.0D, $
           Q25:0.0D, Q75:0.0D, IQR:0.0D, SDEV:0.0D, N:0L }
      IF Arg_Present(stats) THEN stats = Replicate(s, numbox)
      FOR j=1,numbox DO BEGIN
          IF passedDataType EQ 'POINTER' THEN BEGIN
             dataToBox = cgBoxPlot_Prepare_Data(*data[j-1], missing_data_value)       
          ENDIF ELSE BEGIN
             IF numBox GE 1 THEN BEGIN
                IF isRowVector THEN dataToBox = thisData ELSE dataToBox = Reform(thisData[j-1,*])
             ENDIF
          ENDELSE
          IF N_Elements(xlocation) EQ 0 THEN location=j ELSE location = xlocation[j-1]
          cgBoxPlot_Draw, dataToBox, COLOR=color, BOXCOLOR=boxcolor, FILLBOXES=fillboxes, $
             WIDTH=width, XLOCATION=location, STATS=s
          IF Arg_Present(stats) THEN stats[j-1] = s
      ENDFOR
          
      ; Clean up.
      IF N_Elements(thisMulti) NE 0 THEN !P.Multi = thisMulti
      
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
      
   END ;-----------------------------------------------------------------------------------------------------
