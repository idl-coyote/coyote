; docformat = 'rst'
;
; NAME:
;   cgTaylorDiagram
;
; PURPOSE:
;   The program implements a Taylor Diagram in IDL direct graphics (Coyote Graphics).
;     http://onlinelibrary.wiley.com/doi/10.1029/2000JD900719/abstract
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2013, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; The program implements a Taylor Diagram in IDL direct graphics. Addtional information can be found
; `in this article <http://onlinelibrary.wiley.com/doi/10.1029/2000JD900719/abstract>`.
; 
; .. image:: cgtaylordiagram.png
; 
; From the 2001 IPCC Reoprt::
; 
;       "Taylor diagrams provide a way of graphically summarizing how closely 
;       a pattern (or a set of patterns) matches observations.  The similarity between two 
;       patterns is quantified in terms of their correlation, their centered root-mean-square 
;       difference and the amplitude of their variations (represented by their standard 
;       deviations).  These diagrams are especially useful in evaluating multiple aspects of 
;       complex models or in gauging the relative skill of many different models."
;
;       "In general, the Taylor diagram characterizes the statistical relationship between two 
;       fields, a "test" field (often representing a field simulated by a model) and a 
;       "reference" field (usually representing “truth”, based on observations)."
;
;       "The two-dimensional space of the Taylor diagram can represent three different statistics 
;       simultaneously::
;          - The centered RMS difference, 
;          - The correlation, 
;          - The standard deviation
;          
; The reference to Karl Taylor's original paper explaining the diagram is Taylor, K.E., 
; `Summarizing multiple aspects of model performance in a single diagram, <http://onlinelibrary.wiley.com/doi/10.1029/2000JD900719/abstract>` J. Geophys. Res., 106, 7183-7192, 2001.
;       
; Here is a `simple, but complete, explanation <http://www-pcmdi.llnl.gov/about/staff/Taylor/CV/Taylor_diagram_primer.pdf>` of the diagram.      
;  
; :Categories:
;    Graphics
;    
; :Params:
;    correlation: in, required, type=float
;        An array of correlation coefficients for the points that will be plotted on the diagram.
;        This array must be the same length as the `stddev` array and the `Labels` array, if it is used.
;    stddev: in, required, type=float
;        An array of standard deviations for the points that will be plotted on the diagram.
;        This array must be the same length as the `correlation` array and the `Labels` array, if it is used.
;       
; :Keywords:
;    addcmd: in, optional, type=boolean, default=0
;        Set this keyword to add the command to the resizeable graphics window cgWindow.
;    c_correlation: in, optional, type=string, default="grn7"
;        The name of the color used for the correlation lines on the diagram.
;    c_stddev: in, optional, type=string, default="blu7"
;        The name of the color used for the standard deviation lines on the diagram.
;    c_ref: in, optional, type=string, default="pur7"
;        The name of the color used for the observed reference line on the diagram.
;    c_symbol: in, optional, type=string, default="red"
;        The name of the color used for the point symbols on the diagram.
;    labels: in, optional, type=string
;        An array of string labels for the points that will be plotted on the diagram.
;        This array must be the same length as the `stddev` array and the `correlation` array.
;    noerase: in, optional, type=boolean, default=0
;        Set this keyword if you don't want the Taylor Diagram plot to erase what is already on
;        the display.
;    output: in, optional, type=string
;        The name of an output file to write the Taylor Diagram to. The type of file is taken from
;        the file extension. For example, OUTPUT='mydiagram.png'. It is assumed that Ghostscript and
;        ImageMagick have been installed properly for all raster file output. If the Output keyword is
;        used, nothing is drawn on the display. This keyword cannot be used with the Overplot keyword.
;    overplot: in, optional, type=boolean, default=0
;        Set this keyword to overplot onto an already existing Taylor Diagram. Many keywords are
;        ignored if this keyword is set. Only the data is drawn. The Output keyword cannot be used
;        if overplotting.
;    position: in, optional, type=float
;        A four-element, normalized array giving the position of the plot in the display window: [x0,y0,x1,y1].
;    ref_stddev: in, optional, type=float, default=1.0
;        The reference standard deviation. This is typically the "observed" or "model" value. A scalar.
;    rms_circles_off: in, optional, type=boolean, default=0
;        Set this keyword to prevent the drawing of the RMS circles that radiate out from the observed RMS value.
;    rms_format: in, optional, type=string, default='(I0)'
;        Set this keyword to a format string that is used for format the RMS circle labels.
;    rms_increment: in, optional, type=float, default=1.0
;        The RMS circles are drawn from the observed RMS value, using this value as an increment of the circle radius.
;    rms_labels_off: in, optional, type=boolean, default=0
;        Set this keyword to prevent the drawing of the RMS circle labels. If this keyword is set, only the RMS
;        circles are drawn.
;    stddev_max: in, optional, type=float
;        The maximum standard deviation to plot on the graph. 
;    symbol: in, optional, type=integer, default=16
;        The symbol used for the data points on the diagram. Any symbol supported by `cgSymCat`.
;    symsize: in, optional, type=float, default=1.5
;        The size of the symbol used for the data points on the diagram.
;    window: in, optional, type=boolean, default=0
;        Set this keyword to replace all the commands in a current cgWindow or to
;        create a new cgWindow for displaying this command.
;         
; :Examples:
;    Here is how to use this program::
;    
;      labels = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']            ; Point labels.
;      stddev = [1.4, 0.9, 1.0, 1.272, 1.1, 0.95, 1.08, 0.5]        ; Standard Deviations
;      correlation = [0.8, 0.9, 0.65, 0.74, 0.91, 0.98, 0.85, 0.35] ; Correlations
;      ref_std = 1.0                                                ; Reference standard (observed)
;      stddev_max = 1.5                                             ; Standard Deviation maximum
;      cgTaylorDiagram, stddev, correlation, REF_STDDEV=ref_std, STDDEV_MAX=stddev_max, LABELS=labels
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 10 January 2013 by David W. Fanning from a program by Fernando Santoro of
;            ExelisVis that I found in the IDL Code Repository on the ExelisVis web page.
;            Fernando did *all* of the hard work writing the program for the IDL 8 function
;            graphics routine. I simply copied most of his code and adapted it for non-IDL 8 
;            users. I also added a couple of features I though were missing from the original code.
;        Added OVERPLOT keyword. 21 May 2013. DWF.
;        Added RMS_*** keywords to allow more control over the drawing and labeling of the RMS circles 
;            on the plot. 29 July 2013. DWF.
;        Modified the algorithm that places the "Correlation" label to allow multiple plots in a 
;           window. Also removed a cgPolyFill command that appeared to have no effect. 19 Nov 2013. DWF.
;        Added NOERASE keyword and made sure no window was opened when OUTPUT keyword is used. 21 Nov 2013. DWF.
;        Added check to not create initial plot if PostScript is the current device. 18 Feb 2015. DWF.
;        Small modification to prevent extraneous drawing on right edge of plot in PostScript files,
;           and updated figures and documentation. 10 April 2015. DWF.
;        Another small modification to accommodate extraneous line removal when doing multiple plots on
;           a page. Appears to work in both PostScript and on the display for multiple plots. 19 May 2015. DWF.
;
; :Copyright:
;     Copyright (c) 2013-2015, Fanning Software Consulting, Inc.
;-
PRO cgTaylorDiagram, stddev, correlation, $
    ADDCMD=addcmd, $
    C_CORRELATION=c_correlation, $
    C_STDDEV=c_stddev, $
    C_REF=c_ref, $
    C_SYMBOL=c_symbol, $
    LABELS=labels, $
    NOERASE=noerase, $
    OUTPUT=output, $
    OVERPLOT=overplot, $
    POSITION=position, $
    REF_STDDEV=ref_stddev, $
    RMS_CIRCLES_OFF=rms_circles_off, $
    RMS_FORMAT=rms_format, $
    RMS_INCREMENT=rms_increment, $
    RMS_LABELS_OFF=rms_labels_off, $
    STDDEV_MAX=stddev_max, $
    SYMBOL=symbol, $
    SYMSIZE=symsize, $
    WINDOW=window

  Compile_Opt idl2
    
  ; Standard error handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = cgErrorMsg()
      IF N_Elements(currentState) NE 0 THEN cgSetColorState, currentState
      RETURN
  ENDIF
  
  overplot = Keyword_Set(overplot)
  noerase = Keyword_Set(noerase)

  ; Are we doing some kind of output?
  IF (N_Elements(output) NE 0) && (output NE "") && ~overplot THEN BEGIN
    
       ; Determine the type of file from the filename extension.
       root_name = cgRootName(output, DIRECTORY=theDir, EXTENSION=ext)
       IF theDir EQ "" THEN CD, CURRENT=theDir
       outfilename = output
       outputSelection = StrUpCase(ext)
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
   
  ; Set up PostScript device for working with colors.
  IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8

  ; Do they want this plot in a resizeable graphics window?
  IF Keyword_Set(addcmd) THEN window = 1
  IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        ; Special treatment for overplotting or adding a command.
        IF Keyword_Set(addcmd) THEN BEGIN
            cgWindow, 'cgTaylorDiagram', stddev, correlation, $
                C_CORRELATION=c_correlation, $
                C_STDDEV=c_stddev, $
                C_REF=c_ref, $
                C_SYMBOL=c_symbol, $
                LABELS=labels, $
                NOERASE=noerase, $
                OUTPUT=output, $
                OVERPLOT=overplot, $
                POSITION=position, $
                REF_STDDEV=ref_stddev, $
                RMS_CIRCLES_OFF=rms_circles_off, $
                RMS_FORMAT=rms_format, $
                RMS_INCREMENT=rms_increment, $
                RMS_LABELS_OFF=rms_labels_off, $
                STDDEV_MAX=stddev_max, $
                SYMBOL=symbol, $
                SYMSIZE=symsize, $
                ADDCMD=1
             RETURN
       ENDIF
        
        ; Open a new window or replace the current commands, as required.
        currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
        IF wincnt EQ 0 THEN replaceCmd = 0 ELSE replaceCmd=1
        cgWindow, 'cgTaylorDiagram', stddev, correlation, $
                C_CORRELATION=c_correlation, $
                C_STDDEV=c_stddev, $
                C_REF=c_ref, $
                C_SYMBOL=c_symbol, $
                LABELS=labels, $
                NOERASE=noerase, $
                OUTPUT=output, $
                OVERPLOT=overplot, $
                POSITION=position, $
                REF_STDDEV=ref_stddev, $
                RMS_CIRCLES_OFF=rms_circles_off, $
                RMS_FORMAT=rms_format, $
                RMS_INCREMENT=rms_increment, $
                RMS_LABELS_OFF=rms_labels_off, $
                STDDEV_MAX=stddev_max, $
                SYMBOL=symbol, $
                SYMSIZE=symsize, $
                REPLACECMD=replaceCmd
         RETURN
    ENDIF
    
  ; Do this in decomposed color mode.
  cgSetColorState, 1, CURRENT=currentState
  
  ; Check parameters.
  IF N_Params() NE 2 THEN BEGIN
     Print, 'Calling Syntax: cgTaylorDiagram, stddev, correlation'
     RETURN
  ENDIF
  
  ; Default values for keywords.
  SetDefaultValue, c_correlation, 'grn7'
  SetDefaultValue, c_stddev, 'blu7'
  SetDefaultValue, c_ref, 'pur7'
  SetDefaultValue, c_symbol, 'red'
  SetDefaultValue, symbol, 16
  SetDefaultValue, symsize, 1.5
  SetDefaultValue, ref_stddev, 1.0
  SetDefaultValue, rms_increment, 1.0
  SetDefaultValue, rms_format, '(I0)'
  SetDefaultValue, stddev_max, Round((Max(stddev) * 1.25) * 10)/ 10.0
  
  ; Skip all this if you are overplotting
  IF overplot THEN GOTO, overplotComeHere
    
  ; Construction of the diagram.
  
  ; PART I: Outer Axis circle and RMS circles
  cir_npts = 1000     ;number of points of the outer circle
  x = Findgen(cir_npts)/(cir_npts-1) * stddev_max
  y = SQRT(stddev_max^2 - x^2) ; Equation of Outer circle. stddev_max is maximun of the radius of the Correlation Circle Axis

  IF N_Elements(position) EQ 0 && (Total(!P.Multi) LE 0) && (Total(!P.Position) EQ 0.0) THEN BEGIN
     position = [0.125, 0.125, 0.9, 0.9]
  ENDIF
   
  ; Initial plot in window.
  IF (!D.Window LT 0) && (~overplot) && (!D.Name NE 'PS') THEN cgDisplay, 680, 640
  cgPlot, x, y, /NoData, XTITLE='Standard Deviation', YTITLE='Standard Deviation', $
      XSTYLE=9, YSTYLE=9, POSITION=position, BACKGROUND='white', NOERASE=noerase
  
  ; PART II: Building ticks: Long and Short ticks
  ; Long Ticks
  nticks = 10
  cir_ticks = Findgen(nticks)/nticks * stddev_max
  
  ; Coordinates of the two extremes of the ticks for the outer circle: we will create a polyline as ticks
  long_x_right = cir_ticks
  long_y_right = SQRT(stddev_max^2 - cir_ticks^2)
  long_x_left  = FltArr(nticks)
  long_y_left  = FltArr(nticks)
  
  ; Multiple RMS circles
  multi_cir = 1000                                      ; Number of points of each RMS circle
  number_cirs = Fix((stddev_max / rms_increment)) + 5   ; Number of RMS circles
  initial_rms_increment = rms_increment
    
  IF ~Keyword_Set(rms_circles_off) THEN BEGIN
      FOR i=0, number_cirs-1 DO BEGIN
    
        multi_max = ref_stddev + rms_increment
        multi_min = ref_stddev - rms_increment
       
        multi_circlesx = Findgen(multi_cir)/(multi_cir-1)*(multi_max-multi_min)+multi_min
        multi_circlesy = SQRT(rms_increment^2 - (multi_circlesx-ref_stddev)^2)
        x_to_plot = 0 > multi_circlesx < stddev_max
        y_to_plot = 0 > multi_circlesy < stddev_max
        maxIndex = Where(x_to_plot GE stddev_max, maxCnt)
        IF maxCnt GT 0 THEN BEGIN
            x_to_plot = x_to_plot[0:maxIndex[0]-1]
            y_to_plot = y_to_plot[0:maxIndex[0]-1]
        ENDIF
        cgPlotS, x_to_plot, y_to_plot, COLOR=c_stddev, LINESTYLE=1
        number = String(rms_increment, Format=rms_format)

        IF ~Keyword_Set(rms_labels_off) THEN BEGIN
            IF (multi_circlesx[i+50] GT 0) AND (multi_circlesx[i+50] LT stddev_max) THEN BEGIN
                cgText, multi_circlesx[i+50], multi_circlesy[i+50], number, $
                   CHARSIZE=cgDefCharsize()*0.8, ALIGNMENT=1, /DATA, CLIP=0, COLOR=c_stddev
            ENDIF
        ENDIF
        rms_increment = initial_rms_increment + rms_increment
        
      ENDFOR
  ENDIF
  
  ; Masking part of the RMS circles out.
  cgColorFill, [x, stddev_max, x[0]],[y, stddev_max, y[0]], /data, COLOR='white'
  cgPolygon,   [x, stddev_max, x[0]],[y, stddev_max, y[0]], /data, COLOR='white'
  cgColorFill, [         0,               0, stddev_max*1.05, stddev_max*1.05,          0], $
               [stddev_max, stddev_max*1.05, stddev_max*1.05,      stddev_max, stddev_max], $
               COLOR='white' ; To remove traces of line above plot in PostScript files.
  
  cgPlotS, x, y
  
 ; Short Ticks
 ; new circle where its points will be used as the end point of the short ticks
   short_cir = 1000
  short_max = stddev_max*.98
  short_min = 0.0
  short_cir_x = Findgen(short_cir)/(short_cir-1)*(short_max-short_min)+short_min
  short_cir_y = SQRT(short_max^2 - short_cir_x^2)
  
  ;Some points of the new circle to be used as ticks
  short_nticks = 20
  shortx = Findgen(short_nticks)/short_nticks *(short_max-short_min)+short_min
  shorty = SQRT(short_max^2 - shortx^2)
  
  select_shortx = FltArr(nticks)
  select_shorty = FltArr(nticks)
  
  j = 0
  FOR i=0, short_nticks-1, 2 DO BEGIN
    select_shortx[j] = shortx[i+1]
    select_shorty[j] = shorty[i+1]
    j = j+1
  ENDFOR

 ; Some points of the outer circle to be used as extremes for the short ticks
  outer_shortx = Findgen(short_nticks)/short_nticks * stddev_max
  outer_shorty = SQRT(stddev_max^2 - outer_shortx^2)
  
  select_outerx = FltArr(nticks)
  select_outery = FltArr(nticks)
  
  j = 0
  FOR i=0, short_nticks-1, 2 DO BEGIN
    select_outerx[j] = outer_shortx[i+1]
    select_outery[j] = outer_shorty[i+1]
    j = j+1
  ENDFOR

  angles = FltArr(nticks)
  text_ang = FltArr(nticks)
  
  ; Plotting correlation lines and ticks.
  FOR i=0,nticks-1 DO BEGIN
    cgPlotS, [long_x_right[i], long_x_left[i]], [long_y_right[i], long_y_left[i]], $
        COLOR=((i EQ 0) ? 'black' : c_correlation)
    cgPlotS, [select_outerx[i], select_shortx[i]], [select_outery[i], select_shorty[i]]
    
    ;the following IF statement is here because for i=0,long_x_right[i]=0.0
    ;and therefore I get: % Program caused arithmetic error: Floating divide by 0 in line 102
    IF long_x_right[i] EQ 0.0 THEN BEGIN
      angles[i] = 1.57080 ;90 degrees radians
    ENDIF ELSE BEGIN
      angles[i] = atan(long_y_right[i]/long_x_right[i]) 
    ENDELSE
    text_ang[i] = angles[i]*(180.0/!PI)
    tick_number = [' 0.0', ' 0.1', ' 0.2', ' 0.3', ' 0.4', ' 0.5', ' 0.6', ' 0.7', ' 0.8', ' 0.9']
   
    cgText, long_x_right[i], long_y_right[i], tick_number[i], ORIENTATION=text_ang[i], $
        CLIP=0, COLOR=c_correlation, CHARSIZE=cgDefCharsize()*0.85
    
  ENDFOR
  
  last_angle = atan(select_outery[nticks-1]/select_outerx[nticks-1]) 
  short_last_ang = last_angle*(180.0/!PI)

  cgText, select_outerx[nticks-1], select_outery[nticks-1], ' 0.95', ORIENTATION=short_last_ang, $
       CLIP=0, COLOR=c_correlation, CHARSIZE=cgDefCharsize()*0.85

  cgText, stddev_max, y[N_Elements(y)-1], ' 1.0', CHARSIZE=cgDefCharsize()*0.85, CLIP=0, COLOR=c_correlation
  
  
  ;Extra ticks between correlation values 0.9 and 1:
  ;Even Shorter Ticks
 ; new circle where its points will be used as the end point of the Extra short ticks
  extrashort_cir = 1000
  extrashort_max = stddev_max*.99
  sector_x = cos(angles[nticks-1]) * extrashort_max
  extrashort_min  =  sector_x
  extrashort_cir_x = Findgen(extrashort_cir)/(extrashort_cir-1)*(extrashort_max-extrashort_min)+extrashort_min
  extrashort_cir_y = SQRT(extrashort_max^2 - extrashort_cir_x^2)
  
  ;Select some points of the last angular sector to be used as ticks: outer circle and inside circle:
  extrashort_nticks = 10
  
  extrashort_outermax = stddev_max
  sector_outerx  = cos(angles[nticks-1]) * stddev_max
  extrashort_outermin = sector_outerx
  extrashort_outerx = Findgen(extrashort_nticks)/(extrashort_nticks)*(stddev_max-extrashort_outermin)+extrashort_outermin
  extrashort_outery = SQRT(stddev_max^2 - extrashort_outerx^2)
  
  extrashortx = Findgen(extrashort_nticks)/(extrashort_nticks) *(extrashort_max-extrashort_min)+extrashort_min
  extrashorty = SQRT(extrashort_max^2 - extrashortx^2)

  
  FOR i=0, extrashort_nticks-1 DO BEGIN
    cgPlots, [extrashort_outerx[i], extrashortx[i]], [extrashort_outery[i], extrashorty[i]], COLOR=c_correlation
  ENDFOR
  
  ; Correlation Axis Name
  cc_namex  = (!X.Window[1] - !X.Window[0]) * 0.775 + !X.Window[0]
  cc_namey  = (!Y.Window[1] - !Y.Window[0]) * 0.775 + !Y.Window[0]
  cgText, cc_namex, cc_namey, 'Correlation', ORIENTATION=-45., ALIGNMENT=0.5,  $
    COLOR=c_correlation, /NORMAL
 

  ; Observed/Reference Circles. The dashed circles centered in the Observed value are the centered RMS
  ; (root-mean-square) values.
  ref_cir = 1000
  ref_max = ref_stddev
  ref_min = 0.0
  ref_cir_x = Findgen(ref_cir)/(ref_cir-1)*(ref_max-ref_min)+ref_min
  ref_cir_y = SQRT(ref_max^2 - ref_cir_x^2)
  cgPlots, ref_cir_x, ref_cir_y, LINESTYLE=2, COLOR='pur7'
  cgText, ref_max, stddev_max * 0.05, 'Observed', ALIGNMENT=0.5, COLOR='pur7'
  
  ; PART III: Plotting the Input Data Points
  overplotComeHere:
  dataangle = ACos( correlation )            
  data_x = stddev * Cos( dataangle )     
  data_y = stddev * Sin( dataangle )  
  
  cgPlotS, data_x, data_y, PSYM=symbol, COLOR=c_symbol, SymSize=symsize
  xy = Convert_Coord(data_x, data_y, /DATA, /TO_NORMAL)
  squib = 0.0075
  IF N_Elements(labels) NE 0 THEN BEGIN
      cgText, xy[0,*], xy[1,*] + 2*squib, labels, /NORMAL, FONT=0, ALIGNMENT=0.5
  ENDIF
  
  ; Are we producing output? If so, we need to clean up here.
  IF (N_Elements(output) NE 0) && (output NE "") && (~overplot) THEN BEGIN
    
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

  ; Restore color mode.
  cgSetColorState, currentState
  
END
  
  
  
  
  ;#######################################  Main Test Program  #############################################

      cgDisplay, 700, 700
      labels = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']            ; Point labels.
      stddev = [1.4, 0.9, 1.0, 1.272, 1.1, 0.95, 1.08, 0.5]        ; Standard Deviations
      correlation = [0.8, 0.9, 0.65, 0.74, 0.91, 0.98, 0.85, 0.35] ; Correlations
      ref_std = 1.0                                                ; Reference standard (observed)
      stddev_max = 1.5                                             ; Standard Deviation maximum
      cgTaylorDiagram, stddev, correlation, REF_STDDEV=ref_std, STDDEV_MAX=stddev_max, $
          RMS_INCREMENT=0.25, RMS_FORMAT='(F0.2)', LABELS=labels

      labels = ['I',  'J', 'K', 'L',  'M', 'N', 'O',   'P']                 ; Point labels.
      stddev = [1.25, 0.7, 1.1, 0.86, 1.5, 1.21, 0.78, 0.52]                ; Standard Deviations
      correlation = Reverse([0.8, 0.9, 0.65, 0.74, 0.91, 0.98, 0.85, 0.35]) ; Correlations
      cgTaylorDiagram, stddev, correlation, /OVERPLOT, LABELS=labels, C_SYMBOL='blue'
END
