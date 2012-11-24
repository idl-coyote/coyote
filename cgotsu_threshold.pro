; docformat = 'rst'
;
; NAME:
;   cgOTSU_THRESHOLD
;
; PURPOSE:
;   The purpose of this function is to find an optimal threshold for separating
;   a bimodal distribution of pixels in an image histogram. The Otsu Threshold method
;   is explained here: http://www.labbookpages.co.uk/software/imgProc/otsuThreshold.html.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2012, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; The purpose of this function is to find an optimal threshold for separating
; a bimodal distribution of pixels in an image histogram. The algorithm used is the
; "faster approach" algorithm explained 
; `on this web page <http://www.labbookpages.co.uk/software/imgProc/otsuThreshold.html>`.
;
; .. image:: cgotsu_threshold.png
; 
; :Categories:
;    Utility
;    
; :Returns:
;     The optimal threshold that separates two populations of pixels is returned.
;    
; :Params:
;    data: in, required, 
;       The data from which the histogram is created.
;       
; :Keywords:
;    binsize: in, optional
;       The binsize of the histogram. By default, Scott's Choice of bin size for histograms is used::
;                         
;           binsize = (3.5 * StdDev(data)) / N_Elements(data)^(0.3333)
;           
;       unless the data is byte type. Then a BINSIZE of 1 is used by default
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
;    example: in, optional, type=boolean, default=0
;       Set this keyword if you wish to use the example data from the 
;       `reference documentation <http://www.labbookpages.co.uk/software/imgProc/otsuThreshold.html>`.
;    histdata: out, optional
;       The output value of the internal HISTOGRAM command.
;    l64: in, optional, type=boolean, default=0                       
;       If set, the return value of HISTOGRAM are 64-bit integers, rather than
;       the default 32-bit integers. Set by default for data types greater than or
;       equal to 12.
;    locations: out, optional
;       Starting locations of each bin. (See the HISTOGRAM documentation for details.)
;    max: in, optional
;       The maximum value to use in calculating input histogram. Equivalent to the MAX keyword
;       in the HISTOGRAM documentation.
;    min: in, optional
;       The minimum value to use in calculating input histogram. Equivalent to the MIN keyword
;       in the HISTOGRAM documentation.
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
;    plotit: in, optional, type=boolean, default=0
;       If this keyword is set, a histogram of the data is plotted along with a plot of the
;       between class variance to show the selected threshold.
;    reverse_indices: out, optional
;       The list of reverse indices returned from the HISTOGRAM command. (See HISTOGRAM documentation.)
;          
; :Examples:
;    Set the `Example` keyword to use the data from the reference page.
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
;    Change History::
;       Written by:  David W. Fanning, 13 November 2012, from a program named OTSU_THRESHOLD by Carl Salvaggio and
;           modified by Gianguido Cianci.
;       The OTSU_THRESHOLD algorithm used previously made many assumptions about the data. The algorithm used here
;           has been completely rewritten to comply with the values in the reference page and to avoid making 
;           assumptions about the data used to create the histogram. 21 November 2012. DWF.
;       Modified to set L64 keyword if data type GE 14 (suggested by user). 22 November 2012. DWF.
;       Modified the threshold value to use DIndGen instead of IndGen, which was causing incorrect
;           results with integer data. 24 November 2012. DWF.
;         
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgOTSU_THRESHOLD, $        ; The program name.
   data, $                          ; The data to threshold.
   BINSIZE=binsize, $               ; The histogram bin size.
   EXAMPLE=example, $               ; Set this keyword to see the reference page example.
   HISTDATA=histdata, $             ; The output of the HISTOGRAM command.
   L64=l64, $                       ; Input for HISTOGRAM.
   LOCATIONS=locations, $           ; The histogram bin locations.
   MAX=max, $                       ; The maximum value to HISTOGRAM.
   MIN=min, $                       ; The minimum value to HISTOGRAM.
   MISSING=missing, $               ; The value that indicates "missing" data to be excluded from the histgram.
   NAN=nan, $                       ; Check for NAN.
   NBINS=nbins, $                   ; The number of bins to display.
   OMAX=omax, $                     ; The maximum value of the histogram on output.
   OMIN=omin, $                     ; The minimum value of the histogram on output.
   PLOTIT=plotit, $                 ; Set this keyword to see the results of the thresholding algorithm.
   REVERSE_INDICES=ri               ; The reverse indices of the histogram.
    
   Compile_Opt idl2

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message()
      IF N_Elements(nancount) EQ 0 THEN BEGIN
            IF N_Elements(_data) NE 0 THEN data = Temporary(_data)
      ENDIF ELSE BEGIN
            IF nancount EQ 0 THEN BEGIN
                IF N_Elements(_data) NE 0 THEN data = Temporary(_data)
            ENDIF
      ENDELSE
      RETURN, -9999
   ENDIF
   
   ; Need data or the EXAMPLE keyword to continue.
   IF N_Elements(data) EQ 0 && ~Keyword_Set(example) THEN BEGIN
       Message, 'Data values to threshold are required.'
   ENDIF
   
   ; Are we doing an example? Use the data from the reference page at
   ; http://www.labbookpages.co.uk/software/imgProc/otsuThreshold.html.
   IF Keyword_Set(example) THEN BEGIN
      data = [Replicate(0,8), Replicate(1,7), Replicate(2,2), Replicate(3,6), Replicate(4,9), Replicate(5,4)]
      binsize = 1
   ENDIF
   
   ; Get the data type. Important to match data type with binsize type. Otherwise
   ; strange things occur in the Histogram command.
   dataType = Size(data, /TYPE)
   
   ; If this is byte data, then use a BINSIZE of 1, unless instructed otherwise.
   IF dataType EQ 1 THEN BEGIN
      IF (N_Elements(binsize) EQ 0) && (N_Elements(nbins) EQ 0) THEN binsize = 1B
   ENDIF
   
   ; If the data type is 14 or above, set the L64 keyword. Necessary to give enough
   ; precision in the Otsu calculations.
   IF dataType GE 14 THEN L64 = 1
      
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
   IF N_Elements(min) EQ 0 THEN min = Min(_data, NAN=nan)
   IF N_Elements(max) EQ 0 THEN max = Max(_data, NAN=nan)

   ; Calculate the histogram.
    histdata = Histogram(_data, $
      BINSIZE=binsize, $
      L64=l64, $
      MAX=max, $
      MIN=min, $
      NAN=nan, $
      LOCATIONS=locations, $
      OMAX=omax, $
      OMIN=omin, $
      REVERSE_INDICES=ri)
      
   ; Lot's of bad things can happen next. Let's pretend we don't know about them.
   except = !Except
   !Except = 0
   
   ; The threshold values to evaluate.
   thresholds = DIndGen(N_Elements(histdata)) * binsize + oMin
   
   ; Create a cumulative distribution to calculate the weighting factors.
   ; Subscripting of the background weights and addition of a 0 value
   ; is necessary to conform with the outputs in the reference documenation.
   ; I presume it is because the first threshold should be on the near side
   ; of the first bin, rather than on the far side.
   cdf = Total(histdata, /DOUBLE, /CUMULATIVE)
   reverseCDF = Total(Reverse(histdata), /DOUBLE, /CUMULATIVE)
   Wb = [0,cdf[0:N_Elements(cdf)-2]] / Total(histdata)
   Wf = Reverse(reverseCDF / Total(histdata))
   
   ; Find the means. 
   mu_b = Total(histdata * thresholds, /DOUBLE, /CUMULATIVE) / cdf
   mu_b = [0, mu_b[0:N_Elements(mu_b)-2]]
   mu_f = Reverse(Total(Reverse(histdata) * Reverse(thresholds), /DOUBLE, /CUMULATIVE) / reverseCDF)

   ; Calculate the Between-Class variance.
   betweenClassVariance = Wb * Wf * (mu_b - mu_f)^2
   
   ; The threshold is found by locating the maximum value and
   ; obtaining the index into the array.
   maximumVariance = Max(betweenClassVariance, thresholdIndex)
   threshold = thresholdIndex*binsize + oMin

   ; Useful printouts if we are doing the example.
   IF Keyword_Set(example) THEN BEGIN
       Print, 'Wb:        ', Wb
       Print, 'Wf:        ', Wf
       Print, 'Mu_b:      ', mu_b
       Print, 'Mu_f:      ', mu_f
       Print, 'Variance:  ', betweenClassVariance
       Print, 'Threshold: ', threshold
       
       cgDisplay, Title='Example OTSU Threshold Method', /Free
       !P.Multi = [0,1,2]
       cgHistoplot, _data, Binsize=binsize, /Fill
       cgPlots, [threshold, threshold], !Y.CRange, Color='blue', Thick=2
       cgPlot, betweenClassVariance, Title='Between Class Variance'
       cgPlots, [threshold, threshold], !Y.CRange, Color='blue', Thick=2
       cgText, 0.23, 2.60, 'Threshold: ' + String(threshold, Format='(I0)'), Color='blue', Font=0
       !P.Multi = 0
   ENDIF
   
   ; Need a plot?
   IF Keyword_Set(plotit) THEN BEGIN
       cgDisplay, Title='OTSU Threshold Results', /Free
       !P.Multi = [0,1,2]
       cgHistoplot, _data, $
          BINSIZE=binsize, $
          L64=l64, $
          LOCATIONS=locations, $
          MAXINPUT=max, $
          MININPUT=min, $
          NAN=nan, $
          /Fill
       cgPlots, [threshold, threshold], !Y.CRange, Color='blue', Thick=2
       cgPlot, locations, betweenClassVariance, Title='Between Class Variance Threshold: ' + $
           String(threshold,Format='(F0.2)'), XStyle=1
       cgPlots, [threshold, threshold], !Y.CRange, Color='blue', Thick=2
       !P.Multi = 0
   ENDIF
   
   ; Clean up.
   !Except = except
   
   ; Return result.
   RETURN, threshold
   
END