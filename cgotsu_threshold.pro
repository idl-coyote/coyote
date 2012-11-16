; docformat = 'rst'
;
; NAME:
;   cgOTSU_THRESHOLD
;
; PURPOSE:
;   The purpose of this function is to find an optimal threshold for separating
;   a bimodal distribution of pixels in an image histogram. The Otsu Threshold method
;   is used: http://en.wikipedia.org/wiki/Otsu's_method.
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
; `Otsu Method <http://en.wikipedia.org/wiki/Otsu's_method>`. 
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
;    histdata: out, optional
;       The output value of the internal HISTOGRAM command.
;    l64: in, optional, type=boolean, default=0                       
;       If set, the return value of HISTOGRAM are 64-bit integers, rather than
;       the default 32-bit integers.
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
;    reverse_indices: out, optional
;       The list of reverse indices returned from the HISTOGRAM command. (See HISTOGRAM documentation.)
;          
; :Examples:
;    I use the program to separate satellite bimodal images into two groups of pixels::
;         threshold = cgOTSU_Threshold(redBand)
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
;         
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgOTSU_THRESHOLD, $        ; The program name.
   data, $                          ; The data to threshold.
   BINSIZE=binsize, $               ; The histogram bin size.
   HISTDATA=histdata, $             ; The output of the HISTOGRAM command.
   L64=l64, $                       ; Input for HISTOGRAM.
   LOCATIONS=locations, $           ; The histogram bin locations.
   MAX=max, $                       ; The maximum value to HISTOGRAM.
   MIN=min, $                       ; The minimum value to HISTOGRAM.
   MISSING=missing, $               ; The value that indicates "missing" data to be excluded from the histgram.
   NAN=nan, $                       ; Check for NAN.
   NBINS=nbins, $                   ; The number of bins to display.
   OMAX=omax, $
   OMIN=omin, $
   REVERSE_INDICES=ri
    
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

   ; Compute the probability denisty function
   pdf = histdata / Total(histdata, /DOUBLE)
   reversedPDF = Reverse(pdf)
   minimumDC = min
   maxDC = omax

   ; Iterate through all possible thresholds and compute the interclass variance
   ; at each level.
   cdf = Total(pdf, /DOUBLE, /CUMULATIVE)
   omega1 = cdf[0:N_Elements(pdf)-2]
   mu1 = Total(pdf * Indgen(maxDC), /DOUBLE, /CUMULATIVE) / cdf
   omega2 = (Reverse(Total(reversedPDF, /DOUBLE, /CUMULATIVE)))[1:*]
   mu2 = Reverse(Total( reversedPDF * (maxDC - Indgen(maxDC)), /DOUBLE, /CUMULATIVE)) / omega2
   interclassVariance = [0, omega1 * omega2 * (mu1 - mu2)^2]

   ; Determine the threshold by finding the level at which the maximum interclass
   ; variance occurs.
   maximumVariance = Max(interclassVariance, threshold)
   threshold = threshold + minimumDC

   ; Clean up.
   !Except = except
   
   ; Return result.
   RETURN, threshold
   
END