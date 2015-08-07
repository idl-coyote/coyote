; docformat = 'rst'
;
; NAME:
;   cgHistogram
;
; PURPOSE:
;   This program is used as a wrapper to the Histogram command in IDL. It removes the 
;   very real possibility that the Histogram command will return incorrect values silently.
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
; This program is used as a wrapper to the Histogram command in IDL. It works around a bug
; in the Histogram command when byte data is binned in versions prior to IDL 8.2, and it takes
; care to match the data type of the `BinSize` keyword to the data type of the data being binned.
; If this matching is not done, Histogram silently returns incorrect results. I have added the ability to
; smooth the data (with the `Smooth` keyword) and to return the relative frequency of the histogram,
; rather than the histogram counts (with the `Frequency` keyword). The relative frequency is a 
; number between 0 and 1. I have also added the ability to specify "missing" data that should not be
; binned.
;
; :Categories:
;    General
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
;       calculate the histogram of. If it is not, VERY strange things can happen, but the
;       worst is that HISTOGRAM silently returns incorrect results. I try hard to avoid this
;       result in this program.
;    frequency: in, optional, type=boolean, default=0
;       If this keyword is set, the relative frequency is returned, rather than the 
;       histogram counts. Relative frequency is a number between 0 and 1. The total of
;       all the relative frequencies should equal 1.0.
;    input: in, optional
;       Set this keyword to a named variable that contains an array to be added to the 
;       output of cgHistogram. The density function of `data` is added to the existing 
;       contents of `Input` and returned as the result. The array is converted to 
;       longword type if necessary and must have at least as many elements as are 
;       required to form the histogram. Multiple histograms can be efficiently 
;       accumulated by specifying partial sums via this keyword.
;    l64: in, optional, type=boolean, default=0                       
;       If set, the return value of HISTOGRAM are 64-bit integers, rather than
;       the default 32-bit integers. Set by default if 64-bit integers are passed in.
;    locations: out, optional
;       Starting locations of each bin. `Locations` has the same number of elements as the result, 
;       and has the same data type as the input data array.
;    max: in, optional
;       The maximum value to use in calculating input histogram. 
;    min: in, optional
;       The minimum value to use in calculating input histogram. 
;    missing: in, optional
;       The value that should be represented as "missing" and not used in the histogram.
;       Be aware that if the input data is not of type "float" or "double" that the input
;       data will be converted to floating point prior to calculating the histogram.
;    nan: in, optional, type=boolean, default=0   
;       If set, ignore NAN values in calculating and plotting histogram. Set by default if the
;       `Missing` keyword is used.
;    nbins: in, optional, type=integer
;       The number of output bins in the histogram. The meaning is slightly different from
;       the meaning in the HISTOGRAM command. Used only to calculate BINSIZE when BINSIZE is
;       not specified. In this case, binsize = rangeofData/(nbins-1). When the number of bins
;       is low, the results can be non-intuitive. For this reason, I would discourage the use
;       of `NBins` in favor of the `BinSize` keyword.
;    omax: out, optional
;       The maximum output value used to construct the histogram. (See HISTOGRAM documentation.)
;    omin: out, optional
;       The minimum output value used to construct the histogram. (See HISTOGRAM documentation.)
;    reverse_indices: out, optional
;       The list of reverse indices returned from the HISTOGRAM command. (See HISTOGRAM documentation.)
;    smooth: in, optional, type=integer, default=0
;       Set this keyword to an odd positive integer to smooth the histogram output before plotting.
;       The integer will set the width of a smoothing box to be applied to the histogram data with
;       the Smooth function. This keyword is ignored if the `Frequency` keyword is set.
;          
; :Examples:
;    Create a normal distribution of random numbers and take the histogram::
;    
;       numbers = RandomU(-3L, 1000, /Normal)
;       histResults = cgHistogram(numbers, Binsize=0.25)
;       cgPlot, histResults
;       
;    Additional examples of histogram plots can be found here::
;    
;        http://www.idlcoyote.com/gallery/index.html
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
;       Written by:  David W. Fanning, 7 March 2013.
;        
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;-
FUNCTION cgHistogram, $       ; The program name.
   data, $                    ; The data to draw a histogram of.
   BINSIZE=binsize, $         ; The histogram bin size.
   FREQUENCY=frequency, $     ; Plot relative frequency, rather than density.
   INPUT=input, $             ; Add this array to output of cgHistogram.
   L64=l64, $                 ; Input for HISTOGRAM.
   LOCATIONS=locations, $     ; The starting locations of each bin.
   MAX=max, $                 ; The maximum value to HISTOGRAM.
   MIN=min, $                 ; The minimum value to HISTOGRAM.
   MISSING=missing, $         ; The value that indicates "missing" data to be excluded from the histgram.
   NAN=nan, $                 ; Check for NAN.
   NBINS=nbins, $             ; The number of bins to display.
   OMAX=omax, $               ; The maximum value used to construct the histogram.
   OMIN=omin, $               ; The minimum value used to construct the histogram.
   REVERSE_INDICES=ri, $      ; The vector that identifies indices in each bin.
   SMOOTH=smooth              ; Run a smoothing filter of this width over the histogram data before returning.
    
   Compile_Opt idl2

   ; Catch any error in the cgHistogram program.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = cgErrorMsg()
      IF N_Elements(nancount) EQ 0 THEN BEGIN
         IF N_Elements(_data) NE 0 THEN data = Temporary(_data)
      ENDIF ELSE BEGIN
         IF nancount EQ 0 THEN BEGIN
             IF N_Elements(_data) NE 0 THEN data = Temporary(_data)
         ENDIF
      ENDELSE
      RETURN, data
   ENDIF

   ; Check for parameters.
   IF N_Elements(data) EQ 0 THEN Message, 'Must pass data to cgHistogram.'
   frequency = Keyword_Set(frequency)
   l64 = Keyword_Set(l64)
   IF N_Elements(smooth) NE 0 THEN BEGIN
     IF (smooth MOD 2) NE 0 THEN smooth = smooth + 1
   ENDIF
   
   ; What kind of data are we doing a HISTOGRAM on?
   dataType = Size(data, /TYPE)
   
   ; If this is 64-bit integers, set the L64 flag.
   IF dataType GE 14 THEN L64 =1 
      
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
   
   ; Define minimum and maximum input values, if not defined otherwise.
   IF N_Elements(min) EQ 0 THEN min = Min(_data, NAN=nan)
   IF N_Elements(max) EQ 0 THEN max = Max(_data, NAN=nan)

   ; Check for histogram keywords.
   IF N_Elements(binsize) EQ 0 THEN BEGIN
      range = Max(_data < max, /NAN) - Min(_data > min, /NAN)
      IF N_Elements(nbins) EQ 0 THEN BEGIN  ; Scott's Choice
         binsize = (3.5D * StdDev(min > _data < max, /NAN))/N_Elements(_data)^(1./3.0D) 
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

   ; Calculate the histogram. Can't use the INPUT keyword unless you actuall
   ; have something in it.
   IF N_Elements(input) EQ 0 THEN BEGIN
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
   ENDIF ELSE BEGIN
        histdata = Histogram(_data, $
          BINSIZE=binsize, $
          INPUT=input, $
          L64=l64, $
          MAX=max, $
          MIN=min, $
          NAN=nan, $
          LOCATIONS=locations, $
          OMAX=omax, $
          OMIN=omin, $
          REVERSE_INDICES=ri)
   ENDELSE
   ; Are you returning the frequency rather than the count? You can't smooth the frequency data.
   IF frequency THEN BEGIN
    
       histdata = Float(histdata)/N_Elements(_data)
       
   ENDIF ELSE BEGIN
   
      ; Do you need to smooth the data?
       IF N_Elements(smooth) NE 0 THEN histdata = Smooth(histdata, smooth)
       
   ENDELSE
   
   ; Return the data.
   RETURN, histdata
   
END
