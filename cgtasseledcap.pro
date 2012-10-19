; docformat = 'rst'
;
; NAME:
;   cgTasseledCap
;
; PURPOSE:
; 
;  This function returns an image transformed by tasseled cap coefficients. The number of
;  bands returned depends on the shape of the tasseled cap coefficients, but at least three
;  bands are always returned: brightness, greenness, and yellowness.
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
;  This function returns an image transformed by tasseled cap coefficients. The number of
;  bands returned depends on the shape of the tasseled cap coefficients, but at least three
;  bands are always returned: brightness, greenness, and yellowness.
;
; :Categories:
;    Utility
;    
; :Params:
;     input: in, required, type=varies
;        The input may be either an image to be transformed or the name of an image
;        file that can be read with READ_IMAGE (e.g., a GeoTiff file).
;    
; :Keywords:
;     display: in, optional, type=boolean
;        Set this keyword to display the brightness, greenness, and yellowness band in 
;        a display window.
;     envi: in, optional, type=boolean
;       Set this keyword to return the tasseled cap coeficients used by ENVI 4.8.
;       These are reported (by ITTVIS) to be for LandSat-5 TM data, although others
;       report these are in error for TM-5 data and really apply to TM-4 data.
;       Reference: http://www.exelisvis.com/language/en-US/UserCommunity/UserForums/forumid/29/threadid/12985/scope/posts.aspx.
;     etm: in, optional, type=boolean
;       Set this keyword to return the tasseled cap coeficients proposed by the USGS for
;       LandSat-7 ETM+ images. Note that these values should be applied to reflectances.
;       Reference: http://landcover.usgs.gov/pdf/tasseled.pdf.
;     imagine: in, optional, type=boolean
;       Set this keyword to return the tasseled cap coeficients used by the ERDAS Imagine software
;       for LandSat-5 data.
;     ls4: in, optional, type=boolean
;       Set this keyword to return the tasseled cap coeficients reported by Crist, Laurin and Cicone
;       for LandSat-4 imagery. Reference: http://www.ciesin.org/docs/005-419/005-419.html.
;     ls5: in, optional, type=boolean
;       Set this keyword to return the tasseled cap coeficients reported by Crist, Laurin and Cicone
;       for LandSat-5 imagery. Reference: http://www.ciesin.org/docs/005-419/005-419.html.
;     mss: in, optional, type=boolean
;       Set this keyword to return the tasseled cap coeficients originally reported by R.J Kauth and G.S. Thomas
;       for MSS LandSat imagery. Reference: http://web.pdx.edu/~jduh/courses/Archive/geog481w07/Students/Marcello_TasselledCap.pdf.
;     quickbird: in, optional, type=boolean
;       Set this keyword to return the tasseled cap Graham-Schmidt coeficients reportedly used for Quickbird imagery.
;       Reference: http://www.asprs.org/a/publications/proceedings/pecora16/Yarbrough_L.pdf
;     tm: in, optional, type=boolean
;       Set this keyword to return the tasseled cap coeficients originally proposed for TM imagery by Kauth and Thomas.
;       Reference: http://web.pdx.edu/~jduh/courses/Archive/geog481w07/Students/Marcello_TasselledCap.pdf.
;
; :Examples:
;    tasselImage = cgTasseledCap('landsatImageFile.tif', /TM, /Display)
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
;     Written, 24 August 2012, by David W. Fanning.
;     Fixed a dimension problem caused by forgetting a Transpose operation. 17 October 2012. DWF.
;     Fixed a type problem with the ETM keyword. 19 October 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgTasseledCap, input, $
   DISPLAY=display, $
   ENVI=envi, $
   ETM=etm, $
   IMAGINE=imagine, $
   LS4=ls4, $
   LS5=ls5, $
   MSS=mss, $
   QUICKBIRD=quickbird, $
   TM=tm
   
   Compile_Opt idl2
   
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       IF (N_Elements(putback) NE 0) && putback THEN input = Temporary(image)
       RETURN, input
   ENDIF
   
   ; Must set a cgTasseledCapCoeffs
   IF Total(Keyword_Set(envi) + Keyword_Set(etm) + Keyword_Set(imagine) + $
      Keyword_Set(ls4) + Keyword_Set(ls5) + Keyword_Set(mss) + Keyword_Set(quickbird) + $
      Keyword_Set(tm)) EQ 0 THEN Message, 'One of the Tasseled Cap Coefficient keywords must be set.'
   
   ; Is the input a filename or an actual image?
   IF Size(input, /TNAME) EQ 'STRING' THEN BEGIN
       rootName = cgRootName(input, DIRECTORY=filedir, EXTENSION=ext)
       image = Read_Image(input)
       putback = 0
   ENDIF ELSE BEGIN
       image = Temporary(input)
       putback = 1
   ENDELSE
   
   ; This MUST be a three-dimensional image.
   IF Size(image, /N_DIMENSIONS) NE 3 THEN Message, 'The input image must have three dimensions.'
   dims = Size(image, /DIMENSIONS)
   bands = Min(dims, bandindex)
   CASE bandindex OF
      0: 
      1: image = Transpose(image, [1,0,2])
      2: image = Transpose(image, [2,0,1])
   ENDCASE
   dims = Size(image, /DIMENSIONS)
   tasseledImage = Reform(image, dims[0], dims[1]*dims[2])
   xdim = dims[1]
   ydim = dims[2]
   
   ; If this is a 7 band image, we need to eliminate band 6.
   IF bands EQ 7 THEN BEGIN
       tasseledImage = tasseledImage[[0,1,2,3,4,6], *]
       bands = 6
   ENDIF
   
   ; Get the coefficients.
   coeffs = cgTasseledCapCoeffs( $
       ENVI=envi, $
       ETM=etm, $
       IMAGINE=imagine, $
       LS4=ls4, $
       LS5=ls5, $
       MSS=mss, $
       QUICKBIRD=quickbird, $
       TM=tm)
       
   ; Make sure the column dimension of the tasseledImage matches the row dimension of the coefficients.
   ; If it doesn't, just use the first X dimensions of the input image.
   s = Size(coeffs, /DIMENSIONS)
   row = s[1]
   ncoeffs = s[0]
   s = Size(tasseledImage, /DIMENSIONS)
   col = s[0]
   IF col NE row THEN tasseledImage = tasseledImage[Indgen(row), *]
   
   ; Perform the transform.
   tasseledImage = Reform(tasseledImage ## coeffs, ncoeffs, xdim, ydim)
   tasseledImage = Transpose(tasseledImage, [1,2,0])
   
   ; Add values to make all bands positive numbers.
   FOR j=0,ncoeffs-1 DO BEGIN
        temp = tasseledImage[*,*,j]
        minVal = Min(temp)
        IF minVal LT 0 THEN tasseledImage[*,*,j] = Temporary(temp) + Abs(minVal)
   ENDFOR
   
   ; If this is a TIFF image, we need to reverse in Y.
   IF N_Elements(ext) NE 0 THEN BEGIN
        IF StrUpCase(ext) EQ 'TIF' THEN tasseledImage = Reverse(tasseledImage,2)
   ENDIF
   
   ; Clean-up and return.
   IF putback THEN input = Temporary(image)
   
   IF Keyword_Set(display) THEN BEGIN
       cgDisplay, 1200, 400, /Free, Title='Tasseled Cap Transformation'
       !P.Multi=[0,3,1]
       LoadCT, 0
       FOR j=0,2 DO BEGIN
          cgImage, tasseledImage[*,*,j], Stretch=2
       ENDFOR
       cgText, 0.05, 0.025, /Normal, Font=0, 'Brightness', Color='red'
       cgText, 0.38, 0.025, /Normal, Font=0, 'Greeness', Color='red'
       cgText, 0.71, 0.025, /Normal, Font=0, 'Yellowness', Color='red'
       !P.Multi=0    
   ENDIF
   
   RETURN, tasseledImage
   
END