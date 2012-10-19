; docformat = 'rst'
;
; NAME:
;   cgTasseledCapCoeffs
;
; PURPOSE:
; 
;  This function returns tasseled cap coefficients from various sources to perform
;  the tasseled cap transformation on appropriate (e.g. LandSat) images.
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
; This function returns tasseled cap coefficients from various sources to perform
; the tasseled cap transformation on appropriate (e.g. LandSat) images. The program
; requires that one of the keywords be set.
;
; :Categories:
;    Utility
;    
; :Keywords:
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
;       Also: http://www.sjsu.edu/faculty/watkins/tassel.htm.
;     quickbird: in, optional, type=boolean
;       Set this keyword to return the tasseled cap Graham-Schmidt coeficients reportedly used for Quickbird imagery.
;       Reference: http://www.asprs.org/a/publications/proceedings/pecora16/Yarbrough_L.pdf
;     tm: in, optional, type=boolean
;       Set this keyword to return the tasseled cap coeficients originally proposed for TM imagery by Kauth and Thomas.
;       Reference: http://web.pdx.edu/~jduh/courses/Archive/geog481w07/Students/Marcello_TasselledCap.pdf.
;       Also: http://www.sjsu.edu/faculty/watkins/tassel.htm.
;
; :Examples:
;    tasseledCapTransformedImage = landsat_tm_image ## cgTasselecCapCoeffs(/TM)
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
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgTasseledCapCoeffs, $
   ENVI=envi_tm5, $
   ETM=etm_landsat, $
   IMAGINE=mss_imagine, $
   LS4=landsat_4, $
   LS5=landsat_5, $
   MSS=mss_landsat, $
   QUICKBIRD=quickbird, $
   TM=tm_landsat
   
   On_Error, 2 ; Return to caller.

    CASE 1 OF
    
        Keyword_Set(landsat_4): BEGIN
        ; Coefficients from: http://www.ciesin.org/docs/005-419/tab1.gif.
        ; http://www.ciesin.org/docs/005-419/005-419.html. TM-4 Apply to DN values.
        coefficients = FltArr(6,3)
        ;                     Band1     Band2    Band3   Band4    Band5    Band7
        coefficients[*,0] = [ 0.3037,  0.2793,  0.4743, 0.5585,  0.5082,  0.1863] ; Brightness
        coefficients[*,1] = [-0.2848, -0.2435, -0.5436, 0.7243,  0.0840, -0.1800] ; Greenness
        coefficients[*,2] = [ 0.1509,  0.1973,  0.3279, 0.3406, -0.7112, -0.4572] ; Yellowness
        END
        
        Keyword_Set(landsat_5): BEGIN
        ; Coefficients from: http://www.ciesin.org/docs/005-419/tab2.gif.
        ; http://www.ciesin.org/docs/005-419/005-419.html. TM-5 Apply to DN values.
        coefficients = FltArr(6,3)
        ;                     Band1     Band2    Band3   Band4    Band5    Band7
        coefficients[*,0] = [ 0.2909,  0.2493,  0.4806, 0.5568,  0.4438,  0.1706] ; Brightness + 10.3695 additive term
        coefficients[*,1] = [-0.2728, -0.2174, -0.5508, 0.7221,  0.0733, -0.1648] ; Greenness  + -0.7310 additive term
        coefficients[*,2] = [ 0.1446,  0.1761,  0.3322, 0.3396, -0.6210, -0.4186] ; Yellowness + -3.3828 additive term
        END

        Keyword_Set(envi_tm5): BEGIN
        ; Coefficients from: http://www.exelisvis.com/language/en-US/UserCommunity/UserForums/forumid/29/threadid/12985/scope/posts.aspx.
        ; TM-5 (although someone claims these are TM-4 values!). Apply to DN values.
        coefficients = FltArr(6,3)
        ;                     Band1     Band2      Band3     Band4     Band5     Band7
        coefficients[*,0] = [ 0.33183,  0.33121,  0.55177,  0.42514,  0.48087,  0.25252] ; Brightness
        coefficients[*,1] = [-0.24717, -0.16263, -0.40639,  0.85469,  0.05493, -0.11749] ; Greenness
        coefficients[*,2] = [ 0.13929,  0.22490,  0.40359,  0.25178, -0.70133, -0.45732] ; Yellowness
        END

        Keyword_Set(mss_landsat): BEGIN
        ; Coefficients from: http://web.pdx.edu/~jduh/courses/Archive/geog481w07/Students/Marcello_TasselledCap.pdf.
        ; These are from R.J Kauth and G.S. Thomas original 1976 article: http://docs.lib.purdue.edu/cgi/viewcontent.cgi?article=1160&context=lars_symp&sei-redir=1&referer=http%3A%2F%2Fscholar.google.com%2Fscholar_url%3Fhl%3Den%26q%3Dhttp%3A%2F%2Fdocs.lib.purdue.edu%2Fcgi%2Fviewcontent.cgi%253Farticle%253D1160%2526context%253Dlars_symp%26sa%3DX%26scisig%3DAAGBfm3MWqyNxnJ7UYudI9atplMsxTPgzw%26oi%3Dscholarr#search=%22http%3A%2F%2Fdocs.lib.purdue.edu%2Fcgi%2Fviewcontent.cgi%3Farticle%3D1160%26context%3Dlars_symp%22
        coefficients = FltArr(4,4)
        ;                     Band1     Band2    Band3    Band4
        coefficients[*,0] = [ 0.4330,  0.6320,  0.5860,  0.2640] ; Brightness
        coefficients[*,1] = [-0.2900, -0.5620,  0.6000,  0.4910] ; Greenness
        coefficients[*,2] = [-0.8290,  0.5220, -0.0390,  0.1940] ; Yellowness
        coefficients[*,3] = [ 0.2230,  0.0120, -0.5430,  0.8100] ; Yellowness
        END
        
        Keyword_Set(mss_imagine): BEGIN
        ; Coefficients from ERDAS Imagine software
        coefficients = FltArr(4,3)
        ;                     Band1   Band2   Band3   Band4
        coefficients[*,0] = [ 0.332,  0.603,  0.675,  0.262] ; Brightness
        coefficients[*,1] = [-0.283, -0.660,  0.577,  0.388] ; Greenness
        coefficients[*,2] = [-0.899,  0.428,  0.076, -0.041] ; Yellowness
        END

        Keyword_Set(quickbird): BEGIN
        ; Gram-Schmidt coefficients from: http://www.asprs.org/a/publications/proceedings/pecora16/Yarbrough_L.pdf
        coefficients = FltArr(4,3)
        ;                     Band1   Band2   Band3   Band4
        coefficients[*,0] = [ 0.319,  0.543,  0.490,  0.604] ; Brightness
        coefficients[*,1] = [-0.121, -0.331, -0.517,  0.780] ; Greenness
        coefficients[*,2] = [ 0.652,  0.375, -0.639, -0.163] ; Yellowness
        END
        
        Keyword_Set(TM_landsat): BEGIN
        ; Coefficients from: http://web.pdx.edu/~jduh/courses/Archive/geog481w07/Students/Marcello_TasselledCap.pdf.
        ; Should be applied to reflectance values only? Of the same order as ENVI TM-5.
        coefficients = FltArr(6,3)
        ;                     Band1     Band2    Band3    Band4   Band5   Band7
        coefficients[*,0] = [ 0.3037,  0.2793,  0.4343, 0.5585,  0.5082,  0.1863] ; Brightness
        coefficients[*,1] = [-0.2848, -0.2435, -0.5436, 0.7243,  0.0840, -0.1800] ; Greenness
        coefficients[*,2] = [ 0.1509,  0.1793,  0.3299, 0.3406, -0.7112, -0.4572] ; Yellowness
        END
        
        Keyword_Set(etm_landsat): BEGIN
        ; Coefficients from: http://landcover.usgs.gov/pdf/tasseled.pdf.
        coefficients = FltArr(6,6)
        ;                     Band1     Band2    Band3    Band4    Band5    Band7
        coefficients[*,0] = [ 0.3561,  0.3972,  0.3904,  0.6966,  0.2286,  0.1596] ; Brightness
        coefficients[*,1] = [-0.3344, -0.3544, -0.4556,  0.6966, -0.0242, -0.2630] ; Greenness
        coefficients[*,2] = [ 0.2626,  0.2141,  0.0926,  0.0656, -0.7629, -0.5388] ; Yellowness
        coefficients[*,3] = [ 0.0805, -0.0498,  0.1950, -0.1327,  0.5752, -0.7775] ; Fourth
        coefficients[*,4] = [-0.7252, -0.0202,  0.6683,  0.0631, -0.1494, -0.0274] ; Fifth
        coefficients[*,5] = [ 0.4000, -0.8172,  0.3832,  0.0602, -0.1095,  0.0985] ; Sixth
        END
        
        ELSE: Message, 'Must set a keyword to specify type of Tasseled Cap coefficients required.'
        
    ENDCASE
    
    ; Transpose the coefficient matrix so that you can apply image ## coefficients to get tasseled cap transformations.
    ; Note that image bands must be in columns, with band number in rows in the "image" variable.
    RETURN, Transpose(coefficients)
    
END