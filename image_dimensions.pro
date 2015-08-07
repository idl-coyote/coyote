; docformat = 'rst'
;
; PURPOSE: 
; The purpose of this function is to return the various dimensions of the image,
; and also to extract relevant image information via output keywords. The
; function works only with 2D and 3D (24-bit) images, with or without alpha
; channels.
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
; The purpose of this function is to return the various dimensions of the image,
; and also to extract relevant image information via output keywords. The
; function works only with 2D and 3D (24-bit) images, with or without alpha
; channels.
;   
; :Categories:
;    Utilities
;    
; :Returns:
;     A vector containing the size of each dimension of the image. It is equivalent
;     to calling the SIZE function with the DIMENSIONS keyword set.
;       
; :Params:
;    image:  in, optional, type=various
;        The image variable from which information is to be obtained.
;       
; :Keywords:
;     alphachannel: out, optional, type=boolean
;        This keyword is set to 1 if there is an alpha channel in the image. Otherwise,
;        the keyword is set to 0. 
;     trueindex: out, optional, type=integer
;        The position of the "true color" index in the return value. Is -1 for 2D images.
;     xindex: out, optional, type=integer
;        The index (position) of the X dimension in the return value.
;     xsize: out, optional, type=integer
;        The X size of the image.
;     yindex: out, optional, type=integer
;        The index (position) of the Y dimension in the return value.
;     ysize: out, optional, type=integer
;        The Y size of the image.
;        
; :Examples:
;    To load open a window of the appropriate size and display a 24-bit image::
;
;       dims = Image_Dimensions(image24, XSize=xsize, YSize=ysize, TrueIndex=trueindex)
;       Window, XSIZE=xsize, YSIZE=ysize
;       TV, image24, TRUE=trueindex
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;        David W. Fanning 
;        1645 Sheely Drive
;        Fort Collins, CO 80526 USA
;        Phone: 970-221-0438
;        E-mail: david@idlcoyote.com
;        Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; :History:
;    Modification History::
;       Written by:  David W. Fanning, 5 March 2003.
;       Added support for alpha channel images, include ALPHACHANNEL keyword. 13 May 2009. DWF.
;
; :Copyright:
;     Copyright (c) 2003-2011, Fanning Software Consulting, Inc.
;-
FUNCTION Image_Dimensions, image, $

; This function returns the dimensions of the image, and also
; extracts relevant information via output keywords. Works only
; with 2D and 3D (24-bit) images.
   
   AlphaChannel=alphaChannel, $ ; Output keyword that indicates presence of alpha channel in image.
   XSize=xsize, $               ; Output keyword. The X size of the image.
   YSize=ysize, $               ; Output keyword. The Y size of the image.
   TrueIndex=trueindex, $       ; Output keyword. The position of the "true color" index. -1 for 2D images.
   XIndex=xindex, $             ; Output keyword. The position or index of the X image size.
   YIndex=yindex                ; Output keyword. The position or index of the Y image size.

    On_Error, 2
    
    ; Get the number of dimensions and the size of those dimensions.
    ndims = Size(image, /N_Dimensions)
    dims =  Size(image, /Dimensions)
    alphaChannel = 0
    
    ; Is this a 2D or 3D image?
    IF ndims EQ 2 THEN BEGIN
       xsize = dims[0]
       ysize = dims[1]
       trueindex = -1
       xindex = 0
       yindex = 1
    ENDIF ELSE BEGIN
    
       IF ndims NE 3 THEN Message, /NoName, 'Unknown image dimensions. Returning.'
       
          ; This image could have an alpha channel, so we would have to look for a "4" instead of a "3".
          true = Where(dims EQ 3, count) 
          
          IF count EQ 0 THEN BEGIN
            true = Where(dims EQ 4, count)
            IF count GT 0 THEN alphaChannel = 1 ELSE alphaChannel = 0
          ENDIF
       trueindex = true[0]
       IF count EQ 0 THEN Message, /NoName, 'Unknown image type. Returning.'
       CASE true[0] OF
          0: BEGIN
             xsize = dims[1]
             ysize = dims[2]
             xindex = 1
             yindex = 2
             ENDCASE
          1: BEGIN
             xsize = dims[0]
             ysize = dims[2]
             xindex = 0
             yindex = 2
             ENDCASE
          2: BEGIN
             xsize = dims[0]
             ysize = dims[1]
             xindex = 0
             yindex = 1
             ENDCASE
       ENDCASE
    ENDELSE
    
    RETURN, dims
    
END
