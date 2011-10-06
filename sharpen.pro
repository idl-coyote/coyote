;+
; NAME:
;       Sharpen
;
; PURPOSE:
;
;        This function sharpens an image using a Laplacian kernel.
;        The final result is color adjusted to match the histogram
;        of the input image.
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
;       Image Processing
;
; CALLING SEQUENCE:
;
;       sharp_image = Sharpen(image)
;
; INPUTS:
;
;       image - The input image to be sharpened. Assumed to be a 2D byte array.
;
; OUTPUTS:
;
;       sharp_image - The sharpened image.
;
; INPUT KEYWORDS:
;
;       KERNEL -- By default the image is convolved with this 3-by-3 Laplacian kernel:
;           [ [-1, -1, -1], [-1, +8, -1], [-1, -1, -1] ].  You can pass in any  kernel
;           of odd width. The filtered image is added back to the original image to provide
;           the sharpening effect.
;
;       DISPLAY -- If this keyword is set a window is opened and the details of the sharpening
;           process are displayed.
;
; OUTPUT KEYWORDS:
;
;       None.
;
; DEPENDENCIES:
;
;       None.
;
; METHOD:
;
;       This function is based on the Laplacian kernel sharpening method on pages 128-131
;       of Digital Image Processing, 2nd Edition, Rafael C. Gonzalez and Richard E. Woods,
;       ISBN 0-20-118075-8.
;
; EXAMPLE:
;
;       There is an example program at the end of this file.
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, January 2003.
;       Updated slightly to use Coyote Library routines. 3 Dec. 2010. DWF.
;       Modified the example to work with cgImage. 29 March 2011. DWF.
;-
;
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
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

FUNCTION Sharpen_HistoMatch, image, histogram_to_match

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel

      ; Get the call stack and the calling routine's name.

   Help, Calls=callStack
   IF Float(!Version.Release) GE 5.2 THEN $
      callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0] ELSE $
      callingRoutine = (Str_Sep(StrCompress(callStack[1])," "))[0]

      ; Print a traceback.

   Help, /Last_Message, Output=traceback
   Print,''
   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
   Print, ''
   FOR j=0,N_Elements(traceback)-1 DO Print, "     " + traceback[j]

   IF N_Elements(image) NE 0 THEN RETURN, image ELSE RETURN, -1L
ENDIF

   ; We require two input parameters.

IF N_Params() NE 2 THEN Message, 'Two arguments required. Please read the program documentation.'

   ; Must have 2D image array.

IF Size(image, /N_Dimensions) NE 2 THEN Message, 'Image argument must be 2D. Returning.'

   ; Is the histogram_to_match variable a 1D or 2D array? Branch accordingly.

CASE Size(histogram_to_match, /N_Dimensions) OF
   1: BEGIN
      IF N_Elements(histogram_to_match) NE 256 THEN $
         Message, 'Histogram to match has incorrect size. Returning.'
      match_histogram =    histogram_to_match
      END
   2: match_histogram = Histogram(Byte(histogram_to_match), Min=0, Max=255, Binsize=1)
   ELSE: Message, 'Histogram to match has incorrect number of dimensions. Returning.'
ENDCASE

   ; Calculate the histogram of the input image.

h = Histogram(Byte(image), Binsize=1, Min=0, Max=255)

   ; Make sure the two histograms have the same number of pixels. This will
   ; be a problem if the two images are different sizes, you are matching a
   ; histogram from an image subset, etc.

totalPixels = Float(N_Elements(image))
totalHistogramPixels = Float(Total(match_histogram))

IF totalPixels NE totalHistogramPixels THEN $
   factor = totalPixels / totalHistogramPixels ELSE $
   factor = 1.0

match_histogram = match_histogram * factor

   ; Find a mapping from the input pixels to the transformation function s.

s = FltArr(256)
FOR k=0,255 DO BEGIN
  s[k] = Total(h(0:k) / totalPixels)
ENDFOR

   ; Find a mapping from input histogram to the transformation function v.

v = FltArr(256)
FOR q=0,255 DO BEGIN
  v[q] = Total(match_histogram(0:q) / Total(match_histogram))
ENDFOR

   ; Find probablitly density function z from v and s.

z = BytArr(256)
FOR j=0,255 DO BEGIN
   i = Where(v LT s[j], count)
   IF count GT 0 THEN z[j] = (Reverse(i))[0] ELSE z[j]=0
ENDFOR

   ; Create the matched image.

matchedImage = z[Byte(image)]
RETURN, matchedImage
END
; ----------------------------------------------------------------------------



FUNCTION Sharpen, image, Display=display, Kernel=kernel

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel

      ; Get the call stack and the calling routine's name.

   Help, Calls=callStack
   IF Float(!Version.Release) GE 5.2 THEN $
      callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0] ELSE $
      callingRoutine = (Str_Sep(StrCompress(callStack[1])," "))[0]

      ; Print a traceback.

   Help, /Last_Message, Output=traceback
   Print,''
   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
   Print, ''
   FOR j=0,N_Elements(traceback)-1 DO Print, "     " + traceback[j]

   IF N_Elements(image) NE 0 THEN RETURN, image ELSE RETURN, -1L
ENDIF

   ; If an image is not provided. Issue an error message.

IF N_Elements(image) EQ 0 THEN $
   Message, 'A 2D image is required as an argument.'

IF Size(image, /N_Dimensions) NE 2 THEN Message, 'Image must be a 2D array in this program.'

   ; Resize the image, if required.

previewSize = 512
wxsize = previewSize
wysize = previewSize

   ; Set up the convolution kernel for Laplacian filtering.

IF N_Elements(kernel) EQ 0 THEN BEGIN
   k = Replicate(-1, 3, 3)
   k[1,1] = 8
ENDIF ELSE BEGIN
   s = Size(kernel, /Dimensions)
   IF s[0] MOD 2 NE 1 THEN Message, 'Kernel must be an odd width.'
   k = kernel
ENDELSE

   ; Are we doing a display?

IF Keyword_Set(display) THEN BEGIN
   s = Size(image, /Dimensions)
   xsize = s[0]
   ysize = s[1]
   needresize = 1
   IF xsize NE ysize THEN BEGIN
      needresize = 1
      aspect = Float(ysize) / xsize
      IF aspect LT 1 THEN BEGIN
         wxsize = previewSize
         wysize = (previewSize * aspect) < previewSize
      ENDIF ELSE BEGIN
         wysize = previewSize
         wxsize = (previewSize / aspect) < previewSize
      ENDELSE
   ENDIF

   Window, /Free, XSize=2*wxsize, YSize=2*wysize, Title='Image Sharpening-Laplacian'

ENDIF ELSE needresize = 0

   ; Need a resize?

IF needresize THEN thisImage = Byte(Congrid(image, wxsize, wysize)) ELSE $
   thisImage = image

   ; Display the original image.

IF Keyword_Set(display) THEN BEGIN $
   cgImage, thisImage, 0, 0, /TV
   XYOUTS, wxsize/2, 10,  /Device, 'Original Image', Font=0, $
      Alignment=0.5, Color=cgColor('red6')
ENDIF

   ; Create the Laplacian filtered image.

filteredImage = Convol(Float(thisImage), k, Center=1, /Edge_Truncate, /NAN)

   ; Display the filtered image.

IF Keyword_Set(display) THEN BEGIN
   fimage = Convol(thisImage, k, Center=1, /Edge_Truncate, /NAN)
   cgImage, fimage, wxsize, wysize, /TV
   XYOUTS, (2*wxsize/4)*3, wysize + 10, /Device, 'Filtered Image', Font=0, $
      Alignment=0.5, Color=cgColor('red6')
ENDIF

   ; Scale the Laplacian filtered image. Note conversion of
   ; image to integer values and use of 255 as a FLOAT value.

filteredImage = filteredImage - (Min(filteredImage))
filteredImage = filteredImage * (255./Max(filteredImage))
IF Keyword_Set(display) THEN BEGIN
   cgImage, filteredImage, 0, wysize, /TV
   XYOUTS, wxsize/2, wysize + 10, /Device, 'Scaled Filter', Font=0, $
      Alignment=0.5, Color=cgColor('red6')
ENDIF

   ; Create the sharpened image by adding the Laplacian filtered image
   ; back to the original image and re-scaling.

sharpened = thisImage + filteredImage
sharpened = sharpened - (Min(sharpened))
sharpened = sharpened * (255./Max(sharpened))

   ; Adjust the sharpened image to match the histogram of the original.

adjusted = Sharpen_HistoMatch(sharpened, image)

   ; Display the adjusted image.

IF Keyword_Set(display) THEN BEGIN
   cgImage, BytScl(adjusted), wxsize, 0, /TV
   XYOUTS, (2*wxsize/4)*3, 10, /Device, 'Sharpened Image', Font=0, $
      Alignment=0.5, Color=cgColor('red6')
ENDIF

RETURN, adjusted
END


PRO Example

image = cgDemoData(13)
s = Size(image, /Dimensions)
LoadCT, 0, /Silent
Window, /Free, XSize=s[0]*2, YSize=s[1], Title='Image Sharpening'
cgImage, image, 0, /TV
XYOuts, 0.25, 0.1, /Normal, Alignment=0.5, 'Original Image', Font=0, Color=cgColor('red6')
cgImage, Sharpen(image), 1, /NoErase, /TV
XYOuts, 0.75, 0.1, /Normal, Alignment=0.5, 'Sharpened Image', Font=0, Color=cgColor('red6')
END
