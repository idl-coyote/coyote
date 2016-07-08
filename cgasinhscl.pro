; docformat = 'rst'
;
; NAME:
;   cgASinHScl
;
; PURPOSE:
;   This is a utility routine to perform an inverse hyperbolic sine
;   function intensity transformation on an image. I think of this
;   as a sort of "tuned" gamma or power-law function. The algorithm,
;   and notion of "asinh magnitudes", comes from a paper by Lupton,
;   et. al, in The Astronomical Journal, 118:1406-1410, 1999 September.
;   I've relied on the implementation of Erin Sheldon, found here:
;
;      http://cheops1.uchicago.edu/idlhelp/sdssidl/plotting/tvasinh.html
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2015, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; This is a utility routine to perform an inverse hyperbolic sine
; function intensity transformation on an image. I think of this
; as a sort of "tuned" gamma or power-law function. The algorithm,
; and notion of "asinh magnitudes", comes from a paper by Lupton,
; et. al, in The Astronomical Journal, 118:1406-1410, 1999 September.
; I've relied on the implementation of Erin Sheldon, found here::
;
;    http://cheops1.uchicago.edu/idlhelp/sdssidl/plotting/tvasinh.html
;
; I'm also grateful of discussions with Marshall Perrin on the IDL
; newsgroup with respect to the meaning of the "softening parameter", beta,
; and for finding (and fixing!) small problems with the code.
;
; Essentially this transformation allow linear scaling of noise values,
; and logarithmic scaling of signal values, since there is a small
; linear portion of the curve and a much large logarithmic portion of
; the curve. (See the EXAMPLE section for some tips on how to view this
; transformation curve.)
;
; :Categories:
;    Image Processing
;
; :Examples:
;     Plot various values of beta::
;         cgPlot,  cgASinhScl(Indgen(256), Beta=0.0), LineStyle=0
;         cgOPlot, cgASinhScl(Indgen(256), Beta=0.1), LineStyle=1
;         cgOPlot, cgASinhScl(Indgen(256), Beta=1.0), LineStyle=2
;         cgOPlot, cgASinhScl(Indgen(256), Beta=10.), LineStyle=3
;         cgOPlot, cgASinhScl(Indgen(256), Beta=100), LineStyle=4
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
;       Written by:  David W. Fanning, 24 February 2006.
;       Removed ALPHA keyword and redefined the BETA keyword to correspond
;         to the "softening parameter" of Lupton et. al., following the
;         suggestions of Marshall Perrin. 25 April 2006. DWF.
;       Renamed cgASinhScl from ASinhScl. 27 March 2015. DWF.
;       Yikes! Two instances of naming problems from 2015! Fixed. 8 July 2016. DWF.
;       
; :Copyright:
;     Copyright (c) 2008-2016, Fanning Software Consulting, Inc.
;-

;+
; Return the inverse hyperbolic sine of the argument. Taken from the NASA
; IDL Astronomy Library and renamed for use in this program. The inverse 
; hyperbolic sine is used for the calculation of asinh magnitudes, see 
; Lupton et al. (1999, AJ, 118, 1406). Expression given in  Numerical Recipes, 
; Press et al. (1992), eq. 5.6.7. Note that asinh(-x) = -asinh(x) and that 
; asinh(0) = 0. and that if y = asinh(x) then x = sinh(y).
; 
; :Returns:
;    The inverse hyperbolic sine is returned. The output has the same number
;    of elements as X and is double precision if X is double, otherwise floating point.
;
; :Params:
;    x: in, required
;       The hyperbolic sine, numeric scalar or vector or multidimensional array
;       (not complex).
;-
FUNCTION cgASinhScl_ASinh, x

   On_Error, 2

    y = ALog( Abs(x) + SQRT( x^2 + 1.0) )

    index = Where(x LT 0 ,count)
    IF count GT 0 THEN y[index] = -y[index]

    RETURN, y

 END ;-------------------------------------------------------------------------------


 ;+
 ;
 ; The main cgASinhScl function.
 ; 
 ; :Returns:
 ;     A byte scaled image is returned.
 ;
 ; :Params:
 ;    image: in, required
 ;       The image to be scaled. Written for 2D images, but arrays of any size are treated alike.
 ;
 ; :Keywords:
 ;     beta: in, optional, type=float, default=3.0
 ;         This keyword corresponds to the "softening parameter" in the Lupon et. al paper.
 ;         This factor determines the input level at which linear behavior sets in. Beta
 ;         should be set approximately equal to the amount of "noise" in the input signal.
 ;         If BETA=0 there is a very small linear portion of the curve; if BETA=200 the
 ;         curve is essentially all linear. The default value of BETA is set to 3, which
 ;         is appropriate for a small amount of noise in your signal. The value is always
 ;         positive.
 ;
 ;     max: in, optional
 ;          Any value in the input image greater than this value is set to this value
 ;          before scaling.
 ;
 ;     min: in, optional
 ;          Any value in the input image less than this value is set to this value
 ;          before scaling.
 ;
 ;     negative, in, optional, type=boolean, default=0
 ;          If set, the "negative" of the result is returned.
 ;
 ;     omax: in, optional, type=byte, default=255
 ;          The output image is scaled between OMIN and OMAX.
 ;
 ;     omin: in, optional, type=byte, default=0
 ;          The output image is scaled between OMIN and OMAX.
 ;-
FUNCTION cgASinhScl, image, $
   BETA=beta, $
   NEGATIVE=negative, $
   MAX=maxValue, $
   MIN=minValue, $
   OMAX=maxOut, $
   OMIN=minOut

   ; Return to caller on error.
   On_Error, 2

   ; Check arguments.
   IF N_Elements(image) EQ 0 THEN Message, 'Must pass IMAGE argument.'

   ; Check for underflow of values near 0. Yuck!
   curExcept = !Except
   !Except = 0
   i = Where(image GT -1e-35 AND image LT 1e-35, count)
   IF count GT 0 THEN image[i] = 0.0
   void = Check_Math()
   !Except = curExcept

   ; Work in double precision.
   output = Double(image)

   ; Too damn many floating underflow warnings, no matter WHAT I do! :-(
   thisExcept = !Except
   !Except = 0

   ; Perform initial scaling of the image into 0 to 1.0.
   output = cgScaleVector(Temporary(output), 0.0, 1.0, MaxValue=maxValue, $
      MinValue=minValue, /NAN, Double=1)

   ; Check keywords.
   IF N_Elements(beta) EQ 0 THEN beta = 3.0D
   IF N_Elements(maxOut) EQ 0 THEN maxOut = 255B ELSE maxout = 0 > Byte(maxOut) < 255
   IF N_Elements(minOut) EQ 0 THEN minOut = 0B ELSE minOut = 0 > Byte(minOut) < 255
   IF minOut GE maxout THEN Message, 'OMIN must be less than OMAX.'

   ; Create a non-linear factor from the BETA value.
   scaled_beta = ((beta > 0) - minValue)/(maxValue - minValue)
   nonlinearity = 1.0D/(scaled_beta > 1e-12)

  ; Find out where 0 and 1 map in ASINH, then set these as MINVALUE and MAXVALUE
   ; in next cgScaleVector call. This is necessary to preserve proper scaling.
   extrema = cgASinhScl_ASinh([0, 1.0D] * nonlinearity)

   ; Inverse hyperbolic sine scaling.
   output = cgScaleVector(cgASinhScl_ASinh(Temporary(output)*nonlinearity), $
      minOut, maxOut, /NAN, Double=1, MinValue=extrema[0], MaxValue=extrema[1])

   ; Clear math errors.
   void = Check_Math()
   !Except = thisExcept

   ; Does the user want the negative result?
   IF Keyword_Set(negative) THEN RETURN, BYTE(maxout - Round(output) + minOut) $
      ELSE RETURN, BYTE(Round(output))

 END ;-------------------------------------------------------------------------------
