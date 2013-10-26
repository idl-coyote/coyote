; docformat = 'rst'
;
; NAME:
;  cgSnake
;
; PURPOSE:
;  This function applies the Gradient Vector Flow active contour algorithm, as described by
;  Chenyang Xu and Jerry L. Prince in "Snakes, Shapes, and Gradient Vector Flow" in the March
;  1998 IEEE Transactions on Image Processing, Vol. 7, No.3. Additional information,
;  including references to research papers, is available via Cheyang Xu's web page:
;  http://iacl.ece.jhu.edu/projects/gvf/.
;
;  Active contours are often described as "snakes" because they writhe and move
;  under the influence of external and internal forces, toward a feature of interest
;  in an image, usually an edge. This program gives the user the opportunity
;  to control both external and internal forces to find an optimal set of active contour
;  parameters. Active contours are most often used with images to find and describe
;  regions of interest.
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
;+--------------------------------------------------------------------------
;  This program applies the Gradient Vector Flow active contour algorithm, as described by
;  Chenyang Xu and Jerry L. Prince in "Snakes, Shapes, and Gradient Vector Flow" in the March
;  1998 IEEE Transactions on Image Processing, Vol. 7, No.3. Additional information,
;  including references to research papers, is available via Cheyang Xu's
;  `web page <http://iacl.ece.jhu.edu/projects/gvf/>`.
;
;  Active contours are often described as "snakes" because they writhe and move
;  under the influence of external and internal forces, toward a feature of interest
;  in an image, usually an edge. This program gives the user the opportunity
;  to control both external and internal forces to find an optimal set of active contour
;  parameters. Active contours are most often used with images to find and describe
;  regions of interest.
;  
;  This program requires the GVF_Snake object, which can be purchased at the 
;  `Coyote Store <http://www.idlcoyote.com/coyotestore/index.php>`.
;
; :Categories:
;    Image Processing
;    
; :Returns:
;     The function returns an ROI structure containing the deformed points of the final contour
;     in addition to other information. The return structure looks like this::
;     
;        roiStruct = { npts: 0L  ; The length of the X and Y fields in the structure.
;                      x:   0.0  ; The X values (in image coordinates) of the final contour.
;                      y:   0.0  ; The Y values (in image coordinates) of the final contour.
;                      perimeter:  0.0 ; The perimenter length of the ROI.
;                      area: 0.0 ; The area enclosed by the ROI.
;                      values:   ; A vector of length npts, giving the value of the image at (x,y).
;                     }
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
; :Params:
;     image: in, required
;        The image for which the active contour (snake) will be applied.
;        This argument must be 2D. The user will be asked to select an
;        image file if this argument is not provided.
;     x_init: in, required, type=float
;        The initial X points of the active contour or snake. Must be paired with Y. 
;        Assume values are pixel locations within image.
;     y_init: in, required, type=float
;        The initial Y points of the active contour or snake. Must be paired with X. 
;        Assume values are pixel locations within image.
;       
;
; :Keywords:
;     alpha: in, optional, type=float, default=0.10
;        The elasticity parameter of the active contour. It reflects the contour's
;        ability to stretch along its length.
;     beta: in, optional, type=float, default=0.25
;        The rigidity parameter of the active contour. It reflects the contour's
;        ability to bend, as, for example, around corners.
;     blur: in, optional, type=boolean, default=1
;        Set this keyword to 1 if you want a Gaussian Blur applied to image before
;        processing. Set it to 0 otherwise.
;     delta_max: in, optional, type=float, default=5.50
;        The maximum pixel distance for adaptive interpolation.
;     delta_min: in, optional, type=float, default=0.25
;        The minimum pixel distance for adaptive interpolation.
;     gamma: in, optional, type=float, default=1.0
;        The viscosity parameter. Larger values make it harder to deform the active
;        contour in space.
;     gradientscale: in, optional, type=float, default=1.75
;        A multiplication factor for the gradient calculations.
;     kappa: in, optional, type=float, default=1.25
;        The external force weight.
;     gvf_iterations: in, optional, type=integer, default=30
;        The number of iterations for calculating the Gradient Vector Flow (GVF).
;     iterations: in, optional, type=integer, default=120
;        The number of iterations to use in calculating the snake positions.
;     max_value: in, optional, type=varies
;        The maximum value for scaling the image data to create contrast for the edge mask.
;     min_value: in, optional, type=varies
;        The minimum value for scaling the image data to create contrast for the edge mask.
;     mu: in, optional, type=float, default=0.10
;        The regularization parameter. This should be set according to the amount of
;        noise in the image. Use a larger value for noisier images.
;     parameterfile: in, optional, type=string
;        The name of a parameter file created with the ActiveContour program and containing
;        most of the snake parameters set here with other keywords.
;     sigma: in, optional, type=float, default=1.0
;        The standard deviation or sigma of the Gaussian function used in Gaussian
;        blurring.
;     spatial_scale: in, optional, type=double, default=1.0D
;        Set this keyword to a two-element array that specifies the pixel scale in
;        the X and Y directions ([xscale, yscale]). The scale factors are applied
;        when the perimeter and area calculations for the final contour is made.
;        Default is [1.0D, 1.0D].
;
; :History:
;    Modification History::
;       Written by David W. Fanning, 25 October 2013, based on ActiveContour program from 2003.
;
; :Copyright:
;    Copyright (c) 2013, Fanning Software Consulting, Inc.
;---------------------------------------------------------------------------
FUNCTION cgSnake, image, x_init, y_init, $
   ALPHA=alpha, $
   BETA=beta, $
   BLUR=blur, $
   DELTA_MAX=dmax, $
   DELTA_MIN=dmin, $
   DISPLAY_IMAGE=display_image, $
   GAMMA=gamma, $
   GRADIENTSCALE=gradientscale, $
   GVF_ITERATIONS=gvf_iterations, $
   ITERATIONS=iterations, $
   KAPPA=kappa, $
   MAX_VALUE=max_v, $
   MIN_VALUE=min_v, $
   MU=mu, $
   PARAMETERFILE=parameterFile, $
   SIGMA=sigma, $
   SPATIAL_SCALE=spatial_scale

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      IF !Error_State.Code EQ -94 THEN BEGIN ; Attempt to call undefined function...
          Print, '   The GVF_Snake object was not found. You may have to purchase this object'
          Print, '   from the Coyote Store (www.idlcoyote.com/coyotestore) to continue.'
          Print, '   If you have purchased it, please be sure it can be found on your IDL path.'
          void = Dialog_Message('GVF_Snake object not found. Please see command log for details.')
          RETURN, -1
      ENDIF ELSE BEGIN
          void = Error_Message()
          RETURN, -1
      ENDELSE
   ENDIF
   
   ; Create the snake object.
   snakeObj = Obj_New('GVF_Snake', image, x_init, y_init, $
       ALPHA=alpha, $
       BETA=beta, $
       BLUR=blur, $
       DELTA_MAX=dmax, $
       DELTA_MIN=dmin, $
       DISPLAY_IMAGE=display_image, $
       GAMMA=gamma, $
       GRADIENTSCALE=gradientscale, $
       GVF_ITERATIONS=gvf_iterations, $
       ITERATIONS=iterations, $
       KAPPA=kappa, $
       MAX_VALUE=max_v, $
       MIN_VALUE=min_v, $
       MU=mu, $
       SIGMA=sigma, $
       SPATIAL_SCALE=spatial_scale)
       
   ; Nothing to do if you can't make a snake object.
   IF ~Obj_Valid(snakeObj) THEN RETURN, -1
       
   ; Preform the gradient manipulation.
   roi = snakeObj -> ApplySnake(Cancel=cancelled)
   
   ; Return the ROI if you have one.
   IF ~cancelled THEN RETURN, roi ELSE RETURN, -1
   
END