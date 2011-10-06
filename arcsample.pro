;+
; NAME:
;       ARCSAMPLE
;
; PURPOSE:
;
;       Given X and Y points that describe a closed curve in 2D space,
;       this function returns an output curve that is sampled a specified
;       number of times at approximately equal arc distances.
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

;       Utilities
;
; CALLING SEQUENCE:
;
;       ArcSample, x_in, y_in, x_out, y_out
;
; INPUT_PARAMETERS:
;
;       x_in:          The input X vector of points.
;       y_in:          The input Y vector of points.
;
; OUTPUT_PARAMETERS:
;
;      x_out:          The output X vector of points.
;      y_out:          The output Y vector of points.
;
; KEYWORDS:
;
;     POINTS:         The number of points in the output vectors. Default: 50.
;
;     PHASE:          A scalar between 0.0 and 1.0, for fine control of where interpolates
;                     are sampled. Default: 0.0.
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 1 December 2003, based on code supplied
;          to me by Craig Markwardt.
;-
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
PRO ArcSample, x_in, y_in, x_out, y_out, POINTS=points, PHASE=phase

   ; Check parameters.

   IF N_Elements(points) EQ 0 THEN points = 50
   IF N_Elements(phase) EQ 0 THEN phase = 0.0 ELSE phase = 0.0 > phase < 1.0

   ; Make sure the curve is closed (first point same as last point).

   npts = N_Elements(x_in)
   IF (x_in[0] NE x_in[npts-1]) OR (y_in[0] NE y_in[npts-1]) THEN BEGIN
      x_in = [x_in, x_in[0]]
      y_in = [y_in, y_in[0]]
      npts = npts + 1
   ENDIF

   ; Interpolate very finely.

   nc = (npts -1) * 100
   t = DIndgen(npts)
   t1 = DIndgen(nc + 1) / 100
   x1 = Spl_Interp(t, x_in, Spl_Init(t, x_in), t1)
   y1 = Spl_Interp(t, y_in, Spl_Init(t, y_in), t1)

   avgslopex = (x1(1)-x1(0) + x1(nc)-x1(nc-1)) / (t1(1)-t1(0)) / 2
   avgslopey = (y1(1)-y1(0) + y1(nc)-y1(nc-1)) / (t1(1)-t1(0)) / 2


   dx1 = Spl_Init(t, x_in, yp0=avgslopex, ypn_1=avgslopex)
   dy1 = Spl_Init(t, y_in, yp0=avgslopey, ypn_1=avgslopey)
   x1 = Spl_Interp(t, x_in, dx1, t1)
   y1 = Spl_Interp(t, y_in, dy1, t1)

  ; Compute cumulative path length.

  ds = SQRT((x1(1:*)-x1)^2 + (y1(1:*)-y1)^2)
  ss = [0d, Total(ds, /Cumulative)]

  ; Invert this curve, solve for TX, which should be evenly sampled in
  ; the arc length space.

  sx = DIndgen(points) * Max(ss)/points + phase
  tx = Spl_Interp(ss, t1, Spl_Init(ss, t1), sx)

  ; Reinterpolate the original points using the new values of TX.

  x_out = Spl_Interp(t, x_in, dx1, tx)
  y_out = Spl_Interp(t, y_in, dy1, tx)

  x_out = [x_out, x_out[0]]
  y_out = [y_out, y_out[0]]

END
