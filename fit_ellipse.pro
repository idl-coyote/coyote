;+
; NAME:
;       Fit_Ellipse
;
; PURPOSE:
;
;       This program fits an ellipse to an ROI given by a vector of ROI indices.
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
;       Graphics, math.
;
; CALLING SEQUENCE:
;
;       ellipsePts = Fit_Ellipse(indices)
;
; OPTIONAL INPUTS:
;
;       indices - A 1D vector of pixel indices that describe the ROI. For example,
;            the indices may be returned as a result of the WHERE function.
;
; OUTPUTS:
;
;       ellipsePts - A 2-by-npoints array of the X and Y points that describe the
;            fitted ellipse. The points are in the device coodinate system.
;
; INPUT KEYWORDS:
;
;       NPOINTS - The number of points in the fitted ellipse. Set to 120 by default.
;       
;       SCALE - A two-element array that gives the scaling parameters for each X and Y pixel, respectively.
;            Set to [1.0,1.0] by default.
;
;       XSIZE - The X size of the window or array from which the ROI indices are taken.
;            Set to !D.X_Size by default.
;
;       YSIZE - The Y size of the window or array from which the ROI indices are taken.
;            Set to !D.Y_Size by default.
;
; OUTPUT KEYWORDS:
;
;       CENTER -- Set to a named variable that contains the X and Y location of the center
;            of the fitted ellipse in device coordinates.
;
;       ORIENTATION - Set to a named variable that contains the orientation of the major
;            axis of the fitted ellipse. The direction is calculated in degrees
;            counter-clockwise from the X axis.
;
;       AXES - A two element array that contains the length of the major and minor
;            axes of the fitted ellipse, respectively.
;
;       SEMIAXES - A two element array that contains the length of the semi-major and semi-minor
;            axes of the fitted ellipse, respectively. (This is simple AXES/2.)
;
;  EXAMPLE:
;
;       LoadCT, 0, /Silent
;       image = BytArr(400, 300)+125
;       image[180:245, 125:175] = 255B
;       indices = Where(image EQ 255)
;       Window, XSize=400, YSize=300
;       TV, image
;       PLOTS, Fit_Ellipse(indices, XSize=400, YSize=300), /Device, Color=cgColor('red')
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, April 2002. Based on algorithms provided by Craig Markwardt
;            and Wayne Landsman in his TVEllipse program.
;       Added SCALE keyword and modified the algorithm to use memory more efficiently.
;            I no longer have to make huge arrays. The arrays are only as big as the blob
;            being fitted. 17 AUG 2008. DWF.
;       Fixed small typo that caused blobs of indices with a longer X axis than Y axis
;            to misrepresent the center of the ellipse. 23 February 2009.
;-
;******************************************************************************************;
;  Copyright (c) 2008-2009, by Fanning Software Consulting, Inc.                           ;
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
FUNCTION Fit_Ellipse, indices, $
    AXES=axes, $
    CENTER=center, $
    NPOINTS=npoints, $
    ORIENTATION=orientation, $
    SEMIAXES=semiAxes, $
    SCALE=scale, $
    XSIZE=xsize, $
    YSIZE=ysize

    ; The following method determines the "mass density" of the ROI and fits
    ; an ellipse to it. This is used to calculate the major and minor axes of
    ; the ellipse, as well as its orientation. The orientation is calculated in
    ; degrees counter-clockwise from the X axis.
    
    IF N_Elements(xsize) EQ 0 THEN xsize = !D.X_Size
    IF N_Elements(ysize) EQ 0 THEN ysize = !D.Y_Size
    IF N_Elements(npoints) EQ 0 THEN npoints = 120
    IF N_Elements(scale) EQ 0 THEN scale = [1.0, 1.0]
    IF N_Elements(scale) EQ 1 THEN scale = [scale, scale]

    ; Fake indices for testing purposes.
    IF N_Elements(indices) EQ 0 THEN BEGIN
       xs = xsize / 4
       xf = xsize / 4 * 2
       ys = ysize / 4
       yf = ysize / 4 * 2
       array = BytArr(xsize, ysize)
       array[xs:xf, ys:yf] = 255B
       indices = Where(array EQ 255)
    ENDIF
    
    ; Convert the indices to COL/ROW coordinates. Find min and max values
    xyindices = Array_Indices([xsize,ysize], indices, /DIMENSIONS)
    minX = Min(xyindices[0,*], MAX=maxX)
    minY = Min(xyindices[1,*], MAX=maxY)
    cols = Reform(xyindices[0,*]) - minX
    rows = Reform(xyindices[1,*]) - minY

    ; Make an array large enough to hold the blob.
    arrayXSize = maxX-minX+1
    arrayYSize = maxY-minY+1
    array = BytArr(arrayXSize, arrayYSize)
    array[xyindices[0,*] - minX, xyindices[1,*] - minY] = 255B
    totalMass = Total(array)
    xcm = Total( Total(array, 2) * Indgen(arrayXSize) * scale[0] ) / totalMass
    ycm = Total( Total(array, 1) * Indgen(arrayYSize) * scale[1] ) / totalMass
    center = [xcm, ycm] 

    ; Obtain the position of every pixel in the image, with the origin
    ; at the center of mass of the ROI.
    x = Findgen(arrayXSize) * scale[0]
    y = Findgen(arrayYSize) * scale[1]
    xx = (x # (y * 0 + 1)) - (xcm)
    yy = ((x * 0 + 1) # y) - (ycm)
    npts = N_Elements(indices)

    ; Calculate the mass distribution tensor.
    i11 = Total(yy[cols, rows]^2) / npts
    i22 = Total(xx[cols, rows]^2) / npts
    i12 = -Total(xx[cols, rows] * yy[cols, rows]) / npts
    tensor = [[ i11, i12],[i12,i22]]

    ; Find the eigenvalues and eigenvectors of the tensor.
    evals = Eigenql(tensor, Eigenvectors=evecs)

    ; The semi-major and semi-minor axes of the ellipse are obtained from the eigenvalues.
    semimajor = Sqrt(evals[0]) * 2.0
    semiminor = Sqrt(evals[1]) * 2.0

    ; We want the actual axes lengths.
    major = semimajor * 2.0
    minor = semiminor * 2.0
    semiAxes = [semimajor, semiminor]
    axes = [major, minor]

    ; The orientation of the ellipse is obtained from the first eigenvector.
    evec = evecs[*,0]

    ; Degrees counter-clockwise from the X axis.
    orientation = ATAN(evec[1], evec[0]) * 180. / !Pi - 90.0

    ; Divide a circle into Npoints.
    phi = 2 * !PI * (Findgen(npoints) / (npoints-1))

    ; Position angle in radians.
    ang = orientation / !RADEG

    ; Sin and cos of angle.
    cosang = Cos(ang)
    sinang = Sin(ang)

    ; Parameterized equation of ellipse.
    x =  semimajor * Cos(phi)
    y =  semiminor * Sin(phi)

    ; Rotate to desired position angle.
    xprime = xcm + (x * cosang) - (y * sinang)
    yprime = ycm + (x * sinang) + (y * cosang)

    ; Extract the points to return.
    pts = FltArr(2, N_Elements(xprime))
    pts[0,*] = xprime + minX
    pts[1,*] = yprime + minY
    center = center + [minX, minY]

    RETURN, pts
    
END
