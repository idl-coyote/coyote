; docformat = 'rst'
;
; NAME:
;   cgKrig2D
;
; PURPOSE:
; 
;   The cgKrig2D function interpolates a regularly or irregularly sampled set of points of
;   the form z = f(x, y) to produced a gridded 2D array using a statistical process known
;   as kriging.
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
; The cgKrig2D function interpolates a regularly or irregularly sampled set of points of
; the form z = f(x, y) to produced a gridded 2D array using a statistical process known
; as kriging. Kriging is a method of optimal interpolation based on regression against known
; or observed z values of surrounding data points, weighted according to spatial covariance
; values by various types of kriging model functions. Each grid location is estimated from
; observed values at surrounding locations. It is often used with spatial data.
; 
; Like all interpolation schemes, kriging can produces spurious results in extreme cases,
; but has the advantage of being able to compensate for the effects of data clustering and
; other, similar problems better than other interpolation methods such as inverse distance squared,
; splines, and triangulation methods. This particular version of Krig2D is orders of magnitude
; faster than the version of Krig2D that was distributed with IDL through IDL 8.2.3.
; 
; An excellent explanation of the kriging process can be found here::
; 
;    http://help.arcgis.com/en/arcgisdesktop/10.0/help/index.html#//009z00000076000000.htm
;    http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?TopicName=Semivariograms_and_covariance_functions
;    
; An explanation of the innovation that caused Krig2D to be made faster by several orders
; of magnitude can be found here::
; 
;    http://www.idlcoyote.com/code_tips/krigspeed.php
;    
; I've implemented the kriging mathematical models described in the following references::
; 
;    http://help.arcgis.com/en/arcgisdesktop/10.0/help/index.html#//009z00000076000000.htm
;    http://www.nbb.cornell.edu/neurobio/land/OldStudentProjects/cs490-94to95/clang/kriging.html 
;
; :Categories:
;    Math, Interpolation, Gridding
;    
; :Examples:
;    To create a dataset of N random points and determine the surface formed from such points::
;    
;       n = 500 ;# of scattered points
;       seed = -121147L ;For consistency
;       x = RANDOMU(seed, n)
;       y = RANDOMU(seed, n)
;
;      ; Create a dependent variable in the form a function of (x,y)
;      data = 3 * EXP(-((9*x-2)^2 + (7-9*y)^2)/4) + $
;         3 * EXP(-((9*x+1)^2)/49 - (1-0.9*y)) + $
;         2 * EXP(-((9*x-7)^2 + (6-9*y)^2)/4) - $
;         EXP(-(9*x-4)^2 - (2-9*y)^2)
;    
;      params = [0.5, 0.0]
;      interpArray = cgKrig2D(data, x, y, EXPONENTIAL=params, XOUT=xout, YOUT=yout)
;      cgSurf, interpArray, xout, yout, /Save
;      cgPlots, x, y, data, PSYM=2, Color='red', /T3D
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
;     Written, 15 Oct 2013, based on a fast varient of the Krig2D program in the IDL library.
;
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;-

;+
; The exponential kriging semivariogram model function. This model should be applied when spatial
; autocorrelation decreases exponentially with increasing distance.
;
; :Returns:
;    A two-dimensional array containing the covariance model.
;
; :Params:
;
;     d: in, required, type=float
;         The distance matrix of every point to each other.
;
;     t: in, required, type=float
;         A three-element vector containing, in this order, the values for the range, nugget, and
;         covariance value for the sample population (also called the partial sill), or [A, C0, C].
;         The sill is properly described as sill = (c0 + c).
;-
FUNCTION cgKrig2d_Exponential, d, t

    Compile_Opt idl2
    
    ; Make assignments that allow you to create the mathematical models described in the reference.
    a = t[0]
    c0 = t[1]
    c = t[2]
    
    result = c0 + c * ( 1.0 - Exp( (-3*Abs(d))/a ) )
    indices = Where(d EQ 0, count)
    IF count GT 0 THEN result[indices] = 0
    
    RETURN, result
END

;+
; The spherical kriging semivariogram model function. This model show a progressive decrease of
; spatial autocorrelation until some distance, beyond which the autocorrelation is zero. This is
; one of the most commonly used models.
; 
; :Returns:
;    A two-dimensional array containing the covariance model.
;
; :Params:
; 
;     d: in, required, type=float
;         The distance matrix of every point to each other.
;         
;     t: in, required, type=float
;         A three-element vector containing, in this order, the values for the range, nugget, and
;         covariance value for the sample population (also called the partial sill), or [A, C0, C].
;         The sill is properly described as sill = (c0 + c).
;-
FUNCTION cgKrig2d_Spherical, d, t

    Compile_Opt idl2
    
    ; Make assignments that allow you to create the mathematical models described in the reference.
    a = t[0]
    c0 = t[1]
    c = t[2]

    ; Set up the results for the spherical model. Find where distance GT a and where distance = 0.
    gtAIndices = Where(d GT a, gtACount)
    zeroIndices = Where(d EQ 0, zeroCount)
    r = d/a
    result = c0 + c * ( (1.5*r) - (0.5*r*r*r) )
    IF gtACount GT 0 THEN result[gtAIndices] = c0 + c
    IF zeroCount GT 0 THEN result[zeroIndices] = 0
    
    RETURN, result
END

;+
; This function interpolates a regularly or irregularly gridded set of points Z = F(X,Y) using kriging.
; One of the kriging models MUST be used in the call to cgKrig2D. These are `CIRCULAR`, `EXPONENTIAL`, 
; `GAUSSIAN`, `LINEAR`, or `SPHERICAL`. The only models currently tested and in use are EXPONENTIAL and SPHERICAL.
; 
; :Returns:
;    A two-dimensional array containing the interpolated surface, sampled at input locations.
;
; :Params:
;    z: in, required, type=float
;       An array containing the values of the data points as a function of X and Y. If
;       X and Y are provided, this vector should be the same length. If X and Y are not
;       provided, this array must be a 2D array. In this case the output grid is determined
;       by `XGRID` (or `XVALUES`) and `YGRID` (or `YVALUES`) keywords, and default values for 
;       `NX` and `NY` are determined by the 2D dimensions of the input data array. If X and Y  
;       are not provided, regular gridding is assumed. Otherwise, the input data is assumed 
;       to be irregularly gridded, unless the 'REGULAR` keyword is set.
;       
;    x: in, optional, type=float
;       An array containing the x locations of the surface to be gridded. If use, the `Y` data 
;       parameter must also be used and all three positional parameters must be the same length.
;       
;    y: in, optional, type=float
;       An array containing the y locations of the surface to be gridded. If use, the `X` data
;       parameter must also be used and all three positional parameters must be the same length.
;
; :Keywords:
; 
;    bounds: in, optional, type=array
;       Set this keyword to a four-element array [xmin, ymin, xmax, ymax] containing the 
;       grid limits of the output grid. If not provided, the grid limits are set to the extent
;       of the X and Y vectors.
;
;    double: in, optional, type=boolean, default=0
;       Set this keyword to perform all calculations in double precision floating point math.
;       Otherwise, the calculations are done in since precision floating point math. 
;       
;    exponential: in, optional, type=array
;       Set this keyword to a two- or three-element vector containing the kriging model parameters
;       [A, C0, C] for the kriging exponential model. The parameter A is the range. At distances beyond
;       A, the semivariogram or covariance remains essentially constant. The parameter C0 is the nugget.
;       Theoretically, a zero separation distance, the semivariogram model should be zero. But, sometimes
;       the semivariogram model displays a "lag" where the model function intercepts that Y axis at a 
;       location other than zero. This is called the "nugget". The parameter C, if it is present, is the
;       value at which autocorrelation ceases to exist. If it is not present, it is calculated as the sample 
;       variance. The value C0+C is called the sill, which is the variogram value for very large distances. One 
;       of the kriging model keywords, `EXPONENTIAL` or `SPHERICAL`, must be used in the call to cgKrig2D. 
;       
;       For exponential models, the semivariagram at distance d is given as:
;            C(d) = C1 * Exp(-3 * (d/A)   if d not equal 0.
;            C(d) = C1 + C0               if d equal 0.
;     
;    gs: in, optional, type=array
;        A two-element array [xs, ys] giving the grid spacing of the output grid, where xs is the spacing
;        in the horizontal spacing between grid points, and ys is the vertical spacing. The default is 
;        based on the extents of x and y. If the grid starts at x value xmin and ends at xmax, then the 
;        default horizontal spacing is (xmax - xmin)/(`NX`-1). The ys parameter is computed in the same way.  
;        The default grid size, if neither `NX` or `NY` are specified, is 51 by 51.
;
;    nx: in, optional, type=integer, default=51
;        The output grid size in the X direction. If not specified, it can be be inferred from the `GS`
;        and `BOUNDS` keywords. If not specified, and required by the code, a value of 51 is used.
;        
;    ny: in, optional, type=integer, default=51
;        The output grid size in the Y direction. If not specified, it can be be inferred from the `GS`
;        and `BOUNDS` keywords. If not specified, and required by the code, a value of 51 is used.
;
;    regular: in, optional, type=boolean, default=0
;        Set this keyword to indicate the `Data` parameter is a 2D array containing measurements on a regular grid.
;        It is rare to set this keyword, as it is set automatically under many circumstances.
;        
;    spherical: in, optional, type=array
;       Set this keyword to a two- or three-element vector containing the exponential model parameters
;       [A, C0, C] for the kriging spherical model. The parameter A is the range. At distances beyond
;       A, the semivariogram or covariance remains essentially constant. The parameter C0 is the nugget.
;       Theoretically, a zero separation distance, the semivariogram model should be zero. But, sometimes
;       the semivariogram model displays a "lag" where the model function intercepts that Y axis at a 
;       location other than zero. This is called the "nugget". The parameter C, if it is present, is the
;       value at which autocorrelation ceases to exist. If it is not present, it is calculated as the sample
;       variance. The value C0+C is called the sill, which is the variogram value for very large distances. One 
;       of the kriging model keywords, `EXPONENTIAL` or `SPHERICAL`, must be used in the call to cgKrig2D. 
;
;       For spherical models, the semivariagram at distance d is given as:
;            C(d) = c0 + C * ( ( 1.5 * (d/a) ) - ( 0.5 * (d/a)^3) )    if d less than a.
;            C(d) = C + C0                                             if d greater than a.
;            C(d) = 0                                                  if d equals 0.
;
;    xgrid: in, optional, type=array
;        Set this keyword to a two-element array, [xstart, xspacing] to indicate where the output grid starts
;        and what the horizontal spacing will be. Do not specify the `XVALUES` keyword if this keyword is used.
;     
;    xvalues: in, optional, type=array
;        Set this keyword to a vector of X location values corresponding to the equivalent Z values in the `Data`
;        parameter. Do not use this keyword if using the `XGRID` keyword.
;        
;    ygrid: in, optional, type=array
;        Set this keyword to a two-element array, [ystart, yspacing] to indicate where the output grid starts
;        and what the vertical spacing will be. Do not specify the `YVALUES` keyword if this keyword is used.
;
;    yvalues: in, optional, type=array
;        Set this keyword to a vector of Y location values corresponding to the equivalent Z values in the `Data`
;        parameter. Do not use this keyword if using the `YGRID` keyword.
;
;-
FUNCTION cgKrig2d, z, x, y, $
    BOUNDS=bounds, $
    DOUBLE=double, $
    EXPONENTIAL=exponential, $
    GS=gs, $
    NX=nx, $
    NY=ny, $
    REGULAR=regular, $
    SPHERICAL=spherical, $
    XGRID=xgrid, $
    XOUT=xout, $
    XVALUES=xvalues, $
    YGRID=ygrid, $
    YOUT=yout, $
    YVALUES=yvalues
    
    Compile_Opt idl2
    
    ;On_Error, 2 ; Return to caller.
    
    ; Require some kind of positional parameter.
    IF N_Params() EQ 0 THEN Message, 'Syntax: griddedArray = cgKrig2d(z, x, y)'
    IF N_Params() EQ 2 THEN Message, 'Syntax: griddedArray = cgKrig2d(z, x, y) or griddedArray = cgKrig2d(data2d)'
    
    ; Are we doing a regular grid? Then we only need the first positional parameter and it must be 2D.
    regular = Keyword_Set(regular) || (N_Params() EQ 1) 
    double = Keyword_Set(double)
    IF regular THEN BEGIN
        ndims = Size(z, /N_DIMENSIONS)
        IF ndims NE 2 THEN Message, 'Regular gridding requires a 2D data set be provided.'
        dims = Size(z, /DIMENSIONS)
        nx = dims[0]
        ny = dims[1]
    ENDIF ELSE BEGIN ; Otherwise, irregular grid and all three parameters must be present and same size.
        IF N_Params() NE 3 THEN Message, 'All three positional parameters must be present to do irregular gridding.'
        IF N_Elements(z) NE N_Elements(x) THEN Message, 'All three positional parameters must contain the same number of elements.'
        IF N_Elements(z) NE N_Elements(y) THEN Message, 'All three positional parameters must contain the same number of elements.'
        IF N_Elements(nx) EQ 0 THEN nx = 51
        IF N_ELements(ny) EQ 0 THEN ny = 51
    ENDELSE
    
    ; Kriging model parameters MUST be provided.
    IF (N_Elements(exponential) EQ 0) && (N_Elements(spherical) EQ 0) THEN BEGIN
        Message, 'Model parameters must be provided. Use EXPONENTIAL or SPHERICAL keyword.'
    ENDIF ELSE BEGIN
        
        IF N_Elements(exponential) NE 0 THEN BEGIN
            IF (N_Elements(exponential) LT 2) || (N_Elements(exponential) GT 3) THEN BEGIN
                Message, 'The EXPONENTIAL model parameter vector must contain two or three elements.'
            ENDIF
            t = exponential
            functionName = 'cgKrig2d_Exponential'
        ENDIF
        
        IF N_Elements(spherical) NE 0 THEN BEGIN
            IF (N_Elements(spherical) LT 2) || (N_Elements(spherical) GT 3) THEN BEGIN
                Message, 'The SPHERICAL model parameter vector must contain two or three elements.'
            ENDIF
            t = spherical
            functionName = 'cgKrig2d_Spherical'
        ENDIF
        
    ENDELSE
    
    ; Did the user specify an XGRID or XVALUES keyword? If so, regular gridding is implied.
    IF N_Elements(xgrid) EQ 2 THEN BEGIN
        x = (double ? Dindgen(nx) : Findgen(nx)) * xgrid[1] + xgrid[0]
        regular = 1
    ENDIF ELSE BEGIN
        IF N_Elements(xvalues) GT 0 THEN BEGIN
            IF N_Elements(xvalues) NE nx THEN Message, 'XVALUES keyword must have ' + StrTrim(nx,2) + ' elements.'
            x = xvalues
            regular = 1
        ENDIF
    ENDELSE
    
    ; Did the user specify an YGRID or YVALUES keyword? If so, regular gridding is implied.
    IF N_Elements(ygrid) EQ 2 THEN BEGIN
        y = (double ? Dindgen(ny) : Findgen(ny)) * ygrid[1] + ygrid[0]
        regular = 1
    ENDIF ELSE BEGIN
        IF N_Elements(yvalues) GT 0 THEN BEGIN
            IF N_Elements(yvalues) NE ny THEN Message, 'YVALUES keyword must have ' + StrTrim(ny,2) + ' elements.'
            y = yvalues
            regular = 1
        ENDIF
    ENDELSE
    
    ; Make sure you have an X and Y value at this point if you are doing regular gridding.
    ; Expand the two vectors to be 2D arrays to match the data you are gridding.
    IF regular THEN BEGIN
        IF N_Elements(x) NE nx THEN x = (double) ? Dindgen(nx) : Findgen(nx)
        IF N_Elements(y) NE ny THEN y = (double) ? Dindgen(ny) : Findgen(ny)
        x = Rebin(x, nx, ny)
        y = Rebin(Reform(y, 1, ny), nx, ny)
    ENDIF
    
    ; Find the number of elements we are dealing with.
    numElements = N_Elements(x)
    
    ; If the model parameter vector contains only two elements, then we need a value for the variance. 
    IF N_Elements(t) EQ 2 THEN BEGIN
        meanData = Total(z, Double=double) / numElements
        varianceData = Total( (z - meanData)^2, DOUBLE=double) / numElements
        t = [t, varianceData - t[1]] ; Default value for C1 parameter.
    ENDIF
    
    ; Calculate the number of equations to solve.
    equNum = numElements + 1
    array = (double) ? DblArr(equNum, equNum) : FltArr(equNum, equNum)    
    
    ; Construct the symmetric distance matrix.
    FOR j=0,numElements-2 DO BEGIN
        k = Lindgen(numElements - j) + j
        d = (x[j]-x[k])^2 + (y[j]-y[k])^2
        array[j,k] = d
        array[k,j] = d
    ENDFOR

    ; Get the coefficient matrix by calling the appropriate model function.
    ; Fill the edges of the matrix.
    matrix = Call_Function(functionName, SQRT(array), t)       
    matrix[numElements,*] = 1.0           
    matrix[*,numElements] = 1.0
    matrix[numElements,numElements] = 0.0
    
    ; Use LU Decomposition to find a solution.
    LUDC, matrix, indx, DOUBLE=double    
    
    ; Make a boundary for the grid if one is not provided.
    xmin = Min(x, MAX=xmax)   
    ymin = Min(y, MAX=ymax)
    IF N_Elements(bounds) EQ 0 THEN bounds = [xmin, ymin, xmax, ymax]
    
    ; If a grid spacing parameter is not provided, compute one from the grid boundary.
    IF N_Elements(gs) EQ 0 THEN  gs = [(bounds[2]-bounds[0])/(nx-1.), (bounds[3]-bounds[1])/(ny-1.)]
        
    ; Subtract off a fudge factor to lessen roundoff errors.
    nx = Ceil((bounds[2]-bounds[0])/gs[0] - 1e-5)+1 ;# of elements
    ny = Ceil((bounds[3]-bounds[1])/gs[1] - 1e-5)+1
    
    ; Provide output grid vectors for the result.
    xout = bounds[0] + gs[0]*(double ? Dindgen(nx) : Findgen(nx))
    yout = bounds[1] + gs[1]*(double ? Dindgen(ny) : Findgen(ny))
    
    ; Create a similar matrix to hold the Lagrange multiplier.
    d = double ? DblArr(equNum) : FltArr(equNum)
    result = double ? DblArr(nx, ny, /NoZERO) : FltArr(nx, ny, /NoZERO)
    
    ; Solve the equations. Taking the LUSOL call out of the loop below is what
    ; speeds this function up by several orders of magnitude.
    az = LUSOL(matrix, indx, [Reform(z, N_Elements(z)), 0.0], DOUBLE=double)
    az = Rebin(Transpose(az), nx, numElements+1)
    xx = Rebin(Reform(x, 1, numElements), nx, numElements)
    dxsquare = (xx - Rebin(xout, nx, numElements))^2
    yy = Rebin(Reform(y, 1, numElements), nx, numElements)
    
    ; Do each row separately.
    FOR j=0,ny-1 DO BEGIN
        
        y0 = yout[j]
        
        ; Do all of the columns in parallel
        d = SQRT(dxsquare + (yy - y0)^2)
        d = Call_Function(functionName, d, t)
        
        ; Be sure to add the last row of AZ, which is the Lagrange constraint.
        result[*,j] = Total(d*az, 2) + az[*,numElements]
        
    ENDFOR
    
    ; Return the gridded result.
    RETURN, result
    
END
