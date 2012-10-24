; docformat = 'rst'
;
; PURPOSE:
;   The purpose of this function is to warp an image into a map projection, given
;   latitude and longitude values for each data point. It is similar to MAP_PATCH in
;   IDL.
;
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
; The purpose of this function is to warp an image into a map projection, given
; latitude and longitude values for each data point. It is the Coyote Graphics
; equivalent of MAP_PATCH in IDL.
; 
; Please note: I have only used and tested this function with the well-behaved
; data I am using in my own research and for which I needed this functionality.
; I have no doubt there might be problems with less well-behaved data sets.
; If you discover a problem with your own data, please let me know and I'll take
; another look at this function. DWF.
;   
; :Categories:
;    Graphics, Map Projections
;    
; :Returns:
;     The an output 2D grid in which the data points have been warped into the
;     particular map projection at the requested pixel resolution.
;       
; :Params:
;    data:  in, required, type=numerical
;        A vector or 2D data set. Longitude and latitude values must be present
;        (or easily calculated) for each element in the data array.
;    lons: in, required, type=float
;        If data is a vector, a vector of longitude values corresponding to each
;        data value. Values must be in the range -180 to 360. If data is 2D, either
;        a 1D vector or a 2D array of corresponding longitude values. If data is 2D,
;        and the LONS parameter is missing, a vector of appropriate length scaled into
;        the range -180.0 to 180.0 will be created.
;    lats: in, required, type=float
;        If data is a vector, a vector of latitude values corresponding to each
;        data value. Values must be in the range -90 to 90. If data is 2D, either
;        a 1D vector or a 2D array of corresponding latitude values. If data is 2D,
;        and the LONS parameter is missing, a vector of appropriate length scaled into
;        the range -90.0 to 90.0 will be created.
;               
; :Keywords:
;    cubic: in, optional, type=boolean, default=0
;       If this keyword is set, and the data is a two-dimensional grid, then cubic
;       interpolation will be used to create the output image. It is ignored in the
;       case of non-gridded input data.
;    griddata: in, optional, type=boolean, default=1
;       If the input data is non-gridded, setting this keyword will choose the GRIDDATA
;       function to grid the data into a 2D output array. If not set, the data will be
;       gridded using the TRIGRID function.
;    map: in, optional, type=object
;       An input map projection object (cgMap). If provided, the data will be gridded into
;       this map projection. If not provided, a map object using a equirectangular map projection
;       with a spherical datum will be used. The XRANGE and YRANGE properties of the map object
;       will be set by the program in the course of doing the gridding if the `SetRange` keyword is
;       set.
;    missing: in, optional, type=numberical
;       Missing data in the gridding process will be set to this value.
;    nearest_neighbor: in, optional, type=boolean, default=0
;       If this keyword is set, the nearest neighbor algorithm will be used to create the output
;       grid. Otherwise, bilinear (gridded input data) or natural neighbor (non-gridded input data)
;       interpolation is used as the default algorithm. The keyword is ignored if non-grided input 
;       data is being used and the GRIDDATA keyword is not set, or if gridded input data is being 
;       used and the CUBIC keyword is set. 
;    resolution: in, optional, type=integer
;       A two-element array giving the pixel resolution of the output array in X and Y.
;       The default is a 400x400 array.
;    setrange: in, optional, type=boolean, default=1
;       If this keyword is set, the XRANGE and YRANGE parameters of the cgMap object will
;       be set to the output X and Y ranges. 
;    xrange: out, optional, type=float
;       The output X range in projected meter space (usually associated with the longitude).
;    yrange: out, optional, type=float
;       The output Y range in projected meter space (usually associated with the latitude).
;       
; :Examples:
;    To display a GOES image with map annotations::
;        fileURL = 'http://www.idlcoyote.com/misc/goes_example_data.sav'
;        filename = "goes_example_data.sav"
;        netObject = Obj_New('IDLnetURL')
;        void = netObject -> Get(URL=fileURL, FILENAME=filename)
;        Obj_Destroy, netObject
;        Restore, filename 
;        peru_lat = Temporary(peru_lat) / 10000.
;        peru_lon = Temporary(peru_lon) / 10000.
;        s = Size(peruimage, /DIMENSIONS)
;        centerLat = peru_lat[s[0]/2, s[1]/2]
;        centerLon = peru_lon[s[0]/2, s[1]/2]
;        map = Obj_New('cgMap', 'Albers Equal Area', Ellipsoid='sphere', /OnImage, $
;           STANDARD_PAR1=-19, STANDARD_PAR2=20, CENTER_LAT=centerLat, CENTER_LON=centerLon)
;        warped = cgWarpToMap(peruImage, peru_lon, peru_lat, MAP=map, MISSING=0, $
;            Resolution=[400, 300], /SetRange)
;        cgDisplay, /Free, Title='Warped Image with cgWarpToMap'
;        cgImage, warped, Stretch=2, Position=[0,0,1,1]
;        map -> Draw
;        cgMap_Grid, Map=map, /Label, Color='goldenrod'
;        cgMap_Continents, MAP=map, Color='goldenrod'
;        cgMap_Continents, MAP=map, Color='goldenrod', /Countries
;        
;    Additional examples can be found here: http://www.idlcoyote.com/map_tips/warptomap.php.
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
;     Modification History::
;        Written by David W. Fanning, 12 Sept 2012.
;        Modifications to accommodate lat/lon arrays that are one-dimensional to go along
;           with 2D data. 13 Sept 2012. DWF.
;         
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgWarpToMap, data, lons, lats, $
   CUBIC=cubic, $
   GRIDDATA=griddata, $
   MAP=map, $
   MISSING=missing, $
   NEAREST_NEIGHBOR=nearest_neighbor, $
   RESOLUTION=resolution, $
   SETRANGE=setrange, $
   XRANGE=xrange, $
   YRANGE=yrange

   Compile_Opt idl2
   
   Catch, theError
   IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       RETURN, data
   ENDIF

   IF N_Params() EQ 0 THEN BEGIN
      Print, 'Calling Syntax: warpedImage = cgWarpImage(image, lons, lats, MAP=map, RESOLUTION=resolution)'
      RETURN, 0
   ENDIF
   
   ; Handle keywords
   IF N_Elements(missing) EQ 0 THEN missing = 0B
   IF Keyword_Set(nearest_neighbor) THEN method = "Nearest Neighbor" ELSE method = "Natural Neighbor"
   
   ; Assume we are working with a 2D grid.
   grid = 1
   
   ; Assume the latitude and longitude arrays are 2D.
   latlon2d = 1

   ; If no map object, use a Equirectangular grid with a spherical datum.
   IF N_Elements(map) EQ 0 THEN map = Obj_New('cgMap', 'Equirectangular', ELLIPSOID='Sphere')
   
   ; Is the data 1D or 2D. If 1D, lons and lats are required.
   ndim = Size(data, /N_DIMENSIONS)
   CASE ndim OF
   
      1: BEGIN
         IF (N_Elements(lons) EQ 0) || (N_Elements(lats) EQ 0) THEN BEGIN
             Message, 'Must supply longitudes and latitudes with 1D input data.'
         ENDIF
         grid = 0
         latlon2d = 0
         END
         
      2: BEGIN
         s = Size(data, /DIMENSIONS)
         IF N_Elements(lons) EQ 0 THEN lons = Scale_Vector(Findgen(s[0]), -180, 180)
         IF N_Elements(lats) EQ 0 THEN lats = Scale_Vector(Findgen(s[1]), -90, 90)
         IF Size(lons, /N_DIMENSIONS) EQ 1 THEN latlon2d = 0
         IF Size(lats, /N_DIMENSIONS) EQ 1 THEN latlon2d = 0
         
         END
         
      ELSE: Message, 'Input data must be either 1D or 2D.'
         
   ENDCASE
   
     
   ; Make sure the longitudes are in the range -180 to 180.
   lons = ((lons + 180) MOD 360) - 180
   
   ; Convert to XY projected meter space.
   IF N_Elements(lons) NE N_Elements(lats) THEN BEGIN
       s = Size(data, /DIMENSIONS)
       lattemp = Rebin(Reform(lats, 1, s[1]), s[0], s[1])
       lontemp = Rebin(lons, s[0], s[1])
       help, lontemp, lattemp
       xy = map -> Forward(lontemp, lattemp)
   ENDIF ELSE BEGIN
       xy = map -> Forward(lons, lats)
   ENDELSE
   x = Reform(xy[0,*])
   y = Reform(xy[1,*])
   xmin = Min(x, MAX=xmax)
   ymin = Min(y, MAX=ymax)
   
   ; Set the output resolution of the grid.
   IF N_Elements(resolution) EQ 0 THEN BEGIN
      delta_x = (xmax - xmin) / 399
      delta_y = (ymax - ymin) / 399
      resolution = [400,400]
   ENDIF ELSE BEGIN
      delta_x = (xmax - xmin) / (resolution[0]-1)
      delta_y = (ymax - ymin) / (resolution[1]-1)
   ENDELSE
   
   ; Find the sides of the image and make a boundary rectangle.
   IF (grid EQ 0) || (latlon2d EQ 0) THEN BEGIN
      xmin = Min(x, MAX=xmax)
      ymin = Min(y, MAX=ymax)
   ENDIF ELSE BEGIN
       dims = Size(data, /DIMENSIONS)
       x = Reform(x, dims[0], dims[1])
       y = Reform(y, dims[0], dims[1])
       xmin = x[0,Round(s[1]/2.)]
       xmax = x[s[0]-1, Round(s[1]/2.)] 
       ymin = y[Round(s[0]/2.), 0]
       ymax = y[Round(s[0]/2.), s[1]-1] 
       IF ymin GT ymax THEN BEGIN
          temp = ymin
          ymin = ymax
          ymax = temp 
        ENDIF 
   ENDELSE
   rect = [xmin-(delta_x/2.), ymin-(delta_y/2.), xmax+(delta_x/2.), ymax+(delta_y/2.)]  
   xrange = rect[[0,2]]
   yrange = rect[[1,3]] 
   IF Keyword_Set(setrange) THEN map -> SetProperty, XRANGE=xrange, YRANGE=yrange
 
   ; If you don't have a grid, then you have to do the gridding the slow way.
   IF grid EQ 0 THEN BEGIN
   
       ; We need a set of Delaunay triangles.
       QHull, x, y, triangles, /Delaunay
       
       ; A choice of GridData or TriGrid for the actual gridding.
       IF Keyword_Set(griddata) THEN BEGIN
          warpedImage = GridData(x, y, data, TRIANGLES=triangles, DELTA=[delta_x, delta_y], $
             DIMENSION=resolution, START=[Min(x), Min(y)], MISSING=missing, METHOD=method)
       ENDIF ELSE BEGIN
          warpedImage = TriGrid(x, y, data, triangles, [delta_x, delta_y], rect, MISSING=missing)
       ENDELSE
       RETURN, warpedImage
       
   ENDIF

   ; If you have a grid, you can do the gridding the fast way by interpolating the output
   ; grid from the input grid. First, create an output grid.
   xvec = Scale_Vector(Findgen(resolution[0]), xmin-(delta_x/2.), xmax+(delta_x/2.))
   yvec = Scale_Vector(Findgen(resolution[1]), ymin-(delta_y/2.), ymax+(delta_x/2.))
   x_out = Rebin(xvec, resolution[0], resolution[1])
   y_out = Rebin(Reform(yvec, 1, resolution[1]), resolution[0], resolution[1])

   ; Get the fractional indices of the output grid on the input grid.
   dims = Size(data, /DIMENSIONS)
   xindex = Scale_Vector(x_out, 0, dims[0], Min=xmin, Max=xmax)
   yindex = Scale_Vector(y_out, 0, dims[1], MIN=ymin, MAX=ymax)
   
   ; Interpolate the data. Nearest neighbor, bilinear, or cubic interpolation is possible.
   IF Keyword_Set(cubic) THEN BEGIN
      warpedImage = Interpolate(data, xindex, yindex, CUBIC=-0.5, MISSING=missing)
   ENDIF ELSE BEGIN
      IF Keyword_Set(nearest_neighbor) THEN BEGIN
         warpedImage = data[Round(xindex), Round(yindex)]
      ENDIF ELSE BEGIN
         warpedImage = Interpolate(data, xindex, yindex, MISSING=missing)
      ENDELSE
   ENDELSE

   RETURN, warpedImage
   
END

