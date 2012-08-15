

FUNCTION cgWarpToMap, data, lons, lats, MAP=map, MISSING=missing, RESOLUTION=resolution

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
   
   IF N_Elements(missing) EQ 0 THEN missing = 0B

   ; If no map object, use a simple equirectangular projection with a spherical datum.
   IF N_Elements(map) EQ 0 THEN map = Obj_New('cgMap', 'Equirectangular', DATUM=19)
   
   ; Is the data 1D or 2D. If 1D, lons and lats are required.
   ndim = Size(data, /N_DIMENSIONS)
   CASE ndim OF
   
      1: BEGIN
         IF (N_Elements(lons) EQ 0) || (N_Elements(lats) EQ 0) THEN BEGIN
             Message, 'Must supply longitudes and latitudes with 1D input data.'
         ENDIF
         END
         
      2: BEGIN
         dims = Size(data, /DIMENSIONS)
         IF N_Elements(lons) EQ 0 THEN lons = Scale_Vector(Findgen(dims[0]), -180, 180)
         IF N_Elements(lats) EQ 0 THEN lats = Scale_Vector(Findgen(dims[1]), -90, 90)
         
         slats = Size(lats, /DIMENSIONS)
         slons = Size(lons, /DIMENSIONS)
         IF Size(lons, /N_DIMENSIONS) EQ 1 THEN BEGIN
            lons = Rebin(lons, slons, slats)
         ENDIF
         IF Size(lats, /N_DIMENSIONS) EQ 1 THEN BEGIN
            lats = Rebin(Reform(lats, 1, slats), slons, slats)
         ENDIF
         END
         
      ELSE: Message, 'Input data must be either 1D or 2D.'
         
   ENDCASE
   
   ; Make sure the longitudes are in the range -180 to 180.
   lons = ((lons + 180) MOD 360) - 180
   
   ; Cull initial points now to remove duplicates.
   help, lons, lats, data
   Grid_Input, lons, lats, data, lons_p, lats_p, data_p

   ; Find the data range in X.
   xy = map -> Forward(lons_p, lats_p)
   x = Reform(xy[0,*])
   y = Reform(xy[1,*])
   xmin = Min(x, MAX=xmax)
   ymin = Min(y, MAX=ymax)
   rect = [xmin, ymin, xmax, ymax]
   
   ; Construct the grid array.
   IF N_Elements(resolution) EQ 0 THEN BEGIN
      delta_x = (xmax - xmin) / 499
      delta_y = (ymax - ymin) / 499
   ENDIF ELSE BEGIN
      delta_x = resolution[0]
      delta_y = resolution[1]
   ENDELSE
   
   starttime = systime(1)
   
   Triangulate, x, y, triangles, TOLERANCE=1e-6
   warpedImage = TriGrid(x, y, data_p, triangles, [delta_x, delta_y], rect, MISSING=missing)
   RETURN, warpedImage
    
END