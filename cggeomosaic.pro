FUNCTION cgGeoMosaic, geofile_1, geofile_2, $
   FILENAME=filename, $
   IMAGE=image, $
   MISSING=missing

    Compile_Opt idl2
    
    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN, ""
    ENDIF
    
    ; Need two files to do the job!
    IF N_Params() NE 2 THEN BEGIN
       
        void = Dialog_Message('Calling Syntax: geofile = cgGeoMosaic( geofile_1, geofile_2 )')
        RETURN, ""
    
    ENDIF
    
    IF N_Elements(missing) EQ 0 THEN missing = 0B
    
    ; Create map coordinate objects for the two files
    m1 = cgGeoMap(geofile_1, IMAGE=image_1, PALETTE=palette_1, SUCCESS=success)
    IF ~success THEN RETURN, ""
    m2 = cgGeoMap(geofile_2, IMAGE=image_2, PALETTE=palette_2, SUCCESS=success)
    IF ~success THEN RETURN, ""
    
    ; The images must have the same number of dimensions
    IF Size(image_1, /N_DIMENSIONS) NE Size(image_2, /N_DIMENSIONS) THEN $
       Message, 'The images do not have the same number of dimensions'
    
    ; Check to be sure projection and ellipsoid are the same.
    m1 -> GetProperty, Map_Projection=projection_1, Ellipsoid=ellipsoid_1
    m2 -> GetProperty, Map_Projection=projection_2, Ellipsoid=ellipsoid_2
    IF projection_1 NE projection_2 THEN $
       Message, 'The map projections of the two files are not the same.'
    IF ellipsoid_1 NE ellipsoid_2 THEN $
       Message, 'The map projection ellipsoids of the two files are not the same.'
       
    ; Get the file information and geotiff structure.
    IF Query_Tiff(geofile_1, info_1, GEOTIFF=geo_1) NE 1 THEN $
          Message, 'Not a geoTiff file: ' + geofile_1
    IF Query_Tiff(geofile_2, info_2, GEOTIFF=geo_2) NE 1 THEN $
          Message, 'Not a geoTiff file: ' + geofile_2
    xscale_1 = geo_1.ModelPixelScaleTag[0]
    yscale_1 = geo_1.ModelPixelScaleTag[1]
    xscale_2 = geo_2.ModelPixelScaleTag[0]
    yscale_2 = geo_2.ModelPixelScaleTag[1]
    IF xscale_1 NE xscale_2 THEN Message, 'Pixel X scales are incompatible.'
    IF yscale_1 NE yscale_2 THEN Message, 'Pixel Y scales are incompatible.'
    
    ; Get the image map ranges and create a mosaic image of the proper size.
    m1 -> GetProperty, XRange=xr_1, YRange=yr_1
    m2 -> GetProperty, XRange=xr_2, YRange=yr_2
    xr = [xr_1[0] < xr_2[0], xr_1[1] > xr_2[1]]
    yr = [yr_1[0] < yr_2[0], yr_1[1] > yr_2[1]]
    
    ; How many pixels are in the new image?
    xNumPixels = (xr[1] - xr[0]) / xscale_1
    yNumPixels = (yr[1] - yr[0]) / yscale_1
    
    ; If necessary, make sure these images are band interleaved.
    void = Image_Dimensions(image_1, TRUEINDEX=trueIndex, XINDEX=xindex, YINDEX=yindex, $
       XSIZE=xsize_1, YSIZE=ysize_1)
    IF (trueIndex NE -1) && (trueIndex NE 2) THEN image_1 = Transpose(image_1, [xindex, yindex, trueindex])
    void = Image_Dimensions(image_2, TRUEINDEX=trueIndex, XINDEX=xindex, YINDEX=yindex, $
       XSIZE=xsize_2, YSIZE=ysize_2)
    IF (trueIndex NE -1) && (trueIndex NE 2) THEN image_2 = Transpose(image_2, [xindex, yindex, trueindex])
    
    
    ; Create a new image of the proper size. 
    imageType = Size(image_1, /Type)
    IF trueIndex NE -1 THEN BEGIN
       newImage = Make_Array( xnumPixels, ynumPixels, 3, TYPE=imageType, VALUE=missing)
    ENDIF ELSE BEGIN
       newImage = Make_Array( xnumPixels, ynumPixels, TYPE=imageType, VALUE=missing)
    ENDELSE
    
    ; Create image extent vectors.
    xvec = Scale_Vector(DIndgen(xnumPixels), xr[0], xr[1])
    yvec = Scale_Vector(DIndgen(ynumPixels), yr[0], yr[1])

    ; Locate the lower-left corner of each image in pixel coordinates.
    xloc = 0 > Value_Locate(xvec, [xr_1[0], xr_2[0]])
    yloc = 0 > Value_Locate(yvec, [yr_1[0], yr_2[0]]) 
    
    IF trueIndex NE -1 THEN BEGIN
       FOR j=0,2 DO BEGIN
           temp1 = image_1[*,*,j]
           newImage[xloc[0], yloc[0], j] = temp1
           Undefine, temp1
           temp2 = image_2[*,*,j]
           newImage[xloc[1], yloc[1], j] = temp2
           Undefine, temp2
       ENDFOR
    ENDIF ELSE BEGIN
    print, xloc[0], yloc[0]
    print, xloc[0], yloc[0]
    help, newImage, image_1, image_2
        newImage[xloc[0], yloc[0]] = image_1
        newImage[xloc[1], yloc[1]] = image_2
    ENDELSE
    
    IF Keyword_Set(image) THEN BEGIN
        RETURN, newImage
    ENDIF ELSE BEGIN
        newGeo = geo_1
        geo_1.ModelTiePointTag[3:4] = [xr[1], yr[1]]
        IF N_Elements(filename) EQ 0 THEN BEGIN
           filename = cgPickfile(FILE='mosaic.tif')
           IF filename EQ "" THEN RETURN, newImage
           Write_Tiff, filename, Reverse(newimage, 2), GEOTIFF=newGeo
           RETURN, filename
        ENDIF ELSE BEGIN
           Write_Tiff, filename, Reverse(newimage, 2), GEOTIFF=newGeo
           RETURN, filename
        ENDELSE 
    ENDELSE
    
END