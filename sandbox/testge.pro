PRO TestGE

   ; Get the zonal map.
   ;zonefile = "\\paradox.warnercnr.colostate.edu\paradox\usr\dfanning\Amazon\Querencia\Zone\quer_zones_ext_60m.tif"
   zoneMap = cgGeoMap('zonal_map.tif', Image=zoneImage, Palette=palette)
   cgImage2KML, zoneImage, zoneMap, Palette=palette, Filename='zonemap.kml', Missing=0, Transparent=50

   ; Get a test image.
   ;testfile = "\\paradox.warnercnr.colostate.edu\paradox\usr\dfanning\Amazon\Querencia\MSS\1973\19730630_241_068\19730630_241_068_output_b4.tif"
   testMap = cgGeomap('19730630_241_068_b4.tif', Image=testImage)
   dims = Size(testimage, /DIMENSIONS)
   cgImage2KML, cgImgScl(testImage, MINValue=10, MAXValue=16, /Scale, Missing_Index=0), testMap, DrawOrder=1, $
      CTIndex=0, Filename='landsat_b4.kml', Missing_Value=0, Transparent=30
   
;   ; Clip the zonal mask to the boundaries of the test image.
;   testMap -> GetProperty, Boundary=boundary
;   clippedImage = cgClipToMap(zoneImage, boundary, MAP=zoneMap, OUTMAP=clippedmap, LATLONBOX=latlonbox)
;   cgImage2KML, clippedImage, LatLonBox=latlonbox, DrawOrder=2, $
;      Palette=palette, Filename='clippedzone.kml', Missing_Value=0, Transparent=0
;   
;   ; Get the land mask for the test image and apply it to the clipped image.
;   void = cgRootName(testMap, DIRECTORY=thisDir)
;   maskfile = Filepath(ROOT_DIR=thisdir, 'masked_image.img')
;   OpenR, lun, maskFile, /Get_LUN, /COMPRESS
;   img = Assoc(lun, BytArr(dims[0], dims[1]))
;   mask = img[4]
   
 END  