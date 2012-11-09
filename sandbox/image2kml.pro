;   image = cgDemoData(5)
;   cgImage2KML, image, LatLonBox=[48, 25, -125, -75], $
;       CTIndex=3, Filename="ctscan.kml"
;       
;   precipmap, 'C:\IDL\ST4.2005010112.24h.bin', DATA=data, PALETTE=palette, MAP=map
;   cgImage2KML, data, map, Palette=palette, Filename="precipitation.kml", $
;       Missing_Value=15, Transparent=30, Description='24-Hour Precipition'
;   
;   cgImage2KML, GeoTiff='AF03sep15b.n16-VIg.tif', Min_Value=0, CTIndex=11, $
;      /Brewer, /Reverse, Transparent=50, Filename='avhrr_ndvi.kml', $
;      Description='AVHRR NDVI Data from Africa'
;      
   ;zonefile = '\\paradox.warnercnr.colostate.edu\paradox\usr\dfanning\Amazon\Querencia\Zone\quer_zones_ext_60m.tif'
   zonefile = 'H:\Querencia\Zone\quer_zones_ext_60m.tif'
   zoneMap = cgGeoMap(zonefile, Image=zoneImage, Palette=palette)
;   zoneMap = cgGeoMap('zonal_map.tif', Image=zoneImage, Palette=palette)
   cgImage2KML, zoneImage, zoneMap, Palette=palette, Filename='zonemap.kml', Missing=0, Transparent=50, resize_factor=0.25
;
;   ; Get a test image.
;   ;testfile = "\\paradox.warnercnr.colostate.edu\paradox\usr\dfanning\Amazon\Querencia\MSS\1973\19730630_241_068\19730630_241_068_output_b4.tif"
;   landMap = cgGeomap('19730630_241_068_b4.tif', Image=landImage)
;    cgImage2KML, cgImgScl(landImage, MINValue=10, MAXValue=16, /Scale, Missing_Index=0), landMap, DrawOrder=1, $
;      CTIndex=0, Filename='landsat_b4.kml', Missing_Value=0, Transparent=30
;      
;       kmlFile = Obj_New('cgKML_File', 'precipmap.kml')
;       precipmap, 'C:\IDL\ST4.2005010112.24h.bin', DATA=data, PALETTE=palette, MAP=map
;       cgImage2KML, data, map, Palette=palette, Missing_Value=15, $
;          Description='24-Hour Precipition', PlaceName='Precipitation', $
;          AddToFile=kmlFile
;        colors = ['dark green', 'lime green', 'light sea green', 'yellow', 'khaki', $
;                     'dark goldenrod', 'light salmon', 'orange', 'red', 'sienna', 'purple', $
;                     'orchid', 'thistle', 'sky blue', 'black']
;       TVLCT, cgColor(colors, /Triple), 1
;       levels = [0, 2, 5, 10, 15, 20, 25, 50, 75, 100, 125, 150, 200, 250]
;          
;       cgCBar2KML, NColors=13, Bottom=1, Divisions=14, Title='24 Hour Precipitation (mm)', $
;          /Discrete, OOB_High=14, TickNames = StrTrim(levels,2), Charsize=1.5, $
;              Description='Precipitation Values', $
;              PlaceName='Precipitation Color Bar', $
;              AddToFile=kmlFile, Width=400
;           kmlFile -> Save
;           kmlFile -> Destroy
END