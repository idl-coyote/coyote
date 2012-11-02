   image = cgDemoData(5)
   cgImage2KML, image, LatLonBox=[48, 25, -125, -75], $
       CTIndex=3, Filename="ctscan.kml"
       
   precipmap, 'C:\IDL\ST4.2005010112.24h.bin', DATA=image, PALETTE=palette, MAP=map
   cgImage2KML, image, map, Palette=palette, Filename="precipitation.kml", $
       Missing_Value=15, Transparent=30
   
END