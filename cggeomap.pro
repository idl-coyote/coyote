;*****************************************************************************************************
;+
; NAME:
;       cgGEOMAP
;
; PURPOSE:
;
;       The purpose of this function is to translate a GEOTIFF structure
;       (as returned by QUERY_TIFF or READ_TIFF) into a map coordinate
;       object (cgMapCoord) that can be used to georeference images with a 
;       map data coordinate system. The Coyote Library isrequired to be on 
;       your IDL PATH.
;
; AUTHOR:
;
;        FANNING SOFTWARE CONSULTING  
;        1645 Sheely Drive            
;        Fort Collins                  
;        CO 80526 USA                  
;        Phone: 970-221-0438           
;        E-mail: davidf@dfanning.com   
;
; CATEGORY:
;
;       Image display and map projection.
;
; CALLING SEQUENCE:
;
;       mapObj = cgGeoMap( geoTiffFilename )
;        
;    or
;    
;       mapObj = cgGeoMap( image, geotiff )
;       
; ARGUMENTS:
; 
;    geoTiffFilename:     The name of a GeoTIFF file from which can be obtained
;                         the geoTIFF structure of geoTags and the dimensions of the image.
;                         
;    or
;    
;    image:                A GeoTIFF image.
;    
;    geotiff:              A GeoTIFF structure of geoTags. Normally obtained by calling
;                          QUERY_TIFF or READ_TIFF.
;                          
; KEYWORDS:
; 
;    GRID_COLOR:           The name of a color the map grid should be displayed with. The default
;                          is "gray". Color names are those supported by cgColor.
;                          
;    MAP_GRID:             If a cgMapCoord object is made successfully, then setting this keyword
;                          will add a Map_Grid object to the cgMapCoord object.
;
;    MAP_OUTLINE:          If a cgMapCoord object is made successfully, then setting this keyword
;                          will add a Map_Outline object to the cgMapCoord object.
;                         
;    OUTLINE_COLOR:        The name of a color the map outline should be displayed with. The default
;                          is "charcoal". Color names are those supported by cgColor.
;                         
;    SILENT:               IDL cannot map every GeoTiff image to a supported map projection or datum.
;                          Normally, if the GeoTIFF image is unsupported, an error message is issued.
;                          Setting this keyword will suppress such error messages. If you do this, you
;                          MUST check the SUCCESS keyword to see if the program ran successfully. (Of
;                          course, it is a good idea to check it in any case!)
;                          
;    SUCCESS:              An output variable that will contain a 1 if the map coordinate object was
;                          created successfully. Or to a 0 if it was not created successfully.
;                          
; RETURN_VALUE:
; 
;    mapCoord              A cgMapCoord object that can be used as the data coordinate object for
;                          a cgsImage object. 
;                          
; NOTES:                   
; 
;    It is not possible to have a one-to-one mapping between every GeoTIFF file and a map projection
;    in IDL, since IDL has a limited number of map projections and datums available. And, even at that,
;    I have not implemented all of IDL's map projections or datums, only those that I thought were most
;    likely to be encountered in my own work. If you run into a GeoTIFF file that does not work in this
;    code (either because of an error in the code or because it is not supported), please contact me.
;    I am interested in supporting as many GeoTIFF files as possible and I will take pains to do so if
;    I know they are needed.
;    
; RESTRICTIONS:
; 
;    Programs from the Coyote Library are required to run this code.
;    
;       http://www.idlcoyote.com/documents/programs.php
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, modified from GeoCoord in Catalyst Library. 28 Oct 2011.
;       Switch UTM datum from WGS84 to WALBECK to avoid UTM projection bug in all versions
;            of IDL prior to IDL 8.2, when it is suppose to be fixed. For more information,
;            see this article: http://www.idlcoyote.com/map_tips/utmwrong.php. 31 Oct 2011. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2011, by Fanning Software Consulting, Inc.                                ;
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
Function cgGeoMap, image, geotiff, $
    BORDER_COLOR=border_color, $
    GRID_COLOR=grid_color, $
    MAP_GRID=map_grid, $
    MAP_OUTLINE=map_outline, $
    OUTLINE_COLOR=outline_color, $
    SILENT=silent, $
    TITLE=title, $
    SUCCESS=success

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE
       
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        IF ~Keyword_Set(silent) THEN void = Error_Message()
        success = 0
        RETURN, Obj_New()
    ENDIF
   
    ; Set default values of keywords.
    SetDefaultValue, grid_color, 'charcoal'
    SetDefaultValue, outline_color, 'charcoal'
    
   ; If no parameters, ask about reading a GeoTiff file.
   IF N_Params() EQ 0 THEN BEGIN
        geofile = Dialog_Pickfile(Title='Select GeoTiff File...', FILTER=['*.tif', '*.tiff'])
        IF geofile EQ "" THEN RETURN, Obj_New()
        
        ; Read the image query file. Make sure it is a GeoTiff file.
        ok = Query_Tiff(geofile, info, GEOTIFF=geotiff)
        IF ~ok THEN Message, 'Selected file does not appear to be a TIFF file.'
        IF Size(geotiff, /TNAME) NE 'STRUCT' THEN Message, 'Selected file does not appear to be a GeoTiff file.'
        channels = info.channels
        dims = info.dimensions
        CASE channels OF
            1: BEGIN
               xsize = dims[0]
               ysize = dims[1]
               END
               
            3: BEGIN
               indices = Where(dims NE 3, count)
               IF count NE 2 THEN Message, 'Cannot determine the image dimensions.'
               xsize = dims[indices[0]]
               ysize = dims[indices[1]]
               END
               
            ELSE: Message, 'Do not know how to handle an image with ' + StrTrim(channels,2) + ' channels'
        ENDCASE
   ENDIF
   
   ; If only one parameter is passed, assume this is the name of a geotiff file.
   IF N_Params() EQ 1 THEN BEGIN
     
       ; Is the parameter a string?
       IF Size(image, /TNAME) NE 'STRING' THEN Message, 'The input parameter must be the name of a GeoTiff file.'
       
       ; Is it a filename? Can we find the file?
       ok = File_Test(image)
       IF ~ok THEN Message, 'Cannot locate the specified file: ' +  image
       
        ; Read the image query file. Make sure it is a GeoTiff file.
        ok = Query_Tiff(image, info, GEOTIFF=geotiff)
        IF ~ok THEN Message, 'Specified file does not appear to be a TIFF file.'
        IF Size(geotiff, /TNAME) NE 'STRUCT' THEN Message, 'Specified file does not appear to be a GeoTiff file.'
        channels = info.channels
        dims = info.dimensions
        CASE channels OF
            1: BEGIN
               xsize = dims[0]
               ysize = dims[1]
               END
               
            3: BEGIN
               indices = Where(dims NE 3, count)
               IF count NE 2 THEN Message, 'Cannot determine the image dimensions.'
               xsize = dims[indices[0]]
               ysize = dims[indices[1]]
               END
               
            ELSE: Message, 'Do not know how to handle an image with ' + StrTrim(channels,2) + ' channels'
        ENDCASE
   ENDIF

   IF N_Params() EQ 2 THEN BEGIN
     
       ; Get the image dimensions.
       dims = Image_Dimensions(image, XSIZE=xsize, YSIZE=ysize)

       ; Is this a structure?
       IF Size(geotiff, /TNAME) NE 'STRUCT' THEN Message, 'The supplied argument is not an IDL structure variable.'
       
       ; Is this a geoTiff structure?
       fields = Tag_Names(geotiff)
       i = Where(fields EQ 'MODELPIXELSCALETAG', count)
       IF count EQ 0 THEN Message, 'The supplied structure does not appear to be a GeoTiff structure.'
       
   ENDIF

   ; We can only handle raster images with projected coordinate systems.
   IF geotiff.gtModelTypeGeoKey NE 1 THEN Message, 'GEOTIFF image does not have a projected coordinate system.'
   
   ; We can not handle point data.
   ;IF geotiff.gtRasterTypeGeoKey NE 1 THEN Message, 'Image pixels do not represent area in this GEOTIFF image.'
   
   ; Get the fields of the geotiff structure.
   fields = Tag_Names(geotiff)
   
   ; We want all measurements to be in meters. First, check to see of there is a ProjLineearUnitsGeokey.
   ; If not, check the GeogLinearUnitsGeokey.
   index = Where(fields EQ 'PROJLINEARUNITSGEOKEY', unitsCount)
   IF unitsCount EQ 0 THEN BEGIN
      index = Where(fields EQ 'GEOGLINEARUNITSGEOKEY', unitsCount)
      IF unitsCount NE 0 THEN unitValue = geotiff.GEOGLINEARUNITSGEOKEY
   ENDIF ELSE unitValue = geotiff.PROJLINEARUNITSGEOKEY
   IF N_Elements(unitValue) EQ 0 THEN Message, 'Cannot find a linear unit key in the GeoTiff structure.'
   CASE unitValue OF
   
        '9001':        
        '9002': BEGIN
            xscale = xscale * (1.0D/3.28D)
            yscale = yscale * (1.0D/3.28D)
            END            
        ELSE: Message, 'Linear unit ' + StrTrim(unitValue,2) + ' is not supported in this program.'
        
   ENDCASE
   
   ; We need a map projection code.
   index = Where(fields EQ 'PROJECTEDCSTYPEGEOKEY', projCount)
   IF projCount EQ 0 THEN BEGIN
      index = Where(fields EQ 'PROJECTIONGEOKEY', projCount)
      IF projCount GT 0 THEN projCode = geotiff.PROJECTIONGEOKEY
   ENDIF ELSE projCode = geotiff.PROJECTEDCSTYPEGEOKEY
   IF N_Elements(projCode) EQ 0 THEN Message, 'Cannot find a map projection Geokey in the GeoTiff Structure.'
   
   ; Assume successful completion.
   success = 1
   
   ; Get the pixel scale values.
   xscale = (geotiff.ModelPixelScaleTag)[0]
   yscale = (geotiff.ModelPixelScaleTag)[1]
   
   ; Get the tie points and calculate the map projection range.
   x0 = (geotiff.ModelTiePointTag)[3]
   y0 = (geotiff.ModelTiePointTag)[4]
   x1 = x0 + (xscale * xsize)
   y1 = y0 - (yscale * ysize)
   xrange = [x0, x1]
   yrange = [y0, y1]
   
   ; Is this a user-defined projection code?
   IF projCode EQ 32767L THEN BEGIN
   
        ; What kind of datum is being used?
        IF Where(fields EQ 'GEOGRAPHICTYPEGEOKEY') NE -1 THEN BEGIN
            CASE geotiff.GEOGRAPHICTYPEGEOKEY OF
                   0: thisDatum = 19 ; Undefined, assgined to sphere.
                4322: thisDatum = 5  ; WGS 72
                4326: thisDatum = 8  ; WGS 84
                4001: thisDatum = 9  ; Airy
                4002: thisDatum = 11 ; Modified Airy
                4003: thisDatum = 14 ; Astralian National
                4008: thisDatum = 0  ; Clarke 1866
                4017: thisDatum = 6  ; Everest
                4018: thisDatum = 10 ; Modified Everest
                4023: thisDatum = 3  ; International 1967
                4024: thisDatum = 15 ; Krassovsky
                4030: thisDatum = 8  ; WGS 84
                4034: thisDatum = 1  ; Clarke 1880
                4035: thisDatum = 19 ; Sphere
                ELSE:
            ENDCASE
        ENDIF
        
        ; What kind of datum ellipsoid is being used?
        IF Where(fields EQ 'GEOGELLIPSOIDGEOKEY') NE -1 THEN BEGIN
            CASE geotiff.GEOGELLIPSOIDGEOKEY OF
                   0: thisDatum = 19 ; Undefined, assgined to sphere.
                6322: thisDatum = 5  ; WGS 72
                6326: thisDatum = 8  ; WGS 84
                6001: thisDatum = 9  ; Airy
                6002: thisDatum = 11 ; Modified Airy
                6003: thisDatum = 14 ; Astralian National
                6008: thisDatum = 0  ; Clarke 1866
                6017: thisDatum = 6  ; Everest
                6018: thisDatum = 10 ; Modified Everest
                6023: thisDatum = 3  ; International 1967
                6024: thisDatum = 15 ; Krassovsky
                6030: thisDatum = 8  ; WGS 84
                6034: thisDatum = 1  ; Clarke 1880
                6035: thisDatum = 19 ; Sphere
                ELSE:
            ENDCASE
        ENDIF
        
        ; Default datum if you don't have one defined by here.
        IF N_Elements(thisDatum) THEN thisDatum = 19 ; Defaults to Sphere.

        ; What kind of projection is this?
        CASE geotiff.PROJCOORDTRANSGEOKEY OF
             1: thisProjection = 109 ; Transverse Mercator
             7: thisProjection = 105 ; Mercator
             8: thisProjection = 104 ; Lambert Conformal Conic
            10: thisProjection = 111 ; Lambert Azimuthal Equal Area
            11: thisProjection = 103 ; Albers Equal Area
            14: thisProjection = 110 ; Stereographic
            15: thisProjection = 106 ; Polar Stereographic
            17: thisProjection = 117 ; Equirectangular
            20: thisProjection = 118 ; Miller Cylindrical
            21: thisProjection = 114 ; Orthographic
            22: thisProjection = 107 ; Polyconic
            23: thisProjection = 121 ; Robinson
            24: thisProjection = 116 ; Sinusoidal
            ELSE: Message, 'Unsupported map projection for this GeoTIFF image.'      
        ENDCASE
        
        ; Make the map coordinate object, based on the map projection.
        CASE thisProjection OF
        
            103: BEGIN ; Albers Equal Area
            
                 CASE 1 OF ; Longitude
                    (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
                    (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
                    ELSE:
                 ENDCASE
            
                 CASE 1 OF ; Latitude
                    (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
                    (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
                    (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
                    ELSE:
                 ENDCASE

                 index = Where(fields EQ 'PROJFALSEEASTINGGEOKEY', count)
                 IF count GT 0 THEN falseEast = geotiff.PROJFALSEEASTINGGEOKEY ELSE falseEast = 0
                 
                 index = Where(fields EQ 'PROJFALSENORTHINGGEOKEY', count)
                 IF count GT 0 THEN falseNorth = geotiff.PROJFALSENORTHINGGEOKEY ELSE falseNorth = 0

                 std_parallel_1 = geotiff.PROJSTDPARALLEL1GEOKEY
                 std_parallel_2 = geotiff.PROJSTDPARALLEL2GEOKEY

                 mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=thisDatum, $
                      CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, $
                      STANDARD_PAR1=std_parallel_1, STANDARD_PAR2=std_parallel_2, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, NAME='FROM_GEOCOORD', $
                      BORDER_COLOR=border_color, TITLE=title)
                 mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
                 
                 END

            104: BEGIN ; Lambert Conformal Conic
                 CASE 1 OF ; Longitude
                    (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
                    (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
                    ELSE:
                 ENDCASE
            
                 CASE 1 OF ; Latitude
                    (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
                    (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
                    (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
                    ELSE:
                 ENDCASE

                 index = Where(fields EQ 'PROJFALSEEASTINGGEOKEY', count)
                 IF count GT 0 THEN falseEast = geotiff.PROJFALSEEASTINGGEOKEY ELSE falseEast = 0
                 
                 index = Where(fields EQ 'PROJFALSENORTHINGGEOKEY', count)
                 IF count GT 0 THEN falseNorth = geotiff.PROJFALSENORTHINGGEOKEY ELSE falseNorth = 0

                 std_parallel_1 = geotiff.PROJSTDPARALLEL1GEOKEY
                 std_parallel_2 = geotiff.PROJSTDPARALLEL2GEOKEY

                 mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=thisDatum, $
                      CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, $
                      STANDARD_PAR1=std_parallel_1, STANDARD_PAR2=std_parallel_2, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      BORDER_COLOR=border_color, TITLE=title)
                 mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
                 
                 END
                 
            105: BEGIN ; Mercator
            
                 CASE 1 OF ; Longitude
                    (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
                    (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
                    ELSE:
                 ENDCASE
            
                 index = Where(fields EQ 'PROJFALSEEASTINGGEOKEY', count)
                 IF count GT 0 THEN falseEast = geotiff.PROJFALSEEASTINGGEOKEY ELSE falseEast = 0
                 
                 index = Where(fields EQ 'PROJFALSENORTHINGGEOKEY', count)
                 IF count GT 0 THEN falseNorth = geotiff.PROJFALSENORTHINGGEOKEY ELSE falseNorth = 0

                 index = Where(fields EQ 'PROJSTDPARALLEL1GEOKEY', count)
                 IF count EQ 0 THEN BEGIN
                    index = Where(fields EQ 'PROJSTDPARALLELGEOKEY', count)
                    IF count NE 0 THEN true_scale_lat = geotiff.PROJSTDPARALLELGEOKEY
                 ENDIF ELSE true_scale_lat = geotiff. PROJSTDPARALLEL1GEOKEY
                 
                 mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=thisDatum, $
                      TRUE_SCALE_LATITUDE=true_scale_lat, CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      BORDER_COLOR=border_color, TITLE=title)
                 mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
                 
                 END

            106: BEGIN ; Polar Stereographic
            
                 CASE 1 OF ; Longitude
                    (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
                    (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
                    ELSE:
                 ENDCASE
            
                 CASE 1 OF ; Latitude
                    (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
                    (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
                    (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
                    ELSE:
                 ENDCASE

                 index = Where(fields EQ 'PROJFALSEEASTINGGEOKEY', count)
                 IF count GT 0 THEN falseEast = geotiff.PROJFALSEEASTINGGEOKEY ELSE falseEast = 0
                 
                 index = Where(fields EQ 'PROJFALSENORTHINGGEOKEY', count)
                 IF count GT 0 THEN falseNorth = geotiff.PROJFALSENORTHINGGEOKEY ELSE falseNorth = 0

                 mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=thisDatum, $
                      CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      BORDER_COLOR=border_color, TITLE=title)
                 mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
                 
                 END
                 
            107: BEGIN ; Polyconic
            
                 CASE 1 OF ; Longitude
                    (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
                    (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
                    ELSE:
                 ENDCASE
            
                 CASE 1 OF ; Latitude
                    (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
                    (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
                    (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
                    ELSE:
                 ENDCASE

                 index = Where(fields EQ 'PROJFALSEEASTINGGEOKEY', count)
                 IF count GT 0 THEN falseEast = geotiff.PROJFALSEEASTINGGEOKEY ELSE falseEast = 0
                 
                 index = Where(fields EQ 'PROJFALSENORTHINGGEOKEY', count)
                 IF count GT 0 THEN falseNorth = geotiff.PROJFALSENORTHINGGEOKEY ELSE falseNorth = 0

                 mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=thisDatum, $
                      CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      BORDER_COLOR=border_color, TITLE=title)
                 mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
                 
                 END

            109: BEGIN ; Transverse Mercator
            
                 CASE 1 OF ; Longitude
                    (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
                    (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
                    ELSE:
                 ENDCASE
            
                 CASE 1 OF ; Latitude
                    (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
                    (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
                    (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
                    ELSE:
                 ENDCASE

                 index = Where(fields EQ 'PROJFALSEEASTINGGEOKEY', count)
                 IF count GT 0 THEN falseEast = geotiff.PROJFALSEEASTINGGEOKEY ELSE falseEast = 0
                 
                 index = Where(fields EQ 'PROJFALSENORTHINGGEOKEY', count)
                 IF count GT 0 THEN falseNorth = geotiff.PROJFALSENORTHINGGEOKEY ELSE falseNorth = 0

                 index = Where(fields EQ 'PROJSCALEATNATORIGINGEOKEY', count)
                 IF count GT 0 THEN mercator_scale = geotiff.PROJSCALEATNATORIGINGEOKEY ELSE mercator_scale = 0.9996

                 mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=thisDatum, $
                      CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, $
                      MERCATOR_SCALE=mercator_scale, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      BORDER_COLOR=border_color, TITLE=title)
                 mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
                 
                 END
                 
            110: BEGIN ; Stereographic

                 CASE 1 OF ; Longitude
                    (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
                    (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
                    ELSE:
                 ENDCASE
            
                 CASE 1 OF ; Latitude
                    (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
                    (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
                    (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
                    ELSE:
                 ENDCASE

                 index = Where(fields EQ 'PROJFALSEEASTINGGEOKEY', count)
                 IF count GT 0 THEN falseEast = geotiff.PROJFALSEEASTINGGEOKEY ELSE falseEast = 0
                 
                 index = Where(fields EQ 'PROJFALSENORTHINGGEOKEY', count)
                 IF count GT 0 THEN falseNorth = geotiff.PROJFALSENORTHINGGEOKEY ELSE falseNorth = 0

                 mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=19, $
                      CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      BORDER_COLOR=border_color, TITLE=title)
                 mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
                 
                 END

            111: BEGIN ; Lambert Azimuthal
            
                 CASE 1 OF ; Longitude
                    (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
                    (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
                    ELSE:
                 ENDCASE
            
                 CASE 1 OF ; Latitude
                    (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
                    (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
                    (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
                    ELSE:
                 ENDCASE

                 index = Where(fields EQ 'PROJFALSEEASTINGGEOKEY', count)
                 IF count GT 0 THEN falseEast = geotiff.PROJFALSEEASTINGGEOKEY ELSE falseEast = 0
                 
                 index = Where(fields EQ 'PROJFALSENORTHINGGEOKEY', count)
                 IF count GT 0 THEN falseNorth = geotiff.PROJFALSENORTHINGGEOKEY ELSE falseNorth = 0
                 
                 mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=19, $
                      CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      BORDER_COLOR=border_color, TITLE=title)
                 mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
                 
                 END

            114: BEGIN ; Orthographic

                 CASE 1 OF ; Longitude
                    (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
                    (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
                    ELSE:
                 ENDCASE
            
                 CASE 1 OF ; Latitude
                    (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
                    (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
                    (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
                    ELSE:
                 ENDCASE

                 index = Where(fields EQ 'PROJFALSEEASTINGGEOKEY', count)
                 IF count GT 0 THEN falseEast = geotiff.PROJFALSEEASTINGGEOKEY ELSE falseEast = 0
                 
                 index = Where(fields EQ 'PROJFALSENORTHINGGEOKEY', count)
                 IF count GT 0 THEN falseNorth = geotiff.PROJFALSENORTHINGGEOKEY ELSE falseNorth = 0

                 mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=19, $
                      CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      BORDER_COLOR=border_color, TITLE=title)
                 mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
                 
                 END

            116: BEGIN ; Sinusoidal
            
                 CASE 1 OF ; Longitude
                    (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
                    (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
                    ELSE:
                 ENDCASE
            
                 index = Where(fields EQ 'PROJFALSEEASTINGGEOKEY', count)
                 IF count GT 0 THEN falseEast = geotiff.PROJFALSEEASTINGGEOKEY ELSE falseEast = 0
                 
                 index = Where(fields EQ 'PROJFALSENORTHINGGEOKEY', count)
                 IF count GT 0 THEN falseNorth = geotiff.PROJFALSENORTHINGGEOKEY ELSE falseNorth = 0

                 mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=19, $
                      CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      BORDER_COLOR=border_color, TITLE=title)
                 mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
                 
                 END

            117: BEGIN ; Equirectangular
                 CASE 1 OF ; Longitude
                    (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
                    (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
                    ELSE:
                 ENDCASE
            
                 index = Where(fields EQ 'PROJFALSEEASTINGGEOKEY', count)
                 IF count GT 0 THEN falseEast = geotiff.PROJFALSEEASTINGGEOKEY ELSE falseEast = 0
                 
                 index = Where(fields EQ 'PROJFALSENORTHINGGEOKEY', count)
                 IF count GT 0 THEN falseNorth = geotiff.PROJFALSENORTHINGGEOKEY ELSE falseNorth = 0

                 
                 index = Where(fields EQ 'PROJSTDPARALLEL1GEOKEY', count)
                 IF count EQ 0 THEN BEGIN
                    index = Where(fields EQ 'PROJSTDPARALLELGEOKEY', count)
                    IF count NE 0 THEN true_scale_lat = geotiff.PROJSTDPARALLELGEOKEY
                 ENDIF ELSE true_scale_lat = geotiff. PROJSTDPARALLEL1GEOKEY
                 
                 mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=19, $
                      TRUE_SCALE_LATITUDE=true_scale_lat, CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      BORDER_COLOR=border_color, TITLE=title)
                 mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
                 
                 END
                 
            118: BEGIN ; Miller Cylindrical
            
                 CASE 1 OF ; Longitude
                    (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
                    (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
                    ELSE:
                 ENDCASE
            
                 CASE 1 OF ; Latitude
                    (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
                    (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
                    (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
                    ELSE:
                 ENDCASE

                 index = Where(fields EQ 'PROJFALSEEASTINGGEOKEY', count)
                 IF count GT 0 THEN falseEast = geotiff.PROJFALSEEASTINGGEOKEY ELSE falseEast = 0
                 
                 index = Where(fields EQ 'PROJFALSENORTHINGGEOKEY', count)
                 IF count GT 0 THEN falseNorth = geotiff.PROJFALSENORTHINGGEOKEY ELSE falseNorth = 0
                 
                 mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=19, $
                      CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      BORDER_COLOR=border_color, TITLE=title)
                 mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
                 
                 END

            121: BEGIN ; Robinson
            
                 CASE 1 OF ; Longitude
                    (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
                    (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
                    (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
                    ELSE:
                 ENDCASE
            
                 index = Where(fields EQ 'PROJFALSEEASTINGGEOKEY', count)
                 IF count GT 0 THEN falseEast = geotiff.PROJFALSEEASTINGGEOKEY ELSE falseEast = 0
                 
                 index = Where(fields EQ 'PROJFALSENORTHINGGEOKEY', count)
                 IF count GT 0 THEN falseNorth = geotiff.PROJFALSENORTHINGGEOKEY ELSE falseNorth = 0
                 
                 mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=19, $
                      CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      BORDER_COLOR=border_color, TITLE=title)
                 mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
                 
                 END
                 
        ENDCASE
        
;        RETURN, mapCoord
        
   ENDIF
   
   ; UTM projections.
   IF (projCode GT 32200) AND (projCode LE 32260) THEN BEGIN
   
        thisProjection = 101 ; UTM
        thisDatum = 5        ; WGS 72
        zone = projCode - 32200
        
        CASE 1 OF ; Longitude
              (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
              (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
              (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
              (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
               ELSE:
        ENDCASE
            
        CASE 1 OF ; Latitude
               (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
               (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
               (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
               ELSE:
        ENDCASE

        mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
            BORDER_COLOR=border_color, TITLE=title)
        mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
        
;        RETURN, mapCoord
        
   ENDIF
   
   IF (projCode GT 32300) AND (projCode LE 32360) THEN BEGIN
   
        thisProjection = 101 ; UTM
        thisDatum = 5        ; WGS 72
        zone = -(projCode - 32300)
        
        CASE 1 OF ; Longitude
              (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
              (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
              (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
              (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
               ELSE:
        ENDCASE
            
        CASE 1 OF ; Latitude
               (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
               (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
               (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
               ELSE:
        ENDCASE

        mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
            BORDER_COLOR=border_color, TITLE=title)
        mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
;        RETURN, mapCoord
                
   ENDIF

   IF (projCode GT 32400) AND (projCode LE 32460) THEN BEGIN
   
         Message, 'The WGS72BE datum is not currently supported.
;        thisProjection = 101 ; UTM
;        thisDatum = 5        ; WGS 72
;        zone = -(projCode - 32400)
;        index = Where(fields EQ 'PROJNATORIGINLONGGEOKEY', count)
;        IF count EQ 0 THEN BEGIN
;             index = Where(fields EQ 'PROJORIGINLONGGEOKEY', count)
;             IF count NE 0 THEN lon = geotiff.PROJORIGINLONGGEOKEY
;        ENDIF ELSE lon = geotiff.PROJNATORIGINLONGGEOKEY
;             index = Where(fields EQ 'PROJNATORIGINLATGEOKEY', count)
;        IF count EQ 0 THEN BEGIN
;              index = Where(fields EQ 'PROJORIGINLATGEOKEY', count)
;              IF count NE 0 THEN lat = geotiff.PROJORIGINLATGEOKEY
;        ENDIF ELSE lat = geotiff. PROJNATORIGINLATGEOKEY
;        mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=thisDatum, $
;            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone)
;        mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
;        RETURN, mapCoord
                
   ENDIF
   
   IF (projCode GT 32500) AND (projCode LE 32560) THEN BEGIN
   
         Message, 'The WGS72BE datum is not currently supported.
;        thisProjection = 101 ; UTM
;        thisDatum = 5        ; WGS 72
;        zone = -(projCode - 32500)
;        index = Where(fields EQ 'PROJNATORIGINLONGGEOKEY', count)
;        IF count EQ 0 THEN BEGIN
;             index = Where(fields EQ 'PROJORIGINLONGGEOKEY', count)
;             IF count NE 0 THEN lon = geotiff.PROJORIGINLONGGEOKEY
;        ENDIF ELSE lon = geotiff.PROJNATORIGINLONGGEOKEY
;             index = Where(fields EQ 'PROJNATORIGINLATGEOKEY', count)
;        IF count EQ 0 THEN BEGIN
;              index = Where(fields EQ 'PROJORIGINLATGEOKEY', count)
;              IF count NE 0 THEN lat = geotiff.PROJORIGINLATGEOKEY
;        ENDIF ELSE lat = geotiff. PROJNATORIGINLATGEOKEY
;        mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=thisDatum, $
;            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone)
;        mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
;        RETURN, mapCoord
                
   ENDIF   

   IF (projCode GT 32600) AND (projCode LE 32660) THEN BEGIN
   
        thisProjection = 101 ; UTM
        thisDatum = 8        ; WGS 84
        zone = projCode - 32600
        
        ; There is a bug in all versions of IDL up to IDL 8.1 apparently that
        ; produces the wrong result when a UTM projection is used in conjunction
        ; with a WGS84 datum (the most common datum used in this projection). Here
        ; we substitute the WALBECK datum, which is nearly identical to WGS84 are
        ; results in position errors of less than a meter typically.
        IF (Float(!version.release) LE 8.2) THEN BEGIN
              Print, 'Switching UTM datum from WGS84 to WALBECK to avoid UTM projection bug.'
              thisDatum = 12
        ENDIF   
        
        CASE 1 OF ; Longitude
              (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
              (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
              (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
              (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
               ELSE:
        ENDCASE
            
        CASE 1 OF ; Latitude
               (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
               (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
               (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
               ELSE:
        ENDCASE

        mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
                      BORDER_COLOR=border_color, TITLE=title)
        mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
;        RETURN, mapCoord
 
   ENDIF

   IF (projCode GT 32700) AND (projCode LE 32760) THEN BEGIN
   
        thisProjection = 101 ; UTM
        thisDatum = 8        ; WGS 84
        zone = -(projCode - 32700)
        
        ; There is a bug in all versions of IDL up to IDL 8.1 apparently that
        ; produces the wrong result when a UTM projection is used in conjunction
        ; with a WGS84 datum (the most common datum used in this projection). Here
        ; we substitute the WALBECK datum, which is nearly identical to WGS84 are
        ; results in position errors of less than a meter typically.
        IF (Float(!version.release) LE 8.2) THEN BEGIN
              Print, 'Switching UTM datum from WGS84 to WALBECK to avoid UTM projection bug.'
              thisDatum = 12
        ENDIF   
        
        CASE 1 OF ; Longitude
              (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
              (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
              (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
              (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
               ELSE:
        ENDCASE
            
        CASE 1 OF ; Latitude
               (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
               (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
               (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
               ELSE:
        ENDCASE

        mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
            BORDER_COLOR=border_color, TITLE=title)
         mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
;        RETURN, mapCoord
        
   ENDIF

   ; State Zone Projections
   IF (projCode GT 26700) AND (projCode LE 26899) THEN BEGIN
        thisProjection = 101 ; UTM
        thisDatum = 0        ; CLARK 1866 or NAD27
        zone = projCode - 26700
        
        CASE 1 OF ; Longitude
              (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
              (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
              (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
              (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
               ELSE:
        ENDCASE
            
        CASE 1 OF ; Latitude
               (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
               (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
               (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
               ELSE:
        ENDCASE

        mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
            BORDER_COLOR=border_color, TITLE=title)
        mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
;        RETURN, mapCoord
        
   ENDIF
 
   IF (projCode GT 26900) AND (projCode LE 27999) THEN BEGIN
   
        thisProjection = 101 ; UTM
        thisDatum = 8        ; WGS84 or NAD83
        zone = projCode - 26900
        
        ; There is a bug in all versions of IDL up to IDL 8.1 apparently that
        ; produces the wrong result when a UTM projection is used in conjunction
        ; with a WGS84 datum (the most common datum used in this projection). Here
        ; we substitute the WALBECK datum, which is nearly identical to WGS84 are
        ; results in position errors of less than a meter typically.
        IF (Float(!version.release) LE 8.2) THEN BEGIN
              Print, 'Switching UTM datum from WGS84 to WALBECK to avoid UTM projection bug.'
              thisDatum = 12
        ENDIF   
        
        CASE 1 OF ; Longitude
              (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
              (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
              (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
              (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
               ELSE:
        ENDCASE
            
        CASE 1 OF ; Latitude
               (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
               (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
               (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
               ELSE:
        ENDCASE

        mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
            BORDER_COLOR=border_color, TITLE=title)
        mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
;        RETURN, mapCoord
        
   ENDIF
  
   ; State Zone Projections
   IF (projCode GT 32000) AND (projCode LE 32099) THEN BEGIN
   
        thisProjection = 101 ; UTM
        thisDatum = 0        ; CLARK 1866 or NAD27
        zone = projCode - 32000
        
        CASE 1 OF ; Longitude
              (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
              (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
              (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
              (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
               ELSE:
        ENDCASE
            
        CASE 1 OF ; Latitude
               (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
               (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
               (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
               ELSE:
        ENDCASE

        mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
            BORDER_COLOR=border_color, TITLE=title)
        mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
;        RETURN, mapCoord
        
   ENDIF
 
   IF (projCode GT 32100) AND (projCode LE 32299) THEN BEGIN
   
        thisProjection = 101 ; UTM
        thisDatum = 8        ; WGS84 or NAD83
        zone = projCode - 32100
        
        ; There is a bug in all versions of IDL up to IDL 8.1 apparently that
        ; produces the wrong result when a UTM projection is used in conjunction
        ; with a WGS84 datum (the most common datum used in this projection). Here
        ; we substitute the WALBECK datum, which is nearly identical to WGS84 are
        ; results in position errors of less than a meter typically.
        IF (Float(!version.release) LE 8.2) THEN BEGIN
              Print, 'Switching UTM datum from WGS84 to WALBECK to avoid UTM projection bug.'
              thisDatum = 12
        ENDIF   
        
        CASE 1 OF ; Longitude
              (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotiff.PROJNATORIGINLONGGEOKEY
              (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJORIGINLONGGEOKEY
              (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotiff.PROJCENTERLONGGEOKEY
              (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotiff.PROJSTRAIGHTVERTPOLELONGGEOKEY
               ELSE:
        ENDCASE
            
        CASE 1 OF ; Latitude
               (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotiff.PROJNATORIGINLATGEOKEY
               (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJORIGINLATGEOKEY
               (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotiff.PROJCENTERLATGEOKEY
               ELSE:
        ENDCASE

        mapCoord = Obj_New('cgMapCoord', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
            BORDER_COLOR=border_color, TITLE=title)
        mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
;        RETURN, mapCoord
        
   ENDIF
   
   ; If we get here, we don't know what to do. 
   IF N_Elements(mapCoord) EQ 0 THEN BEGIN
      Message, 'Map projection code ' + StrTrim(projCode,2) + ' is not supported.
   ENDIF
   
   ; Need a map outline in this cgMapCoord object?
   IF Keyword_Set(map_outline) THEN BEGIN
       outline = Obj_New('Map_Outline', MAP_OBJECT=mapCoord, /HIRES, $
                        COLOR=outline_color, FILL=Keyword_Set(fill))
       mapCoord -> SetProperty, OUTLINE_OBJECT=outline
   ENDIF
      
   ; Need a map grid in this cgMapCoord object?
   IF Keyword_Set(map_grid) THEN BEGIN
       grid = Obj_New('Map_Grid', MAP_OBJECT=mapCoord, COLOR=grid_color)
       mapCoord -> SetProperty, GRID_OBJECT=grid
   ENDIF

   RETURN, mapCoord
END