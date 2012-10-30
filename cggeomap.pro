; docformat = 'rst'
;
; PURPOSE:
;   The purpose of this function is to translate a GEOTIFF structure
;   (as returned by QUERY_TIFF or READ_TIFF) into a map coordinate
;   object (cgMap) that can be used to georeference images with a 
;   map data coordinate system. The Coyote Library is required.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2011, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
;   The purpose of this function is to translate a GEOTIFF structure
;   (as returned by QUERY_TIFF or READ_TIFF) into a map coordinate
;   object (cgMap) that can be used to georeference images with a 
;   map data coordinate system. The Coyote Library is required.
;
;   It is not possible to have a one-to-one mapping between every GeoTIFF file and a map projection
;   in IDL, since IDL has a limited number of map projections and datums available. And, even at that,
;   I have not implemented all of IDL's map projections or datums, only those that I thought were most
;   likely to be encountered in my own work. If you run into a GeoTIFF file that does not work in this
;   code (either because of an error in the code or because it is not supported), please contact me.
;   I am interested in supporting as many GeoTIFF files as possible and I will take pains to do so if
;   I know they are needed.
;   
; :Categories:
;    Graphics, Map Projections
;    
; :Returns:
;     A cgMap coordinate object is returned containing the map projection information
;     for the image.
;       
; :Params:
;    image:  in, optional, type=various
;        A GeoTIFF image. This can optionally be the name of a GeoTiff file.
;        If a filename is used, do not pass the geoTiff parameter, as this
;        parameter will be obtained from the GeoTiff file.
;               
; :Keywords:
;     boundary: out, optional, type=array
;        A four-element array giving the boundaries of the map projection in the form
;        [x0,y0,x1,y1]. This is a more convenient way of expressing the range
;        of the map space.
;     ccolor: in, optional, type=string, default='Charcoal'
;        The name of a color the map continents should be displayed with. The default
;        is "charcoal". Color names are those supported by cgColor.
;     clip: in, optional, type=boolean, default=0
;         Set this keyword to display the image (assumes the `Display` keyword is set)
;         with a two-precent histogram clipping.
;     continents: in, optional, type=boolean, default=0
;         If a cgMap object is made successfully, then setting this keyword
;         will add a cgMapContinents object to the cgMap object.   
;     display: in, optional, type=boolean, default=0
;         Set this keyword to display the image with the map annotations in a 
;         resizeable cgWindow.          
;     ellipsoid: out, optional, type=string
;         The name of the ellipsoid used in the GeoTiff file.          
;     gcolor: in, optional, type=string, default='Gray'
;         The name of a color the map grid should be displayed with. The default
;         is "gray". Color names are those supported by cgColor.
;     geotiff: in, optional, type=structure
;        A GeoTIFF structure of geoTags. Normally obtained by calling
;        QUERY_TIFF or READ_TIFF, but will be read from the file if a
;        filename is passed as the first positional parameter. If the geotag
;        is read from the file, it can be passed back to the user by setting
;        this keyword to a named variable.         
;     grid: in, optional, type=boolean, default=0
;         If a cgMap object is made successfully, then setting this keyword
;         will add a cgMapGrid object to the cgMap object.  
;     image: out, optional, type=varies
;         Set this keyword to a named variable that on exit will contain the image data.                     
;     latlonbox: out, optional, type=array
;        A four-element array giving the boundaries of the map projection in the
;        Google Map form of [north, south, east, west]. This is useful when you
;        are creating image overlays to be added to Goggle Earth.
;     map_projection: out, optional, type=string
;         The name of the map projection found in the GeoTiff file.
;     mcolor: in, optional, type=string, default='Black'
;         The name of a color the map should be displayed in. (Normally the map
;         border and map title are displayed in this color.)
;     onimage: in, optional, type=boolean, default=0
;         Set this keword if the map object is to get its position from the last
;         cgImage command issued.
;     palette: out, optional, type=bytarr
;         If the GeoTiff file contains RGB color vectors, and keywords to cgGeoTiff cause the
;         file to be read (e.g, IMAGE or DISPLAY), then this output keyword will contain those
;         vectors in a 3-by-256 byte array.
;     silent: in, optional, type=boolean, default=0
;         IDL cannot map every GeoTiff image to a supported map projection or datum.
;         Normally, if the GeoTIFF image is unsupported, an error message is issued.
;         Setting this keyword will suppress such error messages. If you do this, you
;         MUST check the SUCCESS keyword to see if the program ran successfully. (Of
;         course, it is a good idea to check it in any case!)
;     sub_rect: in, optional, type=integer
;         Set this keyword to a four-element array, [x, y, width, height], that 
;         specifies a rectangular region within the file to extract. Only the 
;         rectangular portion of the image selected by this keyword is read and 
;         returned. The rectangle is measured in pixels from the lower left 
;         corner (right hand coordinate system). If this keyword is not use, the
;         entire image is read.
;     success: out, optional, type=boolean, default=0
;         An output variable that will contain a 1 if the map coordinate object was
;         created successfully. Or to a 0 if it was not created successfully.
;     title: in, optional, type=string, default=""                      
;         The title of the map projection.
;     _extra: in, optional
;         Any keyword appropriate for cgImage can be collected and passed along if
;         the DISPLAY keyword is also set.
;          
; :Examples:
;    To display a GeoTiff image in cgWindow with map annotations::
;       file = 'C:\IDL\data\tiff\AF03sep15b.n16-VIg.tif'
;       mapCoord = cgGeoMap(file, /Continents, /Grid, /Display)
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
;        Converted from old GeoTIff program in Catalyst Library to run with Coyote Graphics routines.
;           9 November 2011. David W. Fanning.
;         Assuming meters as unit length if can't find linear measure in file. 1 Dec 2011. DWF.
;         Added UTM Zone 18S (projection 29178) for LandSAT MSS images over Amazon. 1 Dec 2011. DWF.
;         Added the ability to pass cgImage keywords via the keyword inheritance mechanism. 1 Dec 2011. DWF.
;         Added SUB_RECT keyword. 5 Dec 2011. DWF.
;         Added ability to read GOES EAST and GOES WEST GeoTiff files  from this NOAA web site:
;            http://http://www.osdpd.noaa.gov/ml/gis/index.html. 26 Dec 2011. DWF.
;         Modified code to read GeoTiff files created by HEG v2.11 from HDF-EOS2 grid files. 30 Dec 2011. DWF.
;         Modified to read multi-dimensional GeoTiff images and reverse the images correct. 12 Jan 2012. DWF.
;         Write base filename as title for window if the DISPLAY keyword is set. 22 Feb 2012. DWF.
;         Had inexplicably left out CENTER_LATITUDE parameter in Equirectangular projection. 30 July 2012. DWF.
;         Added PALETTE keyword to return the RGB color palette present in the file, if any. 16 August 2012. DWF.
;         Moved the geotiff argument to a GEOTIFF keyword, as I always expect it to be this way. 6 Sept 2012. DWF.
;         Removed UTM/WGS84 warning message in IDL 8.2, as this problem has been fixed in IDL 8.2. 2 Oct 2012. DWF.
;         Added BOUNDARY, ELLIPSOID, LATLONBOX, and MAP_PROJECTION output keywords to facility 
;            creating Google Earth overlays. 30 Oct 2012. DWF.
;         
; :Copyright:
;     Copyright (c) 2011-2012, Fanning Software Consulting, Inc.
;-
Function cgGeoMap, image, $
    BOUNDARY=boundary, $
    CLIP=clip, $
    DISPLAY=display, $
    ELLIPSOID=ellipsoid, $
    GCOLOR=gcolor, $
    GEOTIFF=geotiff, $
    GRID=grid, $
    CCOLOR=ccolor, $
    CONTINENTS=continents, $
    IMAGE=outImage, $
    LATLONBOX=latlonbox, $
    MAP_PROJECTION=map_projection, $
    MCOLOR=mcolor, $
    ONIMAGE=onimage, $
    PALETTE=palette, $
    SILENT=silent, $
    SUB_RECT=sub_rect, $
    SUCCESS=success, $
    TITLE=title, $
    _EXTRA=extra

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
    SetDefaultValue, mcolor, 'black'
    SetDefaultValue, gcolor, 'charcoal'
    SetDefaultValue, ccolor, 'charcoal'
    SetDefaultValue, title, ""
    onimage = Keyword_Set(onimage)
    
   ; If no parameters, ask about reading a GeoTiff file.
   IF N_Elements(image) EQ 0 THEN BEGIN
        geofile = cgPickfile(Title='Select GeoTiff File...', FILTER=['*.tif', '*.tiff'])
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
        
        ; For further processing we need image to be equal to geofile.
        image = geofile
   ENDIF
   
   
   ; Is the first parameter the name of a file or an image?
   IF (N_Elements(image) NE 0) || (N_Elements(geofile) NE 0) THEN BEGIN
            
       ; Is the parameter a string?
       IF Size(image, /TNAME) NE 'STRING' THEN BEGIN
       
           ; Then we are going to assume this is an image and that you have passed
           ; a geotiff structure to us.
           IF N_Elements(geotiff) EQ 0 THEN Message, 'A GeoTiff structure must be passed with the image'
           
           ; Get the image sizes
           dims = Image_Dimensions(image, XSIZE=xsize, YSIZE=ysize)
       
       ENDIF ELSE BEGIN
       
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
                   
                4: BEGIN
                   indices = Where(dims NE 4, count)
                   IF count NE 2 THEN Message, 'Cannot determine the image dimensions.'
                   xsize = dims[indices[0]]
                   ysize = dims[indices[1]]
                   END

                ELSE: Message, 'Do not know how to handle an image with ' + StrTrim(channels,2) + ' channels'
            ENDCASE
      ENDELSE
   ENDIF

   ; Get the fields of the geotiff structure.
   fields = Tag_Names(geotiff)
   
   ; We can only handle raster images with projected coordinate systems, unless this is 
   ; a GeoTiff file with Geographic model, such as GOES images. If it is one of these images,
   ; then I am going to assign a project code to this image and handle it in the special projection
   ; section below.
   geoModel = 0
   gtModelIndex = Where(fields EQ 'GTMODELTYPEGEOKEY', gtModelType)
   
   IF gtModelType GT 0 THEN BEGIN
   
       IF (geotiff.gtModelTypeGeoKey EQ 2) THEN BEGIN
       
           ; We need a map projection code if we have one in the file
           index = Where(fields EQ 'PROJECTEDCSTYPEGEOKEY', projCount)
           IF projCount GT 0 THEN projCode = geotiff.PROJECTEDCSTYPEGEOKEY
           index = Where(fields EQ 'PROJECTIONGEOKEY', projCount)
           IF projCount GT 0 THEN projCode = geotiff.PROJECTIONGEOKEY
           IF N_Elements(projCode) EQ 0 THEN projCode = 99901 ; GOES East/West Satellites
           geoModel = 1   
       ENDIF
   
   ENDIF 
   
   ; We want all measurements to be in meters. First, check to see of there is a ProjLineearUnitsGeokey.
   ; If not, check the GeogLinearUnitsGeokey.
   index = Where(fields EQ 'PROJLINEARUNITSGEOKEY', unitsCount)
   IF unitsCount EQ 0 THEN BEGIN
      index = Where(fields EQ 'GEOGLINEARUNITSGEOKEY', unitsCount)
      IF unitsCount NE 0 THEN unitValue = geotiff.GEOGLINEARUNITSGEOKEY
   ENDIF ELSE unitValue = geotiff.PROJLINEARUNITSGEOKEY
   
   ; Can't find a linear unit. Will assume meters. No trouble with this so far. (Fingers crossed.)
   IF N_Elements(unitValue) EQ 0 THEN BEGIN
        unitValue = 9001L
;        IF ~Keyword_Set(silent) THEN $
;            Message, 'Cannot find a linear unit key in the GeoTiff structure. Assuming meter.', /Informational
   ENDIF
   CASE unitValue OF
   
        '9001':       ; Linear meter 
        '9002': BEGIN ; Linear foot
            xscale = xscale * (1.0D/3.28D)
            yscale = yscale * (1.0D/3.28D)
            END            
        ELSE: Message, 'Linear unit ' + StrTrim(unitValue,2) + ' is not supported in this program.'
        
   ENDCASE
   
   ; We need a map projection code if we have one in the file
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
   y1 = (geotiff.ModelTiePointTag)[4]
   x1 = x0 + (xscale * xsize)
   y0 = y1 - (yscale * ysize)
   xrange = [x0, x1]
   yrange = [y0, y1]
;   Print, 'X Range: ', xrange
;   Print, 'Y Range: ', yrange
   
   ; Is this a user-defined projection code?
   IF projCode EQ 32767L THEN BEGIN
   
        ; What kind of datum is being used?
        IF Where(fields EQ 'GEOGRAPHICTYPEGEOKEY') NE -1 THEN BEGIN
            CASE geotiff.GEOGRAPHICTYPEGEOKEY OF
                   0: thisDatum = 19 ; Undefined, assgined to sphere.
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
                4267: thisDatum = 0  ; NAD27, same as Clark 1866
                4322: thisDatum = 5  ; WGS 72
                4326: thisDatum = 8  ; WGS 84
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
        IF Where(fields EQ 'PROJCOORDTRANSGEOKEY') NE -1 THEN BEGIN
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
        ENDIF

        IF Where(fields EQ 'PROJCOORDTRANSGEOKEY') NE -1 THEN BEGIN
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
        ENDIF ELSE BEGIN
           IF geoModel THEN thisProjection = 117
        ENDELSE
        IF N_Elements(thisProjection) EQ 0 THEN Message, 'Unknown map projection for this GeoTIFF image.' 
        
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

                 mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
                      CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, $
                      STANDARD_PAR1=std_parallel_1, STANDARD_PAR2=std_parallel_2, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, NAME='FROM_GEOCOORD', $
                      COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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

                 mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
                      CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, $
                      STANDARD_PAR1=std_parallel_1, STANDARD_PAR2=std_parallel_2, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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
                 
                 mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
                      TRUE_SCALE_LATITUDE=true_scale_lat, CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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

                 mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
                      CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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

                 mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
                      CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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

                 mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
                      CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, $
                      MERCATOR_SCALE=mercator_scale, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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

                 mapCoord = Obj_New('cgMap', thisProjection, DATUM=19, $
                      CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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
                 
                 mapCoord = Obj_New('cgMap', thisProjection, DATUM=19, $
                      CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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

                 mapCoord = Obj_New('cgMap', thisProjection, DATUM=19, $
                      CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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

                 mapCoord = Obj_New('cgMap', thisProjection, DATUM=19, $
                      CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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
                 
                 mapCoord = Obj_New('cgMap', thisProjection, DATUM=19, $
                      TRUE_SCALE_LATITUDE=true_scale_lat, CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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
                 
                 mapCoord = Obj_New('cgMap', thisProjection, DATUM=19, $
                      CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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
                 
                 mapCoord = Obj_New('cgMap', thisProjection, DATUM=19, $
                      CENTER_LONGITUDE=lon, $
                      FALSE_EASTING=falseEast, FALSE_NORTHING=falseNorth, $
                      COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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

        mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
            COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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

        mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
            COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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
;        mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
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
;        mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
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
        IF (Float(!version.release) LT 8.2) THEN BEGIN
              IF ~Keyword_Set(silent) THEN Print, 'Switching UTM datum from WGS84 to WALBECK to avoid UTM projection bug.'
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

        mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
                      COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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
        IF (Float(!version.release) LT 8.2) THEN BEGIN
              IF ~Keyword_Set(silent) THEN Print, 'Switching UTM datum from WGS84 to WALBECK to avoid UTM projection bug.'
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

        mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
            COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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

        mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
            COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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
        IF (Float(!version.release) LT 8.2) THEN BEGIN
              IF ~Keyword_Set(silent) THEN Print, 'Switching UTM datum from WGS84 to WALBECK to avoid UTM projection bug.'
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

        mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
            COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
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

        mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
            COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
        mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
        
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
        IF (Float(!version.release) LT 8.2) THEN BEGIN
              IF ~Keyword_Set(silent) THEN Print, 'Switching UTM datum from WGS84 to WALBECK to avoid UTM projection bug.'
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

        mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
            COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
        mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange

   ENDIF
   
   ; Projections that have been added to support work I am doing for clients.
   ; LandSAT MSS Amazon Basin
   IF (projCode EQ 29178) THEN BEGIN ; UTM Zone 18S
   
        thisProjection = 101 ; UTM
        IF (Float(!Version.Release) GE 8) $
            THEN thisDatum = 23 $ ; South American 1969
            ELSE thisDatum = 14   ; Australian National (same as SAD-69)
        zone = -18           ; Zone 18S
                
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

        mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
            COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
        mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange

   ENDIF
   
   IF (projCode EQ 29181) THEN BEGIN ; UTM Zone 21S
   
        thisProjection = 101 ; UTM
        IF (Float(!Version.Release) GE 8) $
            THEN thisDatum = 23 $ ; South American 1969
            ELSE thisDatum = 14   ; Australian National (same as SAD-69)
        zone = -21                ; Zone 21S
                
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

        mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, $
            COLOR=mcolor, TITLE=title, ONIMAGE=onimage)
        mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange

   ENDIF

   IF (projCode EQ 99901) THEN BEGIN ; GOES EAST image
   
        thisProjection = 117 ; Equirectangular
        
        ; What kind of datum is being used?
        IF Where(fields EQ 'GEOGRAPHICTYPEGEOKEY') NE -1 THEN BEGIN
            CASE geotiff.GEOGRAPHICTYPEGEOKEY OF
                   0: thisDatum = 19 ; Undefined, assgined to sphere.
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
                4267: thisDatum = 0  ; NAD27, same as Clark 1866
                4322: thisDatum = 5  ; WGS 72
                4326: thisDatum = 8  ; WGS 84
                ELSE:
            ENDCASE
        ENDIF
        
        ; Get the pixel scale values, which are in lat/lon degrees in this case.
        xscale = (geotiff.ModelPixelScaleTag)[0]
        yscale = (geotiff.ModelPixelScaleTag)[1]
       
        ; Get the tie points (in lat/lon space) and calculate the map projection range.
        x0 = (geotiff.ModelTiePointTag)[3]
        y0 = (geotiff.ModelTiePointTag)[4]
        x1 = x0 + (xscale * xsize)
        y1 = y0 - (yscale * ysize)
        xrange = [x0, x1]
        yrange = [y0, y1]
        
        mapCoord = Obj_New('cgMap', thisProjection, DATUM=thisDatum, $
            CENTER_LONGITUDE=(xrange[1]-xrange[0])/2.0 + xrange[0], $
            CENTER_LATITUDE=(yrange[1]-yrange[0])/2.0 + yrange[0], $
            COLOR=mcolor, TITLE=title, ONIMAGE=onimage, XRANGE=xrange, YRANGE=yrange)
 
   ENDIF
   
   ; If this is a Geographic Model, then the ranges are in lon/lat space.
   ; They need to be converted to XY projected space.
   IF geoModel THEN BEGIN
   
       mapCoord -> GetProperty, XRANGE=xrange, YRANGE=yrange
       
       ; Many of these geographic models go between 0 and 360 degrees
       ; of longitude, which cause me a great deal of headache. If the
       ; range is 360 degrees, more or less, then I'm just going to
       ; assign the range.
       IF Floats_Equal(xrange[1] - xrange[0], 360.0, ULP=5) THEN xrange = [-180, 180]
       
       ; Otherwise, I will project them into XY meters.
       xy = mapCoord -> Forward(xrange, yrange)
       mapCoord -> SetProperty, XRANGE=Reform(xy[0,*]), YRANGE=Reform(xy[1,*])
       
   ENDIF
   
   ; If we get here, we don't know what to do. 
   IF (N_Elements(mapCoord) EQ 0) THEN BEGIN
      Message, 'Map projection code ' + StrTrim(projCode,2) + ' is not supported.
   ENDIF
   
   ; Need a map outline in this cgMapCoord object?
   IF Keyword_Set(continents) THEN BEGIN
       contObj = Obj_New('cgMapContinents', mapCoord, /HIRES, $
                        COLOR=ccolor, FILL=Keyword_Set(fill), NAME='MAPCONTINENTS')
       IF ~Obj_Valid(contObj) THEN $
          Message, 'Cannot successfully create a cgMapContinents object.'
       mapCoord -> SetProperty, CONTINENTS=contObj
   ENDIF
      
   ; Need a map grid in this cgMapCoord object?
   IF Keyword_Set(grid) THEN BEGIN
       gridObj = Obj_New('cgMapGrid', mapCoord, COLOR=gcolor, /AUTODRAWGRID, NAME='MAPGRID')
       IF ~Obj_Valid(gridObj) THEN $
          Message, 'Cannot successfully create a cgMapGrid object.'
       mapCoord -> SetProperty, GRID=gridObj
   ENDIF
   
   ; Would you like to display the image?
   IF Keyword_Set(display) THEN BEGIN
       IF N_Elements(geofile) EQ 0 THEN geofile = image
       thisImage = Read_Tiff(geofile, r, g, b, SUB_RECT=sub_rect)
       dims = Image_Dimensions(thisImage, YINDEX=yindex)
       thisImage = Reverse(Temporary(thisImage), yindex+1)
       IF N_Elements(r) NE 0 THEN palette=[[r],[g],[b]]
       mapCoord -> SetProperty, ONIMAGE=1
       cgWindow, WASPECT=Float(ysize)/xsize, WTitle=cgRootName(geofile)
       IF Keyword_Set(clip) THEN BEGIN
            cgImage, thisImage, /Keep_Aspect, /AddCmd, Margin=0.05, $
                STRETCH=2, PALETTE=palette, _EXTRA=extra       
       ENDIF ELSE BEGIN
            cgImage, thisImage, /Keep_Aspect, /AddCmd, Margin=0.05, PALETTE=palette, _EXTRA=extra
       ENDELSE
       cgWindow, 'Draw', mapCoord, /Method, /AddCmd
   ENDIF
   
   IF Arg_Present(outimage) THEN BEGIN 
       IF N_Elements(thisImage) NE 0 THEN BEGIN
            IF N_Elements(r) NE 0 THEN palette=[[r],[g],[b]]
            outimage = Temporary(thisImage)
       ENDIF ELSE BEGIN
             IF N_Elements(geofile) EQ 0 THEN BEGIN
                geofile = image
                outimage = Read_Tiff(geofile, r, g, b, SUB_RECT=sub_rect)
                IF N_Elements(r) NE 0 THEN palette=[[r],[g],[b]]
                dims = Image_Dimensions(outimage, YINDEX=yindex)
                outimage = Reverse(Temporary(outimage), yindex+1)
              ENDIF ELSE outimage = image
       ENDELSE
   ENDIF
   
   ; Get return values.
   mapCoord -> GetProperty, $
      BOUNDARY=boundary, $
      ELLIPSOID=ellipsoid, $
      LATLONBOX=latlonbox, $
      MAP_PROJECTION=map_projection
   
   
   RETURN, mapCoord
END