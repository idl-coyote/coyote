; docformat = 'rst'
;
; NAME:
;   cgImage2KML
;
; PURPOSE:
; This program creates a KML file that can be opened in Google Earth to display the 
; image drapped over the Google Earth terrain. A corresponding image file is also
; produced. The KML and image file must be in the same directory to use them with
; Google Earth.
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
; This program creates a KML file that can be opened in Google Earth to display the 
; image drapped over the Google Earth terrain. A corresponding image file is also
; produced. The KML and image file must be in the same directory to use them with
; Google Earth.
; 
; :Categories:
;    Graphics, FileIO, Maps
;    
; :Params:
; 
;    image: in, optional
;       A 2D image or a 24-bit image with or without an alpha channel. If an alpha
;       channel is present, it will be modified by the program if the `Transparent`
;       keyword is used. An image is required unless the `GeoTiff` keyword is used
;       to obtain an image.
;       
;    mapcoord: in, optional, type=object
;       A map coordinate object (cgMap) from which map projection information and map
;       boundaries for the image overlay can be obtained. This parameter is required
;       unless the `GeoTiff` keyword is used to obtain a map coordinate object.
;       
; :Keywords:
; 
;    brewer: in, optional, type=boolean, default=0
;       This keyword is used only if the `CTIndex` keyword is used to select a color table number.
;       Setting this keyword allows Brewer color tables to be used.
;         
;    ctindex: in, optional, type=integer
;       The index number of a color table. The `Brewer` and `Reverse` keywords will be checked
;       to see how to load the color table into the `Palette` keyword. This keyword will take
;       precidence over any colors that are loaded with the `Palette` keyword. This keyword
;       applies only to 2D images.
;         
;    description: in, optional, type=string
;       A string that is used to describe the image in the Google Earth interface.
;        
;    geotiff: in, optional, type=string
;       The name of a GeoTiff file from which the `image`, `mapcoord`, `palette` (possibly), 
;       and `latlonbox` values can be obtained. 
;        
;    filename: in, optional, type=string, default='kml_image.kml'
;        The name of the KML file that will be created. The image file will have the same name,
;        but with a *.png file extension. The KML file and the image file will be created in the
;        same directory.
;         
;    latlonbox: out, optional, type=array
;        A four-element array giving the boundaries of the map projection in the
;        Google Map form of [north, south, east, west]. Normally, this information
;        is obtained from the mapCoord object and need not be passed in. The values
;        are in latitude and longitude coordinates that go from -90 to 90 and -180 to
;        180 degrees, respectively.
;        
;    max_value: in, optional
;        The value to use for the MAX value when the image is scaled with BYTSCL.
;
;    min_value: in, optional
;        The value to use for the MIN value when the image is scaled with BYTSCL.
;
;    missing_value: in, optional, type=various
;        The "color" of a pixel that will be treated as a "missing" color or value.
;        Any pixels in the image with this color value will be set completely
;        transparent. If `Color` is a string, use cgColor to obtain a color triple. 
;        If `Color` is a non-strint scalar, this value is taken to be the missing color index
;        in a 2D image. Otherwise, this is assumed to be a color triple that indicates
;        the "missing" color or value in the output image. The alpha channel in the output image
;        is set to 0 for the "missing" color, which makes this value completely transparent.
;        Used only if the `Transparent` keyword is used.
;        
;    order: in, optional, type=integer, default=0
;        The drawing order of image overlay. The first order is 0. Images with a higher
;        order are drawn on top of images with a lower order.
;        
;    palette: in, optional, type=byte
;        Set this keyword to a 3x256 or 256x3 byte array containing the RGB color 
;        vectors to be loaded before the transparent image is created. Such vectors can be 
;        obtained, for example, from cgLoadCT with the RGB_TABLE keyword::
;               
;                IDL> cgLoadCT, 4, /BREWER, /REVERSE, RGB_TABLE=palette
;                IDL> tImage = cgTransparentImage( cgDemoData(7), PALETTE=palette)
;                
;        The default is to use whatever colors are loaded in the current hardware color table.
;        A palette applies only to 2D input images.
;         
;    reverse: in, optional, type=boolean, default=0
;        Set this keyword to reverse the color table vectors selected with the `CTIndex` keyword.

;    transparent: in, optional, type=integer, default=50
;        The percentage of transparency desired in the output image. A number 
;        between 0 and 100.
;        
; :Examples:
;    Here is how you can put an AVHRR NDVI image of Africa on a Google Earth display:: 
;    
;       ;; Download the image file from the Coyote web page.
;       netObject = Obj_New('IDLnetURL')
;       url = 'http://www.idlcoyote.com/data/AF03sep15b.n16-VIg.tif'
;       returnName = netObject -> Get(URL=url, FILENAME='AF03sep15b.n16-VIg.tif')
;       Obj_Destroy, netObject
;       
;       ;; Create the image overlay KML file.
;       cgImage2KML, GeoTiff='AF03sep15b.n16-VIg.tif', Min_Value=0, CTIndex=11, $
;          /Brewer, /Reverse, Transparent=50, Filename='avhrr_ndvi.kml', $
;          Description='AVHRR NDVI Data from Africa'
;          
;       ;; Start Google Earth and open the KML file you just created.
;       
;    The output should look like the figure below.
;    
; .. image:: google_earth_overlay.png
;  
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
;     Change History::
;        Written, 30 October 2012 by David W. Fanning.
;        Added ORDER keyword and fixed a typo concerning MISSING_VALUE. 31 Oct 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
PRO cgImage2KML, image, mapCoord, $
   CTINDEX=ctindex, $
   BREWER=brewer, $
   DESCRIPTION=description, $
   GEOTIFF=geotiff, $
   FILENAME=filename, $
   LATLONBOX=latlonbox, $
   MAX_VALUE=max_value, $
   MIN_VALUE=min_value, $
   MISSING_VALUE=missing_value, $
   ORDER=order, $
   PALETTE=palette, $
   REVERSE=reverse, $
   TRANSPARENT=transparent

   Compile_Opt idl2
   
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
   ENDIF
   
   ; Has the GEOTIFF keyword been used to obtain program variables?
   IF N_Elements(geotiff) NE 0 THEN BEGIN
      mapCoord = cgGeoMap(geotiff, $
          BOUNDARY=boundary, $
          ELLIPSOID=ellipsoid, $
          IMAGE=image, $
          LATLONBOX=latlonbox, $
          MAP_PROJECTION=map_projection, $
          PALETTE=palette)
   ENDIF
   
   ; Check required parameters.
   IF N_Elements(image) EQ 0 THEN Message, 'An image parameter is required to proceed.'
   IF N_Elements(mapCoord) EQ 0 THEN BEGIN
      IF N_Elements(latlonBox) EQ 0 THEN Message, 'A map coordinate object is required to proceed.'
   ENDIF ELSE BEGIN
      mapCoord -> GetProperty, $
          BOUNDARY=boundary, $
          ELLIPSOID=ellipsoid, $
          LATLONBOX=latlonbox, $
          MAP_PROJECTION=map_projection
   ENDELSE
   IF N_Elements(latlonBox) EQ 0 THEN Message, 'Map boundaries for the image cannot be obtained.'
   IF N_Elements(order) EQ 0 THEN order = 0
   
   ; Need a filename?
   IF N_Elements(filename) EQ 0 THEN BEGIN
      CD, CURRENT=thisDir
      filename = Filepath(ROOT_DIR=thisDir, 'kml_image.kml')
   ENDIF
   
   ; Construct the image filename
   rootName = cgRootName(filename, DIRECTORY=thisDir, EXTENSION=ext)
   IF StrUpCase(ext) NE 'KML' THEN Message, 'The output filename must have a KML file extension.'
   imageFilename = Filepath(ROOT_DIR=thisDir, rootname + '.png')
   
   ; If you got this far, and you don't have a map coordinate object, you will have
   ; to create one.
   IF N_Elements(mapCoord) EQ 0 THEN BEGIN
       mapCoord = Obj_New('cgMap', 'Equirectangular', Ellipsoid='WGS 84', $
          XRange=[latlonbox[3], latlonbox[4]], YRange=[latlonBox[1], latlonbox[0]], /LATLON_RANGES)
       mapCoord -> GetProperty, $
          BOUNDARY=boundary, $
          ELLIPSOID=ellipsoid, $
          MAP_PROJECTION=map_projection
   ENDIF
   
   ; Are you loading a color table?
   IF N_Elements(ctindex) NE 0 THEN BEGIN
      cgLoadCT, ctindex, BREWER=brewer, REVERSE=reverse, RGB_TABLE=palette
   ENDIF
   
   ; Otherwise, let's use gray-scale colors for the image.
   IF N_Elements(palette) EQ 0 THEN cgLoadCT, 0, RGB_TABLE=palette
   
   ; If the map projection is not "EQUIRECTANGULAR" or the ellipsoid is not "WGS84", then the
   ; image has to be warped into the correct map projection.
   IF (StrUpCase(map_projection) NE 'EQUIRECTANGULAR') || (StrUpCase(StrCompress(ellipsoid, /REMOVE_ALL)) NE 'WGS84') THEN BEGIN
      googleMapCoord = Obj_New('cgMap', 'Equirectangular', Ellipsoid='WGS 84')
       warped = cgChangeMapProjection(image, mapCoord, MAPOUT=googleMapCoord, $
          LATLONBOX=latlonbox)
   ENDIF ELSE warped = image
   
   ; Byte scale the image.
   IF N_Elements(missing_value) NE 0 THEN BEGIN
      imgType = Size(warped, /TNAME)
      IF (imgType NE 'FLOAT') && (imgType NE 'DOUBLE') THEN warped = Float(warped)
      missing = Where(warped EQ missing_value, count)
      IF count GT 0 THEN warped[missing] = !Values.F_NAN
      IF (Min(warped) LT 0) || (Max(warped) GT 255) THEN BEGIN
         warped = BytScl(warped, MIN=min_value, MAX=max_value, /NAN, TOP=254) + 1B
      ENDIF
      IF count GT 0 THEN warped[missing] = 0B
   ENDIF ELSE BEGIN
      IF (Min(warped) LT 0) || (Max(warped) GT 255) THEN BEGIN
         warped = BytScl(warped, MIN=min_value, MAX=max_value, /NAN)
      ENDIF
   ENDELSE
   
   ; If this is a 2D image, create a 24-bit image from the palette.
   r = Reform(palette[*,0])
   g = Reform(palette[*,1])
   b = Reform(palette[*,2])
   IF (Size(warped, /N_Dimensions) EQ 2) THEN BEGIN
      warped = [ [[r[warped]]], [[g[warped]]], [[b[warped]]]]
   ENDIF
   
   ; Do we need transparency?
   IF N_Elements(transparent) NE 0 THEN BEGIN
       warped = cgTransparentImage(warped, TRANSPARENT=transparent, MISSING_VALUE=[r[0],g[0],b[0]])
   ENDIF
   
   ; Save the image as a PNG file.
   dims = Image_Dimensions(warped, XINDEX=xindex, YINDEX=yindex, TRUEINDEX=trueindex)
   IF trueindex GT 0 THEN warped = Transpose(warped, [trueindex, xindex, yindex])
   Write_PNG, imageFilename, warped
   
   ; Write the KML file.
   kmlFile = Obj_New('cgKML_File', filename)
   overlay = Obj_New('cgKML_GroundOverlay', $
     HREF=imageFilename, $
     DESCRIPTION=description, $
     LATLONBOX=latlonBox, $
     ORDER=order)
   kmlFile -> Add, overlay
   kmlFile -> Save
   kmlFile -> Destroy
   
END