; docformat = 'rst'
;
; NAME:
;   cgGeoMosaic
;
; PURPOSE:
;   Creates a mosaic or combination image, given the names of two GeoTiff image files.
;   The images must be 2D images.
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
; Creates a mosaic or combination image, given the names of two GeoTiff image files.
; The images must be 2D images.
; 
; :Categories:
;    Map Utilities
;    
; :Returns:
;    The name of a GeoTiff image file containing the combined image mosaic.
;    
; :Params:
;    geofile_1: in, required, type=string
;        The name of the first GeoTiff file to be combined into a mosaic.
;    geofile_2: in, required, type=string
;        The name of the second GeoTiff file to be combined into a mosaic.
;       
; :Keywords:
;     filename: in, optional, type=string
;         The name of the output GeoTiff file. If not provided, the user will
;         be asked to create a name. If provided, the user will not be prompted
;         and this name will be used.
;     imageout: out, optional, type=varies
;         The final image mosaic.
;     mapout: out, optional, type=object
;         A map coordinate object (cgMap) that geolocates the new image mosaic.
;     missing: in, optional, type=varies
;         The missing value in the input images. If scalar value, the same value is used
;         for both images, but may be a two-element array. 
;     miss_out, in, optional, type=varies
;         The missing value of the output image. If undefined, missing[0] is used. If
;         missing[0] is undefined, the value 0B is used.
;     success: out, optional, type=boolean
;         This keyword is set to 1 if the function completed successfully. And to
;         0 otherwise.
;                
; :Author:
;     FANNING SOFTWARE CONSULTING::
;         David W. Fanning 
;         1645 Sheely Drive
;         Fort Collins, CO 80526 USA
;         Phone: 970-221-0438
;         E-mail: david@idlcoyote.com
;         Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 18 August 2012. DWF. 
;        Added SUCCESS keyword 4 September 2012. DWF.
;        Now blending overlap regions using 50% of pixel values from the two images. 14 Sept 2012. DWF.
;        Revamp of algorithm's handing of missing values.  Added MISS_OUT keyword and removed HISTOMATCH
;           keyword because it only works for BYTE data. Restricted mosaicking to 2D images. 29 Jan 2013. DWF.
;
; :Copyright:
;     Copyright (c) 2012-2013, Fanning Software Consulting, Inc.
;-
FUNCTION cgGeoMosaic, geofile_1, geofile_2, $
   FILENAME=filename, $
   IMAGEOUT=newImage, $
   MAPOUT=mapout, $
   MISSING=missing, $
   MISS_OUT=miss_out, $
   SUCCESS=success

    Compile_Opt idl2
    
    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF Obj_Valid(m1) THEN Obj_Destroy, m1
        IF Obj_Valid(m2) THEN Obj_Destroy, m2
        success = 0
        RETURN, ""
    ENDIF
    
    ; Need two files to do the job!
    IF N_Params() NE 2 THEN BEGIN
        void = Dialog_Message('Calling Syntax: geofile = cgGeoMosaic( geofile_1, geofile_2 )')
        RETURN, ""
    ENDIF
    
    ; Assume success.
    success = 1
    
    ; Make sure the missing values are in two-element array, if needed.
    IF N_Elements(missing) NE 0 THEN BEGIN
       IF N_Elements(missing) EQ 1 THEN missing = [missing, missing]
    ENDIF
    IF N_Elements(miss_out) EQ 0 THEN BEGIN
       IF N_Elements(missing) NE 0 THEN miss_out = missing[0] ELSE miss_out = 0B
    ENDIF
    
    ; Let's read the images and get the bounaries of the map projection.
    m1 = cgGeoMap(geofile_1, IMAGE=image_1, PALETTE=pal_1, BOUNDARY=b1, $
        GEOTIFF=geo1, SUCCESS=success)
    IF ~success THEN RETURN, ""
    m2 = cgGeoMap(geofile_2, IMAGE=image_2, PALETTE=pal_2, BOUNDARY=b2, $
        GEOTIFF=geo2, SUCCESS=success)
    IF ~success THEN RETURN, ""
    
    ; Check to be sure projection and ellipsoid are the same.
    m1 -> GetProperty, Map_Projection=projection_1, Ellipsoid=ellipsoid_1
    m2 -> GetProperty, Map_Projection=projection_2, Ellipsoid=ellipsoid_2
    IF projection_1 NE projection_2 THEN $
       Message, 'The map projections of the two files are not the same.'
    IF ellipsoid_1 NE ellipsoid_2 THEN $
       Message, 'The map projection ellipsoids of the two files are not the same.'
    
    ; If the projected coordinate system is not the same, we have problems.
    IF geo1.PROJECTEDCSTYPEGEOKEY NE geo2.PROJECTEDCSTYPEGEOKEY THEN BEGIN
        Print, 'File 1: ', geofile_1
        Print, 'File 2: ', geofile_2
        Message, 'The projected coordinate systems of the two files are not the same: ' + $
          StrTrim(geo1.PROJECTEDCSTYPEGEOKEY, 2) + ' and ' + StrTrim(geo2.PROJECTEDCSTYPEGEOKEY,2)
    ENDIF
    
    ; Match histograms? Taken out because it only works for Byte images currently.
    ;IF Keyword_Set(histomatch) THEN image_2 = Histomatch(Temporary(image_2), image_1)
         
    ; The scales can be off by a little. We will use the smaller of the two
    ; to make sure we can accommodate both images in the mosaic.
    xscale_1 = geo1.ModelPixelScaleTag[0]
    yscale_1 = geo1.ModelPixelScaleTag[1]
    xscale_2 = geo2.ModelPixelScaleTag[0]
    yscale_2 = geo2.ModelPixelScaleTag[1]
    xscale = xscale_1 < xscale_2
    yscale = yscale_1 < yscale_2
    
    ; Check for missing values.
    IF N_Elements(missing) NE 0 THEN BEGIN
      missingIndices_1 = Where(image_1 EQ missing[0], missingCnt_1)
      IF missingCnt_1 GT 0 THEN image_1[missingIndices_1] = miss_out
      missingIndices_2 = Where(image_2 EQ missing[1], missingCnt_2)
      IF missingCnt_2 GT 0 THEN image_2[missingIndices_2] = miss_out
    ENDIF

    ; Get the image map ranges and create a mosaic image of the proper size.
    m1 -> GetProperty, XRange=xr_1, YRange=yr_1
    m2 -> GetProperty, XRange=xr_2, YRange=yr_2
    xr = [xr_1[0] < xr_2[0], xr_1[1] > xr_2[1]]
    yr = [yr_1[0] < yr_2[0], yr_1[1] > yr_2[1]]
    
    ; How many pixels are in the new image?
    xNumPixels = Ceil((xr[1] - xr[0]) / xscale)
    yNumPixels = Ceil((yr[1] - yr[0]) / yscale)
    
    ; Create a new image of the proper size. 
    imageType = Size(image_1, /Type)
    newImage = Make_Array( xnumPixels, ynumPixels, TYPE=imageType, VALUE=miss_out)
    
    ; Make a copy of this image. This will be the final image.
    finalImage = newImage
    
    ; Create image extent vectors.
    xvec = cgScaleVector(DIndgen(xnumPixels), xr[0], xr[1])
    yvec = cgScaleVector(DIndgen(ynumPixels), yr[0], yr[1])
    
    ; Locate the position of the first image and position it in the final image.
    xloc = 0 > Value_Locate(xvec, [xr_1[0], xr_1[1]])
    yloc = 0 > Value_Locate(yvec, [yr_1[0], yr_1[1]])
    xsize = xloc[1]-xloc[0]+1
    ysize = yloc[1]-yloc[0]+1 
    finalImage[xloc[0]:xloc[1], yloc[0]:yloc[1]] = Congrid(image_1, xsize, ysize)
    
    ; Locate the position of the second image and position it in the new image.
    xloc = 0 > Value_Locate(xvec, [xr_2[0], xr_2[1]])
    yloc = 0 > Value_Locate(yvec, [yr_2[0], yr_2[1]]) 
    xsize = xloc[1]-xloc[0]+1
    ysize = yloc[1]-yloc[0]+1 
    newImage[xloc[0]:xloc[1], yloc[0]:yloc[1]] = Congrid(image_2, xsize, ysize)
    
    ; Where the final image is missing, and the new image is not missing, move
    ; newImage pixels over to final image.
    movingPixels = Where((finalImage EQ miss_out) AND (newimage NE miss_out), movingCount)
    
    ; If there are overlapping pixels, then take the average value.
    overlap =  Where((finalImage NE miss_out) AND (newimage NE miss_out), count)
    finalImage[overlap] = Convert_to_Type(Float(finalImage[overlap])*0.5 + Float(newImage[overlap])*0.5, Size(image_1, /Type))

    ; Move the pixels over.
    IF movingCount GT 0 THEN finalImage[movingPixels] = newImage[movingPixels]
    
    ; Cleanup
    Undefine, newImage
    newImage = Temporary(finalImage)
    
    ; Clean up objects created in the program.
    Obj_Destroy, m1
    Obj_Destroy, m2
    
    ; Need a GeoTiff filename?
    IF N_Elements(filename) EQ 0 THEN BEGIN
        filename = cgPickfile(FILE='mosaic.tif')
        IF filename EQ "" THEN RETURN, ""
    ENDIF    
    
    ; Update the GeoTiff structure for the new file and write the file.
    newGeo = geo1
    newGeo.ModelTiePointTag[3:4] = [xr[0], yr[1]]
    newGeo.ModelPixelScaleTag[0] = xscale
    newGeo.ModelPixelScaleTag[1] = yscale
    CASE imageType OF
        1: Write_Tiff, filename, Reverse(newimage, 2), GEOTIFF=newGeo, PLANARCONFIG=2
        2: Write_Tiff, filename, Reverse(newimage, 2), GEOTIFF=newGeo, PLANARCONFIG=2, /SHORT, /SIGNED
        3: Write_Tiff, filename, Reverse(newimage, 2), GEOTIFF=newGeo, PLANARCONFIG=2, /LONG, /SIGNED
        4: Write_Tiff, filename, Reverse(newimage, 2), GEOTIFF=newGeo, PLANARCONFIG=2, /FLOAT
        5: Write_Tiff, filename, Reverse(newimage, 2), GEOTIFF=newGeo, PLANARCONFIG=2, /DOUBLE
        6: Write_Tiff, filename, Reverse(newimage, 2), GEOTIFF=newGeo, PLANARCONFIG=2, /COMPLEX
        9: Write_Tiff, filename, Reverse(newimage, 2), GEOTIFF=newGeo, PLANARCONFIG=2, /DCOMPLEX
       12: Write_Tiff, filename, Reverse(newimage, 2), GEOTIFF=newGeo, PLANARCONFIG=2, /SHORT
       13: Write_Tiff, filename, Reverse(newimage, 2), GEOTIFF=newGeo, PLANARCONFIG=2, /LONG
       14: Write_Tiff, filename, Reverse(newimage, 2), GEOTIFF=newGeo, PLANARCONFIG=2, /L64, /SIGNED
       15: Write_Tiff, filename, Reverse(newimage, 2), GEOTIFF=newGeo, PLANARCONFIG=2, /L64
    ENDCASE
    
    ; Create a map object for the image, if one is needed.
    IF Arg_Present(mapOut) THEN mapOut = cgGeoMap(filename)
    
    ; Return the name of the GeoTiff file.
    RETURN, filename
    
END