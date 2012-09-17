; docformat = 'rst'
;
; NAME:
;   cgGeoMosaic
;
; PURPOSE:
;   Creates a mosaic or combination image, given the names of two GeoTiff image files.
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
;     histomatch: in, optional, type=boolean, default=0
;         Set this keyword to use the histogram of the first image to adjust
;         the image values of the second image before creating the mosaic.
;     imageout: out, optional, type=varies
;         The final image mosaic.
;     mapout: out, optional, type=object
;         A map coordinate object (cgMap) that geolocates the new image mosaic.
;     missing: in, optional, type=varies
;         The missing value in the input images. Missing values are set to zero
;         to create the mosaic. This assumes that valid values in the images are
;         not zero, which may be a weakness in the current algorithm.
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
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgGeoMosaic, geofile_1, geofile_2, $
   FILENAME=filename, $
   HISTOMATCH=histomatch, $
   IMAGEOUT=newImage, $
   MAPOUT=mapout, $
   MISSING=missing, $
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
    
    IF N_Elements(missing) EQ 0 THEN missing = 0B
    
    ; Create map coordinate objects for the two files. Missing values should
    ; be set to 0 for the algorithm to work correctly.
    m1 = cgGeoMap(geofile_1, IMAGE=image_1, PALETTE=palette_1, SUCCESS=success)
    IF ~success THEN RETURN, ""
    missingIndices_1 = Where(image_1 EQ missing, missingCnt_1)
    IF missingCnt_1 GT 0 THEN image_1[missingIndices_1] = 0
    m2 = cgGeoMap(geofile_2, IMAGE=image_2, PALETTE=palette_2, SUCCESS=success)
    IF ~success THEN RETURN, ""
    missingIndices_2 = Where(image_2 EQ missing, missingCnt_2)
    IF missingCnt_2 GT 0 THEN image_2[missingIndices_2] = 0

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
          
    ; If the projected coordinate system is not the same, we have problems.
    IF geo_1.PROJECTEDCSTYPEGEOKEY NE geo_2.PROJECTEDCSTYPEGEOKEY THEN BEGIN
        Print, 'File 1: ', geofile_1
        Print, 'File 2: ', geofile_2
        Message, 'The projected coordinate systems of the two files are not the same: ' + $
          StrTrim(geo_1.PROJECTEDCSTYPEGEOKEY, 2) + ' and ' + StrTrim(geo_2.PROJECTEDCSTYPEGEOKEY,2)
    ENDIF
              
    ; The scales can be off by a little. We will use the smaller of the two
    ; to make sure we can accommodate both images in the mosaic.
    xscale_1 = geo_1.ModelPixelScaleTag[0]
    yscale_1 = geo_1.ModelPixelScaleTag[1]
    xscale_2 = geo_2.ModelPixelScaleTag[0]
    yscale_2 = geo_2.ModelPixelScaleTag[1]
    xscale = xscale_1 < xscale_2
    yscale = yscale_1 < yscale_2
    
    ; Get the image map ranges and create a mosaic image of the proper size.
    m1 -> GetProperty, XRange=xr_1, YRange=yr_1
    m2 -> GetProperty, XRange=xr_2, YRange=yr_2
    xr = [xr_1[0] < xr_2[0], xr_1[1] > xr_2[1]]
    yr = [yr_1[0] < yr_2[0], yr_1[1] > yr_2[1]]
    
    ; How many pixels are in the new image?
    xNumPixels = Ceil((xr[1] - xr[0]) / xscale)
    yNumPixels = Ceil((yr[1] - yr[0]) / yscale)
    
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
       newImage = Make_Array( xnumPixels, ynumPixels, 3, TYPE=imageType, VALUE=0)
    ENDIF ELSE BEGIN
       newImage = Make_Array( xnumPixels, ynumPixels, TYPE=imageType, VALUE=0)
    ENDELSE
    
    ; Create image extent vectors.
    xvec = Scale_Vector(DIndgen(xnumPixels), xr[0], xr[1])
    yvec = Scale_Vector(DIndgen(ynumPixels), yr[0], yr[1])

    ; Match histograms?
    IF Keyword_Set(histomatch) THEN image_2 = Histomatch(Temporary(image_2), image_1)
    
    IF trueIndex NE -1 THEN BEGIN
       FOR j=0,2 DO BEGIN
           temp1 = image_1[*,*,j]
           xloc = 0 > Value_Locate(xvec, [xr_1[0], xr_1[1]])
           yloc = 0 > Value_Locate(yvec, [yr_1[0], yr_1[1]])
           xsize = xloc[1]-xloc[0]+1
           ysize = yloc[1]-yloc[0]+1 
           newImage[xloc[0]:xloc[1], yloc[0]:yloc[1], j] = Congrid(temp1, xsize, ysize)
           Undefine, temp1
           temp2 = image_2[*,*,j]
           xloc = 0 > Value_Locate(xvec, [xr_2[0], xr_2[1]])
           yloc = 0 > Value_Locate(yvec, [yr_2[0], yr_2[1]]) 
           xsize = xloc[1]-xloc[0]+1
           ysize = yloc[1]-yloc[0]+1 
           temp = Make_Array(xnumPixels, ynumPixels, TYPE=imageType, VALUE=0)
           temp[xloc[0]:xloc[1], yloc[0]:yloc[1]] = Congrid(temp2, xsize, ysize)
           overlap = Where((temp NE 0) AND (temp2 NE 0), count)
           temp[overlap] = Byte(Float(temp[overlap])*0.5 + Float(temp2[overlap])*0.5)
           zeros = Where(temp EQ 0)
           temp[zeros] = temps2[zeros]
           temp2 = Temporary(temp)
           newImage[*,*,j] = Temporary(temp2)
       ENDFOR
    ENDIF ELSE BEGIN
        xloc = 0 > Value_Locate(xvec, [xr_1[0], xr_1[1]])
        yloc = 0 > Value_Locate(yvec, [yr_1[0], yr_1[1]])
        xsize = xloc[1]-xloc[0]+1
        ysize = yloc[1]-yloc[0]+1 
        newImage[xloc[0]:xloc[1], yloc[0]:yloc[1]] = Congrid(image_1, xsize, ysize)
        xloc = 0 > Value_Locate(xvec, [xr_2[0], xr_2[1]])
        yloc = 0 > Value_Locate(yvec, [yr_2[0], yr_2[1]]) 
        xsize = xloc[1]-xloc[0]+1
        ysize = yloc[1]-yloc[0]+1 
        temp = Make_Array(xnumPixels, ynumPixels, TYPE=imageType, VALUE=0)
        temp[xloc[0]:xloc[1], yloc[0]:yloc[1]] = Congrid(image_2, xsize, ysize)
        overlap = Where((temp NE 0) AND (newimage NE 0), count)
        temp[overlap] = Byte(Float(temp[overlap])*0.5 + Float(newimage[overlap])*0.5)
        zeros = Where(temp EQ 0)
        temp[zeros] = newimage[zeros]
        newImage = Temporary(temp)
    ENDELSE
    
    ; Set all zero values in the new image to the missing value. This
    ; can only really be done for 2D image. In 3D images, missing values
    ; will be black [0,0,0].
    IF Size(newImage, /N_DIMENSIONS) EQ 2 THEN BEGIN
        missingIndices = Where(newImage EQ 0, zeroCnt)
        IF zeroCnt GT 0 THEN newImage[missingIndices] = missing
    ENDIF
    
    ; Clean up objects created in the program.
    Obj_Destroy, m1
    Obj_Destroy, m2
    
    ; Need a GeoTiff filename?
    IF N_Elements(filename) EQ 0 THEN BEGIN
        filename = cgPickfile(FILE='mosaic.tif')
        IF filename EQ "" THEN RETURN, ""
    ENDIF    
    
    ; Update the GeoTiff structure for the new file and write the file.
    newGeo = geo_1
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