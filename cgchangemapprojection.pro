; docformat = 'rst'
;
; NAME:
;   cgChangeMapProjection
;
; PURPOSE:
;   This function warps a map projected image from one map projection to another, using
;   Map_Proj_Image to do the warping.
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
; This function warps a map projected image from one map projection to another, using
; Map_Proj_Image to do the warping. Useful in general, it is used specifically to
; warp map projected images into the Equirectangular map projection with a WGS 84 ellipsoid
; that is used by Google Earth. See the program `cgImage2KML` to see how it is used.
;
; :Categories:
;    Map Projections, Utilities
;    
; :Returns:
;    An image warped into the output map projection is returned.
;    
; :Params:
;    image: in, required
;       Any 2D or true-color image with or without an alpha channel. This image will
;       be warped to the output map projection (`mapOut`).
;    mapin: in, required, type=object
;       A map coordinate object (cgMap) describing the map projection and coordinates of the
;       input image that is to be warped.  
;       
; :Keywords:
;    boundary: in, optional, type=dblarr
;       A four-element array specifying the Cartesian (XY) coordinates of the input
;       image range, in the form [xmin, ymin, xmax, ymax]. If this parameter is not
;       present, the boundary will be obtained from the `MapIn` map coordinate object.
;       On output, this variable will contain the Cartesian boundary of the warped image.
;    bilinear: in, optional, type=boolean, default=0
;       Set this keyword to warp the image with bilinear interpolation. The default is
;       to do nearest neighbor interpolation.
;    latlonbox: out, optional, type=fltarr
;       A four-element float array containing the boundaries of the warped image in
;       the [north, south, east, west] form preferred by Google Earth. Listed as degrees.
;    mapout: in, optional, type=object
;       A map coordinate object (cgMap) describing the map projection and coordinates of the
;       warped image. This image will be warped into this map projection. The [XY]Range of
;       this object will be set to the `XYRange` of the output image.  
;    mask: out, optional
;       Set this keyword equal to a named variable that will contain a byte array of the same 
;       dimensions as the output image, containing a mask of the “good” values. Values in the output  
;       image that were set to `Missing` (that is, the values were off the map) will have a mask value 
;       of 0, while all other mask values will be 1.
;    missing: in, optional, type=integer, default=0
;       Set this keyword equal to an integer value to be used for pixels that fall outside of 
;       the valid map coordinates. The default value is 0.
;    xyrange: out, optional, type=dblarr
;       The Cartesian (XY) coordinates associated with the output image. These are the map image
;       boundaries of the output image.
;
; :Examples:
;    To prepare a GeoTiff file for creating a KML overlay on Google Earth::
;       netObject = Obj_New('IDLnetURL')
;       url = 'http://www.idlcoyote.com/data/AF03sep15b.n16-VIg.tif'
;       returnName = netObject -> Get(URL=url, FILENAME=AF03sep15b.n16-VIg.tif')
;       Obj_Destroy, netObject
;       map = cgGeoMap('AF03sep15b.n16-VIg.tif')
;       googleMap = Obj_New('cgMap', 'Equirectangular', Ellipsoid='WGS 84')
;       warpedImage = cgChangeMapProjection(image, map, MAPOUT=googleMap)
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
;        Fixed a problem with a TRANSPOSE command for true-color images. Bad logic. 4 Jan 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgChangeMapProjection, image, mapIn, $
   BOUNDARY=boundary, $
   BILINEAR=bilinear, $
   LATLONBOX=latlonbox, $
   MAPOUT=mapOut, $
   MASK=mask, $
   MISSING=missing, $
   XYRANGE=xyrange
   
   Compile_Opt idl2
   
   ; Error handling.
   ON_Error, 2
   
   ; An image and an input map coordinate object are required.
   IF N_Elements(image) EQ 0 THEN Message, 'An input image is required.'
   IF N_Elements(mapIn) EQ 0 THEN Message, 'An input map coordinate object is required.'
   
   ; Assign default values.
   IF N_Elements(boundary) EQ 0 THEN mapIn -> GetProperty, BOUNDARY=boundary
   bilinear = Keyword_Set(bilinear)
   IF N_Elements(mapOut) EQ 0 THEN BEGIN
      mapOut = Obj_New('cgMap', 'Equirectangular', Ellipsoid='WGS 84')
   ENDIF
   
   ; Gather information about the input image. Do this differently if this is a 2D
   ; image as opposed to a true-color image.
   dims = Image_Dimensions(image, XIndex=xindex, YIndex=yindex, TRUEINDEX=trueindex, $
       XSize=xsize, YSize=ysize, ALPHACHANNEL=alphachannel)
   IF trueindex EQ -1 THEN BEGIN
      warped = Map_Proj_Image(image, boundary, Image_Structure=mapIn->GetMapStruct(), $
        Map_Structure=mapOut->GetMapStruct(), UVRANGE=xyrange, Missing=missing, MASK=mask)
      mapOut -> SetProperty, XRANGE=xyrange[[0,2]], YRANGE=xyrange[[1,3]]
      mapOut -> GetProperty, LATLONBOX=latlonbox, BOUNDARY=boundary
   ENDIF ELSE BEGIN
      IF trueIndex LT 2 THEN img = Transpose(image, [xindex, yindex, trueindex])
      frames = alphachannel ? 4 : 3
      warped = Make_Array(xsize, ysize, frames, TYPE=Size(image, /TYPE))
      warped[*,*,0] = Map_Proj_Image(img[*,*,0], boundary, Image_Structure=mapIn->GetMapStruct(), $
        Map_Structure=mapOut->GetMapStruct(), UVRANGE=xyrange, Missing=missing, MASK=mask, $
        XINDEX=xindices, YINDEX=yindices)
      mapOut -> SetProperty, XRANGE=xyrange[[0,2]], YRANGE=xyrange[[1,3]]
      mapOut -> GetProperty, LATLONBOX=latlonbox, BOUNDARY=boundary
      FOR j=1,frames-1 DO BEGIN
         warped[*,*,j] = Map_Proj_Image(img[*,*,j], XINDEX=xindices, YINDEX=yindices)
      ENDFOR
   
   ENDELSE
   
   RETURN, warped
   
END