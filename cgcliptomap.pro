; docformat = 'rst'
;
; NAME:
;   cgClipToMap
;
; PURPOSE:
;   Allows an image or geoTiff file to be clipped or subset to a map projected boundary.
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
; Allows an image or geoTiff file to be clipped or subset to a map projected boundary.
;
; :Categories:
;    Map Utilities
;    
; :Returns:
;    The clipped or subsetted image is returned.
;    
; :Params:
;    imageIn: in, required, type=varies
;        Either a 2D or true-color image (in which, in both cases, a map coordinate object must be provided with the
;        MAP keyword) or the name of the GeoTiff file from which an image and a map coordinate object
;        can be obtained.
;    boundary: in, required, type=fltarr
;        A four-element array containing the map boundary to which the image should be clipped.
;       
; :Keywords:
;     latlonbox: out, optional, type=fltarr
;         A four-element array representing the boundary of the output image in
;         the Google Map preferred form of [north, south, east, west] in decimal
;         degrees.
;     map: in, required, type=object
;         A map coordinate object (cgMap) that maps or georeferences the input image.
;     outboundary: out, optional, type=fltarr
;         A four-element array containing the final map boundary of the clipped image.
;         The boundary will be in XY coordinates (projected meters).
;     outmap: out, optional, type=object
;         An output map coordinate object (cgMap) that describes the output image.
;     outposition: out, optional, type=intarr
;         A four-element array containing the pixel locations of the output image
;         in the input image pixel coordinate system: [x0,y0,x1,y1]. In other words,
;         these are the values used to subset the input image.
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
;        Written, 16 August 2012. DWF. 
;        If the absolute value of the maximum of the boundary is LE 360, assume you need to convert
;           from lat/lon space to projected meter space. 23 Aug 2012. DWF.
;        Added MAPOUT and LATLONBOX keywords. 1 Nov 2012. DWF.
;        Added OUTPOSITION keywords. 29 Nov 2012. DWF.
;        I have reason to believe the way I was creating the location vectors and
;           and image subset in this program was causing me to be 1 pixel off in
;           creating the image subset. The algorithm has been tweaked to correct this. 12 Dec 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgCliptoMap, imageIn, boundary, $
    LATLONBOX=latlonbox, $
    MAP=map, $
    OUTBOUNDARY=outboundary, $
    OUTMAP=outmap, $
    OUTPOSITION=outposition

   Compile_Opt idl2
   
   ; Return to caller if there is an error.
   On_Error, 2
   
   IF N_Elements(imageIn) EQ 0 THEN Message, 'Calling Syntax: clippedImage = cgClipToMap(image, boundary, MAP=map)'
   
   ; Is the imageIn variable a string. If so, it could be the
   ; have of a GeoTiff file for which you can gather the map
   ; projection information.
   IF Size(imageIn, /TNAME) EQ 'STRING' THEN BEGIN
      map = cgGeoMap(imageIn, Image=image, GeoTiff=geotiff, Palette=pal)
   ENDIF ELSE image = imageIn
   
   ; You need a boundary at this point.
   IF N_Elements(boundary) EQ 0 THEN Message, 'A clipping boundary is required.'
   IF N_Elements(boundary) NE 4 THEN Message, 'A clipping boundary must be a four-element array.'
   
   ; You MUST have a map coordinate object at this point.
   IF N_Elements(map) EQ 0 THEN Message, 'A map coordinate object is required.'
   
   ; If the absoulte value of the maximum of the boundary is less that 360, then assume the boundary
   ; is given in lat/lon space and convert it.
   IF Abs(Max(boundary)) LE 360 THEN BEGIN
       xy = map -> Forward(boundary[[0,2]], boundary[[1,3]])
       thisBoundary = [xy[0,0], xy[1,0], xy[0,1], xy[1,1]]
   ENDIF ELSE thisBoundary = boundary
   
   ; Current dimension of the image.
   dims = Image_Dimensions(image, XSIZE=xsize, YSIZE=ysize, TRUEINDEX=trueindex, XINDEX=xindex, YINDEX=yindex)
   IF (trueindex NE -1) && (trueindex NE 2) THEN image = Transpose(image, [xindex, yindex, trueindex])
   
   ; Get the current image range.
   map -> GetProperty, XRange=xr, YRange=yr
   
   ; Create image vectors.
   xvec = Scale_Vector(DIndgen(dims[0]+1), xr[0], xr[1])
   yvec = Scale_Vector(DIndgen(dims[1]+1), yr[0], yr[1])
   
   ; Clip to get new image subscripts.
   xsubs = 0 > Value_Locate(xvec, [thisBoundary[0],thisBoundary[2]]) < (dims[0]-1)
   ysubs = 0 > Value_Locate(yvec, [thisBoundary[1],thisBoundary[3]]) < (dims[1]-1)
   
   ; Output boundary.
   outboundary = [ xvec[xsubs[0]], yvec[ysubs[0]], xvec[xsubs[1]], yvec[ysubs[1]] ]
   outposition = [ xsubs[0], ysubs[0], xsubs[1], ysubs[1] ]

   ; Clip the image.
   IF trueIndex NE -1 THEN BEGIN
      imageType = Size(image, /TYPE)
      subimage = Make_Array(xsubs[1]-xsubs[0], ysubs[1]-ysubs[0], 3, Type=imageType)
      subimage[*,*,0] = image[xsubs[0]:xsubs[1]-1, ysubs[0]:ysubs[1]-1, 0]
      subimage[*,*,1] = image[xsubs[0]:xsubs[1]-1, ysubs[0]:ysubs[1]-1, 1]
      subimage[*,*,2] = image[xsubs[0]:xsubs[1]-1, ysubs[0]:ysubs[1]-1, 2]   
   ENDIF ELSE subimage = image[xsubs[0]:xsubs[1]-1, ysubs[0]:ysubs[1]-1]
   
   ; Create an output map coordinate object.
   map -> GetProperty, MAP_PROJECTION=projection, ELLIPSOID=ellipsoid, ZONE=zone
   mapout = Obj_New('cgmap', projection, Ellipsoid=ellipsoid, ZONE=zone, $
      XRange=[xvec[xsubs[0]],xvec[xsubs[1]]], $
      YRange=[yvec[ysubs[0]],yvec[ysubs[1]]])
   mapOut -> GetProperty, LATLONBOX=latlonBox
   
   RETURN, subimage
   
END