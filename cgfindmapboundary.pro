; docformat = 'rst'
;
; NAME:
;   cgFINDMAPBOUNDARY
;
; PURPOSE:
; 
; Utility routine to find the map projection grid boundary from a file,
; if it is possible to do so. Currently works with GeoTIFF files, CF 1.4
; compliant netCDF files, and GPD files created with the GPD_Viewer software
; from the Catatlyst Library.
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
; Utility routine to find the map projection grid boundary from a file,
; if it is possible to do so. Currently works with GeoTIFF files, CF 1.4
; compliant netCDF files, and GPD files created with the GPD_Viewer software
; from the Catatlyst Library.
;
; :Categories:
;    Utility
; 
; :Returns:
;    The return value is either a 1, indicating a boundary can be found, or a 0 indicating
;    that the boundary could not be found.
;    
; :Params:
;    filename: in, required, type='string'
;       The name of a filename to open to see if a projected map grid boundary can be found.
;    boundary: out, optional, type=float
;       The boundary of the image in projected meter space, in the form [x0,y0,x1,y1].
;       
; :Keywords:
;    use_latlon:  in, optional, type=boolean, default=0
;        If the filename is a netCDF file, set this keyword to force the boundary 
;        to be determined by reading the include latitude/longitude arrays.
;    utm_south: in, optional, type=boolean, default=0
;        Set this keyword to add 10e6 to each of the Y values. This is sometimes
;        necessary with LandSat images in the Southern hemisphere, where the Y values
;        are given in negative values to indicate southern UTM zones.
;    xrange: out, optional, type=float
;        A two element vector: boundary[[0,2]]
;    yrange: out, optional, type=float
;        A two element vector: boundary[[1,3]]
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
;    Written by: David W. Fanning, 23 February 2010.::
;       Added XRANGE and YRANGE keywords. 25 February 2010. DWF
;       Added USE_LATLON keyword for finding boundary of netCDF files containing latitude
;          and longitude arrays. 6 April 2010. DWF.
;       Added UTM_SOUTH keyword to handle Landsat dat in UTM projections in GeoTiff files
;          that have to have 10e6 added to Y values to make them work in IDL. 14 Aug 2012. DWF.
;       Renamed cgFindMapBoundary from FindMapBoundary. 21 Aug 2012. DWF.
;       Changed reference to NCDF_Coord to cgNCDFMap. 28 Oct 2012. DWF.
;          
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgFindMapBoundary, filename, boundary, $
    USE_LATLON=use_latlon, $
    UTM_SOUTH=utm_south, $
    XRANGE=xrange, $
    YRANGE=yrange

    COMPILE_OPT idl2

    On_Error, 2
        
    ; Is this a GeoTIFF file:
    isGeoTiff = Query_Tiff(filename, geoInfo, GEOTIFF=geoStruct)

    ; If it is a GeoTiff file, do this.
    IF isGeoTiff THEN BEGIN
        
        xscale = geoStruct.ModelPixelScaleTag[0]
        yscale = geoStruct.ModelPixelScaleTag[1]
        tp = geoStruct.ModelTiePointTag[3:4]
        
        dims = geoInfo.dimensions
        xsize = dims[0]
        ysize = dims[1]
        
        xOrigin = tp[0]
        yOrigin = tp[1] - (yscale * ysize)
        xEnd = xOrigin + (xscale * xsize)
        yEnd = tp[1]
        
        ; Some UTM projections indicate southern hemisphere scenes with negative
        ; values for the lats. These have to have 10e6 added to them to work in IDL.
        IF Keyword_Set(UTM_SOUTH) THEN BEGIN
           yOrigin = yOrigin + 10e6
           yEnd = yEnd + 10e6
        ENDIF
        
        boundary = [xOrigin, yOrigin, xEnd, yEnd]
        xrange = boundary[[0,2]]
        yrange = boundary[[1,3]]
        RETURN, 1
        
    ENDIF 
    
    ; Is this an netCDF file?
    isNCDF = NCDF_IsValidFile(filename)
    
    IF isNCDF THEN BEGIN
        mapCoord = cgNCDFMap(filename, XRANGE=xrange, YRANGE=yrange, $
            SUCCESS=success, /SILENT, USE_LATLON=use_latlon)
        IF success THEN BEGIN
            boundary = [xrange[0], yrange[0], xrange[1], yrange[1]]
            xrange = boundary[[0,2]]
            yrange = boundary[[1,3]]
            RETURN, 1
        ENDIF
    ENDIF
    
    ; Maybe it is a GPD file produced from the GPD_Viewer.
    ; If so, compute the limits from the file.
    root_name = cgRootName(filename, EXTENSION=ext)
    IF StrLowCase(ext) NE 'gpd' THEN $
        Message, 'Do not recognize the file as one for which a ' + $
                 'grid boundary can be computed.'
                     
    ; Read the file.
    lines = File_Lines(filename)
    OpenR, lun, filename, /Get_Lun
    data = StrArr(lines)
    ReadF, lun, data
    Free_Lun, lun
    upData = StrUpCase(data)
        
    ; Can you find the origins of the upper left corner and the grid
    ; height and width?
    loc = StrPos(upData, 'MAP ORIGIN X:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = data[index[0]]
        x0 = 0.0D
        ReadS, thisLine, x0, FORMAT ='(30x, D0)'
    ENDIF
    loc = StrPos(upData, 'GRID UPPER-LEFT X:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = data[index[0]]
        x0 = 0.0D
        ReadS, thisLine, x0, FORMAT ='(30x, D0)'
    ENDIF
    loc = StrPos(upData, 'MAP ORIGIN Y:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = data[index[0]]
        y1 = 0.0D
        ReadS, thisLine, y1, FORMAT ='(30x, D0)'
    ENDIF
    loc = StrPos(upData, 'GRID UPPER-LEFT Y:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = data[index[0]]
        y1 = 0.0D
        ReadS, thisLine, y1, FORMAT ='(30x, D0)'
    ENDIF
    loc = StrPos(upData, 'GRID MAP UNITS PER CELL:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = data[index[0]]
        mapunits = 0.0D
        ReadS, thisLine, mapunits, FORMAT ='(30x, F0)'
    ENDIF
    loc = StrPos(upData, 'MAP SCALE:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = data[index[0]]
        mapscale = 0.0D
        ReadS, thisLine, mapscale, FORMAT ='(30x, F0)'
    ENDIF
    loc = StrPos(upData, 'GRID WIDTH:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = data[index[0]]
        cols = 0L
        ReadS, thisLine, cols, FORMAT ='(30x, I0)'
    ENDIF
    loc = StrPos(upData, 'GRID HEIGHT:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
    thisLine = data[index[0]]
    rows = 0L
    ReadS, thisLine, rows, FORMAT ='(30x, I0)'
    ENDIF
    IF (N_Elements(x0) NE 0) AND $
       (N_Elements(y1) NE 0) AND $
       (N_Elements(mapunits) NE 0) AND $
       (N_Elements(mapscale) NE 0) AND $
       (N_Elements(cols) NE 0) AND $
       (N_Elements(rows) NE 0) THEN BEGIN
   
        x1 = x0 + cols*mapunits*mapscale
        y0 = y1 - rows*mapunits*mapscale

        boundary = [x0, y0, x1, y1]
        xrange = boundary[[0,2]]
        yrange = boundary[[1,3]]
        RETURN, 1
    ENDIF 

    ; Can't find boundary, report no success.
    RETURN, 0

END
