; docformat = 'rst'
;
; NAME:
;   cgNCDFMap
;
; PURPOSE:
;   The purpose of this function is to translate map projection information
;   found in a netCDF file into a map coordinate object (cgMap) that can be 
;   used to georeference images with a map data coordinate system. The Coyote 
;   Library is required.
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
;+---------------------------------------------------------------------------------------
;   The purpose of this function is to translate map projection information
;   found in a netCDF file into a map coordinate object (cgMap) that can be 
;   used to georeference images with a map data coordinate system. The Coyote 
;   Library is required.
;
; :Categories:
;    Graphics, Map Projections  
;          
; :Examples:
;    To create a map coordinate from a netCDF file from NSIDC::
;       file = 'C:\IDL\data\netCDF\jun_2004_amsr_e_bt.nc'
;       mapCoord = cgNCDFMap(file, /Continents, /Grid)
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive 
;       Fort Collins, CO 80526 USA 
;       Phone: 970-221-0438 
;       E-mail: david@idlcoyote.com 
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; :File_comments:
;    It is not possible to have a one-to-one mapping between every netCDF file and a map projection
;    in IDL, since IDL has a limited number of map projections and datums available. And, even at that,
;    I have not implemented all of IDL's map projections or datums, only those that I thought were most
;    likely to be encountered in my own work. If you run into a netCDF file that does not work in this
;    code (either because of an error in the code or because it is not supported), please contact me.
;    I am interested in supporting as many netCDF files as possible and I will take pains to do so if
;    I know they are needed.

; :History:
;     Converted from old ncdf_Coord program in Catalyst Library to run with Coyote Graphics routines.
;        9 November 2011. David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;----------------------------------------------------------------------------------------
;
;+--------------------------------------------------------------------------
; :Description:
;   Finds the correct projected map unit from the file. Assumes meters.
;
; :Params:
;    varWithMap: in, required, type=string
;       The name of the variable in the netCDF file that contains map 
;       projection information.
;    fileObj: in, required, type=object
;       The netCDF file object (ncdf_file) which can parse the netCDF file.
;       
; :Keywords:
;     silent: in, optional, type=boolean, default=0
;        If set, do not report errors.
;---------------------------------------------------------------------------
FUNCTION cgNCDFMap_GetMapUnit, varWithMap, fileObj, SILENT=silent

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        IF ~Keyword_Set(silent) THEN void = Error_Message()
        RETURN, mapUnit
    ENDIF
    
    ; Assume meters.
    mapUnit = 1.

    ; What kind of units are being used by the variable with the map projection?
    IF fileObj -> HasVarAttr(varWithMap, 'units') THEN BEGIN
        units = fileObj -> GetVarAttrValue(varWithMap, 'units')
        CASE StrLowCase(units) OF
            'm': mapUnit = 1.
            'metre': mapUnit = 1.
            'meter': mapUnit = 1.
            'meters': mapUnit = 1.
            'km': mapUnit = 1000.
            'kilometer': mapUnit = 1000.
            'kilometers': mapUnit = 1000.
            ELSE: Message, 'Unrecognized unit of measurement: ' + units
         ENDCASE
    ENDIF

    RETURN, mapUnit
END 


;+--------------------------------------------------------------------------
; :Description:
;   Finds the boundary (XRANGE and YRANGE) of the map coordinate space
;   and assigns these to the map object.
;
; :Params:
;    varWithMap: in, required, type=string
;       The name of the variable in the netCDF file that contains map 
;       projection information.
;    thisMapVar: in, required, type=string
;       The name of the map projection variable identified by the 
;       grid_mapping attribute of the varWithMap variable.
;    fileObj: in, required, type=object
;       The netCDF file object (ncdf_file) which can parse the netCDF file.
;    mapCoord: in, required, type=object
;       The map coordinate object we are creating. The XRange and YRange
;       of the discovered boundaries are assigned to this object.
;       
; :Keywords:
;     silent: in, optional, type=boolean, default=0
;        If set, do not report errors.
;     use_latlon: in, optional, type=boolean, default=0
;        If this keyword is set, the boundary ranges will be forced to be determined 
;        from the latitude and longitude arrays in the file. 
;     xrange: out, optional, type=double
;        The discovered X projected meter range of the map projection.
;     yrange: out, optional, type=double
;        The discovered Y projected meter range of the map projection.
;---------------------------------------------------------------------------
FUNCTION cgNCDFMap_FindBoundary, varWithMap, thisMapVar, fileObj, mapCoord, $
    SILENT=silent, XRANGE=xrange, YRANGE=yrange, USE_LATLON=use_latlon

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
        RETURN, success
    ENDIF
 
    ; Assume success
    success = 1
 
    IF ~Keyword_Set(use_latlon) THEN BEGIN
    
        ; Let's look for helpful values in the projection variable.
        IF fileObj -> HasVarAttr(thisMapVar, 'grid_boundary_top_projected_y') THEN BEGIN
            y1 = fileObj -> GetVarAttrValue(thisMapVar, 'grid_boundary_top_projected_y')
            mapUnit = cgNCDFMap_GetMapUnit(thisMapVar, fileObj, SILENT=silent)
            y1 = y1 / mapUnit
        ENDIF
        IF fileObj -> HasVarAttr(thisMapVar, 'grid_boundary_bottom_projected_y') THEN BEGIN
            y0 = fileObj -> GetVarAttrValue(thisMapVar, 'grid_boundary_bottom_projected_y')
            mapUnit = cgNCDFMap_GetMapUnit(thisMapVar, fileObj, SILENT=silent)
            y0 = y0 / mapUnit
        ENDIF
        IF fileObj -> HasVarAttr(thisMapVar, 'grid_boundary_left_projected_x') THEN BEGIN
            x0 = fileObj -> GetVarAttrValue(thisMapVar, 'grid_boundary_left_projected_x')
            mapUnit = cgNCDFMap_GetMapUnit(thisMapVar, fileObj, SILENT=silent)
            x0 = x0 / mapUnit
        ENDIF
        IF fileObj -> HasVarAttr(thisMapVar, 'grid_boundary_right_projected_x') THEN BEGIN
            x1 = fileObj -> GetVarAttrValue(thisMapVar, 'grid_boundary_right_projected_x')
            mapUnit = cgNCDFMap_GetMapUnit(thisMapVar, fileObj, SILENT=silent)
            x1 = x1 / mapUnit
        ENDIF
        IF (Keyword_Set(x0) + Keyword_Set(x1) + Keyword_Set(y0) + Keyword_Set(y1)) EQ 4 THEN BEGIN
            xrange = [x0,x1]
            yrange = [y0,y1]
            mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
            RETURN, success
        ENDIF ELSE Undefine, x0, x1, y0, y1
        
    ENDIF
    
    ; Let's see if you can find a "latitude" and "longitude" variables.
    IF (fileObj -> HasVar('latitude', OBJECT=latVar)) AND $
        (fileObj -> HasVar('longitude', OBJECT=lonVar)) THEN BEGIN
        lats = latVar -> GetValue()
        lons = lonVar -> GetValue()
        s = Size(lats, /DIMENSION)
            
        ; Convert to XY coords
        xy = Map_Proj_Forward(-180.0 > lons < 180.0, -90.0 > lats < 90.0, $
            MAP_STRUCTURE=mapCoord->GetMapStructure())
        x = Reform(xy[0,*], s[0], s[1])
        y = Reform(xy[1,*], s[0], s[1])
            
        ygrid =  Reverse(Reform(y[0,*]))
        xgrid =  Reform(x[*,0])
        half_x_pixel = Abs(Median(xgrid - Shift(xgrid,1))) / 2.0
        half_y_pixel = Abs(Median(ygrid - Shift(ygrid,1))) / 2.0
        xrange = [xgrid[0]-half_x_pixel, xgrid[s[0]-1]+half_x_pixel]
        yrange = [ygrid[0]-half_y_pixel, ygrid[s[1]-1]+half_y_pixel]
        mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
        RETURN, success
    ENDIF
    
    ; Can we stop now?
    IF (N_Elements(xrange) NE 0) AND (N_Elements(yrange) NE 0) THEN BEGIN
        mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
        RETURN, success
    ENDIF
        
    ; Look in the data variable for the attribute "coordinates"
    IF fileObj -> HasVarAttr(varWithMap, 'coordinates', OBJECT=coordAttr) THEN BEGIN
        coordVars = coordAttr -> GetValue()
        
        ; This should be the name of two variables.
        parts = StrSplit(coordVars, /Extract)
        IF N_Elements(parts) NE 2 THEN Message, 'Coordinates attribute did not yield two variable names.'
        
        FOR j=0,1 DO BEGIN
            thisCoordVar = parts[j]
            IF fileObj -> HasVarAttr(thisCoordVar, 'standard_name') THEN BEGIN
                thisCoordName = fileObj -> GetVarAttrValue(thisCoordVar, 'standard_name')
            ENDIF ELSE thisCoordName = "No Standard Name"
            IF fileObj -> HasVarAttr(thisCoordVar, 'units') THEN BEGIN
                mapUnit = cgNCDFMap_GetMapUnit(thisCoordVar, fileObj, SILENT=silent)
            ENDIF
            
            ; Hopefully, this is a projection coordinate.
            CASE thisCoordName OF
                'projection_x_coordinate': BEGIN
                    xvec = fileObj -> GetVarData(thisCoordVar)
                    nvec = N_Elements(xvec)
                    spacing = Median(xvec - Shift(xvec, 1))
                    halfpixel = spacing/2.0
                    xrange = [xvec[0]-halfpixel, xvec[nvec-1]+halfpixel] / mapUnit
                    END
                'projection_y_coordinate': BEGIN
                    yvec = fileObj -> GetVarData(thisCoordVar)
                    nvec = N_Elements(yvec)
                    spacing = Median(yvec - Shift(yvec, 1))
                    halfpixel = spacing/2.0
                    yrange = [yvec[0]-halfpixel, yvec[nvec-1]+halfpixel] / mapUnit
                    END
                ELSE: Message, 'Do not recognize the coordinate name ' + thisCoordName + '.'
            ENDCASE
        
        ENDFOR
        
    ENDIF ELSE Return, 0 ; Didn't find anything.
        
END 


;+--------------------------------------------------------------------------
; :Description:
;   Finds the length of the semi-major and semi-minor axes of the
;   ellipsoid used in the netCDF file.
;
; :Params:
;    varWithMap: in, required, type=string
;       The name of the variable in the netCDF file that contains map 
;       projection information.
;    thisMapVar: in, required, type=string
;       The name of the map projection variable identified by the 
;       grid_mapping attribute of the varWithMap variable.
;    fileObj: in, required, type=object
;       The netCDF file object (ncdf_file) which can parse the netCDF file.
;       
; :Keywords:
;     silent: in, optional, type=boolean, default=0
;        If set, do not report errors.
;     semimajor_axis: out, optional, type=double
;        The length of the semi-major axis.
;     semiminor_axis: out, optional, type=double
;        The length of the semi-minor axis.
;---------------------------------------------------------------------------
FUNCTION cgNCDFMap_EllipseAxes, varWithMap, thisMapVar, fileObj, SILENT=silent, $
        SEMIMAJOR_AXIS=semimajor_axis, SEMIMINOR_AXIS=semiminor_axis
        
    ; varWithMap -- The name of the variable containing the grid_mapping attribute.
    ; thisMapVar -- The map projection variable identified by the grid_mapping attribute.
    ; fileObj --    The NCDF_FILE object for the file.
    ;
    ; SILENT --     If set, errors are handled silently.
    ; SEMIMAJOR_AXIS -- Output, the semi-major axis length in meters.
    ; SEMIMINOR_AXIS -- Output, the semi-major axis length in meters.

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
        RETURN, success
    ENDIF
 
    ; Assume success
    success = 1
    
    ; Convert whatever map units are associated with this variable
    ; into a mapUnit that I can use to divide lengths by to get meters.
    mapUnit = cgNCDFMap_GetMapUnit(varWithMap, fileObj, SILENT=silent)
    
    ; Look for an "earth_radius" attribute.
    IF fileObj -> HasVarAttr(thisMapVar, 'earth_radius') THEN BEGIN
        radius = fileObj -> GetVarAttrValue(thisMapVar, 'earth_radius')
        semimajor_axis = radius/mapUnit
        semiminor_axis = radius/mapUnit
    ENDIF 
    
    ; Are you done?
    IF (Keyword_Set(semimajor_axis) AND Keyword_Set(semiminor_axis)) EQ 2 THEN RETURN, 1
    
    ; Try looking for CF 1.4 compliant attributes "semi_major_axis" and "semi_minor_axis".
     IF fileObj -> HasVarAttr(thisMapVar, 'semi_major_axis') THEN BEGIN
         semimajor_axis = fileObj -> GetVarAttrValue(thisMapVar, 'semi_major_axis')
         semimajor_axis = semimajor_axis / mapUnit
     ENDIF
     IF fileObj -> HasVarAttr(thisMapVar, 'semi_minor_axis') THEN BEGIN
         semiminor_axis = fileObj -> GetVarAttrValue(thisMapVar, 'semi_minor_axis')
         semiminor_axis = semiminor_axis / mapUnit
     ENDIF ELSE BEGIN
         IF N_Elements(semimajor_axis) NE 0 THEN BEGIN
             IF fileObj -> HasVarAttr(thisMapVar, 'inverse_flattening') THEN BEGIN
                 f_inv = fileObj -> GetVarAttrValue(thisMapVar, 'inverse_flattening')
                 f = 1.0D/f_inv
                 semiminor_axis = semimajor_axis - ( f * semimajor_axis )
             ENDIF
             ; Perhaps this is a sphere.
             IF N_Elements(semiminor_axis) EQ 0 THEN semiminor_axis = semimajor_axis
         ENDIF
     ENDELSE
        
     ; Are you done?
     IF (Keyword_Set(semimajor_axis) AND Keyword_Set(semiminor_axis)) EQ 2 THEN RETURN, success

     ; Try looking for CF 1.4 non-compliant attributes.
     IF fileObj -> HasVarAttr(thisMapVar, 'semimajor_radius') THEN BEGIN
         semimajor_axis = fileObj -> GetVarAttrValue(thisMapVar, 'semimajor_radius')
     ENDIF
     IF fileObj -> HasVarAttr(thisMapVar, 'semiminor_radius') THEN BEGIN
         semiminor_axis = fileObj -> GetVarAttrValue(thisMapVar, 'semiminor_radius')
     ENDIF ELSE BEGIN
         IF N_Elements(semimajor_axis) NE 0 THEN BEGIN
             IF fileObj -> HasVarAttr(thisMapVar, 'inverse_flattening') THEN BEGIN
                 f_inv = fileObj -> GetVarAttrValue(thisMapVar, 'inverse_flattening')
                 f = 1.0D/f_inv
                 semiminor_axis = semimajor_axis - ( f * semimajor_axis )
             ENDIF
             ; Perhaps this is a sphere.
             IF N_Elements(semiminor_axis) EQ 0 THEN semiminor_axis = semimajor_axis
         ENDIF
     ENDELSE
     
     ; Are you done?
     IF (Keyword_Set(semimajor_axis) AND Keyword_Set(semiminor_axis)) EQ 2 THEN RETURN, success

     ; Maybe there is an ESRI Projection Engine String we can use.
     IF fileObj -> HasVarAttr(varWithMap, 'esri_pe_string') THEN BEGIN
        esri_str = fileObj -> GetVarAttrValue(varWithMap, 'esri_pe_string')
        pos = StrPos(esri_str, 'SPHEROID[')
        IF pos NE -1 THEN BEGIN
            substring = StrMid(esri_str, pos)
            closebracket = StrPos(substring, ']')
            spheroid = StrMid(substring, 9, closebracket-9)
            parts = StrSplit(spheroid, ',', /EXTRACT)
            semimajor_axis = Double(parts[1])
            IF mapUnit EQ 1000 THEN semimajor_axis = semimajor_axis / mapUnit
            f_inv = Double(parts[2])
            f = 1.0D/f_inv
            semiminor_axis = semimajor_axis - ( f * semimajor_axis )
        ENDIF
     ENDIF

     ; Did we find anything?
     IF (Keyword_Set(semimajor_axis)EQ 0) OR $
       (Keyword_Set(semiminor_axis) EQ 0) THEN BEGIN
       
       ; Hells bells! Let's just issue a warning and assume WGS-84. What else
       ; can we do!?
       Message, /INFORMATIONAL, 'Cannot find any sensible datum radius in file. Assuming WGS-84 values.'
       semimajor_axis = 6378137.0D
       semiminor_axis = 6356752.31414D
       RETURN, success
       
       
     ENDIF ELSE RETURN, success

END 



;+---------------------------------------------------------------------------------------
; :Description:
;   The purpose of this function is to translate map projection information
;   found in a netCDF file into a map coordinate object (cgMap) that can be 
;   used to georeference images with a map data coordinate system. The Coyote 
;   Library is required.
;
; :Params:
;    ncdf_filename: in, optional, type=string  
;       The name of a netCDF file containing map projection information from
;       which a cgMap object can be created.
;       
; :Keywords:
;     ccolor: in, optional, type=string, default='Charcoal'
;         The name of a color the map continents should be displayed with. The default
;         is "charcoal". Color names are those supported by cgColor.
;     continents: in, optional, type=boolean, default=0
;         If a cgMap object is made successfully, then setting this keyword
;         will add a cgMapContinents object to the cgMap object.   
;     gcolor: in, optional, type=string, default='Gray'
;         The name of a color the map grid should be displayed with. The default
;         is "gray". Color names are those supported by cgColor.
;     grid: in, optional, type=boolean, default=0
;         If a cgMap object is made successfully, then setting this keyword
;         will add a cgMapGrid object to the cgMap object.  
;     mcolor: in, optional, type=string, default='Black'
;          The name of a color the map should be displayed in. (Normally the map
;          border and map title are displayed in this color.)
;     onimage: in, optional, type=boolean, default=0
;          Set this keword if the map object is to get its position from the last
;          cgImage command issued.
;     silent: in, optional, type=boolean, default=0
;          IDL cannot map every GeoTiff image to a supported map projection or datum.
;          Normally, if the GeoTIFF image is unsupported, an error message is issued.
;          Setting this keyword will suppress such error messages. If you do this, you
;          MUST check the SUCCESS keyword to see if the program ran successfully. (Of
;          course, it is a good idea to check it in any case!)
;     success: out, optional, type=boolean, default=0
;          An output variable that will contain a 1 if the map coordinate object was
;          created successfully. Or to a 0 if it was not created successfully.
;     title: in, optional, type=string, default=""                      
;          The title of the map projection.
;     use_latlon: in, optional, type=boolean, default=0
;          If this keyword is set, the boundary ranges will be forced to be determined 
;          from the latitude and longitude arrays in the file. 
;     xrange: out, optional, type=double
;          The discovered X projected meter range of the map projection.
;     yrange: out, optional, type=double
;          The discovered Y projected meter range of the map projection.
;------------------------------------------------------------------------------------
FUNCTION cgNCDFMap, ncdf_filename, $
    GCOLOR=gcolor, $
    GRID=grid, $
    CONTINENTS=continents, $
    CCOLOR=ccolor, $
    MCOLOR=mcolor, $
    ONIMAGE=onimage, $
    SILENT=silent, $
    SUCCESS=success, $
    TITLE=title, $
    USE_LATLON=use_latlon, $
    XRANGE=xrange, $
    YRANGE=yrange

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, theError
        IF ~Keyword_Set(silent) THEN void = Error_Message()
        success = 0
        IF Obj_Valid(fileObj) THEN Obj_Destroy, fileObj
        RETURN, Obj_New()
    ENDIF

    ; Need a filename?
    IF N_Elements(ncdf_filename) EQ 0 THEN BEGIN
        ncdf_filename = Dialog_Pickfile(TITLE='Select netCDF File...', FILTER=['*.nc','*.ncdf'])
        IF ncdf_filename EQ "" THEN RETURN, Obj_New()
    ENDIF
    
    ; Set default values of keywords.
    SetDefaultValue, gcolor, 'gray'
    SetDefaultValue, ccolor, 'charcoal'
    SetDefaultValue, mcolor, 'black'
    
    ; Assume success
    success = 1
    
    ; Create a NCDF_FILE object for reading the data file.
    fileObj = Obj_New('NCDF_FILE', ncdf_filename, /NOCLUTTER)
    IF Obj_Valid(fileObj) EQ 0 THEN Message, 'Cannot create NCDF_FILE object for reading data file.'
    
    ; Get a list of the variables in the file. Check each list. The first
    ; one that has a "grid_mapping" attribute will be the map projection we
    ; use here.
    varNames = fileObj -> GetVarNames(COUNT=varCount)
    FOR j=0,varCount-1 DO BEGIN
        IF ~fileObj->HasVarAttr(varNames[j], 'grid_mapping') THEN Continue
        varWithMap = varNames[j]
        thisMapVar = fileObj -> GetVarAttrValue(varWithMap, 'grid_mapping')
        thisMapProj = fileObj -> GetVarAttrValue(thisMapVar, 'grid_mapping_name')
        IF thisMapProj EQ "" THEN Message, 'Cannot find map projection name in file.'
        BREAK
    ENDFOR
    IF N_Elements(varWithMap) EQ 0 THEN $
        Message, 'This netCDF file has no variables with map projection information.'

    ; Define map projection variables you will find in netCDF files.
    cf_std_names = [{name:'albers_conical_equal_area', possible:1, gctp:103}, $
                    {name:'azimuthal_equidistant', possible:1, gctp:112}, $
                    {name:'lambert_azimuthal_equal_area', possible:1, gctp:111}, $
                    {name:'lambert_conformal_conic', possible:1, gctp:104}, $
                    {name:'lambert_cylindrical_equal_area', possible:0, gctp:-1}, $
                    {name:'latitude-longitude', possible:0, gctp:-1}, $
                    {name:'mercator', possible:1, gctp:105}, $
                    {name:'orthographic', possible:1, gctp:114}, $
                    {name:'polar_stereographic', possible:1, gctp:106}, $
                    {name:'rotated_latitude_longitude', possible:0, gctp:-1}, $
                    {name:'stereographic', possible:1, gctp:110}, $
                    {name:'transverse _mercator', possible:1, gctp:109}, $
                    {name:'vertical_perspective', possible:0, gctp:-1}]
                                        
    ; Define the expected parameters for those netCDF projection types.
    cf_std_name_parameters = [ Ptr_New(['standard parallel', $ ; Albers Equal Area
                                      'longitude_of_central_meridian', $ 
                                      'latitude_of_projection_origin', $
                                      'false_easting', 'false_northing']), $
                             Ptr_New(['longitude_of_projection_origin', $ ; Azimuthal Equidistant
                                      'latitude_of_projection_origin', $
                                      'false_easting', 'false_northing']), $
                             Ptr_New(['longitude_of_projection_origin', $ ; Lambert Azimutal Equal Area
                                      'latitude_of_projection_origin', $
                                      'false_easting', 'false_northing']), $
                             Ptr_New(['standard parallel', $ ; Lambert Conformal
                                      'longitude_of_projection_origin', $
                                      'latitude_of_projection_origin', $
                                      'false_easting', 'false_northing']), $
                             Ptr_New(['longitude_of_central_meridian', $ ; Lambert Cylindrical Equal Area
                                      'standard parallel', $
                                      'scale_factor_at_projection_origin', $
                                      'false_easting', 'false_northing']), $
                             Ptr_New(), $ ; Latitude-Longitude
                             Ptr_New(['longitude_of_projection_origin', $ ; Mercator
                                      'standard parallel', $
                                      'latitude_of_true_scale', $
                                      'scale_factor_at_projection_origin', $
                                      'false_easting', 'false_northing']), $
                             Ptr_New(['longitude_of_projection_origin', $ ; Orthographic
                                      'latitude_of_projection_origin', $
                                      'false_easting', 'false_northing']), $
                             Ptr_New(['straight_vertical_longitude_from_pole', $ ; Polar Stereographic
                                      'longitude_of_projection_origin', $
                                      'latitude_of_projection_origin', $
                                      'standard parallel', $
                                      'latitude_of_true_scale', $
                                      'scale_factor_at_projection_origin', $
                                      'false_easting', 'false_northing']), $
                             Ptr_New(), $ ; Rotated Pole
                             Ptr_New(['longitude_of_projection_origin', $ ; Stereographic
                                      'latitude_of_projection_origin', $
                                      'scale_factor_at_projection_origin', $
                                      'false_easting', 'false_northing']), $
                             Ptr_New(['scale_factor_at_central_meridian', $ ; Transverse Mercator
                                      'longitude_of_central_meridian', $
                                      'latitude_of_central_meridian', $
                                      'false_easting', 'false_northing']), $
                             Ptr_New() ] ; Vertical Perspective
                                      
    ; Can you locate the map projection in the list of projections you know about?
    index = Where(cf_std_names.name EQ thisMapProj, count)
    IF count EQ 0 THEN Message, 'Cannot process map projection: ' + thisMapProj
    
    ; Is is possible to do this map projection in IDL?
    IF cf_std_names[index].possible EQ 0 THEN $
        Message, 'Cannot process map projection "' + thisMapProj + '" in IDL.'
        
    ; Can you find values for the ellipsoid?
    success = cgNCDFMap_EllipseAxes(varWithMap, thisMapVar, fileObj, SILENT=silent, $
        SEMIMAJOR_AXIS=semimajor_axis, SEMIMINOR_AXIS=semiminor_axis)
    IF ~success THEN Message, 'Cannot locate ellipsoid axes in file.'
                
    ; Get the attributes for this particular map projection.
    attrs = *(cf_std_name_parameters[index])[0]
    nAttrs = N_Elements(attrs)
    FOR j=0,nAttrs-1 DO BEGIN
        thisAttr = attrs[j]
        IF fileObj -> HasVarAttr(thisMapVar, thisAttr, OBJECT=attrObj) THEN BEGIN
            attrValue = attrObj -> GetValue()
        ENDIF ELSE Continue
        
        CASE thisAttr OF
        
            'longitude_of_central_meridian'  : center_longitude = attrValue
            'latitude_of_central_meridian'   : center_latitude = attrValue
            'false_easting'                  : false_easting = attrValue
            'false_northing'                 : false_northing = attrValue
            'longitude_of_central_meridian'  : center_longitude = attrValue
            'latitude_of_projection_origin'  : center_latitude = attrValue
            'longitude_of_projection_origin' : center_longitude = attrValue
            'latitude_of_true_scale': BEGIN
                CASE cf_std_names[index].name OF
                    'polar_stereographic': center_latitude = attrValue
                    ELSE: true_scale_latitude = attrValue
                ENDCASE
                END
            'straight_vertical_longitude_from_pole': center_longitude = attrValue
            'standard_parallel'              : BEGIN
                IF N_Elements(attrValue) EQ 2 THEN BEGIN
                    standard_par1 = attrValue[0]
                    standard_par2 = attrValue[1]
                ENDIF ELSE standard_par1 = attrValue
                END
             'scale_factor_at_projection_origin': BEGIN
                CASE cf_std_names[index].name OF
                    'transverse _mercator': mercator_scale=attrValue
                    'mercator': mercator_scale=attrValue
                    ELSE: Message, /Informational, 'Not sure what to do with the attribute ' + $
                                '"scale_factor_at_projection_origin".'
                ENDCASE
                END
             'scale_factor_at_central_meridian': BEGIN
                CASE cf_std_names[index].name OF
                    'transverse _mercator': mercator_scale=attrValue
                    'mercator': mercator_scale=attrValue
                    ELSE: Message, /Informational, 'Not sure what to do with the attribute ' + $
                                '"scale_factor_at_central_meridian".'
                ENDCASE
                END
        ENDCASE
    
     ENDFOR
        
     mapCoord = Obj_New('cgMap', cf_std_names[index].gctp, $
            CENTER_LATITUDE=center_latitude, $
            CENTER_LONGITUDE=center_longitude, $
            COLOR=mcolor, $
            SEMIMAJOR_AXIS=semimajor_axis, $
            SEMIMINOR_AXIS=semiminor_axis, $
            NAME=name, $
            MERCATOR_SCALE=mercator_scale, $
            ONIMAGE=Keyword_Set(onimage), $
            STANDARD_PAR1=standard_par1, $
            STANDARD_PAR2=standard_par2, $
            TITLE=title, $
            TRUE_SCALE_LATITUDE=true_scale_latitude, $
            FALSE_EASTING=false_easting, $
            FALSE_NORTHING=false_northing)
            
      ; Now, can we find the extend of the XY Cartesian boundaries to set the range
      ; of the coordinate object.
      success = cgNCDFMap_FindBoundary(varWithMap, thisMapVar, fileObj, mapCoord, $
            SILENT=silent, USE_LATLON=use_latlon, XRANGE=xrange, YRANGE=yrange)
         
      IF ~success THEN Message, 'Cannot find XY boundary values in file.'

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
      
      ; Destroy the file object you created.
      Obj_Destroy, fileObj
          
      RETURN, mapCoord                 
END