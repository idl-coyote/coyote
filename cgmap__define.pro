; docformat = 'rst'
;
; PURPOSE:
;   Provides an easy way to set up a map projection coordinate space using GCTP map 
;   projections normally accessed via Map_Proj_Init.
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
;+--------------------------------------------------------------------------
;   Provides an easy way to set up a map projection coordinate space using GCTP map 
;   projections normally accessed via Map_Proj_Init. Allows an unlimited number of map
;   overlays, and can provide a fresh map structure on demand, eliminating the problem
;   of ephemerial map structures that plaqued Map_Proj_Init until IDL 8.x. This program
;   is basically a wrapper for Map_Proj_Init, with additional features that make it 
;   superiour for working with map projections in IDL.
;
; :Categories:
;    Graphics, Map Projections
;    
; :File_comments:
;     Only GCTP projections are allowed. If you wish to use projections normally 
;     set up with Map_Set, use the comparable cgMap_Set command. 
;
; :Author:
;    FANNING SOFTWARE CONSULTING::
;     David W. Fanning 
;     1645 Sheely Drive 
;     Fort Collins, CO 80526 USA 
;     Phone: 970-221-0438 
;     E-mail: david@idlcoyote.com 
;     Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Brought over to the Coyote Library from a similar routine in the Catalyst Library.
;           Updated to account for the bug that creates incorrect result in UTM projections
;           when using the WGS84 ellipsoid. The Wallbeck ellipsoid is substituted for the
;           WGS84 ellipsoid in this instance. David W. Fanning, 7 November 2011.
;        Added IS_CYLINDRICAL method to solve a problem with grid labeling and drawing
;           of grid lines. 16 Dec 2011. DWF.
;        Added ERASE method to erase the display. 28 Dec 2011. DWF.
;        Added check for identical range values in FORWARD method. 30 Dec 2011. DWF.
;        I am convinced that the map structure returned by Map_Proj_Init, when there
;           is a LIMIT used in the call contains a uv_box with incorrect latitude values.
;           This is important because other routines (e.g., cgMapGrid) depend on these values.
;           I've created a fix wherein I fixed the uv_box latitude values to correspond
;           to the LIMIT of the map projection. 6 April 2012. DWF.
;        Set the default CENTER_LATITUDE and CENTER_LONGITUDE to 0.0. 9 April 2012. DWF.
;        Added NOFORWARDFIX keyword to allow skipping of the "fix" in the FORWARD method,
;           as sometimes this is not needed or required. 29 June 2012. DWF.
;        Fixed a problem that required having to set the UTM zone in addition to the latitude
;           and longitude in a UTM projection. Now using cgUTMZone to determine the proper
;           zone. 8 Aug 2012. DWF.
;        Added a BOUNDARY keyword to the GetProperty method. 16 Aug 2012. DWF.
;        Modified to allow Hotine Oblique Mercator map projections to work correctly. 7 Sept 2012. DWF.
;        Additional changes to better handle IDL 8.2 map projections. 12 Sept 2012. DWF.
;        Added LATLONBOX keyword to the GetProperty method to allow me to obtain
;            the map boundary in the Google Map preferred notation of [north, south, east, west]
;            in degrees. 30 Oct 2012.
;        I was calculating the default X and Y range incorrectly for non-UTM map projections.
;            I have now gone back to my original method of using the UV_BOX of the map structure
;            to do this. However, there is still a problem with the UV_BOX when the center latitude
;            is not zero. I still attempt to fix this problem in the code (SetMapProjection method). 3 Jan 2012. DWF.
;        I added ASPECT and ISOTROPIC keywords to allow the setting of the aspect ratio of the map. 3 Jan 2012. DWF.
;        
; :Copyright:
;     Copyright (c) 2011-2013, Fanning Software Consulting, Inc.
;---------------------------------------------------------------------------
;
;+--------------------------------------------------------------------------
;   The initialization method for the cgMap object.
;
; :Params:
;    map_projection: in, optional, type=string/integer, default='Equirectangular'
;        The name or index number of the map projection desired. Passed directly
;        to Map_Proj_Init as the map projection value. Only GCTP projections are
;        allowed. If you wish to use projections normally set up with Map_Set, use
;        the comparable cgMap_Set command.
;       
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        If this keyword is set, the object is added to the resizeable graphics
;        window, cgWindow. The DRAW method of the object is called in cgWindow.
;     aspect: in, optional, type=float, default=none
;        Set this keyword to a floating point ratio that represents the aspect ratio 
;        (ysize/xsize) of the resulting map projection. Note that `Aspect` cannot be 
;        used when plotting with !P.MULTI. Consider using cgLayout instead for multiple
;        plots.
;     background: in, optional, type=string, default='white'
;        The name of the background color. Used only if the map object erases
;        the display when it draws its contents.
;     bcolor: optional, type=string, default='opposite'
;        The name of the color to draw box axes with. Requires BOX_AXES be set.
;     box_axes: in, optional, type=boolean, default=0
;        Set this keyword to draw a box-style grid axes around the map. Applies
;        only if creating a mapGrid object.
;     center_latitude: in, optional, type=float, default=0.0
;        The center latitude of the map projection.
;     center_longitude: in, optional, type=float, default=0.0
;        The center longitude of the map projection.
;     ccolor: in, optional, type=string, default='charcoal'
;        The name of the drawing color for the MapContinents object if this is requested.
;     color: in, optional, type=string, default='opposite'
;        The name of the drawing color for the object. Passed along to the mapGrid
;        and MapContinents object if these are requested.
;     continents: in, optional, type=boolean, default=0
;        Set this keyword if you wish to create an overlay object of continental outlines
;        that will be rendered when the draw method is called.
;     datum: in, optional, type=string/integer, default='Sphere'
;        This keyword is being depreciated in favor of the keyword ELLIPSOID,
;        corresponding to changes to Map_Proj_Init initiated in IDL 7.
;     draw: in, optional, type=boolean, default=0
;        Set this keyword if you wish to immediately call the DRAW method after the
;        object has been completely initialized.
;     ellipsoid: in, optional, type=string/integer
;        Set this to the name or index number of the ellopsoid or datum you wish to use
;        for the map projection. The value is passed directly to Map_Proj_Init. The
;        default is a sphere for those projections that only support a sphere, otherwise
;        a Clark projection is used to conform to Map_Proj_Init defaults.
;     fill: in, optional, type=boolean, default=0
;        Set this keyword to display filled continents, if the keyword CONTINENTS is set.
;     erase: in, optional, type=boolean, default=0
;        Set this keyword if you wish to have the object erase the current graphics display
;        before drawing its content in the DRAW method. The graphics display will be erased
;        in the background color.
;     gcolor: in, optional, type=string, default='gray'
;        The name of the drawing color for the MapGrid object if this is requested.
;     grid: in, optional, type=boolean, default=0
;        Set this keyword if you wish to create an overlay object of map grid lines
;        that will be rendered when the draw method is called.
;     hires: in, optional, type=boolean, default=0
;        Set this keyword if you wish to use high resolution continental outlines.
;        Passed to the MapContinents object if one is requested.
;     isotropic: in, optional, type=boolean, default=0
;        Set this keyword to set the `Aspect` keyword to a value that correctly represents
;        the same map unit length in both the X and Y directions. In other words, a map
;        unit measured in the X direction is the same physical length as a map unit measured
;        in the Y direction.
;     land_color: in, optional, type=string
;        The name of the drawing color for filled continents, if the keyword CONTINENTS 
;        is set. Passed directly to the cgMapContinents object.
;     latlon_ranges: in, optional, type=boolean, default=0
;        Normally the XRANGE and YRANGE keywords are set in terms of projected meters. If 
;        this keyword is set, then the values of XRANGE and YRANGE are assumed to be in longitude
;        and latitude values, respectively, and will be converted to projected meters prior to 
;        being stored in the object.
;     limit: in, optional, type=FltArr(4), default=none
;        The normal LIMIT keyword to Map_Proj_Init, specifying the limit of the map
;        projection in terms of latitude and longitude. Normally, `Limit` is used when using
;        Map_Proj_Init. Most work is done by specifying the projected XY rectangular
;        coordinate system with the keywords XRANGE and YRANGE.
;     lcolor: in, optional, type=string
;        Set this to the name of the label color to use in labeling grid lines.
;        By default, the same as COLOR, or if BOX_AXIS is set, then same as BCOLOR.
;     name: in, optional, type=string, default=selected by cgContainer.
;        Use this keyword to name the object. Names are often used to select objects in 
;        program code. 
;     noborder: in, optional, type=boolean, default=0
;        If this keyword is set, the customary border than surrounds the map projection is
;        not drawn.
;     noforwardfix: in, optional, type=boolean, default=0
;        There is, I believe, a bug in MAP_PROJ_FORWARD that renders longitude values 
;        incorrectly in projected meter space. This is evidenced by MAP_GRID not producing
;        the correct longitude lines in map coordinate systems set up in projected XY meters.
;        In the FORWARD method I correct for this. But, this correction is not always needed
;        or wanted. This property of the object allows me to turn that correction on or off,
;        as needed. Normally, the fix is provided, unless this keyword is set to 1.
;     onimage: in, optional, type=boolean, default=0
;        If this keyword is set, the position of the map projection in the graphics window
;        is obtained from the last image displayed with cgImage. This makes it extremely
;        easy to display an image and immediately set up a map projection space that will
;        allow you to annotate the image using map locations.
;     position: in, optional, type=FltArr(4)
;        The normalized position of the map projection space in the graphics window.
;        The default is [0.075, 0.075, 0.925, 0.925]
;     radians: in, optional, type=boolean, default=0
;        Set this keyword to indicate latitude and longitude values are in radians rather 
;        than degrees.
;     semimajor_axis: in, optional, type=double, default=varies
;        The length of the semimajor axis of the ellipsoid in meters. Normally calculated
;        from the ELLIPSOID keyword values.
;     semiminor_axis: in, optional, type=double, default=varies
;        The length of the semiminor axis of the ellipsoid in meters. Normally calculated
;        from the ELLIPSOID keyword values.
;     sphere_radius: in, optional, type=double, default=varies
;        The length of the ellipsoidal sphere in meters. Normally calculated from the 
;        ELLIPSOID keyword values.
;     title: in, optional, type=string, default=""
;        The title of the map projection display.
;     uvalue: in, optional, type=any, default=none
;        A storage space for storing any kind of IDL variable of importance to the user.
;     window: in, optional, type=boolean, default=0
;        If this keyword is set, the object replaces any commands in a current
;        cgWindow or it opens a new cgWindow and adds itself to it.
;     xrange: in, optional, type=various
;        Set this keyword to the X axis range desired in the data coordinate system.
;        Normally expressed in XY projected meter space, unless the LATLON_RANGES 
;        keyword is set. The default is mapStruct.uv_box[[0,2]].
;     yrange: in, optional, type=various
;         Set this keyword to the X axis range desired in the data coordinate system.
;         Normally expressed in XY projected meter space, unless the LATLON_RANGES 
;         keyword is set. The default is mapStruct.uv_box[[1,3]].
;     zone: in, optional, type=integer, default=varies
;         The zone (normally in UTM projections) of the map projection. If not given and needed,
;         calculated from the CENTER_LATITUDE and CENTER_LONGITUDE keyword values.
;     _EXTRA: in, optional
;         Other keywords accepted by the MAP_PROJ_INIT command are allowed and are passed
;         directly to the MAP_PROJ_INIT program.
;
;---------------------------------------------------------------------------
FUNCTION cgMap::INIT, map_projection, $
    ADDCMD=addcmd, $
    ASPECT=aspect, $
    BACKGROUND=background, $
    BCOLOR=bcolor, $
    BOX_AXES=box_axes, $
    CCOLOR=ccolor, $
    CENTER_LATITUDE=center_latitude, $
    CENTER_LONGITUDE=center_longitude, $
    COLOR=color, $
    CONTINENTS=continents, $
    DATUM=datum, $
    DRAW=draw, $
    EASTING=easting, $
    ELLIPSOID=ellipsoid, $
    ERASE=erase, $
    FILL=fill, $
    GCOLOR=gcolor, $
    GRID=grid, $
    HIRES=hires, $
    ISOTROPIC=isotropic, $
    LAND_COLOR=land_color, $
    LATLON_RANGES=latlon_ranges, $
    LCOLOR=lcolor, $
    LIMIT=limit, $
    NAME=name, $
    NOBORDER=noborder, $
    NOFORWARDFIX=noForwardFix, $
    NORTHING=northing, $
    ONIMAGE=onimage, $
    POSITION=position, $
    RADIANS=radians, $
    SEMIMAJOR_AXIS=semimajor_axis, $
    SEMIMINOR_AXIS=semiminor_axis, $
    SPHERE_RADIUS=sphere_radius, $
    TITLE=title, $
    WINDOW=window, $
    XRANGE=xrange, $
    YRANGE=yrange, $
    UVALUE=uvalue, $
    ZONE=zone, $
    _EXTRA=extraKeywords

   Compile_Opt idl2
    
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN, 0
   ENDIF
      
   ; Structures used in the object.
   datumStruct = { cgMap_DATUM }
   void = { cgMap_PROJECTION }
   
   ; If you specify a BACKGROUND keyword, then ERASE is set automatically.
   IF N_Elements(background) NE 0 THEN BEGIN
       erase = 1
   ENDIF ELSE background = 'white'
   erase = Keyword_Set(erase)
   SetDefaultValue, center_latitude, 0.0
   SetDefaultValue, center_longitude, 0.0
   SetDefaultValue, color, "opposite"
   SetDefaultValue, easting, 0.0
   SetDefaultValue, northing, 0.0
   SetDefaultValue, position, [0.075, 0.075, 0.925, 0.925]
   SetDefaultValue, title, ""

   ; Default map projection.
   IF N_Elements(map_projection) EQ 0 THEN BEGIN
        this_map_projection = 'Equirectangular'
        map_projection = 117
        limit = [-90, -180, 90, 180]
   ENDIF
   
    projections=[ {cgMap_PROJECTION, 'UTM', 101, 0 }, $  ; GCTP 101
                  {cgMap_PROJECTION, 'State Plane', 102, 0 }, $  ; GCTP 102
                  {cgMap_PROJECTION, 'Albers Equal Area', 103, 0 }, $  ; GCTP 103
                  {cgMap_PROJECTION, 'Lambert Conformal Conic', 104, 0 }, $  ; GCTP 104
                  {cgMap_PROJECTION, 'Mercator', 105, 0 }, $  ; GCTP 105
                  {cgMap_PROJECTION, 'Polar Stereographic', 106, 0 }, $  ; GCTP 106
                  {cgMap_PROJECTION, 'Polyconic', 107, 0 }, $  ; GCTP 107
                  {cgMap_PROJECTION, 'Equidistant Conic A', 108, 0 }, $  ; GCTP 108
                  {cgMap_PROJECTION, 'Transverse Mercator', 109, 0 }, $  ; GCTP 109
                  {cgMap_PROJECTION, 'Stereographic', 110, 1 }, $  ; GCTP 110
                  {cgMap_PROJECTION, 'Lambert Azimuthal', 111, 1 }, $  ; GCTP 111
                  {cgMap_PROJECTION, 'Azimuthal', 112, 1 }, $  ; GCTP 112
                  {cgMap_PROJECTION, 'Gnomonic', 113, 1 }, $  ; GCTP 113
                  {cgMap_PROJECTION, 'Orthographic', 114, 1 }, $  ; GCTP 114
                  {cgMap_PROJECTION, 'Near Side Perspective', 115, 1 }, $  ; GCTP 115
                  {cgMap_PROJECTION, 'Sinusoidal', 116, 1 }, $  ; GCTP 116
                  {cgMap_PROJECTION, 'Equirectangular', 117, 1 }, $  ; GCTP 117
                  {cgMap_PROJECTION, 'Miller Cylindrical', 118, 1 }, $  ; GCTP 118
                  {cgMap_PROJECTION, 'Van der Grinten', 119, 1 }, $  ; GCTP 119
                  {cgMap_PROJECTION, 'Hotine Oblique Mercator A', 120, 0 }, $ ; GCTP 120
                  {cgMap_PROJECTION, 'Robinson', 121, 1 }, $ ; GCTP 121
                  {cgMap_PROJECTION, 'Space Oblique Mercator A', 122, 0 }, $ ; GCTP 122
                  {cgMap_PROJECTION, 'Alaska Conformal', 123, 0 }, $ ; GCTP 123
                  {cgMap_PROJECTION, 'Interrupted Goode', 124, 1 }, $  ; GCTP 124
                  {cgMap_PROJECTION, 'Mollweide', 125, 1 }, $ ; GCTP 125
                  {cgMap_PROJECTION, 'Interrupted Mollweide', 126, 1 }, $ ; GCTP 126
                  {cgMap_PROJECTION, 'Hammer', 127, 1 }, $  ; GCTP 127
                  {cgMap_PROJECTION, 'Wagner IV', 128, 1 }, $ ; GCTP 128
                  {cgMap_PROJECTION, 'Wagner VII', 129, 1 }, $ ; GCTP 129
                  {cgMap_PROJECTION, 'Integerized Sinusoidal', 131, 1 }, $ ; GCTP 131
                  {cgMap_PROJECTION, 'Equidistant Conic B', 208, 0 }, $ ; GCTP 208
                  {cgMap_PROJECTION, 'Hotine Oblique Mercator B', 220, 0 }, $ ; GCTP 220
                  {cgMap_PROJECTION, 'Space Oblique Mercator B', 222, 0 }] ; GCTP 222
                  
    IF Float(!Version.Release) GE 8.0 THEN BEGIN
        projections = [projections, {cgMap_PROJECTION, 'Cylindrical Equal Area', 132, 0 }]
        
        ; Lambert Azimuthal now allows all ellipsoids.
        index = Where(projections.name EQ 'Lambert Azimuthal', count)
        IF count GT 0 THEN projections[index].sphereOnly = 0
    ENDIF

    ; Find the map projection.
    IF Size(map_projection, /TNAME) EQ 'STRING' THEN BEGIN
        index = Where(StrUpCase(projections.name[*]) EQ StrUpCase(map_projection))
        IF index[0] EQ -1 THEN Message, 'Cannot find map projection ' + map_projection + ' in the CGTP projection list.'
    ENDIF ELSE BEGIN
        index = Where(projections.index EQ map_projection, count)
        IF count EQ 0 THEN Message, 'Cannot find map projection index ' + StrTrim(map_projection,2) + ' in GCTP projection list.' 
    ENDELSE
    this_map_projection = projections[index]
    IF StrUpCase(this_map_projection.name) EQ StrUpCase('Mercator') THEN BEGIN
       IF N_Elements(limit) EQ 0 THEN limit = [-84.75, -180, 84.75, 180]
    ENDIF
   
   ; Find the datum.
   theDatums = Replicate(datumStruct, 21)
   theDatums[0] =  { cgMap_DATUM,  0, 'Clark 1866', 6378206.4 , 6356583.8  }
   theDatums[1] =  { cgMap_DATUM,  1, 'Clark 1880', 6378249.145, 6356514.86955  }
   theDatums[2] =  { cgMap_DATUM,  2, 'Bessel', 6377397.155, 6356078.96284 }
   theDatums[3] =  { cgMap_DATUM,  3, 'International 1967', 6378157.5, 6356772.2 }
   theDatums[4] =  { cgMap_DATUM,  4, 'International 1909', 6378388.0, 6356911.94613  }
   theDatums[5] =  { cgMap_DATUM,  5, 'WGS 72', 6378135.0, 6356750.519915  }
   theDatums[6] =  { cgMap_DATUM,  6, 'Everst', 6377276.3452 , 6356075.4133 }
   theDatums[7] =  { cgMap_DATUM,  7, 'WGS 66', 6378145.0 , 6356759.769356  }
   theDatums[8] =  { cgMap_DATUM,  8, 'WGS 84', 6378137.0, 6356752.314245 }
   theDatums[9] =  { cgMap_DATUM,  9, 'Airy', 6377563.396, 6356256.91  }
   theDatums[10] = { cgMap_DATUM, 10, 'Modified Everest', 6377304.063, 6356103.039 }
   theDatums[11] = { cgMap_DATUM, 11, 'Modified Airy', 6377340.189, 6356034.448  }
   theDatums[12] = { cgMap_DATUM, 12, 'Walbeck', 6378137.0, 6356752.314245 }
   theDatums[13] = { cgMap_DATUM, 13, 'Southeast Asia', 6378155.0, 6356773.3205 }
   theDatums[14] = { cgMap_DATUM, 14, 'Australian National', 6378160.0, 6356774.719 }
   theDatums[15] = { cgMap_DATUM, 15, 'Krassovsky', 6378245.0, 6356863.0188 }
   theDatums[16] = { cgMap_DATUM, 16, 'Hough', 6378270.0 , 6356794.343479  }
   theDatums[17] = { cgMap_DATUM, 17, 'Mercury 1960', 6378166.0, 6356784.283666  }
   theDatums[18] = { cgMap_DATUM, 18, 'Modified Mercury 1968', 6378150.0, 6356768.337303 }
   theDatums[19] = { cgMap_DATUM, 19, 'Sphere', 6370997.0, 6370997.0 }
   theDatums[20] = { cgMap_DATUM,  8, 'GRS 1980', 6378137.0, 6356752.31414 }
   
   ; Since I already have "WGS 84" in the list, and since IDL 8 introduces an ellipsoid 
   ; with this name, I am going to use index 24 to list the more commonly used "WGS84" name.
   IF Float(!Version.Release) GE 8.0 THEN BEGIN
      theDatums = [theDatums, $
                  { cgMap_DATUM, 20, 'Clarke IGN', 6378249.2, 6356515.0 }, $
                  { cgMap_DATUM, 21, 'Helmert 1906', 6378200.0, 6356818.2 }, $
                  { cgMap_DATUM, 22, 'Modified Fischer 1960', 6378115.0, 6356773.3 }, $
                  { cgMap_DATUM, 23, 'South American 1969', 6378160.0, 6356774.7 }, $
                  { cgMap_DATUM, 24, 'WGS84', 6378137.0, 6356752.314245 }]
       
   ENDIF
   
   ; Need a datum?
   IF (N_Elements(datum) EQ 0) && (N_Elements(ellipsoid) EQ 0) THEN BEGIN
      IF this_map_projection.sphereOnly EQ 1 THEN datum = 19 ELSE datum = 0
   ENDIF
   IF (N_Elements(datum) EQ 0) && (N_Elements(ellipsoid) NE 0) THEN datum = ellipsoid
   IF N_Elements(datum) EQ 0 THEN BEGIN
        thisDatum = theDatums[19] 
   ENDIF ELSE BEGIN
        IF Size(datum, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase(theDatums.name) EQ StrUpCase(datum))
            
            ; If you can't find one, try compressing the names.
            IF index[0] EQ -1 THEN BEGIN
               index = Where(StrCompress(StrUpCase(theDatums.name), /Remove_All) EQ $
                    StrCompress(StrUpCase(datum), /Remove_All))
            ENDIF
            
            ; Now if you can't find one, report it.
            IF index[0] EQ -1 THEN Message, 'Cannot find datum ' + datum + ' in datum list.' 
            thisDatum = theDatums[index[0]]
        ENDIF ELSE thisDatum = theDatums[0 > datum < (N_Elements(theDatums)-1)]
   ENDELSE
   
   ; There is a bug in all versions of IDL up to IDL 8.1 apparently that
   ; produces the wrong result when a UTM projection is used in conjunction
   ; with a WGS84 datum (the most common datum used in this projection). Here
   ; we substitute the WALBECK datum, which is nearly identical to WGS84 are
   ; results in position errors of less than a meter typically.
   IF ((StrUpCase(thisDatum.Name) EQ 'WGS 84') || (StrUpCase(thisDatum.Name) EQ 'WGS84')) && $
      (StrUpCase(this_map_projection.Name) EQ 'UTM') && $
      (Float(!version.release) LT 8.2) THEN BEGIN
          Print, 'Switching UTM datum from WGS84 to WALBECK to avoid UTM projection bug.'
          thisDatum = { cgMAP_DATUM, 12, 'Walbeck', 6378137.0, 6356752.314245 }
   ENDIF
   
   ; Modify the radii?
   IF N_Elements(sphere_radius) NE 0 THEN BEGIN
        semimajor_axis = sphere_radius
        semiminor_axis = sphere_radius
   ENDIF
   IF N_Elements(semimajor_axis) NE 0 THEN thisDatum.semimajor_axis = semimajor_axis
   IF N_Elements(semiminor_axis) NE 0 THEN thisDatum.semiminor_axis = semiminor_axis
   IF N_Elements(zone) EQ 0 THEN zone = cgUTMZone(center_longitude, center_latitude)
   
      ; Default MAP_PROJ_INIT keywords.
   IF N_Elements(extraKeywords) NE 0 $
        THEN self._cg_map_projection_keywords = Ptr_New(extraKeywords) $
        ELSE self._cg_map_projection_keywords = Ptr_New(/ALLOCATE_HEAP)
   
   ; Are ISOTROPIC or ASPECT keywords used?
   IF Keyword_Set(isotropic) THEN self._cg_isotropic = 1
   IF N_Elements(aspect) NE 0 THEN self._cg_aspect = aspect
   
   ; Load the object.
   self._cg_background = background
   self._cg_center_latitude = center_latitude
   self._cg_center_longitude = center_longitude
   self._cg_color = color
   self._cg_erase = erase
   self._cg_multi_position = FltArr(4)
   self._cg_noborder = Keyword_Set(noborder)
   self._cg_noforwardfix = Keyword_Set(noforwardfix)
   self._cg_onimage = Keyword_Set(onimage)
   self._cg_radians = Keyword_Set(radians)
   self._cg_theDatums = Ptr_New(theDatums)
   self._cg_theProjections = Ptr_New(projections)
   self._cg_thisDatum = thisDatum
   self._cg_thisProjection = this_map_projection
   self._cg_title = title
   self._cg_zone = zone
   self._cg_overlays = Obj_New('cgContainer')
   
   ; Do you have a limit?
   IF N_Elements(limit) NE 0 $
      THEN self._cg_limit = Ptr_New(Double(limit)) $
      ELSE self._cg_limit = Ptr_New(/ALLOCATE_HEAP)
   
   ; Get the map structure.
   mapStruct = self -> SetMapProjection()
   
   ; Do you need to set the data ranges? If so, these should be set from
   ; the UV_BOX of the map structure.
   IF N_Elements(xrange) EQ 0 THEN BEGIN 
      xrange = mapStruct.uv_box[[0,2]]
      latlon_ranges = 0
   ENDIF
   IF N_Elements(yrange) EQ 0 THEN BEGIN 
      yrange = mapStruct.uv_box[[1,3]]
      latlon_ranges = 0
   ENDIF
         
   ; Are the ranges in lat/lon space?
   IF Keyword_Set(latlon_ranges) THEN BEGIN
      uvcoords = self -> Forward(xrange, yrange)
      xrange = Reform(uvcoords[0,*])
      yrange = Reform(uvcoords[1,*])
   ENDIF
   
      ; Call the SUPERCLASS object INIT method.
   ok = self -> cgCOORD::INIT ($
        POSITION=position, $
        XRANGE=xrange, $
        YRANGE=yrange, $
        XLOG=0, YLOG=0, $ ; No log axis on maps!
        NAME=name, $
        UVALUE=uvalue)
        
   IF ~ok THEN RETURN, 0
   
   ; Do you need overlay objects in this map?
   IF N_Elements(continents) NE 0 THEN BEGIN
   
       ; Is this a continental object? If so add it, if not
       ; create one and add it.
       IF Obj_Valid(continents) THEN BEGIN
           continents -> GetProperty, NAME=continentsName
           IF continentsName EQ "" THEN continentsName = 'MAPCONTINENTS'
           continents -> SetProperty, NAME=continentsName
           self._cg_overlays -> Add, continents
       ENDIF ELSE BEGIN
           continents = Obj_New('cgMapContinents', self, /COUNTRIES, $
               COLOR=cccolor, NAME='MAPCONTINENTS', HIRES=Keyword_Set(hires), $
               FILL=Keyword_Set(fill), LAND_COLOR=land_color)
           self._cg_overlays -> Add, continents
       ENDELSE
   ENDIF
   
   IF N_Elements(grid) NE 0 THEN BEGIN
   
       ; Is this a grid object? If so add it, if not
       ; create one and add it.
       IF Obj_Valid(grid) THEN BEGIN
           grid -> GetProperty, NAME=gridName
           IF gridName EQ "" THEN gridName = 'MAPGRID'
           grid -> SetProperty, NAME=gridName
           self._cg_overlays -> Add, grid
       ENDIF ELSE BEGIN
           grid = Obj_New('cgMapGrid', self, COLOR=gcolor, $
              NAME='MAPGRID', BOX_AXES=box_axes, BCOLOR=bcolor, LCOLOR=lcolor)
           self._cg_overlays -> Add, grid
       ENDELSE
   ENDIF
   
   ; Need to add this command to a resizeable cgWindow?
   IF Keyword_Set(window) THEN self -> AddCmd, /REPLACE
   IF Keyword_Set(addcmd) THEN self -> AddCmd
   
   ; Need immediate draw?
   IF Keyword_Set(draw) THEN self -> Draw
   
   RETURN, 1

END 


;+--------------------------------------------------------------------------
;   Adds the object as a command (the DRAW method is called) in a cgWindow 
;   resizeable graphics window. If there is no current cgWindow, one is
;   created.
;
; :Keywords:
;     method: in, optional, type='string', default='draw'
;        The object method to add to the cgWindow.
;     replace: in, optional, type=boolean, default=0
;        If this keyword is set, object DRAW method replaces any commands in the
;        current graphics window.
;---------------------------------------------------------------------------
PRO cgMap::AddCmd, REPLACE=replace, METHOD=method

   IF N_Elements(method) EQ 0 THEN method = 'DRAW'

   ; Which method are we adding?
   currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
   
   ; Do we have a window to add it to?
   IF wincnt EQ 0 THEN cgWindow
   
   ; Add (or replace) the command to the window.
   IF Keyword_Set(replace) $
      THEN cgWindow, method, self, /Method, /ReplaceCmd $ ; Replace all commands in the window
      ELSE cgWindow, method, self, /Method, /AddCmd       ; Add this command to the window.
   
END 


;+--------------------------------------------------------------------------
;   Adds the an overlay object into the overlay container of the object.
;   Overlay objects are drawn (by calling their DRAW methods) after the
;   map coordinate space is set up in the DRAW method of the object. They
;   are drawn in the order they appear in the object.
;
; :Params:
;    overlayObject: required, type=object
;       The object that will draw a graphic overlay in the map projection space
;       created by this map object. Typically, overlay objects contain map grid
;       lines (cgMapGrid object), continental outlines (cgMapContinents object), or 
;       other types of graphical overlays. The only requirement of an overlay  
;       object is that is have a DRAW method and that it draw into a map 
;       projection space. This may be an object array.
;
;---------------------------------------------------------------------------
PRO cgMap::AddOverlay, overLayObject

   ; Required parameter must be a valid object with a DRAW method.
   IF N_Elements(overLayObject) EQ 0 THEN $
      Message, 'A map overlay object is a required parameter.'
   IF ~Obj_Valid(overLayObject) THEN Message, 'A valid overlay object is required.'
   IF Float(!Version.Release) GT 6.4 THEN BEGIN
        hasMethod = Call_Function('Obj_HasMethod', overLayObject, 'DRAW')
        IF ~hasMethod THEN Message, 'The overlay object must have a DRAW method.'
   ENDIF
   
   ; Add the object to the overlay container.
   FOR j=0,N_Elements(overlayObject)-1 DO BEGIN
      self._cg_overlays -> Add, overLayObject   
   ENDFOR
   
END 


;+--------------------------------------------------------------------------
;   Advances the map projection position to the next position of a multiple
;   plot (using !P.MULTI). Does not need to be called directly, as the object
;   will call this method as needed.
;
; :Keywords:
;     draw: in, optional, type=boolean, default=0
;        Set this keyword to immediately call the draw method after the position
;        has been advanced.
;---------------------------------------------------------------------------
PRO cgMap::Advance, DRAW=draw

   IF Total(!P.Multi) NE 0 THEN BEGIN
   
          ; Draw the invisible plot to get plot position.
          IF Size(self._cg_background, /TNAME) EQ 'STRING' $
             THEN background = cgColor(self._cg_background)$
             ELSE background = self._cg_background
          TVLCT, rr, gg, bb, /Get
          Plot, Findgen(11), XStyle=4, YStyle=4, /NoData;, Background=background
          TVLCT, rr, gg, bb
          
          ; Use position coordinates to indicate position in this set of coordinates.
          ; New position based on !P.MULTI position.
          position = [!x.window[0], !y.window[0], !x.window[1], !y.window[1]]
          self._cg_multi_position = position
          
   ENDIF ELSE self._cg_multi_position = FltArr(4)
   
   IF Keyword_Set(draw) THEN self -> Draw
   
END 


;+--------------------------------------------------------------------------
;   This method sets up the map projection space of the object. Also, if map
;   borders or titles are required, they are drawn here. If the object contains
;   any overlay objects, they are also drawn at this time.
;
; :Keywords:
;     erase: in, optional, type=boolean, default=0
;        Set this keyword to erase contents of the map window for one time only
;        in this Draw method. It does NOT set the Erase parameter for the object.
;     nographics: in, optional, type=boolean, default=0
;        If this keyword is set, no graphics are drawn, but the map data coordinate
;        system is set up.
;     
;---------------------------------------------------------------------------
PRO cgMap::Draw, ERASE=erase, NOGRAPHICS=nographics, _EXTRA=extra
 
    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
    ENDIF
    
    displayGraphics = 1 - Keyword_Set(nographics)
    
    ; If this is a graphics device, and there is no current graphics window,
    ; then set the erase flag.
    IF (!D.Name EQ 'WIN' || !D.Name EQ 'X') && (!D.Window LT 0) && displayGraphics THEN BEGIN
        erase = 1
    ENDIF
    
    ; Temporary erase?
    IF N_Elements(erase) NE 0 THEN erase = Keyword_Set(erase)
    
    ; If you are doing multiple plots, then you have to update
    ; your position.
    IF Total(!P.Multi) GT 0 THEN BEGIN
        self -> Advance 
        position = self._cg_multi_position
        old_position = self._cg_position
        self -> SetProperty, POSITION=position
    ENDIF 
    
    ; Are you putting this on an image? If so, get the position from
    ; the last image position in.
    IF self._cg_onimage THEN BEGIN
        COMMON FSC_$CGIMAGE, _cgimage_xsize, _cgimage_ysize, $
                             _cgimage_winxsize, _cgimage_winysize, $
                             _cgimage_position, _cgimage_winID, $
                             _cgimage_current
        old_position = self._cg_position
        self -> SetProperty, POSITION=_cgimage_position
    ENDIF
    
    ; Did you set the isotropic keyword:
    IF self._cg_isotropic THEN BEGIN
        self -> GetProperty, XRANGE=xr, YRANGE=yr
        self._cg_aspect = Double(Abs(yr[1]-yr[0]))/Abs(xr[1]-xr[0])
    ENDIF
    
    ; Do you need an aspect ratio?
    IF (self._cg_aspect NE 0.0) AND (Total(!P.MULTI) EQ 0) THEN BEGIN
        
      position = self._cg_position
       
      trial_position = Aspect(self._cg_aspect, margin=0.)
      trial_width = trial_position[2]-trial_position[0]
      trial_height = trial_position[3]-trial_position[1]
      pos_width = position[2]-position[0]
      pos_height = position[3]-position[1]
    
      ; Same logic as cgImage: try to fit image width, then if you can't get the right aspect
      ; ratio, fit the image height instead.
      fit_ratio = pos_width / trial_width
      IF trial_height * fit_ratio GT pos_height THEN $
      fit_ratio = pos_height / trial_height
    
      ; new width and height
      trial_width *= fit_ratio
      trial_height *= fit_ratio
    
      ; calculate position vector based on trial_width and trial_height
      position[0] += 0.5*(pos_width - trial_width)
      position[2] -= 0.5*(pos_width - trial_width)
      position[1] += 0.5*(pos_height - trial_height)
      position[3] -= 0.5*(pos_height - trial_height)
            
    ENDIF ELSE position = self._cg_position
    
    ; Do you need to erase in the background color? Don't do this if you
    ; are just drawing the coordinate system.
    IF ~Keyword_Set(nographics) THEN BEGIN
        IF N_Elements(erase) EQ 0 THEN erase = self._cg_erase
        p = position
        IF erase THEN cgColorFill, [p[0],p[0],p[2],p[2],p[0]], NORMAL=1, $
                                   [p[1],p[3],p[3],p[1],p[1]], COLOR=self._cg_background
    ENDIF
    
   ; Draw the map data coordinate system.
    mapStruct = self -> SetMapProjection()
    temp_position = self._cg_position
    self._cg_position = position
    self -> cgCoord::Draw, _EXTRA=extra
    self._cg_position = temp_position
    
    ; Draw overlays?
    count = self._cg_overlays -> Count()
    IF (count GT 0) && displayGraphics THEN BEGIN
    
        ; Get the overlay objects out of the overlay container.
        FOR j=0,count-1 DO BEGIN
            thisOverlay = self._cg_overlays -> Get(POSITION=j)
            IF Obj_Valid(thisOverlay) THEN thisOverlay -> Draw
        ENDFOR
    ENDIF
    
    ; Draw a border around the map?
    IF ~Keyword_Set(self._cg_noborder) && displayGraphics THEN BEGIN
        p = position
        cgPlots, [p[0],p[0],p[2],p[2],p[0]], [p[1],p[3],p[3],p[1],p[1]], $
            /NORMAL, COLOR=self._cg_color
    ENDIF
    
    ; Draw a title?
    IF (self._cg_title NE "") && displayGraphics THEN BEGIN
       p = position
       px = (p[2]-p[0])/2.0 + p[0]
       py = (p[3] + 0.05) < 0.975
       cgText, px, py, /Normal, Alignment=0.5, self._cg_title, Charsize=cgDefCharsize()*1.25
    ENDIF
    
    ; If you changed the position for some reason, put it back.
    IF N_Elements(old_position) NE 0 THEN self -> SetProperty, POSITION=old_position
END 


;+--------------------------------------------------------------------------
;   This method erases the graphics window.
;
; :Keywords:
;     color: in, optional, type=string, default='white'
;        The color used in the erasing of the display.
;     
;---------------------------------------------------------------------------
PRO cgMap::Erase, COLOR=color
 
    cgErase, COLOR=color
    
END 


;+--------------------------------------------------------------------------
;   This method transforms latitude and longitude values into projected
;   XY Cartesian values. This is known as the forward map transformation.
;   
; :Returns:
;    The projected XY coordinates are returned in a 2xN array. The first
;    column contains the projected X values and the second column contains
;    the projected Y values.
;    
; :Params:
;     lons: in, required
;         The longitude values to transform. May be a scalar or an array.
;     lats: in, required
;         The latitude values to transform. May be a scalar or an array.
;     mapStruct: in, optional, type=structure
;         The map structure to use in doing the forward transformation. If
;         not provided, the map structure is obtained from the object itself.
;         
; :Keywords:
;     noforwardfix: in, optional, type=boolean, default=0
;         I may be wrong about the fix I put in when the longitude values
;         are the same. If so, setting this keyword will avoid the fix.
;         The default value is the default for the object.
;
;---------------------------------------------------------------------------
FUNCTION cgMap::Forward, lons, lats, mapStruct, NOFORWARDFIX=noForwardFix

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN, -1
    ENDIF
    
    
    IF N_Elements(lons) EQ 0 THEN Message, 'Longitude values are required.'
    IF N_Elements(lats) EQ 0 THEN Message, 'Latitude values are required.'
    IF N_Elements(noforwardfix) EQ 0 $
        THEN noforwardfix = self._cg_noForwardFix $
        ELSE noforwardfix = Keyword_Set(noforwardfix)
    
    ; Forward map transformation.
    IF N_Elements(mapStruct) EQ 0 THEN mapStruct = self->GetMapStruct()
    xycoords = Map_Proj_Forward(lons, lats, MAP_STRUCTURE=mapStruct)
    
    ; Check to see if longitude range values are identical. If so, then fix. This is, I believe,
    ; a bug in IDL code, but I can't get the IDL support engineers to believe it.
    IF ~Keyword_Set(noforwardfix) THEN BEGIN
        xr = Reform(xycoords[0,*])
        n = N_Elements(xr)
        IF (Floats_Equal(xr[0], xr[n-1], ULP=8)) && N_Elements(xr) GT 1 THEN BEGIN
           IF xr[0] GT 0 THEN xycoords[0,0] = -xr[n-1] ELSE xycoords[0,n-1] = Abs(xr[0])
        ENDIF
    ENDIF
    
    RETURN, xycoords
END 



;+--------------------------------------------------------------------------
;   This method returns a 1 if the map projection is a cylindrical projection
;   and a 0 otherwise.
;
;---------------------------------------------------------------------------
FUNCTION cgMap::Is_Cylindrical

    CASE (self._cg_thisProjection).index OF
       100: retval = 1 ; Geographic
       105: retval = 1 ; Mercator
       109: retval = 1 ; Transvere Mercator
       116: retval = 1 ; Sinusoidal
       117: retval = 1 ; Equirectangular
       118: retval = 1 ; Miller Cylindrical
       121: retval = 1 ; Robinson
       125: retval = 1 ; Mollweide
       126: retval = 1 ; Interrupted Mollweide
       127: retval = 1 ; Hammer-Aitoff
       128: retval = 1 ; Wagner IV
       129: retval = 1 ; Wagner VII
       132: retval = 1 ; Cylindrical Equal Area
       ELSE: retval = 0
    ENDCASE
    
    RETURN, retval
END 


;+--------------------------------------------------------------------------
;   This method returns a map structure that is the result of calling Map_Proj_Init.
;   It is important to get a fresh map structure because up until IDL 8, the map
;   structure was ephemeral (http://www.idlcoyote.com/map_tips/ephemeral.php).
;   Every time this function is called, a new map structure is created.
;
;---------------------------------------------------------------------------
FUNCTION cgMap::GetMapStruct
    RETURN, self -> SetMapProjection()
END 


;+--------------------------------------------------------------------------
;   This method allows the user to get various properties of the object. In general,
;   the same keywords that are used for the INIT method can be used here. Here
;   are a few that are different.
;   
; :Keywords:
;     boundary: out, optional, type=array
;        A four-element array giving the boundaries of the image in the form
;        [x0,y0,x1,y1]. This is a more convenient way of expressing the range
;        of the map space.
;     latlonbox: out, optional, type=array
;        A four-element array giving the boundaries of the map projection in the
;        Google Map form of [north, south, east, west]. This is useful when you
;        are creating image overlays to be added to Goggle Earth.
;     overlays: out, optional, type=object
;        Set this keyword to a named variable that will return an object
;        array containing the overlay objects in the map object.
;---------------------------------------------------------------------------
PRO cgMap::GetProperty, $
    BACKGROUND=background, $
    BCOLOR=bcolor, $
    BOUNDARY=boundary, $
    CENTER_LATITUDE=center_latitude, $
    CENTER_LONGITUDE=center_longitude, $
    COLOR=color, $
    DATUM=datum, $
    DRAW=draw, $
    EASTING=easting, $
    ELLIPSOID=ellipsoid, $
    ERASE=erase, $
    HIRES=hires, $
    LATLONBOX=latlonbox, $
    LIMIT=limit, $
    MAP_PROJECTION=map_projection, $
    NAME=name, $
    NOBORDER=noborder, $
    NOFORWARDFIX=noforwardfix, $
    NORTHING=northing, $
    ONIMAGE=onimage, $
    OVERLAYS=overlays, $
    POSITION=position, $
    RADIANS=radians, $
    SEMIMAJOR_AXIS=semimajor_axis, $
    SEMIMINOR_AXIS=semiminor_axis, $
    SPHERE_RADIUS=sphere_radius, $
    TITLE=title, $
    XRANGE=xrange, $
    YRANGE=yrange, $
    UVALUE=uvalue, $
    ZONE=zone, $
    _REF_EXTRA=extraKeywords

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
    ENDIF
   
   ; Make sure the map structure is up to date by always calculating it in real-time.
   mapStruct = self -> SetMapProjection() 
   
   IF Arg_Present(overlays) THEN overlays = self._cg_draw_overlays -> Get(/ALL)
   position = self._cg_position
   IF Arg_Present(xrange) THEN BEGIN
      IF Keyword_Set(latlon_ranges) THEN BEGIN
            llcoords = Map_Proj_Inverse(self._cg_xrange, self._cg_yrange, MAP_STRUCTURE=mapStruct)
            xrange = Reform(llcoords[0,*])
      ENDIF ELSE xrange = self._cg_xrange
   ENDIF
   IF Arg_Present(yrange) THEN BEGIN
      IF Keyword_Set(latlon_ranges) THEN BEGIN
            llcoords = Map_Proj_Inverse(self._cg_xrange, self._cg_yrange, MAP_STRUCTURE=mapStruct)
            yrange = Reform(llcoords[1,*])
      ENDIF ELSE yrange = self._cg_yrange
   ENDIF
   
   ; The boundary is just the XRANGE and YRANGE in another form.
   IF Arg_Present(boundary) THEN BEGIN
      IF Keyword_Set(latlon_ranges) THEN BEGIN
            llcoords = Map_Proj_Inverse(self._cg_xrange, self._cg_yrange, MAP_STRUCTURE=mapStruct)
            xrange = Reform(llcoords[0,*])
      ENDIF ELSE xrange = self._cg_xrange
      IF Keyword_Set(latlon_ranges) THEN BEGIN
            llcoords = Map_Proj_Inverse(self._cg_xrange, self._cg_yrange, MAP_STRUCTURE=mapStruct)
            yrange = Reform(llcoords[1,*])
      ENDIF ELSE yrange = self._cg_yrange
      boundary = [xrange[0], yrange[0], xrange[1], yrange[1]]
   ENDIF
   
   ; The latlonbox is just the XRANGE and YRANGE in a form Google Earth prefers 
   ; [north, south, east, west] in degrees.
   IF Arg_Present(latlonbox) THEN BEGIN
      latlon_ranges = 1
      IF Keyword_Set(latlon_ranges) THEN BEGIN
            llcoords = Map_Proj_Inverse(self._cg_xrange, self._cg_yrange, MAP_STRUCTURE=mapStruct)
            xrange = Reform(llcoords[0,*])
      ENDIF ELSE xrange = self._cg_xrange
      IF Keyword_Set(latlon_ranges) THEN BEGIN
            llcoords = Map_Proj_Inverse(self._cg_xrange, self._cg_yrange, MAP_STRUCTURE=mapStruct)
            yrange = Reform(llcoords[1,*])
      ENDIF ELSE yrange = self._cg_yrange
      latlonbox = [ yrange[1], yrange[0], xrange[0], xrange[1] ]
   ENDIF
   
   ; Other keywords.
   background = self._cg_background
   color = self._cg_color
   center_latitude = self._cg_center_latitude
   center_longitude = self._cg_center_longitude
   easting = self._cg_easting
   erase = self._cg_erase
   IF N_Elements(*self._cg_limit) NE 0 THEN limit = *self._cg_limit
   noborder = self._cg_noborder
   noforwardfix = self._cg_noforwardfix
   northing = self._cg_northing
   map_projection = self._cg_thisProjection.name
   IF Arg_Present(map_proj_keywords) THEN BEGIN
        IF Ptr_Valid(self._cg_map_projection_keywords) THEN BEGIN
            IF N_Elements(*self._cg_map_projection_keywords) NE 0 THEN $
                map_proj_keywords = *self._cg_map_projection_keywords
        ENDIF
   ENDIF
   datum = self._cg_thisDatum.name
   ellipsoid = self._cg_thisDatum.name
   radians = self._cg_radians
   sphere_radius = self._cg_thisDatum.semimajor_axis
   semimajor_axis = self._cg_thisDatum.semimajor_axis
   semiminor_axis = self._cg_thisDatum.semiminor_axis
   zone = self._cg_zone
   
   ; If asked for the GRID, see if you can find an object with the name MAPGRID
   ; in the overlay container. If you can, return it.
   IF Arg_Present(grid) THEN BEGIN
       gridPos = self._cg_overlays -> FindByName('MAPGRID', COUNT=count)
       IF count GT 0 THEN grid = self._cg_overlays -> Get(POSITION=gridPos)
   ENDIF
   
   ; Superclass keywords.
   IF (N_ELEMENTS(extraKeywords) GT 0) THEN $
       self -> cgCOORD::GetProperty, _EXTRA=extraKeywords

END 


;+--------------------------------------------------------------------------
;   This method transforms X and Y projected Cartesian map coordinates into
;   longitude and latitude values. This is known as the inverse map transformation.
;   
; :Returns:
;    The projected lon/lat coordinates are returned in a 2xN array. The first
;    column contains the longitude values and the second column contains
;    the latitude values.
;    
; :Params:
;     x: in, required
;         The projected X values to transform. May be a scalar or an array.
;     y: in, required
;         The projected Y values to transform. May be a scalar or an array.
;
;---------------------------------------------------------------------------
FUNCTION cgMap::Inverse, x, y
    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN, -1
    ENDIF
    
    
    IF N_Params() NE 2 THEN Message, 'Both longitude and latitude values are required.'
    IF N_Elements(x) EQ 0 THEN Message, 'Projected X values are required.'
    IF N_Elements(y) EQ 0 THEN Message, 'Projected Y values are required.'
    
    ; Inverse map transformation.
    lonlat = Map_Proj_Inverse(x, y, MAP_STRUCTURE=self->GetMapStruct())
    
    RETURN, lonlat
END 



;--------------------------------------------------------------------------
;   This method uses the data ranges to calculate appropriate latitude and
;   longitude lines to be drawn through the map projection. Vectors of 
;   latitudes and longitudes, as well as latitute and longitude names is
;   created.
;
; :Keywords:
;     latdelta: in, optional, type=float
;        The degree spacing in latitude between latitude lines.
;     latlab: out, optional, type=float
;        The suggested location for the latitude labels.
;     latnames: out, optional, type=string
;        The vector of latitude names associated with the `lats` vector.
;     lats: out, optional, type=float
;        A vector of latitude values that describe latitude lines traversing
;        the data range.
;     londelta: in, optional, type=float
;        The degree spacing in longitude between longitude lines.
;     lonlab: out, optional, type=float
;        The suggested location for the longitude labels.
;     lonnames: out, optional, type=string
;        The vector of longitude names associated with the `lons` vector.
;     lons: out, optional, type=float
;        A vector of longitude values that describe longitude lines traversing
;        the data range.
;     success: out, optional, type=boolean
;        Will be set to 1 on return, if the operation was successful. Otherwise,
;        this value is set to 0.
;--------------------------------------------------------------------------
PRO cgMap::LatLonLabels, $
    LATDELTA=latdelta, $
    LATLAB=latlab, $
    LATNAMES=latnames, $
    LATS=lats, $
    LONDELTA=londelta, $
    LONLAB=lonlab, $
    LONNAMES=lonnames, $
    LONS=lons, $
    SUCCESS=success

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        success = 0
        RETURN
    ENDIF
    
    ; Assume success.
    success = 1
    
    ; The longitudes might be calculated from the results of the latitude calculation.
    ; If they are, this flag will be set to 1.
    lonsdone = 0
    latsdone = 0
    
    ; Get the ranges of the map coordinate object.
    self -> GetProperty, XRANGE=xrange, YRANGE=yrange

    ; Sample XY grid at 625 locations throughout the grid (25x25).
    xstep = (xrange[1] - xrange[0]) / 24.0
    ystep = (yrange[1] - yrange[0]) / 24.0
    xvec = (Findgen(25) * xstep) + xrange[0]
    yvec = (Findgen(25) * ystep) + yrange[0]
    xarr = Rebin(xvec, 25, 25)
    yarr = Rebin(Reform(Reverse(yvec), 1, 25), 25, 25)
    
    ; Find the latitude/longitude of these locations. Find the min, max,
    ; and lat/lon at the center of the grid.
    ll = Map_Proj_Inverse(xarr, yarr, MAP_STRUCTURE=self->GetMapStruct())
    latlon = Reform(ll, 2, 25, 25)
    latlon = Transpose(latlon, [1,2,0])
    latitudes = latlon[*,*,1]
    longitudes = latlon[*,*,0]

    ; Convert the longitudes to 0 to 360. Otherwise, I have
    ; problems near the date line.
    longitudes = (longitudes + 360.0) MOD 360.0
    
    lon_min = Min(longitudes, MAX=lon_max, /NAN)
    lat_min = Min(latitudes, MAX=lat_max, /NAN)
    center_lat = latitudes[12,12]
    center_lon = longitudes[12,12]
    
    ; We are going to try to have seven lines running through the grid space.
    ; We will have special rules if the center latitude is at the pole.
    latrange = Abs(lat_max - lat_min)
    IF latrange GT 90.0 THEN BEGIN
        IF N_Elements(latdelta) NE 0 THEN BEGIN
            num = 180 / Fix(latdelta)
            lats = -90 > Findgen(num) * latDelta -90.0 < 90.0
            latsdone = 1
        ENDIF ELSE BEGIN
           lats = -90.0 > (Findgen(7) * 30 - 90.0) < 90.0
           latsdone = 1
        ENDELSE
    ENDIF
    IF N_Elements(latDelta) EQ 0 THEN latstep =  latrange / 6.0 ELSE latStep = latDelta
    
    lonrange = Abs(lon_max - lon_min)
    IF lonrange GT 180.0 THEN BEGIN
        IF N_Elements(londelta) NE 0 THEN BEGIN
            num = 360 / Fix(londelta)
            lons = 0.0  > Findgen(num) * lonDelta   < 360.0
            lonsdone = 1
        ENDIF ELSE BEGIN
           lons = 0 > (Findgen(9) * 45 ) < 360.0
           lonsdone = 1
        ENDELSE
    ENDIF
    IF N_Elements(lonDelta) EQ 0 THEN lonstep =  (lonrange)/ 6.0 ELSE lonstep = lonDelta
    
    ; Make sure we don't have a center latitude at either pole. If we
    ; do, then lons are calulated differently.
    IF (center_lat GT (90.-0.05)) && (center_lat LT (90.0 + 0.05)) THEN BEGIN
       lats = Scale_Vector(Findgen(5), 0 > Round(lat_min) < 80, 80) 
       latsdone = 1 
       IF lonstep GT 40 THEN BEGIN
          lons = Findgen(11) * 36 
          lonsDone = 1
       ENDIF      
    ENDIF ELSE BEGIN
       IF (center_lat LT (-90.+0.05)) && (center_lat GT (-90.0 - 0.05)) THEN BEGIN
           lats = Scale_Vector(Findgen(5), -80, 0 < Round(lat_max))  
           latsdone = 1    
           IF lonstep GT 40 THEN BEGIN
              lons = Findgen(11) * 36 
              lonsDone = 1
           ENDIF 
       ENDIF    
    ENDELSE
    
    IF latsdone EQ 0 THEN BEGIN
       CASE 1 OF
       
           (latstep GE 30): BEGIN
                latstep = 30
                center_lat = Round(center_lat)
                lats = -90.0 > [(Indgen(4)+1)*(-latstep) + center_lat, center_lat,  $
                             (Indgen(4)+1)*( latstep) + center_lat] < 90.0
                END
           (latstep GT 10) && (latstep LT 60): BEGIN
                latstep = Ceil(latstep/10.) * 10.0
                center_lat = Round(center_lat)
                lats = -90.0 > [(Indgen(4)+1)*(-latstep) + center_lat, center_lat,  $
                             (Indgen(4)+1)*( latstep) + center_lat] < 90.0
                END
           (latstep GT 1) && (latstep LT 10): BEGIN
                latstep = Ceil(latstep)
                center_lat = Round(center_lat)
                lats = -90.0 > [(Indgen(4)+1)*(-latstep) + center_lat, center_lat,  $
                             (Indgen(4)+1)*( latstep) + center_lat] < 90.0
                END
           (latstep GT 0.1) && (latstep LT 1): BEGIN
                latstep = Ceil(latstep*10.0)/ 10.
                center_lat = Round(center_lat*10.0) / 10.0
                lats = -90.0 > [(Indgen(4)+1)*(-latstep) + center_lat, center_lat,  $
                             (Indgen(4)+1)*( latstep) + center_lat] < 90.0
                END
           (latstep GT 0.01) && (latstep LT 0.1): BEGIN
                latstep = Ceil(latstep*100.0)/ 100.
                center_lat = Round(center_lat*100.0) / 100.0
                lats = -90.0 > [(Indgen(4)+1)*(-latstep) + center_lat, center_lat,  $
                             (Indgen(4)+1)*( latstep) + center_lat] < 90.0
                END
           (latstep LT 0.01) : BEGIN
                latstep = Ceil(latstep*1000.0)/ 1000.
                center_lat = Round(center_lat*1000.0) / 1000.0
                lats = -90.0 > [(Indgen(4)+1)*(-latstep) + center_lat, center_lat,  $
                             (Indgen(4)+1)*( latstep) + center_lat] < 90.0
                END
           ELSE: BEGIN
                latstep = 30
                center_lat = Round(center_lat)
                lats = -90.0 > [(Indgen(4)+1)*(-latstep) + center_lat, center_lat,  $
                             (Indgen(4)+1)*( latstep) + center_lat] < 90.0
                END
       ENDCASE
    ENDIF
    
    IF lonsDone EQ 0 THEN BEGIN
       CASE 1 OF
       
           (lonstep GE 60): BEGIN
                lonstep = 60
                center_lon = Round(center_lon)
                lons = -180.0 > [(Indgen(3)+1)*(-lonstep) + center_lon, center_lon,  $
                             (Indgen(3)+1)*( lonstep) + center_lon] < 360.0
                END
           (lonstep GT 10) && (lonstep LT 60): BEGIN
                lonstep = Ceil(lonstep/10.) * 10.0
                center_lon = Round(center_lon)
                lons = -180.0 > [(Indgen(4)+1)*(-lonstep) + center_lon, center_lon,  $
                             (Indgen(4)+1)*( lonstep) + center_lon] < 360.0
                END
           (lonstep GT 1) && (lonstep LT 10): BEGIN
                lonstep = Ceil(lonstep)
                center_lon = Round(center_lon)
                lons = -180.0 > [(Indgen(4)+1)*(-lonstep) + center_lon, center_lon,  $
                             (Indgen(4)+1)*( lonstep) + center_lon] < 360.0
                END
           (lonstep GT 0.1) && (lonstep LT 1): BEGIN
                lonstep = Ceil(lonstep*10.0)/ 10.
                center_lon = Round(center_lon*10.0) / 10.0
                lons = -180.0 > [(Indgen(4)+1)*(-lonstep) + center_lon, center_lon,  $
                             (Indgen(4)+1)*( lonstep) + center_lon] < 360.0
                END
           (lonstep GT 0.01) && (lonstep LT 0.1): BEGIN
                lonstep = Ceil(lonstep*100.0)/ 100.
                center_lon = Round(center_lon*100.0) / 100.0
                lons = -180.0 > [(Indgen(4)+1)*(-lonstep) + center_lon, center_lon,  $
                             (Indgen(4)+1)*( lonstep) + center_lon] < 360.0
                END
           (lonstep LT 0.01) : BEGIN
                lonstep = Ceil(lonstep*1000.0)/ 1000.
                center_lon = Round(center_lon*1000.0) / 1000.0
                lons = -180.0 > [(Indgen(4)+1)*(-lonstep) + center_lon, center_lon,  $
                             (Indgen(4)+1)*( lonstep) + center_lon] < 360.0
                END
           ELSE: BEGIN
                lonstep = 30
                center_lon = Round(center_lon)
                lons = -180.0 > [(Indgen(4)+1)*(-lonstep) + center_lon, center_lon,  $
                             (Indgen(4)+1)*( lonstep) + center_lon] < 360.0
                END
       ENDCASE
     ENDIF
        
    ; The values might need to be sorted.
    lats = lats[Sort(lats)]
    lons = lons[Sort(lons)]
    lons = lons - (LONG(lons )/180)*360.0
    
    ; Labels should be near the center.
    index = N_Elements(lons) / 2
    latlab = (lons[index] - lons[index-1]) / 2.0 + lons[index-1]
    index = N_Elements(lats) / 2 
    lonlab = (lats[index] - lats[index-1]) / 2.0 + lats[index-1]

    ; Set up the latitude and longitude names.
    IF Total(lats-Long(lats)) EQ 0 THEN format='(I0)' ELSE format='(F0.2)'
    latnames = String(lats, FORMAT=format)
    IF Total(lons-Long(lons)) EQ 0 THEN format='(I0)' ELSE format='(F0.2)'
    lonnames = String(lons, FORMAT=format)
    
 END


;+--------------------------------------------------------------------------
;   This method returns information about the current map projection in an 
;   IDL structure variable. Fields of the structure will reflect
;   values that are used in MAP_PROJ_INIT to create a map structure.
;---------------------------------------------------------------------------
FUNCTION cgMap::MapInfo

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN, -1
    ENDIF
   
   map_keywords = Create_Struct( $
       'projection', self._cg_thisProjection.name, $
       'datum', self._cg_thisDatum.name, $
       'gctp', 1, $
       'center_latitude', self._cg_center_latitude, $
       'center_longitude', self._cg_center_longitude )
   IF N_Elements(*self._cg_limit) NE 0 THEN map_keywords = Create_Struct(map_keywords, 'limit', *self._cg_limit)
   IF self._cg_thisProjection.sphereOnly THEN BEGIN
       map_keywords = Create_Struct(map_keywords, 'sphere_radius', self._cg_thisDatum.semimajor_axis)
   ENDIF ELSE BEGIN
       map_keywords = Create_Struct(map_keywords, $
            'semimajor_axis', self._cg_thisDatum.semimajor_axis, $
            'semiminor_axis', self._cg_thisDatum.semiminor_axis)        
   ENDELSE
   IF N_Elements(*self._cg_map_projection_keywords) NE 0 THEN BEGIN
        keywords = *self._cg_map_projection_keywords
        fields = Tag_Names(keywords)
        FOR j=0,N_Elements(fields)-1 DO BEGIN
           map_keywords = Create_Struct(map_keywords, fields[j], keywords.(j))
        ENDFOR
   ENDIF  
   
   ; Add other important information.
   map_keywords = Create_Struct(map_keywords, $
      'x_tie_point_projected_xy_upper_left', self._cg_xrange[0], $
      'y_tie_point_projected_xy_upper_left', self._cg_yrange[1], $      
      'xrange',  self._cg_xrange, 'yrange', self._cg_yrange, $
      'position', self._cg_position, 'map_title', self._cg_title)
 
   RETURN, map_keywords
   
END 


;+--------------------------------------------------------------------------
;   This method allows the user to set various properties of the object. In general,
;   the same keywords that are used for the INIT method can be used here.
;
; :Keywords:
;     background: in, optional, type=string, default='white'
;        The name of the background color. Used only if the map object erases
;        the display when it draws its contents.
;     bcolor: optional, type=string, default='opposite'
;        The name of the color to draw box axes with. Requires BOX_AXES be set.
;     box_axes: in, optional, type=boolean, default=0
;        Set this keyword to draw a box-style grid axes around the map. Applies
;        only if creating a mapGrid object.
;     center_latitude: in, optional, type=float, default=varies
;        The center latitude of the map projection.
;     center_longitude: in, optional, type=float, default=varies
;        The center longitude of the map projection.
;     ccolor: in, optional, type=string, default='charcoal'
;        The name of the drawing color for the MapContinents object if this is requested.
;     color: in, optional, type=string, default='opposite'
;        The name of the drawing color for the object. Passed along to the mapGrid
;        and MapContinents object if these are requested.
;     continents: in, optional, type=boolean, default=0
;        Set this keyword if you wish to create an overlay object of continental outlines
;        that will be rendered when the draw method is called.
;     datum: in, optional, type=string/integer, default='Sphere'
;        This keyword is being depreciated in favor of the keyword ELLIPSOID,
;        corresponding to changes to Map_Proj_Init initiated in IDL 7.
;     draw: in, optional, type=boolean, default=0
;        Set this keyword if you wish to immediately call the DRAW method after the
;        object has been completely initialized.
;     ellipsoid: in, optional, type=string/integer, default='Sphere'
;        Set this to the name or index number of the ellopsoid or datum you wish to use
;        for the map projection. The value is passed directly to Map_Proj_Init.
;     erase: in, optional, type=boolean, default=0
;        Set this keyword if you wish to have the object erase the current graphics display
;        before drawing its content in the DRAW method. The graphics display will be erased
;        in the background color.
;     gcolor: in, optional, type=string, default='gray'
;        The name of the drawing color for the MapGrid object if this is requested.
;     grid: in, optional, type=boolean, default=0
;        Set this keyword if you wish to create an overlay object of map grid lines
;        that will be rendered when the draw method is called.
;     hires: in, optional, type=boolean, default=0
;        Set this keyword if you wish to use high resolution continental outlines.
;        Passed to the MapContinents object if one is requested.
;     latlon_ranges: in, optional, type=boolean, default=0
;        Normally the XRANGE and YRANGE keywords are set in terms of projected meters. If 
;        this keyword is set, then the values of XRANGE and YRANGE are assumed to be in longitude
;        and latitude values, respectively, and will be converted to projected meters prior to 
;        being stored in the object.
;     limit: in, optional, type=FltArr(4), default=none
;        The normal LIMIT keyword to Map_Proj_Init, specifying the limit of the map
;        projection in terms of latitude and longitude. Normally, little used when using
;        Map_Proj_Init. Most work is done by specifying the projected XY rectangular
;        coordinate system with the keywords XRANGE and YRANGE.
;     lcolor: in, optional, type=string
;        Set this to the name of the label color to use in labeling grid lines.
;        By default, the same as COLOR, or if BOX_AXIS is set, then same as BCOLOR.
;     map_projection, in, optional, type=varies
;        The name or index number of a GCTP map projection to use.
;     name: in, optional, type=string, default=selected by cgContainer.
;        Use this keyword to name the object. Names are often used to select objects in 
;        program code. 
;     noborder: in, optional, type=boolean, default=0
;        If this keyword is set, the customary border than surrounds the map projection is
;        not drawn.
;     noforwardfix: in, optional, type=boolean, default=0
;        There is, I believe, a bug in MAP_PROJ_FORWARD that renders longitude values 
;        incorrectly in projected meter space. This is evidenced by MAP_GRID not producing
;        the correct longitude lines in map coordinate systems set up in projected XY meters.
;        In the FORWARD method I correct for this. But, this correction is not always needed
;        or wanted. This property of the object allows me to turn that correction on or off,
;        as needed. Normally, the fix is provided, unless this keyword is set to 1.
;     onimage: in, optional, type=boolean, default=0
;        If this keyword is set, the position of the map projection in the graphics window
;        is obtained from the last image displayed with cgImage. This makes it extremely
;        easy to display an image and immediately set up a map projection space that will
;        allow you to annotate the image using map locations.
;     position: in, optional, type=FltArr(4)
;        The normalized position of the map projection space in the graphics window.
;        The default value is [0.075, 0.075, 0.925, 0.900].
;     radians: in, optional, type=boolean, default=0
;        Set this keyword to indicate latitude and longitude values are in radians rather 
;        than degrees.
;     semimajor_axis: in, optional, type=double, default=varies
;        The length of the semimajor axis of the ellipsoid in meters. Normally calculated
;        from the ELLIPSOID keyword values.
;     semiminor_axis: in, optional, type=double, default=varies
;        The length of the semiminor axis of the ellipsoid in meters. Normally calculated
;        from the ELLIPSOID keyword values.
;     sphere_radius: in, optional, type=double, default=varies
;        The length of the ellipsoidal sphere in meters. Normally calculated from the 
;        ELLIPSOID keyword values.
;     title: in, optional, type=string, default=""
;        The title of the map projection display.
;     uvalue: in, optional, type=any, default=none
;        A storage space for storing any kind of IDL variable of importance to the user.
;     xrange: in, optional, type=various
;        Set this keyword to the X axis range desired in the data coordinate system.
;        Normally expressed in XY projected meter space, unless the LATLON_RANGES 
;        keyword is set. The default is mapStruct.uv_box[[0,2]].
;     yrange: in, optional, type=various
;         Set this keyword to the X axis range desired in the data coordinate system.
;         Normally expressed in XY projected meter space, unless the LATLON_RANGES 
;         keyword is set. The default is mapStruct.uv_box[[1,3]]
;     zone: in, optional, type=integer, default=varies
;         The zone (normally in UTM projections) of the map projection. If not given and needed,
;         calculated from the CENTER_LATITUDE and CENTER_LONGITUDE keyword values.
;---------------------------------------------------------------------------
PRO cgMap::SetProperty, $
    BACKGROUND=background, $
    BCOLOR=bcolor, $
    BOX_AXES=box_axes, $
    CCOLOR=ccolor, $
    CENTER_LATITUDE=center_latitude, $
    CENTER_LONGITUDE=center_longitude, $
    COLOR=color, $
    CONTINENTS=continents, $
    DATUM=datum, $
    DRAW=draw, $
    EASTING=easting, $
    ELLIPSOID=ellipsoid, $
    ERASE=erase, $
    GCOLOR=gcolor, $
    GRID=grid, $
    HIRES=hires, $
    LATLON_RANGES=latlon_ranges, $
    LCOLOR=lcolor, $
    LIMIT=limit, $
    MAP_PROJECTION=map_projection, $
    NAME=name, $
    NOBORDER=noborder, $
    NOFORWARDFIX=noForwardFix, $
    NORTHING=northing, $
    ONIMAGE=onimage, $
    POSITION=position, $
    RADIANS=radians, $
    SEMIMAJOR_AXIS=semimajor_axis, $
    SEMIMINOR_AXIS=semiminor_axis, $
    SPHERE_RADIUS=sphere_radius, $
    TITLE=title, $
    XRANGE=xrange, $
    YRANGE=yrange, $
    UVALUE=uvalue, $
    ZONE=zone, $
    _EXTRA=extraKeywords
    
    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
    ENDIF
      
   ; Are we changing the map projection?
   IF N_Elements(map_projection) NE 0 THEN BEGIN
        projections = *self._cg_theProjections
        IF Size(map_projection, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase(projections.name[*]) EQ StrUpCase(map_projection))
            IF index[0] EQ -1 THEN Message, 'Cannot find map projection ' + map_projection + ' in the projection list.'
        ENDIF
        IF (N_Elements(index) EQ 0) THEN BEGIN
            index = Where(projections.index EQ map_projection, count)
            IF count EQ 0 THEN Message, 'Cannot find map projection index ' + StrTrim(map_projection,2) + ' in projection list.' 
        ENDIF
        self._cg_thisProjection = projections[index]
   ENDIF
   
   ; Are we changing the map datum.
   IF N_Elements(datum) NE 0 THEN BEGIN
        IF Size(datum, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase((*self._cg_theDatums).name) EQ StrUpCase(datum))
            IF index[0] EQ -1 THEN Message, 'Cannot find datum ' + datum + ' in datum list.' 
            thisDatum = (*self._cg_theDatums)[index]
        ENDIF ELSE thisDatum = (*self._cg_theDatums)[0 > datum < 19]
        self._cg_thisDatum = thisDatum
   ENDIF
   
   ; Are we changing the map ellipsoid.
   IF N_Elements(ellipsoid) NE 0 THEN BEGIN
        IF Size(ellipsoid, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase((*self._cg_theDatums).name) EQ StrUpCase(ellipsoid))
            IF index[0] EQ -1 THEN Message, 'Cannot find ellipsoid ' + ellipsoid + ' in datum list.' 
            thisDatum = (*self._cg_theDatums)[index]
        ENDIF ELSE thisDatum = (*self._cg_theDatums)[0 > ellipsoid < 19]
        self._cg_thisDatum = thisDatum
   ENDIF

   ; Are there map projection keywords to deal with?
   IF N_Elements(map_proj_keywords) NE 0 THEN BEGIN
   
        ; Make the pointer a valid pointer, if necessary.
        IF ~Ptr_Valid(self._cg_map_projection_keywords) THEN self._cg_map_projection_keywords = Ptr_New(/ALLOCATE_HEAP)
        
        ; Is there a NULL field in the current structure that means erase what is currently in the pointer?
        index = Where(Tag_Names(map_proj_keywords) EQ 'NULL', count)
        IF count GT 0 THEN BEGIN
            IF map_proj_keywords.(index) EQ 1 THEN self._cg_map_projection_keywords = Ptr_New(/ALLOCATE_HEAP)
        ENDIF
        
        ; Add these fields to the structure, or modify the tag value if it is already present.
        IF N_Elements(*self._cg_map_projection_keywords) GT 0 THEN BEGIN
            ntags = N_Tags(map_proj_keywords)
            tags = Tag_Names(map_proj_keywords)
            FOR j=0,ntags-1 DO BEGIN
               thisTag = tags[j]
               index = Where(Tag_Names(*self._cg_map_projection_keywords) EQ thisTag, count)
               IF count GT 0 THEN BEGIN
                   (*self._cg_map_projection_keywords).(index) = map_proj_keywords.(j)
               ENDIF ELSE BEGIN
                   *self._cg_map_projection_keywords = Create_Struct(*self._cg_map_projection_keywords, thisTag, map_proj_keywords.(j))
               ENDELSE
            ENDFOR
        ENDIF ELSE BEGIN
            
            ; Add all the tags, except for NULL tags
            ntags = N_Tags(map_proj_keywords)
            tags = Tag_Names(map_proj_keywords)
            FOR j=0,ntags-1 DO BEGIN
               thisTag = tags[j]
               IF thisTag EQ 'NULL' THEN Continue
               IF N_Elements(*self._cg_map_projection_keywords) EQ 0 THEN BEGIN
                    count = 0
               ENDIF ELSE BEGIN
                    index = Where(Tag_Names(*self._cg_map_projection_keywords) EQ thisTag, count)
               ENDELSE
               IF count GT 0 THEN BEGIN
                   (*self._cg_map_projection_keywords).(index) = map_proj_keywords.(j)
               ENDIF ELSE BEGIN
                   IF N_Elements(*self._cg_map_projection_keywords) EQ 0 THEN BEGIN
                        *self._cg_map_projection_keywords = Create_Struct(thisTag, map_proj_keywords.(j))
                   ENDIF ELSE BEGIN
                        *self._cg_map_projection_keywords = Create_Struct(*self._cg_map_projection_keywords, $
                            thisTag, map_proj_keywords.(j))
                   ENDELSE
               ENDELSE
            ENDFOR
        ENDELSE
        
        ; For debugging purposes.
        ;Help, *self._cg_map_projection_keywords, /Structure
   ENDIF
   
   IF N_Elements(background) NE 0 THEN self._cg_background = background
   IF N_Elements(color) NE 0 THEN self._cg_color = color
   IF N_Elements(center_latitude) NE 0 THEN self._cg_center_latitude = center_latitude
   IF N_Elements(center_longitude) NE 0 THEN self._cg_center_longitude = center_longitude
   IF N_Elements(easting) NE 0 THEN self._cg_easting = easting
   IF N_Elements(erase) NE 0 THEN self._cg_erase = Keyword_Set(erase)
   IF N_Elements(noborder) NE 0 THEN self._cg_noborder = Keyword_Set(noborder)
   IF N_Elements(noForwardFix) NE 0 THEN self._cg_noForwardFix = Keyword_Set(noForwardFix)
   IF N_Elements(northing) NE 0 THEN self._cg_northing = northing
   IF N_Elements(onimage) NE 0 THEN self._cg_onimage = Keyword_Set(onimage)
   IF N_Elements(radians) NE 0 THEN self._cg_radians = Keyword_Set(radians)
   IF N_Elements(title) NE 0 THEN self._cg_title = title
   
   
   ; If you change the limit, you really also need to change the XRANGE and YRANGE.
   changedLimit = 0
   IF N_Elements(limit) NE 0 THEN BEGIN
        *self._cg_limit = limit
        changedLimit = 1
   ENDIF
   IF N_Elements(sphere_radius) NE 0 THEN BEGIN
      self._cg_thisDatum.semimajor_axis = sphere_radius
      self._cg_thisDatum.semiminor_axis = sphere_radius
   ENDIF
   IF N_Elements(semimajor_axis) NE 0 THEN self._cg_thisDatum.semimajor_axis = semimajor_axis
   IF N_Elements(semiminor_axis) NE 0 THEN self._cg_thisDatum.semiminor_axis = semiminor_axis
   
   ; Make sure the map structure is up to date.
   map_structure = self -> SetMapProjection() 

   IF N_Elements(parent) NE 0 THEN self -> cgCOORD::SetProperty, PARENT=parent
   IF N_Elements(position) NE 0 THEN self -> cgCOORD::SetProperty, POSITION=position
   IF N_Elements(xrange) NE 0 THEN BEGIN
      IF Keyword_Set(latlon_ranges) THEN BEGIN
        uvcoords = Map_Proj_Forward(xrange, [-5000,5000], MAP_STRUCTURE=map_structure)
        xrange = Reform(uvcoords[0,*])   
      ENDIF
      self -> cgCOORD::SetProperty, XRANGE=xrange
   ENDIF ELSE BEGIN
      IF changedLimit THEN BEGIN
            xrange = map_structure.uv_box[[0,2]]
            self -> cgCOORD::SetProperty, XRANGE=xrange
      ENDIF
   ENDELSE
   IF N_Elements(yrange) NE 0 THEN BEGIN
      IF Keyword_Set(latlon_ranges) THEN BEGIN
        uvcoords = Map_Proj_Forward([-5000,5000], yrange, MAP_STRUCTURE=map_structure)
        yrange = Reform(uvcoords[1,*])     
      ENDIF
      self -> cgCOORD::SetProperty, YRANGE=yrange
   ENDIF ELSE BEGIN
      IF changedLimit THEN BEGIN
            yrange = map_structure.uv_box[[1,3]]
            self -> cgCOORD::SetProperty, YRANGE=yrange
      ENDIF
   ENDELSE
   
   ; Do you need overlay objects in this map?
   IF N_Elements(continents) NE 0 THEN BEGIN
   
       ; Is this a continental object? If so add it, if not
       ; create one and add it.
       IF Obj_Valid(continents) THEN BEGIN
           continents -> SetProperty, NAME='MAPCONTINENTS'
           self._cg_overlays -> Add, continents
       ENDIF ELSE BEGIN
           continents = Obj_New('cgMapContinents', self, /COUNTRIES, $
               COLOR=self._cg_color, NAME='MAPCONTINENTS')
           self._cg_overlays -> Add, continents
       ENDELSE
   ENDIF
   
   IF N_Elements(grid) NE 0 THEN BEGIN
   
       ; Is this a grid object? If so add it, if not
       ; create one and add it.
       IF Obj_Valid(grid) THEN BEGIN
           grid -> SetProperty, NAME='MAPGRID'
           self._cg_overlays -> Add, grid
       ENDIF ELSE BEGIN
           grid = Obj_New('cgMapGrid', self, /AUTODRAW, COLOR=self._cg_color, NAME='MAPGRID')
           self._cg_overlays -> Add, grid
       ENDELSE
   ENDIF
   
   IF (N_ELEMENTS(extraKeywords) GT 0) THEN self -> cgCOORD::SetProperty,  _EXTRA=extraKeywords
   
   ; Need to draw after setting properties?
   IF Keyword_Set(draw) THEN self -> Draw
   
END 


;+--------------------------------------------------------------------------
; :Private:
;   This method calls MAP_PROJ_INIT to create a map structure variable.
;   This method should not be called directly. Use GetMapStruct() instead.
;
;---------------------------------------------------------------------------
FUNCTION cgMap::SetMapProjection, map_projection, $
    LATLON_RANGES=latlon_ranges, $
    POSITION=position, $
    XRANGE=xrange, $
    YRANGE=yrange, $
    ; MAP_PROJ_INIT keywords (partial list)
    CENTER_LATITUDE=center_latitude, $
    CENTER_LONGITUDE=center_longitude, $
    DATUM=datum, $
    EASTING=easting, $
    ELLIPSOID=ellipsoid, $
    LIMIT=limit, $
    NORTHING=northing, $
    RADIANS=radians, $
    SEMIMAJOR_AXIS=semimajor_axis, $
    SEMIMINOR_AXIS=semiminor_axis, $
    SPHERE_RADIUS=sphere_radius, $
    ZONE=zone, $
    _EXTRA=extraKeywords

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN, 1
    ENDIF
   
   ; Need a new map projection?
   IF N_Elements(map_projection) NE 0 THEN BEGIN
        ; Find the map projection.
        IF Size(map_projection, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase((*self._cg_theProjections).name) EQ StrUpCase(map_projection))
            IF index[0] EQ -1 THEN Message, 'Cannot find map projection ' + map_projection + ' in the projection list.'
        ENDIF
        IF (N_Elements(index) EQ 0) AND (N_Elements(map_projection) NE 0) THEN BEGIN
            index = Where((*self._cg_theProjections).index EQ map_projection, count)
            IF count EQ 0 THEN Message, 'Cannot find map projection index ' + StrTrim(map_projection,2) + ' in projection list.' 
        ENDIF 
        thisProjection = (*self._cg_theProjections)[index]
        self._cg_thisProjection = thisProjection
   ENDIF
   
   ; Need a new datum?
   IF N_Elements(datum) NE 0 THEN BEGIN
        IF Size(datum, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase((*self._cg_theDatums).name) EQ StrUpCase(datum))
            IF index[0] EQ -1 THEN Message, 'Cannot find datum ' + datum + ' in datum list.' 
            thisDatum = (*self._cg_theDatums)[index]
        ENDIF ELSE thisDatum = (*self._cg_theDatums)[0 > datum < 24]
        self._cg_thisDatum = thisDatum
   ENDIF

   ; Need a new ellipsoid?
   IF N_Elements(ellipsoid) NE 0 THEN BEGIN
        IF Size(ellipsoid, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase((*self._cg_theDatums).name) EQ StrUpCase(ellipsoid))
            IF index[0] EQ -1 THEN Message, 'Cannot find ellipsoid ' + ellipsoid + ' in datum list.' 
            thisDatum = (*self._cg_theDatums)[index]
        ENDIF ELSE thisDatum = (*self._cg_theDatums)[0 > ellipsoid < 24]
        self._cg_thisDatum = thisDatum
   ENDIF
   
   ; Other map keywords?
   IF N_Elements(center_latitude) NE 0 THEN self._cg_center_latitude = center_latitude
   IF N_Elements(center_longitude) NE 0 THEN self._cg_center_longitude = center_longitude
   IF N_Elements(easting) NE 0 THEN self._cg_easting = easting
   IF N_Elements(northing) NE 0 THEN self._cg_northing = northing
   IF N_Elements(sphere_radius) NE 0 THEN BEGIN
      self._cg_thisDatum.semimajor_axis = sphere_radius
      self._cg_thisDatum.semiminor_axis = sphere_radius
   ENDIF
   IF N_Elements(semimajor_axis) NE 0 THEN self._cg_thisDatum.semimajor_axis = semimajor_axis
   IF N_Elements(semiminor_axis) NE 0 THEN self._cg_thisDatum.semiminor_axis = semiminor_axis
   IF N_Elements(limit) NE 0 THEN *self._cg_limit = limit
   IF N_Elements(zone) NE 0 THEN self._cg_zone = zone
   IF N_Elements(extrakeywords) NE 0 THEN *self._cg_map_projection_keywords = extrakeywords
   
   ; Extract the values you need to call MAP_PROJ_INIT.
   thisProjection = self._cg_thisProjection.name
   sphereOnly = self._cg_thisProjection.sphereOnly
   thisDatum = self._cg_thisDatum.name
   radians = self._cg_radians
   semimajor_axis = self._cg_thisDatum.semimajor_axis 
   semiminor_axis = self._cg_thisDatum.semiminor_axis
   center_lon = self._cg_center_longitude
   center_lat = self._cg_center_latitude
   easting = self._cg_easting
   northing = self._cg_northing
   IF N_Elements(*self._cg_limit) NE 0 THEN limit = *self._cg_limit
   zone = self._cg_zone
   IF N_Elements(*self._cg_map_projection_keywords) NE 0 THEN keywords = *self._cg_map_projection_keywords
   
   ; Center latitudes are not allowed in some projections. Here are the ones where
   ; they are prohibited.
   centerlatOK = 1
   centerlonOK = 1
   badprojLatstr = ['GOODES HOMOLOSINE', 'STATE PLANE', 'MERCATOR', 'SINUSOIDAL', 'EQUIRECTANGULAR', $
      'MILLER CYLINDRICAL', 'ROBINSON', 'SPACE OBLIQUE MERCATOR A', 'SPACE OBLIQUE MERCATOR B', $
      'ALASKA CONFORMAL', 'INTERRUPTED GOODE', 'MOLLWEIDE', 'INTERRUPED MOLLWEIDE', 'HAMMER', $
      'WAGNER IV', 'WAGNER VII', 'INTEGERIZED SINUSOIDAL']
   void = Where(badprojLatstr EQ StrUpCase(thisProjection), count)
   IF count GT 0 THEN centerlatOK = 0

   badprojLonstr = ['HOTINE OBLIQUE MERCATOR A','HOTINE OBLIQUE MERCATOR B']
   void = Where(badprojLonstr EQ StrUpCase(thisProjection), count)
   IF count GT 0 THEN centerLonOK = 0
    
    ; UTM and State Plane projections have to be handled differently.
    IF (StrUpCase(thisProjection) EQ 'UTM') OR (StrUpCase(thisProjection) EQ 'STATE PLANE') THEN BEGIN
    
        CASE StrUpCase(thisProjection) OF
            'UTM': BEGIN
                IF N_Elements(zone) NE 0 THEN BEGIN
                    Undefine, center_lat
                    Undefine, center_lon
                ENDIF
                mapStruct = Map_Proj_Init(thisProjection, DATUM=self._cg_thisDatum.(0), /GCTP, $
                    CENTER_LATITUDE=center_lat, CENTER_LONGITUDE=center_lon, $
                    RADIANS=radians, ZONE=zone, LIMIT=limit)
                END
            'STATE PLANE': BEGIN
                mapStruct = Map_Proj_Init(thisProjection, DATUM=self._cg_thisDatum.(0), /GCTP, $
                    RADIANS=radians, ZONE=zone, LIMIT=limit)
                END
        ENDCASE
        
    ENDIF ELSE BEGIN

        ; Call MAP_PROJ_INIT to get the map projection structure.
        CASE 1 OF
        
            centerLatOK && centerLonOK && sphereOnly: BEGIN
                mapStruct = Map_Proj_Init(thisProjection, /GCTP, $
                    CENTER_LATITUDE=center_lat, $
                    CENTER_LONGITUDE=center_lon, $
                    SPHERE_RADIUS=semimajor_axis, $
                    LIMIT=limit, RADIANS=radians, $
                    _EXTRA=keywords, $
                    FALSE_NORTHING=northing, FALSE_EASTING=easting)
                END
                
            ~centerLatOK && centerLonOK && sphereOnly: BEGIN

                mapStruct = Map_Proj_Init(thisProjection, /GCTP, $
                    CENTER_LONGITUDE=center_lon, $
                    SPHERE_RADIUS=semimajor_axis, $
                    LIMIT=limit, RADIANS=radians, $
                    _EXTRA=keywords, $
                    FALSE_NORTHING=northing, FALSE_EASTING=easting)
                END
                
            ~centerLatOK && centerLonOK &&  ~sphereOnly: BEGIN
                mapStruct = Map_Proj_Init(thisProjection, /GCTP, $
                    CENTER_LONGITUDE=center_lon, $
                    SEMIMAJOR_AXIS=semimajor_axis, $
                    SEMIMINOR_AXIS=semiminor_axis, $
                    LIMIT=limit, RADIANS=radians, $
                    _EXTRA=keywords, $
                    FALSE_NORTHING=northing, FALSE_EASTING=easting)
                END
    
            centerLatOK && centerLonOK && ~sphereOnly: BEGIN
                mapStruct = Map_Proj_Init(thisProjection, /GCTP, $
                    CENTER_LATITUDE=center_lat, $
                    CENTER_LONGITUDE=center_lon, $
                    SEMIMAJOR_AXIS=semimajor_axis, $
                    SEMIMINOR_AXIS=semiminor_axis, $
                    LIMIT=limit, RADIANS=radians, $
                    _EXTRA=keywords, $
                    FALSE_NORTHING=northing, FALSE_EASTING=easting)
                END
                
            centerLatOK && ~centerLonOK && ~sphereOnly: BEGIN
                mapStruct = Map_Proj_Init(thisProjection, /GCTP, $
                    CENTER_LATITUDE=center_lat, $
                    SEMIMAJOR_AXIS=semimajor_axis, $
                    SEMIMINOR_AXIS=semiminor_axis, $
                    LIMIT=limit, RADIANS=radians, $
                    _EXTRA=keywords, $
                    FALSE_NORTHING=northing, FALSE_EASTING=easting)
                END
        ENDCASE
   ENDELSE

   ; The UV_BOX created in Map_Proj_Init will return the wrong values if the CENTER_LON is
   ; not equal to zero and the range is the full 360 degrees. In this case, UV_BOX[0] is
   ; equal to UV_BOX[2]. While this is technically correct, it doesn't work for displaying
   ; grids on maps set up with a projected XY coordinate system. In this grid system, these
   ; two points are not coincident, but are far apart on the plot. This code tries to fix
   ; the UV_BOX in this case. See http://www.idlcoyote.com/map_tips/uvrange.php for details.
   IF N_Elements(center_lon) NE 0 THEN BEGIN
       IF (center_lon NE 0.0) && $
           ( (N_Elements(limit) EQ 0) || (limit[2]-limit[0] EQ 180) || (limit[3]-limit[1] EQ 360) ) THEN BEGIN
            IF mapStruct.uv_box[0] GE 0 THEN BEGIN
               mapStruct.uv_box[2] = -Abs(mapStruct.uv_box[0])
            ENDIF ELSE BEGIN
               mapStruct.uv_box[2] = Abs(mapStruct.uv_box[0])
            ENDELSE
            IF mapStruct.uv_box[1] GE 0 THEN BEGIN
               mapStruct.uv_box[3] = -Abs(mapStruct.uv_box[1])
            ENDIF ELSE BEGIN
               mapStruct.uv_box[3] = Abs(mapStruct.uv_box[1])
            ENDELSE
       ENDIF
   ENDIF
   
   RETURN, mapStruct
    
END 


;+--------------------------------------------------------------------------
;   This is the clean-up routine for the object.
;
;---------------------------------------------------------------------------
PRO cgMap::CLEANUP

    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
    ENDIF
   
   Ptr_Free, self._cg_limit
   Ptr_Free, self._cg_map_projection_keywords
   Ptr_Free, self._cg_theDatums
   Ptr_Free, self._cg_theProjections
   Obj_Destroy, self._cg_overlays
   
   self -> cgCOORD::CLEANUP 

END


;+--------------------------------------------------------------------------
;   This is the class definition module. Structures used to manipulate
;   map projectatum information are also created here.
;
; :Params:
;    class: out, optional, type=structure
;       Occasionally, it is useful to have an object class definition as
;       a structure variable. Using this output keyword will allow that.
;       
;---------------------------------------------------------------------------
PRO cgMap__Define, class

   ; Structures used in the object.
   datumStruct = { cgMap_DATUM, index:0, name:"", semimajor_axis:0.0D, semiminor_axis:0.0D }
   mapStruct =   { cgMap_PROJECTION, name:"", index:0, sphereOnly:0 }

   class = { cgMap, $
             _cg_isotropic: 0B, $
             _cg_aspect: 0.0D, $
             _cg_background: "", $                      ; The background color for erasing the display.
             _cg_center_latitude: 0.0D, $               ; The latitude at the center of the map projection.
             _cg_center_longitude:0.0D, $               ; The lontigude at the center of the map projection.
             _cg_color: "", $                           ; The name of the color in which to draw a border.
             _cg_easting: 0.0D, $                       ; The easting value in meters.
             _cg_erase: 0B, $                           ; A flag that indicates the map should erase the display before drawing.
             _cg_limit: Ptr_New(), $                    ; The limit of the map projection.
             _cg_map_projection_keywords: Ptr_New(), $  ; A storage location for MAP_PROJ_INIT keywords.
             _cg_noborder: 0B, $                        ; A flag that indicates a border should not be drawn.
             _cg_noforwardfix: 0B, $                    ; A flag that indicates no fix in the FORWARD method.
             _cg_northing: 0.0D, $
             _cg_multi_position: FltArr(4), $           ; The position of a multiple plot.
             _cg_onimage: 0B, $                         ; A flag that, if set, will get the position from the last image position.
             _cg_overlays: Obj_New(), $                 ; A storage location for map overlays.
             _cg_radians: 0B, $                         ; A flag that indicated values are in radians, not degrees.
             _cg_theDatums: Ptr_New(), $                ; Information about available map datums.
             _cg_thisDatum: datumStruct, $              ; The particular datum structure for this map projection.
             _cg_theProjections: Ptr_New(), $           ; Information about available map projections.
             _cg_thisProjection: mapStruct, $           ; The particular map projection structure for this map projection.
             _cg_title: "", $                           ; The map title.
             _cg_zone: 0, $                             ; The UTM zone of the map projection.
             INHERITS cgCOORD $                         ; The superclass object.
           }

END ;--------------------------------------------------------------------------


