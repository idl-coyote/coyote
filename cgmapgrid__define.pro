;+
; NAME:
;       cgMapGrid__DEFINE
;
; PURPOSE:
;
;       This object is a wrapper for a modified Map_Grid routine in IDL. It provides a simple 
;       way to allow map grids on images which use a cgMAPCOORD object to set up the map 
;       projection space. A map coordinate space must be in effect at the time the 
;       Draw method of this object is used. 
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:
;
;       Graphics
;
; CALLING SEQUENCE:
;
;       mapCoord = Obj_New('cgMapCoord', 111, CENTER_LON=0, CENTER_LAT=90, LIMIT=[45, -180, 90, 180])
;       gridObject = Obj_New('cgMapGrid', mapCoord)
;       gridObject -> Draw
;       Obj_Destroy, mapCoord, gridObject
;       
; AUGUMENTS:
; 
;       mapCoordObj:   A map coordinate object which can return a map structure for converting coordinates
;                      to/from XY coordinates to lon/lat coordinates. Typically, a cgMAPCOORD object. Note, 
;                      this object is *not* destroyed when the cgMapGrid object is destroyed. You are 
;                      responsible for destroying the map coordinate object.
;
; KEYWORDS:
; 
;  All of the following INIT keywords can be set and obtained using the SETPROPERTY and GETPROPERTY methods.
;  
;      AUTODRAWGRID:    If this keyword is set, the grid latitude and longitude values
;                       are automatically calculated from the Map_Object ranges and drawn
;                       appropriately. Most keywords are ignored when auto drawing the grid.
; 
;      BOX_AXES:        Set this keyword to draw a box-style axes around the map.
;      
;      CHARSIZE:        Set this keyword to the size of characters used for the labels. Default is 1.0.
;      
;      CLIP_TEXT:       Set this keyword to a zero value to turn off clipping of text labels. 
;                       By default, text labels are clipped. This keyword is ignored if the 
;                       BOX_AXES keyword is set. 
;                      
;      COLOR:           The name of the color to draw the grid lines in. Default: "opposite".
;      
;      FILL_HORIZON:    Set this keyword to fill the current map horizon.
;      
;      FIXED_MAP_GRID:  The cgMapGrid command supplied with IDL does not always draw the grids
;                       correctly. This can sometimes be fixed by using a modified version of
;                       cgMapGrid, named FIXED_MAP_GRID. Unfortunately, the FIXED_MAP_GRID command
;                       caused other problem with other applications. Thus, if you are having
;                       map grid problems, this is something to try. The FIXED_MAP_GRID program
;                       is in the Coyote Library.
;                     
;      FORMAT:          Set this keyword to a particular string format for formatting
;                       the grid labels.
;      
;      HORIZON:         Set this keyword to draw the current map horizon.
;      
;      INCREMENT:       Set this keyword to the spacing between the graticle points.
;      
;      LABEL:           Set this keyword to an integer, n, that labels every n parallels and meridians.
;                       For example, LABEL=3 will label every 3rd line. Default is 1.
;                       
;      LATALIGN:        This keyword controls the alignment of the text baseline for latitude 
;                       labels. A value of 0.0 left justifies the label, 1.0 right justifies 
;                       it, and 0.5 centers it. This keyword is ignored if the BOX_AXES keyword is set.
;                      
;      LATDEL:          Set this keyword equal to the spacing (in degrees) between parallels of 
;                       latitude in the grid. If this keyword is not set, a default value of 5 is used.
;                       
;      LATLAB:          The longitude at which to place latitude labels. The default is the center 
;                       longitude on the map. This keyword is ignored if the BOX_AXES keyword is set.
;                       
;      LATNAMES:        Set this keyword equal to an array specifying the names to be used for the 
;                       latitude labels. By default, this array is automatically generated in units 
;                       of degrees. The LATNAMES array can be either type string or any single numeric 
;                       type, but should not be of mixed type.When LATNAMES is specified, the LATS 
;                       keyword must also be specified.
;      
;      LATS:            Set this keyword equal to a one or more element vector of latitudes for which 
;                       lines will be drawn (and optionally labeled). If LATS is omitted, appropriate 
;                       latitudes will be generated based on the value of the (optional) LATDEL keyword. 
;                       If LATS is set to a single value, that latitude and a series of automatically 
;                       generated latitudes will be drawn (and optionally labeled).
;      
;      LINESTYLE:       Set this keyword to the type of linestyle desired. See Graphics Keywords in
;                       the on-line help for additional information.
; 
;      LONALIGN:        This keyword controls the alignment of the text baseline for longitude 
;                       labels. A value of 0.0 left justifies the label, 1.0 right justifies 
;                       it, and 0.5 centers it. This keyword is ignored if the BOX_AXES keyword is set.
;                      
;      LONDEL:          Set this keyword equal to the spacing (in degrees) between parallels of 
;                       longitude in the grid. If this keyword is not set, a default value of 10 is used.
;                       
;      LONTLAB:         The latitude at which to place longitude labels. The default is the center 
;                       latitude on the map. This keyword is ignored if the BOX_AXES keyword is set.
;                       
;      LONNAMES:        Set this keyword equal to an array specifying the names to be used for the 
;                       longitude labels. By default, this array is automatically generated in units 
;                       of degrees. The LONNAMES array can be either type string or any single numeric 
;                       type, but should not be of mixed type.When LONNAMES is specified, the LONS 
;                       keyword must also be specified.
;                       
;      LONS:            Set this keyword equal to a one or more element vector of longitudes for which 
;                       lines will be drawn (and optionally labeled). If LONS is omitted, appropriate 
;                       longitudes will be generated based on the value of the (optional) LONDEL keyword. 
;                       If LONS is set to a single value, that longitudes and a series of automatically 
;                       generated longitudes will be drawn (and optionally labeled).
;                       
; DEPENDENCIES:
;
;       The program requires other programs from the Coyote Library:
;
;          http://www.dfanning.com/documents/programs.php

; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 3 January 2009.
;       Added MAP_OBJECT keyword and depreciated MAP_STRUCTURE keyword. 30 May 2009. DWF.
;       Fixed a problem in setting MAP_OBJECT in SetProperty method. 12 June 2009. DWF.
;       Circular parent references when passed a MAP_OBJECT was fixed, preventing memory
;          leakage. 30 August 2009. DWF.
;       Removed the MAP_STRUCTURE keyword, which caused MASSIVE problems and added a mapCoordObj
;          parameter. 9 November 2009. DWF.
;       Changed the default LATDEL keyword to 5 and the LONDEL keyword to 10. 13 December 2009. DWF.
;       Changed GetProperty keywords LATS and LONS to return data, not pointers to data. 16 December 2009. DWF.
;       I had to incorporate a "fixed" version of the IDL-supplied routine cgMapGrid to work
;          around a bug with MAP_PROJ_FORWARD (IDL version 7.1.2) in which some grid points are
;          simply dropped. The fixed version allows me to draw accurate grid lines. 16 December 2009. DWF.
;       Removed the "fixed" version of the IDL-supplied routine cgMapGrid from this code and
;          made it a separate file named FIXED_cgMapGrid. Unfortunately, the "fixed" program
;          was causing me other problems some map gridding applications. This will now allow
;          the user to choose whether to use the "fixed" (and I really mean that to be in quotes!)
;          or non-fixed version of the software, depending upon how it works for you. 20 Feb 2010. DWF.
;       Added AUTODRAWGRID keyword. 11 March 2010. DWF.
;       Changed default CHARSIZE to 0.75 for Windows machines. 11 March 2010. DWF.
;       Changed calls to IDL's cgMapGrid routine to my MODIFIED_cgMapGrid routine, which
;           has been modified to fix a problem in which hardware fonts are not clipped
;           to the plot region. 25 March 2010. DWF.
;       I've rearranged the code that does the AUTODRAWGRID mode, and I've tweaked the algorithms
;           to respond to several weird cases I've run into lately. Now the AUTODRAWGRID mode sets
;           the LATS, LONS, LATNAMES, LONNAMES, LATLABEL and LONLABEL properties of the object.
;           All drawing is done in the DRAW method. 21 June 2010. DWF.
;       More tweaking of the AUTODRAWGRID algorithm. 5 July 2010. DWF.
;       Added a FORMAT keyword to format the grid labels, which required modifications
;           to Catalyst utility routines Fixed_cgMapGrid and Modfied_cgMapGrid. 5 July 2010. DWF.
;       Converted to Coyote Graphics routine 2 Nov 2011. DWF.
;-
;
;******************************************************************************************;
;  Copyright (c) 2009-2010, by Fanning Software Consulting, Inc.                           ;
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
PRO cgMapGrid::AddCmd, REPLACE=replace

   currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
   IF wincnt EQ 0 THEN cgWindow
   IF Keyword_Set(replace) $
      THEN cgWindow, "Draw", self, /Method, /ReplaceCmd $ ; Replace all commands in the window
      ELSE cgWindow, "Draw", self, /Method, /AddCmd       ; Add this command to the window.
   
END ;--------------------------------------------------------------------------


PRO cgMapGrid::AutoDrawGrid, SUCCESS=success

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        seccess = 0
        RETURN
    ENDIF
    
    ; Assume success.
    success = 1
    
    ; The longitudes might be calculated from the results of the latitude calculation.
    ; If they are, this flag will be set to 1.
    lonsdone = 0
    latsdone = 0
    
    ; Get the ranges of the map coordinate object.
    IF ~Obj_Valid(self._cg_map_object) THEN Message, 'A valid map object is required.'
    self._cg_map_object -> GetProperty, XRANGE=xrange, YRANGE=yrange
        
    ; Sample XY grid at 625 locations throughout the grid (25x25).
    xstep = (xrange[1] - xrange[0]) / 24.0
    ystep = (yrange[1] - yrange[0]) / 24.0
    xvec = (Findgen(25) * xstep) + xrange[0]
    yvec = (Findgen(25) * ystep) + yrange[0]
    xarr = Rebin(xvec, 25, 25)
    yarr = Rebin(Reform(Reverse(yvec), 1, 25), 25, 25)
    
    ; Find the latitude/longitude of these locations. Find the min, max,
    ; and lat/lon at the center of the grid.
    ll = Map_Proj_Inverse(xarr, yarr, MAP_STRUCTURE=self._cg_map_object->GetMapStruct())
    latlon = Reform(ll, 2, 25, 25)
    latlon = Transpose(latlon, [1,2,0])
    latitudes = latlon[*,*,1]
    longitudes = latlon[*,*,0]
    lon_min = Min(longitudes, MAX=lon_max, /NAN)
    lat_min = Min(latitudes, MAX=lat_max, /NAN)
    center_lat = latitudes[12,12]
    center_lon = longitudes[12,12]
    
    ; We are going to try to have seven lines running through the grid space.
    ; We will have special rules if the center latitude is at the pole.
    latstep = (lat_max - lat_min) / 6.0
    lonstep = (lon_max - lon_min) / 6.0
    IF (center_lat GT (90.-0.05)) AND (center_lat LT (90.0 + 0.05)) THEN BEGIN
       lats = Scale_Vector(Indgen(5), 0 > Round(lat_min), 80) 
       latsdone = 1 
       IF lonstep GT 40 THEN BEGIN
          lons = Findgen(11) * 36
          lonsDone = 1
       ENDIF ELSE BEGIN
          lonsDone = 0
       ENDELSE     
    ENDIF ELSE BEGIN
    
       IF (center_lat LT (-90.+0.05)) AND (center_lat GT (-90.0 - 0.05)) THEN BEGIN
           lats = Scale_Vector(Indgen(5), -80, 0 < Round(lat_max))  
           latsdone = 1    
           IF lonstep GT 40 THEN BEGIN
              lons = Findgen(11) * 36
              lonsDone = 1
           ENDIF ELSE BEGIN
              lonsDone = 0
           ENDELSE    
       ENDIF
        
    ENDELSE
    
    IF latsdone EQ 0 THEN lats = -90.0 > [(Indgen(3)+1)*(-latstep) + center_lat, center_lat,  $
                       (Indgen(3)+1)*(latstep) + center_lat] < 90.0
        
    IF lonsDone EQ 0 THEN BEGIN
            lons = -180.0 > [(Indgen(3)+1)*(-lonstep) + center_lon, center_lon,  $
                             (Indgen(3)+1)*( lonstep) + center_lon] < 360.0
    ENDIF
        
    ; The values might need to be sorted.
    lats = lats[Sort(lats)]
    lons = lons[Sort(lons)]
        
    ; Labels should be between the 3nd and 4th grid lines.
    latlab = (lons[3] - lons[2]) / 2.0 + lons[2]
    lonlab = (lats[3] - lats[2]) / 2.0 + lats[2]
    
    ; Check the spacing between latitude and longitude lines. Depending
    ; on the spacing, values can be rounded and names created.
    nlats = N_Elements(lats)
    latspace = Abs(lats[nlats-1] - lats[0])
    IF latspace GT 15 THEN lats = Round(lats)
    nlons = N_Elements(lons)
    lonspace = Abs(lons[nlons-1] - lons[0])
    IF lonspace GT 15 THEN lons = Round(lons)
    
       
    ; Set the properties of the object.
    self -> SetProperty, LATLAB=latlab, LATS=lats, $
                         LONLAB=lonlab, LONS=lons
                             
END ; -------------------------------------------------------------------------------------


PRO cgMapGrid::Draw, _EXTRA=extrakeywords

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Find a map structure, if you can.
    IF Obj_Valid(self._cg_map_object) THEN BEGIN
        mapStruct = self._cg_map_object -> GetMapStruct() 
    ENDIF
        
    ; This can cause all kinds of error messages from MAP_PROJ_FORWARD. Turn them all off.
    except = !Except
    !Except = 0
    
    ; If you are auto drawing grids, set up the lats and lons.
    IF self._cg_autodrawgrid THEN BEGIN
        self -> AutoDrawGrid, SUCCESS=success
        void = Check_Math()
        !Except = except
        IF ~success THEN RETURN
    ENDIF
    
    ; Draw the map grid.
    cg_Map_Grid, $
       BOX_AXES=self._cg_box_axes, $
       CLIP_TEXT=1, $
       CHARSIZE=self._cg_charsize, $
       COLOR=cgColor(self._cg_color), $
       FILL_HORIZON=self._cg_fill_horizon, $
       GLINESTYLE=self._cg_linestyle, $
       GLINETHICK=self._cg_thick, $
       FORMAT=self._cg_format, $
       HORIZON=self._cg_horizon, $
       INCREMENT=*self._cg_increment, $
       LABEL=self._cg_label, $
       LATDEL=*self._cg_latdel, $
       LATLAB=*self._cg_latlab, $
       LATNAMES=*self._cg_latnames, $
       LATS=*self._cg_lats, $
       LONDEL=*self._cg_londel, $
       LONLAB=*self._cg_lonlab, $
       LONNAMES=*self._cg_lonnames, $
       LONS=*self._cg_lons, $
       MAP_STRUCTURE=mapStruct

    
    ; Turn messages back on.
    void = Check_Math()
    !Except = except
    
END ; -------------------------------------------------------------------------------------


PRO cgMapGrid::GetProperty, $
    AUTODRAWGRID=autodrawgrid, $
    BOX_AXES=box_axes, $
    CLIP_TEXT=clip_text, $
    CHARSIZE=charsize, $
    COLOR=color, $
    FILL_HORIZON=fill_horizon, $
    FIXED_cgMapGrid=fixed_map_grid, $
    FORMAT=format, $
    LINESTYLE=linestyle, $
    THICK=thick, $
    HORIZON=horizon, $
    INCREMENT=increment, $
    LABEL=label, $
    LATDEL=latdel, $
    LATLAB=latlab, $
    LATNAMES=latnames, $
    LATS=lats, $
    LONDEL=londel, $
    LONLAB=lonlab, $
    LONNAMES=lonnames, $
    LONS=lons, $
    MAP_OBJECT=map_object, $
    MAP_STRUCTURE=map_structure, $
    _REF_EXTRA=extra

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    autodrawgrid = self._cg_autodrawgrid
    box_axes = self._cg_box_axes
    clip_text = self._cg_clip_text
    charsize = self._cg_charsize
    color = self._cg_color
    fill_horizon = self._cg_fill_horizon
    fixed_map_grid = self._cg_fixed_map_grid
    format = self._cg_format
    linestyle = self._cg_linestyle
    thick = self._cg_thick
    horizon = self._cg_horizon
    increment = self._cg_increment
    IF Ptr_Valid(self._cg_label) THEN label = self._cg_label
    IF N_Elements(*self._cg_latdel) NE 0 THEN latdel = *self._cg_latdel
    IF N_Elements(*self._cg_latlab) NE 0 THEN latlab = *self._cg_latlab
    IF N_Elements(*self._cg_latnames) NE 0 THEN latnames = *self._cg_latnames
    IF N_Elements(*self._cg_lats) NE 0 THEN lats = *self._cg_lats
    IF N_Elements(*self._cg_londel) NE 0 THEN londel = *self._cg_londel
    IF N_Elements(*self._cg_lonlab) NE 0 THEN lonlab = *self._cg_lonlab
    IF N_Elements(*self._cg_lonnames) NE 0 THEN lonnames = *self._cg_lonnames
    IF N_Elements(*self._cg_lons) NE 0 THEN lons = *self._cg_lons
    map_object = self._cg_map_object
    IF Arg_Present(map_structure) THEN map_structure = self._cg_map_object -> GetMapStruct()
    
    IF N_Elements(extra) NE 0 THEN self -> cgContainer::GetProperty, _EXTRA=extra
    
END ; -------------------------------------------------------------------------------------

    
PRO cgMapGrid::SetProperty, $
    AUTODRAWGRID=autodrawgrid, $
    BOX_AXES=box_axes, $
    CLIP_TEXT=clip_text, $
    CHARSIZE=charsize, $
    COLOR=color, $
    FILL_HORIZON=fill_horizon, $
    FIXED_cgMapGrid=fixed_map_grid, $
    FORMAT=format, $
    LINESTYLE=linestyle, $
    THICK=thick, $
    HORIZON=horizon, $
    INCREMENT=increment, $
    LABEL=label, $
    LATDEL=latdel, $
    LATLAB=latlab, $
    LATNAMES=latnames, $
    LATS=lats, $
    LONDEL=londel, $
    LONLAB=lonlab, $
    LONNAMES=lonnames, $
    LONS=lons, $
    MAP_OBJECT=map_object, $
    _EXTRA=extra

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    IF N_Elements(autodrawgrid) NE 0 THEN self._cg_autodrawgrid = Keyword_Set(autodrawgrid)
    IF N_Elements(box_axes) NE 0 THEN self._cg_box_axes = Keyword_Set(box_axes)
    IF N_Elements(clip_text) NE 0 THEN self._cg_clip_text = clip_text
    IF N_Elements(charsize) NE 0 THEN self._cg_charsize = charsize
    IF N_Elements(color) NE 0 THEN self._cg_color = color
    IF N_Elements(fill_horizon) NE 0 THEN self._cg_fill_horizon = Keyword_Set(fill_horizon)
    IF N_Elements(fixed_map_grid) NE 0 THEN self._cg_fixed_map_grid = Keyword_Set(fixed_map_grid)
    IF N_Elements(format) NE 0 THEN self._cg_format = format
    IF N_Elements(linestyle) NE 0 THEN self._cg_linestyle = linestyle
    IF N_Elements(thick) NE 0 THEN self._cg_thick = thick
    IF N_Elements(horizon) NE 0 THEN self._cg_horizon = Keyword_Set(horizon)
    IF N_Elements(increment) NE 0 THEN *self._cg_increment = increment
    IF N_Elements(label) NE 0 THEN self._cg_label = label
    IF N_Elements(latdel) NE 0 THEN *self._cg_latdel = latdel
    IF N_Elements(latlab) NE 0 THEN *self._cg_latlab = latlab
    IF N_Elements(latnames) NE 0 THEN *self._cg_latnames = latnames
    IF N_Elements(lats) NE 0 THEN *self._cg_lats = lats
    IF N_Elements(londel) NE 0 THEN *self._cg_londel = londel
    IF N_Elements(lonlab) NE 0 THEN *self._cg_lonlab = lonlab
    IF N_Elements(lonnames) NE 0 THEN *self._cg_lonnames = lonnames
    IF N_Elements(lons) NE 0 THEN *self._cg_lons = lons
    IF N_Elements(map_object) NE 0 THEN self._cg_map_object = map_object

    IF N_Elements(extra) NE 0 THEN self -> cgContainer::SetProperty, _EXTRA=extra

END ; -------------------------------------------------------------------------------------


PRO cgMapGrid::CLEANUP

    ; Destroy object pointers.
    Ptr_Free, self._cg_increment
    Ptr_Free, self._cg_latdel
    Ptr_Free, self._cg_latlab
    Ptr_Free, self._cg_latnames
    Ptr_Free, self._cg_lats
    Ptr_Free, self._cg_londel
    Ptr_Free, self._cg_lonlab
    Ptr_Free, self._cg_lonnames
    Ptr_Free, self._cg_lons
    
    ; Call the superclass cleanup or memory leaks will occur.
    self -> cgContainer::CLEANUP
END ; -------------------------------------------------------------------------------------


FUNCTION cgMapGrid::INIT, mapCoordObj, $
    ADDCMD=addcmd, $
    AUTODRAWGRID=autodrawgrid, $
    BOX_AXES=box_axes, $
    CLIP_TEXT=clip_text, $
    CHARSIZE=charsize, $
    COLOR=color, $
    FILL_HORIZON=fill_horizon, $
    FORMAT=format, $
    LINESTYLE=linestyle, $
    THICK=thick, $
    HORIZON=horizon, $
    INCREMENT=increment, $
    LABEL=label, $
    LATDEL=latdel, $
    LATLAB=latlab, $
    LATNAMES=latnames, $
    LATS=lats, $
    LONDEL=londel, $
    LONLAB=lonlab, $
    LONNAMES=lonnames, $
    LONS=lons, $
    MAP_OBJECT=map_object, $
    WINDOW=window, $
    _EXTRA=extra
    
    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN, 0
    ENDIF
    
    ; Initialize superclass object,
     ok = self -> cgContainer::INIT(_EXTRA=extra) 
     IF ~ok THEN RETURN, 0

    ; Default values.
    self._cg_autodrawgrid = Keyword_Set(autodrawgrid)
    self._cg_box_axes = Keyword_Set(box_axes)
    IF N_Elements(format) NE 0 THEN self._cg_format = format
    self._cg_fill_horizon = Keyword_Set(fill_horizon)
    self._cg_horizon = Keyword_Set(horizon)
    SetDefaultValue, clip_text, 1
    IF N_Elements(charsize) EQ 0 THEN $
        charsize = (StrUpCase(!Version.OS_Family) EQ 'WINDOWS') ? 0.75 : 1.0
    SetDefaultValue, color, 'opposite'
    SetDefaultValue, fixed_map_grid, 0
    SetDefaultValue, label, 1
    SetDefaultValue, linestyle, 1
    SetDefaultValue, thick, 1.0
    SetDefaultValue, latdel, 5.0
    SetDefaultValue, londel, 10.0
    self._cg_clip_text = clip_text
    self._cg_charsize = charsize
    self._cg_color = color
    self._cg_label = label
    self._cg_linestyle = linestyle
    self._cg_thick = thick
    
    ; Initialize all program pointers.
    IF N_Elements(increment) EQ 0 $
        THEN self._cg_increment = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self._cg_increment = Ptr_New(increment)
    IF N_Elements(latdel) EQ 0 $
        THEN self._cg_latdel = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self._cg_latdel = Ptr_New(latdel)
    IF N_Elements(londel) EQ 0 $
        THEN self._cg_londel = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self._cg_londel = Ptr_New(londel)
    IF N_Elements(latlab) EQ 0 $
        THEN self._cg_latlab = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self._cg_latlab = Ptr_New(latlab)
    IF N_Elements(lonlab) EQ 0 $
        THEN self._cg_lonlab = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self._cg_lonlab = Ptr_New(lonlab)
    IF N_Elements(latnames) EQ 0 $
        THEN self._cg_latnames = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self._cg_latnames = Ptr_New(latnames)
    IF N_Elements(lonnames) EQ 0 $
        THEN self._cg_lonnames = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self._cg_lonnames = Ptr_New(lonnames)
    IF N_Elements(lats) EQ 0 $
        THEN self._cg_lats = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self._cg_lats = Ptr_New(lats)
    IF N_Elements(lons) EQ 0 $
        THEN self._cg_lons = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self._cg_lons = Ptr_New(lons)
    
    ; Make sure you have a valid mapCoord object.
    IF Obj_Valid(mapCoordObj) $
       THEN self._cg_map_object = mapCoordObj $
       ELSE Message, 'A valid map object is required to create a cgMapGrid object.'
       
   ; Need to add this command to a resizeable cgWindow?
   IF Keyword_Set(window) THEN self -> AddCmd, /REPLACE
   IF Keyword_Set(addcmd) THEN self -> AddCmd
   
    RETURN, 1
    
END ; -------------------------------------------------------------------------------------


PRO cgMapGrid__DEFINE, class

    class = { cgMapGrid, $
              _cg_autodrawgrid: 0B, $
              _cg_box_axes: 0B, $              
              _cg_clip_text: 0B, $
              _cg_charsize: 0.0, $
              _cg_color: "", $
              _cg_fill_horizon: 0B, $
              _cg_format: "", $
              _cg_linestyle: 0, $
              _cg_thick: 0, $
              _cg_horizon: 0B, $
              _cg_increment: Ptr_New(), $
              _cg_label: 0, $
              _cg_latdel: Ptr_New(), $
              _cg_latlab: Ptr_New(), $
              _cg_latnames: Ptr_New(), $
              _cg_lats: Ptr_New(), $
              _cg_londel: Ptr_New(), $
              _cg_lonlab: Ptr_New(), $
              _cg_lonnames: Ptr_New(), $
              _cg_lons: Ptr_New(), $
              _cg_map_object: Obj_New(), $
              INHERITS cgContainer $
            }

END ; -------------------------------------------------------------------------------------