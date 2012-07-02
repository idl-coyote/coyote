; docformat = 'rst'
;
; NAME:
;   cgMapGrid
;
; PURPOSE:
;   This object is a wrapper for the cgMap_Grid routine in IDL. It provides a simple 
;   way to allow map grids on images which use a cgMAP object to set up the map 
;   projection space. A map coordinate space must be in effect at the time the 
;   Draw method of this object is used. 
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
;   This object is a wrapper for the cgMap_Grid routine in IDL. It provides a simple 
;   way to allow map grids on images which use a cgMAP object to set up the map 
;   projection space. A map coordinate space must be in effect at the time the 
;   Draw method of this object is used. 
;
; :Categories:
;    Graphics, Map Projections
;    
; :Author:
;   FANNING SOFTWARE CONSULTING::
;      David W. Fanning 
;      1645 Sheely Drive
;      Fort Collins, CO 80526 USA
;      Phone: 970-221-0438
;      E-mail: david@idlcoyote.com
;      Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written by David W. Fanning, 7 November 2011.
;        Modified AutoDrawGrid method to update, better-working method in cgMap_Grid. 28 Dec 2011. DWF.
;                
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;---------------------------------------------------------------------------

;+--------------------------------------------------------------------------
;   The initialization method for the object.
;
; :Params:
;    mapCoord: in, required, type=object
;       A map coordinate object that will set up a map coordinate data space.
;       Required to convert lat/lon values to projected meter space. A cgMap object.
;       
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        If this keyword is set, the object is added to the resizeable graphics
;        window, cgWindow. The DRAW method of the object is called in cgWindow.
;     autodrawgrid: in, optional, type=boolean, default=0
;        If this keyword is set, the grid latitude and longitude values
;        are automatically calculated from the mapCoord object ranges and drawn
;        appropriately. Most keywords are ignored when auto drawing the grid.
;     bcolor: optional, type=string, default='opposite'
;        The name of the color to draw box axes with.
;     box_axes: in, optional, type=boolean, default=0
;        Set this keyword to draw a box-style axes around the map.
;     charsize: in, optional, type=float, default=1.0
;        Set this keyword to the size of characters used for the labels.
;     clip_text: in, optional, type=boolean, default=1
;        Set this keyword to a zero value to turn off clipping of text labels. 
;        By default, text labels are clipped. This keyword is ignored if the 
;        BOX_AXES keyword is set. 
;     color: in, optional, type=string, default="opposite"
;        The name of the color to draw the grid lines in. 
;     fill_horizon: in, optional, type=boolean, default=0
;        Set this keyword to fill the current map horizon.
;     format: in, optional, type=string
;        Set this keyword to a string format for formatting the grid 
;        labels (e.g., '(F0.2)')
;     fuzzy: in, optional, type=float, default=0.0
;        This keyword applies only if the MAP_STRUCTURE keyword is used. Set the
;        keyword to a value that is a percentage of the current data range.
;        This percentage of the range is added to or subtracted from the
;        values used to determine if the label is "inside" the boundary.
;        It allows you to be a little less exact when selecting inside 
;        points. There are occasional aesthetic reasons for allowing fuzzy
;        boundaries. A reasonable value for fuzziness might be 0.0125.
;     horizon: in, optional, type=boolean, default=0 
;        Set this keyword to draw the current map horizon.
;     increment: in, optional, type=float
;        Set this keyword to the spacing between the graticle points.
;     label: in, optional, type=integer, default=1
;        Set this keyword to an integer, n, that labels every n parallels and meridians.
;        For example, LABEL=3 will label every 3rd line. Default is 1.
;     latalign: in, optional, type=float, default=0.5                
;        This keyword controls the alignment of the text baseline for latitude 
;        labels. A value of 0.0 left justifies the label, 1.0 right justifies 
;        it, and 0.5 centers it. This keyword is ignored if the BOX_AXES keyword is set.
;     latdel: in, optional, type=float                 
;        Set this keyword equal to the spacing (in degrees) between parallels of 
;        latitude in the grid. If this keyword is not set, a default value of 5 is used.
;     latlab: in, optional, type=float                  
;        The longitude at which to place latitude labels. The default is the center 
;        longitude on the map. This keyword is ignored if the BOX_AXES keyword is set.
;     latnames: in, optional, type=varies                  
;        Set this keyword equal to an array specifying the names to be used for the 
;        latitude labels. By default, this array is automatically generated in units 
;        of degrees. The LATNAMES array can be either type string or any single numeric 
;        type, but should not be of mixed type.When LATNAMES is specified, the LATS 
;        keyword must also be specified.
;     lats: in, optional, type=float 
;        Set this keyword equal to a one or more element vector of latitudes for which 
;        lines will be drawn (and optionally labeled). If LATS is omitted, appropriate 
;        latitudes will be generated based on the value of the (optional) LATDEL keyword. 
;        If LATS is set to a single value, that latitude and a series of automatically 
;        generated latitudes will be drawn (and optionally labeled).
;     lcolor: in, optional, type=string
;        Set this to the name of the label color to use in labeling grid lines.
;        By default, the same as COLOR, or if BOX_AXIS is set, then same as BCOLOR.
;     linestyle: in, optional, type=integer, default=1 
;        Set this keyword to the type of linestyle desired. See Graphics Keywords in
;        the on-line help for additional information.
;     lonalign: in, optional, type=float, default=0.5
;        This keyword controls the alignment of the text baseline for longitude 
;        labels. A value of 0.0 left justifies the label, 1.0 right justifies 
;        it, and 0.5 centers it. This keyword is ignored if the BOX_AXES keyword is set.
;     londel: in, optional, type=integer, default=10              
;        Set this keyword equal to the spacing (in degrees) between parallels of 
;        longitude in the grid. If this keyword is not set, a default value of 10 is used.
;     lonlab: in, optional, type=float                  
;       The latitude at which to place longitude labels. The default is the center 
;       latitude on the map. This keyword is ignored if the BOX_AXES keyword is set.
;     lonnames: in, optional, type=varies                  
;        Set this keyword equal to an array specifying the names to be used for the 
;        longitude labels. By default, this array is automatically generated in units 
;        of degrees. The LONNAMES array can be either type string or any single numeric 
;        type, but should not be of mixed type.When LONNAMES is specified, the LONS 
;        keyword must also be specified.
;     lons: in, optional, type=float                  
;        Set this keyword equal to a one or more element vector of longitudes for which 
;        lines will be drawn (and optionally labeled). If LONS is omitted, appropriate 
;        longitudes will be generated based on the value of the (optional) LONDEL keyword. 
;        If LONS is set to a single value, that longitudes and a series of automatically 
;        generated longitudes will be drawn (and optionally labeled).
;     thick: in, optional, type=integer, default=1
;        Set this keyword to the thickness of the line used to draw the grid.
;
;---------------------------------------------------------------------------
FUNCTION cgMapGrid::INIT, mapCoord, $
    ADDCMD=addcmd, $
    AUTODRAWGRID=autodrawgrid, $
    BCOLOR=bcolor, $
    BOX_AXES=box_axes, $
    CLIP_TEXT=clip_text, $
    CHARSIZE=charsize, $
    COLOR=color, $
    FILL_HORIZON=fill_horizon, $
    FORMAT=format, $
    FUZZY=fuzzy, $
    LINESTYLE=linestyle, $
    HORIZON=horizon, $
    INCREMENT=increment, $
    LABEL=label, $
    LATALIGN=latalign, $
    LATDEL=latdel, $
    LATLAB=latlab, $
    LATNAMES=latnames, $
    LATS=lats, $
    LCOLOR=lcolor, $
    LONALIGN=lonalign, $
    LONDEL=londel, $
    LONLAB=lonlab, $
    LONNAMES=lonnames, $
    LONS=lons, $
    THICK=thick, $
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
        ;charsize = (StrUpCase(!Version.OS_Family) EQ 'WINDOWS') ? 0.75 : 1.0
        charsize = cgDefCharsize() * 0.75
    SetDefaultValue, bcolor, 'opposite'
    SetDefaultValue, color, 'opposite'
    IF N_Elements(lcolor) EQ 0 THEN lcolor = Keyword_Set(box_axes) ? bcolor : color
    SetDefaultValue, fuzzy, 0.0
    SetDefaultValue, label, 1
    SetDefaultValue, linestyle, 1
    SetDefaultValue, latalign, 0.5
    SetDefaultValue, latdel, 5.0
    SetDefaultValue, lonalign, 0.5
    SetDefaultValue, londel, 10.0
    SetDefaultValue, thick, 1.0
    self._cg_bcolor = bcolor
    self._cg_clip_text = clip_text
    self._cg_charsize = charsize
    self._cg_color = color
    self._cg_label = label
    self._cg_latalign = latalign
    self._cg_lcolor = lcolor
    self._cg_lonalign = lonalign
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
    IF Obj_Valid(mapCoord) $
       THEN self._cg_map_object = mapCoord $
       ELSE Message, 'A valid map object is required to create a cgMapGrid object.'
       
   ; Need to add this command to a resizeable cgWindow?
   IF Keyword_Set(addcmd) THEN self -> AddCmd
   
    RETURN, 1
    
END 


;+--------------------------------------------------------------------------
;   Adds the object as a command (the DRAW method is called) in a cgWindow 
;   resizeable graphics window. 
;
;---------------------------------------------------------------------------
PRO cgMapGrid::AddCmd

   cgWindow, "Draw", self, /Method, /AddCmd 
   
END 


;+--------------------------------------------------------------------------
;   Calculates suitable latitude and longitude lines that run through the
;   map range and suggests a default position for labeling such lines.
;
; :Keywords:
;     success: out, optional, type=boolean
;        Will be set to 1 on return, if the operation was successful. Otherwise,
;        this value is set to 0.
;---------------------------------------------------------------------------
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

    ; Convert the longitudes to 0 to 360. Otherwise, I have
    ; problems near the date line.
;    longitudes = (longitudes + 360.0) MOD 360.0
    
    lon_min = Min(longitudes, MAX=lon_max, /NAN)
    lat_min = Min(latitudes, MAX=lat_max, /NAN)
    center_lat = latitudes[12,12]
    center_lon = longitudes[12,12]
    
    ; We are going to try to have seven lines running through the grid space.
    ; We will have special rules if the center latitude is at the pole.
    latrange = Abs(lat_max - lat_min)
    IF latrange GT 90.0 THEN BEGIN
        lats = -90.0 > (Findgen(13) * 15 - 90.0) < 90.0
        latsdone = 1
    ENDIF
    latstep =  latrange / 6.0
    
    lonrange = Abs(lon_max - lon_min)
    IF lonrange GT 180.0 THEN BEGIN
        lons = -180.0 > (Findgen(13) * 30 - 180.0) < 180.0
        lonsdone = 1
    ENDIF
    lonstep =  (lonrange)/ 6.0
    
    ; Make sure we don't have a center latitude at either pole. If we
    ; do, then lons are calulated differently.
    IF (center_lat GT (90.-0.05)) && (center_lat LT (90.0 + 0.05)) THEN BEGIN
       lats = Scale_Vector(Findgen(5), 0 > lat_min < 80) 
       latsdone = 1 
       IF lonstep GT 40 THEN BEGIN
          lons = Findgen(11) * 36
          lonsDone = 1
       ENDIF      
    ENDIF ELSE BEGIN
       IF (center_lat LT (-90.+0.05)) && (center_lat GT (-90.0 - 0.05)) THEN BEGIN
           lats = Scale_Vector(Findgen(5), -80, 0 < lat_max)  
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
    
    ; Labels should be near the center.
    index = Value_Locate(lons, center_lon)
    latlab = (lons[index] - lons[index-1]) / 2.0 + lons[index-1]
    index = Value_Locate(lats, center_lat)
    lonlab = (lats[index] - lats[index-1]) / 2.0 + lats[index-1]
    
    ; Set up the latitude and longitude names.
    IF Total(lats-Long(lats)) EQ 0 THEN format='(I0)' ELSE format='(F0.2)'
    latnames = String(lats, FORMAT=format)
    IF Total(lons-Long(lons)) EQ 0 THEN format='(I0)' ELSE format='(F0.2)'
    lonnames = String(lons, FORMAT=format)

    ; Set the properties of the object.
    self -> SetProperty, LATLAB=latlab, LATS=lats, LATNAMES=latnames, $
                         LONLAB=lonlab, LONS=lons, LONNAMES=lonnames
                             
END 


;+--------------------------------------------------------------------------
;   Draws the map grid by calling cgMap_Grid.
;
;---------------------------------------------------------------------------
PRO cgMapGrid::Draw

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
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
    cgMap_Grid, $
       BCOLOR=self._cg_bcolor, BOX_AXES=self._cg_box_axes, $
       CLIP_TEXT=1, $
       CHARSIZE=self._cg_charsize, $
       COLOR=self._cg_color, $
       FILL_HORIZON=self._cg_fill_horizon, $
       FORMAT=self._cg_format, $
       FUZZY=self._cg_fuzzy, $
       HORIZON=self._cg_horizon, $
       INCREMENT=*self._cg_increment, $
       LABEL=self._cg_label, $
       LATALIGN=self._cg_latalign, $
       LATDEL=*self._cg_latdel, $
       LATLAB=*self._cg_latlab, $
       LATNAMES=*self._cg_latnames, $
       LATS=*self._cg_lats, $
       LCOLOR=self._cg_lcolor, $
       LINESTYLE=self._cg_linestyle, $
       LONALIGN=self._cg_lonalign, $
       LONDEL=*self._cg_londel, $
       LONLAB=*self._cg_lonlab, $
       LONNAMES=*self._cg_lonnames, $
       LONS=*self._cg_lons, $
       MAP_STRUCTURE=self._cg_map_object, $ ; The map object itself.
       THICK=self._cg_thick

    ; Turn messages back on.
    void = Check_Math()
    !Except = except
    
END 


;+--------------------------------------------------------------------------
;   This method allows the user to get various properties of the object. In general,
;   the same keywords that are used for the INIT method can be used here.
;---------------------------------------------------------------------------
PRO cgMapGrid::GetProperty, $
    AUTODRAWGRID=autodrawgrid, $
    BOX_AXES=box_axes, $
    CLIP_TEXT=clip_text, $
    CHARSIZE=charsize, $
    COLOR=color, $
    FILL_HORIZON=fill_horizon, $
    FIXED_cgMapGrid=fixed_map_grid, $
    FORMAT=format, $
    LCOLOR=lcolor, $
    LINESTYLE=linestyle, $
    THICK=thick, $
    HORIZON=horizon, $
    INCREMENT=increment, $
    LABEL=label, $
    LATALIGN=latalign, $
    LATDEL=latdel, $
    LATLAB=latlab, $
    LATNAMES=latnames, $
    LATS=lats, $
    LONALIGN=lonalign, $
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
    format = self._cg_format
    latalign = self._cg_latalign
    lcolor = self._cg_lcolor
    lonalign = self._cg_lonalign
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
    
END 

    
;+--------------------------------------------------------------------------
;   This method allows the user to set various properties of the object. In general,
;   the same keywords that are used for the INIT method can be used here.
;---------------------------------------------------------------------------
PRO cgMapGrid::SetProperty, $
    AUTODRAWGRID=autodrawgrid, $
    BOX_AXES=box_axes, $
    CLIP_TEXT=clip_text, $
    CHARSIZE=charsize, $
    COLOR=color, $
    DRAW=draw, $
    FILL_HORIZON=fill_horizon, $
    FIXED_cgMapGrid=fixed_map_grid, $
    FORMAT=format, $
    LINESTYLE=linestyle, $
    HORIZON=horizon, $
    INCREMENT=increment, $
    LABEL=label, $
    LATALIGN=latalign, $
    LATDEL=latdel, $
    LATLAB=latlab, $
    LATNAMES=latnames, $
    LATS=lats, $
    LCOLOR=lcolor, $
    LONALIGN=lonalign, $
    LONDEL=londel, $
    LONLAB=lonlab, $
    LONNAMES=lonnames, $
    LONS=lons, $
    THICK=thick, $
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
    IF N_Elements(format) NE 0 THEN self._cg_format = format
    IF N_Elements(linestyle) NE 0 THEN self._cg_linestyle = linestyle
    IF N_Elements(horizon) NE 0 THEN self._cg_horizon = Keyword_Set(horizon)
    IF N_Elements(increment) NE 0 THEN *self._cg_increment = increment
    IF N_Elements(label) NE 0 THEN self._cg_label = label
    IF N_Elements(latalign) NE 0 THEN self._cg_latalign = latalign
    IF N_Elements(latdel) NE 0 THEN *self._cg_latdel = latdel
    IF N_Elements(latlab) NE 0 THEN *self._cg_latlab = latlab
    IF N_Elements(latnames) NE 0 THEN *self._cg_latnames = latnames
    IF N_Elements(lats) NE 0 THEN *self._cg_lats = lats
    IF N_Elements(lcolor) NE 0 THEN self._cg_lcolor = lcolor
    IF N_Elements(lonalign) NE 0 THEN self._cg_lonalign = lonalign
    IF N_Elements(londel) NE 0 THEN *self._cg_londel = londel
    IF N_Elements(lonlab) NE 0 THEN *self._cg_lonlab = lonlab
    IF N_Elements(lonnames) NE 0 THEN *self._cg_lonnames = lonnames
    IF N_Elements(lons) NE 0 THEN *self._cg_lons = lons
    IF N_Elements(thick) NE 0 THEN self._cg_thick = thick

    IF N_Elements(extra) NE 0 THEN self -> cgContainer::SetProperty, _EXTRA=extra
    
    ; Need a draw?
    IF Keyword_Set(draw) THEN self -> Draw

END 


;+--------------------------------------------------------------------------
;   This is the clean-up routine for the object.
;
;---------------------------------------------------------------------------
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
END 


;+--------------------------------------------------------------------------
;   This is the class definition module. Structures used to manipulate
;   map projection and map datum information are also created here.
;
; :Params:
;    class: out, optional, type=structure
;       Occasionally, it is useful to have an object class definition as
;       a structure variable. Using this output keyword will allow that.
;---------------------------------------------------------------------------
PRO cgMapGrid__DEFINE, class

    class = { cgMapGrid, $
              _cg_autodrawgrid: 0B, $
              _cg_bcolor: "", $
              _cg_box_axes: 0B, $              
              _cg_clip_text: 0B, $
              _cg_charsize: 0.0, $
              _cg_color: "", $
              _cg_fill_horizon: 0B, $
              _cg_format: "", $
              _cg_fuzzy: 0.0, $
              _cg_linestyle: 0, $
              _cg_thick: 0, $
              _cg_horizon: 0B, $
              _cg_increment: Ptr_New(), $
              _cg_label: 0, $
              _cg_latalign: 0.0, $
              _cg_latdel: Ptr_New(), $
              _cg_latlab: Ptr_New(), $
              _cg_latnames: Ptr_New(), $
              _cg_lats: Ptr_New(), $
              _cg_lcolor: "", $
              _cg_lonalign: 0.0, $
              _cg_londel: Ptr_New(), $
              _cg_lonlab: Ptr_New(), $
              _cg_lonnames: Ptr_New(), $
              _cg_lons: Ptr_New(), $
              _cg_map_object: Obj_New(), $
              INHERITS cgContainer $
            }

END 