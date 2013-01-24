; docformat = 'rst'
;
; NAME:
;   cgMap_Grid
;
; PURPOSE:
;   Provides a significantly modified MAP_GRID command that can be used together
;   with other Coyote Graphics routines and in resizeable graphics windows.
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
;   This program provides a significantly modified MAP_GRID command that 
;   can be used with other Coyote Graphics routines.
;
;     Description of known MAP_GRID problems::
;        http://www.idlcoyote.com/map_tips/noclip.html
;        http://www.idlcoyote.com/map_tips/missinggrid.html
;        http://www.idlcoyote.com/map_tips/extralines.php

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
;        Significant modification of the MAP_GRID command in IDL to fix known
;           problems, especially when used in conjunction with map projection spaces
;           set up with MAP_PROJ_INIT. David W. Fanning, 7 November 2011.
;        Added an ERASE=0 to the /NOGRAPHICS keyword on the Draw method call to cgMap. 27 Dec 2011.
;        Changed the default line thickness to !P.Thick to better support PostScript files. 28 Dec 2011. DWF.
;        Fixed a problem with grid labeling when values were passed with LATS or LONS keyword. 6 April 2012. DWF.
;        Modified slightly to allow a three-element byte array to be used as the COLOR. 18 April 2012. DWF.
;        If a Map object is available, I make sure to call DRAW method before drawing graphics. 12 Sept 2012. DWF.
;        Added cgGRID keyword to allow the cgMap object to create latitude and longitude grid in its
;            LatLonLabels method. Previously used by default, but it doesn't work well with global
;            map projections. It works best with small map areas in UTM projection space. 3 Jan 2013. DWF.
;        Removed some old code that was used to correct latitude and longitude values. No longer needed,
;            I hope, with the new cgGRID keyword. 3 Jan 2013. 
;        Corrected bug in variable spelling that affect LONDELTA and LATDELTA keywords. 6 Jan 2013. DWF.
;        Lost a piece of code that allows longitude box axes. Added back in. 23 Jan 2013. DWF.
;            
; :Copyright:
;     Copyright (c) 2011-2013, Fanning Software Consulting, Inc.
;---------------------------------------------------------------------------
;
;
;+------------------------------------------------------------------------
;
; Find the grid increment find defining the latitude and longitude delta
; values, if they are not currently defined.
;
; :Params:
;    span: in, required, type=float
;       The data range.
;       
;-------------------------------------------------------------------------
function cgMap_Grid_Incr, span
    ;
    ; Determine LONDEL or LATDEL if not specified
    ;
    COMPILE_OPT hidden, IDL2
    
    IF span eq 0 THEN return, 45.
    ipow = 0
    t = abs(span) < 450.
    WHILE t lt 5 DO BEGIN 
       t = t * 10 & ipow = ipow +1 
    ENDWHILE
    increments = [ 0.1, 1., 2., 4., 5., 10., 15., 30., 45.]
    i = 0
    WHILE t gt (increments[i] * 10) DO i = i + 1
    t = increments[i] / 10^ipow
    retvalue = span ge 0 ? t : -t
    return, retvalue
end


;+------------------------------------------------------------------------
; Find the point on the line between points c0 and c1, expressed in
; DEVICE coordinates, where the longitude (Icoord = 0) or latitude
; (Icoord = 1) is equal to Gwant.  If the segment between c0 and c1
; doesn't intersect the given meridan or parallel, or either endpoint
; is not mappable, return NaN. Otherwise, return the device coordinate, 
; X if Icoord = 0, or Y if Icoord = 1, of the intersection.
; 
; :Params:
;    c0: in, required, type=integer
;       Input coordinate?
;    c1: in, required, type=integer
;       Input coordinate?
;    icoord: in, required, type=integer
;       Index of input coordinate?
;    gwant: in, required, type=integer
;       Global wrapping?
;  
; :Keywords:
;     map_structure: in, optional, type=structure
;         The map structure to covert XY projected meters to lat/lon space.
;         
;-------------------------------------------------------------------------
Function cgMap_Grid_Solve, c0, c1, icoord, gwant, MAP_STRUCTURE=mapStruct

    COMPILE_OPT hidden, IDL2

    hasMap = N_TAGS(mapStruct) gt 0

    p0 = CONVERT_COORD(c0, /DEVICE, /TO_DATA)
    p1 = CONVERT_COORD(c1, /DEVICE, /TO_DATA)

    if (hasMap) then begin   ; Convert from UV to latlon
        p0 = MAP_PROJ_INVERSE(p0[0], p0[1], MAP_STRUCTURE=mapStruct)
        p1 = MAP_PROJ_INVERSE(p1[0], p1[1], MAP_STRUCTURE=mapStruct)
    endif

    p0 = p0[Icoord]
    p1 = p1[Icoord]

    ; Not mappable or zero interval.
    if ~finite(p0) || ~finite(p1) || (p1 eq p0) then return, !values.f_nan

    if (Icoord eq 0) && (p0 gt p1) then begin ;Wrap if we cross dateline
        if gwant ge 0 then p1 += 360. $
        else p0 -= 360.
    endif

    t = (Gwant - p0) / (p1-p0)
    if (t lt 0.0) || (t gt 1.0) then return, !values.f_nan

    low = 0.0
    high = 1.0
    tol = 1.0e-5
    del = c1 - c0
    while abs(high-low) gt tol do begin ;Binary chopping method
        t = (low + high) / 2.
        c = c0 + t * del
        p = CONVERT_COORD(c, /DEVICE, /TO_DATA)
        if (hasMap) then $   ; Convert from UV to latlon
            p = MAP_PROJ_INVERSE(p[0], p[1], MAP_STRUCTURE=mapStruct)
        p = p[Icoord]
        if (~FINITE(p)) then $
            return, !values.f_nan
        if (Icoord eq 0) then begin ;Check for dateline?
            if p lt p0 then p += 360. $ ;Wrap?  P should be in interval p0-p1.
            else if p gt p1 then p -= 360.
        endif
        if (Gwant-p0) * (Gwant - p) gt 0.0 then begin ;In same interval as p0 : low
            low = t
            p0 = p
        endif else high = t         ;Keep low, and fcn at low = p0.
    endwhile

    return, c[Icoord]

end


;
;+------------------------------------------------------------------------
;    This routine fixes a bug in MAP_GRID that causes map labels to be
;    written outside the map boundary when using hardware or true-type
;    fonts. It checks to be sure the label is inside the map boundary
;    before it is written. Users can control how "exact" the boundary is
;    when using GCTP map projections by setting the FUZZY keyword to
;    a multiplication factor that is multiplied times the calculated
;    data range of the map projection. 
;    
;    If a point is determined to be outside the map boundary, a single
;    data value is returned by the function. This is a signal that this
;    data point should not be drawn.
;
; :Params:
;    xy: in, required, type=float
;       The input label point. Normally, a two element array.
;       
; :Keywords:
;     fuzzy: in, optional, type=float, default=0.0
;        This keyword applies only if the GCTP keyword is set. Set the
;        keyword to a value that is a percentage of the current data range.
;        This percentage of the range is added to or subtracted from the
;        values used to determine if the label is "inside" the boundary.
;        It allows you to be a little less exact when selecting inside 
;        points. There are occasional aesthetic reasons for allowing fuzzy
;        boundaries. A reasonable value for fuzziness might be 0.0125.
;     gctp: in, optional, type=boolean, default=0
;        Set this keyword to calculate boundaries in terms of the the
;        current calculated data range variables !X.CRange and !Y.CRange.
;        Otherwise, the boundaries are taken from !Map.LL_BOX.
;        
;-------------------------------------------------------------------------
function cgMap_Grid_Check_Range, xy, FUZZY=fuzzy, GCTP=gctp 

    ; You need at least two points coming in here.
    IF N_Elements(xy) NE 2 THEN RETURN, xy
    x = xy[0]
    y = xy[1]
    IF Keyword_Set(gctp) THEN SetDefaultValue, fuzzy, 0.0
    
    ; If this is a CGTP projection, then check the axis range values. Otherwise,
    ; use the LL_BOX form the map structure !MAP. The latter is more strict about
    ; being inside the box.
    IF Keyword_Set(gctp) THEN BEGIN
        IF x LT (Min(!X.CRange)- fuzzy*(!X.CRange[1]-!X.CRange[0])) THEN RETURN, xy[0]
        IF x GT (Max(!X.CRange)+ fuzzy*(!X.CRange[1]-!X.CRange[0])) THEN RETURN, xy[0]
        IF y LT (Min(!Y.CRange)- fuzzy*(!Y.CRange[1]-!Y.CRange[0])) THEN RETURN, xy[0]
        IF y GT (Max(!Y.CRange)+ fuzzy*(!Y.CRange[1]-!Y.CRange[0])) THEN RETURN, xy[0]
    ENDIF ELSE BEGIN
    
    ENDELSE
    RETURN, xy
end


;+--------------------------------------------------------------------------
; Provides a MAP_GRID command that can be used in conjunction with other
; Coyote Graphics programs and in resizeable graphics windows. Any keyword 
; appropriate for the MAP_GRID command in IDL can be used. 
;     
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        If this keyword is set, the object is added to the resizeable graphics
;        window, cgWindow. Note that a map projection command must be added to the
;        window before this command is added to be effective.
;     bcolor: optional, type=string, default='opposite'
;        The name of the color to draw box axes with.
;     box_axes: optional, type=boolean, default=0
;        Set this keyword to draw box axes on the map projection.
;     cggrid: in, optional, type=boolean, default=0
;        Set this keyword to allow the latitude and longitude values to be set by
;        the LatLon_Labels method in the cgMap object. Previously this was used by
;        default, but it caused a lot of problems with global or near global map projections.
;        This really should be used ONLY if you are mapping a very small region of the Earth,
;        and maybe if you are using a UTM map projection. Othersize, it is probably not 
;        needed, so I have made it an optional choice.
;     charsize: in, optional, type=float
;        The character size for the labels. Default is cgDefCharSize()*0.75.
;     clip_text: in, optional, type=boolean, default=1
;        Set this keyword to a zero value to turn off clipping of text labels. 
;        By default, text labels are clipped. This keyword is ignored if the `Box_Axes` 
;        keyword is set.
;     color: in, optional, type=string, default='opposite'
;        The name of the drawing color for the program.
;     fill_horizon: in, optional, type=boolean, default=0
;     format: in, optional, type=string
;        Set this keyword to a format for the grid labels.
;     fuzzy: in, optional, type=float, default=0.0
;        This keyword applies only if the MAP_STRUCTURE keyword is used. Set the
;        keyword to a value that is a percentage of the current data range.
;        This percentage of the range is added to or subtracted from the
;        values used to determine if the label is "inside" the boundary.
;        It allows you to be a little less exact when selecting inside 
;        points. There are occasional aesthetic reasons for allowing fuzzy
;        boundaries. A reasonable value for fuzziness might be 0.0125.
;     glinestyle: in, optional
;        Not used. Use `Linestyle` instead.
;     glinethick: in, optional
;        Not used. Use `Thick` instead.
;     horizon: in, optional, type=boolean, default=0
;        Set this keyword to draw the current map horizon.
;     increment: in, optional, type=integer, default=4
;        Determines the smoothness of graticle lines by setting the spacing between
;        them. A low number is a smooth number. 
;     label: in, optional, type=integer, default=0
;        An number that tells how to label the grid line. A 0 means no grid lines are 
;        labelled. A 1 means all grid lines are labelled. A 2 means label every 2nd 
;        grid line is labelled. A 3 means every  3rd grid line is labelled, and so on.
;     latalign: in, optional, type=float, default=0.0
;        This keyword controls the alignment of the text baseline for latitude labels. 
;        A value of 0.0 left justifies the label, 1.0 right justifies it, and 0.5 
;        centers it. This keyword is ignored if the `Box_Axes` keyword is set.
;     latdelta: in, optional, type=float
;        Set this keyword to the spacing in degrees between parallels of latitude in
;        the grid. If this keyword is not set, a suitable value is determined from the
;        current map projection.
;     latlabel: in, optional, type=float
;        The longitude at which to place latitude labels. The default is the center 
;        longitude on the map if using Map_Set, and a longitude line on the left of the
;        plot if using Map_Proj_Init. This keyword is ignored if the `Box_Axes` keyword is set. 
;     latnames: in, optional, type=string
;        Set this keyword equal to an array specifying the names to be used for the 
;        latitude labels. By default, this array is automatically generated in units 
;        of degrees. The LATNAMES array can be either type string or any single numeric 
;        type, but should not be of mixed type. When LATNAMES is specified, the LATS 
;        keyword must also be specified. The number of elements in the two arrays need 
;        not be equal. If there are more elements in the LATNAMES array than in the LATS 
;        array, the extra LATNAMES are ignored. If there are more elements in the LATS 
;        array than in the LATNAMES array, labels in degrees will be automatically 
;        provided for the missing latitude labels. The LATNAMES keyword can be also used 
;        when the LATS keyword is set to a single value. It this case, the first label 
;        supplied will be used at the specified latitude; subsequent names will be 
;        placed at the next latitude line to the north, wrapping around the globe if 
;        appropriate. Caution should be used when using LATNAMES in conjunction with a 
;        single LATS value, since the number of visible latitude gridlines is dependent 
;        on many factors. 
;     lats: in, optional, type=float
;        Set this keyword equal to a one or more element vector of latitudes for which 
;        lines will be drawn (and optionally labeled). If LATS is omitted, appropriate 
;        latitudes will be generated based on the value of the (optional) LATDEL keyword. 
;        If LATS is set to a single value, that latitude and a series of automatically 
;        generated latitudes will be drawn (and optionally labeled). Automatically generated 
;        latitudes have values that extend over the map. If LATS is a single value, that 
;        value is taken to be the starting point for labelling (See the LABEL keyword). 
;     lcolor: in, optional, type=string
;        Set this to the name of the label color to use in labeling grid lines.
;        By default, the same as COLOR, or if BOX_AXIS is set, then same as BCOLOR.
;     linestyle: in, optional, type=integer, default=1
;        This keyword is the same as the GLineStyle keyword, but is a more
;        natural way of setting the value.
;     lonalign: in, optional, type=float, default=0.0
;        This keyword controls the alignment of the text baseline for longitude labels. 
;        A value of 0.0 left justifies the label, 1.0 right justifies it, and 0.5 
;        centers it. This keyword is ignored if the `Box_Axes` keyword is set.
;     londelta: in, optional, type=float
;        Set this keyword to the spacing in degrees between parallels of longitude in
;        the grid. If this keyword is not set, a suitable value is determined from the
;        current map projection.
;     lonlabel: in, optional, type=float
;        The latitude at which to place longitude labels. The default is the center 
;        longitude on the map if using Map_Set, and a longitude line on the left of the
;        plot if using Map_Proj_Init. This keyword is ignored if the `Box_Axes` keyword is set. 
;     lonnames: in, optional, type=string
;        Set this keyword equal to an array specifying the names to be used for the 
;        longitude labels. By default, this array is automatically generated in units 
;        of degrees. The LATNAMES array can be either type string or any single numeric 
;        type, but should not be of mixed type. When LATNAMES is specified, the LATS 
;        keyword must also be specified. The number of elements in the two arrays need 
;        not be equal. If there are more elements in the LATNAMES array than in the LATS 
;        array, the extra LATNAMES are ignored. If there are more elements in the LATS 
;        array than in the LATNAMES array, labels in degrees will be automatically 
;        provided for the missing longitude labels. The LATNAMES keyword can be also used 
;        when the LATS keyword is set to a single value. It this case, the first label 
;        supplied will be used at the specified longitude; subsequent names will be 
;        placed at the next longitude line to the north, wrapping around the globe if 
;        appropriate. Caution should be used when using LATNAMES in conjunction with a 
;        single LATS value, since the number of visible longitude gridlines is dependent 
;        on many factors. 
;     lons: in, optional, type=float
;        Set this keyword equal to a one or more element vector of longitudes for which 
;        lines will be drawn (and optionally labeled). If LATS is omitted, appropriate 
;        longitudes will be generated based on the value of the (optional) LATDEL keyword. 
;        If LATS is set to a single value, that longitude and a series of automatically 
;        generated longitudes will be drawn (and optionally labeled). Automatically generated 
;        longitudes have values that extend over the map. If LATS is a single value, that 
;        value is taken to be the starting point for labelling (See the LABEL keyword). 
;     map_structure: in, optional
;        This keyword is normally used to pass in a map structure of the type
;        created by Map_Proj_Init. In this version of the program, it can also
;        be used to pass in a cgMap object, from which the map structure and other
;        pertinent information for creating map grid lines can be obtained.
;     no_grid: in, optional, type=boolean, default=0
;        Set this keyword if you only want labels but not grid lines.
;     orientation: in, optional, type=float, default=0.0
;        Set this keyword equal to an angle in degrees from horizontal (in the clockwise 
;        direction) to rotate the labels. This keyword is ignored if the `Box_Axes` keyword is set. 
;     t3d: in, optional, type=boolean, default=0
;        Set this keyword if you are labeling in 3D space.
;     thick: in, optional, type=integer, default=!P.Thick
;        Sets the thickness of the grid lines.
;     zvalue: in, optional, type=float
;        Draw the grid lines at this Z-value. Implies the use of `T3D`.
;
;---------------------------------------------------------------------------
PRO cgMap_Grid, $
   ADDCMD=addcmd, $
   BCOLOR=bcolor, $
   BOX_AXES=box_axes, $
   CGGRID=cggrid, $
   CHARSIZE=charsize, $
   CLIP_TEXT=clip_text, $
   COLOR=scolor, $
   FILL_HORIZON=fill_horizon, $
   FORMAT=format, $
   FUZZY=fuzzy, $
   GLINESTYLE=glinestyle, $
   GLINETHICK=glinethick, $
   HORIZON=horizon, $
   INCREMENT=increment, $
   LABEL=label, $
   LATALIGN=latalign, $
   LATDELTA = latdelta, $
   LATLABEL=latlab, $
   LATNAMES=latnames, $
   LATS=lats, $
   LCOLOR=lcolor, $
   LINESTYLE=linestyle, $
   LONALIGN=lonalign, $
   LONDELTA=londelta, $
   LONLABEL=lonlab, $
   LONNAMES=lonnames, $
   LONS=lons, $
   MAP_STRUCTURE=mapStruct, $
   NO_GRID=no_grid, $
   ORIENTATION=orientation, $
   T3D=t3d, $
   THICK=thick, $
   ZVALUE=zvalue
   

    Compile_Opt strictarr

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(thisState) NE 0 THEN SetDecomposedState, thisState
        RETURN
    ENDIF

    ; Should this be added to a resizeable graphics window?
    IF (Keyword_Set(addcmd)) && ((!D.Flags && 256) NE 0) THEN BEGIN
    
        cgWindow, 'cgMap_Grid', $
               BOX_AXES=box_axes, $
               CHARSIZE=charsize, $
               CGGRID=cgGrid, $
               CLIP_TEXT=clip_text, $
               COLOR=scolor, $
               FILL_HORIZON=fill_horizon, $
               FORMAT=format, $
               FUZZY=fuzzy, $
               GLINESTYLE=glinestyle, $
               GLINETHICK=glinethick, $
               HORIZON=horizon, $
               INCREMENT=increment, $
               LABEL=label, $
               LATALIGN=latalign, $
               LATDELTA = latdelta, $
               LATLABEL=latlab, $
               LATNAMES=latnames, $
               LATS=lats, $
               LINESTYLE=linestyle, $
               LONALIGN=lonalign, $
               LONDELTA=londelta, $
               LONLABEL=lonlab, $
               LONNAMES=lonnames, $
               LONS=lons, $
               MAP_STRUCTURE=mapStruct, $
               NO_GRID=no_grid, $
               ORIENTATION=orientation, $
               T3D=t3d, $
               THICK=thick, $
               WHOLE_MAP=obsolete_keyword, $
               ZVALUE=zvalue, $
               ADDCMD=1
            
         RETURN
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    SetDefaultValue, charsize, cgDefCharsize() * 0.75
    SetDefaultValue, bcolor, "opposite"
    
    ; I want to use the more natural LINESTYLE and THICK keywords to this routine.
    IF (N_Elements(linestyle) EQ 0) && (N_Elements(glinestyle) NE 0) THEN BEGIN
      linestyle = glinestyle 
    ENDIF ELSE IF (N_Elements(linestyle) EQ 0) THEN linestyle = 1 ; dotted
    IF (N_Elements(thick) EQ 0) && (N_Elements(gthick) NE 0) THEN BEGIN
      thick = gthick 
    ENDIF ELSE IF (N_Elements(thick) EQ 0) THEN thick = !P.Thick 

    ; Try to do this in decomposed color, if possible.
    SetDecomposedState, 1, Current=thisState
    
    ; Need a color.
    IF N_Elements(scolor) NE 0 THEN BEGIN
        CASE Size(scolor, /TNAME) OF
           'STRING':
           'LONG': 
           'BYTE': BEGIN
               IF N_Elements(scolor) NE 3 THEN scolor = StrTrim(Fix(scolor), 2)
               END
           ELSE: scolor = StrTrim(scolor,2)
        ENDCASE 
    ENDIF ELSE scolor = "opposite"
    IF N_Elements(scolor) EQ 0 THEN color = !P.Color ELSE  color = sColor
    IF (Size(scolor, /TNAME) EQ 'BYTE') AND (N_Elements(scolor) EQ 3) THEN color = cgColor(scolor)
    IF Size(color, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN color = Byte(color)
    IF Size(color, /TYPE) LE 2 THEN color = StrTrim(Fix(color),2)
    
    ; Check for label color now. Depends on other colors and value of BOX_AXES.
    IF N_Elements(lcolor) EQ 0 THEN lcolor = (Keyword_Set(box_axes)) ? bcolor : color

  ; Determine if you have a map structure or a map object.
  IF N_Elements(mapStruct) EQ 0 THEN BEGIN
     hasMap = 0
     hasMapObj = 0
  ENDIF ELSE BEGIN
     IF Obj_Valid(mapStruct) THEN BEGIN
         hasMapObj = 1
         
         mapObj = mapStruct
         mapObj -> Draw, /NoGraphics
         thisMapStruct = mapObj -> GetMapStruct()
         
         ; I've taken this out for now, as it is not working as well as I hoped
         ; it would. I may have to revisit this. Or, perhaps, modify the cgMap_Grid_INCR
         ; routine.
         IF Keyword_Set(cgGrid) THEN BEGIN
             mapObj -> LatLonLabels, LATS=mlats, LATLAB=mlatlab, LATDELTA=latdelta, LATNAMES=mlatnames, $
                                     LONS=mlons, LONLAB=mlonlab, LONDELTA=londelta, LONNAMES=mlonnames
             IF N_Elements(lats) EQ 0 THEN BEGIN
                lats = mlats
                IF (N_Elements(latnames) EQ 0) THEN latnames = mlatnames
                IF N_Elements(latlab) EQ 0 THEN latlab = mlatlab
             ENDIF
             IF N_Elements(lons) EQ 0 THEN BEGIN
                lons = mlons
                IF (N_Elements(lonnames) EQ 0) THEN lonnames = mlonnames
                IF N_Elements(lonlab) EQ 0 THEN lonlab = mlonlab
             ENDIF
         ENDIF
         
      ENDIF ELSE BEGIN
          hasMapObj = 0
          thisMapStruct = mapStruct
      ENDELSE
      hasMap = 1
  ENDELSE
  
  if ((!x.type NE 3) && ~hasMap) THEN $
     message, 'cgMap_Grid---Current ploting device must have mapping coordinates'
  
  ; Put a grid on a previously established map projection.
  ;
  ; no grid? - in case someone wants just to put labels
  no_grid = keyword_set(no_grid)
  
  ; if Label = n, then Labels are added every n gridlines
  ;   If box_axes is set, and LABEL isn't explicitly specified, set label.
  ;
  nlabel = (N_ELEMENTS(label) gt 0) ? $
      FIX(ABS(label[0])) : KEYWORD_SET(box_axes)
  
  have_lons =  n_elements(lons) gt 0
  have_lats =  n_elements(lats) gt 0
  
  if n_elements(zvalue) eq 0 THEN zvalue = 0
  
  ; CLIP_TEXT (default value = 1) = 1 to clip text within the map area,
  ; 0 to not clip text.
  noclip = (N_ELEMENTS(clip_text) gt 0) ? ~KEYWORD_SET(clip_text) : 0
  
  
  ;Orientation is reversed & conflicts w/box_axes
  if n_elements(orientation) and (keyword_set(box_axes) eq 0) then $
    map_struct_append, extra,'ORIENTATION', -1 * orientation
  
  
  ;
  ; The gridlines can be specified by
  ;
  ;  1) an array of lats and/or lons
  ;  2) a single lats or lons which is taken to be the center
  ;     or 'for sure' lat or lon with gridlines every latdelta or londelta from it
  ;  3) automatically calculated if lats or lons are not specified.
  ;
  ;
  ; Require that LATS be specified when LATNAMES is ALSO SPECIFIED
  ;
  if (n_elements(latnames) gt 0) and n_elements(lats) le 1 then $
    message,'cgMap_Grid---The LATNAMES keyword MUST be used in conjuction '+$
    'with the LATS keyword.'
  if n_elements(lonnames) gt 0 and have_lons eq 0 then $
    message,'cgMap_Grid---The LONNAMES keyword MUST be used in conjuction '+$
    'with the LONS keyword.'
  
  ; Get lat/lon ranges from !MAP. Did MAP_SET specify 4 element limit?
  map = hasMap ? thisMapStruct : !MAP
  if n_elements(lats) gt 1 then latmin = min(lats, max=latmax) $
  else if map.ll_box[0] ne map.ll_box[2] then begin
      latmin = map.ll_box[0]
      latmax = map.ll_box[2]
  endif else begin
      latmin = -90
      latmax = 90
  endelse
  
  if have_lons then begin        ;Lons directly specified?
      lonmin = lons[0]
      lonmax = lons[n_elements(lons)-1]
  endif else if (map.ll_box[1] ne map.ll_box[3]) and $ ;Lon limit specified?
    (latmax lt 90.) and (latmin gt -90.) then begin ; and poles not included
      lonmin = map.ll_box[1]
      lonmax = map.ll_box[3] ;Copy limits
  endif else begin                ;If not, use entire globe
      lonmin = -180
      lonmax = 180
  endelse
  
  IF lonmax le lonmin THEN lonmax = lonmax + 360.
  
            ;Default grid spacings...
  IF n_elements(latdelta) eq 0 THEN begin
      latdelta = cgMap_Grid_incr(latmax - latmin)
      latd = 1
  endif else latd = latdelta
  
  IF n_elements(londelta) eq 0 THEN begin
      londelta = cgMap_Grid_incr(lonmax - lonmin)
      lond = 1
  endif else lond = londelta
  
  ; IF the deltas are < 1,
  ; do not convert the limits into integers
  IF abs(latmax - latmin) gt 5. and latd ge 1 THEN BEGIN ;Make range integers
      latmin = float(floor(latmin))
      latmax = ceil(latmax)
  ENDIF

  IF abs(lonmax - lonmin) gt 5 and lond ge 1 THEN BEGIN ;Integerize long spans
      lonmin = float(floor(lonmin))
      lonmax = ceil(lonmax)
  ENDIF
  
  ; Where we label things...
  IF N_Elements(Latlab) eq 0 THEN Latlab = (lonmin + lonmax)/2
  IF N_ELements(LonLab) eq 0 THEN LonLab = (latmin +latmax)/2

  IF n_elements(latalign) eq 0 THEN latalign = .5   ;Text alignment of lat labels
  IF n_elements(lonalign) eq 0 THEN lonalign = .5 ;Text alignment of lon labels
  
  ; Is this a cylindrical proj?
  is_cyl = 0
  if (hasMap) then begin
      IF Obj_Valid(mapObj) THEN is_cyl = mapObj -> Is_Cylindrical()
  endif else begin
      map_proj_info, iproj, CYLINDRICAL=is_cyl, /CURRENT
  endelse
  
  if keyword_set(increment) then step = increment $
  else step = 4 < (latmax - latmin)/10.
  
  len = long(float((latmax-latmin)) / float(step) + 1.0)
  
  ; Clip to avoid roundoff errors which can cause the latitude to exceed
  ; 90 degs by a very small amount.
  lati = (float(latmax-latmin) / (len-1.)) * findgen(len) + latmin > (-90) < 90
  
  ; This fudge avoids curved meridians at the poles because of the split planes
  if is_cyl and map.p0lat eq 0 then begin
      del = 2.0e-2
      if lati[0] eq -90 then lati[0] = del - 90.
      if lati[len-1] eq 90 then lati[len-1] = 90. - del
  endif
  
  
  ;Compute longit distance between points for latitude lines.
  step = 4 < (lonmax - lonmin)/10. ;At most 4 degrees
  len = (lonmax-lonmin)/step + 1
  loni = findgen(len) * step + lonmin
  IF (loni[len-1] NE lonmax) THEN loni = [loni, lonmax]
  
  
  ;
  ; Determine the number of lons and the lon array
  ;
  if n_elements(lons) eq 0 then begin
      n_lons = 1+fix((lonmax-lonmin) / londelta)
      longitudes = lonmin - (lonmin mod londelta) + findgen(n_lons) * londelta
  endif else if n_elements(lons) eq 1 then begin
      i0 = ceil((lonmin - lons[0]) / float(londelta)) ;First tick
      i1 = floor((lonmax - lons[0]) / float(londelta)) ;Last tick
      n_lons = i1 - i0 + 1 > 1
      longitudes = (findgen(n_lons) + i0) * londelta + lons[0]
  endif else begin
      n_lons=n_elements(lons)
      longitudes=lons
  endelse
  
  ;
  ; Determine the number of lats and the lat array
  ;
  if n_elements(lats) eq 0 then begin
      lat0 = latmin - (latmin mod float(latdelta)) ;1st lat for grid
      n_lats = 1 + fix((latmax-lat0)/float(latdelta))
      latitudes = lat0 + findgen(n_lats)*latdelta
  endif else if n_elements(lats) eq 1 then begin
      i0 = ceil((latmin - lats[0]) / float(latdelta)) ;First tick
      i1 = floor((latmax - lats[0]) / float(latdelta)) ;Last tick
      n_lats = i1 - i0 + 1 > 1
      latitudes = (findgen(n_lats) + i0) * latdelta + lats[0]
  endif else begin
      n_lats=n_elements(lats)
      latitudes=lats
  endelse
  
  ;
  ; Build the Latitude/Longitude Label Flags
  ;
  lon_label = bytarr(n_lons)
  lat_label = bytarr(n_lats)
  if nlabel ne 0 then begin
      if n_elements(lons) eq 1 then begin ; Ensure center is set and then go out
          index=where(longitudes eq lons[0])
          for i=(index[0] > 0) mod nlabel, n_lons-1, nlabel do lon_label[i] = 1
      endif else begin
          for i=0, n_lons-1, nlabel do lon_label[i] = 1
      endelse
  
      if n_elements(lats) eq 1 then begin ; Make sure the center one is set
                                  ; and go out from there
          index=where(latitudes eq lats[0], count)
          for i=(index[0] > 0) mod nlabel, n_lats-1, nlabel do lat_label[i] = 1
      endif else begin            ; Start with latmin and label each nlabel point
          for i=0, n_lats-1, nlabel do lat_label[i] = 1
      endelse
  
  endif
  
  ;
  ;   Dont repeat 180 labelling if the projection is cylindrical or
  ;   polar and both 180 and -180 are present. This can be defeated by using
  ;   LONS=-180
  ;
  if is_cyl or (abs(map.p0lat) eq 90) then begin
      id_180 = where(longitudes eq 180,count)
      id_m180 = where(longitudes eq -180,mcount)
      if count gt 0 and mcount gt 0 then begin
          if n_elements(lons) eq 1 then begin
              if lons[0] eq -180 then lon_label[id_180]=0
          endif else lon_label[id_m180]=0
      endif
  endif
  
  n = n_lons > n_lats             ;
  latlontxt = strarr(n, 2)
  
  if keyword_set(box_axes) then begin ;Draw a Box legend?
      box_thick = box_axes * 0.1  ;From mm to Thickness in cm
      dc = !d.y_ch_size           ;Char height to draw
      if n_elements(charsize) ne 0 then dc = dc * charsize
      IF hasMapObj THEN mapObj -> Draw, /NoGraphics, Erase=0
      xw = !x.window * !d.x_size  ;Window coords in x & y
      yw = !y.window * !d.y_size
  ; xww and yww = corners of the uv_range that is mappable.  If NOBORDER
  ; was set for MAP_SET, this is the same as the window coords (xw,yw),
  ; otherwise, this rectangle is smaller than the window rectangle.
  ; Fudge factor for window to ensure that the edges are mappable.
      del = [1,-1]* 0.01
      IF hasMapObj THEN BEGIN
        mapObj -> GetProperty, XRANGE=xrange, YRANGE=yrange
        xww = (xrange * !x.s[1] + !x.s[0]) * !d.x_size + del
        yww = (yrange * !y.s[1] + !y.s[0]) * !d.y_size + del
      ENDIF ELSE BEGIN
        xww = (map.uv_box[[0,2]] * !x.s[1] + !x.s[0]) * !d.x_size + del
        yww = (map.uv_box[[1,3]] * !y.s[1] + !y.s[0]) * !d.y_size + del
      ENDELSE
      bdel = box_thick * !d.y_px_cm ;Thickness of box in device units
  
      xp = xw[0] - [0,bdel, bdel,0] ;X  & Y polygon coords for outer box
      yp = yw[0] - [0,0,bdel,bdel]
                                  ;Draw the outline of the box
      cgPlotS, xw[[0,1,1,0,0]], yw[[0,0,1,1,0]], /DEVICE, $
          COLOR=bcolor, LINESTYLE=0, THICK=thick
      cgPlotS, xw[[0,1,1,0,0]]+[-bdel, bdel, bdel, -bdel, -bdel], $
        yw[[0,0,1,1,0]]+[-bdel, -bdel, bdel, bdel, -bdel], /DEVICE, $
          COLOR=bcolor, LINESTYLE=0, THICK=thick
  
      ychar = [yw[0]-bdel-dc, yw[1]+bdel+dc/4.]
      xchar = [xw[0] - bdel - dc/4., xw[1]+bdel+dc/4.]
      boxpos = replicate(!values.f_nan, n, 2,2)
  
  ;Device coordinates for box annotations. Go in to avoid edges of map &
  ;border which are fraught with singularities.  Also to avoid effects
  ;of MAP_SET,/NOBORDER.  For box axes to be annotated, all the edges of the
  ;map rectangle must be mappable.
  ;
  endif else box_thick = 0
  
  ;  Do the horizon if specified.
  hcolor = (Size(color, /TNAME) EQ 'STRING') ? cgColor(color) : color
  if keyword_set(horizon) then map_horizon, COLOR=hcolor, _EXTRA=e
  if keyword_set(fill_horizon) then map_horizon, COLOR=hcolor, _EXTRA=e, /FILL
  ;
  ;   ****************** Draw/Label the meridians ******************
  ;
  FOR i=0,n_lons-1 DO BEGIN
      lon=longitudes[i]
      lon2 = (lon lt -180) ? (lon + 360) : $
          ((lon gt 180) ? (lon - 360) : lon)
  
  ; This block of code draws longitude lines that are at the center + or
  ; - 180 degrees, at center + or - (180-eps) to ensure that the grid
  ;   appears on the correct side.  Its really not necessary if people
  ;   would use the /HORIZON keyword, but they don't.
  
      if is_cyl then begin
          del = lon - map.p0lon
          while del gt 180 do del -= 360.
          while del lt -180 do del += 360.
          if abs(del) eq 180 then $
              lon -= 1.0e-5 * ((del ge 0) ? 1 : -1) ;fudge it (sign(1.0e-5, del))
      endif
  
      IF (~no_grid) THEN begin
          if (hasMap) then begin
              ; Make sure to clear out variable in case all points are clipped.
              polylines = -1
  ;            uv = MAP_PROJ_FORWARD(REPLICATE(lon, N_ELEMENTS(lati)), lati, $
  ;                MAP_STRUCTURE=thisMapStruct, POLYLINES=polylines)
  ;;; DWF correction next two commands.
              uv = MAP_PROJ_FORWARD(REPLICATE(lon, N_ELEMENTS(lati)), lati, $
                  MAP_STRUCTURE=thisMapStruct)
              polylines = [N_Elements(lati), Indgen(N_Elements(lati))]
              index = 0L
              npoly = N_ELEMENTS(polylines)
              ; Loop thru our polylines connectivity array.
              while (index lt npoly) do begin
                  nline = polylines[index]
                  if (nline eq -1) then $
                      break
                  if (nline gt 0) then begin
                      indices = polylines[index + 1 : index + nline]
                      cgPlotS, REFORM(uv[0,indices]), REFORM(uv[1,indices]), $
                          zvalue, $
                          NOCLIP=0, $
                          COLOR=color, LINESTYLE=linestyle, THICK=thick, $
                          _EXTRA=extra
                  endif
                  index += nline + 1
              endwhile
          endif else begin
              cgPlotS, lon, lati, zvalue, NOCLIP=0, $
                   COLOR=color, LINESTYLE=linestyle, THICK=thick, _EXTRA=extra
          endelse
      endif
  
      IF N_Elements(format) EQ 0 THEN BEGIN
          fmt = (lon2 ne long(lon2)) ? '(f7.2)' : '(i4)' 
      ENDIF ELSE BEGIN 
          IF format EQ "" THEN fmt = (lon2 ne long(lon2)) ? '(f7.2)' : '(i4)' ELSE fmt = format
      ENDELSE
  
      IF lon_label[i] THEN BEGIN
  
          IF i lt n_elements(lonnames) then begin ;User specified label?
              IF (reverse(size(lonnames[i])))[1] eq 7 then $ ;String?
                lonname=lonnames[i] else $
                lonname=strtrim(string(lonnames[i], FORMAT=fmt),2)
          endif else $
              lonname=strtrim(string(lon2, format=fmt),2)
  
          latlontxt[i,0] = lonname
          if (box_thick eq 0) then begin
              xy = 0
              if (hasMap) then begin
                  ; We need to convert from latlon to UV ourself.
                  uv = MAP_PROJ_FORWARD(lon, LonLab, MAP_STRUCTURE=thisMapStruct)
                  if (FINITE(uv[0]) && FINITE(uv[1])) then begin
                      xy = uv[0:1]
                      gctp = 1
                  endif
              endif else begin
                  if (noclip eq 1) || map_point_valid(lon, LonLab) then $
                      xy = [lon, LonLab]
              endelse

              IF N_Elements(xy) NE 0 THEN xy = cgMap_Grid_check_range(xy, GCTP=gctp, FUZZY=fuzzy)
              IF N_Elements(xy) EQ 2 THEN BEGIN
                 cgText, xy[0], xy[1], lonname, ALIGNMENT=lonalign, $
                     NOCLIP=1, Z=zvalue, COLOR=lcolor, CHARSIZE=charsize, _EXTRA=extra
                 Undefine, xy
              ENDIF
          endif
  
      ENDIF
  
      if box_thick ne 0 then begin
          dy = (yw[1] - yw[0]) * 0.01 ;1% of the height
          for j=0,1 do begin      ;Save longitude crossings, try for edge...
              k = 0
              ; Try to find longitude crossings.  If it doesn't cross exactly
              ; at the edge, try going in until it crosses and is valid.
              while ~finite(boxpos[i,j,0]) && abs(k) lt 3 do begin
                  boxpos[i, j, 0] = cgMap_Grid_solve( $
                      [xww[0], yw[j]+k*dy], $
                      [xww[1], yw[j]+k*dy], 0, lon, $
                      MAP_STRUCTURE=thisMapStruct)
                  k = k + (j ? -1 : 1)
              endwhile
          endfor
      endif
  ENDFOR

  ;
  ; Draw/Label the parallels of latitude  ******************
  ;
  
  FOR i=0,n_lats-1 DO BEGIN
  
      lat=latitudes[i]
      IF N_Elements(format) EQ 0 THEN BEGIN
          fmt = (lat ne long(lat)) ? '(f7.2)' : '(i4)' 
      ENDIF ELSE BEGIN 
          IF format EQ "" THEN fmt = (lat ne long(lat)) ? '(f7.2)' : '(i4)'  ELSE fmt = format
      ENDELSE
      IF lat_label[i] THEN BEGIN
          IF i lt n_elements(latnames) then begin ;User specified latname?
              IF (reverse(size(latnames[i])))[1] eq 7 then $
                latname=latnames[i] else $
                latname=strtrim(string(latnames[i],format=fmt),2)
          endif else begin
              latname=strtrim(string(lat, format=fmt),2)
          endelse
          latlontxt[i, 1] = latname
          if (box_thick eq 0) then begin
              xy = 0
              if (hasMap) then begin
                  ; We need to convert from latlon to UV ourself.
                  uv = MAP_PROJ_FORWARD(latlab, lat, MAP_STRUCTURE=thisMapStruct)
                  if (FINITE(uv[0]) && FINITE(uv[1])) then begin
                      xy = uv[0:1]
                      gctp = 1
                  endif
              endif else begin
                  if (noclip eq 1) || map_point_valid(latlab, lat) then $
                      xy = [latlab, lat]
              endelse
              IF N_Elements(xy) NE 0 THEN xy = cgMap_Grid_check_range(xy, GCTP=gctp, FUZZY=fuzzy)
              IF N_Elements(xy) EQ 2 THEN BEGIN
                 cgText, xy[0], xy[1], latname, CHARSIZE=charsize, $
                     alignment=latalign, NOCLIP=1, COLOR=lcolor, Z=zvalue, _EXTRA=extra
                 Undefine, xy
              ENDIF
          endif
      ENDIF
  
      IF Abs(lat) EQ 90 THEN BEGIN
        oldlat = lat
        lat = -89.975 > lat < 89.975
      ENDIF
      if (~no_grid && (ABS(lat) ne 90)) then begin
          if (hasMap) then begin
              ; Make sure to clear out variable in case all points are clipped.
              polylines = -1
              IF (hasMapObj) THEN BEGIN
              
                     ; Added this because if there is not a point in the longitude vector
                     ; that is inside our actual data range, then the line can be drawn
                     ; at the wrong place. I don't really understand why. It may have something
                     ; to do with a stale map structure.
                     mapObj -> GetProperty, XRANGE=xrange, YRANGE=yrange
                     thisMapStructure = mapObj -> GetMapStruct()
                     ll = Map_Proj_Inverse(xrange, yrange, MAP_STRUCTURE=thisMapStructure)
                     lonii = Reform(ll[0,*])
                     uv = MAP_PROJ_FORWARD(lonii, REPLICATE(lat, N_ELEMENTS(lonii)), $
                          MAP_STRUCTURE=thisMapStruct)
              ENDIF ELSE BEGIN
                       uv = MAP_PROJ_FORWARD(loni, REPLICATE(lat, N_ELEMENTS(loni)), $
                          MAP_STRUCTURE=thisMapStruct)
              ENDELSE

              ; This line has been modified by DWF to fix a bug in MAP_PROJ_FORWARD that
              ; screws up lines near the poles.
              polylines = [N_Elements(loni), Indgen(N_Elements(loni))]
              index = 0L
              npoly = N_ELEMENTS(polylines)
              ; Loop thru our polylines connectivity array.
              while (index lt npoly) do begin
                  nline = polylines[index]
                  if (nline eq -1) then $
                      break
                  if (nline gt 0) then begin
                      indices = polylines[index + 1 : index + nline]
                      cgPLOTS, REFORM(uv[0,indices]), REFORM(uv[1,indices]), $
                          zvalue, $
                          NOCLIP=0, $
                          COLOR=color, LINESTYLE=linestyle, THICK=thick, $
                          _EXTRA=extra
                  endif
                  index += nline + 1
              endwhile
          endif else $
              cgPLOTS, loni, lat, zvalue, NOCLIP=0, $
                  COLOR=color, LINESTYLE=linestyle, THICK=thick, _EXTRA=extra
      endif
  
      if box_thick ne 0 then begin
          for j=0,1 do begin ;Save latitude crossings
              ; Start at edge and try for an intersection.
              ; If that doesn't work, go in some.
              k = 0
              dx = (xw[1] - xw[0]) * 0.01
              while ~finite(boxpos[i,j,1]) && abs(k) lt 3 do begin
                  boxpos[i, j, 1] = cgMap_Grid_Solve( $
                      [xw[j]+dx*k, yww[0]], $
                      [xw[j]+dx*k, yww[1]], 1, lat, $
                      MAP_STRUCTURE=thisMapStruct)
                  k = k + (j ? -1 : 1)
              endwhile
          endfor
      endif
  
  endfor
  
  
  ; ******************************** Do the box axes **************************
  if box_thick ne 0 then begin
      for iaxis=0,1 do for j=0,1 do begin ; iaxis = 0 for lon axis, 1 for lat axis
          v = boxpos[*,j,iaxis]       ;Values along axes
          good = where(finite(v), count) ;Ignore bad values
          if (count eq 0) then $  ;Anything there?
              continue
          dy = iaxis eq 1
          v = v[good]             ;Remove unmappable elements
          subs = sort(v)          ;Sort the axis crossings
          v = v[subs]             ;Sorted Y values
          vtext = (latlontxt[good,iaxis])[subs]
          v0 = ([xw[0], yw[0]])[iaxis] ;Starting value on axis
          xp0 = xp + j * (xw[1]-xw[0] + bdel) ;Polygon X coords
          yp0 = yp + j * (yw[1]-yw[0] + bdel) ;Y coords
          xychar = [xchar[j], ychar[j]] ;Char position
  
          for i=0, count-1 do begin ;Draw each item
              z = v[i]            ;Axis crossing value
              if iaxis eq 0 then begin
                  xp0 = (i eq (count-1) && (i and 1) && ~vtext[i]) ? $
                      [v0, xw[1], xw[1], v0] : [v0, z, z, v0]
              endif else yp0 = [v0, v0, z, z]
              if (i and 1) then $
                  cgColorFill, xp0, yp0, /DEVICE, COLOR=bcolor
              xychar[iaxis] = z
              if strlen(vtext[i]) gt 0 then begin
                  cgText, xychar[0], xychar[1], vtext[i], $
                      ORIENTATION=dy * (90-180*j), CHARSIZE=charsize, $
                      ALIGN=0.5, CLIP=0, Z=zvalue, COLOR=lcolor, /DEVICE, _EXTRA=extra
              endif
              v0 = z
          endfor
                                  ;Fill to the end of the axis
          if i and 1 then begin
              if iaxis eq 0 then xp0 = [v0, xw[1], xw[1], v0] $
              else yp0 = [v0, v0, yw[1], yw[1]]
              cgColorFill, xp0, yp0, /DEVICE, COLOR=bcolor
          endif
      endfor
  endif   ; box_thick

    ; Restore color mode
    SetDecomposedState, thisState
    
end
