; docformat = 'rst'
;
; NAME:
;   cgMapVector
;
; PURPOSE:
;   This object allows vectors (arrows) to be placed as annotations on map 
;   projections created with the cgMap coordinate object.
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
;   This object allows vectors (arrows) to be placed as annotations on map 
;   projections created with the cgMap coordinate object.
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
;        Written by David W. Fanning, 21 November 2011.
;        Tested and bugs fixed. 17 Sept 2012. DWF.
;        Bug fix in draw method when passing lat/lon vectors. 6 Jan 2013. DWF.
;        Added PALETTE keyword to allow the vectors to be drawn in colors scaled
;           to vector magnitude. 6 Jan 2012. DWF.
;                
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
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
;     clip: in, optional, type=fltarr(4)
;        The coordinates of a rectangle used to clip the graphics output. 
;        The rectangle is specified as a vector of the form [X0, Y0, X1, Y1], 
;        giving coordinates of the lower left and upper right corners, 
;        respectively. The default clipping rectangle is the plot window set
;        up by the cgMap object. 
;     color: in, optional, type=string, default="opposite"
;        The name of the color to draw the arrows lines in. 
;     hsize: in, optional, type=float, default=0.35
;        The value of this keyword sets the length of the arrowhead. See the documenation
;        for the ARROW command in IDL for further explanation.
;     lats: in, optional, type=float
;        The latitude values where the vector is to be drawn.
;     length, in, optional, type=float
;        The U and V vectors are mutiplied by LENGTH before they are used
;        to calculate the (x1,y1) endpoint of the vector. By default, the length is set
;        to 1/100th of the XRANGE of the MapCoord object. This means that the maximum
;        length of a vector will be approximately LENGTH * SQRT(2).
;     linestyle: out, optional, type=integer, default=1 
;        Set this keyword to the type of linestyle desired. See Graphics Keywords in
;        the on-line help for additional information.
;     lons: in, optional, type=float
;        The longitude values where the vector is to be drawn.
;     noclip: in, optional, type=boolean, default=0
;        Set this keyword to supress clipping of the plot.
;     palette: in, optional, type=byte
;        A (256x3) color palette containing the RGB color vectors to use for coloring the vectors
;        according to the magitude of the vectors. If the color palette is not 256 colors in length
;        then the magitude is scaled into the number of colors available. If a color palette is
;        used, then the `Color` keyword is ignored.
;     t3d: in, optional, type=boolean, default=0
;        Set this graphics keyword if you wish to draw using the T3D transformation matrix.
;     thick: in, optional, type=integer, default=1
;        Set this keyword to the thickness of the line used to draw the grid.
;     umagnitude: in, optional, type=float
;        The magnitude of the vector in the U direction.
;     uvcoords: in, optional, type=boolean, default=0
;        Set this keyword if the LONS and LATS are specified in UV (XY) coordinates, rather than
;        longitude and latitude coordinates.
;     vmagnitude: in, optional, type=float
;        The magnitude of the vector in the V direction.
;     zvalue: in, optional, type=float, default=0.0
;        Set this keyword to the ZVALUE where the output should be drawn.
;     _extra: in, optional
;        Any keywords appropriate for superclass objects are also permitted.
;
;---------------------------------------------------------------------------
FUNCTION cgMapVector::INIT, mapCoord, $
    ADDCMD=addcmd, $
    CLIP=clip, $
    COLOR=color, $
    HSIZE=hsize, $
    LATS=lats, $
    LENGTH=length, $
    LINESTYLE=linestyle, $
    LONS=lons, $
    NOCLIP=noclip, $
    PALETTE=palette, $
    SOLID=solid, $
    T3d=t3d, $
    THICK=thick, $
    UMAGNITUDE=u, $
    UVCOORDS=uvcoords, $
    VMAGNITUDE=v, $
    ZVALUE=zvalue, $
    _EXTRA=extra
    
    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN, 0
    ENDIF
    
    ; Make sure you have a valid map object or there is no point in going ahead.
    IF ~Obj_Valid(mapCoord) THEN Message, 'A valid map object is required to create a cgMapPlotS object.'
    
    ; Initialize superclass object,
     ok = self -> cgContainer::INIT( _EXTRA=extra) 
     IF ~ok THEN RETURN, 0

    ; Default values.
    SetDefaultValue, color, 'White'
    SetDefaultValue, hsize, -0.35
    SetDefaultValue, length, !Values.F_NAN
    SetDefaultValue, linestyle, 0
    SetDefaultValue, noclip, 1
    SetDefaultValue, solid, 0
    SetDefaultValue, t3d, 0
    SetDefaultValue, thick, 1.0
    SetDefaultValue, uvcoords, 0B
    SetDefaultValue, zvalue, 0.0

    IF N_Elements(clip) NE 0 THEN self.clip = clip
    self.color = color
    self.hsize = hsize
    self.length = length
    self.linestyle = linestyle
    self.noclip = noclip
    self.solid = solid
    self.t3d = t3d
    self.thick = thick
    self.uvcoords = uvcoords
    self.zvalue = zvalue
    
    IF N_Elements(lons) EQ 0 $
        THEN self.lons = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.lons = Ptr_New(lons)
    IF N_Elements(lats) EQ 0 $
        THEN self.lats = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.lats = Ptr_New(lats)
        
    IF N_Elements(u) EQ 0 $
        THEN self.u = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.u = Ptr_New(u)
        
    IF N_Elements(v) EQ 0 $
        THEN self.v = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.v = Ptr_New(v)
        
    ; Store the map object. 
    self.mapCoord = mapCoord
        
    ; Do you have a color palette?
    IF N_Elements(palette) NE 0 THEN BEGIN
       self.color = ""
       s = Size(palette, /DIMENSIONS)
       IF s[1] GT s[0] THEN palette = Transpose(palette)
       s = Size(palette, /DIMENSIONS)
       ncolors = s[0]
       self.palette = Ptr_New(palette)
       self.magcolors = Ptr_New(BytScl(Sqrt((*self.v)^2 + (*self.u)^2), TOP=ncolors-1))
    ENDIF

    ; Need to add this command to a resizeable cgWindow?
    IF Keyword_Set(addcmd) THEN BEGIN
       window = cgQuery(/Current, OBJECT=winObject)
       IF Obj_Valid(winObject) THEN BEGIN
           cgWindow, 'Draw', self, /Method, /AddCmd
       ENDIF ELSE BEGIN
          cgWindow, 'Draw', self, /Method
       ENDELSE
       
    ENDIF
    RETURN, 1
    
END 



;+
;   This is a heavily modified IDL ARROW procedure, with extra keywords added
;   and the COLOR keyword modified to accept color names. It assumes drawing
;   in the device coordinate space, unless the DATA or NORMALIZED keywords
;   are set.
; 
;   Copyright (c) 1993-2004, Research Systems, Inc.  All rights reserved.
;   
; :Params:
;     x0: in, required, type=float
;         The X value at the butt end of the arrow.
;     x1: in, required, type=float
;         The X value at the tip end of the arrow.
;     y0: in, required, type=float
;         The Y value at the butt end of the arrow.
;     y1: in, required, type=float
;         The Y value at the tip end of the arrow.
;
; :Keywords:
;     color: in, optional, type=string, default="opposite"
;        The name of the color to draw the grid lines in. 
;     data: in, optional, type=boolean, default=0
;        Set this keyword to draw in the data coordinate space.
;     linestyle: in, optional, type=integer, default=0
;        The graphics linestyle to draw the vector in.
;     normalized: in, optional, type=boolean, default=0
;        Set this keyword to draw in the normalized data coordinate space.
;     thick: in, optional, type=integer, default=1
;        Set this keyword to the thickness of the line used to draw the grid.
;     solid: in, optional, type=boolean, default=0
;        Set this keyword to fill the arrow head with a solid color. Otherwise,
;        draw the arrow head as an outline.
;     _extra: in, optional
;        Any keywords appropriate PlotS or PolyFill.
;-
PRO cgMapVector::DrawArrow, x0, y0, x1, y1, $
   COLOR = color, $
   DATA = data, $
   HSIZE = hsize, $
   HTHICK = hthick, $
   LINESTYLE=linestyle, $
   NORMALIZED = norm, $
   THICK = thick, $
   SOLID = solid, $
   _EXTRA=extra

    COMPILE_OPT idl2
    
    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF

    ;  Set up keyword params
    IF N_Elements(thick) EQ 0 THEN thick = 1.
    IF N_Elements(hthick) EQ 0 THEN hthick = thick
    
             ;Head size in device units
    IF N_Elements(hsize) EQ 0 THEN arrowsize = !d.x_size/50. * (hthick/2. > 1) $
        ELSE arrowsize = float(hsize)
    IF N_Elements(color) EQ 0 THEN color = "opposite"
    
    ; If arrowsize GT 15, THEN use 20% arrow. Otherwise use 30%.
    IF arrowsize LT 15 THEN BEGIN
       mcost = -0.866D
       sint = 0.500D
       msint = -sint
    ENDIF ELSE BEGIN
       mcost = - 0.939693D
       sint = 0.342020D
       msint = -sint
    ENDELSE
    
    ; Do this in decomposed color, if possible.
    SetDecomposedState, 1, CURRENT=currentState
    
    FOR i = 0L, N_Elements(x0)-1 DO BEGIN   ;Each vector
       IF Keyword_Set(data) THEN $   ;Convert?
           p = Convert_Coord([x0[i],x1[i]],[y0[i],y1[i]], /data, /to_dev) $
       ELSE IF Keyword_Set(norm) THEN $
           p = Convert_Coord([x0[i],x1[i]],[y0[i],y1[i]], /norm, /to_dev) $
       ELSE p = [[x0[i], y0[i]],[x1[i], y1[i]]]
    
       xp0 = p[0,0]
       xp1 = p[0,1]
       yp0 = p[1,0]
       yp1 = p[1,1]
    
       dx = xp1 - xp0
       dy = yp1 - yp0
       zz = SQRT(dx^2d + dy^2d)  ;Length
    
       IF zz gt 0 THEN BEGIN
         dx = dx/zz     ;Cos th
         dy = dy/zz     ;Sin th
       ENDIF ELSE BEGIN
         dx = 1.
         dy = 0.
         zz = 1.
       ENDELSE
       IF arrowsize gt 0 THEN a = arrowsize $  ;a = length of head
       ELSE a = -zz * arrowsize
    
       xxp0 = xp1 + a * (dx*mcost - dy * msint)
       yyp0 = yp1 + a * (dx*msint + dy * mcost)
       xxp1 = xp1 + a * (dx*mcost - dy * sint)
       yyp1 = yp1 + a * (dx*sint  + dy * mcost)
       
    
       IF Keyword_Set(solid) THEN BEGIN   ;Use polyfill?
         b = a * mcost*.9d ;End of arrow shaft (Fudge to force join)
         Plots, [xp0, xp1+b*dx], [yp0, yp1+b*dy], /DEVICE, $
            COLOR = cgColor(color), THICK = thick, LINESTYLE=linestyle, _Extra=extra
         Polyfill, [xxp0, xxp1, xp1, xxp0], [yyp0, yyp1, yp1, yyp0], $
            /DEVICE, COLOR = cgColor(color)
       ENDIF ELSE BEGIN
         Plots, [xp0, xp1], [yp0, yp1], /DEVICE, COLOR = cgColor(color), THICK = thick, $
            LINESTYLE=linestyle, _Extra=extra
         Plots, [xxp0,xp1,xxp1],[yyp0,yp1,yyp1], /DEVICE, COLOR = cgColor(color), $
            THICK = hthick, LINESTYLE=linestyle, _Extra=extra
       ENDELSE
    ENDFOR
    
    ; Restore color state.
    SetDecomposedState, currentState
    
END


;+--------------------------------------------------------------------------
;   Draws the vectors on the map projection.
;---------------------------------------------------------------------------
PRO cgMapVector::Draw

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    ; You have to have data to plot. If not exit quietly.
    IF (N_Elements(*self.lons) EQ 0) OR (N_Elements(*self.lats) EQ 0) THEN RETURN
    IF (N_Elements(*self.u) EQ 0) OR (N_Elements(*self.v) EQ 0) THEN RETURN
    
    ; If the vectors don't all have the same number of elements, there is an error.
    IF N_Elements(*self.lons) NE N_Elements(*self.lats) THEN BEGIN
        Message, 'The number of elements in the latitude and longitude arrays must be the same.'
    ENDIF
    IF N_Elements(*self.u) NE N_Elements(*self.v) THEN BEGIN
        Message, 'The number of elements in the U and V arrays must be the same.'
    ENDIF
    IF N_Elements(*self.lons) NE N_Elements(*self.v) THEN BEGIN
        Message, 'The number of elements in the lon, lat, u, and v arrays must be the same.'
    ENDIF
   
    
    ; Find a map structure, IF you can.
    IF Obj_Valid(self.mapCoord) THEN BEGIN
        mapStruct = self.mapCoord -> GetMapStruct() 
        self.mapCoord -> Draw, /NoGraphics
    ENDIF ELSE Message, 'There is no valid map object from which a map structure can be obtained.'

    ; If you have a map structure, then determine if the locations to plot
    ; are in lat/lon or UV coordinate space. The MapCoord object sets up
    ; a UV coordinate space. The locations to be plotted here are in lat/lon
    ; space, so they have to be converted to XY space to be plotted.
    IF N_Elements(mapStruct) NE 0 THEN BEGIN
    
        ; If the "lons and lats" are already in UVCOORDS, leave them alone.
        IF self.uvcoords THEN BEGIN
            lon = *self.lons
            lat = *self.lats
        ENDIF ELSE BEGIN
        
            ; Otherwise, convert them, since the map is *always* in UVCoords.
            uv = MAP_PROJ_FORWARD(*self.lons, *self.lats, MAP_STRUCTURE=mapStruct)
            lon = Reform(uv[0,*])
            lat = Reform(uv[1,*])
        ENDELSE
     ENDIF ELSE BEGIN
        lon = *self.lons
        lat = *self.lats
    ENDELSE
    
    ; Do we have to assign a value to length?
    IF ~Finite(self.length) THEN BEGIN
        self.mapCoord -> GetProperty, XRANGE=xr
        length = ABS(xr[1] - xr[0]) / 100.0
    ENDIF ELSE length = self.length
    
    ; Scale the U and V values by the length.
    uscaled = *self.u * length ;Scale_Vector(*self.u, 0, length)
    vscaled = *self.v * length ;Scale_Vector(*self.v, 0, length)
    
    ; If clip is not defined, then set it here.
    IF Total(self.clip) EQ 0 $
        THEN clip = [!X.CRange[0], !Y.CRange[0], !X.CRange[1], !Y.CRange[1]] $
        ELSE clip = self.clip
        
    ; Calculate the endpoint of the arrow and draw it.
    IF Ptr_Valid(self.palette) THEN BEGIN
        TVLCT, r, g, b, /Get
        TVLCT, *self.palette
    ENDIF
    FOR j=0L,N_Elements(*self.u)-1 DO BEGIN
        x0 = lon[j]
        y0 = lat[j]
        x1 = x0 + uscaled[j]
        y1 = y0 + vscaled[j]
        IF self.color EQ "" THEN color = (*self.magcolors)[j] ELSE color = self.color
        self -> DrawArrow, x0, y0, x1, y1, HSIZE=self.hsize, CLIP=clip, THICK=self.thick, $
           HTHICK=self.thick, LENGTH=length, COLOR=color, SOLID=self.solid, $
           _EXTRA=extrakeywords, /DATA, LINESTYLE=self.linestyle
    ENDFOR
    IF Ptr_Valid(self.palette) THEN TVLCT, r, g, b
END 

    
;+--------------------------------------------------------------------------
; This method allows the user to get various properties of the object. 
; The same keywords that are used for the INIT method can be used here.
;       
; :Keywords:
;     clip: out, optional, type=fltarr(4)
;        The coordinates of a rectangle used to clip the graphics output. 
;        The rectangle is specified as a vector of the form [X0, Y0, X1, Y1], 
;        giving coordinates of the lower left and upper right corners, 
;        respectively. The default clipping rectangle is the plot window set
;        up by the cgMap object. 
;     color: out, optional, type=string, default="opposite"
;        The name of the color to draw the grid lines in. 
;     hsize: out, optional, type=float, default=0.35
;        The value of this keyword sets the length of the arrowhead. See the documenation
;        for the ARROW command ind IDL for further explanation.
;     lats: out, optional, type=float
;        The latitude values where the vector is to be drawn.
;     length, out, optional, type=float
;        The U and V vectors are mutiplied by LENGTH before they are used
;        to calculate the (x1,y1) endpoint of the vector. By default, the length is set
;        to 1/100th of the XRANGE of the MapCoord object. This means that the maximum
;        length of a vector will be approximately LENGTH * SQRT(2).
;     linestyle: out, optional, type=integer, default=1 
;        Set this keyword to the type of linestyle desired. See Graphics Keywords in
;        the on-line help for additional information.
;     lons: out, optional, type=float
;        The longitude values where the vector is to be drawn.
;     noclip: out, optional, type=boolean, default=0
;        Set this keyword to supress clipping of the plot.
;     mapcoord: out, optional, type=object
;        The map coordinate for the object.
;     t3d: out, optional, type=boolean, default=0
;        Set this graphics keyword if you wish to draw using the T3D transformation matrix.
;     thick: out, optional, type=integer, default=1
;        Set this keyword to the thickness of the line used to draw the grid.
;     umagnitude: out, optional, type=float
;        The magnitude of the vector in the U direction.
;     uvcoords: out, optional, type=boolean, default=0
;        Set this keyword if the LONS and LATS are specified in UV (XY) coordinates, rather than
;        longitude and latitude coordinates.
;     vmagnitude: out, optional, type=float
;        The magnitude of the vector in the V direction.
;     zvalue: out, optional, type=float, default=0.0
;        Set this keyword to the ZVALUE where the output should be drawn.
;     _ref_extra: in, optional
;        Any keywords appropriate for superclass objects are also permitted.
;
;-----------------------------------------------------------------------------------------------
PRO cgMapVector::GetProperty, $
    CLIP=clip, $
    COLOR=color, $
    HSIZE=hsize, $
    LATS=lats, $
    LENGTH=length, $
    LINESTYLE=linestyle, $
    LONS=lons, $
    MAPCOORD=mapcoord, $
    NOCLIP=noclip, $
    SOLID=solid, $
    T3D=t3d, $
    THICK=thick, $
    UMAGNITUDE=u, $
    UVCOORDS=uvcoords, $
    VMAGNITUDE=v, $
    ZVALUE=zvalue, $
    _REF_EXTRA=extra

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    clip = self.clip
    color = self.color
    hsize = self.hsize
    length = self.length
    linestyle = self.linestyle
    noclip = self.noclip
    solid  = self.solid
    t3d = self.t3d
    thick = self.thick
    uvcoords = self.uvcoords
    IF Arg_Present(lats) THEN lats = *self.lats
    IF Arg_Present(lons) THEN lons = *self.lons
    IF Arg_Present(u) THEN u = *self.u
    IF Arg_Present(v) THEN v = *self.v
    zvalue = self.zvalue
    mapCoord = self.mapCoord
    
    IF N_Elements(extra) NE 0 THEN self -> cgContainer::GetProperty, _EXTRA=extra
    
END 

    
;+--------------------------------------------------------------------------
;   This method allows the user to set various properties of the object. 
;   The same keywords that are used for the INIT method can be used here.
;       
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        If this keyword is set, the object is added to the resizeable graphics
;        window, cgWindow. The DRAW method of the object is called in cgWindow.
;     clip: in, optional, type=fltarr(4)
;        The coordinates of a rectangle used to clip the graphics output. 
;        The rectangle is specified as a vector of the form [X0, Y0, X1, Y1], 
;        giving coordinates of the lower left and upper right corners, 
;        respectively. The default clipping rectangle is the plot window set
;        up by the cgMap object. 
;     color: in, optional, type=string, default="opposite"
;        The name of the color to draw the grid lines in. 
;     hsize: in, optional, type=float, default=0.35
;        The value of this keyword sets the length of the arrowhead. See the documenation
;        for the ARROW command ind IDL for further explanation.
;     lats: in, optional, type=float
;        The latitude values where the vector is to be drawn.
;     length, in, optional, type=float
;        The U and V vectors are mutiplied by LENGTH before they are used
;        to calculate the (x1,y1) endpoint of the vector. By default, the length is set
;        to 1/100th of the XRANGE of the MapCoord object. This means that the maximum
;        length of a vector will be approximately LENGTH * SQRT(2).
;     linestyle: out, optional, type=integer, default=1 
;        Set this keyword to the type of linestyle desired. See Graphics Keywords in
;        the on-line help for additional information.
;     lons: in, optional, type=float
;        The longitude values where the vector is to be drawn.
;     noclip: in, optional, type=boolean, default=0
;        Set this keyword to supress clipping of the plot.
;     mapcoord: in, optional, type=object
;        The map coordinate for the object.
;     palette: in, optional, type=byte
;        A (256x3) color palette containing the RGB color vectors to use for coloring the vectors
;        according to the magitude of the vectors. If the color palette is not 256 colors in length
;        then the magitude is scaled into the number of colors available. If a color palette is
;        used, then the `Color` keyword is ignored.
;     t3d: in, optional, type=boolean, default=0
;        Set this graphics keyword if you wish to draw using the T3D transformation matrix.
;     thick: in, optional, type=integer, default=1
;        Set this keyword to the thickness of the line used to draw the grid.
;     umagnitude: in, optional, type=float
;        The magnitude of the vector in the U direction.
;     uvcoords: in, optional, type=boolean, default=0
;        Set this keyword if the LONS and LATS are specified in UV (XY) coordinates, rather than
;        longitude and latitude coordinates.
;     vmagnitude: in, optional, type=float
;        The magnitude of the vector in the V direction.
;     zvalue: in, optional, type=float, default=0.0
;        Set this keyword to the ZVALUE where the output should be drawn.
;     _extra: in, optional
;        Any keywords appropriate for superclass objects are also permitted.
;
;-----------------------------------------------------------------------------------------------
PRO cgMapVector::SetProperty, $
    ADDCMD=addcmd, $
    CLIP=clip, $
    COLOR=color, $
    HSIZE=hsize, $
    LATS=lats, $
    LENGTH=length, $
    LINESTYLE=linestyle, $
    LONS=lons, $
    MAPCOORD=mapCoord, $
    NOCLIP=noclip, $
    PALETTE=palette, $
    SOLID=solid, $
    T3D=t3d, $
    THICK=thick, $
    UMAGNITUDE=u, $
    UVCOORDS=uvcoords, $
    VMAGNITUDE=v, $
    ZVALUE=zvalue, $
    _EXTRA=extra

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    IF N_Elements(lon) NE 0 THEN BEGIN
    ;print, 'lon:', lon
        *self.lons = lons
    ENDIF
    IF N_Elements(lat) NE 0 THEN BEGIN
        *self.lats = lats
    ;print, 'lat:',  lats
    ENDIF
    IF N_Elements(u) NE 0 THEN BEGIN
    ;print, 'u:', u
        *self.u = u
    ENDIF
    IF N_Elements(v) NE 0 THEN BEGIN
        *self.v = v
    ;print, 'v',  v
    ENDIF

    IF N_Elements(clip) NE 0 THEN self.clip = clip
    IF N_Elements(color) NE 0 THEN self.color = color
    IF N_Elements(hsize) NE 0 THEN self.hsize = hsize
    IF N_Elements(length) NE 0 THEN self.length = length
    IF N_Elements(mapCoord) NE 0 THEN self.mapCoord = mapCoord
    IF N_Elements(linestyle) NE 0 THEN self.linestyle = linestyle
    IF N_Elements(noclip) NE 0 THEN self.noclip = noclip
    IF N_Elements(solid) NE 0 THEN self.solid = Keyword_Set(solid)
    IF N_Elements(t3d) NE 0 THEN self.t3d = t3d
    IF N_Elements(thick) NE 0 THEN self.thick = thick
    IF N_Elements(zvalue) NE 0 THEN self.zvalue = zvalue
    
    ; Do you have a color palette?
    IF N_Elements(palette) NE 0 THEN BEGIN
       Ptr_Free, self.palette
       Ptr_Free, self.magcolors
       self.color = ""
       s = Size(palette, /DIMENSIONS)
       IF s[1] GT s[0] THEN palette = Transpose(palette)
       s = Size(palette, /DIMENSIONS)
       ncolors = s[0]
       self.palette = Ptr_New(palette)
       self.magcolors = Ptr_New(BytScl(Sqrt((*self.v)^2 + (*self.u)^2), TOP=ncolors-1))
    ENDIF

    IF N_Elements(extra) NE 0 THEN self -> cgContainer::SetProperty, _EXTRA=extra

END 


;+--------------------------------------------------------------------------
;   This is the clean-up routine for the object.
;---------------------------------------------------------------------------
PRO cgMapVector::CLEANUP

    Ptr_Free, self.lons
    Ptr_Free, self.lats
    Ptr_Free, self.u
    Ptr_Free, self.v
    Ptr_Free, self.palette
    Ptr_Free, self.magcolors
    
    self -> cgContainer::CLEANUP
END


;+--------------------------------------------------------------------------
;   This is the class definition module. 
;
; :Params:
;    class: out, optional, type=structure
;       Occasionally, it is useful to have an object class definition as
;       a structure variable. Using this output keyword will allow that.
;---------------------------------------------------------------------------
PRO cgMapVector__DEFINE, class

    class = { cgMapVector, $
              lons: Ptr_New(), $      
              lats: Ptr_New(), $ 
              u: Ptr_New(), $
              v: Ptr_New(), $  
              length: 0.0D, $
              clip: DblArr(4),$   
              color: "", $
              hsize: 0.0, $
              linestyle: 0, $
              magcolors: Ptr_New(), $
              noclip: 0B, $
              palette: Ptr_New(), $
              solid: 0, $
              t3d: 0B, $
              thick: 0, $
              uvcoords: 0B, $
              zvalue: 0.0, $
              mapCoord: Obj_New(), $
              INHERITS cgContainer $
            }

END ; -------------------------------------------------------------------------------------