; docformat = 'rst'
;
; NAME:
;   cgMapPlotS
;
; PURPOSE:
;   This object is a wrapper for the cgPlotS routine in IDL. It provides a simple 
;   way to allow lines and symbols to be placed as annotations on map projections
;   created with the cgMap coordinate object.
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
;   This object is a wrapper for the cgPlotS routine in IDL. It provides a simple 
;   way to allow lines and symbols to be placed as annotations on map projections
;   created with the cgMap coordinate object. Note that it may be easier and more
;   convenient to use cgPlotS, if you don't need a complete object solution.
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
;        Never tested, for some reason. Now, several bugs have been fixed and the 
;           program works as advertised. 17 Sept 2012. DWF.
;                
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;---------------------------------------------------------------------------
;
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
;        The name of the color to draw the grid lines in. 
;     lats: in, optional, type=float
;        The latitude values to draw.
;     linestyle: out, optional, type=integer, default=1 
;        Set this keyword to the type of linestyle desired. See Graphics Keywords in
;        the on-line help for additional information.
;     lons: in, optional, type=float
;        The longitude values to draw.
;     noclip: in, optional, type=boolean, default=0
;        Set this keyword to supress clipping of the plot.
;     psym: in, optional, type=integer, default=0
;        The plotting symbol to use for the plot. Can use any symbol available in
;        the Coyote Library routine SYMCAT.
;     symsize: in, optional, type=float, default=1.0
;        Set this keyword to the size of symbols.
;     t3d: in, optional, type=boolean, default=0
;        Set this graphics keyword if you wish to draw using the T3D transformation matrix.
;     thick: in, optional, type=integer, default=1
;        Set this keyword to the thickness of the line used to draw the grid.
;     uvcoords: in, optional, type=boolean, default=0
;        Set this keyword if the LONS and LATS are specified in UV (XY) coordinates, rather than
;        longitude and latitude coordinates.
;     zvalue: in, optional, type=float, default=0.0
;        Set this keyword to the ZVALUE where the output should be drawn.
;     _extra: in, optional
;        Any keywords appropriate for superclass objects are also permitted.
;---------------------------------------------------------------------------
FUNCTION cgMapPlotS::INIT, mapCoord, $
    ADDCMD=addcmd, $
    CLIP=clip, $
    COLOR=color, $
    LATS=lats, $
    LINESTYLE=linestyle, $
    LONS=lons, $
    NOCLIP=noclip, $
    PSYM=psym, $
    SYMSIZE=symsize, $
    T3D=t3d, $
    THICK=thick, $
    UVCOORDS=uvcoords, $
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
     ok = self -> cgContainer::INIT(_EXTRA=extra) 
     IF ~ok THEN RETURN, 0

    ; Default values.
    SetDefaultValue, color, 'opposite'
    SetDefaultValue, linestyle, 0
    SetDefaultValue, noclip, 1
    SetDefaultValue, psym, 0
    SetDefaultValue, symsize, 1.0
    SetDefaultValue, t3d, 0
    SetDefaultValue, thick, 1.0
    SetDefaultValue, uvcoords, 0B
    SetDefaultValue, zvalue, 0.0
    IF N_Elements(clip) NE 0 THEN self.clip = clip
    self.color = color
    self.linestyle = linestyle
    self.noclip = noclip
    self.psym = psym
    self.symsize = symsize
    self.t3d = t3d
    self.thick = thick
    self.uvcoords = uvcoords
    self.zvalue = zvalue
    
    ; If no longitudes yet, create a valid pointer to them.
    IF N_Elements(lons) EQ 0 $
        THEN self.lons = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.lons = Ptr_New(lons)
    
    ; If no latitudes yet, create a valid pointer to them.
    IF N_Elements(lats) EQ 0 $
        THEN self.lats = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.lats = Ptr_New(lats)
        
    ; Store the map object. 
    self.mapCoord = mapCoord
        
    ; Need to add this command to a resizeable cgWindow?
    IF Keyword_Set(addcmd) THEN self -> AddCmd
    
    RETURN, 1
    
END 


;+--------------------------------------------------------------------------
;   Adds the object as a command (the DRAW method is called) in a cgWindow 
;   resizeable graphics window. 
;
;---------------------------------------------------------------------------
PRO cgMapPlotS::AddCmd

   cgWindow, "Draw", self, /Method, /AddCmd 
   
END 


;+--------------------------------------------------------------------------
;   Draws the line or symbol on the map projection.
;---------------------------------------------------------------------------
PRO cgMapPlotS::Draw

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    ; You have to have data to plot. If not exit quietly.
    IF (N_Elements(*self.lons) EQ 0) OR (N_Elements(*self.lats) EQ 0) THEN RETURN
    
    ; Find a map structure, if you can.
    IF Obj_Valid(self.mapCoord) THEN BEGIN
        mapStruct = self.mapCoord -> GetMapStruct() 
        self.mapCoord -> Draw, /NoGraphics
    ENDIF ELSE Message, 'There is no valid map object from which a map structure can be obtained.'

    ; If you have a map structure, then determine if the points to plot
    ; are in lat/lon or UV coordinate space. The cgMap object sets up
    ; a UV coordinate space. The values to be plotted here are in lat/lon
    ; space, so they have to be converted to XY space to be plotted.
    IF N_Elements(mapStruct) NE 0 THEN BEGIN
    
        ; If the "lons and lats" are already in UVCOORDS, leave them alone.
        IF self.uvcoords THEN BEGIN
            lons = *self.lons
            lats = *self.lats
        ENDIF ELSE BEGIN
        
            ; Otherwise, convert them, since the map is *always* in UVCoords.
            uv = MAP_PROJ_FORWARD(*self.lons, *self.lats, MAP_STRUCTURE=mapStruct)
            lons = Reform(uv[0,*])
            lats = Reform(uv[1,*])
        ENDELSE
     ENDIF ELSE BEGIN
        lons = *self.lons
        lats = *self.lats
    ENDELSE
    
;    ; Accommodate SYMCAT symbols
;    IF self.psym GE 0 THEN psym = SymCat(self.psym) ELSE psym = (-1) * SymCat(Abs(self.psym))
;    
    ; If clip is not defined, then set it here.
    IF Total(self.clip) EQ 0 $
        THEN clip = [!X.CRange[0], !Y.CRange[0], !X.CRange[1], !Y.CRange[1]] $
        ELSE clip = self.clip

    ; Draw the lines or symbols.
    cgPlotS, lons, lats,  $
        CLIP=clip, $
        COLOR=self.color, $
        LINESTYLE=self.linestyle, $
        NOCLIP=self.noclip, $
        PSYM=self.psym, $
        SYMSIZE=self.symsize, $
        T3D=self.t3d, $
        THICK=self.thick, $
        Z=self.zvalue

END 

    
;+--------------------------------------------------------------------------
;   This method allows the user to get various properties of the object. 
;   The same keywords that are used for the INIT method can be used here.
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
;     lats: out, optional, type=float
;        The latitude values to draw.
;     linestyle: out, optional, type=integer, default=1 
;        Set this keyword to the type of linestyle desired. See Graphics Keywords in
;        the on-line help for additional information.
;     lons: out, optional, type=float
;        The longitude values to draw.
;     mapcoord: out, optional, type=object
;        Get the map coordinate object.
;     noclip: out, optional, type=boolean, default=0
;        Set this keyword to supress clipping of the plot.
;     psym: out, optional, type=integer, default=0
;        The plotting symbol to use for the plot. Can use any symbol available in
;        the Coyote Library routine SYMCAT.
;     symsize: out, optional, type=float, default=1.0
;        Set this keyword to the size of symbols.
;     t3d: out, optional, type=boolean, default=0
;        Set this graphics keyword if you wish to draw using the T3D transformation matrix.
;     thick: out, optional, type=integer, default=1
;        Set this keyword to the thickness of the line used to draw the grid.
;     uvcoords: out, optional, type=boolean, default=0
;        Set this keyword if the LONS and LATS are specified in UV (XY) coordinates, rather than
;        longitude and latitude coordinates.
;     zvalue: out, optional, type=float, default=0.0
;        Set this keyword to the ZVALUE where the output should be drawn.
;     _ref_extra: in, optional
;        Any keywords appropriate for superclass objects are also permitted.
;
;---------------------------------------------------------------------------
PRO cgMapPlotS::GetProperty, $
    CLIP=clip, $
    COLOR=color, $
    LATS=lats, $
    LINESTYLE=linestyle, $
    LONS=lons, $
    NOCLIP=noclip, $
    MAPCOORD=mapcoord, $
    PSYM=psym, $
    SYMSIZE=symsize, $
    T3D=t3d, $
    THICK=thick, $
    UVCOORDS=uvcoords, $
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
    linestyle = self.linestyle
    noclip = self.noclip
    psym = self.psym
    symsize = self.symsize
    t3d = self.t3d
    thick = self.thick
    uvcoords = self.uvcoords
    zvalue = self.zvalue
    lats = *self.lats
    lons = *self.lons
    
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
;     lats: in, optional, type=float
;        The latitude values to draw.
;     linestyle: out, optional, type=integer, default=1 
;        Set this keyword to the type of linestyle desired. See Graphics Keywords in
;        the on-line help for additional information.
;     lons: in, optional, type=float
;        The longitude values to draw.
;     noclip: in, optional, type=boolean, default=0
;        Set this keyword to supress clipping of the plot.
;     psym: in, optional, type=integer, default=0
;        The plotting symbol to use for the plot. Can use any symbol available in
;        the Coyote Library routine SYMCAT.
;     symsize: in, optional, type=float, default=1.0
;        Set this keyword to the size of symbols.
;     t3d: in, optional, type=boolean, default=0
;        Set this graphics keyword if you wish to draw using the T3D transformation matrix.
;     thick: in, optional, type=integer, default=1
;        Set this keyword to the thickness of the line used to draw the grid.
;     uvcoords: in, optional, type=boolean, default=0
;        Set this keyword if the LONS and LATS are specified in UV (XY) coordinates, rather than
;        longitude and latitude coordinates.
;     zvalue: in, optional, type=float, default=0.0
;        Set this keyword to the ZVALUE where the output should be drawn.
;     _extra: in, optional
;        Any keywords appropriate for superclass objects are also permitted.
;
;---------------------------------------------------------------------------
PRO cgMapPlotS::SetProperty, $
    ADDCMD=addcmd, $
    CLIP=clip, $
    COLOR=color, $
    LATS=lats, $
    LINESTYLE=linestyle, $
    LONS=lons, $
    NOCLIP=noclip, $
    PSYM=psym, $
    SYMSIZE=symsize, $
    T3D=t3d, $
    THICK=thick, $
    UVCOORDS=uvcoords, $
    ZVALUE=zvalue, $
    _EXTRA=extra

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    IF N_Elements(lons) NE 0 THEN BEGIN
        *self.lons = lons
    ENDIF
    IF N_Elements(lats) NE 0 THEN BEGIN
        *self.lats = lats
    ENDIF

    IF N_Elements(addcmd) NE 0 THEN self -> AddCmd
    IF N_Elements(clip) NE 0 THEN self.clip = clip
    IF N_Elements(color) NE 0 THEN self.color = color
    IF N_Elements(linestyle) NE 0 THEN self.linestyle = linestyle
    IF N_Elements(noclip) NE 0 THEN self.noclip = noclip
    IF N_Elements(psym) NE 0 THEN self.psym = psym
    IF N_Elements(symsize) NE 0 THEN self.symsize = symsize
    IF N_Elements(t3d) NE 0 THEN self.t3d = t3d
    IF N_Elements(thick) NE 0 THEN self.thick = thick
    IF N_Elements(zvalue) NE 0 THEN self.zvalue = zvalue

    IF N_Elements(extra) NE 0 THEN self -> cgContainer::SetProperty, _EXTRA=extra

END 


;+--------------------------------------------------------------------------
;   This is the clean-up routine for the object.
;---------------------------------------------------------------------------
PRO cgMapPlotS::CLEANUP

    Ptr_Free, self.lons
    Ptr_Free, self.lats
    
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
PRO cgMapPlotS__DEFINE, class

    class = { cgMapPlotS, $
              lons: Ptr_New(), $         
              lats: Ptr_New(), $        
              clip: DblArr(4),$   
              color: "", $               
              linestyle: 0, $
              noclip: 0B, $
              symsize: 0.0, $
              thick: 0, $
              psym: 0, $
              t3d: 0B, $
              zvalue: 0.0, $
              uvcoords: 0B, $
              mapCoord: Obj_New(), $
              INHERITS cgContainer $
            }

END ; -------------------------------------------------------------------------------------