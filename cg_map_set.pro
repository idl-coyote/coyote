PRO CG_Map_Set, p0lat, p0lon, rot, $
    ;   ********** Projection keywords:
      PROJECTION=proj, $              ;The projection index
      NAME=name, $                    ;The projection name as a string
      STEREOGRAPHIC = stereographic, $
      ORTHOGRAPHIC = orthographic, $
      CONIC = conic, $
      LAMBERT = lambert, $
      GNOMIC = gnomic, $
      AZIMUTHAL = azimuth, $
      SATELLITE = satellite, $      ;Also called general perspective proj
      CYLINDRICAL = cylindrical, $
      MERCATOR = mercator, $
      MILLER_CYLINDRICAL=miller, $
      MOLLWEIDE = mollweide, $
      SINUSOIDAL = sinusoidal, $
      AITOFF = aitoff, $            ;Original Aitoff projection
      HAMMER = hammer, $            ;This is really the Hammer-Aitoff projection
      ALBERS = albers, $            ;Alber's equal-area conic
      TRANSVERSE_MERCATOR = utm, $  ;Transverse mercator for an ellipsoid
      ROBINSON = robinson, $        ;Robinson projection
      GOODESHOMOLOSINE = goodes, $  ;Goode's Homolosine.  Origin should be (0,0).
                                    ; Also called Gauss-Krueger in Europe.
    ; **** Projection specific keywords:
      ELLIPSOID = ellips, $         ;Defines the ellipsoid for projections
                                    ;that handle the ellipsoidal case,
                                    ;currently Transverse Mercator and
                                    ;Lambert's conical.
                                    ;3 elements: a = equatorial radius (meters), e^2
                                    ;= eccentricity squared, k0 = scale on
                                    ;central meridian. e^2 = 2*f-f^2,
                                    ;where f = 1-b/a, b = polar radius
                                    ;Default is the Clarke 1866 ellipsoid, =
                                    ; [6378206.4d0, 0.00676866d0, 0.9996d0]
    CENTRAL_AZIMUTH=cent_azim, $    ;Angle of central azimuth (degrees) for:
                                    ; Cylindrical, Mercator, Miller,
                                    ; Mollw, Robinson, and Sinusoidal
                                    ; projections. Default = 0.
                                    ; The pole is placed at an azimuth of
                                    ; CENTRAL_AZIMUTH degrees CCW of north.
    STANDARD_PARALLELS = std_p, $   ;One or two standard parallels for conics,
                                    ; One or two element array.
    SAT_P = Sat_p, $                ;Satellite parameters: Altitude expressed in
                                    ; units of radii, Omega, and rotation.
                                    ; Rotation may also specified by the
                                    ; rot parameter.
    ;   ********** MAP_SET specific keywords:
    CLIP=clip, $                    ;Default = do map specific clipping,
                                    ; CLIP=0 to disable
    REVERSE=reverse, $              ;Reverse X and/or Y axes. 0=none, 1 = reverse X,
                                    ;  2 = reverse Y, 3 = reverse both.
    SCALE=scale, $                  ;Construct isotropic map with a given scale.
                                    ; Map Scale is 1:scale, otherwise fit to window
    ISOTROPIC = iso, $,             ;If set, make X and Y scales equal
    LIMIT = limit, $                ;4 or 8 point lat/lon limit:
                                    ; 4 point: [latmin, lonmin, latmax, lonmax]
                                    ; 8 point: [latLeft,lonLeft, latTop,
                                    ;       lonTop, LatRt, lonRt, LatBot, LonBot]
    ;   ********** MAP_SET graphics keywords:
    NOERASE=noerase, TITLE=title,$
      ADVANCE = advance, COLOR=color, POSITION = position, $
      NOBORDER=noborder, T3D=t3d, ZVALUE=zvalue, $
      CHARSIZE = charsize, XMARGIN=xmargin, YMARGIN=ymargin, $
    ;   ********** MAP_HORIZON keywords:
    HORIZON=horizon, E_HORIZON=ehorizon, $ ; E_HORIZON = structure containing
                                    ; extra keywords passed to the
                                    ; map_horizon procedure, e.g.
                                    ; E_HORIZON={FILL:1}
    ;   ********** MAP_CONTINENTS keywords:
    CONTINENTS = continents, E_CONTINENTS=econt, $ ;E_CONTINENTS = structure
                                    ; containing extra keywords to be
                                    ; passed to
                                    ; map_continents, e.g. E_CONTINENTS={FILL:1}
      USA=usa, HIRES = hires, $
      MLINESTYLE=mlinestyle, MLINETHICK=mlinethick, CON_COLOR=con_color, $
    
    ;   ********** MAP_GRID keywords:
      GRID=grid, E_GRID=egrid, $    ;E_GRID = extra keywords structure
      GLINESTYLE=glinestyle, GLINETHICK=glinethick, $
      LABEL=label, LATALIGN=latalign, LATDEL=latdel, LATLAB=latlab, $
      LONALIGN=lonalign, LONDEL=londel, LONLAB=lonlab, $
    
    ; Ignored, but here for compatibility:
      WHOLE_MAP=whole_map, $
      
    ; Coyote Graphics Keywords.
    BACKGROUND=background, $
    ERASE=erase, $
    ADDCMD=addcmd
    
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      IF N_Elements(thisState) NE 0 THEN SetDecomposedState, thisState
      RETURN
    ENDIF
      
    IF Keyword_Set(addcmd) THEN BEGIN
    
       cgWindow, 'CG_Map_Set', p0lat, p0lon, rot, $
          PROJECTION=proj, $              
          NAME=name, $                    
          STEREOGRAPHIC = stereographic, $
          ORTHOGRAPHIC = orthographic, $
          CONIC = conic, $
          LAMBERT = lambert, $
          GNOMIC = gnomic, $
          AZIMUTHAL = azimuth, $
          SATELLITE = satellite, $      
          CYLINDRICAL = cylindrical, $
          MERCATOR = mercator, $
          MILLER_CYLINDRICAL=miller, $
          MOLLWEIDE = mollweide, $
          SINUSOIDAL = sinusoidal, $
          AITOFF = aitoff, $            
          HAMMER = hammer, $            
          ALBERS = albers, $            
          TRANSVERSE_MERCATOR = utm, $  
          ROBINSON = robinson, $        
          GOODESHOMOLOSINE = goodes, $                                                    
          ELLIPSOID = ellips, $         
          CENTRAL_AZIMUTH=cent_azim, $    
          STANDARD_PARALLELS = std_p, $   
          SAT_P = Sat_p, $                
          CLIP=clip, $                    
          REVERSE=reverse, $              
          SCALE=scale, $                  
          ISOTROPIC = iso, $,             
          LIMIT = limit, $                
          NOERASE=noerase, TITLE=title,$
          ADVANCE = advance, COLOR=color, POSITION = position, $
          NOBORDER=noborder, T3D=t3d, ZVALUE=zvalue, $
          CHARSIZE = charsize, XMARGIN=xmargin, YMARGIN=ymargin, $
          HORIZON=horizon, E_HORIZON=ehorizon, $ ; E_HORIZON = structure containing
          CONTINENTS = continents, E_CONTINENTS=econt, $ ;E_CONTINENTS = structure
          USA=usa, HIRES = hires, $
          MLINESTYLE=mlinestyle, MLINETHICK=mlinethick, CON_COLOR=con_color, $
          GRID=grid, E_GRID=egrid, $    ;E_GRID = extra keywords structure
          GLINESTYLE=glinestyle, GLINETHICK=glinethick, $
          LABEL=label, LATALIGN=latalign, LATDEL=latdel, LATLAB=latlab, $
          LONALIGN=lonalign, LONDEL=londel, LONLAB=lonlab, $
          WHOLE_MAP=whole_map, $
          BACKGROUND=background, $
          ERASE=erase, $
          ADDCMD=1
          
       RETURN
       
    ENDIF
    
    ; If you specify a BACKGROUND keyword, then ERASE is set automatically.
    IF N_Elements(background) NE 0 THEN BEGIN
        CASE Size(background, /TNAME) OF
           'STRING':
           'LONG': 
           'BYTE': background = StrTrim(Fix(background), 2)
           ELSE: background = StrTrim(background,2)
        ENDCASE 
       IF N_Elements(noerase) EQ 0 THEN BEGIN
          noerase = 1
          eraseForBackground = 1
       ENDIF ELSE eraseForBackground = 0
    ENDIF ELSE background = 'white'
    
    ; If this is a graphics device, and there is no current graphics window,
    ; then set the erase flag.
    IF (!D.Name EQ 'WIN' || !D.Name EQ 'X') && (!D.Window LT 0) THEN BEGIN
        IF N_Elements(noerase) EQ 0 THEN BEGIN
            noerase = 1
            eraseForBackground = 1 
        ENDIF ELSE eraseForBackground = 0
    ENDIF
    
    IF N_Elements(color) NE 0 THEN BEGIN
        CASE Size(color, /TNAME) OF
           'STRING':
           'LONG': 
           'BYTE': color = StrTrim(Fix(color), 2)
           ELSE: color = StrTrim(color,2)
        ENDCASE 
    ENDIF ELSE color = "opposite"
    IF N_Elements(con_color) NE 0 THEN BEGIN
        CASE Size(con_color, /TNAME) OF
           'STRING':
           'LONG': 
           'BYTE': con_color = StrTrim(Fix(con_color), 2)
           ELSE: con_color = StrTrim(con_color,2)
        ENDCASE
    ENDIF ELSE BEGIN
       IF Keyword_Set(continents) THEN BEGIN
          IF (N_Elements(econt) NE 0) THEN BEGIN
             index = Where(Tag_Names(econt) EQ 'COLOR')
             IF index LT 0 THEN con_color = color
          ENDIF ELSE con_color = color
       ENDIF 
    ENDELSE
    IF N_Elements(position) EQ 0 THEN position = [0.075, 0.075, 0.925, 0.925]
    
    ; Try to do this in decomposed color, if possible.
    SetDecomposedState, 1, Current=thisState
    
    ; Need to erase? If you are using this keyword, then I assume
    ; you want the background color to show.
    IF N_Elements(erase) NE 0 THEN BEGIN
         noerase = 1 - Keyword_Set(erase)
         IF Keyword_Set(erase) THEN eraseForBackground = 1
    ENDIF
    
    ; Erase with a background color?
    IF Keyword_Set(eraseForBackground) THEN BEGIN
        cgErase, Color=background
        noerase = 1
    ENDIF
    
    ; Change colors into appropriate values, if needed.
    IF Size(color, /TNAME) EQ 'STRING' THEN color = cgColor(color)
    IF Size(con_color, /TNAME) EQ 'STRING' THEN con_color = cgColor(con_color)
    
    ; Input parameters must be defined because of the way MAP_SET is written.
    IF N_Elements(rot) EQ 0 THEN rot = 0.0d0
    IF N_Elements(p0lon) EQ 0 THEN p0lon = 0.0d0
    IF N_Elements(p0lat) EQ 0 THEN p0lat = 0.0d0    
    
    ; Call the MAP_SET procedure.
    Map_Set, p0lat, p0lon, rot, $
          PROJECTION=proj, $              
          NAME=name, $                    
          STEREOGRAPHIC = stereographic, $
          ORTHOGRAPHIC = orthographic, $
          CONIC = conic, $
          LAMBERT = lambert, $
          GNOMIC = gnomic, $
          AZIMUTHAL = azimuth, $
          SATELLITE = satellite, $      
          CYLINDRICAL = cylindrical, $
          MERCATOR = mercator, $
          MILLER_CYLINDRICAL=miller, $
          MOLLWEIDE = mollweide, $
          SINUSOIDAL = sinusoidal, $
          AITOFF = aitoff, $            
          HAMMER = hammer, $            
          ALBERS = albers, $            
          TRANSVERSE_MERCATOR = utm, $  
          ROBINSON = robinson, $        
          GOODESHOMOLOSINE = goodes, $                                                    
          ELLIPSOID = ellips, $         
          CENTRAL_AZIMUTH=cent_azim, $    
          STANDARD_PARALLELS = std_p, $   
          SAT_P = Sat_p, $                
          CLIP=clip, $                    
          REVERSE=reverse, $              
          SCALE=scale, $                  
          ISOTROPIC = iso, $,             
          LIMIT = limit, $                
          NOERASE=noerase, TITLE=title,$
          ADVANCE = advance, COLOR=color, POSITION = position, $
          NOBORDER=noborder, T3D=t3d, ZVALUE=zvalue, $
          CHARSIZE = charsize, XMARGIN=xmargin, YMARGIN=ymargin, $
          HORIZON=horizon, E_HORIZON=ehorizon, $ ; E_HORIZON = structure containing
          CONTINENTS = continents, E_CONTINENTS=econt, $ ;E_CONTINENTS = structure
          USA=usa, HIRES = hires, $
          MLINESTYLE=mlinestyle, MLINETHICK=mlinethick, CON_COLOR=con_color, $
          GRID=grid, E_GRID=egrid, $    ;E_GRID = extra keywords structure
          GLINESTYLE=glinestyle, GLINETHICK=glinethick, $
          LABEL=label, LATALIGN=latalign, LATDEL=latdel, LATLAB=latlab, $
          LONALIGN=lonalign, LONDEL=londel, LONLAB=lonlab, $
          WHOLE_MAP=whole_map
    
    ; Restore color state.
    SetDecomposedState, thisState
    
    END ;-------------------------------------------------------------------
