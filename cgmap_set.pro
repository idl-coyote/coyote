; docformat = 'rst'
;
; PURPOSE:
;   Provides a modified MAP_SET command that can work together with other Coyote Graphics
;   commands and be used in resizeable graphics windows.
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
;---------------------------------------------------------------------------
;+
;   Provides a modified MAP_SET command that can work together with other Coyote Graphics
;   commands and be used in resizeable graphics windows. Any keyword appropriate for MAP_SET,
;   MAP_CONTINENTS, or MAP_GRID can be used with this routine. Additional or modified 
;   keywords are documented.
;
; :Categories:
;    Graphics, Map Projections
;    
; :Params:
;     p0lat: in, optional, type=float
;         The center latitude of the map projection.
;     p0lon: in, optional, type=float
;         The center longitude of the map projection.
;     rot: in, optional, type=float, default=0.0
;         The angle through which the map projection should be rotated. Positive
;         values are in a clockwise direction.
;       
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        If this keyword is set, the command is added to the resizeable graphics
;        window, cgWindow. 
;     background: in, optional, type=string, default='white'
;        The name of the background color. Using this keyword automatically sets 
;        the ERASE keyword to 1.
;     color: in, optional, type=string, default='opposite'
;        The name of the drawing color for the program.
;        erase: in, optional, type=boolean, default=0
;        Set this keyword if you wish to the current graphics content of the window
;        before drawing the map projection. The graphics display will be erased
;        in the background color. This is a more intuitive way to set this behavior
;        than to set NOERASE to 0, which does the same thing.
;     continents: in, optional, type=boolean, default=0
;         Set this keyword to display continental outlines on the map projection.
;         Any keyword appropriate to the IDL command Map_Continents can be used to
;         modify the look of the continental outlines.
;     erase: in, optional, type=boolean, default=0
;         A more intuitive way to set NOERASE=0. If ERASE is set, the display window
;         is erased in the BACKGROUND color.
;     grid: in, optional, type=boolean, default=0
;         Set this keyword to display a map grid on the map projection. And keyword
;         appropriate to the IDL command Map_Grid can be used to modify the look of the
;         map grid.
;     onimage: in, optional, type=boolean, default=0
;        If this keyword is set, the position of the map projection in the graphics window
;        is obtained from the last image displayed with cgImage. This makes it extremely
;        easy to display an image and immediately set up a map projection space that will
;        allow you to annotate the image using map locations. This keyword is ignored if
;        the 'Position` keyword is used.
;     position: in, optional, type=FltArr(4)
;        The normalized position of the map projection space in the graphics window.
;        The default is [0.075, 0.075, 0.925, 0.925]
;     window: in, optional, type=boolean, default=0
;         If this keyword is set, the command replaces any commands in a current
;         cgWindow or it opens a new cgWindow and adds itself to it.
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
;     Written by David W. Fanning, 7 November 2011.
;     Added ONIMAGE keyword to allow the position to be specified at the last
;        image position from cgImage. 3 March 2012. DWF.
;     Several MAP_CONTINENTS keywords seem to have gone missing. Added COASTS,
;        COUNTRIES, FILL_CONTINENTS, and RIVERS. 4 Dec 2012. DWF.
;     Forgot to pass the "extra" information in (e.g., E_HORIZON keyword) to Map_Set. 18 Jan 2013. DWF.
;     HORIZON keyword not being passed along to Map_Set. 28 Feb 2013. DWF.
;     Forgot to look at !P.Multi before setting a POSITION. Fixed. 11 March 2013. DWF.
;     Modified the E_GRID and E_HORIZON structure code to allow color names. 29 April 2013. Joe Sapp.
;     In some situations, the "extra" continents structure was not being passed along. 29 May 2013. DWF.
;     Now, when "extra" structures are passed along, they can have duplicate fields. Put check in place. 22 June 2013. DWF.
;     Removed parameters E_GRID and E_CONTINENTS in call to MAP_SET so they don't interfere with later calls to cgMap_Grid and cgMap_Continents. 23 Nov 2015. Joe Sapp.
;     Fix typo in passing extra parameters to cgMap_Continents. 23 Nov 2015. Joe Sapp.
;
; :Copyright:
;     Copyright (c) 2011-2012, Fanning Software Consulting, Inc.
;-
;---------------------------------------------------------------------------
PRO cgMap_Set, p0lat, p0lon, rot, $
      ADDCMD=addcmd, $
      ADVANCE = advance,  $
      AITOFF = aitoff, $            
      ALBERS = albers, $            
      AZIMUTHAL = azimuth, $
      BACKGROUND=background, $
      BOX_AXES=box_axes, $
      CENTRAL_AZIMUTH=cent_azim, $
      CHARSIZE = charsize, $
      CLIP=clip, $
      COASTS=coasts, $
      COLOR=color, $
      CON_COLOR=con_color, $
      CONIC = conic, $
      CONTINENTS = continents, $
      COUNTRIES=countries, $
      CYLINDRICAL = cylindrical, $
      E_CONTINENTS=_econt, $ 
      E_HORIZON=_ehorizon, $
      E_GRID=_egrid, $      
      ELLIPSOID = ellips, $
      ERASE=erase, $
      FILL_CONTINENTS=fill_continents, $
      GLINESTYLE=glinestyle, $
      GLINETHICK=glinethick, $
      GRID=grid, $
      GOODESHOMOLOSINE = goodes, $  
      GNOMIC = gnomic, $
      HAMMER = hammer, $            
      HIRES = hires, $
      HORIZON=horizon, $
      ISOTROPIC = iso, $,             
      LAMBERT = lambert, $
      LABEL=label, $
      LATALIGN=latalign, $
      LATDEL=latdel, $
      LATLAB=latlab, $
      LIMIT = limit, $           
      LONALIGN=lonalign, $
      LONDEL=londel, $
      LONLAB=lonlab, $
      MERCATOR = mercator, $
      MILLER_CYLINDRICAL=miller, $
      MLINESTYLE=mlinestyle, $
      MLINETHICK=mlinethick, $
      MOLLWEIDE = mollweide, $
      NAME=name, $                    
      NOBORDER=noborder, $
      NOERASE=noerase, $
      ONIMAGE=onimage, $
      ORTHOGRAPHIC = orthographic, $
      POSITION = position, $
      PROJECTION=proj, $   
      RIVERS=rivers, $           
      REVERSE=reverse, $   
      ROBINSON = robinson, $         
      SAT_P = Sat_p, $ 
      SATELLITE = satellite, $       
      SCALE=scale, $ 
      STANDARD_PARALLELS = std_p, $
      STEREOGRAPHIC = stereographic, $
      SINUSOIDAL = sinusoidal, $
      T3D=t3d, $
      TITLE=title, $
      TRANSVERSE_MERCATOR = utm, $ 
      USA=usa, $
      WHOLE_MAP=whole_map, $
      WINDOW=window, $
      XMARGIN=xmargin, $
      YMARGIN=ymargin, $
      ZVALUE=zvalue
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = cgErrorMsg()
      IF N_Elements(thisState) NE 0 THEN cgSetColorState, thisState
      RETURN
    ENDIF
      
    ; Add this command to a cgWindow.
    IF Keyword_Set(addcmd) THEN BEGIN
    
       cgWindow, 'cgMap_Set', p0lat, p0lon, rot, $
          ADDCMD=1, $
          ADVANCE = advance,  $
          AITOFF = aitoff, $            
          ALBERS = albers, $            
          AZIMUTHAL = azimuth, $
          BACKGROUND=background, $
          BOX_AXES=box_axes, $
          CENTRAL_AZIMUTH=cent_azim, $
          CHARSIZE = charsize, $
          CLIP=clip, $
          COASTS=coasts, $
          COLOR=color, $
          CON_COLOR=con_color, $
          CONIC = conic, $
          CONTINENTS = continents, $
          COUNTRIES=countries, $
          CYLINDRICAL = cylindrical, $
          E_CONTINENTS=_econt, $ 
          E_HORIZON=_ehorizon, $
          E_GRID=_egrid, $      
          ELLIPSOID = ellips, $
          ERASE=erase, $
          FILL_CONTINENTS=fill_continents, $
          GLINESTYLE=glinestyle, $
          GLINETHICK=glinethick, $
          GRID=grid, $
          GOODESHOMOLOSINE = goodes, $  
          GNOMIC = gnomic, $
          HAMMER = hammer, $            
          HIRES = hires, $
          HORIZON=horizon, $
          ISOTROPIC = iso, $,             
          LAMBERT = lambert, $
          LABEL=label, $
          LATALIGN=latalign, $
          LATDEL=latdel, $
          LATLAB=latlab, $
          LIMIT = limit, $           
          LONALIGN=lonalign, $
          LONDEL=londel, $
          LONLAB=lonlab, $
          MERCATOR = mercator, $
          MILLER_CYLINDRICAL=miller, $
          MLINESTYLE=mlinestyle, $
          MLINETHICK=mlinethick, $
          MOLLWEIDE = mollweide, $
          NAME=name, $                    
          NOBORDER=noborder, $
          NOERASE=noerase, $
          ONIMAGE=onimage, $
          ORTHOGRAPHIC = orthographic, $
          POSITION = position, $
          PROJECTION=proj, $    
          RIVERS=rivers, $          
          REVERSE=reverse, $   
          ROBINSON = robinson, $         
          SAT_P = Sat_p, $ 
          SATELLITE = satellite, $       
          SCALE=scale, $ 
          STANDARD_PARALLELS = std_p, $
          STEREOGRAPHIC = stereographic, $
          SINUSOIDAL = sinusoidal, $
          T3D=t3d, $
          TITLE=title, $
          TRANSVERSE_MERCATOR = utm, $ 
          USA=usa, $
          WHOLE_MAP=whole_map, $
          XMARGIN=xmargin, $
          YMARGIN=ymargin, $
          ZVALUE=zvalue
          
       RETURN
       
    ENDIF
    
    ; Replace the commands in or create a cgWindow.
    IF Keyword_Set(window) THEN BEGIN
    
       currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
       IF wincnt EQ 0 THEN BEGIN
           cgWindow
           IF N_Elements(background) EQ 0 THEN background='white'
       ENDIF
       cgWindow, 'cgMap_Set', p0lat, p0lon, rot, $
          ADVANCE = advance,  $
          AITOFF = aitoff, $            
          ALBERS = albers, $            
          AZIMUTHAL = azimuth, $
          BACKGROUND=background, $
          BOX_AXES=box_axes, $
          CENTRAL_AZIMUTH=cent_azim, $
          CHARSIZE = charsize, $
          CLIP=clip, $
          COLOR=color, $
          COASTS=coasts, $
          CON_COLOR=con_color, $
          CONIC = conic, $
          CONTINENTS = continents, $
          COUNTRIES=countries, $
          CYLINDRICAL = cylindrical, $
          E_CONTINENTS=_econt, $ 
          E_HORIZON=_ehorizon, $
          E_GRID=_egrid, $      
          ELLIPSOID = ellips, $
          ERASE=erase, $
          FILL_CONTINENTS=fill_continents, $
          GLINESTYLE=glinestyle, $
          GLINETHICK=glinethick, $
          GRID=grid, $
          GOODESHOMOLOSINE = goodes, $  
          GNOMIC = gnomic, $
          HAMMER = hammer, $            
          HIRES = hires, $
          HORIZON=horizon, $
          ISOTROPIC = iso, $,             
          LAMBERT = lambert, $
          LABEL=label, $
          LATALIGN=latalign, $
          LATDEL=latdel, $
          LATLAB=latlab, $
          LIMIT = limit, $           
          LONALIGN=lonalign, $
          LONDEL=londel, $
          LONLAB=lonlab, $
          MERCATOR = mercator, $
          MILLER_CYLINDRICAL=miller, $
          MLINESTYLE=mlinestyle, $
          MLINETHICK=mlinethick, $
          MOLLWEIDE = mollweide, $
          NAME=name, $                    
          NOBORDER=noborder, $
          NOERASE=noerase, $
          ONIMAGE=onimage, $
          ORTHOGRAPHIC = orthographic, $
          POSITION = position, $
          PROJECTION=proj, $  
          RIVERS=rivers, $            
          REVERSE=reverse, $   
          ROBINSON = robinson, $         
          SAT_P = Sat_p, $ 
          SATELLITE = satellite, $       
          SCALE=scale, $ 
          STANDARD_PARALLELS = std_p, $
          STEREOGRAPHIC = stereographic, $
          SINUSOIDAL = sinusoidal, $
          T3D=t3d, $
          TITLE=title, $
          TRANSVERSE_MERCATOR = utm, $ 
          USA=usa, $
          WHOLE_MAP=whole_map, $
          XMARGIN=xmargin, $
          YMARGIN=ymargin, $
          ZVALUE=zvalue, $
          REPLACECMD=1
          
       RETURN
       
    ENDIF
    
    ; Erasing to get the background color right is going to be very important.
    ; Unfortunately, MAP_SET itself has an ERASE command in it that will result
    ; in a window with a BLACK background. We don't want this, so we have to call
    ; MAP_SET, when we do, with the NOERASE keyword set. We need a flag that 
    ; tells us when we want to do the erasing ourselves. Here we initialize the flag.
    eraseForBackground = 0
    
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
          IF (N_Elements(_econt) NE 0) THEN BEGIN
             tagnames = Tag_Names(_econt)
             index = Where(tagnames EQ 'COLOR')
             IF index LT 0 THEN BEGIN
                econt = _econt
                con_color = color
             ENDIF $
             ELSE BEGIN
                ; Create a copy of _econt to be passed on with an
                ; appropriate color.
                IF index EQ 0 then econt = {COLOR: 0L} $
                ELSE econt = Create_Struct(tagnames[0], _econt.(0))

                FOR j=1L,N_Elements(tagnames)-1 DO BEGIN
                   value = (j NE index) ? _econt.(j) : 0L
                   econt = Create_Struct(econt, $
                      tagnames[j], value)
                ENDFOR
                econt.color = cgColor(_econt.color)
             ENDELSE
          ENDIF ELSE con_color = color
       ENDIF 
    ENDELSE
    
    ; Make sure the extra continent structure is passed on.
    IF (N_Elements(_econt) NE 0) && (N_Elements(econt) EQ 0) THEN econt = _econt

    ; Fix color passed on to MAP_GRID, if needed.
    IF N_Elements(_egrid) NE 0 THEN BEGIN
       tagnames = Tag_Names(_egrid)
       index = Where(Tag_Names(_egrid) EQ 'COLOR')
       IF index LT 0 THEN egrid = _egrid $
       ELSE BEGIN
          ; Create a copy of _egrid to be passed on with an
          ; appropriate color.
          IF index EQ 0 then egrid = {COLOR: 0L} $
          ELSE egrid = Create_Struct(tagnames[0], _egrid.(0))

          FOR j=1L,N_Elements(tagnames)-1 DO BEGIN
             value = (j NE index) ? _egrid.(j) : 0L
             egrid = Create_Struct(egrid, $
                tagnames[j], value)
          ENDFOR
          egrid.color = cgColor(_egrid.color)
       ENDELSE
    ENDIF

    ; Fix color passed on to the HORIZON keyword, if needed.
    IF N_Elements(_ehorizon) NE 0 THEN BEGIN
       tagnames = Tag_Names(_ehorizon)
       index = Where(Tag_Names(_ehorizon) EQ 'COLOR')
       IF index LT 0 THEN ehorizon = _ehorizon $
       ELSE BEGIN
          ; Create a copy of _ehorizon to be passed on with an
          ; appropriate color.
          IF index EQ 0 then ehorizon = {COLOR: 0L} $
          ELSE ehorizon = Create_Struct(tagnames[0], _ehorizon.(0))

          FOR j=1L,N_Elements(tagnames)-1 DO BEGIN
             value = (j NE index) ? _ehorizon.(j) : 0L
             ehorizon = Create_Struct(ehorizon, $
                tagnames[j], value)
          ENDFOR
          ehorizon.color = cgColor(_ehorizon.color)
       ENDELSE
    ENDIF

    IF N_Elements(position) EQ 0 THEN BEGIN
    
        ; Are you putting this on an image? If so, get the position from
        ; the last image position in.
        IF Keyword_Set(onimage) THEN BEGIN
            COMMON FSC_$CGIMAGE, _cgimage_xsize, _cgimage_ysize, $
                                 _cgimage_winxsize, _cgimage_winysize, $
                                 _cgimage_position, _cgimage_winID, $
                                 _cgimage_current
            position = _cgimage_position
        ENDIF ELSE BEGIN
            IF Total(!P.Multi) LT 0 THEN position = [0.075, 0.075, 0.925, 0.925]
        ENDELSE
    ENDIF
    
    ; Try to do this in decomposed color, if possible.
    cgSetColorState, 1, Current=thisState
    
    ; If this is a graphics device, and there is no current graphics window,
    ; then set the erase flag.
    IF (!D.Name EQ 'WIN' || !D.Name EQ 'X') && (!D.Window LT 0) THEN BEGIN
        IF N_Elements(noerase) EQ 0 THEN BEGIN
            noerase = 1
            eraseForBackground = 1 
        ENDIF 
    ENDIF
    
    ; Need to erase? If you are using this keyword, then I assume
    ; you want the background color to show.
    IF N_Elements(erase) NE 0 THEN BEGIN
         noerase = 1 - Keyword_Set(erase)
         IF Keyword_Set(erase) THEN eraseForBackground = 1
    ENDIF
    
    ; We need the following two lines to avoid problems in MAP_SET with
    ; an ERASE command. We are going to do MAP_SET's erasing for it, then
    ; set the NOERASE=1 keyword on the MAP_SET call further down in the code.
    IF (!P.Multi[0] EQ 0) && Keyword_Set(advance) && ~eraseForBackground THEN eraseForBackground = 1
    IF (~Keyword_Set(noerase)) && (~Keyword_Set(advance)) && ~eraseForBackground THEN eraseForBackground = 1
    
    ; Erase with a background color?
    IF eraseForBackground THEN BEGIN
        cgErase, Color=background
        noerase = 1
    ENDIF
    
    ; Change color into appropriate value, if needed.
    IF Size(color, /TNAME) EQ 'STRING' THEN color = cgColor(color)
    
    ; Input parameters must be defined because of the way MAP_SET is written.
    IF N_Elements(rot) EQ 0 THEN rot = 0.0d0
    IF N_Elements(p0lon) EQ 0 THEN p0lon = 0.0d0
    IF N_Elements(p0lat) EQ 0 THEN p0lat = 0.0d0    
    
    ; Call the MAP_SET procedure.
    Map_Set, p0lat, p0lon, rot, $
        ADVANCE = advance,  $
        AITOFF = aitoff, $            
        ALBERS = albers, $            
        AZIMUTHAL = azimuth, $
        CENTRAL_AZIMUTH=cent_azim, $
        CHARSIZE = charsize, $
        COLOR=color, $
        CONIC = conic, $
        CYLINDRICAL = cylindrical, $
        E_HORIZON=ehorizon, $
        ELLIPSOID = ellips, $
        GOODESHOMOLOSINE = goodes, $  
        GNOMIC = gnomic, $
        HAMMER = hammer, $   
        HORIZON=horizon, $         
        ISOTROPIC = iso, $,             
        LAMBERT = lambert, $
        LIMIT = limit, $           
        MERCATOR = mercator, $
        MILLER_CYLINDRICAL=miller, $
        MOLLWEIDE = mollweide, $
        NAME=name, $                    
        NOBORDER=noborder, $
        NOERASE=1, $
        ORTHOGRAPHIC = orthographic, $
        POSITION = position, $
        PROJECTION=proj, $              
        REVERSE=reverse, $   
        ROBINSON = robinson, $         
        SAT_P = Sat_p, $ 
        SATELLITE = satellite, $       
        SCALE=scale, $ 
        STANDARD_PARALLELS = std_p, $
        STEREOGRAPHIC = stereographic, $
        SINUSOIDAL = sinusoidal, $
        T3D=t3d, $
        TITLE=title, $
        TRANSVERSE_MERCATOR = utm, $ 
        WHOLE_MAP=whole_map, $
        XMARGIN=xmargin, $
        YMARGIN=ymargin, $
        ZVALUE=zvalue
        
    ; Need continental outlines?
    IF Keyword_Set(continents) THEN BEGIN
    
       cgMap_Continents, $
         COASTS=kcoasts, $
         COLOR=con_color, $
         CONTINENTS = continents, $
         COUNTRIES=countries, $
         FILL_CONTINENTS=fill_continents, $
         HIRES=hires, $
         LIMITS = lim_u, $
         MAP_STRUCTURE=mapStructure, $
         ORIENTATION=orientation, $
         RIVERS=rivers, $
         SPACING=spacing, $
         T3D=T3D, $
         THICK=thick, $
         USA = usa, $
         ZVALUE=zvalue, $
         _EXTRA=econt

    ENDIF
    
    ; Need a map grid?
    IF Keyword_Set(grid) THEN BEGIN
    
       ; We have a couple of extra keyword structures that can come in
       ; here. We have to build a structure appropriately, depending upon
       ; what we have.
       CASE 1 OF
          (N_Elements(egrid) EQ 0) && (N_Elements(ehorizon) EQ 0):
          (N_Elements(egrid) EQ 0) && (N_Elements(ehorizon) NE 0): extra = ehorizon
          (N_Elements(egrid) NE 0) && (N_Elements(ehorizon) EQ 0): extra = egrid
          (N_Elements(egrid) NE 0) && (N_Elements(ehorizon) NE 0): BEGIN
              g_tagnames = Tag_Names(egrid)
              h_tagnames = Tag_Names(ehorizon)
              extra = egrid
              
              ; It is possible that these two structures have identical tags, if so
              ; we are going to skip them.
              FOR j=0L,N_Elements(tagnames)-1 DO BEGIN
                 check = Where(tagnames[j] EQ g_tagnames, count)
                 IF count EQ 0 THEN BEGIN
                    extra = Create_Struct(extra, tagnames[j], ehorizon.(j))
                 ENDIF
              ENDFOR
              END        
       ENDCASE
       
       cgMap_Grid, $
          BOX_AXES=box_axes, $
          CHARSIZE=charsize, $
          CLIP_TEXT=clip, $
          HORIZON=horizon, $
          LABEL=label, $
          LATALIGN=latalign, $
          LATDEL=latdel, $
          LATLAB=latlab, $
          LINESTYLE=linestyle, $
          LONALIGN=lonalign, $
          LONDEL=londel, $
          LONLAB=lonlab, $
          ORIENTATION=orientation, $
          THICK=thick, $
          _EXTRA=extra
          
    ENDIF

    ; Restore color state.
    cgSetColorState, thisState
    
    END ;-------------------------------------------------------------------
