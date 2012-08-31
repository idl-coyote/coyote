; docformat = 'rst'
;
; NAME:
;   cgMap_Continents
;
; PURPOSE:
;   Provides a simple wrapper to the MAP_CONTINENTS command in IDL that can be used
;   to draw map continental outlines in a device and color independent way with 
;   Coyote Graphics programs.
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
;   Provides a simple wrapper to the MAP_CONTINENTS command in IDL that can be used
;   to draw map continental outlines in a device and color independent way with 
;   Coyote Graphics programs.
;
; :Categories:
;    Graphics, Map Projections
;    
; :Examples:
;     To display a map of the Cuba and Haiti::
;         cgMap_Set, /Mercator, Limit=[5.0, -95, 35.0, -55.0]
;         cgMap_Continents, Background='dodger blue', /Continents, /Fill, Color='tan'
;         cgMap_Continents, /Countries, /USA, Color='navy', /Continents
;       
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        If this keyword is set, the object is added to the resizeable graphics
;        window, cgWindow. Note that a map projection command must be 
;        added to the window before this command is added to be effective.
;     background: in, optional, type=string
;        The name of the background color. A polygon of this color is drawn
;        in the map space set up by the map projection before continents and
;        other items are drawn.
;     coasts: in, optional, type=boolean, default=0
;        Set this keyword if you want coasts to be drawn. This keyword is ignored if using FILENAME.
;     color: in, optional, type=string, default='opposite'
;        The name of the drawing color for the program.
;     continents: in, optional, type=boolean  
;        Set this keyword if you want continental outlines to be drawn. This will be
;        set automatically if COASTS, COUNTRIES, RIVERS, AND USA keywords are all set
;        to zero. This keyword is ignored if using FILENAME.
;     countries: in, optional, type=boolean, default=0
;        Set this keyword to draw political boundaries.
;     fill_continents: in, optional, type=boolean, default=0
;        Set this keyword to draw filled polygons rather than outlines. If the value is
;        2, the continents are filled with lines rather than colors.
;     hires: in, optional, type=boolean, default=0
;        Set this keyword to use the high-resolution data supplied with IDL for MAP_CONTINENTS.
;        This keyword is ignored if using FILENAME.
;     limits: in, optional, type=fltarr(4)
;        Set to a four-element vector, [Latmin, LonMin, LatMax, LonMax] that describes the 
;        limit rectangle. Only plot line segments that pass through this rectangle.
;     linestyle:  in, optional, type=integer, default=0
;        Set to the type of linestyle in drawing outlines. Set solid lines by default.
;     map_structure: in, optional, type=struct/object
;        Set this keyword to a map structure as returned by Map_Proj_Init or to a map
;        coordinate object (i.e., cgMap) from which a map structure can be obtained.
;     mlinestyle:
;        This keyword depreciated in favor of LINESTYLE keyword.
;     mlinethick:
;        This keyword depreciated in favor of THICK keyword.
;     orientation: in, optional, type=float default=0.0
;        Set this keyword to the counterclockwise angle in degrees from horizontal that 
;        the line fill should be drawn. Only applies if the FILL_CONTINENTS keyword is 2.
;     rivers: in, optional, type=boolean, default=0  
;        Set this keyword if you wish to draw rivers. This keyword is ignored if using FILENAME.
;     spacing: in, optional, type=float, default=0.5
;        Set this keyword to the spacing, in centimeters, for a line fill. This keyword only has 
;        effect if the FILL_CONTINENTS keyword is set to 2.
;     t3d: in, optional, type=boolean, default=0  
;        Set this graphics keyword if you wish to draw outlines use the T3D transformation matrix.
;        This keyword is ignored if using FILENAME.
;     thick: in, optional, type=integer, default=!P.Thick
;        Set this keyword to the thickness of the lines that are drawn.
;     usa: in, optional, type=boolean, default=0  
;        Set this keyword if you wish do draw United States state boundaries. This keyword is 
;        ignored if using FILENAME.
;     zvalue: in, optional, type=float, default=0.0  
;        Set this keyword to the ZVALUE where the outlines should be drawn. Set to 0 by default.
;        This keyword is ignored if using FILENAME.
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
;    Change History::
;       Written by David W. Fanning, 7 November 2011.
;       Added an ERASE=0 to the /NOGRAPHICS keyword on the Draw method call to cgMap. 27 Dec 2011. DWF
;       Changed the default line thickness to !P.Thick to better support PostScript files. 28 Dec 2011. DWF.
;       Modified slightly to allow a three-element byte array to be used as the COLOR. 18 April 2012. DWF.
;       Added a BACKGROUND keyword. 31 Aug 2012. DWF.
;        
; :Copyright:
;    Copyright (c) 2011-2012, Fanning Software Consulting, Inc.
;---------------------------------------------------------------------------
PRO cgMap_Continents, $
    ADDCMD=addcmd, $
    BACKGROUND=background, $
    COASTS=kcoasts, $
    COLOR=color, $
    CONTINENTS=kcont, $
    COUNTRIES=kcountries, $
    FILL_CONTINENTS=kfill_continents, $
    HIRES=khires, $
    LIMITS = lim_u, $
    LINESTYLE=linestyle, $
    MAP_STRUCTURE=mapStruct, $
    MLINESTYLE = mlinestyle, $
    MLINETHICK = mlinethick, $
    ORIENTATION=orientation, $
    OUTLINE=outline, $
    RIVERS=krivers, $
    SPACING=spacing, $
    T3D=T3D, $
    THICK=thick, $
    USA = kusa, $
    ZVALUE=zvalue, $
    _EXTRA=extra
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      IF N_Elements(thisState) NE 0 THEN SetDecomposedState, thisState
      RETURN
    ENDIF
      
    IF Keyword_Set(addcmd) THEN BEGIN
    
       cgWindow, 'cgMap_Continents', $
          COASTS=kcoasts, $
          BACKGROUND=background, $
          COLOR=color, $
          CONTINENTS=kcont, $
          COUNTRIES=kcountries, $
          FILL_CONTINENTS=kfill_continents, $
          HIRES=khires, $
          LIMITS = lim_u, $
          LINESTYLE=linestyle, $
          MAP_STRUCTURE=mapStruct, $
          MLINESTYLE = mlinestyle, $
          MLINETHICK = mlinethick, $
          ORIENTATION=orientation, $
          RIVERS=krivers, $
          SPACING=spacing, $
          T3D=T3D, $
          THICK=thick, $
          USA = kusa, $
          ZVALUE=zvalue, $
          _STRICT_EXTRA=extra, $
          ADDCMD=1
          
       RETURN
       
    ENDIF
    
    ; Sort out linestyle and keyword values.
    IF (N_Elements(glinestyle) EQ 0) && (N_Elements(linestyle) EQ 0) THEN linestyle = 0
    IF (N_Elements(glinestyle) NE 0) && (N_Elements(linestyle) EQ 0) THEN linestyle = glinestyle
    IF N_Elements(linestyle) EQ 0 THEN linestyle = 0
    IF (N_Elements(glinethick) EQ 0) && (N_Elements(thick) EQ 0) THEN thick = !P.Thick
    IF (N_Elements(glinethick) NE 0) && (N_Elements(thick) EQ 0) THEN thick = glinethick
    IF N_Elements(thick) EQ 0 THEN thick = !P.Thick
    
    ; Need a color.
    IF N_Elements(color) NE 0 THEN BEGIN
        CASE Size(color, /TNAME) OF
           'STRING':
           'LONG': 
           'BYTE': BEGIN
              IF N_Elements(color) NE 3 THEN color = StrTrim(Fix(color), 2)
              END
           ELSE: color = StrTrim(color,2)
        ENDCASE 
    ENDIF ELSE color = "opposite"

    ; Try to do this in decomposed color, if possible.
    SetDecomposedState, 1, Current=thisState
    
    ; Change colors into appropriate values, if needed.
    IF Size(color, /TNAME) EQ 'STRING' THEN color = cgColor(color)
    IF (Size(color, /TNAME) EQ 'BYTE') AND (N_Elements(color) EQ 3) THEN color = cgColor(color)
    
   ; If you got a map object, use it to recover a map structure
   ; and to set up the map coordinate space.
   IF N_Elements(mapStruct) NE 0 THEN BEGIN
       IF Obj_Valid(mapStruct) THEN BEGIN
          mapObj = mapStruct
          thisMapStruct = mapObj -> GetMapStruct()           
          mapObj -> Draw, /NoGraphics, Erase=0
       ENDIF ELSE thisMapStruct = mapStruct
   ENDIF 
   
   ; Need a background color?
   IF Keyword_Set(background) NE 0 THEN BEGIN
      x = !X.Window
      y = !Y.Window
      cgColorFill, [x[0], x[1], x[1], x[0], x[0]], $
                   [y[0], y[0], y[1], y[1], y[0]], $
                   /NORMAL, Color=background
    ENDIF

    ; Call the IDL routine with default values.
    Map_Continents, $
       COASTS=kcoasts, $
       COLOR=color, $
       CONTINENTS = kcont, $
       COUNTRIES=kcountries, $
       FILL_CONTINENTS=kfill_continents, $
       HIRES=khires, $
       LIMITS = lim_u, $
       MAP_STRUCTURE=thisMapStruct, $
       MLINESTYLE = linestyle, $
       MLINETHICK = thick, $
       ORIENTATION=orientation, $
       RIVERS=krivers, $
       SPACING=spacing, $
       T3D=T3D, $
       USA = kusa, $
       ZVALUE=zvalue
          
    ; Restore color state.
    SetDecomposedState, thisState
    
END ;-------------------------------------------------------------------    