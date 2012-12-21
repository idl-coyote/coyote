; docformat = 'rst'
;
; NAME:
;   cgDrawShapes
;
; PURPOSE:
;   Draws entities in a shapefile, containing latitude and longitude polygons  
;   or polylines, on a map projection. To draw the shapefile entities
;   automatically, no matter what kind of values the polygon vertices
;   have, set the AUTODRAW keyword.
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
;   This program draws the entities in a shapefile, containing latitude and 
;   longitude polygons or polylines, on a map projection. To draw the shapefile 
;   entities automatically, no matter what kind of values the polygon vertices
;   have, and without a map projection, set the AUTODRAW keyword.
;
;   It is assumed a map projection command has been issued and is in effect at
;   the time this program is called. Alternatively, you can use a cgMap object,
;   which will set up the map coordinate space and provide the map structure required
;   for plotting in that XY map coordinate space.
;
;   If the ATTRVALUES keyword is undefined, all entities are drawn, but only a single value
;   for COLORS, FCOLORS, FILL, LINESTYLE, and THICK is allowed.
;
; .. image:: cgdrawshapes.png
; 
; :Categories:
;    Graphics, Map Projections
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
; :Examples:
;    Example using cgMap_Set to set up the map projection space::
;       shapefile = Filepath(subdir=['examples','data'], 'states.shp')
;       cgDisplay, 800, 700
;       cgMap_Set, 40.0, -117.5, /Albers, /IsoTropic, Limit=[30, -125, 50, -108], $
;           Position=[0.05, 0.05, 0.95, 0.95]
;       cgDrawShapes, shapefile, AttrName='STATE_ABBR', $
;           AttrValues=['CA', 'OR', 'WA', 'AZ', 'UT', 'ID', 'NV'], Thick=1, $
;           FColors=['firebrick', 'grn4', 'pur4', 'tan4', 'steel blue', 'org4', 'yellow'], $
;           Fill = Replicate(1, 7), Colors='charcoal'
;       cgMap_Grid, LatDel = 2.0, LonDel = 2.0, /Box_Axes, Color='charcoal'
;
;    Example using cgMap to set up the map projection space::
;
;       shapefile = Filepath(subdir=['examples','data'], 'states.shp')
;       cgDisplay, 800, 700, WID=1
;       mapCoord = Obj_New('cgMap', 'Albers Equal Area', Limit=[30, -125, 50, -108], $
;                  Position=[0.2, 0.05, 0.8, 0.95], CENTER_LATITUDE=40.0, $
;                  CENTER_LONGITUDE=-117, STANDARD_PAR1=40, STANDARD_PAR2=-39)
;       cgDrawShapes, shapefile, AttrName='STATE_ABBR', $
;           AttrValues=['CA', 'OR', 'WA', 'AZ', 'UT', 'ID', 'NV'], Thick=1, $
;           FColors=['firebrick', 'grn4', 'pur4', 'tan4', 'steel blue', 'org4', 'yellow'], $
;           Fill = Replicate(1, 7), Colors='charcoal', MapCoord=mapCoord, /USELIMIT
;       cgMap_Grid,  /Box_Axes, Color='charcoal', Map_Structure=mapCoord
;
;    Example drawing the states.shp file automatically, without a map projection::
;  
;       shapefile = Filepath(subdir=['examples','data'], 'states.shp')
;       cgDrawShapes, shapefile, /Autodraw
;
;    Example using cgMap to set up the map projection space in a resizeable graphics window::
;
;       shapefile = Filepath(subdir=['examples','data'], 'states.shp')
;       cgWindow, WAspect=800.0/700
;       mapCoord = Obj_New('cgMap', 'Albers Equal Area', LIMIT=[30, -125, 45, -108], $
;                  Position=[0.05, 0.05, 0.95, 0.95], CENTER_LATITUDE=37.5, $
;                  CENTER_LONGITUDE=-117, STANDARD_PAR1=40, STANDARD_PAR2=-39)
;       mapCoord -> AddCmd
;       cgDrawShapes, shapefile, AttrName='STATE_ABBR', $
;           AttrValues=['CA', 'OR', 'WA', 'AZ', 'UT', 'ID', 'NV'], Thick=1, $
;           FColors=['firebrick', 'grn4', 'pur4', 'tan4', 'steel blue', 'org4', 'yellow'], $
;           Fill = Replicate(1, 7), Colors='charcoal', MapCoord=mapCoord, /AddCmd
;       cgMap_Grid, LatDel = 2.0, LonDel = 2.0, /Box_Axes, Color=cgColor('charcoal'), $
;            Map_Structure=mapCoord, /AddCmd
;
; :History:
;    Modification History::
;       Written by David W. Fanning by modifiying DrawStates to be more
;          general, 13 May 2010. DWF.
;       Added the AUTODRAW keyword for automatic drawing. 15 May 2010. DWF.
;       Added COMPILE_OPT idl2 to make sure all loop variables are longs. 5 July 2010. DWF.
;       Corrected an aspect ratio problem with AUTODRAW and upgraded to Coyote Graphics. 
;          3 January 2011. DWF.
;       Previous method of freeing entity pointers took 10 times times longer than freeing
;          pointers as I go. Also added MinNumVerts keyword to screen out the drawing of
;          small polygons. 6 October 2011. DWF.
;       Wrong string case for discovering particular attributes caused them not to be drawn. 27 Oct 2011. DWF.
;       Added the ability to draw point shapefiles. Changed default color to "opposite". 20 Aug 2012. DWF.
;       Made sure a window is open when the default color is chosen. 29 Aug 2012. DWF.
;       
; :Copyright:
;    Copyright (c) 2011, Fanning Software Consulting, Inc.
;---------------------------------------------------------------------------
;
;+
; Draw the shape file entity on the map projection.
;
; :Params:
;     entity: in, required, type=structure
;        The shapefile entity that is read out of the shape file. In should be
;        a polygon or a polyline.
;
; :Keywords:
;     color: in, required, type=string
;         The name of the color used to draw the entity. Used as fill color
;         for filled symbols when drawing points.
;     fcolor: in, required, type=string
;         The name of the color used to fill the entity. Used if the FILL
;         keyword is set.
;     fill: in, required, type=boolean
;         Set this keyword to fill the entity with a color.
;     linestyle: in, required, type=integer
;         Set this keyword to the line style type to draw the entity.
;     mapCoord: in, required, type=object
;         A map coordinate object (e.g., cgMap). Required to draw the
;         entity on a GCTP map projection set up with MAP_PROJ_INIT.
;     projected_xy: in, required, type=boolean, default=0
;         The program assumes the shapefile entities are expressed in latitude and
;         longitude values. If the entiites are expressed in projected XY Cartesian
;         coordinates, then set this value.
;     psym: in, optional, type=integer, default=16
;         When drawing points, the symbol to use. May be any value supported by
;         cgSymbol. The default is a filled circle, filled with `Colors`.
;     symsize: in, optional, type=float, default=1.0
;         The default symbol size. Used only when displaying points.
;     thick: in, required, type=integer
;         The thickness of the line used to draw the entity.
;-
PRO cgDrawShapes_DrawEntity, entity, $
    COLOR=color, $
    FCOLOR=fcolor, $
    FILL=fill, $
    LINESTYLE=linestyle, $
    MAPCOORD=mapCoord, $
    PROJECTED_XY=projected_xy, $
    PSYM=psym, $
    SYMSIZE=symsize, $
    THICK=thick

   Compile_Opt idl2

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      ok = Error_Message(/Traceback)
      IF Obj_Valid(shapefile) THEN Obj_Destroy, shapefile
      IF Ptr_Valid(entities) THEN Heap_Free, entities
      RETURN
   ENDIF

   ; Drawing is going to be done based on the shape type.
   CASE 1 OF

      ; Polygon shapes.
      entity.shape_type EQ 5 OR $    ; Polygon.
      entity.shape_type EQ 15 OR $   ; PolygonZ (ignoring Z)
      entity.shape_type EQ 25: BEGIN ; PolygonM (ignoring M)

         IF Ptr_Valid(entity.parts) THEN BEGIN
            cuts = [*entity.parts, entity.n_vertices]
            FOR j=0, entity.n_parts-1 DO BEGIN
               IF N_Elements(mapCoord) NE 0 THEN BEGIN
                   IF Obj_Valid(mapCoord) THEN BEGIN
                        mapCoord -> Draw, /NoGraphics
                        mapStruct = mapCoord->GetMapStruct()
                   ENDIF ELSE mapStruct = mapCoord
                   IF projected_xy THEN BEGIN
                       x = Reform((*entity.vertices)[0, cuts[j]:cuts[j+1]-1])
                       y = Reform((*entity.vertices)[1, cuts[j]:cuts[j+1]-1])
                   ENDIF ELSE BEGIN
                       xy = Map_Proj_Forward((*entity.vertices)[0, cuts[j]:cuts[j+1]-1], $
                                             (*entity.vertices)[1, cuts[j]:cuts[j+1]-1], $
                                              MAP_STRUCTURE=mapStruct)
                       x = Reform(xy[0,*])
                       y = Reform(xy[1,*])
                   ENDELSE
               ENDIF ELSE BEGIN
                   x = (*entity.vertices)[0, cuts[j]:cuts[j+1]-1]
                   y = (*entity.vertices)[1, cuts[j]:cuts[j+1]-1]
               ENDELSE
               CASE fill OF
                  0: cgPlotS, x, y, COLOR=color, LINESTYLE=linestyle, THICK=thick, NOCLIP=0
                  1: BEGIN
                     cgColorFill, x, y, COLOR=fcolor, NOCLIP=0
                     cgPlotS, x, y, COLOR=color, LINESTYLE=linestyle, THICK=thick, NOCLIP=0
                     END
               ENDCASE
            ENDFOR
         ENDIF
      ENDCASE ; Polygon shapes.

      ; Polyline shapes.
      entity.shape_type EQ  3 OR $   ; PolyLine
      entity.shape_type EQ 13 OR $   ; PolyLineZ (ignoring Z)
      entity.shape_type EQ 23: BEGIN ; PolyLineM (ignoring M)

         IF Ptr_Valid(entity.parts) THEN BEGIN
            cuts = [*entity.parts, entity.n_vertices]
            FOR j=0, entity.n_parts-1 DO BEGIN
               IF N_Elements(mapCoord) NE 0 THEN BEGIN
                   IF Obj_Valid(mapCoord) THEN BEGIN
                        mapCoord -> Draw
                        mapStruct = mapCoord->GetMapStruct()
                   ENDIF ELSE mapStruct = mapCoord
                   xy = Map_Proj_Forward((*entity.vertices)[0, cuts[j]:cuts[j+1]-1], $
                                         (*entity.vertices)[1, cuts[j]:cuts[j+1]-1], $
                                          MAP_STRUCTURE=mapStruct)
                   x = Reform(xy[0,*])
                   y = Reform(xy[1,*])
               ENDIF ELSE BEGIN
                   x = (*entity.vertices)[0, cuts[j]:cuts[j+1]-1]
                   y = (*entity.vertices)[1, cuts[j]:cuts[j+1]-1]
               ENDELSE
               CASE fill OF
                  0: cgPlotS, x, y, COLOR=color, LINESTYLE=linestyle, THICK=thick, NOCLIP=0
                  1: BEGIN
                     cgColorFill, x, y, COLOR=fcolor, NOCLIP=0
                     cgPlotS, x, y, COLOR=color, LINESTYLE=linestyle, THICK=thick, NOCLIP=0
                     END
               ENDCASE
            ENDFOR
         ENDIF
     ENDCASE ; Polyline shapes.
         
     ; Various kinds of points.
     entity.shape_type eq  1 or $   ; Point
     entity.shape_type eq 11 or $   ; PointZ (ignoring Z)
     entity.shape_type eq 21 or $   ; PointM (ignoring M)
     entity.shape_type eq  8 or $   ; MultiPoint
     entity.shape_type eq 18 or $   ; MultiPointZ (ignoring Z)
     entity.shape_type eq 28: BEGIN ; MultiPointM (ignoring M)
         IF N_Elements(mapCoord) NE 0 THEN BEGIN
                   IF Obj_Valid(mapCoord) THEN BEGIN
                        mapCoord -> Draw
                        mapStruct = mapCoord->GetMapStruct()
                   ENDIF ELSE mapStruct = mapCoord
                   IF Keyword_Set(projected_xy) THEN BEGIN
                        xy = Map_Proj_Forward(entity.bounds[0], entity.bounds[1], $
                                          MAP_STRUCTURE=mapStruct)
                        x = Reform(xy[0,*])
                        y = Reform(xy[1,*])
                        cgPlotS, x, y, PSYM=psym, COLOR=color, SYMSIZE=symsize, THICK=thick
                   ENDIF ELSE BEGIN
                      cgPlotS, entity.bounds[0], entity.bounds[1], $
                          PSYM=psym, COLOR=color, SYMSIZE=symsize, THICK=thick
                   ENDELSE
         ENDIF ELSE BEGIN 
            cgPlotS, entity.bounds[0], entity.bounds[1], $
                PSYM=psym, COLOR=color, SYMSIZE=symsize, THICK=thick
         ENDELSE
         ENDCASE
         
 
      ELSE: Message, 'Not currently handling entity type: ' + StrTrim(entity.shape_type,2)

   ENDCASE

END ;---------------------------------------------------------------------------------

;+
;   This program draws the entities in a shapefile, containing latitude and 
;   longitude points, polygons, polylines, or polypoings on a map projection. 
;   Values may also be in projected meter space, if the proper map coordinate
;   object is passed to the program (e.g, via the MapCoord keyword). To draw the shapefile 
;   entities automatically, no matter what kind of values the polygon vertices
;   or points have, and without a map projection, set the AUTODRAW keyword.
;
; :Params:
;    shapefile: in, optional, type=string
;        The name of a shapefile. If this variable is not provided, the user will
;        be asked to select a shapefile with the normal file dialogs.
;        
; :Keywords:
;     addcmd: in, optional, type=boolean
;         Set this keyword to add the command to a cgWindow.
;     autodraw: in, optional, type=boolean
;         If set, the shapefile entities are examined to find the X and Y
;         boundaries and a data coordinate space is set up to display the 
;         entities automatically. If the shapefile is drawn automatically,
;         the restriction that the shapefile vertices be in latitude and
;         longitude values is removed. Do NOT set this keyword, if you
;         are using the MAPCOORD keyword, too. If no map projection is
;         currently in effect (!X.TYPE NE 3) and no map coordinate object
;         has been provided, this keyword is automatically set.
;     attrname: in, optional, type=string
;         The name of the attribute in the file that you wish to draw.
;         By default, this is set to the first attribute name in the file.
;         If you are unsure of the attribute names in your shapefile,
;         use the Coyote Library program `cgShapeInfo` to browse the file
;         ahead of time.
;     attrvalues: in, optional, type=varies
;         The value of the attribute identified by ATTRNAME. This variable can
;         be a scalar or vector string array identifying a subset of attribute
;         values to be drawn. For example, if ATTRNAME='STATE_NAME', then
;         ATTRVALUE can identify the particular states to draw. For example,
;         ATTRVALUE=['ARIZONA', 'WYOMING', 'OREGON']. By default, the value
;         is "ALL", which means that all of the shape entities identified by
;         ATTRNAME will be drawn.
;     colors: in, optional, type=string, default="opposite"
;         The name of the color or colors used to draw the entity. This
;         may be a string array of the same size as ATTRVALUES.
;     fcolors: in, optional, type=string
;         The name of the color used to fill the entity. Used if the FILL
;         keyword is set. By default, the same as the COLORS keyword.
;     fill: in, optional, type=boolean
;         Set this keyword to fill the entity with a color.
;     linestyle: in, optional, type=integer, default=0
;         The normal linestyle keyword index to choose plotting line styles.
;         By default, set to 0 and solid lines. May be a vector of the same
;         size as ATTRVALUES.
;     mapCoord: in, optional, type=object
;         A map coordinate object (e.g., cgMap). Required to draw the
;         entity on a GCTP map projection set up with MAP_PROJ_INIT.
;         Note that this could also be a map structure as returned from MAP_PROJ_INIT,
;         but in that case the user is resposible for setting up the XY map
;         coordinate space independently and outside of this program.
;     minnumverts: in, optional, type=long, default=3
;         Set this keyword to the minimum number of vertices required to actually
;         draw a polygon. In other words, to to drawn, a polygon must have at least
;         this number of vertices. 
;     projected_xy: in, optional, type=boolean, default=0
;         The program assumes the shapefile entities are expressed in latitude and
;         longitude values. If the entiites are expressed in projected XY Cartesian
;         coordinates, then set this value.
;     psym: in, optional, type=integer, default=16
;         When drawing points, the symbol to use. May be any value supported by
;         cgSymbol. The default is a filled circle, filled with `Colors`.
;     symsize: in, optional, type=float, default=1.0
;         The default symbol size. Used only when displaying points.
;     thick: in, optional, type=integer, default=1
;         The thickness of the line used to draw the entity.
;     uselimit: in, optional, type=boolean, default=0
;         Set this keyword to use the LIMIT as determined from the map coordinate object.
;         This keyword applies only if a valid `MapCoord` object is passed to the program.
;     window: in, optional, type=boolean, default=0
;         Set this keyword to add this command to a current cgWindow program. If a
;         cgWindow doesn't exist, one is created.
;-
PRO cgDrawShapes, shapeFile, $
   ADDCMD=addcmd, $
   AUTODRAW=autodraw, $
   ATTRNAME=attrname, $
   ATTRVALUES=attrvalues, $
   COLORS=colors, $
   FCOLORS=fcolors, $
   FILL=fill, $
   LINESTYLE=linestyle, $
   MAPCOORD=mapCoord, $
   MINNUMVERTS=minNumVerts, $
   PROJECTED_XY=projected_xy, $
   PSYM=psym, $
   SYMSIZE=symsize, $
   THICK=thick, $
   USELIMIT=uselimit, $
   WINDOW=window
   
   Compile_Opt idl2

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      ok = Error_Message()
      IF Obj_Valid(shapefile) THEN Obj_Destroy, shapefile
      IF Ptr_Valid(entities) THEN Heap_Free, entities
      RETURN
   ENDIF
   
   ; A flag to allow graphics windows to be open.
   nowindows = ((!D.FLAGS AND 256) EQ 0)
   
    ; Replace the commands in or create a cgWindow.
    IF Keyword_Set(window) THEN BEGIN
    
       currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
       IF wincnt EQ 0 THEN cgWindow
       cgWindow, 'cgDrawShapes', shapeFile, $
           AUTODRAW=autodraw, $
           ATTRNAME=attrname, $
           ATTRVALUES=attrvalues, $
           COLORS=colors, $
           FCOLORS=fcolors, $
           FILL=fill, $
           LINESTYLE=linestyle, $
           MAPCOORD=mapCoord, $
           MINNUMVERTS=minNumVerts, $
           PROJECTED_XY=projected_xy, $
           PSYM=psym, $
           SYMSIZE=symsize, $
           THICK=thick, $
           USELIMIT=uselimit, $
           REPLACECMD=1
          
       RETURN
       
    ENDIF

   ; Add this command to the current graphics window?
   IF Keyword_Set(addcmd) THEN BEGIN
   
       cgWindow, 'cgDrawShapes', shapeFile, $
           AUTODRAW=autodraw, $
           ATTRNAME=attrname, $
           ATTRVALUES=attrvalues, $
           COLORS=colors, $
           FCOLORS=fcolors, $
           FILL=fill, $
           LINESTYLE=linestyle, $
           MAPCOORD=mapCoord, $
           MINNUMVERTS=minNumVerts, $
           PROJECTED_XY=projected_xy, $
           PSYM=psym, $
           SYMSIZE=symsize, $
           THICK=thick, $
           USELIMIT=uselimit, $
           ADDCMD=1
           
       RETURN
    
   ENDIF
   
   ; If a graphics window isn't open, open one now. Necessary to make
   ; sure the drawing is not done white on white.
   IF ~noWindows AND (!D.Window EQ -1) THEN cgDisplay
   
   ; Check parameters.
   IF N_Elements(shapeFile) EQ 0 THEN BEGIN
      shapeFile = Filepath(Subdirectory=['examples', 'data'], 'states.shp')
      IF File_Test(shapeFile) THEN BEGIN
         shapeFile = Dialog_Pickfile(FILE=shapefile, FILTER='*.shp')
         IF shapeFile EQ "" THEN RETURN
      ENDIF ELSE BEGIN
         shapeFile = Dialog_Pickfile(FILTER='*.shp')
         IF shapeFile EQ "" THEN RETURN
      ENDELSE
   ENDIF
   IF N_Elements(colors) EQ 0 THEN colors = 'opposite'
   IF N_Elements(fcolors) EQ 0 THEN fcolors = colors
   fastColors = cgColor(colors)
   fastFillColors = cgColor(fcolors)
   IF N_Elements(fill) EQ 0 THEN fill = Keyword_Set(fill)
   IF N_Elements(linestyle) EQ 0 THEN linestyle = 0
   IF N_Elements(psym) EQ 0 THEN psym = 16
   IF N_Elements(symsize) EQ 0 THEN symsize = 1.0
   IF N_Elements(thick) EQ 0 THEN thick = 1.0
   IF N_Elements(minNumVerts) EQ 0 THEN minNumVerts = 3
   projected_xy = Keyword_Set(projected_xy)
   IF N_Elements(attrvalues) EQ 0 THEN attrvalues = 'ALL'
   
   ; Is there a map projection in effect, or do I have a map coordinate object
   ; to create one? If not, then turn autodraw mode on.
   IF (!X.Type NE 3) && (N_Elements(mapCoord) EQ 0) THEN autodraw = 1

   ; Make sure arrays have the same number of elements.
   IF N_Elements(attrvalues) NE 1 THEN BEGIN
      numEntities = N_Elements(attrvalues)
      IF N_Elements(fastColors) EQ 1 THEN fastColors = Replicate(fastColors, numEntities)
      IF N_Elements(fastColors) NE numEntities THEN $
         Message, 'Number of COLORS does not match number of entity names.'
      IF N_Elements(fastFillColors) EQ 1 THEN fastFillColors = Replicate(fastFillColors, numEntities)
      IF N_Elements(fastFillColors) NE numEntities THEN $
         Message, 'Number of FCOLORS does not match number of entity names.'
      IF N_Elements(fill) EQ 1 THEN fill = Replicate(fill, numEntities)
      IF N_Elements(fill) NE numEntities THEN $
         Message, 'Number of FILL values does not match number of entity names.'
      IF N_Elements(linestyle) EQ 1 THEN linestyle = Replicate(linestyle, numEntities)
      IF N_Elements(linestyle) NE numEntities THEN $
         Message, 'Number of LINESTYLE values does not match number of entity names.'
      IF N_Elements(psym) EQ 1 THEN psym = Replicate(psym, numEntities)
      IF N_Elements(psym) NE numEntities THEN $
         Message, 'Number of PSYM values does not match number of entity names.'
      IF N_Elements(symsize) EQ 1 THEN symsize = Replicate(symsize, numEntities)
      IF N_Elements(symsize) NE numEntities THEN $
         Message, 'Number of SYMSIZE values does not match number of entity names.'
      IF N_Elements(thick) EQ 1 THEN thick = Replicate(thick, numEntities)
      IF N_Elements(thick) NE numEntities THEN $
         Message, 'Number of THICK values does not match number of entity names.'
   ENDIF

   ; Open the shape file and create the shape object.
   shapefileObj = Obj_New('IDLffShape', shapeFile)
   IF Obj_Valid(shapefileObj) EQ 0 THEN $
      Message, 'Unable to create shape file object. Returning...'

   ; Make sure you have a default attribute name. If you don't have
   ; one, get the first attribute name from the file.
   IF N_Elements(attrname) EQ 0 THEN BEGIN
       shapefileObj -> GetProperty, ATTRIBUTE_NAMES=theNames
       theNames = StrUpCase(StrTrim(theNames, 2))
       attrname = theNames[0]
   ENDIF ELSE attrname = StrUpCase(attrname)
   
   ; Get the attribute names from the shape file.
   IF N_Elements(theNames) EQ 0 THEN BEGIN
       shapefileObj -> GetProperty, ATTRIBUTE_NAMES=theNames
       theNames = StrUpCase(StrTrim(theNames, 2))
   ENDIF
   
   ; Find the attribute index.
   attIndex = Where(theNames EQ attrname, count)
   IF count EQ 0 THEN Message, 'Unable to find attribute ' + attrname + ' in file. Returning...'

   ; Get all the attribute pointers from the file. These are the entities.
   entities = Ptr_New(/Allocate_Heap)
   *entities = shapefileObj -> GetEntity(/All, /Attributes)
   
   ; Automatic drawing turned on and no map coordinate? 
   ; Then set up a drawing window with a data
   ; coordinate system established.
   IF Keyword_Set(autodraw) AND N_Elements(mapCoord) EQ 0 THEN BEGIN
       entityMinX = FltArr(N_Elements(*entities))
       entityMaxX = FltArr(N_Elements(*entities))
       entityMiny = FltArr(N_Elements(*entities))
       entityMaxY = FltArr(N_Elements(*entities))
       FOR j=0,N_Elements(*entities)-1 DO BEGIN
          thisEntity = (*entities)[j]
          entityMinX[j] =  thisEntity.bounds[0]
          entityMaxX[j] =  thisEntity.bounds[4]
          entityMiny[j] =  thisEntity.bounds[1]
          entityMaxY[j] =  thisEntity.bounds[5]
       ENDFOR
       xrange = [Min(entityMinX), Max(entityMaxX)]
       yrange = [Min(entityMinY), Max(entityMaxY)]
      
       cgPlot, xrange, yrange, XSTYLE=5, YSTYLE=5, Position=[0.05, 0.05, 0.95, 0.95], /NoData
       cgPlotS, [!X.Window[0], !X.Window[0], !X.Window[1], !X.Window[1],  !X.Window[0]], $
                [!Y.Window[0], !Y.Window[1], !Y.Window[1], !Y.Window[0],  !Y.Window[0]], $
                /NORMAL, Color='opposite', THICK=2
;      Print, 'X Range: ', xrange, Format='(a10, f12.2, 2x, f12.2)'
;      Print, 'Y Range: ', yrange, Format='(a10, f12.2, 2x, f12.2)'
   ENDIF
   
   ; If you have a map coordinate, then set the data range appropriately.
   IF Obj_Valid(mapCoord) THEN BEGIN
      
       ; This is only necessary, if a LIMIT wasn't specified.
       IF ~Keyword_Set(uselimit) THEN BEGIN
           entityMinX = FltArr(N_Elements(*entities))
           entityMaxX = FltArr(N_Elements(*entities))
           entityMiny = FltArr(N_Elements(*entities))
           entityMaxY = FltArr(N_Elements(*entities))
           FOR j=0,N_Elements(*entities)-1 DO BEGIN
              thisEntity = (*entities)[j]
              entityMinX[j] =  thisEntity.bounds[0]
              entityMaxX[j] =  thisEntity.bounds[4]
              entityMiny[j] =  thisEntity.bounds[1]
              entityMaxY[j] =  thisEntity.bounds[5]
           ENDFOR
           xrange = [Min(entityMinX), Max(entityMaxX)]
           yrange = [Min(entityMinY), Max(entityMaxY)]
           mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
       ENDIF
       
   ENDIF
   
   ; Cycle through each entity and draw it, if required. Free each pointer
   ; as you go, because it take at least 10 times as long to free them with
   ; HEAP_FREE at the end if you don't do this!
   FOR j=0,N_Elements(*entities)-1 DO BEGIN
      thisEntity = (*entities)[j]
      theEntityName = StrUpCase(StrTrim((*thisEntity.attributes).(attIndex), 2))
      index = Where(StrUpCase(attrvalues) EQ theEntityName, test)
      IF attrvalues[0] EQ 'ALL' THEN BEGIN
         index = 0
         test = 1
      ENDIF
      IF (test EQ 1) THEN BEGIN
          IF Ptr_Valid(thisEntity.vertices) THEN BEGIN
              IF (N_Elements(*thisEntity.vertices) GE minNumVerts) THEN BEGIN
                  cgDrawShapes_DrawEntity, (*entities)[j], COLOR=(fastColors[index])[0], $
                     Fill=(fill[index])[0], LineStyle=(linestyle[index])[0], $
                     THICK=(thick[index])[0], PROJECTED_XY=projected_xy, $
                     MapCoord=mapCoord, FCOLOR=(fastFillColors[index])[0]
              ENDIF 
          ENDIF ELSE BEGIN
              cgDrawShapes_DrawEntity, (*entities)[j], COLOR=(fastColors[index])[0], $
                 PSYM=(psym[index])[0], THICK=(thick[index])[0], $
                 PROJECTED_XY=projected_xy, MapCoord=mapCoord, SYMSIZE=(symsize[index])[0]
          ENDELSE
      ENDIF
      Ptr_Free, thisEntity.vertices
      Ptr_Free, thisEntity.measure
      Ptr_Free, thisEntity.parts
      Ptr_Free, thisEntity.part_types
      Ptr_Free, thisEntity.attributes
      
   ENDFOR

   ; Clean up.
   Obj_Destroy, shapefileObj
   Ptr_Free, entities
   
END ;---------------------------------------------------------------------------------
