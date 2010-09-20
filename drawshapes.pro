;+
; NAME:
;       DRAWSHAPES
;
; PURPOSE:
;
;       Draws entities in a shapefile, containing latitude and longitude
;       polygons, on a map projection. To draw the shapefile entities
;       automatically, no matter what kind of values the polygon vertices
;       have, set the AUTODRAW keyword.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:

;       Utilities
;
; CALLING SEQUENCE:
;
;       DrawShapes, shapeFile
;
; ARGUMENTS:
;
;       shapeFile:     The name of the input shapefile containing map polygons or shapes.
;                      If undefined, the "states.shp" file in the IDL distribution is used
;                      as an initial selection.
;
; KEYWORDS:
;
;     ATTRNAME:        The name of the attribute in the file that you wish to draw.
;                      By default, this is set to the first attribute name in the file.
;                      If you are unsure of the attribute names in your shapefile,
;                      use the Coyote Library program SHAPEINFO to browse the file
;                      ahead of time.
;
;     ATTRVALUE:       The value of the attribute identified by ATTRNAME. This variable can
;                      be a scalar or vector string array identifying a subset of attribute
;                      values to be drawn. For example, if ATTRNAME='STATE_NAME', then
;                      ATTRVALUE can identify the particular states to draw. For example,
;                      ATTRVALUE=['ARIZONA', 'WYOMING', 'OREGON']. By default, the value
;                      is "ALL", which means that all of the shape entities identified by
;                      ATTRNAME will be drawn.
;                      
;     AUTODRAW:        If set, the shapefile entities are examined to find the X and Y
;                      boundaries and a data coordinate space is set up display the 
;                      entities automatically. If the shapefile is drawn automatically,
;                      the restriction that the shapefile vertices be in latitude and
;                      longitude values is removed. Do NOT set this keyword, if you
;                      are using the MAPCOORD keyword, too.
;                      
;     COLORS:          The name of a color to draw the shapefile polygon in. This
;                      may be a string array of the same size as ATTRVALUES. Color names
;                      correspond to the colors available in FSC_COLOR. By default, "Sky Blue".
;                      
;     FCOLORS:         The name of the color to draw filled polygons in. If undefined,
;                      the same as COLOR. This may be a string array of the same size as 
;                      ATTRVALUES.
;
;     FILL:            Normally, the polygon outline is drawn. If this keyword is set,
;                      the polygon is filled with a solid color. May be a vector of 
;                      the same size as ATTRVALUES.
;
;     LINESTYLE:       The normal LINESTYLE keyword index to choose plotting linestyles.
;                      By default, set to 0 and solid lines. May be a vector of the same
;                      size as ATTRVALUES.
;
;     MAPCOORD:        A MapCoord object which implements a map coordinate system using the
;                      GCTP map projections, as implemented with MAP_PROJ_INIT. For more information
;                      about MapCoord objects, see http://www.dfanning.com/catalyst/maponimage.html.
;                      Note that this could also be a map structure as returned from MAP_PROJ_INIT,
;                      but in that case the user is resposible for setting up the XY map
;                      coordinate space independently and outside of this program. Details on 
;                      how this can be done can be found at http://www.dfanning.com/map_tips/ephemeral.html.
;
;
;     THICK:           The line thickness. By default, 1.0.
;
; RESTRICTIONS:
;
;     It is assumed a map projection command has been issued and is in effect at
;     the time this program is called. Alternatively, you can use a MapCoord object,
;     which will set up the map coordinate space and provide the map structure required
;     for plotting in that XY map coordinate space.
;
;     If ATTRVALUES is undefined, all entities are drawn, but only a single value
;     for COLORS, FCOLORS, FILL, LINESTYLE, and THICK is allowed.
;
;     Programs from the Coyote and Catalyst Libraries are required.
;     
;         http://www.dfanning.com/catalyst/howtoinstall.html
;
; EXAMPLES:
;
;       Window, XSize=700, YSize=800
;       Map_Set, 37.5, -117.5, /Albers, /IsoTropic, Limit=[30, -125, 45, -108], $
;           Position=[0.05, 0.05, 0.95, 0.95]
;       Erase, Color=FSC_Color('ivory')
;       DrawShapes, AttrName='STATE_ABBR', $
;           AttrValues=['CA', 'OR', 'WA', 'AZ', 'UT', 'ID', 'NV'], Thick=1, $
;           FColors=['firebrick', 'grn4', 'pur4', 'tan4', 'steel blue', 'org4', 'yellow'], $
;           Fill = Replicate(1, 7), Colors='charcoal'
;       Map_Grid, LatDel = 2.0, LonDel = 2.0, /Box_Axes, Color=FSC_Color('charcoal')
;
;  Example using a MapCoord object.
;
;       Window, XSize=700, YSize=800
;       mapCoord = Obj_New('MapCoord', 'Albers Equal Area', LIMIT=[30, -125, 45, -108], $
;                  Position=[0.05, 0.05, 0.95, 0.95], CENTER_LATITUDE=37.5, $
;                  CENTER_LONGITUDE=-117, STANDARD_PAR1=40, STANDARD_PAR2=-39)
;       Erase, Color=FSC_Color('ivory')
;       DrawShapes, AttrName='STATE_ABBR', $
;           AttrValues=['CA', 'OR', 'WA', 'AZ', 'UT', 'ID', 'NV'], Thick=1, $
;           FColors=['firebrick', 'grn4', 'pur4', 'tan4', 'steel blue', 'org4', 'yellow'], $
;           Fill = Replicate(1, 7), Colors='charcoal', MapCoord=mapCoord
;       Map_Grid, LatDel = 2.0, LonDel = 2.0, /Box_Axes, Color=FSC_Color('charcoal'), $
;            Map_Structure=mapCoord->GetMapStructure()
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning by modifiying DrawStates to be more
;          general, 13 May 2010. DWF.
;       Added the AUTODRAW keyword for automatic drawing. 15 May 2010. DWF.
;       Added COMPILE_OPT idl2 to make sure all loop variables are longs. 5 July 2010. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
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
PRO DrawShapes_DrawEntity, entity, $
    COLOR=color, $
    FCOLOR=fcolor, $
    FILL=fill, $
    LINESTYLE=linestyle, $
    MAPCOORD=mapCoord, $
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
                        mapCoord -> Draw
                        mapStruct = mapCoord->GetMapStructure()
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
                  0: PlotS, x, y, COLOR=FSC_Color(color), LINESTYLE=linestyle, THICK=thick, NOCLIP=0
                  1: BEGIN
                     PolyFill, x, y, COLOR=FSC_Color(fcolor), NOCLIP=0
                     PlotS, x, y, COLOR=FSC_Color(color), LINESTYLE=linestyle, THICK=thick, NOCLIP=0
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
                        mapStruct = mapCoord->GetMapStructure()
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
                  0: PlotS, x, y, COLOR=FSC_Color(color), LINESTYLE=linestyle, THICK=thick, NOCLIP=0
                  1: BEGIN
                     PolyFill, x, y, COLOR=FSC_Color(fcolor), NOCLIP=0
                     PlotS, x, y, COLOR=FSC_Color(color), LINESTYLE=linestyle, THICK=thick, NOCLIP=0
                     END
               ENDCASE
            ENDFOR
         ENDIF
      ENDCASE ; Polyline shapes.

      ELSE: ; All other shapes fall through and are silently ignored.

   ENDCASE

END ;---------------------------------------------------------------------------------



PRO DrawShapes, shapeFile, $
   AUTODRAW=autodraw, $
   ATTRNAME=attrname, $
   ATTRVALUES=attrvalues, $
   COLORS=colors, $
   FCOLORS=fcolors, $
   FILL=fill, $
   LINESTYLE=linestyle, $
   MAPCOORD=mapCoord, $
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
   IF N_Elements(colors) EQ 0 THEN colors = 'Sky Blue'
   IF N_Elements(fcolors) EQ 0 THEN fcolors = colors
   IF N_Elements(fill) EQ 0 THEN fill = Keyword_Set(fill)
   IF N_Elements(linestyle) EQ 0 THEN linestyle = 0
   IF N_Elements(thick) EQ 0 THEN thick = 1.0
   IF N_Elements(attrvalues) EQ 0 THEN attrvalues = 'ALL'

   ; Make sure arrays have the same number of elements.
   IF N_Elements(attrvalues) NE 1 THEN BEGIN
      numEntities = N_Elements(attrvalues)
      IF N_Elements(colors) EQ 1 THEN colors = Replicate(colors, numEntities)
      IF N_Elements(colors) NE numEntities THEN $
         Message, 'Number of COLORS does not match number of entity names.'
      IF N_Elements(fcolors) EQ 1 THEN fcolors = Replicate(fcolors, numEntities)
      IF N_Elements(fcolors) NE numEntities THEN $
         Message, 'Number of FCOLORS does not match number of entity names.'
      IF N_Elements(fill) EQ 1 THEN fill = Replicate(fill, numEntities)
      IF N_Elements(fill) NE numEntities THEN $
         Message, 'Number of FILL values does not match number of entity names.'
      IF N_Elements(linestyle) EQ 1 THEN linestyle = Replicate(linestyle, numEntities)
      IF N_Elements(linestyle) NE numEntities THEN $
         Message, 'Number of LINESTYLE values does not match number of entity names.'
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
   
   ; Automatic drawing turned on? If so, set up a drawing window with a data
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
      
      aspectRatio = Abs(yrange[1] - yrange[0]) / Abs(xrange[1]-xrange[0])
      IF (!D.Flags AND 256) NE 0 THEN BEGIN
          IF aspectRatio LE 1 THEN BEGIN
             Window, XSize=700, YSize=700*aspectRatio, /FREE, Title='Shapefile Contents'
          ENDIF ELSE BEGIN
             Window, XSize=700*aspectRatio, YSize=700, /FREE, Title='Shapefile Contents'
          ENDELSE
          Erase, Color=FSC_Color('snow')
      ENDIF
      PLOT, xrange, yrange, XSTYLE=1, YSTYLE=1, $
         Position=Aspect(aspectRatio), /NoData
   ENDIF
   
   ; Cycle through each entity and draw it, if required.
   FOR j=0,N_Elements(*entities)-1 DO BEGIN
      thisEntity = (*entities)[j]
      theEntityName = StrUpCase(StrTrim((*thisEntity.attributes).(attIndex), 2))
      index = Where(attrvalues EQ theEntityName, test)
      IF attrvalues[0] EQ 'ALL' THEN BEGIN
         index = 0
         test = 1
      ENDIF
      IF (test EQ 1) THEN DrawShapes_DrawEntity, (*entities)[j], Color=(colors[index])[0], $
         Fill=(fill[index])[0], LineStyle=(linestyle[index])[0], Thick=(thick[index])[0], $
         MapCoord=mapCoord, FCOLOR=(fcolors[index])[0]
   ENDFOR

   ; Clean up.
   Obj_Destroy, shapefileObj
   Heap_Free, entities

END ;---------------------------------------------------------------------------------
