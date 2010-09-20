;+
; NAME:
;       DRAWCOUNTIES
;
; PURPOSE:
;
;       Draws state counties in the USA from county shape files.
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
;       DrawCounties, countyFile
;
; ARGUMENTS:
;
;       countyFile:    The name of the input shapefile containing county boundaries.
;                      Must be defined, for example, 'co1990p020.shp'.
;
; KEYWORDS:
;
;     ATTRIBUTE_NAME:  The name of the attribute in the file that you wish to draw.
;                      By default, this is set to the attribute name "STATE".
;                      (In some shapefiles, the attribute might be named "STATE_ABBR".)
;                      If you are unsure of the attribute names in your shapefile,
;                      use the Coyote Library program SHAPEINFO to browse the file
;                      ahead of time.
;
;     COLORS:          The name of a color to draw the state outline or polygon in. This
;                      may be a string array of the same size as STATENAMES. Color names
;                      correspond to the colors available in FSC_COLOR. By default, "Sky Blue".
;
;     LINESTYLE:       The normal LINESTYLE keyword index to choose plotting linestyles.
;                      By default, set to 0 and solid lines. May be a vector of the same
;                      size as STATENAMES.
;
;     STATENAMES:      The names of the states you wish to draw counties for. Normally, these
;                      are two-element state abbreviations, but this will depend upon the entity
;                      attributes in your shape file. If this keyword is undefined, then the counties
;                      in all the states will be drawn. If you are unsure of the entity names, use the
;                      Coyote Library program SHAPEINFO to browse the file ahead of time.
;
;     THICK:           The line thickness. By default, 1.0.
;
; RESTRICTIONS:
;
;     It is assumed a map projection command has been issued and is in effect at
;     the time this program is called.
;
;     If STATENAMES is undefined, all states are drawn, but only a single value
;     for COLORS, LINESTYLE, and THICK is allowed.
;
;     Required Coyote Library programs:
;
;       Error_Message
;       FSC_Color
;
; EXAMPLE:
;
;       Create a map with Nevada in yellow and other state's counties in blue.
;
;       Window, XSize=500, YSize=500, Title='County Boundaries'
;       Map_Set, 37.5, -120, /Albers, /IsoTropic, Limit=[30, -125, 45, -108], $
;         Position=[0.05, 0.05, 0.95, 0.95]
;       Erase, COLOR=FSC_Color('ivory')
;       Map_Grid, LatDel = 2.0, LonDel = 2.0, /Box_Axes, Color=FSC_Color('charcoal')
;       colors = [Replicate('dodger blue', 6), 'indian red']
;       DrawCounties, , 'co1990p020.shp', Statenames=['CA', 'OR', 'WA', 'AZ', 'UT', 'ID', 'NV'], $
;          Colors=colors
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 24 June 2004.
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
PRO DrawCounties_DrawEntity, entity, COLOR=color, LINESTYLE=linestyle, THICK=thick

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
               PlotS, (*entity.vertices)[0, cuts[j]:cuts[j+1]-1], $
                  (*entity.vertices)[1, cuts[j]:cuts[j+1]-1], $
                  COLOR=FSC_Color(color), LINESTYLE=linestyle, THICK=thick
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
               PlotS, (*entity.vertices)[0, cuts[j]:cuts[j+1]-1], $
                  (*entity.vertices)[1, cuts[j]:cuts[j+1]-1], $
                  COLOR=FSC_Color(color), LINESTYLE=linestyle, THICK=thick
            ENDFOR
         ENDIF
      ENDCASE ; Polyline shapes.

      ELSE: ; All other shapes fall through and are silently ignored.

   ENDCASE

END ;---------------------------------------------------------------------------------



PRO DrawCounties, countyFile, $
   ATTRIBUTE_NAME=attribute_name, $
   COLORS=colors, $
   LINESTYLE=linestyle, $
   STATENAMES=statenames, $
   THICK=thick

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      ok = Error_Message(/Traceback)
      IF Obj_Valid(shapefile) THEN Obj_Destroy, shapefile
      IF Ptr_Valid(entities) THEN Heap_Free, entities
      RETURN
   ENDIF

   ; Check parameters.
   IF N_Elements(countyFile) EQ 0 THEN BEGIN
      countyFile = Dialog_Pickfile(Filter='*.shp')
      IF countyFile EQ "" THEN $
         Message, 'The name of a county shape file must be provided.'
   ENDIF
   IF N_Elements(attribute_name) EQ 0 THEN attribute_name = 'STATE' $
      ELSE attribute_name = StrUpCase(attribute_name)
   IF N_Elements(colors) EQ 0 THEN colors = 'Sky Blue'
   IF N_Elements(fill) EQ 0 THEN fill = Keyword_Set(fill)
   IF N_Elements(linestyle) EQ 0 THEN linestyle = 0
   IF N_Elements(thick) EQ 0 THEN thick = 1.0
   IF N_Elements(statenames) EQ 0 THEN statenames = 'ALL'

   ; Make sure arrays have the same number of elements.
   IF N_Elements(statenames) NE 1 THEN BEGIN
      numStates = N_Elements(statenames)
      IF N_Elements(colors) EQ 1 THEN colors = Replicate(colors, numStates)
      IF N_Elements(colors) NE numStates THEN $
         Message, 'Number of COLORS does not match number of state names.'
      IF N_Elements(linestyle) EQ 1 THEN linestyle = Replicate(linestyle, numStates)
      IF N_Elements(linestyle) NE numStates THEN $
         Message, 'Number of LINESTYLE values does not match number of state names.'
      IF N_Elements(thick) EQ 1 THEN thick = Replicate(thick, numStates)
      IF N_Elements(thick) NE numStates THEN $
         Message, 'Number of THICK values does not match number of state names.'
   ENDIF

   ; Open the shape file and create the shape object.
   shapefile = Obj_New('IDLffShape', countyFile)
   IF Obj_Valid(shapefile) EQ 0 THEN $
      Message, 'Unable to create shape file object. Returning...'

   ; Get the attribute names from the shape file.
   shapefile -> GetProperty, ATTRIBUTE_NAMES=theNames
   theNames = StrUpCase(StrTrim(theNames, 2))

   ; Find the attribute index.
   attIndex = Where(theNames EQ attribute_name, count)
   IF count EQ 0 THEN Message, 'Unable to find attribute ' + attribute_name + ' in file. Returning...'

   ; Get all the attribute pointers from the file. These are the entities.
   entities = Ptr_New(/Allocate_Heap)
   *entities = shapefile -> GetEntity(/All, /Attributes)

   ; Cycle through each entity and draw it, if required.
   FOR j=0,N_Elements(*entities)-1 DO BEGIN
      thisEntity = (*entities)[j]
      theState = StrUpCase(StrTrim((*thisEntity.attributes).(attIndex), 2))
      index = Where(stateNames EQ theState, test)
      IF stateNames[0] EQ 'ALL' THEN BEGIN
         index = 0
         test = 1
      ENDIF
      IF (test EQ 1) THEN DrawCounties_DrawEntity, (*entities)[j], Color=(colors[index])[0], $
         LineStyle=(linestyle[index])[0], Thick=(thick[index])[0]
   ENDFOR

   ; Clean up.
   Obj_Destroy, shapefile
   Heap_Free, entities

END ;---------------------------------------------------------------------------------
