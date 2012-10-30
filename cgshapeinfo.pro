; docformat = 'rst'
;
; NAME:
;   cgShapeInfo
;
; PURPOSE:
;   The purpose of this program is allow the user to browse a very narrow
;   selection of shapefiles. Namely, those containing geographical shapes
;   in latitude and longitude coordinates. In other words, shapefiles
;   containing maps. File attributes are listed in the left-hand list
;   widget. Clicking on a file attribute, will list all the entity
;   attributes for that file attribute in the right-hand list. Clicking
;   an entity attribute with list the type of entity, and the X and Y
;   bounds of that entity (shown as LON and LAT, respectively), in the
;   statusbar at the bottom of the display.
;
;   Knowing the attribute and entity names of a shapefile will give
;   you insight into how to read and display the information in the file.
;   For examples, see the Coyote Library programs DrawCounties and DrawStates.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2012, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
;   The purpose of this program is allow the user to browse a very narrow
;   selection of shapefiles. Namely, those containing geographical shapes
;   in latitude and longitude coordinates. In other words, shapefiles
;   containing maps. File attributes are listed in the left-hand list
;   widget. Clicking on a file attribute, will list all the entity
;   attributes for that file attribute in the right-hand list. Clicking
;   an entity attribute with list the type of entity, and the X and Y
;   bounds of that entity (shown as LON and LAT, respectively), in the
;   statusbar at the bottom of the display.
;
;   Knowing the attribute and entity names of a shapefile will give
;   you insight into how to read and display the information in the file.
;   For examples, see the Coyote Library programs DrawCounties and DrawStates.
;
; :Categories:
;    Utilities
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
;    To learn about and draw the states.shp file in the IDL examples directory::
;       filename = Filepath(SubDir=['examples','data'], 'states.shp')
;       cgShapeInfo, filename, XRANGE=xr, YRANGE=yr
;       cgPlot, xr, yr, /NoData, XStyle=1, YStyle=1
;       cgDrawShapes, filename
;
; :History:
;    Modification History::
;       Written by David W. Fanning, 21 October 2006.
;       Slight modifications to the interface. 14 May 2010. DWF.
;       Added XRANGE and YRANGE output keywords. 15 May 2010. DWF.
;       Renamed cgShapeFile from Shapefile. 20 Aug 2012. DWF.
;       
; :Copyright:
;    Copyright (c) 2006-2012, Fanning Software Consulting, Inc.
;-   
;
;+
; The event handler for the program.
; 
; :Params:
;      event: in, required, type=struct
;          The event structure passed by the window manager.
;-
PRO cgShapeInfo_Events, event

   Compile_Opt idl2

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF

   ; Get the info structure.
   Widget_Control, event.top, Get_UValue=info

   ; Possible events identified by UVALUE.
   Widget_Control, event.id, Get_UValue=theEvent

   CASE theEvent OF

      ; User selected entity attribute. Get the particulars for this entity.
      'ENTITYLIST': BEGIN

         types = IntArr(N_Elements(*(*info).entities))
         bounds = FltArr(N_Elements(*(*info).entities))
         thisEntity = (*(*info).entities)[event.index]
         types = thisEntity.shape_type
         bounds = thisEntity.bounds
         typeIndex = Where((*info).typecode EQ types, count)
         IF count EQ 0 THEN BEGIN
            ok = Dialog_Message('Unknown type code: ' + types + ' Returning.')
            RETURN
         ENDIF
         space = String(Replicate(32B, 5))
         type = 'Type = ' + ((*info).shapetype[typeIndex])[0]
         lat = 'Y Bounds = [' + StrTrim(bounds[1],2) + ', ' + StrTrim(bounds[5],2) + ']'
         lon = 'X Bounds = [' + StrTrim(bounds[0],2) + ', ' + StrTrim(bounds[4],2) + ']'
         idx = 'Index = ' + StrTrim(event.index,2)
         Widget_Control, (*info).statusbar, $
            Set_Value=type[0] + space + lon + space + lat + space + idx
         END

      ; User selected a file attribute. Get the entity attributes associated with this file attribute.
      'NAMELIST': BEGIN

         entityAttributes = StrArr(N_Elements(*(*info).entities))
         FOR j=0,N_Elements(*(*info).entities)-1 DO BEGIN
            thisEntity = (*(*info).entities)[j]
            entityAttributes[j] = StrUpCase(StrTrim((*thisEntity.attributes).(event.index), 2))
         ENDFOR
         Widget_Control, (*info).entityList, Set_Value=entityAttributes
         *(*info).entityAttributes = entityAttributes
         Widget_Control, (*info).statusbar, Set_Value=String(Replicate(32B, 150))
         END

      ; User wants to open new shapefile.
      'OPENFILE': BEGIN

         CD, CURRENT=thisDir, (*info).directory
         filename = Dialog_Pickfile(Title='Select Shapefile for Input...', Filter='*.shp')
         CD, thisDir
         IF filename EQ "" THEN RETURN
         (*info).directory = File_DirName(filename)

         ; Clean up current shapefile information.
         Heap_Free, (*info).entities
         Ptr_Free, (*info).entityAttributes
         Obj_Destroy, (*info).shapefile

         ; Create a new shapefile object
         shapefile = Obj_New('IDLffShape', filename)
         IF Obj_Valid(shapefile) EQ 0 THEN BEGIN
            Message, 'Specified file (' + File_Basename(filename) + $
                     ') does not appear to be a shapefile. Returning...'
         ENDIF

         ; Get the attribute names from the shape file.
         shapefile -> GetProperty, ATTRIBUTE_NAMES=theNames
         theNames = StrUpCase(StrTrim(theNames, 2))

         ; Get all the attribute pointers from the file. These are the entities.
         entities = Ptr_New(/Allocate_Heap)
         *entities = shapefile -> GetEntity(/All, /Attributes)

         ; Load the info structure.
         (*info).shapefile = shapefile
         (*info).entities = entities

         entityAttributes = StrArr(N_Elements(*(*info).entities))
         FOR j=0,N_Elements(*(*info).entities)-1 DO BEGIN
            thisEntity = (*(*info).entities)[j]
            entityAttributes[j] = StrUpCase(StrTrim((*thisEntity.attributes).(0), 2))
         ENDFOR
         Widget_Control, (*info).namelist, Set_List_Select=0
         (*info).entityAttributes = Ptr_New(entityAttributes)

         Widget_Control, (*info).nameList, Set_Value=theNames
         Widget_Control, (*info).entityList, Set_Value=entityAttributes
         Widget_Control, (*info).statusbar, Set_Value=String(Replicate(32B, 150))
         END

      'QUIT': Widget_Control, event.top, /Destroy

      ELSE: Print, 'Event not currently handled.

   ENDCASE


END ; -----------------------------------------------------------------------------------------

;+
; The clean-up routine for the program.
;
; :Params:
;     tlb: in, required, type=long
;         The identifier of the widget that just died.
;-
PRO cgShapeInfo_Cleanup, tlb
   Widget_Control, tlb, Get_UValue=info
   Heap_Free, (*info).entities
   Ptr_Free, (*info).entityAttributes
   Obj_Destroy, (*info).shapefile
   Ptr_Free, info
END ; -----------------------------------------------------------------------------------------

;+
; The calling program for learning more about the contents of the shapefile.
;
; :Params:
;     filename: in, optional, type=string
;         The name of the shapefile you wish to browse. If not provided, the user
;         will be asked to select a shapefile.
;
; :Keywords:
;     xrange: out, optional, type=float
;          The X range of the shapefile contents in the native units of the shapefile.
;     yrange: out, optional, type=float
;          The Y range of the shapefile contents in the native units of the shapefile.
;-
PRO cgShapeInfo, filename, XRANGE=xrange, YRANGE=yrange

   Compile_Opt idl2

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF

   ; Ask the user to select file if one is not provided.
   IF N_Elements(filename) EQ 0 THEN BEGIN
      filename = Dialog_Pickfile(Title='Select Shapefile for Input...', Filter='*.shp')
      IF filename EQ "" THEN RETURN
   ENDIF

   ; Create a shapefile object
   shapefile = Obj_New('IDLffShape', filename)
   directory = File_DirName(filename)
   IF Obj_Valid(shapefile) EQ 0 THEN BEGIN
      Message, 'Specified file (' + File_Basename(filename) + $
               ') does not appear to be a shapefile. Returning...'
   ENDIF

   ; Get the attribute names from the shape file.
   shapefile -> GetProperty, ATTRIBUTE_NAMES=theNames
   theNames = StrUpCase(StrTrim(theNames, 2))

   ; Get all the attribute pointers from the file. These are the entities.
   entities = Ptr_New(/Allocate_Heap)
   *entities = shapefile -> GetEntity(/All, /Attributes)

   ; Create the widgets.
   tlb = Widget_Base(Title='ShapeFile Information', Column=1, MBAR=menuID)
   listBase = Widget_Base(tlb, Row=1, XPAD=0, YPAD=0, SPACE=0)

   ; Menubar
   fileID = Widget_Button(menuID, Value='File')
   button = Widget_Button(fileID, Value='Open New Shapefile', UVALUE='OPENFILE')
   button = Widget_Button(fileID, Value='Quit', UVALUE='QUIT')

   ; Shapefile Lists.
   nameBase = Widget_Base(listBase, Column=1)
   label = Widget_Label(nameBase, Value='Attribute Name')
   nameList = Widget_List(nameBase, Value=theNames, YSize=25, XSIZE=40, UVALUE='NAMELIST')
   Widget_Control, nameList, Set_List_Select=0
   attIndex = 0

   ; Cycle through each entity and get the attribute
   entityAttributes = StrArr(N_Elements(*entities))
   entityTypes = IntArr(N_Elements(*entities))
   entityMinX = FltArr(N_Elements(*entities))
   entityMaxX = FltArr(N_Elements(*entities))
   entityMiny = FltArr(N_Elements(*entities))
   entityMaxY = FltArr(N_Elements(*entities))
   FOR j=0,N_Elements(*entities)-1 DO BEGIN
      thisEntity = (*entities)[j]
      entityAttributes[j] = StrUpCase(StrTrim((*thisEntity.attributes).(attIndex), 2))
      entityTypes[j] = thisEntity.shape_type
      entityMinX[j] =  thisEntity.bounds[0]
      entityMaxX[j] =  thisEntity.bounds[4]
      entityMiny[j] =  thisEntity.bounds[1]
      entityMaxY[j] =  thisEntity.bounds[5]
   ENDFOR
   xrange = [Min(entityMinX), Max(entityMaxX)]
   yrange = [Min(entityMinY), Max(entityMaxY)]
   
;   Print, 'XRange = [' + Strtrim(Min(entityMinX),2) + ',' + StrTrim(Max(entityMaxX),2) + ']'
;   Print, 'YRange = [' + Strtrim(Min(entityMinY),2) + ',' + StrTrim(Max(entityMaxY),2) + ']'
;   FOR j=0,N_Elements(*entities)-1 DO Print, entityMinx[j], entityMaxx[j], entityminy[j], entitymaxy[j]

   ; Entity attribute list.
   nameBase = Widget_Base(listBase, Column=1)
   label = Widget_Label(nameBase, Value='Attribute Value')
   entityList = Widget_List(nameBase, Value=entityAttributes, YSize=25,  XSIZE=75, UVALUE='ENTITYLIST')

   ; Entity statusbar widgets.
   infobase = Widget_Base(tlb, Row=1, XPAD=0, YPAD=0, SPACE=0)
   label = Widget_Label(infobase, Value='Entity Info: ')

   ; Size the statusbar appropriately.
   gb = Widget_Info(infobase, /Geometry)
   gl = Widget_Info(label, /Geometry)
   statusbar = Widget_Label(infobase,/Sunken_Frame, SCR_XSIZE=gb.scr_xsize-gl.scr_xsize, $
      Value=String(Replicate(32B, 150)))

   cgCenterTLB, tlb
   Widget_Control, tlb, /Realize

   ; Information to identify entity types.
   typecode = [ 1, 3, 5, 8, 11, 13, 15, 18, 21, 23, 25, 28, 31]
   shapetype = ['Point', 'Polyline', 'Polygon', 'MultiPoint', 'PointZ', 'PointLineZ', 'PolygonZ', $
                'MultiPointZ', 'PointM', 'PolyLineM', 'PolygonM', 'MultiPointM', 'MultiPatch']

   ; Create info structure and store it.
   info = Ptr_New({entities:entities, namelist:namelist, entitylist:entitylist, $
      entityAttributes:Ptr_New(entityAttributes, /No_Copy), shapefile:shapefile, $
      typecode:typecode, shapetype:shapetype, statusbar:statusbar, directory:directory}, /No_Copy)
   Widget_Control, tlb, Set_UValue=info

   ; Start it up.
   XManager, 'cgshapeinfo', tlb, Event_Handler='cgShapeInfo_Events', $
      Cleanup='cgShapeInfo_Cleanup', /No_Block

END ; -----------------------------------------------------------------------------------------
