; docformat = 'rst'
;
; NAME:
;   cgShapePointFile
;
; PURPOSE:
;   The purpose of this program is to create a shapefile filled with multiple
;   individual points as entities.
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
; The purpose of this program is to create a shapefile filled with multiple
; individual points as entities.
;
; :Categories:
;    Mapping, Utilities
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
; :Params:
;    xpts: in, required, type=vector
;       The X location of the input points. Presumed to be longitude values unless
;       the `MapCoord` keyword is used, in which case they are assumed to be in 
;       projected meters values that will be converted to longitude values for 
;       storage in the shapefile.
;    ypts: in, required, type=vector
;       The Y location of the input points. Presumed to be latitude values unless
;       the `MapCoord` keyword is used, in which case they are assumed to be in 
;       projected meters values that will be converted to latitude values for 
;       storage in the shapefile.
;       
; :Keywords:
;    append: in, optional, type=boolean, default=0
;       Set this keyword to append the points to a currently existing file. Otherwise,
;       a new file is created.
;    attributes: in, optional, type=structure
;       An array or scalar of "attribute" structures. If an array, it must be the same length as
;       in input data points. If not provided is will consist of the follow structure: {point:indexValue}. 
;       If appending to a file, it must be defined in exactly the same way as the original file attributes.
;    filename: in, optional, type=string
;       The name of the shapefile. If not provided, the user will be asked to select a shape
;       file. If the file currently exists, it will be written over.
;    mapcoord: in, optional, type=object
;       A MapCoord object (e.g., cgMap) which will be used to convert the input points from
;       projected meter space to lat/lon space prior to saving the data in the shapefile.
;         
; :Examples:
;    Add two cities to a shapefile::
;       num = 2
;       x = FltArr(num)
;       y = FltArr(num)
;       attr = Replicate({namedStruct, state:"", city:""}, num)
;       x[0] = -104.87270
;       y[0] =   39.768040
;       attr[0] ={namedStruct, 'CO', 'DENVER'}
;       x[1] = -105.1
;       y[1] =   40.6
;       attr[1] ={nameStruct, 'CO', 'FORT COLLINS'}
;       cgShapePointFile, x, y, ATTRIBUTES=attr, FILENAME='test.shp'
;       
;    Add two more cities to the same shapefile::
;       num = 2
;       x = FltArr(num)
;       y = FltArr(num)
;       attr = Replicate({namedStruct, state:"", city:""}, num)
;       x[0] = -122.7
;       y[0] =   45.5
;       attr[0] ={namedStruct, 'OR', 'Portland'}
;       x[1] = -122.3
;       y[1] =   47.6
;       attr[1] ={namedStruct, 'WA', 'Seattle'}
;       cgShapePointFile, x, y, ATTRIBUTES=attr, FILENAME='test.shp', /APPEND
;
;    View the file you just created::
;       cgShapeInfo, 'test.shp'
;    
; :History:
;    Modification History::
;       Written by David W. Fanning, 23 August 2012.
;       
; :Copyright:
;    Copyright (c) 2012, Fanning Software Consulting, Inc.
;- 
PRO cgShapePointFile, xpts, ypts, $
   APPEND=appending, $
   ATTRIBUTES=attributes, $
   FILENAME=filename, $
   MAPCOORD=mapCoord
   
   Compile_Opt idl2
   
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       Obj_Destroy, shapefile
       RETURN
   ENDIF
   
   ; Need point input parameters of points.
   IF N_Params() NE 2 THEN BEGIN
      void = Dialog_Message('Calling sequence: cgShapePointFile, xpts, ypts, ATTRIBUTES=attributes, FILENAME=filename')
      RETURN
   ENDIF
   
   ; Do we need a filename?
   IF N_Elements(filename) EQ 0 THEN BEGIN
      filename = cgPickfile(/Write, Title='Select a SHAPEFILE for Writing...')
      IF filename EQ "" THEN RETURN
   ENDIF
   
   ; If the file exists, and you are not appending to it, then delete it.
   appending = Keyword_Set(appending)
   IF File_Test(filename) && ~appending THEN BEGIN
      ; Print, 'Deleting current file: ' + filename
      File_Delete, filename
   ENDIF

   ; Do the points have to be converted to lat/lon?
   IF Obj_Valid(mapCoord) THEN BEGIN
       xy = mapCoord -> Inverse(xpts, ypts)
       x = Reform(xy[0,*])
       y = Reform(xy[1,*])
   ENDIF ELSE BEGIN
       x = xpts
       y = ypts
   ENDELSE
   
   IF N_Elements(xpts) NE N_Elements(ypts) THEN Message, 'The point vectors must be the same length.'
   numPts = N_Elements(xpts)
   
   ; Do we have an attibute structure?
   IF N_Elements(attributes) EQ 0 THEN BEGIN
       struct = {point:0L}
       attributes = Replicate(struct, numPts)
       attributes.point = LIndgen(numPts)
   ENDIF
   IF N_Elements(attributes) EQ 1 THEN attributes = Replicate(attributes, numPts)
   IF N_Elements(attributes) NE numPts THEN $
      Message, 'The number of attribute structures must match the number of points being added.'
   
   ; Open the shapefile for writing point data.
   IF appending THEN BEGIN
      shapefile = Obj_New('IDLffShape', filename, /UPDATE)
   ENDIF ELSE BEGIN
      shapefile = Obj_New('IDLffShape', filename, /UPDATE, ENTITY_TYPE=1)
   ENDELSE
   
   ; Define the attribute names if you are not appending to an already existing shapefile.
   IF ~appending THEN BEGIN
   
       ; Set the attribute names.
       struct = attributes[0]
       
       ; Make sure attribute names are less than 11 characters in length.
       attrnames = Tag_Names(struct)
       index = Where(StrLen(attrnames) GT 11, count)
       IF count GT 0 THEN Message, 'Attribute names cannot be longer than 11 characters.'
       
       ; Get the attribute type.
       FOR j=0,N_Elements(attrnames)-1 DO BEGIN
           value = struct.(j)
           dataType = Size(value, /TYPE)
           CASE dataType OF
               1: attrtype = 3
               2: attrtype = 3
               3: attrtype = 3
               4: attrtype = 5
               5: attrtype = 5
               7: attrtype = 7
               12: attrtype = 3
               13: attrtype = 3
               14: attrtype = 3
               ELSE: Message, 'Attribute value of type: ' + Size(value, /TNAME) + ' is not supported.'
           ENDCASE
           
           ; Define the new attribute.
           CASE attrtype OF
               3: shapefile -> AddAttribute, attrnames[j], attrtype, 16
               5: shapefile -> AddAttribute, attrnames[j], attrtype, 20, Precision=8
               7: shapefile -> AddAttribute, attrnames[j], attrtype, 25
           ENDCASE
           
       ENDFOR
       
   ENDIF
   
   ; Loop, creating entities for all the points. Points are saved in the BOUNDS field.
   FOR j=0,numPts-1 DO BEGIN
   
       ; Get a new entity structure.
       entNew = {IDL_SHAPE_ENTITY}
       
       ; Define the new values.
       entNew.SHAPE_TYPE = 1
       entNew.BOUNDS[0] = x[j]
       entNew.BOUNDS[1] = y[j]
       entNew.BOUNDS[2] = 0.00000000
       entNew.BOUNDS[3] = 0.00000000
       entNew.BOUNDS[4] = x[j]
       entNew.BOUNDS[5] = x[j]
       entNew.BOUNDS[6] = 0.00000000
       entNew.BOUNDS[7] = 0.00000000
       entNew.N_VERTICES = 1 
       
       ; Create a new attribute structure.
       attrNew = shapefile -> GetAttributes(/ATTRIBUTE_STRUCTURE)
       
       thisAttrStruct = attributes[j]
       FOR k=0, N_Tags(attrNew)-1 DO BEGIN
            attrNew.(k) = thisAttrStruct.(k)
       ENDFOR
       
       ; Add the entity to the shapefile.
       shapefile -> PutEntity, entNew
       
        ; Get the entity index.
        shapefile -> GetProperty, N_ENTITIES=numEntities
        entity_index = numEntities-1
   
       ; Add the attributes to the shapefile
       shapefile -> SetAttributes, entity_index, attrNew
   
   ENDFOR
   
   Obj_Destroy, shapefile
END