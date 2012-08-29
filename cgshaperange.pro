; docformat = 'rst'
;
; NAME:
;   cgShapeRange
;
; PURPOSE:
;   Determines the X (longitude) and Y (latitude) data range of the entities in a shapefile.
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
;   Determines the X (longitude) and Y (latitude) data range of the entities in a shapefile.
;
; :Categories:
;    Utilities, Map Projections
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
;    shapeBoundary = cgShapeRange('states.shp')
;    
; :Returns:
;    A four-element array containing the extent of the entity data in the
;    shapefile, in the order [xrange[0], yrange[0], xrange[1], yrange[1]].
;    
; :Params:
;    shapefile: in, optional, type=string
;        The name of the shapefile for which you wish to know the extent.
;        If not provided, the user will be asked to pick a shapefile.
;        
; :Keywords:
;     mean: out, optional, type=float
;        A two-element float or double array giving the mean or average X value
;        and the mean or average Y value of the entities in the shapefile, respectively
;     median: out, optional, type=float
;        A two-element float or double array giving the median X value
;        and the median Y value of the entities in the shapefile, respectively
;     xrange: out, optional, type=float
;        A two-element float or double array giving the minimum and maximum
;        extent of the shapefile entities in the X (longitude) direction, respectively
;     yrange: out, optional, type=float
;        A two-element float or double array giving the minimum and maximum
;        extent of the shapefile entities in the Y (latitude) direction, respectively
;
; :History:
;    Modification History::
;       Written by David W. Fanning by 29 August 2012.
;       
; :Copyright:
;    Copyright (c) 2012, Fanning Software Consulting, Inc.
;---------------------------------------------------------------------------
FUNCTION cgShapeRange, shapefile, $
    MEAN=mean, $
    MEDIAN=median, $
    XRANGE=xrange, $
    YRANGE=yrange

   Compile_Opt idl2
   
   ; Return to caller on error.
   On_Error, 2
   
   ; Need a shapefile?
   IF N_Elements(shapefile) EQ 0 THEN BEGIN
      shapefile = cgPickfile(Title='Select a SHAPEFILE...', Filter='*.shp')
      IF shapefile EQ "" THEN RETURN, FltArr(4)
   ENDIF
   
   ; Open the shape file and create the shape object.
   shapefileObj = Obj_New('IDLffShape', shapefile)
   IF Obj_Valid(shapefileObj) EQ 0 THEN $
      Message, 'Unable to create shape file object. Returning...'

   ; Get all the attribute pointers from the file. These are the entities.
   entities = Ptr_New(/Allocate_Heap)
   *entities = shapefileObj -> GetEntity(/All, /Attributes)
   
   ; Get the entity data ranges.
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
    
   ; Need mean values?
   IF Arg_Present(mean) THEN BEGIN
      xmean = Mean([entityMinX, entityMaxX])
      ymean = Mean([entityMinY, entityMaxY])
      mean = [xmean, ymean]
   ENDIF

   ; Need median values?
   IF Arg_Present(median) THEN BEGIN
      xmedian = Median([entityMinX, entityMaxX])
      ymedian = Median([entityMinY, entityMaxY])
      median = [xmedian, ymedian]
   ENDIF
   
   ; Clean up.
   Obj_Destroy, shapefileObj
   Ptr_Free, entities
   
   ; Return the extent of the data.
   RETURN, [xrange[0], yrange[0], xrange[1], yrange[1]]
   
END