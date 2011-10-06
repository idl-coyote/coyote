;+
; NAME:
;       NCDF_Container
;
; PURPOSE:
;
;       This is a beefed-up IDL_CONTAINER object written as a utility object
;       for the NCDF_FILE object and related objects. In particular, two new
;       container methods have been added. The FindByID method searches container
;       objects by object ID, and the FindByName method searches container object
;       by object name. If found, the object reference is returned. This object
;       is a subclassed IDL_CONTAINER object and uses the IDL_CONTAINER 
;       initialization routine.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:
;
;       Utilities
;
; CALLING SEQUENCE:
;
;       ncdf_container = NCDF_Container()
;
; ARGUMENTS:
;
;       Those used in the IDL_CONTAINER method.
;
; RETURN VALUE:
;
;       A sub-classed IDL_Container object.
;
; NOTES:
;
;     The program is designed to work with the NCDF_FILE object and related programs.
;
; MODIFICATION HISTORY:
;
;       Written by:  David W. Fanning, 3 February 2010.
;-
;******************************************************************************************;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc.                                ;
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
;*****************************************************************************************************
;
; NAME:
;       NCDF_Container::FindByID
;
; PURPOSE:
;
;       This method searches the IDL container object to find objects with a particular ID.
;       If found, the object reference to that object is returned.
;
; SYNTAX:
;
;       object = container -> FindByID( searchID )
;
; ARGUMENTS:
;
;       searchID:     The ID or identifier of the object you are searching for in 
;                     this container (Required)
;
; KEYWORDS:
;
;       COUNT:          Set this keyword to a named variable that upon exit will contain the number
;                       of objects returned that meet the searchName description. (Output)
;
;*****************************************************************************************************
FUNCTION NCDF_Container::FindByID, thisID, COUNT=count

   ; Return to caller on error.
   ON_ERROR, 2
   
   ; Assume there are no matches.
   count = 0

   ; ID name must be a scalar.
   IF N_Elements(thisID) NE 1 THEN Message, 'The search ID must be a scalar.'

   ; Get the IDs of all the child objects.
   children = self -> IDL_CONTAINER::Get (/All, Count=numChildren)
   IF numChildren EQ 0 THEN RETURN, Obj_New()
   ids = LonArr(numChildren)
   FOR childNo = 0L, numChildren - 1 DO BEGIN
      IF Obj_HasMethod(children[childNo], 'GetID') THEN BEGIN
            ids[childNo] = children[childNo] -> GetID()
      ENDIF
   ENDFOR

   ; Can you find this ID is the ID list?
   index = Where(ids EQ thisID, count)
   CASE count OF
        0: RETURN, Obj_New()
        1: index = index[0]
        ELSE: 
   ENDCASE
   
   ; Get the matching objects.
   IF count GT 0 THEN BEGIN
        matchingObjects = self -> Get(POSITION=matches)
   ENDIF ELSE matchingObjects = Obj_New()
   
   RETURN, matchingObjects
   
END



;*****************************************************************************************************
;
; NAME:
;       NCDF_Container::FindByName
;
; PURPOSE:
;
;       This method searches the IDL container object to find objects with a particular name.
;       If found, the object reference to that object is returned.
;
; SYNTAX:
;
;       indices = container -> FindByName( searchName )
;
; ARGUMENTS:
;
;       searchName:     The string name of the object you are searching for in 
;                       this container (Required)
;
; KEYWORDS:
;
;       CASE_SENSITIVE: Set this keyword to 1 to indicate a case-sensitive search. By default, the
;                       search is case-insensitive.
;
;       COUNT:          Set this keyword to a named variable that upon exit will contain the number
;                       of objects returned that meet the searchName description. (Output)
;
;       REGEXP:         Set this keyword to 1 to indicate the searchName is a regular expression.
;
;       _EXTRA:         Any keywords supported by STREGEX can also be used. Requires REGEXP to be set.
;
;
;*****************************************************************************************************
FUNCTION NCDF_Container::FindByName, searchName, $
   Case_Sensitive=case_sensitive, $
   Count=count, $
   RegExp=regexp, $
   _Extra=extra

   ; Return to caller on error.
   ON_ERROR, 2
   
   ; Assume there are no matches.
   count = 0

   ; Search name must be a scalar.
   IF N_Elements(searchName) NE 1 THEN Message,'Search expression must be a scalar string.'

   ; Get the names of all the child objects.
   children = self -> IDL_CONTAINER::Get (/All, Count=numChildren)
   IF numChildren EQ 0 THEN RETURN, Obj_New()

   ; We assume we can get the NAME as a property.
   names = StrArr(numChildren)
   FOR childNo = 0L, numChildren - 1 DO BEGIN
      names[childNo] = children[childNo] -> GetProperty('NAME')
   ENDFOR

   ; Does the user want to evaluate a regular expression?
   ; If not, do a simple search for the search name.
   fold_case = Keyword_Set(case_sensitive) EQ 0
   IF Keyword_Set (regexp) THEN BEGIN
        mask = StRegex(names, searchName, FOLD_CASE=fold_case, /BOOLEAN, _Extra=extra)
   ENDIF ELSE BEGIN
        mask = StrMatch(names, searchName, FOLD_CASE=fold_case)
   ENDELSE

   ; Transform boolean array to index array. A side effect is that
   ; the count value will be set.
   matches = Where(mask, count)
   IF N_Elements(matches) EQ 1 THEN matches = matches[0]
   
   ; Get the matching objects.
   IF count GT 0 THEN BEGIN
        matchingObjects = self -> Get(POSITION=matches)
   ENDIF ELSE matchingObjects = Obj_New()
   
   RETURN, matchingObjects

END


PRO NCDF_Container__DEFINE, class
    
    class = { NCDF_CONTAINER, INHERITS IDL_CONTAINER }
 
END
