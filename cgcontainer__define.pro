PRO cgContainer::AddToTrash, object

   FOR j=0,N_Elements(object)-1 DO BEGIN
      IF Obj_Valid(object[j]) THEN self._cg_trash -> Add, object[j]
   ENDFOR

END ; -----------------------------------------------------------------

;*****************************************************************************************************
;+
; NAME:
;       cgContainer::FindByName
;
; PURPOSE:
;
;       This method returns the positions (indices) of named objects in the container.
;
; SYNTAX:
;
;       positions = container -> FindByName( searchName )
;
; ARGUMENTS:
;
;       searchName:     The name of the object you are looking for in this container (Required)
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
;-
;*****************************************************************************************************
FUNCTION cgContainer::FindByName, searchName, $
   Case_Sensitive=case_sensitive, $
   Count=count, $
   RegExp=regexp, $
   _Extra=extra
   
   Compile_Opt idl2

   ; Return to caller on error.
   ON_ERROR, 2
   
   ; Assume there are no matches.
   count = 0

   ; Search name must be a scalar.
   IF N_Elements(searchName) NE 1 THEN Message,'Search expression must be a scalar string.'

   ; Get the names of all the child objects, if they have names.
   children = self -> IDL_CONTAINER::Get(/All, Count=numChildren)
   IF numChildren EQ 0 THEN RETURN, -1

   names = StrArr(numChildren)
   FOR childNumber = 0, numChildren-1 DO $
   BEGIN
      thisChild = children[childNumber]
      IF Obj_HasMethod(thischild, 'GetName') THEN BEGIN
        childName = thisChild -> GetName()
        names[childNumber] = childName
      ENDIF ELSE names[childNumber] = ""
   ENDFOR

   ; Does the user want to evaluate a regular expression?
   ; If not, do a simple search for the search name.
   fold_case = Keyword_Set(case_sensitive) EQ 0
   IF Keyword_Set(regexp) THEN BEGIN
      mask = StRegEx(names, searchName, FOLD_CASE=fold_case, /BOOLEAN, _Extra=extra) 
   ENDIF ELSE mask = StrMatch(names, searchName, FOLD_CASE=fold_case)

   ; Transform boolean array to index array. A side effect is that
   ; the count value will be set.
   positions = Where(mask, count)

   RETURN, positions

END ;------------------------------------------------------------------

   
FUNCTION cgContainer::GetName
   RETURN, self._cg_name
END ;------------------------------------------------------------------

   
PRO cgContainer::GetProperty, NAME=name, UVALUE=uvalue, _REF_EXTRA=extra

   Compile_Opt idl2
    
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
    ; Get this object properties.
    IF Arg_Present(name) THEN name = self._cg_name
    IF Arg_Present(uvalue) && Ptr_Valid(self._cg_uvalue) THEN uvalue = *self._cg_uvalue
    
    ; Get the superclass object properties.
    IF N_Elements(extra) NE 0 THEN self -> IDL_Container::GetProperty, _STRICT_EXTRA=extra
    
END ;------------------------------------------------------------------

   
PRO cgContainer::SetProperty, NAME=name, UVALUE=uvalue, _EXTRA=extra

   Compile_Opt idl2
    
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Set this object properties.
   IF N_Elements(name) NE 0 THEN self._cg_name = name
   IF N_Elements(uvalue) NE 0 THEN BEGIN
      IF Ptr_Valid(self._cg_uvalue) THEN *self._cg_uvalue = uvalue $
          ELSE self._cg_uvalue = Ptr_New(uvalue)
   ENDIF
   
   ; Set superclass object properties, if any.
   IF N_Elements(extra) NE 0 THEN self -> IDL_Container::SetProperty, _Strict_Extra=extra

END ;------------------------------------------------------------------

   
PRO cgContainer::CLEANUP

    Ptr_Free, self._cg_uvalue
    Obj_Destroy, self._cg_trash
    self -> IDL_Container::CleanUp

END ;------------------------------------------------------------------

   
FUNCTION cgContainer::INIT, $
   NAME=name, $
   UVALUE=uvalue
   
   Compile_Opt idl2
   
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN, 0
   ENDIF
   
   ; Initialize superclass object.
   IF ~self -> IDL_Container::INIT() THEN RETURN, 0
   
   ; Every object should have a name. If we don't have one
   ; create it from the objects reference number.
   IF N_Elements(name) NE 0 THEN BEGIN
      self._cg_name = name 
   ENDIF ELSE BEGIN
      Help, self, Output=helpString
      parts = StrSplit(helpString, '<(', /Extract)
      objNo = StrMid(parts[1], 10)
      self._cg_name = Obj_Class(self) + '_' + objNo
   ENDELSE
   
   ; Have something to store in the user value?
   IF N_Elements(uvalue) NE 0 THEN self._cg_uvalue = Ptr_New(uvalue)
   
   ; Initialize the trash container.
   self._cg_trash = Obj_New('IDL_Container')

   RETURN, 1
   
END ;------------------------------------------------------------------

   
PRO cgContainer__Define, class

   class = { CGCONTAINER, $
             _cg_name: "",          $ ; A name for the container.
             _cg_trash: Obj_New(),  $ ; A trash container for destroying other objects.
             _cg_uvalue: Ptr_New(), $ ; A user value placeholder for the object.
             INHERITS IDL_CONTAINER          $
            }
            
END ;------------------------------------------------------------------