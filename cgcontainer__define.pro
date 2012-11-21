; docformat = 'rst'
;
; NAME:
;   cgContainer
;
; PURPOSE:
;   A modified IDL container with properties needed for Coyote Graphics routines.
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
;   A modified IDL container with properties needed for Coyote Graphics routines.
;
; :Categories:
;    Obejct Programming, Utility
;    
; :Author:
;       FANNING SOFTWARE CONSULTING::
;        David W. Fanning 
;        1645 Sheely Drive
;        Fort Collins, CO 80526 USA
;        Phone: 970-221-0438
;        E-mail: david@idlcoyote.com
;        Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, by David W. Fanning, 7 November 2011.
;        Modified the SetProperty and GetProperty method to indicate errors if
;           extra keywords reach them. 3 Nov 2012. DWF.
;        
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;---------------------------------------------------------------------------
;
;+--------------------------------------------------------------------------
;   The object initialization method.
;    
; :Keywords:
;      name: in, optional, type=string, default=selected by cgContainer.
;         Use this keyword to name the object. Names are often used to select objects in 
;         program code. 
;      uvalue: in, optional, type=any, default=none
;         A storage space for storing any kind of IDL variable of importance to the user.
;
;---------------------------------------------------------------------------
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
   
END

   
;+--------------------------------------------------------------------------
;   Adds an object to the trash container. Any object add to the trash will
;   be destroyed when this object is destroyed.
;
; :Params:
;    object: in, required, type=object, default=none
;      The object to be added to the trash container.
;---------------------------------------------------------------------------
PRO cgContainer::AddToTrash, object

   FOR j=0,N_Elements(object)-1 DO BEGIN
      IF Obj_Valid(object[j]) THEN self._cg_trash -> Add, object[j]
   ENDFOR

END 

;+--------------------------------------------------------------------------
;   This method returns the positions (indices) of named objects in 
;   the container.
;
; :Params:
;    searchName: in, required, type=string, default=none
;      The name of the object that you wish to locate in the container.
;      
; :Keywords:
;     case_sensitive: in, optional, type=boolean, default=0
;        Set this keyword to 1 to indicate a case sensitive search. By default, the
;        search is case insensitive.
;     count: out, optional, type=integer
;         On exit, this variable will contain the number of positions returned that
;         meet the search criteria.
;     regexp: in, optional, type=boolean, default=0
;         Set this keyword to indicate that searchName is a regular expression.
;     _EXTRA: in, optional
;         Any keywords supported by STREGEX can also be used. Requires REGEXP to be set. 
;---------------------------------------------------------------------------
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

END

   
;+--------------------------------------------------------------------------
;   Returns the name of the object.
;---------------------------------------------------------------------------
FUNCTION cgContainer::GetName
   RETURN, self._cg_name
END

   
;+--------------------------------------------------------------------------
;   This method allows the user to get various properties of the object. In general,
;   the same keywords that are used for the INIT method can be used here. Superclass
;   values can also be obtained.
;   
; :Keywords:
;      count: out, optional, type=integer
;         Use the keyword to return the number of objects in the container.
;      name: out, optional, type=string
;         Use this keyword to name the object. Names are often used to select objects in 
;         program code. 
;      uvalue: out, optional, type=any
;         Returns the user value, if any.
;      _ref_extra: out, optional
;         Returns the value of any keyword in the superclass object.
;---------------------------------------------------------------------------
PRO cgContainer::GetProperty, NAME=name, COUNT=count, UVALUE=uvalue, _REF_EXTRA=extra

   Compile_Opt idl2
    
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      IF N_Elements(extra) NE 0 THEN Print, '      ' + 'Unhandled keywords: ' + extra
      RETURN
   ENDIF
   
    ; Get this object properties.
    IF Arg_Present(count) THEN count  = self -> IDL_Container::Count()
    IF Arg_Present(name) THEN name = self._cg_name
    IF Arg_Present(uvalue) && Ptr_Valid(self._cg_uvalue) THEN uvalue = *self._cg_uvalue
    
    ; If you get to here with a keyword, it is an error.
    IF N_Elements(extra) NE 0 THEN Message, 'There are unresolved keywords that should not be here.'
    
END

   
;+--------------------------------------------------------------------------
;   This method allows the user to set various properties of the object. In general,
;   the same keywords that are used for the INIT method can be used here. Superclass
;   values can also be set.
;   
; :Keywords:
;      name: in, optional, type=string, default=selected by cgContainer.
;         Use this keyword to name the object. Names are often used to select objects in 
;         program code. 
;      uvalue: in, optional, type=any, default=none
;         A storage space for storing any kind of IDL variable of importance to the user.
;      _ref_extra: in, optional
;         Any superclass keyword can be set here.
;---------------------------------------------------------------------------
PRO cgContainer::SetProperty, NAME=name, UVALUE=uvalue, _REF_EXTRA=extra

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
   
    ; If you get to here with a keyword, it is an error.
    IF N_Elements(extra) NE 0 THEN Message, 'There are unresolved keywords that should not be here.'

END

   
;+--------------------------------------------------------------------------
;   This is the clean-up routine for the object.
;
;---------------------------------------------------------------------------
PRO cgContainer::CLEANUP

    Ptr_Free, self._cg_uvalue
    Obj_Destroy, self._cg_trash
    self -> IDL_Container::CleanUp

END

   
;+--------------------------------------------------------------------------
;   This is the class definition module. Structures used to manipulate
;   map projection and map datum information are also created here.
;
; :Params:
;    class: out, optional, type=structure
;       Occasionally, it is useful to have an object class definition as
;       a structure variable. Using this output keyword will allow that.
;---------------------------------------------------------------------------
PRO cgContainer__Define, class

   class = { CGCONTAINER, $
             _cg_name: "",          $ ; A name for the container.
             _cg_trash: Obj_New(),  $ ; A trash container for destroying other objects.
             _cg_uvalue: Ptr_New(), $ ; A user value placeholder for the object.
             INHERITS IDL_OBJECT,   $ ; An IDL object for overloading "dot" operator, etc. in IDL 8.x.
             INHERITS IDL_CONTAINER $ ; An IDL container.
            }
            
END