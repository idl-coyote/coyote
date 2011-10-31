PRO IDL_Object
END

FUNCTION IDL_Object

   self._cg_version = Float(!Version.Release)
   RETURN, 1
   
END

PRO IDL_Object__Define

   ; This is a stand-in class for the IDL_OBJECT in IDL 8.x and above.
   ; Used in cgContainer and other objects.

   class = {IDL_OBJECT, _cg_version: 0.0}
   
END