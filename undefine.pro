;+
; NAME:
;       UNDEFINE
;
; PURPOSE:
;       The purpose of this program is to delete or undefine
;       an IDL program variable from within an IDL program or
;       at the IDL command line. It is a more powerful DELVAR.
;       Pointer and structure variables are traversed recursively
;       to undefine any variables pointed to in the pointer or in
;       a structure dereference.
;
; AUTHOR:
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1642 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:
;       Utilities.
;
; CALLING SEQUENCE:
;       UNDEFINE, variable
;
; REQUIRED INPUTS:
;       variable: The variable to be deleted. Up to 10 variables may be specified as arguments.
;
; SIDE EFFECTS:
;       The variable no longer exists.
;
; EXAMPLE:
;       To delete the variable "info", type:
;
;        IDL> Undefine, info
;        
;        IDL> var = ptr_new({a:ptr_New(5), b:findgen(11), c: {d:ptr_New(10), f:findgen(11)}})
;        IDL> Help, /Heap
;        Heap Variables:
;            # Pointer: 3
;            # Object : 0
;        <PtrHeapVar3>   LONG      =            5
;        <PtrHeapVar4>   LONG      =            10
;        <PtrHeapVar5>   STRUCT    = -> <Anonymous> Array[1]
;         
;        IDL> Undefine, var
;        IDL> Help, /Heap
;        Heap Variables:
;            # Pointer: 0
;            # Object : 0
;        IDL> Help, var
;         VAR               UNDEFINED = <Undefined>
;
; MODIFICATION HISTORY:
;       Written by David W. Fanning, 8 June 97, from an original program
;       given to me by Andrew Cool, DSTO, Adelaide, Australia.
;       Simplified program so you can pass it an undefined variable. :-) 17 May 2000. DWF
;       Simplified it even more by removing the unnecessary SIZE function. 28 June 2002. DWF.
;       Added capability to delete up to 10 variables at suggestion of Craig Markwardt. 10 Jan 2008. DWF.
;       If the variable is a pointer, object or structure reference the variable is recursively traversed
;          to free up all variables pointed to before the variable is itself destroyed. 10 June 2009. DWF.
;       Updated to allow undefining of pointer arrays. 8 October 2009. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2008 - 2009, by Fanning Software Consulting, Inc.                         ;
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
PRO UNDEFINE, var0, var1, var2, var3, var4, var5, var6, var7, var8, var9

   
   IF N_Elements(var0) NE 0 THEN BEGIN
        dataType = Size(var0, /TNAME)
        IF dataType EQ 'POINTER' THEN BEGIN
            FOR j=0L,N_Elements(var0)-1 DO BEGIN
              IF Ptr_Valid(var0[j]) THEN Undefine, *var0[j]
              Ptr_Free, var0[j]
            ENDFOR
        ENDIF
        IF dataType EQ 'OBJREF' THEN Obj_Destroy, var0
        IF dataType EQ 'STRUCT' THEN FOR j=0,N_Tags(var0)-1 DO Undefine, var0.(j)
        var0 = 0
        dummy = Temporary(var0)
   ENDIF

   IF N_Elements(var1) NE 0 THEN BEGIN
        dataType = Size(var1, /TNAME)
        IF dataType EQ 'POINTER' THEN BEGIN
            FOR j=0L,N_Elements(var1)-1 DO BEGIN
              IF Ptr_Valid(var1[j]) THEN Undefine, *var1[j]
              Ptr_Free, var1[j]
            ENDFOR
        ENDIF
        IF dataType EQ 'OBJREF' THEN Obj_Destroy, var1
        IF dataType EQ 'STRUCT' THEN FOR j=0,N_Tags(var1)-1 DO Undefine, var1.(j)
        var1 = 0
        dummy = Temporary(var1)
   ENDIF

   IF N_Elements(var2) NE 0 THEN BEGIN
        dataType = Size(var2, /TNAME)
        IF dataType EQ 'POINTER' THEN BEGIN
            FOR j=0L,N_Elements(var2)-1 DO BEGIN
              IF Ptr_Valid(var2[j]) THEN Undefine, *var2[j]
              Ptr_Free, var2[j]
            ENDFOR
        ENDIF
        IF dataType EQ 'OBJREF' THEN Obj_Destroy, var2
        IF dataType EQ 'STRUCT' THEN FOR j=0,N_Tags(var2)-1 DO Undefine, var2.(j)
        var2 = 0
        dummy = Temporary(var2)
   ENDIF

   IF N_Elements(var3) NE 0 THEN BEGIN
        dataType = Size(var3, /TNAME)
        IF dataType EQ 'POINTER' THEN BEGIN
            FOR j=0L,N_Elements(var3)-1 DO BEGIN
              IF Ptr_Valid(var3[j]) THEN Undefine, *var3[j]
              Ptr_Free, var3[j]
            ENDFOR
        ENDIF
        IF dataType EQ 'OBJREF' THEN Obj_Destroy, var3
        IF dataType EQ 'STRUCT' THEN FOR j=0,N_Tags(var3)-1 DO Undefine, var3.(j)
        var3 = 0
        dummy = Temporary(var3)
   ENDIF

   IF N_Elements(var4) NE 0 THEN BEGIN
        dataType = Size(var4, /TNAME)
        IF dataType EQ 'POINTER' THEN BEGIN
            FOR j=0L,N_Elements(var4)-1 DO BEGIN
              IF Ptr_Valid(var4[j]) THEN Undefine, *var4[j]
              Ptr_Free, var4[j]
            ENDFOR
        ENDIF
        IF dataType EQ 'OBJREF' THEN Obj_Destroy, var4
        IF dataType EQ 'STRUCT' THEN FOR j=0,N_Tags(var4)-1 DO Undefine, var4.(j)
        var4 = 0
        dummy = Temporary(var4)
   ENDIF

   IF N_Elements(var5) NE 0 THEN BEGIN
        dataType = Size(var5, /TNAME)
        IF dataType EQ 'POINTER' THEN BEGIN
            FOR j=0L,N_Elements(var5)-1 DO BEGIN
              IF Ptr_Valid(var5[j]) THEN Undefine, *var5[j]
              Ptr_Free, var5[j]
            ENDFOR
        ENDIF
        IF dataType EQ 'OBJREF' THEN Obj_Destroy, var5
        IF dataType EQ 'STRUCT' THEN FOR j=0,N_Tags(var5)-1 DO Undefine, var5.(j)
        var5 = 0
        dummy = Temporary(var5)
   ENDIF

   IF N_Elements(var6) NE 0 THEN BEGIN
        dataType = Size(var6, /TNAME)
        IF dataType EQ 'POINTER' THEN BEGIN
            FOR j=0L,N_Elements(var6)-1 DO BEGIN
              IF Ptr_Valid(var6[j]) THEN Undefine, *var6[j]
              Ptr_Free, var6[j]
            ENDFOR
        ENDIF
        IF dataType EQ 'OBJREF' THEN Obj_Destroy, var6
        IF dataType EQ 'STRUCT' THEN FOR j=0,N_Tags(var6)-1 DO Undefine, var6.(j)
        var6 = 0
        dummy = Temporary(var6)
   ENDIF

   IF N_Elements(var7) NE 0 THEN BEGIN
        dataType = Size(var7, /TNAME)
        IF dataType EQ 'POINTER' THEN BEGIN
            FOR j=0L,N_Elements(var7)-1 DO BEGIN
              IF Ptr_Valid(var7[j]) THEN Undefine, *var7[j]
              Ptr_Free, var7[j]
            ENDFOR
        ENDIF
        IF dataType EQ 'OBJREF' THEN Obj_Destroy, var7
        IF dataType EQ 'STRUCT' THEN FOR j=0,N_Tags(var7)-1 DO Undefine, var7.(j)
        var7 = 0
        dummy = Temporary(var7)
   ENDIF

   IF N_Elements(var8) NE 0 THEN BEGIN
        dataType = Size(var8, /TNAME)
        IF dataType EQ 'POINTER' THEN BEGIN
            FOR j=0L,N_Elements(var8)-1 DO BEGIN
              IF Ptr_Valid(var8[j]) THEN Undefine, *var8[j]
              Ptr_Free, var8[j]
            ENDFOR
        ENDIF
        IF dataType EQ 'OBJREF' THEN Obj_Destroy, var8
        IF dataType EQ 'STRUCT' THEN FOR j=0,N_Tags(var8)-1 DO Undefine, var8.(j)
        var8 = 0
        dummy = Temporary(var8)
   ENDIF

   IF N_Elements(var9) NE 0 THEN BEGIN
        dataType = Size(var9, /TNAME)
        IF dataType EQ 'POINTER' THEN BEGIN
            FOR j=0L,N_Elements(var9)-1 DO BEGIN
              IF Ptr_Valid(var9[j]) THEN Undefine, *var9[j]
              Ptr_Free, var9[j]
            ENDFOR
        ENDIF
        IF dataType EQ 'OBJREF' THEN Obj_Destroy, var9
        IF dataType EQ 'STRUCT' THEN FOR j=0,N_Tags(var9)-1 DO Undefine, var9.(j)
        var9 = 0
        dummy = Temporary(var9)
   ENDIF


END
