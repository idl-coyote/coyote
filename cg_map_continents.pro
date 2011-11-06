PRO cg_Map_Continents, USA = kusa, CONTINENTS = kcont, COUNTRIES=kcountries, $
    HIRES=khires, FILL_CONTINENTS=kfill_continents, COASTS=kcoasts, $
    LIMITS = lim_u, MLINESTYLE = mlinestyle, MLINETHICK = mlinethick, $
    SPACING=spacing, COLOR=color, T3D=T3D, ORIENTATION=orientation, $
    ZVALUE=zvalue, RIVERS=krivers, $
    MAP_STRUCTURE=mapStruct, _EXTRA=extra, $
    ADDCMD=addcmd
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      IF N_Elements(thisState) NE 0 THEN SetDecomposedState, thisState
      RETURN
    ENDIF
      
    IF Keyword_Set(addcmd) THEN BEGIN
    
       cgWindow, 'CG_Map_Continents', USA = kusa, CONTINENTS = kcont, COUNTRIES=kcountries, $
          HIRES=khires, FILL_CONTINENTS=kfill_continents, COASTS=kcoasts, $
          LIMITS = lim_u, MLINESTYLE = mlinestyle, MLINETHICK = mlinethick, $
          SPACING=spacing, COLOR=color, T3D=T3D, ORIENTATION=orientation, $
          ZVALUE=zvalue, RIVERS=krivers, $
          MAP_STRUCTURE=mapStruct, _EXTRA=extra, $
          ADDCMD=1
          
       RETURN
       
    ENDIF
    
    IF N_Elements(color) NE 0 THEN BEGIN
        CASE Size(color, /TNAME) OF
           'STRING':
           'LONG': 
           'BYTE': color = StrTrim(Fix(color), 2)
           ELSE: color = StrTrim(color,2)
        ENDCASE 
    ENDIF ELSE color = "opposite"

    ; Try to do this in decomposed color, if possible.
    SetDecomposedState, 1, Current=thisState
    
    ; Change colors into appropriate values, if needed.
    IF Size(color, /TNAME) EQ 'STRING' THEN color = cgColor(color)

    Map_Continents, USA = kusa, CONTINENTS = kcont, COUNTRIES=kcountries, $
          HIRES=khires, FILL_CONTINENTS=kfill_continents, COASTS=kcoasts, $
          LIMITS = lim_u, MLINESTYLE = mlinestyle, MLINETHICK = mlinethick, $
          SPACING=spacing, COLOR=color, T3D=T3D, ORIENTATION=orientation, $
          ZVALUE=zvalue, RIVERS=krivers, $
          MAP_STRUCTURE=mapStruct, _EXTRA=extra
          
    ; Restore color state.
    SetDecomposedState, thisState
    
END ;-------------------------------------------------------------------    