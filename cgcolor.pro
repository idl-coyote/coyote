; docformat = 'rst'
;+
; :Description:
;   Simply a Coyote Graphics System wrapper for the FSC_Color program.
;
; :Categories:
;    Graphics
;+
FUNCTION cgCOLOR, theColour, colorIndex, $
   AllColors=allcolors, $
   Brewer=brewer, $
   Check_Connection=check_connection, $ ; This keyword is completely ignored.
   ColorStructure=colorStructure, $
   Cancel=cancelled, $
   Decomposed=decomposedState, $
   _Ref_Extra=extra, $
   Filename=filename, $
   Names=names, $
   NColors=ncolors, $
   NODISPLAY=nodisplay, $ ; This keyword is completely ignored.
   Row=row, $
   SelectColor=selectcolor, $
   Triple=triple
   
   RETURN, FSC_COLOR(theColour, colorIndex, $
       AllColors=allcolors, $
       Brewer=brewer, $
       Check_Connection=check_connection, $ ; This keyword is completely ignored.
       ColorStructure=colorStructure, $
       Cancel=cancelled, $
       Decomposed=decomposedState, $
       _Extra=extra, $
       Filename=filename, $
       Names=names, $
       NColors=ncolors, $
       NODISPLAY=nodisplay, $ ; This keyword is completely ignored.
       Row=row, $
       SelectColor=selectcolor, $
       Triple=triple)
   
END 