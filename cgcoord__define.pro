;+
; NAME:
;       cgCOORD
;
; PURPOSE:
;
;       The purpose of this object is to create a coordinate system for data objects.
;-
PRO cgCoord::Draw, Extra=extrakeywords

   ; You cannot do the next part successfully, unless there is an open window.
   IF ((!D.FLAGS AND 256) NE 0) AND !D.Window LT 0 THEN BEGIN
        createdWindow = 1
        Window, /Pixmap
   ENDIF ELSE createdWindow = 0

   ; Draw an invisible plot
   Plot, self._cg_xrange, self._cg_yrange, POSITION=self._cg_position, $
        XSTYLE=5, YSTYLE=21, /NODATA, /NOERASE, $
        YLOG=self._cg_ylog, XLOG=self._cg_xlog

   ; Clean up. 
   IF createdWindow THEN WDelete, !D.Window

END ; -------------------------------------------------------------------------


PRO cgCoord::GetProperty, $
   POSITION=position, $
   XLOG=xlog, $
   XRANGE=xrange, $
   YLOG=ylog, $
   YRANGE=yrange, $
   _REF_EXTRA=extraKeywords

   Compile_Opt idl2
    
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Object properties.
   position = self._cg_position
   xrange = self._cg_xrange
   yrange = self._cg_yrange
   xlog = self._cg_xlog
   ylog = self._cg_ylog
   
   ; Need superclass properties?
   IF N_Elements(extraKeywords) NE 0 THEN self -> cgContainer::GetProperty, _Extra=extraKeywords

   self -> Report, /Completed

END ; -------------------------------------------------------------------------


PRO cgCoord::SetProperty, $
   POSITION=position, $
   XLOG=xlog, $
   XRANGE=xrange, $
   YLOG=ylog, $
   YRANGE=yrange, $
   _EXTRA=extraKeywords

   Compile_Opt idl2
    
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   

   ; Object properties.
   IF N_Elements(position) NE 0 THEN self._cg_position = position
   IF N_Elements(xlog) NE 0 THEN self._cg_xlog = Keyword_Set(xlog)
   IF N_Elements(xrange) NE 0 THEN self._cg_xrange = xrange
   IF N_Elements(ylog) NE 0 THEN self._cg_ylog = Keyword_Set(ylog)
   IF N_Elements(yrange) NE 0 THEN self._cg_yrange = yrange

   ; Need to set superclass properties?
   IF N_Elements(extraKeywords) NE 0 THEN self -> CATATOM::SetProperty, _Extra=extraKeywords

END ; -------------------------------------------------------------------------


PRO cgCoord::CLEANUP

   self -> cgContainer::CLEANUP

END ; -------------------------------------------------------------------------


FUNCTION cgCoord::INIT, $
   POSITION=position, $
   XLOG=xlog, $
   XRANGE=xrange, $
   YLOG=ylog, $
   YRANGE=yrange, $
   _REF_EXTRA=extraKeywords

   Compile_Opt idl2
    
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN, 0
   ENDIF
   
   ; Check parameters.
   IF N_Elements(position) EQ 0 THEN position = [0.15, 0.125, 0.95, 0.925]
   IF N_Elements(xrange) EQ 0 THEN xrange = [0,1]
   IF N_Elements(yrange) EQ 0 THEN yrange = [0,1]

   ; Call the superclass INIT method.
   ok = self->cgContainer::INIT(_Extra=extraKeywords)
   IF NOT ok THEN Message, 'Failed to initialise the system component cgContainer.'

   ; Load the object.
   self._cg_position = position
   self._cg_xrange = xrange
   self._cg_yrange = yrange
   self._cg_xlog = Keyword_Set(xlog)
   self._cg_ylog = Keyword_Set(ylog)

   RETURN, 1

END ; -------------------------------------------------------------------------


PRO cgCoord__Define, class

   class = { cgCoord, $
             _cg_position: DblArr(4), $ ; A four-element array indicating normalized position of axes.
             _cg_xrange: DblArr(2), $   ; A two-element array representing the range of the X axis.
             _cg_yrange: DblArr(2), $   ; A two-element array representing the range of the Y axis.
             _cg_xlog: 0L, $            ; A flag that indicates an XLOG axis.
             _cg_ylog: 0L, $            ; A flag that indicates an YLOG axis.             
             INHERITS cgContainer $
           }

END ; -------------------------------------------------------------------------


