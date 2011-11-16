; docformat = 'rst'
;
; NAME:
;   cgCoord__Define
;
; PURPOSE:
;   An object for maintaining a coordinate system for data annotation or display.
;   Simply calling the Draw method of the object sets up the data coordinate system.
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
;   An object for maintaining a coordinate system for data annotation or display.
;   Simply calling the Draw method of the object sets up the data coordinate system.
;
; :Categories:
;    Object Programming, Graphics
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
;        
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;---------------------------------------------------------------------------
;
;+--------------------------------------------------------------------------
;   The initialization method of the object.
;
; :Keywords:
;      position: in, optional, type=float
;         The normalized position of the map projection space in the graphics window.
;         The default position is [0.075, 0.075, 0.925, 0.900].
;      xlog: in, optional, type=boolean, default=0
;         Set this keyword if you wish the X axis range to be logarithmic.
;      ylog: in, optional, type=boolean, default=0
;         Set this keyword if you wish the Y axis range to be logarithmic.
;      xrange: in, optional, type=various
;         Set this keyword to the X axis range desired in the data coordinate system.
;         The default os [0,1].
;      yrange: in, optional, type=various
;         Set this keyword to the Y axis range desired in the data coordinate system.
;         The default os [0,1].
;      _ref_extra: in, optional, type=various
;          Any keywords appropriate for superclass objects can also be specified.
;
;---------------------------------------------------------------------------
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

END  


;+--------------------------------------------------------------------------
;   This method sets up the data coordinate space of the object by drawing
;   an "invisible" plot.
;---------------------------------------------------------------------------
PRO cgCoord::Draw, _EXTRA=extra

   ; You cannot do the next part successfully, unless there is an open window.
   IF ((!D.FLAGS AND 256) NE 0) AND !D.Window LT 0 THEN BEGIN
        createdWindow = 1
        Window, /Pixmap
   ENDIF ELSE createdWindow = 0
   
   ; Shouldn't be getting keywords here.
   IF N_Elements(extra) NE 0 THEN BEGIN
       Message, 'Received unexpected keywords.', /Informational
       Help, extra
   ENDIF

   ; Draw an invisible plot
   Plot, self._cg_xrange, self._cg_yrange, POSITION=self._cg_position, $
        XSTYLE=5, YSTYLE=21, /NODATA, /NOERASE, $
        YLOG=self._cg_ylog, XLOG=self._cg_xlog

   ; Clean up. 
   IF createdWindow THEN WDelete, !D.Window

END  


;+--------------------------------------------------------------------------
;   This method allows the user to get various properties of the object. In general,
;   the same keywords that are used for the INIT method can be used here.
;---------------------------------------------------------------------------
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

END  


;+--------------------------------------------------------------------------
;   This method allows the user to set various properties of the object. In general,
;   the same keywords that are used for the INIT method can be used here.
;---------------------------------------------------------------------------
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
   IF N_Elements(extraKeywords) NE 0 THEN self -> cgContainer::SetProperty, _Extra=extraKeywords

END  


;+--------------------------------------------------------------------------
;   This is the clean-up routine for the object.
;---------------------------------------------------------------------------
PRO cgCoord::CLEANUP

   self -> cgContainer::CLEANUP

END  


;+--------------------------------------------------------------------------
;   This is the class definition module. 
;
; :Params:
;    class: out, optional, type=structure
;       Occasionally, it is useful to have an object class definition as
;       a structure variable. Using this output keyword will allow that.
;---------------------------------------------------------------------------
PRO cgCoord__Define, class

   class = { cgCoord, $
             _cg_position: DblArr(4), $ ; A four-element array indicating normalized position of axes.
             _cg_xrange: DblArr(2), $   ; A two-element array representing the range of the X axis.
             _cg_yrange: DblArr(2), $   ; A two-element array representing the range of the Y axis.
             _cg_xlog: 0L, $            ; A flag that indicates an XLOG axis.
             _cg_ylog: 0L, $            ; A flag that indicates an YLOG axis.             
             INHERITS cgContainer $
           }

END  


