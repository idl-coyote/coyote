; docformat = 'rst'
;
; NAME:
;   cgGoggleMapMarker
;
; PURPOSE:
; 
;   This program is the definition module for Google Map Marker structures, which are
;   used in the GoogleMapWidget object to add markers to Google Static Map images.
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
;+
; This program is the definition module for a Google Map Marker structure, which is
; used in the GoogleMapWidget object to add markers to Google Static Map images.
;
; :Categories:
;    Utility
;    
; :Examples:
;    Create a variety of map markers::
;       marker1 = {cgGoggleMapMarker, 'normal', 'dodger blue', 'A', $
;            Ptr_New(40.600), Ptr_New(-105.100)}
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
; :History:
;     Written, 20 June 2012.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
PRO cgGoogleMapMarker__Define

    struct = { CGGOOGLEMAPMARKER, $
               size: "", $         ; The marker size ("tiny", "small", "mid" or "normal") Default: "normal".
               color: "", $        ; A color name as provided with cgColor. Default: "red".
               label: "", $        ; A single uppercase character label from the set {A-Z,0-9}. Default: "".
               lats: Ptr_New(), $  ; A pointer to one or more latitude values where markers are to be placed.
               lons: Ptr_New() $   ; A pointer to one or more longitude values where markers are to be placed.
             }
END