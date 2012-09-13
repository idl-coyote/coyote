; docformat = 'rst'
;
; NAME:
;   cgGoogle_MetersPerPixel
;
; PURPOSE:
; 
;   This funtion returns the number of meters per pixel for a particular zoom level
;   in images returned using Google's Static Image API.
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
; This funtion returns the number of meters per pixel for a particular zoom level
; in images returned using Google's Static Image API.
;
; :Categories:
;    Utility
;    
; :Params:
;    zoomlevel: in, optional, type=integer, default=12
;       The Google zoom level used in retrieving Google Maps using the static image API.
;       Documentation: https://developers.google.com/maps/documentation/staticmaps/.
;       
;
; :Examples:
;    For example, to find the meters per pixel for a zoom level of 12::
;       IDL> Print, cgGoogle_MetersPerPixel(12)
;            38.218514
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
;     Updated the algorithm for determining meters per pixel to use WGS-84 equitorial radius
;        based on suggestion by Alain LeCacheax. 11 Sept 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
Function cgGoogle_MetersPerPixel, zoomLevel

   Compile_Opt idl2
   
   ; Return to the caller if there is an error.
   On_Error, 2
   
   ; Need a zoom level?
   IF N_Elements(zoomLevel) EQ 0 THEN zoomLevel = 12
   
   ; Number of pixels in an image with a zoom level of 0.
   pixels_in_image = 256
   
   ; The equitorial radius of the Earth assuming WGS-84 ellipsoid.
   earth_radius = 6378137.0D
   
   ; The number of meters per pixel.
   metersPerPixel = (2*!DPI*earth_radius) / pixels_in_image / 2L^zoomLevel
   
   ; Return the value.
   RETURN, metersPerPixel
   
END