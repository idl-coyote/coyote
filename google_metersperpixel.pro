; docformat = 'rst'
;
; NAME:
;   Google_MetersPerPixel
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
;    For example, to find the pixels per meter for a zoom level of 12::
;       IDL> Print, Google_MetersPerPixel(12)
;            38.1757
;
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: david@idlcoyote.com
;           Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Written, 20 June 2012.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
Function Google_MetersPerPixel, zoomLevel

   Compile_Opt idl2
   
   ; Return to the caller if there is an error.
   On_Error, 2
   
   ; Need a zoom level?
   IF N_Elements(zoomLevel) EQ 0 THEN zoomLevel = 12
   
   ; Number of pixels in an image with a zoom level of 0.
   pixels_in_image = 256
   
   ; The number of meters in a longitude degree at the equator,
   ; assuming an Earth radius of 6371000 meters.
   metersPerLonDeg = (2*!DPI*6371000.0) / 360
   
   ; Create some variables.
   pixelsPerLonDeg = FIndgen(zoomLevel+1)
   metersPerPixel = FIndgen(zoomLevel+1)
   
   ; Calculate the pixels per meter for all the zoom levels,
   ; including the one you are interested in (the last one).
   FOR j=0,zoomLevel DO BEGIN
       pixelsPerLonDeg[j] = pixels_in_image / 360.0
       metersPerPixel[j] = 1.0 / (pixelsPerLonDeg[j] / metersPerLonDeg)
       pixels_in_image = pixels_in_image * 2
   ENDFOR
   
   ; Return the value you are after.
   RETURN, metersPerPixel[N_Elements(metersPerPixel)-1]
   
END