; docformat = 'rst'
;
; NAME:
;   cgUTMZone
;
; PURPOSE:
; 
;   This function returns the correct UTM zone for UTM map projections, given a longitude
;   and latitude value. Southern hemisphere zones are indicated by a negative value.
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
; This function returns the correct UTM zone for UTM map projections, given a longitude
; and latitude value. Southern hemisphere zones are indicated by a negative value.
;
; :Categories:
;    Utility
;    
; :Params:
;    longitude: in, required, type=float
;       The requested longitude value east of the central meridian.
;    latitude: in, required, type=float
;       The requested latitude value in the range -90 to 90.
;       
; :Keywords:
;     formal: out, optional, type=string
;       The formal UTM zone designation, with the zone number and band letter combined. For example, "32M".
;
; :Examples:
;    For example, to find the UTM zone for London, England::
;       Print, cgUTMZone(-1.062, 51.5171, FORMAL=strDesignation)
;           30
;       Print, strDesignation
;           30U
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
;     Written, 8 August 2012.
;     Modified to assure the UTM band index is always in range for polar map projections. 22 Sept 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgUTMZone, longitude, latitude, FORMAL=formal

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN, 0
    ENDIF
    
    ; Must pass a lon/lat pair of values.
    IF N_Params() NE 2 THEN Message, "Must past longitude and latitude as input parameters."
    
    ; Set up the latitudes.
    latnotes = ['AB', 'C','D','E','F','G','H','J','K','L','M','N','P','Q','R','S','T','U','V','W','X','YZ' ]
    latbands  = [-90, Scale_Vector(Findgen(21), -80, 80), 90] ; Zones every 8 degrees, except for poles.
    latbands[20] = 84 ; Last zone extended 4 degrees.
    
    ; Find the latitude band.
    latbandIndex =  0 > Value_Locate(latbands, latitude) < (N_Elements(latnotes)-1)
    latBand = latnotes[latbandIndex]
    IF latBand EQ 'AB' THEN BEGIN
        IF (longitude LT 0) THEN latBand = 'A' ELSE latBand = 'B'
    ENDIF
    IF latBand EQ 'YZ' THEN BEGIN
        IF (longitude LT 0) THEN latBand = 'X' ELSE latBand = 'Z'
    ENDIF

    ; Get longitude in the range -180 to 180.
    IF longitude GT 180 THEN longitude = longitude - (Long(longitude)/180)*360.0
    
    ; Find the zone, with exceptions.
    lonbands = Scale_Vector(Findgen(61),-180, 180)
    IF latBand EQ 'V' THEN lonbands[32] = 9.00
   
    ; Find the zone.
    zone = ((Value_Locate(lonbands, longitude) + 1))[0]
    
    ; This program does NOT correct for exceptions in the Svalbard region in the X latitude band.
    ; Warn the user.
    IF latBand EQ 'X' && ((zone GE 32) && (zone LE 37)) THEN $
        Message, 'UTM zone designations for the Svalbard region are not correct.', /Informational
    
    ; The formal designation.
    formal = StrTrim(zone,2) + latband
    
    ; For IDL's purpose, southern hemisphere is indicated by negative value.
    IF (latBand LT 'N') THEN zone = -zone
    
    RETURN, zone
END