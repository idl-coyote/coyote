; docformat = 'rst'
;
; NAME:
;   cgGetColorState
;
; PURPOSE:
;   Provides a device-independent way to get the color decomposition state of the
;   current graphics device. 
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
;  Provides a device-independent way to get the color decomposition state of the
;  current graphics device. 
;
; :Categories:
;    Graphics, Utilities
;    
; :Returns:
;     Returns a 1 if color decomposition is turned on and a 0 if indexed color is used.
;     
; :Params:
;     device: in, optional, type=string
;         The IDL graphics device whose color decomposition state you wish to know the
;         current value of. If undefined, the current graphics device is used.
;       
; :Keywords:
;     Depth: out, optional, type=integer
;         The depth of the color display. Typically 8 for indexed color devices
;         and 24 for true-color devices.
;          
; :Examples:
;       IDL> currentState = cgGetColorState()
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
;     Change History::
;        Written, 5 Nov 2013, as a combination of DecomposedColor and GetDecomposedState, which
;           have both been retired from the Coyote Library.
;
; :Copyright:
;     Copyright (c) 2010-2013, Fanning Software Consulting, Inc.
;-
FUNCTION cgGetColorState, device, DEPTH=depth

    ; Return to caller on error.
    ON_ERROR, 2

    ; Was a graphics device passed in?
    IF N_Elements(device) EQ 0 THEN device = !D.NAME
    
    ; If the asked for graphics device is not the same as the current device,
    ; load the one the user asked for.
    IF StrUpCase(device) NE !D.NAME THEN BEGIN
        thisDevice = !D.NAME
        Set_Plot, device
    ENDIF

    ; Which graphics device are you interested in?
    CASE !D.NAME OF
    
        'PS': BEGIN ; PostScript
           CASE 1 OF
                Float(!Version.Release) EQ 7.1: BEGIN
                    Help, /DEVICE, OUTPUT=outstr
                    psinfo = outstr[4]
                    parts = StrSplit(psinfo, ':', /EXTRACT)
                    IF StrUpCase(StrCompress(parts[1], /REMOVE_ALL)) EQ 'DECOMPOSED' THEN BEGIN
                        decomposed = 1
                        depth = 24
                    ENDIF ELSE BEGIN
                        decomposed = 0
                        depth = 8
                    ENDELSE
                END
                Float(!Version.Release) GT 7.1: BEGIN
                    Device, GET_DECOMPOSED=decomposed
                    IF decomposed THEN depth = 24 ELSE depth = 8
                    END
                ELSE: BEGIN
                    decomposed = 0
                    depth = 8
                    END
            ENDCASE
           END
           
        'Z': BEGIN ; Z-graphics buffer.
            IF (Float(!Version.Release) GE 6.4) THEN BEGIN
                Device, GET_DECOMPOSED=decomposed
                Device, GET_PIXEL_DEPTH=depth
            ENDIF ELSE BEGIN
                decomposed = 0
                depth = 8
            ENDELSE
            END
            
        'X': Device, GET_DECOMPOSED=decomposed, GET_VISUAL_DEPTH=depth
        
        'WIN': Device, GET_DECOMPOSED=decomposed, GET_VISUAL_DEPTH=depth
        
        'MAC': BEGIN
            IF (Float(!Version.Release) GE 5.2) THEN BEGIN
                Device, Get_Decomposed=decomposedState, GET_VISUAL_DEPTH=depth
            ENDIF ELSE BEGIN
                decomposed = 0
                depth = 8
            ENDELSE
            END
            
         'NULL': BEGIN ; Setting up in decomposed mode will make sure
                       ; drawing colors are never loaded, which is not
                       ; allowed for the NULL device.
            decomposed = 1
            depth = 24
            END
            
        ELSE: BEGIN ; All other devices are 8-bit oldsters.
            decomposed = 0
            depth = 8
            END
    ENDCASE
 
    ; Need to clean up?
    IF N_Elements(thisDevice) NE 0 THEN Set_Plot, thisDevice
    
    ; Return the result.
    RETURN, decomposed
    
END    
