; docformat = 'rst'
;
; NAME:
;   MaxWindowSize
;
; PURPOSE:
;   Returns the resolution of the largest unobstructed graphics window that can be
;   created on this particular graphics device. Works properly for Windows, UNIX, and
;   Macintosh computers.
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
; :Description:
;    Returns the resolution of the largest unobstructed graphics window that can be
;    created on this particular graphics device. Works properly for Windows, UNIX, and
;    Macintosh computers.
;
; :Categories:
;    Utility
;    
; :Params:
;    none:
;       
; :Keywords:
;     monitor_resolution: out, optional, type=long
;        Set this keyword to a named variable to return the resolution of the
;        primary display monitor.
;
; :Examples:
;    To create a window of maximum size::
;       maxsize = MaxWindowSize()
;       Window, XSize=maxsize[0], YSize=maxsize[1], /Free
;
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: davidf@dfanning.com
;           Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; :History:
;     Written, 26 October 2010.
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
FUNCTION MaxWindowSize, MONITOR_RESOLUTION=monitor_resolution

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN, [-1,-1]
    ENDIF
    
    ; Need monitor resolution?
    IF Arg_Present(monitor_resolution) THEN BEGIN
        Device, Get_Screen_Size=monitor_resolution
    ENDIF
    
    ; Different ways to obtain information based on Windows, UNIX, 
    ; and Macintosh computers.
    CASE StrUpCase(!Version.OS_Family) OF
    
        'WINDOWS': BEGIN 
        
            oMonInfo = Obj_New('IDLsysMonitorInfo')
            rects = oMonInfo -> GetRectangles(Exclude_Taskbar=1)
            primaryIndex = oMonInfo -> GetPrimaryMonitorIndex()
            Obj_Destroy, oMonInfo
            retValue = rects[[2, 3], primaryIndex]
            
            END
            
        'UNIX': BEGIN
        
            ; Check to see if this is a Macintosh.
            IF StrPos(!Version.OS_Family, 'Mac ') NE -1 THEN BEGIN
                retValue =  Get_Screen_Size()
            ENDIF ELSE BEGIN ; All other UNIX computers.
                ; Unavoidable screen flash here. Uughh!
                s = Get_Screen_Size()
                Window, XSIZE=s[0], YSIZE=s[1], /FREE
                retValue = [!D.X_Size, !D.Y_Size]
                WDelete, !D.Window
            ENDELSE
            
            END
        
    ENDCASE
    
    ; Return answer.
    RETURN, retValue
 
 END