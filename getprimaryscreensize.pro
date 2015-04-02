; docformat = 'rst'
;
; NAME:
;   GetPrimaryScreenSize
;
; PURPOSE:
;   Provides a way to get the screen size of the primary monitor, especially when
;   there are several being used.
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
;+
; :Description:
;   Provides a way to get the screen size of the primary monitor, especially when
;   there are several being used.
;
; :Categories:
;    Graphics
;    
; :Params:
;    none 
;       
; :Keywords:
;     exclude_taskbar: in, optional, boolean, default=0
;         Set this keyword to exclude the taskbar from the monitor size.
;         This keyword is ignored on all but Windows machines.
;          
; :Author:
;       Dick Jackson, www.dick-jackson.com
;       
; :History:
;     Change History::
;        Written, 8 March 2011. DJ
;        Modified to only use IDLsysMonitorInfo for IDL 6.3 and higher. 23 Feb 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
FUNCTION GetPrimaryScreenSize, Exclude_Taskbar=exclude_Taskbar

    IF Float(!Version.Release) GE 6.3 THEN BEGIN
        oMonInfo = Obj_New('IDLsysMonitorInfo')
        rects = oMonInfo -> GetRectangles(Exclude_Taskbar=exclude_Taskbar)
        pmi = oMonInfo -> GetPrimaryMonitorIndex()
        Obj_Destroy, oMonInfo
        RETURN, rects[[2, 3], pmi]     ; w & h of primary monitor avbl. space
    ENDIF ELSE RETURN, Get_Screen_Size()        

END
