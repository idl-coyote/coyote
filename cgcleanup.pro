; docformat = 'rst'
;
; NAME:
;   cgCLEANUP
;
; PURPOSE:
; 
; This procedure cleans-up and/or destroys any open graphics or widget windows on the display.
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
; This procedure cleans-up and/or destroys any open graphics or widget windows on the display.
;
; :Categories:
;    Utility
;       
; :Keywords:
;     all: in, optional, type=boolean, default=1
;       Set this keyword if you wish to clean up windows of all types.
;     cg: in, optional, type=boolean, default=0
;       Set this keyword if you wish to clean up only Coyote Graphics windows.
;     dg: in, optional, type=boolean, default=0
;       Set this keyword if you wish to clean up only Direct Graphics windows.
;     fg: in, optional, type=boolean, default=0
;       Set this keyword if you wish to clean up only Function Graphics windows.
;
; :Examples:
;    For example, to destroy all windows on the display::
;       IDL> cgCleanup
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
;     Written, 6 October 2012.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
PRO cgCleanUp, ALL=all, CG=cg, DG=dg, FG=fg

   IF (Float(!Version.Release) GE 8.0) THEN BEGIN
      fscCleanUp8, ALL=all, CG=cg, DG=dg, FG=fg
   ENDIF ELSE BEGIN
      fscCleanUp7, ALL=all, CG=cg, DG=dg
   ENDELSE

END