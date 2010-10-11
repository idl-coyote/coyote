;+
; NAME:
;       CANCONNECT
;
; PURPOSE:
;
;       This function will check to see if it is possible to make
;       a connection with the window system. It is useful when 
;       running IDL as a CRON job or from remote logins. The program
;       will establish a system variable named !FSC_Display_Connection the
;       first time it is run. Programs which need to know if a connection
;       can be established can consult this system variable as a faster
;       way of determining a connection than running this program. The
;       system variable is set to 1 if a connection can be made and to
;       0 otherwise.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:

;       Utilities
;
; CALLING SEQUENCE:
;
;       connection = CanConnect()
;       
; RETURN_VALUE:
; 
;       connection:    Will be set to 1 if a window connection is possible
;                      Otherwise, it will be set to 0.
;
; INPUT_PARAMETERS:
;
;       None.
;
; KEYWORDS:
;
;        None.
;        
; NOTES:
; 
;      A system variable, !FSC_Display_Connection is created.
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 10 February 2010.
;       Modified program to set the system variable !FSC_Display_Connection. This is primarily
;           a way for legacy code to run in cron jobs without continually checking for
;           a connection, which before this update required opening and closing a window.
;           7 October 2010. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
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
Function CanConnect

      ; Assume a good connection.
      haveConnection = 1

      ; Try to open a window. If you fail, you have no window connection.
      Catch, theError
      IF theError NE 0 THEN BEGIN
          Catch, /CANCEL
          haveConnection = 0
          GOTO, cleanUp
      ENDIF

      ; Try to open a window.
      theWindow = !D.Window
      Window, /FREE, XSIZE=5, YSIZE=5, /PIXMAP
      Catch, /CANCEL

      ; Come here if things go south or you actually make it here.
      cleanUp: 
      IF !D.Window NE theWindow THEN BEGIN
          WDelete, !D.Window
          IF theWindow GE 0 THEN WSet, theWindow
      ENDIF
      
      ; Does the system variable !FSC_Display_Connection exist? If so, set its value.
      ; If not, create it.
      DefSysV, '!FSC_Display_Connection', EXISTS=sysvarExists
      IF sysvarExists $
          THEN !FSC_Display_Connection = haveConnection $
          ELSE DefSysV, '!FSC_Display_Connection', haveConnection
          
      RETURN, haveConnection
      
END
