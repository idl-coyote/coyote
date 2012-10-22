;+
; NAME:
;       WindowAvailable
;
; PURPOSE:
;
;       This function returns a 1 if the specified window index number is
;       currently open or available. It returns a 0 if the window is currently
;       closed or unavailable.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:
;
;       Utilities
;
; CALLING SEQUENCE:
;
;       available = WindowAvaiable(windowIndexNumber)
;
; INPUTS:
;
;       windowIndexNumber:   The window index number of the window you wish to
;                            know is available or not.
;
; KEYWORDS:
;
;       None.
;
; NOTES:
;
;       The window vector obtained from the DEVICE command is not always the same length. It
;       is normally (on my machine) 65 elements long, but can be much longer if you have lots
;       of IDL windows open (by calling cgPickColorName, for example). But if no windows with 
;       index numbers greater than 65 are open, IDL shinks the larger vector to the smaller one
;       as part of its housekeeping operations, which means it happens on their timetable, not yours.
;       This can result in the user having "stale" index numbers greater than 65, but no larger vector
;       to check them against. I have modified the code to return a 0 in this case, assuming that
;       whatever window your index number points to is long gone. I have not experience any ill effects
;       by doing this, but I STRONGLY advice you to ALWAYS know what window you are drawing into
;       when you issue a graphics command.
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, June 2005.
;       Modified to return 0 if the window index number is larger than the number of elements
;             in the WINDOW_STATE array. 25 June 2008. DWF.
;-
;
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
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
FUNCTION WindowAvailable, windowID

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message(Traceback=1)
   Print, 'Window ID: ', windowID
   RETURN, 0
ENDIF

   ; Get current window if window index number is unspecified.

IF N_Elements(windowID) EQ 0 THEN RETURN, 0
IF windowID LT 0 THEN RETURN, 0

   ; Default is window closed.

result = 0


CASE !D.Name OF

   'WIN': BEGIN
      Device, Window_State=theState
      IF N_Elements(theState) GT windowID THEN result = theState[windowID] ELSE result = 0
      END

   'X': BEGIN
      Device, Window_State=theState
      IF N_Elements(theState) GT windowID THEN result = theState[windowID] ELSE result = 0
      END

   'MAC': BEGIN
      Device, Window_State=theState
      IF N_Elements(theState) GT windowID THEN result = theState[windowID] ELSE result = 0
      END

   ELSE:
ENDCASE

RETURN, result
END
