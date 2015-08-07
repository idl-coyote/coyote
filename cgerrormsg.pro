; docformat = 'rst'
;
; NAME:
;   cgErrorMsg
;
; PURPOSE:
;   The purpose of this function is to have a device-independent error messaging function. 
;   The error message is reported to the user by using DIALOG_MESSAGE if widgets are
;    supported. Otherwise, it is just printed to standard out.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2013, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; The purpose of this function is to have a device-independent error messaging function.  
; The error message is reported to the user by using DIALOG_MESSAGE if widgets are
; supported. Otherwise, the error message is just printed to standard out.
;
; :Categories:
;    Utilities
;
; :Params:
;    themessage: in, optional, type=string
;       This is a string argument containing the error message you want reported. If undefined, 
;       this variable is set to the string in the !Error_State.Msg system variable.
;       
; :Keywords:
;    error: in, optional, type=boolean, default=1
;       Set this keyword to cause Dialog_Message to use the ERROR reporting dialog. 
;       Note that a longstanding bug in IDL causes the ERROR dialog to be used whether 
;       this keyword is set to 0 or 1!
;    informational: in, optional, type=boolean, default=0
;       Set this keyword to cause Dialog_Message to use the INFORMATION dialog instead of the 
;       WARNING dialog. Note that a bug in IDL causes the ERROR dialog to be used if this keyword 
;       is set to 0!
;    noname: in, optional, type=boolean, default=0
;       Normally, the name of the routine in which the error occurs is prepended to the error
;       message. Setting this keyword will suppress this behavior and no routine name will be used.
;    quiet: in, optional, type=boolean, default=0
;       Set this keyword to suppress the DIALOG_MESSAGE pop-up dialog.
;    title: in, optional, type=string
;       Set this keyword to the title of the DIALOG_MESSAGE window. By default the keyword is set to 
;       'System Error' unless !ERROR_STATE.NAME equals "IDL_M_USER_ERR", in which case it is set to 
;       "Trapped Error'.
;     traceback: in, optional, type=boolean, default=1
;        Setting this keyword results in an error traceback being printed to standard output with the 
;        PRINT command. Use TRACEBACK=0 to turn this functionality off.
;        
; :Examples:
;   In general, the cgErrorMsg function is not called directly. Rather, it is used in a 
;   CATCH error handler. Errors are thrown to cgErrorMsg with the MESSAGE command. A typical 
;   CATCH error handler is shown below::
;
;       Catch, theError
;       IF theError NE 0 THEN BEGIN
;          Catch, /Cancel
;          void = cgErrorMsg()
;          RETURN
;       ENDIF
;
;   Error messages will get into the cgErrorMsg function by throwing an error with the 
;   MESSAGE command, like this::
;
;       IF test NE 1 THEN Message, 'The test failed.'
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
;    Change History::
;      Written by: David W. Fanning, 27 April 1999.
;      Added the calling routine's name in the message and NoName keyword. 31 Jan 2000. DWF.
;      Added _Extra keyword. 10 February 2000. DWF.
;      Forgot to add _Extra everywhere. Fixed for MAIN errors. 8 AUG 2000. DWF.
;      Adding call routine's name to Traceback Report. 8 AUG 2000. DWF.
;      Added ERROR, INFORMATIONAL, and TITLE keywords. 19 SEP 2002. DWF.
;      Removed the requirement that you use the NONAME keyword with the MESSAGE
;        command when generating user-trapped errors. 19 SEP 2002. DWF.
;      Added distinctions between trapped errors (errors generated with the
;        MESSAGE command) and IDL system errors. Note that if you call ERROR_MESSAGE
;        directly, then the state of the !ERROR_STATE.NAME variable is set
;        to the *last* error generated. It is better to access ERROR_MESSAGE
;        indirectly in a Catch error handler from the MESSAGE command. 19 SEP 2002. DWF.
;      Change on 19 SEP 2002 to eliminate NONAME requirement did not apply to object methods.
;        Fixed program to also handle messages from object methods. 30 JULY 2003. DWF.
;      Removed obsolete STR_SEP and replaced with STRSPLIT. 27 Oct 2004. DWF.
;      Made a traceback the default case without setting TRACEBACK keyword. 19 Nov 2004. DWF.
;      Added check for window connection specifically for CRON jobs. 6 May 2008. DWF.
;      Added QUIET keyword. 18 October 2008. DWF.
;      The traceback information was bypassed when in the PostScript device. Not what I
;        had in mind. Fixed. 6 July 2009. DWF.
;      The QUIET keyword was clearing traceback information. Fixed with help from Phillip Bitzer. 2 Oct 2012. DWF.
;      Name changed to cgErrorMsg from Error_Message, 4 November 2013, by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;-
FUNCTION cgErrorMsg, theMessage, Error=error, Informational=information, $
   Traceback=traceback, NoName=noname, Title=title, Quiet=quiet, _Extra=extra

   On_Error, 2
   
   ; Check for presence and type of message.
   
   IF N_Elements(theMessage) EQ 0 THEN theMessage = !Error_State.Msg
   s = Size(theMessage)
   messageType = s[s[0]+1]
   IF messageType NE 7 THEN BEGIN
      Message, "The message parameter must be a string.", _Extra=extra
   ENDIF
   IF N_Elements(traceback) EQ 0 THEN traceback = 1
   
   ; Get the call stack and the calling routine's name.
   Help, Calls=callStack
   callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]
   
   ; Are widgets supported?
   IF !D.Name EQ 'PS' OR !D.Name EQ 'Z' THEN BEGIN
      widgetsSupported = 1
   ENDIF ELSE BEGIN
      widgetsSupported = ((!D.Flags AND 65536L) NE 0)
   ENDELSE

   ; Is the QUIET keyword set? Then no dialogs.
   IF Keyword_Set(quiet) THEN widgetsSupported = 0
   
   ; It is not enough to know if widgets are supported. In CRON jobs, widgets are
   ; supported, but there is no X connection and pop-up dialogs are not allowed.
   ; Here is a quick test to see if we can connect to a windowing system. If not,
   ; then we are going to assume widgets are not supported.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      widgetsSupported = 0
      GOTO, testWidgetSupport
   ENDIF
   theWindow = !D.Window
   IF (!D.Flags AND 256) NE 0 THEN Window, /FREE, XSIZE=5, YSIZE=5, /PIXMAP
   Catch, /CANCEL
   
   testWidgetSupport: ; Come here if you choke on creating a window.
   IF !D.Window NE theWindow THEN BEGIN
      WDelete, !D.Window
      IF theWindow GE 0 THEN WSet, theWindow
   ENDIF
   
   IF widgetsSupported THEN BEGIN
   
      ; If this is an error produced with the MESSAGE command, it is a trapped
      ; error and will have the name "IDL_M_USER_ERR".
      IF !ERROR_STATE.NAME EQ "IDL_M_USER_ERR" THEN BEGIN
   
         IF N_Elements(title) EQ 0 THEN title = 'Trapped Error'
   
         ; If the message has the name of the calling routine in it,
         ; it should be stripped out. Can you find a colon in the string?
   
         ; Is the calling routine an object method? If so, special processing
         ; is required. Object methods will have two colons together.
         doublecolon = StrPos(theMessage, "::")
         IF doublecolon NE -1 THEN BEGIN
   
            prefix = StrMid(theMessage, 0, doublecolon+2)
            submessage = StrMid(theMessage, doublecolon+2)
            colon = StrPos(submessage, ":")
            IF colon NE -1 THEN BEGIN
   
               ; Extract the text up to the colon. Is this the same as
               ; the callingRoutine? If so, strip it.
               IF StrMid(theMessage, 0, colon+StrLen(prefix)) EQ callingRoutine THEN $
                  theMessage = StrMid(theMessage, colon+1+StrLen(prefix))
            ENDIF
         ENDIF ELSE BEGIN
   
            colon = StrPos(theMessage, ":")
            IF colon NE -1 THEN BEGIN
   
               ; Extract the text up to the colon. Is this the same as
               ; the callingRoutine? If so, strip it.
               IF StrMid(theMessage, 0, colon) EQ callingRoutine THEN $
                  theMessage = StrMid(theMessage, colon+1)
            ENDIF
   
         ENDELSE
   
   
         ; Add the calling routine's name, unless NONAME is set.
         IF Keyword_Set(noname) THEN BEGIN
            answer = Dialog_Message(theMessage, Title=title, _Extra=extra, $
               Error=error, Information=information)
         ENDIF ELSE BEGIN
            answer = Dialog_Message(StrUpCase(callingRoutine) + ": " + $
               theMessage, Title=title, _Extra=extra, $
               Error=error, Information=information)
         ENDELSE
   
      ENDIF ELSE BEGIN
   
         ; Otherwise, this is an IDL system error.
         IF N_Elements(title) EQ 0 THEN title = 'System Error'
   
         IF StrUpCase(callingRoutine) EQ "$MAIN$" THEN $
            answer = Dialog_Message(theMessage, _Extra=extra, Title=title, $
               Error=error, Information=information) ELSE $
         IF Keyword_Set(noname) THEN BEGIN
            answer = Dialog_Message(theMessage, _Extra=extra, Title=title, $
               Error=error, Information=information)
         ENDIF ELSE BEGIN
            answer = Dialog_Message(StrUpCase(callingRoutine) + "--> " + $
               theMessage, _Extra=extra, Title=title, $
               Error=error, Information=information)
         ENDELSE
      ENDELSE
   ENDIF ELSE BEGIN
         Help, /Last_Message, Output=traceback_msg ; Required because following MESSAGE call clears traceback info.
         Message, theMessage, /Continue, /NoPrint, /NoName, /NoPrefix, _Extra=extra
         IF Keyword_Set(noname) THEN $
            Print, theMessage ELSE $
            Print, '%' + callingRoutine + ': ' + theMessage 
         answer = 'OK'
   ENDELSE
   
   ; Provide traceback information if requested and this is NOT an informational message.
   IF Keyword_Set(traceback) AND ~Keyword_Set(informational)THEN BEGIN
      IF N_Elements(traceback_msg) NE 0 THEN traceback = traceback_msg ELSE Help, /Last_Message, Output=traceback
      Print,''
      Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
      Print, ''
      FOR j=0,N_Elements(traceback)-1 DO Print, "     " + traceback[j]
   ENDIF
   
   RETURN, answer
END ; ----------------------------------------------------------------------------

