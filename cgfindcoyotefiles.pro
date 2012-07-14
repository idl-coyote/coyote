; docformat = 'rst'
;
; NAME:
;   cgFindCoyoteFiles
;
; PURPOSE:
; 
;   This procedure looks for Coyote Library routines in every directory in the IDL path
;   and reports the directories that it strongly suspects contain Coyote Library files.
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
; This procedure looks for Coyote Library routines in every directory in the IDL path
; and reports the directories that it strongly suspects contain Coyote Library files.
; The purpose of this is to aid in eliminating old and out-dated Coyote Library routines
; from your IDL path. The goal is to have a single Coyote Library directory with the latest
; Coyote Library programs in it.
;
; :Categories:
;    Utility
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
;     Written, 11 July 2012 by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
PRO cgFindCoyoteFiles

   Compile_Opt idl2
   
   ON_Error, 2

   ; Make an array of directories on the IDL path.
   directories = StrSplit(!PATH, Path_Sep(/SEARCH_PATH), /Extract)
   numDirs = N_Elements(directories)
   
   ; Search each directory for programs that have a "FSC_" or "cg" prefix. These
   ; are potential coyote directories. If you find files with an "FSC_" prefix, you
   ; can feel confident this is a Coyote Library containing directory. If you find
   ; files with a "cg" prefix, you need at least 10 files to feel confident, or you
   ; need to find cgcolor.pro in the directory to feel confident. Mark all the files
   ; you feel confident about and report them.
   coyoteDirs = BytArr(numDirs)
   
   FOR j=0,numDirs-1 DO BEGIN
      foundFiles = 0
      thisDir = directories[j]
      files = File_Search(Filepath(Root_Dir=thisdir, 'FSC_*.pro'), COUNT=count)
      IF count GT 0 THEN foundFiles = 1
      IF foundFiles THEN BEGIN
          coyoteDirs[j] = 1
          CONTINUE
      ENDIF
      files = File_Search(Filepath(Root_Dir=thisdir, 'cg*.pro'), COUNT=count)
      afile = File_Search(Filepath(Root_Dir=thisdir, 'cgcolor.pro'), COUNT=acount)
      IF (count GT 10) || (acount EQ 1) THEN BEGIN
         coyoteDirs[j] = 1
      ENDIF
   ENDFOR
   
   ; Report the likely directories here.
   index = Where(coyoteDirs EQ 1, fileCount)
   IF fileCount GT 0 THEN BEGIN
      Print, ""
      Print, 'The following directories in !PATH probably have Coyote Library routines in them.'
      directories = directories[index]
      FOR j=0,N_Elements(directories)-1 DO Print, "     " + directories[j]
   ENDIF ELSE BEGIN
      Print, 'No COYOTE directories have been found in your IDL path.'
   ENDELSE
END