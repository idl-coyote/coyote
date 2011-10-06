;+
; NAME:
;    DIRPATH
;
; PURPOSE:
;
;    The purpose of this function is to return a device-independent
;    name of a directory. It is similar to the IDL-supplied FILEPATH
;    routine, except that a file name is not required.
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: david@idlcoyote.com
;   Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; CATEGORY:
;
;    Utility.
;
; CALLING SEQUENCE:
;
;    IDL> theDirectory = DIRPATH('examples')
;    IDL> Print, theDirectory
;             C:\IDL\IDL56\examples
;
; INPUTS:
;
;    subDirectory:    This is a string argument containing the name of the
;                     sub-directory you wish to use. It can be a string
;                     array of sub-directory names. By default, the subDirectory
;                     is set to ['examples', 'data']. To only return the Root_Directory,
;                     set the subDirectory to a null string ("").
;
; KEYWORDS:
;
;    ROOT_DIRECTORY: The name of the root directory to use. By default,
;                    the root directory is set to !DIR.
;
; OUTPUTS:
;
;    The machine-independent directory path.
;
; MODIFICATION HISTORY:
;
;    Written by: David W. Fanning, 28 April 2003.
;-
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
FUNCTION DirPath, subdirectory, RootDirectory=rootdirectory

   ; Returns the name of a directory in the spirit of FILEPATH.

   ; rootdirectory - The name of the root directory where subdirectories branch.
   ;                 Use !Dir by default.
   ;
   ; subdirectory -  The name of a subdirectory. Use string array to specify path of
   ;                 multiple subdirectories. If absent, uses ['examples', 'data'].

   ; Catch the error. If something goes wrong, return the current directory.

   Forward_Function Error_Message

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      CD, Current=currentDir
      Return, currentDir
   ENDIF

   ; Check for arguments. Define defaults, if necessary.
   IF N_Elements(rootdirectory) EQ 0 THEN rootdirectory = !Dir
   IF N_Elements(subdirectory) EQ 0 THEN subdirectory = ['examples', 'data']

   ; Use FILEPATH to construct a device-independent file name. Strip the directory
   ; information from that and return it.
   source = FilePath(Root_Dir=rootdirectory, SubDirectory=subdirectory, 'junk.pro')
   directory = Strmid(source, 0, Strpos(source, Path_Sep(), /REVERSE_SEARCH))

   RETURN, directory
END
