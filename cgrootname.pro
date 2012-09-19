; docformat = 'rst'
;
; NAME:
;   cgRootName
;
; PURPOSE:
;    The purpose of this routine is to extract from a long file path the
;    base or root file name. That is, the name of the actual file without
;    the preceeding directory information or the final file extension.
;    The directory information and file extension can be obtained via
;    keywords. It is similar to File_Basename in IDL.
;
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
; The purpose of this routine is to extract from a long file path, the
; base or root file name. That is, the name of the actual file without
; the preceeding directory information or the final file extension.
; The directory information and file extension can be obtained via
; keywords. It is similar to File_Basename in IDL.
;
; :Categories:
;    Utilities
;    
; :Returns: 
;      The base or root file name is returned. This is the "name" of the file
;      without either the preceeding directory information, or the prepended
;      file extension.
;      
; :Params:
;    filename: in, required, type=string
;        This file name to be parsed. It may be a relative or absolute path
;        name to a file.
;        
; :Keywords:
;     directory: out, optional, type=string
;         The name of the directory preceeding the root file name. If the file name
;         contains no directory information, this keyword is set equal to the current
;         working directory at the time the program is called.
;     extension: out, optional, type=string
;         The (usually!) three character file extention of the file.
;     path_separator: in, optional, type=string
;         The string to use as a path separator in parsing the file. If undefined
;         the out of Path_Sep() is used.
;
; :Examples:
;    Print, the root name, directory, and extension of a file::
;       IDL> thePath = "C:\rsi\idl7.8\lib\jester.pro"
;       IDL> Print, cgRootName(thePath, Directory=theDirectory, Extension=theExtension)
;           jester
;       IDL> Print, theDirectory
;            C:\rsi\idl7.8\lib\
;       IDL> Print, theExtension
;            pro
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
;     Change History::
;        Written by: David W. Fanning, 31 July 2003.
;        Modified by KaRo, 13 Feb. 2005 to allow dots in the filename.
;        Added PATH_SEPARATOR keyword. 25 July 2005. DWF.
;        Added ability to recongnize directory by path separator in last character. 19 Sept 2005. DWF.
;        If directory is blank (because a relative filename was passed), set to current directory. 6 Aug 2009. DWF.
;        There were a couple of instances where the directory did NOT end in a path separator. Fixed. 24 Feb 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2003-2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgRootName, filename, $
   Directory=directory, $
   Extension=extension, $
   Path_Separator=pathsep

   On_Error, 2

   ; Default values.
   directory = ""
   extension = ""
   file = ""

   ; If there is no filename, return NULL.
   IF (N_Elements(filename) EQ 0) OR (filename EQ "") THEN RETURN, file

   ; Is a path separator specified?
   IF N_Elements(pathsep) EQ 0 THEN pathsep = Path_Sep()

   ; If the last element of filename is a path separator, then separation is easy.
   IF StrMid(filename, StrLen(filename)-1, 1) EQ pathsep THEN BEGIN
      directory = filename
      RETURN, file
   ENDIF

   ; Split the file by the path separator and extract into parts.
   parts = StrSplit(filename, pathsep, /Extract)
   IF StrMid(filename, 0, 1) EQ pathsep AND N_Elements(parts) GT 1 THEN parts[0] = pathsep + parts[0]
   numParts = N_Elements(parts)

   ; Put the parts back together after identifying them.
   CASE numParts OF
      1: BEGIN
            subparts = StrSplit(filename, ".", /Extract)
            numsubParts = N_Elements(subparts)
            CASE numsubParts OF
               1: file = subparts[0]
               2: BEGIN
                     file = subparts[0]
                     extension = subparts[1]
                  END
               ELSE: BEGIN
                     file = StrJoin(subparts[0:numsubParts-2],'.')
                     extension = subparts[numsubParts-1]
                  END
            ENDCASE
         END

      2: BEGIN
            file = parts[1]
            directory = parts[0] + pathsep
            subparts = StrSplit(file, ".", /Extract)
            numsubParts = N_Elements(subparts)
            CASE numsubParts OF
               1: file = subparts[0]
               2: BEGIN
                     file = subparts[0]
                     extension = subparts[1]
                  END
               ELSE: BEGIN
                     file = StrJoin(subparts[0:numsubParts-2],'.')
                     extension = subparts[numsubParts-1]
                  END
            ENDCASE
         END

      ELSE: BEGIN

            file = parts[numParts-1]
            subparts = StrSplit(file, ".", /Extract)
            numsubParts = N_Elements(subparts)
            CASE numsubParts OF
               1: file = subparts[0]
               2: BEGIN
                     file = subparts[0]
                     extension = subparts[1]
                  END
               ELSE: BEGIN
                     file = StrJoin(subparts[0:numsubParts-2],'.')
                     extension = subparts[numsubParts-1]
                  END
            ENDCASE
            directory = parts[0]
            FOR j=1,numParts-2 DO BEGIN
               directory = directory + pathsep + parts[j]
            ENDFOR
            directory = directory + pathsep
         END

   ENDCASE
   
   ; If the directory is a null string. Make it the current directory.
   IF directory EQ "" THEN CD, CURRENT=directory
   
   ; Does the directory need a final path separator.
   IF (directory NE "") THEN BEGIN
      lastChar = StrMid(directory, 0, 1, /REVERSE_OFFSET)
      IF lastChar NE pathsep THEN directory = directory + pathsep
   ENDIF

   RETURN, file

END

