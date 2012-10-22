; docformat = 'rst'
;
; NAME:
;   cgFixPS
;
; PURPOSE:
;   This program modifies an IDL-produced PostScript landscape mode file so that the output
;   is right side up rather than upside down. In other words, it turns a so-called seascape
;   file into an actual landscape file.
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
; This program modifies an IDL-produced PostScript landscape mode file so that the output
; is right side up rather than upside down. In other words, it turns a so-called seascape
; file into an actual landscape file. Files that are not currently in landscape mode will 
; be ignored. Tested with single and multiple page PostScript output from IDL 7.0.1 and 7.1.
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics, Utilities
;    
; :Params:
;     in_filename: in, required, type=string
;        The name of an IDL-produced PostScript file in landscape mode.
;     out_filename: in, optional, type=string
;        The name of the fixed output PostScript file. If not provided, the input
;        file is overwritten. Overwritting assumes proper read/write permission in 
;        TEMP directory and in the directory where the input file is located.
;
; :Keywords:
;     a4: in, optional, type=boolean, default=0
;        Set this keyword if the PostScript file is using a A4 Europeran sized page.
;     ledger: in, optional, type=boolean, default=0
;        Set this keyword if the PostScript file is using a US ledger size (11 x 17 inch) page.
;     legal: in, optional, type=boolean, default=0
;        Set this keyword if the PostScript file is using a US legal size (8.5 x 14 inch) page.
;     letter: in, optional, type=boolean, default=0
;        Set this keyword if the PostScript file is using a US letter size (8.5 x 11 inch) page.
;     pagetype: in, optional, type=string, default="Letter"
;        A generic way to set the page size. A string of "LETTER", "LEDGER", "LEGAL", or "A4".
;     quiet: in, optional, type=boolean, default=0
;        Set this keyword to suppress error messages from the program.
;     success: out, optional, type=boolean
;        If this keyword is set to a named variable, then on output the variable will
;        return a 1 if the operation was successful, and a 0 otherwise. Using this
;        keyword also supresses the program's ability to "throw" an error. Informational
;        messages are issued about program developments, but this program will allow the
;        program caller to decide what to do with unsuccessful program completion.
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
;       Written by: David W. Fanning, 6 August 2009.
;       Change to overwrite input file if output filename is not provided. 6 August 2009. DWF.
;       Incorporated checks for non-landscape mode files and files that have already been fixed. 6 August 2009. DWF.
;       Modified to fix multiple-page PostScript files and to work seamlessly with PS_START output. 8 August 2009. DWF.
;       Ran into a problem in which the PostScript file is stored in the directory pointed
;          to by the IDL_TMPDIR environment variable. Now check to see if the input filename
;          is the same as the output filename and make a change, if necessary. 22 July 2010. DWF.
;        Retreated to standard error handling with ERROR_MESSAGE as there are inevitable errors. 2 August 2010. DWF.
;        Output file was created, even if not used. Now deleting file and issuing messages to
;           explain why output file was not created. 1 November 2010. DWF.
;        Added SUCCESS and QUIET keywords. 15 Novemember 2010. DWF.
;        PostScript file structure changed in IDL 8. Made adjustment to find the 
;            PageBoundingBox line. 19 Dec 2010. DWF.
;            
; :Copyright:
;     Copyright (c) 2009-2012, Fanning Software Consulting, Inc.
;-
PRO cgFIXPS, in_filename, out_filename, $
    A4=A4, $
    LEDGER=ledger, $
    LEGAL=legal, $
    LETTER=letter, $
    PAGETYPE=pagetype, $
    QUIET=quiet, $
    SUCCESS=success

  Compile_Opt idl2
  
  ; Error handling.
  IF Arg_Present(success) THEN BEGIN
      Catch, theError
      IF theError NE 0 THEN BEGIN
          Catch, /CANCEL
          success = 0
          IF N_Elements(out_lun) NE 0 THEN Free_Lun, out_lun
          IF N_Elements(in_lun) NE 0 THEN Free_Lun, in_lun
          IF ~Keyword_Set(quiet) THEN Print, !Error_State.MSG
          RETURN
      ENDIF
  ENDIF ELSE BEGIN
      Catch, theError
      IF theError NE 0 THEN BEGIN
          Catch, /CANCEL
          IF ~Keyword_Set(quiet) THEN ok = Error_Message()
          success = 0
          IF N_Elements(out_lun) NE 0 THEN Free_Lun, out_lun
          IF N_Elements(in_lun) NE 0 THEN Free_Lun, in_lun
          RETURN
      ENDIF
  ENDELSE
  
  ; Assume success
  success = 1
  
  ; Is there an input filename?
  IF N_Elements(in_filename) EQ 0 THEN BEGIN
      in_filename = Dialog_Pickfile(FILTER='*.ps', TITLE='Select PostScript file to repair.')
      IF in_filename EQ "" THEN RETURN
  ENDIF
  
  ; Is there an output filename?
  IF N_Elements(out_filename) EQ 0 THEN BEGIN
        root_name = cgRootName(in_filename, EXTENSION=ext)
        out_filename = Filepath(ROOT_DIR=GetEnv('IDL_TMPDIR'), root_name + '_tmp.' + ext)
        print_out = 1
        no_output_filename = 1
  ENDIF ELSE no_output_filename = 0
  
  ; The out_filename can be the same as the in_filename in some cases.
  IF out_filename EQ in_filename THEN BEGIN
     rootname = cgRootName(out_filename, DIRECTORY=outDir, EXTENSION=ext)
     out_filename = FilePath(ROOT_DIR=outDir, rootname + '_tmp.' + ext)
  ENDIF
  
  ; Open the output filename.
  OpenW, out_lun, out_filename, /GET_LUN
  
  ; What kind of page is this?
  IF N_Elements(pagetype) EQ 0 THEN BEGIN
     IF Keyword_Set(A4) THEN pageType = 'A4'
     IF Keyword_Set(legal) THEN pageType = 'LEGAL'
     IF Keyword_Set(ledger) THEN pageType = 'LEDGER'
     IF Keyword_Set(letter) THEN pageType = 'LETTER'
     IF N_Elements(pageType) EQ 0 THEN pageType = 'LETTER' 
  ENDIF ELSE pageType = StrUpCase(pageType)
  
  ; Set the rotate/translate command appropriately. 
  CASE pageType OF
        'LETTER': rtcmd = '180 rotate -612 -792 translate'
        'A4':     rtcmd = '180 rotate -595.28 -841.89 translate'
        'LEGAL':  rtcmd = '180 rotate -612 -1008 translate'
        'LEDGER': rtcmd = '180 rotate -792 -1224 translate'
        ELSE: Message, 'Unknown PageType: ' + pageType
  ENDCASE
  
  ; Move along in the file until the end of the Prolog.
  line = ""
  count = 0
  target = "void"
  buffer = StrArr(100)
  
  OpenR, in_lun, in_filename, /GET_LUN
  WHILE target NE '%%EndProlog' DO BEGIN
      ReadF, in_lun, line
      buffer[count] = line
      target = StrMid(line, 0, 11)
      count = count + 1
      IF count MOD 100 EQ 0 THEN buffer = [buffer, StrArr(100)]
  ENDWHILE
  
  ; Read the next 10 lines all at once. If you have already processed
  ; this file, exit. But if you haven't write the buffer to the output file.
  in_lines = StrArr(10)
  ReadF, in_lun, in_lines
  
  ; Is this a landscape file? If not, out of here.
  index = Where(in_lines EQ '%%PageOrientation: Landscape', landscape_cnt)
  IF landscape_cnt EQ 0 THEN BEGIN
        Free_Lun, in_lun
        Free_Lun, out_lun
        File_Delete, out_filename
        Message, 'File not a landscape file. Exiting...', /Informational
        RETURN
  ENDIF
  
  ; Has this file already been mucked with? If so, out of here. If not, write
  ; the buffer to the output file.
  IF StrMid(in_lines[1], 0, 10) EQ '180 rotate' THEN BEGIN
        Free_Lun, in_lun
        Free_Lun, out_lun
        File_Delete, out_filename
        Message, 'File is in the proper rotation. Exiting...', /Informational
        RETURN
  ENDIF ELSE BEGIN
        FOR j=0,count-1 DO PrintF, out_lun, buffer[j]
  ENDELSE
  
  count = count + 10
  
  ; We are going to add an extra line in the output file.
  out_lines = StrArr(11)
  out_lines[0] = in_lines[0]
  out_lines[2:10] = in_lines[1:9]
  out_lines[1] = rtcmd
  
  ; Calculate the new bounding box boundaries.
  bbox_line_num = Where(StrMid(in_lines, 0, 17) EQ '%%PageBoundingBox', bbox_count)
  IF bbox_count EQ 0 THEN Message, 'Cannot find PageBoundingBox line in file.'
  bbox = StrMid(in_lines[bbox_line_num[0]], 18)
  x0 = 0
  x1 = 0
  y0 = 0
  y1 = 0
  ReadS, bbox, x0, y0, x1, y1
  CASE pageType OF
        'LETTER': BEGIN
            lx = String(612 - x1, FORMAT='(I5)')
            ly = String(792 - y1, FORMAT='(I5)')
            ux = String(612 - lx, FORMAT='(I5)')
            uy = String(792 - ly, FORMAT='(I5)')
            END
        'A4': BEGIN
            lx = String(595.28 - x1, FORMAT='(I5)')
            ly = String(841.89 - y1, FORMAT='(I5)')
            ux = String(595.28 - lx, FORMAT='(I5)')
            uy = String(841.89 - ly, FORMAT='(I5)')
            END
        'LEGAL': BEGIN
            lx = String(612 - x1, FORMAT='(I5)')
            ly = String(1008 - y1, FORMAT='(I5)')
            ux = String(612 - lx, FORMAT='(I5)')
            uy = String(1008 - ly, FORMAT='(I5)')
            END
        'LEDGER': BEGIN
            lx = String(792 - x1, FORMAT='(I5)')
            ly = String(1224 - y1, FORMAT='(I5)')
            ux = String(792 - lx, FORMAT='(I5)')
            uy = String(1224 - ly, FORMAT='(I5)')
            END
  ENDCASE
  
  ; Output the new boundaries.
  out_lines[5] = '%%PageBoundingBox: ' + lx + ly + ux + uy
  FOR j=0,10 DO PrintF, out_lun, out_lines[j]
  
  ; Output the rest of the file, looking for another "%%Page:" marker.
  WHILE ~EOF(in_lun) DO BEGIN
     ReadF, in_lun, line
     PrintF, out_lun, line
     IF StrMid(line, 0, 7) EQ '%%Page:' THEN BEGIN
          IF Keyword_Set(A4) THEN BEGIN
              PrintF, out_lun, rtcmd
          ENDIF ELSE BEGIN
              PrintF, out_lun, rtcmd
          ENDELSE
     ENDIF
  ENDWHILE

  ; Clean up.
  Free_lun, in_lun
  Free_lun, out_lun
  
  ; If there was no output filename given, then we are going
  ; to replace the input file with the temporary output file.
  IF no_output_filename THEN BEGIN
    inputDir = File_Dirname(in_filename)
    root_name = File_BaseName(in_filename)
    
    ; Can you write into the input directory?
    IF File_Test(in_filename, /WRITE) EQ 0 THEN Message, 'Cannot write TEMPORARY file into input file directory.'
    
    ; Replace the input file with the temporary output file.
    File_Delete, in_filename
    File_Move, out_filename, in_filename
  ENDIF 
  
END
