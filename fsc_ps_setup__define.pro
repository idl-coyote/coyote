;+
; NAME:
;   FSC_PS_SETUP__DEFINE
;
; PURPOSE:
;
;    The purpose of FSC_PS_SETUP__DEFINE is to define a structure that is
;    use with PS_START and PS_END, programs that make it easy to set-up
;    for and close a PostScript file. The programs work in close conjunction
;    with PSCONFIG, another program from the Coyote Library.
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
;       Graphics, File Output, PostScript
;
; CALLING SEQUENCE:
;
;       Used internally in PS_START and PS_END.
;
; COMMON BLOCKS:
;
;       _$FSC_PS_START_   Contains the PS_STRUCT structure for communication between
;                         PS_START and PS_END.
;
; MODIFICATION HISTORY:
;
;       Separated from PS_START file, 7 April 2009, by David W. Fanning.
;       Added PAGETYPE field to structure. 8 August 2009. DWF.
;       Changes to handle inability to create raster files from PS encapsulated files in 
;           landscape mode. Added "encapsulated" field to structure. 26 Aug 2011. DWF.
;-
;
;******************************************************************************************;
;  Copyright (c) 2008 - 2011, by Fanning Software Consulting, Inc.                         ;
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
PRO FSC_PS_SETUP__DEFINE

    struct = { FSC_PS_SETUP, $
               currentDevice: "", $   ; Current graphics device when PS_START is called.
               setup: 0, $            ; A flag that makes sure PS device is opened/closed in correct sequence.
               convert: "", $         ; Set to type of output file, if converting with ImageMagick.
               filename: "", $        ; PostScript filename.
               landscape: 0, $        ; Set to 1 if PostScript in landscape mode.
               encapsulated: 0, $     ; Set to 1 if PostScript is in encapsulated mode.
               quiet: 0, $            ; Flag to indicate if messages should be suppressed.
               pagetype: "", $        ; The type of page used. (Letter, A4, Legal, Ledger).
               tt_font:"", $          ; The name of the True-Type font in effect.
               font: 0, $             ; The type of font being used. -1 Hershey, 0 PostScript, 1 True-type
               p: !P, $               ; The plotting system variable.
               x: !X, $               ; The X axis system variable.
               y: !Y, $               ; The Y axis system variable.
               z: !Z $                ; The Z axis system variable.
              }
            
END ;---------------------------------------------------------------



