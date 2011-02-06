; docformat = 'rst'
;
; NAME:
;   cgDefCharSize
;
; PURPOSE:
;   Defines a default character size for Coyote Graphics routines (cgPlot, cgContour, etc.)
;   IF !P.Charsize is set, the function simply returns !P.Charsize.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
;   Defines a default character size for Coyote Graphics routines (cgPlot, cgContour, etc.)
;   IF !P.Charsize is set, the function simply returns !P.Charsize.
;
; :Categories:
;    Graphics, Utilities
;    
; :Params:
;    None.
;       
; :Keywords:
;     font: in, optional, type=integer, default=!P.Font
;        The font type: -1 = Hershey, 0 = hardware, 1 = true-type. 
;          
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: davidf@dfanning.com
;           Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; :History:
;     Change History::
;        Written, 11 January 2011. DWF.        
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
FUNCTION cgDefCharSize, FONT=font

    Compile_Opt idl2
    
    ; Return to caller on an error.
    On_Error, 2
    
    ; Check parameters
    IF N_Elements(font) EQ 0 THEN font = !P.Font

    ; Calculate a default character size. We absolutely do not want to
    ; do this if !P.Charsize is not set to its default value of 0.
    IF !P.Charsize EQ 0 THEN BEGIN
        
            CASE StrUpCase(!Version.OS_Family) OF
            
                'WINDOWS': BEGIN
                    IF Total(!P.MULTI) EQ 0 THEN BEGIN
                        thisCharsize = 1.25                         
                    ENDIF ELSE BEGIN
                        totalplots = !P.Multi[1]*!P.Multi[2]*(!P.Multi[3] > 1)
                        CASE 1 OF
                            totalplots LE 4: thisCharsize = 1.25
                            totalplots GT 4: thisCharsize = 1.00
                        ENDCASE
                    ENDELSE
                    IF (font EQ 1) THEN BEGIN
                        IF Total(!P.MULTI) EQ 0 THEN BEGIN
                            thisCharsize = 1.50 
                        ENDIF ELSE BEGIN
                            totalplots = !P.Multi[1]*!P.Multi[2]*(!P.Multi[3] > 1)
                            CASE 1 OF
                                totalplots LE 4: thisCharsize = 1.50
                                totalplots GT 4: thisCharsize = 1.25
                            ENDCASE
                        ENDELSE
                    ENDIF
                    END
                    
                ELSE: BEGIN
                    IF Total(!P.MULTI) EQ 0 THEN BEGIN
                        thisCharsize = 1.50 
                    ENDIF ELSE BEGIN
                         totalplots = !P.Multi[1]*!P.Multi[2]*(!P.Multi[3] > 1)
                        CASE 1 OF
                            totalplots LE 4: thisCharsize = 1.50
                            totalplots GT 4: thisCharsize = 1.25
                        ENDCASE
                    ENDELSE 
                    IF (font EQ 1) THEN BEGIN
                        IF Total(!P.MULTI) EQ 0 THEN BEGIN
                            thisCharsize = 1.75 
                        ENDIF ELSE BEGIN
                            totalplots = !P.Multi[1]*!P.Multi[2]*(!P.Multi[3] > 1)
                            CASE 1 OF
                                totalplots LE 4: thisCharsize = 1.75
                                totalplots GT 4: thisCharsize = 1.50
                            ENDCASE
                        ENDELSE
                    ENDIF
                    END
            
            ENDCASE
             
    ENDIF ELSE thisCharSize = !P.Charsize
        
    ; Adjust this size for the size of the window.
    thisCharsize = Str_Size('This is the text size for a normal window', 0.475, INITSIZE=thisCharsize)
    
    RETURN, thisCharSize
    
END
