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
;   Defines a default character size for Coyote Graphics routines (cgPlot, cgContour, etc.)
;   IF !P.Charsize is set, the function simply returns !P.Charsize.
;
; :Categories:
;    Graphics, Utilities
;    
; :Keywords:
;     adjustsize: in, optional, type=boolean, default=0
;        If this keyword is set, the output character size is adjusted to
;        fit the size of the output graphics window. No adjustment is ever
;        done in PostScript. Applies only when !P.Charsize=0.
;     font: in, optional, type=integer, default=!P.Font
;        The font type: -1 = Hershey, 0 = hardware, 1 = true-type. 
;          
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: david@idlcoyote.com
;           Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 11 January 2011. DWF.      
;        Added an ADJUSTSIZE keyword to allow adjustable sizing of characters
;           in resizeable graphics windows. 24 April 2011. DWF.  
;        Made sure this program only adjusts text size on devices that support 
;           windows. 20 July 2011. DWF.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
FUNCTION cgDefCharSize, ADJUSTSIZE=adjustsize, FONT=font

    Compile_Opt idl2
    
    ; Return to caller on an error.
    On_Error, 2
    
    ; Check parameters
    IF N_Elements(font) EQ 0 THEN font = !P.Font
    
    ; If the current window is a cgWindow, then the ADJUSTSIZE property of the
    ; window is used to set the AdjustSize keyword. This can only be done on
    ; devices that support windows.
    IF ~((!D.Flags AND 256) NE 0) THEN adjustsize = 0
    IF (N_Elements(adjustsize) EQ 0) THEN BEGIN
    
        ; Each instance of cgWindow will store evidence of its
        ; existance in a linked list.
        DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
        IF ~exists THEN BEGIN
            adjustsize = 0 
        ENDIF ELSE BEGIN
            IF Obj_Valid(!FSC_WINDOW_LIST) THEN BEGIN
                wid = cgQuery(/Current, COUNT=count)
                IF count GT 0 THEN BEGIN
                   IF wid EQ !D.Window THEN BEGIN
                       void = cgQuery(ObjectRef=windowObj, /Current)
                       IF Obj_Valid(windowObj) THEN windowObj -> GetProperty, AdjustSize=adjustsize
                   ENDIF ELSE adjustsize = 0
                ENDIF ELSE adjustsize = 0
            ENDIF ELSE adjustsize = 0
        ENDELSE
            
    ENDIF

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
             
        ; Adjust this size for the size of the window. Can't do this in PostScript
        ; for some reason, as it creates an extra page of output.
        IF !D.Name NE 'PS' THEN BEGIN
        
          ; The adjustment attempts to compensate for multiple plots in the window.
          IF Keyword_Set(adjustsize) THEN BEGIN
                thisCharsize = Str_Size('This is the text size for a normal window', $
                    0.65 * ((10-!P.multi[1])/10. > 0.5), INITSIZE=thisCharsize)
           ENDIF
        ENDIF

    ENDIF ELSE thisCharSize = !P.Charsize
        
    
    RETURN, thisCharSize
    
END
