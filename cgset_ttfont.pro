; docformat = 'rst'
;
; NAME:
;   cgSet_TTFont
;
; PURPOSE:
;   The purpose of this procedure is to allow the user to set the True-Type font in use
;   both at the IDL command line and in a PostScript file.
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
; The purpose of this procedure is to allow the user to set the True-Type font in use
; both at the IDL command line and in a PostScript file. Normally, True-Type fonts are
; set with the Device command and the Set_Font and TT_Font keywords. However, if such a
; command is issued at the IDL command line, it has no effect in the PostScript device.
; The cgSet_TTFont command will set the True-Type font for both devices, thereby keeping 
; them in sync with each other so that the expected output can be created both on the display 
; and in a PostScript file.
; 
; Using this procedure also has an effect on Coyote Graphics routines by setting both the
; PS_TT_FONT and PS_FONT keywords to cgWindow_SetDefs. This ensures that any Coyote Graphics
; routine that creates a PostScript file will use the requested font upon producing PostScript
; output. Revert to the default Helvetica font by either using the font name "Helvetica", 
; or by simply calling the function without a font name parameter. If the ADDCMD keyword is 
; used, the font change will apply ONLY to the current cgWindow (i.e., the PS_TT_FONT and PS_FONT 
; keywords will be set for just the current cgWindow).
;
; :Categories:
;    Utility
;    
; :Params:
;    font_name: in, optional, type=string, default="Helvetica"
;         The name of a true-type font to use in producing graphical output.
;       
; :Keywords:
;    addcmd: in, optional, type=boolean, default=0
;         Set this keyword to apply this True-Type font ONLY to the current cgWindow via
;         the PS_TT_FONT and PS_FONT keywords of cgControl. An error will occur if there
;         is no current cgWindow in existance.
;         
; :Examples:
;    Here is how to use this program to use the Times True-Type font::
;        cgSet_TTFont, 'Times'
;        cgPlot, cgDemoData(1), Title='This is Times Text', Output='cgplot.png'
;        
;    or::
;        cgSet_TTFont, 'Times'
;        cgPS_Open, 'cgplot.ps', Font=1
;        cgPlot, cgDemoData(1), Title='This is Times Text'
;        cgPS_Close, /PNG
;         
;    or, to set the font ONLY for the current cgWindow::
;        cgPlot, cgDemoData(1), Title='This is Times Text', /Window
;        cgSet_TTFont, 'Times', /AddCmd
;        
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
;        Written, 21 May 2013 by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;-
PRO cgSet_TTFont, font_name, ADDCMD=addcmd
   
    Compile_Opt idl2
    
    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF
    
    ; A font name is required. Get default here, if needed.
    IF N_Elements(font_name) EQ 0 THEN font_name = "Helvetica"
    
    ; Add this to a graphics window?
    IF Keyword_Set(addcmd) THEN BEGIN
        currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
        IF wincnt EQ 0 THEN Message, 'There is no current cgWindow to set the font for.'
        cgControl, PS_TT_FONT=font_name, PS_FONT=1
        RETURN
    ENDIF    
    
    ; Set the true-type font for the display. Only interested in X, WIN, and PS devices.
    CASE StrUpCase(!D.Name) OF
       'WIN': Device, Set_Font=font_name, /TT_FONT
       'X':   Device, Set_Font=font_name, /TT_FONT
       'PS': BEGIN
             IF StrUpCase(!Version.OS_Family) EQ 'WINDOWS' THEN BEGIN
                Set_Plot, 'WIN'
             ENDIF ELSE BEGIN
                Set_Plot, 'X'
             ENDELSE
             Device, Set_Font=font_name, /TT_FONT
             Set_Plot, 'PS'
             END
        ELSE:
    ENDCASE
    
    ; Set the true-type font for the PostScript device. It is sticky, so it should stay this way.
    thisDevice = !D.Name
    Set_Plot, 'PS'
    Device, Set_Font=font_name, /TT_FONT
    Set_Plot, thisDevice
    
    ; Set the default Coyote Graphics PostScript font and make sure True-Type fonts are used.
    cgWindow_SetDefs, PS_TT_FONT=font_name, PS_FONT=1
   
END