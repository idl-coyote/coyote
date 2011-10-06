 ;+
; NAME:
;      iXYOUTS
;
; PURPOSE:
;
;      This program is meant to be an iTools version of the IDL direct graphics
;      program XYOUTS. In other words, it is a way to add strings to a current
;      iTool.
;
; AUTHOR:
;
;     FANNING SOFTWARE CONSULTING
;     David W. Fanning, Ph.D.
;     1645 Sheely Drive
;     Fort Collins, CO 80526 USA
;     Phone: 970-221-0438
;     E-mail: david@idlcoyote.com
;     Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; CATEGORY:
;
;     Graphics
;
; CALLING SEQUENCE:
;
;     iXYOUTS, theString
;
; ARGUMENTS:
;
;     theString:     The string that will be added to the iTool.
;
; KEYWORDS:       
;
;     Any keyword supported by IDLgrTEXT will be passed along to 
;     the string. Because I want you to see *something* I will
;     tentitively define the following keywords with these
;     default values. You can overwrite any of these defaults
;     by defining your own keywords.
;
;     ALIGNMENT: 0.0             ; Center the string about the location.
;     FONT_SIZE: 12              ; 12-point font.
;     LOCATION: [0.0, 0.0, 1.0]  ; In the center of the unzoomed iTool.
;
;     In addition, you can use the following ID keyword to select the idenifier
;     of a particular iTool. (This is usually obtained from the iTool when it
;     is created: iPlot, findgen(11), IDENTIFIER=id)
;
;     ID:    The identifier of an iTool when the text string is to be displayed.
;            If not used, the current iTool is used.
;
; OUTPUTS:
;
;     None.
;
; EXAMPLE:
;
;     ; First, create an iTool of some kind.
;     IDL> iPlot, findgen(11)
;
;     ; Add a string.
;     IDL> iXYOUTS, 'This is a string'
;
;     ; Add a left-justified red string in 24-point type.
;     IDL> iXYOUTS, 'This is red string', ALIGNMENT=0, COLOR=[255,0,0], FONT_SIZE=24, LOCATION=[-1.25, -0.75, 1.0]
;
; RESTRICTIONS:
;
;     The default view when a iTool starts up and there has been no magification
;     or other manipulation appears to go from -1.4 to 1.4 in X units, from -1.0 to 1.0
;     units in Y, and from 1.0 to -1.0 units in Z. I put the default position in the
;     very center of this viewplane rectangle, at [0,0,1.0], but there is no guarantee you 
;     will actually see the string with this or any other position you use. We are working with 
;     object graphics objects, after all! The text can be selected and moved to position it more
;     exactly. Also, the text is being put into the view in an ONGLASS mode, unless you change
;     this with keywords.
;
; DEPENDENCIES:
;
;     Requires ERROR_MESSAGE from the Coyote Library:
;
;         http://www.idlcoyote.com/programs/error_message.pro
;
; MODIFICATION HISTORY:
;
;      Written by: David W. Fanning, 31 August 2007, from suggestions made
;         by Mike Galloy and Ken Bowman on the IDL newsgroup.
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
PRO iXYOUTS, theString, ID=id, _EXTRA=extra

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /Cancel
       void = Error_Message()
       RETURN
    ENDIF
    
    ; Did the user pass a string?
    IF N_Elements(theString) EQ 0 THEN Message, 'Must pass a string to add to iTool.'
    IF Size(theString, /TNAME) NE 'STRING' THEN Message, 'Input argument must be a string.'
    theString = theString[0] ; No arrays.
    
    ; Did the user specify an iTool?
    IF Keyword_Set(id) EQ 0 THEN BEGIN
        id = ITGETCURRENT(TOOL=theTool) 
    ENDIF ELSE BEGIN
        theSystem = _IDLitSys_GetSystem()
        theTool = theSystem -> GetByIdentifier(id)
    ENDELSE
    
    ; Is the tool valid?
    IF Obj_Valid(theTool) EQ 0 THEN Message, 'Cannot find a valid iTool to add the string to.'
       
    ; Get the annotation layer for the tool. We will add the string to this layer.
    idAnnotate = theTool -> FindIdentifiers('*/ANNOTATION LAYER')              
    theAnnotation  = theTool -> GetByIdentifier(idAnnotate)                        
    theAnnotation -> Add, Obj_New('IDLitVisText', _STRING = theString, $           
           ALIGNMENT = 0.5, $
           FONT_SIZE = 12, $
           LOCATION  = [0.0, 0.0, 1.0], $
           _EXTRA=extra)
           
   END
