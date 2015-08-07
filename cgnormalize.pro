; docformat = 'rst'
;
; NAME:
;   cgNormalize
;
; PURPOSE:
;   This is a utility routine to calculate the scaling vector required to position 
;   a graphics primitive of specified range at a specific position in an arbitray 
;   coordinate system.
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
; This is a utility routine to calculate the scaling vector required to position a 
; graphics primitive of specified range at a specific position in an arbitray coordinate 
; system. The scaling vector is given as a two-element array like this::
;
;    scalingVector = [translationFactor, scalingFactor]
;
; The scaling vector should be used with the [XYZ]COORD_CONV keywords of a graphics object 
; or model. For example, if you wanted to scale an X axis into the coordinate range of 
; -0.5 to 0.5, you might type something like this::
;
;    xAxis->GetProperty, Range=xRange
;    xScale = cgNormalize(xRange, Position=[-0.5, 0.5])
;    xAxis, XCoord_Conv=xScale
;
; :Categories:
;    Graphics Utility
;
; :Params:
;    range: in, required
;       A two-element vector specifying the data range.
;
; :Keywords:
;    position: in, optional, type=float
;       A two-element vector specifying the location in the coordinate system you 
;       are scaling into. The vector [0.0,1.0] is used by default if Position is not 
;       specified.
;
; :Examples:
;    Here is how to use this program::
;    
;       xAxis->GetProperty, Range=xRange
;       xScale = cgNormalize(xRange, Position=[-0.5, 0.5])
;       xAxis, XCoord_Conv=xScale

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
;       Written by:  David W. Fanning, OCT 1997.
;       Fixed a problem with illegal divide by zero. 21 April 2005. DWF.
;       Fixed a problem when range[0] is greater than range[1]. 11 July 2006. DWF.
;       Renamed to FSC_Normalize to avoid conflicts with 10,000 other programs named NORMALIZE. 17 October 2008. DWF.
;       Renamed to cgNormalize 6 February 2013. DWF.
;       The number I chose (1e-12) to fix the illegal divide by zero problem was too large. I'm now using 
;           1d-25 instead. 9 September 2013. DWF.
;       
; :Copyright:
;     Copyright (c)1997-2013, Fanning Software Consulting, Inc.
;-
FUNCTION cgNormalize, range, Position=position

    On_Error, 2 ; Return to caller.
    
    IF N_Params() EQ 0 THEN Message, 'Please pass range vector as argument.'
    
    IF (N_Elements(position) EQ 0) THEN position = [0.0D, 1.0D] ELSE $
        position=Double(position)
    range = Double(range)
    
    IF range[1] GE range[0] THEN BEGIN
       scale = [((position[0]*range[1])-(position[1]*range[0])) / $
           ((range[1]-range[0]) > 1d-25), (position[1]-position[0])/((range[1]-range[0]) > 1d-25)]
    ENDIF ELSE BEGIN
       scale = [((position[1]*range[0])-(position[0]*range[1])) / $
           ((range[0]-range[1]) > 1d-25), (position[1]-position[0])/((range[0]-range[1]) > 1d-25)]
       scale[1] = -scale[1]
    ENDELSE
    
    RETURN, scale
    
END
;-------------------------------------------------------------------------
