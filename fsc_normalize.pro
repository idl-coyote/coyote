;+
; NAME:
;       FSC_NORMALIZE
;
; PURPOSE:
;
;       This is a utility routine to calculate the scaling vector
;       required to position a graphics primitive of specified range
;       at a specific position in an arbitray coordinate system. The
;       scaling vector is given as a two-element array like this:
;
;          scalingVector = [translationFactor, scalingFactor]
;
;       The scaling vector should be used with the [XYZ]COORD_CONV
;       keywords of a graphics object or model. For example, if you
;       wanted to scale an X axis into the coordinate range of -0.5 to 0.5,
;       you might type something like this:
;
;          xAxis->GetProperty, Range=xRange
;          xScale = FSC_Normalize(xRange, Position=[-0.5, 0.5])
;          xAxis, XCoord_Conv=xScale
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:

;       Object Graphics
;
; CALLING SEQUENCE:
; 
;       xscaling = FSC_NORMALIZE(xrange, POSITION=position)
;
; INPUTS:
; 
;       XRANGE: A two-element vector specifying the data range.
;
; KEYWORD PARAMETERS:
; 
;       POSITION: A two-element vector specifying the location
;       in the coordinate system you are scaling into. The vector [0,1]
;       is used by default if POSITION is not specified.
;
; COMMON BLOCKS:
; 
;       None.
;
; EXAMPLE:
; 
;       See above.
;
; MODIFICATION HISTORY:
;       Written by:  David W. Fanning, OCT 1997.
;       Fixed a problem with illegal divide by zero. 21 April 2005. DWF.
;       Fixed a problem when range[0] is greater than range[1]. 11 July 2006. DWF.
;       Renamed to FSC_Normalize to avoid conflicts with 10,000 other programs named NORMALIZE. 17 October 2008. DWF.
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

FUNCTION FSC_Normalize, range, Position=position

On_Error, 1
IF N_Params() EQ 0 THEN Message, 'Please pass range vector as argument.'

IF (N_Elements(position) EQ 0) THEN position = [0.0D, 1.0D] ELSE $
    position=Double(position)
range = Double(range)

IF range[1] GE range[0] THEN BEGIN
   scale = [((position[0]*range[1])-(position[1]*range[0])) / $
       ((range[1]-range[0]) > 1e-12), (position[1]-position[0])/((range[1]-range[0]) > 1e-12)]
ENDIF ELSE BEGIN
   scale = [((position[1]*range[0])-(position[0]*range[1])) / $
       ((range[0]-range[1]) > 1e-12), (position[1]-position[0])/((range[0]-range[1]) > 1e-12)]
   scale[1] = -scale[1]
ENDELSE
RETURN, scale
END
;-------------------------------------------------------------------------
