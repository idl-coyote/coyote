; docformat = 'rst'
;
; NAME:
;   cgLineIntersect
;
; PURPOSE:
;   This function returns the intersection of two line segments, represented by four points.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2014, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; This function returns the intersection of two line segments, represented by four points.
;
; :Categories:
;    Utilities
;
; :Params:
;    x0: in, required
;         The x location of the one end of the first line segment.
;    y0: in, required
;         The y location of the one end of the first line segment.
;    x1: in, required
;         The x location of the other end of the first line segment.
;    y1: in, required
;         The y location of the other end of the first line segment.
;    x2: in, required
;         The x location of the one end of the second line segment.
;    y2: in, required
;         The y location of the one end of the second line segment.
;    x3: in, required
;         The x location of the other end of the second line segment.
;    y3: in, required
;         The y location of the other end of the second line segment.
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
;        Written, 10 September 2014 on suggestion of Wang Kang.
;
; :Copyright:
;     Copyright (c) 2014, Fanning Software Consulting, Inc.
;-
FUNCTION cgLineIntersect, x0, y0, x1, y1, x2, y2, x3, y3

    param1 = LINFIT([x0, x1], [y0, y1])
    param2 = LINFIT([x3, x2], [y3, y2])
    
    x = [[param1[1]*(-1.), 1.],[param2[1]*(-1.), 1.]]
    b = [[param1[0]],[param2[0]]]
    
    intersection = b # Invert(x)
    
    RETURN, intersection
    
END
