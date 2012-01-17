; docformat = 'rst'
;
; NAME:
;   cgLogGen
;
; PURPOSE:
;   This function creates an evenly spaced vector of points in log space.
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
; This function creates an evenly spaced vector of points in log space.
;
; :Categories:
;    Math, Utilities
;    
; :Returns:
;    A vector of values, evenly spaced along a log axis.
;    
; :Params:
;    numpts: in, optional, type=integer, default=10
;       The number of points desired in the output vector.
;       
; :Keywords:
;     double: in, optional, type=boolean, default=0
;         Set this keyword to return the values as double precision values.
;         Otherwise, floating point values are returned.
;     finish: in, optional, type=float/double, default=100
;         The ending value of the vector. The output values are evenly spaced
;         between `Start` and `Finish`. This value must be a positive number.
;     start: in, optional, type=float/double, default=1
;         The starting value of the vector. The output values are evenly spaced
;         between `Start` and `Finish`. This value must be a positive number.
;          
; :Examples:
;    To create 10 values evenly spaced between 5 and 15::
;       points = cgLogGen(10, Start=5, Finish=150)
;       cgPlot, points, PSym=2, /YLog
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;        David W. Fanning 
;        1645 Sheely Drive
;        Fort Collins, CO 80526 USA
;        Phone: 970-221-0438
;        E-mail: david@idlcoyote.com
;        Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 17 January 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgLogGen, numpts, $
    Double=double, $
    Finish=finish, $
    Start=start
    
    Compile_Opt idl2
    
    ; Error handling. Return to caller.
    On_Error, 2
    
    ; Check parameters.
    SetDefaultValue, numpts, 10
    SetDefaultValue, finish, 100
    SetDefaultValue, start, 1
    SetDefaultValue, double, 0, /Boolean
    
    ; Make sure values are positive.
    IF start LE 0 THEN Message, 'The starting value for the vector must be a positive value.'
    IF finish LE 0 THEN Message, 'The ending value for the vector must be a positive value.'
    
    ; Compute the logarithms of the end points.
    s = ALog10(start)
    f = ALog10(finish)
    
    ; Create a vector of exponent values between these two end points.
    exponents = Scale_Vector(Findgen(numpts), s, f, Double=double)
    
    ; Return the actual values.
    RETURN, 10.^exponents
    
END