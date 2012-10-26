; docformat = 'rst'
;
; NAME:
;   cgRandomWald
;
; PURPOSE:
; 
;   This function creates a vector of N random numbers using an Inverse Gaussian Distribution,
;   which is also known as the Wald Distribution. The reference for the code can be found at 
;   http://en.wikipedia.org/wiki/Inverse_Gaussian_distribution.
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
; This function creates a vector of N random numbers using an Inverse Gaussian Distribution,
; which is also known as the Wald Distribution. The reference for the code can be found at 
; http://en.wikipedia.org/wiki/Inverse_Gaussian_distribution.
;
; :Categories:
;    Utility
;    
; :Params:
;    seed: in, optional
;       The seed for the random number generator. If undefined, on output will
;       have the seed used by the IDL RandomU function. See the documentation
;       for RandomU for additional information.
;    dims: in, optional, type=integer
;       A scalar or integer array defining the dimensions of the result. If no dimensions 
;       are specified, a single random number is returned. If `Dims` is a scalar, a 1D vector 
;       of that number of values will be returned.
;       
; :Keywords:
;     mu: in, optional, type=double, default=1.0
;        The mean of the distribution.
;     lambda: in, optional, type=double, default=1.0
;        The shape parameter of the distribution. As lambda tends to infinity, the inverse
;        distribution becomes more like a normal distribution.
;     
; :Examples:
;    For example, to create 100 random numbers, using the Wald distribution::
;       wald = cgRandomWald(seed, 100)
;    To create a 20-column by 10-row array of random numbers::
;       wald = cgRandoWald(seed, [20,10])
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
;     Written, 25 Oct 2012, by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgRandomWald, seed, dims, MU=mu, LAMBDA=lambda

    Compile_Opt idl2
    
    ; Return to the caller on an error.
    On_Error, 2
    
    ; Handle the number of input parameters appropriately.
    CASE N_Params() OF
       0: dims = 1
       1: dims = 1
       2: 
    ENDCASE
    
    ; Check for keywords.
    IF N_Elements(mu) EQ 0 THEN mu = 1D ELSE mu = Double(mu)
    IF N_Elements(lambda) EQ 0 THEN lambda = 1D ELSE lambda = Double(lambda)

    ; Define parameters for the inverse Gaussian distribution.
    nu = RandomN(seed, dims)
    z =  RandomU(seed, dims)
    randomNumbers = DblArr(dims)
    
    ; Implement the algorithm given in the Wikipedia reference above.
    y = nu^2
    x = mu + mu^2.0*y/(2.0*lambda) - mu/2.0/lambda*SQRT(4.0*mu*lambda*y + mu^2*y^2)
    indices = Where(z LE mu/(mu+x), Complement=cindices, indicesCnt, NComplement=cindicesCnt)
    IF indicesCnt GT 0 THEN  randomNumbers[indices] =  x[indices]
    IF cindicesCnt GT 0 THEN randomNumbers[cindices] = (mu^2/x)[cindices]
    
    RETURN, randomNumbers

END