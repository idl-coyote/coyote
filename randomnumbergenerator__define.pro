;+
; NAME:
;       RANDOMNUMBERGENERATOR
;
; PURPOSE:
;
;       Allows the user to obtain a sequence of pseudo-random numbers. The
;       object maintains the random number generator seed in such a way that
;       subsequent calls to GetRandomNumbers will guarentee that you don't 
;       get the same random numbers each time you ask for random numbers.
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

;       Utilities
;
; CALLING SEQUENCE:
; 
;       Generate three random numbers.
;
;       IDL> rng = Obj_New('RandomNumberGenerator', initialSeed)
;       IDL> numberOfNumbersNeeded = 3
;       IDL> randomNumbers = rng -> GetRandomNumbers(numberOfNumbersNeeded)
;       IDL> Print, randomNumbers
;             0.80952855      0.35878432      0.52150406
;             
;       Generate a sequence of 8 random digits.
;       IDL> Print, rng -> GetRandomDigits(8)
;             21855786
;
; INPUT PARAMETERS FOR INIT METHOD:
;
;       initialSeed:    The initial seed for the random number generator. If undefined
;                       or absent, the number of seconds after 1 January 1970 is used.
;                       
; METHODS:
;
;       randomNumbers -> GetRandomNumbers(d1, d2, d3, d4 d5, d6, d7, d8)
;       randomDigits = obj -> GetRandomDigits(numDigets)
;       obj -> SetSeed, seed
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 13 November 2009.
;       Added GetRandomDigits method. 7 February 2010. DWF.
;       Incorrect cleanup of the seed pointer fixed in the CLEANUP procedure. 
;           25 February 2010, DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2009-2010, by Fanning Software Consulting, Inc.                           ;
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
;------------------------------------------------------------------------------------------;
; NAME:                                                                                
;    RandomNumberGenerator::GetRandomDigits                                                                   
;                                                                                      
; Purpose:                                                                             
;                                                                                      
;    Allows the user to obtain a string variable of random digits.                                                               
;                                                                                      
; Method Syntax:                                                                       
;                                                                                      
;    randomDigits = obj -> GetRandomDigits(numberOfDigits)                             
;                                                                                      
; Auguments:                                                                           
;                                                                                      
;    numberOfDigits:   The number of digits desired. (Input, default is 8. )           
;                                                                                      
; Keywords:                                                                            
;                                                                                      
;    BYTES:             If this keyword is set, the random digits are returned
;                       as a byte array, rather than as a string.                                                                             
;                                                                                      
; Return Value:                                                                        
;                                                                                      
;    randomDigits:      A string variable containing the random digits.                
;                                                                                      
;------------------------------------------------------------------------------------------;
FUNCTION RandomNumberGenerator::GetRandomDigits, digits, BYTES=bytes

    COMPILE_OPT idl2

    On_Error, 2
    
    ; Default is 8 digits.
    IF N_Elements(digits) EQ 0 THEN digits = 8
    
    ; How many random numbers are needed to construct this number of digits?
    numbersNeeded = Long( digits/16 ) + 1
    
    ; Get the numbers and use only the digits after the "0.".
    randomNumbers = self -> GetRandomNumbers(numbersNeeded, /DOUBLE)
    strNumbers = StrMid(String(randomNumbers, FORMAT='(F18.16)'), 2)
    
    ; Put the digits together to come up with the correct number.
    digitNumbers = strNumbers[0]
    FOR j=1,N_Elements(randomNumbers)-1 DO digitNumbers = digitNumbers + strNumbers[j]
    
    ; Trim the result to the correct number of digits.
    IF StrLen(digitNumbers) GT digits THEN digitNumbers = StrMid(digitNumbers,0,digits)
    
    ; Do you need bytes instead of a string?
    IF Keyword_Set(bytes) THEN BEGIN
       numbers = BytArr(digits)
       FOR j=0,digits-1 DO BEGIN
            numbers[j] = Byte(Fix(StrMid(digitNumbers, j, 1)))
       ENDFOR
       digitNumbers = numbers
    ENDIF
    
    ;Return the digits you need.
    RETURN, digitNumbers
    
END
    
    
;------------------------------------------------------------------------------------------;
; NAME:                                                                                
;    RandomNumberGenerator::GetRandomNumbers                                                                  
;                                                                                      
; Purpose:                                                                             
;                                                                                      
;    Allows the user to obtain a scalar or array of random numbers.                                                              
;                                                                                      
; Method Syntax:                                                                       
;                                                                                      
;    randomNumbers = obj -> GetRandomNumbers(d1, d2, d3, d4, d5, d6, d7, d8)           
;                                                                                      
; Auguments:                                                                           
;                                                                                      
;    d1...d8         Up to 8 dimension values can be specified, as in the IDL on-line  
;                    documentation for RANDOMU.                                        
; Keywords:                                                                            
;                                                                                      
;        All the keywords that are defined for the IDL built-in routine RANDOMU are    
;        available for use here. The only difference is that non-integer numbers are   
;        *always* returned as double-precision values. That is to say DOUBLE is always 
;        set to 1. See the IDL on-line documentation for RANDOMU to understand how to use 
;        the following keywords:                                                       
;                BINOMIAL                                                              
;                DOUBLE                                                                
;                GAMMA                                                                 
;                NORMAL                                                                
;                POISSON                                                               
;                UNIFORM                                                               
;                LONG                                                                  
;                                                                                      
; Return Value:                                                                        
;                                                                                      
;    randomNumbers:      The scalar or array of random numbers.                        
;                                                                                      
;------------------------------------------------------------------------------------------;
FUNCTION RandomNumberGenerator::GetRandomNumbers, d1, d2, d3, d4, d5, d6, d7, d8, $
    BINOMIAL=binomial, $
    DOUBLE=double, $ 
    GAMMA=gamma, $
    NORMAL=normal, $
    POISSON=poisson, $
    UNIFORM=uniform, $
    LONG=long
    
    ; I just assume any floating value is *suppose* to be in double precision.
    IF N_Elements(double) EQ 0 THEN double = 1
    
    ; Yuck! Have to do this according to the number of positional parameters.
    CASE N_Params() OF
        0: results = RandomU(*self.seed, $
                BINOMIAL=binomial, $
                DOUBLE=double, $ 
                GAMMA=gamma, $
                NORMAL=normal, $
                POISSON=poisson, $
                UNIFORM=uniform, $
                LONG=long)
        1: results = RandomU(*self.seed, d1, $
                BINOMIAL=binomial, $
                DOUBLE=double, $ 
                GAMMA=gamma, $
                NORMAL=normal, $
                POISSON=poisson, $
                UNIFORM=uniform, $
                LONG=long)
        2: results = RandomU(*self.seed, d1, d2, $
                BINOMIAL=binomial, $
                DOUBLE=double, $ 
                GAMMA=gamma, $
                NORMAL=normal, $
                POISSON=poisson, $
                UNIFORM=uniform, $
                LONG=long)
        3: results = RandomU(*self.seed, d1, d2, d3, $
                BINOMIAL=binomial, $
                DOUBLE=double, $ 
                GAMMA=gamma, $
                NORMAL=normal, $
                POISSON=poisson, $
                UNIFORM=uniform, $
                LONG=long)
        4: results = RandomU(*self.seed, d1, d2, d3, d4, $
                BINOMIAL=binomial, $
                DOUBLE=double, $ 
                GAMMA=gamma, $
                NORMAL=normal, $
                POISSON=poisson, $
                UNIFORM=uniform, $
                LONG=long)
        5: results = RandomU(*self.seed, d1, d2, d3, d4, d5, $
                BINOMIAL=binomial, $
                DOUBLE=double, $ 
                GAMMA=gamma, $
                NORMAL=normal, $
                POISSON=poisson, $
                UNIFORM=uniform, $
                LONG=long)
        6: results = RandomU(*self.seed, d1, d2, d3, d4, d5, d6, $
                BINOMIAL=binomial, $
                DOUBLE=double, $ 
                GAMMA=gamma, $
                NORMAL=normal, $
                POISSON=poisson, $
                UNIFORM=uniform, $
                LONG=long)
        7: results = RandomU(*self.seed, d1, d2, d3, d4, d5, d6, d7, $
                BINOMIAL=binomial, $
                DOUBLE=double, $ 
                GAMMA=gamma, $
                NORMAL=normal, $
                POISSON=poisson, $
                UNIFORM=uniform, $
                LONG=long)
        8: results = RandomU(*self.seed, d1, d2, d3, d4, d5, d6, d7, d8, $
                BINOMIAL=binomial, $
                DOUBLE=double, $ 
                GAMMA=gamma, $
                NORMAL=normal, $
                POISSON=poisson, $
                UNIFORM=uniform, $
                LONG=long)
    ENDCASE
       
    ; Return the results.
    RETURN, results
    
END ;----------------------------------------------------------------



PRO RandomNumberGenerator::SetSeed, theSeed

    ; Sets the seed for the random number generator.

    IF N_Elements(theSeed) NE 0 THEN *self.seed = theSeed
    
 END ;----------------------------------------------------------------

   

PRO RandomNumberGenerator::CLEANUP

    Ptr_Free, self.seed

END ;----------------------------------------------------------------


FUNCTION RandomNumberGenerator::INIT, initialSeed

    ; Initializes the random number generator. The initialSeed is the initial
    ; seed for the generator. If absent, the number of seconds after 1 January 1970 
    ; is used as the seed.

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        Help, /Last_Message
        Return, 0
    ENDIF
    
    ; Initialize the pointer.
    self.seed = Ptr_New(/ALLOCATE_HEAP)

    ; Create the initial seed, if needed. Otherwise, just store it.
    IF N_Elements(initialSeed) EQ 0 $
        THEN self -> SetSeed, Systime(1) $
        ELSE self -> SetSeed, initialSeed
        
    RETURN, 1
    
END ;----------------------------------------------------------------


PRO RandomNumberGenerator__Define

    class = { RANDOMNUMBERGENERATOR, $
              seed: Ptr_New() }
END ;----------------------------------------------------------------
