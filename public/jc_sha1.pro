; docformat = 'rst'
;
;+
; This program implements the US Secure Hash Algorithm 1 (SHA-1) for 
; computing a condensed representation of a message or data file. When 
; a message of any length < 2^64 bits is input, the SHA-1 produces a
; 160-bit output called a message digest.  The message digest can then,
; for example, be input to a signature algorithm which generates or
; verifies the signature for the message.  Signing the message digest
; rather than the message often improves the efficiency of the process
; because the message digest is usually much smaller in size than the
; message.  The same hash algorithm must be used by the verifier of a
; digital signature as was used by the creator of the digital
; signature.  Any change to the message in transit will, with very high
; probability, result in a different message digest, and the signature
; will fail to verify.
;
; The SHA-1 is called secure because it is computationally infeasible
; to find a message which corresponds to a given message digest, or to
; find two different messages which produce the same message digest.
; Any change to a message in transit will, with very high probability,
; result in a different message digest, and the signature will fail to
; verify.
; 
; This function has not been tested on big endian machines.
;
; :Categories:
;    Utility
;    
; :Returns:
;    The function returns a 40 character hexadecimal string called the message digest.
;
; :Examples:
; 
;    The following examples demonstrate how the program can be used::
;   
;      IDL> Print, jc_SHA1('The quick brown fox jumps over the lazy dog')
;           2fd4e1c67a2d28fced849ee1bb76e7391b93eb12
;
;      IDL> Print, jc_SHA1('')
;           da39a3ee5e6b4b0d3255bfef95601890afd80709
;
;      IDL> Print, jc_SHA1('/path/to/an/empty.file')
;           5c9eabac232b38b1e418a17fa984582f491b5b75
;
; :Author:
;    John Corriera
;
; :History:
;     Change History::
;        Written and offered to IDL newsgroup, 12 Sept 2013,
;
; :Copyright:
;     Copyright (c) 2013, John Corriera.
;-

;+
; The first of three essential SHA-1 functions. Each function operates on 
; three 32-bit words and produces one 32-bit word as output.
; 
; :Returns:
;     The function returns a single 32-bit word which is used in the SHA-1 algorithm.
; 
; :Params:
;    x: in, required
;       The first of three 32-bit words passed as input.
;    y: in, required
;       The second of three 32-bit words passed as input.
;    z: in, required
;       The third of three 32-bit words passed as input.
;-
function jc_SHAfunction1, x, y, z
    return, (x AND y) OR ((NOT x) AND z)
end

;+
; The second of three essential SHA-1 functions. Each function operates on
; three 32-bit words and produces one 32-bit word as output.
;
; :Returns:
;     The function returns a single 32-bit word which is used in the SHA-1 algorithm.
;
; :Params:
;    x: in, required
;       The first of three 32-bit words passed as input.
;    y: in, required
;       The second of three 32-bit words passed as input.
;    z: in, required
;       The third of three 32-bit words passed as input.
;-
function jc_SHAfunction2, x, y, z
    return, x XOR y XOR z
end

;+
; The third of three essential SHA-1 functions. Each function operates on
; three 32-bit words and produces one 32-bit word as output.
;
; :Returns:
;     The function returns a single 32-bit word which is used in the SHA-1 algorithm.
;
; :Params:
;    x: in, required
;       The first of three 32-bit words passed as input.
;    y: in, required
;       The second of three 32-bit words passed as input.
;    z: in, required
;       The third of three 32-bit words passed as input.
;-
function jc_SHAfunction3, x, y, z
    return, (x AND y) OR (x AND z) OR (y AND z)
end

;+
; The function implemnets the SHA-1 algorithm and returns a 40-character "message digest"
; of hexadecimal values.
;
; :Params:
;    input: in, required, type=string
;       The input string to be converted to a message digest.
;       
; :Keywords:
;    file: in, optional, type=boolean, default=0
;        Set this keyword to indicate the input is a filename. In this case, the file will
;        be examined to be sure it exists, can be read, and contains content.
;    string: in, optional, type=boolean, default=0
;        Set this keyword if you are passing a filename and want to avoid file testing.
;-
function jc_sha1, input, STRING=STRING, FILE=FILE

    COMPILE_OPT IDL2, STRICTARRSUBS
    
    isFile = file_test(input) or KEYWORD_SET(FILE)
    if KEYWORD_SET(STRING) then isFile=0
    if isFile then begin
        isZeroLength = file_test(input,/ZERO_LENGTH)
        canRead = file_test(input,/READ)
        if ~(canRead) then begin
            print, "Can't read this file"
            return, -1
        endif
        bytearr = isZeroLength ? byte('') : read_binary(input)
    endif else begin
        bytearr = byte(input)
    endelse
    
    mlen = bytearr[0] eq 0b ? 0ULL : 8ULL*N_ELEMENTS(bytearr)
    bytearr = bytearr[0] eq 0 ? 128b : [TEMPORARY(bytearr),128b]
    while (8*N_ELEMENTS(bytearr) mod 512) ne 448 do $
        bytearr = [TEMPORARY(bytearr),0b]
    bytearr = [TEMPORARY(bytearr),reverse(byte(mlen,0,8))]
    message = ulong(bytearr)
    
    h0 = '67452301'xul
    h1 = 'EFCDAB89'xul
    h2 = '98BADCFE'xul
    h3 = '10325476'xul
    h4 = 'C3D2E1F0'xul
    
    w0 = ULONARR(80)
    
    for chind=0, n_elements(message)-1, 64 do begin
    
        M = message[chind:chind+63]
        w = w0
        for i=0,15 do $
            w[i] = TOTAL(M[i*4:i*4+3]*[16777216ul,65536ul,256ul,1ul],$
            /PRESERVE_TYPE)
        temp = w
        for i=16,79 do begin
            temp = w[i-3] xor w[i-8] xor w[i-14] xor w[i-16]
            w[i] = (temp*2ul) OR (temp/2147483648ul)
        endfor
        
        a = h0
        b = h1
        c = h2
        d = h3
        e = h4
        
        for i=0, 19 do begin
            temp = ((a*32ul) OR (a/134217728ul)) + jc_SHAfunction1(b,c,d) $
                + e + 1518500249ull + w[i]
            e = d
            d = c
            c = (b*1073741824ul) OR (b/4ul)
            b = a
            a = ulong(temp)
        endfor
        for i=20, 39 do begin
            temp = ((a*32ul) OR (a/134217728ul)) + jc_SHAfunction2(b,c,d) $
                + e + 1859775393ull + w[i]
            e = d
            d = c
            c = (b*1073741824ul) OR (b/4ul)
            b = a
            a = ulong(temp)
        endfor
        for i=40, 59 do begin
            temp = ((a*32ul) OR (a/134217728ul)) + jc_SHAfunction3(b,c,d) $
                + e + 2400959708ull + w[i]
            e = d
            d = c
            c = (b*1073741824ul) OR (b/4)
            b = a
            a = ulong(temp)
        endfor
        for i=60, 79 do begin
            temp = ((a*32ul) OR (a/134217728ul)) + jc_SHAfunction2(b,c,d) $
                + e + 3395469782ull + w[i]
            e = d
            d = c
            c = (b*1073741824ul) OR (b/4ul)
            b = a
            a = ulong(temp)
        endfor
        
        h0 += a
        h1 += b
        h2 += c
        h3 += d
        h4 += e
        
    endfor
    
    h0 = string(h0,format='(z08)')
    h1 = string(h1,format='(z08)')
    h2 = string(h2,format='(z08)')
    h3 = string(h3,format='(z08)')
    h4 = string(h4,format='(z08)')
    
    return, strupcase(h0+h1+h2+h3+h4)
end