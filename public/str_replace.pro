;+
; Handle string replacment with regular expressions.
;
; @examples The following example demonstrate basic operations of STR_REPLACE:<pre>
; <b>IDL></b> print, str_replace('Mike was here', 'was', 'was not')
; Mike was not here
; <b>IDL></b> print, str_replace('Mike was here', '([^ ]*) ([^ ]*)', '$2 $1')
; was Mike here
; <b>IDL></b> print, str_replace('MikeGeorgeHenryMikeBill', 'Mike([A-Z][a-z]*)', '"Mike"+strupcase("$1")', /evaluate, /global)
; MikeGEORGEHenryMikeBILL
; <b>IDL></b> print, str_replace('Mike5', 'Mike([0-9]+)', 'strtrim(fix($1) * 2, 2)', /evaluate)
; 10
; <b>IDL></b> str = '187438273587285'
; <b>IDL></b> for i = 0, strlen(str)/3 - 1 do str = str_replace(str, '^[+-]?([[:digit:]]+)([[:digit:]]{3})', '$1,$2', /global)
; <b>IDL></b> print, str
; 187,438,273,587,285</pre>
; @returns string
; @param str {in}{required}{type=string} a string to search for expressions
;            and replace them
; @param pattern {in}{required}{type=string} a regular expression possibly
;        using subexpressions; see IDL's online help for STREGEX for more
;        help on regular expressions
; @param replacement {in}{required}{type=string} the string to replace matches
;        of the "pattern"; can use $1, $2, etc. to refer to subexpressions
;        in "pattern"
; @keyword evaluate {in}{optional}{type=boolean} set to evaluate the
;          "replacement" as a IDL expression instead of just a string.
; @keyword fold_case {in}{optional}{type=boolean} set to make a case
;          insensitive match with "pattern"
; @keyword global {in}{optional}{type=boolean} set to replace all expressions
;          that match
; @keyword start {out}{optional}{type=integral}{private} where to start looking
; @author Michael Galloy
; @copyright RSI, 2002
;-
function str_replace, str, pattern, replacement, evaluate=evaluate, $
    fold_case=fold_case, global=global, start=start
    compile_opt idl2
    on_error, 2

    if (n_elements(str) ne 1) then begin
        message, 'str parameter must be a scalar string'
    endif

    if (keyword_set(global)) then begin
        ans = str_replace(str, pattern, replacement, start=start, $
            fold_case=keyword_set(fold_case), evaluate=keyword_set(evaluate))

        while (start lt strlen(ans)) do begin
            temp = strmid(ans, 0, start)
            ans =  temp $
                + str_replace(strmid(ans, start), pattern, replacement, start=start, $
                    fold_case=keyword_set(fold_case), evaluate=keyword_set(evaluate))
            start = strlen(temp) + start
        endwhile

        return, ans
    endif

    pos = stregex(str, pattern, length=len, /subexpr, $
        fold_case=keyword_set(fold_case))

    ; pattern not found
    if (pos[0] eq -1) then begin
        start = strlen(str)
        return, str
    endif

    pre = pos[0] eq 0 ? '' : strmid(str, 0, pos[0])
    post = pos[0] + len[0] ge strlen(str) ? '' : strmid(str, pos[0] + len[0])

    ; $& -> pos[0], len[0]
    ; $1 -> pos[1], len[1]
    ; $2 -> pos[2], len[2]
    ; etc...
    rpos = strsplit(replacement, '$', escape='\', length=rlen)
    static_replacement = ''
    if ((n_elements(rlen) ne 1) or (rlen[0] ne 0)) then begin
        for i = 0, n_elements(rpos) - 1 do begin
            if (rpos[i] ne 0) then begin
                part = strmid(replacement, rpos[i], rlen[i])
                ppos = stregex(part, '^[0-9]+|^&', length=plen)
                if (ppos[0] eq -1) then $
                    message, 'illegal $, use \ to escape'

                match = strmid(part, ppos, plen)
                var_no = match eq '&' ? 0 : long(match)
                if (var_no ge n_elements(pos)) then begin
                    message, '$' + strtrim(var_no, 2) + ' undefined'
                endif

                var = strmid(str, pos[var_no], len[var_no])
                static_replacement $
                    = static_replacement + var + strmid(part, ppos + plen)
            endif else begin
                if (rlen[0] eq 0) then begin
                    message, 'illegal $, use \ to escape'
                endif
                static_replacement = strmid(replacement, rpos[0], rlen[0])
            endelse
        endfor
    endif

    ; call IDL if EVALUATE keyword is set
    if (keyword_set(evaluate)) then begin
        result = execute('static_replacement = ' + static_replacement)
    endif

    ret_str = pre + static_replacement + post
    start = strlen(pre) + strlen(static_replacement)

    return, ret_str
end