FUNCTION MS_FileTime, fileTime, AS_STRING=as_string

    ; If Microsoft filetime is not passed in, make one up with the current
    ; date. Filetime is 64-bit value containing the number of 100-nanosecond
    ; intervals since 1 January 1601.

    IF N_Elements(fileTime) EQ 0 THEN BEGIN
    
        date = Systime(1) ; Number of seconds elapsed since 1 Jan 1970.
        Print, 'Using current time example" ', Systime(0, /UTC)
        seconds_per_day = 60.D * 60.D *24.D
        days_since_1970 = date / seconds_per_day
        
        ; Since Systime only goes back to 1970, we have to add an offset to
        ; get all the way back to 1601. Add 1 to the day offset because you are
        ; counting the days.
        day_offset = Julday(1, 1, 1970, 0, 0, 0) - Julday(1, 1, 1601, 0, 0, 0) + 1 
        second_offset = day_offset * seconds_per_day
        number_of_seconds_since_1601 = date + second_offset
        
        ; Filetime is the number of 100 nanoseconds *intervals*!
        fileTime = ULONG64(number_of_seconds_since_1601 / (1d-9 * 100)) 
    
    ENDIF

    ; Convert fileTime (in 100 nanosecond intervals) to seconds.
    sfileTime = fileTime * (1d-7)
    
    ; Calculate days, hours, minutes, and seconds since 1 Jan 1601.
    seconds_per_day = 60.D * 60.D *24.D
    days_since_1601 = sfileTime / seconds_per_day
    hour = (days_since_1601 - Long(days_since_1601)) * 24.d
    minute = (hour - Long(hour)) * 60.d
    seconds = (minute - Long(minute)) * 60.d
    
    ; Find the month, day, and year. Use the "naive" meaning of JULDAY
    ; as the day starting at mid-night and not noon.
    CalDat, Julday(1, days_since_1601, 1601), month, day, year
    
    ; Create a time structure to return.
    timeStruct = { year: year, $
                   month: month, $
                   day: day, $
                   hour: Long(hour), $
                   minute: Long(minute), $
                   seconds: seconds }
                   
    ; Return the time as a string?
    IF Keyword_Set(as_string) THEN BEGIN
        dateStr = StrTrim(day,2) + " " + TheMonths(month, /ABBREVIATION) + " " + StrTrim(year,2)
        dateStr = dateStr + " " + StrTrim(Long(hour),2) + ":" + StrTrim(Long(minute),2) + ":" 
        dataStr = dateStr + StrTrim(Round(seconds),2)
        timeStruct = dataStr
    ENDIF
    
    RETURN, timeStruct
    
END


   ; Another example. Construct a fileTime for 23 August 1981 at 09:35:23 UTC.
   numDays = Julday(8, 23, 1981, 9, 35, 23) - Julday(1, 1, 1601, 0, 0, 0) + 1
   numSecs = numDays * 24D * 60D * 60D
   fileTime = ULONG64(numSecs / 10d-7)
   
   Print, MS_FileTime(fileTime, /AS_STRING)
END
