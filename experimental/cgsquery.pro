FUNCTION cgsQuery, $
    COUNT=count, $
    CURRENT=current, $
    DIMENSIONS=dimensions, $
    OBJECTREF=objectRef, $
    TITLE=title, $
    WIDGETID=widgetID

    ; Are there cgsWindow applications around?
    DefSysV, '!CGS_WINDOW_LIST', EXISTS=exists
    
    ; If it doesn't exist, or it is invalid, leave.
    IF ~exists THEN BEGIN
        count = 0
        RETURN, Obj_New()
    ENDIF ELSE BEGIN
        IF ~Obj_Valid(!CGS_WINDOW_LIST) THEN BEGIN
            count = 0
            RETURN, Obj_New()
        ENDIF   
    ENDELSE
    
    ; Get the window list and find out how many windows there are.
    list = !CGS_WINDOW_LIST
    count = list -> Get_Count()
    
    ; Make arrays to hold the values.
    widgetID = LonArr(count)
    objectRef = ObjArr(count)
    title = StrArr(count)
    windowIndex = IntArr(count)
    dimensions = IntArr(2, count)
    
    ; Fill them up.
    thisWindow = !D.Window
    FOR j=0,count-1 DO BEGIN
        thisItem = list -> Get_Item(j, /DEREFERENCE)
        widgetID[j] = thisItem.tlb
        objectRef[j] = thisItem.windowobj
        title[j] = thisItem.title
        windowIndex[j] = thisItem.wid
        WSet, windowIndex[j]
        dimensions[*,j] = [!D.X_Size, !D.Y_Size]
    ENDFOR
    IF (thisWindow GE 0) && WindowAvailable(thisWindow) THEN WSet, thisWindow ELSE WSet, -1
    
    ; Return just the current values if the CURRENT keyword is set.
    IF Keyword_Set(current) THEN BEGIN
        IF (count-1) GE 0 THEN BEGIN
            windowIndex = windowIndex[count-1]
            widgetID = widgetID[count-1]
            objectRef = objectRef[count-1]
            title = title[count-1] 
            dimensions = dimensions[*,count-1]
        ENDIF ELSE BEGIN
            windowIndex = -1
            widgetID = -1
            objectRef = Obj_New()
            title = ""
            dimensions = [0,0]
        ENDELSE
    ENDIF

    ; Make sure scalar is returned if just one element.
    IF count EQ 1 THEN BEGIN
        widgetID = widgetID[0]
        objectRef = objectRef[0]
        title = title[0]
        windowIndex = windowIndex[0]
        dimensions = dimensions[*,0]
    ENDIF
    
    
    RETURN, objectRef
    
END