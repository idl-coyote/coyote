FUNCTION GoogleMapWidget, parent, $
    BUTTON_EVENTS=button_events, $
    CENTER_LATITUDE=centerLat, $
    CENTER_LONGITUDE=centerLon, $
    IMAGETYPE=imageType, $
    KEEP_IMAGE=keep_image, $
    EVENT_METHOD=event_method, $
    MAPTYPE=maptype, $
    MOTION_EVENTS=motion_events, $
    TEMPDIR=tempdir, $
    XSIZE=xsize, $
    YSIZE=ysize, $
    ZOOMLEVEL=zoomlevel
    
    mapObject = Obj_New('GoogleMapWidget', parent, $
        BUTTON_EVENTS=button_events, $
        CENTER_LATITUDE=centerLat, $
        CENTER_LONGITUDE=centerLon, $
        IMAGETYPE=imageType, $
        KEEP_IMAGE=keep_image, $
        EVENT_METHOD=event_method, $
        MAPTYPE=maptype, $
        MOTION_EVENTS=motion_events, $
        TEMPDIR=tempdir, $
        XSIZE=xsize, $
        YSIZE=ysize, $
        ZOOMLEVEL=zoomlevel)
            
    IF Obj_Valid(mapObject) THEN RETURN, mapObject ELSE RETURN, -1
    
END
