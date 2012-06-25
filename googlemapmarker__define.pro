PRO GoogleMapMarker__Define

    struct = { GOOGLEMAPMARKER, $
               size: "", $         ; The marker size ("tiny", "small", "mid" or "normal")
               color: "", $        ; A color name as provided with cgColor.
               label: "", $        ; A single uppercase character label from the set {A-Z,0-9}.
               lats: Ptr_New(), $  ; A pointer to one or more latitude values.
               lons: Ptr_New() $   ; A pointer to one or more longitude values.
             }
END