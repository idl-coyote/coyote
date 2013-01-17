;+
; The initialization module for the cgImageGroup object creates a specific
; instance of the object.
; 
; :Params:
;    image: in, required, type=varies
;       A 2D or true-color image variable to display and interact with. Optionally,
;       this variable may also be the name of an image file that IDL can open with
;       READ_IMAGE.
;  
; :Keywords:
;     filename: in, optional, type=string
;        The name of an IDL image file that IDL can read with READ_IMAGE.
;     reverse: in, optional, type=boolean, default=0
;        Set this keyword to reverse the image in the Y direction before display.
;-
FUNCTION cgImageGroup::Init, image, FILENAME=filename, REVERSE=reverse

   Compile_Opt idl2
   
   Catch, theError
   IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       RETURN,1
   ENDIF

   ; Required parameter.
   IF N_Elements(image) EQ 0 THEN Message, 'Must supply either an image or the name of an image file.'
   
   ; Is the parameter a string?
   IF Size(image, /TNAME) EQ 'STRING' THEN BEGIN
       filename = image
       Undefine, image
       ok = Query_Image(filename, info)
       IF ~ok THEN Message, 'Image file cannot be read with READ_IMAGE.'
       channels = info.channels
       dims = info.dimensions
       has_palette = info.has_palette
       image_index = info.image_index
       dataType = info.pixel_type
       fileType = info.type
       
       ; Only certain types of data files are allowed.
       validDataTypes = [1,2,3,4,5,12,13,14,15]
       void = Where(validDataTypes EQ dataType, count)
       IF count EQ 0 THEN Message, 'Image file does not contain a valid IDL data type.'
       
       
       
       image = Read_Image(filename, r, g, b, IMAGE_INDEX=image_index)
       IF StrUpCase(fileType) EQ 'TIFF' THEN BEGIN
           dims = Image_Dimensions(image, YINDEX=yindex)
           image = Reverse(image, yindex)
       ENDIF
       
       ; Load the color table, if you have color vectors.
       IF N_Elements(r) NE 0 THEN TVLCT, r, g, b
   ENDIF
   
   original = image
   range = Max(original) - Min(original)
   IF range LE 256 THEN image = BytScl(original) ELSE image = ClipScl(original)
   
   ; Get the dimensions of the image.
   dims = Image_Dimensions(image, YINDEX=yindex, XINDEX=xindex, TRUEINDEX=trueIndex, $
      ALPHACHANNEL=alphaChannel, XSIZE=xsize, YSIZE=ysize)
   
   ; Need to reverse the image before display?
   IF Keyword_Set(reverse) THEN image = Reverse(image, yindex)
   
   ; Save the color table palette.
   TVLCT, palette, /Get
   
   ; Create the widgets for the display.
   imgAspect = Float(ysize) / xsize
   
   ; Calculate the size of the full-size draw widget.
   fullSize = 200 < xsize < ysize
   IF imgAspect LE 1.0 THEN BEGIN
       full_ysize = 250 * imgAspect
       full_xsize = 250 
   ENDIF ELSE BEGIN
       full_xsize = 250 / imgAspect
       full_ysize = 250
   ENDELSE
   tlb_full = Widget_Base(Title='Scroll Image', TLB_Size_Events=1, $
      UNAME='TLB_FULL_EVENTS', UVALUE=self)
   win_full = Obj_New('cgCmdWindow', tlb_full, WXSize=full_xsize, WYSize=full_ysize)
   win_full -> AddCommand, win_full -> PackageCommand('cgImage', image)
   
   ; Calculate the size of the main draw widget.
   main_xsize = 400 < xsize
   main_ysize = 400 < ysize
   tlb_main = Widget_Base(Title='1:1 Image', TLB_Size_Events=1, $
      UNAME='TLB_MAIN_EVENTS', UVALUE=self, XOFFSET=25, YOFFSET=25)
   win_main = Obj_New('cgCmdWindow', tlb_main, WXSize=main_xsize, WYSize=main_ysize)
   win_main -> AddCommand, win_full -> PackageCommand('cgImage', image[0:main_xsize-1,0:main_ysize-1])
   
   ; Calculate the size of the main draw widget.
   zoom_xsize = 200 < xsize
   zoom_ysize = 200 < ysize
   tlb_zoom = Widget_Base(Title='Zoom', TLB_Size_Events=1, $
      UNAME='TLB_ZOOM_EVENTS', UVALUE=self)
   win_zoom = Obj_New('cgCmdWindow', tlb_zoom, WXSize=zoom_xsize, WYSize=zoom_ysize)
   win_zoom -> AddCommand, win_full -> PackageCommand('cgImage', image[0:zoom_xsize/4-1,0:zoom_ysize/4-1])

   Widget_Control, tlb_main, /Realize
   geo = Widget_Info(tlb_main, /Geometry)
   Widget_Control, tlb_full, $
      XOFFSET=geo.xoffset, $
      YOFFSET=geo.yoffset + geo.ysize + 40
   Widget_Control, tlb_full, /Realize, Group_Leader=tlb_main
   geo1 = Widget_Info(tlb_full, /Geometry)
   
   Widget_Control, tlb_zoom, $
      XOFFSET=geo1.xoffset + geo1.xsize + 20, $
      YOFFSET=geo.yoffset + geo.ysize + 40
   Widget_Control, tlb_zoom, /Realize, Group_Leader=tlb_main
   
   RETURN, 1
END 

;+
; The class definition module for the cgImageGroup object.
;-
PRO cgImageGroup__Define, class

   class = { cgIMAGEGROUP, $
             INHERITS IDL_OBJECT, $
             palette: BytArr(256,3), $
             image: Ptr_New(), $
             tlb_full: 0L, $
             tlb_mail: 0L, $
             tlb_zoom: 0L, $
             wid_full: 0L, $
             wid_main: 0L, $
             wid_zoom: 0L, $
             win_full: Obj_New(), $
             win_main: Obj_New(), $
             win_zoom: Obj_New(), $
             zoom_factor: 0L }
END