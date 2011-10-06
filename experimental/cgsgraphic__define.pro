FUNCTION cgsGraphic::GetKeywords
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        void = Error_Message()
        RETURN, {background:'white', color:'black'}
    ENDIF
    
    keywordStruct = { $
        BACKGROUND: self.background, $
        CHARSIZE: self.charsize, $
        CHARTHICK: self.charthick, $
        COLOR: self.color, $
        DATA: self.data, $
        DEVICE: self.device, $
        NORMAL: self.normal, $
        LINESTYLE: self.linestyle, $
        NOCLIP: self.noclip, $
        NODATA: self.nodata, $
        NOERASE: self.noerase, $
        PSYM: self.psym, $
        SUBTITLE: self.subtitle, $
        SYMSIZE: self.symsize, $
        T3D: self.t3d, $
        THICK: self.thick, $
        TICKLEN: self.ticklen, $
        TITLE: self.title, $
        
        XCHARSIZE: self.xcharsize, $
        XGRIDSTYLE: self.xgridstyle, $
        XMARGIN: self.xmargin, $
        XMINOR: self.xminor, $
        XRANGE: self.xrange, $
        XSTYLE: self.xstyle, $
        XTHICK: self.xthick, $
        XTICKFORMAT: self.xtickformat, $
        XTICKINTERVAL: self.xtickinterval, $
        XTICKLAYOUT: self.xticklayout, $
        XTICKLEN: self.xticklen, $
        XTICKS: self.xticks, $
        XTICKUNITS: self.xtickunits, $
        XTITLE: self.xtitle, $
    
        YCHARSIZE: self.ycharsize, $
        YGRIDSTYLE: self.ygridstyle, $
        YMARGIN: self.ymargin, $
        YMINOR: self.yminor, $
        YRANGE: self.yrange, $
        YSTYLE: self.ystyle, $
        YTHICK: self.ythick, $
        YTICKFORMAT: self.ytickformat, $
        YTICKINTERVAL: self.ytickinterval, $
        YTICKLAYOUT: self.yticklayout, $
        YTICKLEN: self.yticklen, $
        YTICKS: self.yticks, $
        YTICKUNITS: self.ytickunits, $
        YTITLE: self.ytitle, $
       
        ZCHARSIZE: self.zcharsize, $
        ZGRIDSTYLE: self.zgridstyle, $
        ZMARGIN: self.zmargin, $
        ZMINOR: self.zminor, $
        ZRANGE: self.zrange, $
        ZSTYLE: self.zstyle, $
        ZTHICK: self.zthick, $
        ZTICKFORMAT: self.ztickformat, $
        ZTICKINTERVAL: self.ztickinterval, $
        ZTICKLAYOUT: self.zticklayout, $
        ZTICKLEN: self.zticklen, $
        ZTICKS: self.zticks, $
        ZTICKUNITS: self.ztickunits, $
        ZTITLE: self.ztitle, $
        ZVALUE: self.zvalue }
        
        IF N_Elements(*self.clip) NE 0 THEN BEGIN
            keywordStruct = Create_Struct(keywordStruct, 'clip', *self.clip)
        ENDIF
        IF N_Elements(*self.font) NE 0 THEN BEGIN
            keywordStruct = Create_Struct(keywordStruct, 'font', *self.font)
        ENDIF
        IF N_Elements(*self.position) NE 0 THEN BEGIN
            keywordStruct = Create_Struct(keywordStruct, 'position', *self.position)
        ENDIF
        IF N_Elements(*self.xtickname) NE 0 THEN BEGIN
            keywordStruct = Create_Struct(keywordStruct, 'xtickname', *self.xtickname)
        ENDIF
        IF N_Elements(*self.xtickv) NE 0 THEN BEGIN
            keywordStruct = Create_Struct(keywordStruct, 'xtickv', *self.xtickv)
        ENDIF
        IF N_Elements(*self.ytickname) NE 0 THEN BEGIN
            keywordStruct = Create_Struct(keywordStruct, 'ytickname', *self.ytickname)
        ENDIF
        IF N_Elements(*self.ytickv) NE 0 THEN BEGIN
            keywordStruct = Create_Struct(keywordStruct, 'ytickv', *self.ytickv)
        ENDIF
        IF N_Elements(*self.ztickname) NE 0 THEN BEGIN
            keywordStruct = Create_Struct(keywordStruct, 'ztickname', *self.ztickname)
        ENDIF
        IF N_Elements(*self.ztickv) NE 0 THEN BEGIN
            keywordStruct = Create_Struct(keywordStruct, 'ztickv', *self.ztickv)
        ENDIF
        
    RETURN, keywordStruct
    
END ;---------------------------------------------------------------------------


PRO cgsGraphic::GetProperty, $
    BACKGROUND=background, $
    CHARSIZE=charsize, $
    CHARTHICK=charthick, $
    CLIP=clip, $
    COLOR=color, $
    DATA=data, $
    DEVICE=device, $
    NORMAL=normal, $
    FONT=font, $
    LINESTYLE=linestyle, $
    NOCLIP=noclip, $
    NODATA=nodata, $
    NOERASE=noerase, $
    POSITION=position, $
    PSYM=psym, $
    SUBTITLE=subtitle, $
    SYMSIZE=symsize, $
    T3D=t3d, $
    THICK=thick, $
    TICKLEN=ticklen, $
    TITLE=title, $
    
    XCHARSIZE=xcharsize, $
    XGRIDSTYLE=xgridstyle, $
    XMARGIN=xmargin, $
    XMINOR=xminor, $
    XRANGE=xrange, $
    XSTYLE=xstyle, $
    XTHICK=xthick, $
    XTICK_GET=xtick_get, $         
    XTICKFORMAT=xtickformat, $
    XTICKINTERVAL=xtickinterval, $
    XTICKLAYOUT=xticklayout, $
    XTICKLEN=xticklen, $
    XTICKNAME=xtickname, $
    XTICKS=xticks, $
    XTICKUNITS=xtickunits, $
    XTICKV=xtickv, $
    XTITLE=xtitle, $
    
    YCHARSIZE=ycharsize, $
    YGRIDSTYLE=ygridstyle, $
    YMARGIN=ymargin, $
    YMINOR=yminor, $
    YRANGE=yrange, $
    YSTYLE=ystyle, $
    YTHICK=ythick, $
    YTICK_GET=ytick_get, $         
    YTICKFORMAT=ytickformat, $
    YTICKINTERVAL=ytickinterval, $
    YTICKLAYOUT=yticklayout, $
    YTICKLEN=yticklen, $
    YTICKNAME=ytickname, $
    YTICKS=yticks, $
    YTICKUNITS=ytickunits, $
    YTICKV=ytickv, $
    YTITLE=ytitle, $
   
    ZCHARSIZE=zcharsize, $
    ZGRIDSTYLE=zgridstyle, $
    ZMARGIN=zmargin, $
    ZMINOR=zminor, $
    ZRANGE=zrange, $
    ZSTYLE=zstyle, $
    ZTHICK=zthick, $
    ZTICK_GET=ztick_get, $         
    ZTICKFORMAT=ztickformat, $
    ZTICKINTERVAL=ztickinterval, $
    ZTICKLAYOUT=zticklayout, $
    ZTICKLEN=zticklen, $
    ZTICKNAME=ztickname, $
    ZTICKS=zticks, $
    ZTICKUNITS=ztickunits, $
    ZTICKV=ztickv, $
    ZTITLE=ztitle, $
    ZVALUE=zvalue, $
    _EXTRA=extra
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Get the properties of the object.
    IF Arg_Present(background) THEN background = self.background
    IF Arg_Present(charsize) THEN charsize = self.charsize 
    IF Arg_Present(charthick) THEN charthick = self.charthick 
    IF Arg_Present(clip) THEN IF N_Elements(*self.clip) NE 0 THEN clip = *self.clip 
    IF Arg_Present(color) THEN color = self.color
    IF Arg_Present(data) THEN data = self.data 
    IF Arg_Present(device) THEN device = self.device
    IF Arg_Present(normal) THEN normal = self.normal
    IF Arg_Present(font) THEN IF N_Elements(*self.font) NE 0 THEN font = *self.font
    IF Arg_Present(linestyle) THEN linestyle = self.linestyle
    IF Arg_Present(noclip) THEN noclip = self.noclip
    IF Arg_Present(nodata) THEN nodata = self.nodata 
    IF Arg_Present(noerase) THEN noerase = self.noerase
    IF Arg_Present(position) THEN IF N_Elements(*self.position) NE 0 THEN position = *self.position
    IF Arg_Present(psym) THEN psym = self.psym
    IF Arg_Present(subtitle) THEN subtitle = self.subtitle
    IF Arg_Present(symsize) THEN symsize = self.symsize
    IF Arg_Present(t3d) THEN t3d = self.t3d 
    IF Arg_Present(thick) THEN thick = self.thick
    IF Arg_Present(ticklen) THEN ticklen = self.ticklen
    IF Arg_Present(title) THEN title = self.title

    IF Arg_Present(xcharsize) THEN xcharsize = self.xcharsize
    IF Arg_Present(xgridstyle) THEN xgridstyle = self.xgridstyle
    IF Arg_Present(xmargin) THEN xmargin = self.xmargin
    IF Arg_Present(xminor) THEN xminor = self.xminor 
    IF Arg_Present(xrange) THEN xrange = self.xrange
    IF Arg_Present(xstyle) THEN xstyle = self.xstyle
    IF Arg_Present(xthick) THEN xthick = self.xthick
    IF Arg_Present(xtick_get) THEN IF N_Elements(*self.xtick_get) NE 0 THEN xtick_get = *self.xtick_get
    IF Arg_Present(xtickformat) THEN xtickformat = self.xtickformat
    IF Arg_Present(xtickinterval) THEN xtickinterval = self.xtickinterval
    IF Arg_Present(xticklayout) THEN xticklayout = self.xticklayout 
    IF Arg_Present(xticklen) THEN xticklen = self.xticklen 
    IF Arg_Present(xtickname) THEN IF N_Elements(*self.xtickname) NE 0 THEN xtickname = *self.xtickname
    IF Arg_Present(xticks) THEN xticks = self.xticks
    IF Arg_Present(xtickunits) THEN xtickunits = self.xtickunits
    IF Arg_Present(xtickv) THEN IF N_Elements(*self.xtickv) NE 0 THEN xtickv = *self.xtickv
    IF Arg_Present(xtitle) THEN xtitle = self.xtitle
    
    IF Arg_Present(ycharsize) THEN ycharsize = self.ycharsize
    IF Arg_Present(ygridstyle) THEN ygridstyle = self.ygridstyle
    IF Arg_Present(ymargin) THEN ymargin = self.ymargin
    IF Arg_Present(yminor) THEN yminor = self.yminor 
    IF Arg_Present(yrange) THEN yrange = self.yrange
    IF Arg_Present(ystyle) THEN ystyle = self.ystyle
    IF Arg_Present(ythick) THEN ythick = self.ythick
    IF Arg_Present(ytick_get) THEN IF N_Elements(*self.ytick_get) NE 0 THEN ytick_get = *self.ytick_get
    IF Arg_Present(ytickformat) THEN ytickformat = self.ytickformat
    IF Arg_Present(ytickinterval) THEN ytickinterval = self.ytickinterval
    IF Arg_Present(yticklayout) THEN yticklayout = self.yticklayout 
    IF Arg_Present(yticklen) THEN yticklen = self.yticklen 
    IF Arg_Present(ytickname) THEN IF N_Elements(*self.ytickname) NE 0 THEN ytickname = *self.ytickname
    IF Arg_Present(yticks) THEN yticks = self.yticks
    IF Arg_Present(ytickunits) THEN ytickunits = self.ytickunits
    IF Arg_Present(ytickv) THEN IF N_Elements(*self.ytickv) NE 0 THEN ytickv = *self.ytickv
    IF Arg_Present(ytitle) THEN ytitle = self.ytitle

    IF Arg_Present(zcharsize) THEN zcharsize = self.zcharsize
    IF Arg_Present(zgridstyle) THEN zgridstyle = self.zgridstyle
    IF Arg_Present(zmargin) THEN zmargin = self.zmargin
    IF Arg_Present(zminor) THEN zminor = self.zminor 
    IF Arg_Present(zrange) THEN zrange = self.zrange
    IF Arg_Present(zstyle) THEN zstyle = self.zstyle
    IF Arg_Present(zthick) THEN zthick = self.zthick
    IF Arg_Present(ztick_get) THEN IF N_Elements(*self.ztick_get) NE 0 THEN ztick_get = *self.ztick_get
    IF Arg_Present(ztickformat) THEN ztickformat = self.ztickformat
    IF Arg_Present(ztickinterval) THEN ztickinterval = self.ztickinterval
    IF Arg_Present(zticklayout) THEN zticklayout = self.zticklayout 
    IF Arg_Present(zticklen) THEN zticklen = self.zticklen 
    IF Arg_Present(ztickname) THEN IF N_Elements(*self.ztickname) NE 0 THEN ztickname = *self.ztickname
    IF Arg_Present(zticks) THEN zticks = self.zticks
    IF Arg_Present(ztickunits) THEN ztickunits = self.ztickunits
    IF Arg_Present(ztickv) THEN IF N_Elements(*self.ztickv) NE 0 THEN ztickv = *self.ztickv
    IF Arg_Present(ztitle) THEN ztitle = self.ztitle
    IF Arg_Present(zvalue) THEN zvalue = self.zvalue

END ;---------------------------------------------------------------------------


PRO cgsGraphic::SetProperty, $
    ALTPS_KEYWORDS=altps_keywords, $
    BACKGROUND=background, $
    CHARSIZE=charsize, $
    CHARTHICK=charthick, $
    CLIP=clip, $
    COLOR=color, $
    DATA=data, $
    DEVICE=device, $
    NORMAL=normal, $
    FONT=font, $
    LINESTYLE=linestyle, $
    NOCLIP=noclip, $
    NODATA=nodata, $
    NOERASE=noerase, $
    POSITION=position, $
    PSYM=psym, $
    SUBTITLE=subtitle, $
    SYMSIZE=symsize, $
    T3D=t3d, $
    THICK=thick, $
    TICKLEN=ticklen, $
    TITLE=title, $
    
    XCHARSIZE=xcharsize, $
    XGRIDSTYLE=xgridstyle, $
    XMARGIN=xmargin, $
    XMINOR=xminor, $
    XRANGE=xrange, $
    XSTYLE=xstyle, $
    XTHICK=xthick, $
    XTICK_GET=xtick_get, $         ; Ignored in the SetProperty method.
    XTICKFORMAT=xtickformat, $
    XTICKINTERVAL=xtickinterval, $
    XTICKLAYOUT=xticklayout, $
    XTICKLEN=xticklen, $
    XTICKNAME=xtickname, $
    XTICKS=xticks, $
    XTICKUNITS=xtickunits, $
    XTICKV=xtickv, $
    XTITLE=xtitle, $
    
    YCHARSIZE=ycharsize, $
    YGRIDSTYLE=ygridstyle, $
    YMARGIN=ymargin, $
    YMINOR=yminor, $
    YRANGE=yrange, $
    YSTYLE=ystyle, $
    YTHICK=ythick, $
    YTICK_GET=ytick_get, $         ; Ignored in the SetProperty method.
    YTICKFORMAT=ytickformat, $
    YTICKINTERVAL=ytickinterval, $
    YTICKLAYOUT=yticklayout, $
    YTICKLEN=yticklen, $
    YTICKNAME=ytickname, $
    YTICKS=yticks, $
    YTICKUNITS=ytickunits, $
    YTICKV=ytickv, $
    YTITLE=ytitle, $
   
    ZCHARSIZE=zcharsize, $
    ZGRIDSTYLE=zgridstyle, $
    ZMARGIN=zmargin, $
    ZMINOR=zminor, $
    ZRANGE=zrange, $
    ZSTYLE=zstyle, $
    ZTHICK=zthick, $
    ZTICK_GET=ztick_get, $         ; Ignored in the SetProperty method.
    ZTICKFORMAT=ztickformat, $
    ZTICKINTERVAL=ztickinterval, $
    ZTICKLAYOUT=zticklayout, $
    ZTICKLEN=zticklen, $
    ZTICKNAME=ztickname, $
    ZTICKS=zticks, $
    ZTICKUNITS=ztickunits, $
    ZTICKV=ztickv, $
    ZTITLE=ztitle, $
    ZVALUE=zvalue, $
    _EXTRA=extra
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Set the properties of the object.
    IF N_Elements(altps_keywords) NE 0 THEN BEGIN
        self.altps_keywords = Ptr_New(altps_keywords)
    ENDIF 
    IF N_Elements(background) NE 0 THEN self.background = background
    IF N_Elements(charsize) NE 0 THEN self.charsize = charsize
    IF N_Elements(charthick) NE 0 THEN self.charthick = charthick
    IF N_Elements(clip) NE 0 THEN *self.clip = clip 
    IF N_Elements(color) NE 0 THEN self.color = color
    IF N_Elements(data) NE 0 THEN self.data = Keyword_Set(data)
    IF N_Elements(device) NE 0 THEN self.device = Keyword_Set(device)
    IF N_Elements(normal) NE 0 THEN self.normal = Keyword_Set(normal)
    IF N_Elements(font) NE 0 THEN *self.font = font 
    IF N_Elements(linestyle) NE 0 THEN self.linestyle = linestyle
    IF N_Elements(noclip) NE 0 THEN self.noclip = Keyword_Set(noclip)
    IF N_Elements(nodata) NE 0 THEN self.nodata = Keyword_Set(nodata)
    IF N_Elements(noerase) NE 0 THEN self.noerase = Keyword_Set(noerase)
    IF N_Elements(position) NE 0 THEN *self.position = position 
    IF N_Elements(psym) NE 0 THEN BEGIN
        thisSym = SymCat(Abs(psym))
        IF psym LT 0 THEN self.psym = -thisSym ELSE self.psym = thisSym
    ENDIF
    IF N_Elements(subtitle) NE 0 THEN self.subtitle = subtitle
    IF N_Elements(symsize) NE 0 THEN self.symsize = symsize
    IF N_Elements(t3d) NE 0 THEN self.t3d = Keyword_Set(t3d)
    IF N_Elements(thick) NE 0 THEN self.thick = thick
    IF N_Elements(ticklen) NE 0 THEN self.ticklen = ticklen
    IF N_Elements(title) NE 0 THEN self.title = title

    IF N_Elements(xcharsize) NE 0 THEN self.xcharsize = xcharsize
    IF N_Elements(xgridstyle) NE 0 THEN self.xgridstyle = xgridstyle
    IF N_Elements(xmargin) NE 0 THEN self.xmargin = xmargin
    IF N_Elements(xminor) NE 0 THEN self.xminor = xminor
    IF N_Elements(xrange) NE 0 THEN self.xrange = xrange
    IF N_Elements(xstyle) NE 0 THEN self.xstyle = xstyle
    IF N_Elements(xthick) NE 0 THEN self.xthick = xthick
    IF N_Elements(xtickformat) NE 0 THEN self.xtickformat = xtickformat
    IF N_Elements(xtickinterval) NE 0 THEN self.xtickinterval = xtickinterval
    IF N_Elements(xticklayout) NE 0 THEN self.xticklayout = xticklayout
    IF N_Elements(xticklen) NE 0 THEN self.xticklen = xticklen
    IF N_Elements(xtickname) NE 0 THEN *self.xtickname = xtickname 
    IF N_Elements(xticks) NE 0 THEN self.xticks = xticks
    IF N_Elements(xtickunits) NE 0 THEN self.xtickunits = xtickunits
    IF N_Elements(xtickv) NE 0 THEN *self.xtickv = xtickv 
    IF N_Elements(xtitle) NE 0 THEN self.xtitle = xtitle

    IF N_Elements(ycharsize) NE 0 THEN self.ycharsize = ycharsize
    IF N_Elements(ygridstyle) NE 0 THEN self.ygridstyle = ygridstyle
    IF N_Elements(ymargin) NE 0 THEN self.ymargin = ymargin
    IF N_Elements(yminor) NE 0 THEN self.yminor = yminor
    IF N_Elements(yrange) NE 0 THEN self.yrange = yrange
    IF N_Elements(ystyle) NE 0 THEN self.ystyle = ystyle
    IF N_Elements(ythick) NE 0 THEN self.ythick = ythick
    IF N_Elements(ytickformat) NE 0 THEN self.ytickformat = ytickformat
    IF N_Elements(ytickinterval) NE 0 THEN self.ytickinterval = ytickinterval
    IF N_Elements(yticklayout) NE 0 THEN self.yticklayout = yticklayout
    IF N_Elements(yticklen) NE 0 THEN self.yticklen = yticklen
    IF N_Elements(ytickname) NE 0 THEN *self.ytickname = ytickname 
    IF N_Elements(yticks) NE 0 THEN self.yticks = yticks
    IF N_Elements(ytickunits) NE 0 THEN self.ytickunits = ytickunits
    IF N_Elements(ytickv) NE 0 THEN *self.ytickv = ytickv 
    IF N_Elements(ytitle) NE 0 THEN self.ytitle = ytitle

    IF N_Elements(zcharsize) NE 0 THEN self.zcharsize = zcharsize
    IF N_Elements(zgridstyle) NE 0 THEN self.zgridstyle = zgridstyle
    IF N_Elements(zmargin) NE 0 THEN self.zmargin = zmargin
    IF N_Elements(zminor) NE 0 THEN self.zminor = zminor
    IF N_Elements(zrange) NE 0 THEN self.zrange = zrange
    IF N_Elements(zstyle) NE 0 THEN self.zstyle = zstyle
    IF N_Elements(zthick) NE 0 THEN self.zthick = zthick
    IF N_Elements(ztickformat) NE 0 THEN self.ztickformat = ztickformat
    IF N_Elements(ztickinterval) NE 0 THEN self.ztickinterval = ztickinterval
    IF N_Elements(zticklayout) NE 0 THEN self.zticklayout = zticklayout
    IF N_Elements(zticklen) NE 0 THEN self.zticklen = zticklen
    IF N_Elements(ztickname) NE 0 THEN *self.ztickname = ztickname
    IF N_Elements(zticks) NE 0 THEN self.zticks = zticks
    IF N_Elements(ztickunits) NE 0 THEN self.ztickunits = ztickunits
    IF N_Elements(ztickv) NE 0 THEN *self.ztickv = ztickv 
    IF N_Elements(ztitle) NE 0 THEN self.ztitle = ztitle
    IF N_Elements(zvalue) NE 0 THEN self.zvalue = zvalue
    

END ;---------------------------------------------------------------------------


PRO cgsGraphic::CLEANUP

   Ptr_Free, self.altps_keywords
   Ptr_Free, self.clip
   Ptr_Free, self.font
   Ptr_Free, self.position
   Ptr_Free, self.xtick_get
   Ptr_Free, self.xtickname
   Ptr_Free, self.xtickv
   Ptr_Free, self.ytick_get
   Ptr_Free, self.ytickname
   Ptr_Free, self.ytickv
   Ptr_Free, self.ztick_get
   Ptr_Free, self.ztickname
   Ptr_Free, self.ztickv
   
END ;---------------------------------------------------------------------------


FUNCTION cgsGraphic::INIT, $
    ALTPS_KEYWORDS=altps_keywords, $
    BACKGROUND=background, $
    CHARSIZE=charsize, $
    CHARTHICK=charthick, $
    CLIP=clip, $
    COLOR=color, $
    DATA=data, $
    DEVICE=device, $
    NORMAL=normal, $
    FONT=font, $
    LINESTYLE=linestyle, $
    NOCLIP=noclip, $
    NODATA=nodata, $
    NOERASE=noerase, $
    POSITION=position, $
    PSYM=psym, $
    SUBTITLE=subtitle, $
    SYMSIZE=symsize, $
    T3D=t3d, $
    THICK=thick, $
    TICKLEN=ticklen, $
    TITLE=title, $
    
    XCHARSIZE=xcharsize, $
    XGRIDSTYLE=xgridstyle, $
    XMARGIN=xmargin, $
    XMINOR=xminor, $
    XRANGE=xrange, $
    XSTYLE=xstyle, $
    XTHICK=xthick, $
    XTICK_GET=xtick_get, $
    XTICKFORMAT=xtickformat, $
    XTICKINTERVAL=xtickinterval, $
    XTICKLAYOUT=xticklayout, $
    XTICKLEN=xticklen, $
    XTICKNAME=xtickname, $
    XTICKS=xticks, $
    XTICKUNITS=xtickunits, $
    XTICKV=xtickv, $
    XTITLE=xtitle, $
    
    YCHARSIZE=ycharsize, $
    YGRIDSTYLE=ygridstyle, $
    YMARGIN=ymargin, $
    YMINOR=yminor, $
    YRANGE=yrange, $
    YSTYLE=ystyle, $
    YTHICK=ythick, $
    YTICK_GET=ytick_get, $
    YTICKFORMAT=ytickformat, $
    YTICKINTERVAL=ytickinterval, $
    YTICKLAYOUT=yticklayout, $
    YTICKLEN=yticklen, $
    YTICKNAME=ytickname, $
    YTICKS=yticks, $
    YTICKUNITS=ytickunits, $
    YTICKV=ytickv, $
    YTITLE=ytitle, $
   
    ZCHARSIZE=zcharsize, $
    ZGRIDSTYLE=zgridstyle, $
    ZMARGIN=zmargin, $
    ZMINOR=zminor, $
    ZRANGE=zrange, $
    ZSTYLE=zstyle, $
    ZTHICK=zthick, $
    ZTICK_GET=ztick_get, $
    ZTICKFORMAT=ztickformat, $
    ZTICKINTERVAL=ztickinterval, $
    ZTICKLAYOUT=zticklayout, $
    ZTICKLEN=zticklen, $
    ZTICKNAME=ztickname, $
    ZTICKS=zticks, $
    ZTICKUNITS=ztickunits, $
    ZTICKV=ztickv, $
    ZTITLE=ztitle, $
    ZVALUE=zvalue, $
    _EXTRA=extraKeywords
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        void = Error_Message()
        RETURN, 0
    ENDIF
    
    ; Default values.
    IF N_Elements(background) EQ 0 THEN background = 'white'
    IF N_Elements(color) EQ 0 THEN color = 'black'
    IF N_Elements(font) EQ 0 THEN self.font = Ptr_New(/ALLOCATE_HEAP) ELSE self.font = Ptr_New(font)
    thisFont = *self.font
    IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharSize(FONT=thisFont)
    IF N_Elements(ticklen) EQ 0 THEN ticklen = !P.TICKLEN
    IF N_Elements(xmargin) EQ 0 THEN xmargin = [10.0, 4.0]
    IF N_Elements(ymargin) EQ 0 THEN ymargin = [4.0, 4.0]
    
        
    ; Allocate heap for variables.
    self.clip = Ptr_New(/ALLOCATE_HEAP)
   self.font = Ptr_New(/ALLOCATE_HEAP)
   self.position = Ptr_New(/ALLOCATE_HEAP)
   self.xtick_get = Ptr_New(/ALLOCATE_HEAP)
   self.xtickname = Ptr_New(/ALLOCATE_HEAP)
   self.xtickv = Ptr_New(/ALLOCATE_HEAP)
   self.ytick_get = Ptr_New(/ALLOCATE_HEAP)
   self.ytickname = Ptr_New(/ALLOCATE_HEAP)
   self.ytickv = Ptr_New(/ALLOCATE_HEAP)
   self.ztick_get = Ptr_New(/ALLOCATE_HEAP)
   self.ztickname = Ptr_New(/ALLOCATE_HEAP)
   self.ztickv = Ptr_New(/ALLOCATE_HEAP)
    
        
    ; Set all the keywords.
    self -> SetProperty, $
        ALTPS_KEYWORDS=altps_keywords, $
        BACKGROUND=background, $
        CHARSIZE=charsize, $
        CHARTHICK=charthick, $
        CLIP=clip, $
        COLOR=color, $
        DATA=data, $
        DEVICE=device, $
        NORMAL=normal, $
        LINESTYLE=linestyle, $
        NOCLIP=noclip, $
        NODATA=nodata, $
        NOERASE=noerase, $
        POSITION=position, $
        PSYM=psym, $
        SUBTITLE=subtitle, $
        SYMSIZE=symsize, $
        T3D=t3d, $
        THICK=thick, $
        TICKLEN=ticklen, $
        TITLE=title, $
        
        XCHARSIZE=xcharsize, $
        XGRIDSTYLE=xgridstyle, $
        XMARGIN=xmargin, $
        XMINOR=xminor, $
        XRANGE=xrange, $
        XSTYLE=xstyle, $
        XTHICK=xthick, $
        XTICK_GET=xtick_get, $
        XTICKFORMAT=xtickformat, $
        XTICKINTERVAL=xtickinterval, $
        XTICKLAYOUT=xticklayout, $
        XTICKLEN=xticklen, $
        XTICKNAME=xtickname, $
        XTICKS=xticks, $
        XTICKUNITS=xtickunits, $
        XTICKV=xtickv, $
        XTITLE=xtitle, $
        
        YCHARSIZE=ycharsize, $
        YGRIDSTYLE=ygridstyle, $
        YMARGIN=ymargin, $
        YMINOR=yminor, $
        YRANGE=yrange, $
        YSTYLE=ystyle, $
        YTHICK=ythick, $
        YTICK_GET=ytick_get, $
        YTICKFORMAT=ytickformat, $
        YTICKINTERVAL=ytickinterval, $
        YTICKLAYOUT=yticklayout, $
        YTICKLEN=yticklen, $
        YTICKNAME=ytickname, $
        YTICKS=yticks, $
        YTICKUNITS=ytickunits, $
        YTICKV=ytickv, $
        YTITLE=ytitle, $
       
        ZCHARSIZE=zcharsize, $
        ZGRIDSTYLE=zgridstyle, $
        ZMARGIN=zmargin, $
        ZMINOR=zminor, $
        ZRANGE=zrange, $
        ZSTYLE=zstyle, $
        ZTHICK=zthick, $
        ZTICK_GET=ztick_get, $
        ZTICKFORMAT=ztickformat, $
        ZTICKINTERVAL=ztickinterval, $
        ZTICKLAYOUT=zticklayout, $
        ZTICKLEN=zticklen, $
        ZTICKNAME=ztickname, $
        ZTICKS=zticks, $
        ZTICKUNITS=ztickunits, $
        ZTICKV=ztickv, $
        ZTITLE=ztitle, $
        ZVALUE=zvalue, $
        _STRICT_EXTRA=extraKeywords
    
    RETURN, 1
    
END ;---------------------------------------------------------------------------


PRO cgsGraphic__DEFINE, class

    IF Fix(!Version.Release) GE 8 THEN BEGIN
    
        class = { CGSGRAPHIC, $
                  INHERITS IDL_OBJECT, $
                  ALTPS_KEYWORDS: Ptr_New(), $
                  BACKGROUND: "", $
                  CHARSIZE: 0.0, $
                  CHARTHICK: 0.0, $
                  CLIP: Ptr_New(), $
                  COLOR: "", $
                  DATA: 0B, $
                  DEVICE: 0B, $
                  NORMAL: 0B, $
                  FONT: Ptr_New(), $
                  LINESTYLE: 0S, $
                  NOCLIP: 0B, $
                  NODATA: 0B, $
                  NOERASE: 0B, $
                  POSITION: Ptr_New(), $
                  PSYM: 0S, $
                  SUBTITLE: "", $
                  SYMSIZE: 0.0, $
                  T3D: 0B, $
                  THICK: 0.0, $
                  TICKLEN: 0.0, $
                  TITLE: "", $
                  XCHARSIZE: 0.0, $
                  XGRIDSTYLE: 0S, $
                  XMARGIN: DblArr(2), $
                  XMINOR: 0S, $
                  XRANGE: DblArr(2), $
                  XSTYLE: 0UL, $
                  XTHICK: 0.0, $
                  XTICK_GET: Ptr_New(), $
                  XTICKFORMAT: "", $
                  XTICKINTERVAL: 0S, $
                  XTICKLAYOUT: 0S, $
                  XTICKLEN: 0.0, $
                  XTICKNAME: Ptr_New(), $
                  XTICKS: 0S, $
                  XTICKUNITS: "", $
                  XTICKV: Ptr_New(), $
                  XTITLE: "", $

                  YCHARSIZE: 0.0, $
                  YGRIDSTYLE: 0S, $
                  YMARGIN: DblArr(2), $
                  YMINOR: 0S, $
                  YRANGE: DblArr(2), $
                  YSTYLE: 0UL, $
                  YTHICK: 0.0, $
                  YTICK_GET: Ptr_New(), $
                  YTICKFORMAT: "", $
                  YTICKINTERVAL: 0S, $
                  YTICKLAYOUT: 0S, $
                  YTICKLEN: 0.0, $
                  YTICKNAME: Ptr_New(), $
                  YTICKS: 0S, $
                  YTICKUNITS: "", $
                  YTICKV: Ptr_New(), $
                  YTITLE: "", $
                  
                  ZCHARSIZE: 0.0, $
                  ZGRIDSTYLE: 0S, $
                  ZMARGIN: DblArr(2), $
                  ZMINOR: 0S, $
                  ZRANGE: DblArr(2), $
                  ZSTYLE: 0UL, $
                  ZTHICK: 0.0, $
                  ZTICK_GET: Ptr_New(), $
                  ZTICKFORMAT: "", $
                  ZTICKINTERVAL: 0S, $
                  ZTICKLAYOUT: 0S, $
                  ZTICKLEN: 0.0, $
                  ZTICKNAME: Ptr_New(), $
                  ZTICKS: 0S, $
                  ZTICKUNITS: "", $
                  ZTICKV: Ptr_New(), $
                  ZTITLE: "", $
                  
                  ZVALUE: 0.0 $
                  }
    
    ENDIF ELSE BEGIN
    
        class = { CGSGRAPHIC, $
                  ALTPS_KEYWORDS: Ptr_New(), $
                  BACKGROUND: "", $
                  CHARSIZE: 0.0, $
                  CHARTHICK: 0.0, $
                  CLIP: Ptr_New(), $
                  COLOR: "", $
                  DATA: 0B, $
                  DEVICE: 0B, $
                  NORMAL: 0B, $
                  FONT: Ptr_New(), $
                  LINESTYLE: 0S, $
                  NOCLIP: 0B, $
                  NODATA: 0B, $
                  NOERASE: 0B, $
                  POSITION: Ptr_New(), $
                  PSYM: 0S, $
                  SUBTITLE: "", $
                  SYMSIZE: 0.0, $
                  T3D: 0B, $
                  THICK: 0.0, $
                  TICKLEN: 0.0, $
                  TITLE: "", $
                  XCHARSIZE: 0.0, $
                  XGRIDSTYLE: 0S, $
                  XMARGIN: DblArr(2), $
                  XMINOR: 0S, $
                  XRANGE: DblArr(2), $
                  XSTYLE: 0UL, $
                  XTHICK: 0.0, $
                  XTICK_GET: Ptr_New(), $
                  XTICKFORMAT: "", $
                  XTICKINTERVAL: 0S, $
                  XTICKLAYOUT: 0S, $
                  XTICKLEN: 0.0, $
                  XTICKNAME: Ptr_New(), $
                  XTICKS: 0S, $
                  XTICKUNITS: "", $
                  XTICKV: Ptr_New(), $
                  XTITLE: "", $

                  YCHARSIZE: 0.0, $
                  YGRIDSTYLE: 0S, $
                  YMARGIN: DblArr(2), $
                  YMINOR: 0S, $
                  YRANGE: DblArr(2), $
                  YSTYLE: 0UL, $
                  YTHICK: 0.0, $
                  YTICK_GET: Ptr_New(), $
                  YTICKFORMAT: "", $
                  YTICKINTERVAL: 0S, $
                  YTICKLAYOUT: 0S, $
                  YTICKLEN: 0.0, $
                  YTICKNAME: Ptr_New(), $
                  YTICKS: 0S, $
                  YTICKUNITS: "", $
                  YTICKV: Ptr_New(), $
                  YTITLE: "", $
                  
                  ZCHARSIZE: 0.0, $
                  ZGRIDSTYLE: 0S, $
                  ZMARGIN: DblArr(2), $
                  ZMINOR: 0S, $
                  ZRANGE: DblArr(2), $
                  ZSTYLE: 0UL, $
                  ZTHICK: 0.0, $
                  ZTICK_GET: Ptr_New(), $
                  ZTICKFORMAT: "", $
                  ZTICKINTERVAL: 0S, $
                  ZTICKLAYOUT: 0S, $
                  ZTICKLEN: 0.0, $
                  ZTICKNAME: Ptr_New(), $
                  ZTICKS: 0S, $
                  ZTICKUNITS: "", $
                  ZTICKV: Ptr_New(), $
                  ZTITLE: "", $
                  
                  ZVALUE: 0.0 $
                  }
    ENDELSE
           
END