; docformat = 'rst'
;
; NAME:
;   cgGraphicsKeywords__Define
;
; PURPOSE:
;   Provides an object interface to handle IDL direct graphics plotting keywords. Basically,
;   any graphics keyword that is common to IDL plotting routines (e.g. Plot, Contour, 
;   Surface, etc.) is supported here.
;
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
; Provides an object interface to handle IDL direct graphics plotting keywords. Basically,
; any graphics keyword that is common to IDL plotting routines (e.g. Plot, Contour, 
; Surface, etc.) is supported here. See the IDL documentation for "Graphics Keywords for
; a complete list.
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics
;           
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: david@idlcoyote.com
;           Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 16 May 2012, by David W. Fanning.
;        Added missing LINESTYLE keyword. 22 May 2012. DWF.
;        BIG problem in the way I was handling the PSYM keyword solved! 18 July 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-

;+
; This method initializes the object. Any "graphics keyword" that falls under
; the IDL definition is allowed. Plus, there are a few "Coyote Graphics" specific
; keywords (e.g., AXISCOLOR) that are not allowed in normal IDL plotting routines.
; Colors are handled in the Coyote Graphics way, as color names, primarily.
;-
FUNCTION cgGraphicsKeywords::INIT, $
    AXISCOLOR=axiscolor, $
    BACKGROUND=background, $
    CHARSIZE=charsize, $
    CHARTHICK=charthick, $
    CLIP=clip, $
    COLOR=color, $
    DATA=data, $
    DEVICE=device, $
    LINESTYLE=linestyle, $
    NORMAL=normal, $
    FONT=font, $
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
    ZVALUE=zvalue
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        void = Error_Message()
        RETURN, 0
    ENDIF
    
    ; Default values.
    IF N_Elements(background) EQ 0 THEN background = 'white'
    IF N_Elements(axiscolor) EQ 0 THEN axiscolor = 'opposite' 
    IF N_Elements(color) EQ 0 THEN color = 'opposite'
    IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharSize(FONT=!P.Font)
    
    ; Allocate heap for variables.
    self.axiscolor = Ptr_New(/Allocate_Heap)
    self.background = Ptr_New(/Allocate_Heap)
    self.charsize = Ptr_New(/Allocate_Heap)
    self.charthick = Ptr_New(/Allocate_Heap)
    self.clip = Ptr_New(/Allocate_Heap)
    self.color = Ptr_New(/Allocate_Heap)
    self.data = Ptr_New(/Allocate_Heap)
    self.device = Ptr_New(/Allocate_Heap)
    self.font = Ptr_New(/Allocate_Heap)
    self.linestyle = Ptr_New(/Allocate_Heap)
    self.normal = Ptr_New(/Allocate_Heap)
    self.noclip = Ptr_New(/Allocate_Heap)
    self.nodata = Ptr_New(/Allocate_Heap)
    self.noerase = Ptr_New(/Allocate_Heap)
    self.position = Ptr_New(/Allocate_Heap)
    self.psym = Ptr_New(/Allocate_Heap)
    self.subtitle = Ptr_New(/Allocate_Heap)
    self.symsize = Ptr_New(/Allocate_Heap)
    self.t3d = Ptr_New(/Allocate_Heap)
    self.thick = Ptr_New(/Allocate_Heap)
    self.ticklen = Ptr_New(/Allocate_Heap)
    self.title = Ptr_New(/Allocate_Heap)
    
    self.xcharsize = Ptr_New(/Allocate_Heap)
    self.xgridstyle = Ptr_New(/Allocate_Heap)
    self.xmargin = Ptr_New(/Allocate_Heap)
    self.xminor = Ptr_New(/Allocate_Heap)
    self.xrange = Ptr_New(/Allocate_Heap)
    self.xstyle = Ptr_New(/Allocate_Heap)
    self.xthick = Ptr_New(/Allocate_Heap)
    self.xtick_get = Ptr_New(/Allocate_Heap)
    self.xtickformat = Ptr_New(/Allocate_Heap)
    self.xtickinterval = Ptr_New(/Allocate_Heap)
    self.xticklayout = Ptr_New(/Allocate_Heap)
    self.xticklen = Ptr_New(/Allocate_Heap)
    self.xtickname = Ptr_New(/Allocate_Heap)
    self.xticks = Ptr_New(/Allocate_Heap)
    self.xtickunits = Ptr_New(/Allocate_Heap)
    self.xtickv = Ptr_New(/Allocate_Heap)
    self.xtitle = Ptr_New(/Allocate_Heap)
    
    self.ycharsize = Ptr_New(/Allocate_Heap)
    self.ygridstyle = Ptr_New(/Allocate_Heap)
    self.ymargin = Ptr_New(/Allocate_Heap)
    self.yminor = Ptr_New(/Allocate_Heap)
    self.yrange = Ptr_New(/Allocate_Heap)
    self.ystyle = Ptr_New(/Allocate_Heap)
    self.ythick = Ptr_New(/Allocate_Heap)
    self.ytick_get = Ptr_New(/Allocate_Heap)
    self.ytickformat = Ptr_New(/Allocate_Heap)
    self.ytickinterval = Ptr_New(/Allocate_Heap)
    self.yticklayout = Ptr_New(/Allocate_Heap)
    self.yticklen = Ptr_New(/Allocate_Heap)
    self.ytickname = Ptr_New(/Allocate_Heap)
    self.yticks = Ptr_New(/Allocate_Heap)
    self.ytickunits = Ptr_New(/Allocate_Heap)
    self.ytickv = Ptr_New(/Allocate_Heap)
    self.ytitle = Ptr_New(/Allocate_Heap)
       
    self.zcharsize = Ptr_New(/Allocate_Heap)
    self.zgridstyle = Ptr_New(/Allocate_Heap)
    self.zmargin = Ptr_New(/Allocate_Heap)
    self.zminor = Ptr_New(/Allocate_Heap)
    self.zrange = Ptr_New(/Allocate_Heap)
    self.zstyle = Ptr_New(/Allocate_Heap)
    self.zthick = Ptr_New(/Allocate_Heap)
    self.ztick_get = Ptr_New(/Allocate_Heap)
    self.ztickformat = Ptr_New(/Allocate_Heap)
    self.ztickinterval = Ptr_New(/Allocate_Heap)
    self.zticklayout = Ptr_New(/Allocate_Heap)
    self.zticklen = Ptr_New(/Allocate_Heap)
    self.ztickname = Ptr_New(/Allocate_Heap)
    self.zticks = Ptr_New(/Allocate_Heap)
    self.ztickunits = Ptr_New(/Allocate_Heap)
    self.ztickv = Ptr_New(/Allocate_Heap)
    self.ztitle = Ptr_New(/Allocate_Heap)
    self.zvalue = Ptr_New(/Allocate_Heap)
    
        
    ; Set all the keywords.
    self -> SetProperty, $
        AXISCOLOR=axiscolor, $
        BACKGROUND=background, $
        CHARSIZE=charsize, $
        CHARTHICK=charthick, $
        CLIP=clip, $
        COLOR=color, $
        DATA=data, $
        DEVICE=device, $
        FONT=font, $
        LINESTYLE=linestyle, $
        NORMAL=normal, $
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
        
        ZVALUE=zvalue
    
    RETURN, 1
    
END 

;+
; The clean-up method for the object. Nearly all keywords are stored as pointers
; that must be cleaned up here.
;-
PRO cgGraphicsKeywords::CLEANUP

    Ptr_Free, self.axiscolor
    Ptr_Free, self.background
    Ptr_Free, self.charsize
    Ptr_Free, self.charthick
    Ptr_Free, self.clip
    Ptr_Free, self.color
    Ptr_Free, self.data
    Ptr_Free, self.device
    Ptr_Free, self.font
    Ptr_Free, self.linestyle
    Ptr_Free, self.normal
    Ptr_Free, self.noclip
    Ptr_Free, self.nodata
    Ptr_Free, self.noerase
    Ptr_Free, self.position
    Ptr_Free, self.psym
    Ptr_Free, self.subtitle
    Ptr_Free, self.symsize
    Ptr_Free, self.t3d
    Ptr_Free, self.thick
    Ptr_Free, self.ticklen
    Ptr_Free, self.title
    
    Ptr_Free, self.xcharsize
    Ptr_Free, self.xgridstyle
    Ptr_Free, self.xmargin
    Ptr_Free, self.xminor
    Ptr_Free, self.xrange
    Ptr_Free, self.xstyle
    Ptr_Free, self.xthick
    Ptr_Free, self.xtick_get
    Ptr_Free, self.xtickformat
    Ptr_Free, self.xtickinterval
    Ptr_Free, self.xticklayout
    Ptr_Free, self.xticklen
    Ptr_Free, self.xtickname
    Ptr_Free, self.xticks
    Ptr_Free, self.xtickunits
    Ptr_Free, self.xtickv
    Ptr_Free, self.xtitle
    
    Ptr_Free, self.ycharsize
    Ptr_Free, self.ygridstyle
    Ptr_Free, self.ymargin
    Ptr_Free, self.yminor
    Ptr_Free, self.yrange
    Ptr_Free, self.ystyle
    Ptr_Free, self.ythick
    Ptr_Free, self.ytick_get
    Ptr_Free, self.ytickformat
    Ptr_Free, self.ytickinterval
    Ptr_Free, self.yticklayout
    Ptr_Free, self.yticklen
    Ptr_Free, self.ytickname
    Ptr_Free, self.yticks
    Ptr_Free, self.ytickunits
    Ptr_Free, self.ytickv
    Ptr_Free, self.ytitle
       
    Ptr_Free, self.zcharsize
    Ptr_Free, self.zgridstyle
    Ptr_Free, self.zmargin
    Ptr_Free, self.zminor
    Ptr_Free, self.zrange
    Ptr_Free, self.zstyle
    Ptr_Free, self.zthick
    Ptr_Free, self.ztick_get
    Ptr_Free, self.ztickformat
    Ptr_Free, self.ztickinterval
    Ptr_Free, self.zticklayout
    Ptr_Free, self.zticklen
    Ptr_Free, self.ztickname
    Ptr_Free, self.zticks
    Ptr_Free, self.ztickunits
    Ptr_Free, self.ztickv
    Ptr_Free, self.ztitle
    Ptr_Free, self.zvalue
END

;+
; The GetProperty method is the way graphics routines obtain the keyword values for
; the graphics keywords.
;-
PRO cgGraphicsKeywords::GetProperty, $
    AXISCOLOR=axiscolor, $
    BACKGROUND=background, $
    CHARSIZE=charsize, $
    CHARTHICK=charthick, $
    CLIP=clip, $
    COLOR=color, $
    DATA=data, $
    DEVICE=device, $
    FONT=font, $
    LINESTYLE=linestyle, $
    NORMAL=normal, $
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
    ZVALUE=zvalue
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Get the properties of the object.
    IF Arg_Present(axiscolor) THEN IF N_Elements(*self.axiscolor) NE 0 THEN axiscolor = *self.axiscolor
    IF Arg_Present(background) THEN IF N_Elements(*self.background) NE 0 THEN background = *self.background
    IF Arg_Present(charsize) THEN IF N_Elements(*self.charsize) NE 0 THEN charsize = *self.charsize 
    IF Arg_Present(charthick) THEN IF N_Elements(*self.charthick) NE 0 THEN charthick = *self.charthick 
    IF Arg_Present(clip) THEN IF N_Elements(*self.clip) NE 0 THEN clip = *self.clip 
    IF Arg_Present(color) THEN IF N_Elements(*self.color) NE 0 THEN color = *self.color
    IF Arg_Present(data) THEN IF N_Elements(*self.data) NE 0 THEN data = *self.data 
    IF Arg_Present(device) THEN IF N_Elements(*self.device) NE 0 THEN device = *self.device
    IF Arg_Present(normal) THEN IF N_Elements(*self.normal) NE 0 THEN normal = *self.normal
    IF Arg_Present(font) THEN IF N_Elements(*self.font) NE 0 THEN font = *self.font
    IF Arg_Present(linestyle) THEN IF N_Elements(*self.linestyle) NE 0 THEN linestyle = *self.linestyle
    IF Arg_Present(noclip) THEN IF N_Elements(*self.noclip) NE 0 THEN noclip = *self.noclip
    IF Arg_Present(nodata) THEN IF N_Elements(*self.nodata) NE 0 THEN nodata = *self.nodata 
    IF Arg_Present(noerase) THEN IF N_Elements(*self.noerase) NE 0 THEN noerase = *self.noerase
    IF Arg_Present(position) THEN IF N_Elements(*self.position) NE 0 THEN position = *self.position
    IF Arg_Present(psym) THEN IF N_Elements(*self.psym) NE 0 THEN psym = *self.psym
    IF Arg_Present(subtitle) THEN IF N_Elements(*self.subtitle) NE 0 THEN subtitle = *self.subtitle
    IF Arg_Present(symsize) THEN IF N_Elements(*self.symsize) NE 0 THEN symsize = *self.symsize
    IF Arg_Present(t3d) THEN IF N_Elements(*self.t3d) NE 0 THEN t3d = *self.t3d 
    IF Arg_Present(thick) THEN IF N_Elements(*self.thick) NE 0 THEN thick = *self.thick
    IF Arg_Present(ticklen) THEN IF N_Elements(*self.ticklen) NE 0 THEN ticklen = *self.ticklen
    IF Arg_Present(title) THEN IF N_Elements(*self.title) NE 0 THEN title = *self.title

    IF Arg_Present(xcharsize) THEN IF N_Elements(*self.xcharsize) NE 0 THEN xcharsize = *self.xcharsize
    IF Arg_Present(xgridstyle) THEN IF N_Elements(*self.xgridstyle) NE 0 THEN xgridstyle = *self.xgridstyle
    IF Arg_Present(xmargin) THEN IF N_Elements(*self.xmargin) NE 0 THEN xmargin = *self.xmargin
    IF Arg_Present(xminor) THEN IF N_Elements(*self.xminor) NE 0 THEN xminor = *self.xminor 
    IF Arg_Present(xrange) THEN IF N_Elements(*self.xrange) NE 0 THEN xrange = *self.xrange
    IF Arg_Present(xstyle) THEN IF N_Elements(*self.xstyle) NE 0 THEN xstyle = *self.xstyle
    IF Arg_Present(xthick) THEN IF N_Elements(*self.xthick) NE 0 THEN xthick = *self.xthick
    IF Arg_Present(xtick_get) THEN IF N_Elements(*self.xtick_get) NE 0 THEN xtick_get = *self.xtick_get
    IF Arg_Present(xtickformat) THEN IF N_Elements(*self.xtickformat) NE 0 THEN xtickformat = *self.xtickformat
    IF Arg_Present(xtickinterval) THEN IF N_Elements(*self.xtickinterval) NE 0 THEN xtickinterval = *self.xtickinterval
    IF Arg_Present(xticklayout) THEN IF N_Elements(*self.xticklayout) NE 0 THEN xticklayout = *self.xticklayout 
    IF Arg_Present(xticklen) THEN IF N_Elements(*self.xticklen) NE 0 THEN xticklen = *self.xticklen 
    IF Arg_Present(xtickname) THEN IF N_Elements(*self.xtickname) NE 0 THEN xtickname = *self.xtickname
    IF Arg_Present(xticks) THEN IF N_Elements(*self.xticks) NE 0 THEN xticks = *self.xticks
    IF Arg_Present(xtickunits) THEN IF N_Elements(*self.xtickunits) NE 0 THEN xtickunits = *self.xtickunits
    IF Arg_Present(xtickv) THEN IF N_Elements(*self.xtickv) NE 0 THEN xtickv = *self.xtickv
    IF Arg_Present(xtitle) THEN IF N_Elements(*self.xtitle) NE 0 THEN xtitle = *self.xtitle
    
    IF Arg_Present(ycharsize) THEN IF N_Elements(*self.ycharsize) NE 0 THEN ycharsize = *self.ycharsize
    IF Arg_Present(ygridstyle) THEN IF N_Elements(*self.ygridstyle) NE 0 THEN ygridstyle = *self.ygridstyle
    IF Arg_Present(ymargin) THEN IF N_Elements(*self.ymargin) NE 0 THEN ymargin = *self.ymargin
    IF Arg_Present(yminor) THEN IF N_Elements(*self.yminor) NE 0 THEN yminor = *self.yminor 
    IF Arg_Present(yrange) THEN IF N_Elements(*self.yrange) NE 0 THEN yrange = *self.yrange
    IF Arg_Present(ystyle) THEN IF N_Elements(*self.ystyle) NE 0 THEN ystyle = *self.ystyle
    IF Arg_Present(ythick) THEN IF N_Elements(*self.ythick) NE 0 THEN ythick = *self.ythick
    IF Arg_Present(ytick_get) THEN IF N_Elements(*self.ytick_get) NE 0 THEN ytick_get = *self.ytick_get
    IF Arg_Present(ytickformat) THEN IF N_Elements(*self.ytickformat) NE 0 THEN ytickformat = *self.ytickformat
    IF Arg_Present(ytickinterval) THEN IF N_Elements(*self.ytickinterval) NE 0 THEN ytickinterval = *self.ytickinterval
    IF Arg_Present(yticklayout) THEN IF N_Elements(*self.yticklayout) NE 0 THEN yticklayout = *self.yticklayout 
    IF Arg_Present(yticklen) THEN IF N_Elements(*self.yticklen) NE 0 THEN yticklen = *self.yticklen 
    IF Arg_Present(ytickname) THEN IF N_Elements(*self.ytickname) NE 0 THEN ytickname = *self.ytickname
    IF Arg_Present(yticks) THEN IF N_Elements(*self.yticks) NE 0 THEN yticks = *self.yticks
    IF Arg_Present(ytickunits) THEN IF N_Elements(*self.ytickunits) NE 0 THEN ytickunits = *self.ytickunits
    IF Arg_Present(ytickv) THEN IF N_Elements(*self.ytickv) NE 0 THEN ytickv = *self.ytickv
    IF Arg_Present(ytitle) THEN IF N_Elements(*self.ytitle) NE 0 THEN ytitle = *self.ytitle

    IF Arg_Present(zcharsize) THEN IF N_Elements(*self.zcharsize) NE 0 THEN zcharsize = *self.zcharsize
    IF Arg_Present(zgridstyle) THEN IF N_Elements(*self.zgridstyle) NE 0 THEN zgridstyle = *self.zgridstyle
    IF Arg_Present(zmargin) THEN IF N_Elements(*self.zmargin) NE 0 THEN zmargin = *self.zmargin
    IF Arg_Present(zminor) THEN IF N_Elements(*self.zminor) NE 0 THEN zminor = *self.zminor 
    IF Arg_Present(zrange) THEN IF N_Elements(*self.zrange) NE 0 THEN zrange = *self.zrange
    IF Arg_Present(zstyle) THEN IF N_Elements(*self.zstyle) NE 0 THEN zstyle = *self.zstyle
    IF Arg_Present(zthick) THEN IF N_Elements(*self.ztick_get) NE 0 THEN zthick = *self.ztick_get
    IF Arg_Present(ztick_get) THEN IF N_Elements(*self.ztick_get) NE 0 THEN ztick_get = *self.ztick_get
    IF Arg_Present(ztickformat) THEN IF N_Elements(*self.ztickformat) NE 0 THEN ztickformat = *self.ztickformat
    IF Arg_Present(ztickinterval) THEN IF N_Elements(*self.ztickinterval) NE 0 THEN ztickinterval = *self.ztickinterval
    IF Arg_Present(zticklayout) THEN IF N_Elements(*self.zticklayout) NE 0 THEN zticklayout = *self.zticklayout 
    IF Arg_Present(zticklen) THEN IF N_Elements(*self.zticklen) NE 0 THEN zticklen = *self.zticklen 
    IF Arg_Present(ztickname) THEN IF N_Elements(*self.ztickname) NE 0 THEN ztickname = *self.ztickname
    IF Arg_Present(zticks) THEN IF N_Elements(*self.zticks) NE 0 THEN zticks = *self.zticks
    IF Arg_Present(ztickunits) THEN IF N_Elements(*self.ztickunits) NE 0 THEN ztickunits = *self.ztickunits
    IF Arg_Present(ztickv) THEN IF N_Elements(*self.ztickv) NE 0 THEN ztickv = *self.ztickv
    IF Arg_Present(ztitle) THEN IF N_Elements(*self.ztitle) NE 0 THEN ztitle = *self.ztitle
    IF Arg_Present(zvalue) THEN IF N_Elements(*self.zvalue) NE 0 THEN zvalue = *self.zvalue

END

;+
; The SetProperty method is how these keyword values are set for the plotting routine.
;-
PRO cgGraphicsKeywords::SetProperty, $
    AXISCOLOR=axiscolor, $
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
    ZVALUE=zvalue

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        void = Error_Message()
        RETURN
    ENDIF
    
    IF N_Elements(axiscolor) NE 0 THEN *self.axiscolor = axiscolor
    IF N_Elements(background) NE 0 THEN *self.background = background
    IF N_Elements(charsize) NE 0 THEN *self.charsize = charsize
    IF N_Elements(charthick) NE 0 THEN *self.charthick = charthick
    IF N_Elements(clip) NE 0 THEN *self.clip = clip 
    IF N_Elements(color) NE 0 THEN *self.color = color
    IF N_Elements(data) NE 0 THEN *self.data = Keyword_Set(data)
    IF N_Elements(device) NE 0 THEN *self.device = Keyword_Set(device)
    IF N_Elements(normal) NE 0 THEN *self.normal = Keyword_Set(normal)
    IF N_Elements(font) NE 0 THEN *self.font = font 
    IF N_Elements(linestyle) NE 0 THEN *self.linestyle = linestyle 
    IF N_Elements(noclip) NE 0 THEN *self.noclip = Keyword_Set(noclip)
    IF N_Elements(nodata) NE 0 THEN *self.nodata = Keyword_Set(nodata)
    IF N_Elements(noerase) NE 0 THEN *self.noerase = Keyword_Set(noerase)
    IF N_Elements(position) NE 0 THEN *self.position = position 
    IF N_Elements(psym) NE 0 THEN *self.psym = psym
    IF N_Elements(subtitle) NE 0 THEN *self.subtitle = subtitle
    IF N_Elements(symsize) NE 0 THEN *self.symsize = symsize
    IF N_Elements(t3d) NE 0 THEN *self.t3d = Keyword_Set(t3d)
    IF N_Elements(thick) NE 0 THEN *self.thick = thick
    IF N_Elements(ticklen) NE 0 THEN *self.ticklen = ticklen
    IF N_Elements(title) NE 0 THEN *self.title = title

    IF N_Elements(xcharsize) NE 0 THEN *self.xcharsize = xcharsize
    IF N_Elements(xgridstyle) NE 0 THEN *self.xgridstyle = xgridstyle
    IF N_Elements(xmargin) NE 0 THEN *self.xmargin = xmargin
    IF N_Elements(xminor) NE 0 THEN *self.xminor = xminor
    IF N_Elements(xrange) NE 0 THEN *self.xrange = xrange
    IF N_Elements(xstyle) NE 0 THEN *self.xstyle = xstyle
    IF N_Elements(xthick) NE 0 THEN *self.xthick = xthick
    IF N_Elements(xtickformat) NE 0 THEN *self.xtickformat = xtickformat
    IF N_Elements(xtickinterval) NE 0 THEN *self.xtickinterval = xtickinterval
    IF N_Elements(xticklayout) NE 0 THEN *self.xticklayout = xticklayout
    IF N_Elements(xticklen) NE 0 THEN *self.xticklen = xticklen
    IF N_Elements(xtickname) NE 0 THEN *self.xtickname = xtickname 
    IF N_Elements(xticks) NE 0 THEN *self.xticks = xticks
    IF N_Elements(xtickunits) NE 0 THEN *self.xtickunits = xtickunits
    IF N_Elements(xtickv) NE 0 THEN *self.xtickv = xtickv 
    IF N_Elements(xtitle) NE 0 THEN *self.xtitle = xtitle

    IF N_Elements(ycharsize) NE 0 THEN *self.ycharsize = ycharsize
    IF N_Elements(ygridstyle) NE 0 THEN *self.ygridstyle = ygridstyle
    IF N_Elements(ymargin) NE 0 THEN *self.ymargin = ymargin
    IF N_Elements(yminor) NE 0 THEN *self.yminor = yminor
    IF N_Elements(yrange) NE 0 THEN *self.yrange = yrange
    IF N_Elements(ystyle) NE 0 THEN *self.ystyle = ystyle
    IF N_Elements(ythick) NE 0 THEN *self.ythick = ythick
    IF N_Elements(ytickformat) NE 0 THEN *self.ytickformat = ytickformat
    IF N_Elements(ytickinterval) NE 0 THEN *self.ytickinterval = ytickinterval
    IF N_Elements(yticklayout) NE 0 THEN *self.yticklayout = yticklayout
    IF N_Elements(yticklen) NE 0 THEN *self.yticklen = yticklen
    IF N_Elements(ytickname) NE 0 THEN *self.ytickname = ytickname 
    IF N_Elements(yticks) NE 0 THEN *self.yticks = yticks
    IF N_Elements(ytickunits) NE 0 THEN *self.ytickunits = ytickunits
    IF N_Elements(ytickv) NE 0 THEN *self.ytickv = ytickv 
    IF N_Elements(ytitle) NE 0 THEN *self.ytitle = ytitle

    IF N_Elements(zcharsize) NE 0 THEN *self.zcharsize = zcharsize
    IF N_Elements(zgridstyle) NE 0 THEN *self.zgridstyle = zgridstyle
    IF N_Elements(zmargin) NE 0 THEN *self.zmargin = zmargin
    IF N_Elements(zminor) NE 0 THEN *self.zminor = zminor
    IF N_Elements(zrange) NE 0 THEN *self.zrange = zrange
    IF N_Elements(zstyle) NE 0 THEN *self.zstyle = zstyle
    IF N_Elements(zthick) NE 0 THEN *self.zthick = zthick
    IF N_Elements(ztickformat) NE 0 THEN *self.ztickformat = ztickformat
    IF N_Elements(ztickinterval) NE 0 THEN *self.ztickinterval = ztickinterval
    IF N_Elements(zticklayout) NE 0 THEN *self.zticklayout = zticklayout
    IF N_Elements(zticklen) NE 0 THEN *self.zticklen = zticklen
    IF N_Elements(ztickname) NE 0 THEN *self.ztickname = ztickname
    IF N_Elements(zticks) NE 0 THEN *self.zticks = zticks
    IF N_Elements(ztickunits) NE 0 THEN *self.ztickunits = ztickunits
    IF N_Elements(ztickv) NE 0 THEN *self.ztickv = ztickv 
    IF N_Elements(ztitle) NE 0 THEN *self.ztitle = ztitle
    IF N_Elements(zvalue) NE 0 THEN *self.zvalue = zvalue

END

;+
; This is the object class definition for the cgGraphicsKeywords object class.
; Normally, this class serves as the superclass for Coyote Graphics graphics 
; objects that need graphics keyword support.
;-
PRO cgGraphicsKeywords__Define, class

   class = {cgGraphicsKeywords, $
              AXISCOLOR: Ptr_New(), $
              BACKGROUND: Ptr_New(), $
              CHARSIZE: Ptr_New(), $
              CHARTHICK: Ptr_New(), $
              CLIP: Ptr_New(), $
              COLOR: Ptr_New(), $
              DATA: Ptr_New(), $
              DEVICE: Ptr_New(), $
              NORMAL: Ptr_New(), $
              FONT: Ptr_New(), $
              LINESTYLE: Ptr_New(), $
              NOCLIP: Ptr_New(), $
              NODATA: Ptr_New(), $
              NOERASE: Ptr_New(), $
              POSITION: Ptr_New(), $
              PSYM: Ptr_New(), $
              SUBTITLE: Ptr_New(), $
              SYMSIZE: Ptr_New(), $
              T3D: Ptr_New(), $
              THICK: Ptr_New(), $
              TICKLEN: Ptr_New(), $
              TITLE: Ptr_New(), $
              XCHARSIZE: Ptr_New(), $
              XGRIDSTYLE: Ptr_New(), $
              XMARGIN: Ptr_New(), $
              XMINOR: Ptr_New(), $
              XRANGE: Ptr_New(), $
              XSTYLE: Ptr_New(), $
              XTHICK: Ptr_New(), $
              XTICK_GET: Ptr_New(), $
              XTICKFORMAT: Ptr_New(), $
              XTICKINTERVAL: Ptr_New(), $
              XTICKLAYOUT: Ptr_New(), $
              XTICKLEN: Ptr_New(), $
              XTICKNAME: Ptr_New(), $
              XTICKS: Ptr_New(), $
              XTICKUNITS: Ptr_New(), $
              XTICKV: Ptr_New(), $
              XTITLE: Ptr_New(), $

              YCHARSIZE: Ptr_New(), $
              YGRIDSTYLE: Ptr_New(), $
              YMARGIN: Ptr_New(), $
              YMINOR: Ptr_New(), $
              YRANGE: Ptr_New(), $
              YSTYLE: Ptr_New(), $
              YTHICK: Ptr_New(), $
              YTICK_GET: Ptr_New(), $
              YTICKFORMAT: Ptr_New(), $
              YTICKINTERVAL: Ptr_New(), $
              YTICKLAYOUT: Ptr_New(), $
              YTICKLEN: Ptr_New(), $
              YTICKNAME: Ptr_New(), $
              YTICKS: Ptr_New(), $
              YTICKUNITS: Ptr_New(), $
              YTICKV: Ptr_New(), $
              YTITLE: Ptr_New(), $
              
              ZCHARSIZE: Ptr_New(), $
              ZGRIDSTYLE: Ptr_New(), $
              ZMARGIN: Ptr_New(), $
              ZMINOR: Ptr_New(), $
              ZRANGE: Ptr_New(), $
              ZSTYLE: Ptr_New(), $
              ZTHICK: Ptr_New(), $
              ZTICK_GET: Ptr_New(), $
              ZTICKFORMAT: Ptr_New(), $
              ZTICKINTERVAL: Ptr_New(), $
              ZTICKLAYOUT: Ptr_New(), $
              ZTICKLEN: Ptr_New(), $
              ZTICKNAME: Ptr_New(), $
              ZTICKS: Ptr_New(), $
              ZTICKUNITS: Ptr_New(), $
              ZTICKV: Ptr_New(), $
              ZTITLE: Ptr_New(), $
              
              ZVALUE: Ptr_New() $
              }
END