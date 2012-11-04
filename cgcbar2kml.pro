; docformat = 'rst'
;
; NAME:
;   cgCBar2KML
;
; PURPOSE:
; This program creates a KML file that can be opened in Google Earth to display a
; color bar as a ScreenOverlay. Screen overlays are images that are displayed in 
; a fixed location on the Google Earth display. A corresponding color bar image file is 
; produced. The KML and color bar image file must be in the same directory to use them with
; Google Earth.
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
; This program creates a KML file that can be opened in Google Earth to display a
; color bar as a ScreenOverlay. Screen overlays are images that are displayed in 
; a fixed location on the Google Earth display. A corresponding color bar image file is 
; produced. The KML and color bar image file must be in the same directory to use them with
; Google Earth. In general, any keyword used for horizontal color bars in cgColorbar can
; be used with this program. The colorbar image is made from a PostScript intermediate file, which 
; means ImageMagick must be installed on your computer and available to IDL to run this program 
; successfully. Please see `IDL Output for Web Display <http://www.idlcoyote.com/graphics_tips/weboutput.php>`
; for details.
; 
; .. image:: cgcbar2kml.png
; 
; :Categories:
;    Graphics, FileIO, Maps
;    
; :Keywords:
;    addtofile: in, optional, type=object
;       If this keyword contains a cgKML_File object, the image is added to the file
;       as a <ScreenOverlay> element and a separate KML file is not created. In other
;       words, the `Filename` keyword is ignored and the image file created takes its
;       name from the cgKML_File object.
;    background: in, optional, type=string, default='gray'
;       The background color for the color bar.
;    bottom: in, optional, type=integer, default=0
;       The lowest color index of the colors to be loaded in the color bar.
;    brewer: in, optional, type=boolean, default=0
;         This keyword is used only if the `CTIndex` keyword is used to select a color table number.
;         Setting this keyword allows Brewer color tables to be used.
;    charpercent: in, optional, type=float, default=0.85                 
;       A value from 0.0 go 1.0 that is multiplied by the CHARSIZE to produce
;       the character size for the color bar. This value is only used if CHARSIZE is 
;       undefined. This keyword is primarily useful for using color bars in resizeable 
;       graphics windows (cgWindow).
;    charsize: in, optional, type=float
;       The character size of the color bar annotations. Default is cgDefCharsize()*charPercent.
;    clamp: in, optional, type=float
;        A two-element array in data units. The color bar is clamped to these
;        two values. This is mostly of interest if you are "window-leveling"
;        an image. The clamp is set to the "window" of the color bar.
;        Normally, when you are doing this, you would like the colors outside
;        the "window" to be set to a neutral color. Use the NEUTRALINDEX keyword
;        to set the netural color index in the color bar. (See the Examples section
;        for more information.)
;    color: in, optional, type=string
;        The name of the color to use for color bar annotations. Ignored unless passed 
;        the name of a cgColor color. The default value is to use the ANNOTATECOLOR.
;    ctindex: in, optional, type=integer
;         The index number of a color table. The `Brewer` and `Reverse` keywords will be checked
;         to see how to load the color table into the `Palette` keyword. This keyword will take
;         precidence over any colors that are loaded with the `Palette` keyword. 
;    description: in, optional, type=string
;       A string that is used to describe the image in the Google Earth interface.
;    discrete: in, optional, type=boolean, default=0
;         Set this keyword to configure certain properties of the color bar to make
;         discrete color blocks for the color bar. This works best if you are using
;         a handful of colors in the color bar (e.g, 8-16).
;    divisions: in, optional, type=integer
;         The number of divisions to divide the bar into. There will
;         be (divisions + 1) annotations. The default is 0 if using the
;         default color bar formatting, which allows the plot command to 
;         determine how many divisions to make. Otherwise, if you are specifying
;         some other format for the tick labels, the default number of divisions
;         is six.
;    draworder: in, optional, type=integer, default=0
;        The drawing order of image overlay. The first order is 0. Images with a higher
;        order are drawn on top of images with a lower order.
;    filename: in, optional, type=string, default='kml_cbimage.kml'
;        The name of the KML file that will be created. The image file will have the same name,
;        but with a *.png file extension. The KML file and the image file will be created in the
;        same directory.
;    format: in, optional, type=string, default=""
;       The format of the color bar annotations. Default is "". Note that the
;       formatting behaviour can change, depending up values for the keywords
;       `RANGE` and `DIVISIONS`. If you prefer to let the IDL Plot command determine
;       how the color bar labels are formatted, set the format to a null string and
;       set the `DIVISIONS` keyword to 0. Note the difference in these two commands::
;       
;           cgColorbar, Range=[18,125], Position=[0.1, 0.8, 0.9, 0.85]
;           cgColorbar, Range=[18,125], Position=[0.1, 0.7, 0.9, 0.75], Divisions=0
;    location: in, optional, type=intarr
;       A two-element array giving the location of the top-left corner of the
;       color bar in normalized coordinates from the upper-left of the Google Earth
;       display screen. Default is [0.025, 0.975].
;    maxrange: in, optional
;       The maximum data value for the color bar annotation. Default is NCOLORS.
;    minrange: in, optional, type=float, default=0.0
;       The minimum data value for the bar annotation. 
;    minor: in, optional, type=integer, default=2
;       The number of minor tick divisions. 
;    ncolors: in, optional, type=integer, default=256
;       This is the number of colors in the color bar.
;    neutralindex: in, optional, type=integer   
;       This is the color index to use for color bar values outside the
;       clamping range when clamping the color bar with the CLAMP keyword.
;       If this keyword is absent, the highest color table value is used
;       for low range values and the lowest color table value is used
;       for high range values, in order to provide contrast with the
;       clamped region. (See the Examples section for more information.)
;    oob_factor: in, optional, type=float, default=0.2
;       The default is to make the length of the out-of-bounds triangle the
;       same distance as the height (or width, in the case of a vertical
;       color bar) of the color bar. If you would prefer a shorted triangle length, 
;       set this keyword to a value less than zero (e.g., 0.5). If you prefer a 
;       longer length, set this keyword to a value greater than zero. The "standard"
;       length will be multiplied by this value.
;    oob_high: in, optional, type=string
;       The name of an out-of-bounds high color. This color will be represented
;       by a triangle on the right or top of the color bar. If the color is
;       a string byte value (e.g., "215"), then this color in the current color
;       table is used. The color can also be a three-element color triple 
;       (e.g., [240, 200, 65]).
;    oob_low: in, optional, type=string
;       The name of an out-of-bounds low color. This color will be represented
;       by a triangle on the left or bottom of the color bar. If the color is
;       a string byte value (e.g., "215"), then this color in the current color
;       table is used. The color can also be a three-element color triple 
;       (e.g., [240, 200, 65]).
;    palette: in, optional, type=byte
;       A color palette containing the RGB color vectors to use for the color
;       bar. The program will sample NCOLORS from the color palette. 
;    placename: in, optional, type=string, default='Color Bar'
;        This is the <name> element in a Feature object. It is user-defined text that is used as
;        the label for an object in Google Earth.
;    range: in, optional, type=float
;       A two-element vector of the form [min, max]. Provides an alternative 
;       and faster way way of setting the MINRANGE and MAXRANGE keywords.
;    reverse: in, optional, type=boolean, default=0
;       An alternative keyword name (one I can actually remember!) for the INVERTCOLORS keyword.
;       It reverses the colors in the color bar.
;    tickinterval: in, optional, type=float
;       Set this keyword to the interval spacing of major tick marks. Use this keyword in
;       place of XTickInterval or YTickInterval keywords.
;    ticklen: in, optional, type=float, default=0.25
;       Set this keyword to the major tick length desired. Default is 0.25. Setting this 
;       keyword to a value greater than or equal to 0.5 will result in major tick marks 
;       extending the width of the color bar. Note that setting this keyword to 0.3 or
;       greater will result in minor tick mark lengths being set to 0.01, which is almost 
;       too small to be seen. All direct graphics tick marks act in this (strange!) way.
;    ticknames: in, optional, type=string                 
;       A string array of names or values for the color bar tick marks.
;    title: in, optional, type=string, default=""
;       This is title for the color bar. The default is to have no title.
;    tcharsize: in, optional, type=float
;       The title size. By default, the same as `Charsize`. Note that this keyword is
;       ignored for vertical color bars unless the title location (`TLocation`) is on
;       the opposite side of the color bar from the color bar labels. This is a consequence
;       of being upable to determine the length of color bar labels programmatically in this
;       orientation.
;    width: in, optional, type=integer, default=300
;       The width, in pixels, of the colorbar image that is created.
;    xlog: in, optional, type=boolean, default=0
;       Set this keyword to use logarithmic scaling for the colorbar data range.
;    xtickinterval: in, optional, type=float
;       This keyword is trapped, but unused. Please use the`TickInterval` keyword instead.
;    xtitle: in, optional, type=string
;        This keyword is ignored. Use the `Title` keyword to set a title for the color bar.
;    _ref_extra: in, optional
;         Any keyword appropriate KML screen overlay objects is allowed.
;
; :Examples:
;    Here is how you can put an AVHRR NDVI image of Africa on a Google Earth display
;    with a color bar:: 
;    
;       ;; Download the image file from the Coyote web page.
;       netObject = Obj_New('IDLnetURL')
;       url = 'http://www.idlcoyote.com/data/AF03sep15b.n16-VIg.tif'
;       returnName = netObject -> Get(URL=url, FILENAME='AF03sep15b.n16-VIg.tif')
;       Obj_Destroy, netObject
;       
;       ;; Create the image overlay KML file.
;       kmlFile = Obj_New('cgKML_File', 'avhrr_ndvi_cb.kml')
;       cgLoadCT, 11, /Brewer, /Reverse, RGB_Table=palette
;       map = cgGeoMap('AF03sep15b.n16-VIg.tif', Image=image)
;       scaledImage = BytScl(image > 0)
;       cgImage2KML, scaledImage, map, $
;          Palette=palette, Missing_Value=0, $
;          Description='AVHRR NDVI Data from Africa', $
;          PlaceName='AVHRR Africa', $
;          AddToFile=kmlFile
;       cgCBar2KML, Palette=palette, Range=[0,9400], $
;          Title='NDVI Index', $
;          Description='AVHRR NDVI Color Bar', $
;          PlaceName='NDVI Color Bar', $
;          AddToFile=kmlFile
;       kmlFile -> Save
;       kmlFile -> Destroy
;          
;       ;; Start Google Earth and open the KML file you just created.
;       
;    The output should look like the figure above.
;    
;    If you just wish to create a KML file with a color bar, you can do this::
;    
;       cgCBar2KML, Filename='colorbar.kml', CTIndex=5, Title='Test Color Bar'
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 30 October 2012 by David W. Fanning.
;        Added DRAWORDER keyword and fixed a typo concerning MISSING_VALUE. 31 Oct 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
PRO cgCBar2KML, $
    ADDTOFILE=addtofile, $
    BACKGROUND=background, $
    BOTTOM=bottom, $
    BREWER=brewer, $
    CHARPERCENT=charpercent, $
    CHARSIZE=charsize, $
    CLAMP=clamp, $
    COLOR=color, $
    CTINDEX=ctindex, $
    DESCRIPTION=description, $
    DISCRETE=discrete, $
    DIVISIONS=divisions, $
    DRAWORDER=draworder, $
    FILENAME=filename, $
    FORMAT=format, $
    LOCATION=location, $
    MAXRANGE=maxrange, $
    MINOR=minor, $
    MINRANGE=minrange, $
    NCOLORS=ncolors, $
    NEUTRALINDEX=neutralIndex, $
    OOB_FACTOR=oob_factor, $
    OOB_HIGH=oob_high, $
    OOB_LOW=oob_low, $
    PALETTE=palette, $
    PLACENAME=placename, $
    RANGE=range, $
    REVERSE=reverse, $
    TCHARSIZE=tcharsize, $
    TICKINTERVAL=tickinterval, $
    TICKLEN=ticklen, $
    TICKNAMES=ticknames, $
    TITLE=title, $
    XLOG=xlog, $
    XTICKINTERVAL=xtickinterval, $ ; Ignored.
    XTITLE=xtitle, $ ; Ignored.
    WIDTH=width, $
    _REF_EXTRA=extra
    
    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       RETURN
    ENDIF 
    
    ; Default values.
    IF N_Elements(background) EQ 0 THEN background = 'gray'
    IF N_Elements(location) EQ 0 THEN location = [0.025, 0.975]
    IF N_Elements(placename) EQ 0 THEN placename = 'Color Bar'
    IF N_Elements(oob_factor) EQ 0 THEN oob_factor = 0.2
    IF N_Elements(charsize) EQ 0 THEN charsize=1.75
    IF N_Elements(tcharsize) EQ 0 THEN tcharsize = 2.0
    IF N_Elements(title) EQ 0 THEN BEGIN
        position = [0.1, 0.425, 0.9, 0.775]
    ENDIF ELSE BEGIN
        position = [0.1, 0.525, 0.9, 0.875]
    ENDELSE
    
    ; Need a filename?
    IF N_Elements(filename) EQ 0 THEN BEGIN
       CD, CURRENT=thisDir
       filename = Filepath(ROOT_DIR=thisDir, 'kml_cbimage.kml')
    ENDIF
   
    ; Construct the image filename
    rootName = cgRootName(filename, DIRECTORY=thisDir, EXTENSION=ext)
    IF StrUpCase(ext) NE 'KML' THEN Message, 'The output filename must have a KML file extension.'
    imageFilename = Filepath(ROOT_DIR=thisDir, rootname + '.png')
    
   IF cgObj_Isa(addtofile, 'cgKML_File') THEN BEGIN
   
      addToFile -> GetProperty, FILENAME=filename, COUNT=count
      rootname = cgRootName(filename, DIRECTORY=thisDir, EXTENSION=ext)
      imageFilename = FilePath(ROOT_DIR=thisDir, rootname + StrTrim(count+1,2) + '.png')
      
     ; Write the image file.
     rootname = cgRootName(imageFilename, DIRECTORY=thisDir)
     psFileName = Filepath(ROOT_DIR=thisDir, rootname + '.ps')
     PS_Start, psFilename, /Quiet
     cgDisplay, 1000, 200
     cgColorFill, [0,1,1,0,0], [0,0,1,1,0], /Normal, Color=background
     cgColorbar, $
        BOTTOM=bottom, $
        BREWER=brewer, $
        CHARPERCENT=charpercent, $
        CHARSIZE=charsize, $
        CLAMP=clamp, $
        COLOR=color, $
        CTINDEX=ctindex, $
        DISCRETE=discrete, $
        DIVISIONS=divisions, $
        FORMAT=format, $
        MAXRANGE=maxrange, $
        MINOR=minor, $
        MINRANGE=minrange, $
        NCOLORS=ncolors, $
        NEUTRALINDEX=neutralIndex, $
        OOB_FACTOR=oob_factor, $
        OOB_HIGH=oob_high, $
        OOB_LOW=oob_low, $
        PALETTE=palette, $
        POSITION=position, $
        RANGE=range, $
        REVERSE=reverse, $
        TCHARSIZE=tcharsize, $
        TICKINTERVAL=tickinterval, $
        TICKLEN=ticklen, $
        TICKNAMES=ticknames, $
        TITLE=title, $
        XLOG=xlog
     PS_End, /PNG, /DELETE_PS, WIDTH=width, /NoMessage
     
     ; Create the screen overlay object if a raster file was created.
     IF File_Test(imageFilename) THEN BEGIN
       overlay = Obj_New('cgKML_ScreenOverlay', $
            HREF=imageFilename, $
            DESCRIPTION=description, $
            DRAWORDER=draworder, $
            SCREEN_XY=location, $
            PLACENAME=placename)
        addToFile -> Add, overlay
      ENDIF
   
   ENDIF ELSE BEGIN
   
     ; Write the image file.
     rootname = cgRootName(imageFilename, DIRECTORY=thisDir)
     psFileName = Filepath(ROOT_DIR=thisDir, rootname + '.ps')
     PS_Start, psFilename, /Quiet
     cgDisplay, 1000, 200
     cgColorFill, [0,1,1,0,0], [0,0,1,1,0], /Normal, Color=background
     cgColorbar, $
        BOTTOM=bottom, $
        BREWER=brewer, $
        CHARPERCENT=charpercent, $
        CHARSIZE=charsize, $
        CLAMP=clamp, $
        COLOR=color, $
        CTINDEX=ctindex, $
        DISCRETE=discrete, $
        DIVISIONS=divisions, $
        FORMAT=format, $
        MAXRANGE=maxrange, $
        MINOR=minor, $
        MINRANGE=minrange, $
        NCOLORS=ncolors, $
        NEUTRALINDEX=neutralIndex, $
        OOB_FACTOR=oob_factor, $
        OOB_HIGH=oob_high, $
        OOB_LOW=oob_low, $
        PALETTE=palette, $
        POSITION=position, $
        RANGE=range, $
        REVERSE=reverse, $
        TCHARSIZE=tcharsize, $
        TICKINTERVAL=tickinterval, $
        TICKLEN=ticklen, $
        TICKNAMES=ticknames, $
        TITLE=title, $
        XLOG=xlog
     PS_End, /PNG, /DELETE_PS, WIDTH=width, /NoMessage
   
     ; Write the KML file.
     kmlFile = Obj_New('cgKML_File', filename)
     
     ; Create the screen overlay object if a raster file was created.
     IF File_Test(imageFilename) THEN BEGIN
       overlay = Obj_New('cgKML_ScreenOverlay', $
            HREF=imageFilename, $
            DESCRIPTION=description, $
            DRAWORDER=draworder, $
            SCREEN_XY=location, $
            PLACENAME=placename)
        kmlFile -> Add, overlay
      ENDIF
     kmlFile -> Save
     kmlFile -> Destroy
     
   ENDELSE

END    