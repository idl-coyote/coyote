; docformat = 'rst'
;
; NAME:
;   cgcgTeminatorMap
;
; PURPOSE:
;   This is a program for viewing a map of the Earth in several different projections
;   with a day/night terminator.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2011, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
;   This is a program for viewing a map of the Earth in several different projections
;   with a day/night terminator. Both the `Coyote Library <http://www.idlcoyote.com/code_tips/installcoyote.php>`
;   and the `JHUAPL IDL Library <http://fermi.jhuapl.edu/s1r/idl/s1rlib/local_idl.html>`
;   are required to run the program.   
;
;   The spherical geometry problems inherent with working the map projections are not
;   completely solved in this program. The main problem seems to be that if the
;   terminator polygon (represented in the program by the variables lon_terminus
;   and lat_terminus) overlaps the pole regions, then stardard methods for determining
;   if a point is inside or outside a polygon are not reliable. (*All* longitudes are
;   inside the polygon if the polygon overlaps a pole!) In the program, this manifests
;   itself as the sun side of the terminator appearing dark and the night side appearing
;   light. Hence, the FLIPDAY keyword. 
;
;   To some extent, this problem can be eliminated by setting the CENTER_LAT parameter
;   to zero, at least with some map projections. I've thought about forcing it to be
;   zero and forcing you to use well-behaved map projections, but have decided to leave
;   the program the way it is so you can discover the limitations yourself.
;
; :Categories:
;    Graphics, Map Projections
;    
; :Params:
;    center_lon: in, optional, type=float, default=0.0
;       The longitude, in degrees, of the center of the map, (-180 to 180).
;    center_lat: in, optional, type=float, default=0.0
;       The latitude, in degrees, of the center of the map, (-90 to 90).
;       Latitudes other than zero can often cause problems with the terminator
;       polygon "spilling over" into the wrong areas of the map.
;       
; :Keywords:
;     flipday: in, optional, type=boolean, default=0
;        The program suffers from an inability to always correctly distinguish the
;        "day" side of the day/night terminator. If the program
;        gets it wrong, you can use this keyword to "flip" the day/night area. For
;        example, here is a call that gets it wrong:
;
;        cgTeminatorMap, 0, 90.0, MAP_PROJECTION=4, time="Thu Apr 13 08:49:34 2006"
;
;        Unfortunately, you can only know if it is "wrong" by visual inspection. The sun
;        is always placed correctly. I realize this limits the usefulness of the program,
;        generally, but it is a limitation I am not motivated enough to fix. That is to
;        say, the program meets my limited needs perfectly. :-)
;     image: out, optional, type=byte
;        Set this keyword to a named variable to return a 24-bit color image of the
;        final result.
;     map_projection: in, optional, type=varies, default=0
;        Either the number or the name of one of the following map projection types. 
;        
;        0 CYLINDRICAL
;        1 GOODESHOMOLOSINE
;        2 HAMMER
;        3 LAMBERT
;        4 MERCATOR
;        5 MILLER_CYLINDRICAL
;        6 MOLLWEIDE
;        7 ROBINSON
;        
;     outfilename: in, optional, type=string, default="cgTeminatorMap.ps"
;        The name of the output filename. Used only if the PS or PNG keywords
;        are set.
;     png: in, optional, type=boolean, default=0
;        If this keyword is set, a PNG file is created from a PostScript intermediary
;        file. ImageMagick must be installed for this option not to cause an error.
;     ps: in, optional, type=boolean, default=0
;        Set this keyword to create a PostScript file of the terminator image.
;     time: in, optional, type=string
;        A date/time string of nearly any format. The month, however, should be represented
;        as a three-letter string. The time is written as hh:mm:ss. Examples of date/time strings
;        are these:
;
;        'Wed Apr 12 22:39:52 2006'
;        '12 APR 2006 04:16:00'
;        'Apr 12, 2006 04:16:00'
;        'Nov 12, 2006'
;
;        If not specified, the current day and time (from SYSTIME) is used.
;          
; :Examples:
;    To see a single day passing::
;
;        XInterAnimate, Set=[800,400,24], /ShowLoad, Title='Wednesday Nov 16, 2011'
;        FOR j=0,23 DO BEGIN
;           cgTeminatorMap, Time = "Wed Nov 16 " + String(j, Format='(I2.2)') + ":00:00 2011"
;           XInterAnimate, Frame=j, Window=!D.Window
;        ENDFOR
;        XInterAnimate, 20
;        END
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
;       Written by David W. Fanning, April 12, 2006.
;       Updated to use Coyote Graphics, 6 September 2011. DWF.
;       Algorithm reworked and improved. 13 November 2011. DWF.
;       The map projection order seems to have gotten itself scrambled. Fixed. 1 March 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2006-2011, Fanning Software Consulting, Inc.
;-
PRO cgTeminatorMap, center_lon, center_lat, $
   FlipDay=flipday, $
   Image=image, $
   Map_Projection=map_index, $
   OutFilename = outfilename, $
   PNG=png, $
   PS=ps, $
   Time=time

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      IF N_Elements(thisDevice) NE 0 THEN Set_Plot, thisDevice
      RETURN
   ENDIF
   
   ; Check keywords.

   mapProjections = ['Cylindrical', 'Goodeshomolosine', 'Hammer', $
      'Lambert', 'Mercator', 'Miller_Cylindrical', 'Mollweide', 'Robinson']

   ; Check arguments.
   IF N_Elements(center_lon) EQ 0 THEN center_lon = 0
   IF N_Elements(center_lat) EQ 0 THEN center_lat = 0
   png = Keyword_Set(png)
   ps = Keyword_Set(ps)
   IF N_Elements(time) EQ 0 THEN time = Systime()
   IF N_Elements(map_index) EQ 0 THEN map_index=0 ELSE BEGIN
      IF Size(map_index, /TNAME) NE 'STRING' THEN BEGIN
         map_index = 0 > map_index < 7
      ENDIF ELSE BEGIN
         index = Where(StrMid(StrUpCase(mapProjections), 0, $
     StrLen(map_index)) EQ StrUpCase(map_index), count)

         IF count EQ 0 THEN BEGIN
            Message, 'Cannot find map projection: ' + map_index +    '. Using CYLINDRICAL.', /Informational
            map_index = 0
         ENDIF ELSE map_index = index
      ENDELSE
   ENDELSE
   
   ; Set aspect ratio for various map projections
   CASE map_index OF
         3:    BEGIN
               win_xsize = 800 & win_ysize = 800
               END
         4:    BEGIN
               win_xsize = 800 & win_ysize = Round(800 * 2 / 3.)
               END
         6:    BEGIN
               win_xsize = 800 & win_ysize = Round(800 * 2 / 3.)
               END
         ELSE: BEGIN
               win_xsize = 800 & win_ysize = 400
               END
   ENDCASE
   position = [0., 0., 1., 1.]
    
   ; Open a window if one is not currently open. If you are making
   ; a PostScript file, this will have to be a pixmap.
   IF (!D.Window LT 0) && ~(ps || png) THEN BEGIN
        cgDisplay, win_xsize, win_ysize, /Free, Title=time
        displayWindow = !D.Window
   ENDIF ELSE BEGIN
        IF (ps || png) THEN BEGIN
            cgDisplay, win_xsize, win_ysize, /Free, /Pixmap
            displayWindow = !D.Window
            ps_pixmap = 1
        ENDIF
   ENDELSE
   displayWindow = !D.Window

   ; Set up the map projection space.
   CASE map_index OF
      0: cgMap_Set, center_lat, center_lon, 0, Position=position, /NoErase, /NoBorder, /CYLINDRICAL
      1: cgMap_Set, center_lat, center_lon, 0, Position=position, /NoErase, /NoBorder, /GOODESHOMOLOSINE
      2: cgMap_Set, center_lat, center_lon, 0, Position=position, /NoErase, /NoBorder, /HAMMER
      3: cgMap_Set, center_lat, center_lon, 0, Position=position, /NoErase, /NoBorder, /LAMBERT
      4: cgMap_Set, center_lat, center_lon, 0, Position=position, /NoErase, /NoBorder, /MERCATOR
      5: cgMap_Set, center_lat, center_lon, 0, Position=position, /NoErase, /NoBorder, /MILLER_CYLINDRICAL
      6: cgMap_Set, center_lat, center_lon, 0, Position=position, /NoErase, /NoBorder, /MOLLWEIDE
      7: cgMap_Set, center_lat, center_lon, 0, Position=position, /NoErase, /NoBorder, /ROBINSON
   ENDCASE

   ; Obtain the AVHRR image, along with its color table vectors, to display.
   filename = Filepath(SubDirectory=['examples', 'data'], 'avhrr.png')
   image = Read_PNG(filename, r, g, b)

   ; Find the latitude and longitude of the sub-solar point at the
   ; time the user specified. First, sort out the time values.
   time_copy = time
   offset = GMT_OFFSEC()                      ; Find correction from local time to Universal Time (UT).
   DT_TM_INC, time_copy, offset               ; Convert time to UT.
   DT_TM_BRK, time_copy, date_part, time_part ; Break into a date and a time part.
   DATE2YMD, date_part, y, m, d               ; Break data into year, month, and day.
   jd = YMD2JD(y, m, d)                       ; Convert date to julian day.
   ut = SECSTR(time_part)/3600.               ; Convert time string (in seconds) to sun ephemeris time in hours.

    ; Calculate solar RA and DEC and distance (in AU).
   SUN, y, m, d, ut, $
      APP_RA=ra, $           ; Apparent right accession.
      APP_DEC=dec, $         ; Apparent declination.
      DIST=sun_distance      ; Distance to sun in AU.

   ; Calculate local mean sidereal time. LMST returns value as fraction of a day,
   lm_sidereal_time = LMST(jd, ut/24.0, 0) * 24

   ; Calculate sub-solar point.
   sun_lat = dec
   sun_lon = 15.0 * (ra - lm_sidereal_time)
   
   ; Calculate the terminus.
   earthRadius = 6.371009D6 ; The mean Earth radius.
   scanAngle = ASIN(earthRadius / (sun_distance * 1.4956D11))
   arc_distance = !DPI - 1.57080D - scanAngle  ; 90 degrees - scanangle (but here in radians)

   lon_terminus = FltArr(36)
   lat_Terminus = FltArr(36)
   FOR j=0,35 DO BEGIN
      count = j
      IF Keyword_Set(flipday) THEN count = -count
      azimuth =  count * 10.0 
      results = LL_Arc_Distance([sun_lon, sun_lat], arc_distance, azimuth, /Degrees)
      
      ; You have to add the center longitude to the result here. I don't know why!!
      lon_terminus[j] = results[0] + center_lon
      lat_terminus[j] = results[1]
   ENDFOR
   
   ; We are going to do the smoke and mirrors thing in a pixmap.
   thisDevice = !D.Name   
   IF (!D.Name NE 'X') && (!D.Name NE 'WIN') THEN BEGIN
       IF StrUpCase(!Version.OS_Family) EQ 'WINDOWS' THEN Set_Plot, 'WIN' ELSE Set_Plot, 'X'
   ENDIF
   
   ; Create a window that matches the current window in size.
   cgDisplay, win_xsize, win_ysize, /Free, /Pixmap
   pixwindow = !D.Window
   
   ; Load the color table and display the image.
   TVLCT, r, g, b
   cgImage, image, Position=position
   
   ; Take a snapshot of the window to create a 24-bit image.
   imgsnap = cgSnapshot(TRUE=3)
   
   ; In the same window, fill in the night terminus with a black color.
   cgColorFill, lon_terminus, lat_terminus, Color='black'
   
   ; Take a snap shot of this. Half the resulting image is of the
   ; original image, and half the image is black.
   darksnap = cgSnapshot(TRUE=3)
   
   ; Delete the pixmap window.
   WDelete, pixWindow
   
   ; Back to the current graphics device.
   Set_Plot, thisDevice
   
   ; Now blend the two snapshots at 50% of each. This will give the
   ; night sky look to the terminus.
   alpha=0.5
   terminatorImage = (imgsnap * alpha) + (1 - alpha) * darksnap
   
   ; Make the display window the current window
   IF displayWindow GE 0 THEN WSet, displayWindow
   
   ; Copy the terminator image into a warp image variable. You will replace
   ; the warp contents in a moment.
   warp = terminatorImage
   
   ; Now warp the image into the map projection. You have to do this plane by plane,
   ; as only 2D images are allowed.
   FOR j=0,2 DO BEGIN
      warp[*,*,j] = Map_Image(terminatorImage[*,*,j], Compress=1, Missing=255) ; White pixels
   ENDFOR
   
   ; Need a PostScript file? Make sure you have a PostScript file extension.
   IF N_Elements(outfilename) EQ 0 THEN outfilename = 'cgTeminatorMap.ps'
   rootname = FSC_Base_Filename(outfilename, EXTENSION=ext, DIRECTORY=dir)
   IF (StrUpCase(ext) EQ "PNG") && png THEN BEGIN
        outfilename = FilePath(ROOT_DIR=dir, rootname + '.' + 'ps')
   ENDIF
   
   ; Set up the PostScript file.
   IF (ps || png) THEN PS_Start, Filename=outfilename

   ; Display the image.
   cgImage, warp, Position=position

   ; Plot the sun on the map.
   cgPlotS, sun_lon, sun_lat, psym=SymCat(16), color='yellow', symsize=3

   ; Add continental outlines and time zones.
   cgMap_Continents, Color='Medium Gray'
   cgMap_Continents, Color='Medium Gray', /Countries
   cgMap_Grid, Color='charcoal', Lons=Scale_Vector(Indgen(25), -180, 180), LineStyle=5, $
      LonNames = Reverse(StrTrim(Indgen(25),2)), LonLabel=-10, /Label, LColor='Gray', $
      LatLabel=-7.5
      
   ; Close the PostScrit file and create a PNG file, if required.
   IF (ps || png) THEN PS_End, PNG=png, DELETE_PS=(1-ps)
      
   ; Need output image?
   IF Arg_Present(image) THEN image = cgSnapshot()
   
   ; Need to clean up the PostScript pixmap?
   IF N_Elements(ps_pixmap) THEN WDelete, displayWindow
   
END