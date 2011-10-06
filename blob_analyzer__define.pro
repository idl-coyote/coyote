;+
; NAME:
;       BLOB_ANALYZER__DEFINE
;
; PURPOSE:
; 
;       The purpose of this routine is to create a system for analyzing
;       regions of interest (ROIs) or (more commonly) "blobs" inside images.
;       In particular, given a suitable image (this will require judgement on
;       your part), the program will automatically select "blobs" or connected
;       regions in the image and make it possible for you to analyze these
;       blobs. An example program is provided to show you one way the program
;       can be used.
;       
;       The code is a wrapper, essentially, for LABEL_REGION and HISTOGRAM, with
;       a couple of my other image processing routines (FIND_BOUNDARY and FIT_ELLIPSE)
;       used in a supporting role.
;
; AUTHOR:
; 
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:
; 
;       Analysis, Image Processing
;
; CALLING SEQUENCE:
; 
;       analyzer = Obj_New("BLOB_ANALYZER", image)
;
; INPUTS:
; 
;   image:           A two-dimensional image array. To make this program memory efficient,
;                    a copy of the image is *not* stored in the object. You will be responsible
;                    for image operations outside this program.
;
; KEYWORDS:
;
;   ALL_NEIGHBORS:    Set this keyword to look at all eight neighbors when searching
;                     for connectivity. The default is to look for four neighbors on
;                     each side of the starting pixel. Passed directly to LABEL_REGION.
;                     
;   MASK:             A two-dimensional array, the same size as image, that identifies the
;                     foreground and background pixels in the image. Applying the mask
;                     should result in a bi-level image of 0s and 1s, where 1 identifies the 
;                     blobs you wish to analyze. If a mask is not provided, the mask is created
;                     like this:
;                     
;                     mask = image > 0
;
;   SCALE:            A one- or two-dimensional given the pixel scaling parameters. By default [1.0, 1.0].
;                     If passed a scalar, the scale parameter is applied to both the X and Y directions of
;                     each pixel. Statistical output is reported with scaling unless the NOSCALE keyword
;                     is set. Scaling also effects the data that is output from the various methods.
;
; OBJECT METHODS:
; 
;   The following methods are provided. Please see the documentation header for each method for
;   information on arguments and keywords that can be used with the method.
;
;   FitEllipse:       This method fits an ellipse to the blob. It returns information about the fitted
;                     ellipse, including the points that all the ellipse to be drawn.
;                     
;   GetIndices:       This method returns the indices for a particular blob in the image.
;   
;   GetStats:         This method returns a structure containing statistics for a particular blob in the image.
;                     The structure is defined as follows:
;                     
;                     stats = {INDEX: indexNumber, $                  ; The index number of this blob.
;                              COUNT: N_Elements(indices), $          ; The number of indices in this blob.
;                              PERIMETER_PTS: boundaryPts, $          ; A [2,n] array of points that describe the blob perimeter.
;                              PIXEL_AREA: pixelArea, $               ; The area as calculated by pixels in the blob.
;                              PERIMETER_AREA: perimeterArea, $       ; The area as calculated by the blob perimeter. (Smaller than pixel area.)
;                              CENTER: centroid[0:1], $               ; The [x,y] array that identifies the centroid of the blob.
;                              PERIMETER_LENGTH: perimeter_length, $  ; The perimenter length (scaled unless the NOSCALE keyword is set).
;                              SCALE: scale, $                        ; The [xscale, yscale] array used in scaling.
;                              MINCOL: Min(xyindices[0,*]), $         ; The minimum column index in the blob.
;                              MAXCOL: Max(xyindices[0,*]), $         ; The maximum column index in the blob.
;                              MINROW: Min(xyindices[1,*]), $         ; The minimum row index in the blob.
;                              MAXROW: Max(xyindices[1,*])}           ; The maximum row index in the blob.
;   
;   NumberOfBlobs:     The number of blobs identified in the image.
;   
;   ReportStats:       This methods reports statistics on every identified blob in the image. The 
;                      report can be sent to the display (the default) or to a file. The format for
;                      the report works for most images, but you may have to change the format or override
;                      this method for your particular image. The reported statistics are basically the
;                      output of the GetStats and FitEllipse methods.
;
;    Here is an example of statistical output from the example program below.
;    
;  INDEX   NUM_PIXELS   CENTER_X    CENTER_Y   PIXEL_AREA   PERIMETER_AREA   PERIMETER_LENGTH  MAJOR_AXIS   MINOR_AXIS    ANGLE
;     0        426        107.89       9.78       106.50          98.00            37.56          12.15        11.29       -8.05
;     1        580        151.97      10.22       145.00         134.25            49.21          17.49        11.77       -0.99
;     2        812        266.29      15.36       203.00         190.75            52.56          17.88        14.65     -107.48
;     3       1438        204.53      43.29       359.50         344.13            70.23          21.68        21.12      -76.47
;
; RESTRICTIONS:
; 
;       Requires programs from the Coyote Library. At the very least, those below are required.
;       It is *highly* recommended that you install the entire library. FIT_ELLIPSE has been
;       changed specifically for this release, so by sure you get a copy of that with this
;       source code.
;       
;       http://www.idlcoyote.com/programs/coyoteprograms.zip
;       
;       ERROR_MESSAGE     http://www.idlcoyote.com/programs/error_message.pro
;       FIND_BOUNDARY     http://www.idlcoyote.com/programs/find_boundary.pro
;       FIT_ELLIPSE       http://www.idlcoyote.com/programs/fit_ellipse.pro
;       
;       The program currently works only with 2D bi-level images.
;
; EXAMPLE:
; 
;       To run an example program. Compile the file and type "example" at the IDL command line.
;       
;       IDL> .compile blob_analyzer__define
;       IDL> example
;
; MODIFICATION HISTORY:
; 
;       Written by David W. Fanning, Fanning Software Consulting, 17 August 2008.
;       Ideas taken from discussion with Ben Tupper and Ben's program HBB_ANALYZER.
;-
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
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
; NAME:
;  Blob_Analyzer::FitEllipse
;
; PURPOSE:
;
;   This function fits an ellipse to a particular blob and returns information
;   about the fit to the user.
;
; CALLING SEQUENCE:
;
;   ellipsePts = theBlobs -> FitEllipse(indexNumber)
;
; RETURN VALUE:
;
;     ellipsePts:   A [2,n] array containing the XY points of the fitted ellipse. The values
;                   are in scaled units unless the NOSCALE keyword is set, in which case the
;                   values are in DEVICE units.
;
; ARGUMENTS:
;
;    indexNumber:   The index number of the blob. Indices start at 0 and go to n-1.
;  
; INPUT KEYWORDS:  
; 
;    NOSCALE:       Set this keyword if you would prefer that lengths and positions NOT be
;                   scaled in the output of this function. If not scaled, results are in pixel
;                   or device coordinates. The default is to scale all output.
;                   
;    NPOINTS:       The number of points in the ellipse. By default, 120.
;
; OUTPUT KEYWORDRS:
;
;    AXES:          A two-element array containing the lengths of the major and minor axes,
;                   respectively. Lenghts are scaled unless the NOSCALE keyword is set.
;                   
;    CENTER:        A two-element array containing the [x,y] values of the center of the ellipse.
;                   Values are scaled unless the NOSCALE keyword is set.
;                   
;    ORIENTATION:   The orientation of the ellipse. The value is in degrees counter-clockwise of 
;                   the postive X direction.  Note that a value of 60 is the same as a value of 240.
;                   In other words, there is no direction associated with this value.
;                   
;    SEMIAXES:      A two-element array containing the lengths of the semi-major and semi-minor axes,
;                   respectively. Lenghts are scaled unless the NOSCALE keyword is set. (Half the length
;                   of AXES.
;
;
FUNCTION Blob_Analyzer::FitEllipse, indexNumber, $
    AXES=axes, $
    CENTER=center, $
    NOSCALE=noscale, $
    NPOINTS=npoints, $
    ORIENTATION=orientation, $
    SEMIAXES=semiAxes

    Compile_Opt idl2

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN, -1
    ENDIF
    
     ; Argument checking.
    IF N_Elements(indexNumber) EQ 0 THEN indexNumber = 0
    IF indexNumber LT 0 THEN Message, 'Required index number must be positive.'
    IF indexNumber GE self.count THEN Message, 'Index number exceeds total number of blobs.'
    IF N_Elements(npoints) EQ 0 THEN npoints = 120
    IF Keyword_Set(noscale) THEN scale = [1.0, 1.0] ELSE scale = self.scale

    ; Get the indices.
    indices = self ->GetIndices(indexNumber, XSIZE=xsize, YSIZE=ysize)
    
    ellipsePts = Fit_Ellipse(indices, XSIZE=xsize, YSIZE=ysize, NPOINTS=npoints, $
        AXES=axes, SEMIAXES=semiAxes, ORIENTATION=orientation, SCALE=scale)
        
    RETURN, ellipsePts
END ; ------------------------------------------------------------------------------

  
;
; NAME:
;  Blob_Analyzer::GetIndices
;
; PURPOSE:
;
;   This function returns the indices of a blob to the caller.
;
; CALLING SEQUENCE:
;
;   indices = theBlobs -> GetIndices(indexNumber)
;
; RETURN VALUE:
;
;     indices:     A vector of blob indices that describes the blob in the original image.
;
; ARGUMENTS:
;
;    indexNumber:   The index number of the blob. Indices start at 0 and go to n-1.
;  
; INPUT KEYWORDS:  
; 
;    None.
;
; OUTPUT KEYWORDRS:
;
;    COUNT:         The number of indices in the indices vector.
;                   
;    XSIZE:         The X size of the image from which the blob is taken.
;                   
;    YSIZE:         The Y size of the image from which the blob is taken.
;
;
FUNCTION Blob_Analyzer::GetIndices, indexNumber, COUNT=count, XSIZE=xsize, YSIZE=ysize

    Compile_Opt idl2

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN, -1
    ENDIF
    
    ; Argument checking.
    IF N_Elements(indexNumber) EQ 0 THEN indexNumber = 0
    IF indexNumber LT 0 THEN Message, 'Required index number must be positive.'
    IF indexNumber GE self.count THEN Message, 'Index number exceeds total number of blobs.'
    
    ; Return the size of the image, if the user asked for them.
    IF Arg_Present(xsize) THEN xsize = self.xsize
    IF Arg_Present(ysize) THEN ysize = self.ysize
    
    ; Get the indices:
    indices = (*self.ri)[(*self.ri)[indexNumber]:(*self.ri)[indexNumber+1]-1]
    IF Arg_Present(count) THEN count = N_Elements(indices)
    
    ; Return them.
    RETURN, indices
    
END ; ------------------------------------------------------------------------------


;
; NAME:
;  Blob_Analyzer::GetStats
;
; PURPOSE:
;
;   This function returns statistics of the blob in question.
;
; CALLING SEQUENCE:
;
;   statistics = theBlobs -> GetStats(indexNumber)
;
; RETURN VALUE:
;
;     statistics:   A structure of statistics that is defined like this.
;     
;                     stats = {INDEX: indexNumber, $                  ; The index number of this blob.
;                              COUNT: N_Elements(indices), $          ; The number of indices in this blob.
;                              PERIMETER_PTS: boundaryPts, $          ; A [2,n] array of points that describe the blob perimeter.
;                              PIXEL_AREA: pixelArea, $               ; The area as calculated by pixels in the blob.
;                              PERIMETER_AREA: perimeterArea, $       ; The area as calculated by the blob perimeter. (Smaller than pixel area.)
;                              CENTER: centroid[0:1], $               ; The [x,y] array that identifies the centroid of the blob.
;                              PERIMETER_LENGTH: perimeter_length, $  ; The perimenter length (scaled unless the NOSCALE keyword is set).
;                              SCALE: scale, $                        ; The [xscale, yscale] array used in scaling.
;                              MINCOL: Min(xyindices[0,*]), $         ; The minimum column index in the blob.
;                              MAXCOL: Max(xyindices[0,*]), $         ; The maximum column index in the blob.
;                              MINROW: Min(xyindices[1,*]), $         ; The minimum row index in the blob.
;                              MAXROW: Max(xyindices[1,*])}           ; The maximum row index in the blob.
;                              
; ARGUMENTS:
;
;    indexNumber:   The index number of the blob. Indices start at 0 and go to n-1.
;  
; INPUT KEYWORDS:  
; 
;    NOSCALE:       Set this keyword if you would prefer that lengths and positions NOT be
;                   scaled in the output of this function. If not scaled, results are in pixel
;                   or device coordinates. The default is to scale all output.
;
; OUTPUT KEYWORDRS:
;
;    INDICES:       A vector of blob indices that describes the blob in the original image.
;                   
;    XYINDICES:     A 2xN array of column/row indices that describes teh blob in the original image.
;    
; NOTES:
; 
;     The statistics are calculated by calling FIND_BOUNDARY from the Coyote Library. This program
;     uses a chain-code algorithm to calculate the perimeter and report the blob area using either of
;     two methods: a strict pixel area (counts the number of pixels in the blob times the scale factor
;     and takes the total), or it uses the perimeter to calculate an area using the method described in
;     Russ, The Image Processing Handbook, 2nd Edition, pp490+. The perimeter area is almost always less 
;     than the pixel area.
;
FUNCTION Blob_Analyzer::GetStats, indexNumber, INDICES=indices, NOSCALE=noscale, XYINDICES=xyindices

    Compile_Opt idl2

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN, -1
    ENDIF
    
    ; Argument checking.
    IF N_Elements(indexNumber) EQ 0 THEN indexNumber = 0
    IF indexNumber LT 0 THEN Message, 'Required index number must be positive.'
    IF indexNumber GE self.count THEN Message, 'Index number exceeds total number of blobs.'
    IF Keyword_Set(noscale) THEN scale = [1.0D, 1.0D] ELSE scale = self.scale

    ; Get the indices.
    indices = self ->GetIndices(indexNumber, XSIZE=xsize, YSIZE=ysize)
    
    ; Calculate the indices in terms of col/row coordinates.
    xyindices = Array_Indices([xsize,ysize], indices, /DIMENSIONS)
    
    ; Calculate statistics.
    boundaryPts = Find_Boundary(indices, $
        SCALE=scale, $
        XSIZE=xsize, $
        YSIZE=ysize, $
        AREA=pixelArea, $
        PERIM_AREA=perimeterArea, $
        CENTER=centroid, $
        PERIMETER=perimeter_length)
                                  
    ; Report them.
    stats = {INDEX: indexNumber, $                   ; The index number of thisblob.
             COUNT: N_Elements(indices), $           ; The number of indices in this blob.
             PERIMETER_PTS: boundaryPts, $           ; A [2,n] array of points that describe the blob perimeter.
             PIXEL_AREA: pixelArea, $                ; The area as calculated by pixels in the blob.
             PERIMETER_AREA: perimeterArea, $        ; The area as calculated by the blob perimeter. (Smaller than pixel area.)
             CENTER: centroid[0:1], $                ; The [x,y] array that identifies the centroid of the blob.
             PERIMETER_LENGTH: perimeter_length, $   ; The perimenter length (scaled unless the NOSCALE keyword is set).
             SCALE: scale, $                         ; The [xscale, yscale] array used in scaling.
             MINCOL: Min(xyindices[0,*]), $          ; The minimum column index in the blob.
             MAXCOL: Max(xyindices[0,*]), $          ; The maximum column index in the blob.
             MINROW: Min(xyindices[1,*]), $          ; The minimum row index in the blob.
             MAXROW: Max(xyindices[1,*])}            ; The maximum row index in the blob.
             
     RETURN, stats
             
END ; ------------------------------------------------------------------------------


;
; NAME:
;  Blob_Analyzer::NumberOfBlobs
;
; PURPOSE:
;
;   This function returns the number of blobs in the input image.
;
; CALLING SEQUENCE:
;
;   numBlobs = theBlobs -> NumberOfBlobs()
;   
; RETURN VALUE:
;
;     numBlobs:   The number of blobs in the input image.
;                              
; ARGUMENTS:
;
;    None.
;  
; KEYWORDS:  
; 
;    None.
;
FUNCTION Blob_Analyzer::NumberOfBlobs
    RETURN, self.count
END ; ------------------------------------------------------------------------------

  
;
; NAME:
;  Blob_Analyzer::ReportStats
;
; PURPOSE:
;
;   This function reports statistics on blobs in the image.
;
; CALLING SEQUENCE:
;
;   theBlobs -> ReportStats
;
; ARGUMENTS:
;
;    None.
;  
; INPUT KEYWORDS:  
; 
;    FILENAME:      The name of a file to contain the statistical output.
; 
;    NOSCALE:       Set this keyword if you would prefer that lengths and positions NOT be
;                   scaled in the output of this function. If not scaled, results are in pixel
;                   or device coordinates. The default is to scale all output.
;
;    TOFILE:         Normally the report is sent to standard ouput. If this keyword is set,
;                    the output is sent to a file.
;
; OUTPUT KEYWORDRS:
;
;    None.
;
; EXAMPLE:
; 
;    Here is an example of statistical output from the example program below.
;    
;  INDEX   NUM_PIXELS   CENTER_X    CENTER_Y   PIXEL_AREA   PERIMETER_AREA   PERIMETER_LENGTH  MAJOR_AXIS   MINOR_AXIS    ANGLE
;     0        426        107.89       9.78       106.50          98.00            37.56          12.15        11.29       -8.05
;     1        580        151.97      10.22       145.00         134.25            49.21          17.49        11.77       -0.99
;     2        812        266.29      15.36       203.00         190.75            52.56          17.88        14.65     -107.48
;     3       1438        204.53      43.29       359.50         344.13            70.23          21.68        21.12      -76.47
;
PRO Blob_Analyzer::ReportStats, NOSCALE=noscale, TOFILE=tofile, FILENAME=filename

    Compile_Opt idl2

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(lun) NE 0 THEN Free_Lun, lun
        RETURN
    ENDIF
    
     ; Argument checking.
     tofile = Keyword_Set(tofile)
     IF tofile THEN BEGIN
        IF N_Elements(filename) EQ 0 THEN BEGIN
            filename = Dialog_Pickfile(Title='Select file for statistical ouput.')
            IF filename EQ "" THEN RETURN
        ENDIF
        
            ; Open the file for writing.
            OpenW, lun, filename, /GET_LUN, WIDTH=100
     ENDIF
     
     ; Get the stats and output them to the display or to a file.
     header = '  INDEX   NUM_PIXELS   CENTER_X    CENTER_Y   PIXEL_AREA   PERIMETER_AREA   PERIMETER_LENGTH  MAJOR_AXIS   MINOR_AXIS    ANGLE'
     
     ; Write the header.
     IF tofile THEN $
        PrintF, lun, header ELSE $
        Print, header
        
     ; Write the rest of the information.
     format = '(x,I5, 6x,I5, 4x,F10.2, 1x,F10.2, 3x,F10.2, 5x,F10.2, 7x,F10.2, 5x,F10.2, 3x,F10.2, 2x,F10.2)' 
     FOR j=0, self.count-1 DO BEGIN
        stats = self -> GetStats(j, NOSCALE=noscale)
        void = self -> FitEllipse(j, AXES=axes, ORIENTATION=angle, NOSCALE=noscale)
         IF tofile THEN BEGIN
            PrintF, lun, StrTrim(j,2), stats.count, stats.center[0], stats.center[1], $
                stats.pixel_area, stats.perimeter_area,  $
                stats.perimeter_length, axes[0], axes[1], angle, FORMAT=format
         ENDIF ELSE BEGIN
            Print, StrTrim(j,2), stats.count, stats.center[0], stats.center[1], $
                stats.pixel_area, stats.perimeter_area,  $
                stats.perimeter_length, axes[0], axes[1], angle, FORMAT=format
         ENDELSE
         
     ENDFOR
     
     ; Close the file, if open.
     IF tofile THEN Free_Lun, lun
     
END ; ------------------------------------------------------------------------------


PRO Blob_Analyzer::CLEANUP
    Ptr_Free, self.ri
END ; ------------------------------------------------------------------------------


;
; NAME:
;  Blob_Analyzer::INIT
;
; PURPOSE:
;
;   This function initializes the Blob_Analyzer object.
;
; CALLING SEQUENCE:
;
;   theBlobs = Obj_New('Blob_Analyzer', image)
;   
; ARGUMENTS:
;
;   image:           A two-dimensional image array. To make this program memory efficient,
;                    a copy of the image is *not* stored in the object. You will be responsible
;                    for image operations outside this program.
;
; KEYWORDS:
;
;   ALL_NEIGHBORS:    Set this keyword to look at all eight neighbors when searching
;                     for connectivity. The default is to look for four neighbors on
;                     each side of the starting pixel. Passed directly to LABEL_REGION.
;                     
;   MASK:             A two-dimensional array, the same size as image, that identifies the
;                     foreground and background pixels in the image. Applying the mask
;                     should result in a bi-level image of 0s and 1s, where 1 identifies the 
;                     blobs you wish to analyze. If a mask is not provided, the mask is created
;                     like this:
;                     
;                     mask = image > 0
;
;   SCALE:            A one- or two-dimensional given the pixel scaling parameters. By default [1.0, 1.0].
;                     If passed a scalar, the scale parameter is applied to both the X and Y directions of
;                     each pixel. Statistical output is reported with scaling unless the NOSCALE keyword
;                     is set. Scaling also effects the data that is output from the various methods.
;
FUNCTION Blob_Analyzer::INIT, inputImage, $
    ALL_NEIGHBORS=all_neighbors, $
    MASK=mask, $
    SCALE=scale
    
    Compile_Opt idl2

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN, 0
    ENDIF
    
    ; Do you have an image. It is required.
    IF N_Elements(inputImage) EQ 0 THEN Message, 'An input image is a required argument.'
    
    ; Is the image 2D?
    ndims = Size(inputImage, /N_DIMENSIONS)
    IF ndims NE 2 THEN Message, 'The BLOB_ANALYZER only works with 2D images.'
        
    ; Check keywords.
    IF N_Elements(mask) EQ 0 THEN BEGIN
        image = inputImage GT 0
    ENDIF ELSE BEGIN
        
        ; Is the mask the same size as the image?
        IF Total(Size(inputImage, /DIMENSIONS) EQ Size(mask, /DIMENSIONS)) NE 2 THEN $
            Message, 'The image mask is not the same size as the input image.'
            
        ; Apply the mask to the image.
        image = inputImage * mask
    ENDELSE
    
    ; Check other keywords.
    IF N_Elements(scale) EQ 0 THEN scale = [1.0, 1.0]
    
    ; Get the size of the image.
    s = Size(image, /DIMENSIONS)
    xsize = s[0]
    ysize = s[1]
    type = Size(image, /TYPE)

    ; Label the regions.
    bigImage = Make_Array(s+2, TYPE=type)
    bigImage[1,1] = Temporary(image)
    labeledImage = Label_Region(bigImage, ALL_NEIGHBORS=Keyword_Set(all_neighbors))
    image = (Temporary(labeledImage))[1:xsize, 1:ysize]

    ; Get the indices of the label image by taking its histogram.
    h = Histogram(image, REVERSE_INDICES=ri, BINSIZE=1.0, MIN=1)
    count = N_Elements(h)
    
    ; Populate the object.
    self.count = count
    self.ri = Ptr_New(ri, /NO_COPY)
    self.xsize = xsize
    self.ysize = ysize
    self.scale = scale
    
    RETURN, 1
    
END ; ------------------------------------------------------------------------------


PRO Example

  ; Get a file for analysis.
  file = FILEPATH('r_seeberi.jpg', SUBDIRECTORY = ['examples', 'data'])
  READ_JPEG, file, image, /GRAYSCALE

  ; Define a structuring kernal for an opening operation on the image.
  radius = 5
  kernel = SHIFT(DIST(2*radius+1), radius, radius) LE radius
  
  ; Apply the opening operator to the image.
  openImage = MORPH_OPEN(image, kernel, /GRAY)
  
  ; Threshold the image to prepare to remove background noise.
  ; Notice that changing this value can produce more or less
  ; artifacts. You will have to decide what you can live with
  ; in your analysis. It requires some judgement on your part.
  mask = openImage GE 150
  
  ; Do the analysis.
  blobs = Obj_New('blob_analyzer', openImage, MASK=mask, SCALE=[0.5, 0.5])
  
  ; Display the original image
  s = Size(image, /DIMENSIONS)
  Window, XSIZE=2*s[0], YSIZE=2*s[1], Title='Blob Analyzer Example'
  LoadCT, 0
  cgImage, image, 0, /TV
  
  ; Display the opened image beside it.
  cgImage, openImage, 1, /TV
  
  ; Display the blobs we located with LABEL_REGION.
  count = blobs -> NumberOfBlobs()
  blank = BytArr(s[0], s[1])
  FOR j=0,count-1 DO BEGIN
    blobIndices = blobs -> GetIndices(j)
    blank[blobIndices] = image[blobIndices]
  ENDFOR
  cgImage, blank, 2, /TV
  
  ; Display the original image, with blob outlined and labelled.
  cgImage, image, 3, /TV
  FOR j=0,count-1 DO BEGIN
    stats = blobs -> GetStats(j, /NoScale)
    PLOTS, stats.perimeter_pts[0,*] + s[0], stats.perimeter_pts[1,*], /Device, COLOR=cgColor('dodger blue')
    XYOUTS, stats.center[0]+s[0], stats.center[1]-5, /Device, StrTrim(j,2), $
        COLOR=cgColor('red'), ALIGNMENT=0.5, CHARSIZE=0.75
  ENDFOR
  
  ; Add labels for captions.
  XYOUTS, 0.05, 0.95, 'A', FONT=0, ALIGNMENT=0.5, COLOR=cgColor('Yellow')
  XYOUTS, 0.55, 0.95, 'B', FONT=0, ALIGNMENT=0.5, COLOR=cgColor('Yellow')
  XYOUTS, 0.05, 0.45, 'C', FONT=0, ALIGNMENT=0.5, COLOR=cgColor('Yellow')
  XYOUTS, 0.55, 0.45, 'D', FONT=0, ALIGNMENT=0.5, COLOR=cgColor('Yellow')
  
  ; Report stats.
  blobs -> ReportStats
  
  ; Destroy the object.
  Obj_Destroy, blobs
  
END ; ------------------------------------------------------------------------------


PRO Blob_Analyzer__DEFINE, class 

    class = { BLOB_ANALYZER, $
              count: 0L, $        ; The total number of blobs being analyzed.
              ri: Ptr_New(), $    ; The reverse indices from HISTOGRAM. Used to get the blob indices.
              xsize: 0L, $        ; The X size of the image the indices originate in.
              ysize: 0L, $        ; The Y size of the image the indices originate in.
              scale: FltArr(2) $  ; The X and Y pixel scale for the indices. Set to [1,1] by default.
           }

END ; ------------------------------------------------------------------------------
