;+
; NAME:
;   IMAGESELECT
;
; PURPOSE:
;
;   The purpose of this program is to allow the user to select a formatted
;   image file for reading. The image data is returned as the result of the
;   function. The program allows the user to see a thumbnail version of the
;   image, along with information about the image, before selection. The
;   program uses the file extention to determine what kind of image is to
;   be read. The following image types are supported:
;
;      TYPE      FILE EXTENSION
;      BMP       *.bmp
;      DICOM     *.dcm
;      FITS      *.fits, *.fts (requires NASA ASTRO library on IDL Path)
;      GIF       *.gif (IDL 6.2 and higher)
;      JPEG      *.jpg, *.jpeg, *.jpe
;      JPEG2000  *.jpf, *.jpx, *.jp2, *.j2c, *.j2k
;      PICT      *.pict
;      PNG       *.png
;      TIFF      *.tif, *tiff
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: david@idlcoyote.com
;   Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; CATEGORY:
;
;   General programming.
;
; CALLING SEQUENCE:
;
;   image = ImageSelect()
;
; INPUT PARAMETERS:
;
;   None. All input is via keywords.
;
; INPUT KEYWORDS:
;
;   BMP -- Set this keyword to select BMP files.
;   
;   DEMO -- If this keyword is set, the program changes directory to !DIR/examples/data.
;
;   DICOM -- Set this keyword to select DICOM files.
;
;   DIRECTORY -- The initial input directory name. The current directory by default.
;   
;   EXCLUDE -- A list of filenames that should excluded from the file selection list.
;
;   FILENAME -- The initial filename. If the initial directory has image files of the
;               correct type, the default is to display the first of these files. Otherwise, blank.
;
;   FILTER -- A string, representing the file filter. For example, '*.jpg'.
;
;   FITS -- Set the keyword to select FITS files. (Must have NASA Astro Library on path.)
;
;   FLIPIMAGE -- Set this keyword if you wish to flip the image from its current orientation. Setting
;                this keyword reverses the Y dimension of the image.
;
;   GIF -- Set this keyword to select GIF files. (IDL versions before 5.4 and after 6.0, only.)
;
;   GROUP_LEADER -- Set this keyword to a widget identifier group leader. This keyword MUST be
;                   set when calling this program from another widget program to guarantee modal operation.
;
;   J2000 -- Set this keyword to select JPEG2000 files. (May also be set as J2K.) (IDL 6.1 or above.)
;
;   J2K -- Set this keyword to select JPEG2000 files. (May also be set as J2000.) (IDL 6.1 or above.)
;
;   JPEG -- Set this keyword to select JPEG files.
;
;   LISTXSIZE -- Set this keyword to the XSIZE of the list widget. Default is 30 or MAX(StrLen(filenames)), whichever is larger.
;
;   OFFSETS -- A two-element array containing the X and Y offsets of the program, from the upper left
;              corner of the display. On dismissal of the program, if this is a named variable (passed into
;              the program by reference), then it will contain the last offsets of the program. This is
;              useful if you want to call ImageSelect again and have it positioned in exactly the same
;              location it was before.
;
;   ONLY2D -- Set this keyword if you only want the user to be able to select 2D images. Note
;             that the user will be able to browse all images, but the Accept button will only
;             be sensitive for 2D images.
;
;   ONLY3D -- Set this keyword if you only want the user to be able to select 3D or true-color images.
;             Note that the user will be able to browse all images, but the Accept button will only
;             be sensitive for 3D or true-color images.
;
;   PICT -- Set this keyword to select PICT files.
;
;   PGM -- Set this keyword to select PGM files.
;
;   PPM -- Set this keyword to select PPM files.
;
;   PNG -- Set this keyword to select PNG files.
;
;   PREVIEWSIZE -- Set this keyword to the maximum size (in pixels) of the preview window. Default is 150.
;
;   SILENT -- Set this keyword to turn off Group_Leader educational message. Use only if you
;             are sure you know what you are doing. :-)
;
;   TIFF -- Set this keyword to select TIFF files. (This is the default filter selection.)
;
;   TITLE -- Set this keyword to the text to display as the title of the main image selection window.
;
;   NOTE: Any extra keywords passed into the program will collected and passed along to the READ_XXX routines
;   that actually do the image file reading. Using this keyword inheritance mechanism makes it impossible
;   to trap misspelled or misused keywords. Please take care when using ANY keyword for this routine!
;
; OUTPUT KEYWORDS:
;
;   CANCEL -- This keyword is set to 1 if the user exits the program in any way except hitting the ACCEPT button.
;             The ACCEPT button will set this keyword to 0.
;
;   FHEADER -- Set this keyword to a named variable that will return the FITS header information for a FITS file.
;
;   FILEINFO -- This keyword returns information about the selected file. Obtained from the QUERY_**** functions.
;
;   GEOTIFF --  If the file is a GeoTIFF file, this keyword will return the GeoTIFF structure containing
;               the files GeoTags.
;               
;   OUTDIRECTORY -- The directory where the selected file is found.
;
;   OUTFILENAME -- The short filename of the selected file.
;
;   PALETTE -- The current color table palette returned as a 256-by-3 byte array.
;
; COMMON BLOCKS:
;
;   None.
;
; RESTRICTIONS:
;
;   Requires other programs from the Coyote Library.
;
;  Note: Keyword inheritance to collect undefined keywords that may be passed into the
;  program for use in READ_XXX routines, make it impossible to trap keyword useage errors.
;  Please take care when using keywords.
;
; EXAMPLE:
;
;   To read JPEG files from the directory:
;
;      IDL> image = ImageSelect(/JPEG)
;
; MODIFICATION HISTORY:
;
;   Written by: David W. Fanning, 18 Jan 2001.
;   Added modification to read both 8-bit and 24-bit BMP files. 27 Jan 2001. DWF.
;   Fixed a problem with calculating the new size of the draw widget. 5 April 2002. DWF.
;   Fixed a problem with List Widgets not sizing correctly on UNIX machines. 10 Aug 2002. DWF.
;   Fixed a problem with the initial file not being selected correctly when you changed
;     the file type. 10 Aug 2002. DWF.
;   Added a FLIPIMAGE keyword 10 Aug 2002. DWF.
;   When user chooses to Flip Image, I now reverse the Y dimension of the image,
;     rather than set the !Order system variable. 10 Aug 2002. DWF.
;   Added OUTDIRECTORY and OUTFILENAME keywords. 18 Aug 2002. DWF.
;   Fairly extensive changes in the way this program works and selects images.
;     A new version of FSC_FileSelect is also required. Because of interactions
;     with the operating system with image filters, the program has probably
;     become more Windows-centric. The default is now to display all image
;     files the program is capable of reading. 31 October 2002. DWF.
;   Added ONLY2D keyword to allow the acceptance of 2D images only. 3 Nov 2002. DWF.
;   Added ability to center itself on the display. 8 Nov 2002. DWF.
;   Fixed a problem caused by reading old images with short color table vectors. 26 Nov 2002. DWF.
;   Fixed a problem with specifying a fully-qualified filename. 26 Nov 2002. DWF.
;   Now highlights the selected file in the directory. 26 Nov 2002. DWF.
;   Improved error handling. 9 Dec 2002. DWF.
;   Added PALETTE keyword and improved color operation on 8-bit displays. If the image file
;     contains a color palette, that palette is now loaded when the image is read from the file.
;     The current color palette can be obtained with the PALETTE keyword. 4 April 2003. DWF.
;   Added ONLY3D keyword. 19 April 2003. DWF.
;   Added ability to read PPM and PGM files. 24 November 2003. DWF.
;   Added TITLE keyword. 1 December 2003. DWF.
;   Added EXAMPLES keyword. 22 December 2005. DWF.
;   Added GIF and JPEG2000 file types. Rearranged and cleaned up code. 3 January 2006. DWF.
;   Added LISTXSIZE keyword. 3 January 2006. DWF.
;   Added file type checkmark buttons. Program now compatible with IDL 5.6 and higher. 3 January 2006. DWF.
;   Improved error handling with invalid file types. 5 January 2006. DWF.
;   Added OFFSETS and EXCLUDE keywords. 3 March 2006 DWF.
;   Modified the program to check for FITS unsigned integer data. 3 March 2006. DWF.
;   Added ability to double-click image name in list to Accept. 10 March 2006. DWF.
;   Added FHEADER keyword to return FITS header information. 3 April 2006. DWF.
;   Fixed a problem in which the file type was not set if the user cancelled. 10 July 2006. DWF.
;   Added a "fit" file extension for FITS images. 1 April 2008. DWF.
;   Added a FILTER keyword. 1 April 2008. DWF.
;   Updated for reading transparent images. 13 May 2009. DWF.
;   Provided check for PNG images with more than 8 bits per channel. 5 August 2009. DWF.
;   Fixed a problem in which the starting directory was changed on exit. 20 Nov 2010. DWF.
;   Change EXAMPLES to more easily remembered DEMO keyword. 29 Nov 2010. DWF.
;   Removed NOINTERPOLATION keywords in going from TVIMAGE to cgImage. 22 Feb 2011. DWF.
;   Fixed a problem reading 2D Tiff files. 20 Sept 2012. DWF.
;   
;-
;
;******************************************************************************************;
;  Copyright (c) 2008-2012, by Fanning Software Consulting, Inc.                           ;
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
FUNCTION ImageSelect_WindowSize, image, XSIZE=xsize, YSIZE=ysize

; This function determines the correct window size for an image that keeps
; its aspect ratio. Returns two element array: [xsize, ysize]. Assumes the
; current graphics window is where the image is to be displayed.

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN, Size(image, /Dimensions)
   ENDIF

   ; Need X and Y size of image.
   dims = Image_Dimensions(image, XSize=imgXSize, YSize=imgYSize)

   ; Fill the window.
   position=[0.0, 0.0, 1.0, 1.0]

   ; Find aspect ratio of image.
   ratio = FLOAT(imgYsize) / imgXSize

   ; Find the proposed size of the image in pixels without aspect
   ; considerations.
   xpixSize = (position(2) - position(0)) * xsize
   ypixSize = (position(3) - position(1)) * ysize

   ; Try to fit the image width. If you can't maintain
   ; the aspect ratio, fit the image height.
   trialX = xpixSize
   trialY = trialX * ratio
   IF trialY GT ypixSize THEN BEGIN
      trialY = ypixSize
      trialX = trialY / ratio
   ENDIF

   ; Recalculate the position of the image in the window.
   position(0) = (((xpixSize - trialX) / 2.0) / xsize) + position(0)
   position(2) = position(0) + (trialX/FLOAT(xsize))
   position(1) = (((ypixSize - trialY) / 2.0) / ysize)  + position(1)
   position(3) = position(1) + (trialY/FLOAT(ysize))

   ; Return the X and Y sizes.
   RETURN, Round([(position[2]-position[0])*xsize, (position[3]-position[1])*ysize])

END ; ----------------------------------------------------------------------------------------


PRO ImageSelect_Action, event

; This event handler responds to CANCEL and ACCEPT buttons.

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message()
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF
   IF event.select NE 1 THEN RETURN

   Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get the button value.
   Widget_Control, event.id, Get_Value=buttonValue

   ; Set the cancel flag appropriately.
   IF buttonValue EQ 'Accept' THEN (*info.storagePtr).cancel = 0

   ; Get the name and directory you selected and set them into output fields.
   info.filenameObj->GetProperty, Directory=outdirectory, Filename=outfilename
   (*info.storagePtr).outdirectory = outdirectory
   (*info.storagePtr).outfilename = outfilename
   
   ; Is this a GEOTIFF file?
   isTiff = Query_Tiff(Filepath(ROOT_DIR=outdirectory, outfilename), GEOTIFF=geotiff)
   IF isTiff THEN BEGIN
        IF Size(geotiff, /TNAME) EQ 'STRUCT' THEN (*info.storagePtr).geotiff = Ptr_New(geotiff)
   ENDIF

   ; Get the current offsets.
   Widget_Control, event.top, TLB_GET_OFFSET=offsets
   (*info.storagePtr).offsets = offsets

   Widget_Control, event.top, Set_UValue=info, /No_Copy
   Widget_Control, event.top, /Destroy

END ;---------------------------------------------------------------------------------



FUNCTION ImageSelect_BSort, Array, Asort, INFO=info, REVERSE = rev
;
; NAME:
;       ImageSelect_BSort
; PURPOSE:
;       Function to sort data into ascending order, like a simple bubble sort.
; EXPLANATION:
;       Original subscript order is maintained when values are equal (FIFO).
;       (This differs from the IDL SORT routine alone, which may rearrange
;       order for equal values)
;
; CALLING SEQUENCE:
;       result = ImageSelect_BSort( array, [ asort, /INFO, /REVERSE ] )
;
; INPUT:
;       Array - array to be sorted
;
; OUTPUT:
;       result - sort subscripts are returned as function value
;
; OPTIONAL OUTPUT:
;       Asort - sorted array
;
; OPTIONAL KEYWORD INPUTS:
;       /REVERSE - if this keyword is set, and non-zero, then data is sorted
;                 in descending order instead of ascending order.
;       /INFO = optional keyword to cause brief message about # equal values.
;
; HISTORY
;       written by F. Varosi Oct.90:
;       uses WHERE to find equal clumps, instead of looping with IF ( EQ ).
;       compatible with string arrays, test for degenerate array
;       20-MAY-1991     JKF/ACC via T AKE- return indexes if the array to
;                       be sorted has all equal values.
;       Aug - 91  Added  REVERSE keyword   W. Landsman
;       Always return type LONG    W. Landsman     August 1994
;       Converted to IDL V5.0   W. Landsman   September 1997
;
        N = N_elements( Array )
        if N lt 1 then begin
                print,'Input to ImageSelect_BSort must be an array'
                return, [0L]
           endif

        if N lt 2 then begin
            asort = array       ;MDM added 24-Sep-91
            return,[0L]    ;Only 1 element
        end
;
; sort array (in descending order if REVERSE keyword specified )
;
        subs = sort( Array )
        if keyword_set( REV ) then subs = rotate(subs,5)
        Asort = Array[subs]
;
; now sort subscripts into ascending order
; when more than one Asort has same value
;
             weq = where( (shift( Asort, -1 ) eq Asort) , Neq )

        if keyword_set( info ) then $
                message, strtrim( Neq, 2 ) + " equal values Located",/CON,/INF

        if (Neq EQ n) then return,lindgen(n) ;Array is degenerate equal values

        if (Neq GT 0) then begin

                if (Neq GT 1) then begin              ;find clumps of equality

                        wclump = where( (shift( weq, -1 ) - weq) GT 1, Nclump )
                        Nclump = Nclump + 1

                  endif else Nclump = 1

                if (Nclump LE 1) then begin
                        Clump_Beg = 0
                        Clump_End = Neq-1
                  endif else begin
                        Clump_Beg = [0,wclump+1]
                        Clump_End = [wclump,Neq-1]
                   endelse

                weq_Beg = weq[ Clump_Beg ]              ;subscript ranges
                weq_End = weq[ Clump_End ] + 1          ; of Asort equalities.

                if keyword_set( info ) then message, strtrim( Nclump, 2 ) + $
                                " clumps of equal values Located",/CON,/INF

                for ic = 0L, Nclump-1 do begin          ;sort each clump.

                        subic = subs[ weq_Beg[ic] : weq_End[ic] ]
                        subs[ weq_Beg[ic] ] = subic[ sort( subic ) ]
                  endfor

                if N_params() GE 2 then Asort = Array[subs]     ;resort array.
           endif

return, subs
END ; ----------------------------------------------------------------------------------------



PRO ImageSelect_Cleanup, tlb

; Program pointers are cleaned up here.

   Widget_Control, tlb, Get_UValue=info, /No_Copy
   IF N_Elements(info) EQ 0 THEN RETURN
   Ptr_Free, info.theFiles
   Ptr_Free, info.filter
   Ptr_Free, info.excludeFiles

END ; ----------------------------------------------------------------------------------------



FUNCTION ImageSelect_FileExtension, filename

; This function finds the file extension of the filename by
; searching for the last ".".

   parts = StrSplit(filename, ".", /Extract)
   IF N_Elements(parts) EQ 1 THEN extension = "*" ELSE $
      extension = parts[N_Elements(parts)-1]

   RETURN, StrUpCase(extension)

END ; ----------------------------------------------------------------------------------------



PRO ImageSelect_FilenameEvents, event

; This event handler responds to events from the filename compound widget.

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message()
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get the name of the file.
   filename = event.basename
   CD, event.directory, Current=thisDirectory

   ; Locate appropriate files.
   Ptr_Free, info.theFiles
   info.theFiles = Ptr_New(/Allocate_Heap)

   ; Sort the files.
   FOR j=0, N_Elements(*info.filter)-1 DO BEGIN
      specificFiles = File_Search((*info.filter)[j], Count=fileCount)
      IF fileCount GT 0 THEN IF N_Elements(*(info.theFiles)) EQ 0 THEN $
         *info.theFiles = specificFiles[ImageSelect_BSort(specificFiles)] ELSE $
         *info.theFiles = [*info.theFiles, specificFiles[ImageSelect_BSort(specificFiles)]]
   ENDFOR
   fileCount = N_Elements(*info.theFiles)
   IF fileCount EQ 0 THEN *info.theFiles = "" ELSE BEGIN
      IF Ptr_Valid(info.excludeFiles) THEN BEGIN
        FOR k=0,N_Elements(*info.excludeFiles)-1 DO BEGIN
           index = Where(StrUpCase(*info.theFiles) EQ StrUpCase((*info.excludeFiles)[k]), count, COMPLEMENT=ok)
           IF count GT 0 THEN *info.theFiles = (*info.theFiles)[ok]
        ENDFOR
      ENDIF
      IF filename EQ "" THEN filename = (*info.theFiles)[0]
   ENDELSE
   info.dataDirectory = event.directory

   ; Is the filename amoung the list of files? If not,
   ; chose another filename.
   index = Where(StrLowCase(*info.theFiles) EQ StrLowCase(filename), count)
   IF count EQ 0 THEN BEGIN
      filename = (*info.theFiles)[0]
      Widget_Control, info.filenameID, Set_Value=filename
   ENDIF

   ; Set the file list.
   Widget_Control, info.fileListID, Set_Value=*info.theFiles

   ; Can you find the filename in the list of files? If so,
   ; highlight it in the list.
   i = Where(StrUpCase(*info.theFiles) EQ StrUpCase(filename), count)
   IF count GT 0 THEN Widget_Control, info.filelistID, Set_List_Select=i

   ; Set the file extension.
   thisExtension = ImageSelect_FileExtension(filename)

   ; Try to read the image
   ImageSelect_ReadFiles, thisExtension, filename, info, fileinfo, $
        image, ok, type, _Extra=*((*info.storagePtr).extra)

   ; If you are only displaying 2D images, set sensitive buttons.
   IF info.only2d THEN IF fileinfo.channels NE 1 THEN $
      Widget_Control, info.acceptID, Sensitive=0 ELSE $
      Widget_Control, info.acceptID, Sensitive=1

   ; If you are only displaying 3D images, set sensitive buttons.
   IF info.only3d THEN IF fileinfo.channels NE 3 THEN $
      Widget_Control, info.acceptID, Sensitive=0 ELSE $
      Widget_Control, info.acceptID, Sensitive=1

   ; What kind of image is this?
   CASE fileinfo.channels OF
      3: imageType = 'True-Color ' + type + ' Image'
      0: imageType = 'No Image'
      -1: imageType = 'Invalid Image File'
      ELSE: imageType = '2D ' + type + ' Image'
   ENDCASE

   ; Get the file sizes. Dicom images can report incorrect sizes,
   ; which is what we are trying to fix in the ysize line.
   xsize = fileInfo.dimensions[0]
   ysize = fileInfo.dimensions[1] > Fix(xsize * 0.5)

   ; Get the file sizes.
   dimensions = Image_Dimensions(image, XSize=xsize, YSize=ysize, YIndex=yindex)

   ; Flip the image if required.
   IF info.flipimage THEN image = Reverse(image, yindex+1)

   ; Calculate a window size for the image preview.
   aspect = Float(xsize) / ysize
   IF aspect GT 1 THEN BEGIN
      wxsize = Fix(info.previewSize)
      wysize = Fix(info.previewSize / aspect) < info.previewSize
   ENDIF ELSE BEGIN
      wysize = Fix(info.previewSize)
      wxsize = Fix(info.previewSize / aspect) < info.previewSize
   ENDELSE

   ; If you don't have an image, then get sensible numbers for the labels.
   IF imageType EQ 'No Image' OR imageType EQ 'Invalid Image File' THEN BEGIN
      xsize = 0
      ysize = 0
      minval = 0
      maxval = 0
   ENDIF

   ; Update the display with what you have.
   IF imageType EQ 'No Image' THEN imageDataType = 'NONE' ELSE imageDataType = Size(image, /TNAME)
   IF imageType EQ 'Invalid Image File' THEN imageDataType = 'INVALID'
   Widget_Control, info.labelTypeID, Set_Value=imageType
   Widget_Control, info.labelXSizeID, Set_Value="X Size: " + StrTrim(xsize, 2)
   Widget_Control, info.labelYSizeID, Set_Value="Y Size: " + StrTrim(ysize, 2)
   Widget_Control, info.labelDataTypeID, Set_Value="Type: " + imageDataType
   Widget_Control, info.labelminvalID, Set_Value="Min Value: " + cgNumber_Formatter(Min(image))
   Widget_Control, info.labelmaxvalID, Set_Value="Max Value: " + cgNumber_Formatter(Max(image))

   ; Draw the preview image.
   WSet, info.previewWID
   sizes = ImageSelect_WindowSize(image, XSIZE=info.previewSize, YSIZE=info.previewSize)
   Widget_Control, info.previewID, Draw_XSize=sizes[0], Draw_YSize=sizes[1]
   TVLCT, info.r, info.g, info.b
   IF (Min(image) LT 0) OR (Max(image) GT (!D.Table_Size-1)) THEN $
      cgImage, BytScl(image, Top=!D.Table_Size-1), /Keep_Aspect ELSE $
      cgImage, image, /Keep_Aspect
   IF imageDataType EQ 'NONE' THEN image = 0

   ; Store the image data for later retrieval.
   *(*(info.storagePtr)).image = image
   *(*(info.storagePtr)).fileInfo = fileInfo
   (*info.storagePtr).r = info.r
   (*info.storagePtr).g = info.g
   (*info.storagePtr).b = info.b

   ; Clean up.
   CD, thisDirectory
   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ;---------------------------------------------------------------------------------



PRO ImageSelect_FlipImage, event

; This event handler reverses the Y dimension of the image and re-displays it.

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message()
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Reverse the Y dimensions.
   dims = Image_Dimensions(*(*(info.storagePtr)).image, YIndex=yindex)
   *(*(info.storagePtr)).image = Reverse(*(*(info.storagePtr)).image, yindex + 1)

   ; Display it again.
   WSet, info.previewWID
   sizes = ImageSelect_WindowSize(*(*(info.storagePtr)).image, XSIZE=info.previewSize, YSIZE=info.previewSize)
   Widget_Control, info.previewID, Draw_XSize=sizes[0], Draw_YSize=sizes[1]
   TVLCT, info.r, info.g, info.b
   IF (Min(*(*(info.storagePtr)).image) LT 0) OR (Max(*(*(info.storagePtr)).image) GT (!D.Table_Size-1)) THEN $
      cgImage, BytScl(*(*(info.storagePtr)).image, Top=!D.Table_Size-1), /Keep_Aspect ELSE $
      cgImage, *(*(info.storagePtr)).image, /Keep_Aspect

   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ; ----------------------------------------------------------------------------------------



PRO ImageSelect_ListEvents, event

; Handles events from the list widget of file names.

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message()
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   ; Handle double click events here.
   IF event.clicks NE 1 THEN BEGIN

       Widget_Control, event.top, Get_UValue=info, /No_Copy

      ; Get the name of the file.
      filename = (*info.theFiles)[event.index]
      CD, info.dataDirectory, Current=thisDirectory

      ; Set it in the Filename widget.
      Widget_Control, info.filenameID, Set_Value=filename

      ; Try to read the image
      thisExtension = ImageSelect_FileExtension(filename)
      ImageSelect_ReadFiles, thisExtension, filename, info, fileinfo, $
            image, ok, type, _Extra=*((*info.storagePtr).extra)

      ; If this is an image you can accept, the read it and exit.
      ; If not treat as a single click.
      canRead = 1
      IF info.only2d AND (fileinfo.channels NE 1) THEN canRead = 0
      IF info.only3d AND (fileinfo.channels NE 3) THEN canRead = 0

      IF canRead THEN BEGIN

         ; Set the cancel flag appropriately.
         (*info.storagePtr).cancel = 0

         ; Get the name and directory you selected and set them into output fields.
         info.filenameObj->GetProperty, Directory=outdirectory, Filename=outfilename
         (*info.storagePtr).outdirectory = outdirectory
         (*info.storagePtr).outfilename = outfilename

         ; Get the current offsets.
         Widget_Control, event.top, TLB_GET_OFFSET=offsets
         (*info.storagePtr).offsets = offsets

         ; Store the image data for later retrieval.
         *(*(info.storagePtr)).image = image
         *(*(info.storagePtr)).fileInfo = fileInfo
         (*info.storagePtr).r = info.r
         (*info.storagePtr).g = info.g
         (*info.storagePtr).b = info.b
         TVLCT, info.r, info.g, info.b

         Widget_Control, event.top, Set_UValue=info, /No_Copy
         Widget_Control, event.top, /Destroy

      ENDIF ELSE BEGIN

         ; Set sensive buttons on for both 2D and 3D images.
         IF info.only2d THEN IF fileinfo.channels NE 1 THEN $
            Widget_Control, info.acceptID, Sensitive=0 ELSE $
            Widget_Control, info.acceptID, Sensitive=1
         IF info.only3d THEN IF fileinfo.channels NE 3 THEN $
            Widget_Control, info.acceptID, Sensitive=0 ELSE $
            Widget_Control, info.acceptID, Sensitive=1

         ; What kind of image is this?
           CASE fileinfo.channels OF
              3: imageType = 'True-Color ' + type + ' Image'
              0: imageType = 'No Image'
              -1: imageType = 'Invalid Image File'
              ELSE: imageType = '2D ' + type + ' Image'
           ENDCASE

         ; Get the file sizes. Dicom images can report incorrect sizes,
         ; which is what we are trying to fix in the ysize line.
         xsize = fileInfo.dimensions[0]
         ysize = fileInfo.dimensions[1] > Fix(xsize * 0.5)

         ; Get the file sizes.
         dimensions = Image_Dimensions(image, XSize=xsize, YSize=ysize, YIndex=yindex)

         ; Flip the image if required.
         IF info.flipimage THEN image = Reverse(image, yindex+1)

         ; Calculate a window size for the image preview.
         aspect = Float(xsize) / ysize
         IF aspect GT 1 THEN BEGIN
            wxsize = Fix(info.previewSize)
            wysize = Fix(info.previewSize / aspect) < info.previewSize
         ENDIF ELSE BEGIN
            wysize = Fix(info.previewSize)
            wxsize = Fix(info.previewSize / aspect) < info.previewSize
         ENDELSE

         ; If you don't have an image, then get sensible numbers for the labels.
         IF imageType EQ 'No Image' OR imageType EQ 'Invalid Image File' THEN BEGIN
            xsize = 0
            ysize = 0
            minval = 0
            maxval = 0
         ENDIF

         ; Update the display with what you have.
         IF imageType EQ 'No Image' THEN imageDataType = 'NONE' ELSE imageDataType = Size(image, /TNAME)
         IF imageType EQ 'Invalid Image File' THEN imageDataType = 'INVALID'
         Widget_Control, info.labelTypeID, Set_Value=imageType
         Widget_Control, info.labelXSizeID, Set_Value="X Size: " + StrTrim(xsize, 2)
         Widget_Control, info.labelYSizeID, Set_Value="Y Size: " + StrTrim(ysize, 2)
         Widget_Control, info.labelDataTypeID, Set_Value="Type: " + imageDataType
         IF imageDataType NE 'FLOAT' THEN BEGIN
            Widget_Control, info.labelminvalID, Set_Value="Min Value: " + cgNumber_Formatter(Min(image))
            Widget_Control, info.labelmaxvalID, Set_Value="Max Value: " + cgNumber_Formatter(Max(image))
         ENDIF ELSE BEGIN
            Widget_Control, info.labelminvalID, Set_Value="Min Value: " + cgNumber_Formatter(Min(image))
            Widget_Control, info.labelmaxvalID, Set_Value="Max Value: " + cgNumber_Formatter(Max(image))
         ENDELSE

         ; Draw the preview image.
         WSet, info.previewWID
         sizes = ImageSelect_WindowSize(image, XSIZE=info.previewSize, YSIZE=info.previewSize)
         Widget_Control, info.previewID, Draw_XSize=sizes[0], Draw_YSize=sizes[1]
         TVLCT, info.r, info.g, info.b
         IF (Min(image) LT 0) OR (Max(image) GT (!D.Table_Size-1)) THEN $
            cgImage, BytScl(image, Top=!D.Table_Size-1), /Keep_Aspect ELSE $
            cgImage, image, /Keep_Aspect
         IF imageDataType EQ 'NONE' THEN image = 0

         ; Store the image data for later retrieval.
         *(*(info.storagePtr)).image = image
         *(*(info.storagePtr)).fileInfo = fileInfo
         (*info.storagePtr).r = info.r
         (*info.storagePtr).g = info.g
         (*info.storagePtr).b = info.b

         ; Clean up.
         CD, thisDirectory
         Widget_Control, event.top, Set_UValue=info, /No_Copy

      ENDELSE

   ENDIF


   ; Handle single click events here.
   IF event.clicks EQ 1 THEN BEGIN

      Widget_Control, event.top, Get_UValue=info, /No_Copy

      ; Get the name of the file.
      filename = (*info.theFiles)[event.index]
      CD, info.dataDirectory, Current=thisDirectory

      ; Set it in the Filename widget.
      Widget_Control, info.filenameID, Set_Value=filename

      ; Try to read the image
      thisExtension = ImageSelect_FileExtension(filename)
      ImageSelect_ReadFiles, thisExtension, filename, info, fileinfo, $
            image, ok, type, _Extra=*((*info.storagePtr).extra)

      ; Set sensive buttons on for both 2D and 3D images.
      IF info.only2d THEN IF fileinfo.channels NE 1 THEN $
         Widget_Control, info.acceptID, Sensitive=0 ELSE $
         Widget_Control, info.acceptID, Sensitive=1
      IF info.only3d THEN IF fileinfo.channels NE 3 THEN $
         Widget_Control, info.acceptID, Sensitive=0 ELSE $
         Widget_Control, info.acceptID, Sensitive=1

      ; What kind of image is this?
       CASE fileinfo.channels OF
          3: imageType = 'True-Color ' + type + ' Image'
          0: imageType = 'No Image'
          -1: imageType = 'Invalid Image File'
          ELSE: imageType = '2D ' + type + ' Image'
       ENDCASE

      ; Get the file sizes. Dicom images can report incorrect sizes,
      ; which is what we are trying to fix in the ysize line.
      xsize = fileInfo.dimensions[0]
      ysize = fileInfo.dimensions[1] > Fix(xsize * 0.5)

      ; Get the file sizes.
      dimensions = Image_Dimensions(image, XSize=xsize, YSize=ysize, YIndex=yindex)

      ; Flip the image if required.
      IF info.flipimage THEN image = Reverse(image, yindex+1)

      ; Calculate a window size for the image preview.
      aspect = Float(xsize) / ysize
      IF aspect GT 1 THEN BEGIN
         wxsize = Fix(info.previewSize)
         wysize = Fix(info.previewSize / aspect) < info.previewSize
      ENDIF ELSE BEGIN
         wysize = Fix(info.previewSize)
         wxsize = Fix(info.previewSize / aspect) < info.previewSize
      ENDELSE

      ; If you don't have an image, then get sensible numbers for the labels.
      IF imageType EQ 'No Image' OR imageType EQ 'Invalid Image File' THEN BEGIN
         xsize = 0
         ysize = 0
         minval = 0
         maxval = 0
      ENDIF

      ; Update the display with what you have.
      IF imageType EQ 'No Image' THEN imageDataType = 'NONE' ELSE imageDataType = Size(image, /TNAME)
      IF imageType EQ 'Invalid Image File' THEN imageDataType = 'INVALID'
      Widget_Control, info.labelTypeID, Set_Value=imageType
      Widget_Control, info.labelXSizeID, Set_Value="X Size: " + StrTrim(xsize, 2)
      Widget_Control, info.labelYSizeID, Set_Value="Y Size: " + StrTrim(ysize, 2)
      Widget_Control, info.labelDataTypeID, Set_Value="Type: " + imageDataType
      IF imageDataType NE 'FLOAT' THEN BEGIN
         Widget_Control, info.labelminvalID, Set_Value="Min Value: " + cgNumber_Formatter(Min(image))
         Widget_Control, info.labelmaxvalID, Set_Value="Max Value: " + cgNumber_Formatter(Max(image))
      ENDIF ELSE BEGIN
         Widget_Control, info.labelminvalID, Set_Value="Min Value: " + cgNumber_Formatter(Min(image))
         Widget_Control, info.labelmaxvalID, Set_Value="Max Value: " + cgNumber_Formatter(Max(image))
      ENDELSE

      ; Draw the preview image.
      WSet, info.previewWID
      sizes = ImageSelect_WindowSize(image, XSIZE=info.previewSize, YSIZE=info.previewSize)
      Widget_Control, info.previewID, Draw_XSize=sizes[0], Draw_YSize=sizes[1]
      TVLCT, info.r, info.g, info.b
      IF (Min(image) LT 0) OR (Max(image) GT (!D.Table_Size-1)) THEN $
         cgImage, BytScl(image, Top=!D.Table_Size-1), /Keep_Aspect ELSE $
         cgImage, image, /Keep_Aspect
      IF imageDataType EQ 'NONE' THEN image = 0

      ; Store the image data for later retrieval.
      *(*(info.storagePtr)).image = image
      *(*(info.storagePtr)).fileInfo = fileInfo
      (*info.storagePtr).r = info.r
      (*info.storagePtr).g = info.g
      (*info.storagePtr).b = info.b

      ; Clean up.
      CD, thisDirectory
      Widget_Control, event.top, Set_UValue=info, /No_Copy

   ENDIF

END ;---------------------------------------------------------------------------------



PRO ImageSelect_ReadFiles, extension, filename, info, fileinfo, image, ok, type, _Extra=extra

; Utility routine to read the data file. Returns the image and file information.
; First three parameters are required INPUT, last three are optional OUTPUT.

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      ok = 0
      RETURN
   ENDIF

   ; We need three required parameters.
   IF N_Params() LT 3 THEN Message, 'Called with incorrect number of required parameters.'

   ; Initialize output variables.
   image = BytArr(info.previewsize, info.previewsize)
   fileInfo = {channels:0, dimensions:[info.previewsize, info.previewsize],  type:""}
   ok = 0

   ; Must have non-null filename to continue.
   IF filename EQ "" THEN RETURN

   ; Read the image for display.
   CASE extension OF

      "BMP": BEGIN
         ok = Query_BMP(filename, fileInfo)
         IF ok THEN BEGIN
            IF fileInfo.channels EQ 3 THEN image = Read_BMP(filename, /RGB, _EXTRA=extra) ELSE $
                                                   image = Read_BMP(filename, r, g, b, _EXTRA=extra)
            IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         type = 'BMP'
         ENDCASE

      "DCM": BEGIN
         IF StrUpCase(!Version.OS_Family) EQ 'WINDOWS' THEN BEGIN
            thisRelease = Float(!Version.Release)
            IF (thisRelease GT 6.4) AND (thisRelease LT 8) THEN BEGIN
                IF !Version.Memory_Bits EQ 64 THEN Message, 'This version of IDL does not support reading DICOM files.'
            ENDIF
         ENDIF
         ok = Query_DICOM(filename, fileInfo)
         IF ok THEN BEGIN
            image = Read_Dicom(filename, r, g, b, _EXTRA=extra)
            IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         type = 'DICOM'
         ENDCASE

      "GIF": BEGIN
         ok = Query_GIF(filename, fileInfo)
         IF ok THEN BEGIN
            IF fileInfo.channels EQ 0 THEN fileInfo.channels = 1 ; Bug?
            Read_GIF, filename, image, r, g, b, _EXTRA=extra
            IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         type = 'GIF'
         ENDCASE

      "FITS": BEGIN
         header = Call_Function('HeadFits', filename, /Silent, ERRMSG=ok)
         IF ok EQ "" THEN BEGIN
            ok = 1

          ; Check for unsigned integer
          offset = Call_Function('SXPar', header, 'BZERO', Count=count)
          IF count GT 0 THEN BEGIN
             IF offset EQ '32768' THEN BEGIN
                image = Call_Function("MRDFITS", filename, /Unsigned, _EXTRA=extra, /Silent)
             ENDIF ELSE BEGIN
                image = Call_Function("MRDFITS", filename, _EXTRA=extra, /Silent, /FScale)
             ENDELSE
          ENDIF ELSE image = Call_Function("MRDFITS", filename, _EXTRA=extra, /Silent, /FScale)

          ; Construct a fileInfo structure for this image.
          dims = Image_Dimensions(image, XSize=xsize, YSize=ysize, TrueIndex=trueindex)
          fileinfo = {has_palette:0, channels:1, dimensions:dims, image_index:0L, $
               num_images:1L, pixel_type:Size(image, /Type), type:"FITS", header:header}
          IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         type = 'FITS'
         ENDCASE

      "FIT": BEGIN
         header = Call_Function('HeadFits', filename, /Silent, ERRMSG=ok)
         IF ok EQ "" THEN BEGIN
            ok = 1

          ; Check for unsigned integer
          offset = Call_Function('SXPar', header, 'BZERO', Count=count)
          IF count GT 0 THEN BEGIN
             IF offset EQ '32768' THEN BEGIN
                image = Call_Function("MRDFITS", filename, /Unsigned, _EXTRA=extra, /Silent)
             ENDIF ELSE BEGIN
                image = Call_Function("MRDFITS", filename, _EXTRA=extra, /Silent, /FScale)
             ENDELSE
          ENDIF ELSE image = Call_Function("MRDFITS", filename, _EXTRA=extra, /Silent, /FScale)

          ; Construct a fileInfo structure for this image.
            dims = Image_Dimensions(image, XSize=xsize, YSize=ysize, TrueIndex=trueindex)
            fileinfo = {has_palette:0, channels:1, dimensions:dims, image_index:0L, $
               num_images:1L, pixel_type:Size(image, /Type), type:"FITS", header:header}
            IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         type = 'FITS'
         ENDCASE

      "FTS": BEGIN
         header = Call_Function('HeadFits', filename, /Silent, ERRMSG=ok)
         IF ok EQ "" THEN BEGIN
            ok = 1

          ; Check for unsigned integer
          offset = Call_Function('SXPar', header, 'BZERO', Count=count)
          IF count GT 0 THEN BEGIN
             IF offset EQ '32768' THEN BEGIN
                image = Call_Function("MRDFITS", filename, /Unsigned, _EXTRA=extra, /Silent)
             ENDIF ELSE BEGIN
                image = Call_Function("MRDFITS", filename, _EXTRA=extra, /Silent, /FScale)
             ENDELSE
          ENDIF ELSE image = Call_Function("MRDFITS", filename, _EXTRA=extra, /Silent, /FScale)

          ; Construct a fileInfo structure for this image.
            dims = Image_Dimensions(image, XSize=xsize, YSize=ysize, TrueIndex=trueindex)
            fileinfo = {has_palette:0, channels:1, dimensions:dims, image_index:0L, $
               num_images:1L, pixel_type:Size(image, /Type), type:"FITS", header:header}
            IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         type = 'FITS'
         ENDCASE

      "JPEG": BEGIN
         ok = Query_JPEG(filename, fileInfo)
         IF ok THEN $
         BEGIN
            Device, Get_Visual_Depth = theDepth
            IF theDepth GT 8 THEN BEGIN
               Read_JPEG, filename, image, True=1, _EXTRA=extra
            ENDIF ELSE BEGIN
               Read_JPEG, filename, image, colortable, Dither=1, Colors=!D.Table_Size, _EXTRA=extra
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
               TVLCT, r, g, b
            ENDELSE
            IF fileInfo.has_palette EQ 1 THEN $
            BEGIN
                  r = colortable[*,0]
                  g = colortable[*,1]
                  b = colortable[*,2]
               TVLCT, r, g, b
            ENDIF
         ENDIF
         type = 'JPEG'
         ENDCASE

      "JPG": BEGIN
         ok = Query_JPEG(filename, fileInfo)
         IF ok THEN $
         BEGIN
            Device, Get_Visual_Depth = theDepth
            IF theDepth GT 8 THEN BEGIN
               Read_JPEG, filename, image, True=1, _EXTRA=extra
            ENDIF ELSE BEGIN
               Read_JPEG, filename, image, colortable, Dither=1, Colors=!D.Table_Size, _EXTRA=extra
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
               TVLCT, r, g, b
            ENDELSE
            IF fileInfo.has_palette EQ 1 THEN $
            BEGIN
                  r = colortable[*,0]
                  g = colortable[*,1]
                  b = colortable[*,2]
               TVLCT, r, g, b
            ENDIF
         ENDIF
         type = 'JPEG'
         ENDCASE

      "JPE": BEGIN
         ok = Query_JPEG(filename, fileInfo)
         IF ok THEN $
         BEGIN
            Device, Get_Visual_Depth = theDepth
            IF theDepth GT 8 THEN BEGIN
               Read_JPEG, filename, image, True=1, _EXTRA=extra
            ENDIF ELSE BEGIN
               Read_JPEG, filename, image, colortable, Dither=1, Colors=!D.Table_Size, _EXTRA=extra
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
               TVLCT, r, g, b
            ENDELSE
            IF fileInfo.has_palette EQ 1 THEN $
            BEGIN
                  r = colortable[*,0]
                  g = colortable[*,1]
                  b = colortable[*,2]
               TVLCT, r, g, b
            ENDIF
         ENDIF
         type = 'JPEG'
         ENDCASE

      "JPF": BEGIN
         ok = Query_JPEG2000(filename, fileInfo)
         IF ok THEN BEGIN
            image = Read_JPEG2000(filename, r, g, b, _EXTRA=extra)
            IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         type = 'JPEG2000'
         ENDCASE

      "JPX": BEGIN
         ok = Query_JPEG2000(filename, fileInfo)
         IF ok THEN BEGIN
            image = Read_JPEG2000(filename, r, g, b, _EXTRA=extra)
            IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         type = 'JPEG2000'
         ENDCASE

      "JP2": BEGIN
         ok = Query_JPEG2000(filename, fileInfo)
         IF ok THEN BEGIN
            image = Read_JPEG2000(filename, r, g, b, _EXTRA=extra)
            IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         type = 'JPEG2000'
         ENDCASE

      "J2C": BEGIN
         ok = Query_JPEG2000(filename, fileInfo)
         IF ok THEN BEGIN
            image = Read_JPEG2000(filename, r, g, b, _EXTRA=extra)
            IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         type = 'JPEG2000'
         ENDCASE

      "J2K": BEGIN
         ok = Query_JPEG2000(filename, fileInfo)
         IF ok THEN BEGIN
            image = Read_JPEG2000(filename, r, g, b, _EXTRA=extra)
            IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         type = 'JPEG2000'
         ENDCASE

      "PICT": BEGIN
         ok = Query_PICT(filename, fileInfo)
         IF ok THEN BEGIN
            Read_PICT, filename, image, r, g, b, _EXTRA=extra
            IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         type = 'PICT'
         ENDCASE

      "PGM": BEGIN
         ok = Query_PPM(filename, fileInfo)
         IF ok THEN BEGIN
            Read_PPM, filename, image, _EXTRA=extra
            TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         type = 'PGM'
         ENDCASE

      "PPM": BEGIN
         ok = Query_PPM(filename, fileInfo)
         IF ok THEN BEGIN
            Read_PPM, filename, image, _EXTRA=extra
            TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         type = 'PPM'
         ENDCASE

      "PNG": BEGIN
         ok = Query_PNG(filename, fileInfo)
         IF ok THEN BEGIN
            image = Read_PNG(filename, r, g, b, _EXTRA=extra)
            IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
            
            ; Some PNG files (from ImageMagick, for example) are created with 16-bits per channel.
            ; Convert this to 8-bits per channel, and also remove any alpha channel for this application.
            IF Size(image, /TNAME) NE 'BYTE' THEN image = BytScl(image)
            IF Size(image, /N_DIMENSIONS) GT 3 THEN BEGIN
                dims = Image_Dimensions(image, TRUEINDEX=trueindex)
                CASE trueIndex OF
                    0: image = image[0:2, *, *]
                    1: image = image[*, 0:2, *]
                    2: image = image[*, *, 0:2]
                ENDCASE
            ENDIF
         ENDIF
         type = 'PNG'
         ENDCASE

      "TIF": BEGIN
         ok = Query_TIFF(filename, fileInfo, GEOTIFF=geotiff)
         IF ok THEN BEGIN
            CASE fileInfo.channels OF
               3: image = Read_TIFF(filename, _EXTRA=extra, ORIENTATION=orientation)
               ELSE: image = Read_TIFF(filename, r, g, b, _EXTRA=extra, ORIENTATION=orientation)
            ENDCASE
            IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
            IF Size(geotiff, /TNAME) EQ 'STRUCT' THEN BEGIN

               ; Save the geotiff variable at the IDL main level.
               (Scope_VarFetch('geotiff', LEVEL=1, /ENTER)) = geotiff
            
            ENDIF
         ENDIF ELSE orientation = 1
         IF orientation EQ 1 THEN BEGIN
             dims = Image_Dimensions(image, YINDEX=yindex)
             image = Reverse(Temporary(image), yindex+1)
         ENDIF
         IF Size(geotiff, /TNAME) EQ 'STRUCT' THEN type = 'GEOTIFF' ELSE type = 'TIFF'
         ENDCASE

      "TIFF": BEGIN
         ok = Query_TIFF(filename, fileInfo, GEOTIFF=geotiff)
         IF ok THEN BEGIN
            CASE fileInfo.channels OF
               3: image = Read_TIFF(filename, _EXTRA=extra, ORIENTATION=orientation)
               ELSE: image = Read_TIFF(filename, r, g, b, _EXTRA=extra, ORIENTATION=orientation)
            ENDCASE
            IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
            IF Size(geotiff, /TNAME) EQ 'STRUCT' THEN BEGIN
               
               ; Save the geotiff variable at the IDL main level.
               (Scope_VarFetch('geotiff', LEVEL=1, /ENTER)) = geotiff
            
            ENDIF
         ENDIF ELSE orientation = 1
         IF orientation EQ 1 THEN BEGIN
             dims = Image_Dimensions(image, YINDEX=yindex)
             image = Reverse(Temporary(image), yindex+1)
         ENDIF
         IF Size(geotiff, /TNAME) EQ 'STRUCT' THEN type = 'GEOTIFF' ELSE type = 'TIFF'
         ENDCASE

      ELSE: ok = 0

   ENDCASE

   ; If there was a problem, report it and indicate an invalid file.
   IF ~ok THEN BEGIN
      void = Dialog_Message('Selected file is not a valid ' + extension + ' type. Returning.')
      fileInfo.channels = -1
      RETURN
   ENDIF
   
   ; Store RGB vectors if they got set.
   IF N_Elements(r) NE 0 THEN info.r = r ELSE info.r = Bindgen(!D.Table_Size)
   IF N_Elements(g) NE 0 THEN info.g = g ELSE info.g = Bindgen(!D.Table_Size)
   IF N_Elements(b) NE 0 THEN info.b = b ELSE info.b = Bindgen(!D.Table_Size)

END ; ------------------------------------------------------------------------------------


PRO ImageSelect_SetFilter, event

; This event handler responds to events on the Filter button.

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message()
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   ; This event handler sets the filter for image data files.
   Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; The filter is in the User Value of the button. Store it.
   Widget_Control, event.id, Get_UValue=theFilter
   *info.filter = theFilter

   ; Put a check mark next to this button.
   parent = Widget_Info(event.id, /Parent)
   child = Widget_Info(parent, /Child)
   WHILE child  NE 0 DO BEGIN
      IF child NE event.id THEN Widget_Control, child, Set_Button=0 ELSE Widget_Control, child, Set_Button=1
      child = Widget_Info(child, /Sibling)
   ENDWHILE

   ; Get the current filename.
   Widget_Control, info.filenameID, Get_Value=filename

   ; Set the new filter in the Filename compound widget.
   info.filenameObj->SetProperty, Filter=theFilter

   ; Look in the data directory for the files.
   CD, info.dataDirectory, Current=thisDirectory

   ; Locate appropriate files.
   FOR j=0, N_Elements(*info.filter)-1 DO BEGIN

      specificFiles = File_Search((*info.filter)[j], Count=fileCount)
      IF fileCount GT 0 THEN IF N_Elements(theFiles) EQ 0 THEN $
         theFiles = specificFiles[ImageSelect_BSort(StrLowCase(specificFiles))] ELSE $
         theFiles = [theFiles, specificFiles[ImageSelect_BSort(StrLowCase(specificFiles))]]
   ENDFOR
   fileCount = N_Elements(theFiles)
   IF fileCount EQ 0 THEN BEGIN
      theFiles = ""
      filename = ""
   ENDIF ELSE BEGIN
      IF Ptr_Valid(info.excludeFiles) THEN BEGIN
        FOR k=0,N_Elements(*info.excludeFiles)-1 DO BEGIN
           index = Where(StrUpCase(theFiles) EQ StrUpCase((*info.excludeFiles)[k]), count, COMPLEMENT=ok)
           IF count GT 0 THEN theFiles = theFiles[ok]
        ENDFOR
      ENDIF
      filename = theFiles[0]
   ENDELSE

   ; Update the widget interface according to what you found.
   Widget_Control, info.filenameID, Set_Value=filename
   Widget_Control, info.fileListID, Set_Value=theFiles
   IF fileCount GT 0 THEN Widget_Control, info.fileListID, Set_List_Select=0
   *info.theFiles = theFiles

   ; Try to read the image
   thisExtension = ImageSelect_FileExtension(filename)
   ImageSelect_ReadFiles, thisExtension, filename, info, fileinfo, $
        image, ok, type, _Extra=*((*info.storagePtr).extra)

   ; What kind of image is this?
   CASE fileinfo.channels OF
      3: imageType = 'True-Color ' + type + ' Image'
      0: imageType = 'No Image'
      -1: imageType = 'Invalid Image File'
      ELSE: imageType = '2D ' + type + ' Image'
   ENDCASE

   ; Get the file sizes. Dicom images can report incorrect sizes,
   ; which is what we are trying to fix in the ysize line.
   xsize = fileInfo.dimensions[0]
   ysize = fileInfo.dimensions[1] > Fix(xsize * 0.5)

   ; Get the file sizes.
   dimensions = Image_Dimensions(image, XSize=xsize, YSize=ysize, YIndex=yindex)

   ; Flip the image if required.
   IF info.flipimage THEN image = Reverse(image, yindex+1)

   ; Calculate a window size for the image preview.
   aspect = Float(xsize) / ysize
   IF aspect GT 1 THEN BEGIN
      wxsize = Fix(info.previewSize)
      wysize = Fix(info.previewSize / aspect) < info.previewSize
   ENDIF ELSE BEGIN
      wysize = Fix(info.previewSize)
      wxsize = Fix(info.previewSize / aspect) < info.previewSize
   ENDELSE

   ; If you don't have an image, then get sensible numbers for the labels.
   IF imageType EQ 'No Image' OR imageType EQ 'Invalid Image File' THEN BEGIN
      xsize = 0
      ysize = 0
      minval = 0
      maxval = 0
   ENDIF

   ; Update the display with what you have.
   IF imageType EQ 'No Image' THEN imageDataType = 'NONE' ELSE imageDataType = Size(image, /TNAME)
   IF imageType EQ 'Invalid Image File' THEN imageDataType = 'INVALID'
   Widget_Control, info.labelTypeID, Set_Value=imageType
   Widget_Control, info.labelXSizeID, Set_Value="X Size: " + StrTrim(xsize, 2)
   Widget_Control, info.labelYSizeID, Set_Value="Y Size: " + StrTrim(ysize, 2)
   Widget_Control, info.labelDataTypeID, Set_Value="Type: " + imageDataType
   Widget_Control, info.labelminvalID, Set_Value="Min Value: " + cgNumber_Formatter(Min(image))
   Widget_Control, info.labelmaxvalID, Set_Value="Max Value: " + cgNumber_Formatter(Max(image))

   ; Draw the preview image.
   WSet, info.previewWID
   sizes = ImageSelect_WindowSize(image, XSIZE=info.previewSize, YSIZE=info.previewSize)
   Widget_Control, info.previewID, Draw_XSize=sizes[0], Draw_YSize=sizes[1]
   TVLCT, info.r, info.g, info.b
   IF (Min(image) LT 0) OR (Max(image) GT (!D.Table_Size-1)) THEN $
      cgImage, BytScl(image, Top=!D.Table_Size-1), /Keep_Aspect ELSE $
      cgImage, image, /Keep_Aspect
   IF imageDataType EQ 'NONE' THEN image = 0

   ; Save the image data for later retrieval.
   *(*(info.storagePtr)).image = image
   *(*(info.storagePtr)).fileInfo = fileInfo
   (*info.storagePtr).r = info.r
   (*info.storagePtr).g = info.g
   (*info.storagePtr).b = info.b

   ; Clean up.
   CD, thisDirectory
   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ; ----------------------------------------------------------------------------------------





FUNCTION ImageSelect, $
   BMP=bmp, $                      ; Set this keyword to select BMP files.
   Cancel=cancel, $                ; An output keyword. Returns 0 if the ACCEPT button is used, 1 otherwise.
   Demo=demo, $                    ; If set, check the $IDL_DIR/examples/data directory first.
   Dicom=dicom, $                  ; Set this keyword to select DICOM files
   Directory=directory, $          ; Initial directory to search for files.
   Examples=examples, $            ; If set, check the $IDL_DIR/examples/data directory first.
   Exclude=exclude, $              ; A list of files that should be excluded from the list of available files.
   _Extra=extra, $                 ; This is used to pass keywords on to READ_XXX routines.
   FileInfo=fileInfo, $            ; An output keyword containing file information from the Query_*** routine.
   Filename=filename, $            ; Initial file name of image file.
   Filter = filter, $
   FITS=fits, $                    ; Set this keyword to select FITS files. (Must have NASA Astro Library on !PATH)
   Flipimage=flipimage, $          ; Set this keyword to flip the Y indices of the image. Set to 0 by default.
   GEOTIFF=geotiff, $              ; Returns the geotiff structure of GeoTIFF files.
   GIF=gif, $                      ; Set this keyword to select GIF files. (IDL 6.2 and above, only)
   Group_Leader=group_leader, $    ; The group leader ID of this widget program.
   FHeader=fheader, $              ; Output keyword returns FITS header from FITS files.
   J2000=j2000, $                  ; Set this keyword to select JPEG2000 files (IDL 6.1 and above, only.)
   J2K=j2k, $                      ; Set this keyword to select JPEG2000 files (IDL 6.1 and above, only.)
   JPEG=jpeg, $                    ; Set this keyword to select JPEG files
   LISTXSIZE=listxsize, $          ; Set this keyword to the XSIZE of the list widget.
   OFFSETS=offsets, $              ; The offsets of the program.
   ONLY2D=only2d, $                ; Set this keyword so that only 2D images can be accepted.
   ONLY3D=only3d, $                ; Set this keyword so that only 3D or true-color images can be accepted.
   OutDirectory=outdirectory, $    ; The directory name of the selected image file.
   OutFilename=outfilename, $      ; The short filename (without directory) of the selected image file.
   Palette=palette, $              ; The color palette associated with the file.
   PICT=pict, $                    ; Set this keyword to select PICT files
   PGM=pgm, $                      ; Set this keyword to read PGM files.
   PPM=ppm, $                      ; Set this keyword to read PPM files.
   PNG=png, $                      ; Set this keyword to select PNG files.
   SILENT=silent, $                ; Set this keyword to run off educational message.
   TIFF=tiff, $                    ; Set this keyword to select TIFF files.
   TITLE=title, $                  ; The title of the main image selection window.
   PreviewSize=previewsize         ; The maximum size of the image preview window. 150 pixels by default.

CD, Current=originalDir

; First thing we are going to do is check for the availability of FITS files. We are
; going to look for MRDFITS on the path. If we don't find it, we will generate an error
; condition and handle it silently.
skip = 0
Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   skip = 1
   haveFits = 0
ENDIF
IF skip EQ 0 THEN BEGIN
   RESOLVE_ROUTINE, 'MRDFITS', /IS_FUNCTION, /NO_RECOMPILE
   haveFits = 1
ENDIF

; Normal error handling.
Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   Cancel = 1
   ok = Error_Message()
   RETURN, 0
ENDIF

; Set up the info structure.
info = { storagePtr: Ptr_New(), $            ; The "outside the program" storage pointer.
         previewID: 0L, $                    ; The ID of the preview draw widget.
         previewWID: 0L, $                   ; The window index number of the preview draw widget.
         r:BytArr(256), $                    ; The R color vector.
         g:BytArr(256), $                    ; The G color vector.
         b:BytArr(256), $                    ; The B color vector.
         theFiles: Ptr_New(), $              ; The current list of files in the directory.
         excludeFiles: Ptr_New(), $          ; The list of files to exclude from the file list.
         filenameID: 0L, $                   ; The identifier of the FileSelect compound widget.
         fileListID: 0L, $                   ; The identifier of the file list widget.
         flipimage:0L, $                     ; A flag to flip the image Y order.
         previewSize: 0L, $                  ; The default size of the preview window.
         acceptID: 0L, $                     ; The idenfier of the Accept button widget.
         only2d: 0L, $                       ; A flag that permits only the acceptance of 2D images.
         only3d: 0L, $                       ; A flag that permits only the acceptance of true-color images.
         filter: Ptr_New(), $                ; The file filter.
         filenameObj: Obj_New(), $           ; The FileSelect compound widget object reference.
         originalDir: "", $                  ; The starting or original directory.
         dataDirectory: "", $                ; The current data directory.
         display: 0B, $                      ; Display the image after reading?
         labelmaxvalID: 0L, $                ; The ID of the Max Value label.
         labelminvalID: 0L, $                ; The ID of the Max Value label.
         labelTypeID: 0L, $                  ; The ID of the Image Type label.
         labelXSizeID: 0L, $                 ; The ID of the X Size label.
         labelYSizeID: 0L, $                 ; The ID of the Y Size label.
         labelDataTypeID: 0L $               ; The ID of the Data Type label.
       }

; Check for availability of GIF and JPEG2000 files.
thisVersion = Float(!Version.Release)
IF ((thisVersion LT 5.3) OR (thisVersion GE 6.2)) THEN haveGif = 1 ELSE haveGIF = 0
IF thisVersion GE 6.1 THEN BEGIN
   haveJ2000 = 1
   scopeOK = 1 ; Use ScopeLevel.
ENDIF ELSE BEGIN
   haveJ2000 = 0
   scopeOK = 0 ; Use Routine_Names.
ENDELSE

; Set up the filter.
IF Keyword_Set(bmp) THEN IF N_Elements(filter) EQ 0 THEN filter = ["*.bmp"] ELSE filter = [filter, "*.bmp"]
IF Keyword_Set(dicom) THEN IF N_Elements(filter) EQ 0 THEN filter = ["*.dcm"] ELSE filter = [filter, "*.dcm"]
flipimage = Keyword_Set(flipimage)
IF Keyword_Set(examples) THEN demo = 1
demo = Keyword_Set(demo)
IF N_Elements(exclude) NE 0 THEN info.excludeFiles = Ptr_New(exclude)
IF Keyword_Set(gif) THEN BEGIN
   IF havegif THEN BEGIN
      IF N_Elements(filter) EQ 0 THEN filter = ["*.gif"] ELSE filter = [filter, "*.gif"]
   ENDIF ELSE ok = Dialog_Message('GIF files are not available in this version of IDL. Continuing.')
ENDIF
IF haveFits THEN BEGIN
   IF Keyword_Set(fits) THEN IF N_Elements(filter) EQ 0 THEN filter = ["*.fits", "*.fts", "*.fit"] ELSE $
      filter = [filter, "*.fits", "*.fts", "*.fit"]
ENDIF
IF Keyword_Set(jpeg) THEN IF N_Elements(filter) EQ 0 THEN filter = ["*.jpg", "*.jpeg", "*.jpe"] ELSE $
   filter = [filter, "*.jpg", "*.jpeg", "*.jpe"]
IF Keyword_Set(j2000) OR Keyword_Set(j2k) THEN BEGIN
   IF haveJ2000 THEN BEGIN
      IF N_Elements(filter) EQ 0 THEN filter = ["*.jpf", "*.jpx", "*.jp2", "*.j2c", "*.j2k"] ELSE $
         filter = [filter, "*.jpf", "*.jpx", "*.jp2", "*.j2c", "*.j2k"]
   ENDIF ELSE ok = Dialog_Message('JPEG2000 files are not available in this version of IDL. Continuing.')
ENDIF
IF Keyword_Set(pict) THEN IF N_Elements(filter) EQ 0 THEN filter = ["*.pict"] ELSE filter = [filter, "*.pict"]
IF Keyword_Set(png) THEN IF N_Elements(filter) EQ 0 THEN filter = ["*.png"] ELSE filter = [filter, "*.png"]
IF Keyword_Set(tiff) THEN IF N_Elements(filter) EQ 0 THEN filter = ["*.tif"] ELSE filter = [filter, "*.tif"]
IF N_Elements(filter) EQ 0 THEN BEGIN
   filter = ['*.bmp', '*.dcm', '*.jpg', "*.jpeg", "*.jpe", '*.pict', '*.ppm', '*.pgm', '*.png', '*.tif']
   IF haveGif THEN filter = [filter, "*.gif"]
   IF havej2000 THEN filter = [filter, "*.jpf", "*.jpx", "*.jp2", "*.j2c", "*.j2k"]
   IF haveFits THEN filter = [filter, "*.fits", "*.fts", "*.fit"]
   allextensions = 1
ENDIF ELSE allextensions = 0
only2D = Keyword_Set(only2d)
only3D = Keyword_Set(only3d)
IF N_Elements(title) EQ 0 THEN title = 'Select Image File'

; Get the current directory. Some processing involved.
IF Keyword_Set(demo) THEN BEGIN
   path = Filepath(SubDir=['examples', 'data'], 'junk.pro')
   directory = File_Dirname(path)
ENDIF
CD, Current=startDirectory
IF N_Elements(directory) EQ 0 THEN directory = startDirectory ELSE BEGIN
   IF StrMid(directory, 0, 2) EQ ".." THEN BEGIN
      CASE StrUpCase(!Version.OS_Family) OF
      'MACOS': BEGIN
         CD, '..'
         CD, Current=basename
         directory = basename + StrMid(directory, 3)
         END
      'VMS': BEGIN
         CD, '..'
         CD, Current=basename
         directory = basename + StrMid(directory, 3)
         END
      ELSE: BEGIN
         CD, '..'
         CD, Current=basename
         directory = basename + StrMid(directory, 2)
         END
      ENDCASE
   ENDIF
   IF StrMid(directory, 0, 1) EQ "." THEN BEGIN
      CASE StrUpCase(!Version.OS_Family) OF
      'MACOS': BEGIN
         CD, Current=basename
         directory = basename + StrMid(directory, 2)
         END
      'VMS': BEGIN
         CD, Current=basename
         directory = basename + StrMid(directory, 2)
         END
      ELSE: BEGIN
         CD, Current=basename
         directory = basename + StrMid(directory, 1)
      END
      ENDCASE
   ENDIF
ENDELSE
CD, directory

; Check other keyword values.
IF N_Elements(filename) EQ 0 THEN file = "" ELSE BEGIN
   dir=StrMid(filename, 0, StrPos(filename, Path_Sep(), /REVERSE_SEARCH))
   IF dir NE "" THEN BEGIN
      directory = dir
      CD, directory
      file = StrMid(filename, StrLen(directory)+1)
   ENDIF ELSE file = filename
ENDELSE
IF N_Elements(previewSize) EQ 0 THEN previewSize = 150
info.previewSize = previewSize

; Locate appropriate files.
FOR j=0, N_Elements(filter)-1 DO BEGIN
   specificFiles = File_Search(filter[j], Count=fileCount)
   IF fileCount GT 0 THEN IF N_Elements(theFiles) EQ 0 THEN $
      theFiles = specificFiles[ImageSelect_BSort(StrLowCase(specificFiles))] ELSE $
      theFiles = [theFiles, specificFiles[ImageSelect_BSort(StrLowCase(specificFiles))]]
ENDFOR
fileCount = N_Elements(theFiles)
IF fileCount EQ 0 THEN theFiles = "" ELSE BEGIN
   theFiles = theFiles[ImageSelect_BSort(theFiles)]
   IF Ptr_Valid(info.excludeFiles) THEN BEGIN
      FOR k=0,N_Elements(*info.excludeFiles)-1 DO BEGIN
         index = Where(StrUpCase(theFiles) EQ StrUpCase((*info.excludeFiles)[k]), count, COMPLEMENT=ok)
         IF count GT 0 THEN theFiles = theFiles[ok]
      ENDFOR
   ENDIF
   IF file EQ "" THEN file = theFiles[0]
ENDELSE

; Try to read the file.
extension = ImageSelect_FileExtension(file)
ImageSelect_ReadFiles, extension, file, info, fileinfo, image, ok, type, _Extra=extra

; Get the file sizes.
dimensions = Image_Dimensions(image, XSize=xsize, YSize=ysize, YIndex=yindex)

; Flip the image if required.
IF flipimage THEN image = Reverse(image, yindex+1)

; Do you have a group leader? Here is my attempt to educate
; people to its use. I'll create a group leader if I have
; to, but it's ugly and I don't want to. I rule it out for
; RUNTIME and VM use. You should know better. :-)
IF scopeOK THEN scopeLevel = Scope_Level() ELSE scopeLevel = Routine_Names(/Level)
IF scopeLevel GT 2 THEN BEGIN
      IF N_Elements(group_leader) EQ 0 THEN BEGIN
         IF ~Keyword_Set(silent) THEN BEGIN
            CASE 1 OF
               LMGR(/VM): Message, 'In a VM application, a GROUP_LEADER is required for MODAL operation.'
               LMGR(/RUNTIME): Message, 'In a RUNTIME application, a GROUP_LEADER is required for MODAL operation.'
               ELSE:
            ENDCASE
         ENDIF
         group_leader = Widget_Base(Map=0)
         Widget_Control, group_leader, /Realize
         destroy_groupleader = 1
      ENDIF ELSE destroy_groupleader = 0
ENDIF ELSE destroy_groupleader = 0

; Create the widgets.
IF N_Elements(group_leader) NE 0 THEN BEGIN
   tlb = Widget_Base(Title=title, Column=1, /Base_Align_Center, $
      /Modal, Group_Leader=group_leader)
ENDIF ELSE BEGIN
   tlb = Widget_Base(Title=title, Column=1, /Base_Align_Center)
ENDELSE

fileSelectBase = Widget_Base(tlb, Column=1, Frame=1)
buttonBase = Widget_Base(tlb, Row=1)

; Define file selection widgets.
filenameID = FSC_FileSelect(fileSelectBase, Filename=file, ObjectRef=filenameObj,$
   Directory=directory, Event_Pro='ImageSelect_FilenameEvents', Filter=filter, _Extra=extra)
fsrowbaseID = Widget_Base(fileSelectBase, Row=1, XPad=10)
IF N_Elements(listxsize) EQ 0 THEN listxsize = Max(StrLen(theFiles)) + 0.1*Max(StrLen(theFiles)) > 30
filelistID = Widget_List(fsrowbaseID, Value=theFiles, YSize = 10, XSize=listxsize, $
   Event_Pro='ImageSelect_ListEvents')
spacer = Widget_Label(fsrowbaseID, Value="     ")
previewID = Widget_Draw(fsrowbaseID, XSize=previewSize, YSize=previewSize)
spacer = Widget_Label(fsrowbaseID, Value="     ")
labelBaseID = Widget_Base(fsrowbaseID, Column=1, /Base_Align_Left)

; What kind of image is this?
   CASE fileinfo.channels OF
      3: imageType = 'True-Color ' + type + ' Image'
      0: imageType = 'No Image'
      -1: imageType = 'Invalid Image File'
      ELSE: imageType = '2D ' + type + ' Image'
   ENDCASE


xsize = fileInfo.dimensions[0]
ysize = fileInfo.dimensions[1] > Fix(xsize * 0.5)
imageDataType = Size(image, /TNAME)
IF imageType EQ 'Invalid Image File' THEN imageDataType = 'INVALID'
IF imageType EQ 'No Image' THEN imageDataType = 'NONE'

; If you don't have an image, then get sensible numbers for the labels.
IF imageType EQ 'No Image' OR imageType EQ 'Invalid Image File' THEN BEGIN
   xsize = 0
   ysize = 0
   minval = 0
   maxval = 0
ENDIF

labeltypeID = Widget_Label(labelBaseID, Value=imageType, /Dynamic_Resize)
labelxsizeID = Widget_Label(labelBaseID, Value="X Size: " + StrTrim(xsize, 2), /Dynamic_Resize)
labelysizeID = Widget_Label(labelBaseID, Value="Y Size: " + StrTrim(ysize, 2), /Dynamic_Resize)
labeldataTypeID = Widget_Label(labelBaseID, Value="Type: " + imageDataType, /Dynamic_Resize)
labelminvalID = Widget_Label(labelBaseID, Value="Min Value: " + cgNumber_Formatter(Min(image)), /Dynamic_Resize)
labelmaxvalID = Widget_Label(labelBaseID, Value="Max Value: " + cgNumber_Formatter(Max(image)), /Dynamic_Resize)

; Size the draw widget appropriately.
; Calculate a window size for the image preview.
IF xsize NE ysize THEN BEGIN
   aspect = Float(ysize) / xsize
   IF aspect LT 1 THEN BEGIN
      wxsize = previewSize
      wysize = (previewSize * aspect) < previewSize
   ENDIF ELSE BEGIN
      wysize = previewSize
      wxsize = (previewSize / aspect) < previewSize
   ENDELSE
ENDIF

; Can you find the filename in the list of files? If so,
; highlight it in the list.
index = Where(StrUpCase(theFiles) EQ StrUpCase(file), count)
IF count GT 0 THEN Widget_Control, filelistID, Set_List_Select=index

; Define buttons widgets.
button = Widget_Button(buttonBase, Value='Cancel', Event_Pro='ImageSelect_Action')
filterID = Widget_Button(buttonBase, Value='Image Type...', /Menu, Event_Pro='ImageSelect_SetFilter')
button = Widget_Button(filterID, Value='BMP Files', UValue=['*.bmp'], /Checked_Menu)
IF Keyword_Set(bmp) THEN Widget_Control, button, Set_Button=1
button = Widget_Button(filterID, Value='DICOM Files', UValue=['*.dcm'], /Checked_Menu)
IF Keyword_Set(dicom) THEN Widget_Control, button, Set_Button=1
IF haveFits THEN BEGIN
   button = Widget_Button(filterID, Value='FITS Files', UValue=['*.fits', '*.fts', '*.fit'], /Checked_Menu)
   IF Keyword_Set(fits) THEN Widget_Control, button, Set_Button=1
ENDIF
IF havegif THEN BEGIN
   button = Widget_Button(filterID, Value='GIF Files', UValue=['*.gif'], /Checked_Menu)
   IF Keyword_Set(gif) THEN Widget_Control, button, Set_Button=1
ENDIF
button = Widget_Button(filterID, Value='JPEG Files', UValue=['*.jpg', '*.jpeg', '*.jpe'], /Checked_Menu)
IF Keyword_Set(jpeg) THEN Widget_Control, button, Set_Button=1
IF haveJ2000 THEN BEGIN
   button = Widget_Button(filterID, Value='JPEG2000 Files', UValue=['*.jpf', '*.jpx', '*.jp2', '*.j2c', '*.j2k'], /Checked_Menu)
   IF Keyword_Set(j2000) THEN Widget_Control, button, Set_Button=1
ENDIF
button = Widget_Button(filterID, Value='PICT Files', UValue=['*.pict'], /Checked_Menu)
IF Keyword_Set(pict) THEN Widget_Control, button, Set_Button=1
button = Widget_Button(filterID, Value='PNG Files', UValue=['*.png'], /Checked_Menu)
IF Keyword_Set(png) THEN Widget_Control, button, Set_Button=1
button = Widget_Button(filterID, Value='PPM Files', UValue=['*.pgm', '*.ppm'], /Checked_Menu)
IF Keyword_Set(ppm) THEN Widget_Control, button, Set_Button=1
button = Widget_Button(filterID, Value='PGM Files', UValue=['*.pgm', '*.ppm'], /Checked_Menu)
IF Keyword_Set(pgm) THEN Widget_Control, button, Set_Button=1
button = Widget_Button(filterID, Value='TIFF Files', UValue=['*.tif', '*.tiff'], /Checked_Menu)
IF Keyword_Set(tiff) THEN Widget_Control, button, Set_Button=1
IF haveFits THEN BEGIN
   button = Widget_Button(filterID, Value='All Types', $
      UValue=['*.bmp', '*.dcm', '*.fits', '*.fts', '*.fit', '*.gif', '*.jpg', '*.jpeg', '*.jpe', '*.jpf', '*.jpx', $
      '*.jp2', '*.j2c', '*.j2k', '*.pict', '*.png', '*.ppm', '*.pgm', '*.tif', '*.tiff'], /Checked_Menu)
ENDIF ELSE BEGIN
   button = Widget_Button(filterID, Value='All Types', $
      UValue=['*.bmp', '*.dcm', '*.gif', '*.jpg', '*.jpeg', '*.jpe', '*.jpf', '*.jpx', $
      '*.jp2', '*.j2c', '*.j2k', '*.pict', '*.png', '*.ppm', '*.pgm', '*.tif', '*.tiff'], /Checked_Menu)
ENDELSE
IF allextensions THEN Widget_Control, button, Set_Button=1
button = Widget_Button(buttonBase, Value='Flip Image', Event_Pro='ImageSelect_FlipImage')
acceptID = Widget_Button(buttonBase, Value='Accept', Event_Pro='ImageSelect_Action')
IF only2d THEN BEGIN
   IF fileinfo.channels NE 1 THEN Widget_Control, acceptID, Sensitive=0
   Widget_Control, tlb, TLB_Set_Title=title + ' (2D Images Only)'
ENDIF

IF only3d THEN BEGIN
   IF fileinfo.channels NE 3 THEN Widget_Control, acceptID, Sensitive=0
   Widget_Control, tlb, TLB_Set_Title=title + ' (True-Color Images Only)'
ENDIF

; Center the TLB or use offsets, if defined.
IF N_Elements(offsets) NE 0 THEN BEGIN
   IF N_Elements(offsets) NE 2 THEN offsets = [offsets[0], offsets[0]]
   Widget_Control, tlb, XOffset=offsets[0], YOffset=offsets[1]
ENDIF ELSE BEGIN
   cgCenterTLB, tlb
   offsets = LonArr(2)
ENDELSE
Widget_Control, tlb, /Realize

; Display the image.
Widget_Control, previewID, Get_Value=previewWID
WSet, previewWID
sizes = ImageSelect_WindowSize(image, XSIZE=previewSize, YSIZE=previewSize)
Widget_Control, previewID, Draw_XSize=sizes[0], Draw_YSize=sizes[1]
IF (Min(image) LT 0) OR (Max(image) GT (!D.Table_Size-1)) THEN $
   cgImage, BytScl(image, Top=!D.Table_Size-1), /Keep_Aspect ELSE $
   cgImage, image, /Keep_Aspect

; Set up information to run the program.
storagePtr = Ptr_New({cancel:1, image:Ptr_New(image), fileInfo:Ptr_New(fileInfo), offsets:offsets, $
   outdirectory:"", outfilename:"", r:info.r, g:info.g, b:info.b, $
   geotiff:Ptr_New(), extra:Ptr_New(extra), originalDir:originalDir})

; Load the info structure.
info.storagePtr = storagePtr
info.previewID = previewID
info.previewWID = previewWID
info.theFiles = Ptr_New(theFiles)
info.filenameID = filenameID
info.fileListID = fileListID
info.flipImage = flipImage
info.previewSize = previewSize
info.acceptID = acceptID
info.only2d = only2d
info.only3d = only3d
info.filter = Ptr_New(filter)
info.filenameObj = filenameObj
info.dataDirectory = directory
info.display = Keyword_Set(display)
info.labelmaxvalID = labelmaxvalID
info.labelminvalID = labelminvalID
info.labelTypeID = labelTypeID
info.labelXSizeID = labelXSizeID
info.labelYSizeID = labelYSizeID
info.labelDataTypeID = labelDataTypeID
info.originalDir = originalDir

Widget_Control, tlb, Set_UValue=info, /No_Copy

; Blocking or modal widget mode, depending upon presence of GROUP_LEADER.
XManager, "imageselect", tlb, Cleanup='ImageSelect_Cleanup'


; Return collected information.
cancel = (*storagePtr).cancel
fileInfo = *(*storagePtr).fileInfo
type = fileInfo.type
IF Ptr_Valid((*storagePtr).geotiff) THEN geotiff = *(*storagePtr).geotiff ELSE Undefine, geotiff
IF type EQ 'FITS' THEN fheader = fileInfo.header
image = *((*storagePtr).image)
outDirectory = (*storagePtr).outDirectory
outFilename = (*storagePtr).outFilename
originalDir = (*storagePtr).originalDir
CD, originalDir
offsets = (*storagePtr).offsets
Ptr_Free, (*storagePtr).image
Ptr_Free, (*storagePtr).fileInfo
Ptr_Free, (*storagePtr).geotiff
palette = BytArr(256,3)
palette[*,0] = (*storagePtr).r
palette[*,1] = (*storagePtr).g
palette[*,2] = (*storagePtr).b
IF Ptr_Valid((*storagePtr).extra) NE 0 THEN BEGIN
   IF N_Elements(*(*storagePtr).extra) NE 0 THEN extra = *(*storagePtr).extra
ENDIF
Ptr_Free, (*storagePtr).extra
Ptr_Free, storagePtr

; Did you create your own group leader?
IF destroy_groupleader THEN Widget_Control, group_leader, /Destroy

; Return the image.
IF cancel EQ 1 THEN RETURN, 0 ELSE RETURN, image

END ; ----------------------------------------------------------------------------------------
