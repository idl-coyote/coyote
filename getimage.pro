;+
; NAME:
;       GETIMAGE
;
; PURPOSE:
;
;       The purpose of this function is to allow the user to open either
;       unformmated or XDR binary image files of up to eight dimensions.
;
; CATEGORY:
;
;       Widgets, File I/O.
;
; CALLING SEQUENCE:
;
;       image = GETIMAGE(filename)
;
; OPTIONAL INPUTS:
;
;       filename: The name of the file to open for reading.
;
; OPTIONAL KEYWORD PARAMETERS:
;
;       CANCEL: An output variable that can be set to a named variable.
;       The value of the return variable will be 1 if the user clicked
;       the "Cancel" button or if there was a problem reading the file.
;
;       DATATYPE: The "type" of data you wish to read out of the file.
;       The data type corresponds to the Size(image, /TYPE) value. Here
;       are the types supported:
;
;       BYTE                   1 (default)
;       INTEGER                2
;       LONG INTEGER           3
;       FLOAT                  4
;       DOUBLE                 5
;       UNSIGNED INTEGER      12
;       UNSIGNED LONG INTEGER 13
;       64-bit LONG           14
;       64-bit UNSIGNED LONG  15
;
;
;
;       DIMENSIONS: A vector of image dimensions. The default value is [256, 256].
;
;       DIRECTORY: The name of the directory the file is located in. By
;       default the program looks in the "coyote" directory under the
;       main IDL directory, if one exists. Otherwise, it defaults to the
;       current directory.
;
;       ENDIAN: Set this keyword to an integer that indicates the byte
;       ordering of the data file. If you don't know what byte order means,
;       or you don't know anything about the byte order of the data, or
;       if you are sure the data was created on the same type of machine
;       you are now running IDL on, then just accept the default of 0 or
;       "native" ordering. If you are wrong, you will soon know it and you
;       can set the keyword to another value on your next try. :-)
;
;       If you know the machine was created on a big endian machine (such
;       as a Sun or HP workstation), set this value to 1 (Big Endian). If e
;       you are sur the image data was create on a little endian machine (such
;       as a Windows PC or laptop running LINUX), set the value to 2 (Little Endian).
;
;       HEADER: The size of any header information in the file in BYTES.
;       Default is 0.
;
;       HEADDATA: An optional output keyword that will contain the header
;       information read from the file.
;
;       PARENT: The group leader for this widget program. The PARENT is
;       required if GETIMAGE is called from another widget program in order
;       to make this program a MODAL widget program.
;
;       XDR: Set this keyword if the binary file is of XDR type. The default
;       type is "Unformatted".
;
;       XOFFSET: This is the X offset of the program on the display. The
;       program will be placed approximately in the middle of the display
;       by default.
;
;       YOFFSET: This is the Y offset of the program on the display. The
;       program will be placed approximately in the middle of the display
;       by default.
;
; COMMON BLOCKS:
;
;       None.
;
; SIDE EFFECTS:
;
;       A "CANCEL" operation is indicated by a 0 return value.
;       Any error in reading the file results in a 0 return value.
;
; RESTRICTIONS:
;
;      Requires the following Coyote Library files:
;
;      CENTER_TLB
;      ERROR_MESSAGE
;      FSC_FIELD
;
; EXAMPLE:
;       To load the image "galaxy.dat" in the $IDL/examples/data
;       directory, type:
;
;       image = GETIMAGE('galaxy.dat', DIRECTORY=!DIR + '/examples/data', $
;          DIMENSIONS=[256,256], Cancel=cancelled, Parent=event.top)
;       IF NOT cancelled THEN TV, image
;
; MODIFICATION HISTORY:
;       Written by: David Fanning, 3 February 96.
;       Fixed bug that prevented reading INTEGER data. 19 Dec 96.
;       Modifed program for IDL 5 MODAL operation. 19 Oct 97.
;       Added CANCEL keyword. 27 Oct 97. DWF.
;       Fixed CANCLE keyword spelling. Sigh... 29 JUN 98. DWF.
;       Added COYOTE_FIELD, improved appearance. 19 NOV 99. DWF.
;       Updated with latest version of COYOTE_FIELD. 18 FEB 2000. DWF.
;       Added CATCH keyword so the program will break when I want
;       it to. :-) 18 MAR 2000. DWF.
;       Added GROUP_LEADER keyword, which is synonymous with PARENT. 31 MAR 2000. DWF.
;       Updated obsolete PICKFILE call to DIALOG_PICKFILE. 17 JAN 2001. DWF.
;       Extensive update for IDL Programming Techniques, 3rd Edition. 1 November 2006. DWF.
;          XSIZE, YSIZE, CATCH, and FRAMES keyword made obsolete.
;          HEADDATA, ENDIAN, DATATYPE, DIMENSIONS keywords added.
;          Now dependent on FSC_FIELD, ERROR_MESSAGE, and CENTER_TLB from Coyote Library.
;       Added ability to parse fully qualified file names passed from Dialog_Pickfile. 30 Oct 2010. DWF.
;       IF a file name is not passed into the program, it asks the user to select one now. 10 Jan 2011. DWF.
;       Problem with SWAP_ENDIAN keywords fixed. 7 March 2011. DWF.
;-
;
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
PRO GETIMAGE_NULL_EVENTS, event

   ; The purpose of this event handler is to do nothing
   ; and ignore all events that come to it.

END ;-------------------------------------------------------------------------



PRO GETIMAGE_DIMS_EVENTS, event

   ; The purpose of this event handler is to make the proper dimension widgets
   ; sensitive or insensitive.
   Widget_Control, event.top, Get_UValue=info
   num = event.index + 1
   FOR j=0,num-1 DO Widget_Control, info.d[j], Sensitive=1
   FOR j=num, 7 DO Widget_Control, info.d[j], Sensitive=0, Set_Value=0

END ;-------------------------------------------------------------------------



PRO GETIMAGE_XDR_EVENTS, event

   ; The purpose of this event handler is to make the byte order widget
   ; sensitive or insensitive depending on file format.
   Widget_Control, event.top, Get_UValue=info

   IF event.index THEN Widget_Control, info.byteorderID, Sensitive=0 ELSE $
      Widget_Control, info.byteorderID, Sensitive=1

END ;-------------------------------------------------------------------------



FUNCTION GETIMAGE_FIND_COYOTE

   ; The purpose of this function is to find the "coyote"
   ; training directory and return its path. If no
   ; directory is found, the function returns a null string.
   ON_ERROR, 1

   ; Check this directory first.
   CD, Current=thisDir
   IF STRPOS(STRUPCASE(thisDir), 'COYOTE') GT 0 THEN RETURN, thisDir

   ; Look in !Path directories.
   pathDir = EXPAND_PATH(!Path, /Array)
   s = SIZE(pathDir)
   IF s[1] LT 1 THEN RETURN, ''
   FOR j=0,s[1]-1 DO BEGIN
      check = STRPOS(STRUPCASE(pathDir[j]), 'COYOTE')
      IF check GT 0 THEN RETURN, pathDir[j]
   ENDFOR
   RETURN, ''

END ;-------------------------------------------------------------------------



PRO GETIMAGE_EVENT, event

   ; The only events that can come here are button events.

   ; Get the info structure out of the user value of the top-level base.
   Widget_Control, event.top, Get_UValue=info

   ; There may be errors we can't anticipate. Catch them here, alert the
   ; user as to what the error was, and exit the event handler without
   ; doing any damage.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      ok = Error_Message()
      formdata = {cancel:1}
      *info.ptrToFormData =formdata
      RETURN
   ENDIF

   ; Which button caused this event?
   Widget_Control, event.id, Get_Value=buttonValue

   CASE buttonValue OF

      'Pick Filename': BEGIN

         ; Start in the directory listed in the directory text widget.
         ; Convert the text value to a scalar.
         Widget_Control, info.dirnameID, Get_Value=startDirectory
         startDirectory = startDirectory[0]

         ; If this directory doesn't exist, use the current directory.
         test = File_Search(startDirectory, Count=foundfile)
         IF foundfile NE 1 THEN CD, Current=startDirectory

         ; Use PICKFILE to pick a name.
         pick = Dialog_Pickfile(Path=startDirectory, /NoConfirm, $
            Get_Path=path, Filter='*.*')

         ; Set the directory text widget with the name of the directory.
         ; Make sure the user didn't cancel out of PICKFILE.
         IF pick NE '' THEN BEGIN

            ; Find the lengths of the PICK and the PATH.
            pathLen = StrLen(path)
            picklen = StrLen(pick)

           ; Shorten the PATH to take off last file separator.
            path = StrMid(path, 0, pathLen-1)

            ; Put the PATH in the directory location.
            Widget_Control, info.dirnameID, Set_Value=path

            ; Set the filename text widget with the name of the file.
            filename = StrMid(pick, pathlen, picklen-pathlen)
            Widget_Control, info.filenameID, Set_Value=filename

         ENDIF

         END ; of the Pick Filename button case

       'Cancel': BEGIN

         ; Have to exit here gracefully. Set the "CANCEL" flag.
         formdata = {cancel:1}
         *info.ptrToFormData =formdata

         ; Out of here!
         Widget_Control, event.top, /Destroy
         END ; of the Cancel button case

       'Accept': BEGIN  ; Gather the form information.

          ; Put the directory and filename together to make a path.
          Widget_Control, info.dirnameID, Get_Value=directory
          Widget_Control, info.filenameID, Get_Value=file

          filename = Filepath(Root_Dir=directory[0],file[0])

          ; Get the size and header info. Remember these are Pointers to Integers!
          Widget_Control, info.headerID, Get_Value=header
          header = Long(header[0])

          ; Get the number of dimensions.
          num = Widget_Info(info.dimsID, /Droplist_Select) + 1

          ; Get a vector of dimensions. If any of these numbers is 0, you
          ; have a problem.
          dimensions = IntArr(num)
          FOR j=0,num-1 DO BEGIN
            Widget_Control, info.d[j], Get_Value=theDimension
            IF theDimension EQ 0 THEN Message, 'Dimension ' + StrTrim(j+1,2) + ' cannot be zero.'
            dimensions[j] = theDimension
          ENDFOR

          ; Get the data type from the droplist widget.
          listIndex = Widget_Info(info.datatypesID, /Droplist_Select)
          type = info.datatypes(listIndex)

          ; What kind of endness do we have?
          endian = Widget_Info(info.byteorderID, /Droplist_Select)

          ; Get the format index from the formatlist widget.
          xdr = Widget_Info(info.formatlistID, /Droplist_Select)

          ; If we want XDR, then the endian order is always "native".
          IF xdr THEN endian = 0

          ; Create the formdata structure from the information you collected.
          formdata = {header:header, dimensions:dimensions, endian:endian, $
             filename:filename, type:type, xdr:xdr, cancel:0}

          ; Store the formdata in the pointer location.
          *info.ptrToFormData = formdata

         ; Out of here!
         Widget_Control, event.top, /Destroy
         END ; of the Accept button case

   ENDCASE

END ; of GETIMAGE_EVENT event handler ***************************************



FUNCTION GETIMAGE, filename, Directory=directory, DataType=datatype, Dimensions=dimensions, $
   Header=header, HeadData=headdata, Parent=parent, XDR=xdr, XOffSet=xoffset, $
   YOffSet=yoffset, Cancel=canceled, Group_Leader=group_leader, Endian=endian, $
   XSIZE=xsize, YSIZE=ysize, FRAMES=frames, Catch=catchit  ; Obsolete keywords on this line

   ; This is a function to specify the size, data type, and header information
   ; about an image that you would like to read. It reads the data and returns
   ; it as the result of the function. If an error occurs or the user CANCELS,
   ; the function returns a 0.
   On_Error, 2

   ; Catch errors unless explicitly told not to.
   IF N_Elements(catchit) EQ 0 THEN catchit = 1  ; Now obsolete. Doing nothing with value.

   ; Respond to either PARENT or GROUP_LEADER keywords.
   IF N_Elements(parent) NE 0 THEN group_leader = parent

   ; Check for parameters and keywords.
   IF N_Elements(filename) EQ 0 THEN BEGIN
        filename=Dialog_Pickfile()
        IF filename EQ "" THEN RETURN, ""
   ENDIF
   
   ; Does the file name have a directory? Is so, use it.
   fileDir = File_Dirname(filename)
   IF StrLen(fileDir) GT 1 THEN BEGIN
       filename = File_Basename(filename)
       directory = fileDir
   ENDIF ELSE BEGIN
       IF fileDir EQ "." THEN BEGIN
          CD, CURRENT=directory
          filename = File_Basename(filename)
       ENDIF
   ENDELSE

   ; If DIRECTORY keyword is not used, use the IDL example/data directory.
   IF N_Elements(directory) EQ 0 THEN BEGIN
       directory = File_DirName(Filepath(Subdirectory=['examples', 'data'], 'nonesense.dat'))
   ENDIF 
   startDirectory = directory
   
   ; Check for size and header keywords. These probably come in as
   ; numbers and you need strings to put them into text widgets.
   IF N_Elements(endian) EQ 0 THEN endian = 0 ELSE endian = 0 > endian < 2
   IF N_Elements(datatype) EQ 0 THEN datatype = 1
   IF N_Elements(dimensions) EQ 0 THEN dimensions = [256, 256]
   IF N_Elements(header) EQ 0 THEN header='0' ELSE header=StrTrim(header,2)

   ; Create a modal top-level base if group leader is present.
   IF N_Elements(group_leader) EQ 0 THEN $
      tlb = Widget_Base(Column=1, Title='Read Image Data', /Base_Align_Center) ELSE $
      tlb = Widget_Base(Column=1, Title='Read Image Data', Modal=1, $
         Group_Leader=group_leader, /Base_Align_Center)

   frameBase = Widget_Base(tlb, Frame=1, Column=1)

   ; Create the directory widgets.
   dirnamebase = Widget_Base(frameBase, Row=1)
      dirnamelabel = Widget_Label(dirnamebase, Value='Directory:')
      dirnameID = Widget_Text(dirnamebase, Value=startDirectory, /Editable, $
         Event_Pro='GETIMAGE_NULL_EVENTS', XSize=Fix(2.0*StrLen(startDirectory) > 50))

   ; Create the filename widgets.
   filenamebase = Widget_Base(frameBase, Row=1)
      filenamelabel = Widget_Label(filenamebase, Value='Filename:')
      filenameID = Widget_Text(filenamebase, Value=filename, /Editable, $
         Event_Pro='GETIMAGE_NULL_EVENTS', XSize=2*StrLen(filename) > 20)

   ; Create a button to allow user to pick a filename.
   pickbutton = Widget_Button(filenamebase, Value='Pick Filename')

   ; Create a droplist widget to select file data types.
   database = Widget_Base(frameBase, Row=1)
      datatypes = ['Byte', 'Integer (16-bit)', 'Long Integer (32-bit)', 'Float', 'Double', 'Unsigned Integer', 'Unsigned Long', '64-bit Long', '64-bit Unsigned Long']
      types = [1, 2, 3, 4, 5, 12, 13, 14, 15]
      theIndex = Where(types EQ datatype, count)
      datatypesID = Widget_Droplist(database, Value=datatypes, UValue=types, $
         Title='Data Type: ', Event_Pro='GETIMAGE_NULL_EVENTS')
      IF count EQ 0 THEN theIndex = 0
      Widget_Control, datatypesID, Set_Droplist_Select=theIndex

   ; Create a droplist widget to select file formats.
   formatlistID = Widget_Droplist(database, Value=['Unformatted', 'XDR format'], $
      Title='File Format: ', Event_Pro='GETIMAGE_XDR_EVENTS')


   row4 = Widget_Base(frameBase, Row=1)
   dimsID = Widget_Droplist(row4, Title='Image Dimensions: ', Value=StrTrim(Indgen(7)+1, 2), $
      Event_PRO = 'GETIMAGE_DIMS_EVENTS')
   Widget_Control, dimsID, Set_Droplist_Select=1

   byteorderID = Widget_Droplist(row4, Title='Image Byte Order: ', $
      Value=['Native', 'Big Endian', 'Little Endian'], Event_PRO = 'GETIMAGE_NULL_EVENTS')
   Widget_Control, byteorderID, Set_Droplist_Select=endian

   label = Widget_Label(row4, Value=' Header Size: ')
   combovalues = ['0','32','64','128', '256', '512','1024']
   IF header GT 0 THEN combovalues = [StrTrim(header,2), combovalues]
   headerID = Widget_Combobox(row4, Value=combovalues, $
      /Editable, Event_PRO = 'GETIMAGE_NULL_EVENTS')


   ; Create widgets to gather the required file sizes.

   sizebase = Widget_Base(frameBase, Row=2)

   d = IntArr(8)
   d[0] = FSC_Field(sizebase, Title='1st Dim:', Value=0, /Integer, XSize=6, /Positive)
   d[1] = FSC_Field(sizebase, Title='2nd Dim:', Value=0, /Integer, XSize=6, /Positive)
   d[2] = FSC_Field(sizebase, Title='3rd Dim:', Value=0, /Integer, XSize=6, /Positive)
   d[3] = FSC_Field(sizebase, Title='4th Dim:', Value=0, /Integer, XSize=6, /Positive)
   d[4] = FSC_Field(sizebase, Title='5th Dim:', Value=0, /Integer, XSize=6, /Positive)
   d[5] = FSC_Field(sizebase, Title='6th Dim:', Value=0, /Integer, XSize=6, /Positive)
   d[6] = FSC_Field(sizebase, Title='7th Dim:', Value=0, /Integer, XSize=6, /Positive)
   d[7] = FSC_Field(sizebase, Title='8th Dim:', Value=0, /Integer, XSize=6, /Positive)

   num = 1 > Size(dimensions, /N_Elements) < 8
   FOR j=0,num-1 DO Widget_Control, d[j], Set_Value=dimensions[j]
   FOR j=num, 7 DO Widget_Control, d[j], Sensitive=0

   ; Create cancel and accept buttons.
   cancelbase = Widget_Base(tlb, Row=1)
      cancel = Widget_Button(cancelbase, Value='Cancel')
      accept = Widget_Button(cancelbase, Value='Accept')

   ; Recalculate the length of the filenameID widget.
   g1 = Widget_Info(dirnamebase, /Geometry)
   g2 = Widget_Info(filenamelabel, /Geometry)
   g3 = Widget_Info(pickbutton, /Geometry)
   target = g1.scr_xsize - (g2.scr_xsize + g3.scr_xsize) - 10
   Widget_Control, filenameID, Scr_XSize=target

   ; Center the program on the display.
   cgCenterTLB, tlb

   ; Realize the program.
   Widget_Control, tlb, /Realize

   ; Create a pointer to store the information collected from the form.
   ; The initial data stored here is set to CANCEL, so nothing needs to
   ; be done if the user kills the widget with the mouse.
   ptrToFormData = Ptr_New({cancel:1})

   ; Set the correct file format in the format droplist widget.
   Widget_Control, formatlistID, Set_Droplist_Select=Keyword_Set(xdr)

   ; Set the text insertion point at the end of the filename text widget.
   tip = [StrLen(filename),0]
   Widget_Control, filenameID, Input_Focus=1
   Widget_Control, filenameID, Set_Text_Select=tip

   ; Create an info structure with program information.
   info = { filenameID:filenameID, $        ; The name of the file.
            d:d, $                          ; The dimension widgets IDs.
            dimsID:dimsID, $                ; The number of dimensions widget ID.
            byteorderID:byteorderID, $      ; The byte order widget ID.
            dirnameID:dirnameID, $          ; The ID of the widget with the directory name.
            headerID:headerID, $            ; The ID of the widget with the image HEADER size.
            datatypesID:datatypesID, $      ; The ID of the image data TYPE droplist ID.
            formatlistID:formatlistID, $    ; The ID of the image FORMAT droplist ID.
            datatypes:types, $              ; The possible data types.
            ptrToFormData:ptrToFormData}    ; A pointer to store the form information.

  ; Store the info structure in the user value of the top-level base.
   Widget_Control, tlb, Set_UValue=info

   ; The form will be a MODAL or BLOCKING widget, depending upon the
   ; presence of the PARENT. We block or are MODAL here until the widget
   ; is destroyed.
   XManager, 'getimage', tlb, Event_Handler='GETIMAGE_EVENT'

   ; When the widget is destroyed, the block is released, and we
   ; return here. Get the form data that was collected by the form
   ; and stored in the pointer location.
   formdata = *ptrToFormData

   ; If there is nothing here. Free the pointer and return.
   IF N_Elements(formdata) EQ 0 THEN BEGIN
      Ptr_Free, ptrToFormData
      canceled = 1
      RETURN, 0
   ENDIF

   ; Did the user cancel out of the form? If so, return a 0.
   IF formdata.cancel EQ 1 THEN BEGIN
      Ptr_Free, ptrToFormData
      canceled = 1
      RETURN, 0
   ENDIF

   ; Make the proper sized image array.
   image = Make_Array(Dimension=formdata.dimensions, Type=formdata.type)

   ; We can have all kinds of trouble reading data. Let's catch all
   ; input and output errors and alert user without crashing the program!
   Catch, theError
   IF theError NE 0 THEN BEGIN

      ; If we can't read the file for some reason, let the user know
      ; why, free the pointer and its information, check the logical
      ; unit number back in if it is checked out, and return a 0.
      ok = Error_Message()
      Ptr_Free, ptrToFormData
      canceled = 1
      IF N_Elements(lun) NE 0 THEN Free_Lun, lun
      RETURN, 0
   ENDIF

   ; Set the canceled flag.
   canceled = formdata.cancel

   ; Read the data file.
   IF formdata.header GT 0 THEN headdata = BytArr(formdata.header)
   Get_Lun, lun
   CASE formdata.endian OF
      0: OpenR, lun, formdata.filename, XDR=formdata.xdr
      1: OpenR, lun, formdata.filename, XDR=formdata.xdr, /Swap_If_Little_Endian
      2: OpenR, lun, formdata.filename, XDR=formdata.xdr, /Swap_If_Big_Endian
   ENDCASE
   IF formdata.header GT 0 THEN ReadU, lun, headdata
   ReadU, lun, image
   Free_Lun, lun

   ; Free the pointer.
   Ptr_Free, ptrToFormData

   RETURN, image

END ; of GETIMAGE program ***************************************************
