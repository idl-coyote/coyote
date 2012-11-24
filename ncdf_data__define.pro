;+
; NAME:
;       NCDF_DATA__DEFINE
;
; PURPOSE:
;
;       This program is designed to make it easier to browse and read the 
;       data and metadata in netCDF and HDF files. The user can browse files, 
;       and read the data and metadata into main-level IDL variables. New netCDF 
;       and HDF files can be opened at any time. The user interacts with the 
;       program via a browser window (GUI) or directly through the methods of
;       the object. The program implements an IDL object.
;       
;       Note that only HDF files with scientific datasets (SD) can be read currently.
;       There is no support for VDATA objects or other objects sometimes found in HDF
;       files. Also note that when variables are returned from HDF files, they are returned
;       in a calibrated form, if calibration information about the variable is present in the
;       file. Calibration information is presented as an extra variable attribute in the
;       browser.
;       
;          calibratedData = calData.cal * (uncalibratedData - calData.offset)
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
;       File I/O
;
; CALLING SEQUENCE:
;
;       IDL> nCDFObject = Obj_New('NCDF_DATA', filename)
;
; ARGUMENTS:
;
;       filename: The name of a netCDF or HDF file to open and browse.
;
; KEYWORD PARAMETERS:
;       
;       BROWSE:   If this keyword is set, the Browse Window is invoked as soon
;                 as the object is initiated.
;
;       DESTROY_FROM_BROWSER:  As with all objects, this object is persistent until
;                  it is destroyed. However, with this keyword set, the object will
;                  be destroyed when the user closes the Browse Window.
;
;       EXTENSION: In general, netCDF and HDF files use *.nc, *.ncf, *.ncdf of *.hdf file extensions to
;                  identify themselves as netCDF or HDF files. Some users have their own file extensions.
;                  You can use this keyword to identify the file extension you wish to use. If
;                  set here, it will be used as the file filter in place of the normal file 
;                  extensions in DIALOG_PICKFILE.
;
;                      obj = ('NCDF_DATA', file, EXTENSION='*.bin')
;                
;       NO_READ_ON_PARSE: Normally, when a file is opened it is parsed for information.
;                  One piece of information is the minimum and maximum values of the variables.
;                  This requires actually reading the variables. This can slow things down 
;                  considerably is the variable is large. Setting this keyword will suppress 
;                  the reading of the variables during the parsing of the data file, with the
;                  result that no minimum or maximum values will be reported.
;
; NOTES:
;       
;       This program is designed to be flexible in how it is used, so it
;       can be used in both interactive and non-interactive (called directly)
;       ways. A less flexible way of interacting with the program is via the
;       NCDF_BROWSER program, which is a front-end to this object.
;       
;       The netCDF and HDF file formats are thought to be "standards". And to 
;       a large extent, they are. But files are not always created to standards,
;       and both netCDF and HDF files can be quirky. If you look carefully at the 
;       code you will see places where I work around quirks in the files I typically
;       use on a daily basis. If you find you can't read a particular file, let me know
;       about it. I may be able to improve the program in such as way that it can be read.
;       
;       This program is not meant to be the be-all and end-all of programs. Rather, it is
;       a tool I use, and improve upon whenever necessary, in my own work with netCDF and HDF
;       files. It will get better for all of us if you report problems to me directly.
;
; REQUIRES:
;
;     The following programs are required from the Coyote Library. And it is always a
;     good idea to make sure you have the latest version of the Coyote Library code,
;     as updates are irregular and frequent.
;
;              http://www.idlcoyote.com/programs/netcdf_data__define.pro
;              http://www.idlcoyote.com/programs/error_message.pro
;;              http://www.idlcoyote.com/programs/undefine.pro
;              http://www.idlcoyote.com/programs/textbox.pro
;              http://www.idlcoyote.com/programs/cgrootname.pro
;              http://www.idlcoyote.com/programs/textlineformat.pro
;
; METHODS:
;
;     The following methods can be used directly.
;
;     ncdfObject -> Browse                             ; Use GUI to browse file data and metadata.
;     ncdfObject -> OpenFile, filename                 ; Opens a new netCDF or HDF file.
;     globalAttr = ncdfObject -> ReadGlobalAttr()      ; Return a structure containing global attributes.
;     attribute = ncdfObject -> ReadAttribute(attrname); Return an attribute, identified by name.
;     dim = ncdfObject -> ReadDimension(dimName)        ; Return a dimension, identified by name.
;     variable = ncdfObject -> ReadVariable(varname)   ; Return a variable, identified by name.
;     varstruct = ncdfObject -> ReadVariableWithAttr(varname)   ; Return a variable, identified by 
;                                                               ; name, along with its attributes.
;     allData = ncdfObject -> ReadFile(filename)        ; Read all the data in the file, into structures.
;
; EXAMPLE:
;
;       IDL> filename = 'example.nc'
;       IDL> ncdfObj = Obj_New('NCDF_DATA', filename)
;       IDL> ncdfObj -> Browse
;       IDL> Obj_Destroy, ncdfObj
;
; MODIFICATION HISTORY:
;       Written by:  David W. Fanning, 03 Feb 2008. Used ideas from many
;           people, including Chris Torrence, Ken Bowman, Liam Gumely, 
;           Andrew Slater, and Paul van Delst.
;       Added EXTENSION keyword, resizeable TLB, and ability to download
;           individual global attibutes. DWF. 04 Feb 2008.
;       Added ReadDimension and ReadVariableWithAttr methods. DWF. 05 Feb 2008.
;       Ill-formed attribute names giving me fits. Now doing checks with IDL_VALIDNAME
;            before creating structures. 06 February 2008. DWF.
;       Same problem. Wide use of IDL_VALIDNAME everywhere it seems wise. 06 Feb 2008. DWF.
;       Added functionality to read a variable with its attributes from the browser interface,
;            and fixed a problem with reading CHAR values. 2 March 2008. DWF.
;       Fixed a problem with changing variable name when reading variable plus attributes. 6 March 2008. DWF.
;       Fixed a problem with not setting GLOBAL keyword when inquiring about global attribute. 6 March 2008. DWF.
;       Made sure file was parsed before attempting to read variables and attributes to avoid errors. 7 March 2008. DWF.
;       Small bug with variable attributes fixed. 18 Dec 2008. DWF.
;       Added ability to read HDF files containing Scientific Datasets (SD). 21 February 2009. DWF.
;       Added error handling and protection for NCDF variables that have a dimension of length zero. 22 April 2009. DWF.
;       Added NO_READ_ON_PARSE keyword. 22 April 2009. DWF.
;       Now convert NCDF CHAR type variables to strings on output. 22 April 2009. DWF
;       Fixed a problem with the directory being correct when file name passed in. 11 May 2009. DWF.
;       Added COUNT, OFFSET, and STRIDE keywords to ReadVariable method. 25 June 2009. DWF.
;       When reading a netCDF variable by itself (without it's attributes), the program now looks for
;          a SCALE_FACTOR and ADD_OFFSET attribute, and if found will apply this to the variable before
;          it is returned to the user. 24 August 2009. DWF.
;       Added the methods GetAttrNames, GetVarNames, GetVarAttrNames, and ReadVarAttr to retrieve specfic
;          information from the data files. 16 November 2009. DWF.
;       Modified the ReadVariableWithAttr method to include the number of dimensions (in the NDIMS field,
;          and the dimensions (in the DIMS field) in the return structure. For HDF files, the DIMS field
;          is a vector of the dimensions of the variable. For netCDF files, the DIMS field is a vector
;          of dimension IDs for the dimensions of the variable. 27 Nov 2009. DWF.
;       Andy Meigs alerted me to a problem creating a structure when the ncdf variable name
;          is ill-formed according to IDL structure tag name rules. Fixed in the ReadFile method.
;          30 November 2009. DWF.
;       Added NO_NEW_FILE keyword to the BROWSE method. This keyword will suppress the OPEN FILE
;          button on the browse interface. 3 Feb 2010. DWF.
;       Made the default browser size a bit larger to accomodate longer variable names. 3 Feb 2010. DWF.
;       Add a check for HDF/netCDF file type in the INIT method to better accommodate reading data
;          from the file without first parsing the file. 16 March 2010. DWF.
;       Changed the ReadVariable for netCDF files to now check for missing data, using either the
;           depreciated missing_value attribute or the compliant _FillValue attribute. Missing data
;           is now identified via new output keywords MISSINGINDICES and FILLVALUE, and missing data
;           is not scaled or offset, if these operations are applied to the data prior to return. 
;           21 March 2010. DWF. Problem with these changes, fixed 23 March 2010. DWF.
;       Fixed a problem with memory leakage when the input file cannot be read. 1 May 2010. DWF.
;       Fixed a problem with memory leakage from created structures. 1 May 2010. DWF.
;       Have done some work on parsing HDF-EOS swath files, but currently unused in code. 15 May 2010. DWF.
;       Modified the ReadVariable method to check for 0 length dimensions when reading variables
;           from HDF files. 21 July 2010. DWF.
;       Modified the global attribute structure so that the "filename" field, which holds the
;           name of the netCDF of HDF file is now named "ncdf_filename" or "hdf_filename". This
;           will avoid conflicts with global attributes with "filename". 20 January 2011. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2008-2010, by Fanning Software Consulting, Inc.                           ;
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

PRO NCDF_DATA::Browse, $
    NO_NEW_FILE=no_new_file, $
    SUCCESS=success, $
    TITLE=title, $
    XOFFSET=xoffset, $
    YOFFSET=yoffset
;
; NAME:
;       NCDF_DATA::Browse
;
; PURPOSE:
;
;       This method is invoked to create a Browser Window the user can
;       interact with to explored the data and metadata in a netCDF or HDF file.
;
; CALLING SEQUENCE:
;
;       IDL> nCDFObject -> Browse
;
; ARGUMENTS:
;
;       None.
;
; KEYWORD PARAMETERS:
; 
;       NONEWFILE: If this keyword is set, the browser does not allow selecting
;                  a new netCDF file from the interface.
;                  
;       TITLE:     The text on the title bar. By default, 'File Browser'.
;       
;       SUCCESS:   An output keyword set to 1 if this method exits successfully.
;       
;       XOFFSET:   Normally, the Browser Window is centered, however is this
;                  keyword and the YOFFSET keywords are used, the Browser Window
;                  can be located with the upper-left corner at these locations in 
;                  device coordinates. The X offset of the Browser Window.
;
;       YOFFSET:    The Y offset of the Browser Window.
;

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      Widget_Control, self.tlb, /DESTROY
      success = 0
      RETURN
   ENDIF
   
   ; Assume the best.
   success = 1
   
   ; Check input parameters.
   IF N_Elements(xoffset) EQ 0 THEN xoffset = -1
   IF N_Elements(yoffset) EQ 0 THEN yoffset = -1
   newFileOK = ~Keyword_Set(no_new_file)
   IF N_Elements(title) EQ 0 THEN title = 'File Browser'

   ; Only one browser with this TLB on the display at a time.
   IF Widget_Info(self.tlb, /VALID_ID) THEN BEGIN
      Widget_Control, self.tlb, /SHOW
      success = 0
      RETURN
   ENDIF

   ; Make sure the file has been parsed.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile
   IF self.hasBeenParsed EQ 0 THEN BEGIN
        success = 0
        RETURN
   ENDIF
   
   ; Get some bitmaps for the widget_tree.
   bmfile = Filepath(SubDir=['resource','bitmaps'], 'volume.bmp')
   IF File_Test(bmfile) THEN BEGIN
      bm = Read_BMP(bmfile, r, g, b)
      i = Where(bm EQ 8, count)
      IF count GT 0 THEN bm[i] = 16B
      variableBM = [ [[r[bm]]], [[g[bm]]], [[b[bm]]] ]
   ENDIF

   bmfile = Filepath(SubDir=['resource','bitmaps'], 'axis.bmp')
   IF File_Test(bmfile) THEN BEGIN
      bm = Read_BMP(bmfile, r, g, b)
      i = Where(bm EQ 8, count)
      IF count GT 0 THEN bm[i] = 16B
      dimensionBM = [ [[r[bm]]], [[g[bm]]], [[b[bm]]] ]
   ENDIF
   
   bmfile = Filepath(SubDir=['resource','bitmaps'], 'ascii.bmp')
   IF File_Test(bmfile) THEN BEGIN
      bm = Read_BMP(bmfile, r, g, b)
      i = Where(bm EQ 80, count)
      IF count GT 0 THEN bm[i] = 95B
      attributeBM = [ [[r[bm]]], [[g[bm]]], [[b[bm]]] ]
   ENDIF
   
   bmfile = Filepath(SubDir=['resource','bitmaps'], 'sum.bmp')
   IF File_Test(bmfile) THEN BEGIN
      bm = Read_BMP(bmfile, r, g, b)
      i = Where(bm EQ 8, count)
      IF count GT 0 THEN bm[i] = 16B
      summaryBM = [ [[r[bm]]], [[g[bm]]], [[b[bm]]] ]
   ENDIF
   
   ; Set up the initial tree widget.
   self.tlb = Widget_Base(TITLE=title, COLUMN=1, UVALUE=self, /BASE_ALIGN_CENTER, $
      TLB_SIZE_EVENT=1)
   rowbase = Widget_Base(self.tlb, ROW=1, XPAD=0, YPAD=0)
   theTree = Widget_Tree(rowbase, SCR_XSIZE=350, SCR_YSIZE=400, UNAME='theTree')
   self.textDisplay = Widget_Text(rowbase, SCR_XSIZE=450, SCR_YSIZE=400, /SCROLL)

   ; Set up fundamental branch.
   aBranch = Widget_Tree(theTree, Value='File Overview', /FOLDER, /EXPANDED, UNAME='FOLDER')
   aNode = Widget_Tree(aBranch, Value='Directory', UValue=self.directory, UNAME='DIRECTORY', BITMAP=summaryBM)
   aNode = Widget_Tree(aBranch, Value='File Name', UValue=self.filename, UNAME='FILENAME', BITMAP=summaryBM)
   summaryNode = Widget_Tree(aBranch, Value='Summary', UNAME='SUMMARY', BITMAP=summaryBM)
   Widget_Control, summaryNode, Set_Tree_Select=1
   
   ; Set up global attribute branch.
   IF Ptr_Valid(self.theAttributes) THEN BEGIN
      aBranch = Widget_Tree(theTree, Value='Global Attributes', /FOLDER, UNAME='FOLDER')
      theAttributes = *self.theAttributes
      FOR j=0,N_Elements(theAttributes)-1 DO BEGIN
         aNode = Widget_Tree(aBranch, Value=theAttributes[j].name, UValue=theAttributes[j].name, $
            UNAME='GLOBAL ATTRIBUTE', BITMAP=attributeBM)
      ENDFOR
   ENDIF

   ; Set up dimension branch.
   IF Ptr_Valid(self.theDimensions) THEN BEGIN
      aBranch = Widget_Tree(theTree, Value='Dimensions', /FOLDER, UNAME='FOLDER')
      theDimensions = *self.theDimensions
      FOR j=0,N_Elements(theDimensions)-1 DO BEGIN
         aNode = Widget_Tree(aBranch, Value=theDimensions[j].name, UValue=theDimensions[j].name, $
            UNAME='DIMENSION', BITMAP=dimensionBM)
      ENDFOR
   ENDIF

   ; Set up variable branch.
   IF Ptr_Valid(self.theVariables) THEN BEGIN
      aBranch = Widget_Tree(theTree, Value='Variables', /FOLDER, UNAME='FOLDER')

      theVariables = *self.theVariables
      FOR j=0,N_Elements(theVariables)-1 DO BEGIN

         IF Ptr_Valid(theVariables[j].var_attributes) THEN BEGIN
            varBranch = Widget_Tree(aBranch, Value=theVariables[j].name, UValue=theVariables[j].name, $
               UNAME='VARIABLE', /FOLDER, BITMAP=variableBM)
            theAttributes = *theVariables[j].var_attributes
            FOR k=0,N_Elements(theAttributes)-1 DO BEGIN
               IF theAttributes[k].name NE "" THEN BEGIN
                  aNode = Widget_Tree(varBranch, Value=theAttributes[k].name, $
                     UValue=[theVariables[j].name,theAttributes[k].name], $
                     BITMAP=attributeBM, UNAME='VARIABLE ATTRIBUTE')
               ENDIF
            ENDFOR

         ENDIF ELSE BEGIN
            aNode = Widget_Tree(aBranch, Value=theVariables[j].name, UValue=theVariables[j].name, $
               UNAME='VARIABLE', BITMAP=variableBM)
         ENDELSE
      ENDFOR

   ENDIF
   
   ; Application Buttons
   buttonBase = Widget_Base(self.tlb, /ROW, BASE_ALIGN_CENTER=1)
   button = Widget_Button(buttonBase, Value='Read Variable', UVALUE='READ_VAR_FROM_GUI')
   button = Widget_Button(buttonBase, Value='Read Variable with Attributes', UVALUE='READ_VARPLUS_FROM_GUI')
   button = Widget_Button(buttonBase, Value='Read Global Attribute', UVALUE='READ_ATTRIBUTE_FROM_GUI')
   button = Widget_Button(buttonBase, Value='Read Entire File', UVALUE='READ_FILE_FROM_GUI')
   IF newFileOK THEN button = Widget_Button(buttonBase, Value='Open New File', UVALUE='OPEN_NEW_FILE')
   button = Widget_Button(buttonBase, Value='Exit', UVALUE='QUIT_BROWSER')
   
   ; Get the geometries of the tree widget and the button base. These
   ; will set the minimun and maximum values for resizing.
   self.geoWindow = Widget_Info(self.tlb, /GEOMETRY)
   self.geoTree = Widget_Info(theTree, /GEOMETRY)
   self.geoButton = Widget_Info(buttonBase, /GEOMETRY)
   self.geoDisplay = Widget_Info(self.textDisplay, /GEOMETRY)
   self.minxsize = self.geoDisplay.scr_xsize
   self.minysize = self.geoDisplay.scr_ysize
   
   ; Position the application and realize it.
   IF (xoffset LT 0 AND yoffset LT 0) THEN cgCenterTLB, self.tlb ELSE cgCenterTLB, self.tlb, xoffset, yoffset, /DEVICE, /NOCENTER
   Widget_Control, self.tlb, /REALIZE
   self.theTree = theTree
   XManager, 'ncdf_data', self.tlb, /NO_BLOCK, EVENT_HANDLER='NCDF_DATA_WIDGET_EVENTS', $
      CLEANUP='NCDF_DATA_WIDGET_CLEANUP'
  
   ; Send an event to start up in the summary browse look.
   event ={WIDGET_TREE_SEL}
   event.top = self.tlb
   event.id = summaryNode
   event.handler = self.tlb
   event.clicks = 1
   event.type = 0
   Widget_Control, summaryNode, Send_Event=event
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::CleanParsedStructures
   ; An internal method used to clean up pointers in names structures
   ; to prevent memory leakage from the object.
   
   ; Clean up all pointers in the global attribute structures.
   IF Ptr_Valid(self.theAttributes) THEN BEGIN
      num = N_Elements(*self.theAttributes)
      FOR j=0,num-1 DO Ptr_Free, (*self.theAttributes)[j].value
   ENDIF

   ; Clean up all pointers in the variable structures.
   IF Ptr_Valid(self.theVariables) THEN BEGIN
      num = N_Elements(*self.theVariables)
      FOR j=0,num-1 DO BEGIN
         Ptr_Free, (*self.theVariables)[j].value
         Ptr_Free, (*self.theVariables)[j].datasize
         Ptr_Free, (*self.theVariables)[j].calibration
         IF Ptr_Valid((*self.theVariables)[j].var_attributes) THEN BEGIN
            attrs = *(*self.theVariables)[j].var_attributes
            attnum = N_Elements( attrs )
            FOR k=0,attnum-1 DO Ptr_Free, attrs[k].value
            Ptr_Free, (*self.theVariables)[j].var_attributes
         ENDIF
      ENDFOR
   ENDIF

   ; Clean up all pointers in the swath structures.
   IF Ptr_Valid(self.theSwaths) THEN BEGIN
      num = N_Elements(*self.theSwaths)
      FOR j=0,num-1 DO BEGIN
         Ptr_Free, (*self.theSwaths)[j].maps
         Ptr_Free, (*self.theSwaths)[j].idxmaps
         Ptr_Free, (*self.theSwaths)[j].dimensions
         IF Ptr_Valid((*self.theSwaths)[j].attributes) THEN BEGIN
            attrs = *(*self.theSwaths)[j].attributes
            attnum = N_Elements( attrs )
            FOR k=0,attnum-1 DO Ptr_Free, attrs[k].value
            Ptr_Free, (*self.theSwaths)[j].attributes
         ENDIF
         IF Ptr_Valid((*self.theSwaths)[j].geofields) THEN BEGIN
              num = N_Elements(*(*self.theSwaths)[j].geofields)
              FOR j=0,num-1 DO BEGIN
                 thisGeoField = (*(*(*self.theSwaths)[j].geofields))[j]
                 Ptr_Free, thisGeoField.value
                 Ptr_Free, thisGeoField.datasize
                 Ptr_Free, thisGeoField.calibration
                 IF Ptr_Valid(*thisGeoField.var_attributes) THEN BEGIN
                    attrs = *(*thisGeoField.var_attributes)
                    attnum = N_Elements( attrs )
                    FOR k=0,attnum-1 DO Ptr_Free, attrs[k].value
                    Ptr_Free, (*thisGeoField)[j].var_attributes
                 ENDIF
                 Ptr_Free,(*self.theSwaths)[j].geofields
              ENDFOR
         ENDIF
         IF Ptr_Valid((*self.theSwaths)[j].datafields) THEN BEGIN
              num = N_Elements(*(*self.theSwaths)[j].datafields)
              FOR j=0,num-1 DO BEGIN
                 thisGeoField = (*(*(*self.theSwaths)[j].datafields))[j]
                 Ptr_Free, thisGeoField.value
                 Ptr_Free, thisGeoField.datasize
                 Ptr_Free, thisGeoField.calibration
                 IF Ptr_Valid(*thisGeoField.var_attributes) THEN BEGIN
                    attrs = *(*thisGeoField.var_attributes)
                    attnum = N_Elements( attrs )
                    FOR k=0,attnum-1 DO Ptr_Free, attrs[k].value
                    Ptr_Free, (*thisGeoField)[j].var_attributes
                 ENDIF
                 Ptr_Free,(*self.theSwaths)[j].datafields
              ENDFOR
         ENDIF
      ENDFOR
   ENDIF

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::EventHandler, event
   ; An internal method used to process events and sent them to appropriate event handler methods.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Event branching is based initially on event structure names.
   thisEvent = Tag_Names(event, /STRUCTURE_NAME)
   CASE thisEvent OF
   
     'WIDGET_BASE': BEGIN
         Widget_Control, self.textDisplay, $
            SCR_XSIZE = (event.x -(self.geoTree.scr_xsize + 8)) > self.minXSize , $
            SCR_YSIZE = (event.y -(self.geoButton.scr_ysize + 8)) > self.minYSize
            
         END
   
     'WIDGET_TREE_SEL': self -> SelectionInTree, event
         
     'WIDGET_TREE_EXPAND': 
     
     'WIDGET_BUTTON': BEGIN
         Widget_Control, event.id, GET_UVALUE=buttonUValue
         CASE buttonUValue OF
         
            'READ_ATTRIBUTE_FROM_GUI': self -> ReadAttributeFromGUI, event
            'READ_ATTRIBUTE_AND_LEAVE': self -> ReadAttributeFromGUI_Events, event
            'READ_ATTRIBUTE_AND_STAY': self -> ReadAttributeFromGUI_Events, event
            'QUIT_READ_ATTRIBUTE_GUI': self -> ReadAttributeFromGUI_Events, event

            'READ_VAR_FROM_GUI': self -> ReadVariableFromGUI, event
            'READ_AND_LEAVE': self -> ReadVariableFromGUI_Events, event
            'READ_AND_STAY': self -> ReadVariableFromGUI_Events, event
            'QUIT_READ_VARIABLE_GUI': self -> ReadVariableFromGUI_Events, event

            'READ_VARPLUS_FROM_GUI': self -> ReadVarPlusFromGUI, event
            'READ_VARPLUS_AND_STAY': self -> ReadVarPlusFromGUI_Events, event
            'READ_VARPLUS_AND_LEAVE': self -> ReadVarPlusFromGUI_Events, event
            'QUIT_READ_VARPLUS_GUI': self -> ReadVarPlusFromGUI_Events, event
            
            'READ_FILE_FROM_GUI': self -> ReadFileFromGUI, event
            'OPEN_NEW_FILE': self -> OpenNewFile, event
            'QUIT_BROWSER': Widget_Control, event.top, /DESTROY
            'APPEND_FILENAME': 
            
            ELSE: Print, 'No case for ' + buttonUValue
         ENDCASE
         END
         
      'WIDGET_DROPLIST': BEGIN
         theName = Widget_Info(event.id, /UNAME)
         CASE theName OF
            'VARIABLES': self -> ReadVariableFromGUI_Events, event
            'VARIABLESPLUS': self -> ReadVarPlusFromGUI_Events, event     
            'ATTRIBUTES': self -> ReadAttributeFromGUI_Events, event
         ENDCASE
         END
         
      'WIDGET_TEXT_CH':
      
      ELSE: BEGIN
               ok = Dialog_Message('Unrecognized event in EventHandler method. See Console for details.')
               HELP, event, /Structure
            END
      
   ENDCASE 
END ;---------------------------------------------------------------------------------------------


FUNCTION NCDF_DATA::FindDimensions, fileID, varID

    ; Get information about the variable.
    r = NCDF_VarInq(fileID, varID)
    dims = LonArr(N_Elements(r.dim))
    FOR j=0,N_Elements(dims)-1 DO BEGIN
        dimID = r.dim[j]
        NCDF_DIMINQ, fileID, dimID, dimension_name, dimension_size
        dims[j] = dimension_size
    ENDFOR
    
    ; If this is a character variable, then the first dimension is irrelevant
    ; if there is more than one variable.
    IF r.datatype EQ 'CHAR' THEN IF N_Elements(dims) GT 1 THEN dims = dims[1:*] 

    RETURN, dims

END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::Destroy_From_Browser
   RETURN, self.destroy_from_browser
END ;---------------------------------------------------------------------------------------------


FUNCTION NCDF_DATA::GetAttrNames

   ; Returns a list of the global attribute names.
    
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN, ""
   ENDIF
    
   ; The file has to be parsed to carry this out.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile
   
   numAttrs = N_Elements(*self.theAttributes)
   attributeList = StrArr(numAttrs)
   
   FOR j=0,numAttrs-1 DO BEGIN
        thisAttribute = (*self.theAttributes)[j]
        attributeList[j] = thisAttribute.name
   ENDFOR
   
   RETURN, attributeList
   
END ;---------------------------------------------------------------------------------------------


PRO NCDF_DATA::ListAttrNames

   ; Prints out the global attribute names found in the file.
    
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
    
   ; The file has to be parsed to carry this out.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile
   
   numAttrs = N_Elements(*self.theAttributes)
   
   Print, 'There are ', StrTrim(numAttrs,2), ' attributes in ' + self.filename
   Print, ""
   FOR j=0,numAttrs-1 DO BEGIN
        thisAttribute = (*self.theAttributes)[j]
        Print, '   ', StrTrim(j+1,2), ') ', thisAttribute.name
   ENDFOR
   Print, ""
   
END ;---------------------------------------------------------------------------------------------


FUNCTION NCDF_DATA::GetVarNames

   ; This function returns a list of variable names found in the file.
    
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN, ""
   ENDIF
    
   ; The file has to be parsed to carry this out.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile
   
   numVars = N_Elements(*self.theVariables)
   varList = StrArr(numVars)
   
   FOR j=0,numVars-1 DO BEGIN
        thisVariable = (*self.theVariables)[j]
        varList[j] = thisVariable.name
   ENDFOR
   
   RETURN, varList
   
END ;---------------------------------------------------------------------------------------------


PRO NCDF_DATA::ListVarNames

   ; Prints out the variable names found in the file.
    
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
    
   ; The file has to be parsed to carry this out.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile
   
   numVars = N_Elements(*self.theVariables)
   
   Print, 'There are ', StrTrim(numVars,2), ' variables in ' + self.filename
   Print, ""
   FOR j=0,numVars-1 DO BEGIN
        thisVariable = (*self.theVariables)[j]
        Print, '   ', StrTrim(j+1,2), ') ', thisVariable.name
   ENDFOR
   Print, ""
   
END ;---------------------------------------------------------------------------------------------


FUNCTION NCDF_DATA::GetVarAttrNames, theVariable

   ; This function returns a list of attribute names which it finds for a particular variable.
    
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN, ""
   ENDIF
    
   ; The file has to be parsed to carry this out.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile
   
   ; Get the variable list.
   theVarStructures = *self.theVariables
   
   ; Find this variable in the variable structures.
   index = Where(theVarStructures.name EQ theVariable, count)
   IF count EQ 0 THEN Message, 'Cannot find the variable ' + theVariable + ' in the file.'
   thisVariableStruct = theVarStructures[index]
   
   ; Get the pointer to the variable attribute structures.
   varAttrStructures = *thisVariableStruct.var_attributes
   
   ; Extract the attribute names.
   numAttrs = N_Elements(varAttrStructures)
   varAttrList = StrArr(numAttrs)
   FOR j=0,numAttrs-1 DO BEGIN
        varAttrList[j] = varAttrStructures[j].name
   ENDFOR
   
   RETURN, varAttrList
   
END ;---------------------------------------------------------------------------------------------
PRO NCDF_DATA::OpenNewFile, event

   ; Creates a dialog for the user to specify the name of a new netCDF or HDF file to open.
   ; Loads the file into the object.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Get the name of a new file. Look in the directory of the last file.
   filename = Dialog_Pickfile(FILTER=self.extension , PATH=self.directory, $
      /READ, TITLE='Select a File to Open')
   IF filename EQ "" THEN RETURN
   
   ; Open the new file.
   self -> OpenFile, filename
   
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::OpenFile, filename
;
; NAME:
;       NCDF_DATA::OpenFile
;
; PURPOSE:
;
;       This method is used to open a new netCDF or HDF file and add it to the object.
;
; CALLING SEQUENCE:
;
;       IDL> nCDFObject -> OpenFile, filename
;
; ARGUMENTS:
;
;       filename:  The name of a netCDF or HDF file to open.
;
; KEYWORD PARAMETERS:
;       
;       None.
;

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF

   IF N_Elements(filename) NE 0 THEN BEGIN
   
      ; Clean up from the old file that will here before.
      self -> CleanParsedStructures
      
      ; Set the filename and directory name.
      directory = File_Dirname(filename)
      IF directory EQ "" THEN CD, CURRENT=directory
      self.directory = directory
      self.filename = File_Basename(filename)
      
      ; Parse the new file.
      self.hasbeenParsed = 0
      self -> ParseFile
      
      ; If a browser is currently displayed, kill it and display
      ; a browser in the same location with new file information.
      IF Widget_Info(self.tlb, /Valid_ID) THEN BEGIN
         thisState = self.destroy_from_browser
         IF thisState THEN self.destroy_from_browser = 0
         Widget_Control, self.tlb, TLB_GET_OFFSET=offsets
         Widget_Control, self.tlb, /DESTROY
         self -> Browse, XOFFSET=offsets[0], YOFFSET=offsets[1]
         self.destroy_from_browser = thisState
      ENDIF
      
   ENDIF ELSE Message, 'Must pass name of file to open.'  
   
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ParseFile
   ; This internal method parses the new netCDF or HDF file initially, and creates 
   ; the IDL structures necessary for browsing the object.
   
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
       void = Error_Message()
      self.hasBeenParsed = 0
      IF N_Elements(fileID) NE 0 THEN NCDF_Close, fileID
      RETURN
   ENDIF

   ; Check to see if file is available.
   IF self.filename EQ "" THEN BEGIN
      ok = Dialog_Message('Add a file to the NCDF_DATA object before proceeding.')
      RETURN
   ENDIF
   
   ; Be sure everything is cleaned up and ready to go.
   self -> CleanParsedStructures
   Ptr_Free, self.theAttributes
   Ptr_Free, self.theDimensions
   Ptr_Free, self.theCalibration
   Ptr_Free, self.theSwaths
   Ptr_Free, self.theVariables
   Ptr_Free, self.zeroDimensionID
   
   ; Special processing if this is an HDF file. Otherwise, we believe it is a netCDF file.
   self.isHDF = HDF_ISHDF(Filepath(ROOT_DIR=self.directory, self.filename))
   IF self.isHDF THEN BEGIN
   
        ; Is this an HDF-EOS type file? If so, we want to parse it differently
        isEOS_File = EOS_Query(Filepath(ROOT_DIR=self.directory, self.filename), info)
        IF isEOS_File THEN BEGIN
            IF info.num_grids NE 0 OR info.num_points NE 0 THEN BEGIN
                Message, 'This program does not current parse HDF-EOS grids or point data.', /INFORMATIONAL
            ENDIF
            
            ; This code is not quite ready. Bypassing this for the moment.
            self -> Parse_HDF_File
        ENDIF ELSE self -> Parse_HDF_File
        RETURN
   ENDIF

   ; Open the file and find out how many dimensions, global attributes, and variables are there.
   fileID = NCDF_Open(Filepath(ROOT_DIR=self.directory, self.filename))
   info = NCDF_Inquire(fileID)
   
   ; Is there an unlimited dimension in this file?
   IF info.recdim NE -1 THEN BEGIN
        NCDF_DIMINQ, fileID, info.recdim, unlimitedName, unlimitedSize
        unlimitedID = info.recdim
   ENDIF ELSE unlimitedName = ""
   
   ; First, get the global attributes.
   num_attr = info.ngatts
   IF num_attr GT 0 THEN BEGIN
      theAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, num_attr)
      FOR j=0,num_attr-1 DO BEGIN
          attribute_name = NCDF_AttName(fileID, j, /GLOBAL)
          NCDF_AttGet, fileID, attribute_name, theAttribute, /GLOBAL
          attinfo = NCDF_ATTINQ(fileID, attribute_name, /GLOBAL)
          att_type = StrUpCase(attinfo.dataType)
          
          ; Strings are stored as byte values in attributes, so convert them back.
          IF (Size(theAttribute, /TNAME) EQ 'BYTE') AND (att_type EQ 'CHAR') $
            THEN theAttribute = String(theAttribute)
          theAttributes[j].attrType = 'GLOBAL'
          theAttributes[j].dataType = att_type
          theAttributes[j].length = N_Elements(theAttribute)
          theAttributes[j].name = attribute_name
          theAttributes[j].value = Ptr_New(theAttribute)

      ENDFOR
      self.theAttributes = Ptr_New(theAttributes, /No_Copy)
   ENDIF

   ; Next, get the dimensions.
   num_dims = info.ndims
   IF num_dims GT 0 THEN BEGIN
      theDimensions = REPLICATE({NCDF_DATA_DIMENSION}, num_dims)
      FOR j=0,num_dims-1 DO BEGIN
          NCDF_DIMINQ, fileID, j, dimension_name, dimension_size
          IF dimension_size EQ 0 THEN BEGIN
            IF Ptr_Valid(self.zeroDimensionID) $
                THEN *self.zeroDimensionID = [*self.zeroDimensionID, j] $
                ELSE  self.zeroDimensionID = Ptr_New(j)
          ENDIF
          theDimensions[j].name = dimension_name
          theDimensions[j].value = String(dimension_size)
      ENDFOR
      self.theDimensions = Ptr_New(theDimensions, /No_Copy)
   ENDIF

   ; Next, get the variables.
   num_vars = info.nvars
   IF num_vars GT 0 THEN BEGIN
      theVariables = REPLICATE({NCDF_DATA_VARIABLE}, num_vars)
      FOR j=0,num_vars-1 DO BEGIN

         ; Get information about the variable.
         varinfo = NCDF_VarInq(fileID, j)
         theVariables[j].datatype = varinfo.datatype
         theVariables[j].name = varinfo.name

          ; If this variable has attributes, get those, too.
          IF varinfo.natts GT 0 THEN BEGIN
               varAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, varinfo.natts)
               FOR k=0,varinfo.natts-1 DO BEGIN
                   attribute_name = NCDF_AttName(fileID, j, k)
                   NCDF_AttGet, fileID, j, attribute_name, theAttribute
                   attinfo = NCDF_ATTINQ(fileID, j, attribute_name)
                   att_type = StrUpCase(attinfo.dataType)
          
                   ; Strings are stored as byte values in attributes, so convert them back.
                   IF (Size(theAttribute, /TNAME) EQ 'BYTE') AND (att_type EQ 'CHAR') $
                     THEN theAttribute = String(theAttribute)
                   varAttributes[k].attrType = StrUpCase(varinfo.name)
                   varAttributes[k].dataType = att_type
                   varAttributes[k].length = N_Elements(theAttribute)
                   varAttributes[k].name = attribute_name
                   varAttributes[k].value = Ptr_New(theAttribute)
               ENDFOR
               theVariables[j].var_attributes = Ptr_New(varAttributes)
          ENDIF

          ; Get information about the variable, including the dimension IDs.
          r = NCDF_VarInq(fileID, j)
          
          ; Now, read the data so you can collect information about it. It is possible
          ; that one of the dimensions has a zero length. If true, reading the data will
          ; cause an error. We want to prevent that, so if we have a zero dimension we
          ; are going to make sure we don't read that variable.
          ; 
          IF Ptr_Valid(self.zeroDimensionID) EQ 0 THEN BEGIN
              IF self.no_read_on_parse THEN BEGIN
                  theVariables[j].dataSize = Ptr_New(self -> FindDimensions(fileID, j))
                  theVariables[j].minValue = 0
                  theVariables[j].maxValue = 0                
              ENDIF ELSE BEGIN
                  NCDF_VarGet, fileID, j, data
                  theVariables[j].dataSize = Ptr_New(self -> FindDimensions(fileID, j))
                  IF r.datatype NE 'CHAR' THEN BEGIN
                      minData = Min(data, MAX=maxData)
                      theVariables[j].minValue = minData
                      theVariables[j].maxValue = maxData
                  ENDIF
                  Undefine, data       
              ENDELSE
          ENDIF ELSE BEGIN
          
                ; Is there a match between the dimension IDs and any zero dimension ID we 
                ; have stored?
                match = 0
                FOR m = 0, N_Elements(*self.zeroDimensionID)-1 DO BEGIN
                    i = Where(r.dim EQ (*self.zeroDimensionID)[m], count)
                    IF count GT 0 THEN match = 1
                ENDFOR
                IF match GT 0 THEN BEGIN
                          theVariables[j].dataSize = Ptr_New(self -> FindDimensions(fileID, j))
                          theVariables[j].minValue = 0
                          theVariables[j].maxValue = 0                
                ENDIF ELSE BEGIN
                          IF self.no_read_on_parse THEN BEGIN
                              theVariables[j].dataSize = Ptr_New(self -> FindDimensions(fileID, j))
                              theVariables[j].minValue = 0
                              theVariables[j].maxValue = 0                
                          ENDIF ELSE BEGIN
                              NCDF_VarGet, fileID, j, data
                              theVariables[j].dataSize = Ptr_New(self -> FindDimensions(fileID, j))
                              IF r.datatype NE 'CHAR' THEN BEGIN
                                  minData = Min(data, MAX=maxData)
                                  theVariables[j].minValue = minData
                                  theVariables[j].maxValue = maxData
                              ENDIF
                              Undefine, data  
                          ENDELSE     
                ENDELSE
          ENDELSE
      ENDFOR
      self.theVariables = Ptr_New(theVariables, /No_Copy)
   ENDIF

   ; Successfully parsed file.
   self.hasBeenParsed = 1
   
   ; Close the file
   NCDF_Close, fileID

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::Parse_HDF_File
   ; This internal method parses the new HDF file initially, and creates 
   ; the IDL structures necessary for browsing the object.
   
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      self.hasBeenParsed = 0
      IF N_Elements(fileID) NE 0 THEN HDF_SD_End, fileID
      RETURN
   ENDIF

   ; Check to see if file is available.
   IF self.filename EQ "" THEN BEGIN
      ok = Dialog_Message('Add a file to the NCDF_DATA object before proceeding.')
      RETURN
   ENDIF
   
   ; Be sure everything is cleaned up and ready to go.
   self -> CleanParsedStructures
   Ptr_Free, self.theAttributes
   Ptr_Free, self.theDimensions
   Ptr_Free, self.theCalibration
   Ptr_Free, self.theSwaths
   Ptr_Free, self.theVariables
   Ptr_Free, self.zeroDimensionID

   ; Open the file and find out how many dimensions, global attributes, and variables are there.
   fileID = HDF_SD_START(Filepath(ROOT_DIR=self.directory, self.filename), /READ)
   HDF_SD_Fileinfo, fileID, num_vars, num_attr
   
   ; First, get the global attributes.
   IF num_attr GT 0 THEN BEGIN
      theAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, num_attr)
      FOR j=0,num_attr-1 DO BEGIN
          HDF_SD_ATTRINFO, fileID, j, DATA=theAttribute, HDF_TYPE=hdf_type, NAME=attribute_name, TYPE=att_type

          theAttributes[j].attrType = 'GLOBAL'
          theAttributes[j].dataType = att_type
          theAttributes[j].length = N_Elements(theAttribute)
          theAttributes[j].name = attribute_name
          IF N_Elements(theAttribute) EQ 1 THEN theAttribute = theAttribute[0]
          theAttributes[j].value = Ptr_New(theAttribute)
       ENDFOR
      self.theAttributes = Ptr_New(theAttributes, /No_Copy)
   ENDIF

   ; Next, get the variables.
   IF num_vars GT 0 THEN BEGIN
      theVariables = REPLICATE({NCDF_DATA_VARIABLE}, num_vars)
      FOR j=0,num_vars-1 DO BEGIN

         ; Get information about the variable.
         sdID = HDF_SD_Select(fileID, j)
         
         ; This routine throws all kinds of scary messages if CALDATA, for example, is
         ; not in the file. Turn this off for this call.
         !QUIET = 1
         HDF_SD_GetInfo, sdID, DIMS=dims, NAME=name, NATTS=natts, NDIMS=ndims, $
            RANGE=range, TYPE=datatype, CALDATA=calData
         !QUIET = 0
         
         theVariables[j].datatype = datatype
         theVariables[j].name = name
         theVariables[j].calibration = Ptr_New(calData)

          ; If this variable has attributes, get those, too.
          IF natts GT 0 THEN BEGIN
               varAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, natts+1)
               FOR k=0,natts-1 DO BEGIN
                   HDF_SD_ATTRINFO, sdID, k, DATA=theAttribute, NAME=attribute_name, TYPE=attribute_datatype
                             
                   varAttributes[k].attrType = StrUpCase(name)
                   varAttributes[k].dataType = attribute_datatype
                   varAttributes[k].length = N_Elements(theAttribute)
                   varAttributes[k].name = attribute_name
                   IF N_Elements(theAttribute) EQ 1 THEN theAttribute = theAttribute[0]
                   varAttributes[k].value = Ptr_New(theAttribute)
               ENDFOR
               
               ; Add the calibration data as an attibute.
               IF calData.cal EQ 0 THEN BEGIN
                    varAttributes[natts].attrType = StrUpCase(name)
                    varAttributes[natts].dataType = 'STRING'
                    varAttributes[natts].length = 0
                    varAttributes[natts].name = '_calibration_data'
                    varAttributes[natts].value = Ptr_New('Not Present in File')               
               ENDIF ELSE BEGIN
                    varAttributes[natts].attrType = StrUpCase(name)
                    varAttributes[natts].dataType = 'STRUCT'
                    varAttributes[natts].length = N_Tags(calData, /Length)
                    varAttributes[natts].name = '_calibration_data'
                    varAttributes[natts].value = Ptr_New(calData)
               ENDELSE
               
               theVariables[j].var_attributes = Ptr_New(varAttributes)
          ENDIF

          ; Now, read the data so you can collect information about it.
          theVariables[j].dataSize = Ptr_New(dims)
          IF N_Elements(range) NE 0 THEN BEGIN
              theVariables[j].minValue = range[0]
              theVariables[j].maxValue = range[1]
          ENDIF ELSE BEGIN
              IF self.no_read_on_parse THEN BEGIN
                  theVariables[j].minValue = !VALUES.F_NAN
                  theVariables[j].maxValue = !VALUES.F_NAN             
              ENDIF ELSE BEGIN
                  HDF_SD_GetData, sdID, data
                  IF calData.cal NE 0 THEN BEGIN
                        data = calData.cal * (Temporary(data) - calData.offset)
                  ENDIF
                  minData = Min(data, MAX=maxData)
                  theVariables[j].minValue = minData
                  theVariables[j].maxValue = maxData
                  Undefine, data
              ENDELSE
          ENDELSE
          HDF_SD_EndAccess, sdID
      ENDFOR
      self.theVariables = Ptr_New(theVariables, /No_Copy)
   ENDIF

   ; Successfully parsed file.
   self.hasBeenParsed = 1
   
   ; Close the file
   HDF_SD_End, fileID

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::Parse_HDF_EOS_File

   ; This internal method parses the new HDF-EOS file initially, and creates 
   ; the IDL structures necessary for browsing the object.
   
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      self.hasBeenParsed = 0
      IF N_Elements(fileID) NE 0 THEN BEGIN
        IF N_Elements(swathID) NE 0 THEN ok = EOS_SW_DETACH(swathID)
        ok = EOS_SW_CLOSE(fileID)
      ENDIF
      RETURN
   ENDIF

   ; Check to see if file is available.
   IF self.filename EQ "" THEN BEGIN
      ok = Dialog_Message('Add a file to the NCDF_DATA object before proceeding.')
      RETURN
   ENDIF
   
   ; Be sure everything is cleaned up and ready to go.
   self -> CleanParsedStructures
   Ptr_Free, self.theAttributes
   Ptr_Free, self.theDimensions
   Ptr_Free, self.theSwaths
   Ptr_Free, self.theCalibration
   Ptr_Free, self.theVariables
   Ptr_Free, self.zeroDimensionID

   ; Open the file and find out how many swaths, grids, and points there are.
   filename = Filepath(ROOT_DIR=self.directory, self.filename)
   ok = EOS_Query(filename, info)
   
   ; Process the swaths first.
   IF info.num_swaths GT 0 THEN BEGIN
       theSwaths = Replicate({NCDF_DATA_SWATH}, info.num_swaths)
       FOR j=0, info.num_swaths-1 DO BEGIN
            swathNames = StrSplit(info.swath_names, ',', /EXTRACT)
            fileID = EOS_SW_OPEN(filename, /READ)
            swathID = EOS_SW_Attach(fileID, swathNames[j])
            Print, 'Swath Name: ', swathNames[j]
            theSwaths[j].name = swathNames[j]
            nattr = EOS_SW_INQATTRS(swathID, attrlist)
            IF nattr GT 0 THEN BEGIN
                theAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, nattr)
                attrNames = StrSplit(attrlist, ',', /EXTRACT)
                FOR k=0,nattr-1 DO BEGIN
                    ok = EOS_SW_READATTR(swathID, attrNames[k], attrValue)
                    theAttributes[k].name = attrNames[k]
                    theAttributes[k].attrType = 'SWATH ATTRIBUTE'
                    theAttributes[k].datatype = Size(attrValue, /TNAME)
                    theAttributes[k].length = N_Elements(attrValue)
                    IF N_Elements(attrValue) EQ 1 THEN attrValue = attrValue[0]
                    theAttributes[k].value = Ptr_New(attrValue)
                    Help, attrValue
                ENDFOR
                theSwaths[j].attributes = Ptr_New(theAttributes)
                theSwaths[j].nattrs = nattr
            ENDIF
            ndims = EOS_SW_INQDIMS(swathID, dimslist, dimSize)
            IF ndims GT 0 THEN BEGIN
               dimNames = StrSplit(dimslist, ',', /EXTRACT)
               theDimensions = Replicate({NCDF_DATA_DIMENSION}, ndims)
                FOR k=0,ndims-1 DO BEGIN
                    theDimensions[k].name = dimNames[k]
                    theDimensions[k].value = dimSize[k]
                ENDFOR
                theSwaths[j].dimensions = Ptr_New(theDimensions)
                theSwaths[j].ndims = ndims
            ENDIF
            ngeofields = EOS_SW_INQGEOFIELDS(swathID, geofieldslist, rank, numbertype)
            IF ngeofields GT 0 THEN BEGIN
               geoFieldNames = StrSplit(geofieldslist, ',', /EXTRACT)
               theGeoFields = Replicate({NCDF_DATA_VARIABLE}, ngeofields)
                FOR k=0,ngeofields-1 DO BEGIN
    
                    ; Get information about the variable.
                    sdFileID = HDF_SD_START(filename)
                    sdIndex = HDF_SD_NameToIndex(sdFileID, geoFieldNames[k])
                    sdID = HDF_SD_Select(sdFileID, sdIndex)
                 
                    ; This routine throws all kinds of scary messages if CALDATA, for example, is
                    ; not in the file. Turn this off for this call.
                    !QUIET = 1
                    HDF_SD_GetInfo, sdID, DIMS=dims, NAME=name, NATTS=natts, NDIMS=ndims, $
                        RANGE=range, TYPE=datatype, CALDATA=calData
                    !QUIET = 0
                 
                    theGeoFields[k].name = geoFieldNames[k]
                    theGeoFields[k].datatype = datatype
                    theGeoFields[k].calibration = Ptr_New(calData)
                    theGeoFields[k].datasize = Ptr_New(dims)
                    IF N_Elements(range) NE 0 THEN BEGIN
                        theGeoFields[k].minValue = range[0]
                        theGeoFields[k].maxValue = range[1]
                        Undefine, range ; Do this so it is not hanging around for the next variable.
                    ENDIF ELSE BEGIN
                        IF self.no_read_on_parse THEN BEGIN
                            theGeoFields[k].minValue = !VALUES.F_NAN
                            theGeoFields[k].maxValue = !VALUES.F_NAN             
                        ENDIF ELSE BEGIN
                            HDF_SD_GetData, sdID, data
                            IF calData.cal NE 0 THEN BEGIN
                                data = calData.cal * (Temporary(data) - calData.offset)
                            ENDIF
                            minData = Min(data, MAX=maxData)
                            theGeoFields[k].minValue = minData
                            theGeoFields[k].maxValue = maxData
                            Undefine, data
                         ENDELSE
                    ENDELSE
                  
                    ; If this variable has attributes, get those, too.
                    ; If this variable has attributes, get those, too.
                    IF natts GT 0 THEN BEGIN
                         varAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, natts+1)
                         FOR m=0,natts-1 DO BEGIN
                             HDF_SD_ATTRINFO, sdID, m, DATA=theAttribute, NAME=attribute_name, TYPE=attribute_datatype
                                     
                             varAttributes[m].attrType = StrUpCase(name)
                             varAttributes[m].dataType = attribute_datatype
                             varAttributes[m].length = N_Elements(theAttribute)
                             varAttributes[m].name = attribute_name
                             IF N_Elements(theAttribute) EQ 1 THEN theAttribute = theAttribute[0]
                             varAttributes[m].value = Ptr_New(theAttribute)
                         ENDFOR
                       
                         ; Add the calibration data as an attibute.
                         IF calData.cal EQ 0 THEN BEGIN
                            varAttributes[natts].attrType = StrUpCase(name)
                            varAttributes[natts].dataType = 'STRING'
                            varAttributes[natts].length = 0
                            varAttributes[natts].name = '_calibration_data'
                            varAttributes[natts].value = Ptr_New('Not Present in File')               
                         ENDIF ELSE BEGIN
                            varAttributes[natts].attrType = StrUpCase(name)
                            varAttributes[natts].dataType = 'STRUCT'
                            varAttributes[natts].length = N_Tags(calData, /Length)
                            varAttributes[natts].name = '_calibration_data'
                            varAttributes[natts].value = Ptr_New(calData)
                         ENDELSE
                       
                         theGeoFields[k].var_attributes = Ptr_New(varAttributes)
                     ENDIF
                     theSwaths[j].ngeoFields = ngeoFields
                     HDF_SD_EndAccess, sdID
                ENDFOR
            ENDIF
            ndatafields = EOS_SW_INQDATAFIELDS(swathID, datafieldslist, rank, numbertype)
            IF ndatafields GT 0 THEN BEGIN
               dataFieldNames = StrSplit(datafieldslist, ',', /EXTRACT)
               theDataFields = Replicate({NCDF_DATA_VARIABLE}, ndatafields)
                FOR k=0,ndatafields-1 DO BEGIN
    
                    ; Get information about the variable.
                    sdFileID = HDF_SD_START(filename)
                    sdIndex = HDF_SD_NameToIndex(sdFileID, dataFieldNames[k])
                    sdID = HDF_SD_Select(sdFileID, sdIndex)
                 
                    ; This routine throws all kinds of scary messages if CALDATA, for example, is
                    ; not in the file. Turn this off for this call.
                    !QUIET = 1
                    HDF_SD_GetInfo, sdID, DIMS=dims, NAME=name, NATTS=natts, NDIMS=ndims, $
                        RANGE=range, TYPE=datatype, CALDATA=calData
                    !QUIET = 0
                 Print, 'number of swath dataset attributes for variable ' + name + ': ', natts
                    theDataFields[k].name = dataFieldNames[k]
                    theDataFields[k].datatype = datatype
                    theDataFields[k].calibration = Ptr_New(calData)
                    theDataFields[k].datasize = Ptr_New(dims)
                    IF N_Elements(range) NE 0 THEN BEGIN
                        theDataFields[k].minValue = range[0]
                        theDataFields[k].maxValue = range[1]
                        Undefine, range ; Do this so it is not hanging around for the next variable.
                    ENDIF ELSE BEGIN
                        IF self.no_read_on_parse THEN BEGIN
                            theDataFields[k].minValue = !VALUES.F_NAN
                            theDataFields[k].maxValue = !VALUES.F_NAN             
                        ENDIF ELSE BEGIN
                            HDF_SD_GetData, sdID, data
                            IF calData.cal NE 0 THEN BEGIN
                                data = calData.cal * (Temporary(data) - calData.offset)
                            ENDIF
                            minData = Min(data, MAX=maxData)
                            theGeoFields[k].minValue = minData
                            theGeoFields[k].maxValue = maxData
                            Undefine, data
                         ENDELSE
                    ENDELSE
                  
                    ; If this variable has attributes, get those, too.
                    ; If this variable has attributes, get those, too.
                    IF natts GT 0 THEN BEGIN
                         varAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, natts+1)
                         FOR m=0,natts-1 DO BEGIN
                             HDF_SD_ATTRINFO, sdID, m, DATA=theAttribute, NAME=attribute_name, TYPE=attribute_datatype
                                     
                             varAttributes[m].attrType = StrUpCase(name)
                             varAttributes[m].dataType = attribute_datatype
                             varAttributes[m].length = N_Elements(theAttribute)
                             varAttributes[m].name = attribute_name
                             IF N_Elements(theAttribute) EQ 1 THEN theAttribute = theAttribute[0]
                             varAttributes[m].value = Ptr_New(theAttribute)
                         ENDFOR
                       
                         ; Add the calibration data as an attibute.
                         IF calData.cal EQ 0 THEN BEGIN
                            varAttributes[natts].attrType = StrUpCase(name)
                            varAttributes[natts].dataType = 'STRING'
                            varAttributes[natts].length = 0
                            varAttributes[natts].name = '_calibration_data'
                            varAttributes[natts].value = Ptr_New('Not Present in File')               
                         ENDIF ELSE BEGIN
                            varAttributes[natts].attrType = StrUpCase(name)
                            varAttributes[natts].dataType = 'STRUCT'
                            varAttributes[natts].length = N_Tags(calData, /Length)
                            varAttributes[natts].name = '_calibration_data'
                            varAttributes[natts].value = Ptr_New(calData)
                         ENDELSE
                       
                         theDataFields[k].var_attributes = Ptr_New(varAttributes)
                     ENDIF
                     theSwaths[j].ndataFields = ndataFields
                     HDF_SD_EndAccess, sdID
                ENDFOR
            ENDIF
            nmaps = EOS_SW_INQMAPS(swathID, mapslist, offset, increment)
            theSwaths[j].nmaps = nmaps
            IF nmaps GT 0 THEN BEGIN
               mapNames = StrSplit(mapslist, ',', /EXTRACT)
                FOR k=0,nmaps-1 DO BEGIN
                    Print, 'Map Name: ', mapNames[k], $
                        '   Offset: ', offset[k], '   Increment: ', increment[k]
                ENDFOR
                Print, ''
            ENDIF
            nidxmaps = EOS_SW_INQIDXMAPS(swathID, mapslist, sizes)
            theSwaths[j].nidxmaps = nidxmaps
            IF nidxmaps GT 0 THEN BEGIN
               mapNames = StrSplit(mapslist, ',', /EXTRACT)
                FOR k=0,nidxmaps-1 DO BEGIN
                    Print, 'Map Name: ', mapNames[k], '   Size: ', sizes[k]
                ENDFOR
                Print, ''
            ENDIF
            ok = EOS_SW_DETACH(swathID)
            ok = EOS_SW_CLOSE(fileID)
        ENDFOR 
        self.theSwaths = Ptr_New(theSwaths)
   ENDIF
   
   
   fileID = HDF_SD_START(Filepath(ROOT_DIR=self.directory, self.filename), /READ)
   HDF_SD_Fileinfo, fileID, num_vars, num_attr
   
   ; First, get the global attributes.
;   IF num_attr GT 0 THEN BEGIN
;      theAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, num_attr)
;      FOR j=0,num_attr-1 DO BEGIN
;          HDF_SD_ATTRINFO, fileID, j, DATA=theAttribute, HDF_TYPE=hdf_type, NAME=attribute_name, TYPE=att_type
;
;          theAttributes[j].attrType = 'GLOBAL'
;          theAttributes[j].dataType = att_type
;          theAttributes[j].length = N_Elements(theAttribute)
;          theAttributes[j].name = attribute_name
;          IF N_Elements(theAttribute) EQ 1 THEN theAttribute = theAttribute[0]
;          theAttributes[j].value = Ptr_New(theAttribute)
;
;      ENDFOR
;      self.theAttributes = Ptr_New(theAttributes, /No_Copy)
;   ENDIF
  ; Successfully parsed file.
   self.hasBeenParsed = 1
   
   ; Close the file
   HDF_SD_End, fileID

END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::ReadAttribute, theAttribute, SUCCESS=success
;
; NAME:
;       NCDF_DATA::ReadAttribute
;
; PURPOSE:
;
;       This method is used to read and return a global attribute from a netCDF or HDF file.
;
; CALLING SEQUENCE:
;
;       IDL> value = nCDFObject -> ReadAttribute(theAttribute)
;
; RETURN VALUE:
;
;       value:      A variable containing the attribute.
;
; ARGUMENTS:
;
;       theAttribute: The name of the attribute you wish to read from the file.
;
; KEYWORD PARAMETERS:
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      success = 0
      RETURN, -1
   ENDIF
   
   ; Make sure the file has been parsed.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile
   
   ; Check again.
   success = 0
   IF self.hasBeenParsed EQ 0 THEN RETURN, -1
   
   ; Check for the name of the attribute.
   IF N_Elements(theAttribute) EQ 0 THEN Message, 'Must pass name of the attribute to read.'

   IF Ptr_Valid(self.theAttributes) EQ 0 THEN Message, 'No global attributes currently available for file.'
   index = Where(StrUpCase((*self.theAttributes).name) EQ StrUpCase(theAttribute), count)
   IF count GT 0 THEN BEGIN
      value = *(*self.theAttributes)[index].value
      success = 1
   ENDIF ELSE Message, 'Cannot locate global attribute ' + theAttribute + ' in file.'
   RETURN, value

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadAttributeFromGUI, event
   ; This internal method sets up a dialog for obtaining information from the user
   ; about which variables to read, etc.
   
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Get some position information.
   Widget_Control, event.top, TLB_GET_OFFSET=offsets

   ; We want a modal pop-up dialog widget.
   tlb = Widget_Base(GROUP_LEADER=event.top, XOFFSET=offsets[0]+50, YOFFSET=offsets[1]+50, $
      COLUMN=1, BASE_ALIGN_CENTER=1, /FLOATING, UVALUE=self, /MODAL)
   row = Widget_Base(tlb, ROW=2, /GRID_LAYOUT, FRAME=1)
   label = Widget_Label(row, Value='Attribute to Read: ')
   theList = ['All', (*self.theAttributes).name]
   self.attributeID = Widget_Droplist(row, Value=theList, UNAME='ATTRIBUTES', $
      UVALUE=['all_attributes', (*self.theAttributes).name], SCR_XSIZE=250)
   label = Widget_Label(row, Value='Attribute Name: ')
   self.attrnameID = Widget_Text(row, Value='all_attributes', /Editable, SCR_XSIZE=250)
   b = Widget_Base(tlb, ROW=1, XPAD=0, YPAD=0, /NONEXCLUSIVE)
   
   okToAppend = 1
   IF StrPos(cgRootName(self.filename), '.') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, '-') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, ' ') NE -1 THEN okToAppend = 0
   IF okToAppend THEN self.appendNameID = Widget_Button(b, Value='Append Filename to Attribute Name', UVALUE='APPEND_FILENAME')
   buttonrow = Widget_Base(tlb, ROW=1)
   button = Widget_Button(buttonrow, Value='Read Attribute and Leave', UVALUE='READ_ATTRIBUTE_AND_LEAVE')
   button = Widget_Button(buttonrow, Value='Read Attribute and Stay', UVALUE='READ_ATTRIBUTE_AND_STAY')
   button = Widget_Button(buttonrow, Value='Quit', UVALUE='QUIT_READ_ATTRIBUTE_GUI')
   
   ; If there is a tree selection, see if this corresponds to a variable in the list.
   ; If so, set this variable in the droplist widget.
   theSelection = Widget_Info(self.theTree, /TREE_SELECT)
   Widget_Control, theSelection, Get_Value=attrName
   index = Where(theList EQ attrName, count)
   IF count GT 0 THEN BEGIN
      Widget_Control, self.attributeID, SET_DROPLIST_SELECT=index
      Widget_Control, self.attrnameID, Set_Value=IDL_ValidName(theList[index], /CONVERT_ALL)
   ENDIF
   
   ; Get it going...
   Widget_Control, tlb, /REALIZE
   XMANAGER, 'read_attribute_and_leave', tlb, EVENT_HANDLER='NCDF_DATA_WIDGET_EVENTS', /NO_BLOCK
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadAttributeFromGUI_Events, event
   ; This internal method processes events from the user dialogs.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   thisEvent = Tag_Names(event, /STRUCTURE_NAME)
   CASE thisEvent OF
   
      'WIDGET_BUTTON': BEGIN
      
         Widget_Control, event.id, Get_UValue=buttonValue
         CASE buttonValue OF
         
            'READ_ATTRIBUTE_AND_STAY': BEGIN
            
               ; Get the variable name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
               Widget_Control, self.attrNameID, Get_Value=attrName
               thisAttrName = IDL_ValidName(attrName, /CONVERT_ALL)
               IF thisAttrName NE attrName THEN BEGIN
                  Widget_Control, self.attrNameID, Set_Value=thisAttrName       
                  attrName = thisAttrName
               ENDIF
               
               attrName = (addName) ? cgRootName(self.filename) + '_' + attrName[0] : attrName[0]
               IF attrName EQ "" THEN Message, 'Must have a non-null attribute name to create an attribute.'
               
               ; Which attribute do you want to read?
               Widget_Control, self.attributeID, Get_Value=theList
               index = Widget_Info(self.attributeID, /DROPLIST_SELECT)
               theAttribute = theList[index]
               IF StrUpCase(theAttribute) EQ 'ALL' THEN BEGIN
                   theData = self -> ReadGlobalAttr(Success=success)
                   IF success EQ 0 THEN RETURN
               ENDIF ELSE BEGIN
                   theData = self -> ReadAttribute(theAttribute, Success=success)
                   IF success EQ 0 THEN RETURN
               ENDELSE
               
               ; Create the variable at the main IDL level. 
               (Scope_VarFetch(attrName, LEVEL=1, /ENTER)) = theData
               Print, 'An attribute named "' + attrName + '" has been created at the main IDL level.'
               
               ; Go to the next attribute on the list
               IF index EQ (N_Elements(theList)-1) THEN index = 0 ELSE index = index + 1
               Widget_Control, self.attributeID, SET_DROPLIST_SELECT=index
               Widget_Control, self.attrNameID, Set_Value=theList[index]
               END
               
            'READ_ATTRIBUTE_AND_LEAVE': BEGIN
               
               ; Get the attribute name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
               Widget_Control, self.attrNameID, Get_Value=attrName
               attrName = (addName) ? cgRootName(self.filename) + '_' + attrName[0] : attrName[0]
               thisAttrName = IDL_ValidName(attrName, /CONVERT_ALL)
               IF thisAttrName NE attrName THEN BEGIN
                  Widget_Control, self.attrNameID, Set_Value=thisAttrName       
                  attrName = thisAttrName
               ENDIF
               IF attrName EQ "" THEN Message, 'Must have a non-null attribute name to create an attribute.'
               
               ; Which attribute do you want to read?
               Widget_Control, self.attributeID, Get_Value=theList
               index = Widget_Info(self.attributeID, /DROPLIST_SELECT)
               theAttribute = theList[index]
               IF StrUpCase(theAttribute) EQ 'ALL' THEN BEGIN
                   theData = self -> ReadGlobalAttr(Success=success)
                   IF success EQ 0 THEN RETURN
               ENDIF ELSE BEGIN
                   theData = self -> ReadAttribute(theAttribute, Success=success)
                   IF success EQ 0 THEN RETURN
               ENDELSE
               
               ; Create the attribute at the main IDL level.
               (Scope_VarFetch(attrName, LEVEL=1, /ENTER)) = theData
               Print, 'An attribute named "' + attrName + '" has been created at the main IDL level.'
               
               Widget_Control, event.top, /DESTROY
               END
               
            'QUIT_READ_ATTRIBUTE_GUI': Widget_Control, event.top, /DESTROY
            
         ENDCASE
      
         END
         
      'WIDGET_DROPLIST': BEGIN
         ; The name of the variable to write has to be changed when the droplist value changes.
         Widget_Control, event.id, Get_UVALUE=list
         Widget_Control, self.attrNameID, Set_Value=IDL_ValidName(list[event.index], /CONVERT_ALL)
         END
   
      'WIDGET_TEXT': ; Nothing to do here. We just want to read the value. Don't what it is.
      
   ENDCASE
END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::ReadDimension, dimensionName, SUCCESS=success
;
; NAME:
;       NCDF_DATA::ReadDimension
;
; PURPOSE:
;
;       This method is used to read and return a dimension of a netCDF file.
;
; CALLING SEQUENCE:
;
;       IDL> dimension = nCDFObject -> ReadDimension(dimensionName)
;
; RETURN VALUE:
;
;       dimension: The value of the dimension.
;
; ARGUMENTS:
;
;       dimensionName:   The name of the dimension to read.
;
; KEYWORD PARAMETERS:
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      success = 0
      RETURN, -1
   ENDIF
      
   ; Check for the name of the file.
   theFile = Filepath(ROOT_DIR=self.directory, self.filename)

   ; Open the file.
   fileID = NCDF_Open(theFile)
   info = NCDF_Inquire(fileID)
   
   ; Add the dimensions.
   IF info.ndims GT 0 THEN BEGIN
      dimsStruct = Create_Struct('ndims', info.ndims)
      FOR j=0,info.ndims-1 DO BEGIN
         NCDF_DIMINQ, fileID, j, name, value
         name = IDL_ValidName(name, /CONVERT_ALL)
         dimsStruct = Create_Struct(dimsStruct, name, value)
       ENDFOR
    ENDIF
   
   ; Can you find a field in the structure with the dimension name?
   fields = Tag_Names(dimsStruct)
   i = Where(fields EQ StrUpCase(dimensionName), count)
   IF count EQ 0 THEN Message, 'Cannot find a dimension with name: ' + dimensionName + ' in file.'
   value = dimsStruct.(i)
  
   ; Close the file, set status flag, return the data.
   NCDF_CLOSE, fileID
   success = 1
   RETURN, value

END ;---------------------------------------------------------------------------------------------




FUNCTION NCDF_DATA::ReadFile, theFile, SUCCESS=success
;
; NAME:
;       NCDF_DATA::ReadFile
;
; PURPOSE:
;
;       This method is used to read and return the contents of a netCDF or HDF file.
;
; CALLING SEQUENCE:
;
;       IDL> data = nCDFObject -> ReadFile(theFile)
;
; RETURN VALUE:
;
;       data:      A structure variable containing the filename, a structure of global attributes,
;                  a structure of dimensions, and one struture for each variable in the file.
;
; ARGUMENTS:
;
;       theFile:   The optional name of a netCDF or HDF file to read. If not supplied, the
;                  name of the file currently stored in the object will be read.
;
; KEYWORD PARAMETERS:
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      success = 0
      RETURN, -1
   ENDIF
      
   ; Check for the name of the file.
   IF N_Elements(theFile) EQ 0 THEN theFile = Filepath(ROOT_DIR=self.directory, self.filename)
   IF File_Test(theFile, /READ) EQ 0 THEN Message, 'Specified file does not exist or is not readable.'

   ; Branch appropriately.
   IF self.isHDF THEN BEGIN
   
       ; Open the file.
       fileID = HDF_SD_Start(theFile)
       
       ; Create the initial structure.
       struct = Create_Struct('_filename', self.filename)
   
       ; Add the global attributes.
       g_attributes = self -> ReadGlobalAttr(Success=success)
       IF success THEN struct = Create_Struct(struct, '_global_attr', Temporary(g_attributes))

       HDF_SD_Fileinfo, fileID, nvars, nattrs
       FOR j=0,nvars-1 DO BEGIN
       
           ; Select the variable and read it.
           varID = HDF_SD_Select(fileID, j)
           HDF_SD_GetData, varID, data
           HDF_SD_GetInfo, varID, NAME=varName, NATTS=natts
           
           data = Reform(Temporary(data))
           varStruct = Create_Struct('data', Temporary(data))
           
           ; If this variable has attributes, get those, too.
           IF natts GT 0 THEN BEGIN
                varAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, natts)
                FOR k=0,natts-1 DO BEGIN
                    HDF_SD_ATTRINFO, varID, k, DATA=value, NAME=attrName
                    attrName = IDL_ValidName(attrName, /CONVERT_ALL)
                    IF Where(Tag_Names(varStruct) EQ StrUpCase(attrName)) NE -1 THEN CONTINUE
                    varStruct = Create_Struct(varStruct, attrName, value)         
                ENDFOR
                struct = Create_Struct(struct, varName, Temporary(varStruct))
                
           ENDIF
       ENDFOR
       HDF_SD_EndAccess, varID
       HDF_SD_END, fileID
       success = 1

   ENDIF ELSE BEGIN
   
       ; Open the file.
       fileID = NCDF_Open(theFile)
       info = NCDF_Inquire(fileID)
       
       ; Create the initial structure.
       struct = Create_Struct('_filename', self.filename)
       
       ; Add the global attributes.
       g_attributes = self -> ReadGlobalAttr(Success=success)
       IF success THEN struct = Create_Struct(struct, '_global_attr', Temporary(g_attributes))
    
       ; Add the dimensions.
       IF info.ndims GT 0 THEN BEGIN
          dimsStruct = Create_Struct('_ndims', info.ndims)
          FOR j=0,info.ndims-1 DO BEGIN
             NCDF_DIMINQ, fileID, j, name, value
             name = IDL_ValidName(name, /CONVERT_ALL)
             dimsStruct = Create_Struct(dimsStruct, name, value)
           ENDFOR
           struct = Create_Struct(struct, '_dimensions', dimsStruct)
       ENDIF
       
       ; Add the variables.
       IF info.nvars GT 0 THEN BEGIN
          FOR j=0,info.nvars-1 DO BEGIN
             varInfo = NCDF_VarInq(fileID, j)
             NCDF_VarGet, fileID, j, data
             IF Size(data, /N_DIMENSIONS) GT 0 THEN data = REFORM(Temporary(data))
             varStruct = Create_Struct('data', Temporary(data))
             
             ; Add the variable attributes to the structure.
             FOR k=0,varInfo.natts-1 DO BEGIN
                attrName = NCDF_AttName(fileID, j, k)
                NCDF_AttGet, fileID, j, attrName, theAttribute
                attinfo = NCDF_ATTINQ(fileID, j, attrName)
                att_type = StrUpCase(attinfo.dataType)
              
                ; Strings are stored as byte values in attributes, so convert them back.
                IF (Size(theAttribute, /TNAME) EQ 'BYTE') AND (att_type EQ 'CHAR') $
                   THEN theAttribute = String(theAttribute)
                
                 attrName = IDL_ValidName(attrName, /CONVERT_ALL)
                 varStruct = Create_Struct(varStruct, attrName, theAttribute)
             ENDFOR
          struct = Create_Struct(struct, IDL_ValidName(varInfo.name, /CONVERT_ALL), $
              Temporary(varStruct))
          ENDFOR
       ENDIF
       
       ; Close the file, set status flag, return the data.
       NCDF_CLOSE, fileID
       success = 1
   
   ENDELSE
   
   RETURN, struct

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadFileFromGUI, event
   ; This internal method obtains the name of the variable from the user
   ; and creates a variable of that name at the main IDL level.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF

   ; What would you like to name the variable.
   varName = TextBox(Title='Name of IDL Variable...', Label='Name of IDL Variable:', $
      Value='data', Cancel=cancelled)
   IF cancelled THEN RETURN
   varName = IDL_ValidName(varName, /CONVERT_ALL)
   IF varName EQ "" THEN Message, 'Variable names cannot be NULL.'
   
   ; Read the NCDF data file.
   Widget_Control, /HOURGLASS
   data = self -> ReadFile(Success=success)
   IF success EQ 0 THEN RETURN
   
   ; Create a main-level variable.
   (Scope_VarFetch(varName, LEVEL=1, /ENTER)) = data
    Print, 'A variable named "' + varName + '" has been created at the main IDL level.'
END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::ReadGlobalAttr, SUCCESS=success
;
; NAME:
;       NCDF_DATA::ReadGlobalAttr
;
; PURPOSE:
;
;       This method is used to read and return the global attributes of a netCDF or HDF file.
;
; CALLING SEQUENCE:
;
;       IDL> struct = nCDFObject -> ReadGlobalAttr()
;
; RETURN VALUE:
;
;       struct:      A structure variable containing global attributes of the file.
;                    The attribute names are the fields of the structure.
;
; ARGUMENTS:
;
;       None. The global attributes of the file loaded into the object will be read and returned.
;
; KEYWORD PARAMETERS:
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      success = 0
      RETURN, -1
   ENDIF
   
   ; Make sure the file has been parsed.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile
   
   ; Check again.
   success = 0
   IF self.hasBeenParsed EQ 0 THEN RETURN, -1
   
   IF self.isHDF THEN BEGIN
   
        ; Open the file and find out how many dimensions, global attributes, and variables are there.
        fileID = HDF_SD_START(Filepath(ROOT_DIR=self.directory, self.filename), /READ)
        HDF_SD_Fileinfo, fileID, num_vars, num_attr
   
       ; Create a structure to hold the global attribute values.
       attrStruct = Create_Struct('hdf_filename', self.filename)
       FOR j=0,num_attr-1 DO BEGIN
           HDF_SD_ATTRINFO, fileID, j, DATA=value, NAME=name
           
          ; Names cannot have oddball characters in them. They have to
          ; conform to IDL's rules for creating variable names.
          name = IDL_ValidName(name, /CONVERT_ALL)
          IF Where(Tag_Names(attrStruct) EQ StrUpCase(name)) NE -1 THEN CONTINUE
          attrStruct = Create_Struct(attrStruct, name, value)
       ENDFOR
       
       ; Close the file, set status flag, return the data.
       HDF_SD_End, fileID
       success = 1
       RETURN, attrStruct

   ENDIF ELSE BEGIN

       ; Open the file.
       fileID = NCDF_Open(Filepath(ROOT_DIR=self.directory, self.filename))
       info = NCDF_Inquire(fileID)
    
       ; Create a structure to hold the global attribute values.
       attrStruct = Create_Struct('ncdf_filename', self.filename)
       FOR j=0,info.ngatts-1 DO BEGIN
          name = NCDF_AttName(fileID, j, /GLOBAL)
          NCDF_AttGet, fileID, name, value, /GLOBAL
          attinfo = NCDF_ATTINQ(fileID, name, /GLOBAL)
          att_type = StrUpCase(attinfo.dataType)
          IF Size(value, /TNAME) EQ 'BYTE' AND att_type EQ 'CHAR' THEN value = String(value)
          
          ; Names cannot have oddball characters in them. They have to
          ; conform to IDL's rules for creating variable names.
          name = IDL_ValidName(name, /CONVERT_ALL)
          attrStruct = Create_Struct(attrStruct, name, value)
       ENDFOR
    
       ; Close the file, set status flag, return the data.
       NCDF_CLOSE, fileID
       success = 1
       RETURN, attrStruct

   ENDELSE
END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::ReadVariable, theVariable, $
    SUCCESS=success, $
    COUNT=count, $
    FILLVALUE=fillvalue, $
    MISSINGINDICES=missingIndices, $
    OFFSET=offset, $
    START=start, $
    STRIDE=stride
;
; NAME:
;       NCDF_DATA::ReadVariable
;
; PURPOSE:
;
;       This method is used to read and return a variable from a netCDF or HDF file.
;
; CALLING SEQUENCE:
;
;       IDL> data = nCDFObject -> ReadVariable(theVariable)
;
; RETURN VALUE:
;
;       data:      The nCDF variable.
;
; ARGUMENTS:
;
;       theVariable: The name of the variable you wish to read from the file.
;
; INPUT KEYWORD PARAMETERS:
; 
;       COUNT:      An optional vector containing the counts to be used in reading theVariable.
;                   Count is a 1-based vector with an element for each dimension. The default 
;                   matches the size of the variable so that all data is written out. 
;                   
;       OFFSET:     An optional vector containing the starting position for the read. The default 
;                   start position is [0, 0, ...].
;                   
;       START:      Equivalent to the OFFSET vector, except for HDF files.
;                   
;       STRIDE:     An optional vector containing the strides, or sampling intervals, between 
;                   accessed values of the netCDF variable. The default stride vector is that 
;                   for a contiguous read, [1, 1, ...]. Note that for HDF files, the default
;                   STRIDE vector is [0, 0, ...].
;       
; OUTPUT KEYWORD PARAMETERS:
; 
;       FILLVALUE:  The value that is being used for the "missing" value in this variable.
;                                                                              
;       MISSINGINDICES: A vector containing the missing indices in the returned data. Missing
;                   data is identified by either the depreciated "missing_value" attribute
;                   or the approved "_FillValue" attribute.  
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;
;

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      IF self.isHDF THEN HDF_SD_End, fileID ELSE NCDF_CLOSE, fileID
      success = 0
      RETURN, -1
   ENDIF
   
   ; Check for the name of the variable.
   IF N_Elements(theVariable) EQ 0 THEN Message, 'Must pass name of variable to read.'
   
   ; Assume no success.
   success = 0
   
   ; Read the variable, based on what kind of file this is.
   IF self.isHDF THEN BEGIN
   
       ; Open the file.
       fileID = HDF_SD_Start(Filepath(ROOT_DIR=self.directory, self.filename))
       
       ; Get the index of the variable.
       index = HDF_SD_NameToIndex(fileID, theVariable)
       IF index EQ -1 THEN Message, 'Variable (' + theVariable + ') not found.'
       
       ; Select the variable and read it.
       varID = HDF_SD_Select(fileID, index)
       
       ; Make sure this variable has a valid dimension.
       HDF_SD_GetInfo, varID, DIMS=dims
       IF dims[0] EQ 0 THEN BEGIN
            void = Dialog_Message('Requested data variable has a dimension of 0 and cannot be read.')
            RETURN, -1
       ENDIF
       
       ; Read the data.
       HDF_SD_GetData, varID, data, COUNT=count, START=start, STRIDE=stride
       
       ; This routine throws all kinds of scary messages if CALDATA is
       ; not in the file. Turn this off for this call.
       !QUIET = 1
       HDF_SD_GetInfo, varID, CALDATA=calData
       !QUIET = 0
         
       HDF_SD_EndAccess, varID
       
       ; Reverse the indices in HDF files.
       data = Reform(Temporary(data))
       IF calData.cal NE 0 THEN data = calData.cal * (Temporary(data) - calData.offset)
       
       ; Close the file
       HDF_SD_End, fileID
       success = 1
       RETURN, data
       
   ENDIF ELSE BEGIN

       ; Open the file.
       fileID = NCDF_Open(Filepath(ROOT_DIR=self.directory, self.filename))
       
       ; Get the variable ID.
       varID = NCDF_VarID(fileID, theVariable)
       
       ; Get information about the variable.
       r = NCDF_VarInq(fileID, varID)
           
       ; Do we have to worry about zero dimensions?
       IF Ptr_Valid(self.zeroDimensionID) THEN BEGIN
       
           ; Is there a match between the dimension IDs and any zero dimension ID we 
           ; have stored?
           match = 0
           FOR m = 0, N_Elements(*self.zeroDimensionID)-1 DO BEGIN
               i = Where(r.dim EQ (*self.zeroDimensionID)[m], count)
               IF count GT 0 THEN match = 1
           ENDFOR
           IF match GT 0 THEN BEGIN
               ok = Dialog_Message('This variable has a dimension of length zero and cannot be read.')
               NCDF_CLOSE, fileID
               success = 0
               RETURN, -1             
           ENDIF 
       ENDIF
       
       ; Read the data.
       NCDF_VarGet, fileID, varID, data, COUNT=count, OFFSET=offset, STRIDE=stride
       
       ; Get the variable attribute names
       IF r.natts GT 0 THEN varAttNames = StrArr(r.natts)
       FOR k=0,r.natts-1 DO BEGIN
           varAttNames[k] = NCDF_AttName(fileID, varID, k)
       ENDFOR
       
       ; Does this variable contain "missing" values. If so, identify and return
       ; the missing data indices so they can be identified after scaling.
       index = Where(StrUpCase(varAttNames) EQ 'MISSING_VALUE', count)
       IF count GT 0 THEN BEGIN
           varAttName = (varAttNames[index])[0]
           NCDF_AttGet, fileID, varID, varAttName, missingValue
           missingIndices = Where(data EQ missingValue, missingCount)
       ENDIF
       index = Where(StrUpCase(varAttNames) EQ '_FILLVALUE', count)
       IF count GT 0 THEN BEGIN
           varAttName = (varAttNames[index])[0]
           NCDF_AttGet, fileID, varID, varAttName, missingValue
           missingIndices = Where(data EQ missingValue, missingCount)
       ENDIF
    
       ; Is there a scale_factor attribute? If so, get and scale the data.
       IF N_Elements(varAttNames) NE 0 THEN BEGIN
           index = Where(StrUpCase(varAttNames) EQ 'SCALE_FACTOR', count)
           IF count GT 0 THEN BEGIN
               varAttName = (varAttNames[index])[0]
               NCDF_AttGet, fileID, varID, varAttName, scale_factor
               IF scale_factor NE 1.0 THEN data = Temporary(data) * scale_factor
           ENDIF
           
           ; Is there an add_offset attribute? If so, get and add to the data.
           index = Where(StrUpCase(varAttNames) EQ 'ADD_OFFSET', count)
           IF count GT 0 THEN BEGIN
               varAttName = (varAttNames[index])[0]
               NCDF_AttGet, fileID, varID, varAttName, add_offset
               data = Temporary(data) + add_offset
           ENDIF
       ENDIF
       
       ; If there was missing data, restore it.
       IF (N_Elements(missingIndices) NE 0) THEN BEGIN
            IF missingCount GT 0 THEN data[missingIndices] = missingValue
       ENDIF
       
       ; Is this a CHAR data type? If so, convert it to a string.
       IF StrUpCase(r.datatype) EQ 'CHAR' THEN data = String(data)
              
       ; Close the file, set status flag, return the data.
       NCDF_CLOSE, fileID
       success = 1
       RETURN, data
       
   ENDELSE
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadVariableFromGUI, event
   ; This internal method sets up a dialog for obtaining information from the user
   ; about which variables to read, etc.
   
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Get some position information.
   Widget_Control, event.top, TLB_GET_OFFSET=offsets

   ; We want a modal pop-up dialog widget.
   tlb = Widget_Base(GROUP_LEADER=event.top, XOFFSET=offsets[0]+50, YOFFSET=offsets[1]+50, $
      COLUMN=1, BASE_ALIGN_CENTER=1, /FLOATING, UVALUE=self, /MODAL)
   row = Widget_Base(tlb, ROW=2, /GRID_LAYOUT, FRAME=1)
   label = Widget_Label(row, Value='Variable to Read: ')
   theList = [(*self.theVariables).name]
   self.variablelistID = Widget_Droplist(row, Value=[(*self.theVariables).name], $
      UVALUE=[(*self.theVariables).name], SCR_XSIZE=250, UNAME='VARIABLES')
   label = Widget_Label(row, Value='Variable Name: ')
   self.varnameID = Widget_Text(row, Value=IDL_ValidName((*self.theVariables)[0].name, /CONVERT_ALL), $
      /Editable, SCR_XSIZE=250)
   b = Widget_Base(tlb, ROW=1, XPAD=0, YPAD=0, /NONEXCLUSIVE)
   
   okToAppend = 1
   IF StrPos(cgRootName(self.filename), '.') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, '-') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, ' ') NE -1 THEN okToAppend = 0
   IF okToAppend THEN self.appendNameID = Widget_Button(b, Value='Append Filename to Variable Name', UVALUE='APPEND_FILENAME')
   buttonrow = Widget_Base(tlb, ROW=1)
   button = Widget_Button(buttonrow, Value='Read Variable and Leave', UVALUE='READ_AND_LEAVE')
   button = Widget_Button(buttonrow, Value='Read Variable and Stay', UVALUE='READ_AND_STAY')
   button = Widget_Button(buttonrow, Value='Quit', UVALUE='QUIT_READ_VARIABLE_GUI')
   
   ; If there is a tree selection, see if this corresponds to a variable in the list.
   ; If so, set this variable in the droplist widget.
   theSelection = Widget_Info(self.theTree, /TREE_SELECT)
   Widget_Control, theSelection, Get_Value=varName
   index = Where(theList EQ varName, count)
   IF count GT 0 THEN BEGIN
      Widget_Control, self.variablelistID, SET_DROPLIST_SELECT=index
      Widget_Control, self.varnameID, Set_Value=IDL_ValidName(theList[index], /CONVERT_ALL)
   ENDIF
   
   ; Get it going...
   Widget_Control, tlb, /REALIZE
   XMANAGER, 'read_and_leave', tlb, EVENT_HANDLER='NCDF_DATA_WIDGET_EVENTS', /NO_BLOCK
END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::ReadVarAttr, theVariableName, theAttributeName

; This method reads and returns a particular variable attribute.
; Both the name of the variable and the name of the attribute are
; required parameters.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN, ""
   ENDIF

   IF N_Params() NE 2 THEN Message, 'Both the variable name and the attribute name must be present.'

   ; The file has to be parsed to carry this out.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile
   
   ; Get the variable list.
   theVarStructures = *self.theVariables
   
   ; Find this variable in the variable structures.
   index = Where(StrUpCase(theVarStructures.name) EQ StrUpCase(theVariableName), count)
   IF count EQ 0 THEN Message, 'Cannot find the variable ' + theVariableName + ' in the file.'
   thisVariableStruct = theVarStructures[index]
   
   ; Get the pointer to the variable attribute structures.
   varAttrStructures = *thisVariableStruct.var_attributes
   
   ; Find the name of the attribute in the varAttrStructures list
   index = Where(StrUpCase(varAttrStructures.name) EQ StrUpCase(theAttributeName), count)
   IF count EQ 0 THEN Message, 'Cannot find the attribute ' + theAttributeName + ' in the file.'
   
   ; Extract the attribute names.
   theAttributeStruct = varAttrStructures[index]
   theAttributeValue = *theAttributeStruct.value
   
   RETURN, theAttributeValue
    
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadVariableFromGUI_Events, event
   ; This internal method processes events from the user dialogs.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   thisEvent = Tag_Names(event, /STRUCTURE_NAME)
   CASE thisEvent OF
   
      'WIDGET_BUTTON': BEGIN
      
         Widget_Control, event.id, Get_UValue=buttonValue
         CASE buttonValue OF
         
            'READ_AND_STAY': BEGIN
            
               ; Get the variable name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
               Widget_Control, self.varNameID, Get_Value=varName
               varName = (addName) ? cgRootName(self.filename) + '_' + varName[0] : varName[0]
               thisVarName = IDL_ValidName(varName, /CONVERT_ALL)
               IF thisVarName NE varName THEN BEGIN
                  Widget_Control, self.varNameID, Set_Value=IDL_ValidName(thisVarName, /CONVERT_ALL)       
                  varName = thisVarName
               ENDIF
               IF varName EQ "" THEN Message, 'Must have a non-null variable name to create a variable.'
               
               ; Which variable do you want to read?
               Widget_Control, self.variableListID, Get_UValue=theList
               index = Widget_Info(self.variableListID, /DROPLIST_SELECT)
               theVariable = theList[index]
               Widget_Control, /HOURGLASS
               theData = self -> ReadVariable(theVariable, Success=success)
               IF success EQ 0 THEN RETURN
               
               ; Create the variable at the main IDL level.
               (Scope_VarFetch(varName, LEVEL=1, /ENTER)) = theData
               Print, 'A variable named "' + varName + '" has been created at the main IDL level.'
               
               ; Go to the next variable on the list
               IF index EQ (N_Elements(theList)-1) THEN index = 0 ELSE index = index + 1
               Widget_Control, self.variableListID, SET_DROPLIST_SELECT=index
               Widget_Control, self.varnameID, Set_Value=IDL_ValidName(theList[index], /CONVERT_ALL)
               END
               
            'READ_AND_LEAVE': BEGIN
               
               ; Get the variable name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
               Widget_Control, self.varNameID, Get_Value=varName
               varName = (addName) ? cgRootName(self.filename) + '_' + varName[0] : varName[0]
               IF varName EQ "" THEN Message, 'Must have a non-null variable name to create a variable.'
               
               ; Which variable do you want to read?
               Widget_Control, self.variableListID, Get_UValue=theList
               index = Widget_Info(self.variableListID, /DROPLIST_SELECT)
               theVariable = theList[index]
               Widget_Control, /HOURGLASS
               theData = self -> ReadVariable(theVariable, Success=success)
               IF success EQ 0 THEN RETURN
              
               ; Create the variable at the main IDL level.
               (Scope_VarFetch(varName, LEVEL=1, /ENTER)) = theData
               Print, 'A variable named "' + varName + '" has been created at the main IDL level.'
               
               Widget_Control, event.top, /DESTROY
               END
               
            'QUIT_READ_VARIABLE_GUI': Widget_Control, event.top, /DESTROY
            
         ENDCASE
      
         END
         
      'WIDGET_DROPLIST': BEGIN
         ; The name of the variable to write has to be changed when the droplist value changes.
         Widget_Control, event.id, Get_UVALUE=list
         Widget_Control, self.varNameID, Set_Value=IDL_ValidName(list[event.index], /CONVERT_ALL)
         END
   
      'WIDGET_TEXT': ; Nothing to do here. We just want to read the value. Don't care what it is.
      
   ENDCASE
END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::ReadVariableWithAttr, theVariable, SUCCESS=success
;
; NAME:
;       NCDF_DATA::ReadVariableWithAttr
;
; PURPOSE:
;
;       This method is used to read and return a variable and its attributes from a netCDF or HDF file.
;
; CALLING SEQUENCE:
;
;       IDL> struct = nCDFObject -> ReadVariable(theVariable)
;
; RETURN VALUE:
;
;       struct:      A structure containing the variable (in the field "data") and its
;                    attributes in other fields. Plus, the field NDIMS holds the number
;                    of dimensions of the variable, and the field DIMS is a vector of
;                    the dimensions of the variable (for HDF files) or the dimension
;                    IDs (for netCDF file).
;
; ARGUMENTS:
;
;       theVariable: The name of the variable you wish to read from the file.
;
; KEYWORD PARAMETERS:
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      success = 0
      RETURN, -1
   ENDIF
   
   ; Make sure the file has been parsed.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile
   
   ; Check again.
   success = 0
   IF self.hasBeenParsed EQ 0 THEN RETURN, -1

   ; Check for the name of the variable.
   IF N_Elements(theVariable) EQ 0 THEN Message, 'Must pass name of variable to read.'
   
   ; Branch on type of file to read.
   IF self.isHDF THEN BEGIN
   
       ; Open the file.
       fileID = HDF_SD_Start(Filepath(ROOT_DIR=self.directory, self.filename))
       
       ; Get the variable ID.
       index = HDF_SD_NameToIndex(fileID, theVariable)
       IF index EQ -1 THEN Message, 'Variable (' + theVariable + ') not found.'
       
       ; Select the variable and read it.
       varID = HDF_SD_Select(fileID, index)
       HDF_SD_GetData, varID, data
       
       ; This routine throws all kinds of scary messages if CALDATA is
       ; not in the file. Turn this off for this call.
       !QUIET = 1
       HDF_SD_GetInfo, varID, CALDATA=calData, DIMS=dims, NDIMS=ndims
       !QUIET = 0
         
       ; Reverse the indices in HDF files and calibrate, if neccesary.
       data = Reform(Temporary(data))
       IF calData.cal NE 0 THEN data = calData.cal * (Temporary(data) - calData.offset)
       varStruct = Create_Struct('data', Temporary(data))
       
       ; If this variable has attributes, get those, too.
       HDF_SD_GetInfo, varID, NATTS=natts
       IF natts GT 0 THEN BEGIN
            varAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, natts+1)
            FOR k=0,natts-1 DO BEGIN
                HDF_SD_ATTRINFO, varID, k, DATA=value, NAME=attrName
                attrName = IDL_ValidName(attrName, /CONVERT_ALL)
                IF Where(Tag_Names(varStruct) EQ StrUpCase(attrName)) NE -1 THEN CONTINUE
                varStruct = Create_Struct(varStruct, attrName, value)         
            ENDFOR
            IF calData.cal EQ 0 $
                THEN varStruct = Create_Struct(varStruct, '_calibration_data', 'Not Present in File') $
                ELSE varStruct = Create_Struct(varStruct, '_calibration_data', catData)
       ENDIF
       
       ; Add dimension informatio to the structure.
       varStruct = Create_Struct(varStruct, 'ndims', ndims, 'dims', dims)

       HDF_SD_EndAccess, varID
       HDF_SD_END, fileID
       success = 1
       
   ENDIF ELSE BEGIN

       ; Open the file.
       fileID = NCDF_Open(Filepath(ROOT_DIR=self.directory, self.filename))
       
       ; Get the variable ID.
       varID = NCDF_VarID(fileID, theVariable)

       ; Get information about the variable.
       varInfo = NCDF_VarInq(fileID, varID)

       ; Do we have to worry about zero dimensions?
       IF Ptr_Valid(self.zeroDimensionID) THEN BEGIN
       
           ; Is there a match between the dimension IDs and any zero dimension ID we 
           ; have stored?
           match = 0
           FOR m = 0, N_Elements(*self.zeroDimensionID)-1 DO BEGIN
               i = Where(varInfo.dim EQ (*self.zeroDimensionID)[m], count)
               IF count GT 0 THEN match = 1
           ENDFOR
           IF match GT 0 THEN BEGIN
               ok = Dialog_Message('This variable has a dimension of length zero and cannot be read.')
               NCDF_CLOSE, fileID
               success = 0
               RETURN, -1             
           ENDIF 
       ENDIF
       
       ; Read the variable.
       NCDF_VarGet, fileID, varID, data
       IF Size(data, /N_DIMENSIONS) GT 0 THEN data = REFORM(Temporary(data))
       IF StrUpCase(varInfo.datatype) EQ 'CHAR' THEN data = String(Temporary(data))
       varStruct = Create_Struct('data', Temporary(data))
             
       ; Add the variable attributes to the structure.
       FOR k=0,varInfo.natts-1 DO BEGIN
           attrName = NCDF_AttName(fileID, varID, k)
           NCDF_AttGet, fileID, varID, attrName, value
           IF Size(value, /TNAME) EQ 'BYTE' THEN value = String(value)
           attrName = IDL_ValidName(attrName, /CONVERT_ALL)
           IF Where(Tag_Names(varStruct) EQ StrUpCase(attrName)) NE -1 THEN CONTINUE
           varStruct = Create_Struct(varStruct, attrName, value)
       ENDFOR
       
       ; Add a dimensions field to the structure.
       varStruct = Create_Struct(varStruct, 'ndims', varInfo.Ndims, 'dims', varInfo.dim)
       
       ; Close the file, set status flag, return the data.
       NCDF_CLOSE, fileID
       success = 1
   
   ENDELSE
   
   RETURN, varstruct

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadVarPlusFromGUI, event
   ; This internal method sets up a dialog for obtaining information from the user
   ; about which variables to read, etc.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF

   ; Get some position information.
   Widget_Control, event.top, TLB_GET_OFFSET=offsets

   ; We want a modal pop-up dialog widget.
   tlb = Widget_Base(GROUP_LEADER=event.top, XOFFSET=offsets[0]+50, YOFFSET=offsets[1]+50, $
      COLUMN=1, BASE_ALIGN_CENTER=1, /FLOATING, UVALUE=self, /MODAL)
   row = Widget_Base(tlb, ROW=2, /GRID_LAYOUT, FRAME=1)
   label = Widget_Label(row, Value='Variable to Read: ')
   theList = [(*self.theVariables).name]
   self.varpluslistID = Widget_Droplist(row, Value=[(*self.theVariables).name], $
      UVALUE=[(*self.theVariables).name], SCR_XSIZE=250, UNAME='VARIABLESPLUS')
   label = Widget_Label(row, Value='Variable Name: ')
   thisVarname = IDL_ValidName((*self.theVariables)[0].name + '_struct', /CONVERT_ALL)
   self.varplusnameID = Widget_Text(row, Value=thisVarname, /Editable, SCR_XSIZE=250)
   b = Widget_Base(tlb, ROW=1, XPAD=0, YPAD=0, /NONEXCLUSIVE)
   
   okToAppend = 1
   IF StrPos(cgRootName(self.filename), '.') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, '-') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, ' ') NE -1 THEN okToAppend = 0
   IF okToAppend THEN self.appendNameID = Widget_Button(b, Value='Append Filename to Variable Name', UVALUE='APPEND_FILENAME')
   buttonrow = Widget_Base(tlb, ROW=1)
   button = Widget_Button(buttonrow, Value='Read Variable and Leave', UVALUE='READ_VARPLUS_AND_LEAVE')
   button = Widget_Button(buttonrow, Value='Read Variable and Stay', UVALUE='READ_VARPLUS_AND_STAY')
   button = Widget_Button(buttonrow, Value='Quit', UVALUE='QUIT_READ_VARPLUS_GUI')
   
   ; If there is a tree selection, see if this corresponds to a variable in the list.
   ; If so, set this variable in the droplist widget.
   theSelection = Widget_Info(self.theTree, /TREE_SELECT)
   Widget_Control, theSelection, Get_Value=varName
   index = Where(theList EQ varName, count)
   IF count GT 0 THEN BEGIN
      Widget_Control, self.varpluslistID, SET_DROPLIST_SELECT=index
      Widget_Control, self.varplusnameID, Set_Value=IDL_ValidName(theList[index], /CONVERT_ALL)
   ENDIF
   
   ; Get it going...
   Widget_Control, tlb, /REALIZE
   XMANAGER, 'read_and_leave', tlb, EVENT_HANDLER='NCDF_DATA_WIDGET_EVENTS', /NO_BLOCK
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadVarPlusFromGUI_Events, event
   ; This internal method processes events from the user dialogs.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   thisEvent = Tag_Names(event, /STRUCTURE_NAME)
   CASE thisEvent OF
   
      'WIDGET_BUTTON': BEGIN
      
         Widget_Control, event.id, Get_UValue=buttonValue
         CASE buttonValue OF
         
            'READ_VARPLUS_AND_STAY': BEGIN
            
               ; Get the variable name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
               Widget_Control, self.varPlusNameID, Get_Value=varName
               varName = (addName) ? cgRootName(self.filename) + '_' + varName[0] : varName[0]
               thisVarName = IDL_ValidName(varName, /CONVERT_ALL)
               IF thisVarName NE varName THEN BEGIN
                  Widget_Control, self.varPlusNameID, Set_Value=IDL_ValidName(thisVarName, /CONVERT_ALL)       
                  varName = thisVarName
               ENDIF
               IF varName EQ "" THEN Message, 'Must have a non-null variable name to create a variable.'
               
               ; Which variable do you want to read?
               Widget_Control, self.varpluslistID, Get_UValue=theList
               index = Widget_Info(self.varpluslistID, /DROPLIST_SELECT)
               theVariable = theList[index]
               theData = self -> ReadVariableWithAttr(theVariable, Success=success)
               IF success EQ 0 THEN RETURN
               
               ; Create the variable at the main IDL level.
               (Scope_VarFetch(varName, LEVEL=1, /ENTER)) = theData
               Print, 'A structure variable named "' + varName + '" has been created at the main IDL level.'
               
               ; Go to the next variable on the list
               IF index EQ (N_Elements(theList)-1) THEN index = 0 ELSE index = index + 1
               Widget_Control, self.varpluslistID, SET_DROPLIST_SELECT=index
               Widget_Control, self.varplusnameID, $
                  Set_Value=IDL_ValidName(theList[index] + '_struct', /CONVERT_ALL)
               END
               
            'READ_VARPLUS_AND_LEAVE': BEGIN
               
               ; Get the variable name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
               Widget_Control, self.varplusNameID, Get_Value=varName
               varName = (addName) ? cgRootName(self.filename) + '_' + varName[0] : varName[0]
               IF varName EQ "" THEN Message, 'Must have a non-null variable name to create a variable.'
               
               ; Which variable do you want to read?
               Widget_Control, self.varpluslistID, Get_UValue=theList
               index = Widget_Info(self.varpluslistID, /DROPLIST_SELECT)
               theVariable = theList[index]
               theData = self -> ReadVariableWithAttr(theVariable, Success=success)
               IF success EQ 0 THEN RETURN
               
               ; Create the variable at the main IDL level.
               (Scope_VarFetch(varName, LEVEL=1, /ENTER)) = theData
               Print, 'A structure variable named "' + varName + '" has been created at the main IDL level.'
               
               Widget_Control, event.top, /DESTROY
               END
               
            'QUIT_READ_VARPLUS_GUI': Widget_Control, event.top, /DESTROY
            
         ENDCASE
      
         END
         
      'WIDGET_DROPLIST': BEGIN
         ; The name of the variable to write has to be changed when the droplist value changes.
         Widget_Control, event.id, Get_UVALUE=list
         Widget_Control, self.varPlusNameID, Set_Value=IDL_ValidName(list[event.index], /CONVERT_ALL)
         END
   
      'WIDGET_TEXT': ; Nothing to do here. We just want to read the value. Don't what it is.
      
   ENDCASE
END ;---------------------------------------------------------------------------------------------




PRO NCDF_DATA::SelectionInTree, event
   ; This internal method processes events from the tree widget.
   
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF

   ; Create variable for better formatting.
   tab = '   '
   
   ; What to do depends on the node name of the tree selection.
   nodeName = Widget_Info(event.id, /UNAME)
   CASE nodeName OF
   
      'FOLDER': Widget_Control, self.textDisplay, Set_Value=""
               
      'GLOBAL ATTRIBUTE': BEGIN
         si = 0
         Widget_Control, event.id, Get_Value=name
         gattr = *self.theAttributes
         i = Where(gattr.name EQ name, count)
         IF count GT 0 THEN BEGIN
             text = StrArr(3)
             thisAttr = (*self.theAttributes)[i]
             IF thisAttr.dataType EQ 'STRING' OR thisAttr.dataType EQ 'CHAR' THEN BEGIN
                  str = TextLineFormat(*thisAttr.value, LENGTH=80)
                  lines = N_Elements(str)
                  text = [text, StrArr(2 + lines)]
                   text[si] = StrUpCase(thisAttr.name) + ':'
                   text[si+1:si+1+(lines-1)] = tab + str
                   si = si + 2 + lines
             ENDIF ELSE BEGIN
                   var = *thisAttr.value
                   IF N_Elements(var) EQ 0 THEN var = var[0]
                   Help, var, OUTPUT=helptext, /STRUCTURE
                   text = [text, StrArr(N_Elements(helptext) + 2)]
                   text[si] = StrUpCase(thisAttr.name) + ':'
                   aString = StrMid(helptext, 3)
                   parts = StrSplit(aString, '=', /Extract)
                   lastPart = StrCompress(parts[1], /REMOVE_ALL)
                   IF StrUpCase(lastPart) EQ 'ARRAY[2]' THEN BEGIN
                        IF Size(var, /TNAME) EQ 'BYTE' THEN BEGIN
                            lastpart = '[' + StrTrim(Fix(var[0]),2) + ', ' + StrTrim(Fix(var[1]),2) + ']'                    
                        ENDIF ELSE BEGIN
                            lastpart = '[' + StrTrim(var[0],2) + ', ' + StrTrim(var[1],2) + ']'
                        ENDELSE
                   ENDIF
                   aString = '   ' + StrCompress(parts[0],/REMOVE_ALL) + ' = ' + lastPart
                   text[si+1] = aString
                   si = si + N_Elements(helptext) + 2
             ENDELSE
             Widget_Control, self.textDisplay, Set_Value=text
         ENDIF ELSE Message, 'Cannot find global attribute ' + name
         END
                  
     'DIMENSION': BEGIN
          si = 0
          Widget_Control, event.id, Get_Value=name
          dims = *self.theDimensions
          i = Where(dims.name EQ name, count)
          IF count GT 0 THEN BEGIN
             text = StrArr(3)
             thisDim = (*self.theDimensions)[i]
             text[si] = StrUpCase(thisDim.name) + ':'
             text[si+1] = tab + StrTrim(thisDim.value,2)
          ENDIF
          Widget_Control, self.textDisplay, Set_Value=text
          END
                   
     'VARIABLE': BEGIN
           Widget_Control, event.id, Get_Value=name
           vars = *self.theVariables 
           IF N_Elements(vars) EQ 0 THEN vars = vars[0]
           i = Where(vars.name EQ name, count)
           FOR k=0,count-1 DO BEGIN
               thisVar = vars[i[k]]
               text = StrArr(6)
               text[0] = tab + 'NAME: ' + name
               text[1] = tab + 'DATATYPE: ' + thisVar.datatype
               n = StrTrim(N_Elements(*thisVar.datasize),2)
               f = ' (' + n + '(I0, :, ", "))'
               d = String(*thisVar.datasize, FORMAT=f)
               text[2] = tab + 'N_DIMENSIONS:  ' + StrTrim(N_Elements(*thisVar.datasize),2)
               text[3] = tab + 'DIMENSIONS:  [' + d + ']'
               IF (self.no_read_on_parse EQ 0) THEN BEGIN
                    IF  thisVar.datatype NE 'CHAR' THEN BEGIN 
                        text[4] = tab + 'MIN VALUE:  ' + StrTrim(thisVar.minValue,2)
                        text[5] = tab + 'MAX VALUE:  ' + StrTrim(thisVar.maxValue,2)
                    ENDIF
               ENDIF
                      
               Widget_Control, self.textDisplay, Set_Value=text
           ENDFOR    
           END
                  
      'FILENAME': BEGIN
             text = StrArr(3)
             text[0] = 'FILENAME:'
             text[1] = tab + self.filename
             Widget_Control, self.textDisplay, Set_Value=text
             END
                  
      'DIRECTORY': BEGIN
                  text = StrArr(3)
                  text[0] = 'DIRECTORY:'
                  text[1] = tab + self.directory
                  Widget_Control, self.textDisplay, Set_Value=text
                  END
                  
       'SUMMARY': BEGIN
         text = StrArr(3)
         text[0] = 'DIRECTORY:'
         text[1] = tab + self.directory
         si = 3
         
         text = [text, strArr(3)]
         text[3] = 'FILENAME:'
         text[4] = tab + self.filename
         si = si + 3
         
         IF Ptr_Valid(self.theAttributes) THEN BEGIN
            attr = *self.theAttributes
            FOR j=0,N_Elements(attr)-1 DO BEGIN
               thisAttr = attr[j]
               IF thisAttr.dataType EQ 'STRING' OR thisAttr.dataType EQ 'CHAR' THEN BEGIN
                  str = TextLineFormat(*thisAttr.value, LENGTH=80)
                  lines = N_Elements(str)
                  text = [text, StrArr(2 + lines)]
                  text[si] = StrUpCase(thisAttr.name) + ':'
                  text[si+1:si+1+(lines-1)] = tab + str
                  si = si + 2 + lines
               ENDIF ELSE BEGIN
                var = *thisAttr.value
                Help, var, OUTPUT=helptext, /STRUCTURE
                text = [text, StrArr(N_Elements(helptext) + 2)]
                text[si] = StrUpCase(thisAttr.name) + ':'
                aString = StrMid(helptext, 3)
                parts = StrSplit(aString, '=', /Extract)
                lastPart = StrCompress(parts[1], /REMOVE_ALL)
                IF StrUpCase(lastPart) EQ 'ARRAY[2]' THEN BEGIN
                    IF Size(var, /TNAME) EQ 'BYTE' THEN BEGIN
                        lastpart = '[' + StrTrim(Fix(var[0]),2) + ', ' + StrTrim(Fix(var[1]),2) + ']'                    
                    ENDIF ELSE BEGIN
                        lastpart = '[' + StrTrim(var[0],2) + ', ' + StrTrim(var[1],2) + ']'
                    ENDELSE
                ENDIF
                aString = '   ' + StrCompress(parts[0],/REMOVE_ALL) + ' = ' + lastPart
                text[si+1] = aString
                si = si + N_Elements(helptext) + 2
            ENDELSE
            ENDFOR
         ENDIF
         
         Widget_Control, self.textDisplay, Set_Value=text
         END
         
      'VARIABLE ATTRIBUTE': BEGIN
         si = 0
         Widget_Control, event.id, Get_UValue=value
         varname = value[0]
         name = value[1]
         vars = *self.theVariables 
         i = Where(vars.name EQ varname, count)
         thisVar = vars[i]
         var_attributes = *thisVar.var_attributes
         i = Where(var_attributes.name EQ name, count)
         IF count GT 0 THEN BEGIN
            text = StrArr(3)
            thisAttr = var_attributes[i]
            IF thisAttr.dataType EQ 'STRING' OR thisAttr.dataType EQ 'CHAR' THEN BEGIN
                  str = TextLineFormat(*thisAttr.value, LENGTH=80)
                  lines = N_Elements(str)
                  text = [text, StrArr(2 + lines)]
                  text[si] = StrUpCase(thisAttr.name) + ':'
                  text[si+1:si+1+(lines-1)] = tab + str
                  si = si + 2 + lines
             ENDIF ELSE BEGIN
                var = *thisAttr.value
                Help, var, OUTPUT=helptext, /STRUCTURE
                text = [text, StrArr(N_Elements(helptext) + 2)]
                text[si] = StrUpCase(thisAttr.name) + ':'
                aString = StrMid(helptext, 3)
                IF Size(var, /TNAME) NE 'STRUCT' THEN BEGIN
                    parts = StrSplit(aString, '=', /Extract)
                    lastPart = StrCompress(parts[1], /REMOVE_ALL)
                    IF StrUpCase(lastPart) EQ 'ARRAY[2]' THEN BEGIN
                        IF Size(var, /TNAME) EQ 'BYTE' THEN BEGIN
                            lastpart = '[' + StrTrim(Fix(var[0]),2) + ', ' + StrTrim(Fix(var[1]),2) + ']'                    
                        ENDIF ELSE BEGIN
                            lastpart = '[' + StrTrim(var[0],2) + ', ' + StrTrim(var[1],2) + ']'
                        ENDELSE
                    ENDIF
                    aString = '   ' + StrCompress(parts[0],/REMOVE_ALL) + ' = ' + lastPart
                    text[si+1] = aString
                    si = si + N_Elements(helptext) + 2
                ENDIF ELSE BEGIN
                  text[si + 1] = '      ' + 'Structure Variable:'
                  FOR kk=2,N_Elements(aString) DO BEGIN
                    text[si + kk] = '      ' + aString[kk-1]
                  ENDFOR
                ENDELSE
            ENDELSE
            Widget_Control, self.textDisplay, Set_Value=text
         ENDIF ELSE Message, 'Cannot find global attribute ' + name
         END
         
         ELSE: Message, 'Unexpected event from ' + nodename + '. Please investigate.'
         
   ENDCASE 

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::CLEANUP

   ; This is the main cleanup routine for the object. Delete all created pointers.
   self -> CleanParsedStructures
   
   Ptr_Free, self.theAttributes
   Ptr_Free, self.theDimensions
   Ptr_Free, self.theCalibration
   Ptr_Free, self.theSwaths
   Ptr_Free, self.theVariables
   Ptr_Free, self.zeroDimensionID

END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::INIT, filename, $
   BROWSE=browse, $
   DESTROY_FROM_BROWSER=destroy_from_browser, $
   EXTENSION=extension, $
   NO_READ_ON_PARSE=no_read_on_parse, $
   NO_NEW_FILE=no_new_file

   ; Error handling. Return 0 if can't finish.
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN, 0
   ENDIF

   ; Check parameters.
   IF N_Elements(filename) NE 0 THEN BEGIN
      IF File_Test(filename, /READ) EQ 0 THEN Message, 'Specified file does not exist or is not readable.'
      basename = File_BaseName(filename)
      directory = File_DirName(filename, /MARK_DIRECTORY)
      IF directory EQ '.\' OR directory EQ './' THEN CD, Current=directory
      self.filename = basename
      self.directory = directory
   ENDIF
   IF N_Elements(extension) EQ 0 THEN extension = '*.nc;*.ncd;*.ncdf;*.hdf'

   ; Set other object properties
   self.destroy_from_browser = Keyword_Set(destroy_from_browser)
   self.extension = extension
   self.no_read_on_parse = Keyword_Set(no_read_on_parse)

   ; Browse now?
   success = 1
   IF Keyword_Set(browse) THEN self -> Browse, SUCCESS=success
   IF success EQ 0 THEN BEGIN
       Obj_Destroy, self
       RETURN, 0
   ENDIF
   
   ; Determine if this is a netCDF or HDF file.
   self.isHDF = HDF_ISHDF(Filepath(ROOT_DIR=self.directory, self.filename))
   
   RETURN, 1

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA_ATTRIBUTE__DEFINE

   struct = { NCDF_DATA_ATTRIBUTE, $
              attrtype: "", $
              datatype: "", $
              length: 0L, $
              name: "", $
              value: Ptr_New() }
              
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA_DIMENSION__DEFINE

   struct = { NCDF_DATA_DIMENSION, $
              name: "", $
              value: "" }       ; Length or UNLIMITED.

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA_VARIABLE__DEFINE

   struct = { NCDF_DATA_VARIABLE, $
              datasize: Ptr_New(), $
              datatype: "", $
              minValue: 0.0D, $
              maxValue: 0.0D, $
              name: "", $
              var_attributes: Ptr_New(), $
              calibration: Ptr_New(), $
              value: Ptr_New() }

END;---------------------------------------------------------------------------------------------



PRO NCDF_DATA_SWATH__DEFINE

   struct = { NCDF_DATA_SWATH, $
              name: "", $
              nattrs: 0L, $
              ndims: 0L, $
              ngeofields: 0L, $
              ndatafields: 0L, $
              nmaps: 0L, $
              nidxmaps: 0L, $
              attributes: Ptr_New(), $
              dimensions: Ptr_New(), $
              geofields: Ptr_New(), $
              datafields: Ptr_New(), $
              maps: Ptr_New(), $
              idxmaps: Ptr_New() }

END;---------------------------------------------------------------------------------------------



PRO NCDF_DATA_WIDGET_CLEANUP, tlb

   Widget_Control, tlb, GET_UVALUE=self
   IF self -> Destroy_From_Browser() THEN Obj_Destroy, self
   
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA_WIDGET_EVENTS, event

   Widget_Control, event.TOP, GET_UVALUE=self
   self -> EventHandler, event
   
END ;---------------------------------------------------------------------------------------------




PRO NCDF_DATA__DEFINE, class

   class = { NCDF_DATA,                $  ; The object class NCDF_DATA.
             appendNameID: 0L,         $  ; The button for appending filenames to variables.
             attributeID: 0L,          $  ; The widget containing the attribute list.
             attrNameID: 0L,           $  ; The widget containing the main-level attribute name.
             filename: "",             $  ; The filename of the netCDF or HDF file.
             destroy_from_browser: 0B, $  ; A flag to indicate the object is destroyed if brower is destroyed.
             directory: "",            $  ; The directory the file is located in.
             extension: '',            $  ; The file extension for FILTER keyword in DIALOG_PICKFILE.
             geoDisplay: {WIDGET_GEOMETRY}, $ ; Widget geometries for calculating resizeable windows.
             geoWindow: {WIDGET_GEOMETRY}, $
             geoButton: {WIDGET_GEOMETRY}, $
             geoTree: {WIDGET_GEOMETRY}, $
             hasBeenParsed: 0B,        $  ; A flag to indicate if the file has been parsed.
             no_read_on_parse: 0B,     $  ; A flag to indicate that the variables should not be read while parsing.
             isHDF: 0B,                $  ; A flag to indicate this is an HDF file instead of a netCDF file.
             minXSize: 0L,             $  ; Minimum X size of the Browser window.
             minYSize: 0L,             $  ; Minimum Y size of the Browser window.             
             textDisplay: 0L,          $  ; The widget where text information is displayed
             theAttributes: Ptr_New(), $  ; An array of global attribute structures.
             theDimensions: Ptr_New(), $  ; An array of dimension structures.
             theCalibration: Ptr_New(),$  ; An array of calibration structures for the HDF SD variable.
             zeroDimensionID: Ptr_New(), $ ; A pointer to the dimension IDs whose current size is 0.
             theSwaths: Ptr_New(), $      ; A pointer to an array of swath structures.
             theTree: 0L,              $  ; The tree widget ID.
             theVariables: Ptr_New(),  $  ; An array of variable structures.
             tlb: 0L,                  $  ; The TLB of the browser window.
             varpluslistID: 0L,        $  ; The list of variable plus attributes to be read.
             varplusnameID: 0L,        $  ; The widget containing the main-level variable name.
             variablelistID: 0L,       $  ; The list of variables available to be read.
             varnameID: 0L             $  ; The widget containing the main-level variable name.
           }

END ;---------------------------------------------------------------------------------------------
