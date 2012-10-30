;+
; NAME:
;       NCDF_BROWSER
;
; PURPOSE:
;
;       This program is designed to make it easier to browse and read the 
;       data and metadata in netCDF and HDF files. The user can browse files, 
;       and read the data and metadata into main-level IDL variables. New netCDF 
;       and HDF files can be opened at any time. The user interacts with the 
;       program via a browser window (GUI). This program is a wrapper for the
;       NCDF_DATA object (ncdf_data__define.pro), which must also be downloaded.
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
;       IDL> NCDF_Browser, filename
;
; Arguments:
;
;       filename: The name of a netCDF and HDF file to open and browse.
;
; KEYWORD PARAMETERS:
;       
;       EXTENSION: In general, netCDF and HDF files use *.nc, *.ncf, *.ncdf and *.hdf file extensions to
;                  identify themselves as netCDF and HDF files. Some users have their own file extensions.
;                  You can use this keyword to identify the file extension you wish to use. If
;                  set here, it will be used as the file filter in place of the normal file 
;                  extensions in DIALOG_PICKFILE.
;
;                      obj = ('NCDF_DATA', file, EXTENSION='*.bin')
;                      
;       NO_NEW_FILE: If this keyword is set, then the button that allows a new file to be open
;                  on the browser is not created.
;
;       NO_READ_ON_PARSE: Normally, when a file is opened it is parsed for information.
;                  One piece of information is the minimum and maximum values of the variables.
;                  This requires actually reading the variables. This can slow things down 
;                  considerably is the variable is large. Setting this keyword will suppress 
;                  the reading of the variables during the parsing of the data file, with the
;                  result that no minimum or maximum values will be reported.
;                  
;       TITLE:     Set this keyword to a string that is on the title bar of the browser.
;       
;       XOFFSET:   Set this keyword to the X offset in pixels of the top-left corner of the browser.
;
;       YOFFSET:   Set this keyword to the Y offset in pixels of the top-left corner of the browser.
;
; NOTES:
;       
;       This program is only a (useful) front-end for a more flexible
;       object program of class NCDF_DATA. In this front end, the NCDF_DATA
;       object is created and then destroyed when the GUI is destroyed.
;       The NCDF_DATA object can be used to read netCDF data in a non-interactive
;       way, if you prefer not to use a GUI to interact with the data file.
;
; REQUIRES:
;
;        The following programs are required from the Coyote Library.
;
;              http://www.idlcoyote.com/netcdf_data__define.pro
;              http://www.idlcoyote.com/error_message.pro
;              http://www.idlcoyote.com/undefine.pro
;              http://www.idlcoyote.com/textbox.pro
;              http://www.idlcoyote.com/cgRootName.pro
;              http://www.idlcoyote.com/textlineformat.pro
;
; MODIFICATION HISTORY:
;       Written by:  David W. Fanning, 03 Feb 2008. Used ideas from many
;           people, including Chris Torrence, Ken Bowman, Liam Gumely, 
;           Andrew Slater, and Paul van Delst.
;       Added Extension keyword. DWF. 04 Feb 2008.
;       Added error handling and protection for NCDF variables that have a dimension of length zero. 22 April 2009. DWF.
;       Added NO_READ_ON_PARSE keyword. 22 April 2009. DWF.
;       Now convert NCDF CHAR type variables to strings on output. 22 April 2009. DWF
;       Made the default value of NO_READ_ON_PARSE set to 1. 25 June 2009. DWF.
;       Added NO_NEW_FILE keyword to suppress the Open File button. 3 February 2010. DWF.
;       Added TITLE, XOFFSET, and YOFFSET keywords. 5 February 2010. DWF.
;       Fixed a problem with memory leakage when the input file cannot be read. 1 May 2010. DWF.
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
PRO NCDF_BROWSER, filename, $
    EXTENSION=extension, $
    NO_NEW_FILE=no_new_file, $
    NO_READ_ON_PARSE=no_read_on_parse, $
    TITLE=title, $
    XOFFSET=xoffset, $
    YOFFSET=yoffset

   ; Error handling. 
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   IF N_Elements(extension) EQ 0 THEN extension = '*.nc;*.ncd;*.ncdf;*.hdf'
   IF N_Elements(no_read_on_parse) EQ 0 THEN no_read_on_parse = 1

   ; Need a filename?
   IF N_Elements(filename) EQ 0 THEN BEGIN
      filename = cgPickfile(/READ, TITLE='Select a File to Open', $
         FILTER=extension)
    ENDIF
    IF filename EQ "" THEN RETURN
    
   ; Create an nCDF_DATA browse object.
   ncdfObj = Obj_New('NCDF_DATA', filename, /Destroy_From_Browser, EXTENSION=extension, $
        NO_READ_ON_PARSE=no_read_on_parse)
   IF Obj_Valid(ncdfObj) THEN BEGIN
        ncdfObj -> Browse, NO_NEW_FILE=Keyword_Set(no_new_file), $
            XOFFSET=xoffset, YOFFSET=yoffset, SUCCESS=success, TITLE=title
        IF ~success THEN Obj_Destroy, ncdfObj
   ENDIF
   
END
