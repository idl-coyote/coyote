PRO HDFWRITE, filename

  ; Create randomly-distributed data.
  
seed = -1L
x = RANDOMU(seed, 40)
y = RANDOMU(seed, 40)
distribution = SHIFT(DIST(40,40), 25, 15)
distribution = EXP(-(distribution/15)^2)

lat = x * (24./1.0) + 24
lon = y * 50.0/1.0 - 122
temp = distribution(x*40, y*40) * 273

   ; Select the name of a new file and open it.

IF N_ELEMENTS(filename) EQ 0 THEN filename = Pickfile(/Write)
IF filename EQ '' THEN RETURN
PRINT, ''
PRINT, 'Opening HDF file "' + filename + '" ...'

   ; If there is a problem opening the file, catch the error.
   
CATCH, error
IF error NE 0 THEN BEGIN
   PRINT, ''
   PRINT, 'Unable to obtain an SDS file ID.'
   PRINT, 'This file may already be open by an HDF routine.'
   PRINT, 'Returning...'
   PRINT, ''
   RETURN
ENDIF
fileID = HDF_SD_START(filename, /CREATE)
CATCH, /CANCEL

   ; Write some attributes into the file.

PRINT, 'Writing file attributes ...'   
version = 'MacOS 4.0.1b'
date = "Jan 1, 1997"
experiment = "Experiment 25A36M"
name = 'David Fanning'
email = 'david@idlcoyote.com'

HDF_SD_ATTRSET, fileID, 'VERSION', version
HDF_SD_ATTRSET, fileID, 'DATE', date
HDF_SD_ATTRSET, fileID, 'EXPERIMENT', experiment
HDF_SD_ATTRSET, fileID, 'NAME', name
HDF_SD_ATTRSET, fileID, 'EMAIL ADDRESS', email

   ; Create the SDS data sets for the raw data. 
   
PRINT, 'Writing raw data ...'
latsdsID = HDF_SD_CREATE(fileID, "Raw Latitude", [40L], /FLOAT)
lonsdsID = HDF_SD_CREATE(fileID, "Raw Longitude", [40L], /FLOAT)
tempsdsID = HDF_SD_CREATE(fileID, "Raw Temperature", [40L], /FLOAT)

   ; Write the raw data.
   
HDF_SD_ADDDATA, latsdsID, lat
HDF_SD_ADDDATA, lonsdsID, lon
HDF_SD_ADDDATA, tempsdsID, temp

   ; Terminate access to the raw data SDSs.

HDF_SD_ENDACCESS, latsdsID
HDF_SD_ENDACCESS, lonsdsID
HDF_SD_ENDACCESS, tempsdsID

   ; Grid the irregularly spaced, raw data.
   
latMax = 49.0
latMin = 24.0
lonMax = -67.0
lonMin = -125.0
mapBounds = [lonMin, latMin, lonMax, latMax]
mapSpacing = [0.5, 0.25]
 
TRIANGULATE, lon, lat, FVALUE=temp, SPHERE=triangles, /DEGREES
gridData = TRIGRID(temp, SPHERE=triangles, /DEGREES, /EXTRAPOLATE, $
  mapSpacing, mapBounds)

   ; Calculate xlon and ylat vectors corresponding to gridded data.
   
s = SIZE(gridData)
gridlon = FINDGEN(s(1))*((lonMax - lonMin)/(s(1)-1)) + lonMin
gridlat = FINDGEN(s(2))*((latMax - latMin)/(s(2)-1)) + latMin

   ; Now store the gridded data.

PRINT, 'Writing gridded data ...'
gridID = HDF_SD_CREATE(fileID, "Gridded Data", [s(1), s(2)], /FLOAT)
HDF_SD_ADDDATA, gridID, gridData

   ; Store local SDS attributes with the gridded data.
   
method = 'Delauney Triangulation'
routines = 'TRIANGULATE, TRIGRID'

PRINT, 'Writing gridded data set attributes ...'
HDF_SD_AttrSet, gridID, 'METHOD', method
HDF_SD_AttrSet, gridID, 'IDL ROUTINES', routines
HDF_SD_AttrSet, gridID, 'GRID SPACING', mapSpacing
HDF_SD_AttrSet, gridID, 'MAP BOUNDRIES', mapBounds

   ; Store the scales for the two dimensions of the gridded data.

PRINT, 'Writing dimension data ...'
longitudeDimID = HDF_SD_DIMGETID(gridID, 0)
HDF_SD_DIMSET, longitudeDimID, $
   LABEL='Longitude', $
   NAME='Longitude Dimension', $
   SCALE=gridlon, $
   UNIT='Degrees'
   
latitudeDimID = HDF_SD_DIMGETID(gridID, 1)
HDF_SD_DIMSET, latitudeDimID, $
   LABEL='Latitude', $
   NAME='Latitude Dimension', $
   SCALE=gridlat, $
   UNIT='Degrees'
   
   ; Load a color palette you will use to display the data.
   
thisDevice = !D.NAME
SET_PLOT, 'Z'
LOADCT, 5, /SILENT
TVLCT, r, g, b, /GET
palette = BYTARR(3,256)
palette(0,*) = r
palette(1,*) = g
palette(2,*) = b
SET_PLOT, thisDevice

   ; Store the color palette along with the data.

PRINT, 'Writing color palette ...'
HDF_DFP_ADDPAL, filename, palette

   ; Close everything up properly.
   
HDF_SD_ENDACCESS, gridID
HDF_SD_END, fileID
PRINT, 'Write Operation Completed'
PRINT, ''
END
