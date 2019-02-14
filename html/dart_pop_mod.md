[]{#TOP}

MODULE dart\_pop\_mod (POP)
===========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../documentation/images/ | Index](../../documentation/index. |
| Dartboard7.png){height="70"}      | html)\                            |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [INTERFACES](#Interface) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[PRIVATE COMPONENTS](#PrivateComponents) / [TERMS OF USE](#Legalese)

Overview
--------

*dart\_pop\_mod* provides a consistent collection of routines that are
useful for multiple programs e.g. *dart\_to\_pop*, *pop\_to\_dart*, etc.

[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

There are no namelists unique to this module. It is necessary for this
module to read some of the POP namelists, and so they are declared in
this module. In one instance, DART will read the *time\_manager\_nml*
namelist and **write** an updated version to control the length of the
integration of POP. All other information is simply read from the
namelists and is used in the same context as POP itself. The POP
documentation should be consulted. **Only the variables of interest to
DART are described in this document.**\
\
All namelists are read from a file named *pop\_in*.

[]{#time_manager_nml}

<div class="namelist">

    namelist /time_manager_nml/  allow_leapyear, stop_count, stop_option

</div>

<div class="indent1">

*dart\_to\_pop* controls the model advance of LANL/POP by creating a
*&time\_manager\_nml* in *pop\_in.DART* **IFF** the DART state being
converted has the 'advance\_to\_time' record. The *pop\_in.DART* must be
concatenated with the other namelists needed by POP into a file called
*pop\_in* . We have chosen to store the other namelists (which contain
static information) in a file called *pop\_in.part2*. Initially, the
*time\_manager\_nml* is stored in a companion file called
*pop\_in.part1* and the two files are concatenated into the expected
*pop\_in* - then, during the course of an assimilation experiment, DART
keeps writing out a new *time\_manager\_nml* with new integration
information - which gets appended with the static information in
*pop\_in.part2* 

Contents
Type
Description
allow\_leapyear   
logical
DART ignores the setting of this parameter. All observations must use a
Gregorian calendar. There are pathological cases, but if you are doing
data assimilation, just use the Gregorian calendar.
*\[default: .true.\]*
stop\_count
integer
the number of model advance steps to take. *\[default: 1\]*
stop\_option
character(len=64)   
The units for the number of model advance steps (*stop\_count*) to take.
*\[default: 'ndays'\]*

</div>

\
[]{#io_nml}

<div class="namelist">

    namelist /io_nml/  luse_pointer_files, pointer_filename

</div>

<div class="indent1">

Contents
Type
Description
luse\_pointer\_files
logical
switch to indicate the use of pointer files or not. If *.true.*, a
pointer file is used to contain the name of the restart file to be used.
DART requires this to be *.true*. *\[default: .true.\]*
pointer\_filename
character(len=100)
The name of the pointer file. All of the DART scripts presume and
require the use of the default. Each ensmeble member gets its own
pointer file. *\[default: rpointer.ocn.\[1-N\].restart\]*

</div>

\
[]{#restart_nml}\

<div class="namelist">

    namelist /restart_nml/  restart_freq_opt, restart_freq

</div>

<div class="indent1">

Contents
Type
Description
luse\_pointer\_files   
logical
switch to indicate the use of pointer files or not. If *.true.*, a
pointer file is used to contain the name of the restart file to be used.
DART requires this to be *.true*. *\[default: .true.\]*
pointer\_filename
character(len=100)   
The name of the pointer file. All of the DART scripts presume and
require the use of the default. Each ensmeble member gets its own
pointer file. *\[default: rpointer.ocn.\[1-N\].restart\]*

</div>

\
[]{#init_ts_nml}\

<div class="namelist">

    namelist /init_ts_nml/  init_ts_option, init_ts_file, init_ts_file_fmt

</div>

<div class="indent1">

The *dart\_pop\_mod:initialize\_module()* routine reads *pop\_in* .
There are several code stubs for future use that may allow for a more
fully-supported POP namelist implementation. This namelist is one of
them. Until further notice, the *init\_ts\_nml* is completely ignored by
DART.

Contents
Type
Description
init\_ts\_option
character(len=64)
NOT USED by DART. All T,S information comes from a netCDF restart file
named *pop.r.nc* *\[default: 'restart'\]*
init\_ts\_file
character(len=100)   
NOT USED by DART. All T,S information comes from *pop.r.nc*
*\[default: 'pop.r'\]*
init\_ts\_file\_fmt   
character(len=64)
NOT USED by DART. The file format is *'nc'* *\[default: 'nc'\]*

</div>

\
[]{#domain_nml}\

<div class="namelist">

    namelist /domain_nml/  ew_boundary_type

</div>

<div class="indent1">

DART needs to know if the East-West domain is cyclic for spatial
interpolations. Presently, DART has only been tested for the dipole
grid, which is cyclic E-W and closed N-S.

Contents
Type
Description
ew\_boundary\_type   
character(len=64)   
switch to indicate whether the East-West domain is cyclic or not.
DART/POP has not been tested in a regional configuration, so DART
requires this to be *'cyclic'*. *\[default: 'cyclic'\]*

</div>

\
[]{#grid_nml}\

<div class="namelist">

    namelist /grid_nml/  horiz_grid_opt,  vert_grid_opt,  topography_opt, &
                                   horiz_grid_file, vert_grid_file, topography_file

</div>

<div class="indent1">

The POP grid information comes in several files: horizontal grid
lat/lons in one, the vertical grid spacing in another, and the
topography (lowest valid vertical level) in a third.\
\
Here is what we can get from the (binary) horizontal grid file:

    real(r8), dimension(:,:) :: ULAT,  &! latitude  (radians) of U points
    real(r8), dimension(:,:) :: ULON,  &! longitude (radians) of U points
    real(r8), dimension(:,:) :: HTN ,  &! length (cm) of north edge of T box
    real(r8), dimension(:,:) :: HTE ,  &! length (cm) of east  edge of T box
    real(r8), dimension(:,:) :: HUS ,  &! length (cm) of south edge of U box
    real(r8), dimension(:,:) :: HUW ,  &! length (cm) of west  edge of U box
    real(r8), dimension(:,:) :: ANGLE  &! angle

The vertical grid file is ascii, with 3 columns/line:

    cell thickness(in cm)   cell center(in m)   cell bottom(in m)

Here is what we can get from the topography file:

    integer, dimension(:,:), :: KMT    &! k index of deepest grid cell on T grid

These must be derived or come from someplace else ...

    KMU               k index of deepest grid cell on U grid
    HT                real(r8) value of deepest valid T depth (in cm)
    HU                real(r8) value of deepest valid U depth (in cm)

Contents
Type
Description
horiz\_grid\_opt, vert\_grid\_opt, topography\_opt
character(len=64)
switch to indicate whether or not the grids will come from an external
file or not. DART requires ALL of these to be *'file'*.
*\[default: 'file'\]*
horiz\_grid\_file
character(len=100)
The name of the binary file containing the values for the horizontal
grid. The **dimensions** of the grid are read from *pop.r.nc*. It would
have been nice to include the actual grid information in the netCDF
files. *\[default: 'horiz\_grid.gx3v5.r8ieee.le'\]*
vert\_grid\_file
character(len=100)
The name of the ASCII file containing the values for the vertical grid.
The file must contain three columns of data pertaining to the cell
thickness (in cm), the cell center (in meters), and the cell bottom (in
meters). Again, it would have been nice to include the vertical grid
information in the netCDF files. *\[default: 'vert\_grid.gx3v5'\]*
topography\_grid\_file
character(len=100)
The name of the binary file containing the values for the topography
information. The **dimensions** of the grid are read from *pop.r.nc*.
*\[default: 'topography.gx3v5.r8ieee.le'\]*

</div>

\
[]{#OtherModulesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    types_mod
    time_manager_mod
    utilities_mod
    typesizes
    netcdf

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

Only a select number of interfaces used are discussed here. Each module
has its own discussion of their routines.

### Interface Routines {#interface-routines .indent1}

  ------------------------------ ----------------------------------------------------------
  *use dart\_pop\_mod, only :*   [get\_pop\_calendar](#get_pop_calendar)
                                 [set\_model\_time\_step](#set_model_time_step)
                                 [get\_horiz\_grid\_dims](#get_horiz_grid_dims)
                                 [get\_vert\_grid\_dim](#get_vert_grid_dim)
                                 [read\_horiz\_grid](#read_horiz_grid)
                                 [read\_topography](#read_topography)
                                 [read\_vert\_grid](#read_vert_grid)
                                 [write\_pop\_namelist](#write_pop_namelist)
                                 [get\_pop\_restart\_filename](#get_pop_restart_filename)
  ------------------------------ ----------------------------------------------------------

### Required Interface Routines {#required-interface-routines .indent1}

[]{#get_pop_calendar}\

<div class="routine">

*call get\_pop\_calendar(calstring)*
    character(len=*), intent(out) :: calstring

</div>

<div class="indent1">

Returns a string containing the type of calendar in use.

  ------------- ---------------------------------------
  *calstring*   DART/POP uses a 'gregorian' calendar.
  ------------- ---------------------------------------

</div>

\
[]{#set_model_time_step}\

<div class="routine">

*poptimestep = set\_model\_time\_step()*
    type(time_type), intent(out) :: poptimestep

</div>

<div class="indent1">

*set\_model\_time\_step* returns the model time step that was set in the
[restart\_nml](#restart_nml)*restart\_freq*. This is the minimum amount
of time DART thinks the POP model can advance. Indirectly, this
specifies the minimum assimilation interval.

  --------------- -----------------------------------
  *poptimestep*   the minimum assimilation interval
  --------------- -----------------------------------

</div>

\
[]{#get_horiz_grid_dims}\

<div class="routine">

*call get\_horiz\_grid\_dims(Nx, Ny)*
    integer, intent(out) :: Nx, Ny

</div>

<div class="indent1">

*get\_horiz\_grid\_dims* reads *pop.r.nc* to determine the number of
longitudes and latitudes.

  --------- -------------------------------------------------------------------------------------------
  *Nx   *   the length of the 'i' dimension in the POP restart file. The number of longitudes in use.
  *Ny   *   the length of the 'j' dimension in the POP restart file. The number of latitudes in use.
  --------- -------------------------------------------------------------------------------------------

</div>

\
[]{#get_vert_grid_dim}\

<div class="routine">

*call get\_vert\_grid\_dim( Nz )*
    integer, intent(out) :: Nz

</div>

<div class="indent1">

*get\_vert\_grid\_dim* reads *pop.r.nc* to determine the number of
vertical levels in use.

  --------- ------------------------------------------------------------------------------------------------
  *Nz   *   the length of the 'k' dimension in the POP restart file. The number of vertical levels in use.
  --------- ------------------------------------------------------------------------------------------------

</div>

\
[]{#read_horiz_grid}\

<div class="routine">

*call read\_horiz\_grid(nx, ny, ULAT, ULON, TLAT, TLON)*
    integer,                    intent(in)  :: nx, ny
    real(r8), dimension(nx,ny), intent(out) :: ULAT, ULON, TLAT, TLON

</div>

<div class="indent1">

*read\_horiz\_grid* reads the direct access binary files containing the
POP grid information. **The first record is REQUIRED to be 'ULAT', the
second record is REQUIRED to be 'ULON'.**

  ----------- ----------------------------------------------------------------------------------------
  *nx   *     The number of longitudes in the grid.
  *ny   *     The number of latitudes in the grid.
  *ULAT   *   The matrix of latitudes for the UVEL and VVEL variables. Units are degrees \[-90,90\].
  *ULON   *   The matrix of longitudes for the UVEL and VVEL variables. Units are degrees. \[0,360\]
  *TLAT   *   The matrix of latitudes for the SALT and TEMP variables. Units are degrees \[-90,90\].
  *TLON   *   The matrix of longitudes for the SALT and TEMP variables. Units are degrees. \[0,360\]
  ----------- ----------------------------------------------------------------------------------------

</div>

\
[]{#read_topography}\

<div class="routine">

*call read\_topography(nx, ny, KMT, KMU)*
    integer,                   intent(in)  :: nx, ny
    integer, dimension(nx,ny), intent(out) :: KMT, KMU

</div>

<div class="indent1">

*read\_topography* reads the direct access binary files containing the
POP topography information. **The first record is REQUIRED to be
'KMT'.** 'KMU' is calculated from 'KMT'.

  ---------- -----------------------------------------------------------------------
  *nx   *    The number of longitudes in the grid.
  *ny   *    The number of latitudes in the grid.
  *KMT   *   The matrix containing the lowest valid depth index at grid centroids.
  *KMU   *   The matrix containing the lowest valid depth index at grid corners.
  ---------- -----------------------------------------------------------------------

</div>

\
[]{#read_vert_grid}\

<div class="routine">

*call read\_vert\_grid(nz, ZC, ZG)*
    integer,                 intent(in)  :: nz
    real(r8), dimension(nz), intent(out) :: ZC, ZG

</div>

<div class="indent1">

*read\_vert\_grid* reads the ASCII file containing the information about
the vertical levels. The file must contain three columns of data
pertaining to; 1) the cell thickness (in cm),\
2) the cell center (in meters),\
and 3) the cell bottom (in meters).

  --------- --------------------------------------------
  *nz   *   The number of vertical levels.
  *ZC   *   The depth (in meters) at the grid centers.
  *ZG   *   The depth (in meters) at the grid edges.
  --------- --------------------------------------------

</div>

\
[]{#write_pop_namelist}\

<div class="routine">

*call write\_pop\_namelist(model\_time, adv\_to\_time)*
    type(time_type), intent(in)  :: model_time
    type(time_type), intent(in)  :: adv_to_time

</div>

<div class="indent1">

*write\_pop\_namelist* writes the POP namelist *time\_manager\_nml* with
the information necessary to advance POP to the next assimilation time.
The namelist is written to a file called *pop\_in.DART*. Presently, DART
is configured to minimally advance POP for 86400 seconds - i.e. 1 day.
The forecast length (the difference between 'model\_time' and
'adv\_to\_time') must be an integer number of days with the current
setup. An error will result if it is not.

  -------------------- ----------------------------------------------
  *model\_time   *     The 'valid' time of the current model state.
  *adv\_to\_time   *   The time of the next assimilation.
  -------------------- ----------------------------------------------

</div>

\
[]{#get_pop_restart_filename}\

<div class="routine">

*call get\_pop\_restart\_filename( filename )*
    character(len=*), intent(out) :: filename

</div>

<div class="indent1">

*get\_pop\_restart\_filename* returns the filename containing the POP
restart information. At this point the filename is **hardwired** to
*pop.r.nc*, but may become more flexible in future versions. The
filename may be derived from the *restart\_nml* but is currently
ignored.

  --------------- -----------------------------------
  *filename   *   The name of the POP restart file.
  --------------- -----------------------------------

</div>

\
[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

  filename                                purpose
  --------------------------------------- ---------------------------------------------------------------
  pop\_in                                 to read the POP namelists
  pop.r.nc                                provides grid dimensions and 'valid\_time' of the model state
  *&grid\_nml* "horiz\_grid\_file"        contains the values of the horizontal grid
  *&grid\_nml* "vert\_grid\_file"         contains the number and values of the vertical levels
  *&grid\_nml* "topography\_grid\_file"   contains the indices of the wet/dry cells
  pop\_in.DART                            to control the integration of the POP model advance

\
[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

-   none

[]{#Errors}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

ERROR CODES and CONDITIONS
--------------------------

<div class="errors">

Routine
Message
Comment
initialize\_module
pop\_in:init\_ts\_file pop.r.nc not found'
The POP restart file MUST be called 'pop.r.nc'. Make a soft link if
necessary.
get\_horiz\_grid\_dims
unable to find either 'i' or 'nlon' in file pop.r.nc
The POP restart file must contain dimensions named either 'i' or 'nlon'.
get\_horiz\_grid\_dims
unable to find either 'j' or 'nlat' in file pop.r.nc
The POP restart file must contain dimensions named either 'j' or 'nlat'.
set\_model\_time\_step
restart\_freq\_opt must be nday
Pretty self-explanatory. The POP namelist must specify the forecast
length as a multiple of 'days'.
write\_pop\_namelist
adv\_to\_time has seconds == xxx must be zero'
DART is asking POP to advance to a time that is a fraction of a day
away. This should not be possible. Contact the DART developers.
write\_pop\_namelist
stop\_option must be "nday"
the POP *time\_manager\_nml:stop\_option* is not set to 'nday'. This is
required by DART.
read\_horiz\_grid
pop\_in:horiz\_grid\_file 'XYZ' not found
The horizontal grid filename specified in *pop\_ingrid\_nml* cannot be
found.
calc\_tpoints
pop\_in&domain\_nml:ew\_boundary\_type 'X' unknown
The *ew\_boundary\_type* must be 'cyclic' - until DART/POP gets tested
with non-cyclic domains.
read\_topography
pop\_in:topography\_file 'XYZ' not found
The topography file specified in *pop\_ingrid\_nml* cannot be found.
read\_vert\_grid
pop\_in:vert\_grid\_file 'XYZ' not found
The vertical grid file specified in *pop\_ingrid\_nml* cannot be found.
read\_vert\_grid
error reading depths, line 'X'
The vertical grid file is corrupt or does not have the expected three
pieces of information per line.

</div>

KNOWN BUGS
----------

There are no known bugs, but there sure is a lot of dependence on
assimilating on daily boundaries - and the pop.r.nc file.

[]{#FuturePlans}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FUTURE PLANS
------------

none at this time

[]{#PrivateComponents}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PRIVATE COMPONENTS
------------------

N/A

[]{#Legalese}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Terms of Use
------------

DART software - Copyright UCAR. This open source software is provided by
UCAR, "as is", without charge, subject to all terms of use at
<http://www.image.ucar.edu/DAReS/DART/DART_download>

  ------------------ -----------------------------
  Contact:           DART core group
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


