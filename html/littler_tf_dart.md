[]{#TOP}

PROGRAM *littler\_tf\_dart*
===========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[DATA SOURCES](#DataSources) / [PROGRAMS](#Programs) /
[MODULES](#Modules) / [NAMELIST](#Namelist) / [ERRORS](#Errors) /
[FUTURE PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
--------

Programs to convert littler data files into DART observation sequence
files, and vice versa. The capability of the program is limited to wind
and temperature from radiosondes.

The littler data files do not contain observation errors. The
observation errors are in a separate file called *obserr.txt*. The
littler file generated here has to be preprocessed by the program
*3dvar\_obs.exe* before beeing ingested in the WRF 3D-Var system.

[]{#Modules}

------------------------------------------------------------------------

MODULES USED
------------

    types_mod
    obs_sequence_mod
    obs_def_mod
    obs_kind_mod
    location/threed_sphere/location_mod
    time_manager_mod
    utilities_mod

MODULES INDIRECTLY USED
-----------------------

    assim_model_mod
    models/wrf/model_mod
    models/wrf/module_map_utils
    random_seq_mod

[]{#Namelist}\

------------------------------------------------------------------------

\

NAMELIST
--------

The program does not have its own namelist. However, an *input.nml* file
is required for the modules used by the program.

[]{#FilesUsed}

------------------------------------------------------------------------

FILES
-----

-   input namelist ; *input.nml*
-   Input - output observation files; *obs\_seq.out* and *little-r.dat*
-   Input - output littler observation error files ; *obserr.txt*

### File formats

If there are no observation error at a particular pressure level, the
default value of -1 is written in *obserr.txt*.

[]{#References}

------------------------------------------------------------------------

REFERENCES
----------

-   [3DVAR GROUP PAGE](http://www.mmm.ucar.edu/wrf/WG4/)

[]{#Errors}

------------------------------------------------------------------------

ERROR CODES and CONDITIONS
--------------------------

<div class="errors">

Routine
Message
Comment
littler\_tf\_dart
Did not get all obs
There is observation before dart time (0, 0) or beyond day 200000 in the
Gregorian calendar in the *obs\_seq.out* file.
littler\_tf\_dart
No vertical coordinate.
Only vertical pressure coordinate is supported.
littler\_tf\_dart
Error when reading or writing header of sounding.
Problem with littler file.
littler\_tf\_dart
Error when reading or writing sounding.
Problem with littler file.
littler\_tf\_dart
Error when reading or writing footer of the sounding.
Problem with littler file.
intplin, intplog
arrays xx and yy must have same size
Each x value needs a corresponding y value.
intplin, intplog
bad value in yy
It is assumed that the input yy contains rms errors and should not be
negative. That should be traced back to the file *obserr.txt*.

</div>

KNOWN BUGS
----------

none

[]{#FuturePlans}

------------------------------------------------------------------------

FUTURE PLANS
------------

1.  Develop the program to support all observations in DART and WRF
    3D-Var.
2.  Use the preprocessor to include the observation list provided by
    obs\_kind\_mod.

[]{#PrivateComponents}

------------------------------------------------------------------------

PRIVATE COMPONENTS
------------------

\

<div class="routine">

*call set\_str\_date(timestring, dart\_time)*
    type(time_type),   intent(in)  ::  dart_time 
    character(len=20), intent(out) ::  timestring 

</div>

<div class="indent1">

Given a dart\_time (seconds, days), returns date as
bbbbbbyyyymmddhhmmss, where b is a blank space.

</div>

\
\

<div class="routine">

*call set\_dart\_time(tstring, dart\_time)*
    character(len=20), intent(in)  ::  tstring 
    type(time_type),   intent(out) ::  dart_time 

</div>

<div class="indent1">

Given a date as bbbbbbyyyymmddhhmmss, where b is a blank space, returns
the dart\_time (seconds, days).

</div>

\
\

<div class="routine">

*call StoreObsErr(obs\_err\_var, pres, plevel, nlev, obs\_err\_std)*
    integer,  intent(in)    ::  nlev, pres 
    real(r8), intent(in)    ::  obs_err_var 
    integer,  intent(in)    ::  plevel(nlev) 
    real(r8), intent(inout) ::  obs_err_std(nlev) 

</div>

<div class="indent1">

If the incoming pres corresponds exactly to a pressure level in plevel,
then transfers the incoming obs\_err\_var into the array obs\_err\_std
at the corresponding level.

</div>

\
\

<div class="routine">

*level\_index = GetClosestLevel(ilev, vlev, nlev)*
    integer,  intent(in) ::  nlev, ilev 
    integer,  intent(in) ::  vlev(nlev) 

</div>

<div class="indent1">

Returns the index of the closest level in vlev to the incoming ilev.

</div>

\
\

<div class="routine">

*call READ\_OBSERR(filein, platform, sensor\_name, err, nlevels)*
    CHARACTER (LEN=80), intent(in)  ::  filein 
    CHARACTER (LEN=80), intent(in)  ::  platform 
    CHARACTER (LEN=80), intent(in   ::  sensor_name 
    INTEGER,            intent(in)  ::  nlevels 
    REAL(r8),           intent(out) ::  err(nlevels) 

</div>

<div class="indent1">

Read observational error on pressure levels (in hPa) from the incoming
filein and store the result in the array err. It is assumed that filein
has the same format as WRF 3D-Var *obserr.txt* file. It reads
observational error for a specific platform (e.g. RAOBS) and a specific
sensor (e.g. WIND SENSOR ERRORS).

</div>

\
\

<div class="routine">

*f\_obstype = obstype(line)*
    CHARACTER (LEN= 80), intent(in) ::  line 

</div>

<div class="indent1">

Read in a line the string present after keyword 'BOGUS', which should be
the sensor name.

</div>

\
\

<div class="routine">

*f\_sensor = sensor(line)*
    CHARACTER (LEN= 80), intent(in) ::  line 

</div>

<div class="indent1">

Read in a line the string present after numbers, which should be the
platform name.

</div>

\
\

<div class="routine">

*val = intplin(x,xx,yy)*
    INTEGER,  DIMENSION (:), intent(in) ::  xx 
    REAL(r8), DIMENSION (:), intent(in) ::  yy 
    REAL(r8),                intent(in) ::  x 

</div>

<div class="indent1">

Do a linear interpolation.

</div>

\
\

<div class="routine">

*val = intplog(x,xx,yy)*
    INTEGER,  DIMENSION (:), intent(in) ::  xx 
    REAL(r8), DIMENSION (:), intent(in) ::  yy 
    REAL(r8),                intent(in) ::  x 

</div>

<div class="indent1">

Do a log-linear interpolation.

</div>

\
\

<div class="routine">

*index = locate(x,xx)*
    INTEGER, DIMENSION (:), intent(in) ::  xx 
    REAL(r8),               intent(in) ::  x 

</div>

<div class="indent1">

Return the index in xx such that xx(index) &lt; x &lt; xx(index+1).

</div>

\
[]{#Legalese}

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


