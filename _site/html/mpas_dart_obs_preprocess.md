[]{#TOP}

PROGRAM *mpas\_dart\_obs\_preprocess*
=====================================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../documentation/images/ | Index](../../documentation/index. |
| Dartboard7.png){height="70"}      | html)\                            |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [MODULES](#Modules) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

Overview
--------

Program to preprocess observations, with specific knowledge of the MPAS
grid.

This program can superob (average) aircraft and satellite wind obs if
they are too dense, based on the given MPAS ATM grid. It will average
all observations of the same type in each grid cell. The averaging grid
can be different than the grid used for the assimilation run.

This program can read up to 10 additional obs\_seq files and merge their
data in with the basic obs\_sequence file which is the main input.

This program can reject surface observations if the elevation encoded in
the observation is too different from the mpas surface elevation.

This program can exclude observations above a specified height or
pressure.

This program can exclude observations outside a given time window
defined by the specified analysis time and a window width in hours.

This program can overwrite the incoming Data QC value with another.

[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

This namelist is read from the file *input.nml*. Namelists start with an
ampersand '&' and terminate with a slash '/'. Character strings that
contain a '/' must be enclosed in quotes to prevent them from
prematurely terminating the namelist.

<div class="namelist">

    &mpas_obs_preproc_nml

      file_name_input          = 'obs_seq.old'
      file_name_output         = 'obs_seq.new'
      
      sonde_extra              = 'obs_seq.rawin'
      land_sfc_extra           = 'obs_seq.land_sfc'
      metar_extra              = 'obs_seq.metar'
      marine_sfc_extra         = 'obs_seq.marine'
      sat_wind_extra           = 'obs_seq.satwnd'
      profiler_extra           = 'obs_seq.profiler'
      gpsro_extra              = 'obs_seq.gpsro'
      acars_extra              = 'obs_seq.acars'
      gpspw_extra              = 'obs_seq.gpspw'
      trop_cyclone_extra       = 'obs_seq.tc'
      
      overwrite_obs_time       = .false.  
      windowing_obs_time       = .false. 
      windowing_int_hour       = 1.5
      
      obs_boundary             = 0.0
      increase_bdy_error       = .false.  
      maxobsfac                = 2.5   
      obsdistbdy               = 15.0  
      
      sfc_elevation_check      = .false.  
      sfc_elevation_tol        = 300.0  
      obs_pressure_top         = 0.0  
      obs_height_top           = 2.0e10  
      
      include_sig_data         = .true.   
      tc_sonde_radii           = -1.0  
      superob_qc_threshold     = 4         
      
      superob_aircraft         = .false.  
      aircraft_horiz_int       = 36.0  
      aircraft_pres_int        = 2500.0  
      
      superob_sat_winds        = .false.    
      sat_wind_horiz_int       = 100.0   
      sat_wind_pres_int        = 2500.0  
      
      overwrite_ncep_satwnd_qc = .false.    
      overwrite_ncep_sfc_qc    = .false.  

      max_num_obs              = 1000000 
    /

</div>

\
\

<div>

Item
Type
Description
**Generic parameters:**
file\_name\_input
character(len=129)
The input obs\_seq file.
file\_name\_output
character(len=129)
The output obs\_seq file.
sonde\_extra, land\_sfc\_extra, metar\_extra, marine\_sfc\_extra,
marine\_sfc\_extra, sat\_wind\_extra, profiler\_extra, gpsro\_extra,
acars\_extra, gpspw\_extra, trop\_cyclone\_extra
character(len=129)
The names of additional input obs\_seq files, which if they exist, will
be merged in with the obs from the *file\_name\_input* obs\_seq file. If
the files do not exist, they are silently ignored without error.
max\_num\_obs
integer
Must be larger than the total number of observations to be processed.
**Parameters to reduce observation count:**
sfc\_elevation\_check
logical
If true, check the height of surface observations against the surface
height in the model. Observations further away than the specified
tolerance will be excluded.
sfc\_elevation\_tol
real(r8)
If *sfc\_elevation\_check* is true, the maximum difference between the
elevation of a surface observation and the model surface height, in
meters. If the difference is larger than this value, the observation is
excluded.
obs\_pressure\_top
real(r8)
Observations with a vertical coordinate in pressure which are located
above this pressure level (i.e. the obs vertical value is smaller than
the given pressure) will be excluded.
obs\_height\_top
real(r8)
Observations with a vertical coordinate in height which are located
above this height value (i.e. the obs vertical value is larger than the
given height) will be excluded.
**Radio/Rawinsonde-specific parameters:**
include\_sig\_data
logical
If true, include significant level data from radiosondes.
tc\_sonde\_radii
real(r8)
If greater than 0.0 remove any sonde observations closer than this
distance in Kilometers to the center of a Tropical Cyclone.
**Aircraft-specific parameters:**
superob\_aircraft
logical
If true, average all aircraft observations within the same MPAS grid
cell, at the given vertical levels. The output obs will be only a single
observation per cell, per vertical level.
aircraft\_pres\_int
real(r8)
If *superob\_aircraft* is true, the vertical distance in pressure which
defines a series of superob vertical bins.
superob\_qc\_threshold
integer
If *superob\_aircraft* is true, the Quality Control threshold at which
observations are ignored when doing superob averaging. The value
specified here is the largest acceptable QC; values equal to or lower
are kept, and values larger than this are rejected.
**Satellite Wind-specific parameters:**
superob\_sat\_winds
logical
If true, average all satellite wind observations within the same MPAS
grid cell, at the given vertical levels. The output obs will be only a
single observation per cell, per vertical level.
sat\_wind\_pres\_int
real(r8)
If *superob\_sat\_winds* is true, the vertical distance in pressure
which defines a series of superob vertical bins.
overwrite\_ncep\_satwnd\_qc
logical
If true, replace the incoming Data QC value in satellite wind
observations with 2.0.
**Surface Observation-specific parameters:**
overwrite\_ncep\_sfc\_qc
logical
If true, replace the incoming Data QC value in surface observations with
2.0.
**Parameters to select by time or alter observation time:**
windowing\_obs\_time
logical
If true, exclude observations with a time outside the given window. The
window is specified as a number of hours before and after the current
analysis time.
windowing\_int\_hour
real(r8)
The window half-width, in hours. If 'windowing\_obs\_time' is .false.
this value is ignored. If 'windowing\_obs\_time' is true, observations
with a time further than this number of hours away from the analysis
time will be excluded. To ensure disjoint subsets from a continueous
sequence of observations, time values equal to the earliest time
boundaries are discarded while time values equal to the latest time
boundary are retained.
overwrite\_obs\_time
logical
If true, replace the incoming observation time with the analysis time.
Not recommended.

</div>

\
\
[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    types_mod
    obs_sequence_mod
    utilities_mod
    obs_kind_mod
    time_manager_mod
    model_mod
    netcdf

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   Input namelist ; *input.nml*
-   Input MPAS state netCDF file: *mpas\_init.nc*
-   Input obs\_seq files (as specified in namelist)
-   Output obs\_seq file (as specified in namelist)

### File formats

This utility can read one or more obs\_seq files and combine them while
doing the rest of the processing. It uses the standard DART observation
sequence file format. It uses the grid information from an MPAS file to
define the bins for combining nearby aircraft and satellite wind
observations.

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

-   Developed by Soyoung Ha, based on the WRF observation preprocessor
    contributed by Ryan Torn.

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
mpas\_dart\_obs\_preprocess
mpas\_dart\_obs\_preprocess
mpas\_dart\_obs\_preprocess
mpas\_dart\_obs\_preprocess

</div>

KNOWN BUGS
----------

none

[]{#FuturePlans}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FUTURE PLANS
------------

none.

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
  Contact:           Glen Romine, Nancy Collins
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


