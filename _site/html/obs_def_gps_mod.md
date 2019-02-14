[]{#TOP}

MODULE *obs\_def\_gps\_mod*
===========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../documentation/images/ | Index](../../documentation/index. |
| Dartboard7.png){height="70"}      | html)\                            |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[INTERFACES](#Interface) / [NAMELIST](#Namelist) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[PRIVATE COMPONENTS](#PrivateComponents) / [TERMS OF USE](#Legalese)

Overview
--------

DART GPS Radio Occultation observation module, including the observation
operators for both local and non-local refractivity computations.

Author and Contact information:

-   GPS Science: Hui Liu, hliu at ncar.edu
-   DART Code: Nancy Collins, nancy at ucar.edu

[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

This namelist is now enabled by default. The maximum number of GPS
observations is settable at runtime by changing the value in the
namelist. If you get an error about a missing namelist add
*&obs\_def\_gps\_nml* using the example below to your *input.nml*
namelist file and rerun. No recompiling is needed.

This namelist is read from the file *input.nml*. Namelists start with an
ampersand '&' and terminate with a slash '/'. Character strings that
contain a '/' must be enclosed in quotes to prevent them from
prematurely terminating the namelist.

<div class="namelist">

    &obs_def_gps_nml
      max_gpsro_obs = 100000,
    / 

</div>

\
\

<div>

  Item              Type      Description
  ----------------- --------- --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  max\_gpsro\_obs   integer   The maximum number of GPS refractivity observations supported for a single execution. Generally the default will be sufficient for a single run of filter, but not enough for a long diagnostics run to produce a time series.

</div>

\
\
[]{#OtherModulesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    types_mod
    utilities_mod
    location_mod (threed_sphere)
    assim_model_mod
    obs_kind_mod

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

  ---------------------------------- ------------------------------------------------------
  *use obs\_def\_gps\_mod, only :*   [read\_gpsro\_ref](#read_gpsro_ref)
                                     [write\_gpsro\_ref](#write_gpsro_ref)
                                     [get\_expected\_gpsro\_ref](#get_expected_gpsro_ref)
                                     [interactive\_gpsro\_ref](#interactive_gpsro_ref)
                                     [set\_gpsro\_ref](#set_gpsro_ref)
                                     [get\_gpsro\_ref](#get_gpsro_ref)
  ---------------------------------- ------------------------------------------------------

A note about documentation style. Optional arguments are enclosed in
brackets *\[like this\]*.

[]{#read_gpsro_ref}\

<div class="routine">

*call read\_gpsro\_ref(gpskey, ifile, *\[, fform\]*)*
    integer,          intent(out)          :: gpskey
    integer,          intent(in)           :: ifile
    character(len=*), intent(in), optional :: fform

</div>

<div class="indent1">

Refractivity observations have several items of auxiliary data to read
or write. This routine reads in the data for the next observation and
returns the private GPS key index number that identifies the auxiliary
data for this observation.

  ------------ ------------------------------------------------------------------------------------------------------
  *gpskey  *   GPS key number returned to the caller.
  *ifile*      Open file unit number to read from.
  *fform*      If specified, indicate whether the file was opened formatted or unformatted. Default is 'formatted'.
  ------------ ------------------------------------------------------------------------------------------------------

</div>

\
[]{#write_gpsro_ref}\

<div class="routine">

*call write\_gpsro\_ref(gpskey, ifile, *\[, fform\]*)*
    integer,          intent(in)           :: gpskey
    integer,          intent(in)           :: ifile
    character(len=*), intent(in), optional :: fform

</div>

<div class="indent1">

Refractivity observations have several items of auxiliary data to read
or write. This routine writes out the auxiliary data for the specified
observation to the file unit given.

  ------------ ------------------------------------------------------------------------------------------------------
  *gpskey  *   GPS key number identifying which observation to write aux data for.
  *ifile*      Open file unit number to write to.
  *fform*      If specified, indicate whether the file was opened formatted or unformatted. Default is 'formatted'.
  ------------ ------------------------------------------------------------------------------------------------------

</div>

\
[]{#get_expected_gpsro_ref}\

<div class="routine">

*call get\_expected\_gpsro\_ref(state\_vector, location, gpskey,
ro\_ref, istatus)*
    real(r8),            intent(in)  :: state_vector(:)
    type(location_type), intent(in)  :: location
    integer,             intent(in)  :: gpskey
    real(r8),            intent(out) :: ro_ref
    integer,             intent(out) :: istatus

</div>

<div class="indent1">

Given a location and the state vector from one of the ensemble members,
compute the model-predicted GPS refractivity that would be observed at
that location. There are two types of operators: modeled *local*
refractivity (N-1)\*1.0e6 or *non\_local* refractivity (excess phase, m)
The type is indicated in the auxiliary information for each
observation.\
\

  ------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *state\_vector  *   A one dimensional representation of the model state vector
  *location*          Location of this observation
  *gpskey*            Integer key identifying which GPS observation this is, so the correct corresponding auxiliary information can be accessed.
  *ro\_ref*           The returned GPS refractivity value
  *istatus*           Returned integer status code describing problems with applying forward operator. 0 is a good value; any positive value indicates an error; negative values are reserved for internal DART use only.
  ------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#interactive_gpsro_ref}\

<div class="routine">

*call interactive\_gpsro\_ref(gpskey)*
    integer, intent(out) :: gpskey

</div>

<div class="indent1">

Prompts the user for the auxiliary information needed for a GPS
refractivity observation, and returns the new key associated with this
data.

  ------------ -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *gpskey  *   Unique identifier associated with this GPS refractivity observation. In this code it is an integer index into module local arrays which hold the additional data. This routine returns the incremented value associated with this data.
  ------------ -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#set_gpsro_ref}\

<div class="routine">

*call set\_gpsro\_ref(gpskey, nx, ny, nz, rfict0, ds, htop, subset0)*
    integer,          intent(out) :: gpskey
    real(r8),         intent(in)  :: nx
    real(r8),         intent(in)  :: ny
    real(r8),         intent(in)  :: nz
    real(r8),         intent(in)  :: rfict0
    real(r8),         intent(in)  :: ds
    real(r8),         intent(in)  :: htop
    character(len=6), intent(in)  :: subset0

</div>

<div class="indent1">

Sets the auxiliary information associated with a GPS refractivity
observation. This routine increments and returns the new key associated
with these values.

  ----------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *gpskey*    Unique identifier associated with this GPS refractivity observation. In this code it is an integer index into module local arrays which hold the additional data. This routine returns the incremented value associated with this data.
  *nx*        X component of direction of ray between the LEO (detector) satellite and the GPS transmitter satellite at the tangent point.
  *ny*        Y component of tangent ray.
  *nz*        Z component of tangent ray.
  *rfict0*    Local curvature radius (meters).
  *ds*        Delta S, increment to move along the ray in each direction when integrating the non-local operator (meters).
  *htop*      Elevation (in meters) where integration stops along the ray.
  *subset0*   The string 'GPSREF' for the local operator (refractivity computed only at the tangent point), or 'GPSEXC' for the non-local operator which computes excess phase along the ray.
  ----------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#get_gpsro_ref}\

<div class="routine">

*call get\_gpsro\_ref(gpskey, nx, ny, nz, rfict0, ds, htop, subset0)*
    integer,          intent(in)  :: gpskey
    real(r8),         intent(out) :: nx
    real(r8),         intent(out) :: ny
    real(r8),         intent(out) :: nz
    real(r8),         intent(out) :: rfict0
    real(r8),         intent(out) :: ds
    real(r8),         intent(out) :: htop
    character(len=6), intent(out) :: subset0

</div>

<div class="indent1">

Gets the auxiliary information associated with a GPS refractivity
observation, based on the GPS key number specified.

  ----------- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *gpskey*    Unique identifier associated with this GPS refractivity observation. In this code it is an integer index into module local arrays which hold the additional data. The value specified selects which observation to return data for.
  *nx*        X component of direction of ray between the LEO (detector) satellite and the GPS transmitter satellite at the tangent point.
  *ny*        Y component of tangent ray.
  *nz*        Z component of tangent ray.
  *rfict0*    Local curvature radius (meters).
  *ds*        Delta S, increment to move along the ray in each direction when integrating the non-local operator (meters).
  *htop*      Elevation (in meters) where integration stops along the ray.
  *subset0*   The string 'GPSREF' for the local operator (refractivity computed only at the tangent point), or 'GPSEXC' for the non-local operator which computes excess phase along the ray.
  ----------- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   A DART observation sequence file containing GPS obs.

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

-   Assimilation of GPS Radio Occultation Data for Numerical Weather
    Prediction, Kuo,Y.H., Sokolovskiy,S.V., Anthes,R.A.,
    Vendenberghe,F., Terrestrial Atm and Ocn Sciences, Vol 11,
    pp157-186, 2000.

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
initial allocation failed for gps observation data, itemcount =
(max\_gpsro\_obs)
Need to increase max\_gpsro\_obs count in namelist
gpskey\_out\_of\_range
gpskey (key\#) exceeds max\_radial\_gps\_obs (maxval)
The number of GPS observations exceeds the array size allocated in the
module. Need to increase max\_gpsro\_obs count in namelist.
read\_gpsro\_ref
Expected header 'gpsroref' in input file
The format of the input obs\_seq file is not consistent.
get\_expected\_gpsro\_ref
vertical location must be height; gps obs key \#
GPS observations must have vertical coordinates of height

</div>

KNOWN BUGS
----------

none at this time

[]{#FuturePlans}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FUTURE PLANS
------------

The current code first bins the very densely-sampled vertical profile
into 200 bins, and then interpolates the requested vertical location
from that. The original profiles have been plotted and are smooth; there
appears to be no need to pre-bin the ata.

The local operator needs no additional auxiliary data. The observation
files would be much smaller if the local operator observation was a
separate type without aux data, and only the non-local operator
observation types would need the ray direction, the curvature, etc.

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

  ------------------ ---------------------------------------
  Contact:           Nancy Collins, Hui Liu, Jeff Anderson
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ ---------------------------------------


