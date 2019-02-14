[]{#TOP}

MODULE *obs\_def\_radar\_mod*
=============================

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

DART radar observation module, including the observation operators for
the two primary radar-observation types -- Doppler velocity and
reflectivity -- plus other utility subroutines and functions. A number
of simplifications are employed for the observation operators. Most
notably, the model state is mapped to a "point" observation, whereas a
real radar observation is a volumetric sample. The implications of this
approximation have not been investigated fully, so in the future it
might be worth developing and testing more sophisticated observation
operators that produce volumetric power- weighted samples.\
\
This module is able to compute reflectivity and precipitation fall speed
(needed for computing Doppler radial velocity) from the prognostic model
fields only for simple single-moment microphysics schemes such as the
Kessler and Lin schemes. If a more complicated microphysics scheme is
used, then reflectivity and fall speed must be accessible instead as
diagnostic fields in the model state.\
\
Author and Contact information:

-   Radar Science: David Dowell, david.dowell at noaa.gov, Glen Romine,
    romine at ucar.edu
-   DART Code: Nancy Collins, nancy at ucar.edu
-   Original DART/Radar work: Alain Caya

### Backward compatibility note:

For users of previous versions of the radar obs\_def code, here are a
list of changes beginning with subversion revision 3616 which are not
backward compatible:

-   The namelist has changed quite a bit; some items were removed, some
    added, and some renamed. See the [namelist documention](#Namelist)
    in this file for the current item names and default values.
-   Some constants which depend on the microphysics scheme have been
    added to the namelist to make it easier to change the values for
    different schemes, but the defaults have also changed. Verify they
    are appropriate for the scheme being used.
-   The interactive create routine prompts for the beam direction
    differently now. It takes azimuth and elevation, and then does the
    trigonometry to compute the three internal values which are stored
    in the file. The previous version prompted for the internal values
    directly.
-   The get\_expected routines try to call the model interpolate routine
    for *QTY\_POWER\_WEIGHTED\_FALL\_SPEED* and
    *QTY\_RADAR\_REFLECTIVITY* values. If they are not available then
    the code calls the model interpolate routines for several other
    quantities and computes these quantities. However, this requires
    that the model\_mod interpolate code returns gracefully if the
    quantity is unknown or unsupported. The previous version of the WRF
    model\_mod code used to print an error message and stop if the
    quantity was unknown. The updated version in the repository which
    went in with this radar code has been changed to return an error
    status code but continue if the quantity is unknown.
-   The value for gravity is currently hardcoded in this module.
    Previous versions of this code used the gravity constant in the DART
    types\_mod.f90 code, but in reality the code should be using
    whatever value of gravity is being used in the model code. For now,
    the value is at least separated so users can change the value in
    this code if necessary.

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

  ------------------------------------ --------------------------------------------------------
  *use obs\_def\_radar\_mod, only :*   [read\_radar\_ref](#read_radar_ref)
                                       [get\_expected\_radar\_ref](#get_expected_radar_ref)
                                       [read\_radial\_vel](#read_radial_vel)
                                       [write\_radial\_vel](#write_radial_vel)
                                       [interactive\_radial\_vel](#interactive_radial_vel)
                                       [get\_expected\_radial\_vel](#get_expected_radial_vel)
                                       [get\_obs\_def\_radial\_vel](#get_obs_def_radial_vel)
                                       [set\_radial\_vel](#set_radial_vel)
  ------------------------------------ --------------------------------------------------------

Namelist interface [*&obs\_def\_radar\_mod\_nml*](#Namelist) is read
from file *input.nml*.

A note about documentation style. Optional arguments are enclosed in
brackets *\[like this\]*.

[]{#read_radar_ref}\

<div class="routine">

*call read\_radar\_ref(obsvalue, refkey)*
    real(r8),                   intent(inout) :: obsvalue
    integer,                    intent(out)   :: refkey

</div>

<div class="indent1">

Reflectivity observations have no auxiliary data to read or write, but
there are namelist options that can alter the observation value at
runtime. This routine tests the observation value and alters it if
required.

  -------------- ---------------------------------------------------------------
  *obsvalue  *   Observation value.
  *refkey*       Set to 0 to avoid uninitialized values, but otherwise unused.
  -------------- ---------------------------------------------------------------

</div>

\
[]{#get_expected_radar_ref}\

<div class="routine">

*call get\_expected\_radar\_ref(state\_vector, location, ref, istatus)*
    real(r8),            intent(in)  :: state_vector(:)
    type(location_type), intent(in)  :: location
    real(r8),            intent(out) :: ref
    integer,             intent(out) :: istatus

</div>

<div class="indent1">

Given a location and the state vector from one of the ensemble members,
compute the model-predicted radar reflectivity that would be observed at
that location. The returned value is in dBZ.\
\
If *apply\_ref\_limit\_to\_fwd\_op* is .TRUE. in the namelist,
reflectivity values less than *reflectivity\_limit\_fwd\_op* will be set
to *lowest\_reflectivity\_fwd\_op*.

  ------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *state\_vector  *   A one dimensional representation of the model state vector
  *location*          Location of this observation
  *ref*               The returned radar reflectivity value
  *istatus*           Returned integer status code describing problems with applying forward operator. 0 is a good value; any positive value indicates an error; negative values are reserved for internal DART use only.
  ------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#read_radial_vel}\

<div class="routine">

*call read\_radial\_vel(velkey, ifile *\[, fform\]*)*
    integer,                    intent(out) :: velkey
    integer,                    intent(in)  :: ifile
    character(len=*), optional, intent(in)  :: fform

</div>

<div class="indent1">

Reads the additional auxiliary information associated with a radial
velocity observation. This includes the location of the radar source,
the beam direction, and the nyquist velocity.

  ------------ ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *velkey  *   Unique identifier associated with this radial velocity observation. In this code it is an integer index into module local arrays which hold the additional data. This routine increments it and returns the new value.
  *ifile*      File unit descriptor for input file
  *fform*      File format specifier: FORMATTED or UNFORMATTED; default FORMATTED
  ------------ ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#write_radial_vel}\

<div class="routine">

*call write\_radial\_vel(velkey, ifile *\[, fform\]*)*
    integer,                    intent(in) :: velkey
    integer,                    intent(in) :: ifile
    character(len=*), optional, intent(in) :: fform

</div>

<div class="indent1">

Writes the additional auxiliary information associated with a radial
velocity observation. This includes the location of the radar source,
the beam direction, and the nyquist velocity.

  ------------ ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *velkey  *   Unique identifier associated with this radial velocity observation. In this code it is an integer index into module local arrays which hold the additional data. This routine uses the value to select the appropriate data to write for this observation.
  *ifile*      File unit descriptor for output file
  *fform*      File format specifier: FORMATTED or UNFORMATTED; default FORMATTED
  ------------ ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#get_obs_def_radial_vel}\

<div class="routine">

*call get\_obs\_def\_radial\_vel(velkey, radar\_location,
beam\_direction, nyquist\_velocity)*
    integer,             intent(in)  :: velkey
    type(location_type), intent(out) :: radar_location
    real(r8),            intent(out) :: beam_direction(3)
    real(r8),            intent(out) :: nyquist_velocity

</div>

<div class="indent1">

Returns the auxiliary information associated with a given radial
velocity observation.

  ----------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *velkey*                Unique identifier associated with this radial velocity observation. In this code it is an integer index into module local arrays which hold the additional data. This routine uses the value to select the appropriate data to return.
  *radar\_location*       Location of the radar.
  *beam\_orientation*     Orientation of the radar beam at the observation location. The three values are: sin(azimuth)\*cos(elevation), cos(azimuth)\*cos(elevation), and sin(elevation).
  *nyquist\_velocity  *   Nyquist velocity at the observation point in meters/second.
  ----------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#set_radial_vel}\

<div class="routine">

*call set\_radial\_vel(velkey, radar\_location, beam\_direction,
nyquist\_velocity)*
    integer,             intent(out) :: velkey
    type(location_type), intent(in)  :: radar_location
    real(r8),            intent(in)  :: beam_direction(3)
    real(r8),            intent(in)  :: nyquist_velocity

</div>

<div class="indent1">

Sets the auxiliary information associated with a radial velocity
observation. This routine increments and returns the new key associated
with these values.

  ----------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *velkey*                Unique identifier associated with this radial velocity observation. In this code it is an integer index into module local arrays which hold the additional data. This routine returns the incremented value associated with this data.
  *radar\_location*       Location of the radar.
  *beam\_orientation*     Orientation of the radar beam at the observation location. The three values are: sin(azimuth)\*cos(elevation), cos(azimuth)\*cos(elevation), and sin(elevation).
  *nyquist\_velocity  *   Nyquist velocity at the observation point in meters/second.
  ----------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#interactive_radial_vel}\

<div class="routine">

*call interactive\_radial\_vel(velkey)*
    integer, intent(out) :: velkey

</div>

<div class="indent1">

Prompts the user for the auxiliary information needed for a radial
velocity observation, and returns the new key associated with this data.

  ------------ ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *velkey  *   Unique identifier associated with this radial velocity observation. In this code it is an integer index into module local arrays which hold the additional data. This routine returns the incremented value associated with this data.
  ------------ ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#get_expected_radial_vel}\

<div class="routine">

*call get\_expected\_radial\_vel(state\_vector, location, velkey,
radial\_vel, istatus)*
    real(r8),            intent(in)  :: state_vector(:)
    type(location_type), intent(in)  :: location
    integer,             intent(in)  :: velkey
    real(r8),            intent(out) :: radial_vel
    integer,             intent(out) :: istatus

</div>

<div class="indent1">

Given a location and the state vector from one of the ensemble members,
compute the model-predicted radial velocity in meters/second that would
be observed at that location. *velkey* is the unique index for this
particular radial velocity observation. The value is returned in
*radial\_vel*, *istatus* is the return code.\
\
The along-beam component of the 3-d air velocity is computed from the u,
v, and w fields plus the beam\_direction. The along-beam component of
power-weighted precipitation fall velocity is added to the result.

  ------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *state\_vector  *   A one dimensional representation of the model state vector
  *location*          Location of this observation
  *velkey*            Unique identifier associated with this radial velocity observation
  *radial\_vel*       The returned radial velocity value in meters/second
  *istatus*           Returned integer status code describing problems with applying forward operator. 0 is a good value; any positive value indicates an error; negative values are reserved for internal DART use only.
  ------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
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

    &obs_def_radar_mod_nml
       apply_ref_limit_to_obs      =   .false.,
       reflectivity_limit_obs      =     -10.0,
       lowest_reflectivity_obs     =     -10.0,
       apply_ref_limit_to_fwd_op   =   .false.,
       reflectivity_limit_fwd_op   =     -10.0,
       lowest_reflectivity_fwd_op  =     -10.0,
       max_radial_vel_obs          =   1000000,
       allow_wet_graupel           =   .false.,
       microphysics_type           =       2  ,
       allow_dbztowt_conv          =   .false.,
       dielectric_factor           =     0.224,
       n0_rain                     =     8.0e6,
       n0_graupel                  =     4.0e6,
       n0_snow                     =     3.0e6,
       rho_rain                    =    1000.0,
       rho_graupel                 =     400.0,
       rho_snow                    =     100.0 
       /

</div>

\
\

<div>

Item
Type
Description
apply\_ref\_limit\_to\_obs
logical
If .TRUE. replace all reflectivity values less than
"reflectivity\_limit\_obs" with "lowest\_reflectivity\_obs" value. If
.FALSE. leave all values as-is.
reflectivity\_limit\_obs
real(r8)
The threshold value. Observed reflectivity values less than this
threshold will be set to the "lowest\_reflectivity\_obs" value. Units
are dBZ.
lowest\_reflectivity\_obs
real(r8)
The 'set-to' value. Observed reflectivity values less than the threshold
will be set to this value. Units are dBZ.
apply\_ref\_limit\_to\_fwd\_op
logical
Same as "apply\_ref\_limit\_to\_obs", but for the forward operator.
reflectivity\_limit\_fwd\_op
real(r8)
Same as "reflectivity\_limit\_obs", but for the forward operator values.
lowest\_reflectivity\_fwd\_op
real(r8)
Same as "lowest\_reflectivity\_obs", but for the forward operator
values.
max\_radial\_vel\_obs
integer
Maximum number of observations of this type to support at run time. This
is combined total of all obs\_seq files, for example the observation
diagnostic program potentially opens multiple obs\_seq.final files, or
the obs merge program can also open multiple obs files.
allow\_wet\_graupel
logical
It is difficult to predict/diagnose whether graupel/hail has a wet or
dry surface. Even when the temperature is above freezing, evaporation
and/or absorption can still result in a dry surface. This issue is
important because the reflectivity from graupel with a wet surface is
significantly greater than that from graupel with a dry surface.
Currently, the user has two options for how to compute graupel
reflectivity. If allow\_wet\_graupel is .false. (the default), then
graupel is always assumed to be dry. If allow\_wet\_graupel is .true.,
then graupel is assumed to be wet (dry) when the temperature is above
(below) freezing. A consequence is that a sharp gradient in reflectivity
will be produced at the freezing level. In the future, it might be
better to provide the option of having a transition layer.
microphysics\_type
integer
If the state vector contains the reflectivity or the power weighted fall
speed, interpolate directly from those regardless of the setting of this
item. If the state vector does not contain the fields, this value should
be set to be compatible with whatever microphysical scheme is being used
by the model. If the model is using a different microphysical scheme but
has compatible fields to the ones listed below, setting this value will
select the scheme to use.
-   1 = Kessler scheme.
-   2 = Lin et al. microphysics
-   3 = User selected scheme where 10 cm reflectivity and power weighted
    fall velocity are expected in the state vector (failure if not
    found)
-   4 = User selected scheme where only power weighted fall velocity is
    expected (failure if not found)
-   5 = User selected scheme where only reflectivity is expected
    (failure if not found)
-   -1 = ASSUME FALL VELOCITY IS ZERO, allows over-riding the failure
    modes above if reflectivity and/or fall velocity are not available
    but a result is desired for testing purposes only.

allow\_dbztowt\_conv
logical
Flag to enable use of the dbztowt routine where reflectivity is
available, but not the power-weighted fall velocity. This scheme uses
emperical relations between reflectivity and fall velocity, with poor
accuracy for highly reflective, low density particles (such as water
coated snow aggregates). Expect questionable accuracy in radial velocity
from the forward operator with high elevation angles where ice is
present in the model state.
dielectric\_factor
real(r8)
According to Smith (1984), there are two choices for the dielectric
factor depending on how the snow particle sizes are specified. If melted
raindrop diameters are used, then the factor is 0.224. If equivalent ice
sphere diameters are used, then the factor is 0.189. The default is set
to use the common convention of melted raindrop diameters.
n0\_rain
real(r8)
Intercept parameters (m\^-4) for size distributions of each hydrometeor.
The default of 8.0e6 is for the Lin et al. microphysics scheme with the
Hobbs settings for graupel/hail. (The Hobbs graupel settings are also
the default for the Lin scheme in WRF 2.2 and 3.0.)
n0\_graupel
real(r8)
Intercept parameters (m\^-4) for size distributions of each hydrometeor.
The default of 4.0e6 is for the Lin et al. microphysics scheme with the
Hobbs settings for graupel/hail. (The Hobbs graupel settings are also
the default for the Lin scheme in WRF 2.2 and 3.0.)
n0\_snow
real(r8)
Intercept parameters (m\^-4) for size distributions of each hydrometeor.
The default of 3.0e6 is for the Lin et al. microphysics scheme with the
Hobbs settings for graupel/hail. (The Hobbs graupel settings are also
the default for the Lin scheme in WRF 2.2 and 3.0.)
rho\_rain
real(r8)
Density (kg m\^-3) of each hydrometeor type. The default of 1000.0 is
for the Lin et al. microphysics scheme with the Hobbs setting for
graupel/hail.
rho\_graupel
real(r8)
Density (kg m\^-3) of each hydrometeor type. The default of 400.0 is for
the Lin et al. microphysics scheme with the Hobbs setting for
graupel/hail.
rho\_snow
real(r8)
Density (kg m\^-3) of each hydrometeor type. The default of 100.0 is for
the Lin et al. microphysics scheme with the Hobbs setting for
graupel/hail.

</div>

\
\
[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   A DART observation sequence file containing Radar obs.

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

-   Battan, L. J., 1973: *Radar Observation of the Atmosphere.* Univ. of
    Chicago Press, 324 pp.
-   Caya, A. *Radar Observations in Dart.* DART Subversion repository.
-   Doviak, R. J., and D. S. Zrnic, 1993: *Doppler Radar and Weather
    Observations.* Academic Press, 562 pp.
-   Ferrier, B. S., 1994: A double-moment multiple-phase four-class bulk
    ice scheme. Part I: Description. *J. Atmos. Sci.*, **51**, 249-280.
-   Lin, Y.-L., Farley R. D., and H. D. Orville, 1983: Bulk
    parameterization of the snow field in a cloud model. *J. Climate
    Appl. Meteor.*, **22**, 1065-1092.
-   Smith, P. L. Jr., 1984: Equivalent radar reflectivity factors for
    snow and ice particles. *J. Climate Appl. Meteor.*, 23, 1258-1260.
-   Smith, P. L. Jr., Myers C. G., and H. D. Orville, 1975: Radar
    reflectivity factor calculations in numerical cloud models using
    bulk parameterization of precipitation. *J. Appl. Meteor.*, **14**,
    1156-1165.

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
initial allocation failed for radial vel obs data, itemcount =
(max\_radial\_vel\_obs)
Need to increase max\_radial\_vel\_obs count in namelist
read\_radial\_vel
Expected location header "platform" in input file
The format of the input file is not consistent.
velkey\_out\_of\_range
velkey (val) exceeds max\_radial\_vel\_obs (maxval)
The number of radial velocity observations exceeds the array size
allocated in the module. Need to increase max\_radial\_vel\_obs count in
namelist.
read\_nyquist\_velocity
bad value for nyquist velocity
The format of the input obs\_seq file is not consistent.
read\_beam\_direction
beam\_direction value must be between -1 and 1, got ()
The format of the input obs\_seq file is not consistent.
read\_beam\_direction
Expected orientation header "dir3d" in input file
The format of the input obs\_seq file is not consistent.

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

none at this time

[]{#PrivateComponents}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PRIVATE COMPONENTS
------------------

  ------------------------------------ -----------------------------------------------------------------
  *use obs\_def\_radar\_mod, only :*   [initialize\_module](#initialize_module)
                                       [read\_beam\_direction](#read_beam_direction)
                                       [read\_nyquist\_velocity](#read_nyquist_velocity)
                                       [write\_beam\_direction](#write_beam_direction)
                                       [write\_nyquist\_velocity](#write_nyquist_velocity)
                                       [interactive\_beam\_direction](#interactive_beam_direction)
                                       [interactive\_nyquist\_velocity](#interactive_nyquist_velocity)
                                       [get\_reflectivity](#get_reflectivity)
                                       [get\_precip\_fall\_speed](#get_precip_fall_speed)
                                       [initialize\_constants](#initialize_constants)
                                       [print\_constants](#print_constants)
                                       [pr\_con](#pr_con)
                                       [velkey\_out\_of\_range](#velkey_out_of_range)
                                       [check\_namelist\_limits](#check_namelist_limits)
                                       [ascii\_file\_format](#ascii_file_format)
  ------------------------------------ -----------------------------------------------------------------

[]{#initialize_module}\

<div class="routine">

*call initialize\_module()*

</div>

<div class="indent1">

Reads the namelist, allocates space for the auxiliary data associated
wtih radial velocity observations, initializes the constants used in
subsequent computations (possibly altered by values in the namelist),
and prints out the list of constants and the values in use. These may
need to change depending on which microphysics scheme is being used.

</div>

\
[]{#read_beam_direction}\

<div class="routine">

*beam\_direction = read\_beam\_direction(ifile, is\_asciiformat)*
    real(r8), dimension(3)            :: read_beam_direction
    integer,               intent(in) :: ifile
    logical,               intent(in) :: is_asciiformat

</div>

<div class="indent1">

Reads the beam direction at the observation location. Auxiliary data for
doppler radial velocity observations.

  --------------------------- ------------------------------------------------------------------------------------------------------------
  *read\_beam\_direction  *   Returns three real values for the radar beam orientation
  *ifile*                     File unit descriptor for input file
  *is\_asciiformat*           File format specifier: .TRUE. if file is formatted/ascii, or .FALSE. if unformatted/binary. Default .TRUE.
  --------------------------- ------------------------------------------------------------------------------------------------------------

</div>

\
[]{#read_nyquist_velocity}\

<div class="routine">

*nyquist\_velocity = read\_nyquist\_velocity(ifile, is\_asciiformat)*
    real(r8),            :: read_nyquist_velocity
    integer,  intent(in) :: ifile
    logical,  intent(in) :: is_asciiformat

</div>

<div class="indent1">

Reads nyquist velocity for a doppler radial velocity observation.

  ----------------------------- ------------------------------------------------------------------------------------------------------------
  *read\_nyquist\_velocity  *   Returns a real value for the nyquist velocity value
  *ifile*                       File unit descriptor for input file
  *is\_asciiformat*             File format specifier: .TRUE. if file is formatted/ascii, or .FALSE. if unformatted/binary. Default .TRUE.
  ----------------------------- ------------------------------------------------------------------------------------------------------------

</div>

\
[]{#write_beam_direction}\

<div class="routine">

*call write\_beam\_direction(ifile, beam\_direction, is\_asciiformat)*
    integer,                intent(in) :: ifile
    real(r8), dimension(3), intent(in) :: beam_direction
    logical,                intent(in) :: is_asciiformat

</div>

<div class="indent1">

Writes the beam direction at the observation location. Auxiliary data
for doppler radial velocity observations.

  --------------------- ------------------------------------------------------------------------------------------------------------
  *ifile*               File unit descriptor for output file
  *beam\_direction  *   Three components of the radar beam orientation
  *is\_asciiformat*     File format specifier: .TRUE. if file is formatted/ascii, or .FALSE. if unformatted/binary. Default .TRUE.
  --------------------- ------------------------------------------------------------------------------------------------------------

</div>

\
[]{#write_nyquist_velocity}\

<div class="routine">

*call write\_nyquist\_velocity(ifile, nyquist\_velocity,
is\_asciiformat)*
    integer,  intent(in) :: ifile
    real(r8), intent(in) :: nyquist_velocity
    logical,  intent(in) :: is_asciiformat

</div>

<div class="indent1">

Writes nyquist velocity for a doppler radial velocity observation.

  ----------------------- ------------------------------------------------------------------------------------------------------------
  *ifile*                 File unit descriptor for output file
  *nyquist\_velocity  *   The nyquist velocity value for this observation
  *is\_asciiformat*       File format specifier: .TRUE. if file is formatted/ascii, or .FALSE. if unformatted/binary. Default .TRUE.
  ----------------------- ------------------------------------------------------------------------------------------------------------

</div>

\
[]{#interactive_beam_direction}\

<div class="routine">

*call interactive\_beam\_direction(beam\_direction)*
    real(r8), dimension(3), intent(out) :: beam_direction

</div>

<div class="indent1">

Prompts the user for input for the azimuth and elevation of the radar
beam at the observation location. Will be converted to the three values
actually stored in the observation sequence file.

  --------------------- ------------------------------------------------
  *beam\_direction  *   Three components of the radar beam orientation
  --------------------- ------------------------------------------------

</div>

\
[]{#interactive_nyquist_velocity}\

<div class="routine">

*call interactive\_nyquist\_velocity(nyquist\_velocity)*
    real(r8), intent(out) :: nyquist_velocity

</div>

<div class="indent1">

Prompts the user for input for the nyquist velocity value associated
with a doppler radial velocity observation.

  ----------------------- ---------------------------------------------
  *nyquist\_velocity  *   Nyquist velocity value for the observation.
  ----------------------- ---------------------------------------------

</div>

\
[]{#get_reflectivity}\

<div class="routine">

*call get\_reflectivity(qr, qg, qs, rho, temp, ref)*
    real(r8), intent(in)  :: qr
    real(r8), intent(in)  :: qg
    real(r8), intent(in)  :: qs
    real(r8), intent(in)  :: rho
    real(r8), intent(in)  :: temp
    real(r8), intent(out) :: ref

</div>

<div class="indent1">

Computes the equivalent radar reflectivity factor in mm^6^ m^-3^ for
simple single-moment microphysics schemes such as Kessler and Lin, et
al. See the [references](#References) for more details.

  ---------- ---------------------------------------
  *qr*       Rain water content (kg kg^-1^)
  *qg*       Graupel/hail content (kg kg^-1^)
  *qs*       Snow content (kg kg^-1^)
  *rho*      Air density (kg m^-3^)
  *temp  *   Air temperature (K)
  *ref*      The returned radar reflectivity value
  ---------- ---------------------------------------

</div>

\
[]{#get_precip_fall_speed}\

<div class="routine">

*call get\_precip\_fall\_speed(qr, qg, qs, rho, temp,
precip\_fall\_speed)*
    real(r8), intent(in)  :: qr
    real(r8), intent(in)  :: qg
    real(r8), intent(in)  :: qs
    real(r8), intent(in)  :: rho
    real(r8), intent(in)  :: temp
    real(r8), intent(out) :: precip_fall_speed

</div>

<div class="indent1">

Computes power-weighted precipitation fall speed in m s^-1^ for simple
single-moment microphysics schemes such as Kessler and Lin, et al. See
the [references](#References) for more details.

  ------------------------- ---------------------------------------
  *qr*                      Rain water content (kg kg^-1^)
  *qg*                      Graupel/hail content (kg kg^-1^)
  *qs*                      Snow content (kg kg^-1^)
  *rho*                     Air density (kg m^-3^)
  *temp*                    Air temperature (K)
  *precip\_fall\_speed  *   The returned precipitation vall speed
  ------------------------- ---------------------------------------

</div>

\
[]{#initialize_constants}\

<div class="routine">

*call initialize\_constants()*

</div>

<div class="indent1">

Set values for a collection of constants used throughout the module
during the various calculations. These are set once in this routine and
are unchanged throughout the rest of the execution. They cannot be true
Fortran *parameters* because some of the values can be overwritten by
namelist entries, but once they are set they are treated as read-only
parameters.

</div>

\
[]{#print_constants}\

<div class="routine">

*call print\_constants()*

</div>

<div class="indent1">

Print out the names and values of all constant parameters used by this
module. The error handler message facility is used to print the message,
which by default goes to both the DART log file and to the standard
output of the program.

</div>

\
[]{#pr_con}\

<div class="routine">

*call pr\_con(c\_val, c\_str)*
    real(r8),         intent(in)  :: c_val
    character(len=*), intent(in)  :: c_str

</div>

<div class="indent1">

Calls the DART error handler routine to print out a string label and a
real value to both the log file and to the standard output.

  ----------------------- ---------------------
  *Value of constant  *   A real value.
  *Name of constant*      A character string.
  ----------------------- ---------------------

</div>

\
[]{#velkey_out_of_range}\

<div class="routine">

*call velkey\_out\_of\_range(velkey)*
    integer, intent(in)  :: velkey

</div>

<div class="indent1">

Range check key and trigger a fatal error if larger than the allocated
array for observation auxiliary data.

  ------------ ---------------------------------------------------------------
  *velkey  *   Integer key into a local array of auxiliary observation data.
  ------------ ---------------------------------------------------------------

</div>

\
[]{#check_namelist_limits}\

<div class="routine">

*call check\_namelist\_limits(apply\_ref\_limit\_to\_obs,
reflectivity\_limit\_obs, lowest\_reflectivity\_obs,
apply\_ref\_limit\_to\_fwd\_op, reflectivity\_limit\_fwd\_op,
lowest\_reflectivity\_fwd\_op)*
    logical,  intent(in) :: apply_ref_limit_to_obs
    real(r8), intent(in) :: reflectivity_limit_obs
    real(r8), intent(in) :: lowest_reflectivity_obs
    logical,  intent(in) :: apply_ref_limit_to_fwd_op
    real(r8), intent(in) :: reflectivity_limit_fwd_op
    real(r8), intent(in) :: lowest_reflectivity_fwd_op

</div>

<div class="indent1">

Check the values set in the namelist for consistency. Print out a
message if the limits and set-to values are different; this may be
intentional but is not generally expected to be the case. In all cases
below, see the namelist documentation for a fuller explanation of each
value.

  ----------------------------------- ----------------------------------------
  *apply\_ref\_limit\_to\_obs*        Logical. See [namelist](#Namelist).
  *reflectivity\_limit\_obs*          Real value. See [namelist](#Namelist).
  *lowest\_reflectivity\_obs*         Real value. See [namelist](#Namelist).
  *apply\_ref\_limit\_to\_fwd\_op*    Logical. See [namelist](#Namelist).
  *reflectivity\_limit\_fwd\_op*      Real value. See [namelist](#Namelist).
  *lowest\_reflectivity\_fwd\_op  *   Real value. See [namelist](#Namelist).
  ----------------------------------- ----------------------------------------

</div>

\
[]{#ascii_file_format}\

<div class="routine">

*is\_asciifile = ascii\_file\_format(fform)*
    logical                                :: ascii_file_format
    character(len=*), intent(in), optional :: fform

</div>

<div class="indent1">

Should be moved to DART utility module at some point. Returns .TRUE. if
the optional argument is missing or if it is not one of the following
values: *"unformatted", "UNFORMATTED", "unf", "UNF"*.

  ------------------------- ------------------------------------------
  *ascii\_file\_format  *   Return value. Logical. Default is .TRUE.
  *fform*                   Character string file format.
  ------------------------- ------------------------------------------

</div>

\
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

  ------------------ --------------------------------------------
  Contact:           Nancy Collins, David Dowell, Jeff Anderson
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ --------------------------------------------


