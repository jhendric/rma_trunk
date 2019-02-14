[]{#TOP}

MODULE *obs\_def\_1d\_state\_mod*
=================================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../documentation/images/ | Index](../../documentation/index. |
| Dartboard7.png){height="70"}      | html)\                            |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[INTERFACES](#Interface) / [NAMELIST](#Namelist) / [MODULES](#Modules) /
[FILES](#FilesUsed) / [REFERENCES](#References) / [ERRORS](#Errors) /
[PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
--------

There is a flexible mechanism built into the DART framework for
defining, at compile time, the list of observation types to be supported
by the executables. This can be changed at any time by adding or
removing items from a namelist and rebuilding the programs. The special
obs\_def module being described here is used by the program *preprocess*
to insert appropriate code sections into a [DEFAULT\_obs\_def
module](DEFAULT_obs_def_mod.html) and a [DEFAULT\_obs\_kind
module](../../assimilation_code/modules/observations/DEFAULT_obs_kind_mod.html)
to generate *obs\_def\_mod.f90* and *obs\_kind\_mod.f90* that are used
by *filter* and other DART programs. This module is intended to provide
a prototype for those developing more complicated specialized
observation definition modules.\
\
This is an extended format Fortran 90 module that provides the
definition for observation types designed for use with idealized
low-order models that use the 1D location module and can be thought of
as having a state vector that is equally spaced on a 1D cyclic domain.
The three observation types are: a straight linear interpolation to a
point on a \[0,1\] domain called a RAW\_STATE\_VARIABLE; the
RAW\_STATE\_VAR\_POWER is the interpolated RAW\_STATE\_VARIABLE raised
to a real-valued power; an area-weighted 'integral' of the state
variable over some part of the cyclic 1D domain called
RAW\_STATE\_1D\_INTEGRAL. The second type is convenient for studying
non-gaussian, non-linear assimilation problems. The third type can be
used to do idealized studies related to remote sensing observations that
are best thought of as weighted integrals of some quantity over a finite
volume.\
\
The RAW\_STATE\_1D\_INTEGRAL has an associated half\_width and
localization type (see the [cov\_cutoff
module](../../assimilation_code/modules/assimilation/cov_cutoff_mod.html)
documentation) and a number of points at which to compute the associated
integral by quadrature. The location of the observation defines the
center of mass of the integral. The integral is centered around the
location and extends outward on each side to 2\*half\_width. The weight
associated with the integral is defined by the weight of the
localization function (for instance Gaspari Cohn) using the same
localization options as defined by the cov\_cutoff module. The number of
points are used to equally divide the range for computing the integral
by quadrature.\
\
This module was partly motivated to provide a prototype for those
developing more complicated specialized observation definition modules.
Special observation modules like this contain Fortran 90 code PLUS
additional specially formatted commented code that is used to guide the
preprocess program in constructing the obs\_def\_mod.f90 and
obs\_kind\_mod.f90. These specially formatted comments are most
conveniently placed at the beginning of the module and comprise seven
sections, each beginning and ending with a special F90 comment line that
must be included VERBATIM.\
\
The seven sections and their specific instances for the
1d\_raw\_state\_mod are:

1.  A list of all observation types defined by this module and their
    associated generic quantities (see obs\_kind module). The header
    line is followed by lines that have the observation type name (an
    all caps Fortran 90 identifier) and their associated generic
    quantity identifier from the obs\_kind module. If there is no
    special processing needed for an observation type and no additional
    data needed beyond the standard contents of an observation then a
    third word on the line, *COMMON\_CODE*, will instruct the preprocess
    program to automatically generate all stubs and code needed for this
    type. For observation types needing special code or additional data,
    this word should not be specified and the user must supply the code
    manually.

        ! BEGIN DART PREPROCESS KIND LIST 
        ! RAW_STATE_VARIABLE,    QTY_STATE_VARIABLE,   COMMON_CODE
        ! RAW_STATE_1D_INTEGRAL, QTY_1D_INTEGRAL 
        ! END DART PREPROCESS KIND LIST 

    \
    \

2.  A list of all the use statements that the completed
    obs\_def\_mod.f90 must have in order to use the public interfaces
    provided by this special obs\_def module. This section is optional
    if there are no external interfaces.

        ! BEGIN DART PREPROCESS USE OF SPECIAL OBS_DEF MODULE 
        !   use obs_def_1d_state_mod, only : write_1d_integral, read_1d_integral,  &
        !                                    interactive_1d_integral, get_expected_1d_integral, &
        !                                    set_1d_integral
        ! END DART PREPROCESS USE OF SPECIAL OBS_DEF MODULE 

    \
    \

3.  Case statement entries for each observation type defined by this
    special obs\_def module stating how to compute the forward
    observation operator. Not permitted if COMMON\_CODE was specified
    (because the case statement will be automatically generated),
    otherwise there must be a case statement entry for each type of
    observation.

        ! BEGIN DART PREPROCESS GET_EXPECTED_OBS_FROM_DEF
        !         case(RAW_STATE_1D_INTEGRAL) 
        !            call get_expected_1d_integral(state, location, obs_def%key, obs_val, istatus) 
        ! END DART PREPROCESS GET_EXPECTED_OBS_FROM_DEF 

    \
    \

4.  Case statement entries for each observation type defined by this
    special obs\_def module stating how to read any extra required
    information from an obs sequence file. Not permitted if COMMON\_CODE
    was specified (because the case statement will be automatically
    generated), otherwise there must be a case statement entry for each
    type of observation defined even if no special action is required.
    (In that case put a 'continue' statement as the body of the case
    instead of a subroutine call.)

        ! BEGIN DART PREPROCESS READ_OBS_DEF 
        !      case(RAW_STATE_1D_INTEGRAL) 
        !         call read_1d_integral(obs_def%key, ifile, fform) 
        ! END DART PREPROCESS READ_OBS_DEF 

    \
    \

5.  Case statement entries for each observation type defined by this
    special obs\_def module stating how to write any extra required
    information to an obs sequence file. Not permitted if COMMON\_CODE
    was specified (because the case statement will be automatically
    generated), otherwise there must be a case statement entry for each
    type of observation defined even if no special action is required.
    (In that case put a 'continue' statement as the body of the case
    instead of a subroutine call.)

        ! BEGIN DART PREPROCESS WRITE_OBS_DEF 
        !      case(RAW_STATE_1D_INTEGRAL) 
        !         call write_1d_integral(obs_def%key, ifile, fform) 
        ! END DART PREPROCESS WRITE_OBS_DEF 

    \
    \

6.  Case statement entries for each observation type defined by this
    special obs\_def module stating how to interactively create any
    extra required information. Not permitted if COMMON\_CODE was
    specified (because the case statement will be automatically
    generated), otherwise there must be a case statement entry for each
    type of observation defined even if no special action is required.
    (In that case put a 'continue' statement as the body of the case
    instead of a subroutine call.)

        ! BEGIN DART PREPROCESS INTERACTIVE_OBS_DEF 
        !      case(RAW_STATE_1D_INTEGRAL) 
        !         call interactive_1d_integral(obs_def%key) 
        ! END DART PREPROCESS INTERACTIVE_OBS_DEF 

    \
    \

7.  Any executable F90 module code must be tagged with the following
    comments. All lines between these markers will be copied, verbatim,
    to obs\_def\_mod.f90. This section is not required if there are no
    observation-specific subroutines.

        ! BEGIN DART PREPROCESS MODULE CODE
        module obs_def_1d_state_mod

        ... (module executable code)

        end module obs_def_1d_state_mod
        ! END DART PREPROCESS MODULE CODE

    \
    \

\
[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    types_mod
    utilities_mod
    location_mod (1d_location_mod_only)
    time_manager_mod
    assim_model_mod
    cov_cutoff_mod

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

  ----------------------------- ----------------------------------------------------------
  *use obs\_def\_mod, only :*   [write\_1d\_integral](#write_1d_integral)
                                [read\_1d\_integral](#read_1d_integral)
                                [interactive\_1d\_integral](#interactive_1d_integral)
                                [get\_expected\_1d\_integral](#get_expected_1d_integral)
                                [set\_1d\_integral](#set_1d_integral)
                                [write\_power](#write_power)
                                [read\_power](#read_power)
                                [interactive\_power](#interactive_power)
                                [get\_expected\_power](#get_expected_power)
                                [set\_power](#set_power)
  ----------------------------- ----------------------------------------------------------

[]{#write_1d_integral}\

<div class="routine">

*call write\_1d\_integral(igrkey, ifile, fform)*
    integer,          intent(in) :: igrkey
    integer,          intent(in) :: ifile
    character(len=*), intent(in) :: fform

</div>

<div class="indent1">

Writes out the extra information for observation with unique identifier
key for a 1d\_integral observation type. This includes the half-width,
localization type and number of quadrature points for this observation.

  ------------ -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *igrkey  *   Unique integer key associated with the 1d integral observation being processed. This is not the same as the key that all types of observations have and uniquely distinguishes all observations from each other; this is a key that is only set and retrieved by this code for 1d integral observations. It is stored in the obs\_def derived type, not in the main obs\_type definition.
  *ifile  *    Unit number on which observation sequence file is open
  *fform  *    String noting whether file is opened for 'formatted' or 'unformatted' IO.
  ------------ -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#read_1d_integral}\

<div class="routine">

*call read\_1d\_integral(igrkey, ifile, fform)*
    integer,          intent(out) :: igrkey
    integer,          intent(in)  :: ifile
    character(len=*), intent(in)  :: fform

</div>

<div class="indent1">

Reads the extra information for observation with unique identifier key
for a 1d\_integral observation type. This information includes the
half-width, localization type and number of quadrature points for this
observation. The key that is returned is uniquely associated with the
definition that has been created and is used by this module to keep
track of the associated parameters for this observation.

  ------------ ---------------------------------------------------------------------------
  *igrkey  *   Unique integer key associated with the observation being processed.
  *ifile  *    Unit number on which observation sequence file is open
  *fform  *    String noting whether file is opened for 'formatted' or 'unformatted' IO.
  ------------ ---------------------------------------------------------------------------

</div>

\
[]{#interactive_1d_integral}\

<div class="routine">

*call interactive\_1d\_integral(igrkey)*
    integer, intent(out) :: igrkey

</div>

<div class="indent1">

Uses input from standard in to define the characteristics of a 1D
integral observation. The key that is returned is uniquely associated
with the definition that has been created and can be used by this module
to keep track of the associated parameters (half\_width, localization
option, number of quadrature points) for this key.

  ------------ -------------------------------------------------------------------------------------------
  *igrkey  *   Unique identifier associated with the created observation definition in the obs sequence.
  ------------ -------------------------------------------------------------------------------------------

</div>

\
[]{#get_expected_1d_integral}\

<div class="routine">

*call get\_expected\_1d\_integral(state, location, igrkey, val,
istatus)*
    real(r8), intent(in)            :: state
    type(location_type), intent(in) :: location
    integer, intent(in)             :: igrkey
    real(r8), intent(out)           :: val
    integer, intent(out)            :: istatus

</div>

<div class="indent1">

Computes the forward observation operator for a 1d integral observation.
Calls the *interpolate()* routine multiple times to invoke the forward
operator code in whatever model this has been compiled with.

  -------------- ----------------------------------------------------------------------------------------------------------------------------------------
  *state  *      Model state vector (or extended state vector).
  *location  *   Location of this observation.
  *igrkey  *     Unique integer key associated with this observation.
  *val  *        Returned value of forward observation operator.
  *istatus  *    Returns 0 if forward operator was successfully computed, else returns a positive value. (Negative values are reserved for system use.)
  -------------- ----------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#set_1d_integral}\

<div class="routine">

*call set\_1d\_integral(integral\_half\_width, num\_eval\_pts,
localize\_type, igrkey, istatus)*
    real(r8), intent(in)  :: integral_half_width
    integer,  intent(in)  :: num_eval_pts
    integer,  intent(in)  :: localize_type
    integer,  intent(out) :: igrkey
    integer,  intent(out) :: istatus

</div>

<div class="indent1">

Available for use by programs that create observations to set the
additional metadata for these observation types. This information
includes the integral half-width, localization type and number of
quadrature points for this observation. The key that is returned is
uniquely associated with the definition that has been created and should
be set in the obs\_def structure by calling *set\_obs\_def\_key()*. This
key is different from the main observation key which all observation
types have. This key is unique to this observation type and is used when
reading in the observation sequence to match the corresponding metadata
with each observation of this type.

  --------------------------- ----------------------------------------------------------------------
  *integral\_half\_width  *   Real value setting the half-width of the integral.
  *num\_eval\_pts  *          Integer, number of evaluation points. 5-20 recommended.
  *localize\_type  *          Integer localization type: 1=Gaspari-Cohn; 2=Boxcar; 3=Ramped Boxcar
  *igrkey  *                  Unique integer key associated with the observation being processed.
  *istatus  *                 Return code. 0 means success, any other value is an error
  --------------------------- ----------------------------------------------------------------------

</div>

\
[]{#write_power}\

<div class="routine">

*call write\_power(powkey, ifile, fform)*
    integer,          intent(in) :: powkey
    integer,          intent(in) :: ifile
    character(len=*), intent(in) :: fform

</div>

<div class="indent1">

Writes out the extra information, the power, for observation with unique
identifier key for a power observation type.

  ------------ -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *powkey  *   Unique integer key associated with the power observation being processed. This is not the same as the key that all types of observations have and uniquely distinguishes all observations from each other; this is a key that is only set and retrieved by this code for power observations. It is stored in the obs\_def derived type, not in the main obs\_type definition.
  *ifile  *    Unit number on which observation sequence file is open
  *fform  *    String noting whether file is opened for 'formatted' or 'unformatted' IO.
  ------------ -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#read_power}\

<div class="routine">

*call read\_power(powkey, ifile, fform)*
    integer,          intent(out) :: powkey
    integer,          intent(in)  :: ifile
    character(len=*), intent(in)  :: fform

</div>

<div class="indent1">

Reads the extra information, the power, for observation with unique
identifier key for a power observation type. The key that is returned is
uniquely associated with the definition that has been created and is
used by this module to keep track of the associated parameters for this
observation.

  ------------ ---------------------------------------------------------------------------
  *powkey  *   Unique integer key associated with the observation being processed.
  *ifile  *    Unit number on which observation sequence file is open
  *fform  *    String noting whether file is opened for 'formatted' or 'unformatted' IO.
  ------------ ---------------------------------------------------------------------------

</div>

\
[]{#interactive_power}\

<div class="routine">

*call interactive\_power(powkey)*
    integer, intent(out) :: powkey

</div>

<div class="indent1">

Uses input from standard in to define the characteristics of a power
observation. The key that is returned is uniquely associated with the
definition that has been created and can be used by this module to keep
track of the associated parameter, the power, for this key.

  ------------ -------------------------------------------------------------------------------------------
  *powkey  *   Unique identifier associated with the created observation definition in the obs sequence.
  ------------ -------------------------------------------------------------------------------------------

</div>

\
[]{#get_expected_power}\

<div class="routine">

*call get\_expected\_power(state, location, powkey, val, istatus)*
    real(r8), intent(in)            :: state
    type(location_type), intent(in) :: location
    integer, intent(in)             :: powkey
    real(r8), intent(out)           :: val
    integer, intent(out)            :: istatus

</div>

<div class="indent1">

Computes the forward observation operator for a power observation. Calls
the *interpolate()* routine to invoke the forward operator code in
whatever model this has been compiled with, then raises the result to
the specified power associated with this powkey.

  -------------- ----------------------------------------------------------------------------------------------------------------------------------------
  *state  *      Model state vector (or extended state vector).
  *location  *   Location of this observation.
  *powkey  *     Unique integer key associated with this observation.
  *val  *        Returned value of forward observation operator.
  *istatus  *    Returns 0 if forward operator was successfully computed, else returns a positive value. (Negative values are reserved for system use.)
  -------------- ----------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#set_power}\

<div class="routine">

*call set\_power(power\_in, powkey, istatus)*
    real(r8), intent(in)  :: power_in
    integer,  intent(out) :: powkey
    integer,  intent(out) :: istatus

</div>

<div class="indent1">

Available for use by programs that create observations to set the
additional metadata for these observation types. This information
includes the power to which to raise the state variable. The key that is
returned is uniquely associated with the definition that has been
created and should be set in the obs\_def structure by calling
*set\_obs\_def\_key()*. This key is different from the main observation
key which all observation types have. This key is unique to this
observation type and is used when reading in the observation sequence to
match the corresponding metadata with each observation of this type.

  --------------- ---------------------------------------------------------------------
  *power\_in  *   Real value setting the power.
  *powkey  *      Unique integer key associated with the observation being processed.
  *istatus  *     Return code. 0 means success, any other value is an error
  --------------- ---------------------------------------------------------------------

</div>

\
[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

This module has no namelist.

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   NONE

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

1.  none

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
interactive\_1d\_integral
Out of space, max\_1d\_integral\_obs limit NNNN (currently 1000).
There is only room for a fixed number of 1d integral observations. The
max number is defined by max\_1d\_integral\_obs. Set this to a larger
value if more are needed.

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


