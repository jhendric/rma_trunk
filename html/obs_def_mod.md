[]{#TOP}

MODULE obs\_def\_mod
====================

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

The DART Fortran90 derived type *obs\_def* provide an abstraction of the
definition of an observation. An observation sequence *obs\_seq* at a
higher level is composed of observation definitions associated with
observed values. For now, the basic operations required to implement an
observation definition are an ability to compute a forward operator
given the model state vector, the ability to read/write the observation
definition from/to a file, and a capability to do a standard input
driven interactive definition of the observation definition.\
\
DART makes a distinction between specific *observation types* and
generic *observation quantities*. The role of the various obs\_def input
files is to define the mapping between the types and quantities, and
optionally to provide type-specific processing routines.\
\
A single obs\_def output module is created by the program *preprocess*
from two kinds of input files. First, a DEFAULT obs\_def module
(normally called *DEFAULT\_obs\_def\_mod.F90* and documented in this
directory) is used as a template into which the preprocessor
incorporates information from zero or more special obs\_def modules
(such as *obs\_def\_1d\_state\_mod.f90* or
*obs\_def\_reanalysis\_bufr\_mod.f90*, also documented in this
directory). If no special obs\_def files are included in the
preprocessor namelist, a minimal *obs\_def\_mod.f90* is created which
can only support identity forward observation operators.\
\
To add a new observation type which does not fit into any of the
already-defined obs\_def files, a new file should be created in the
*obs\_def* directory. These files are usually named according the the
pattern *obs\_def\_*X*\_mod.f90*, where the X is either an instrument
name, a data source, or a class of observations. See the existing
filenames in that directory for ideas. Then this new filename must be
listed in the *input.nml* namelist for the model, in the
*&preprocess\_nml* section, in the *input\_files* variable. This
variable is a string list type which can contain multiple filenames.
Running the *preprocess* program will then use the contents of the new
file to generate the needed output files for use in linking to the rest
of the DART system.

### Simple observations

If the new observation type can be directly interpolated by a model\_mod
interpolation routine, and has no additional observation-specific code
for reading, writing, or initializing the observation, then the entire
contents of the new file is:

    ! BEGIN DART PREPROCESS KIND LIST
    ! type, quantity, COMMON_CODE
    ! (repeat lines for each type)
    ! END DART PREPROCESS KIND LIST

DART will automatically generate all interface code needed for these new
observation types. For example, here is a real list:

    ! BEGIN DART PREPROCESS KIND LIST
    !VELOCITY,                     QTY_VELOCITY,              COMMON_CODE
    !TRACER_CONCENTRATION,         QTY_TRACER_CONCENTRATION,  COMMON_CODE
    !TRACER_SOURCE,                QTY_TRACER_SOURCE,         COMMON_CODE
    !MEAN_SOURCE,                  QTY_MEAN_SOURCE,           COMMON_CODE
    !SOURCE_PHASE,                 QTY_SOURCE_PHASE,          COMMON_CODE
    ! END DART PREPROCESS KIND LIST

The first column is the specific observation *type* and should be
unique. The second column is the generic observation *quantity* and must
match the list of known quantities in the *obs\_kind\_mod.f90*. (To add
a new quantity, the
*assimilation\_code/modules/observations/DEFAULT\_obs\_kind\_mod.F90*
file must be edited to include a unique integer identifier for the
quantity.) The third column must be the keyword *COMMON\_CODE* which
tells the *preprocess* program to automatically generate all necessary
interface code for this type.

### Observations needing special handling

For observation types which have observation-specific routines, must
interpolate using a combination of other generic quantities, or require
additional observation-specific data to be stored, the following format
is used:

    ! BEGIN DART PREPROCESS KIND LIST
    ! type, quantity
    ! (repeat lines for each type/quantity pair)
    ! END DART PREPROCESS KIND LIST

DART will need user-supplied interface code for each of the listed
types. For example, here is a real list:

    ! BEGIN DART PREPROCESS KIND LIST
    ! DOPPLER_RADIAL_VELOCITY, QTY_VELOCITY
    ! RADAR_REFLECTIVITY,      QTY_RADAR_REFLECTIVITY
    ! END DART PREPROCESS KIND LIST

In this case, DART needs additional information for how to process these
types. They include code sections delimited by precisely formatted
comments, and possibly module code sections:

1.  ! BEGIN DART PREPROCESS USE OF SPECIAL OBS_DEF MODULE
        ! END DART PREPROCESS USE OF SPECIAL OBS_DEF MODULE

    Any fortran use statements for public subroutines or variables from
    other modules should be placed between these lines, with comment
    characters in the first column.\
    \
    For example, if the forward operator code includes a module with
    public routines then a "use" statement like:

        use obs_def_1d_state_mod, only : write_1d_integral, read_1d_integral, &
                                         interactive_1d_integral, get_expected_1d_integral

    needs to be added to the obs\_def\_mod so the listed subroutines are
    available to be called. This would look like:

        ! BEGIN DART PREPROCESS USE OF SPECIAL OBS_DEF MODULE
        ! use obs_def_1d_state_mod, only : write_1d_integral, read_1d_integral, &
        !                                  interactive_1d_integral, get_expected_1d_integral
        ! END DART PREPROCESS USE OF SPECIAL OBS_DEF MODULE

2.  ! BEGIN DART PREPROCESS GET_EXPECTED_OBS_FROM_DEF
        ! END DART PREPROCESS GET_EXPECTED_OBS_FROM_DEF

    These comments must enclose a case statement for each defined type
    that returns the expected observation value based on the current
    values of the state vector. The code must be in comments, with the
    comment character in the first column.\
    \
    The variables available to be passed to subroutines or used in this
    section of code are:

      ------------------- --------------------------------------------
      *state*             the entire model state vector
      *state\_time*       the time of the state data
      *ens\_index*        the ensemble member number
      *location*          the observation location
      *obs\_kind\_ind *   the index of the specific observation type
      *obs\_time*         the time of the observation
      *error\_val*        the observation error variance
      ------------------- --------------------------------------------

    \
    The routine must fill in the values of these variables:

      ------------- ------------------------------------------------------------------
      *obs\_val *   the computed forward operator value
      *istatus*     return code: 0=ok, &gt;0 is error, &lt;0 reserved for system use
      ------------- ------------------------------------------------------------------

    \
    To call a model\_mod interpolate routine directly, the argument list
    must match exactly:

        interpolate(state, location, QTY_xxx, obs_val, istatus)

    This can be useful if the forward operator needs to retrieve values
    for fields which are typically found in a model and then compute a
    derived value from them.

3.  ! BEGIN DART PREPROCESS READ_OBS_DEF
        ! END DART PREPROCESS READ_OBS_DEF

    These comments must enclose a case statement for each defined type
    that reads any additional data associated with a single observation.
    If there is no information beyond that for the basic obs\_def type,
    the case statement must still be provided, but the code can simply
    be *continue*. The code must be in comments, with the comment
    character in the first column.\
    \
    The variables available to be passed to subroutines or used in this
    section of code are:

      ------------- -----------------------------------------------------------------------
      *ifile*       the open unit number positioned ready to read, read-only
      *obs\_def *   the rest of the obs\_def derived type for this obs, read-write
      *key*         the index observation number in this sequence, read-only
      *obs\_val*    the observation value, if needed. in general should not be changed
      *is\_ascii*   logical to indicate how the file was opened, formatted or unformatted
      ------------- -----------------------------------------------------------------------

    \
    The usual use of this routine is to read in additional metadata per
    observation and to set the private key in the *obs\_def* to indicate
    which index to use for this observation to look up the corresponding
    metadata in arrays or derived types. Do not confuse the key in the
    obs\_def with the key argument to this routine; the latter is the
    global observation sequence number for this observation.

4.  ! BEGIN DART PREPROCESS WRITE_OBS_DEF
        ! END DART PREPROCESS WRITE_OBS_DEF

    These comments must enclose a case statement for each defined type
    that writes any additional data associated with a single
    observation. If there is no information beyond that for the basic
    obs\_def type, the case statement must still be provided, but the
    code can simply be *continue*. The code must be in comments, with
    the comment character in the first column.\
    \
    The variables available to be passed to subroutines or used in this
    section of code are:

      ------------- -----------------------------------------------------------------------
      *ifile*       the open unit number positioned ready to write, read-only
      *obs\_def *   the rest of the obs\_def derived type for this obs, read-only
      *key*         the index observation number in this sequence, read-only
      *is\_ascii*   logical to indicate how the file was opened, formatted or unformatted
      ------------- -----------------------------------------------------------------------

    \
    The usual use of this routine is to write the additional metadata
    for this observation based on the private key in the *obs\_def*. Do
    not confuse this with the key in the subroutine call which is the
    observation number relative to the entire observation sequence file.

5.  ! BEGIN DART PREPROCESS INTERACTIVE_OBS_DEF
        ! END DART PREPROCESS INTERACTIVE_OBS_DEF

    These comments must enclose a case statement for each defined type
    that prompts the user for any additional data associated with a
    single observation. If there is no information beyond that for the
    basic obs\_def type, the case statement must still be provided, but
    the code can simply be *continue*. The code must be in comments,
    with the comment character in the first column.\
    \
    The variables available to be passed to subroutines or used in this
    section of code are:

      ------------- ----------------------------------------------------------------
      *obs\_def *   the rest of the obs\_def derived type for this obs, read-write
      *key*         the index observation number in this sequence, read-only
      ------------- ----------------------------------------------------------------

    \
    The DART code will prompt for the rest of the obs\_def values
    (location, type, value, error) but any additional metadata needed by
    this observation type should be prompted to, and read from, the
    console (e.g. *write(\*,\*)*, and *read(\*, \*)*). The code will
    generally set the *obs\_def%key* value as part of setting the
    metadata.

6.  ! BEGIN DART PREPROCESS MODULE CODE
        ! END DART PREPROCESS MODULE CODE

    If the code to process this observation requires module data and/or
    subroutines, then these comments must surround the module
    definitions. Unlike all the other sections, this comment pair is
    optional, and if used, the code must not be in comments; it will be
    copied verbatim over to the output file.\
    \
    Generally the code for a forward operator should be defined inside a
    module, to keep module variables and other private subroutines from
    colliding with unrelated routines and variables in other forward
    operator files.

It is possible to mix automatic code types and user-supplied code types
in the same list. Simply add the COMMON\_CODE keyword on the lines which
need no special data or interfaces. For example, here is an extract from
the 1d state obs\_def module, where the raw state variable needs only
autogenerated code, but the 1d integral has user-supplied processing
code:

    ! BEGIN DART PREPROCESS KIND LIST
    ! RAW_STATE_VARIABLE,    QTY_STATE_VARIABLE, COMMON_CODE
    ! RAW_STATE_1D_INTEGRAL, QTY_1D_INTEGRAL
    ! END DART PREPROCESS KIND LIST


    ! BEGIN DART PREPROCESS USE OF SPECIAL OBS_DEF MODULE
    !   use obs_def_1d_state_mod, only : write_1d_integral, read_1d_integral, &
    !                                    interactive_1d_integral, get_expected_1d_integral
    ! END DART PREPROCESS USE OF SPECIAL OBS_DEF MODULE

    ! BEGIN DART PREPROCESS GET_EXPECTED_OBS_FROM_DEF
    !         case(RAW_STATE_1D_INTEGRAL)                                                         
    !            call get_expected_1d_integral(state, location, obs_def%key, obs_val, istatus)  
    ! END DART PREPROCESS GET_EXPECTED_OBS_FROM_DEF

    ! BEGIN DART PREPROCESS READ_OBS_DEF
    !      case(RAW_STATE_1D_INTEGRAL)
    !         call read_1d_integral(obs_def%key, ifile, fileformat)
    ! END DART PREPROCESS READ_OBS_DEF

    ! BEGIN DART PREPROCESS WRITE_OBS_DEF
    !      case(RAW_STATE_1D_INTEGRAL)
    !         call write_1d_integral(obs_def%key, ifile, fileformat)
    ! END DART PREPROCESS WRITE_OBS_DEF

    ! BEGIN DART PREPROCESS INTERACTIVE_OBS_DEF
    !      case(RAW_STATE_1D_INTEGRAL)
    !         call interactive_1d_integral(obs_def%key)
    ! END DART PREPROCESS INTERACTIVE_OBS_DEF

    ! BEGIN DART PREPROCESS MODULE CODE
    module obs_def_1d_state_mod

    use        types_mod, only : r8
    use    utilities_mod, only : register_module, error_handler, E_ERR, E_MSG
    use     location_mod, only : location_type, set_location, get_location 
    use  assim_model_mod, only : interpolate
    use   cov_cutoff_mod, only : comp_cov_factor

    implicit none

    public :: write_1d_integral, read_1d_integral, interactive_1d_integral, &
              get_expected_1d_integral

    ...  (module code here)

    end module obs_def_1d_state_mod
    ! END DART PREPROCESS MODULE CODE

See the [obs\_def\_1d\_state\_mod](obs_def_1d_state_mod.html)
documentation for more details and examples of each section. Also see
[obs\_def\_wind\_speed\_mod.f90](obs_def_wind_speed_mod.f90) for an
example of a 3D geophysical forward operator.\
\
In addition to collecting and managing any additional observation
type-specific code, this module provides the definition of the
obs\_def\_type derived type, and a collection of subroutines for
creating, accessing, and updating this type. The remainder of this
document describes the subroutines provided by this module.

[]{#OtherModulesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    types_mod
    utilities_mod
    location_mod (depends on model choice)
    time_manager_mod
    assim_model_mod
    obs_kind_mod
    Other special obs_def_kind modules as required

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

  ----------------------------- ---------------------------------------------------------------
  *use obs\_def\_mod, only :*   [obs\_def\_type](#obs_def_type)
                                [init\_obs\_def](#init_obs_def)
                                [get\_obs\_def\_location](#get_obs_def_location)
                                [get\_obs\_def\_type\_of\_obs](#get_obs_def_type_of_obs)
                                [get\_obs\_def\_time](#get_obs_def_time)
                                [get\_obs\_def\_error\_variance](#get_obs_def_error_variance)
                                [get\_obs\_def\_key](#get_obs_def_key)
                                [set\_obs\_def\_location](#set_obs_def_location)
                                [set\_obs\_def\_type\_of\_obs](#set_obs_def_type_of_obs)
                                [set\_obs\_def\_time](#set_obs_def_time)
                                [set\_obs\_def\_error\_variance](#set_obs_def_error_variance)
                                [set\_obs\_def\_key](#set_obs_def_key)
                                [interactive\_obs\_def](#interactive_obs_def)
                                [write\_obs\_def](#write_obs_def)
                                [read\_obs\_def](#read_obs_def)
                                [get\_expected\_obs\_from\_def](#get_expected_obs_from_def)
                                [destroy\_obs\_def](#destroy_obs_def)
                                [copy\_obs\_def](#copy_obs_def)
                                [assignment(=)](#copy_obs_def)
                                [get\_name\_for\_type\_of\_obs](#get_name_for_type_of_obs)
  ----------------------------- ---------------------------------------------------------------

A note about documentation style. Optional arguments are enclosed in
brackets *\[like this\]*.

[]{#obs_def_type}\

<div class="routine">

    type obs_def_type
       private
       type(location_type)  :: location
       integer              :: kind
       type(time_type)      :: time
       real(r8)             :: error_variance
       integer              :: key
    end type obs_def_type

</div>

<div class="indent1">

Models all that is known about an observation except for actual values.
Includes a location, type, time and error variance.

  Component         Description
  ----------------- ----------------------------------------------------------
  location          Location of the observation.
  kind              Despite the name, the specific type of the observation.
  time              Time of the observation.
  error\_variance   Error variance of the observation.
  key               Unique identifier for observations of a particular type.

</div>

\
[]{#init_obs_def}\

<div class="routine">

*call init\_obs\_def(obs\_def, location, kind, time, error\_variance)*
    type(obs_def_type),  intent(out) :: obs_def
    type(location_type), intent(in)  :: location
    integer,             intent(in)  :: kind
    type(time_type),     intent(in)  :: time
    real(r8),            intent(in)  :: error_variance

</div>

<div class="indent1">

Creates an obs\_def type with location, type, time and error\_variance
specified.

  --------------------- ------------------------------------
  *obs\_def  *          The obs\_def that is created
  *location  *          Location for this obs\_def
  *kind  *              Observation type for obs\_def
  *time  *              Time for obs\_def
  *error\_variance  *   Error variance of this observation
  --------------------- ------------------------------------

</div>

\
[]{#copy_obs_def}\

<div class="routine">

*call copy\_obs\_def(obs\_def1, obs\_def2)*
    type(obs_def_type), intent(out) :: obs_def1
    type(obs_def_type), intent(in)  :: obs_def2

</div>

<div class="indent1">

Copies obs\_def2 to obs\_def1, overloaded as assignment (=).

  --------------- ----------------------------
  *obs\_def1  *   obs\_def to be copied into
  *obs\_def2  *   obs\_def to be copied from
  --------------- ----------------------------

</div>

\
[]{#get_obs_def_key}\

<div class="routine">

*var = get\_obs\_def\_key(obs\_def)*
    integer                        :: get_obs_def_key
    type(obs_def_type), intent(in) :: obs_def

</div>

<div class="indent1">

Returns key from an observation definition.

  -------------- ------------------------------
  *var  *        Returns key from an obs\_def
  *obs\_def  *   An obs\_def
  -------------- ------------------------------

</div>

\
[]{#get_obs_def_error_variance}\

<div class="routine">

*var = get\_obs\_def\_error\_variance(obs\_def)*
    real(r8)                       :: get_obs_def_error_variance
    type(obs_def_type), intent(in) :: obs_def

</div>

<div class="indent1">

Returns error variance from an observation definition.

  -------------- ---------------------------------
  *var  *        Error variance from an obs\_def
  *obs\_def  *   An obs\_def
  -------------- ---------------------------------

</div>

\
[]{#get_obs_def_location}\

<div class="routine">

*var = get\_obs\_def\_location(obs\_def)*
    type(location_type)              :: get_obs_def_location
    type(obs_def_type), intent(in)   :: obs_def

</div>

<div class="indent1">

Returns the location from an observation definition.

  -------------- -----------------------------------
  *var  *        Returns location from an obs\_def
  *obs\_def  *   An obs\_def
  -------------- -----------------------------------

</div>

\
[]{#get_obs_def_type_of_obs}\

<div class="routine">

*var = get\_obs\_def\_type\_of\_obs(obs\_def)*
    integer                         :: get_obs_def_type_of_obs
    type(obs_def_type),  intent(in) :: obs_def

</div>

<div class="indent1">

Returns an observation type from an observation definition.

  -------------- -----------------------------------------------
  *var  *        Returns the observation type from an obs\_def
  *obs\_def  *   An obs\_def
  -------------- -----------------------------------------------

</div>

\
[]{#get_obs_def_time}\

<div class="routine">

*var = get\_obs\_def\_time(obs\_def)*
    type(time_type)                :: get_obs_def_time
    type(obs_def_type), intent(in) :: obs_def

</div>

<div class="indent1">

Returns time from an observation definition.

  -------------- -------------------------------
  *var  *        Returns time from an obs\_def
  *obs\_def  *   An obs\_def
  -------------- -------------------------------

</div>

\
[]{#get_name_for_type_of_obs}\

<div class="routine">

*obs\_name = get\_name\_for\_type\_of\_obs(obs\_kind\_ind)*
    character(len = 32)            :: get_name_for_type_of_obs
    integer, intent(in)            :: obs_kind_ind

</div>

<div class="indent1">

Returns an observation name from an observation type.

  -------------------- ---------------------------------------
  *var  *              Returns name from an observation type
  *obs\_kind\_ind  *   An observation type
  -------------------- ---------------------------------------

</div>

\
[]{#set_obs_def_location}\

<div class="routine">

*call set\_obs\_def\_location(obs\_def, location)*
    type(obs_def_type),  intent(inout) :: obs_def
    type(location_type), intent(in)    :: location

</div>

<div class="indent1">

Set the location in an observation definition.

  -------------- -------------
  *obs\_def  *   An obs\_def
  *location  *   A location
  -------------- -------------

</div>

\
[]{#set_obs_def_error_variance}\

<div class="routine">

*call set\_obs\_def\_error\_variance(obs\_def, error\_variance)*
    type(obs_def_type), intent(inout) :: obs_def
    real(r8), intent(in)              :: error_variance

</div>

<div class="indent1">

Set error variance for an observation definition.

  --------------------- ----------------
  *obs\_def  *          An obs\_def
  *error\_variance  *   Error variance
  --------------------- ----------------

</div>

\
[]{#set_obs_def_key}\

<div class="routine">

*call set\_obs\_def\_key(obs\_def, key)*
    type(obs_def_type), intent(inout) :: obs_def
    integer,            intent(in)    :: key

</div>

<div class="indent1">

Set the key for an observation definition.

  -------------- ----------------------------------------
  *obs\_def  *   An obs\_def
  *key  *        Unique identifier for this observation
  -------------- ----------------------------------------

</div>

\
[]{#set_obs_def_type_of_obs}\

<div class="routine">

*call set\_obs\_def\_type\_of\_obs(obs\_def, kind)*
    type(obs_def_type), intent(inout) :: obs_def
    integer,            intent(in)    :: kind

</div>

<div class="indent1">

Set the type of observation in an observation definition.

  -------------- -----------------------------
  *obs\_def  *   An obs\_def
  *kind  *       An integer observation type
  -------------- -----------------------------

</div>

\
[]{#set_obs_def_time}\

<div class="routine">

*call set\_obs\_def\_time(obs\_def, time)*
    type(obs_def_type), intent(inout) :: obs_def
    type(time_type), intent(in)       :: time

</div>

<div class="indent1">

Sets time for an observation definition.

  -------------- -------------
  *obs\_def  *   An obs\_def
  *time  *       Time to set
  -------------- -------------

</div>

\
[]{#get_expected_obs_from_def}\

<div class="routine">

*call get\_expected\_obs\_from\_def(key, obs\_def, obs\_kind\_ind,
ens\_index, state, state\_time, obs\_val, istatus, assimilate\_this\_ob,
evaluate\_this\_ob)*
    integer,            intent(in)  :: key
    type(obs_def_type), intent(in)  :: obs_def
    integer,            intent(in)  :: obs_kind_ind
    integer,            intent(in)  :: ens_index
    real(r8),           intent(in)  :: state(:)
    type(time_type),    intent(in)  :: state_time
    real(r8),           intent(out) :: obs_val
    integer,            intent(out) :: istatus
    logical,            intent(out) :: assimilate_this_ob
    logical,            intent(out) :: evaluate_this_ob

</div>

<div class="indent1">

Compute the observation (forward) operator for a particular obs
definition.

  -------------------------- ----------------------------------------------------------------------------------------------------------------------------
  *key  *                    descriptor for observation type
  *obs\_def  *               The input obs\_def
  *obs\_kind\_ind  *         The obs type
  *ens\_index  *             The ensemble member number of this state vector
  *state  *                  Model state vector
  *state\_time  *            Time of the data in the model state vector
  *istatus  *                Returned integer describing problems with applying forward operator (0 == OK, &gt;0 == error, &lt;0 reserved for sys use).
  *assimilate\_this\_ob  *   Indicates whether to assimilate this obs or not
  *evaluate\_this\_ob  *     Indicates whether to evaluate this obs or not
  -------------------------- ----------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#read_obs_def}\

<div class="routine">

*call read\_obs\_def(ifile, obs\_def, key, obs\_val *\[,fform\]*)*
    integer,                    intent(in)    :: ifile
    type(obs_def_type),         intent(inout) :: obs_def
    integer,                    intent(in)    :: key
    real(r8),                   intent(inout) :: obs_val
    character(len=*), optional, intent(in)    :: fform

</div>

<div class="indent1">

Reads an obs\_def from file open on channel ifile. Uses format specified
in fform or FORMATTED if fform is not present.

  -------------- --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *ifile  *      File unit open to output file
  *obs\_def  *   Observation definition to be read
  *key  *        Present if unique identifier key is needed by some obs type. Unused by default code.
  *obs\_val  *   Present if needed to perform operations based on value. Unused by default code.
  *fform  *      File format specifier: FORMATTED or UNFORMATTED; default FORMATTED (FORMATTED in this case is the human readable/text option as opposed to UNFORMATTED which is binary.)
  -------------- --------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#interactive_obs_def}\

<div class="routine">

*call interactive\_obs\_def(obs\_def, key)*
    type(obs_def_type), intent(inout) :: obs_def
    integer,            intent(in)    :: key

</div>

<div class="indent1">

Creates an obs\_def via input from standard in.

  -------------- --------------------------------------------------------------------------------------
  *obs\_def  *   An obs\_def to be created
  *key  *        Present if unique identifier key is needed by some obs type. Unused by default code.
  -------------- --------------------------------------------------------------------------------------

</div>

\
[]{#write_obs_def}\

<div class="routine">

*call write\_obs\_def(ifile, obs\_def, key *\[,fform\]*)*
    integer,                    intent(in) :: ifile
    type(obs_def_type),         intent(in) :: obs_def
    integer,                    intent(in) :: key
    character(len=*), optional, intent(in) :: fform

</div>

<div class="indent1">

Writes an obs\_def to file open on channel ifile. Uses format specified
in fform or FORMATTED if fform is not present.

  -------------- --------------------------------------------------------------------------------------
  *ifile  *      File unit open to output file
  *obs\_def  *   Observation definition to be written
  *key  *        Present if unique identifier key is needed by some obs type. Unused by default code.
  *fform  *      File format specifier: FORMATTED or UNFORMATTED; default FORMATTED
  -------------- --------------------------------------------------------------------------------------

</div>

\
[]{#destroy_obs_def}\

<div class="routine">

*call destroy\_obs\_def(obs\_def)*
    type(obs_def_type), intent(inout) :: obs_def

</div>

<div class="indent1">

Releases all storage associated with an obs\_def and its subcomponents.

  -------------- -----------------------------
  *obs\_def  *   An obs\_def to be released.
  -------------- -----------------------------

</div>

\
[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   The read\_obs\_def() and write\_obs\_def() routines are passed an
    already-opened file channel/descriptor and read to or write from it.

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
get\_expected\_obs\_from\_def
Attempt to evaluate undefined observation type
An observation type for which no forward operator has been defined is an
error.
read\_obs\_def
Expected header "obdef" in input file
The format of the input file is not consistent.
read\_obs\_def
Expected kind header "kind " in input file
The format of the input file is not consistent.
read\_obs\_def
Attempt to read for undefined obs\_kind index
Reading for an observation type for which no forward operator has been
defined is an error.
write\_obs\_def
Attempt to write for undefined obs\_kind index
Writing for an observation type for which no forward operator has been
defined is an error.
interactive\_obs\_def
Attempt to interactively create undefined obs\_kind index
Creating an observation type for which no forward operator has been
defined is an error.

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


