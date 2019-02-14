[]{#TOP}

MODULE model\_mod
=================

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

Every model that is DART compliant must provide an set of interfaces
that will be called by DART code. For models which have no special code
for some of these routines, they can pass through the call to this
default module, which satisfies the call but does no work. To use these
routines in a *model\_mod.f90*, add at the top:\

    use default_model_mod, only : xxx, yyy

and then leave them in the public list.

[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

The default routines have no namelist.

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    types_mod
    time_manager_mod
    location_mod
    utilities_mod
    netcdf_utilities_mod
    ensemble_manager_mod
    dart_time_io_mod

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

  -------------------------- --------------------------------------------------------------------------------
  *use model\_mod, only :*   [get\_model\_size](#get_model_size)
                             [adv\_1step](#adv_1step)
                             [get\_state\_meta\_data](#get_state_meta_data)
                             [model\_interpolate](#model_interpolate)
                             [shortest\_time\_between\_assimilations](#shortest_time_between_assimilations)
                             [static\_init\_model](#static_init_model)
                             [init\_time](#init_time)
                             [fail\_init\_time](#fail_init_time)
                             [init\_conditions](#init_conditions)
                             [fail\_init\_conditions](#fail_init_conditions)
                             [nc\_write\_model\_atts](#nc_write_model_atts)
                             [nc\_write\_model\_vars](#nc_write_model_vars)
                             [pert\_model\_copies](#pert_model_copies)
                             [get\_close\_obs](#get_close_obs)
                             [get\_close\_state](#get_close_state)
                             [convert\_vertical\_obs](#convert_vertical_obs)
                             [convert\_vertical\_state](#convert_vertical_state)
                             [read\_model\_time](#read_model_time)
                             [write\_model\_time](#write_model_time)
                             [end\_model](#end_model)
  -------------------------- --------------------------------------------------------------------------------

A note about documentation style. Optional arguments are enclosed in
brackets *\[like this\]*.

[]{#get_model_size}\

<div class="routine">

*model\_size = get\_model\_size( )*
    integer(i8) :: get_model_size

</div>

<div class="indent1">

Returns the length of the model state vector as 1. Probably not what you
want. The model\_mod should set this to the right size and not use this
routine.

  --------------- ---------------------------------------
  *model\_size*   The length of the model state vector.
  --------------- ---------------------------------------

</div>

\
[]{#adv_1step}\

<div class="routine">

*call adv\_1step(x, time)*
    real(r8), dimension(:), intent(inout) :: x
    type(time_type),        intent(in)    :: time

</div>

<div class="indent1">

Throws a fatal error. If the model\_mod can advance the model it should
provide a real routine. This default routine is intended for use by
models which cannot advance themselves from inside filter.

  -------- -------------------------------------
  *x*      State vector of length model\_size.
  *time*   Current time of model state.
  -------- -------------------------------------

</div>

\
[]{#get_state_meta_data}\

<div class="routine">

*call get\_state\_meta\_data (index\_in, location, *\[, var\_type\]* )*
    integer,             intent(in)  :: index_in
    type(location_type), intent(out) :: location
    integer, optional,   intent(out) ::  var_type 

</div>

<div class="indent1">

Sets the location to missing and the variable type to 0. The model\_mod
should provide a routine that sets a real location and a state vector
type for the requested item in the state vector.

  ------------- ---------------------------------------------------------------------
  *index\_in*   Index of state vector element about which information is requested.
  *location*    The location of state variable element.
  *var\_type*   The generic quantity of the state variable element.
  ------------- ---------------------------------------------------------------------

</div>

\
[]{#model_interpolate}\

<div class="routine">

*call model\_interpolate(state\_handle, ens\_size, location,
obs\_quantity, expected\_obs, istatus)*
    type(ensemble_type),    intent(in)  :: state_handle
    integer,                intent(in)  :: ens_size
    type(location_type),    intent(in)  :: location
    integer,                intent(in)  :: obs_quantity
    real(r8),               intent(out) :: expected_obs(ens_size)
    integer,                intent(out) :: istatus(ens_size)

</div>

<div class="indent1">

Sets the expected obs to missing and returns an error code for all obs.
This routine should be supplied by the model\_mod.

  ----------------- -----------------------------------------------------------------------------------------------------------------------
  *state\_handle*   The handle to the state structure containing information about the state vector about which information is requested.
  *ens\_size*       The ensemble size.
  *location*        Location to which to interpolate.
  *obs\_quantity*   Quantity of state field to be interpolated.
  *expected\_obs*   The interpolated values from the model.
  *istatus*         Integer values return 0 for success. Other positive values can be defined for various failures.
  ----------------- -----------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#shortest_time_between_assimilations}\

<div class="routine">

*var = shortest\_time\_between\_assimilations()*
    type(time_type) :: shortest_time_between_assimilations

</div>

<div class="indent1">

Returns 1 day.

  ------- -------------------------------------
  *var*   Smallest advance time of the model.
  ------- -------------------------------------

</div>

\
[]{#static_init_model}\

<div class="routine">

*call static\_init\_model()*

</div>

<div class="indent1">

Does nothing.

</div>

\
[]{#init_time}\

<div class="routine">

*call init\_time(time)*
    type(time_type), intent(out) :: time

</div>

<div class="indent1">

Returns a time of 0.

  -------- ---------------------
  *time*   Initial model time.
  -------- ---------------------

</div>

\
[]{#fail_init_time}\

<div class="routine">

*call fail\_init\_time(time)*
    type(time_type), intent(out) :: time

</div>

<div class="indent1">

Throws a fatal error. This is appropriate for models that cannot start
from arbitrary initial conditions.

  -------- ------------------------------
  *time*   NOT SET. Initial model time.
  -------- ------------------------------

</div>

\
[]{#init_conditions}\

<div class="routine">

*call init\_conditions(x)*
    real(r8), dimension(:), intent(out) :: x

</div>

<div class="indent1">

Returns x(:) = 0.0

  ----- --------------------------------------
  *x*   Initial conditions for state vector.
  ----- --------------------------------------

</div>

\
[]{#fail_init_conditions}\

<div class="routine">

*call fail\_init\_conditions(x)*
    real(r8), dimension(:), intent(out) :: x

</div>

<div class="indent1">

Throws a fatal error. This is appropriate for models that cannot start
from arbitrary initial conditions.

  ----- -----------------------------------------------
  *x*   NOT SET: Initial conditions for state vector.
  ----- -----------------------------------------------

</div>

\
[]{#nc_write_model_atts}\

<div class="routine">

*call nc\_write\_model\_atts(ncFileID, domain\_id)*
    integer, intent(in) :: ncFileID
    integer, intent(in) :: domain_id

</div>

<div class="indent1">

Does nothing.

  -------------- --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *ncFileID*     Integer file descriptor to previously-opened netCDF file.
  *domain\_id*   integer describing the domain (which can be a nesting level, a component model ...) Models with nested grids are decomposed into 'domains' in DART. The concept is extended to refer to 'coupled' models where one model component may be the atmosphere, another component may be the ocean, or land, or ionosphere ... these would be referenced as different domains.
  -------------- --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#nc_write_model_vars}\

<div class="routine">

*call nc\_write\_model\_vars(ncFileID, domain\_id, state\_ens\_handle
*\[, memberindex\]* *\[, timeindex\]*)*
    integer,             intent(in) :: ncFileID
    integer,             intent(in) :: domain_id
    type(ensemble_type), intent(in) :: state_ens_handle
    integer, optional,   intent(in) :: memberindex
    integer, optional,   intent(in) :: timeindex

</div>

<div class="indent1">

Does nothing

  ---------------------- -----------------------------------------------------------------------------------------------------------------------
  *ncFileID*             file descriptor to previously-opened netCDF file.
  *domain\_id*           integer describing the domain (which can be a nesting level, a component model ...)
  *state\_ens\_handle*   The handle to the state structure containing information about the state vector about which information is requested.
  *memberindex*          Integer index of ensemble member to be written.
  *timeindex*            The timestep counter for the given state.
  ---------------------- -----------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#pert_model_copies}\

<div class="routine">

*call pert\_model\_copies(state\_ens\_handle, ens\_size, pert\_amp,
interf\_provided)*
    type(ensemble_type), intent(inout) :: state_ens_handle
    integer,             intent(in)    :: ens_size
    real(r8),            intent(in)    :: pert_amp
    logical,             intent(out)   :: interf_provided

</div>

<div class="indent1">

Returns 'interface provided' flag as false, so the default perturb
routine in DART will add small amounts of gaussian noise to all parts of
the state vector.

  ---------------------- -------------------------------------------------------------------------------------------------------
  *state\_ens\_handle*   The handle containing an ensemble of state vectors to be perturbed.
  *ens\_size*            The number of ensemble members to perturb.
  *pert\_amp*            the amplitude of the perturbations. The interpretation is based on the model-specific implementation.
  *interf\_provided*     Returns false if model\_mod cannot do this, else true.
  ---------------------- -------------------------------------------------------------------------------------------------------

</div>

\
[]{#get_close_obs}\

<div class="routine">

*call get\_close\_obs(gc, base\_loc, base\_type, locs, loc\_qtys,
loc\_types, num\_close, close\_ind *\[, dist\] \[, state\_handle*)*
    type(get_close_type),          intent(in)  :: gc
    type(location_type),           intent(in)  :: base_loc
    integer,                       intent(in)  :: base_type
    type(location_type),           intent(in)  :: locs(:)
    integer,                       intent(in)  :: loc_qtys(:)
    integer,                       intent(in)  :: loc_types(:)
    integer,                       intent(out) :: num_close
    integer,                       intent(out) :: close_ind(:)
    real(r8),            optional, intent(out) :: dist(:)
    type(ensemble_type), optional, intent(in)  :: state_handle

</div>

<div class="indent1">

Passes the call through to the location module code.

  ----------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *gc*              The get\_close\_type which stores precomputed information about the locations to speed up searching
  *base\_loc*       Reference location. The distances will be computed between this location and every other location in the obs list
  *base\_type*      The DART quantity at the *base\_loc*
  *locs(:)*         Compute the distance between the *base\_loc* and each of the locations in this list
  *loc\_qtys(:)*    The corresponding quantity of each item in the *locs* list
  *loc\_types(:)*   The corresponding type of each item in the *locs* list. This is not available in the default implementation but may be used in custom implementations.
  *num\_close*      The number of items from the *locs* list which are within maxdist of the base location
  *close\_ind(:)*   The list of index numbers from the *locs* list which are within maxdist of the base location
  *dist(:)*         If present, return the distance between each entry in the close\_ind list and the base location. If not present, all items in the obs list which are closer than maxdist will be added to the list but the overhead of computing the exact distances will be skipped.
  *state\_handle*   The handle to the state structure containing information about the state vector about which information is requested.
  ----------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#get_close_state}\

<div class="routine">

*call get\_close\_state(gc, base\_loc, base\_type, state\_loc,
state\_qtys, state\_indx, num\_close, close\_ind, dist, state\_handle*)
    type(get_close_type), intent(in)    :: gc
    type(location_type),  intent(inout) :: base_loc
    integer,              intent(in)    :: base_type
    type(location_type),  intent(inout) :: state_loc(:)
    integer,              intent(in)    :: state_qtys(:)
    integer(i8),          intent(in)    :: state_indx(:)
    integer,              intent(out)   :: num_close
    integer,              intent(out)   :: close_ind(:)
    real(r8),             intent(out)   :: dist(:)
    type(ensemble_type),  intent(in)    :: state_handle

</div>

<div class="indent1">

Passes the call through to the location module code.

  ------------------ ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *gc*               The get\_close\_type which stores precomputed information about the locations to speed up searching
  *base\_loc*        Reference location. The distances will be computed between this location and every other location in the obs list
  *base\_type*       The DART quantity at the *base\_loc*
  *state\_loc(:)*    Compute the distance between the *base\_loc* and each of the locations in this list
  *state\_qtys(:)*   The corresponding quantity of each item in the *state\_loc* list
  *state\_indx(:)*   The corresponding DART index of each item in the *state\_loc* list. This is not available in the default implementation but may be used in custom implementations.
  *num\_close*       The number of items from the *state\_loc* list which are within maxdist of the base location
  *close\_ind(:)*    The list of index numbers from the *state\_loc* list which are within maxdist of the base location
  *dist(:)*          If present, return the distance between each entry in the *close\_ind* list and the base location. If not present, all items in the *state\_loc* list which are closer than maxdist will be added to the list but the overhead of computing the exact distances will be skipped.
  *state\_handle*    The handle to the state structure containing information about the state vector about which information is requested.
  ------------------ ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#convert_vertical_obs}\

<div class="routine">

*call convert\_vertical\_obs(state\_handle, num, locs, loc\_qtys,
loc\_types, which\_vert, status)*
    type(ensemble_type), intent(in)  :: state_handle
    integer,             intent(in)  :: num
    type(location_type), intent(in)  :: locs(:)
    integer,             intent(in)  :: loc_qtys(:)
    integer,             intent(in)  :: loc_types(:)
    integer,             intent(in)  :: which_vert
    integer,             intent(out) :: status(:)

</div>

<div class="indent1">

Passes the call through to the location module code.

  ----------------- -------------------------------------------------------------------------------------------------------------------------------------------
  *state\_handle*   The handle to the state.
  *num*             the number of observation locations
  *locs*            the array of observation locations
  *loc\_qtys*       the array of observation quantities.
  *loc\_types*      the array of observation types.
  *which\_vert*     the desired vertical coordinate system. There is a table in the *location\_mod.f90* that relates integers to vertical coordinate systems.
  *status*          Success or failure of the vertical conversion. If *istatus = 0*, the conversion was a success. Any other value is a failure.
  ----------------- -------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#convert_vertical_state}\

<div class="routine">

*call convert\_vertical\_state(state\_handle, num, locs, loc\_qtys,
loc\_types, which\_vert, status)*
    type(ensemble_type), intent(in)  :: state_handle
    integer,             intent(in)  :: num
    type(location_type), intent(in)  :: locs(:)
    integer,             intent(in)  :: loc_qtys(:)
    integer,             intent(in)  :: loc_types(:)
    integer,             intent(in)  :: which_vert
    integer,             intent(out) :: status(:)

</div>

<div class="indent1">

Passes the call through to the location module code.

  ----------------- -------------------------------------------------------------------------------------------------------------------------------------------
  *state\_handle*   The handle to the state.
  *num*             the number of state locations
  *locs*            the array of state locations
  *loc\_qtys*       the array of state quantities.
  *loc\_types*      the array of state types.
  *which\_vert*     the desired vertical coordinate system. There is a table in the *location\_mod.f90* that relates integers to vertical coordinate systems.
  *status*          Success or failure of the vertical conversion. If *istatus = 0*, the conversion was a success. Any other value is a failure.
  ----------------- -------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#read_model_time}\

<div class="routine">

*model\_time = read\_model\_time(filename)*
    character(len=*), intent(in) :: filename
    type(time_type)              :: model_time

</div>

<div class="indent1">

Passes the call through to the dart\_time\_io module code.

  --------------- --------------------------------------
  *filename*      netCDF file name
  *model\_time*   The current time of the model state.
  --------------- --------------------------------------

</div>

\
[]{#write_model_time}\

<div class="routine">

*call write\_model\_time(ncid, dart\_time)*
    integer,          intent(in) :: ncid
    type(time_type),  intent(in) :: dart_time

</div>

<div class="indent1">

Passes the call through to the dart\_time\_io module code.

  -------------- --------------------------------------
  *ncid*         handle to an open netCDF file
  *dart\_time*   The current time of the model state.
  -------------- --------------------------------------

</div>

\
[]{#end_model}\

<div class="routine">

*call end\_model()*

</div>

<div class="indent1">

Does nothing.

</div>

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

[]{#FilesUsed}

FILES
-----

none

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

Standard errors.

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

none at this time.

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

  ----------------- -----------------------------
  Contact:          your\_name\_here
  Revision:         \$Revision\$
  Source:           \$URL\$
  Change Date:      \$Date\$
  Change history:   try "svn log" or "svn diff"
  ----------------- -----------------------------


