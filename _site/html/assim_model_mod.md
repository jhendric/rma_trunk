[]{#TOP}

MODULE assim\_model\_mod
========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [INTERFACES](#Interface) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[PRIVATE COMPONENTS](#PrivateComponents) / [TERMS OF USE](#Legalese)

Overview
--------

This module acts as an intermediary between DART compliant models and
the filter. At one time the assim\_model\_type, which combines a state
vector and a time\_type, was envisioned as being fundamental to how DART
views model states. This paradigm is gradually being abandoned so that
model state vectors and times are handled as separate data types. It is
important to call static\_init\_assim\_model before using routines in
assim\_model\_mod. Interfaces to work with model time stepping, restart
files, and computations about the locations of model state variables and
the distance between observations and state variables. Many of the
interfaces are passed through nearly directly to the model\_mod.

### NOTES {#notes .indent1}

A note about documentation style. Optional arguments are enclosed in
brackets *\[like this\]*.

[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

This module does not have a namelist.

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    types_mod
    location_mod (model dependent choice)
    time_manager_mod
    utilities_mod
    model_mod
    netcdf
    typeSizes (part of netcdf)

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

  --------------------------------- ----------------------------------------------------------------------
  *use assim\_model\_mod, only :*
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
  --------------------------------- ----------------------------------------------------------------------

[]{#assim_model_type}\

<div class="routine">

    type assim_model_type
       private
       real(r8), pointer   :: state_vector(:) 
       type(time_type)     :: time
       integer             :: model_size
       integer             :: copyID
    end type assim_model_type

</div>

<div class="indent1">

This type is used to represent both the state and time of a state from a
model.

  Component       Description
  --------------- -------------------------------------------------------------
  state\_vector   A one dimensional representation of the model state vector.
  time            The time of the model state.
  model\_s        Size of the model state vector.
  copyID          Not used in present implementation.

</div>

\
[]{#netcdf_file_type}\

<div class="routine">

    type netcdf_file_type
       integer             :: ncid
       integer             :: Ntimes
       integer             :: NtimesMAX
       real(r8), pointer   :: rtimes(:)
       type(time_type), pointer :: times(:)
       character(len = 80)      :: fname
    end type netcdf_file_type

</div>

<div class="indent1">

Basically, we want to keep a local mirror of the unlimited dimension
coordinate variable (i.e. time) because dynamically querying it causes
unacceptable performance degradation over "long" integrations.

  Component   Description
  ----------- -----------------------------
  ncid        The netcdf file unit id.
  Ntimes      The current working length.
  NtimesMAX   Allocated length.
  rtimes      Times as real (r8).
  times       Times as time\_types.
  fname       Netcdf file name.

</div>

\
[]{#static_init_assim_model}\

<div class="routine">

*call static\_init\_assim\_model()*

</div>

<div class="indent1">

Initializes the assim\_model class. Must be called before any other
assim\_model\_mod interfaces are used. Also calls the static
initialization for the underlying model. There are no arguments.

</div>

\
[]{#init_diag_output}\

<div class="routine">

*ncFileID = init\_diag\_output(FileName, global\_meta\_data,
copies\_of\_field\_per\_time, meta\_data\_per\_copy *\[, lagID\]*)*
    type(netcdf_file_type)          :: init_diag_output 
    character (len = *), intent(in) :: FileName 
    character (len = *), intent(in) :: global_meta_data 
    integer, intent(in)             :: copies_of_field_per_time 
    character (len = *), intent(in) :: meta_data_per_copy(copies_of_field_per_time) 
    integer, optional, intent(in)   :: lagID 

</div>

<div class="indent1">

Initializes a netCDF file for output of state space diagnostics. A
handle to the channel on which the file is opened is returned.

  ----------------------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *ncFileID*                          Identifier for the netcdf file is returned. This is not an integer unit number, but a derived type containing additional information about the opened file.
  *FileName*                          Name of file to open.
  *global\_meta\_data*                Global metadata that describes the contents of this file.
  *copies\_of\_field\_per\_time   *   Number of copies of data to be written at each time. For instance, these could be the prior ensemble members, prior ensemble mean, prior ensemble spread, posterior ensemble members, posterior spread and mean, etc..
  *meta\_data\_per\_copy*             Metadata describing each of the copies.
  *lagID*                             If using the smoother, which lag number this output is for.
  ----------------------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#get_model_size}\

<div class="routine">

*var = get\_model\_size()*
    integer :: get_model_size 

</div>

<div class="indent1">

Returns the size of the model state vector. This is a direct pass
through to the model\_mod.

</div>

\
[]{#get_closest_state_time_to}\

<div class="routine">

*var = get\_closest\_state\_time\_to(model\_time, time)*
    type(time_type)              ::  get_closest_state_time_to 
    type(time_type), intent(in)  ::  model_time 
    type(time_type), intent(in)  ::  time

</div>

<div class="indent1">

Returns the closest time that a model is capable of advancing a given
state to a specified time. For instance, what is the closest time to
12GMT 01 January, 2004 that a model state at 00GMT 01 January, 2004 can
be advanced? If the model time is past the time, the model time is
returned (new feature in releases after Hawaii).

  ------------------ ------------------------------------------------------------------
  *var*              The closest time to which the model can be advanced is returned.
  *model\_time   *   The time of a model state vector.
  *time*             A time that one would like to get close to with the model.
  ------------------ ------------------------------------------------------------------

</div>

\
[]{#get_state_meta_data}\

<div class="routine">

*call get\_state\_meta\_data()*

</div>

<div class="indent1">

Pass through to model\_mod. See model\_mod documentation for arguments
and description.

</div>

\
[]{#get_model_time}\

<div class="routine">

*var = get\_model\_time(assim\_model)*
    type(time_type)                    :: get_model_time
    type(assim_model_type), intent(in) :: assim_model

</div>

<div class="indent1">

Returns time from an assim\_model type.

  ------------------- ----------------------------------------------
  *var*               Returned time from assim\_model
  *assim\_model   *   Assim\_model type from which to extract time
  ------------------- ----------------------------------------------

</div>

\
[]{#get_model_state_vector}\

<div class="routine">

*var = get\_model\_state\_vector(assim\_model)*
    real(r8)                           :: get_model_state_vector(model_size)
    type(assim_model_type), intent(in) :: assim_model

</div>

<div class="indent1">

Returns the state vector component from an assim\_model\_type.

  ------------------- --------------------------
  *var*               Returned state vector
  *assim\_model   *   Input assim\_model\_type
  ------------------- --------------------------

</div>

\
[]{#copy_assim_model}\

<div class="routine">

*call copy\_assim\_model(model\_out, model\_in)*
    type(assim_model_type), intent(out) :: model_out
    type(assim_model_type), intent(in)  :: model_in

</div>

<div class="indent1">

Copies one assim\_model\_type to another.

  ----------------- --------------------
  *model\_out   *   Copy.
  *model\_in*       Data to be copied.
  ----------------- --------------------

</div>

\
[]{#interpolate}\

<div class="routine">

*call interpolate(x, location, loctype, obs\_vals, istatus)*
    real(r8),            intent(in)  :: x(:)
    type(location_type), intent(in)  :: location
    integer,             intent(in)  :: loctype
    real(r8),            intent(out) :: obs_vals
    integer,             intent(out) :: istatus

</div>

<div class="indent1">

Interpolates a given model state variable type to a location given the
model state vector. Nearly direct call to model\_interpolate in
model\_mod. See model\_mod for the error return values in istatus.

  --------------- ----------------------------------------------------
  *x*             Model state vector.
  *location   *   Location to which to interpolate.
  *loctype*       Type of variable to interpolate.
  *obs\_vals*     Returned interpolated value.
  *istatus*       Returned as 0 if all is well, else various errors.
  --------------- ----------------------------------------------------

</div>

\
[]{#set_model_time}\

<div class="routine">

*call set\_model\_time(assim\_model, time)*
    type(assim_model_type), intent(inout) :: assim_model
    type(time_type), intent(in)           :: time

</div>

<div class="indent1">

Sets the time in an assim\_model\_type.

  ------------------- ------------------------------------------
  *assim\_model   *   Set the time in this assim\_model\_type.
  *time*              Set to this time
  ------------------- ------------------------------------------

</div>

\
[]{#set_model_state_vector}\

<div class="routine">

*call set\_model\_state\_vector(assim\_model, state)*
    type(assim_model_type), intent(inout) :: assim_model
    real(r8), intent(in)                  :: state(:)

</div>

<div class="indent1">

Set the state in an assim\_model\_type.

  ------------------- --------------------------------------------------
  *assim\_model   *   Set the state vector in this assim\_model\_type.
  *state*             The state vector to be inserted.
  ------------------- --------------------------------------------------

</div>

\
[]{#write_state_restart}\

<div class="routine">

*call write\_state\_restart(assim\_model, funit *\[, target\_time\]*)*
    type(assim_model_type),    intent(in) :: assim_model
    integer,                   intent(in) :: funit
    type(time_type), optional, intent(in) :: target_time

</div>

<div class="indent1">

Writes a restart from an assim\_model\_type with an optional
target\_time.

  ------------------- --------------------------------------------------------------------
  *assim\_model   *   Write a restart from this assim\_model\_type.
  *funit*             Integer file unit id open for output of restart files.
  *target\_time*      If present, put this target time at the front of the restart file.
  ------------------- --------------------------------------------------------------------

</div>

\
[]{#read_state_restart}\

<div class="routine">

*call read\_state\_restart(assim\_model, funit *\[, target\_time\]*)*
    type(assim_model_type),    intent(out) :: assim_model
    integer,                   intent(in)  :: funit
    type(time_type), optional, intent(out) :: target_time

</div>

<div class="indent1">

Read a state restart file into assim\_model\_type. Optionally read a
prepended target time.

  ------------------- ----------------------------------------------------------------------
  *assim\_model   *   Read the time and state vector from restart into this.
  *funit*             File id that has been opened for reading restart files.
  *target\_time*      If present, read a target time from the front of the file into this.
  ------------------- ----------------------------------------------------------------------

</div>

\
[]{#output_diagnostics}\

<div class="routine">

*call output\_diagnostics(ndFileID, state *\[, copy\_index\]*)*
    type(netcdf_file_type), intent(inout) :: ndFileID
    type(assim_model_type), intent(in)    :: state
    integer, optional,      intent(in)    :: copy_index

</div>

<div class="indent1">

Writes one copy of the state time and vector to a netCDF file.

  ------------------ -------------------------------------
  *ndFileID*         An identifier for a netCDF file
  *state*            State vector and time
  *copy\_index   *   Which copy of state is to be output
  ------------------ -------------------------------------

</div>

\
[]{#end_assim_model}\

<div class="routine">

*call end\_assim\_model()*

</div>

<div class="indent1">

Called to clean-up at end of assim\_model use. For now just passes
through to model\_mod.

</div>

\
[]{#input_diagnostics}\

<div class="routine">

*call input\_diagnostics(file\_id, state, copy\_index)*
    integer,                intent(in)    :: file_id
    type(assim_model_type), intent(inout) :: state
    integer,                intent(out)   :: copy_index

</div>

<div class="indent1">

Used to read in a particular copy of the state vector from an open state
diagnostics file.

  ------------------ ------------------------------------------------------------------------
  *file\_id*         Integer descriptor (channel number) for a diagnostics file being read.
  *state*            Assim\_model\_type to read in data.
  *copy\_index   *   Which copy of state to be read.
  ------------------ ------------------------------------------------------------------------

</div>

\
[]{#init_diag_input}\

<div class="routine">

*var = init\_diag\_input(file\_name, global\_meta\_data, model\_size,
copies\_of\_field\_per\_time)*
    integer                       :: init_diag_input
    character(len=*), intent(in)  :: file_name
    character(len=*), intent(out) :: global_meta_data
    integer,          intent(out) :: model_size
    integer,          intent(out) :: copies_of_field_per_time

</div>

<div class="indent1">

Opens a state diagnostic file and reads the global meta data, model
size, and number of data copies.

  ----------------------------------- ----------------------------------------------------
  *var*                               Returns the unit number on which the file is open.
  *file\_name*                        File name of state diagnostic file.
  *global\_meta\_data*                Global metadata string from file.
  *model\_size*                       Size of model.
  *copies\_of\_field\_per\_time   *   Number of copies of the state vector at each time.
  ----------------------------------- ----------------------------------------------------

</div>

\
[]{#init_assim_model}\

<div class="routine">

*call init\_assim\_model(state)*
    type(assim_model_type), intent(inout) :: state

</div>

<div class="indent1">

Creates storage for an assim\_model\_type.

  ------------ ---------------------------------------------------
  *state   *   An assim\_model\_type that needs storage created.
  ------------ ---------------------------------------------------

</div>

\
[]{#get_diag_input_copy_meta_data}\

<div class="routine">

*call get\_diag\_input\_copy\_meta\_data(file\_id, model\_size\_out,
num\_copies, location, meta\_data\_per\_copy)*
    integer,             intent(in)  :: file_id
    integer,             intent(in)  :: model_size_out
    integer,             intent(in)  :: num_copies
    type(location_type), intent(out) :: location(model_size_out)
    character(len = *)               :: meta_data_per_copy(num_copies)

</div>

<div class="indent1">

Reads meta-data describing state vectors in a state diagnostics file.
Given the file, the model\_size, and the number of copies, returns the
locations of each state variable and the text description of each copy.

  ---------------------------- -----------------------------------------------------------
  *file\_id*                   Integer channel open to state diagostic file being read
  *Model\_size\_out*           model size
  *num\_copies*                Number of copies of state in file
  *location*                   Returned locations for state vector
  *meta\_data\_per\_copy   *   Meta data describing what is in each copy of state vector
  ---------------------------- -----------------------------------------------------------

</div>

\
[]{#finalize_diag_output}\

<div class="routine">

*var = finalize\_diag\_output(ncFileID)*
    integer                               :: finalize_diag_output
    type(netcdf_file_type), intent(inout) :: ncFileID

</div>

<div class="indent1">

Used to complete writing on and open netcdf file. An error return is
provided for passing to the netcdf error handling routines.

  --------------- ---------------------------------
  *var*           Returns an error value.
  *ncFileID   *   Netcdf file id of an open file.
  --------------- ---------------------------------

</div>

\
[]{#aread_state_restart}\

<div class="routine">

*call aread\_state\_restart(model\_time, model\_state, funit
*\[, target\_time\]*)*
    type(time_type),           intent(out) :: model_time
    real(r8),                  intent(out) :: model_state(:)
    integer,                   intent(in)  :: funit
    type(time_type), optional, intent(out) :: target_time

</div>

<div class="indent1">

Reads a model time and state, and optionally a prepended target time,
from a state restart file.

  ------------------- -------------------------------------------------------------------
  *model\_time*       Returned time of model state
  *model\_state   *   Returned model state.
  *funit*             Channel open for reading a state restart file.
  *target\_time*      If present, this time is read from the front of the restart file.
  ------------------- -------------------------------------------------------------------

</div>

\
[]{#aoutput_diagnostics}\

<div class="routine">

*call aoutput\_diagnostics(ncFileID, model\_time, model\_state *\[,
copy\_index\]*)*
    type(netcdf_file_type), intent(inout) :: ncFileID
    type(time_type),        intent(in)    :: model_time
    real(r8),               intent(in)    :: model_state(:)
    integer, optional,      intent(in)    :: copy_index

</div>

<div class="indent1">

Write a state vector to a state diagnostics netcdf file.

  ------------------- ----------------------------------------------------------------
  *ncFileID*          Unit for a state vector netcdf file open for output.
  *model\_time*       The time of the state to be output
  *model\_state   *   A model state vector to be output.
  *copy\_index*       Which copy of state vector is to be written, default is copy 1
  ------------------- ----------------------------------------------------------------

</div>

\
[]{#awrite_state_restart}\

<div class="routine">

*call awrite\_state\_restart(model\_time, model\_state, funit *\[,
target\_time\]*)*
    type(time_type),           intent(in) :: model_time
    real(r8),                  intent(in) :: model_state(:)
    integer,                   intent(in) :: funit
    type(time_type), optional, intent(in) :: target_time

</div>

<div class="indent1">

Writes a model time and state vector to a restart file and optionally
prepends a target time.

  ------------------- ----------------------------------------------------------
  *model\_time*       Time of model state.
  *model\_state*      Model state vector.
  *funit*             Channel of file open for restart output.
  *target\_time   *   If present, time to be prepended to state time / vector.
  ------------------- ----------------------------------------------------------

</div>

\
[]{#pert_model_state}\

<div class="routine">

*call pert\_model\_state()*

</div>

<div class="indent1">

Passes through to pert\_model\_state in model\_mod. See model\_mod
documentation for arguments and details.

</div>

\
[]{#nc_append_time}\

<div class="routine">

*var = nc\_append\_time(ncFileID, time)*
    integer                               :: nc_append_time
    type(netcdf_file_type), intent(inout) :: ncFileID
    type(time_type),        intent(in)    :: time

</div>

<div class="indent1">

Appends the time to the time coordinate variable of the netcdf file. The
new length of the time variable is returned. Requires that time is a
coordinate variable AND it is the unlimited dimension.

  --------------- ----------------------------------------
  *var*           Returns new length of time variable.
  *ncFileID   *   Points to open netcdf file.
  *time*          The next time to be added to the file.
  --------------- ----------------------------------------

</div>

\
[]{#nc_write_calendar_atts}\

<div class="routine">

*var = nc\_write\_calendar\_atts(ncFileID, TimeVarID)*
    integer                            :: nc_write_calendar_atts
    type(netcdf_file_type), intent(in) :: ncFileID
    integer,                intent(in) :: TimeVarID

</div>

<div class="indent1">

Sets up the metadata for the appropriate calendar being used in the time
manager an writes it to a netcdf file.

  ---------------- -----------------------------------------------------
  *var*            Returns a netcdf error code.
  *ncFileID*       Netcdf file id pointing to a file open for writing.
  *TimeVarID   *   The index of the time variable in the netcdf file.
  ---------------- -----------------------------------------------------

</div>

\
[]{#nc_get_tindex}\

<div class="routine">

*var = nc\_get\_tindex(ncFileID, statetime)*
    integer                               :: nc_get_tindex
    type(netcdf_file_type), intent(inout) :: ncFileID
    type(time_type),        intent(in)    :: statetime

</div>

<div class="indent1">

Returns the index of a time from the time variable in a netcdf file.
This function has been replaced with more efficient approaches and may
be deleted from future releases.

  ---------------- -------------------------------------------
  *var*            The index of the time in the netcdf file.
  *ncFileID*       File id for an open netcdf file.
  *statetime   *   The time to be found in the netcdf file.
  ---------------- -------------------------------------------

</div>

\
[]{#get_model_time_step}\

<div class="routine">

*var = get\_model\_time\_step()*
    type(time_type) :: get_model_time_step

</div>

<div class="indent1">

This passes through to model\_mod. See model\_mod documentation for
arguments and details.

  ---------- -----------------------------
  *var   *   Returns time step of model.
  ---------- -----------------------------

</div>

\
[]{#open_restart_read}\

<div class="routine">

*var = open\_restart\_read(file\_name)*
    integer                      :: open_restart_read
    character(len=*), intent(in) :: file_name

</div>

<div class="indent1">

Opens a restart file for readig.

  ----------------- ----------------------------------------------
  *var*             Returns a file descriptor (channel number).
  *file\_name   *   Name of restart file to be open for reading.
  ----------------- ----------------------------------------------

</div>

\
[]{#open_restart_write}\

<div class="routine">

*var = open\_restart\_write(file\_name)*
    integer                      :: open_restart_write
    character(len=*), intent(in) :: file_name

</div>

<div class="indent1">

Open a restart file for writing.

  ----------------- ---------------------------------------------------------
  *var*             Returns a file descriptor (channel) for a restart file.
  *file\_name   *   File name of restart file to be opened.
  ----------------- ---------------------------------------------------------

</div>

\
[]{#close_restart}\

<div class="routine">

*call close\_restart(file\_unit)*
    integer, intent(in) :: file_unit

</div>

<div class="indent1">

Closes a restart file.

  ----------------- --------------------------------------------------------
  *file\_unit   *   File descriptor (channel number) of open restart file.
  ----------------- --------------------------------------------------------

</div>

\
[]{#adv_1step}\

<div class="routine">

*call adv\_1step()*

</div>

<div class="indent1">

Advances a model by one step. Pass through to model\_mod. See model\_mod
documentation for arguments and details.

</div>

\
[]{#get_initial_conditions}\

<div class="routine">

*call get\_initial\_condition(time, x)*
    type(time_type), intent(out) :: time
    real(r8),        intent(out) :: x

</div>

<div class="indent1">

Obtains an initial condition from models that support this option.

  ----------- -----------------------------------
  *time   *   the valid time of the model state
  *x*         the initial model state
  ----------- -----------------------------------

</div>

\
[]{#ens_mean_for_model}\

<div class="routine">

*call ens\_mean\_for\_model(ens\_mean)*
    type(r8), intent(in) :: ens_mean(:)

</div>

<div class="indent1">

An array of length model\_size containing the ensemble means. This is a
direct pass through to the model\_mod.

  ---------------- -------------------------------------------------------------------------------------
  *ens\_mean   *   Array of length model\_size containing the mean for each entry in the state vector.
  ---------------- -------------------------------------------------------------------------------------

</div>

\
[]{#get_close_maxdist_init}\

<div class="routine">

*call get\_close\_maxdist\_init(gc, maxdist)*
    type(get_close_type), intent(inout) :: gc
    type(r8), intent(in)                :: maxdist

</div>

<div class="indent1">

Sets the threshold distance. Anything closer than this is deemed to be
close. This is a direct pass through to the model\_mod, which in turn
can pass through to the location\_mod.

  -------------- ---------------------------------------------------------
  *gc*           Data for efficiently finding close locations.
  *maxdist   *   Anything closer than this distance is a close location.
  -------------- ---------------------------------------------------------

</div>

\
[]{#get_close_obs}\

<div class="routine">

*call get\_close\_obs(gc, base\_obs\_loc, base\_obs\_kind, obs,
obs\_kind, num\_close, close\_ind *\[, dist\]*)*
    type(get_close_type), intent(in)  :: gc
    type(location_type),  intent(in)  :: base_obs_loc
    integer,              intent(in)  :: base_obs_kind
    type(location_type),  intent(in)  :: obs(:)
    integer,              intent(in)  :: obs_kind(:)
    integer,              intent(out) :: num_close
    integer,              intent(out) :: close_ind(:)
    real(r8),  optional,  intent(out) :: dist(:)

</div>

<div class="indent1">

Given a single location and a list of other locations, returns the
indices of all the locations close to the single one along with the
number of these and the distances for the close ones. The observation
kinds are passed in to allow more sophisticated distance computations to
be done if needed. This is a direct pass through to the model\_mod,
which in turn can pass through to the location\_mod.

  ---------------------- ------------------------------------------------------------------------------
  *gc*                   Data for efficiently finding close locations.
  *base\_obs\_loc*       Single given location.
  *base\_obs\_kind   *   Kind of the single location.
  *obs*                  List of observations from which close ones are to be found.
  *obs\_kind*            Kind associated with observations in obs list.
  *num\_close*           Number of observations close to the given location.
  *close\_ind*           Indices of those locations that are close.
  *dist*                 Distance between given location and the close ones identified in close\_ind.
  ---------------------- ------------------------------------------------------------------------------

</div>

\
[]{#get_close_obs_init}\

<div class="routine">

*call get\_close\_obs\_init(gc, num, obs)*
    type(get_close_type), intent(inout) :: gc
    integer,              intent(in)    :: num
    type(location_type),  intent(in)    :: obs(:)

</div>

<div class="indent1">

Initialize storage for efficient identification of locations close to a
given location. Allocates storage for keeping track of which 'box' each
observation in the list is in. This is a direct pass through to the
model\_mod, which in turn can pass through to the location\_mod.

  ---------- --------------------------------------------------------------------------
  *gc*       Data for efficiently finding close locations.
  *num*      The number of locations in the list.
  *obs   *   The location of each element in the list, not used in 1D implementation.
  ---------- --------------------------------------------------------------------------

</div>

\
[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

  filename          purpose/comment
  ----------------- --------------------------------------------------
  filter\_restart   specified in &filter\_nml:restart\_in\_filename
  filter\_restart   specified in &filter\_nml:restart\_out\_filename
  input.nml         to read namelists

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
init\_diag\_output
Compiler does not support required kinds of variables.
NetCDF-f90 interface function byteSizeOK returned FALSE
init\_diag\_output and various nc\_XXX
various NetCDF-f90 messages
Returned by one of the NetCDF calls in this subroutine. Consult the
NetCDF manual.
get\_diag\_input\_copy\_meta\_data
expected to read "locat" got ...
The header of the metadata for the copies of the data in diagnostic
input file is not = 'locat'
set\_model\_state\_vector
state vector has length \# model size (\#) does not match.
Check your model resolution and fields included in the state vector.
aread\_state\_restart
read error is : \#
Unable to read model state from assim\_model\_state\_ic\# file. \# is
error condition retured by read statement.
open\_restart\_read
OPEN status was \#
Failed to open file listed for reason \#.
aoutput\_diagnostics
model time (d,s) (\#,\#) is index \# in ncFileID \#
Time index for file listed is &lt; 0
ainput\_diagnostics
expected "copy", got \_\_\_\_\_'
Trying to read diagnostic state output header.
nc\_append\_time
"time" expected to be rank-1
ndims /= 1\
The time array of the NetCDF file should be 1-dimensional
nc\_append\_time
unlimited dimension expected to be slowest-moving
dimids(1) /= unlimitedDimID
nc\_append\_time
time mirror and netcdf file time dimension out-of-sync
lngth /= ncFileId%Ntimes
nc\_append\_time
various NetCDF-f90 error messages
Returned from one of the NetCDF calls in this subroutine. Consult the
NetCDF manual.
nc\_get\_tindex
trouble deep ... can go no farther. Stopping.
timeindex &lt; -1
nc\_get\_tindex
Model time preceeds earliest netCDF time.
Time of current assim\_model is earlier than all the times on the NetCDF
file to which the state is to be written by aoutput\_diagnostics.
nc\_get\_tindex
subsequent netCDF time (days, seconds) \# \#
Time of current assim\_model is in the midst of the times on the NetCDF
file to which the state is to be written by aoutput\_diagnostics, but
doesn't match any of them. Very bad.
nc\_get\_tindex
various NetCDF-f90 error messages
Returned from one of the NetCDF calls in this subroutine. Consult the
NetCDF manual.
nc\_write\_calendar\_atts
various NetCDF-f90 error messages
Returned from one of the NetCDF calls in this subroutine. Consult the
NetCDF manual.

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


