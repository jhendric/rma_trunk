[]{#TOP}

DART Manhattan Differences from Lanai Release Notes
===================================================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../images/Dartboard7.png){h | Index](../index.html)\            |
| eight="70"}                       | version information for this      |
|                                   | file:\                            |
|                                   | \$Id:                             |
|                                   | Manhattan\_diffs\_from\_Lanai.htm |
|                                   | l                                 |
|                                   | 11243 2017-03-08 21:35:09Z        |
|                                   | nancy@ucar.edu \$                 |
+-----------------------------------+-----------------------------------+

[Overview](#Overview) / [NetCDF Restart Files](#NetcdfRestarts) /
[Forward Operators](#ForwardOps) / [Vertical Conversion](#ObsConvert) /
[DART Diagnostics Changes](#Diagnostics) / [model\_mod.f90 Interface
Changes](#ModelMod) / [Observation Quantities](#ObsQuantity) /
[Additions/Changes to Existing Namelists](#NameListChanges) /
[Perturbations](#Perturbations) / [Terms of Use](#Legalese)
[]{#Overview}

Overview
--------

This document includes an overview of the changes in the DART system
since the Lanai release. For further details on any of these items look
at the HTML documentation for that specific part of the system.

The two most significant changes in the Manhattan version of DART are it
can support running models with a state vector larger than the memory of
a single task, removing a limit from the Lanai version of DART. It also
reads and writes NetCDF files directly instead of requiring a conversion
from one file to another. There are many other smaller changes, detailed
below.

Manhattan supported models:

-   9var
-   bgrid\_solo
-   cam-fv
-   cice
-   clm
-   cm1
-   forced\_lorenz\_96
-   ikeda
-   lorenz\_63
-   lorenz\_84
-   lorenz\_96
-   lorenz\_96\_2scale
-   lorenz\_04
-   mpas\_atm (NetCDF overwrite not supported for
    update\_u\_from\_reconstruct = .true. )
-   null\_model
-   POP
-   ROMS
-   simple\_advection
-   wrf

If your model of interest is not on the list consider checking out the
'Classic' release of DART, which is Lanai plus bug fixes and minor
enhancements. All models previously supported by Lanai are still in DART
'Classic'.

These are the major differences between the Lanai/Classic and Manhattan
releases of DART:

-   Read and write NetCDF restarts
-   Calculation of forward operators
-   Vertical conversion of observation locations
-   Diagnostic file changes
-   [State structure module](state_structure.html)
-   model\_mod interface changes
-   Observation Quantity replaces Kind
-   Perturbation of the state

[]{#NetcdfRestarts}

NetCDF Restart Files
--------------------

The programs filter and perfect\_model\_obs now read/write directly from
NetCDF files rather than having to run converters (`model_to_dart` and
`dart_to_model`). To facilitate this there is a new required call
`add_domain` which must be called during `static_init_model`. It can be
called multiple times in static\_model\_mod, e.g. once for each NetCDF
file that contains state variables. There are three ways to add a
domain:

-   **From Blank** : This is for small models such as lorenz\_96 and no
    NetCDF restarts
    -   `dom_id = add_domain(model_size)`
-   **From File** : This is for models which have NetCDF restart files
    -   `dom_id = add_domain(template_file, num_vars, var_names, ... )`
-   **From Spec** : Creates a skeleton structure for a domain (
    currently only used in bgrid\_solo )
    -   `dom_id = add_domain(num_vars, var_names, ... )`\
        `call add_dimension_to_variable(dom_id, var_id, dim_nam, dim_size)`\
        `call finished_adding_domain`

For models without NetCDF restarts, use `add_domain(model_size)`. This
is the minimum amount of information needed by DART to create a netdcf
file. For models with NetCDF restarts use
`add_domain(info_file, num_vars, var_names)` which lets DART read the
NetCDF dimensions for a list of variables from a file (`info_file`).
There are several routines that can be used together to create a domain
from a description:
`add_domain, add_dimension_to_variable, finished_adding_domain`. This
can be used in models such as bgrid\_solo where the model is spun up in
perfect\_model\_obs, but the model itself has variable structure (3D
variables with names). See [Additions/Changes to existing namelists for
how to use NetCDF IO](#namelist_changes).

**Note** when using NetCDF restarts, inflation files are NetCDF also.
The inflation mean and inflation standard deviation are in separate
files when you use NetCDF restarts. See [NetCDF inflation
files](netcdf_inflation_files.html) for details. []{#ForwardOps}

Calculation of Forward Operators
--------------------------------

The forward operator code in model\_mod now operates on an array of
state values. See [forward operator](forward_operator.html) for more
detail about distributed vs. non-distributed forward operators. In
distributed mode the forward operators for all ensemble members are
calculated in the same `model_interpolate` call. In non-distributed
mode, the forward operators for all ensemble members a task owns
(1-ens\_size) are calculated at once. []{#ObsConvert}

Vertical Conversion of Observation and State Locations
------------------------------------------------------

The vertical conversion of observation locations is done before the
assimilation by default. This can be changed by namelist options.

In Lanai this calculation is done in the assimilation as part of
`get_close_obs` if a model\_mod does vertical conversion. See [vertical
conversion](vertical_conversion.html) for details about this change.
Note that not all models do vertical conversion or even have a concept
of vertical location, but every model\_mod must have the following
routines:

    call set_vertical_localization_coord(vert_localization_coord)

    call convert_vertical_obs(ens_handle, num, locs, loc_qtys, loc_types, &
                              which_vert, status)

    call convert_vertical_state(ens_handle, num, locs, loc_qtys, loc_indx, &
                                which_vert, istatus)

If there are NOT multiple choices for a vertical coordinate (e.g.
cartesian, one dimensional), all these routines can be no-ops.

If there are multiple types of vertical coordinates, the convert
routines must be able to convert between them. The
'set\_vertical\_localization\_coord()' routine should be called from
'static\_init\_model()' to set what localization coordinate type is
being requested.

The three routines related to vertical coordinates/localization choices
are:

-   `set_vert_localization_coord` - sets the vertical localization
    coordiate (not required if there is no vertical conversion)
-   `convert_vertical_obs` - converts observation location to required
    vertical type (does nothing if there is no vertical conversion)
-   `convert_vertical_state` - converts state vector location to
    required vertical type (does nothing if there is no vertical
    conversion)

[]{#Diagnostics}

DART Diagnostic file changes
----------------------------

For large models DART format diagnostic files (Prior\_Diag.nc and
Posterior\_Diag.nc) have been replaced with separate files for each copy
that would have gone into Prior\_Diag.nc and Posterior\_Diag.nc.

For Prior\_Diag.nc:

-   **Mean and standard deviation**:\
      preassim\_mean.nc\
      preassim\_sd.nc
-   **Inflation mean and standard deviation** (if state space inflation
    is used):\
      preassim\_priorinf\_mean.nc\
      preassim\_priorinf\_sd.nc
-   **The number of ensemble members specifed** in filter\_nml
    (num\_output\_state\_members):\
      preassim\_member\_\#\#\#\#.nc

For Posterior\_Diag.nc:

-   **Mean and standard deviation**:\
      postassim\_mean.nc\
      postassim\_sd.nc
-   **Inflation mean and standard deviation** (if state space inflation
    is used):\
      postassim\_priorinf\_mean.nc\
      postassim\_priorinf\_sd.nc
-   **The number of ensemble members specifed** in filter\_nml
    (num\_output\_state\_members):\
      postassim\_member\_\#\#\#\#.nc

The `num_output_state_members` are not written separately from the
restarts. Note that restarts will have been clamped if any clamping is
applied (given as an arguement to add\_domain). This is *different* to
Posterior\_Diag.nc which contains unclamped values. Note also that there
are 2 more ["stages"](#STAGES_TO_WRITE) which might be output, in
addition to the preassim and postassim discussed here.

For models with multiple domains the filenames above are appended with
the domain number, e.g. preassim\_mean.nc becomes
preassim\_mean\_d01.nc, preassim\_mean\_d02.nc, etc.

##### Changes to nc\_write\_model\_atts

`nc_write_model_atts` now has 2 arguments:

-   ncid - open netcdf file identifier
-   domain\_id - domain number being written

The calling code will write the model state, so this routine should only
add attributes and optionally, non-state information like grid arrays.

This routine will only be called if DART is creating an output NetCDF
file from scratch. This may include any of the preassim, postassim, or
output files.

##### Changes to nc\_write\_model\_vars

`nc_write_model_vars` is currently unused (and in fact uncalled). It
remains for possible future expansion. []{#ModelMod}

model\_mod.f90 Interface Changes
--------------------------------

The model\_mod.f90 file contains all code that is specific to any
particular model. The code in this file is highly constrained since
these routines are \*called by\* other code in the DART system. All
routine interfaces -- the names, number of arguments, and the names of
those arguments -- must match the prescribed interfaces exactly. Since
not all required interfaces are needed for every model there are default
routines provided that can be referenced from a 'use' statement and then
the routine name can be put in the module 'public' list without any code
for that routine having to be written in the model\_mod.f90 file.

The following 18 routines are required:

-   static\_init\_model
-   get\_model\_size
-   get\_state\_meta\_data
-   shortest\_time\_between\_assimilations
-   model\_interpolate
-   end\_model
-   nc\_write\_model\_atts
-   nc\_write\_model\_vars
-   init\_time
-   init\_conditions
-   adv\_1step
-   pert\_model\_copies
-   get\_close\_obs
-   get\_close\_state
-   convert\_vertical\_obs
-   convert\_vertical\_state
-   read\_model\_time
-   write\_model\_time

Here is an example of code from the top of a model\_mod file, including
the modules where the default routines live and the required public
list.


    use     location_mod, only : location_type, get_close_type, &
                                 get_close_obs, get_close_state, &
                                 convert_vertical_obs, convert_vertical_state, &
                                 set_location, set_location_missing, &
                                 set_vertical_localization_coord
    use    utilities_mod, only : register_module, error_handler, &
                                 E_ERR, E_MSG
                                 ! nmlfileunit, do_output, do_nml_file, do_nml_term,  &
                                 ! find_namelist_in_file, check_namelist_read
    use netcdf_utilities_mod, only : nc_add_global_attribute, nc_synchronize_file, &
                                     nc_add_global_creation_time, &
                                     nc_begin_define_mode, nc_end_define_mode
    use state_structure_mod, only : add_domain
    use ensemble_manager_mod, only : ensemble_type
    use dart_time_io_mod, only  : read_model_time, write_model_time
    use default_model_mod, only : pert_model_copies, nc_write_model_vars

    implicit none
    private

    ! required by DART code - will be called from filter and other
    ! DART executables.  interfaces to these routines are fixed and
    ! cannot be changed in any way.
    public :: static_init_model,      &
              get_model_size,         &
              get_state_meta_data,    &
              shortest_time_between_assimilations, &
              model_interpolate,      &
              end_model,              &
              nc_write_model_atts,    &
              adv_1step,              &
              init_time,              &
              init_conditions

    ! public but in another module
    public :: nc_write_model_vars,    &
              pert_model_copies,      &
              get_close_obs,          &
              get_close_state,        &
              convert_vertical_obs,   &
              convert_vertical_state, &
              read_model_time,        &
              write_model_time

[]{#ObsQuantity}

Observation Quantity replaces Kinds
-----------------------------------

Historically there has been confusion about the terms for specific
observation types (which often include the name of the instrument
collecting the data) and the generic quantity that is being measured
(e.g. temperature). The previous terms for these were 'types' and
'kinds', respectively.

Starting with the Manhattan release we have tried to clarify the
terminology and make the interfaces consistent. The following table
lists the original names from the Lanai/Classic release and the
replacement routines in Manhattan.

All code that is part of the DART code repository has been updated to
use the replacment routines, but if you have your own utilities written
using this code, you will need to update your code. Contact us
(<dart@ucar.edu>) for help if you have any questions.


    public subroutines, existing name on left, replacement on right:

    assimilate_this_obs_kind()     =>     assimilate_this_type_of_obs(type_index)
    evaluate_this_obs_kind()       =>       evaluate_this_type_of_obs(type_index)
    use_ext_prior_this_obs_kind()  =>  use_ext_prior_this_type_of_obs(type_index)

    get_num_obs_kinds()      =>  get_num_types_of_obs()
    get_num_raw_obs_kinds()  =>  get_num_quantities()

    get_obs_kind_index()     => get_index_for_type_of_obs(type_name)
    get_obs_kind_name()      => get_name_for_type_of_obs(type_index)

    get_raw_obs_kind_index()  =>  get_index_for_quantity(quant_name)
    get_raw_obs_kind_name()   =>  get_name_for_quantity(quant_index)

    get_obs_kind_var_type()  =>  get_quantity_for_type_of_obs(type_index)

    get_obs_kind()      =>  get_obs_def_type_of_obs(obs_def)
    set_obs_def_kind()  =>  set_obs_def_type_of_obs(obs_def)

    get_kind_from_menu()      =>  get_type_of_obs_from_menu()

    read_obs_kind()     =>   read_type_of_obs_table(file_unit, file_format)
    write_obs_kind()    =>  write_type_of_obs_table(file_unit, file_format)

    maps obs_seq nums to specific type nums, only used in read_obs_seq:
    map_def_index()  => map_type_of_obs_table()  

    removed.  apparently unused, and simply calls get_obs_kind_name():
    get_obs_name()

    apparently unused anywhere, removed:
    add_wind_names()
    do_obs_form_pair()

    public integer parameter constants and subroutine formal argument names,
    old on left, new on right:

    KIND_ => QTY_
    kind => quantity

    TYPE_ => TYPE_
    type => type_of_obs

    integer parameters:
    max_obs_generic  =>  max_defined_quantities  (not currently public, leave private)
    max_obs_kinds    =>  max_defined_types_of_obs 

[]{#NamelistChanges}

Additions/Changes to Existing Namelists
---------------------------------------

#### quality\_control\_nml

These namelist options used to be in filter\_nml, now they are in
quality\_control\_nml.

<div class="namelist">

    &quality_control_nml
       input_qc_threshold          = 3,
       outlier_threshold           = 4,
       enable_special_outlier_code = .false.
    /

</div>

New namelist variables

#### filter\_nml

<div class="namelist">

    &filter_nml
       single_file_in               = .false.,
       single_file_out              = .false.,

       input_state_file_list        = 'null',
       output_state_file_list       = 'null',
       input_state_files            = 'null',
       output_state_files           = 'null',

       stages_to_write              = 'output'
       write_all_stages_at_end      = .false.
       output_restarts              = .true.
       output_mean                  = .true.
       output_sd                    = .true.

       perturb_from_single_instance = .false.,
       perturbation_amplitude       = 0.2_r8,

       distributed_state            = .true.
    /

</div>

\

<div>

Item
Type
Description
single\_file\_in
logical
True means that all of the restart and inflation information is read
from a single NetCDF file. False means that you must specify an
input\_state\_file\_list and DART will be expecting
input\_{priorinf,postinf}\_{mean,sd}.nc files for inflation.
single\_file\_out
logical
True means that all of the restart and inflation information is written
to a single NetCDF file. False means that you must specify a
output\_state\_files and DART will be output files specified in the
list. Inflation files will be written in the form
input\_{priorinf,postinf}\_{mean,sd}.nc.
input\_state\_files
character array
This is used for single file input for low order models. For multiple
domains you can specify a file for each domain. When specifying a list
single\_file\_in, single\_file\_out must be set to .true.
output\_state\_files
character array
This is used for single file input for low order models. For multiple
domains you can specify a file for each domain. When specifying a list
single\_file\_in, single\_file\_out must be set to .true.
input\_state\_file\_list
character array
A list of files containing input model restarts. For multiple domains
you can specify a file for each domain. When specifying a list
single\_file\_in, single\_file\_out must be set to .false.
output\_state\_file\_list
character array
A list of files containing output model restarts. For multiple domains
you can specify a file for each domain. When specifying a list
single\_file\_in, single\_file\_out must be set to .false.
[]{#STAGES_TO_WRITE}
stages\_to\_write
character array
Controls which stages to write. Currently there are four options:
-   `input` -- writes input mean and sd only
-   `preassim` -- before assimilation, before prior inflation is applied
-   `postassim` -- after assimilation, before posterior inflation is
    applied
-   `output` -- final output for filter which includes clamping and
    inflation

write\_all\_stages\_at\_end
logical
True means output all stages at the end of filter. This is more memory
intensive but requires less time. For larger models IO begins to
dominate the overall cost of the assimilation, so writting all stages at
the end writes more files in parallel, reducing the IO time. Filenames
are defined in `output_state_files`.
output\_restarts
logical
True means output a restart file(s). Filenames are defined in
`output_state_files`.
output\_mean
logical
True means output a restart file which contains the ensemble mean for
the stages that have been turned on in `stages_to_write`. The file name
will have the stage with *\_mean* appended.
output\_sd
logical
True means output a restart file which contains the ensemble standard
deviation for the stages that have been turned on in `stages_to_write`.
The file name will have the stage with *\_sd* appended.
perturb\_from\_single\_instance
logical
Read a single file and perturb this to create an ensemble
perturbation\_amplitude
float
Perturbation amplitude
distribute\_state
logical
True keeps the state distributed across all tasks throughout the entire
execution of filter.

</div>

**NetCDF reads and writes:**

For **input** file names:

-   give `input_state_file_list ` a file for each domain, each of which
    contains a list of restart files. An example of an 'input\_list.txt'
    might look something like :\
    <div class="namelist">

        advance_temp1/wrfinput_d01
        advance_temp2/wrfinput_d01
        advance_temp3/wrfinput_d01
        advance_temp4/wrfinput_d01
        advance_temp5/wrfinput_d01
        ....

    </div>

    \
-   if no `input_state_file_list` is provided then default filenames
    will be used e.g. input\_member\_\#\#\#\#.nc,
    input\_priorinf\_mean.nc, input\_priorinf\_sd.nc

For **output** file names:

-   give `output_state_file_list` a file for each domain, each of which
    contains a list of restart files. An example of an 'input\_list.txt'
    might for WRF might look something like :\
    <div class="namelist">

        wrf_out_d01.0001.nc
        wrf_out_d01.0002.nc
        wrf_out_d01.0003.nc
        wrf_out_d01.0004.nc
        wrf_out_d01.0005.nc
        ....

    </div>

    \
    if you would like to simply like to overwrite your previous data
    input\_list.txt = output\_list.txt
-   if no `output_state_files` is provided then default filenames will
    be used e.g. output\_member\_\#\#\#\#.nc, output\_priorinf\_mean.nc,
    output\_priorinf\_sd.nc

For small models you may want to use `single_file_in`, `single_file_out`
which contains all copies needed to run filter.

#### state\_vector\_io\_nml

<div class="namelist">

    &state_vector_io_nml
       buffer_state_io          = .false.,
       single_precision_output  = .false.,
    /

</div>

When `buffer_state_io` is `.false.` the entire state is read into memory
at once if .true. variables are read one at a time. If your model can
not fit into memory at once this must be set to `.true.` .

`single_precision_output` allows you to run filter in double precision
but write NetCDF files in single presision

#### assim\_tools\_nml

<div class="namelist">

    &assim_tools_nml
       distribute_mean  = .true.
    /

</div>

In previous DART releases, each processor gets a copy of the mean (in
ens\_mean\_for\_model). In RMA DART, the mean is distributed across all
processors. However, a user can choose to have a copy of the mean on
each processor by setting `distribute_mean = .false.` . Note that the
mean state is accessed through `get_state` whether distribute\_mean is
`.true.` or `.false.`

### Removed from existing namelists

    &filter_nml
       input_qc_threshold          = 3,
       outlier_threshold           = 4,
       enable_special_outlier_code = .false.
       start_from_restart          = .false.
       output_inflation            = .true.
       output_restart              = .true.
       /

NOTE : `output_restart` has been renamed to `output_restarts`.
**`output_inflation` is no longer supported** and only writes inflation
files if `inf_flavor > 1`

    &ensemble_manager_nml
       single_restart_file_out = .true.
       perturbation_amplitude  = 0.2,
       /

    &assim_manager_nml
       write_binary_restart_files = .true.,
       netCDF_large_file_support  = .false.
       /

[]{#Perturbations}

Perturbations
-------------

The option to perturb one ensemble member to produce an ensemble is in
filter\_nml:`perturb_from_single_instance`. The model\_mod interface is
now `pert_model_copies` not `pert_model_state`. Each task perturbs every
ensemble member for its own subsection of state. This is more
complicated than the Lanai routine `pert_model_state`, where a whole
state vector is available. If a model\_mod does not provide a perturb
interface, filter will do the perturbing with an amplitude set in
filter\_nml:perturbation\_amplitude. Note the perturb namelist options
have been removed from ensemble\_manager\_nml []{#Legalese}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Terms of Use
------------

DART software - Copyright UCAR. This open source software is provided by
UCAR, "as is", without charge, subject to all terms of use at
<http://www.image.ucar.edu/DAReS/DART/DART_download>

  ------------------ ------------------------------------------------------------------------------------------------------------------
  Contact:           DART core group
  Revision:          \$Revision: 11299 \$
  Source:            \$URL: https://svn-dares-dart.cgd.ucar.edu/DART/branches/rma\_trunk/documentation/documentation/html/rma.html \$
  Change Date:       \$Date: 2017-03-10 16:45:23 -0700 (Fri, 10 Mar 2017) \$
  Change history:    try "svn log" or "svn diff"
  ------------------ ------------------------------------------------------------------------------------------------------------------


