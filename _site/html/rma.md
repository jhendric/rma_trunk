[]{#TOP}

RMA notes
=========

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../images/Dartboard7.png){h | Index](../index.html)\            |
| eight="70"}                       | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

In the RMA version of DART, the state vector is not required to be
stored completely on any process. This is achieved using Remote Memory
Access (RMA). The RMA programing model allows processes to read (and
write) memory on other processors asynchronously. RMA DART supported
models:

-   9var
-   bgrid\_solo
-   cam-fv
-   cice
-   cm1
-   lorenz\_04
-   lorenz\_63
-   lorenz\_84
-   lorenz\_96
-   mpas\_atm (NetCDF overwrite not supported for
    update\_u\_from\_reconstruct = .true. )
-   POP
-   ROMS
-   wrf

There are six major differences between Lanai and RMA DART:

-   Read and write NetCDF restarts
-   Calculation of forward operators
-   Vertical conversion of observation locations
-   Diagnostic file changes
-   [State structure module](state_structure.html)
-   Perturbation of the state

Before bitwise testing with Lanai please read [bitwise
considerations](bitwise_considerations.html)

#### NetCDF Restarts

The programs filter and perfect\_model\_obs now read/write directly from
NetCDF files, rather than having to run converters (`model_to_dart` and
`dart_to_model`). To facilitate this, there is a new required call
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
files](netcdf_inflation_files.html) for details.

#### Calculation of forward operators

The forward operator code in model\_mod now operates on an array of
state values. See [forward operator](forward_operator.html) for more
detail about distributed vs. non-distributed forward operators. In
distributed mode the forward operators for all ensemble members are
calculated in the same `model_interpolate` call. In non-distributed
mode, the forward oeprators for all ensemble members a task owns
(1-ens\_size) are calculated at once.

#### Vertical conversion of observation locations

The vertical conversion of observation locations is done before the
assimilation. In Lanai this calculation is done in the assimilation as
part of `get_close_obs` if a model\_mod does vertical conversion. See
[vertical conversion](vertical_conversion.html) for details about this
change. Note that not all models do vertical conversion or even have a
concept of vertical location, but every model\_mod must have the
following routines:

-   `query_vert_localization_coord` - returns the vertical localization
    coordiate (or does nothing if there is no vertical conversion)
-   `vert_convert` - converts location to required vertical (or does
    nothing if there is no vertical conversion)

#### Diagnostic file changes

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

`nc_write_model_atts` has an new argument
'`model_mod_writes_state_variables`'. This is used to communicate to
DART whether the model will create and write state variables in
Prior\_Diag.nc and Posterior\_Diag.nc. If
`model_model_writes_state_variables = .false.` DART will define and
write state variables to the new diagnostic files. If
`model_model_writes_state_variables = .true.,  nc_write_model_vars` is
called as normal.

#### Perturbations

The option to perturb one ensemble member to produce an ensemble is in
filter\_nml:`perturb_from_single_instance`. The model\_mod interface is
now `pert_model_copies` not `pert_model_state`. Each task perturbs every
ensemble member for its own subsection of state. This is more
complicated than the Lanai routine `pert_model_state`, where a whole
state vector is available. If a model\_mod does not provide a perturb
interface, filter will do the perturbing with an amplitude set in
filter\_nml:perturbation\_amplitude. Note the perturb namelist options
have been removed from ensemble\_manager\_nml

#### state\_vector\_io\_nml

<div class="namelist">

    &state_vector_io_nml
       buffer_state_io         = .false.,
       single_precision_output = .false.,
    /

</div>

When `buffer_state_io` is `.false.` the entire state is read into memory
at once if .true. variables are read one at a time. If your model can
not fit into memory at once this must be set to `.true.` .

`single_precision_output` allows you to run filter in double precision
but write NetCDF files in single precision.

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

[]{#namelist_changes}

### Additions/Changes to existing namelists

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
output\_state\_file\_list and DART will be output files specified in the
list. Inflation files will be written in the form
input\_{priorinf,postinf}\_{mean,sd}.nc.
input\_restart\_files
character array
This is used for single file input for low order models. For multiple
domains you can specify a file for each domain. When specifying a list
single\_file\_in, single\_file\_out must be set to .true.
output\_restart\_files
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
the end writes more files in parallel, reducing the IO time.
`output_state_file_list`.
output\_restarts
logical
True means output a restart file(s). Filenames are defined in
`output_state_file_list`.
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

**For NetCDF reads and writes**

For **input** file names:

-   give `input_state_file_list ` a file for each domain, each of which
    contains a list of restart files.
-   if no `input_state_file_list` is provided then default filenames
    will be used e.g. input\_member\_000\*.nc, input\_priorinf\_mean.nc,
    input\_priorinf\_sd.nc

For **output** file names:

-   give `output_state_file_list` a file for each domain, each of which
    contains a list of restart files.
-   if no `output_state_file_list` is provided then default filenames
    will be used e.g. output\_member\_000\*.nc,
    output\_priorinf\_mean.nc, output\_priorinf\_sd.nc

For small models you may want to use `single_file_in`, `single_file_out`
which contains all copies needed to run filter.

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


