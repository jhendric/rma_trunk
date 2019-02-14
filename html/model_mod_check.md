[]{#TOP}

program *model\_mod\_check*
===========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [MODULES](#Modules) / [FILES](#FilesUsed) /
[USAGE](#Usage) / [REFERENCES](#References) / [ERRORS](#Errors) /
[PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
--------

*model\_mod\_check* tests some of the more fundamental routines in any
*model\_mod*. This is intended to be used when adding a new model to
DART - test the pieces as they are written. As such, this program is
meant to be hacked up and customized to your own purpose. Right now, it
reads in model netCDF file(s) - one per domain/nest/whatever - and
writes out files, queries the metdata, etc. It also exercises
*static\_init\_model()*, which is the first routine to get right ...

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

    &model_mod_check 
       num_ens               = 1
       single_file           = .FALSE.
       input_state_files     = 'null'
       output_state_files    = 'null'
       all_metadata_file     = 'metadata.txt'

       test1thru             = 7
       run_tests             = -1

       x_ind                 = -1
       loc_of_interest       = -1.0, -1.0, -1.0
       quantity_of_interest  = 'NONE'

       interp_test_dlon      = 10.0
       interp_test_dlat      = 10.0
       interp_test_dvert     = 10.0

       interp_test_lonrange  = 0.0, 120.0
       interp_test_latrange  = 0.0, 120.0
       interp_test_vertrange = 0.0, 100.0

       interp_test_dx        = -888888.0
       interp_test_dy        = -888888.0
       interp_test_dz        = -888888.0

       interp_test_xrange    = -888888.0, -888888.0
       interp_test_yrange    = -888888.0, -888888.0
       interp_test_zrange    = -888888.0, -888888.0

       interp_test_vertcoord = 'VERTISHEIGHT'
       verbose               = .FALSE.
       /

</div>

\
\

<div>

+-----------------------+-----------------------+-----------------------+
| Item                  | Type                  | Description           |
+=======================+=======================+=======================+
| num\_ens              | integer               | Provided for future   |
|                       |                       | use. Must be 1.       |
|                       |                       | Ultimately, The       |
|                       |                       | number of ensemble    |
|                       |                       | members you would     |
|                       |                       | like to read in for   |
|                       |                       | testing.              |
+-----------------------+-----------------------+-----------------------+
| single\_file          | logical               | If .TRUE. all members |
|                       |                       | are stored in a       |
|                       |                       | single restart file.  |
+-----------------------+-----------------------+-----------------------+
| input\_state\_files(: | character(len=256)    | The name(s) of the    |
| )                     |                       | NetCDF file(s)        |
|                       |                       | containing the model  |
|                       |                       | states, one per       |
|                       |                       | domain. If num\_ens   |
|                       |                       | &gt; 1 and not        |
|                       |                       | single\_file, specify |
|                       |                       | a filename for each   |
|                       |                       | ensemble member       |
|                       |                       | (num\_ens). If you    |
|                       |                       | have both multiple    |
|                       |                       | ensemble members in   |
|                       |                       | separate files AND    |
|                       |                       | multiple domains,     |
|                       |                       | specify all the       |
|                       |                       | ensemble member       |
|                       |                       | filenames for domain  |
|                       |                       | 1, then all the       |
|                       |                       | ensemble member       |
|                       |                       | filenames for domain  |
|                       |                       | 2, etc.               |
+-----------------------+-----------------------+-----------------------+
| output\_state\_files( | character(len=256)    | The name(s) of the    |
| :)                    |                       | output NetCDF file(s) |
|                       |                       | for testing IO, one   |
|                       |                       | per domain. If        |
|                       |                       | num\_ens &gt; 1 and   |
|                       |                       | not single\_file,     |
|                       |                       | specify a filename    |
|                       |                       | for each ensemble     |
|                       |                       | member (num\_ens). If |
|                       |                       | you have both         |
|                       |                       | multiple ensemble     |
|                       |                       | members in separate   |
|                       |                       | files AND multiple    |
|                       |                       | domains, specify all  |
|                       |                       | the ensemble member   |
|                       |                       | filenames for domain  |
|                       |                       | 1, then all the       |
|                       |                       | ensemble member       |
|                       |                       | filenames for domain  |
|                       |                       | 2, etc.               |
+-----------------------+-----------------------+-----------------------+
| all\_metadata\_file   | character(len=256)    | Test 6 produces an    |
|                       |                       | exhaustive list of    |
|                       |                       | metadata for EVERY    |
|                       |                       | element in the DART   |
|                       |                       | state vector. The     |
|                       |                       | metadata get written  |
|                       |                       | to this file name.    |
+-----------------------+-----------------------+-----------------------+
| x\_ind                | integer(i8)           | An integer index into |
|                       |                       | the DART state        |
|                       |                       | vector. This will be  |
|                       |                       | used to test the      |
|                       |                       | metadata routines.    |
|                       |                       | Answers questions     |
|                       |                       | about location, what  |
|                       |                       | variable type is      |
|                       |                       | stored there, etc.    |
+-----------------------+-----------------------+-----------------------+
| loc\_of\_interest     | real(r8),             | The lat/lon/level for |
|                       | dimension(3)          | a **particular**      |
|                       |                       | location. Used in     |
|                       |                       | Test 4, the           |
|                       |                       | single-point          |
|                       |                       | interpolation test.   |
|                       |                       | Indirectly tests the  |
|                       |                       | routine to find the   |
|                       |                       | closest gridpoint.    |
+-----------------------+-----------------------+-----------------------+
| quantity\_of\_interes | character(len=32)     | Specifies the         |
| t                     |                       | QUANTITY of the model |
|                       |                       | state to use in Tests |
|                       |                       | 4, 5, and 7.          |
+-----------------------+-----------------------+-----------------------+
| interp\_test\_dlon    | real(r8)              | The distance          |
|                       |                       | (measured in degrees) |
|                       |                       | on the longitude      |
|                       |                       | interpolation grid.   |
|                       |                       | Ignored if            |
|                       |                       | interpolating with    |
|                       |                       | cartesian             |
|                       |                       | coordinates. Used in  |
|                       |                       | Test 5.               |
+-----------------------+-----------------------+-----------------------+
| interp\_test\_dlat    | real(r8)              | The distance          |
|                       |                       | (measured in degrees) |
|                       |                       | on the latitude       |
|                       |                       | interpolation grid.   |
|                       |                       | Ignored if            |
|                       |                       | interpolating with    |
|                       |                       | cartesian             |
|                       |                       | coordinates. Used in  |
|                       |                       | Test 5.               |
+-----------------------+-----------------------+-----------------------+
| interp\_test\_dvert   | real(r8)              | The distance          |
|                       |                       | (measured in          |
|                       |                       | interp\_vertcoord) on |
|                       |                       | the vertical          |
|                       |                       | interpolation grid.   |
|                       |                       | Ignored if            |
|                       |                       | interpolating with    |
|                       |                       | cartesian             |
|                       |                       | coordinates. Used in  |
|                       |                       | Test 5.               |
+-----------------------+-----------------------+-----------------------+
| interp\_test\_lonrang | real(r8)              | The range of y to be  |
| e                     |                       | tested with           |
|                       |                       | model\_interpolate,   |
|                       |                       | with spacing          |
|                       |                       | *interp\_test\_dlon*. |
|                       |                       | Ignored if            |
|                       |                       | interpolating with    |
|                       |                       | cartesian             |
|                       |                       | coordinates. Used in  |
|                       |                       | Test 5.               |
+-----------------------+-----------------------+-----------------------+
| interp\_test\_latrang | real(r8)              | The range of y to be  |
| e                     |                       | tested with           |
|                       |                       | model\_interpolate,   |
|                       |                       | with spacing          |
|                       |                       | *interp\_test\_dlat*. |
|                       |                       | Ignored if            |
|                       |                       | interpolating with    |
|                       |                       | cartesian             |
|                       |                       | coordinates. Used in  |
|                       |                       | Test 5.               |
+-----------------------+-----------------------+-----------------------+
| interp\_test\_vertran | real(r8)              | The range in the      |
| ge                    |                       | vertical direction to |
|                       |                       | be tested with        |
|                       |                       | model\_interpolate,   |
|                       |                       | with spacing          |
|                       |                       | *interp\_test\_dvert* |
|                       |                       | .                     |
|                       |                       | Ignored if            |
|                       |                       | interpolating with    |
|                       |                       | cartesian             |
|                       |                       | coordinates. Used in  |
|                       |                       | Test 5.               |
+-----------------------+-----------------------+-----------------------+
| interp\_test\_dx      | real(r8)              | The interval on the x |
|                       |                       | axis of the           |
|                       |                       | interpolation grid.   |
|                       |                       | This is used in Test  |
|                       |                       | 5 for models with     |
|                       |                       | threed\_cartesian     |
|                       |                       | coordinates.          |
+-----------------------+-----------------------+-----------------------+
| interp\_test\_dy      | real(r8)              | The interval on the y |
|                       |                       | axis of the           |
|                       |                       | interpolation grid.   |
|                       |                       | This is used in Test  |
|                       |                       | 5 for models with     |
|                       |                       | threed\_cartesian     |
|                       |                       | coordinates.          |
+-----------------------+-----------------------+-----------------------+
| interp\_test\_dz      | real(r8)              | The interval on the z |
|                       |                       | axis of the           |
|                       |                       | interpolation grid.   |
|                       |                       | This is used in Test  |
|                       |                       | 5 for models with     |
|                       |                       | threed\_cartesian     |
|                       |                       | coordinates.          |
+-----------------------+-----------------------+-----------------------+
| interp\_test\_xrange  | real(r8)              | The range of x to be  |
|                       |                       | tested with           |
|                       |                       | model\_interpolate in |
|                       |                       | Test 5, with spacing  |
|                       |                       | *interp\_test\_dx*.   |
+-----------------------+-----------------------+-----------------------+
| interp\_test\_yrange  | real(r8)              | The range of y to be  |
|                       |                       | tested with           |
|                       |                       | model\_interpolate in |
|                       |                       | Test 5, with spacing  |
|                       |                       | *interp\_test\_dy*.   |
+-----------------------+-----------------------+-----------------------+
| interp\_test\_zrange  | real(r8)              | The range in the      |
|                       |                       | vertical direction to |
|                       |                       | be tested with        |
|                       |                       | model\_interpolate in |
|                       |                       | Test 5, with spacing  |
|                       |                       | *interp\_test\_dz*.   |
+-----------------------+-----------------------+-----------------------+
| interp\_test\_vertcoo | character(len=32)     | Specifies the         |
| rd                    |                       | vertical coordinate   |
|                       |                       | system to use during  |
|                       |                       | the interpolation     |
|                       |                       | tests. Valid values   |
|                       |                       | are:                  |
|                       |                       | 'VERTISHEIGHT','VERTI |
|                       |                       | SPRESSURE','VERTISLEV |
|                       |                       | EL',                  |
|                       |                       | and                   |
|                       |                       | 'VERTISSCALEHEIGHT'.  |
+-----------------------+-----------------------+-----------------------+
| test1thru             | integer               | If *test1thru &gt;    |
|                       |                       | 0*, specifies the     |
|                       |                       | last test to be       |
|                       |                       | performed. All tests  |
|                       |                       | get performed         |
|                       |                       | sequentially. If      |
|                       |                       | *test1thru &lt; 0*,   |
|                       |                       | *run\_tests* is used  |
|                       |                       | to specify the tests  |
|                       |                       | to perform.           |
|                       |                       |   test    summary     |
|                       |                       |   ------- ----------- |
|                       |                       | --------------------- |
|                       |                       | --------------------- |
|                       |                       | --------------------- |
|                       |                       | --------------------- |
|                       |                       | --------------------- |
|                       |                       | --------------------- |
|                       |                       | --------------        |
|                       |                       |   0       Mandatory.  |
|                       |                       | Tests *static\_init\_ |
|                       |                       | model()* by calling * |
|                       |                       | static\_init\_assim\_ |
|                       |                       | model()*. Reads *inpu |
|                       |                       | t.nml* *&model\_nml*  |
|                       |                       |   1       Tests *get\ |
|                       |                       | _model\_size()* and r |
|                       |                       | eports on the makeup  |
|                       |                       | of the DART state vec |
|                       |                       | tor.                  |
|                       |                       |   2       Reads and w |
|                       |                       | rites a restart file. |
|                       |                       |   3       Tests *get\ |
|                       |                       | _state\_meta\_data()* |
|                       |                       |  for a single index i |
|                       |                       | nto the DART state. H |
|                       |                       | elps determine if the |
|                       |                       |  state vector is cons |
|                       |                       | tructed correctly.    |
|                       |                       |   4       Tests *mode |
|                       |                       | l\_interpolate()* for |
|                       |                       |  a single point.      |
|                       |                       |   5       Tests *mode |
|                       |                       | l\_interpolate()* for |
|                       |                       |  a range of interpola |
|                       |                       | tion points.          |
|                       |                       |   6       Long, expen |
|                       |                       | sive test to return t |
|                       |                       | he metadata for every |
|                       |                       |  element of the state |
|                       |                       |  vector. May be usefu |
|                       |                       | l to decide on known  |
|                       |                       | locations for subsequ |
|                       |                       | ent testing.          |
|                       |                       |   7       Find the cl |
|                       |                       | osest gridpoint to a  |
|                       |                       | known location.       |
+-----------------------+-----------------------+-----------------------+
| run\_tests(:)         | integer               | Specifies a list of   |
|                       |                       | tests to be           |
|                       |                       | performed. Same test  |
|                       |                       | numbers as described  |
|                       |                       | in test1thru. There   |
|                       |                       | are some              |
|                       |                       | dependencies. Tests 4 |
|                       |                       | and 5 require a valid |
|                       |                       | model state - which   |
|                       |                       | is read by Test 2. If |
|                       |                       | a required test is    |
|                       |                       | not specified, the    |
|                       |                       | required test is      |
|                       |                       | enabled and run. A    |
|                       |                       | value of -1 means     |
|                       |                       | that *test1thru* will |
|                       |                       | be used.              |
+-----------------------+-----------------------+-----------------------+
| verbose               | logical               | Print extra info      |
|                       |                       | about the             |
|                       |                       | *model\_mod\_check*   |
|                       |                       | run. This is only     |
|                       |                       | used for more         |
|                       |                       | reporting during Test |
|                       |                       | 5. Be warned - it     |
|                       |                       | will generate several |
|                       |                       | lines of output for   |
|                       |                       | each point in the     |
|                       |                       | test!                 |
+-----------------------+-----------------------+-----------------------+

</div>

A more typical namelist for a single ensemble member for a model with an
outer grid and a single nested grid is shown below.

<div class="namelist">

    &model_mod_check_nml
       input_state_files     = 'dart_vector1.nc','dart_vector2.nc'
       output_state_files    = 'check_me1.nc', 'check_me2.nc'
       all_metadata_file     = 'metadata.txt'
       verbose               = .TRUE.
       test1thru             = 5
       run_tests             = -1
       loc_of_interest       = 243.72386169, 52.78578186, 10.0
       x_ind                 = 12666739
       quantity_of_interest  = 'QTY_POTENTIAL_TEMPERATURE'
       interp_test_lonrange  = 144.0, 326.0
       interp_test_dlon      = 1.0
       interp_test_latrange  = -5.0, 80.0
       interp_test_dlat      = 1.0
       interp_test_vertrange = 100.0, 11000.0
       interp_test_dvert     = 200.0
       interp_test_vertcoord = 'VERTISHEIGHT'
      /

</div>

[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    assimilation_code/location/threed_sphere/location_mod.f90
    assimilation_code/location/utilities/default_location_mod.f90
    assimilation_code/location/utilities/location_io_mod.f90
    assimilation_code/modules/assimilation/adaptive_inflate_mod.f90
    assimilation_code/modules/assimilation/assim_model_mod.f90
    assimilation_code/modules/assimilation/assim_tools_mod.f90
    assimilation_code/modules/assimilation/cov_cutoff_mod.f90
    assimilation_code/modules/assimilation/filter_mod.f90
    assimilation_code/modules/assimilation/obs_model_mod.f90
    assimilation_code/modules/assimilation/quality_control_mod.f90
    assimilation_code/modules/assimilation/reg_factor_mod.f90
    assimilation_code/modules/assimilation/sampling_error_correction_mod.f90
    assimilation_code/modules/assimilation/smoother_mod.f90
    assimilation_code/modules/io/dart_time_io_mod.f90
    assimilation_code/modules/io/direct_netcdf_mod.f90
    assimilation_code/modules/io/io_filenames_mod.f90
    assimilation_code/modules/io/state_structure_mod.f90
    assimilation_code/modules/io/state_vector_io_mod.f90
    assimilation_code/modules/observations/forward_operator_mod.f90
    assimilation_code/modules/observations/obs_kind_mod.f90
    assimilation_code/modules/observations/obs_sequence_mod.f90
    assimilation_code/modules/utilities/distributed_state_mod.f90
    assimilation_code/modules/utilities/ensemble_manager_mod.f90
    assimilation_code/modules/utilities/netcdf_utilities_mod.f90
    assimilation_code/modules/utilities/null_mpi_utilities_mod.f90
    assimilation_code/modules/utilities/null_win_mod.f90
    assimilation_code/modules/utilities/obs_impact_mod.f90
    assimilation_code/modules/utilities/options_mod.f90
    assimilation_code/modules/utilities/parse_args_mod.f90
    assimilation_code/modules/utilities/random_seq_mod.f90
    assimilation_code/modules/utilities/sort_mod.f90
    assimilation_code/modules/utilities/time_manager_mod.f90
    assimilation_code/modules/utilities/types_mod.f90
    assimilation_code/modules/utilities/utilities_mod.f90
    assimilation_code/programs/model_mod_check/model_mod_check.f90
    models/your_model_here/model_mod.f90
    models/model_mod_tools/test_interpolate_threed_sphere.f90
    models/model_mod_tools/model_check_utilities_mod.f90
    models/utilities/default_model_mod.f90
    observations/forward_operators/obs_def_mod.f90
    observations/forward_operators/obs_def_utilities_mod.f90

Items highlighted may change based on which model is being tested.

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   *input.nml* is used for *model\_mod\_check\_nml*
-   The *"input\_state\_files"* can either be a single file containing
    multiple restart files, or a single NetCDF restart file. One file
    per domain.
-   The *"output\_state\_files"* is the output netCDF files from Test 2.
    Check the attributes, values, etc.
-   *check\_me\_interptest.nc* and *check\_me\_interptest.m* are the
    result of Test 5.
-   *"all\_metadata\_file"* is the run-time output of Test 6.

[]{#Usage}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

USAGE
-----

Normal circumstances indicate that you are trying to put a new model
into DART, so to be able to build and run *model\_mod\_check*, you will
need to create a *path\_names\_model\_mod\_check* file with the
following contents:

    assimilation_code/location/threed_sphere/location_mod.f90
    assimilation_code/location/utilities/default_location_mod.f90
    assimilation_code/location/utilities/location_io_mod.f90
    assimilation_code/modules/assimilation/adaptive_inflate_mod.f90
    assimilation_code/modules/assimilation/assim_model_mod.f90
    assimilation_code/modules/assimilation/assim_tools_mod.f90
    assimilation_code/modules/assimilation/cov_cutoff_mod.f90
    assimilation_code/modules/assimilation/filter_mod.f90
    assimilation_code/modules/assimilation/obs_model_mod.f90
    assimilation_code/modules/assimilation/quality_control_mod.f90
    assimilation_code/modules/assimilation/reg_factor_mod.f90
    assimilation_code/modules/assimilation/sampling_error_correction_mod.f90
    assimilation_code/modules/assimilation/smoother_mod.f90
    assimilation_code/modules/io/dart_time_io_mod.f90
    assimilation_code/modules/io/direct_netcdf_mod.f90
    assimilation_code/modules/io/io_filenames_mod.f90
    assimilation_code/modules/io/state_structure_mod.f90
    assimilation_code/modules/io/state_vector_io_mod.f90
    assimilation_code/modules/observations/forward_operator_mod.f90
    assimilation_code/modules/observations/obs_kind_mod.f90
    assimilation_code/modules/observations/obs_sequence_mod.f90
    assimilation_code/modules/utilities/distributed_state_mod.f90
    assimilation_code/modules/utilities/ensemble_manager_mod.f90
    assimilation_code/modules/utilities/netcdf_utilities_mod.f90
    assimilation_code/modules/utilities/null_mpi_utilities_mod.f90
    assimilation_code/modules/utilities/null_win_mod.f90
    assimilation_code/modules/utilities/obs_impact_mod.f90
    assimilation_code/modules/utilities/options_mod.f90
    assimilation_code/modules/utilities/parse_args_mod.f90
    assimilation_code/modules/utilities/random_seq_mod.f90
    assimilation_code/modules/utilities/sort_mod.f90
    assimilation_code/modules/utilities/time_manager_mod.f90
    assimilation_code/modules/utilities/types_mod.f90
    assimilation_code/modules/utilities/utilities_mod.f90
    assimilation_code/programs/model_mod_check/model_mod_check.f90
    models/your_model_here/model_mod.f90
    models/model_mod_tools/test_interpolate_threed_sphere.f90
    models/utilities/default_model_mod.f90
    observations/forward_operators/obs_def_mod.f90
    observations/forward_operators/obs_def_utilities_mod.f90

as well as a *mkmf\_model\_mod\_check* script. You should be able to
look at any other *mkmf\_xxxx* script and figure out what to change.
Once they exist:\
\

<div class="unix">

    [~/DART/models/yourmodel/work] % csh mkmf_model_mod_check
    [~/DART/models/yourmodel/work] % make
    [~/DART/models/yourmodel/work] % ./model_mod_check

</div>

Unlike other DART components, you are expected to modify
*model\_mod\_check.f90* to suit your needs as you develop your
*model\_mod*. The code is roughly divided into the following categories:

1.  Check the geometry information,
2.  Read/write a restart file,
3.  Check the construction of the state vector ... i.e. the metadata,
4.  Interpolate at a single point,
5.  Interpolate for a range of points.

### Test 0. Mandatory. {#test-0.-mandatory. .indent1}

The first test in *model\_mod\_check* reads the namelist and runs
*static\_init\_model* - which generally sets the geometry of the grid,
the number of state variables and their shape, etc. Virtually everything
requires knowledge of the grid and state vector, so this block cannot be
skipped.

### Test 1. Checking the Geometry Information: {#test-1.-checking-the-geometry-information .indent1}

The first test in *model\_mod\_check* exercises a basic required
interface *get\_model\_size()*. This also generates a report on the
geometry of the grid, the number of state variables and their shape,
etc. as well as the total number of elements in the DART state vector.

### Test 2. Read/writing a restart file: {#test-2.-readwriting-a-restart-file .indent1}

This directly reads and write state variables from the model netCDF
file. This is a nice sanity check to make sure that the DART state
vector is being read in properly.

### Test 3. Check the construction of the state vector: {#test-3.-check-the-construction-of-the-state-vector .indent1}

It is critical to return the correct metadata for any given index into
the DART state vector. This code block tests the two most common
features of the metadata. As a bonus, this routine is also quite useful
to determine EXACTLY where to place your first test observation. If you
test precisely at a grid location, you should be able to really get a
handle on debugging your *model\_interpolate()* routine.

### Test 4. Test interpolation on a single point. {#test-4.-test-interpolation-on-a-single-point. .indent1}

This tests your model's interpolation routine on a single point and
returns the interpolated value. This requires that Test 2 works - it
needs a valid model state with data. Test 2 is automatically run if this
test is selected.

### Test 5. Test interpolation on a range of values. {#test-5.-test-interpolation-on-a-range-of-values. .indent1}

This tests your model's interpolation routine on a range of values
returns the interpolated grid in *check\_me\_interptest.nc* and
*check\_me\_interptest.m* which can be read in Matlab and used to
visualize the result. This requires that Test 2 works - it needs a valid
model state with data. Test 2 is automatically run if this test is
selected.

### Test 6. Exhaustively test the construction of the state vector. {#test-6.-exhaustively-test-the-construction-of-the-state-vector. .indent1}

This can be a long test, depending on the size of your state vector.
This returns the same data as in Test 3 - but *for every element* in the
state vector. The metadata are written to a file specified by
*all\_metadata\_file* and *check\_me\_interptest.m* which can be read in
Matlab and used to visualize the result.

### Test 7. Find the closest gridpoint to a test location. {#test-7.-find-the-closest-gridpoint-to-a-test-location. .indent1}

This is a good test to verify that *get\_state\_meta\_data()* and the
grid information are correct. Typically, one would put in a location
that is actually **on** the grid and see if the correct gridpoint index
is returned. Repeat the test with slightly different locations until the
next gridpoint is closer. Repeat ...

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

There are no error conditions to check. This program is intended to
demonstrate simple checks that will allow you to proceed with improving
and testing the *model\_mod*. There will be plenty of run-time errors, I
suggest compiling your code with "bounds checking" turned on - at a
minimum.

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

Expanded instructions on how to add a model, and how to methodically
test piece-by-piece.

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
  Contact:           Tim Hoar
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


