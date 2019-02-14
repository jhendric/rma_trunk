[]{#TOP}

MODULE model\_mod (WRF)
=======================

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

DART interface module for the WRF model. This page documents the details
of the module compiled into DART that interfaces with the WRF data in
the state vector. There is additional overview and tutorial
documentation for running a WRF/DART assimilation on this web page:

<http://www.image.ucar.edu/wrfdart/tutorial>

The *model\_mod* reads WRF netCDF files directly to acquire the model
state data. The *wrf\_to\_dart* and *dart\_to\_wrf* programs are no
longer necessary.\
\
A netCDF file named *wrfinput\_d01* is required and must be at the same
resolution and have the same surface elevation data as the files
converted to create the DART initial conditions. No data will be read
from this file, but the grid information must match exactly.

The model interface code supports WRF configurations with multiple
domains. Data for all domains is read into the DART state vector. During
the computation of the forward operators (getting the estimated
observation values from each ensemble member), the search starts in the
domain with the highest number, which is generally the finest nest or
one of multiple finer nests. The search stops as soon as a domain
contains the observation location, working its way from largest number
to smallest number domain, ending with domain 1. For example, in a 4
domain case the data in the state vector that came from *wrfinput\_d04*
is searched first, then *wrfinput\_d03*, *wrfinput\_d02*, and finally
*wrfinput\_d01*. The forward operator is computed from the first domain
grid that contains the lat/lon of the observation. During the
assimilation phase, when the state values are adjusted based on the
correlations and assimilation increments, all points in all domains that
are within the localization radius are adjusted, regardless of domain.
The impact of an observation on the state depends only on the distance
between the observation and the state vector point, and the regression
coefficient based on the correlation between the distributions of the
ensemble of state vector points and the ensemble of observation forward
operator values.

The fields from WRF that are copied into the DART state vector are
controlled by namelist. See below for the documentation on the
&model\_nml entries. The state vector should include all fields needed
to restart a WRF run. There may be additional fields needed depending on
the microphysics scheme selected. See the ascii file
*wrf\_state\_variables\_table* in the *models/wrf* directory for a list
of fields that are often included in the DART state.

The 18 public interfaces are standardized for all DART-compliant models.
These interfaces allow DART to get the model state and metadata
describing this state, find state variables that are close to a given
location, and do spatial interpolation for a variety of variables
required in observational operators.

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

    &model_nml
       default_state_variables     = .true.
       wrf_state_variables         = 'NULL'
       wrf_state_bounds            = 'NULL'
       num_domains                 = 1
       calendar_type               = 3
       assimilation_period_seconds = 21600
       allow_obs_below_vol         = .false.
       vert_localization_coord     = 3
       center_search_half_length   = 500000.
       center_spline_grid_scale    = 10
       circulation_pres_level      = 80000.0
       circulation_radius          = 108000.0
       sfc_elev_max_diff           = -1.0
       polar                       = .false.
       periodic_x                  = .false.
       periodic_y                  = .false.
       scm                         = .false.  
       allow_perturbed_ics         = .false.   # testing purposes only
     /

    # Notes for model_nml:
    # (1) vert_localization_coord must be one of:
    #     1 = model level
    #     2 = pressure
    #     3 = height
    #     4 = scale height
    # (2) see bottom of this file for explanations of polar, periodic_x, 
    #     periodic_y, and scm
    # (3) calendar = 3 is GREGORIAN, which is what WRF uses.
    # (4) if 'default_state_variables' is .true. the model_mod.f90 code will
    #     fill the state variable table with the following wrf vars: 
    #        U, V, W, PH, T, MU
    #     you must set it to false before you change the value 
    #     of 'wrf_state_variables' and have it take effect.
    # (5) the format for 'wrf_state_variables' is an array of 5 strings:
    #     wrf netcdf variable name, dart QTY_xxx string, type string (must be 
    #     unique, will soon be obsolete, we hope), 'UPDATE', and '999' if the 
    #     array is part of all domains.  otherwise, it is a string with the domain
    #     numbers (e.g. '12' for domains 1 and 2, '13' for domains 1 and 3).
    #   example:
    # wrf_state_variables='U','QTY_U_WIND_COMPONENT','TYPE_U','UPDATE','999',
    #                     'V','QTY_V_WIND_COMPONENT','TYPE_V','UPDATE','999',
    #                     'W','QTY_VERTICAL_VELOCITY','TYPE_W','UPDATE','999',
    #                     'T','QTY_POTENTIAL_TEMPERATURE','TYPE_T','UPDATE','999',
    #                     'PH','QTY_GEOPOTENTIAL_HEIGHT','TYPE_GZ','UPDATE','999',
    #                     'MU','QTY_PRESSURE','TYPE_MU','UPDATE','999',
    #                     'QVAPOR','QTY_VAPOR_MIXING_RATIO','TYPE_QV','UPDATE','999',
    #                     'QCLOUD','QTY_CLOUD_LIQUID_WATER','TYPE_QC','UPDATE','999',
    #                     'QRAIN','QTY_RAINWATER_MIXING_RATIO','TYPE_QR','UPDATE','999',
    #                     'U10','QTY_U_WIND_COMPONENT','TYPE_U10','UPDATE','999',
    #                     'V10','QTY_V_WIND_COMPONENT','TYPE_V10','UPDATE','999',
    #                     'T2','QTY_TEMPERATURE','TYPE_T2','UPDATE','999',
    #                     'TH2','QTY_POTENTIAL_TEMPERATURE','TYPE_TH2','UPDATE','999',
    #                     'Q2','QTY_SPECIFIC_HUMIDITY','TYPE_Q2','UPDATE','999',
    #                     'PSFC','QTY_PRESSURE','TYPE_PS','UPDATE','999',
    # (6) the format for 'wrf_state_bounds' is an array of 4 strings:
    #     wrf netcdf variable name, minimum value, maximum value, and either
    #     FAIL or CLAMP.  FAIL will halt the program if an out of range value
    #     is detected.  CLAMP will set out of range values to the min or max.
    #     The special string 'NULL' will map to plus or minus infinity and will
    #     not change the values.  arrays not listed in this table will not
    #     be changed as they are read or written.
    #
    #
    # polar and periodic_x are used in global wrf.  if polar is true, the 
    # grid interpolation routines will wrap over the north and south poles.  
    # if periodic_x is true, when the east and west edges of the grid are
    # reached the interpolation will wrap.  note this is a separate issue
    # from regional models which cross the GMT line; those grids are marked
    # as having a negative offset and do not need to wrap; this flag controls
    # what happens when the edges of the grid are reached.

    # the scm flag is used for the 'single column model' version of WRF.
    # it needs the periodic_x and periodic_y flags set to true, in which
    # case the X and Y directions are periodic; no collapsing of the grid
    # into a single location like the 3d-spherical polar flag implies.

</div>

\
\

<div>

+-----------------------+-----------------------+-----------------------+
| Item                  | Type                  | Description           |
+=======================+=======================+=======================+
| default\_state\_varia | logical               | If *.true.*, the dart |
| bles                  |                       | state vector contains |
|                       |                       | the fields U, V, W,   |
|                       |                       | PH, T, MU, in that    |
|                       |                       | order, and only       |
|                       |                       | those. Any values     |
|                       |                       | listed in the         |
|                       |                       | *wrf\_state\_variable |
|                       |                       | s*                    |
|                       |                       | namelist item will be |
|                       |                       | ignored.              |
+-----------------------+-----------------------+-----------------------+
| wrf\_state\_variables | character(:, 5)       | A 2D array of         |
|                       |                       | strings, 5 per wrf    |
|                       |                       | array to be added to  |
|                       |                       | the dart state        |
|                       |                       | vector. If            |
|                       |                       | *default\_state\_vari |
|                       |                       | ables*                |
|                       |                       | is *.true.*, this is  |
|                       |                       | ignored. When         |
|                       |                       | *.false.*, this list  |
|                       |                       | of array names        |
|                       |                       | controls which arrays |
|                       |                       | and the order that    |
|                       |                       | they are added to the |
|                       |                       | state vector. The 5   |
|                       |                       | strings are:          |
|                       |                       | 1.  WRF field name -  |
|                       |                       |     must match netcdf |
|                       |                       |     name exactly      |
|                       |                       | 2.  DART KIND name -  |
|                       |                       |     must match a      |
|                       |                       |     valid DART        |
|                       |                       |     QTY\_xxx exactly  |
|                       |                       | 3.  TYPE\_NN - will   |
|                       |                       |     hopefully be      |
|                       |                       |     obsolete, but for |
|                       |                       |     now NN should     |
|                       |                       |     match the field   |
|                       |                       |     name.             |
|                       |                       | 4.  the string        |
|                       |                       |     UPDATE. at some   |
|                       |                       |     future point,     |
|                       |                       |     non-updatable     |
|                       |                       |     fields may become |
|                       |                       |     part of the state |
|                       |                       |     vector.           |
|                       |                       | 5.  A numeric string  |
|                       |                       |     listing the       |
|                       |                       |     domain numbers    |
|                       |                       |     this array is     |
|                       |                       |     part of. The      |
|                       |                       |     specical string   |
|                       |                       |     999 means all     |
|                       |                       |     domains. For      |
|                       |                       |     example, '12'     |
|                       |                       |     means domains 1   |
|                       |                       |     and 2, '13' means |
|                       |                       |     1 and 3.          |
+-----------------------+-----------------------+-----------------------+
| wrf\_state\_bounds    | character(:, 4)       | A 2D array of         |
|                       |                       | strings, 4 per wrf    |
|                       |                       | array. During the     |
|                       |                       | copy of data to and   |
|                       |                       | from the wrf netcdf   |
|                       |                       | file, variables       |
|                       |                       | listed here will have |
|                       |                       | minimum and maximum   |
|                       |                       | values enforced. The  |
|                       |                       | 4 strings are:        |
|                       |                       | 1.  WRF field name -  |
|                       |                       |     must match netcdf |
|                       |                       |     name exactly      |
|                       |                       | 2.  Minimum --        |
|                       |                       |     specified as a    |
|                       |                       |     string but must   |
|                       |                       |     be a numeric      |
|                       |                       |     value (e.g.       |
|                       |                       |     '0.1') Can be     |
|                       |                       |     'NULL' to allow   |
|                       |                       |     any minimum       |
|                       |                       |     value.            |
|                       |                       | 3.  Maximum --        |
|                       |                       |     specified as a    |
|                       |                       |     string but must   |
|                       |                       |     be a numeric      |
|                       |                       |     value (e.g.       |
|                       |                       |     '0.1') Can be     |
|                       |                       |     'NULL' to allow   |
|                       |                       |     any maximum       |
|                       |                       |     value.            |
|                       |                       | 4.  Action -- valid   |
|                       |                       |     strings are       |
|                       |                       |     'CLAMP', 'FAIL'.  |
|                       |                       |     'FAIL' means if a |
|                       |                       |     value is found    |
|                       |                       |     outside the       |
|                       |                       |     range, the code   |
|                       |                       |     fails with an     |
|                       |                       |     error. 'CLAMP'    |
|                       |                       |     simply sets the   |
|                       |                       |     out of range      |
|                       |                       |     values to the     |
|                       |                       |     given minimum or  |
|                       |                       |     maximum without   |
|                       |                       |     error.            |
+-----------------------+-----------------------+-----------------------+
| num\_domains          | integer               | Total number of WRF   |
|                       |                       | domains, including    |
|                       |                       | nested domains.       |
+-----------------------+-----------------------+-----------------------+
| calendar\_type        | integer               | Calendar type. Should |
|                       |                       | be 3 (GREGORIAN) for  |
|                       |                       | WRF.                  |
+-----------------------+-----------------------+-----------------------+
| assimilation\_period\ | integer               | The time (in seconds) |
| _seconds              |                       | between               |
|                       |                       | assimilations. This   |
|                       |                       | is modified if        |
|                       |                       | necessary to be an    |
|                       |                       | integer multiple of   |
|                       |                       | the underlying model  |
|                       |                       | timestep.             |
+-----------------------+-----------------------+-----------------------+
| periodic\_x           | logical               | If *.true.*, the grid |
|                       |                       | is periodic in        |
|                       |                       | longitude, and points |
|                       |                       | above the last grid   |
|                       |                       | cell and points below |
|                       |                       | the first grid cell   |
|                       |                       | are wrapped. Note     |
|                       |                       | this is not the same  |
|                       |                       | as a grid which       |
|                       |                       | crosses the prime     |
|                       |                       | meridian. WRF handles |
|                       |                       | that with an offset   |
|                       |                       | in longitude and      |
|                       |                       | points beyond the     |
|                       |                       | last grid index are   |
|                       |                       | outside the domain.   |
+-----------------------+-----------------------+-----------------------+
| periodic\_y           | logical               | Used for the Single   |
|                       |                       | Column Model to make  |
|                       |                       | the grid wrap in Y    |
|                       |                       | (see scm below). This |
|                       |                       | is NOT the same as    |
|                       |                       | wrapping in latitude  |
|                       |                       | (see polar below).    |
+-----------------------+-----------------------+-----------------------+
| polar                 | logical               | If *.true.*, points   |
|                       |                       | at the poles are      |
|                       |                       | wrapped across the    |
|                       |                       | grid. It is not clear |
|                       |                       | this is a good idea   |
|                       |                       | since the grid is     |
|                       |                       | degnerate here.       |
+-----------------------+-----------------------+-----------------------+
| scm                   | logical               | If *.true.* the       |
|                       |                       | Single Column Model   |
|                       |                       | is assumed. The grid  |
|                       |                       | is a single vertical  |
|                       |                       | column, and there are |
|                       |                       | 9 cells arranged in a |
|                       |                       | 3x3 grid. See the WRF |
|                       |                       | documentation for     |
|                       |                       | more information on   |
|                       |                       | this configuration.   |
|                       |                       | *periodic\_x* and     |
|                       |                       | *periodic\_y* should  |
|                       |                       | also be *.true.* in   |
|                       |                       | this case.            |
+-----------------------+-----------------------+-----------------------+
| sfc\_elev\_max\_diff  | real(r8)              | If &gt; 0, the        |
|                       |                       | maximum difference,   |
|                       |                       | in meters, between an |
|                       |                       | observation marked as |
|                       |                       | a 'surface obs' as    |
|                       |                       | the vertical type     |
|                       |                       | (with the surface     |
|                       |                       | elevation, in meters, |
|                       |                       | as the numerical      |
|                       |                       | vertical location),   |
|                       |                       | and the surface       |
|                       |                       | elevation as defined  |
|                       |                       | by the model.         |
|                       |                       | Observations further  |
|                       |                       | away from the surface |
|                       |                       | than this threshold   |
|                       |                       | are rejected and not  |
|                       |                       | assimilated. If the   |
|                       |                       | value is negative,    |
|                       |                       | this test is skipped. |
+-----------------------+-----------------------+-----------------------+
| allow\_obs\_below\_vo | logical               | If *.false.* then if  |
| l                     |                       | an observation with a |
|                       |                       | vertical coordinate   |
|                       |                       | of pressure or height |
|                       |                       | (i.e. not a surface   |
|                       |                       | observation) is below |
|                       |                       | the lowest 3d sigma   |
|                       |                       | level, it is outside  |
|                       |                       | the field volume and  |
|                       |                       | the interpolation     |
|                       |                       | routine rejects it.   |
|                       |                       | If this is set to     |
|                       |                       | *.true.* and the      |
|                       |                       | observation is above  |
|                       |                       | the surface elevation |
|                       |                       | but below the lowest  |
|                       |                       | field volume level,   |
|                       |                       | the code will         |
|                       |                       | extrapolate downward  |
|                       |                       | from data values at   |
|                       |                       | levels 1 and 2.       |
+-----------------------+-----------------------+-----------------------+
| center\_search\_half\ | real(r8)              | The model\_mod now    |
| _length               |                       | contains two schemes  |
|                       |                       | for searching for a   |
|                       |                       | vortex center         |
|                       |                       | location. If the      |
|                       |                       | **old** scheme is     |
|                       |                       | compiled in, then     |
|                       |                       | this and the          |
|                       |                       | center\_spline\_grid\ |
|                       |                       | _scale                |
|                       |                       | namelist items are    |
|                       |                       | used. (Search code    |
|                       |                       | for                   |
|                       |                       | 'use\_old\_vortex'.)  |
|                       |                       | Half length (in       |
|                       |                       | meters) of a square   |
|                       |                       | box for searching the |
|                       |                       | vortex center.        |
+-----------------------+-----------------------+-----------------------+
| center\_spline\_grid\ | integer               | The model\_mod now    |
| _scale                |                       | contains two schemes  |
|                       |                       | for searching for a   |
|                       |                       | vortex center         |
|                       |                       | location. If the      |
|                       |                       | **old** scheme is     |
|                       |                       | compiled in, then     |
|                       |                       | this and the          |
|                       |                       | center\_search\_half\ |
|                       |                       | _length               |
|                       |                       | namelist items are    |
|                       |                       | used. (Search code    |
|                       |                       | for                   |
|                       |                       | 'use\_old\_vortex'.)  |
|                       |                       | Ratio of refining     |
|                       |                       | grid for              |
|                       |                       | spline-interpolation  |
|                       |                       | in determining the    |
|                       |                       | vortex center.        |
+-----------------------+-----------------------+-----------------------+
| circulation\_pres\_le | real(r8)              | The model\_mod now    |
| vel                   |                       | contains two schemes  |
|                       |                       | for searching for a   |
|                       |                       | vortex center         |
|                       |                       | location. If the      |
|                       |                       | **new** scheme is     |
|                       |                       | compiled in, then     |
|                       |                       | this and the          |
|                       |                       | circulation\_radius   |
|                       |                       | namelist items are    |
|                       |                       | used. (Search code    |
|                       |                       | for                   |
|                       |                       | 'use\_old\_vortex'.)  |
|                       |                       | Pressure, in pascals, |
|                       |                       | of the level at which |
|                       |                       | the circulation is    |
|                       |                       | computed when         |
|                       |                       | searching for the     |
|                       |                       | vortex center.        |
+-----------------------+-----------------------+-----------------------+
| circulation\_radius   | real(r8)              | The model\_mod now    |
|                       |                       | contains two schemes  |
|                       |                       | for searching for a   |
|                       |                       | vortex center         |
|                       |                       | location. If the      |
|                       |                       | **new** scheme is     |
|                       |                       | compiled in, then     |
|                       |                       | this and the          |
|                       |                       | circulation\_pres\_le |
|                       |                       | vel                   |
|                       |                       | namelist items are    |
|                       |                       | used. (Search code    |
|                       |                       | for                   |
|                       |                       | 'use\_old\_vortex'.)  |
|                       |                       | Radius, in meters, of |
|                       |                       | the circle over which |
|                       |                       | the circulation       |
|                       |                       | calculation is done   |
|                       |                       | when searching for    |
|                       |                       | the vortex center.    |
+-----------------------+-----------------------+-----------------------+
| vert\_localization\_c | integer               | Vertical coordinate   |
| oord                  |                       | for vertical          |
|                       |                       | localization.         |
|                       |                       | -   1 = model level   |
|                       |                       | -   2 = pressure (in  |
|                       |                       |     pascals)          |
|                       |                       | -   3 = height (in    |
|                       |                       |     meters)           |
|                       |                       | -   4 = scale height  |
|                       |                       |     (unitless)        |
+-----------------------+-----------------------+-----------------------+
| allow\_perturbed\_ics | logical               | *allow\_perturbed\_ic |
|                       |                       | s*                    |
|                       |                       | should not be used in |
|                       |                       | most cases. It is     |
|                       |                       | provided only as a    |
|                       |                       | means to create a     |
|                       |                       | tiny ensemble for     |
|                       |                       | non-advancing tests.  |
|                       |                       | Creating an initial   |
|                       |                       | ensemble is covered   |
|                       |                       | in the [WRF-DART      |
|                       |                       | tutorial](http://www. |
|                       |                       | image.ucar.edu/wrfdar |
|                       |                       | t/tutorial).          |
+-----------------------+-----------------------+-----------------------+

</div>

The following items used to be in the WRF namelist but have been
removed. The first 4 are no longer needed, and the last one was moved to
[&dart\_to\_wrf\_nml](WRF_DART_utilities/dart_to_wrf.html#Namelist) in
2010. In the Lanai release having these values in the namelist does not
cause a fatal error, but more recent versions of the code will fail if
any of these values are specified. Remove them from your namelist to
avoid errors.

<div>

  Item                  Type                Description
  --------------------- ------------------- -------------------------------------------
  *surf\_obs*           logical             OBSOLETE -- now an error to specify this.
  *soil\_data*          logical             OBSOLETE -- now an error to specify this.
  *h\_diab*             logical             OBSOLETE -- now an error to specify this.
  *num\_moist\_vars*    integer             OBSOLETE -- now an error to specify this.
  *adv\_mod\_command*   character(len=32)   OBSOLETE -- now an error to specify this.

</div>

\
\

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    types_mod
    time_manager_mod
    threed_sphere/location_mod
    utilities_mod
    netcdf_utilities_mod
    mpi_utilities_mod
    random_seq_mod
    obs_kind_mod
    ensemble_manager_mod
    sort_mod
    distributed_state_mod
    default_model_mod
    state_structure_mod
    map_utils
    misc_definitions_module
    netcdf
    typesizes

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

These interfaces are **required** DART interfaces and are used by many
core routines. The argument lists to these routines should not be
modified.

  -------------------------- --------------------------------------------------------------------------------
  *use model\_mod, only :*   [get\_model\_size](#get_model_size)
                             [static\_init\_model](#static_init_model)
                             [get\_state\_meta\_data](#get_state_meta_data)
                             [model\_interpolate](#model_interpolate)
                             [shortest\_time\_between\_assimilations](#shortest_time_between_assimilations)
                             [nc\_write\_model\_atts](#nc_write_model_atts)
                             [pert\_model\_copies](#pert_model_copies)
                             [get\_close\_obs](#get_close_obs)
                             [get\_close\_state](#get_close_state)
                             [convert\_vertical\_obs](#convert_vertical_obs)
                             [convert\_vertical\_state](#convert_vertical_state)
                             [read\_model\_time](#read_model_time)
                             [write\_model\_time](#write_model_time)
                             [end\_model](#end_model)
                             [adv\_1step](#adv_1step)
                             [init\_time](#init_time)
                             [init\_conditions](#init_conditions)
                             [nc\_write\_model\_vars](#nc_write_model_vars)
  -------------------------- --------------------------------------------------------------------------------

The rest of these public interfaces are unique to the WRF model\_mod and
are not documented ... yet.

  -------------------------- -----------------------------------------------------------------
  *use model\_mod, only :*   [get\_number\_domains](#get_number_domains)
                             [get\_wrf\_static\_data](#get_wrf_static_data)
                             [model\_pressure\_distrib](#model_pressure_distrib)
                             [model\_height\_distrib](#model_height_distrib)
                             [pres\_to\_zk](#pres_to_zk)
                             [height\_to\_zk](#height_to_zk)
                             [get\_domain\_info](#get_domain_info)
                             [get\_wrf\_state\_variables](#get_wrf_state_variables)
                             [fill\_default\_state\_table](#fill_default_state_table)
                             [read\_wrf\_dimensions](#read_wrf_dimensions)
                             [get\_number\_of\_wrf\_variables](#get_number_of_wrf_variables)
                             [get\_variable\_bounds](#get_variable_bounds)
                             [set\_variable\_bound\_defaults](#set_variable_bound_defaults)
                             [get\_variable\_size\_from\_file](#get_variable_size_from_file)
                             [get\_wrf\_date](#get_wrf_date)
                             [set\_wrf\_date](#set_wrf_date)
                             [vert\_convert](#vert_convert)
                             [height\_diff\_check](#height_diff_check)
                             [max\_state\_variables](#max_state_variables)
                             [num\_state\_table\_columns](#num_state_table_columns)
                             [num\_bounds\_table\_columns](#num_bounds_table_columns)
                             [wrf\_dom](#wrf_dom)
                             [wrf\_static\_data\_for\_dart](#wrf_static_data_for_dart)
  -------------------------- -----------------------------------------------------------------

A note about documentation style. Optional arguments are enclosed in
brackets *\[like this\]*.

[]{#get_model_size}\

<div class="routine">

*model\_size = get\_model\_size( )*
    integer :: get_model_size

</div>

<div class="indent1">

Returns the length of the model state vector as an integer. This
includes all nested domains.

  --------------- ---------------------------------------
  *model\_size*   The length of the model state vector.
  --------------- ---------------------------------------

</div>

\
[]{#static_init_model}\

<div class="routine">

*call static\_init\_model()*

</div>

<div class="indent1">

Used for runtime initialization of the model. This is the first call
made to the model by any DART compliant assimilation routine. It reads
the model namelist parameters, set the calendar type (the GREGORIAN
calendar is used with the WRF model), and determine the dart vector
length. This subroutine requires that wrfinput\_d01, wrfinput\_d02, ...
(one file for each domain) be present in the working directory to
retrieve model information (grid dimensions and spacing, time step,
pressure at the top of the model, map projection parameters, etc).

</div>

\
[]{#get_state_meta_data}\

<div class="routine">

*call get\_state\_meta\_data (index\_in, location
*\[, var\_type\_out, id\_out\]* )*
    integer(i8),         intent(in)  :: index_in
    type(location_type), intent(out) :: location
    integer, optional,   intent(out) :: var_type_out
    integer, optional,   intent(out) :: id_out

</div>

<div class="indent1">

Returns metadata about a given element, indexed by index\_in, in the
model state vector. The location defines where the state variable is
located while the type of the variable (for instance temperature, or u
wind component) is returned by var\_type. The integer values used to
indicate different variable types in var\_type are themselves defined as
public interfaces to model\_mod if required. The last optional argument
is the wrf domain identification number - obviously this is unique to
the WRF version of this *required* routine.

  ------------------ -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *index\_in   *     Index of state vector element about which information is requested.
  *location*         Returns location of indexed state variable. The location should use a location\_mod that is appropriate for the model domain. For realistic atmospheric models, for instance, a three-dimensional spherical location module that can represent height in a variety of ways is provided.
  *var\_type\_out*   Returns the type of the indexed state variable as an optional argument.
  *id\_out*          Returns the wrf domain identification number of the indexed state variable as an optional argument.
  ------------------ -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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

Given a handle containing information for a state vector, an ensemble
size, a location, and a model state variable quantity; interpolates the
state variable field to that location and returns an ensemble-sized
array of values in *expected\_obs(:)*. The *obs\_quantity* variable is
one of the quantity (QTY) parameters defined in the
[obs\_kind\_mod.f90](../../assimilation_code/modules/observations/obs_kind_mod.html)
file and defines the quantity to interpolate. In the case where the
observational operator is not defined at the given location (e.g. the
observation is below the model surface or outside the domain), obs\_val
is returned as -888888.0 and a positive istatus. A successful
interpolation results in an istatus = 0. The interpolation is performed
in the domain with the highest resolution containing the observation.

  ----------------- -----------------------------------------------------------------------------------------------------------------------
  *state\_handle*   The handle to the state structure containing information about the state vector about which information is requested.
  *ens\_size*       The ensemble size.
  *location   *     Location to which to interpolate.
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

Sets the width of the assimilation window. Observations outside this
window will create a fatal error. This is ensured to be a multiple of
the time step used for domain 1 (usually the largest time step among all
domains because domain 1 is the coarser grid). The time step is read
from the *wrfinput\_d01* file and the nominal assimilation time is
specified in the *assimilation\_period\_seconds* namelist item.

  ------- -------------------------------
  *var*   Width of assimilation window.
  ------- -------------------------------

</div>

\
[]{#nc_write_model_atts}\

<div class="routine">

*call nc\_write\_model\_atts(ncid, dom\_id)*
    integer, intent(in) :: ncid
    integer, intent(in) :: dom_id

</div>

<div class="indent1">

Routine to write model-specific attributes and metadata to the output
and diagnostic files. This includes the model grid information.

  ----------- -----------------------------------------------------------
  *ncid   *   Integer file descriptor to previously-opened netCDF file.
  *dom\_id*   Domain number.
  ----------- -----------------------------------------------------------

</div>

\
[]{#pert_model_copies}\

<div class="routine">

*call pert\_model\_copies(ens\_handle, ens\_size, dummy\_pert\_amp,
interf\_provided)*
    type(ensemble_type), intent(inout) :: ens_handle
    integer,             intent(in)    :: ens_size
    real(r8),            intent(in)    :: dummy_pert_amp
    logical,             intent(out)   :: interf_provided

</div>

<div class="indent1">

Given a model state, produces an ensemble of model states that are
somehow different. This is used to generate initial ensemble conditions
perturbed around some control trajectory state when one is preparing to
spin-up ensembles.\
\
*pert\_model\_copies* is presently not encouraged for WRF. The initial
ensemble has to be generated off-line. If coherent structures are not
required, the filter can generate an ensemble with uncorrelated random
Gaussian noise of 0.002. This is of course not appropriate for a model
like WRF which has variables expressed in a wide range of scales. It is
thus recommended to generate the initial ensemble off-line, perhaps with
the tools provided in *models/wrf/PERTURB/3DVAR-COVAR*.\
\
This topic is also covered in the [WRF-DART
tutorial](http://www.image.ucar.edu/wrfdart/tutorial).\
\
Generally you do not want to just perturb a single state to begin an
experiment, especially for a regional weather model, because the
resulting fields will have spread but they won't have organized
features. we have had good luck with some global atmosphere models where
there is a lot of model divergence; after a few days of running they
evolve into plausible conditions that allow assimilation of real obs.\
\
If you really need to start with a single state and proceed, the
suggestion is to start with small magnitude perturbations and then get a
good ensemble of boundary conditions and run the model for a while (many
days) to let it evolve into plausible weather patterns. Then start
assimilating real obs.\
\
Using this routine requires you to set the new namelist item
*allow\_perturbed\_ics = .true.* so you have to read the warnings here
or in the source code.\
\
This code will add random noise field by field (T, U, V, etc), and new
values will not exceed the original max or min values for each field.
This means it will not generate illegal values (e.g. negatives for
percentages or number concentrations) but it also means that if all
values in a field are identical (e.g. all 0.0) this routine will not
change those values. The code can easily be modified to set allowed min
and max values here instead of using the incoming field min and max
values; but you will have to modify the code to enable that
functionality.

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
loc\_types, num\_close, close\_ind *\[, dist, state\_handle\]*)*
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

Given a location and quantity, compute the distances to all other
locations in the *obs* list. The return values are the number of items
which are within maxdist of the base, the index numbers in the original
obs list, and optionally the distances. The *gc* contains precomputed
information to speed the computations.\
\
Calls the 3-D sphere locations module to get a list of other close
observations. See the
[threed\_sphere:get\_close\_obs()](../../assimilation_code/location/threed_sphere/location_mod.html#get_close_obs)
for the documentation of the locations module version of this code.
Then, if vertical localization is enabled, this code converts all
vertical locations to the selected vertical type
(&model\_nml::vert\_localization\_coord). It then computes a real 3D
distance and returns it to the calling code.

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
  *state\_handle*   If present, the handle to the state structure containing information about the state vector about which information is requested.
  ----------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#get_close_state}\

<div class="routine">

*call get\_close\_state(gc, base\_loc, base\_type, state\_loc,
state\_qtys, state\_indx, num\_close, close\_ind
*\[, dist, state\_handle\]*)*
    type(get_close_type),           intent(in)    :: gc
    type(location_type),            intent(inout) :: base_loc
    integer,                        intent(in)    :: base_type
    type(location_type),            intent(inout) :: state_loc(:)
    integer,                        intent(in)    :: state_qtys(:)
    integer(i8),                    intent(in)    :: state_indx(:)
    integer,                        intent(out)   :: num_close
    integer,                        intent(out)   :: close_ind(:)
    real(r8),             optional, intent(out)   :: dist(:)
    type(ensemble_type),  optional, intent(in)    :: state_handle

</div>

<div class="indent1">

Given a location and quantity, compute the distances to all other
locations in the *state\_loc* list. The return values are the number of
items which are within maxdist of the base, the index numbers in the
original state\_loc list, and optionally the distances. The *gc*
contains precomputed information to speed the computations.

  ------------------ ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *gc*               The get\_close\_type which stores precomputed information about the locations to speed up searching
  *base\_loc*        Reference location. The distances will be computed between this location and every other location in the list
  *base\_type*       The DART quantity at the *base\_loc*
  *state\_loc(:)*    Compute the distance between the *base\_loc* and each of the locations in this list
  *state\_qtys(:)*   The corresponding quantity of each item in the *state\_loc* list
  *state\_indx(:)*   The corresponding DART index of each item in the *state\_loc* list. This is not available in the default implementation but may be used in custom implementations.
  *num\_close*       The number of items from the *state\_loc* list which are within maxdist of the base location
  *close\_ind(:)*    The list of index numbers from the *state\_loc* list which are within maxdist of the base location
  *dist(:)*          If present, return the distance between each entry in the *close\_ind* list and the base location. If not present, all items in the *state\_loc* list which are closer than maxdist will be added to the list but the overhead of computing the exact distances will be skipped.
  *state\_handle*    If present, the handle to the state structure containing information about the state vector about which information is requested.
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

Converts the observations to the desired vertical localization
coordinate system. To be able to interpolate the model state to the
observation location, or to compute the true distance between the state
and the observation, it is necessary to convert everything to a single
coordinate system.

  ----------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *state\_handle*   The handle to the state.
  *num*             The number of observation locations.
  *locs*            The array of observation locations.
  *loc\_qtys*       The array of observation quantities.
  *loc\_types*      The array of observation types.
  *which\_vert*     An integer describing the desired vertical coordinate system. There is a [table describing the vertical coordinate system possibilities](../../assimilation_code/location/threed_sphere/location_mod.html#vert_constants).
  *status*          Success or failure of the vertical conversion. If *istatus = 0*, the conversion was a success. Any other value is a failure.
  ----------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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

Converts the state to the desired vertical localization coordinate
system. To compute the true distance between the state and the
observation, it is necessary to convert everything to a single
coordinate system.

  ----------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *state\_handle*   The handle to the state.
  *num*             The number of state locations.
  *locs*            The array of state locations.
  *loc\_qtys*       The array of state quantities.
  *loc\_types*      The array of state types.
  *which\_vert*     An integer describing the desired vertical coordinate system. There is a [table describing the vertical coordinate system possibilities](../../assimilation_code/location/threed_sphere/location_mod.html#vert_constants).
  *status*          Success or failure of the vertical conversion. If *istatus = 0*, the conversion was a success. Any other value is a failure.
  ----------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#read_model_time}\

<div class="routine">

*model\_time = read\_model\_time(filename)*
    character(len=*), intent(in) :: filename
    type(time_type)              :: model_time

</div>

<div class="indent1">

Reads the *Times* variable in the WRF netCDF file and converts it to a
DART time\_type object.

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

Writes the DART time object to a netCDF file in a manner consistent with
WRF, i.e. into a *Times* character string variable.

  -------------- --------------------------------------
  *ncid*         handle to an open netCDF file
  *dart\_time*   The current time of the model state.
  -------------- --------------------------------------

</div>

\
[]{#end_model}\

<div class="routine">

*call end\_model( )*

</div>

<div class="indent1">

Called when use of a model is completed to clean up storage, etc.

</div>

\
[]{#adv_1step}\

<div class="routine">

*call adv\_1step(x, time)*
    real(r8), dimension(:),   intent(inout) ::  x 
    type(time_type),          intent(in)    ::  time 

</div>

<div class="indent1">

Not supported for the WRF model. Will throw a fatal error if called.

</div>

\
[]{#init_time}\

<div class="routine">

*call init\_time(time)*
    type(time_type),        intent(in)  ::  time 

</div>

<div class="indent1">

Not supported for the WRF model. Will throw a fatal error if called.

</div>

\
[]{#init_conditions}\

<div class="routine">

*call init\_conditions( x )*
    real(r8), dimension(:), intent(out) ::  x 

</div>

<div class="indent1">

Not supported for the WRF model. Will throw a fatal error if called.

</div>

\
[]{#nc_write_model_vars}\

<div class="routine">

*ierr = nc\_write\_model\_vars(ncFileID, statevec, copyindex,
timeindex)*
    integer                            :: nc_write_model_vars
    integer,                intent(in) :: ncFileID
    real(r8), dimension(:), intent(in) :: statevec
    integer,                intent(in) :: copyindex
    integer,                intent(in) :: timeindex

</div>

<div class="indent1">

This routine is not used and has been deprecated since the core DART
routines natively write netCDF.

</div>

\

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   model\_nml in input.nml
-   wrfinput\_d01, wrfinput\_d02, ... (one file for each domain)
-   netCDF output state diagnostics files

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

<http://www2.mmm.ucar.edu/wrf/users/docs/user_guide_V3/contents.html>

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
static\_init\_model
'Please put wrfinput\_d0'//idom//' in the work directory.'
One of the wrfinput\_d0\# is missing in the work directory
static\_init\_model
Map projection no supported
Try PROJ\_LATLON(0), PROJ\_LC(1), PROJ\_PS(2), PROJ\_MERC(3)
static\_init\_model\
nc\_write\_model\_atts\
Various NetCDF-f90 interface error messages
From one of the NetCDF calls in the named routine
get\_state\_meta\_data
dart index out of range
Unlikely. Would indicate a serious bug in the code
model\_interpolate\
get\_dist\_wrf
wrong option for which\_vert
See the [which\_vert
description](../../assimilation_code/location/threed_sphere/location_mod.html#vert_constants)
model\_interpolate
'do not recognize obs kind ',obs\_kind
See list in 'use obs\_kind\_mod' statement in model\_mod.f90
get\_wrf\_index
'Indices ',i,j,k,' exceed grid dimensions: ',\#1,\#2,\#3
One of the grid indices exceeds the corresponding dimension for the
var\_type input. Unlikely to happen but would indicate a serious bug in
the code
get\_dist\_wrf
Unable to define vloc
The vertical location is below the model surface or above the model lid
nc\_write\_model\_atts
Time dimension ID \# must match Unlimited Dimension ID \#
NetCDF file writing error
read\_dt\_from\_wrf\_nml
'max\_dom in namelist.input = ',max\_dom'num\_domains in input.nml =
',num\_domains'Make them consistent.'
The number of WRF domains in namelist.input and in input.nml do not
match

</div>

KNOWN BUGS
----------

None.

[]{#FuturePlans}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FUTURE PLANS
------------

Support for WRF hybrid coordinates.

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
  Contact:           dart@ucar.edu
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


