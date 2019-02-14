[]{#TOP}

PROGRAM *dart\_to\_noah*
========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | [DART Documentation Main          |
| logo](../../documentation/images/ | Index](../../documentation/index. |
| Dartboard7.png){height="70"}      | html)\                            |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [MODULES](#Modules) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

Overview
--------

*dart\_to\_noah* is the program that **overwrites** a NOAH netCDF-format
restart file (usually linked to "*restart.nc*") with the state
information contained in a DART output/restart file (e.g.
*perfect\_restart, filter\_restart, ...* ). The DART model time is
compared to the time in the NOAH restart file. If the time in the
restart file does not match the DART model time, the
*dart\_vector\_to\_model\_file* routine issues an error message and
aborts. Due to the fact the NOAH restart files are timestamped such that
the time reflects the valid time **UPON THE NEXT ADVANCE**, one
*NOAH\_TIMESTEP* is subtracted from the time in the *Time* variable in
the NOAH restart file. See the expanded explanation in the NOAH
[model\_mod.html](model_mod.html#NOAHtimes).\
\
The standalone version of NOAH (*Noah\_hrldas\_beta*) requires a
separate forcing file for each hour in its current configuration.
*dart\_to\_noah* creates a file (*noah\_advance\_information.txt*) that
has a list of the expected *\*\*\*\*\*\*\*.LDASIN\_DOMAIN1* files needed
for the next model advance. Example *noah\_advance\_information.txt*
file:

    dart_to_noah:noah  model      date 2009 Jan 02 08:00:00
    dart_to_noah:noah  advance_to_date 2009 Jan 02 09:00:00
    khour  =      1
    nfiles =      2
    2009010209.LDASIN_DOMAIN1
    2009010210.LDASIN_DOMAIN1

While setting up an experiment or for testing purposes, *dart\_to\_noah*
may be used on DART files that have a header containing one time stamp
followed by the model state by setting *input.nml*
*&dart\_to\_noah\_nml:advance\_time\_present* = *.false.* .\
\
During *perfect\_model\_obs* or *filter*, the
*&dart\_to\_noah\_nml:advance\_time\_present* must be set to *.true.* to
read the internal DART files that have *two* timestamps in the header.
These files are temporarily generated when DART is used to advance the
model. One timestamp is the 'advance\_to' time, the other is the
'valid\_time' of the model state.\
\
Conditions required for successful execution of *dart\_to\_noah*:

-   a valid *input.nml* namelist file for DART
-   a valid *namelist.hrldas* namelist file for NOAH
-   a DART file (typically linked to *dart\_restart*)
-   a NOAH restart file (typically linked to *noah\_restart.nc*).

*dart\_to\_noah* and *noah\_to\_dart* are used extensively in the
scripts in the *shell\_scripts* directory. Since this program is called
repeatedly for every ensemble member, we have found it convenient to
link the DART input file to the default input filename
(*dart\_restart*). The same thing is true for the NOAH restart files
(*noah\_restart.nc*).

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

    &dart_to_noah_nml
       dart_to_noah_input_file = 'dart_restart',
       skip_variables          = ' '
       advance_time_present    = .true.,
      /

</div>

<div>

  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  Contents                      Type                    Description
  ----------------------------- ----------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  dart\_to\_noah\_input\_file   character(len=128)      The name of the DART file containing the model state to insert into the NOAH restart file.

  skip\_variables               character(len=32) ::\   The list of NOAH variables that **must not** be updated in the NOAH restart file even if they were used to create the DART state vector. Be aware that these variables WILL be modified in *analysis.nc* so you can see the assimilated state.
                                dimension(40)           

  advance\_time\_present        logical                 If you are converting a DART initial conditions or restart file this should be *.false.*; these files have a single timestamp describing the valid time of the model state. If *.true.*, TWO timestamps are expected to be the DART file header. In this case, a namelist for NOAH (called *noah\_in.DART*) is created that contains the *&time\_manager\_nml* settings appropriate to advance NOAH to the time requested by DART.
  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\

<div class="namelist">

    &model_nml
         noah_netcdf_filename         = 'restart.nc',
         assimilation_period_days     =    0,
         assimilation_period_seconds  = 3600,
         model_perturbation_amplitude = 0.0,
         output_state_vector          = .false.
         debug                        = 1,
         noah_state_variables         = 'SOIL_T',   'QTY_SOIL_TEMPERATURE',
                                        'SOIL_M',   'QTY_SOIL_MOISTURE',
                                        'SOIL_W',   'QTY_SOIL_LIQUID_WATER',
                                        'SKINTEMP', 'QTY_SKIN_TEMPERATURE',
                                        'SNODEP',   'QTY_SNOW_THICKNESS',
                                        'WEASD',    'QTY_SNOW_WATER',
                                        'CANWAT',   'QTY_CANOPY_WATER',
                                        'QFX',      'QTY_LATENT_HEAT_FLUX',
      /

</div>

The full description of the *model\_nml* namelist is documented in the
[noah model\_mod](model_mod.html#Namelist), but the most important
variables for *noah\_to\_dart* are repeated here.

<div>

  -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  Contents                 Type                   Description
  ------------------------ ---------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  noah\_netcdf\_filename   character(len=128)     The name of the NOAH RESTART file to use to create the DART state vector. For convenience, the *advance\_model.csh* script usually links the most recent restart file to a static name.

  noah\_state\_variables   character(len=32)::\   The list of variable names in the NOAH restart file to use to create the DART state vector and their corresponding DART kinds.
                           dimension(2,40)        
  -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\

### Example of namelists from *input.nml* {#example-of-namelists-from-input.nml .indent1}

    &model_nml
       noah_netcdf_filename    = 'restart.nc',
       noah_state_variables    = 'SOIL_T',   'QTY_SOIL_TEMPERATURE',
                                 'SOIL_M',   'QTY_SOIL_MOISTURE',
                                 'SOIL_W',   'QTY_SOIL_LIQUID_WATER',
                                 'SKINTEMP', 'QTY_SKIN_TEMPERATURE',
                                 'SNODEP',   'QTY_SNOW_THICKNESS',
                                 'WEASD',    'QTY_SNOW_WATER',
                                 'CANWAT',   'QTY_CANOPY_WATER',
                                 'QFX',      'QTY_LATENT_HEAT_FLUX',
                                 'HFX',      'QTY_SENSIBLE_HEAT_FLUX',
                                 'GRDFLX',   'QTY_GROUND_HEAT_FLUX'
      /

    &dart_to_noah_nml
       dart_to_noah_input_file = 'dart_restart',
       skip_variables          = 'QFX','HFX',
       advance_time_present    = .true.
      /

<div class="namelist">

    &NOAHLSM_OFFLINE
       ...  
       KHOUR                   = 1
       FORCING_TIMESTEP        = 3600
       NOAH_TIMESTEP           = 3600
       OUTPUT_TIMESTEP         = 3600
       RESTART_FREQUENCY_HOURS = 1
       SPLIT_OUTPUT_COUNT      = 1
       ...
    /

</div>

The *NOAHLSM\_OFFLINE* namelist is read from *namelist.hrldas* - only
those variables important for *noah\_to\_dart* are repeated here. The
interpretations are exactly as described in the NOAH documentation.\
**The values listed above are the only values expected to work.** Change
these at your own risk!

[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    assim_model_mod
    location_mod
    model_mod
    null_mpi_utilities_mod
    obs_kind_mod
    random_seq_mod
    time_manager_mod
    types_mod
    utilities_mod

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES Read
----------

-   DART initial conditions/restart file, e.g. *dart\_restart*
-   DART namelist file: *input.nml*
-   NOAH namelist file: *namelist.hrldas*
-   NOAH restart file: *noah\_restart.nc*

FILES Written
-------------

-   NOAH restart file: *noah\_restart.nc*
-   *noah\_advance\_information.txt* - which contains a list of "LDASIN"
    files needed to advance the model.

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

-   The site for the offline 2D driver code *HRLDAS v3.4.1* can be found
    at <http://www.ral.ucar.edu/research/land/technology/lsm.php>

[]{#Errors}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

ERROR CODES and CONDITIONS
--------------------------

none - all error messages come from modules that have their own
documentation.

KNOWN BUGS
----------

none

[]{#FuturePlans}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FUTURE PLANS
------------

None.

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
  Contact:          Tim Hoar
  Revision:         \$Revision\$
  Source:           \$URL\$
  Change Date:      \$Date\$
  Change history:   try "svn log" or "svn diff"
  ----------------- -----------------------------


