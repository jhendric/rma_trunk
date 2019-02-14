[]{#TOP}

PROGRAM *noah\_to\_dart*
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

*noah\_to\_dart* is the program that reads a NOAH restart file (usually
linked to *restart.nc*) and creates a DART output/restart file (e.g.
*perfect\_ics, filter\_ics, ...* ).\
\
The list of variables used to create the DART state vector are specified
in the *input.nml* file.\
\
Conditions required for successful execution of *noah\_to\_dart*:

-   a valid *input.nml* namelist file for DART
-   a valid *namelist.hrldas* namelist file for NOAH
-   the NOAH restart file referenced in the *input.nml*
    *&model\_nml:noah\_netcdf\_filename* variable.

The individual model instances are run in unique directories. This is
also where the converter routines *noah\_to\_dart* and *dart\_to\_noah*
are run. This makes it easy to use a single 'static' name for the input
and output filenames so we do not have to dynamically modify *input.nml*
or *namelist.hrldas*. *advance\_model.csh* is responsibile for linking
the appropriate files to these static filenames.

[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

We adhere to the F90 standard of starting a namelist with an ampersand
'&' and terminating with a slash '/' for all our namelist input.
Character strings that contain a '/' must be enclosed in quotes to
prevent them from prematurely terminating the namelist.

<div class="namelist">

    &noah_to_dart_nml
       noah_to_dart_output_file = 'dart_ics',
       /

    &model_nml
         noah_netcdf_filename         = 'restart.nc',
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

<div>

Contents
Type
Description
noah\_to\_dart\_output\_file
character(len=128)
The name of the DART file containing the model state derived from the
NOAH restart file. *\[default: 'dart\_ics'\]*

</div>

\

<div class="indent1">

The full description of the *model\_nml* namelist is documented in the
[noah model\_mod](model_mod.html#Namelist), but the most important
variables for *noah\_to\_dart* are repeated here.

Contents
Type
Description
noah\_netcdf\_filename
character(len=128)
The name of the NOAH RESTART file to use to create the DART state
vector. For convenience, the *advance\_model.csh* script usually links
the most recent restart file to a static name. *\[default:
'restart.nc'\]*
noah\_state\_variables
character(len=32)::\
dimension(2,40)
The list of variable names in the NOAH restart file to use to create the
DART state vector and their corresponding DART kind. *\[default: see
example below\]*
### Example {#example .indent1}


    &noah_to_dart_nml
       noah_to_dart_output_file = 'dart_ics'
      /

    &model_nml
         noah_netcdf_file     = 'restart.nc',
         noah_state_variables = 'SOIL_T',   'QTY_SOIL_TEMPERATURE',
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

</div>

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

-   noah restart file: *restart.nc*
-   NOAH namelist files: *namelist.hrldas*
-   DART namelist files: *input.nml*

FILES Written
-------------

-   DART initial conditions/restart file; e.g. *dart\_ics*

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


