[]{#TOP}

PROGRAM *gitm\_to\_dart*
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

The [Global Ionosphere Thermosphere Model
(GITM)](http://ccmc.gsfc.nasa.gov/models/modelinfo.php?model=GITM) is a
3-dimensional spherical code that models the Earth's thermosphere and
ionosphere system using a stretched grid in latitude and altitude. For a
fuller description of using GITM within DART, please see the [DART GITM
model documentation](model_mod.html).\
\
*gitm\_to\_dart* is the program that reads GITM restart files (i.e.
*b?????.rst*) and creates a DART output/restart file
(e.g. *perfect\_ics, filter\_ics, ... *).\
\
The list of variables used to create the DART state vector are specified
in the *input.nml* file.\
\
Conditions required for successful execution of *gitm\_to\_dart*:

-   a valid *input.nml* namelist file for DART
-   a valid *UAM.in* control file for GITM
-   a set of *b?????.rst* data files for GITM
-   a *header.rst* file for GITM
-   the DART/GITM interfaces must be compiled in a manner consistent
    with the GITM data and control files. The following GITM source
    files are required to build *any* DART interface:
    -   models/gitm/GITM2/src/ModConstants.f90
    -   models/gitm/GITM2/src/ModEarth.f90
    -   models/gitm/GITM2/src/ModKind.f90
    -   models/gitm/GITM2/src/ModSize.f90
    -   models/gitm/GITM2/src/ModTime.f90
    -   models/gitm/GITM2/src/time\_routines.f90

    Versions of these are included in the DART release. *ModSize.f90*,
    in particular, must match what was used to create the *b????.rst*
    files.

The individual model instances are run in unique directories. This is
also where the converter routines *gitm\_to\_dart* and *dart\_to\_gitm*
are run. This makes it easy to use a single 'static' name for the input
and output filenames. *advance\_model.csh* is responsibile for linking
the appropriate files to these static filenames.

The simplest way to test the converter is to compile GITM and run a
single model state forward using *work/clean.sh*. To build GITM ...
download GITM and unpack the code into *DART/models/gitm/GITM2* and
follow these instructions:

<div class="unix">

    cd models/gitm/GITM2
    ./Config.pl -install -compiler=ifortmpif90 -earth
    make
    cd ../work
    ./clean.sh 1 1 0 150.0 170.0 1.0 

</div>

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

    &gitm_to_dart_nml
       gitm_to_dart_output_file = 'dart_ics',
       /

    &model_nml
       gitm_restart_dirname         = 'advance_temp_e1/UA/restartOUT',
       assimilation_period_days     = 0,
       assimilation_period_seconds  = 1800,
       model_perturbation_amplitude = 0.2,
       output_state_vector          = .false.,
       calendar                     = 'Gregorian',
       debug                        = 0,
       gitm_state_variables = 'Temperature',            'QTY_TEMPERATURE',
                              'eTemperature',           'QTY_TEMPERATURE_ELECTRON',
                              'ITemperature',           'QTY_TEMPERATURE_ION',
                              'iO_3P_NDensityS',        'QTY_DENSITY_NEUTRAL_O3P',
       ...

</div>

Contents

Type

Description

gitm\_to\_dart\_output\_file   

character(len=128)  

The name of the DART file containing the model state derived from the
GITM restart files.

\

The full description of the *model\_nml* namelist is documented in the
[gitm model\_mod](model_mod.html#Namelist), but the most important
variable for *gitm\_to\_dart* is repeated here.

Contents

Type

Description

gitm\_restart\_dirname  

character(len=256)  

The name of the directory containing the GITM restart files and runtime
control information.

gitm\_state\_variables 

character(len=32),  \
dimension(2,80)

The list of variable names in the gitm restart file to use to create the
DART state vector and their corresponding DART kind. The default list is
specified in [model\_mod.nml](model_mod.nml)

[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    obs_def_upper_atm_mod.f90
    assim_model_mod.f90
    types_mod.f90
    location/threed_sphere/location_mod.f90
    models/gitm/GITM2/src/ModConstants.f90
    models/gitm/GITM2/src/ModEarth.f90
    models/gitm/GITM2/src/ModKind.f90
    models/gitm/GITM2/src/ModSize.f90
    models/gitm/GITM2/src/ModTime.f90
    models/gitm/GITM2/src/time_routines.f90
    models/gitm/dart_gitm_mod.f90
    models/gitm/gitm_to_dart.f90
    models/gitm/model_mod.f90
    null_mpi_utilities_mod.f90
    obs_kind_mod.f90
    random_seq_mod.f90
    time_manager_mod.f90
    utilities_mod.f90

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES Read
----------

-   gitm restart files: *b????.rst*
-   gitm control files: *header.rst*
-   gitm control files: *UAM.in.rst*
-   DART namelist file: *input.nml*

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

-   The official *GITM* site is: can be found at
    [ccmc.gsfc.nasa.gov/models/modelinfo.php?model=GITM](http://ccmc.gsfc.nasa.gov/models/modelinfo.php?model=GITM)

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


