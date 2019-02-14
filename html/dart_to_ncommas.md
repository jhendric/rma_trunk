[]{#TOP}

PROGRAM *dart\_to\_ncommas*
===========================

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

*dart\_to\_ncommas* is the program that **updates** a ncommas
netCDF-format restart file (usually *ncommas\_restart.nc*) with the
state information contained in a DART output/restart file (e.g.
*perfect\_ics, filter\_ics, ...* ). Only the CURRENT values in the
ncommas restart file will be updated. The DART model time is compared to
the time in the ncommas restart file. If the last time in the restart
file does not match the DART model time, the program issues an error
message and aborts.\
\
From the user perspective, most of the time *dart\_to\_ncommas* will be
used on DART files that have a header containing one time stamp followed
by the model state.\
\
The [dart\_to\_ncommas\_nml](#Namelist) namelist allows
*dart\_to\_ncommas* to read the *assim\_model\_state\_ic* files that
have *two* timestamps in the header. These files are temporarily
generated when DART is used to advance the model. One timestamp is the
'advance\_to' time, the other is the 'valid\_time' of the model state.
In this case, a namelist for ncommas (called *ncommas\_in.DART*) is
written that contains the *&time\_manager\_nml* settings appropriate to
advance ncommas to the time requested by DART. The repository version of
the *advance\_model.csh* script has a section to ensure the proper DART
namelist settings for this case.\
\
Conditions required for successful execution of *dart\_to\_ncommas*:

-   a valid *input.nml* namelist file for DART
-   a valid *ncommas\_vars.nml* namelist file for ncommas - the same one
    used to create the DART state vector, naturally,
-   a DART file (typically *filter\_restart.xxxx* or *filter\_ics.xxxx*)
-   a ncommas restart file (typically *ncommas\_restart.nc*).

Since this program is called repeatedly for every ensemble member, we
have found it convenient to link the DART input file to the default
input filename (*dart\_restart*). The same thing goes true for the
ncommas output filename *ncommas\_restart.nc*.

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
       ncommas_restart_filename     = 'ncommas_restart.nc';
       assimilation_period_days     = 1,
       assimilation_period_seconds  = 0,
       model_perturbation_amplitude = 0.2,
       output_state_vector          = .true.,
       calendar                     = 'Gregorian',
       debug                        = 0
    /

</div>

\

<div class="namelist">

    &dart_to_ncommas_nml
       dart_to_ncommas_input_file = 'dart_restart',
       advance_time_present   = .false.  
    /

</div>

\
\

*dart\_to\_ncommas\_nml* and *model\_nml* are always read from a file
called *input.nml*. The full description of the *model\_nml* namelist is
documented in the [NCOMMAS model\_mod](model_mod.html#Namelist).

<div>

  Item                             Type                 Description
  -------------------------------- -------------------- --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  dart\_to\_ncommas\_input\_file   character(len=128)   The name of the DART file containing the model state to insert into the ncommas restart file.
  advance\_time\_present           logical              If you are converting a DART initial conditions or restart file this should be *.false.*; these files have a single timestamp describing the valid time of the model state. If *.true.* TWO timestamps are expected to be the DART file header. In this case, a namelist for ncommas (called *ncommas\_in.DART*) is created that contains the *&time\_manager\_nml* settings appropriate to advance ncommas to the time requested by DART.

</div>

\
\

*ncommas\_vars\_nml* is always read from a file called
*ncommas\_vars.nml*.

<div>

  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  Item                        Type                                 Description
  --------------------------- ------------------------------------ ----------------------------------------------------------------------------------------------------------------------------------
  ncommas\_state\_variables   character(len=NF90\_MAX\_NAME) ::\   The list of variable names in the NCOMMAS restart file to use to create the DART state vector and their corresponding DART kind.
                              dimension(160)                       
  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\

<div class="namelist">

    &ncommas_vars_nml
       ncommas_state_variables = 'U',   'QTY_U_WIND_COMPONENT',
                                 'V',   'QTY_V_WIND_COMPONENT',
                                 'W',   'QTY_VERTICAL_VELOCITY',
                                 'TH',  'QTY_POTENTIAL_TEMPERATURE',
                                 'DBZ', 'QTY_RADAR_REFLECTIVITY',
                                 'WZ',  'QTY_VERTICAL_VORTICITY',
                                 'PI',  'QTY_EXNER_FUNCTION',
                                 'QV',  'QTY_VAPOR_MIXING_RATIO',
                                 'QC',  'QTY_CLOUDWATER_MIXING_RATIO',
                                 'QR',  'QTY_RAINWATER_MIXING_RATIO',
                                 'QI',  'QTY_ICE_MIXING_RATIO',
                                 'QS',  'QTY_SNOW_MIXING_RATIO',
                                 'QH',  'QTY_GRAUPEL_MIXING_RATIO'
      /

</div>

\
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

-   DART initial conditions/restart file; e.g. *filter\_ic*
-   DART namelist file; *input.nml*
-   ncommas namelist file; *ncommas\_vars.nml*
-   ncommas restart file *ncommas\_restart.nc*

FILES Written
-------------

-   ncommas restart file; *ncommas\_restart.nc*
-   ncommas namelist file; *ncommas\_in.DART*

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

none

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

  ------------------ -----------------------------
  Contact:           DART core group
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


