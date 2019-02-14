[]{#TOP}

PROGRAM *ncommas\_to\_dart*
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

*ncommas\_to\_dart* is the program that reads a ncommas restart file
(usually *ncommas\_restart.nc*) and creates a DART state vector file
(e.g. *perfect\_ics, filter\_ics, ...* ).\
\
The list of variables used to create the DART state vector are specified
in the *ncommas\_vars.nml* file.\
\
Conditions required for successful execution of *ncommas\_to\_dart*:

-   a valid *input.nml* namelist file for DART
-   a valid *ncommas\_vars.nml* namelist file for ncommas
-   the ncommas restart file mentioned in the
    *input.nml&model\_nml:ncommas\_restart\_filename* variable.

Since this program is called repeatedly for every ensemble member, we
have found it convenient to link the ncommas restart files to the
default input filename (*ncommas\_restart.nc*). The default DART state
vector filename is *dart\_ics* - this may be moved or linked as
necessary.

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

    &ncommas_to_dart_nml
       ncommas_to_dart_output_file = 'dart_ics'  
    /

</div>

\
\

*ncommas\_to\_dart\_nml* and *model\_nml* are always read from a file
called *input.nml*. The full description of the *model\_nml* namelist is
documented in the [NCOMMAS model\_mod](model_mod.html#Namelist).

<div>

  Item                              Type                 Description
  --------------------------------- -------------------- ---------------------------------------------------------------------------------------------------------------------
  ncommas\_to\_dart\_output\_file   character(len=128)   The name of the DART file which contains the updated model state info that should be written into the NCOMMAS file.

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

-   ncommas restart file; *ncommas\_restart.nc*
-   DART namelist files; *input.nml* and *ncommas\_vars.nml*

FILES Written
-------------

-   DART state vector file; e.g. *dart\_ics*

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


