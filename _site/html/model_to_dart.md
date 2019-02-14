[]{#TOP}

PROGRAM *model\_to\_dart* for MPAS OCN
======================================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
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

*model\_to\_dart* is the program that reads an MPAS OCN analysis file
(nominally named *mpas\_restart.nc*) and creates a DART state vector
file (e.g. *perfect\_ics, filter\_ics, ...* ). The MPAS analysis files
have a **Time** UNLIMITED Dimension, which indicates there may (at some
point) be more than one timestep in the file. The DART routines are
currently designed to use the LAST timestep. If the Time dimension of
length 3, we use the third timestep. A warning message is issued and
indicates exactly the time being used.\
\
*input.nml&mpas\_vars\_nml* defines the list of MPAS variables used to
build the DART state vector. This namelist is more fully described in
the [MPAS model\_mod.html](model_mod.html) documentation. For example:

    &mpas_vars_nml
       mpas_state_variables = 'temperature',  'QTY_TEMPERATURE',
                              'salinity',     'QTY_SALINITY',
                              'rho',          'QTY_DENSITY',
                              'u',            'QTY_EDGE_NORMAL_SPEED',
                              'h',            'QTY_SEA_SURFACE_HEIGHT'
                              'tracer1',      'QTY_TRACER_CONCENTRATION'
       /

Conditions required for successful execution of *model\_to\_dart* are:

-   a valid *input.nml* namelist file for DART which contains
-   a MPAS OCN analysis file (nominally named *mpas\_analysis.nc*).

Since this program is called repeatedly for every ensemble member, we
have found it convenient to link the MPAS OCN analysis files to a static
input filename (e.g. *mpas\_analysis.nc*). The default DART filename is
*dart\_ics* - this may be moved or linked as necessary.

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

    &model_to_dart_nml
       model_to_dart_output_file = 'dart_ics'
       /

</div>

\

<div class="namelist">

    &model_nml
       model_analysis_filename  = 'mpas_analysis.nc'
       /

    (partial namelist)

</div>

\

<div class="namelist">

    &mpas_vars_nml
       mpas_state_variables = '',
       mpas_state_bounds = '',
       /

</div>

\
\

The model\_to\_dart namelist includes:

<div>

  Item                            Type                 Description
  ------------------------------- -------------------- -------------------------------------------------------------------------------------------
  model\_to\_dart\_output\_file   character(len=128)   The name of the DART file containing the model state derived from the MPAS analysis file.

</div>

\

Two more namelists need to be mentioned. The
[model\_nml](model_mod.html#Namelist) namelist specifies the MPAS
analysis file to be used as the source. The
[mpas\_vars\_nml](model_mod.html#mpas_vars_nml) namelist specifies the
MPAS variables that will comprise the DART state vector.

For example:

    &mpas_vars_nml
       mpas_state_variables = 'temperature',  'QTY_TEMPERATURE',
                              'salinity',     'QTY_SALINITY',
                              'rho',          'QTY_DENSITY',
                              'u',            'QTY_EDGE_NORMAL_SPEED',
                              'h',            'QTY_SEA_SURFACE_HEIGHT'
                              'tracer1',      'QTY_TRACER_CONCENTRATION'
       /

\
[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    assim_model_mod.f90
    types_mod.f90
    location_mod.f90
    model_to_dart.f90
    model_mod.f90
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

-   MPAS analysis file; *mpas\_analysis.nc*
-   DART namelist file; [input.nml](work/input.nml)

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


