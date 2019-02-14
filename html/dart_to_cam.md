[]{#TOP}

PROGRAM *dart\_to\_cam*
=======================

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

*dart\_to\_cam* is the program that reads a DART restart or model
advance file (e.g. *perfect\_ics, filter\_ics, assim\_model\_state\_id
...* ). and overwrites the part of the CAM data in a single CAM restart
file (usually *caminput.nc*) which is in the DART state vector. If you
have multiple input files, you will need to rename the output files as
you create them.

The list of variables extracted from the DART state vector and exported
to the CAM netCDF file is controlled by the set of *input.nml*
*&model\_nml:state\_names\_\** variables.

If the input is a model advance file, containing 2 timestamps (the
current model time and a future time the model should run until), this
program also creates a separate file named *times* that contains three
lines: the advance-to time, the current model time, and the number of
hours to advance. These will need to be extracted and inserted in a CAM
namelist to indicate to CAM how long to run.

This program also updates the *date* and *datesec* variables in the CAM
netcdf file. Generally these are identical times since the assimilation
doesn't change the time of the data, but in case the original file had a
different time that was overwritten in the state vector, it will update
the time for consistency.

Conditions required for successful execution of *dart\_to\_cam*:

-   a valid *input.nml* namelist file for DART
-   a CAM 'phis' netCDF file \[default: *cam\_phis.nc*\]
-   a DART restart file \[default: *dart\_ics*\] (read)
-   a CAM restart file \[default: *caminput.nc*\] (read and written)

Since this program is called repeatedly for every ensemble member, we
have found it convenient to link the DART input and CAM restart files to
the default filenames *dart\_ics* and *caminput.nc*). The output files
may be moved or relinked as necessary.

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

    &dart_to_cam_nml
       dart_to_cam_input_file  = 'dart_ics',
       dart_to_cam_output_file = 'caminput.nc',
       advance_time_present    = .true.,
       /

</div>

\
\

<div>

  Item                          Type                 Description
  ----------------------------- -------------------- ---------------------------------------------------------------------------------------------------------------------------------------------
  dart\_to\_cam\_input\_file    character(len=128)   The name of the DART restart file containing the CAM state.
  dart\_to\_cam\_output\_file   character(len=128)   The name of the CAM restart netcdf file.
  advance\_time\_present        logical              Set to .false. for DART initial condition and restart files. Use the .true. setting for the files written by filter during a model advance.

</div>

\
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
    threed_sphere/location_mod.f90
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

-   DART namelist file; *input.nml*
-   DART initial conditions/restart file; e.g. *dart\_ics* (read)
-   CAM restart file; *caminput.nc* (read and written)
-   CAM "phis" file specified in *&model\_nml::cam\_phis* (normally
    *cam\_phis.nc*)

FILES Written
-------------

-   CAM restart file; *caminput.nc* (read and written)

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

[]{#Bugs}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

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


