[]{#TOP}

PROGRAM *cam\_to\_dart*
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

*cam\_to\_dart* is the program that reads a CAM restart file (usually
*caminput.nc*) and creates a single DART output/restart file (e.g.
*perfect\_ics, filter\_ics, ...* ). If you have multiple input files,
you will need to rename the output files as you create them.\
\
The list of variables extracted from the CAM netCDF file and conveyed to
DART is controlled by the set of *input.nml*
*&model\_nml:state\_names\_\** variables. The *date* and *datesec*
variables in the CAM netcdf file are used to specify the valid time of
the state vector. The time may be changed with the
[restart\_file\_tool](../../assimilation_code/programs/restart_file_tool/restart_file_tool.html)
if desired.\
\
Some CAM restart files are from climatological runs and have a valid
time that predates the use of the Gregorian calendar. In such instances,
the year component of the original date is changed to be a valid
Gregorian year (by adding 1601). A warning is issued to the screen and
to the logfile. Please use the
[restart\_file\_tool](../../assimilation_code/programs/restart_file_tool/restart_file_tool.html)
to change this time.\
\
Conditions required for successful execution of *cam\_to\_dart*:

-   a valid *input.nml* namelist file for DART
-   a CAM 'phis' netCDF file \[default: *cam\_phis.nc*\]
-   a CAM restart file \[default: *caminput.nc*\].

Since this program is called repeatedly for every ensemble member, we
have found it convenient to link the CAM restart files to the default
input filename (*caminput.nc*). The default DART output filename is
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

    &cam_to_dart_nml
       cam_to_dart_input_file  = 'caminput.nc',
       cam_to_dart_output_file = 'dart_ics', 
       /

</div>

\
\

<div>

  Item                          Type                 Description
  ----------------------------- -------------------- -----------------------------------------------------------------------------------------
  cam\_to\_dart\_input\_file    character(len=128)   The name of the DART file containing the CAM state.
  cam\_to\_dart\_output\_file   character(len=128)   The name of the DART file containing the model state derived from the CAM restart file.

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
-   CAM restart file; *caminput.nc*
-   CAM "phis" file specified in *&model\_nml::cam\_phis* (normally
    *cam\_phis.nc*)

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


