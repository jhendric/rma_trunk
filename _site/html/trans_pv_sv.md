[]{#TOP}

[MODULES](#Modules) / [NAMELIST](#Namelist) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

PROGRAM *trans\_pv\_sv*
=======================

\$Id\$

*trans\_pv\_sv* is responsible for converting the ocean model 'snapshot'
files to a DART 'initial conditions' file. In order to do that, the
valid time for the snapshot files must be calculated from several pieces
of information: the filename contains a timestep index, the
*data&PARM03* namelist contains information about the amount of time per
timestep, and the *data.cal&CAL\_NML* namelist contains the start time.
Additionally, the grid characteristics must be read from *data&PARM04*.
Consequently, the files *data*, and *data.cal* as well as the general
*input.nml* are needed in addition to the snapshot files.\
\
This program has a number of options that are driven from namelists and
**one** piece of input read from STDIN: the integer representing the
timestep index of the snapshot file set.

Usage
-----

The output filename is hardwired to that expected by *filter*. This
example creates an output file named *assim\_model\_state\_ud* from the
following files in the local directory:\
\
*S.0000000096.data*\
*T.0000000096.data*\
*U.0000000096.data*\
*V.0000000096.data*\
*Eta.0000000096.data*

<div class="unix">

./trans\_pv\_sv &lt; 96

</div>

\
[]{#Modules}

------------------------------------------------------------------------

MODULES USED
------------

    types_mod
    utilities_mod
    model_mod
    assim_model_mod
    time_manager_mod

[]{#Namelist}

------------------------------------------------------------------------

NAMELIST
--------

This program has no namelist of its own, but some of the underlying
modules require namelists. To avoid duplication and, possibly, some
inconsistency in the documentation, only a list of the required
namelists is provided here, with a hyperlink to the full documentation
for each namelist.

  Namelist                                                                                               Primary Purpose
  ------------------------------------------------------------------------------------------------------ --------------------------------------------------------------------------
  [utilities\_nml](../../assimilation_code/modules/utilities/utilities_mod.html#Namelist)                set the termination level and file name for the run-time log
  [assim\_model\_mod\_nml](../../assimilation_code/modules/assimilation/assim_model_mod.html#Namelist)   write DART restart files in binary or ASCII
  [model\_nml](model_mod.html#Namelist)                                                                  write netCDF files with prognostic variables
  [CAL\_NML](model_mod.html#namelist_cal_nml)                                                            determine start time of the ocean model
  [PARM03](model_mod.html#namelist_parm03)                                                               the amount of time per model timestep for deciphering snapshot filenames
  [PARM04](model_mod.html#namelist_parm04)                                                               ocean model grid parameters

[]{#FilesUsed}

------------------------------------------------------------------------

FILES
-----

-   input namelist files: *data, data.cal, input.nml*
-   input snapshot files:
    *\[S,T,U,V,Eta\].nnnnnnnnnn.\[data\[,.meta\]\]*
-   output initial conditions file: *assim\_model\_state\_ud*

[]{#References}

------------------------------------------------------------------------

REFERENCES
----------

-   none

[]{#Errors}

------------------------------------------------------------------------

ERROR CODES and CONDITIONS
--------------------------

The most common problem is trying to read the Fortran direct-access
big-endian snapshot files on a little-endian architecture. This can
manifest itself in very misleading ways. Make sure you have the right
compiler settings to be able to read these files. There is no one error
message that indicates the read was unsuccessful.\
\
The read takes place in
[model\_mod:read\_snapshot()](model_mod.html#read_snapshot).

<div class="errors">

Routine
Message
Comment
trans\_sv\_pv
unable to read timestep from stdin.
look at the example in the 'Usage' section.

</div>

KNOWN BUGS
----------

There are no known bugs.

[]{#FuturePlans}

------------------------------------------------------------------------

FUTURE PLANS
------------

None at this time. Feel free to suggest improvements.

[]{#PrivateComponents}

------------------------------------------------------------------------

PRIVATE COMPONENTS
------------------

N/A

[]{#Legalese}

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


