[]{#TOP}

PROGRAM *compute\_error*
========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [MODULES](#Modules) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

Overview
--------

Utility program to compute the time-mean ensemble error and spread in
the same manner that the DART MATLAB diagnostic routine
'plot\_total\_err' does. It runs from the command line, opens no
windows, and outputs several types of numerical results on standard
output. Grep for 'Total' to get the 2 lines with total error and total
spread. Intended for scripts where only the numeric results are wanted
instead of a time-series plot. This routine does not do any weighted
computations.

The default is to compare a True\_State.nc file output from
perfect\_model\_obs to a Prior\_Diag.nc file output from filter. Other
filenames can be specified in the namelist. These files must have at
least one overlapping value in the 'time' array. The statistics will be
done on the overlapping time region only.

The output includes the min and max error and spread values, and the
time index and time value where that occurs. There is also an option to
recompute the time mean ensemble error and spread after skipping the
first N times. This can be useful to skip an initial error spike while
the model is spinning up which can result in a larger than expected
total error.

Namelist interface [*&compute\_error\_nml*](#Namelist) is read from file
*input.nml*.

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

    &compute_error_nml
       truth_file_name   = 'true_state.nc'
       diag_file_name    = 'preassim.nc'
       skip_first_ntimes = 0
      /

</div>

\
\

<div>

  Item                  Type                 Description
  --------------------- -------------------- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  truth\_file\_name     character(len=256)   State-space diagnostic file from the 'perfect\_model\_obs' program.
  diag\_file\_name      character(len=256)   State space diagnostic file output from the 'filter' program.
  skip\_first\_ntimes   integer              If set to a value greater than 0, the error values will be recomputed a second time, skipping the first N times. This can be useful when running an experiment that has an initial error spike as the model spins up and then decays down to a more steady state.

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

    types_mod
    utilities_mod

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   DART diagnosic files (True\_State.nc, Prior\_Diag.nc)
-   compute\_error.nml

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

-   none

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
time dimension error
files must have overlapping time series
The unlimited 'time' dimension must values in common between both files.

</div>

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

The matlab script has an option for doing weighted statistics. This code
does not.

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


