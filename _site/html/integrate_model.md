[]{#TOP}

PROGRAM *integrate\_model*
==========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [FILES](#FilesUsed) / [REFERENCES](#References)
/ [ERRORS](#Errors) / [PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
--------

Generic main program which can be compiled with a model-specific
*model\_mod.f90* file. The model must provide an *adv\_1step()*
subroutine which advances one copy of the model forward in time.

The executable built by this program can be used by the serial program
*perfect\_model\_obs*, or either the serial or parallel version of the
*filter* program. This program is called by the default script in the
template directory called *advance\_model.csh*, and is selected by
setting the corresponding *"async = "* namelist setting to 2.

This program only advances a single ensemble member per execution and is
expected to be run as a serial program. It can be compiled with the MPI
wrappers and called with mpirun with more than 1 task, however, it will
only call the model advance subroutine from a single task (task 0). This
can be useful in testing various scripting options using simpler and
smaller models in preparation for running a larger parallel model.

[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

There is no namelist for this program.

[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    types_mod
    time_manager_mod
    utilities_mod
    assim_model_mod
    obs_model_mod
    ensemble_manager_mod
    mpi_utilities_mod

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   inputfile (temp\_ic)
-   outputfile (temp\_ud)

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
integrate\_model
none
none

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

This program could have a namelist. Possible items for the namelist
include the names of the input and output files, and the format of the
updated model state in the output files. Right now the names are
hardcoded and the format is always binary (for more accurate numerical
fidelity), but for debugging human-readable ascii files can be more
useful.

A true MPI version of this program could advance multiple ensemble
members at a time, one per MPI task.

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


