[]{#TOP}

PROGRAM *wakeup\_filter*
========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[MODULES](#Modules) / [NAMELIST](#Namelist) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

Overview
--------

Small auxiliary program for use in the "async=4" case where the main
filter program is an MPI program and the model being run with DART is
also an MPI program. The main MPI job script runs each of the model
advances for the ensemble members, and then runs this program to restart
the filter program.

[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    mpi_utilities_mod

[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

There are no namelist options for this program. It must be run as an MPI
program with the same number of tasks as filter was originally started
with.

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

Named pipes (fifo) files are used to synchronize with the main MPI job
run script, to ensure that the filter program and the script do not do a
"busy-wait" in which they consume CPU cycles while they are waiting for
each other. The fifo names are:

-   filter\_to\_model.lock
-   model\_to\_filter.lock
-   filter\_lockNNNNN (where NNNNN is the task number with leading 0s)

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

-   Anderson, J., T. Hoar, K. Raeder, H. Liu, N. Collins, R. Torn,
    and A. Arellano, 2009:\
    The Data Assimilation Research Testbed: A Community Facility. [Bull.
    Amer. Meteor. Soc.]{style="font-style: italic;"},
    [90]{style="font-weight: bold;"}, 1283-1296.\
    [DOI:
    10.1175/2009BAMS2618.1](http://dx.doi.org/10.1175%2F2009BAMS2618.1)

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
block\_task
cannot handle task counts &gt; 99999
Ensemble size must be less than 100,000.
block\_task
initialize\_mpi\_utilities() must be called first
The mpi init routine must be called first
restart\_task
cannot handle task counts &gt; 99999
Ensemble size must be less than 100,000.
restart\_task
initialize\_mpi\_utilities() must be called first
The mpi init routine must be called first

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

Some queueing systems balk at having multiple MPI jobs active at the
same time, even if one is sleeping while the other is running. At some
point filter may exit instead of blocking while the model advances are
running. In that case this program will no longer be needed.

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
  Contact:           Nancy Collins
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


