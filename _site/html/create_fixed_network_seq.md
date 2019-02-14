[]{#TOP}

program *create\_fixed\_network\_seq*
=====================================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[MODULES](#Modules) / [FILES](#FilesUsed) / [REFERENCES](#References) /
[ERRORS](#Errors) / [PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
--------

Reads in an observation sequence file and creates a second observation
sequence file. Any time information in the input file is ignored
entirely. All of the observations in the input file define a set of
observations. The output sequence replicates this set multiple times,
either with a fixed period in time or at arbitrarily selected times. The
program is driven by input from standard input, either the terminal or a
text file.

First, one must select either a regularly repeating time sequence of
observations (option 1) or an arbitrarily repeating sequence (option 2).
For the fixed period, the total number of observation times, the first
observation time and the period of the observations is input and an
output observation sequence is generated. For the arbitrary period, the
user is queried for the number of observing times and then a set of
monotonically increasing times. Finally, the user selects a file name
(traditionally obs\_seq.in) to which the output file is written. The
format of the output file is controlled by the namelist options in
[obs\_sequence\_mod](../../modules/observations/obs_sequence_mod.html#Namelist).

Any data values or quality control flags associated with the input set
are replicated to the output, but this program is typically used with
perfect model experiments to create observations without data, which are
then filled in by running
[perfect\_model\_obs](../../../assimilation_code/programs/perfect_model_obs/perfect_model_obs.html).

[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    types_mod
    utilities_mod
    obs_def_mod
    obs_sequence_mod
    time_manager_mod
    model_mod

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   Input observation sequence (set\_def.out is standard).
-   Output observation sequence (obs\_seq.in is standard).

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

none

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


