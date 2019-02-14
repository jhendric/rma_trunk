[]{#TOP}

program *create\_obs\_sequence*
===============================

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

This program creates an observation sequence file using values read from
standard input. It is typically used to create synthetic observations,
or shorter sequences of observations (although there is no limit on the
number of observations). For creating observation sequence files
directly from large, real-world observation datasets, see the
[observations](../../../observations/obs_converters/observations.html)
directory.

This program can be run interactively (input from a terminal), or input
files can be created with a text editor, perl or matlab script, or any
other convenient method, and then run with standard input redirected
from this file. The latter method is most commonly used to create larger
observation sequence files for perfect model applications.

The program can create complete observation sequences ready to be
assimilated, or it can create observations with only partial data which
is later filled in by another program. Each observation needs to have a
type, location, time, expected error, and optionally a data value and/or
a quality control indicator. For perfect model applications, it is
usually convenient to define 0 quality control fields and 0 copies of
the data for each observation. The output of create\_obs\_sequence can
be read by
[perfect\_model\_obs](../../../assimilation_code/programs/perfect_model_obs/perfect_model_obs.html)
which will then create a synthetic (perfect\_model) observation sequence
complete with two copies of the data for each observation: the observed
value and the 'true' value.

Another common approach for perfect model applications is to use
create\_obs\_sequence to define a set of observation locations and
types, and where observations will be repeatedly sampled in time. When
running create\_obs\_sequence, specify a single observation for each
different location and type, with 0 copies of data and giving all the
observations the same time. Then the program
[create\_fixed\_network\_seq](../create_fixed_network_seq/create_fixed_network_seq.html)
can read the output of create\_obs\_sequence and create an observation
sequence file that will contain the set of input observations at a
number of different times. This models a fixed observation station,
observing the system at some frequency in time.

This program can also create what are called "identity observations".
These are observations located directly at one of the state variables,
so that computing the value requires no model interpolation but simply
returns the actual state variable value. To specify these types of
observations, the convention is to put in the negative index number for
the offset of that state variable in the state vector. By specifying the
index both the observation kind and location are defined by the kind and
location of that state variable.

The types of observations which can be created by this program is
controlled by the observation types built into the source files created
by the
[preprocess](../../../assimilation_code/programs/preprocess/preprocess.html)
program. The preprocess namelist sets the available observation types,
and must be run each time it is changed, and then the
create\_obs\_sequence program must be recompiled to incorporate the
updated source files.

[]{#OtherModulesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    utilities_mod
    obs_sequence_mod
    assim_model_mod

[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

This program does not use a namelist. All user input is prompted for at
the command line.

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   A file containing the output sequence is created.\
    (*set\_def.out* is the recommended name)

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


