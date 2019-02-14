[]{#TOP}

program *obs\_loop*
===================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[USAGE](#Usage%20Notes) / [NAMELIST](#Namelist) /
[DISCUSSION](#Discussion) / [BUILDING](#Building) / [FILES](#FilesUsed)
/ [REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans)
/ [TERMS OF USE](#Legalese)

Overview
--------

This program is a template that is intended to be modified by the user
to do any desired operations on an observation sequence file.

[]{#Usage Notes}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Usage
-----

This program is intended to be used as a template to read in
observations from one obs\_seq file and write them, optionally modified
in some way, to another obs\_seq file. It can be compiled and run as-is,
but it simply makes an exact copy of the input file.

There are comments in the code (search for *MODIFY HERE* ) where you can
test values, types, times, error values, and either modify them or skip
copying that observation to the output.

There are build files in *observations/utilities/oned* and
*observations/utilities/threed\_sphere* to build the *obs\_loop*
program.

\
\
[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Namelist
--------

This namelist is read from the file *input.nml*. Namelists start with an
ampersand '&' and terminate with a slash '/'. Character strings that
contain a '/' must be enclosed in quotes to prevent them from
prematurely terminating the namelist.

<div class="namelist">

    &obs_loop_nml
       filename_in  = ''
       filename_out = '' 
       print_only   = .false.
       calendar     = 'Gregorian'
       /

</div>

\
\

Items in this namelist set the input and output files.

<div>

  Item            Type                 Description
  --------------- -------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  filename\_in    character(len=256)   Observation sequence file to read
  filename\_out   character(len=256)   Observation sequence file to create and write. If this file exists it will be overwritten.
  print\_only     logical              If .TRUE. then do the work but only print out information about what would be written as output without actually creating the output file.
  calendar        character(len=32)    The string name of a valid DART calendar type. (See the [time\_manager\_mod](../../modules/utilities/time_manager_mod.html) documentation for a list of valid types.) The setting here does not change what is written to the output file; it only changes how the date information is printed to the screen in the informational messages.

</div>

\
\
[]{#Discussion}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Discussion
----------

See the documentation in the obs\_kind and obs\_def modules for things
you can query about an observation, and how to set (overwrite) existing
values.

\
\
[]{#Building}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

BUILDING
--------

There are build files in *observations/utilities/oned* and
*observations/utilities/threed\_sphere* to build the *obs\_loop*
program.

The *preprocess* program must be built and run first to define what set
of observation types will be supported. See the [preprocess
documentation](../../../assimilation_code/programs/preprocess/preprocess.html)
for more details on how to define the list and run it. The
*&preprocess\_nml* namelist in the *input.nml* file must contain files
with definitions for the combined set of all observation types which
will be encountered over all input obs\_seq files.

If you have observation types which are not part of the default list in
the &preprocess\_nml namelist, add them to the input.nml file and then
either run quickbuild.csh or make and run preprocess and then make the
obs\_loop tool.

Usually the directories where executables are built will include a
*quickbuild.csh* script which builds and runs preprocess and then builds
the rest of the executables by executing all files with names starting
with *mkmf\_*.

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Files
-----

  filename    purpose
  ----------- --------------------------------------
  input.nml   to read the &obs\_loop\_nml namelist

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

References
----------

1.  none

[]{#Errors}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Error Codes and Conditions
--------------------------

<div class="errors">

Routine
Message
Comment
obs\_loop
obs\_loop

</div>

KNOWN BUGS
----------

none

[]{#FuturePlans}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Future Plans
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


