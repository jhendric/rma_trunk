[]{#TOP}

PROGRAM *dwl\_to\_obs*
======================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[DATA SOURCES](#DataSources) / [PROGRAMS](#Programs) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

Overview
========

#### DWL to DART Converter

These are Doppler Wind Lidar measurements which have previously been
extracted from the incoming format and output in ascii format, one pair
of wind component observations per line. This converter reads in the
ascii file and outputs the data in DART observation sequence (obs\_seq)
format.

This is OSSE data from a satellite which is expected to be launched in
2015. Information on the satellite mission is here at
<http://en.wikipedia.org/wiki/ADM-Aeolus>.

The workflow is:

-   read in the needed information about each observation - location,
    time, observation values, obs errors - from an ascii file
-   call a series of DART library routines to construct a derived type
    that contains all the information about a single observation
-   call another set of DART library routines to put it into a
    time-sorted series
-   repeat the last 2 steps until all observations are processed
-   finally, call a write subroutine that writes out the entire series
    to a file in a format that DART can read in

[]{#DataSources}

------------------------------------------------------------------------

DATA SOURCES
------------

Matic Savli at University of Ljubljana has programs which read the
expected instrument formats, do the proper conversions, and write out
ascii lines, one per wind observation.

[]{#Programs}

------------------------------------------------------------------------

PROGRAMS
--------

The *dwl\_to\_obs.f90* file is the source for the main converter
program. There is a sample data file in the "data" directory. The
converter reads each text line into a character buffer and then reads
from that buffer to parse up the data items.

To compile and test, go into the work subdirectory and run the
*quickbuild.csh* script to build the converter and a couple of general
purpose utilities. *advance\_time* helps with calendar and time
computations, and the *obs\_sequence\_tool* manipulates DART observation
files once they have been created.

The observation types are defined in
*DART/obs\_def/obs\_def\_dwl\_mod.f90*. That filename must be added to
the *input.nml* namelist file, to the &preprocess\_nml namelist, the
'input\_files' variable before compiling any program that uses these
observation types. Multiple files can be listed. Then run quickbuild.csh
again. It remakes the table of supported observation types before trying
to recompile the source code.

An example script for converting batches of files is in the
*shell\_scripts* directory. It will need customization before being
used.

[]{#KnownBugs}

------------------------------------------------------------------------

KNOWN BUGS
----------

none

[]{#FuturePlans}

------------------------------------------------------------------------

FUTURE PLANS
------------

none

[]{#Legalese}

------------------------------------------------------------------------

Terms of Use
------------

DART software - Copyright UCAR. This open source software is provided by
UCAR, "as is", without charge, subject to all terms of use at
<http://www.image.ucar.edu/DAReS/DART/DART_download>

  ------------------ -----------------------------
  Contact:           nancy collins
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


