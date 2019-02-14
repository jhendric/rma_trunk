[]{#TOP}

PROGRAM *text\_to\_obs*
=======================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[DATA SOURCES](#DataSources) / [PROGRAMS](#Programs) /
[DECISIONS](#Decisions) / [REFERENCES](#References) / [ERRORS](#Errors)
/ [PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
========

#### Text File to DART Converter

If you have observations in spreadsheet or column format, in text, with
a single line per observation, then the files this directory are a
template for how to convert these observations into a format suitable
for DART use.

The workflow is usually:

-   read in the needed information about each observation - location,
    time, data value, observation type - from a data source (usually a
    file)
-   call a series of DART library routines to construct a derived type
    that contains all the information about a single observation
-   call another set of DART library routines to put it into a
    time-sorted series
-   repeat the last 2 steps until all observations are processed
-   finally, call a write subroutine that writes out the entire series
    to a file in a format that DART can read in

It is not recommended that you try to mimic the ascii file format by
other means; the format is subject to change and the library routines
will continue to be supported even if the physical format changes.

If your input data is in some kind of format like netCDF or HDF, then
one of the other converters (e.g. the MADIS ones for netCDF) might be a
better starting place for adapting code.

[]{#DataSources}

------------------------------------------------------------------------

DATA SOURCES
------------

This part is up to you. For each observation you will need a location, a
data value, a type, a time, and some kind of error estimate. The error
estimate can be hardcoded in the converter if they are not available in
the input data. See below for more details on selecting an appropriate
error value.

[]{#Programs}

------------------------------------------------------------------------

PROGRAMS
--------

The *text\_to\_obs.f90* file is the source for the main converter
program. Look at the source code where it reads the example data file.
You will almost certainly need to change the "read" statement to match
your data format. The example code reads each text line into a character
buffer and then reads from that buffer to parse up the data items.

To compile and test, go into the work subdirectory and run the
*quickbuild.csh* script to build the converter and a couple of general
purpose utilities. *advance\_time* helps with calendar and time
computations, and the *obs\_sequence\_tool* manipulates DART observation
files once they have been created.

To change the observation types, look in the *DART/obs\_def* directory.
If you can find an obs\_def\_XXX\_mod.f90 file with an appropriate set
of observation types, change the 'use' lines in the converter source to
include those types. Then add that filename in the *input.nml* namelist
file to the &preprocess\_nml namelist, the 'input\_files' variable.
Multiple files can be listed. Then run quickbuild.csh again. It remakes
the table of supported observation types before trying to recompile the
source code.

An example script for converting batches of files is in the
*shell\_scripts* directory. A tiny example data file is in the *data*
directory. These are *NOT* intended to be turnkey scripts; they will
certainly need to be customized for your use. There are comments at the
top of the script saying what options they include, and should be
commented enough to indicate where changes will be likely to need to be
made.

[]{#Decisions}

------------------------------------------------------------------------

DECISIONS YOU MIGHT NEED TO MAKE
--------------------------------

See the discussion in the [observations
introduction](../observations.html#Decisions) page about what options
are available for the things you need to specify. These include setting
a time, specifying an expected error, setting a location, and an
observation type.

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


