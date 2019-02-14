[]{#TOP}

PROGRAM *cice\_to\_obs*
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
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

Overview
========

#### Sea Ice Percentage Observations to DART Converter

This converter reads the binary sea ice observations from the snow and
ice data center files and outputs DART obs\_seq format files. It will
loop over multiple days inside a single run of the converter program.

[]{#DataSources}

------------------------------------------------------------------------

DATA SOURCES
------------

The [National Snow and Ice Data Center](http://nsidc.org/) supplies the
data files read by this converter. (I believe it is [this
format?](http://nsidc.org/data/NSIDC-0051))

[]{#Programs}

------------------------------------------------------------------------

PROGRAMS
--------

The *cice\_to\_obs.f90* file is the source for the main converter
program. More documentation is in the source code file especially around
where the namelist variables are declared.

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


