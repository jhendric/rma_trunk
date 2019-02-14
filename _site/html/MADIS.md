[]{#TOP}

MADIS Data Ingest System
========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[DATA SOURCES](#DataSources) / [PROGRAMS](#Programs) /
[MODULES](#Modules) / [NAMELIST](#Namelist) / [ERRORS](#Errors) /
[FUTURE PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
--------

The [MADIS](http://madis.noaa.gov/) (Meteorological Assimilation Data
Ingest System) service provides access to real-time and archived data of
a variety of types, with added Quality Control (QC) and integration of
data from a variety of sources.

To convert a series of MADIS data files (where different types of
observations are distributed in separate files), one high level view of
the workflow is:

1.  convert each madis file, by platform type, into an obs\_seq file.
    one file in, one file out. no time changes. use the
    *shell\_scripts/madis\_conv.csh* script. there are script options
    for hourly output files, or a single daily output file.
2.  if you aren't using the wrf preprocessing program, you're ready to
    go.
3.  if you do want to do subsequent wrf preprocessing, you need to:
    1.  decide on the windowing. each platform has a different
        convention and if you're going to put them into the wrf
        preprocessing you'll need to have the windowing match. use the
        *shell\_scripts/windowing.csh* script.
    2.  the wrf preprocessing takes a list of files and assumes they
        will all be assimilated at the same time, for superob'ing
        purposes, so it should match the expected assimilation window
        when running filter.

[]{#DataSources}

------------------------------------------------------------------------

DATA SOURCES
------------

[http://madis.noaa.gov](http://madis.noaa.gov/)

There are two satellite wind converter programs; the one in this
directory and one in the [SSEC](../SSEC/SSEC.html) directory. The
observations distributed here come from
[NESDIS](http://www.nesdis.noaa.gov). The SSEC observations are
processed by SSEC itself and will differ from the observations converted
here.

[]{#Programs}

------------------------------------------------------------------------

PROGRAMS
--------

The programs in the *DART/observations/MADIS/* directory extract data
from the distribution files and create DART observation sequence
(obs\_seq) files. Build them in the *work* directory by running the
*./quickbuild.csh* script. In addition to the converters, the
*advance\_time* and *obs\_sequence\_tool* utilities will be built.

There are currently converters for these data types:

  ----------------------------- --------------------------
  ACARS aircraft T,U,V,Q data   convert\_madis\_acars
  Marine surface data           convert\_madis\_marine
  Mesonet surface data          convert\_madis\_mesonet
  Metar data                    convert\_madis\_metar
  Wind Profiler data            convert\_madis\_profiler
  Rawinsonde/Radiosonde data    convert\_madis\_rawin
  Satellite Wind data           convert\_madis\_satwnd
  ----------------------------- --------------------------

Example data files are in the *data* directory. Example scripts for
converting batches of these files are in the *shell\_scripts* directory.
These are *NOT* intended to be turnkey scripts; they will certainly need
to be customized for your use. There are comments at the top of the
scripts saying what options they include, and should be commented enough
to indicate where changes will be likely to need to be made.

Several converters have compile-time choices for outputting various
types of moist variables. Check the source code for more details. Some
converters also read multiple T/F strings from the console (standard
input) to control at run-time what types of observations to convert.
Again, check the source code for more details.

Each converter has hard-coded input and output filenames:

  --------------------------- -------------------- -------------------
  convert\_madis\_acars:      acars\_input.nc      obs\_seq.acars
  convert\_madis\_marine:     marine\_input.nc     obs\_seq.marine
  convert\_madis\_mesonet:    mesonet\_input.nc    obs\_seq.mesonet
  convert\_madis\_metar:      metar\_input.nc      obs\_seq.metar
  convert\_madis\_profiler:   profiler\_input.nc   obs\_seq.profiler
  convert\_madis\_rawin:      rawin\_input.nc      obs\_seq.rawin
  convert\_madis\_satwnd:     satwnd\_input.nc     obs\_seq.satwnd
  --------------------------- -------------------- -------------------

The expected usage pattern is that a script will copy, rename, or make a
symbolic link from the actual input file (which often contains a
timestamp in the name) to the fixed input name before conversion, and
move the output file to an appropriate filename before the next
invocation of the converter. If an existing observation sequence file of
the same output name is found when the converter is run again, it will
open that file and append the next set of observations to it.

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


