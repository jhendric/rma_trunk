[]{#TOP}

Oklahoma Mesonet MDF Data
=========================

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

Program to convert Oklahoma Mesonet MDF files into DART observation
sequence files.\

[]{#DataSources}

------------------------------------------------------------------------

DATA SOURCES
------------

The observation files can be obtained from the Oklahoma Mesonet archive
using urls of the format:
<http://www.mesonet.org/index.php/dataMdfMts/dataController/getFile/YYYYMMDDHHMM/mdf/TEXT>
where YYYYMMDDHHMM is the date and time of the desired set of
observations. Files are available every 5 minutes.

If you are located outside of Oklahoma or are going to use this for a
non-research purpose see this web page for information about access:
<http://www.mesonet.org/index.php/site/about/data_access_and_pricing>

Static fields are drawn from the station description file provided by
the OK Mesonet. Update the local file from:
<http://www.mesonet.org/index.php/api/siteinfo/from_all_active_with_geo_fields/format/csv>

[]{#Programs}

------------------------------------------------------------------------

PROGRAMS
--------

The programs in the *DART/observations/ok\_mesonet/* directory extract
data from the distribution files and create DART observation sequence
(obs\_seq) files. Build them in the *work* directory by running the
*./quickbuild.csh* script. In addition to the converters, the
*advance\_time* and *obs\_sequence\_tool* utilities will be built.

The converter is a preliminary version which has no namelist inputs. It
has hard-coded input and output filenames. It always reads a data file
named *okmeso\_mdf.in* and creates an output file named
*obs\_seq.okmeso*. The converter also requires a text file with the
location of all the observating stations, called *geoinfo.csv*.

The converter creates observations of the following types:

-   LAND\_SFC\_ALTIMETER
-   LAND\_SFC\_U\_WIND\_COMPONENT
-   LAND\_SFC\_V\_WIND\_COMPONENT
-   LAND\_SFC\_TEMPERATURE
-   LAND\_SFC\_SPECIFIC\_HUMIDITY
-   LAND\_SFC\_DEWPOINT
-   LAND\_SFC\_RELATIVE\_HUMIDITY

Example data files are in the *data* directory. Example scripts for
converting batches of these files are in the *shell\_scripts* directory.
These are *NOT* intended to be turnkey scripts; they will certainly need
to be customized for your use. There are comments at the top of the
scripts saying what options they include, and should be commented enough
to indicate where changes will be likely to need to be made.

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


