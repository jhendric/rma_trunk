[]{#TOP}

PROGRAM *snow\_to\_obs*
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
[NAMELIST](#Namelist) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

MODIS Snowcover Fraction Observation Converter
----------------------------------------------

#### Overview

There are several satellite sources for snow observations. Generally the
data is distributed in HDF-EOS format. The converter code in this
directory DOES NOT READ HDF FILES as input. It expects the files to have
been preprocessed to contain text, one line per observation, with
northern hemisphere data only.

[]{#DataSources}

------------------------------------------------------------------------

DATA SOURCES
------------

not sure.

[]{#Programs}

------------------------------------------------------------------------

PROGRAMS
--------

The *snow\_to\_obs.f90* file is the source for the main converter
program.

To compile and test, go into the work subdirectory and run the
*quickbuild.csh* script to build the converter and a couple of general
purpose utilities. *advance\_time* helps with calendar and time
computations, and the *obs\_sequence\_tool* manipulates DART observation
files once they have been created.

This converter creates observations of the "MODIS\_SNOWCOVER\_FRAC"
type.

There is another program in this directory called
*snow\_to\_obs\_netcdf.f90* which is a prototype for reading netcdf
files that contain some metadata and presumably have been converted from
the original HDF. THIS HAS NOT BEEN TESTED but if you have such data,
please contact <dart@ucar.edu> for more assistance. If you write
something that reads the HDF-EOS MODIS files directly, please, please
contact us! Thanks.

[]{#Namelist}

------------------------------------------------------------------------

NAMELIST
--------

This namelist is read from the file *input.nml*. Namelists start with an
ampersand '&' and terminate with a slash '/'. Character strings that
contain a '/' must be enclosed in quotes to prevent them from
prematurely terminating the namelist.

<div class="namelist">

    &snow_to_obs_nml
      longrid         = 360,
      latgrid         = 90, 
      year            = 2000, 
      doy             = 1,
      snow_input_file = 'snowdata.input', 
      missing_value   = -20.0, 
      debug           = .false.
    /

</div>

  Item                Type                 Description
  ------------------- -------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  longrid             integer              The number of divisions in the longitude dimension.
  latgrid             integer              The number of divisions in the latitude dimension. This converter assumes the data is for the northern hemisphere only. A namelist item could be added to select northern verses southern hemisphere if needed.
  year                integer              The year number of the data.
  doy                 integer              The day number in the year. Valid range 1 to 365 in a non-leap year, 1 to 366 in a leap year.
  snow\_input\_file   character(len=128)   The name of the input file.
  missing\_value      real(r8)             The value used to mark missing data.
  debug               logical              If set to .true. the converter will print out more information as it does the conversion.

[]{#KnownBugs}

------------------------------------------------------------------------

KNOWN BUGS
----------

This program is hardcoded to read only northern hemisphere data. It
should handle global values.

[]{#FuturePlans}

------------------------------------------------------------------------

FUTURE PLANS
------------

This program should use the HDF-EOS libraries to read the native MODIS
granule files. Right now the ascii intermediate files contain no
metadata, so if the namelist values don't match the actual division of
the globe, bad things will happen.

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


