[]{#TOP}

Total Precipitable Water Observations
=====================================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[DATA SOURCES](#DataSources) / [PROGRAMS](#Programs) /
[NAMELIST](#Namelist) / [KNOWN BUGS](#KnownBugs) / [FUTURE
PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
--------

Several satellites contain instruments that return observations of
integrated Total Precipitable Water (TPW). There are two
[MODIS](http://modis.gsfc.nasa.gov/) Spectroradiometers, one aboard the
[TERRA](http://terra.nasa.gov/) satellite, and the other aboard the
[AQUA](http://aqua.nasa.gov/) satellite. There is also an
[AMSR-E](http://wwwghcc.msfc.nasa.gov/AMSR/) instrument on the AQUA
satellite.

These instruments produce a variety of data products which are generally
distributed in HDF format using the HDF-EOS libraries. The converter
code in this directory IS NOT USING THESE FILES AS INPUT. The code is
expecting to read ASCII TEXT files, which contain one line per
observation, with the latitude, longitude, TPW data value, and the
observation time. The Fortran read line is:

          read(iunit, '(f11.6, f13.5, f10.4, 4x, i4, 4i3, f7.3)') &
                    lat, lon, tpw, iyear, imonth, iday, ihour, imin, seconds

No program to convert between the HDF and text files is currently
provided. Contact <dart@ucar.edu> for more information if you are
interested in using this converter.

[]{#DataSources}

------------------------------------------------------------------------

DATA SOURCES
------------

This converter reads files produced as part of a data research effort.
Contact <dart@ucar.edu> for more information if you are interested in
this data.

Alternatively, if you can read HDF-EOS files and output a text line per
observation in the format listed above, then you can use this converter
on TPW data from any MODIS file.

[]{#Programs}

------------------------------------------------------------------------

PROGRAMS
--------

The programs in the *DART/observations/tpw* directory extract data from
the distribution text files and create DART observation sequence
(obs\_seq) files. Build them in the *work* directory by running the
*./quickbuild.csh* script. In addition to the converters, several other
general observation sequence file utilities will be built.

Generally the input data comes in daily files, with the string YYYYMMDD
(year, month, day) as part of the name. This converter has the option to
loop over multiple days within the same month and create an output file
per day.

Like many kinds of satellite data, the TWP data is dense and generally
needs to be subsampled or averaged (super-ob'd) before being used for
data assimilation. This converter will average in both space and time.
There are 4 namelist items (see the [namelist](#Namelist) section below)
which set the centers and widths of time bins for each day. All
observations within a single time bin are eligible to be averaged
together. The next available observation in the bin is selected and any
other remaining observations in that bin that are within delta latitude
and delta longitude of it are averaged in both time and space. Then all
observations which were averaged are removed from the bin, so each
observation is only averaged into one output observation. Observations
that are within delta longitude of the prime meridian are handled
correctly by averaging observations on both sides of the boundary.

It is possible to restrict the output observation sequence to contain
data from a region of interest using namelist settings. If your region
spans the Prime Meridian min\_lon can be a larger number than max\_lon.
For example, a region from 300 E to 40 E and 60 S to 30 S (some of the
South Atlantic), specify *min\_lon = 300, max\_lon = 40, min\_lat = -60,
max\_lat = -30*. So 'min\_lon' sets the western boundary, 'max\_lon' the
eastern.

The specific type of observation created in the output observation
sequence file can be select by namelist.
"MODIS\_TOTAL\_PRECIPITABLE\_WATER" is the most general term, or a more
satellite-specific name can be chosen. The choice of which observations
to assimilate or evaluate are made using this name. The
observation-space diagnostics also aggregate statistics based on this
name.

[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

This namelist is read from the file *input.nml*. Namelists start with an
ampersand '&' and terminate with a slash '/'. Character strings that
contain a '/' must be enclosed in quotes to prevent them from
prematurely terminating the namelist.

<div class="namelist">

    &convert_tpw_nml
       start_year          = 2008
       start_month         = 1
       start_day           = 1
       total_days          = 31
       max_obs             = 150000
       time_bin_start      = 0.0  
       time_bin_interval   = 0.50
       time_bin_half_width = 0.25
       time_bin_end        = 24.0
       delta_lat_box       = 1.0
       delta_lon_box       = 1.0
       min_lon             =   0.0
       max_lon             = 360.0
       min_lat             = -90.0
       max_lat             =  90.0
       ObsBase             = '../data'
       InfilePrefix        = 'datafile.'
       InfileSuffix        = '.txt'
       OutfilePrefix       = 'obs_seq.'
       OutfileSuffix       = ''
       observation_name    = 'MODIS_TOTAL_PRECIPITABLE_WATER'
     /

</div>

+-----------------------+-----------------------+-----------------------+
| Item                  | Type                  | Description           |
+=======================+=======================+=======================+
| start\_year           | integer               | The year for the      |
|                       |                       | first day to be       |
|                       |                       | converted. (The       |
|                       |                       | converter will        |
|                       |                       | optionally loop over  |
|                       |                       | multiple days in the  |
|                       |                       | same month.)          |
+-----------------------+-----------------------+-----------------------+
| start\_month          | integer               | The month number for  |
|                       |                       | the first day to be   |
|                       |                       | converted. (The       |
|                       |                       | converter will        |
|                       |                       | optionally loop over  |
|                       |                       | multiple days in the  |
|                       |                       | same month.)          |
+-----------------------+-----------------------+-----------------------+
| start\_day            | integer               | The day number for    |
|                       |                       | the first day to be   |
|                       |                       | converted. (The       |
|                       |                       | converter will        |
|                       |                       | optionally loop over  |
|                       |                       | multiple days in the  |
|                       |                       | same month.)          |
+-----------------------+-----------------------+-----------------------+
| total\_days           | integer               | The number of days to |
|                       |                       | be converted. (The    |
|                       |                       | converter will        |
|                       |                       | optionally loop over  |
|                       |                       | multiple days in the  |
|                       |                       | same month.) The      |
|                       |                       | observations for each |
|                       |                       | day will be created   |
|                       |                       | in a separate output  |
|                       |                       | file which will       |
|                       |                       | include the YYYYMMDD  |
|                       |                       | date as part of the   |
|                       |                       | output filename.      |
+-----------------------+-----------------------+-----------------------+
| max\_obs              | integer               | The largest number of |
|                       |                       | obs in the output     |
|                       |                       | file. If you get an   |
|                       |                       | error, increase this  |
|                       |                       | number and run again. |
+-----------------------+-----------------------+-----------------------+
| time\_bin\_start      | real(r8)              | The next four         |
|                       |                       | namelist values       |
|                       |                       | define a series of    |
|                       |                       | time intervals that   |
|                       |                       | define time bins      |
|                       |                       | which are used for    |
|                       |                       | averaging. The input  |
|                       |                       | data from the         |
|                       |                       | satellite is very     |
|                       |                       | dense and generally   |
|                       |                       | the data values need  |
|                       |                       | to be subsetted in    |
|                       |                       | some way before       |
|                       |                       | assimilating. All     |
|                       |                       | observations in the   |
|                       |                       | same time bin are     |
|                       |                       | eligible to be        |
|                       |                       | averaged in space if  |
|                       |                       | they are within the   |
|                       |                       | latitude/longitude    |
|                       |                       | box. The input files  |
|                       |                       | are distributed as    |
|                       |                       | daily files, so use   |
|                       |                       | care when defining    |
|                       |                       | the first and last    |
|                       |                       | bins of the day. The  |
|                       |                       | units are in hours.   |
|                       |                       | This item defines the |
|                       |                       | midpoint of the first |
|                       |                       | bin.                  |
+-----------------------+-----------------------+-----------------------+
| time\_bin\_interval   | real(r8)              | Increment added the   |
|                       |                       | time\_bin\_start to   |
|                       |                       | compute the center of |
|                       |                       | the next time bin.    |
|                       |                       | The units are in      |
|                       |                       | hours.                |
+-----------------------+-----------------------+-----------------------+
| time\_bin\_half\_widt | real(r8)              | The amount of time    |
| h                     |                       | added to and          |
|                       |                       | subtracted from the   |
|                       |                       | time bin center to    |
|                       |                       | define the full bin.  |
|                       |                       | The units are in      |
|                       |                       | hours.                |
+-----------------------+-----------------------+-----------------------+
| time\_bin\_end        | real(r8)              | The center of the     |
|                       |                       | last bin of the day.  |
|                       |                       | The units are in      |
|                       |                       | hours.                |
+-----------------------+-----------------------+-----------------------+
| delta\_lat\_box       | real(r8)              | For all observations  |
|                       |                       | in the same time bin, |
|                       |                       | the next available    |
|                       |                       | observation is        |
|                       |                       | selected. All other   |
|                       |                       | observations in that  |
|                       |                       | bin that are within   |
|                       |                       | delta latitude or     |
|                       |                       | longitude of it are   |
|                       |                       | averaged together and |
|                       |                       | a single observation  |
|                       |                       | is output.            |
|                       |                       | Observations which    |
|                       |                       | are averaged with     |
|                       |                       | others are removed    |
|                       |                       | from the bin and so   |
|                       |                       | only contribute to    |
|                       |                       | the output data once. |
|                       |                       | The units are         |
|                       |                       | degrees.              |
+-----------------------+-----------------------+-----------------------+
| delta\_lon\_box       | real(r8)              | See delta\_lat\_box   |
|                       |                       | above.                |
+-----------------------+-----------------------+-----------------------+
| min\_lon              | real(r8)              | The output            |
|                       |                       | observations can be   |
|                       |                       | constrained to only   |
|                       |                       | those which lie       |
|                       |                       | between two           |
|                       |                       | longitudes and two    |
|                       |                       | latitudes. If         |
|                       |                       | specified, this is    |
|                       |                       | the western-most      |
|                       |                       | longitude. The units  |
|                       |                       | are degrees, and      |
|                       |                       | valid values are      |
|                       |                       | between 0.0 and       |
|                       |                       | 360.0. To define a    |
|                       |                       | box that crosses the  |
|                       |                       | prime meridian        |
|                       |                       | (longitude = 0.0) it  |
|                       |                       | is legal for this     |
|                       |                       | value to be larger    |
|                       |                       | than max\_lon.        |
|                       |                       | Observations on the   |
|                       |                       | boundaries are        |
|                       |                       | included in the       |
|                       |                       | output.               |
+-----------------------+-----------------------+-----------------------+
| max\_lon              | real(r8)              | The output            |
|                       |                       | observations can be   |
|                       |                       | constrained to only   |
|                       |                       | those which lie       |
|                       |                       | between two           |
|                       |                       | longitudes and two    |
|                       |                       | latitudes. If         |
|                       |                       | specified, this is    |
|                       |                       | the eastern-most      |
|                       |                       | longitude. The units  |
|                       |                       | are degrees, and      |
|                       |                       | valid values are      |
|                       |                       | between 0.0 and       |
|                       |                       | 360.0. To define a    |
|                       |                       | box that crosses the  |
|                       |                       | prime meridian        |
|                       |                       | (longitude = 0.0) it  |
|                       |                       | is legal for this     |
|                       |                       | value to be smaller   |
|                       |                       | than min\_lon.        |
|                       |                       | Observations on the   |
|                       |                       | boundaries are        |
|                       |                       | included in the       |
|                       |                       | output.               |
+-----------------------+-----------------------+-----------------------+
| min\_lat              | real(r8)              | The output            |
|                       |                       | observations can be   |
|                       |                       | constrained to only   |
|                       |                       | those which lie       |
|                       |                       | between two           |
|                       |                       | longitudes and two    |
|                       |                       | latitudes. If         |
|                       |                       | specified, this is    |
|                       |                       | the southern-most     |
|                       |                       | latitude. The units   |
|                       |                       | are degrees, and      |
|                       |                       | valid values are      |
|                       |                       | between -90.0 and     |
|                       |                       | 90.0. Observations on |
|                       |                       | the boundaries are    |
|                       |                       | included in the       |
|                       |                       | output.               |
+-----------------------+-----------------------+-----------------------+
| max\_lat              | real(r8)              | The output            |
|                       |                       | observations can be   |
|                       |                       | constrained to only   |
|                       |                       | those which lie       |
|                       |                       | between two           |
|                       |                       | longitudes and two    |
|                       |                       | latitudes. If         |
|                       |                       | specified, this is    |
|                       |                       | the northern-most     |
|                       |                       | latitude. The units   |
|                       |                       | are degrees, and      |
|                       |                       | valid values are      |
|                       |                       | between -90.0 and     |
|                       |                       | 90.0. Observations on |
|                       |                       | the boundaries are    |
|                       |                       | included in the       |
|                       |                       | output.               |
+-----------------------+-----------------------+-----------------------+
| ObsBase               | character(len=128)    | A directory name      |
|                       |                       | which is prepended to |
|                       |                       | the input filenames   |
|                       |                       | only. For files in    |
|                       |                       | the current           |
|                       |                       | directory, specify    |
|                       |                       | '.' (dot).            |
+-----------------------+-----------------------+-----------------------+
| InfilePrefix          | character(len=64)     | The input filenames   |
|                       |                       | are constructed by    |
|                       |                       | prepending this       |
|                       |                       | string before the     |
|                       |                       | string 'YYYYMMDD'     |
|                       |                       | (year, month, day)    |
|                       |                       | and then the suffix   |
|                       |                       | is appended. This     |
|                       |                       | string can be ' '     |
|                       |                       | (empty).              |
+-----------------------+-----------------------+-----------------------+
| InfileSuffix          | character(len=64)     | The input filenames   |
|                       |                       | are constructed by    |
|                       |                       | appending this string |
|                       |                       | to the filename. This |
|                       |                       | string can be ' '     |
|                       |                       | (empty).              |
+-----------------------+-----------------------+-----------------------+
| OutfilePrefix         | character(len=64)     | The output files are  |
|                       |                       | always created in the |
|                       |                       | current directory,    |
|                       |                       | and the filenames are |
|                       |                       | constructed by        |
|                       |                       | prepending this       |
|                       |                       | string before the     |
|                       |                       | string 'YYYYMMDD'     |
|                       |                       | (year, month day) and |
|                       |                       | then the suffix is    |
|                       |                       | appended. This string |
|                       |                       | can be ' ' (empty).   |
+-----------------------+-----------------------+-----------------------+
| OutfileSuffix         | character(len=64)     | The output filenames  |
|                       |                       | are constructed by    |
|                       |                       | appending this string |
|                       |                       | to the filename. This |
|                       |                       | string can be ' '     |
|                       |                       | (empty).              |
+-----------------------+-----------------------+-----------------------+
| observation\_name     | character(len=31)     | The specific          |
|                       |                       | observation type to   |
|                       |                       | use when creating the |
|                       |                       | output observation    |
|                       |                       | sequence file. The    |
|                       |                       | possible values are:  |
|                       |                       | -   "AQUA\_TOTAL\_PRE |
|                       |                       | CIPITABLE\_WATER"     |
|                       |                       | -   "TERRA\_TOTAL\_PR |
|                       |                       | ECIPITABLE\_WATER"    |
|                       |                       | -   "AMSR\_TOTAL\_PRE |
|                       |                       | CIPITABLE\_WATER"     |
|                       |                       | -   "MODIS\_TOTAL\_PR |
|                       |                       | ECIPITABLE\_WATER"    |
|                       |                       |                       |
|                       |                       | These must match the  |
|                       |                       | parameters defined in |
|                       |                       | the                   |
|                       |                       | 'obs\_def\_tpw\_mod.f |
|                       |                       | 90'                   |
|                       |                       | file in the           |
|                       |                       | DART/obs\_def         |
|                       |                       | directory. There is a |
|                       |                       | maximum limit of 31   |
|                       |                       | characters in these   |
|                       |                       | names.                |
+-----------------------+-----------------------+-----------------------+

\
[]{#KnownBugs}

------------------------------------------------------------------------

KNOWN BUGS
----------

The input files are daily; be cautious of time bin boundaries at the
start and end of the day.

[]{#FuturePlans}

------------------------------------------------------------------------

FUTURE PLANS
------------

This program should use the HDF-EOS libraries to read the native MODIS
granule files.

This program could loop over arbitrary numbers of days by using the time
manager calendar functions to increment the bins across month and year
boundaries; it could also use the schedule module to define the bins.

[]{#Legalese}

------------------------------------------------------------------------

Terms of Use
------------

DART software - Copyright UCAR. This open source software is provided by
UCAR, "as is", without charge, subject to all terms of use at
<http://www.image.ucar.edu/DAReS/DART/DART_download>

  ----------------- -----------------------------
  Contact:          nancy collins
  Revision:         \$Revision\$
  Source:           \$URL\$
  Change Date:      \$Date\$
  Change history:   try "svn log" or "svn diff"
  ----------------- -----------------------------


