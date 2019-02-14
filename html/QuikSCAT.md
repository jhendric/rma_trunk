[]{#TOP}

QuikSCAT SeaWinds Data
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
[MODULES](#Modules) / [NAMELIST](#Namelist) / [ERRORS](#Errors) /
[FUTURE PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
--------

NASA's QuikSCAT mission is described in
[http://winds.jpl.nasa.gov/missions/quikscat/](http://winds.jpl.nasa.gov/missions/quikscat/index.cfm).
"QuikSCAT" refers to the satellite, "SeaWinds" refers to the instrument
that provides near-surface wind speeds and directions over large bodies
of water. QuikSCAT has an orbit of about 100 minutes, and the SeaWinds
microwave radar covers a swath under the satellite. The swath is
comprised of successive scans (or rows) and each scan has many
wind-vector-cells (WVCs). For the purpose of this document, we will
focus only the **Level 2B** product at 25km resolution. If you go to the
official JPL data distribution site
<http://podaac.jpl.nasa.gov/DATA_CATALOG/quikscatinfo.html> , we are
using the product labelled **L2B OWV 25km Swath**. Each orbit consists
of (potentially) 76 WVCs in each of 1624 rows or scans. The azimuthal
diversity of the radar returns affects the error characteristics of the
retrieved wind speeds and directions, as does rain, interference of land
in the radar footprint, and very low wind speeds. Hence, not all wind
retrievals are created equal.

The algorithm that converts the 'sigma naughts' (the measure of radar
backscatter) into wind speeds and directions has multiple solutions.
Each candidate solution is called an 'ambiguity', and there are several
ways of choosing 'the best' ambiguity. Beauty is in the eye of the
beholder. At present, the routine to convert the original L2B data files
(one per orbit) in HDF format into the DART observation sequence file
makes several assumptions:

1.  All retrievals are labelled with a 10m height, in accordance with
    the retrieval algorithm.
2.  Only the highest-ranked (by the MLE method) solution is desired.
3.  Only the WVCs with a wvc\_quality\_flag of **zero** are desired.
4.  The mission specification of a wind speed rms error of 2 ms (for
    winds less than 20 m/s) and 10% for windspeeds between 20 and 30 m/s
    can be extended to all winds with a qc flag of zero.
5.  The mission specification of an error in direction of 20 degrees rms
    is applicable to all retrieved directions.
6.  All retrievals with wind speeds less than 1.0 are not used.
7.  The above error characterstics can be simplified when deriving the
    horizontal wind components (i.e. U,V). **Note :** this may or may
    not be a good assumption, and efforts to assimilate the speed and
    direction directly are under way.

[]{#DataSources}

------------------------------------------------------------------------

DATA SOURCES
------------

The NASA Jet Propulsion Laboratory (JPL) [data
repository](http://winds.jpl.nasa.gov/imagesAnim/quikscat.cfm) has a
collection of animations and data sets from this instrument. In keeping
with NASA tradition, these data are in HDF format (specifically, HDF4),
so if you want to read these files directly, you will need to install
the HDF4 libraries (which can be downloaded from
<http://www.hdfgroup.org/products/hdf4/>)

If you go to the official JPL data distribution site
<http://podaac.jpl.nasa.gov/DATA_CATALOG/quikscatinfo.html>, we are
using the product labelled **L2B OWV 25km Swath**. They are organized in
folders by day ... with each orbit (each revolution) in one compressed
file. There are 14 revolutions per day. The conversion to DART
observation sequence format is done on each revolution, multiple
revolutions may be combined 'after the fact' by any
*obs\_sequence\_tool* in the *work* directory of any model.

[]{#Programs}

------------------------------------------------------------------------

PROGRAMS
--------

There are several programs that are distributed from the JPL www-site,
<ftp://podaac.jpl.nasa.gov/pub/ocean_wind/quikscat/L2B/sw/>; we
specifically started from the Fortran file
[read\_qscat2b.f](ftp://podaac.jpl.nasa.gov/pub/ocean_wind/quikscat/L2B/sw/FORTRAN/read_qscat2b.f)
and modified it to be called as a subroutine to make it more similar to
the rest of the DART framework. The original *Makefile* and
*read\_qscat2b.f* are included in the DART distribution in the
*DART/observations/quikscat* directory. You will have to modify the
*Makefile* to build the executable.

### convert\_L2b.f90

*convert\_L2b* is the executable that reads the HDF files distributed by
JPL. *DART/observations/quikscat/work* has the expected
*mkmf\_convert\_L2b* and *path\_names\_convert\_L2b* files and compiles
the executable in the typical DART fashion - with one exception. The
location of the HDF (and possible dependencies) installation must be
conveyed to the *mkmf* build mechanism. Since this information is not
required by the rest of DART, it made sense (to me) to isolate it in the
*mkmf\_convert\_L2b* script. **It will be necessary to modify the
*mkmf\_convert\_L2b* script to be able to build *convert\_L2b***. In
particular, you will have to change the two lines specifying the
location of the HDF (and probably the JPG) libraries. The rest of the
script should require little, if any, modification.

<div class="routine">

set JPGDIR = */contrib/jpeg-6b\_gnu-4.1.2-64*\
set HDFDIR = */contrib/hdf-4.2r4\_gnu-4.1.2-64*\

</div>

There are a lot of observations in every QuikSCAT orbit. Consequently,
the observation sequence files are pretty large - particularly if you
use the ASCII format. Using the binary format (i.e.
*obs\_sequence\_nml:write\_binary\_obs\_sequence = .true.*) will result
in observation sequence files that are about *half* the size of the
ASCII format.

Since there are about 14 QuikSCAT orbits per day, it may be useful to
convert individual orbits to an observation sequence file and then
concatenate multiple observation sequence files into one file per day.
This may be trivially accomplished with the *obs\_sequence\_tool*
program in any *model/xxxx/work* directory. Be sure to include the
*'../../../obs\_def/obs\_def\_QuikSCAT\_mod.f90'* string in
*input.nml&preprocess\_nml:input\_files* when you run *preprocess*.

### obs\_to\_table.f90, plot\_wind\_vectors.m

*DART/diagnostics/threed\_sphere/obs\_to\_table.f90* is a potentially
useful tool. You can run the observation sequence files through this
filter to come up with a 'XYZ'-like file that can be readily plotted
with *DART/diagnostics/matlab/plot\_wind\_vectors.m*.

[]{#Namelist}

------------------------------------------------------------------------

NAMELIST
--------

This namelist is read from the file *input.nml*. We adhere to the F90
standard of starting a namelist with an ampersand '&' and terminating
with a slash '/' for all our namelist input. Character strings that
contain a '/' must be enclosed in quotes to prevent them from
prematurely terminating the namelist. The following values are the
defaults for these namelist items.

<div class="namelist">

    &convert_L2b_nml
       l2b_file = '',
       datadir = '.',
       outputdir = '.',
       lon1 = 0.0, 
       lon2 = 360.0, 
       lat1 = -90.0, 
       lat2 = 90.0,
       along_track_thin = 0,
       cross_track_thin = 0
     /

</div>

\

<div>

It is possible to restrict the output observation sequence to contain
data from a region of interest throught the use of the namelist
parameters. If you need a region that spans the Prime Meridian lon1 can
be a larger number than lon2, for example, a region from 300 E to 40 E
and 60 S to 30 S (some of the South Atlantic), would be *lon1 = 300,
lon2 = 40, lat1 = -60, lat2 = -30*.

  Contents             Type                 Description
  -------------------- -------------------- ----------------------------------------------------------------------------------------------------------------------------------
  l2b\_file            character(len=128)   name of the HDF file to read - NOT including the directory, e.g. QS\_S2B44444.20080021548
  datadir              character(len=128)   the directory containing the HDF files
  outputdir            character(len=128)   the directory for the output observation sequence files.
  lon1                 real(r4)             the West-most longitude of interest in degrees. \[0.0, 360\]
  lon2                 real(r4)             the East-most longitude of interest in degrees. \[0.0, 360\]
  lat1                 real(r4)             the South-most latitude of interest in degrees. \[-90.0, 90.0\]
  lat2                 real(r8)             the North-most latitude of interest in degrees. \[-90.0, 90.0\]
  along\_track\_thin   integer              provides ability to thin the data by keeping only every Nth row. e.g. 3 == keep every 3rd row.
  cross\_track\_thin   integer              provides ability to thin the data by keeping only every Nth wind vector cell in a particular row. e.g. 5 == keep every 5th cell.

</div>

\
[]{#KnownBugs}

------------------------------------------------------------------------

KNOWN BUGS
----------

There are no known bugs at this time.

[]{#FuturePlans}

------------------------------------------------------------------------

FUTURE PLANS
------------

1.  There is one bit of error-checking that did not survive the
    conversion from F77 to F90. I need to restore the check that the HDF
    file being read is a 'Level 2B' product.
2.  There is a lot of error-checking that is not being done. I need to
    bulletproof the code more.
3.  We need namelist options to select something other than the
    highest-ranked ambiguity.
4.  We need namelist options to select more QC flags - not just the ones
    with the 'perfect' QC value of 0
5.  Add an option to leave the observations as speed and direction
    instead of converting them to U,V components. This is a natural
    implementation of the instrument error characteristics. However, it
    would require writing a specialized forward operator in order to
    assimilate obs of this type (speed, direction), and there is still a
    numerical problem with trying to do the statistics required during
    the assimilation of a cyclic direction value.

[]{#Legalese}

------------------------------------------------------------------------

Terms of Use
------------

DART software - Copyright UCAR. This open source software is provided by
UCAR, "as is", without charge, subject to all terms of use at
<http://www.image.ucar.edu/DAReS/DART/DART_download>

  ------------------ -----------------------------
  Contact:           Tim Hoar
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


