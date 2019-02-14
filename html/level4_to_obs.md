[]{#TOP}

PROGRAM *level4\_to\_obs*
=========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [DATA SOURCES](#DataSources) /
[PROGRAMS](#Programs) / [DECISIONS](#Decisions) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

Overview
========

#### AmeriFlux Level 4 data to DART Observation Sequence Converter

This routine is designed to convert the flux tower Level 4 data from the
[AmeriFlux](http://ameriflux.lbl.gov) network of observations from
micrometeorological tower sites. AmeriFlux is part of
[FLUXNET](http://fluxnet.ornl.gov) and the converter is hoped to be a
suitable starting point for the conversion of observations from FLUXNET.
As of May 2012, I have not yet tried to work with any other observations
from FLUXNET.\
\
The AmeriFlux Level 4 products are recorded using the local time. DART
observation sequence files use GMT. For more information about AmeriFlux
data products, go to <http://ameriflux.lbl.gov>.

#### Notice

There was a pretty severe bug in the converter that swapped latent heat
flux and sensible heat flux. The bug was present through revision 7200.
It has been corrected in all subsequent versions.

The workflow is usually:

1.  download the Level 4 data for the towers and years in question ([see
    DATA SOURCES below](#DataSources))
2.  record the TIME ZONE, latitude, longitude, and elevation for each
    tower
3.  build the DART executables with support for the tower observations.
    This is done by running *preprocess* with *obs\_def\_tower\_mod.f90*
    in the list of *input\_files* for *preprocess\_nml*.
4.  provide basic tower information via the *level4\_to\_obs\_nml*
    namelist since this information is not contained in the Level 4 data
    file
5.  convert each Level 4 data file individually using *level4\_to\_obs*
6.  combine all output files for the region and timeframe of interest
    into one file using
    [obs\_sequence\_tool](../../../assimilation_code/programs/obs_sequence_tool/obs_sequence_tool.html%20)

For some models (CLM, for example), it is required to reorganize the
observation sequence files into a series of files that contains ONLY the
observations for each assimilation. This can be achieved with the
[makedaily.sh](makedaily.sh) script.

[]{#Namelist}

------------------------------------------------------------------------

NAMELIST
--------

This namelist is read from the file *input.nml*. Namelists start with an
ampersand '&' and terminate with a slash '/'. Character strings that
contain a '/' must be enclosed in quotes to prevent them from
prematurely terminating the namelist.

<div class="namelist">

    &level4_to_obs_nml
       text_input_file = 'textdata.input',
       obs_out_file    = 'obs_seq.out',
       year            = -1,
       timezoneoffset  = -1,
       latitude        = -1.0,
       longitude       = -1.0,
       elevation       = -1.0,
       flux_height     = -1.0,
       maxgoodqc       = 3,
       verbose         = .false.
       /

</div>

<div>

Contents
Type
Description
text\_input\_file
character(len=128)
Name of the Level 4 ASCII file of comma-separated values. This may be a
relative or absolute filename.
obs\_out\_file
character(len=128)
Name of the output observation sequence file.
year
integer
The year of the observations in the Level 4 text file.
timezoneoffset
real
the time zone offset (in hours) of the station. The tower observation
times are local time, we need to convert them to GMT.
latitude
real
Latitude (in degrees N) of the tower.
longitude
real
Longitude (in degrees E) of the tower. For internal consistency, DART
uses longitudes in the range \[0,360\]. An input value of -90 will be
converted to 270, for example.
elevation
real
surface elevation (in meters) of the tower.
flux\_height
real
height (in meters) of the flux instrument on the tower.
maxgoodqc
real
maximum value of any observation quality control flag to pass through to
the output observation sequence. Keep in mind that *filter* has the
ability to discriminate on the value, so there is really little to be
gained by rejecting them during the conversion.
verbose
logical
Print extra information during the *level4\_to\_obs* execution.

</div>

[]{#DataSources}

------------------------------------------------------------------------

DATA SOURCES
------------

The data was acquired from
<http://cdiac.ornl.gov/ftp/ameriflux/data/Level4/Sites_ByName>\
and have names like *USBar2004\_L4\_h.txt, USHa12004\_L4\_h.txt,
USNR12004\_L4\_h.txt, USSP32004\_L4\_h.txt, USSRM2004\_L4\_h.txt,
USWCr2004\_L4\_h.txt, USWrc2004\_L4\_h.txt, ...*\
\
The Level 4 products in question are ASCII files of comma-separated
values taken every 30 minutes for an entire year. The first line is a
comma-separated list of column descriptors, all subsequent lines are
comma-separated numerical values. The converter presently searches for
the columns pertaining to *NEE\_or\_fMDS*, *H\_f*, *LE\_f*, their
corresponding quality control fields, and those columns pertaining to
the time of the observation. These values are mapped as follows:

Level 4 units

Level 4 variable

description

DART type

DART kind

DART units

------------------------------------------------------------------------

W/m\^2

LE\_f

Latent Heat Flux

TOWER\_LATENT\_HEAT\_FLUX

QTY\_LATENT\_HEAT\_FLUX

W/m\^2

\[0-3\]

LE\_fqc

QC for LE\_f

N/A

N/A

same

------------------------------------------------------------------------

W/m\^2

H\_f

Sensible Heat Flux

TOWER\_SENSIBLE\_HEAT\_FLUX

QTY\_SENSIBLE\_HEAT\_FLUX

W/m\^2

\[0-3\]

H\_fqc

QC for H\_f

N/A

N/A

same

------------------------------------------------------------------------

umolCO2/m\^2/s

NEE\_or\_fMDS

Net Ecosystem Production

TOWER\_NETC\_ECO\_EXCHANGE

QTY\_NET\_CARBON\_PRODUCTION

gC/m\^2/s

\[0-3\]

NEE\_or\_fMDSqc

QC for NEE\_or\_fMDS

N/A

N/A

same

The *LE\_fqc*, *H\_fqc*, and *NEE\_or\_fMDSqc* variables use the
following convention:

> 0 = original, 1 = category A (most reliable), 2 = category B (medium),
> 3 = category C (least reliable). (Refer to Reichstein et al. 2005
> Global Change Biology for more information)

\

I am repeating the AmeriFlux [Data Fair-Use
Policy](http://ameriflux.lbl.gov/Data/Pages/DataUsagePolicy.aspx)
because I believe it is important to be a good scientific citizen:

> "The AmeriFlux data provided on this site are freely available and
> were furnished by individual AmeriFlux scientists who encourage their
> use.\
> \
> Please kindly inform in writing (or e-mail) the appropriate AmeriFlux
> scientist(s) of how you intend to use the data and of any publication
> plans. It is also important to contact the AmeriFlux investigator to
> assure you are downloading the latest revision of the data and to
> prevent potential misuse or misinterpretation of the data.\
> \
> Please acknowledge the data source as a citation or in the
> acknowledgments if no citation is available. If the AmeriFlux
> Principal Investigators (PIs) feel that they should be acknowledged or
> offered participation as authors, they will let you know and we assume
> that an agreement on such matters will be reached before publishing
> and/or use of the data for publication.\
> \
> If your work directly competes with the PI's analysis they may ask
> that they have the opportunity to submit a manuscript before you
> submit one that uses unpublished data. In addition, when publishing
> please acknowledge the agency that supported the research.\
> \
> Lastly, we kindly request that those publishing papers using AmeriFlux
> data provide reprints to the PIs providing the data and to the
> AmeriFlux archive via ameriflux.lbl.gov."

[]{#Programs}

------------------------------------------------------------------------

PROGRAMS
--------

The *level4\_to\_obs.f90* file is the source for the main converter
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
  Contact:           Tim Hoar
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


