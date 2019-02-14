[]{#TOP}

SSUSI F16 EDR-DSK format to observation sequence converters
===========================================================

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

The Special Sensor Ultraviolet Spectrographic Imager
[SSUSI](http://http://ssusi.jhuapl.edu/) is designed to remotely sense
the ionosphere and thermosphere. The following is repeated from the
SSUSI home page:

> ***Overview**\
> Beginning in 2003, the Defense Meteorological Satellite Program (DMSP)
> satellites began carrying the SSUSI instrument - a combination of
> spectrographic imaging and photometric systems designed to remotely
> sense the ionosphere and thermosphere.\
> \
> The long term focus of the SSUSI program is to provide data concerning
> the upper atmospheric response to the sun over the changing conditions
> of the solar cycle. Data collected by SSUSI instrument can help
> identify structure in the equatorial and polar regions.\
> \
> **Mission**\
> SSUSI was designed for the DMSP Block 5D-3 satellites. These
> satellites are placed into nearly polar, sun-synchronous orbits at an
> altitude of about 850 km. SSUSI is a remote-sensing instrument which
> measures ultraviolet (UV) emissions in five different wavelength bands
> from the Earth's upper atmosphere. SSUSI is mounted on a nadir-looking
> panel of the satellite. The multicolor images from SSUSI cover the
> visible Earth disk from horizon to horizon and the anti-sunward limb
> up to an altitude of approximately 520 km.\
> \
> The UV images and the derived environmental data provide the Air Force
> Weather Agency (Offutt Air Force Base, Bellevue, NE) with near
> real-time information that can be utilized in a number of
> applications, such as maintenance of high frequency (HF) communication
> links and related systems and assessment of the environmental hazard
> to astronauts on the Space Station.*

*convert\_f16\_edr\_dsk.f90* will extract the ON2 observations from the
F16 "edr-dsk" format files and create DART observation sequence files.
There is one additional preprocessing step before the edr-dsk files may
be converted.\
\
The ON2\_UNCERTAINTY variable in the netcdf files have IEEE NaN values,
but none of the required metadata to interpret them correctly. These 2
lines will add the required attributes so that NaNs are replaced with a
fill value that can be queried and manipulated. Since the
ON2\_UNCERTAINTY is a standard deviation, it is sufficient to make the
fill value negative. [See the section on Known Bugs]{}

<div class="unix">

    ncatted -a _FillValue,ON2_UNCERTAINTY,o,f,NaN        input_file.nc
    ncatted -a _FillValue,ON2_UNCERTAINTY,m,f,-1.0       input_file.nc

</div>

[]{#DataSources}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

DATA SOURCES
------------

<http://ssusi.jhuapl.edu/data_products>

Please read their [data usage](http://ssusi.jhuapl.edu/home_data_usage)
policy.

[]{#Programs}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PROGRAMS
--------

*DART/observations/SSUSI/convert\_f16\_edr\_dsk.f90* will extract ON2
data from the distribution files and create DART observation sequence
(obs\_seq) files. Build it in the *SSUSI/work* directory by running the
*./quickbuild.csh* script located there. In addition to the converters,
the *advance\_time* and *obs\_sequence\_tool* utilities will be built.

An example data file is in the *data* directory. An example scripts for
adding the required metadata to the ON2\_UNCERTAINTY variable in the
*shell\_scripts* directory. These are *NOT* intended to be turnkey
scripts; they will certainly need to be customized for your use. There
are comments at the top of the scripts saying what options they include,
and should be commented enough to indicate where changes will be likely
to need to be made.

[]{#Errors}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

ERRORS
------

The code for setting observation error variances is using fixed values,
and we are not certain if they are correct. Incoming QC values larger
than 0 are suspect, but it is not clear if they really signal unusable
values or whether there are some codes we should accept.

[]{#KnownBugs}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

KNOWN BUGS
----------

The netCDF files - as distributed - have NaN values to indicate
"MISSING". This makes it exceptionally hard to read or work with, as
almost everything will core dump when trying to perform any math with
NaNs. *convert\_f16\_edr\_dsk.f90* tries to count how many values are
missing. If the NaN has not been replaced with a numerically valid
MISSING value, the following FATAL ERROR is generated (by the Intel
compiler, with debug and traceback enabled):

     set_nml_output Echo NML values to log file only
     Trying to open namelist log dart_log.nml
    forrtl: error (65): floating invalid
    Image              PC                Routine            Line        Source             
    convert_f16_edr_d  000000000051717D  MAIN__                    143  convert_f16_edr_dsk.f90
    convert_f16_edr_d  0000000000409B3C  Unknown               Unknown  Unknown
    libc.so.6          0000003101E1ED5D  Unknown               Unknown  Unknown
    convert_f16_edr_d  0000000000409A39  Unknown               Unknown  Unknown
    Abort (core dumped)

The solution is to replace the NaN values with a viable MISSING value
using the *shell\_scripts/netcdf\_manip.csh* script. It relies on the
netCDF Operators, freely available from  <http://nco.sourceforge.net>

[]{#FuturePlans}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FUTURE PLANS
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
  Contact:           Tim Hoar
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


