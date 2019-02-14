[]{#TOP}

PROGRAM *sst\_to\_obs, oi\_sst\_to\_obs*
========================================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [DECISIONS](#Decisions) / [ERRORS](#Errors) /
[PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
========

There are two gridded SST observation converters in this directory, one
for data from PODAAC, and one from NOAA/NCDC. *sst\_to\_obs* converts
data from PODAAC and has been used by Romain Escudier for regional
studies with ROMS. *oi\_sst\_to\_obs* converts data from NOAA/NCDC and
has been used by Fred Castruccio for global studies with POP.

#### sst\_to\_obs -- GHRSST to DART Observation Sequence Converter

These routines are designed to convert the [GHRSST Level 4 AVHRR\_OI
Global Blended Sea Surface Temperature Analysis (GDS version 2) from
NCEI
data](https://podaac.jpl.nasa.gov/dataset/AVHRR_OI-NCEI-L4-GLOB-v2.0)
distributed by the [Physical Oceanography Distributed Active Archive
Center](http://podaac.jpl.nasa.gov). Please remember to cite the data in
your publications, [specific instructions from PODAAC are available
here.](https://podaac.jpl.nasa.gov/dataset/AVHRR_OI-NCEI-L4-GLOB-v2.0)
This is an example:

> National Centers for Environmental Information. 2016. GHRSST Level 4
> AVHRR\_OI Global Blended Sea Surface Temperature Analysis (GDS version
> 2) from NCEI. Ver. 2.0. PO.DAAC, CA, USA. Dataset accessed
> \[YYYY-MM-DD\] at http://dx.doi.org/10.5067/GHAAO-4BC02.

**Many thanks to Romain Escudier (then at Rutgers) who did the bulk of
the work and graciously contributed his efforts to the DART project.**
Romain gave us scripts and source code to download the data from the
PODAAC site, subset the global files to a region of interest, and
convert that subsetted file to a DART observation sequence file. Those
scripts and programs have been only lightly modified to work with the
Manhattan version of DART and contain a bit more documentation.

The workflow is usually:

1.  compile the converters by running *work/quickbuild.csh* in the usual
    way.
2.  customize the *shell\_scripts/parameters\_SST* resource file to
    specify variables used by the rest of the scripting.
3.  run *shell\_scripts/get\_sst\_ftp.sh* to download the data from
    PODAAC.
4.  provide a mask for the desired study area.
5.  run *shell\_scripts/Prepare\_SST.sh* to subset the PODAAC data and
    create the DART observation sequence files. Be aware that the
    *Prepare\_SST.sh* modifies the *shell\_scripts/input.nml.template*
    file and generates its own *input.nml*. *work/input.nml* is not
    used.
6.  combine all output files for the region and timeframe of interest
    into one file using the
    [obs\_sequence\_tool](../../../assimilation_code/programs/obs_sequence_tool/obs_sequence_tool.html%20)

#### Example:

It is worth describing a small example. If you configure
*get\_sst\_ftp.sh* to download the last two days of 2010 and then
specify the mask to subset for the NorthWestAtlantic (NWA) and run
*Prepare\_SST.sh* your directory structure should look like the
following:

    0[1234] cheyenne6:/<6>obs_converters/SST
    .
    |-- ObsData
    |   `-- SST
    |       |-- ncfile
    |       |   `-- 2010
    |       |       |-- 20101230120000-NCEI-L4_GHRSST-SSTblend-AVHRR_OI-GLOB-v02.0-fv02.0.nc
    |       |       `-- 20101231120000-NCEI-L4_GHRSST-SSTblend-AVHRR_OI-GLOB-v02.0-fv02.0.nc
    |       `-- nwaSST
    |           `-- 2010
    |               |-- 20101230120000-NCEI-L4_GHRSST-SSTblend-AVHRR_OI-GLOB-v02.0-fv02.0_NWA.nc
    |               `-- 20101231120000-NCEI-L4_GHRSST-SSTblend-AVHRR_OI-GLOB-v02.0-fv02.0_NWA.nc
    |-- oi_sst_to_obs.f90
    |-- oi_sst_to_obs.nml
    |-- sst_to_obs.f90
    |-- sst_to_obs.nml
    |-- shell_scripts
    |   |-- Prepare_SST.sh
    |   |-- functions.sh
    |   |-- get_sst_ftp.sh
    |   |-- input.nml
    |   |-- input.nml.template
    |   |-- my_log.txt
    |   |-- parameters_SST
    |   `-- prepare_SST_file_NWA.sh
    |-- masks
    |   |-- Mask_NWA-NCDC-L4LRblend-GLOB-v01-fv02_0-AVHRR_OI.nc
    |   `-- Mask_NWA120000-NCEI-L4_GHRSST-SSTblend-AVHRR_OI-GLOB-v02.0-fv02.0.nc
    `-- work
        |-- Makefile
        |-- advance_time
        |-- input.nml
        |-- mkmf_advance_time
        |-- mkmf_obs_sequence_tool
        |-- mkmf_oi_sst_to_obs
        |-- mkmf_preprocess
        |-- mkmf_sst_to_obs
        |-- obs_sequence_tool
        |-- oi_sst_to_obs
        |-- path_names_advance_time
        |-- path_names_obs_sequence_tool
        |-- path_names_oi_sst_to_obs
        |-- path_names_preprocess
        |-- path_names_sst_to_obs
        |-- preprocess
        |-- quickbuild.csh
        `-- sst_to_obs

The location of the DART observation sequence files is specified by
*parameter\_SST*:*DIR\_OUT\_DART*. That directory should contain the
following two files:

    0[1236] cheyenne6:/<6>v2/Err30 > ls -l
    'total 7104
    -rw-r--r-- 1 thoar p86850054 3626065 Jan 10 11:08 obs_seq.sst.20101230
    -rw-r--r-- 1 thoar p86850054 3626065 Jan 10 11:08 obs_seq.sst.20101231

#### oi\_sst\_to\_obs -- NOAA/NCDC to DART Observation Sequence Converter

*oi\_sst\_to\_obs* is designed to convert the [NOAA High-resolution
Blended Analysis: Daily Values using AVHRR
only](https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html)
data. The global metadata of a typical file is shown here:

    :Conventions = "CF-1.5" ;
    :title = "NOAA High-resolution Blended Analysis: Daily Values using AVHRR only" ;
    :institution = "NOAA/NCDC" ;
    :source = "NOAA/NCDC  ftp://eclipse.ncdc.noaa.gov/pub/OI-daily-v2/" ;
    :comment = "Reynolds, et al., 2007:
         Daily High-Resolution-Blended Analyses for Sea Surface Temperature.
         J. Climate, 20, 5473-5496.
         Climatology is based on 1971-2000 OI.v2 SST, 
         Satellite data: Navy NOAA17 NOAA18 AVHRR, Ice data: NCEP ice." ;
    :history = "Thu Aug 24 13:46:51 2017: ncatted -O -a References,global,d,, sst.day.mean.2004.v2.nc\n",
        "Version 1.0" ;
    :references = "https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html" ;
    :dataset_title = "NOAA Daily Optimum Interpolation Sea Surface Temperature" ;

The workflow is usually:

1.  compile the converters by running *work/quickbuild.csh* in the usual
    way.
2.  [download the desired
    data.](https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html)
3.  customize the *work/input.nml* file.
4.  run *work/oi\_sst\_to\_obs* to create a single DART observation
    sequence file.
5.  combine all output files for the region and timeframe of interest
    into one file using the
    [obs\_sequence\_tool](../../../assimilation_code/programs/obs_sequence_tool/obs_sequence_tool.html%20)

[]{#Namelist}

------------------------------------------------------------------------

sst\_to\_obs NAMELIST
---------------------

This namelist is read from the file *input.nml*. Namelists start with an
ampersand '&' and terminate with a slash '/'. Character strings that
contain a '/' must be enclosed in quotes to prevent them from
prematurely terminating the namelist.

<div class="namelist">

    &sst_to_obs_nml
       sst_netcdf_file     = '1234567.nc'
       sst_netcdf_filelist = 'sst_to_obs_filelist'
       sst_out_file        = 'obs_seq.sst'
       subsample_intv      = 1
       sst_rep_error       = 0.3
       debug               = .false.
       /

</div>

<div>

Contents
Type
Description
sst\_netcdf\_file
character(len=256)
Name of the (usually subsetted) netcdf data file. This may be a relative
or absolute filename. If you run the scripts 'as is', this will be
something like:
*../ObsData/SST/nwaSST/2010/20101231120000-NCEI-L4\_GHRSST-SSTblend-AVHRR\_OI-GLOB-v02.0-fv02.0\_NWA.nc*
sst\_netcdf\_filelist
character(len=256)
Name of the file that contains a list of (usually subsetted) data files,
one per line. **You may not specify both sst\_netcdf\_file AND
sst\_netcdf\_filelist.** One of them must be empty.
sst\_out\_file
character(len=256)
Name of the output observation sequence file.
subsample\_intv
integer
It is possible to 'thin' the observations. *subsample\_intv* allows one
to take every Nth observation.
sst\_rep\_error
real
In DART the observation error variance can be thought of as having two
components, an instrument error and a representativeness error. In
*sst\_to\_obs* the instrument error is specified in the netCDF file by
the variable *analysis\_error*. The representativeness error is
specified by *sst\_rep\_error*, which is specified as a standard
deviation. These two values are added together and squared and used as
the observation error variance. **Note:** This algorithm maintains
backwards compatibility, but is technically not the right way to combine
these two quantities. If they both specified variance, adding them
together and then taking the square root would correctly specify a
standard deviation. Variances add, standard deviations do not. Since the
true observation error variance (in general) is not known, we are
content to live with an algorithm that produces useful observation error
variances. If your research comes to a more definitive conclusion,
please let us know.
debug
logical
Print extra information during the *sst\_to\_obs* execution.

</div>

oi\_sst\_to\_obs NAMELIST
-------------------------

This namelist is read from the file *input.nml*. Namelists start with an
ampersand '&' and terminate with a slash '/'. Character strings that
contain a '/' must be enclosed in quotes to prevent them from
prematurely terminating the namelist.

<div class="namelist">

    &oi_sst_to_obs_nml
       input_file       = '1234567.nc'
       output_file_base = 'obs_seq.sst'
       subsample_intv   = 1
       sst_error_std    = 0.3
       debug            = .false.
       /

</div>

<div>

Contents
Type
Description
input\_file
character(len=256)
Name of the input netcdf data file. This may be a relative or absolute
filename. If you run the scripts 'as is', this will be something like:
*../ObsData/SST/nwaSST/2010/20101231120000-NCEI-L4\_GHRSST-SSTblend-AVHRR\_OI-GLOB-v02.0-fv02.0\_NWA.nc*
output\_file\_base
character(len=256)
Partial filename for the output file. The date and time are appended to
*output\_file\_base* to construct a unique filename reflecting the time
of the observations in the file.
subsample\_intv
integer
It is possible to 'thin' the observations. *subsample\_intv* allows one
to take every Nth observation.
sst\_error\_std
real
This is the total observation error standard deviation.
debug
logical
Print extra information during the *oi\_sst\_to\_obs* execution.

</div>

[]{#Decisions}

------------------------------------------------------------------------

DECISIONS YOU MIGHT NEED TO MAKE
--------------------------------

See the general discussion in the [observations
introduction](../observations.html#Decisions) page about what options
are available for the things you need to specify. These include setting
a time, specifying an expected error, setting a location, and an
observation type.

[]{#KnownBugs}

------------------------------------------------------------------------

KNOWN BUGS
----------

I do not believe *sst\_to\_obs* will work correctly if given multiple
files in *sst\_netcdf\_filelist*. The number of observation used to
declare the length of the output observation sequence is based on a
single file ... yet seems to be used by many. I have not tested this
configuration, since the scripting does not use the
*sst\_netcdf\_filelist* mechanism.

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


