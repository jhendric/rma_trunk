[]{#TOP}

PROGRAM *MIDAS\_to\_obs*
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
[NAMELIST](#Namelist) / [REFERENCES](#References) / [ERRORS](#Errors) /
[PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
========

#### MIDAS netCDF File to DART observation converter

Alex Chartier (University of Bath, UK) is the point-of-contact for this
effort.

> "MIDAS runs in Matlab. The raw observations come from GPS receivers as
> RINEX files, but we can't use them directly just yet ... Currently,
> the 'slant' (satellite-to-receiver path) observations are inverted by
> MIDAS to make vertical, column-integrated 'observations' of plasma
> density."

[]{#DataSources}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

DATA SOURCES
------------

Contact Alex for MIDAS observations.\
\
Alex writes out netCDF files that may be converted to DART observation
sequence files. The netCDF files have a pretty simple format.

    netcdf Test {
    dimensions:
            latitude = 5 ;
            longitude = 6 ;
            height = 30 ;
            time = UNLIMITED ; // (1 currently)
    variables:
            double latitude(latitude) ;
                    latitude:units = "degrees_north" ;
                    latitude:long_name = "latitude" ;
                    latitude:standard_name = "latitude" ;
            double longitude(longitude) ;
                    longitude:units = "degrees_east" ;
                    longitude:long_name = "longitude" ;
                    longitude:standard_name = "longitude" ;
            double height(height) ;
                    height:units = "metres" ;
                    height:long_name = "height" ;
                    height:standard_name = "height" ;
            double time(time) ;
                    time:units = "Days since 1601-01-01" ;
                    time:long_name = "Time (UT)" ;
                    time:standard_name = "Time" ;
            double Ne(height, latitude, longitude) ;
                    Ne:grid_mapping = "standard" ;
                    Ne:units = "1E11 e/m^3" ;
                    Ne:long_name = "electron density" ;
                    Ne:coordinates = "latitude longitude" ;
            double TEC(time, latitude, longitude) ;
                    TEC:grid_mapping = "standard" ;
                    TEC:units = "1E16 e/m^2" ;
                    TEC:long_name = "total electron content" ;
                    TEC:coordinates = "latitude longitude" ;
            double Variance(time, latitude, longitude) ;
                    Variance:grid_mapping = "standard" ;
                    Variance:units = "1E16 e/m^2" ;
                    Variance:long_name = "Variance of total electron content" ;
                    Variance:coordinates = "latitude longitude" ;
                    Variance:standard_name = "TEC variance" ;
    // global attributes:
                    :Conventions = "CF-1.5" ;
    }

[]{#Programs}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PROGRAMS
--------

The *MIDAS\_to\_obs.f90* file is the source code for the main converter
program.\
\
To compile and test, go into the *MIDAS/work* subdirectory and run the
*quickbuild.csh* script to build the converter and a couple of general
purpose utilities. The
[obs\_sequence\_tool](../../../assimilation_code/programs/obs_sequence_tool/obs_sequence_tool.html)
manipulates (i.e. combines, subsets) DART observation files once they
have been created. The default observations supported are those defined
in
[observations/forward\_operators/obs\_def\_upper\_atm\_mod.f90](../../forward_operators/obs_def_upper_atm_mod.f90).
If you need additional observation types, you will have to add the
appropriate *obs\_def\_XXX\_mod.f90* file to the
*input.nml* *&preprocess\_nml:input\_files* variable and run
*quickbuild.csh* again. It rebuilds the table of supported observation
types before compiling the source code.

 

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

    &MIDAS_to_obs_nml
       input_file    = 'infile.nc'
       obs_out_file  = 'obs_seq.out',
       verbose       = .false.
       /

</div>

  Item             Type                 Description
  ---------------- -------------------- --------------------------------------------------------------------------------------------------------------------------------------------------
  input\_file      character(len=256)   Name of the input netCDF MIDAS file to read.
  obs\_out\_file   character(len=256)   Name of the output observation sequence file that is created.
  verbose          logical              Controls how much informational output is printed during a conversion. *.true.* the most amount of output. *.false.* the least amount of output.

### Example {#example .indent1}

    &MIDAS_to_obs_nml
       input_file    = '../data/Test.nc',
       obs_out_file  = 'obs_seq.out',
       verbose       = .TRUE.,

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

References
----------

[]{#KnownBugs}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

KNOWN BUGS
----------

none

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


