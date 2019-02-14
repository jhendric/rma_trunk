[]{#TOP}

PROGRAM *tc\_to\_obs*
=====================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[DATA SOURCES](#DataSources) / [PROGRAMS](#Programs) / [EXPECTED
ERROR](#ObservationalErrors) / [NAMELIST](#Namelist) / [KNOWN
BUGS](#KnownBugs) / [PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Tropical Cyclone ATCF File to DART Converter

Overview

Tropical Cyclone data created by the 'Automated Tropical Cyclone
Forecast (ATCF) System' can be converted into DART observations of the
storm center location, minimum sea level pressure, and maximum wind
speed. Several of the options can be customized at runtime by setting
values in a Fortran namelist. See the [namelist](#Namelist) section
below for more details. In the current release of DART only the [WRF
model](../../../models/wrf/model_mod.html) has forward operator code to
generate expected obs values for these vortex observations.

[This
webpage](http://www.ral.ucar.edu/hurricanes/realtime/index.php#about_atcf_data_files)
documents many things about the ATCF system and the various file formats
that are used for storm track data and other characteristics.

The converter in this directory is only configured to read the packed
"b-deck" format (as described on the webpage referenced above). There
are sections in the fortran code which can be filled in to read other
format variants. This should mostly be a matter of changing the read
format string to match the data in the file.

\

[]{#DataSources}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

DATA SOURCES
------------

A collection of past storm ATCF information can be found
[here](http://www.ral.ucar.edu/hurricanes/repository). For each
observation you will need a location, a data value, a type, a time, and
some kind of error estimate. The error estimates will need to be
hardcoded or computed in the converter since they are not available in
the input data. See below for more details on selecting an appropriate
error value.

\

[]{#Programs}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PROGRAMS
--------

The *tc\_to\_obs.f90* file is the source for the main converter program.
Look at the source code where it reads the example data file. Given the
variety of formatting details in different files, you may quite possibly
need to change the "read" statement to match your data format. There is
a 'select case' section which is intended to let you add more formats
and select them at runtime via namelist.

To compile and test, go into the work subdirectory and run the
*quickbuild.csh* script to build the converter and a couple of general
purpose utilities. *advance\_time* helps with calendar and time
computations, and the *obs\_sequence\_tool* manipulates DART observation
files once they have been created.

This converter creates observation types defined in the
*DART/observations/forward\_operators/obs\_def\_vortex\_mod.f90* file.
This file must be listed in the *input.nml* namelist file, in the
*&preprocess\_nml* namelist, in the 'input\_files' variable, for any
programs which are going to process these observations. If you have to
change the *&preprocess\_nml* namelist you will have to run
*quickbuild.csh* again to build and execute the *preprocess* program
before compiling other executables. It remakes the table of supported
observation types before trying to recompile other source code.

There is an example b-deck data file in the *data* directory. This
format is what is supported in the code as distributed. There are other
variants of this format which have more spaces so the columns line up,
and variants which have many more fields than what is read here.

\

[]{#ObservationalErrors}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

SPECIFYING EXPECTED ERROR
-------------------------

The ATCF files DO NOT include any estimated error values. The source
code currently has hardcoded values for location, sea level pressure,
and max wind errors. These may need to be adjusted as needed if they do
not give the expected results.

\

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

    &tc_to_obs_nml
       input_atcf_file         = 'input.txt'
       fileformat              = 'b-deck'
       obs_out_file            = 'obs_seq.out'
       append_to_existing_file = .false.
       debug                   = .false.
     /

</div>

\
\

<div>

  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  Item                         Type                 Description
  ---------------------------- -------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  input\_atcf\_file            character(len=256)   Name of the input ascii text file in ATCF format.

  fileformat                   character(len=128)   Currently only supports 'b-deck' but if other format strings are added, can switch at runtime between reading different varieties of ATCF file formats.

  obs\_out\_file               character(len=256)   Name of the output observation sequence file to create.

  append\_to\_existing\_file   logical              If .false., this program will overwrite an existing file. If .true. and if a file already exists with the same name the newly converted observations will be appended to that file. Useful if you have multiple small input files that you want to concatenate into a single output file. However, there is no code to check for duplicated observations. If this is .true. and you run the converter twice you will get duplicate observations in the file which is bad. (It will affect the quality of your assimilation results.) Use with care.\
                                                    You can concatenate multiple obs sequence files as a postprocessing step with the [observation sequence tool](../../../assimilation_code/programs/obs_sequence_tool/obs_sequence_tool.html) which comes with DART and is in fact built by the quickbuild.csh script in the TC converter work directory.

  debug                        logical              Set to .true. to print out more details during the conversion process.
  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\

[]{#KnownBugs}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

KNOWN BUGS
----------

none

\

[]{#FuturePlans}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FUTURE PLANS
------------

If users add support for some of the other format variants, the DART
group would be happy to accept code contributions.

\

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
  Contact:           nancy collins
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


