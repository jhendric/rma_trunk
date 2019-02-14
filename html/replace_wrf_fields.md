[]{#TOP}

PROGRAM *replace\_wrf\_fields*
==============================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [MODULES](#Modules) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

Overview
--------

Program to copy various fields from one WRF netCDF file to another.

There are many existing utilities to process netCDF files, i.e. the NCO
operators and NCL scripts, which have more functionality than this
program. The only purpose for having this one is that it is a standalone
program with no prerequisites or dependencies other than the netCDF
libraries. If you already have other tools available they can do the
same functions that this program does.

This program copies the given data fields from the input file to the
output file, failing if their sizes, shapes, or data types do not match
exactly. The expected use is to copy fields which are updated by the WRF
program but are not part of the DART state vector, for example, sea
surface temperature or soil fields. After DART has updated the WRF
restart *wrfinput\_d01* file, this program can be used to update other
fields in the file before running the model.

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

    &replace_wrf_fields_nml
       fieldnames = 'SST',
       fieldlist_file = '',
       fail_on_missing_field = .true.
       debug = .false.,
       /

</div>

\
\

<div>

  Item                       Type                     Description
  -------------------------- ------------------------ --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  fieldnames                 character(len=129) (:)   An array of ASCII field names to be copied from the input netCDF file to the output netCDF file. The names must match exactly, and the size and shape of the data must be the same in the input and output files for the data to be copied. If the field names are set here, the fieldlist\_file item must be ' '.
  fieldlist\_file            character(len=129)       An alternative to an explicit list of field names to copy. This is a single string, the name of a file which contains a single field name, one per line. If this option is set, the fieldnames namelist item must be ' '.
  fail\_on\_missing\_field   logical                  If any fields in the input list are not found in either the input or output netcdf files, fail if this is set to true. If false, a warning message will be printed but execution will continue.
  debug                      logical                  If true, print out debugging messages about which fields are found in the input and output files.

</div>

\
\
[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    types_mod
    utilities_mod
    parse_args_mod

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   input namelist ; *input.nml*
-   Input - output WRF state netCDF files; *wrfinput\_d01,
    wrfinput\_d02, ...*
-   fieldlist\_file (if specified in namelist)

### File formats

This utility works on any pair of netCDF files, doing a simple read and
copy from one to the other.

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

-   none

[]{#Errors}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

ERROR CODES and CONDITIONS
--------------------------

<div class="errors">

Routine
Message
Comment
replace\_wrf\_fields
Usage: echo infile.nc outfile.nc | ./replace\_wrf\_fields
The program did not read 2 filenames from the console.
replace\_wrf\_fields
cannot specify both fieldnames and fieldlist\_file
In the namelist you must either specify an explicit list of fieldnames
to copy between the files, or give a single filename which contains the
list of field names. You cannot specify both.
replace\_wrf\_fields
*field* not found in input/output file
If 'fail\_on\_missing\_field' is true in the namelist and a field is not
found in either the input or output file.
replace\_wrf\_fields
*field* does not match
If the input and output files have different sizes, number of
dimensions, or data types, the program cannot copy the data.

</div>

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

none.

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
  Contact:           Glen Romine, Nancy Collins
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


