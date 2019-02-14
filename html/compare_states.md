[]{#TOP}

PROGRAM *compare\_states*
=========================

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

Utility program to compare fields in two NetCDF files and print out the
min and max values from each file and the min and max of the differences
between the two fields. The default is to compare all numeric variables
in the files, but specific variables can be specified in the namelist or
in a separate file. The two input NetCDF filenames are read from the
console or can be echo'd into the standard input of the program.

If you want to restrict the comparison to only specific variables in the
files, specify the list of field names to compare either in the
namelist, or put a list of fields, one per line, in a text file and
specify the name of the text file. Only data arrays can be compared, not
character arrays, strings, or attribute values.

Namelist interface [*&compare\_states\_nml*](#Namelist) must be read
from file *input.nml*.

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

    &compare_states_nml
       do_all_numeric_fields   = .true.
       fieldnames              = ''
       fieldlist_file          = ''
       fail_on_missing_field   = .true.
       only_report_differences = .true.
       debug                   = .false.
      /

</div>

\
\

<div>

  Item                        Type             Description
  --------------------------- ---------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  do\_all\_numeric\_fields    logical          If .true., all integer, float, and double variables in the NetCDF files will have their values compared. If .false. the list of specific variables to be compared must be given either directly in the namelist in the *fieldnames* item, or else the field names must be listed in an ASCII file, one name per line, and the name of that file is specified in *fieldlist\_file*.
  fieldnames                  character list   One or more names of arrays in the NetCDF files to be compared. Only read if *do\_all\_numeric\_fields* is .false.
  fieldlist\_file             character        Name of a text file containing the fieldnames, one per line. It is an error to specify both the fieldnames namelist item and this one. Only read if *do\_all\_numeric\_fields* is .false.
  fail\_on\_missing\_field    logical          If .true. and any one of the field names is not found in both files it is a fatal error. If .false. a message is printed about the missing field but execution continues.
  only\_report\_differences   logical          If .true. only print the name of the variable being tested; skip printing the variable value min and max if the two files are identical. If .false. print more details about both variables which differ and varibles with the same values.
  debug                       logical          If true print out debugging info.

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

-   two NetCDF input files
-   compare\_states.nml
-   field names text file (optionally)

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
compare\_states
Only use single process
Only a single mpi process can be used with this program
compare\_states
must specify data days and times
If overwrite\_data\_time is true, the namelist must include the new day
and time.
compare\_states
output\_is\_model\_advance\_file must be true to set advance time
If overwrite\_advance\_time is true, output\_is\_model\_advance\_file
must also be true.
compare\_states
must specify advance days and times
If overwrite\_advance\_time is true, the namelist must include the new
day and time.
compare\_states
overwrite\_advance\_time must be true if output file has advance time
If the incoming file does not have a model advance time, the output
cannot have one unless the user gives one in the namelist, and sets
overwrite\_advance\_time to true.

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
  Contact:           DART core group
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


