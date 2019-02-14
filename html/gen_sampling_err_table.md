[]{#TOP}

PROGRAM *gen\_sampling\_err\_table*
===================================

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
[TERMS OF USE](#Legalese) []{#Overview}

Overview
--------

Utility program which computes a table of values needed to apply
Sampling Error Correction (SEC) during assimilation. These values are
used to correct covariances based on small sample size statistics. See
[reference](#References) below.

The name of the SEC table is always
*sampling\_error\_correction\_table.nc*. This is a NetCDF format file.
If this file already exists in the current directory any tables for new
ensemble sizes will be appended to the existing file. If the file does
not exist a new file will be created by this tool. The resulting file
should be copied into the current working directory when *filter* is
run.

A file with 40 common ensemble sizes is distributed with the system. Any
new ensemble sizes can be generated on demand. Be aware that the
computation can be time consuming. The job may need to be submitted to a
batch system if many new ensemble sizes are being generated, or start
the job on a laptop and leave it to run overnight.

The file contains a "sparse array" of ensemble sizes. Only sizes which
have an existing table are stored in the file so large ensemble sizes do
not require a large jump in the size of the output file.

This program uses the random number generator to compute the correction
factors. The generator is seeded with the ensemble size so repeated runs
of the program will generate the same values for the tables.

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

    &gen_sampling_error_table_nml
       ens_sizes = -1
       debug = .false.
       /

</div>

\
\

<div>

+-----------------------+-----------------------+-----------------------+
| Item                  | Type                  | Description           |
+=======================+=======================+=======================+
| ens\_sizes            | integer(200)          | List of ensemble      |
|                       |                       | sizes to compute      |
|                       |                       | Sampling Error        |
|                       |                       | Correction tables     |
|                       |                       | for. These do not     |
|                       |                       | need to be in any     |
|                       |                       | particular order.     |
|                       |                       | Duplicates will be    |
|                       |                       | removed and any sizes |
|                       |                       | which already have    |
|                       |                       | tables computed in    |
|                       |                       | the output file will  |
|                       |                       | be skipped. The file  |
|                       |                       | which comes with the  |
|                       |                       | system already has    |
|                       |                       | tables computed for   |
|                       |                       | these ensemble sizes: |
|                       |                       |        ens_sizes = 5, |
|                       |                       |  6, 7, 8, 9, 10, 12,  |
|                       |                       | 14, 15, 16, 18, 20,   |
|                       |                       |                   22, |
|                       |                       |  24, 28, 30, 32, 36,  |
|                       |                       | 40, 44, 48, 49, 50,   |
|                       |                       |                   52, |
|                       |                       |  56, 60, 64, 70, 72,  |
|                       |                       | 80, 84, 88, 90, 96,   |
|                       |                       |                   100 |
|                       |                       | , 120, 140, 160, 180, |
|                       |                       |  200                  |
+-----------------------+-----------------------+-----------------------+
| debug                 | logical               | If true print out     |
|                       |                       | debugging info.       |
+-----------------------+-----------------------+-----------------------+

</div>

\
\

[]{#Examples}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

EXAMPLES
--------

To add tables for ensemble sizes 128 and 256 run the program with this
namelist:

<div>

    &gen_sampling_error_table_nml
       ens_sizes = 128, 256,
       debug = .false.
       /

</div>

[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    types_mod
    utilities_mod
    random_seq_mod
    netcdf

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   output file is always *sampling\_error\_corrrection\_table.nc* If
    one exists new ensemble sizes will be appended. If it doesn't exist
    a new file will be created. This is a NetCDF format file.

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

-   Ref: Anderson, J., 2012: Localization and Sampling Error Correction
    in Ensemble Kalman Filter Data Assimilation. Mon. Wea. Rev., 140,
    2359-2371, doi: 10.1175/MWR-D-11-00013.1.

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
gen\_sampling\_err\_tool

</div>

KNOWN BUGS
----------

This table was regenerated using a different random number generator
than the Lanai release of DART used. If you are doing a regression test
with Lanai and have Sampling Error Correction enabled you will not get
bitwise reproducible results. [Contact the DART
team](mailto:dart@ucar.edu) if you are interested in a table that
includes the exact tables used in the Lanai version.

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


