[]{#TOP}

program *perfect\_model\_obs*
=============================

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

Main program for creating synthetic observation sequences given a model
for use in filter assimilations. Reads in an observation sequence file
which has only observation definitions and generates synthetic
observation values for an output observation sequence file. The
execution of perfect\_model\_obs is controlled by the input observation
sequence file and the model time-stepping capabilities in a manner
analogous to that used by the filter program.

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

    &perfect_model_obs_nml
       single_file_in             = .false.,
       read_input_state_from_file = .false.,
       input_state_files          = "",
       init_time_days             = 0,
       init_time_seconds          = 0,

       single_file_out            = .false.,
       output_state_files         = "",
       write_output_state_to_file = .false.,
       output_interval            = 1,

       distributed_state          = .false.,
       async                      = 0,
       adv_ens_command            = "./advance_model.csh",
       tasks_per_model_advance    = 1,

       obs_seq_in_file_name       = "obs_seq.in",
       obs_seq_out_file_name      = "obs_seq.out",
       first_obs_days             = -1,
       first_obs_seconds          = -1,
       last_obs_days              = -1,
       last_obs_seconds           = -1,
       obs_window_days            = -1,
       obs_window_seconds         = -1,

       trace_execution            = .false.,
       output_timestamps          = .false.,
       print_every_nth_obs        = 0,
       output_forward_op_errors   = .false.,
       silence                    = .false.,
    /

</div>

\
\

<div>

+-----------------------+-----------------------+-----------------------+
| Item                  | Type                  | Description           |
+=======================+=======================+=======================+
| read\_input\_state\_f | logical               | If false, model\_mod  |
| rom\_file             |                       | must provide the      |
|                       |                       | input state.          |
+-----------------------+-----------------------+-----------------------+
| single\_file\_in      | logical               | Get all states from a |
|                       |                       | single file.          |
+-----------------------+-----------------------+-----------------------+
| input\_state\_files   | character(len=256)    | A list of files, one  |
|                       | dimension(MAX\_NUM\_D | per domain. Each file |
|                       | OMS)                  | must be a text file   |
|                       |                       | containing the name   |
|                       |                       | of the NetCDF file to |
|                       |                       | open.                 |
+-----------------------+-----------------------+-----------------------+
| write\_output\_state\ | logical               | If false, state is    |
| _to\_file             |                       | not written out.      |
+-----------------------+-----------------------+-----------------------+
| single\_file\_out     | logical               | Write all states to a |
|                       |                       | single file.          |
+-----------------------+-----------------------+-----------------------+
| output\_state\_files  | character(len=256)    | A list of files, one  |
|                       | dimension(MAX\_NUM\_D | per domain. Each file |
|                       | OMS)                  | must be a text file   |
|                       |                       | containing the names  |
|                       |                       | of the NetCDF file to |
|                       |                       | open.                 |
+-----------------------+-----------------------+-----------------------+
| init\_time\_days      | integer               | If negative, don't    |
|                       |                       | use. If non-negative, |
|                       |                       | override the initial  |
|                       |                       | data time read from   |
|                       |                       | restart file.         |
+-----------------------+-----------------------+-----------------------+
| init\_time\_seconds   | integer               | If negative don't     |
|                       |                       | use. If non-negative, |
|                       |                       | override the initial  |
|                       |                       | data time read from   |
|                       |                       | restart file.         |
+-----------------------+-----------------------+-----------------------+
| output\_interval      | integer               | Output state and      |
|                       |                       | observation           |
|                       |                       | diagnostics every nth |
|                       |                       | assimilation time, n  |
|                       |                       | is output\_interval.  |
+-----------------------+-----------------------+-----------------------+
| distributed\_state    | logical               | True means the        |
|                       |                       | ensemble data is      |
|                       |                       | distributed across    |
|                       |                       | all tasks as it is    |
|                       |                       | read in, so a single  |
|                       |                       | task never has to     |
|                       |                       | have enough memory to |
|                       |                       | store the data for an |
|                       |                       | ensemble member.      |
|                       |                       | Large models should   |
|                       |                       | always set this to    |
|                       |                       | .true., while for     |
|                       |                       | small models it may   |
|                       |                       | be faster to set this |
|                       |                       | to .false.            |
+-----------------------+-----------------------+-----------------------+
| async                 | integer               | Controls method for   |
|                       |                       | advancing model:      |
|                       |                       | -   0 = subroutine    |
|                       |                       |     call              |
|                       |                       | -   2 = shell         |
|                       |                       |     command, single   |
|                       |                       |     task model        |
|                       |                       | -   4 = shell         |
|                       |                       |     command, parallel |
|                       |                       |     model             |
+-----------------------+-----------------------+-----------------------+
| adv\_ens\_command     | character(len=129)    | Command sent to shell |
|                       |                       | if async == 2 or 4.   |
+-----------------------+-----------------------+-----------------------+
| tasks\_per\_model\_ad | integer               | Number of tasks to    |
| vance                 |                       | use while advancing   |
|                       |                       | the model.            |
+-----------------------+-----------------------+-----------------------+
| obs\_seq\_in\_file\_n | character(len=256)    | File name from which  |
| ame                   |                       | to read an            |
|                       |                       | observation sequence. |
+-----------------------+-----------------------+-----------------------+
| obs\_seq\_out\_file\_ | character(len=256)    | File name to which to |
| name                  |                       | write output          |
|                       |                       | observation sequence. |
+-----------------------+-----------------------+-----------------------+
| first\_obs\_days      | integer               | If negative, don't    |
|                       |                       | use. If non-negative, |
|                       |                       | ignore any            |
|                       |                       | observations before   |
|                       |                       | this time.            |
+-----------------------+-----------------------+-----------------------+
| first\_obs\_seconds   | integer               | If negative, don't    |
|                       |                       | use. If non-negative, |
|                       |                       | ignore any            |
|                       |                       | observations before   |
|                       |                       | this time.            |
+-----------------------+-----------------------+-----------------------+
| last\_obs\_days       | integer               | If negative, don't    |
|                       |                       | use. If non-negative, |
|                       |                       | ignore any            |
|                       |                       | observations after    |
|                       |                       | this time.            |
+-----------------------+-----------------------+-----------------------+
| last\_obs\_seconds    | integer               | If negative, don't    |
|                       |                       | use. If non-negative, |
|                       |                       | ignore any            |
|                       |                       | observations after    |
|                       |                       | this time.            |
+-----------------------+-----------------------+-----------------------+
| obs\_window\_days     | integer               | If negative, don't    |
|                       |                       | use. If non-negative, |
|                       |                       | reserved for future   |
|                       |                       | use.                  |
+-----------------------+-----------------------+-----------------------+
| obs\_window\_seconds  | integer               | If negative, don't    |
|                       |                       | use. If non-negative, |
|                       |                       | reserved for future   |
|                       |                       | use.                  |
+-----------------------+-----------------------+-----------------------+
| trace\_execution      | logical               | True means output     |
|                       |                       | very detailed         |
|                       |                       | messages about what   |
|                       |                       | routines are being    |
|                       |                       | called in the main    |
|                       |                       | loop. Useful if a job |
|                       |                       | hangs or otherwise    |
|                       |                       | doesn't execute as    |
|                       |                       | expected.             |
+-----------------------+-----------------------+-----------------------+
| output\_timestamps    | logical               | True means output     |
|                       |                       | timestamps before and |
|                       |                       | after the model       |
|                       |                       | advance and the       |
|                       |                       | forward observation   |
|                       |                       | computation phases.   |
+-----------------------+-----------------------+-----------------------+
| print\_every\_nth\_ob | integer               | If negative, don't    |
| s                     |                       | use. If non-negative, |
|                       |                       | print a message       |
|                       |                       | noting the processing |
|                       |                       | of every Nth          |
|                       |                       | observation.          |
+-----------------------+-----------------------+-----------------------+
| output\_forward\_op\_ | logical               | True means output     |
| errors                |                       | errors from forward   |
|                       |                       | observation           |
|                       |                       | operators. This is    |
|                       |                       | the 'istatus' error   |
|                       |                       | return code from the  |
|                       |                       | model interpolate     |
|                       |                       | routine. An ascii     |
|                       |                       | text file             |
|                       |                       | 'forward\_op\_errors' |
|                       |                       | will be created in    |
|                       |                       | the current           |
|                       |                       | directory. Each line  |
|                       |                       | will contain an       |
|                       |                       | observation key       |
|                       |                       | number, and the       |
|                       |                       | istatus return code.  |
+-----------------------+-----------------------+-----------------------+
| silence               | logical               | True means output     |
|                       |                       | almost no runtime     |
|                       |                       | messages. Not         |
|                       |                       | recommended for       |
|                       |                       | general use, but can  |
|                       |                       | speed test programs   |
|                       |                       | if the execution time |
|                       |                       | becomes dominated by  |
|                       |                       | the volume of output. |
+-----------------------+-----------------------+-----------------------+

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
    time_manager_mod
    obs_sequence_mod
    obs_def_mod
    obs_model_mod
    assim_model_mod
    mpi_utilities_mod
    random_seq_mod
    ensemble_manager_mod

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   observation sequence input file; name comes from
    obs\_seq\_in\_file\_name
-   observation sequence output file; name comes from
    obs\_seq\_out\_file\_name
-   input state vector file; name comes from restart\_in\_file\_name
-   output state vector file; name comes from restart\_out\_file\_name
-   perfect\_model\_mod.nml in input.nml

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
perfect\_main
Only use one mpi process here: \#\#\# were requested
Don't use mpi for this.

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


