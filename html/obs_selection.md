[]{#TOP}

program *obs\_selection*
========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [BUILDING](#Building) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

Overview
--------

This specialized tool selects a subset of input observations from an
observation sequence file. For a more general purpose observation
sequence file tool, see the
[obs\_sequence\_tool](../obs_sequence_tool/obs_sequence_tool.html). This
tool takes a selected list of observation types, times, and locations,
and extracts only the matching observations out of one or more
obs\_sequence files. The tool which creates the input selection file is
usually [obs\_seq\_coverage](../obs_seq_coverage/obs_seq_coverage.html).
Alternatively, the selection file can be a full observation sequence
file, in which case the types, times, and locations of those
observations are used as the selection criteria.

This tool processes each observation sequence file listed in the input
namelist *filename\_seq* or *filename\_seq\_list*. If the observation
type, time and location matches an entry in the selection file, it is
copied through to the output. Otherwise it is ignored.

The actions of the *obs\_selection* program are controlled by a Fortran
namelist, read from a file named *input.nml* in the current directory. A
detailed description of each namelist item is described in the [namelist
section](#Namelist) of this document. The names used in this discussion
refer to these namelist items.

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

    &obs_selection_nml
       filename_seq          = ''
       filename_seq_list     = ''
       filename_out          = 'obs_seq.processed'
       num_input_files       = 0
       selections_file       = 'obsdef_mask.txt'
       selections_is_obs_seq = .false.
       latlon_tolerance      = 0.000001
       match_vertical        = .false.
       surface_tolerance     = 0.0001
       pressure_tolerance    = 0.001
       height_tolerance      = 0.0001
       scaleheight_tolerance = 0.001
       level_tolerance       = 0.00001
       print_only            = .false.
       partial_write         = .false.
       print_timestamps      = .false.
       calendar              = 'Gregorian'
      /

</div>

\
\

<div>

  Item                       Type                                 Description
  -------------------------- ------------------------------------ ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  filename\_seq              character(len=256), dimension(500)   The array of names of the observation sequence files to process, up to a max count of 500 files. (Specify only the actual number of input files. It is not necessary to specify 500 entries.)
  filename\_seq\_list        character(len=256)                   An alternative way to specify the list of input files. The name of a text file which contains, one per line, the names of the observation sequence files to process. You can only specify one of filename\_seq OR filename\_seq\_list, not both.
  num\_input\_files          integer                              Optional. The number of observation sequence files to process. Maximum of 500. If 0, the length is set by the number of input files given. If non-zero, must match the given input file list length. (Can be used to verify the right number of input files were processed.)
  filename\_out              character(len=256)                   The name of the resulting output observation sequence file. There is only a single output file from this tool. If the input specifies multiple obs\_seq input files, the results are concatinated into a single output file.
  selections\_file           character(len=256)                   The name of the input file containing the mask of observation definitions (the textfile output of [obs\_seq\_coverage](../obs_seq_coverage/obs_seq_coverage.html)). Alternatively, this can be the name of a full observation sequence file. In this case, the types, times, and locations are extracted from this file and then used in the same manner as a mask file from the coverage tool.
  selections\_is\_obs\_seq   logical                              If .TRUE. the filename given for the "selections\_file" is a full obs\_sequence file and not a text file from the coverage tool.
  latlon\_tolerance          real(r8)                             Specified in degrees. For observations to match in the horizontal the difference in degrees for each of latitude and longitude must be less than this threshold. If less than or equal to 0, the values must match exactly.
  match\_vertical            logical                              If .TRUE. the locations of the observations in the input files have to match the selection list not only the horizontal but also in the vertical.
  surface\_tolerance         real(r8)                             Specified in meters. If "match\_vertical" is .FALSE. this value is ignored. If "match\_vertical" is .TRUE., this applies to observations with a vertical type of VERTISSURFACE. For observations which match in the horizontal, the vertical surface elevation difference must be less than this to be considered the same.
  pressure\_tolerance        real(r8)                             Specified in pascals. If "match\_vertical" is .FALSE. this value is ignored. If "match\_vertical" is .TRUE., this applies to observations with a vertical type of VERTISPRESSURE. For observations which match in the horizontal, the vertical difference must be less than this to be considered the same.
  height\_tolerance          real(r8)                             Specified in meters. If "match\_vertical" is .FALSE. this value is ignored. If "match\_vertical" is .TRUE., this applies to observations with a vertical type of VERTISHEIGHT. For observations which match in the horizontal, the vertical difference must be less than this to be considered the same.
  scaleheight\_tolerance     real(r8)                             Specified in unitless values. If "match\_vertical" is .FALSE. this value is ignored. If "match\_vertical" is .TRUE., this applies to observations with a vertical type of VERTISSCALEHEIGHT. For observations which match in the horizontal, the vertical difference must be less than this to be considered the same.
  level\_tolerance           real(r8)                             Specified in fractional model levels. If "match\_vertical" is .FALSE. this value is ignored. If "match\_vertical" is .TRUE., this applies to observations with a vertical type of VERTISLEVEL. For observations which match in the horizontal, the vertical difference must be less than this to be considered the same. Note that some models only support integer level values, but others support fractional levels. The vertical value in an observation is a floating point/real value, so fractional levels are possible to specify for an observation.
  print\_only                logical                              If .TRUE. do not create an output file, but print a summary of the number and types of each observation in each input file, and then the number of observations and types which would have been created in an output file.
  partial\_write             logical                              Generally only used for debugging problems. After each input obs\_seq file is processed, this flag, if .TRUE., causes the code to write out the partial results to the output file. The default is to process all input files (if more than a single file is specified) and write the output file only at the end of the processing.
  print\_timestamps          logical                              Generally only used for debugging very slow execution runs. This flag, if .TRUE., causes the code to output timestamps (wall clock time) at various locations during the processing phases. It may help isolate where particularly slow execution times are occurring. For very large input files, or long lists of input files, it can also help to estimate what the eventual run time of the job will be.
  calendar                   character(len=32)                    Set to the name of the calendar; only controls the printed output for the dates of the first and last observations in the file. Set this to "no\_calendar" if the observations are not using any calendar.

</div>

\
\
[]{#Building}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

BUILDING
--------

Most *\$DART/models/\*/work* directories contain files needed to build
this tool along with the other executable programs. It is also possible
to build this tool in the *\$DART/observations/utilities* directory. In
either case the *preprocess* program must be built and run first to
define what set of observation types will be supported. See the
[preprocess
documentation](../../../assimilation_code/programs/preprocess/preprocess.html)
for more details on how to define the list and run it. The
*&preprocess\_nml* namelist in the *input.nml* file must contain files
with definitions for the combined set of all observation types which
will be encountered over all input obs\_seq files. The other important
choice when building the tool is to include a compatible locations
module in the *path\_names\_obs\_selection* file. For the low-order
models the *oned* module should be used; for real-world observations the
*threed\_sphere* module should be used.

Usually the directories where executables are built will include a
*quickbuild.csh* script which builds and runs preprocess and then builds
the rest of the executables by executing all files with names starting
with *mkmf\_*. If the obs\_selection tool is not built because there is
no *mkmf\_obs\_selection* and *path\_names\_obs\_selection* file in the
current directory they can be copied from another model. The
*path\_names\_obs\_selection* file will need to be edited to be
consistent with the model you are building.

[]{#OtherModulesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    types_mod
    utilities_mod
    time_manager_mod
    obs_def_mod
    obs_sequence_mod

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   *input.nml*
-   The input files specified in the *filename\_seq* namelist variable.
-   The output file specified in the *filename\_out* namelist variable.

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
obs\_selection
num\_input\_files &gt; max\_num\_input\_files. change
max\_num\_input\_files in source file
The default is 500 files.
obs\_selection
num\_input\_files and filename\_seq mismatch
The number of filenames does not match the filename count.

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

Long laundry list of things this tool could do, including:

-   Remove duplicates. Lots of clever bookkeeping will be needed to make
    sure this doesn't run excruciatingly slow.
-   General superob functions could be added - all observations within a
    given tolerance could be combined. Hard questions are how to specify
    the error of the combined observation, and how to combine obs near
    or over the poles.

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


