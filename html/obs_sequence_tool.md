[]{#TOP}

program *obs\_sequence\_tool*
=============================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [EXAMPLES](#Examples) / [BUILDING](#Building) /
[DISCUSSION](#Discussion) / [FAQ](#FAQ) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

Overview
--------

DART observation sequence files are stored in a proprietary format. This
tool makes it easier to manipulate these files, allowing the user to
subset or combine one or more files into a single output file.

The tool has many options to select subsets of observations by time,
type, data value, and location. The tool also allows the contents of
observations to be changed by subsetting and/or reordering the copies
and qc entries. Files with equivalent data but with different metadata
labels (e.g. 'NCEP QC' vs. 'QC') can now be merged as well. The tool can
be run without creating an output file, only printing a summary of the
counts of each observation type in the input files, and it can be used
to convert from binary to ASCII and back.

The actions of the *obs\_sequence\_tool* program are controlled by a
Fortran namelist, read from a file named *input.nml* in the current
directory. A detailed description of each namelist item is described in
the [namelist section](#Namelist) below.

The [examples section](#Examples) of this document below has extensive
examples of common usages for this tool. Below that are more details
about DART observation sequence files, the structure of individual
observations, and general background information.

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

    &obs_sequence_tool_nml
       filename_seq         = ''
       filename_seq_list    = ''
       filename_out         = 'obs_seq.processed'
       first_obs_days       = -1
       first_obs_seconds    = -1
       last_obs_days        = -1
       last_obs_seconds     = -1
       obs_types            = ''
       keep_types           = .true.
       min_box              = -888888.0
       max_box              = -888888.0
       min_lat              =  -90.0
       max_lat              =   90.0
       min_lon              =    0.0
       max_lon              =  360.0
       copy_metadata        = ''
       min_copy             = -888888.0
       max_copy             = -888888.0
       copy_type            = ''
       edit_copy_metadata   = .false.
       new_copy_metadata    = ''
       edit_copies          = .false.
       new_copy_index       = -1
       new_copy_data        = -888888.0
       qc_metadata          = ''
       min_qc               = -888888.0
       max_qc               = -888888.0
       edit_qc_metadata     = .false.
       new_qc_metadata      = ''
       edit_qcs             = .false.
       new_qc_index         = -1
       new_qc_data          = -888888.0
       synonymous_copy_list = ''
       synonymous_qc_list   = ''
       print_only           = .false.
       gregorian_cal        = .true.
       min_gps_height       = -888888.0
       /

</div>

\
\

<div>

  Item                     Type                                  Description
  ------------------------ ------------------------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  filename\_seq            character(len=256), dimension(1000)   The array of names of the observation sequence files to process. (With the F90 namelist mechanism it is only necessary to specify as many names as you have input files; unspecified list values are cleanly ignored.)
  filename\_seq\_list      character(len=256)                    The name of a text file which contains, one per line, the names of the observation sequence files to process. The names on each line in the file should not have any delimiters, e.g. no single or double quotes at the start or end of the filename. This file can be made with a text editor or with the output of the 'ls' command, e.g. *ls obs\_seq.\* &gt; flist*. You can only specify one of *filename\_seq* OR *filename\_seq\_list*, not both.
  filename\_out            character(len=256)                    The name of the resulting output observation sequence file.
  first\_obs\_days         integer                               If non-negative, restrict the timestamps of the observations copied to the output file to be equal to or after this day number (specified in the Gregorian calendar; day number since 1601).
  first\_obs\_seconds      integer                               If non-negative, restrict the timestamps of the observations copied to the output file to be equal to or after this time.
  last\_obs\_days          integer                               If non-negative, restrict the timestamps of the observations copied to the output file to be equal to or before this date (specified in the Gregorian calendar; day number since 1601).
  last\_obs\_seconds       integer                               If non-negative, restrict the timestamps of the observations copied to the output file to be equal to or before this time.
  obs\_types               character(len=32), dimension(500)     The array of observation type names to process. If any names specified, then based on the setting of *keep\_types*, these observation types will either be the only types kept in the output file, or they will be removed and all other types will be copied to the output file.
  keep\_types              logical                               Ignored unless one or more observation types are specified in the *obs\_types* namelist. If .TRUE., only the specified observation types will be copied to the output file; if .FALSE., all types except the listed ones will be copied to the output file.
  min\_box                 real(r8)(:)                           If the locations are 1D, set a min value here instead of using the lat/lon box values.
  max\_box                 real(r8)(:)                           If the locations are 1D, set a max value here instead of using the lat/lon box values.
  min\_lat                 real(r8)                              If specified, the minimum latitude, in degrees, of observations to be copied to the output file. This assumes compiling with the 3d-sphere locations module.
  max\_lat                 real(r8)                              If specified, the maximum latitude, in degrees, of observations to be copied to the output file. This assumes compiling with the 3d-sphere locations module.
  min\_lon                 real(r8)                              If specified, the minimum longitude, in degrees, of observations to be copied to the output file. This assumes compiling with the 3d-sphere locations module. If min\_lon is larger than max\_lon, wrap across 360 to 0 is assumed.
  max\_lon                 real(r8)                              If specified, the maximum longitude, in degrees, of observations to be copied to the output file. This assumes compiling with the 3d-sphere locations module. If min\_lon is larger than max\_lon, wrap across 360 to 0 is assumed.
  copy\_metadata           character                             If specified, the metadata string describing one of the data copy fields in the input observation sequence files.
  min\_copy                real                                  If specified, the minimum value in the data copy field matching the copy\_metadata name that will be copied to the output file.
  max\_copy                real                                  If specified, the maximum value in the data copy field matching the copy\_metadata name that will be copied to the output file.
  copy\_type               character(len=32)                     If specified, the string name of an observation type to be copied to the output file only if the min and max values specified are in range. All other observation types are discarded if this option is specified.
  edit\_copy\_metadata     logical                               If true, replace the output file metadata strings with the list specified in the new\_copy\_metadata list.
  new\_copy\_metadata      character(len=\*)(:)                  List of new metadata strings. Use with care, there is no error checking to ensure you are doing a valid replacement.
  edit\_copies             logical                               If true, subset or rearrange the actual data copies in the output. The new\_copy\_index list controls the output order of copies from the input files.
  new\_copy\_index         integer(:)                            An array of integers, which control how copies in the input are moved to the output sequence. The values must be between 0 and the number of copies in the input sequence. They can be repeated to replicate an existing copy; they can be specified in any order to reorder the entries; they can include the value 0 to insert a new copy. -1 ends the list. If -1 is specified as the first value all copies will be deleted.
  new\_copy\_data          real(:)                               An array of reals. The length should correspond to the number of 0s in the new\_copy\_index list, and will be the data value for the new copies. This value will be constant for all observations.
  qc\_metadata             character                             If specified, the metadata string describing one of the quality control (QC) fields in the input observation sequence files.
  min\_qc                  real                                  If specified, the minimum qc value in the QC field matching the qc\_metadata name that will be copied to the output file.
  max\_qc                  real                                  If specified, the maximum qc value in the QC field matching the qc\_metadata name that will be copied to the output file.
  edit\_qc\_metadata       logical                               If true, replace the output file metadata strings with the list specified in the new\_qc\_metadata list.
  new\_qc\_metadata        character(len=\*)(:)                  List of new metadata strings. Use with care, there is no error checking to ensure you are doing a valid replacement.
  edit\_qcs                logical                               If true, subset or rearrange the actual data QCs in the output. The new\_qc\_index list controls the output order of QCs from the input files.
  new\_qc\_index           integer(:)                            An array of integers, which control how QCs in the input are moved to the output sequence. The values must be between 0 and the number of QCs in the input sequence. They can be repeated to replicate an existing QCs; they can be specified in any order to reorder the entries; they can include the value 0 to insert a new qc. -1 ends the list. If -1 is specified as the first value, all QCs will be deleted.
  new\_qc\_data            real(:)                               An array of reals. The length should correspond to the number of 0s in the new\_qc\_index list, and will be the data value for the new QCs. This value will be constant for all observations.
  synonymous\_copy\_list   character(len=\*)(:)                  An array of strings which are to be considered synonyms in the copy metadata strings for all the input obs seq files. Any string in this list will match any other string. The first obs sequence file to copy observations to the output file will set the actual values used, unless they are explicitly overridden by edit\_copy\_metadata.
  synonymous\_qc\_list     character(len=\*)(:)                  An array of strings which are to be considered synonyms in the qc metadata strings for all the input obs seq files. Any string in this list will match any other string. The first obs sequence file to qc observations to the output file will set the actual values used, unless they are explicitly overridden by edit\_qc\_metadata.
  print\_only              logical                               If .TRUE., do not create an output file, but print a summary of the number and types of each observation in each input file, and then the number of observations and types which would have been created in an output file. If other namelist selections are specified (e.g. start and end times, select by observation type, qc value, etc) the summary message will include the results of that processing.
  gregorian\_cal           logical                               If .true. the dates of the first and last observations in each file will be printed in both (day/seconds) format and in gregorian calendar year/month/day hour:min:sec format. Set this to .false. if the observations were not created with gregorian calendar times.
  num\_input\_files        integer                               DEPRECATED. The number of observation sequence files to process is now set by counting up the number of input filenames specified. This namelist item is ignored and will be removed in future versions of the code.

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

Here are details on how to set up common cases using this tool:

-   [Merge multiple files](#merge)
-   [Subset in Time](#timeclamp)
-   [Subset by Observation Type](#keepobs)
-   [Subset by Location](#byloc)
-   [Binary to ASCII and back](#ascii)
-   [Merging files with incompatible Metadata](#editmeta)
-   [Altering the number of Copies or QC values](#editdata)
-   [Printing only](#print)
-   [Subset by Observation or QC Value](#keepval)

[]{#merge}

### Merge multiple files

Either specify a list of input files for *filename\_seq*, like:

    &obs_sequence_tool_nml
       filename_seq       = 'obs_seq20071101',
                            'qscatL2B_2007_11_01a.out',
                            'obs_seq.gpsro_2007110106',
       filename_out       = 'obs_seq20071101.all',
       gregorian_cal      = .true.
    /

and all observations in each of the three input files will be merged in
time order and output in a single observation sequence file. Or from the
command line create a file containing one filename per line, either with
'ls':

    ls obs_seq_in* > tlist

or with a text editor, or any other tool of your choice. Then,

    &obs_sequence_tool_nml
       filename_seq_list = 'tlist',
       filename_out       = 'obs_seq20071101.all',
       gregorian_cal      = .true.
    /

will open 'tlist' and read the filenames, one per line, and merge them
together. The output file will be named 'obs\_seq20071101.all'. Note
that the filenames inside the list file should not have delimiters (e.g.
single or double quotes) around the filenames.

[]{#timeclamp}

### Subset in Time

The observations copied to the output file can be restricted in time by
setting the namelist items for the first and last observation timestamps
(in days and seconds). It is not an error for some of the input files to
have no observations in the requested time range, and multiple input
files can have overlapping time ranges. For example:

    &obs_sequence_tool_nml
       filename_seq       = 'obs_seq20071101',
                            'qscatL2B_2007_11_01a.out',
                            'obs_seq.gpsro_2007110106',
       filename_out       = 'obs_seq20071101.06hrs',
       first_obs_days     = 148592,
       first_obs_seconds  =  10801,
       last_obs_days      = 148592,
       last_obs_seconds   =  32400,
       gregorian_cal      = .true.
    /

The time range is inclusive on both ends; observations with times equal
to the boundary times will be copied to the output. To split a single
input file up into proper subsets (no replicated observations), the
first time of the following output sequence should be +1 second from the
last time of the previous output sequence. If the goal is to match an
observation sequence file with an assimilation window during the
execution of the *filter* program, the windows should be centered around
the assimilation time starting at minus 1/2 the window time plus 1
second, and ending at exactly plus 1/2 the window time. []{#keepobs}

### Subset by Observation Type

You specify a list of observation types, by string name, and then
specify a logical value to say whether this is the list of observations
to keep, or if it's the list of observations to discard. For example,

    &obs_sequence_tool_nml
       filename_seq       = 'obs_seq20071101.06hrs',
       filename_out       = 'obs_seq20071101.wind',
       obs_types          = 'RADIOSONDE_U_WIND_COMPONENT',
                            'RADIOSONDE_V_WIND_COMPONENT',
       keep_types         = .true.,
       gregorian_cal      = .true.
    /

will create an output file which contains only the U and V wind
observations from the given input file.

    &obs_sequence_tool_nml
       filename_seq       = 'obs_seq20071101.06hrs',
       filename_out       = 'obs_seq20071101.notemp',
       obs_types          = 'RADIOSONDE_TEMPERATURE',
       keep_types         = .false.,
       gregorian_cal      = .true.
    /

will strip out all the radiosonde temperature observations and leave
everything else.

[]{#byloc}

### Subset by Location

If the observations have locations specified in 3 dimensions, as
latitude, longitude, and a vertical coordinate, then it can be subset by
specifying the corners of a lat, lon box. There is currently no vertical
subsetting option. For example:

       min_lat            =    0.0,
       max_lat            =   20.0,
       min_lon            =  230.0,
       max_lon            =  260.0,

will only output observations between 0 and 20 latitude and 230 to 260
in longitude. Latitude ranges are −90 to 90, longitude can either be
specified from −180 to +180, or 0 to 360.

If the observations have 1 dimensional locations, between 0 and 1, then
a bounding box can be specified like:

       min_box = 0.2,
       max_box = 0.4,

will keep only those observations between 0.2 and 0.4. In all these
tests, points on the boundaries are considered inside the box.

[]{#ascii}

### Binary to ASCII and back

To convert a (more compact) binary observation sequence file to a (human
readable and portable) ASCII file, a single input and single output file
can be specified with no selection criteria. The output file format is
specified by the *write\_binary\_obs\_sequence* item in the
*&obs\_sequence\_nml* namelist in the *input.nml* file. It is a Fortran
logical; setting it to *.TRUE.* will write a binary file, setting it to
*.FALSE.* will write an ASCII text file. If you have a binary file, it
must be converted on the same kind of platform as it was created on
before being moved to another architecture. At this point in time, there
are only 2 remaining incompatible platforms: IBM systems based on
PowerPC chips, and everything else (which is Intel or AMD).

Any number of input files and selection options can be specified, as
well, but for a simple conversion, leave all other input namelist items
unset.

[]{#editmeta}

### Merging files with incompatible Metadata

To merge files which have the same number of copies and qc but different
labels for what is exactly the same data, you can specify a list of
synonym strings that will pass the matching test. For example:

    &obs_sequence_tool_nml
       filename_seq       = 'qscatL2B_2007_11_01.out',
                            'obs_seq20071101',
                            'obs_seq.gpsro_2007110124',
       filename_out       = 'obs_seq20071101.all',
       gregorian_cal      = .true.
       synonymous_copy_list = 'NCEP BUFR observation', 'AIRS observation', 'observation',
       synonymous_qc_list   = 'NCEP QC index', 'AIRS QC', 'QC flag - wvc quality flag', 'QC',
    /

will allow any copy listed to match any other copy on that list, and
same with the QC values. If the output metadata strings are not
specified (see below), then the actual metadata strings from the first
file which is used will set the output metadata strings.

To rename or override, with care, existing metadata strings in a file,
set the appropriate edit strings to true, and set the same number of
copies and/or QC values as will be in the output file. Note that this
will replace, without warning, whatever is originally listed as
metadata. You can really mangle things here, so use this with caution:

    &obs_sequence_tool_nml
       filename_seq       = 'qscat_all_qc_305.out', 'qscat_all_qc_306.out',
       filename_out       = 'qscat_1_qc_2007_11.out',
       edit_copy_metadata = .true.,
       new_copy_metadata  = 'observation', 
       edit_qc_metadata   = .true.,
       new_qc_metadata    = 'QC', 'DART quality control',
       gregorian_cal      = .true.
    /

The log file will print out what input strings are being replaced; check
this carefully to be sure you are doing what you expect.

If you use both a synonym list and the edit list, the output file will
have the specified edit list strings for metadata.

[]{#editdata}

### Altering the number of Copies or QC values

To delete some of the copies or QC values in each observation, specify
the copy or QC index numbers which are to be passed through, and list
them in the exact order they should appear in the output:

       edit_copies = .true.,
       new_copy_index = 1, 2, 81, 82,

       edit_qcs = .true.,
       new_qc_index = 2, 

This will create an output sequence file with only 4 copies; the
original first and second copies, and copies 81 and 82. The original
metadata will be retained. It will have only the second QC value from
the original file.

If you are editing the copies or QCs and also specifying new metadata
strings, use the number and order appropriate to the output file
regardless of how many copies or QC values there were in the original
input files.

You can use these index lists to reorder copies or QC values by
specifying the same number of index values as currently exist but list
them in a different order. Index values can be repeated multiple times
in a list. This will duplicate both the metadata string as well as the
data values for the copy or QC.

To delete all copies or QCs specify -1 as the first (only) entry in the
new index list.

       edit_qcs = .true.,
       new_qc_index = -1, 

To add copies or QCs, use 0 as the index value.

       edit_copies = .true.,
       new_copy_index = 1, 2, 0, 81, 82, 0
       new_copy_data = 3.0, 8.0,

       edit_qcs = .true.,
       new_qc_index = 2, 1, 3, 0,
       new_qc_data = 1.0,

This will insert 2 new copies in each observation and give them values
of 3.0 and 8.0 in all observations. There is no way to insert a
different value on a per-obs basis. This example will also reorder the 3
existing QC values and then add 1 new QC value of 1 in all observations.
The 'edit\_copy\_metadata' and 'edit\_qc\_metadata' flags with the
'new\_copy\_metadata' and 'new\_qc\_metadata' lists can be used to set
the metadata names of the new copies and QCs.

       edit_copies = .true.,
       new_copy_index = 1, 0, 2, 0,
       new_copy_data = 3.0, 8.0,
       edit_copy_metadata = .true.,
       new_copy_metadata = 'observation', 'new copy 1',
                           'truth',       'new copy 2',

       edit_qcs = .true.,
       new_qc_index = 0, 2,
       new_qc_data = 0.0,
       edit_qc_metadata = .true.,
       new_qc_metadata = 'dummy QC', 'DART QC',

To remove an existing QC value and add a QC value of 0 for all
observations, run with:

       edit_qcs = .true.,
       new_qc_index = 0,
       new_qc_data = 0.0,
       edit_qc_metadata = .true.,
       new_qc_metadata = 'dummy QC',

to add a constant QC of 0 for all observations, with a metadata label of
'dummy QC'.

It would be useful to allow copies or QCs from one file to be combined,
obs by obs, with those from another file. However, it isn't easy to
figure out how to ensure the observations in multiple files are in
exactly the same order so data from the same obs are being combined.
Also how to specify what should be combined is a bit complicated. So
this functionality is NOT available in this tool.

[]{#print}

### Printing only

Note that you can set all the other options and then set print true, and
it will do all the work and then just print out how many of each obs
type would have been created. It is an easy way to preview what your
choices would do without waiting to write an output file. It only prints
the type breakdown for output file, but does print a running total of
how many obs are being kept from each input file. For example:

    &obs_sequence_tool_nml
       filename_seq       = 'obs_seq20071101',
       print_only         =  .true.,
    /

[]{#keepval}

### Subset by Observation or QC Value

You can specify a min, max data value and/or min, max qc value, and only
those within the range will be kept. There is no exclude option. For the
data value, you must also specify an observation type since different
types have different units and valid ranges. For example:

    # keep only observations with a DART QC of 0:
       qc_metadata        = 'Dart quality control',
       min_qc             = 0,
       max_qc             = 0,

    # keep only radiosonde temp obs between 250 and 300 K:
       copy_metadata      = 'NCEP BUFR observation',
       copy_type          = 'RADIOSONDE_TEMPERATURE',
       min_copy           = 250.0,
       max_copy           = 300.0,

[]{#Discussion}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

DISCUSSION
----------

DART observation sequence files are lists of individual observations,
each with a type, a time, one or more values (called copies), zero or
more quality control flags, a location, and an error estimate.
Regardless of the physical order of the observations in the file, they
are always processed in increasing time order, using a simple linked
list mechanism. This tool reads in one or more input observation
sequence files, and creates a single output observation sequence file
with all observations sorted into a single, monotonically increasing
time ordered output file.

DART observation sequence files contain a header with the total
observation count and a table of contents of observation types. The
output file from this tool culls out unused observations, and only
includes observation types in the table of contents which actually occur
in the output file. The table of contents **does not** need to be the
same across multiple files to merge them. Each file has a self-contained
numbering system for observation types. However, the
*obs\_sequence\_tool* must be compiled with a list of observation types
(defined in the *obs\_def* files listed in the *preprocess* namelist)
which includes all defined types across all input files. See the
[building section](#Building) below for more details on compiling the
tool.

The tool can handle observation sequence files at any stage along the
processing pipeline: a template file with locations but no data, input
files for an assimilation which have observation values only, or output
files from an assimilation which then might include the prior and
posterior mean and standard deviation, and optionally the output from
the forward operator from each ensemble member. In all of these cases,
the format of each individual observation is the same. It has zero or
more *copies*, which is where the observation value and the means,
forward operators, etc are stored. Each observation also has zero or
more quality control values, *qc*, which can be associated with the
incoming data quality, or can be added by the DART software to indicate
how the assimilation processed this observation. Each of the copies and
qc entries has an single associated character label at the start of the
observation sequence file which describes what each entry is, called the
*metadata*.

For multiple observation sequence files to be merged they must have the
same number of *copies* and *qc* values, and all associated *metadata*
must be identical. To merge multiple files where the numbers do not
match exactly, the tool can be used on the individual files to rename,
subset, and reorder the *copies* and/or *qc* first, and then the
resulting files are mergeable. To merge multiple files where the
metadata strings do not match, but the data copy or qc values are indeed
the same things, there are options to rename the metadata strings.
**This option should be used with care. If the copies or qc values in
different files are not really the same, the tool will go ahead and
merge them but the resulting file will be very wrong.**

The tool offers an additional option for specifying a list of input
files. The user creates an ASCII file by any desired method (e.g. ls
&gt; file, editor), with one filename per line. The names on each line
in the file should not have any delimiters, e.g. no single or double
quotes at the start or end of the filename. They specify this file with
the *filename\_seq\_list* namelist item, and the tool opens the list
file and processes each input file in turn. The namelist item
*num\_input\_files* is now DEPRECATED and is ignored. The number of
input files is computed from either the explicit list in
*filename\_seq*, or the contents of the *filename\_seq\_list* file.

Time is stored inside of DART as a day number and number of seconds,
which is the same no matter which calendar is being used. But many
real-world observations use the Gregorian calendar for converting
between number of days and an actual date. If the *gregorian\_cal*
namelist item is set to *.TRUE.* then any times will be printed out to
the log file will be both in day/seconds and calendar date. If the
observation times are not using the Gregorian calendar, then set this
value to *.FALSE.* and only days/seconds will be printed.

The most common use of this tool is to process a set of input files into
a single output file, or to take one input file and extract a subset of
observations into a smaller file. The [examples](#Examples) section
below outlines several common scenerios.

The tool now also allows the number of copies to be changed, but only to
select subsets or reorder them. It is not yet possible to merge copies
or QCs from observations in different files into a single observation
with more copies.

Observations can also be selected by a given range of quality control
values or data values.

Observations can be restricted to a given bounding box, either in
latitude and longitude (in the horizontal only), or if the observations
have 1D locations, then a single value for min\_box and max\_box can be
specified to restrict the observations to a subset of the space.

[]{#FAQ}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FAQ
---

### Can I merge files where the observation types are different?

Yes. The numbering in the table of contents at the top of each file is
only local to that file. All processing of types is done with the string
name, not the numbers. Neither the set of obs types, nor the observation
numbers need to match across files.

### I get an error about unknown observation types.

Look at the *&preprocess\_nml* namelist in the input.nml file in the
directory where your tool was built. It must have all the observation
types you need to handle listed in the *input\_files* item.

### Can I list more files than necessary in my input file list?

Sure. It will take slightly longer to run, in that the tool must open
the file and check the times and observation types. But it is not an
error to list files where no observations will be copied to the output
file. It is a common task to list a set of observation files and then
set the first and last observation times, run the tool to select a
shorter time period, then change the first and last times and run again
with the same list of files.

[]{#Building}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

BUILDING
--------

Most `$DART/models/*/work` directories will build the tool along with
other executable programs. It is also possible to build the tool in the
`$DART/observations/utilities` directory. The `preprocess` program must
be built and run first, to define what set of observation types will be
supported. See the [preprocess
documentation](../../../assimilation_code/programs/preprocess/preprocess.html)
for more details on how to define the list and run it. The combined list
of all observation types which will be encountered over all input files
must be in the preprocess input list. The other important choice when
building the tool is to include a compatible locations module. For the
low-order models, the `oned` module should be used; for real-world
observations, the `threed_sphere` module should be used.

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
-   The input files specified in the *filename\_seq* namelist variable,
    or inside the file named in *filename\_seq\_list*.
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
obs\_sequence\_tool
num\_input\_files &gt; max\_num\_input\_files.
The limit is currently 1000 files. Change 'max\_num\_input\_files' in
the source file and recompile.
obs\_sequence\_tool
use either lat/lon box or min/max box but not both
When selecting a region you can specify a box by latitude/longitude
namelist limits, or you can use the min/max box namelist lists but not
both.
obs\_sequence\_tool
can only use lat/lon box with 2d/3d sphere locations
The lat/lon limits work only for the 2D and 3D sphere locations.
obs\_sequence\_tool
min\_lat must be less than max\_lat
adjust region limits so min is less than max
obs\_sequence\_tool
min\_lat cannot be less than -90.0 degrees\
max\_lat cannot be greater than 90.0 degrees\
min\_lon cannot be greater than 360.0 degrees\
max\_lon cannot be greater than 360.0 degrees\
fix latitude limits to be within -90 to 90, longitude limits to be 0 to
360. If longitude is negative, 360 will be added so values of -180 to
180 are ok.
obs\_sequence\_tool
min\_lon cannot exactly equal max\_lon\
min\_lat cannot exactly equal max\_lat
the region select box must have a positive volume
obs\_sequence\_tool
must specify the metadata name of a QC field
if selecting observations by QC value, you must specify the metadata
string for which QC field to use.
obs\_sequence\_tool
must specify the metadata name of a copy field
if selecting observations by value, you must specify the metadata string
for which copy field to use.
obs\_sequence\_tool
first time cannot be later than last time
if selecting a time range, the interval must be legal
obs\_sequence\_tool
new\_copy\_index values must be between 0 and N
if reordering or selecting only certain copies the list must only
include valid indices from the input sequence
obs\_sequence\_tool
new\_qc\_index values must be between 0 and N
if reordering or selecting only certain QCs the list must only include
valid indices from the input sequence
obs\_sequence\_tool
All input files are empty or all obs excluded by time/type/location
The selection criteria has excluded all the possible observations, or
none of the input observation sequence files contain observations.
obs\_sequence\_tool
Internal error trying to process file
Shouldn't happen. Contact the DART development team with details of how
this occurred.
obs\_sequence\_tool
observations must be in increasing time order
one or more of the input observation sequence files has
out-of-time-order observations. This should not happen if the input file
was created with DART subroutines.
obs\_sequence\_tool
cannot specify both filename\_seq and filename\_seq\_list
In the input namelist you can either give a file or a list of files for
the 'filename\_seq' item, or you can give the name of a file that
contains the names in 'filename\_seq\_list'. You cannot specify both.
Set one of these to ' '.
obs\_sequence\_tool
contains no filenames
the 'filename\_seq\_list' file contains no filenames to be used as input
observation sequence files

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

-   Combine copies or qc fields from multiple files into a single
    observation
-   Select based on vertical location value. (Complicated by the options
    of specifying vertical in meters, pressure, or model levels.) The
    source code already contains a commented-out subroutine that can
    select in the vertical. It can be uncommented and used if needed.
-   Sort obs with identical timestamps by a specified type list, so one
    kind of ob gets assimilated before another.
-   Sort obs with idential timestamps so any at the same location are
    contiguous in file (so the internal caching of previous locations
    will be more efficient).
-   Remove duplicates (difficult to accurately compare floating point
    values, but identical obs should be possible to recognize, or some
    epsilon could be specified.)
-   A way to split a single input file into multiple output files, based
    on a time interval. Now with the *schedule* type, specifying the
    intervals is easy; need a way to generate the output filenames with
    a pattern.
-   Eventually move to NetCDF for observation sequence files. The
    complication is that it's possible for observations to contain
    additional information on a per-type basis, which means the arrays
    of data are no longer the same size. Perhaps features of NetCDF 4
    can be used in this case.

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


