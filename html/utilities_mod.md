[]{#TOP}

MODULE utilities\_mod
=====================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [INTERFACES](#Interface) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[PRIVATE COMPONENTS](#PrivateComponents) / [TERMS OF USE](#Legalese)

Overview
--------

Provides a number of tools used by most DART modules including tools for
file IO, diagnostic tools for registering modules and recording namelist
arguments, and an error handler.

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

    &utilities_nml
       TERMLEVEL      = 2,
       logfilename    = 'dart_log.out',
       nmlfilename    = 'dart_log.nml',
       module_details = .true.,
       print_debug    = .false.,
       write_nml      = 'file'
    /

</div>

\
\

The namelist controls how the logging, namelist, messages, and general
utility routines behave.

<div>

  Item              Type                 Description
  ----------------- -------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  TERMLEVEL         integer              Level at which calls to error manager terminate program. The default setting is warnings and errors terminate the program. Setting this to 2 (E\_ERR) means only errors terminate. Setting this to 3 means even errors do not cause an exit (which is not a good idea).
  logfilename       character(len=129)   File to which the log messages are written.
  nmlfilename       character(len=129)   File to which the namelist output is written. Can be the same name as the logfile.
  module\_details   logical              Each source code module can write out the repository version number and filename to the logfile. Verbose, but useful for knowing what version of the code was used during the run.
  print\_debug      logical              Setting this to .true. causes additional debug messages to print. These can be very verbose and by default are turned off.
  write\_nml        character(len=32)    String which controls where to write the namelist values that are being used for this execution. Valid values are: 'none', 'file', 'terminal', 'both'. 'none' turns off this write. 'file' writes a copy only to the *nmlfilename*. Writes are always in append mode, so the most recent information will be at the end of an existing file. 'terminal' will write to the job's standard output. 'both' will write both to the nml file and the standard output unit.

</div>

\
\
[]{#OtherModulesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    types_mod
    netCDF

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

  ------------------------- ----------------------------------------------------
  *use utilities, only :*   [file\_exist](#file_exist)
                            [get\_unit](#get_unit)
                            [open\_file](#open_file)
                            [close\_file](#close_file)
                            [timestamp](#timestamp)
                            [register\_module](#register_module)
                            [error\_handler](#error_handler)
                            [to\_upper](#to_upper)
                            [nc\_check](#nc_check)
                            [logfileunit](#logfileunit)
                            [nmlfileunit](#nmlfileunit)
                            [initialize\_utilities](#initialize_utilities)
                            [finalize\_utilities](#finalize_utilities)
                            [dump\_unit\_attributes](#dump_unit_attributes)
                            [find\_namelist\_in\_file](#find_namelist_in_file)
                            [check\_namelist\_read](#check_namelist_read)
                            [find\_textfile\_dims](#find_textfile_dims)
                            [file\_to\_text](#file_to_text)
                            [is\_longitude\_between](#is_longitude_between)
                            [get\_next\_filename](#get_next_filename)
                            [set\_filename\_list](#set_filename_list)
                            [set\_tasknum](#set_tasknum)
                            [set\_output](#set_output)
                            [do\_output](#do_output)
                            [E\_DBG, DEBUG](#ERROR_LEVELS)
                            [E\_MSG, MESSAGE](#ERROR_LEVELS)
                            [E\_WARN, WARNING](#ERROR_LEVELS)
                            [E\_ERR, FATAL](#ERROR_LEVELS)
  ------------------------- ----------------------------------------------------

A note about documentation style. Optional arguments are enclosed in
brackets *\[like this\]*.

[]{#file_exist}\

<div class="routine">

*var = file\_exist(file\_name)*
    logical                      :: file_exist
    character(len=*), intent(in) :: file_name

</div>

<div class="indent1">

Returns true if file\_name exists in the working directory, else false.

  ---------------- -------------------------------------------------
  *var  *          True if file\_name exists in working directory.
  *file\_name  *   Name of file to look for.
  ---------------- -------------------------------------------------

</div>

\
[]{#get_unit}\

<div class="routine">

*var = get\_unit()*
    integer :: get_unit

</div>

<div class="indent1">

Returns an unused unit number for IO.

  --------- ------------------------
  *var  *   An unused unit number.
  --------- ------------------------

</div>

\
[]{#open_file}\

<div class="routine">

*var = open\_file(fname *\[, form, action\]*)*
    integer                                :: open_file
    character(len=*), intent(in)           :: fname
    character(len=*), optional, intent(in) :: form
    character(len=*), optional, intent(in) :: action

</div>

<div class="indent1">

Returns a unit number that is opened to the file fname. If form is not
present or if form is "formatted" or "FORMATTED", file is opened for
formatted IO. Otherwise, it is unformatted. The action string is the
standard action string for Fortran IO (see F90 language description).

  ------------ --------------------------------------------------------------------------------------------------------
  *var  *      Unit number opened to file fname.
  *fname  *    Name of file to be opened.
  *form  *     Format: 'formatted' or 'FORMATTED' give formatted, anything else is unformatted. Default is formatted.
  *action  *   Standard fortran string description of requested file open action.
  ------------ --------------------------------------------------------------------------------------------------------

</div>

\
[]{#timestamp}\

<div class="routine">

*call timestamp(*\[string1, string2, string3,\]  *pos)*
    character(len=*), optional, intent(in) :: string1
    character(len=*), optional, intent(in) :: string2
    character(len=*), optional, intent(in) :: string3
    character(len=*), intent(in)           :: pos

</div>

<div class="indent1">

Prints the message 'Time is YYYY MM DD HH MM SS' to the logfile along
with three optional message strings. If the pos argument is 'end', the
message printed is 'Finished... at YYYY MM DD HH MM SS' and the logfile
is closed.

  ------------- ---------------------------------------
  *string1  *   An optional message to be printed.
  *string2  *   An optional message to be printed.
  *string3  *   An optional message to be printed.
  *pos  *       If 'end' terminates log\_file output.
  ------------- ---------------------------------------

</div>

\
[]{#close_file}\

<div class="routine">

*call close\_file(iunit)*
    integer, intent(in) :: iunit

</div>

<div class="indent1">

Closes the given unit number. If the unit is not open, nothing happens.

  ----------- -------------------------
  *iunit  *   File unit to be closed.
  ----------- -------------------------

</div>

\
[]{#register_module}\

<div class="routine">

*call register\_module(src, rev, rdate)*
    character(len=*), intent(in) :: src
    character(len=*), intent(in) :: rev
    character(len=*), intent(in) :: rdate

</div>

<div class="indent1">

Writes the source name, revision number and revision date to both the
logfileunit and to standard out. All dart modules are supposed to
register when first called.

  ----------- ----------------------------
  *src  *     source file name.
  *rev  *     Revision number of source.
  *rdate  *   Date of revision.
  ----------- ----------------------------

</div>

\
[]{#error_handler}\

<div class="routine">

*call error\_handler(level, routine, text, src, rev, rdate *\[, aut,
text2, text3\]*)*
    integer, intent(in)                    :: level
    character(len=*), intent(in)           :: routine
    character(len=*), intent(in)           :: text
    character(len=*), intent(in)           :: src
    character(len=*), intent(in)           :: rev
    character(len=*), intent(in)           :: rdate
    character(len=*), optional, intent(in) :: aut
    character(len=*), optional, intent(in) :: text2
    character(len=*), optional, intent(in) :: text3

</div>

<div class="indent1">

Prints an error message to standard out and to the logfileunit. The
message contains the routine name, an error message, the source file,
revision and revision date, and optionally the author. The level of
severity is message, debug, warning, or error. If the level is greater
than or equal to the TERMLEVEL (set in the namelist), execution is
terminated. The default TERMLEVEL only stops for ERRORS.

  ------------- ---------------------------------------------------------------------------------
  *level  *     Error severity (message, debug, warning, error). See below for specific ations.
  *routine  *   Name of routine generating error.
  *text  *      Error message.
  *src  *       Source file containing routine generating message.
  *rev  *       Revision number of source file.
  *rdate  *     Revision date of source file.
  *aut  *       Author of routine.
  *text2  *     If specified, the second line of text for the error message.
  *text3  *     If specified, the third line of text for the error message.
  ------------- ---------------------------------------------------------------------------------

</div>

\
[]{#find_namelist_in_file}\

<div class="routine">

*call find\_namelist\_in\_file(namelist\_file\_name, nml\_name, iunit,
*\[,write\_to\_logfile\_in\]*)*
    character(len=*),  intent(in)          :: namelist_file_name
    character(len=*),  intent(in)          :: nml_name
    integer,           intent(out)         :: iunit
    logical, optional, intent(in)          :: write_to_logfile_in

</div>

<div class="indent1">

Opens the file namelist\_file\_name if it exists on unit iunit. A fatal
error occurs if the file does not exist (DART requires an input.nml to
be available, even if it contains no values). Searches through the file
for a line containing ONLY the string &nml\_name (for instance
&filter\_nml if nml\_name is "filter\_nml"). If this line is found, the
file is rewound and the routine returns. Otherwise, a fatal error
message is issued.

  ---------------------------- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *namelist  *                 Name of file assumed to hold the namelist.
  *nml\_name  *                Name of the namelist to be searched for in the file, for instance, filter\_nml.
  *iunit  *                    Channel number on which file is opened.
  *write\_to\_logfile\_in  *   When the namelist for the utilities module is read, the logfile has not yet been open because its name is in the namelist. If errors are found, have to write to standard out. So, when utilities module calls this internally, this optional argument is set to false. For all other applications, it is normally not used (default is false).
  ---------------------------- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#check_namelist_read}\

<div class="routine">

*call check\_namelist\_read(iunit, iostat\_in, nml\_name, *\[,
write\_to\_logfile\_in\]*)*
    integer, intent(in)                    :: iunit
    integer, intent(in)                    :: iostat_in
    character(len=*), intent(in)           :: nml_name
    logical, optional, intent(in)          :: write_to_logfile_in

</div>

<div class="indent1">

Once a namelist has been read from an opened namelist file, this routine
checks for possible errors in the read. If the namelist read was
successful, the file opened on iunit is closed and the routine returns.
If iostat is not zero, an attempt is made to rewind the file on iunit
and read the last line that was successfully read. If this can be done,
this last line is printed with the preamble "INVALID NAMELIST ENTRY". If
the attempt to read the line after rewinding fails, it is assumed that
the original read (before the call to this subroutine) failed by
reaching the end of the file. An error message stating that the namelist
started but was never terminated is issued.

  ---------------------------- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *iunit  *                    Channel number on which file is opened.
  *iostat\_in  *               Error status return from an attempted read of a namelist from this file.
  *nml\_name  *                The name of the namelist that is being read (for instance filter\_nml).
  *write\_to\_logfile\_in  *   When the namelist for the utilities module is read, the logfile has not yet been open because its name is in the namelist. If errors are found, have to write to standard out. So, when utilities module calls this internally, this optional argument is set to false. For all other applications, it is normally not used (default is false).
  ---------------------------- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#find_textfile_dims}\

<div class="routine">

*call find\_textfile\_dims (fname, nlines, linelen)*
    character(len=*), intent (IN)  :: fname
    integer,          intent (OUT) :: nlines
    integer,          intent (OUT) :: linelen

</div>

<div class="indent1">

Determines the number of lines and maximum line length of an ASCII text
file.

  ----------- --------------------------------------------
  *fname*     input, character string file name
  *nlines*    output, number of lines in the file
  *linelen*   output, length of longest line in the file
  ----------- --------------------------------------------

</div>

\
[]{#file_to_text}\

<div class="routine">

*call file\_to\_text (fname, textblock)*
    character(len=*),               intent (IN)  :: fname
    character(len=*), dimension(:), intent (OUT) :: textblock

</div>

<div class="indent1">

Opens the given filename and reads ASCII text lines into a character
array.

  ------------- ---------------------------------------------
  *fname*       input, character string file name
  *textblock*   output, character array of text in the file
  ------------- ---------------------------------------------

</div>

\
[]{#is_longitude_between}\

<div class="routine">

*var = is\_longitude\_between(lon, minlon, maxlon *\[, doradians\]*)*
    real(r8), intent(in)           :: lon
    real(r8), intent(in)           :: minlon
    real(r8), intent(in)           :: maxlon
    logical,  intent(in), optional :: doradians
    logical                        :: is_longitude_between

</div>

<div class="indent1">

Uniform way to test longitude ranges, in degrees, on a globe. Returns
true if lon is between min and max, starting at min and going EAST until
reaching max. Wraps across 0 longitude. If min equals max, all points
are inside. Includes endpoints. If optional arg doradians is true, do
computation in radians between 0 and 2\*PI instead of default 360. There
is no rejection of input values based on range; they are all converted
to a known range by calling modulo() first.

  ------------- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *var  *       True if lon is between min and max.
  *lon*         Location to test.
  *minlon*      Minimum longitude. Region will start here and go east.
  *maxlon*      Maximum longitude. Region will end here.
  *doradians*   Optional argument. Default computations are in degrees. If this argument is specified and is .true., do the computation in radians, and wrap across the globe at 2 \* PI. All inputs must then be specified in radians.
  ------------- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#get_next_filename}\

<div class="routine">

*var = get\_next\_filename( listname, lineindex )*
    character(len=*),  intent(in) :: listname
    integer,           intent(in) :: lineindex
    character(len=128)            :: get_next_filename

</div>

<div class="indent1">

Returns the specified line of a text file, given a filename and a line
number. It returns an empty string when the line number is larger than
the number of lines in a file.

Intended as an easy way to process a list of files. Use a command like
'ls &gt; out' to create a file containing the list, in order, of files
to be processed. Then call this function with an increasing index number
until the return value is empty.

  ------------- ---------------------------------------------------------------------------------------------------------------------------
  *var*         An ascii string, up to 128 characters long, containing the contents of line *lineindex* of the input file.
  *listname*    The filename to open and read lines from.
  *lineindex*   Integer line number, starting at 1. If larger than the number of lines in the file, the empty string '' will be returned.
  ------------- ---------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#set_filename_list}\

<div class="routine">

*var = set\_filename\_list( name\_array, listname, caller\_name )*
    character(len=*),  intent(inout) :: name_array
    character(len=*),  intent(in)    :: listname
    character(len=*),  intent(in)    :: caller_name
    integer                          :: var

</div>

<div class="indent1">

Returns the count of filenames specified. Verifies that one of either
the name\_array or the listname was specified but not both. If the input
was a listname copy the names into the name\_array so when this routine
returns all the filenames are in name\_array(). Verifies that no more
than the allowed number of names was specified if the input was a
listname file.

  ---------------- ---------------------------------------------------------------------------------------------------------------------------------
  *var*            The count of input files specified.
  *name\_array*    Array of input filename strings. Either this item or the listname must be specified, but not both.
  *listname*       The filename to open and read filenames from, one per line. Either this item or the name\_array must be specified but not both.
  *caller\_name*   Calling subroutine name, used for error messages.
  ---------------- ---------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#to_upper}\

<div class="routine">

*call to\_upper(string)*
    character(len=*), intent (INOUT) :: string

</div>

<div class="indent1">

Converts the character string to UPPERCASE - in place. The input string
**is** modified.

  ---------- ----------------------
  *string*   any character string
  ---------- ----------------------

</div>

\
[]{#nc_check}\

<div class="routine">

*call nc\_check(istatus, subr\_name *\[, context\]*)*
    integer, intent(in)                    :: istatus
    character(len=*), intent(in)           :: subr_name
    character(len=*), optional, intent(in) :: context

</div>

<div class="indent1">

Check the return code from a netcdf call. If no error, return without
taking any action. If an error is indicated (in the *istatus* argument)
then call the error handler with the subroutine name and any additional
context information (e.g. which file or which variable was being
processed at the time of the error). All errors are currently hardcoded
to be *FATAL* and this routine will not return.

This routine calls a netCDF library routine to construct the text error
message corresponding to the error code in the first argument. An
example use of this routine is:
    call nc_check(nf90_create(path = trim(ncFileID%fname), cmode = nf90_share, ncid = ncFileID%ncid), &
                 'init_diag_output', 'create '//trim(ncFileID%fname))

  ---------------- ---------------------------------------------------------------------------------------------------------------------------
  *istatus  *      The return value from any netCDF call.
  *subr\_name  *   String name of the current subroutine, used in case of error.
  *context  *      Additional text to be used in the error message, for example to indicate which file or which variable is being processed.
  ---------------- ---------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#set_tasknum}\

<div class="routine">

*call set\_tasknum(tasknum)*
    integer, intent(in)               :: tasknum

</div>

<div class="indent1">

Intended to be used in the MPI multi-task case. Sets the local task
number, which is then prepended to subsequent messages.

  ------------- -----------------------------------------------------------------------------------------------------------------------
  *tasknum  *   Task number returned from MPI\_Comm\_Rank(). MPI task numbers are 0 based, so for a 4-task job these numbers are 0-3.
  ------------- -----------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#set_output}\

<div class="routine">

*call set\_output(doflag)*
    logical, intent(in)               :: doflag

</div>

<div class="indent1">

Set the status of output. Can be set on a per-task basis if you are
running with multiple tasks. If set to false only warnings and fatal
errors will write to the log. The default in the multi-task case is
controlled by the MPI module initialization code, which sets task 0 to
.TRUE. and all other tasks to .FALSE.

  ------------ -----------------------------------------------------------------------------------------------------------------------------------------
  *doflag  *   Sets, on a per-task basis, whether messages are to be written to the logfile or standard output. Warnings and errors are always output.
  ------------ -----------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#do_output}\

<div class="routine">

*var = do\_output()*
    logical                      :: do_output

</div>

<div class="indent1">

Returns true if this task should write to the log, false otherwise. Set
by the *set\_output()* routine. Defaults to true for the single task
case. Can be used in code like so:

    if (do_output()) then
     write(*,*) 'At this point in the code'
    endif

  --------- ----------------------------------------
  *var  *   True if this task should write output.
  --------- ----------------------------------------

</div>

\
[]{#initialize_utilities}\

<div class="routine">

*call initialize\_utilities( *\[progname\]* *\[, alternatename\]* )*
    character(len=*), intent(in), optional :: progname
    character(len=*), intent(in), optional :: alternatename

</div>

<div class="indent1">

Reads the namelist and opens the logfile. Records the values of the
namelist and registers this module.

  ------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *progname  *        If given, use in the timestamp message in the log file to say which program is being started.
  *alternatename  *   If given, log filename to use instead of the value in the namelist. This permits, for example, different programs sharing the same input.nml file to have different logs. If not given here and no value is specified in the namelist, this defaults to dart\_log.out
  ------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#finalize_utilities}\

<div class="routine">

*call finalize\_utilities()*

</div>

<div class="indent1">

Closes the logfile; using utilities after this call is a bad idea.

</div>

\
[]{#dump_unit_attributes}\

<div class="routine">

*call dump\_unit\_attributes(iunit)*
    integer, intent(in) :: iunit

</div>

<div class="indent1">

Writes all information about the status of the IO unit to the error
handler with error level message.

  ----------- --------------------------------------------
  *iunit  *   Unit about which information is requested.
  ----------- --------------------------------------------

</div>

\
[]{#ERROR_LEVELS}\

<div class="routine">

    integer :: E_DBG, DEBUG
    integer :: E_MSG, MESSAGE
    integer :: E_WARN, WARNING
    integer :: E_ERR, FATAL

</div>

<div class="indent1">

  ------ -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *  *   Severity levels to be passed to error handler. Levels are debug, message, warning and fatal. The namelist parameter TERMLEVEL can be used to control at which level program termination should occur.
  ------ -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#logfileunit}\

<div class="routine">

    integer :: logfileunit

</div>

<div class="indent1">

  --------------- --------------------------------------------
  *logfileunit*   Unit opened to file for diagnostic output.
  --------------- --------------------------------------------

</div>

\
[]{#nmlfileunit}\

<div class="routine">

    integer :: nmlfileunit

</div>

<div class="indent1">

  --------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *nmlfileunit*   Unit opened to file for diagnostic output of namelist files. Defaults to same as *logfileunit*. Provides the flexibility to log namelists to a separate file, reducing the clutter in the log files and perhaps increasing readability.
  --------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   assim\_model\_mod.nml in input.nml
-   logfile, name specified in namelist

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
get\_unit
No available units
Unable to open enough IO channels
check\_nml\_error
while reading namelist \_\_\_\_\_
Fatal error reading namelist. This could be caused by having an entry in
the namelist input file that is not in the namelist, by having illegal
values for namelist variables, or by a variety of other compiler
dependent problems.
find\_namelist\_in\_file
Namelist entry &\_\_\_\_ must exist in namelist\_nml.
There must be an entry for the required namelist, for instance
&filter\_nml, in the input.nml namelist file. Even if no values are to
be changed from the default, an entry like &filter\_nml followed by a
line containing only / is required.
find\_namelist\_in\_file
Namelist input file: input.nml must exist
The namelist input file (usually input.nml) must exist.
check\_namelist\_read
INVALID NAMELIST ENTRY: \_\_\_ in namelist \_\_\_\_
While reading the namelist, either a bad entry was found or an end of
file was encountered. The most confusing case is when a namelist is
being read successfully but is not appropriately terminated with a /.
The line printed out by the error message will be the start of the next
namelist in the input.nml file in this case.

</div>

KNOWN BUGS
----------

none at this time

[]{#FuturePlans}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FUTURE PLANS
------------

none

[]{#PrivateComponents}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PRIVATE COMPONENTS
------------------

N/A

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


