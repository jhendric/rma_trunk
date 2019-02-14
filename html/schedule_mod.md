[]{#TOP}

MODULE schedule\_mod
====================

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

Provides a set of routines to generate a regular pattern of time
windows. This module is only used for converting observation sequences
files to netCDF format. If it stands the test of time, it will likely be
used to create an assimilation schedule independent of the observation
sequence file. Wouldn't that be nice ...

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

    &schedule_nml
       first_bin_start      =  1601,  1,  1,  0,  0,  0
       first_bin_end        =  2999,  1,  1,  0,  0,  0
       last_bin_end         =  2999,  1,  1,  0,  0,  0
       bin_interval_days    = 1000000
       bin_interval_seconds = 0
       max_num_bins         = 1000
       calendar             = 'Gregorian'
       print_table          = .true.
      /

</div>

\
\

Controls various aspects of filter. The inflation control variables are
all dimensioned 2, the first value being for the prior inflation and the
second being for the posterior inflation.

The default values will cause (pretty much) all possible observations to
be put into one output file.

<div>

  Item                     Type                    Description
  ------------------------ ----------------------- ----------------------------------------------------------------------------------------------------------------
  first\_bin\_start        integer, dimension(6)   Date/time specification for starting time of first bin.
  first\_bin\_end          integer, dimension(6)   Date/time specification for ending time of first bin. Sets the bin width.
  last\_bin\_end           integer, dimension(6)   Date/time specification for ending time of last bin. Sets the length of the overall time of the schedule.
  bin\_interval\_days      integer                 Sets the time between bins. Must be larger or equal to the bin width.
  bin\_interval\_seconds   integer                 Sets the time between bins. Must be larger or equal to the bin width.
  max\_num\_bins           integer                 Upper limit on the number of bins.
  calendar                 character(len=32)       String calendar type. Valid types are listed in the [time\_manager\_mod](time_manager_mod.html#cal_type) file.
  print\_table             logical                 If .TRUE., print out information about the schedule each time set\_regular\_schedule() is called.

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
    utilities_mod
    time_manager_mod

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

  ----------------------------- ------------------------------------------------------
  *use schedule\_mod, only :*   [schedule\_type](#schedule_type)
                                [set\_regular\_schedule](#set_regular_schedule)
                                [get\_time\_from\_schedule](#get_time_from_schedule)
                                [get\_schedule\_length](#get_schedule_length)
  ----------------------------- ------------------------------------------------------

Namelist [*&schedule\_mod\_nml*](#Namelist) may be read from file
*input.nml*.

[]{#set_regular_schedule}\

<div class="routine">

*call set\_regular\_schedule(schedule)*
    type(schedule_type), intent(out) :: schedule

</div>

<div class="indent1">

Uses the namelist information to compute and fill a schedule\_type
variable.

  ------------ ------------------------------------------------------------------------------------------------------------
  *schedule*   Fills this derived type with the information needed to generate a series of regularly spaced time windows.
  ------------ ------------------------------------------------------------------------------------------------------------

</div>

\
[]{#get_time_from_schedule}\

<div class="routine">

*call get\_time\_from\_schedule(mytime, schedule, iepoch *\[, edge\]*)*
    type(time_type),     intent(out) :: mytime
     or
    real(digits12),      intent(out) :: mytime
    type(schedule_type), intent(in)  :: schedule
    integer,             intent(in)  :: iepoch
    integer, optional,   intent(in)  :: edge

</div>

<div class="indent1">

Returns either the leading or trailing time for the specified bin/epoch
number for the given schedule. The time can be returned in one of two
formats, depending on the variable type specified for the first
argument: either a DART derived time\_type, or a real of kind digits12
(defined in the types\_mod).

  ------------ --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *mytime*     Return value with the leading or trailing edge time for the requested bin. There are two supported return formats, either as a standard DART time\_type, or as a real value which will contain the number of days plus any fraction.
  *schedule*   Schedule type to extract information from.
  *iepoch*     The bin number, or epoch number, to return a time for. Unless edge is specified and requests the ending time, the time returned is the starting time for this bin.
  *edge*       If specified, and if edge is larger than 1, the trailing edge time of the bin is returned. Any other value, or if this argument is not specified, returns the leading edge time of the bin.
  ------------ --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#get_schedule_length}\

<div class="routine">

*var = get\_schedule\_length()*
    integer                             :: get_schedule_length
    type(schedule_type), intent(in)     :: schedule

</div>

<div class="indent1">

Return the total number of intervals/bins/epochs defined by this
schedule.

  ------------ ---------------------------------------------------
  *schedule*   Return number of time intervals in this schedule.
  ------------ ---------------------------------------------------

</div>

\
[]{#schedule_type}\

<div class="type">

    type schedule_type
       private
       integer :: num_bins
       integer :: current_bin
       logical :: last_bin
       integer :: calendar
       character(len=32) :: calendarstring
       type(time_type)          :: binwidth
       type(time_type)          :: bininterval
       type(time_type), pointer :: binstart(   :) => NULL()
       type(time_type), pointer :: binend(     :) => NULL()
       real(digits12),  pointer :: epoch_start(:) => NULL()
       real(digits12),  pointer :: epoch_end(  :) => NULL()
    end type schedule_type

</div>

<div class="indent1">

This type is used to define a schedule.

</div>

\
[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

  filename    purpose
  ----------- ------------------------------------
  input.nml   to read the schedule\_mod namelist

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

Setting the schedule from the namelist values means you can only have a
single schedule object in the entire program. We also need a subroutine
to initialize a schedule type by giving explicit arguments.

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


