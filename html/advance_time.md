[]{#TOP}

PROGRAM *advance\_time*
=======================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[USAGE](#Usage) / [MODULES](#Modules) / [NAMELIST](#Namelist) /
[FILES](#FilesUsed) / [ERRORS](#Errors) / [PLANS](#FuturePlans) / [TERMS
OF USE](#Legalese)

Overview
--------

Provides a shell-scripting-friendly way to increment and decrement
calendar dates and times. The code uses the standard DART time manager
for all time calculations.\
\
A date, an increment or decrement, and an optional output formatting
flag are read from standard input. Increments can be days, hours,
minutes, or seconds. The accuracy is to the second. The resulting output
time string is echoed to standard output. For example:

    echo 2007073012 12 | advance_time

will output the string 2007073100. It uses the Gregorian calendar and
will roll over month and year boundaries, both going forward and
backwards in time. See the Usage section below for more examples of
use.\
\
The program is general purpose, but based on a time program distributed
with the WRF model. This is the reason there are a few WRF specific
options, for example the '-w' flag outputs a date string in a
WRF-specific format, useful for creating WRF filenames.\
\
The program does require that an 'input.nml' namelist file exist in the
current directory, and at least a &utilities\_nml namelist (which can be
empty) exists.

[]{#Usage}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

USAGE
-----

Interface identical to *advance\_cymdh*, except for reading the arg line
from standard input, to be more portable since iargc() is nonstandard
across different fortran implementations.

-   default numeric increment is hours
-   has accuracy down to second
-   can use day/hour/minute/second (with/without +/- sign) to advance
    time
-   can digest various input date format if it still has the right order
    (ie. cc yy mm dd hh nn ss)
-   can digest flexible time increment
-   can output in wrf date format (ccyy-mm-dd\_hh:nn:ss)
-   can specify output date format
-   can output Julian day
-   can output Gregorian days and seconds (since year 1601)

Some examples:

    advance 12 h:
      echo 20070730      12         | advance_time    

    back 1 day 2 hours 30 minutes and 30 seconds:
      echo 2007073012   -1d2h30m30s | advance_time    

    back 3 hours 30 minutes less 1 second:
      echo 2007073012    1s-3h30m   | advance_time    

    advance 2 days and 1 second, output in wrf date format :
      echo 200707301200  2d1s -w    | advance_time    
      echo 2007-07-30_12:00:00 2d1s -w  | advance_time  
      echo 200707301200  2d1s -f ccyy-mm-dd_hh:nn:ss | advance_time 

    advance 120 h, and print year and Julian day:
      echo 2007073006    120 -j     | advance_time    

    advance 120 h, print year, Julian day, hour, minute and second:
      echo 2007073006    120 -J     | advance_time    

    print Gregorian day and second (since year 1601):
      echo 2007073006    0 -g       | advance_time    

[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    utilities_mod
    time_manager_mod
    parse_args_mod

[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

No namelist is currently defined for *advance\_time*.

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   input.nml

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

