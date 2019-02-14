[]{#TOP}

PROGRAM *PrecisionCheck*
========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../documentation/images/ | Index](../../documentation/index. |
| Dartboard7.png){height="70"}      | html)\                            |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[TERMS OF USE](#Legalese)

Overview
--------

This is a self-contained program to explore the interaction between the
compiler options to 'autopromote' variables from one precision to
another and the intrinsic F90 mechanism for getting consistent behavior
without relying on autopromotion - namely, the *SELECT\_INT\_KIND()* and
*SELECT\_REAL\_KIND()* functions. The most portable code explicity types
the variables to avoid relying on compiler flags. The core DART code
abides by these rules; some pieces that are derived from dynamical
models may have original code fragments.\
\
All that is required is to compile the single file and run the resulting
executable. There are no required libraries - any F90 compiler should
have no trouble with this program. There is no input of any kind.\
\
You are encouraged to view the source code. It's pretty obvious what is
being tested.

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

EXAMPLES
--------

The following examples have differences from the default configuration
highlighted in boldface. You are strongly encouraged to test your
compiler and its autopromotion options. The Absoft compiler actually
does what I consider to be reasonable and logical (as long as you know
that "-dp" means **d**emote **p**recision). Many other compilers are
surprising.

### PowerPC chipset : Absoft Pro Fortran 9.0 {#powerpc-chipset-absoft-pro-fortran-9.0 .indent}

<div class="unix">

    [~/DART/utilities] % f90 PrecisionCheck.f90
    [~/DART/utilities] % ./a.out
     
     This explores the use of the intrinisc SELECTED_[REAL,INT]_KIND() functions
     and the interplay with the compiler options. You are encouraged to use the
     "autopromotion" flags on your compiler and compare the results.
     
    ----------------------------------------------
     "integer"
     DIGITS    =   31
     HUGE      =   2147483647
     KIND      =   4
    ----------------------------------------------
     "integer(i4)" i4 = SELECTED_INT_KIND(8)
     DIGITS    =   31
     HUGE      =   2147483647
     KIND      =   4
    ----------------------------------------------
     "integer(i8)" i8 = SELECTED_INT_KIND(13)
     DIGITS    =   63
     HUGE      =   9223372036854775807
     KIND      =   8
    ----------------------------------------------
     "real"
     DIGITS    =   24
     EPSILON   =   1.192093E-07
     HUGE      =   3.402823E+38
     KIND      =   4
     PRECISION =   6
    ----------------------------------------------
     "real(r4)" r4 = SELECTED_REAL_KIND(6,30)
     DIGITS    =   24
     EPSILON   =   1.192093E-07
     HUGE      =   3.402823E+38
     KIND      =   4
     PRECISION =   6
    ----------------------------------------------
     "real(r8)" r8 = SELECTED_REAL_KIND(13)
     DIGITS    =   53
     EPSILON   =   2.220446049250313E-016
     HUGE      =   1.797693134862315E+308
     KIND      =   8
     PRECISION =   15
    ----------------------------------------------
     "double precision"
     DIGITS    =   53
     EPSILON   =   2.220446049250313E-016
     HUGE      =   1.797693134862315E+308
     KIND      =   8
     PRECISION =   15

</div>

### PowerPC chipset : Absoft Pro Fortran 9.0 : "-dp" {#powerpc-chipset-absoft-pro-fortran-9.0--dp .indent}

<div class="unix">

    [~/DART/utilities] % f90 -dp PrecisionCheck.f90
    [~/DART/utilities] % ./a.out
     
     This explores the use of the intrinisc SELECTED_[REAL,INT]_KIND() functions
     and the interplay with the compiler options. You are encouraged to use the
     "autopromotion" flags on your compiler and compare the results.
     
    ----------------------------------------------
     "integer"
     DIGITS    =   31
     HUGE      =   2147483647
     KIND      =   4
    ----------------------------------------------
     "integer(i4)" i4 = SELECTED_INT_KIND(8)
     DIGITS    =   31
     HUGE      =   2147483647
     KIND      =   4
    ----------------------------------------------
     "integer(i8)" i8 = SELECTED_INT_KIND(13)
     DIGITS    =   63
     HUGE      =   9223372036854775807
     KIND      =   8
    ----------------------------------------------
     "real"
     DIGITS    =   24
     EPSILON   =   1.192093E-07
     HUGE      =   3.402823E+38
     KIND      =   4
     PRECISION =   6
    ----------------------------------------------
     "real(r4)" r4 = SELECTED_REAL_KIND(6,30)
     DIGITS    =   24
     EPSILON   =   1.192093E-07
     HUGE      =   3.402823E+38
     KIND      =   4
     PRECISION =   6
    ----------------------------------------------
     "real(r8)" r8 = SELECTED_REAL_KIND(13)
     DIGITS    =   53
     EPSILON   =   2.220446049250313E-016
     HUGE      =   1.797693134862315E+308
     KIND      =   8
     PRECISION =   15
    ----------------------------------------------
     "double precision"
     DIGITS    =   24
     EPSILON   =   1.192093E-07
     HUGE      =   3.402823E+38
     KIND      =   4
     PRECISION =   6

</div>

### PowerPC chipset : Absoft Pro Fortran 9.0 : "-N113" {#powerpc-chipset-absoft-pro-fortran-9.0--n113 .indent}

<div class="unix">

    [~/DART/utilities] % f90 -N113 PrecisionCheck.f90
    [~/DART/utilities] % ./a.out
     
     This explores the use of the intrinisc SELECTED_[REAL,INT]_KIND() functions
     and the interplay with the compiler options. You are encouraged to use the
     "autopromotion" flags on your compiler and compare the results.
     
    ----------------------------------------------
     "integer"
     DIGITS    =   31
     HUGE      =   2147483647
     KIND      =   4
    ----------------------------------------------
     "integer(i4)" i4 = SELECTED_INT_KIND(8)
     DIGITS    =   31
     HUGE      =   2147483647
     KIND      =   4
    ----------------------------------------------
     "integer(i8)" i8 = SELECTED_INT_KIND(13)
     DIGITS    =   63
     HUGE      =   9223372036854775807
     KIND      =   8
    ----------------------------------------------
     "real"
     DIGITS    =   53
     EPSILON   =   2.220446049250313E-016
     HUGE      =   1.797693134862315E+308
     KIND      =   8
     PRECISION =   15
    ----------------------------------------------
     "real(r4)" r4 = SELECTED_REAL_KIND(6,30)
     DIGITS    =   24
     EPSILON   =   1.192093E-07
     HUGE      =   3.402823E+38
     KIND      =   4
     PRECISION =   6
    ----------------------------------------------
     "real(r8)" r8 = SELECTED_REAL_KIND(13)
     DIGITS    =   53
     EPSILON   =   2.220446049250313E-016
     HUGE      =   1.797693134862315E+308
     KIND      =   8
     PRECISION =   15
    ----------------------------------------------
     "double precision"
     DIGITS    =   53
     EPSILON   =   2.220446049250313E-016
     HUGE      =   1.797693134862315E+308
     KIND      =   8
     PRECISION =   15

</div>

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
  Contact:           Tim Hoar
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


