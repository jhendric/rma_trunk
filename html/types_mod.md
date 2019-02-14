[]{#TOP}

MODULE types\_mod
=================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[INTERFACES](#Interface) / [NAMELIST](#Namelist) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[PRIVATE COMPONENTS](#PrivateComponents) / [TERMS OF USE](#Legalese)

Overview
--------

Provides some commonly used mathematical constants, and a set of Fortran
integer and real kinds, to be used to select the right variable size
(e.g. 4 bytes, 8 bytes) to match the rest of the DART interfaces. (DART
does not depend on compiler flags to set precision, but explicitly
specifies a kind for each variable in the public interfaces.)

[]{#OtherModulesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    none

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

This routine provides the following constants, but no routines of any
kind.\
\
The constants defined here *may* or *may not* be declared the same as
constants used in non-DART pieces of code. It would seem like a good
idea to match the DART definition of 'gas\_constant' to the WRF
equivalent if you are going to be running WRF/DART experiments (for
example).

  -------------------------- ---------------------------------
  *use types\_mod, only :*   [i4](#kinds)
                             [i8](#kinds)
                             [r4](#kinds)
                             [r8](#kinds)
                             [c4](#kinds)
                             [c8](#kinds)
                             [digits12](#kinds)
                             [PI](#math)
                             [DEG2RAD](#math)
                             [RAD2DEG](#math)
                             [SECPERDAY](#math)
                             [MISSING\_R4](#no_data)
                             [MISSING\_R8](#no_data)
                             [MISSING\_I](#no_data)
                             [MISSING\_DATA](#no_data)
                             [metadatalength](#dart_lengths)
                             [obstypelength](#dart_lengths)
                             [t\_kelvin](#phys_const)
                             [es\_alpha](#phys_const)
                             [es\_beta](#phys_const)
                             [es\_gamma](#phys_const)
                             [gas\_constant\_v](#phys_const)
                             [gas\_constant](#phys_const)
                             [L\_over\_Rv](#phys_const)
                             [ps0](#phys_const)
                             [earth\_radius](#phys_const)
                             [gravity](#phys_const)
  -------------------------- ---------------------------------

[]{#kinds}\

<div class="type">

    integer, parameter :: i4
    integer, parameter :: i8
    integer, parameter :: r4
    integer, parameter :: r8
    integer, parameter :: c4
    integer, parameter :: c8
    integer, parameter :: digits12

</div>

<div class="indent1">

These kinds are used when declaring variables, like:

    real(r8)    :: myvariable
    integer(i4) :: shortint

All DART public interfaces use types on the real values to ensure they
are consistent across various compilers and compile-time options. The
digits12 is generally only used for reals which require extra
precision.\
\
Some models are able to run with single precision real values, which
saves both memory when executing and file space when writing and reading
restart files. To accomplish this, the users edit this file, redefine r8
to equal r4, and then rebuild all of DART.

</div>

\
[]{#math}\

<div class="type">

    real(KIND=R8), parameter :: PI
    real(KIND=R8), parameter :: DEG2RAD
    real(KIND=R8), parameter :: RAD2DEG
    real(KIND=R8), parameter :: SECPERDAY

</div>

<div class="indent1">

Some commonly used math constants, defined here for convenience.

</div>

\
[]{#no_data}\

<div class="type">

    real(KIND=R4), parameter :: MISSING_R4
    real(KIND=R8), parameter :: MISSING_R8
    integer,       parameter :: MISSING_I
    integer,       parameter :: MISSING_DATA

</div>

<div class="indent1">

Numeric constants used in the DART code when a numeric value is
required, but the data is invalid or missing. These are typically
defined as negative and a series of 8's, so they are distinctive when
scanning a list of values.

</div>

\
[]{#dart_lengths}\

<div class="type">

    integer, parameter :: metadatalength
    integer, parameter :: obstypelength

</div>

<div class="indent1">

Some common string limits used system-wide by DART code. The
obstypelength is limited by the Fortran-imposed maximum number of
characters in a parameter; the metadatalength was selected to be long
enough to allow descriptive names but short enough to keep printing to
less than a single line.

</div>

\
[]{#phys_const}\

<div class="type">

    real(KIND=R8), parameter :: t_kevin
    real(KIND=R8), parameter :: es_alpha
    real(KIND=R8), parameter :: es_beta
    real(KIND=R8), parameter :: es_gamma
    real(KIND=R8), parameter :: gas_constant_v
    real(KIND=R8), parameter :: gas_constant
    real(KIND=R8), parameter :: L_over_Rv
    real(KIND=R8), parameter :: ps0
    real(KIND=R8), parameter :: earth_radius
    real(KIND=R8), parameter :: gravity

</div>

<div class="indent1">

A set of geophysical constants, which could be argued do not belong in a
DART-supplied file since they are quite probably specific to a model or
a particular forward operator.\
\
Best case would be if we could engineer the code so these constants were
provided by the model and then used when compiling the forward operator
files. But given that Fortran use statements cannot be circular, this
poses a problem. Perhaps we could work out how the obs\_def code could
define these constants and then they could be used by the model code.
For now, they are defined here but it is up to the model and obs\_def
code writers whether to use these or not.

</div>

\
[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

There is no namelist for this module.

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

None.

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

1.  none

[]{#Errors}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

ERROR CODES and CONDITIONS
--------------------------

none

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

none at this time

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


