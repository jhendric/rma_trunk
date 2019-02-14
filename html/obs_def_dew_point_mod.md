[]{#TOP}

MODULE obs\_def\_dew\_point\_mod
================================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../documentation/images/ | Index](../../documentation/index. |
| Dartboard7.png){height="70"}      | html)\                            |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[INTERFACES](#Interface) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

Overview
--------

Provides a subroutine to calculate the dew point temperature from model
temperature, specific humidity, and pressure.\
\
Revision 2801 implements a more robust method (based on Bolton's
Approximation) for calculating dew point. This has been further revised
to avoid a numerical instability that could lead to failed forward
operators for dewpoints almost exactly 0 C.

[]{#OtherModulesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    types_mod
    utilities_mod
    location_mod (most likely threed_sphere)
    assim_model_mod
    obs_kind_mod

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

  ----------------------------------------- ------------------------------------------------------
  *use obs\_def\_dew\_point\_mod, only :*   [get\_expected\_dew\_point](#get_expected_dew_point)
  ----------------------------------------- ------------------------------------------------------

A note about documentation style. Optional arguments are enclosed in
brackets *\[like this\]*.

[]{#get_expected_dew_point}\

<div class="routine">

*call get\_expected\_dew\_point(state\_vector, location, key, td,
istatus)*
    real(r8),            intent(in)  :: state_vector
    type(location_type), intent(in)  :: location
    integer,             intent(in)  :: key
    real(r8),            intent(out) :: td
    integer,             intent(out) :: istatus

</div>

<div class="indent1">

Calculates the dew point temperature (Kelvin).

  -------------------- ---------------------------------------------------------------------------
  *state\_vector   *   A one dimensional representation of the model state vector
  *location*           Location for this obs
  *key*                Controls whether upper levels (key = 1) or 2-meter (key = 2) is required.
  *td*                 The returned dew point temperature value
  *istatus*            Returned integer describing problems with applying forward operator
  -------------------- ---------------------------------------------------------------------------

</div>

\
[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   NONE

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

1.  Bolton, David, 1980: The Computation of Equivalent Potential
    Temperature. Monthly Weather Review, 108, 1046-1053.

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
get\_expected\_dew\_point
'key has to be 1 (upper levels) or 2 (2-meter), got ',key
The input value of key is not allowed.

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

none at this time

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


