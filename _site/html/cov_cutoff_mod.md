[]{#TOP}

MODULE cov\_cutoff\_mod
=======================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [FILES](#FilesUsed) / [INTERFACES](#Interface) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[PRIVATE COMPONENTS](#PrivateComponents) / [TERMS OF USE](#Legalese)

Overview
--------

Computes the weight with which an observation should impact a state
variable that is separated by a given distance. The distance is in units
determined by the location module being used.

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

    &cov_cutoff_nml
       select_localization = 1  
    /

</div>

\
\

<div>

+-----------------------+-----------------------+-----------------------+
| Item                  | Type                  | Description           |
+=======================+=======================+=======================+
| select\_localization  | integer               | Selects the           |
|                       |                       | localization          |
|                       |                       | function.             |
|                       |                       | -   1 = Gaspari-Cohn  |
|                       |                       |     5th order         |
|                       |                       |     polynomial with   |
|                       |                       |     halfwidth c.      |
|                       |                       | -   2 = Boxcar with   |
|                       |                       |     halfwidth c (goes |
|                       |                       |     to 0 for z\_in    |
|                       |                       |     &lt; 2c).         |
|                       |                       | -   3 = Ramped        |
|                       |                       |     Boxcar. Has value |
|                       |                       |     1 for z\_in &lt;  |
|                       |                       |     c and then        |
|                       |                       |     reduces linearly  |
|                       |                       |     to 0 at z\_in =   |
|                       |                       |     2c.               |
+-----------------------+-----------------------+-----------------------+

</div>

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
    location_mod

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

  -------------------------------- ---------------------------------------
  *use cov\_factor\_mod, only :*   [comp\_cov\_factor](#comp_cov_factor)
  -------------------------------- ---------------------------------------

A note about documentation style. Optional arguments are enclosed in
brackets *\[like this\]*.

[]{#comp_cov_factor}\

<div class="routine">

*var = comp\_cov\_factor(z\_in, c *\[, obs\_loc\] \[, obs\_type\]
\[, target\_loc\] \[, target\_kind\] \[, localization\_override\]*)*
    real(r8)                                  :: comp_cov_factor
    real(r8), intent(in)                      :: z_in
    real(r8), intent(in)                      :: c
    type(location_type), optional, intent(in) :: obs_loc
    integer, optional, intent(in)             :: obs_type
    type(location_type), optional, intent(in) :: target_loc
    integer, optional, intent(in)             :: target_kind
    integer, optional, intent(in)             :: localization_override

</div>

<div class="indent1">

Returns a weighting factor for observation and a target variable (state
or observation) separated by distance z\_in and with a half-width
distance, c. Three options are provided and controlled by a namelist
parameter. The optional argument localization\_override controls the
type of localization function if present. The optional arguments
obs\_loc, obs\_type and target\_loc, target\_kind are not used in the
default code. They are made available for users who may want to design
more sophisticated localization functions.

  -------------------------- -------------------------------------------------------------------------------------------
  *var*                      Weighting factor.
  *z\_in*                    The distance between observation and target.
  *c*                        Factor that describes localization function. Describes half-width of functions used here.
  *obs\_loc*                 Location of the observation.
  *obs\_type*                Observation specific type.
  *target\_loc*              Location of target.
  *target\_kind*             Generic kind of target.
  *localization\_override*   Controls localization type if present. Same values as for namelist control.
  -------------------------- -------------------------------------------------------------------------------------------

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
  ----------- ----------------------------
  input.nml   to read *cov\_cutoff\_nml*

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

1.  Gaspari and Cohn, 1999, QJRMS, **125**, 723-757. (eqn. 4.10)

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
comp\_cov\_factor
Illegal value of "select\_localization" in cov\_cutoff\_mod namelist
Only values 1 through 3 select a localization function.

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

  ----------------- -----------------------------
  Contact:          DART core group
  Revision:         \$Revision\$
  Source:           \$URL\$
  Change Date:      \$Date\$
  Change history:   try "svn log" or "svn diff"
  ----------------- -----------------------------


