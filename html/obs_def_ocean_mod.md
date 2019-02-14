[]{#TOP}

MODULE obs\_def\_ocean\_mod
===========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../documentation/images/ | Index](../../documentation/index. |
| Dartboard7.png){height="70"}      | html)\                            |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[INTERFACES](#Interface) / [NAMELIST](#Namelist) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[PRIVATE COMPONENTS](#PrivateComponents) / [TERMS OF USE](#Legalese)

Overview
--------

DART includes a flexible, powerful, and slightly complicated mechanism
for incorporating new types of observations. The *obs\_def\_ocean\_mod*
module being described here is used by the program *preprocess* to
insert appropriate definitions of ocean observations into the
*DEFAULT\_obs\_def\_mod.f90* template and generate the source files
*obs\_def\_mod.f90* and *obs\_kind\_mod.f90* that are used by *filter*
and other DART programs.\
\
There are no code segments in this module, only definitions of
observation types that map specific observation types to generic
observation quantities. DART contains logic that supports a limited
inheritance of attributes. If you need to interpolate observations of
'FLOAT\_TEMPERATURE', DART will check to see if a specific routine is
provided for that type, if none exists, the interpolation routine for
the generic 'QTY\_TEMPERATURE' will be used; that way one interpolation
routine may support many observation types.\
\
The mandatory header line is followed by lines that have the observation
type name (an all caps Fortran 90 identifier) and their associated
generic quantity identifier from the obs\_kind module. If there is no
special processing needed for an observation type, and no additional
data needed beyond the standard contents of an observation, then a third
word on the line, the *COMMON\_CODE* will instruct the preprocess
program to automatically generate all stubs and code needed for this
type. For observation types needing any special code or additional data,
this word should not be specified and the user must supply the code
manually. One of the future extensions of this module will be to support
acoustic tomographic observations, which will necessitate specific
support routines.

### ocean variable types and their corresponding quantities {#ocean-variable-types-and-their-corresponding-quantities .indent1}

``` {.indent1}
! BEGIN DART PREPROCESS KIND LIST
!SALINITY,                      QTY_SALINITY,              COMMON_CODE
!TEMPERATURE,                   QTY_TEMPERATURE,           COMMON_CODE
!U_CURRENT_COMPONENT,           QTY_U_CURRENT_COMPONENT,   COMMON_CODE
!V_CURRENT_COMPONENT,           QTY_V_CURRENT_COMPONENT,   COMMON_CODE
!SEA_SURFACE_HEIGHT,            QTY_SEA_SURFACE_HEIGHT,    COMMON_CODE
!ARGO_U_CURRENT_COMPONENT,      QTY_U_CURRENT_COMPONENT,   COMMON_CODE
!ARGO_V_CURRENT_COMPONENT,      QTY_V_CURRENT_COMPONENT,   COMMON_CODE
!ARGO_SALINITY,                 QTY_SALINITY,              COMMON_CODE
!ARGO_TEMPERATURE,              QTY_TEMPERATURE,           COMMON_CODE
!ADCP_U_CURRENT_COMPONENT,      QTY_U_CURRENT_COMPONENT,   COMMON_CODE
!ADCP_V_CURRENT_COMPONENT,      QTY_V_CURRENT_COMPONENT,   COMMON_CODE
!ADCP_SALINITY,                 QTY_SALINITY,              COMMON_CODE
!ADCP_TEMPERATURE,              QTY_TEMPERATURE,           COMMON_CODE
!FLOAT_SALINITY,                QTY_SALINITY,              COMMON_CODE
!FLOAT_TEMPERATURE,             QTY_TEMPERATURE,           COMMON_CODE
!DRIFTER_U_CURRENT_COMPONENT,   QTY_U_CURRENT_COMPONENT,   COMMON_CODE
!DRIFTER_V_CURRENT_COMPONENT,   QTY_V_CURRENT_COMPONENT,   COMMON_CODE
!DRIFTER_SALINITY,              QTY_SALINITY,              COMMON_CODE
!DRIFTER_TEMPERATURE,           QTY_TEMPERATURE,           COMMON_CODE
!GLIDER_U_CURRENT_COMPONENT,    QTY_U_CURRENT_COMPONENT,   COMMON_CODE
!GLIDER_V_CURRENT_COMPONENT,    QTY_V_CURRENT_COMPONENT,   COMMON_CODE
!GLIDER_SALINITY,               QTY_SALINITY,              COMMON_CODE
!GLIDER_TEMPERATURE,            QTY_TEMPERATURE,           COMMON_CODE
!MOORING_U_CURRENT_COMPONENT,   QTY_U_CURRENT_COMPONENT,   COMMON_CODE
!MOORING_V_CURRENT_COMPONENT,   QTY_V_CURRENT_COMPONENT,   COMMON_CODE
!MOORING_SALINITY,              QTY_SALINITY,              COMMON_CODE
!MOORING_TEMPERATURE,           QTY_TEMPERATURE,           COMMON_CODE
!SATELLITE_MICROWAVE_SST,       QTY_TEMPERATURE,           COMMON_CODE
!SATELLITE_INFRARED_SST,        QTY_TEMPERATURE,           COMMON_CODE
!SATELLITE_SSH,                 QTY_SEA_SURFACE_HEIGHT,    COMMON_CODE
!SATELLITE_SSS,                 QTY_SALINITY,              COMMON_CODE
! END DART PREPROCESS KIND LIST
```

New observation types may be added to this list with no loss of
generality. Supporting the observations and actually **assimilating**
them are somewhat different and is controlled by the
*input.nml&obs\_kind\_nml*
[assimilate\_these\_obs\_types](../../assimilation_code/modules/observations/obs_kind_mod.html#Namelist)
variable. This provides the flexibility to have an observation sequence
file containing many different observation types and being able to
selectively choose what types will be assimilated.

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

none

[]{#PublicEntities}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC COMPONENTS
-----------------

none

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

none

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

none

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

-   support acoustic tomography observations - similar to GPS soundings
    in the atmosphere. Take a look at
    [obs\_def\_gps\_mod.f90](obs_def_gps_mod.f90)

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
  Contact:           Tim Hoar
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


