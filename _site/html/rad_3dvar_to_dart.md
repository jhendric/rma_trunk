[]{#TOP}

PROGRAM *rad\_3dvar\_to\_dart*
==============================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [MODULES](#Modules) / [ERRORS](#Errors) /
[FUTURE PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
--------

Programs to convert MM5 3D-VAR 2.0 Radar data files into DART
observation sequence files. The capability of the program is limited to
DOPPLER\_RADIAL\_VELOCITY and RADAR\_REFLECTIVITY.

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

    &rad_3dvar_to_dart_nml
       var_file = 'qc_radr_3dvar_2002083100.dat',
       obs_seq_out_file_name = 'obs_seq.out',
       calendar_type = 3  
    /

</div>

\
\

<div>

  Item                        Type                 Description
  --------------------------- -------------------- ----------------------------------------------------------------------------
  var\_file                   character(len=129)   This is the name of the file containing MM5 3D-VAR 2.0 Radar observations.
  obs\_seq\_out\_file\_name   character(len=129)   File name for output observation sequence file.
  calendar\_type              integer              Calendar type. We recommend using 3 (GREGORIAN).

</div>

\
\
[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES DIRECTLY USED
---------------------

    types_mod
    obs_sequence_mod
    obs_def_mod
    obs_def/obs_def_radar_mod
    obs_kind_mod
    location/threed_sphere/location_mod
    time_manager_mod
    utilities_mod

MODULES INDIRECTLY USED
-----------------------

    assim_model_mod
    models/wrf/model_mod
    models/wrf/module_map_utils
    random_seq_mod

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   input namelist ; *input.nml*
-   Input observation file; *qc\_radr\_3dvar\_2002083100.dat*
-   Output observation file; *obs\_seq.out*

### File formats

*input.nml* and *qc\_radr\_3dvar\_2002083100.dat* are ASCII files.
*obs\_seq.out* is either ASCII or binary, depending on the logical
write\_binary\_obs\_sequence, which is the namelist entry for
obs\_sequence\_mod.

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

-   [3DVAR GROUP PAGE](http://www.mmm.ucar.edu/wrf/WG4/)

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

none

[]{#FuturePlans}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FUTURE PLANS
------------

1.  Use the preprocessor to include the observation list provided by
    obs\_kind\_mod.
2.  Add the capability to convert from DART to MM5 3D-VAR 2.0 Radar data
    file format.

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


