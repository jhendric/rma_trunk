[]{#TOP}

Bitwise Considerations
======================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../images/Dartboard7.png){h | Index](../index.html)\            |
| eight="70"}                       | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

By bitwise we mean bit for bit identical results in the output
obs\_sequence file and the restarts (netcdf-to-netcdf or DART
format-to-dart format) when comparing one version of the code to
another. For testing the code to be bitwise with Lanai there are several
things to change/set in Manhattan and your mkmf:

1.  assim\_tools\_mod.f90:\
    `lanai_bitwise = .true.` This is hard coded to false because it is
    very slow to be bitwise with Lanai. `lanai_bitwise = .true.` causes
    the vertical conversion of observations to be done in
    get\_close\_obs inside the sequential obs do loop. See [vertical
    conversion](vertical_conversion.html) for details about this change.
2.  filter\_nml:\
    `output_forward_op_errors = .true.` This will cause the forward
    operator code to calculate all forward operators, even if some
    ensemble members fail.
3.  mkmf:\
    ifort fp-model precise\
    In general use the debugging FLAGS in the mkmfs provided with DART.
4.  sampling\_error\_correction:\
    Use the sampling\_error\_correction\_table.Lanai.nc from the
    assimilation\_code/programs/system\_simulation/work directory. These
    values are identical to the Lanai release.
5.  posterior inflation:\
    Try to avoid testing cases which use posterior inflation. The
    posterior inflation has additional code in the Manhattan version
    compared to Lanai. If you need to test a case that has posterior,
    copy assim\_tools/assim\_tools\_mod.f90 from the 'classic' release
    to your Lanai build, and run your test against that version.

### Important

The *CAM* and *bgrid\_solo* model\_mods have been altered so the state
is in a different order inside filter. Thus DART format restarts will
**not** be bitwise with Lanai DART format restarts, but netcdf files
will be (after running dart\_to\_cam).

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


