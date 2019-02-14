[]{#TOP}

Netcdf Inflation Files
======================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../images/Dartboard7.png){h | Index](../index.html)\            |
| eight="70"}                       | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

The filter\_nml now read restart and inflation files directly from
NetCDF files

Netcdf inflation files are no longer special files. DART format
inflation files were always 2 copies in one file (mean and standard
devation). Taking away this special status of inflation files has the
advantage that all copies (restarts, ensemble mean, ensemble standard
deviation, inflation mean, inflation sd, etc.) can all be treated the
same for IO purposes. Since there are two inflation files when
reading/writing netcdf the filenames are different to DART format
restart files.

The names of the netcdf inflation files are now fixed.

**Input inflation file names**

The filter\_nml option:

`inf_in_file_name = prior_inflation_ics,   post_inflation_ics`

has been **deprecated** and for 1 domain filter is expecting to read:

input\_{priorinf,postinf}\_mean.nc\
input\_{priorinf,postinf}\_sd.nc

For multiple domains filter is expecting to read:

input\_{priorinf,postinf}\_mean\_d01.nc\
input\_{priorinf,postinf}\_sd\_d01.nc\
input\_{priorinf,postinf}\_mean\_d02.nc\
input\_{priorinf,postinf}\_sd\_d02.nc

where d0\* is the domain number.

**Output inflation file names**

The filter\_nml option:

`inf_out_file_name = prior_inflation_restart,    post_inflation_restart`

has been **deprecated** and for 1 domain filter is expecting to read:

output\_{priorinf,postinf}\_mean.nc\
output\_{priorinf,postinf}\_sd.nc

For multiple domains filter is expecting to write:

prior\_inflation\_restart\_mean\_d01\
prior\_inflation\_restart\_sd\_d01\
prior\_inflation\_restart\_mean\_d02\
prior\_inflation\_restart\_sd\_d02\

where d0\* is the domain number.

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


