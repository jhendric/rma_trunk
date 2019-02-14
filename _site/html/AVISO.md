[]{#TOP}

Aviso+/CMEMS Observations
=========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[DATA SOURCES](#DataSources) / [PROGRAMS](#Programs) /
[NAMELIST](#Namelist) / [MODULES](#Modules) / [ERRORS](#Errors) /
[FUTURE PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
--------

### *convert\_aviso* was contributed by Frederic Castruccio - Thanks Fred!

This short description of the
*SEALEVEL\_GLO\_SLA\_L3\_REP\_OBSERVATIONS\_008\_018* product is
repeated from the **INFORMATION** tab from the [Copernicus Marine
Environment Monitoring
Service](http://marine.copernicus.eu/about-us/about-your-copernicus-marine-service/)
online catalogue (in April 2017).

> For the Global Ocean- Mono altimeter satellite along-track sea surface
> heights computed with respect to a twenty-year mean. Previously
> distributed by Aviso+, no change in the scientific content. All the
> missions are homogenized with respect to a reference mission which is
> currently Jason-2. This product is computed with an optimal and
> centered computation time window (6 weeks before and after the date).
> Two kinds of datasets are proposed: filtered (nominal dataset) and
> unfiltered.

The *convert\_aviso.f90* program is designed to read a
[netCDF](http://www.unidata.ucar.edu/software/netcdf) file containing
the (Level 3) sea surface anomalies from any of the following platforms:
"*Jason-1*", "*Envisat*", or "*Geosat Follow On*". One of those
platforms must be listed in the netCDF file global attribute:
*platform*\
\
The data files have names like:\
*dt\_global\_j1\_sla\_vfec\_20080101\_20140106.nc*,\
*dt\_global\_en\_sla\_vfec\_20080101\_20140106.nc*, or\
*dt\_global\_g2\_sla\_vfec\_20080101\_20140106.nc*; corresponding to the
*Jason-1*, *Envisat*, and the *Geosat Follow On* platforms.\
\
The DART observation TYPE corresponding to each of these platforms are
*J1\_SEA\_SURFACE\_ANOMALY*, *EN\_SEA\_SURFACE\_ANOMALY*, and
*GFO\_SEA\_SURFACE\_ANOMALY*, respectively and are defined in
[obs\_def\_ocean\_mod.f90](../../forward_operators/obs_def_ocean_mod.f90).\
\
Fred wrote a python script (*shell\_scripts/convert\_aviso.py*) to
repeatedly call *convert\_aviso* and decided it was easiest to simply
provide the input file name as a command line argument and always have
the output file have the name *obs\_seq.aviso*. As such, there is no
input namelist specifically for these parameters, but other DART modules
still require run-time crontrol specified by *input.nml*.

After creating a large number of output observation sequence files, it
is usually necessary to consolidate the files and subset them into files
containing just the timeframe required for a single assimilation.
**NOTE**: the *obs\_sequence\_tool* is constructed for just this
purpose.\
\
The *shell\_scripts/makedaily.sh* script attempts to consolidate all the
SLA observations and those that may have been (separately) converted
from the World Ocean Database into 24-hour segments centered at midnight
GMT. You will have to modify the *makedaily.sh* script to suit your
filesystem and naming convention. It is provided as a starting point.

**Reminder**: (according to the data providers): In order to compute
Absolute Dynamic Topography, the Mean Dynamic Topography (MDT) can be
added. It is distributed by Aviso+
(<http://www.aviso.altimetry.fr/en/data/products/auxiliary-products/mdt.html>
). Fred was using this product in assimilations with POP, so he chose a
different source for MDT - consistent with POP's behavior.

[]{#DataSources}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

DATA SOURCES
------------

The Copernicus Marine and Environment Monitoring Service (CMEMS) has
taken over the processing and distribution of the Ssalto/Duacs
multimission altimeter products formerly administered by Aviso+. After a
registration process, the along-track sea level anomalies (SLA) may be
downloaded from
[http://marine.copernicus.eu/services-portfolio/access-to-products/](http://marine.copernicus.eu/services-portfolio/access-to-products/?option=com_csw&view=details&product_id=SEALEVEL_GLO_SLA_L3_REP_OBSERVATIONS_008_018)
- search for the *SEALEVEL\_GLO\_SLA\_L3\_REP\_OBSERVATIONS\_008\_018*
if it does not come up directly.

[]{#Programs}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PROGRAMS
--------

  ------------------------------------ -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *convert\_aviso.f90*                 does the actual conversion from netCDF to a DART observation sequence file, which may be ASCII or binary.
  *shell\_scripts/convert\_aviso.py*   python script to convert a series of input files and datestamp the output files.
  *shell\_scripts/makedaily.sh*        shell script to repeatedly call *obs\_sequence\_tool* to consolidate multiple observation sequence files into an observation sequence file that has ALL the observations from ALL platforms in a single file. *makedaily.sh* is capable of looping over time ranges and creating observation sequences for each time range.
  ------------------------------------ -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

There is no namelist for *convert\_aviso*, but other namelists control
aspects of the execution, namely
*&obs\_sequence\_nml:write\_binary\_obs\_sequence*. see
[obs\_sequence\_mod.html](../../../assimilation_code/modules/observations/obs_sequence_mod.html).

[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    assimilation_code/location/threed_sphere/location_mod.f90
    assimilation_code/modules/assimilation/assim_model_mod.f90
    assimilation_code/modules/io/dart_time_io_mod.f90
    assimilation_code/modules/observations/obs_kind_mod.f90
    assimilation_code/modules/observations/obs_sequence_mod.f90
    assimilation_code/modules/utilities/ensemble_manager_mod.f90
    assimilation_code/modules/utilities/null_mpi_utilities_mod.f90
    assimilation_code/modules/utilities/random_seq_mod.f90
    assimilation_code/modules/utilities/sort_mod.f90
    assimilation_code/modules/utilities/time_manager_mod.f90
    assimilation_code/modules/utilities/types_mod.f90
    assimilation_code/modules/utilities/utilities_mod.f90
    models/template/model_mod.f90
    observations/forward_operators/obs_def_mod.f90
    observations/obs_converters/AVISO/convert_aviso.f90
    observations/obs_converters/utilities/obs_utilities_mod.f90

[]{#Errors} []{#KnownBugs}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

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
  Contact:           Tim Hoar
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


