[]{#TOP}

PROGRAM *COSMOS\_to\_obs*
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
[NAMELIST](#Namelist) / [REFERENCES](#References) / [ERRORS](#Errors) /
[PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
========

#### COSMOS "Level 2" Text File to DART Converter

[COSMOS](http://cosmos.hwr.arizona.edu/) is an NSF supported project to
measure soil moisture on the horizontal scale of hectometers and depths
of decimeters using cosmic-ray neutrons. The data for each station is
available from the COSMOS data portal with several levels of processing.
The metadata for each station (location, height, etc) is also available
from the data portal. The **Level 2 Data** is most suited for use with
DART.\
\
Since each site has a separate input data file, and the metadata for
each site must essentially be hand-input to the converter program, it is
generally easiest to convert the observations for each site separately
and then use the
[obs\_sequence\_tool.f90](../../../assimilation_code/programs/obs_sequence_tool/obs_sequence_tool.html)
to combine the observations from multiple sites and restrict the DART
observation sequence file to contain just the observations of the
timeframe of interest.\
\
FYI - in DART, the soil moisture profile is converted to expected
neutron counts using the **CO**smic-ray **S**oil **M**oisture
**I**nteraction **C**ode (COSMIC), developed at the University of
Arizona by Rafael Rosolem and Jim Shuttleworth.\
\
The workflow is usually:

1.  [get the site
    metadata](http://cosmos.hwr.arizona.edu/Probes/probemap.php) and
    enter it in the *input.nml* *&COSMOS\_to\_obs\_nml*
2.  download the Level 2 Data and prefix the filename with the station
    name (or else they all get named *corcounts.txt*) and enter the
    filename into *&COSMOS\_to\_obs\_nml*
3.  make sure the station soil parameters and COSMIC parameters are
    contained in the *observations/COSMOS/data/COSMIC\_parlist.nc* (more
    on this in [the section on COSMIC parameters](#COSMICparameters))
4.  run *COSMOS\_to\_obs* to generate a DART observation sequence file
    for the station and rename the output file if necessary (you can
    explicity name the output file via the namelist).
5.  repeat steps 1-4 for this converter to generate a DART observation
    sequence file for each station.
6.  use the
    [obs\_sequence\_tool.f90](../../../assimilation_code/programs/obs_sequence_tool/obs_sequence_tool.html)
    to combine the observations from multiple sites

[]{#DataSources}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

DATA SOURCES
------------

The COSMOS data portal can be found at:
<http://cosmos.hwr.arizona.edu/Probes/probemap.php> The data for each
station is available from the data portal with several levels of
processing. The metadata for each station (location, height, etc) is
also available from the data portal. The **Level 2 Data** is most suited
for use with DART. An example of the Level 2 Data follows:

    YYYY-MM-DD HH:MM  MOD PROBE PRESS  SCALE SANPE INTEN OTHER CORR ERR
    2009-10-23 18:34 5996 0.800 1.087 06.901 2.486 1.062 1.000 1768 022
    2009-10-23 19:34 5885 0.800 1.080 06.901 2.486 1.059 1.000 1729 022
    2009-10-23 20:34 6085 0.800 1.072 06.901 2.486 1.059 1.000 1774 022
    2009-10-23 21:34 6339 0.800 1.068 06.901 2.486 1.059 1.000 1843 023
    ...

[]{#Programs}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PROGRAMS
--------

The *COSMOS\_to\_obs.f90* file is the source code for the main converter
program. At present there is an uncomfortable assumption that the order
of the columns in the Level 2 data is fixed. I hope to relax that
requirement in the near future. *COSMOS\_to\_obs* reads each text line
into a character buffer and then reads from that buffer to parse up the
data items. The items are then combined with the COSMIC parameters for
that site and written to a DART-format observation sequence file. The
DART format allows for the additional COSMIC parameters to be contained
as metadata for each observation.

To compile and test, go into the *COSMOS/work* subdirectory and run the
*quickbuild.csh* script to build the converter and a couple of general
purpose utilities. The
[obs\_sequence\_tool](../../../assimilation_code/programs/obs_sequence_tool/obs_sequence_tool.html)
manipulates (i.e. combines, subsets) DART observation files once they
have been created. The default observations supported are those defined
in
[DART/observations/forward\_operators/obs\_def\_tower\_mod.f90](../../observations/forward_operators/obs_def_tower_mod.f90)
and
[DART/observations/forward\_operators/obs\_def\_COSMOS\_mod.f90](../../observations/forward_operators/obs_def_COSMOS_mod.f90).
If you need additional observation types, you will have to add the
appropriate *obs\_def\_XXX\_mod.f90* file to the
*input.nml* *&preprocess\_nml:input\_files* variable and run
*quickbuild.csh* again. It rebuilds the table of supported observation
types before compiling the source code.

[]{#COSMICparameters}

<div class="indent1">

### COSMIC parameters

Additional information is needed by DART to convert soil moisture
profiles to neutron counts. Each COSMOS instrument has site-specific
parameters describing soil properties etc. Those parameters have been
inserted into the observation file as metadata for each observation to
simplify the DART observation operator. It is a bit redundant as
currently implemented, but it is convenient.\
\
*COSMOS\_to\_obs* reads the site name from the input namelist and the
known station information from *COSMIC\_parlist.nc*. The simplest way to
add a new station to *COSMIC\_parlist.nc* is probably to:

1.  manually enter the information into the "data" section of
    *COSMIC\_parlist\_station.txt*
2.  then use *ncgen* to convert *COSMIC\_parlist\_station.txt* to a
    netCDF file.
3.  That netCDF file can be concatenated onto *COSMIC\_parlist.nc* with
    a simple *ncrcat* command.

Listing the sites already supported is easy:

<div class="unix">

    observations/COSMOS/data % ncdump -v sitenames COSMIC_parlist.nc
    netcdf COSMIC_parlist {
    dimensions:
            nsites = UNLIMITED ; // (42 currently)
            strlength = 21 ;
    variables:
            char sitenames(nsites, strlength) ;
                    sitenames:long_name = "COSMOS Site Names" ;
            double longitude(nsites) ;
                    longitude:long_name = "Longitude" ;
                    longitude:units = "degrees" ;
            double latitude(nsites) ;
                    latitude:long_name = "Latitude" ;
                    latitude:units = "degrees" ;
            double elevation(nsites) ;
                    elevation:long_name = "Elevation" ;
                    elevation:units = "m" ;
            double bd(nsites) ;
                    bd:long_name = "Dry Soil Bulk Density" ;
                    bd:units = "g cm{-3}" ;
            double lattwat(nsites) ;
                    lattwat:long_name = "Lattice Water Content" ;
                    lattwat:units = "m{3} m{-3}" ;
            double N(nsites) ;
                    N:long_name = "High Energy Neutron Intensity" ;
                    N:units = "relative counts" ;
            double alpha(nsites) ;
                    alpha:long_name = "Ratio of Fast Neutron Creation Factor (Soil to Water)" ;
                    alpha:units = "-" ;
            double L1(nsites) ;
                    L1:long_name = "High Energy Soil Attenuation Length" ;
                    L1:units = "g cm{-2}" ;
            double L2(nsites) ;
                    L2:long_name = "High Energy Water Attenuation Length" ;
                    L2:units = "g cm{-2}" ;
            double L3(nsites) ;
                    L3:long_name = "Fast Neutron Soil Attenuation Length" ;
                    L3:units = "g cm{-2}" ;
            double L4(nsites) ;
                    L4:long_name = "Fast Neutron Water Attenuation Length" ;
                    L4:units = "g cm{-2}" ;

    // global attributes:
                    :website = "COsmic-ray Soil Moisture Observing System (COSMOS) - 
                                http://cosmos.hwr.arizona.edu" ;
    data:

     sitenames =
      "ARM-1                ",
      "Austin_Cary          ",
      "Bondville            ",
      "Brookings            ",
      "Chestnut_Ridge_NOAA  ",
      "Coastal_Sage_UCI     ",
      "Daniel_Forest        ",
      "Desert_Chaparral_UCI ",
      "Fort_Peck            ",
      "Harvard_Forest       ",
      "Hauser_Farm_North    ",
      "Hauser_Farm_South    ",
      "Howland              ",
      "Iowa_Validation_Site ",
      "Island_Dairy         ",
      "JERC                 ",
      "Kendall              ",
      "KLEE                 ",
      "Manitou_Forest_Ground",
      "Metolius             ",
      "Morgan_Monroe        ",
      "Mozark               ",
      "Mpala_North          ",
      "Neb_Field_3          ",
      "P301                 ",
      "Park_Falls           ",
      "Pe-de-Gigante        ",
      "Rancho_No_Tengo      ",
      "Reynolds_Creek       ",
      "Rietholzbach         ",
      "Rosemount            ",
      "San_Pedro_2          ",
      "Santa_Rita_Creosote  ",
      "Savannah_River       ",
      "Silver_Sword         ",
      "SMAP-OK              ",
      "Soaproot             ",
      "Sterling             ",
      "Tonzi_Ranch          ",
      "UMBS                 ",
      "UVA                  ",
      "Wind_River           " ;
    }

</div>

The observation sequence files will look something like the following,
the attributes in yellow are the information from *COSMIC\_parlist.nc*:

<div class="unix">

     obs_sequence
    obs_kind_definitions
               1
              20 COSMOS_NEUTRON_INTENSITY
      num_copies:            1  num_qc:            1
      num_obs:         3840  max_num_obs:         3840
    observation
    COSMOS QC
      first:            1  last:         3840
     OBS            1
       1048.0000000000000
       1.0000000000000000
              -1           2          -1
    obdef
    loc3d
         4.154723123116714        0.7997185899100618         0.000000000000000     -1
    kind
              20
     cosmic
      0.88500000000000001       5.84099999999999966E-002   336.95696938999998       0.31918025877000000
       161.98621864285701        129.14558984999999        55.311849408000000        3.8086191933000002
               1
     77340     150034
       1225.0000000000000
       ...

</div>

</div>

 

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

    &COSMOS_to_obs_nml
       site_metadata_file = 'COSMIC_parlist.nc'
       text_input_file    = 'textdata.input',
       obs_out_file       = 'obs_seq.out',
       sitename           = 'missing',
       maxgoodqc          =  3,
       verbose            = .false.
       /

</div>

<div>

  Contents               Type                 Description
  ---------------------- -------------------- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  site\_metadata\_file   character(len=256)   The netCDF file containing the parameter values for each site.
  text\_input\_file      character(len=128)   The text file containing the raw observations for each site.
  obs\_out\_file         character(len=128)   The output observation sequence file for DART.
  sitename               character(len=128)   The name of the site. Must match one of the site names in the *site\_metadata\_file*. Case-insensitive match, trailing blanks ignored. Use *ncdump -v sitenames COSMIC\_parlist.nc*
  maxgoodqc              integer              left for future implementation.
  verbose                logical              A switch to specify the amount of run-time output. *.true.* the most amount of output. *.false.* the least amount of output.

### Example {#example .indent1}

    &COSMOS_to_obs_nml
       site_metadata_file = 'COSMIC_parlist.nc',
       text_input_file    = 'SantaRita_corcounts.txt',
       obs_out_file       = 'SantaRita_obs_seq.out',
       sitename           = 'Santa_Rita_Creosote',

</div>

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

References
----------

-   [The COSMOS web page.](http://cosmos.hwr.arizona.edu)
-   Franz, T.E, M. Zreda, T.P.A. Ferre, R. Rosolem, C. Zweck, S.
    Stillman, X. Zeng and W.J. Shuttleworth, 2012: Measurement depth of
    the cosmic-ray soil moisture probe affected by hydrogen from various
    sources. [ Water Resources Research]{style="font-style: italic;"}
    [48]{style="font-weight: bold;"}, W08515,
    [doi:10.1029/2012WR011871](http://dx.doi.org/10.1029/2012WR011871)
-   Franz, T.E, M. Zreda, R. Rosolem, T.P.A. Ferre, 2012: Field
    validation of cosmic-ray soil moisture probe using a distributed
    sensor network. [Vadose Zone Journal]{style="font-style: italic;"}
    (in press),
    [doi:10.2136/vzj2012.0046](http://dx.doi.org/10.2136/vzj2012.0046)

[]{#KnownBugs}

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

-   Implement a routine to automatically determine the column indices of
    the columns of interest.
-   Implement a QC encoding that reflects the uncertainty of the
    measurement. Presently, all Level 2 data have an incoming QC of 1.

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

