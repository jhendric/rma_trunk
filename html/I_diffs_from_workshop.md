[]{#TOP}

DART Iceland revisions
======================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../images/Dartboard7.png | Index](../../index.html)\         |
| ){height="70"}                    | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

The DART Iceland release (23 Nov 2005) is expected to be the last update
that makes major modifications to the user interfaces to DART and the
DART interfaces to models or observations. The primary purpose of this
release is to improve the way that DART handles observation sequences.
The goal is to provide a more modular fashion for adding new types of
observations with potentially complex forward operators and arbitrary
amounts and types of metadata that are needed to define a particular
observation. Limited backward compatibility has been implemented for
existing observation sequence files, however, it is strongly recommended
that users switch to the new format as quickly as possible.

Improvements
------------

The changes to the observation sequences impact large portions of the
DART code. In addition, Iceland also implements a number of other
improvements and additions to DART and fixes several known bugs from
earlier releases. Highlights of the changes from the workshop and Hawaii
releases are:

1.  Namelist error detection.\
    Enhanced error checking capabilities for DART namelist reads are
    included. The input.nml must now contain an entry for each namelist
    that is read by a program, even if none of the values in that
    namelist entry are set. For instance, if a program that uses the
    assim\_tools\_mod is run, input.nml MUST contain an entry
    &assim\_tools\_nml. Any namelist values to be set must follow this
    and the list is terminated by a /. If no entries are to be set in a
    given namelist, it must still be terminated by a line that contains
    only a slash (/). If a variable that is NOT in a given namelist is
    found in input.nml, a fatal error occurs. Failing to terminate a
    namelist with slash is also fatal. Tools to support this improved
    namelist error detection are implemented in utilities\_mod and can
    be used to check for errors in other applications.
2.  Automatic detection of input file format.\
    Restart and observation sequence files can be written in binary or
    ascii as in previous releases. However, file formats for reads are
    now detected automatically. The namelist entries
    'read\_binary\_restart\_files' in assim\_model\_nml and
    'read\_binary\_obs\_sequence' in obs\_sequence\_nml have been
    removed.
3.  Error corrected in threed\_sphere/location\_mod.\
    An error in the Hawaii and Workshop releases led to the possibility
    of erroneous computations of distance in the
    threed\_sphere/location\_mod. The error only occurred if the
    namelist entry 'approximate\_distance' was set to .true. (the
    default was .false.) and errors were generally small and confined to
    observations located in polar regions. The result could be that
    localization was not applied properly to polar observations.
4.  Support for reduced precision real computation.\
    For some large model applications, using reduced precision reals can
    enhance performance. The definition of 'real(r8)' in types\_mod can
    now be modified to support different real precision. Some
    computations in the time\_manager can fail with reduced precision,
    so all time computations now use an independent fixed precision.
5.  Quality control definition change.\
    values written to quality control (qc) fields by perfect\_model\_obs
    and filter have been modified. If a prior (posterior) forward
    operator computation fails, qc for that observation is incremented
    by 1000 (1000000). If the prior (posterior) observation lies outside
    the outlier\_threshold, the qc for that observation is incremented
    by 100 (400).
6.  Added obs\_sequence file header.\
    Observation sequence files (obs\_sequence files) now include a
    metadata header that includes a list of the names of each
    observation type that might be used in the file along with a mapping
    to an integer for each name in the file. Old format files without a
    header are automatically detected and a default mapping of integer
    indices for observation types to observation type names is used.
    This default mapping works for most low-order model, bgrid and CAM
    obs\_sequence files. It is strongly recommended that legacy
    obs\_sequence files be converted to the new format by inserting a
    header block with the appropriate names and integer mappings.
7.  Interactive creation input for obs\_sequence files.\
    A standard method for creating obs\_sequence files is to redirect a
    text file to standard input and use the interactive creation
    facility in create\_obs\_sequence. Old input files for this
    procedure may no longer work correctly and may need to be updated.
    The new interactive creation interface allows the name of
    observations to be used, or an integer index. However, the integer
    index is now defined by a table in the obs\_kind\_mod and may change
    dynamically. Users should understand this procedure.
8.  obs\_def\_nml moved to obs\_kind\_nml.\
    The obs\_def\_nml in previous releases had entries to choose what
    observation types to assimilate or evaluate. These entries have been
    moved to obs\_kind\_nml and obs\_def\_nml no longer exists.
9.  Preprocessor functionality has changed.\
    While the preprocess program still needs to be run when setting up
    DART experiments, it now works differently. Previously, the
    preprocessor read obs\_def\_mod.F90 and created obs\_def\_mod.f90 in
    a fashion very similar to the standard CPP preprocessor. The new
    preprocessor does not look like CPP. It reads input from
    DEFAULT\_obs\_kind\_mod.F90, DEFAULT\_obs\_def\_mod.F90, and from
    any requested special obs\_def modules and creates an
    obs\_kind\_mod.f90 and an obs\_def\_mod.f90. Documentation of this
    new functionality is available in tutorial section 21 and in the
    html files for preprocess, obs\_kind\_mod, and obs\_def\_mod.
10. Improved observation space diagnostics interfaces.\
    The observation space diagnostics program and associated diagnostic
    tools are now all located in the diagnostics directory. The
    interfaces and controls have been modified and are described in html
    documentation and in the tutorial section 18. Consistent interfaces
    for low-order models and large three-dimensional atmospheric models
    have been provided.

Changes
-------

A summary list of changes occurring in each DART directory follows:

  ------------------------------------- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  assim\_model                          Uses digits12 for netcdf time computations allowing reduced precision models; automatic detection of restart file input format, read\_binary\_restart removed from namelist, namelist error checking.
  assim\_tools                          Namelist error checking added. Order in which computation of ensemble prior and observation to state distance are computed has been changed in attempt to increase efficiency (should have only round-off level impact on results). Added ability to print out regional regression coefficients when using the parallel implementation.
  common: types\_mod.f90                Added digits12 real precision kind for doing times with required precision when rest of assimilation is done with reduced real precision, added gravity to constants list.
  converters                            These WRF specific routines have been significantly modified to use the updated release of WRF. See the files and the WRF specific documentation for details.
  cov\_cutoff                           added optional argument localization\_override to comp\_cov\_factor to override the namelist selection of the localization\_type for a single call. Changed to more computationally efficient method for computing value of Gaspari Cohn function. Namelist error checking.
  diagnostics                           Observation space diagnostics are now included in this directory. There are directories for oned models and threed\_sphere models, each contains an obs\_diag.f90 program along with its namelist and documentation. The matlab directory contains matlab scripts for plotting the results of observation space diagnostics.
  ensemble\_manager                     Includes commented block needed to write out ensemble mean for WRF boundary forcing computations. Namelist error checking.
  filter                                Incorporated new namelist error checking, modified calls to read\_obs\_seq\_header to support automatic file format detection, changed to new qc values (see summary above). Namelist error checking.
  integrate\_model                      Namelist error checking.
  location/threed\_sphere               Added 5 VERTIS\*\*\*\* variables for describing vertical location kinds. Corrected error in table lookup for approximate computation of cos and sin by doubling range of lookup table. Added public logical functions vert\_is\_undef and vert\_is\_surface. Improved menu for interactive definition of locations. Namelist error checking.
  matlab                                Minor modifications to several scripts.
  mkmf                                  Templates cleaned up and templates for additional platforms added.
  models                                All with namelists have namelist error detection.
  models/bgrid\_solo                    Use new generic kind definitions to decide how to interpolate observations.
  models/lorenz\_04                     Added nc\_read\_model\_vars to read in netcdf file format.
  ncep\_obs                             The code from NCEP to read bufr files has been added to the directory. This is not technically part of DART but is required as a first phase for BUFR file translation. Program create\_real\_obs has been generalized to read in portions of days if required and to use the new obs\_kind and obs\_def modules and the obs\_def\_reanalysis\_bufr\_mod.f90 to include much more detailed descriptions of the types of observations. The obs\_diag programs have been moved to the diagnostics directory. The matlab diagnostic routines have also been moved to the diagnostics directory and generalized.
  DEFAULT\_obs\_def\_mod.F90            Replaces obs\_def\_mod.f90, preprocessed to create obs\_def\_mod.f90. No longer has a namelist (previous namelist moved to obs\_kind\_nml). Function get\_obs\_name returns the name string for an observation kind given the integer kind index. Routine set\_obs\_def\_key added to set the value of the integer key associated with an obs\_def\_type. Provides default mapping for obs\_sequence files in the old format that do not have a header table mapping indices to obs\_kind strings.
  obs\_def\_dew\_point\_mod.f90         New module for doing dew point forward operators.
  obs\_def\_metar\_mod.f90              New module for doing surface observation forward operators.
  obs\_def\_radar\_mod.f90              Revised version of radar forward operator module that works with DEFAULT\_obs\_def\_mod.F90.
  obs\_def\_1d\_state\_mod.f90          Computes forward operators for interpolation and integrals of low-order models with a single state variable type on a cyclic domain.
  obs\_def\_reanalysis\_bufr\_mod.f90   Computes forward operators for all types of observations available in the reanalysis BUFR files.
  DEFAULT\_obs\_kind\_mod.F90           Replaces obs\_kind\_mod.f90, preprocessed to create obs\_kind\_mod.f90. Includes new 'generic' kind definitions list with associated integers. Each observation kind must be associated with one of these generic kinds. Now has namelist to define what observation kinds are being assimilated or evaluated plus new namelist error checking. Provides new interfaces to get information about obs\_kind: get\_obs\_kind\_name returns the observation name string given a kind index; get\_obs\_kind\_index does the inverse, assimilate\_this\_obs\_kind and evaluate\_this\_obs\_kind return true if this observation index is one that is to be used in this way; get\_obs\_kind\_var\_type returns the generic kind associated with an observation type, get\_kind\_from\_menu offers interactive creation capability.
  obs\_sequence\_mod.f90                obs\_sequence files now have a header that maps from obs\_kind indices to a string that uniquely identifies the observation kind. Automatic detection of obs\_sequence file formats and old format without header. Automatic namelist error detection and removal of read\_binary\_obs\_sequence from namelist. Removal of code for WRF radar observations.
  perfect\_model\_obs.f90               Uses revised calls to read\_obs\_seq\_header and read\_obs\_seq to use automatic file format detection. Automatic namelist error detection.
  preprocess.f90                        Now preprocesses the DEFAULT\_obs\_kind\_mod.F90 and DEFAULT\_obs\_def\_mod.F90 and inputs information from obs\_def\_???\_mod.f90 files such as obs\_def\_reanalysis\_bufr\_mod. Looks for fixed format text strings in the input files to determine what sections of code to extract and where to insert them in the DEFAULT files. Namelist includes the names of the two input DEFAULT files, the names of the output preprocessed files (normally obs\_def\_mod.f90 and obs\_kind\_mod.f90 in the appropriate directories) and a list of all the obs\_def\_???\_mod.f90 files that are to be incorporated.
  reg\_factor\_mod.f90                  Automatic namelist error detection.
  shell\_scripts                        Several new scripts for managing files and cleaning up DART directories have been added. Significant modifications have been made to the platform specific scripts advance\_ens, assim\_filter, and filter\_server. Versions for additional platforms have been added.
  time\_manager\_mod.f90                Use of digits12 precision for real computations allows reduced precision to be used for rest of dart. Optional error return added to read\_time to support automatic file detection for dart state vector files.
  tutorial                              The workshop tutorial scripts have been updated to correct several errors and to be consistent with the preprocessing changes. Section 21 has been added to describe the new obs\_sequence implementation.
  utilities\_mod.f90                    Namelist error detection added.
  ------------------------------------- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

[]{#FutureWork}

Future Enhancements / Work
--------------------------

-   Extend PBL\_1d support for all matlab scripts.\
    currently only supported by the observation-space diagnostics and a
    crude implementation for 'plot\_total\_err'.
-   Unify the machine-specific scripts to handle PBS, LSF and
    interactive submission in one script.
-   Incorporate support for 'null\_model'.\
    A useful exercise to test many facets of the DART code without a
    chaotic model. Should provide capability to perform regression
    testing of DART infrasturcture.
-   Improve netcdf error messages.\
    Will incorporate an additional argument to the 'check' routine to
    append a string to the netCDF error library string.

[]{#Legalese}

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


