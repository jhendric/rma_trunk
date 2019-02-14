[]{#TOP}

system simulation programs
==========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [MODULES](#Modules) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

Overview
--------

A collection of standalone programs for simulating various properties of
ensembles.

-   *gen\_sampling\_err\_table.f90*
-   *full\_error.f90*
-   *obs\_sampling\_err.f90*
-   *sampling\_error.f90*
-   *system\_simulation.f90*
-   *test\_sampling\_err\_table.f90*
-   *correl\_error.f90*

**The program of most interest here is *gen\_sampling\_err\_table.f90*
which generates the lookup table needed when using sampling error
correction in *filter*.** Talk to Jeff Anderson about the other programs
in this directory.

To enable the sampling error correction algorithm in *filter*, set the
namelist item
[&assim\_tools\_nml : sampling\_error\_correction](../../modules/assimilation/assim_tools_mod.html#Namelist)
to *.true.*, and copy the netCDF file
[system\_simulation/sampling\_error\_correction\_table.nc into the run
directory.\
\
The supported set of precomputed ensemble sizes can be found by
exploring the *ens\_sizes* variable
in]{.file}[sampling\_error\_correction\_table.nc. To add support for
another ensemble size, build the executables in
the]{.file}[work](../system_simulation/work) directory, (usually by
running *quickbuild.csh*) set the *ens\_sizes* (it takes a list, but
keep it short) namelist item in *work/input.nml*, and run
*gen\_sampling\_err\_table*. It generates a LARGE number of samples *per
ensemble size* for statistical rigor. Larger ensemble sizes take longer
to generate, and compiler optimizations matter - perhaps significantly.
For example, the numbers below come from calculating one ensemble size
at a time on my desktop machine with gfortran and basic optimization:

  ensemble size   run-time (seconds)
  --------------- --------------------
  10              57
  50              273
  100             548

The basic structure of sampling\_error\_correction\_table.nc is shown
below.

<div>

    0[1095] desktop:system_simulation/work % ncdump -v ens_sizes *nc
    netcdf sampling_error_correction_table {
    dimensions:
            bins = 200 ;
            ens_sizes = UNLIMITED ; // (40 currently)
    variables:
            int count(ens_sizes, bins) ;
                    count:description = "number of samples in each bin" ;
            double true_corr_mean(ens_sizes, bins) ;
            double alpha(ens_sizes, bins) ;
                    alpha:description = "sampling error correction factors" ;
            int ens_sizes(ens_sizes) ;
                    ens_sizes:description = "ensemble size used for calculation" ;

    // global attributes:
                    :num_samples = 100000000 ;
                    :title = "Sampling Error Corrections for fixed ensemble sizes." ;
                    :reference = "Anderson, J., 2012: Localization and Sampling Error 
                                  Correction in Ensemble Kalman Filter Data Assimilation.
                                  Mon. Wea. Rev., 140, 2359-2371, doi: 10.1175/MWR-D-11-00013.1." ;
                    :version = "$Id$" ;
    data:

    These ensemble sizes are already supported!
     ens_sizes = 5,  6,  7,  8,  9, 10, 12, 14, 15, 16, 18, 20, 22, 24, 28, 30, 32, 36, 40, 44,
                48, 49, 50, 52, 56, 60, 64, 70, 72, 80, 84, 88, 90, 96, 100, 120, 140, 160, 180, 200
    }

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

    &gen_sampling_error_table_nml
       ens_sizes = 5,  6,  7,  8,  9, 10, 12, 14, 15, 16, 18, 20, 22, 24, 28, 30, 32, 36, 40, 44,
                  48, 49, 50, 52, 56, 60, 64, 70, 72, 80, 84, 88, 90, 96, 100, 120, 140, 160, 180, 200
       debug = .false.
       /

</div>

\
\

  Item         Type           Description
  ------------ -------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ens\_sizes   integer(200)   An array of ensemble sizes to compute. Any new size gets appended to the variables in the netCDF file. Any order is fine, the array does not have to be monotonic. The numbers listed in the example exist in the file distributed with DART. *Do not get carried away by generating a lot of new ensemble sizes in one execution.* The table of run-time above should give you some indication of how long it takes to create a new entry.
  debug        logical        A switch to add some run-time output. Generally not needed.

[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    types_mod
    utilities_mod
    random_seq_mod

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES for *gen\_sampling\_err\_table*
-------------------------------------

-   *input.nml* for the run-time input
-   *sampling\_error\_correction\_table.nc* is both read and written.
    Any new ensemble sizes are simply appended to the file.
-   *dart\_log.out* has the run-time output.

FILES for *full\_error*
-----------------------

-   *input.nml* for the run-time input
-   *final\_full.N* are created - N is the ensemble size.
-   *dart\_log.out* has the run-time output.

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

-   **Anderson, J. L.**, 2012: Localization and Sampling Error
    Correction in Ensemble Kalman Filter Data Assimilation. *Mon. Wea.
    Rev.*, **140**, 2359-2371 [doi:
    10.1175/MWR-D-11-00013.1](http://dx.doi.org/doi:10.1175/MWR-D-11-00013.1)

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
*gen\_sampling\_err\_table*
duplicate ensemble size found
no need to recompute an alpha for an ensemble size already supported.
*gen\_sampling\_err\_table*
existing file used a different bin size
The code has been modified to use a different number of bins than the
existing netCDF file. If that's what you intend, you need to make a new
file.
*gen\_sampling\_err\_table*
existing file uses *N* samples, the program has *Y* samples.
The code has been modified to use a different number of replicates used
to estimate the *alphas*. If that's what you intend, you need to make a
new file.
*full\_error*
cannot handle task counts &gt; 99999
Ensemble size must be less than 100,000.
*full\_error*
empty bin
The sample size must be large enough for all bins to have counts

</div>

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

none at this time.

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
  Contact:           Jeff Anderson
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


