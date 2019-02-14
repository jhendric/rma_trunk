[]{#TOP}

DART POST-Iceland revisions
===========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../images/Dartboard7.png | Index](../../index.html)\         |
| ){height="70"}                    | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

DART "POST Iceland release" summary of changes.
===============================================

\$Id\$

The DART POST Iceland release (16 June 2006) is an intermediate release
designed to provide error fixes required for a number of users and to
provide enhanced support for the CAM, WRF, and PBL\_1D models. It also
provides several new assimilation algorithm options that are already
widely used by CAM implementations. The most important are additional
methods for doing adaptive inflation. The previously existing
observation space inflation is retained and two new options for adaptive
state space inflation are introduced. One option has a value of
inflation that is constant across all state variables but varies in
time. The second has an inflation factor for EACH state variable and
these can each vary in time. An initial version of an algorithm to do
adaptive localization is also introduced. The local density of
observations is measured by computing the number of observations from a
set at a given time that is close to the observation being assimilated.
If the density is such that more than a given threshold number of
observations is close to the current observation, the localization
radius is adjusted so that the expected number of observations in the
adjusted radius is equal to the threshold value. This will decrease the
localization radius in areas with dense observations.\
\
Another important new capability is provided by a *merge\_obs\_sequence*
program that can combine most existing observation sequences into a
single sequence.\
\
The *shell scripts* used to coordinate the execution of the model
advances or regional assimilation have been modified to be more robust
and easier to read. Furthermore, each script has been standardized to be
much more machine/queueing system independent. Each script should now be
able to be used interactively (in a debug scenario), with the LSF
queueing system (as a batch job), or with the PBS queueing system. The
scripts have a block of logic that converts the system-specific
variables to ones commonly used throughout the DART documentation. This
has now made it possible to prepare a script that will conditionally
execute a series of batch jobs wherein each batch job will process
exactly one observation sequence file. Only upon successful completion
of the previous batch job with the subsequent batch job be started. Look
for information on *Qfiller.csh* on the DART www page.\
\
*merge\_obs\_sequence* program that can combine most existing
observation sequences into a single sequence.\
\
An initial version of an ensemble smoother was added to DART and
implemented with the low-order models and the bgrid model. The smoother
continues to use the OLD inflation options and an old version of the
*assim\_tools\_mod* module.

Changes
-------

A summary list of changes occurring in each DART directory/file follows:

  ---------------------------------------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *test\_dart.csh*                                     This testing script in the main DART directory has been updated to test the new inflation options with the new scripts. An attempt has been made to preserve the input environment such that if you wanted to run it twice in a row, you could. It now stores all the run-time output of the lorenz\_96 tests in the *lorenz\_96/work/xxxx* directory, where xxx is now an argument to *test\_dart.csh*. Simply typing the file name will now echo usage notes.
  *adaptive\_inflate.f90*                              This new module now handles all the computations for the adaptive inflation computations. It has the code that was previously in *assim\_tools* to do the Bayesian update of observation space inflation. It also provides the additional algorithms required to do state space inflation. Documentation is provided in the module and in several papers on the DART home page.
  *assim\_tools\_mod.f90*                              Uses *adaptive\_inflation* module. Namelist modified (see below). Storage for observation space inflation for regions no longer needed. No longer an *assim\_restart* file; all restart info is now stored in *adaptive\_inflate*. Routine *update\_inflation* replaces *bayes\_cov\_inflate* which has been moved to *adaptive\_inflate\_mod*. Correlation is now passed out of *update\_from\_obs\_inc* for use with adaptive inflation updates. Ability to do sampling\_error\_correction from a file using *correl\_error.f90* in *system\_simulation*. This is turned on by namelist parameter *sampling\_error\_correction* and requires appropriate error correction files for a given ensemble size (this is still in preliminary testing phase). The inflation values are now passed into *filter\_assim\_region*. The mean and variance of the observation space priors are computed up front in *filter\_assim\_region* for use with adaptive inflation algorithm. Observational density thinning controlled by namelist parameter *num\_close\_threshold* implemented. If number of observations close to a given observation is larger than the cutoff, the localization cutoff is adjusted to try to make the number close the same as the cutoff. This is a fundamentally two-d algorithm in this naive implementation. Update computations for spatial inflation are included in *filter\_assim\_region*. Routine *filter\_assim* also gets the inflation values as arguments. Copying of these inflation values for the shell driven files is added. Previously commented *print\_regional\_results* is deleted (produced errors with absoft compilers). Routine *comp\_correl* added to compute correlations. Routine *get\_correction\_from\_file* added to support *sampling\_error\_correction*. No longer need routine *assim\_tools\_end* since there is no longer a restart requirement. An error in the correlation computation for assimilation was removed. If all prior values of an observation were identical, a *NaN* could result from the old correlation computation. The *assim\_tools\_nml* was modified as follows: Removed *cov\_inflate, cov\_inflate\_sd, sd\_lower\_bound, deterministic\_cov\_inflate, start\_from\_assim\_restart, assim\_restart\_in\_file\_name, assim\_restart\_out\_file\_name, cov\_inflate\_upper\_bound*. Added *sampling\_error\_correction* and *num\_close\_threshold*.
  *assim\_tools\_smoother.f90*                         The smoother is still using the previous version of inflation and the corresponding *assim\_tools module* which has been renamed as *assim\_tools\_smoother*. Additional routines have been added to support the updates required to do smoothing (see documentation in smoother directory).
  diagnostics                                          Observation space diagnostics *obs\_diag* were modified to support observations on model levels. This is particularly useful for "perfect model" experiments. Direct observations of model variables (identity observations) are *not* supported, as this is an exploration of state-space, not observation-space. However, since the advent of *merge\_obs\_sequence* (see below) an observation sequence file *may* contain synthetic and real observations. Any identity observations are skipped. All other observations may be considered.
  *ensemble\_manager\_mod.f90*                         Modified to be able to handle multiple ensembles at one time to support the smoother which has an ensemble for each lag-time in the smoothing.
  *filter.f90*                                         The *adaptive\_inflate* module is now used and the namelist entry *cov\_inflate* has been removed from *filter\_nml*. Inflation is now done with *filter\_ensemble\_inflate* only if constant or varying spatial inflation is selected in the *adaptive\_inflate* namelist. Information about state space inflation is passed to *filter\_assim* as arguments. The call to *assim\_tools\_end* has been replace by *adaptive\_inflate\_end* which creates restarts for adaptive inflation. For spatially varying state inflation, two extra fields are tacked onto the state space diagnostic netcdf files to record the inflation mean and standard deviation. At present, inflation is done for the whole state at once; this may be very inefficient and should be examined. The entry *cov\_inflate* was removed from the namelist.
  *merge\_obs\_seq.f90*                                This is a fundamentally new program to DART. This routine can combine any two observation sequence files that are compatible. The files are deemed compatible if the 'copies' of the observations and the QC fields are *identical* between the two sequences. If one observation sequence file has only an ensemble mean and spread, the other observation sequence file can have *only* an ensemble mean and spread -- it cannot additionally have the N ensemble member estimates of the observation. Most of the time, this routine is envisioned to be used to combine *obs\_seq.out* files (as opposed to *obs\_seq.final* files). If the two sequences temporally overlap, it is faster to put the shorter sequence as *filenam\_seq2*, the insertion sort can get tedious. A new namelist *merge\_obs\_seq\_nml* has been added.
  mkmf                                                 New templates provided to support corral and lightning at NCAR.
  models                                               All models work with *merge\_obs\_seq* and adaptive inflation options.
  PBL\_1d                                              The PBL\_1d model has undergone extensive revisions as per the author's instructions. The DART portion of the code (i.e. those modules not directly imported from WRF) now compile cleanly with a variety of compilers. Note that because of the WRF convention of naming modules with a *.F* extension (instead of *.F90* or *.f90*) several compilers try to interpret this code as fixed-format code when it is, in fact, free-format. This necessitates setting the compiler flags to *force* the free-format interpretation. See your compiler for details.
  models/cam-fv                                        is a new model ... the finite-volume core version of CAM.
  models/cam                                           Now able to handle observations with height as a vertical coordinate. Can return interpolated values of pressure for use with GPS observation forward operators.
  cam/shell\_scripts/*job.simple.csh*                  is a new script that demonstrates the simplest possible (I think) way to assimilate ONE observation sequence file with CAM. It requires CAM restart files and the like, so it WILL need to be modified to work for you. Hopefully, you can just change a couple of the directories referenced in the script and be off ... This is likely to be the underpinnings of the next generation script that will flood the queue with conditionally executed batch jobs. If the observation sequence file for 06Z completes normally, the batch job for 12Z will start ... that sort of thing.
  doc/html/cgd\_cam.shtml                              New, more general information about using CAM and DART is available in [cgd\_cam.shtml](http://www.image.ucar.edu/DAReS/DART/cgd_cam.shtml).
  models/wrf                                           A new namelist variable, *assimilation\_period\_seconds*, allows the specification of the desired assimilation period, which was previously hardwired in the code. The *assimilation\_period\_seconds* is guaranteed to be an integer multiple of the underlying wrf model timestep. Added support for gps observations and observations of vortex position.
  ncep\_obs                                            Made changes to improve translation from *prep\_bufr*. Data from up to 3:00 UTC of the next day is included in the file with the current days' date.
  *obs\_def\_gps\_mod.f90*                             Modified to allow merging of two gps observation sequences.
  *obs\_def\_radar\_mod.f90*                           Added ability to merge multiple observation sequences.
  *obs\_def\_vortex\_mod.f90*                          Provides observation types for position of vortex center.
  *obs\_model\_mod.f90*                                Modified to be compatible with smoother use of multiple ensemble handles.
  *obs\_sequence\_mod.f90*                             Added initialization for observation sequence in *init\_obs\_sequence*; prevents possible access to uninitialized pointer.
  *preprocess.f90*                                     Changed defaults for input and output files to standard values rather than null.
  DART/shell\_scripts (the machine-specific ones)      *filter\_server.csh, assim\_filter.csh*, and *advance\_ens.csh* have been modified to use 'standard' language and can be used with multiple queuing systems as well as interactively. Extensive commenting has been added to help explain the semaphore files. All the other scripts in this directory should be considered 'deprecated'.
  model/xxx/shell\_scripts (the model-specific ones)   *advance\_model.csh*, and *assim\_region.csh* have been modified to use 'standard' language and can be used with multiple queuing systems as well as interactively. Extensive commenting has been added to help explain the semaphore files. All the other scripts in this directory should be considered 'deprecated'.
  smoother                                             A main program (*smoother.f90*) to do fixed-lag ensemble smoothing has been added and is documented in the smoother directory. This program still uses the previous version of the inflation and *assim\_tools*, which are available as *assim\_tools/assim\_tools\_smoother\_mod.f90*.
  *time\_manager\_mod.f90*                             Corrected problems with module initialization and modified print format to strictly comply such that it now compiles with gfortran.
  tutorial                                             Modified section 12 to give accurate discussion of new implementation of observation space inflation and a brief overview of the state space inflation options.
  ---------------------------------------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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


