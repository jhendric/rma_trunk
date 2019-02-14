[]{#TOP}

MODULE adaptive\_inflate\_mod
=============================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[INTERFACES](#Interface) / [NAMELIST](#Namelist) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[PRIVATE COMPONENTS](#PrivateComponents) / [TERMS OF USE](#Legalese)

Overview
--------

This module implements a variety of hierarchical Bayesian adaptive
inflation algorithms for use with ensemble filters. It can provide
constant valued inflation in state or observation space, consistent with
previous DART releases. It can provide spatially-constant, time-varying
adaptive inflation. It can provide spatially-varying, time-varying
adaptive inflation and it can provide temporally-varying observation
space inflation. And finally, it can provide adaptive damped inflation,
which decreases inflation through time when observation density varies.
Diagnostic output and restart files are available. Several papers on the
NCAR [IMAGe/DAReS](http://www.image.ucar.edu/DAReS) web page document
the algorithms in detail. The *DART/tutorial/section12* chapter has more
information.

Details on controlling the inflation options are contained in the
documentation for the filter. The filter\_nml controls what inflation
options are used.

Inflation flavor 3 (spatially-constant state space) reads and writes a
restart file that is the full size of the state vector, however it takes
the first value in the array and replicates that throughout the array.
This allows one to switch between flavors 2 and 3. Going from inflation
flavor 3 to 2 the initial value for all items in the state vector will
be a constant value and will then start to adapt. Going from inflation
flavor 2 to 3 whatever value is in the array at index 1 will be
replicated and used for the entire rest of the state vector items.

[]{#OtherModulesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    types_mod
    utilities_mod
    random_seq_mod
    time_manager_mod
    ensemble_manager_mod

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

  -------------------------------------- -------------------------------------------------------------
  *use adaptive\_inflate\_mod, only :*   [update\_inflation](#update_inflation)
                                         [adaptive\_inflate\_end](#adaptive_inflate_end)
                                         [inflate\_ens](#inflate_ens)
                                         [output\_inflate\_diagnostics](#output_inflate_diagnostics)
                                         [do\_obs\_inflate](#do_obs_inflate)
                                         [do\_single\_ss\_inflate](#do_single_ss_inflate)
                                         [do\_varying\_ss\_inflate](#do_varying_ss_inflate)
                                         [adaptive\_inflate\_init](#adaptive_inflate_init)
                                         [adaptive\_inflate\_type](#adaptive_inflate_type)
                                         [get\_inflate](#get_inflate)
                                         [set\_inflate](#set_inflate)
                                         [set\_sd](#set_sd)
                                         [set\_sd](#get_sd)
                                         [deterministic\_inflate](#deterministic_inflate)
  -------------------------------------- -------------------------------------------------------------

A note about documentation style. Optional arguments are enclosed in
brackets *\[like this\]*.

[]{#update_inflation}\

<div class="routine">

*call update\_inflation(inflate\_handle, inflate, inflate\_sd,
prior\_mean, prior\_var, obs, obs\_var, gamma)*
    type(adaptive_inflate_type), intent(in)    :: inflate_handle
    real(r8),                    intent(inout) :: inflate
    real(r8),                    intent(inout) :: inflate_sd
    real(r8),                    intent(in)    :: prior_mean
    real(r8),                    intent(in)    :: prior_var
    real(r8),                    intent(in)    :: obs
    real(r8),                    intent(in)    :: obs_var
    real(r8),                    intent(in)    :: gamma

</div>

<div class="indent1">

Updates the mean and standard deviation of an inflation distribution
given the prior values, the prior observation ensemble mean and
variance, and the observation and its error variance. The factor gamma
is the expected impact (0 to 1) of the state variable corresponding to
the inflation on the observation and is the product of the ensemble
correlation plus an additional localization factor or group regression
factors.

  --------------------- ----------------------------------------------------------------------------------
  *inflate\_handle  *   Handle to object that describes the inflation type and values.
  *inflate  *           Prior mean value of the inflation distribution.
  *inflate\_sd  *       Prior standard deviation of the inflation distribution.
  *prior\_mean  *       Mean of the prior observation ensemble.
  *prior\_var  *        Variance of the prior observation ensemble.
  *obs  *               The observed value.
  *obs\_var  *          Observational error variance.
  *gamma  *             Expected impact factor, product of correlation, localization, regression factor.
  --------------------- ----------------------------------------------------------------------------------

</div>

\
[]{#adaptive_inflate_end}\

<div class="routine">

*call adaptive\_inflate\_end(inflate\_handle, ens\_handle,
ss\_inflate\_index, ss\_inflate\_sd\_index)*
    type(adaptive_inflate_type), intent(in)    :: inflate_handle
    type(ensemble_type),         intent(inout) :: ens_handle
    integer,                     intent(in)    :: ss_inflate_index
    integer,                     intent(in)    :: ss_inflate_sd_index

</div>

<div class="indent1">

Outputs the values of inflation to restart files using the
ensemble\_manager for state space inflation and file output for
observation space inflation. Releases allocated storage in
inflate\_handle.

  ---------------------------- --------------------------------------------------------------------------------
  *inflate\_handle  *          Handle for the details of the inflation being performed.
  *ens\_handle  *              Handle for ensemble storage that holds values of state space inflation.
  *ss\_inflate\_index  *       Index in ensemble storage copies for state space inflation.
  *ss\_inflate\_sd\_index  *   Index in ensemble storage copies for state space inflation standard deviation.
  ---------------------------- --------------------------------------------------------------------------------

</div>

\
[]{#inflate_ens}\

<div class="routine">

*call inflate\_ens(inflate\_handle, ens,mean, inflate *\[,var\_in\]*)*
    type(adaptive_inflate_type),               intent(in)  :: inflate_handle
    real(r8),                    dimension(:), intent(out) :: ens
    real(r8),                                  intent(in)  :: mean
    real(r8),                                  intent(in)  :: inflate
    real(r8),                    optional,     intent(in)  :: var_in

</div>

<div class="indent1">

Given an ensemble, its mean and the covarance inflation factor, inflates
the ensemble.

  --------------------- ----------------------------------------------------------
  *inflate\_handle  *   Handle for the details of the inflation being performed.
  *ens  *               Values for the ensemble to be inflated
  *mean  *              The mean of the ensemble.
  *inflate  *           The covariance inflation factor.
  *var\_in  *           The variance of the ensemble.
  --------------------- ----------------------------------------------------------

</div>

\
[]{#output_inflate_diagnostics}\

<div class="routine">

*call output\_inflate\_diagnostics(inflate\_handle, time)*
    type(adaptive_inflate_type), intent(in) :: inflate_handle
    type(time_type),             intent(in) :: time

</div>

<div class="indent1">

Outputs diagnostic record of inflation for the observation space of
spatially constant state space inflation. Spatially varying state space
diagnostics are in the Posterior and Prior Diagnostic netcdf files and
are written with calls from filter.f90.

  --------------------- ----------------------------------------------------------
  *inflate\_handle  *   Handle for the details of the inflation being performed.
  *time  *              Time of this diagnostic info.
  --------------------- ----------------------------------------------------------

</div>

\
[]{#do_obs_inflate}\

<div class="routine">

*var = do\_obs\_inflate(inflate\_handle)*
    logical,               intent(out) :: do_obs_inflate
    adaptive_inflate_type, intent(in)  :: inflate_handle

</div>

<div class="indent1">

Returns true if observation space inflation is being done by this
handle.

  ---------------------- -----------------------------------------------------------
  *do\_obs\_inflate  *   True if obs space inflation is being done by this handle.
  *inflate\_handle  *    Handle to inflation details.
  ---------------------- -----------------------------------------------------------

</div>

\
[]{#do_varying_ss_inflate}\

<div class="routine">

*var = do\_varying\_ss\_inflate(inflate\_handle)*
    logical,               intent(out) :: do_varying_ss_inflate
    adaptive_inflate_type, intent(in)  :: inflate_handle

</div>

<div class="indent1">

Returns true if spatially varying state space inflation is being done by
this handle.

  ------------------------------ -------------------------------------------------------------------------------
  *do\_varying\_ss\_inflate  *   True if spatially varying state space inflation is being done by this handle.
  *inflate\_handle  *            Handle to inflation details.
  ------------------------------ -------------------------------------------------------------------------------

</div>

\
[]{#do_single_ss_inflate}\

<div class="routine">

*var = do\_single\_ss\_inflate(inflate\_handle)*
    logical,               intent(out) :: do_single_ss_inflate
    adaptive_inflate_type, intent(in)  :: inflate_handle

</div>

<div class="indent1">

Returns true if spatially fixed state space inflation is being done by
this handle.

  ----------------------------- -----------------------------------------------------------------------------
  *do\_single\_ss\_inflate  *   True if spatially fixed state space inflation is being done by this handle.
  *inflate\_handle  *           Handle to inflation details.
  ----------------------------- -----------------------------------------------------------------------------

</div>

\
[]{#adaptive_inflate_init}\

<div class="routine">

*call adaptive\_inflate\_init(inflate\_handle, inf\_flavor,
mean\_from\_restart, sd\_from\_restart, output\_restart, deterministic,
in\_file\_name, out\_file\_name, diag\_file\_name, inf\_initial,
sd\_initial, inf\_lower\_bound, inf\_upper\_bound, sd\_lower\_bound,
ens\_handle, ss\_inflate\_index, ss\_inflate\_sd\_index, label)*
    type(adaptive_inflate_type), intent(inout) :: inflate_handle
    integer, intent(in)                        :: inf_flavor
    logical, intent(in)                        :: mean_from_restart
    logical, intent(in)                        :: sd_from_restart
    logical, intent(in)                        :: output_restart
    logical, intent(in)                        :: deterministic
    character(len=*), intent(in)               :: in_file_name
    character(len=*), intent(in)               :: out_file_name
    character(len=*), intent(in)               :: diag_file_name
    real(r8), intent(in)                       :: inf_initial
    real(r8), intent(in)                       :: sd_initial
    real(r8), intent(in)                       :: inf_lower_bound
    real(r8), intent(in)                       :: inf_upper_bound
    real(r8), intent(in)                       :: sd_lower_bound
    type(ensemble_type), intent(inout)         :: ens_handle
    integer, intent(in)                        :: ss_inflate_index
    integer, intent(in)                        :: ss_inflate_sd_index
    character(len=*), intent(in)               :: label

</div>

<div class="indent1">

Initializes a descriptor of an inflation object.

  ---------------------------- -----------------------------------------------------------------------------------
  *inflate\_handle  *          Handle for the inflation descriptor being initialized.
  *inf\_flavor  *              Type of inflation, 1=obs\_inflate, 2=varying\_ss\_inflate, 3=single\_ss\_inflate.
  *mean\_from\_restart  *      True if inflation mean values to be read from restart file.
  *sd\_from\_restart  *        True if inflation standard deviation values to be read from restart file.
  *output\_restart  *          True if an inflation restart file is to be output.
  *deterministic  *            True if deterministic inflation is to be done.
  *in\_file\_name  *           File name from which to read restart.
  *out\_file\_name  *          File name to which to write restart.
  *diag\_file\_name  *         File name to which to write diagnostic output; obs space inflation only .
  *inf\_initial  *             Initial value of inflation for start\_from\_restart=.false.
  *sd\_initial  *              Initial value of inflation standard deviation for start\_from\_restart=.false.
  *inf\_lower\_bound  *        Lower bound on inflation value.
  *inf\_upper\_bound  *        Upper bound on inflation value.
  *sd\_lower\_bound  *         Lower bound on inflation standard deviation.
  *ens\_handle  *              Ensemble handle with storage for state space inflation.
  *ss\_inflate\_index  *       Index op copy in ensemble storage for inflation value.
  *ss\_inflate\_sd\_index  *   Index of copy in ensemble storage for inflation standard deviation.
  *label  *                    Character label to be used in diagnostic output (e.g. 'Prior', 'Posterior').
  ---------------------------- -----------------------------------------------------------------------------------

</div>

\
[]{#get_sd}\

<div class="routine">

*var = get\_sd(inflate\_handle)*
    real(r8), intent(out)                   :: get_sd
    type(adaptive_inflate_type), intent(in) :: inflate_handle

</div>

<div class="indent1">

Returns value of observation space inflation standard deviation.

  --------------------- ---------------------------------------------------
  *get\_sd  *           Returns the value of observation space inflation.
  *inflate\_handle  *   Handle for inflation descriptor.
  --------------------- ---------------------------------------------------

</div>

\
[]{#get_inflate}\

<div class="routine">

*var = get\_inflate(inflate\_handle)*
    real(r8), intent(out)                   :: get_inflate
    type(adaptive_inflate_type), intent(in) :: inflate_handle

</div>

<div class="indent1">

Returns value of observation space inflation.

  --------------------- ---------------------------------------------------
  *get\_inflate  *      Returns the value of observation space inflation.
  *inflate\_handle  *   Handle for inflation descriptor.
  --------------------- ---------------------------------------------------

</div>

\
[]{#set_inflate}\

<div class="routine">

*call set\_inflate(inflate\_handle, inflate)*
    type(adaptive_inflate_type), intent(inout) :: inflate_handle
    real(r8), intent(in)                       :: inflate

</div>

<div class="indent1">

Set the value of observation space inflation.

  --------------------- ------------------------------------------------
  *inflate\_handle  *   Handle for inflation descriptor.
  *inflate  *           Set observation space inflation to this value.
  --------------------- ------------------------------------------------

</div>

\
[]{#set_sd}\

<div class="routine">

*call set\_sd(inflate\_handle, sd)*
    type(adaptive_inflate_type), intent(inout) :: inflate_handle
    real(r8), intent(in)                       :: sd

</div>

<div class="indent1">

Set the value of observation space inflation standard deviation.

  --------------------- -------------------------------------------------------------------
  *inflate\_handle  *   Handle for inflation descriptor.
  *sd  *                Set observation space inflation standard deviation to this value.
  --------------------- -------------------------------------------------------------------

</div>

\
[]{#deterministic_inflate}\

<div class="routine">

*var = deterministic\_inflate(inflate\_handle)*
    logical, intent(out)                    :: deterministic_inflate
    type(adaptive_inflate_type), intent(in) :: inflate_handle

</div>

<div class="indent1">

Returns true if deterministic inflation is being done.

  ---------------------------- --------------------------------------------------------
  *deterministic\_inflate  *   Returns true if deterministic inflation is being done.
  *inflate\_handle  *          Handle for inflation descriptor.
  ---------------------------- --------------------------------------------------------

</div>

\
[]{#adaptive_inflate_type}\

<div class="type">

    type adaptive_inflate_type
       private
       integer :: inflation_flavor
       integer :: obs_diag_unit
       logical :: start_from_restart
       logical :: output_restart
       logical :: deterministic
       character(len = 129) :: in_file_name
       character(len = 129) :: out_file_name
       character(len = 129) :: diag_file_name
       real(r8) :: inflate
       real(r8) :: sd
       real(r8) :: sd_lower_bound
       real(r8) :: inf_lower_bound
       real(r8) :: inf_upper_bound
       type(random_seq_type) :: ran_seq
    end type adaptive_inflate_type

</div>

<div class="indent1">

Provides a handle for a descriptor of inflation. Includes type of
inflation, values controlling it, input and output file names, an output
file descriptor for observation space inflation diagnotics, and a random
sequence for doing reproducible non-determinstic inflation. There are 2
instances of this type, one for Prior and one for Posterior inflation.

  Component              Description
  ---------------------- ----------------------------------------------------------------------------------
  inflation\_flavor      Type of inflation; 0=none, 1=obs. space, 2=spatially varying, 3=spatially-fixed.
  obs\_diag\_unit        Unit descriptor for output diagnostic file.
  start\_from\_restart   True if initial inflation to be read from file.
  output\_restart        True if final inflation values to be written to file.
  deterministic          True if inflation is to be done be deterministic algorithm.
  in\_file\_name         File name containing restart.
  out\_file\_name        File to contain output restart.
  diag\_file\_name       File to hold observation space diagnostics.
  inflate                Initial value of inflation for all types; current value for obs. space.
  sd                     Initial value of sd for all types; current value for obs. space.
  sd\_lower\_bound       Don't allow standard deviation to get smaller than this.
  inf\_lower\_bound      Don't let inflation get smaller than this.
  inf\_upper\_bound      Don't let inflation get larger than this.
  ran\_seq               Handle to random number sequence to allow reproducing non-deterministic inflate.

</div>

\
[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

The adaptive\_inflate module no longer has a namelist. Control has been
moved to [&filter\_nml](filter_mod.html#Namelist) in filter.

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

Three files are opened from this module, but all names are passed in
from the filter\_nml now, and there are 2 values for each name: one for
the prior and one for the posterior inflation.

-   inf\_in\_file\_name\
    Mean and standard deviation values read in restart file format.
-   inf\_out\_file\_name\
    Mean and standard deviation values written in restart file format.
-   inf\_diag\_file\_name\
    Contains diagnostic history of inflation values for obs space and
    spatially-fixed state space inflation. Diagnostics for
    spatially-varying state space inflation are extra fields on the
    Posterior and Prior diagnostic netcdf files created in filter.f90.

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

-   Anderson, J. L., 2007: An adaptive covariance inflation error
    correction algorithm for ensemble filters. [Tellus
    A]{style="font-style: italic;"}, [59]{style="font-weight: bold;"},
    210-224.\
    [doi:
    10.1111/j.1600-0870.2006.00216.x](http://dx.doi.org/10.1111/j.1600-0870.2006.00216.x)\
-   Anderson, J. L., 2009: Spatially and temporally varying adaptive
    covariance inflation for ensemble filters. [Tellus
    A]{style="font-style: italic;"}, [61]{style="font-weight: bold;"},
    72-83.\
    [doi:
    10.1111/j.1600-0870.2008.00361.x](http://dx.doi.org/10.1111/j.1600-0870.2008.00361.x)\

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
adaptive\_inflate\_init
Cannot have non-deterministic inflation and inf\_lower\_bound &lt; 1.
Algorithm can't work in this case.\
adaptive\_inflate\_init
ss\_inflate\_index = \#\#\# and ss\_inflate\_sd\_index = \#\#\# must be
contiguous.
Storage for these two must be contiguous in ensemble\_manager.
adaptive\_inflate\_end
ss\_inflate\_index = \#\#\# and ss\_inflate\_sd\_index = \#\#\# must be
contiguous.
Storage for these two must be contiguous in ensemble\_manager.

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

no discussion

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


