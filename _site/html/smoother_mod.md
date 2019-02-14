[]{#TOP}

MODULE smoother\_mod
====================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [INTERFACES](#Interface) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[PRIVATE COMPONENTS](#PrivateComponents) / [TERMS OF USE](#Legalese)

Overview
--------

Implements a fixed lag ensemble smoother as part of the filter. For now,
this is done inefficiently with a separate call to
*assim\_tools\_mod:filter\_assim()* for each lag.\
\
To enable the smoother, set the number of lags (num\_lags) to something
larger than 0 in the *smoother\_nml* section of your *input.nml* file
and run *filter* as before.

<div class="routine">

    &smoother_nml
       num_lags              = 10,
       start_from_restart    = .false.,
       output_restart        = .true.,
       restart_in_file_name  = "ics",
       restart_out_file_name = "restart"  /

</div>

In the low order models, 10 is a plausible number.\
\
In addition to generating *preassim.nc* and *analysis.nc* files, files
of the form *Lag\_NNNNN\_Diag.nc* will be generated. Each of these has N
fewer timesteps than the lag=0 run, starting at the same time but ending
N timesteps sooner. The *obs\_seq.final* file and the *preassim.nc* and
*analysis.nc* files will be the same as the non-lagged version; the new
output will be in each of the *Lag\_NNNNN\_Diag.nc* files.

[]{#Example}

EXAMPLE
-------

If you have a *true\_state.nc* file and want to use the
*plot\_total\_err* matlab function to plot the error, you must do the
following steps to generate analogs of lagged *true\_state.nc* files to
use as a comparison. (The logic is not currently implemented in the
matlab scripts to be able to compare netCDF files with unequal time
coordinates.)\
\
Make N separate versions of the true\_state.nc with the last N timesteps
removed. Using the netCDF NCO operator program 'ncks' is one way. If the
true\_state.nc file has 1000 time steps, then this command removes the
last one:

<div class="unix">

ncks -d time,0,998 true\_state.nc True\_Lag01.nc

</div>

Note that the first time is at index 0, so the last timestep is index
999 in the full file, and 998 in the truncated file. Repeat this step
for all N lags. Here are NCO commands to generate 10 truth files for
num\_lags = 10, 1000 time steps in true\_state.nc:

<div class="unix">

ncks -d time,0,998 true\_state.nc True\_Lag01.nc\
ncks -d time,0,997 true\_state.nc True\_Lag02.nc\
ncks -d time,0,996 true\_state.nc True\_Lag03.nc\
ncks -d time,0,995 true\_state.nc True\_Lag04.nc\
ncks -d time,0,994 true\_state.nc True\_Lag05.nc\
ncks -d time,0,993 true\_state.nc True\_Lag06.nc\
ncks -d time,0,992 true\_state.nc True\_Lag07.nc\
ncks -d time,0,991 true\_state.nc True\_Lag08.nc\
ncks -d time,0,990 true\_state.nc True\_Lag09.nc\
ncks -d time,0,989 true\_state.nc True\_Lag10.nc\

</div>

Here is an example matlab session which plots the lag=0 results and then
odd numbered lags from 1 to 9. It uses the *plot\_total\_err* function
from the \$DART/matlab directory:

    datadir    = '.';
    truth_file = fullfile(datadir,'true_state.nc');
    diagn_file = fullfile(datadir,'preassim.nc');
    plot_total_err
    reply = input('original data.  hit enter to continue ');

    truth_file = fullfile(datadir,'True_Lag01.nc');
    diagn_file = fullfile(datadir,'Lag_00001_Diag.nc');
    plot_total_err
    reply = input('Lag 01.  hit enter to continue ');

    truth_file = fullfile(datadir,'True_Lag03.nc');
    diagn_file = fullfile(datadir,'Lag_00003_Diag.nc');
    plot_total_err
    reply = input('Lag 03.  hit enter to continue ');

    truth_file = fullfile(datadir,'True_Lag05.nc');
    diagn_file = fullfile(datadir,'Lag_00005_Diag.nc');
    plot_total_err
    reply = input('Lag 05.  hit enter to continue ');

    truth_file = fullfile(datadir,'True_Lag07.nc');
    diagn_file = fullfile(datadir,'Lag_00007_Diag.nc');
    plot_total_err
    reply = input('Lag 07.  hit enter to continue ');

    truth_file = fullfile(datadir,'True_Lag09.nc');
    diagn_file = fullfile(datadir,'Lag_00009_Diag.nc');
    plot_total_err
    reply = input('Lag 09.  hit enter to continue ');

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

    &smoother_nml
       num_lags              = 0,
       start_from_restart    = .false.,
       output_restart        = .false.,
       restart_in_file_name  = 'ics',
       restart_out_file_name = 'restart'  
    /

</div>

\
\

<div>

  Item                          Type                 Description
  ----------------------------- -------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  num\_lags                     integer              Number of smoother lags; &lt; 1 means no smoother.
  start\_from\_restart          logical              True if smoother states are to come from restart file(s). False if they are to be spun up from scratch.
  output\_restart               logical              True if restart file(s) are to be written, else false.
  restart\_in\_file\_name       character(len=129)   String used to construct the file name from which to read restart data. 'Lag\_NNNNN\_' will be prepended to the specified value to create the actual filename. If each ensemble is to be read from a separate file, the .NNNN ensemble number will also be appended. e.g. specifying 'ics' here results in 'Lag\_00001\_ics' if all ensemble members are read from a single file, 'Lag\_00001\_ics.0001', 'Lag\_00001\_ics.0002', etc for multiples.
  restart\_out\_file\_name      character(len=129)   String used to construct the file name to which to write restart data. 'Lag\_NNNNN\_' will be prepended to the specified value to create the actual filename. If each ensemble is to be written to a separate file, the .NNNN ensemble number will also be appended. e.g. specifying 'restart' here results in 'Lag\_00001\_restart' if all ensemble members are written to a single file, 'Lag\_00001\_restart.0001', 'Lag\_00001\_restart.0002', etc for multiples.

</div>

\
\
[]{#OtherModulesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    types_mod
    mpi_utilities_mod
    utilities_mod
    ensemble_manager_mod
    time_manager_mod
    assim_model_mod
    assim_tools_mod
    obs_sequence_mod
    adaptive_inflate_mod

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

  ----------------------------- ----------------------------------------------------------------------
  *use smoother\_mod, only :*   [smoother\_read\_restart](#smoother_read_restart)
                                [advance\_smoother](#advance_smoother)
                                [smoother\_gen\_copy\_meta\_data](#smoother_gen_copy_meta_data)
                                [smoother\_write\_restart](#smoother_write_restart)
                                [init\_smoother](#init_smoother)
                                [do\_smoothing](#do_smoothing)
                                [smoother\_mean\_spread](#smoother_mean_spread)
                                [smoother\_assim](#smoother_assim)
                                [filter\_state\_space\_diagnostics](#filter_state_space_diagnostics)
                                [smoother\_ss\_diagnostics](#smoother_ss_diagnostics)
                                [smoother\_end](#smoother_end)
  ----------------------------- ----------------------------------------------------------------------

A note about documentation style. Optional arguments are enclosed in
brackets *\[like this\]*.

[]{#smoother_read_restart}\

<div class="routine">

*call smoother\_read\_restart(ens\_handle, ens\_size, model\_size,
time1, init\_time\_days)*
    type(ensemble_type), intent(inout) :: ens_handle
    integer, intent(in)                :: ens_size
    integer, intent(in)                :: model_size
    type(time_type), intent(inout)     :: time1
    integer, intent(in)                :: init_time_days

</div>

<div class="indent1">

Reads in ensemble of states for all lag estimates from a restart file.

  ---------------------- ---------------------------------------------------------------------------------------------
  *ens\_handle  *        Handle of ensemble manager structure of single state; copied into all lags for startup.
  *ens\_size  *          Size of the ensemble.
  *model\_size  *        Size of the model state vector.
  *time1  *              Overwrite the time in the restart file with this value if init\_time\_days is non-negative.
  *init\_time\_days  *   If non-negative, use time1 instead of time in restart file.
  ---------------------- ---------------------------------------------------------------------------------------------

</div>

\
[]{#advance_smoother}\

<div class="routine">

*call advance\_smoother(ens\_handle)*
    type(ensemble_type), intent(in) :: ens_handle

</div>

<div class="indent1">

Advances smoother state estimates at all lags forward in time. This
entails copying the most recent smoother state, contained in
ens\_handle, into the lag 1 smoother state and pushing back all other
lags by 1 (i.e. lag 1 becomes lag 2, etc.).

  ----------------- --------------------------------------------------
  *ens\_handle  *   Ensemble handle with most recent filtered state.
  ----------------- --------------------------------------------------

</div>

\
[]{#smoother_gen_copy_meta_data}\

<div class="routine">

*call smoother\_gen\_copy\_meta\_data(num\_output\_state\_members,
output\_inflation)*
    integer, intent(in) :: num_output_state_members
    logical, intent(in) :: output_inflation

</div>

<div class="indent1">

Initializes the metadata required for the smoother state space
diagnostic files.

  --------------------------------- --------------------------------------------------------------------------------------------
  *num\_output\_state\_members  *   Number of copies of smoother state vector that should be in state space diagnostic output.
  *output\_inflation  *             True if smoother state space output should include inflation values.
  --------------------------------- --------------------------------------------------------------------------------------------

</div>

\
[]{#smoother_write_restart}\

<div class="routine">

*call smoother\_write\_restart(start\_copy, end\_copy)*
    integer, intent(in) :: start_copy
    integer, intent(in) :: end_copy

</div>

<div class="indent1">

Outputs restart files for all lags of smoother state. Integer arguments
specify the start and end global indices of a continguous set of copies
that contain the ensemble members.

  ----------------- -------------------------------------------------------------------------------------
  *start\_copy  *   Global index of ensemble copy that starts the actual ensemble members for smoother.
  *end\_copy  *     Global index of ensemble copy that ends the actual ensemble members for smoother.
  ----------------- -------------------------------------------------------------------------------------

</div>

\
[]{#init_smoother}\

<div class="routine">

*call init\_smoother(ens\_handle, POST\_INF\_COPY, POST\_INF\_SD\_COPY)*
    type(ensemble_type), intent(inout) :: ens_handle
    integer, intent(in)                :: POST_INF_COPY
    integer, intent(in)                :: POST_INF_SD_COPY

</div>

<div class="indent1">

Initializes the storage needed for a smoother. Also initializes an
adaptive inflation type that does NO inflation (not currently supported
for smoothers).

  ------------------------- --------------------------------------------------------------------------------------------
  *ens\_handle  *           An ensemble handle for the filter that contains information about ensemble and model size.
  *POST\_INF\_COPY  *       Global index of ensemble copy that holds posterior state space inflation values.
  *POST\_INF\_SD\_COPY  *   Global index of ensemble copy that holds posterior inflation standard deviation values.
  ------------------------- --------------------------------------------------------------------------------------------

</div>

\
[]{#do_smoothing}\

<div class="routine">

*var = do\_smoothing()*
    logical, intent(out) :: do_smoothing

</div>

<div class="indent1">

Returns true if smoothing is to be done, else false.

  ------------------- ------------------------------------------
  *do\_smoothing  *   Returns true if smoothing is to be done.
  ------------------- ------------------------------------------

</div>

\
[]{#smoother_mean_spread}\

<div class="routine">

*call smoother\_mean\_spread(ens\_size,ENS\_MEAN\_COPY,ENS\_SD\_COPY,
output\_state\_ens\_mean,output\_state\_ens\_spread)*
    integer, intent(in) :: ens_size
    integer, intent(in) :: ENS_MEAN_COPY
    integer, intent(in) :: ENS_SD_COPY
    logical, intent(in) :: output_state_ens_mean
    logical, intent(in) :: output_state_ens_spread

</div>

<div class="indent1">

Computes the ensemble mean (and spread if required) of all state
variables for all lagged ensembles. Spread is only computed if it is
required for output.

  -------------------------------- ---------------------------------------------------------------------
  *ens\_size  *                    Size of ensemble.
  *ENS\_MEAN\_COPY  *              Global index of copy that stores ensemble mean.
  *ENS\_SD\_COPY  *                Global index of copy that stores ensemble spread.
  *output\_state\_ens\_mean  *     True if the ensemble mean is to be output to state diagnostic file.
  *output\_state\_ens\_spread  *   True if ensemble spread is to be output to state diagnostic file.
  -------------------------------- ---------------------------------------------------------------------

</div>

\
[]{#smoother_assim}\

<div class="routine">

*call smoother\_assim(obs\_ens\_handle, seq, keys, ens\_size,
num\_groups, obs\_val\_index, ENS\_MEAN\_COPY, ENS\_SD\_COPY,
PRIOR\_INF\_COPY, PRIOR\_INF\_SD\_COPY, OBS\_KEY\_COPY,
OBS\_GLOBAL\_QC\_COPY, OBS\_PRIOR\_MEAN\_START, OBS\_PRIOR\_MEAN\_END,
OBS\_PRIOR\_VAR\_START, OBS\_PRIOR\_VAR\_END)*
    type(ensemble_type), intent(inout)  :: obs_ens_handle
    type(obs_sequence_type), intent(in) :: seq
    integer, dimension(:), intent(in)   :: keys
    integer, intent(in)                 :: ens_size
    integer, intent(in)                 :: num_groups
    integer, intent(in)                 :: obs_val_index
    integer, intent(in)                 :: ENS_MEAN_COPY
    integer, intent(in)                 :: ENS_SD_COPY
    integer, intent(in)                 :: PRIOR_INF_COPY
    integer, intent(in)                 :: PRIOR_INF_SD_COPY
    integer, intent(in)                 :: OBS_KEY_COPY
    integer, intent(in)                 :: OBS_GLOBAL_QC_COPY
    integer, intent(in)                 :: OBS_PRIOR_MEAN_START
    integer, intent(in)                 :: OBS_PRIOR_MEAN_END
    integer, intent(in)                 :: OBS_PRIOR_VAR_START
    integer, intent(in)                 :: OBS_PRIOR_VAR_END

</div>

<div class="indent1">

Does assimilation of a set of observations for each smoother lag.

  ----------------------------- ---------------------------------------------------------------------------------------------------
  *obs\_ens\_handle  *          Handle for ensemble manager holding prior estimates of observations.
  *seq  *                       Observation sequence being assimilated.
  *keys  *                      A one dimensional array containing indices in seq of observations to as similate at current time.
  *ens\_size  *                 Ensemble size.
  *num\_groups  *               Number of groups in filter.
  *obs\_val\_index  *           Integer index of copy of data in seq that contains the observed value from instruments.
  *ENS\_MEAN\_COPY  *           Global index in smoother's state ensemble that holds ensemble mean.
  *ENS\_SD\_COPY  *             Global index in smoother's state ensemble that holds ensemble standard deviation.
  *PRIOR\_INF\_COPY  *          Global index in obs\_ens\_handle that holds inflation values (not used for smoother).
  *PRIOR\_INF\_SD\_COPY  *      Global index in obs\_ens\_handle that holds inflation sd values (not used for smoother).
  *OBS\_KEY\_COPY  *            Global index in obs\_ens\_handle that holds the key for the observation.
  *OBS\_GLOBAL\_QC\_COPY  *     Global index in obs\_ens\_handle that holds the quality control value.
  *OBS\_PRIOR\_MEAN\_START  *   Global index in obs\_ens\_handle that holds the first group's prior mean.
  *OBS\_PRIOR\_MEAN\_END  *     Global index in obs\_ens\_handle that holds the last group's prior mean.
  *OBS\_PRIOR\_VAR\_START  *    Global index in obs\_ens\_handle that holds the first group's prior variance.
  *OBS\_PRIOR\_VAR\_END  *      Global index in obs\_ens\_handle that holds the last group's prior variance.
  ----------------------------- ---------------------------------------------------------------------------------------------------

</div>

\
[]{#filter_state_space_diagnostics}\

<div class="routine">

*call filter\_state\_space\_diagnostics(out\_unit, ens\_handle,
model\_size, num\_output\_state\_members, output\_state\_mean\_index,
output\_state\_spread\_index, output\_inflation, temp\_ens,
ENS\_MEAN\_COPY, ENS\_SD\_COPY, inflate, INF\_COPY, INF\_SD\_COPY)*
    type(netcdf_file_type), intent(inout)   :: out_unit
    type(ensemble_type), intent(inout)      :: ens_handle
    integer, intent(in)                     :: model_size
    integer, intent(in)                     :: num_output_state_members
    integer, intent(in)                     :: output_state_mean_index
    integer, intent(in)                     :: output_state_spread_index
    logical, intent(in)                     :: output_inflation
    real(r8), intent(out)                   :: temp_ens(model_size)
    integer, intent(in)                     :: ENS_MEAN_COPY
    integer, intent(in)                     :: ENS_SD_COPY
    type(adaptive_inflate_type), intent(in) :: inflate
    integer, intent(in)                     :: INF_COPY
    integer, intent(in)                     :: INF_SD_COPY

</div>

<div class="indent1">

Writes state space diagnostic values including ensemble members, mean
and spread, and inflation mean and spread to a netcdf file.

  ---------------------------------- ---------------------------------------------------------------------
  *out\_unit  *                      Descriptor for the netcdf file being written.
  *ens\_handle  *                    Ensemble handle whose state space values are to be written.
  *model\_size  *                    Size of the model state vector.
  *num\_output\_state\_members  *    Number of individual state members to be output.
  *output\_state\_mean\_index  *     Index in netcdf file for ensemble mean.
  *output\_state\_spread\_index  *   Index in netcdf file for ensemble spread.
  *output\_inflation  *              True if the inflation values are to be output. Default is .TRUE.
  *temp\_ens  *                      Storage passed in to avoid having to allocate extra space.
  *ENS\_MEAN\_COPY  *                Global index in ens\_handle for ensemble mean.
  *ENS\_SD\_COPY  *                  Global index in ens\_handle for ensemble spread.
  *inflate  *                        Contains description and values of state space inflation.
  *INF\_COPY  *                      Global index in ens\_handle of inflation values.
  *INF\_SD\_COPY  *                  Global index in ens\_handle of inflation standard deviation values.
  ---------------------------------- ---------------------------------------------------------------------

</div>

\
[]{#smoother_ss_diagnostics}\

<div class="routine">

*call smoother\_ss\_diagnostics(model\_size,
num\_output\_state\_members, output\_inflation, temp\_ens,
ENS\_MEAN\_COPY, ENS\_SD\_COPY, POST\_INF\_COPY, POST\_INF\_SD\_COPY)*
    integer, intent(in)   :: model_size
    integer, intent(in)   :: num_output_state_members
    logical, intent(in)   :: output_inflation
    real(r8), intent(out) :: temp_ens(model_size)
    integer, intent(in)   :: ENS_MEAN_COPY
    integer, intent(in)   :: ENS_SD_COPY
    integer, intent(in)   :: POST_INF_COPY
    integer, intent(in)   :: POST_INF_SD_COPY

</div>

<div class="indent1">

Outputs state space diagnostics files for all smoother lags.

  --------------------------------- -------------------------------------------------------------------------------------------------
  *model\_size  *                   Size of the model state vector.
  *num\_output\_state\_members  *   Number of state copies to be output in the state space diagnostics file.
  *output\_inflation  *             True if the inflation values are to be output. Default is .TRUE.
  *temp\_ens  *                     Storage passed in to avoid having to allocate extra space.
  *ENS\_MEAN\_COPY  *               Global index of the ensemble mean in the lag smoother ensemble handles.
  *ENS\_SD\_COPY  *                 Global index of the ensemble spread in the lag smoother ensemble handles.
  *POST\_INF\_COPY  *               Global index of the inflation value in the lag smoother ensemble handles (not currently used).
  *POST\_INF\_SD\_COPY  *           Global index of the inflation spread in the lag smoother ensemble handles (not currently used).
  --------------------------------- -------------------------------------------------------------------------------------------------

</div>

\
[]{#smoother_end}\

<div class="routine">

*call smoother\_end()*

</div>

<div class="indent1">

Releases storage allocated for smoother.

</div>

\
[]{#smoother_inc_lags}\

<div class="routine">

*call smoother\_inc\_lags()*

</div>

<div class="indent1">

Increments the number of lags that are in use for smoother. Used when a
smoother is being started up and there have not been enough times to
propagate the state to all requested lags.

</div>

\
[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   input.nml
-   smoother initial condition files
-   smoother restart files

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

1.  none

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
smoother\_gen\_copy\_meta\_data
output metadata in smoother needs ensemble size &lt; 10000, not \#\#\#
Can't output more than 9999 copies.

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
  Contact:           DART core group
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


