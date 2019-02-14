[]{#TOP}

MODULE filter\_mod
==================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [MODULES](#Modules) / [FILES](#FilesUsed) /
[ERRORS](#Errors) / [PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
--------

Main module for driving ensemble filter assimilations. Used by
filter.f90, perfect\_model\_obs.f90, model\_mod\_check.f90, and a
variety of test programs. See the [filter
description](../../programs/filter/filter.html) for a general
description of filter capabilities and controls.

*filter\_mod* is a Fortran 90 module, and provides a large number of
options for controlling execution behavior and parameter configuration
that are driven from its namelist. See the [namelist](#Namelist) section
below for more details. The number of assimilation steps to be done is
controlled by the input observation sequence and by the time-stepping
capabilities of the model being used in the assimilation.

See the [DART web site](http://www.image.ucar.edu/DAReS/DART) for more
documentation, including a discussion of the capabilities of the
assimilation system, a diagram of the entire execution cycle, the
options and features.

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

    &filter_nml
       single_file_in               = .false.,
       input_state_files            = '',
       input_state_file_list        = '',
       init_time_days               = 0,
       init_time_seconds            = 0,
       perturb_from_single_instance = .false.,
       perturbation_amplitude       = 0.2,

       stages_to_write              = 'output'

       single_file_out              = .false.,
       output_state_files           = '',
       output_state_file_list       = '',
       output_interval              = 1,
       output_members               = .true.,
       num_output_state_members     = 0,
       output_mean                  = .true.,
       output_sd                    = .true.,
       write_all_stages_at_end      = .false.,
       compute_posterior            = .true.

       ens_size                     = 20,
       num_groups                   = 1,
       distributed_state            = .true.,

       async                        = 0,
       adv_ens_command              = "./advance_model.csh",
       tasks_per_model_advance      = 1,

       obs_sequence_in_name         = "obs_seq.out",
       obs_sequence_out_name        = "obs_seq.final",
       num_output_obs_members       = 0,
       first_obs_days               = -1,
       first_obs_seconds            = -1,
       last_obs_days                = -1,
       last_obs_seconds             = -1,
       obs_window_days              = -1,
       obs_window_seconds           = -1,

       inf_flavor                   = 0,                       0,
       inf_initial_from_restart     = .false.,                 .false.,
       inf_sd_initial_from_restart  = .false.,                 .false.,
       inf_deterministic            = .true.,                  .true.,
       inf_initial                  = 1.0,                     1.0,
       inf_lower_bound              = 1.0,                     1.0,
       inf_upper_bound              = 1000000.0,               1000000.0,
       inf_damping                  = 1.0,                     1.0,
       inf_sd_initial               = 0.0,                     0.0,
       inf_sd_lower_bound           = 0.0,                     0.0,
       inf_sd_max_change            = 1.05,                    1.05,

       trace_execution              = .false.,
       output_timestamps            = .false.,
       output_forward_op_errors     = .false.,
       write_obs_every_cycle        = .false.,
       silence                      = .false.,
     /

</div>

\
\

Particular options to be aware of are: async, ens\_size, cutoff
(localization radius), inflation flavor, outlier\_threshold, restart
filenames (including inflation), obs\_sequence\_in\_name,
horiz\_dist\_only, binary or ascii controls for observation sequence
file formats. Some of these important items are located in other
namelists, but all are in the same input.nml file.

The inflation control variables are all dimensioned 2, the first value
controls the prior inflation and the second controls the posterior
inflation.

\

<div>

Item
Type
Description
single\_file\_in
logical
True means all ensemble members are read from a single NetCDF file.
False means each member is in a separate file. NOT SUPPORTED as of
March, 2017 only multiple files can be used.
input\_state\_files
character(len=256) dimension(MAXFILES)
A list of the NetCDF files to open to read the state vectors. Models
using multiple domains must put the domain and ensemble numbers in the
file names. The order and format of those is to be determined. NOT
SUPPORTED as of March, 2017.
input\_state\_file\_list
character(len=256) dimension(MAXFILES)
A list of files, one per domain. Each file must be a text file
containing the names of the NetCDF files to open, one per ensemble
member, one per line.
init\_time\_days
integer
If negative, don't use. If non-negative, override the initial days read
from state data restart files.
init\_time\_seconds
integer
If negative don't use. If non-negative, override the initial seconds
read from state data restart files.
perturb\_from\_single\_instance
logical
False means start from an ensemble-sized set of restart files. True
means perturb a single state vector from one restart file. This may be
done by model\_mod, if model\_mod provides subroutine
*pert\_model\_copies*.
perturbation\_amplitude
real(r8)
Standard deviation for the gaussian noise added when generating
perturbed ensemble members. Ignored if
*perturb\_from\_single\_instance = .false.* or the perturbed ensemble is
created in model\_mod.\
Random noise values drawn from a gaussian distribution with this
standard deviation will be added to the data in a single initial
ensemble member to generate the rest of the members.\
This option is more frequently used in the low order models and less
frequently used in large models. This is in part due to the different
scales of real geophysical variable values, and the resulting
inconsistencies between related field values. A more successful initial
condition generation strategy is to generate climatological
distributions from long model runs which have internally consistent
structures and values and then use observations with a 'spin-up' period
of assimilation to shape the initial states into a set of members with
enough spread and which match the current set of observations.
stages\_to\_write
character(len=10) dimension(4)
Controls diagnostic and restart output. Valid values are 'input',
'preassim', 'postassim', 'output', and 'null'.
single\_file\_out
logical
True means all ensemble members are written to a single NetCDF file.
False means each member is output in a separate file. NOT SUPPORTED as
of March, 2017 - only multiple files can be used.
output\_state\_files
character(len=256) dimension(MAXFILES)
A list of the NetCDF files to open for writing updated state vectors.
Models using multiple domains must put the domain and ensemble numbers
in the file names. The order and format of those is to be determined.
NOT SUPPORTED as of March, 2017.
output\_state\_file\_list
character(len=256) dimension(MAXFILES)
A list of files, one per domain. Each file must be a text file
containing the names of the NetCDF files to open, one per ensemble
member, one per line.
output\_interval
integer
Output state and observation diagnostics every 'N'th assimilation time,
N is output\_interval.
output\_members
logical
True means output the ensemble members in any stage that is enabled.
num\_output\_state\_members
integer
Number of ensemble members to be included in the state diagnostic output
for stages 'preassim' and 'postassim'. output\_members must be TRUE.
output\_mean
logical
True means output the ensemble mean in any stage that is enabled.
output\_sd
logical
True means output the ensemble standard deviation (spread) in any stage
that is enabled.
write\_all\_stages\_at\_end
logical
For most cases this should be .false. and data will be output as it is
generated for the 'preassim', 'postassim' diagnostics, and then restart
data will be output at the end. However, if I/O time dominates the
runtime, setting this to .true. will store the data and it can all be
written in parallel at the end of the execution. This will require
slightly more memory at runtime, but can lower the cost of the job
significantly in some cases.
compute\_posterior
logical
If .false., skip computing posterior forward operators and do not write
posterior values in the obs\_seq.final file. Saves time and memory.
Cannot enable posterior inflation and skip computing the posteriors. For
backwards compatibility the default for this is .true.
ens\_size
integer
Size of ensemble.
num\_groups
integer
Number of groups for hierarchical filter. It should evenly divide
ens\_size.
distributed\_state
logical
True means the ensemble data is distributed across all tasks as it is
read in, so a single task never has to have enough memory to store the
data for an ensemble member. Large models should always set this to
.true., while for small models it may be faster to set this to .false.
This is different from *&assim\_tools\_mod :: distributed\_mean* .
async
integer
Controls method for advancing model:
-   0 is subroutine call
-   2 is shell command
-   4 is mpi-job script

Ignored if filter is not controlling the model advance, e.g. in CESM
assimilations.
adv\_ens\_command
character(len=256)
Command sent to shell if async is 2.
tasks\_per\_model\_advance
integer
Number of tasks to assign to each ensemble member advance.
obs\_sequence\_in\_name
character(len=256)
File name from which to read an observation sequence.
obs\_sequence\_out\_name
character(len=256)
File name to which to write output observation sequence.
num\_output\_obs\_members
integer
Number of ensemble members to be included in the output observation
sequence file.
first\_obs\_days
integer
If negative, don't use. If non-negative, ignore all observations before
this time.
first\_obs\_seconds
integer
If negative, don't use. If non-negative, ignore all observations before
this time.
last\_obs\_days
integer
If negative, don't use. If non-negative, ignore all observations after
this time.
last\_obs\_seconds
integer
If negative, don't use. If non-negative, ignore all observations after
this time.
obs\_window\_days
integer
Assimilation window days; defaults to model timestep size.
obs\_window\_seconds
integer
Assimilation window seconds; defaults to model timestep size.
All variables named inf\_\* are arrays of length 2.\
The first element controls the prior inflation, the second element
controls the posterior inflation. See
[filter.html](../../programs/filter/filter.html#Inflation) for a
discussion of inflation and effective strategies.
inf\_flavor
integer array dimension(2)
Inflation flavor for \[prior, posterior\]
-   0 = none
-   2 = spatially-varying state-space (gaussian)
-   3 = spatially-fixed state-space (gaussian)
-   4 = Relaxation To Prior Spread (Posterior inflation only)
-   5 = enhanced spatially-varying state-space (inverse gamma)

(See inf\_sd\_initial below for how to set the time evolution options.)
inf\_initial\_from\_restart
logical array dimension(2)
If true, get initial mean values for inflation from restart file. If
false, use the corresponding namelist value *inf\_initial*.
inf\_sd\_initial\_from\_restart
logical array dimension(2)
If true, get initial standard deviation values for inflation from
restart file. If false, use the corresponding namelist value
*inf\_sd\_initial*.
inf\_deterministic
logical array dimension(2)
True means deterministic inflation, false means stochastic.
inf\_initial
real(r8) dimension(2)
Initial value of inflation if not read from restart file.
inf\_lower\_bound
real(r8) dimension(2)
Lower bound for inflation value.
inf\_upper\_bound
real(r8) dimension(2)
Upper bound for inflation value.
inf\_damping
real(r8) dimension(2)
Damping factor for inflation mean values. The difference between the
current inflation value and 1.0 is multiplied by this factor before the
next assimilation cycle. The value should be between 0.0 and 1.0.
Setting a value of 0.0 is full damping, which in fact turns all
inflation off by fixing the inflation value at 1.0. A value of 1.0 turns
inflation damping off leaving the original inflation value unchanged.
inf\_sd\_initial
real(r8) dimension(2)
Initial value of inflation standard deviation if not read from restart
file. If ≤ 0, do not update the inflation values, so they are
time-constant. If positive, the inflation values will adapt through
time, so they are time-varying.
inf\_sd\_lower\_bound
real(r8) dimension(2)
Lower bound for inflation standard deviation. If using a negative value
for *sd\_initial* this should also be negative to preserve the setting.
inf\_sd\_max\_change
real(r8) dimension(2)
For inflation type 5 (enhanced inflation), controls the maximum change
of the inflation standard deviation when adapting for the next
assimilation cycle. The value should be between 1.0 and 2.0. 1.0
prevents any changes, while 2.0 allows 100% change. For the enhanced
inflation option, if the standard deviation initial value is equal to
the standard deviation lower bound the standard deviation will not adapt
in time. See [this section](../../programs/filter/filter.html#Inflation)
for a discussion of how the standard deviation adapts based on different
types of inflation.
trace\_execution
logical
True means output very detailed messages about what routines are being
called in the main filter loop. Useful if a job hangs or otherwise
doesn't execute as expected.
output\_timestamps
logical
True means write timing information to the log before and after the
model advance and the observation assimilation phases.
output\_forward\_op\_errors
logical
True means output errors from forward observation operators. This is the
'istatus' error return code from the model\_interpolate routine. An
ascii text file *prior\_forward\_op\_errors* and/or
*post\_forward\_op\_errors* will be created in the current directory.
For each ensemble member which returns a non-zero return code, a line
will be written to this file. Each line will have three values listed:
the observation number, the ensemble member number, and the istatus
return code. Be cautious when turning this option on. The number of
lines in this file can be up to the number of observations times the
number of ensemble members times the number of assimilation cycles
performed. This option is generally most useful when run with a small
observation sequence file and a small number of ensemble members to
diagnose forward operator problems.
write\_obs\_every\_cycle
logical
For debug use; this option can significantly slow the execution of
filter. True means to write the entire output observation sequence
diagnostic file each time through the main filter loop even though only
observations with times up to and including the current model time will
have been assimilated. Unassimilated observations have the value
-888888.0 (the DART "missing value"). If filter crashes before finishing
it may help to see the forward operator values of observations that have
been assimilated so far.
silence
logical
True means output almost no runtime messages. Not recommended for
general use, but can speed long runs of the lower order models if the
execution time becomes dominated by the volume of output.

</div>

\
\

[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    types_mod
    obs_sequence_mod
    obs_def_mod
    obs_def_utilities_mod
    time_manager_mod
    utilities_mod
    assim_model_mod
    assim_tools_mod
    obs_model_mod
    ensemble_manager_mod
    adaptive_inflate_mod
    mpi_utilities_mod
    smoother_mod
    random_seq_mod
    state_vector_io_mod
    io_filenames_mod
    forward_operator_mod
    quality_control_mod

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

See the [filter overview](../../programs/filter/filter.html#FilesUsed)
for the list of files.

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
filter\_main
ens\_size in namelist is \#\#\#: Must be &gt; 1
Ensemble size must be at least 2.
filter\_main
inf\_flavor= \#\#\# Must be 0, 2, 3.
Observation Inflation is no longer supported (i.e flavor 1).
filter\_main
Posterior observation space inflation (type 1) not supported.
Posterior observation space inflation doesn't work.
filter\_main
Number of processes &gt; model size.
Number of processes can't exceed model size for now.
filter\_generate\_copy\_meta\_data
output metadata in filter needs state ensemble size &lt; 10000, not
\#\#\#.
Only up to 10000 ensemble members with state output for now.
filter\_generate\_copy\_meta\_data
output metadata in filter needs obs ensemble size &lt; 10000, not
\#\#\#.
Only up to 10000 ensemble members with obs space output for now.
filter\_setup\_obs\_sequence
input obs\_seq file has \#\#\# qc fields; must be &lt; 2.
Only 0 or 1 qc fields in input obs sequence for now.
get\_obs\_copy\_index
Did not find observation copy with metadata observation.
Only 0 or 1 qc fields in input obs sequence for now.

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

Many. New assimilation algorithms, support for new observation types,
support for additional models, better performance on higher numbers of
MPI tasks... The list is long. Send email to <dart@ucar.edu> if you are
interested in additional functionality in DART.

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

  ----------------- -----------------------------
  Contact:          DART core group
  Revision:         \$Revision\$
  Source:           \$URL\$
  Change Date:      \$Date\$
  Change History:   try "svn log" or "svn diff"
  ----------------- -----------------------------

