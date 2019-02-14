[]{#TOP}

MODULE obs\_model\_mod
======================

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

The code in this module computes the assimilation windows, and decides
if the model needs to run in order for the data to be at the appropriate
time to assimilate the next available observations. It also has the code
to write out the current states, advance the model (in a variety of
ways) and then read back in the updated states.

[]{#OtherModulesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    types_mod
    utilities_mod
    assim_model_mod
    obs_sequence_mod
    obs_def_mod
    time_manager_mod
    ensemble_manager_mod
    mpi_utilities_mod

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

  ------------------------------- ----------------------------------
  *use obs\_model\_mod, only :*   [advance\_state](#advance_state)
                                  [move\_ahead](#move_ahead)
  ------------------------------- ----------------------------------

[]{#move_ahead}\

<div class="routine">

*call move\_ahead(ens\_handle, ens\_size, seq, last\_key\_used,
window\_time, key\_bounds, num\_obs\_in\_set, curr\_ens\_time,
next\_ens\_time, trace\_messages)*
    type(ensemble_type),     intent(in)  :: ens_handle
    integer,                 intent(in)  :: ens_size
    type(obs_sequence_type), intent(in)  :: seq
    integer,                 intent(in)  :: last_key_used
    type(time_type),         intent(in)  :: window_time
    integer, dimension(2),   intent(out) :: key_bounds
    integer,                 intent(out) :: num_obs_in_set
    type(time_type),         intent(out) :: curr_ens_time
    type(time_type),         intent(out) :: next_ens_time
    logical, optional,       intent(in)  :: trace_messages

</div>

<div class="indent1">

Given an observation sequence and an ensemble, determines how to advance
the model so that the next set of observations can be assimilated. Also
returns the first and last keys and the number of observations to be
assimilated at this time. The algorithm implemented here (one might want
to have other variants) first finds the time of the next observation
that has not been assimilated at a previous time. It also determines the
time of the ensemble state vectors. It then uses information about the
model's time stepping capabilities to determine the time to which the
model can be advanced that is CLOSEST to the time of the next
observation. For now, this algorithm assumes that the model's timestep
is a constant. A window of width equal to the model timestep is centered
around the closest model time to the next observation and all
observations in this window are added to the set to be assimilated.\
\
Previous versions of this routine also made the call which actually
advanced the model before returning. This is no longer true. The routine
only determines the time stepping and number of observations. The
calling code must then call advance\_state() if indeed the next
observation to be assimilated is not within the current window. This is
determined by comparing the current ensemble time with the next ensemble
time. If equal no advance is needed. Otherwise, next ensemble time is
the target time for advance\_state().

  --------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *ens\_handle*         Identifies the model state ensemble
  *ens\_size*           Number of ensemble members
  *seq*                 An observation sequence
  *last\_key\_used*     Identifies the last observation from the sequence that has been used
  *window\_time*        Reserved for future use.
  *key\_bounds*         Returned lower and upper bound on observations to be used at this time
  *num\_obs\_in\_set*   Number of observations to be used at this time
  *curr\_ens\_time*     The time of the ensemble data passed into this routine.
  *next\_ens\_time*     The time the ensemble data should be advanced to. If equal to curr\_ens\_time, the model does not need to advance to assimilate the next observation.
  *trace\_messages*     Optional argument. By default, detailed time trace messages are disabled but can be turned on by passing this in as .True. . The messages will print the current window times, data time, next observation time, next window time, next data time, etc.
  --------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#advance_state}\

<div class="routine">

*call advance\_state(ens\_handle, ens\_size, target\_time, async,
adv\_ens\_command, tasks\_per\_model\_advance)*
    type(ensemble_type), intent(inout) :: ens_handle
    integer, intent(in)                :: ens_size
    type(time_type), intent(in)        :: target_time
    integer, intent(in)                :: async
    character(len=*), intent(in)       :: adv_ens_command
    integer, intent(in)                :: tasks_per_model_advance

</div>

<div class="indent1">

Advances all ensemble size copies of an ensemble stored in ens\_handle
to the target\_time. If async=0 this is done by repeated calls to the
`adv_1step()` subroutine. If async=2, a call to the shell with the
command *adv\_ens\_command* is used. If async=4, the filter program
synchronizes with the MPI job shell script using the `block_task()` and
`restart_task()` routines to suspend execution until all model advances
have completed. The script can start the model advances using MPI and
have it execute in parallel in this mode.

+-----------------------------------+-----------------------------------+
| *ens\_handle*                     | Structure for holding ensemble    |
|                                   | information and data              |
+-----------------------------------+-----------------------------------+
| *ens\_size*                       | Ensemble size.                    |
+-----------------------------------+-----------------------------------+
| *target\_time*                    | Time to which model is to be      |
|                                   | advanced.                         |
+-----------------------------------+-----------------------------------+
| *async*                           | How to advance model:             |
|                                   |   ------------------------------- |
|                                   | --------------------------------  |
|                                   |   0 = subroutine adv\_1step       |
|                                   |   2 = shell executes adv\_ens\_co |
|                                   | mmand                             |
|                                   |   4 = MPI job script advances mod |
|                                   | els and syncs with filter task    |
|                                   |   ------------------------------- |
|                                   | --------------------------------  |
+-----------------------------------+-----------------------------------+
| *adv\_ens\_command*               | Command to be issued to shell to  |
|                                   | advance model if async=2.         |
+-----------------------------------+-----------------------------------+
| *tasks\_per\_model\_advance   *   | Reserved for future use.          |
+-----------------------------------+-----------------------------------+

</div>

\
[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

This module does not have a namelist.

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

  filename                               purpose
  -------------------------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  assim\_model\_state\_ic*\#\#\#\#*      a binary representation of the state vector prepended by a small header consisting of the 'advance-to' time and the 'valid-time' of the state vector. The *\#\#\#\#* represents the ensemble member number if *&ensemble\_manager\_nml*: *single\_restart\_file\_out = .true.*.
  assim\_model\_state\_ud*\#\#\#\#   *   a binary representation of the state vector prepended by a small header consisting of the 'valid-time' of the state vector. This is the 'updated' model state (after the model has advanced the state to the desired 'advance-to' time).
  filter\_control*\#\#\#\#*              a text file containing information needed to advance the ensemble members; i.e., the ensemble member number, the input state vector file, the output state vector file - that sort of thing.

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

-   none

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
move\_ahead
next obs time not in model time window
Error in algorithm to compute observation window
advance\_state
target time \#\#\#,\#\#\# is before model\_time \#\#\#,\#\#\#
Target time must not be before current model time.
advance\_state
Trying to use \#\#\# model states -- too many. Use less than 10000
member ensemble.
Maximum of 9999 ensemble members is allowed.
advance\_state
Can only have 10000 processes.
No more than 9999 processes can run.
advance\_state
input.nml - async is \#, must be 0, or 2.
Only 0 or 2 work for async.

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


