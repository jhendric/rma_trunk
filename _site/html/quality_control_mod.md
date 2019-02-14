[]{#TOP}

MODULE quality\_control\_mod
============================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[USAGE](#Usage%20Notes) / [NAMELIST](#Namelist) /
[DISCUSSION](#Discussion) / [INTERFACES](#Interface) /
[FILES](#FilesUsed) / [REFERENCES](#References) / [ERRORS](#Errors) /
[PLANS](#FuturePlans) / [PRIVATE COMPONENTS](#PrivateComponents) /
[TERMS OF USE](#Legalese)

Overview
--------

Routines in this module deal with two different types of quality control
(QC) related functions. The first is to support interpretation of the
*incoming* data quality, to reject observations at assimilation time
which are marked as poor quality. The second is to document how DART
disposed of each observation; whether it was successfully assimilated or
rejected, and if rejected, for which reason.

[]{#Usage Notes}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Usage
-----

#### Incoming Data Quality Control

DART currently supports a single incoming quality control scheme
compatible with NCEP usage. Lower values are considered better and
higher values are considered poorer. A single namelist item,
*input\_qc\_threshold* sets the boundary between accepted and rejected
observations. Values *larger* than this value are rejected; values equal
to or lower are accepted. Note that observations could be subsequently
rejected for other reasons, including failing the outlier threshold test
or all observations of this type being excluded by namelist control. See
the [obs\_kind\_mod](../observations/obs_kind_mod.html#Namelist)
namelist documentation for more details on how to enable or disable
assimilation by observation type at runtime.

The incoming quality control value is set when an observation sequence
file is created. If the data provider user a different scheme the values
must be translated into NCEP-consistent values. Generally we use the
value 3 for most runs.

Observations can also be rejected by the assimilation if the observation
value is too far from the mean of the ensemble of expected values (the
forward operator results). This is controlled by the
*outlier\_threshold* namelist item.

Specifically, the outlier test computes the difference between the
observation value and the prior ensemble mean. It then computes a
standard deviation by taking the square root of the sum of the
observation error variance and the prior ensemble variance for the
observation. If the difference between the ensemble mean and the
observation value is more than the specified number of standard
deviations then the observation is not used. This can be an effective
way to discard clearly erroneous observation values. A commonly used
value is 3. -1 disables this check.

There is an option to add code to this module to specialize the outlier
threshold routine. For example, it is possible to allow all observations
of one type to be assimilated regardless of the outlier value, and
enforce the outlier threshold only on other types of observations. To
enable this capability requires two actions: setting the
*enable\_special\_outlier\_code* namelist to *.TRUE.*, and adding your
custom code to the *failed\_outlier()* subroutine in this module.

#### DART Outgoing Quality Control

As DART assimilates each observation it adds a *DART Quality Control*
value to the output observation sequence (frequently written to a file
named *obs\_seq.final)*. This flag indicates how the observation was
used during the assimilation. The flag is a numeric value with the
following meanings:

  ---- ----------------------------------------------------------------------------------------------------------------------
  0:   Observation was assimilated successfully
  1:   Observation was evaluated only so not used in the assimilation
  2:   The observation was used but one or more of the posterior forward observation operators failed
  3:   The observation was evaluated only so not used AND one or more of the posterior forward observation operators failed
  4:   One or more prior forward observation operators failed so the observation was not used
  5:   The observation was not used because it was not selected in the namelist to be assimilated or evaluated
  6:   The incoming quality control value was larger than the threshold so the observation was not used
  7:   Outlier threshold test failed (as described above)
  8:   The location conversion to the vertical localization unit failed so the observation was not used
  ---- ----------------------------------------------------------------------------------------------------------------------

\
\
[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Namelist
--------

This namelist is read from the file *input.nml*. Namelists start with an
ampersand '&' and terminate with a slash '/'. Character strings that
contain a '/' must be enclosed in quotes to prevent them from
prematurely terminating the namelist.

<div class="namelist">

    &quality_control_nml
       input_qc_threshold          = 3
       outlier_threshold           = -1
       enable_special_outlier_code = .false.
      /

</div>

\
\

Items in this namelist control whether an observation is assimilated or
not.

<div>

  Item                             Type      Description
  -------------------------------- --------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  input\_qc\_threshold             integer   Numeric value indicating whether this observation is considered "good quality" and should be assimilated, or whether it is suspect because of previous quality control processes. This value would have been set when the observation was created and added to the observation sequence file. Observations with an incoming QC value larger than this threshold are rejected and not assimilated.
  outlier threshold                integer   This numeric value defines the maximum number of standard deviations an observation value can be away from the ensemble forward operator mean and still be assimilated. Setting it to the value -1 disables this check.
  enable\_special\_outlier\_code   logical   Setting this value to .TRUE. will call a subroutine *failed\_outlier()* instead of using the default code. The user can then customize the tests in this subroutine, for example to accept all observations of a particular type, or use different numerical thresholds for different observation types or locations.

</div>

\
\
[]{#Discussion}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Discussion
----------

#### Small ensemble spread

If an ensemble is spun up from a single state the ensemble spread may be
very small to begin and many observations may be rejected by the
*outlier\_threshold*. But as the ensemble spread increases the
assimilation should be able to assimilate more and more observations as
the model trajectory becomes consistent with those observations.

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    types_mod
    utilities_mod
    random_seq_mod

------------------------------------------------------------------------

Public Interfaces
-----------------

  ------------------------------------- -------------------------------------------------------
  *use quality\_control\_mod, only :*   [initialize\_qc](#initialize_qc)
                                        [input\_qc\_ok](#input_qc_ok)
                                        [get\_dart\_qc](#get_dart_qc)
                                        [check\_outlier\_threshold](#check_outlier_threshold)
                                        [good\_dart\_qc](#good_dart_qc)
                                        [set\_input\_qc](#set_input_qc)
                                        [dart\_flags](#dart_flags)
  ------------------------------------- -------------------------------------------------------

A note about documentation style. Optional arguments are enclosed in
brackets *\[like this\]*.

[]{#check_outlier_threshold}\

<div class="routine">

*call check\_outlier\_threshold(obs\_prior\_mean, obs\_prior\_var,
obs\_val, obs\_err\_var, & obs\_seq, this\_obs\_key, dart\_qc)*
    real(r8),                intent(in)    :: obs_prior_mean !>  prior observation mean
    real(r8),                intent(in)    :: obs_prior_var  !>  prior observation variance
    real(r8),                intent(in)    :: obs_val        !>  observation value
    real(r8),                intent(in)    :: obs_err_var    !>  observation error variance
    type(obs_sequence_type), intent(in)    :: obs_seq        !>  observation sequence
    integer,                 intent(in)    :: this_obs_key   !>  index for this observation
    integer,                 intent(inout) :: dart_qc        !>  possibly modified DART QC

</div>

<div class="indent1">

Computes whether this observation failed the outlier threshold test and
if so, updates the DART QC.

</div>

\
[]{#input_qc_ok}\

<div class="routine">

*var = input\_qc\_ok(input\_qc, qc\_to\_use)*
    real(r8), intent(in)  :: input_qc    !> incoming QC data value
    integer,  intent(out) :: qc_to_use   !> resulting DART QC
    logical               :: input_qc_ok !> true if input_qc is good

</div>

<div class="indent1">

Returns true if the input qc indicates this observation is good to use.

</div>

\
[]{#DART QC VALUES}\

<div class="routine">

    ! Dart quality control variables
    integer, parameter :: DARTQC_ASSIM_GOOD_FOP        = 0
    integer, parameter :: DARTQC_EVAL_GOOD_FOP         = 1
    integer, parameter :: DARTQC_ASSIM_FAILED_POST_FOP = 2
    integer, parameter :: DARTQC_EVAL_FAILED_POST_FOP  = 3
    integer, parameter :: DARTQC_FAILED_FOP            = 4
    integer, parameter :: DARTQC_NOT_IN_NAMELIST       = 5
    integer, parameter :: DARTQC_BAD_INCOMING_QC       = 6
    integer, parameter :: DARTQC_FAILED_OUTLIER_TEST   = 7
    integer, parameter :: DARTQC_FAILED_VERT_CONVERT   = 8
    !!integer, parameter :: DARTQC_OUTSIDE_DOMAIN        = 9  ! we have no way (yet) for the model_mod to signal this

</div>

<div class="indent1">

These are public constants for use in other parts of the DART code.

</div>

\
[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Files
-----

  filename    purpose
  ----------- --------------------------------------------
  input.nml   to read the quality\_control\_mod namelist

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

References
----------

1.  none

[]{#Errors}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Error Codes and Conditions
--------------------------

<div class="errors">

Routine
Message
Comment
routine name
output string
description or comment

</div>

KNOWN BUGS
----------

none at this time.

[]{#FuturePlans}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Future Plans
------------

Should support different incoming data QC schemes.

It would be nice to have a different DART QC flag for observations which
fail the forward operator because they are simply outside the model
domain. The diagnosic routines may indicate a large number of failed
forward operators which make it confusing to identify observations where
the forward operator should have been computed and can skew the
statistics. Unfortunately, this requires adding an additional
requirement on the model-dependent *model\_mod.f90* code in the
*model\_interpolate()* routine. The current interface defines a return
status code of 0 as success, any positive value as failure, and negative
numbers are reserved for other uses. To identify obs outside the domain
would require reserving another value that the interpolate routine could
return.

At this time the best suggestion is to cull out-of-domain obs from the
input observation sequence file by a preprocessing program before
assimilation.

[]{#PrivateComponents}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Private Components
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


