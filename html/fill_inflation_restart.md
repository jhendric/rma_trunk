[]{#TOP}

PROGRAM *fill\_inflation\_restart*
==================================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [FILES](#FilesUsed) / [REFERENCES](#References)
/ [ERRORS](#Errors) / [PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
--------

Utility program to create inflation restart files with constant values.
Useful for a multi-step assimilation, so the values
*inf\_initial\_from\_restart* and *inf\_sd\_initial\_from\_restart*
items in the &filter\_nml namelist can be .TRUE. from the beginning.

This reads values from the namelist, prior\_inf\_mean, prior\_inf\_sd
and/or post\_inf\_mean, post\_inf\_sd.

This program uses the information from the model\_mod code to determine
the number of items in the state vector. It must be compiled with the
right model's model\_mod, and if the items in the state vector are
selectable by namelist options, the namelist when running this program
must match exactly the namelist used during the assimilation run.

Alternatively you can use one of the standard NCO utilities like
"*ncap2*" on a copy of another restart file to set the initial inflation
mean, and another for the initial inflation standard deviation.
Inflation mean and sd values look exactly like restart values, arranged
by variable type like T, U, V, etc.

Depending on your version of the NCO utilities, you can use *ncap2* to
set the T,U and V inf values using one of two syntaxes:

<div class="unix">

      ncap2 -s 'T=1.0;U=1.0;V=1.0' wrfinput_d01 input_priorinf_mean.nc
      ncap2 -s 'T=0.6;U=0.6;V=0.6' wrfinput_d01 input_priorinf_sd.nc
      -or-
      ncap2 -s 'T(:,:,:)=1.0;U(:,:,:)=1.0;V(:,:,:)=1.0' wrfinput_d01 input_priorinf_mean.nc
      ncap2 -s 'T(:,:,:)=0.6;U(:,:,:)=0.6;V(:,:,:)=0.6' wrfinput_d01 input_priorinf_sd.nc

</div>

Some versions of the NCO utilities change the full 3D arrays into a
single scalar. If that's your result (check your output with
`ncdump -h`) use the alternate syntax or a more recent version of the
NCO tools.

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

    &fill_inflation_restart_nml
       input_state_files  = ''

       write_prior_inf    = .FALSE.
       prior_inf_mean     = -88888.8888
       prior_inf_sd       = -88888.8888

       write_post_inf     = .FALSE.
       prior_inf_mean     = -88888.8888
       prior_inf_sd       = -88888.8888

       single_file        = .FALSE.
       verbose            = .FALSE.
    /

</div>

The namelist controls how the logging, namelist, messages, and general
utility routines behave.

<div>

  Item                  Type           Description
  --------------------- -------------- --------------------------------------------------------------------------------------------------------------------------------------------------
  input\_state\_files   character(:)   A template file that contains all of the variables in the state vector. If there is multiple domains you need to provide a file for each domain.
  write\_prior\_inf     logical        Setting this to .TRUE. writes both the prior inflation mean and standard deviation file 'inflation\_prior\_mean', 'inflation\_prior\_sd'.
  prior\_inf\_mean      real(r8)       Prior inflation mean value.
  prior\_inf\_sd        real(r8)       Prior inflation standard deviation value.
  write\_post\_inf      logical        Setting this to .TRUE. writes both the posterior inflation mean and standard deviation file 'inflation\_post\_mean', 'inflation\_post\_sd'.
  post\_inf\_mean       real(r8)       Posterior inflation mean value.
  post\_inf\_sd         real(r8)       Posterior inflation standard deviation value.
  single\_file          logical        Currently not supported, but would be used in the case where you have a single restart file that contains all of the ensemble members.
  verbose               logical        Setting this to .TRUE. gives more output, and is generally used for debugging

</div>

\
\

Here is an example of a typical namelist for fill\_inflation\_restart.

<div class="namelist">

    &fill_inflation_restart_nml
       input_state_files  = 'caminput.nc'

       write_prior_inf    = .TRUE.
       prior_inf_mean     = 1.0
       prior_inf_sd       = 0.6

       write_post_inf     = .FALSE.
       prior_inf_mean     = -88888.8888
       prior_inf_sd       = -88888.8888

       single_file        = .FALSE.
       verbose            = .FALSE.
    /

</div>

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

inpute\_state\_files

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

[]{#Errors}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

ERROR CODES and CONDITIONS
--------------------------

Only works for models which have individual restart files and not the
'single\_file' format, where all the ensemble members are contained in
one file.

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

If requested we can implement the 'single\_file' version of
fill\_inflation\_restart.

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

  ------------------ ----------------------------------------------------------------------------------------------------------------------------------------------------------
  Contact:           DART core group
  Revision:          \$Revision\$
  Source:            \$URL: https://svn-dares-dart.cgd.ucar.edu/DART/branches/rma\_trunk/assimilation\_code/programs/fill\_inflation\_restart/fill\_inflation\_restart.html\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ ----------------------------------------------------------------------------------------------------------------------------------------------------------


