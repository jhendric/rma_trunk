[]{#TOP}

PROGRAM *perturb\_single\_instance*
===================================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id:                             |
|                                   | perturb\_single\_instance.html    |
|                                   | 12663 2018-06-12 21:55:34Z        |
|                                   | nancy@ucar.edu \$                 |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [MODULES](#Modules) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

Overview
--------

Utility program to generate an ensemble of perturbed ensemble member
restart files. This program can be run in parallel and used as a stand
alone program.

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

    &perturb_single_instance
       ens_size               = ''
       input_files            = ''      
       output_files           = ''
       output_file_list       = ''
       perturbation_amplitude = 0.0     
       single_restart_file_in = .false.      
      /

</div>

<div>

  Item                        Type                                         Description
  --------------------------- -------------------------------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ens\_size                   integer                                      Total number of ensemble members.
  input\_files                character(len=256),dimension(num\_domains)   The restart file you would like to perturb from.
  output\_file\_list          character(len=256)                           A file containing a list of the desired output names.
  output\_files               character(len=256)                           An array of filenames
  perturbation\_amplitude     real(r8)                                     The desired perturbation amplitude. If the model provides an interface then it will use that subroutine, otherwise it will simply add gaussian noise to the entire state, and this is the standard deviation.
  single\_restart\_file\_in   logical                                      A boolean, specifying if you have a single file restart, such as the case for lower order models.

</div>

Below is an example of a typical namelist for the
perturb\_single\_instance.

<div class="namelist">

    &perturb_single_instance_nml
       ens_size         = 3
       input_files      = 'caminput.nc'
       output_files     = 'cam_pert1.nc','cam_pert2.nc','cam_pert3.nc'
    /

</div>

\
\
\
\
[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   inputfile.nc (description file that will be perturbed)
-   output\_file\_list.txt (a file containing a list of restart files)
    and,
-   perturb\_single\_instance.nml

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
perturb\_single\_instance
Invalid method number
Values 1-4 are supported

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

none

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

  ------------------ ----------------------------------------------------------------------------------------------------------------------------------------------------------------
  Contact:           DART core group
  Revision:          \$Revision: 12663 \$
  Source:            \$URL: https://svn-dares-dart.cgd.ucar.edu/DART/branches/pertirb\_tool/assimilation\_code/programs/perturb\_single\_instance/perturb\_single\_instance.html \$
  Change Date:       \$Date: 2018-06-12 15:55:34 -0600 (Tue, 12 Jun 2018) \$
  Change history:    try "svn log" or "svn diff"
  ------------------ ----------------------------------------------------------------------------------------------------------------------------------------------------------------


