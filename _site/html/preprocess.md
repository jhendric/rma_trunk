[]{#TOP}

PROGRAM preprocess
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
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

Overview
--------

Preprocess is a DART-supplied preprocessor program which creates
observation kind and observation definition modules from a set of other
specially formatted Fortran 90 files. The output files are normally
named *assimilation\_code/modules/observations/obs\_kind\_mod.f90* and
*observations/forward\_operators/obs\_def\_mod.f90*, but can be renamed
by namelist control if needed.\
\
There are three kinds of input files:

1.  A default template for the obs\_kind\_mod (normally
    *assimilation\_code/modules/observations/DEFAULT\_obs\_kind\_mod.F90*).
2.  A default template of the obs\_def\_mod (normally
    *observations/forward\_operators/DEFAULT\_obs\_def\_mod.F90*).
3.  0 or more special obs\_def modules which contain observation
    specific types and code, such as
    *obs\_def\_mod\_reanalysis\_bufr\_mod.f90*. By convention these
    files are in the *obs\_def* directory.

The DEFAULT files contain specially formatted comment lines which are
used as markers to insert additional information into the templates and
create the output files.\
\
If one or more special obs\_def files are specified, information from
these files is incorporated into the output obs\_def and obs\_kind
modules and the observation types specified in the special obs\_def
files will be available for use by the filter programs.\
\
The special obs\_def files must contain a number of sections of
specially formatted F90 comments. They can also contain F90 code
required for implementing operations needed for the observations. These
sections are described in the documentation for the special obs\_def
modules, especially see *obs\_def\_1d\_state\_mod.f90*.\
\
The DEFAULT obs\_kind and obs\_def templates also require specially
commented sections to indicate where code from the special obs\_def
modules is to be inserted. See the documentation for the
DEFAULT\_obs\_kind\_mod.F90 and the DEFAULT\_obs\_def\_mod.F90 for
details.\
\
Although uncommon, it is possible to list no obs\_def modules in the
namelist, which results in *DEFAULT\_obs\_kind\_mod.F90* and
*DEFAULT\_obs\_def\_mod.F90* being copied directly to the final
obs\_kind and obs\_def modules. In this case, only identity observations
can be used by the filter (i.e. the state variable values are directly
observed; forward operator is an identity).

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

    &preprocess_nml
              overwrite_output = .true.,
        input_obs_def_mod_file = '../../../observations/forward_operators/DEFAULT_obs_def_mod.F90', 
       output_obs_def_mod_file = '../../../observations/forward_operators/obs_def_mod.f90', 
       input_obs_kind_mod_file = '../../../assimilation_code/modules/observations/DEFAULT_obs_kind_mod.F90', 
      output_obs_kind_mod_file = '../../../assimilation_code/modules/observations/obs_kind_mod.f90', 
                   input_files = '../../../observations/forward_operators/null
     /

</div>

\
\

<div>

  --------------------------------------------------------------------------------------------------------------------------
  Item                                     Type                                     Description
  ---------------------------------------- ---------------------------------------- ----------------------------------------
  input\_obs\_def\_mod\_file               character(len=129)\                      
                                           Path name of input obs definition module 
                                           to be preprocessed. Normally this is     
                                           *DEFAULT\_obs\_def\_mod.F90* in the      
                                           obs\_def directory. This file must have  
                                           the appropriate commented lines          
                                           indicating where the different parts of  
                                           the input special obs definition modules 
                                           are to be inserted.                      

  output\_obs\_def\_mod\_file              character(len=129)\                      
                                           Path name of output obs definition       
                                           module to be created by preprocessor.    
                                           Normally this is *obs\_def\_mod.f90* in  
                                           the obs\_def directory.                  

  input\_obs\_kind\_mod\_file              character(len=129)\                      
                                           Path name of input obs kind module to be 
                                           preprocessed. Normally this is           
                                           *DEFAULT\_obs\_kind\_mod.F90* in the     
                                           obs\_kind directory. This file must have 
                                           the appropriate commented lines          
                                           indicating where the different parts of  
                                           the input special obs definition modules 
                                           are to be inserted.                      

  output\_obs\_kind\_mod\_file             character(len=129)\                      
                                           Path name of output obs kind module to   
                                           be created by the preprocessor. Normally 
                                           this is *obs\_kind\_mod.F90* in the      
                                           obs\_kind directory.                     

  input\_files                             character(len=129)(:)\                   
                                           A list of up to max\_input\_files file   
                                           names that contain special format obs    
                                           definition files that are to be          
                                           incorporated into the preprocessed       
                                           obs\_kind\_mod.f90 and                   
                                           obs\_def\_mod.f90. The files must be in  
                                           the special obs definition format that   
                                           includes commented F90 lines delimitting 
                                           information about the observation        
                                           type(s). These files normally reside in  
                                           the obs\_def directory in files such as  
                                           *obs\_def\_reanalysis\_bufr\_mod.f90*.   

  overwrite\_output                        logical\                                 
                                           If set to .true., will allow preprocess  
                                           to overwrite existing obs\_kind\_mod.f90 
                                           and obs\_def\_mod.f90 files without      
                                           complaint. The default setting has       
                                           changed from .FALSE. to .TRUE., so by    
                                           default it will not complain about       
                                           existing files but will silently update  
                                           them in place.                           
  --------------------------------------------------------------------------------------------------------------------------

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

    utilities_mod

Namelist interface *&preprocess\_nml* must be read from file
*input.nml*.

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   input\_obs\_def\_mod\_file, specified by namelist; usually
    *DEFAULT\_obs\_def\_mod.F90*.
-   output\_obs\_def\_mod\_file, specified by namelist; usually
    *obs\_def\_mod.f90*.
-   input\_obs\_kind\_mod\_file, specified by namelist; usually
    *DEFAULT\_obs\_kind\_mod.F90*.
-   output\_obs\_kind\_mod\_file, specified by namelist; usually
    *obs\_kind\_mod.f90*.
-   input\_files, specified by namelist; usually files like
    *obs\_def\_reanalysis\_bufr\_mod.f90*.
-   namelistfile

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
preprocess
Namelist must provide input\_obs\_def\_mod\_file
Need an input obs\_def file to preprocess.
preprocess
Namelist must provide input\_obs\_kind\_mod\_file
Need an input obs\_kind file to preprocess.
preprocess
Namelist must provide output\_obs\_def\_mod\_file
Need an output obs\_def file to create.
preprocess
Namelist must provide output\_obs\_kind\_mod\_file
Need an output obs\_kind file to create.
preprocess
file \_\_\_\_ does not exist
The input obs\_def and obs\_kind files must exist.
preprocess
file \_\_\_\_\_\_ exists: Please Rename
The output obs\_kind and obs\_def files must not exist. Want to avoid
overwriting something important.
preprocess
input\_files \_\_\_\_\_\_\_ does NOT exist
All of the specified special obs\_def input modules must exist.
preprocess
file \_\_\_\_\_ does NOT contain ! BEGIN DART PREPROCESS KIND LIST
Each special obs\_def input file must contain this comment string.
preprocess
file \_\_\_\_\_ does NOT contain " END DART PREPROCESS KIND LIST
Each special obs\_def input file must contain this comment string.
preprocess
Input DEFAULT obs\_kind file ended unexpectedly.
Did not find strings indicating where to insert special obs\_def
sections in the input obs\_kind module.
preprocess
Input DEFAULT obs\_def file ended unexpectedly.
Did not find strings indicating where to insert special obs\_def
sections in the input obs\_def module.
preprocess
file \_\_\_\_\_ does NOT contain ! BEGIN DART PREPROCESS.
Input special obs\_def file must contain this comment string.
preprocess
file \_\_\_\_\_ does NOT contain ! END DART PREPROCESS.
Input special obs\_def file must contain this comment string.

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

  ------------------ -----------------------------
  Contact:           DART core group
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


