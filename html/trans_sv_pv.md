[]{#TOP}

[MODULES](#Modules) / [NAMELIST](#Namelist) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[TERMS OF USE](#Legalese)

PROGRAM *trans\_sv\_pv*
=======================

\$Id\$

*trans\_sv\_pv* is responsible for converting a DART 'initial
conditions' file to a set of model 'snapshot' files and appropriate
namelist files: *data.cal* and *data*. This is easier than the reverse
process because the DART initial conditions file have a header that
contains the valid time for the accompanying state. This same header
also has the 'advance-to' time. *trans\_sv\_pv* uses this information to
write out appropriate *&CAL\_NML* and *&PARM03* namelists in
*data.cal.DART* and *data.DART*, respectively. The rest of the
information in *data* is preserved, so it is possible to simply replace
*data* with the new *data.DART*.\
\
The input filename is hardwired to that expected by *filter* and the
output filenames are able to be renamed into those defined by the
*data&PARM05* namelist specifying the filenames to use to cold-start the
ocean model. The output filename is comprised of 4 parts: the variable
name, the startDate\_1 component (YYYYMMDD), the startDate\_2 component
(HHMMSS), and the extension (.data for the data and .meta for the
metadata). The startDate\_1 and startDate\_2 pieces are identical in
format to that used by identically named variables in the
*data.cal&CAL\_NML* namelist.

Usage
-----

There must be several input files in the current working directory; most
of these are required by the *model\_mod* interface. The input filename
is hardwired to *assim\_model\_state\_ic*. Assuming the time tag in the
input file is set to 06Z 23 July 1996, this example creates output files
named\
*S.19960723.060000.\[data,meta\]*\
*T.19960723.060000.\[data,meta\]*\
*U.19960723.060000.\[data,meta\]*\
*V.19960723.060000.\[data,meta\]*\
*Eta.19960723.060000.\[data,meta\]*\
*data.cal.DART*, and\
*data.DART*

<div class="unix">

mv some\_DART\_ics\_input\_file assim\_model\_state\_ic\
./trans\_sv\_pv\
cp data.cal.DART data.cal\
cp data.DART data

</div>

\
[]{#Modules}

------------------------------------------------------------------------

MODULES USED
------------

    types_mod
    utilities_mod
    model_mod
    assim_model_mod
    time_manager_mod

[]{#Namelist}

------------------------------------------------------------------------

NAMELIST
--------

This program has no namelist of its own, but some of the underlying
modules require namelists to be read, even if the values are not used.
To avoid duplication and, possibly, some inconsistency in the
documentation; only a list of the required namelists is provided - with
a hyperlink to the full documentation for each namelist.

  Namelist                                                                                  Primary Purpose
  ----------------------------------------------------------------------------------------- --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  [utilities\_nml](../../assimilation_code/modules/utilities/utilities_mod.html#Namelist)   set the termination level and file name for the run-time log
  [CAL\_NML](model_mod.html#namelist_cal_nml)                                               must be read, values are not used. The *data.cal.DART* file has an updated namelist to be used for the model advance.
  [PARM03](model_mod.html#namelist_parm03)                                                  must be read, values are not used, The *data.DART* is an 'identical' version of *data* with the exception of the *PARM03* namelist. The parameters *endTime*, *dumpFreq*, and *taveFreq* reflect the amount of time needed to advance the model. The parameter *startTime* is set to 0.0, which is required to force the model to read the startup files specified by *PARM05*
  [PARM04](model_mod.html#namelist_parm04)                                                  ocean model grid parameters, read - never changed.

[]{#FilesUsed}

------------------------------------------------------------------------

FILES
-----

-   input namelist files: *data, data.cal, input.nml*
-   output namelist files: *data.cal.DART, data.DART*
-   input data file: *assim\_model\_state\_ic*
-   output data files: *\[S,T,U,V,Eta\].YYYYMMDD.HHMMSS.\[data,meta\]*

[]{#References}

------------------------------------------------------------------------

REFERENCES
----------

-   none

[]{#Errors}

------------------------------------------------------------------------

ERROR CODES and CONDITIONS
--------------------------

There are no error conditions specific to *trans\_sv\_pv*.

KNOWN BUGS
----------

There are no known bugs.

[]{#FuturePlans}

------------------------------------------------------------------------

FUTURE PLANS
------------

I may put in a routine to change the *PARM05* namelist parameters
defining the input file names. The hack in *advance\_model.csh* to grep
out the filenames from the namelist and rename the files at the shell
level is ugly.\
\
Feel free to suggest improvements.

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


