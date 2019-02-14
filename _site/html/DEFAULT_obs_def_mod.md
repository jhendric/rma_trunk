[]{#TOP}

MODULE DEFAULT\_obs\_def\_mod
=============================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../documentation/images/ | Index](../../documentation/index. |
| Dartboard7.png){height="70"}      | html)\                            |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[TERMS OF USE](#Legalese)

Overview
--------

Beginning with the I-release of DART, a more flexible, powerful (and
complicated) mechanism for incorporating new types of observations is
part of DART. The DEFAULT\_obs\_def module being described here is used
by the program *preprocess* to create *obs\_def\_mod.f90*.\
\
To read more detailed instructions for adding your own new observation
types, see the documentation for [obs\_def\_mod](obs_def_mod.html). The
process does not involve altering this *DEFAULT\_obs\_def\_mod.F90*
file. Instead, new obs\_def files are specified as input to the
*preprocess* program by namelist, and a new *obs\_def\_mod.f90* file is
generated. The rest of the documentation here describes the internals of
how the *preprocess* program uses the template file to do the code
generation.\
\
Information from zero or more special obs\_def modules, such as
*obs\_def\_1d\_state\_mod.f90* or *obs\_def\_reanalyis\_bufr\_mod.f90*,
(also documented in this directory) are incorporated into the template
provided by DEFAULT\_obs\_def\_mod. If no special obs\_def files are
included in the preprocessor namelist, a minimal *obs\_def\_mod.f90* is
created which can only support identity forward observation operators.
Any identity observations on the obs\_seq.out file will be assimilated,
regardless of the obs types specified in assimilate\_these\_obs\_types.\
\
This documentation file only describes the special formatting that is
included in the DEFAULT\_obs\_def\_mod in order to guide the preprocess
program. Up to seven sections of code are inserted into the
DEFAULT\_obs\_def\_mod from each of the special obs\_def modules that
are requested. The insertion point for each section is denoted by a
special comment line that must be included VERBATIM in
*DEFAULT\_obs\_def\_mod.F90*. These special comment lines and their
significance are:

1.  ! DART PREPROCESS MODULE CODE INSERTED HERE\
    Some special observation definition modules (see for instance
    *obs\_def\_1d\_state\_mod.f90*) contain code for evaluating forward
    observation operators, reading or writing special information about
    an observation definition to an obs sequence file, or for
    interactive definition of an observation. The entire module code
    section is inserted here, so the resulting output file will be
    completely self-contained. Fortran 90 allows multiple modules to be
    defined in a single source file, and subsequent module code can use
    previously defined modules, so this statement must preceed the rest
    of the other comment lines.\
    \
2.  ! DART PREPROCESS USE FOR OBS\_QTY\_MOD INSERTED HERE\
    Integer identifiers with a unique integer value corresponding to
    each available observation quantity are defined in a table in the
    obs\_kind\_mod which is also created by the preprocessor. The use
    statements for these entries are inserted here.\
    \
3.  ! DART PREPROCESS USE OF SPECIAL OBS\_DEF MODULE INSERTED HERE\
    Some special observation definition modules (see for instance
    *obs\_def\_1d\_state\_mod.f90*) contain code for evaluating forward
    observation operators, reading or writing special information about
    an observation definition to an obs sequence file, or for
    interactive definition of an observation. The use statements for
    these routines from the special observation definition modules are
    inserted here.\
    \
4.  ! DART PREPROCESS GET\_EXPECTED\_OBS\_FROM\_DEF INSERTED HERE\
    Special observation definition modules must contain case statement
    code saying what to do to evaluate a forward observation operator
    for each observation type that they define. This code is inserted
    here.\
    \
5.  ! DART PREPROCESS READ\_OBS\_DEF INSERTED HERE\
    Special observation definition modules must contain case statement
    code saying what to do to read any additional information required
    for each observation type that they define from an observation
    sequence file. This code is inserted here.\
    \
6.  ! DART PREPROCESS WRITE\_OBS\_DEF INSERTED HERE\
    Special observation definition modules must contain case statement
    code saying what to do to write any additional information required
    for each observation type that they define to an observation
    sequence file. This code is inserted here.\
    \
7.  ! DART PREPROCESS INTERACTIVE\_OBS\_DEF INSERTED HERE\
    Special observation definition modules must contain case statement
    code saying what to do to interactively create any additional
    information required for each observation type that they define.
    This code is inserted here.\
    \

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


