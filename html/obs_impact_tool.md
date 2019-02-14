[]{#TOP}

PROGRAM *obs\_impact\_tool*
===========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [EXAMPLES](#Examples) / [MODULES](#Modules) /
[FILES](#FilesUsed) / [REFERENCES](#References) / [ERRORS](#Errors) /
[PLANS](#FuturePlans) / [TERMS OF USE](#Legalese) []{#Overview}

Overview
--------

Utility program which assists in constructing a table that is read by
filter at run time to alter how the assimilation of different types of
observations impact the state vector values based on their quantity, and
other observations based on their observation type or quantity. This
tool allows users to group related collections of observation types and
state vector quantities by name and then express the relationship of the
named groups to each other in a concise way.

At run time filter reads the output file from this tool and uses it to
control the impact at assimilation time based on the relationships
specified.

This version of this tool requires the last numeric column to be 0.0 or
1.0, but future extensions may allow different values to be used.

All the listed observation types and state vector quantities must be
known by the system. If they are not, look at the &preprocess\_nml ::
input\_items namelist which specifies which obs\_def\_xxx\_mod.f90 files
are included, which is where observation types are defined. Quantities
are defined in the obs\_kinds/DEFAULT\_obs\_kinds\_mod.F90 file and are
static. (Note you must add new quantities in 2 places if you do alter
this file.)

This program can define groups of observation types and then describe
the relationship of groups to groups. It can also define relationships
by exceptions.

Format of the input file can be any combination of these types of
sections:

<div>



    # hash mark starts a comment.

    # the GROUP keyword starts a group and must be followed
    # by a name.  All types or quantities listed before the END
    # line becomes members of this group.

    # GROUPs cannot contain nested groups.

    GROUP groupname1
     QTY_xxx  QTY_xxx  QTY_xxx
     QTY_xxx                          # comments can be here
    END GROUP

    GROUP groupname2
     QTY_xxx  
     QTY_xxx  
     QTY_xxx
     QTY_xxx
    END GROUP

    # GROUPs can also be defined by specifying ALL, ALLQTYS,
    # or ALLTYPES and then EXCEPT and listing the types or
    # quantities which should be removed from this group.
    # ALL EXCEPT must be the first line in a group, and all
    # subsequent items are removed from the list.
    # The items listed after EXCEPT can include the names
    # of other groups.

    GROUP groupnameM
    ALL EXCEPT QTY_xxx QTY_xxx
    QTY_xxx
    END GROUP

    GROUP groupnameN
    ALL EXCEPT groupnameY
    END GROUP


    # once any groups have been defined, a single instance
    # of the IMPACT table is specified by listing a TYPE,
    # QTY, or group in column 1, then a QTY or GROUP
    # in column 2 (the second name cannot be a specific type).
    # column 3 must be 0.0 or 1.0.  subsequent entries
    # that overlap previous entries have precedence
    # (last entry wins).

    IMPACT
     QTY_xxx    QTY_xxx     0.0
     QTY_xxx    groupname1   0.0
     groupname1  QTY_xxx     0.0
     groupname1  groupname1   0.0
    END IMPACT

</div>

Namelist interface [*&obs\_impact\_tool\_nml*](#Namelist) must be read
from file *input.nml*.

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

    &obs_impact_tool_nml
      input_filename          = 'cross_correlations.txt'
      output_filename         = 'control_impact_runtime.txt'
      allow_any_impact_values = .false.
      debug                   = .true.
      /

</div>

\
\

<div>

  Item                         Type                 Description
  ---------------------------- -------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  input\_filename              character(len=512)   Name of an ascii text file which describes how the interaction of observations to state vector values and observations to other observations should be controlled. See the [Overview](#Overview) section for details about the format of the input file entries.
  output\_filename             character(len=512)   Name of an ascii text file which created by this tool. It can be read at filter run time to control the impact of observations on state vector items and other observation values. The format of this file is set by this tool and should not be modified by hand. Rerun this tool to recreate the file.
  allow\_any\_impact\_values   logical              Recommended to stay false. This allows only 0.0 and 1.0 as the impact factors, effectively using the full increments or no increments during the assimilation. To experiment with partial application of the increments this flag can be set to true to allow other values.
  debug                        logical              If true print out debugging info.

</div>

\
\

[]{#Examples}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

EXAMPLES
--------

To prevent chemistry species from impacting the meterological variables
in the model state, and vice versa:

<div>

    GROUP chem
     QTY_CO QTY_NO QTY_C2H4
    END GROUP

    GROUP met
     ALLQTYS EXCEPT chem
    END GROUP

    IMPACT
     chem   met    0.0
     met    chem   0.0
    END IMPACT

</div>

[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    types_mod
    utilities_mod
    parse_args_mod

[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   two text files, one input and one output.
-   obs\_impact\_tool.nml

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
obs\_impact\_tool
Only use single process
Only a single mpi process can be used with this program
obs\_impact\_tool
cannot nest groups
Groups cannot contain other groups. You can exclude a group from another
group.
obs\_impact\_tool
Impact must be 0.0 or 1.0
Currently the impact can be either full or nothing. Contact the DART
developers if you want to experiment with other values.

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


