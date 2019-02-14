[]{#TOP}

MODULE *obs\_kind\_mod*
=======================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [MODULES](#Modules) / [INTERFACES](#Interface) /
[FILES](#FilesUsed) / [REFERENCES](#References) / [ERRORS](#Errors) /
[PLANS](#FuturePlans) / [TERMS OF USE](#Legalese)

Overview
--------

#### Introduction

This module provides definitions of specific observation types and
generic variable quantities, routines for mapping between integer
identifiers and string names, routines for reading and writing this
information, and routines for determining whether and how to process
observations from an observation sequence file.

The distinction between quantities and types is this: *Quantities* apply
both to observations and to state vector variables. Knowing the QTY\_xxx
of an observation must be sufficient to compute the correct forward
operator. The quantities also must be able to identify the different
variables in the state vector. *Types* only apply to observations, and
are usually observation-platform dependent. Making distinctions between
different observation sources by using different types allows users to
selectively assimilate, evaluate, or ignore them.

#### Examples and Use

Generic quantities are associated with an observation type or with a
model state variable. An example quantity is *QTY\_U\_WIND\_COMPONENT*.
Multiple different specific observation types can be associated with
this generic quantity, for instance *RADIOSONDE\_U\_WIND\_COMPONENT*,
*ACARS\_U\_WIND\_COMPONENT*, and *SAT\_U\_WIND\_COMPONENT*. Generic
quantities are defined via an integer parameter statement at the start
of this module. As new generic quantities are needed they are added to
this list. Generic quantity integer parameters are required to start
with *QTY\_* and observation types are NOT allowed to start with
*QTY\_*.

Typically quantities are used by model-interface files
*models/xx/model\_mod.f90*, observation forward operator files
*observations/forward\_operators/obs\_def\_xx\_mod.f90*, and observation
converter programs *observations/obs\_converters/xx/xx.f90*.

The obs\_kind module being described here is created by the program
*preprocess* from two categories of input files. First, a DEFAULT
obs\_kind module (normally called *DEFAULT\_obs\_kind\_mod.F90* and
documented in this directory) is used as a template into which the
preprocessor incorporates information from zero or more special obs\_def
modules (such as *obs\_def\_1d\_state\_mod.f90* or
*obs\_def\_reanalysis\_bufr\_mod.f90*) which are documented in the
obs\_def directory. If no special obs\_def files are included in the
preprocessor namelist, a minimal *obs\_kind\_mod.f90* is created which
can only support identity forward observation operators.

When making code changes remember to edit only the template file
(normally *DEFAULT\_obs\_kind\_mod.F90*) and not the
*obs\_kind\_mod.f90* file directly. All of the build scripts in DART
remove the existing obs\_kind and obs\_def modules and regenerate them
using the *preprocess* program and any direct changes will be lost.

#### Adding Additional Quantities

To add an additional quantity, edit the
*assimilation\_code/modules/observations/DEFAULT\_obs\_kind\_mod.F90*
file and add a unique number to the list of generic quantities at the
top of the file. Then find the *obs\_kind\_names* initializer statements
in the *initialize\_module()* subroutine, and add an entry with the same
index to define the string associated with the parameter. The names must
be identical. If the number is larger than the current maximum, increase
the value of *max\_defined\_quantities*. Run the *preprocess* program to
generate the
*assimilation\_code/modules/observations/obs\_kind\_mod.f90* file which
will then be used by the rest of the DART system.

#### Implementation Details

The obs\_kind module contains an automatically-generated list of integer
parameters, derived from the obs\_def files, an integer parameter
*max\_defined\_types\_of\_obs*, and an automatically-generated list of
initializers for the *obs\_type\_type* derived type that defines the
details of each observation type that has been created by the preprocess
program. Each entry contains the integer index of the observation type,
the string name of the observation type (which is identical to the F90
identifier), the integer index of the associated generic quantities, and
three logicals indicating whether this observation type is to be
assimilated, evaluated only (forward operator is computed but not
assimilated), assimilated but has externally computed forward operator
values in the input obsesrvation sequence file, or ignored entirely. The
logicals initially default to .false. and are set to .true. via the
*&obs\_kind\_nml* namelist. A second derived type *obs\_kind\_type* maps
generic quantity parameters to the equivalent string names.

\
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

    &obs_kind_nml
       assimilate_these_obs_types          = 'null',
       evaluate_these_obs_types            = 'null'  
       use_precomputed_FOs_these_obs_types = 'null'
     /

</div>

\
\

Controls what observation types are to be assimilated, evaluated, or
ignored. For each entry, a list of observation type names can be
specified. Any name in the obs\_type\_type table is eligible. Specifying
a name that is not in the table results in an error. Specifying the same
name for both namelist entries also results in an error. Observation
types specified in the list for assimilate\_these\_obs\_types are
assimilated. Those in the evaluate\_these\_obs\_types list have their
forward operators computed and included in diagnostic files but are not
assimilated. An observation type that is specified in neither list is
ignored. Identity observations, however, are always assimilated if
present in the obs\_seq.out file.

<div>

  Item                                       Type                              Description
  ------------------------------------------ --------------------------------- --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  assimilate\_these\_obs\_types              character(len=31), dimension(:)   Names of observation types to be assimilated.
  evaluate\_these\_obs\_types                character(len=31), dimension(:)   Names of observation types to be evaluated only.
  use\_precomputed\_FOs\_these\_obs\_types   character(len=31), dimension(:)   If the forward operator values have been precomputed outside of filter, for example for radiances or other compute intensive computations, the ensemble of forward operator values can be stored in the observation sequence file. For any type listed here, the forward operator interpolation code will not be called and the values in the file will be used instead.

</div>

For example:

    &obs_kind_nml
       assimilate_these_obs_types = 'RADIOSONDE_TEMPERATURE',
                                    'RADIOSONDE_U_WIND_COMPONENT',
                                    'RADIOSONDE_V_WIND_COMPONENT',
       evaluate_these_obs_types   = 'RADIOSONDE_SURFACE_PRESSURE', 
      use_precomputed_FOs_these_obs_types = 'RADIANCE'
    /

would assimilate temperature and wind observations, but only compute the
forward operators for surface pressure obs. Radiance observations have
precomputed values for each ensemble member in the input observation
sequence file which would be used instead of calling the forward
operator code.\
\
[]{#Modules}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    utilities_mod

\
[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

  ----------------------------- --------------------------------------------------------------------
  *use obs\_def\_mod, only :*   [max\_defined\_types\_of\_obs](#max_defined_types_of_obs)
                                [get\_num\_types\_of\_obs](#get_num_types_of_obs)
                                [get\_num\_quantities](#get_num_quantities)
                                [get\_name\_for\_type\_of\_obs](#get_name_for_type_of_obs)
                                [get\_name\_for\_quantity](#get_name_for_quantity)
                                [get\_index\_for\_type\_of\_obs](#get_index_for_type_of_obs)
                                [get\_index\_for\_quantity](#get_index_for_quantity)
                                [assimilate\_this\_type\_of\_obs](#assimilate_this_type_of_obs)
                                [evaluate\_this\_type\_of\_obs](#evaluate_this_type_of_obs)
                                [get\_quantity\_for\_type\_of\_obs](#get_quantity_for_type_of_obs)
                                [write\_type\_of\_obs\_table](#write_type_of_obs_table)
                                [read\_type\_of\_obs\_table](#read_type_of_obs_table)
                                [get\_type\_of\_obs\_from\_menu](#get_type_of_obs_from_menu)
                                [map\_type\_of\_obs\_table](#map_type_of_obs_table)
                                [paramname\_length](#paramname_length)
                                [GENERIC\_QTY\_DEFINITIONS](#GENERIC_QTY_DEFINITIONS)
                                [OBSERVATION\_TYPES](#OBSERVATION_TYPES)
  ----------------------------- --------------------------------------------------------------------

A note about documentation style. Optional arguments are enclosed in
brackets *\[like this\]*.

[]{#max_defined_types_of_obs}\

<div class="routine">

*integer, parameter :: max\_defined\_types\_of\_obs*

</div>

<div class="indent1">

The total number of available observation types in the obs\_type\_type
table. This value is added by the preprocess program and depends on
which *obs\_def\_xxx\_mod.f90* files are listed in the
[&preprocess\_nml](../../programs/preprocess/preprocess.html#Namelist)
namelist. (Note that in spite of the name, this is the number of
specific types, e.g. RADIOSONDE\_TEMPERATURE,
AIRCRAFT\_SPECIFIC\_HUMIDITY) and not the number of generic quantities.

There is also a function interface which is an alternate method to get
this value. In some cases the code requires a parameter value known at
compile time (for declaring a fixed length array, for example). For an
array allocated at run time the size can be returned by the function
interface.

</div>

\
[]{#get_num_types_of_obs}\

<div class="routine">

*var = get\_num\_types\_of\_obs()*
    integer :: get_num_types_of_obs

</div>

<div class="indent1">

Returns the number of different specific observation types (e.g.
RADIOSONDE\_TEMPERATURE, AIRCRAFT\_SPECIFIC\_HUMIDITY) defined in the
obs\_kind\_mod.f90 file. This file is generated by the preprocess
program. This is the same value as the public
'max\_defined\_types\_of\_obs' above.

  ------- ---------------------------------------------------------------------------------------------
  *var*   Integer count of the total number of specific types defined in the obs\_kind\_mod.f90 file.
  ------- ---------------------------------------------------------------------------------------------

</div>

\
[]{#get_num_quantities}\

<div class="routine">

*var = get\_num\_quantities()*
    integer :: get_num_quantities

</div>

<div class="indent1">

Returns the number of different generic quantities (e.g.
QTY\_TEMPERATURE, QTY\_SPECIFIC\_HUMIDITY) defined in the
obs\_kind\_mod.f90 file. This file is generated by the preprocess
program.

  ------- -------------------------------------------------------------------------------------------------
  *var*   Integer count of the total number of generic quantities defined in the obs\_kind\_mod.f90 file.
  ------- -------------------------------------------------------------------------------------------------

</div>

\
[]{#get_name_for_type_of_obs}\

<div class="routine">

*var = get\_name\_for\_type\_of\_obs(obs\_kind\_ind)*
    character(len=32)              :: get_name_for_type_of_obs
    integer, intent(in)            :: obs_kind_ind

</div>

<div class="indent1">

Given an integer index return the string name of the corresponding
specific observation type (e.g. "RADIOSONDE\_TEMPERATURE",
"AIRCRAFT\_SPECIFIC\_HUMIDITY"). This string is the same as the F90
identifier associated with the integer index.

  ------------------ ----------------------------------------------------------------------
  *var*              Name string associated with this entry in the obs\_type\_type table.
  *obs\_kind\_ind*   An integer index into the obs\_type\_type table.
  ------------------ ----------------------------------------------------------------------

</div>

\
[]{#get_name_for_quantity}\

<div class="routine">

*var = get\_name\_for\_quantity(obs\_kind\_ind)*
    character(len=32)              :: get_name_for_quantity
    integer, intent(in)            :: obs_kind_ind

</div>

<div class="indent1">

Given an integer index return the string name of the corresponding
generic quantity (e.g. "QTY\_TEMPERATURE", "QTY\_SPECIFIC\_HUMIDITY").
This string is the same as the F90 identifier associated with the
integer index.

  ------------------ ----------------------------------------------------------------------
  *var*              Name string associated with this entry in the obs\_kind\_type table.
  *obs\_kind\_ind*   An integer index into the obs\_kind\_type table.
  ------------------ ----------------------------------------------------------------------

</div>

\
[]{#get_index_for_type_of_obs}\

<div class="routine">

*var = get\_index\_for\_type\_of\_obs(obs\_kind\_name)*
    integer                       :: get_index_for_type_of_obs
    character(len=32), intent(in) :: obs_kind_name

</div>

<div class="indent1">

Given the name of a specific observation type (e.g.
"RADIOSONDE\_TEMPERATURE", "AIRCRAFT\_SPECIFIC\_HUMIDITY"), returns the
index of the entry in the obs\_type\_type table with this name. If the
name is not found in the table, a -1 is returned. The integer returned
for a successful search is the value of the integer parameter with the
same identifier as the name string.

  ---------------------------------- -------------------------------------------------------------------------------------------------------
  *get\_index\_for\_type\_of\_obs*   Integer index into the obs\_type\_type table entry with name string corresponding to obs\_kind\_name.
  *obs\_kind\_name*                  Name of specific observation type found in obs\_type\_type table.
  ---------------------------------- -------------------------------------------------------------------------------------------------------

</div>

\
[]{#get_index_for_quantity}\

<div class="routine">

*var = get\_index\_for\_quantity(obs\_kind\_name)*
    integer                       :: get_index_for_quantity
    character(len=32), intent(in) :: obs_kind_name

</div>

<div class="indent1">

Given the name of a generic quantity (e.g. "QTY\_TEMPERATURE",
"QTY\_SPECIFIC\_HUMIDITY"), returns the index of the entry in the
obs\_kind\_type table with this name. If the name is not found in the
table, a -1 is returned. The integer returned for a successful search is
the value of the integer parameter with the same identifier as the name
string.

  ----------------------------- -------------------------------------------------------------------------------------------------------
  *get\_index\_for\_quantity*   Integer index into the obs\_kind\_type table entry with name string corresponding to obs\_kind\_name.
  *obs\_kind\_name*             Name of generic kind found in obs\_kind\_type table.
  ----------------------------- -------------------------------------------------------------------------------------------------------

</div>

\
[]{#assimilate_this_type_of_obs}\

<div class="routine">

*var = assimilate\_this\_type\_of\_obs(obs\_kind\_ind)*
    logical              :: assimilate_this_type_of_obs
    integer, intent(in)  :: obs_kind_ind

</div>

<div class="indent1">

Given the integer index associated with a specific observation type
(e.g. RADIOSONDE\_TEMPERATURE, AIRCRAFT\_SPECIFIC\_HUMIDITY), return
true if this observation type is to be assimilated, otherwise false. The
parameter defined by this name is used as an integer index into the
obs\_type\_type table to return the status of this type.

  ------------------ -------------------------------------------------------------------------------
  *var*              Returns true if this entry in the obs\_type\_type table is to be assimilated.
  *obs\_kind\_ind*   An integer index into the obs\_type\_type table.
  ------------------ -------------------------------------------------------------------------------

</div>

\
[]{#evaluate_this_type_of_obs}\

<div class="routine">

*var = evaluate\_this\_type\_of\_obs(obs\_kind\_ind)*
    logical              :: evaluate_this_type_of_obs
    integer, intent(in)  :: obs_kind_ind

</div>

<div class="indent1">

Given the integer index associated with a specific observation type
(e.g. RADIOSONDE\_TEMPERATURE, AIRCRAFT\_SPECIFIC\_HUMIDITY), return
true if this observation type is to be evaluated only, otherwise false.
The parameter defined by this name is used as an integer index into the
obs\_type\_type table to return the status of this type.

  ------------------------------------------------------------------------
  *var*
  Returns true if this entry in the obs\_type\_type table is to be
  evaluated.

  *obs\_kind\_ind*
  ------------------------------------------------------------------------

</div>

\
[]{#get_quantity_for_type_of_obs}\

<div class="routine">

*var = get\_quantity\_for\_type\_of\_obs(obs\_kind\_ind)*
    integer              :: get_quantity_for_type_of_obs
    integer, intent(in)  :: obs_kind_ind

</div>

<div class="indent1">

Given the integer index associated with a specific observation type
(e.g. RADIOSONDE\_TEMPERATURE, AIRCRAFT\_SPECIFIC\_HUMIDITY), return the
generic quantity associated with this type (e.g. QTY\_TEMPERATURE,
QTY\_SPECIFIC\_HUMIDITY). The parameter defined by this name is used as
an integer index into the obs\_type\_type table to return the generic
quantity associated with this type.

  ------------------------------------------------------------------------
  *var*
  Returns the integer GENERIC quantity index associated with this obs
  type.

  *obs\_kind\_ind*
  ------------------------------------------------------------------------

</div>

\
[]{#write_type_of_obs_table}\

<div class="routine">

*call write\_type\_of\_obs\_table(ifile *\[, fform, use\_list\]*)*
    integer,                    intent(in) :: ifile
    character(len=*), optional, intent(in) :: fform
    integer,          optional, intent(in) :: use_list(:)

</div>

<div class="indent1">

Writes out information about all defined observation types from the
obs\_type\_type table. For each entry in the table, the integer index of
the observation type and the associated string are written. These appear
in the header of an obs\_sequence file. If given, the *use\_list(:)*
must be the same length as the max\_obs\_specific count. If greater than
0, the corresponding index will be written out; if 0 this entry is
skipped. This allows a table of contents to be written which only
includes those types actually being used.

  ---------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *ifile*          Unit number of output observation sequence file being written.
  *fform*          Optional format for file. Default is FORMATTED.
  *use\_list(:)*   Optional integer array the same length as the number of specific types (from get\_num\_types\_of\_obs() or the public max\_defined\_types\_of\_obs). If value is larger than 0, the corresponding type information will be written out. If 0, it will be skipped. If this argument is not specified, all values will be written.
  ---------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#read_type_of_obs_table}\

<div class="routine">

*call read\_type\_of\_obs\_table(ifile, pre\_I\_format *\[, fform\]*)*
    integer,                    intent(in) :: ifile
    logical,                    intent(in) :: pre_I_format !(deprecated)
    character(len=*), optional, intent(in) :: fform

</div>

<div class="indent1">

Reads the mapping between integer indices and observation type names
from the header of an observation sequence file and prepares mapping to
convert these to values defined in the obs\_type\_type table. If
pre\_I\_format is true, there is no header in the observation sequence
file and it is assumed that the integer indices for observation types in
the file correspond to the storage order of the obs\_type\_type table
(integer index 1 in the file corresponds to the first table entry, etc.)
Support for pre\_I\_format is deprecated and may be dropped in future
releases of DART.

  ------------------ -----------------------------------------------------------------------------
  *ifile*            Unit number of output observation sequence file being written.
  *pre\_I\_format*   True if the file being read has no obs type definition header (deprecated).
  *fform*            Optional format for file. Default is FORMATTED.
  ------------------ -----------------------------------------------------------------------------

</div>

\
[]{#get_type_of_obs_from_menu}\

<div class="routine">

*var = get\_type\_of\_obs\_from\_menu()*
    integer              :: get_type_of_obs_from_menu

</div>

<div class="indent1">

Interactive input of observation type. Prompts user with list of
available types and validates entry before returning.

  ------- ------------------------------------
  *var*   Integer index of observation type.
  ------- ------------------------------------

</div>

\
[]{#map_type_of_obs_table}\

<div class="routine">

*var = map\_type\_of\_obs\_table(obs\_def\_index)*
    integer              :: map_type_of_obs_table
    integer, intent(in)  :: obs_def_index

</div>

<div class="indent1">

Maps from the integer observation type index in the header block of an
input observation sequence file into the corresponding entry in the
obs\_type\_type table. This allows observation sequences that were
created with different obs\_kind\_mod.f90 versions to be used with the
current obs\_kind\_mod.

  ------------------- -----------------------------------------------------------------
  *var*               Index of this observation type in obs\_type\_type table.
  *obs\_def\_index*   Index of observation type from input observation sequence file.
  ------------------- -----------------------------------------------------------------

</div>

\
[]{#paramname_length}\

<div class="routine">

*integer, parameter :: paramname\_length*

</div>

<div class="indent1">

The current Fortran standards define the maximum length of a Fortran
parameter to be 32 chars long. This named parameter can be used anyplace
the string name length of a parameter is needed. All the specific types
and generic quantities in DART are defined both as an integer parameter
number for efficiency, and as a string variable when a descriptive or
human-readable name is needed. The length of these names cannot exceed
this parameter-name limit.

</div>

\
[]{#GENERIC_QTY_DEFINITIONS}\

<div class="routine">

*integer, parameter :: QTY\_.....*

</div>

<div class="indent1">

All generic quantities available are public parameters that begin with
QTY\_.

</div>

\
[]{#OBSERVATION_TYPES}\

<div class="routine">

*integer, parameter :: SAMPLE\_OBS\_TYPE*

</div>

<div class="indent1">

A list of all observation types that are available is provided as a set
of integer parameter statements. The F90 identifiers are the same as the
string names that are associated with this identifier in the
obs\_type\_type table.

</div>

\
[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   &obs\_kind\_nml in input.nml
-   Files containing input or output observation sequences.

\
[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

-   none

\
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
initialize\_module
\_\_\_\_\_\_ from obs\_kind\_nml is not a legal observation kind
An observation type name that is not in the obs\_type\_type table has
been specified to be assimilated or evaluted.
initialize\_module
Illegal to evaluate and assimilate the same kind \_\_\_\_\_\_
The same observation type name has been specified in both namelist
entries.
map\_type\_of\_obs\_table
Couldnt find obs\_def\_index \_\_ in obs\_kind map.
An attempt to use an observation type that was NOT in the obs\_sequence
header.
read\_type\_of\_obs\_table
Didnt find obs\_kind\_definition string
An obs\_sequence file that was expected to contain an
obs\_kind\_definition list in its header did not.
read\_type\_of\_obs\_table
didnt find observation kind \_\_\_\_\_ in obs\_kind\_mod list
An observation type specified by name in an observation sequence file
header was NOT found in the obs\_type\_type table.

</div>

\
[]{#FuturePlans}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FUTURE PLANS
------------

The terminology has evolved since the initial version of this code was
developed, but since the names are part of the public interfaces they
have remained constant and are now confusing. The current system makes a
definite distinction between specific observation *types* versus generic
state variable and observation *quantities*. However, the interfaces to
the code in this module are still using 'kind' where they should be
using 'type'. As a byproduct of the interface name confusion there is
additional confusion inside other parts of the DART code where 'kind' is
used for local variable names where it really is a 'type'.

As noted in the Overview, the real distinction is that knowing the
QTY\_xxx of an observation should be enough to compute the correct
forward operator. The kinds also should be sufficient to identify the
different variables in the state vector. The types are
observation-platform dependent and allow users to select to assimilate
and evaluate these as they choose.

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


