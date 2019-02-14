[]{#TOP}

PROGRM OR MODULE name\_of\_thing
================================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../images/Dartboard7.png | Index](../../index.html)\         |
| ){height="70"}                    | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [MODULES USED](#ModulesUsed) /
[INTERFACES](#Interface) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[PRIVATE COMPONENTS](#PrivateComponents) / [TERMS OF USE](#Legalese)

Overview
--------

Explain in general terms what this is.

[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

NAMELIST
--------

DART namelists are always read from file *input.nml*.

We adhere to the F90 standard of starting a namelist with an ampersand
'&' and terminating with a slash '/' for all our namelist input.
Character strings that contain a '/' must be enclosed in quotes to
prevent them from prematurely terminating the namelist.

<div class="namelist">

    &NAMELIST_NML 
       name=value,
       name=value, 
       name=value
    /

</div>

Any comments about the namelist as a whole.

\
\

<div>

Item
Type
Description
name
type
(often multi-line) description

</div>

\
[]{#ModulesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

MODULES USED
------------

    types_mod
    utilities_mod
    random_seq_mod
    time_manager_mod
    ensemble_manager_mod

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

*use this\_module\_name\_mod, only :*

[subr/function name](#tag)

 

[name2](#tag2)

 

[name3](#tag3)

A note about documentation style. Optional arguments are enclosed in
brackets *\[like this\]*.

[]{#subroutine1}\

<div class="routine">

*subroutine subroutine1(arg1, *\[, arg2\]*)*
    real(r8),           intent(in) :: arg1
    real(r8), optional, intent(in) :: arg2

</div>

<div class="indent1">

describe what this subroutine does.

*arg1*
Describe arg1.
*arg2*
Describe optional arg2.

</div>

\
[]{#function1}\

<div class="routine">

*function function1(arg1)*
    logical,             :: function1
    integer, intent(in)  :: arg1

</div>

<div class="indent1">

Describe function.

*function1*
describe what this function returns
*arg1*
describe function argument

</div>

\
[]{#bob_type}\

<div class="type">

    type bob_type
       private
       integer :: bob1
       integer :: bob2
    end type bob_type

</div>

<div class="indent1">

describe bob

Component
Description
bob1
Describe bob1.
bob2
Describe bob2.

</div>

\
[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

describe files used by code

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

-   author, title. [publication]{style="font-style: italic;"},
    [volume]{style="font-weight: bold;"}, pages.\
    [doi:
    nn.yyyy/rest\_of\_number](http://dx.doi.org/nn.yyyy/rest_of_number)\

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
subroutine\_name
error message text
what it means to the end user.

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

no discussion

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
  Change history:   try "svn log" or "svn diff"
  ----------------- -----------------------------


