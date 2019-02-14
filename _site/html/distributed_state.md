[]{#TOP}

Distributed State
=================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../images/Dartboard7.png){h | Index](../index.html)\            |
| eight="70"}                       | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

The key part of RMA DART is having a state that is physically
distributed across processors. The location in memory of any part of the
state vector (which processor and where in memory on that processor) is
completely under the control of filter, not model\_mod.

Implications of this:

-   The model\_mod never gets a whole state vector to use. So no whole
    vector for a forward operator, and no whole vector for the mean.
-   The model\_mod can not make any assumptions about the order of
    elements in the state.\
    Currently, filter is ordering variables in the order they are listed
    in add\_domain and with the dimenion order of the netcdf file. This
    is what is happening in most model\_mod converters (model\_to\_dart,
    dart\_to\_model). However CAM and bgrid\_solo rearrange the state in
    Lanai. These model\_mods (and converters) have been changed to not
    rearrage the state.

So, how does the model\_mod access the state without having the vector
and not knowing the state order? - state accessor routines.

#### State Accessor Routines

##### Getting the dart index

function get\_dart\_vector\_index(i, j, k, dom\_id, var\_id)

get\_dart\_vector\_index returns the dart index for a given i,j,k of a
variable. Note if the variable is 1D j and k are ignored. If a variable
is 2D k is ignored. Note only variables upto 3D are supported, but this
could be extended to support upto 7 dimensional variables (or whatever
fortran and netcdf will support).

##### Getting the state at a given dart index

`function x = get_state(index, state_handle)`

get\_state returns the state x at the given index. state\_handle is a
derived type which conatins the state information. state\_handle is
passed to the model\_mod from above. get\_state returns an array of
values (the whole ensemble at index) during model\_mod and a single
value (the mean) during get\_close\_obs or vert\_convert.

If you have an array of indices, for example a forward operator which is
located in different levels on different ensemble members you can use
get\_state\_array. An example of this is in CAM when an observation is
in pressure, the level an observation is in depends on the state and so
can vary across the ensemble.

`subroutine get_state_array(x(:), index(:), state_handle)`

The code inside get\_state\_array will do the minimum amount of
communication to get you the indices you need. For example if

  index = \[ 3 4 3 3 4 3\]

get\_state\_array will only do 2 mpi communications and return

  x = \[state(3), state(4), state(3), state(3), state(4), state(3)\]

A limited module diagram is shown below. A -&gt; B means A uses B:

![](./Graphs/window.gv.svg)

Filter\_mod and assim\_tools\_mod take care of making data available for
use with get\_state. Note get\_state will only return data during
*model\_interpolate*, *get\_close\_obs*, or *vert\_convert*. If you use
get\_state outside these routines you will get and error.

**Compliation Notes**

The Remote Memory Access programming model we are using uses
mpi\_windows. There are 2 ways to compile window mods for mpi and
non-mpi filter. This is taken care of automatically when you run
quickbuild.csh or an mkmf\_\* with -mpi or -nompi. However, if you use
mpi, there is a choice of mpi\_window mods:

-   cray\_win\_mod.f90
-   no\_cray\_win\_mod.f90

We have these two modules that you can swap in your path\_names files
because the MPI 2 standard states:\
Implementors may restrict the use of RMA communication that is
synchronized by lock calls to windows in memory allocated by
MPI\_ALLOC\_MEM.\
MPI\_ALLOC\_MEM uses cray pointers, thus we have supplied a window
module that uses cray pointers. However, no\_cray\_win\_mod.f90 is the
default since some versions of gfortran (4.9.0) do not support cray
pointers. These different modules will go away when we swap to MPI 3.

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


