[]{#TOP}

Forward Operator
================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../images/Dartboard7.png){h | Index](../index.html)\            |
| eight="70"}                       | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

In Lanai the forward operator is performed by the first ens\_size
processors. This was because access to the whole state vector is
required for the forward operator, and only the first ens\_size
processors had the whole state vector. The distributed state forward
operator has a diffent loop structure to Lanai because all processors
can do the foward operator for their observations.

-   Lanai: the first ens\_size processors loop around ALL OBSERVATIONS.
-   Distributed state: ALL PROCESSORS loop around
    observations/num\_procs.

The forward operator is performed in `get_obs_ens_distrb_state`. A
limited call tree for `get_obs_ens_distrb_state` is shown below.

![](../Graphs/forward_operator.gv.svg)

The QC\_LOOP is in `get_obs_ens_distrb_state` because the qc across the
ensemble is known. This removes the need for a transpose of the
forward\_op\_ens\_handle. Note this is different from Lanai. The window
opening and closing in `get_obs_ens_distrb_state` is as follows:

1.  State window created (processors can access other processor's
    memory)
2.  Forward operator called
3.  QC calculated
4.  State window destroyed (processors can no longer access other
    processor's memory)

However, there may be occasions where having only the first ens\_size
processors perform the forward operator. For example, if the forward
operator is being read from a file, or the forward operator uses a large
portion of the state. Or when debugging it may be easier to have 1 task
per ensemble member.

To transpose and do the forward operators like Lanai, you can use the
filter\_nml namelist option distribute\_state = .false. The process is
the same as above except the window creation and destruction are
transposing the state.

1.  State window created (state ensemble is transposed var complete)
2.  Forward operator called
3.  QC calculated
4.  State window destroyed (state ensemble is tranaposed to copy
    complete)

Note, that if you have fewer tasks than ensemble members some tasks will
still be doing vectorized forward operators (because they own more than
one ensemble member).

State Access
------------

Model\_mod routines no longer get an array containing the state. The
state is accessed through the function `get_state`.

` x = get_state(i, state_handle)`

where x is the state at index i. `state_handle` is passed from above.
During model\_interpolate `get_state` returns an array. Durring
`get_state` returns a single value (the mean state).

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


