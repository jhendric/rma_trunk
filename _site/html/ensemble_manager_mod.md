[]{#TOP}

MODULE ensemble\_manager\_mod
=============================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[NAMELIST](#Namelist) / [INTERFACES](#Interface) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[PRIVATE COMPONENTS](#PrivateComponents) / [TERMS OF USE](#Legalese)

Overview
--------

Manages storage and a number of operations for multiple copies of a
vector. The most obvious use is to manage ensembles of model state
vectors. In this case, the number of copies stored for each state vector
element is the ensemble size plus one or more additional copies like the
mean, variance, associated inflation values, etc. The ensemble\_manager
provides routines to compute the mean and variance of a subset of the
copies, to track the time associated with the copies, and to write and
read restart files. Most importantly, it provides a capability to do
transposes between two storage representations of an ensemble. In one
representation, each process stores all copies of a subset of the state
variables while in the other, each process stores all of the state
variables for a subset of copies. The ensemble manager is also used to
manage ensembles of observation priors and quality control and ensembles
of forward observation operator error status.

The ensemble manager interacts strongly with the multiple process
capability of the Message Passing Interface (MPI) libraries. It is used
to partition the data so each MPI process stores only a subset of the
copies and variables, dividing the data as evenly as possible across the
processes. At no time during the execution does any one process have to
store the entire dataset for all ensemble members (unless running in
serial mode without MPI, or if running with 1 MPI task).

The ensemble manager is set of general purpose data management routines.
For run-time efficiency, the derived type information is not marked
private which means other modules can directly manipulate the data
arrays. However it means much care must be taken to access the most
recently updated representation of the data, either the copies or
variables arrays.

A set of sanity check routines have been added to track the last
modified version of the data: the copies array or the vars array. Before
directly reading or writing these arrays call one of the 'prepare'
routines to indicate what kind of data access you are about to make. If
the most recently updated data is not as expected an error message will
occur. After the direct access if the following operations detect that
the data they are operating on is not the most recently updated they
will print an error message. Routines inside the ensemble manager that
alter the copies or vars will set the state automatically so these
routines are only necessary to call if you are directly accessing the
copies or vars arrays from outside the ensemble manager.

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

    &ensemble_manager_nml
       layout                      = 1
       tasks_per_node              = 1
       communication_configuration = 1
       debug                       = .false.
      /

</div>

\
\

<div>

  Item                           Type      Description
  ------------------------------ --------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  layout                         integer   Determines the process (pe) layout on MPI tasks. 1 is PE = MPI task. 2 is a round-robin layout around the nodes. Layout 2 results in a more even usage of memory across nodes. This may allow you to run with a larger state vector without hitting the memory limit of the node. It may give a slight (5%) increase in performance, but this is machine dependent. It has no effect on serial runs of filter.
  tasks\_per\_node               integer   The number of tasks per node. This is only used if layout = 2.
  communication\_configuration   integer   For most users, the default value of 1 is the best choice. However there are multiple strategies for the internal MPI communication patterns (see \*Note below). Values from 1 to 4 select different options; try the various options to see if one might be faster than the others.
  debug                          logical   If true print debugging information.

</div>

\
\

*\*Note about MPI communication flags:*\
The communication\_configuration flags select various combinations of
the internal settings for use\_copy2var\_send\_loop and
use\_var2copy\_rec\_loop. These flags change the order of the MPI send
and MPI receives in the the routines all\_copies\_to\_all\_vars and
all\_vars\_to\_all\_copies. The figures below show the data transferred
between tasks for an 80 member ensemble. The left figure is using 96
tasks, the right figure is using 512 tasks. As the number of tasks
increases, the 'all to all' data transfer becomes a 'some to all, all to
some' transfer and the order of MPI send and MPI receives becomes
increasingly important. The default values give a performance advantage
as the number of tasks becomes much greater than the the ensemble size.
However, for small numbers of tasks, i.e. less than the ensemble size,
changing the default values may improve performance.

<div>

  ----------------------------------------------------------------------------------------------------------------------------
  [![communication
  pattern](../../../documentation/images/comm_pattern96.png){width="400"}](../../../documentation/images/comm_pattern96.png)
  [![communication pattern
  2](../../../documentation/images/comm_pattern512.png){width="400"}](../../../documentation/images/comm_pattern512.png)
  ----------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    types_mod
    utilities_mod
    assim_model_mod
    time_manager_mod
    random_seq_mod
    mpi_utilities_mod
    sort_mod

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

  -------------------------------------- -----------------------------------------------------------------
  *use ensemble\_manager\_mod, only :*   [init\_ensemble\_manager](#init_ensemble_manager)
                                         [read\_ensemble\_restart](#read_ensemble_restart)
                                         [write\_ensemble\_restart](#write_ensemble_restart)
                                         [get\_copy](#get_copy)
                                         [put\_copy](#put_copy)
                                         [broadcast\_copy](#broadcast_copy)
                                         [set\_ensemble\_time](#set_ensemble_time)
                                         [get\_ensemble\_time](#get_ensemble_time)
                                         [end\_ensemble\_manager](#end_ensemble_manager)
                                         [duplicate\_ens](#duplicate_ens)
                                         [get\_my\_num\_copies](#get_my_num_copies)
                                         [get\_my\_copies](#get_my_copies)
                                         [get\_my\_num\_vars](#get_my_num_vars)
                                         [get\_my\_vars](#get_my_vars)
                                         [get\_copy\_owner\_index](#get_copy_owner_index)
                                         [get\_var\_owner\_index](#get_var_owner_index)
                                         [all\_vars\_to\_all\_copies](#all_vars_to_all_copies)
                                         [all\_copies\_to\_all\_vars](#all_copies_to_all_vars)
                                         [compute\_copy\_mean](#compute_copy_mean)
                                         [compute\_copy\_mean\_sd](#compute_copy_mean_sd)
                                         [compute\_copy\_mean\_var](#compute_copy_mean_var)
                                         [prepare\_to\_write\_to\_vars](#prepare_to_write_to_vars)
                                         [prepare\_to\_write\_to\_copies](#prepare_to_write_to_copies)
                                         [prepare\_to\_read\_from\_vars](#prepare_to_read_from_vars)
                                         [prepare\_to\_read\_from\_copies](#prepare_to_read_from_copies)
                                         [prepare\_to\_update\_vars](#prepare_to_update_vars)
                                         [prepare\_to\_update\_copies](#prepare_to_update_copies)
                                         [print\_ens\_handle](#print_ens_handle)
                                         [map\_pe\_to\_task](#map_pe_to_task)
                                         [map\_task\_to\_pe](#map_task_to_pe)
  -------------------------------------- -----------------------------------------------------------------

A note about documentation style. Optional arguments are enclosed in
brackets *\[like this\]*.

[]{#ensemble_type}\

<div class="type">

    type ensemble_type
       !DIRECT ACCESS INTO STORAGE IS ALLOWED; BE CAREFUL
       integer :: num_copies
       integer :: num_vars
       integer :: my_num_copies
       integer :: my_num_vars
       integer, pointer :: my_copies(:)
       integer, pointer :: my_vars(:)
       ! Storage in next line is to be used when each PE has all copies of subset of vars
       real(r8), pointer :: copies(:, :)  ! Dimensioned (num_copies, my_num_vars)
       ! Storage on next line is used when each PE has subset of copies of all vars
       real(r8), pointer :: vars(:, :)    ! Dimensioned (num_vars, my_num_copies)
       ! Time is only related to var complete
       type(time_type), pointer :: time(:)
       integer :: distribution_type
       integer :: valid     ! copies modified last, vars modified last, both same
       integer :: id_num
       integer, allocatable :: task_to_pe_list(:) ! List of tasks
       integer, allocatable :: pe_to_task_list(:) ! List of tasks
       ! Flexible my_pe, layout_type which allows different task layouts for different ensemble handles
       integer :: my_pe
       integer :: layout_type
    end type ensemble_type

</div>

<div class="indent1">

Provides a handle for an ensemble that manages copies of a vector. For
efficiency, the type internals are not private and direct access to the
storage arrays is used throughout DART.

  Component            Description
  -------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  num\_copies          Global number of copies of the vector.
  num\_vars            Global number of elements (variables) in the vector.
  my\_num\_copies      Number of copies stored by this process.
  my\_num\_vars        Number of variables stored by this process.
  my\_copies           Dimensioned to size my\_num\_copies. Contains a list of the global indices of copies stored by this process.
  my\_vars             Dimensioned to size my\_num\_vars. Contains a list of the global indices of variables stored by this process.
  copies               Dimensioned (num\_copies, my\_num\_vars). Storage for all copies of variables stored by this process.
  vars                 Dimensioned (num\_vars, my\_num\_copies). Storage for all variables of copies stored by this process.
  time                 Dimensioned my\_num\_copies. A time\_type that stores time associated with a given copy of the vector.
  distribution\_type   Does nothing at present. Can be used for future releases to control the layout of different copies and variables in storage.
  valid                Flag to track whether the copies array has the most recently updated data, the vars array is most recently modified, or if both the arrays have identical data, like after a transpose.
  id\_num              Internal number unique to each ensemble handle, used for debugging purposes.
  task\_to\_pe\_list   Mapping from MPI task number to logical Processing Element (PE) number. Enables different assignment of MPI tasks to PEs. If the number of MPI tasks is larger than the number of copies of the vector, when the ensemble is var complete then the first N MPI tasks have allocated 'vars' arrays and the remaining ones do not. Assigning the MPI tasks round-robin to multi-processor nodes can make the memory usage more uniform across nodes, which may allow more MPI tasks per node than the standard layout.
  pe\_to\_task\_list   Logical PE to MPI task mapping. See above for more description.
  my\_pe               The logical PE number for the MPI task.
  layout\_type         Controls the mapping type between MPI tasks and PEs. Currently type 1 is the standard layout (one-to-one mapping) and type 2 is a round-robin mapping where each node gets a task in turn before assigning a second task to each node, until all tasks are assigned.

</div>

\
[]{#init_ensemble_manager}\

<div class="routine">

*call init\_ensemble\_manager(ens\_handle, num\_copies, num\_vars
*\[, distribution\_type\_in\]* *\[, layout\_type\]*)*
    type(ensemble_type), intent(out) :: ens_handle
    integer,             intent(in)  :: num_copies
    integer,             intent(in)  :: num_vars
    integer, optional,   intent(in)  :: distribution_type_in
    integer, optional,   intent(in)  :: layout_type

</div>

<div class="indent1">

Initializes an instance of an ensemble. Storage is allocated and the
size descriptions in the ensemble\_type are initialized.

  -------------------------- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *ens\_handle*              Handle for the ensemble being initialized
  *num\_copies*              Number of copies of vector.
  *num\_vars*                Number of variables in the vector.
  *distribution\_type\_in*   Controls layout of storage on PEs. Currently only option 1 is supported.
  *layout\_type*             Controls layout of MPI tasks on PEs. Type 1 is the default, where MPI tasks are assigned to PEs on a one-to-one basis. Type 2 is a round-robin assignment where each node gets one task before the nodes are assigned a second task. If running with more MPI tasks than *num\_copies*, this can result in a more uniform usage of memory across the nodes.
  -------------------------- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#read_ensemble_restart}\

<div class="routine">

*call read\_ensemble\_restart(ens\_handle, start\_copy, end\_copy,
start\_from\_restart, file\_name *\[, init\_time\]*
*\[, force\_single\_file\]*)*
    type(ensemble_type),       intent(inout) :: ens_handle
    integer,                   intent(in)    :: start_copy
    integer,                   intent(in)    :: end_copy
    logical,                   intent(in)    :: start_from_restart
    character(len=*),          intent(in)    :: file_name
    type(time_type), optional, intent(in)    :: init_time
    logical, optional,         intent(in)    :: force_single_file

</div>

<div class="indent1">

Read in a set of copies of a vector from file *file\_name*. The copies
read are place into global copies start\_copy:end\_copy in the
ens\_handle. If start\_from\_restart is false, then only a single copy
of the vector is read from the file and then it is perturbed using
routines in assim\_model\_mod to generate the required number of copies.
The read can be from a single file that contains all needed copies or
from a different file for each copy. This choice is controlled by the
namelist entry single\_restart\_file\_in. However, the optional argument
force\_single\_file forces the read to be from a single file if it is
present and true. This is used for ensembles that contain the inflation
values for state space inflation. If multiple files are to be read, the
file names are generated by appending integers to the input file\_name.
If the input is a single file all reads are done sequentially by process
0 and then shipped to the PE that stores that copy. If the input is
multiple files each MPI task reads the copies it stores directly and
independently.

  ------------------------ -------------------------------------------------------------------------------------------------
  *ens\_handle*            Handle of ensemble.
  *start\_copy*            Global index of first of continguous set of copies to be read.
  *end\_copy*              Global index of last of contiguous set of copies to be read, copies(start\_copy:end\_copy).
  *start\_from\_restart*   If true, read all copies from file. If false, read one copy and perturb to get required number.
  *file\_name*             Name of file from which to read.
  *init\_time*             If present, set time of all copies read to this value.
  *force\_single\_file*    If present and true, force the read to be from a single file which contains all copies.
  ------------------------ -------------------------------------------------------------------------------------------------

</div>

\
[]{#write_ensemble_restart}\

<div class="routine">

*call write\_ensemble\_restart(ens\_handle, file\_name, start\_copy,
end\_copy *\[, force\_single\_file\]*)*
    type(ensemble_type), intent(inout) :: ens_handle
    character(len=*),    intent(in)    :: file_name
    integer,             intent(in)    :: start_copy
    integer,             intent(in)    :: end_copy
    logical, optional,   intent(in)    :: force_single_file

</div>

<div class="indent1">

Writes a set of copies of a vector to file file\_name. The copies
written are from global copies start\_copy:end\_copy in the ens\_handle.
The write can be to a single file or to a different file for each copy.
This choice is controlled by the namelist entry
single\_restart\_file\_out. However, the optional argument
force\_single\_file forces the write to be to a single file if it is
present and true. This is used for ensembles that contain the inflation
values for state space inflation. If multiple files are to be written,
the file names are generated by appending integers to the input
file\_name. If the output is a single file all copies are shipped from
the PE that stores that copy to process 0, and then written out
sequentially. If the output is to multiple files each MPI task writes
the copies it stores directly and independently.

  ----------------------- ------------------------------------------------------------------------------------------------
  *ens\_handle*           Handle of ensemble.
  *file\_name*            Name of file from which to read.
  *start\_copy*           Global index of first of continguous set of copies to be written.
  *end\_copy*             Global index of last of contiguous set of copies to be written, copies(start\_copy:end\_copy).
  *force\_single\_file*   If present and true, force the write to be to a single file which contains all copies.
  ----------------------- ------------------------------------------------------------------------------------------------

</div>

\
[]{#get_copy}\

<div class="routine">

*call get\_copy(receiving\_pe, ens\_handle, copy, vars *\[, mtime\]*)*
    integer,                   intent(in)  :: receiving_pe
    type(ensemble_type),       intent(in)  :: ens_handle
    integer,                   intent(in)  :: copy
    real(r8), dimension(:),    intent(out) :: vars
    type(time_type), optional, intent(out) :: mtime

</div>

<div class="indent1">

Retrieves a copy of the state vector, indexed by the global index copy.
The process that is to receive the copy is receiving\_pe and the copy is
returned in the one dimensional array vars. The time of the copy is also
returned if mtime is present. This is generally used for operations,
like IO, that require a single processor to do things with the entire
state vector. Data is only returned in vars on the receiving PE; vars on
all other PEs is unset.

  ----------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *receiving\_pe*   This process ends up with the requested copy of the state vector.
  *ens\_handle*     Handle for ensemble.
  *copy*            The global index of the copy of the state vector that is to be retrieved.
  *vars*            One dimensional array in which the requested copy of the state vector is returned. Data is only returned in vars on the receiving PE; vars on all other PEs is unset.
  *mtime*           If present returns the time of the requested copy.
  ----------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#put_copy}\

<div class="routine">

*call put\_copy(sending\_pe, ens\_handle, copy, vars *\[, mtime\]*)*
    integer,                   intent(in)    :: sending_pe
    type(ensemble_type),       intent(inout) :: ens_handle
    integer,                   intent(in)    :: copy
    real(r8), dimension(:),    intent(in)    :: vars
    type(time_type), optional, intent(in)    :: mtime

</div>

<div class="indent1">

Sends a state vector, in vars, from the given process to the process
storing the global index copy. The time of the copy is also sent if
mtime is present. This is generally used for operations, like IO, that
require a single processor to do things with the entire state vector.
For instance, if a single process reads in a state vector, it can be
shipped to the storing process by this subroutine. Only the data in vars
on the sending PE is processed; vars on all other PEs is ignored.

  --------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *sending\_pe*   This process sends the copy of the state vector.
  *ens\_handle*   Handle for ensemble.
  *copy*          The global index of the copy of the state vector that is to be sent.
  *vars*          One dimensional array in which the requested copy of the state vector is located. Only the data in vars on the sending PE is processed; vars on all other PEs is ignored.
  *mtime*         If present send the time of the copy.
  --------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#broadcast_copy}\

<div class="routine">

*call broadcast\_copy(ens\_handle, copy, arraydata)*
    type(ensemble_type),    intent(in)   :: ens_handle
    integer,                intent(in)   :: copy
    real(r8), dimension(:), intent(out)  :: arraydata

</div>

<div class="indent1">

Finds which PE has the global index copy and broadcasts that copy to all
PEs. *arraydata* is an output on all PEs, even on the PE which is the
owner if it is separate storage from the vars array in the ensemble
handle. This is a collective routine, which means it must be called by
all processes in the job.

  --------------- ------------------------------------------------------------------------------------------------------------------------------
  *ens\_handle*   Handle for ensemble.
  *copy*          The global index of the copy of the state vector that is to be sent.
  *arraydata*     One dimensional array into which the requested copy of the state vector will be copied on all PEs, including the sending PE.
  --------------- ------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#set_ensemble_time}\

<div class="routine">

*call set\_ensemble\_time(ens\_handle, indx, mtime)*
    type(ensemble_type), intent(inout) :: ens_handle
    integer,             intent(in)    :: indx
    type(time_type),     intent(in)    :: mtime

</div>

<div class="indent1">

Set the time of a copy to the given value. *indx* in this case is the
local copy number for a specific task.
[get\_copy\_owner\_index()](#get_copy_owner_index) can be called to see
if you are the owning task for a given global copy number, and to get
the local index number for that copy.

  --------------- --------------------------------------------------------------------
  *ens\_handle*   Handle for ensemble.
  *indx*          The local index of the copy of the state vector that is to be set.
  *mtime*         The time to set for this copy.
  --------------- --------------------------------------------------------------------

</div>

\
[]{#get_ensemble_time}\

<div class="routine">

*call get\_ensemble\_time(ens\_handle, indx, mtime)*
    type(ensemble_type), intent(in)   :: ens_handle
    integer,             intent(in)   :: indx
    type(time_type),     intent(out)  :: mtime

</div>

<div class="indent1">

Get the time associated with a copy. *indx* in this case is the local
copy number for a specific task.
[get\_copy\_owner\_index()](#get_copy_owner_index) can be called to see
if you are the owning task for a given global copy number, and to get
the local index number for that copy.

  --------------- --------------------------------------------------------
  *ens\_handle*   Handle for ensemble.
  *indx*          The local index of the copy to retrieve the time from.
  *mtime*         The returned time value.
  --------------- --------------------------------------------------------

</div>

\
[]{#end_ensemble_manager}\

<div class="routine">

*call end\_ensemble\_manager(ens\_handle)*
    type(ensemble_type), intent(in)  :: ens_handle

</div>

<div class="indent1">

Frees up storage associated with an ensemble.

  --------------- -------------------------
  *ens\_handle*   Handle for an ensemble.
  --------------- -------------------------

</div>

\
[]{#duplicate_ens}\

<div class="routine">

*call duplicate\_ens(ens1, ens2, duplicate\_time)*
    type(ensemble_type), intent(in)    :: ens1
    type(ensemble_type), intent(inout) :: ens2
    logical, intent(in)                :: duplicate_time

</div>

<div class="indent1">

Copies the contents of the vars array from ens1 into ens2. If the
num\_copies and num\_vars are not consistent or if the
distribution\_type is not consistent, fails with an error. If
duplicate\_time is true, the times from ens1 are copied over the times
of ens2. Only the vars array data is copied from the source to the
destination. Transpose the data after duplication if you want to access
the copies.

  --------------------- --------------------------------------------------------------------------------------------------
  *ens1*                Ensemble handle of ensemble to be copies into ens2. Data from the vars array will be replicated.
  *ens2*                Ensemble handle of ensemble into which ens1 vars data will be copied.
  *duplicate\_time  *   If true, copy the times from ens1 into ens2, else leave ens2 times unchanged.
  --------------------- --------------------------------------------------------------------------------------------------

</div>

\
[]{#get_my_num_copies}\

<div class="routine">

*var = get\_my\_num\_copies(ens\_handle)*
    integer                          :: get_my_num_copies
    type(ensemble_type), intent(in)  :: ens_handle

</div>

<div class="indent1">

Returns number of copies stored by this process when storing all
variables for a subset of copies. Same as num\_copies if running with
only a single process.

  --------------- --------------------------------------------------------------------------------------------------------
  *var*           Returns the number of copies stored by this process when storing all variables for a subset of copies.
  *ens\_handle*   Handle for an ensemble.
  --------------- --------------------------------------------------------------------------------------------------------

</div>

\
[]{#get_my_num_vars}\

<div class="routine">

*var = get\_my\_num\_vars(ens\_handle)*
    integer                         :: get_my_num_vars
    type(ensemble_type), intent(in) :: ens_handle

</div>

<div class="indent1">

Returns number of variables stored by this process when storing all
copies of a subset of variables. Same as num\_vars if running with only
a single process.

  --------------- -----------------------------------------------------------------------------------------------------
  *var*           Returns the number of vars stored by this process when storing all copies of a subset of variables.
  *ens\_handle*   Handle for an ensemble.
  --------------- -----------------------------------------------------------------------------------------------------

</div>

\
[]{#get_my_copies}\

<div class="routine">

*call get\_my\_copies(ens\_handle, copies)*
    type(ensemble_type), intent(in) :: ens_handle
    integer, intent(out)            :: copies(:)

</div>

<div class="indent1">

Returns a list of the global copy numbers stored on this process when
storing subset of copies of all variables.

  --------------- -------------------------------------------------------------------------------------------
  *ens\_handle*   Handle for an ensemble.
  *copies*        List of all copies stored by this process when storing subset of copies of all variables.
  --------------- -------------------------------------------------------------------------------------------

</div>

\
[]{#get_my_vars}\

<div class="routine">

*call get\_my\_vars(ens\_handle, vars)*
    type(ensemble_type), intent(in) :: ens_handle
    integer, intent(out)            :: vars(:)

</div>

<div class="indent1">

Returns a list of the global variable numbers stored on this process
when storing all copies of a subset of variables.

  --------------- ------------------------------------------------------------------------------------------------
  *ens\_handle*   Handle for an ensemble.
  *vars*          List of all variables stored on this process when storing all copies of a subset of variables.
  --------------- ------------------------------------------------------------------------------------------------

</div>

\
[]{#get_copy_owner_index}\

<div class="routine">

*call get\_copy\_owner\_index(copy\_number, owner, owners\_index)*
    integer, intent(in)  :: copy_number
    integer, intent(out) :: owner
    integer, intent(out) :: owners_index

</div>

<div class="indent1">

Given the global index of a copy number, returns the PE that stores this
copy when all variables of a subset of copies are stored and the local
storage index for this copy on that process.

  ------------------- -----------------------------------------------------------------------------------------------
  *copy\_number*      Global index of a copy from an ensemble.
  *owner*             Process Element (PE) that stores this copy when each has all variables of a subset of copies.
  *owners\_index  *   Local storage index for this copy on the owning process.
  ------------------- -----------------------------------------------------------------------------------------------

</div>

\
[]{#get_var_owner_index}\

<div class="routine">

*call get\_var\_owner\_index(var\_number, owner, owners\_index)*
    integer, intent(in)  :: var_number
    integer, intent(out) :: owner
    integer, intent(out) :: owners_index

</div>

<div class="indent1">

Given the global index of a variable in the vector, returns the PE that
stores this variable when all copies of a subset of variables are stored
and the local storage index for this variable on that process.

  ------------------- -------------------------------------------------------------------------------------------------
  *var\_number*       Global index of a variable in the vector from an ensemble.
  *owner*             Process Element (PE) that stores this variable when each has all copies of subset of variables.
  *owners\_index  *   Local storage index for this variable on the owning process.
  ------------------- -------------------------------------------------------------------------------------------------

</div>

\
[]{#all_vars_to_all_copies}\

<div class="routine">

*call all\_vars\_to\_all\_copies(ens\_handle, label)*
    type(ensemble_type), intent(inout)        :: ens_handle
    character(len=*),    intent(in), optional :: label

</div>

<div class="indent1">

Transposes data from a representation in which each PE has a subset of
copies of all variables to one in which each has all copies of a subset
of variables. In the current implementation, storage is not released so
both representations are always available. However, one representation
may be current while the other is out of date.

Different different numbers of copies, different lengths of the vectors,
different numbers of PEs and different implementations of the MPI
parallel libraries can have very different performance characteristics.
The namelist item *communication\_configuration* controls one of four
possible combinations of the operation order during the transposes. If
performance is an issue the various settings on this namelist item can
be explored. See the [namelist section](#Namelist) for more details.

The transpose routines make both representations of the data equivalent
until the next update to either the copies or the vars arrays, so either
can be used as a data source.

  --------------- ---------------------------------------------------------------------------------------------------------------------
  *ens\_handle*   The handle of the ensemble being transposed.
  *label*         A character string label. If present, a timestamp with this label is printed at the start and end of the transpose.
  --------------- ---------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#all_copies_to_all_vars}\

<div class="routine">

*call all\_copies\_to\_all\_vars(ens\_handle, label)*
    type(ensemble_type), intent(inout)        :: ens_handle
    character(len=*),    intent(in), optional :: label

</div>

<div class="indent1">

Transposes data from a representation in which each processor has all
copies of a subset of variables to one in which each has a subset of
copies of all variables. In the current implementation, storage is not
released so both representations are always available. However, one
representation may be current while the other is out of date.

Different different numbers of copies, different lengths of the vectors,
different numbers of PEs and different implementations of the MPI
parallel libraries can have very different performance characteristics.
The namelist item *communication\_configuration* controls one of four
possible combinations of the operation order during the transposes. If
performance is an issue the various settings on this namelist item can
be explored. See the [namelist section](#Namelist) for more details.

The transpose routines make both representations of the data equivalent
until the next update to either the copies or the vars arrays, so either
can be used as a data source.

  --------------- ---------------------------------------------------------------------------------------------------------------------
  *ens\_handle*   The handle of the ensemble being transposed.
  *label*         A character string label. If present, a timestamp with this label is printed at the start and end of the transpose.
  --------------- ---------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#compute_copy_mean}\

<div class="routine">

*call compute\_copy\_mean(ens\_handle, start\_copy, end\_copy,
mean\_copy)*
    type(ensemble_type), intent(inout) :: ens_handle
    integer,             intent(in)    :: start_copy
    integer,             intent(in)    :: end_copy
    integer,             intent(in)    :: mean_copy

</div>

<div class="indent1">

Computes the mean of a contiguous subset of copies starting with global
index start\_copy and ending with global index ens\_copy. Mean is
written to global index mean\_copy.

When this routine is called the ensemble must have all copies of a
subset of the vars. It updates the copies array with the mean, so after
this call the copies array data is more current and the vars data is
stale.

  --------------- --------------------------------------------------------
  *ens\_handle*   Handle for an ensemble.
  *start\_copy*   Global index of first copy in mean and sd computation.
  *end\_copy*     Global index of last copy in mean and sd computation.
  *mean\_copy*    Global index of copy into which mean is written.
  --------------- --------------------------------------------------------

</div>

\
[]{#compute_copy_mean_sd}\

<div class="routine">

*call compute\_copy\_mean\_sd(ens\_handle, start\_copy, end\_copy,
mean\_copy, sd\_copy)*
    type(ensemble_type), intent(inout) :: ens_handle
    integer,             intent(in)    :: start_copy
    integer,             intent(in)    :: end_copy
    integer,             intent(in)    :: mean_copy
    integer,             intent(in)    :: sd_copy

</div>

<div class="indent1">

Computes the mean and standard deviation of a contiguous subset of
copies starting with global index start\_copy and ending with global
index ens\_copy. Mean is written to index mean\_copy and standard
deviation to index sd\_copy.

When this routine is called the ensemble must have all copies of a
subset of the vars. It updates the copies arrays with the mean and sd,
so after this call the copies array data is more current and the vars
data is stale.

  --------------- ----------------------------------------------------------------
  *ens\_handle*   Handle for an ensemble.
  *start\_copy*   Global index of first copy in mean and sd computation.
  *end\_copy*     Global index of last copy in mean and sd computation.
  *mean\_copy*    Global index of copy into which mean is written.
  *sd\_copy*      Global index of copy into which standard deviation is written.
  --------------- ----------------------------------------------------------------

</div>

\
[]{#compute_copy_mean_var}\

<div class="routine">

*call compute\_copy\_mean\_var(ens\_handle, start\_copy, end\_copy,
mean\_copy, var\_copy)*
    type(ensemble_type), intent(inout) :: ens_handle
    integer,             intent(in)  :: start_copy
    integer,             intent(in)  :: end_copy
    integer,             intent(in)  :: mean_copy
    integer,             intent(in)  :: var_copy

</div>

<div class="indent1">

Computes the mean and variance of a contiguous subset of copies starting
with global index start\_copy and ending with global index ens\_copy.
Mean is written to index mean\_copy and variance to index var\_copy.

When this routine is called the ensemble must have all copies of a
subset of the vars. It updates the copies arrays with the mean and
variance, so after this call the copies array data is more current and
the vars data is stale.

  --------------- --------------------------------------------------------
  *ens\_handle*   Handle for an ensemble.
  *start\_copy*   Global index of first copy in mean and sd computation.
  *end\_copy*     Global index of last copy in mean and sd computation.
  *mean\_copy*    Global index of copy into which mean is written.
  *var\_copy*     Global index of copy into which variance is written.
  --------------- --------------------------------------------------------

</div>

\
[]{#prepare_to_update_vars}\

<div class="routine">

*call prepare\_to\_update\_vars(ens\_handle)*
    type(ensemble_type), intent(inout)  :: ens_handle

</div>

<div class="indent1">

Call this routine before directly accessing the *ens\_handle%vars* array
when the data is going to be updated, and the incoming vars array should
have the most current data representation.

Internally the ensemble manager tracks which of the copies or vars
arrays, or both, have the most recently updated representation of the
data. For example, before a transpose (*all\_vars\_to\_all\_copies()* or
*all\_copies\_to\_all\_vars()*) the code checks to be sure the source
array has the most recently updated representation before it does the
operation. After a transpose both representations have the same update
time and are both valid.

For efficiency reasons we allow the copies and vars arrays to be
accessed directly from other code without going through a routine in the
ensemble manager. The "prepare" routines verify that the desired array
has the most recently updated representation of the data, and if needed
marks which one has been updated so the internal consistency checks have
an accurate accounting of the representations.

  --------------- --------------------------------------------------
  *ens\_handle*   Handle for the ensemble being accessed directly.
  --------------- --------------------------------------------------

</div>

\
[]{#prepare_to_update_copies}\

<div class="routine">

*call prepare\_to\_update\_copies(ens\_handle)*
    type(ensemble_type), intent(inout)  :: ens_handle

</div>

<div class="indent1">

Call this routine before directly accessing the *ens\_handle%copies*
array when the data is going to be updated, and the incoming copies
array should have the most current data representation.

Internally the ensemble manager tracks which of the copies or vars
arrays, or both, have the most recently updated representation of the
data. For example, before a transpose (*all\_vars\_to\_all\_copies()* or
*all\_copies\_to\_all\_vars()*) the code checks to be sure the source
array has the most recently updated representation before it does the
operation. After a transpose both representations have the same update
time and are both valid.

For efficiency reasons we allow the copies and vars arrays to be
accessed directly from other code without going through a routine in the
ensemble manager. The "prepare" routines verify that the desired array
has the most recently updated representation of the data, and if needed
marks which one has been updated so the internal consistency checks have
an accurate accounting of the representations.

  --------------- --------------------------------------------------
  *ens\_handle*   Handle for the ensemble being accessed directly.
  --------------- --------------------------------------------------

</div>

\
[]{#prepare_to_read_from_vars}\

<div class="routine">

*call prepare\_to\_read\_from\_vars(ens\_handle)*
    type(ensemble_type), intent(inout)  :: ens_handle

</div>

<div class="indent1">

Call this routine before directly accessing the *ens\_handle%vars* array
for reading only, when the incoming vars array should have the most
current data representation.

Internally the ensemble manager tracks which of the copies or vars
arrays, or both, have the most recently updated representation of the
data. For example, before a transpose (*all\_vars\_to\_all\_copies()* or
*all\_copies\_to\_all\_vars()*) the code checks to be sure the source
array has the most recently updated representation before it does the
operation. After a transpose both representations have the same update
time and are both valid.

For efficiency reasons we allow the copies and vars arrays to be
accessed directly from other code without going through a routine in the
ensemble manager. The "prepare" routines verify that the desired array
has the most recently updated representation of the data, and if needed
marks which one has been updated so the internal consistency checks have
an accurate accounting of the representations.

  --------------- --------------------------------------------------
  *ens\_handle*   Handle for the ensemble being accessed directly.
  --------------- --------------------------------------------------

</div>

\
[]{#prepare_to_read_from_copies}\

<div class="routine">

*call prepare\_to\_read\_from\_copies(ens\_handle)*
    type(ensemble_type), intent(inout)  :: ens_handle

</div>

<div class="indent1">

Call this routine before directly accessing the *ens\_handle%copies*
array for reading only, when the incoming copies array should have the
most current data representation.

Internally the ensemble manager tracks which of the copies or vars
arrays, or both, have the most recently updated representation of the
data. For example, before a transpose (*all\_vars\_to\_all\_copies()* or
*all\_copies\_to\_all\_vars()*) the code checks to be sure the source
array has the most recently updated representation before it does the
operation. After a transpose both representations have the same update
time and are both valid.

For efficiency reasons we allow the copies and vars arrays to be
accessed directly from other code without going through a routine in the
ensemble manager. The "prepare" routines verify that the desired array
has the most recently updated representation of the data, and if needed
marks which one has been updated so the internal consistency checks have
an accurate accounting of the representations.

  --------------- --------------------------------------------------
  *ens\_handle*   Handle for the ensemble being accessed directly.
  --------------- --------------------------------------------------

</div>

\
[]{#prepare_to_write_to_vars}\

<div class="routine">

*call prepare\_to\_write\_to\_vars(ens\_handle)*
    type(ensemble_type), intent(inout)  :: ens_handle

</div>

<div class="indent1">

Call this routine before directly accessing the *ens\_handle%vars* array
for writing. This routine differs from the 'update' version in that it
doesn't care what the original data state is. This routine might be used
in the case where an array is being filled for the first time and
consistency with the data in the copies array is not an issue.

Internally the ensemble manager tracks which of the copies or vars
arrays, or both, have the most recently updated representation of the
data. For example, before a transpose (*all\_vars\_to\_all\_copies()* or
*all\_copies\_to\_all\_vars()*) the code checks to be sure the source
array has the most recently updated representation before it does the
operation. After a transpose both representations have the same update
time and are both valid.

For efficiency reasons we allow the copies and vars arrays to be
accessed directly from other code without going through a routine in the
ensemble manager. The "prepare" routines verify that the desired array
has the most recently updated representation of the data, and if needed
marks which one has been updated so the internal consistency checks have
an accurate accounting of the representations.

  --------------- --------------------------------------------------
  *ens\_handle*   Handle for the ensemble being accessed directly.
  --------------- --------------------------------------------------

</div>

\
[]{#prepare_to_write_to_copies}\

<div class="routine">

*call prepare\_to\_write\_to\_copies(ens\_handle)*
    type(ensemble_type), intent(inout)  :: ens_handle

</div>

<div class="indent1">

Call this routine before directly accessing the *ens\_handle%copies*
array for writing. This routine differs from the 'update' version in
that it doesn't care what the original data state is. This routine might
be used in the case where an array is being filled for the first time
and consistency with the data in the vars array is not an issue.

Internally the ensemble manager tracks which of the copies or vars
arrays, or both, have the most recently updated representation of the
data. For example, before a transpose (*all\_vars\_to\_all\_copies()* or
*all\_copies\_to\_all\_vars()*) the code checks to be sure the source
array has the most recently updated representation before it does the
operation. After a transpose both representations have the same update
time and are both valid.

For efficiency reasons we allow the copies and vars arrays to be
accessed directly from other code without going through a routine in the
ensemble manager. The "prepare" routines verify that the desired array
has the most recently updated representation of the data, and if needed
marks which one has been updated so the internal consistency checks have
an accurate accounting of the representations.

  --------------- --------------------------------------------------
  *ens\_handle*   Handle for the ensemble being accessed directly.
  --------------- --------------------------------------------------

</div>

\

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PRIVATE INTERFACES
------------------

  --- ---------------------------------------------------------
      [assign\_tasks\_to\_pes](#assign_tasks_to_pes)
      [calc\_tasks\_on\_each\_node](#calc_tasks_on_each_node)
      [create\_pe\_to\_task\_list](#create_pe_to_task_list)
      [get\_copy\_list](#get_copy_list)
      [get\_max\_num\_copies](#get_max_num_copies)
      [get\_max\_num\_vars](#get_max_num_vars)
      [get\_var\_list](#get_var_list)
      [round\_robin](#round_robin)
      [set\_up\_ens\_distribution](#set_up_ens_distribution)
      [simple\_layout](#simple_layout)
      [sort\_task\_list](#sort_task_list)
      [timestamp\_message](#timestamp_message)
  --- ---------------------------------------------------------

[]{#get_max_num_copies}\

<div class="routine">

*var = get\_max\_num\_copies(num\_copies)*
    integer              :: get_max_num_copies
    integer, intent(in)  :: num_copies

</div>

<div class="indent1">

Returns the largest number of copies that are on any pe when var
complete. Depends on distribution\_type with only option 1 currently
implemented. Used to get size for creating storage to receive a list of
the copies on a PE.

  --------------- ------------------------------------------------------------------------------
  *var*           Returns the largest number of copies any an individual PE when var complete.
  *num\_copies*   Total number of copies in the ensemble.
  --------------- ------------------------------------------------------------------------------

</div>

\
[]{#get_max_num_vars}\

<div class="routine">

*var = get\_max\_num\_vars(num\_vars)*
    integer              :: get_max_num_vars
    integer, intent(in)  :: num_vars

</div>

<div class="indent1">

Returns the largest number of vars that are on any pe when copy
complete. Depends on distribution\_type with only option 1 currently
implemented. Used to get size for creating storage to receive a list of
the vars on a PE.

  --------------- -----------------------------------------------------------------------------
  *var*           Returns the largest number of vars any an individual PE when copy complete.
  *num\_copies*   Total number of vars in an ensemble vector.
  --------------- -----------------------------------------------------------------------------

</div>

\
[]{#set_up_ens_distribution}\

<div class="routine">

*call set\_up\_ens\_distribution(ens\_handle)*
    type(ensemble_type), intent(inout) :: ens_handle

</div>

<div class="indent1">

Figures out how to lay out the copy complete and vars complete
distributions. The distribution\_type identifies different options. Only
distribution\_type 1 is implemented. This puts every Nth var or copy on
a given processor where N is the total number of processes.

  --------------- -------------------------
  *ens\_handle*   Handle for an ensemble.
  --------------- -------------------------

</div>

\
[]{#get_var_list}\

<div class="routine">

*call get\_var\_list(num\_vars, pe, var\_list, pes\_num\_vars)*
    integer,   intent(in)     :: num_vars
    integer,   intent(in)     :: pe
    integer,   intent(out)    :: var_list(:)
    integer,   intent(out)    :: pes_num_vars

</div>

<div class="indent1">

Returns a list of the vars stored by process pe when copy complete and
the number of these vars. var\_list must be dimensioned large enough to
hold all vars. Depends on distribution\_type with only option 1
currently implemented.

  ------------------ --
  *num\_vars*        
  *pe*               
  *var\_list(:)*     
  *pes\_num\_vars*   
  ------------------ --

</div>

\
[]{#get_copy_list}\

<div class="routine">

*call get\_copy\_list(num\_copies, pe, copy\_list, pes\_num\_copies)*
    integer,   intent(in)     :: num_copies
    integer,   intent(in)     :: pe
    integer,   intent(out)    :: copy_list(:)
    integer,   intent(out)    :: pes_num_copies

</div>

<div class="indent1">

Returns a list of the copies stored by process pe when var complete and
the number of these copies. copy\_list must be dimensioned large enough
to hold all copies. Depends on distribution\_type with only option 1
currently implemented.

  -------------------- --
  *num\_copies*        
  *pe*                 
  *copy\_list(:)*      
  *pes\_num\_copies*   
  -------------------- --

</div>

\
[]{#timestamp_message}\

<div class="routine">

*call timestamp\_message(msg *\[, sync\]* *\[, alltasks\]*)*
    character(len=*), intent(in)           :: msg
    logical,          intent(in), optional :: sync
    logical,          intent(in), optional :: alltasks

</div>

<div class="indent1">

Write current time and message to stdout and log file. If sync is
present and true, sync mpi jobs before printing time. If alltasks is
present and true, all tasks print the time. The default is only task 0
prints a timestamp.

  ------------ ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *msg*        character string to prepend to the time info
  *sync*       if present and true, execute an MPI\_Barrier() to sync all MPI tasks before printing the time. this means the time will be the value of the slowest of the tasks to reach this point.
  *alltasks*   if present and true, have all tasks print out a timestamp. the default is for just task 0 to print. the usual combination is either sync=true and alltasks=false, or sync=false and alltasks=true.
  ------------ ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#print_ens_handle}\

<div class="routine">

*call print\_ens\_handle(ens\_handle, force, label)*
    type(ensemble_type),        intent(in) :: ens_handle
    logical,          optional, intent(in) :: force
    character(len=*), optional, intent(in) :: label

</div>

<div class="indent1">

For debugging use, dump the contents of an ensemble handle derived type.
If the *debug* namelist item is true, this will print in any case. If
*debug* is false, set *force* to true to force printing. The optional
string label can help provide context for the output.

  --------------- -----------------------------------------------------------------------------
  *ens\_handle*   The derived type to print information about.
  *force*         If the *debug* namelist item is false, set this to true to enable printing.
  *label*         Optional string label to print to provide context for the output.
  --------------- -----------------------------------------------------------------------------

</div>

\
[]{#assign_tasks_to_pes}\

<div class="routine">

*call assign\_tasks\_to\_pes(ens\_handle, nEns\_members, layout\_type)*
    type(ensemble_type), intent(inout)    :: ens_handle
    integer,             intent(in)       :: nEns_members
    integer,             intent(inout)    :: layout_type

</div>

<div class="indent1">

Calulate the task layout based on the tasks per node and the total
number of tasks. Allows the user to spread out the ensemble members as
much as possible to balance memory usage between nodes. Possible
options: 1. Standard task layout - first n tasks have the ensemble
members my\_pe = my\_task\_id() 2. Round-robin on the nodes

  --------------- -------------------------
  *ens\_handle*   Handle for an ensemble.
  **              
  **              
  --------------- -------------------------

</div>

\
[]{#round_robin}\

<div class="routine">

*call round\_robin(ens\_handle)*
    type(ensemble_type), intent(inout)    :: ens_handle

</div>

<div class="indent1">

Round-robin MPI task layout starting at the first node. Starting on the
first node forces pe 0 = task 0. The smoother code assumes task 0 has an
ensemble member. If you want to break the assumption that pe 0 = task 0,
this routine is a good place to start. Test with the smoother.

  --------------- -------------------------
  *ens\_handle*   Handle for an ensemble.
  --------------- -------------------------

</div>

\
[]{#create_pe_to_task_list}\

<div class="routine">

*call create\_pe\_to\_task\_list(ens\_handle)*
    type(ensemble_type), intent(inout)    :: ens_handle

</div>

<div class="indent1">

Creates the *ens\_handle%pe\_to\_task\_list*.
*ens\_handle%task\_to\_pe\_list* must have been assigned first,
otherwise this routine will just return nonsense.

  --------------- -------------------------
  *ens\_handle*   Handle for an ensemble.
  --------------- -------------------------

</div>

\
[]{#calc_tasks_on_each_node}\

<div class="routine">

*call calc\_tasks\_on\_each\_node(nodes, last\_node\_task\_number)*
    integer, intent(out)  :: last_node_task_number
    integer, intent(out)  :: nodes

</div>

<div class="indent1">

Finds the of number nodes and how many tasks are on the last node, given
the number of tasks and the tasks\_per\_node (ptile). The total number
of tasks is num\_pes = task\_count() The last node may have fewer tasks,
for example, if ptile = 16 and the number of mpi tasks = 17

  ---------------------------- --
  *nodes*                      
  *last\_node\_task\_number*   
  ---------------------------- --

</div>

\
[]{#simple_layout}\

<div class="routine">

*call simple\_layout(ens\_handle, n)*
    type(ensemble_type), intent(inout) :: ens_handle
    integer,             intent(in)    :: n

</div>

<div class="indent1">

assigns the arrays task\_to\_pe\_list and pe\_to\_task list for the
simple layout where my\_pe = my\_task\_id()

  --------------- -------------------------
  *ens\_handle*   Handle for an ensemble.
  *n*             
  --------------- -------------------------

</div>

\
[]{#sort_task_list}\

<div class="routine">

*call sort\_task\_list(i, idx, n)*
    integer, intent(in)    :: n
    integer, intent(inout) :: x(n)   ! array to be sorted
    integer, intent(out)   :: idx(n) ! index of sorted array

</div>

<div class="indent1">

sorts an array and returns the sorted array, and the index of the
original array

  ---------- --
  *n*        
  *x(n)*     
  *idx(n)*   
  ---------- --

</div>

\
[]{#map_pe_to_task}\

<div class="routine">

*call map\_pe\_to\_task(ens\_handle, p)*
    type(ensemble_type), intent(in) :: ens_handle
    integer,             intent(in) :: p

</div>

<div class="indent1">

Return the physical task for my\_pe

  --------------- ---------------------------------------------------
  *ens\_handle*   Handle for an ensemble.
  *p*             The MPI task corresponding to the given PE number
  --------------- ---------------------------------------------------

</div>

\
[]{#map_task_to_pe}\

<div class="routine">

*call map\_task\_to\_pe(ens\_handle, t)*
    type(ensemble_type), intent(in) :: ens_handle
    integer,             intent(in) :: t

</div>

<div class="indent1">

Return my\_pe corresponding to the physical task

  --------------- -----------------------------------------------------------
  *ens\_handle*   Handle for an ensemble.
  *t*             Return the PE corresponding to the given MPI task number.
  --------------- -----------------------------------------------------------

</div>

\
[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   input.nml
-   State vector restart files, either one for all copies or one per
    copy.
-   State vector output files, either one for all copies or one per
    copy.

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

REFERENCES
----------

1.  none

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
init\_ensemble\_manager
only distribution type 1 is implemented
For now, can't request option other than 1 for layout
read\_ensemble\_restart
start\_from\_restart in filter\_nml and single\_restart\_file\_in in
ensemble\_manager\_nml cannot both be false
Doesn't make sense to specify both of these options.
get\_copy
Requested copy is &gt; maximum\_copy
Can't ask for a copy that is greater than the maximum.
get\_copy
Size of vars \#\#\# Must be at least \#\#\#
The vars array is not big enough to hold the returned copy of the
vector.
put\_copy
Requested copy: \#\#\# is &gt; maximum\_copy: \#\#\#
Can't ask for a copy that is greater than maximum.
put\_copy
Size of vars: \#\#\# Must be at least \#\#\#
The vars array is not big enough to hold the state vector.
get\_ensemble\_time
indx \#\#\# cannot exceed \#\#\#
The index of the requested copy must be no greater than the maximum
number of copies.
duplicate\_ens
num\_copies \#\#\# and \#\#\# must be equal
Number of copies in ensembles being copied must be the same.
duplicate\_ens
num\_vars \#\#\# and \#\#\# must be equal
Number of variables in ensembles being copied must be the same.
duplicate\_ens
distribution\_type \#\#\# and \#\#\# must be equal.
Distribution types of ensembles being copies must be the same.
get\_my\_copies
Array copies only has size \#\#\# but must be at least \#\#\#
The copies array must be large enough to hold all copies of the state
vector.
get\_my\_vars
Array vars only has size \#\#\# but must be at least \#\#\#
The vars array must be large enough to hold all variables of the state
vector.

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

Additional options for the layout of ensemble storage may lead to
improved performance for different problem sizes on different
architectures.

[]{#PrivateComponents}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PRIVATE COMPONENTS
------------------

N/A

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


