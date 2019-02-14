[]{#TOP}

MODULE obs\_sequence\_mod
=========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[INTERFACES](#Interface) / [NAMELIST](#Namelist) / [FILES](#FilesUsed) /
[REFERENCES](#References) / [ERRORS](#Errors) / [PLANS](#FuturePlans) /
[PRIVATE COMPONENTS](#PrivateComponents) / [TERMS OF USE](#Legalese)

Overview
--------

Provides interfaces to the observation type and observation sequence
type. An observation contains everything there is to know about an
observation including all metadata contained in the observation
definition and any number of copies of data associated with the
observation (for instance an actual observation, an ensemble of first
guess values, etc). An observation sequence is a time-ordered set of
observations that is defined by a linked list so that observations can
be easily added or deleted. A number of commands to extract observations
depending on the times at which they were taken are provided. For now,
the observations are only ordered by time, but the ability to add extra
sort keys could be added.

These routines are commonly used in conversion programs which read
observation data from various formats and create a DART observation
sequence in memory, and then write it out to a file. See the
[observations](../../../observations/obs_converters/observations.html)
directory for examples of programs which create and manipulate
observations using this routines.

[]{#OtherModulesUsed}\

OTHER MODULES USED
------------------

    types_mod
    location_mod (depends on model_choice)
    obs_def_mod
    time_manager_mod
    utilities_mod
    obs_kind_mod

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

PUBLIC INTERFACES
-----------------

  ---------------------------------- ----------------------------------------------------------
  *use obs\_sequence\_mod, only :*   [obs\_sequence\_type](#obs_sequence_type)
                                     [init\_obs\_sequence](#init_obs_sequence)
                                     [interactive\_obs\_sequence](#interactive_obs_sequence)
                                     [get\_num\_copies](#get_num_copies)
                                     [get\_num\_qc](#get_num_qc)
                                     [get\_num\_obs](#get_num_obs)
                                     [get\_max\_num\_obs](#get_max_num_obs)
                                     [get\_copy\_meta\_data](#get_copy_meta_data)
                                     [get\_qc\_meta\_data](#get_qc_meta_data)
                                     [get\_next\_obs](#get_next_obs)
                                     [get\_prev\_obs](#get_prev_obs)
                                     [get\_next\_obs\_from\_key](#get_next_obs_from_key)
                                     [get\_prev\_obs\_from\_key](#get_prev_obs_from_key)
                                     [insert\_obs\_in\_seq](#insert_obs_in_seq)
                                     [delete\_obs\_from\_seq](#delete_obs_from_seq)
                                     [set\_copy\_meta\_data](#set_copy_meta_data)
                                     [set\_qc\_meta\_data](#set_qc_meta_data)
                                     [get\_first\_obs](#get_first_obs)
                                     [get\_last\_obs](#get_last_obs)
                                     [add\_copies](#add_copies)
                                     [add\_qc](#add_qc)
                                     [write\_obs\_seq](#write_obs_seq)
                                     [read\_obs\_seq](#read_obs_seq)
                                     [append\_obs\_to\_seq](#append_obs_to_seq)
                                     [get\_obs\_from\_key](#get_obs_from_key)
                                     [get\_obs\_time\_range](#get_obs_time_range)
                                     [set\_obs](#set_obs)
                                     [get\_time\_range\_keys](#get_time_range_keys)
                                     [get\_num\_times](#get_num_times)
                                     [static\_init\_obs\_sequence](#static_init_obs_sequence)
                                     [destroy\_obs\_sequence](#destroy_obs_sequence)
                                     [read\_obs\_seq\_header](#read_obs_seq_header)
                                     [get\_expected\_obs](#get_expected_obs)
                                     [delete\_seq\_head](#delete_seq_head)
                                     [delete\_seq\_tail](#delete_seq_tail)
                                     
                                     LINKS BELOW FOR OBS\_TYPE INTERFACES
                                     
                                     [obs\_type](#obs_type)
                                     [init\_obs](#init_obs)
                                     [destroy\_obs](#destroy_obs)
                                     [get\_obs\_def](#get_obs_def)
                                     [set\_obs\_def](#set_obs_def)
                                     [get\_obs\_values](#get_obs_values)
                                     [set\_obs\_values](#set_obs_values)
                                     [replace\_obs\_values](#replace_obs_values)
                                     [get\_qc](#get_qc)
                                     [set\_qc](#set_qc)
                                     [replace\_qc](#replace_qc)
                                     [write\_obs](#write_obs)
                                     [read\_obs](#read_obs)
                                     [interactive\_obs](#interactive_obs)
                                     [copy\_obs](#copy_obs)
                                     [assignment(=)](#copy_obs)
  ---------------------------------- ----------------------------------------------------------

[]{#obs_sequence_type}\

<div class="type">

    type obs_sequence_type
       private
       integer                       :: num_copies
       integer                       :: num_qc
       integer                       :: num_obs
       integer                       :: max_num_obs
       character(len=64), pointer    :: copy_meta_data(:)
       character(len=64), pointer    :: qc_meta_data(:)
       integer                       :: first_time
       integer                       :: last_time
       type(obs_type), pointer       :: obs(:)
    end type obs_sequence_type

</div>

<div class="indent1">

The obs\_sequence type represents a series of observations including
multiple copies of data and quality control fields and complete metadata
about the observations. The sequence is organized as an integer pointer
linked list using a fixed array of storage for obs (type obs\_type).
Each observation points to the previous and next observation in time
order (additional sort keys could be added if needed) and has a unique
integer key (see obs\_type below). The maximum number of observations in
the sequence is represented in the type as max\_num\_obs, the current
number of observations is in num\_obs. The number of quality control
(qc) fields per observation is num\_qc and the number of data values
associated with each observation is num\_copies. Metadata for each copy
of the data is in copy\_meta\_data and metadata for the qc fields is in
qc\_meta\_data. The first and last pointers into the time linked list
are in first\_time and last\_time. A capability to write and read an
obs\_sequence structure to disk is available. At present, the entire
observation sequence is read in to core memory. An on-disk
implementation may be necessary for very large observational datasets.

  Component          Description
  ------------------ -----------------------------------------------------------------
  num\_copies        Number of data values associated with each observation.
  num\_qc            Number of qc fields associated with each observation.
  num\_obs           Number of observations currently in sequence.
  max\_num\_obs      Upper bounds on number of observations in sequence.
  copy\_meta\_data   Text describing each copy of data associated with observations.
  qc\_meta\_data     Text describing each quality control field.
  first\_time        Location of first observation in sequence.
  last\_time         Location of last observation in sequence.
  obs                Storage for all of the observations in the sequence.

</div>

\
[]{#obs_type}\

<div class="type">

    type obs_type
       private
       integer            :: key
       type(obs_def_type) :: def
       real(r8), pointer  :: values(:)
       real(r8), pointer  :: qc(:)
       integer            :: prev_time
       integer            :: next_time
       integer            :: cov_group
    end type obs_type

</div>

<div class="indent1">

Structure to represent everything known about a given observation and to
help with storing the observation in the observation sequence structure
(see above). The prev\_time and next\_time are integer pointers that
allow a linked list sorted on time to be constructed. If needed, other
sort keys could be introduced (for instance by time available?). Each
observation in a sequence has a unique key and each observation has an
obs\_def\_type that contains all the definition and metadata for the
observation. A set of values is associated with the observation along
with a set of qc fields. The cov\_group is not yet implemented but will
allow non-diagonal observation error covariances in a future release.

  -------------------------------------------------------------------------
  Component                            Description
  ------------------------------------ ------------------------------------
  key                                  
  Unique integer key when in an        
  obs\_sequence.                       

  def                                  
  The definition of the observation    
  (see obs\_def\_mod).                 

  values                               
  Values associated with the           
  observation.                         

  qc                                   
  Quality control fields associated    
  with the observation.                

  prev\_time                           
  When in an obs\_sequence, points to  
  previous time sorted observation.    

  next\_time                           
  When in an obs\_sequence, points to  
  next time sorted observation.        

  cov\_group                           
  Not currently implemented.           
  -------------------------------------------------------------------------

</div>

\
[]{#init_obs_sequence}\

<div class="routine">

*call init\_obs\_sequence(seq, num\_copies, num\_qc,
expected\_max\_num\_obs)*
    type(obs_sequence_type), intent(out) :: seq
    integer,                 intent(in)  :: num_copies
    integer,                 intent(in)  :: num_qc
    integer,                 intent(in)  :: expected_max_num_obs

</div>

<div class="indent1">

Constructor to create a variable of obs\_sequence\_type. This routine
must be called before using an obs\_sequence\_type. The number of copies
of the data to be associated with each observation (for instance the
observation from an instrument, an ensemble of prior guesses, etc.) and
the number of quality control fields associated with each observation
must be specified. Also, an estimated upper bound on the number of
observations to be stored in the sequence is helpful in making creation
of the sequence efficient.

  ----------------------------- ------------------------------------------------------------------------------
  *seq  *                       The observation sequence being constructed
  *num\_copies  *               Number of copies of data to be associated with each observation
  *num\_qc  *                   Number of quality control fields associated with each observation
  *expected\_max\_num\_obs  *   An estimate of the largest number of observations the sequence might contain
  ----------------------------- ------------------------------------------------------------------------------

</div>

\
[]{#interactive_obs_sequence}\

<div class="routine">

*var = interactive\_obs\_sequence()*
    type(obs_sequence_type) :: interactive_obs_sequence

</div>

<div class="indent1">

Uses input from standard in to create an observation sequence.
Initialization of the sequence is handled by the function.

  --------- -----------------------------------------------------
  *var  *   An observation sequence created from standard input
  --------- -----------------------------------------------------

</div>

\
[]{#get_num_copies}\

<div class="routine">

*var = get\_num\_copies(seq)*
    integer                             :: get_num_copies
    type(obs_sequence_type), intent(in) :: seq

</div>

<div class="indent1">

Returns number of copies of data associated with each observation in an
observation sequence.

  --------- -------------------------------------------------------------------------------
  *var  *   Returns number of copies of data associated with each observation in sequence
  *seq  *   An observation sequence
  --------- -------------------------------------------------------------------------------

</div>

\
[]{#get_num_qc}\

<div class="routine">

*var = get\_num\_qc(seq)*
    integer                             :: get_num_qc
    type(obs_sequence_type), intent(in) :: seq

</div>

<div class="indent1">

Returns number of quality control fields associated with each
observation in an observation sequence.

  --------- ---------------------------------------------------------------------------------------
  *var  *   Returns number of quality control fields associated with each observation in sequence
  *seq  *   An observation sequence
  --------- ---------------------------------------------------------------------------------------

</div>

\
[]{#get_num_obs}\

<div class="routine">

*var = get\_num\_obs(seq)*
    integer                             :: get_num_obs
    type(obs_sequence_type), intent(in) :: seq

</div>

<div class="indent1">

Returns number of observations currently in an observation sequence.

  --------- ---------------------------------------------------------------------
  *var  *   Returns number of observations currently in an observation sequence
  *seq  *   An observation sequence
  --------- ---------------------------------------------------------------------

</div>

\
[]{#get_max_num_obs}\

<div class="routine">

*var = get\_max\_num\_obs(seq)*
    integer                             :: get_max_num_obs
    type(obs_sequence_type), intent(in) :: seq

</div>

<div class="indent1">

Returns maximum number of observations an observation sequence can hold.

  --------- -------------------------------------------------------------------------
  *var  *   Returns maximum number of observations an observation sequence can hold
  *seq  *   An observation sequence
  --------- -------------------------------------------------------------------------

</div>

\
[]{#get_copy_meta_data}\

<div class="routine">

*var = get\_copy\_meta\_data(seq, copy\_num)*
    character(len=64)                   :: get_copy_meta_data
    type(obs_sequence_type), intent(in) :: seq
    integer,                 intent(in) :: copy_num

</div>

<div class="indent1">

Returns metadata associated with a given copy of data in an observation
sequence.

  --------------- -------------------------------------------------------------------------
  *var  *         Returns metadata associated with a copy of data in observation sequence
  *seq  *         An observation sequence
  *copy\_num  *   Return metadata for this copy
  --------------- -------------------------------------------------------------------------

</div>

\
[]{#get_qc_meta_data}\

<div class="routine">

*var = get\_qc\_meta\_data(seq,qc\_num)*
    character(len=64)                   :: get_qc_meta_data
    type(obs_sequence_type), intent(in) :: seq
    integer,                 intent(in) :: qc_num

</div>

<div class="indent1">

Returns metadata associated with a given copy of quality control fields
associated with observations in an observation sequence.

  ------------- --------------------------------------------------
  *var  *       Returns metadata associated with a given qc copy
  *seq  *       An observation sequence
  *qc\_num  *   Return metadata for this copy
  ------------- --------------------------------------------------

</div>

\
[]{#get_next_obs}\

<div class="routine">

*call get\_next\_obs(seq, obs, next\_obs, is\_this\_last)*
    type(obs_sequence_type), intent(in)  :: seq
    type(obs_type),          intent(in)  :: obs
    type(obs_type),          intent(out) :: next_obs
    logical,                 intent(out) :: is_this_last

</div>

<div class="indent1">

Given an observation in a sequence, returns the next observation in the
sequence. If there is no next observation, is\_this\_last is set to
true.

  -------------------- ------------------------------------------
  *seq  *              An observation sequence
  *obs  *              Find the next observation after this one
  *next\_obs  *        Return the next observation here
  *is\_this\_last  *   True if obs is the last obs in sequence
  -------------------- ------------------------------------------

</div>

\
[]{#get_prev_obs}\

<div class="routine">

*call get\_prev\_obs(seq, obs, prev\_obs, is\_this\_first)*
    type(obs_sequence_type), intent(in)  :: seq
    type(obs_type),          intent(in)  :: obs
    type(obs_type),          intent(out) :: prev_obs
    logical,                 intent(out) :: is_this_first

</div>

<div class="indent1">

Given an observation in a sequence, returns the previous observation in
the sequence. If there is no previous observation, is\_this\_first is
set to true.

  --------------------- -----------------------------------------------
  *seq  *               An observation sequence
  *obs  *               Find the previous observation before this one
  *prev\_obs  *         Return the previous observation here
  *is\_this\_first  *   True if obs is the first obs in sequence
  --------------------- -----------------------------------------------

</div>

\
[]{#get_next_obs_from_key}\

<div class="routine">

*call get\_next\_obs\_from\_key(seq, last\_key\_used, next\_obs,
is\_this\_last)*
    type(obs_sequence_type), intent(in)  :: seq
    integer,                 intent(in)  :: last_key_used
    type(obs_type),          intent(out) :: next_obs
    logical,                 intent(out) :: is_this_last

</div>

<div class="indent1">

Given the last key used in a sequence, returns the next observation in
the sequence. If there is no next observation, is\_this\_last is set to
true.

  --------------------- ------------------------------------------
  *seq  *               An observation sequence
  *last\_key\_used  *   Find the next observation after this key
  *next\_obs  *         Return the next observation here
  *is\_this\_last  *    True if obs is the last obs in sequence
  --------------------- ------------------------------------------

</div>

\
[]{#get_prev_obs_from_key}\

<div class="routine">

*call get\_prev\_obs\_from\_key(seq, last\_key\_used, prev\_obs,
is\_this\_first)*
    type(obs_sequence_type), intent(in)  :: seq
    integer,                 intent(in)  :: last_key_used
    type(obs_type),          intent(out) :: prev_obs
    logical,                 intent(out) :: is_this_first

</div>

<div class="indent1">

Given the last key used in a sequence, returns the previous observation
in the sequence. If there is no previous observation, is\_this\_first is
set to true.

  --------------------- -----------------------------------------------
  *seq  *               An observation sequence
  *last\_key\_used  *   Find the previous observation before this key
  *prev\_obs  *         Return the previous observation here
  *is\_this\_first  *   True if obs is the first obs in sequence
  --------------------- -----------------------------------------------

</div>

\
[]{#get_obs_from_key}\

<div class="routine">

*call get\_obs\_from\_key(seq, key, obs)*
    type(obs_sequence_type), intent(in)  :: seq
    integer,                 intent(in)  :: key
    type(obs_type),          intent(out) :: obs

</div>

<div class="indent1">

Each entry in an observation sequence has a unique integer key. This
subroutine returns the observation given an integer key.

  --------- --------------------------------------
  *seq  *   An observation sequence
  *key  *   Return the observation with this key
  *obs  *   The returned observation
  --------- --------------------------------------

</div>

\
[]{#insert_obs_in_seq}\

<div class="routine">

*call insert\_obs\_in\_seq(seq, obs *\[, prev\_obs\]*)*
    type(obs_sequence_type),  intent(inout) :: seq
    type(obs_type),           intent(inout) :: obs
    type(obs_type), optional, intent(in)    :: prev_obs

</div>

<div class="indent1">

Inserts an observation in a sequence in appropriate time order. If the
optional argument prev\_obs is present, the new observation is inserted
directly after the prev\_obs. If an incorrect prev\_obs is provided so
that the sequence is no longer time ordered, bad things will happen.

  --------------- -------------------------------------------------------------------------
  *seq  *         An observation sequence
  *obs  *         An observation to be inserted in the sequence
  *prev\_obs  *   If present, says the new observation belongs immediately after this one
  --------------- -------------------------------------------------------------------------

</div>

\
[]{#delete_obs_from_seq}\

<div class="routine">

*call delete\_obs\_from\_seq(seq, obs)*
    type(obs_sequence_type), intent(inout) :: seq
    type(obs_type),          intent(inout) :: obs

</div>

<div class="indent1">

Given an observation and a sequence, removes the observation with the
same key from the observation sequence.

  --------- -------------------------------------------------
  *seq  *   An observation sequence
  *obs  *   The observation to be deleted from the sequence
  --------- -------------------------------------------------

</div>

\
[]{#set_copy_meta_data}\

<div class="routine">

*call set\_copy\_meta\_data(seq, copy\_num, meta\_data)*
    type(obs_sequence_type), intent(inout) :: seq
    integer,                 intent(in)    :: copy_num
    character(len=64),       intent(in)    :: meta_data

</div>

<div class="indent1">

Sets the copy metadata for this copy of the observations in an
observation sequence.

  ---------------- ------------------------------------
  *seq  *          An observation sequence
  *copy\_num  *    Set metadata for this copy of data
  *meta\_data  *   The metadata
  ---------------- ------------------------------------

</div>

\
[]{#set_qc_meta_data}\

<div class="routine">

*call set\_qc\_meta\_data(seq, qc\_num, meta\_data)*
    type(obs_sequence_type), intent(inout) :: seq
    integer,                 intent(in)    :: qc_num
    character(len=64),       intent(in)    :: meta_data

</div>

<div class="indent1">

Sets the quality control metadata for this copy of the qc in an
observation sequence.

  ---------------- ---------------------------------------------
  *seq  *          An observation sequence
  *qc\_num  *      Set metadata for this quality control field
  *meta\_data  *   The metadata
  ---------------- ---------------------------------------------

</div>

\
[]{#get_first_obs}\

<div class="routine">

*var = get\_first\_obs(seq, obs)*
    logical                              :: get_first_obs
    type(obs_sequence_type), intent(in)  :: seq
    type(obs_type),          intent(out) :: obs

</div>

<div class="indent1">

Returns the first observation in a sequence. If there are no
observations in the sequence, the function returns false, else true.

  --------- -----------------------------------------------
  *var  *   Returns false if there are no obs in sequence
  *seq  *   An observation sequence
  *obs  *   The first observation in the sequence
  --------- -----------------------------------------------

</div>

\
[]{#get_last_obs}\

<div class="routine">

*var = get\_last\_obs(seq, obs)*
    logical                              :: get_last_obs
    type(obs_sequence_type), intent(in)  :: seq
    type(obs_type),          intent(out) :: obs

</div>

<div class="indent1">

Returns the last observation in a sequence. If there are no observations
in the sequence, the function returns false, else true.

  --------- -----------------------------------------------
  *var  *   Returns false if there are no obs in sequence
  *seq  *   An observation sequence
  *obs  *   The last observation in the sequence
  --------- -----------------------------------------------

</div>

\
[]{#add_copies}\

<div class="routine">

*call add\_copies(seq, num\_to\_add)*
    type(obs_sequence_type), intent(inout) :: seq
    integer,                 intent(in)    :: num_to_add

</div>

<div class="indent1">

Increases the number of copies of data associated with each observation
by num\_to\_add. The current implementation re-creates the entire
observation sequence by deallocating and reallocating each entry with a
larger size.

  ------------------ ---------------------------------
  *seq  *            An observation sequence
  *num\_to\_add  *   Number of copies of data to add
  ------------------ ---------------------------------

</div>

\
[]{#add_qc}\

<div class="routine">

*call add\_qc(seq, num\_to\_add)*
    type(obs_sequence_type), intent(inout) :: seq
    integer,                 intent(in)    :: num_to_add

</div>

<div class="indent1">

Increases the number of quality control fields associated with each
observation by num\_to\_add. The current implementation re-creates the
entire observation sequence by deallocating and reallocating each entry
with a larger size.

  ------------------ -----------------------------------------
  *seq  *            An observation sequence
  *num\_to\_add  *   Number of quality control fields to add
  ------------------ -----------------------------------------

</div>

\
[]{#read_obs_seq}\

<div class="routine">

*call read\_obs\_seq(file\_name, add\_copies, add\_qc, add\_obs, seq)*
    character(len=*),        intent(in)  :: file_name
    integer,                 intent(in)  :: add_copies
    integer,                 intent(in)  :: add_qc
    integer,                 intent(in)  :: add_obs
    type(obs_sequence_type), intent(out) :: seq

</div>

<div class="indent1">

Read an observation sequence from *file\_name*. The sequence will have
enough space for the number of observations in the file plus any
additional space requested by the "add\_xx" args. It is more efficient
to allocate the additional space at create time rather than try to add
it in later. The arguments can specify that the caller wants to add
additional data copies associated with each observation, or to add
additional quality control fields, or to add space for additional
observations. The format of the file (`formatted` vs. `unformatted`) has
been automatically detected since the I release. The obs\_sequence file
format with I and later releases has a header that associates
observation type strings with an integer which was not present in
previous versions. I format files are no longer supported.

  ----------------- -----------------------------------------------------------------------------------
  *file\_name  *    Read from this file
  *add\_copies  *   Add this number of copies of data to the obs\_sequence on file
  *add\_qc  *       Add this number of qc fields to the obs\_sequence on file
  *add\_obs  *      Add space for this number of additional observations to the obs\_sequence on file
  *seq  *           The observation sequence read in with any additional space
  ----------------- -----------------------------------------------------------------------------------

</div>

\
[]{#write_obs_seq}\

<div class="routine">

*call write\_obs\_seq(seq, file\_name)*
    type(obs_sequence_type), intent(in) :: seq
    character(len=*),        intent(in) :: file_name

</div>

<div class="indent1">

Write the observation sequence to file file\_name. The format is
controlled by the namelist parameter write\_binary\_obs\_sequence.

  ---------------- ---------------------------------
  *seq  *          An observation sequence
  *file\_name  *   Write the sequence to this file
  ---------------- ---------------------------------

</div>

\
[]{#set_obs}\

<div class="routine">

*call set\_obs(seq,obs *\[, key\_in\]*)*
    type(obs_sequence_type), intent(inout) :: seq
    type(obs_type),          intent(in)    :: obs
    integer, optional,       intent(in)    :: key_in

</div>

<div class="indent1">

Given an observation, copies this observation into the observation
sequence using the key specified in the observation. If the optional
key\_in argument is present, the observation is instead copied into this
element of the observation sequence (and the key is changed to be
key\_in).

  ------------- -------------------------------------------------------------
  *seq  *       An observation sequence
  *obs  *       Observation to be put in sequence
  *key\_in  *   If present, the obs is copied into this key of the sequence
  ------------- -------------------------------------------------------------

</div>

\
[]{#append_obs_to_seq}\

<div class="routine">

*call append\_obs\_to\_seq(seq, obs)*
    type(obs_sequence_type), intent(inout) :: seq
    type(obs_type),          intent(inout) :: obs

</div>

<div class="indent1">

Append an observation to an observation sequence. An error results if
the time of the observation is not equal to or later than the time of
the last observation currently in the sequence.

  --------- -----------------------------------------
  *seq  *   An observation sequence
  *obs  *   Append this observation to the sequence
  --------- -----------------------------------------

</div>

\
[]{#get_obs_time_range}\

<div class="routine">

*call get\_obs\_time\_range(seq, time1, time2, key\_bounds, num\_keys,
out\_of\_range *\[, obs\]*)*
    type(obs_sequence_type),  intent(in)  :: seq
    type(time_type),          intent(in)  :: time1
    type(time_type),          intent(in)  :: time2
    integer, dimension(2),    intent(out) :: key_bounds
    integer,                  intent(out) :: num_keys
    logical,                  intent(out) :: out_of_range
    type(obs_type), optional, intent(in)  :: obs

</div>

<div class="indent1">

Given a time range specified by a beginning and ending time, find the
keys that bound all observations in this time range and the number of
observations in the time range. The routine get\_time\_range\_keys can
then be used to get a list of all the keys in the range if desired. The
logical out\_of\_range is returned as true if the beginning time of the
time range is after the time of the latest observation in the sequence.
The optional argument obs can increase the efficiency of the search
through the sequence by indicating that all observations before obs are
definitely at times before the start of the time range.

  -------------------- --------------------------------------------------------------------------------------
  *seq  *              An observation sequence
  *time1  *            Lower time bound
  *time2  *            Upper time bound
  *key\_bounds  *      Lower and upper bounds on keys that are in the time range
  *num\_keys  *        Number of keys in the time range
  *out\_of\_range  *   Returns true if the time range is entirely past the time of the last obs in sequence
  *obs  *              If present, can start search for time range from this observation
  -------------------- --------------------------------------------------------------------------------------

</div>

\
[]{#get_time_range_keys}\

<div class="routine">

*call get\_time\_range\_keys(seq, key\_bounds, num\_keys, keys)*
    type(obs_sequence_type),      intent(in)  :: seq
    integer, dimension(2),        intent(in)  :: key_bounds
    integer,                      intent(in)  :: num_keys
    integer, dimension(num_keys), intent(out) :: keys

</div>

<div class="indent1">

Given the keys of the observations at the start and end of a time range
and the number of observations in the time range (these are returned by
*get\_obs\_time\_range()*), return a list of the keys of all
observations in the time range. Combining the two routines allows one to
get a list of all observations in any time range by key. The *keys*
array must be at least *num\_keys* long to hold the return values.

  ----------------- ----------------------------------------------------
  *seq  *           An observation sequence
  *key\_bounds  *   Keys of first and last observation in a time range
  *num\_keys  *     Number of obs in the time range
  *keys  *          Output list of keys of all obs in the time range
  ----------------- ----------------------------------------------------

</div>

\
[]{#get_num_times}\

<div class="routine">

*var = get\_num\_times(seq)*
    integer                             :: get_num_times
    type(obs_sequence_type), intent(in) :: seq

</div>

<div class="indent1">

Returns the number of unique times associated with observations in an
observation sequence.

  --------- -------------------------------------------------------
  *var  *   Number of unique times for observations in a sequence
  *seq  *   An observation sequence
  --------- -------------------------------------------------------

</div>

\
[]{#get_num_key_range}\

<div class="routine">

*var = get\_num\_key\_range(seq, key1, key2)*
    integer                             :: get_num_key_range
    type(obs_sequence_type), intent(in) :: seq
    integer, optional,       intent(in) :: key1, key2

</div>

<div class="indent1">

Returns the number of observations between the two given keys. The
default key numbers are the first and last in the sequence file. This
routine can be used to count the actual number of observations in a
sequence and will be accurate even if the sequence has been trimmed with
delete\_seq\_head() or delete\_seq\_tail().

  ---------- -----------------------------------------------------------------------------
  *var  *    Number of unique times for observations in a sequence
  *seq  *    An observation sequence
  *key1  *   The starting key number. Defaults to the first observation in the sequence.
  *key2  *   The ending key number. Defaults to the last observation in the sequence.
  ---------- -----------------------------------------------------------------------------

</div>

\
[]{#static_init_obs_sequence}\

<div class="routine">

*call static\_init\_obs\_sequence()*

</div>

<div class="indent1">

Initializes the obs\_sequence module and reads namelists. This MUST BE
CALLED BEFORE USING ANY OTHER INTERFACES.

</div>

\
[]{#destroy_obs_sequence}\

<div class="routine">

*call destroy\_obs\_sequence(seq)*
    type(obs_sequence_type), intent(inout) :: seq

</div>

<div class="indent1">

Releases all allocated storage associated with an observation sequence.

  --------- -------------------------
  *seq  *   An observation sequence
  --------- -------------------------

</div>

\
[]{#read_obs_seq_header}\

<div class="routine">

*call read\_obs\_seq\_header(file\_name, num\_copies, num\_qc, num\_obs,
max\_num\_obs, file\_id, read\_format, pre\_I\_format
*\[, close\_the\_file\]*)*
    character(len=*),   intent(in)  :: file_name
    integer,            intent(out) :: num_copies
    integer,            intent(out) :: num_qc
    integer,            intent(out) :: num_obs
    integer,            intent(out) :: max_num_obs
    integer,            intent(out) :: file_id
    character(len=*),   intent(out) :: read_format
    logical,            intent(out) :: pre_I_format
    logical, optional,  intent(in)  :: close_the_file

</div>

<div class="indent1">

Allows one to see the global metadata associated with an observation
sequence that has been written to a file without reading the whole file.

  ---------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------
  *file\_name  *         File contatining an obs\_sequence
  *num\_copies  *        Number of copies of data associated with each observation
  *num\_qc  *            Number of quality control fields associated with each observation
  *num\_obs  *           Number of observations in sequence
  *max\_num\_obs  *      Maximum number of observations sequence could hold
  *file\_id  *           File channel/descriptor returned from opening the file
  *read\_format  *       Either the string `'unformatted'` or `'formatted'`
  *pre\_I\_format  *     Returns .true. if the file was written before the observation type string/index number table was added to the standard header starting with the I release.
  *close\_the\_file  *   If specified and .TRUE. close the file after the header has been read. The default is to leave the file open.
  ---------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#init_obs}\

<div class="routine">

*call init\_obs(obs, num\_copies, num\_qc)*
    type(obs_type), intent(out) :: obs
    integer,        intent(in)  :: num_copies
    integer,        intent(in)  :: num_qc

</div>

<div class="indent1">

Initializes an obs\_type variable. This allocates storage for the
observation type and creates the appropriate obs\_def\_type and related
structures. IT IS ESSENTIAL THAT OBS\_TYPE VARIABLES BE INITIALIZED
BEFORE USE.

  ----------------- ------------------------------------------------------
  *obs  *           An obs\_type data structure to be initialized
  *num\_copies  *   Number of copies of data associated with observation
  *num\_qc  *       Number of qc fields associated with observation
  ----------------- ------------------------------------------------------

</div>

\
[]{#destroy_obs}\

<div class="routine">

*call destroy\_obs(obs)*
    type(obs_type), intent(inout) :: obs

</div>

<div class="indent1">

Destroys an observation variable by releasing all associated storage.

  --------- -----------------------------------------
  *obs  *   An observation variable to be destroyed
  --------- -----------------------------------------

</div>

\
[]{#get_obs_def}\

<div class="routine">

*call get\_obs\_def(obs, obs\_def)*
    type(obs_type),     intent(in)  :: obs
    type(obs_def_type), intent(out) :: obs_def

</div>

<div class="indent1">

Extracts the definition portion of an observation.

  -------------- -------------------------------------------
  *obs  *        An observation
  *obs\_def  *   The definition portion of the observation
  -------------- -------------------------------------------

</div>

\
[]{#set_obs_def}\

<div class="routine">

*call set\_obs\_def(obs, obs\_def)*
    type(obs_type),     intent(out) :: obs
    type(obs_def_type), intent(in)  :: obs_def

</div>

<div class="indent1">

Given an observation and an observation definition, insert the
definition in the observation structure.

  -------------- ---------------------------------------------------------
  *obs  *        An observation whose definition portion will be updated
  *obs\_def  *   The observation definition that will be inserted in obs
  -------------- ---------------------------------------------------------

</div>

\
[]{#get_obs_values}\

<div class="routine">

*call get\_obs\_values(obs, values *\[, copy\_indx\]*)*
    type(obs_type),         intent(in)  :: obs
    real(r8), dimension(:), intent(out) :: values
    integer, optional,      intent(in)  :: copy_indx

</div>

<div class="indent1">

Extract copies of the data from an observation. If *copy\_indx* is
present extract a single value indexed by *copy\_indx* into *values(1)*.
 *copy\_indx* must be between 1 and `num_copies`, inclusive. If
*copy\_indx* is not present extract all copies of data into the *values*
array which must be `num_copies` long (See
[*get\_num\_copies*](#get_num_copies).)

  ---------------- -----------------------------------------------------------------
  *obs  *          Observation from which to extract values
  *values  *       The values extracted
  *copy\_indx  *   If present extract only this copy, otherwise extract all copies
  ---------------- -----------------------------------------------------------------

</div>

\
[]{#get_qc}\

<div class="routine">

*call get\_qc(obs, qc *\[, qc\_indx\]*)*
    type(obs_type),         intent(in)  :: obs
    real(r8), dimension(:), intent(out) :: qc
    integer, optional,      intent(in)  :: qc_indx

</div>

<div class="indent1">

Extract quality control fields from an observation. If *qc\_indx* is
present extract a single field indexed by *qc\_indx* into *qc(1)*.
 *qc\_indx* must be between 1 and `num_qc`, inclusive. If *qc\_indx* is
not present extract all quality control fields into the *qc* array which
must be `num_qc` long (See [*get\_num\_qc*](#get_num_qc).)

  -------------- ---------------------------------------------------------------------
  *obs  *        Observation from which to extract qc field(s)
  *qc  *         Extracted qc fields
  *qc\_indx  *   If present extract only this field, otherwise extract all qc fields
  -------------- ---------------------------------------------------------------------

</div>

\
[]{#set_obs_values}\

<div class="routine">

*call set\_obs\_values(obs, values *\[, copy\_indx\]*)*
    type(obs_type),         intent(out) :: obs
    real(r8), dimension(:), intent(in)  :: values
    integer, optional,      intent(in)  :: copy_indx

</div>

<div class="indent1">

Set value(s) of data in this observation. If *copy\_indx* is present set
the single value indexed by *copy\_indx* to *values(1)*.  *copy\_indx*
must be between 1 and `num_copies`, inclusive. If *copy\_indx* is not
present set all copies of data from the *values* array which must be
`num_copies` long (See [*get\_num\_copies*](#get_num_copies).)

  ---------------- -----------------------------------------------------------------
  *obs  *          Observation whose values are being set
  *values  *       Array of value(s) to be set
  *copy\_indx  *   If present set only this copy of data, otherwise set all copies
  ---------------- -----------------------------------------------------------------

</div>

\
[]{#replace_obs_values}\

<div class="routine">

*call replace\_obs\_values(seq, key, values *\[, copy\_indx\]*)*
    type(obs_sequence_type), intent(inout) :: seq
    integer,                 intent(in)    :: key
    real(r8), dimension(:),  intent(in)    :: values
    integer, optional,       intent(in)    :: copy_indx

</div>

<div class="indent1">

Set value(s) of data in the observation from a sequence with the given
*key*. If *copy\_indx* is present set the single value indexed by
*copy\_indx* to *values(1)*.  *copy\_indx* must be between 1 and
`num_copies`, inclusive. If *copy\_indx* is not present set all copies
of data from the *values* array which must be `num_copies` long (See
[*get\_num\_copies*](#get_num_copies).)

  ---------------- -----------------------------------------------------------------
  *seq  *          Sequence which contains observation to update
  *key  *          Key to select which observation
  *values  *       Array of value(s) to be set
  *copy\_indx  *   If present set only this copy of data, otherwise set all copies
  ---------------- -----------------------------------------------------------------

</div>

\
[]{#set_qc}\

<div class="routine">

*call set\_qc(obs, qc *\[, qc\_indx\]*)*
    type(obs_type),         intent(out) :: obs
    real(r8), dimension(:), intent(in)  :: qc
    integer, optional,      intent(in)  :: qc_indx

</div>

<div class="indent1">

Sets the quality control fields in an observation. If *qc\_indx* is
present set a single field indexed by *qc\_indx* to *qc(1)*.  *qc\_indx*
must be between 1 and `num_qc`, inclusive. If *qc\_indx* is not present
set all quality control fields from the *qc* array which must be
`num_qc` long (See [*get\_num\_qc*](#get_num_qc).)

  -------------- -------------------------------------------------------------------
  *obs  *        Observation having its qc fields set
  *qc  *         Input values of qc fields
  *qc\_indx  *   If present update only this field, otherwise update all qc fields
  -------------- -------------------------------------------------------------------

</div>

\
[]{#replace_qc}\

<div class="routine">

*call replace\_qc(seq, key, qc *\[, qc\_indx\]*)*
    type(obs_sequence_type), intent(inout) :: seq
    integer,                 intent(in)    :: key
    real(r8), dimension(:),  intent(in)    :: qc
    integer, optional,       intent(in)    :: qc_indx

</div>

<div class="indent1">

Set value(s) of the quality control fields in the observation from a
sequence with the given *key*. If *qc\_indx* is present set the single
value indexed by *qc\_indx* to *qc(1)*.  *qc\_indx* must be between 1
and `num_qc`, inclusive. If *qc\_indx* is not present set all quality
control fields from the *qc* array which must be `num_qc` long (See
[*get\_num\_qc*](#get_num_qc).)

  -------------- --------------------------------------------------------------------
  *seq  *        Observation sequence containing observation to update
  *key  *        Key to select which observation
  *qc  *         Input values of qc fields
  *qc\_indx  *   If present, only update single qc field, else update all qc fields
  -------------- --------------------------------------------------------------------

</div>

\
[]{#write_obs}\

<div class="routine">

*call write\_obs(obs, file\_id, num\_copies, num\_qc)*
    type(obs_type), intent(in) :: obs
    integer,        intent(in) :: file_id
    integer,        intent(in) :: num_copies
    integer,        intent(in) :: num_qc

</div>

<div class="indent1">

Writes an observation and all its associated metadata to a disk file
that has been opened with a format consistent with the namelist
parameter `write_binary_obs_sequence`.

  ----------------- ---------------------------------------------------------------------------
  *obs  *           Observation to be written to file
  *file\_id  *      Channel open to file for writing
  *num\_copies  *   The number of copies of data associated with the observation to be output
  *num\_qc  *       The number of qc fields associated with the observation to be output
  ----------------- ---------------------------------------------------------------------------

</div>

\
[]{#read_obs}\

<div class="routine">

*call read\_obs(file\_id, num\_copies, add\_copies, num\_qc, add\_qc,
key, obs, read\_format *\[, max\_obs\]*)*
    integer,            intent(in)    :: file_id
    integer,            intent(in)    :: num_copies
    integer,            intent(in)    :: add_copies
    integer,            intent(in)    :: num_qc
    integer,            intent(in)    :: add_qc
    integer,            intent(in)    :: key
    type(obs_type),     intent(inout) :: obs
    character(len=*),   intent(in)    :: read_format
    integer, optional,  intent(in)    :: max_obs

</div>

<div class="indent1">

Reads an observation from an obs\_sequence file. The number of copies of
data and the number of qc values associated with each observation must
be provided. If additional copies of data or additional qc fields are
needed, arguments allow them to be added. WARNING: The key argument is
no longer used and should be removed.

  ------------------ --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *file\_id  *       Channel open to file from which to read
  *num\_copies  *    Number of copies of data associated with observation in file
  *add\_copies  *    Number of additional copies of observation to be added
  *num\_qc  *        Number of qc fields associated with observation in file
  *add\_qc  *        Number of additional qc fields to be added
  *key  *            No longer used, should be deleted
  *obs  *            The observation being read in
  *read\_format  *   Either the string `'formatted'` or `'unformatted'`
  *max\_obs  *       If present, specifies the largest observation key number in the sequence. This is used only for additional error checks on the next and previous obs linked list values.
  ------------------ --------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#interactive_obs}\

<div class="routine">

*call interactive\_obs(num\_copies, num\_qc, obs, key)*
    integer,        intent(in)    :: num_copies
    integer,        intent(in)    :: num_qc
    type(obs_type), intent(inout) :: obs
    integer,        intent(in)    :: key

</div>

<div class="indent1">

Use standard input to create an observation. The number of values,
number of qc fields, and an observation type-specific key associated
with the observation are input. (Note that the key here is not the same
as the key in an observation sequence.)

  ----------------- --------------------------------------------------------------------------------------------------------
  *num\_copies  *   Number of copies of data to be associated with observation
  *num\_qc  *       Number of qc fields to be associated with observation
  *obs  *           Observation created via standard input
  *key  *           An observation type-specific key can be associated with each observation for use by the obs\_def code.
  ----------------- --------------------------------------------------------------------------------------------------------

</div>

\
[]{#copy_obs}\

<div class="routine">

*call copy\_obs(obs1, obs2)*
    type(obs_type), intent(out) :: obs1
    type(obs_type), intent(in)  :: obs2

</div>

<div class="indent1">

Copies the observation type obs2 to obs1. If the sizes of obs fields are
not compatible, the space in obs1 is deallocated and reallocated with
the appropriate size. This is overloaded to assignment(=).

  ---------- ---------------------------------
  *obs1  *   Copy obs2 to here (destination)
  *obs2  *   Copy into obs1 (source)
  ---------- ---------------------------------

</div>

\
[]{#get_expected_obs}\

<div class="routine">

*call get\_expected\_obs\_from\_def\_distrib\_state(state\_handle,
ens\_size, copy\_indices, key, & obs\_def, obs\_kind\_ind, state\_time,
isprior, assimilate\_this\_ob, evaluate\_this\_ob, expected\_obs, &
istatus)*
    type(ensemble_type), intent(in)  :: state_handle
    integer,             intent(in)  :: ens_size
    integer,             intent(in)  :: copy_indices(ens_size)
    integer,             intent(in)  :: key
    type(obs_def_type),  intent(in)  :: obs_def
    integer,             intent(in)  :: obs_kind_ind
    type(time_type),     intent(in)  :: state_time
    logical,             intent(in)  :: isprior
    integer,             intent(out) :: istatus(ens_size)
    logical,             intent(out) :: assimilate_this_ob, evaluate_this_ob
    real(r8),            intent(out) :: expected_obs(ens_size)

</div>

<div class="indent1">

Used to compute the expected value of a set of observations in an
observation sequence given a model state vector. Also returns a status
variable that reports on problems taking forward operators. This version
returns forward operator values for the entire ensemble in a single
call.

  ------------------------ ------------------------------------------------------------------------------
  *state\_handle*          An observation sequence
  *keys*                   List of integer keys that specify observations in seq
  *ens\_index*             The ensemble number for this state vector
  *state*                  Model state vector
  *state\_time*            The time of the state data
  *obs\_vals*              Returned expected values of the observations
  *istatus*                Integer error code for use in quality control (0 means no error)
  *assimilate\_this\_ob*   Returns true if this observation type is being assimilated
  *evaluate\_this\_ob*     Returns true if this observation type is being evaluated but not assimilated
  ------------------------ ------------------------------------------------------------------------------

</div>

\
[]{#delete_seq_head}\

<div class="routine">

*call delete\_seq\_head(first\_time, seq, all\_gone)*
    type(time_type),         intent(in)    :: first_time
    type(obs_sequence_type), intent(inout) :: seq
    logical,                 intent(out)   :: all_gone

</div>

<div class="indent1">

Deletes all observations in the sequence with times before first\_time.
If no observations remain, return all\_gone as .true. If no observations
fall into the time window (e.g. all before first\_time or empty sequence
to begin with), no deletions are done and all\_gone is simply returned
as .true.

  ----------------- ---------------------------------------------------------------------------------------------
  *first\_time  *   Delete all observations with times before this
  *seq  *           An observation sequence
  *all\_gone  *     Returns true if there are no valid observations remaining in the sequence after first\_time
  ----------------- ---------------------------------------------------------------------------------------------

</div>

\
[]{#delete_seq_tail}\

<div class="routine">

*call delete\_seq\_tail(last\_time, seq, all\_gone)*
    type(time_type),         intent(in)    :: last_time
    type(obs_sequence_type), intent(inout) :: seq
    logical,                 intent(out)   :: all_gone

</div>

<div class="indent1">

Deletes all observations in the sequence with times after last\_time. If
no observations remain, return all\_gone as .true. If no observations
fall into the time window (e.g. all after last\_time or empty sequence
to begin with), no deletions are done and all\_gone is simply returned
as .true.

  ---------------- ---------------------------------------------------------------------------------------------
  *last\_time  *   Delete all observations with times after this
  *seq  *          An observation sequence
  *all\_gone  *    Returns true if there are no valid observations remaining in the sequence before last\_time
  ---------------- ---------------------------------------------------------------------------------------------

</div>

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

    &obs_sequence_nml
       write_binary_obs_sequence = .false.
       read_binary_file_format   = 'native'
      /

</div>

\
\

<div>

  Item                           Type                Description
  ------------------------------ ------------------- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  write\_binary\_obs\_sequence   logical             If true, write binary obs\_sequence files. If false, write ascii obs\_sequence files.
  read\_binary\_file\_format     character(len=32)   The 'endian'ness of binary obs\_sequence files. May be 'native' (endianness matches hardware default), 'big-endian', 'little-endian', and possibly 'cray'. Ignored if observation sequence files are ASCII.

</div>

\
\
[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

FILES
-----

-   obs\_sequence\_mod.nml in input.nml
-   Files for reading and writing obs\_sequences and obs specified in
    filter\_nml.

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
insert\_obs\_in\_seq
ran out of room, num\_obs \# &gt; max\_num\_obs \#
Overflowed number of obs in sequence. Called from many public entries.
append\_obs\_to\_seq
tried to append an obs to sequence with bad time
Tried to append an obs with earlier time than last obs in sequence.
append\_obs\_to\_seq
ran out of room, max\_num\_obs = \#
Overflowed the obs sequence.

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

Future versions should automate file reading and only the write namelist
parameter should remain.

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


