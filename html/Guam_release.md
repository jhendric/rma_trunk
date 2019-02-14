[]{#TOP}

DART "Guam release" Documentation
=================================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../images/Dartboard7.png | Index](../../index.html)\         |
| ){height="70"}                    | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[]{#OVERVIEW}

Overview of DART
================

The Data Assimilation Research Testbed (DART) is designed to facilitate
the combination of assimilation algorithms, models, and observation sets
to allow increased understanding of all three. The DART programs have
been compiled with the Intel 7.1 Fortran compiler and run on a linux
compute-server. If your system is different, you will definitely need to
read the [Customizations](#customizations) section.

[The latest software release -- "guam".](/pub/DART/DART_guam.tar.gz)

The **guam** release provides several new models and has a fundamentally
different implementation of the low-level routines that will now operate
on a much broader class of observations. As such, the first two program
units have changed somewhat. The strategy of these two programs units is
still the same; declare the characteristics of the observations and then
declare the temporal nature of the observations. These are still
destined to be run as a back-end to some sort of GUI-driven interface,
so virtually no work has been done to change the level of user
interaction required to run these programs.

The guam release has also been tested with more compilers in an attempt
to determine non-portable code elements. It is my experience that the
largest impediment to portable code is the reliance on the compiler to
autopromote `real` variables to one flavor or another. [Different
compilers perform this autopromotion in different
ways](/DART/PrecisionNotes.html) Using the F90 "kind" allows for much
more flexible code, in that the use of interface procedures is possible
only when two routines do not have identical sets of input arguments --
something that happens when the compiler autopromotes 32bit reals to
64bit reals, for example.

DART programs can require three different types of input. First, some of
the DART programs, those for creating synthetic observational datasets,
require interactive input from the keyboard. For simple cases, this
interactive input can be made directly from the keyboard. In more
complicated cases, a file containing the appropriate keyboard input can
be created and this file can be directed to the standard input of the
DART program. Second, many DART programs expect one or more input files
in DART specific formats to be available. For instance,
*perfect\_model\_obs* creates a synthetic observation set given a
particular model and a description of a sequence of observations
requires an input file that describes this observation sequence. At
present, the observation files for DART are inefficient but
human-readable ascii files in a custom format. Third, many DART modules
(including main programs) make use of the Fortan90 namelist facility to
obtain values of certain parameters at run-time. All programs look for a
namelist input file called *input.nml* in the directory in which the
program is executed. The *input.nml* file can contain a sequence of
individual Fortran90 namelists which specify values of particular
parameters for modules that compose the executable program.
Unfortunately, the Fortran90 namelist interface is poorly defined in the
language standard, leaving considerable leeway to compiler developers in
implementing the facility. The Intel 7.1 compiler has some particularly
unpleasant behavior when a namelist file contains an entry that is NOT
defined in the program reading the namelist. Error behavior is
unpredictable, but often results in read errors for other input files
opened by DART programs. If you encounter run-time read errors, the
first course of action should be to ensure the components of the
namelist are actual components. Changing the names of the namelist
components **will** create unpleasantries. DART provides a mechanism
that automatically generates namelists with the default values for each
program to be run.

DART uses the [netCDF](http://www.unidata.ucar.edu/packages/netcdf/)
self-describing data format with a particular metadata convention to
describe output that is used to analyze the results of assimilation
experiments. These files have the extension *.nc* and can be read by a
number of standard data analysis tools. A set of
[Matlab](http://www.mathworks.com/) scripts, designed to produce
graphical diagnostics from DART netCDF output files are available. DART
users have also used
[ncview](http://meteora.ucsd.edu/~pierce/ncview_home_page.html) to
create rudimentary graphical displays of output data fields. The
[NCO](http://nco.sourceforge.net) tools, produced by UCAR's Unidata
group, are available to do operations like concatenating, slicing, and
dicing of netCDF files.

Document conventions {#document-conventions .indent1}
--------------------

Anything underlined is a URL.\
*All filenames look like this -- (typewriter font, green)*.\
*Program names look like this -- (italicized font, green)*.\
*user input looks like this -- (bold, magenta)*.

<div class="unix">

commands to be typed at the command line are contained in an indented
gray box.

</div>

And the contents of a file are enclosed in a box with a border:

<div class="routine">

&hypothetical\_nml\
  obs\_seq\_in\_file\_name = "obs\_seq.in",\
  obs\_seq\_out\_file\_name = "obs\_seq.out",\
  init\_time\_days = 0,\
  init\_time\_seconds = 0,\
  output\_interval = 1 /

</div>

[]{#Installation}

------------------------------------------------------------------------

Installation
============

This document outlines the installation of the DART software and the
system requirements. For convenience, some of the original colloquium
exercises are repeated here, mostly just to check the installation. A
few of the [exercises from the ASP summer 2003
Colloquium](/DART/dart_exercise_doc.pdf) are repeated here, primarily to
serve as the verification of the installation. The entire installation
process is summarized in the following steps:

1.  [Determine which F90 compiler is available](#compilers).
2.  [Determine the location of the *netCDF* library](#netCDFlib).
3.  [Determine the location of the *udunits* library](#udunits).
4.  [Download the DART software bundle and untar it into the expected
    source tree](#download).
5.  [Modify certain DART files to reflect the available F90 compiler and
    location of the appropriate libraries](#customizations).
6.  [Build the executables](#building).

We have tried to make the code as portable as possible, but we do not
have access to all compilers on all platforms, so there are no
guarantees. We are interested in your experience building the system, so
please email me (Tim Hoar) thoar 'at' ucar 'dot' edu (trying to cut down
on the spam).

After the installation, you might want to peruse the following.

-   [Running the Lorenz\_63 Model](#Running).
-   [Using the Matlab diagnostic scripts](#matlab).
-   A short discussion on [bias, filter divergence and covariance
    inflation.](#discussion)
-   And another one on [synthetic observations](#syntheticobservations).

[]{#compilers}

------------------------------------------------------------------------

Requirements: an F90 Compiler
-----------------------------

The DART software has been successfully built on several Linux/x86
platforms with the [Intel Fortran Compiler 7.1 for
Linux](http://www.intel.com/software/products/compilers/flin), which is
free for individual scientific use. It has also been built and
successfully run with the [Portland Group Fortran
Compiler](http://www.pgroup.com) (5.02), and again with the Intel
8.0.034 compiler. Since recompiling the code is a necessity to
experiment with different models, there are no binaries to distribute.

[]{#netCDFlib}

------------------------------------------------------------------------

Requirements: the *netCDF* library
----------------------------------

DART uses the [netCDF](http://www.unidata.ucar.edu/packages/netcdf/)
self-describing data format for the results of assimilation experiments.
These files have the extension *.nc* and can be read by a number of
standard data analysis tools. In particular, DART also makes use of the
F90 interface to the library which are available through the
*netcdf.mod* and *typesizes.mod* modules. *IMPORTANT*: different
compilers create these modules with different "case" filenames, and
sometimes they are not **both** installed into the expected directory.
It is required that both modules be present. The normal place would be
in the `netcdf/include` directory, as opposed to the `netcdf/lib`
directory.

If the netCDF library does not exist on your system, you must build it
(as well as the F90 interface modules). The library and instructions for
building the library or installing from an RPM may be found at the
netCDF home page: <http://www.unidata.ucar.edu/packages/netcdf/> Pay
particular attention to the compiler-specific patches that must be
applied for the Intel Fortran Compiler. (Or the PG compiler, for that
matter.)

The location of the netCDF library, *libnetcdf.a*, and the locations of
both *netcdf.mod* and *typesizes.mod* will be needed by the makefile
template, as described in the [compiling](#compiling) section.

[]{#udunits}

------------------------------------------------------------------------

Requirements: the *udunits* library
-----------------------------------

DART also uses the **very** common
[udunits](http://my.unidata.ucar.edu/content/software/udunits/index.html)
library for manipulating units of physical quantities. If, somehow, it
is not installed on your system, you will need to install it
(instructions are available from [Unidata's
Downloads](http://www.unidata.ucar.edu) page).

The location of the udunits library, *libudunits.a*, will be needed by
the makefile template, as described in the [compiling](#compiling)
section.

[]{#download}

------------------------------------------------------------------------

Unpacking the distribution.
---------------------------

The DART source code is distributed as a compressed tar file.
[DART\_guam.tar.gz](/pub/DART/DART_guam.tar.gz) \[22347692 bytes\]. When
untarred, the source tree will begin with a directory named *DART* and
will be approximately 105 Mb. Compiling the code in this tree (as is
usually the case) will necessitate much more space.

<div class="unix">

gunzip *DART\_guam.tar.gz*\
tar -xvf *DART\_guam.tar*\

</div>

The code tree is very "bushy"; there are many directories of support
routines, etc. but only a few directories involved with the
customization and installation of the DART software. If you can compile
and run ONE of the low-order models, you should be able to compile and
run ANY of the low-order models. For this reason, we can focus on the
Lorenz \`63 model. Subsequently, the only directories with files to be
modified to check the installation are:  *DART/mkmf*,
 *DART/models/lorenz\_63/work*, and  *DART/matlab* (but only for
analysis).

[]{#customizations}

------------------------------------------------------------------------

Customizing the build scripts -- Overview.
------------------------------------------

DART executable programs are constructed using two tools: *make* and
*mkmf*. The *make* utility is a relatively common piece of software that
requires a user-defined input file that records dependencies between
different source files. *make* then performs a hierarchy of actions when
one or more of the source files is modified. The *mkmf* utility is a
custom preprocessor that generates a *make* input file (named
*Makefile*) and an example namelist *input.nml.mkmf* with the default
values. The *Makefile* is designed specifically to work with
object-oriented Fortran90 (and other languages) for systems like DART.

*mkmf* requires two separate input files. The first is a \`template'
file which specifies details of the commands required for a specific
Fortran90 compiler and may also contain pointers to directories
containing pre-compiled utilities required by the DART system. **This
template file will need to be modified to reflect your system**. The
second input file is a \`path\_names' file which includes a complete
list of the locations (either relative or absolute) of all Fortran90
source files that are required to produce a particular DART program.
Each 'path\_names' file must contain a path for exactly one Fortran90
file containing a main program, but may contain any number of additional
paths pointing to files containing Fortran90 modules. An *mkmf* command
is executed which uses the 'path\_names' file and the mkmf template file
to produce a *Makefile* which is subsequently used by the standard
*make* utility.

Shell scripts that execute the mkmf command for all standard DART
executables are provided as part of the standard DART software. For more
information on *mkmf* see [the FMS mkmf
description](http://www.gfdl.gov/fms/pubrel/j/atm_dycores/doc/dycore_public_manual.html#mkmf).\
One of the benefits of using *mkmf* is that it also creates an example
namelist file for each program. The example namelist is called
*input.nml.mkmf*, so as not to clash with any exising *input.nml* that
may exist in that directory.

[]{#template}

### Building and Customizing the 'mkmf.template' file {#building-and-customizing-the-mkmf.template-file .indent1}

A series of templates for different compilers/architectures exists in
the *DART/mkmf/* directory and have names with extensions that identify
either the compiler, the architecture, or both. This is how you inform
the build process of the specifics of your system. Our intent is that
you copy one that is similar to your system into *mkmf.template* and
customize it. For the discussion that follows, knowledge of the contents
of one of these templates (i.e. *mkmf.template.pgi*) is needed: (note
that only the first few lines are shown here)

<div class="routine">

\# Makefile template for PGI f90\
FC = pgf90\
CPPFLAGS =\
FFLAGS = -r8 -Ktrap=fp -pc 64 -I/usr/local/netcdf/include\
LD = pgf90\
LDFLAGS = \$(LIBS)\
LIBS = -L/usr/local/netcdf/lib -lnetcdf -L/usr/local/udunits-1.11.7/lib
-ludunits\
LIST = -Mlist\
\
\# you should never need to change any lines below.\
...\

</div>

Essentially, each of the lines defines some part of the resulting
*Makefile*. Since *make* is particularly good at sorting out
dependencies, the order of these lines really doesn't make any
difference. The *FC = pgf90* line ultimately defines the Fortran90
compiler to use, etc. The lines which are most likely to need
site-specific changes start with *FFLAGS* and *LIBS*, which indicate
where to look for the netCDF F90 modules and the location of the netCDF
and udunits libraries.

[](fflags)

#### FFLAGS {#fflags .indent2}

Each compiler has different compile flags, so there is really no way to
exhaustively cover this other than to say the templates as we supply
them should work -- depending on the location of the netCDF modules
*netcdf.mod* and *typesizes.mod*. Change the */usr/local/netcdf/include*
string to reflect the location of your modules. The low-order models can
be compiled without the *-r8* switch, but the *bgrid\_solo* model
cannot.

[](libs)

#### LIBS {#libs .indent2}

Modifying the *LIBS* value should be relatively straightforward.\
Change the */usr/local/netcdf/lib* string to reflect the location of
your *libnetcdf.a*.\
Change the */usr/local/udunits-1.11.7/lib* string to reflect the
location of your *libudunits.a*.

[]{#path_names}

### Customizing the 'path\_names\_\*' file {#customizing-the-path_names_-file .indent1}

Several *path\_names\_\** files are provided in the *work* directory for
each specific model, in this case: *DART/models/lorenz\_63/work*.

1.  *path\_names\_create\_obs\_sequence*
2.  *path\_names\_create\_fixed\_network\_seq*
3.  *path\_names\_perfect\_model\_obs*
4.  *path\_names\_filter*

Since each model comes with its own set of files, no further
customization is needed.

[]{#building}

------------------------------------------------------------------------

Building the Lorenz\_63 DART project.
-------------------------------------

Currently, DART executables are constructed in a *work* subdirectory
under the directory containing code for the given model. In the
top-level DART directory, change to the L63 work directory and list the
contents:

<div class="unix">

cd DART/models/lorenz\_63/work\
ls -1

</div>

With the result:

    filter_ics 
    mkmf_create_obs_sequence 
    mkmf_create_fixed_network_seq 
    mkmf_filter 
    mkmf_perfect_model_obs 
    path_names_create_obs_sequence 
    path_names_create_fixed_network_seq 
    path_names_filter 
    path_names_perfect_model_obs 
    perfect_ics

There are four *mkmf\_xxxxxx* files for the programs
*create\_obs\_sequence*, *create\_obs\_fixed\_network\_seq*,
*perfect\_model\_obs*, and *filter* along with the corresponding
*path\_names\_xxxxxx* files. You can examine the contents of one of the
*path\_names\_xxxxxx* files, for instance *path\_names\_filter*, to see
a list of the relative paths of all files that contain Fortran90 modules
required for the program *filter* for the L63 model. All of these paths
are relative to your *DART* directory. The first path is the main
program (*filter.f90*) and is followed by all the Fortran90 modules used
by this program.

The *mkmf\_xxxxxx* scripts are cryptic but should not need to be
modified -- as long as you do not restructure the code tree (by moving
directories, for example).\
\
The only function of the *mkmf\_xxxxxx* script is to generate a
*Makefile* and an *input.nml.mkmf* file. It is not supposed to compile
anything:

<div class="unix">

csh mkmf\_create\_obs\_sequence\
make

</div>

The first command generates an appropriate *Makefile* and the
*input.nml.create\_obs\_sequence\_default* file. The second command
results in the compilation of a series of Fortran90 modules which
ultimately produces an executable file: *create\_obs\_sequence*. Should
you need to make any changes to the *DART/mkmf/mkmf.template*, you will
need to regenerate the *Makefile*. A series of object files for each
module compiled will also be left in the work directory, as some of
these are undoubtedly needed by the build of the other DART components.
You can proceed to create the other three programs needed to work with
L63 in DART as follows:

<div class="unix">

csh mkmf\_create\_fixed\_network\_seq\
make\
csh mkmf\_perfect\_model\_obs\
make\
csh mkmf\_filter\
make

</div>

The result (hopefully) is that four executables now reside in your work
directory. The most common problem is that the netCDF libraries and
include files (particularly *typesizes.mod*) are not found. Edit the
*DART/mkmf/mkmf.template*, recreate the *Makefile*, and try again.

  program                         purpose
  ------------------------------- -------------------------------------------------------------------------------------------
  *create\_obs\_sequence*         specify a (set) of observation characteristics taken by a particular (set of) instruments
  *create\_fixed\_network\_seq*   specify the temporal attributes of the observation sets
  *perfect\_model\_obs*           spinup, generate "true state" for synthetic observation experiments, ...
  *filter*                        perform experiments

[]{#Running}

------------------------------------------------------------------------

Running Lorenz\_63.
-------------------

This initial sequence of exercises includes detailed instructions on how
to work with the DART code and allows investigation of the basic
features of one of the most famous dynamical systems, the 3-variable
Lorenz-63 model. The remarkable complexity of this simple model will
also be used as a case study to introduce a number of features of a
simple ensemble filter data assimilation system. To perform a synthetic
observation assimilation experiment for the L63 model, the following
steps must be performed (an overview of the process is given first,
followed by detailed procedures for each step):

Experiment Overview {#experiment-overview .indent1}
-------------------

1.  [Integrate the L63 model for a long time](#integrate)\
    starting from arbitrary initial conditions to generate a model state
    that lies on the attractor. The ergodic nature of the L63 system
    means a 'lengthy' integration always converges to some point on the
    computer's finite precision representation of the model's
    attractor.\
    \
2.  [Generate a set of ensemble initial conditions](#ensemblate)\
    from which to start an assimilation. Since L63 is ergodic, the
    ensemble members can be designed to look like random samples from
    the model's 'climatological distribution'. To generate an ensemble
    member, very small perturbations can be introduced to the state on
    the attractor generated by step 1. This perturbed state can then be
    integrated for a very long time until all memory of its initial
    condition can be viewed as forgotten. Any number of ensemble initial
    conditions can be generated by repeating this procedure.\
    \
3.  [Simulate a particular observing system](#simulate)\
    by first creating an 'observation set definition' and then creating
    an 'observation sequence'. The 'observation set definition'
    describes the instrumental characteristics of the observations and
    the 'observation sequence' defines the temporal sequence of the
    observations.\
    \
4.  [Populate the 'observation sequence' with 'perfect'
    observations](#generate)\
    by integrating the model and using the information in the
    'observation sequence' file to create simulated observations. This
    entails operating on the model state at the time of the observation
    with an appropriate forward operator (a function that operates on
    the model state vector to produce the expected value of the
    particular observation) and then adding a random sample from the
    observation error distribution specified in the observation set
    definition. At the same time, diagnostic output about the 'true'
    state trajectory can be created.\
    \
5.  [Assimilate the synthetic observations](#assimilate)\
    by running the filter; diagnostic output is generated.\
    \

[]{#integrate}

### 1. Integrate the L63 model for a 'long' time. {#integrate-the-l63-model-for-a-long-time. .indent1}

*perfect\_model\_obs* integrates the model for all the times specified
in the 'observation sequence definition' file. To this end, begin by
creating an 'observation sequence definition' file that spans a long
time. Creating an 'observation sequence definition' file is a two-step
procedure involving *create\_obs\_sequence* followed by
*create\_fixed\_network\_seq*. After they are both run, it is necessary
to integrate the model with *perfect\_model\_obs*.

#### 1.1 Create an observation set definition. {#create-an-observation-set-definition. .indent1}

*create\_obs\_sequence* creates an observation set definition, the
time-independent part of an observation sequence. An observation set
definition file only contains the *location, type,* and *observational
error characteristics* (normally just the diagonal observational error
variance) for a related set of observations. There are no actual
observations, nor are there any times associated with the definition.
For spin-up, we are only interested in integrating the L63 model, not in
generating any particular synthetic observations. Begin by creating a
minimal observation set definition.\
\
In general, for the low-order models, only a single observation set need
be defined. Next, the number of individual scalar observations (like a
single surface pressure observation) in the set is needed. To spin-up an
initial condition for the L63 model, only a single observation is
needed. Next, the error variance for this observation must be entered.
Since we do not need (nor want) this observation to have any impact on
an assimilation (it will only be used for spinning up the model and the
ensemble), enter a very large value for the error variance. An
observation with a very large error variance has essentially no impact
on deterministic filter assimilations like the default variety
implemented in DART. Finally, the location and type of the observation
need to be defined. For all types of models, the most elementary form of
synthetic observations are called 'identity' observations. These
observations are generated simply by adding a random sample from a
specified observational error distribution directly to the value of one
of the state variables. This defines the observation as being an
identity observation of the first state variable in the L63 model. The
program will respond by terminating after generating a file (generally
named *set\_def.out*) that defines the single identity observation of
the first state variable of the L63 model. The following is a screenshot
(much of the verbose logging has been left off for clarity), the user
input looks *like this*.

<div class="unix">

    [unixprompt]$ ./create_obs_sequence
     Initializing the utilities module.
     Trying to read from unit           10
     Trying to open file dart_log.out
     
     Registering module :
     $source: /home/dart/CVS.REPOS/DART/utilities/utilities_mod.f90,v $
     $revision: 1.18 $
     $date: 2004/06/29 15:16:40 $
     Registration complete.
     
     &UTILITIES_NML
     TERMLEVEL= 2,LOGFILENAME=dart_log.out                                          
                                                                                
     /
     
     Registering module :
     $source: /home/dart/CVS.REPOS/DART/obs_sequence/create_obs_sequence.f90,v $
     $revision: 1.18 $
     $date: 2004/05/24 15:41:46 $
     Registration complete.

     { ... }

     Input upper bound on number of observations in sequence
    10
     
     Input number of copies of data (0 for just a definition)
    0

     Input number of quality control values per field (0 or greater)
    0

     input a -1 if there are no more obs 
    0

     Registering module :
     $source: /home/dart/CVS.REPOS/DART/obs_def/obs_def_mod.f90,v $
     $revision: 1.21 $
     $date: 2004/06/25 16:17:43 $
     Registration complete.
     
     Registering module :
     $source: /home/dart/CVS.REPOS/DART/obs_kind/obs_kind_mod.f90,v $
     $revision: 1.15 $
     $date: 2004/06/24 21:49:47 $
     Registration complete.
     
     input obs kind: u =            1  v =            2  ps =            3  t = 
               4  qv =            5  p =            6  Td =           10  Vr = 
             100  Ref =          101
     input -1 times the state variable index for an identity observation
    -1

     input time in days and seconds
    1 0

     input time in days and seconds
    1 0

     Input error variance for this observation definition
    1000000

     input a -1 if there are no more obs 
    -1

     Input filename for sequence (  set_def.out   usually works well)
     set_def.out 
     write_obs_seq  opening formatted file set_def.out
     write_obs_seq  closed file set_def.out

</div>

#### 1.2 Create an observation sequence definition. {#create-an-observation-sequence-definition. .indent1}

*create\_fixed\_network\_seq* creates an 'observation sequence
definition' by extending the 'observation set definition' with the
temporal attributes of the observations.\
\
The first input is the name of the file created in the previous step,
i.e. the name of the observation set definition that you've just
created. It is possible to create sequences in which the observation
sets are observed at regular intervals or irregularly in time. Here, all
we need is a sequence that takes observations over a long period of time
- indicated by entering a 1. Although the L63 system normally is defined
as having a non-dimensional time step, the DART system arbitrarily
defines the model timestep as being 3600 seconds. By declaring we have
1000 observations taken once per day, we create an observation sequence
definition spanning 24000 'model' timesteps; sufficient to spin-up the
model onto the attractor. Finally, enter a name for the 'observation
sequence definition' file. Note again: there are no observation values
present in this file. Just an observation type, location, time and the
error characteristics. We are going to populate the observation sequence
with the *perfect\_model\_obs* program.

<div class="unix">

    [unixprompt]$ ./create_fixed_network_seq

     ...

     Registering module :
     $source: /home/dart/CVS.REPOS/DART/obs_sequence/obs_sequence_mod.f90,v $
     $revision: 1.31 $
     $date: 2004/06/29 15:04:37 $
     Registration complete.
     
     Input filename for network definition sequence (usually  set_def.out  )
    set_def.out

     ...

     To input a regularly repeating time sequence enter 1
     To enter an irregular list of times enter 2
    1
     Input number of observations in sequence
    1000
     Input time of initial ob in sequence in days and seconds
    1, 0
     Input period of obs in days and seconds
    1, 0
     time             1  is             0            1
     time             2  is             0            2
     time             3  is             0            3
    ...
     time           998  is             0          998
     time           999  is             0          999
     time          1000  is             0         1000
    What is output file name for sequence (  obs_seq.in   is recommended )
    obs_seq.in

</div>

#### 1.3 Initialize the model onto the attractor.

*perfect\_model\_obs* can now advance the arbitrary initial state for
24,000 timesteps to move it onto the attractor. *perfect\_model\_obs*
uses the Fortran90 namelist input mechanism instead of (admittedly gory,
but temporary) interactive input. All of the DART software expects the
namelists to found in a file called *input.nml*. When you built the
executable, an example namelist was created *input.nml.mkmf* that
contains all of the namelist input for the executable. If you followed
the example, each namelist was saved to a unique name. We must now
rename and edit the namelist file for *perfect\_model\_obs*. Copy
*input.nml.perfect\_model\_obs* to *input.nml* and edit it to look like
the following:

<div class="routineIndent1">

&perfect\_model\_obs\_nml\
   async = 0,\
   obs\_seq\_in\_file\_name = "obs\_seq.in",\
   obs\_seq\_out\_file\_name = "obs\_seq.out",\
   start\_from\_restart = .false.,\
   output\_restart = *.true.*,\
   restart\_in\_file\_name = "perfect\_ics",\
   restart\_out\_file\_name = "perfect\_restart",\
   init\_time\_days = 0,\
   init\_time\_seconds = 0,\
   output\_interval = 1 /\
&assim\_tools\_nml    prior\_spread\_correction = .false.,
   filter\_kind = 1,    slope\_threshold = 1.0 / &cov\_cutoff\_nml
   select\_localization = 1 / &assim\_model\_nml
   binary\_restart\_files = .true. / &model\_nml    sigma = 10.0,    r =
28.0,    b = 2.6666666666667,    deltat = 0.01 / &utilities\_nml
   TERMLEVEL = 1    logfilename = 'dart\_log.out' /

</div>

For the moment, only two namelists warrant explanation. Each namelists
is covered in detail in the html files accompanying the source code for
the module.

### perfect\_model\_obs\_nml {#perfect_model_obs_nml .indent1}

  namelist variable             description
  ----------------------------- -------------------------------------------------------------------------------------------------------------------------------------------------------
  *async*                       For the lorenz\_63, simply ignore this. Leave it set to '0'
  *obs\_seq\_in\_file\_name*    specifies the file name that results from running *create\_fixed\_network\_seq*, i.e. the 'observation sequence definition' file.
  *obs\_seq\_out\_file\_name*   specifies the output file name containing the 'observation sequence', finally populated with (perfect?) 'observations'.
  *start\_from\_restart*        When set to 'false', *perfect\_model\_obs* generates an arbitrary initial condition (which cannot be guaranteed to be on the L63 attractor).
  *output\_restart*             When set to 'true', *perfect\_model\_obs* will record the model state at the end of this integration in the file named by *restart\_out\_file\_name*.
  *restart\_in\_file\_name*     is ignored when 'start\_from\_restart' is 'false'.
  *restart\_out\_file\_name*    if *output\_restart* is 'true', this specifies the name of the file containing the model state at the end of the integration.
  *init\_time\_xxxx*            the start time of the integration.
  *output\_interval*            interval at which to save the model state.

### utilities\_nml {#utilities_nml .indent1}

  namelist variable   description
  ------------------- --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *TERMLEVEL*         When set to '1' the programs terminate when a 'warning' is generated. When set to '2' the programs terminate only with 'fatal' errors.
  *logfilename*       Run-time diagnostics are saved to this file. This namelist is used by all programs, so the file is opened in APPEND mode. Subsequent executions cause this file to grow.

Executing *perfect\_model\_obs* will integrate the model 24,000 steps
and output the resulting state in the file *perfect\_restart*.
Interested parties can check the spinup in the *True\_State.nc* file.

<div class="unix">

perfect\_model\_obs

</div>

[]{#ensemblate}

### 2. Generate a set of ensemble initial conditions. {#generate-a-set-of-ensemble-initial-conditions. .indent1}

The set of initial conditions for a 'perfect model' experiment is
created by taking the spun-up state of the model (available in
*perfect\_restart*), running *perfect\_model\_obs* to generate the 'true
state' of the experiment and a corresponding set of observations, and
then feeding the same initial spun-up state and resulting observations
into *filter*.\
\
Generating ensemble initial conditions is achieved by changing a
perfect\_model\_obs namelist parameter, copying *perfect\_restart* to
*perfect\_ics*, and rerunning *perfect\_model\_obs*. This execution of
*perfect\_model\_obs* will advance the model state from the end of the
first 24,000 steps to the end of an additional 24,000 steps and place
the final state in *perfect\_restart*. The rest of the namelists in
*input.nml* should remain unchanged.

<div class="routineIndent1">

&perfect\_model\_obs\_nml\
   async = 0,\
   obs\_seq\_in\_file\_name = "obs\_seq.in",\
   obs\_seq\_out\_file\_name = "obs\_seq.out",\
   start\_from\_restart = *.true.*,\
   output\_restart = .true.,\
   restart\_in\_file\_name = "perfect\_ics",\
   restart\_out\_file\_name = "perfect\_restart",\
   init\_time\_days = 0,\
   init\_time\_seconds = 0,\
   output\_interval = 1 /\

</div>

\

<div class="unix">

cp perfect\_restart perfect\_ics\
perfect\_model\_obs

</div>

A *True\_State.nc* file is also created. It contains the 'true' state of
the integration.

#### Generating the ensemble {#generating-the-ensemble .indent1}

is done with the program *filter*, which also uses the Fortran90
namelist mechanism for input. It is now necessary to copy the
*input.nml.filter* namelist to *input.nml* or you may simply insert the
*filter\_nml* namelist into the existing *input.nml*. Having the
*perfect\_model\_obs* namelist in the input.nml does not hurt anything.
In fact, I generally create a single *input.nml* that has all the
namelist blocks in it.

<div class="routineIndent1">

&perfect\_model\_obs\_nml\
   async = 0,\
   obs\_seq\_in\_file\_name = "obs\_seq.in",\
   obs\_seq\_out\_file\_name = "obs\_seq.out",\
   start\_from\_restart = .true.,\
   output\_restart = .true.,\
   restart\_in\_file\_name = "perfect\_ics",\
   restart\_out\_file\_name = "perfect\_restart",\
   init\_time\_days = 0,\
   init\_time\_seconds = 0,\
   output\_interval = 1 /\
&assim\_tools\_nml\
   prior\_spread\_correction = .false.,\
   filter\_kind = 1,\
   slope\_threshold = 1.0 /\
&cov\_cutoff\_nml\
   select\_localization = 1 /\
&assim\_model\_nml\
   binary\_restart\_files = .true. /\
&model\_nml\
   sigma = 10.0,\
   r = 28.0,\
   b = 2.6666666666667\
   deltat = 0.01 /\
&utilities\_nml\
   TERMLEVEL = 1\
   logfilename = 'dart\_log.out' /\
&reg\_factor\_nml\
   select\_regression = 1,\
   input\_reg\_file = "time\_mean\_reg" /\
&filter\_nml\
   async = 0,\
   ens\_size = 20,\
   cutoff = 0.20,\
   cov\_inflate = 1.00,\
   start\_from\_restart = .false.,\
   output\_restart = *.true.*,\
   obs\_sequence\_file\_name = "obs\_seq.out",\
   restart\_in\_file\_name = "perfect\_ics",\
   restart\_out\_file\_name = "filter\_restart",\
   init\_time\_days = 0,\
   init\_time\_seconds = 0,\
   output\_state\_ens\_mean = .true.,\
   output\_state\_ens\_spread = .true.,\
   num\_output\_ens\_members = *20*,\
   output\_interval = 1,\
   num\_groups = 1,\
   confidence\_slope = 0.0,\
   output\_obs\_diagnostics = .false.,\
   get\_mean\_reg = .false.,\
   get\_median\_reg = .false. /\

</div>

Only the non-obvious(?) entries for *filter\_nml* will be discussed.

  namelist variable              description
  ------------------------------ ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *ens\_size*                    Number of ensemble members. 20 is sufficient for most of the L63 exercises.
  *cutoff*                       to limit the impact of an observation, set to 0.0 (i.e. spin-up)
  *cov\_inflate*                 A value of 1.0 results in no inflation.(spin-up)
  *start\_from\_restart*         when '.false.', *filter* will generate its own set of initial conditions. It is important to note that the filter still makes use of *perfect\_ics* by randomly perturbing these state variables.
  *num\_output\_ens\_members*    may be a value from 0 to *ens\_size*
  *output\_state\_ens\_mean*     when '.true.' the mean of all ensemble members is output.
  *output\_state\_ens\_spread*   when '.true.' the spread of all ensemble members is output.
  *output\_interval*             Jeff - units for interval?

The filter is told to generate its own ensemble initial conditions since
*start\_from\_restart* is '.false.'. However, it is important to note
that the filter still makes use of *perfect\_ics* which is set to be the
*restart\_in\_file\_name*. This is the model state generated from the
first 24,000 step model integration by *perfect\_model\_obs*. *Filter*
generates its ensemble initial conditions by randomly perturbing the
state variables of this state.

The arguments *output\_state\_ens\_mean* and
*output\_state\_ens\_spread* are '.true.' so that these quantities are
output at every time for which there are observations (once a day here)
and *num\_output\_ens\_members* means that the same diagnostic files,
*Posterior\_Diag.nc* and *Prior\_Diag.nc* also contain values for all 20
ensemble members once a day. Once the namelist is set, execute *filter*
to integrate the ensemble forward for 24,000 steps with the final
ensemble state written to the *filter\_restart*. Copy the
*perfect\_model\_obs* restart file *perfect\_restart* (the \`true
state') to *perfect\_ics*, and the *filter* restart file
*filter\_restart* to *filter\_ics* so that future assimilation
experiments can be initialized from these spun-up states.

<div class="unix">

filter\
cp perfect\_restart perfect\_ics\
cp filter\_restart filter\_ics

</div>

The spin-up of the ensemble can be viewed by examining the output in the
netCDF files *True\_State.nc* generated by *perfect\_model\_obs* and
*Posterior\_Diag.nc* and *Prior\_Diag.nc* generated by *filter*. To do
this, see the detailed discussion of matlab diagnostics in Appendix I.
[]{#simulate}

### 3. Simulate a particular observing system. {#simulate-a-particular-observing-system. .indent1}

*Please note this input sequence was wrong prior to 14 Sep 2004, sorry
-- TJH*

Begin by using *create\_obs\_sequence* to generate an observation set in
which each of the 3 state variables of L63 is observed with an
observational error variance of 1.0 for each observation. To do this,
use the following input sequence (the text including and after \# is a
comment and does not need to be entered):

*4*

\# upper bound on num of observations in sequence

*0*

\# number of copies of data (0 for just a definition)

*0*

\# number of quality control values per field (0 or greater)

*0*

\# -1 to exit/end observation definitions

*-1*

\# observe state variable 1

*0   0*

\# time -- days, seconds

*1.0*

\# observational variance

*0*

\# -1 to exit/end observation definitions

*-2*

\# observe state variable 2

*0   0*

\# time -- days, seconds

*1.0*

\# observational variance

*0*

\# -1 to exit/end observation definitions

*-3*

\# observe state variable 3

*0   0*

\# time -- days, seconds

*1.0*

\# observational variance

*-1*

\# -1 to exit/end observation definitions

*set\_def.out*

\# Output file name

Now, generate an observation sequence definition by running
*create\_fixed\_network\_seq* with the following input sequence:

  ---------------- ------------------------------------------------------------------
  *set\_def.out*   \# Input observation set definition file
  *1*              \# Regular spaced observation interval in time
  *1000*           \# 1000 observation times
  *0, 43200*       \# First observation after 12 hours (0 days, 3600 \* 12 seconds)
  *0, 43200*       \# Observations every 12 hours
  *obs\_seq.in*    \# Output file for observation sequence definition
  ---------------- ------------------------------------------------------------------

[]{#generate}

### 4. Generate a particular observing system and true state. {#generate-a-particular-observing-system-and-true-state. .indent1}

An observation sequence file is now generated by running
*perfect\_model\_obs* with the namelist values (unchanged from step 2):

<div class="routineIndent1">

&perfect\_model\_obs\_nml\
   async = 0,\
   obs\_seq\_in\_file\_name = "obs\_seq.in",\
   obs\_seq\_out\_file\_name = "obs\_seq.out",\
   start\_from\_restart = .true.,\
   output\_restart = .true.,\
   restart\_in\_file\_name = "perfect\_ics",\
   restart\_out\_file\_name = "perfect\_restart",\
   init\_time\_days = 0,\
   init\_time\_seconds = 0,\
   output\_interval = 1 /

</div>

This integrates the model starting from the state in *perfect\_ics* for
1000 12-hour intervals outputting synthetic observations of the three
state variables every 12 hours and producing a netCDF diagnostic file,
*True\_State.nc*.

[]{#assimilate}

### 5. Filtering. {#filtering. .indent1}

Finally, *filter* can be run with its namelist set to:

<div class="routineIndent1">

&filter\_nml\
   async = 0,\
   ens\_size = 20,\
   cutoff = *22222222.0*,\
   cov\_inflate = 1.00,\
   start\_from\_restart = *.true.*,\
   output\_restart = .true.,\
   obs\_sequence\_file\_name = "obs\_seq.out",\
   restart\_in\_file\_name = "*filter\_ics*",\
   restart\_out\_file\_name = "filter\_restart",\
   init\_time\_days = 0,\
   init\_time\_seconds = 0,\
   output\_state\_ens\_mean = .true.,\
   output\_state\_ens\_spread = .true.,\
   num\_output\_ens\_members = 20,\
   output\_interval = 1,\
   num\_groups = 1,\
   confidence\_slope = 0.0,\
   output\_obs\_diagnostics = .false.,\
   get\_mean\_reg = .false.,\
   get\_median\_reg = .false. /

</div>

The large value for the cutoff allows each observation to impact all
other state variables (see Appendix V for localization). *filter*
produces two output diagnostic files, *Prior\_Diag.nc* which contains
values of the ensemble members, ensemble mean and ensemble spread for
12- hour lead forecasts before assimilation is applied and
*Posterior\_Diag.nc* which contains similar data for after the
assimilation is applied (sometimes referred to as analysis values).

Now try applying all of the matlab diagnostic functions described in
[the Matlab Diagnostics section](#matlab). []{#matlab}

------------------------------------------------------------------------

Matlab Diagnostics
------------------

The output files are netCDF files, and may be examined with many
different software packages. We happen to use Matlab, and provide our
diagnostic scripts in the hopes that they are useful.

The Matlab diagnostic scripts and underlying functions reside in the
*DART/matlab* directory. They are reliant on the public-domain [netcdf
toolbox](http://woodshole.er.usgs.gov/staffpages/cdenham/public_html/MexCDF/nc4ml5.html)
from
*http://woodshole.er.usgs.gov/staffpages/cdenham/public\_html/MexCDF/nc4ml5.html*
as well as the public-domain [CSIRO matlab/netCDF
interface](http://www.marine.csiro.au/sw/matlab-netcdf.html) from
*http://www.marine.csiro.au/sw/matlab-netcdf.html*. If you do not have
them installed on your system and want to use Matlab to peruse netCDF,
you must follow their installation instructions.

Once you can access the *getnc* function from within Matlab, you can use
our diagnostic scripts. It is necessary to prepend the location of the
DART/matlab scripts to the matlabpath. Keep in mind the location of the
netcdf operators on your system WILL be different from ours ... and
that's OK.

<div class="unix">

    0[269]0 ghotiol:/<5>models/lorenz_63/work]$ matlab -nojvm

                                                 < M A T L A B >
                                     Copyright 1984-2002 The MathWorks, Inc.
                                         Version 6.5.0.180913a Release 13
                                                   Jun 18 2002

      Using Toolbox Path Cache.  Type "help toolbox_path_cache" for more info.
     
      To get started, type one of these: helpwin, helpdesk, or demo.
      For product information, visit www.mathworks.com.

    >> which getnc
    /contrib/matlab/matlab_netcdf_5_0/getnc.m
    >>ls *.nc

    ans =

    Posterior_Diag.nc  Prior_Diag.nc  True_State.nc


    >>path('../../../matlab',path)
    >>which plot_ens_err_spread
    ../../../matlab/plot_ens_err_spread.m
    >>help plot_ens_err_spread

      DART : Plots summary plots of the ensemble error and ensemble spread.
                             Interactively queries for the needed information.
                             Since different models potentially need different 
                             pieces of information ... the model types are 
                             determined and additional user input may be queried.
     
      Ultimately, plot_ens_err_spread will be replaced by a GUI.
      All the heavy lifting is done by PlotEnsErrSpread.
     
      Example 1 (for low-order models)
     
      truth_file = 'True_State.nc';
      diagn_file = 'Prior_Diag.nc';
      plot_ens_err_spread

    >>plot_ens_err_spread

</div>

And the matlab graphics window will display the spread of the ensemble
error for each state variable. The scripts are designed to do the
"obvious" thing for the low-order models and will prompt for additional
information if needed. The philosophy of these is that anything that
starts with a lower-case *plot\_*some\_specific\_task** is intended to
be user-callable and should handle any of the models. All the other
routines in *DART/matlab* are called BY the high-level routines.

  Matlab script                     description
  --------------------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *plot\_bins*                      plots ensemble rank histograms
  *plot\_correl*                    Plots space-time series of correlation between a given variable at a given time and other variables at all times in a n ensemble time sequence.
  *plot\_ens\_err\_spread*          Plots summary plots of the ensemble error and ensemble spread. Interactively queries for the needed information. Since different models potentially need different pieces of information ... the model types are determined and additional user input may be queried.
  *plot\_ens\_mean\_time\_series*   Queries for the state variables to plot.
  *plot\_ens\_time\_series*         Queries for the state variables to plot.
  *plot\_phase\_space*              Plots a 3D trajectory of (3 state variables of) a single ensemble member. Additional trajectories may be superimposed.
  *plot\_total\_err*                Summary plots of global error and spread.
  *plot\_var\_var\_correl*          Plots time series of correlation between a given variable at a given time and another variable at all times in an ensemble time sequence.

[]{#discussion}

------------------------------------------------------------------------

Bias, filter divergence and covariance inflation (with the L63 model)
---------------------------------------------------------------------

One of the common problems with ensemble filters is filter divergence,
which can also be an issue with a variety of other flavors of filters
including the classical Kalman filter. In filter divergence, the prior
estimate of the model state becomes too confident, either by chance or
because of errors in the forecast model, the observational error
characteristics, or approximations in the filter itself. If the filter
is inappropriately confident that its prior estimate is correct, it will
then tend to give less weight to observations then they should be given.
The result can be enhanced overconfidence in the model's state estimate.
In severe cases, this can spiral out of control and the ensemble can
wander entirely away from the truth, confident that it is correct in its
estimate. In less severe cases, the ensemble estimates may not diverge
entirely from the truth but may still be too confident in their
estimate. The result is that the truth ends up being farther away from
the filter estimates than the spread of the filter ensemble would
estimate. This type of behavior is commonly detected using rank
histograms (also known as Talagrand diagrams). You can see the rank
histograms for the L63 initial assimilation by using the matlab script
*plot\_bins*.\
\
A simple, but surprisingly effective way of dealing with filter
divergence is known as covariance inflation. In this method, the prior
ensemble estimate of the state is expanded around its mean by a constant
factor, effectively increasing the prior estimate of uncertainty while
leaving the prior mean estimate unchanged. The program *filter* has a
namelist parameter that controls the application of covariance
inflation, *cov\_inflate*. Up to this point, *cov\_inflate* has been set
to 1.0 indicating that the prior ensemble is left unchanged. Increasing
*cov\_inflate* to values greater than 1.0 inflates the ensemble before
assimilating observations at each time they are available. Values
smaller than 1.0 contract (reduce the spread) of prior ensembles before
assimilating.

You can do this by modifying the value of *cov\_inflate* in the
namelist, (try 1.05 and 1.10 and other values at your discretion) and
run the filter as above. In each case, use the diagnostic matlab tools
to examine the resulting changes to the error, the ensemble spread (via
rank histogram bins, too), etc. What kind of relation between spread and
error is seen in this model?

[]{#syntheticobservations}

------------------------------------------------------------------------

Synthetic Observations
----------------------

Synthetic observations are generated from a \`perfect' model
integration, which is often referred to as the \`truth' or a \`nature
run'. A model is integrated forward from some set of initial conditions
and observations are generated as *y = H(x) + e* where *H* is an
operator on the model state vector, *x*, that gives the expected value
of a set of observations, *y*, and *e* is a random variable with a
distribution describing the error characteristics of the observing
instrument(s) being simulated. Using synthetic observations in this way
allows students to learn about assimilation algorithms while being
isolated from the additional (extreme) complexity associated with model
error and unknown observational error characteristics. In other words,
for the real-world assimilation problem, the model has (often
substantial) differences from what happens in the real system and the
observational error distribution may be very complicated and is
certainly not well known. Be careful to keep these issues in mind while
exploring the capabilities of the ensemble filters with synthetic
observations.

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

