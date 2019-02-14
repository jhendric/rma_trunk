[]{#TOP}

DART Lanai Release Notes
========================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to DART Documentation Main   |
| logo](../images/Dartboard7.png){h | Index                             |
| eight="70"}                       | [Website](https://svn-dares-dart. |
|                                   | cgd.ucar.edu/DART/releases/Lanai/ |
|                                   | index.html)                       |
|                                   | or [local                         |
|                                   | file](../../../documentation/inde |
|                                   | x.html)\                          |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[Dart Overview](#Overview) / [Getting Started](#GettingStarted) /
[Installation](#Installation) / [Notes for Current Users](#CurrentUsers)
/ [Non-backwards Compatible Changes](#Nonbackward) / [New
Features](#NewFeatures) / [New Models](#NewModels) / [Changed
Models](#ChangedModels) / [New Forward Operators](#NewFOs) / [New
Observations](#NewObs) / [New Diagnostics and
Documentation](#NewDiagnostics) / [New Utilities](#NewUtilities) /
[Known Problems](#KnownProblems) / [Terms of Use](#Legalese)
[]{#Overview}

Dart Overview
-------------

The Data Assimilation Research Testbed (DART) is designed to facilitate
the combination of assimilation algorithms, models, and real (or
synthetic) observations to allow increased understanding of all three.
The DART programs are highly portable, having been compiled with many
Fortran 90 compilers and run on linux compute-servers, linux clusters,
OSX laptops/desktops, SGI Altix clusters, supercomputers running AIX,
and more. Read the [Customizations](#customizations) section for help in
building on new platforms.

DART employs a modular programming approach to apply an Ensemble Kalman
Filter which adjusts model values toward a state that is more consistent
with information from a set of observations. Models may be swapped in
and out, as can different algorithms in the Ensemble Kalman Filter. The
method requires running multiple instances of a model to generate an
ensemble of states. A forward operator appropriate for the type of
observation being assimilated is applied to each of the states to
generate the model's estimate of the observation. Comparing these
estimates and their uncertainty to the observation and its uncertainty
ultimately results in the adjustments to the model states. See the
DARTLAB demos or read more in the tutorials included with the DART
distribution. They are described below.

DART diagnostic output includes two netCDF files containing the model
states just before the adjustment (*Prior\_Diag.nc*) and just after the
adjustment (*Posterior\_Diag.nc*) as well as a file *obs\_seq.final*
with the model estimates of the observations. There is a suite of
Matlab® functions that facilitate exploration of the results, but the
netCDF files are inherently portable and contain all the necessary
metadata to interpret the contents with other analysis programs such as
NCL, R, etc.

In this document links are available which point to Web-based
documentation files and also to the same information in html files
distributed with DART. If you have used subversion to check out a local
copy of the DART files you can open this file in a browser by loading
*DART/doc/html/documentation/documentation/html/Lanai\_release.html* and
then use the *local file* links to see other documentation pages without
requiring a connection to the internet. If you are looking at this
documentation from the *www.image.ucar.edu* web server or you are
connected to the internet you can use the *Website* links to view other
documentation pages.

[]{#GettingStarted}

Getting Started
---------------

### What's Required

1.  a Fortran 90 compiler
2.  a netCDF library including the F90 interfaces
3.  the C shell
4.  (optional, to run in parallel) an MPI library

DART has been tested on many Fortran compilers and platforms. We don't
have any platform-dependent code sections and we use only the parts of
the language that are portable across all the compilers we have access
to. We explicitly set the Fortran 'kind' for all real values and do not
rely on autopromotion or other compile-time flags to set the default
byte size for numbers. It is possible that some model-specific interface
code from outside sources may have specific compiler flag requirements;
see the documentation for each model. The low-order models and all
common portions of the DART code compile cleanly.\
\
DART uses the [netCDF](http://www.unidata.ucar.edu/packages/netcdf/)
self-describing data format with a particular metadata convention to
describe output that is used to analyze the results of assimilation
experiments. These files have the extension *.nc* and can be read by a
number of standard data analysis tools.\
\
Since most of the models being used with DART are written in Fortran and
run on various UNIX or \*nix platforms, the development environment for
DART is highly skewed to these machines. We do most of our development
on a small linux workstation and a mac laptop running OSX 10.x, and we
have an extensive test network. (I've never built nor run DART on a
Windows machine - so I don't even know if it's possible. If you have run
it (under Cygwin?) please let me know how it went -- I'm curious. Tim -
thoar 'at' ucar 'dot ' edu)

### What's nice to have

**ncview**: DART users have used
[ncview](http://meteora.ucsd.edu/~pierce/ncview_home_page.html) to
create graphical displays of output data fields. The 2D rendering is
good for 'quick-look' type uses, but I wouldn't want to publish with
it.\
\
**NCO**: The [NCO](http://nco.sourceforge.net) tools are able to perform
operations on netCDF files like concatenating, slicing, and dicing.\
\
**Matlab**®: A set of [Matlab®](http://www.mathworks.com/) scripts
designed to produce graphical diagnostics from DART. netCDF output files
are also part of the DART project.\
\
**MPI**: The DART system includes an MPI option. MPI stands for 'Message
Passing Interface', and is both a library and run-time system that
enables multiple copies of a single program to run in parallel, exchange
data, and combine to solve a problem more quickly. DART does **NOT**
require MPI to run; the default build scripts do not need nor use MPI in
any way. However, for larger models with large state vectors and large
numbers of observations, the data assimilation step will run much faster
in parallel, which requires MPI to be installed and used. However, if
multiple ensembles of your model fit comfortably (in time and memory
space) on a single processor, you need read no further about MPI.\
\

### Types of input

DART programs can require three different types of input. First, some of
the DART programs, like those for creating synthetic observational
datasets, require interactive input from the keyboard. For simple cases
this interactive input can be made directly from the keyboard. In more
complicated cases a file containing the appropriate keyboard input can
be created and this file can be directed to the standard input of the
DART program. Second, many DART programs expect one or more input files
in DART specific formats to be available. For instance,
*perfect\_model\_obs*, which creates a synthetic observation set given a
particular model and a description of a sequence of observations,
requires an input file that describes this observation sequence. At
present, the observation files for DART are in a custom format in either
human-readable ascii or more compact machine-specific binary. Third,
many DART modules (including main programs) make use of the Fortran90
namelist facility to obtain values of certain parameters at run-time.
All programs look for a namelist input file called *input.nml* in the
directory in which the program is executed. The *input.nml* file can
contain a sequence of individual Fortran90 namelists which specify
values of particular parameters for modules that compose the executable
program.

Document conventions
--------------------

Anything underlined is a URL.\
\
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
  output\_interval = 1\
&end

</div>

[]{#Installation}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Installation
------------

This document outlines the installation of the DART software and the
system requirements. The entire installation process is summarized in
the following steps:

1.  [Determine which F90 compiler is available](#compilers).
2.  [Determine the location of the *netCDF* library](#netCDFlib).
3.  [Download the DART software into the expected source
    tree](#download).
4.  [Modify certain DART files to reflect the available F90 compiler and
    location of the appropriate libraries](#customizations).
5.  [Build the executables](#building).

We have tried to make the code as portable as possible, but we do not
have access to all compilers on all platforms, so there are no
guarantees. We are interested in your experience building the system, so
please email me (Tim Hoar) thoar 'at' ucar 'dot' edu (trying to cut down
on the spam).

After the installation, you might want to peruse the following.

-   [Running the Lorenz\_63 Model](#Running).
-   [Using the Matlab® diagnostic scripts](#matlab).
-   A short discussion on [bias, filter divergence and covariance
    inflation.](#discussion)
-   And another one on [synthetic observations](#syntheticobservations).

You should *absolutely* run the DARTLAB interactive tutorial (if you
have Matlab available) and look at the DARTLAB presentation slides
[Website](https://svn-dares-dart.cgd.ucar.edu/DART/releases/Lanai/documentation/DART_LAB/DART_LAB.html)
or [local file](../../documentation/DART_LAB/DART_LAB.html) in the
*DART\_LAB* directory, and then take the tutorial in the *DART/tutorial*
directory.

[]{#compilers}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

### Requirements: an F90 Compiler

The DART software has been successfully built on many Linux, OS/X, and
supercomputer platforms with compilers that include [GNU Fortran
Compiler ("gfortran")](http://gcc.gnu.org/fortran) (free), [Intel
Fortran Compiler for Linux and Mac
OS/X](http://software.intel.com/en-us/fortran-compilers), [Portland
Group Fortran Compiler](http://www.pgroup.com), [Lahey Fortran
Compiler](http://www.lahey.com), [Pathscale Fortran
Compiler](http://www.pathscale.com), and the Cray native compiler. Since
recompiling the code is a necessity to experiment with different models,
there are no binaries to distribute.

[]{#netCDFlib}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

### Requirements: the *netCDF* library

DART uses the [netCDF](http://www.unidata.ucar.edu/packages/netcdf/)
self-describing data format for the results of assimilation experiments.
These files have the extension *.nc* and can be read by a number of
standard data analysis tools. In particular, DART also makes use of the
F90 interface to the library which is available through the *netcdf.mod*
and *typesizes.mod* modules. *IMPORTANT*: different compilers create
these modules with different "case" filenames, and sometimes they are
not **both** installed into the expected directory. It is required that
both modules be present. The normal place would be in the
`netcdf/include` directory, as opposed to the `netcdf/lib` directory.

If the netCDF library does not exist on your system, you must build it
(as well as the F90 interface modules). The library and instructions for
building the library or installing from an RPM may be found at the
netCDF home page: <http://www.unidata.ucar.edu/packages/netcdf/>

The location of the netCDF library, *libnetcdf.a*, and the locations of
both *netcdf.mod* and *typesizes.mod* will be needed by the makefile
template, as described in the [compiling](#compiling) section. Depending
on the netCDF build options, the Fortran 90 interfaces may be built in a
separate library named *netcdff.a* and you may need to add *-lnetcdff*
to the library flags.

[]{#download}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Downloading the distribution.
-----------------------------

**HURRAY**! The DART source code is now distributed through an anonymous
Subversion server! The **big** advantage is the ability to patch or
update existing code trees at your discretion. Subversion (the
client-side app is '**svn**') allows you to compare your code tree with
one on a remote server and selectively update individual files or groups
of files. Furthermore, now everyone has access to any version of any
file in the project, which is a huge help for developers. I have a brief
summary of the svn commands I use most posted at:
<http://www.image.ucar.edu/~thoar/svn_primer.html>

The resources to develop and support DART come from our ability to
demonstrate our growing user base. We ask that you register at our
download site <http://www.image.ucar.edu/DAReS/DART/DART_download> and
promise that the information will only be used to notify you of new DART
releases and shown to our sponsers in an aggregated form: "Look - we
have three users from Tonawanda, NY". After filling in the form, you
will be directed to a website that has instructions on how to download
the code.

svn has adopted the strategy that "disk is cheap". In addition to
downloading the code, it downloads an additional copy of the code to
store locally (in hidden .svn directories) as well as some
administration files. This allows svn to perform some commands even when
the repository is not available. It does double the size of the code
tree for the initial download, but then future updates download just the
changes, so they usually happen very quickly.

If you follow the instructions on the download site, you should wind up
with a directory named *DART*. Compiling the code in this tree (as is
usually the case) will necessitate much more space.

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

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Customizing the build scripts -- Overview.
------------------------------------------

DART executable programs are constructed using two tools: *make* and
*mkmf*. The *make* utility is a very common piece of software that
requires a user-defined input file that records dependencies between
different source files. *make* then performs a hierarchy of actions when
one or more of the source files is modified. The *mkmf* utility is a
custom preprocessor that generates a *make* input file (named
*Makefile*) and an example namelist *input.nml.**program*\_default**
with the default values. The *Makefile* is designed specifically to work
with object-oriented Fortran90 (and other languages) for systems like
DART.

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
*input.nml.**program*\_default**, so as not to clash with any exising
*input.nml* that may exist in that directory.

[]{#template}

### Building and Customizing the 'mkmf.template' file {#building-and-customizing-the-mkmf.template-file .indent1}

A series of templates for different compilers/architectures exists in
the *DART/mkmf/* directory and have names with extensions that identify
the compiler, the architecture, or both. This is how you inform the
build process of the specifics of your system. Our intent is that you
copy one that is similar to your system into *mkmf.template* and
customize it. For the discussion that follows, knowledge of the contents
of one of these templates (i.e. *mkmf.template.gfortran*) is needed.
Note that only the LAST lines are shown here, the head of the file is
just a big comment (worth reading, btw).

<div class="routine">

...\
MPIFC = mpif90\
MPILD = mpif90\
FC = gfortran\
LD = gfortran\
NETCDF = /usr/local\
INCS = \${NETCDF}/include\
FFLAGS = -O2 -I\$(INCS)\
LIBS = -L\${NETCDF}/lib -lnetcdf\
LDFLAGS = -I\$(INCS) \$(LIBS)\

</div>

Essentially, each of the lines defines some part of the resulting
*Makefile*. Since *make* is particularly good at sorting out
dependencies, the order of these lines really doesn't make any
difference. The *FC = gfortran* line ultimately defines the Fortran90
compiler to use, etc. The lines which are most likely to need
site-specific changes start with *FFLAGS* and *NETCDF*, which indicate
where to look for the netCDF F90 modules and the location of the netCDF
library and modules.\
\
If you have MPI installed on your system *MPIFC, MPILD* dictate which
compiler will be used in that instance. If you do not have MPI, these
variables are of no consequence.

[](netCDF)

#### NETCDF {#netcdf .indent2}

Modifying the *NETCDF* value should be relatively straightforward.\
Change the string to reflect the location of your netCDF installation
containing *netcdf.mod* and *typesizes.mod*. The value of the *NETCDF*
variable will be used by the *FFLAGS, LIBS,* and *LDFLAGS* variables.\

[](fflags)

#### FFLAGS {#fflags .indent2}

Each compiler has different compile flags, so there is really no way to
exhaustively cover this other than to say the templates as we supply
them should work -- depending on the location of your netCDF. The
low-order models can be compiled without a *-r8* switch, but the
*bgrid\_solo* model cannot.

[](libs)

#### LIBS {#libs .indent2}

The Fortran 90 interfaces may be part of the default *netcdf.a* library
and *-lnetcdf* is all you need. However it is also common for the
Fortran 90 interfaces to be built in a separate library named
*netcdff.a*. In that case you will need *-lnetcdf* and also *-lnetcdff*
on the **LIBS** line. This is a build-time option when the netCDF
libraries are compiled so it varies from site to site.

\
[]{#path_names}

### Customizing the 'path\_names\_\*' file {#customizing-the-path_names_-file .indent1}

Several *path\_names\_\** files are provided in the *work* directory for
each specific model, in this case: *DART/models/lorenz\_63/work*. Since
each model comes with its own set of files, the *path\_names\_\** files
need no customization.

[]{#building}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Building the Lorenz\_63 DART project.
-------------------------------------

DART executables are constructed in a *work* subdirectory under the
directory containing code for the given model. From the top-level DART
directory change to the L63 work directory and list the contents:

<div class="unix">

cd DART/models/lorenz\_63/work\
ls -1

</div>

With the result:

    Posterior_Diag.nc
    Prior_Diag.nc
    True_State.nc
    filter_ics
    filter_restart
    input.nml
    mkmf_create_fixed_network_seq
    mkmf_create_obs_sequence
    mkmf_filter
    mkmf_obs_diag
    mkmf_obs_sequence_tool
    mkmf_perfect_model_obs
    mkmf_preprocess
    mkmf_restart_file_tool
    mkmf_wakeup_filter
    obs_seq.final
    obs_seq.in
    obs_seq.out
    obs_seq.out.average
    obs_seq.out.x
    obs_seq.out.xy
    obs_seq.out.xyz
    obs_seq.out.z
    path_names_create_fixed_network_seq
    path_names_create_obs_sequence
    path_names_filter
    path_names_obs_diag
    path_names_obs_sequence_tool
    path_names_perfect_model_obs
    path_names_preprocess
    path_names_restart_file_tool
    path_names_wakeup_filter
    perfect_ics
    perfect_restart
    quickbuild.csh
    set_def.out
    workshop_setup.csh

In all the *work* directories there will be a *quickbuild.csh* script
that builds or rebuilds the executables. The following instructions do
this work by hand to introduce you to the individual steps, but in
practice running quickbuild will be the normal way to do the compiles.

There are nine *mkmf\_xxxxxx* files for the programs

1.  *preprocess*,
2.  *create\_obs\_sequence*,
3.  *create\_fixed\_network\_seq*,
4.  *perfect\_model\_obs*,
5.  *filter*,
6.  *wakeup\_filter*,
7.  *obs\_sequence\_tool*, and
8.  *restart\_file\_tool*, and
9.  *obs\_diag*,

along with the corresponding *path\_names\_xxxxxx* files. There are also
files that contain initial conditions, netCDF output, and several
observation sequence files, all of which will be discussed later. You
can examine the contents of one of the *path\_names\_xxxxxx* files, for
instance *path\_names\_filter*, to see a list of the relative paths of
all files that contain Fortran90 modules required for the program
*filter* for the L63 model. All of these paths are relative to your
*DART* directory. The first path is the main program (*filter.f90*) and
is followed by all the Fortran90 modules used by this program (after
preprocessing).

The *mkmf\_xxxxxx* scripts are cryptic but should not need to be
modified -- as long as you do not restructure the code tree (by moving
directories, for example). The function of the *mkmf\_xxxxxx* script is
to generate a *Makefile* and an *input.nml.**program*\_default** file.
It does not do the compile; *make* does that:

<div class="unix">

csh mkmf\_preprocess\
make

</div>

The first command generates an appropriate *Makefile* and the
*input.nml.preprocess\_default* file. The second command results in the
compilation of a series of Fortran90 modules which ultimately produces
an executable file: *preprocess*. Should you need to make any changes to
the *DART/mkmf/mkmf.template*, you will need to regenerate the
*Makefile*.\
\
The *preprocess* program actually builds source code to be used by all
the remaining modules. It is **imperative** to actually **run**
*preprocess* before building the remaining executables. This is how the
same code can assimilate state vector 'observations' for the Lorenz\_63
model and real radar reflectivities for WRF without needing to specify a
set of radar operators for the Lorenz\_63 model!\
\
*preprocess* reads the *&preprocess\_nml* namelist to determine what
observations and operators to incorporate. For this exercise, we will
use the values in *input.nml*. *preprocess* is designed to abort if the
files it is supposed to build already exist. For this reason, it is
necessary to remove a couple files (if they exist) before you run the
preprocessor. (The *quickbuild.csh* script will do this for you
automatically.)

<div class="unix">

    \rm -f ../../../obs_def/obs_def_mod.f90
    \rm -f ../../../obs_kind/obs_kind_mod.f90
    ./preprocess
    ls -l ../../../obs_def/obs_def_mod.f90
    ls -l ../../../obs_kind/obs_kind_mod.f90

</div>

This created *../../../obs\_def/obs\_def\_mod.f90* from
*../../../obs\_kind/DEFAULT\_obs\_kind\_mod.F90* and several other
modules. *../../../obs\_kind/obs\_kind\_mod.f90* was created similarly.
Now we can build the rest of the project.\
\
A series of object files for each module compiled will also be left in
the work directory, as some of these are undoubtedly needed by the build
of the other DART components. You can proceed to create the other
programs needed to work with L63 in DART as follows:

<div class="unix">

csh mkmf\_create\_obs\_sequence\
make\
csh mkmf\_create\_fixed\_network\_seq\
make\
csh mkmf\_perfect\_model\_obs\
make\
csh mkmf\_filter\
make\
csh mkmf\_obs\_diag\
make

</div>

\

The result (hopefully) is that six executables now reside in your work
directory. The most common problem is that the netCDF libraries and
include files (particularly *typesizes.mod*) are not found. Edit the
*DART/mkmf/mkmf.template*, recreate the *Makefile*, and try again.

  program                         purpose
  ------------------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *preprocess*                    creates custom source code for just the observation types of interest
  *create\_obs\_sequence*         specify a (set) of observation characteristics taken by a particular (set of) instruments
  *create\_fixed\_network\_seq*   repeat a set of observations through time to simulate observing networks where observations are taken in the same location at regular (or irregular) intervals
  *perfect\_model\_obs*           generate "true state" for synthetic observation experiments. Can also be used to 'spin up' a model by running it for a long time.
  *filter*                        does the assimilation
  *obs\_diag*                     creates observation-space diagnostic files to be explored by the Matlab® scripts.
  *obs\_sequence\_tool*           manipulates observation sequence files. It is not generally needed (particularly for low-order models) but can be used to combine observation sequences or convert from ASCII to binary or vice-versa. We will not cover its use in this document.
  *restart\_file\_tool*           manipulates the initial condition and restart files. We're going to ignore this one here.
  *wakeup\_filter*                is only needed for MPI applications. We're starting at the beginning here, so we're going to ignore this one, too.

[]{#Running}

<div class="top">

\[[top](#)\]

</div>

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
    by running the filter; diagnostic output is generated.

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
     Starting program create_obs_sequence
     Initializing the utilities module.
     Trying to log to unit   10
     Trying to open file dart_log.out
     
     Registering module :
     $url: http://squish/DART/trunk/utilities/utilities_mod.f90 $
     $revision: 2713 $
     $date: 2007-03-25 22:09:04 -0600 (Sun, 25 Mar 2007) $
     Registration complete.

     &UTILITIES_NML
     TERMLEVEL= 2,LOGFILENAME=dart_log.out                                          
                                                                                
     /
     
     Registering module :
     $url: http://squish/DART/trunk/obs_sequence/create_obs_sequence.f90 $
     $revision: 2713 $
     $date: 2007-03-25 22:09:04 -0600 (Sun, 25 Mar 2007) $
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
     $url: http://squish/DART/trunk/obs_def/DEFAULT_obs_def_mod.F90 $
     $revision: 2820 $
     $date: 2007-04-09 10:37:47 -0600 (Mon, 09 Apr 2007) $
     Registration complete.
     
     
     Registering module :
     $url: http://squish/DART/trunk/obs_kind/DEFAULT_obs_kind_mod.F90 $
     $revision: 2822 $
     $date: 2007-04-09 10:39:08 -0600 (Mon, 09 Apr 2007) $
     Registration complete.
     
     ------------------------------------------------------
     
     initialize_module obs_kind_nml values are
     
     -------------- ASSIMILATE_THESE_OBS_TYPES --------------
     RAW_STATE_VARIABLE
     -------------- EVALUATE_THESE_OBS_TYPES --------------
     ------------------------------------------------------
     
          Input -1 * state variable index for identity observations
          OR input the name of the observation kind from table below:
          OR input the integer index, BUT see documentation...
            1 RAW_STATE_VARIABLE

    -1

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
defines the model timestep as being 3600 seconds. If we declare that we
have one observation per day for 1000 days, we create an observation
sequence definition spanning 24000 'model' timesteps; sufficient to
spin-up the model onto the attractor. Finally, enter a name for the
'observation sequence definition' file. Note again: there are no
observation values present in this file. Just an observation type,
location, time and the error characteristics. We are going to populate
the observation sequence with the *perfect\_model\_obs* program.

<div class="unix">

    [unixprompt]$ ./create_fixed_network_seq

     ...

     Registering module :
     $url: http://squish/DART/trunk/obs_sequence/obs_sequence_mod.f90 $
     $revision: 2749 $
     $date: 2007-03-30 15:07:33 -0600 (Fri, 30 Mar 2007) $
     Registration complete.
     
     static_init_obs_sequence obs_sequence_nml values are
     &OBS_SEQUENCE_NML
     WRITE_BINARY_OBS_SEQUENCE =  F,
     /
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
               1
               2
               3
    ...
             997
             998
             999
            1000
    What is output file name for sequence (  obs_seq.in   is recommended )
    obs_seq.in
     write_obs_seq  opening formatted file obs_seq.in
     write_obs_seq closed file obs_seq.in

</div>

#### 1.3 Initialize the model onto the attractor. {#initialize-the-model-onto-the-attractor. .indent1}

*perfect\_model\_obs* can now advance the arbitrary initial state for
24,000 timesteps to move it onto the attractor.\
*perfect\_model\_obs* uses the Fortran90 namelist input mechanism
instead of (admittedly gory, but temporary) interactive input. All of
the DART software expects the namelists to found in a file called
*input.nml*. When you built the executable, an example namelist was
created *input.nml.perfect\_model\_obs\_default* that contains all of
the namelist input for the executable. If you followed the example, each
namelist was saved to a unique name. We must now rename and edit the
namelist file for *perfect\_model\_obs*. Copy
*input.nml.perfect\_model\_obs\_default* to *input.nml* and edit it to
look like the following: (just worry about the highlighted stuff - and
whitespace doesn't matter)

<div class="unix">

cp input.nml.perfect\_model\_obs\_default input.nml\

</div>

<div class="routineIndent1">

    &perfect_model_obs_nml
       start_from_restart    = .false.,
       output_restart        = .true.,
       async                 = 0,
       init_time_days        = 0,
       init_time_seconds     = 0,
       first_obs_days        = -1,
       first_obs_seconds     = -1,
       last_obs_days         = -1,
       last_obs_seconds      = -1,
       output_interval       = 1,
       restart_in_file_name  = "perfect_ics",
       restart_out_file_name = "perfect_restart",
       obs_seq_in_file_name  = "obs_seq.in",
       obs_seq_out_file_name = "obs_seq.out",
       adv_ens_command       = "./advance_ens.csh"  /

    &ensemble_manager_nml
       single_restart_file_in  = .true.,
       single_restart_file_out = .true.,
       perturbation_amplitude  = 0.2  /

    &assim_tools_nml
       filter_kind                     = 1,
       cutoff                          = 0.2,
       sort_obs_inc                    = .false.,
       spread_restoration              = .false.,
       sampling_error_correction       = .false.,
       adaptive_localization_threshold = -1,
       print_every_nth_obs             = 0  /

    &cov_cutoff_nml
       select_localization = 1  /

    &reg_factor_nml
       select_regression    = 1,
       input_reg_file       = "time_mean_reg",
       save_reg_diagnostics = .false.,
       reg_diagnostics_file = "reg_diagnostics"  /

    &obs_sequence_nml
       write_binary_obs_sequence = .false.  /

    &obs_kind_nml
       assimilate_these_obs_types = 'RAW_STATE_VARIABLE'  /

    &assim_model_nml
       write_binary_restart_files = .true. /

    &model_nml
       sigma  = 10.0,
       r      = 28.0,
       b      = 2.6666666666667,
       deltat = 0.01,
       time_step_days = 0,
       time_step_seconds = 3600  /

    &utilities_nml
       TERMLEVEL = 1,
       logfilename = 'dart_log.out'  /

</div>

For the moment, only two namelists warrant explanation. Each namelists
is covered in detail in the html files accompanying the source code for
the module.

### perfect\_model\_obs\_nml {#perfect_model_obs_nml .indent1}

  namelist variable             description
  ----------------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *start\_from\_restart*        When set to 'false', *perfect\_model\_obs* generates an arbitrary initial condition (which cannot be guaranteed to be on the L63 attractor). When set to 'true', a restart file (specified by *restart\_in\_file\_name*) is read.
  *output\_restart*             When set to 'true', *perfect\_model\_obs* will record the model state at the end of this integration in the file named by *restart\_out\_file\_name*.
  *async*                       The lorenz\_63 model is advanced through a subroutine call - indicated by async = 0. There is no other valid value for this model.
  *init\_time\_xxxx*            the start time of the integration.
  *first\_obs\_xxxx*            the time of the first observation of interest. While not needed in this example, you can skip observations if you want to. A value of -1 indicates to start at the beginning.
  *last\_obs\_xxxx*             the time of the last observation of interest. While not needed in this example, you do not have to assimilate all the way to the end of the observation sequence file. A value of -1 indicates to use all the observations.
  *output\_interval*            interval at which to save the model state (in True\_State.nc).
  *restart\_in\_file\_name*     is ignored when 'start\_from\_restart' is 'false'.
  *restart\_out\_file\_name*    if *output\_restart* is 'true', this specifies the name of the file containing the model state at the end of the integration.
  *obs\_seq\_in\_file\_name*    specifies the file name that results from running *create\_fixed\_network\_seq*, i.e. the 'observation sequence definition' file.
  *obs\_seq\_out\_file\_name*   specifies the output file name containing the 'observation sequence', finally populated with (perfect?) 'observations'.
  *advance\_ens\_command*       specifies the shell commands or script to execute when async /= 0.

### utilities\_nml {#utilities_nml .indent1}

  namelist variable   description
  ------------------- --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *TERMLEVEL*         When set to '1' the programs terminate when a 'warning' is generated. When set to '2' the programs terminate only with 'fatal' errors.
  *logfilename*       Run-time diagnostics are saved to this file. This namelist is used by all programs, so the file is opened in APPEND mode. Subsequent executions cause this file to grow.

Executing *perfect\_model\_obs* will integrate the model 24,000 steps
and output the resulting state in the file *perfect\_restart*.
Interested parties can check the spinup in the *True\_State.nc* file.

<div class="unix">

./perfect\_model\_obs

</div>

[]{#ensemblate}

### 2. Generate a set of ensemble initial conditions. {#generate-a-set-of-ensemble-initial-conditions. .indent1}

The set of initial conditions for a 'perfect model' experiment is
created in several steps. 1) Starting from the spun-up state of the
model (available in *perfect\_restart*), run *perfect\_model\_obs* to
generate the 'true state' of the experiment and a corresponding set of
observations. 2) Feed the same initial spun-up state and resulting
observations into *filter*.\
\
The first step is achieved by changing a perfect\_model\_obs namelist
parameter, copying *perfect\_restart* to *perfect\_ics*, and rerunning
*perfect\_model\_obs*. This execution of *perfect\_model\_obs* will
advance the model state from the end of the first 24,000 steps to the
end of an additional 24,000 steps and place the final state in
*perfect\_restart*. The rest of the namelists in *input.nml* should
remain unchanged.

<div class="routineIndent1">

    &perfect_model_obs_nml
       start_from_restart    = .true.,
       output_restart        = .true.,
       async                 = 0,
       init_time_days        = 0,
       init_time_seconds     = 0,
       first_obs_days        = -1,
       first_obs_seconds     = -1,
       last_obs_days         = -1,
       last_obs_seconds      = -1,
       output_interval       = 1,
       restart_in_file_name  = "perfect_ics",
       restart_out_file_name = "perfect_restart",
       obs_seq_in_file_name  = "obs_seq.in",
       obs_seq_out_file_name = "obs_seq.out",
       adv_ens_command       = "./advance_ens.csh"  /

</div>

\
\

<div class="unix">

cp perfect\_restart perfect\_ics\
./perfect\_model\_obs

</div>

A *True\_State.nc* file is also created. It contains the 'true' state of
the integration.

#### Generating the ensemble {#generating-the-ensemble .indent1}

This step (\#2 from above) is done with the program *filter*, which also
uses the Fortran90 namelist mechanism for input. It is now necessary to
copy the *input.nml.filter\_default* namelist to *input.nml*.

<div class="indent1">

cp input.nml.filter\_default input.nml\

</div>

You may also build one master namelist containting all the required
namelists. Having unused namelists in the *input.nml* does not hurt
anything, and it has been so useful to be reminded of what is possible
that we made it an error to NOT have a required namelist. Take a peek at
any of the other models for examples of a "fully qualified"
*input.nml*.\
\
*HINT:* if you used *svn* to get the project, try 'svn revert input.nml'
to restore the namelist that was distributed with the project - which
DOES have all the namelist blocks. Just be sure the values match the
examples here.

<div class="routineIndent1">

    &filter_nml
       async                    = 0,
       adv_ens_command          = "./advance_model.csh",
       ens_size                 = 100,
       start_from_restart       = .false.,
       output_restart           = .true.,
       obs_sequence_in_name     = "obs_seq.out",
       obs_sequence_out_name    = "obs_seq.final",
       restart_in_file_name     = "perfect_ics",
       restart_out_file_name    = "filter_restart",
       init_time_days           = 0,
       init_time_seconds        = 0,
       first_obs_days           = -1,
       first_obs_seconds        = -1,
       last_obs_days            = -1,
       last_obs_seconds         = -1,
       num_output_state_members = 20,
       num_output_obs_members   = 20,
       output_interval          = 1,
       num_groups               = 1,
       input_qc_threshold       =  4.0,
       outlier_threshold        = -1.0,
       output_forward_op_errors = .false.,
       output_timestamps        = .false.,
       output_inflation         = .true.,

       inf_flavor               = 0,                       0,
       inf_start_from_restart   = .false.,                 .false.,
       inf_output_restart       = .false.,                 .false.,
       inf_deterministic        = .true.,                  .true.,
       inf_in_file_name         = 'not_initialized',       'not_initialized',
       inf_out_file_name        = 'not_initialized',       'not_initialized',
       inf_diag_file_name       = 'not_initialized',       'not_initialized',
       inf_initial              = 1.0,                     1.0,
       inf_sd_initial           = 0.0,                     0.0,
       inf_lower_bound          = 1.0,                     1.0,
       inf_upper_bound          = 1000000.0,               1000000.0,
       inf_sd_lower_bound       = 0.0,                     0.0
    /

    &smoother_nml
       num_lags              = 0,
       start_from_restart    = .false.,
       output_restart        = .false.,
       restart_in_file_name  = 'smoother_ics',
       restart_out_file_name = 'smoother_restart'  /

    &ensemble_manager_nml
       single_restart_file_in  = .true.,
       single_restart_file_out = .true.,
       perturbation_amplitude  = 0.2  /

    &assim_tools_nml
       filter_kind                     = 1,
       cutoff                          = 0.2,
       sort_obs_inc                    = .false.,
       spread_restoration              = .false.,
       sampling_error_correction       = .false.,
       adaptive_localization_threshold = -1,
       print_every_nth_obs             = 0  /

    &cov_cutoff_nml
       select_localization = 1  /

    &reg_factor_nml
       select_regression    = 1,
       input_reg_file       = "time_mean_reg",
       save_reg_diagnostics = .false.,
       reg_diagnostics_file = "reg_diagnostics"  /

    &obs_sequence_nml
       write_binary_obs_sequence = .false.  /

    &obs_kind_nml
       assimilate_these_obs_types = 'RAW_STATE_VARIABLE'  /

    &assim_model_nml
       write_binary_restart_files = .true. /

    &model_nml
       sigma  = 10.0,
       r      = 28.0,
       b      = 2.6666666666667,
       deltat = 0.01,
       time_step_days = 0,
       time_step_seconds = 3600  /

    &utilities_nml
       TERMLEVEL = 1,
       logfilename = 'dart_log.out'  /

</div>

Only the non-obvious(?) entries for *filter\_nml* will be discussed.

  namelist variable               description
  ------------------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *ens\_size*                     Number of ensemble members. 100 is sufficient for most of the L63 exercises.
  *start\_from\_restart*          when '.false.', *filter* will generate its own ensemble of initial conditions. It is important to note that the filter still makes use of the file named by *restart\_in\_file\_name* (i.e. *perfect\_ics*) by randomly perturbing these state variables.
  *num\_output\_state\_members*   specifies the number of state vectors contained in the netCDF diagnostic files. May be a value from 0 to *ens\_size*.
  *num\_output\_obs\_members*     specifies the number of 'observations' (derived from applying the forward operator to the state vector) are contained in the *obs\_seq.final* file. May be a value from 0 to *ens\_size*
  *inf\_flavor*                   A value of 0 results in no inflation.(spin-up)

The filter is told to generate its own ensemble initial conditions since
*start\_from\_restart* is '.false.'. However, it is important to note
that the filter still makes use of *perfect\_ics* which is set to be the
*restart\_in\_file\_name*. This is the model state generated from the
first 24,000 step model integration by *perfect\_model\_obs*. *Filter*
generates its ensemble initial conditions by randomly perturbing the
state variables of this state.

*num\_output\_state\_members* are '.true.' so the state vector is output
at every time for which there are observations (once a day here).
*Posterior\_Diag.nc* and *Prior\_Diag.nc* then contain values for 20
ensemble members once a day. Once the namelist is set, execute *filter*
to integrate the ensemble forward for 24,000 steps with the final
ensemble state written to the *filter\_restart*. Copy the
*perfect\_model\_obs* restart file *perfect\_restart* (the \`true
state') to *perfect\_ics*, and the *filter* restart file
*filter\_restart* to *filter\_ics* so that future assimilation
experiments can be initialized from these spun-up states.

<div class="unix">

    ./filter
    cp perfect_restart perfect_ics
    cp filter_restart filter_ics

</div>

The spin-up of the ensemble can be viewed by examining the output in the
netCDF files *True\_State.nc* generated by *perfect\_model\_obs* and
*Posterior\_Diag.nc* and *Prior\_Diag.nc* generated by *filter*. To do
this, see the detailed discussion of matlab diagnostics in Appendix I.
[]{#simulate}

### 3. Simulate a particular observing system. {#simulate-a-particular-observing-system. .indent1}

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
  *0, 43200*       \# First observation after 12 hours (0 days, 12 \* 3600 seconds)
  *0, 43200*       \# Observations every 12 hours
  *obs\_seq.in*    \# Output file for observation sequence definition
  ---------------- ------------------------------------------------------------------

[]{#generate}

### 4. Generate a particular observing system and true state. {#generate-a-particular-observing-system-and-true-state. .indent1}

An observation sequence file is now generated by running
*perfect\_model\_obs* with the namelist values (unchanged from step 2):

<div class="routineIndent1">

    &perfect_model_obs_nml
       start_from_restart    = .true.,
       output_restart        = .true.,
       async                 = 0,
       init_time_days        = 0,
       init_time_seconds     = 0,
       first_obs_days        = -1,
       first_obs_seconds     = -1,
       last_obs_days         = -1,
       last_obs_seconds      = -1,
       output_interval       = 1,
       restart_in_file_name  = "perfect_ics",
       restart_out_file_name = "perfect_restart",
       obs_seq_in_file_name  = "obs_seq.in",
       obs_seq_out_file_name = "obs_seq.out",
       adv_ens_command       = "./advance_ens.csh"  /

</div>

This integrates the model starting from the state in *perfect\_ics* for
1000 12-hour intervals outputting synthetic observations of the three
state variables every 12 hours and producing a netCDF diagnostic file,
*True\_State.nc*.

[]{#assimilate}

### 5. Filtering. {#filtering. .indent1}

Finally, *filter* can be run with its namelist set to:

<div class="routineIndent1">

    &filter_nml
       async                    = 0,
       adv_ens_command          = "./advance_model.csh",
       ens_size                 = 100,
       start_from_restart       = .true.,
       output_restart           = .true.,
       obs_sequence_in_name     = "obs_seq.out",
       obs_sequence_out_name    = "obs_seq.final",
       restart_in_file_name     = "filter_ics",
       restart_out_file_name    = "filter_restart",
       init_time_days           = 0,
       init_time_seconds        = 0,
       first_obs_days           = -1,
       first_obs_seconds        = -1,
       last_obs_days            = -1,
       last_obs_seconds         = -1,
       num_output_state_members = 20,
       num_output_obs_members   = 20,
       output_interval          = 1,
       num_groups               = 1,
       input_qc_threshold       =  4.0,
       outlier_threshold        = -1.0,
       output_forward_op_errors = .false.,
       output_timestamps        = .false.,
       output_inflation         = .true.,

       inf_flavor               = 0,                       0,
       inf_start_from_restart   = .false.,                 .false.,
       inf_output_restart       = .false.,                 .false.,
       inf_deterministic        = .true.,                  .true.,
       inf_in_file_name         = 'not_initialized',       'not_initialized',
       inf_out_file_name        = 'not_initialized',       'not_initialized',
       inf_diag_file_name       = 'not_initialized',       'not_initialized',
       inf_initial              = 1.0,                     1.0,
       inf_sd_initial           = 0.0,                     0.0,
       inf_lower_bound          = 1.0,                     1.0,
       inf_upper_bound          = 1000000.0,               1000000.0,
       inf_sd_lower_bound       = 0.0,                     0.0
     /

</div>

*filter* produces two output diagnostic files, *Prior\_Diag.nc* which
contains values of the ensemble mean, ensemble spread, and ensemble
members for 12- hour lead forecasts before assimilation is applied and
*Posterior\_Diag.nc* which contains similar data for after the
assimilation is applied (sometimes referred to as analysis values).

Now try applying all of the matlab diagnostic functions described in
[the Matlab® Diagnostics section](#matlab). []{#Workshop}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

The Tutorial.
-------------

The *DART/tutorial* documents are an excellent way to kick the tires on
DART and learn about ensemble data assimilation. If you have gotten this
far, you can run anything in the tutorial.

[]{#matlab}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Matlab® Diagnostics
-------------------

The output files are netCDF files and may be examined with many
different software packages. We use Matlab®, and provide our diagnostic
scripts in the hopes that they are useful.

The diagnostic scripts and underlying functions reside in two places:
*DART/diagnostics/matlab* and *DART/matlab*. They are reliant on the
public-domain MEXNC/SNCTOOLS netCDF interface from
<http://mexcdf.sourceforge.net>. If you do not have them installed on
your system and want to use Matlab to peruse netCDF, you must follow
their installation instructions. The 'interested reader' may want to
look at the *DART/matlab/startup.m* file I use on my system. If you put
it in your *\$HOME/matlab* directory it is invoked every time you start
up Matlab.

Once you can access the *nc\_varget* function from within Matlab you can
use our diagnostic scripts. It is necessary to prepend the location of
the *DART/matlab* scripts to the *matlabpath*. Keep in mind the location
of the netcdf operators on your system WILL be different from ours ...
and that's OK.\
\

<div class="unix">

    [models/lorenz_63/work]$ matlab -nodesktop

                                                 < M A T L A B >
                                     Copyright 1984-2002 The MathWorks, Inc.
                                         Version 6.5.0.180913a Release 13
                                                   Jun 18 2002

      Using Toolbox Path Cache.  Type "help toolbox_path_cache" for more info.
     
      To get started, type one of these: helpwin, helpdesk, or demo.
      For product information, visit www.mathworks.com.

    >> which nc_varget
    /contrib/matlab/snctools/4024/nc_varget.m
    >>ls *.nc

    ans =

    Posterior_Diag.nc  Prior_Diag.nc  True_State.nc


    >>path('../../../matlab',path)
    >>path('../../../diagnostics/matlab',path)
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

<div class="top">

\[[top](#)\]

</div>

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
then tend to give less weight to observations than they should be given.
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
*plot\_bins*.

A simple, but surprisingly effective way of dealing with filter
divergence is known as covariance inflation. In this method, the prior
ensemble estimate of the state is expanded around its mean by a constant
factor, effectively increasing the prior estimate of uncertainty while
leaving the prior mean estimate unchanged. The program *filter* has a
group of namelist parameters that controls the application of covariance
inflation. For a simple set of inflation values, you will set
*inf\_flavor*, and *inf\_initial*. These values come in pairs; the first
value controls inflation of the prior ensemble values, while the second
controls inflation of the posterior values. Up to this point
*inf\_flavor* has been set to 0 indicating that the prior ensemble is
left unchanged. Setting the first value of *inf\_flavor* to 3 enables
one variety of inflation. Set *inf\_initial* to different values (try
1.05 and 1.10 and other values). In each case, use the diagnostic matlab
tools to examine the resulting changes to the error, the ensemble spread
(via rank histogram bins, too), etc. What kind of relation between
spread and error is seen in this model?

There are many more options for inflation, including spatially and
temporarily varying values, with and without damping. See the discussion
of all inflation-related namelist items
[Website](https://svn-dares-dart.cgd.ucar.edu/DART/releases/Lanai/filter/filter.html#Inflation)
or [local file](../../assimilation_code/programs/filter.html#Inflation).

[]{#syntheticobservations}

<div class="top">

\[[top](#)\]

</div>

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

[]{#CurrentUsers}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Notes for Current Users
-----------------------

If you have been updating from the development branch of the DART
subversion repository you will not notice much difference between that
and the Lanai release. If you are still running the Kodiak release there
are many new models, new observation types, capabilities in the
assimilation tools, new diagnostics, and new utilities. There is a short
list of non-backwards compatible changes ([see below](#Nonbackward)),
and then a long list of new options and functions.

In the near future we will be making substantial changes to the internal
structure of DART to accomodate both larger models and machines with
thousands of processors. We will continue to maintain the Lanai release
with bug fixes, but we will be updating the subversion trunk with new
and non-backwards-compatible code. Checking out the Lanai release branch
and running 'svn update' from time to time is the recommended way to
update your DART tree.

[]{#Nonbackward}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Non-backwards Compatible Changes
--------------------------------

Changes in the Lanai release (13 Dec 2013) which are *not* backwards
compatible with the Kodiak release (30 June 2011):

1.  The DART system uses a new random number generator based on the
    Mersenne Twister algorithm from the GNU scientific library. It is
    believed to have better behavior in general, and in particular when
    it is frequently reseeded, as may be the case in some
    perfect\_model\_obs experiments. The seed in perfect\_model\_obs is
    now based on the time-stamp associated with the data, so running
    single advances as separate invocations of the executable will still
    result in a good random distribution of the observation errors. The
    seeds in several other places in the code have been changed so they
    are more consistent in the face of different numbers of MPI tasks
    when executing. The random values should reproduce if an identical
    run is repeated, but there are still a few places in the code where
    changing the number of MPI tasks results in different seeds being
    created for the random number generator, and so the
    non-deterministic values will differ.
2.  The WRF model\_mod now interpolates in the vertical in log(pressure)
    space instead of linear pressure space. This is the new default.
    There is a module global variable that can be set at compile time to
    restore the previous behavior.
3.  The POP model\_mod used to interpolate sensible temperature
    observations using a potential temperature field in the state
    vector. The code now correctly does the conversion from potential
    temperature to sensible (in-situ) temperature during the forward
    operator process.
4.  If your *model\_mod.f90* provides a customized *get\_close\_obs()*
    routine that makes use of the types/kinds arguments for either the
    base location or the close location list, there is an important
    change in this release. The fifth argument to the
    *get\_close\_obs()* call is now a list of generic kinds
    corresponding to the location list. The fourth argument to the
    *get\_dist()* routine is now also a generic kind and not a specific
    type. In previous versions of the system the list of close locations
    was sometimes a list of specific types and other times a list of
    generic kinds. The system now always passes generic kinds for the
    close locations list for consistency. The base location and specific
    type remains the same as before. If you have a *get\_close\_obs()*
    routine in your *model\_mod.f90* file and have questions about
    usage, [contact](mailto:dart@ucar.edu) the DART development team.
5.  The *obs\_common\_subset* program namelist has changed. The program
    compares *obs\_seq.final* files that were produced by different runs
    of filter using the same input obs\_sequence file. The old version
    supported comparing only 2 files at a time; this version supports up
    to 50. It also enforces the implicit assumption that the incoming
    obs\_seq.final files are identical except for the DART QC and the
    obs values.
6.  The simple\_advection model was incorrectly calling the random
    number generator initialization routines after generating some
    random numbers. It now correctly initializes the generator before
    getting any random values.
7.  The gts\_to\_dart converter now creates separate obs types for
    surface dewpoint vs obs aloft because they have different vertical
    coordinates. The obs\_diag program (and other diagnostic routines)
    do not cope with the same obs type having different vertical
    coordinates because it is trying to bin observations in the vertical
    (it is unable to convert pressure to height after the fact, for
    example, or bin surface obs with a height with pressure obs).
8.  Shell scripts which used to contain MSS (mass store) commands for
    long-term archiving have been converted to call HSI (HPSS) commands.
9.  The 'wrf\_dart\_obs\_preprocess' program will now refuse to superob
    observations which are too close to the poles. If the superob radius
    includes either pole, the computation of an average obs location
    becomes more complicated than the existing code is prepared to deal
    with. (If this case is of interest to you,
    [contact](mailto:dart@ucar.edu) the DART development team. We have
    ideas on how to implement this.)
10. The default namelist values for the 'obs\_seq\_to\_netcdf' program
    has changed so the default is a single large time bin, which means
    you don't have to know the exact time extents when converting an
    obs\_seq.final file into a netCDF file. You can still set specific
    bins and get multiple netCDF files as output if you prefer.
11. The tutorial files are now directly in the DART/tutorial directory
    and no longer in separate subdirectories.
12. The default flags in the mkmf\_template.XXX files have been updated
    to be more consistent with current compiler versions.
13. The default work/input.nml namelists for Lorenz 63 and Lorenz 96
    have been changed to give good assimilation results by default.
    Originally these were set to work with a workshop tutorial in which
    the settings did not work and as part of the tutorial they were
    changed to good values. Now the workshop versions of the namelists
    are separate and copied into place by a workshop\_setup script.
14. filter now calls the end\_model() subroutine in the model\_mod for
    the first time. It should have been called all along, but was not.
15. The 'rat\_cri' namelist item has been removed from the &obs\_diag
    namelist.
16. The preprocess program has a new namelist item 'overwrite\_output'
    and it is .true. by default. The program will no longer fail if the
    target obs\_kind\_mod.f90 or obs\_def\_mod.f90 files exist but will
    silently overwrite them. Set this namelist item to .false. to
    recover the previous behavior.

[]{#NewFeatures}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

New Features
------------

-   Customizable Outlier-Threshold Handling
    -   Filter contains code to compute whether an observation should
        not be assimilated because the forward operator mean is too
        different from the observation value. This is done uniformly for
        all observation values and types. To customize this computation
        (e.g. to allow all obs of a particular type to be assimilated
        without having to pass the outlier threshold test), there is a
        new namelist item *enable\_special\_outlier\_code* in the
        &filter\_nml namelist that enables a call to a subroutine at the
        end of the filter.f90 source file. That subroutine can be
        customized by the user to do any computation required. See the
        filter namelist documentation
        [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/filter/filter.html#Namelist)
        or [local
        file](../../assimilation_code/programs/filter.html#Namelist) for
        more details.
-   Fill inflation restart files
    -   There is a new utility that will write inflation restart files
        based on values read from the console. This enables multi-step
        runs to start with the 'read inflation values from a file'
        option set to .true. for all steps instead of having to change
        the namelist after the first cycle. See the documentation
        [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/adaptive_inflate/fill_inflation_restart.html)
        or [local
        file](../../assimilation_code/programs/fill_inflation_restart.html)
        for more details.
-   New location module options
    -   There are additional options for the model and observation
        coordinate systems. Note that only a single location option can
        be chosen and all observations and all model locations must use
        that coordinate system. New options include:
        -   Channel coordinate system
        -   \[0-1\] periodic 3D coordinate system
        -   X,Y,Z 3D Cartesian coordinate system
        -   2D annulus coordinate system

        See the documentation
        [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/location/location_mod.html)
        or [local file](../../location/location_mod.html) for more
        details.
-   Missing values in state
    -   In some models there are values which are not valid in all
        ensemble members. With this release there is limited support for
        this in DART. There are still serious questions about what the
        correct results should be if the ensemble count for some state
        vector item is smaller than the total ensemble size.
        Nevertheless, with this release we have implemented support for
        missing state vector values in the CLM Land model. There is a
        new namelist item *allow\_missing\_in\_clm* in the
        &assim\_tools\_nml namelist. Setting this to .true. will allow
        DART to avoid updating any state vector items in which one or
        more of the ensemble members in CLM have a missing value.
        Inflation will be disabled for any state vector items where one
        or more ensemble members have missing values. All CLM forward
        operators must test for and be prepared to return with a failed
        forward operator code if any of the interpolation items it
        requires are missing. See the documentation
        [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/assim_tools/assim_tools_mod.html#Namelist)
        or [local
        file](../../assimilation_code/modules/assimilation/assim_tools_mod.html#Namelist)
        for more details.
-   Different task layout options
    -   The ensemble manager has a new option to distribute MPI tasks
        round robin across the available nodes instead of assigning them
        sequentially. The first N tasks, where N is the ensemble size,
        require more memory than other tasks. Distributing them
        round-robin may allow assigning more tasks per node with a more
        uniform memory usage. This may result in a small decrease in
        performance at runtime, but it might allow using fewer nodes for
        the job and thus reduce the job cost. See the documentation for
        the *layout* and *tasks\_per\_node* in the
        &ensemble\_manager\_nml namelist
        [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/assimilation_code/modules/utilities/ensemble_manager_mod.html#Namelist)
        or [local
        file](../../assimilation_code/modules/utilities/ensemble_manager_mod.html#Namelist)
        for more details.
-   Different MPI communication options
    -   The ensemble manager has 3 new options for the order in which
        the communication is done when transposing the ensemble of state
        vectors. There is a new namelist option in the
        &ensemble\_manager\_nml called *communication\_configuration*
        which can have the values 1-4. If DART is running slower than
        expected, try the various options and see which is fastest on
        your hardware. The fastest value depends on the MPI library
        implementation, the type and speed of interconnect, the
        processor speed, and node memory size and so it is almost
        impossible to recommend a value without doing timing tests on
        the target system. See the documentation in the
        &ensemble\_manager\_nml namelist
        [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/assimilation_code/modules/utilities/ensemble_manager_mod.html#Namelist)
        or [local
        file](../../assimilation_code/modules/utilities/ensemble_manager_mod.html#Namelist)
        for more details.
-   Several more places where large arrays were put on the stack have
    been removed, decreasing the total amount of stack required by DART.

[]{#NewModels}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

New Models
----------

-   CESM framework components
    -   DART now supports running CESM components CAM, POP, and CLM
        under the CESM framework. Setup scripts are provided to
        configure a single or multiple component assimilation. See:
        -   [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/models/CESM/model_mod.html)
            or [local file](../../models/CESM/model_mod.html) for
            multi-component assimilation,
        -   [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/models/cam/model_mod.html)
            or [local file](../../models/cam/model_mod.html) for CAM
            single component assimilation
        -   [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/models/POP/model_mod.html)
            or [local file](../../models/POP/model_mod.html) for POP
            single component assimilation
        -   [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/models/clm/model_mod.html)
            or [local file](../../models/clm/model_mod.html) for CLM
            single component assimilation

        Documentation for the model:
        -   the user's guide for CESM version 1.1.1:\
            <http://www.cesm.ucar.edu/models/cesm1.1/cesm/doc/usersguide/book1.html>
        -   the page that explains how to download the release code:\
            <http://www.cesm.ucar.edu/models/cesm1.1/cesm/doc/usersguide/x388.html>
        -   the web page that shows the names of the 'compsets' which
            are the configurations of the various models:\
            <http://www.cesm.ucar.edu/models/cesm1.1/cesm/doc/modelnl/compsets.html>
        -   list of recent CESM versions:\
            <http://www2.cesm.ucar.edu/models/current>
-   MPAS Atmosphere and Ocean Models
    -   DART interface documentation for the MPAS Atmosphere component:
        [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/models/mpas_atm/model_mod.html)
        or [local file](../../models/mpas_atm/model_mod.html).
    -   DART interface documentation for the MPAS Ocean component:
        [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/models/mpas_ocn/model_mod.html)
        or [local file](../../models/mpas_ocn/model_mod.html).
    -   Documentation for the model: [MPAS](http://mpas-dev.github.io/).
-   NOAH Land Model
    -   Dart interface documentation
        [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/models/noah/model_mod.html)
        or [local file](../../models/noah/model_mod.html).
    -   Documentation for the model: [The Community NOAH Land Surface
        Model
        (LSM)](http://www.ral.ucar.edu/research/land/technology/lsm.php).
-   NAAPS Aerosol Model
    -   Dart interface documentation
        [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/models/NAAPS/model_mod.html)
        or [local file](../../models/NAAPS/model_mod.html).
    -   Documentation for the model: [NRL/Monterery Aerosol
        Model](http://www.nrlmry.navy.mil/aerosol_web).
-   GITM Global Ionosphere Thermosphere Model
    -   Dart interface documentation
        [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/models/GITM/model_mod.html)
        or [local file](../../models/GITM/model_mod.html).
    -   Documentation for the model: [GITM Web
        Pages](http://ccmc.gsfc.nasa.gov/models/modelinfo.php?model=GITM).
-   NOGAPS Global Atmosphere Model
    -   Dart interface documentation
        [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/models/NOGAPS/model_mod.html)
        or [local file](../../models/NOGAPS/model_mod.html).
    -   Documentation for the model:
        [NOGAPS](http://www.srh.noaa.gov/ssd/nwpmodel/html/nogover.htm).
-   SQG Surface Quasi-Geostrophic Model
    -   Dart interface documentation
        [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/models/SQG/model_mod.html)
        or [local file](../../models/SQG/model_mod.html).
    -   Documentation for the model: [Paper on SQG
        model](http://dx.doi.org/10.1175/1520-0469(2000)057%3C2951:RONGAN%3E2.0.CO%3B2).

The *DART/models/template* directory contains sample files for adding a
new model. See [this
section](http://www.image.ucar.edu/DAReS/DART/DART_Documentation.php#adding_a_model)
of the DART web pages for more help on adding a new model.

[]{#ChangedModels}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Changed Models
--------------

-   WRF
    -   Allow advanced microphysics schemes (needed interpolation for 7
        new kinds)
    -   Interpolation in the vertical is now done in log(p) instead of
        linear pressure space. log(p) is the default, but a compile-time
        variable can restore the linear interpolation.
    -   Added support in the namelist to avoid writing updated fields
        back into the wrf netcdf files. The fields are still updated
        during the assimilation but the updated data is not written back
        to the wrfinput file during the dart\_to\_wrf step.
    -   Fixed an obscure bug in the vertical convert routine of the wrf
        model\_mod that would occasionally fail to convert an obs. This
        would make tiny differences in the output as the number of mpi
        tasks change. No quantitative differences in the results but
        they were not bitwise compatible before and they are again now.
-   CAM
    -   DART/CAM now runs under the CESM framework, so all options
        available with the framework can be used.
    -   Support for the SE core (HOMME) has been developed but is NOT
        part of this release. Please contact the [DART Development
        Group](mailto:dart@ucar.edu) if you have an interest in this
        configuration of CAM.
-   Simple Advection Model
    -   Fixed a bug where the random number generator was being used
        before being called with an initial seed.

[]{#NewFOs}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

New Observation Types/Forward Operators
---------------------------------------

-   Many new observation types related to land and atmospheric chemistry
    have been added. See the *obs\_kind/obs\_kind\_mod.f90* for a list
    of the generic kinds now available.
-   New forward operator for total precipitable water. It loops over
    model levels to compute the accumulated value. See
    [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/observations/obs_converters/tpw/tpw.html)
    or [local file](../../observations/obs_converters/tpw/tpw.html).
-   New forward operator for COSMOS ground moisture observations. See
    [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/observations/COSMOS/observations/obs_converters/COSMOS/COSMOS_to_obs.html)
    or [local
    file](../../observations/COSMOS/observations/obs_converters/COSMOS/COSMOS_to_obs.html).
-   New forward operator for MIDAS total electron count observations.
    See
    [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/observations/MIDAS/MIDAS.html)
    or [local file](../../observations/MIDAS/MIDAS.html).
-   Added example of how to set additional metadata in an observation to
    the *obs\_def\_1d\_state\_mod.f90* file.
    [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/observations/forward_operators/observations/forward_operators/obs_def_1d_state_mod.html)
    or [local
    file](../../obs_def_observations/forward_operators/obs_def_1d_state_mod.html).

[]{#NewObs}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

New Observation Types/Sources
-----------------------------

-   MADIS\
    Added a converter for wind profiler data to the set of existing
    MADIS converters. More scripting support in the MADIS obs
    converters; more error checks added to the rawin converter.
    Documentation
    [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/observations/observations/obs_converters/MADIS/MADIS.html)
    or [local
    file](../../observations/observations/obs_converters/MADIS/MADIS.html).
-   Ameriflux\
    Added an obs\_sequence converter for Ameriflux land observations of
    latent heat flux, sensible heat flux, net ecosystem production).
    Documentation
    [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/observations/obs_converters/Ameriflux/level4_to_obs.html)
    or [local
    file](../../observations/obs_converters/Ameriflux/level4_to_obs.html).
-   MODIS\
    Added an obs\_sequence converter for MODIS snow coverage
    measurements. Documentation
    [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/observations/obs_converters/snow/snow_to_obs.html)
    or [local
    file](../../observations/obs_converters/snow/snow_to_obs.html).
-   COSMOS\
    Added an obs\_sequence converter for COSMOS ground moisture
    observations. Documentation
    [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/observations/COSMOS/observations/obs_converters/COSMOS/COSMOS_to_obs.html)
    or [local
    file](../../observations/COSMOS/observations/obs_converters/COSMOS/COSMOS_to_obs.html).
-   MIDAS\
    Added an obs\_sequence converter for MIDAS observations of Total
    Electron Count. Documentation
    [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/observations/obs_converters/MIDAS/MIDAS_to_obs.html)
    or [local
    file](../../observations/obs_converters/MIDAS/MIDAS_to_obs.html).
-   GPS\
    Updated scripts for the GPS converter; added options to convert data
    from multiple satellites. Documentation
    [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/observations/observations/obs_converters/gps/gps.html)
    or [local
    file](../../observations/observations/obs_converters/gps/gps.html).
-   wrf\_dart\_obs\_preprocess\
    Added processing for wind profiler observation to the
    wrf\_dart\_obs\_preprocess program. Documentation
    [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/models/wrf/WRF_DART_utilities/wrf_dart_obs_preprocess.html)
    or [local
    file](../../models/wrf/WRF_DART_utilities/wrf_dart_obs_preprocess.html).
-   AIRS\
    Fix BUG in AIRS converter - the humidity obs are accumulated across
    the layers and so the best location for them is the layer midpoint
    and not on the edges (levels) as the temperature obs are. Also fixed
    off-by-one error where the converter would make one more obs above
    the requested top level. Documentation
    [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/observations/observations/obs_converters/AIRS/AIRS.html)
    or [local
    file](../../observations/observations/obs_converters/AIRS/AIRS.html).
-   GTS\
    Made gts\_to\_dart converter create separate obs types for surface
    dewpoint vs obs aloft because they have different vertical
    coordinates. Documentation
    [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/observations/observations/obs_converters/GTSPP/GTSPP.html)
    or [local
    file](../../observations/observations/obs_converters/GTSPP/GTSPP.html).
-   Tape Archive scripts\
    Converted mss commands to hpss commands for a couple observation
    converter shell scripts (inc AIRS).
-   Evenly distributed obs\
    New matlab code to generate evenly spaced observations on the
    surface of a sphere (e.g. the globe). Documentation
    [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/observations/even_sphere/README)
    or [local file](../../observations/even_sphere/README).
-   observation utilities\
    Added obs\_loop.f90 example file in obs\_sequence directory; example
    template for how to construct special purpose obs\_sequence tools.
    Documentation (source)
    [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/obs_sequence/obs_loop.f90)
    or [local file](../../obs_sequence/obs_loop.f90).
-   PREPBUFR\
    Change the default in the script for the prepbufr converter so it
    will swap bytes, since all machines except ibms will need this now.
    Documentation
    [Website](https://svn-dares-dart.cgd.ucar.edu/DART/trunk/observations/observations/obs_converters/NCEP/prep_bufr/prep_bufr.html)
    or [local
    file](../../observations/observations/obs_converters/NCEP/prep_bufr/prep_bufr.html).

[]{#NewDiagnostics}

New Diagnostics and Documentation
---------------------------------

**Better Web Pages.** We've put a lot of effort into expanding our
documentation. For example, please check out [the Matlab diagnostics
section](http://www.image.ucar.edu/DAReS/DART/DART_Documentation.php#mat_obs)
or the pages outlining the [observation sequence file
contents](http://www.image.ucar.edu/DAReS/DART/DART_Observations.php#obs_seq_overview).\
\
But there's always more to add. **Please [let us
know](http://www2.image.ucar.edu/forms/dart-suggestion-box) where we are
lacking.**

Other new stuff:

-   Handle empty epochs in the obs\_seq\_to\_netcdf converter.
-   Added a matlab utility to show the output of a 'hop' test (running a
    model for a continuous period vs. stopping and restarting a run).
-   Improved the routine that computes axes tick values in plots with
    multiple values plotted on the same plot.
-   The obs\_common\_subset program can select common observations from
    up to 4 observation sequence files at a time.
-   Add code in obs\_seq\_verify to ensure that the ensemble members are
    in the same order in all netcdf files.
-   Added support for the unstructured grids of mpas to our matlab
    diagnostics.
-   Fix to writing of ReportTime in obs\_seq\_coverage.
-   Fixed logic in obs\_seq\_verify when determining the forecast lat.
-   Fixed loops inside obs\_seq\_coverage which were using the wrong
    limits on the loops. Fixed writing of 'ntimes' in output netcdf
    variable.
-   Rewrote the algorithm in the obs\_selection tool so it had better
    scaling with large numbers of obs.
-   Several improvements to the 'obs\_diag' program:
    -   Added preliminary support for a list of 'trusted obs' in the
        obs\_diag program.
    -   Can disable the rank histogram generation with a namelist item.
    -   Can define height\_edges or heights in the namelist, but not
        both.
    -   The 'rat\_cri' namelist item (critical ratio) has been
        deprecated.
-   Extend obs\_seq\_verify so it can be used for forecasts from a
    single member. minor changes to obs\_selection, obs\_seq\_coverage
    and obs\_seq\_verify to support a single member.
-   Added Matlab script to read/print timestamps from binary dart
    restart/ic files.
-   Default for obs\_seq\_to\_netcdf in all the namelists is now 'one
    big time bin' so you don't have to know the exact timespan of an
    obs\_seq.final file before converting to netCDF.

[]{#NewUtilities}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

New Utilities
-------------

This section describes updates and changes to the tutorial materials,
scripting, setup, and build information since the Kodiak release.

-   The mkmf-generated Makefiles now take care of calling 'fixsystem' if
    needed so the mpi utilities code compiles without further user
    intervention for any compiler.
-   Make the default input.nml for the Lorenz 96 and Lorenz 63 model
    gives good assimilation results. Rename the original input.nml to
    input.workshop.nml. The workshop\_setup script renames it back
    before doing anything else so this won't break the workshop
    instructions. Simplify all the workshop\_setup.csh scripts to do the
    minimal work needed by the DART tutorial.
-   Updates to the models/template directory with the start of a full 3d
    geophysical model template. Still under construction.
-   Move the pdf files in the tutorial directory up a level. Removed
    framemaker source files because we no longer have access to a
    working version of the Framemaker software. Moved routines that
    generate figures and diagrams to a non-distributed directory of the
    subversion repository.
-   Enable netCDF large file support in the work/input.nml for models
    which are likely to have large state vectors.
-   Minor updates to the doc.css file, make pages look identical in the
    safari and firefox browsers.
-   Added a utility that sorts and reformats namelists, culls all
    comments to the bottom of the file. Useful for doing diffs and
    finding duplicated namelists in a file.
-   Cleaned up mkmf files - removed files for obsolete platforms and
    compilers, updated suggested default flags for intel.
-   Update the mkmf template for gfortran to allow fortran source lines
    longer than 132 characters.

[]{#KnownProblems}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Known Problems
--------------

-   none.

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

