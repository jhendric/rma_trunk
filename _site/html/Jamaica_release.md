[]{#TOP}

DART "Jamaica release" Documentation
====================================

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
the combination of assimilation algorithms, models, and **real** (as
well as synthetic) observations to allow increased understanding of all
three. The DART programs have been compiled with several (many?) Fortran
90 compilers and run on linux compute-servers, linux clusters, OSX
laptops/desktops, SGI Altix clusters, supercomputers running AIX ... a
pretty broad range, really. You should definitely read the
[Customizations](#customizations) section.\
\
DART employs a modular programming approach to apply an Ensemble Kalman
Filter which nudges models toward a state that is more consistent with
information from a set of observations. Models may be swapped in and
out, as can different algorithms in the Ensemble Kalman Filter. The
method requires running multiple instances of a model to generate an
ensemble of states. A forward operator appropriate for the type of
observation being used is applied to each of the states to generate the
model's estimate of the observation. Comparing these estimates and their
uncertainty to the observation and its uncertainty ultimately results in
the adjustments to the model states. There's much more to it, described
in detail in the tutorial directory of the package.\
\
DART ultimately creates a few netCDF files containing the model states
just before the adjustment (*Prior\_Diag.nc*) and just after the
adjustment (*Posterior\_Diag.nc*) as well as a file *obs\_seq.final*
with the model estimates of the observations. There is a suite of
Matlab® functions that facilitate exploration of the results, but the
netCDF files are inherently portable and contain all the necessary
metadata to interpret the contents.

What's New {#whats-new .indent1}
----------

[download "jamaica" -- the latest release](/DAReS/DART/DART_download)\
\
The jamaica release has been tested on several supercomputers, including
the NCAR IBM machines 'bluevista' and 'blueice', the SGI Altix machines
'columbia' and 'origin'. There are **many** more changes - much more
thoroughly described - in the
[documentation/html/history/Jamaica\_diffs\_from\_I.html](documentation/html/history/Jamaica_diffs_from_I.html)
document. What follows here is only meant to draw attention to the fact
that experienced users should really read the change document.\
\
The most visible changes in the **jamaica** release are:

#### Changes in the inflation strategies. {#changes-in-the-inflation-strategies. .indent1}

The changes in the inflation mechanism now provide the ability to
perform inflation in observation-space, fixed-state-space, or
spatially-varying-state-space in the prior and/or posterior with either
a deterministic or stochastic algorithm.

#### Fundamental changes for parallel behavior {#fundamental-changes-for-parallel-behavior .indent1}

The **optional** use of MPI required a complete rewrite of many of the
routines that previously governed attempts at parallel computations.
This resulted in the removal of the *async = 3* method of achieving
parallel assimilations and model advances. As a consequence,
*assim\_region.csh*, *advance\_ens.csh*, and *filter\_server.csh* no
longer exist.\
\
Some assimilation strategies previously controlled by
*assim\_tools\_nml*, (namely, whether to perform a parallel
assimilation, how many domains to use, and a few others) have been
replaced by a new parallel assimilation algorithm. Associated inflation
computations are documented in the manuscript by Anderson and Collins
that is to appear in the Journal of Atmospheric and Oceanic Technology
and in section 22 of the tutorial. Namelist entries *do\_parallel,
num\_domains*, and *parallel\_command* no longer exist.\
\
The (optional) MPI implementation allows for very large state vectors -
some model configurations (i.e. state vectors too large to fit in core)
are now possible. And **yes**, it scales rather nicely, thank you. There
are references throughout the documents and code to MPI - keep in mind
all the MPI interfaces are in one module *mpi\_utilities\_mod.f90*. If
you are NOT using MPI - there is a simple equivalent module named
*null\_mpi\_utilities\_mod.f90* which is the default.\
\
Using MPI greatly increases the performance for large models and/or
large numbers of observations. It also greatly **decreases** the shell
script complexity. The use of MPI is enabled by giving *mkmf\_filter* an
optional argument *-mpi* . To learn more about DART's implementation of
MPI - please refer to
[DART/documentation/html/mpi\_intro.html](documentation/html/mpi_intro.html) .

#### Vertical localization {#vertical-localization .indent1}

It is now possible to perform localization in the vertical in 3D models,
which necessitated a change in the *model\_mod* interface. The
*model\_mod:get\_close\_states()* and
*model\_mod:get\_num\_close\_states()* routines have been replaced by
"pass-through" interfaces to routines in *location\_mod*. An optional
interface *model\_mod:ens\_mean\_for\_model()* is required only by some
models when performing localization in the vertical. This routine
provides the ensemble mean so that the same model state is used when
computing the vertical localization.

#### 3D plots of observation locations & type {#d-plots-of-observation-locations-type .indent1}

It's more than just eye candy. It is also possible to plot (in 3D) the
locations of observations by processing an observation sequence through
*obs\_diag* and running the resulting output file through the matlab
function *plot\_observation\_locations*.

What's Required {#whats-required .indent1}
---------------

1.  a working fortran compiler
2.  a working F90 interface to the netCDF libraries
3.  something that can run 'csh'

DART has been tested on many Fortran compilers and platforms. Our
philosophy seems to be to burn away the 'impurities' by compiling on any
architecture we can get our hands on. We completely buy into the use of
the Fortran 'kind' strategy to declare variable types rather than
relying on compiler autopromotion flags - which are a BAD idea in my
opinion. There are some models in DART that come from outside developers
and \_may\_ contain code segments that require the use of some special
compiler flags - we are not so draconian as to mandate the use of the
Fortran 'kind'. In general, the low-order models and all common portions
of the DART code compile very cleanly.\
\
DART uses the [netCDF](http://www.unidata.ucar.edu/packages/netcdf/)
self-describing data format with a particular metadata convention to
describe output that is used to analyze the results of assimilation
experiments. These files have the extension *.nc* and can be read by a
number of standard data analysis tools.\
\
Since most (all?) of the models envisioned being used by DART are
written in Fortran and run on various UNIX or \*nix platforms, the
development environment for DART is highly skewed to these machines. We
do most of our development on a small linux workstation and a mac laptop
running OSX 10.4.9 - but have an extensive test network. I've never
built nor run DART on a Windows machine - so I don't even know if it's
possible. If you have run it (under Cygwin?) please let me know how it
went -- I'm curious.

### What's nice to have {#whats-nice-to-have .indent1}

<div class="indent1">

**ncview**: DART users have used
[ncview](http://meteora.ucsd.edu/~pierce/ncview_home_page.html) to
create graphical displays of output data fields. The 2D rendering is
good for 'quick-look' type uses, but I wouldn't want to publish with it.

</div>

\

<div class="indent1">

**NCO**: The [NCO](http://nco.sourceforge.net) tools are able to perform
operations on netCDF files like concatenating, slicing, and dicing.

</div>

\

<div class="indent1">

**Matlab**® A set of [Matlab®](http://www.mathworks.com/) scripts,
designed to produce graphical diagnostics from DART netCDF output files
are also part of the DART project.

</div>

\

<div class="indent1">

**MPI**: The latest release of the DART system includes an MPI option.
MPI stands for 'Message Passing Interface', and is both a library and
run-time system that enables multiple copies of a single program to run
in parallel, exchange data, and combine to solve a problem more quickly.
The latest release of DART does \*NOT\* require MPI to run; the default
build scripts do not need nor use MPI in any way. However, for larger
models with large state vectors and large numbers of observations, the
data assimilation step will run much faster in parallel, which requires
MPI to be installed and used. However, if multiple ensembles of your
model fit comfortably (in time and memory space) on a single processor,
you need read no further about MPI.

</div>

Where's the (GUI) interface? {#wheres-the-gui-interface .indent1}
----------------------------

DART programs can require three different types of input. First, some of
the DART programs, those for creating synthetic observational datasets,
require interactive input from the keyboard. For simple cases, this
interactive input can be made directly from the keyboard. In more
complicated cases, a file containing the appropriate keyboard input can
be created and this file can be directed to the standard input of the
DART program. Second, many DART programs expect one or more input files
in DART specific formats to be available. For instance,
*perfect\_model\_obs*, which creates a synthetic observation set given a
particular model and a description of a sequence of observations,
requires an input file that describes this observation sequence. At
present, the observation files for DART are in a custom format in either
human-readable ascii or more compact machine-specific binary. Third,
many DART modules (including main programs) make use of the Fortan90
namelist facility to obtain values of certain parameters at run-time.
All programs look for a namelist input file called *input.nml* in the
directory in which the program is executed. The *input.nml* file can
contain a sequence of individual Fortran90 namelists which specify
values of particular parameters for modules that compose the executable
program. DART provides a mechanism that automatically generates
namelists with the default values for each program to be run.

Document conventions {#document-conventions .indent1}
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

------------------------------------------------------------------------

Installation
============

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

### You should *absolutely* take the tutorial in the *DART/tutorial* directory. {#you-should-absolutely-take-the-tutorial-in-the-darttutorial-directory. .indent1}

[]{#compilers}

------------------------------------------------------------------------

Requirements: an F90 Compiler
-----------------------------

The DART software has been successfully built on several Linux/x86
platforms with several versions of the [Intel Fortran Compiler for
Linux](http://www.intel.com/software/products/compilers/flin), which (at
one point) is/was free for individual scientific use. It has also been
built and successfully run with several versions of each of the
following: [Portland Group Fortran Compiler](http://www.pgroup.com),
[Lahey Fortran Compiler](http://www.lahey.com), [Pathscale Fortran
Compiler](http://www.pathscale.com), [GNU Fortran 95 Compiler
("gfortran")](http://gcc.gnu.org/fortran), [Absoft Fortran 90/95
Compiler (Mac OSX)](http://www.absoft.com). Since recompiling the code
is a necessity to experiment with different models, there are no
binaries to distribute.

[]{#netCDFlib}

------------------------------------------------------------------------

Requirements: the *netCDF* library
----------------------------------

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
netCDF home page: <http://www.unidata.ucar.edu/packages/netcdf/> Pay
particular attention to the compiler-specific patches that must be
applied for the Intel Fortran Compiler. (Or the PG compiler, for that
matter.)

The location of the netCDF library, *libnetcdf.a*, and the locations of
both *netcdf.mod* and *typesizes.mod* will be needed by the makefile
template, as described in the [compiling](#compiling) section.

[]{#download}

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
<http://www.image.ucar.edu/~thoar/svn_primer.html>\
\
The resources to develop and support DART come from our ability to
demonstrate our growing user base. We ask that you register at our
download site <http://www.image.ucar.edu/DAReS/DART/DART_download> and
promise that the information will only be used to notify you of new DART
releases and shown to our sponsers in an aggregated form: "Look - we
have three users from Tonawanda, NY". After filling in the form, you
will be directed to a website that has instructions on how to download
the code.\
\
svn has adopted the strategy that "disk is cheap". In addition to
downloading the code, it downloads an additional copy of the code to
store locally (in hidden .svn directories) as well as some
administration files. This allows svn to perform some commands even when
the repository is not available. It does double the size of the code
tree ... so the download is something like 480MB -- pretty big. BUT -
all future updates are (usually) just the differences, so they happen
very quickly.\
\
If you follow the instructions on the download site, you should wind up
with a directory named *DART*. Compiling the code in this tree (as is
usually the case) will necessitate much more space.\
\
If you cannot use svn, just let me know and I will create a tar file for
you. svn is so superior to a tar file that a tar file should be
considered a last resort.\
\
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

[]{#path_names}

### Customizing the 'path\_names\_\*' file {#customizing-the-path_names_-file .indent1}

Several *path\_names\_\** files are provided in the *work* directory for
each specific model, in this case: *DART/models/lorenz\_63/work*. Since
each model comes with its own set of files, the *path\_names\_\** files
need no customization.

[]{#tutorial}

### The Tutorial. {#the-tutorial. .indent1}

The *DART/tutorial* documents are an excellent way to kick the tires on
DART and learn about ensemble data assimilation. If you have correctly
configured your *mkmf.template*, you can run anything in the tutorial.

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

    Posterior_Diag.nc
    Prior_Diag.nc
    True_State.nc
    filter_ics
    filter_restart
    input.nml
    mkmf_create_fixed_network_seq
    mkmf_create_obs_sequence
    mkmf_filter
    mkmf_merge_obs_seq
    mkmf_obs_diag
    mkmf_perfect_model_obs
    mkmf_preprocess
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
    path_names_merge_obs_seq
    path_names_obs_diag
    path_names_perfect_model_obs
    path_names_preprocess
    path_names_wakeup_filter
    perfect_ics
    perfect_restart
    set_def.out
    workshop_setup.csh

There are eight *mkmf\_xxxxxx* files for the programs

1.  *preprocess*,
2.  *create\_obs\_sequence*,
3.  *create\_fixed\_network\_seq*,
4.  *perfect\_model\_obs*,
5.  *filter*,
6.  *wakeup\_filter*,
7.  *merge\_obs\_seq*, and
8.  *obs\_diag*,

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
directories, for example). The only function of the *mkmf\_xxxxxx*
script is to generate a *Makefile* and an
*input.nml.**program*\_default** file. It is not supposed to compile
anything -- *make* does that:

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
preprocessor. It is just a good habit to develop.

<div class="unix">

\\rm -f ../../../obs\_def/obs\_def\_mod.f90\
\\rm -f ../../../obs\_kind/obs\_kind\_mod.f90\
./preprocess\
ls -l ../../../obs\_def/obs\_def\_mod.f90\
ls -l ../../../obs\_kind/obs\_kind\_mod.f90\

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
  ------------------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *preprocess*                    creates custom source code for just the observations of interest
  *create\_obs\_sequence*         specify a (set) of observation characteristics taken by a particular (set of) instruments
  *create\_fixed\_network\_seq*   specify the temporal attributes of the observation sets
  *perfect\_model\_obs*           spinup, generate "true state" for synthetic observation experiments, ...
  *filter*                        perform experiments
  *obs\_diag*                     creates observation-space diagnostic files to be explored by the Matlab® scripts.
  *merge\_obs\_sequence*          manipulates observation sequence files. It is not generally needed (particularly for low-order models) but can be used to combine observation sequences or convert from ASCII to binary or vice-versa. Since this is a specialty routine - we will not cover its use in this document.
  *wakeup\_filter*                is only needed for MPI applications. We're starting at the beginning here, so we're going to ignore this one, too.

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

#### 1.3 Initialize the model onto the attractor.

*perfect\_model\_obs* can now advance the arbitrary initial state for
24,000 timesteps to move it onto the attractor. *perfect\_model\_obs*
uses the Fortran90 namelist input mechanism instead of (admittedly gory,
but temporary) interactive input. All of the DART software expects the
namelists to found in a file called *input.nml*. When you built the
executable, an example namelist was created
*input.nml.perfect\_model\_obs\_default* that contains all of the
namelist input for the executable. If you followed the example, each
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

perfect\_model\_obs

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
perfect\_model\_obs

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

------------------------------------------------------------------------

The Tutorial.
-------------

The *DART/tutorial* documents are an excellent way to kick the tires on
DART and learn about ensemble data assimilation. If you have gotten this
far, you can run anything in the tutorial.

[]{#matlab}

------------------------------------------------------------------------

Matlab® Diagnostics
-------------------

The output files are netCDF files, and may be examined with many
different software packages. We happen to use Matlab®, and provide our
diagnostic scripts in the hopes that they are useful.

The diagnostic scripts and underlying functions reside in two places:
*DART/diagnostics/matlab* and *DART/matlab*. They are reliant on the
public-domain [netcdf
toolbox](http://woodshole.er.usgs.gov/staffpages/cdenham/public_html/MexCDF/nc4ml5.html)
from
*http://woodshole.er.usgs.gov/staffpages/cdenham/public\_html/MexCDF/nc4ml5.html*
as well as the public-domain [CSIRO matlab/netCDF
interface](http://www.marine.csiro.au/sw/matlab-netcdf.html) from
*http://www.marine.csiro.au/sw/matlab-netcdf.html*. If you do not have
them installed on your system and want to use Matlab to peruse netCDF,
you must follow their installation instructions. The 'interested reader'
may want to look at the *DART/matlab/startup.m* file I use on my system.
If you put it in your *\$HOME/matlab* directory, it is invoked every
time you start up Matlab.

Once you can access the *getnc* function from within Matlab, you can use
our diagnostic scripts. It is necessary to prepend the location of the
*DART/matlab* scripts to the *matlabpath*. Keep in mind the location of
the netcdf operators on your system WILL be different from ours ... and
that's OK.\
\

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


