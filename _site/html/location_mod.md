[]{#TOP}

MODULE location\_mod (threed\_sphere)
=====================================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[USAGE](#Usage%20Notes) / [NAMELIST](#Namelist) /
[DISCUSSION](#Discussion) / [INTERFACES](#Interface) /
[FILES](#FilesUsed) / [REFERENCES](#References) / [ERRORS](#Errors) /
[PLANS](#FuturePlans) / [PRIVATE COMPONENTS](#PrivateComponents) /
[TERMS OF USE](#Legalese)

Overview
--------

The DART framework needs to be able to compute distances between
locations, to pass location information to and from the model interface
code (model\_mod.f90), and to be able to read and write location
information to files. DART isolates all this location information into
separate modules so that the main algorithms can operate with the same
code independent of whether the model uses latitude/longitude/height, 1D
unit sphere coordinates, cylindrical coordinates, etc. DART provides
about half a dozen possible coordinate systems, and others can be added.
The most common one for geophysical models is this one: threed\_sphere.

This location module provides a representation of a physical location on
a 3-D spherical shell, using latitude and longitude plus a vertical
component with choices of vertical coordinate type such as pressure or
height in meters. A type that abstracts the location is provided along
with operators to set, get, read, write, and compute great circle
distances between locations. This is a member of a class of similar
location modules that provide the same abstraction for different
represenations of physical space.

[]{#Usage Notes}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Usage
-----

The location routines are general purpose code that can be used for a
variety of utilities. The following discussion is specifically
restricted to how the location namelist settings affect the execution of
the *filter* assimilation program.

Issues related to changing the results of an assimilation based on the
location module settings:\

-   Whether and how to treat the vertical separation when computing
    distances between two locations.
-   Whether to use different distances in the vertical for different
    observation types.

Issues related to changing the results of an assimilation based on code
in the model-specific *model\_mod.f90* module:\

-   Whether the model-specific code needs to convert vertical
    coordinates.
-   Whether the model-specific code alters the distances in some other
    way.

Issues related to the speed/efficiency of an assimilation based on the
location module settings:\

-   Whether to use a faster but less precise distance computation.
-   Whether to change the number of internal bins used to more quickly
    find nearby locations.

#### Vertical Issues

The localization distance during an assimilation -- the maximum
separation between an observation and a state vector item potentially
affected by the assimilation -- is set in the
[&assim\_tools\_nml](../../modules/assimilation/assim_tools_mod.html)
namelist (the *cutoff* item).

Setting *horiz\_dist\_only = .TRUE.* in the namelist means the great
circle distances will be computed using only the latitude and longitudes
of the two locations, ignoring the vertical components of the locations.
The cutoff is specified in radians to be independent of the radius of
the sphere. For the Earth the radius is nominally 6,371 Km. To compute
the horizontal only localization distance, multiply 6,371 Km by the
cutoff to get the distance in Km. The cutoff is by definition 1/2 the
distance to where the increments go to 0, so multiply that result by 2
to get the maximum distance at which an observation can alter the state.

Setting *horiz\_dist\_only = .FALSE.* in the namelist means the code
will compute a 3D distance, including the vertical separation. In this
case, the *vert\_normalization\_xxx* namelist values will be used to
convert from pressure, height, model level, or scale heights into
radians so the distances are computed in a consistent unit system. In
practice, multiply the cutoff by the normalization factor (and then
again by 2) to get the maximum vertical separation in each of the given
units.

When including vertical separation the potential area of influence of an
assimilated observation is an ellipsoid with the observation at the
center. The horizontal radius is defined by the cutoff and the vertical
radius is defined by the normalization factors.

See [examples below](#Example) for specific examples that highlight some
vertical localization issues.

#### Different vertical factors per observation type

Generally a single cutoff value and a single set of normalization
factors are sufficient for most assimilations. The localization
distances define the maximum range of impact of an observation, but
there still must be a positive or negative correlation between the state
ensemble and the forward operator/expected obs ensemble for the values
to change.

However, the
[&assim\_tools\_nml](../../modules/assimilation/assim_tools_mod.html)
namelist includes the option to set a different cutoff on a
per-observation-type basis. There are corresponding items in the
location module namelist to similiarly control the vertical distance
contribution on a per-observation, per-vertical-type basis.

#### Model-dependent Vertical Conversion Issues

If the model supports either a different vertical coordinate than the
vertical coordinate of the observations, or if the user wants to
localize in a different vertical coordinate than the observations or
state vector items, the model-specific *model\_mod.f90* code will have
to provide a conversion between different vertical coordinates. This
cannot be done by the location module since most vertical conversions
require additional model-specific information such as temperature,
moisture, depth, surface elevation, model levels, etc.

Once the locations have the same vertical units the location module code
can compute the distances between them. It is an error to ask for the
distance between two locations with different vertical coordinates
unless you have set the namelist to horizontal distances only.

There is a vertical type of VERTISUNDEF (Vertical is Undefined). This is
used to describe observations where there is no specific single vertical
location, for example the position of a hurricane or a column integrated
quantity. In this case the location code computes only horizontal
distances between any pair of observations in which one or both have an
undefined vertical location.

#### Model-dependent Distance Adjustments

The calls to routines that collect the distances between locations for
the assimilation code pass through the model-specific *model\_mod.f90*
code. This allows the code to alter the actual distances to either
increase or decrease the effect of an observation on the state or on
other observations.

For example, if the top of a model is externally constrained then
modifications by the assimilation code may lead to bad results. The
model-specific code can compute the actual distances between two
locations and then increase it artificially as you reach the top of the
model, so observations have smaller and smaller impacts. For ocean
models, the distances to points on land can be set to a very large value
and those points will be unaffected by the assimilation.

#### Approximate distances

For regional models this should usually be *.FALSE.* in the namelist.

For global models this is usually set to *.TRUE.* which allows the code
to run slightly faster by precomputing tables of sines, cosines, and arc
cosines used in the distance computations. Values are linearly
interpolated between entries in the table which leads to minor roundoff
errors. These are negligible in a global model but may be significant in
models that over a small region of the globe.

#### Internal bin counts

The default settings for *nlon* and *nlat* are usually sufficient.
However if this is a high resolution model with a large state vector the
assimilation may run faster by doubling these values or multiplying them
by 4. (The *nlon* item must be odd; compute the value and subtract 1.)
These values set the number of internal bins used inside the code to
pre-sort locations and make it faster to retrieve all locations close to
another location. A larger bin count uses more memory but shortens the
linear part of the location search.

[]{#Example}

#### Examples and Questions involving vertical issues

##### Example of specifying a cutoff based on a distance in kilometers

The Earth radius is nominally 6,371 Km. If you want the maximum
horizontal distance that an observation can possibly influence something
in the model state to be X km, then set the cutoff to be (X / 6,371) /
2. Remember the actual impact will depend on a combination of this
distance and the regression coefficient computed from the distribution
of forward operator values and the ensemble of values in the model
state.

##### Cutoff and half-widths

Q: Why is the cutoff specified as half the distance to where the impact
goes to 0, and why is it called 'cutoff'?\
A: Because the original paper by Gaspari & Cohn used that definition in
this paper which our localization function is based on.\
Gaspari, G. and Cohn, S. E. (1999), Construction of correlation
functions in two and three dimensions. Q.J.R. Meteorol. Soc., 125:
723-757. <doi:10.1002/qj.49712555417>

##### Computing vertical normalization values

Because distances are computed in radians, the vertical distances have
to be translated to radians. To get a maximum vertical separation of X
meters (if localizing in height), specify the
vert\_normalization\_height of X / cutoff. If localizing in pressure,
specify vert\_normalization\_pressure as X pascals / cutoff, etc.

##### Single vertical coordinate type

Vertical distances can only be computed between two locations that have
the same vertical type. In practice this means if vertical localization
is enabled all observations which have a vertical location need to be
converted to a single vertical coordinate type, which matches the
desired localization unit. The model state must also be able to be
converted to the same vertical coordinate type.

For example, if some observations come with a vertical coordinate type
of pressure and some with height, and you want to localize in height,
the pressure coordinates need to be converted to an equivalant height.
This usually requires information only available to the model interface
code in the model\_mod.f90 file, so a convert\_vertical\_obs() routine
is called to do the conversion.

The locations of the model state are returned by the
get\_state\_meta\_data() routine in the model\_mod.f90 file. If the
vertical coordinate used in the state is not the same as the desired
vertical localization type, they must also be converted using a
convert\_vertical\_state() routine.

\
\
[]{#Namelist}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Namelist
--------

This namelist is read from the file *input.nml*. Namelists start with an
ampersand '&' and terminate with a slash '/'. Character strings that
contain a '/' must be enclosed in quotes to prevent them from
prematurely terminating the namelist.

<div class="namelist">

    &location_nml
        horiz_dist_only                          = .true.
        vert_normalization_pressure              = 100000.0
        vert_normalization_height                = 10000.0
        vert_normalization_level                 = 20.0
        vert_normalization_scale_height          = 5.0
        approximate_distance                     = .false.
        nlon                                     = 71
        nlat                                     = 36
        output_box_info                          = .false.
        print_box_level                          = 0
        special_vert_normalization_obs_types     = 'null'
        special_vert_normalization_pressures     = -888888.0
        special_vert_normalization_heights       = -888888.0
        special_vert_normalization_levels        = -888888.0
        special_vert_normalization_scale_heights = -888888.0
      /

</div>

\
\

Items in this namelist either control the way in which distances are
computed and/or influence the code performance.

<div>

  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  Item                                          Type                                Description
  --------------------------------------------- ----------------------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  horiz\_dist\_only                             logical                             If .TRUE. compute great-circle distance using the horizontal distance component only. If .FALSE. compute distances by including the vertical and horizontal separation. All distances are computed in radians; the corresponding vertical normalization factors are used to compute the vertical distance.\
                                                                                    The vertical coordinate system must be the same for both locations in order to compute a distance. However, if either location is VERTISUNDEF, or both are VERTISSURFACE, only a horizontal distance is computed. For any other combination of vertical coordinate systems this routine will fail because it cannot convert between vertical coordinate systems without model-specific information. The model\_mod interface code may supply a get\_close\_obs() routine to intercept and convert the vertical coordinates before calling this get\_close\_obs() routine.

  vert\_normalization\_pressure                 real(r8)                            The number of pascals equivalent to a horizontal distance of one radian.

  vert\_normalization\_height                   real(r8)                            The number of meters equivalent to a horizontal distance of one radian.

  vert\_normalization\_level                    real(r8)                            The number of model levels equivalent to a horizontal distance of one radian.

  vert\_normalization\_scale\_height            real(r8)                            The number of scale heights equivalent to a horizontal distance of one radian.

  approximate\_distance                         logical                             If true, uses a table lookup for fast approximate computation of distances on sphere. Distance computation can be a first order cost for some spherical problems so this can increase speed significantly at a loss of some precision. WARNING: This should be set to .FALSE. if you need to compute small distances accurately or you have a regional model.

  nlon                                          integer                             Used internally by the search code to speed the search for nearby locations. Number of boxes (bins) created in the longitude direction. Must be an odd number. (See discussion above for more information about this item.)

  nlat                                          integer                             Used internally by the search code to speed the search for nearby locations. Number of boxes (bins) created in the latitude direction. (See discussion above for more information about this item.)

  output\_box\_info                             logical                             If true, print details about the distribution of locations across the array of boxes. *print\_box\_level* controls how much detail is printed.

  print\_box\_level                             integer                             If *output\_box\_info = .true.*, *print\_box\_level* controls how much detail is printed. 0 = no detail. 1,2,3 are progressively more and more detail.

  special\_vert\_normalization\_obs\_types      character(len=32), dimension(500)   If specified, must be a string array of observation specific types (e.g. RADIOSONDE\_TEMPERATURE, AIRCRAFT\_TEMPERATURE, etc). For each type listed here a vertical normalization value must be given which overrides the default vertical normalization values. Even if only one is going to be used, all 4 normalization values must be specified for each special type.

  special\_vert\_normalization\_pressure        real(r8), dimension(500)            The number of pascals equivalent to a horizontal distance of one radian, one value for each special observation type listed in the 'special\_vert\_normalization\_obs\_types' list.

  special\_vert\_normalization\_height          real(r8), dimension(500)            The number of geopotential meters equivalent to a horizontal distance of one radian, one value for each special observation type listed in the 'special\_vert\_normalization\_obs\_types' list.

  special\_vert\_normalization\_scale\_height   real(r8), dimension(500)            The number of scale heights equivalent to a horizontal distance of one radian, one value for each special observation type listed in the 'special\_vert\_normalization\_obs\_types' list.

  special\_vert\_normalization\_level           real(r8), dimension(500)            The number of model levels equivalent to a horizontal distance of one radian, one value for each special observation type listed in the 'special\_vert\_normalization\_obs\_types' list.
  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
\
[]{#Discussion}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Discussion
----------

#### Location-independent code

All types of location modules define the same module name
*location\_mod*. Therefore, the DART framework and any user code should
include a Fortran 90 *use* statement of *location\_mod*. The selection
of which location module will be compiled into the program is controlled
by which source file name is specified in the *path\_names\_xxx* file,
which is used by the *mkmf\_xxx* scripts.

All types of location modules define the same Fortran 90 derived type
*location\_type*. Programs that need to pass location information to
subroutines but do not need to interpret the contents can declare,
receive, and pass this derived type around in their code independent of
which location module is specified at compile time. Model and
location-independent utilities should be written in this way. However,
as soon as the contents of the location type needs to be accessed by
user code then it becomes dependent on the exact type of location module
that it is compiled with.

#### Usage of distance routines

[]{#Distance}

Regardless of the fact that the distance subroutine names include the
string 'obs', there is nothing specific to observations in these
routines. They work to compute distances between any set of locations.
The most frequent use of these routines in the filter code is to compute
the distance between a single observation and items in the state vector,
and also between a single observation and other nearby observations.
However, any source for locations is supported.

In simpler location modules (like the *oned* version) there is no need
for anything other than a brute force search between the base location
and all available state vector locations. However in the case of large
geophysical models which typically use the *threed\_sphere* locations
code, the brute-force search time is prohibitive. The location code
pre-processes all locations into a set of *bins* and then only needs to
search the lists of locations in nearby bins when looking for locations
that are within a specified distance.

The expected calling sequence of the *get\_close* routines is as
follows:


    call get_close_maxdist_init()  ! is called before get_close_obs_init()
    call get_close_obs_init()
    ...
    call get_close_obs()           ! called many, many times
    ...
    call get_close_obs_destroy()

In the *threed\_sphere* implementation the first routine initializes
some data structures, the second one bins up the list of locations, and
then the third one is called multiple times to find all locations within
a given radius of some reference location, and to optionally compute the
exact separation distance from the reference location. The last routine
deallocates the space. See the documentation below for the specific
details for each routine.

All 4 of these routines must be present in every location module but in
most other versions all but *get\_close\_obs()* are stubs. In this
*threed\_sphere* version of the locations module all are fully
implemented.

#### Interaction with model\_mod.f90 code

[]{#ModelMod}

The filter and other DART programs could call the *get\_close* routines
directly, but typically do not. They declare them (in a *use* statement)
to be in the *model\_mod* module, and all model interface modules are
required to supply them. However in many cases the model\_mod only needs
to contain another *use* statement declaring them to come from the
*location\_mod* module. Thus they 'pass through' the model\_mod but the
user does not need to provide a subroutine or any code for them.

However, if the model interface code wants to intercept and alter the
default behavior of the get\_close routines, it is able to. Typically
the model\_mod still calls the location\_mod routines and then adjusts
the results before passing them back to the calling code. To do that,
the model\_mod must be able to call the routines in the location\_mod
which have the same names as the subroutines it is providing. To allow
the compiler to distinguish which routine is to be called where, we use
the Fortran 90 feature which allows a module routine to be renamed in
the use statement. For example, a common case is for the model\_mod to
want to supply additions to the get\_close\_obs() routine only. At the
top of the model\_mod code it would declare:


    use location_mod, only :: location_get_close_obs => get_close_obs,    &
                              get_close_maxdist_init, get_close_obs_init, &
                              get_close_obs_destroy

That makes calls to the maxdist\_init, init, and destroy routines simply
pass through to the code in the location\_mod, but the model\_mod must
supply a get\_close\_obs() subroutine. When it wants to call the code in
the location\_mod it calls *location\_get\_close\_obs()*.

One use pattern is for the model\_mod to call the location
get\_close\_obs() routine without the *dist* argument. This returns a
list of any potentially close locations without computing the exact
distance from the base location. At this point the list of locations is
a copy and the model\_mod routine is free to alter the list in any way
it chooses: it can change the locations to make certain types of
locations appear closer or further away from the base location; it can
convert the vertical coordinates into a common coordinate type so that
calls to the *get\_dist()* routine can do full 3d distance computations
and not just 2d (the vertical coordinates must match between the base
location and the locations in the list in order to compute a 3d
distance). Then typically the model\_mod code loops over the list
calling the *get\_dist()* routine to get the actual distances to be
returned to the calling code. To localize in the vertical in a
particular unit type, this is the place where the conversion to that
vertical unit should be done.

#### Horizontal Distance Only

If *horiz\_distance\_only* is .true. in the namelist then the vertical
coordinate is ignored and only the great-circle distance between the two
locations is computed, as if they were both on the surface of the
sphere.

If *horiz\_distance\_only* is .false. in the namelist then the
appropriate normalization constant determines the relative impact of
vertical and horizontal separation. Since only a single localization
distance is specified, and the vertical scales might have very different
distance characteristics, the vert\_normalization\_xxx values can be
used to scale the vertical appropriately to control the desired
influence of observations in the vertical.

#### Precomputation for Run-time Search Efficiency

For search efficiency all locations are pre-binned. The surface of the
sphere is divided up into *nlon* by *nlat* boxes and the index numbers
of all items (both state vector entries and observations) are stored in
the appropriate box. To locate all points close to a given location,
only the locations listed in the boxes within the search radius must be
checked. This speeds up the computations, for example, when localization
controls which state vector items are impacted by any given observation.
The search radius is the localization distance and only those state
vector items in boxes closer than the radius to the observation location
are processed.

The default values have given good performance on many of our existing
model runs, but for tuning purposes the box counts have been added to
the namelist to allow adjustment. By default the code prints some
summary information about how full the average box is, how many are
empty, and how many items were in the box with the largest count. The
namelist value *output\_box\_info* can be set to .true. to get even more
information about the box statistics. The best performance will be
obtained somewhere between two extremes; the worst extreme is all the
points are located in just a few boxes. This degenerates into a (slow)
linear search through the index list. The other extreme is a large
number of empty or sparsely filled boxes. The overhead of creating,
managing, and searching a long list of boxes will impact performance.
The best performance lies somewhere in the middle, where each box
contains a reasonable number of values, more or less evenly distributed
across boxes. The absolute numbers for best performance will certainly
vary from case to case.

For latitude, the *nlat* boxes are distributed evenly across the actual
extents of the data. (Locations are in radians, so the maximum limits
are the poles at -PI/2 and +PI/2). For longitude, the code automatically
determines if the data is spread around more than half the sphere, and
if so, the boxes are distributed evenly across the entire sphere
(longitude range 0 to 2\*PI). If the data spans less than half the
sphere in longitude, the actual extent of the data is determined
(including correctly handling the cyclic boundary at 0) and the boxes
are distributed only within the data extent. This simplifies the actual
distance calculations since the distance from the minimum longitude box
to the maximum latitude box cannot be shorter going the other way around
the sphere. In practice, for a global model the boxes are evenly
distributed across the entire surface of the sphere. For local or
regional models, the boxes are distributed only across the the extent of
the local grid.

For efficiency in the case where the boxes span less than half the
globe, the 3D location module needs to be able to determine the greatest
longitude difference between a base point at latitude `φs` and all
points that are separated from that point by a central angle of `θ`. We
might also want to know the latitude, `φf` , at which the largest
separation occurs. Note also that an intermediate form below allows the
computation of the maximum longitude difference at a particular
latitude.

The central angle between a point at latitude `φs` and a second point at
latitude `φf` that are separated in longitude by `Δλ` is\
\
`   θ = cos-1(sinφssinφf +     cosφscosφfcosΔλ) `\
\
Taking the cos of both sides gives\
\
`   cosθ = (sinφssinφf +    cosφscosφfcosΔλ)`\
\
Solving for `cosΔλ` gives\
\
`   cosΔλ      = (a - b sinφf)/(c cosφf)     = a/c secφf -        b/c tanφf `\
\
where `a = cosθ` , `b = sinφs` , and `c = cosφs` . We want to maximize
`Δλ` which implies minimizing `cosΔλ` subject to constraints. Taking the
derivative with respect to `φf` gives\
\
`   (d cosΔλ)/(dφf) =     a/c secφf tanφf  - b/c sec2φf = 0`\
\
Factoring out `secφf` which can never be 0 and using the definitions of
`sec` and `tan` gives\
\
`   (a sinφf)/(c cosφf) - b/(c cosφf) = 0`\
\
Solving in the constrained range from 0 to PI/2 gives\
\
`    sinφf = b/a =     sinφs/cosθ`\
\
So knowing base point (`φs`, `λs`), latitude `φf`, and distance `θ` we
can use the great circle equation to find the longitude difference at
the greatest separation point\
\
`    Δλ = cos-1((a -  (b sinφf))   / (c cosφf))`\
\
Note that if the angle between the base point and a pole is less than or
equal to the central angle, all longitude differences will occur as the
pole is approached.

[]{#Interface}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

OTHER MODULES USED
------------------

    types_mod
    utilities_mod
    random_seq_mod

------------------------------------------------------------------------

Public Interfaces
-----------------

  ----------------------------- ------------------------------------------------------
  *use location\_mod, only :*   [location\_type](#location_type)
                                [get\_close\_type](#get_close_type)
                                [get\_location](#get_location)
                                [set\_location](#set_location)
                                [write\_location](#write_location)
                                [read\_location](#read_location)
                                [interactive\_location](#interactive_location)
                                [set\_location\_missing](#set_location_missing)
                                [query\_location](#query_location)
                                [get\_close\_maxdist\_init](#get_close_maxdist_init)
                                [get\_close\_obs\_init](#get_close_obs_init)
                                [get\_close\_obs](#get_close_obs)
                                [get\_close\_obs\_destroy](#get_close_obs_destroy)
                                [get\_dist](#get_dist)
                                [LocationDims](#LocationDims)
                                [LocationName](#LocationName)
                                [LocationLName](#LocationLName)
                                [horiz\_dist\_only](#horiz_dist_only)
                                [vert\_is\_undef](#vert_is_undef)
                                [vert\_is\_surface](#vert_is_surface)
                                [vert\_is\_pressure](#vert_is_pressure)
                                [vert\_is\_scale\_height](#vert_is_scale_height)
                                [vert\_is\_level](#vert_is_level)
                                [vert\_is\_height](#vert_is_height)
                                [VERTISUNDEF](#vert_constants)
                                [VERTISSURFACE](#vert_constants)
                                [VERTISLEVEL](#vert_constants)
                                [VERTISPRESSURE](#vert_constants)
                                [VERTISHEIGHT](#vert_constants)
                                [VERTISSCALEHEIGHT](#vert_constants)
                                [operator(==)](#equal)
                                [operator(/=)](#not_equal)
  ----------------------------- ------------------------------------------------------

Namelist interface [*&location\_nml*](#Namelist) must be read from file
*input.nml*.

A note about documentation style. Optional arguments are enclosed in
brackets *\[like this\]*.

[]{#location_type}\

<div class="type">

*type location\_type*
       private
       real(r8) :: lon, lat, vloc
       integer  :: which_vert
    end type location_type

</div>

<div class="indent1">

Provides an abstract representation of physical location on a three-d
spherical shell.

  Component     Description
  ------------- --------------------------------------------------------------------------------------------------------------------
  lon           longitude in radians
  lat           latitude in radians
  vloc          vertical location, units as selected by which\_vert
  which\_vert   type of vertical location: -2=no specific vert location; -1=surface; 1=level; 2=pressure; 3=height, 4=scale height

The vertical types have parameters defined for them so they can be
referenced by name instead of number.

</div>

\
[]{#get_close_type}\

<div class="type">

*type get\_close\_type*
       private
       integer  :: num
       real(r8) :: maxdist
       integer, pointer :: lon_offset(:, :)
       integer, pointer :: obs_box(:)
       integer, pointer :: count(:, :)
       integer, pointer :: start(:, :)
    end type get_close_type

</div>

<div class="indent1">

Provides a structure for doing efficient computation of close locations.

  Component     Description
  ------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------
  num           Number of locations in list
  maxdist       Threshhold distance. Anything closer is close.
  lon\_offset   Dimensioned nlon by nlat. For a given offset in longitude boxes and difference in latitudes, gives max distance from base box to a point in offset box.
  obs\_box      Dimensioned num. Gives index of what box each location is in.
  count         Dimensioned nlon by nlat. Number of obs in each box.
  start         Dimensioned nlon by nlat. Index in straight storage list where obs in each box start.

</div>

\
[]{#get_location}\

<div class="routine">

*var = get\_location(loc)*
    real(r8), dimension(3)          :: get_location
    type(location_type), intent(in) :: loc

</div>

<div class="indent1">

Extracts the longitude and latitude (converted to degrees) and the
vertical location from a location type and returns in a 3 element real
array.

  ----------------- ---------------------------------------------------------------
  *get\_location*   The longitude and latitude (in degrees) and vertical location
  *loc*             A location type
  ----------------- ---------------------------------------------------------------

</div>

\
[]{#set_location}\

<div class="routine">

*var = set\_location(lon, lat, vert\_loc, which\_vert)*
    type(location_type) :: set_location
    real(r8), intent(in)    :: lon
    real(r8), intent(in)    :: lat
    real(r8), intent(in)    :: vert_loc
    integer,  intent(in)    :: which_vert

</div>

<div class="indent1">

Returns a location type with the input longitude and latitude (input in
degrees) and the vertical location of type specified by which\_vert.

  ----------------- -----------------------------------------------
  *set\_location*   A location type
  *lon*             Longitude in degrees
  *lat*             Latitude in degrees
  *vert\_loc*       Vertical location consistent with which\_vert
  *which\_vert*     The vertical location type
  ----------------- -----------------------------------------------

</div>

\
[]{#write_location}\

<div class="routine">

*call write\_location(locfile, loc *\[, fform, charstring\]*)*
    integer,               intent(in)       ::  locfile 
    type(location_type),   intent(in)       ::  loc 
    character(len=*), optional, intent(in)  ::  fform 
    character(len=*), optional, intent(out) ::  charstring 

</div>

<div class="indent1">

Given an integer IO channel of an open file and a location, writes the
location to this file. The *fform* argument controls whether write is
"FORMATTED" or "UNFORMATTED" with default being formatted. If the final
*charstring* argument is specified, the formatted location information
is written to the character string only, and the *locfile* argument is
ignored.

  -------------- --------------------------------------------------------------------------------------------------------------------
  *locfile*      the unit number of an open file.
  *loc*          location type to be written.
  *fform*        Format specifier ("FORMATTED" or "UNFORMATTED"). Default is "FORMATTED" if not specified.
  *charstring*   Character buffer where formatted location string is written if present, and no output is written to the file unit.
  -------------- --------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#read_location}\

<div class="routine">

*var = read\_location(locfile *\[, fform\]*)*
    type(location_type)                    :: read_location
    integer, intent(in)                    :: locfile
    character(len=*), optional, intent(in) :: fform

</div>

<div class="indent1">

Reads a location\_type from a file open on channel locfile using format
*fform* (default is formatted).

  ------------------ --------------------------------------------------------------------------------
  *read\_location*   Returned location type read from file
  *locfile*          Integer channel opened to a file to be read
  *fform*            Optional format specifier ("FORMATTED" or "UNFORMATTED"). Default "FORMATTED".
  ------------------ --------------------------------------------------------------------------------

</div>

\
[]{#interactive_location}\

<div class="routine">

*call interactive\_location(location *\[, set\_to\_default\]*)*
    type(location_type), intent(out) :: location
    logical, optional, intent(in)    :: set_to_default

</div>

<div class="indent1">

Use standard input to define a location type. With set\_to\_default true
get one with all elements set to 0.

  -------------------- --------------------------------------------------
  *location*           Location created from standard input
  *set\_to\_default*   If true, sets all elements of location type to 0
  -------------------- --------------------------------------------------

</div>

\
[]{#query_location}\

<div class="routine">

*var = query\_location(loc *\[, attr\]*)*
    real(r8)                               :: query_location
    type(location_type), intent(in)        :: loc
    character(len=*), optional, intent(in) :: attr

</div>

<div class="indent1">

Returns the value of which\_vert, latitude, longitude, or vertical
location from a location type as selected by the string argument attr.
If attr is not present or if it is 'WHICH\_VERT', the value of
which\_vert is converted to real and returned. Otherwise, attr='LON'
returns longitude, attr='LAT' returns latitude and attr='VLOC' returns
the vertical location.

  ------------------- ------------------------------------------------------------------------------------
  *query\_location*   Returns longitude, latitude, vertical location, or which\_vert (converted to real)
  *loc*               A location type
  *attr*              Selects 'WHICH\_VERT', 'LON', 'LAT' or 'VLOC'
  ------------------- ------------------------------------------------------------------------------------

</div>

\
[]{#set_location_missing}\

<div class="routine">

*var = set\_location\_missing()*
    type(location_type) :: set_location_missing

</div>

<div class="indent1">

Returns a location with all elements set to missing values defined in
types module.

  -------------------------- ----------------------------------------------------
  *set\_location\_missing*   A location with all elements set to missing values
  -------------------------- ----------------------------------------------------

</div>

\
[]{#get_close_maxdist_init}\

<div class="routine">

*call get\_close\_maxdist\_init(gc,maxdist, *\[maxdist\_list\]*)*
    type(get_close_type), intent(inout) :: gc
    real(r8), intent(in)                :: maxdist
    real(r8), intent(in), optional      :: maxdist_list(:)

</div>

<div class="indent1">

Sets the threshhold distance. *maxdist* is in units of radians. Anything
closer than this is deemed to be close. This routine must be called
first, before the other *get\_close* routines. It allocates space so it
is necessary to call *get\_close\_obs\_destroy* when completely done
with getting distances between locations.

If the last optional argument is not specified, maxdist applies to all
locations. If the last argument is specified, it must be a list of
exactly the length of the number of specific types in the
obs\_kind\_mod.f90 file. This length can be queried with the
[get\_num\_types\_of\_obs()](../../modules/observations/obs_kind_mod.html#get_num_types_of_obs)
function to get count of obs types. It allows a different maximum
distance to be set per base type when get\_close() is called.

  ----------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  *gc*        Data for efficiently finding close locations.
  *maxdist*   Anything closer than this number of radians is a close location.
  *maxdist*   If specified, must be a list of real values. The length of the list must be exactly the same length as the number of observation types defined in the obs\_def\_kind.f90 file. (See [get\_num\_types\_of\_obs()](../../modules/observations/obs_kind_mod.html#get_num_types_of_obs) to get count of obs types.) The values in this list are used for the obs types as the close distance instead of the maxdist argument.
  ----------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

</div>

\
[]{#get_close_obs_init}\

<div class="routine">

*call get\_close\_obs\_init(gc, num, obs)*
    type(get_close_type),             intent(inout) :: gc
    integer,                          intent(in)    :: num
    type(location_type), dimension(:) intent(in)    :: obs

</div>

<div class="indent1">

Initialize storage for efficient identification of locations close to a
given location. Allocates storage for keeping track of which 'box' each
location in the list is in. Must be called after
*get\_close\_maxdist\_init*, and the list of locations here must be the
same as the list of locations passed into *get\_close\_obs()*. If the
list changes, *get\_close\_obs\_destroy()* must be called, and both the
initialization routines must be called again. It allocates space so it
is necessary to call *get\_close\_obs\_destroy* when completely done
with getting distances between locations.

  ------- ---------------------------------------------------------------------------------------
  *gc*    Structure that contains data to efficiently find locations close to a given location.
  *num*   The number of locations in the list.
  *obs*   The locations of each element in the list, not used in 1D implementation.
  ------- ---------------------------------------------------------------------------------------

</div>

\
[]{#get_close_obs}\

<div class="routine">

*call get\_close\_obs(gc, base\_obs\_loc, base\_obs\_type, obs,
obs\_kind, num\_close, close\_ind, dist)*
    type(get_close_type),              intent(in)  :: gc
    type(location_type),               intent(in)  :: base_obs_loc
    integer,                           intent(in)  :: base_obs_type
    type(location_type), dimension(:), intent(in)  :: obs
    integer,             dimension(:), intent(in)  :: obs_kind
    integer,                           intent(out) :: num_close
    integer,             dimension(:), intent(out) :: close_ind
    real(r8), optional,  dimension(:), intent(out) :: dist

</div>

<div class="indent1">

Given a single location and a list of other locations, returns the
indices of all the locations close to the single one along with the
number of these and the distances for the close ones. The list of
locations passed in via the *obs* argument must be identical to the list
of *obs* passed into the most recent call to *get\_close\_obs\_init()*.
If the list of locations of interest changes
*get\_close\_obs\_destroy()* must be called and then the two
initialization routines must be called before using *get\_close\_obs()*
again.

Note that the base location is passed with the specific type associated
with that location. The list of potential close locations is matched
with a list of generic kinds. This is because in the current usage in
the DART system the base location is always associated with an actual
observation, which has both a specific type and generic kind. The list
of potentially close locations is used both for other observation
locations but also for state variable locations which only have a
generic kind.

If called without the optional *dist* argument, all locations that are
potentially close are returned, which is likely a superset of the
locations that are within the threshold distance specified in the
*get\_close\_maxdist\_init()* call. This can be useful to collect a list
of potential locations, and then to convert all the vertical coordinates
into one consistent unit (pressure, height in meters, etc), and then the
list can be looped over, calling get\_dist() directly to get the exact
distance, either including vertical or not depending on the setting of
*horiz\_dist\_only*.

  ------------------- -------------------------------------------------------------------------------------
  *gc*                Structure to allow efficient identification of locations close to a given location.
  *base\_obs\_loc*    Single given location.
  *base\_obs\_type*   Specific type of the single location.
  *obs*               List of locations from which close ones are to be found.
  *obs\_kind*         Generic kind associated with locations in obs list.
  *num\_close*        Number of locations close to the given location.
  *close\_ind*        Indices of those locations that are close.
  *dist*              Distance between given location and the close ones identified in close\_ind.
  ------------------- -------------------------------------------------------------------------------------

</div>

\
[]{#get_close_obs_destroy}\

<div class="routine">

*call get\_close\_obs\_destroy(gc)*
    type(get_close_type), intent(inout) :: gc

</div>

<div class="indent1">

Releases memory associated with the *gc* derived type. Must be called
whenever the list of locations changes, and then
*get\_close\_maxdist\_init* and *get\_close\_obs\_init* must be called
again with the new locations list.

  ------ -----------------------------------------------
  *gc*   Data for efficiently finding close locations.
  ------ -----------------------------------------------

</div>

\
[]{#get_dist}\

<div class="routine">

*var = get\_dist(loc1, loc2, *\[, type1, kind2, no\_vert\]*)*
    real(r8)                        :: get_dist
    type(location_type), intent(in) :: loc1
    type(location_type), intent(in) :: loc2
    integer, optional,   intent(in) :: type1
    integer, optional,   intent(in) :: kind2
    logical, optional,   intent(in) :: no_vert 

</div>

<div class="indent1">

Returns the distance between two locations in radians. If
*horiz\_dist\_only* is set to .TRUE. in the locations namelist, it
computes great circle distance on sphere. If *horiz\_dist\_only* is
false, then it computes an ellipsoidal distance with the horizontal
component as above and the vertical distance determined by the types of
the locations and the normalization constants set by the namelist for
the different vertical coordinate types. The vertical normalization
gives the vertical distance that is equally weighted as a horizontal
distance of 1 radian. If *no\_vert* is present, it overrides the value
in the namelist and controls whether vertical distance is included or
not.

The type and kind arguments are not used by the default location code,
but are available to any user-supplied distance routines which want to
do specialized calculations based on the types/kinds associated with
each of the two locations.

  ------------ ---------------------------------------------------------------------------------------
  *loc1*       First of two locations to compute distance between.
  *loc2*       Second of two locations to compute distance between.
  *type1*      DART specific type associated with location 1.
  *kind2*      DART generic kind associated with location 2.
  *no\_vert*   If true, no vertical component to distance. If false, vertical component is included.
  *var*        distance between loc1 and loc2.
  ------------ ---------------------------------------------------------------------------------------

</div>

\
[]{#vert_is_undef}\

<div class="routine">

*var = vert\_is\_undef(loc)*
    logical                         :: vert_is_undef
    type(location_type), intent(in) :: loc

</div>

<div class="indent1">

Returns true if which\_vert is set to undefined, else false. The meaning
of 'undefined' is specific; it means there is no particular vertical
location associated with this type of measurement; for example a
column-integrated value.

  ------------------- ----------------------------------------------------------
  *vert\_is\_undef*   Returns true if vertical coordinate is set to undefined.
  *loc*               A location type
  ------------------- ----------------------------------------------------------

</div>

\
[]{#vert_is_surface}\

<div class="routine">

*var = vert\_is\_surface(loc)*
    logical                         :: vert_is_surface
    type(location_type), intent(in) :: loc

</div>

<div class="indent1">

Returns true if which\_vert is for surface, else false.

  --------------------- -----------------------------------------------------
  *vert\_is\_surface*   Returns true if vertical coordinate type is surface
  *loc*                 A location type
  --------------------- -----------------------------------------------------

</div>

\
[]{#vert_is_pressure}\

<div class="routine">

*var = vert\_is\_pressure(loc)*
    logical                         :: vert_is_pressure
    type(location_type), intent(in) :: loc

</div>

<div class="indent1">

Returns true if which\_vert is for pressure, else false.

  ---------------------- ------------------------------------------------------
  *vert\_is\_pressure*   Returns true if vertical coordinate type is pressure
  *loc*                  A location type
  ---------------------- ------------------------------------------------------

</div>

\
[]{#vert_is_scale_height}\

<div class="routine">

*var = vert\_is\_scale\_height(loc)*
    logical                         :: vert_is_scale_height
    type(location_type), intent(in) :: loc

</div>

<div class="indent1">

Returns true if which\_vert is for scale\_height, else false.

  --------------------------- -----------------------------------------------------------
  *vert\_is\_scale\_height*   Returns true if vertical coordinate type is scale\_height
  *loc*                       A location type
  --------------------------- -----------------------------------------------------------

</div>

\
[]{#vert_is_level}\

<div class="routine">

*var = vert\_is\_level(loc)*
    logical                         :: vert_is_level
    type(location_type), intent(in) :: loc

</div>

<div class="indent1">

Returns true if which\_vert is for level, else false.

  ------------------- ---------------------------------------------------
  *vert\_is\_level*   Returns true if vertical coordinate type is level
  *loc*               A location type
  ------------------- ---------------------------------------------------

</div>

\
[]{#vert_is_height}\

<div class="routine">

*var = vert\_is\_height(loc)*
    logical                         :: vert_is_height
    type(location_type), intent(in) :: loc

</div>

<div class="indent1">

Returns true if which\_vert is for height, else false.

  -------------------- ----------------------------------------------------
  *vert\_is\_height*   Returns true if vertical coordinate type is height
  *loc*                A location type
  -------------------- ----------------------------------------------------

</div>

\
[]{#has_vertical_localization}\

<div class="routine">

*var = has\_vertical\_localization()*
    logical :: has_vertical_localization

</div>

<div class="indent1">

Returns .TRUE. if the namelist variable *horiz\_dist\_only* is .FALSE.
meaning that vertical separation between locations is going to be
computed by *get\_dist()* and by *get\_close\_obs()*.

This routine should perhaps be renamed to something like
'using\_vertical\_for\_distance' or something similar. The current use
for it is in the localization code inside filter, but that doesn't make
this a representative function name. And at least in current usage,
returning the opposite setting of the namelist item makes the code read
more direct (fewer double negatives).

</div>

\
[]{#equal}\

<div class="routine">

*loc1 == loc2*
    type(location_type), intent(in) :: loc1, loc2

</div>

<div class="indent1">

Returns true if the two location types have identical values, else
false.

</div>

\
[]{#not_equal}\

<div class="routine">

*loc1 /= loc2*
    type(location_type), intent(in) :: loc1, loc2

</div>

<div class="indent1">

Returns true if the two location types do NOT have identical values,
else false.

</div>

\
[]{#vert_constants}\

<div class="routine">

    integer, parameter :: VERTISUNDEF       = -2
    integer, parameter :: VERTISSURFACE     = -1
    integer, parameter :: VERTISLEVEL       =  1
    integer, parameter :: VERTISPRESSURE    =  2
    integer, parameter :: VERTISHEIGHT      =  3
    integer, parameter :: VERTISSCALEHEIGHT =  4

</div>

<div class="indent1">

Constant parameters used to differentiate vertical types.

</div>

\
[]{#LocationDims}\

<div class="routine">

    integer, parameter :: LocationDims = 3

</div>

<div class="indent1">

This is a **constant**. Contains the number of real values in a location
type. Useful for output routines that must deal transparently with many
different location modules.

</div>

\
[]{#LocationName}\

<div class="routine">

    character(len=129), parameter :: LocationName = "loc3Dsphere"

</div>

<div class="indent1">

This is a **constant**. A parameter to identify this location module in
output metadata.

</div>

\
[]{#LocationLName}\

<div class="routine">

    character(len=129), parameter :: LocationLName = 

           "threed sphere locations: lon, lat, vertical"

</div>

<div class="indent1">

This is a **constant**. A parameter set to "threed sphere locations:
lon, lat, vertical" used to identify this location module in output long
name metadata.

</div>

\
[]{#FilesUsed}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Files
-----

  filename    purpose
  ----------- ------------------------------------
  input.nml   to read the location\_mod namelist

[]{#References}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

References
----------

1.  none

[]{#Errors}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Error Codes and Conditions
--------------------------

<div class="errors">

Routine
Message
Comment
initialize\_module
nlon must be odd
Tuning parameter for number of longitude boxes must be odd for algorithm
to function.
get\_dist
Dont know how to compute vertical distance for unlike vertical
coordinates
Need same which\_vert for distances.
set\_location
longitude (\#) is not within range \[0,360\]
Is it really a longitude?
set\_location
latitude (\#) is not within range \[-90,90\]
Is it really a latitude?
set\_location
which\_vert (\#) must be one of -2, -1, 1, 2, 3, or 4
Vertical coordinate type restricted to:\
-2 = no specific vertical location\
-1 = surface value\
1 = (model) level\
2 = pressure\
3 = height\
4 = scale height\
read\_location
Expected location header "loc3d" in input file, got \_\_\_
Probable mixing of other location modules in observation sequence
processing.
nc\_write\_location
Various NetCDF-f90 interface error messages
From one of the NetCDF calls in nc\_write\_location

</div>

KNOWN BUGS
----------

The Hawaii and Workshop versions of this module had an error in the
approximate distance computation. The available values in the lookup
table for cosine were insufficient for some cases. This manifested
itself as potential errors, most commonly for computing distances near
the poles. For relatively small horizontal localizations, this problem
only occurred for locations very near the pole.

[]{#FuturePlans}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Future Plans
------------

Need to provide more efficient algorithms for getting close locations
and document the nlon and nlat choices and their impact on cost.

The collection of 'val = vert\_is\_xxx()' routines should probably be
replaced by a single call 'val = vert\_is(loc, VERTISxxx)'.

See the note in the 'has\_vertical\_localization()' about a better name
for this routine.

The functions of 'get\_close\_maxdist\_init()' and
'get\_close\_obs\_init()' appear to be able to be combined into a single
init routine. This impacts all model\_mods, however, since they can
intercept these routines. Doing this will be a non-backwards compatible
change.

The use of 'obs' in all these routine names should probably be changed
to 'loc' since there is no particular dependence that they be
observations. They may need to have an associated DART kind, but these
routines are used for DART state vector entries so it's misleading to
always call them 'obs'.

[]{#PrivateComponents}

<div class="top">

\[[top](#)\]

</div>

------------------------------------------------------------------------

Private Components
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


