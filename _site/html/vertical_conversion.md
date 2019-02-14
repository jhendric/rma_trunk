[]{#TOP}

Vertical Conversion of Observations
===================================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../images/Dartboard7.png){h | Index](../index.html)\            |
| eight="70"}                       | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

In Lanai vertical conversion of observations occurs in get\_close\_obs.
The Lanai code in filter\_assim is as follows:

    SEQUENTIAL_OBS do i = 1, obs_ens_handle%num_vars
       ...
       broadcast increments for observation i
       call get_close_obs()
       call convert_vertical_location(observation(i))
    enddo

If this algorithm was followed in RMA DART, all processors would have to
communicate to calculate the location of observation i. This is a huge
amount of contention since all processors are doing exactly the same
calculation so need to access exactly the same state elements. This
causes the code to run very slowly, for example 1 minute for 1000
observations, versus 5 seconds for 1000 observations for Lanai.

However, there is no need to calculate the vertical conversion inside
the SEQUENTIAL\_OBS do loop, since the mean state vector used is not
updated during the loop. (In Lanai it is the array passed to the
model\_mod by ens\_mean\_for\_model in filter\_main). Also this
calculation does not scale, because all processors do the same
calculation.

In DART RMA the owner of an observation converts the vertical location
of an observation and broacasts it to all other processors as part of
the broadcast in the SEQUENTIAL\_OBS do loop.

The DART RMA code calculates the vertical of all observations before the
loop. This potentially scales better because processors only calculate
their own observation conversions, but does require model\_mod
interfaces for vertical conversion.

The DART RMA code in filter\_assim is as follows:

    do i =, obs_ens_handle%my_num_vars
       call convert_vertical_location(my_obs_loc(i))
    end do
    SEQUENTIAL_OBS do i = 1, obs_ens_handle%num_vars
       ...
       broadcast increments and vertical location for observation i
       ...
    enddo

### Bitwise Problem

Moving the `convert_vertical_location` changes the number of
`get/set location` calls. There is a bitwise creep of the location when
you do this. This is in the conversion from degrees to radians and back
again. If you want to do the exact number of `get/set location` you can
change the line lanai\_bitwise = .false. to lanai\_bitwise = .true. in
assim\_tools\_mod.f90. Note this is not a namelist option because
production code should not be run with lanai\_bitwise = .true. For more
detail on running bitwise with Lanai see [bitwise
considerations](bitwise_considerations.html).

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


