! DART software - Copyright UCAR. This open source software is provided
! by UCAR, "as is", without charge, subject to all terms of use at
! http://www.image.ucar.edu/DAReS/DART/DART_download
!
! $Id$

program COSMOS_to_obs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! COSMOS_to_obs - reads the COSMOS data as defined in the level 2 
!     product available from the COSMOS data portal at:
!     http://cosmos.hwr.arizona.edu/Probes/probemap.php
!
!     original 3 May       2012   Tim Hoar       NCAR/IMAGe
!     modified 6 September 2012   Rafael Rosolem University of Arizona 
!     modified 10 October  2012   Tim Hoar
!        * include site-specific metadata in obs sequence 
!
! QC flags are defined as: 0 = BEST     (corrected for water vapor)
!                          1 = OK       (NOT corrected for water vapor)
!
! The correction for water vapor is intented to be available 
! in the level 2 data at some point in the future.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

use          types_mod, only : r8, MISSING_R8, metadatalength

use      utilities_mod, only : initialize_utilities, finalize_utilities, &
                               register_module, error_handler, E_MSG, E_ERR, &
                               open_file, close_file, do_nml_file, do_nml_term, &
                               check_namelist_read, find_namelist_in_file, &
                               nmlfileunit, file_exist, nc_check, to_upper, &
                               find_textfile_dims

use   time_manager_mod, only : time_type, set_calendar_type, GREGORIAN, &
                               set_date, set_time, get_time, print_time, &
                               print_date, operator(-), operator(+), operator(>), &
                               operator(<), operator(==), operator(<=), operator(>=)

use       location_mod, only : location_type, set_location, VERTISSURFACE

use   obs_sequence_mod, only : obs_sequence_type, obs_type, read_obs_seq, &
                               static_init_obs_sequence, init_obs, write_obs_seq, &
                               init_obs_sequence, get_num_obs, set_obs_def, &
                               set_copy_meta_data, set_qc_meta_data, &
                               set_obs_values, set_qc

use        obs_def_mod, only : obs_def_type, set_obs_def_type_of_obs, &
                               set_obs_def_location, set_obs_def_time, &
                               set_obs_def_error_variance, set_obs_def_key

use  obs_utilities_mod, only : add_obs_to_seq

use obs_def_COSMOS_mod, only : set_cosmos_metadata

use       obs_kind_mod, only : COSMOS_NEUTRON_INTENSITY

use typesizes
use netcdf

implicit none

! version controlled file description for error handling, do not edit
character(len=256), parameter :: source   = &
   "$URL$"
character(len=32 ), parameter :: revision = "$Revision$"
character(len=128), parameter :: revdate  = "$Date$"

!-----------------------------------------------------------------------
! Namelist with default values
!-----------------------------------------------------------------------

character(len=256) :: site_metadata_file = 'COSMIC_parlist.nc'
character(len=128) :: text_input_file = 'corcounts.txt'
character(len=128) :: obs_out_file    = 'obs_seq.out'
character(len=128) :: sitename        = 'missing'
real(r8)           :: maxgoodqc       = 3
logical            :: verbose         = .false.

namelist /COSMOS_to_obs_nml/ site_metadata_file, text_input_file, &
   obs_out_file, sitename, maxgoodqc, verbose

!-----------------------------------------------------------------------
! globally-scoped variables
!-----------------------------------------------------------------------

character(len=256)      :: input_line, string1, string2, string3
integer                 :: iline
logical                 :: first_obs
integer                 :: oday, osec, rcio, iunit
integer                 :: num_copies, num_qc, max_obs
real(r8)                :: oerr, qc
type(obs_sequence_type) :: obs_seq
type(obs_type)          :: obs, prev_obs
type(time_type)         :: prev_time
integer, parameter      :: LEVEL2QC = 1

! The cosmosdata type holds all the information from the text file
! from the COSMOS instrument.
!
! "You should use this Level 2 data to get the counts and its standard deviation.
!  They are the last two columns in the file (respectively, 'CORR' and 'ERR')"

type cosmosdata
  integer           :: dateindex
  integer           :: timeindex
  integer           :: neutronindex
  integer           :: neutronstdindex
  character(len=20) :: datestring       = 'YYYY-MM-DD'
  character(len=20) :: timestring       = 'HH:MM'
  character(len=20) :: neutronstring    = 'CORR'
  character(len=20) :: neutronstdstring = 'ERR'
  type(time_type)   :: time_obs
  integer  :: year
  integer  :: month
  integer  :: day
  integer  :: hour
  integer  :: minute
  real(r8) :: neutron
  real(r8) :: neutronstd
  real(r8) :: qc
end type cosmosdata

type(cosmosdata) :: cosmos ! we only need one of these.

! The site_metadata type holds all the site-specific information

type site_metadata
   character(len=metadatalength) :: sitename
   type(location_type) :: location
   real(r8)            :: latitude
   real(r8)            :: longitude
   real(r8)            :: elevation
   real(r8)            :: bd         ! Dry Soil Bulk Density [  g / cm^3]
   real(r8)            :: lattwat    ! Lattice Water Content [M^3 /  M^3]
   real(r8)            :: N          ! High Energy Neutron Intensity
   real(r8)            :: alpha      ! Ratio of Fast Neutron Creation Factor
   real(r8)            :: L1         ! High Energy   Soil Attenuation Length
   real(r8)            :: L2         ! High Energy  Water Attenuation Length
   real(r8)            :: L3         ! Fast Neutron  Soil Attenuation Length
   real(r8)            :: L4         ! Fast Neutron Water Attenuation Length
end type site_metadata

type(site_metadata), allocatable, dimension(:) :: cosmos_metadata
integer  :: nSites, siteIndx, obsindx
real(r8) :: bd, lattwat, N, alpha, L1, L2, L3, L4   

!-----------------------------------------------------------------------
! start of executable code
!-----------------------------------------------------------------------

call initialize_utilities('COSMOS_to_obs')

! Print module information to log file and stdout.
call register_module(source, revision, revdate)

! Read the namelist entry
call find_namelist_in_file("input.nml", "COSMOS_to_obs_nml", iunit)
read(iunit, nml = COSMOS_to_obs_nml, iostat = rcio)
call check_namelist_read(iunit, rcio, "COSMOS_to_obs_nml")

! Record the namelist values used for the run ...
if (do_nml_file()) write(nmlfileunit, nml=COSMOS_to_obs_nml)
if (do_nml_term()) write(     *     , nml=COSMOS_to_obs_nml)

! time setup
call set_calendar_type(GREGORIAN)
prev_time = set_time(0, 0)

! Read the COSMOS metadata/parameters for each site.
! These will be added to the metadata for neutron intensity observations.
nSites   = read_site_metadata(site_metadata_file)
siteIndx = find_site_index(sitename)
bd       = cosmos_metadata(siteIndx)%bd
lattwat  = cosmos_metadata(siteIndx)%lattwat
N        = cosmos_metadata(siteIndx)%N
alpha    = cosmos_metadata(siteIndx)%alpha
L1       = cosmos_metadata(siteIndx)%L1
L2       = cosmos_metadata(siteIndx)%L2
L3       = cosmos_metadata(siteIndx)%L3
L4       = cosmos_metadata(siteIndx)%L4

if (verbose) print *, 'COSMOS site located at lat, lon, elev  =', &
                       cosmos_metadata(siteIndx)%latitude, &
                       cosmos_metadata(siteIndx)%longitude, &
                       cosmos_metadata(siteIndx)%elevation

! We need to know the maximum number of observations in the input file.
! Each line has info for a single observation we want (COSMOS neutron counts).
! Each observation in this series will have a single
! observation value, its standard deviation, and a quality control flag.  
! Initialize two empty observations - one to track location
! in observation sequence - the other is for the new observation.

call find_textfile_dims(text_input_file, max_obs)

if (max_obs < 0) then
   write (string1,*) '<'//trim(text_input_file)//'> does not exist.'
   call error_handler(E_ERR,'main', string1, source, revision, revdate)
elseif (max_obs < 2) then
   write (string1,*) trim(text_input_file)//' has no observation values in it.'
   call error_handler(E_ERR,'main', string1, source, revision, revdate)
endif

iunit = open_file(text_input_file, 'formatted', 'read')
if (verbose) print *, 'opened input file ' // trim(text_input_file)

num_copies = 1
num_qc     = 1
first_obs  = .true.

call static_init_obs_sequence()
call init_obs(        obs,      num_copies, num_qc)
call init_obs(        prev_obs, num_copies, num_qc)
call init_obs_sequence(obs_seq, num_copies, num_qc, max_obs)

! the first one needs to contain the string 'observation' and the
! second needs the string 'QC'.
call set_copy_meta_data(obs_seq, 1, 'observation')
call set_qc_meta_data(  obs_seq, 1, 'COSMOS QC')

! The first line describes all the fields ... column headers, if you will
call decode_header(iunit)

obsloop: do iline = 2,max_obs

   ! read in entire text line into a buffer
   read(iunit,'(A)',iostat=rcio) input_line
   if (rcio < 0) exit obsloop
   if (rcio > 0) then
      write (string1,'(''Cannot read (error '',i3,'') line '',i8,'' in '',A)') &
                    rcio, iline, trim(text_input_file)
      call error_handler(E_ERR,'main', string1, source, revision, revdate)
   endif

   ! parse the line into the cosmos structure (including the observation time)
   call stringparse(input_line, iline)

   if (iline <= 2) then
      write(*,*)''
      write(*,*)'Check of the first observation: (column,string,value)'
      write(*,*)cosmos%dateindex, cosmos%datestring, cosmos%year, cosmos%month, cosmos%day
      write(*,*)cosmos%timeindex, cosmos%timestring, cosmos%hour, cosmos%minute
      write(*,*)cosmos%neutronindex    , cosmos%neutronstring     , cosmos%neutron
      write(*,*)cosmos%neutronstdindex , cosmos%neutronstdstring  , cosmos%neutronstd
      call print_date(cosmos%time_obs, 'observation date is')
      call print_time(cosmos%time_obs, 'observation time is')
   end if

   if (verbose) call print_date(cosmos%time_obs, 'obs time is')

   call get_time(cosmos%time_obs, osec, oday)

   ! make an obs derived type, and then add it to the sequence
   ! If the QC value is good, use the observation.
   ! Increasingly larger QC values are more questionable quality data.
   ! Quality Control Flags by Rafael Rosolem (rosolem@email.arizona.edu)
   ! QC flags define as: 0 = BEST     (corrected for water vapor)
   !                     1 = OK       (NOT corrected for water vapor)
   !                     2 = BAD      (not reliable/consistent)
   !                     3 = MISSING  (missing data, i.e. -9999)

   if (cosmos%qc <= maxgoodqc) then   ! COSMOS neutron counts
      oerr = cosmos%neutronstd
      qc   = real(cosmos%qc,r8)

      call set_cosmos_metadata(obsindx, bd, lattwat, N, alpha, L1, L2, L3, L4) 

      call create_3d_obs(cosmos_metadata(siteIndx)%latitude, &
                         cosmos_metadata(siteIndx)%longitude, 0.0_r8, VERTISSURFACE, &
           cosmos%neutron, COSMOS_NEUTRON_INTENSITY, oerr, oday, osec, qc, obsindx, obs)

      call add_obs_to_seq(obs_seq, obs, cosmos%time_obs, prev_obs, prev_time, first_obs)
   endif

end do obsloop

! if we added any obs to the sequence, write it out to a file now.
if ( get_num_obs(obs_seq) > 0 ) then
   if (verbose) print *, 'writing obs_seq, obs_count = ', get_num_obs(obs_seq)
   call write_obs_seq(obs_seq, obs_out_file)
endif

! end of main program
call finalize_utilities()

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   create_3d_obs - subroutine that is used to create an observation
!                   type from observation data.  
!
!   NOTE: assumes the code is using the threed_sphere locations module, 
!         that the observation has a single data value and a single
!         qc value.
!
!    lat   - latitude of observation
!    lon   - longitude of observation
!    vval  - vertical coordinate
!    vkind - kind of vertical coordinate (pressure, level, etc)
!    obsv  - observation value
!    okind - observation kind
!    oerr  - observation error
!    day   - gregorian day
!    sec   - gregorian second
!    qc    - quality control value
!    key   - index to metadata in obs_def_COSMOS_mod arrays
!    obs   - observation type
!
!    extended from the observations/utilities/obs_utilities_mod.f90 v 5601
!    to support the extra metadata -- TJH
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine create_3d_obs(lat, lon, vval, vkind, obsv, okind, oerr, day, sec, qc, key, obs)
integer,        intent(in)    :: okind, vkind, day, sec
real(r8),       intent(in)    :: lat, lon, vval, obsv, oerr, qc
integer,        intent(in)    :: key
type(obs_type), intent(inout) :: obs

real(r8)              :: obs_val(1), qc_val(1)
type(obs_def_type)    :: obs_def

call set_obs_def_location(obs_def, set_location(lon, lat, vval, vkind))
call set_obs_def_type_of_obs(obs_def, okind)
call set_obs_def_time(obs_def, set_time(sec, day))
call set_obs_def_error_variance(obs_def, oerr * oerr)
call set_obs_def_key(obs_def, key)
call set_obs_def(obs, obs_def)

obs_val(1) = obsv
call set_obs_values(obs, obs_val)
qc_val(1)  = qc
call set_qc(obs, qc_val)

end subroutine create_3d_obs




subroutine decode_header(iunit)
! Reads the first line of the header and parses the information.
! I should break the line into words and match which word with each
! desired string. But not today ...
! FIXME ... decode the header ... do not assume ... 
!    Ameriflux/level_4_to_obs.f90 is a good place to start. 
!    Must count whitespace gaps instead of commas for word delineation.
!
!YYYY-MM-DD HH:MM  MOD PROBE PRESS  SCALE SANPE INTEN OTHER CORR ERR
!2010-06-02 19:12 2555 1.000 0.990 02.066 2.486 1.030 1.000 2954 058
!2010-06-02 20:13 2593 1.000 0.987 02.066 2.486 1.030 1.000 2989 058

integer, intent(in) :: iunit

read(iunit,'(A)',iostat=rcio) input_line
if (rcio /= 0) then
  write(string1,*)'Cannot parse header. Begins <',trim(input_line(1:40)),'>'
  call error_handler(E_ERR,'decode_header',string1, source, revision, revdate)
endif

call error_handler(E_MSG,'decode_header','hardcoding values for now ... dangerous', &
                     source, revision, revdate)

cosmos%dateindex       = 1
cosmos%timeindex       = 2
cosmos%neutronindex    = 10
cosmos%neutronstdindex = 11

end subroutine decode_header



subroutine stringparse(str1,linenum)
! The first two items are character strings, the rest can be read as reals.

character(len=*), intent(in) :: str1
integer         , intent(in) :: linenum

character(len=10) :: yyyymmdd
character(len=5)  :: tod
real(r8), dimension(9) :: values

values = MISSING_R8

read(str1,*,iostat=rcio) yyyymmdd,tod,values
if (rcio /= 0) then
  write(string1,*)'Cannot parse line',linenum,'. Begins <',trim(str1(1:30)),'>'
  call error_handler(E_ERR,'stringparse',string1, source, revision, revdate)
endif

read(yyyymmdd,'(i4,1x,i2,1x,i2)',iostat=rcio) cosmos%year, cosmos%month, cosmos%day
if (rcio /= 0) then
  write(string1,*)'Cannot parse yyyymmdd on line ',linenum,' from <',yyyymmdd,'>'
  call error_handler(E_ERR,'stringparse',string1, source, revision, revdate)
endif

read(     tod,'(      i2,1x,i2)',iostat=rcio) cosmos%hour, cosmos%minute
if (rcio /= 0) then
  write(string1,*)'Cannot parse tod on line ',linenum,' from <',tod,'>'
  call error_handler(E_ERR,'stringparse',string1, source, revision, revdate)
endif

! Stuff what we want into the cosmos structure

cosmos%qc         = LEVEL2QC                           ! for all level 2 data
cosmos%neutron    = values(cosmos%neutronindex    - 2) ! minus the first two words
cosmos%neutronstd = values(cosmos%neutronstdindex - 2) ! minus the first two words
cosmos%time_obs   = set_date(cosmos%year, cosmos%month, cosmos%day, cosmos%hour, cosmos%minute, 0)

end subroutine stringparse




  function read_site_metadata(site_metadata_file)
!----------------------------------------------------------------------------
! nsites = read_site_metadata(site_metadata_file)
!
! Read the list of parameters for every site we know about and
! return the number of sites we know about.

integer                      :: read_site_metadata
character(len=*), intent(in) :: site_metadata_file

integer                               :: strlength
integer                               :: ncid, isite, VarID
integer, dimension(NF90_MAX_VAR_DIMS) :: dimIDs
character(len=metadatalength)         :: sitename

! Check to make sure the required parameter file exists

if ( .not. file_exist(site_metadata_file) ) then
   write(string1,*) 'COSMIC parameter file [', trim(site_metadata_file),'] does not exist.'
   call error_handler(E_ERR,'read_site_metadata',string1,source,revision,revdate)
endif

call nc_check(nf90_open(site_metadata_file, NF90_NOWRITE, ncid), &
                   'read_site_metadata', 'open '//trim(site_metadata_file))

call nc_check(nf90_inq_dimid(ncid, 'nsites', dimIDs(1)), &
                  'read_site_metadata','inq_dimid nsites '//trim(site_metadata_file))
call nc_check(nf90_inquire_dimension(ncid, dimIDs(1), len=read_site_metadata), &
                  'read_site_metadata','inquire_dimension nsites '//trim(site_metadata_file))

call nc_check(nf90_inq_dimid(ncid, 'strlength', dimIDs(2)), &
                  'read_site_metadata','inq_dimid strlength '//trim(site_metadata_file))
call nc_check(nf90_inquire_dimension(ncid, dimIDs(2), len=strlength), &
                  'read_site_metadata','inquire_dimension strlength '//trim(site_metadata_file))

call nc_check(nf90_inq_varid(ncid, 'sitenames', VarID), &
                 'read_site_metadata','inq_varid sitenames '//trim(site_metadata_file))

allocate(cosmos_metadata(read_site_metadata))

do isite = 1,read_site_metadata

   sitename = ''

   write(string1,*)'get_var sitename site ',isite,' '//trim(site_metadata_file)
   call nc_check(nf90_get_var(ncid, VarID, sitename(1:strlength), &
           start=(/1, isite/), count=(/strlength,1/)), 'read_site_metadata', string1)

   cosmos_metadata(isite)%sitename = trim(sitename(1:strlength))

   call fill_site(ncid, isite)

enddo

call nc_check(nf90_close(ncid), 'read_site_metadata', 'close '//trim(site_metadata_file))

if (verbose) then
do isite = 1,read_site_metadata
   write(*,*)
   write(*,*)'site name ',cosmos_metadata(isite)%sitename
   write(*,*)'longitude ',cosmos_metadata(isite)%longitude 
   write(*,*)'latitude  ',cosmos_metadata(isite)%latitude
   write(*,*)'elevation ',cosmos_metadata(isite)%elevation
   write(*,*)'bd        ',cosmos_metadata(isite)%bd
   write(*,*)'lattwat   ',cosmos_metadata(isite)%lattwat
   write(*,*)'N         ',cosmos_metadata(isite)%N
   write(*,*)'alpha     ',cosmos_metadata(isite)%alpha
   write(*,*)'L1        ',cosmos_metadata(isite)%L1
   write(*,*)'L2        ',cosmos_metadata(isite)%L2
   write(*,*)'L3        ',cosmos_metadata(isite)%L3
   write(*,*)'L4        ',cosmos_metadata(isite)%L4
   write(*,*)
enddo
endif

end function read_site_metadata



subroutine fill_site(ncid, siteindx)

integer,  intent(in) :: ncid, siteindx

call get_var(ncid, siteindx, 'longitude', cosmos_metadata(siteindx)%longitude)
call get_var(ncid, siteindx, 'latitude' , cosmos_metadata(siteindx)%latitude )
call get_var(ncid, siteindx, 'elevation', cosmos_metadata(siteindx)%elevation)
call get_var(ncid, siteindx, 'bd'       , cosmos_metadata(siteindx)%bd       )
call get_var(ncid, siteindx, 'lattwat'  , cosmos_metadata(siteindx)%lattwat  )
call get_var(ncid, siteindx, 'N'        , cosmos_metadata(siteindx)%N        )
call get_var(ncid, siteindx, 'alpha'    , cosmos_metadata(siteindx)%alpha    )
call get_var(ncid, siteindx, 'L1'       , cosmos_metadata(siteindx)%L1       )
call get_var(ncid, siteindx, 'L2'       , cosmos_metadata(siteindx)%L2       )
call get_var(ncid, siteindx, 'L3'       , cosmos_metadata(siteindx)%L3       )
call get_var(ncid, siteindx, 'L4'       , cosmos_metadata(siteindx)%L4       )

! Ensure that longitudes are [0, 360)
if (cosmos_metadata(siteindx)%longitude < 0.0_r8) &
    cosmos_metadata(siteindx)%longitude = cosmos_metadata(siteindx)%longitude + 360.0_r8
if (cosmos_metadata(siteindx)%longitude == 360.0_r8) &
    cosmos_metadata(siteindx)%longitude = 0.0_r8

cosmos_metadata(siteindx)%location = set_location(cosmos_metadata(siteindx)%longitude, &
                                                  cosmos_metadata(siteindx)%latitude,  &
                                                  cosmos_metadata(siteindx)%elevation, &
                                                  VERTISSURFACE)
end subroutine fill_site



subroutine get_var(ncid, siteindx, varname, slot)

integer,          intent(in)  :: ncid, siteindx
character(len=*), intent(in)  :: varname
real(r8),         intent(out) :: slot

real(r8), dimension(1) :: tempspace
integer :: VarID, start(1), count(1)

start(1) = siteindx
count(1) = 1

call nc_check(nf90_inq_varid(ncid, varname, VarID), 'read_site_metadata', 'inq_varid '//trim(varname))
call nc_check(nf90_get_var(ncid, VarID, tempspace, start=start, count=count ), &
                     'read_site_metadata', 'get_var '//trim(varname))
slot = tempspace(1)

end subroutine get_var



    function find_site_index(sitename)
!-------------------------------------------------------------------------------
! siteIndx = find_site_index(sitename)

integer                      :: find_site_index
character(len=*), intent(in) :: sitename

integer :: i, siteID
character(len=metadatalength) :: testsite, insite

insite = trim(sitename)
call to_upper(insite) ! convert to upper case - in place

siteID = 0

SITENAMES : do i = 1,nSites

   testsite = trim(cosmos_metadata(i)%sitename)
   call to_upper(testsite)

   if (testsite == insite) then
      siteID = i
      exit SITENAMES
   endif

enddo SITENAMES

if (siteID == 0) then
   string1 = 'Unable to find case-insensitive match for site <'//trim(sitename)//'>'
   string2 = 'Make sure '//trim(site_metadata_file)//' has metadata for this site. Run:'
   string3 = 'ncdump -v sitenames '//trim(site_metadata_file)
   call error_handler(E_ERR,'find_site_index', string1, source, revision, revdate, &
                     text2=string2,text3=string3)
endif

if (verbose) then
   write(*,*)
   write(*,*)'site <'//trim(cosmos_metadata(siteID)%sitename)//'> is number ',siteID
   write(*,*)'longitude ',cosmos_metadata(siteID)%longitude 
   write(*,*)'latitude  ',cosmos_metadata(siteID)%latitude
   write(*,*)'elevation ',cosmos_metadata(siteID)%elevation
   write(*,*)'bd        ',cosmos_metadata(siteID)%bd
   write(*,*)'lattwat   ',cosmos_metadata(siteID)%lattwat
   write(*,*)'N         ',cosmos_metadata(siteID)%N
   write(*,*)'alpha     ',cosmos_metadata(siteID)%alpha
   write(*,*)'L1        ',cosmos_metadata(siteID)%L1
   write(*,*)'L2        ',cosmos_metadata(siteID)%L2
   write(*,*)'L3        ',cosmos_metadata(siteID)%L3
   write(*,*)'L4        ',cosmos_metadata(siteID)%L4
   write(*,*)
endif

find_site_index = siteID

end function find_site_index



end program COSMOS_to_obs

! <next few lines under version control, do not edit>
! $URL$
! $Id$
! $Revision$
! $Date$
