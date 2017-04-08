module lon_lat_parallel_element_mod

  implicit none

  type lon_lat_parallel_element_type
    integer id
    integer nlon
    integer nlat
    integer nlev
    integer start_lon_idx(2)
    integer start_lat_idx(2)
    integer start_lev_idx(2)
    integer halo_width_lon
    integer halo_width_lat
    integer halo_width_lev
  end type lon_lat_parallel_element_type

contains

  subroutine lon_lat_parallel_element_init(pe, id)

    type(lon_lat_parallel_element_type), intent(out) :: pe
    integer, intent(in) :: id

    pe%id = id

  end subroutine lon_lat_parallel_element_init

  subroutine lon_lat_parallel_element_final(pe)

    type(lon_lat_parallel_element_type), intent(inout) :: pe

  end subroutine lon_lat_parallel_element_final

end module lon_lat_parallel_element_mod