module parallel_element_mod

  use commons_mod

  implicit none

  type cartesian_rect_parallel_element_type
    integer id
    integer nx
    integer ny
    integer nz
    integer start_x_idx(2)
    integer start_y_idx(2)
    integer start_z_idx(2)
    integer halo_width_x
    integer halo_width_y
    integer halo_width_z
  end type cartesian_rect_parallel_element_type

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

  type parallel_element_type
    type(cartesian_rect_parallel_element_type), pointer :: cartesian_rect
    type(lon_lat_parallel_element_type), pointer :: lon_lat
  end type parallel_element_type

contains

  subroutine parallel_element_init(pe)

    type(parallel_element_type), intent(out) :: pe

    nullify(pe%cartesian_rect)
    nullify(pe%lon_lat)
    if (commons%mesh_type == 'cartesian_rect') then
      allocate(pe%cartesian_rect)
    else if (commons%mesh_type == 'lon_lat') then
      allocate(pe%lon_lat)
    end if

  end subroutine parallel_element_init

  subroutine parallel_element_final(pe)

    type(parallel_element_type), intent(inout) :: pe

    if (associated(pe%cartesian_rect)) deallocate(pe%cartesian_rect)
    if (associated(pe%lon_lat)) deallocate(pe%lon_lat)

  end subroutine parallel_element_final

end module parallel_element_mod