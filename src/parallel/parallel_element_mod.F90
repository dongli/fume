module parallel_element_mod

  use commons_mod
  use cartesian_rect_parallel_element_mod
  use lon_lat_parallel_element_mod

  implicit none

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
      call cartesian_rect_parallel_element_init(pe%cartesian_rect)
    else if (commons%mesh_type == 'lon_lat') then
      allocate(pe%lon_lat)
      call lon_lat_parallel_element_init(pe%lon_lat)
    end if

  end subroutine parallel_element_init

  subroutine parallel_element_final(pe)

    type(parallel_element_type), intent(inout) :: pe

    if (associated(pe%cartesian_rect)) deallocate(pe%cartesian_rect)
    if (associated(pe%lon_lat)) deallocate(pe%lon_lat)

  end subroutine parallel_element_final

end module parallel_element_mod