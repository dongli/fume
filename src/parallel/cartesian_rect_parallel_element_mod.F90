module cartesian_rect_parallel_element_mod

  use cartesian_rect_mesh_mod

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

contains

  subroutine cartesian_rect_parallel_element_init(pe, id)

    type(cartesian_rect_parallel_element_type), intent(out) :: pe
    integer, intent(in) :: id

    pe%id = id

  end subroutine cartesian_rect_parallel_element_init

  subroutine cartesian_rect_parallel_element_final(pe)

    type(cartesian_rect_parallel_element_type), intent(inout) :: pe

  end subroutine cartesian_rect_parallel_element_final

end module cartesian_rect_parallel_element_mod