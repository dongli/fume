module parallel_process_mod

  use mpi
  use parallel_element_mod

  implicit none

  type parallel_process_type
    integer id
    type(parallel_element_type), allocatable :: parallel_elements(:)
  end type parallel_process_type

contains

  subroutine parallel_process_init(pp, ne)

    type(parallel_process_type), intent(inout) :: pp
    integer, intent(in) :: ne

    integer i

    allocate(pp%parallel_elements(ne))
    do i = 1, ne
      call parallel_element_init(pp%parallel_elements(i))
    end do

  end subroutine parallel_process_init

  subroutine parallel_process_final(pp)

    type(parallel_process_type), intent(inout) :: pp

    if (allocated(pp%parallel_elements)) deallocate(pp%parallel_elements)

  end subroutine parallel_process_final

end module parallel_process_mod