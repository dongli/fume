module parallel_process_mod

  use mpi
  use parallel_element_mod

  implicit none

  private

  public parallel_process_init
  public parallel_process_final
  public parallel_process_type

  type parallel_process_type
    integer id
    integer num_element
    type(parallel_element_type), allocatable :: parallel_elements(:)
  end type parallel_process_type

contains

  subroutine parallel_process_init(pp, num_element)

    type(parallel_process_type), intent(inout) :: pp
    integer, intent(in) :: num_element

    integer i

    pp%num_element = num_element
    allocate(pp%parallel_elements(num_element))
    do i = 1, num_element
      call parallel_element_init(pp%parallel_elements(i), id=i)
    end do

  end subroutine parallel_process_init

  subroutine parallel_process_final(pp)

    type(parallel_process_type), intent(inout) :: pp

    if (allocated(pp%parallel_elements)) deallocate(pp%parallel_elements)

  end subroutine parallel_process_final

end module parallel_process_mod