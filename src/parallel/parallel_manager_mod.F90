module parallel_manager_mod

  use mpi
  use commons_mod
  use parallel_process_mod

  implicit none

  private

  public parallel_manager_init
  public parallel_manager_final
  public local_process

  type parallel_manager_type
    integer num_process
    integer, allocatable :: comm(:)
  end type parallel_manager_type

  type(parallel_manager_type) parallel_manager

  type(parallel_process_type), pointer :: local_process
  type(parallel_process_type), allocatable, target :: parallel_processes(:)

contains

  subroutine parallel_manager_init()

    integer ierr, i, rank

    call mpi_init(ierr)

    call mpi_comm_size(MPI_COMM_WORLD, parallel_manager%num_process, ierr)

    call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)

    allocate(parallel_processes(parallel_manager%num_process))
    ! Each process contains information of all other processes.
    do i = 1, parallel_manager%num_process
      parallel_processes(i)%id = i-1
      if (parallel_processes(i)%id == rank) then
        local_process => parallel_processes(i)
      end if
    end do

    ! TODO: Set parallel elements from config file, or other rules.
    call parallel_process_init(local_process, 1)

  end subroutine parallel_manager_init

  subroutine parallel_manager_final()

    integer ierr

    if (allocated(parallel_processes)) deallocate(parallel_processes)

    call mpi_finalize(ierr)

  end subroutine parallel_manager_final

end module parallel_manager_mod