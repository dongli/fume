module cartesian_mesh_mod

  use commons_mod
  use cartesian_domain_mod

  implicit none

  private

  public cartesian_mesh_init
  public cartesian_mesh_final
  public FULL, HALF
  public mesh

  integer, parameter :: FULL = 1
  integer, parameter :: HALF = 2

  type cartesian_mesh_type
    integer, public :: nx(2)
    integer, public :: ny(2)
    integer, public :: nz(2)
    real dx
    real dy
    real dz
    real, public, allocatable :: x(:,:) ! Grid X coordinates
    real, public, allocatable :: y(:,:) ! Grid Y coordinates
    real, public, allocatable :: z(:,:) ! Grid Z coordinates
  end type cartesian_mesh_type

  type(cartesian_mesh_type) mesh

contains

  subroutine cartesian_mesh_init(nx, ny, nz)

    integer, optional, intent(in) :: nx(2)
    integer, optional, intent(in) :: ny(2)
    integer, optional, intent(in) :: nz(2)

    integer i, j, k

    if (present(nx)) then
      mesh%nx = nx
      allocate(mesh%x(maxval(nx),2))
      if (domain%axis_x_btype(1) == BT_PERIODIC .and. domain%axis_x_btype(2) == BT_PERIODIC) then
        mesh%dx = (domain%axis_x_range(2) - domain%axis_x_range(1)) / nx(FULL)
      else if (domain%axis_x_btype(1) == BT_WALL .and. domain%axis_x_btype(2) == BT_WALL) then
        mesh%dx = (domain%axis_x_range(2) - domain%axis_x_range(1)) / (nx(FULL) - 1)
      end if
      do i = 1, mesh%nx(FULL)
        mesh%x(i,FULL) = (i - 1) * mesh%dx
      end do
      do i = 1, mesh%nx(HALF)
        mesh%x(i,HALF) = (i - 0.5) * mesh%dx
      end do
    end if
    if (present(ny)) then
      mesh%ny = ny
      allocate(mesh%y(maxval(ny),2))
      if (domain%axis_y_btype(1) == BT_PERIODIC .and. domain%axis_y_btype(2) == BT_PERIODIC) then
        mesh%dy = (domain%axis_y_range(2) - domain%axis_y_range(1)) / ny(FULL)
      else if (domain%axis_x_btype(1) == BT_WALL .and. domain%axis_x_btype(2) == BT_WALL) then
        mesh%dy = (domain%axis_y_range(2) - domain%axis_y_range(1)) / (ny(FULL) - 1)
      end if
      do j = 1, mesh%ny(FULL)
        mesh%y(i,FULL) = (j - 1) * mesh%dy
      end do
      do j = 1, mesh%ny(HALF)
        mesh%y(i,HALF) = (j - 0.5) * mesh%dy
      end do
    end if
    if (present(nz)) then
      mesh%nz = nz
      allocate(mesh%z(maxval(nz),2))
      if (domain%axis_z_btype(1) == BT_PERIODIC .and. domain%axis_z_btype(2) == BT_PERIODIC) then
        mesh%dz = (domain%axis_z_range(2) - domain%axis_z_range(1)) / nz(FULL)
      else if (domain%axis_x_btype(1) == BT_WALL .and. domain%axis_x_btype(2) == BT_WALL) then
        mesh%dz = (domain%axis_z_range(2) - domain%axis_z_range(1)) / (nz(FULL) - 1)
      end if
      do j = 1, mesh%nz(FULL)
        mesh%z(i,FULL) = (j - 1) * mesh%dz
      end do
      do j = 1, mesh%nz(HALF)
        mesh%z(i,HALF) = (j - 0.5) * mesh%dz
      end do
    end if

  end subroutine cartesian_mesh_init

  subroutine cartesian_mesh_final()

    if (allocated(mesh%x)) deallocate(mesh%x)
    if (allocated(mesh%y)) deallocate(mesh%y)
    if (allocated(mesh%z)) deallocate(mesh%z)

  end subroutine cartesian_mesh_final

end module cartesian_mesh_mod
