module cartesian_rect_mesh_mod

  use commons_mod
  use cartesian_domain_mod

  implicit none

  private

  public cartesian_rect_mesh_init
  public cartesian_rect_mesh_final
  public FULL, HALF
  public mesh

  integer, parameter :: FULL = 1
  integer, parameter :: HALF = 2

  type cartesian_rect_mesh_type
    integer nx(2)
    integer ny(2)
    integer nz(2)
    integer halo_width_x
    integer halo_width_y
    integer halo_width_z
    real dx
    real dy
    real dz
    real, allocatable :: x(:,:) ! Grid X coordinates
    real, allocatable :: y(:,:) ! Grid Y coordinates
    real, allocatable :: z(:,:) ! Grid Z coordinates
  end type cartesian_rect_mesh_type

  type(cartesian_rect_mesh_type) mesh

contains

  subroutine cartesian_rect_mesh_init(full_grid, half_grid, halo_width)

    integer, intent(in) :: full_grid(:)
    integer, intent(in) :: half_grid(:)
    integer, intent(in) :: halo_width(:)

    integer i, j, k

    mesh%nx(FULL) = full_grid(1)
    mesh%nx(HALF) = half_grid(1)
    mesh%halo_width_x = halo_width(1)
    allocate(mesh%x(-mesh%halo_width_x+1:maxval(mesh%nx)+mesh%halo_width_x,2))
    if (domain%axis_x_btype(1) == BT_WALL .and. domain%axis_x_btype(2) == BT_WALL) then
      mesh%dx = (domain%axis_x_range(2) - domain%axis_x_range(1)) / (mesh%nx(FULL)-1)
    else
      mesh%dx = (domain%axis_x_range(2) - domain%axis_x_range(1)) / mesh%nx(FULL)
    end if
    do i = -mesh%halo_width_x+1, mesh%nx(FULL)+mesh%halo_width_x
      mesh%x(i,FULL) = domain%axis_x_range(1) + (i-1) * mesh%dx
      call cartesian_domain_validate_coord(1, mesh%x(i,FULL))
    end do
    do i = -mesh%halo_width_x+1, mesh%nx(HALF)+mesh%halo_width_x
      mesh%x(i,HALF) = domain%axis_x_range(1) + (i-0.5) * mesh%dx
      call cartesian_domain_validate_coord(1, mesh%x(i,HALF))
    end do

    mesh%ny(FULL) = full_grid(2)
    mesh%ny(HALF) = half_grid(2)
    mesh%halo_width_y = halo_width(2)
    allocate(mesh%y(-mesh%halo_width_y+1:maxval(mesh%ny)+mesh%halo_width_y,2))
    if (domain%axis_x_btype(1) == BT_WALL .and. domain%axis_x_btype(2) == BT_WALL) then
      mesh%dy = (domain%axis_y_range(2) - domain%axis_y_range(1)) / (mesh%ny(FULL)-1)
    else
      mesh%dy = (domain%axis_y_range(2) - domain%axis_y_range(1)) / mesh%ny(FULL)
    end if
    do j = -mesh%halo_width_y+1, mesh%ny(FULL)+mesh%halo_width_y
      mesh%y(j,FULL) = domain%axis_y_range(1) + (j-1) * mesh%dy
      call cartesian_domain_validate_coord(2, mesh%y(j,FULL))
    end do
    do j = -mesh%halo_width_y+1, mesh%ny(HALF)+mesh%halo_width_y
      mesh%y(j,HALF) = domain%axis_y_range(1) + (j-0.5) * mesh%dy
      call cartesian_domain_validate_coord(2, mesh%y(j,HALF))
    end do

    if (commons%num_dim == 3) then
      mesh%nz(FULL) = full_grid(3)
      mesh%nz(HALF) = half_grid(3)
      mesh%halo_width_z = halo_width(3)
      allocate(mesh%z(-mesh%halo_width_z+1:maxval(mesh%nz)+mesh%halo_width_z,2))
      if (domain%axis_x_btype(1) == BT_WALL .and. domain%axis_x_btype(2) == BT_WALL) then
        mesh%dz = (domain%axis_z_range(2) - domain%axis_z_range(1)) / (mesh%nz(FULL)-1)
      else
        mesh%dz = (domain%axis_z_range(2) - domain%axis_z_range(1)) / mesh%nz(FULL)
      end if
      do k = -mesh%halo_width_z+1, mesh%nz(FULL)+mesh%halo_width_z
        mesh%z(k,FULL) = domain%axis_z_range(1) + (k-1) * mesh%dz
        call cartesian_domain_validate_coord(3, mesh%z(k,FULL))
      end do
      do k = -mesh%halo_width_z+1, mesh%nz(HALF)+mesh%halo_width_z
        mesh%z(k,HALF) = domain%axis_z_range(1) + (k-0.5) * mesh%dz
        call cartesian_domain_validate_coord(3, mesh%z(k,HALF))
      end do
    end if

  end subroutine cartesian_rect_mesh_init

  subroutine cartesian_rect_mesh_final()

    if (allocated(mesh%x)) deallocate(mesh%x)
    if (allocated(mesh%y)) deallocate(mesh%y)
    if (allocated(mesh%z)) deallocate(mesh%z)

  end subroutine cartesian_rect_mesh_final

end module cartesian_rect_mesh_mod
