module cartesian_domain_mod

  use commons_mod

  implicit none

  private

  public cartesian_domain_init
  public cartesian_domain_final
  public BT_OPEN, BT_WALL, BT_PERIODIC
  public domain

  integer, parameter :: BT_OPEN = 1
  integer, parameter :: BT_WALL = 2
  integer, parameter :: BT_PERIODIC = 3

  type cartesian_domain_type
    real :: axis_x_range(2) = [0, 1]
    integer :: axis_x_btype(2) = [BT_OPEN, BT_OPEN]
    real :: axis_y_range(2) = [0, 1]
    integer :: axis_y_btype(2) = [BT_OPEN, BT_OPEN]
    real :: axis_z_range(2) = [0, 1]
    integer :: axis_z_btype(2) = [BT_WALL, BT_OPEN]
  end type cartesian_domain_type

  type(cartesian_domain_type) domain

contains

  subroutine cartesian_domain_init(num_dim, &
      axis_x_range, axis_x_btype, &
      axis_y_range, axis_y_btype, &
      axis_z_range, axis_z_btype)

    integer, intent(in) :: num_dim
    real, intent(in), optional :: axis_x_range(2)
    integer, intent(in), optional :: axis_x_btype(2)
    real, intent(in), optional :: axis_y_range(2)
    integer, intent(in), optional :: axis_y_btype(2)
    real, intent(in), optional :: axis_z_range(2)
    integer, intent(in), optional :: axis_z_btype(2)

    commons%domain_type = 'cartesian'
    commons%num_dim = num_dim

    if (present(axis_x_range)) domain%axis_x_range = axis_x_range
    if (present(axis_x_btype)) domain%axis_x_btype = axis_x_btype
    if (present(axis_y_range)) domain%axis_y_range = axis_y_range
    if (present(axis_y_btype)) domain%axis_y_btype = axis_y_btype
    if (present(axis_z_range)) domain%axis_z_range = axis_z_range
    if (present(axis_z_btype)) domain%axis_z_btype = axis_z_btype

  end subroutine cartesian_domain_init

  subroutine cartesian_domain_final()

  end subroutine cartesian_domain_final

end module cartesian_domain_mod
