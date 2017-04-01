module cartesian_domain_mod

  use commons_mod

  implicit none

  private

  public cartesian_domain_init
  public cartesian_domain_final
  public cartesian_domain_validate_coord
  public BT_OPEN, BT_WALL, BT_PERIODIC
  public domain

  integer, parameter :: BT_OPEN = 1
  integer, parameter :: BT_WALL = 2
  integer, parameter :: BT_PERIODIC = 3

  type cartesian_domain_type
    real :: axis_x_range(2) = [0, 1]
    real :: axis_x_span = 1
    integer :: axis_x_btype(2) = [BT_OPEN, BT_OPEN]
    real :: axis_y_range(2) = [0, 1]
    real :: axis_y_span = 1
    integer :: axis_y_btype(2) = [BT_OPEN, BT_OPEN]
    real :: axis_z_range(2) = [0, 1]
    real :: axis_z_span = 1
    integer :: axis_z_btype(2) = [BT_WALL, BT_OPEN]
  end type cartesian_domain_type

  type(cartesian_domain_type) domain

  interface cartesian_domain_validate_coord
    module procedure cartesian_domain_validate_coord_component
    module procedure cartesian_domain_validate_coord_vector
  end interface cartesian_domain_validate_coord

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

    if (present(axis_x_range)) then
      domain%axis_x_range = axis_x_range
      domain%axis_x_span = axis_x_range(2) - axis_x_range(1)
    end if
    if (present(axis_x_btype)) domain%axis_x_btype = axis_x_btype
    if (present(axis_y_range)) then
      domain%axis_y_range = axis_y_range
      domain%axis_y_span = axis_y_range(2) - axis_y_range(1)
    end if
    if (present(axis_y_btype)) domain%axis_y_btype = axis_y_btype
    if (present(axis_z_range)) then
      domain%axis_z_range = axis_z_range
      domain%axis_z_span = axis_z_range(2) - axis_z_range(1)
    end if
    if (present(axis_z_btype)) domain%axis_z_btype = axis_z_btype

  end subroutine cartesian_domain_init

  subroutine cartesian_domain_final()

  end subroutine cartesian_domain_final

  subroutine cartesian_domain_validate_coord_component(axis, x)

    integer, intent(in) :: axis
    real, intent(inout) :: x

    if (axis == 1) then
      if (x < domain%axis_x_range(1)) then
        if (domain%axis_x_btype(1) == BT_PERIODIC .and. domain%axis_x_btype(2) == BT_PERIODIC) then
          x = domain%axis_x_range(2) - mod(domain%axis_x_range(1)-x, domain%axis_x_span)
        else if (domain%axis_x_btype(1) == BT_WALL) then
          ! Handle out-of-range coordinate.
        end if
      end if
      if (x > domain%axis_x_range(2)) then
        if (domain%axis_x_btype(1) == BT_PERIODIC .and. domain%axis_x_btype(2) == BT_PERIODIC) then
          x = domain%axis_x_range(1) + mod(x-domain%axis_x_range(2), domain%axis_x_span)
        else if (domain%axis_x_btype(2) == BT_WALL) then
          ! Handle out-of-range coordinate.
        end if
      end if
    else if (axis == 2) then
      if (x < domain%axis_y_range(1)) then
        if (domain%axis_y_btype(1) == BT_PERIODIC .and. domain%axis_y_btype(2) == BT_PERIODIC) then
          x = domain%axis_y_range(2) - mod(domain%axis_y_range(1)-x, domain%axis_y_span)
        else if (domain%axis_y_btype(1) == BT_WALL) then
          ! Handle out-of-range coordinate.
        end if
      end if
      if (x > domain%axis_y_range(2)) then
        if (domain%axis_y_btype(1) == BT_PERIODIC .and. domain%axis_y_btype(2) == BT_PERIODIC) then
          x = domain%axis_y_range(1) + mod(x-domain%axis_y_range(2), domain%axis_y_span)
        else if (domain%axis_y_btype(2) == BT_WALL) then
          ! Handle out-of-range coordinate.
        end if
      end if
    else if (axis == 3) then
      if (x < domain%axis_z_range(1)) then
        if (domain%axis_z_btype(1) == BT_PERIODIC .and. domain%axis_z_btype(2) == BT_PERIODIC) then
          x = domain%axis_z_range(2) - mod(domain%axis_z_range(1)-x, domain%axis_z_span)
        else if (domain%axis_z_btype(1) == BT_WALL) then
          ! Handle out-of-range coordinate.
        end if
      end if
      if (x > domain%axis_z_range(2)) then
        if (domain%axis_z_btype(1) == BT_PERIODIC .and. domain%axis_z_btype(2) == BT_PERIODIC) then
          x = domain%axis_z_range(1) + mod(x-domain%axis_z_range(2), domain%axis_z_span)
        else if (domain%axis_z_btype(2) == BT_WALL) then
          ! Handle out-of-range coordinate.
        end if
      end if
    end if

  end subroutine cartesian_domain_validate_coord_component

  subroutine cartesian_domain_validate_coord_vector(x)

    real, intent(inout) :: x(:)

    call cartesian_domain_validate_coord_component(1, x(1))
    call cartesian_domain_validate_coord_component(2, x(2))
    if (commons%num_dim == 3) then
      call cartesian_domain_validate_coord_component(3, x(3))
    end if

  end subroutine cartesian_domain_validate_coord_vector

end module cartesian_domain_mod
