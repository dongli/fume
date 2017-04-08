module commons_mod

  implicit none

  type commons_type
    integer :: num_dim = -1
    character(30) :: domain_type = 'cartesian'
    character(30) :: mesh_type = 'cartesian_rect'
  end type commons_type

  type(commons_type) commons

end module commons_mod
