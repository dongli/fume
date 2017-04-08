program test_parallel_element

  use cartesian_domain_mod
  use cartesian_rect_mesh_mod
  use parallel_manager_mod
  use parallel_process_mod
  use parallel_element_mod
  use fut

  implicit none

  type(parallel_process_type), pointer :: pp
  type(cartesian_rect_parallel_element_type), pointer :: pe

  call fut_test_case_init()

  call fut_test_case_create('parallel_element')

  call cartesian_domain_init(2, [0.0, 1.0], [BT_PERIODIC, BT_PERIODIC], [0.0, 1.0], [BT_PERIODIC, BT_PERIODIC])
  call cartesian_rect_mesh_init(full_grid=[10, 10], half_grid=[10, 10], halo_width=[1, 1])

  call parallel_manager_init()

  pp => parallel_manager%local_process
  pe => pp%parallel_elements(1)%cartesian_rect

  call fut_assert_true(associated(pp%parallel_elements(1)%cartesian_rect))
  call fut_assert_false(associated(pp%parallel_elements(1)%lon_lat))
  print *, pe%nx

  print *, pp%id

  call parallel_manager_final()

  call fut_test_case_final()

end program test_parallel_element