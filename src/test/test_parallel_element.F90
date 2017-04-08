program test_parallel_element

  use parallel_manager_mod
  use parallel_process_mod
  use parallel_element_mod
  use fut

  implicit none

  type(cartesian_rect_parallel_element_type), pointer :: pe

  call fut_test_case_init()

  call fut_test_case_create('parallel_element')

  call parallel_manager_init()

  print *, associated(local_process%parallel_elements(1)%cartesian_rect)
  print *, associated(local_process%parallel_elements(1)%lon_lat)

  pe => local_process%parallel_elements(1)%cartesian_rect
  print *, pe%nx

  print *, local_process%id

  call parallel_manager_final()

  call fut_test_case_final()

end program test_parallel_element