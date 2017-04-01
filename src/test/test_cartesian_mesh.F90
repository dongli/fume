program test_cartesian_mesh

  use cartesian_domain_mod
  use cartesian_mesh_mod
  use fut

  implicit none

  call fut_test_case_init()

  call fut_test_case_create('cartesian_mesh')

  call cartesian_domain_init(2, [0.0, 1.0], [BT_PERIODIC, BT_PERIODIC], [0.0, 1.0], [BT_PERIODIC, BT_PERIODIC])
  call cartesian_mesh_init(full_grid=[10, 10], half_grid=[10, 10], halo_width=[1, 1])

  call fut_assert_equal(lbound(mesh%x, 1), 0)

  call fut_test_case_final()

end program test_cartesian_mesh
