program test_cartesian_mesh

  use cartesian_domain_mod
  use cartesian_mesh_mod
  use fut

  implicit none

  integer i, j

  call fut_test_case_init()

  call fut_test_case_create('cartesian_mesh')

  call cartesian_domain_init(2, [0.0, 1.0], [BT_PERIODIC, BT_PERIODIC], [0.0, 1.0], [BT_PERIODIC, BT_PERIODIC])
  call cartesian_mesh_init(full_grid=[10, 10], half_grid=[10, 10], halo_width=[1, 1])

  call fut_assert_equal(lbound(mesh%x, 1), 0)
  call fut_assert_equal(ubound(mesh%x, 1), 11)
  call fut_assert_equal(lbound(mesh%x, 2), FULL)
  call fut_assert_equal(ubound(mesh%x, 2), HALF)
  call fut_assert_equal(lbound(mesh%y, 1), 0)
  call fut_assert_equal(ubound(mesh%y, 1), 11)
  call fut_assert_equal(lbound(mesh%y, 2), FULL)
  call fut_assert_equal(ubound(mesh%y, 2), HALF)

  call fut_assert_approximate(mesh%x(0,FULL), 0.9)
  do i = 1, 10
    call fut_assert_approximate(mesh%x(i,FULL), 0.1*(i-1))
  end do
  call fut_assert_approximate(mesh%x(11,FULL), 1.0)

  call fut_assert_approximate(mesh%y(0,FULL), 0.9)
  do j = 1, 10
    call fut_assert_approximate(mesh%y(j,FULL), 0.1*(j-1))
  end do
  call fut_assert_approximate(mesh%y(11,FULL), 1.0)

  call fut_test_case_final()

end program test_cartesian_mesh
