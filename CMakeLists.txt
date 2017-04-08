cmake_minimum_required (VERSION 2.8)

project (fume Fortran)

set (CMAKE_Fortran_FLAGS "-fdefault-real-8")

add_subdirectory ("lib/fut")
include_directories ("${CMAKE_BINARY_DIR}/lib/fut")

set (src
  "src/commons_mod.F90"
  "src/domain/cartesian_domain_mod.F90"
  "src/mesh/cartesian_rect_mesh_mod.F90"
  "src/parallel/parallel_element_mod.F90"
  "src/parallel/parallel_process_mod.F90"
  "src/parallel/parallel_manager_mod.F90"
  "src/cartesian_fume.F90"
)

add_library (fume ${src})

# Tests
add_executable (test_cartesian_rect_mesh "src/test/test_cartesian_rect_mesh.F90")
target_link_libraries (test_cartesian_rect_mesh fut fume)

add_executable (test_parallel_element "src/test/test_parallel_element.F90")
target_link_libraries (test_parallel_element fut fume)