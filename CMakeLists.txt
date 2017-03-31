cmake_minimum_required (VERSION 2.8)

project (fume Fortran)

set (CMAKE_Fortran_FLAGS "-fdefault-real-8")

set (src
  "${PROJECT_SOURCE_DIR}/src/commons_mod.F90"
  "${PROJECT_SOURCE_DIR}/src/domain/cartesian_domain_mod.F90"
  "${PROJECT_SOURCE_DIR}/src/mesh/cartesian_mesh_mod.F90"
)

add_library (fume ${src})
