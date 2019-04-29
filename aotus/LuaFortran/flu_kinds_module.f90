! Copyright (C) 2011-2013 German Research School for Simulation Sciences GmbH,
!                         Aachen and others.
!               2013-2018 University of Siegen
! Please see the COPYRIGHT file in this directory for details.

!> Global definitions of some handy kind declarations
!! with the help of the intrinsic selected_*_kind
!! functions.
module flu_kinds_module
  implicit none

  integer, parameter :: quad_k = selected_real_kind(33)
  integer, parameter :: double_k = selected_real_kind(15)
  integer, parameter :: single_k = selected_real_kind(6)
  integer, parameter :: int_k = selected_int_kind(6)
  integer, parameter :: long_k = selected_int_kind(15)

end module flu_kinds_module
