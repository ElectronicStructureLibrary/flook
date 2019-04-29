! Copyright (C) 2011-2013 German Research School for Simulation Sciences GmbH,
!                         Aachen and others.
!              2013-2016 University of Siegen
! Please see the COPYRIGHT file in this directory for details.

!> Helping module to define the aot_fun_type without causing dependency locks.
module aot_fun_declaration_module
  use flu_kinds_module, only: long_k

  implicit none

  private

  type aot_fun_type
    integer :: handle = 0
    integer :: arg_count = 0
    integer(kind=long_k) :: id = 0
  end type

  public :: aot_fun_type

end module aot_fun_declaration_module
