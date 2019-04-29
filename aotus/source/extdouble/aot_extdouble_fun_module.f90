! Copyright (C) 2011-2013 German Research School for Simulation Sciences GmbH,
!              Aachen and others.
! Please see the ../COPYRIGHT file one directory above for details.

!> A module providing extdouble number input to Lua functions
!!
!! Note that Lua actually only handles double precision, and the numbers are
!! converted accordingly. Thus this is merely a convenience interface, to allow
!! the usage of the functions from this module with extdouble precision numbers.
module aot_extdouble_fun_module
  use flu_binding
  use flu_kinds_module, only: double_k
  use aot_extdouble_top_module, only: xdble_k
  use aot_fun_declaration_module, only: aot_fun_type
  use aot_table_module, only: aot_table_from_1Darray

  implicit none

  private

  public :: aot_fun_put

  !> Put an argument into the lua function.
  !!
  !! Arguments have to be in order, first put the first argument then the second
  !! and so on.
  !! Here we add support for extdouble precision numbers
  interface aot_fun_put
    module procedure aot_fun_put_extdouble
    module procedure aot_fun_put_extdouble_v
  end interface aot_fun_put

contains

  !> Put an argument of type extended double into the list of arguments for the
  !! function.
  subroutine aot_fun_put_extdouble(L, fun, arg)
    type(flu_state) :: L !< Handle for the Lua script.

    !> Handle of the function, this argument should be put into.
    type(aot_fun_type) :: fun

    !> Actual argument to hand over to the Lua function.
    real(kind=xdble_k), intent(in) :: arg

    real(kind=double_k) :: locarg

    ! Only do something, if the function is actually properly defined.
    if (fun%handle /= 0) then

      locarg = real(arg, kind=double_k)

      ! If the function was executed before this call, it has to be
      ! reset.
      if (fun%arg_count == -1) then
        ! Set the top of the stack to the reference of the function.
        ! Discarding anything above it.
        call flu_settop(L, fun%handle)
        ! Push a copy of the function itself on the stack again, before
        ! adding arguments, to savely survive popping of the function
        ! upon execution.
        call flu_pushvalue(L, fun%handle)
        ! Increase the argument count to 0 again (really start counting
        ! arguments afterwards.
        fun%arg_count = fun%arg_count+1
      end if

      call flu_pushNumber(L, locarg)
      fun%arg_count = fun%arg_count+1
    end if

  end subroutine aot_fun_put_extdouble


  !> Put an array of extended doubles into the list of arguments for the
  !! function.
  subroutine aot_fun_put_extdouble_v(L, fun, arg)
    type(flu_state) :: L !< Handle for the Lua script.

    !> Handle of the function, this argument should be put into.
    type(aot_fun_type) :: fun

    !> Actual argument to hand over to the Lua function.
    real(kind=xdble_k), intent(in) :: arg(:)

    real(kind=double_k) :: locarg(size(arg))
    integer :: thandle

    ! Only do something, if the function is actually properly defined.
    if (fun%handle /= 0) then

      locarg = real(arg, kind=double_k)

      ! If the function was executed before this call, it has to be
      ! reset.
      if (fun%arg_count == -1) then
        ! Set the top of the stack to the reference of the function.
        ! Discarding anything above it.
        call flu_settop(L, fun%handle)
        ! Push a copy of the function itself on the stack again, before
        ! adding arguments, to savely survive popping of the function
        ! upon execution.
        call flu_pushvalue(L, fun%handle)
        ! Increase the argument count to 0 again (really start counting
        ! arguments afterwards.
        fun%arg_count = fun%arg_count+1
      end if

      call aot_table_from_1Darray(L, thandle, locarg)
      fun%arg_count = fun%arg_count+1
    end if

  end subroutine aot_fun_put_extdouble_v

end module aot_extdouble_fun_module
