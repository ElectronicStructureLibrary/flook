! Copyright (C) 2011-2013 German Research School for Simulation Sciences GmbH,
!              Aachen and others.
! Please see the ../COPYRIGHT file one directory above for details.

module aot_quadruple_top_module
  use flu_binding
  use aot_err_module, only: aoterr_Fatal, aoterr_NonExistent, &
    &                       aoterr_WrongType, aot_err_handler

  implicit none

  private

  public :: aot_top_get_val

  interface aot_top_get_val
    module procedure aot_top_get_quadruple
  end interface

  integer, parameter, public :: quad_k = selected_real_kind(33)

contains

  !> Interpret topmost entry on Lua stack as a quadruple precision real.
  !!
  !! NOTE that numbers provided by Lua are only double precision.
  subroutine aot_top_get_quadruple(val, ErrCode, L, default)
    type(flu_State) :: L !< Handle to the Lua script

    !> Value of the Variable in the script
    real(kind=quad_k), intent(out) :: val

    !> Error code to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Some default value, that should be used, if the variable is not set in
    !! the Lua script.
    real(kind=quad_k), optional, intent(in) :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    if (flu_isNoneOrNil(L, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isNumber(L, -1)) then
        val = real(flu_toDouble(L, -1), kind=quad_k)
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(L)

  end subroutine aot_top_get_quadruple


end module aot_quadruple_top_module
