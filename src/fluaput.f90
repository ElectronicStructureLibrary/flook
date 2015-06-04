

!> Accomodates a generic interface for mingling data structures between an
!! embedded @lua library and @f.
!! In particular one can insert @f calls in LUA and return
!! information to @lua scripts.

!> A generic and simple interface for running scripts in @f via a @lua
!! channel.
!!
!! The library opens an embedded @lua state and enables the user to call
!! @lua code at certains points in the @f pipeline.
!! The interface enables direct interaction through 

!> @author 
!!  Nick Papior Andersen

!> @copyright LGPL

module fluaput

  ! Required libraries (which should be fully
  ! separated to the user)
  use, intrinsic :: iso_c_binding
  use flu_binding, fl_ptr2state => flu_copyptr

  ! Pre-load all aotus modules
  use aot_table_module
  use aotus_module

  implicit none

  ! We denote everything as private as we then force
  ! exposure by this library.
  private

  ! Default variables for passing float and double
  integer, parameter :: i4b = selected_int_kind(r=9) !< Internal precision
  integer, parameter :: i8b = selected_int_kind(r=18) !< Internal precision
  integer, parameter :: r4b = selected_real_kind(p=6) !< Internal precision
  integer, parameter :: r8b = selected_real_kind(p=15) !< Internal precision

  ! Create information about the lua-state
  !> \fn fl_state
  !! @lua handle for a current state
  !!
  !! Pointer to the @lua handle which exposes the 
  !! @lua library to the user.
  !! This type is the handle for everything performed by @lib
  type, extends(flu_State) :: fl_state
     contains
       procedure, pass :: fl_state_init_
       procedure, pass :: fl_state_init_ptr_
       generic :: init => fl_state_init_, fl_state_init_ptr_
  end type fl_state
  public :: fl_state

  !> Retrieval of states from @lua called @f functions
  !!
  !! The @f routines are passed a pointer to the current top  !! of the @lua stack.
  !! This is required to modify the @lua stack inside @f.
  !! This function translates a @lua stack pointer to a
  !! @f #fl_state.
  !!
  !! \param[in] ptr C-pointer to @lua stack
  !! \return a @f state handle #fl_state
  public :: fl_ptr2state

  public :: fl_state_init
  !> See #fl_state_end.
  !!
  !! See #fl_state_end.
  interface fl_state_quit
     module procedure fl_state_end
  end interface fl_state_quit
  !> See #fl_state_end.
  !!
  !! See #fl_state_end.
  interface fl_state_close
     module procedure fl_state_end
  end interface fl_state_close
  public :: fl_state_quit, fl_state_close, fl_state_end

  !> Open/create a @lua table.
  !!
  !! Open or creates a @lua table to be post-processed in
  !! @f.
  !! 
  !! This function returns a handle for a table within the 
  !! `parent` table, or if the `parent` has not been passed
  !! within the global scope.
  !!
  !! If you supply a `key` it will try and open the table
  !! at that respective key. If the key already has a
  !! table it will open that and interact with that table,
  !! else it will create a new table and create the it in
  !! the key.
  !!
  !! The key can either be a string or an integer
  !! specifying an indexable position or hashable position of
  !! the table.
  !!
  !! Note that the table handle is an integer and per-see only
  !! has meaning for the @lua library.
  !!
  !! \param[inout] state A @lua state
  !! \param[in] parent A parent table handle
  !! \param[in] key The key which is queried
  !! \return a table handle 
  interface fl_tbl
     module procedure fl_tbl_none, fl_tbl_str, fl_tbl_int
  end interface fl_tbl
  public :: fl_tbl

  !> Stores values in @lua tables.
  !!
  !! Stores @f arrays in @lua tables by direct indices
  !! and bounds.
  !! It preserves any bounds found by the passed array
  !! and expresses that in the equivalent @lua table.
  !!
  !! The @lua table passed can be expressed in two different
  !! methods:
  !! - __parent__, __key__ based, where an initial #fl_tbl
  !!   is called to create, or retrieve a table by key before
  !!   storing the array in that table.
  !!   
  !!   __NOTE__: Currently this is only implemented using a character `key`.
  !! - __handle__ based, an existing table has been created by the user
  !!   and subsequent storage is directly in this table.
  !!   This is handy if you want to assign other information subsequently.
  !!
  !! \param[inout] state A @lua state
  !! \param NOT-INP See the above explanation for details.
  !! \param[in] val The array to be stored, either 1D or 2D
  interface fl_tbl_set
!     module procedure fl_tbl_set_i_1d, fl_tbl_set_i_2d
!     module procedure fl_tbl_set_l_1d, fl_tbl_set_l_2d
!     module procedure fl_tbl_set_s_1d, fl_tbl_set_s_2d
     module procedure fl_tbl_set_d_1d, fl_tbl_set_d_2d
     module procedure fl_tbl_open_set_i_0d
     module procedure fl_tbl_open_set_d_0d, fl_tbl_open_set_d_1d, fl_tbl_open_set_d_2d
  end interface fl_tbl_set
  public :: fl_tbl_set

  !> Retrieves values from @lua tables.
  !!
  !! Retrieves values from integer indexed @lua tables
  !! by direct bounds searching in the @f array.
  !! 
  !! __Note__:
  !! The @lua table _has_ to pre-exist __and__ all
  !! indices within the bounds must exist in the
  !! table.
  !!
  !! \param[inout] state A @lua state
  !! \param[in] handle A table handle
  !! \param[out] val The array to be retrieved
  interface fl_tbl_get
!     module procedure fl_tbl_get_i_1d, fl_tbl_get_i_2d
!     module procedure fl_tbl_get_l_1d, fl_tbl_get_l_2d
!     module procedure fl_tbl_get_s_1d, fl_tbl_get_s_2d
     module procedure fl_tbl_get_d_1d, fl_tbl_get_d_2d
     module procedure fl_tbl_open_get_i_0d
     module procedure fl_tbl_open_get_d_0d, fl_tbl_open_get_d_1d, fl_tbl_open_get_d_2d
  end interface fl_tbl_get
  public :: fl_tbl_get

  public :: fl_tbl_close

  public :: fl_reg
  public :: fl_run

contains

  !> Create a new @lua environment.
  !!
  !! This calls the @lua library and opens up a new stack 
  !! for calling @lua code.
  !! @param[inout] state
  !!    A new open @lua handle which is ready for calling @lua code in.
  subroutine fl_state_init(state)
    class(fl_state), intent(inout) :: state
    state%flu_State = fluL_newstate()
  end subroutine fl_state_init

  subroutine fl_state_init_(this)
    class(fl_state), intent(inout) :: this
  end subroutine fl_state_init_

  subroutine fl_state_init_ptr_(this,ptr)
    class(fl_state), intent(inout) :: this
    type(c_ptr), value :: ptr
  end subroutine fl_state_init_ptr_

  !> Register @f functions to be called directly from @lua.
  !!
  !!
  !! Allows registrering @f functions as callable functions in @lua.
  !! 
  !! Example:
  !! \code{.f90}
  !! function f(L_c) result(nret) bind(c)
  !!  type(c_ptr), value : L_c
  !!  type(fl_state) :: L
  !!  L = fl_ptr2state(L_c)
  !!  ! do operations
  !!  nret = 0
  !! end function
  !! subroutine register_fortran_funcs()
  !!  call fl_reg(L,name='fortran_f',func=f)
  !! end subroutine
  !! \endcode
  !! Inside the @lua script you can then
  !! call the `f` code using this:
  !! \code{.lua}
  !! fortran_f()
  !! \endcode
  !!
  !! Some notes on the function declaration.
  !! The returned result `nret` is an integer count of how many
  !! objects on the stack you wish to return.
  !! For instance, if you create a single table in the `f` function
  !! `nret = 1` and the @lua call had to be `tbl = fortran_f()`.
  !!
  !! \param[in] state A @lua state
  !! \param[in] name The exposed function name in @lua
  !! \param[in] func The @f function passed by pointer to be attached to 
  !!    the @lua environment.
  subroutine fl_reg(state, name, func)
    class(fl_state), intent(in) :: state
    character(len=*), intent(in) :: name
    ! Interoperable interface required for a function that is callable from Lua.
    abstract interface
       function lua_function(s) result(val) bind(c)
         use, intrinsic :: iso_c_binding
         integer(c_int) :: val 
         type(c_ptr), value :: s
       end function lua_Function
    end interface
    procedure(lua_function) :: func
    call flu_register(state%flu_State,name,func)
  end subroutine fl_reg

  !> Runs code in the `state` @lua environment
  !!
  !! Starts the `state` with running a @lua file and/or direct
  !! code.
  !! 
  !! The methods are optional and if both supplied the file will
  !! be executed first.
  !! 
  !! __Note__ that we currently silently discards any error messages.
  !!
  !! \param[in] state A @lua state
  !! \param[in] file File name which can be found in the current executing 
  !!            directory. _This is your responsibility!_
  !! \param[in] code Direct text to be executed in @lua.
  subroutine fl_run(state,file,code)
    class(fl_state), intent(in) :: state
    character(len=*), intent(in), optional :: file, code

    integer :: err
    character(len=255) :: err_string
    if ( present(file) ) then
       call open_config_file(state%flu_State, file, err, err_string)
    end if
    if ( present(code) ) then
       call open_config_chunk(state%flu_State, code)
    end if
  end subroutine fl_run

  !> Closes a @lua environment.
  !!
  !! This closes and disconnects @f from @lua by closing the 
  !! the embedded @lua state.
  !!
  !! \param[inout] state
  !!    The open @lua handle to close.
  subroutine fl_state_end(state)
    class(fl_state), intent(inout) :: state
    call flu_close(state%flu_State)
  end subroutine fl_state_end

#ifndef DOX_SKIP_THIS

  ! Documentation @ interface
  function fl_tbl_str(state,parent,key) result(handle)
    class(fl_state), intent(inout) :: state
    integer, intent(in), optional :: parent
    character(len=*), intent(in) :: key
    integer :: handle
    if ( .not. aot_exists(state%flu_State,parent, key = key) ) then
       call aot_table_open(state%flu_state,thandle = handle )
       call aot_table_set_top(state%flu_State,parent, key = key)
       call aot_table_close(state%flu_State,handle)
    end if
    call aot_table_open(state%flu_State,parent , thandle = handle , key = key )
  end function fl_tbl_str

  ! Documentation @ interface
  function fl_tbl_int(state,parent,key) result(handle)
    class(fl_state), intent(inout) :: state
    integer, intent(in), optional :: parent
    integer, intent(in) :: key
    integer :: handle
    if ( .not. aot_exists(state%flu_State,parent, pos = key) ) then
       call aot_table_open(state%flu_state,thandle = handle )
       call aot_table_set_top(state%flu_State,parent, pos = key)
       call aot_table_close(state%flu_State,handle)
    end if
    call aot_table_open(state%flu_State,parent , thandle = handle , pos = key)
  end function fl_tbl_int

  ! Documentation @ interface
  function fl_tbl_none(state,parent) result(handle)
    class(fl_state), intent(inout) :: state
    integer, intent(in), optional :: parent
    integer :: handle
    call aot_table_open(state%flu_state,thandle = handle )
  end function fl_tbl_none

#endif

  !> Closes a @lua table.
  !!
  !! This closes an open table which disables it from
  !! interaction using the handle `handle`.
  !!
  !! \param[inout] state
  !!    The @lua handle where the table reference exist.
  !! \param[inout] handle
  !!    The table handle that is closed for input/output.
  subroutine fl_tbl_close(state, handle)
    class(fl_state), intent(inout) :: state
    integer, intent(inout) :: handle
    call aot_table_close(state%flu_State,handle)
  end subroutine fl_tbl_close

#ifndef DOX_SKIP_THIS

  ! Documentation @ interface
  subroutine fl_tbl_set_d_1d(state,handle,val)
    class(fl_state), intent(inout) :: state
    integer, intent(in) :: handle ! Open table handle
    real(r8b), intent(in) :: val(:)
    integer :: i, b(2)
    b(1) = lbound(val,dim=1)
    b(2) = ubound(val,dim=1)
    do i = b(1) , b(2)
       call aot_table_set_val(val(i),state%flu_State,Thandle=handle, pos = i)
    end do
  end subroutine fl_tbl_set_d_1d

  ! Documentation @ interface
  subroutine fl_tbl_set_d_2d(state,handle,val)
    class(fl_state), intent(inout) :: state
    integer, intent(in) :: handle ! Open table handle
    real(r8b), intent(in) :: val(:,:)
    integer :: i, j, b(2,2)
    ! Handle for the, possibly newly created variable table
    integer :: h
    b(1,:) = lbound(val)
    b(2,:) = ubound(val)
    do j = b(1,2) , b(2,2)
       h = fl_tbl(state,handle, key = j)
       do i = b(1,1) , b(2,1)
          call aot_table_set_val(val(i,j),state%flu_State,Thandle=h, pos = i)
       end do
       call fl_tbl_close(state,h)
    end do
  end subroutine fl_tbl_set_d_2d

  ! Documentation @ interface
  subroutine fl_tbl_open_set_i_0d(state,parent,key,val)
    class(fl_state), intent(inout) :: state
    integer, intent(in) :: parent
    character(len=*), intent(in) :: key
    integer(i4b), intent(in) :: val
    call aot_table_set_val(val,state%flu_state,thandle=parent, key = key)
  end subroutine fl_tbl_open_set_i_0d

  ! Documentation @ interface
  subroutine fl_tbl_open_set_d_0d(state,parent,key,val)
    class(fl_state), intent(inout) :: state
    integer, intent(in) :: parent
    character(len=*), intent(in) :: key
    real(r8b), intent(in) :: val
    call aot_table_set_val(val,state%flu_state,thandle=parent, key = key)
  end subroutine fl_tbl_open_set_d_0d

  ! Documentation @ interface
  subroutine fl_tbl_open_set_d_1d(state,parent,key,val)
    class(fl_state), intent(inout) :: state
    integer, intent(in), optional :: parent
    character(len=*), intent(in) :: key
    real(r8b), intent(in) :: val(:)
    integer :: handle
    handle = fl_tbl(state, parent, key)
    call fl_tbl_set(state,handle,val)
    call fl_tbl_close(state,handle)
  end subroutine fl_tbl_open_set_d_1d

  ! Documentation @ interface
  subroutine fl_tbl_open_set_d_2d(state,parent,key,val)
    class(fl_state), intent(inout) :: state
    integer, intent(in), optional :: parent
    character(len=*), intent(in) :: key
    real(r8b), intent(in) :: val(:,:)
    integer :: handle
    handle = fl_tbl(state, parent, key)
    call fl_tbl_set(state,handle,val)
    call fl_tbl_close(state,handle)
  end subroutine fl_tbl_open_set_d_2d

  ! Documentation @ interface
  subroutine fl_tbl_get_d_1d(state,handle,val)
    class(fl_state), intent(inout) :: state
    integer, intent(in) :: handle ! Open table handle
    real(r8b), intent(inout) :: val(:)
    integer :: i, b(2), err
    b(1) = lbound(val,dim=1)
    b(2) = ubound(val,dim=1)
    do i = b(1) , b(2)
       call aot_table_get_val(val(i),err,state%flu_State,Thandle=handle, pos = i)
    end do
  end subroutine fl_tbl_get_d_1d

  ! Documentation @ interface
  subroutine fl_tbl_get_d_2d(state,handle,val)
    class(fl_state), intent(inout) :: state
    integer, intent(in) :: handle ! Open table handle
    real(r8b), intent(inout) :: val(:,:)
    integer :: i, j, b(2,2)
    ! Handle for the, possibly newly created variable table
    integer :: h, err
    b(1,:) = lbound(val)
    b(2,:) = ubound(val)
    do j = b(1,2) , b(2,2)
       h = fl_tbl(state,handle, key = j)
       do i = b(1,1) , b(2,1)
          call aot_table_get_val(val(i,j),err,state%flu_State,Thandle=h, pos = i)
       end do
       call fl_tbl_close(state,h)
    end do
  end subroutine fl_tbl_get_d_2d

  ! Documentation @ interface
  subroutine fl_tbl_open_get_i_0d(state,parent,key,val)
    class(fl_state), intent(inout) :: state
    integer, intent(in) :: parent ! Open table handle
    character(len=*), intent(in) :: key
    integer(i4b), intent(inout) :: val
    integer :: err
    call aot_table_get_val(val,err,state%flu_State,thandle=parent,key=key)
  end subroutine fl_tbl_open_get_i_0d

  ! Documentation @ interface
  subroutine fl_tbl_open_get_d_0d(state,parent,key,val)
    class(fl_state), intent(inout) :: state
    integer, intent(in) :: parent ! Open table handle
    character(len=*), intent(in) :: key
    real(r8b), intent(inout) :: val
    integer :: err
    call aot_table_get_val(val,err,state%flu_State,thandle=parent,key=key)
  end subroutine fl_tbl_open_get_d_0d

  ! Documentation @ interface
  subroutine fl_tbl_open_get_d_1d(state,parent,key,val)
    class(fl_state), intent(inout) :: state
    integer, intent(in), optional :: parent ! Open table handle
    character(len=*), intent(in) :: key
    real(r8b), intent(inout) :: val(:)
    integer :: h
    h = fl_tbl(state,parent,key)
    call fl_tbl_get(state,h,val)
    call fl_tbl_close(state,h)
  end subroutine fl_tbl_open_get_d_1d

  ! Documentation @ interface
  subroutine fl_tbl_open_get_d_2d(state,parent,key,val)
    class(fl_state), intent(inout) :: state
    integer, intent(in), optional :: parent ! Open table handle
    character(len=*), intent(in) :: key
    real(r8b), intent(inout) :: val(:,:)
    integer :: h
    h = fl_tbl(state,parent,key)
    call fl_tbl_get(state,h,val)
    call fl_tbl_close(state,h)
  end subroutine fl_tbl_open_get_d_2d

#endif

end module fluaput
