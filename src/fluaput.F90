! @LICENSE@ see Copyright notice.

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

  ! Required libraries (which should be fully separated to the user)
  use, intrinsic :: iso_c_binding
  use flu_binding

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
  type :: luaState
     
     !> Reference to the underlying library controlling @lua
     !!
     !! The @lua instance controlled in this type.
     type(flu_State) :: L

     !> Logical which specifies whether the state
     !! has been initialized or not.
     !!
     !! This enables one to re-call `this%init()` which will
     !! close a currently running state in the variable (if any) 
     !! and open a new one.
     logical :: initialized = .false.

   contains

     ! Generate a generic interface for initialization of
     ! a @lua backend.
     procedure, pass :: state_init_
     procedure, pass :: state_init_ptr_
     generic :: init => state_init_, state_init_ptr_

     ! Close the @lua state
     procedure, pass :: state_close_
     generic :: close => state_close_
     generic :: quit => state_close_

     ! Register functions
     procedure, pass :: reg_func_
     generic :: reg => reg_func_

     ! Run specific code/files in the @lua state
     procedure, pass :: state_run_
     generic :: run => state_run_

     ! Interface for table creation
     ! If nothing is passed it returns
     ! a new tbl on the stack.
     ! Or you can pass a key to grab the table
     ! associated with the key name in the current 
     ! scope.
     procedure, pass :: state_tbl_
     generic :: tbl => state_tbl_

  end type luaState
  public :: luaState

  ! Signals to the table that it hasn't been created yet
  integer, parameter :: LUA_TBL_UNDEFINED = -9999999

  ! Data container for a li

  ! Create a type to contain a table.
  !> \fn luaTbl
  !! 
  !! @lua handle for a table.
  type :: luaTbl

     !> Handle for the @lua table
     !!
     !! The handle for the table in the @lua instance.
     integer :: h = LUA_TBL_UNDEFINED

     !> Handle for the parent @lua table
     !! 
     !! Handle for the parent @lua table which enables
     !! direct traversal.
     type(luaTbl), pointer :: p => null()

     !> The associated @lua instance
     !!
     !! A pointer to the parent @lua instance.
     !! This enables one to pass tables as entire entities 
     !! for the @lua state.
     type(luaState), pointer :: lua => null()

   contains

     ! Create a new table and push it on this object
     procedure, pass :: tbl_open_
     generic :: open => tbl_open_

     procedure, pass :: tbl_close_
     generic :: close => tbl_close_
     procedure, pass :: tbl_close_tree_
     generic :: close_tree => tbl_close_tree_

     procedure, pass :: tbl_close_open_
     generic :: close_open => tbl_close_open_

     procedure, pass :: tbl_create_str_, tbl_create_int_
     procedure, pass :: tbl_create_
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
     generic :: create => tbl_create_str_, tbl_create_int_, &
          tbl_create_

     procedure, pass :: set_b_0d_, set_b_1d_, set_b_2d_
     procedure, pass :: set_i_0d_, set_i_1d_, set_i_2d_
     procedure, pass :: set_s_0d_, set_s_1d_, set_s_2d_
     procedure, pass :: set_d_0d_, set_d_1d_, set_d_2d_

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
     generic :: set => set_b_0d_, set_b_1d_, set_b_2d_, &
          set_i_0d_, set_i_1d_, set_i_2d_, &
          set_s_0d_, set_s_1d_, set_s_2d_, &
          set_d_0d_, set_d_1d_, set_d_2d_

     procedure, pass :: get_b_0d_, get_b_1d_, get_b_2d_
     procedure, pass :: get_i_0d_, get_i_1d_, get_i_2d_
     procedure, pass :: get_s_0d_, get_s_1d_, get_s_2d_
     procedure, pass :: get_d_0d_, get_d_1d_, get_d_2d_

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
     generic :: get => get_b_0d_, get_b_1d_, get_b_2d_, &
          get_i_0d_, get_i_1d_, get_i_2d_, &
          get_s_0d_, get_s_1d_, get_s_2d_, &
          get_d_0d_, get_d_1d_, get_d_2d_

  end type luaTbl
  public :: luaTbl

  ! Internal interface to create tables easily
  interface tbl_create__
     module procedure tbl_create_by_handle_key__
     module procedure tbl_create_by_handle_pos__
     module procedure tbl_create_by_handle__
  end interface tbl_create__

contains

  !> Create a new @lua environment.
  !!
  !! This calls the @lua library and opens up a new stack 
  !! for calling @lua code.
  subroutine state_init_(lua)
    class(luaState), intent(inout) :: lua
    call lua%close()
    lua%L = fluL_newstate()
    lua%initialized = .true.
  end subroutine state_init_

  !> Retrieval of states from @lua called @f functions
  !!
  !! The @f routines are passed a pointer to the current top  !! of the @lua stack.
  !! This is required to modify the @lua stack inside @f.
  !! This function translates a @lua stack pointer to a
  !! @f #luaState.
  !!
  !! \param[in] ptr C-pointer to @lua stack
  subroutine state_init_ptr_(lua,ptr)
    class(luaState), intent(inout) :: lua
    type(c_ptr), value :: ptr
    call lua%close()
    lua%L = flu_copyptr(ptr)
  end subroutine state_init_ptr_

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
  subroutine reg_func_(lua, name, func)
    class(luaState), intent(in) :: lua
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
    call flu_register(lua%L,name,func)
  end subroutine reg_func_

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
  subroutine state_run_(lua,file,code)
    class(luaState), intent(in) :: lua
    character(len=*), intent(in), optional :: file, code

    integer :: err
    character(len=255) :: err_string
    if ( present(file) ) then
       call open_config_file(lua%L, file, err, err_string)
    end if
    if ( present(code) ) then
       call open_config_chunk(lua%L, code)
    end if
  end subroutine state_run_

  !> Closes a @lua environment.
  !!
  !! This closes and disconnects @f from @lua by closing the 
  !! the embedded @lua state.
  !!
  !! \param[inout] state
  !!    The open @lua handle to close.
  subroutine state_close_(lua)
    class(luaState), intent(inout) :: lua
    if ( lua%initialized ) call flu_close(lua%L)
    lua%initialized = .false.
  end subroutine state_close_

  !> Creates a table in the @lua state
  !!
  !! Creates a new table from this state and returns
  !! a table handle.
  function state_tbl_(lua,name) result(tbl)
    class(luaState), intent(inout), target :: lua
    character(len=*), intent(in) :: name
    type(luaTbl) :: tbl
    ! Create the 
    tbl%lua => lua
    ! nullify the parent (the user shall assure it is closed)
    nullify(tbl%p)
    tbl%h = LUA_TBL_UNDEFINED

    ! Create a new table
    call tbl%open(name)
    
  end function state_tbl_

  !> Creation of a new table within an existing table
  !!
  !!...
  recursive subroutine tbl_open_(tbl,name)
    class(luaTbl), intent(inout), target :: tbl
    character(len=*), intent(in) :: name

    ! To create the traversal tree
    type(luaTbl), pointer :: tbl_new => null()
    character(len=len(name)) :: tmp
    integer :: idx_dot

    ! if an empty string, return immediately
    if ( len_trim(name) == 0 ) return

    ! Traverse the list of tables that are to be created
    idx_dot = index(name,'.')
    ! We cannot handle tables with "" as key
    ! We return immediately.
    if ( idx_dot == 1 ) return

    if ( idx_dot > 1 ) then
       tmp = adjustl(trim(name(1:idx_dot-1)))
    else
       tmp = adjustl(trim(name))
    end if

    if ( tbl%h /= LUA_TBL_UNDEFINED ) then
       ! We are creating a tree, 
       ! we have to trick the table
       ! to create a linked list.

       allocate(tbl_new)
       tbl_new%lua => tbl%lua
       ! Point to the current parent
       tbl_new%p => tbl%p
       ! Copy the handle
       tbl_new%h = tbl%h
       ! Make the new table the parent
       tbl%p => tbl_new
       ! Reset the current table such that it will
       ! be created
       tbl%h = LUA_TBL_UNDEFINED

    end if

    ! We create it directly
    if ( associated(tbl%p) ) then
       tbl%h = tbl_create__(tbl%lua,tbl%p%h, key = trim(tmp) )
    else
       tbl%h = tbl_create__(tbl%lua, key = trim(tmp) )
    end if

    if ( idx_dot > 1 ) then
       ! Create remaining table
       tmp = adjustl(trim(name(idx_dot+1:)))
       call tbl%open(trim(tmp))
    end if

  end subroutine tbl_open_

  !> Closes a @lua table.
  !!
  !! This closes an open table which disables it from
  !! interaction using the handle `handle`.
  !!
  !! \param[inout] state
  !!    The @lua handle where the table reference exist.
  !! \param[inout] handle
  !!    The table handle that is closed for input/output.
  recursive subroutine tbl_close_(tbl,tree)
    class(luaTbl), intent(inout) :: tbl
    logical, intent(in), optional :: tree
    logical :: ltree
    type(luaTbl), pointer :: p
    integer :: h
    ! If the table is not open, do not do anything
    if ( tbl%h == LUA_TBL_UNDEFINED ) return
    ! Default to not close the entire tree
    ltree = .false.
    if ( present(tree) ) ltree = tree

    ! Retrieve the next handle for the table
    h = LUA_TBL_UNDEFINED
    if ( associated(tbl%p) ) h = tbl%p%h

    ! Actually close the current table
    call aot_table_close(tbl%lua%L,tbl%h)

    if ( associated(tbl%p) ) then
       ! Re-create the linked list
       p => tbl%p%p
       deallocate(tbl%p)
       tbl%p => p
    end if

    ! Store the new table handle
    tbl%h = h
    if ( ltree ) call tbl%close(.true.)

  end subroutine tbl_close_

  subroutine tbl_close_tree_(tbl)
    class(luaTbl), intent(inout) :: tbl
    call tbl%close(.true.)
  end subroutine tbl_close_tree_

  subroutine tbl_close_open_(tbl,name)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    call tbl%close()
    call tbl%open(name)
  end subroutine tbl_close_open_
  
  function tbl_create_str_(tbl,name) result(tbl_new)
    class(luaTbl), intent(inout), target :: tbl
    character(len=*), intent(in) :: name

    type(luaTbl) :: tbl_new

    ! if an empty string, return immediately
    if ( len_trim(name) == 0 ) return

    ! Currently, we do not allow tree like creation of tables
    if ( index(name,'.') > 0 ) return

    ! Create linked list
    tbl_new%lua => tbl%lua
    tbl_new%p => tbl

    ! We create it directly
    if ( tbl%h /= LUA_TBL_UNDEFINED ) then
       tbl_new%h = tbl_create__(tbl%lua,tbl%h, key = trim(name) )
    else
       tbl_new%h = tbl_create__(tbl%lua, key = trim(name) )
    end if
    
  end function tbl_create_str_

  function tbl_create_int_(tbl,pos) result(tbl_new)
    class(luaTbl), intent(inout), target :: tbl
    integer, intent(in) :: pos

    type(luaTbl) :: tbl_new

    tbl_new%lua => tbl%lua
    tbl_new%p => tbl

    ! We create it directly
    if ( tbl%h /= LUA_TBL_UNDEFINED ) then
       tbl_new%h = tbl_create__(tbl%lua,tbl%h, key = pos )
    else
       tbl_new%h = tbl_create__(tbl%lua, key = pos )
    end if
    
  end function tbl_create_int_

  function tbl_create_(tbl) result(tbl_new)
    class(luaTbl), intent(inout), target :: tbl

    type(luaTbl) :: tbl_new

    tbl_new%lua => tbl%lua
    tbl_new%p => tbl

    ! We create it directly
    if ( tbl%h /= LUA_TBL_UNDEFINED ) then
       tbl_new%h = tbl_create__(tbl%lua,tbl%h)
    else
       tbl_new%h = tbl_create__(tbl%lua)
    end if
    
  end function tbl_create_


#ifndef DOX_SKIP_THIS

  ! Documentation @ interface
  function tbl_create_by_handle_key__(lua,parent,key) result(handle)
    class(luaState), intent(inout) :: lua
    integer, intent(in), optional :: parent
    character(len=*), intent(in) :: key
    integer :: handle
    if ( .not. aot_exists(lua%L,parent, key = key) ) then
       call aot_table_open(lua%L,thandle = handle )
       call aot_table_set_top(lua%L,parent, key = key)
       call aot_table_close(lua%L,handle)
    end if
    call aot_table_open(lua%L,parent , thandle = handle , key = key )
  end function tbl_create_by_handle_key__

  ! Documentation @ interface
  function tbl_create_by_handle_pos__(lua,parent,key) result(handle)
    class(luaState), intent(inout) :: lua
    integer, intent(in), optional :: parent
    integer, intent(in) :: key
    integer :: handle
    if ( .not. aot_exists(lua%L,parent, pos = key) ) then
       call aot_table_open(lua%L,thandle = handle )
       call aot_table_set_top(lua%L,parent, pos = key)
       call aot_table_close(lua%L,handle)
    end if
    call aot_table_open(lua%L,parent , thandle = handle , pos = key)
  end function tbl_create_by_handle_pos__

  ! Documentation @ interface
  function tbl_create_by_handle__(lua,parent) result(handle)
    class(luaState), intent(inout) :: lua
    integer, intent(in), optional :: parent
    integer :: handle
    ! This will for sure not exist, it is created on the stack
    call aot_table_open(lua%L,parent, thandle = handle )
  end function tbl_create_by_handle__

#endif


#ifndef DOX_SKIP_THIS

  !########   LOGICAL   ###############
  ! Documentation @ interface
  subroutine set_b_0d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    logical, intent(in) :: val
    call aot_table_set_val(val,tbl%lua%L,thandle=tbl%h, key = name)
  end subroutine set_b_0d_

  ! Documentation @ interface
  subroutine set_b_1d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    logical, intent(in) :: val(:)
    integer :: i, b(2)
    b(1) = lbound(val,dim=1)
    b(2) = ubound(val,dim=1)
    do i = b(1) , b(2)
       call aot_table_set_val(val(i),tbl%lua%L,thandle=tbl%h, pos = i)
    end do
  end subroutine set_b_1d_

  ! Documentation @ interface
  subroutine set_b_2d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    logical, intent(in) :: val(:,:)
    integer :: i, j, b(2,2), h
    b(1,:) = lbound(val)
    b(2,:) = ubound(val)
    do j = b(1,2) , b(2,2)
       h = tbl_create__(tbl%lua,tbl%h,j)
       do i = b(1,1) , b(2,1)
          call aot_table_set_val(val(i,j),tbl%lua%L,thandle=h, pos = i)
       end do
       call aot_table_close(tbl%lua%L,h)
    end do
  end subroutine set_b_2d_

  !####### END LOGICAL   ###############


  !########   INTEGER   ###############
  ! Documentation @ interface
  subroutine set_i_0d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    integer, intent(in) :: val
    call aot_table_set_val(val,tbl%lua%L,thandle=tbl%h, key = name)
  end subroutine set_i_0d_

  ! Documentation @ interface
  subroutine set_i_1d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    integer, intent(in) :: val(:)
    integer :: i, b(2)
    b(1) = lbound(val,dim=1)
    b(2) = ubound(val,dim=1)
    do i = b(1) , b(2)
       call aot_table_set_val(val(i),tbl%lua%L,thandle=tbl%h, pos = i)
    end do
  end subroutine set_i_1d_

  ! Documentation @ interface
  subroutine set_i_2d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    integer, intent(in) :: val(:,:)
    integer :: i, j, b(2,2), h
    b(1,:) = lbound(val)
    b(2,:) = ubound(val)
    do j = b(1,2) , b(2,2)
       h = tbl_create__(tbl%lua,tbl%h,j)
       do i = b(1,1) , b(2,1)
          call aot_table_set_val(val(i,j),tbl%lua%L,thandle=h, pos = i)
       end do
       call aot_table_close(tbl%lua%L,h)
    end do
  end subroutine set_i_2d_

  !####### END INTEGER   ###############

  !#######      REAL     ###############

  ! Documentation @ interface
  subroutine set_s_0d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r4b), intent(in) :: val
    call aot_table_set_val(val,tbl%lua%L,thandle=tbl%h, key = name)
  end subroutine set_s_0d_

  ! Documentation @ interface
  subroutine set_s_1d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    real(r4b), intent(in) :: val(:)
    integer :: i, b(2)
    b(1) = lbound(val,dim=1)
    b(2) = ubound(val,dim=1)
    do i = b(1) , b(2)
       call aot_table_set_val(val(i),tbl%lua%L,thandle=tbl%h, pos = i)
    end do
  end subroutine set_s_1d_

  ! Documentation @ interface
  subroutine set_s_2d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    real(r4b), intent(in) :: val(:,:)
    integer :: i, j, b(2,2), h
    b(1,:) = lbound(val)
    b(2,:) = ubound(val)
    do j = b(1,2) , b(2,2)
       h = tbl_create__(tbl%lua,tbl%h,j)
       do i = b(1,1) , b(2,1)
          call aot_table_set_val(val(i,j),tbl%lua%L,thandle=h, pos = i)
       end do
       call aot_table_close(tbl%lua%L,h)
    end do
  end subroutine set_s_2d_

  !#######  END REAL     ###############

  !#######      DOUBLE     ###############

  ! Documentation @ interface
  subroutine set_d_0d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r8b), intent(in) :: val
    call aot_table_set_val(val,tbl%lua%L,thandle=tbl%h, key = name)
  end subroutine set_d_0d_

  ! Documentation @ interface
  subroutine set_d_1d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    real(r8b), intent(in) :: val(:)
    integer :: i, b(2)
    b(1) = lbound(val,dim=1)
    b(2) = ubound(val,dim=1)
    do i = b(1) , b(2)
       call aot_table_set_val(val(i),tbl%lua%L,thandle=tbl%h, pos = i)
    end do
  end subroutine set_d_1d_

  ! Documentation @ interface
  subroutine set_d_2d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    real(r8b), intent(in) :: val(:,:)
    integer :: i, j, b(2,2), h
    b(1,:) = lbound(val)
    b(2,:) = ubound(val)
    do j = b(1,2) , b(2,2)
       h = tbl_create__(tbl%lua,tbl%h,j)
       do i = b(1,1) , b(2,1)
          call aot_table_set_val(val(i,j),tbl%lua%L,thandle=h, pos = i)
       end do
       call aot_table_close(tbl%lua%L,h)
    end do
  end subroutine set_d_2d_

  !#######  END DOUBLE     ###############

#endif



#ifndef DOX_SKIP_THIS

  !########   LOGICAL   ###############
  ! Documentation @ interface
  subroutine get_b_0d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    logical, intent(inout) :: val
    integer :: err
    call aot_table_get_val(val,err,tbl%lua%L,thandle=tbl%h, key = name)
  end subroutine get_b_0d_

  ! Documentation @ interface
  subroutine get_b_1d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    logical, intent(inout) :: val(:)
    integer :: err, i, b(2)
    b(1) = lbound(val,dim=1)
    b(2) = ubound(val,dim=1)
    do i = b(1) , b(2)
       call aot_table_get_val(val(i),err,tbl%lua%L,thandle=tbl%h, pos = i)
    end do
  end subroutine get_b_1d_

  ! Documentation @ interface
  subroutine get_b_2d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    logical, intent(inout) :: val(:,:)
    integer :: err, i, j, b(2,2), h
    b(1,:) = lbound(val)
    b(2,:) = ubound(val)
    do j = b(1,2) , b(2,2)
       h = tbl_create__(tbl%lua,tbl%h,j)
       do i = b(1,1) , b(2,1)
          call aot_table_get_val(val(i,j),err,tbl%lua%L,thandle=h, pos = i)
       end do
       call aot_table_close(tbl%lua%L,h)
    end do
  end subroutine get_b_2d_

  !####### END LOGICAL   ###############


  !########   INTEGER   ###############
  ! Documentation @ interface
  subroutine get_i_0d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    integer, intent(inout) :: val
    integer :: err
    call aot_table_get_val(val,err,tbl%lua%L,thandle=tbl%h, key = name)
  end subroutine get_i_0d_

  ! Documentation @ interface
  subroutine get_i_1d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    integer, intent(inout) :: val(:)
    integer :: err, i, b(2)
    b(1) = lbound(val,dim=1)
    b(2) = ubound(val,dim=1)
    do i = b(1) , b(2)
       call aot_table_get_val(val(i),err,tbl%lua%L,thandle=tbl%h, pos = i)
    end do
  end subroutine get_i_1d_

  ! Documentation @ interface
  subroutine get_i_2d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    integer, intent(inout) :: val(:,:)
    integer :: err, i, j, b(2,2), h
    b(1,:) = lbound(val)
    b(2,:) = ubound(val)
    do j = b(1,2) , b(2,2)
       h = tbl_create__(tbl%lua,tbl%h, j)
       do i = b(1,1) , b(2,1)
          call aot_table_get_val(val(i,j),err,tbl%lua%L,thandle=h, pos = i)
       end do
       call aot_table_close(tbl%lua%L,h)
    end do
  end subroutine get_i_2d_

  !####### END INTEGER   ###############

  !#######      REAL     ###############

  ! Documentation @ interface
  subroutine get_s_0d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r4b), intent(inout) :: val
    integer :: err
    call aot_table_get_val(val,err,tbl%lua%L,thandle=tbl%h, key = name)
  end subroutine get_s_0d_

  ! Documentation @ interface
  subroutine get_s_1d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    real(r4b), intent(inout) :: val(:)
    integer :: err, i, b(2)
    b(1) = lbound(val,dim=1)
    b(2) = ubound(val,dim=1)
    do i = b(1) , b(2)
       call aot_table_get_val(val(i),err,tbl%lua%L,thandle=tbl%h, pos = i)
    end do
  end subroutine get_s_1d_

  ! Documentation @ interface
  subroutine get_s_2d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    real(r4b), intent(inout) :: val(:,:)
    integer :: err, i, j, b(2,2), h
    b(1,:) = lbound(val)
    b(2,:) = ubound(val)
    do j = b(1,2) , b(2,2)
       h = tbl_create__(tbl%lua,tbl%h, j)
       do i = b(1,1) , b(2,1)
          call aot_table_get_val(val(i,j),err,tbl%lua%L,thandle=h, pos = i)
       end do
       call aot_table_close(tbl%lua%L,h)
    end do
  end subroutine get_s_2d_

  !#######  END REAL     ###############

  !#######      DOUBLE     ###############

  ! Documentation @ interface
  subroutine get_d_0d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r8b), intent(inout) :: val
    integer :: err
    call aot_table_get_val(val,err,tbl%lua%L,thandle=tbl%h, key = name)
  end subroutine get_d_0d_

  ! Documentation @ interface
  subroutine get_d_1d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    real(r8b), intent(inout) :: val(:)
    integer :: err, i, b(2)
    b(1) = lbound(val,dim=1)
    b(2) = ubound(val,dim=1)
    do i = b(1) , b(2)
       call aot_table_get_val(val(i),err,tbl%lua%L,thandle=tbl%h, pos = i)
    end do
  end subroutine get_d_1d_

  ! Documentation @ interface
  subroutine get_d_2d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    real(r8b), intent(inout) :: val(:,:)
    integer :: err, i, j, b(2,2), h
    b(1,:) = lbound(val)
    b(2,:) = ubound(val)
    do j = b(1,2) , b(2,2)
       h = tbl_create__(tbl%lua,tbl%h,j)
       do i = b(1,1) , b(2,1)
          call aot_table_get_val(val(i,j),err,tbl%lua%L,thandle=h, pos = i)
       end do
       call aot_table_close(tbl%lua%L,h)
    end do
  end subroutine get_d_2d_

  !#######  END DOUBLE     ###############

#endif

end module fluaput
