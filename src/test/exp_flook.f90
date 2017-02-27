!> @example exp_flook.f90
! Our calculating program
program main
  use flook

  ! Global variables in the scope that
  ! we will communicate with
  real :: matrix(3,3), vector(3)
  real :: control

  type(luaState) :: lua

  ! Initialize the @lua environment
  call lua_init(lua)

  ! Register a couple of functions to pass information back and
  ! forth between @lua.
  call lua_register(lua, 'fortran_get', script_set )
  call lua_register(lua, 'fortran_set', script_get )

  ! Call pre-initialize script (this should define
  ! all functions that are directly called in the program.
  ! Needless to say you can create a single @lua function
  ! which will determine the path via a control parameter.
  call lua_run(lua, 'exp_flook.lua' )

  call lua_run(lua, code = 'pre_init()' )
  call initialize()
  call lua_run(lua, code = 'post_init()' )

  call lua_run(lua, code = 'pre_calc()' )
  call calculate()
  call lua_run(lua, code = 'post_calc()' )

  call lua_run(lua, code = 'pre_finalize()' )
  call finalize()
  call lua_run(lua, code = 'post_finalize()' )

  ! Close @lua
  call lua_close(lua)

contains

  function script_set(state) result(nret)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    
    ! Define the state
    type(c_ptr), value :: state
    ! Define the in/out
    integer(c_int) :: nret

    type(luaState) :: lua
    type(luaTbl) :: tbl

    call lua_init(lua,state)

    ! open global table in variable struct
    tbl = lua_table(lua,'struct')

    ! Set the variables to the struct table:
    ! struct.control = `control`
    call lua_set(tbl,'control',control)
    ! struct.matrix = `matrix`
    call lua_set(tbl,'matrix',matrix)
    ! struct.vector = `vector`
    call lua_set(tbl,'vector',vector)

    call lua_close_tree(tbl)

    ! this function returns nothing
    nret = 0
    
  end function script_set

  function script_get(state) result(nret)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    
    ! Define the state
    type(c_ptr), value :: state
    ! Define the in/out
    integer(c_int) :: nret

    type(luaState) :: lua
    type(luaTbl) :: tbl

    call lua_init(lua,state)

    ! open global table in variable struct
    tbl = lua_table(lua,'struct')

    ! Get the variables from the struct table:
    call lua_get(tbl,'control',control)
    call lua_get(tbl,'matrix',matrix)
    call lua_get(tbl,'vector',vector)

    call lua_close_tree(tbl)

    ! this function returns nothing
    nret = 0
    
  end function script_get
  
  subroutine initialize()
    control = 0.
    matrix = 0.5
    matrix(1,1) = 1.
    matrix(2,2) = 2.
    matrix(3,3) = 3.
    vector = (/1.,2.,3./)
  end subroutine initialize

  subroutine calculate
    integer :: i
    do i = 1 , 3
       vector(i) = sum(matrix(:,i) * vector) * control
       matrix(i,:) = vector
    end do
  end subroutine calculate

  subroutine finalize
    matrix(1,1) = vector(1)
    matrix(2,2) = vector(2)
    matrix(3,3) = vector(3)
  end subroutine finalize

end program main



