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
  call lua%init()

  ! Register a couple of functions to pass information back and
  ! forth between @lua.
  call lua%register( 'fortran_get', lua_set )
  call lua%register( 'fortran_set', lua_get )

  ! Call pre-initialize script (this should define
  ! all functions that are directly called in the program.
  ! Needless to say you can create a single @lua function
  ! which will determine the path via a control parameter.
  call lua%run( 'tst_exp_flook.lua' )

  call lua%run( code = 'pre_init()' )
  call initialize()
  call lua%run( code = 'post_init()' )

  call lua%run( code = 'pre_calc()' )
  call calculate()
  call lua%run( code = 'post_calc()' )

  call lua%run( code = 'pre_finalize()' )
  call finalize()
  call lua%run( code = 'post_finalize()' )

  ! Close @lua
  call lua%close()

contains

  function lua_set(state) result(nret)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    
    ! Define the state
    type(c_ptr), value :: state
    ! Define the in/out
    integer(c_int) :: nret

    type(luaState) :: lua
    type(luaTbl) :: tbl

    call lua%init(state)

    ! open global table in variable struct
    tbl = lua%table('struct')

    ! Set the variables to the struct table:
    ! struct.control = `control`
    call tbl%set('control',control)
    ! struct.matrix = `matrix`
    call tbl%set('matrix',matrix)
    ! struct.vector = `vector`
    call tbl%set('vector',vector)

    call tbl%close_tree()

    ! this function returns nothing
    nret = 0
    
  end function lua_set

  function lua_get(state) result(nret)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    
    ! Define the state
    type(c_ptr), value :: state
    ! Define the in/out
    integer(c_int) :: nret

    type(luaState) :: lua
    type(luaTbl) :: tbl

    call lua%init(state)

    ! open global table in variable struct
    tbl = lua%table('struct')

    ! Get the variables from the struct table:
    call tbl%get('control',control)
    call tbl%get('matrix',matrix)
    call tbl%get('vector',vector)

    call tbl%close_tree()

    ! this function returns nothing
    nret = 0
    
  end function lua_get
  
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



