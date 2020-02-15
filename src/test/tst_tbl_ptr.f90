program main

  use flook

  implicit none

  character(*), parameter :: fortran_static_lua = '&
      --[[ Print out hello statement directly, and immediately when &
      Using the LUA interface &
      --]] &
      print("tbl-ptr-test") &
      flook = {} &
      '

  ! the lua embedded state
  type(luaState) :: lua
  type(luaTbl) :: tbl

  integer :: i
  real :: array(10)
  real, pointer :: array_p(:)
  logical :: fail
  character(len=16) :: dtype
  
  fail = .false.

  ! Open new lua state.
  call lua_init(lua)

  call lua_run(lua,code = fortran_static_lua)

  ! Initialize the array
  do i = 1, size(array)
    array(i) = i
  end do

  ! create a table
  tbl = lua_table(lua,'flook')
  ! define pointer in table `flook`
  ! Since a pointer does not have *bounds* per-see, we will
  ! store it.
  ! So
  !    flook.type == 'r4'
  !    flook.size == size(array) ! in correct dimensions
  call lua_set_ptr(tbl, array)
  ! get pointer
  call lua_get_ptr(tbl, array_p)

  call lua_get(tbl, 'type', dtype)

  ! Check we have the same data!
  if ( dtype /= 'r4' ) then
    fail = .true.
  end if
  if ( size(array) /= size(array_p) ) then
    fail = .true.
  end if
  do i = 1, size(array)
    if ( abs(array(i) - array_p(i)) > 0.0000001 ) then
      fail = .true.
    end if
  end do

  ! Print fail or success
  if ( fail ) then
    print *, 'FAIL'
  else
    print *, 'SUCCESS'
  end if

  call lua_close(lua)

end program main
