program main

  use flook

  implicit none

  character(*), parameter :: fortran_static_lua = '&
       --[[ Print out hello statement directly, and immediately when &
            Using the LUA interface &
       --]] &
       print("tbl-test") &
       flook = {} &
       function flook.print(arg) &
         print(arg) &
       end'

  character(*), parameter :: fortran_lua_struct = '&
struct = { &
} &
tbl_print = function(tbl) &
if type(tbl) == "table" then &
for i,v in pairs(tbl) do &
   if i ~= "print" and i ~= "__index" then &
      print("index: "..i) &
      tbl_print(v) &
   end &
end &
else &
   print(tbl) &
end &
end &
struct.__index = struct'
  
  ! the lua embedded state
  type(luaState) :: lua

  ! Open new lua state.
  call lua%init()

  ! Register some fortran function to a lua function call

  call lua%register("get_info", pass_info)
  call lua%register("return_info", retrieve_info)

  ! Add standard code to the parser
  call lua%run(code = fortran_static_lua)
  call lua%run(code = fortran_lua_struct)

  call lua%run('tst_tbl.lua')

  call lua%close()

  print '(/,a)','LUA parsed'
            
contains

  ! Create function to be called by LUA
  function pass_info(state) result(npush) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    use flook

    ! Define the state
    type(c_ptr), value :: state
    ! Define the in/out
    integer(c_int) :: npush

    type(luaState) :: lua
    type(luaTbl) :: tbl

    integer :: lvls
    ! Copy the c-pointer to the lua-state
    call lua%init(state)

    ! Open table named 
    tbl = lua%table('struct')
    call tbl%open('test.tmp.a',lvls = lvls)
    print*,'opened: ',lvls
    call tbl%set('size', 2)
    call tbl%close(lvls=2)
    call tbl%set('a', 1.5)
    call tbl%close_tree()

    ! we have no return values
    npush = 0
    
  end function pass_info

  ! Create function to be called by LUA
  function retrieve_info(state) result(npush) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    use flook

    ! Define the state
    type(c_ptr), value :: state
    ! Define the in/out
    integer(c_int) :: npush

    type(luaState) :: lua
    type(luaTbl) :: tbl

    integer :: i
    real :: tmp

    ! Copy the c-pointer to the lua-state
    call lua%init(state)

    ! Open table named 
    tbl = lua%table('struct.test.tmp.a')
    call tbl%get('size', i)
    call tbl%close(lvls=2)
    call tbl%get('a', tmp)
    print *, 'Size: ',i
    print *, 'tmp: ',tmp
    call tbl%close_tree()

    ! we have no return values
    npush = 0

  end function retrieve_info

end program main
