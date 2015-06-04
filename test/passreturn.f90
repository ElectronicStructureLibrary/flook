module m_array

  implicit none

  integer, parameter :: dp = selected_real_kind(p=14)

  save

  integer :: na_u
  real(dp), allocatable :: xa(:,:), fa(:,:)

contains
  subroutine aalloc(na)
    integer, intent(in) :: na
    allocate(xa(3,na),fa(3,na))
    xa = 1._dp
    fa = 1._dp
    na_u = na
  end subroutine aalloc
  subroutine print_a()
    print '(3000(3(tr1,f6.2),/))',xa
    print '(3000(3(tr1,f6.2),/))',fa
  end subroutine print_a

end module m_array

program main

  use fluaput

  use m_array

  implicit none

  character(*), parameter :: fortran_static_lua = '&
       --[[ Print out hello statement directly, and immediately when &
            Using the LUA interface &
       --]] &
       print("Hello from FORTRAN") &
       fluaput = {} &
       function fluaput.print(arg) &
         print(arg) &
       end'

  character(*), parameter :: fortran_lua_crt_mat = '&
--[[ &
   Initialize a lua matrix &
--]] &
fluaput = {} &
function fluaput.crt_mat(a,b,c,d,e,f) &
   if a == nil then &
      return 0. &
   else &
      local t = {} &
      for i = 1 , a do &
	 t[i] = fluaput.crt_mat(b,c,d,e,f) &
      end &
      return t &
   end &
end'

  character(*), parameter :: fortran_lua_geom = '&
geom = { &
   unit = "Bohr", &
   size = 0, &
   --[[  &
      Create initialization function for &
      setting up the different variables  &
      needed for communication &
   --]] &
   init = function (self) &
      self["cell"] = fluaput.crt_mat(3,3) &
      setmetatable(self["cell"],self) &
      for _,v in pairs({"xa"}) do &
	 self[v] = fluaput.crt_mat(3,self.size) &
      end &
      self["fa"] = {} &
      setmetatable(self["xa"],self) &
      setmetatable(self["fa"],self) &
   end, &
   print = function (self,msg) print("") &
      for _,v in pairs({"xa","fa"}) do &
	 if #msg > 0 then print(msg .. " " .. v) end &
         print(self[v].unit) &
	 for ia,xyz in pairs(self[v]) do &
            if type(ia) == "table" then &
	    a = "" &
	    for _,x in pairs(xyz) do a = a .. " " .. x end &
	    print(a) &
            end &
	 end &
      end &
      print("") &
   end, &
   update_atoms = update_atoms, &
} &
geom.__index = geom'
  
  integer :: err
  character(255) :: err_string
  
  ! the lua embedded state
  type(fl_state) :: L

  ! Allocate arrays
  call aalloc(3)

  print '(/,a)','Fortran initialized'
  call print_a()

  ! Open new lua state.
  call fl_state_init(L)

  ! Register some fortran function to a lua function call

  call fl_reg(L, "update_atoms", array_size_pass)
  call fl_reg(L, "get_atom_info", array_pass)
  call fl_reg(L, "return_atom_info", array_return)

  ! Add standard code to the parser
  call fl_run(L, code = fortran_static_lua)
  call fl_run(L, code = fortran_lua_crt_mat)
  call fl_run(L, code = fortran_lua_geom)
  ! Updated number of atoms and initialize the geometry
  ! to be ready to be parsed
  call fl_run(L, code = 'geom.update_atoms() geom:init()')

  call fl_run(L, 'passreturn.lua')

  call fl_state_quit(L)

  print '(/,a)','LUA parsed'
  call print_a()
            
contains

  ! Create function to be called by LUA
  function array_size_pass(state) result(npush) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    use fluaput

    use m_array

    ! Define the state
    type(c_ptr), value :: state
    ! Define the in/out
    integer(c_int) :: npush

    ! Local variables
    integer :: geom

    type(fl_state) :: L

    ! Copy the c-pointer to the lua-state
    L%flu_state = fl_ptr2state(state)

    ! Open table named 
    geom = fl_tbl(L,key = 'geom')
    call fl_tbl_set(L,geom,'size', na_u)
    ! Real passing
    call fl_tbl_set(L,geom,'tmp', 1.5_8)

    call fl_tbl_close(L,geom)

    npush = 0

  end function array_size_pass

  ! Create function to be called by LUA
  function array_pass(state) result(npush) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    use fluaput

    use m_array

    ! Define the state
    type(c_ptr), value :: state
    ! Define the in/out
    integer(c_int) :: npush

    ! Local variables
    integer :: geom
    type(fl_state) :: L
    real(8) :: tmp

    ! Copy the c-pointer to the lua-state
    L%flu_state = fl_ptr2state(state)

    ! Open table named 
    geom = fl_tbl(L, key = 'geom')

    ! Within this table we pass the xa value
    call fl_tbl_set(L, geom, 'xa', xa)
    call fl_tbl_set(L, geom, 'fa', fa)

    call fl_tbl_close(L, geom)

    ! we have no return values
    npush = 0
    
  end function array_pass

  ! Create function to be called by LUA
  function array_return(state) result(npush) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    use fluaput

    use m_array

    ! Define the state
    type(c_ptr), value :: state
    ! Define the in/out
    integer(c_int) :: npush

    ! Local variables
    integer :: geom
    type(fl_state) :: L
    real(8) :: tmp

    ! Copy the c-pointer to the lua-state
    L%flu_State = fl_ptr2state(state)

    ! Open table named 
    geom = fl_tbl(L, key = 'geom')

    ! Within this table we pass the xa value
    call fl_tbl_get(L, geom, 'tmp', tmp)
    print*,'tmp variable: ',tmp
    call fl_tbl_get(L, geom, 'xa', xa)
    call fl_tbl_get(L, geom, 'fa', fa)

    call fl_tbl_close(L, geom)

    ! we have no return values
    npush = 0

  end function array_return

end program main
