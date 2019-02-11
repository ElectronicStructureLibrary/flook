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

  use flu_binding
  use aotus_module
  use m_array

  character(*), parameter :: fortran_static_lua = '&
       --[[ Print out hello statement directly, and immediately when &
            Using the LUA interface &
       --]] &
       print("Hello from FORTRAN") &
       flook = {} &
       function flook.print(arg) &
         print(arg) &
       end'

  character(*), parameter :: fortran_lua_crt_mat = '&
--[[ &
   Initialize a lua matrix &
--]] &
function flook.crt_mat(a,b,c,d,e,f) &
   if a == nil then &
      return 0. &
   else &
      local t = {} &
      for i = 1 , a do &
	 t[i] = flook.crt_mat(b,c,d,e,f) &
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
      self["cell"] = flook.crt_mat(3,3) &
      setmetatable(self["cell"],self) &
      for _,v in pairs({"xa"}) do &
	 self[v] = flook.crt_mat(3,self.size) &
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
            if type(xyz) == "table" then &
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
  type(flu_State) :: L

  ! Allocate arrays
  call aalloc(3)

  print '(/,a)','Fortran initialized'
  call print_a()

  ! Open new lua state.
  L = fluL_newstate()

  ! Register some fortran function to a lua function call
  call flu_register(L, "update_atoms", array_size_pass)
  call flu_register(L, "get_atom_info", array_pass)
  call flu_register(L, "return_atom_info", array_return)

  ! Add standard code to the parser
  call open_config_chunk(L, fortran_static_lua)
  call open_config_chunk(L, fortran_lua_crt_mat)
  call open_config_chunk(L, fortran_lua_geom)
  ! Updated number of atoms and initialize the geometry
  ! to be ready to be parsed
  call open_config_chunk(L, 'geom.update_atoms() geom:init()')


  call open_config_file(L, 'tst_passreturn.lua', err, err_string)
  if ( err /= 0 ) print *,'Error: ',trim(err_string)

  call flu_close(L)

  print '(/,a)','LUA parsed'
  call print_a()
            
contains

  ! Create function to be called by LUA
  function array_size_pass(state) result(npush) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    use flu_binding
    use aotus_module
    use aot_table_module
    use aotus_module

    use m_array

    ! Define the state
    type(c_ptr), value :: state
    ! Define the in/out
    integer(c_int) :: npush

    ! Local variables
    integer :: geom

    type(flu_State) :: L

    ! Copy the c-pointer to the lua-state
    L = flu_copyptr(state)

    geom = 0

    ! Open table named 
    call aot_table_open(L, thandle = geom, key='geom' )
    call aot_table_set_val(na_u,L,thandle=geom, key = 'size')
    call aot_table_close(L,geom)

    npush = 0

  end function array_size_pass

  ! Create function to be called by LUA
  function array_pass(state) result(npush) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    use flu_binding
    use aotus_module
    use aot_table_module
    use aotus_module

    use m_array

    ! Define the state
    type(c_ptr), value :: state
    ! Define the in/out
    integer(c_int) :: npush

    ! Local variables
    integer :: geom

    type(flu_State) :: L

    ! Copy the c-pointer to the lua-state
    L = flu_copyptr(state)

    geom = 0

    ! Open table named 
    call aot_table_open(L, thandle = geom, key='geom' )

    ! Within this table we pass the xa value
    call pass(L,geom,'xa',na_u,xa)
    call pass(L,geom,'fa',na_u,fa)

    call aot_table_close(L,geom)

    ! we have no return values
    npush = 0
    
  end function array_pass

  subroutine pass(L,h,k,n,a)
    use aot_table_module
    type(flu_State), intent(in) :: L
    character(len=*), intent(in) :: k
    integer, intent(in) :: h,n
    real(dp), intent(in) :: a(3,n)
    integer :: t, xyz, i

    if ( .not. aot_exists(L,h, key = k) ) then
       call aot_table_open(L, thandle = t )
       call aot_table_set_top(L, h, key = k)
       call aot_table_close(L, t)
    end if

    call aot_table_open(L, h, thandle = t, key = k )
    do i = 1 , n
       if ( .not. aot_exists(L, t , pos = i) ) then
          call aot_table_open(L, thandle = xyz)
          call aot_table_set_top(L, t, pos = i)
          call aot_table_close(L, xyz)
       end if
       call aot_table_open(L, t, thandle=xyz, pos = i)

       call aot_table_set_val(a(1,i),L,thandle=xyz, pos = 1)
       call aot_table_set_val(a(2,i),L,thandle=xyz, pos = 2)
       call aot_table_set_val(a(3,i),L,thandle=xyz, pos = 3)
       call aot_table_close(L, xyz)
    end do

    ! Attach the new table to the main handle
    ! This has to be done before it is closed (else the 
    ! stack is altered)
    call aot_table_close(L, t)

  end subroutine pass


  ! Create function to be called by LUA
  function array_return(state) result(npush) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    use flu_binding
    use aotus_module
    use aot_table_module
    use aot_fun_module
    use aotus_module

    use m_array

    ! Define the state
    type(c_ptr), value :: state
    ! Define the in/out
    integer(c_int) :: npush

    ! Local variables
    integer :: geom

    type(flu_State) :: L

    ! Copy the c-pointer to the lua-state
    L = flu_copyptr(state)

    ! Open table named 
    call aot_table_open(L, thandle = geom, key='geom' )

    ! Within this table we pass the xa value
    call return(L,geom,'xa',na_u,xa)
    call return(L,geom,'fa',na_u,fa)

    call aot_table_close(L,geom)

    npush = 0

  end function array_return

  subroutine return(L,h,k,n,a)
    use aot_table_module
    use aot_err_module
    type(flu_State), intent(inout) :: L
    character(len=*), intent(in) :: k
    integer, intent(in) :: h,n
    real(dp), intent(out) :: a(3,n)
    integer :: t, xyz, i
    integer :: err
    character(255) :: err_string
    t = 0
    xyz = 0
    call aot_table_open(L, h, thandle = t, key = k )
    do i = 1 , n
       call aot_table_open(L, t, xyz, pos = i)
       call aot_table_get_val(a(1,i),err,L,thandle=xyz, pos = 1)

       call aot_err_handler(L,err,'None',ErrString=err_string)
       if ( err/=0 ) print*,'Error ',k,err,trim(err_string)

       call aot_table_get_val(a(2,i),err,L,thandle=xyz, pos = 2)
       call aot_table_get_val(a(3,i),err,L,thandle=xyz, pos = 3)
       call aot_table_close(L, xyz)
    end do
    call aot_table_close(L, t)
      
  end subroutine return

end program main
