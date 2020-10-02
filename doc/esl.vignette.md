| License | Authors | Download |
|:--------|:--------|:---------|
| [MPL-2.0](https://opensource.org/licenses/MPL-2.0) | Nick Papior | [flook](https://github.com/ElectronicStructureLibrary/flook/releases) |

Documentation
-------------

The documentation for flook can be found at
[documentation](http://electronicstructurelibrary.github.io/flook/doxygen/index.html).

Generalized API for interacting with Lua from fortran.

By embedding Lua in existing fortran codes, one can exchange information
from powerful DFT software with scripting languages such as Lua.

By abstracting the interface in fortran one can easily generalize a
communication layer to facilitate *on-the-fly* interaction with the
program.

Example
-------

Currently flook is implemented in both f95 and f03 compliant source.
Below is an example of the f03 interface, but please see the
documentation for the f95 interface which is based purely on types and
direct subroutine calls.

Use the test files as much as you like to create compliant code.

An example of an interaction layer with Lua:

``` {.fortran}
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
  call lua%run( 'exp_flook.lua' )

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
```

Which will call Lua at 6 instances (all **%run(\...)** calls).

The interacting Lua script could look like this (note that each of the 6
function *must* be defined in the Lua script (or the interpreter
will die!):

``` {.lua}
-- @example exp_flook.lua
--[[
   LUA function called by fortran
--]]

print("LUA called from FORTRAN")

-- Define the handle for retaining data
struct = {}

function pre_init()
   -- Communicate data from fortran
   fortran_get()
   struct:print("pre_init")
   struct.control = 2.
   -- Communicate data to fortran
   fortran_set()
end

function post_init()
   fortran_get()
   struct:print("post_init")
   struct.control = 1.
   fortran_set()
end

function pre_calc()
   fortran_get()
   struct:print("pre_calc")
   struct.control = 2.
   struct.vector[2] = 0.
   fortran_set()
end

function post_calc()
   fortran_get()
   struct:print("post_calc")
   fortran_set()
end

function pre_finalize()
   fortran_get()
   struct:print("pre_finalize")
   struct.control = 3.
   fortran_set()
end

function post_finalize()
   fortran_get()
   struct:print("post_finalize")

   fortran_set()
   print("Fully ran everything in the LUA file")
end


--[[
   To ease the printing of the data structures
   we add a few helper functions to print matrices in a stringent 
   way
--]]
function mat_print(name,mat)
   print("Printing matrix: "..name)
   a = ""
   is_matrix = false
   for ia,xyz in pairs(mat) do
      if type(xyz) == "table" then
     is_matrix = true
     a = ""
     for _,x in pairs(xyz) do a = a .. " " .. x end
     print(a)
      else
     a = a .. " " .. xyz
      end
   end
   if not is_matrix then print(a) end
end

struct.print = function(self,msg)
   if msg then print(msg) end
   print("Control: "..self.control)
   mat_print("matrix",self.matrix)
   mat_print("vector",self.vector)
   print("") -- new line
end
```

Current code usage
------------------

-   [siesta](https://departments.icmab.es/leem/siesta/) has flook implemented, the following
    capabilities are implemented (see the [siesta](https://departments.icmab.es/leem/siesta/)
    manual for updates)
    -   creation of custom MD routines
    -   creation of complex constraints on system relaxation
    -   change of options *on-the-fly*
        -   complex convergence paths can be created by changing the
            mixing weight, the number of self-consistency cycles, or
            even signalling [siesta](https://departments.icmab.es/leem/siesta/) to stop at the
            next SCF step
    -   retrieval of MD steps to directly output to specific formats
