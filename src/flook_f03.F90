! @LICENSE@ see Copyright notice.

!> \defgroup fortran-03
!!
!! The @lib interface for fortran-2003 standard.
!! This realizes object oriented programming in the model
!! and abstracts routines _onto_ the objects themselves such
!! that operations are intrinsic to each type.
!!
!! This can accomodate more flexibility and allows users
!! to extend data-types etc.
!!
!! @{

!> Accomodates a generic interface for communicating data between an
!! embedded @lua @env and @f.
!!
!! In particular one can insert @f calls in LUA and return
!! information to @lua scripts.

!> A generic and simple interface for running scripts in @f via a @lua
!! channel.
!!
!! The library opens an embedded @lua @env and enables the user to call
!! @lua code at certains points in the @f pipeline.

!> @author
!!  Nick Papior

!> @copyright MPL-2.0

module flook

  ! Required libraries (which should be fully separated to the user)
  use, intrinsic :: iso_c_binding
  use flu_binding

  ! Pre-load all needed aotus modules
  use aot_table_module
  use aotus_module

  implicit none

  ! We denote everything as private as we then force
  ! exposure by this library.
  private

  !> @cond SHOW_PRIVATE

  ! Default variables for passing float and double
  integer, parameter :: i4b = selected_int_kind(r=9) !< @internal precision
  integer, parameter :: i8b = selected_int_kind(r=18) !< @internal precision
  integer, parameter :: r4b = selected_real_kind(p=6) !< @internal precision
  integer, parameter :: r8b = selected_real_kind(p=15) !< @internal precision

  !> @endcond

  public :: luaState
  !> @lua handle for the current @lua @env.
  !! It provides the handles and interaction level with the @lua @env.
  !!
  !! This type is the back-bone of the @lib methodology.
  !! It contains a reference to a @lua @env which is
  !! the equivalent of a @lua shell embedded in the application.
  !! This lets you create several @lua @envs simulatenously
  !! and/or open close new/old ones, at will.
  !!
  !! It provides several class procedures which enables
  !! direct interaction with the @lua @env as well
  !! as the creation of a flook::luaTbl.
  !!
  !! All procedures related to this type are created
  !! with the object methodology in mind. Hence @f 2003 knowledge might
  !! be applicable.
  !!
  !! \section Usage
  !! As an example we will create a new @lua instance and close
  !! it
  !! \code{.f90}
  !! type(luaState) :: lua
  !!
  !! ! Create a new lua environment
  !! call lua%init()
  !!
  !! ! At this point you can interact with lua through
  !! ! the other procedures in the state.
  !!
  !! ! Close lua, and create a new one (clears the lua stack
  !! ! for a fresh restart)
  !! call lua%init()
  !!
  !! ! Do some operations...
  !! !  ... and finally close it
  !! call lua%close()
  !! \endcode
  type :: luaState

     !> @cond SHOW_INSTANCE_VARIABLES

     !> Reference to the underlying library controlling @lua.
     !! This data type is the communication layer to @lua.
     !! You should never need to use this.
     !!
     !! @internal data structure for retaining the @lua
     !! @env in the state. It should only be used
     !! internally in @lib.
     type(flu_State) :: L

     !> Field for denoting a running or non-initialized @lua instance.
     !!
     !! @internal data structure for retaining the status
     !! of the @env.
     !!
     !! You can re-call `this%%init()` on an open @lua @env
     !! which will close the current instance and re-open a new.
     logical :: initialized = .false.

     !> @endcond

   contains

     ! Generate a generic interface for initialization of
     ! a @lua backend.
     !> @cond SHOW_PRIVATE
     !> @isee luaState::init
     procedure, pass :: state_init_
     !> @isee luaState::init
     procedure, pass :: state_init_ptr_
     !> @endcond

     !> Initialization of a new @lua @env.
     !! Can be instantiated either by no arguments (creates a new @lua @env)
     !! or by passing a C-pointer which extracts the current @lua @env from
     !! an already running instance.
     !!
     !! Creates new @lua @envs by starting @lua.
     !!
     !! If the luaState is an already opened @lua @env, it will be closed by an
     !! `call luaState%%close()`.
     !!
     !! Then, a new @lua @env will be created and will be made available
     !! to interact with @lua.
     !!
     !! You cannot interact with @lua until you have called `luaState%%init`.
     !! \param[in] ptr @opt pointer to C-state
     generic, public :: init => state_init_, state_init_ptr_

     ! Close the @lua state
     !> @cond SHOW_PRIVATE
     !> @isee luaState::close or luaState::quit
     procedure, pass :: state_close_
     !> @isee luaState::close
     generic, public :: quit => state_close_
     !> @endcond

     !> Closes the current @lua @env.
     !!
     !! Closes the @lua @env for processing. After a call to this
     !! you will have to initialize a new @lua instance before you can
     !! proceed.
     generic, public :: close => state_close_

     ! Register functions
     !> @cond SHOW_PRIVATE
     !> @isee luaState::register
     procedure, pass :: reg_func_
     !> @endcond

     !> Register @f functions to be called directly from @lua.
     !!
     !! Allows registrering @f functions as callable functions in @lua.
     !!
     !! Example:
     !! \code{.f90}
     !! function f(L_c) result(nret) bind(c)
     !!  type(c_ptr), value : L_c
     !!  type(luaState) :: lua
     !!  call lua%init(L_c)
     !!     ! do operations
     !!  nret = 0
     !! end function
     !! subroutine register_fortran_funcs()
     !!  call lua%register('fortran_f',func=f)
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
     !! \param[in] name the exposed function name in @lua
     !! \param[in] func the @f function passed by pointer to be attached to
     !!    the @lua @env by calling `name`
     generic, public :: register => reg_func_

     ! Run specific code/files in the @lua state
     !> @cond SHOW_PRIVATE
     !> @isee luaState::run
     procedure, pass :: state_run_
     !> @endcond

     !> Runs either code by `character(len=*)` or by reading a file.
     !!
     !! Run @lua code from either direct interaction, or by
     !! running a file.
     !!
     !! The default mode is that it will run the file given as the
     !! argument (`file`-keyword).
     !!
     !! By passing a `code = character(*)` by named argument one
     !! can execute specific code.
     !!
     !! A few examples:
     !!
     !! \code{.f90}
     !! type(luaState) :: lua
     !! call lua%init()
     !!
     !! ! Call initialization file called 'init_file.lua'
     !! call lua%run( 'init_file.lua' )
     !!
     !! ! Call lua code
     !! call lua%run( code = 'table = {}' )
     !! call lua%run( code = 'table.item = 1.' )
     !!
     !! call lua%close()
     !! \endcode
     !!
     !! The equivalent @lua code would be:
     !! \code{.lua}
     !! dofile("init_file.lua")
     !! table = {}
     !! tabel.item = 1.
     !! \endcode
     !!
     !! To create an interactive @lua enviroment in @f
     !! you do a loop
     !! \code{.f90}
     !! type(luaState) :: lua
     !! character(len=512) :: line
     !! do
     !!   ! Read line
     !!   read(*,*) line
     !!   call lua%run( code = line )
     !! end do
     !! call lua%close()
     !! \endcode
     !! Note that running @lua like the above example may require
     !! additional work as each line segment has to be completed.
     !! Hence executing several lines in a disconnected way is
     !! not as easy.
     !!
     !! If both `file` and `code` arguments has been given,
     !! first the file with be executed, then the code.
     !!
     !! \param[in] file @opt executes a `dofile(file)` in @lua
     !! \param[in] code @opt executes `code` in @lua
     !! \param[out] error @opt error ID from @lua (non-zero for error)
     !! \param[out] message @opt error message from @lua
     generic, public :: run => state_run_

     ! Interface for table creation
     ! If nothing is passed it returns
     ! a new tbl on the stack.
     ! Or you can pass a key to grab the table
     ! associated with the key name in the current
     ! scope.
     ! Interface for retrieving stuff from the stack
     !> @cond SHOW_PRIVATE
     !> @isee table
     procedure, pass :: state_tbl_
     !> @isee table
     procedure, pass :: state_top_tbl_
     !> @endcond

     !> Creation/retrieving tables via either names or from the top of the @lua stack in the
     !! current @env.
     !!
     !! Create a table in the main environment by names (creates a new table)
     !! or retrieve a table from the stack (via function calls).
     !!
     !! The table name has the same format as #open.
     !!
     !! See #luaTbl for specifications of the return value.
     !!
     !! As an example:
     !! \code{.f90}
     !! type(luaState) :: lua
     !! type(luaTbl) :: tbl
     !!
     !! ! Create new lua env
     !! call lua%init()
     !!
     !! ! Create a table as a variable
     !! tbl = lua%table('main')
     !! ! Close it again
     !!
     !! ! Create a nested table inside `main`
     !! call tbl%open('main.nest_one.nest_two')
     !!
     !! ! Close the entire tree
     !! call tbl%close_tree()
     !!
     !! ! You can also do everything at one time
     !! tbl = lua%table('new.nest_one.nest_two')
     !!
     !! \endcode
     !! The equivalent @lua code would look like:
     !! \code{.lua}
     !! main = {}
     !! main.nest_one = {}
     !! main.nest_one.nest_two = {}
     !! -- The later table creation can be expressed in compact form:
     !! new = { nest_one = { nest_two = {} } }
     !! \endcode
     !!
     !! \param[in] name @opt name of table to open, can be "."
     !!    delimited as luaTbl::open
     !! \return luaTbl the table object to post-process
     generic, public :: table => state_tbl_, state_top_tbl_

  end type luaState

  !> @cond SHOW_PRIVATE
  ! Signals to the table that it hasn't been created yet
  integer, parameter :: LUA_TBL_UNDEFINED = -9999999 !> @internal for table recognition
  !> @endcond

  public :: luaTbl
  ! Create a type to contain a table.
  !> Handle for interacting with @lua tables.
  !!
  !! @lua handle for a tables.
  !!
  !! This type is a list construct which keeps track of
  !! the opened tree-structure in the @lua @env.
  !!
  !! One can open new tables on already tables to traverse
  !! a complex table of high depth.
  !!
  !! The main used feature is the #open construct
  !! which opens a new table at the given point, which
  !! could be a nested table.
  !!
  !! For example:
  !! \code{.f90}
  !! type(luaState) :: lua
  !! type(luaTbl) :: tbl
  !! integer :: lvls
  !! tbl = lua%table('main')
  !! ! now tbl is `main = {}`
  !! call tbl%open('one.two')
  !! ! now tbl is `two = {}` in `main.one`
  !! ! i.e. tbl = `main.one.two`
  !! lvls = 2
  !! call tbl%close(lvls=lvls)
  !! ! now tbl is `main` again
  !! call tbl%close_open('main.one',lvls=lvls)
  !! ! now tbl is `main.one` and `lvl = 2`.
  !! call tbl%close_tree()
  !! \endcode
  !! Using this nested structure of the table construct
  !! requires far fewer variables to control the @lua table
  !! tree in @f code while providing a clean an efficient traversal
  !! path.
  type :: luaTbl

     !> @cond SHOW_INSTANCE_VARIABLES

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

     !> @endcond

   contains

     ! Initializes a new table
     !> @cond SHOW_PRIVATE
     procedure, pass :: tbl_init_
     !> @endcond

     !> Initializes a table attaching it to a Lua state
     !!
     !! This is mainly for initializing a table without the
     !! need for opening a table.
     !!
     !! \param[inout] luaTbl the table
     !! \param[in] @luaState C-state of Lua
     generic, public :: init => tbl_init_

     ! Create a new table and push it on this object
     !> @cond SHOW_PRIVATE
     procedure, pass :: tbl_open_
     !> @endcond

     !> Opens a table in the current table tree.
     !!
     !! If this object already has an open table
     !! it will open all named arguments as a nested
     !! table.
     !!
     !! This openening provides easy access to several
     !! nested tables using a "." notation.
     !! Hence providing `luaTbl%%open(name)` with
     !! value `main.nested.nested` a table
     !! will be created with this equivalent @lua code:
     !! \code{.lua}
     !! main = { nested = { nested } } }
     !! \endcode
     !!
     !! By using the @opt keyword `luaTbl%%open(lvls=lvls)`
     !! one can retrieve how many levels was opened
     !! by #open. This is handy when you want
     !! to close as many nested tables as you have just
     !! opened.
     !! Hence you can do:
     !! \code{.f90}
     !! lvls = 0
     !! call luaTbl%open("main.nested.nested",lvls = lvls)
     !! ! do operations
     !! call luaTbl%close(lvls = lvls)
     !! \endcode
     !! __Note__ the initialization of `lvls = 0`, this
     !! is needed so as to re-use the same operation
     !! for nested operation of the table, i.e.:
     !! \code{.f90}
     !! lvls = 0
     !! call luaTbl%open("main.nested.nested",lvls = lvls)
     !!
     !! ! do operations at this level
     !! call luaTbl%open("more.nested",lvls = lvls)
     !!
     !! ! do more opeartions at this, deeper, level.
     !!
     !! ! Finally return to the level at `lvls = 0`
     !! call luaTbl%close(lvls = lvls)
     !! \endcode
     !!
     !! \param[in] name the table
     !! \param[inout] lvls @opt keep track of how many levels was actually opened
     generic, public :: open => tbl_open_

     !> @cond SHOW_PRIVATE
     procedure, pass :: tbl_close_
     !> @endcond

     !> Closes a @lua table.
     !!
     !! This closes an open table which disables it from
     !! interaction using the handle `handle`.
     !!
     !! As the luaTbl class is a _following_ data structure a close
     !! will move the table handle to the parent table.
     !!
     !! Hence:
     !! \code{.f90}
     !! call luaTbl%open('main.nested.nested')
     !! call luaTbl%close()
     !! \endcode
     !! will be retain the table handle at `main.nested`, where as
     !! call `call luaTbl%close(tree = .true.)` will close all.
     !!
     !! One can supply an integer `lvls` to specify the number
     !! of levels that will be closed.
     !! If this number exceeds the number of available nested tables
     !! it will be equivalent to `tree = .true.`.
     !!
     !! \param[in] tree @opt whether the entire table tree will be closed
     !! \param[inout] lvls @opt the number of levels that will be closed
     generic, public :: close => tbl_close_

     !> @cond SHOW_PRIVATE
     procedure, pass :: tbl_close_tree_
     !> @endcond

     !> Shorthand for `close(tree=.true.)`
     !!
     !! @isee #close(tree=.true.)
     generic, public :: close_tree => tbl_close_tree_

     !> @cond SHOW_PRIVATE
     procedure, pass :: tbl_close_open_
     !> @endcond

     !> Closes a @lua table, then re-opens it using the provided table name.
     !!
     !! The following two codes are equivalent, without ambiguity:
     !! \code{.f90}
     !! call luaTbl%open('main.nested')
     !! call luaTbl%close()
     !! call luaTbl%open('nested_2')
     !! \endcode
     !! and
     !! \code{.f90}
     !! call luaTbl%open('main.nested')
     !! call luaTbl%close_open('nested_2')
     !! \endcode
     !!
     !! \param[in] name the table
     !! \param[inout] lvls @opt keep track of how many levels was actually closed
     !!     _and_ opened, works as the statement in #close and #open
     !! For instance doing #close_open('main',lvls=lvls) with `lvls=2` will
     !! return with `lvls=1`.
     generic, public :: close_open => tbl_close_open_

     !> @cond SHOW_PRIVATE
     procedure, pass :: tbl_create_str_, tbl_create_int_
     procedure, pass :: tbl_create_
     !> @endcond

     !> Open/create a @lua table from a @lua @env.
     !!
     !! Open or creates a @lua table to be post-processed in
     !! @f.
     !!
     !! This function returns a table handle #luaTbl to do
     !! operations within this table.
     !!
     !! If you supply a `name` it will open the table
     !! at that respective name. If the name already has a
     !! table it will open that and interact with that table,
     !! else it will create a new table and create it in
     !! the name. The table names abide to the rules in #open.
     !!
     !! \param[in] name name of table, possibly "." separated
     !! \return a table handle #luaTbl
     generic, public :: create => tbl_create_str_, tbl_create_int_, &
          tbl_create_

     !> @cond SHOW_PRIVATE
     procedure, pass :: set_s_
     procedure, pass :: set_b_0d_, set_b_1d_, set_b_2d_
     procedure, pass :: set_i_0d_, set_i_1d_, set_i_2d_
     procedure, pass :: set_s_0d_, set_s_1d_, set_s_2d_
     procedure, pass :: set_d_0d_, set_d_1d_, set_d_2d_
     procedure, pass :: open_set_b_1d_, open_set_b_2d_
     procedure, pass :: open_set_i_1d_, open_set_i_2d_
     procedure, pass :: open_set_s_1d_, open_set_s_2d_
     procedure, pass :: open_set_d_1d_, open_set_d_2d_
     !> @endcond

     !> Stores values in @lua tables.
     !!
     !! Stores @f arrays in @lua tables by direct indices
     !! and bounds.
     !! It preserves any bounds found by the passed array
     !! and expresses that in the equivalent @lua table.
     !!
     !! The @lua table passed can be expressed in two different
     !! methods:
     !! - __name__ based, where an initial #open
     !!   is called to create, or retrieve a table by key before
     !!   storing the array in that table.
     !!
     !!   __NOTE__: Currently this is only implemented using a character `key`,
     !!     hence you cannot use positional entries in this method.
     !! - __direct__ based, the currently opened table has been created by
     !!   the user and subsequent storage is directly in this table.
     !!   This is handy if you want to assign other information subsequently.
     !!
     !! \param[in] name @opt this constitutes the __name__ based method
     !! \param[in] val the array to be stored, currently supported dimension and
     !!     kinds are: scalars, and 1-2 D arrays.
     !!     The current data types are:
     !!
     !!     - `character`, (no arrays of this data is allowed)
     !!     - `logical`
     !!     - `integer`
     !!     - `real(kind(0.))`
     !!     - `real(kind(0.d0))`
     generic, public :: set => set_s_, &
          set_b_0d_, set_b_1d_, set_b_2d_, set_i_0d_, set_i_1d_, set_i_2d_, &
          set_s_0d_, set_s_1d_, set_s_2d_, set_d_0d_, set_d_1d_, set_d_2d_, &
          open_set_b_1d_, open_set_b_2d_, open_set_i_1d_, open_set_i_2d_, &
          open_set_s_1d_, open_set_s_2d_, open_set_d_1d_, open_set_d_2d_

     !> @cond SHOW_PRIVATE
     procedure, pass :: set_ptr_b_1d_, set_ptr_b_2d_
     procedure, pass :: set_ptr_i_1d_, set_ptr_i_2d_
     procedure, pass :: set_ptr_s_1d_, set_ptr_s_2d_
     procedure, pass :: set_ptr_d_1d_, set_ptr_d_2d_
     procedure, pass :: open_set_ptr_b_1d_, open_set_ptr_b_2d_
     procedure, pass :: open_set_ptr_i_1d_, open_set_ptr_i_2d_
     procedure, pass :: open_set_ptr_s_1d_, open_set_ptr_s_2d_
     procedure, pass :: open_set_ptr_d_1d_, open_set_ptr_d_2d_
     !> @endcond

     !> Stores pointers to data in @lua tables.
     !!
     !! Stores @f pointers in @lua tables by storing several related quantities
     !! in the table.
     !! Notably a table will be created at the position with the following entries:
     !!
     !! - `.type` which contains the string defining the C-type (except logical)
     !! - `.size` which contains a table of dimension sizes
     !! - `.ptr` which contains the actual C-pointer
     !!
     !! The @lua table passed can be expressed in two different
     !! methods:
     !! - __name__ based, where an initial #lua_open
     !!   is called to create, or retrieve a table by key before
     !!   storing the array in that table.
     !!
     !!   __NOTE__: Currently this is only implemented using a character `key`,
     !!     hence you cannot use positional entries in this method.
     !! - __direct__ based, the currently opened table has been created by
     !!   the user and subsequent storage is directly in this table.
     !!   This is handy if you want to assign other information subsequently.
     !!
     !!
     !! __NOTE__: setting and fetching a pointer does not preserve bounds. Any
     !!           returned pointer using `luaTbl%get_ptr` will force bounds
     !!           `1:size(...)` for all dimensions.
     !!
     !! \param[in] name @opt this constitutes the __name__ based method
     !! \param[in] val the array to be stored as a pointer, currently supported dimension and
     !!     kinds are: 1-2 D arrays.
     !!     The current data types are:
     !!
     !!     - `logical`
     !!     - `integer`
     !!     - `real(kind(0.))`
     !!     - `real(kind(0.d0))`
     generic, public :: set_ptr => &
         set_ptr_b_1d_, set_ptr_b_2d_, set_ptr_i_1d_, set_ptr_i_2d_, &
         set_ptr_s_1d_, set_ptr_s_2d_, set_ptr_d_1d_, set_ptr_d_2d_, &
         open_set_ptr_b_1d_, open_set_ptr_b_2d_, open_set_ptr_i_1d_, open_set_ptr_i_2d_, &
         open_set_ptr_s_1d_, open_set_ptr_s_2d_, open_set_ptr_d_1d_, open_set_ptr_d_2d_

     !> @cond SHOW_PRIVATE
     procedure, pass :: get_s_, get_s_i_
     procedure, pass :: get_b_0d_, get_b_1d_, get_b_2d_
     procedure, pass :: get_i_0d_, get_i_1d_, get_i_2d_
     procedure, pass :: get_s_0d_, get_s_1d_, get_s_2d_
     procedure, pass :: get_d_0d_, get_d_1d_, get_d_2d_
     procedure, pass :: open_get_b_1d_, open_get_b_2d_
     procedure, pass :: open_get_i_1d_, open_get_i_2d_
     procedure, pass :: open_get_s_1d_, open_get_s_2d_
     procedure, pass :: open_get_d_1d_, open_get_d_2d_
     !> @endcond

     !> Retrieves values from @lua tables.
     !!
     !! Retrieves @lua tables, or entries, to @f arrays, or variables
     !! direct indices and bounds.
     !! It will try and read in entries as provided by the
     !! @f array from the equivalent @lua table.
     !!
     !! The @f variable/array passed can be expressed in two different
     !! methods:
     !! - __name__ based, where an initial #open
     !!   is retrieving the table by key before locating
     !!   array values from that table.
     !!
     !!   __NOTE__: Currently this is only implemented using a character `key`,
     !!     hence you cannot use positional entries in this method.
     !! - __direct__ based, the currently opened table has the variable/array
     !!   directly stored.
     !!
     !! \param[in] name @opt this constitutes the __name__ based method
     !! \param[in] val the array to be retrieved, currently supported
     !!     dimension and kinds are: scalars, and 1-2 D arrays.
     !!     The current data types are:
     !!
     !!     - `character`, (no arrays of this data is allowed)
     !!     - `logical`
     !!     - `integer`
     !!     - `real(kind(0.))`
     !!     - `real(kind(0.d0))`
     generic, public :: get => get_s_, get_s_i_, &
          get_b_0d_, get_b_1d_, get_b_2d_, get_i_0d_, get_i_1d_, get_i_2d_, &
          get_s_0d_, get_s_1d_, get_s_2d_, get_d_0d_, get_d_1d_, get_d_2d_, &
          open_get_b_1d_, open_get_b_2d_, open_get_i_1d_, open_get_i_2d_, &
          open_get_s_1d_, open_get_s_2d_, open_get_d_1d_, open_get_d_2d_

     !> @cond SHOW_PRIVATE
     procedure, pass :: get_ptr_b_1d_, get_ptr_b_2d_
     procedure, pass :: get_ptr_i_1d_, get_ptr_i_2d_
     procedure, pass :: get_ptr_s_1d_, get_ptr_s_2d_
     procedure, pass :: get_ptr_d_1d_, get_ptr_d_2d_
     procedure, pass :: open_get_ptr_b_1d_, open_get_ptr_b_2d_
     procedure, pass :: open_get_ptr_i_1d_, open_get_ptr_i_2d_
     procedure, pass :: open_get_ptr_s_1d_, open_get_ptr_s_2d_
     procedure, pass :: open_get_ptr_d_1d_, open_get_ptr_d_2d_
     !> @endcond

     !> Retrieve pointers from @lua tables.
     !!
     !! Retrieves pointers stored using `lua_set_ptr`.
     !! The data will be fetched from the current entries:
     !!
     !! - `.size` containing the dimensions
     !! - `.ptr` containing the actual C-pointer
     !!
     !! Retrieves @lua tables, or entries, to @f arrays, or variables
     !! direct indices and bounds.
     !! It will try and read in entries as provided by the
     !! @f array from the equivalent @lua table.
     !!
     !! The @f variable/array passed can be expressed in two different
     !! methods:
     !! - __name__ based, where an initial #lua_open
     !!   is retrieving the table by key before locating
     !!   array values from that table.
     !!
     !!   __NOTE__: Currently this is only implemented using a character `key`,
     !!     hence you cannot use positional entries in this method.
     !! - __direct__ based, the currently opened table has the variable/array
     !!   directly stored.
     !!
     !! \param[in] name @opt this constitutes the __name__ based method
     !! \param[in] val the pointer to be retrieved, currently supported
     !!     dimension and kinds are: 1D and 2D arrays.
     !!     The current data types are:
     !!
     !!     - `logical`
     !!     - `integer`
     !!     - `real(kind(0.))`
     !!     - `real(kind(0.d0))`
     generic, public :: get_ptr => &
         get_ptr_b_1d_, get_ptr_b_2d_, get_ptr_i_1d_, get_ptr_i_2d_, &
         get_ptr_s_1d_, get_ptr_s_2d_, get_ptr_d_1d_, get_ptr_d_2d_, &
         open_get_ptr_b_1d_, open_get_ptr_b_2d_, open_get_ptr_i_1d_, open_get_ptr_i_2d_, &
         open_get_ptr_s_1d_, open_get_ptr_s_2d_, open_get_ptr_d_1d_, open_get_ptr_d_2d_

  end type luaTbl

  public :: len
  !> Length of a table
  !!
  !! Retrieve the length of a table.
  !! \param[in] luaTbl table to retrieve the length of
  !! \return integer the number of elements in the table, note that
  !!     _hidden_ entries are also counted, i.e. `metatable`s etc.
  interface len
     module procedure tbl_len_
  end interface len

  !> @cond SHOW_PRIVATE

  ! Internal interface to create tables easily
  interface tbl_create__
     module procedure tbl_create_by_handle_key__
     module procedure tbl_create_by_handle_pos__
     module procedure tbl_create_by_handle__
  end interface tbl_create__

  !> @endcond

contains

  !> @cond SHOW_PRIVATE

  !> @isee luaState::init
  subroutine state_init_(lua)
    class(luaState), intent(inout) :: lua
    call lua%close()
    lua%L = fluL_newstate()
    lua%initialized = .true.
  end subroutine state_init_

  !> @isee luaState::init
  subroutine state_init_ptr_(lua,ptr)
    class(luaState), intent(inout) :: lua
    type(c_ptr), value :: ptr
    call lua%close()
    lua%L = flu_copyptr(ptr)
  end subroutine state_init_ptr_


  !> @isee luaState::register
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


  !> @isee luaState::run
  subroutine state_run_(lua,file,code, error, message)
    class(luaState), intent(in) :: lua
    character(len=*), intent(in), optional :: file, code
    integer, intent(out), optional :: error
    character(len=*), intent(out), optional :: message

    if ( present(file) ) then
       call open_config_file(lua%L, file, ErrCode=error, ErrString=message)
    end if
    if ( present(code) ) then
       call open_config_chunk(lua%L, code, ErrCode=error, ErrString=message)
    end if
  end subroutine state_run_


  !> @isee luaState::close
  subroutine state_close_(lua)
    class(luaState), intent(inout) :: lua
    if ( lua%initialized ) call close_config(lua%L)
    lua%initialized = .false.
  end subroutine state_close_


  !> @isee luaState::table
  function state_tbl_(lua,name) result(tbl)
    class(luaState), intent(inout), target :: lua
    character(len=*), intent(in) :: name
    type(luaTbl) :: tbl

    tbl%lua => lua
    ! nullify the parent (the user shall assure it is closed)
    nullify(tbl%p)
    tbl%h = LUA_TBL_UNDEFINED

    ! Create a new table
    call tbl%open(name)

  end function state_tbl_

  !> @isee luaState::table
  function state_top_tbl_(lua) result(tbl)
    class(luaState), intent(inout), target :: lua
    type(luaTbl) :: tbl
    tbl%lua => lua
    nullify(tbl%p)
    tbl%h = aot_table_top(lua%L)
  end function state_top_tbl_

  !> @endcond


  ! Here we have functions related to the table object

  !> @cond SHOW_PRIVATE

  !> @isee luaTbl::init
  subroutine tbl_init_(tbl,state)
    class(luaTbl), intent(inout) :: tbl
    type(luaState), intent(in), target :: lua

    call tbl%close()
    tbl%lua => lua

  end subroutine tbl_init_

  !> @isee luaTbl::open
  recursive subroutine tbl_open_(tbl,name,lvls)
    class(luaTbl), intent(inout), target :: tbl
    character(len=*), intent(in) :: name
    integer, intent(inout), optional :: lvls !< has to be zero by the user

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

    ! Update count of levels that has been openened
    if ( present(lvls) ) lvls = lvls + 1

    if ( idx_dot > 1 ) then
       ! Create remaining table
       tmp = adjustl(trim(name(idx_dot+1:)))
       call tbl%open(trim(tmp),lvls = lvls)
    end if

  end subroutine tbl_open_

  !> @isee luaTbl::close
  recursive subroutine tbl_close_(tbl,tree,lvls)
    class(luaTbl), intent(inout) :: tbl
    logical, intent(in), optional :: tree
    integer, intent(inout), optional :: lvls
    logical :: ltree
    type(luaTbl), pointer :: p
    integer :: h
    ! If the table is not open, do not do anything
    if ( tbl%h == LUA_TBL_UNDEFINED ) return

    ! Get options
    if ( present(lvls) ) then
       ! Do an immediate catch of zero closures
       if ( lvls <= 0 ) return
    end if

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
    if ( present(lvls) ) lvls = lvls - 1

    ! Figure out if we should continue close
    ! tables
    if ( ltree ) then
       call tbl%close(.true.)
    else if ( present(lvls) ) then
       call tbl%close(lvls = lvls)
    end if

  end subroutine tbl_close_

  !> @isee luaTbl::close_tree
  subroutine tbl_close_tree_(tbl)
    class(luaTbl), intent(inout) :: tbl
    call tbl%close(.true.)
  end subroutine tbl_close_tree_

  !> @isee luaTbl::close_open
  subroutine tbl_close_open_(tbl,name,lvls)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    integer, intent(inout), optional :: lvls
    call tbl%close(lvls=lvls)
    call tbl%open(name,lvls=lvls)
  end subroutine tbl_close_open_

  !> Retrieves the number of elements in the currently opened table.
  !!
  !! Retrieves the number of elements in the currently opened table.
  function tbl_len_(tbl) result(len)
    type(luaTbl), intent(in) :: tbl
    integer :: len
    len = aot_table_length(tbl%lua%L,tbl%h)
  end function tbl_len_

  !> @isee luaTbl::create
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

  !> @isee luaTbl::create
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

  !> @isee luaTbl::create
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

  !> @endcond





  ! Internal used interfaces

  !> @cond SHOW_PRIVATE


  ! Reverse an integer list
  ! Only used for pointers
  pure subroutine reverse_(list)
    integer, intent(inout) :: list(:)
    integer :: i, N, item

    N = size(list)

    do i = 1, N / 2
      item = list(i)
      list(i) = list(N+1-i)
      list(N+1-i) = item
    end do

  end subroutine reverse_


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

  !> @endcond

  !> @cond SHOW_PRIVATE

  ! get and set character
  subroutine set_s_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: val
    call aot_table_set_val(val,tbl%lua%L,thandle=tbl%h, key = name)
  end subroutine set_s_


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
  subroutine open_set_b_1d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    logical, intent(in) :: val(:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%set(val)
    call tbl%close(lvls=lvls)
  end subroutine open_set_b_1d_

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

  ! Documentation @ interface
  subroutine open_set_b_2d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    logical, intent(in) :: val(:,:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%set(val)
    call tbl%close(lvls=lvls)
  end subroutine open_set_b_2d_

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
  subroutine open_set_i_1d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    integer, intent(in) :: val(:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%set(val)
    call tbl%close(lvls=lvls)
  end subroutine open_set_i_1d_

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

  ! Documentation @ interface
  subroutine open_set_i_2d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    integer, intent(in) :: val(:,:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%set(val)
    call tbl%close(lvls=lvls)
  end subroutine open_set_i_2d_

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
  subroutine open_set_s_1d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r4b), intent(in) :: val(:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%set(val)
    call tbl%close(lvls=lvls)
  end subroutine open_set_s_1d_

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

  ! Documentation @ interface
  subroutine open_set_s_2d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r4b), intent(in) :: val(:,:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%set(val)
    call tbl%close(lvls=lvls)
  end subroutine open_set_s_2d_

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
  subroutine open_set_d_1d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r8b), intent(in) :: val(:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%set(val)
    call tbl%close(lvls=lvls)
  end subroutine open_set_d_1d_

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

  ! Documentation @ interface
  subroutine open_set_d_2d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r8b), intent(in) :: val(:,:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%set(val)
    call tbl%close(lvls=lvls)
  end subroutine open_set_d_2d_

  !#######  END DOUBLE     ###############


  !#######   POINTERS    ###############

  !#######      LOGICAL     ###############

  ! Documentation @ interface
  subroutine set_ptr_b_1d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    logical, intent(in), target :: val(:)
    type(c_ptr) :: ptr
    integer :: i, s(1)
    s(:) = size(val)
    !call tbl%set('type', 'logical')
    call tbl%set('size', s)
    ptr = c_loc(val(1))
    call aot_table_set_val(ptr,tbl%lua%L,thandle=tbl%h, key = 'ptr')
  end subroutine set_ptr_b_1d_

  ! Documentation @ interface
  subroutine open_set_ptr_b_1d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    logical, intent(in), target :: val(:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%set_ptr(val)
    call tbl%close(lvls=lvls)
  end subroutine open_set_ptr_b_1d_

  ! Documentation @ interface
  subroutine set_ptr_b_2d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    logical, intent(in), target :: val(:,:)
    type(c_ptr) :: ptr
    integer :: i, s(2)
    s(:) = size(val)
    call reverse_(s)
    !call tbl%set('type', 'logical')
    call tbl%set('size', s)
    ptr = c_loc(val(1,1))
    call aot_table_set_val(ptr,tbl%lua%L,thandle=tbl%h, key = 'ptr')
  end subroutine set_ptr_b_2d_

  ! Documentation @ interface
  subroutine open_set_ptr_b_2d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    logical, intent(in), target :: val(:,:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%set_ptr(val)
    call tbl%close(lvls=lvls)
  end subroutine open_set_ptr_b_2d_

  !#######  END LOGICAL     ###############

  !#######      INTEGER     ###############

  ! Documentation @ interface
  subroutine set_ptr_i_1d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    integer, intent(in), target :: val(:)
    type(c_ptr) :: ptr
    integer :: i, s(1)
    s(:) = size(val)
    call tbl%set('type', 'int')
    call tbl%set('size', s)
    ptr = c_loc(val(1))
    call aot_table_set_val(ptr,tbl%lua%L,thandle=tbl%h, key = 'ptr')
  end subroutine set_ptr_i_1d_

  ! Documentation @ interface
  subroutine open_set_ptr_i_1d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    integer, intent(in), target :: val(:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%set_ptr(val)
    call tbl%close(lvls=lvls)
  end subroutine open_set_ptr_i_1d_

  ! Documentation @ interface
  subroutine set_ptr_i_2d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    integer, intent(in), target :: val(:,:)
    type(c_ptr) :: ptr
    integer :: i, s(2)
    s(:) = size(val)
    call reverse_(s)
    call tbl%set('type', 'int')
    call tbl%set('size', s)
    ptr = c_loc(val(1,1))
    call aot_table_set_val(ptr,tbl%lua%L,thandle=tbl%h, key = 'ptr')
  end subroutine set_ptr_i_2d_

  ! Documentation @ interface
  subroutine open_set_ptr_i_2d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    integer, intent(in), target :: val(:,:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%set_ptr(val)
    call tbl%close(lvls=lvls)
  end subroutine open_set_ptr_i_2d_

  !#######  END INTEGER     ###############

  !#######      REAL     ###############

  ! Documentation @ interface
  subroutine set_ptr_s_1d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    real(r4b), intent(in), target :: val(:)
    type(c_ptr) :: ptr
    integer :: i, s(1)
    s(:) = size(val)
    call tbl%set('type', 'float')
    call tbl%set('size', s)
    ptr = c_loc(val(1))
    call aot_table_set_val(ptr,tbl%lua%L,thandle=tbl%h, key = 'ptr')
  end subroutine set_ptr_s_1d_

  ! Documentation @ interface
  subroutine open_set_ptr_s_1d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r4b), intent(in), target :: val(:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%set_ptr(val)
    call tbl%close(lvls=lvls)
  end subroutine open_set_ptr_s_1d_

  ! Documentation @ interface
  subroutine set_ptr_s_2d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    real(r4b), intent(in), target :: val(:,:)
    type(c_ptr) :: ptr
    integer :: i, s(2)
    s(:) = size(val)
    call reverse_(s)
    call tbl%set('type', 'float')
    call tbl%set('size', s)
    ptr = c_loc(val(1,1))
    call aot_table_set_val(ptr,tbl%lua%L,thandle=tbl%h, key = 'ptr')
  end subroutine set_ptr_s_2d_

  ! Documentation @ interface
  subroutine open_set_ptr_s_2d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r4b), intent(in), target :: val(:,:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%set_ptr(val)
    call tbl%close(lvls=lvls)
  end subroutine open_set_ptr_s_2d_

  !#######  END REAL     ###############

  !#######      DOUBLE     ###############

  ! Documentation @ interface
  subroutine set_ptr_d_1d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    real(r8b), intent(in), target :: val(:)
    type(c_ptr) :: ptr
    integer :: i, s(1)
    s(:) = size(val)
    call tbl%set('type', 'double')
    call tbl%set('size', s)
    ptr = c_loc(val(1))
    call aot_table_set_val(ptr,tbl%lua%L,thandle=tbl%h, key = 'ptr')
  end subroutine set_ptr_d_1d_

  ! Documentation @ interface
  subroutine open_set_ptr_d_1d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r8b), intent(in), target :: val(:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%set_ptr(val)
    call tbl%close(lvls=lvls)
  end subroutine open_set_ptr_d_1d_

  ! Documentation @ interface
  subroutine set_ptr_d_2d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    real(r8b), intent(in), target :: val(:,:)
    type(c_ptr) :: ptr
    integer :: i, s(2)
    s(:) = size(val)
    call reverse_(s)
    call tbl%set('type', 'double')
    call tbl%set('size', s)
    ptr = c_loc(val(1,1))
    call aot_table_set_val(ptr,tbl%lua%L,thandle=tbl%h, key = 'ptr')
  end subroutine set_ptr_d_2d_

  ! Documentation @ interface
  subroutine open_set_ptr_d_2d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r8b), intent(in), target :: val(:,:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%set_ptr(val)
    call tbl%close(lvls=lvls)
  end subroutine open_set_ptr_d_2d_

  !#######  END DOUBLE     ###############


  !#######      LOGICAL       ###############

  ! Documentation @ interface
  subroutine get_ptr_b_1d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    logical, pointer :: val(:)
    type(c_ptr) :: ptr
    integer :: err, s(1)
    call tbl%get('size', s)
    call aot_table_get_val(ptr,err,tbl%lua%L,thandle=tbl%h, key = 'ptr')
    call c_f_pointer(ptr, val, shape=s)
  end subroutine get_ptr_b_1d_

  ! Documentation @ interface
  subroutine open_get_ptr_b_1d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    logical, pointer :: val(:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%get_ptr(val)
    call tbl%close(lvls=lvls)
  end subroutine open_get_ptr_b_1d_

  ! Documentation @ interface
  subroutine get_ptr_b_2d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    logical, pointer :: val(:,:)
    type(c_ptr) :: ptr
    integer :: err, s(2)
    call tbl%get('size', s)
    !call tbl%get('type', ...)
    call reverse_(s)
    call aot_table_get_val(ptr,err,tbl%lua%L,thandle=tbl%h, key = 'ptr')
    call c_f_pointer(ptr, val, shape=s)
  end subroutine get_ptr_b_2d_

  ! Documentation @ interface
  subroutine open_get_ptr_b_2d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    logical, pointer :: val(:,:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%get_ptr(val)
    call tbl%close(lvls=lvls)
  end subroutine open_get_ptr_b_2d_

  !#######  END LOGICAL       ###############

  !#######      INTEGER       ###############

  ! Documentation @ interface
  subroutine get_ptr_i_1d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    integer, pointer :: val(:)
    type(c_ptr) :: ptr
    integer :: err, s(1)
    call tbl%get('size', s)
    !call tbl%get('type', ...)
    call aot_table_get_val(ptr,err,tbl%lua%L,thandle=tbl%h, key = 'ptr')
    call c_f_pointer(ptr, val, shape=s)
  end subroutine get_ptr_i_1d_

  ! Documentation @ interface
  subroutine open_get_ptr_i_1d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    integer, pointer :: val(:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%get_ptr(val)
    call tbl%close(lvls=lvls)
  end subroutine open_get_ptr_i_1d_

  ! Documentation @ interface
  subroutine get_ptr_i_2d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    integer, pointer :: val(:,:)
    type(c_ptr) :: ptr
    integer :: err, s(2)
    call tbl%get('size', s)
    !call tbl%get('type', ...)
    call reverse_(s)
    call aot_table_get_val(ptr,err,tbl%lua%L,thandle=tbl%h, key = 'ptr')
    call c_f_pointer(ptr, val, shape=s)
  end subroutine get_ptr_i_2d_

  ! Documentation @ interface
  subroutine open_get_ptr_i_2d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    integer, pointer :: val(:,:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%get_ptr(val)
    call tbl%close(lvls=lvls)
  end subroutine open_get_ptr_i_2d_

  !#######  END INTEGER       ###############

  !#######      REAL       ###############

  ! Documentation @ interface
  subroutine get_ptr_s_1d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    real(r4b), pointer :: val(:)
    type(c_ptr) :: ptr
    integer :: err, s(1)
    call tbl%get('size', s)
    !call tbl%get('type', ...)
    call aot_table_get_val(ptr,err,tbl%lua%L,thandle=tbl%h, key = 'ptr')
    call c_f_pointer(ptr, val, shape=s)
  end subroutine get_ptr_s_1d_

  ! Documentation @ interface
  subroutine open_get_ptr_s_1d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r4b), pointer :: val(:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%get_ptr(val)
    call tbl%close(lvls=lvls)
  end subroutine open_get_ptr_s_1d_

  ! Documentation @ interface
  subroutine get_ptr_s_2d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    real(r4b), pointer :: val(:,:)
    type(c_ptr) :: ptr
    integer :: err, s(2)
    call tbl%get('size', s)
    !call tbl%get('type', ...)
    call reverse_(s)
    call aot_table_get_val(ptr,err,tbl%lua%L,thandle=tbl%h, key = 'ptr')
    call c_f_pointer(ptr, val, shape=s)
  end subroutine get_ptr_s_2d_

  ! Documentation @ interface
  subroutine open_get_ptr_s_2d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r4b), pointer :: val(:,:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%get_ptr(val)
    call tbl%close(lvls=lvls)
  end subroutine open_get_ptr_s_2d_

  !#######  END REAL       ###############

  !#######      DOUBLE       ###############

  ! Documentation @ interface
  subroutine get_ptr_d_1d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    real(r8b), pointer :: val(:)
    type(c_ptr) :: ptr
    integer :: err, s(1)
    call tbl%get('size', s)
    !call tbl%get('type', ...)
    call aot_table_get_val(ptr,err,tbl%lua%L,thandle=tbl%h, key = 'ptr')
    call c_f_pointer(ptr, val, shape=s)
  end subroutine get_ptr_d_1d_

  ! Documentation @ interface
  subroutine open_get_ptr_d_1d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r8b), pointer :: val(:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%get_ptr(val)
    call tbl%close(lvls=lvls)
  end subroutine open_get_ptr_d_1d_

  ! Documentation @ interface
  subroutine get_ptr_d_2d_(tbl,val)
    class(luaTbl), intent(inout) :: tbl
    real(r8b), pointer :: val(:,:)
    type(c_ptr) :: ptr
    integer :: err, s(2)
    call tbl%get('size', s)
    !call tbl%get('type', ...)
    call reverse_(s)
    call aot_table_get_val(ptr,err,tbl%lua%L,thandle=tbl%h, key = 'ptr')
    call c_f_pointer(ptr, val, shape=s)
  end subroutine get_ptr_d_2d_

  ! Documentation @ interface
  subroutine open_get_ptr_d_2d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r8b), pointer :: val(:,:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%get_ptr(val)
    call tbl%close(lvls=lvls)
  end subroutine open_get_ptr_d_2d_

  !#######  END DOUBLE       ###############


  !###### END POINTER ################


  ! get and set character
  subroutine get_s_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    character(len=*), intent(inout) :: val
    integer :: err
    call aot_table_get_val(val,err,tbl%lua%L,thandle=tbl%h, key = name)
  end subroutine get_s_

  ! get and set character
  subroutine get_s_i_(tbl,idx,val)
    class(luaTbl), intent(inout) :: tbl
    integer, intent(in) :: idx
    character(len=*), intent(inout) :: val
    integer :: err
    call aot_table_get_val(val,err,tbl%lua%L,thandle=tbl%h, pos = idx)
  end subroutine get_s_i_


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
  subroutine open_get_b_1d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    logical, intent(inout) :: val(:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%get(val)
    call tbl%close(lvls=lvls)
  end subroutine open_get_b_1d_

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

  ! Documentation @ interface
  subroutine open_get_b_2d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    logical, intent(inout) :: val(:,:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%get(val)
    call tbl%close(lvls=lvls)
  end subroutine open_get_b_2d_

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
  subroutine open_get_i_1d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    integer, intent(inout) :: val(:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%get(val)
    call tbl%close(lvls=lvls)
  end subroutine open_get_i_1d_

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

  ! Documentation @ interface
  subroutine open_get_i_2d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    integer, intent(inout) :: val(:,:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%get(val)
    call tbl%close(lvls=lvls)
  end subroutine open_get_i_2d_

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
  subroutine open_get_s_1d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r4b), intent(inout) :: val(:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%get(val)
    call tbl%close(lvls=lvls)
  end subroutine open_get_s_1d_

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

  ! Documentation @ interface
  subroutine open_get_s_2d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r4b), intent(inout) :: val(:,:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%get(val)
    call tbl%close(lvls=lvls)
  end subroutine open_get_s_2d_

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
  subroutine open_get_d_1d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r8b), intent(inout) :: val(:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%get(val)
    call tbl%close(lvls=lvls)
  end subroutine open_get_d_1d_

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

  ! Documentation @ interface
  subroutine open_get_d_2d_(tbl,name,val)
    class(luaTbl), intent(inout) :: tbl
    character(len=*), intent(in) :: name
    real(r8b), intent(inout) :: val(:,:)
    integer :: lvls
    lvls = 0
    call tbl%open(name,lvls=lvls)
    call tbl%get(val)
    call tbl%close(lvls=lvls)
  end subroutine open_get_d_2d_

  !#######  END DOUBLE     ###############

  !> @endcond

  !> @}

end module flook
