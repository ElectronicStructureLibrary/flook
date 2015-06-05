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

