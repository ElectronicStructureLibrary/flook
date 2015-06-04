--[[
   This lua script is designed for testing 
   passing and return of arrays from fortran to 
   lua and back
--]]

-- fortran have provided a basic geometry object

print("Number of atoms retrieved: "..geom.size)

-- Print values before fortran communication
--geom:print("Lua init")

-- Call fortran retrieval of values
get_atom_info()
print("Real passed: "..geom.tmp)
geom.tmp = 2.

geom:print("Fortran passed")

geom.xa[2][2] = geom.xa[2][2] + 2.
geom.fa[3][3] = 2.

-- Return the values to fortran
return_atom_info()