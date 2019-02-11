--[[
   This lua script is designed for testing 
   passing and return of arrays from fortran to 
   lua and back
--]]

-- fortran have provided a basic geometry object

-- Call fortran retrieval of values
get_info()

print('H1')
tbl_print(struct)
print('H2')
struct.test.tmp.a.size = 2
struct.test.a = 2.

-- Return the values to fortran
return_info()