-- Small sample Config File in Lua
width = 200
height = 150

stl_files = { {'filename', 123, 'binary'},
              {'geomfile', 456, fileformat='ascii'} }

coord = { 0.1, 0.0, 0.0 }

--function 'gausspulse'
function gauss_pulse(x, y, z, origin, amplitude, hwidth)
  fact = -0.5/(hwidth*hwidth)
  dist = (x - origin[1])*(x - origin[1]) 
       + (y - origin[2])*(y - origin[2]) 
       + (z - origin[3])*(z - origin[3])
  res = amplitude * math.exp(fact*dist)
  return res
end

--function 'ic_density' 
function ic_density(x, y, z)
  origin = {0.0, 0.0, 0.0}
  amplitude = 1.0
  hwidth = 1.0
  return gauss_pulse(x, y, z, origin, amplitude, hwidth)
end

