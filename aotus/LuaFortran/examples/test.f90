program test
  use flu_binding

  implicit none

  type(flu_State) :: L
  character(len=89) :: filename
  integer :: str_len
  integer :: widthtype, heighttype

  L = fluL_newstate()

  filename = "config.test"
  if (fluL_loadfile(L, filename).ne.0) then
     write(*,*) "cannot load configuration file: ", flu_tolstring(L,-1, str_len)
     STOP
  endif
  if (flu_pcall(L, 0, 0, 0).ne.0) then
     write(*,*) "cannot run configuration file: ", flu_tolstring(L,-1, str_len)
     STOP
  endif

  widthtype = flu_getglobal(L, "width")
  heighttype = flu_getglobal(L, "height")

  if (widthtype == FLU_TNUMBER) then
    write(*,*) 'width=', flu_tonumber(L, -2)
  end if
  if (heighttype == FLU_TNUMBER) then
    write(*,*) 'height=', flu_tonumber(L, -1)
  end if

  call flu_close(L)

end program test
