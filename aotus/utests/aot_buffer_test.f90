program aot_buffer_test
  use flu_binding, only: flu_State, cbuf_type, fluL_newstate, fluL_loadfile, &
    &                    flu_dump
  use aotus_module, only: aot_get_val, aot_err_handler, &
    &                     open_config_buffer, close_config, &
    &                     aoterr_Fatal, aoterr_NonExistent, aoterr_WrongType

  implicit none

  integer :: err
  integer :: buflen
  integer :: glob_int
  integer :: iError
  logical :: passed
  type(cbuf_type) :: scriptBuffer
  type(flu_State) :: L

  passed = .true.
  call create_script('aot_buffer_config.lua')

  L = fluL_newstate()
  err = fluL_loadfile(L, 'aot_buffer_config.lua')
  call aot_err_handler(L, err, 'Cannot load configuration file:')

  ! Dump the loaded script into a buffer
  write(*,*) 'Dumping a script into a buffer'
  call flu_dump(L = L, buf = scriptbuffer, length = buflen, iError = err)
  if (err /= 0) then
    write(*,*) 'Could not dump the script into a buffer'
    STOP
  else
    write(*,*) '  : success'
  end if

  ! Completely close the state to reopen it with the buffered code.
  call close_config(L)

  write(*,*) 'Reading the script back from a buffer'
  call open_config_buffer(L, scriptbuffer%buffer)

  ! Testing for global INTEGER
  write(*,*) ' * reading a global integer'
  call aot_get_val(L = L, key = 'int_test', &
    &              val = glob_int, ErrCode = iError)

  if (btest(iError, aoterr_Fatal)) then
    write(*,*) '  : unexpected FATAL Error occured !!!'
    if (btest(iError, aoterr_NonExistent)) &
      &   write(*,*) '  : Variable not existent!'
    if (btest(iError, aoterr_WrongType)) &
      &   write(*,*) '  : Variable has wrong type!'
    passed = .false.
  else
    if (glob_int == 5) then
      write(*,*) '  : success.'
    else
      write(*,*) '  : unexpected ERROR, value mismatch, got: ', glob_int
      write(*,*) '  :                             should be: ', 5
      passed = .false.
    end if
  end if
  ! -------------------------------- !

  call close_config(L)

  if (passed) then
    write(*,*) 'PASSED'
  else
    write(*,*) 'FAILED'
  end if

contains

  subroutine create_script(filename)
    character(len=*) :: filename

    open(file=trim(filename), unit=22, action='write', status='replace')
    write(22,*) '-- test script for aotus_test'
    write(22,*) 'int_test = 5'
    write(22,*) 'real_test = 0.5'
    write(22,*) 'log_test = true'
    write(22,*) "string_test = 'last words'"
    close(22)
  end subroutine create_script

end program aot_buffer_test
