program aot_path_test

  use aot_path_module,  only: aot_path_type,     &
    &                         aot_init_path,     &
    &                         aot_path_addNode,  &
    &                         aot_path_delNode,  &
    &                         aot_path_toString, &
    &                         aot_path_dump

  implicit none

  type(aot_path_type) :: path
  character(len=256) :: buffer
  character(len=10) :: shortBuffer
  logical :: passed = .true.

  call aot_init_path(path)

  call aot_path_addNode( me = path, NodeType = 'table', key = 'level1' )
  call aot_path_addNode( me = path, NodeType = 'table', key = 'level2' )
  call aot_path_addNode( me = path, NodeType = 'table', key = 'level3' )

  ! Test whether the conversion works at all
  call aot_path_toString( path, buffer )
  if (buffer /= 'level1.level2.level3') then
    write(*,*) buffer
    write(*,*) 'Converting path into string failed'
    passed = .false.
  end if

  call aot_path_delNode( path )

  call aot_path_toString( path, buffer )
  if (buffer /= 'level1.level2') then
    write(*,*) 'Converting path into string failed after deleting last node'
    passed = .false.
  end if

  ! Does it return an empty buffer when the buffer is to short?
  shortBuffer = 'test'
  call aot_path_toString( path, shortBuffer )
  if (shortBuffer /= '') then
    write(*,*) 'Buffer not empty when result is too long.'
    passed = .false.
  end if

  if (passed) then
    write(*,*) 'PASSED'
  else
    write(*,*) 'FAILED'
  end if

end program aot_path_test
