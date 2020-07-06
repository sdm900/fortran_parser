program test

  !*********************************************************************
  !
  !This software is provided AS IS with NO WARRANTY. I also only provide
  !it for "not for profit" use. If you wish to use the software for any
  !other purpose, please contact me on sdm900@gmail.com .
  !
  !*********************************************************************

  use precision
  use interpreter


  character(len = 50) :: s, f
  real(fdp), dimension(2) :: t
  character(len=50) :: error

  read(*,'(a)') f
  read(*,'(a)') s
  read(*,*) t

  write(*,*) ''
  write(*,*) ''

  write(*,*) 'Without the error field'
  write(*,*) '======================='
  write(*,*) f_evaluatefn(f, s, t)
  
  write(*,*) ''
  write(*,*) ''

  write(*,*) 'With the error field'
  write(*,*) '===================='
  write(*,*) f_evaluatefn(f, s, t, error)
  write(*,*) error
  
end program test
