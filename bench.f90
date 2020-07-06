program check

  !*********************************************************************
  !
  !This software is provided AS IS with NO WARRANTY. I also only provide 
  !it for "not for profit" use. If you wish to use the software for any 
  !other purpose, please contact me on sdm900@gmail.com
  !
  !*********************************************************************

  use extrafunc
  use interpreter

  implicit none

  character(len=255) :: s, f
  real(8) :: x, y, z, r
  integer i, funcnum
  character(len=255) :: error1, error2


  f = '(x +y+z+x*y +x*z+y*z+x/y +x /z+y/z+x*cos ( x)    +y*sin(y)+z*tan(z)&
       &)**2/(x+y+z+x*y+x*z+y*z+x/y+x/z+y/z+x*cos(x)+y*sin(y)+z*tan(z))**3&
       &+sqrt(x*y*z+x+y+z)*exp(sqrt(x**2+y**2+z**2)+x+y+z)'
  s = 'x y z'
  x = 1.1d0
  y = 2.2d0
  z = 3.3d0
  call s_createfn(f, s, funcnum, error1)
  do i = 1, 500000
    call s_evaluatefn(funcnum, (/x, y, z/), r, error2)
  end do
  write(*,*)r

end program check
