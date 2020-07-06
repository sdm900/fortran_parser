program speed
  use interpreter
  implicit none

  integer, parameter :: N=10000000
  character(200) :: s, v, e
  real(8) :: x, y, r, t
  integer :: i, ff, c1, c2, cr, cm

  r = 0.0d0
  
  call system_clock(c1, cr, cm)
  do i = 1, N
     x = 500.0d0 - dble(i)/1000.0d0
     y = 100.0d0 - dble(i)/5000.0d0
     r = r + f(x, y)
  end do
  call system_clock(c2)

  write(*, *) "Compiled code time= ", dble(c2-c1)/dble(cr)
  write(*, *) "Compiled code result= ", r

  s = '0.5d0 - 0.5d0*cos(x-2.0d0*y) '
  v = 'x y'
  call s_createfn(s, v, ff, e)
  r = 0.0d0
  
  call system_clock(c1)
  do i = 1, N
     x = 500.0d0 - dble(i)/1000.0d0
     y = 100.0d0 - dble(i)/5000.0d0
     call s_evaluatefn(ff, (/ x, y /), t, e)
     r = r + t
  end do
  call system_clock(c2)
     
  write(*,*) "Parsed code time= ", dble(c2-c1)/dble(cr)
  write(*,*) "Parsed code result= ", r

contains

  function f(x,y)
    real(8) :: f, x, y

    f = 0.5d0 - 0.5d0*cos(x-2.0d0*y)
  end function f
end program speed
