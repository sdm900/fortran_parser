module checkfn

  use interpreter

  implicit none

  integer, parameter :: fdp=selected_real_kind(10,200)
  integer, save :: count = 0

contains

  subroutine s_checkfn(f, s, varvals, err, res, tol)
    character(len=255), intent(in) :: f, s
    real(fdp), dimension(:), intent(in) :: varvals
    character(len=255), optional, intent(in) :: err
    real(fdp), optional, intent(in) :: res, tol
    character(len=255) :: error
    real(fdp) :: r, ttol

    count = count + 1

    r = f_evaluatefn(f, s, varvals, error)
    
    if (present(tol)) then
       ttol = tol
    else
       ttol = epsilon(r)
    end if

    if (present(err)) then
       if (trim(error) == trim(err)) then
          write(*,'(i4.4,a)') count, '  Testing(ERROR)           :  PASSED'
       else
          write(*,'(a)') '==============================='
          write(*, '(a,i4.4)') 'Test(ERROR): ', count
          write(*,'(a,a)') 'Func: ', trim(f)
          write(*,'(a,a,a)') 'Error: "',trim(error),'"'
          write(*,'(a,e18.10e3)') 'Evaluate value: ', r
          write(*,'(a)') '**** ERROR:         FAIL'
          write(*,'(a,a,a)') '**** Expecting: "', trim(err),'"'
          write(*,'(a)') '==============================='
       end if
    end if

    if (present(res)) then
       if (abs(r-res) <= ttol) then
          write(*,'(i4.4,a)') count, '  Testing(EVALUATION)      :  PASSED'
       else
          write(*,'(a)') '==============================='
          write(*, '(a,i4.4)') 'Test(EVALUATION): ', count
          write(*,'(a,a)') 'Func: ', trim(f)
          write(*,'(a,a,a)') 'Error: "',trim(error),'"'
          write(*,'(a,e18.10e3)') 'Evaluate value: ', r
          write(*,'(a)') '**** EVALUATION:    FAIL'
          write(*,'(a,e18.10e3)') '**** Expecting: ', res
          write(*,'(a,e18.10e3)') '**** Difference: ', res-r
          write(*,'(a)') '==============================='
       end if
    end if
  end subroutine s_checkfn

end module checkfn



program check

  !*********************************************************************
  !
  !This software is provided AS IS with NO WARRANTY. I also only provide 
  !it for "not for profit" use. If you wish to use the software for any 
  !other purpose, please contact me on sdm900@gmail.com .
  !
  !*********************************************************************

  use interpreter
  use checkfn

  implicit none

  character(len=255) :: s, f
  character(len=255) :: error
  real(fdp) :: w, x, y, z, r


  !
  ! Test 1
  !
  f = '3.14159265359'
  s = ''
  r = 3.14159265359d0
  call s_checkfn(f, s, (/0.0d0/), res=r)

  !
  ! Test 2
  !
  f = 'x'
  s = 'x'
  x = 3.14159265359d0
  r = x
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 3
  !
  f = 'x**y'
  s = 'x y'
  x = 2.0d0
  y = 3.0d0
  r = x**y
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 4
  !
  f = 'x!=y'
  s = 'x y'
  x = 2.0d0
  y = 3.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 5
  !
  f = 'x!=y'
  s = 'x y'
  x = 2.0d0
  y = 2.0d0
  r = 0.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 6
  !
  f = 'x==y'
  s = 'x y'
  x = 3.0d0
  y = 3.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 7
  !
  f = 'x==y'
  s = 'x y'
  x = 3.0d0
  y = 2.0d0
  r = 0.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 8
  !
  f = 'x>=y'
  s = 'x y'
  x = 4.0d0
  y = 3.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 9
  !
  f = 'x>=y'
  s = 'x y'
  x = 3.0d0
  y = 4.0d0
  r = 0.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 10
  !
  f = 'x<=y'
  s = 'x y'
  x = 2.0d0
  y = 3.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 11
  !
  f = 'x<=y'
  s = 'x y'
  x = 3.0d0
  y = 2.0d0
  r = 0.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 12
  !
  f = 'x>y'
  s = 'x y'
  x = 4.0d0
  y = 3.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 13
  !
  f = 'x>y'
  s = 'x y'
  x = 3.0d0
  y = 4.0d0
  r = 0.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 14
  !
  f = 'x<y'
  s = 'x y'
  x = 2.0d0
  y = 3.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 15
  !
  f = 'x<y'
  s = 'x y'
  x = 3.0d0
  y = 2.0d0
  r = 0.0d0
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 16
  !
  f = 'x*y'
  s = 'x y'
  x = 3.0d0
  y = 2.0d0
  r = x*y
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 17
  !
  f = 'x/y'
  s = 'x y'
  x = 3.0d0
  y = 2.0d0
  r = x/y
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 18
  !
  f = 'x/y'
  s = 'x y'
  x = 3.0d0
  y = 0.0d0
  error = 'x/y, y=0'
  call s_checkfn(f, s, (/x, y/), err=error)

  !
  ! Test 19
  !
  f = 'x+y'
  s = 'x y'
  x = 3.0d0
  y = 2.0d0
  r = x+y
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 20
  !
  f = 'x-y'
  s = 'x y'
  x = 3.0d0
  y = 2.0d0
  r = x-y
  call s_checkfn(f, s, (/x, y/), res=r)

  !
  ! Test 21
  !
  f = 'cos(x)'
  s = 'x'
  x = 3.0d0
  r = cos(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 22
  !
  f = 'sin(x)'
  s = 'x'
  x = 3.0d0
  r = sin(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 23
  !
  f = 'tan(x)'
  s = 'x'
  x = 3.0d0
  r = tan(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 24
  !
  f = 'exp(x)'
  s = 'x'
  x = 3.0d0
  r = exp(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 25
  !
  f = 'log(x)'
  s = 'x'
  x = 3.0d0
  r = log(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 26
  !
  f = 'log(x)'
  s = 'x'
  x = -3.0d0
  error = 'log(x), x<=0'
  call s_checkfn(f, s, (/x/), err=error)

  !
  ! Test 27
  !
  f = 'log10(x)'
  s = 'x'
  x = 3.0d0
  r = log10(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 28
  !
  f = 'log10(x)'
  s = 'x'
  x = -3.0d0
  error = 'log10(x), x<=0'
  call s_checkfn(f, s, (/x/), err=error)

  !
  ! Test 29
  !
  f = 'sqrt(x)'
  s = 'x'
  x = 3.0d0
  r = sqrt(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 30
  !
  f = 'sqrt(x)'
  s = 'x'
  x = -3.0d0
  error = 'sqrt(x), x<0'
  call s_checkfn(f, s, (/x/), err=error)

  !
  ! Test 31
  !
  f = 'acos(x)'
  s = 'x'
  x = 0.3d0
  r = acos(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 32
  !
  f = 'acos(x)'
  s = 'x'
  x = -3.0d0
  error = 'acos(x), |x|>1'
  call s_checkfn(f, s, (/x/), err=error)

  !
  ! Test 33
  !
  f = 'asin(x)'
  s = 'x'
  x = 0.3d0
  r = asin(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 34
  !
  f = 'asin(x)'
  s = 'x'
  x = -3.0d0
  error = 'asin(x), |x|>1'
  call s_checkfn(f, s, (/x/), err=error)

  !
  ! Test 35
  !
  f = 'atan(x)'
  s = 'x'
  x = 0.3d0
  r = atan(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 36
  !
  f = 'cosh(x)'
  s = 'x'
  x = 0.3d0
  r = cosh(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 37
  !
  f = 'sinh(x)'
  s = 'x'
  x = 0.3d0
  r = sinh(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 38
  !
  f = 'tanh(x)'
  s = 'x'
  x = 0.3d0
  r = tanh(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 39
  !
  f = 'anint(x)'
  s = 'x'
  x = 1.3d0
  r = anint(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 40
  !
  f = 'aint(x)'
  s = 'x'
  x = 1.7d0
  r = aint(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 41
  !
  f = 'abs(x)'
  s = 'x'
  x = -1.7d0
  r = abs(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  !  Test 42
  !
  f = '(x +y+z+x*y +x*z+y*z+x/y +x /z+y/z+x*cos ( x)    +y*sin(y)+z*tan(z)&
       &)**2/(x+y+z+x*y+x*z+y*z+x/y+x/z+y/z+x*cos(x)+y*sin(y)+z*tan(z))**3&
       &+sqrt(x*y*z+x+y+z)*exp(sqrt(x**2+y**2+z**2)+x+y+z)'
  s = 'x y z'
  x = 1.1d0
  y = 2.2d0
  z = 3.3d0
  r = (x +y+z+x*y +x*z+y*z+x/y +x /z+y/z+x*cos ( x)    +y*sin(y)+z*tan(z)&
       &)**2/(x+y+z+x*y+x*z+y*z+x/y+x/z+y/z+x*cos(x)+y*sin(y)+z*tan(z))**3&
       &+sqrt(x*y*z+x+y+z)*exp(sqrt(x**2+y**2+z**2)+x+y+z)
  call s_checkfn(f, s, (/x, y, z/), res=r)

  !
  !  Test 43
  !
  f = 'cos(x)+sin(y)+z**2'
  s = 'x y z'
  x = 1.1d0
  y = 2.2d0
  z = 3.3d0
  r = cos(x)+sin(y)+z**2
  call s_checkfn(f, s, (/x, y, z/), res=r)

  !
  !  Test 44
  !
  f = 'x+y*z+1-cos(x**y)'
  s = 'x y z'
  x = 1.1d0
  y = 2.2d0
  z = 3.3d0
  r = x+y*z+1-cos(x**y)
  call s_checkfn(f, s, (/x, y, z/), res=r)

  !
  ! Test 45
  !
  f = '7.1323e-05'
  s = ''
  r = 7.1323d-05
  call s_checkfn(f, s, (/0.0d0/), res=r)

  !
  ! Test 46
  !
  f = '7.13230000E-05'
  s = ''
  r = 7.1323d-05
  call s_checkfn(f, s, (/0.0d0/), res=r)

  !
  ! Test 47
  !
  f = '(((a+b)/(a+b)-exp(b)+exp(b))/2.0)-const'
  s = 'a b const'
  x = 2.d0
  y = 1.5d0
  z = -1.d0
  r = (((x+y)/(x+y)-exp(y)+exp(y))/2.0)-z
  call s_checkfn(f, s, (/x, y, z/), res=r)

  !
  ! Test 48
  !
  f = '((x+y)*z'
  s = 'x y z'
  x = 1.0d0
  y = 2.0d0
  z = 3.0d0
  error='Unbalanced brackets: ((x+y)*z'
  call s_checkfn(f, s, (/x, y, z/), err=error)

  !
  ! Test 49
  !
  f = '0**0'
  s = ''
  error = 'Undefined result 0**0'
  call s_checkfn(f, s, (/0.0d0/), err=error)

  !
  ! Test 50
  !
  f = '-1**4'
  s = ''
  r = -1.0d0**4.0d0
  call s_checkfn(f, s, (/0.0d0/), res=r)

  !
  ! Test 51
  !
  f = '(-1)**4.1'
  s = ''
  error = 'x**y, x<0 and y not integer'
  call s_checkfn(f, s, (/0.0d0/), err=error)

  !
  ! Test 52
  !
  f = 'TAN(x)'
  s = 'x'
  x = 3.0d0
  r = tan(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 53
  !
  f = 'TaN(x)'
  s = 'x'
  x = 3.0d0
  r = tan(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 54
  !
  f = 'taN(x)'
  s = 'x'
  x = 3.0d0
  r = tan(x)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 55
  !
  f = '2*(3+2)'
  s = 'x'
  x = 3.0d0
  r = 2*(3+2)
  call s_checkfn(f, s, (/x/), res=r)

  !
  ! Test 56
  !
  f = '2*(3+2'
  s = 'x'
  x = 4.0d0
  error = 'Unbalanced brackets: (3+2'
  call s_checkfn(f, s, (/x/), err=error)

  !
  ! Test 57
  !
  f = 'TAN(1)'
  s = ' '
  r = tan(1.0d0)
  call s_checkfn(f, s, (/ 0.0d0 /), res=r)

  !
  ! Test 58
  !
  f = '(-2.0d0)**2.0d0'
  s = ' '
  r = 4.0d0
  call s_checkfn(f, s, (/ 0.0d0 /), res=r)

  !
  ! Test 59
  !
  f = '(-1)**(4.1)'
  s = ''
  error = 'x**y, x<0 and y not integer'
  call s_checkfn(f, s, (/0.0d0/), err=error)

  !
  !  Test 60
  !
  f = 'cos(x)+sin(y)+z**2'
  s = ' x  y   z '
  x = 1.1d0
  y = 2.2d0
  z = 3.3d0
  r = cos(x)+sin(y)+z**2
  call s_checkfn(f, s, (/x, y, z/), res=r)

  !
  !  Test 61
  !
  f = 'x+1'
  s = 'x'
  x = 0.0d0
  error = 'OK'
  call s_checkfn(f, s, (/x/), err=error)

  !
  ! Test 62
  !
  f = '-1'
  s = ''
  r = -1.0d0
  error = 'OK'
  call s_checkfn(f, s, (/ 0.0d0 /), err=error)

  !
  ! Test 63
  !
  f = 'cos(x_y)+cos(x)+sin(y)+y_x**2'
  s = ' x  y   x_y  y_x'
  w = 0.5d0
  x = 1.1d0
  y = 2.2d0
  z = 3.3d0
  r = cos(y)+cos(w)+sin(x)+z**2
  call s_checkfn(f, s, (/w, x, y, z/), res=r)

  !
  ! Test 64
  !
  f = '(-1)/(-2)'
  s = ''
  r = 0.5d0
  error = 'OK'
  call s_checkfn(f, s, (/ 0.0d0 /), res=r)

  !
  ! Test 65
  !
  f = '(-1)/(2)'
  s = ''
  r = -0.5d0
  error = 'OK'
  call s_checkfn(f, s, (/ 0.0d0 /), res=r)

  !
  ! Test 66
  !
  f = '(1)/(2)'
  s = ''
  r = 0.5d0
  error = 'OK'
  call s_checkfn(f, s, (/ 0.0d0 /), res=r)

  !
  ! Test 67
  !
  f = '(1)/(-2)'
  s = ''
  r = -0.5d0
  error = 'OK'
  call s_checkfn(f, s, (/ 0.0d0 /), res=r)

  !
  ! Test 68
  !
  f = '-(1)/(2)'
  s = ''
  r = -0.5d0
  error = 'OK'
  call s_checkfn(f, s, (/ 0.0d0 /), res=r)

  !
  ! Test 69
  !
  f = 'x_y+y'
  s = 'x_y y'
  x = 1.0d0
  y = 2.0d0
  r = 3.0d0
  call s_checkfn(f, s, (/ x, y /), res=r)

  !
  ! Test 70
  !
  f = 'x**0'
  s = 'x'
  x = 2.0d0
  r = 1.0d0
  call s_checkfn(f, s, (/ x /), res=r)

  !
  ! Test 71
  !
  f = 'K +4.0d0*K*CL'
  s = 'K CL'
  r = 1.0d0+4.0d0*1.0d0*1.0d0
  call s_checkfn(f, s, (/ 1.0d0, 1.0d0 /), res=r)

  !
  ! Test 72
  !
  f = 'K +(4.0d0*K*CL)'
  s = 'K CL'
  r = 1.0d0+4.0d0*1.0d0*1.0d0
  call s_checkfn(f, s, (/ 1.0d0, 1.0d0 /), res=r)

  !
  ! Test 73
  !
  f = 'K +(4.0d0*K*CLL)'
  s = 'K CL CLL'
  r = 1.0d0+4.0d0*1.0d0*2.0d0
  call s_checkfn(f, s, (/ 1.0d0, 1.0d0, 2.0d0 /), res=r)

  !
  ! Test 74
  !
  f = 'K +4.0d0*K*CLL'
  s = 'K CL CLL'
  r = 1.0d0+4.0d0*1.0d0*2.0d0
  call s_checkfn(f, s, (/ 1.0d0, 1.0d0, 2.0d0 /), res=r)

end program check
