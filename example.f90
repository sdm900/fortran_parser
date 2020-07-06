program example
  !
  ! First, "import" all the module
  !
  use interpreter

  implicit none

  !
  ! Functions are interpreted and evaulated separately for performance.
  !
  ! The first part is to interpret the function into byte code and then
  ! evaluate the byte code for specific values of variables
  !

  character(len=1000) :: function, variables, error
  real(8), dimension(5) :: values
  real(8) :: result
  integer :: funcnum, x, y

  !
  ! define a function as a string.  This can obviously be read from
  ! an input file or from the command line if necessary
  !
  function="a*x+b+c*sin(y)"
  
  !
  ! define the variable names
  !
  variables="a b c x y"

  !
  ! parse the function into byte code and get back a function number
  !  
  ! You can parse multiple functions and store the reference for later use
  !
  call s_createfn(function, variables, funcnum, error)

  !
  ! We can check the error if we want, in this case we are printing it off
  !
  write(*,*) trim(error)

  !
  ! Now we can evaulate the function for some set of vaules
  !

  !
  ! define the variable values
  !
  values=(/ 10, 20, 30, 40, 50 /)

  !
  ! Evaulate
  !
  call s_evaluatefn(funcnum, values, result, error)

  !
  ! Again, check the error
  !
  ! The error string is optional... leave it out of the call if you want
  !
  write(*,*) trim(error)
  
  !
  ! Now display the result of the function
  !
  write(*,*) "Evaulating ", trim(function), " for variables ", trim(variables), " with values ", values
  write(*,*) "results in ", result

  !
  ! If you need to, you can actually evaulate the same function for lots of different values...
  !

  do x=1,10
     do y=20,30
        values(1)=x
        values(2)=y
        call s_evaluatefn(funcnum, values, result, error)
        write(*,*) trim(error), result
     end do
  end do

  !
  ! once we are finished with the function, destroy it and clean up
  !
  call s_destroyfn(funcnum)
end program example

  
