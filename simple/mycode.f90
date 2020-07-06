program mycode
  use interpreter
  
  character(len=255) :: func, vars, error
  real(8), dimension(2) :: varvals
  
  write(*,*) "Enter function to evaluate: "
  read(*,'(a)') func
  write(*,*) "Enter variables (space separated): "
  read(*,'(a)') vars
  write(*,*) "Enter variable values (2): "
  read(*,*) varvals
  
  r = f_evaluatefn(func, vars, varvals, error)
  
  write(*,*) "Result is: ", r
  write(*,*) "Error: ", trim(error)
end program mycode
