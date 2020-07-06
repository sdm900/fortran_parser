module interpreter

  !*********************************************************************
  !
  !This software is provided AS IS with NO WARRANTY. I also only provide 
  !it for "not for profit" use. If you wish to use the software for any 
  !other purpose, please contact me on sdm900@gmail.com .
  !
  !*********************************************************************

  !
  !  This module implements a function interpreter so that function
  !  form potentials can be used
  !

  implicit none

  private
  public :: f_evaluatefn

  integer, parameter :: fdp=selected_real_kind(10,200)
  integer, parameter :: commandlength=10
  integer, parameter :: variablelength=10
  integer, parameter :: errorlength=255
  character(len=variablelength),  dimension(:), allocatable :: variables
  integer, dimension(:), allocatable :: lenvariables
  real(fdp),  dimension(:), allocatable :: varvalues
  integer :: numvariables
  character(len=errorlength) :: error

contains

  subroutine s_seterror(t_error)
    character(len=*), intent(in) :: t_error

    if (len_trim(t_error) > 0) error = trim(t_error)
  end subroutine s_seterror



  function f_clean(func) result(tmpfunc)
    character(len=*), intent(in) :: func
    character(len=len(func)) :: tmpfunc
    integer :: i, c, l
    
    tmpfunc = adjustl(func)
    l = len_trim(tmpfunc)
    i = 1

    do while (i <= l)
       if (tmpfunc(i:i)== ' ') then
          tmpfunc(i:) = tmpfunc(i+1:)
          l = l - 1
       else
          c = ichar(tmpfunc(i:i))
          if ((c <= ichar('Z')) .and. (c >= ichar('A'))) tmpfunc(i:i) = achar(c-ichar('A')+ichar('a'))
          i = i + 1
       end if
    end do
  end function f_clean



  subroutine s_assignvariables(vars, varvals)
    character(len=*), intent(in) :: vars
    real(fdp), dimension(:), intent(in) :: varvals
    integer :: i, j, c

    if (len_trim(adjustl(vars)) /= 0) then
       numvariables = size(varvals)
       allocate(variables(numvariables), varvalues(numvariables), lenvariables(numvariables))
       read(vars, *) variables
       lenvariables = len_trim(variables)

       do i = 1, numvariables
          do j = 1, lenvariables(i)
             c = ichar(variables(i)(j:j))
             if ((c <= ichar('Z')) .and. (c >= ichar('A'))) variables(i)(j:j) = achar(c-ichar('A')+ichar('a'))
          end do
       end do

       varvalues = varvals
    else
       numvariables = 0
    end if
  end subroutine s_assignvariables



  subroutine s_erasevariables()
    if (allocated(variables)) deallocate(variables)
    if (allocated(lenvariables)) deallocate(lenvariables)
    if (allocated(varvalues)) deallocate(varvalues)
  end subroutine s_erasevariables



  function f_readnumber(func) result(number)
    character(len=*), intent(inout) :: func
    real(fdp) :: number
    integer :: pos
    logical :: dflag, exponent, finish

    exponent = .false.
    dflag = .false.
    finish = .false.
    pos = 0
    number = 0.0d0

    do while(pos < len_trim(func) .and. .not. finish)
       pos = pos + 1

       select case(func(pos:pos))

       case ('0':'9')
          if (dflag) then
             exponent = .true.
             dflag = .false.
          end if

       case ('d','e','D','E')
          if (dflag .or. exponent) then
             finish = .true.
          else
             dflag = .true.
          end if

       case('.')
          if (dflag .or. exponent) then
             finish = .true.
          end if

       case('+','-')
          if (dflag) then
             dflag = .false.
             exponent = .true.
          else
             finish = .true.
          end if

       case default
          finish = .true.

       end select
    end do

    if (pos == len_trim(func) .and. .not. finish) then
       read(func, *) number
       func = ''
    else
       read(func(:pos-1), *) number
       func = func(pos:)
    end if
  end function f_readnumber



  subroutine s_readtext(func, command, number)
    character(len=*), intent(inout) :: func
    character(len=commandlength), intent(out) :: command
    real(fdp), intent(out) :: number
    integer :: i, funccut, lenfunc
    integer, parameter :: numcommands = 16
    character(len=commandlength), dimension(numcommands), parameter :: &
         commands = (/ 'cos(  ','sin(  ','tan(  ','exp(  ','log(  ','abs(  ','aint( ','sqrt( ',&
         'acos( ','asin( ','atan( ','cosh( ','sinh( ','tanh( ','anint(','log10(' /)
    integer, dimension(numcommands), parameter :: lencommands = len_trim(commands)

    command = ''
    number = 0.0d0
    funccut = 0
    lenfunc = len_trim(func)

    do i = 1, numcommands
       if (lenfunc >= lencommands(i)) then
          if (func(:lencommands(i))==commands(i)(:lencommands(i)) .and. lencommands(i) > funccut) then
             command = commands(i)(:lencommands(i))
             number = 0.0d0
             funccut = lencommands(i)-1
          end if
       end if
    end do

    if (command == '') then
       do i = 1, numvariables
          if (lenfunc >= lenvariables(i)) then
             if (func(:lenvariables(i)) == variables(i)(:lenvariables(i)) .and. lenvariables(i) > funccut) then
                number = varvalues(i)
                command = 'variable'
                funccut = lenvariables(i)
             end if
          end if
       end do
    end if

    if (command == '') call s_seterror('Unknown command or variable: '//trim(func))
    
    if (lenfunc > funccut) then
       func = func(funccut+1:)
    else
       func = ''
    end if

    if (funccut == 0) func = func(2:)
  end subroutine s_readtext



  function f_readoperator(func) result(operator)
    character(len=*), intent(inout) :: func
    character(len=commandlength) :: operator
    integer :: i, lenfunc
    integer, parameter :: numoperators=15
    character(len=2), dimension(numoperators), parameter :: &
         operators = (/ '**', '!=', '==', '>=', '=>', '<=', '=<', '= ', &
       '> ', '< ', '^ ', '* ', '/ ', '+ ', '- ' /)
    integer, dimension(numoperators), parameter :: &
         lenoperators = len_trim(operators)

    operator = ''
    lenfunc = len_trim(func)

    do i = 1, numoperators
       if (lenfunc > lenoperators(i)) then
          if (func(:lenoperators(i)) == operators(i)(:lenoperators(i)) .and. lenoperators(i) &
               > len_trim(operator)) operator = operators(i)
       end if
    end do

    if (operator == '') call s_seterror('Unknown operator: '//trim(func))
    
    if (len_trim(func) > len_trim(operator)) then
       func = func(len_trim(operator)+1:)
    else
       func = ''
    end if
  end function f_readoperator



  function f_readbracket(func) result(subfunc)
    character(len=*), intent(inout) :: func
    character(len = len(func)) :: subfunc
    integer :: pos, count, lenfunc
    
    pos = 0
    count = 0
    lenfunc = len_trim(func)

    do while((pos < lenfunc) .and. (count >= 0))
       pos = pos + 1

       select case(func(pos:pos))

       case('(')
          count = count + 1

       case(')')
          count = count - 1

       end select

       if (count == 0) exit
    end do

    subfunc = trim(func(2:pos-1))

    if (count /= 0) then
       call s_seterror('Unbalanced brackets: '//trim(func))
       func = ''
       subfunc = ''
    else if (len_trim(subfunc) == 0) then
       call s_seterror('Empty brackets encountered: '//trim(func))
       func = ''
       subfunc = ''
    else if (len_trim(func) < pos+1) then
       func = ''
    else
       func = func(pos+1:)
    end if
  end function f_readbracket



  recursive function f_compute(func, value, command) result(number)
    character(len=*), intent(inout) :: func
    real(fdp), intent(in) :: value
    character(len = commandlength), intent(in) :: command
    real(fdp) :: number, nextnumber, tmp
    character(len = commandlength) :: nextcommand
    character(len = len(func)) :: func_t

    select case(command)

    case('number')
       number = value

    case('variable')
       number = value

    case('bracket')
       func_t = f_readbracket(func)
       number = f_evaluate(func_t)

    case('nop')
       number = value

    case('end')
       number = value

    case('+')
       number = value + f_evaluateblock(func, command)

    case('-')
       number = value - f_evaluateblock(func, command)

    case('*')
       number = value * f_evaluateblock(func, command)

    case('/')
       tmp = f_evaluateblock(func, command)

       if (tmp /= 0.0d0) then
          number = value / tmp
       else
          call s_seterror('x/y, y=0')
       end if

    case('**', '^')
       tmp = f_evaluateblock(func, command)

       if (value > 0.0d0) then
          number = value ** tmp
       elseif (value < 0.0d0) then
          if (aint(tmp) == tmp) then
             number = value ** nint(tmp)
          else
             call s_seterror('x**y, x<0 and y not integer')
          end if
       elseif (tmp /= 0.0d0) then
          number = 0.0d0
       else             
          call s_seterror('Undefined result 0**0')
       end if

    case('==', '=')
       number = merge(1.0d0, 0.0d0, value==f_evaluateblock(func, command))

    case('!=')
       number = merge(1.0d0, 0.0d0, value/=f_evaluateblock(func, command))

    case('>=', '=>')
       number = merge(1.0d0, 0.0d0, value>=f_evaluateblock(func, command))

    case('<=', '=<')
       number = merge(1.0d0, 0.0d0, value<=f_evaluateblock(func, command))

    case('<')
       number = merge(1.0d0, 0.0d0, value<f_evaluateblock(func, command))

    case('>')
       number = merge(1.0d0, 0.0d0, value>f_evaluateblock(func, command))

    case('cos(')
       call s_getnextatom(func, nextnumber, nextcommand)
       number = cos(f_compute(func, nextnumber, nextcommand))

    case('sin(')
       call s_getnextatom(func, nextnumber, nextcommand)
       number = sin(f_compute(func, nextnumber, nextcommand))

    case('tan(')
       call s_getnextatom(func, nextnumber, nextcommand)
       number = tan(f_compute(func, nextnumber, nextcommand))

    case('exp(')
       call s_getnextatom(func, nextnumber, nextcommand)
       number = exp(f_compute(func, nextnumber, nextcommand))

    case('log(')
       call s_getnextatom(func, nextnumber, nextcommand)
       tmp = f_compute(func, nextnumber, nextcommand)
       
       if (tmp > 0.0d0) then
          number = log(tmp)
       else
          call s_seterror('log(x), x<=0')
       end if

    case('log10(')
       call s_getnextatom(func, nextnumber, nextcommand)
       tmp = f_compute(func, nextnumber, nextcommand)

       if (tmp > 0.0d0) then
          number = log10(tmp)
       else
          call s_seterror('log10(x), x<=0')
       end if

    case('sqrt(')
       call s_getnextatom(func, nextnumber, nextcommand)
       tmp = f_compute(func, nextnumber, nextcommand)

       if (tmp >= 0.0d0) then
          number = sqrt(tmp)
       else
          call s_seterror('sqrt(x), x<0')
       end if

    case('acos(')
       call s_getnextatom(func, nextnumber, nextcommand)
       tmp = f_compute(func, nextnumber, nextcommand)
          
       if (abs(tmp) <= 1.0d0) then
          number = acos(tmp)
       else
          call s_seterror('acos(x), |x|>1')
       end if

    case('asin(')
       call s_getnextatom(func, nextnumber, nextcommand)
       tmp = f_compute(func, nextnumber, nextcommand)

       if (abs(tmp) <= 1.0d0) then
          number = asin(tmp)
       else
          call s_seterror('asin(x), |x|>1')
       end if

    case('atan(')
       call s_getnextatom(func, nextnumber, nextcommand)
       number = atan(f_compute(func, nextnumber, nextcommand))

    case('cosh(')
       call s_getnextatom(func, nextnumber, nextcommand)
       number = cosh(f_compute(func, nextnumber, nextcommand))

    case('sinh(')
       call s_getnextatom(func, nextnumber, nextcommand)
       number = sinh(f_compute(func, nextnumber, nextcommand))

    case('tanh(')
       call s_getnextatom(func, nextnumber, nextcommand)
       number = tanh(f_compute(func, nextnumber, nextcommand))

    case('anint(')
       call s_getnextatom(func, nextnumber, nextcommand)
       number = anint(f_compute(func, nextnumber, nextcommand))

    case('aint(')
       call s_getnextatom(func, nextnumber, nextcommand)
       number = aint(f_compute(func, nextnumber, nextcommand))

    case('abs(')
       call s_getnextatom(func, nextnumber, nextcommand)
       number = abs(f_compute(func, nextnumber, nextcommand))

    case default
       call s_seterror('Unknown input: '//command)

    end select
  end function f_compute



  subroutine s_getnextatom(func, number, command)
    character(len=*), intent(inout) :: func
    real(fdp), intent(inout) :: number
    character(len = commandlength), intent(inout) :: command

    command = ''

    do while (command == '' .or. command == 'nop')

       command = ''
       
       select case(func(1:1))
          
       case('0':'9', '.')
          number = f_readnumber(func)
          command = 'number'
          
       case('+', '-', '/', '*', '^', '!', '<', '>', '=')
          command = f_readoperator(func)
          
       case('a':'z','A':'Z')
          call s_readtext(func, command, number)
          
       case('(')
          command = 'bracket'

       case(' ')
          if (len_trim(func) > 1) then
             command = 'nop'
             func = func(2:)
          else
             func = ''
             command = 'end'
          end if

       case default
          if (len_trim(func) > 1) then
             call s_seterror('Unknown input: '//trim(func))
             func = func(2:)
             command = 'nop'
          else
             func = ''
             command = 'end'
          end if
       end select
    end do
  end subroutine s_getnextatom



  function f_order(command) result(order)
    character(len=*), intent(in) :: command
    integer :: order

    order = 0
    
    select case(trim(command))

    case('!=','==','>=','=>','<=','=<','=','<','>')
       order = 1

    case('+','-')
       order = 10

    case('*','/')
       order = 100

    case('**','^')
       order = 1000

    end select
  end function f_order
  
  
    
  subroutine s_peeknextatom(func, number, command)
    character(len=*), intent(in) :: func
    real(fdp), intent(inout) :: number
    character(len = commandlength), intent(inout) :: command
    character(len = len(func)) :: func_t

    func_t = func
    call s_getnextatom(func_t, number, command)
  end subroutine s_peeknextatom


  recursive function f_evaluateblock(func, blockcommand) result(number)
    character(len=*), intent(inout) :: func
    character(len=*), intent(in) :: blockcommand
    real(fdp) :: number
    character(len = commandlength) :: command, nextcommand
    real(fdp) :: nextnumber

    call s_peeknextatom(func, nextnumber, nextcommand)

    if (trim(nextcommand) == 'end') call s_seterror('Premature end of function')

    do while((f_order(nextcommand) == 0 .or. (f_order(nextcommand) > f_order(blockcommand))) .and. trim(nextcommand) /= 'end')
       call s_getnextatom(func, number, command)
       number = f_compute(func, number, command)
       call s_peeknextatom(func, nextnumber, nextcommand)
    end do
  end function f_evaluateblock



  recursive function f_evaluate(func) result(number)
    character(len=*), intent(inout) :: func
    real(fdp) :: number
    character(len = commandlength) :: command

    number = 0
    command = ''

    do while(command /= 'end')
       call s_getnextatom(func, number, command)
       number = f_compute(func, number, command)
    end do
  end function f_evaluate



  function f_evaluatefn(func, vars, varvals, t_error) result(number)
    character(len=*), intent(in) :: func, vars
    real(fdp), dimension(:), intent(in) :: varvals
    character(len=*), intent(out), optional :: t_error
    real(fdp) :: number
    character(len=len_trim(func)) :: func_t
    
    error = 'OK'
    call s_assignvariables(vars, varvals)
    func_t = f_clean(func)
    number = f_evaluate(func_t)

    if (present(t_error)) t_error = error

    call s_erasevariables()
  end function f_evaluatefn

end module interpreter
