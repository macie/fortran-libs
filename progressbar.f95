!************************************************************
! Progress indicators library.
! 
! (gfortran needs compilation parameter: -fbackslash)
!
! Maciej Żok, 2010 MIT License
! https://github.com/macie/fortran-libs


subroutine progress_bar(iteration, maximum)
!
! Prints progress bar.
!
! Args: 
!     iteration - iteration number
!     maximum - total iterations
!
    implicit none
    integer :: iteration, maximum
    integer :: counter
    integer :: step, done

    step = nint(iteration * 100 / (1.0 * maximum))
    done = floor(step / 10.0)  ! mark every 10%

    do counter = 1, 36                    ! clear whole line - 36 chars
        write(6,'(a)',advance='no') '\b'  ! (\b - backslash)
    end do

    write(6,'(a)',advance='no') ' -> In progress... ['
    if (done .LE. 0) then
        do counter = 1, 10
            write(6,'(a)',advance='no') '='
        end do
    else if ((done .GT. 0) .and. (done .LT. 10)) then
        do counter = 1, done
            write(6,'(a)',advance='no') '#'
        end do
        do counter = done+1, 10
            write(6,'(a)',advance='no') '='
        end do 
    else
        do counter = 1, 10
            write(6,'(a)',advance='no') '#'
        end do
    end if
    write(6,'(a)',advance='no') '] '
    write(6,'(I3.1)',advance='no') step
    write(6,'(a)',advance='no') '%'
end


subroutine remaining_time(iteration, maximum)
!
! Prints remaining time.
!
! Args: 
!     iteration - iteration number
!     maximum - total iterations
!
    implicit none
    integer :: iteration, maximum
    integer :: counter
    integer :: hours, minutes, seconds
    real :: tarray(2), current, remains

    call etime(tarray, current)

    remains = 2 * current * (maximum / (1.0 * iteration) - 1)
    hours = floor(remains / 3600)
    minutes = floor((remains - hours * 3600) / 60)
    seconds = nint(remains - (hours * 3600 + minutes * 60))

    do counter = 1, 38                    ! clear whole line - 38 chars
        write(6,'(a)',advance='no') '\b'  ! (\b - backslash)
    end do

    write(6,'(a)',advance='no') ' -> Remaining time (h:m:s): '
    write(6,'(I4.1)',advance='no') hours
    write(6,'(a)',advance='no') ':'
    write(6,'(I2.2)',advance='no') minutes
    write(6,'(a)',advance='no') ':'
    write(6,'(I2.2)',advance='no') seconds
end


subroutine progress_bar_time(iteration, maximum)
!
! Prints progress bar with remaining time.
!
! Args: 
!     iteration - iteration number
!     maximum - total iterations
!
    implicit none
    integer :: iteration, maximum
    integer :: counter
    integer :: hours, minutes, seconds
    integer :: step, done
    real :: tarray(2), current, remains

    call etime(tarray, current)
    
    remains = 2 * current * (maximum / (1.0 * iteration) - 1)
    hours = floor(remains / 3600)
    minutes = floor((remains - hours * 3600) / 60)
    seconds = nint(remains - (hours * 3600 + minutes * 60))

    step = nint(100/(1.0*maximum)*iteration)
    done = floor(step / 10.0)  ! mark every 10%

    do counter = 1, 63                    ! clear whole line - 63 chars
        write(6,'(a)',advance='no') '\b'  ! (\b - backslash)
    end do  

    write(6,'(a)',advance='no') ' -> In progress... ['
    if (done .LE. 0) then
        do counter = 1, 10
            write(6,'(a)',advance='no') '='
        end do
    else if ((done .GT. 0) .and. (done .LT. 10)) then
        do counter = 1, done
            write(6,'(a)',advance='no') '#'
        end do
        do counter = done+1, 10
            write(6,'(a)',advance='no') '='
        end do  
    else
        do counter = 1, 10
            write(6,'(a)',advance='no') '#'
        end do
    end if
    write(6,'(a)',advance='no') '] '
    write(6,'(I3.1)',advance='no') x
    write(6,'(a)',advance='no') '% (remaining '
    write(6,'(I4.1)',advance='no') hours
    write(6,'(a)',advance='no') ':'
    write(6,'(I2.2)',advance='no') minutes
    write(6,'(a)',advance='no') "'"
    write(6,'(I2.2)',advance='no') seconds
    write(6,'(a)',advance='no') '")'
end
