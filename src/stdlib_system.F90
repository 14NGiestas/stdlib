module stdlib_system
use, intrinsic :: iso_c_binding, only : c_int, c_long, c_bool
implicit none
private
public :: sleep, is_console

interface
#ifdef _WIN32
    subroutine win_Sleep(dwMilliseconds) bind (C, name='Sleep')
    !! version: experimental
    !! Interface to win32 API function
    !! [Sleep](https://docs.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-sleep)
    !!```c
    !! void Sleep(DWORD dwMilliseconds);
    !!```
    import c_long
    integer(c_long), value, intent(in) :: dwMilliseconds
    end subroutine

    integer(c_long) &
    function win_GetStdHandle(nStdHandle) &
             bind(C, name='GetStdHandle')
    !! version: experimental
    !! Interface to win32 API function
    !! [GetStdHandle](https://docs.microsoft.com/en-us/windows/console/getstdhandle)
    !!```c
    !!HANDLE WINAPI GetStdHandle(
    !!  _In_ DWORD nStdHandle
    !!);
    !!```
    import c_long
    integer(c_long), value, intent(in) :: nStdHandle
    end function

    logical(c_bool) &
    function win_GetConsoleMode(hConsoleHandle, lpMode) &
                           bind(C, name='GetConsoleMode')
    !! version: experimental
    !! Interface to win32 API function
    !! [GetConsoleMode](https://docs.microsoft.com/en-us/windows/console/getconsolemode)
    !!```c
    !! BOOL WINAPI GetConsoleMode(
    !! _In_  HANDLE  hConsoleHandle,
    !! _Out_ LPDWORD lpMode
    !! );
    !!```
    import c_long, c_bool
    integer(c_long), value, intent(in) :: hConsoleHandle
    integer(c_long), value, intent(in) :: lpMode
    end function

    logical(c_int) &
    function win_isatty(fd) &
        bind(C, name='_isatty')
    !! version: experimental
    !! Interface to C Runtime API function
    !! [isatty](https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/isatty)
    !!```c
    !! int isatty(int fd);
    !!```
    import c_int
    integer(c_int), value, intent(in) :: fd
    end function

#else
    integer(c_int) &
    function usleep(usec) bind(C)
    !! version: experimental
    !!
    !! int usleep(useconds_t usec);
    !! https://linux.die.net/man/3/usleep
    import c_int
    integer(c_int), value, intent(in) :: usec
    end function

    integer(c_int) &
    function isatty(fd) bind(C)
    !! version: experimental
    !!```c
    !! int isatty(int fd);
    !!```
    !! https://linux.die.net/man/3/isatty
    import c_int
    integer(c_int), value, intent(in) :: fd
    end function
#endif
end interface

contains

subroutine sleep(millisec, status)
    !! version: experimental
    !!
    integer, intent(in) :: millisec
    integer, optional :: status
    integer(c_int) :: ierr
#ifdef _WIN32
    ! PGI Windows, Ifort Windows, ....
    call win_Sleep(int(millisec, c_long))
#else
    ierr = 0_c_int
    ! Linux, Unix, MacOS, MSYS2, ...
    ierr = usleep(int(millisec * 1000, c_int))
    if (present(status)) then
        status = ierr
    else
        if (ierr/=0) error stop 'problem with usleep() system call'
    end if
#endif
end subroutine

logical function is_console(file_unit, status)
    !! version: experimental
    !!
    integer, intent(in) :: file_unit
    integer, optional :: status
    integer(c_int) :: ierr
    ierr = 0_c_int
#ifdef _WIN32
    ! PGI Windows, Ifort Windows, ....
    ierr = win_isatty(file_unit)
#else
    ! Linux, Unix, MacOS, MSYS2, ...
    ierr = isatty(file_unit)
#endif
    select case(ierr)
    case(0:1)
        is_console = ierr /= 0
        return
    case default
        if (present(status)) then
            status = ierr
        else
            error stop 'problem with isatty() system call'
        end if
    end select
end function

end module stdlib_system
