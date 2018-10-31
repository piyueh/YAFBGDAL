!
! c_conv_type.f90
! Copyright (C) 2018 Pi-Yueh Chuang <pychuang@gwu.edu>
!
! Distributed under terms of the MIT license.
!


module c_conv_type
use, intrinsic:: iso_c_binding, only: c_ptr, c_null_ptr, c_size_t, c_char, c_f_pointer
implicit none

public
private:: c_ptr, c_null_ptr, c_size_t, c_char

    type, bind(C):: CString
        type(c_ptr):: ptr = c_null_ptr
    end type CString

    interface
        module function cstrlen(s) bind(C, name="strlen")
            type(CString), intent(in), value:: s
            integer(kind=c_size_t):: cstrlen
        end function cstrlen
    end interface

contains

    function cstr2fstr(cstr)
        type(CString), intent(in):: cstr
        character(len=1, kind=c_char), dimension(:), pointer:: f_ptr
        character(len=:), allocatable:: cstr2fstr

        integer(kind=c_size_t):: len
        integer(kind=4):: i

        len = cstrlen(cstr)

        call c_f_pointer(cstr%ptr, f_ptr, [len])

        allocate(character(len=len)::cstr2fstr)

        do i = 1, len
            cstr2fstr(i:i) = f_ptr(i)
        enddo 
    end function cstr2fstr

end module c_conv_type
! vi: ft=fortran
