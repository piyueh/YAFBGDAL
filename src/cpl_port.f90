!
! cpl_port.f90
! Copyright (C) 2018 Pi-Yueh Chuang <pychuang@gwu.edu>
!
! Distributed under terms of the MIT license.
!


module cpl_port
use, intrinsic:: iso_c_binding, only: c_ptr, c_null_ptr
implicit none

public
private:: c_ptr, c_null_ptr

    type, bind(C):: CSLConstList
        type(c_ptr):: ptr = c_null_ptr
    end type CSLConstList

end module cpl_port
! vi: ft=fortran
