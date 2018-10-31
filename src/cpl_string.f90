!
! cpl_string.f90
! Copyright (C) 2018 Pi-Yueh Chuang <pychuang@gwu.edu>
!
! Distributed under terms of the MIT license.
!


module cpl_string
use, intrinsic:: iso_c_binding, only: c_int, c_ptr
use:: cpl_port, only: CSLConstList
implicit none

    interface
        module function CSLCount(papszStrList) bind(C, name="CSLCount")
            type(CSLConstList), intent(in), value:: papszStrList
            integer(kind=c_int):: CSLCount
        end function CSLCount

        module subroutine CSLDestroy(papszStrList) bind(C, name="CSLDestroy")
            type(CSLConstList), intent(in), value:: papszStrList
        end subroutine CSLDestroy

        module function CSLPrint(papszStrList, fpOut) bind(C, name="CSLPrint")
            type(CSLConstList), intent(in), value:: papszStrList
            type(c_ptr), intent(in), value:: fpOut ! FILE*
            integer(kind=c_int):: CSLPrint
        end function CSLPrint
    end interface

end module cpl_string
! vi: ft=fortran
