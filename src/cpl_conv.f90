!
! cpl_conv.f90
! Copyright (C) 2018 Pi-Yueh Chuang <pychuang@gwu.edu>
!
! Distributed under terms of the MIT license.
!


module cpl_conv
use::c_type_ext, only: CString
implicit none

    interface
        module subroutine CPLFree(a) bind(C, name="VSIFree")
            type(CString), intent(in), value:: a
        end subroutine CPLFree
    end interface

end module cpl_conv
! vi: ft=fortran
