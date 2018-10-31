!
! ogr_core.f90
! Copyright (C) 2018 Pi-Yueh Chuang <pychuang@gwu.edu>
!
! Distributed under terms of the MIT license.
!


module ogr_core
use, intrinsic:: iso_c_binding, only: c_int
implicit none

    type, bind(C):: OGRErr
        integer(kind=c_int):: err = 0
    end type OGRErr

end module ogr_core
! vi: ft=fortran
