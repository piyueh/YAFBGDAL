!
! ogr_srs_api.f90
! Copyright (C) 2018 Pi-Yueh Chuang <pychuang@gwu.edu>
!
! Distributed under terms of the MIT license.
!


module ogr_srs_api
use, intrinsic:: iso_c_binding, only: c_ptr, c_null_ptr, c_int
use:: c_type_ext, only: CString
use:: ogr_core, only: OGRErr
implicit none

public
private:: c_ptr, c_null_ptr, c_int, CString, OGRErr

    type, bind(C):: OGRSpatialReferenceH
        type(c_ptr):: ptr = c_null_ptr
    end type OGRSpatialReferenceH

    interface
        module function OSRExportToPrettyWkt( &
            hSRS, ppszReturn, bSimplify) bind(C, name="OSRExportToPrettyWkt")
            type(OGRSpatialReferenceH), intent(in), value:: hSRS
            type(CString), intent(inout):: ppszReturn
            integer(kind=c_int), intent(in), value:: bSimplify
            type(OGRErr):: OSRExportToPrettyWkt
        end function OSRExportToPrettyWkt
    end interface

end module ogr_srs_api
! vi: ft=fortran
