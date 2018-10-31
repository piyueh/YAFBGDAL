!
! ogr_api.f90
! Copyright (C) 2018 Pi-Yueh Chuang <pychuang@gwu.edu>
!
! Distributed under terms of the MIT license.
!


module ogr_api
use, intrinsic:: iso_c_binding, only: c_ptr, c_null_ptr, c_int
use:: c_conv_type, only: CString
use:: ogr_core, only: OGRErr
use:: ogr_srs_api, only: OGRSpatialReferenceH
implicit none

public
private:: c_ptr, c_null_ptr, c_int, CString, OGRErr, OGRSpatialReferenceH

    type, bind(C):: OGRGeometryH
        type(c_ptr):: ptr = c_null_ptr
    end type OGRGeometryH

    type, bind(C):: OGRFieldDefnH
        type(c_ptr):: ptr = c_null_ptr
    end type OGRFieldDefnH

    type, bind(C):: OGRFeatureDefnH
        type(c_ptr):: ptr = c_null_ptr
    end type OGRFeatureDefnH

    type, bind(C):: OGRLayerH
        type(c_ptr):: ptr = c_null_ptr
    end type OGRLayerH

    type, bind(C):: OGRFeatureH
        type(c_ptr):: ptr = c_null_ptr
    end type OGRFeatureH

    interface

        module function OGR_G_ExportToWkt(hGeom, ppszSrcText) bind(C, name="OGR_G_ExportToWkt")
            type(OGRGeometryH), intent(in), value:: hGeom
            type(CString), intent(inout):: ppszSrcText
            type(OGRErr):: OGR_G_ExportToWkt
        end function OGR_G_ExportToWkt

        module function OGR_G_GetGeometryName(hGeom) bind(C, name="OGR_G_GetGeometryName")
            type(OGRGeometryH), intent(in), value:: hGeom
            type(CString):: OGR_G_GetGeometryName
        end function OGR_G_GetGeometryName

        module function OGR_Fld_GetNameRef(hDefn) bind(C, name="OGR_Fld_GetNameRef")
            type(OGRFieldDefnH), intent(in), value:: hDefn
            type(CString):: OGR_Fld_GetNameRef
        end function OGR_Fld_GetNameRef

        module function OGR_FD_GetFieldCount(hDefn) bind(C, name="OGR_FD_GetFieldCount")
            type(OGRFeatureDefnH), intent(in), value:: hDefn
            integer(kind=c_int):: OGR_FD_GetFieldCount
        end function OGR_FD_GetFieldCount

        module function OGR_FD_GetFieldDefn(hDefn, iField) bind(C, name="OGR_FD_GetFieldDefn")
            type(OGRFeatureDefnH), intent(in), value:: hDefn
            integer(kind=c_int), intent(in), value:: iField
            type(OGRFieldDefnH):: OGR_FD_GetFieldDefn
        end function OGR_FD_GetFieldDefn

        module function OGR_F_GetGeometryRef(hFeat) bind(C, name="OGR_F_GetGeometryRef")
            type(OGRFeatureH), intent(in), value::hFeat
            type(OGRGeometryH):: OGR_F_GetGeometryRef
        end function OGR_F_GetGeometryRef

        module function OGR_F_GetFieldAsString(hFeat, iField) bind(C, name="OGR_F_GetFieldAsString")
            type(OGRFeatureH), intent(in), value::hFeat
            integer(kind=c_int), intent(in), value:: iField
            type(CString):: OGR_F_GetFieldAsString
        end function OGR_F_GetFieldAsString

        module subroutine OGR_F_Destroy(hFeat) bind(C, name="OGR_F_Destroy")
            type(OGRFeatureH), intent(in), value::hFeat
        end subroutine OGR_F_Destroy

        module function OGR_L_GetName(hLayer) bind(C, name="OGR_L_GetName")
            type(OGRLayerH), intent(in), value:: hLayer
            type(CString):: OGR_L_GetName
        end function OGR_L_GetName

        module subroutine OGR_L_ResetReading(hLayer) bind(C, name="OGR_L_ResetReading")
            type(OGRLayerH), intent(in), value::hLayer
        end subroutine OGR_L_ResetReading

        module function OGR_L_GetNextFeature(hLayer) bind(C, name="OGR_L_GetNextFeature")
            type(OGRLayerH), intent(in), value:: hLayer
            type(OGRFeatureH):: OGR_L_GetNextFeature
        end function OGR_L_GetNextFeature

        module function OGR_L_GetLayerDefn(hLayer) bind(C, name="OGR_L_GetLayerDefn")
            type(OGRLayerH), intent(in), value:: hLayer
            type(OGRFeatureDefnH):: OGR_L_GetLayerDefn
        end function OGR_L_GetLayerDefn

        module function OGR_L_GetSpatialRef(hLayer) bind(C, name="OGR_L_GetSpatialRef")
            type(OGRLayerH), intent(in), value:: hLayer
            type(OGRSpatialReferenceH):: OGR_L_GetSpatialRef
        end function OGR_L_GetSpatialRef
    end interface
end module ogr_api
! vi: ft=fortran
