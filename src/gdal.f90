!
! gdal.f90
! Copyright (C) 2018 Pi-Yueh Chuang <pychuang@gwu.edu>
!
! Distributed under terms of the MIT license.
!


module gdal
use, intrinsic:: iso_c_binding, only: c_ptr, c_null_ptr, c_char, c_int
use:: c_type_ext
use:: cpl_conv
use:: cpl_port
use:: cpl_string
use:: ogr_core
use:: ogr_srs_api
use:: ogr_api
implicit none

public

    integer(kind=c_int), parameter:: GDAL_OF_ALL = z"00"
    integer(kind=c_int), parameter:: GDAL_OF_ARRAY_BLOCK_ACCESS = z"100" 
    integer(kind=c_int), parameter:: GDAL_OF_DEFAULT_BLOCK_ACCESS = 0
    integer(kind=c_int), parameter:: GDAL_OF_GNM = z"08"
    integer(kind=c_int), parameter:: GDAL_OF_HASHSET_BLOCK_ACCESS = z"200"
    integer(kind=c_int), parameter:: GDAL_OF_INTERNAL = z"80"
    integer(kind=c_int), parameter:: GDAL_OF_RASTER = z"02"
    integer(kind=c_int), parameter:: GDAL_OF_READONLY = z"00"
    integer(kind=c_int), parameter:: GDAL_OF_SHARED = z"20"
    integer(kind=c_int), parameter:: GDAL_OF_UPDATE = z"01"
    integer(kind=c_int), parameter:: GDAL_OF_VECTOR = z"04"
    integer(kind=c_int), parameter:: GDAL_OF_VERBOSE_ERROR = z"40"

    type, bind(C):: GDALDatasetH
        type(c_ptr):: ptr = c_null_ptr
    end type GDALDatasetH

    interface
        module subroutine GDALAllRegister() bind(C, name='GDALAllRegister')
        end subroutine GDALAllRegister

        module function GDALOpenEx(pszFilename, nOpenFlags, &
            papszAllowedDrivers, papszOpenOptions, papszSiblingFiles) bind(&
            C, name="GDALOpenEx")
            character(kind=c_char), dimension(*), intent(in):: pszFilename
            integer(kind=c_int), intent(in), value:: nOpenFlags
            type(c_ptr), intent(in), value:: papszAllowedDrivers
            type(c_ptr), intent(in), value:: papszOpenOptions
            type(c_ptr), intent(in), value:: papszSiblingFiles
            type(GDALDatasetH):: GDALOpenEx
        end function GDALOpenEx

        module function GDALDatasetGetLayerCount(hDS) bind(&
            C, name="GDALDatasetGetLayerCount")
            type(GDALDatasetH), intent(in), value:: hDS
            integer(kind=c_int):: GDALDatasetGetLayerCount
        end function GDALDatasetGetLayerCount

        module function GDALDatasetGetLayer(hDS, iLayer) bind(&
            C, name="GDALDatasetGetLayer")
            type(GDALDatasetH), intent(in), value:: hDS
            integer(kind=c_int), intent(in), value:: iLayer
            type(OGRLayerH):: GDALDatasetGetLayer
        end function GDALDatasetGetLayer

        module function GDALGetFileList(hDS) bind(C, name="GDALGetFileList")
            type(GDALDatasetH), intent(in), value:: hDS
            type(CSLConstList):: GDALGetFileList
        end function GDALGetFileList

        module subroutine GDALClose(hDS) bind(C, name="GDALClose")
            type(GDALDatasetH), intent(in), value:: hDS
        end subroutine GDALClose
    end interface

end module gdal
! vi: ft=fortran
