!
! read_files_from_CMD.f90
! Copyright (C) 2018 Pi-Yueh Chuang <pychuang@gwu.edu>
!
! Distributed under terms of the MIT license.
!

program main
use, intrinsic:: iso_c_binding, only: c_associated, c_null_ptr
use gdal
implicit none

type(GDALDatasetH):: hDS
type(CSLConstList):: list

type(OGRLayerH):: layer
type(CString):: layer_name_c

type(OGRSpatialReferenceH):: scr
type(CString):: pretty_wkt
type(OGRErr):: err

type(OGRFeatureDefnH):: layer_defn

type(OGRFieldDefnH):: field_defn
type(CString):: field_name

type(OGRFeatureH):: feature

type(OGRGeometryH):: geometry
type(CString):: geometry_name
type(CString):: geometry_wkt

integer(kind=c_int):: n_layers
integer(kind=c_int):: n_files
integer(kind=c_int):: n_fields

integer(kind=c_int):: i, j

character(len=255):: arg
integer(kind=4):: arg_len, arg_stat


! initialize GDAL
call GDALAllRegister()

! get file name/path from CMD arg
call get_command_argument(1, arg, arg_len, arg_stat)

if (arg_stat .gt. 0) stop "Error: failed to get the CMD argument."
if (arg_stat .eq. -1) stop "Error: CMD argument is too long."

! open files
hDS = GDALOpenEx(trim(arg)//char(0), 4, c_null_ptr, c_null_ptr, c_null_ptr)
if (.not. c_associated(hDS%ptr)) then
    print *, "Failed to open dataset path. Program stopped."
    stop
endif

! get number of layers
n_layers = GDALDatasetGetLayerCount(hDS)
print *, "# of Layers: ", n_layers

! get file list
list = GDALGetFileList(hDS)
n_files = CSLCount(list)
print *, "# of Files: ", n_files
print *, "List of Files:"
n_files = CSLPrint(list, c_null_ptr)

call CSLDestroy(list)

do i = 0, n_layers-1

    print *, "i = ", i
    layer = GDALDatasetGetLayer(hDS, i)

    if (.not. c_associated(layer%ptr)) then
        print *, "layer%ptr not associated."
    endif

    layer_name_c = OGR_L_GetName(layer)
    print *, "Name of the layer:", cstr2fstr(layer_name_c)

    scr = OGR_L_GetSpatialRef(layer)
    err = OSRExportToPrettyWkt(scr, pretty_wkt, 1)
    print *, cstr2fstr(pretty_wkt)
    call CPLFree(pretty_wkt)

    layer_defn = OGR_L_GetLayerDefn(layer)
    n_fields = OGR_FD_GetFieldCount(layer_defn)
    print *, "Number of fields: ", n_fields

    print *, "List of fields: "

    do j = 0, n_fields-1
        field_defn = OGR_FD_GetFieldDefn(layer_defn, j)
        field_name = OGR_Fld_GetNameRef(field_defn)
        print *, j, ": ", cstr2fstr(field_name)
    enddo

    feature%ptr = c_null_ptr
    call OGR_L_ResetReading(layer)
    do while(.true.)
        if(c_associated(feature%ptr)) call OGR_F_Destroy(feature)

        feature = OGR_L_GetNextFeature(layer)
        if(.not. c_associated(feature%ptr)) exit

        do j = 0, n_fields-1
            field_name = OGR_F_GetFieldAsString(feature, j)
            write(*, "(A, A)", advance="no") cstr2fstr(field_name), ","
        enddo
        print *,
    enddo
    call OGR_F_Destroy(feature)

    feature%ptr = c_null_ptr
    call OGR_L_ResetReading(layer)
    do while(.true.)
        if(c_associated(feature%ptr)) call OGR_F_Destroy(feature)

        feature = OGR_L_GetNextFeature(layer)
        if(.not. c_associated(feature%ptr)) exit

        geometry = OGR_F_GetGeometryRef(feature)
        write(*, "(TL2, A)", advance="no") "Geometry type: "

        if (c_associated(geometry%ptr)) then
            geometry_name = OGR_G_GetGeometryName(geometry)
            print *, cstr2fstr(geometry_name)

            err = OGR_G_ExportToWkt(geometry, geometry_wkt)
            write(*, "(TL2, A)") cstr2fstr(geometry_wkt)
            call CPLFree(geometry_wkt)
        else
            print *,
        endif
    enddo
    call OGR_F_Destroy(feature)
enddo

call GDALClose(hDS)

end program main
