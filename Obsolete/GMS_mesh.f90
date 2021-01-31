

module mod_mesh

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_mesh'
 !          
 !          Purpose:
 !                     Data aggregator for modified *ivread module 
 !                     *ivread -- originally program by J. Burkardt
 !
 !          History:
 !                        
 !                        Date: 16-10-2018
 !                        Time: 15:31 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !                 
 !                     John Burkardt
 !         
 !          Modified by 
 !                      
 !                     Bernard Gingold
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !=================================================================================
     ! Tab:5 col - Type and etc.. definitions
     ! Tab:10,11 col - Type , function and subroutine code blocks.
     use mod_kinds,    only : int4,sp
     use mod_vectypes   only : XMM3r4_t, YMM8r4_t,   &
                             XMM2r4_t, YMM8i4_t,   &
                             XMM4i4_t, XMM4r4_t
     implicit none                                                                                                                    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_MESH_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_MESH_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_MESH_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_MESH_FULLVERSION = 1000_int4*MOD_MESH_MAJOR+100_int4*MOD_MESH_MINOR+ &
                                                                    10_int4*MOD_MESH_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_MESH_CREATE_DATE = "16-10-2018 15:31 +00200 (TUE 16 OCT 2018 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_MESH_BUILD_DATE = " "
    
    ! Module author info
    character(*),       parameter, public :: MOD_MESH_AUTHOR = "Programmer: Bernard Gingold, based on John Burkardt: -- ivread.f90 program"
    
    ! Module short synopsis
    character(*),       parameter, public :: MOD_MESH_SYNOPSIS = "Data aggregator for ivread module."
    
    ! Module constants
    
    integer(kind=int4), parameter, public :: cor3_max     = 100000
    integer(kind=int4), parameter, public :: edge_max     = 128
    integer(kind=int4), parameter, public :: face_max     = 100000
    integer(kind=int4), parameter, public :: line_max     = 100000
    integer(kind=int4), parameter, public :: material_max = 200
    integer(kind=int4), parameter, public :: order_max    = 6
    integer(kind=int4), parameter, public :: point_max    = 1000
    integer(kind=int4), parameter, public :: texture_max  = 10
    ! Vector quantities
    integer(kind=int4), parameter, public :: vcor3_max      = 128000
    integer(kind=int4), parameter, public :: vedge_max      = 100
    integer(kind=int4), parameter, public :: vface_max      = 128000
    integer(kind=int4), parameter, public :: vline_max      = 128000
    integer(kind=int4), parameter, public :: vmaterial_max  = 256
    integer(kind=int4), parameter, public :: vorder_max     = 8
    integer(kind=int4), parameter, public :: vpoint_max     = 1024
    integer(kind=int4), parameter, public :: vtexture_max   = 10
    
    ! Data aggregator SoA model
    type, public :: Mesh_t
         
          integer(kind=int4)                                  :: point_num
          integer(kind=int4)                                  :: ierror
          integer(kind=int4)                                  :: line_prune
          integer(kind=int4)                                  :: core3_num   ! for OBJ
          integer(kind=int4)                                  :: face_num    ! for OBJ
          logical(kind=int4)                                  :: debug
          character(len=255)                                  :: filein_name
          character(len=255)                                  :: fileout_name
          character(len=25)                                   :: object_name
          real(kind=sp),      dimension(3,cor3_max)           :: cor3         ! for OBJ 
          integer(kind=int4), dimension(cor3_max)             :: cor3_material
          real(kind=sp),      dimension(3,cor3_max)           :: cor3_new
          real(kind=sp),      dimension(3,cor3_max)           :: cor3_normal
          real(kind=sp),      dimension(2,cor3_max)           :: cor3_tex_uv
          integer(kind=int4), dimension(4,edge_max)           :: edge
          integer(kind=int4), dimension(order_max,face_max)   :: face        ! for OBJ
                                   
          real(kind=sp ),     dimension(face_max)             :: face_area    ! for OBJ
          integer(kind=int4), dimension(face_max)             :: face_material
          real(kind=sp),      dimension(3,face_max)           :: face_normal  ! for OBJ 
          integer(kind=int4), dimension(face_max)             :: face_object
          real(kind=sp),      dimension(3,face_max)           :: face_point  ! for OBJ
          integer(kind=int4), dimension(face_max)             :: face_order
          integer(kind=int4), dimension(face_max)             :: face_rank
          real(kind=sp),      dimension(2,face_max)           :: face_tex_uv
          integer(kind=int4), dimension(face_max)             :: face_tier
          integer(kind=int4), dimension(line_max)             :: line_dex
          integer(kind=int4), dimension(line_max)             :: line_material
          integer(kind=int4), dimension(cor3_max)             :: list
          character(len=255), dimension(material_max)         :: material_name
          real(kind=sp),      dimension(4,material_max)       :: material_rgba
          integer(kind=int4), dimension(point_max)            :: point
          character(len=255 ),dimension(texture_max)          :: texture_name
          real(kind=sp),      dimension(2,order_max*face_max) :: texture_temp
          real(kind=sp),      dimension(4,4)                  :: transform_matrix
          integer(kind=int4), dimension(order_max,face_max)   :: vertex_material
          real(kind=sp),      dimension(3,order_max,face_max) :: vertex_normal
          real(kind=sp),      dimension(2,order_max,face_max) :: vertex_tex_uv
         
    end type Mesh_t
 
      ! Data aggregator PAoS model (experimental stage and not-tested)
  
    type, public :: PAoSMesh_t
         
          integer(kind=int4)                                  :: point_num
          integer(kind=int4)                                  :: ierror
          integer(kind=int4)                                  :: line_prune
          integer(kind=int4)                                  :: core3_num   ! for OBJ
          integer(kind=int4)                                  :: face_num    ! for OBJ
          logical(kind=int4)                                  :: debug
          character(len=255)                                  :: filein_name
          character(len=255)                                  :: fileout_name
          character(len=25)                                   :: object_name
          type(XMM3r4_t),       dimension(vcor3_max)             :: vcor3
          type(YMM8i4_t),       dimension(vcor3_max/8)           :: vcor3_material
          type(XMM3r4_t),       dimension(vcor3_max)             :: vcor3_new
          type(XMM3r4_t),       dimension(vcor3_max)             :: vcor3_normal
          type(XMM2r4_t),       dimension(vcor3_max)             :: vcor3_tex_uv
          type(XMM4i4_t),       dimension(vedge_max)             :: vedge
          type(YMM8i4_t),       dimension(vface_max)             :: vface
          type(YMM8r4_t),       dimension(vface_max/8)           :: vface_area
          type(YMM8i4_t),       dimension(vface_max/8)           :: vface_material
          type(XMM3r4_t),       dimension(vface_max)             :: vface_normal  ! for OBJ 
          type(YMM8i4_t),       dimension(vface_max/8)           :: face_object
          type(XMM3r4_t),       dimension(vface_max)             :: vface_point  ! for OBJ
          type(YMM8i4_t),       dimension(vface_max/8)           :: vface_order
          type(YMM8i4_t),       dimension(vface_max/8)           :: vface_rank
          type(XMM2r4_t),       dimension(vface_max)             :: vface_tex_uv
          type(YMM8i4_t),       dimension(vface_max/8)           :: vface_tier
          type(YMM8i4_t),       dimension(vline_max/8)           :: vline_dex
          type(YMM8i4_t),       dimension(vline_max/8)           :: vline_material
          type(YMM8i4_t),       dimension(cor3_max/8)            :: vlist
          character(len=255),   dimension(material_max)          :: material_name
          type(XMM4r4_t),       dimension(vmaterial_max)         :: vmaterial_rgba
          type(YMM8i4_t),       dimension(vpoint_max/8)          :: vpoint
          character(len=255),   dimension(texture_max)           :: texture_name
          type(XMM2r4_t),       dimension(vorder_max*vface_max)  :: vtexture_temp
          type(XMM4r4_t),       dimension(4)                     :: vtransform_matrix
          type(YMM8i4_t),       dimension(vface_max)             :: vvertex_material
          type(XMM3r4_t),       dimension(vorder_max,vface_max)  :: vvertex_normal
          type(XMM2r4_t),       dimension(vorder_max,vface_max)  :: vvertex_tex_uv
    end type PAosMesh_t
   
    
end module mod_mesh
