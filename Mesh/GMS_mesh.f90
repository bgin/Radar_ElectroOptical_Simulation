

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
     use mod_kinds,    only : i4,sp
     
     implicit none                                                                                                                    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=i4), parameter, public :: MOD_MESH_MAJOR = 1_i4
    
    ! Minor version
    integer(kind=i4), parameter, public :: MOD_MESH_MINOR = 0_i4
    
    ! Micro version
    integer(kind=i4), parameter, public :: MOD_MESH_MICRO = 0_i4
    
    ! Module full version
    integer(kind=i4), parameter, public :: MOD_MESH_FULLVERSION = 1000_i4*MOD_MESH_MAJOR+100_i4*MOD_MESH_MINOR+ &
                                                                    10_i4*MOD_MESH_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_MESH_CREATE_DATE = "16-10-2018 15:31 +00200 (TUE 16 OCT 2018 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_MESH_BUILD_DATE = " "
    
    ! Module author info
    character(*),       parameter, public :: MOD_MESH_AUTHOR = "Programmer: Bernard Gingold, based on John Burkardt: -- ivread.f90 program"
    
    ! Module short synopsis
    character(*),       parameter, public :: MOD_MESH_SYNOPSIS = "Data aggregator for ivread module."
    
    ! Module constants
    
    integer(kind=i4), parameter, public :: cor3_max     = 100000
    integer(kind=i4), parameter, public :: edge_max     = 128
    integer(kind=i4), parameter, public :: face_max     = 100000
    integer(kind=i4), parameter, public :: line_max     = 100000
    integer(kind=i4), parameter, public :: material_max = 200
    integer(kind=i4), parameter, public :: order_max    = 6
    integer(kind=i4), parameter, public :: point_max    = 1000
    integer(kind=i4), parameter, public :: texture_max  = 10
    ! Vector quantities
    integer(kind=i4), parameter, public :: vcor3_max      = 128000
    integer(kind=i4), parameter, public :: vedge_max      = 100
    integer(kind=i4), parameter, public :: vface_max      = 128000
    integer(kind=i4), parameter, public :: vline_max      = 128000
    integer(kind=i4), parameter, public :: vmaterial_max  = 256
    integer(kind=i4), parameter, public :: vorder_max     = 8
    integer(kind=i4), parameter, public :: vpoint_max     = 1024
    integer(kind=i4), parameter, public :: vtexture_max   = 10
    
    ! Data aggregator SoA model
    type, public :: Mesh_static_t
         
          integer(kind=i4)                                  :: point_num
          integer(kind=i4)                                  :: ierror
          integer(kind=i4)                                  :: line_prune
          integer(kind=i4)                                  :: core3_num   ! for OBJ
          integer(kind=i4)                                  :: face_num    ! for OBJ
          logical(kind=i4)                                  :: debug
          character(len=255)                                  :: filein_name
          character(len=255)                                  :: fileout_name
          character(len=25)                                   :: object_name
          real(kind=sp),      dimension(3,cor3_max)           :: cor3         ! for OBJ 
          integer(kind=i4), dimension(cor3_max)             :: cor3_material
          real(kind=sp),      dimension(3,cor3_max)           :: cor3_new
          real(kind=sp),      dimension(3,cor3_max)           :: cor3_normal
          real(kind=sp),      dimension(2,cor3_max)           :: cor3_tex_uv
          integer(kind=i4), dimension(4,edge_max)           :: edge
          integer(kind=i4), dimension(order_max,face_max)   :: face        ! for OBJ
                                   
          real(kind=sp ),     dimension(face_max)             :: face_area    ! for OBJ
          integer(kind=i4), dimension(face_max)             :: face_material
          real(kind=sp),      dimension(3,face_max)           :: face_normal  ! for OBJ 
          integer(kind=i4), dimension(face_max)             :: face_object
          real(kind=sp),      dimension(3,face_max)           :: face_point  ! for OBJ
          integer(kind=i4), dimension(face_max)             :: face_order
          integer(kind=i4), dimension(face_max)             :: face_rank
          real(kind=sp),      dimension(2,face_max)           :: face_tex_uv
          integer(kind=i4), dimension(face_max)             :: face_tier
          integer(kind=i4), dimension(line_max)             :: line_dex
          integer(kind=i4), dimension(line_max)             :: line_material
          integer(kind=i4), dimension(cor3_max)             :: list
          character(len=255), dimension(material_max)         :: material_name
          real(kind=sp),      dimension(4,material_max)       :: material_rgba
          integer(kind=i4), dimension(point_max)            :: point
          character(len=255 ),dimension(texture_max)          :: texture_name
          real(kind=sp),      dimension(2,order_max*face_max) :: texture_temp
          real(kind=sp),      dimension(4,4)                  :: transform_matrix
          integer(kind=i4), dimension(order_max,face_max)   :: vertex_material
          real(kind=sp),      dimension(3,order_max,face_max) :: vertex_normal
          real(kind=sp),      dimension(2,order_max,face_max) :: vertex_tex_uv
         
    end type Mesh_static_t
    
    type, public :: Mesh_dynamic_t
          
         
          integer(kind=i4)                                  :: ierror
          integer(kind=i4)                                  :: line_prune
          integer(kind=i4)                                  :: n_cor3   ! for OBJ
          integer(kind=i4)                                  :: n_face    ! for OBJ
          integer(kind=i4)                                    :: n_edge
          integer(kind=i4)                                    :: n_order
          integer(kind=i4)                                    :: n_face
          integer(kind=i4)                                    :: n_line
          integer(kind=i4)                                    :: n_material
          integer(kind=i4)                                    :: n_point
          integer(kind=i4)                                    :: n_texture
          logical(kind=i4)                                  :: debug
          character(len=255)                                  :: filein_name
          character(len=255)                                  :: fileout_name
          character(len=25)                                   :: object_name
          character(len=255), dimension(material_max)         :: material_name !         dim: n_material
          character(len=255 ),dimension(texture_max)                 :: texture_name  !         dim: n_texture
          real(kind=sp),      dimension(4,4)                         :: transform_matrix !      dim: 4,4
          real(kind=sp),      dimension(:,:),  allocatable           :: cor3          ! for OBJ dim: 3,n_cor3
          integer(kind=i4), dimension(:),    allocatable           :: cor3_material ! dim: n_cor3
          real(kind=sp),      dimension(:,:),  allocatable           :: cor3_new      ! dim: 3,n_cor3
          real(kind=sp),      dimension(:,:),  allocatable           :: cor3_normal   ! dim: 3,n_cor3
          real(kind=sp),      dimension(:,:),  allocatable           :: cor3_tex_uv   ! dim: 2,n_cor3
          integer(kind=i4), dimension(:,:),  allocatable           :: edge          ! dim: 4,n_edge
          integer(kind=i4), dimension(:,:),  allocatable           :: face          ! for OBJ dim: n_order,n_face
                       
          real(kind=sp ),     dimension(:),    allocatable           :: face_area     ! for OBJ dim: n_face
          integer(kind=i4), dimension(:),    allocatable           :: face_material !         dim: n_face
          real(kind=sp),      dimension(:,:),  allocatable           :: face_normal  ! for OBJ  dim: 3,n_face
          integer(kind=i4), dimension(:),    allocatable           :: face_object  !          dim: n_face
          real(kind=sp),      dimension(:,:),  allocatable           :: face_point  ! for OBJ   dim: 3,n_face
          integer(kind=i4), dimension(:),    allocatable           :: face_order  !           dim: n_face
          integer(kind=i4), dimension(:),    allocatable           :: face_rank   !           dim: n_face
          real(kind=sp),      dimension(:,:),  allocatable           :: face_tex_uv !           dim: 2,n_face
          integer(kind=i4), dimension(:),    allocatable           :: face_tier   !           dim: n_face
          integer(kind=i4), dimension(:),    allocatable           :: line_dex    !           dim: n_line
          integer(kind=i4), dimension(:),    allocatable           :: line_material !         dim: n_line
          integer(kind=i4), dimension(:),    allocatable           :: list          !         dim: n_cor3
         
          real(kind=sp),      dimension(:,:),   allocatable          :: material_rgba !         dim: 4, n_material
          integer(kind=i4), dimension(:),     allocatable          :: point         !         dim: n_point
         
          real(kind=sp),      dimension(:,:),   allocatable          :: texture_temp  !         dim: 2,n_order*n_face
         
          integer(kind=i4), dimension(:,:),   allocatable          :: vertex_material  !      dim: n_order,n_face
          real(kind=sp),      dimension(:,:,:), allocatable          :: vertex_normal    !      dim: 3,n_order,n_face
          real(kind=sp),      dimension(:,:,:), allocatable          :: vertex_tex_uv    !      dim: 2,n_order,n_face
#if defined(__ICC) || defined(__INTEL_COMPILER)
          !dir$ attributes align : 64 :: cor3
          !dir$ attributes align : 64 :: cor3_material
          !dir$ attributes align : 64 :: cor3_new
          !dir$ attributes align : 64 :: cor3_normal
          !dir$ attributes align : 64 :: cor3_tex_uv
          !dir$ attributes align : 64 :: edge
          !dir$ attributes align : 64 :: face
          !dir$ attributes align : 64 :: face_area
          !dir$ attributes align : 64 :: face_material
          !dir$ attributes align : 64 :: face_normal
          !dir$ attributes align : 64 :: face_object
          !dir$ attributes align : 64 :: face_point
          !dir$ attributes align : 64 :: face_order
          !dir$ attributes align : 64 :: face_rank
          !dir$ attributes align : 64 :: face_tex_uv
          !dir$ attributes align : 64 :: face_tier
          !dir$ attributes align : 64 :: line_dex
          !dir$ attributes align : 64 :: line_material
          !dir$ attributes align : 64 :: list
          !dir$ attributes align : 64 :: material_rgba
          !dir$ attributes align : 64 :: point
          !dir$ attributes align : 64 :: texture_temp
          !dir$ attributes align : 64 :: vertex_material
          !dir$ attributes align : 64 :: vertex_normal
          !dir$ attributes align : 64 :: vertex_tex_uv
#endif
    end type Mesh_dynamic_t
 
    
   
    
end module mod_mesh
