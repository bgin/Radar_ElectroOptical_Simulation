! MODULE: mesh
! AUTHOR: Jouni Makitalo
! Modified and adapted to better suite the 'GMS' project by Bernard Gingold, beniekg@gmail.com
! DESCRIPTION:
! Loading and manipulating meshes consisting of triangles and tetrahedra.
! Supports the msh-format exported by Gmsh and the neutral mesh format of Netgen.
! Contains also functions for splitting a mesh into submeshes and various mesh manipulation routines.
MODULE gmsh_mesh
  !USE aux
  !USE linalg
  use mod_kinds, only : i4, dp
  public
  private :: dotc,dotr,dotrc,normr,crossr
  IMPLICIT NONE

  !INTRINSIC NINT

  INTEGER(kind=i4), PARAMETER :: meshver = 4
  INTEGER(kind=i4), PARAMETER :: mesh_bnd_none = 1,&
       mesh_bnd_xplane = 2,&
       mesh_bnd_yplane = 3,&
       mesh_bnd_zplane = 4,&
       mesh_bnd_prdx1 = 5,&
       mesh_bnd_prdx2 = 6,&
       mesh_bnd_prdy1 = 7,&
       mesh_bnd_prdy2 = 8,&
       mesh_bnd_rz1 = 9,&
       mesh_bnd_rz2 = 10

   REAL (KIND=dp), PARAMETER :: &
       pi = 3.141592653589, &
       eps0 = 8.8541878176D-12, &
       mu0 = 4*pi*1D-7, &
       c0 = 2.997924580003D8, &
       eta0 = 3.767303134622D2
  REAL (KIND=dp), PARAMETER :: radtodeg = 180.0_dp/pi,&
       degtorad = pi/180.0_dp
  INTEGER(kind=i4), PARAMETER :: prd_none = 1,&
       prd_2d = 2

  TYPE node
     REAL (KIND=dp), DIMENSION(4) :: p
     !INTEGER(kind=i4), DIMENSION(:), POINTER :: face_indices, node_indices
     INTEGER(kind=i4) :: parent_index
     !INTEGER(kind=i4) :: bnd, nbnd
  END TYPE node

  TYPE face
     REAL (KIND=dp), DIMENSION(4) :: n, cp
     REAL (KIND=dp), DIMENSION(4,3) :: s, m
     INTEGER(kind=i4), DIMENSION(4) :: node_indices
     INTEGER(kind=i4), DIMENSION(4) :: edge_indices
     REAL (KIND=dp) :: area, pd
     INTEGER(kind=i4) :: id
     INTEGER(kind=i4) :: parent_index
  END TYPE face

  TYPE line
     INTEGER(kind=i4), DIMENSION(2) :: node_indices
     INTEGER(kind=i4) :: id
  END TYPE line

  TYPE edge
     INTEGER(kind=i4), DIMENSION(2) :: node_indices, bnode_indices, face_indices
     REAL (KIND=dp) :: length
!     REAL (KIND=dp), DIMENSION(2) :: rwgDiv
     INTEGER(kind=i4) :: bnd
     INTEGER(kind=i4) :: parent_index
     INTEGER(kind=i4) :: couple_index
     INTEGER(kind=i4), DIMENSION(:,:), ALLOCATABLE :: child_indices ! (:,1)=submesh, (:,2)=local edge
     !dir$ attributes align : 64 :: child_indices
  END TYPE edge

  ! Interior face.
  TYPE solid_face
     INTEGER(kind=i4), DIMENSION(4) :: node_indices
     INTEGER(kind=i4), DIMENSION(2) :: solid_indices, bnode_indices
     INTEGER(kind=i4) :: face_index ! -1 if not a boundary
     REAL (KIND=dp) :: area
  END TYPE solid_face

  TYPE solid
     INTEGER(kind=i4), DIMENSION(4) :: node_indices
     INTEGER(kind=i4), DIMENSION(4) :: solid_face_indices
     REAL (KIND=dp) :: volume
     INTEGER(kind=i4) :: id
  END TYPE solid

  TYPE mesh_container
     TYPE(node), DIMENSION(:), ALLOCATABLE :: nodes
     !dir$ attributes align : 64 :: nodes
     TYPE(face), DIMENSION(:), ALLOCATABLE :: faces
      !dir$ attributes align : 64 :: faces
     TYPE(line), DIMENSION(:), ALLOCATABLE :: lines
       !dir$ attributes align : 64 :: lines
     TYPE(edge), DIMENSION(:), ALLOCATABLE :: edges
        !dir$ attributes align : 64 :: edges
     TYPE(solid), DIMENSION(:), ALLOCATABLE :: solids
         !dir$ attributes align : 64 :: solids
     TYPE(solid_face), DIMENSION(:), ALLOCATABLE :: solid_faces
          !dir$ attributes align : 64 :: solid_faces
     INTEGER(kind=i4) :: nnodes, nfaces, nlines, nedges, nsolids, nsolid_faces
     REAL (KIND=dp) :: avelen
  END TYPE mesh_container

CONTAINS
  FUNCTION has_mesh_bnd(mesh, bnd) RESULT(res)
     !dir$ optimize:3
     !dir$ attributes forceinline :: has_mesh_bnd
    TYPE(mesh_container), INTENT(IN) :: mesh
    INTEGER(kind=i4), INTENT(IN) :: bnd
    LOGICAL :: res
    INTEGER(kind=i4) :: n

    res = .FALSE.

    DO n=1,mesh.nedges
       IF(mesh.edges(n).bnd==bnd) THEN
          res = .TRUE.
          EXIT
       END IF
    END DO
  END FUNCTION has_mesh_bnd

  FUNCTION adjacent_face(mesh, faceind, edgeind) RESULT(res)
     !dir$ optimize:3
     !dir$ attributes code_align : 32 :: adjacent_face
     !dir$ attributes forceinline :: adjecent_face
    TYPE(mesh_container), INTENT(IN) :: mesh
    INTEGER(kind=i4), INTENT(IN) :: faceind, edgeind

    INTEGER(kind=i4) :: res, gedgeind

    gedgeind = mesh.faces(faceind).edge_indices(edgeind)

    IF(mesh.edges(gedgeind).face_indices(1)==faceind) THEN
       res = mesh.edges(gedgeind).face_indices(2)
    ELSE IF(mesh.edges(gedgeind).face_indices(2)==faceind) THEN
       res = mesh.edges(gedgeind).face_indices(1)
    ELSE
       WRITE(*,*) 'Could not determine adjacent face!'
       STOP
    END IF
  END FUNCTION adjacent_face

  FUNCTION local_edge_index(mesh, faceind, gedgeind) RESULT(ledgeind)
     !dir$ optimize:3
     !dir$ attributes code_align : 32 :: local_edge_index
     !dir$ attributes forceinline :: local_edge_index
    TYPE(mesh_container), INTENT(IN) :: mesh
    INTEGER(kind=i4), INTENT(IN) :: faceind, gedgeind

    INTEGER(kind=i4) :: ledgeind

    IF(mesh.faces(faceind).edge_indices(1)==gedgeind) THEN
       ledgeind = 1
    ELSE IF(mesh.faces(faceind).edge_indices(2)==gedgeind) THEN
       ledgeind = 2
    ELSE IF(mesh.faces(faceind).edge_indices(3)==gedgeind) THEN
       ledgeind = 3
    ELSE
       WRITE(*,*) 'Invalid edge index request (nlsurf.f90)!'
       STOP
    END IF
  END FUNCTION local_edge_index

  FUNCTION average_edge_length(mesh) RESULT(res)
     !dir$ optimize:3
     !dir$ attributes code_align : 32 :: average_edge_length
     !dir$ attributes forceinline :: average_edge_length
    TYPE(mesh_container), INTENT(IN) :: mesh
    INTEGER(kind=i4) :: n
    REAL (KIND=dp) :: l
    REAL (KIND=dp) :: res

    res = 0.0_dp

    DO n=1,mesh.nedges
       l = normr(mesh.nodes(mesh.edges(n).node_indices(1)).p -&
            mesh.nodes(mesh.edges(n).node_indices(2)).p)
       res = res + l
    END DO

    res = res/mesh.nedges
  END FUNCTION average_edge_length

  SUBROUTINE classify_edges(mesh, id, bnd)
     !dir$ optimize:3
     !dir$ attributes code_align : 32 :: classify_edges
     !dir$ attributes forceinline :: classify_edges
    TYPE(mesh_container), INTENT(INOUT) :: mesh
    INTEGER(kind=i4), INTENT(IN) :: id, bnd
    INTEGER(kind=i4) :: n, m, nbnd

    nbnd = 0

    DO n=1,mesh.nedges
       DO m=1,mesh.nlines
          IF(mesh.lines(m).id==id .AND.&
               cmp_pairs(mesh.edges(n).node_indices, mesh.lines(m).node_indices)) THEN
             mesh.edges(n).bnd = bnd

             nbnd = nbnd + 1
          END IF
       END DO
    END DO

    WRITE(*,'(A,I0,A)') ' Classified ', nbnd, ' boundary edges'

  END SUBROUTINE classify_edges

  FUNCTION cmp_real_pairs(a, b, eps) RESULT(res)
     !dir$ optimize:3
     !dir$ attributes code_align : 32 :: cmp_real_pairs
     !dir$ attributes forceinline :: cmp_real_pairs
    REAL (KIND=dp), DIMENSION(2), INTENT(IN) :: a, b
    REAL (KIND=dp), INTENT(IN) :: eps
    LOGICAL :: res

    res = .FALSE.

    IF((ABS(a(1)-b(1))<eps .AND. ABS(a(2)-b(2))<eps) .OR.&
         (ABS(a(1)-b(2))<eps .AND. ABS(a(2)-b(1))<eps)) THEN
       res = .TRUE.
    END IF
  END FUNCTION cmp_real_pairs

  SUBROUTINE determine_edge_couples(mesh, eps)
     !dir$ optimize:3
     !dir$ attributes code_align : 32 :: determine_edge_couples
    
    TYPE(mesh_container), INTENT(INOUT) :: mesh
    REAL (KIND=dp), INTENT(IN) :: eps
    INTEGER(kind=i4) :: n, m, numx, numy
    REAL (KIND=dp), DIMENSION(4) :: a1, a2, b1, b2

    !WRITE(*,*) 'Determining edge couples.'

    numx = 0
    numy = 0

    mesh.edges(:).couple_index = -1

    ! Determine couples in x-boundaries.
    DO n=1,mesh.nedges
       IF(mesh.edges(n).bnd/=mesh_bnd_prdx1) THEN
          CYCLE
       END IF

       DO m=1,mesh.nedges
          IF(mesh.edges(m).bnd/=mesh_bnd_prdx2) THEN
             CYCLE
          END IF

          a1 = mesh.nodes(mesh.edges(n).node_indices(1)).p
          a2 = mesh.nodes(mesh.edges(n).node_indices(2)).p

          b1 = mesh.nodes(mesh.edges(m).node_indices(1)).p
          b2 = mesh.nodes(mesh.edges(m).node_indices(2)).p

          IF(cmp_real_pairs((/a1(2),a2(2)/), (/b1(2),b2(2)/), eps) .AND.&
               cmp_real_pairs((/a1(3),a2(3)/), (/b1(3),b2(3)/), eps)) THEN
             mesh.edges(n).couple_index = m
             mesh.edges(m).couple_index = n
             numx = numx + 1
          END IF
       END DO
    END DO

    ! Determine couples in y-boundaries.
    DO n=1,mesh.nedges
       IF(mesh.edges(n).bnd/=mesh_bnd_prdy1) THEN
          CYCLE
       END IF

       DO m=1,mesh.nedges
          IF(mesh.edges(m).bnd/=mesh_bnd_prdy2) THEN
             CYCLE
          END IF

          a1 = mesh.nodes(mesh.edges(n).node_indices(1)).p
          a2 = mesh.nodes(mesh.edges(n).node_indices(2)).p

          b1 = mesh.nodes(mesh.edges(m).node_indices(1)).p
          b2 = mesh.nodes(mesh.edges(m).node_indices(2)).p

          IF(cmp_real_pairs((/a1(1),a2(1)/), (/b1(1),b2(1)/), eps) .AND.&
               cmp_real_pairs((/a1(3),a2(3)/), (/b1(3),b2(3)/), eps)) THEN
             mesh.edges(n).couple_index = m
             mesh.edges(m).couple_index = n
             numy = numy + 1
          END IF
       END DO
    END DO

    ! Make sure that the edge couples have basis functions with the same orientation,
    ! so that they can be given equal expansion coefficients.
    DO n=1,mesh.nedges
       IF(mesh.edges(n).bnd/=mesh_bnd_prdx1 .AND. mesh.edges(n).bnd/=mesh_bnd_prdy1) THEN
          CYCLE
       END IF

       m = mesh.edges(n).couple_index
       
       IF(mesh.edges(n).face_indices(1)/=-1 .AND. mesh.edges(m).face_indices(1)/=-1) THEN
          CALL swap(mesh.edges(m).face_indices(1), mesh.edges(m).face_indices(2))
          CALL swap(mesh.edges(m).bnode_indices(1), mesh.edges(m).bnode_indices(2))
       END IF

       IF(mesh.edges(n).face_indices(2)/=-1 .AND. mesh.edges(m).face_indices(2)/=-1) THEN
          CALL swap(mesh.edges(m).face_indices(1), mesh.edges(m).face_indices(2))
          CALL swap(mesh.edges(m).bnode_indices(1), mesh.edges(m).bnode_indices(2))
       END IF
    END DO

    WRITE(*,'(A,I0,A,I0,A)') 'Found ', numx, ' x-couples and ', numy, ' y-couples.'
  END SUBROUTINE determine_edge_couples

  SUBROUTINE invert_faces(mesh, id)
     !dir$ optimize:3
     !dir$ attributes code_align : 32 :: invert_faces
     !dir$ attributes forceinline :: invert_faces
    TYPE(mesh_container), INTENT(INOUT) :: mesh
    INTEGER(kind=i4), INTENT(IN) :: id
    INTEGER(kind=i4) :: n

    DO n=1,mesh.nfaces
       IF(mesh.faces(n).id==id) THEN
          mesh.faces(n).node_indices(1:3) = mesh.faces(n).node_indices((/3,2,1/))
       END IF
    END DO
  END SUBROUTINE invert_faces

  FUNCTION extract_submesh(mesh, ids, vol_ids) RESULT(submesh)
     !dir$ optimize:3
     !dir$ attributes code_align : 32 :: extract_submesh
    TYPE(mesh_container), INTENT(IN) :: mesh
    INTEGER(kind=i4), DIMENSION(:), INTENT(IN) :: ids
    INTEGER(kind=i4), DIMENSION(:), POINTER, INTENT(IN) :: vol_ids
    TYPE(mesh_container) :: submesh
    INTEGER(kind=i4) :: n, m, nfaces, nsolids
    LOGICAL, DIMENSION(mesh.nnodes) :: nmask
    INTEGER(kind=i4), DIMENSION(mesh.nnodes) :: local_node_indices

    nmask(:) = .FALSE.

    ! Compute the number of faces for this submesh.
    nfaces = 0
    DO n=1,mesh.nfaces
       IF(COUNT(ids==mesh.faces(n).id)/=0) THEN
          nfaces = nfaces + 1
          DO m=1,3
             nmask(mesh.faces(n).node_indices(m)) = .TRUE.
          END DO
       END IF
    END DO

    submesh.nfaces = nfaces
    ALLOCATE(submesh.faces(1:nfaces))

    ! Compute the number of solids for this submesh.
    IF(ASSOCIATED(vol_ids)) THEN
       nsolids = 0
       DO n=1,mesh.nsolids
          IF(COUNT(vol_ids==mesh.solids(n).id)/=0) THEN
             nsolids = nsolids + 1
             DO m=1,4
                nmask(mesh.solids(n).node_indices(m)) = .TRUE.
             END DO
          END IF
       END DO

       submesh.nsolids = nsolids
       ALLOCATE(submesh.solids(1:nsolids))
    ELSE
       submesh.nsolids = 0
    END IF

    ! Copy the vertices.
    local_node_indices(:) = -1
    submesh.nnodes = COUNT(nmask==.TRUE.)
    ALLOCATE(submesh.nodes(1:submesh.nnodes))
    m = 0
    DO n=1,mesh.nnodes
       IF(nmask(n)) THEN
          m = m + 1
          submesh.nodes(m).p = mesh.nodes(n).p
          submesh.nodes(m).parent_index = n

          local_node_indices(n) = m
       END IF
    END DO

    ! Copy the faces.
    m = 0
    DO n=1,mesh.nfaces
       IF(COUNT(ids==mesh.faces(n).id)/=0) THEN
          m = m + 1
          submesh.faces(m).node_indices(1:3) = local_node_indices(mesh.faces(n).node_indices(1:3))
          submesh.faces(m).parent_index = n
          submesh.faces(m).id = mesh.faces(n).id
       END IF
    END DO

    ! Copy the solids.
    IF(ASSOCIATED(vol_ids)) THEN
       m = 0
       DO n=1,mesh.nsolids
          IF(COUNT(vol_ids==mesh.solids(n).id)/=0) THEN
             m = m + 1
             submesh.solids(m).node_indices(1:4) = &
                  local_node_indices(mesh.solids(n).node_indices(1:4))
             submesh.solids(m).id = mesh.solids(n).id
          END IF
       END DO
    END IF

    submesh.nlines = 0
    submesh.nedges = 0
    submesh.nsolid_faces = 0

  END FUNCTION extract_submesh

  SUBROUTINE submesh_edge_connectivity(mesh, submeshes)
    TYPE(mesh_container), INTENT(INOUT) :: mesh
    TYPE(mesh_container), DIMENSION(:), INTENT(INOUT) :: submeshes
    INTEGER(kind=i4) :: n, m, l
    INTEGER(kind=i4), DIMENSION(2) :: node_indices
    INTEGER(kind=i4), DIMENSION(1:mesh.nedges) :: nchild
    INTEGER(kind=i4), DIMENSION(1:SIZE(submeshes),1:2) :: tmp
    INTEGER(kind=i4) :: ge1, ge2, le1, le2, sm1, sm2

    WRITE(*,*) 'Establishing submesh-parent edge connectivity.'

    nchild(:) = 0

    ! Allocate reservoir for child edge indices.
    DO n=1,mesh.nedges
       ALLOCATE(mesh.edges(n).child_indices(1:SIZE(submeshes),1:2))
    END DO
   
    ! Determine child-parent edge connectivity.
    DO n=1,mesh.nedges
       DO m=1,SIZE(submeshes)
          DO l=1,submeshes(m).nedges
             node_indices = submeshes(m).nodes(submeshes(m).edges(l).node_indices(1:2)).parent_index

             ! Compare (parent) node indices of the edges.
             IF(cmp_pairs(mesh.edges(n).node_indices, node_indices)) THEN
                submeshes(m).edges(l).parent_index = n
                nchild(n) = nchild(n) + 1
                mesh.edges(n).child_indices(nchild(n),1) = m
                mesh.edges(n).child_indices(nchild(n),2) = l
             END IF
          END DO
       END DO
    END DO

    ! Trim child edge index arrays.
    DO n=1,mesh.nedges
       IF(nchild(n)<2) THEN
          WRITE(*,*) 'Submesh decomposition is inconsistent (invalid child edges)!'
          STOP
       END IF

       tmp(1:nchild(n),:) = mesh.edges(n).child_indices(1:nchild(n),:)
       DEALLOCATE(mesh.edges(n).child_indices)
       ALLOCATE(mesh.edges(n).child_indices(1:nchild(n),1:2))
       mesh.edges(n).child_indices(1:nchild(n),1:2) = tmp(1:nchild(n),1:2)
    END DO

    ! Inherit edge properties to submeshes.
    DO ge1=1,mesh.nedges
       ge2 = mesh.edges(ge1).couple_index

       DO m=1,SIZE(mesh.edges(ge1).child_indices,1)
          sm1 = mesh.edges(ge1).child_indices(m,1)
          le1 = mesh.edges(ge1).child_indices(m,2)

          submeshes(sm1).edges(le1).bnd = mesh.edges(ge1).bnd

          IF(ge2/=-1) THEN
             DO l=1,SIZE(mesh.edges(ge2).child_indices,1)
                sm2 = mesh.edges(ge2).child_indices(l,1)
                le2 = mesh.edges(ge2).child_indices(l,2)

                IF(sm2==sm1) THEN
                   submeshes(sm1).edges(le1).couple_index = le2
                   submeshes(sm2).edges(le2).couple_index = le1
                   EXIT
                END IF
             END DO
          END IF

       END DO
    END DO

  END SUBROUTINE submesh_edge_connectivity

  SUBROUTINE orient_basis(mesh, submeshes)
    TYPE(mesh_container), INTENT(IN) :: mesh
    TYPE(mesh_container), DIMENSION(:), INTENT(INOUT) :: submeshes
    INTEGER(kind=i4) :: n, m, l, k, nchild
    LOGICAL, DIMENSION(1:SIZE(submeshes)) :: oriented
    INTEGER(kind=i4) :: pf1, pf2, nf1, nf2

    WRITE(*,*) 'Orienting submesh basis functions'

    ! Orient the boundary basis functions of the submeshes.
    ! Make sure that the edge couples have basis functions with the same orientation,
    ! so that they can be given equal expansion coefficients.
    DO k=1,SIZE(submeshes)
       DO n=1,submeshes(k).nedges
          IF(submeshes(k).edges(n).bnd/=mesh_bnd_prdx1 .AND.&
               submeshes(k).edges(n).bnd/=mesh_bnd_prdy1) THEN
             CYCLE
          END IF
          
          m = submeshes(k).edges(n).couple_index
          
          IF(submeshes(k).edges(n).face_indices(1)/=-1 .AND.&
               submeshes(k).edges(m).face_indices(1)/=-1) THEN
             CALL swap(submeshes(k).edges(m).face_indices(1), submeshes(k).edges(m).face_indices(2))
             CALL swap(submeshes(k).edges(m).bnode_indices(1), submeshes(k).edges(m).bnode_indices(2))
          END IF
          
          IF(submeshes(k).edges(n).face_indices(2)/=-1 .AND.&
               submeshes(k).edges(m).face_indices(2)/=-1) THEN
             CALL swap(submeshes(k).edges(m).face_indices(1), submeshes(k).edges(m).face_indices(2))
             CALL swap(submeshes(k).edges(m).bnode_indices(1), submeshes(k).edges(m).bnode_indices(2))
          END IF
       END DO
    END DO

    DO n=1,mesh.nedges
       nchild = SIZE(mesh.edges(n).child_indices,1)
       oriented(:) = .FALSE.
       oriented(1) = .TRUE.

       ! This loop is a limited iteration until all basis functions
       ! assigned to edge n are oriented.
       DO m=1,nchild

          ! Loop through child edges of parent edge n. Skip the reference l==1.
          DO l=2,nchild
             
             ! If basis function of local edge l is oriented, skip.
             IF(oriented(l)) THEN
                CYCLE
             END IF

             ! Compare local edge l to oriented local edges k.
             DO k=1,nchild
                IF(oriented(k)==.FALSE.) THEN
                   CYCLE
                END IF

                ! Determine local face indices of basis functions.
                pf1 = submeshes(mesh.edges(n).child_indices(l,1)).edges(mesh.edges(n).child_indices(l,2)).face_indices(1)
                nf1 = submeshes(mesh.edges(n).child_indices(l,1)).edges(mesh.edges(n).child_indices(l,2)).face_indices(2)
                pf2 = submeshes(mesh.edges(n).child_indices(k,1)).edges(mesh.edges(n).child_indices(k,2)).face_indices(1)
                nf2 = submeshes(mesh.edges(n).child_indices(k,1)).edges(mesh.edges(n).child_indices(k,2)).face_indices(2)

                ! Transform local indices into parent indices.
                IF(pf1/=-1) THEN
                   pf1 = submeshes(mesh.edges(n).child_indices(l,1)).faces(pf1).parent_index
                END IF
                IF(nf1/=-1) THEN
                   nf1 = submeshes(mesh.edges(n).child_indices(l,1)).faces(nf1).parent_index
                END IF
                IF(pf2/=-1) THEN
                   pf2 = submeshes(mesh.edges(n).child_indices(k,1)).faces(pf2).parent_index
                END IF
                IF(nf2/=-1) THEN
                   nf2 = submeshes(mesh.edges(n).child_indices(k,1)).faces(nf2).parent_index
                END IF

                ! If necessary, invert the basis function of edge l.
                IF(pf1==pf2 .OR. nf1==nf2) THEN
                   CALL swap(submeshes(mesh.edges(n).child_indices(l,1)).edges(mesh.edges(n).child_indices(l,2)).face_indices(1),&
                        submeshes(mesh.edges(n).child_indices(l,1)).edges(mesh.edges(n).child_indices(l,2)).face_indices(2))
                   CALL swap(submeshes(mesh.edges(n).child_indices(l,1)).edges(mesh.edges(n).child_indices(l,2)).bnode_indices(1),&
                        submeshes(mesh.edges(n).child_indices(l,1)).edges(mesh.edges(n).child_indices(l,2)).bnode_indices(2))
                END IF

                IF(pf1==pf2 .OR. nf1==nf2 .OR. pf1==nf2 .OR. nf1==pf2) THEN
                   oriented(l) = .TRUE.
                   EXIT
                END IF
             END DO

          END DO

          IF(COUNT(oriented==.TRUE.)==nchild) THEN
             EXIT
          END IF
       END DO

       IF(m==nchild) THEN
          WRITE(*,*) 'Basis orientation was unsuccessful!'
          EXIT
       END IF
    END DO
  END SUBROUTINE orient_basis

  SUBROUTINE get_bnd_edges(mesh, bnd, nse, edgeind)
    TYPE(mesh_container), INTENT(IN) :: mesh
    INTEGER(kind=i4), INTENT(IN) :: bnd
    INTEGER(kind=i4), INTENT(OUT) :: nse
    INTEGER(kind=i4), INTENT(INOUT), DIMENSION(mesh.nedges) :: edgeind

    INTEGER(kind=i4) :: n

    nse = 0

    DO n=1,mesh.nedges
       IF(mesh.edges(n).bnd==bnd) THEN
          nse = nse + 1
          edgeind(nse) = n
       END IF
    END DO
  END SUBROUTINE get_bnd_edges

  FUNCTION get_mesh_element_lines(filename) RESULT(lines)
    CHARACTER (LEN=256), INTENT(IN) :: filename
    INTEGER(kind=i4), DIMENSION(:,:), POINTER :: lines
    INTEGER(kind=i4) :: nelements, element_number, iovar, fid = 10, n
    CHARACTER (LEN=256) :: lineid

    OPEN(fid, FILE=TRIM(filename), ACTION='READ', IOSTAT=iovar)
    IF(iovar>0) THEN
       WRITE(*,*) 'Could not open mesh file ', TRIM(filename), '!'
       STOP
    END IF

    iovar = 0
    DO WHILE(iovar==0)
       READ(fid, *, IOSTAT=iovar) lineid
       IF(lineid=='$Elements') THEN
          READ(fid, *, IOSTAT=iovar) nelements
          ALLOCATE(lines(nelements,1:2))
          DO n=1,nelements
             READ(fid, *, IOSTAT=iovar) element_number, lines(n,1), lines(n,2)
          END DO
          READ(fid, *, IOSTAT=iovar) lineid
          IF(lineid/='$EndElements') THEN
             WRITE(*,*) 'Could not find $EndElements specifier!'
             STOP
          END IF
       END IF
    END DO

    CLOSE(fid)
  END FUNCTION get_mesh_element_lines

  FUNCTION load_mesh_gmsh(filename) RESULT(mesh)
    CHARACTER (LEN=*), INTENT(IN) :: filename
    CHARACTER (LEN=256) :: lineid, line
    TYPE(mesh_container) :: mesh
    INTEGER(kind=i4) :: fid = 10, iovar, nnodes, node_number, n,&
         nelements, element_number, element_type, cface, cline, csolid, ntags
    INTEGER(kind=i4), DIMENSION(10) :: element_data
    INTEGER(kind=i4), DIMENSION(:,:), POINTER :: element_types
    REAL (KIND=dp), DIMENSION(3) :: np
    CHARACTER (LEN=3) :: mshver

    element_types => get_mesh_element_lines(filename)

    OPEN(fid, FILE=TRIM(filename), ACTION='READ', IOSTAT=iovar)
    IF(iovar>0) THEN
       WRITE(*,*) 'Could not open mesh file!'
       STOP
    END IF

    iovar = 0
    DO WHILE(iovar==0)
       READ(fid, *, IOSTAT=iovar) lineid

       IF(lineid=='$MeshFormat') THEN
          READ(fid, '(A3)', IOSTAT=iovar) mshver
          IF(mshver/='2.1' .AND. mshver/='2.2') THEN
             WRITE(*,*) 'Mesh version is unsupported!'
             CLOSE(fid)
             STOP
          END IF

          WRITE(*,'(A,A3)') 'Mesh version: ', mshver

          READ(fid, *, IOSTAT=iovar) lineid
          IF(lineid/='$EndMeshFormat') THEN
             WRITE(*,*) 'Could not find $MeshFormatNodes specifier!'
             STOP
          END IF
       ELSE IF(lineid=='$Nodes') THEN
          READ(fid, *, IOSTAT=iovar) nnodes
          ALLOCATE(mesh.nodes(nnodes))

          DO n=1,nnodes
             READ(fid, *, IOSTAT=iovar) node_number, np(1:3)
             mesh.nodes(node_number).p = np
          END DO

          READ(fid, *, IOSTAT=iovar) lineid
          IF(lineid/='$EndNodes') THEN
             WRITE(*,*) 'Could not find $EndNodes specifier!'
             STOP
          END IF
          mesh.nnodes = nnodes
       ELSE IF(lineid=='$Elements') THEN
          mesh.nfaces = COUNT(element_types(:,1)==2)
          mesh.nlines = COUNT(element_types(:,1)==1)
          mesh.nsolids = COUNT(element_types(:,1)==4)

          ALLOCATE(mesh.faces(1:mesh.nfaces))

          IF(mesh.nlines/=0) THEN
             ALLOCATE(mesh.lines(1:mesh.nlines))
          END IF

          IF(mesh.nsolids/=0) THEN
             ALLOCATE(mesh.solids(1:mesh.nsolids))
          END IF

          READ(fid, *, IOSTAT=iovar) nelements

          cface = 0
          cline = 0
          csolid = 0

          DO n=1,nelements
             ntags = element_types(n,2)

             IF(element_types(n,1)==2) THEN
                cface = cface + 1
                READ(fid, *, IOSTAT=iovar) element_data(1:(ntags+6))
                mesh.faces(cface).id = element_data(4)
                mesh.faces(cface).node_indices(1:3) = element_data((4+ntags):(6+ntags))
             ELSE IF(element_types(n,1)==1) THEN
                cline = cline + 1
                READ(fid, *, IOSTAT=iovar) element_data(1:(ntags+5))
                mesh.lines(cline).id = element_data(4)
                mesh.lines(cline).node_indices(1:2) = element_data((4+ntags):(5+ntags))
             ELSE IF(element_types(n,1)==4) THEN
                csolid = csolid + 1
                READ(fid, *, IOSTAT=iovar) element_data(1:(ntags+7))
                mesh.solids(csolid).id = element_data(4)
                mesh.solids(csolid).node_indices(1:4) = element_data((ntags+4):(ntags+7))
             ELSE
                READ(fid, *, IOSTAT=iovar) element_data(1)
             END IF
          END DO

          READ(fid, *, IOSTAT=iovar) lineid
          IF(lineid/='$EndElements') THEN
             WRITE(*,*) 'Could not find $EndElements specifier!'
             STOP
          END IF
       END IF
    END DO

    DEALLOCATE(element_types)

    CLOSE(fid)

    ! Change vertex index rotation.
  !  DO n=1,mesh.nfaces
  !     CALL swap(mesh.faces(n).node_indices(2), mesh.faces(n).node_indices(3))
  !  END DO


    !OPEN(10, FILE='nodes', ACTION='write')
    !DO n=1,mesh.nnodes
    !   WRITE(10,'(I5,3E15.3)') n, mesh.nodes(n).p
    !END DO
    !CLOSE(10)

    !OPEN(10, FILE='faces', ACTION='write')
    !DO n=1,mesh.nfaces
    !   WRITE(10,'(I5,3I5)') n, mesh.faces(n).node_indices
    !END DO
    !CLOSE(10)
  END FUNCTION load_mesh_gmsh

  FUNCTION load_mesh_neutral(filename) RESULT(mesh)
    CHARACTER (LEN=*), INTENT(IN) :: filename
    TYPE(mesh_container) :: mesh
    INTEGER(kind=i4) :: fid = 10, n, iovar, bndid

    OPEN(fid, FILE=TRIM(filename), ACTION='READ', IOSTAT=iovar)
    IF(iovar>0) THEN
       WRITE(*,*) 'Could not open mesh file!'
       STOP
    END IF

    iovar = 0

    ! Load nodes.
    
    READ(fid, *, IOSTAT=iovar) mesh.nnodes
    
    ALLOCATE(mesh.nodes(mesh.nnodes))
    
    DO n=1,mesh.nnodes
       READ(fid, *, IOSTAT=iovar) mesh.nodes(n).p(1:3)
    END DO
    
    ! Load solids (tetrahedra).
    
    READ(fid, *, IOSTAT=iovar) mesh.nsolids
    
    IF(mesh.nsolids>0) THEN
       ALLOCATE(mesh.solids(mesh.nsolids))
       
       DO n=1,mesh.nsolids
          mesh.solids(n).id = 1
          READ(fid, *, IOSTAT=iovar) mesh.solids(n).node_indices(1:4)
       END DO
    END IF
    
    ! Load faces (triangles).
    
    READ(fid, *, IOSTAT=iovar) mesh.nfaces
    
    ALLOCATE(mesh.faces(mesh.nfaces))
    
    DO n=1,mesh.nfaces
       mesh.faces(n).id = 1
       READ(fid, *, IOSTAT=iovar) bndid, mesh.faces(n).node_indices(1:3)
    END DO

    CLOSE(fid)

    mesh.nlines = 0

  END FUNCTION load_mesh_neutral

  FUNCTION load_mesh(filename) RESULT(mesh)
    CHARACTER (LEN=*), INTENT(IN) :: filename
    TYPE(mesh_container) :: mesh
    CHARACTER (LEN=3) :: ext

    ext = getext(filename)

    IF(ext=='msh') THEN
       mesh = load_mesh_gmsh(filename)
    ELSE IF(ext=='nmf') THEN
       mesh = load_mesh_neutral(filename)
    ELSE
       WRITE(*,*) 'Unrecognized mesh file extension!'
       STOP
    END IF

    mesh.nsolid_faces = 0

    WRITE(*,*) 'Mesh file loaded successfully.'
    WRITE(*,'(A,I0,:,A)') ' - Read ', mesh.nnodes, ' nodes'
    WRITE(*,'(A,I0,:,A)') ' - Read ', mesh.nfaces, ' faces'
    WRITE(*,'(A,I0,:,A)') ' - Read ', mesh.nlines, ' lines'
    WRITE(*,'(A,I0,:,A)') ' - Read ', mesh.nsolids, ' solids'
    
  END FUNCTION load_mesh

  SUBROUTINE delete_mesh(mesh)
    TYPE(mesh_container), INTENT(INOUT) :: mesh
    INTEGER(kind=i4) :: n

    IF(ALLOCATED(mesh.nodes)) THEN
       DEALLOCATE(mesh.nodes)
    END IF

    IF(ALLOCATED(mesh.faces)) THEN
       DEALLOCATE(mesh.faces)
    END IF

    IF(ALLOCATED(mesh.lines)) THEN
       DEALLOCATE(mesh.lines)
    END IF

    IF(ALLOCATED(mesh.edges)) THEN
       DO n=1,mesh.nedges
          IF(ALLOCATED(mesh.edges(n).child_indices)) THEN
             DEALLOCATE(mesh.edges(n).child_indices)
          END IF
       END DO

       DEALLOCATE(mesh.edges)
    END IF

    IF(ALLOCATED(mesh.solids)) THEN
       DEALLOCATE(mesh.solids)
    END IF

    IF(ALLOCATED(mesh.solid_faces)) THEN
       DEALLOCATE(mesh.solid_faces)
    END IF
  END SUBROUTINE delete_mesh

  SUBROUTINE order(p,q)
    INTEGER(kind=i4) :: p,q,temp

    IF(p>q) THEN
       temp = p
       p = q
       q = temp
    END IF
  END SUBROUTINE order

  SUBROUTINE sort(array, n)
    INTEGER(kind=i4), DIMENSION(:), INTENT(INOUT) :: array
    INTEGER(kind=i4), INTENT(IN) :: n
    INTEGER(kind=i4) :: i, j

    DO i=1, n
       DO j=n, i+1, -1
          CALL order(array(j-1), array(j))
       END DO
    END DO
  END SUBROUTINE sort

  SUBROUTINE swap(a,b)
    INTEGER(kind=i4), INTENT(INOUT) :: a,b
    INTEGER(kind=i4) :: tmp

    tmp = a
    a = b
    b = tmp
  END SUBROUTINE swap

  SUBROUTINE sort3(array)
    INTEGER(kind=i4), DIMENSION(3), INTENT(INOUT) :: array
    INTEGER(kind=i4) :: tmp

    IF(array(1)<=array(2) .AND. array(1)<=array(3)) THEN
       IF(array(3)<=array(2)) THEN
          CALL swap(array(2), array(3))
       END IF
    ELSE IF(array(2)<=array(1) .AND. array(2)<=array(3)) THEN
       CALL swap(array(1), array(2))
       IF(array(3)<=array(2)) THEN
          CALL swap(array(2), array(3))
       END IF
    ELSE IF(array(3)<=array(1) .AND. array(3)<=array(2)) THEN
       CALL swap(array(1), array(3))
       IF(array(3)<=array(2)) THEN
          CALL swap(array(2), array(3))
       END IF
    END IF
  END SUBROUTINE sort3

  SUBROUTINE append_integer(array, element)
    INTEGER(kind=i4), DIMENSION(:), POINTER :: array
    INTEGER(kind=i4), DIMENSION(:), ALLOCATABLE :: temp
    INTEGER(kind=i4), INTENT(IN) :: element
    INTEGER(kind=i4) :: s

    s = SIZE(array)

    IF(ASSOCIATED(array)==.FALSE.) THEN
       ALLOCATE(array(1))
    ELSE
       ALLOCATE(temp(s))
       temp = array
       DEALLOCATE(array)
       ALLOCATE(array(s+1))
       array(1:SIZE(temp)) = temp
       DEALLOCATE(temp)
    END IF
    array(SIZE(array)) = element
  END SUBROUTINE append_integer

  FUNCTION test_inclusion(array, element) RESULT(res)
    INTEGER(kind=i4), DIMENSION(:), POINTER :: array
    INTEGER(kind=i4), INTENT(IN) :: element
    INTEGER(kind=i4) :: n
    LOGICAL :: res

    res = .FALSE.

    IF(ASSOCIATED(array)==.FALSE.) THEN
       RETURN
    END IF

    DO n=1,SIZE(array)
       IF(array(n)==element) THEN
          res = .TRUE.
          RETURN
       END IF
    END DO
  END FUNCTION test_inclusion

  FUNCTION test_inclusion2(array, element) RESULT(res)
    INTEGER(kind=i4), DIMENSION(:) :: array
    INTEGER(kind=i4), INTENT(IN) :: element
    INTEGER(kind=i4) :: n
    LOGICAL :: res

    res = .FALSE.

    DO n=1,SIZE(array)
       IF(array(n)==element) THEN
          res = .TRUE.
          RETURN
       END IF
    END DO
  END FUNCTION test_inclusion2

  FUNCTION cmp_pairs(pair1, pair2) RESULT(res)
      !dir$ optimize:3
      !dir$ attributes code_align : 32 :: cmp_pairs
      !dir$ attributes forceinline :: cmp_pairs
    INTEGER(kind=i4), DIMENSION(2), INTENT(IN) :: pair1, pair2
    LOGICAL :: res

    res = .FALSE.

    IF((pair1(1)==pair2(1) .AND. pair1(2)==pair2(2)) .OR.&
         (pair1(1)==pair2(2) .AND. pair1(2)==pair2(1))) THEN
       res = .TRUE.
    END IF
  END FUNCTION cmp_pairs

  FUNCTION cmp_triplets(triplet1, triplet2) RESULT(res)
    INTEGER(kind=i4), DIMENSION(3), INTENT(IN) :: triplet1, triplet2
    LOGICAL :: res
    INTEGER(kind=i4), PARAMETER, DIMENSION(3,6) :: indices = (/1,2,3, 1,3,2, 2,1,3, 2,3,1, 3,1,2, 3,2,1/)
    INTEGER(kind=i4) :: i

    res = .FALSE.

    DO i=1,6
       IF(triplet1(1)==triplet2(indices(1,i)) .AND.&
            triplet1(2)==triplet2(indices(2,i)) .AND.&
            triplet1(3)==triplet2(indices(3,i))) THEN
          res = .TRUE.
          RETURN
       END IF
    END DO
  END FUNCTION cmp_triplets

  FUNCTION get_free_index(indices, dublet) RESULT(res)
    INTEGER(kind=i4), DIMENSION(3), INTENT(IN) :: indices
    INTEGER(kind=i4), DIMENSION(2), INTENT(IN) :: dublet
    INTEGER(kind=i4) :: res

    IF(cmp_pairs(dublet, indices((/1,2/)))) THEN
       res = indices(3)
    ELSE IF(cmp_pairs(dublet, indices((/2,3/)))) THEN
       res = indices(1)
    ELSE IF(cmp_pairs(dublet, indices((/1,3/)))) THEN
       res = indices(2)
    ELSE
       WRITE(*,*) 'Erroneous dublet comparison!'
    END IF
  END FUNCTION get_free_index

  SUBROUTINE scale_mesh(mesh, scale)
    TYPE(mesh_container), INTENT(INOUT) :: mesh
    REAL (KIND=dp), INTENT(IN) :: scale
    INTEGER(kind=i4) :: n

    DO n=1,mesh.nnodes
       mesh.nodes(n).p =  mesh.nodes(n).p*scale
    END DO
  END SUBROUTINE scale_mesh

  ! Reduces n periodically into set {1,2,3}.
  FUNCTION indexrot3(n) RESULT(index)
    INTEGER(kind=i4), INTENT(IN) :: n
    INTEGER(kind=i4) :: index

    index = n

    IF(index>3) THEN
       index = index - ((index-1)/3)*3
    END IF
  END FUNCTION indexrot3

  ! Reduces n periodically into set {1,2,3,4}.
  FUNCTION indexrot4(n) RESULT(index)
    INTEGER(kind=i4), INTENT(IN) :: n
    INTEGER(kind=i4) :: index

    index = n

    IF(index>4) THEN
       index = index - ((index-1)/4)*4
    END IF
  END FUNCTION indexrot4

  ! Creates unique edges from the given nodes and faces. These edges
  ! are directly associated with the expansion coefficients of surface
  ! current densities.
  SUBROUTINE build_mesh(mesh, scale)
    TYPE(mesh_container), INTENT(INOUT) :: mesh
    REAL (KIND=dp), INTENT(IN) :: scale
    INTEGER(kind=i4) :: n, m, l, cedge, nedges
    TYPE(edge), DIMENSION(:), ALLOCATABLE :: tmpedges
    LOGICAL :: found_edge
    INTEGER(kind=i4), DIMENSION(2) :: pair

    WRITE(*,*) 'Building mesh data'

    CALL scale_mesh(mesh, scale)

    ! Allocate edge reservoir with upper bound size.
    nedges = SIZE(mesh.faces)*3
    ALLOCATE(tmpedges(1:nedges))

    ! Declare node indices with undefined values.
    DO n=1,nedges
       tmpedges(n).node_indices(:) = -1
       tmpedges(n).bnode_indices(:) = -1
       tmpedges(n).face_indices(:) = -1
    END DO

    cedge = 0

    ! Create unique edges.
    DO n=1,SIZE(mesh.faces)
       DO m=1,3

          ! This determined the edge node indexing.
          pair = (/mesh.faces(n).node_indices(m), mesh.faces(n).node_indices(indexrot3(m+1))/)

          ! Check if this edge already exists in the list.
          found_edge = .FALSE.
          DO l=1,cedge
             IF(cmp_pairs(tmpedges(l).node_indices, pair)) THEN
                found_edge = .TRUE.

                ! Add this face index to edge's face list and the second one.
                ! If an edge is shared by more than two faces, only two connections
                ! are recorded.
                tmpedges(l).face_indices(2) = n
                tmpedges(l).bnode_indices(2) = mesh.faces(n).node_indices(indexrot3(m+2))

                ! Add this edge index to face's edge list.
                mesh.faces(n).edge_indices(m) = l

                EXIT
             END IF
          END DO

          IF(found_edge==.FALSE.) THEN
             ! Add new edge.
             cedge = cedge + 1
             tmpedges(cedge).node_indices = pair

             ! Add this face index to edge's face list as the first one.
             tmpedges(cedge).face_indices(1) = n
             tmpedges(cedge).bnode_indices(1) = mesh.faces(n).node_indices(indexrot3(m+2))

             ! Add this edge index to face's edge list.
             mesh.faces(n).edge_indices(m) = cedge
          END IF
       END DO
    END DO

    ! Trim edge arrays.
    mesh.nedges = cedge
    ALLOCATE(mesh.edges(1:cedge))
    DO n=1,cedge
       mesh.edges(n).node_indices(:) = tmpedges(n).node_indices(:)
       mesh.edges(n).bnode_indices(:) = tmpedges(n).bnode_indices(:)
       mesh.edges(n).face_indices(:) = tmpedges(n).face_indices(:)
    END DO

    ! Deallocate temporary arrays.
    DEALLOCATE(tmpedges)

    mesh.edges(:).bnd = mesh_bnd_none

    ! Compute average edge length.
    mesh.avelen = average_edge_length(mesh)

    WRITE(*,'(A,I0,:,A)') ' - Created ', cedge, ' unique edges'

    !IF(mesh.nsolids>0) THEN
    !   CALL build_solid_faces(mesh)
    !END IF

    CALL compute_basis_data(mesh)

    WRITE(*,*) 'Mesh data built successfully'

  END SUBROUTINE build_mesh

  ! Called by build_mesh.
  SUBROUTINE build_solid_faces(mesh)
    TYPE(mesh_container), INTENT(INOUT) :: mesh
    INTEGER(kind=i4) :: n, m, l, cface, nfaces, nbnds
    TYPE(solid_face), DIMENSION(:), ALLOCATABLE :: tmpfaces
    LOGICAL :: found_face
    INTEGER(kind=i4), DIMENSION(3) :: triplet

    ! Allocate face reservoir with upper bound size.
    nfaces = SIZE(mesh.solids)*4
    ALLOCATE(tmpfaces(1:nfaces))

    ! Declare node indices with undefined values.
    DO n=1,nfaces
       tmpfaces(n).node_indices(:) = -1
       tmpfaces(n).bnode_indices(:) = -1
       tmpfaces(n).solid_indices(:) = -1
    END DO

    cface = 0

    ! Create unique faces.
    DO n=1,SIZE(mesh.solids)
       DO m=1,4

          ! This determined the face node indexing.
          triplet = (/mesh.solids(n).node_indices(m),&
               mesh.solids(n).node_indices(indexrot4(m+1)),&
               mesh.solids(n).node_indices(indexrot4(m+2))/)

          ! Check if this face already exists in the list.
          found_face = .FALSE.
          DO l=1,cface
             IF(cmp_triplets(tmpfaces(l).node_indices, triplet)) THEN
                found_face = .TRUE.

                ! Add this face index to edge's face list and the second one.
                ! If an edge is shared by more than two faces, only two connections
                ! are recorded.
                tmpfaces(l).solid_indices(2) = n
                tmpfaces(l).bnode_indices(2) = mesh.solids(n).node_indices(indexrot4(m+3))

                ! Add this face index to tetrahedra's face list.
                mesh.solids(n).solid_face_indices(m) = l

                EXIT
             END IF
          END DO

          IF(found_face==.FALSE.) THEN
             ! Add new face.
             cface = cface + 1
             tmpfaces(cface).node_indices = triplet

             ! Add this face index to edge's face list as the first one.
             tmpfaces(cface).solid_indices(1) = n
             tmpfaces(cface).bnode_indices(1) = mesh.solids(n).node_indices(indexrot4(m+3))

             ! Add this face index to tetrahedra's face list.
             mesh.solids(n).solid_face_indices(m) = cface
          END IF
       END DO
    END DO

    ! Trim face arrays.
    mesh.nsolid_faces = cface
    ALLOCATE(mesh.solid_faces(1:cface))
    
    DO n=1,cface
       mesh.solid_faces(n).node_indices(:) = tmpfaces(n).node_indices(:)
       mesh.solid_faces(n).bnode_indices(:) = tmpfaces(n).bnode_indices(:)
       mesh.solid_faces(n).solid_indices(:) = tmpfaces(n).solid_indices(:)
       mesh.solid_faces(n).face_index = -1
    END DO

    ! Deallocate temporary arrays.
    DEALLOCATE(tmpfaces)

    ! Connect boundary solid faces to surface faces.
    DO n=1,mesh.nsolid_faces
       IF(mesh.solid_faces(n).solid_indices(1)==-1 .OR.&
            mesh.solid_faces(n).solid_indices(2)==-1) THEN
          DO m=1,mesh.nfaces
             IF(cmp_triplets(mesh.solid_faces(n).node_indices, mesh.faces(m).node_indices)) THEN
                mesh.solid_faces(n).face_index = m
                EXIT
             END IF
          END DO

          IF(mesh.solid_faces(n).face_index==-1) THEN
             WRITE(*,*) 'Could not connect solid boundary face to surface face!'
             STOP
          END IF
       END IF
    END DO

    WRITE(*,'(A,I0,:,A)') ' - Created ', cface, ' unique solid faces'

  END SUBROUTINE build_solid_faces

  SUBROUTINE compute_mesh_essentials(mesh)
      !dir$ optimize:3
      !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_mesh_essentials
    TYPE(mesh_container), INTENT(INOUT) :: mesh
    INTEGER(kind=i4) :: n
    REAL (KIND=dp), DIMENSION(4) :: b, p1, p2, p3, p4
    
    !dir$ vector vectorlength(8)
    !dir$ vector multiple_gather_scatter_by_shuffles 
    !dir$ vector always
    DO n=1,mesh.nfaces
       p1 = mesh.nodes(mesh.faces(n).node_indices(1)).p
       p2 = mesh.nodes(mesh.faces(n).node_indices(2)).p
       p3 = mesh.nodes(mesh.faces(n).node_indices(3)).p
    
       b = crossr((p2-p1), (p3-p1))
       mesh.faces(n).n = b/normr(b)

       mesh.faces(n).pd = dotr(mesh.faces(n).n, p1)

       mesh.faces(n).cp = (p1 + p2 + p3)*0.33333333333333333333333333333333333_dp
    END DO
    !dir$ vector vectorlength(8)
    !dir$ vector multiple_gather_scatter_by_shuffles 
    !dir$ vector always
    DO n=1,mesh.nsolids
       p1 = mesh.nodes(mesh.solids(n).node_indices(1)).p
       p2 = mesh.nodes(mesh.solids(n).node_indices(2)).p
       p3 = mesh.nodes(mesh.solids(n).node_indices(3)).p
       p4 = mesh.nodes(mesh.solids(n).node_indices(4)).p
       mesh.solids(n).volume = ABS(dotr((p1-p4),crossr((p2-p4),(p3-p4))))/6.0_dp
    END DO
  END SUBROUTINE compute_mesh_essentials

  SUBROUTINE compute_basis_data(mesh)
      !dir$ optimize:3
      !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_basis_data
    TYPE(mesh_container), INTENT(INOUT) :: mesh
    INTEGER(kind=i4) :: n
    REAL (KIND=dp), DIMENSION(4) :: b, p1, p2, p3, p4, p21, p32, p13

    !WRITE(*,*) '- Computing basis function data'
    
    !dir$ vector vectorlength(8)
    !dir$ vector multiple_gather_scatter_by_shuffles 
    !dir$ vector always
    DO n=1,mesh.nfaces
       p1 = mesh.nodes(mesh.faces(n).node_indices(1)).p
       p2 = mesh.nodes(mesh.faces(n).node_indices(2)).p
       p3 = mesh.nodes(mesh.faces(n).node_indices(3)).p
    
       b = crossr((p2-p1), (p3-p1))
       mesh.faces(n).n = b/normr(b)

       mesh.faces(n).pd = dotr(mesh.faces(n).n, p1)
    
       p21 = p2 - p1
       p32 = p3 - p2
       p13 = p1 - p3
       mesh.faces(n).s(:,1) = p21/normr(p21)
       mesh.faces(n).s(:,2) = p32/normr(p32)
       mesh.faces(n).s(:,3) = p13/normr(p13)
    
       mesh.faces(n).m(:,1) = crossr(mesh.faces(n).s(:,1), mesh.faces(n).n)
       mesh.faces(n).m(:,2) = crossr(mesh.faces(n).s(:,2), mesh.faces(n).n)
       mesh.faces(n).m(:,3) = crossr(mesh.faces(n).s(:,3), mesh.faces(n).n)

       mesh.faces(n).m(:,1) = mesh.faces(n).m(:,1)/normr(mesh.faces(n).m(:,1))
       mesh.faces(n).m(:,2) = mesh.faces(n).m(:,2)/normr(mesh.faces(n).m(:,2))
       mesh.faces(n).m(:,3) = mesh.faces(n).m(:,3)/normr(mesh.faces(n).m(:,3))
    
       mesh.faces(n).area = 0.5_dp*normr(crossr(-p13,p32))

       mesh.faces(n).cp = (p1 + p2 + p3)/3.0_dp
    END DO

    DO n=1,mesh.nedges
       mesh.edges(n).length = normr(mesh.nodes(mesh.edges(n).node_indices(1)).p -&
            mesh.nodes(mesh.edges(n).node_indices(2)).p)
    END DO
    
    !dir$ vector vectorlength(8)
    !dir$ vector multiple_gather_scatter_by_shuffles 
    !dir$ vector always
    DO n=1,mesh.nsolids
       p1 = mesh.nodes(mesh.solids(n).node_indices(1)).p
       p2 = mesh.nodes(mesh.solids(n).node_indices(2)).p
       p3 = mesh.nodes(mesh.solids(n).node_indices(3)).p
       p4 = mesh.nodes(mesh.solids(n).node_indices(4)).p
       mesh.solids(n).volume = ABS(dotr((p1-p4),crossr((p2-p4),(p3-p4))))*0.16666666666_dp
    END DO

    !dir$ vector vectorlength(8)
    !dir$ vector multiple_gather_scatter_by_shuffles 
    !dir$ vector always
    DO n=1,mesh.nsolid_faces
       p1 = mesh.nodes(mesh.solid_faces(n).node_indices(1)).p
       p2 = mesh.nodes(mesh.solid_faces(n).node_indices(2)).p
       p3 = mesh.nodes(mesh.solid_faces(n).node_indices(3)).p

       mesh.solid_faces(n).area = 0.5_dp*normr(crossr(p3-p1,p3-p2))
    END DO
  END SUBROUTINE compute_basis_data

  ! Result is positive if the given face is the positive faces of given edge.
  ! Result is negative if the given face is the negative faces of given edge.
  FUNCTION get_face_sign(faceind, edgeind, mesh) RESULT(res)
    INTEGER(kind=i4), INTENT(IN) :: faceind, edgeind
    TYPE(mesh_container), INTENT(IN) :: mesh
    REAL (KIND=dp) :: res

    IF(faceind==mesh.edges(mesh.faces(faceind).edge_indices(edgeind)).face_indices(1)) THEN
       res = 1.0_dp
    ELSE IF(faceind==mesh.edges(mesh.faces(faceind).edge_indices(edgeind)).face_indices(2)) THEN
       res = -1.0_dp
    ELSE
       WRITE(*,*) 'Error evaluating mesh face sign (this could imply corrupted mesh connectivity)!'
    END IF
  END FUNCTION get_face_sign

  FUNCTION get_posit_bnode(faceind, edgeind, mesh) RESULT(res)
    INTEGER(kind=i4), INTENT(IN) :: faceind, edgeind
    TYPE(mesh_container), INTENT(IN) :: mesh
    REAL (KIND=dp), DIMENSION(3) :: res

    res = mesh.nodes(mesh.edges(mesh.faces(faceind).edge_indices(edgeind)).bnode_indices(1)).p
  END FUNCTION get_posit_bnode

  FUNCTION get_negat_bnode(faceind, edgeind, mesh) RESULT(res)
    INTEGER(kind=i4), INTENT(IN) :: faceind, edgeind
    TYPE(mesh_container), INTENT(IN) :: mesh
    REAL (KIND=dp), DIMENSION(3) :: res

    res = mesh.nodes(mesh.edges(mesh.faces(faceind).edge_indices(edgeind)).bnode_indices(2)).p
  END FUNCTION get_negat_bnode

  FUNCTION get_face_bnode(faceind, edgeind, mesh) RESULT(res)
    INTEGER(kind=i4), INTENT(IN) :: faceind, edgeind
    TYPE(mesh_container), INTENT(IN) :: mesh
    REAL (KIND=dp), DIMENSION(3) :: res

    IF(mesh.edges(mesh.faces(faceind).edge_indices(edgeind)).face_indices(1)==faceind) THEN
       res = mesh.nodes(mesh.edges(mesh.faces(faceind).edge_indices(edgeind)).bnode_indices(1)).p
    ELSE IF(mesh.edges(mesh.faces(faceind).edge_indices(edgeind)).face_indices(2)==faceind) THEN
       res = mesh.nodes(mesh.edges(mesh.faces(faceind).edge_indices(edgeind)).bnode_indices(2)).p
    ELSE
       WRITE(*,*) 'Indexing error in get_face_bnode!'
       STOP
    END IF
  END FUNCTION get_face_bnode


  ! Result is positive if the given face is the positive faces of given edge.
  ! Result is negative if the given face is the negative faces of given edge.
  FUNCTION get_solid_face_sign(solidind, faceind, mesh) RESULT(res)
    INTEGER(kind=i4), INTENT(IN) :: solidind, faceind
    TYPE(mesh_container), INTENT(IN) :: mesh
    REAL (KIND=dp) :: res

    IF(solidind==mesh.solid_faces(mesh.solids(solidind).solid_face_indices(faceind)).solid_indices(1)) THEN
       res = 1.0_dp
    ELSE IF(solidind==mesh.solid_faces(mesh.solids(solidind).solid_face_indices(faceind)).solid_indices(2)) THEN
       res = -1.0_dp
    ELSE
       WRITE(*,*) 'Error evaluating solid face sign (this could imply corrupted mesh connectivity)!'
    END IF
  END FUNCTION get_solid_face_sign

  FUNCTION get_posit_solid_bnode(solidind, faceind, mesh) RESULT(res)
    INTEGER(kind=i4), INTENT(IN) :: solidind, faceind
    TYPE(mesh_container), INTENT(IN) :: mesh
    REAL (KIND=dp), DIMENSION(3) :: res

    res = mesh.nodes(mesh.solid_faces(mesh.solids(solidind).solid_face_indices(faceind)).bnode_indices(1)).p
  END FUNCTION get_posit_solid_bnode

  FUNCTION get_negat_solid_bnode(solidind, faceind, mesh) RESULT(res)
    INTEGER(kind=i4), INTENT(IN) :: solidind, faceind
    TYPE(mesh_container), INTENT(IN) :: mesh
    REAL (KIND=dp), DIMENSION(3) :: res

    res = mesh.nodes(mesh.solid_faces(mesh.solids(solidind).solid_face_indices(faceind)).bnode_indices(2)).p
  END FUNCTION get_negat_solid_bnode

  FUNCTION point_in_mesh(mesh, pt) RESULT(res)
    TYPE(mesh_container), INTENT(IN) :: mesh
    REAL (KIND=dp), DIMENSION(3), INTENT(IN) :: pt
    LOGICAL :: res
    INTEGER(kind=i4) :: n
    REAL (KIND=dp) :: d

    res = .FALSE.

    DO n=1, mesh.nfaces
       d = dotr(mesh.faces(n).n, mesh.nodes(mesh.faces(n).node_indices(1)).p)
       IF(dotr(pt, mesh.faces(n).n) > d) THEN
          RETURN
       END IF
    END DO

    res = .TRUE.
  END FUNCTION point_in_mesh

  SUBROUTINE export_mesh(filename, mesh)
    CHARACTER (LEN=*), INTENT(IN) :: filename
    TYPE(mesh_container), INTENT(IN) :: mesh
    INTEGER(kind=i4) :: fid = 10, iovar, i

    OPEN(fid, FILE=TRIM(filename), ACTION='WRITE', IOSTAT=iovar)
    IF(iovar>0) THEN
       WRITE(*,*) 'Could not open mesh file for export!'
       STOP
    END IF

    WRITE(fid,'(A,T9,I0)') 'version', meshver

    WRITE(fid,'(A)') '[nodes]'
    WRITE(fid,*) mesh.nnodes
    DO i=1,mesh.nnodes
       WRITE(fid,*) mesh.nodes(i).p
       WRITE(fid,*) mesh.nodes(i).parent_index
    END DO

    WRITE(fid,'(A)') '[faces]'
    WRITE(fid,*) mesh.nfaces
    DO i=1,mesh.nfaces
       WRITE(fid,*) mesh.faces(i).n
       WRITE(fid,*) mesh.faces(i).cp
       WRITE(fid,*) mesh.faces(i).s
       WRITE(fid,*) mesh.faces(i).m
       WRITE(fid,*) mesh.faces(i).node_indices
       WRITE(fid,*) mesh.faces(i).edge_indices
       WRITE(fid,*) mesh.faces(i).area
       WRITE(fid,*) mesh.faces(i).pd
       WRITE(fid,*) mesh.faces(i).id
       WRITE(fid,*) mesh.faces(i).parent_index
    END DO

    WRITE(fid,'(A)') '[lines]'
    WRITE(fid,*) mesh.nlines
    DO i=1,mesh.nlines
       WRITE(fid,*) mesh.lines(i).node_indices
       WRITE(fid,*) mesh.lines(i).id
    END DO

    WRITE(fid,'(A)') '[edges]'
    WRITE(fid,*) mesh.nedges
    DO i=1,mesh.nedges
       WRITE(fid,*) mesh.edges(i).node_indices
       WRITE(fid,*) mesh.edges(i).bnode_indices
       WRITE(fid,*) mesh.edges(i).face_indices
       WRITE(fid,*) mesh.edges(i).length
       WRITE(fid,*) mesh.edges(i).bnd
       WRITE(fid,*) mesh.edges(i).parent_index
       WRITE(fid,*) mesh.edges(i).couple_index
       WRITE(fid,*) SIZE(mesh.edges(i).child_indices,1)
       IF(SIZE(mesh.edges(i).child_indices,1)/=0) THEN
          WRITE(fid,*) mesh.edges(i).child_indices(:,:)
       END IF
    END DO

    WRITE(fid,'(A)') '[solids]'
    WRITE(fid,*) mesh.nsolids
    DO i=1,mesh.nsolids
       WRITE(fid,*) mesh.solids(i).node_indices
       WRITE(fid,*) mesh.solids(i).solid_face_indices
       WRITE(fid,*) mesh.solids(i).volume
       WRITE(fid,*) mesh.solids(i).id
    END DO

    WRITE(fid,'(A)') '[solid faces]'
    WRITE(fid,*) mesh.nsolid_faces
    DO i=1,mesh.nsolid_faces
       WRITE(fid,*) mesh.solid_faces(i).node_indices
       WRITE(fid,*) mesh.solid_faces(i).solid_indices
       WRITE(fid,*) mesh.solid_faces(i).bnode_indices
       WRITE(fid,*) mesh.solid_faces(i).face_index
       WRITE(fid,*) mesh.solid_faces(i).area
    END DO

    WRITE(fid,'(A)') '[end]'

    CLOSE(fid)
  END SUBROUTINE export_mesh

  FUNCTION generate_sphere(nseg, nrow) RESULT(mesh)
    TYPE(mesh_container) :: mesh
    INTEGER(kind=i4), INTENT(IN) :: nseg, nrow
    INTEGER(kind=i4) :: n, m, cn = 1, nf, nn, ne
    REAL (KIND=dp) :: theta, phi

    mesh.nnodes = 2 + (nrow - 1)*nseg
    mesh.nfaces = 2*nseg*(nrow - 1)
    ALLOCATE(mesh.nodes(mesh.nnodes), mesh.faces(mesh.nfaces))

    DO n=1,(nrow+1)
       theta = pi*(n-1)/nrow

       DO m=1,nseg
          phi = 2*pi*(m-1)/nseg

          mesh.nodes(cn).p = (/SIN(theta)*COS(phi), SIN(theta)*SIN(phi), COS(theta)/)
          cn = cn + 1

          IF(n==1 .OR. n==(nrow+1)) THEN
             EXIT
          END IF
       END DO
    END DO

    DO n=1,nseg
       IF(n==nseg) THEN
          mesh.faces(n).node_indices = (/1, nseg+1, 2/)
       ELSE
          mesh.faces(n).node_indices = (/1, n+1, n+2/)
       END IF
    END DO

    nf = nseg + 1
    nn = nf
    DO m=1,(nrow-2)
       DO n=1,nseg

          IF(n==nseg) THEN
             mesh.faces(nf).node_indices = (/nn+1, nn-nseg+2, nn-nseg+1/)
             mesh.faces(nf+1).node_indices = (/nn-nseg+2, nn-2*nseg+2, nn-nseg+1/)
          ELSE
             mesh.faces(nf).node_indices = (/nn+1, nn+2, nn+1-nseg/)
             mesh.faces(nf+1).node_indices = (/nn+2, nn+2-nseg, nn+1-nseg/)
          END IF
          nf = nf + 2
          nn = nn + 1
       END DO
    END DO

    nn = 1 + nseg*(nrow-2)
    DO n=1,nseg
       IF(n==nseg) THEN
          mesh.faces(nf).node_indices = (/mesh.nnodes, nn+1, nn+nseg/)
       ELSE
          mesh.faces(nf).node_indices = (/mesh.nnodes, nn+n+1, nn+n/)
       END IF
       nf = nf + 1
    END DO

    WRITE(*,*) 'Generated a sphere with ', mesh.nnodes, 'nodes and ', mesh.nfaces, ' faces'

 !   mesh.nedges = 3/2*mesh.nfaces
 !   ALLOCATE(mesh.edges(mesh.nedges))
  END FUNCTION generate_sphere

  SUBROUTINE offset_mesh(mesh, offset)
    use omp_lib
    TYPE(mesh_container), INTENT(INOUT) :: mesh
    REAL (KIND=dp), INTENT(IN) :: offset

    INTEGER(kind=i4) :: n, m
    REAL (KIND=dp), DIMENSION(mesh.nnodes,3) :: nor

    ! Compute the face normals.
    CALL compute_mesh_essentials(mesh)

    nor(:,:) = 0.0_dp
    
    DO n=1,mesh.nfaces
       !dir$ vector vectorlength(8)
       !dir$ vector multiple_gather_scatter_by_shuffles 
       !$omp simd reduction(+:nor)
       DO m=1,3
          nor(mesh.faces(n).node_indices(m),:) = nor(mesh.faces(n).node_indices(m),:) + mesh.faces(n).n*0.33333333333333333333333333333_dp
       END DO
    END DO

    DO n=1,mesh.nnodes
       mesh.nodes(n).p = mesh.nodes(n).p + nor(n,:)*offset
    END DO
  END SUBROUTINE offset_mesh

  SUBROUTINE save_msh(mesh, scale, filename)
    TYPE(mesh_container), INTENT(IN) :: mesh
    CHARACTER (LEN=*), INTENT(IN) :: filename
    REAL (KIND=dp), INTENT(IN) :: scale
    INTEGER(kind=i4) :: fid = 10, iovar, n

    OPEN(fid, FILE=TRIM(filename), ACTION='WRITE', IOSTAT=iovar)
    IF(iovar>0) THEN
       WRITE(*,*) 'Could not open output file' // filename // '!'
       STOP
    END IF

    WRITE(fid,'(A11)') '$MeshFormat'
    WRITE(fid,'(A7)') '2.1 0 8'
    WRITE(fid,'(A14)') '$EndMeshFormat'

    WRITE(fid,'(A6)') '$Nodes'
    WRITE(fid,'(I0)') mesh.nnodes

    DO n=1,mesh.nnodes
       WRITE(fid,'(I0,A1,3EN15.3)') n, ' ', mesh.nodes(n).p(1:3)/scale
    END DO

    WRITE(fid,'(A9)') '$EndNodes'

    WRITE(fid,'(A9)') '$Elements'
    WRITE(fid,'(I0)') mesh.nfaces

    DO n=1,mesh.nfaces
       WRITE(fid,'(I0,A11,I0,A1,I0,A1,I0)') n, ' 2 3 0 0 0 ', mesh.faces(n).node_indices(1),&
            ' ', mesh.faces(n).node_indices(2), ' ', mesh.faces(n).node_indices(3)
    END DO

    WRITE(fid,'(A12)') '$EndElements'
  END SUBROUTINE save_msh

  SUBROUTINE save_field_msh(filename, mesh, fn, ft, scale)
    CHARACTER (LEN=*), INTENT(IN) :: filename
    TYPE(mesh_container), INTENT(IN) :: mesh
    REAL (KIND=dp), DIMENSION(:), INTENT(IN) :: ft
    COMPLEX (KIND=dp), DIMENSION(:), INTENT(IN) :: fn
    REAL (KIND=dp), INTENT(IN) :: scale

    INTEGER(kind=i4) :: fid = 10, iovar, n, m, nsteps

    nsteps = 25

    OPEN(fid, FILE=TRIM(filename), ACTION='WRITE', IOSTAT=iovar)
    IF(iovar>0) THEN
       WRITE(*,*) 'Could not open output file' // filename // '!'
       STOP
    END IF

    WRITE(fid,'(A11)') '$MeshFormat'
    WRITE(fid,'(A7)') '2.1 0 8'
    WRITE(fid,'(A14)') '$EndMeshFormat'

    WRITE(fid,'(A6)') '$Nodes'
    WRITE(fid,'(I0)') mesh.nnodes

    DO n=1,mesh.nnodes
       WRITE(fid,'(I0,A1,3EN15.3)') n, ' ', mesh.nodes(n).p(1:3)/scale
    END DO

    WRITE(fid,'(A9)') '$EndNodes'

    WRITE(fid,'(A9)') '$Elements'
    WRITE(fid,'(I0)') mesh.nfaces

    DO n=1,mesh.nfaces
       WRITE(fid,'(I0,A11,I0,A1,I0,A1,I0)') n, ' 2 3 0 0 0 ', mesh.faces(n).node_indices(1),&
            ' ', mesh.faces(n).node_indices(2), ' ', mesh.faces(n).node_indices(3)
    END DO

    WRITE(fid,'(A12)') '$EndElements'

    ! Write total field amplitude.
    WRITE(fid,'(A12)') '$ElementData'
    WRITE(fid,'(I0)') 1
    WRITE(fid,'(A17)') '"|F|"'
    WRITE(fid,'(I0)') 0
    WRITE(fid,'(I0)') 3
    WRITE(fid,'(I0)') 0
    WRITE(fid,'(I0)') 1
    WRITE(fid,'(I0)') mesh.nfaces

    DO n=1,mesh.nfaces
       WRITE(fid,'(I0,A1,EN15.3)') n, ' ', SQRT(ft(n)**2 + ABS(fn(n))**2)
    END DO

    WRITE(fid,'(A15)') '$EndElementData'

    ! Write field normal component amplitude.
    WRITE(fid,'(A12)') '$ElementData'
    WRITE(fid,'(I0)') 1
    WRITE(fid,'(A17)') '"|F_n|"'
    WRITE(fid,'(I0)') 0
    WRITE(fid,'(I0)') 3
    WRITE(fid,'(I0)') 0
    WRITE(fid,'(I0)') 1
    WRITE(fid,'(I0)') mesh.nfaces

    DO n=1,mesh.nfaces
       WRITE(fid,'(I0,A1,EN15.3)') n, ' ', ABS(fn(n))
    END DO

    WRITE(fid,'(A15)') '$EndElementData'

    ! Write field normal component animation.
    DO m=1,nsteps
       WRITE(fid,'(A12)') '$ElementData'
       WRITE(fid,'(I0)') 1
       WRITE(fid,'(A17)') '"Re(F_n)"'
       WRITE(fid,'(I0)') 0
       WRITE(fid,'(I0)') 3
       WRITE(fid,'(I0)') m-1
       WRITE(fid,'(I0)') 1
       WRITE(fid,'(I0)') mesh.nfaces
       
       DO n=1,mesh.nfaces
          WRITE(fid,'(I0,A1,EN15.3)') n, ' ', REAL(fn(n)*EXP(-(0,1)*2*pi*REAL(m)/nsteps))
       END DO
       
       WRITE(fid,'(A15)') '$EndElementData'
    END DO

    ! Write field tangential component amplitude.
    WRITE(fid,'(A12)') '$ElementData'
    WRITE(fid,'(I0)') 1
    WRITE(fid,'(A17)') '"|F_t|"'
    WRITE(fid,'(I0)') 0
    WRITE(fid,'(I0)') 3
    WRITE(fid,'(I0)') 0
    WRITE(fid,'(I0)') 1
    WRITE(fid,'(I0)') mesh.nfaces

    DO n=1,mesh.nfaces
       WRITE(fid,'(I0,A1,EN15.3)') n, ' ', ft(n)
    END DO

    WRITE(fid,'(A15)') '$EndElementData'

    CLOSE(fid)

  END SUBROUTINE save_field_msh

  SUBROUTINE save_vector_fields_msh(filename, mesh, e, h, scale)
    CHARACTER (LEN=*), INTENT(IN) :: filename
    TYPE(mesh_container), INTENT(IN) :: mesh
    COMPLEX (KIND=dp), DIMENSION(:,:), INTENT(IN) :: e, h
    REAL (KIND=dp), INTENT(IN) :: scale

    INTEGER(kind=i4) :: fid = 10, iovar, n, nsteps, l
    COMPLEX (KIND=dp) :: fn

    nsteps = 25

    OPEN(fid, FILE=TRIM(filename), ACTION='WRITE', IOSTAT=iovar)
    IF(iovar>0) THEN
       WRITE(*,*) 'Could not open output file' // filename // '!'
       STOP
    END IF

    WRITE(fid,'(A11)') '$MeshFormat'
    WRITE(fid,'(A7)') '2.1 0 8'
    WRITE(fid,'(A14)') '$EndMeshFormat'

    WRITE(fid,'(A6)') '$Nodes'
    WRITE(fid,'(I0)') mesh.nnodes

    DO n=1,mesh.nnodes
       WRITE(fid,'(I0,A1,3EN15.3)') n, ' ', mesh.nodes(n).p(1:3)/scale
    END DO

    WRITE(fid,'(A9)') '$EndNodes'

    WRITE(fid,'(A9)') '$Elements'
    WRITE(fid,'(I0)') mesh.nfaces

    DO n=1,mesh.nfaces
       WRITE(fid,'(I0,A11,I0,A1,I0,A1,I0)') n, ' 2 3 0 0 0 ', mesh.faces(n).node_indices(1),&
            ' ', mesh.faces(n).node_indices(2), ' ', mesh.faces(n).node_indices(3)
    END DO

    WRITE(fid,'(A12)') '$EndElements'

    DO l=1,nsteps
       WRITE(fid,'(A12)') '$ElementData'
       WRITE(fid,'(I0)') 1
       WRITE(fid,'(A17)') '"Re(E)"'
       WRITE(fid,'(I0)') 0
       WRITE(fid,'(I0)') 3
       WRITE(fid,'(I0)') l-1
       WRITE(fid,'(I0)') 3
       WRITE(fid,'(I0)') mesh.nfaces

       DO n=1,mesh.nfaces
          WRITE(fid,'(I0,A1,3EN15.3)') n, ' ', REAL(e(:,n)*EXP(-(0,1)*2*pi*REAL(l)/nsteps))
       END DO
       
       WRITE(fid,'(A15)') '$EndElementData'
    END DO

    DO l=1,nsteps
       WRITE(fid,'(A12)') '$ElementData'
       WRITE(fid,'(I0)') 1
       WRITE(fid,'(A17)') '"Re(H)"'
       WRITE(fid,'(I0)') 0
       WRITE(fid,'(I0)') 3
       WRITE(fid,'(I0)') l-1
       WRITE(fid,'(I0)') 3
       WRITE(fid,'(I0)') mesh.nfaces

       DO n=1,mesh.nfaces
          WRITE(fid,'(I0,A1,3EN15.3)') n, ' ', REAL(h(:,n)*EXP(-(0,1)*2*pi*REAL(l)/nsteps))
       END DO
       
       WRITE(fid,'(A15)') '$EndElementData'
    END DO

    DO l=1,nsteps
       WRITE(fid,'(A12)') '$ElementData'
       WRITE(fid,'(I0)') 1
       WRITE(fid,'(A17)') '"Re(En)"'
       WRITE(fid,'(I0)') 0
       WRITE(fid,'(I0)') 3
       WRITE(fid,'(I0)') l-1
       WRITE(fid,'(I0)') 1
       WRITE(fid,'(I0)') mesh.nfaces

       DO n=1,mesh.nfaces
          fn = dotc(CMPLX(mesh.faces(n).n,KIND=dp), e(:,n))

          WRITE(fid,*) n, ' ', REAL(fn*EXP(-(0,1)*2*pi*REAL(l)/nsteps))
       END DO
       
       WRITE(fid,'(A15)') '$EndElementData'
    END DO

    DO l=1,nsteps
       WRITE(fid,'(A12)') '$ElementData'
       WRITE(fid,'(I0)') 1
       WRITE(fid,'(A17)') '"Re(Hn)"'
       WRITE(fid,'(I0)') 0
       WRITE(fid,'(I0)') 3
       WRITE(fid,'(I0)') l-1
       WRITE(fid,'(I0)') 1
       WRITE(fid,'(I0)') mesh.nfaces

       DO n=1,mesh.nfaces
          fn = dotc(CMPLX(mesh.faces(n).n,KIND=dp), h(:,n))

          WRITE(fid,*) n, ' ', REAL(fn*EXP(-(0,1)*2*pi*REAL(l)/nsteps))
       END DO
       
       WRITE(fid,'(A15)') '$EndElementData'
    END DO

    DO l=1,nsteps
       WRITE(fid,'(A12)') '$ElementData'
       WRITE(fid,'(I0)') 1
       WRITE(fid,'(A17)') '"|Re(E)|"'
       WRITE(fid,'(I0)') 0
       WRITE(fid,'(I0)') 3
       WRITE(fid,'(I0)') l-1
       WRITE(fid,'(I0)') 1
       WRITE(fid,'(I0)') mesh.nfaces

       DO n=1,mesh.nfaces
          WRITE(fid,*) n, ' ', normr(REAL(REAL(e(:,n)*EXP(-(0,1)*2*pi*REAL(l)/nsteps)),KIND=dp))
       END DO
       
       WRITE(fid,'(A15)') '$EndElementData'
    END DO

    DO l=1,nsteps
       WRITE(fid,'(A12)') '$ElementData'
       WRITE(fid,'(I0)') 1
       WRITE(fid,'(A17)') '"|Re(H)|"'
       WRITE(fid,'(I0)') 0
       WRITE(fid,'(I0)') 3
       WRITE(fid,'(I0)') l-1
       WRITE(fid,'(I0)') 1
       WRITE(fid,'(I0)') mesh.nfaces

       DO n=1,mesh.nfaces
          WRITE(fid,*) n, ' ', normr(REAL(REAL(h(:,n)*EXP(-(0,1)*2*pi*REAL(l)/nsteps)),KIND=dp))
       END DO
       
       WRITE(fid,'(A15)') '$EndElementData'
    END DO

    CLOSE(fid)

  END SUBROUTINE save_vector_fields_msh

  SUBROUTINE save_domain_vector_fields_msh(filename, mesh, pt, e, h, scale)
    CHARACTER (LEN=*), INTENT(IN) :: filename
    TYPE(mesh_container), INTENT(IN) :: mesh
    COMPLEX (KIND=dp), DIMENSION(:,:), INTENT(IN) :: e, h
    REAL (KIND=dp), INTENT(IN) :: scale
    REAL (KIND=dp), DIMENSION(:,:), INTENT(IN) :: pt

    INTEGER(kind=i4) :: fid = 10, iovar, n, nsteps, l, nnodes
    COMPLEX (KIND=dp) :: fn

    nsteps = 25

    nnodes = mesh.nnodes + SIZE(pt,2)

    OPEN(fid, FILE=TRIM(filename), ACTION='WRITE', IOSTAT=iovar)
    IF(iovar>0) THEN
       WRITE(*,*) 'Could not open output file' // filename // '!'
       STOP
    END IF

    WRITE(fid,'(A11)') '$MeshFormat'
    WRITE(fid,'(A7)') '2.1 0 8'
    WRITE(fid,'(A14)') '$EndMeshFormat'

    WRITE(fid,'(A6)') '$Nodes'
    WRITE(fid,'(I0)') nnodes

    DO n=1,mesh.nnodes
       WRITE(fid,'(I0,A1,3EN15.3)') n, ' ', mesh.nodes(n).p(1:3)/scale
    END DO

    DO n=1,SIZE(pt,2)
       WRITE(fid,'(I0,A1,3EN15.3)') mesh.nnodes + n, ' ', pt(:,n)/scale
    END DO

    WRITE(fid,'(A9)') '$EndNodes'

    WRITE(fid,'(A9)') '$Elements'
    WRITE(fid,'(I0)') mesh.nfaces+SIZE(pt,2)

    DO n=1,mesh.nfaces
       WRITE(fid,'(I0,A11,I0,A1,I0,A1,I0)') n, ' 2 3 0 0 0 ', mesh.faces(n).node_indices(1),&
            ' ', mesh.faces(n).node_indices(2), ' ', mesh.faces(n).node_indices(3)
    END DO

    DO n=1,SIZE(pt,2)
       WRITE(fid,'(I0,A12,I0)') n+mesh.nfaces, ' 15 3 0 0 0 ', mesh.nnodes+n
    END DO

    WRITE(fid,'(A12)') '$EndElements'

    DO l=1,nsteps
       WRITE(fid,'(A9)') '$NodeData'
       WRITE(fid,'(I0)') 1
       WRITE(fid,'(A7)') '"Re(E)"'
       WRITE(fid,'(I0)') 0
       WRITE(fid,'(I0)') 3
       WRITE(fid,'(I0)') l-1
       WRITE(fid,'(I0)') 3
       WRITE(fid,'(I0)') SIZE(pt,2)

       DO n=1,SIZE(pt,2)
          WRITE(fid,'(I0,A1,3EN15.3)') mesh.nnodes + n, ' ',&
               REAL(e(:,n)*EXP(-(0,1)*2*pi*REAL(l)/nsteps))
       END DO
       
       WRITE(fid,'(A12)') '$EndNodeData'
    END DO

    DO l=1,nsteps
       WRITE(fid,'(A9)') '$NodeData'
       WRITE(fid,'(I0)') 1
       WRITE(fid,'(A7)') '"Re(H)"'
       WRITE(fid,'(I0)') 0
       WRITE(fid,'(I0)') 3
       WRITE(fid,'(I0)') l-1
       WRITE(fid,'(I0)') 3
       WRITE(fid,'(I0)') SIZE(pt,2)

       DO n=1,SIZE(pt,2)
          WRITE(fid,'(I0,A1,3EN15.3)') mesh.nnodes + n, ' ',&
               REAL(h(:,n)*EXP(-(0,1)*2*pi*REAL(l)/nsteps))
       END DO
       
       WRITE(fid,'(A12)') '$EndNodeData'
    END DO

    CLOSE(fid)
  END SUBROUTINE save_domain_vector_fields_msh

  SUBROUTINE save_stream_fields_msh(filename, mesh, npts, pts, e, scale)
    CHARACTER (LEN=*), INTENT(IN) :: filename
    TYPE(mesh_container), INTENT(IN) :: mesh
    REAL (KIND=dp), DIMENSION(:,:,:), INTENT(IN) :: e
    REAL (KIND=dp), INTENT(IN) :: scale
    REAL (KIND=dp), DIMENSION(:,:,:,:), INTENT(IN) :: pts
    INTEGER(kind=i4), DIMENSION(:,:) :: npts

    INTEGER(kind=i4) :: fid = 10, iovar, n, l, m, nnodes, nlines, index, nlines2, index2
    COMPLEX (KIND=dp) :: fn

    nnodes = mesh.nnodes + SUM(npts)

    OPEN(fid, FILE=TRIM(filename), ACTION='WRITE', IOSTAT=iovar)
    IF(iovar>0) THEN
       WRITE(*,*) 'Could not open output file' // filename // '!'
       STOP
    END IF

    WRITE(fid,'(A11)') '$MeshFormat'
    WRITE(fid,'(A7)') '2.1 0 8'
    WRITE(fid,'(A14)') '$EndMeshFormat'

    WRITE(fid,'(A6)') '$Nodes'
    WRITE(fid,'(I0)') nnodes

    DO n=1,mesh.nnodes
       WRITE(fid,'(I0,A1,3EN15.3)') n, ' ', mesh.nodes(n).p(1:3)/scale
    END DO

    index = mesh.nnodes + 1

    DO n=1,SIZE(npts,2)
       DO m=1,SIZE(npts,1)
          DO l=1,npts(m,n)
             WRITE(fid,'(I0,A1,3EN15.3)') index, ' ', pts(:,l,m,n)/scale
             index = index + 1
          END DO
       END DO
    END DO

    WRITE(fid,'(A9)') '$EndNodes'

    nlines = 0

    DO n=1,SIZE(npts,2)
       DO m=1,SIZE(npts,1)
          IF(npts(m,n)>0) THEN
             nlines = nlines + npts(m,n)-1
          END IF
       END DO
    END DO

    WRITE(fid,'(A9)') '$Elements'
    WRITE(fid,'(I0)') mesh.nfaces+nlines

    DO n=1,mesh.nfaces
       WRITE(fid,'(I0,A11,I0,A1,I0,A1,I0)') n, ' 2 3 0 0 0 ', mesh.faces(n).node_indices(1),&
            ' ', mesh.faces(n).node_indices(2), ' ', mesh.faces(n).node_indices(3)
    END DO

    index = 1
    index2 = 1

    DO n=1,SIZE(npts,2)
       DO m=1,SIZE(npts,1)
          DO l=1,(npts(m,n)-1)
             WRITE(fid,'(I0,A11,I0,A1,I0)') mesh.nfaces + index2, ' 1 3 0 0 0 ', mesh.nnodes + index,&
                  ' ', mesh.nnodes + index + 1
             index = index + 1
             index2 = index2 + 1
          END DO

          IF(npts(m,n)>0) THEN
             index = index + 1
          END IF
       END DO
    END DO

    WRITE(fid,'(A12)') '$EndElements'

    nlines2 = 0

    DO l=1,SIZE(npts,2)
       nlines = 0

       DO m=1,SIZE(npts,1)
          IF(npts(m,l)>0) THEN
             nlines = nlines + npts(m,l)-1
          END IF
       END DO

       WRITE(fid,'(A12)') '$ElementData'
       WRITE(fid,'(I0)') 1
       WRITE(fid,'(A17)') '"E"'
       WRITE(fid,'(I0)') 0
       WRITE(fid,'(I0)') 3
       WRITE(fid,'(I0)') l-1
       WRITE(fid,'(I0)') 1
       WRITE(fid,'(I0)') nlines

       DO m=1,SIZE(npts,1)
          DO n=1,(npts(m,l)-1)
             nlines2 = nlines2 + 1
             WRITE(fid,*) mesh.nfaces + nlines2, ' ', e(n,m,l)
          END DO
       END DO
       
       WRITE(fid,'(A15)') '$EndElementData'
    END DO


    CLOSE(fid)
  END SUBROUTINE save_stream_fields_msh

  !$omp declare simd simdlen(8)
  FUNCTION dotc(v1, v2) RESULT(res)
    !dir$ attributes inline :: dotc
    use omp_lib
    COMPLEX (KIND=dp), DIMENSION(:), INTENT(IN) :: v1, v2
    COMPLEX (KIND=dp) :: res

    res = SUM(v1*v2)
  END FUNCTION dotc

  !$omp declare simd simdlen(8)
  PURE FUNCTION dotr(v1, v2) RESULT(res)
    !dir$ attributes inline :: dotr
    REAL (KIND=dp), DIMENSION(:), INTENT(IN) :: v1, v2
    REAL (KIND=dp) :: res

    res = SUM(v1*v2)
  END FUNCTION dotr


  FUNCTION getext(filename) RESULT(ext)
    !dir$ attributes inline :: getext
    CHARACTER (LEN=*), INTENT(IN) :: filename
    CHARACTER (LEN=3) :: ext
    INTEGER(kind=i4) :: n

    n = LEN_TRIM(filename)

    ext = filename((n-2):n)
  END FUNCTION getext
 
  !$omp declare simd simdlen(8)
  PURE FUNCTION normr(v) RESULT(res)
    !dir$ attributes inline :: normr
    REAL (KIND=dp), DIMENSION(:), INTENT(IN) :: v
    REAL (KIND=dp) :: res

    
    res = SQRT(SUM(v*v))
  END FUNCTION normr

  !$omp declare simd simdlen(8)
  FUNCTION crossr(v1, v2) RESULT(res)
    !dir$ attributes forceinline :: crossr
    REAL (KIND=dp), DIMENSION(4), INTENT(IN) :: v1, v2
    REAL (KIND=dp), DIMENSION(4) :: res

    res(1) = v1(2)*v2(3) - v1(3)*v2(2)
    res(2) = v1(3)*v2(1) - v1(1)*v2(3)
    res(3) = v1(1)*v2(2) - v1(2)*v2(1)
  END FUNCTION crossr

END MODULE gmsh_mesh
