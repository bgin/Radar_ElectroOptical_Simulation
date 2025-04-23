
!MIT License

!Copyright (c) 2021 USU Aero Lab

!Permission is hereby granted, free of charge, to any person obtaining a copy
!of this software and associated documentation files (the "Software"), to deal
!in the Software without restriction, including without limitation the rights
!to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
!copies of the Software, and to permit persons to whom the Software is
!furnished to do so, subject to the following conditions:

!The above copyright notice and this permission notice shall be included in all
!copies or substantial portions of the Software.

!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!SOFTWARE.

! A basic mesh type consisting of a collection of vertices and panels
module mesh_mod
     ! ************Slightly modified by Bernard Gingold, 30/03/2024, 10:13AM**************
    use mod_kinds, only : i4,sp
    use base_geom_mod
    use panel_mod
    use flow_mod

    implicit none

    type mesh

        integer(kind=i4):: N_verts, N_panels = 0
        type(vertex),allocatable,dimension(:) :: vertices
        type(panel),allocatable,dimension(:) :: panels
        logical :: mirrored = .false. ! Whether the mesh is to be mirrored about any planes
        integer(kind=i4):: mirror_plane ! Index of the plane across which the mesh is mirrored (1: yz, 2: xz, 3: xy); this is the index of the normal to that plane

        contains

            procedure :: has_zero_area => mesh_has_zero_area
            procedure :: get_indices_to_panel_vertices => mesh_get_indices_to_panel_vertices
            procedure :: allocate_new_vertices => mesh_allocate_new_vertices
            procedure :: get_verts_in_dod_of_point => mesh_get_verts_in_dod_of_point
            procedure :: get_panel_dod_info_for_point => mesh_get_panel_dod_info_for_point

    end type mesh

contains


    function mesh_has_zero_area(this, i1, i2, i3) result(has)
        ! Checks whether the panel to be defined by the three given vertices will have zero area

        implicit none

        class(mesh),intent(in) :: this
        integer(kind=i4),intent(in) :: i1, i2, i3
        logical :: has

        ! Check for zero area
        has = norm2(cross(this%vertices(i3)%loc-this%vertices(i2)%loc, this%vertices(i2)%loc-this%vertices(i1)%loc)) < 1.e-12

    end function mesh_has_zero_area


    subroutine mesh_get_indices_to_panel_vertices(this, i_vertices)
        ! Returns of the list of indices which point to the vertices of each panel

        implicit none

        class(mesh),intent(in) :: this
        integer(kind=i4),dimension(:,:),allocatable,intent(out) :: i_vertices

        integer(kind=i4):: i, j

        ! Allocate space
        allocate(i_vertices(8,this%N_panels))

        ! Get vertex indices for each panel since we will lose this information as soon as this%vertices is real(kind=sp) :: located
        do i=1,this%N_panels
            do j=1,this%panels(i)%N

                ! Get vertex indices
                i_vertices(j,i) = this%panels(i)%get_vertex_index(j)
                
            end do
        end do
        
    end subroutine mesh_get_indices_to_panel_vertices


    subroutine mesh_allocate_new_vertices(this, N_new_verts)
        ! Adds the specified number of vertex objects to the end of the wake mesh's vertex array.
        ! Handles moving panel pointers to the new allocation of previously-existing vertices.

        implicit none

        class(mesh),intent(inout),target :: this
        integer(kind=i4),intent(in) :: N_new_verts
        
        type(vertex),dimension(:),allocatable :: temp_vertices
        integer(kind=i4):: i, j
        integer(kind=i4),dimension(:,:),allocatable :: i_vertices

        ! Get panel vertex indices
        call this%get_indices_to_panel_vertices(i_vertices)

        ! Allocate more space
        allocate(temp_vertices(this%N_verts + N_new_verts))

        ! Copy vertices
        temp_vertices(1:this%N_verts) = this%vertices

        ! Move allocation
        call move_alloc(temp_vertices, this%vertices)

        ! Fix vertex pointers in panel objects (necessary because this%vertices got real(kind=sp) :: located)
        do i=1,this%N_panels
            do j=1,this%panels(i)%N

                ! Fix vertex pointers
                this%panels(i)%vertices(j)%ptr => this%vertices(i_vertices(j,i))

            end do
        end do

        ! Update number of vertices
        this%N_verts = this%N_verts + N_new_verts
        
    end subroutine mesh_allocate_new_vertices


    function mesh_get_verts_in_dod_of_point(this, point, freestream, mirror_points) result(verts_in_dod)
        ! Returns an array telling which vertices in the mesh belong to the DoD of the given point

        implicit none
        
        class(mesh),intent(in) :: this
        real(kind=sp) :: ,dimension(3),intent(in) :: point
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_points

        logical,dimension(this%N_verts) :: verts_in_dod

        integer(kind=i4):: i

        ! Loop through vertices
        do i=1,this%N_verts

            ! Check DoD
            if (mirror_points) then
                verts_in_dod(i) = freestream%point_in_dod(mirror_across_plane(this%vertices(i)%loc, this%mirror_plane), point)
            else
                verts_in_dod(i) = freestream%point_in_dod(this%vertices(i)%loc, point)
            end if
        end do
    
    end function mesh_get_verts_in_dod_of_point


    function mesh_get_panel_dod_info_for_point(this, point, freestream, verts_in_dod, mirror_panels) result(dod_info)
        ! Returns an array describing how the panels in the mesh fall wrt the DoD of the given point

        implicit none
        
        class(mesh),intent(in) :: this
        real(kind=sp) :: ,dimension(3),intent(in) :: point
        type(flow),intent(in) :: freestream
        logical,dimension(:),allocatable,intent(in) :: verts_in_dod
        logical,intent(in) :: mirror_panels

        type(dod),dimension(this%N_panels) :: dod_info

        integer(kind=i4):: i

        ! Loop through panels
        do i=1,this%N_panels
            dod_info(i) = this%panels(i)%check_dod(point, freestream, verts_in_dod, mirror_panels, this%mirror_plane)
        end do
        
    end function mesh_get_panel_dod_info_for_point
    
end module mesh_mod
