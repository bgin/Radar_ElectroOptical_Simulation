module stl_mod

    use panel_mod
    use base_geom_mod
    use math_mod

    implicit none
    
contains

    subroutine load_surface_stl(mesh_file, N_verts, N_panels, vertices, panels)
        ! Loads a surface mesh from an stl file

        implicit none

        character(len=:),allocatable,intent(in) :: mesh_file
        integer,intent(out) :: N_verts, N_panels
        type(vertex),dimension(:),allocatable,intent(out) :: vertices
        type(panel),dimension(:),allocatable,intent(out) :: panels

        character(len=200) :: dummy_read
        integer :: stat, i, i1, i2, i3, N_duplicates, i_panel, i_vert, unit
        integer,dimension(:,:),allocatable :: panel_vertex_indices
        real,dimension(:,:),allocatable :: vertex_locs
        integer,dimension(:),allocatable :: new_ind

        ! Open mesh file
        open(newunit=unit, file=mesh_file)

            ! Skip header
            read(unit,*)

            ! Figure out how many panels are in the mesh
            N_panels = 0
            do

                ! Read line
                read(unit,*,iostat=stat) dummy_read

                ! Check status
                if (stat == 0) then

                    ! If not at the end of the file, check for "facet", indicating a new panel
                    if (dummy_read == 'facet') then
                        N_panels = N_panels + 1
                    end if

                else
                    ! At end of file
                    exit
                end if

            end do

        close(unit)

        ! Allocate storage
        N_verts = N_panels*3
        allocate(panel_vertex_indices(3,N_panels))
        allocate(vertex_locs(3,N_verts))

        ! Reopen file
        open(newunit=unit, file=mesh_file)

            ! Skip header
            read(unit,*)

            ! Read in vertex locations
            do i=1,N_panels

                ! Skip normal and outer loop
                read(unit,*)
                read(unit,*)

                ! Get vertex locations
                read(unit,*) dummy_read, vertex_locs(1,i*3-2), vertex_locs(2,i*3-2), vertex_locs(3,i*3-2)
                read(unit,*) dummy_read, vertex_locs(1,i*3-1), vertex_locs(2,i*3-1), vertex_locs(3,i*3-1)
                read(unit,*) dummy_read, vertex_locs(1,i*3), vertex_locs(2,i*3), vertex_locs(3,i*3)

                ! Skip end loop and end facet
                read(unit,*)
                read(unit,*)

            end do

        close(unit)

        ! Find duplicates
        call collapse_duplicate_vertices(vertex_locs, vertices, N_verts, N_duplicates, new_ind)

        ! Get panel vertex indices
        do i=1,N_verts+N_duplicates

            ! Get index of panel and the index of the vertex in that panel
            i_panel = (i-1)/3+1
            i_vert = mod(i-1, 3)+1

            ! Point panel to non-duplicate vertex
            panel_vertex_indices(i_vert, i_panel) = new_ind(i)

        end do

        ! Initialize panel objects
        allocate(panels(N_panels))
        do i=1,N_panels

            ! Get vertex indices
            i1 = panel_vertex_indices(1,i)
            i2 = panel_vertex_indices(2,i)
            i3 = panel_vertex_indices(3,i)

            ! Initialize
            call panels(i)%init(vertices(i1), vertices(i2), vertices(i3), i)

        end do

    end subroutine load_surface_stl


    subroutine collapse_duplicate_vertices(vertex_locs, vertices, N_verts, N_duplicates, new_ind)
        ! Takes in an array of vertex locations, collapses duplicates, and provides a list of indices pointing to the new (unique) vertices

        implicit none

        real,dimension(:,:),allocatable,intent(in) :: vertex_locs
        type(vertex),dimension(:),allocatable,intent(out) :: vertices
        integer,intent(out) :: N_verts, N_duplicates
        integer,dimension(:),allocatable,intent(out) :: new_ind

        integer,dimension(:),allocatable :: duplicate_of
        logical,dimension(:),allocatable :: is_duplicate
        integer :: i, j, i_unique

        ! Locate duplicate vertices
        N_verts = size(vertex_locs)/3
        allocate(is_duplicate(N_verts), source=.false.)
        allocate(duplicate_of(N_verts))
        do i=1,N_verts
            duplicate_of(i) = i
        end do

        ! Loop through each vertex and try to find any which are the same
        !$OMP parallel do private(j) schedule(dynamic)
        do i=1,N_verts

            ! Check we don't already know this is a duplicate (this is always false for the first vertex)
            if (.not. is_duplicate(i)) then

                ! Loop through possible duplicates (don't need to check itself or any previous vertices)
                do j=i+1,N_verts

                    ! Check we don't already know this is a duplicate of another vertex
                    if (.not. is_duplicate(j)) then

                        ! Check if the vertices are the same
                        if (dist(vertex_locs(:,i), vertex_locs(:,j)) < 1.e-12) then

                            ! Mark duplicate
                            !$OMP critical
                            is_duplicate(j) = .true.
                            duplicate_of(j) = i
                            !$OMP end critical

                        end if
                    end if
                end do
            end if

        end do

        ! Collapse duplicates
        do i=1,N_verts

            ! Check if this one is a duplicate
            if (is_duplicate(i)) then

                ! Start at this vertex
                i_unique = i

                ! If it is not a duplicate of itself, move to the duplicate
                do while (.not. i_unique == duplicate_of(i_unique))
                    i_unique = duplicate_of(i_unique)
                end do

                ! Assign to new_ind
                duplicate_of(i) = i_unique

            end if

        end do

        ! Allocate new indices
        allocate(new_ind(N_verts))

        ! Loop through to determine the new indices and count up the number of duplicates
        N_duplicates = 0
        do i=1,N_verts

            ! Check if this vertex is a duplicate
            if (is_duplicate(i)) then

                ! The new index of a duplicate will be the new index of the unique vertex
                new_ind(i) = new_ind(duplicate_of(i))

                ! Update number of duplicates
                N_duplicates = N_duplicates + 1

            else

                ! If it is not a duplicate, then we simply have to shift its index down to account for vertices skipped
                new_ind(i) = i - N_duplicates

            end if
        end do

        ! Determine number of non-duplicate vertices
        N_verts = N_verts - N_duplicates

        ! Initialize vertex objects, skipping duplicates
        allocate(vertices(N_verts))
        do i=1,N_verts+N_duplicates

            ! If this vertex is not a duplicate, initialize its object
            if (.not. is_duplicate(i)) then

                ! Initialize
                j = new_ind(i)
                call vertices(j)%init(vertex_locs(:,i), j)

            end if

        end do
        
    end subroutine collapse_duplicate_vertices
    

end module stl_mod