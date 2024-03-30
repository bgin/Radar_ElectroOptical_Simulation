module tri_mod

    use panel_mod
    use base_geom_mod
    use math_mod
    use stl_mod

    implicit none
    
contains


    subroutine load_surface_tri(mesh_file, N_verts, N_panels, vertices, panels)
        ! Loads a surface mesh from a tri file. Only a body.
        ! Needs to be updated to automatically delete duplicate vertices.

        implicit none

        character(len=:),allocatable,intent(in) :: mesh_file
        integer,intent(out) :: N_verts, N_panels
        type(vertex),dimension(:),allocatable,intent(out) :: vertices
        type(panel),dimension(:),allocatable,intent(out) :: panels

        integer :: i, i1, i2, i3, N_words, N_duplicates, unit
        character(len=200) :: dummy_read
        logical :: more_than_three, on_space
        real,dimension(:,:),allocatable :: vertex_locs
        integer,dimension(:),allocatable :: new_ind

        ! Open mesh file
        open(newunit=unit, file=mesh_file)

            ! Get number of vertices and panels
            read(unit,*) N_verts, N_panels

            ! Determine number of elements per line
            read(unit,'(a)') dummy_read
            more_than_three = .false.

            ! Loop through each character
            on_space = .true.
            N_words = 0
            do i=1,200

                ! Check if we're on a space
                if (dummy_read(i:i) == ' ') then

                    on_space = .true.

                ! If we're not on a space but we were, then we've moved onto a word
                else if (on_space) then

                    N_words = N_words + 1
                    on_space = .false.

                    ! Check number of words found
                    if (N_words > 3) then
                        more_than_three = .true.
                        exit
                    end if
                end if

            end do

            ! Go back a line
            backspace(unit)

            ! Read in and initialize vertices
            allocate(vertex_locs(3,N_verts))
            do i=1,N_verts

                ! Get coords
                if (more_than_three) then
                    read(unit,*) vertex_locs(1,i), vertex_locs(2,i), vertex_locs(3,i), dummy_read
                else
                    read(unit,*) vertex_locs(1,i), vertex_locs(2,i), vertex_locs(3,i)
                end if

            end do

            ! Find duplicates
            call collapse_duplicate_vertices(vertex_locs, vertices, N_verts, N_duplicates, new_ind)

            ! Allocate panel storage
            allocate(panels(N_panels))

            ! Read in and initialize panels
            do i=1,N_panels

                ! Get vertex indices
                read(unit,*) i1, i2, i3

                ! Initialize
                call panels(i)%init(vertices(new_ind(i1)), vertices(new_ind(i2)), vertices(new_ind(i3)), i)

            end do

        close(unit)

    end subroutine load_surface_tri

    
end module tri_mod