! Class for modeling wake meshes
module wake_mesh_mod

    use json_mod
    use json_xtnsn_mod
    use linked_list_mod
    use helpers_mod
    use base_geom_mod
    use panel_mod
    use math_mod
    use flow_mod
    use vtk_mod
    use wake_strip_mod
    use mesh_mod

    implicit none


    type, extends(mesh) ::  wake_mesh

        type(wake_strip),allocatable,dimension(:) :: strips
        integer :: N_strips = 0
        integer :: N_max_strip_verts = 0
        integer :: N_max_strip_panels = 0

        contains

            procedure :: init => wake_mesh_init
            procedure :: init_strips => wake_mesh_init_strips
            procedure :: write_strips => wake_mesh_write_strips

    end type wake_mesh


contains


    subroutine wake_mesh_init(this, body_edges, body_verts, freestream, asym_flow, mirror_plane, N_panels_streamwise, &
                              trefftz_dist, body_mirrored, initial_panel_order, N_body_panels)
        ! Initializes the wake mesh

        implicit none

        class(wake_mesh),intent(inout),target :: this
        type(edge),allocatable,dimension(:),intent(in) :: body_edges
        type(vertex),allocatable,dimension(:),intent(inout) :: body_verts
        type(flow),intent(in) :: freestream
        logical,intent(in) :: asym_flow
        integer,intent(in) :: mirror_plane, N_panels_streamwise, initial_panel_order, N_body_panels
        real,intent(in) :: trefftz_dist
        logical,intent(in) :: body_mirrored

        if (verbose) write(*,'(a ES10.4 a)',advance='no') "     Initializing wake with a Trefftz distance of ", trefftz_dist, "..."

        ! Set whether the wake will be mirrored
        this%mirrored = body_mirrored .and. .not. asym_flow
        this%mirror_plane = mirror_plane

        ! Initialize strips
        call this%init_strips(body_edges, body_verts, freestream, asym_flow, mirror_plane, N_panels_streamwise, &
                              trefftz_dist, initial_panel_order, N_body_panels)

        if (verbose) write(*,'(a i7 a i7 a i7 a)') "Done. Created ", this%N_verts, " wake vertices and ", &
                                              this%N_panels, " wake panels distributed between ", &
                                              this%N_strips, " strips."

    end subroutine wake_mesh_init


    subroutine wake_mesh_init_strips(this, body_edges, body_verts, freestream, asym_flow, mirror_plane, &
                              N_panels_streamwise, trefftz_dist, initial_panel_order, N_body_panels)
        ! Creates the strips for this wake

        implicit none

        class(wake_mesh),intent(inout),target :: this
        type(edge),allocatable,dimension(:),intent(in) :: body_edges
        type(vertex),allocatable,dimension(:),intent(inout) :: body_verts
        type(flow),intent(in) :: freestream
        logical,intent(in) :: asym_flow
        integer,intent(in) :: mirror_plane, N_panels_streamwise, initial_panel_order, N_body_panels
        real,intent(in) :: trefftz_dist

        integer :: i, i_strip, i_start_edge
        type(list) :: wake_shedding_edges

        ! Loop through edges to find which ones shed a wake and how many there are
        this%N_strips = 0
        do i=1,size(body_edges)

            ! Check if it sheds a wake
            if (body_edges(i)%sheds_wake) then
                this%N_strips = this%N_strips + 1
                call wake_shedding_edges%append(i)

                ! If the flow is asymmetric, then wake-shedding edges not on the mirror plane will also be used to generate a wake strip
                if (asym_flow .and. .not. body_edges(i)%on_mirror_plane) then
                    this%N_strips = this%N_strips + 1
                end if
            end if

        end do

        ! Allocate strip storage
        allocate(this%strips(this%N_strips))
    
        ! Initialize strips
        i = 0
        i_strip = 0
        do while (i_strip < this%N_strips)

            ! Get starting edge index
            i = i + 1
            call wake_shedding_edges%get(i, i_start_edge)

            ! Initialize strip
            i_strip = i_strip + 1
            call this%strips(i_strip)%init(freestream, body_edges(i_start_edge), .false., mirror_plane, &
                                           N_panels_streamwise, trefftz_dist, body_verts, this%mirrored, &
                                           initial_panel_order, N_body_panels)

            ! Check if we need to create a mirror strip
            if (asym_flow .and. .not. body_edges(i_start_edge)%on_mirror_plane) then
                i_strip = i_strip + 1
                call this%strips(i_strip)%init(freestream, body_edges(i_start_edge), .true., mirror_plane, &
                                               N_panels_streamwise, trefftz_dist, body_verts, this%mirrored, &
                                               initial_panel_order, N_body_panels)
            end if
        end do

        ! Find out the maximum number of vertices and panels between the strips and totals
        this%N_verts = 0
        this%N_panels = 0
        do i=1,this%N_strips

            ! Find maxima
            this%N_max_strip_panels = max(this%N_max_strip_panels, this%strips(i)%N_panels)
            this%N_max_strip_verts = max(this%N_max_strip_verts, this%strips(i)%N_verts)

            ! Sum totals
            this%N_verts = this%N_verts + this%strips(i)%N_verts
            this%N_panels = this%N_panels + this%strips(i)%N_panels
        end do
        
    end subroutine wake_mesh_init_strips


    subroutine wake_mesh_write_strips(this, wake_file, exported, mu)
        ! Writes the wake strips out to file

        implicit none
        
        class(wake_mesh),intent(in) :: this
        character(len=:),allocatable,intent(in) :: wake_file
        logical,intent(out) :: exported
        real,dimension(:),allocatable,intent(in),optional :: mu

        type(vtk_out) :: wake_vtk
        integer :: i, j, k, N_verts, N_panels, shift
        real,dimension(:),allocatable :: mu_on_wake
        real,dimension(:,:),allocatable :: verts

        ! Clear old file
        call delete_file(wake_file)

        if (this%N_strips > 0) then

            ! Get total number of vertices and panels
            N_verts = 0
            N_panels = 0
            do i=1,this%N_strips
                N_verts = N_verts + this%strips(i)%N_verts
                N_panels = N_panels + this%strips(i)%N_panels
            end do

            ! Get all vertices
            allocate(verts(3,N_verts))
            i = 0
            do k=1,this%N_strips
                do j=1,this%strips(k)%N_verts
                    i = i + 1
                    verts(:,i) = this%strips(k)%vertices(j)%loc
                end do
            end do

            ! Initialize and write out vertices
            call wake_vtk%begin(wake_file)
            call wake_vtk%write_points(verts)

            ! Write out panels
            shift = 0
            do k=1,this%N_strips
                call wake_vtk%write_panels(this%strips(k)%panels, mirror=.false., &
                                           vertex_index_shift=shift, N_total_panels=N_panels)
                shift = shift + this%strips(k)%N_verts
            end do

            if (present(mu)) then

                ! Calculate doublet strengths
                allocate(mu_on_wake(N_verts))
                i = 0
                do k=1,this%N_strips
                    do j=1,this%strips(k)%N_verts
                        i = i + 1
                        mu_on_wake(i) = mu(this%strips(k)%vertices(j)%top_parent) - mu(this%strips(k)%vertices(j)%bot_parent)
                    end do
                end do

                ! Write doublet strengths
                call wake_vtk%write_point_scalars(mu_on_wake, "mu")
            end if

            ! Finish up
            call wake_vtk%finish()
            exported = .true.

        else
            exported = .false.
        end if
        
    end subroutine wake_mesh_write_strips


end module wake_mesh_mod