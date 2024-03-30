! A type for defining closed bodies to be analyzed by a linear panel method
module surface_mesh_mod

    use json_mod
    use json_xtnsn_mod
    use vtk_mod
    use stl_mod
    use tri_mod
    use base_geom_mod
    use panel_mod
    use mesh_mod
    use flow_mod
    use math_mod
    use wake_mesh_mod
    use sort_mod
    use helpers_mod

    implicit none


    type, extends(mesh) :: surface_mesh

        integer :: N_cp, N_edges ! as in, not midpoints
        integer :: N_subinc, N_supinc
        type(edge),allocatable,dimension(:) :: edges
        type(wake_mesh) :: wake
        real :: C_wake_shedding_angle, trefftz_distance, C_min_panel_angle, C_max_cont_angle
        real,dimension(:),allocatable :: CG
        integer :: N_wake_panels_streamwise
        logical :: wake_present, append_wake
        type(control_point),dimension(:),allocatable :: cp, cp_mir ! Control points
        real,dimension(:),allocatable :: R_cp ! System residuals at each control point
        real,dimension(:),allocatable :: Phi_u ! Total potential on outer surface
        real,dimension(:),allocatable :: C_p_pg, C_p_lai, C_p_kt ! Corrected surface pressure coefficients
        real,dimension(:),allocatable :: C_p_inc, C_p_ise, C_p_2nd, C_p_sln, C_p_lin ! Surface pressure coefficients
        real,dimension(:,:),allocatable :: V_cells, V_cells_inner, dC_f ! Surface velocities and pressure forces
        real :: control_point_offset
        logical :: asym_flow ! Whether the flow is asymmetric about the mirror plane
        logical :: found_wake_edges
        real,dimension(:),allocatable :: mu, sigma ! Singularity strengths
        real :: S_ref, l_ref ! Reference parameters
        integer,dimension(:),allocatable :: vertex_ordering
        integer :: initial_panel_order ! Distribution order for the panels (initially)
        character(len=:),allocatable :: singularity_order

        contains

            ! Basic initialization
            procedure :: init => surface_mesh_init
            procedure :: load_mesh_file => surface_mesh_load_mesh_file
            procedure :: locate_adjacent_panels => surface_mesh_locate_adjacent_panels
            procedure :: store_adjacent_vertices => surface_mesh_store_adjacent_vertices
            procedure :: check_panels_adjacent => surface_mesh_check_panels_adjacent
            procedure :: calc_vertex_geometry => surface_mesh_calc_vertex_geometry

            ! Loading settings
            procedure :: parse_singularity_settings => surface_mesh_parse_singularity_settings
            procedure :: parse_mirror_settings => surface_mesh_parse_mirror_settings
            procedure :: parse_wake_settings => surface_mesh_parse_wake_settings

            ! Initialization based on flow properties
            procedure :: init_with_flow => surface_mesh_init_with_flow
            procedure :: init_panels_with_flow => surface_mesh_init_panels_with_flow
            procedure :: characterize_edges => surface_mesh_characterize_edges
            procedure :: set_needed_vertex_clones => surface_mesh_set_needed_vertex_clones

            ! Cloning vertices
            procedure :: find_next_wake_edge =>surface_mesh_find_next_wake_edge
            procedure :: clone_vertices => surface_mesh_clone_vertices
            procedure :: init_vertex_clone => surface_mesh_init_vertex_clone

            ! Domains of dependence
            procedure :: calc_point_dod => surface_mesh_calc_point_dod
            procedure :: is_convex_at_vertex => surface_mesh_is_convex_at_vertex

            ! Helpers
            procedure :: find_vertices_on_mirror => surface_mesh_find_vertices_on_mirror

            ! Getters
            procedure :: get_avg_characteristic_panel_length => surface_mesh_get_avg_characteristic_panel_length

            ! Wake stuff
            procedure :: init_wake => surface_mesh_init_wake
            procedure :: update_supersonic_trefftz_distance => surface_mesh_update_supersonic_trefftz_distance
            procedure :: update_subsonic_trefftz_distance => surface_mesh_update_subsonic_trefftz_distance

            ! Control point locations
            procedure :: get_cp_locs_vertex_based_interior => surface_mesh_get_cp_locs_vertex_based_interior
            procedure :: get_cp_locs_centroid_based => surface_mesh_get_cp_locs_centroid_based
            procedure :: get_clone_control_point_dir => surface_mesh_get_clone_control_point_dir

            ! Control point placement
            procedure :: place_internal_vertex_control_points => surface_mesh_place_internal_vertex_control_points
            procedure :: place_centroid_control_points => surface_mesh_place_centroid_control_points
            procedure :: place_sparse_centroid_control_points => surface_mesh_place_sparse_centroid_control_points
            procedure :: place_centroid_vertex_avg_control_points => surface_mesh_place_centroid_vertex_avg_control_points

            ! Control point checks
            procedure :: control_point_outside_mesh => surface_mesh_control_point_outside_mesh

            ! Post-processing
            procedure :: get_induced_potentials_at_point => surface_mesh_get_induced_potentials_at_point
            procedure :: get_induced_velocities_at_point => surface_mesh_get_induced_velocities_at_point
            procedure :: output_results => surface_mesh_output_results
            procedure :: write_body => surface_mesh_write_body
            procedure :: write_body_mirror => surface_mesh_write_body_mirror
            procedure :: write_control_points => surface_mesh_write_control_points

    end type surface_mesh
    

contains


    subroutine surface_mesh_init(this, settings)

        implicit none

        class(surface_mesh),intent(inout) :: this
        type(json_value),pointer,intent(in) :: settings

        character(len=:),allocatable :: mesh_file
        logical :: found

        ! Get mesh filename
        call json_get(settings, 'file', mesh_file)
        mesh_file = trim(mesh_file)

        ! Get singularity settings
        call this%parse_singularity_settings(settings)

        ! Load mesh from file
        call this%load_mesh_file(mesh_file)

        ! Determine mirroring
        call this%parse_mirror_settings(settings)

        ! Load wake settings
        call this%parse_wake_settings(settings)

        ! Store references
        call json_xtnsn_get(settings, 'reference.area', this%S_ref, 1.)
        call json_xtnsn_get(settings, 'reference.length', this%l_ref, 1.)
        call json_get(settings, 'reference.CG', this%CG, found)
        if (.not. found) then
            allocate(this%CG(3), source=0.)
        end if

        ! Locate which vertices are on the mirror plane
        if (this%mirrored) call this%find_vertices_on_mirror()

        ! Locate adjacent panels
        call this%locate_adjacent_panels()

        ! Calculate vertex geometries
        call this%calc_vertex_geometry()

    end subroutine surface_mesh_init


    subroutine surface_mesh_parse_singularity_settings(this, settings)
        ! Parses the settings regarding singularity distributions from the input

        implicit none
        
        class(surface_mesh), intent(inout) :: this
        type(json_value),pointer,intent(in) :: settings

        real :: discont_angle

        ! Set singularity orders
        call json_xtnsn_get(settings, 'singularity_order', this%singularity_order, default_value='lower')
        select case (this%singularity_order)

        ! First order
        case ('lower')
            if (verbose) write(*,*) "    User has selected linear-doublet-constant-source panels."
            this%initial_panel_order = 1
            p_refine = .false.

        case ('higher')
            if (verbose) write(*,*) "    User has selected quadratic-doublet-linear-source panels."
            this%initial_panel_order = 2
            p_refine = .false.

        case ('adaptive')
            if (verbose) write(*,*) "    User has selected adaptive singularity distributions."
            this%initial_panel_order = 1
            p_refine = .true.

        case default
            write(*,*) "!!! ", this%singularity_order, " is not a valid singularity order. Defaulting to 'lower'."
            this%singularity_order = 'lower'
            this%initial_panel_order = 1
            p_refine = .false.

        end select

        ! Store cosine of max continuity angle
        call json_xtnsn_get(settings, 'max_continuity_angle', discont_angle, default_value=5.)
        this%C_max_cont_angle = cos(pi*discont_angle/180.)

        ! Get how sigma distributions should be calculated
        call json_xtnsn_get(settings, 'force_sigma_match', force_sigma_match, default_value=.true.)
        if (this%singularity_order=="higher" .and. verbose) then
            if (force_sigma_match) then
                write(*,*) "    User has selected matched source distributions."
            else
                write(*,*) "    User has selected unmatched source distributions."
            end if
        end if
    
    end subroutine surface_mesh_parse_singularity_settings


    subroutine surface_mesh_load_mesh_file(this, mesh_file)
        ! Loads the mesh from file

        implicit none
        
        class(surface_mesh),intent(inout) :: this
        character(len=:),allocatable,intent(in) :: mesh_file

        logical :: file_exists
        integer :: loc
        character(len=:),allocatable :: extension

        ! Check mesh file exists
        inquire(file=mesh_file, exist=file_exists)
        if (.not. file_exists) then
            write(*,*) "!!! Mesh file ", mesh_file, " does not exist. Quitting..."
            stop
        end if

        ! Determine the type of mesh file
        loc = index(mesh_file, '.')
        extension = mesh_file(loc:len(mesh_file))

        ! Load mesh file
        if (verbose) write(*,'(a, a, a)',advance='no') "     Reading surface mesh in from file ", mesh_file, "..."

        select case (extension)

        case ('.vtk')
            call load_surface_vtk(mesh_file, this%N_verts, this%N_panels, this%vertices, this%panels)
            
        case (".stl")
            call load_surface_stl(mesh_file, this%N_verts, this%N_panels, this%vertices, this%panels)

        case ('.tri')
            call load_surface_tri(mesh_file, this%N_verts, this%N_panels, this%vertices, this%panels)
            
        case default
            write(*,*) "MachLine cannot read ", extension, " type mesh files. Quitting..."
            stop
            
        end select
        if (verbose) write(*,*) "Done."

        ! Display mesh info
        if (verbose) write(*,'(a, i7, a, i7, a)') "     Surface mesh has ", this%N_verts, " vertices and ", &
                                                  this%N_panels, " panels."
        
    end subroutine surface_mesh_load_mesh_file


    subroutine surface_mesh_parse_mirror_settings(this, settings)

        implicit none
        
        class(surface_mesh),intent(inout) :: this
        type(json_value),pointer,intent(in) :: settings

        character(len=:),allocatable :: mirror_plane

        ! Get mirroring
        call json_xtnsn_get(settings, 'mirror_about', mirror_plane, "none")
        this%mirrored = .true.
        select case (mirror_plane)
        case ("xy")
            this%mirror_plane = 3
            if (verbose) write(*,*) "    Mesh set to mirror about xy plane."
        case ("xz")
            this%mirror_plane = 2
            if (verbose) write(*,*) "    Mesh set to mirror about xz plane."
        case ("yz")
            this%mirror_plane = 1
            if (verbose) write(*,*) "    Mesh set to mirror about yz plane."
        case default
            this%mirror_plane = 0
            this%mirrored = .false.
        end select
        
    end subroutine surface_mesh_parse_mirror_settings


    subroutine surface_mesh_parse_wake_settings(this, settings)

        implicit none
        
        class(surface_mesh),intent(inout) :: this
        type(json_value),pointer,intent(in) :: settings

        real :: wake_shedding_angle

        ! Check if the user wants a wake
        call json_xtnsn_get(settings, 'wake_model.wake_present', this%wake_present, .true.)
        call json_xtnsn_get(settings, 'wake_model.append_wake', this%append_wake, this%wake_present)

        ! Check that we're not trying to append a wake which is not present
        if (.not. this%wake_present .and. this%append_wake) then
            this%append_wake = .false.
        end if

        ! Store settings for wake models
        if (this%wake_present) then
            call json_xtnsn_get(settings, 'wake_model.wake_shedding_angle', wake_shedding_angle, 90.) ! Maximum allowable angle between panel normals without having separation
            this%C_wake_shedding_angle = cos(wake_shedding_angle*pi/180.)

            if (this%append_wake) then
                call json_xtnsn_get(settings, 'wake_model.trefftz_distance', this%trefftz_distance, -1.) ! Distance from origin to wake termination
                call json_xtnsn_get(settings, 'wake_model.N_panels', this%N_wake_panels_streamwise, 1)
            end if
        end if
        
    end subroutine surface_mesh_parse_wake_settings


    subroutine surface_mesh_find_vertices_on_mirror(this)
        ! Locates which vertices are on the mirror plane

        implicit none

        class(surface_mesh),intent(inout) :: this

        integer :: i

        ! Search for vertices lying on mirror plane
        do i=1,this%N_verts
            call this%vertices(i)%set_whether_on_mirror_plane(this%mirror_plane)
        end do
    
    end subroutine surface_mesh_find_vertices_on_mirror


    subroutine surface_mesh_locate_adjacent_panels(this)
        ! Loops through panels to determine which are adjacent

        implicit none

        class(surface_mesh),intent(inout),target :: this

        integer :: i, j, i_edge, edge_index_i, edge_index_j
        integer,dimension(2) :: i_endpoints
        integer,dimension(this%N_panels*3) :: panel1, panel2, vertex1, vertex2, edge_index1, edge_index2 ! By definition, we will have no more than 3*N_panels edges; we should have much less, but some people...
        logical,dimension(this%N_panels*3) :: on_mirror_plane

        if (verbose) write(*,'(a)',advance='no') "     Locating adjacent panels..."

        ! Initialize a few things
        on_mirror_plane = .false.

        ! Loop through each panel
        this%N_edges = 0

        !$OMP parallel do schedule(dynamic) private(j, i_endpoints, i_edge, edge_index_i, edge_index_j)
        do i=1,this%N_panels

            ! Loop through each potential neighbor
            neighbor_loop: do j=i+1,this%N_panels

                ! Check if we've found all neighbors for this panel
                if (all(this%panels(i)%abutting_panels /= 0)) exit neighbor_loop

                ! Check if these are abutting
                if (this%check_panels_adjacent(i, j, i_endpoints, edge_index_i, edge_index_j)) then

                    !$OMP critical
                    
                    ! Update number of edges
                    this%N_edges = this%N_edges + 1
                    i_edge = this%N_edges

                    ! Store vertices being adjacent to one another
                    call this%store_adjacent_vertices(i_endpoints, i_edge)

                    ! Store adjacent panels and panel edges
                    edge_index1(i_edge) = edge_index_i
                    edge_index2(i_edge) = edge_index_j

                    ! Store information in arrays for later storage in edge objects
                    panel1(i_edge) = i
                    panel2(i_edge) = j
                    vertex1(i_edge) = i_endpoints(1)
                    vertex2(i_edge) = i_endpoints(2)

                    !$OMP end critical

                end if

            end do neighbor_loop

        end do

        ! For each panel, check if it abuts the mirror plane
        if (this%mirrored) then

            mirror_plane_loop: do i=1,this%N_panels

                ! Check whether all neighbors have not been found
                if (all(this%panels(i)%abutting_panels /= 0)) cycle mirror_plane_loop

                ! Perform check
                if (this%panels(i)%check_abutting_mirror_plane(this%N_panels, i_endpoints, edge_index_i)) then

                    ! Update number of edges
                    this%N_edges = this%N_edges + 1
                    i_edge = this%N_edges

                    ! Store adjacent vertices
                    call this%store_adjacent_vertices(i_endpoints, i_edge)

                    ! Store edge index for this panel
                    edge_index1(i_edge) = edge_index_i

                    ! Store in arrays for later storage in edge objects
                    panel1(i_edge) = i
                    panel2(i_edge) = i + this%N_panels
                    vertex1(i_edge) = i_endpoints(1)
                    vertex2(i_edge) = i_endpoints(2)
                    on_mirror_plane(i_edge) = .true.
                    edge_index2(i_edge) = 0 ! Just a placeholder since the second panel doesn't technically exist

                end if

            end do mirror_plane_loop
        end if

        ! Check for panels abutting empty space and add those edges.
        do i=1,this%N_panels

            ! Check for an edge with no abutting panel
            do j=1,this%panels(i)%N
                if (this%panels(i)%abutting_panels(j) == 0) then

                    ! Warn that the mesh is not watertight
                    if (run_checks) then
                        write(*,*) "!!! Panel ", i, " is missing a neighbor, meaning the supplied mesh may not be watertight."
                        write(*,*) "!!! Solution quality may be adversely affected."
                    end if

                    ! Get endpoint indices
                    i_endpoints(1) = this%panels(i)%get_vertex_index(j)
                    i_endpoints(2) = this%panels(i)%get_vertex_index(mod(j, this%panels(i)%N) + 1)

                    ! Set up an edge
                    this%N_edges = this%N_edges + 1
                    i_edge = this%N_edges
                    panel1(i_edge) = i
                    panel2(i_edge) = 0 ! Placeholder
                    vertex1(i_edge) = i_endpoints(1)
                    vertex2(i_edge) = i_endpoints(2)
                    on_mirror_plane(i_edge) = .false.
                    edge_index1(i_edge) = j
                    edge_index2(i_edge) = 0

                    ! Store adjacent vertices
                    call this%store_adjacent_vertices(i_endpoints, i_edge)

                end if
            end do
        end do

        ! Allocate edge storage
        allocate(this%edges(this%N_edges))

        ! Initialize edges
        do i=1,this%N_edges

            ! Initialize
            call this%edges(i)%init(vertex1(i), vertex2(i), panel1(i), panel2(i))

            ! Store more information
            this%edges(i)%on_mirror_plane = on_mirror_plane(i)
            this%edges(i)%edge_index_for_panel(1) = edge_index1(i)
            this%edges(i)%edge_index_for_panel(2) = edge_index2(i)
            this%panels(panel1(i))%edges(this%edges(i)%edge_index_for_panel(1)) = i
            if (panel2(i) <= this%N_panels .and. panel2(i) > 0) then
                this%panels(panel2(i))%edges(this%edges(i)%edge_index_for_panel(2)) = i
            end if

        end do

        if (verbose) write(*,"(a, i7, a)") "Done. Found ", this%N_edges, " edges."
    
    end subroutine surface_mesh_locate_adjacent_panels


    subroutine surface_mesh_store_adjacent_vertices(this, i_verts, i_edge)
        ! Stores that the given two vertices are adjacent (i.e. they share an edge)

        implicit none

        class(surface_mesh), intent(inout) :: this
        integer,dimension(2), intent(in) :: i_verts
        integer,intent(in) :: i_edge

        ! Store that the vertices are adjacent
        if (.not. this%vertices(i_verts(1))%adjacent_vertices%is_in(i_verts(2))) then
            call this%vertices(i_verts(1))%adjacent_vertices%append(i_verts(2))
        end if
        if (.not. this%vertices(i_verts(2))%adjacent_vertices%is_in(i_verts(1))) then
            call this%vertices(i_verts(2))%adjacent_vertices%append(i_verts(1))
        end if

        ! Store that they touch this edge
        call this%vertices(i_verts(1))%adjacent_edges%append(i_edge)
        call this%vertices(i_verts(2))%adjacent_edges%append(i_edge)
        
    end subroutine surface_mesh_store_adjacent_vertices


    function surface_mesh_check_panels_adjacent(this, i, j, i_endpoints, edge_index_i, edge_index_j) result(adjacent)
        ! Checks whether panels i and j are adjacent

        class(surface_mesh),intent(inout) :: this
        integer,intent(in) :: i, j
        integer,dimension(2),intent(out) :: i_endpoints
        integer,intent(out) :: edge_index_i, edge_index_j

        logical :: adjacent

        logical :: already_found_shared
        integer :: m, n, m1, n1, temp

        ! Initialize
        adjacent = .false.

        ! Initialize for this panel pair
        already_found_shared = .false.

        ! Check if the panels are abutting
        abutting_loop: do m=1,this%panels(i)%N
            do n=1,this%panels(j)%N

                ! Check if vertices have the same index
                if (this%panels(i)%get_vertex_index(m) == this%panels(j)%get_vertex_index(n)) then

                    ! Previously found a shared vertex, so we have abutting panels
                    if (already_found_shared) then

                        adjacent = .true.

                        ! Store second shared vertex
                        i_endpoints(2) = this%panels(i)%get_vertex_index(m)

                        ! Check order; edge should proceed counterclockwise about panel i
                        if (m1 == 1 .and. m == this%panels(i)%N) then
                            temp = i_endpoints(1)
                            i_endpoints(1) = i_endpoints(2)
                            i_endpoints(2) = temp
                        end if

                        ! Store adjacent panels and panel edges
                        ! This stores the adjacent panels and edges according to the index of that edge
                        ! for the current panel

                        ! Store that i is adjacent to j
                        ! This one is more complicated because we don't know that n1 will be less than n; just the nature of the nested loop.
                        ! Basically, if one is 1 and the other is N, then we're dealing with edge N for panel j.
                        ! Otherwise, we're dealing with abs(n1-n) being 1, meaning edge min(n1, n).
                        if ( (n1 == 1 .and. n == this%panels(j)%N) .or. (n == 1 .and. n1 == this%panels(j)%N) ) then
                            this%panels(j)%abutting_panels(this%panels(j)%N) = i
                            edge_index_j = this%panels(j)%N
                        else
                            n1 = min(n, n1)
                            this%panels(j)%abutting_panels(n1) = i
                            edge_index_j = n1
                        end if

                        ! Store that j is adjacent to i
                        if (m1 == 1 .and. m == this%panels(i)%N) then ! Nth edge
                            this%panels(i)%abutting_panels(m) = j
                            edge_index_i = m
                        else ! 1st or 2nd edge
                            this%panels(i)%abutting_panels(m1) = j
                            edge_index_i = m1
                        end if

                        return

                    ! First shared vertex
                    else

                        already_found_shared = .true.
                        i_endpoints(1) = this%panels(i)%get_vertex_index(m)
                        m1 = m
                        n1 = n

                    end if
                end if

            end do

        end do abutting_loop
    
    end function surface_mesh_check_panels_adjacent


    subroutine surface_mesh_calc_vertex_geometry(this)
        ! Initializes the geometric parameters for the vertices which are dependent upon other parts of the mesh

        implicit none
        
        class(surface_mesh),intent(inout) :: this

        real(16),dimension(3) :: n_avg
        integer :: i, j, j_panel, N_panels

        if (verbose) write(*,'(a)',advance='no') "     Calculating vertex geometric parameters..."

        ! Loop through vertices
        !$OMP parallel do private(n_avg, j, j_panel, N_panels) schedule(dynamic)
        do i=1,this%N_verts

            ! Loop through neighboring panels and compute the average of their normal vectors
            n_avg = 0
            N_panels = this%vertices(i)%panels%len()
            do j=1,N_panels

                ! Get panel index
                call this%vertices(i)%panels%get(j, j_panel)

                ! Update using weighted normal
                n_avg = n_avg + this%panels(j_panel)%get_weighted_normal_at_corner(this%vertices(i)%loc)

            end do

            ! For vertices on the mirror plane, the component normal to the plane should be zeroed
            if (this%vertices(i)%on_mirror_plane) then
                n_avg(this%mirror_plane) = 0.
            end if

            ! Normalize and store
            this%vertices(i)%n_g = n_avg/norm2(n_avg)

            ! Calculate mirrored normal for mirrored vertex
            if (this%mirrored) then
                this%vertices(i)%n_g_mir = mirror_across_plane(this%vertices(i)%n_g, this%mirror_plane)
            end if

            ! Calculate average edge lengths
            call this%vertices(i)%set_average_edge_length(this%vertices)

        end do

        if (verbose) write(*,*) "Done."
        
    end subroutine surface_mesh_calc_vertex_geometry


    function surface_mesh_get_avg_characteristic_panel_length(this) result(l_avg)
        ! Returns the average characteristic length of the panels

        implicit none
        
        class(surface_mesh),intent(in) :: this

        real :: l_avg

        integer :: i

        ! Loop through panels
        l_avg = 0.
        do i=1,this%N_panels
            l_avg = l_avg + this%panels(i)%get_characteristic_length()
        end do
        l_avg = l_avg/this%N_panels
        
    end function surface_mesh_get_avg_characteristic_panel_length


    subroutine surface_mesh_init_with_flow(this, freestream, body_file, wake_file)

        implicit none

        class(surface_mesh),intent(inout) :: this
        type(flow),intent(in) :: freestream
        character(len=:),allocatable,intent(in) :: body_file, wake_file

        integer :: i

        ! Check flow symmetry condition
        this%asym_flow = .false.
        if (this%mirrored) then
            if (.not. freestream%sym_about(this%mirror_plane)) then
                this%asym_flow = .true.
            end if
        end if

        ! Initialize properties of panels dependent upon the flow
        call this%init_panels_with_flow(freestream)

        ! Figure out wake-shedding edges, discontinuous edges, etc.
        ! According to Davis, sharp, subsonic, leading edges in supersonic flow must have discontinuous doublet strength.
        ! I don't know why this would be, except in the case of leading-edge vortex separation. But Davis doesn't
        ! model leading-edge vortices. Wake-shedding trailing edges are still discontinuous in supersonic flow. Supersonic
        ! leading edges should have continuous doublet strength.
        call this%characterize_edges(freestream)

        if (this%wake_present) then

            ! Determine how cloning needs to be done
            call this%set_needed_vertex_clones()

            ! Clone necessary vertices
            call this%clone_vertices()

        end if

        ! Initialize vertex ordering (normally happens during vertex cloning)
        if (.not. this%found_wake_edges) then

            allocate(this%vertex_ordering(this%N_verts))
            do i=1,this%N_verts
                this%vertex_ordering(i) = i
            end do

        end if

        ! Determine surface convexity at each vertex
        !$OMP parallel do schedule(static)
        do i=1,this%N_verts
            this%vertices(i)%convex = this%is_convex_at_vertex(i)
        end do

        ! Initialize wake
        call this%init_wake(freestream, wake_file)

        ! Set up panel distributions
        if (verbose) write(*,"(a)",advance='no') "     Setting up panel singularity distributions..."

        !$OMP parallel do schedule(static)
        do i=1,this%N_panels
            call this%panels(i)%set_distribution(this%initial_panel_order, this%panels, &
                                                 this%vertices, this%mirrored, this%mirror_plane)
        end do
        
        if (verbose) write(*,*) "Done."

        ! Write out body file to let user ensure it's been parsed correctly
        if (body_file /= 'none') then
            call this%write_body(body_file, .false.)
        end if
    
    end subroutine surface_mesh_init_with_flow


    subroutine surface_mesh_init_panels_with_flow(this, freestream)
        ! Counts the number of sub- and superinclined panels

        implicit none
        
        class(surface_mesh),intent(inout) :: this
        type(flow),intent(in) :: freestream

        integer :: i

        if (verbose) write(*,'(a)',advance='no') "     Initializing flow-dependent panel properties..."

        !$OMP parallel do schedule(static)
        do i=1,this%N_panels
            call this%panels(i)%init_with_flow(freestream, this%mirrored, this%mirror_plane)
        end do

        ! Determine number of sub- and superinclined panels
        this%N_subinc = 0
        this%N_supinc = 0
        do i=1,this%N_panels

            ! Original panel
            if (this%panels(i)%r > 0.) then
                this%N_subinc = this%N_subinc + 1
            else
                this%N_supinc = this%N_supinc + 1
            end if

            ! Mirrored panel
            if (this%asym_flow) then
                if (this%panels(i)%r_mir > 0.) then
                    this%N_subinc = this%N_subinc + 1
                else
                    this%N_supinc = this%N_supinc + 1
                end if
            end if

        end do

        if (verbose) write(*,'(a i7 a)') " Done. Found ", this%N_supinc, " superinclined panels."
        
    end subroutine surface_mesh_init_panels_with_flow


    subroutine surface_mesh_characterize_edges(this, freestream)
        ! Locates wake-shedding edges and supersonic/subsonic edges on the mesh based on the flow conditions.

        implicit none

        class(surface_mesh),intent(inout) :: this
        type(flow),intent(in) :: freestream

        integer :: i, j, k, i_vert_1, i_vert_2, N_wake_edges
        real :: C_angle, C_min_angle
        real,dimension(3) :: second_normal, cross_result
        real,dimension(3) :: t_hat_g, d

        if (verbose) write(*,'(a)',advance='no') "     Characterizing edges..."

        ! Initialize
        N_wake_edges = 0
        this%found_wake_edges = .false.

        !$OMP parallel private(i, j, second_normal, C_angle, i_vert_1, i_vert_2) &
        !$OMP & private(cross_result, d, t_hat_g) reduction(min : C_min_angle)

        ! Loop through each edge
        !$OMP do schedule(dynamic)
        do k=1,this%N_edges

            ! Get info
            i = this%edges(k)%panels(1)
            j = this%edges(k)%panels(2)

            ! Check for edge on empty space, as this will definitely be discontinuous
            if (j == 0) then

                ! Mark as discontinuous
                this%edges(k)%discontinuous = .true.

                ! Store for panel
                !$OMP critical(update_edge)
                this%panels(this%edges(k)%panels(1))%N_discont_edges = this%panels(this%edges(k)%panels(1))%N_discont_edges + 1
                this%panels(this%edges(k)%panels(1))%edge_is_discontinuous(this%edges(k)%edge_index_for_panel(1)) = .true.
                !$OMP end critical(update_edge)

                ! Skip the rest
                cycle

            end if

            ! Get normal for panel j (dependent on mirroring)
            if (this%edges(k)%on_mirror_plane) then
                second_normal = mirror_across_plane(this%panels(i)%n_g, this%mirror_plane)
            else
                second_normal = this%panels(j)%n_g
            end if

            ! Calculate angle between panels (this is the flow-turning angle; it is the most straightforward to compute)
            C_angle = inner(this%panels(i)%n_g, second_normal)

            ! Store whether this edge is discontinuous

            ! Update panel information
            if (C_angle < this%C_max_cont_angle) then

                ! Set that the edge is discontinuous
                this%edges(k)%discontinuous = .true.

                !$OMP critical(update_edge)

                ! Update panel 1
                this%panels(this%edges(k)%panels(1))%N_discont_edges = this%panels(this%edges(k)%panels(1))%N_discont_edges + 1
                this%panels(this%edges(k)%panels(1))%edge_is_discontinuous(this%edges(k)%edge_index_for_panel(1)) = .true.

                ! Update panel 2
                if (.not. this%edges(k)%on_mirror_plane) then
                    this%panels(this%edges(k)%panels(2))%N_discont_edges = this%panels(this%edges(k)%panels(2))%N_discont_edges + 1
                    this%panels(this%edges(k)%panels(2))%edge_is_discontinuous(this%edges(k)%edge_index_for_panel(2)) = .true.
                end if
                !$OMP end critical(update_edge)

            end if

            ! Update minimum angle
            C_min_angle = min(C_angle, C_min_angle)

            ! Determine if this edge is wake-shedding
            if (.not. this%wake_present) cycle

            ! Check the angle between the panels
            if (C_angle < this%C_wake_shedding_angle) then

                ! Check angle of panel normal with freestream
                if (inner(this%panels(i)%n_g, freestream%V_inf) > 0.0 .or. inner(second_normal, freestream%V_inf) > 0.0) then
                    
                    ! Get vertex indices (simplifies later code)
                    i_vert_1 = this%edges(k)%top_verts(1)
                    i_vert_2 = this%edges(k)%top_verts(2)
                    
                    ! Calculate tangent in global coords (all we care about is the sign, so we don't need to normalize this)
                    t_hat_g = this%vertices(i_vert_2)%loc - this%vertices(i_vert_1)%loc
                    
                    ! Calculate cross product
                    cross_result = cross(this%panels(i)%n_g, second_normal)
                    
                    ! Check sign between normal vectors cross product and edge tangent
                    if (inner(cross_result, t_hat_g) > 0.) then
                        
                        ! Having passed the previous three checks, we've found a wake-shedding edge
                        this%found_wake_edges = .true.
                        this%edges(k)%sheds_wake = .true.
                        this%edges(k)%discontinuous = .true.

                        ! Update number of wake-shedding edges
                        !$OMP critical
                        N_wake_edges = N_wake_edges + 1
                        !$OMP end critical

                    end if
                end if
            end if

        end do

        !$OMP end parallel

        ! Store minimum angle
        this%C_min_panel_angle = C_min_angle

        if (verbose) write(*,'(a, i3, a, i3, a)') "Done. Found ", N_wake_edges, " wake-shedding edges."

    end subroutine surface_mesh_characterize_edges


    subroutine surface_mesh_set_needed_vertex_clones(this)
        ! Determines how many clones each vertex needs

        implicit none
        
        class(surface_mesh), intent(inout) :: this

        integer :: i

        if (verbose) write(*,'(a)',advance='no') "     Determining where vertex clones are needed..."

        ! Loop through vertices
        do i=1,this%N_verts
            call this%vertices(i)%set_needed_clones(this%edges)
        end do

        if (verbose) write(*,*) "Done."
        
    end subroutine surface_mesh_set_needed_vertex_clones


    subroutine surface_mesh_clone_vertices(this)
        ! Takes vertices which lie within wake-shedding edges and splits them into two vertices.
        ! Handles rearranging of necessary dependencies.

        implicit none

        class(surface_mesh),intent(inout) :: this

        integer :: i, j, k, N_clones, i_jango, i_boba, N_boba, i_edge, i_start_panel
        integer,dimension(:),allocatable :: i_panels_between, i_rearrange_inv, i_start_edge, i_end_edge
        integer,dimension(:,:),allocatable :: i_panels_between_all
        logical,dimension(:),allocatable :: mirrored_is_unique

        ! Check whether any wake edges exist
        if (this%found_wake_edges) then

            if (verbose) write(*,'(a)',advance='no') "     Cloning vertices at wake-shedding edges..."

            ! Determine how many clones we need to produce
            N_clones = 0
            do i=1,this%N_verts
                N_clones = N_clones + this%vertices(i)%get_needed_clones()
            end do

            ! Add space for new vertices
            call this%allocate_new_vertices(N_clones)

            ! Allocate rearranged indices array; we use this to place clones next to clones in the linear system
            allocate(i_rearrange_inv(this%N_verts), source=0)

            ! Initialize clones
            j = 0
            do i_jango=1,this%N_verts-N_clones ! Only need to loop through original vertices here

                ! Get location of original vertex in new array; this is shifted forward by how many clones have already been created
                i_rearrange_inv(i_jango) = i_jango + j

                ! Get number of needed clones
                N_boba = this%vertices(i_jango)%get_needed_clones()

                ! Create clones
                if (N_boba > 0) then

                    ! Get starting edge index for figuring out how to divvy up panels
                    ! We prefer to start at an edge on the mirror plane
                    ! Otherwise, we want to start at a wake-shedding edge
                    allocate(i_start_edge(N_boba+1))
                    allocate(i_end_edge(N_boba+1))
                    starting_edge_loop: do i=1,this%vertices(i_jango)%adjacent_edges%len()

                        ! Get edge index
                        call this%vertices(i_jango)%adjacent_edges%get(i, i_edge)

                        ! Check if it's on the mirror plane
                        if (this%edges(i_edge)%on_mirror_plane) then
                            i_start_edge(1) = i_edge
                            exit starting_edge_loop
                        end if

                        ! Check if it's a wake-shedding edge
                        if (this%edges(i_edge)%sheds_wake) then
                            i_start_edge(1) = i_edge ! We don't exit here because we may find a better edge
                        end if

                    end do starting_edge_loop

                    ! Loop through segments of the mesh surrounding the vertex divided by the wake edges
                    ! We only need to do this for true vertices, as midpoints have only two neighboring panels and one wake-shedding edge
                    allocate(mirrored_is_unique(N_boba+1), source=.true.)
                    allocate(i_panels_between_all(20,N_boba+1), source=0) ! I'm hoping we don't ever have more than 20 panels here
                    i_start_panel = this%edges(i_start_edge(1))%panels(1)
                    do i=1,N_boba+1

                        ! Figure out segment of panels
                        call this%find_next_wake_edge(i_start_edge(i), i_jango, i_start_panel, i_end_edge(i), i_panels_between)

                        ! Store panels
                        i_panels_between_all(1:size(i_panels_between),i) = i_panels_between

                        ! Check if the mirrored vertex for this segment will be unique
                        if (this%edges(i_start_edge(i))%on_mirror_plane .and. .not. this%edges(i_start_edge(i))%sheds_wake) then
                            mirrored_is_unique(i) = .false.
                        end if
                        if (this%edges(i_end_edge(i))%on_mirror_plane .and. .not. this%edges(i_end_edge(i))%sheds_wake) then
                            mirrored_is_unique(i) = .false.
                        end if

                        ! Move on to the next edge
                        if (i <= N_boba) i_start_edge(i+1) = i_end_edge(i)

                        ! The start panel will be across the ending edge from the last panel
                        i_start_panel = this%edges(i_end_edge(i))%get_opposing_panel(i_panels_between(size(i_panels_between)))

                    end do
                    deallocate(i_panels_between)

                    ! Set whether Jango has a unique mirror
                    this%vertices(i_jango)%mirrored_is_unique = mirrored_is_unique(1)

                    ! Create all the clones
                    do i=1,N_boba

                        ! Get index for the clone
                        j = j + 1
                        i_boba = this%N_verts - N_clones + j ! Will be at position N_verts-N_clones+j in the new vertex array

                        ! Get rearranged indices
                        i_rearrange_inv(i_boba) = i_jango + j

                        ! Initialize clone
                        allocate(i_panels_between, source=i_panels_between_all(:,i+1))
                        call this%init_vertex_clone(i_jango, i_boba, mirrored_is_unique(i+1), i_panels_between)
                        deallocate(i_panels_between)

                        ! Update vertices for edges to point to clones

                        ! For the starting edge, the clones will always be on the bottom
                        call this%edges(i_start_edge(i+1))%point_bottom_to_new_vert(i_jango, i_boba)

                        ! For the end edge, all but the last clone will be on the top
                        if (i < N_boba) then
                            call this%edges(i_end_edge(i+1))%point_top_to_new_vert(i_jango, i_boba)
                        else
                            call this%edges(i_end_edge(i+1))%point_bottom_to_new_vert(i_jango, i_boba)
                        end if

                    end do

                    ! Clean up memory for next iteration
                    deallocate(mirrored_is_unique)
                    deallocate(i_panels_between_all)
                    deallocate(i_start_edge)
                    deallocate(i_end_edge)

                    ! Set that this vertex has been cloned
                    this%vertices(i_jango)%clone = .true.

                ! If this vertex did not need to be cloned, but it is on the mirror plane and its mirror is unique
                ! then the wake strength will be determined by its mirror as well in the case of an asymmetric flow.
                else
                    if (this%mirrored .and. this%asym_flow .and.  this%vertices(i_jango)%on_mirror_plane .and. &
                        this%vertices(i_jango)%mirrored_is_unique) then

                        ! Loop through edges
                        do k=1,this%vertices(i_jango)%adjacent_edges%len()

                            ! Get edge index
                            call this%vertices(i_jango)%adjacent_edges%get(k, i_edge)

                            ! If this edge is a wake edge, set the bottom vert
                            if (this%edges(i_edge)%sheds_wake) then
                                call this%edges(i_edge)%point_bottom_to_new_vert(i_jango, i_jango+this%N_verts)
                            end if
                        end do

                    end if
                end if

            end do

            ! Get inverse mapping
            call invert_permutation_vector(this%N_verts, i_rearrange_inv, this%vertex_ordering)

            if (verbose) write(*,'(a, i4, a, i7, a)') "Done. Cloned ", N_clones, " vertices. Mesh now has ", &
                                                      this%N_verts, " vertices."
        end if

    end subroutine surface_mesh_clone_vertices


    subroutine surface_mesh_find_next_wake_edge(this, i_start_edge, i_shared_vert, i_start_panel, i_end_edge, i_panels_between)
        ! Locates the next wake edge starting from i_start_edge and i_start_panel which is also tied to i_shared_vert
        ! i_end_edge will be the index of the next wake edge
        ! i_panels_between will be a list of panel indices for the panels traversed between the two edges
        ! If the set of panels ends on the mirror plane and that edge does not shed a wake, the i_end_edge will be 0

        implicit none
        
        class(surface_mesh),intent(in) :: this
        integer,intent(in) :: i_start_edge, i_shared_vert, i_start_panel
        integer,intent(out) :: i_end_edge
        integer,dimension(:),allocatable,intent(out) :: i_panels_between

        integer :: i_curr_panel, i_next_panel, i_prev_panel, i
        type(list) :: i_panels_between_list
    
        ! Initialize the panel stepping
        i_curr_panel = i_start_panel
        i_prev_panel = this%edges(i_start_edge)%get_opposing_panel(i_start_panel)
        
        ! Step through panels to the next edge
        step_loop: do

            ! Add the current panel to the list
            call i_panels_between_list%append(i_curr_panel)

            ! Find the neighboring panel that touches the relevant vertex but isn't going backwards
            neighbor_loop: do i=1,3
                
                ! Get potential next panel and edge
                i_next_panel = this%panels(i_curr_panel)%abutting_panels(i)
                i_end_edge = this%panels(i_curr_panel)%edges(i)

                ! Check we're not going backwards
                if (i_next_panel == i_prev_panel .or. i_end_edge == i_start_edge) cycle neighbor_loop

                ! See if the edge touches the vertex
                if (this%edges(i_end_edge)%touches_vertex(i_shared_vert)) then
                    exit neighbor_loop
                end if

            end do neighbor_loop

            ! See if the edge we have reached is a wake edge; if so, we're done
            if (this%edges(i_end_edge)%sheds_wake) then
                exit step_loop
            end if

            ! See if the next panel exists; if it doesn't, we're done
            if (i_next_panel <= 0 .or. i_next_panel > this%N_panels) then
                exit step_loop
            end if

            ! Update for next iteration
            i_prev_panel = i_curr_panel
            i_curr_panel = i_next_panel
            
        end do step_loop

        ! Convert list to array
        allocate(i_panels_between(i_panels_between_list%len()))
        do i=1,i_panels_between_list%len()
            call i_panels_between_list%get(i, i_panels_between(i))
        end do
        
    end subroutine surface_mesh_find_next_wake_edge


    subroutine surface_mesh_init_vertex_clone(this, i_jango, i_boba, mirrored_is_unique, panels_for_this_clone)
        ! Clones the vertex at i_jango into i_boba

        implicit none
        
        class(surface_mesh), intent(inout) :: this
        integer,intent(in) :: i_jango, i_boba
        logical,intent(in) :: mirrored_is_unique
        integer,dimension(:),allocatable,intent(in) :: panels_for_this_clone

        integer :: j, k, i_panel, i_edge
        logical :: found_edge

        ! Basic initialization
        call this%vertices(i_boba)%init(this%vertices(i_jango)%loc, i_boba)
        this%vertices(i_boba)%clone = .true.

        ! Copy information which is the same
        call this%vertices(i_jango)%copy_to(this%vertices(i_boba))

        ! Mirroring properties
        this%vertices(i_boba)%mirrored_is_unique = mirrored_is_unique

        ! Remove necessary panels from Jango and give them to Boba
        do k=1,size(panels_for_this_clone)

            ! Make sure panel exists
            i_panel = panels_for_this_clone(k)
            if (i_panel /= 0) then

                ! Remove from Jango
                call this%vertices(i_jango)%panels_not_across_wake_edge%delete(i_panel)

                ! Add to Boba
                if (.not. this%vertices(i_boba)%panels_not_across_wake_edge%is_in(i_panel)) then
                    call this%vertices(i_boba)%panels_not_across_wake_edge%append(i_panel)
                end if

                ! Update panel to point to Boba (doesn't need to be done for mirrored panels)
                if (i_panel <= this%N_panels) then
                    call this%panels(i_panel)%point_to_new_vertex(this%vertices(i_boba))
                end if

            end if

        end do

        ! Update edges
        edge_loop: do k=1,this%vertices(i_boba)%adjacent_edges%len()

            ! Get edge index
            call this%vertices(i_boba)%adjacent_edges%get(k, i_edge)

            ! If this is a wake-shedding edge, just skip it, because the vertex reassignment has already been handled
            if (this%edges(i_edge)%sheds_wake) cycle edge_loop

            ! Loop through the panels being assigned to this clone to see if this edge touches any of them
            found_edge = .false.
            panel_loop: do j=1,size(panels_for_this_clone)

                ! Get panel index
                i_panel = panels_for_this_clone(j)

                ! Check if the edge touches this panel
                if (this%edges(i_edge)%panels(1)==i_panel .or. this%edges(i_edge)%panels(2)==i_panel) then
                    found_edge = .true.
                    exit panel_loop
                end if

            end do panel_loop

            ! If the edge touched a panel being assigned to this clone, update that edge as well
            ! We can do both top and bottom verts here, because this edge doesn't have top and bottom panels; it's not wake-shedding
            if (found_edge) then
                call this%edges(i_edge)%point_top_to_new_vert(i_jango, i_boba)
                call this%edges(i_edge)%point_bottom_to_new_vert(i_jango, i_boba)
            end if

        end do edge_loop
        
    end subroutine surface_mesh_init_vertex_clone


    subroutine surface_mesh_init_wake(this, freestream, wake_file)
        ! Handles wake initialization

        implicit none

        class(surface_mesh),intent(inout) :: this
        type(flow),intent(in) :: freestream
        character(len=:),allocatable,intent(in) :: wake_file

        logical :: dummy

        if (this%append_wake .and. this%found_wake_edges) then

            ! Update default Trefftz distance
            if (this%trefftz_distance < 0.) then

                ! Supersonic
                if (freestream%supersonic) then
                    call this%update_supersonic_trefftz_distance(freestream)

                ! Subsonic
                else
                    call this%update_subsonic_trefftz_distance(freestream)

                end if
            end if

            ! Initialize wake
            call this%wake%init(this%edges, this%vertices, freestream, this%asym_flow, this%mirror_plane, &
                                this%N_wake_panels_streamwise, this%trefftz_distance, this%mirrored, this%initial_panel_order, &
                                this%N_panels)
        
            ! Export wake geometry
            if (wake_file /= 'none') then
                call this%wake%write_strips(wake_file, dummy)
            end if

        else
            
            ! Set parameters to let later code know the wake is not being modeled
            this%wake%N_panels = 0
            this%wake%N_verts = 0
            this%wake%N_strips = 0

        end if
    
    end subroutine surface_mesh_init_wake


    subroutine surface_mesh_update_supersonic_trefftz_distance(this, freestream)
        ! Determines the appropriate Trefftz distance based on the mesh geometry

        implicit none

        class(surface_mesh),intent(inout) :: this
        type(flow),intent(in) :: freestream

        real :: max_dist, distance
        integer :: i

        ! Loop through mesh vertices, looking for the most downstream
        max_dist = 0.
        do i=1,this%N_verts

            ! Calculate distance
            distance = inner(this%vertices(i)%loc, freestream%c_hat_g)

            ! Check maximum
            max_dist = max(distance, max_dist)

            ! Check for mirror
            if (this%asym_flow) then

                ! Calculate distance
                distance = inner(mirror_across_plane(this%vertices(i)%loc, this%mirror_plane), freestream%c_hat_g)

                ! Check maximum
                max_dist = max(distance, max_dist)

            end if

        end do

        ! Set Trefftz distance
        this%trefftz_distance = max_dist
    
    end subroutine surface_mesh_update_supersonic_trefftz_distance


    subroutine surface_mesh_update_subsonic_trefftz_distance(this, freestream)
        ! Determines the appropriate Trefftz distance based on the mesh geometry

        implicit none

        class(surface_mesh),intent(inout) :: this
        type(flow),intent(in) :: freestream

        real :: back, front, x
        integer :: i

        ! Loop through vertices to calculate most downstream and upstream distances
        front = inner(freestream%c_hat_g, this%vertices(1)%loc)
        back = front
        do i=2,this%N_verts
            x = inner(freestream%c_hat_g, this%vertices(i)%loc)
            front = min(front, x)
            back = max(back, x)
        end do

        ! Calculate Trefftz distance
        this%trefftz_distance = 20.*abs(front-back)
    
    end subroutine surface_mesh_update_subsonic_trefftz_distance


    function surface_mesh_is_convex_at_vertex(this, i_vert) result(is_convex)
        ! Determines whether the mesh is convex at the given vertex

        implicit none
        
        class(surface_mesh),intent(in) :: this
        integer,intent(in) :: i_vert

        logical :: is_convex

        integer :: i, i_neighbor, j, j_neighbor
        real :: s, h
        logical :: first

        ! Initialize
        is_convex = .true.
        first = .true.
        s = 1.

        ! Loop through neighboring panels
        do i=1,this%vertices(i_vert)%panels%len()

            ! Get index
            call this%vertices(i_vert)%panels%get(i, i_neighbor)

            ! Loop through neighboring vertices
            vertex_loop: do j=1,this%vertices(i_vert)%adjacent_vertices%len()

                ! Get index
                call this%vertices(i_vert)%adjacent_vertices%get(j, j_neighbor)

                ! Check height above this panel
                h = inner(this%panels(i_neighbor)%n_g, this%vertices(j_neighbor)%loc-this%panels(i_neighbor)%centr)

                ! If it's in the plane of the panel, we can't say anything
                if (abs(h) > 1.e-10) then

                    ! Check sign of height
                    if (first) then
                        s = sign(s, h)
                        first = .false.
                    else
                        if (s*h < 0.) then
                            is_convex = .false.
                            return
                        end if
                    end if

                end if

                ! Do mirror checks
                if (this%vertices(i_vert)%on_mirror_plane) then

                    ! Check height above the mirrored panel
                    h = inner(this%panels(i_neighbor)%n_g_mir, this%vertices(j_neighbor)%loc-this%panels(i_neighbor)%centr_mir)

                    ! If it's in the plane of the panel, we can't say anything
                    if (abs(h) > 1.e-10) then

                        ! Check sign of height
                        if (first) then
                            s = sign(s, h)
                            first = .false.
                        else
                            if (s*h < 0.) then
                                is_convex = .false.
                                return
                            end if
                        end if

                    end if

                end if

            end do vertex_loop

        end do
        
    end function surface_mesh_is_convex_at_vertex


    function surface_mesh_get_clone_control_point_dir(this, i_vert) result(dir)
        ! Calculates the offset direction for the control point associated with the cloned vertex

        implicit none
        
        class(surface_mesh),intent(in) :: this
        integer,intent(in) :: i_vert
        real,dimension(3) :: dir

        integer :: j, k, i_panel, i_edge_1, i_edge_2, i_edge, panel1, panel2
        real,dimension(3) :: t1, t2, t_avg, tp, n_avg
        real :: C_min_panel_angle, offset_ratio, x
        logical :: found_first, tp_found

        ! Get the two edges defining the split for this vertex
        found_first = .false.
        i_edge_2 = 0
        do j=1,this%vertices(i_vert)%adjacent_edges%len()

            ! Get index
            call this%vertices(i_vert)%adjacent_edges%get(j, i_edge)

            ! Make sure it's a wake-shedding edge (we don't need to check other edges)
            if (this%edges(i_edge)%sheds_wake) then

                ! See if only one of this edge's panels belongs to the panels not across a wake edge for this vertex
                panel1 = this%edges(i_edge)%panels(1)
                panel2 = this%edges(i_edge)%panels(2)
                if ((this%vertices(i_vert)%panels_not_across_wake_edge%is_in(panel1) &
                     .and. .not. this%vertices(i_vert)%panels_not_across_wake_edge%is_in(panel2)) .or. &
                    (this%vertices(i_vert)%panels_not_across_wake_edge%is_in(panel2) &
                     .and. .not. this%vertices(i_vert)%panels_not_across_wake_edge%is_in(panel1))) then

                    ! Store
                    if (found_first) then
                        i_edge_2 = i_edge
                    else
                        i_edge_1 = i_edge
                        found_first = .true.
                    end if
                end if

            end if

        end do

        ! Get average tangent vector for the edge
        ! If we've found both, then we can use both
        if (i_edge_2 /= 0) then

            ! First edge
            t1 = this%vertices(i_vert)%loc - &
                    this%vertices(this%edges(i_edge_1)%get_opposite_endpoint(i_vert, this%vertices))%loc
            t1 = t1/norm2(t1)

            ! Second edge
            t2 = this%vertices(this%edges(i_edge_2)%get_opposite_endpoint(i_vert, this%vertices))%loc - &
                    this%vertices(i_vert)%loc
            t2 = t2/norm2(t2)

            ! Get average
            t_avg = t1 + t2
            t_avg = t_avg/norm2(t_avg)

        ! If we've only found one, then the other edge must be mirrored
        else
            t_avg = 0.
            t_avg(this%mirror_plane) = 1.
        end if

        ! Get the vector which is perpendicular to t_avg that also lies inside one of the panels not across a wake edge
        tp_found = .false.
        tp_loop: do j=1,this%vertices(i_vert)%panels_not_across_wake_edge%len()

            ! Get index of panel
            call this%vertices(i_vert)%panels_not_across_wake_edge%get(j, i_panel)

            ! Loop through vertices of panel j
            vertex_loop: do k=1,this%panels(i_panel)%N

                ! Check we've got a different vertex than the one we're trying to place a control point for
                if (i_vert == this%panels(i_panel)%get_vertex_index(k)) cycle vertex_loop

                ! Get vector to vertex
                tp = this%panels(i_panel)%get_vertex_loc(k) - this%vertices(i_vert)%loc

                ! Project the vector so it is perpendicular to t_avg
                tp = tp - t_avg*inner(t_avg, tp)

                ! Check tp isn't perfectly aligned with t_avg
                if (norm2(tp) < 1.e-12) cycle vertex_loop

                ! If it's still inside the panel, we've found our vector
                if (this%panels(i_panel)%projection_inside(0.1*tp + this%vertices(i_vert)%loc, .false., 0)) then
                    tp_found = .true.
                    exit tp_loop
                end if

            end do vertex_loop

            ! If none of the vertices worked, try the centroid
            tp = this%panels(i_panel)%centr - this%vertices(i_vert)%loc

            ! Project the vector so it is perpendicular to t_avg
            tp = tp - t_avg*inner(t_avg, tp)

            ! Check tp isn't perfectly aligned with t_avg
            if (norm2(tp) < 1.e-12) cycle tp_loop

            ! If it's still inside the panel, we've found our vector
            if (this%panels(i_panel)%projection_inside(0.1*tp + this%vertices(i_vert)%loc, .false., 0)) then
                tp_found = .true.
                exit tp_loop
            end if

        end do tp_loop

        ! Check we actually found tp
        if (.not. tp_found) then
            write(*,*)
            write(*,*) "!!! Failed to find t_p for placing a cloned control point at near vertex ", i_vert, ". Quitting..."
            stop
        end if

        ! Normalize
        tp = tp/norm2(tp)

        ! Find minimum angle between panels
        C_min_panel_angle = 1.
        do j=1,this%vertices(i_vert)%panels%len()

            ! Get index for first panel
            call this%vertices(i_vert)%panels%get(j, panel1)

            do k=j+1,this%vertices(i_vert)%panels%len()

                ! Get index for second panel
                call this%vertices(i_vert)%panels%get(k, panel2)

                ! Get minimum angle
                x = inner(this%panels(panel1)%n_g, this%panels(panel2)%n_g)
                C_min_panel_angle = min(C_min_panel_angle, x)

            end do

            ! Check angle between the panel and the mirror plane
            if (this%mirrored .and. this%vertices(i_vert)%on_mirror_plane) then

                x = -this%panels(panel1)%n_g(this%mirror_plane)
                C_min_panel_angle = min(C_min_panel_angle, x)

            end if

        end do

        ! Get average normal vector for panels not across wake edge
        n_avg = 0.
        do j=1,this%vertices(i_vert)%panels_not_across_wake_edge%len()

            ! Get index
            call this%vertices(i_vert)%panels_not_across_wake_edge%get(j, i_panel)

            ! Update normal
            n_avg = n_avg + this%panels(i_panel)%get_weighted_normal_at_corner(this%vertices(i_vert)%loc)

        end do
        n_avg = n_avg/norm2(n_avg)

        ! Determine direction
        offset_ratio = 0.5*sqrt(0.5*(1. + C_min_panel_angle))
        dir = tp - offset_ratio*n_avg

        ! Put in mirror plane
        if (this%vertices(i_vert)%on_mirror_plane) then
            dir(this%mirror_plane) = 0.
        end if

        ! Normalize
        dir = dir/norm2(dir)
    
    end function surface_mesh_get_clone_control_point_dir


    function surface_mesh_get_cp_locs_vertex_based_interior(this, offset, offset_type, freestream) result(cp_locs)
        ! Returns the locations of interior vertex-based control points
        ! Takes vertex clones into account

        implicit none
        
        class(surface_mesh),intent(in) :: this
        real,intent(in) :: offset
        character(len=:),allocatable,intent(in) :: offset_type
        type(flow),intent(in) :: freestream

        real,dimension(:,:),allocatable :: cp_locs

        integer :: i, j, i_panel
        real,dimension(3) :: dir, new_dir, n_avg, disp
        real :: this_offset

        ! Allocate memory
        allocate(cp_locs(3,this%N_verts))

        ! Loop through vertices
        !$OMP parallel do private(j, i_panel, dir, new_dir, n_avg, this_offset, disp) &
        !$OMP & schedule(dynamic) shared(this, offset, freestream, offset_type)
        do i=1,this%N_verts

            ! If the vertex is a clone, it needs to be shifted off the normal slightly so that it is unique from its counterpart
            if (this%vertices(i)%clone) then

                dir = this%get_clone_control_point_dir(i)

            ! If it has no clone, then placement simply follows the average normal vector
            else

                ! Set direction simply off of the average normal vector
                dir = -this%vertices(i)%n_g

            end if

            ! Determine offset
            select case (offset_type)

            case ('local')
                this_offset = offset*this%vertices(i)%l_avg

            case default ! Direct
                this_offset = offset

            end select
            
            ! Initialize control point
            cp_locs(:,i) = this%vertices(i)%loc + this_offset*dir

            ! Check if the control point is outside the mesh
            get_back_in_loop: do while (this%control_point_outside_mesh(cp_locs(:,i), i))

                ! Loop through neighboring panels to find ones the control point is outside
                n_avg = 0.
                do j=1,this%vertices(i)%panels%len()
                    
                    ! Get panel index
                    call this%vertices(i)%panels%get(j, i_panel)

                    ! Check if it's above the original panel
                    if (this%panels(i_panel)%point_above(cp_locs(:,i), .false., 0)) then
                        n_avg = n_avg + this%panels(i_panel)%get_weighted_normal_at_corner(this%vertices(i)%loc)
                    end if

                end do

                ! A control point on the mirror plane should stay there
                if (this%vertices(i)%on_mirror_plane) n_avg(this%mirror_plane) = 0.

                ! If there were no panels this control point was above, then exit
                if (norm2(n_avg) < 1.e-16) exit get_back_in_loop

                ! Reflect (partially, length-preserving) across plane defined by n_avg
                n_avg = n_avg/norm2(n_avg)
                disp = cp_locs(:,i)- this%vertices(i)%loc
                new_dir = disp - 1.1*n_avg*inner(disp, n_avg)
                new_dir = new_dir/norm2(new_dir)
                cp_locs(:,i)= this%vertices(i)%loc + this_offset*new_dir

            end do get_back_in_loop

        end do
    
    end function surface_mesh_get_cp_locs_vertex_based_interior


    function surface_mesh_get_cp_locs_centroid_based(this, offset) result(cp_locs)
        ! Places control points relative to each panel centroid
        ! offset is in the normal direction. A positive offset will place control points outside the mesh, and vice versa.

        implicit none
        
        class(surface_mesh),intent(in) :: this
        real,intent(in) :: offset

        real,dimension(:,:),allocatable :: cp_locs

        integer :: i

        ! Allocate
        allocate(cp_locs(3,this%N_panels))

        ! Loop through panels
        do i=1,this%N_panels
            cp_locs(:,i) = this%panels(i)%centr + this%panels(i)%n_g*offset
        end do
        
    end function surface_mesh_get_cp_locs_centroid_based


    function surface_mesh_control_point_outside_mesh(this, cp_loc, i_vert) result(outside)
        ! Checks whether the given control point (tied to vertex i_vert) is outside the mesh
        ! Note this will only check panels (and their mirrors) touching the vertex
        ! Uses the ray-casting algorithm

        implicit none
        
        class(surface_mesh),intent(in) :: this
        real,dimension(3),intent(in) :: cp_loc
        integer,intent(in) :: i_vert

        logical :: outside

        real,dimension(3) :: start, dir
        real :: s_star
        integer :: N_crosses, i, i_panel

        ! As the starting point for the ray-casting algorithm, we'll pick a point above the first neighboring panel
        call this%vertices(i_vert)%panels%get(1, i_panel)
        start = this%panels(i_panel)%get_characteristic_length()*this%panels(i_panel)%n_g + this%panels(i_panel)%centr
        dir = cp_loc - start

        ! Loop through neighboring panels, counting the number of times the line passes through
        N_crosses = 0
        do i=1,this%vertices(i_vert)%panels%len()

            ! Get panel index
            call this%vertices(i_vert)%panels%get(i, i_panel)

            ! Check for original panel
            if (this%panels(i_panel)%line_passes_through(start, dir, .false., 0, s_star)) then
                if (s_star <= 1. .and. s_star >= 0.) N_crosses = N_crosses + 1
            end if

            ! Check for mirrored panel
            if (this%vertices(i_vert)%on_mirror_plane) then
                if (this%panels(i_panel)%line_passes_through(start, dir, .true., this%mirror_plane, s_star)) then
                    if (s_star <= 1. .and. s_star >= 0.) N_crosses = N_crosses + 1
                end if
            end if
        end do

        ! The point is outside if there was an even number of crossings
        outside = mod(N_crosses, 2) == 0
        
    end function surface_mesh_control_point_outside_mesh


    subroutine surface_mesh_place_internal_vertex_control_points(this, offset, offset_type, freestream)

        implicit none

        class(surface_mesh),intent(inout) :: this
        real,intent(in) :: offset
        character(len=:),allocatable,intent(in) :: offset_type
        type(flow),intent(in) :: freestream

        integer :: i
        real,dimension(:,:),allocatable :: cp_locs

        ! Specify number of control points
        if (this%asym_flow) then
            this%N_cp = this%N_verts*2
        else
            this%N_cp = this%N_verts
        end if
        this%N_cp = this%N_cp

        ! Allocate memory
        allocate(this%cp(this%N_cp))

        ! Get control point locations
        cp_locs = this%get_cp_locs_vertex_based_interior(offset, offset_type, freestream)

        ! Initialize control points
        do i=1,this%N_verts
            call this%cp(i)%init(cp_locs(:,i), INTERNAL, TT_VERTEX, i)
        end do

        ! Initialize mirrored control points, if necessary
        if (this%asym_flow) then

            !$OMP parallel do schedule(static)
            do i=1,this%N_cp/2

                ! Initialize
                call this%cp(i+this%N_cp/2)%init(mirror_across_plane(this%cp(i)%loc, this%mirror_plane), &
                                                 this%cp(i)%cp_type, this%cp(i)%tied_to_type, this%cp(i)%tied_to_index)

                ! Specify that this is a mirror
                this%cp(i+this%N_cp/2)%is_mirror = .true.

            end do

        end if

    end subroutine surface_mesh_place_internal_vertex_control_points


    subroutine surface_mesh_place_centroid_control_points(this, add_one_inside, offset)
        ! Places control points on the surface of the mesh at panel centroids
        ! for the purpose of doublet strength matching
        ! add_one_inside will place an extra control point just under the first panel
        ! offset will specify whether the control points are to be placed a distance (offset) above the panel centroid

        implicit none
        
        class(surface_mesh),intent(inout) :: this
        logical,intent(in) :: add_one_inside
        real,intent(in) :: offset

        integer :: i, N_strength_matching
        real,dimension(:,:),allocatable :: cp_locs

        ! Specify number of control points
        if (this%asym_flow) then
            this%N_cp = this%N_panels*2
        else
            this%N_cp = this%N_panels
        end if
        if (add_one_inside) this%N_cp = this%N_cp + 1

        ! Count number of needed strength-matching points
        if (this%asym_flow) then

            ! Count up vertices that will need strength matching
            N_strength_matching = 0
            do i=1,this%N_verts
                if (this%vertices(i)%on_mirror_plane .and. .not. this%vertices(i)%mirrored_is_unique) then
                    N_strength_matching = N_strength_matching + 1
                end if
            end do
            this%N_cp = this%N_cp + N_strength_matching
        end if

        ! Get centroid-based locations
        cp_locs = this%get_cp_locs_centroid_based(offset)

        ! Allocate memory
        allocate(this%cp(this%N_cp))

        ! Initialize control points
        do i=1,this%N_panels
            if (offset == 0.) then
                call this%cp(i)%init(cp_locs(:,i), SURFACE, TT_PANEL, i)
            else if (offset > 0.) then
                call this%cp(i)%init(cp_locs(:,i), EXTERNAL, TT_PANEL, i)
            else
                call this%cp(i)%init(cp_locs(:,i), INTERNAL, TT_PANEL, i)
            end if
        end do

        ! Initialize mirrored control points, if necessary
        if (this%asym_flow) then

            !$OMP parallel do schedule(static)
            do i=1,this%N_panels

                ! Initialize
                call this%cp(i+this%N_panels)%init(mirror_across_plane(this%cp(i)%loc, this%mirror_plane), &
                                                   this%cp(i)%cp_type, this%cp(i)%tied_to_type, this%cp(i)%tied_to_index)

                ! Specify that this is a mirror
                this%cp(i+this%N_panels)%is_mirror = .true.

            end do

        end if

        ! Add in strength matching
        if (this%asym_flow) then
            do i=1,this%N_verts
                if (this%vertices(i)%on_mirror_plane .and. .not. this%vertices(i)%mirrored_is_unique) then
                    call this%cp(this%N_cp-N_strength_matching+i)%init((/0., 0., 0./), SURFACE, TT_VERTEX, i)
                    call this%cp(this%N_cp-N_strength_matching+i)%set_bc(STRENGTH_MATCHING)
                end if
            end do
        end if

        ! Add point inside
        if (add_one_inside) then
            call this%cp(this%N_cp)%init(this%panels(1)%centr-1.e-8*this%panels(1)%n_g*this%panels(1)%get_characteristic_length(), &
                         INTERNAL, TT_PANEL, 1)
        end if

    end subroutine surface_mesh_place_centroid_control_points


    subroutine surface_mesh_place_sparse_centroid_control_points(this, offset)
        ! Places control points on the surface of the mesh at panel centroids
        ! Will remove control points in a (hopefully) regular fashion to make as many control points as vertices
        ! for the purpose of doublet strength matching
        ! offset will specify whether the control points are to be placed a distance (offset) above the panel centroid

        implicit none
        
        class(surface_mesh),intent(inout) :: this
        real,intent(in) :: offset

        integer :: i, N_strength_matching, N_orig_cp, i_cp, N_initialized
        logical :: use_this_one
        real,dimension(:,:),allocatable :: cp_locs

        ! Specify number of control points we will start out with before sparsifying
        if (this%asym_flow) then
            N_orig_cp = this%N_panels*2
        else
            N_orig_cp = this%N_panels
        end if

        ! Count number of needed strength-matching points
        if (this%asym_flow) then

            ! Count up vertices that will need strength matching
            N_strength_matching = 0
            do i=1,this%N_verts
                if (this%vertices(i)%on_mirror_plane .and. .not. this%vertices(i)%mirrored_is_unique) then
                    N_strength_matching = N_strength_matching + 1
                end if
            end do
            N_orig_cp = N_orig_cp + N_strength_matching
        end if

        ! Get centroid-based locations
        cp_locs = this%get_cp_locs_centroid_based(offset)

        ! Decide how many control points we actually need
        if (this%asym_flow) then
            this%N_cp = this%N_verts*2
        else
            this%N_cp = this%N_verts
        end if

        ! Allocate memory
        allocate(this%cp(this%N_cp))

        ! Initialize control points selectively
        i_cp = 1
        do i=1,this%N_panels

            ! Check if we actually want this control point
            use_this_one = .false.
            if (this%asym_flow) then
            else
                if ((mod(i, 2) == 0) .or. (this%N_cp - i_cp == this%N_panels - i)) then
                    use_this_one = .true.
                end if
            end if

            ! If we want it, get it
            if (use_this_one) then

                ! Initialize
                if (offset == 0.) then
                    call this%cp(i_cp)%init(cp_locs(:,i), SURFACE, TT_PANEL, i)
                else if (offset > 0.) then
                    call this%cp(i_cp)%init(cp_locs(:,i), EXTERNAL, TT_PANEL, i)
                else
                    call this%cp(i_cp)%init(cp_locs(:,i), INTERNAL, TT_PANEL, i)
                end if

                ! Update index
                i_cp = i_cp + 1

            end if

        end do

        ! Initialize mirrored control points, if necessary
        if (this%asym_flow) then

            do i=1,this%N_panels

                ! Initialize
                call this%cp(i+this%N_panels)%init(mirror_across_plane(this%cp(i)%loc, this%mirror_plane), &
                                                   this%cp(i)%cp_type, this%cp(i)%tied_to_type, this%cp(i)%tied_to_index)

                ! Specify that this is a mirror
                this%cp(i+this%N_panels)%is_mirror = .true.

            end do

        end if

        ! Add in strength matching
        if (this%asym_flow) then
            do i=1,this%N_verts
                if (this%vertices(i)%on_mirror_plane .and. .not. this%vertices(i)%mirrored_is_unique) then
                    call this%cp(this%N_cp-N_strength_matching+i)%init((/0., 0., 0./), SURFACE, TT_VERTEX, i)
                    call this%cp(this%N_cp-N_strength_matching+i)%set_bc(STRENGTH_MATCHING)
                end if
            end do
        end if

    end subroutine surface_mesh_place_sparse_centroid_control_points


    subroutine surface_mesh_place_centroid_vertex_avg_control_points(this, add_one_inside)
        ! Places control points on the surface of the mesh at panel centroids
        ! for the purpose of doublet strength matching

        implicit none
        
        class(surface_mesh),intent(inout) :: this
        logical,intent(in) :: add_one_inside

        integer :: i, N_strength_matching, i_panel
        real,dimension(3) :: loc

        ! Specify number of control points
        if (this%asym_flow) then
            this%N_cp = this%N_verts*2
        else
            this%N_cp = this%N_verts
        end if
        if (add_one_inside) this%N_cp = this%N_cp + 1

        ! Count number of needed strength-matching points
        if (this%asym_flow) then

            ! Count up vertices that will need strength matching
            N_strength_matching = 0
            do i=1,this%N_verts
                if (this%vertices(i)%on_mirror_plane .and. .not. this%vertices(i)%mirrored_is_unique) then
                    N_strength_matching = N_strength_matching + 1
                end if
            end do
            this%N_cp = this%N_cp + N_strength_matching
        end if

        ! Allocate memory
        allocate(this%cp(this%N_cp))

        ! Loop through panels
        !$OMP parallel do schedule(static) private(i_panel, loc)
        do i=1,this%N_verts

            ! Get neighboring panel
            call this%vertices(i)%panels_not_across_wake_edge%get(1, i_panel)

            ! Determine location
            !loc = 0.5*this%panels(i_panel)%centr + 0.5*this%vertices(i)%loc
            loc = this%panels(i_panel)%centr + 1.e-7*this%panels(i_panel)%n_g
            
            ! Initialize control point
            call this%cp(i)%init(loc, SURFACE, TT_PANEL, i_panel)

        end do

        ! Initialize mirrored control points, if necessary
        if (this%asym_flow) then

            !$OMP parallel do schedule(static)
            do i=1,this%N_panels

                ! Initialize
                call this%cp(i+this%N_panels)%init(mirror_across_plane(this%cp(i)%loc, this%mirror_plane), &
                                                   this%cp(i)%cp_type, this%cp(i)%tied_to_type, this%cp(i)%tied_to_index)

                ! Specify that this is a mirror
                this%cp(i+this%N_panels)%is_mirror = .true.

            end do

        end if

        ! Add in strength matching
        if (this%asym_flow) then
            do i=1,this%N_verts
                if (this%vertices(i)%on_mirror_plane .and. .not. this%vertices(i)%mirrored_is_unique) then
                    call this%cp(this%N_cp-N_strength_matching+i)%init((/0., 0., 0./), SURFACE, TT_VERTEX, i)
                    call this%cp(this%N_cp-N_strength_matching+i)%set_bc(STRENGTH_MATCHING)
                end if
            end do
        end if

        ! Add point inside
        if (add_one_inside) then
            call this%cp(this%N_cp)%init(this%panels(1)%centr &
                         - 1.e-8*this%panels(1)%n_g*this%panels(1)%get_characteristic_length(), &
                         INTERNAL, TT_PANEL, 1)
        end if

    end subroutine surface_mesh_place_centroid_vertex_avg_control_points


    subroutine surface_mesh_calc_point_dod(this, point, freestream, dod_info, wake_dod_info)
        ! Calculates the domain of dependence for the point

        implicit none
        
        class(surface_mesh),intent(in) :: this
        real,dimension(3),intent(in) :: point
        type(flow),intent(in) :: freestream
        type(dod),dimension(:),allocatable,intent(out) :: dod_info
        type(dod),dimension(:,:),allocatable :: wake_dod_info

        logical,dimension(:),allocatable :: verts_in_dod, wake_verts_in_dod
        integer :: j, stat, N_verts, N_panels, N_strip_verts, N_strip_panels

        ! Figure out how many verts and panels we're going to consider
        if (this%mirrored) then
            if (this%asym_flow) then
                N_strip_verts = this%wake%N_max_strip_verts
                N_strip_panels = this%wake%N_max_strip_panels
            else
                N_strip_verts = 2*this%wake%N_max_strip_verts
                N_strip_panels = 2*this%wake%N_max_strip_panels
            end if
            N_verts = 2*this%N_verts
            N_panels = 2*this%N_panels
        else
            N_verts = this%N_verts
            N_panels = this%N_panels
            N_strip_verts = this%wake%N_max_strip_verts
            N_strip_panels = this%wake%N_max_strip_panels
        end if

        ! Allocate

        ! Whether mesh vertices are in the DoD
        allocate(verts_in_dod(N_verts), source=.false., stat=stat)
        call check_allocation(stat, "vertex domain of dependence storage")

        ! DoD info for panels
        allocate(dod_info(N_panels), stat=stat)
        call check_allocation(stat, "domain of dependence storage")

        ! Whether wake vertices are in the DoD
        allocate(wake_verts_in_dod(N_strip_verts), stat=stat)
        call check_allocation(stat, "wake vertex domain of dependence storage")

        ! DoD info for wake panels
        allocate(wake_dod_info(N_strip_panels,this%wake%N_strips), stat=stat)
        call check_allocation(stat, "wake domain of dependence storage")

        ! If the freestream is supersonic, calculate domain of dependence info
        if (freestream%supersonic) then

            ! Get DoD for original vertices
            verts_in_dod(1:this%N_verts) = this%get_verts_in_dod_of_point(point, freestream, .false.)

            ! Get DoD for mirrored vertices
            if (this%mirrored) then
                verts_in_dod(this%N_verts+1:) = this%get_verts_in_dod_of_point(point, freestream, .true.)
            end if

            ! Get info for original panels
            dod_info(1:this%N_panels) = this%get_panel_dod_info_for_point(point, freestream, verts_in_dod, .false.)

            ! Get info for mirrored panels
            if (this%mirrored) then
                dod_info(this%N_panels+1:) = this%get_panel_dod_info_for_point(point, freestream, verts_in_dod, .true.)
            end if

            deallocate(verts_in_dod)

            ! Loop through wake strips
            do j=1,this%wake%N_strips

                ! Get DoD for original vertices
                wake_verts_in_dod(1:this%wake%strips(j)%N_verts) = &
                    this%wake%strips(j)%get_verts_in_dod_of_point(point, freestream, .false.)

                ! Get DoD for mirrored vertices
                if (this%wake%strips(j)%mirrored) then
                    wake_verts_in_dod(this%wake%strips(j)%N_verts+1:) = &
                        this%wake%strips(j)%get_verts_in_dod_of_point(point, freestream, .true.)
                end if 

                ! Get info for original panels
                wake_dod_info(1:this%wake%strips(j)%N_panels,j) = &
                    this%wake%strips(j)%get_panel_dod_info_for_point(point, freestream, wake_verts_in_dod, .false.)

                ! Get info for mirrored panels
                if (this%wake%strips(j)%mirrored) then
                    wake_dod_info(this%wake%strips(j)%N_panels+1:,j) = &
                        this%wake%strips(j)%get_panel_dod_info_for_point(point, freestream, wake_verts_in_dod, .true.)
                end if
            end do

        end if

    end subroutine surface_mesh_calc_point_dod


    subroutine surface_mesh_get_induced_potentials_at_point(this, point, freestream, phi_d, phi_s)
        ! Calculates the source- and doublet-induced potentials at the given point

        implicit none
        
        class(surface_mesh),intent(in) :: this
        real,dimension(3),intent(in) :: point
        type(flow),intent(in) :: freestream
        real,intent(inout) :: phi_d, phi_s

        integer :: j, k
        real :: phi_d_panel, phi_s_panel
        type(dod),dimension(:),allocatable :: dod_info
        type(dod),dimension(:,:),allocatable :: wake_dod_info

        ! Calculate domain of dependence
        call this%calc_point_dod(point, freestream, dod_info, wake_dod_info)

        ! Loop through panels
        phi_s = 0.
        phi_d = 0.
        do k=1,this%N_panels

            ! Check DoD
            if (dod_info(k)%in_dod) then
            
                ! Calculate influence
                call this%panels(k)%calc_potentials(point, freestream, dod_info(k), .false., &
                                                    this%sigma, this%mu, this%N_panels, this%N_verts, &
                                                    this%asym_flow, phi_s_panel, phi_d_panel)
                phi_s = phi_s + phi_s_panel
                phi_d = phi_d + phi_d_panel

            end if

            ! Calculate mirrored influences
            if (this%mirrored) then

                if (dod_info(k+this%N_panels)%in_dod) then

                    if (this%asym_flow) then

                        ! Get influence of mirrored panel
                        call this%panels(k)%calc_potentials(point, freestream, dod_info(k+this%N_panels), .true., &
                                                            this%sigma, this%mu, this%N_panels, this%N_verts, &
                                                            this%asym_flow, phi_s_panel, phi_d_panel)
                        phi_s = phi_s + phi_s_panel
                        phi_d = phi_d + phi_d_panel

                    else

                        ! Get influence of panel on mirrored point
                        call this%panels(k)%calc_potentials(mirror_across_plane(point, this%mirror_plane), freestream, &
                                                            dod_info(k+this%N_panels), .false., &
                                                            this%sigma, this%mu, this%N_panels, this%N_verts, &
                                                            this%asym_flow, phi_s_panel, phi_d_panel)
                        phi_s = phi_s + phi_s_panel
                        phi_d = phi_d + phi_d_panel

                    end if

                end if

            end if

        end do

        ! Loop through wake panels
        do j=1,this%wake%N_strips
            do k=1,this%wake%strips(j)%N_panels

                ! Check DoD
                if (wake_dod_info(k,j)%in_dod) then
                
                    ! Calculate influence
                    call this%wake%strips(j)%panels(k)%calc_potentials(point, freestream, wake_dod_info(k,j), .false., &
                                                             this%sigma, this%mu, this%N_panels, this%N_verts, &
                                                             this%asym_flow, phi_s_panel, phi_d_panel)
                    phi_s = phi_s + phi_s_panel
                    phi_d = phi_d + phi_d_panel

                end if

                ! Calculate mirrored influences
                if (this%mirrored .and. .not. this%asym_flow) then

                    if (wake_dod_info(k+this%wake%N_max_strip_panels,j)%in_dod) then

                        ! Get influence of mirrored panel
                        call this%wake%strips(j)%panels(k)%calc_potentials(point, freestream, &
                                                                 wake_dod_info(k+this%wake%N_max_strip_panels,j), .true., &
                                                                 this%sigma, this%mu, this%N_panels, this%N_verts, &
                                                                 this%asym_flow, phi_s_panel, phi_d_panel)
                        phi_s = phi_s + phi_s_panel
                        phi_d = phi_d + phi_d_panel

                    end if

                end if

            end do
        end do
        
    end subroutine surface_mesh_get_induced_potentials_at_point


    subroutine surface_mesh_get_induced_velocities_at_point(this, point, freestream, v_d, v_s)
        ! Calculates the source- and doublet-induced potentials at the given point

        implicit none
        
        class(surface_mesh),intent(in) :: this
        real,dimension(3),intent(in) :: point
        type(flow),intent(in) :: freestream
        real,dimension(3),intent(out) :: v_d, v_s

        integer :: j, k
        real,dimension(3) :: v_d_panel, v_s_panel
        type(dod),dimension(:),allocatable :: dod_info
        type(dod),dimension(:,:),allocatable :: wake_dod_info

        ! Calculate domain of dependence
        call this%calc_point_dod(point, freestream, dod_info, wake_dod_info)

        ! Loop through panels
        v_d = (/0.,0.,0./)
        v_s = (/0.,0.,0./)

        do k=1,this%N_panels

            ! Check DoD
            if (dod_info(k)%in_dod) then
            
                ! Calculate influence
                call this%panels(k)%calc_velocities(point, freestream, dod_info(k), .false., &
                                                    this%sigma, this%mu, this%N_panels, this%N_verts, &
                                                    this%asym_flow, v_s_panel, v_d_panel)

                ! Because computers don't like adding zero a bunch
                if (this%panels(k)%has_sources) v_s = v_s + v_s_panel
                v_d = v_d + v_d_panel

            end if

            ! Calculate mirrored influences
            if (this%mirrored) then

                if (dod_info(k+this%N_panels)%in_dod) then

                    if (this%asym_flow) then

                        ! Get influence of mirrored panel
                        call this%panels(k)%calc_velocities(point, freestream, dod_info(k+this%N_panels), .true., &
                                                            this%sigma, this%mu, this%N_panels, this%N_verts, &
                                                            this%asym_flow, v_s_panel, v_d_panel)
                        if (this%panels(k)%has_sources) v_s = v_s + v_s_panel
                        v_d = v_d + v_d_panel

                    else

                        ! Get influence of panel on mirrored point
                        call this%panels(k)%calc_velocities(mirror_across_plane(point, this%mirror_plane), freestream, &
                                                            dod_info(k+this%N_panels), .false., &
                                                            this%sigma, this%mu, this%N_panels, this%N_verts, &
                                                            this%asym_flow, v_s_panel, v_d_panel)

                        v_d_panel = mirror_across_plane(v_d_panel, this%mirror_plane)
                        v_s_panel = mirror_across_plane(v_s_panel, this%mirror_plane)
                        if (this%panels(k)%has_sources) v_s = v_s + v_s_panel
                        v_d = v_d + v_d_panel

                    end if

                end if

            end if

        end do

        ! Loop through wake panels
        do j=1,this%wake%N_strips
            do k=1,this%wake%strips(j)%N_panels

                ! Check DoD
                if (wake_dod_info(k,j)%in_dod) then
                
                    ! Calculate influence
                    call this%wake%strips(j)%panels(k)%calc_velocities(point, freestream, wake_dod_info(k,j), .false., &
                                                             this%sigma, this%mu, this%N_panels, this%N_verts, &
                                                             this%asym_flow, v_s_panel, v_d_panel)
                    
                    v_d = v_d + v_d_panel
                    if (this%wake%strips(j)%panels(k)%has_sources) v_s = v_s + v_s_panel

                end if

                ! Calculate mirrored influences
                if (this%mirrored .and. .not. this%asym_flow) then

                    if (wake_dod_info(k+this%wake%N_max_strip_panels,j)%in_dod) then

                        ! Get influence of mirrored panel
                        call this%wake%strips(j)%panels(k)%calc_velocities(point, freestream, &
                                                                 wake_dod_info(k+this%wake%N_max_strip_panels,j), .true., &
                                                                 this%sigma, this%mu, this%N_panels, this%N_verts, &
                                                                 this%asym_flow, v_s_panel, v_d_panel)
                        
                        v_d_panel = mirror_across_plane(v_d_panel, this%mirror_plane)
                        v_s_panel = mirror_across_plane(v_s_panel, this%mirror_plane)
                        if (this%wake%strips(j)%panels(k)%has_sources) v_s = v_s + v_s_panel
                        v_d = v_d + v_d_panel

                    end if

                end if

            end do
        end do
    end subroutine surface_mesh_get_induced_velocities_at_point



    subroutine surface_mesh_output_results(this, body_file, wake_file, control_point_file, mirrored_body_file)

        implicit none

        class(surface_mesh),intent(inout) :: this
        character(len=:),allocatable,intent(in) :: body_file, wake_file, control_point_file
        character(len=:),allocatable,intent(in) :: mirrored_body_file

        logical :: wake_exported

        ! Write out data for body
        if (body_file /= 'none') then
            call this%write_body(body_file, solved=.true.)
            if (verbose) write(*,'(a30 a)') "    Surface: ", body_file
        end if

        ! Write out data for mirrored body
        if (mirrored_body_file /= 'none' .and. this%asym_flow) then
            call this%write_body_mirror(mirrored_body_file, solved=.true.)
            if (verbose) write(*,'(a30 a)') "    Mirrored surface: ", mirrored_body_file
        end if
        
        ! Write out data for wake
        if (wake_file /= 'none') then
            call this%wake%write_strips(wake_file, wake_exported, this%mu)

            if (wake_exported) then
                if (verbose) write(*,'(a30 a)') "    Wake: ", wake_file
            else
                if (verbose) write(*,'(a30 a)') "    Wake: ", "no wake to export"
            end if
        end if
        
        ! Write out data for control points
        if (control_point_file /= 'none') then
            call this%write_control_points(control_point_file, solved=.true.)

            if (verbose) write(*,'(a30 a)') "    Control points: ", control_point_file
        end if
        
    end subroutine surface_mesh_output_results


    subroutine surface_mesh_write_body(this, body_file, solved)
        ! Writes the body and results (if solved) out to file

        implicit none
        
        class(surface_mesh),intent(in) :: this
        character(len=:),allocatable,intent(in) :: body_file
        logical,intent(in) :: solved

        type(vtk_out) :: body_vtk
        integer :: i, N_cells
        real,dimension(:),allocatable :: panel_inclinations, orders, N_discont_edges, convex
        real,dimension(:,:),allocatable :: cents

        ! Clear old file
        call delete_file(body_file)

        ! Determine number of cells to export
        N_cells = this%N_panels

        ! Get panel inclinations, centroids, and distribution orders
        allocate(panel_inclinations(this%N_panels))
        allocate(orders(this%N_panels))
        allocate(N_discont_edges(this%N_panels))
        allocate(cents(3,this%N_panels))
        allocate(convex(this%N_verts), source=0.)
        do i=1,this%N_panels
            panel_inclinations(i) = this%panels(i)%r
            orders(i) = this%panels(i)%order
            N_discont_edges(i) = this%panels(i)%N_discont_edges
            cents(:,i) = this%panels(i)%centr
        end do
        do i=1,this%N_verts
            if (this%vertices(i)%convex) convex(i) = 1.
        end do

        ! Write geometry
        call body_vtk%begin(body_file)
        call body_vtk%write_points(this%vertices)
        call body_vtk%write_panels(this%panels, mirror=.false.)
        call body_vtk%write_cell_normals(this%panels)
        call body_vtk%write_cell_scalars(panel_inclinations, "inclination")
        call body_vtk%write_cell_scalars(orders, "distribution_order")
        call body_vtk%write_cell_scalars(N_discont_edges, "N_discontinuous_edges")
        call body_vtk%write_cell_vectors(cents, "centroid")

        if (solved) then

            ! Pressures
            if (allocated(this%C_p_inc)) then
                call body_vtk%write_cell_scalars(this%C_p_inc(1:N_cells), "C_p_inc")
            end if
            if (allocated(this%C_p_ise)) then
                call body_vtk%write_cell_scalars(this%C_p_ise(1:N_cells), "C_p_ise")
            end if
            if (allocated(this%C_p_2nd)) then
                call body_vtk%write_cell_scalars(this%C_p_2nd(1:N_cells), "C_p_2nd")
            end if
            if (allocated(this%C_p_lin)) then
                call body_vtk%write_cell_scalars(this%C_p_lin(1:N_cells), "C_p_lin")
            end if
            if (allocated(this%C_p_sln)) then
                call body_vtk%write_cell_scalars(this%C_p_sln(1:N_cells), "C_p_sln")
            end if

            ! Corrected pressures
            if (allocated(this%C_p_pg)) then
                call body_vtk%write_cell_scalars(this%C_p_pg(1:N_cells), "C_p_PG")
            end if
            if (allocated(this%C_p_kt)) then
                call body_vtk%write_cell_scalars(this%C_p_kt(1:N_cells), "C_p_KT")
            end if
            if (allocated(this%C_p_lai)) then
                call body_vtk%write_cell_scalars(this%C_p_lai(1:N_cells), "C_p_L")
            end if

            ! Source strengths
            call body_vtk%write_cell_scalars(this%sigma(1:this%N_panels), "sigma")

            ! Cell velocities and sources
            call body_vtk%write_cell_vectors(this%V_cells(:,1:N_cells), "v")
            call body_vtk%write_cell_vectors(this%V_cells_inner(:,1:N_cells), "v_inner")
            call body_vtk%write_cell_vectors(this%dC_f(:,1:N_cells), "dC_f")

            ! Surface potential values
            call body_vtk%write_point_scalars(this%mu(1:this%N_verts), "mu")
            call body_vtk%write_point_scalars(this%Phi_u(1:this%N_verts), "Phi_u")

        end if
        call body_vtk%write_point_scalars(convex, "convex")

        ! Finalize
        call body_vtk%finish()
    
    end subroutine surface_mesh_write_body


    subroutine surface_mesh_write_body_mirror(this, mirrored_body_file, solved)
        ! Writes the body mirror and results (if solved) out to file

        implicit none
        
        class(surface_mesh),intent(in) :: this
        character(len=:),allocatable,intent(in) :: mirrored_body_file
        logical,intent(in) :: solved

        type(vtk_out) :: body_vtk
        integer :: i, N_cells
        real,dimension(:),allocatable :: panel_inclinations, orders, N_discont_edges, convex
        real,dimension(:,:),allocatable :: cents

        ! Clear old file
        call delete_file(mirrored_body_file)

        ! Determine number of cells to export
        N_cells = this%N_panels

        ! Get panel inclinations, centroids, and orders
        allocate(panel_inclinations(this%N_panels))
        allocate(orders(this%N_panels))
        allocate(N_discont_edges(this%N_panels))
        allocate(cents(3,this%N_panels))
        allocate(convex(this%N_verts), source=0.)
        do i=1,this%N_panels
            panel_inclinations(i) = this%panels(i)%r_mir
            orders(i) = this%panels(i)%order
            N_discont_edges(i) = this%panels(i)%N_discont_edges
            cents(:,i) = this%panels(i)%centr_mir
        end do
        do i=1,this%N_verts
            if (this%vertices(i)%convex) convex(i) = 1.
        end do

        ! Write geometry
        call body_vtk%begin(mirrored_body_file)
        call body_vtk%write_points(this%vertices, this%mirror_plane)
        call body_vtk%write_panels(this%panels, mirror=.true.)
        call body_vtk%write_cell_normals(this%panels, this%mirror_plane)
        call body_vtk%write_cell_scalars(panel_inclinations, "inclination")
        call body_vtk%write_cell_scalars(orders, "distribution_order")
        call body_vtk%write_cell_scalars(N_discont_edges, "N_discontinuous_edges")
        call body_vtk%write_cell_vectors(cents, "centroid")

        if (solved) then

            ! Pressures
            if (allocated(this%C_p_inc)) then
                call body_vtk%write_cell_scalars(this%C_p_inc(N_cells+1:N_cells*2), "C_p_inc")
            end if
            if (allocated(this%C_p_ise)) then
                call body_vtk%write_cell_scalars(this%C_p_ise(N_cells+1:N_cells*2), "C_p_ise")
            end if
            if (allocated(this%C_p_2nd)) then
                call body_vtk%write_cell_scalars(this%C_p_2nd(N_cells+1:N_cells*2), "C_p_2nd")
            end if
            if (allocated(this%C_p_lin)) then
                call body_vtk%write_cell_scalars(this%C_p_lin(N_cells+1:N_cells*2), "C_p_lin")
            end if
            if (allocated(this%C_p_sln)) then
                call body_vtk%write_cell_scalars(this%C_p_sln(N_cells+1:N_cells*2), "C_p_sln")
            end if

            ! Corrected pressures
            if (allocated(this%C_p_pg)) then
                call body_vtk%write_cell_scalars(this%C_p_pg(N_cells+1:N_cells*2), "C_p_PG")
            end if
            if (allocated(this%C_p_kt)) then
                call body_vtk%write_cell_scalars(this%C_p_kt(N_cells+1:N_cells*2), "C_p_KT")
            end if
            if (allocated(this%C_p_lai)) then
                call body_vtk%write_cell_scalars(this%C_p_lai(N_cells+1:N_cells*2), "C_p_L")
            end if

            ! Sources
            call body_vtk%write_cell_scalars(this%sigma(this%N_panels+1:this%N_panels*2), "sigma")

            ! Other
            call body_vtk%write_cell_vectors(this%V_cells(:,N_cells+1:N_cells*2), "v")
            call body_vtk%write_cell_vectors(this%V_cells_inner(:,N_cells+1:N_cells*2), "v_inner")
            call body_vtk%write_cell_vectors(this%dC_f(:,N_cells+1:N_cells*2), "dC_f")

            ! Surface potentials
            call body_vtk%write_point_scalars(this%mu(this%N_verts+1:this%N_verts*2), "mu")
            call body_vtk%write_point_scalars(this%Phi_u(this%N_verts+1:this%N_verts*2), "Phi_u")

        end if
        call body_vtk%write_point_scalars(convex, "convex")

        call body_vtk%finish()

    end subroutine surface_mesh_write_body_mirror


    subroutine surface_mesh_write_control_points(this, control_point_file, solved)
        ! Writes the control points (and results if solved) out to file

        implicit none
        
        class(surface_mesh),intent(in) :: this
        character(len=:),allocatable,intent(in) :: control_point_file
        logical,intent(in) :: solved

        type(vtk_out) :: cp_vtk
        real,dimension(3,this%N_cp) :: cp_locs
        integer,dimension(this%N_cp) :: cp_bcs
        integer :: i

        ! Get control point locations and boundary conditions
        do i=1,this%N_cp
            cp_locs(:,i) = this%cp(i)%loc
            cp_bcs(i) = this%cp(i)%bc
        end do

        ! Clear old file
        call delete_file(control_point_file)

        ! Write out data
        call cp_vtk%begin(control_point_file)
        call cp_vtk%write_points(cp_locs)
        call cp_vtk%write_vertices(this%N_cp)
        call cp_vtk%write_point_scalars(cp_bcs, "BC_type")
        
        ! Results
        if (solved) then
            call cp_vtk%write_point_scalars(this%R_cp, "residual")
        end if

        call cp_vtk%finish()
        
    end subroutine surface_mesh_write_control_points

end module surface_mesh_mod