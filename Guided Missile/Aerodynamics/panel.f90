! Panel type
module panel_mod

    use helpers_mod
    use linked_list_mod
    use base_geom_mod
    use math_mod
    use flow_mod
    use linalg_mod

    implicit none

    logical :: force_sigma_match, p_refine


    type integrals
        ! Container type for the fundamental integrals used to calculate influence coefficients

        integer :: r, s, rs ! Parameters that will be most convenient to keep here (panel inclination and flow type indicators)
        real :: H111, H211, H121 ! Source integrals
        real :: hH113, H213, H123, H313, H223, H133 ! Doublet integrals; we use hH(1,1,3) because it can be reliably calculated, unlike H(1,1,3)
        real,dimension(:),allocatable :: F111, F211, F121 ! Necessary line integrals
        real :: h3H115, H215, H125, H225, hH315, hH135, H415, H145, H325, H235, H113_3rsh2H115 ! Doublet velocity integrals. Yeah, there are a lot...
        real,dimension(:),allocatable :: F113, F123, F213, F133, F313 ! Necessary for doublet velocity integrals

    end type integrals


    type dod
        ! Container type for parameters of whether a panel lies in a point's domain of dependence

        logical :: in_dod = .true.
        logical,dimension(3) :: edges_in_dod = .true.

    end type dod


    type panel
        ! A three-sided panel

        integer :: N = 3 ! Number of sides/vertices
        integer :: index ! Index of this panel in the mesh array
        type(vertex_pointer),dimension(:),allocatable :: vertices
        real,dimension(3) :: n_g, nu_g ! Normal and conormal vectors
        real,dimension(3) :: n_g_mir, nu_g_mir ! Mirrored normal and conormal vectors
        real,dimension(3) :: centr, centr_mir ! Centroid
        real,dimension(3,3) :: A_g_to_ls, A_ls_to_g ! Coordinate transformation matrices
        real,dimension(3,3) :: A_g_to_ls_mir, A_ls_to_g_mir
        real,dimension(:,:),allocatable :: vertices_ls, vertices_ls_mir ! Location of the vertices described in local scaled coords
        real,dimension(:,:),allocatable :: n_hat_g, n_hat_ls ! Edge unit outward normals
        real,dimension(:,:),allocatable :: n_hat_g_mir, n_hat_ls_mir
        real,dimension(:),allocatable :: b, sqrt_b ! Edge parameter
        real,dimension(:),allocatable :: b_mir, sqrt_b_mir
        real :: A ! Surface area (same for mirror, in global coordinates at least)
        real,dimension(:,:),allocatable :: T_mu, T_sigma ! Matrix relating doublet/source strengths to doublet/source influence parameters
        real,dimension(:,:),allocatable :: T_mu_mir, T_sigma_mir ! Same for mirrored panels
        real,dimension(:,:),allocatable :: S_mu_inv, S_mu_inv_mir
        real,dimension(:,:),allocatable :: C, C_mir ! Integrals for integrating a quadratic pressure distribution
        logical :: in_wake ! Whether this panel belongs to a wake mesh
        integer :: i_top_parent, i_bot_parent ! The parent panels for this panel, if it is in the wake
        integer,dimension(3) :: abutting_panels ! Indices of panels abutting this one
        integer,dimension(3) :: edges ! Indices of the edges of this panel
        integer :: r, r_mir ! Panel inclination indicator; r=-1 -> superinclined, r=1 -> subinclined
        real :: J, J_mir ! Local scaled transformation Jacobian
        integer,dimension(:),allocatable :: i_vert_d, i_panel_s
        integer :: order, N_discont_edges
        logical,dimension(:),allocatable :: edge_is_discontinuous
        logical :: has_sources ! Whether this panel has a source distribution
        integer :: mu_dim, M_dim, sigma_dim, S_dim ! Dimensions of doublet and source parameter and strength spaces

        contains

            ! Initialization procedures
            procedure :: init => panel_init_3
            procedure :: calc_derived_geom => panel_calc_derived_geom
            procedure :: calc_normal => panel_calc_normal
            procedure :: calc_area => panel_calc_area
            procedure :: calc_centroid => panel_calc_centroid
            procedure :: calc_g_edge_vectors => panel_calc_g_edge_vectors
            procedure :: get_characteristic_length => panel_get_characteristic_length

            ! Flow-dependent initialization procedures
            procedure :: init_with_flow => panel_init_with_flow
            procedure :: calc_g_to_ls_transform => panel_calc_g_to_ls_transform
            procedure :: calc_ls_edge_vectors => panel_calc_ls_edge_vectors

            ! Mirror initialization
            procedure :: init_mirror => panel_init_mirror
            procedure :: calc_mirrored_g_to_ls_transform => panel_calc_mirrored_g_to_ls_transform
            procedure :: calc_mirrored_edge_vectors => panel_calc_mirrored_edge_vectors

            ! Singularity distributions
            procedure :: set_distribution => panel_set_distribution
            procedure :: set_doublet_verts => panel_set_doublet_verts
            procedure :: set_source_panels => panel_set_source_panels
            procedure :: calc_M_mu_transform => panel_calc_M_mu_transform
            procedure :: calc_S_sigma_transform => panel_calc_S_sigma_transform

            ! Pressure integration
            procedure :: calc_C_integrals => panel_calc_C_integrals
            procedure :: calc_mirrored_C_integrals => panel_calc_mirrored_C_integrals

            ! Getters
            procedure :: get_vertex_loc => panel_get_vertex_loc
            procedure :: get_vertex_index => panel_get_vertex_index
            procedure :: get_corner_angle => panel_get_corner_angle
            procedure :: get_weighted_normal_at_corner => panel_get_weighted_normal_at_corner
            procedure :: get_projection_onto_surface => panel_get_projection_onto_surface
            procedure :: get_opposite_vertex => panel_get_opposite_vertex
            procedure :: get_opposite_panel => panel_get_opposite_panel
            procedure :: get_opposite_edge => panel_get_opposite_edge
            procedure :: get_local_coords_of_point => panel_get_local_coords_of_point

            ! Checks
            procedure :: touches_vertex => panel_touches_vertex
            procedure :: check_abutting_mirror_plane => panel_check_abutting_mirror_plane
            procedure :: projection_inside => panel_projection_inside
            procedure :: point_outside => panel_point_outside
            procedure :: point_above => panel_point_above
            procedure :: line_passes_through => panel_line_passes_through

            ! Update information
            procedure :: point_to_new_vertex => panel_point_to_new_vertex

            ! Domain of dependence checking
            procedure :: check_dod => panel_check_dod

            ! Influence calculations

            ! Geometry
            procedure :: calc_basic_geom => panel_calc_basic_geom
            procedure :: calc_subsonic_geom => panel_calc_subsonic_geom
            procedure :: calc_supersonic_subinc_geom => panel_calc_supersonic_subinc_geom
            procedure :: calc_supersonic_supinc_geom => panel_calc_supersonic_supinc_geom
            
            ! Endpoint integrals
            procedure :: EMNK => panel_EMNK

            ! Edge integrals
            procedure :: calc_basic_F_integrals_subsonic => panel_calc_basic_F_integrals_subsonic
            procedure :: calc_basic_F_integrals_supersonic_subinc => panel_calc_basic_F_integrals_supersonic_subinc
            procedure :: calc_basic_F_integrals_supersonic_supinc => panel_calc_basic_F_integrals_supersonic_supinc

            ! hH(1,1,3)
            procedure :: calc_hH113_subsonic => panel_calc_hH113_subsonic
            procedure :: calc_hH113_supersonic_subinc => panel_calc_hH113_supersonic_subinc
            procedure :: calc_hH113_supersonic_supinc => panel_calc_hH113_supersonic_supinc

            ! Integral recursions
            procedure :: calc_remaining_integrals => panel_calc_remaining_integrals
            procedure :: calc_F_recursions_for_velocity => panel_calc_F_recursions_for_velocity
            procedure :: calc_H_recursions_for_velocity => panel_calc_H_recursions_for_velocity

            ! All integrals
            procedure :: calc_integrals => panel_calc_integrals

            ! Potentials
            procedure :: assemble_phi_s_S_space => panel_assemble_phi_s_S_space
            procedure :: assemble_phi_d_M_space => panel_assemble_phi_d_M_space
            procedure :: calc_potential_influences => panel_calc_potential_influences
            procedure :: calc_potentials => panel_calc_potentials

            ! Velocities
            procedure :: assemble_v_s_S_space => panel_assemble_v_s_S_space
            procedure :: assemble_v_d_M_space => panel_assemble_v_d_M_space
            procedure :: calc_velocity_influences => panel_calc_velocity_influences
            procedure :: calc_velocities => panel_calc_velocities

            ! Quantities for source and doublet distributions
            procedure :: get_source_strengths => panel_get_source_strengths
            procedure :: get_doublet_strengths => panel_get_doublet_strengths
            procedure :: get_source_parameters => panel_get_source_parameters
            procedure :: get_doublet_parameters => panel_get_doublet_parameters

            ! Surface properties
            procedure :: get_velocity_jump => panel_get_velocity_jump
            procedure :: get_velocity => panel_get_velocity
            procedure :: get_quadratic_pressure_params => panel_get_quadratic_pressure_params
            procedure :: get_avg_pressure_coef => panel_get_avg_pressure_coef
            procedure :: get_moment_about_centroid => panel_get_moment_about_centroid

    end type panel

    
contains


    subroutine panel_init_3(this, v1, v2, v3, index, in_wake)
        ! Initializes a 3-panel

        implicit none

        class(panel),intent(inout) :: this
        type(vertex),intent(inout),target :: v1, v2, v3
        integer,intent(in) :: index
        logical,intent(in),optional :: in_wake

        integer :: i

        ! Set number of sides
        this%N = 3

        ! Allocate vertex arrays
        allocate(this%vertices(this%N))

        ! Assign vertex pointers
        this%vertices(1)%ptr => v1
        this%vertices(2)%ptr => v2
        this%vertices(3)%ptr => v3

        ! Store the index of the panel
        this%index = index

        ! Store that this panel is attached to its vertices
        do i=1,this%N
            call this%vertices(i)%ptr%panels%append(this%index)
            call this%vertices(i)%ptr%panels_not_across_wake_edge%append(this%index)
        end do

        ! Initialize a few things
        this%abutting_panels = 0
        this%i_top_parent = 0
        this%i_bot_parent = 0
        this%N_discont_edges = 0
        if (present(in_wake)) then
            this%in_wake = in_wake
            this%has_sources = .not. in_wake
        else
            this%in_wake = .false.
            this%has_sources = .true.
        end if
        allocate(this%edge_is_discontinuous(3), source=.false.)

        ! Calculate panel geometries only dependent upon vertex locations (nothing flow-dependent at this point)
        call this%calc_derived_geom()

    end subroutine panel_init_3


    subroutine panel_calc_derived_geom(this)
        ! Initializes geometry based on the location of the vertices.
        ! Should be called when panel geometry is updated.

        implicit none

        class(panel),intent(inout) :: this

        ! Calculate midpoints

        ! Calculate normal vec
        call this%calc_normal()

        ! Calculate area
        call this%calc_area()

        ! Calculate centroid
        call this%calc_centroid()

        ! Calculate ledge vectors
        call this%calc_g_edge_vectors()

    end subroutine  panel_calc_derived_geom


    subroutine panel_calc_normal(this)

        implicit none

        class(panel),intent(inout) :: this

        real,dimension(3) :: d1, d2

        ! Get two edge vectors
        d1 = this%get_vertex_loc(2)-this%get_vertex_loc(1)
        d2 = this%get_vertex_loc(3)-this%get_vertex_loc(2)

        ! Find normal
        this%n_g = cross(d1, d2)
        this%n_g = this%n_g/norm2(this%n_g)

    end subroutine panel_calc_normal


    subroutine panel_calc_area(this)

        implicit none

        class(panel),intent(inout) :: this
        real,dimension(3) :: d1, d2

        ! 3-sided panel
        if (this%N == 3) then

            ! Get side vectors
            d1 = this%get_vertex_loc(2)-this%get_vertex_loc(1)
            d2 = this%get_vertex_loc(3)-this%get_vertex_loc(2)

            ! Calculate area from cross product
            this%A = 0.5*norm2(cross(d1, d2))

            ! Check for zero area
            if (this%A < 1.e-12) then
                write(*,*) "!!! Panel", this%index, "has zero area. Quitting..."
                write(*,*) "!!! Vertex 1 (", this%get_vertex_index(1), "): ", this%get_vertex_loc(1)
                write(*,*) "!!! Vertex 2 (", this%get_vertex_index(2), "): ", this%get_vertex_loc(2)
                write(*,*) "!!! Vertex 3 (", this%get_vertex_index(3), "): ", this%get_vertex_loc(3)
                stop
            end if

        end if

    end subroutine panel_calc_area


    subroutine panel_calc_centroid(this)

        implicit none

        class(panel),intent(inout) :: this
        real,dimension(3) :: sum
        integer :: i

        ! Get average of corner points
        sum = 0.
        do i=1,this%N
            sum = sum + this%get_vertex_loc(i)
        end do

        ! Set centroid
        this%centr = sum/this%N

    end subroutine panel_calc_centroid


    subroutine panel_calc_g_edge_vectors(this)

        implicit none

        class(panel),intent(inout) :: this

        real,dimension(3) :: d_g, t_hat_g
        integer :: i, i_next

        ! Allocate memory
        allocate(this%n_hat_g(3,this%N))

        ! Loop through edges
        do i=1,this%N

            i_next = mod(i, this%N)+1

            ! Calculate edge vector based on index
            d_g = this%get_vertex_loc(i_next)-this%get_vertex_loc(i)

            ! Calculate tangent in global coords
            t_hat_g = d_g/norm2(d_g)

            ! Calculate edge outward normal
            this%n_hat_g(:,i) = cross(t_hat_g, this%n_g)

        end do
    
    end subroutine panel_calc_g_edge_vectors


    function panel_get_characteristic_length(this) result(l)
        ! Returns the square root of the panel area

        implicit none
        
        class(panel),intent(in) :: this

        real :: l

        l = sqrt(this%A)
        
    end function panel_get_characteristic_length


    subroutine panel_init_with_flow(this, freestream, mirrored, mirror_plane)

        implicit none

        class(panel),intent(inout) :: this
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirrored
        integer,intent(in) :: mirror_plane

        ! Calculate transforms
        call this%calc_g_to_ls_transform(freestream)

        ! Calculate properties dependent on the transforms
        call this%calc_ls_edge_vectors(freestream)

        ! Calculate mirrored properties
        if (mirrored) then
            call this%init_mirror(freestream, mirror_plane)
        end if

    end subroutine panel_init_with_flow


    subroutine panel_calc_g_to_ls_transform(this, freestream)
        ! Calculates the necessary transformations to move from global to local, scaled coordinates (Eq. (E.0.1) in Epton and Magnus)

        implicit none

        class(panel),intent(inout) :: this
        type(flow),intent(in) :: freestream

        real,dimension(3) :: u0, v0
        real,dimension(3,3) :: B_mat_ls
        real :: x, y
        integer :: i, rs

        ! Get in-panel basis vectors
        if (abs(abs(inner(this%n_g, freestream%c_hat_g)) - 1.) < 1e-12) then ! Check the freestream isn't aligned with the normal vector
            v0 = this%get_vertex_loc(2)-this%get_vertex_loc(1)
        else
            v0 = cross(this%n_g, freestream%c_hat_g)
        end if
        v0 = v0/norm2(v0)
        u0 = cross(v0, this%n_g)
        u0 = u0/norm2(u0)

        ! Calculate compressible parameters
        this%nu_g = matmul(freestream%B_mat_g, this%n_g)
        x = inner(this%n_g, this%nu_g)

        ! Check for Mach-inclined panels
        if (freestream%supersonic .and. abs(x) < 1.e-12) then
            write(*,*) "!!! Panel", this%index, "is Mach-inclined, which is not allowed. Quitting..."
            stop
        end if

        ! Calculate panel inclination indicator (E&M Eq. (E.3.16b))
        this%r = int(sign(1., x)) ! r = -1 -> superinclined, r = 1 -> subinclined

        ! Check for Mach-inclined panels
        if (this%r == -1) then
            write(*,*) "!!! Panel", this%index, "is superinclined, which is not allowed. Quitting..."
            stop
        end if

        ! Other inclination parameters
        rs = int(this%r*freestream%s)

        ! Calculate transformation
        y = 1./sqrt(abs(x))
        this%A_g_to_ls(1,:) = y*matmul(freestream%C_mat_g, u0)
        this%A_g_to_ls(2,:) = rs/freestream%B*matmul(freestream%C_mat_g, v0)
        this%A_g_to_ls(3,:) = freestream%B*y*this%n_g

        ! Check determinant
        x = det3(this%A_g_to_ls)
        if (abs(x - freestream%B*freestream%B) > 1.e-10) then
            write(*,*) "!!! Calculation of local scaled coordinate transform failed. Quitting..."
            stop
        end if

        ! Calculate inverse
        if (freestream%M_inf == 0.) then
            this%A_ls_to_g = transpose(this%A_g_to_ls)
        else
            call matinv(3, this%A_g_to_ls, this%A_ls_to_g)
        end if

        ! Calculate Jacobian
        this%J = 1./(freestream%B*sqrt(abs(1.-freestream%M_inf**2*inner(freestream%c_hat_g, this%n_g)**2)))

        ! Transform vertex coords to ls
        allocate(this%vertices_ls(2,this%N))
        do i=1,this%N
            this%vertices_ls(:,i) = matmul(this%A_g_to_ls(1:2,:), this%get_vertex_loc(i)-this%centr)
        end do

        ! Calculate local scaled metric matrices
        B_mat_ls = 0.
        B_mat_ls(1,1) = freestream%B**2*rs
        B_mat_ls(2,2) = freestream%B**2
        B_mat_ls(3,3) = freestream%B**2*this%r

        ! Check calculation (E&M Eq. (E.2.19))
        if (any(abs(B_mat_ls - matmul(this%A_g_to_ls, matmul(freestream%B_mat_g, transpose(this%A_g_to_ls)))) > 1e-10)) then
            write(*,*) "!!! Calculation of local scaled coordinate transform failed. Quitting..."
            stop
        end if
    
    end subroutine panel_calc_g_to_ls_transform


    subroutine panel_calc_ls_edge_vectors(this, freestream)

        implicit none

        class(panel),intent(inout) :: this
        type(flow),intent(in) :: freestream

        real,dimension(2) :: d_ls
        real,dimension(:,:),allocatable :: t_hat_ls
        integer :: i, i_next

        ! Allocate memory
        allocate(t_hat_ls(2,this%N))
        allocate(this%n_hat_ls(2,this%N))
        allocate(this%b(this%N))
        allocate(this%b_mir(this%N)) ! This needs to be initialized here because of some DoD checks. It will have no effect.
        allocate(this%sqrt_b(this%N))

        ! Loop through edges
        do i=1,this%N

            i_next = mod(i, this%N)+1

            ! Calculate tangent in local scaled coords 
            d_ls = this%vertices_ls(:,i_next) - this%vertices_ls(:,i)
            t_hat_ls(:,i) = d_ls/norm2(d_ls)

        end do

        ! Calculate edge normal in local scaled coords E&M Eq. (J.6.45)
        this%n_hat_ls(1,:) = t_hat_ls(2,:)
        this%n_hat_ls(2,:) = -t_hat_ls(1,:)

        ! Calculate edge parameter (Ehlers Eq. (E14))
        ! This really only matters for subinclined, supersonic panels
        ! But we set defaults for the other cases to make unified calcs work
        if (freestream%supersonic) then
            if (this%r > 0) then
                this%b = (this%n_hat_ls(1,:) - this%n_hat_ls(2,:))*(this%n_hat_ls(1,:) + this%n_hat_ls(2,:))
                this%sqrt_b = sqrt(abs(this%b))
            else
                this%b = 1.
                this%sqrt_b = 1.
            end if
        else
            this%b = -1.
            this%sqrt_b = 1.
        end if
    
    end subroutine panel_calc_ls_edge_vectors


    subroutine panel_set_distribution(this, order, body_panels, body_verts, mirror_needed, mirror_plane)
        ! Sets up the singularity distribution for this panel

        implicit none
        
        class(panel),intent(inout) :: this
        integer,intent(in) :: order
        type(panel),dimension(:),allocatable,intent(in) :: body_panels
        type(vertex),dimension(:),allocatable,intent(in) :: body_verts
        logical,intent(in) :: mirror_needed
        integer,intent(in) :: mirror_plane

        ! Store order, while forcing wake panels to be lower-order
        if (this%in_wake) then
            this%order = 1
            this%has_sources = .false. ! A wake panel also will not have sources
        else
            this%order = order
        end if

        ! A higher-order panel with 3 discontinuous edges is just a lower-order panel
        if (this%N_discont_edges == 3 .and. this%order == 2) this%order = 1 

        ! Get dimension of {mu} and {sigma} (parameter space), and {M} and {S} (strength space)
        if (this%order == 1) then
            this%mu_dim = 3
            this%M_dim = 3
            this%sigma_dim = 1
            this%S_dim = 1
        else
            this%mu_dim = 6
            this%M_dim = 6 - this%N_discont_edges
            this%sigma_dim = 3
            this%S_dim = 4 - this%N_discont_edges
        end if

        ! Set up doublet distribution
        call this%set_doublet_verts(body_panels, size(body_verts))
        call this%calc_M_mu_transform(body_verts, .false., mirror_plane)

        ! Set up source distribution
        if (this%has_sources) then
            call this%set_source_panels()
            call this%calc_S_sigma_transform(body_panels, .false., mirror_plane)
        end if

        ! Set up transformations for mirrored panels
        if (mirror_needed) then
            call this%calc_M_mu_transform(body_verts, .true., mirror_plane)
            if (this%has_sources) call this%calc_S_sigma_transform(body_panels, .true., mirror_plane)
        end if

        ! Calculate quadratic pressure integrals
        if (this%order == 2) then
            call this%calc_C_integrals()
            if (mirror_needed) call this%calc_mirrored_C_integrals()
        end if
        
    end subroutine panel_set_distribution


    subroutine panel_set_doublet_verts(this, body_panels, N_body_verts)
        ! Sets up the arrays of vertex indices which set the influence for this panel

        class(panel),intent(inout) :: this
        type(panel),dimension(:),allocatable,intent(in) :: body_panels
        integer,intent(in) :: N_body_verts

        integer :: i, j, N_body_panels

        ! Get number of panels
        N_body_panels = size(body_panels)

        ! Wake panel (always linear)
        if (this%in_wake) then

            ! Allocate space
            allocate(this%i_vert_d(this%M_dim*2))

            ! Get top and bottom vertices on panel
            do i=1,this%N
                this%i_vert_d(i) = this%vertices(i)%ptr%top_parent
                this%i_vert_d(i+this%M_dim) = this%vertices(i)%ptr%bot_parent
            end do

        ! Non-wake panel
        else

            ! Allocate space
            allocate(this%i_vert_d(this%M_dim))

            ! This panel
            do i=1,this%N
                this%i_vert_d(i) = this%get_vertex_index(i)
            end do

            ! Neighbors (if needed)
            if (this%order == 2) then
                j = 4
                do i=1,this%N

                    ! Check this edge is not discontinuous
                    if (.not. this%edge_is_discontinuous(i)) then

                        ! Check for neighbor that is a mirror
                        if (this%abutting_panels(i) > N_body_panels) then
                            this%i_vert_d(j) = this%get_opposite_vertex(this%get_vertex_index(i), &
                                                                        this%get_vertex_index(mod(i, this%N)+1)) + N_body_verts

                        ! Neighbor is not a mirror
                        else
                            this%i_vert_d(j) = body_panels(this%abutting_panels(i))%get_opposite_vertex(this%get_vertex_index(i), &
                                                                                           this%get_vertex_index(mod(i, this%N)+1))
                        end if
                        j = j + 1
                    end if
                end do
            end if

        end if

    end subroutine panel_set_doublet_verts


    subroutine panel_set_source_panels(this)
        ! Sets the panel indices which influence this panel's source distribution

        implicit none
        
        class(panel),intent(inout) :: this

        integer :: i, j

        allocate(this%i_panel_s(this%S_dim))

        ! This panel is first
        this%i_panel_s(1) = this%index

        ! Loop through neighbors for linear distribution
        if (this%order == 2) then
            j = 2
            do i=1,this%N
                if (.not. this%edge_is_discontinuous(i)) then ! Skip neighbors across discontinuous edges
                    this%i_panel_s(j) = this%abutting_panels(i)
                    j = j + 1
                end if
            end do
        end if
        
    end subroutine panel_set_source_panels


    subroutine panel_calc_M_mu_transform(this, body_verts, calc_mirror, mirror_plane)
        ! Calculates the transformation from M space to mu space
        ! calc_mirror tells whether the transformation for this panel or its mirror needs to be calculated

        implicit none

        class(panel),intent(inout) :: this
        type(vertex),dimension(:),allocatable,intent(in) :: body_verts
        logical,intent(in) :: calc_mirror
        integer,intent(in) :: mirror_plane

        real,dimension(:,:),allocatable :: S_mu, S_mu_inv, M_mat, E_mat, EE_inv, T_mu
        real,dimension(:),allocatable :: M_row
        integer :: i, j, N_body_verts
        real,dimension(3) :: P_g, P_ls

        ! Get number of vertices
        N_body_verts = size(body_verts)

        ! Set up S_mu
        allocate(S_mu(this%mu_dim, this%mu_dim))

        ! Constant
        S_mu(:,1) = 1.

        ! x and y from this panel
        if (calc_mirror) then
            S_mu(1:3,2) = this%vertices_ls_mir(1,:)
            S_mu(1:3,3) = this%vertices_ls_mir(2,:)
        else
            S_mu(1:3,2) = this%vertices_ls(1,:)
            S_mu(1:3,3) = this%vertices_ls(2,:)
        end if

        ! Add in quadratic contributions
        if (this%order == 2) then

            ! x and y from edge midpoints
            S_mu(4:6,2) = 0.5*(S_mu(1:3,2) + cshift(S_mu(1:3,2), 1))
            S_mu(4:6,3) = 0.5*(S_mu(1:3,3) + cshift(S_mu(1:3,3), 1))

            ! x^2
            S_mu(:,4) = 0.5*S_mu(:,2)**2

            ! xy
            S_mu(:,5) = S_mu(:,2)*S_mu(:,3)

            ! y^2
            S_mu(:,6) = 0.5*S_mu(:,3)**2

        end if

        ! Invert S_mu
        allocate(S_mu_inv(this%mu_dim, this%mu_dim))
        call matinv(this%mu_dim, S_mu, S_mu_inv)

        ! Store inverse of S_mu
        if (this%order == 2) then
            if (calc_mirror) then
                allocate(this%S_mu_inv_mir, source=S_mu_inv)
            else
                allocate(this%S_mu_inv, source=S_mu_inv)
            end if
        end if

        ! Allocate transformation matrix
        allocate(T_mu(this%mu_dim, this%M_dim))

        ! For a linear distribution, that's all that's needed
        if (this%order == 1) then
            T_mu = S_mu_inv

        ! For a quadratic distribution, we need to build the M matrix as well
        else

            ! Allocate necessary matrices
            allocate(M_mat(this%mu_dim, this%M_dim), source=0.)
            allocate(M_row(4))
            allocate(EE_inv(4,4))

            ! Upper-left identity
            do i=1,3
                M_mat(i,i) = 1.
            end do

            ! Initialize E matrix
            allocate(E_mat(4, this%mu_dim), source=0.)
            E_mat(1:3,:) = S_mu(1:3,:)

            ! Loop through edges to skip discontinuous edges
            j = 4
            do i=1,3

                ! If the edge is discontinuous, then the midpoint strength is simply the average of the endpoint strengths
                if (this%edge_is_discontinuous(i)) then

                    ! Average of neighboring vertices
                    M_mat(i+3,i) = 0.5
                    M_mat(i+3,modulo(i,3)+1) = 0.5

                ! If the edge is not discontinuous, then build up the underdetermined least-squares fit
                else

                    ! Get opposite vertex location
                    if (calc_mirror) then
                        if (this%i_vert_d(j) > N_body_verts) then
                            P_g = body_verts(this%i_vert_d(j) - N_body_verts)%loc
                        else
                            P_g = mirror_across_plane(body_verts(this%i_vert_d(j))%loc, mirror_plane)
                        end if
                        P_ls = matmul(this%A_g_to_ls_mir, P_g - this%centr_mir)
                    else
                        if (this%i_vert_d(j) > N_body_verts) then
                            P_g = mirror_across_plane(body_verts(this%i_vert_d(j) - N_body_verts)%loc, mirror_plane)
                        else
                            P_g = body_verts(this%i_vert_d(j))%loc
                        end if
                        P_ls = matmul(this%A_g_to_ls, P_g - this%centr)
                    end if

                    ! Add to E matrix
                    E_mat(4,1) = 1.
                    E_mat(4,2) = P_ls(1)
                    E_mat(4,3) = P_ls(2)
                    E_mat(4,4) = 0.5*P_ls(1)**2
                    E_mat(4,5) = P_ls(1)*P_ls(2)
                    E_mat(4,6) = 0.5*P_ls(2)**2

                    ! Create pseudoinverse
                    call matinv(4, matmul(E_mat, transpose(E_mat)), EE_inv)

                    ! Add to M matrix
                    M_row = matmul(S_mu(i+3,:), matmul(transpose(E_mat), EE_inv))
                    M_mat(i+3,1:3) = M_row(1:3)
                    M_mat(i+3,j) = M_row(4)

                    ! Increment column
                    j = j + 1

                end if
            end do

            ! Calculate transformation
            T_mu = matmul(S_mu_inv, M_mat)

        end if
        
        ! Store
        if (calc_mirror) then
            allocate(this%T_mu_mir(this%mu_dim, this%M_dim))
            this%T_mu_mir = T_mu
        else
            allocate(this%T_mu(this%mu_dim, this%M_dim))
            this%T_mu = T_mu
        end if

    end subroutine panel_calc_M_mu_transform


    subroutine panel_calc_S_sigma_transform(this, body_panels, calc_mirror, mirror_plane)
        ! Calculates the transformation from S space to sigma space
        ! calc_mirror tells whether the transformation for this panel or its mirror needs to be calculated

        implicit none
        
        class(panel), intent(inout) :: this
        type(panel),dimension(:),allocatable,intent(in) :: body_panels
        logical,intent(in) :: calc_mirror
        integer,intent(in) :: mirror_plane

        real,dimension(:,:),allocatable :: S_sigma, SS_inv, A_mat, SA_inv, T_sigma
        integer :: i
        real,dimension(3) :: P_g, P_ls

        ! Linear (not needed for constant-strength source panels)
        if (this%order == 2) then

            ! Allocate space
            allocate(S_sigma(this%S_dim, this%sigma_dim))

            ! Influence of this panel
            S_sigma(1,:) = (/1., 0., 0./)

            ! Influence of neighboring panels
            do i=2,this%S_dim

                ! Get coordinates
                if (calc_mirror) then
                    if (this%i_panel_s(i) > size(body_panels)) then
                        P_g = body_panels(this%i_panel_s(i)-size(body_panels))%centr
                    else
                        P_g = body_panels(this%i_panel_s(i))%centr_mir
                    end if
                    P_ls = matmul(this%A_g_to_ls_mir, P_g - this%centr_mir)
                else
                    if (this%i_panel_s(i) > size(body_panels)) then
                        P_g = mirror_across_plane(body_panels(this%i_panel_s(i)-size(body_panels))%centr, mirror_plane)
                    else
                        P_g = body_panels(this%i_panel_s(i))%centr
                    end if
                    P_ls = matmul(this%A_g_to_ls, P_g - this%centr)
                end if

                ! Parse
                S_sigma(i,1) = 1.
                S_sigma(i,2) = P_ls(1)
                S_sigma(i,3) = P_ls(2)
            end do

            ! Invert based on number of source strengths
            allocate(T_sigma(this%sigma_dim, this%S_dim), source=0.)
            select case (this%S_dim)

            ! For overdetermined, do pseudoinverse using least-squares
            case (4)

                ! Method that does not force sigma_0 = sigma_i
                if (.not. force_sigma_match) then
                    allocate(SS_inv(this%sigma_dim, this%sigma_dim))
                    call matinv(3, matmul(transpose(S_sigma), S_sigma), SS_inv)
                    T_sigma = matmul(SS_inv, transpose(S_sigma))

                ! Method that does force sigma_0 = sigma_i
                else
                    allocate(SS_inv(this%sigma_dim-1, this%sigma_dim-1))
                    call matinv(2, matmul(transpose(S_sigma(2:4,2:3)), S_sigma(2:4,2:3)), SS_inv)
                    allocate(A_mat(3,4), source=0.)
                    A_mat(:,1) = -1.
                    A_mat(1,2) = 1.
                    A_mat(2,3) = 1.
                    A_mat(3,4) = 1.
                    T_sigma(1,1) = 1.
                    T_sigma(2:3,:) = matmul(SS_inv, matmul(transpose(S_sigma(2:4,2:3)), A_mat))
                end if

            ! For well-posed, do standard inverse
            case (3)
                call matinv(this%S_dim, S_sigma, T_sigma)

            ! Two discontinuous edges
            case (2)

                ! Build A matrix
                allocate(A_mat(this%sigma_dim, this%S_dim), source=0.)
                A_mat(1,1) = 1.

                ! Determine which parameter to eliminate
                if (abs(P_ls(1)) > abs(P_ls(2))) then
                    A_mat(2,2) = 1.
                    A_mat(3,2) = P_ls(2)/P_ls(1)
                else
                    A_mat(2,2) = P_ls(1)/P_ls(2)
                    A_mat(3,2) = 1.
                end if

                ! Get inverse
                allocate(SA_inv(this%S_dim,this%S_dim))
                call matinv(this%S_dim, matmul(S_sigma, A_mat), SA_inv)
                T_sigma = matmul(A_mat, SA_inv)

            end select

            ! Store
            if (calc_mirror) then
                allocate(this%T_sigma_mir(this%sigma_dim, this%S_dim))
                this%T_sigma_mir = T_sigma
            else
                allocate(this%T_sigma(this%sigma_dim, this%S_dim))
                this%T_sigma = T_sigma
            end if

        end if
        
    end subroutine panel_calc_S_sigma_transform


    subroutine panel_init_mirror(this, freestream, mirror_plane)

        implicit none

        class(panel),intent(inout) :: this
        type(flow),intent(in) :: freestream
        integer,intent(in) :: mirror_plane

        integer :: i

        ! Calculate mirrored normal vector
        this%n_g_mir = mirror_across_plane(this%n_g, mirror_plane)

        ! Calculate mirrored centroid
        this%centr_mir = mirror_across_plane(this%centr, mirror_plane)

        ! Calculate mirrored g to ls transform
        call this%calc_mirrored_g_to_ls_transform(freestream, mirror_plane)

        ! Calculate mirrored edge vectors
        ! Global
        allocate(this%n_hat_g_mir(3,this%N))
        do i=1,this%N
            this%n_hat_g_mir(:,i) = mirror_across_plane(this%n_hat_g(:,i), mirror_plane)
        end do

        ! Local-scaled
        call this%calc_mirrored_edge_vectors(freestream)

    end subroutine panel_init_mirror


    subroutine panel_calc_mirrored_g_to_ls_transform(this, freestream, mirror_plane)

        implicit none

        class(panel),intent(inout) :: this
        type(flow),intent(in) :: freestream
        integer,intent(in) :: mirror_plane

        real,dimension(3) :: u0, v0
        real :: x, y
        integer :: i, rs

        ! Get in-panel basis vectors
        if (abs(abs(inner(this%n_g_mir, freestream%c_hat_g)) - 1.) < 1e-12) then ! Check the freestream isn't aligned with the normal vector
            v0 = mirror_across_plane(this%get_vertex_loc(2) - this%get_vertex_loc(1), mirror_plane)
        else
            v0 = cross(this%n_g_mir, freestream%c_hat_g)
        end if
        v0 = v0/norm2(v0)
        u0 = cross(v0, this%n_g_mir)
        u0 = u0/norm2(u0)

        ! Calculate compressible parameters
        this%nu_g_mir = matmul(freestream%B_mat_g, this%n_g_mir)
        x = inner(this%n_g_mir, this%nu_g_mir)

        ! Calculate panel inclination indicator (E&M Eq. (E.3.16b))
        this%r_mir = int(sign(1., x)) ! r=-1 -> superinclined, r=1 -> subinclined

        ! Other inclination parameters
        rs = int(this%r*freestream%s)

        ! Calculate transformation
        y = 1./sqrt(abs(x))
        this%A_g_to_ls_mir(1,:) = y*matmul(freestream%C_mat_g, u0)
        this%A_g_to_ls_mir(2,:) = rs/freestream%B*matmul(freestream%C_mat_g, v0)
        this%A_g_to_ls_mir(3,:) = freestream%B*y*this%n_g_mir

        ! Check determinant
        x = det3(this%A_g_to_ls_mir)
        if (abs(x-freestream%B**2) >= 1e-10) then
            write(*,*) "!!! Calculation of mirrored local scaled coordinate transform failed. Quitting..."
            stop
        end if

        ! Calculate inverse
        if (freestream%M_inf == 0.) then
            this%A_ls_to_g_mir = transpose(this%A_g_to_ls_mir)
        else
            call matinv(3, this%A_g_to_ls_mir, this%A_ls_to_g_mir)
        end if

        ! Calculate Jacobian
        this%J_mir = 1./(freestream%B*sqrt(abs(1.-freestream%M_inf**2*inner(freestream%c_hat_g, this%n_g_mir)**2)))

        ! Transform vertex coords to ls
        allocate(this%vertices_ls_mir(2,this%N))
        do i=1,this%N
            this%vertices_ls_mir(:,i) = matmul(this%A_g_to_ls_mir(1:2,:), &
                                               mirror_across_plane(this%get_vertex_loc(i), mirror_plane)-this%centr_mir)
        end do
    
    end subroutine panel_calc_mirrored_g_to_ls_transform


    subroutine panel_calc_mirrored_edge_vectors(this, freestream)

        implicit none

        class(panel),intent(inout) :: this
        type(flow),intent(in) :: freestream

        real,dimension(2) :: d_ls
        real,dimension(:,:),allocatable :: t_hat_ls_mir
        integer :: i, i_next

        ! Allocate memory
        allocate(t_hat_ls_mir(2,this%N))
        allocate(this%n_hat_ls_mir(2,this%N))
        allocate(this%sqrt_b_mir(this%N))

        ! Loop through edges
        do i=1,this%N

            i_next = mod(i, this%N)+1

            ! Calculate tangent in local scaled coords 
            ! Direction is flipped so that we're still going counter-clockwise about the panel
            d_ls = this%vertices_ls_mir(:,i) - this%vertices_ls_mir(:,i_next)
            t_hat_ls_mir(:,i) = d_ls/norm2(d_ls)

        end do

        ! Calculate edge normal in local scaled coords E&M Eq. (J.6.45)
        this%n_hat_ls_mir(1,:) = t_hat_ls_mir(2,:)
        this%n_hat_ls_mir(2,:) = -t_hat_ls_mir(1,:)

        ! Calculate edge parameter (Ehlers Eq. (E14))
        if (freestream%supersonic) then
            if (this%r_mir > 0) then
                this%b_mir = (this%n_hat_ls_mir(1,:) - this%n_hat_ls_mir(2,:))*(this%n_hat_ls_mir(1,:) + this%n_hat_ls_mir(2,:))
                this%sqrt_b_mir = sqrt(abs(this%b_mir))
            else
                this%b_mir = 1.
                this%sqrt_b_mir = 1.
            end if
        else
            this%b_mir = -1.
            this%sqrt_b_mir = 1.
        end if
    
    end subroutine panel_calc_mirrored_edge_vectors


    subroutine panel_calc_C_integrals(this)
        ! Calculates the C integrals for integrating quadratic pressure distributions

        implicit none
        
        class(panel),intent(inout) :: this

        integer :: i, j, k, k_next
        real,dimension(3) :: d_eta, d_xi
        real,dimension(:,:,:,:),allocatable :: II

        integer :: Ni, Nj

        ! Set how many C integrals we need
        Ni = 3
        Nj = 3

        ! Calculate offsets for each edge
        do k=1,3

            ! Get endpoint index
            k_next = modulo(k, 3) + 1

            ! Get offset
            d_xi(k) = this%vertices_ls(1,k_next) - this%vertices_ls(1,k)
            d_eta(k) = this%vertices_ls(2,k_next) - this%vertices_ls(2,k)

        end do

        ! Initialize H integrals (remember II(i,j,0,:) = H(i,j,:))
        allocate(II(0:Ni+2,0:Nj,0:Ni+1,3))
        do i=0,Ni+2
            II(i,0,0,:) = 1./(i+1.)
        end do

        ! Calculate other H integrals using eta recursion
        do j=1,Nj
            do i=0,Ni-j
                II(i,j,0,:) = this%vertices_ls(2,:)*II(i,j-1,0,:) + d_eta*II(i+1,j-1,0,:)
            end do 
        end do

        ! Calculate I integrals using xi recursion
        do j=0,Nj
            do k=1,Ni-j
                do i=Ni-j,k,-1
                    II(i,j,k,:) = this%vertices_ls(1,:)*II(i-1,j,k-1,:) + d_xi*II(i,j,k-1,:)
                end do
            end do
        end do

        ! Calculate C integrals (remember G(i,j,:) = II(i,j,i,:))
        allocate(this%C(0:Ni,0:Nj))
        do i=0,Ni
            do j=0,Nj
                this%C(i,j) = sum(d_eta*II(i+1,j,i+1,:))/(i+1)
            end do
        end do
        
    end subroutine panel_calc_C_integrals


    subroutine panel_calc_mirrored_C_integrals(this)
        ! Calculates the C integrals for integrating quadratic pressure distributions for the mirrored panel

        implicit none
        
        class(panel),intent(inout) :: this

        integer :: i, j, k, k_next
        real,dimension(3) :: d_eta, d_xi
        real,dimension(:,:,:,:),allocatable :: II

        integer :: Ni, Nj

        ! Set how many C integrals we need
        Ni = 3
        Nj = 3

        ! Calculate offsets for each edge
        do k=1,3

            ! Get endpoint index
            k_next = modulo(k, 3) + 1

            ! Get offset
            d_xi(k_next) = this%vertices_ls_mir(1,k) - this%vertices_ls_mir(1,k_next)
            d_eta(k_next) = this%vertices_ls_mir(2,k) - this%vertices_ls_mir(2,k_next)

        end do

        ! Initialize H integrals (remember II(i,j,0,:) = H(i,j,:))
        allocate(II(0:Ni+2,0:Nj,0:Ni+1,3))
        do i=0,Ni+2
            II(i,0,0,:) = 1./(i+1.)
        end do

        ! Calculate other H integrals using eta recursion
        do j=1,Nj
            do i=0,Ni-j
                II(i,j,0,:) = this%vertices_ls_mir(2,:)*II(i,j-1,0,:) + d_eta*II(i+1,j-1,0,:)
            end do 
        end do

        ! Calculate I integrals using xi recursion
        do j=0,Nj
            do k=1,Ni-j
                do i=Ni-j,k,-1
                    II(i,j,k,:) = this%vertices_ls_mir(1,:)*II(i-1,j,k-1,:) + d_xi*II(i,j,k-1,:)
                end do
            end do
        end do

        ! Calculate C integrals (remember G(i,j,:) = II(i,j,i,:))
        allocate(this%C_mir(0:Ni,0:Nj))
        do i=0,Ni
            do j=0,Nj
                this%C_mir(i,j) = sum(d_eta*II(i+1,j,i+1,:))/(i+1)
            end do
        end do
        
    end subroutine panel_calc_mirrored_C_integrals


    function panel_get_vertex_loc(this, i) result(loc)

        implicit none

        class(panel),intent(in) :: this
        integer,intent(in) :: i
        real,dimension(3) :: loc

        loc = this%vertices(i)%ptr%loc

    end function panel_get_vertex_loc


    function panel_get_vertex_index(this, i) result(index)

        implicit none

        class(panel),intent(in) :: this
        integer,intent(in) :: i
        integer :: index

        index = this%vertices(i)%ptr%index

    end function panel_get_vertex_index


    function panel_touches_vertex(this, i) result(touches)

        implicit none

        class(panel),intent(in) :: this
        integer,intent(in) :: i
        logical :: touches
        integer :: j

        touches = .false.

        ! Loop through vertices
        do j=1,this%N

            ! Check index
            if (this%get_vertex_index(j) == i) then
                touches = .true.
                return
            end if

        end do

    end function panel_touches_vertex


    function panel_check_abutting_mirror_plane(this, N_panels, i_endpoints, edge_index) result(abuts)
        ! Tells whether this panel abuts the mirror plane.

        class(panel),intent(inout) :: this
        integer,intent(in) :: N_panels
        integer,dimension(2),intent(out) :: i_endpoints
        integer,intent(out) :: edge_index

        logical :: abuts

        logical :: already_found_vert_on_mirror_plane
        integer :: m, m1, temp

        ! Initialize checks
        already_found_vert_on_mirror_plane = .false.
        abuts = .false.

        ! Loop through vertices
        mirror_loop: do m=1,this%N

            ! Check if vertex is on the mirror plane
            if (this%vertices(m)%ptr%on_mirror_plane) then

                ! Previously found a vertex on mirror plane, so the panels are abutting
                if (already_found_vert_on_mirror_plane) then

                    abuts = .true.

                    ! Store the second shared vertex
                    i_endpoints(2) = this%get_vertex_index(m)

                    ! Check order
                    if (m1 == 1 .and. m == 3) then
                        temp = i_endpoints(1)
                        i_endpoints(1) = i_endpoints(2)
                        i_endpoints(2) = temp
                    end if

                    ! Store adjacent panel
                    if (m-m1 == 1) then
                        this%abutting_panels(m1) = this%index + N_panels
                        edge_index = m1
                    else
                        this%abutting_panels(m) = this%index + N_panels
                        edge_index = m
                    end if

                    return

                ! First vertex on the mirror plane
                else

                    already_found_vert_on_mirror_plane = .true.
                    i_endpoints(1) = this%get_vertex_index(m)
                    m1 = m

                end if
            end if

        end do mirror_loop
        
    end function panel_check_abutting_mirror_plane


    function panel_projection_inside(this, point, mirror_panel, mirror_plane) result(inside)
        ! Checks whether the given point, when projected into the plane of the panel, is inside the panel

        implicit none
        
        class(panel),intent(in) :: this
        real,dimension(3),intent(in) :: point
        logical,intent(in),optional :: mirror_panel
        integer,intent(in),optional :: mirror_plane

        logical :: inside

        real,dimension(3) :: d
        integer :: i
        real :: x
        logical :: mirrored

        if (present(mirror_panel)) then
            mirrored = mirror_panel
        else
            mirrored = .false.
        end if

        ! Loop through edges
        inside = .true.
        do i=1,this%N

            ! Shift origin to the edge and get inner product
            if (mirrored) then
                d = point - mirror_across_plane(this%get_vertex_loc(i), mirror_plane)
                x = inner(d, this%n_hat_g_mir(:,i))
            else
                d = point - this%get_vertex_loc(i)
                x = inner(d, this%n_hat_g(:,i))
            end if

            ! Check
            if (x >= 1.e-16) then
                inside = .false.
                return
            end if

        end do
        
    end function panel_projection_inside


    function panel_point_outside(this, point, mirror_panel, mirror_plane) result(outside)
        ! Tells whether the given point is above the panel and its projection is inside the surface of the panel

        implicit none
        
        class(panel),intent(in) :: this
        real,dimension(3),intent(in) :: point
        logical,intent(in) :: mirror_panel
        integer,intent(in) :: mirror_plane

        logical :: outside

        real :: h

        ! Get height above panel
        if (mirror_panel) then
            h = inner(point-this%centr_mir, this%n_g_mir)
        else
            h = inner(point-this%centr, this%n_g)
        end if

        ! If height is negative, we know this isn't outside the panel
        if (h < 0.) then
            outside = .false.

        ! Otherwise, it's dependent upon whether the projection is inside the panel surface
        else
            outside = this%projection_inside(point, mirror_panel, mirror_plane)
        end if
        
    end function panel_point_outside


    function panel_point_above(this, point, mirror_panel, mirror_plane) result(above)
        ! Tells whether the given point is above the panel

        implicit none
        
        class(panel),intent(in) :: this
        real,dimension(3),intent(in) :: point
        logical,intent(in) :: mirror_panel
        integer,intent(in) :: mirror_plane

        logical :: above

        real :: h

        ! Get height above panel
        if (mirror_panel) then
            h = inner(point-this%centr_mir, this%n_g_mir)
        else
            h = inner(point-this%centr, this%n_g)
        end if

        ! If height is negative, we know this isn't outside the panel
        if (h < 0.) then
            above = .false.
        else
            above = .true.
        end if
        
    end function panel_point_above


    function panel_line_passes_through(this, a, b, mirror_panel, mirror_plane, s_star) result(passes_through)
        ! Determines whether the line given by r(s) = a + s*b passes through the panel

        implicit none
        
        class(panel),intent(in) :: this
        real,dimension(3),intent(in) :: a, b
        logical,intent(in) :: mirror_panel
        integer,intent(in) :: mirror_plane
        real,intent(out) :: s_star

        logical :: passes_through

        real :: d
        real,dimension(3) :: loc

        ! Get denominator
        if (mirror_panel) then
            d = inner(b, this%n_g_mir)
        else
            d = inner(b, this%n_g)
        end if

        ! Check whether the line is parallel to the panel
        if (abs(d) < 1.e-16) then
            passes_through = .false.
            return
        end if

        ! Get s otherwise
        if (mirror_panel) then
            s_star = inner(this%centr_mir - a, this%n_g_mir)/d
        else
            s_star = inner(this%centr - a, this%n_g)/d
        end if

        ! Calculate intersection point
        loc = a + s_star*b

        ! Check whether the intersection point is inside the panel
        passes_through = this%projection_inside(loc, mirror_panel, mirror_plane)
        
    end function panel_line_passes_through


    function panel_get_corner_angle(this, vert_loc) result(angle)
        ! Calculates the angle of the corner at which the given vertex lies

        implicit none
        
        class(panel),intent(in) :: this
        real,dimension(3),intent(in) :: vert_loc

        real(16) :: angle

        integer :: i, i_prev

        ! Find the right corner
        do i=1,this%N

            ! Check vertex
            if (dist(this%get_vertex_loc(i), vert_loc) < 1.e-12) then

                ! Get previous edge index
                if (i == 1) then
                    i_prev = this%N
                else
                    i_prev = i-1
                end if

                ! Calculate angle
                angle = acos(inner(-this%n_hat_g(:,i), this%n_hat_g(:,i_prev)))
                return

            end if
        end do

        ! This vertex doesn't belong, so it has no angle
        angle = 0.
        
    end function panel_get_corner_angle


    function panel_get_weighted_normal_at_corner(this, vert_loc) result(n_weighted)
        ! Returns the panel normal weighted by the angle of the corner given

        implicit none
        
        class(panel),intent(in) :: this
        real,dimension(3),intent(in) :: vert_loc

        real(16),dimension(3) :: n_weighted

        real(16) :: W

        ! Get angle
        W = this%get_corner_angle(vert_loc)
    
        ! Apply weight
        n_weighted = this%n_g*W

    end function panel_get_weighted_normal_at_corner


    function panel_get_projection_onto_surface(this, v, mirror_panel) result(v_proj)
        ! Projects the given vector into the plane of the panel

        implicit none
        
        class(panel),intent(in) :: this
        real,dimension(3),intent(in) :: v
        logical,intent(in) :: mirror_panel

        real,dimension(3) :: v_proj
    
        ! Project
        if (mirror_panel) then
            v_proj = v - this%n_g_mir*inner(v, this%n_g_mir)
        else
            v_proj = v - this%n_g*inner(v, this%n_g)
        end if
        
    end function panel_get_projection_onto_surface


    function panel_get_opposite_vertex(this, i1, i2) result(i_opp)
        ! Returns the index of the vertex opposite of the two given

        implicit none
        
        class(panel),intent(in) :: this
        integer,intent(in) :: i1, i2

        integer :: i_opp
    
        integer :: i, j

        ! Return zero if we can't find it
        i_opp = 0

        ! Find the first vertex
        do i=1,3
            if (this%get_vertex_index(i) == i1) then

                ! Find the second vertex
                do j=1,3
                    if (j /= i .and. this%get_vertex_index(j) /= i2) then
                        i_opp = this%get_vertex_index(j)
                        exit
                    end if
                end do
                
                exit
            end if
        end do
        
    end function panel_get_opposite_vertex


    function panel_get_opposite_panel(this, i_vert) result(i_panel_opp)
        ! Returns the index of the panel abutting the edge opposite the given vertex

        implicit none
        
        class(panel),intent(in) :: this
        integer,intent(in) :: i_vert

        integer :: i_panel_opp
    
        integer :: i, j, ind, i_vert_for_panel

        ! Return zero if we can't find it
        i_panel_opp = 0

        ! Figure out which vertex this is for this panel
        do i=1,3
            if (i_vert == this%get_vertex_index(i)) then
                i_vert_for_panel = i
            end if
        end do

        ! Figure out opposite panel
        ind = mod(i_vert_for_panel, 3) + 1
        i_panel_opp = this%abutting_panels(ind)

    end function panel_get_opposite_panel


    function panel_get_opposite_edge(this, i_vert) result(i_edge_opp)
        ! Returns the index of the edge opposite the given vertex

        implicit none
        
        class(panel),intent(in) :: this
        integer,intent(in) :: i_vert

        integer :: i_edge_opp
    
        integer :: i, j, ind, i_vert_for_panel

        ! Return zero if we can't find it
        i_edge_opp = 0

        ! Figure out which vertex this is for this panel
        do i=1,3
            if (i_vert == this%get_vertex_index(i)) then
                i_vert_for_panel = i
            end if
        end do

        ! Figure out opposite panel
        ind = mod(i_vert_for_panel, 3) + 1
        i_edge_opp = this%edges(ind)

    end function panel_get_opposite_edge


    function panel_get_local_coords_of_point(this, P, mirrored) result(P_ls)
        ! Calculates the coordinates of the given point in local-scaled coordinates

        implicit none
        
        class(panel),intent(in) :: this
        real,dimension(3),intent(in) :: P
        logical,intent(in) :: mirrored

        real,dimension(3) :: P_ls

        if (mirrored) then
            P_ls = matmul(this%A_g_to_ls_mir, P-this%centr_mir)
        else
            P_ls = matmul(this%A_g_to_ls, P-this%centr)
        end if
        
    end function panel_get_local_coords_of_point


    subroutine panel_point_to_new_vertex(this, new_vertex)
        ! Updates the panel to point to this new vertex (assumed to be a copy of a current vertex)

        implicit none

        class(panel),intent(inout) :: this
        type(vertex),intent(in),target :: new_vertex
        integer :: i

        ! Loop through vertices
        do i=1,this%N

            ! Check the vertex locations
            if (dist(this%get_vertex_loc(i), new_vertex%loc) < 1e-12) then

                ! Update pointer
                this%vertices(i)%ptr => new_vertex

                return

            end if

        end do
    
    end subroutine panel_point_to_new_vertex


    function panel_check_dod(this, eval_point, freestream, verts_in_dod, mirror_panel, mirror_plane) result(dod_info)
        ! Determines how (if) this panel lies within the domain of dependence of the evaluation point

        implicit none

        class(panel),intent(in) :: this
        real,dimension(3),intent(in) :: eval_point
        type(flow),intent(in) :: freestream
        logical,dimension(:),intent(in) :: verts_in_dod
        logical,intent(in),optional :: mirror_panel
        integer,intent(in),optional :: mirror_plane

        type(dod) :: dod_info

        real,dimension(3) :: d, a, b, R_star, Q_end
        real,dimension(3,this%N) :: d_from_vert
        integer :: i, i_next
        real :: x, s_star
        logical :: mirrored, in_panel
        logical,dimension(3) :: these_verts_in_dod
        logical :: downstream

        ! Set default mirroring
        if (present(mirror_panel)) then
            mirrored = mirror_panel
        else
            mirrored = .false.
        end if

        ! First check the flow is supersonic
        if (freestream%supersonic) then

            ! Read in vertex information
            do i=1,this%N
                if (mirrored) then
                    these_verts_in_dod(i) = verts_in_dod(this%get_vertex_index(i)+size(verts_in_dod)/2)
                else
                    these_verts_in_dod(i) = verts_in_dod(this%get_vertex_index(i))
                end if
            end do

            ! If all the vertices are in, then the panel is totally in and we can be done
            if (all(these_verts_in_dod)) then
                dod_info%in_dod = .true.
                dod_info%edges_in_dod = .true.

            ! If it is not guaranteed to be totally in, then check all the edges
            else

                ! Get displacements from vertices
                do i=1,this%N
                    if (mirrored) then
                        d_from_vert(:,i) = eval_point - mirror_across_plane(this%get_vertex_loc(i), mirror_plane)
                    else
                        d_from_vert(:,i) = eval_point - this%get_vertex_loc(i)
                    end if
                end do

                ! Make sure we're downstream
                downstream = .false.
                do i=1,this%N

                    ! Get downstream distance
                    x = inner(d_from_vert(:,i), freestream%c_hat_g)

                    ! Check
                    downstream = x > 0. .or. downstream

                end do

                if (downstream) then

                    ! Check edges
                    do i=1,this%N

                        i_next = mod(i, this%N)+1

                        ! If if at least one endpoint is in, then the edge is in
                        if (these_verts_in_dod(i) .or. these_verts_in_dod(i_next)) then
                            dod_info%edges_in_dod(i) = .true.

                        ! If both aren't in, then the intersection will depend on the edge type
                        else

                            ! For a subsonic or sonic edge, both being out means the edge is out
                            if ((.not. mirrored .and. this%b(i) <= 0.) .or. (mirrored .and. this%b_mir(i) <= 0.)) then
                                dod_info%edges_in_dod(i) = .false.

                            ! For a supersonic edge, the edge can still intersect the DoD, so calculate the point of closest approach
                            else

                                ! Get end vertex and vector describing edge
                                if (mirrored) then
                                    Q_end = mirror_across_plane(this%get_vertex_loc(i_next), mirror_plane)
                                    d = Q_end - mirror_across_plane(this%get_vertex_loc(i), mirror_plane)
                                else
                                    Q_end = this%get_vertex_loc(i_next)
                                    d = Q_end - this%get_vertex_loc(i)
                                end if
                            
                                ! Calculate nondimensional location of the point of closest approach (E&M Eq. (J.3.39))
                                a = cross(freestream%c_hat_g, d)
                                b = cross(freestream%c_hat_g, -d_from_vert(:,i_next))
                                s_star = inner(a, b)/abs(inner(a, a))

                                ! Calculate point of closest approach
                                R_star = Q_end - s_star*d

                                ! Check if the point of closest approach is in the edge and in the DoD
                                if (s_star > 0. .and. s_star < 1.) then
                                    dod_info%edges_in_dod(i) = freestream%point_in_dod(R_star, eval_point)

                                ! If not, this edge is not in the DoD
                                else
                                    dod_info%edges_in_dod(i) = .false.

                                end if
                            end if
                        end if

                    end do

                    ! If any edge or vertex is in the DoD, then the panel is in
                    if (any(these_verts_in_dod) .or. any(dod_info%edges_in_dod)) then
                        dod_info%in_dod = .true.

                    ! If a superinclined panel has no edges or vertices in the DoD, check if the DoD is encompassed by the panel
                    else if (this%r < 0.) then

                        ! Get the projection of the evaluation point onto the panel in the direction of c_hat
                        if (mirrored) then
                            s_star = inner(-d_from_vert(:,1), this%n_g_mir) / inner(freestream%c_hat_g, this%n_g_mir)
                        else
                            s_star = inner(-d_from_vert(:,1), this%n_g) / inner(freestream%c_hat_g, this%n_g)
                        end if
                        R_star = eval_point + freestream%c_hat_g*s_star

                        ! See if the projected point is in the panel
                        if (mirrored) then
                            in_panel = this%projection_inside(R_star, mirror_panel, mirror_plane)
                        else
                            in_panel = this%projection_inside(R_star)
                        end if

                        ! Store information
                        dod_info%in_dod = in_panel

                    ! Not superinclined and no edges or vertices in. Not in.
                    else
                        dod_info%in_dod = .false.

                    end if

                ! Not downstream
                else
                    dod_info%edges_in_dod = .false.
                    dod_info%in_dod = .false.
                end if
            end if

        else

            ! Subsonic flow. DoD is everywhere. Life is easy.
            ! This shouldn't be necessary, but I'll keep it here for now.
            dod_info%in_dod = .true.
            dod_info%edges_in_dod = .true.

        end if
    
    end function panel_check_dod


    function panel_calc_basic_geom(this, eval_point, mirror_panel) result(geom)
        ! Initializes geometry common to the three panel types

        implicit none
        
        class(panel), intent(in) :: this
        real,dimension(3),intent(in) :: eval_point
        logical,intent(in) :: mirror_panel

        type(eval_point_geom) :: geom

        integer :: i

        ! Initialize
        call geom%init(eval_point, this%get_local_coords_of_point(eval_point, mirror_panel))

        ! Get edge normal vectors
        if (mirror_panel) then
            geom%v_xi = this%n_hat_ls_mir(1,:)
            geom%v_eta = this%n_hat_ls_mir(2,:)
        else
            geom%v_xi = this%n_hat_ls(1,:)
            geom%v_eta = this%n_hat_ls(2,:)
        end if

        ! Calculate vertex displacements
        do i=1,this%N
            if (mirror_panel) then
                geom%d_ls(:,i) = this%vertices_ls_mir(:,i) - geom%P_ls
            else
                geom%d_ls(:,i) = this%vertices_ls(:,i) - geom%P_ls
            end if
        end do
    
    end function panel_calc_basic_geom


    function panel_calc_subsonic_geom(this, eval_point, freestream, mirror_panel) result(geom)
        ! Calculates the geometric parameters necessary for calculating the influence of the panel at the given evaluation point in subsonic flow.

        implicit none

        class(panel),intent(in) :: this
        real,dimension(3),intent(in) :: eval_point
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel

        type(eval_point_geom) :: geom

        integer :: i, i_next
        real,dimension(this%N) :: dummy

        ! Initialize
        geom = this%calc_basic_geom(eval_point, mirror_panel)

        ! Calculate edge quantities
        do i=1,this%N

            ! Get index of end vertex
            i_next = mod(i, this%N) + 1

            ! Integration length on edge to start vertex
            geom%l1(i) = -geom%d_ls(1,i)*geom%v_eta(i) + geom%d_ls(2,i)*geom%v_xi(i)

            ! Integration length on edge to start vertex
            geom%l2(i) = -geom%d_ls(1,i_next)*geom%v_eta(i) + geom%d_ls(2,i_next)*geom%v_xi(i)

        end do

        ! Perpendicular distance in plane from evaluation point to edge
        geom%a = geom%d_ls(1,:)*geom%v_xi + geom%d_ls(2,:)*geom%v_eta

        ! Square of the perpendicular distance to edge
        geom%g2 = geom%a*geom%a + geom%h2

        ! Distance from evaluation point to end vertices
        geom%R1 = sqrt(geom%d_ls(1,:)*geom%d_ls(1,:) + geom%d_ls(2,:)*geom%d_ls(2,:) + geom%h2)
        geom%R2 = cshift(geom%R1, 1)

        ! Swap directions for mirror
        if (mirror_panel) then
            dummy = geom%l1
            geom%l1 = geom%l2
            geom%l2 = dummy

            dummy = geom%R1
            geom%R1 = geom%R2
            geom%R2 = dummy
        end if

        ! Difference in R
        geom%dR = geom%R2 - geom%R1

    end function panel_calc_subsonic_geom


    function panel_calc_supersonic_subinc_geom(this, eval_point, freestream, mirror_panel, dod_info) result(geom)
        ! Calculates the geometric parameters necessary for calculating the influence of the panel at the given evaluation point

        implicit none

        class(panel),intent(in) :: this
        real,dimension(3),intent(in) :: eval_point
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel
        type(dod),intent(in) :: dod_info
        type(eval_point_geom) :: geom

        real :: x
        integer :: i, i_next
        real :: dummy

        ! Initialize
        geom = this%calc_basic_geom(eval_point, mirror_panel)

        ! Loop through edges
        do i=1,this%N

            ! Check DoD
            if (dod_info%edges_in_dod(i)) then

                ! Get index of end vertex
                i_next = mod(i, this%N)+1

                ! Edge integration lengths
                geom%l1(i) = geom%v_eta(i)*geom%d_ls(1,i) + geom%v_xi(i)*geom%d_ls(2,i)
                geom%l2(i) = geom%v_eta(i)*geom%d_ls(1,i_next) + geom%v_xi(i)*geom%d_ls(2,i_next)

                ! Perpendicular in-plane distance
                geom%a(i) = geom%v_xi(i)*geom%d_ls(1,i) + geom%v_eta(i)*geom%d_ls(2,i)

                ! Perpendicular hyperbolic distance
                if (mirror_panel) then
                    geom%g2(i) = geom%a(i)**2 - this%b_mir(i)*geom%h2
                else
                    geom%g2(i) = geom%a(i)**2 - this%b(i)*geom%h2
                end if

                ! Hyperbolic radius to first vertex
                x = geom%d_ls(1,i)*geom%d_ls(1,i) - geom%d_ls(2,i)*geom%d_ls(2,i) - geom%h2
                if (x > 0. .and. geom%d_ls(1,i) < 0.) then
                    geom%R1(i) = sqrt(x)
                else
                    geom%l1(i) = -sqrt(abs(geom%g2(i)))
                    geom%R1(i) = 0.
                end if

                ! Hyperbolic radius to second vertex
                x = geom%d_ls(1,i_next)*geom%d_ls(1,i_next) - geom%d_ls(2,i_next)*geom%d_ls(2,i_next) - geom%h2
                if (x > 0. .and. geom%d_ls(1,i_next) < 0.) then
                    geom%R2(i) = sqrt(x)
                else
                    geom%l2(i) = sqrt(abs(geom%g2(i)))
                    geom%R2(i) = 0.
                end if

                ! Swap directions for mirror
                if (mirror_panel) then

                    ! Swap l1 and l2
                    ! The check is necessary because we set the sign of l1 and l2 in the case of R=0 based on which end each came from
                    dummy = geom%l1(i)
                    if (geom%R2(i) == 0.) then
                        geom%l1(i) = -geom%l2(i)
                    else
                        geom%l1(i) = geom%l2(i)
                    end if
                    if (geom%R1(i) == 0) then
                        geom%l2(i) = -dummy
                    else
                        geom%l2(i) = dummy
                    end if

                    ! Swap R1 and R2
                    dummy = geom%R1(i)
                    geom%R1(i) = geom%R2(i)
                    geom%R2(i) = dummy

                end if

            end if

        end do

        ! Difference in R
        geom%dR = geom%R2 - geom%R1

    end function panel_calc_supersonic_subinc_geom


    function panel_calc_supersonic_supinc_geom(this, eval_point, freestream, mirror_panel, dod_info) result(geom)
        ! Calculates the geometric parameters necessary for calculating the influence of the panel at the given evaluation point

        implicit none

        class(panel),intent(in) :: this
        real,dimension(3),intent(in) :: eval_point
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel
        type(dod),intent(in) :: dod_info
        type(eval_point_geom) :: geom

        real :: x
        integer :: i, i_next
        real :: dummy

        ! Initialize
        geom = this%calc_basic_geom(eval_point, mirror_panel)

        ! Loop through edges
        do i=1,this%N

            ! Check DoD
            if (dod_info%edges_in_dod(i)) then

                ! Get index of end vertex
                i_next = mod(i, this%N)+1

                ! Perpendicular in-plane distance
                geom%a(i) = geom%v_xi(i)*geom%d_ls(1,i) + geom%v_eta(i)*geom%d_ls(2,i)

                ! Edge integration lengths
                geom%l1(i) = -geom%v_eta(i)*geom%d_ls(1,i) + geom%v_xi(i)*geom%d_ls(2,i)
                geom%l2(i) = -geom%v_eta(i)*geom%d_ls(1,i_next) + geom%v_xi(i)*geom%d_ls(2,i_next)

                ! Hyperbolic radius to first vertex
                x = -geom%d_ls(1,i)*geom%d_ls(1,i) - geom%d_ls(2,i)*geom%d_ls(2,i) + geom%h2
                if (geom%h > 0. .and. x > 0.) then
                    geom%R1(i) = sqrt(x)
                else
                    geom%l1(i) = -1.
                    geom%R1(i) = 0.
                end if

                ! Hyperbolic radius to second vertex
                x = -geom%d_ls(1,i_next)*geom%d_ls(1,i_next) - geom%d_ls(2,i_next)*geom%d_ls(2,i_next) + geom%h2
                if (geom%h > 0. .and. x > 0.) then
                    geom%R2(i) = sqrt(x)
                else
                    geom%l2(i) = 1.
                    geom%R2(i) = 0.
                end if

                ! Swap directions for mirror
                if (mirror_panel) then

                    ! Swap l1 and l2
                    ! The check is necessary because we set the sign of l1 and l2 in the case of R=0 based on which end each came from
                    dummy = geom%l1(i)
                    if (geom%R2(i) == 0.) then
                        geom%l1(i) = -geom%l2(i)
                    else
                        geom%l1(i) = geom%l2(i)
                    end if
                    if (geom%R1(i) == 0) then
                        geom%l2(i) = -dummy
                    else
                        geom%l2(i) = dummy
                    end if

                    ! Swap R1 and R2
                    dummy = geom%R1(i)
                    geom%R1(i) = geom%R2(i)
                    geom%R2(i) = dummy

                end if

            end if

        end do

        ! Difference in R
        geom%dR = geom%R2 - geom%R1

    end function panel_calc_supersonic_supinc_geom


    function panel_EMNK(this, geom, M, N, K, mirror_panel) result(E)
        ! Calculates E(M,N,K)

        implicit none
        
        class(panel),intent(in) :: this
        type(eval_point_geom),intent(in) :: geom
        integer,intent(in) :: M, N, K
        logical,intent(in) :: mirror_panel

        real,dimension(this%N) :: E

        integer :: i, i_next
        real :: E1, E2

        ! Get displacements from vertices

        ! Loop through edges
        do i=1,this%N

            ! Get index of end vertex
            i_next = mod(i, this%N)+1

            ! Calculate E(M,N,K)
            if (geom%R1(i) == 0.) then
                E1 = 0.0
            else 
                if (mirror_panel) then
                    E1 = geom%d_ls(1,i_next)**(M-1) * geom%d_ls(2,i_next)**(N-1) / geom%R1(i)**K
                else
                    E1 = geom%d_ls(1,i)**(M-1) * geom%d_ls(2,i)**(N-1) / geom%R1(i)**K
                end if
            end if
            
            if (geom%R2(i) == 0.) then
                E2 = 0.0
            else 
                if (mirror_panel) then
                    E2 = geom%d_ls(1,i)**(M-1) * geom%d_ls(2,i)**(N-1) / geom%R2(i)**K
                else
                    E2 = geom%d_ls(1,i_next)**(M-1) * geom%d_ls(2,i_next)**(N-1) / geom%R2(i)**K
                end if
            end if

            E(i) = E2 - E1

        end do
        
    end function panel_EMNK


    subroutine panel_calc_basic_F_integrals_subsonic(this, geom, freestream, mirror_panel, int)
        ! Calculates the F integrals necessary for determining the influence of a triangular panel in subsonic flow.
        ! This is a pared-down version of the algorithm presented by Johnson (1980) Appendix D.3.

        implicit none

        class(panel),intent(in) :: this
        type(eval_point_geom),intent(in) :: geom
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel
        type(integrals),intent(inout) :: int

        integer :: i

        ! Loop through edges
        do i=1,this%N

            ! Calculate F(1,1,1)
            ! Within edge (Johnson Eq. (D.60))
            if (sign(1., geom%l1(i)) /= sign(1., geom%l2(i))) then

                ! Check for point on perimeter
                if(sqrt(geom%g2(i)) < 1e-12) then
                    write(*,*) "!!! Detected control point colinear with panel edge. Solution quality may be negatively affected."
                end if

                ! Calculate
                int%F111(i) = log( ( (geom%R1(i) - geom%l1(i)) * (geom%R2(i) + geom%l2(i)) ) / geom%g2(i) )

            ! Above or below edge; this is a unified form of Johnson Eq. (D.60)
            else

                ! Check for point on perimeter
                if (min(geom%R1(i), geom%R2(i)) < 1e-12) then
                    write(*,*) "!!! Detected control point on perimeter of panel. Solution quality may be negatively affected."
                end if

                ! Calculate
                int%F111(i) = sign(1., geom%l1(i)) * log( (geom%R2(i) + abs(geom%l2(i))) / (geom%R1(i) + abs(geom%l1(i))) )

            end if

        end do

        ! Calculate F(1,2,1) and F(2,1,1)

        ! Calculate (these formulas come from PAN AIR and are equivalent to Johnson, but simplified)
        int%F121 = geom%a*geom%v_eta*int%F111 + geom%v_xi*geom%dR
        int%F211 = geom%a*geom%v_xi*int%F111 - geom%v_eta*geom%dR


    end subroutine panel_calc_basic_F_integrals_subsonic


    subroutine panel_calc_basic_F_integrals_supersonic_subinc(this, geom, dod_info, freestream, mirror_panel, int)
        ! Calculates the F integrals necessary to determine the influence of a subinclined triangular panel in supersonic flow.
        ! Taken from Ehlers et al. (1979) Appendix E.

        implicit none

        class(panel),intent(in) :: this
        type(eval_point_geom),intent(in) :: geom
        type(dod),intent(in) :: dod_info
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel
        type(integrals),intent(inout) :: int

        real :: F1, F2, eps, eps2, series, b, s_b
        integer :: i, i_next

        ! Loop through edges
        do i=1,this%N

            ! Check DoD
            if (dod_info%edges_in_dod(i)) then

                i_next = mod(i, this%N) + 1

                ! Get b and its square root; doing this removes a lot of mirror checks later
                if (mirror_panel) then
                    b = this%b_mir(i)
                    s_b = this%sqrt_b_mir(i)
                else
                    b = this%b(i)
                    s_b = this%sqrt_b(i)
                end if

                ! Mach wedge
                if (geom%R1(i) == 0. .and. geom%R2(i) == 0) then

                    ! F(1,1,1)
                    int%F111(i) = pi/s_b

                    ! Higher-order
                    int%F121(i) = -geom%a(i)*geom%v_eta(i)*int%F111(i)/b
                    int%F211(i) = geom%a(i)*geom%v_xi(i)*int%F111(i)/b

                else

                    ! Calculate F factors
                    if (b > 0.) then
                        F1 = (geom%l1(i)*geom%R2(i) - geom%l2(i)*geom%R1(i)) / geom%g2(i)
                        F2 = (b*geom%R1(i)*geom%R2(i) + geom%l1(i)*geom%l2(i)) / geom%g2(i)
                    else
                        F1 = (geom%R2(i) - geom%R1(i))*(geom%R2(i) + geom%R1(i)) / (geom%l1(i)*geom%R2(i) + geom%l2(i)*geom%R1(i))
                        F2 = (geom%g2(i) - geom%l1(i)**2 - geom%l2(i)**2) / (b*geom%R1(i)*geom%R2(i) - geom%l1(i)*geom%l2(i))
                    end if

                    ! Nearly-sonic edge
                    if (abs(F2) > 125.0*abs(s_b*F1)) then

                        ! F(1,1,1)
                        eps = F1/F2
                        eps2 = eps*eps
                        series = eps*eps2*(1./3. - b*eps2/5. + (b*eps2)*(b*eps2)/7.)
                        int%F111(i) = -eps + b*series

                        ! Higher-order
                        if (mirror_panel) then
                            int%F121(i) = (-geom%v_xi(i)*geom%dR(i)*geom%R1(i)*geom%R2(i) &
                                           + geom%l2(i)*geom%R1(i)*(this%vertices_ls_mir(2,i_next) - geom%P_ls(2)) &
                                           - geom%l1(i)*geom%R2(i)*(this%vertices_ls_mir(2,i) - geom%P_ls(2)) &
                                          ) / (geom%g2(i)*F2) - geom%a(i)*geom%v_eta(i)*series
                        else
                            int%F121(i) = (-geom%v_xi(i)*geom%dR(i)*geom%R1(i)*geom%R2(i) &
                                           + geom%l2(i)*geom%R1(i)*(this%vertices_ls(2,i) - geom%P_ls(2)) &
                                           - geom%l1(i)*geom%R2(i)*(this%vertices_ls(2,i_next) - geom%P_ls(2)) &
                                          ) / (geom%g2(i)*F2) - geom%a(i)*geom%v_eta(i)*series
                        end if
                        int%F211(i) = -geom%v_eta(i)*geom%dR(i) + geom%a(i)*geom%v_xi(i)*int%F111(i) - &
                                      2.*geom%v_xi(i)*geom%v_eta(i)*int%F121(i)

                    ! Supersonic edge
                    else if (b > 0.) then

                        ! F(1,1,1)
                        int%F111(i) = -atan2(s_b*F1, F2) / s_b

                        ! Higher-order
                        int%F121(i) = -(geom%v_xi(i)*geom%dR(i) + geom%a(i)*geom%v_eta(i)*int%F111(i)) / b
                        int%F211(i) = -geom%v_eta(i)*geom%dR(i) + geom%a(i)*geom%v_xi(i)*int%F111(i) - &
                                        2.*geom%v_xi(i)*geom%v_eta(i)*int%F121(i)
                            !int%F211(i) = (geom%a(i)*int%F111(i) - geom%v_eta(i)*int%F121(i)) / geom%v_xi(i) ! alternative

                    ! Subsonic edge
                    else
                        
                        ! F(1,1,1)
                        F1 = s_b*geom%R1(i) + abs(geom%l1(i))
                        F2 = s_b*geom%R2(i) + abs(geom%l2(i))
                        if (F1 /= 0. .and. F2 /= 0.) then
                            int%F111(i) = -sign(1., geom%v_eta(i))*log(F1/F2)/s_b
                        else
                            if (verbose) write(*,*) "!!! Detected evaluation point on perimeter of panel. Solution may be affected."
                        end if

                        ! Higher-order
                        int%F121(i) = -(geom%v_xi(i)*geom%dR(i) + geom%a(i)*geom%v_eta(i)*int%F111(i)) / b
                        int%F211(i) = -geom%v_eta(i)*geom%dR(i) + geom%a(i)*geom%v_xi(i)*int%F111(i) - &
                                          2.*geom%v_xi(i)*geom%v_eta(i)*int%F121(i)
                            !int%F211(i) = (geom%a(i)*int%F111(i) - geom%v_eta(i)*int%F121(i)) / geom%v_xi(i) ! alternative unstable in this case
                        end if
                    end if

                end if

            ! Check
            if (this%order == 2) then
                if (abs(geom%v_xi(i)*int%F211(i) + geom%v_eta(i)*int%F121(i) - geom%a(i)*int%F111(i)) > 1.e-12) then
                    write(*,*) "!!! Calculation of F(2,1,1) and F(1,2,1) failed. Please submit a bug report on GitHub."
                end if
            end if

        end do

    end subroutine panel_calc_basic_F_integrals_supersonic_subinc


    subroutine panel_calc_basic_F_integrals_supersonic_supinc(this, geom, dod_info, freestream, mirror_panel, int)
        ! Calculates the F integrals necessary to determine the influence of a superinclined triangular panel in supersonic flow.
        ! Taken from Epton and Magnus, but mostly the PAN AIR source code

        implicit none

        class(panel),intent(in) :: this
        type(eval_point_geom),intent(in) :: geom
        type(dod),intent(in) :: dod_info
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel
        type(integrals),intent(inout) :: int

        real :: F1, F2
        integer :: i

        ! Loop through edges
        do i=1,this%N

            ! Check DoD
            if (dod_info%edges_in_dod(i)) then

                ! Mach wedge
                if (geom%R1(i) == 0. .and. geom%R2(i) == 0) then

                    ! F(1,1,1)
                    int%F111(i) = -pi

                    ! Higher-order
                    int%F211(i) = geom%a(i)*geom%v_xi(i)*int%F111(i)
                    int%F121(i) = geom%a(i)*geom%v_eta(i)*int%F111(i)

                else

                    ! Calculate F factors
                    F1 = geom%l1(i)*geom%R2(i) - geom%l2(i)*geom%R1(i)
                    F2 = geom%R1(i)*geom%R2(i) + geom%l1(i)*geom%l2(i)

                    ! F(1,1,1)
                    int%F111(i) = atan2(F1, F2)

                    ! Higher-order
                    int%F211(i) = geom%a(i)*geom%v_xi(i)*int%F111(i) - geom%v_eta(i)*geom%dR(i)
                    int%F121(i) = geom%a(i)*geom%v_eta(i)*int%F111(i) + geom%v_xi(i)*geom%dR(i)

                end if

            end if

            ! Check
            if (this%order == 2) then
                if (abs(geom%v_xi(i)*int%F211(i) + geom%v_eta(i)*int%F121(i) - geom%a(i)*int%F111(i)) > 1.e-10) then
                    write(*,*) "!!! Calculation of F(2,1,1) and F(1,2,1) failed. Please submit a bug report on GitHub"
                    stop
                end if
            end if

        end do

    end subroutine panel_calc_basic_F_integrals_supersonic_supinc


    subroutine panel_calc_hH113_subsonic(this, geom, freestream, mirror_panel, int)
        ! Calculates hH(1,1,3) for a panel in subsonic flow.
        ! Taken from Johnson (1980) Appendix D.3. with alterations made based on PAN AIR.

        implicit none

        class(panel),intent(in) :: this
        type(eval_point_geom),intent(in) :: geom
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel
        type(integrals),intent(inout) :: int

        real :: S, C, c1, c2, x
        integer :: i

        ! Calculate hH(1,1,3) (Johnson Eqs. (D.41) and (G.24))
        ! No check on the magnitude of h is necessary since we never divide by it
        int%hH113 = 0.
        do i=1,this%N

            ! Calculate intermediate quantities
            c1 = geom%g2(i) + abs(geom%h)*geom%R1(i)
            c2 = geom%g2(i) + abs(geom%h)*geom%R2(i)
        
            ! Calculate integral for edge
            S = geom%a(i)*(geom%l2(i)*c1 - geom%l1(i)*c2)
            C = c1*c2 + geom%a(i)**2*geom%l1(i)*geom%l2(i)
            x = atan2(S, C)

            ! Sum
            int%hH113 = int%hH113 + x

        end do
        
        ! Apply sign factor (Johnson Eq. (D.42)
        int%hH113 = sign(int%hH113, geom%h)

    end subroutine panel_calc_hH113_subsonic


    subroutine panel_calc_hH113_supersonic_subinc(this, geom, dod_info, freestream, mirror_panel, int)
        ! Calculates hH(1,1,3) for a subinclined panel in supersonic flow.
        ! Taken from Ehlers et al. (1979) Appendix E.

        implicit none

        class(panel),intent(in) :: this
        type(eval_point_geom),intent(in) :: geom
        type(dod),intent(in) :: dod_info
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel
        type(integrals),intent(inout) :: int

        real(16) :: F1, F2, b
        integer :: i

        ! Calculate hH(1,1,3) (Ehlers Eq. (E18))
        int%hH113 = 0.

        ! Loop through edges
        do i=1,this%N

            ! Check DoD
            if (dod_info%edges_in_dod(i)) then

                ! Get b
                if (mirror_panel) then
                    b = this%b_mir(i)
                else
                    b = this%b(i)
                end if

                ! Check not on panel plane
                if (abs(geom%h) > 1.e-12) then

                    ! Mach wedge
                    if (geom%R1(i) == 0. .and. geom%R2(i) == 0.) then
                        int%hH113 = int%hH113 + pi*sign(1., geom%h*geom%v_xi(i))
                    else

                        ! Calculate F factors for supersonic edge
                        if (b > 0.) then
                            F1 = (geom%l1(i)*geom%R2(i) - geom%l2(i)*geom%R1(i)) / geom%g2(i)
                            F2 = (b*geom%R1(i)*geom%R2(i) + geom%l1(i)*geom%l2(i)) / geom%g2(i)

                        ! Calculate F factors for subsonic edge
                        else
                            F1 = geom%dR(i)*(geom%R2(i) + geom%R1(i)) / (geom%l1(i)*geom%R2(i) + geom%l2(i)*geom%R1(i))
                            F2 = (geom%g2(i) - geom%l1(i)**2 - geom%l2(i)**2) &
                                 / (b*geom%R1(i)*geom%R2(i) - geom%l1(i)*geom%l2(i))
                        end if

                        ! Calculate hH113
                        int%hH113 = int%hH113 + atan2(geom%h*geom%a(i)*F1, geom%R1(i)*geom%R2(i) + geom%h2*F2)

                    end if

                end if
            end if
        end do

    end subroutine panel_calc_hH113_supersonic_subinc


    subroutine panel_calc_hH113_supersonic_supinc(this, geom, dod_info, freestream, mirror_panel, int)
        ! Calculates hH(1,1,3) for a superinclined panel in supersonic flow.
        ! Taken from Epton and Magnus, but mostly the PAN AIR source code

        implicit none

        class(panel),intent(in) :: this
        type(eval_point_geom),intent(in) :: geom
        type(dod),intent(in) :: dod_info
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel
        type(integrals),intent(inout) :: int

        real :: t_dot, t_cross, X, Y
        integer :: i, i_prev

        ! Calculate hH(1,1,3)
        int%hH113 = 2.*pi

        ! Loop through corners
        do i=1,this%N

            ! Edge influence
            if (dod_info%edges_in_dod(i)) int%hH113 = int%hH113 - pi

            ! Corner influence
            if (geom%R1(i) > 0.) then

                ! Cancel out edge influence
                int%hH113 = int%hH113 + pi

                ! Get index of previous corner
                if (i == 1) then
                    i_prev = this%N
                else
                    i_prev = i-1
                end if

                ! Get dot and cross products
                t_dot = geom%v_xi(i)*geom%v_xi(i_prev) + geom%v_eta(i)*geom%v_eta(i_prev)
                t_cross = geom%v_xi(i_prev)*geom%v_eta(i) - geom%v_eta(i_prev)*geom%v_xi(i)

                ! Intermediate values
                X = geom%a(i)*geom%a(i_prev) - geom%h2*t_dot
                Y = geom%h*geom%R1(i)*t_cross

                ! Update hH113
                int%hH113 = int%hH113 - atan2(Y, -X)

            end if
        end do

    end subroutine panel_calc_hH113_supersonic_supinc

    
    subroutine panel_calc_remaining_integrals(this, geom, influence_type, freestream, mirror_panel, int, dod_info)
        ! Calculates the remaining necessary H and F integrals using the unified recursion relations

        implicit none
        
        class(panel),intent(in) :: this
        type(eval_point_geom),intent(in) :: geom
        character(len=*),intent(in) :: influence_type
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel
        type(integrals),intent(inout) :: int
        type(dod),intent(in) :: dod_info

        ! Lower-order potential integrals
        int%H111 = sum(geom%a*int%F111) - int%rs*geom%h*int%hH113
        int%H213 = -int%r*sum(geom%v_xi*int%F111)
        int%H123 = -int%s*sum(geom%v_eta*int%F111)

        ! Higher-order potential integrals
        if (this%order == 2) then

            ! For sources
            if (this%has_sources) then
                int%H211 = 0.5*(-int%rs*geom%h2*int%H213 + sum(geom%a*int%F211))
                int%H121 = 0.5*(-int%rs*geom%h2*int%H123 + sum(geom%a*int%F121))
            end if

            ! For doublets
            !int%H313 = sum(geom%v_eta*int%F121) - geom%h*int%hH113
            int%H313 = int%r*(int%H111 - sum(geom%v_xi*int%F211))
            int%H223 = -int%r*sum(geom%v_xi*int%F121)
            int%H133 = int%s*(int%H111 - sum(geom%v_eta*int%F121))

            ! Run checks
            if (abs(-int%s*sum(geom%v_eta*int%F211) - int%H223) > 1e-10) then
                write(*,*) "!!! Calculation failed for H(2,2,3). Please submit a bug report on GitHub."
            end if

            if (this%has_sources) then
                if (abs(int%H111 - int%r*int%H313 - int%s*int%H133 - int%rs*geom%h*int%hH113) > 1e-10) then
                    write(*,*) "!!! Calculation failed for H(3,1,3) and H(1,3,3). Please submit a bug report on GitHub."
                end if
            end if

        end if

        ! Velocity integrals
        if (influence_type == 'velocity') then
            call this%calc_F_recursions_for_velocity(geom, freestream, mirror_panel, int, dod_info)
            call this%calc_H_recursions_for_velocity(geom, freestream, mirror_panel, int)
        end if
        
    end subroutine panel_calc_remaining_integrals


    subroutine panel_calc_F_recursions_for_velocity(this, geom, freestream, mirror_panel, int, dod_info)
        ! Calculates the remaining necessary F integrals for velocity influences using the unified recursion relations

        implicit none
        
        class(panel),intent(in) :: this
        type(eval_point_geom),intent(in) :: geom
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel
        type(integrals),intent(inout) :: int
        type(dod),intent(in) :: dod_info

        allocate(int%F113(3), source=0.)
        allocate(int%F123(3), source=0.)
        allocate(int%F133(3), source=0.)

        
        where(dod_info%edges_in_dod)
            ! F(1,1,3) from Johnson (D.61) and Velocity Influences (69)
            int%F113 = (- int%s*geom%v_eta*this%EMNK(geom, 2, 1, 1, mirror_panel) &
                        + int%r*geom%v_xi*this%EMNK(geom, 1, 2, 1, mirror_panel)) / geom%g2

            ! F(1,2,3) from Johnson (D.66)
            !int%F123 = geom%v_eta*geom%a*int%F113 - geom%v_xi*this%EMNK(geom, 1, 1, 1)

            ! F(1,2,3) from Velocity Influences (72)
            int%F123 = ( - int%r*(geom%v_xi**2)*int%F121 + int%r*geom%v_xi*this%EMNK(geom, 1, 3, 1, mirror_panel) &
                        - int%s*geom%v_eta*this%EMNK(geom, 2, 2, 1, mirror_panel) + int%s*geom%v_eta*geom%v_xi*int%F211) / geom%g2

            ! F(1,3,3) from Johnson (D.67)
            !int%F133 = 2.*geom%a*geom%v_eta*int%F123 - (geom%a*geom%a + geom%v_xi*geom%v_xi*geom%h2)*int%F113 &
            !           + geom%v_xi*geom%v_xi*int%F111

            ! F(1,3,3) from velocity Influences (71)
            int%F133 = (int%r*geom%v_eta*geom%a*int%F123 + geom%v_xi**2*int%F111 &
                        - geom%v_xi*this%EMNK(geom, 1, 2, 1, mirror_panel)) / (int%s*geom%v_xi**2 + int%r*geom%v_eta**2)
        elsewhere
            
            int%F113 = 0.
            int%F123 = 0.
            int%F133 = 0.

        end where
    end subroutine panel_calc_F_recursions_for_velocity


    subroutine panel_calc_H_recursions_for_velocity(this, geom, freestream, mirror_panel, int)
        ! Calculates the remaining necessary H integrals for velocity influences using the unified recursion relations

        implicit none
        
        class(panel),intent(in) :: this
        type(eval_point_geom),intent(in) :: geom
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel
        type(integrals),intent(inout) :: int

        ! THESE ARE ONLY SUBSONIC RIGHT NOW!!!!
        ! Johnson (D.42)
        int%h3H115 = int%rs*(int%hH113 + geom%h * sum(geom%a*int%F113))/3

        ! Johnson (D.46)
        int%H125 = -int%s*sum(geom%v_eta*int%F113)/3.
        int%hH135 = int%s*(int%hH113 - geom%h*sum(geom%v_eta*int%F123))/3.
        int%H145 = int%s*(2.*int%H123 - sum(geom%v_eta*int%F133))/3.

        ! Johnson (D.47)
        int%H215 = -int%r*sum(geom%v_xi*int%F113)/3.
        int%H225 = -int%r*sum(geom%v_xi*int%F123)/3.
        int%H235 = -int%r*sum(geom%v_xi*int%F133)/3.

        ! Johnson (D.48)
        int%hH315 = -int%rs*int%hH135 - int%s*int%h3H115 + int%r*int%hH113
        int%H325 = - int%rs*int%H145 - int%s*geom%h2*int%H125 + int%r*int%H123
        int%H415 = - int%rs*int%H235 - int%s*geom%h2*int%H215 + int%r*int%H223
        int%H113_3rsh2H115 = -sum(geom%a*int%F113)

    end subroutine panel_calc_H_recursions_for_velocity


    function panel_calc_integrals(this, geom, influence_type, freestream, mirror_panel, dod_info) result(int)
        ! Calculates the H and F integrals necessary for the given influence

        implicit none

        class(panel),intent(in) :: this
        type(eval_point_geom),intent(in) :: geom
        character(len=*),intent(in) :: influence_type
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel
        type(dod),intent(in) :: dod_info

        type(integrals) :: int

        ! Allocate space for edge integrals
        allocate(int%F111(this%N), source=0.)
        allocate(int%F121(this%N), source=0.)
        allocate(int%F211(this%N), source=0.)

        ! Store parameters
        if (mirror_panel) then
            int%r = this%r_mir
        else
            int%r = this%r
        end if
        int%s = freestream%s
        int%rs = int%r*int%s

        ! Calculate necessary integrals based on the flow condition and panel type
        ! These are needed for both velocity and potential calculations
        if (freestream%supersonic) then
            if (int%r < 0) then
                call this%calc_basic_F_integrals_supersonic_supinc(geom, dod_info, freestream, mirror_panel, int)
                call this%calc_hH113_supersonic_supinc(geom, dod_info, freestream, mirror_panel, int)
            else
                call this%calc_basic_F_integrals_supersonic_subinc(geom, dod_info, freestream, mirror_panel, int)
                call this%calc_hH113_supersonic_subinc(geom, dod_info, freestream, mirror_panel, int)
            end if
        else
            call this%calc_basic_F_integrals_subsonic(geom, freestream, mirror_panel, int)
            call this%calc_hH113_subsonic(geom, freestream, mirror_panel, int)
        end if

        ! Run H recursions
        call this%calc_remaining_integrals(geom, influence_type, freestream, mirror_panel, int, dod_info)

    end function panel_calc_integrals


    function panel_assemble_phi_s_S_space(this, int, geom, freestream, mirror_panel) result(phi_s_S_space)
        ! Assembles the vector of source-induced potential influences expressed in strength space

        implicit none
        
        class(panel),intent(in) :: this
        type(integrals),intent(in) :: int
        type(eval_point_geom),intent(in) :: geom
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel

        real,dimension(:),allocatable :: phi_s_S_space

        real,dimension(:),allocatable :: phi_s_sigma_space

        ! Allocate space
        allocate(phi_s_sigma_space(this%sigma_dim), source=0.)
        allocate(phi_s_S_space(this%S_dim), source=0.)

        ! Constant influence
        phi_s_sigma_space(1) = int%H111

        ! Linear influences
        if (this%order == 2) then

            ! Johnson Eq. (D21)
            ! Equivalent to Ehlers Eq. (8.6)
            phi_s_sigma_space(2) = int%H111*geom%P_ls(1) + int%H211
            phi_s_sigma_space(3) = int%H111*geom%P_ls(2) + int%H121

            ! Convert to strength influences (Davis Eq. (4.41))
            if (mirror_panel) then
                phi_s_S_space = -this%J_mir*freestream%K_inv*matmul(phi_s_sigma_space, this%T_sigma_mir)
            else
                phi_s_S_space = -this%J*freestream%K_inv*matmul(phi_s_sigma_space, this%T_sigma)
            end if

        else

            ! Convert to strength influence
            if (mirror_panel) then
                phi_s_S_space = -this%J_mir*freestream%K_inv*phi_s_sigma_space
            else
                phi_s_S_space = -this%J*freestream%K_inv*phi_s_sigma_space
            end if

        end if
        
    end function panel_assemble_phi_s_S_space


    function panel_assemble_phi_d_M_space(this, int, geom, freestream, mirror_panel) result(phi_d_M_space)
        ! Assembles the vector of doublet-induced potential influences expressed in strength space
        implicit none
        
        class(panel),intent(in) :: this
        type(integrals),intent(in) :: int
        type(eval_point_geom),intent(in) :: geom
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel

        real,dimension(:),allocatable :: phi_d_M_space

        real,dimension(:),allocatable :: phi_d_mu_space

        ! Allocate space
        allocate(phi_d_mu_space(this%mu_dim), source=0.)
        if (this%in_wake) then
            allocate(phi_d_M_space(this%M_dim*2), source=0.)
        else
            allocate(phi_d_M_space(this%M_dim), source=0.)
        end if

        ! Johnson Eq. (D.30)
        ! Equivalent to Ehlers Eq. (5.17))
        phi_d_mu_space(1) = int%hH113
        phi_d_mu_space(2) = int%hH113*geom%P_ls(1) + geom%h*int%H213
        phi_d_mu_space(3) = int%hH113*geom%P_ls(2) + geom%h*int%H123

        ! Add quadratic terms
        if (this%order == 2) then
            phi_d_mu_space(4) = 0.5*int%hH113*geom%P_ls(1)**2 + geom%h*(geom%P_ls(1)*int%H213 + 0.5*int%H313)
            phi_d_mu_space(5) = int%hH113*geom%P_ls(1)*geom%P_ls(2) &
                                + geom%h*(geom%P_ls(2)*int%H213 + geom%P_ls(1)*int%H123 + int%H223)
            phi_d_mu_space(6) = 0.5*int%hH113*geom%P_ls(2)**2 + geom%h*(geom%P_ls(2)*int%H123 + 0.5*int%H133)
        end if

        ! Convert to strength influences (Davis Eq. (4.41))
        if (mirror_panel) then
            phi_d_M_space(1:this%M_dim) = int%s*freestream%K_inv*matmul(phi_d_mu_space, this%T_mu_mir)
        else
            phi_d_M_space(1:this%M_dim) = int%s*freestream%K_inv*matmul(phi_d_mu_space, this%T_mu)
        end if

        ! Wake bottom influence is opposite the top influence
        if (this%in_wake) then
            phi_d_M_space(this%M_dim+1:) = -phi_d_M_space(1:this%M_dim)
        end if
        
    end function panel_assemble_phi_d_M_space


    subroutine panel_calc_potential_influences(this, P, freestream, dod_info, mirror_panel, phi_s_S_space, phi_d_M_space)
        ! Calculates the source- and doublet-induced potentials at the given point P

        implicit none

        class(panel),intent(in) :: this
        real,dimension(3),intent(in) :: P
        type(flow),intent(in) :: freestream
        type(dod),intent(in) :: dod_info
        logical,intent(in) :: mirror_panel
        real,dimension(:),allocatable,intent(out) :: phi_s_S_space, phi_d_M_space

        type(eval_point_geom) :: geom
        type(integrals) :: int

        ! Check DoD
        if (dod_info%in_dod .and. this%A > 0.) then

            ! Calculate geometric parameters
            if (freestream%supersonic) then
                if ((mirror_panel .and. this%r_mir < 0.) .or. (.not. mirror_panel .and. this%r < 0.)) then
                    geom = this%calc_supersonic_supinc_geom(P, freestream, mirror_panel, dod_info)
                else
                    geom = this%calc_supersonic_subinc_geom(P, freestream, mirror_panel, dod_info)
                end if
            else
                geom = this%calc_subsonic_geom(P, freestream, mirror_panel)
            end if

            ! Get integrals
            int = this%calc_integrals(geom, 'potential', freestream, mirror_panel, dod_info)

            ! Source potential
            if (this%has_sources) then
                phi_s_S_space = this%assemble_phi_s_S_space(int, geom, freestream, mirror_panel)
            else
                allocate(phi_s_S_space(this%S_dim), source=0.)
            end if

            ! Doublet potential
            phi_d_M_space = this%assemble_phi_d_M_space(int, geom, freestream, mirror_panel)

        else

            ! Allocate placeholders
            allocate(phi_s_S_space(this%S_dim), source=0.)
            if (this%in_wake) then
                allocate(phi_d_M_space(this%M_dim*2), source=0.)
            else
                allocate(phi_d_M_space(this%M_dim), source=0.)
            end if

        end if
    
    end subroutine panel_calc_potential_influences


    subroutine panel_calc_potentials(this, P, freestream, dod_info, mirror_panel, sigma, mu, &
                                     N_body_panels, N_body_verts, asym_flow, phi_s, phi_d)
        ! Calculates the potentials induced at the given point

        implicit none
        
        class(panel),intent(in) :: this
        real,dimension(3),intent(in) :: P
        type(flow),intent(in) :: freestream
        type(dod),intent(in) :: dod_info
        logical,intent(in) :: mirror_panel, asym_flow
        real,dimension(:),allocatable,intent(in) :: sigma, mu
        integer,intent(in) :: N_body_panels, N_body_verts
        real,intent(out) :: phi_d, phi_s

        real,dimension(:),allocatable :: source_inf, doublet_inf
        real,dimension(:),allocatable :: doublet_strengths
        real,dimension(this%S_dim) :: source_strengths

        ! Get influences
        call this%calc_potential_influences(P, freestream, dod_info, mirror_panel, source_inf, doublet_inf)

        ! Get strengths
        source_strengths = this%get_source_strengths(sigma, mirror_panel, N_body_panels, asym_flow)
        doublet_strengths = this%get_doublet_strengths(mu, mirror_panel, N_body_verts, asym_flow)

        ! Apply strengths to calculate potentials
        phi_s = sum(source_inf*source_strengths)
        if (this%in_wake) then
            phi_d = sum((doublet_inf(1:this%M_dim) + doublet_inf(this%M_dim+1:))*doublet_strengths)
        else
            phi_d = sum(doublet_inf*doublet_strengths)
        end if
        
    end subroutine panel_calc_potentials


    function panel_assemble_v_s_S_space(this, int, geom, freestream, mirror_panel) result(v_s_S_space)
        ! Assembles the source-induced velocity influence coefficient matrix from the previously-calculated influence integrals

        implicit none

        class(panel),intent(in) :: this
        type(integrals),intent(in) :: int
        type(eval_point_geom),intent(in) :: geom
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel

        real,dimension(:,:),allocatable :: v_s_S_space

        real,dimension(:,:),allocatable :: v_s_sigma_space

        ! Allocate space
        allocate(v_s_sigma_space(3,this%sigma_dim), source=0.)
        allocate(v_s_S_space(3,this%S_dim), source=0.)

        ! Constant influence (Johnson Eq. (D.28))
        v_s_sigma_space(1,1) = int%r*int%H213
        v_s_sigma_space(2,1) = int%s*int%H123
        v_s_sigma_space(3,1) = -int%rs*int%hH113

        ! Linear influences
        if (this%order == 2) then

            ! x-component
            v_s_sigma_space(1,2) = int%r*(int%H213*geom%P_ls(1) + int%H313)
            v_s_sigma_space(1,3) = int%r*(int%H213*geom%P_ls(2) + int%H223)

            ! y-component
            v_s_sigma_space(2,2) = int%s*(int%H123*geom%P_ls(1) + int%H223)
            v_s_sigma_space(2,3) = int%s*(int%H123*geom%P_ls(2) + int%H133)

            ! z-component
            v_s_sigma_space(3,2) = -int%rs*(int%hH113*geom%P_ls(1) + geom%h*int%H213)
            v_s_sigma_space(3,3) = -int%rs*(int%hH113*geom%P_ls(2) + geom%h*int%H123)

            ! Convert to vertex influences
            if (mirror_panel) then
                v_s_S_space = matmul(v_s_sigma_space, this%T_sigma_mir)
            else
                v_s_S_space = matmul(v_s_sigma_space, this%T_sigma)
            end if

        else
            v_s_S_space = v_s_sigma_space
        end if
        
        ! Add area Jacobian and kappa factor
        if (mirror_panel) then
            v_s_S_space = -v_s_S_space*freestream%K_inv*this%J_mir
        else
            v_s_S_space = -v_s_S_space*freestream%K_inv*this%J
        end if

        ! Transform to global coordinates
        if (mirror_panel) then
            v_s_S_space = matmul(transpose(this%A_g_to_ls_mir), v_s_S_space)
        else
            v_s_S_space = matmul(transpose(this%A_g_to_ls), v_s_S_space)
        end if
        
    end function panel_assemble_v_s_S_space


    function panel_assemble_v_d_M_space(this, int, geom, freestream, mirror_panel) result(v_d_M_space)
        ! Assembles the doublet-induced velocity influence coefficient matrix from the previously-calculated influence integrals

        implicit none

        class(panel),intent(in) :: this
        type(integrals),intent(in) :: int
        type(eval_point_geom),intent(in) :: geom
        type(flow),intent(in) :: freestream
        logical,intent(in) :: mirror_panel

        real,dimension(:,:),allocatable :: v_d_M_space

        real,dimension(:,:),allocatable :: v_d_mu_space

        ! Allocate space
        allocate(v_d_mu_space(3,this%mu_dim), source=0.)
        if (this%in_wake) then
            allocate(v_d_M_space(3,2*this%M_dim), source=0.)
        else
            allocate(v_d_M_space(3,this%M_dim), source=0.)
        end if

        ! Linear terms
        !v_d_mu_space(1,1) = 3.*int%r*geom%h*int%H215
        !v_d_mu_space(1,2) = 3.*int%r*(int%H215*geom%P_ls(1)*geom%h + int%hH315)
        !v_d_mu_space(1,3) = 3.*int%r*geom%h*(int%H215*geom%P_ls(2) + int%H225)

        !v_d_mu_space(2,1) = 3.*int%s*geom%h*int%H125
        !v_d_mu_space(2,2) = 3.*int%s*geom%h*(int%H125*geom%P_ls(1) + int%H225)
        !v_d_mu_space(2,3) = 3.*int%s*(int%H125*geom%P_ls(2)*geom%h + int%hH135)

        !v_d_mu_space(3,1) = int%H113_3rsh2H115
        !v_d_mu_space(3,2) = int%H113_3rsh2H115*geom%P_ls(1) + int%H213 - 3.*int%rs*geom%h2*int%H215
        !v_d_mu_space(3,3) = int%H113_3rsh2H115*geom%P_ls(2) + int%H123 - 3.*int%rs*geom%h2*int%H125

        v_d_mu_space(1,1) = 0
        v_d_mu_space(1,2) = int%hH113
        v_d_mu_space(1,3) = 0

        v_d_mu_space(2,1) = 0
        v_d_mu_space(2,2) = 0
        v_d_mu_space(2,3) = int%hH113

        v_d_mu_space(3,1) = 0
        v_d_mu_space(3,2) = int%H213
        v_d_mu_space(3,3) = int%H123

        if (this%order == 2) then

            ! Quadratic terms
            v_d_mu_space(1,4) = 3.*int%r*(0.5*int%H215*(geom%P_ls(1)**2)*geom%h + int%hH315*geom%P_ls(1) + 0.5*int%H415*geom%h)
            v_d_mu_space(1,5) = 3.*int%r*(int%H215*geom%h*geom%P_ls(1)*geom%P_ls(2) + int%hH315*geom%P_ls(2) &
                                + int%H225*geom%h*geom%P_ls(1) + int%H325*geom%h)
            v_d_mu_space(1,6) = 3.*int%r*(0.5*int%H215*(geom%P_ls(2)**2)*geom%h + int%H225*geom%h*geom%P_ls(2) &
                                + 0.5*int%H235*geom%h)

            v_d_mu_space(2,4) = 3.*int%s*(0.5*int%H125*(geom%P_ls(1)**2)*geom%h + int%H225*geom%h*geom%P_ls(1) &
                                + 0.5*int%H325*geom%h)
            v_d_mu_space(2,5) = 3.*int%s*(int%H125*geom%h*geom%P_ls(1)*geom%P_ls(2) + int%hH135*geom%P_ls(1) &
                                + int%H225*geom%h*geom%P_ls(2) + int%H235*geom%h)
            v_d_mu_space(2,6) = 3.*int%s*(0.5*int%H125*(geom%P_ls(2)**2)*geom%h + int%hH135*geom%P_ls(2) + 0.5*int%H145*geom%h)

            v_d_mu_space(3,4) = 0.5*(geom%P_ls(1)**2)*int%H113_3rsh2H115 + geom%P_ls(1)*(int%H213 - 3.*int%rs*geom%h2*int%H215) &
                                + 0.5*(int%H313 - 3.*int%rs*geom%h*int%hH315)
            v_d_mu_space(3,5) = geom%P_ls(1)*geom%P_ls(2)*(int%H113_3rsh2H115) &
                                + geom%P_ls(2)*(int%H213 - 3.*int%rs*geom%h2*int%H215) &
                                + geom%P_ls(1)*(int%H123 - 3.*int%rs*geom%h2*int%H125) + int%H223 - 3.*int%rs*geom%h2*int%H225
            v_d_mu_space(3,6) = 0.5*(geom%P_ls(2)**2)*int%H113_3rsh2H115 + geom%P_ls(2)*(int%H123 - 3.*int%rs*geom%h2*int%H125) &
                                + 0.5*(int%H133 - 3.*int%rs*geom%h*int%hH135)

        end if 
            
        ! Convert to strength influences (Davis Eq. (4.41))
        if (mirror_panel) then
            v_d_M_space(:,1:this%M_dim) = int%s*freestream%K_inv*matmul(v_d_mu_space, this%T_mu_mir)
        else
            v_d_M_space(:,1:this%M_dim) = int%s*freestream%K_inv*matmul(v_d_mu_space, this%T_mu)
        end if

        ! Wake bottom influence is opposite the top influence
        if (this%in_wake) then
            v_d_M_space(:,this%M_dim+1:this%M_dim*2) = -v_d_M_space(:,1:this%M_dim)
        end if

        ! Transform to global coordinates
        if (mirror_panel) then
            v_d_M_space = matmul(transpose(this%A_g_to_ls_mir), v_d_M_space)
        else
            v_d_M_space = matmul(transpose(this%A_g_to_ls), v_d_M_space)
        end if
        
    end function panel_assemble_v_d_M_space


    subroutine panel_calc_velocity_influences(this, P, freestream, dod_info, mirror_panel, v_s_S_space, v_d_M_space)
        ! Calculates the source- and doublet-induced velocity influences at the given point P

        implicit none

        class(panel),intent(in) :: this
        real,dimension(3),intent(in) :: P
        type(flow),intent(in) :: freestream
        type(dod),intent(in) :: dod_info
        logical,intent(in) :: mirror_panel
        real,dimension(:,:),allocatable,intent(out) :: v_s_S_space, v_d_M_space

        type(eval_point_geom) :: geom
        type(integrals) :: int

        ! Check DoD
        if (dod_info%in_dod .and. this%A > 0.) then

            ! Calculate geometric parameters
            if (freestream%supersonic) then
                if ((mirror_panel .and. this%r_mir < 0.) .or. (.not. mirror_panel .and. this%r < 0.)) then
                    geom = this%calc_supersonic_supinc_geom(P, freestream, mirror_panel, dod_info)
                else
                    geom = this%calc_supersonic_subinc_geom(P, freestream, mirror_panel, dod_info)
                end if
            else
                geom = this%calc_subsonic_geom(P, freestream, mirror_panel)
            end if

            ! Get integrals
            int = this%calc_integrals(geom, 'velocity', freestream, mirror_panel, dod_info)

            ! Source velocity
            if (this%has_sources) then
                v_s_S_space = this%assemble_v_s_S_space(int, geom, freestream, mirror_panel)
            else
                allocate(v_s_S_space(3,this%S_dim), source=0.)
            end if

            ! Doublet velocity
            v_d_M_space = this%assemble_v_d_M_space(int, geom, freestream, mirror_panel)

        else

            ! Allocate placeholders
            allocate(v_s_S_space(3,this%S_dim), source=0.)
            if (this%in_wake) then
                allocate(v_d_M_space(3,2*this%M_dim), source=0.)
            else
                allocate(v_d_M_space(3,this%M_dim), source=0.)
            end if

        end if
    
    end subroutine panel_calc_velocity_influences


    subroutine panel_calc_velocities(this, P, freestream, dod_info, mirror_panel, sigma, mu, &
                                     N_body_panels, N_body_verts, asym_flow, v_s, v_d)
        ! Calculates the velocity induced at the given point

        implicit none
        
        class(panel),intent(in) :: this
        real,dimension(3),intent(in) :: P
        type(flow),intent(in) :: freestream
        type(dod),intent(in) :: dod_info
        logical,intent(in) :: mirror_panel, asym_flow
        real,dimension(:),allocatable,intent(in) :: sigma, mu
        integer,intent(in) :: N_body_panels, N_body_verts
        real,dimension(3),intent(out) :: v_d, v_s

        real,dimension(:,:),allocatable :: source_inf, doublet_inf
        real,dimension(:),allocatable :: doublet_strengths
        real,dimension(this%S_dim) :: source_strengths

        ! Get influences
        call this%calc_velocity_influences(P, freestream, dod_info, mirror_panel, source_inf, doublet_inf)

        ! Get strengths
        source_strengths = this%get_source_strengths(sigma, mirror_panel, N_body_panels, asym_flow)
        doublet_strengths = this%get_doublet_strengths(mu, mirror_panel, N_body_verts, asym_flow)

        ! Apply strengths to calculate potentials
        v_s = matmul(source_inf, source_strengths)

        if (this%in_wake) then
            v_d = matmul((doublet_inf(:,1:this%M_dim)+doublet_inf(:,this%M_dim+1:)), doublet_strengths)
        else
            v_d = matmul(doublet_inf, doublet_strengths)
        end if

    end subroutine panel_calc_velocities


    function panel_get_source_strengths(this, sigma, mirror, N_body_panels, asym_flow) result(sigma_strengths)
        ! Returns a vector of the relevant source strengths for this panel

        implicit none
        
        class(panel),intent(in) :: this
        real,dimension(:),allocatable,intent(in) :: sigma
        logical,intent(in) :: mirror
        integer,intent(in) :: N_body_panels
        logical,intent(in) :: asym_flow

        real,dimension(this%S_dim) :: sigma_strengths

        integer :: i

        ! Check we're not in the wake
        if (.not. this%in_wake) then

            ! Get strengths
            sigma_strengths = 0.
            do i=1,size(this%i_panel_s)

                ! Handle mirroring
                if (asym_flow) then
                    if (mirror) then
                        if (this%i_panel_s(i) > N_body_panels) then
                            sigma_strengths(i) = sigma(this%i_panel_s(i) - N_body_panels)
                        else
                            sigma_strengths(i) = sigma(this%i_panel_s(i) + N_body_panels)
                        end if
                    else
                        sigma_strengths(i) = sigma(this%i_panel_s(i))
                    end if
                else
                    if (this%i_panel_s(i) > N_body_panels) then
                        sigma_strengths(i) = sigma(this%i_panel_s(i) - N_body_panels)
                    else
                        sigma_strengths(i) = sigma(this%i_panel_s(i))
                    end if
                end if

            end do

        end if
        
    end function panel_get_source_strengths


    function panel_get_source_parameters(this, sigma, mirror, N_body_panels, asym_flow) result(sigma_params)
        ! Returns a vector describing the distribution of source strength across the panel surface

        implicit none
        
        class(panel),intent(in) :: this
        real,dimension(:),allocatable,intent(in) :: sigma
        logical,intent(in) :: mirror
        integer,intent(in) :: N_body_panels
        logical,intent(in) :: asym_flow
        
        real,dimension(this%sigma_dim) :: sigma_params

        real,dimension(this%S_dim) :: sigma_strengths

        ! Get strengths
        sigma_strengths = this%get_source_strengths(sigma, mirror, N_body_panels, asym_flow)

        ! Linear sources
        if (this%sigma_dim > 1) then

            if (mirror) then
                sigma_params = matmul(this%T_sigma_mir, sigma_strengths)
            else
                sigma_params = matmul(this%T_sigma, sigma_strengths)
            end if

        ! Constant sources
        else
            sigma_params(1) = sigma_strengths(1)
        end if
            
    end function panel_get_source_parameters


    function panel_get_doublet_strengths(this, mu, mirror, N_body_verts, asym_flow) result(mu_strengths)
        ! Returns the relevant doublet strengths for this panel

        implicit none
        
        class(panel),intent(in) :: this
        real,dimension(:),allocatable,intent(in) :: mu
        logical,intent(in) :: mirror
        integer,intent(in) :: N_body_verts
        logical,intent(in) :: asym_flow

        real,dimension(:),allocatable :: mu_strengths

        integer :: shift, i, i_top, i_bot

        ! Determine shift
        if (mirror) then
            shift = size(mu)/2
        else
            shift = 0
        end if

        ! Allocate
        allocate(mu_strengths(this%M_dim))

        ! Get doublet strengths based on parents
        if (this%in_wake) then
            do i=1,this%N
                i_top = this%vertices(i)%ptr%top_parent + shift
                i_bot = this%vertices(i)%ptr%bot_parent + shift
                if (i_top > size(mu)) i_top = i_top - size(mu)
                if (i_bot > size(mu)) i_bot = i_bot - size(mu)
                mu_strengths(i) = mu(i_top) - mu(i_bot)
            end do

        ! Get doublet strengths at vertices
        else
            do i=1,size(this%i_vert_d)

                ! Handle mirroring
                if (asym_flow) then
                    if (mirror) then
                        if (this%i_vert_d(i) > N_body_verts) then
                            mu_strengths(i) = mu(this%i_vert_d(i) - N_body_verts)
                        else
                            mu_strengths(i) = mu(this%i_vert_d(i) + N_body_verts)
                        end if
                    else
                        mu_strengths(i) = mu(this%i_vert_d(i))
                    end if
                else
                    if (this%i_vert_d(i) > N_body_verts) then
                        mu_strengths(i) = mu(this%i_vert_d(i) - N_body_verts)
                    else
                        mu_strengths(i) = mu(this%i_vert_d(i))
                    end if
                end if

            end do
        end if
        
    end function panel_get_doublet_strengths


    function panel_get_doublet_parameters(this, mu, mirror, N_body_verts, asym_flow) result(mu_params)
        ! Returns a vector describing the distribution of doublet strength across the panel surface

        implicit none
        
        class(panel),intent(in) :: this
        real,dimension(:),allocatable,intent(in) :: mu
        logical,intent(in) :: mirror
        integer,intent(in) :: N_body_verts
        logical,intent(in) :: asym_flow

        real,dimension(this%mu_dim) :: mu_params

        real,dimension(this%M_dim) :: mu_verts

        ! Get doublet strengths at vertices
        mu_verts = this%get_doublet_strengths(mu, mirror, N_body_verts, asym_flow)

        ! Calculate doublet parameters
        if (mirror) then
            mu_params = matmul(this%T_mu_mir, mu_verts)
        else
            mu_params = matmul(this%T_mu, mu_verts)
        end if
        
    end function panel_get_doublet_parameters


    function panel_get_velocity_jump(this, mu, sigma, mirrored, N_body_panels, N_body_verts, asym_flow, point) result(dv)
        ! Calculates the jump in perturbation velocity across this panel in global coordinates at the given location
        ! If a location is not given, this will default to the centroid

        implicit none

        class(panel),intent(in) :: this
        real,dimension(:),allocatable,intent(in) :: mu, sigma
        logical,intent(in) :: mirrored, asym_flow
        integer,intent(in) :: N_body_panels, N_body_verts
        real,dimension(3),intent(in),optional :: point

        real,dimension(3) :: dv

        real,dimension(this%mu_dim) :: mu_params
        real,dimension(this%sigma_dim) :: sigma_params
        real,dimension(3) :: Q_ls, s_dir
        real :: s

        ! Get point
        if (present(point)) then
            Q_ls = this%get_local_coords_of_point(point, mirrored)
        else
            Q_ls = 0.
        end if

        ! Calculate doublet parameters (derivatives)
        mu_params = this%get_doublet_parameters(mu, mirrored, N_body_verts, asym_flow)

        ! Calculate tangential velocity jump in panel coordinates E&M Eq. (N.1.11b)
        if (this%order == 2) then
            dv(1) = mu_params(2) + mu_params(4)*Q_ls(1) + mu_params(5)*Q_ls(2)
            dv(2) = mu_params(3) + mu_params(5)*Q_ls(1) + mu_params(6)*Q_ls(2)
        else
            dv(1) = mu_params(2)
            dv(2) = mu_params(3)
        end if
        dv(3) = 0.

        ! Transform to global coordinates
        if (mirrored) then
            dv = matmul(transpose(this%A_g_to_ls_mir), dv)
        else
            dv = matmul(transpose(this%A_g_to_ls), dv)
        end if

        ! Add source component
        if (this%has_sources) then

            ! Get source direction
            if (mirrored) then
                s_dir = this%n_g_mir/inner(this%nu_g_mir, this%n_g_mir)
            else
                s_dir = this%n_g/inner(this%nu_g, this%n_g)
            end if

            ! Source strength
            sigma_params = this%get_source_parameters(sigma, mirrored, N_body_panels, asym_flow)
            if (this%sigma_dim > 1) then
                s = sigma_params(1) + sigma_params(2)*Q_ls(1) + sigma_params(3)*Q_ls(2)
            else
                s = sigma_params(1)
            end if

            ! Add normal velocity jump in global coords E&M Eq. (N.1.11b)
            dv = dv + s*s_dir

        end if

    end function panel_get_velocity_jump


    function panel_get_velocity(this, mu, sigma, mirrored, N_body_panels, N_body_verts, asym_flow, &
                                freestream, inner_flow, point) result(v)
        ! Calculates the velocity on the outside of this panel in global coordinates at the given location
        ! If a location is not given, this will default to the centroid

        implicit none

        class(panel),intent(in) :: this
        real,dimension(:),allocatable,intent(in) :: mu, sigma
        logical,intent(in) :: mirrored, asym_flow
        integer,intent(in) :: N_body_panels, N_body_verts
        type(flow),intent(in) :: freestream
        real,dimension(3),intent(in) :: inner_flow
        real,dimension(3),intent(in),optional :: point

        real,dimension(3) :: v

        real,dimension(3) :: dv

        ! Get velocity jump
        if (present(point)) then
            dv = this%get_velocity_jump(mu, sigma, mirrored, N_body_panels, N_body_verts, asym_flow, point)
        else
            dv = this%get_velocity_jump(mu, sigma, mirrored, N_body_panels, N_body_verts, asym_flow)
        end if

        ! Get total velocity
        v = freestream%U*(inner_flow + dv)

    end function panel_get_velocity


    function panel_get_quadratic_pressure_params(this, mu, sigma, mirrored, N_body_panels, N_body_verts, asym_flow, &
                                         freestream, inner_flow, mirror_plane, rule, M_corr) result(C_P_params)
        ! Calculates the average pressure coefficient on the panel

        implicit none

        class(panel),intent(in) :: this
        real,dimension(:),allocatable,intent(in) :: mu, sigma
        logical,intent(in) :: mirrored, asym_flow
        integer,intent(in) :: N_body_panels, N_body_verts
        type(flow),intent(in) :: freestream
        real,dimension(3),intent(in) :: inner_flow
        integer,intent(in) :: mirror_plane
        character(len=*),intent(in) :: rule
        real,intent(in),optional :: M_corr

        real,dimension(6) :: C_P_params

        integer :: i, i_next
        real,dimension(3) :: v, p
        real,dimension(6) :: C_P

        ! Get pressures at vertices
        do i=1,3

            ! Get velocity
            v = this%get_velocity(mu, sigma, mirrored, N_body_panels, N_body_verts, asym_flow, &
                                  freestream, inner_flow, point=this%get_vertex_loc(i))

            ! Get pressure
            if (present(M_corr)) then
                C_P(i) = freestream%get_C_P(v, rule=rule, M_corr=M_corr)
            else
                C_P(i) = freestream%get_C_P(v, rule=rule)
            end if

        end do

        ! Get pressures at midpoints
        do i=1,3

            ! Get point
            i_next = modulo(i, 3) + 1
            p = 0.5*(this%get_vertex_loc(i) + this%get_vertex_loc(i_next))
            if (mirrored) p = mirror_across_plane(p, mirror_plane)

            ! Get velocity
            v = this%get_velocity(mu, sigma, mirrored, N_body_panels, N_body_verts, asym_flow, &
                                  freestream, inner_flow, point=p)

            ! Get pressure
            if (present(M_corr)) then
                C_P(i+3) = freestream%get_C_P(v, rule=rule, M_corr=M_corr)
            else
                C_P(i+3) = freestream%get_C_P(v, rule=rule)
            end if

        end do

        ! Get pressure distribution parameters
        if (mirrored) then
            C_P_params = matmul(this%S_mu_inv_mir, C_P)
        else
            C_P_params = matmul(this%S_mu_inv, C_P)
        end if
        
    end function panel_get_quadratic_pressure_params


    function panel_get_avg_pressure_coef(this, mu, sigma, mirrored, N_body_panels, N_body_verts, asym_flow, &
                                         freestream, inner_flow, mirror_plane, rule, M_corr) result(C_P_avg)
        ! Calculates the average pressure coefficient on the panel

        implicit none

        class(panel),intent(in) :: this
        real,dimension(:),allocatable,intent(in) :: mu, sigma
        logical,intent(in) :: mirrored, asym_flow
        integer,intent(in) :: N_body_panels, N_body_verts
        type(flow),intent(in) :: freestream
        real,dimension(3),intent(in) :: inner_flow
        integer,intent(in) :: mirror_plane
        character(len=*),intent(in) :: rule
        real,intent(in),optional :: M_corr

        real :: C_P_avg

        real,dimension(3) :: v
        real,dimension(6) :: C_P_params

        ! Constant pressure
        if (this%order == 1) then

            ! Get velocity at centroid
            v = this%get_velocity(mu, sigma, mirrored, N_body_panels, N_body_verts, asym_flow, &
                                  freestream, inner_flow)

            ! Get pressure
            if (present(M_corr)) then
                C_P_avg = freestream%get_C_P(v, rule=rule, M_corr=M_corr)
            else
                C_P_avg = freestream%get_C_P(v, rule=rule)
            end if

        ! Quadratic pressure
        else

            ! Get pressure distribution parameters
            if (present(M_corr)) then
                C_P_params = this%get_quadratic_pressure_params(mu, sigma, mirrored, N_body_panels, N_body_verts, asym_flow, &
                                                                freestream, inner_flow, mirror_plane, rule, M_corr)
            else
                C_P_params = this%get_quadratic_pressure_params(mu, sigma, mirrored, N_body_panels, N_body_verts, asym_flow, &
                                                                freestream, inner_flow, mirror_plane, rule)
            end if

            ! Integrate
            if (mirrored) then

                ! Integrate
                C_P_avg = this%C_mir(0,0)*C_P_params(1) + this%C_mir(1,0)*C_P_params(2) + this%C_mir(0,1)*C_P_params(3) + &
                          0.5*this%C_mir(2,0)*C_P_params(4) + this%C_mir(1,1)*C_P_params(5) + 0.5*this%C_mir(0,2)*C_P_params(6)

                ! Calculate average
                C_P_avg = this%J_mir*C_P_avg/this%A

            else

                ! Integrate
                C_P_avg = this%C(0,0)*C_P_params(1) + this%C(1,0)*C_P_params(2) + this%C(0,1)*C_P_params(3) + &
                          0.5*this%C(2,0)*C_P_params(4) + this%C(1,1)*C_P_params(5) + 0.5*this%C(0,2)*C_P_params(6)

                ! Calculate average
                C_P_avg = this%J*C_P_avg/this%A

            end if

        end if

    end function panel_get_avg_pressure_coef


    function panel_get_moment_about_centroid(this, mu, sigma, mirrored, N_body_panels, N_body_verts, asym_flow, &
                                                   freestream, inner_flow, mirror_plane, rule, M_corr) result(C_M)
        ! Calculates the moment coefficient (units of length) about the panel centroid

        implicit none

        class(panel),intent(in) :: this
        real,dimension(:),allocatable,intent(in) :: mu, sigma
        logical,intent(in) :: mirrored, asym_flow
        integer,intent(in) :: N_body_panels, N_body_verts
        type(flow),intent(in) :: freestream
        real,dimension(3),intent(in) :: inner_flow
        integer,intent(in) :: mirror_plane
        character(len=*),intent(in) :: rule
        real,intent(in),optional :: M_corr

        real,dimension(3) :: C_M

        real,dimension(6) :: C_P_params

        ! Constant pressure
        if (this%order == 1) then

            C_M = 0.

        ! Quadratic pressure
        else

            ! Get pressure distribution parameters
            if (present(M_corr)) then
                C_P_params = this%get_quadratic_pressure_params(mu, sigma, mirrored, N_body_panels, N_body_verts, asym_flow, &
                                                                freestream, inner_flow, mirror_plane, rule, M_corr)
            else
                C_P_params = this%get_quadratic_pressure_params(mu, sigma, mirrored, N_body_panels, N_body_verts, asym_flow, &
                                                                freestream, inner_flow, mirror_plane, rule)
            end if

            ! Integrate
            if (mirrored) then

                ! Integrate
                C_M(1) = this%C_mir(1,0)*C_P_params(1) + this%C_mir(2,0)*C_P_params(2) + this%C_mir(1,1)*C_P_params(3) + &
                         0.5*this%C_mir(3,0)*C_P_params(4) + this%C_mir(2,1)*C_P_params(5) + 0.5*this%C_mir(1,2)*C_P_params(6)
                C_M(2) = this%C_mir(0,1)*C_P_params(1) + this%C_mir(1,1)*C_P_params(2) + this%C_mir(0,2)*C_P_params(3) + &
                         0.5*this%C_mir(2,1)*C_P_params(4) + this%C_mir(1,2)*C_P_params(5) + 0.5*this%C_mir(0,3)*C_P_params(6)
                C_M(3) = 0.

                ! Apply area factor and transform
                C_M = this%J_mir*cross(this%n_g_mir, matmul(this%A_ls_to_g_mir, C_M))

            else

                ! Integrate
                C_M(1) = this%C(1,0)*C_P_params(1) + this%C(2,0)*C_P_params(2) + this%C(1,1)*C_P_params(3) + &
                         0.5*this%C(3,0)*C_P_params(4) + this%C(2,1)*C_P_params(5) + 0.5*this%C(1,2)*C_P_params(6)
                C_M(2) = this%C(0,1)*C_P_params(1) + this%C(1,1)*C_P_params(2) + this%C(0,2)*C_P_params(3) + &
                         0.5*this%C(2,1)*C_P_params(4) + this%C(1,2)*C_P_params(5) + 0.5*this%C(0,3)*C_P_params(6)
                C_M(3) = 0.

                ! Apply area factor and transform
                C_M = this%J*cross(this%n_g, matmul(this%A_ls_to_g, C_M))

            end if

        end if
        
    end function panel_get_moment_about_centroid

    
end module panel_mod
