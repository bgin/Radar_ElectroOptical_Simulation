! Types for the most basic geometric objects
module base_geom_mod

    use linked_list_mod
    use math_mod
    use helpers_mod

    implicit none

    integer,parameter :: INTERNAL = 1
    integer,parameter :: EXTERNAL = 2
    integer,parameter :: SURFACE = 3

    integer,parameter :: ZERO_POTENTIAL = 1
    integer,parameter :: SF_POTENTIAL = 2
    integer,parameter :: ZERO_NORMAL_MF = 3
    integer,parameter :: STRENGTH_MATCHING = 4 
    integer,parameter :: ZERO_NORMAL_VEL = 5
    integer,parameter :: ZERO_X_VEL = 6
    integer,parameter :: MF_INNER_FLOW = 7

    integer,parameter :: TT_VERTEX = 1
    integer,parameter :: TT_PANEL = 2


    type vertex
        ! A vertex in 3-space

        real,dimension(3) :: loc ! Location
        real,dimension(3) :: n_g, n_g_mir ! Normal vector associated with this vertex
        real :: l_avg ! Average of the edge lengths adjacent to this vertex
        real :: l_min ! Minimum of the edge lengths adjacent to this vertex
        type(list) :: adjacent_vertices ! List of indices for the vertices which share an edge with this vertex
        type(list) :: adjacent_edges ! List of indices for the edges which touch this vertex
        type(list) :: panels ! List of indices for the panels which connect to this vertex
        type(list) :: panels_not_across_wake_edge ! List of indices for the panels which connect to this vertex not across a wake-shedding edge
        integer :: N_wake_edges ! Number of wake edges this vertex belongs to
        integer :: index ! Index of this vertex in the mesh
        integer :: top_parent, bot_parent ! Indices of the top and bottom vertices this vertex's strength is determined by (for a wake vertex)
        logical :: on_mirror_plane ! Whether this vertex lies in the mirroring plane
        logical :: clone ! Whether this vertex needs a clone depending on whether it's in a wake-shedding edge
        logical :: mirrored_is_unique ! Whether this vertice's mirror image will be the same for an asymmetric freestream condition
        logical :: convex ! Whether the mesh is convex at this vertex
        integer :: N_needed_clones

        contains

            procedure :: init => vertex_init

            ! Initializer-setters
            procedure :: set_whether_on_mirror_plane => vertex_set_whether_on_mirror_plane
            procedure :: set_average_edge_length => vertex_set_average_edge_length
            procedure :: set_needed_clones => vertex_set_needed_clones

            ! Getters
            procedure :: get_needed_clones => vertex_get_needed_clones

            ! Cloning
            procedure :: copy_to => vertex_copy_to

    end type vertex


    type vertex_pointer
        ! A pointer to a vertex, for creating vertex arrays

        type(vertex),pointer :: ptr

    end type vertex_pointer


    type edge
        ! A mesh edge

        integer,dimension(2) :: top_verts ! Indices of the end vertices in the mesh vertex array belonging to the top panel
        integer,dimension(2) :: bot_verts ! Indices of the end vertices in the mesh vertex array belonging to the bottom panel
        integer,dimension(2) :: panels ! Indices of the top and bottom (an odd thing to call these) panels for this edge
        integer :: top_midpoint, bot_midpoint
        integer,dimension(2) :: edge_index_for_panel ! Index of the edge which this is for each panel; edge should proceed counterclockwise for the top panel
        logical :: on_mirror_plane ! Whether this edge lies on the mirror plane
        logical :: sheds_wake ! Whether this edge sheds a wake
        logical :: discontinuous ! Whether this edge is discontinuous in a geometric sense

        contains

            procedure :: init => edge_init
            procedure :: get_opposing_panel => edge_get_opposing_panel
            procedure :: touches_vertex => edge_touches_vertex
            procedure :: point_top_to_new_vert => edge_point_top_to_new_vert
            procedure :: point_bottom_to_new_vert => edge_point_bottom_to_new_vert
            procedure :: shares_panel_with => edge_shares_panel_with
            procedure :: get_opposite_endpoint => edge_get_opposite_endpoint

    end type edge


    type eval_point_geom
        ! Container type for the geometric parameters necessary for calculating a panel's influence on a given field point

        real,dimension(3) :: P_g ! Point position in global coords
        real,dimension(2) :: P_ls ! Transformed point in panel plane
        real,dimension(2,3) :: d_ls ! Local displacements from panel vertices
        real :: h, h2 ! Transformed height above panel
        real,dimension(3) :: a, l1, l2, R1, R2, dR ! Edge integration parameters for the Ehlers-Johnson method
        real,dimension(3) :: g2 ! Also
        real,dimension(3) :: v_xi, v_eta ! Edge in-plane normal vectors

        contains

            procedure :: init => eval_point_geom_init

    end type eval_point_geom


    type control_point

        real,dimension(3) :: loc
        integer :: cp_type ! 1 = internal, 2 = external, 3 = surface
        integer :: bc=0 ! Boundary condition: 0 = not initialized, 1 = zero perturbation potential (for Morino), 2 = source-free internal potential, 3 = zero normal velocity, 4 = strength-matching (for mirroring)
        real,dimension(3) :: n_g ! Normal vector associated with bc = 3
        logical :: is_mirror ! Whether this control point belongs to the mirrored half of the mesh
        integer :: tied_to_type ! 1 = this control point is associated with a vertex, 2 = panel
        integer :: tied_to_index ! Index of the mesh component this control point is tied to
    
        contains

            procedure :: init => control_point_init
            procedure :: set_bc => control_point_set_bc
    
    end type control_point

    
contains


    subroutine vertex_init(this, loc, index)
        ! Initializes a vertex

        implicit none

        class(vertex),intent(inout) :: this
        real,dimension(3),intent(in) :: loc
        integer,intent(in) :: index

        ! Store info
        this%loc = loc
        this%index = index

        ! Intitialize some data
        this%top_parent = 0
        this%bot_parent = 0

        ! Default cases
        this%mirrored_is_unique = .true.
        this%clone = .false.
        this%N_needed_clones = 0
        this%on_mirror_plane = .false.
        this%N_wake_edges = 0

    end subroutine vertex_init


    subroutine vertex_set_average_edge_length(this, vertices)
        ! Calculates the average edge length of edges adjacent to this vertex

        implicit none

        class(vertex),intent(inout) :: this
        type(vertex),dimension(:),allocatable,intent(in) :: vertices

        integer :: i, adj_ind, N
        real :: l_i

        ! Loop through adjacent vertices
        this%l_avg = 0.
        this%l_min = huge(this%l_min)
        N = 0
        do i=1,this%adjacent_vertices%len()
            
            ! Get index of adjacent vertex
            call this%adjacent_vertices%get(i, adj_ind)

            ! Calculate edge length
            l_i = dist(this%loc, vertices(adj_ind)%loc)

            ! Get minimum
            this%l_min = min(this%l_min, l_i)

            ! For a vertex on the mirror plane where the adjacent vertex is not on the mirror plane
            ! that length will need to be added twice
            if (this%on_mirror_plane .and. .not. vertices(adj_ind)%on_mirror_plane) then

                ! Add twice
                this%l_avg = this%l_avg + 2*l_i
                N = N + 2
                
            else

                ! Add once
                this%l_avg = this%l_avg + l_i
                N = N + 1

            end if

        end do
        
        ! Compute average
        if (N > 0) then
            this%l_avg = this%l_avg/N
        else
            this%l_avg = 1.
        end if
    
    end subroutine vertex_set_average_edge_length


    subroutine vertex_set_whether_on_mirror_plane(this, mirror_plane)
        ! Sets the member variable telling whether this vertex is on the mirror plane

        implicit none
        
        class(vertex), intent(inout) :: this
        integer, intent(in) :: mirror_plane

        ! Check distance from mirror plane
        if (abs(this%loc(mirror_plane))<1e-12) then

            ! The vertex is on the mirror plane
            this%on_mirror_plane = .true.

        end if
    
        
    end subroutine vertex_set_whether_on_mirror_plane


    subroutine vertex_set_needed_clones(this, mesh_edges)
        ! Determines and sets how many clones this vertex needs

        implicit none
        
        class(vertex),intent(inout) :: this
        type(edge),dimension(:),allocatable,intent(in) :: mesh_edges

        integer :: j, i_edge, N_wake_edges_on_mirror_plane

        ! Initialize
        this%N_wake_edges = 0
        this%N_needed_clones = 0
        N_wake_edges_on_mirror_plane = 0

        ! Loop through edges
        do j=1,this%adjacent_edges%len()

            ! Get edge index
            call this%adjacent_edges%get(j, i_edge)

            ! Check if this is a wake-shedding edge
            if (mesh_edges(i_edge)%sheds_wake) then

                ! Update number of wake edges touching this vertex
                this%N_wake_edges = this%N_wake_edges + 1

                ! Check if this edge is on the mirror plane
                if (mesh_edges(i_edge)%on_mirror_plane) then

                    ! Update counter
                    N_wake_edges_on_mirror_plane = N_wake_edges_on_mirror_plane + 1

                end if

            end if
        end do

        ! Set number of needed clones for vertices which have at least one wake edge
        if (this%N_wake_edges > 0) then

            ! This depends on how many wake edges it has
            ! If the vertex is on the mirror plane, then how many clones is dependent upon how many wake edges are on the mirror plane
            if (this%on_mirror_plane) then
                this%N_needed_clones = this%N_wake_edges - N_wake_edges_on_mirror_plane

            ! If the vertex is not on the mirror plane, then we need one fewer clones than edges
            else
                this%N_needed_clones = this%N_wake_edges - 1
            end if
            
        end if
        
    end subroutine vertex_set_needed_clones


    function vertex_get_needed_clones(this) result(N_clones)
        ! Returns the number of clones this vertex needs

        implicit none
        
        class(vertex),intent(in) :: this

        integer :: N_clones

        N_clones = this%N_needed_clones
        
    end function vertex_get_needed_clones


    subroutine vertex_copy_to(this, new_vert)
        ! Copies the information to the new vertex which will be the same for a clone

        implicit none
        
        class(vertex),intent(in) :: this
        type(vertex),intent(inout) :: new_vert

        integer :: k, i_panel, i_vert, i_edge

        ! Store number of adjacent wake-shedding and discontinuous edges (probably unecessary at this point, but let's be consistent)
        new_vert%N_wake_edges = this%N_wake_edges

        ! Mirroring properties
        new_vert%on_mirror_plane = this%on_mirror_plane

        ! Geometry
        new_vert%n_g = this%n_g
        new_vert%n_g_mir = this%n_g_mir
        new_vert%l_avg = this%l_avg

        ! Copy over adjacent panels
        do k=1,this%panels%len()

            ! Get adjacent panel index from original vertex
            call this%panels%get(k, i_panel)

            ! Copy to clone
            call new_vert%panels%append(i_panel)

        end do

        ! Copy over adjacent vertices
        do k=1,this%adjacent_vertices%len()

            ! Get adjacent vertex index from original vertex
            call this%adjacent_vertices%get(k, i_vert)

            ! Copy to new vertex
            call new_vert%adjacent_vertices%append(i_vert)

        end do

        ! Copy over adjacent edges
        do k=1,this%adjacent_edges%len()

            ! Get adjacent edge index from original vertex
            call this%adjacent_edges%get(k, i_edge)

            ! Copy to new vertex
            call new_vert%adjacent_edges%append(i_edge)

        end do
    
        
    end subroutine vertex_copy_to


    subroutine edge_init(this, i1, i2, top_panel, bottom_panel)

        implicit none

        class(edge),intent(inout) :: this
        integer,intent(in) :: i1, i2
        integer,intent(in) :: top_panel, bottom_panel

        ! Store indices
        this%top_verts(1) = i1
        this%top_verts(2) = i2

        ! Store panels
        this%panels(1) = top_panel
        this%panels(2) = bottom_panel

        ! Set defaults
        this%on_mirror_plane = .false.
        this%sheds_wake = .false.
        this%bot_verts = this%top_verts
    
    end subroutine edge_init


    function edge_get_opposing_panel(this, i_panel) result(i_oppose)
        ! Returns the index of the panel opposite this one on the edge

        implicit none
        
        class(edge),intent(in) :: this
        integer,intent(in) :: i_panel

        integer :: i_oppose

        if (i_panel == this%panels(1)) then
            i_oppose = this%panels(2)
        else if (i_panel == this%panels(2)) then
            i_oppose = this%panels(1)
        else
            i_oppose = 0
        end if
        
    end function edge_get_opposing_panel


    function edge_touches_vertex(this, i_vert) result(touches)
        ! Checks whether the edge touches the given vertex

        implicit none
        
        class(edge),intent(in) :: this
        integer,intent(in) :: i_vert

        logical :: touches

        touches = this%top_verts(1) == i_vert .or. this%top_verts(2) == i_vert
        
    end function edge_touches_vertex


    subroutine edge_point_top_to_new_vert(this, i_orig_vert, i_new_vert)
        ! Overwrites the edge's dependency on i_orig_vert in the top vertices with i_new_vert

        implicit none
        
        class(edge),intent(inout) :: this
        integer,intent(in) :: i_orig_vert, i_new_vert

        if (this%top_verts(1) == i_orig_vert) then
            this%top_verts(1) = i_new_vert
        else if (this%top_verts(2) == i_orig_vert) then
            this%top_verts(2) = i_new_vert
        else if (this%top_midpoint == i_orig_vert) then
            this%top_midpoint = i_new_vert
        end if
        
    end subroutine edge_point_top_to_new_vert


    subroutine edge_point_bottom_to_new_vert(this, i_orig_vert, i_new_vert)
        ! Overwrites the edge's dependency on i_orig_vert in the bottom vertices with i_new_vert

        implicit none
        
        class(edge),intent(inout) :: this
        integer,intent(in) :: i_orig_vert, i_new_vert

        if (this%bot_verts(1) == i_orig_vert) then
            this%bot_verts(1) = i_new_vert
        else if (this%bot_verts(2) == i_orig_vert) then
            this%bot_verts(2) = i_new_vert
        else if (this%bot_midpoint == i_orig_vert) then
            this%bot_midpoint = i_new_vert
        end if
        
    end subroutine edge_point_bottom_to_new_vert


    function edge_shares_panel_with(this, other_edge) result(shares)
        ! Checks whether this edge shares a panel with the other edge

        implicit none
        
        class(edge),intent(in) :: this
        type(edge),intent(in) :: other_edge

        logical :: shares
    
        shares = (this%panels(1) == other_edge%panels(1)) .or. &
                 (this%panels(1) == other_edge%panels(2)) .or. &
                 (this%panels(2) == other_edge%panels(2)) .or. &
                 (this%panels(2) == other_edge%panels(1))
        
    end function edge_shares_panel_with


    function edge_get_opposite_endpoint(this, i_vert, mesh_verts) result(i_opp_vert)
        ! Returns the index of the top vertex opposite the given vertex (may be top or bottom) on this edge

        implicit none
        
        class(edge),intent(in) :: this
        integer,intent(in) :: i_vert
        type(vertex),dimension(:),allocatable :: mesh_verts

        integer :: i_opp_vert
    
        if (dist(mesh_verts(i_vert)%loc, mesh_verts(this%top_verts(1))%loc) < 1.e-12) then
            i_opp_vert = this%top_verts(2)
        else
            i_opp_vert = this%top_verts(1)
        end if
        
    end function edge_get_opposite_endpoint


    subroutine eval_point_geom_init(this, P_g, P_ls)

        implicit none

        class(eval_point_geom),intent(out) :: this
        real,dimension(3),intent(in) :: P_g, P_ls

        ! Store point
        this%P_g = P_g

        ! Transform to local scaled coordinates
        this%P_ls = P_ls(1:2)
        this%h = P_ls(3) ! Equivalent to E&M Eq. (J.7.41)
        this%h2 = this%h**2

        ! These are sometimes accessed when the DoD is not checked, so they need to be set to zero
        this%R1 = 0.
        this%R2 = 0.
        this%a = 0.
    
    end subroutine eval_point_geom_init


    subroutine control_point_init(this, loc, cp_type, tied_to_type, tied_to_index)
        ! Initializes this control point

        implicit none
        
        class(control_point),intent(inout) :: this
        real,dimension(3),intent(in) :: loc
        integer,intent(in) :: cp_type, tied_to_type, tied_to_index

        ! Store
        this%loc = loc
        this%cp_type = cp_type
        this%tied_to_type = tied_to_type
        this%tied_to_index = tied_to_index

        ! Set defaults
        this%is_mirror = .false.
        
    end subroutine control_point_init


    subroutine control_point_set_bc(this, bc, n)
        ! Sets up the boundary condition on this control point

        implicit none
        
        class(control_point),intent(inout) :: this
        integer,intent(in) :: bc
        real,dimension(3),optional :: n

        ! Store type
        this%bc = bc

        ! Get normal (if needed)
        if (this%bc == ZERO_NORMAL_MF .or. this%bc == ZERO_NORMAL_VEL .or. this%bc == MF_INNER_FLOW) then
            if (.not. present(n)) then
                write(*,*) "!!! Associated normal vector is required for enforcing a zero-normal-mass-flux &
                            or velocity boundary condition."
                stop
            else
                this%n_g = n
            end if
        end if
        
    end subroutine control_point_set_bc

    
end module base_geom_mod