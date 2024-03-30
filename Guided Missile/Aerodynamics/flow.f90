
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


module flow_mod
    ! ************Slightly modified by Bernard Gingold, 30/03/2024, 10:20AM**************
    use mod_kinds, only : i4,sp
    use json_mod
    use json_xtnsn_mod
    use math_mod
    use linalg_mod
    use helpers_mod

    implicit none
    
    type flow

        real(kind=sp),dimension(:),allocatable :: v_inf ! Freestream velocity
        real(kind=sp) :: M_inf ! Freestream Mach number
        real(kind=sp) :: gamma ! Ratio of specific heats
        real(kind=sp) :: U, U_inv ! Freestream velocity magnitude
        real(kind=sp) :: B ! Compressibility scale factor
        real(kind=sp) :: s ! Sign of 1-M^2; determines character of governing PDE (hyperbolic (s=-1) vs elliptic(s=1))
        real(kind=sp) :: c ! Freestream speed of sound
        real(kind=sp) :: mu, C_mu ! Mach angle
        real(kind=sp) :: K, K_inv ! Kappa factor
        real(kind=sp),dimension(3) :: c_hat_g ! Compressibility axis (assumed in MachLine to be aligned with the freestream direction)
        logical,dimension(3) :: sym_about ! Whether the flow condition is symmetric about any plane
        real(kind=sp),dimension(3,3) :: B_mat_g, B_mat_c, B_mat_g_inv ! Dual metric matrix
        real(kind=sp),dimension(3,3) :: C_mat_g, C_mat_c ! Metric matrix
        logical :: supersonic, incompressible
        real(kind=sp),dimension(3,3) :: A_g_to_c, A_c_to_s, A_g_to_s ! Coordinate transformation matrices
        real(kind=sp) :: a_ise, b_ise, c_ise ! Constant parameters for isentropic pressure coefficient calculations
        real(kind=sp) :: C_P_vac, C_P_stag ! Vacuum and stagnation pressure coefficients for this flow

        contains

            procedure :: init => flow_init
            procedure :: calc_metric_matrices => flow_calc_metric_matrices
            procedure :: calc_transforms => flow_calc_transforms
            procedure :: B_g_inner => flow_B_g_inner
            procedure :: C_g_inner => flow_C_g_inner
            procedure :: point_in_dod => flow_point_in_dod
            procedure :: get_C_P_inc => flow_get_C_P_inc
            procedure :: get_C_P_ise => flow_get_C_P_ise
            procedure :: get_v_pert_c => flow_get_v_pert_c
            procedure :: get_C_P_2nd => flow_get_C_P_2nd
            procedure :: get_C_P_sln => flow_get_C_P_sln
            procedure :: get_C_P_lin => flow_get_C_P_lin
            procedure :: get_C_P_crit => flow_get_C_P_crit
            procedure :: correct_C_P_PG => flow_correct_C_P_PG
            procedure :: correct_C_P_KT => flow_correct_C_P_KT
            procedure :: correct_C_P_L => flow_correct_C_P_L
            procedure :: restrict_pressure => flow_restrict_pressure
            procedure :: get_C_P => flow_get_C_P

    end type flow


contains


    subroutine flow_init(this, settings, spanwise_axis)

        implicit none

        class(flow),intent(inout) :: this
        type(json_value),pointer,intent(in) :: settings
        character(len=:),allocatable,intent(in) :: spanwise_axis

        logical :: found

        ! Get flow params
        call json_get(settings, 'freestream_velocity', this.v_inf, found)
        if (.not. found) then
            write(*,*) "!!! Freestream velocity was not specified. Quitting..."
            stop
        end if
        call json_xtnsn_get(settings, 'freestream_mach_number', this.M_inf, 0.)
        call json_xtnsn_get(settings, 'gamma', this.gamma, 1.4)

        ! Check for positive Mach number (idiot-proofing)
        if (this.M_inf < 0.) then
            write(*,*) "!!! Invalid freestream Mach number selected. Cannot be a negative number. Quitting..."
            stop
        end if

        ! Check symmetry
        this.sym_about = this.v_inf == 0.

        ! Derived quantities
        this.U = norm2(this.v_inf)
        this.U_inv = 1./this.U
        this.c_hat_g = this.v_inf*this.U_inv

        ! Determine condition
        if (this.M_inf == 1.) then
            write(*,*) "!!! A freestream Mach number of 1.0 is not allowed in MachLine. Quitting..."
            stop
        end if
        this.supersonic = this.M_inf > 1.0
        this.incompressible = this.M_inf == 0.

        ! Calculate B and s
        if (this.supersonic) then
            this.B = sqrt(this.M_inf**2 - 1.)
            this.s = -1.
            this.K = 2.*pi
        else
            this.B = sqrt(1. - this.M_inf**2)
            this.s = 1.
            this.K = 4.*pi
        end if
        this.K_inv = 1./this.K

        ! Calculate freestream speed of sound
        this.c = this.M_inf*this.U

        ! Calculate Mach angle (if supersonic; it is meaningless otherwise)
        if (this.supersonic) then
            this.mu = asin(1.0/this.M_inf)
            this.C_mu = cos(this.mu)
        end if

        ! Calculate relevant matrices
        call this.calc_metric_matrices()
        call this.calc_transforms(spanwise_axis)

        if (.not. this.incompressible) then

            ! Parameters for calculating isentropic pressure coefficients
            this.a_ise = 2./(this.gamma*this.M_inf**2)
            this.b_ise = 0.5*(this.gamma-1.)*this.M_inf**2
            this.c_ise = this.gamma/(this.gamma-1.)
        
            ! Vacuum pressure coefficient
            this.C_P_vac = -this.a_ise

            ! Stagnation pressure coefficient
            this.C_P_stag = this.a_ise*((1. + this.b_ise)**this.c_ise - 1.)

        else

            ! Vacuum pressure coefficient (non-physical) (but then again, so are incompressible flows)
            this.C_P_vac = -huge(this.C_P_vac)

            ! Stagnation pressure coefficient
            this.C_P_stag = 1.

        end if

    end subroutine flow_init


    subroutine flow_calc_metric_matrices(this)

        implicit none

        class(flow),intent(inout) :: this

        integer(kind=i4):: i

        ! Assemble dual metric matrix
        ! Global (E&M Eq. (E.3.9))
        do i=1,3
            this.B_mat_g(i,i) = 1.
        end do
        this.B_mat_g = this.B_mat_g - this.M_inf**2*outer(this.c_hat_g, this.c_hat_g)

        ! Invert (used for source-free formulation)
        call matinv(3, this.B_mat_g, this.B_mat_g_inv)

        ! Compressible (E&M Eq. (E.3.8))
        this.B_mat_c = 0.
        this.B_mat_c(1,1) = this.s*this.B**2
        this.B_mat_c(2,2) = 1.
        this.B_mat_c(3,3) = 1.
        
        ! Assemble metric matrix
        ! Global (E&M Eq. (E.3.9))
        do i=1,3
            this.C_mat_g(i,i) = 1.-this.M_inf**2
        end do
        this.C_mat_g = this.C_mat_g + this.M_inf**2*outer(this.c_hat_g, this.c_hat_g)

        ! Compressible (E&M Eq. (E.3.8))
        this.C_mat_c = 0.
        this.C_mat_c(1,1) = 1.
        this.C_mat_c(2,2) = this.s*this.B**2
        this.C_mat_c(3,3) = this.s*this.B**2
    
    end subroutine flow_calc_metric_matrices


    subroutine flow_calc_transforms(this, spanwise_axis)

        implicit none

        class(flow),intent(inout) :: this
        character(len=:),allocatable,intent(in) :: spanwise_axis

        real(kind=sp),dimension(3) :: j_g, c_hat_c

        ! Set positive spanwise axis
        j_g = 0.
        select case (spanwise_axis)

        case ('+x')
            j_g(1) = 1.

        case ('-x')
            j_g(1) = -1.

        case ('+y')
            j_g(2) = 1.

        case ('-y')
            j_g(2) = -1.

        case ('+z')
            j_g(3) = 1.

        case ('-z')
            j_g(3) = -1.

        case default
            j_g(2) = 1.

        end select

        ! Calculate transform from global to compressible coordinates
        this.A_g_to_c = 0.
        this.A_g_to_c(1,:) = this.c_hat_g
        this.A_g_to_c(3,:) = cross(this.c_hat_g, j_g)
        this.A_g_to_c(3,:) = this.A_g_to_c(3,:)/norm2(this.A_g_to_c(3,:))
        this.A_g_to_c(2,:) = cross(this.A_g_to_c(3,:), this.c_hat_g)

        ! Calculate transform from compressible to scaled coordinates
        this.A_c_to_s = 0.
        this.A_c_to_s(1,1) = 1.
        this.A_c_to_s(2,2) = this.B
        this.A_c_to_s(3,3) = this.B

        ! Check calculation
        if (run_checks) then
            c_hat_c = matmul(this.A_g_to_c, this.c_hat_g)
            if (abs(c_hat_c(1)-1.)>1e-12 .or. abs(c_hat_c(2))>1e-12 .or. abs(c_hat_c(3))>1e-12) then
                write(*,*) "!!! Transformation to the compressible coordinate system failed. Quitting..."
                stop
            end if
        end if

        ! Calculate transform from global to scaled coordinates
        this.A_g_to_s = matmul(this.A_c_to_s, this.A_g_to_c)

    end subroutine flow_calc_transforms


    function flow_B_g_inner(this, a, b) result(c)
        ! Calculates the inner product a*B_g*b = c

        implicit none

        class(flow),intent(in) :: this
        real(kind=sp),dimension(3),intent(in) :: a, b
        real(kind=sp) :: c

        c = inner(a, matmul(this.B_mat_g, b))

    end function flow_B_g_inner


    function flow_C_g_inner(this, a, b) result(c)
        ! Calculates the inner product a*C_g*b = c

        implicit none

        class(flow),intent(in) :: this
        real(kind=sp),dimension(3),intent(in) :: a, b
        real(kind=sp) :: c

        c = inner(a, matmul(this.C_mat_g, b))

    end function flow_C_g_inner


    function flow_point_in_dod(this, Q, P) result(in_dod)
        ! Calculates whether the point Q lies in the DoD of point P

        implicit none

        class(flow),intent(in) :: this
        real(kind=sp),dimension(3),intent(in) :: Q, P
        logical :: in_dod

        real(kind=sp),dimension(3) :: d

        in_dod = .false.

        ! Calculate displacement
        d = P-Q

        ! Check upstream
        if (inner(d, this.c_hat_g) >= 0.) then ! E&M Eq. (J.3.1)

            ! Check in dod
            if (this.C_g_inner(d, d) >= 0.) then ! E&M Eq. (J.3.2)

                in_dod = .true.

            end if

        end if
        
    end function flow_point_in_dod


    function flow_get_C_P_inc(this, v) result(C_P_inc)
        ! Calculates the incompressible pressure coefficient for the given velocity

        implicit none
        
        class(flow),intent(in) :: this
        real(kind=sp),dimension(3),intent(in) :: v

        real(kind=sp) :: C_P_inc

        C_P_inc = 1.-inner(v, v)*this.U_inv*this.U_inv
        
    end function flow_get_C_P_inc


    function flow_get_C_P_ise(this, v) result(C_P_ise)
        ! Calculates the isentropic pressure coefficient for the given velocity

        implicit none
        
        class(flow),intent(in) :: this
        real(kind=sp),dimension(3),intent(in) :: v

        real(kind=sp) :: C_P_ise

        ! Calculate
        C_P_ise = this.get_C_P_inc(v)
        C_P_ise = this.a_ise*( (1. + this.b_ise*C_P_ise)**this.c_ise - 1.)

        ! Check for NaN
        if (isnan(C_P_ise)) C_P_ise = this.C_P_vac
        
    end function flow_get_C_P_ise


    function flow_get_v_pert_c(this, v) result(v_pert_c)
        ! Calculates the perturbation velocity expressed in the compressible frame

        implicit none
        
        class(flow),intent(in) :: this
        real(kind=sp),dimension(3),intent(in) :: v

        real(kind=sp),dimension(3) :: v_pert_c

        v_pert_c = matmul(this.A_g_to_c, v - this.v_inf)
        
    end function flow_get_v_pert_c


    function flow_get_C_P_2nd(this, v) result(C_P_2nd)
        ! Calculates the second-order pressure coefficient for the given velocity

        implicit none
        
        class(flow),intent(in) :: this
        real(kind=sp),dimension(3),intent(in) :: v

        real(kind=sp) :: C_P_2nd

        real(kind=sp) :: C_P_sln
        real(kind=sp),dimension(3) :: v_pert_c

        ! Get prerequisites
        C_P_sln = this.get_C_P_sln(v)
        v_pert_c = this.get_v_pert_c(v)

        ! Calculate
        C_P_2nd = C_P_sln - (1.-this.M_inf**2)*v_pert_c(1)**2*this.U_inv**2
        call this.restrict_pressure(C_P_2nd)
        
    end function flow_get_C_P_2nd


    function flow_get_C_P_sln(this, v) result(C_P_sln)
        ! Calculates the slender-body pressure coefficient for the given velocity

        implicit none
        
        class(flow),intent(in) :: this
        real(kind=sp),dimension(3),intent(in) :: v

        real(kind=sp) :: C_P_sln

        real(kind=sp) :: C_P_lin
        real(kind=sp),dimension(3) :: v_pert_c

        ! Get prerequisites
        C_P_lin = this.get_C_P_lin(v)
        v_pert_c = this.get_v_pert_c(v)

        C_P_sln = C_P_lin - (v_pert_c(2)**2 + v_pert_c(3)**2)*this.U_inv**2
        call this.restrict_pressure(C_P_sln)
        
    end function flow_get_C_P_sln


    function flow_get_C_P_lin(this, v) result(C_P_lin)
        ! Calculates the second-order pressure coefficient for the given velocity

        implicit none
        
        class(flow),intent(in) :: this
        real(kind=sp),dimension(3),intent(in) :: v

        real(kind=sp) :: C_P_lin

        real(kind=sp),dimension(3) :: v_pert_c

        ! Get prerequisites
        v_pert_c = this.get_v_pert_c(v)

        C_P_lin = -2.*v_pert_c(1)*this.U_inv
        call this.restrict_pressure(C_P_lin)
        
    end function flow_get_C_P_lin


    function flow_get_C_P_crit(this, M) result(C_P_crit)
        ! Calculates the critical pressure coefficient for the given Mach number

        implicit none
        
        class(flow),intent(in) :: this
        real(kind=sp),intent(in) :: M

        real(kind=sp) :: C_P_crit

        real(kind=sp) :: x, n, d, M2

        ! Modern Compressible Flow by John Anderson Eq. (9.55)
        M2 = M*M
        x = 0.5*(this.gamma - 1.)
        n = 1. + x*M2
        d = 1. + x
        C_P_crit = 2./(this.gamma*M2)*((n/d)**(this.gamma/(this.gamma-1.)) - 1.)
        
    end function flow_get_C_P_crit


    function flow_correct_C_P_PG(this, C_P_inc, M_corr) result(C_P_PG)
        ! Corrects the given incompressible pressure coefficient for compressibility using the Prandtl-Glauert correction

        implicit none
        
        class(flow),intent(in) :: this
        real(kind=sp),intent(in) :: C_P_inc, M_corr

        real(kind=sp) :: C_P_PG

        ! Modern Compressible Flow by John Anderson Eq. (9.36)
        C_P_PG = C_P_inc / sqrt(1. - M_corr*M_corr)
        
    end function flow_correct_C_P_PG


    function flow_correct_C_P_KT(this, C_P_inc, M_corr) result(C_P_KT)
        ! Corrects the given incompressible pressure coefficient for compressibility using the Karman-Tsien correction

        implicit none
        
        class(flow),intent(in) :: this
        real(kind=sp),intent(in) :: C_P_inc, M_corr

        real(kind=sp) :: C_P_KT

        real(kind=sp) :: x, M2, sM2
            
        ! Modern Compressible Flow by John Anderson Eq. (9.40)
        M2 = M_corr*M_corr
        sM2 = sqrt(1. - M2)
        x = M2 / (1. + sM2)
        C_P_KT = C_P_inc / (sM2 + 0.5*x*C_P_inc)

    end function flow_correct_C_P_KT


    function flow_correct_C_P_L(this, C_P_inc, M_corr) result(C_P_L)
        ! Corrects the given incompressible pressure coefficient for compressibility using the Laitone correction

        implicit none
        
        class(flow),intent(in) :: this
        real(kind=sp),intent(in) :: C_P_inc, M_corr

        real(kind=sp) :: C_P_L

        real(kind=sp) :: x, M2, sM2
            
        ! Modern Compressible Flow by John Anderson Eq. (9.39)
        M2 = M_corr*M_corr
        sM2 = sqrt(1. - M2)
        x = M2 * (1. + (0.5 * (this.gamma - 1.) * M2)) / (2 * sM2)
        C_P_L = C_P_inc / (sM2 + (x * C_P_inc))

    end function flow_correct_C_P_L


    subroutine flow_restrict_pressure(this, C_P)
        ! Restricts the given pressure coefficient to the range [C_P_vac, C_P_stag]

        implicit none
        
        class(flow), intent(in) :: this
        real(kind=sp), intent(inout) :: C_P
        
        if (C_P > this.C_P_stag) then
            C_P = this.C_P_stag
        else if (C_P < this.C_P_vac) then
            C_P = this.C_P_vac
        end if
        
    end subroutine flow_restrict_pressure


    function flow_get_C_P(this, v, rule, M_corr) result(C_P)
        ! Calculates the pressure coefficient for the given velocity using the given rule
        ! Rule defaults to the incompressible or isentropic rule, based on the freestream Mach number

        implicit none
        
        class(flow),intent(in) :: this
        real(kind=sp),dimension(3),intent(in) :: v
        character(len=*),intent(in),optional :: rule
        real(kind=sp),intent(in),optional :: M_corr

        real(kind=sp) :: C_P
        character(len=:),allocatable :: pressure_rule

        ! Get pressure rule
        if (present(rule)) then
            pressure_rule = rule
        else
            if (this.M_inf == 0.) then
                pressure_rule = "incompressible"
            else
                pressure_rule = "isentropic"
            end if
        end if

        ! Calculate
        select case (pressure_rule)

        case ("incompressible")
            C_P = this.get_C_P_inc(v)

        case ("isentropic")
            C_P = this.get_C_P_ise(v)

        case ("second-order")
            C_P = this.get_C_P_2nd(v)

        case ("slender-body")
            C_P = this.get_C_P_sln(v)

        case ("linear")
            C_P = this.get_C_P_lin(v)

        case ("prandtl-glauert")
            C_P = this.get_C_P_inc(v)
            C_P = this.correct_C_P_PG(C_P, M_corr)

        case ("karman-tsien")
            C_P = this.get_C_P_inc(v)
            C_P = this.correct_C_P_KT(C_P, M_corr)

        case ("laitone")
            C_P = this.get_C_P_inc(v)
            C_P = this.correct_C_P_L(C_P, M_corr)

        end select
        
    end function flow_get_C_P


end module flow_mod
