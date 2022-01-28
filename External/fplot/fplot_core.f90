! fplot_core.f90

!> @brief \b fplot_core
!!
!! @par Purpose
!! FPLOT is a Fortran library providing a means of interacting with
!! [Gnuplot](http://www.gnuplot.info/) from a Fortran program.  The library is
!! designed in an object-oriented manner, and as such utilizes language features
!! that require a compiler that supports the 2003 and 2008 standards.  Additionally,
!! it is expected that Gnuplot is installed on the system path.  For full
!! functionallity, a minimum of Gnuplot v5.2 is expected.
!!
!! @image html example_surface_plot_lighting_2.png
module fplot_core
    use, intrinsic :: iso_fortran_env, only : real64, real32, int32
    use strings
    use collection_list
    use fplot_errors
    use ferror, only : errors
    implicit none
    private
    public :: GNUPLOT_TERMINAL_WIN32
    public :: GNUPLOT_TERMINAL_WXT
    public :: GNUPLOT_TERMINAL_QT
    public :: GNUPLOT_TERMINAL_PNG
    public :: GNUPLOT_TERMINAL_LATEX
    public :: CLR_BLACK
    public :: CLR_WHITE
    public :: CLR_RED
    public :: CLR_LIME
    public :: CLR_BLUE
    public :: CLR_YELLOW
    public :: CLR_CYAN
    public :: CLR_MAGENTA
    public :: CLR_SILVER
    public :: CLR_GRAY
    public :: CLR_MAROON
    public :: CLR_OLIVE
    public :: CLR_GREEN
    public :: CLR_PURPLE
    public :: CLR_TEAL
    public :: CLR_NAVY
    public :: CLR_ORANGE
    public :: MARKER_PLUS
    public :: MARKER_X
    public :: MARKER_ASTERISK
    public :: MARKER_EMPTY_SQUARE
    public :: MARKER_FILLED_SQUARE
    public :: MARKER_EMPTY_CIRCLE
    public :: MARKER_FILLED_CIRCLE
    public :: MARKER_EMPTY_TRIANGLE
    public :: MARKER_FILLED_TRIANGLE
    public :: MARKER_EMPTY_NABLA
    public :: MARKER_FILLED_NABLA
    public :: MARKER_EMPTY_RHOMBUS
    public :: MARKER_FILLED_RHOMBUS
    public :: LINE_SOLID
    public :: LINE_DASHED
    public :: LINE_DOTTED
    public :: LINE_DASH_DOTTED
    public :: LINE_DASH_DOT_DOT
    public :: LEGEND_CENTER
    public :: LEGEND_LEFT
    public :: LEGEND_RIGHT
    public :: LEGEND_TOP
    public :: LEGEND_BOTTOM
    public :: PLOTDATA_MAX_NAME_LENGTH
    public :: linspace
    public :: logspace
    public :: meshgrid
    public :: color
    public :: plot_data
    public :: plot_axis
    public :: terminal
    public :: windows_terminal
    public :: qt_terminal
    public :: wxt_terminal
    public :: png_terminal
    public :: latex_terminal
    public :: legend
    public :: plot
    public :: scatter_plot_data
    public :: plot_data_2d
    public :: plot_2d
    public :: plot_data_3d
    public :: plot_3d
    public :: surface_plot_data
    public :: surface_plot
    public :: colormap
    public :: rainbow_colormap
    public :: hot_colormap
    public :: cool_colormap
    public :: plot_label
    public :: multiplot
    public :: plot_data_error_bars
    public :: plot_data_colored

! ******************************************************************************
! GNUPLOT TERMINAL CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief Defines a Win32 terminal.
    integer(int32), parameter :: GNUPLOT_TERMINAL_WIN32 = 1
    !> @brief Defines a WXT terminal.
    integer(int32), parameter :: GNUPLOT_TERMINAL_WXT = 2
    !> @brief Defines a QT terminal.
    integer(int32), parameter :: GNUPLOT_TERMINAL_QT = 3
    !> @brief Defines a PNG terminal.
    integer(int32), parameter :: GNUPLOT_TERMINAL_PNG = 4
    !> @brief Defines a LATEX terminal.
    integer(int32), parameter :: GNUPLOT_TERMINAL_LATEX = 5

! ******************************************************************************
! MARKER CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief Defines a + data point marker.
    integer(int32), parameter :: MARKER_PLUS = 1
    !> @brief Defines an x data point marker.
    integer(int32), parameter :: MARKER_X = 2
    !> @brief Defines an * data point marker.
    integer(int32), parameter :: MARKER_ASTERISK = 3
    !> @brief Defines an empty square-shaped data point marker.
    integer(int32), parameter :: MARKER_EMPTY_SQUARE = 4
    !> @brief Defines an filled square-shaped data point marker.
    integer(int32), parameter :: MARKER_FILLED_SQUARE = 5
    !> @brief Defines an empty circle-shaped data point marker.
    integer(int32), parameter :: MARKER_EMPTY_CIRCLE = 6
    !> @brief Defines an filled circle-shaped data point marker.
    integer(int32), parameter :: MARKER_FILLED_CIRCLE = 7
    !> @brief Defines an empty triangle-shaped data point marker.
    integer(int32), parameter :: MARKER_EMPTY_TRIANGLE = 8
    !> @brief Defines an filled triangle-shaped data point marker.
    integer(int32), parameter :: MARKER_FILLED_TRIANGLE = 9
    !> @brief Defines an empty nabla-shaped data point marker.
    integer(int32), parameter :: MARKER_EMPTY_NABLA = 10
    !> @brief Defines an filled nabla-shaped data point marker.
    integer(int32), parameter :: MARKER_FILLED_NABLA = 11
    !> @brief Defines an empty rhombus-shaped data point marker.
    integer(int32), parameter :: MARKER_EMPTY_RHOMBUS = 12
    !> @brief Defines an filled rhombus-shaped data point marker.
    integer(int32), parameter :: MARKER_FILLED_RHOMBUS = 13

! ******************************************************************************
! LINE CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief Defines a solid line.
    integer(int32), parameter :: LINE_SOLID = 1
    !> @brief Defines a dashed line.
    integer(int32), parameter :: LINE_DASHED = 2
    !> @brief Defines a dotted line.
    integer(int32), parameter :: LINE_DOTTED = 3
    !> @brief Defines a dash-dotted line.
    integer(int32), parameter :: LINE_DASH_DOTTED = 4
    !> @brief Defines a dash-dot-dotted line.
    integer(int32), parameter :: LINE_DASH_DOT_DOT = 5

! ******************************************************************************
! LEGEND CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief Defines the legend should be placed at the top of the plot.
    character(len = *), parameter :: LEGEND_TOP = "top"
    !> @brief Defines the legend should be centered on the plot.
    character(len = *), parameter :: LEGEND_CENTER = "center"
    !> @brief Defines the legend should be placed at the left of the plot.
    character(len = *), parameter :: LEGEND_LEFT = "left"
    !> @brief Defines the legend should be placed at the right of the plot.
    character(len = *), parameter :: LEGEND_RIGHT = "right"
    !> @brief Defines the legend should be placed at the bottom of the plot.
    character(len = *), parameter :: LEGEND_BOTTOM = "bottom"

! ******************************************************************************
! PLOT DATA CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief Defines the maximum number of characters allowed in a graph label.
    integer(int32), parameter :: PLOTDATA_MAX_NAME_LENGTH = 128

! ******************************************************************************
! PRIVATE/DEFAULT CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief The default GNUPLOT window width, in pixels.
    integer(int32), parameter :: GNUPLOT_DEFAULT_WINDOW_WIDTH = 640
    !> @brief The default GNUPLOT window height, in pixels.
    integer(int32), parameter :: GNUPLOT_DEFAULT_WINDOW_HEIGHT = 420
    !> @brief Defines the maximum number of characters allowed in a graph label.
    integer(int32), parameter :: GNUPLOT_MAX_LABEL_LENGTH = 128
    !> @brief Defines the default font used by text on the graph.
    character(len = *), parameter :: GNUPLOT_DEFAULT_FONTNAME = "Calibri"
    !> @brief Defines the default font size used by text on the graph.
    integer(int32), parameter :: GNUPLOT_DEFAULT_FONT_SIZE = 10
    !> @brief Defines the maximum number of characters allowed in a file path.
    integer(int32), parameter :: GNUPLOT_MAX_PATH_LENGTH = 256

! ******************************************************************************
! BASE TYPES
! ------------------------------------------------------------------------------
    !> @brief The base type for a GNUPLOT object.
    type, abstract :: plot_object
    contains
        !> @brief Returns the appropriate GNUPLOT command string to define the
        !! plot object properties.
        procedure(get_string_result), deferred, public :: get_command_string
    end type

! ******************************************************************************
! FPLOT_CORE_ROUTINES.F90
! ------------------------------------------------------------------------------
    interface
        !> @brief Constructs a linearly spaced array.
        !!
        !! @param[in] start The first value in the array.
        !! @param[in] finish The last value in the array.
        !! @param[in] npts The number of values in the array.
        !!
        !! @return The resulting array.
        pure module function linspace(start, finish, npts) result(x)
            real(real64), intent(in) :: start, finish
            integer(int32), intent(in) :: npts
            real(real64), dimension(npts) :: x
        end function

        !> @brief Construcst a logarithmically spaced array.
        !!
        !! @param[in] start The exponent of the first value in the array.
        !! @param[in] finish The exponent of the final value in the array.
        !! @param[in] npts The number of values in the array.
        !!
        !! @return The resulting array.
        pure module function logspace(start, finish, npts) result(x)
            real(real64), intent(in) :: start, finish
            integer(int32), intent(in) :: npts
            real(real64), dimension(npts) :: x
        end function

        !> @brief Constructs two matrices (X and Y) from x and y data arrays.
        !!
        !! @param[in] x An M-element array of x data points.
        !! @param[in] y An N-element array of y data points.
        !! @return An N-by-M-by-2 array containing the x data matrix on the
        !!  first page of the array, and the y data matrix on the second page.
        pure module function meshgrid(x, y) result(xy)
            real(real64), intent(in), dimension(:) :: x, y
            real(real64), dimension(size(y), size(x), 2) :: xy
        end function
    end interface

! ******************************************************************************
! FPLOT_COLORS.F90
! ------------------------------------------------------------------------------
    !> @brief Describes an RGB color.
    type color
        !> @brief The red component of the color (must be between 0 and 255).
        integer(int32), public :: red = 0
        !> @brief The green component of the color (must be between 0 and 255).
        integer(int32), public :: green = 0
        !> @brief The blue component of the color (must be between 0 and 255).
        integer(int32), public :: blue = 255
    contains
        !> @brief Returns the color in hexadecimal format.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure character(6) function clr_to_hex_string(class(color) this)
        !! @endcode
        !!
        !! @param[in] this The color object.
        !! @return A string containing the hexadecimal equivalent.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(color) :: clr
        !!     character(6) :: hex_str
        !!
        !!     ! Return the hexadecimal form of the color
        !!     hex_str = clr%to_hex_string()
        !! end program
        !! @endcode
        procedure, public, pass :: to_hex_string => clr_to_hex_string
        !> @brief Copies another color to this color.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine copy_from(class(color) this, class(color) clr)
        !! @endcode
        !!
        !! @param[in,out] this The color object.
        !! @param[in] clr The color to copy.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(color) :: clr1, clr2
        !!
        !!     ! Copy clr1 to clr2
        !!     call clr2%copy_from(clr1)
        !! end program
        !! @endcode
        procedure, public, pass :: copy_from => clr_copy_from
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function clr_to_hex_string(this) result(txt)
            class(color), intent(in) :: this
            character(6) :: txt
        end function

        module subroutine clr_copy_from(this, clr)
            class(color), intent(inout) :: this
            class(color), intent(in) :: clr
        end subroutine
    end interface

! ------------------------------------------------------------------------------
    !> @brief Defines a black color.
    type(color), parameter :: CLR_BLACK = color(0, 0, 0)
    !> @brief Defines a white color.
    type(color), parameter :: CLR_WHITE = color(255, 255, 255)
    !> @brief Defines a red color.
    type(color), parameter :: CLR_RED = color(255, 0, 0)
    !> @brief Defines a lime color.
    type(color), parameter :: CLR_LIME = color(0, 255, 0)
    !> @brief Defines a blue color.
    type(color), parameter :: CLR_BLUE = color(0, 0, 255)
    !> @brief Defines a yellow color.
    type(color), parameter :: CLR_YELLOW = color(255, 255, 0)
    !> @brief Defines a cyan color.
    type(color), parameter :: CLR_CYAN = color(0, 255, 255)
    !> @brief Defines a magenta color.
    type(color), parameter :: CLR_MAGENTA = color(255, 0, 255)
    !> @brief Defines a silver color.
    type(color), parameter :: CLR_SILVER = color(192, 192, 192)
    !> @brief Defines a gray color.
    type(color), parameter :: CLR_GRAY = color(128, 128, 128)
    !> @brief Defines a maroon color.
    type(color), parameter :: CLR_MAROON = color(128, 0, 0)
    !> @brief Defines a olive color.
    type(color), parameter :: CLR_OLIVE = color(128, 128, 0)
    !> @brief Defines a green color.
    type(color), parameter :: CLR_GREEN = color(0, 128, 0)
    !> @brief Defines a purple color.
    type(color), parameter :: CLR_PURPLE = color(128, 0, 128)
    !> @brief Defines a teal color.
    type(color), parameter :: CLR_TEAL = color(0, 128, 128)
    !> @brief Defines a navy color.
    type(color), parameter :: CLR_NAVY = color(0, 0, 128)
    !> @brief Defines an orange color.
    type(color), parameter :: CLR_ORANGE = color(255, 165, 0)

    ! A list of colors that can be cycled through by plotting code
    type(color), parameter, dimension(8) :: color_list = [ &
        CLR_BLUE, CLR_GREEN, CLR_RED, CLR_CYAN, CLR_LIME, CLR_PURPLE, &
        CLR_ORANGE, CLR_BLACK]

! ******************************************************************************
! FPLOT_LABEL.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a label object for a plot.
    !!
    !! @par Example
    !! The following example illustrates how to add a simple label to a plot.
    !! @code{.f90}
    !! program example
    !!     use fplot_core
    !!     use iso_fortran_env
    !!     implicit none
    !!
    !!     ! Local Variables
    !!     integer(int32), parameter :: npts = 1000
    !!     real(real64), dimension(npts) :: x, y
    !!     type(plot_2d) :: plt
    !!     type(plot_data_2d) :: dataset
    !!     class(plot_axis), pointer :: xAxis, yAxis
    !!     type(legend), pointer :: leg
    !!     type(plot_label) :: lbl
    !!
    !!     ! Build a data set
    !!     x = linspace(0.0d0, 10.0d0, npts)
    !!     y = sin(10.0d0 * x) * sin(0.5d0 * x)
    !!
    !!     call dataset%define_data(y)
    !!
    !!     ! Define the label
    !!     call lbl%set_text("Test Label 1")
    !!     call lbl%set_position([600.0, 0.6, 0.0])
    !!
    !!     ! Set up the plot
    !!     call plt%initialize()
    !!     call plt%set_title("Example Plot")
    !!     call plt%set_font_size(14)
    !!     call plt%set_show_gridlines(.false.)
    !!
    !!     ! Add the label to the plot
    !!     call plt%push_label(lbl)
    !!
    !!     xAxis => plt%get_x_axis()
    !!     call xAxis%set_title("X Axis")
    !!
    !!     yAxis => plt%get_y_axis()
    !!     call yAxis%set_title("Y Axis")
    !!
    !!     ! Hide the legend
    !!     leg => plt%get_legend()
    !!     call leg%set_is_visible(.false.)
    !!
    !!     ! Add the data to the plot
    !!     call plt%push(dataset)
    !!
    !!     ! Draw
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! @image html example_plot_with_label_1.png
    type, extends(plot_object) :: plot_label
    private
        !> Determines if the label is visible
        logical :: m_visible = .true.
        !> The x, y, and z coordinates of the label
        real(real32), dimension(3) :: m_position
        !> The rotation angle of the label
        real(real32) :: m_angle = 0.0
        !> The label text
        character(len = PLOTDATA_MAX_NAME_LENGTH) :: m_text
    contains
        !> @brief Gets the GNUPLOT command string for the label.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(:) function allocatable get_command_string(class(plot_label) this)
        !! @endcode
        !!
        !! @param[in] this The plot_label object.
        !! @return The command string.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!  
        !!     type(plot_label) :: lbl
        !!     character(len = :), allocatable :: txt
        !!
        !!     txt = lbl%get_command_string()
        !! end program
        !! @endcode
        procedure, public :: get_command_string => lbl_get_cmd
        !> @brief Gets a value determining if the label is to be drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_is_visible(class(plot_label) this)
        !! @endcode
        !!
        !! @param[in] this The plot_label object.
        !! @return Returns true if the label is to be drawn; else, false.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!  
        !!     type(plot_label) :: lbl
        !!     logical :: x
        !!
        !!     x = lbl%get_is_visible()
        !! end program
        !! @endcode
        procedure, public :: get_is_visible => lbl_get_is_visible
        !> @brief Sets a value determining if the label is to be drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_is_visible(class(plot_label) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_label object.
        !! @param[in] x Set to true to draw the label; else, false.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!  
        !!     type(plot_label) :: lbl
        !!
        !!     ! Show the label
        !!     call lbl%set_is_visible(.true.)
        !! end program
        !! @endcode
        procedure, public :: set_is_visible => lbl_set_is_visible
        !> @brief Gets the position of the label in terms of plot coordinates.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) dimension(3) function get_position(class(plot_label) this)
        !! @endcode
        !!
        !! @param[in] this The plot_label object.
        !! @return A 3-element array containing the X, Y, and Z position of the label.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_label) :: lbl
        !!     real(real32) :: pos(3)
        !!
        !!     pos = lbl%get_position()
        !! end program
        !! @endcode
        procedure, public :: get_position => lbl_get_position
        !> @brief Sets the position of the label in terms of plot coordinates.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_position(class(plot_label) this, real(real32) x(3))
        !! @endcode
        !!
        !! @param[in,out] this The plot_label object.
        !! @param[in] x A 3-element array containing the X, Y, and Z position of the
        !!  label.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_label) :: lbl
        !!     real(real32) :: pos(3)
        !!
        !!     call lbl%set_position(pos)
        !! end program
        !! @endcode
        procedure, public :: set_position => lbl_set_position
        !> @brief Gets the angle of the label text, in degrees.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_angle(class(plot_label) this)
        !! @endcode
        !!
        !! @param[in] this The plot_label object.
        !! @return The angle, in degrees.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_label) :: lbl
        !!     real(real32) :: angle
        !!
        !!     angle = lbl%get_angle()
        !! end program
        !! @endcode
        procedure, public :: get_angle => lbl_get_angle
        !> @brief Sets the angle of the label text, in degrees.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_angle(class(plot_label) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_label object.
        !! @param[in] x The angle, in degrees.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_label) :: lbl
        !!
        !!     call lbl%set_angle(45.0)
        !! end program
        !! @endcode
        procedure, public :: set_angle => lbl_set_angle
        !> @brief Gets the text displayed by the label.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function allocatable get_text(class(plot_label) this)
        !! @endcode
        !!
        !! @param[in] this The plot_label object.
        !! @return The string of text to display.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_label) :: lbl
        !!     character(len = :), allocatable :: x
        !!
        !!     x = lbl%get_text()
        !! end program
        !! @endcode
        procedure, public :: get_text => lbl_get_txt
        !> @brief Sets the text displayed by the label.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_text(class(plot_label) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_label object.
        !! @param[in] x The text string to display.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_label) :: lbl
        !!     character(len = :), allocatable :: x
        !!
        !!     call lbl%set_text("Example Label Text")
        !! end program
        !! @endcode
        procedure, public :: set_text => lbl_set_txt
    end type

! ------------------------------------------------------------------------------
    interface
        module function lbl_get_cmd(this) result(x)
            class(plot_label), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        pure module function lbl_get_is_visible(this) result(x)
            class(plot_label), intent(in) :: this
            logical :: x
        end function

        module subroutine lbl_set_is_visible(this, x)
            class(plot_label), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function lbl_get_position(this) result(x)
            class(plot_label), intent(in) :: this
            real(real32), dimension(3) :: x
        end function

        module subroutine lbl_set_position(this, x)
            class(plot_label), intent(inout) :: this
            real(real32), intent(in), dimension(3) :: x
        end subroutine

        pure module function lbl_get_angle(this) result(x)
            class(plot_label), intent(in) :: this
            real(real32) :: x
        end function

        module subroutine lbl_set_angle(this, x)
            class(plot_label), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        module function lbl_get_txt(this) result(x)
            class(plot_label), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine lbl_set_txt(this, x)
            class(plot_label), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_TERMINAL.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a GNUPLOT terminal object.
    type, abstract, extends(plot_object) :: terminal
    private
        !> The window height, in pixels
        integer(int32) :: m_windowHeight = GNUPLOT_DEFAULT_WINDOW_HEIGHT
        !> The window width, in pixels
        integer(int32) :: m_windowWidth = GNUPLOT_DEFAULT_WINDOW_WIDTH
        !> The plot window number.
        integer(int32) :: m_termID = 0
        !> The plot window title.
        character(len = GNUPLOT_MAX_LABEL_LENGTH) :: m_title = ""
        !> Determines if a plot title is defined
        logical :: m_hasTitle = .false.
        !> The font used by the graph.
        character(len = GNUPLOT_MAX_LABEL_LENGTH) :: m_fontName = &
            GNUPLOT_DEFAULT_FONTNAME
        !> The size of the font used by the graph.
        integer(int32) :: m_fontSize = GNUPLOT_DEFAULT_FONT_SIZE
    contains
        !> @brief Gets the width of the plot window.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_window_width(class(terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The width of the plot window.
        !!
        !! @par Example
        !! Notice, this example uses a wxt_terminal.  Any type that derives from
        !! the terminal type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(wxt_terminal) :: term
        !!     integer(int32) :: width
        !!
        !!     ! Get the width of the plot window
        !!     width = term%get_window_width()
        !! end program
        !! @endcode
        procedure, public :: get_window_width => term_get_window_width
        !> @brief Sets the width of the plot window.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_window_width(class(terminal) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The terminal object.
        !! @param[in] x The width of the plot window.  If a value of zero is
        !! provided, the window width is reset to its default value; or, if a
        !! negative value is provided, the absolute value of the supplied value
        !! is utilized.
        !!
        !! @par Example
        !! Notice, this example uses a wxt_terminal.  Any type that derives from
        !! the terminal type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(wxt_terminal) :: term
        !!
        !!     ! Set the width of the plot window to 400 pixels.
        !!     call term%set_window_width(400)
        !! end program
        !! @endcode
        procedure, public :: set_window_width => term_set_window_width
        !> @brief Gets the height of the plot window.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_window_height(class(terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The height of the plot window.
        !!
        !! @par Example
        !! Notice, this example uses a wxt_terminal.  Any type that derives from
        !! the terminal type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(wxt_terminal) :: term
        !!     integer(int32) :: height
        !!
        !!     ! Get the height of the plot window
        !!     height = term%get_window_height()
        !! end program
        !! @endcode
        procedure, public :: get_window_height => term_get_window_height
        !> @brief Sets the height of the plot window.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_window_height(class(terminal) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The terminal object.
        !! @param[in] x The height of the plot window.  If a value of zero is
        !! provided, the window height is reset to its default value; or, if a
        !! negative value is provided, the absolute value of the supplied value is
        !! utilized.
        !!
        !! @par Example
        !! Notice, this example uses a wxt_terminal.  Any type that derives from
        !! the terminal type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(wxt_terminal) :: term
        !!
        !!     ! Set the height of the plot window to 400 pixels.
        !!     call term%set_window_height(400)
        !! end program
        !! @endcode
        procedure, public :: set_window_height => term_set_window_height
        !> @brief Returns the appropriate GNUPLOT command string to establish
        !! appropriate parameters.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The GNUPLOT command string.
        procedure, public :: get_command_string => term_get_command_string
        !> @brief Gets the targeted plot window number.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_plot_window_number(class(terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The plot window number.
        procedure, public :: get_plot_window_number => &
            term_get_plot_window_number
        !> @brief Sets the targeted plot window number.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_plot_window_number(class(terminal) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The terminal object.
        !! @param[in] x The plot window number.
        procedure, public :: set_plot_window_number => &
            term_set_plot_window_number
        !> @brief Gets the plot window's title.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_title(class(terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The title.
        !!
        !! @par Example
        !! Notice, this example uses a wxt_terminal.  Any type that derives from
        !! the terminal type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(wxt_terminal) :: term
        !!     character(len = :), allocatable :: title
        !!
        !!     ! Get the plot window title.
        !!     title = term%get_title()
        !! end program
        !! @endcode
        procedure, public :: get_title => term_get_title
        !> @brief Sets the plot window's title.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_title(class(terminal) this, character(len = *) txt)
        !! @endcode
        !!
        !! @param[in,out] this The terminal object.
        !! @param[in] txt The title.
        !!
        !! @par Example
        !! Notice, this example uses a wxt_terminal.  Any type that derives from
        !! the terminal type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(wxt_terminal) :: term
        !!
        !!     ! Set the plot window title.
        !!     call term%set_title("New Window Title")
        !! end program
        !! @endcode
        procedure, public :: set_title => term_set_title
        !> @brief Gets the name of the font used for text displayed by the
        !! graph.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_font_name(class(terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The font name.
        !!
        !! @par Example
        !! Notice, this example uses a wxt_terminal.  Any type that derives from
        !! the terminal type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(wxt_terminal) :: term
        !!     character(len = :), allocatable :: font
        !!
        !!     ! Get the name of the font.
        !!     font = term%get_font_name()
        !! end program
        !! @endcode
        procedure, public :: get_font_name => term_get_font_name
        !> @brief Sets the name of the font used for text displayed by the
        !! graph.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_font_name(class(terminal) this, character(len = *) name)
        !! @endcode
        !!
        !! @param[in,out] this The terminal object.
        !! @param[in] name The name of the font.  If no name is supplied, the
        !!  name is reset back to its default setting.
        !!
        !! @par Example
        !! Notice, this example uses a wxt_terminal.  Any type that derives from
        !! the terminal type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(wxt_terminal) :: term
        !!
        !!     ! Get the name of the font.
        !!     call term%set_font_name("Arial")
        !! end program
        !! @endcode
        procedure, public :: set_font_name => term_set_font_name
        !> @brief Gets the size of the font used by the graph.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_font_size(class(terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The font size, in points.
        !!
        !! @par Example
        !! Notice, this example uses a wxt_terminal.  Any type that derives from
        !! the terminal type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(wxt_terminal) :: term
        !!     integer(int32) :: sz
        !!
        !!     ! Get the font size.
        !!     sz = term%get_font_size()
        !! end program
        !! @endcode
        procedure, public :: get_font_size => term_get_font_size
        !> @brief Sets the size of the font used by the graph.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_font_size(class(terminal) this, integer(int32) sz)
        !! @endcode
        !!
        !! @param[in,out] this The terminal object.
        !! @param[in] sz The font size, in points.  If a value of zero is provided,
        !! the font size is reset to its default value; or, if a negative value
        !! is provided, the absolute value of the supplied value is utilized.
        !!
        !! @par Example
        !! Notice, this example uses a wxt_terminal.  Any type that derives from
        !! the terminal type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(wxt_terminal) :: term
        !!
        !!     ! Set the size of the font.
        !!     call term%set_font_size(12)
        !! end program
        !! @endcode
        procedure, public :: set_font_size => term_set_font_size
        !> @brief Gets the GNUPLOT terminal identification string.
        procedure(term_get_string_result), deferred, public :: get_id_string
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function term_get_window_width(this) result(x)
            class(terminal), intent(in) :: this
            integer :: x
        end function

        module subroutine term_set_window_width(this, x)
            class(terminal), intent(inout) :: this
            integer, intent(in) :: x
        end subroutine

        pure module function term_get_window_height(this) result(x)
            class(terminal), intent(in) :: this
            integer :: x
        end function

        module subroutine term_set_window_height(this, x)
            class(terminal), intent(inout) :: this
            integer, intent(in) :: x
        end subroutine

        pure module function term_get_plot_window_number(this) result(x)
            class(terminal), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine term_set_plot_window_number(this, x)
            class(terminal), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        module function term_get_title(this) result(str)
            class(terminal), intent(in) :: this
            character(len = :), allocatable :: str
        end function

        module subroutine term_set_title(this, txt)
            class(terminal), intent(inout) :: this
            character(len = *), intent(in) :: txt
        end subroutine

        module function term_get_font_name(this) result(name)
            class(terminal), intent(in) :: this
            character(len = :), allocatable :: name
        end function

        module subroutine term_set_font_name(this, name)
            class(terminal), intent(inout) :: this
            character(len = *), intent(in) :: name
        end subroutine

        pure module function term_get_font_size(this) result(sz)
            class(terminal), intent(in) :: this
            integer(int32) :: sz
        end function

        module subroutine term_set_font_size(this, sz)
            class(terminal), intent(inout) :: this
            integer(int32), intent(in) :: sz
        end subroutine

        module function term_get_command_string(this) result(x)
            class(terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_WINDOWS_TERMINAL.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a GNUPLOT Win32 terminal object.
    type, extends(terminal) :: windows_terminal
    private
        !> The terminal ID string
        character(len = 3) :: m_id = "win"
    contains
        !> @brief Retrieves a GNUPLOT terminal identifier string.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_id_string(class(windows_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The windows_terminal object.
        !! @return The string.
        procedure, public :: get_id_string => wt_get_term_string
    end type

! ------------------------------------------------------------------------------
    interface
        module function wt_get_term_string(this) result(x)
            class(windows_terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_QT_TERMINAL.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a GNUPLOT QT terminal object.
    type, extends(terminal) :: qt_terminal
    private
        !> The terminal ID string
        character(len = 2) :: m_id = "qt"
    contains
        !> @brief Retrieves a GNUPLOT terminal identifier string.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_id_string(class(qt_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The qt_terminal object.
        !! @return The string.
        procedure, public :: get_id_string => qt_get_term_string
    end type

! ------------------------------------------------------------------------------
    interface
        module function qt_get_term_string(this) result(x)
            class(qt_terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_WXT_TERMINAL.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a GNUPLOT WXT terminal object.
    type, extends(terminal) :: wxt_terminal
    private
        !> The terminal ID string
        character(len = 3) :: m_id = "wxt"
    contains
        !> @brief Retrieves a GNUPLOT terminal identifier string.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_id_string(class(wxt_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The wxt_terminal object.
        !! @return The string.
        procedure, public :: get_id_string => wxt_get_term_string
    end type

! ------------------------------------------------------------------------------
    interface
        module function wxt_get_term_string(this) result(x)
            class(wxt_terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_PNG_TERMINAL.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a GNUPLOT PNG terminal object.
    !!
    !! @par Example
    !! The following example draws a simple plot, and illustrates the use of a
    !! png_terminal to draw directly to a PNG file.
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use fplot_core
    !!     implicit none
    !!
    !!     ! Local Variables & Parameters
    !!     integer(int32), parameter :: npts = 1000
    !!     real(real64), dimension(npts) :: x, y1, y2
    !!     type(plot_2d) :: plt
    !!     type(plot_data_2d) :: d1, d2
    !!     class(plot_axis), pointer :: xAxis, yAxis
    !!     type(legend), pointer :: leg
    !!
    !!     ! Build a data set to plot
    !!     x = linspace(0.0d0, 10.0d0, npts)
    !!     y1 = sin(x) * cos(x)
    !!     y2 = sqrt(x) * sin(x)
    !!
    !!     call d1%define_data(x, y1)
    !!     call d2%define_data(x, y2)
    !!
    !!     ! Set up the plot
    !!     call plt%initialize(GNUPLOT_TERMINAL_PNG, "example_plot.png") ! Save to file directly
    !!     call plt%set_title("Example Plot")
    !!
    !!     xAxis => plt%get_x_axis()
    !!     call xAxis%set_title("X Axis")
    !!
    !!     yAxis => plt%get_y_axis()
    !!     call yAxis%set_title("Y Axis")
    !!
    !!     ! Put the legend in the upper left corner of the plot
    !!     leg => plt%get_legend()
    !!     call leg%set_horizontal_position(LEGEND_LEFT)
    !!     call leg%set_vertical_position(LEGEND_TOP)
    !!
    !!     ! Set up line color and style properties to better distinguish each data set
    !!     call d1%set_name("Data Set 1")
    !!     call d1%set_line_color(CLR_BLUE)
    !!
    !!     call d2%set_name("Data Set 2")
    !!     call d2%set_line_color(CLR_GREEN)
    !!
    !!     ! Add the data to the plot
    !!     call plt%push(d1)
    !!     call plt%push(d2)
    !!
    !!     ! Draw the plot
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! @image html example_plot.png
    type, extends(terminal) :: png_terminal
    private
        !> The terminal ID string
        character(len = 3) :: m_id = "png"
        !> The filename of the PNG file to write.
        character(len = GNUPLOT_MAX_PATH_LENGTH) :: m_fname = "default.png"
    contains
        !> @brief Gets the filename for the output PNG file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_filename(class(png_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The png_terminal object.
        !! @return The filename, including the file extension (.png).
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(png_terminal) :: term
        !!     character(len = :), allocatable :: fname
        !!
        !!     ! Get the filename
        !!     fname = term%get_filename()
        !! end program
        procedure, public :: get_filename => png_get_filename
        !> @brief Sets the filename for the output PNG file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_filename(class(png_terminal) this, character(len = *) txt)
        !! @endcode
        !!
        !! @param[in,out] this The png_terminal object.
        !! @param[in] txt The filename, including the file extension (.png).
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(png_terminal) :: term
        !!
        !!     ! Set the filename
        !!     call term%set_filename("Example PNG File.png")
        !! end program
        procedure, public :: set_filename => png_set_filename
        !> @brief Retrieves a GNUPLOT terminal identifier string.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_id_string(class(png_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The png_terminal object.
        !! @return The string.
        procedure, public :: get_id_string => png_get_term_string
        !> @brief Returns the appropriate GNUPLOT command string to establish
        !! appropriate parameters.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(png_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The GNUPLOT command string.
        procedure, public :: get_command_string => png_get_command_string
    end type

! ------------------------------------------------------------------------------
    interface
        module function png_get_term_string(this) result(x)
            class(png_terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function png_get_filename(this) result(txt)
            class(png_terminal), intent(in) :: this
            character(len = :), allocatable :: txt
        end function

        module subroutine png_set_filename(this, txt)
            class(png_terminal), intent(inout) :: this
            character(len = *), intent(in) :: txt
        end subroutine

        module function png_get_command_string(this) result(x)
            class(png_terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_LATEX.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a GNUPLOT LATEX terminal object.
    type, extends(terminal) :: latex_terminal
    private
        !> The terminal ID string
        character(len = 14) :: m_id = "epslatex color"
        !> The filename of the PNG file to write.
        character(len = GNUPLOT_MAX_PATH_LENGTH) :: m_fname = "default.tex"
    contains
        !> @brief Gets the filename for the output LATEX file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_filename(class(latex_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The latex_terminal object.
        !! @return The filename, including the file extension (.tex).
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(latex_terminal) :: term
        !!     character(len = :), allocatable :: fname
        !!
        !!     ! Get the filename
        !!     fname = term%get_filename()
        !! end program
        procedure, public :: get_filename => tex_get_filename
        !> @brief Sets the filename for the output LATEX file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_filename(class(latex_terminal) this, character(len = *) txt)
        !! @endcode
        !!
        !! @param[in,out] this The latex_terminal object.
        !! @param[in] txt The filename, including the file extension (.tex).
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(latex_terminal) :: term
        !!
        !!     ! Set the filename
        !!     call term%set_filename("Example LATEX File.tex")
        !! end program
        procedure, public :: set_filename => tex_set_filename
        !> @brief Retrieves a GNUPLOT terminal identifier string.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_id_string(class(latex_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The latex_terminal object.
        !! @return The string.
        procedure, public :: get_id_string => tex_get_term_string
        !> @brief Returns the appropriate GNUPLOT command string to establish
        !! appropriate parameters.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(latex_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The GNUPLOT command string.
        procedure, public :: get_command_string => tex_get_command_string
    end type

! ------------------------------------------------------------------------------
    interface
        module function tex_get_term_string(this) result(x)
            class(latex_terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function tex_get_filename(this) result(txt)
            class(latex_terminal), intent(in) :: this
            character(len = :), allocatable :: txt
        end function

        module subroutine tex_set_filename(this, txt)
            class(latex_terminal), intent(inout) :: this
            character(len = *), intent(in) :: txt
        end subroutine

        module function tex_get_command_string(this) result(x)
            class(latex_terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_PLOT_DATA.F90
! ------------------------------------------------------------------------------
    !> @brief Provides a container for plot data.
    type, abstract, extends(plot_object) :: plot_data
    private
        !> The name of the data set.
        character(len = PLOTDATA_MAX_NAME_LENGTH) :: m_name = ""
    contains
        !> @brief Gets the name to associate with this data set.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_name(class(plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data object.
        !! @return The name.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_data) :: pd
        !!     character(len = :), allocatable :: name
        !!
        !!     ! Get the name
        !!     name = pd%get_name()
        !! end program
        procedure, public :: get_name => pd_get_name
        !> @brief Sets the name to associate with this data set.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_name(class(plot_data) this, character(len = *) txt)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data object.
        !! @param[in] txt The name.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_data) :: pd
        !!
        !!     ! Set the name
        !!     call pd%set_name("Example Data Set")
        !! end program
        procedure, public :: set_name => pd_set_name
        !> @brief Gets the GNUPLOT command string containing the actual data
        !! to plot.
        procedure(pd_get_string_result), deferred, public :: get_data_string
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function pd_get_name(this) result(txt)
            class(plot_data), intent(in) :: this
            character(len = :), allocatable :: txt
        end function

        module subroutine pd_set_name(this, txt)
            class(plot_data), intent(inout) :: this
            character(len = *), intent(in) :: txt
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_PLOT_DATA_COLORED.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a plot_data based object best represented by a color.
    type, abstract, extends(plot_data) :: plot_data_colored
    private
        !> The line color.
        type(color) :: m_color = CLR_BLUE
        !> Let the object choose colors automatically
        logical :: m_useAutoColor = .true.
        !> The color index to use, assuming we're using auto color
        integer(int32) :: m_colorIndex = 1
    contains
        !> @brief Gets the line color.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure type(color) function get_line_color(class(plot_data_colored) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_colored instance.
        !! @return The color.
        procedure, public :: get_line_color => pdc_get_line_color
        !> @brief Sets the line color.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_line_color(class(plot_data_colored) this, type(color) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_colored instance.
        !! @param[in] x The color.
        procedure, public :: set_line_color => pdc_set_line_color
        procedure, private :: get_color_index => pdc_get_color_index
        procedure, private :: set_color_index => pdc_set_color_index
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function pdc_get_line_color(this) result(x)
            class(plot_data_colored), intent(in) :: this
            type(color) :: x
        end function

        module subroutine pdc_set_line_color(this, x)
            class(plot_data_colored), intent(inout) :: this
            type(color), intent(in) :: x
        end subroutine

        pure module function pdc_get_color_index(this) result(x)
            class(plot_data_colored), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine pdc_set_color_index(this, x)
            class(plot_data_colored), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_PLOT_AXIS.F90
! ------------------------------------------------------------------------------
    !> @brief Describes a single plot axis.
    type, abstract, extends(plot_object) :: plot_axis
    private
        !> @brief Has a title.
        logical :: m_hasTitle = .false.
        !> @brief The axis title.
        character(len = PLOTDATA_MAX_NAME_LENGTH) :: m_title = ""
        !> @brief Autoscale?
        logical :: m_autoscale = .true.
        !> @brief Display limits.
        real(real64), dimension(2) :: m_limits = [0.0d0, 1.0d0]
        !> @brief Log scaled?
        logical :: m_logScale = .false.
        !> @brief Zero axis?
        logical :: m_zeroAxis = .false.
        !> @brief The width, in pixels, of the zero axis line.
        real(real32) :: m_axisWidth = 1.0
    contains
        !> @brief Gets the axis' title.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_title(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in] this The plot_axis object.
        !! @return The title.
        !!
        !! @par Example
        !! Notice, this example uses an x_axis type.  Any type that derives from
        !! the plot_axis type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(x_axis) :: axis
        !!     character(len = :), allocatable :: txt
        !!
        !!     txt = axis%get_title()
        !! end program
        !! @endcode
        procedure, public :: get_title => pa_get_title
        !> @brief Sets the axis' title.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_title(class(plot_axis) this, character(len = *) txt)
        !! @endcode
        !!
        !! @param[in,out] this The plot_axis object.
        !! @param[in] txt The axis title.  The number of characters must be less
        !!  than or equal to PLOTDATA_MAX_NAME_LENGTH; else, the text string is
        !!  truncated.
        !!
        !! @par Example
        !! Notice, this example uses an x_axis type.  Any type that derives from
        !! the plot_axis type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(x_axis) :: axis
        !!
        !!     call axis%set_title("X Axis")
        !! end program
        !! @endcode
        procedure, public :: set_title => pa_set_title
        !> @brief Gets a value determining if a title has been defined for the
        !!  plot_axis object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function is_title_defined(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in] this The plot_axis object.
        !! @return Returns true if a title has been defined for this axis; else,
        !!  returns false.
        !!
        !! @par Example
        !! Notice, this example uses an x_axis type.  Any type that derives from
        !! the plot_axis type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(x_axis) :: axis
        !!     logical :: check
        !!
        !!     check = axis%is_title_defined()
        !! end program
        !! @endcode
        procedure, public :: is_title_defined => pa_has_title
        !> @brief Gets a logical value determining if the axis should be
        !! automatically scaled to fit the data.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_autoscale(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in] this The plot_axis object.
        !! @return Returns true if the axis should be automatically scaled; else,
        !! false.
        !!
        !! @par Example
        !! Notice, this example uses an x_axis type.  Any type that derives from
        !! the plot_axis type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(x_axis) :: axis
        !!     logical :: check
        !!
        !!     check = axis%get_autoscale()
        !! end program
        !! @endcode
        procedure, public :: get_autoscale => pa_get_autoscale
        !> @brief Sets a logical value determining if the axis should be
        !! automatically scaled to fit the data.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_autoscale(class(plot_axis) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_axis object.
        !! @param[in] x Set to true if the axis should be automatically scaled; else,
        !! false.
        !!
        !! @par Example
        !! Notice, this example uses an x_axis type.  Any type that derives from
        !! the plot_axis type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(x_axis) :: axis
        !!
        !!     call axis%set_autoscale(.true.)
        !! end program
        !! @endcode
        procedure, public :: set_autoscale => pa_set_autoscale
        !> @brief Gets the axis display limits, assuming autoscaling is not
        !! active for this axis.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function, dimension(2) get_limits(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in] this The plot_axis object.
        !! @return A two-element array containing the limits as follows:
        !!  [lower, upper].
        !!
        !! @par Example
        !! Notice, this example uses an x_axis type.  Any type that derives from
        !! the plot_axis type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(x_axis) :: axis
        !!     real(real64) :: lim(2)
        !!
        !!     lim = axis%get_limits()
        !! end program
        !! @endcode
        procedure, public :: get_limits => pa_get_axis_limits
        !> @brief Sets the axis display limits, assuming autoscaling is not
        !! active for this axis.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_limits(class(plot_axis) this, real(real64) lower, real(real64) upper)
        !! @endcode
        !!
        !! @param[in,out] this The plot_axis object.
        !! @param[in] lower The lower display limit.
        !! @param[in] upper The upper display limit.
        !!
        !! @par Example
        !! Notice, this example uses an x_axis type.  Any type that derives from
        !! the plot_axis type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(x_axis) :: axis
        !!
        !!     call axis%set_limits(0.0d0, 5.0d0)
        !! end program
        !! @endcode
        procedure, public :: set_limits => pa_set_axis_limits
        !> @brief Gets a logical value defining if the axis should be log
        !! scaled.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_is_log_scaled(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in,out] this The plot_axis object.
        !! @return Returns true if log scaling is applied to the axis; else, false.
        !!
        !! @par Example
        !! Notice, this example uses an x_axis type.  Any type that derives from
        !! the plot_axis type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(x_axis) :: axis
        !!     logical :: check
        !!
        !!     check = axis%get_is_log_scaled()
        !! end program
        !! @endcode
        procedure, public :: get_is_log_scaled => pa_get_log_scale
        !> @brief Sets a logical value defining if the axis should be log
        !! scaled.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_is_log_scaled(class(plot_axis) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_axis object.
        !! @param[in] x Set to true if log scaling is applied to the axis; else,
        !! false.
        !!
        !! @par Example
        !! Notice, this example uses an x_axis type.  Any type that derives from
        !! the plot_axis type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(x_axis) :: axis
        !!
        !!     call axis%set_is_log_scaled(.true.)
        !! end program
        !! @endcode
        procedure, public :: set_is_log_scaled => pa_set_log_scale
        !> @brief Returns the appropriate GNUPLOT command string to define the
        !! plot_axis properties.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in] this The plot_axis object.
        !! @return The GNUPLOT command string.
        procedure, public :: get_command_string => pa_get_cmd_string
        !> @brief Gets a value determining if the axis should be drawn through
        !! zero of opposing axes.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_zero_axis(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in] this The plot_axis object.
        !! @return Returns true to draw as a zero axis; else, set to false.
        !!
        !! @par Example
        !! Notice, this example uses an x_axis type.  Any type that derives from
        !! the plot_axis type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(x_axis) :: axis
        !!     logical :: check
        !!
        !!     check = axis%get_zero_axis()
        !! end program
        !! @endcode
        procedure, public :: get_zero_axis => pa_get_zero_axis
        !> @brief Sets a value determining if the axis should be drawn through
        !! zero of opposing axes.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_zero_axis(class(plot_axis) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_axis object.
        !! @param[in] x Set to true to draw as a zero axis; else, set to false.
        !!
        !! @par Example
        !! Notice, this example uses an x_axis type.  Any type that derives from
        !! the plot_axis type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(x_axis) :: axis
        !!
        !!     call axis%get_zero_axis(.true.)
        !! end program
        !! @endcode
        procedure, public :: set_zero_axis => pa_set_zero_axis
        !> @brief Gets the width of the line used to represent the zero axis
        !!  line, if active.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_zero_axis_line_width(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in] this The plot_axis object.
        !! @return The width of the line, in pixels.
        !!
        !! @par Example
        !! Notice, this example uses an x_axis type.  Any type that derives from
        !! the plot_axis type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(x_axis) :: axis
        !!     real(real32) :: width
        !!
        !!     width = axis%get_zero_axis_line_width()
        !! end program
        !! @endcode
        procedure, public :: get_zero_axis_line_width => pa_get_zero_axis_width
        !> @brief Sets the width of the line used to represent the zero axis
        !!  line, if active.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_zero_axis_line_width(class(plot_axis) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_axis object.
        !! @param[in] x The width of the line, in pixels.
        !!
        !! @par Example
        !! Notice, this example uses an x_axis type.  Any type that derives from
        !! the plot_axis type can be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(x_axis) :: axis
        !!
        !!     call axis%get_zero_axis_line_width(3.0)
        !! end program
        !! @endcode
        procedure, public :: set_zero_axis_line_width => pa_set_zero_axis_width
        !> @brief Gets a string identifying the axis as: x, y, z, y2, etc.
        procedure(pa_get_string_result), deferred, public :: get_id_string
    end type

! ------------------------------------------------------------------------------
    interface
        module function pa_get_title(this) result(txt)
            class(plot_axis), intent(in) :: this
            character(len = :), allocatable :: txt
        end function

        module subroutine pa_set_title(this, txt)
            class(plot_axis), intent(inout) :: this
            character(len = *), intent(in) :: txt
        end subroutine

        pure module function pa_has_title(this) result(x)
            class(plot_axis), intent(in) :: this
            logical :: x
        end function

        pure module function pa_get_autoscale(this) result(x)
            class(plot_axis), intent(in) :: this
            logical :: x
        end function

        module subroutine pa_set_autoscale(this, x)
            class(plot_axis), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function pa_get_axis_limits(this) result(x)
            class(plot_axis), intent(in) :: this
            real(real64), dimension(2) :: x
        end function

        module subroutine pa_set_axis_limits(this, lower, upper)
            class(plot_axis), intent(inout) :: this
            real(real64), intent(in) :: lower, upper
        end subroutine

        pure module function pa_get_log_scale(this) result(x)
            class(plot_axis), intent(in) :: this
            logical :: x
        end function

        module subroutine pa_set_log_scale(this, x)
            class(plot_axis), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module function pa_get_cmd_string(this) result(txt)
            class(plot_axis), intent(in) :: this
            character(len = :), allocatable :: txt
        end function

        pure module function pa_get_zero_axis(this) result(x)
            class(plot_axis), intent(in) :: this
            logical :: x
        end function

        module subroutine pa_set_zero_axis(this, x)
            class(plot_axis), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function pa_get_zero_axis_width(this) result(x)
            class(plot_axis), intent(in) :: this
            real(real32) :: x
        end function

        module subroutine pa_set_zero_axis_width(this, x)
            class(plot_axis), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_LEGEND.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a legend object.
    type, extends(plot_object) :: legend
    private
        !> Legend on inside or outside of axes
        logical :: m_inside = .true.
        !> Draw a box around the legend
        logical :: m_box = .true.
        !> Defines the horizontal position
        character(len = 20) :: m_horzPosition = LEGEND_RIGHT
        !> Defines the vertical position
        character(len = 20) :: m_vertPosition = LEGEND_TOP
        !> Determines if the legend is visible.
        logical :: m_show = .false.
    contains
        !> @brief Gets a value determining if the legend should be drawn inside
        !! the axes border (true), or outside the axes border (false).
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_draw_inside_axes(class(legend) this)
        !! @endcode
        !!
        !! @param[in] this The legend object.
        !! @return The logical value.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(legend) :: leg
        !!     logical :: check
        !!
        !!     check = leg%get_draw_inside_axes()
        !! end program
        !! @endcode
        procedure, public :: get_draw_inside_axes => leg_get_inside
        !> @brief Sets a value determining if the legend should be drawn inside
        !! the axes border (true), or outside the axes border (false).
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_draw_inside_axes(class(legend) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The legend object.
        !! @param[in] x The logical value.
        !!
        !! @par Example
        !! The following example draws a simple plot, adjusts the position
        !! of the legend to be located outside the plot axes, and removes the
        !! border around the legend.
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     ! Local Variables & Parameters
        !!     integer(int32), parameter :: npts = 1000
        !!     real(real64), dimension(npts) :: x, y1, y2
        !!     type(plot_2d) :: plt
        !!     class(terminal), pointer :: term
        !!     type(plot_data_2d) :: d1, d2
        !!     class(plot_axis), pointer :: xAxis, yAxis
        !!     type(legend), pointer :: leg
        !!
        !!     ! Build a data set to plot
        !!     x = linspace(0.0d0, 10.0d0, npts)
        !!     y1 = sin(x) * cos(x)
        !!     y2 = sqrt(x) * sin(x)
        !!
        !!     call d1%define_data(x, y1)
        !!     call d2%define_data(x, y2)
        !!
        !!     ! Set up the plot
        !!     call plt%initialize(GNUPLOT_TERMINAL_PNG) ! Save to file directly
        !!     call plt%set_title("Example Plot")
        !!
        !!     xAxis => plt%get_x_axis()
        !!     call xAxis%set_title("X Axis")
        !!
        !!     yAxis => plt%get_y_axis()
        !!     call yAxis%set_title("Y Axis")
        !!
        !!     ! Put the legend outside the axes, and remove it's border
        !!     leg => plt%get_legend()
        !!     call leg%set_draw_inside_axes(.false.)
        !!     call leg%set_draw_border(.false.)
        !!
        !!     ! Set up line color and style properties to better distinguish each data set
        !!     call d1%set_name("Data Set 1")
        !!     call d1%set_line_color(CLR_BLUE)
        !!
        !!     call d2%set_name("Data Set 2")
        !!     call d2%set_line_color(CLR_GREEN)
        !!
        !!     ! Add the data to the plot
        !!     call plt%push(d1)
        !!     call plt%push(d2)
        !!
        !!     ! Define the file to which the plot should be saved
        !!     term => plt%get_terminal()
        !!     select type (term)
        !!     class is (png_terminal)
        !!         call term%set_filename("example_plot.png")
        !!     end select
        !!
        !!     ! Draw the plot
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_plot_legend_out.png
        procedure, public :: set_draw_inside_axes => leg_set_inside
        !> @brief Gets a value determining if the legend should have a border.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_draw_border(class(legend) this)
        !! @endcode
        !!
        !! @param[in] this The legend object.
        !! @return The logical value.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(legend) :: leg
        !!     logical :: check
        !!
        !!     check = leg%get_draw_border()
        !! end program
        !! @endcode
        procedure, public :: get_draw_border => leg_get_box
        !> @brief Sets a value determining if the legend should have a border.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_draw_border(class(legend) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The legend object.
        !! @param[in] x The logical value.
        !!
        !! @par Example
        !! For an example, see @ref set_draw_inside_axes.
        procedure, public :: set_draw_border => leg_set_box
        !> @brief Gets the horizontal position of the legend.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_horizontal_position(class(legend) this)
        !! @endcode
        !!
        !! @param[in] this The legend object.
        !! @return The horizontal position of the legend (LEGEND_LEFT,
        !!  LEGEND_CENTER, or LEGEND_RIGHT).
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(legend) :: leg
        !!     character(len = :), allocatable :: pos
        !!
        !!     pos = leg%get_horizontal_position()
        !! end program
        !! @endcode
        procedure, public :: get_horizontal_position => leg_get_horz_pos
        !> @brief Sets the horizontal position of the legend.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_horizontal_position(class(legend) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The legend object.
        !! @param x The horizontal position of the legend.  The parameter must be
        !!  set to one of the following: LEGEND_LEFT, LEGEND_CENTER, or
        !!  LEGEND_RIGHT.  If not, the default LEGEND_RIGHT will be used.
        !!
        !! @par Example
        !! The following example draws a simple plot, and adjusts the position
        !! of the legend.
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     ! Local Variables & Parameters
        !!     integer(int32), parameter :: npts = 1000
        !!     real(real64), dimension(npts) :: x, y1, y2
        !!     type(plot_2d) :: plt
        !!     class(terminal), pointer :: term
        !!     type(plot_data_2d) :: d1, d2
        !!     class(plot_axis), pointer :: xAxis, yAxis
        !!     type(legend), pointer :: leg
        !!
        !!     ! Build a data set to plot
        !!     x = linspace(0.0d0, 10.0d0, npts)
        !!     y1 = sin(x) * cos(x)
        !!     y2 = sqrt(x) * sin(x)
        !!
        !!     call d1%define_data(x, y1)
        !!     call d2%define_data(x, y2)
        !!
        !!     ! Set up the plot
        !!     call plt%initialize(GNUPLOT_TERMINAL_PNG) ! Save to file directly
        !!     call plt%set_title("Example Plot")
        !!
        !!     xAxis => plt%get_x_axis()
        !!     call xAxis%set_title("X Axis")
        !!
        !!     yAxis => plt%get_y_axis()
        !!     call yAxis%set_title("Y Axis")
        !!
        !!     ! Put the legend in the upper left corner of the plot
        !!     leg => plt%get_legend()
        !!     call leg%set_horizontal_position(LEGEND_LEFT)
        !!     call leg%set_vertical_position(LEGEND_TOP)
        !!
        !!     ! Set up line color and style properties to better distinguish each data set
        !!     call d1%set_name("Data Set 1")
        !!     call d1%set_line_color(CLR_BLUE)
        !!
        !!     call d2%set_name("Data Set 2")
        !!     call d2%set_line_color(CLR_GREEN)
        !!
        !!     ! Add the data to the plot
        !!     call plt%push(d1)
        !!     call plt%push(d2)
        !!
        !!     ! Define the file to which the plot should be saved
        !!     term => plt%get_terminal()
        !!     select type (term)
        !!     class is (png_terminal)
        !!         call term%set_filename("example_plot.png")
        !!     end select
        !!
        !!     ! Draw the plot
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_plot.png
        procedure, public :: set_horizontal_position => leg_set_horz_pos
        !> @brief Gets the vertical position of the legend.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_vertical_position(class(legend) this)
        !! @endcode
        !!
        !! @param[in] this The legend object.
        !! @return The vertical position of the legend (LEGEND_TOP,
        !!  LEGEND_CENTER, or LEGEND_BOTTOM).
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(legend) :: leg
        !!     character(len = :), allocatable :: pos
        !!
        !!     pos = leg%get_vertical_position()
        !! end program
        !! @endcode
        procedure, public :: get_vertical_position => leg_get_vert_pos
        !> @brief Gets the vertical position of the legend.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_vertical_position(class(legend) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The legend object.
        !! @param x The vertical position of the legend.  The parameter must be
        !!  set to one of the following: LEGEND_TOP, LEGEND_CENTER, or
        !!  LEGEND_BOTTOM.  If not, the default LEGEND_TOP will be used.
        !!
        !! @par Example
        !! For an example, see @ref set_horizontal_position.
        procedure, public :: set_vertical_position => leg_set_vert_pos
        !> @brief Gets a value determining if the legend is visible.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_is_visible(class(legend) this)
        !! @endcode
        !!
        !! @param[in] this The legend object.
        !! @return The logical value.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(legend) :: leg
        !!     logical :: check
        !!
        !!     check = leg%get_is_visible()
        !! end program
        !! @endcode
        procedure, public :: get_is_visible => leg_get_visible
        !> @brief Sets a value determining if the legend is visible.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_is_visible(class(legend) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The legend object.
        !! @param[in] x The logical value.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(legend) :: leg
        !!
        !!     call leg%set_is_visible(.true.)
        !! end program
        !! @endcode
        procedure, public :: set_is_visible => leg_set_visible
        !> @brief Gets the command string defining the legend properties.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(legend) this)
        !! @endcode
        !!
        !! @param[in] this The legend object.
        !! @return The GNUPLOT command string.
        procedure, public :: get_command_string => leg_get_command_txt
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function leg_get_inside(this) result(x)
            class(legend), intent(in) :: this
            logical :: x
        end function

        module subroutine leg_set_inside(this, x)
            class(legend), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function leg_get_box(this) result(x)
            class(legend), intent(in) :: this
            logical :: x
        end function

        module subroutine leg_set_box(this, x)
            class(legend), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module function leg_get_horz_pos(this) result(x)
            class(legend), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine leg_set_horz_pos(this, x)
            class(legend), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine

        module function leg_get_vert_pos(this) result(x)
            class(legend), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine leg_set_vert_pos(this, x)
            class(legend), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine

        pure module function leg_get_visible(this) result(x)
            class(legend), intent(in) :: this
            logical :: x
        end function

        module subroutine leg_set_visible(this, x)
            class(legend), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module function leg_get_command_txt(this) result(txt)
            class(legend), intent(in) :: this
            character(len = :), allocatable :: txt
        end function
    end interface

! ******************************************************************************
! FPLOT_PLOT.F90
! ------------------------------------------------------------------------------
    !> @brief Defines the basic GNUPLOT plot.
    type, abstract, extends(plot_object) :: plot
    private
        !> The plot title
        character(len = PLOTDATA_MAX_NAME_LENGTH) :: m_title = ""
        !> Has a title?
        logical :: m_hasTitle = .false.
        !> The GNUPLOT terminal object to target.
        class(terminal), pointer :: m_terminal => null()
        !> A collection of plot_data items to plot.
        type(list) :: m_data
        !> The legend.
        type(legend), pointer :: m_legend => null()
        !> Show grid lines?
        logical :: m_showGrid = .true.
        !> Point tic marks in?
        logical :: m_ticsIn = .true.
        !> Draw the border?
        logical :: m_drawBorder = .true.
        !> A collection of plot_label items to draw
        type(list) :: m_labels ! Added 6/22/2018, JAC
        !> The color index to use for automatic line coloring for scatter plots.
        integer(int32) :: m_colorIndex = 1
    contains
        !> @brief Cleans up resources held by the plot object.  Inheriting
        !! classes are expected to call this routine to free internally held
        !! resources.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! module free_resources(class(plot) this)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        procedure, public :: free_resources => plt_clean_up
        !> @brief Initializes the plot object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine initialize(class(plot) this, optional class(terminal) term, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] term An optional input that is used to define the terminal.
        !!  The default terminal is a WXT terminal.  The acceptable inputs are:
        !!  - GNUPLOT_TERMINAL_PNG
        !!  - GNUPLOT_TERMINAL_QT
        !!  - GNUPLOT_TERMINAL_WIN32
        !!  - GNUPLOT_TERMINAL_WXT
        !!  - GNUPLOT_TERMINAL_LATEX
        !! @param[in] fname A filename to pass to the terminal in the event the
        !!  terminal is a file type (e.g. GNUPLOT_TERMINAL_PNG).
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!
        !! @par Example
        !! See @p png_terminal for an example.
        procedure, public :: initialize => plt_init
        !> @brief Gets the plot's title.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_title(class(plot))
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return The plot's title.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     character(len = :), allocatable :: txt
        !!
        !!     txt = plt%get_title()
        !! end program
        !! @endcode
        procedure, public :: get_title => plt_get_title
        !> @brief Sets the plot's title.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_title(class(plot) this, character(len = *) txt)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] txt The plot's title.  The number of characters must be less
        !! than or equal to PLOTDATA_MAX_NAME_LENGTH; else, the text string is
        !! truncated.
        !!
        !! @par Example
        !! See @p png_terminal for an example.
        procedure, public :: set_title => plt_set_title
        !> @brief Gets a value determining if a title has been defined for the
        !!  plot object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function is_title_defined(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return Returns true if a title has been defined for this plot; else,
        !!  returns false.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     logical :: check
        !!
        !!     check = plt%is_title_defined()
        !! end program
        !! @endcode
        procedure, public :: is_title_defined => plt_has_title
        !> @brief Gets the plot's legend object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(legend) function, pointer get_legend(class(this) plot)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return A pointer to the legend object.
        !!
        !! @par Example
        !! See @p png_terminal for an example.
        !!
        !! @par Example
        !! See @p png_terminal for an example.
        procedure, public :: get_legend => plt_get_legend
        !> @brief Gets the number of stored plot_data objects.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_count(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return The number of plot_data objects.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     integer(int32) :: n
        !!
        !!     n = plt%get_count()
        !! end program
        !! @endcode
        procedure, public :: get_count => plt_get_count
        !> @brief Pushes a plot_data object onto the stack.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine push(class(plot) this, class(plot_data) x, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] x The plot_data object.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!
        !! @par Example
        !! See @p png_terminal for an example.
        procedure, public :: push => plt_push_data
        !> @brief Pops the last plot_data object from the stack.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine pop(class(plot) this)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!
        !!     call plt%pop()
        !! end program
        !! @endcode
        procedure, public :: pop => plt_pop_data
        !> @brief Removes all plot_data objects from the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine clear(class(plot) this)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!
        !!     call plt%clear_all()
        !! end program
        !! @endcode
        procedure, public :: clear_all => plt_clear_all
        !> @brief Gets a pointer to the requested plot_data object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_data) function, pointer get(class(plot), integer(int32) i)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @param[in] i The index of the plot_data object.
        !! @return A pointer to the requested plot_data object.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     class(plot_data), pointer :: ptr
        !!
        !!     ! Add some data ... (not shown)
        !!
        !!     ! Retrieve the second data set added
        !!     ptr => plt%get(2)
        !! end program
        !! @endcode
        procedure, public :: get => plt_get
        !> @brief Sets the requested plot_data object into the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set(class(plot) this, integer(int32) i, class(plot_data) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] i The index of the plot_data object.
        !! @param[in] x The plot_data object.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     type(plot_data_2d) :: dataset
        !!
        !!     ! Add some data to the plot ... (not shown)
        !!
        !!     ! Add dataset to the second spot in the collection
        !!     call plt%set(2, dataset)
        !! end program
        !! @endcode
        procedure, public :: set => plt_set
        !> @brief Gets the GNUPLOT terminal object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(terminal) function, pointer get_terminal(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return A pointer to the GNUPLOT terminal object.
        !!
        !! @par Example
        !! See @p png_terminal for an example.
        procedure, public :: get_terminal => plt_get_term
        !> @brief Gets a flag determining if the grid lines should be shown.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_show_gridlines(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return Returns true if the grid lines should be shown; else, false.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     logical :: check
        !!
        !!     check = plt%get_show_gridlines()
        !! end program
        !! @endcode
        procedure, public :: get_show_gridlines => plt_get_show_grid
        !> @brief Sets a flag determining if the grid lines should be shown.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_show_gridlines(class(plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] x Set to true if the grid lines should be shown; else, false.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!
        !!     ! Turn off the gridlines
        !!     call plt%set_show_gridlines(.false.)
        !! end program
        !! @endcode
        procedure, public :: set_show_gridlines => plt_set_show_grid
        !> @brief Launches GNUPLOT and draws the plot per the current state of
        !! the command list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine draw(class(plot) this, optional logical persist, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @param[in] persist An optional parameter that can be used to keep GNUPLOT
        !!  open.  Set to true to force GNUPLOT to remain open; else, set to false
        !!  to allow GNUPLOT to close after drawing.  The default is true.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_GNUPLOT_FILE_ERROR: Occurs if the command file cannot be written.
        !!
        !! @par Example
        !! See @p png_terminal for an example.
        procedure, public :: draw => plt_draw
        !> @brief Saves a GNUPLOT command file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine save_file(class(plot) this, character(len = *) fname, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @param[in] fname The filename.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_GNUPLOT_FILE_ERROR: Occurs if the command file cannot be written.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     ! Local Variables & Parameters
        !!     integer(int32), parameter :: npts = 1000
        !!     real(real64), dimension(npts) :: x, y
        !!     type(plot_2d) :: plt
        !!     type(plot_data_2d) :: dataset
        !!     class(plot_axis), pointer :: xAxis, yAxis
        !!     type(legend), pointer :: leg
        !!
        !!     ! Build a data set to plot
        !!     x = linspace(0.0d0, 10.0d0, npts)
        !!     y = exp(-0.5d0 * x) * sin(10.0d0 * x - 0.5d0)
        !!
        !!     call dataset%define_data(x, y)
        !!
        !!     ! Set up the plot
        !!     call plt%initialize()
        !!     call plt%set_title("Example Plot")
        !!
        !!     xAxis => plt%get_x_axis()
        !!     call xAxis%set_title("X Axis")
        !!
        !!     yAxis => plt%get_y_axis()
        !!     call yAxis%set_title("Y Axis")
        !!
        !!     ! Hide the legend
        !!     leg => plt%get_legend()
        !!     call leg%set_is_visible(.false.)
        !!
        !!     ! Add the data to the plot
        !!     call plt%push(dataset)
        !!
        !!     ! Save the plot to a file that can be opened by GNUPLOT at a later time
        !!     call plt%save_file("example_gnuplot_file.plt")
        !! end program
        !! @endcode
        !! Then, from gnuplot, simply issue the command: load
        !! "example_gnuplot_file.plt" to obtain the plot.
        !! @image html example_plot_from_file.png
        procedure, public :: save_file => plt_save
        !> @brief Gets the name of the font used for plot text.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_font_name(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return The font name.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     character(len = :), allocatable :: name
        !!
        !!     name = plt%get_font_name()
        !! end program
        !! @endcode
        procedure, public :: get_font_name => plt_get_font
        !> @brief Sets the name of the font used for plot text.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_font_name(class(plot) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] x The font name.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!
        !!     ! Establish the font used by the plot as Arial.
        !!     call plt%set_title("Arial")
        !! end program
        !! @endcode
        procedure, public :: set_font_name => plt_set_font
        !> @brief Gets the size of the font used by the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) function get_font_size(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return The size of the font, in points.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     integer(int32) :: sz
        !!
        !!     sz = plt%get_font_size()
        !! end program
        !! @endcode
        procedure, public :: get_font_size => plt_get_font_size
        !> @brief Sets the size of the font used by the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_font_size(class(plot) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] x The font size, in points.  If a value of zero is provided,
        !! the font size is reset to its default value; or, if a negative value
        !! is provided, the absolute value of the supplied value is utilized.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!
        !!     ! Set the font to be 14 point in size
        !!     call plt%set_font_size(14)
        !! end program
        !! @endcode
        procedure, public :: set_font_size => plt_set_font_size
        !> @brief Gets a value determining if the axis tic marks should point
        !! inwards.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_tics_inward(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return Returns true if the tic marks should point inwards; else, false
        !!  if the tic marks should point outwards.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     logical :: check
        !!
        !!     check = plt%get_tics_inward()
        !! end program
        !! @endcode
        procedure, public :: get_tics_inward => plt_get_tics_in
        !> @brief Sets a value determining if the axis tic marks should point
        !! inwards.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_tics_inward(class(plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] x Set to true if the tic marks should point inwards; else,
        !!  false if the tic marks should point outwards.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!
        !!     ! Point the axes tic marks outward
        !!     call plt%set_tics_inward(.false.)
        !! end program
        !! @endcode
        procedure, public :: set_tics_inward => plt_set_tics_in
        !> @brief Gets a value determining if the border should be drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_draw_border(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return Returns true if the border should be drawn; else, false.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     logical :: check
        !!
        !!     check = plt%get_draw_border()
        !! end program
        !! @endcode
        procedure, public :: get_draw_border => plt_get_draw_border
        !> @brief Sets a value determining if the border should be drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_draw_border(class(plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] x Set to true if the border should be drawn; else, false.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!
        !!     ! Shut off the axes border
        !!     call plt%set_draw_border(.false.)
        !! end program
        !! @endcode
        procedure, public :: set_draw_border => plt_set_draw_border
        !> @brief Adds a label to the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine push_label(class(plot) this, class(plot_labels) lbl, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] lbl
        !! @param[in] err
        !!
        !! @par Example
        !! The following example illustrates how to add a label to a plot.  A 2D
        !! plot is utilized, but any plot-based type could be used.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     ! Local Variables
        !!     integer(int32), parameter :: npts = 1000
        !!     real(real64), dimension(npts) :: x, y
        !!     type(plot_2d) :: plt
        !!     type(plot_data_2d) :: dataset
        !!     class(plot_axis), pointer :: xAxis, yAxis
        !!     type(legend), pointer :: leg
        !!     type(plot_label) :: lbl
        !!
        !!     ! Build a data set
        !!     x = linspace(0.0d0, 10.0d0, npts)
        !!     y = sin(10.0d0 * x) * sin(0.5d0 * x)
        !!
        !!     call dataset%define_data(y)
        !!
        !!     ! Define the label
        !!     call lbl%set_text("Test Label 1")
        !!     call lbl%set_position([600.0, 0.6, 0.0])
        !!
        !!     ! Set up the plot
        !!     call plt%initialize()
        !!     call plt%set_title("Example Plot")
        !!     call plt%set_font_size(14)
        !!     call plt%set_show_gridlines(.false.)
        !!
        !!     ! Add the label to the plot
        !!     call plt%push_label(lbl)
        !!
        !!     xAxis => plt%get_x_axis()
        !!     call xAxis%set_title("X Axis")
        !!
        !!     yAxis => plt%get_y_axis()
        !!     call yAxis%set_title("Y Axis")
        !!
        !!     ! Hide the legend
        !!     leg => plt%get_legend()
        !!     call leg%set_is_visible(.false.)
        !!
        !!     ! Add the data to the plot
        !!     call plt%push(dataset)
        !!
        !!     ! Draw
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_label_plot.png
        procedure, public :: push_label => plt_push_label
        !> @brief Removes the last label from the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine pop_label(class(plot) this)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!
        !!     call plt%pop_label()
        !! end program
        !! @endcode
        procedure, public :: pop_label => plt_pop_label
        !> @brief Gets the requested plot_label from the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_label) pointer function get_label(class(plot) this, integer(int32) i)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @param[in] i The index of the plot_label object to retrieve.
        !! @return A pointer to the requested plot_label object.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     class(plot_label), pointer :: lbl
        !!
        !!     lbl => plt%get_label(1)
        !! end program
        !! @endcode
        procedure, public :: get_label => plt_get_label
        !> @brief Sets the specified plot_label object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_label(class(plot) this, integer(int32) i, class(plot_label) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] i The index of the plot_label to replace.
        !! @param[in] x The new plot_label object.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     type(plot_label) :: lbl
        !!
        !!     call plt%set_label(1, lbl)
        !! end program
        !! @endcode
        procedure, public :: set_label => plt_set_label
        !> @brief Gets the number of plot_label objects belonging to the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_label_count(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return The number of plot_label objects.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     integer(int32) :: n
        !!
        !!     n = plt%get_label_count()
        !! end program
        !! @endcode
        procedure, public :: get_label_count => plt_get_label_count
        !> @brief Clears all plot_label objects from the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine clear_all_labels(class(plot) this)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !!
        !! @par Example
        !! This example uses a plot_2d type, but this example is valid for any
        !! type that derives from the plot type.
        !! @code{.f90}
        !! program
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!
        !!     call plt%clear_all_labels()
        !! end program
        !! @endcode
        procedure, public :: clear_all_labels => plt_clear_labels
    end type

! ------------------------------------------------------------------------------
    interface
        module subroutine plt_clean_up(this)
            class(plot), intent(inout) :: this
        end subroutine

        module subroutine plt_init(this, term, fname, err)
            class(plot), intent(inout) :: this
            integer(int32), intent(in), optional :: term
            character(len = *), intent(in), optional :: fname
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function plt_get_title(this) result(txt)
            class(plot), intent(in) :: this
            character(len = :), allocatable :: txt
        end function

        module subroutine plt_set_title(this, txt)
            class(plot), intent(inout) :: this
            character(len = *), intent(in) :: txt
        end subroutine

        pure module function plt_has_title(this) result(x)
            class(plot), intent(in) :: this
            logical :: x
        end function

        module function plt_get_legend(this) result(x)
            class(plot), intent(in) :: this
            type(legend), pointer :: x
        end function

        pure module function plt_get_count(this) result(x)
            class(plot), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine plt_push_data(this, x, err)
            class(plot), intent(inout) :: this
            class(plot_data), intent(inout) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine plt_pop_data(this)
            class(plot), intent(inout) :: this
        end subroutine

        module subroutine plt_clear_all(this)
            class(plot), intent(inout) :: this
        end subroutine

        module function plt_get(this, i) result(x)
            class(plot), intent(in) :: this
            integer(int32), intent(in) :: i
            class(plot_data), pointer :: x
        end function

        module subroutine plt_set(this, i, x)
            class(plot), intent(inout) :: this
            integer(int32), intent(in) :: i
            class(plot_data), intent(in) :: x
        end subroutine

        module function plt_get_term(this) result(x)
            class(plot), intent(in) :: this
            class(terminal), pointer :: x
        end function

        pure module function plt_get_show_grid(this) result(x)
            class(plot), intent(in) :: this
            logical :: x
        end function

        module subroutine plt_set_show_grid(this, x)
            class(plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module subroutine plt_draw(this, persist, err)
            class(plot), intent(in) :: this
            logical, intent(in), optional :: persist
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine plt_save(this, fname, err)
            class(plot), intent(in) :: this
            character(len = *), intent(in) :: fname
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function plt_get_font(this) result(x)
            class(plot), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine plt_set_font(this, x)
            class(plot), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine

        module function plt_get_font_size(this) result(x)
            class(plot), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine plt_set_font_size(this, x)
            class(plot), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        pure module function plt_get_tics_in(this) result(x)
            class(plot), intent(in) :: this
            logical :: x
        end function

        module subroutine plt_set_tics_in(this, x)
            class(plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function plt_get_draw_border(this) result(x)
            class(plot), intent(in) :: this
            logical :: x
        end function

        module subroutine plt_set_draw_border(this, x)
            class(plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module subroutine plt_push_label(this, lbl, err)
            class(plot), intent(inout) :: this
            class(plot_label), intent(in) :: lbl
            class(errors), intent(inout), optional, target :: err
        end subroutine

         module subroutine plt_pop_label(this)
            class(plot), intent(inout) :: this
        end subroutine

        module function plt_get_label(this, i) result(x)
            class(plot), intent(in) :: this
            integer(int32), intent(in) :: i
            class(plot_label), pointer :: x
        end function

        module subroutine plt_set_label(this, i, x)
            class(plot), intent(inout) :: this
            integer(int32), intent(in) :: i
            class(plot_label), intent(in) :: x
        end subroutine

        pure module function plt_get_label_count(this) result(x)
            class(plot), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine plt_clear_labels(this)
            class(plot), intent(inout) :: this
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_COLORMAP.F90
! ------------------------------------------------------------------------------
    !> @brief A colormap object for a surface plot.
    type, abstract, extends(plot_object) :: colormap
    contains
        !> @brief Gets the GNUPLOT command string to represent this colormap
        !! object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable :: get_command_string(class(colormap) this)
        !! @endcode
        !!
        !! @param[in] this The colormap object.
        !! @return The command string.
        procedure, public :: get_command_string => cm_get_cmd
        !> @brief Gets the GNUPLOT string defining the color distribution.  For
        !! instance, this routine could return the string: '0 "dark-blue",
        !! 1 "blue", 2 "cyan", 3 "green", 4 "yellow", 5 "orange", 6 "red",
        !! 7 "dark-red"'.  This string would result in a rainbow type map.
        procedure(cm_get_string_result), deferred, public :: get_color_string
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a rainbow colormap.
    !!
    !! @par Example
    !! The following example illustrates a surface plot using a rainbow
    !! colormap.
    !! @code{.f90}
    !! program example
    !!     use, intrinsic :: iso_fortran_env
    !!     use fplot_core
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: m = 50
    !!     integer(int32), parameter :: n = 50
    !!     real(real64), parameter :: xMax = 5.0d0
    !!     real(real64), parameter :: xMin = -5.0d0
    !!     real(real64), parameter :: yMax = 5.0d0
    !!     real(real64), parameter :: yMin = -5.0d0
    !!
    !!     ! Local Variables
    !!     real(real64), dimension(n) :: xdata
    !!     real(real64), dimension(m) :: ydata
    !!     real(real64), dimension(:,:), pointer :: x, y
    !!     real(real64), dimension(m, n, 2), target :: xy
    !!     real(real64), dimension(m, n) :: z
    !!     type(surface_plot) :: plt
    !!     type(surface_plot_data) :: d1
    !!     type(rainbow_colormap) :: map ! Using a rainbow colormap
    !!     class(plot_axis), pointer :: xAxis, yAxis, zAxis
    !!
    !!     ! Define the data
    !!     xdata = linspace(xMin, xMax, n)
    !!     ydata = linspace(yMin, yMax, m)
    !!     xy = meshgrid(xdata, ydata)
    !!     x => xy(:,:,1)
    !!     y => xy(:,:,2)
    !!
    !!     ! Define the function to plot
    !!     z = sin(sqrt(x**2 + y**2))
    !!
    !!     ! Create the plot
    !!     call plt%initialize()
    !!     call plt%set_colormap(map)
    !!
    !!     ! Define titles
    !!     call plt%set_title("Surface Example Plot 1")
    !!
    !!     xAxis => plt%get_x_axis()
    !!     call xAxis%set_title("X Axis")
    !!
    !!     yAxis => plt%get_y_axis()
    !!     call yAxis%set_title("Y Axis")
    !!
    !!     zAxis => plt%get_z_axis()
    !!     call zAxis%set_title("Z Axis")
    !!
    !!     ! Define the data set
    !!     call d1%define_data(x, y, z)
    !!     call d1%set_name("sin(sqrt(x**2 + y**2))")
    !!     call plt%push(d1)
    !!
    !!     ! Let GNUPLOT draw the plot
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! @image html example_surface_plot.png
    type, extends(colormap) :: rainbow_colormap
    contains
        !> @brief Gets the GNUPLOT string defining the color distribution.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_color_string(class(rainbow_colormap) this)
        !! @endcode
        !!
        !! @param[in] this The rainbow_colormap object.
        !! @return The command string.
        procedure, public :: get_color_string => rcm_get_clr
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a colormap consisting of "hot" colors.
    !!
    !! @par Example
    !! The following example illustrates a surface plot using a rainbow
    !! colormap.
    !! @code{.f90}
    !! program example
    !!     use, intrinsic :: iso_fortran_env
    !!     use fplot_core
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: m = 50
    !!     integer(int32), parameter :: n = 50
    !!     real(real64), parameter :: xMax = 5.0d0
    !!     real(real64), parameter :: xMin = -5.0d0
    !!     real(real64), parameter :: yMax = 5.0d0
    !!     real(real64), parameter :: yMin = -5.0d0
    !!
    !!     ! Local Variables
    !!     real(real64), dimension(n) :: xdata
    !!     real(real64), dimension(m) :: ydata
    !!     real(real64), dimension(:,:), pointer :: x, y
    !!     real(real64), dimension(m, n, 2), target :: xy
    !!     real(real64), dimension(m, n) :: z
    !!     type(surface_plot) :: plt
    !!     type(surface_plot_data) :: d1
    !!     type(hot_colormap) :: map ! Using a hot colormap
    !!     class(plot_axis), pointer :: xAxis, yAxis, zAxis
    !!
    !!     ! Define the data
    !!     xdata = linspace(xMin, xMax, n)
    !!     ydata = linspace(yMin, yMax, m)
    !!     xy = meshgrid(xdata, ydata)
    !!     x => xy(:,:,1)
    !!     y => xy(:,:,2)
    !!
    !!     ! Define the function to plot
    !!     z = sin(sqrt(x**2 + y**2))
    !!
    !!     ! Create the plot
    !!     call plt%initialize()
    !!     call plt%set_colormap(map)
    !!
    !!     ! Define titles
    !!     call plt%set_title("Surface Example Plot 1")
    !!
    !!     xAxis => plt%get_x_axis()
    !!     call xAxis%set_title("X Axis")
    !!
    !!     yAxis => plt%get_y_axis()
    !!     call yAxis%set_title("Y Axis")
    !!
    !!     zAxis => plt%get_z_axis()
    !!     call zAxis%set_title("Z Axis")
    !!
    !!     ! Define the data set
    !!     call d1%define_data(x, y, z)
    !!     call d1%set_name("sin(sqrt(x**2 + y**2))")
    !!     call plt%push(d1)
    !!
    !!     ! Let GNUPLOT draw the plot
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! @image html example_surface_plot_hot.png
    type, extends(colormap) :: hot_colormap
    contains
        !> @brief Gets the GNUPLOT string defining the color distribution.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_color_string(class(hot_colormap) this)
        !! @endcode
        !!
        !! @param[in] this The hot_colormap object.
        !! @return The command string.
        procedure, public :: get_color_string => hcm_get_clr
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a colormap consisting of "cool" colors.
    !!
    !! @par Example
    !! The following example illustrates a surface plot using a rainbow
    !! colormap.
    !! @code{.f90}
    !! program example
    !!     use, intrinsic :: iso_fortran_env
    !!     use fplot_core
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: m = 50
    !!     integer(int32), parameter :: n = 50
    !!     real(real64), parameter :: xMax = 5.0d0
    !!     real(real64), parameter :: xMin = -5.0d0
    !!     real(real64), parameter :: yMax = 5.0d0
    !!     real(real64), parameter :: yMin = -5.0d0
    !!
    !!     ! Local Variables
    !!     real(real64), dimension(n) :: xdata
    !!     real(real64), dimension(m) :: ydata
    !!     real(real64), dimension(:,:), pointer :: x, y
    !!     real(real64), dimension(m, n, 2), target :: xy
    !!     real(real64), dimension(m, n) :: z
    !!     type(surface_plot) :: plt
    !!     type(surface_plot_data) :: d1
    !!     type(cool_colormap) :: map ! Using a cool colormap
    !!     class(plot_axis), pointer :: xAxis, yAxis, zAxis
    !!
    !!     ! Define the data
    !!     xdata = linspace(xMin, xMax, n)
    !!     ydata = linspace(yMin, yMax, m)
    !!     xy = meshgrid(xdata, ydata)
    !!     x => xy(:,:,1)
    !!     y => xy(:,:,2)
    !!
    !!     ! Define the function to plot
    !!     z = sin(sqrt(x**2 + y**2))
    !!
    !!     ! Create the plot
    !!     call plt%initialize()
    !!     call plt%set_colormap(map)
    !!
    !!     ! Define titles
    !!     call plt%set_title("Surface Example Plot 1")
    !!
    !!     xAxis => plt%get_x_axis()
    !!     call xAxis%set_title("X Axis")
    !!
    !!     yAxis => plt%get_y_axis()
    !!     call yAxis%set_title("Y Axis")
    !!
    !!     zAxis => plt%get_z_axis()
    !!     call zAxis%set_title("Z Axis")
    !!
    !!     ! Define the data set
    !!     call d1%define_data(x, y, z)
    !!     call d1%set_name("sin(sqrt(x**2 + y**2))")
    !!     call plt%push(d1)
    !!
    !!     ! Let GNUPLOT draw the plot
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! @image html example_surface_plot_cool.png
    type, extends(colormap) :: cool_colormap
    contains
        !> @brief Gets the GNUPLOT string defining the color distribution.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_color_string(class(cool_colormap) this)
        !! @endcode
        !!
        !! @param[in] this The cool_colormap object.
        !! @return The command string.
        procedure, public :: get_color_string => ccm_get_clr
    end type

! ------------------------------------------------------------------------------
    interface
        module function cm_get_cmd(this) result(x)
            class(colormap), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function rcm_get_clr(this) result(x)
            class(rainbow_colormap), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function hcm_get_clr(this) result(x)
            class(hot_colormap), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function ccm_get_clr(this) result(x)
            class(cool_colormap), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_SCATTER_PLOT_DATA.F90
! ------------------------------------------------------------------------------
    !> @brief A plot_data object for describing scatter plot data sets.
    type, abstract, extends(plot_data_colored) :: scatter_plot_data
    private
        !> Draw the line?
        logical :: m_drawLine = .true.
        !> Draw the markers?
        logical :: m_drawMarkers = .false.
        !> Marker frequency.
        integer(int32) :: m_markerFrequency = 1
        !> Line width.
        real(real32) :: m_lineWidth = 1.0
        !> Line style.
        integer(int32) :: m_lineStyle = LINE_SOLID
        !> Marker type.
        integer(int32) :: m_markerType = MARKER_X
        !> Marker size multiplier.
        real(real32) :: m_markerSize = 1.0
        !> True if large data sets should be simplified before sending to
        !! GNUPLOT.
        logical :: m_simplifyData = .true.
        !> A scaling factor used to establish the simplification tolerance.
        !! The simplification tolerance is established by multiplying this
        !! factor by the range in the dependent variable data.
        real(real64) :: m_simplifyFactor = 1.0d-3
    contains
        !> @brief Gets the GNUPLOT command string to represent this
        !! scatter_plot_data object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return The command string.
        procedure, public :: get_command_string => spd_get_cmd
        !> @brief Gets the width of the line, in pixels.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_line_width(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return The line width.
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     real(real32) :: width
        !!
        !!     ! Get the line width
        !!     width = pd%get_line_width()
        !! end program
        !! @endcode
        procedure, public :: get_line_width => spd_get_line_width
        !> @brief Sets the width of the line, in pixels.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_line_width(class(scatter_plot_data) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x The line width.
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!
        !!     ! Set the line width
        !!     call pd%set_line_width(2.0)
        !! end program
        !! @endcode
        procedure, public :: set_line_width => spd_set_line_width
        !> @brief Gets the line style.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_line_style(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return The line style.  The line style must be one of the following:
        !!  - LINE_DASHED
        !!  - LINE_DASH_DOTTED
        !!  - LINE_DASH_DOT_DOT
        !!  - LINE_DOTTED
        !!  - LINE_SOLID
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     integer(int32) :: style
        !!
        !!     ! Get the line style
        !!     style = pd%get_line_style()
        !! end program
        !! @endcode
        procedure, public :: get_line_style => spd_get_line_style
        !> @brief Sets the line style.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_line_style(class(scatter_plot_data) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x The line style.  The line style must be one of the
        !!      following:
        !!  - LINE_DASHED
        !!  - LINE_DASH_DOTTED
        !!  - LINE_DASH_DOT_DOT
        !!  - LINE_DOTTED
        !!  - LINE_SOLID
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!
        !!     ! Set the line style
        !!     call pd%get_line_style(LINE_DASHED)
        !! end program
        !! @endcode
        procedure, public :: set_line_style => spd_set_line_style
        !> @brief Gets a value determining if a line should be drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_draw_line(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return Returns true if the line should be drawn; else, false.
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     logical :: check
        !!
        !!     ! Check to see if a line should be drawn to connect data points
        !!     check = pd%get_draw_line()
        !! end program
        !! @endcode
        procedure, public :: get_draw_line => spd_get_draw_line
        !> @brief Sets a value determining if a line should be drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_draw_line(class(scatter_plot_data) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x Set to true if the line should be drawn; else, false.
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!
        !!     ! Force a line to be drawn between data points
        !!     call pd%set_draw_line(.true.)
        !! end program
        !! @endcode
        procedure, public :: set_draw_line => spd_set_draw_line
        !> @brief Gets a value determining if data point markers should be
        !! drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_draw_markers(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return Returns true if the markers should be drawn; else, false.
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     logical :: check
        !!
        !!     ! Check to see if markers should be drawn at data points
        !!     check = pd%get_draw_markers()
        !! end program
        !! @endcode
        procedure, public :: get_draw_markers => spd_get_draw_markers
        !> @brief Sets a value determining if data point markers should be
        !! drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_draw_markers(class(scatter_plot_data) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x Set to true if the markers should be drawn; else, false.
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!
        !!     ! Force markers to be drawn at data points
        !!     call pd%set_draw_markers(.true.)
        !! end program
        !! @endcode
        procedure, public :: set_draw_markers => spd_set_draw_markers
        !> @brief Gets the marker style.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_marker_style(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return The marker type.  The marker type must be one of the following:
        !!  - MARKER_ASTERISK
        !!  - MARKER_EMPTY_CIRCLE
        !!  - MARKER_EMPTY_NABLA
        !!  - MARKER_EMPTY_RHOMBUS
        !!  - MARKER_EMPTY_SQUARE
        !!  - MARKER_EMPTY_TRIANGLE
        !!  - MARKER_FILLED_CIRCLE
        !!  - MARKER_FILLED_NABLA
        !!  - MARKER_FILLED_RHOMBUS
        !!  - MARKER_FILLED_SQUARE
        !!  - MARKER_FILLED_TRIANGLE
        !!  - MARKER_PLUS
        !!  - MARKER_X
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     integer(int32) :: marker
        !!
        !!     ! Get the data point marker style
        !!     marker = pd%get_marker_style()
        !! end program
        !! @endcode
        procedure, public :: get_marker_style => spd_get_marker_style
        !> @brief Sets the marker style.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_marker_style(class(scatter_plot_data) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x The marker type.  The marker type must be one of the
        !!  following:
        !!  - MARKER_ASTERISK
        !!  - MARKER_EMPTY_CIRCLE
        !!  - MARKER_EMPTY_NABLA
        !!  - MARKER_EMPTY_RHOMBUS
        !!  - MARKER_EMPTY_SQUARE
        !!  - MARKER_EMPTY_TRIANGLE
        !!  - MARKER_FILLED_CIRCLE
        !!  - MARKER_FILLED_NABLA
        !!  - MARKER_FILLED_RHOMBUS
        !!  - MARKER_FILLED_SQUARE
        !!  - MARKER_FILLED_TRIANGLE
        !!  - MARKER_PLUS
        !!  - MARKER_X
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!
        !!     ! Set the data point marker style to a plus (+) sign
        !!     call pd%set_marker_style(MARKER_PLUS)
        !! end program
        !! @endcode
        procedure, public :: set_marker_style => spd_set_marker_style
        !> @brief Gets the marker scaling.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_marker_scaling(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return The scaling factor.
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     real(real32) :: scaling
        !!
        !!     ! Get the data point marker scaling factor
        !!     scaling = pd%get_marker_scaling()
        !! end program
        !! @endcode
        procedure, public :: get_marker_scaling => spd_get_marker_scaling
        !> @brief Sets the marker scaling.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_marker_scaling(class(scatter_plot_data) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x The scaling factor.
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!
        !!     ! Set the data point marker scaling factor such that the marker
        !!     ! is scaled by a factor of 2
        !!     call pd%set_marker_scaling(2.0)
        !! end program
        !! @endcode
        procedure, public :: set_marker_scaling => spd_set_marker_scaling
        !> @brief Gets the marker frequency.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_marker_frequency(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return The marker frequency.
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     integer(int32) :: freq
        !!
        !!     ! Get the data point marker frequency
        !!     freq = pd%get_marker_frequency()
        !! end program
        !! @endcode
        procedure, public :: get_marker_frequency => spd_get_marker_frequency
        !> @brief Sets the marker frequency.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_marker_frequency(class(scatter_plot_data) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x The marker frequency.
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     real(real32) :: scaling
        !!
        !!     ! Set a data point marker every second data point
        !!     call pd%set_marker_frequency(2)
        !! end program
        !! @endcode
        procedure, public :: set_marker_frequency => spd_set_marker_frequency
        !> @brief Gets the number of data points.
        procedure(spd_get_int_value), deferred, public :: get_count
        !> @brief Gets the requested X data point.
        procedure(spd_get_value), deferred, public :: get_x
        !> @brief Sets the requested X data point.
        procedure(spd_set_value), deferred, public :: set_x
        !> @brief Gets the requested Y data point.
        procedure(spd_get_value), deferred, public :: get_y
        !> @brief Sets the requested X data point.
        procedure(spd_set_value), deferred, public :: set_y
        !> @brief Gets the GNUPLOT command string defining which axes the data
        !! is to be plotted against.
        procedure(spd_get_string_result), deferred, public :: get_axes_string
        !> @brief Gets a value determining if the stored data should be
        !! simplified (reduced) before passing to GNUPLOT.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_simplify_data(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return Returns true if the data should be simplified prior to sending
        !!  to GNUPLOT; else, false to leave the data alone.
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     logical :: x
        !!
        !!     x = this%get_simplify_data()
        !! end program
        !! @endcode
        procedure, public :: get_simplify_data => spd_get_simplify_data
        !> @brief Sets a value determining if the stored data should be
        !! simplified (reduced) before passing to GNUPLOT.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_simplify_data(class(scatter_plot_data) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x True if the data should be simplified prior to sending
        !!  to GNUPLOT; else, false to leave the data alone.
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!
        !!     call this%get_simplify_data(.false.)
        !! end program
        !! @endcode
        procedure, public :: set_simplify_data => spd_set_simplify_data
        !> @brief Gets a factor used to establish the simplification tolerance.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_simplification_factor(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return Returns the scaling factor.
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     real(real64) :: x
        !!
        !!     x = this%get_simplification_factor()
        !! end program
        !! @endcode
        procedure, public :: get_simplification_factor => spd_get_simplify_factor
        !> @brief Sets a factor used to establish the simplification tolerance.  The
        !! tolerance is established by multplying this factor by the range of the
        !! dependent variable data.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_simplification_factor(class(scatter_plot_data) this, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x The scaling factor.
        !!
        !! @par Example
        !! This example makes use of the plot_data_2d type; however, this
        !! example is valid for any type that derives from scatter_plot_data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     real(real64) :: x
        !!
        !!     call this%set_simplification_factor(1.0d-3)
        !! end program
        !! @endcode
        procedure, public :: set_simplification_factor => spd_set_simplify_factor
    end type

! ------------------------------------------------------------------------------
    interface
        module function spd_get_cmd(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        pure module function spd_get_line_width(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            real(real32) :: x
        end function

        module subroutine spd_set_line_width(this, x)
            class(scatter_plot_data), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        pure module function spd_get_line_style(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine spd_set_line_style(this, x)
            class(scatter_plot_data), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        pure module function spd_get_draw_line(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            logical :: x
        end function

        module subroutine spd_set_draw_line(this, x)
            class(scatter_plot_data), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function spd_get_draw_markers(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            logical :: x
        end function

        module subroutine spd_set_draw_markers(this, x)
            class(scatter_plot_data), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function spd_get_marker_style(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine spd_set_marker_style(this, x)
            class(scatter_plot_data), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        pure module function spd_get_marker_scaling(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            real(real32) :: x
        end function

        module subroutine spd_set_marker_scaling(this, x)
            class(scatter_plot_data), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        pure module function spd_get_marker_frequency(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine spd_set_marker_frequency(this, x)
            class(scatter_plot_data), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        pure module function spd_get_simplify_data(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            logical :: x
        end function
        
        module subroutine spd_set_simplify_data(this, x)
            class(scatter_plot_data), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function spd_get_simplify_factor(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            real(real64) :: x
        end function
        
        module subroutine spd_set_simplify_factor(this, x)
            class(scatter_plot_data), intent(inout) :: this
            real(real64), intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_PLOT_DATA_2D.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a two-dimensional plot data set.
    type, extends(scatter_plot_data) :: plot_data_2d
    private
        !> An N-by-2 matrix containing the x and y data points.
        real(real64), allocatable, dimension(:,:) :: m_data
        !> Draw against the secondary y axis?
        logical :: m_useY2 = .false.
    contains
        !> @brief Gets the GNUPLOT command string defining which axes the data
        !! is to be plotted against.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_axis_string(class(plot_data_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @return The command string.
        procedure, public :: get_axes_string => pd2d_get_axes_cmd
        !> @brief Gets the GNUPLOT command string containing the actual data
        !! to plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_data_string(class(plot_data_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @return The command string.
        procedure, public :: get_data_string => pd2d_get_data_cmd
        !> @brief Gets the number of data points.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) get_count(class(plot_data_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @return The number of data points.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     integer(int32) :: n
        !!
        !!     ! Get the number of stored data points
        !!     n = pd%get_count()
        !! end program
        !! @endcode
        procedure, public :: get_count => pd2d_get_data_count
        !> @brief Gets the requested X data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) get_x(class(plot_data_2d) this, integer(int32) index)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @param[in] index The index of the data point to retrieve.
        !! @return The requested data point.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     real(real64) :: x
        !!
        !!     ! Get the x data point at the 100th index
        !!     x = pd%get_x(100)
        !! end program
        !! @endcode
        procedure, public :: get_x => pd2d_get_x_data
        !> @brief Sets the requested X data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_x(class(plot_data_2d) this, integer(int32) index, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_2d object.
        !! @param[in] index The index of the data point to replace.
        !! @param[in] x The data point.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!
        !!     ! Set the x data point at the 100th index
        !!     call pd%set_x(100, 1.25d0)
        !! end program
        !! @endcode
        procedure, public :: set_x => pd2d_set_x_data
        !> @brief Gets the requested Y data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) get_y(class(plot_data_2d) this, integer(int32) index)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @param[in] index The index of the data point to retrieve.
        !! @return The requested data point.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     real(real64) :: y
        !!
        !!     ! Get the y data point at the 100th index
        !!     y = pd%get_y(100)
        !! end program
        !! @endcode
        procedure, public :: get_y => pd2d_get_y_data
        !> @brief Sets the requested Y data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_y(class(plot_data_2d) this, integer(int32) index, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_2d object.
        !! @param[in] index The index of the data point to replace.
        !! @param[in] x The data point.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!
        !!     ! Set the y data point at the 100th index
        !!     call pd%set_y(100, 1.25d0)
        !! end program
        !! @endcode
        procedure, public :: set_y => pd2d_set_y_data
        !> @brief Gets a value determining if the data should be plotted against
        !! the secondary y-axis.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_draw_against_y2(class(plot_data_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @return Returns true if the data should be plotted against the secondary
        !!  y-axis; else, false to plot against the primary y-axis.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     logical :: check
        !!
        !!     ! Determine if this data set is plotted against the secondary
        !!     ! y axis.
        !!     check = pd%get_draw_against_y2()
        !! end program
        !! @endcode
        procedure, public :: get_draw_against_y2 => pd2d_get_draw_against_y2
        !> @brief Sets a value determining if the data should be plotted against
        !! the secondary y-axis.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_draw_against_y2(class(plot_data_2d) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_2d object.
        !! @param[in] x Set to true if the data should be plotted against the
        !!  secondary y-axis; else, false to plot against the primary y-axis.
        !!
        !! @par Example
        !! This example illustrates the use of a secondary y axis.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     ! Local Variables
        !!     integer(int32), parameter :: npts = 1000
        !!     real(real64), dimension(npts) :: x, y1, y2
        !!     type(plot_2d) :: plt
        !!     type(plot_data_2d) :: ds1, ds2
        !!     class(plot_axis), pointer :: xAxis, yAxis, y2Axis
        !!
        !!     ! Build a data set
        !!     x = linspace(0.0d0, 10.0d0, npts)
        !!     y1 = exp(-0.5d0 * x) * abs(sin(x))
        !!     y2 = cos(0.5d0 * x) * sin(10.0d0 * x)
        !!
        !!     call ds1%define_data(x, y1)
        !!     call ds1%set_name("f(x) = exp(-x / 2) * |sin(x)|")
        !!
        !!     call ds2%define_data(x, y2)
        !!     call ds2%set_name("f(x) = cos(x / 2) * sin(10 x)")
        !!
        !!     ! Make the ds2 line green and dashed
        !!     call ds2%set_line_color(CLR_GREEN)
        !!     call ds2%set_line_style(LINE_DASHED)
        !!
        !!     ! Draw ds2 against the secondary y axis
        !!     call ds2%set_draw_against_y2(.true.)
        !!
        !!     ! Ensure the plot knows it needs a secondary y axis
        !!     call plt%set_use_y2_axis(.true.)
        !!
        !!     ! Set up the plot
        !!     call plt%initialize()
        !!     call plt%set_title("Example Plot")
        !!
        !!     xAxis => plt%get_x_axis()
        !!     call xAxis%set_title("X Axis")
        !!
        !!     yAxis => plt%get_y_axis()
        !!     call yAxis%set_title("Y Axis")
        !!
        !!     y2Axis => plt%get_y2_axis()
        !!     call y2Axis%set_title("Secondary Y Axis")
        !!
        !!     ! Add the data to the plot
        !!     call plt%push(ds1)
        !!     call plt%push(ds2)
        !!
        !!     ! Draw
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_plot_y2_axis_1.png
        procedure, public :: set_draw_against_y2 => pd2d_set_draw_against_y2
        !> @brief Defines the data set.
        !!
        !! @par Overload 1
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_data(class(plot_data_2d) this, real(real64) x(:), real(real64) y(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_2d object.
        !! @param[in] x An N-element array containing the x coordinate data.
        !! @param[in] y An N-element array containing the y coordinate data.
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if @p x and @p y are not the
        !!      same size.
        !!
        !! @par Example
        !! The following example illustrates the use of the first overload.
        !! This form of the routine simply plots the supplied y coordinate
        !! data against the supplied x coordinate data.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     ! Local Variables
        !!     integer(int32), parameter :: npts = 1000
        !!     real(real64), dimension(npts) :: x, y
        !!     type(plot_2d) :: plt
        !!     type(plot_data_2d) :: dataset
        !!     class(plot_axis), pointer :: xAxis, yAxis
        !!     type(legend), pointer :: leg
        !!
        !!     ! Build a data set
        !!     x = linspace(0.0d0, 10.0d0, npts)
        !!     y = sin(10.0d0 * x) * sin(0.5d0 * x)
        !!
        !!     call dataset%define_data(x, y)
        !!
        !!     ! Set up the plot
        !!     call plt%initialize()
        !!     call plt%set_title("Example Plot")
        !!
        !!     xAxis => plt%get_x_axis()
        !!     call xAxis%set_title("X Axis")
        !!
        !!     yAxis => plt%get_y_axis()
        !!     call yAxis%set_title("Y Axis")
        !!
        !!     ! Hide the legend
        !!     leg => plt%get_legend()
        !!     call leg%set_is_visible(.false.)
        !!
        !!     ! Add the data to the plot
        !!     call plt%push(dataset)
        !!
        !!     ! Draw
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_plot_2d_1.png
        !!
        !! @par Overload 2
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_data(class(plot_data_2d) this, real(real64) y(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_2d object.
        !! @param[in] y An N-element array containing the y-coordinate data.  This
        !!  data will be plotted against its own index.
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!
        !! @par Example
        !! The following example illustrates the use of the second overload.
        !! This form of the routine simply plots the data against its array
        !! index (one-based).
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     ! Local Variables
        !!     integer(int32), parameter :: npts = 1000
        !!     real(real64), dimension(npts) :: x, y
        !!     type(plot_2d) :: plt
        !!     type(plot_data_2d) :: dataset
        !!     class(plot_axis), pointer :: xAxis, yAxis
        !!     type(legend), pointer :: leg
        !!
        !!     ! Build a data set
        !!     x = linspace(0.0d0, 10.0d0, npts)
        !!     y = sin(10.0d0 * x) * sin(0.5d0 * x)
        !!
        !!     call dataset%define_data(y)
        !!
        !!     ! Set up the plot
        !!     call plt%initialize()
        !!     call plt%set_title("Example Plot")
        !!
        !!     xAxis => plt%get_x_axis()
        !!     call xAxis%set_title("X Axis")
        !!
        !!     yAxis => plt%get_y_axis()
        !!     call yAxis%set_title("Y Axis")
        !!
        !!     ! Hide the legend
        !!     leg => plt%get_legend()
        !!     call leg%set_is_visible(.false.)
        !!
        !!     ! Add the data to the plot
        !!     call plt%push(dataset)
        !!
        !!     ! Draw
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_plot_2d_2.png
        generic, public :: define_data => pd2d_set_data_1, pd2d_set_data_2
        procedure :: pd2d_set_data_1
        procedure :: pd2d_set_data_2

        !> @brief Gets the stored X data array.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_x_data(class(plot_data_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @return A copy of the stored data array.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     real(real64), allocatable, dimension(:) :: x
        !!
        !!     ! Get the data array
        !!     x = pd%get_x_data()
        !! end program
        !! @endcode
        procedure, public :: get_x_data => pd2d_get_x_array
        !> @brief Gets the stored Y data array.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_y_data(class(plot_data_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @return A copy of the stored data array.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     real(real64), allocatable, dimension(:) :: y
        !!
        !!     ! Get the data array
        !!     y = pd%get_y_data()
        !! end program
        !! @endcode
        procedure, public :: get_y_data => pd2d_get_y_array
    end type

! ------------------------------------------------------------------------------
    interface
        module function pd2d_get_axes_cmd(this) result(x)
            class(plot_data_2d), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function pd2d_get_data_cmd(this) result(x)
            class(plot_data_2d), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        pure module function pd2d_get_data_count(this) result(x)
            class(plot_data_2d), intent(in) :: this
            integer(int32) :: x
        end function

        pure module function pd2d_get_x_data(this, index) result(x)
            class(plot_data_2d), intent(in) :: this
            integer(int32), intent(in) :: index
            real(real64) :: x
        end function

        module subroutine pd2d_set_x_data(this, index, x)
            class(plot_data_2d), intent(inout) :: this
            integer(int32), intent(in) :: index
            real(real64), intent(in) :: x
        end subroutine

        pure module function pd2d_get_y_data(this, index) result(x)
            class(plot_data_2d), intent(in) :: this
            integer(int32), intent(in) :: index
            real(real64) :: x
        end function

        module subroutine pd2d_set_y_data(this, index, x)
            class(plot_data_2d), intent(inout) :: this
            integer(int32), intent(in) :: index
            real(real64), intent(in) :: x
        end subroutine

        module subroutine pd2d_set_data_1(this, x, y, err)
            class(plot_data_2d), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x, y
            class(errors), intent(inout), optional, target :: err
        end subroutine

        pure module function pd2d_get_draw_against_y2(this) result(x)
            class(plot_data_2d), intent(in) :: this
            logical :: x
        end function

        module subroutine pd2d_set_draw_against_y2(this, x)
            class(plot_data_2d), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module subroutine pd2d_set_data_2(this, y, err)
            class(plot_data_2d), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: y
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function pd2d_get_x_array(this) result(x)
            class(plot_data_2d), intent(in) :: this
            real(real64), allocatable, dimension(:) :: x
        end function

        module function pd2d_get_y_array(this) result(x)
            class(plot_data_2d), intent(in) :: this
            real(real64), allocatable, dimension(:) :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_PLOT_DATA_3D.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a three-dimensional plot data set.
    type, extends(scatter_plot_data) :: plot_data_3d
    private
        !> An N-by-3 matrix containing the x, y, and z data points.
        real(real64), allocatable, dimension(:,:) :: m_data
    contains
        !> @brief Gets the number of data points.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_count(class(plot_data_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @return The number of data points.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_3d) :: pd
        !!     integer(int32) :: n
        !!
        !!     ! Get the number of stored data points
        !!     n = pd%get_count()
        !! end program
        !! @endcode
        procedure, public :: get_count => pd3d_get_data_count
        !> @brief Gets the requested X data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_x(class(plot_data_3d), this, integer(int32) index)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @param[in] index The index of the data point to retrieve.
        !! @return The requested data point.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_3d) :: pd
        !!     real(real64) :: x
        !!
        !!     ! Get the 10th value from the x-coordinate data
        !!     x = pd%get_x(10)
        !! end program
        !! @endcode
        procedure, public :: get_x => pd3d_get_x_data
        !> @brief Sets the requested X data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_x(class(plot_data_3d) this, integer(int32) index, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_3d object.
        !! @param[in] index The index of the data point to replace.
        !! @param[in] x The data point.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_3d) :: pd
        !!
        !!     ! Set the 10th value in the x-coordinate data
        !!     call pd%set_x(10, 50.0d0)
        !! end program
        !! @endcode
        procedure, public :: set_x => pd3d_set_x_data
        !> @brief Gets the requested Y data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_y(class(plot_data_3d) this, this, integer(int32) index)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @param[in] index The index of the data point to retrieve.
        !! @return The requested data point.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_3d) :: pd
        !!     real(real64) :: y
        !!
        !!     ! Get the 10th value from the y-coordinate data
        !!     y = pd%get_y(10)
        !! end program
        !! @endcode
        procedure, public :: get_y => pd3d_get_y_data
        !> @brief Sets the requested Y data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_y(class(plot_data_3d) this, integer(int32) index, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_3d object.
        !! @param[in] index The index of the data point to replace.
        !! @param[in] x The data point.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_3d) :: pd
        !!
        !!     ! Set the 10th value in the y-coordinate data
        !!     call pd%set_y(10, 50.0d0)
        !! end program
        !! @endcode
        procedure, public :: set_y => pd3d_set_y_data
        !> @brief Gets the requested Z data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_z(class(plot_data_3d) this, this, integer(int32) index)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @param[in] index The index of the data point to retrieve.
        !! @return The requested data point.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_3d) :: pd
        !!     real(real64) :: z
        !!
        !!     ! Get the 10th value from the z-coordinate data
        !!     z = pd%get_z(10)
        !! end program
        !! @endcode
        procedure, public :: get_z => pd3d_get_z_data
        !> @brief Sets the requested Z data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_z(class(plot_data_3d) this, integer(int32) index, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_3d object.
        !! @param[in] index The index of the data point to replace.
        !! @param[in] x The data point.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_3d) :: pd
        !!
        !!     ! Set the 10th value in the z-coordinate data
        !!     call pd%set_z(10, 50.0d0)
        !! end program
        !! @endcode
        procedure, public :: set_z => pd3d_set_z_data
        !> @brief Gets the GNUPLOT command string defining which axes the data
        !! is to be plotted against.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable :: get_axes_string(class(plot_data_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @return The command string.
        procedure, public :: get_axes_string => pd3d_get_axes_cmd
        !> @brief Gets the GNUPLOT command string containing the actual data
        !! to plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable :: get_data_string(class(plot_data_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @return The command string.
        procedure, public :: get_data_string => pd3d_get_data_cmd
        !> @brief Defines the data set.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_data(class(plot_data_3d) this, real(real64) x(:), real(real64) y(:), real(real64) z(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_2d object.
        !! @param[in] x An N-element array containing the x coordinate data.
        !! @param[in] y An N-element array containing the y coordinate data.
        !! @param[in] z An N-element array containing the z coordinate data.
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if @p x, @p y, and @p z are
        !!      not the same size.
        !!
        !! @par Example
        !! The following example adds data to draw a helix to a 3D plot.
        !! @code{.f90}
        !! program example
        !!     use, intrinsic :: iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     ! Parameters
        !!     integer(int32), parameter :: n = 1000
        !!
        !!     ! Local Variables
        !!     real(real64), dimension(n) :: t, x, y, z
        !!     type(plot_3d) :: plt
        !!     type(plot_data_3d) :: d1
        !!     class(plot_axis), pointer :: xAxis, yAxis, zAxis
        !!     type(legend), pointer :: leg
        !!
        !!     ! Initialize the plot object
        !!     call plt%initialize()
        !!     leg => plt%get_legend()
        !!     call leg%set_is_visible(.false.)
        !!
        !!     ! Define titles
        !!     call plt%set_title("Example Plot")
        !!
        !!     xAxis => plt%get_x_axis()
        !!     call xAxis%set_title("X Axis")
        !!
        !!     yAxis => plt%get_y_axis()
        !!     call yAxis%set_title("Y Axis")
        !!
        !!     zAxis => plt%get_z_axis()
        !!     call zAxis%set_title("Z Axis")
        !!
        !!     ! Define the data
        !!     t = linspace(0.0d0, 10.0d0, n)
        !!     x = cos(5.0d0 * t)
        !!     y = sin(5.0d0 * t)
        !!     z = 2.0d0 * t
        !!
        !!     call d1%define_data(x, y, z)
        !!
        !!     ! Set up the data set
        !!     call d1%set_line_color(CLR_BLUE)
        !!     call d1%set_line_width(2.0)
        !!
        !!     ! Add the data to the plot
        !!     call plt%push(d1)
        !!
        !!     ! Let GNUPLOT draw the plot
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_plot_3d_1.png
        procedure, public :: define_data => pd3d_set_data_1
        !> @brief Gets the stored X data array.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_x_data(class(plot_data_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @return A copy of the stored data array.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     real(real64), allocatable, dimension(:) :: x
        !!
        !!     ! Get the data array
        !!     x = pd%get_x_data()
        !! end program
        !! @endcode
        procedure, public :: get_x_data => pd3d_get_x_array
        !> @brief Gets the stored Y data array.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_y_data(class(plot_data_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @return A copy of the stored data array.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     real(real64), allocatable, dimension(:) :: y
        !!
        !!     ! Get the data array
        !!     y = pd%get_y_data()
        !! end program
        !! @endcode
        procedure, public :: get_y_data => pd3d_get_y_array
        !> @brief Gets the stored Z data array.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_z_data(class(plot_data_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @return A copy of the stored data array.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_data_2d) :: pd
        !!     real(real64), allocatable, dimension(:) :: z
        !!
        !!     ! Get the data array
        !!     z = pd%get_z_data()
        !! end program
        !! @endcode
        procedure, public :: get_z_data => pd3d_get_z_array
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function pd3d_get_data_count(this) result(x)
            class(plot_data_3d), intent(in) :: this
            integer(int32) :: x
        end function

        pure module function pd3d_get_x_data(this, index) result(x)
            class(plot_data_3d), intent(in) :: this
            integer(int32), intent(in) :: index
            real(real64) :: x
        end function

        module subroutine pd3d_set_x_data(this, index, x)
            class(plot_data_3d), intent(inout) :: this
            integer(int32), intent(in) :: index
            real(real64), intent(in) :: x
        end subroutine

        pure module function pd3d_get_y_data(this, index) result(x)
            class(plot_data_3d), intent(in) :: this
            integer(int32), intent(in) :: index
            real(real64) :: x
        end function

        module subroutine pd3d_set_y_data(this, index, x)
            class(plot_data_3d), intent(inout) :: this
            integer(int32), intent(in) :: index
            real(real64), intent(in) :: x
        end subroutine

        pure module function pd3d_get_z_data(this, index) result(x)
            class(plot_data_3d), intent(in) :: this
            integer(int32), intent(in) :: index
            real(real64) :: x
        end function

        module subroutine pd3d_set_z_data(this, index, x)
            class(plot_data_3d), intent(inout) :: this
            integer(int32), intent(in) :: index
            real(real64), intent(in) :: x
        end subroutine

        module function pd3d_get_axes_cmd(this) result(x)
            class(plot_data_3d), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function pd3d_get_data_cmd(this) result(x)
            class(plot_data_3d), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine pd3d_set_data_1(this, x, y, z, err)
            class(plot_data_3d), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x, y, z
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function pd3d_get_x_array(this) result(x)
            class(plot_data_3d), intent(in) :: this
            real(real64), allocatable, dimension(:) :: x
        end function

        module function pd3d_get_y_array(this) result(x)
            class(plot_data_3d), intent(in) :: this
            real(real64), allocatable, dimension(:) :: x
        end function

        module function pd3d_get_z_array(this) result(x)
            class(plot_data_3d), intent(in) :: this
            real(real64), allocatable, dimension(:) :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_SURFACE_PLOT_DATA.F90
! ------------------------------------------------------------------------------
    !> @brief Provides a three-dimensional surface plot data set.
    type, extends(plot_data) :: surface_plot_data
    private
        !> Stores the x-coordinate data
        real(real64), allocatable, dimension(:,:) :: m_x
        !> Stores the y-coordinate data
        real(real64), allocatable, dimension(:,:) :: m_y
        !> Stores the z-coordinate data
        real(real64), allocatable, dimension(:,:) :: m_z
        !> Set to true to display a wireframe of the surface; else, just a
        !! smooth surface will be drawn
        logical :: m_wireframe = .false.
    contains
        !> @brief Gets the size of the stored data set.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_size(class(surface_plot_data) this, integer(int32) dim)
        !! @endcode
        !!
        !! @param[in] this The suface_plot_data object.
        !! @param[in] dim The dimension of interest.  Notice, data is stored as a
        !!  2D matrix (i.e. only 1 and 2 are valid inputs).
        !! @return The size of the requested dimension.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(surface_plot_data) :: pd
        !!     integer(int32) :: nrows, ncols
        !!
        !!     ! Get the number of rows in the data matrices
        !!     nrows = pd%get_size(1)
        !!
        !!     ! Get the number of columns in the data matrices
        !!     ncols = pd%get_size(2)
        !! end program
        !! @endcode
        procedure, public :: get_size => surfd_get_size
        !> @brief Gets the requested X data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_x(class(surface_plot_data) this, integer(int32) i, integer(int32) j)
        !! @endcode
        !!
        !! @param[in] this The surface_plot_data object.
        !! @param[in] i The row index.
        !! @param[in] j The column index.
        !! @return The value.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(surface_plot_data) :: pd
        !!     real(real64) :: val
        !!
        !!     ! Get a value from the 10th row and 15th column of the X data
        !!     val = pd%get_x(10, 15)
        !! end program
        !! @endcode
        procedure, public :: get_x => surfd_get_x
        !> @brief Sets the requested X data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_x(class(surface_plot_data) this, integer(int32) i, integer(int32) j, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot_data object.
        !! @param[in] i The row index.
        !! @param[in] j The column index.
        !! @param[in] x The value.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(surface_plot_data) :: pd
        !!     real(real64) :: val
        !!
        !!     ! Set a value into the 10th row and 15th column of the X data
        !!     call pd%set_x(10, 15, 5.0d0)
        !! end program
        !! @endcode
        procedure, public :: set_x => surfd_set_x
        !> @brief Gets the requested Y data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_y(class(surface_plot_data) this, integer(int32) i, integer(int32) j)
        !! @endcode
        !!
        !! @param[in] this The surface_plot_data object.
        !! @param[in] i The row index.
        !! @param[in] j The column index.
        !! @return The value.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(surface_plot_data) :: pd
        !!     real(real64) :: val
        !!
        !!     ! Get a value from the 10th row and 15th column of the Y data
        !!     val = pd%get_y(10, 15)
        !! end program
        !! @endcode
        procedure, public :: get_y => surfd_get_y
        !> @brief Sets the requested Y data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_y(class(surface_plot_data) this, integer(int32) i, integer(int32) j, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot_data object.
        !! @param[in] i The row index.
        !! @param[in] j The column index.
        !! @param[in] x The value.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(surface_plot_data) :: pd
        !!     real(real64) :: val
        !!
        !!     ! Set a value into the 10th row and 15th column of the Y data
        !!     call pd%set_y(10, 15, 5.0d0)
        !! end program
        !! @endcode
        procedure, public :: set_y => surfd_set_y
        !> @brief Gets the requested Z data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_z(class(surface_plot_data) this, integer(int32) i, integer(int32) j)
        !! @endcode
        !!
        !! @param[in] this The surface_plot_data object.
        !! @param[in] i The row index.
        !! @param[in] j The column index.
        !! @return The value.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(surface_plot_data) :: pd
        !!     real(real64) :: val
        !!
        !!     ! Get a value from the 10th row and 15th column of the Z data
        !!     val = pd%get_z(10, 15)
        !! end program
        !! @endcode
        procedure, public :: get_z => surfd_get_z
        !> @brief Sets the requested Z data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_z(class(surface_plot_data) this, integer(int32) i, integer(int32) j, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot_data object.
        !! @param[in] i The row index.
        !! @param[in] j The column index.
        !! @param[in] x The value.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(surface_plot_data) :: pd
        !!     real(real64) :: val
        !!
        !!     ! Set a value into the 10th row and 15th column of the Z data
        !!     call pd%set_z(10, 15, 5.0d0)
        !! end program
        !! @endcode
        procedure, public :: set_z => surfd_set_z
        !> @brief Gets a value determining if a wireframe mesh should be
        !! displayed.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_wireframe(class(surface_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot_data object.
        !! @return Returns true if a wireframe mesh should be displayed; else, false
        !!  to display a solid surface.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(surface_plot_data) :: pd
        !!     logical :: check
        !!
        !!     ! Check to see if the data set is to be plotted in wireframe
        !!     check = pd%get_use_wireframe()
        !! end program
        !! @endcode
        procedure, public :: get_use_wireframe => surfd_get_wireframe
        !> @brief Sets a value determining if a wireframe mesh should be
        !! displayed.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_wireframe(class(surface_plot_data) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot_data object.
        !! @param[in] x Set to true if a wireframe mesh should be displayed; else,
        !!  false to display a solid surface.
        !!
        !! @par Example
        !! This example builds a wireframe surface plot.
        !! @code{.f90}
        !! program example
        !!     use, intrinsic :: iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     ! Parameters
        !!     integer(int32), parameter :: m = 50
        !!     integer(int32), parameter :: n = 50
        !!     real(real64), parameter :: xMax = 5.0d0
        !!     real(real64), parameter :: xMin = -5.0d0
        !!     real(real64), parameter :: yMax = 5.0d0
        !!     real(real64), parameter :: yMin = -5.0d0
        !!
        !!     ! Local Variables
        !!     real(real64), dimension(n) :: xdata
        !!     real(real64), dimension(m) :: ydata
        !!     real(real64), dimension(:,:), pointer :: x, y
        !!     real(real64), dimension(m, n, 2), target :: xy
        !!     real(real64), dimension(m, n) :: z
        !!     type(surface_plot) :: plt
        !!     type(surface_plot_data) :: d1
        !!     class(plot_axis), pointer :: xAxis, yAxis, zAxis
        !!
        !!     ! Define the data
        !!     xdata = linspace(xMin, xMax, n)
        !!     ydata = linspace(yMin, yMax, m)
        !!     xy = meshgrid(xdata, ydata)
        !!     x => xy(:,:,1)
        !!     y => xy(:,:,2)
        !!
        !!     ! Define the function to plot
        !!     z = sin(sqrt(x**2 + y**2))
        !!
        !!     ! Create the plot
        !!     call plt%initialize()
        !!     call d1%set_use_wireframe(.true.)
        !!
        !!     ! Define titles
        !!     call plt%set_title("Example Plot")
        !!
        !!     xAxis => plt%get_x_axis()
        !!     call xAxis%set_title("X Axis")
        !!
        !!     yAxis => plt%get_y_axis()
        !!     call yAxis%set_title("Y Axis")
        !!
        !!     zAxis => plt%get_z_axis()
        !!     call zAxis%set_title("Z Axis")
        !!
        !!     ! Define the data set
        !!     call d1%define_data(x, y, z)
        !!     call d1%set_name("sin(sqrt(x**2 + y**2))")
        !!     call plt%push(d1)
        !!
        !!     ! Let GNUPLOT draw the plot
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_wireframe_surface_plot.png
        procedure, public :: set_use_wireframe => surfd_set_wireframe
        !> @brief Gets the GNUPLOT command string to represent this
        !! surface_plot_data object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(surface_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot_data object.
        !! @return The command string.
        procedure, public :: get_command_string => surfd_get_cmd
        !> @brief Gets the GNUPLOT command string containing the actual data
        !! to plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_data_string(class(surface_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot_data object.
        !! @return The GNUPLOT command string.
        procedure, public :: get_data_string => surfd_get_data_cmd
        !> @brief Defines the data set.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_data(class(surface_plot_data) this, real(real64) x(:,:), real(real64) y(:,:), real(real64) z(:,:))
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot_data object.
        !! @param[in] x An M-by-N matrix containing the x-coordinate data.
        !! @param[in] y An M-by-N matrix containing the y-coordinate data.
        !! @param[in] z An M-by-N matrix containing the z-coordinate data.
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if @p x, @p y, and @p z are
        !!      not the same size.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     ! Parameters
        !!     integer(int32), parameter :: m = 50
        !!     integer(int32), parameter :: n = 50
        !!
        !!     ! Local Variables
        !!     real(real64), dimension(m, n, 2), target :: xy
        !!     real(real64), pointer, dimension(:,:) :: x, y
        !!     real(real64), dimension(m, n) :: z
        !!     type(surface_plot) :: plt
        !!     type(surface_plot_data) :: d1
        !!     class(plot_axis), pointer :: xAxis, yAxis, zAxis
        !!     type(rainbow_colormap) :: map
        !!
        !!     ! Define the data
        !!     xy = meshgrid(linspace(-5.0d0, 5.0d0, n), linspace(-5.0d0, 5.0d0, m))
        !!     x => xy(:,:,1)
        !!     y => xy(:,:,2)
        !!
        !!     ! Initialize the plot
        !!     call plt%initialize()
        !!     call plt%set_colormap(map)
        !!
        !!     ! Set the orientation of the plot
        !!     call plt%set_elevation(20.0d0)
        !!     call plt%set_azimuth(30.0d0)
        !!
        !!     ! Define titles
        !!     call plt%set_title("Example Plot")
        !!
        !!     xAxis => plt%get_x_axis()
        !!     call xAxis%set_title("X Axis")
        !!
        !!     yAxis => plt%get_y_axis()
        !!     call yAxis%set_title("Y Axis")
        !!
        !!     zAxis => plt%get_z_axis()
        !!     call zAxis%set_title("Z Axis")
        !!
        !!     ! Define the function to plot
        !!     z = sqrt(x**2 + y**2) * sin(x**2 + y**2)
        !!     call d1%define_data(x, y, z)
        !!     call plt%push(d1)
        !!
        !!     ! Draw the plot
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_surface_plot_2.png
        procedure, public :: define_data => surfd_set_data_1
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function surfd_get_size(this, dim) result(x)
            class(surface_plot_data), intent(in) :: this
            integer(int32), intent(in) :: dim
            integer(int32) :: x
        end function

        pure module function surfd_get_x(this, i, j) result(x)
            class(surface_plot_data), intent(in) :: this
            integer(int32), intent(in) :: i, j
            real(real64) :: x
        end function

        module subroutine surfd_set_x(this, i, j, x)
            class(surface_plot_data), intent(inout) :: this
            integer(int32), intent(in) :: i, j
            real(real64), intent(in) :: x
        end subroutine

        pure module function surfd_get_y(this, i, j) result(x)
            class(surface_plot_data), intent(in) :: this
            integer(int32), intent(in) :: i, j
            real(real64) :: x
        end function

        module subroutine surfd_set_y(this, i, j, x)
            class(surface_plot_data), intent(inout) :: this
            integer(int32), intent(in) :: i, j
            real(real64), intent(in) :: x
        end subroutine

        pure module function surfd_get_z(this, i, j) result(x)
            class(surface_plot_data), intent(in) :: this
            integer(int32), intent(in) :: i, j
            real(real64) :: x
        end function

        module subroutine surfd_set_z(this, i, j, x)
            class(surface_plot_data), intent(inout) :: this
            integer(int32), intent(in) :: i, j
            real(real64), intent(in) :: x
        end subroutine

        pure module function surfd_get_wireframe(this) result(x)
            class(surface_plot_data), intent(in) :: this
            logical :: x
        end function

        module subroutine surfd_set_wireframe(this, x)
            class(surface_plot_data), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module function surfd_get_cmd(this) result(x)
            class(surface_plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function surfd_get_data_cmd(this) result(x)
            class(surface_plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine surfd_set_data_1(this, x, y, z, err)
            class(surface_plot_data), intent(inout) :: this
            real(real64), intent(in), dimension(:,:) :: x, y, z
            class(errors), intent(inout), optional, target :: err
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_PLOT_2D.F90
! ------------------------------------------------------------------------------
    !> @brief A plot object defining a 2D plot.
    !!
    !! @par Example
    !! The following example illustrates a 2D plot, and several examples of how
    !! to modify various plot settings.
    !! @code{.f90}
    !! program example
    !!     use, intrinsic :: iso_fortran_env
    !!     use fplot_core
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: n = 1000
    !!
    !!     ! Local Variables
    !!     real(real64), dimension(n) :: x, y1, y2
    !!     type(plot_2d) :: plt
    !!     type(plot_data_2d) :: d1, d2
    !!     class(plot_axis), pointer :: xAxis, yAxis
    !!     type(legend), pointer :: leg
    !!
    !!     ! Initialize the plot object
    !!     call plt%initialize()
    !!
    !!     ! Define titles
    !!     call plt%set_title("2D Example Plot 1")
    !!     call plt%set_font_size(14)
    !!
    !!     xAxis => plt%get_x_axis()
    !!     call xAxis%set_title("X Axis")
    !!
    !!     yAxis => plt%get_y_axis()
    !!     call yAxis%set_title("Y Axis")
    !!
    !!     ! Establish legend properties
    !!     leg => plt%get_legend()
    !!     call leg%set_draw_inside_axes(.false.)
    !!     call leg%set_horizontal_position(LEGEND_CENTER)
    !!     call leg%set_vertical_position(LEGEND_BOTTOM)
    !!     call leg%set_draw_border(.false.)
    !!
    !!     ! Define the data, and then add it to the plot
    !!     x = linspace(0.0d0, 10.0d0, n)
    !!     y1 = sin(5.0d0 * x)
    !!     y2 = 2.0d0 * cos(2.0d0 * x)
    !!
    !!     call d1%define_data(x, y1)
    !!     call d2%define_data(x, y2)
    !!
    !!     ! Define properties for each data set
    !!     call d1%set_name("Data Set 1")
    !!     call d1%set_line_color(CLR_BLUE)
    !!     call d1%set_draw_markers(.true.)
    !!     call d1%set_marker_frequency(10)
    !!     call d1%set_marker_style(MARKER_EMPTY_CIRCLE)
    !!     call d1%set_marker_scaling(2.0)
    !!
    !!     call d2%set_name("Data Set 2")
    !!     call d2%set_line_color(CLR_GREEN)
    !!     call d2%set_line_style(LINE_DASHED)
    !!     call d2%set_line_width(2.0)
    !!
    !!     ! Add the data sets to the plot
    !!     call plt%push(d1)
    !!     call plt%push(d2)
    !!
    !!     ! Let GNUPLOT draw the plot
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! @image html example_plot_2d_3.png
    type, extends(plot) :: plot_2d
    private
        !> The x-axis.
        type(x_axis), pointer :: m_xAxis => null()
        !> The y-axis.
        type(y_axis), pointer :: m_yAxis => null()
        !> The secondary y-axis.
        type(y2_axis), pointer :: m_y2Axis => null()
        !> Display the secondary y axis?
        logical :: m_useY2 = .false.
    contains
        !> @brief Cleans up resources held by the plot_2d object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine p2d_clean_up(type(plot_2d) this)
        !! @endcode
        !!
        !! @param[in,out] this The plot_2d object.
        final :: p2d_clean_up
        !> @brief Initializes the plot_2d object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine initialize(class(plot_2d) this, optional integer(int32) term, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The plot_2d object.
        !! @param[in] term An optional input that is used to define the terminal.
        !!  The default terminal is a WXT terminal.  The acceptable inputs are:
        !!  - GNUPLOT_TERMINAL_PNG
        !!  - GNUPLOT_TERMINAL_QT
        !!  - GNUPLOT_TERMINAL_WIN32
        !!  - GNUPLOT_TERMINAL_WXT
        !!  - GNUPLOT_TERMINAL_LATEX
        !! @param[in] fname A filename to pass to the terminal in the event the
        !!  terminal is a file type (e.g. GNUPLOT_TERMINAL_PNG).
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!
        !! @par Example
        !! See png_terminal for an example.
        procedure, public :: initialize => p2d_init
        !> @brief Gets the GNUPLOT command string to represent this plot_2d
        !! object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(plot_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_2d object.
        !! @return The command string.
        procedure, public :: get_command_string => p2d_get_cmd
        !> @brief Gets the x-axis object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_axis) function, pointer get_x_axis(class(plot_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_2d object.
        !! @return A pointer to the x-axis object.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     class(plot_axis) :: axis
        !!
        !!     ! Get a pointer to the axis object
        !!     axis => plt%get_x_axis()
        !! end program
        !! @endcode
        procedure, public :: get_x_axis => p2d_get_x_axis
        !> @brief Gets the y-axis object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_axis) function, pointer get_y_axis(class(plot_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_2d object.
        !! @return A pointer to the y-axis object.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     class(plot_axis) :: axis
        !!
        !!     ! Get a pointer to the axis object
        !!     axis => plt%get_y_axis()
        !! end program
        !! @endcode
        procedure, public :: get_y_axis => p2d_get_y_axis
        !> @brief Gets the secondary y-axis object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_axis) function, pointer get_y2_axis(class(plot_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_2d object.
        !! @return A pointer to the secondary y-axis object.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     class(plot_axis) :: axis
        !!
        !!     ! Get a pointer to the axis object
        !!     axis => plt%get_y2_axis()
        !! end program
        !! @endcode
        procedure, public :: get_y2_axis => p2d_get_y2_axis
        !> @brief Gets a flag determining if the secondary y-axis should be
        !! displayed.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_use_y2_axis(class(plot_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_2d object.
        !! @return Returns true if the axis should be displayed; else, false.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_2d) :: plt
        !!     logical :: check
        !!
        !!     ! Determine if a secondary y axis is in use
        !!     check = plt%get_use_y2_axis()
        !! end program
        !! @endcode
        procedure, public :: get_use_y2_axis => p2d_get_use_y2
        !> @brief Sets a flag determining if the secondary y-axis should be
        !! displayed.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_use_y2_axis(class(plot_2d) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_2d object.
        !! @param[in] x Set to true if the axis should be displayed; else, false.
        !!
        !! @par Example
        !! This example illustrates the use of a secondary y axis.
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     ! Local Variables
        !!     integer(int32), parameter :: npts = 1000
        !!     real(real64), dimension(npts) :: x, y1, y2
        !!     type(plot_2d) :: plt
        !!     type(plot_data_2d) :: ds1, ds2
        !!     class(plot_axis), pointer :: xAxis, yAxis, y2Axis
        !!
        !!     ! Build a data set
        !!     x = linspace(0.0d0, 10.0d0, npts)
        !!     y1 = exp(-0.5d0 * x) * abs(sin(x))
        !!     y2 = cos(0.5d0 * x) * sin(10.0d0 * x)
        !!
        !!     call ds1%define_data(x, y1)
        !!     call ds1%set_name("f(x) = exp(-x / 2) * |sin(x)|")
        !!
        !!     call ds2%define_data(x, y2)
        !!     call ds2%set_name("f(x) = cos(x / 2) * sin(10 x)")
        !!
        !!     ! Make the ds2 line green and dashed
        !!     call ds2%set_line_color(CLR_GREEN)
        !!     call ds2%set_line_style(LINE_DASHED)
        !!
        !!     ! Draw ds2 against the secondary y axis
        !!     call ds2%set_draw_against_y2(.true.)
        !!
        !!     ! Ensure the plot knows it needs a secondary y axis
        !!     call plt%set_use_y2_axis(.true.)
        !!
        !!     ! Set up the plot
        !!     call plt%initialize()
        !!     call plt%set_title("Example Plot")
        !!
        !!     xAxis => plt%get_x_axis()
        !!     call xAxis%set_title("X Axis")
        !!
        !!     yAxis => plt%get_y_axis()
        !!     call yAxis%set_title("Y Axis")
        !!
        !!     y2Axis => plt%get_y2_axis()
        !!     call y2Axis%set_title("Secondary Y Axis")
        !!
        !!     ! Add the data to the plot
        !!     call plt%push(ds1)
        !!     call plt%push(ds2)
        !!
        !!     ! Draw
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_plot_y2_axis_1.png
        procedure, public :: set_use_y2_axis => p2d_set_use_y2
    end type

! ------------------------------------------------------------------------------
    interface
        module subroutine p2d_clean_up(this)
            type(plot_2d), intent(inout) :: this
        end subroutine

        module subroutine p2d_init(this, term, fname, err)
            class(plot_2d), intent(inout) :: this
            integer(int32), intent(in), optional :: term
            character(len = *), intent(in), optional :: fname
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function p2d_get_cmd(this) result(x)
            class(plot_2d), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function p2d_get_x_axis(this) result(ptr)
            class(plot_2d), intent(in) :: this
            class(plot_axis), pointer :: ptr
        end function

        module function p2d_get_y_axis(this) result(ptr)
            class(plot_2d), intent(in) :: this
            class(plot_axis), pointer :: ptr
        end function

        module function p2d_get_y2_axis(this) result(ptr)
            class(plot_2d), intent(in) :: this
            class(plot_axis), pointer :: ptr
        end function

        pure module function p2d_get_use_y2(this) result(x)
            class(plot_2d), intent(in) :: this
            logical :: x
        end function

        module subroutine p2d_set_use_y2(this, x)
            class(plot_2d), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_PLOT_3D.F90
! ------------------------------------------------------------------------------
    !> @brief A plot object defining a 3D plot.
    !!
    !! @par Example
    !! The following example adds data to draw a helix to a 3D plot.
    !! @code{.f90}
    !! program example
    !!     use, intrinsic :: iso_fortran_env
    !!     use fplot_core
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: n = 1000
    !!
    !!     ! Local Variables
    !!     real(real64), dimension(n) :: t, x, y, z
    !!     type(plot_3d) :: plt
    !!     type(plot_data_3d) :: d1
    !!     class(plot_axis), pointer :: xAxis, yAxis, zAxis
    !!     type(legend), pointer :: leg
    !!
    !!     ! Initialize the plot object
    !!     call plt%initialize()
    !!     leg => plt%get_legend()
    !!     call leg%set_is_visible(.false.)
    !!
    !!     ! Define titles
    !!     call plt%set_title("Example Plot")
    !!
    !!     xAxis => plt%get_x_axis()
    !!     call xAxis%set_title("X Axis")
    !!
    !!     yAxis => plt%get_y_axis()
    !!     call yAxis%set_title("Y Axis")
    !!
    !!     zAxis => plt%get_z_axis()
    !!     call zAxis%set_title("Z Axis")
    !!
    !!     ! Define the data
    !!     t = linspace(0.0d0, 10.0d0, n)
    !!     x = cos(5.0d0 * t)
    !!     y = sin(5.0d0 * t)
    !!     z = 2.0d0 * t
    !!
    !!     call d1%define_data(x, y, z)
    !!
    !!     ! Set up the data set
    !!     call d1%set_line_color(CLR_BLUE)
    !!     call d1%set_line_width(2.0)
    !!
    !!     ! Add the data to the plot
    !!     call plt%push(d1)
    !!
    !!     ! Let GNUPLOT draw the plot
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! @image html example_plot_3d_1.png
    type, extends(plot) :: plot_3d
    private
        !> The x-axis.
        type(x_axis), pointer :: m_xAxis => null()
        !> The y-axis.
        type(y_axis), pointer :: m_yAxis => null()
        !> The z-axis.
        type(z_axis), pointer :: m_zAxis => null()
        !> The elevation angle.
        real(real64) :: m_elevation = 60.0d0
        !> The azimuth.
        real(real64) :: m_azimuth = 30.0d0
        !> Z-axis intersect X-Y plane?
        logical :: m_zIntersect = .true.
    contains
        !> @brief Cleans up resources held by the plot_3d object.
        !!
        !! @param[in,out] this The plot_3d object.
        final :: p3d_clean_up
        !> @brief Initializes the plot_3d object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine initialize(class(plot_3d) this, optional integer(int32) term, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @param[in] term An optional input that is used to define the terminal.
        !!  The default terminal is a WXT terminal.  The acceptable inputs are:
        !!  - GNUPLOT_TERMINAL_PNG
        !!  - GNUPLOT_TERMINAL_QT
        !!  - GNUPLOT_TERMINAL_WIN32
        !!  - GNUPLOT_TERMINAL_WXT
        !!  - GNUPLOT_TERMINAL_LATEX
        !! @param[in] fname A filename to pass to the terminal in the event the
        !!  terminal is a file type (e.g. GNUPLOT_TERMINAL_PNG).
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        procedure, public :: initialize => p3d_init
        !> @brief Gets the GNUPLOT command string to represent this plot_3d
        !! object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(plot_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @return The command string.
        procedure, public :: get_command_string => p3d_get_cmd
        !> @brief Gets the x-axis object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_axis) function, pointer get_x_axis(class(plot_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @return A pointer to the x-axis object.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_3d) :: plt
        !!     class(plot_axis) :: axis
        !!
        !!     ! Get a pointer to the axis object
        !!     axis => plt%get_x_axis()
        !! end program
        !! @endcode
        procedure, public :: get_x_axis => p3d_get_x_axis
        !> @brief Gets the y-axis object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_axis) function, pointer get_y_axis(class(plot_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @return A pointer to the y-axis object.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_3d) :: plt
        !!     class(plot_axis) :: axis
        !!
        !!     ! Get a pointer to the axis object
        !!     axis => plt%get_y_axis()
        !! end program
        !! @endcode
        procedure, public :: get_y_axis => p3d_get_y_axis
        !> @brief Gets the z-axis object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_axis) function, pointer get_z_axis(class(plot_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @return A pointer to the z-axis object.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_3d) :: plt
        !!     class(plot_axis) :: axis
        !!
        !!     ! Get a pointer to the axis object
        !!     axis => plt%get_z_axis()
        !! end program
        !! @endcode
        procedure, public :: get_z_axis => p3d_get_z_axis
        !> @brief Gets the plot elevation angle.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64) function get_elevation(class(plot_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @return The elevation angle, in degrees.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_3d) :: plt
        !!     real(real64) :: val
        !!
        !!     ! Get the elevation angle of the plot
        !!     val = plt%get_elevation()
        !! end program
        !! @endcode
        procedure, public :: get_elevation => p3d_get_elevation
        !> @brief Sets the plot elevation angle.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_elevation(class(plot_3d) this, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_3d object.
        !! @param[in] x The elevation angle, in degrees.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_3d) :: plt
        !!
        !!     ! Set the elevation angle of the plot
        !!     call plt%set_elevation(15.0d0)
        !! end program
        !! @endcode
        procedure, public :: set_elevation => p3d_set_elevation
        !> @brief Gets the plot azimuth angle.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64) function get_azimuth(class(plot_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @return The azimuth angle, in degrees.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_3d) :: plt
        !!     real(real64) :: val
        !!
        !!     ! Get the azimuth angle of the plot
        !!     val = plt%get_azimuth()
        !! end program
        !! @endcode
        procedure, public :: get_azimuth => p3d_get_azimuth
        !> @brief Sets the plot azimuth angle.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_azimuth(class(plot_3d) this, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_3d object.
        !! @param[in] x The azimuth angle, in degrees.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(plot_3d) :: plt
        !!
        !!     ! Set the azimuth angle of the plot
        !!     call plt%set_azimuth(15.0d0)
        !! end program
        !! @endcode
        procedure, public :: set_azimuth => p3d_set_azimuth
        !> @brief Gets a value determining if the z-axis should intersect the
        !! x-y plane.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_z_intersect_xy(class(plot_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @return Returns true if the z-axis should intersect the x-y plane; else,
        !!  false to allow the z-axis to float.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_3d) :: plt
        !!     logical :: check
        !!
        !!     ! Determine if the z axis is drawn to intersect the x-y plane
        !!     check = plt%get_z_intersect_xy()
        !! end program
        !! @endcode
        procedure, public :: get_z_intersect_xy => p3d_get_z_axis_intersect
        !> @brief Sets a value determining if the z-axis should intersect the
        !! x-y plane.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_z_intersect_xy(class(plot_3d) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_3d object.
        !! @param[in] x Set to true if the z-axis should intersect the x-y plane;
        !!  else, false to allow the z-axis to float.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use, intrinsic :: iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     ! Parameters
        !!     integer(int32), parameter :: n = 1000
        !!
        !!     ! Local Variables
        !!     real(real64), dimension(n) :: t, x, y, z
        !!     type(plot_3d) :: plt
        !!     type(plot_data_3d) :: d1
        !!     class(plot_axis), pointer :: xAxis, yAxis, zAxis
        !!     type(legend), pointer :: leg
        !!
        !!     ! Initialize the plot object
        !!     call plt%initialize()
        !!     leg => plt%get_legend()
        !!     call leg%set_is_visible(.false.)
        !!
        !!     ! Set the Z-axis to not intersect the X-Y plane
        !!     call plt%set_z_intersect_xy(.false.)
        !!
        !!     ! Define titles
        !!     call plt%set_title("Example Plot")
        !!
        !!     xAxis => plt%get_x_axis()
        !!     call xAxis%set_title("X Axis")
        !!
        !!     yAxis => plt%get_y_axis()
        !!     call yAxis%set_title("Y Axis")
        !!
        !!     zAxis => plt%get_z_axis()
        !!     call zAxis%set_title("Z Axis")
        !!
        !!     ! Define the data
        !!     t = linspace(0.0d0, 10.0d0, n)
        !!     x = cos(5.0d0 * t)
        !!     y = sin(5.0d0 * t)
        !!     z = 2.0d0 * t
        !!
        !!     call d1%define_data(x, y, z)
        !!
        !!     ! Set up the data set
        !!     call d1%set_line_color(CLR_BLUE)
        !!     call d1%set_line_width(2.0)
        !!
        !!     ! Add the data to the plot
        !!     call plt%push(d1)
        !!
        !!     ! Let GNUPLOT draw the plot
        !!     call plt%draw()
        !! end program
        !! @endcode
        !!
        !! @par
        !! The above code results in the following plot.
        !! @image html example_plot_3d_offset_1.png
        !!
        !! @par
        !! Compare to the default (allowing the z-axis to intersect the x-y
        !! plane).
        !! @image html example_plot_3d_1.png
        procedure, public :: set_z_intersect_xy => p3d_set_z_axis_intersect
    end type

! ------------------------------------------------------------------------------
    interface
        module subroutine p3d_clean_up(this)
            type(plot_3d), intent(inout) :: this
        end subroutine

        module subroutine p3d_init(this, term, fname, err)
            class(plot_3d), intent(inout) :: this
            integer(int32), intent(in), optional :: term
            character(len = *), intent(in), optional :: fname
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function p3d_get_cmd(this) result(x)
            class(plot_3d), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function p3d_get_x_axis(this) result(ptr)
            class(plot_3d), intent(in) :: this
            class(plot_axis), pointer :: ptr
        end function

        module function p3d_get_y_axis(this) result(ptr)
            class(plot_3d), intent(in) :: this
            class(plot_axis), pointer :: ptr
        end function

        module function p3d_get_z_axis(this) result(ptr)
            class(plot_3d), intent(in) :: this
            class(plot_axis), pointer :: ptr
        end function

        pure module function p3d_get_elevation(this) result(x)
            class(plot_3d), intent(in) :: this
            real(real64) :: x
        end function

        module subroutine p3d_set_elevation(this, x)
            class(plot_3d), intent(inout) :: this
            real(real64), intent(in) :: x
        end subroutine

        pure module function p3d_get_azimuth(this) result(x)
            class(plot_3d), intent(in) :: this
            real(real64) :: x
        end function

        module subroutine p3d_set_azimuth(this, x)
            class(plot_3d), intent(inout) :: this
            real(real64), intent(in) :: x
        end subroutine

        pure module function p3d_get_z_axis_intersect(this) result(x)
            class(plot_3d), intent(in) :: this
            logical :: x
        end function

        module subroutine p3d_set_z_axis_intersect(this, x)
            class(plot_3d), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_SURFACE_PLOT.F90
! ------------------------------------------------------------------------------
    !> @brief A plot object defining a 3D surface plot.
    !!
    !! @par Example
    !! The following example illustrates a surface plot using a rainbow
    !! colormap.
    !! @code{.f90}
    !! program example
    !!     use, intrinsic :: iso_fortran_env
    !!     use fplot_core
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: m = 50
    !!     integer(int32), parameter :: n = 50
    !!     real(real64), parameter :: xMax = 5.0d0
    !!     real(real64), parameter :: xMin = -5.0d0
    !!     real(real64), parameter :: yMax = 5.0d0
    !!     real(real64), parameter :: yMin = -5.0d0
    !!
    !!     ! Local Variables
    !!     real(real64), dimension(n) :: xdata
    !!     real(real64), dimension(m) :: ydata
    !!     real(real64), dimension(:,:), pointer :: x, y
    !!     real(real64), dimension(m, n, 2), target :: xy
    !!     real(real64), dimension(m, n) :: z
    !!     type(surface_plot) :: plt
    !!     type(surface_plot_data) :: d1
    !!     class(plot_axis), pointer :: xAxis, yAxis, zAxis
    !!
    !!     ! Define the data
    !!     xdata = linspace(xMin, xMax, n)
    !!     ydata = linspace(yMin, yMax, m)
    !!     xy = meshgrid(xdata, ydata)
    !!     x => xy(:,:,1)
    !!     y => xy(:,:,2)
    !!
    !!     ! Define the function to plot
    !!     z = sin(sqrt(x**2 + y**2))
    !!
    !!     ! Create the plot
    !!     call plt%initialize()
    !!
    !!     ! Define titles
    !!     call plt%set_title("Surface Example Plot 1")
    !!
    !!     xAxis => plt%get_x_axis()
    !!     call xAxis%set_title("X Axis")
    !!
    !!     yAxis => plt%get_y_axis()
    !!     call yAxis%set_title("Y Axis")
    !!
    !!     zAxis => plt%get_z_axis()
    !!     call zAxis%set_title("Z Axis")
    !!
    !!     ! Define the data set
    !!     call d1%define_data(x, y, z)
    !!     call d1%set_name("sin(sqrt(x**2 + y**2))")
    !!     call plt%push(d1)
    !!
    !!     ! Let GNUPLOT draw the plot
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! @image html example_surface_plot_default.png
    type, extends(plot_3d) :: surface_plot
    private
        !> Show hidden lines
        logical :: m_showHidden = .false.
        !> The colormap
        class(colormap), pointer :: m_colormap
        !> Smooth the surface?
        logical :: m_smooth = .true.
        !> Show a contour plot as well as the surface plot?
        logical :: m_contour = .false.
        !> Show the colorbar?
        logical :: m_showColorbar = .true.
        !> Use lighting?
        logical :: m_useLighting = .false.
        !> Lighting intensity (0 - 1) - default is 0.5
        real(real32) :: m_lightIntensity = 0.5
        !> Specular highlight intensity (0 - 1)
        real(real32) :: m_specular = 0.5
        !> Defines the translucency value.  Must exist on (0, 1].
        real(real32) :: m_transparency = 1.0
    contains
        !> @brief Cleans up resources held by the surface_plot object.
        !!
        !! @param[in,out] this The surface_plot object.
        final :: surf_clean_up
        !> @brief Initializes the surface_plot object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine initialize(class(surface_plot) this, optional integer(int32) term, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @param[in] term An optional input that is used to define the terminal.
        !!  The default terminal is a WXT terminal.  The acceptable inputs are:
        !!  - GNUPLOT_TERMINAL_PNG
        !!  - GNUPLOT_TERMINAL_QT
        !!  - GNUPLOT_TERMINAL_WIN32
        !!  - GNUPLOT_TERMINAL_WXT
        !!  - GNUPLOT_TERMINAL_LATEX
        !! @param[in] fname A filename to pass to the terminal in the event the
        !!  terminal is a file type (e.g. GNUPLOT_TERMINAL_PNG).
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        procedure, public :: initialize => surf_init
        !> @brief Gets a value indicating if hidden lines should be shown.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_show_hidden(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return Returns true if hidden lines should be shown; else, false.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(surface_plot) :: plt
        !!     logical :: check
        !!
        !!     ! Check to see if hidden lines are to be shown
        !!     check = plt%get_show_hidden()
        !! end program
        !! @endcode
        procedure, public :: get_show_hidden => surf_get_show_hidden
        !> @brief Sets a value indicating if hidden lines should be shown.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_show_hidden(class(surface_plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot object.
        !! @param[in] x Set to true if hidden lines should be shown; else, false.
        !!
        !! @par Example
        !! The following example illustrates the use of hidden lines.  The
        !! default wireframe behavior is to hide hidden lines.
        !! @code{.f90}
        !! program example
        !!     use, intrinsic :: iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     ! Parameters
        !!     integer(int32), parameter :: m = 50
        !!     integer(int32), parameter :: n = 50
        !!     real(real64), parameter :: xMax = 5.0d0
        !!     real(real64), parameter :: xMin = -5.0d0
        !!     real(real64), parameter :: yMax = 5.0d0
        !!     real(real64), parameter :: yMin = -5.0d0
        !!
        !!     ! Local Variables
        !!     real(real64), dimension(n) :: xdata
        !!     real(real64), dimension(m) :: ydata
        !!     real(real64), dimension(:,:), pointer :: x, y
        !!     real(real64), dimension(m, n, 2), target :: xy
        !!     real(real64), dimension(m, n) :: z
        !!     type(surface_plot) :: plt
        !!     type(surface_plot_data) :: d1
        !!     class(plot_axis), pointer :: xAxis, yAxis, zAxis
        !!
        !!     ! Define the data
        !!     xdata = linspace(xMin, xMax, n)
        !!     ydata = linspace(yMin, yMax, m)
        !!     xy = meshgrid(xdata, ydata)
        !!     x => xy(:,:,1)
        !!     y => xy(:,:,2)
        !!
        !!     ! Define the function to plot
        !!     z = sin(sqrt(x**2 + y**2))
        !!
        !!     ! Create the plot
        !!     call plt%initialize()
        !!     call plt%set_show_hidden(.true.)
        !!     call d1%set_use_wireframe(.true.)
        !!
        !!     ! Set up lighting
        !!     call plt%set_use_lighting(.true.)
        !!     call plt%set_light_intensity(0.7)
        !!     call plt%set_specular_intensity(0.7)
        !!
        !!     ! Define titles
        !!     call plt%set_title("Example Plot")
        !!
        !!     xAxis => plt%get_x_axis()
        !!     call xAxis%set_title("X Axis")
        !!
        !!     yAxis => plt%get_y_axis()
        !!     call yAxis%set_title("Y Axis")
        !!
        !!     zAxis => plt%get_z_axis()
        !!     call zAxis%set_title("Z Axis")
        !!
        !!     ! Define the data set
        !!     call d1%define_data(x, y, z)
        !!     call d1%set_name("sin(sqrt(x**2 + y**2))")
        !!     call plt%push(d1)
        !!
        !!     ! Let GNUPLOT draw the plot
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html surface_plot_hidden_lines_1.png
        procedure, public :: set_show_hidden => surf_set_show_hidden
        !> @brief Gets the GNUPLOT command string to represent this plot_3d
        !! object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return The command string.
        procedure, public :: get_command_string => surf_get_cmd
        !> @brief Gets a pointer to the colormap object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(colormap) function, pointer get_colormap(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return A pointer to the colormap object.  If no colormap is defined, a
        !!  null pointer is returned.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(surface_plot) :: plt
        !!     class(colormap), pointer :: map
        !!
        !!     ! Get a pointer to the current colormap
        !!     map => plt%get_colormap()
        !! end program
        !! @endcode
        procedure, public :: get_colormap => surf_get_colormap
        !> @brief Sets the colormap object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_colormap(class(surface_plot) this, class(colormap) x, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot object.
        !! @param[in] x The colormap object.  Notice, a copy of this object is
        !!  stored, and the surface_plot object then manages the lifetime of the
        !!  copy.
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(surface_plot) :: plt
        !!     type(rainbow_colormap) :: map
        !!
        !!     ! Set the colormap to a rainbow colormap
        !!     call plt%set_colormap(map)
        !! end program
        !! @endcode
        procedure, public :: set_colormap => surf_set_colormap
        !> @brief Gets a value determining if the plotted surfaces should be
        !! smoothed.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_allow_smoothing(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return Returns true if the surface should be smoothed; else, false.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(surface_plot) :: plt
        !!     logical :: check
        !!
        !!     ! Check to see if the surfaces should be smoothed by interpolation.
        !!     check = plt%get_allow_smoothing()
        !! end program
        !! @endcode
        procedure, public :: get_allow_smoothing => surf_get_smooth
        !> @brief Sets a value determining if the plotted surfaces should be
        !! smoothed.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_allow_smoothing(class(surface_plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot object.
        !! @param[in] x Set to true if the surface should be smoothed; else, false.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(surface_plot) :: plt
        !!
        !!     ! Turn off smoothing (the default is on)
        !!     call plt%set_allow_smoothing(.false.)
        !! end program
        !! @endcode
        procedure, public :: set_allow_smoothing => surf_set_smooth
        !> @brief Gets a value determining if a contour plot should be drawn in
        !! conjunction with the surface plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_show_contours(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return Returns true if the contour plot should be drawn; else, false to
        !!  only draw the surface.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(surface_plot) :: plt
        !!     logical :: check
        !!
        !!     ! Check to see if contour lines are to be drawn
        !!     check = plt%get_show_countours()
        !! end program
        !! @endcode
        procedure, public :: get_show_contours => surf_get_show_contours
        !> @brief Sets a value determining if a contour plot should be drawn in
        !! conjunction with the surface plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_show_contours(class(surface_plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot object.
        !! @param[in] x Set to true if the contour plot should be drawn; else, false
        !!  to only draw the surface.
        !!
        !! @par Example
        !! The following example illustrates the use of a contour and surface
        !! plot together.  Additionally, the z axis is allowed to shift away
        !! from the X-Y plane in order to better show the counter plot.
        !! @code{.f90}
        !! program example
        !!     use, intrinsic :: iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     ! Parameters
        !!     integer(int32), parameter :: m = 50
        !!     integer(int32), parameter :: n = 50
        !!     real(real64), parameter :: xMax = 5.0d0
        !!     real(real64), parameter :: xMin = -5.0d0
        !!     real(real64), parameter :: yMax = 5.0d0
        !!     real(real64), parameter :: yMin = -5.0d0
        !!
        !!     ! Local Variables
        !!     real(real64), dimension(n) :: xdata
        !!     real(real64), dimension(m) :: ydata
        !!     real(real64), dimension(:,:), pointer :: x, y
        !!     real(real64), dimension(m, n, 2), target :: xy
        !!     real(real64), dimension(m, n) :: z
        !!     type(surface_plot) :: plt
        !!     type(surface_plot_data) :: d1
        !!     type(rainbow_colormap) :: map
        !!     class(plot_axis), pointer :: xAxis, yAxis, zAxis
        !!
        !!     ! Define the data
        !!     xdata = linspace(xMin, xMax, n)
        !!     ydata = linspace(yMin, yMax, m)
        !!     xy = meshgrid(xdata, ydata)
        !!     x => xy(:,:,1)
        !!     y => xy(:,:,2)
        !!
        !!     ! Define the function to plot
        !!     z = sin(sqrt(x**2 + y**2))
        !!
        !!     ! Create the plot
        !!     call plt%initialize()
        !!     call plt%set_colormap(map)
        !!     call plt%set_show_contours(.true.)
        !!     call plt%set_z_intersect_xy(.false.)
        !!
        !!     ! Define titles
        !!     call plt%set_title("Example Plot")
        !!
        !!     xAxis => plt%get_x_axis()
        !!     call xAxis%set_title("X Axis")
        !!
        !!     yAxis => plt%get_y_axis()
        !!     call yAxis%set_title("Y Axis")
        !!
        !!     zAxis => plt%get_z_axis()
        !!     call zAxis%set_title("Z Axis")
        !!
        !!     ! Define the data set
        !!     call d1%define_data(x, y, z)
        !!     call d1%set_name("sin(sqrt(x**2 + y**2))")
        !!     call plt%push(d1)
        !!
        !!     ! Let GNUPLOT draw the plot
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_surface_plot_with_contour_1.png
        procedure, public :: set_show_contours => surf_set_show_contours
        !> @brief Gets a value determining if the colorbar should be shown.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_show_colorbar(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return Returns true if the colorbar should be drawn; else, false.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(surface_plot) :: plt
        !!     logical :: check
        !!
        !!     ! Check to see if the colorbar is shown
        !!     check = plt%get_show_colorbar()
        !! end program
        !! @endcode
        procedure, public :: get_show_colorbar => surf_get_show_colorbar
        !> @brief Sets a value determining if the colorbar should be shown.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_show_colorbar(class(surface_plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot object.
        !! @param[in] x Set to true if the colorbar should be drawn; else, false.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(surface_plot) :: plt
        !!     logical :: check
        !!
        !!     ! Hide the colorbar
        !!     call plt%set_show_colorbar(.false.)
        !! end program
        !! @endcode
        procedure, public :: set_show_colorbar => surf_set_show_colorbar
        !> @brief Gets a value indicating if lighting, beyond the ambient
        !! light source, is to be used.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_use_lighting(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return True if lighting should be used; else, false.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(surface_plot) :: plt
        !!     logical :: check
        !!
        !!     ! Determine if lighting is to be used
        !!     check = plt%get_use_lighting()
        !! end program
        !! @endcode
        procedure, public :: get_use_lighting => surf_get_use_lighting
        !> @brief Sets a value indicating if lighting, beyond the ambient
        !! light source, is to be used.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_use_lighting(class(surface_plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot object.
        !! @param[in] x True if lighting should be used; else, false.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use, intrinsic :: iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     ! Parameters
        !!     integer(int32), parameter :: m = 50
        !!     integer(int32), parameter :: n = 50
        !!     real(real64), parameter :: xMax = 5.0d0
        !!     real(real64), parameter :: xMin = -5.0d0
        !!     real(real64), parameter :: yMax = 5.0d0
        !!     real(real64), parameter :: yMin = -5.0d0
        !!
        !!     ! Local Variables
        !!     real(real64), dimension(n) :: xdata
        !!     real(real64), dimension(m) :: ydata
        !!     real(real64), dimension(:,:), pointer :: x, y
        !!     real(real64), dimension(m, n, 2), target :: xy
        !!     real(real64), dimension(m, n) :: z
        !!     type(surface_plot) :: plt
        !!     type(surface_plot_data) :: d1
        !!     type(rainbow_colormap) :: map
        !!     class(plot_axis), pointer :: xAxis, yAxis, zAxis
        !!
        !!     ! Define the data
        !!     xdata = linspace(xMin, xMax, n)
        !!     ydata = linspace(yMin, yMax, m)
        !!     xy = meshgrid(xdata, ydata)
        !!     x => xy(:,:,1)
        !!     y => xy(:,:,2)
        !!
        !!     ! Define the function to plot
        !!     z = sin(sqrt(x**2 + y**2))
        !!
        !!     ! Create the plot
        !!     call plt%initialize()
        !!     call plt%set_colormap(map)
        !!
        !!     ! Set up lighting
        !!     call plt%set_use_lighting(.true.)
        !!     call plt%set_light_intensity(0.7)
        !!     call plt%set_specular_intensity(0.7)
        !!
        !!     ! Define titles
        !!     call plt%set_title("Example Plot")
        !!
        !!     xAxis => plt%get_x_axis()
        !!     call xAxis%set_title("X Axis")
        !!
        !!     yAxis => plt%get_y_axis()
        !!     call yAxis%set_title("Y Axis")
        !!
        !!     zAxis => plt%get_z_axis()
        !!     call zAxis%set_title("Z Axis")
        !!
        !!     ! Define the data set
        !!     call d1%define_data(x, y, z)
        !!     call d1%set_name("sin(sqrt(x**2 + y**2))")
        !!     call plt%push(d1)
        !!
        !!     ! Let GNUPLOT draw the plot
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_surface_plot_lighting_1.png
        procedure, public :: set_use_lighting => surf_set_use_lighting
        !> @brief Gets the ratio of the strength of the light source relative
        !! to the ambient light.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_light_intensity(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return The light intensity ratio.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(surface_plot) :: plt
        !!     real(real32) :: val
        !!
        !!     ! Get the lighting intensity
        !!     val = plt%get_light_intensity()
        !! end program
        !! @endcode
        procedure, public :: get_light_intensity => surf_get_light_intensity
        !> @brief Sets the ratio of the strength of the light source relative
        !! to the ambient light.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_light_intensity(class(surface_plot) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot object.
        !! @param[in] x The light intensity ratio.  The value must exist in the
        !!  set [0, 1]; else, it will be clipped to lie within the range.
        !!
        !! @par Example
        !! See set_use_lighting for example useage.
        procedure, public :: set_light_intensity => surf_set_light_intensity
        !> @brief Gets the ratio of the strength of the specular light source
        !! relative to the ambient light.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_specular_intensity(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return The specular light intensity ratio.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(surface_plot) :: plt
        !!     real(real32) :: val
        !!
        !!     ! Get the lighting intensity
        !!     val = plt%get_specular_intensity()
        !! end program
        !! @endcode
        procedure, public :: get_specular_intensity => surf_get_specular_intensity
        !> @brief Sets the ratio of the strength of the specular light source
        !! relative to the ambient light.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_specular_intensity(class(surface_plot) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot object.
        !! @param[in] x The specular light intensity ratio.  The value must
        !!  exist in the set [0, 1]; else, it will be clipped to lie within the
        !!  range.
        !!
        !! @par Example
        !! See set_use_lighting for example useage.
        procedure, public :: set_specular_intensity => surf_set_specular_intensity
        !> @brief Gets a factor defining the transparency of plotted surfaces.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_transparency(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return A value existing on the set (0 1] defining the level of
        !!  transparency.  A value of 1 indicates a fully opaque surface.
        procedure, public :: get_transparency => surf_get_transparency
        !> @brief Sets a factor defining the transparency of plotted surfaces.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_transparency(class(surface_plot) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot object.
        !! @param[in] x A value existing on the set (0 1] defining the level of
        !!  transparency.  A value of 1 indicates a fully opaque surface.  
        !!  Any values supplied outside of the set are clipped to fit within
        !!  (0 1].
        procedure, public :: set_transparency => surf_set_transparency
    end type

! ------------------------------------------------------------------------------
    interface
        module subroutine surf_clean_up(this)
            type(surface_plot), intent(inout) :: this
        end subroutine

        module subroutine surf_init(this, term, fname, err)
            class(surface_plot), intent(inout) :: this
            integer(int32), intent(in), optional :: term
            character(len = *), intent(in), optional :: fname
            class(errors), intent(inout), optional, target :: err
        end subroutine

        pure module function surf_get_show_hidden(this) result(x)
            class(surface_plot), intent(in) :: this
            logical :: x
        end function

        module subroutine surf_set_show_hidden(this, x)
            class(surface_plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module function surf_get_cmd(this) result(x)
            class(surface_plot), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function surf_get_colormap(this) result(x)
            class(surface_plot), intent(in) :: this
            class(colormap), pointer :: x
        end function

        module subroutine surf_set_colormap(this, x, err)
            class(surface_plot), intent(inout) :: this
            class(colormap), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        pure module function surf_get_smooth(this) result(x)
            class(surface_plot), intent(in) :: this
            logical :: x
        end function

        module subroutine surf_set_smooth(this, x)
            class(surface_plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function surf_get_show_contours(this) result(x)
            class(surface_plot), intent(in) :: this
            logical :: x
        end function

        module subroutine surf_set_show_contours(this, x)
            class(surface_plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function surf_get_show_colorbar(this) result(x)
            class(surface_plot), intent(in) :: this
            logical :: x
        end function

        module subroutine surf_set_show_colorbar(this, x)
            class(surface_plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function surf_get_use_lighting(this) result(x)
            class(surface_plot), intent(in) :: this
            logical :: x
        end function

        module subroutine surf_set_use_lighting(this, x)
            class(surface_plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function surf_get_light_intensity(this) result(x)
            class(surface_plot), intent(in) :: this
            real(real32) :: x
        end function

        module subroutine surf_set_light_intensity(this, x)
            class(surface_plot), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        pure module function surf_get_specular_intensity(this) result(x)
            class(surface_plot), intent(in) :: this
            real(real32) :: x
        end function

        module subroutine surf_set_specular_intensity(this, x)
            class(surface_plot), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        pure module function surf_get_transparency(this) result(x)
            class(surface_plot), intent(in) :: this
            real(real32) :: x
        end function

        module subroutine surf_set_transparency(this, x)
            class(surface_plot), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_AXIS.F90
! ------------------------------------------------------------------------------
    !> @brief An x-axis object.
    !!
    !! @par Syntax
    !! @code{.f90}
    !! character(len = :) function, allocatable get_id_string(class(x_axis) this)
    !! @endcode
    !!
    !! @param[in] this The x_axis object.
    !! @return The string.
    type, extends(plot_axis) :: x_axis
        !> The ID character
        character :: m_id = "x"
    contains
        !> @brief Gets the axis identification string.
        procedure, public :: get_id_string => xa_get_id
    end type

! ------------------------------------------------------------------------------
    !> @brief A y-axis object.
    !!
    !! @par Syntax
    !! @code{.f90}
    !! character(len = :) function, allocatable get_id_string(class(y_axis) this)
    !! @endcode
    !!
    !! @param[in] this The y_axis object.
    !! @return The string.
    type, extends(plot_axis) :: y_axis
        !> The ID character
        character :: m_id = "y"
    contains
        !> @brief Gets the axis identification string.
        procedure, public :: get_id_string => ya_get_id
    end type

! ------------------------------------------------------------------------------
    !> @brief A secondary y-axis object.
    !!
    !! @par Syntax
    !! @code{.f90}
    !! character(len = :) function, allocatable get_id_string(class(y2_axis) this)
    !! @endcode
    !!
    !! @param[in] this The y2_axis object.
    !! @return The string.
    type, extends(plot_axis) :: y2_axis
        !> The ID character
        character(len = 2) :: m_id = "y2"
    contains
        !> @brief Gets the axis identification string.
        procedure, public :: get_id_string => y2a_get_id
    end type

! ------------------------------------------------------------------------------
    !> @brief A z-axis object.
    !!
    !! @par Syntax
    !! @code{.f90}
    !! character(len = :) function, allocatable get_id_string(class(z_axis) this)
    !! @endcode
    !!
    !! @param[in] this The z_axis object.
    !! @return The string.
    type, extends(plot_axis) :: z_axis
        !> The ID character
        character :: m_id = "z"
    contains
        !> @brief Gets the axis identification string.
        procedure, public :: get_id_string => za_get_id
    end type

! ------------------------------------------------------------------------------
    interface
        module function xa_get_id(this) result(x)
            class(x_axis), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function ya_get_id(this) result(x)
            class(y_axis), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function y2a_get_id(this) result(x)
            class(y2_axis), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function za_get_id(this) result(x)
            class(z_axis), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

! ******************************************************************************
! ABSTRACT METHOD INTERFACES
! ------------------------------------------------------------------------------
    interface
        !> @brief Retrieves a string from a plot_object.
        !!
        !! @param[in] this The plot_object object.
        !! @return The string.
        function get_string_result(this) result(x)
            import plot_object
            class(plot_object), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        !> @brief Retrieves a string from a terminal.
        !!
        !! @param[in] this The terminal object.
        !! @return The string.
        function term_get_string_result(this) result(x)
            import terminal
            class(terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        !> @brief Retrieves a string from a plot_data object.
        !!
        !! @param[in] this The plot_data object.
        !! @return The string.
        function pd_get_string_result(this) result(x)
            import plot_data
            class(plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        !> @brief Retrieves a string from a plot_axis.
        !!
        !! @param[in] this The plot_axis object.
        !! @return The string.
        function pa_get_string_result(this) result(x)
            import plot_axis
            class(plot_axis), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        !> @brief Retrieves a numeric value from a scatter_plot_data object.
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @param[in] index The index of the value to retrieve.
        !! @return The requested value.
        pure function spd_get_value(this, index) result(x)
            use, intrinsic :: iso_fortran_env, only : int32, real64
            import scatter_plot_data
            class(scatter_plot_data), intent(in) :: this
            integer(int32), intent(in) :: index
            real(real64) :: x
        end function

        !> @brief Sets a numeric value into a scatter_plot_data object.
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] index The index of the value to retrieve.
        !! @param[in] x The value.
        subroutine spd_set_value(this, index, x)
            use, intrinsic :: iso_fortran_env, only : int32, real64
            import scatter_plot_data
            class(scatter_plot_data), intent(inout) :: this
            integer(int32), intent(in) :: index
            real(real64), intent(in) :: x
        end subroutine

        !> @brief Retrieves an integer value from a scatter_plot_data object.
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return The requested value.
        pure function spd_get_int_value(this) result(x)
            use, intrinsic :: iso_fortran_env, only : int32
            import scatter_plot_data
            class(scatter_plot_data), intent(in) :: this
            integer(int32) :: x
        end function

        !> @brief Retrieves a string from a scatter_plot_data object.
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return The string.
        function spd_get_string_result(this) result(x)
            import scatter_plot_data
            class(scatter_plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        !> @brief Retrieves a string from a colormap.
        !!
        !! @param[in] this The colormap object.
        !! @return The string.
        function cm_get_string_result(this) result(x)
            import colormap
            class(colormap), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_MULTIPLOT.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a multi-plot layout.
    !!
    !! @par Example
    !! The following example builds a multiplot consisiting of 2 rows of 2 
    !! plots.
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use fplot_core 
    !!     implicit none
    !!
    !!     ! Variables
    !!     integer(int32), parameter :: n = 1000
    !!     real(real64), allocatable, dimension(:) :: x1, y1, x2, y2, x3, y3, x4, y4
    !!     type(multiplot) :: mplt
    !!     type(plot_2d) :: plt1, plt2, plt3, plt4
    !!     type(plot_data_2d) :: d1, d2, d3, d4
    !!        
    !!     ! Build the data sets
    !!     x1 = linspace(0.0d0, 1.0d0, n)
    !!     x2 = linspace(0.0d0, 2.0d0, n)
    !!     x3 = linspace(0.0d0, 3.0d0, n)
    !!     x4 = linspace(0.0d0, 4.0d0, n)
    !!     y1 = sin(20.0d0 * x1)
    !!     y2 = sin(20.0d0 * x2) * cos(50.0d0 * x2)
    !!     y3 = sqrt(x3) * sin(10.0d0 * x3)
    !!     y4 = exp(-0.1d0 * x4) * sin(15.0d0 * x4)
    !!
    !!     ! Define the plots
    !!     call mplt%initialize(2, 2)
    !!     call plt1%initialize()
    !!     call plt2%initialize()
    !!     call plt3%initialize()
    !!     call plt4%initialize()
    !!
    !!     ! Add the data to the plots
    !!     call d1%define_data(x1, y1)
    !!     call d2%define_data(x2, y2)
    !!     call d3%define_data(x3, y3)
    !!     call d4%define_data(x4, y4)
    !!
    !!     call plt1%push(d1)
    !!     call plt1%set_title("Plot 1 (1, 1)")
    !!
    !!     call plt2%push(d2)
    !!     call plt2%set_title("Plot 2 (2, 1)")
    !!
    !!     call plt3%push(d3)
    !!     call plt3%set_title("Plot 3 (1, 2)")
    !!
    !!     call plt4%push(d4)
    !!     call plt4%set_title("Plot 4 (2, 2)")
    !!
    !!     ! Locate the plots within the multiplot, and then draw
    !!     call mplt%set(1, 1, plt1)
    !!     call mplt%set(2, 1, plt2)
    !!     call mplt%set(1, 2, plt3)
    !!     call mplt%set(2, 2, plt4)
    !!     call mplt%draw()
    !! end program
    !! @endcode
    !! @image html example_multiplot_2.png
    type, extends(plot_object) :: multiplot
        !> The collection of plot objects.
        type(list) :: m_plots
        !> The number of rows of plots.
        integer(int32) :: m_rows = 0
        !> The number of columns of plots.
        integer(int32) :: m_cols = 0
        !> The page title.
        character(len = PLOTDATA_MAX_NAME_LENGTH) :: m_title
        !> Has a title?
        logical :: m_hasTitle = .false.
        !> The BNUPLOT terminal object to target.
        class(terminal), pointer :: m_terminal => null()
    contains
        final :: mp_clean
        procedure, public :: get_command_string => mp_get_command
        !> @brief Initializes the multiplot object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine initialize(class(multiplot) this, optional class(terminal) term, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The multiplot object.
        !! @param[in] term An optional input that is used to define the terminal.
        !!  The default terminal is a WXT terminal.  The acceptable inputs are:
        !!  - GNUPLOT_TERMINAL_PNG
        !!  - GNUPLOT_TERMINAL_QT
        !!  - GNUPLOT_TERMINAL_WIN32
        !!  - GNUPLOT_TERMINAL_WXT
        !!  - GNUPLOT_TERMINAL_LATEX
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!
        !! @par Example
        !! The following example illustrates a multiplot using 2 plots in a 
        !! single column.
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     ! Variables
        !!     integer(int32), parameter :: n = 1000
        !!     real(real64), allocatable, dimension(:) :: x1, y1, x2, y2
        !!     type(multiplot) :: mplt
        !!     type(plot_2d) :: plt1, plt2
        !!     type(plot_data_2d) :: d1, d2
        !!     class(plot_axis), pointer :: x1Axis, x2Axis, y1Axis, y2Axis
        !!
        !!     ! Build the data sets
        !!     x1 = linspace(0.0d0, 5.0d0, n)
        !!     x2 = linspace(0.0d0, 10.0d0, n)
        !!     y1 = exp(-0.1d0 * x1) * sin(20.0d0 * x1)
        !!     y2 = exp(-0.2d0 * x2) * sin(15.0d0 * x2) + 0.1d0 * sin(75.0d0 * x2)
        !!
        !!     ! Define the plots
        !!     call mplt%initialize(2, 1)
        !!     call mplt%set_font_size(14)
        !!     call plt1%initialize()
        !!     call plt2%initialize()
        !!
        !!     x1Axis => plt1%get_x_axis()
        !!     y1Axis => plt1%get_y_axis()
        !!
        !!     x2Axis => plt2%get_x_axis()
        !!     y2Axis => plt2%get_y_axis()
        !!
        !!     call x1Axis%set_title("X1")
        !!     call y1Axis%set_title("Y1")
        !!
        !!     call x2Axis%set_title("X2")
        !!     call y2Axis%set_title("Y2")
        !!
        !!     call d1%set_name("Data Set 1")
        !!     call d1%set_line_color(CLR_BLUE)
        !!     call d1%set_line_width(2.0)
        !!     call d1%define_data(x1, y1)
        !!
        !!     call d2%set_name("Data Set 2")
        !!     call d2%set_line_color(CLR_GREEN)
        !!     call d2%set_line_width(2.0)
        !!     call d2%define_data(x2, y2)
        !!
        !!     call plt1%push(d1)
        !!     call plt2%push(d2)
        !!
        !!     call mplt%set(1, 1, plt1)
        !!     call mplt%set(2, 1, plt2)
        !!     call mplt%draw()
        !! end program
        !! @endcode
        !! @image html example_multiplot_1.png
        procedure, public :: initialize => mp_init
        !> @brief Gets the number of rows of plots.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_row_count(class(multiplot) this)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @return The number of rows.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(multiplot) :: plt
        !!     integer(int32) :: n
        !!
        !!     n = plt%get_row_count()
        !! end program
        !! @endcode
        procedure, public :: get_row_count => mp_get_rows
        !> @brief Gets the number of columns of plots.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_column_count(class(multiplot) this)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @return The number of columns.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(multiplot) :: plt
        !!     integer(int32) :: n
        !!
        !!     n = plt%get_column_count()
        !! end program
        !! @endcode
        procedure, public :: get_column_count => mp_get_cols
        !> @brief Gets the number of plots.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_plot_count(class(multiplot) this)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @return The number of plots.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(multiplot) :: plt
        !!     integer(int32) :: n
        !!
        !!     n = plt%get_plot_count()
        !! end program
        !! @endcode
        procedure, public :: get_plot_count => mp_get_count
        !> @brief Gets the multiplot's title.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_title(class(multiplot))
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @return The multiplot's title.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(multiplot) :: plt
        !!     character(len = :), allocatable :: txt
        !!
        !!     txt = plt%get_title()
        !! end program
        !! @endcode
        procedure, public :: get_title => mp_get_title
        !> @brief Sets the multiplot's title.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_title(class(multiplot) this, character(len = *) txt)
        !! @endcode
        !!
        !! @param[in,out] this The multiplot object.
        !! @param[in] txt The multiplot's title.  The number of characters must be less
        !! than or equal to PLOTDATA_MAX_NAME_LENGTH; else, the text string is
        !! truncated.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(multiplot) :: plt
        !!
        !!     call plt%set_title("Title")
        !! end program
        !! @endcode
        procedure, public :: set_title => mp_set_title
        !> @brief Launches GNUPLOT and draws the multiplot per the current state of
        !! the command list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine draw(class(multiplot) this, optional logical persist, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @param[in] persist An optional parameter that can be used to keep GNUPLOT
        !!  open.  Set to true to force GNUPLOT to remain open; else, set to false
        !!  to allow GNUPLOT to close after drawing.  The default is true.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_GNUPLOT_FILE_ERROR: Occurs if the command file cannot be written.
        !!
        !! @par Example
        !! The following example illustrates a multiplot using 2 plots in a 
        !! single column.
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     ! Variables
        !!     integer(int32), parameter :: n = 1000
        !!     real(real64), allocatable, dimension(:) :: x1, y1, x2, y2
        !!     type(multiplot) :: mplt
        !!     type(plot_2d) :: plt1, plt2
        !!     type(plot_data_2d) :: d1, d2
        !!     class(plot_axis), pointer :: x1Axis, x2Axis, y1Axis, y2Axis
        !!
        !!     ! Build the data sets
        !!     x1 = linspace(0.0d0, 5.0d0, n)
        !!     x2 = linspace(0.0d0, 10.0d0, n)
        !!     y1 = exp(-0.1d0 * x1) * sin(20.0d0 * x1)
        !!     y2 = exp(-0.2d0 * x2) * sin(15.0d0 * x2) + 0.1d0 * sin(75.0d0 * x2)
        !!
        !!     ! Define the plots
        !!     call mplt%initialize(2, 1)
        !!     call mplt%set_font_size(14)
        !!     call plt1%initialize()
        !!     call plt2%initialize()
        !!
        !!     x1Axis => plt1%get_x_axis()
        !!     y1Axis => plt1%get_y_axis()
        !!
        !!     x2Axis => plt2%get_x_axis()
        !!     y2Axis => plt2%get_y_axis()
        !!
        !!     call x1Axis%set_title("X1")
        !!     call y1Axis%set_title("Y1")
        !!
        !!     call x2Axis%set_title("X2")
        !!     call y2Axis%set_title("Y2")
        !!
        !!     call d1%set_name("Data Set 1")
        !!     call d1%set_line_color(CLR_BLUE)
        !!     call d1%set_line_width(2.0)
        !!     call d1%define_data(x1, y1)
        !!
        !!     call d2%set_name("Data Set 2")
        !!     call d2%set_line_color(CLR_GREEN)
        !!     call d2%set_line_width(2.0)
        !!     call d2%define_data(x2, y2)
        !!
        !!     call plt1%push(d1)
        !!     call plt2%push(d2)
        !!
        !!     call mplt%set(1, 1, plt1)
        !!     call mplt%set(2, 1, plt2)
        !!     call mplt%draw()
        !! end program
        !! @endcode
        !! @image html example_multiplot_1.png
        procedure, public :: draw => mp_draw
        !> @brief Gets the requested plot object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot) pointer function get(class(multiplot) this, integer(int32) i, integer(int32) j)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @param[in] i The row index of the plot to retrieve.
        !! @param[in] j The column index of the plot to retrieve.
        !! @return A pointer to the requested plot object.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(multiplot) :: plt
        !!     class(plot), pointer :: obj
        !!
        !!     obj => plt%get(1, 1)
        !! end program
        !! @endcode
        procedure, public :: get => mp_get
        !> @brief Sets the requested plot object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set(class(multiplot) this, integer(int32) i, integer(int32) j, class(plot) pointer x)
        !! @endcode
        !!
        !! @param[in,out] this The multiplot object.
        !! @param[in] i The row index of the plot to retrieve.
        !! @param[in] j The column index of the plot to retrieve.
        !! @param[in] x A pointer to the requested plot object.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(multiplot) :: plt
        !!     type(plot) :: obj
        !!
        !!     ! Be sure to initialze the plot and multiplot objects
        !!     ! ...
        !!
        !!     ! Store the plot object
        !!     call plt%set(1, 1, obj)
        !! end program
        !! @endcode
        procedure, public :: set => mp_set
        !> @brief Gets a value determining if a title has been defined for the
        !!  multiplot object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function is_title_defined(class(multiplot) this)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @return Returns true if a title has been defined for this multiplot; else,
        !!  returns false.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(multiplot) :: plt
        !!     logical :: check
        !!
        !!     check = plt%is_title_defined()
        !! end program
        !! @endcode
        procedure, public :: is_title_defined => mp_has_title
        !> @brief Gets the GNUPLOT terminal object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(terminal) function, pointer get_terminal(class(multiplot) this)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @return A pointer to the GNUPLOT terminal object.
        !!
        !! @par Example
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(multiplot) :: plt
        !!     class(terminal), pointer :: term
        !!
        !!     term => plt%get_terminal()
        !! end program
        procedure, public :: get_terminal => mp_get_term
        !> @brief Saves a GNUPLOT command file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine save_file(class(multiplot) this, character(len = *) fname, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @param[in] fname The filename.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_GNUPLOT_FILE_ERROR: Occurs if the command file cannot be written.
        !! @par Example
        !! @code{.f90}
        !! ! This example illustrates the frequency response of the following mechanical
        !! ! system.
        !! !
        !! ! -> y       |-> x1       |-> x2
        !! ! |                                  |/
        !! ! |-/\/\/\-| m |-/\/\/\-| m |-/\/\/\-|/
        !! ! |   k, b         k, b        k, b  |/
        !! !
        !! ! The equations of motion for this system are as follows.
        !! ! M x" + B x' + K x = F1 y' + F2 y
        !! !
        !! ! Where:
        !! !     | m   0 |
        !! ! M = |       |
        !! !     | 0   m |
        !! !
        !! !     | 2b     -b |
        !! ! B = |           |
        !! !     | -b     2b |
        !! !
        !! !     | 2k     -k |
        !! ! K = |           |
        !! !     | -k     2k |
        !! !
        !! !      | b |
        !! ! F1 = |   |
        !! !      | 0 |
        !! !
        !! !      | k |
        !! ! F2 = |   |
        !! !      | 0 |
        !! !
        !! ! The solution may be computed by applying the Laplace transform such that:
        !! ! (M s**2 + B s + K) X = (F1 s + F2) Y
        !! !
        !! ! And then solving for X / Y such that:
        !! ! Z = X / Y = inv(M s**2 + B s + K) * (F1 s + F2)
        !! program example
        !!     use iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     ! Parameters
        !!     real(real64), parameter :: m = 2.0d0
        !!     real(real64), parameter :: k = 450.0d3
        !!     real(real64), parameter :: b = 3.0d0
        !!     real(real64), parameter :: pi = 3.1415926535897932384626433832795d0
        !!     integer(int32), parameter :: npts = 1000
        !!     complex(real64), parameter :: j = (0.0d0, -1.0d0)
        !!
        !!     ! Local Variables
        !!     complex(real64), dimension(npts) :: s, z1, z2
        !!     real(real64), dimension(npts) :: freq, omega
        !!     type(multiplot) :: mplt
        !!     type(plot_2d) :: plt, pplt
        !!     type(plot_data_2d) :: d1, d2, d3, d4
        !!     class(plot_axis), pointer :: xAxis, yAxis
        !!     class(legend), pointer :: lgnd
        !!
        !!     ! Generate a frequency vector from 10 Hz to 1 kHz
        !!     freq = logspace(1.0d0, 3.0d0, npts)
        !!     omega = 2.0d0 * pi * freq
        !!     s = j * omega
        !!
        !!     ! Compute the frequency response functions for each mass
        !!     z1 = (b * s + k) * (m * s**2 + 2.0d0 * b * s + 2.0d0 * k) / &
        !!         ((m * s**2 + 2.0d0 * b * s + 2.0d0 * k)**2 + (-b * s - k) * (b * s + k))
        !!     z2 = (b * s + k)**2 / &
        !!         ((m * s**2 + 2.0d0 * b * s + 2.0d0 * k)**2 + (-b * s - k) * (b * s + k))
        !!
        !!     ! Create the plots
        !!     call mplt%initialize(2, 1)
        !!     call mplt%set_font_size(14)
        !!     call plt%initialize()
        !!     xAxis => plt%get_x_axis()
        !!     yAxis => plt%get_y_axis()
        !!
        !!     call xAxis%set_title("Frequency [Hz]")
        !!     call yAxis%set_title("Amplitude (X / Y)")
        !!
        !!     call xAxis%set_is_log_scaled(.true.)
        !!     call yAxis%set_is_log_scaled(.true.)
        !!
        !!     call d1%set_name("X1")
        !!     call d1%set_line_width(2.0)
        !!     call d1%define_data(freq, abs(z1))
        !!
        !!     call d2%set_name("X2")
        !!     call d2%set_line_width(2.0)
        !!     call d2%set_line_style(LINE_DASHED)
        !!     call d2%define_data(freq, abs(z2))
        !!
        !!     call plt%push(d1)
        !!     call plt%push(d2)
        !!
        !!     ! Set up the phase plot
        !!     call pplt%initialize()
        !!     xAxis => pplt%get_x_axis()
        !!     yAxis => pplt%get_y_axis()
        !!
        !!     call xAxis%set_title("Frequency [Hz]")
        !!     call yAxis%set_title("Phase [deg]")
        !!
        !!     call xAxis%set_is_log_scaled(.true.)
        !!
        !!     call d3%set_name("X1")
        !!     call d3%set_line_width(2.0)
        !!     call d3%define_data(freq, 180.0d0 * atan2(aimag(z1), real(z1)) / pi)
        !!
        !!     call d4%set_name("X2")
        !!     call d4%set_line_width(2.0)
        !!     call d4%set_line_style(LINE_DASHED)
        !!     call d4%define_data(freq, 180.0d0 * atan2(aimag(z2), real(z2)) / pi)
        !!
        !!     call pplt%push(d3)
        !!     call pplt%push(d4)
        !!
        !!     ! Don't use a legend on the phase plot
        !!     lgnd => pplt%get_legend()
        !!     call lgnd%set_is_visible(.false.)
        !!
        !!     ! Save the plot to file
        !!     call mplt%set(1, 1, plt)
        !!     call mplt%set(2, 1, pplt)
        !!     call mplt%save_file("example_multiplot_file.plt")
        !! end program
        !! @endcode
        !! Then, from gnuplot, simply issue the command: load
        !! "example_multiplot_file.plt" to obtain the plot.
        !! @image html example_multiplot_log_scaled.png
        procedure, public :: save_file => mp_save
        !> @brief Gets the name of the font used for plot text.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_font_name(class(multiplot) this)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @return The font name.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(multiplot) :: plt
        !!     character(len = :), allocatable :: name
        !!
        !!     name = plt%get_font_name()
        !! end program
        !! @endcode
        procedure, public :: get_font_name => mp_get_font
        !> @brief Sets the name of the font used for plot text.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_font_name(class(multiplot) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The multiplot object.
        !! @param[in] x The font name.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(multiplot) :: plt
        !!
        !!     ! Establish the font used by the plot as Arial.
        !!     call plt%set_title("Arial")
        !! end program
        !! @endcode
        procedure, public :: set_font_name => mp_set_font
        !> @brief Gets the size of the font used by the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) function get_font_size(class(multiplot) this)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @return The size of the font, in points.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     implicit none
        !!
        !!     type(multiplot) :: plt
        !!     integer(int32) :: sz
        !!
        !!     sz = plt%get_font_size()
        !! end program
        !! @endcode
        procedure, public :: get_font_size => mp_get_font_size
        !> @brief Sets the size of the font used by the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_font_size(class(multiplot) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The multiplot object.
        !! @param[in] x The font size, in points.  If a value of zero is provided,
        !! the font size is reset to its default value; or, if a negative value
        !! is provided, the absolute value of the supplied value is utilized.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(multiplot) :: plt
        !!
        !!     ! Set the font to be 14 point in size
        !!     call plt%set_font_size(14)
        !! end program
        !! @endcode
        procedure, public :: set_font_size => mp_set_font_size
    end type

! ------------------------------------------------------------------------------
    interface
        module function mp_get_command(this) result(x)
            class(multiplot), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine mp_init(this, m, n, term, err)
            class(multiplot), intent(inout) :: this
            integer(int32), intent(in) :: m, n
            integer(int32), intent(in), optional :: term
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine mp_clean(this)
            type(multiplot), intent(inout) :: this
        end subroutine

        pure module function mp_get_rows(this) result(x)
            class(multiplot), intent(in) :: this
            integer(int32) :: x
        end function

        pure module function mp_get_cols(this) result(x)
            class(multiplot), intent(in) :: this
            integer(int32) :: x
        end function

        pure module function mp_get_count(this) result(x)
            class(multiplot), intent(in) :: this
            integer(int32) :: x
        end function

        module function mp_get_title(this) result(x)
            class(multiplot), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine mp_set_title(this, x)
            class(multiplot), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine

        module subroutine mp_draw(this, persist, err)
            class(multiplot), intent(in) :: this
            logical, intent(in), optional :: persist
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function mp_get(this, i, j) result(x)
            class(multiplot), intent(in) :: this
            integer(int32), intent(in) :: i, j
            class(plot), pointer :: x
        end function

        module subroutine mp_set(this, i, j, x)
            class(multiplot), intent(inout) :: this
            integer(int32), intent(in) :: i, j
            class(plot), intent(in) :: x
        end subroutine

        pure module function mp_has_title(this) result(x)
            class(multiplot), intent(in) :: this
            logical :: x
        end function

        module function mp_get_term(this) result(x)
            class(multiplot), intent(in) :: this
            class(terminal), pointer :: x
        end function

        module subroutine mp_save(this, fname, err)
            class(multiplot), intent(in) :: this
            character(len = *), intent(in) :: fname
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function mp_get_font(this) result(x)
            class(multiplot), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine mp_set_font(this, x)
            class(multiplot), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine

        module function mp_get_font_size(this) result(x)
            class(multiplot), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine mp_set_font_size(this, x)
            class(multiplot), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_PLOT_DATA_ERRORS.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a 2D error bar based data set.
    !!
    !! @par Example
    !! The following example illustrates the use of y error bars.
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use fplot_core
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: n = 50
    !!
    !!     ! Local Variables
    !!     real(real64) :: x(n), y(n), yerr(n)
    !!     type(plot_2d) :: plt
    !!     type(plot_data_2d) :: d1
    !!     type(plot_data_error_bars) :: e1
    !!
    !!     ! Initialization
    !!     x = linspace(0.0d0, 1.0d1, n)
    !!     y = sin(x)
    !!     call random_number(yerr)
    !!     yerr = 3.0d-1 * (yerr - 0.5d0)
    !!
    !!     ! Create the plot
    !!     call plt% initialize()
    !!
    !!     call d1%define_data(x, y)
    !!     call d1%set_name("Raw")
    !!
    !!     call e1%define_y_error_data(x, y, yerr)
    !!     call e1%set_name("Errors")
    !!
    !!     call plt%push(d1)
    !!     call plt%push(e1)
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! @image html example_y_errorbars_1.png
    type, extends(plot_data_colored) :: plot_data_error_bars
        !> Display x error bars?
        logical :: m_xBars = .false.
        !> Display y error bars?
        logical :: m_yBars = .false.
        !> A matrix containing the raw and error data.  Column 1 is for the
        !! x coordinate, column 2 for the y coordinate, and the remaining 
        !! columns are for the error data (x, then y if applicable)
        real(real64), allocatable, dimension(:,:) :: m_data
        !> Display an error box for the case where x and y errors are defined.
        logical :: m_box = .false.
    contains
        procedure, public :: get_command_string => pde_get_cmd
        procedure, public :: get_data_string => pde_get_data_cmd
        !> @brief Defines the x error data.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_x_error_data(class(plot_data_error_bars) this, real(real64) x(:), real(real64) y(:), real(real64) xerr(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_error_bars object.
        !! @param[in] x An N-element array containing the x coordinates of the 
        !!  data.
        !! @param[in] y An N-element array containing the y coordinates of the 
        !!  data.
        !! @param[in] xerr An N-element array containing the x errors at each
        !!  data point.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if the input arrays are not
        !!      the same size.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        !!
        !! @par Example
        !! The following example illustrates the use of x error bars.
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     ! Parameters
        !!     integer(int32), parameter :: n = 50
        !!
        !!     ! Local Variables
        !!     real(real64) :: x(n), y(n), xerr(n)
        !!     type(plot_2d) :: plt
        !!     type(plot_data_2d) :: d1
        !!     type(plot_data_error_bars) :: e1
        !!
        !!     ! Initialization
        !!     x = linspace(0.0d0, 1.0d1, n)
        !!     y = sin(x)
        !!     call random_number(xerr)
        !!     xerr = (xerr - 0.5d0)
        !!
        !!     ! Create the plot
        !!     call plt% initialize()
        !!
        !!     call d1%define_data(x, y)
        !!     call d1%set_name("Raw")
        !!
        !!     call e1%define_x_error_data(x, y, xerr)
        !!     call e1%set_name("Errors")
        !!
        !!     call plt%push(d1)
        !!     call plt%push(e1)
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_x_errorbars_1.png
        procedure, public :: define_x_error_data => pde_define_x_err
        !> @brief Defines the y error data.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_y_error_data(class(plot_data_error_bars) this, real(real64) x(:), real(real64) y(:), real(real64) yerr(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_error_bars object.
        !! @param[in] x An N-element array containing the x coordinates of the 
        !!  data.
        !! @param[in] y An N-element array containing the y coordinates of the 
        !!  data.
        !! @param[in] yerr An N-element array containing the y errors at each
        !!  data point.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if the input arrays are not
        !!      the same size.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        !!
        !! @par Example
        !! The following example illustrates the use of y error bars.
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     ! Parameters
        !!     integer(int32), parameter :: n = 50
        !!
        !!     ! Local Variables
        !!     real(real64) :: x(n), y(n), yerr(n)
        !!     type(plot_2d) :: plt
        !!     type(plot_data_2d) :: d1
        !!     type(plot_data_error_bars) :: e1
        !!
        !!     ! Initialization
        !!     x = linspace(0.0d0, 1.0d1, n)
        !!     y = sin(x)
        !!     call random_number(yerr)
        !!     yerr = 3.0d-1 * (yerr - 0.5d0)
        !!
        !!     ! Create the plot
        !!     call plt% initialize()
        !!
        !!     call d1%define_data(x, y)
        !!     call d1%set_name("Raw")
        !!
        !!     call e1%define_y_error_data(x, y, yerr)
        !!     call e1%set_name("Errors")
        !!
        !!     call plt%push(d1)
        !!     call plt%push(e1)
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_y_errorbars_1.png
        procedure, public :: define_y_error_data => pde_define_y_err
        !> @brief Defines the x and y error data.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_xy_error_data(class(plot_data_error_bars) this, real(real64) x(:), real(real64) y(:), real(real64) xerr(:), real(real64) yerr(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_error_bars object.
        !! @param[in] x An N-element array containing the x coordinates of the 
        !!  data.
        !! @param[in] y An N-element array containing the y coordinates of the 
        !!  data.
        !! @param[in] xerr An N-element array containing the x errors at each
        !!  data point.
        !! @param[in] yerr An N-element array containing the y errors at each
        !!  data point.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if the input arrays are not
        !!      the same size.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        !!
        !! @par Example
        !! The following example illustrates the use of x and y error bars.
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     ! Parameters
        !!     integer(int32), parameter :: n = 50
        !!
        !!     ! Local Variables
        !!     real(real64) :: x(n), y(n), xerr(n), yerr(n)
        !!     type(plot_2d) :: plt
        !!     type(plot_data_2d) :: d1
        !!     type(plot_data_error_bars) :: e1
        !!
        !!     ! Initialization
        !!     x = linspace(0.0d0, 1.0d1, n)
        !!     y = sin(x)
        !!     call random_number(xerr)
        !!     xerr = (xerr - 0.5d0)
        !!     call random_number(yerr)
        !!     yerr = 3.0d-1 * (yerr - 0.5d0)
        !!
        !!     ! Create the plot
        !!     call plt% initialize()
        !!
        !!     call d1%define_data(x, y)
        !!     call d1%set_name("Raw")
        !!
        !!     call e1%define_xy_error_data(x, y, xerr, yerr)
        !!     call e1%set_name("Errors")
        !!
        !!     call plt%push(d1)
        !!     call plt%push(e1)
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_xy_errorbars_1.png
        procedure, public :: define_xy_error_data => pde_define_xy_err
        !> @brief Tests to see if the x error bar data has been defined, and as
        !!  a result, if the x error data is to be plotted.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_plot_x_error_bars(class(plot_data_error_bars) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_error_bars object.
        !! @return Returns true if the x error bars are to be plotted; else,
        !!  false.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_data_error_bars) :: obj
        !!     logical :: check
        !!
        !!     check = obj%get_plot_x_error_bars()
        !! end program
        !! @endcode
        procedure, public :: get_plot_x_error_bars => pde_get_plot_x_err
        !> @brief Tests to see if the y error bar data has been defined, and as
        !!  a result, if the y error data is to be plotted.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_plot_y_error_bars(class(plot_data_error_bars) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_error_bars object.
        !! @return Returns true if the y error bars are to be plotted; else,
        !!  false.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_data_error_bars) :: obj
        !!     logical :: check
        !!
        !!     check = obj%get_plot_y_error_bars()
        !! end program
        !! @endcode
        procedure, public :: get_plot_y_error_bars => pde_get_plot_y_err
        !> @brief Gets the number of stored data points.
        !!
        !! @param[in] this The plot_data_error_bars object.
        !! @return The number of data points.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_data_error_bars) :: obj
        !!     integer(int32) :: n
        !!
        !!     n = obj%get_count()
        !! end program
        !! @endcode
        procedure, public :: get_count => pde_get_count
        !> @brief Tests to see if the x and y error boxes should be utilized.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_use_error_box(class(plot_data_error_bars) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_error_bars object.
        !! @return Returns true if the error boxes are to be plotted; else,
        !!  false.
        !!
        !! @par Remarks
        !! Notice, the error boxes are only utilized if there is both x and y
        !! error data defined, regardless of the value of this property.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     implicit none
        !!
        !!     type(plot_data_error_bars) :: obj
        !!     logical :: check
        !!
        !!     check = obj%get_use_error_box()
        !! end program
        !! @endcode
        procedure, public :: get_use_error_box => pde_get_box
        !> @brief Deterimines if the x and y error boxes should be utilized.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_use_error_box(class(plot_data_error_bars) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_error_bars object.
        !! @param[in] x Set to true if the error boxes are to be plotted; else,
        !!  false.
        !!
        !! @par Remarks
        !! Notice, the error boxes are only utilized if there is both x and y
        !! error data defined, regardless of the value of this property.
        !!
        !! @par Example
        !! The following example illustrates the use of error boxes instead of
        !! error bars.
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use fplot_core
        !!     implicit none
        !!
        !!     ! Parameters
        !!     integer(int32), parameter :: n = 50
        !!
        !!     ! Local Variables
        !!     real(real64) :: x(n), y(n), xerr(n), yerr(n)
        !!     type(plot_2d) :: plt
        !!     type(plot_data_2d) :: d1
        !!     type(plot_data_error_bars) :: e1
        !!
        !!     ! Initialization
        !!     x = linspace(0.0d0, 1.0d1, n)
        !!     y = sin(x)
        !!     call random_number(xerr)
        !!     xerr = (xerr - 0.5d0)
        !!     call random_number(yerr)
        !!     yerr = 3.0d-1 * (yerr - 0.5d0)
        !!
        !!     ! Create the plot
        !!     call plt% initialize()
        !!
        !!     call d1%define_data(x, y)
        !!     call d1%set_name("Raw")
        !!
        !!     call e1%define_xy_error_data(x, y, xerr, yerr)
        !!     call e1%set_name("Errors")
        !!     call e1%set_use_error_box(.true.)
        !!
        !!     call plt%push(d1)
        !!     call plt%push(e1)
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_xy_errorbox_1.png
        procedure, public :: set_use_error_box => pde_set_box
    end type

! ------------------------------------------------------------------------------
    interface
        module function pde_get_cmd(this) result(cmd)
            class(plot_data_error_bars), intent(in) :: this
            character(len = :), allocatable :: cmd
        end function

        module function pde_get_data_cmd(this) result(cmd)
            class(plot_data_error_bars), intent(in) :: this
            character(len = :), allocatable :: cmd
        end function

        module subroutine pde_define_x_err(this, x, y, xerr, err)
            class(plot_data_error_bars), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x, y, xerr
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine pde_define_y_err(this, x, y, yerr, err)
            class(plot_data_error_bars), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x, y, yerr
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine pde_define_xy_err(this, x, y, xerr, yerr, err)
            class(plot_data_error_bars), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x, y, xerr, yerr
            class(errors), intent(inout), optional, target :: err
        end subroutine

        pure module function pde_get_plot_x_err(this) result(x)
            class(plot_data_error_bars), intent(in) :: this
            logical :: x
        end function

        pure module function pde_get_plot_y_err(this) result(x)
            class(plot_data_error_bars), intent(in) :: this
            logical :: x
        end function

        pure module function pde_get_count(this) result(x)
            class(plot_data_error_bars), intent(in) :: this
            integer(int32) :: x
        end function

        pure module function pde_get_box(this) result(x)
            class(plot_data_error_bars), intent(in) :: this
            logical :: x
        end function

        module subroutine pde_set_box(this, x)
            class(plot_data_error_bars), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine
    end interface

end module
