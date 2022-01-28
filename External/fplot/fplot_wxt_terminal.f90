! fplot_wxt_terminal.f90

submodule (fplot_core) fplot_wxt_terminal
contains
    module function wxt_get_term_string(this) result(x)
        class(wxt_terminal), intent(in) :: this
        character(len = :), allocatable :: x
        n = len_trim(this%m_id)
        allocate(character(len = n) :: x)
        x = this%m_id
    end function
end submodule
