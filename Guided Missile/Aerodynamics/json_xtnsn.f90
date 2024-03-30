! Extends the original JSON module for usability
module json_xtnsn_mod

    use json_mod
    implicit none

    logical :: json_found

    interface json_xtnsn_get
        module procedure :: json_xtnsn_value_get_real, json_xtnsn_file_get_real
        module procedure :: json_xtnsn_value_get_integer, json_xtnsn_file_get_integer
        module procedure :: json_xtnsn_value_get_string, json_xtnsn_file_get_string
        module procedure :: json_xtnsn_value_get_logical, json_xtnsn_file_get_logical
    end interface json_xtnsn_get

contains


    subroutine json_xtnsn_value_get_real(json, name, value, default_value)
        implicit none
        type(json_value),intent(in),pointer :: json
        character(len=*), intent(in) :: name
        real, intent(out) :: value
        real, intent(in), optional :: default_value
    
        call json_get(json, name, value, json_found)
        if(json_failed() .or. (.not. json_found)) then
            if (present(default_value)) then
                value = default_value
                call json_clear_exceptions()
            else
                write(*,*) 'Error: Unable to read required value: ',name
                STOP
            end if
        end if
    end subroutine json_xtnsn_value_get_real
    
    
    subroutine json_xtnsn_value_get_integer(json, name, value, default_value)
        implicit none
        type(json_value),intent(in),pointer :: json
        character(len=*), intent(in) :: name
        integer, intent(out) :: value
        integer, intent(in), optional :: default_value
    
        call json_get(json, name, value, json_found)
        if((.not.json_found) .or. json_failed()) then
            if (present(default_value)) then
                value = default_value
                call json_clear_exceptions()
            else
                write(*,*) 'Error: Unable to read required value: ', name
                STOP
            end if
        end if
    
    end subroutine json_xtnsn_value_get_integer
    
    
    subroutine json_xtnsn_value_get_string(json, name, value, default_value)
        implicit none
        type(json_value), intent(in), pointer :: json
        character(len=*), intent(in) :: name
        character(:), allocatable, intent(out) :: value
        character(len=*), intent(in), optional :: default_value
    
        call json_get(json, name, value, json_found)
        if((.not.json_found) .or. json_failed()) then
            if (present(default_value)) then
                value = default_value
                call json_clear_exceptions()
            else
                write(*,*) 'Error: Unable to read required value: ', name
                STOP
            end if
        end if
    
    end subroutine json_xtnsn_value_get_string
    
    
    subroutine json_xtnsn_value_get_logical(json, name, value, default_value)
        implicit none
        type(json_value), intent(in), pointer :: json
        character(len=*), intent(in) :: name
        logical, intent(out) :: value
        logical, intent(in), optional :: default_value
    
        call json_get(json, name, value, json_found)
        if((.not.json_found) .or. json_failed()) then
            if (present(default_value)) then
                value = default_value
                call json_clear_exceptions()
            else
                write(*,*) 'Error: Unable to read required value: ', name
                STOP
            end if
        end if
    
    end subroutine json_xtnsn_value_get_logical
    
    
    subroutine json_xtnsn_file_get_real(json, name, value, default_value)
        implicit none
        type(json_file) :: json
        character(len=*), intent(in) :: name
        real, intent(out) :: value
        real, intent(in), optional :: default_value
    
        call json%get(name, value)
        if(json_failed()) then
            if (present(default_value)) then
                value = default_value
                call json_clear_exceptions()
            else
                write(*,*) 'Error: Unable to read required value: ', trim(name)
                STOP
            end if
        end if
    
    end subroutine json_xtnsn_file_get_real
    
    
    subroutine json_xtnsn_file_get_integer(json, name, value, default_value)
        implicit none
        type(json_file) :: json
        character(len=*), intent(in) :: name
        integer, intent(out) :: value
        integer, intent(in), optional :: default_value
    
        call json%get(name, value)
        if(json_failed()) then
            if (present(default_value)) then
                value = default_value
                call json_clear_exceptions()
            else
                write(*,*) 'Error: Unable to read required value: ',name
                STOP
            end if
        end if
    end subroutine json_xtnsn_file_get_integer
    
    
    subroutine json_xtnsn_file_get_string(json, name, value)
        implicit none
        type(json_file) :: json
        character(len=*), intent(in) :: name
        character(:), allocatable, intent(out) :: value
    
        call json%get(name, value)
        if(json_failed()) then
            write(*,*) 'Error: Unable to read required value: ',name
            STOP
        end if
    
        value = trim(value)
    end subroutine json_xtnsn_file_get_string
    
    
    subroutine json_xtnsn_file_get_logical(json, name, value)
        implicit none
        type(json_file) :: json
        character(len=*), intent(in) :: name
        logical, intent(out) :: value
    
        call json%get(name, value)
        if(json_failed()) then
            write(*,*) 'Error: Unable to read required value: ',name
            STOP
        end if
    
    end subroutine json_xtnsn_file_get_logical
    
    
    subroutine json_check()
        if(json_failed()) then
            call print_json_error_message()
            STOP
        end if
    end subroutine json_check
    
    
    subroutine print_json_error_message()
        implicit none
        character(len=:),allocatable :: error_msg
        logical :: status_ok
    
        !get error message:
        call json_check_for_errors(status_ok, error_msg)
    
        !print it if there is one:
        if (.not. status_ok) then
            write(*,'(A)') error_msg
            deallocate(error_msg)
            call json_clear_exceptions()
        end if
    
    end subroutine print_json_error_message

end module json_xtnsn_mod