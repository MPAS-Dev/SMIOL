#include "smiol_codes.inc"

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! SMIOL -- The Simple MPAS I/O Library
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module SMIOLf

    use iso_c_binding, only : c_int, c_size_t, c_ptr

    private

    public :: SMIOLf_context, &
              SMIOLf_decomp, &
              SMIOLf_file

    public :: SMIOLf_init, &
              SMIOLf_finalize, &
              SMIOLf_inquire, &
              SMIOLf_open_file, &
              SMIOLf_close_file, &
              SMIOLf_define_dim, &
              SMIOLf_inquire_dim, &
              SMIOLf_define_var, &
              SMIOLf_inquire_var, &
              SMIOLf_put_var, &
              SMIOLf_get_var, &
              SMIOLf_define_att, &
              SMIOLf_inquire_att, &
              SMIOLf_file_sync, &
              SMIOLf_error_string, &
              SMIOLf_set_option, &
              SMIOLf_create_decomp, &
              SMIOLf_free_decomp


    type, bind(C) :: SMIOLf_context
        integer :: fcomm             ! Fortran handle to MPI communicator; MPI_Fint on the C side, which is supposed to match a Fortran integer
        integer(c_int) :: comm_size  ! Size of MPI communicator
        integer(c_int) :: comm_rank  ! Rank within MPI communicator
    end type SMIOLf_context

    type, bind(C) :: SMIOLf_file
        integer(c_int) :: i
    end type SMIOLf_file

    type, bind(C) :: SMIOLf_decomp
        integer(c_size_t) :: n_compute_elements
        integer(c_size_t) :: n_io_elements
        type(c_ptr) :: compute_elements
        type(c_ptr) :: io_elements
    end type SMIOLf_decomp


contains


    !
    ! Library methods
    !

    !-----------------------------------------------------------------------
    !  routine SMIOLf_init
    !
    !> \brief Initialize a SMIOL context
    !> \details
    !>  Initializes a SMIOL context, within which decompositions may be defined and
    !>  files may be read and written. At present, the only input argument is an MPI
    !>  communicator.
    !>
    !>  Upon successful return the context argument points to a valid SMIOL context;
    !>  otherwise, it is NULL and an error code other than MPI_SUCCESS is returned.
    !>
    !>  Note: It is assumed that MPI_Init has been called prior to this routine, so
    !>        that any use of the provided MPI communicator will be valid.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_init(comm, context) result(ierr)

        use iso_c_binding, only : c_ptr, c_f_pointer, c_null_ptr, c_associated

        implicit none

        integer, intent(in) :: comm
        type (SMIOLf_context), pointer :: context

        type (c_ptr) :: c_context = c_null_ptr

        ! C interface definitions
        interface
            function SMIOL_fortran_init(comm, context) result(ierr) bind(C, name='SMIOL_fortran_init')
                use iso_c_binding, only : c_int, c_ptr
                integer, value :: comm   ! MPI_Fint on the C side, which is supposed to match a Fortran integer
                type (c_ptr) :: context
                integer(kind=c_int) :: ierr
            end function
        end interface

        ierr = SMIOL_fortran_init(comm, c_context)

        if (ierr == SMIOL_SUCCESS) then
            if (.not. c_associated(c_context)) then
                nullify(context)
                ierr = -997     ! TODO: define an error code for this. The c_context should not be null...
            else
                call c_f_pointer(c_context, context)
            end if
        else
            if (.not. c_associated(c_context)) then
                nullify(context)
            else
                ierr = -997     ! TODO: define an error code for this. The c_context should be null...
            end if
        end if

    end function SMIOLf_init


    !-----------------------------------------------------------------------
    !  routine SMIOLf_finalize
    !
    !> \brief Finalize a SMIOL context
    !> \details
    !>  Finalizes a SMIOL context and frees all memory in the SMIOL_context instance.
    !>  After this routine is called, no other SMIOL routines that make reference to
    !>  the finalized context should be called.
    !>
    !>  Upon return, the context argument will be unassociated if no errors occurred.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_finalize(context) result(ierr)

        use iso_c_binding, only : c_ptr, c_loc, c_associated, c_null_ptr

        implicit none

        type (SMIOLf_context), pointer :: context

        type (c_ptr) :: c_context = c_null_ptr

        ! C interface definitions
        interface
            function SMIOL_finalize(context) result(ierr) bind(C, name='SMIOL_finalize')
                use iso_c_binding, only : c_int, c_ptr
                type (c_ptr) :: context
                integer(kind=c_int) :: ierr
            end function
        end interface

        if (associated(context)) then
            c_context = c_loc(context)
        end if

        ierr = SMIOL_finalize(c_context)

        if (ierr == SMIOL_SUCCESS) then
            if (c_associated(c_context)) then
                ierr = -997     ! TODO: define an error code for this. The c_context should be null...
            else
                nullify(context)
            end if
        else
            if (.not. c_associated(c_context)) then
                nullify(context)
            end if
        end if

    end function SMIOLf_finalize


    !-----------------------------------------------------------------------
    !  routine SMIOLf_inquire
    !
    !> \brief Inquire about a SMIOL context
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_inquire() result(ierr)

        implicit none

        ierr = 0

    end function SMIOLf_inquire


    !
    ! File methods
    !

    !-----------------------------------------------------------------------
    !  routine SMIOLf_open_file
    !
    !> \brief Opens a file within a SMIOL context
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_open_file() result(ierr)

        implicit none

        ierr = 0

    end function SMIOLf_open_file


    !-----------------------------------------------------------------------
    !  routine SMIOLf_close_file
    !
    !> \brief Closes a file within a SMIOL context
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_close_file() result(ierr)

        implicit none

        ierr = 0

    end function SMIOLf_close_file



    !
    ! Dimension methods
    !

    !-----------------------------------------------------------------------
    !  routine SMIOLf_define_dim
    !
    !> \brief Defines a new dimension in a file
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_define_dim() result(ierr)

        implicit none

        ierr = 0

    end function SMIOLf_define_dim


    !-----------------------------------------------------------------------
    !  routine SMIOLf_inquire_dim
    !
    !> \brief Inquires about an existing dimension in a file
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_inquire_dim() result(ierr)

        implicit none

        ierr = 0

    end function SMIOLf_inquire_dim


    !
    ! Variable methods
    !

    !-----------------------------------------------------------------------
    !  routine SMIOLf_define_var
    !
    !> \brief Defines a new variable in a file
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_define_var() result(ierr)

        implicit none

        ierr = 0

    end function SMIOLf_define_var


    !-----------------------------------------------------------------------
    !  routine SMIOLf_inquire_var
    !
    !> \brief Inquires about an existing variable in a file
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_inquire_var() result(ierr)

        implicit none

        ierr = 0

    end function SMIOLf_inquire_var


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var
    !
    !> \brief Writes a variable to a file
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_put_var() result(ierr)

        implicit none

        ierr = 0

    end function SMIOLf_put_var


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var
    !
    !> \brief Reads a variable from a file
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_get_var() result(ierr)

        implicit none

        ierr = 0

    end function SMIOLf_get_var


    !
    ! Attribute methods
    !

    !-----------------------------------------------------------------------
    !  routine SMIOLf_define_att
    !
    !> \brief Defines a new attribute in a file
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_define_att() result(ierr)

        implicit none

        ierr = 0

    end function SMIOLf_define_att


    !-----------------------------------------------------------------------
    !  routine SMIOLf_inquire_att
    !
    !> \brief Inquires about an attribute in a file
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_inquire_att() result(ierr)

        implicit none

        ierr = 0

    end function SMIOLf_inquire_att


    !
    ! Control methods
    !

    !-----------------------------------------------------------------------
    !  routine SMIOLf_file_sync
    !
    !> \brief Forces all in-memory data to be flushed to disk
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_file_sync() result(ierr)

        implicit none

        ierr = 0

    end function SMIOLf_file_sync


    !-----------------------------------------------------------------------
    !  routine SMIOLf_error_string
    !
    !> \brief Returns an error string for a specified error code
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    character(len=128) function SMIOLf_error_string(ierrno) result(err_mesg)

        use iso_c_binding, only : c_ptr, c_char, c_null_char, c_f_pointer

        implicit none

        integer, intent(in) :: ierrno

        type (c_ptr) :: c_mesg_ptr
        character(kind=c_char), dimension(:), pointer :: c_mesg
        integer :: i
    
        ! C interface definitions
        interface
            function SMIOL_error_string(errno) result(err_mesg) bind(C, name='SMIOL_error_string')
                use iso_c_binding, only : c_int, c_ptr
                integer(kind=c_int), value :: errno
                type (c_ptr) :: err_mesg
            end function
        end interface

        c_mesg_ptr = SMIOL_error_string(ierrno)
        call c_f_pointer(c_mesg_ptr, c_mesg, shape=[len(err_mesg)])

        do i=1,len(err_mesg)
            if (c_mesg(i) == c_null_char) exit
        end do

        i = i - 1

        err_mesg(1:i) = transfer(c_mesg(1:i), err_mesg)
        err_mesg = err_mesg(1:i)

    end function SMIOLf_error_string


    !-----------------------------------------------------------------------
    !  routine SMIOLf_set_option
    !
    !> \brief Sets an option for the SMIOL library
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_set_option() result(ierr)

        implicit none

        ierr = 0

    end function SMIOLf_set_option


    !-----------------------------------------------------------------------
    !  routine SMIOLf_create_decomp
    !
    !> \brief Creates a mapping between compute elements and I/O elements
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_create_decomp(n_compute_elements, n_io_elements, compute_elements, io_elements, decomp) result(ierr)

        use iso_c_binding, only : c_size_t, c_int64_t
        use iso_c_binding, only : c_ptr, c_null_ptr, c_loc, c_f_pointer, c_associated

        implicit none

        integer(c_size_t), intent(in) :: n_compute_elements
        integer(c_size_t), intent(in) :: n_io_elements
        integer(c_int64_t), dimension(n_compute_elements), target, intent(in) :: compute_elements
        integer(c_int64_t), dimension(n_io_elements), target, intent(in) :: io_elements
        type(SMIOLf_decomp), pointer, intent(inout) :: decomp

        type(c_ptr) :: c_compute_elements = c_null_ptr
        type(c_ptr) :: c_io_elements = c_null_ptr
        type(c_ptr) :: c_decomp = c_null_ptr
        
        interface
            function SMIOL_create_decomp(n_compute_elements, n_io_elements, compute_elements, io_elements) & 
                                            result(decomp) bind(C, name='SMIOL_create_decomp')
                use iso_c_binding, only : c_size_t, c_ptr
                integer(c_size_t), value :: n_compute_elements
                integer(c_size_t), value :: n_io_elements
                type(c_ptr), value :: compute_elements
                type(c_ptr), value :: io_elements
                type(c_ptr) :: decomp
            end function
        end interface

        ierr = SMIOL_SUCCESS

        ! Translate Fortran types into C interoperable types
        c_compute_elements = c_loc(compute_elements)
        c_io_elements = c_loc(io_elements)

        ! Create SMIOL_decomp type via c SMIOL_create_decomp
        c_decomp = SMIOL_create_decomp(n_compute_elements, n_io_elements, c_compute_elements, c_io_elements)

        ! Error check and translate c_decomp ptr into Fortran SMIOLf_decomp 
        if (.not. c_associated(c_decomp)) then
            nullify(decomp)
            ierr = SMIOL_FORTRAN_ERROR
        else
            call c_f_pointer(c_decomp, decomp)
        endif

    end function SMIOLf_create_decomp


    !-----------------------------------------------------------------------
    !  routine SMIOLf_free_decomp
    !
    !> \brief Frees a mapping between compute elements and I/O elements
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_free_decomp() result(ierr)

        implicit none

        ierr = 0

    end function SMIOLf_free_decomp

end module SMIOLf
