#include "smiol_codes.inc"

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! SMIOL -- The Simple MPAS I/O Library
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module SMIOLf

    use iso_c_binding, only : c_int, c_ptr

    private

    public :: SMIOLf_context, &
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
        type (c_ptr) :: context      ! Pointer to (struct SMIOL_context); the context within which the file was opened
#ifdef SMIOL_PNETCDF
        integer(c_int) :: ncidp      ! parallel-netCDF file handle
#endif
    end type SMIOLf_file

    type, bind(C) :: SMIOLf_decomp
        integer(c_int) :: i
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
    !>  Creates or opens the file specified by filename within the provided SMIOL
    !>  context.
    !>
    !>  Upon successful completion, SMIOL_SUCCESS is returned, and the file handle argument
    !>  will point to a valid file handle. Otherwise, the file handle is not associated
    !>  and an error code other than SMIOL_SUCCESS is returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_open_file(context, filename, file) result(ierr)

        use iso_c_binding, only : c_loc, c_ptr, c_null_ptr, c_char, c_null_char, c_associated, c_f_pointer

        implicit none

        type (SMIOLf_context), pointer :: context
        character(len=*), intent(in) :: filename
        type (SMIOLf_file), pointer :: file

        type (c_ptr) :: c_context = c_null_ptr
        type (c_ptr) :: c_file = c_null_ptr
        character(kind=c_char), dimension(:), pointer :: c_filename => null()

        integer :: i

        ! C interface definitions
        interface
            function SMIOL_open_file(context, filename, file) result(ierr) bind(C, name='SMIOL_open_file')
                use iso_c_binding, only : c_char, c_ptr, c_int
                type (c_ptr), value :: context
                character(kind=c_char), dimension(*) :: filename
                type (c_ptr) :: file
                integer(kind=c_int) :: ierr
            end function
        end interface

        if (associated(context)) then
            c_context = c_loc(context)
        end if

        !
        ! Convert Fortran string to C character array
        !
        allocate(c_filename(len_trim(filename) + 1))
        do i=1,len_trim(filename)
            c_filename(i) = filename(i:i)
        end do
        c_filename(i) = c_null_char

        ierr = SMIOL_open_file(c_context, c_filename, c_file)

        deallocate(c_filename)

        if (ierr == SMIOL_SUCCESS) then
            if (.not. c_associated(c_file)) then
                nullify(file)
                ierr = -997     ! TODO: define an error code for this. The c_file should not be null...
            else
                call c_f_pointer(c_file, file)
            end if
        else
            if (.not. c_associated(c_file)) then
                nullify(file)
            else
                ierr = -997     ! TODO: define an error code for this. The c_file should be null...
            end if
        end if

    end function SMIOLf_open_file


    !-----------------------------------------------------------------------
    !  routine SMIOLf_close_file
    !
    !> \brief Closes a file within a SMIOL context
    !> \details
    !>  Closes the file associated with the provided file handle. Upon successful
    !>  completion, SMIOL_SUCCESS is returned, the file will be closed, and all memory
    !>  that is uniquely associated with the file handle will be deallocated.
    !>  Otherwise, an error code other than SMIOL_SUCCESS will be returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_close_file(file) result(ierr)

        use iso_c_binding, only : c_loc, c_ptr, c_null_ptr, c_associated

        implicit none

        type (SMIOLf_file), pointer :: file

        type (c_ptr) :: c_file = c_null_ptr

        ! C interface definitions
        interface
            function SMIOL_close_file(file) result(ierr) bind(C, name='SMIOL_close_file')
                use iso_c_binding, only : c_ptr, c_int
                type (c_ptr) :: file
                integer(kind=c_int) :: ierr
            end function
        end interface

        if (associated(file)) then
            c_file = c_loc(file)
        end if

        ierr = SMIOL_close_file(c_file)

        if (ierr == SMIOL_SUCCESS) then
            if (c_associated(c_file)) then
                ierr = -997     ! TODO: define an error code for this. The c_file should be null...
            else
                nullify(file)
            end if
        else
            if (.not. c_associated(c_file)) then
                nullify(file)
            end if
        end if

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
    integer function SMIOLf_create_decomp() result(ierr)

        implicit none

        ierr = 0

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
