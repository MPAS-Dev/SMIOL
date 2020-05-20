#include "smiol_codes.inc"

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! SMIOL -- The Simple MPAS I/O Library
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module SMIOLf

    use iso_c_binding, only : c_int, c_size_t, c_int64_t, c_ptr

    private

    public :: SMIOLf_context, &
              SMIOLf_decomp, &
              SMIOLf_file

    public :: SMIOL_offset_kind

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
              SMIOLf_sync_file, &
              SMIOLf_error_string, &
              SMIOLf_lib_error_string, &
              SMIOLf_set_option, &
              SMIOLf_create_decomp, &
              SMIOLf_free_decomp


    integer, parameter :: SMIOL_offset_kind = c_int64_t   ! Must match SMIOL_Offset in smiol_types.h


    type, bind(C) :: SMIOLf_context
        integer :: fcomm             ! Fortran handle to MPI communicator; MPI_Fint on the C side, which is supposed to match a Fortran integer
        integer(c_int) :: comm_size  ! Size of MPI communicator
        integer(c_int) :: comm_rank  ! Rank within MPI communicator

        integer(c_int) :: lib_ierr   ! Library-specific error code
        integer(c_int) :: lib_type   ! From which library the error code originated
    end type SMIOLf_context

    type, bind(C) :: SMIOLf_file
        type (c_ptr) :: context      ! Pointer to (struct SMIOL_context); the context within which the file was opened
#ifdef SMIOL_PNETCDF
        integer(c_int) :: state      ! parallel-netCDF file state (i.e. Define or data mode)
        integer(c_int) :: ncidp      ! parallel-netCDF file handle
#endif
    end type SMIOLf_file

    type, bind(C) :: SMIOLf_decomp
        !
        ! The lists below are structured (in C) as follows:
        !   list[0] - the number of neighbors for which a task sends/recvs
        !                                                                             |
        !   list[n] - neighbor task ID                                                | repeated for
        !   list[n+1] - number of elements, m, to send/recv to/from the neighbor      | each neighbor
        !   list[n+2 .. n+2+m] - local element IDs to send/recv to/from the neighbor  |
        !                                                                             |
        !
        type(c_ptr) :: comp_list  ! Elements to be sent/received from/on a compute task
        type(c_ptr) :: io_list    ! Elements to be send/received from/on an I/O task

        type (c_ptr) :: context   ! Pointer to (struct SMIOL_context); the context for this decomp
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
                ierr = SMIOL_FORTRAN_ERROR
            else
                call c_f_pointer(c_context, context)
            end if
        else
            if (.not. c_associated(c_context)) then
                nullify(context)
            else
                ierr = SMIOL_FORTRAN_ERROR
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
                ierr = SMIOL_FORTRAN_ERROR
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
    !>  Depending on the specified file mode, creates or opens the file specified
    !>  by filename within the provided SMIOL context.
    !>
    !>  Upon successful completion, SMIOL_SUCCESS is returned, and the file handle argument
    !>  will point to a valid file handle. Otherwise, the file handle is not associated
    !>  and an error code other than SMIOL_SUCCESS is returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_open_file(context, filename, mode, file) result(ierr)

        use iso_c_binding, only : c_loc, c_ptr, c_null_ptr, c_char, c_null_char, c_associated, c_f_pointer

        implicit none

        type (SMIOLf_context), pointer :: context
        character(len=*), intent(in) :: filename
        integer, intent(in) :: mode
        type (SMIOLf_file), pointer :: file

        type (c_ptr) :: c_context = c_null_ptr
        type (c_ptr) :: c_file = c_null_ptr
        integer(kind=c_int) :: c_mode
        character(kind=c_char), dimension(:), pointer :: c_filename => null()

        integer :: i

        ! C interface definitions
        interface
            function SMIOL_open_file(context, filename, mode, file) result(ierr) bind(C, name='SMIOL_open_file')
                use iso_c_binding, only : c_char, c_ptr, c_int
                type (c_ptr), value :: context
                character(kind=c_char), dimension(*) :: filename
                integer(kind=c_int), value :: mode
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

        c_mode = mode

        ierr = SMIOL_open_file(c_context, c_filename, c_mode, c_file)

        deallocate(c_filename)

        if (ierr == SMIOL_SUCCESS) then
            if (.not. c_associated(c_file)) then
                nullify(file)
                ierr = SMIOL_FORTRAN_ERROR
            else
                call c_f_pointer(c_file, file)
            end if
        else
            if (.not. c_associated(c_file)) then
                nullify(file)
            else
                ierr = SMIOL_FORTRAN_ERROR
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
                ierr = SMIOL_FORTRAN_ERROR
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
    !>  Defines a dimension with the specified name and size in the file associated
    !>  with the file handle. If a negative value is provided for the size argument,
    !>  the dimension will be defined as an unlimited or record dimension.
    !>
    !>  Upon successful completion, SMIOL_SUCCESS is returned; otherwise, an error
    !>  code is returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_define_dim(file, dimname, dimsize) result(ierr)

        use iso_c_binding, only : c_char, c_null_char, c_loc, c_ptr, c_null_ptr, c_associated

        implicit none

        type (SMIOLf_file), target :: file
        character(len=*), intent(in) :: dimname
        integer(kind=SMIOL_offset_kind), intent(in) :: dimsize

        type (c_ptr) :: c_file
        character(kind=c_char), dimension(:), pointer :: c_dimname

        integer :: i

        ! C interface definitions
        interface
            function SMIOL_define_dim(file, dimname, dimsize) result(ierr) bind(C, name='SMIOL_define_dim')
                use iso_c_binding, only : c_ptr, c_char, c_int
                import SMIOL_offset_kind
                type (c_ptr), value :: file
                character(kind=c_char), dimension(*) :: dimname
                integer(kind=SMIOL_offset_kind), value :: dimsize
                integer(kind=c_int) :: ierr
            end function
        end interface

        ! Get C address of file; there is no need to worry about an unassociated file here,
        ! since the file argument is not a pointer
        c_file = c_loc(file)

        !
        ! Convert Fortran string to C character array
        !
        allocate(c_dimname(len_trim(dimname) + 1))
        do i=1,len_trim(dimname)
            c_dimname(i) = dimname(i:i)
        end do
        c_dimname(i) = c_null_char

        ierr = SMIOL_define_dim(c_file, c_dimname, dimsize)

        deallocate(c_dimname)

    end function SMIOLf_define_dim


    !-----------------------------------------------------------------------
    !  routine SMIOLf_inquire_dim
    !
    !> \brief Inquires about an existing dimension in a file
    !> \details
    !>  Inquire about an existing dimension's size or if a dimension is the
    !>  unlimited dimension or not. If dimsize is present, the size of the dimension
    !>  will be returned in it; likewise, if is_unlimited is present, is_unlimited
    !>  will return either .true. or .false. depending on whether or not the dimension
    !>  is the unlimited dimension or not.
    !>
    !>  For unlimited dimensions, the current size of the dimension is returned;
    !>  future writes of additional records to a file can lead to different return
    !>  sizes for unlimited dimensions.
    !>
    !>  Upon successful completion, SMIOL_SUCCESS is returned; otherwise, an error
    !>  code is returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_inquire_dim(file, dimname, dimsize, is_unlimited) result(ierr)

        use iso_c_binding, only : c_char, c_null_char, c_loc, c_ptr, c_null_ptr, c_associated

        implicit none

        type (SMIOLf_file), target :: file
        character(len=*), intent(in) :: dimname
        integer(kind=SMIOL_offset_kind), intent(out), optional :: dimsize
        logical, intent(out), optional :: is_unlimited

        type (c_ptr) :: c_file
        character(kind=c_char), dimension(:), pointer :: c_dimname
        integer (kind=SMIOL_offset_kind), target :: c_dimsize
        integer (kind=c_int), target :: c_is_unlimited
        type (c_ptr) :: c_dimsize_ptr
        type (c_ptr) :: c_is_unlimited_ptr

        integer :: i

        ! C interface definitions
        interface
            function SMIOL_inquire_dim(file, dimname, dimsize, is_unlimited) result(ierr) bind(C, name='SMIOL_inquire_dim')
                use iso_c_binding, only : c_ptr, c_char, c_int
                import SMIOL_offset_kind
                type (c_ptr), value :: file
                character(kind=c_char), dimension(*) :: dimname
                type (c_ptr), value :: dimsize
                type (c_ptr), value :: is_unlimited
                integer(kind=c_int) :: ierr
            end function
        end interface

        ! Get C address of file; there is no need to worry about an unassociated file here,
        ! since the file argument is not a pointer
        c_file = c_loc(file)

        !
        ! Convert Fortran string to C character array
        !
        allocate(c_dimname(len_trim(dimname) + 1))
        do i=1,len_trim(dimname)
            c_dimname(i) = dimname(i:i)
        end do
        c_dimname(i) = c_null_char

        !
        ! Set C dimsize
        !
        if (present(dimsize)) then
            c_dimsize_ptr = c_loc(c_dimsize)
        else
            c_dimsize_ptr = c_null_ptr
        endif

        !
        ! Set C pointer for unlimited dimension inquiry argument
        !
        if (present(is_unlimited)) then
            c_is_unlimited_ptr = c_loc(c_is_unlimited)
        else
            c_is_unlimited_ptr = c_null_ptr
        end if

        ierr = SMIOL_inquire_dim(c_file, c_dimname, c_dimsize_ptr, c_is_unlimited_ptr)

        if (present(dimsize)) then
            dimsize = c_dimsize
        end if

        if (present(is_unlimited)) then
            if (c_is_unlimited == 1) then
                is_unlimited = .true.
            else
                is_unlimited = .false.
            end if
        end if

        deallocate(c_dimname)

    end function SMIOLf_inquire_dim


    !
    ! Variable methods
    !

    !-----------------------------------------------------------------------
    !  routine SMIOLf_define_var
    !
    !> \brief Defines a new variable in a file
    !> \details
    !>  Defines a variable with the specified name, type, and dimensions in an open
    !>  file pointed to by the file argument. The varname and dimnames arguments
    !>  are expected to be null-terminated strings, except if the variable has
    !>  zero dimensions, in which case the dimnames argument is ignored.
    !>
    !>  Upon successful completion, SMIOL_SUCCESS is returned; otherwise, an error
    !>  code is returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_define_var(file, varname, vartype, ndims, dimnames) result(ierr)

        use iso_c_binding, only : c_int, c_char, c_null_char, c_ptr, c_loc

        implicit none

        type (SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        integer, intent(in) :: vartype
        integer, intent(in) :: ndims
        character(len=*), dimension(:), intent(in) :: dimnames

        type (c_ptr) :: c_file
        character(kind=c_char), dimension(:), pointer :: c_varname
        integer(kind=c_int) :: c_vartype
        integer(kind=c_int) :: c_ndims

        type (c_ptr), dimension(:), allocatable, target :: c_dimnames

        integer :: i, j

        ! C interface definitions
        interface
            function SMIOL_define_var(file, varname, vartype, ndims, dimnames) result(ierr) bind(C, name='SMIOL_define_var')
                use iso_c_binding, only : c_ptr, c_char, c_int
                type (c_ptr), value :: file
                character(kind=c_char), dimension(*) :: varname
                integer(kind=c_int), value :: vartype
                integer(kind=c_int), value :: ndims
                type (c_ptr), value :: dimnames
                integer(kind=c_int) :: ierr
            end function
        end interface

        ! Used to store an array of pointers to character arrays
        type string_ptr
            character(kind=c_char), dimension(:), allocatable :: str
        end type string_ptr

        type (string_ptr), dimension(:), allocatable, target :: strings

        !
        ! Check that the 'dimnames' array has at least ndims elements
        !
        if (size(dimnames) < ndims) then
            ierr = SMIOL_FORTRAN_ERROR
            return
        end if

        ! Get C address of file; there is no need to worry about an unassociated file here,
        ! since the file argument is not a pointer
        c_file = c_loc(file)

        !
        ! Convert Fortran string to C character array
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! Convert vartype and ndims
        !
        c_vartype = vartype
        c_ndims = ndims

        !
        ! Convert dimnames
        !
        allocate(c_dimnames(ndims))
        allocate(strings(ndims))

        do j=1,ndims
            allocate(strings(j) % str(len_trim(dimnames(j))+1))

            do i=1,len_trim(dimnames(j))
                strings(j) % str(i) = dimnames(j)(i:i)
            end do
            strings(j) % str(i) = c_null_char
            c_dimnames(j) = c_loc(strings(j) % str)
        end do

        ierr = SMIOL_define_var(c_file, c_varname, c_vartype, c_ndims, c_loc(c_dimnames))

        do j=1,ndims
            deallocate(strings(j) % str)
        end do

        deallocate(c_varname)
        deallocate(strings)
        deallocate(c_dimnames)

    end function SMIOLf_define_var


    !-----------------------------------------------------------------------
    !  routine SMIOLf_inquire_var
    !
    !> \brief Inquires about an existing variable in a file
    !> \details
    !>  Inquires about a variable in a file, and optionally returns the type
    !>  of the variable, the dimensionality of the variable, and the names of
    !>  the dimensions of the variable.
    !>
    !>  If the names of a variable's dimensions are requested (by providing an
    !>  actual argument for dimnames), the size of the dimnames array must be at
    !>  least the number of dimensions in the variable, and each character string
    !>  in the dimnames array must be large enough to accommodate the corresponding
    !>  dimension name.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_inquire_var(file, varname, vartype, ndims, dimnames) result(ierr)

        use iso_c_binding, only : c_char, c_null_char, c_loc, c_ptr, c_null_ptr, c_int

        implicit none

        type (SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        integer, intent(out), optional :: vartype
        integer, intent(out), optional :: ndims
        character(len=*), dimension(:), intent(out), optional :: dimnames

        type (c_ptr) :: c_file
        character(kind=c_char), dimension(:), pointer :: c_varname
        integer(kind=c_int), target :: c_vartype
        integer(kind=c_int), target :: c_ndims
        type (c_ptr), dimension(:), allocatable, target :: c_dimnames

        type (c_ptr) :: c_vartype_ptr
        type (c_ptr) :: c_ndims_ptr
        type (c_ptr) :: c_dimnames_ptr

        integer :: i, j

        ! C interface definitions
        interface
            function SMIOL_inquire_var(file, varname, vartype, ndims, dimnames) result(ierr) bind(C, name='SMIOL_inquire_var')
                use iso_c_binding, only : c_ptr, c_char, c_int
                type (c_ptr), value :: file
                character(kind=c_char), dimension(*) :: varname
                type (c_ptr), value :: vartype
                type (c_ptr), value :: ndims
                type (c_ptr), value :: dimnames
                integer(kind=c_int) :: ierr
            end function
        end interface

        ! Used to store an array of pointers to character arrays
        type string_ptr
            character(kind=c_char), dimension(:), allocatable :: str
        end type string_ptr

        type (string_ptr), dimension(:), allocatable, target :: strings


        ! Get C address of file; there is no need to worry about an unassociated file here,
        ! since the file argument is not a pointer
        c_file = c_loc(file)

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! Set C pointer for variable type
        !
        if (present(vartype)) then
            c_vartype_ptr = c_loc(c_vartype)
        else
            c_vartype_ptr = c_null_ptr
        end if

        !
        ! Set C pointer for number of dimensions
        ! This is done even if dimnames is requested but ndims is not,
        ! since c_ndims may be used later on when copying out strings
        ! to dimnames.
        !
        if (present(ndims) .or. present(dimnames)) then
            c_ndims_ptr = c_loc(c_ndims)
        else
            c_ndims_ptr = c_null_ptr
        end if

        !
        ! Set C pointers for dimension names
        !
        if (present(dimnames)) then
            allocate(c_dimnames(size(dimnames)))
            allocate(strings(size(dimnames)))

            do j=1,size(dimnames)
                allocate(strings(j) % str(len(dimnames(j))+1))
                c_dimnames(j) = c_loc(strings(j) % str)
            end do
            c_dimnames_ptr = c_loc(c_dimnames)
        else
            c_dimnames_ptr = c_null_ptr
        end if


        ierr = SMIOL_inquire_var(c_file, c_varname, c_vartype_ptr, c_ndims_ptr, c_dimnames_ptr)

        deallocate(c_varname)

        if (ierr /= SMIOL_SUCCESS) then
            return
        end if

        !
        ! Copy variable type to output argument
        !
        if (present(vartype)) then
            vartype = c_vartype
        end if

        !
        ! Copy number of dimensions to output argument
        !
        if (present(ndims)) then
            ndims = c_ndims
        end if

        !
        ! Copy dimension names to output argument
        !
        if (present(dimnames)) then
            do j=1,c_ndims
                do i=1,len(dimnames(j))
                    if (strings(j) % str(i) == c_null_char) exit
                end do

                i = i - 1

                dimnames(j)(1:i) = transfer(strings(j) % str(1:i), dimnames(j))
                dimnames(j) = dimnames(j)(1:i)
            end do

            do j=1,size(dimnames)
                deallocate(strings(j) % str)
            end do
            deallocate(strings)
            deallocate(c_dimnames)
        end if

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
    !  routine SMIOLf_sync_file
    !
    !> \brief Forces all in-memory data to be flushed to disk
    !> \details
    !> Upon success, all in-memory data for the file associatd with the file
    !> handle will be flushed to the file system and SMIOL_SUCCESS will be
    !> returned; otherwise, an error code is returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_sync_file(file) result(ierr)

        use iso_c_binding, only : c_ptr, c_loc, c_null_ptr

        implicit none

        type (SMIOLf_file), pointer :: file
        type (c_ptr) :: c_file

        interface
            function SMIOL_sync_file(file) result(ierr) bind(C, name='SMIOL_sync_file')
                use iso_c_binding, only : c_ptr, c_int
                type(c_ptr), value :: file
                integer(kind=c_int) :: ierr
            end function
        end interface

        c_file = c_null_ptr

        if (associated(file)) then
            c_file = c_loc(file)
        end if

        ierr = SMIOL_sync_file(c_file)

    end function SMIOLf_sync_file


    !-----------------------------------------------------------------------
    !  routine SMIOLf_error_string
    !
    !> \brief Returns an error string for a specified error code
    !> \details
    !>  Returns an error string corresponding to a SMIOL error code. If the error code is
    !>  SMIOL_LIBRARY_ERROR and a valid SMIOL context is available, the SMIOLf_lib_error_string
    !>  function should be called instead.
    !>
    !>  The error string is always of length 128, and so it is recommended to trim
    !>  the string before it is printed.
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
    !  routine SMIOLf_lib_error_string
    !
    !> \brief Returns an error string for a third-party library called by SMIOL
    !> \details
    !>  Returns an error string corresponding to an error that was generated by
    !>  a third-party library that was called by SMIOL. The library that was the source
    !>  of the error, as well as the library-specific error code, are retrieved from
    !>  a SMIOL context. If successive library calls resulted in errors, only the error
    !>  string for the last of these errors will be returned. 
    !>
    !>  The error string is always of length 128, and so it is recommended to trim
    !>  the string before it is printed.
    !
    !-----------------------------------------------------------------------
    character(len=128) function SMIOLf_lib_error_string(context) result(err_mesg)

        use iso_c_binding, only : c_ptr, c_null_ptr, c_char, c_null_char, c_f_pointer, c_loc

        implicit none

        type (SMIOLf_context), target :: context

        type (c_ptr) :: c_context = c_null_ptr
        type (c_ptr) :: c_mesg_ptr = c_null_ptr
        character(kind=c_char), dimension(:), pointer :: c_mesg => null()
        integer :: i

        ! C interface definitions
        interface
            function SMIOL_lib_error_string(context) result(err_mesg) bind(C, name='SMIOL_lib_error_string')
                use iso_c_binding, only : c_ptr
                type(c_ptr), value :: context
                type (c_ptr) :: err_mesg
            end function
        end interface

        c_context = c_loc(context)

        c_mesg_ptr = SMIOL_lib_error_string(c_context)
        call c_f_pointer(c_mesg_ptr, c_mesg, shape=[len(err_mesg)])

        do i=1,len(err_mesg)
            if (c_mesg(i) == c_null_char) exit
        end do

        i = i - 1

        err_mesg(1:i) = transfer(c_mesg(1:i), err_mesg)
        err_mesg = err_mesg(1:i)

    end function SMIOLf_lib_error_string


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
    !>  Given arrays of global element IDs that each task computes and global
    !>  element IDs that each task reads/writes, this routine works out a mapping
    !>  of elements between compute and I/O tasks.
    !>
    !>  If all input arguments are determined to be valid and if the routine is
    !>  successful in working out a mapping, the decomp pointer is allocated
    !>  and given valid contents, and SMIOL_SUCCESS is returned; otherwise
    !>  a non-success error code is returned and the decomp pointer is unassociated.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_create_decomp(context, n_compute_elements, compute_elements, n_io_elements, io_elements, decomp) &
                                          result(ierr)

        use iso_c_binding, only : c_size_t, c_ptr, c_null_ptr, c_loc, c_f_pointer, c_associated

        implicit none

        ! Arguments
        type (SMIOLf_context), target, intent(in) :: context
        integer(kind=c_size_t), intent(in) :: n_compute_elements
        integer(kind=SMIOL_offset_kind), dimension(n_compute_elements), target, intent(in) :: compute_elements
        integer(kind=c_size_t), intent(in) :: n_io_elements
        integer(kind=SMIOL_offset_kind), dimension(n_io_elements), target, intent(in) :: io_elements
        type (SMIOLf_decomp), pointer, intent(inout) :: decomp

        ! Local variables
        type (c_ptr) :: c_context
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_compute_elements
        type (c_ptr) :: c_io_elements
        
        interface
            function SMIOL_create_decomp(context, n_compute_elements, compute_elements, n_io_elements, io_elements, decomp) &
                                         result(ierr) bind(C, name='SMIOL_create_decomp')
                use iso_c_binding, only : c_size_t, c_ptr, c_int
                type (c_ptr), value :: context
                integer(c_size_t), value :: n_compute_elements
                type (c_ptr), value :: compute_elements
                integer(c_size_t), value :: n_io_elements
                type (c_ptr), value :: io_elements
                integer(kind=c_int) :: ierr
                type (c_ptr) :: decomp
            end function
        end interface



        ! Get C pointers to Fortran types
        c_context = c_loc(context)
        c_compute_elements = c_loc(compute_elements)
        c_io_elements = c_loc(io_elements)

        c_decomp = c_null_ptr

        ierr = SMIOL_create_decomp(c_context, n_compute_elements, c_compute_elements, &
                                   n_io_elements, c_io_elements, c_decomp)

        ! Error check and translate c_decomp pointer into a Fortran SMIOLf_decomp pointer
        if (ierr == SMIOL_SUCCESS) then
            if (c_associated(c_decomp)) then
                call c_f_pointer(c_decomp, decomp)
            else
                nullify(decomp)
                ierr = SMIOL_FORTRAN_ERROR
            end if
        else
            nullify(decomp)
            if (c_associated(c_decomp)) then
                ierr = SMIOL_FORTRAN_ERROR
            endif
        end if

    end function SMIOLf_create_decomp


    !-----------------------------------------------------------------------
    !  routine SMIOLf_free_decomp
    !
    !> \brief Frees a mapping between compute elements and I/O elements
    !> \details
    !>  Frees all memory of a SMIOLf_decomp and returns SMIOL_SUCCESS. If 
    !>  decomp is unassociated, nothing will be done and SMIOL_SUCCESS will 
    !>  be returned. After this function has been called, no other SMIOL 
    !>  routines should use the freed SMIOL_decomp.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_free_decomp(decomp) result(ierr)

        use iso_c_binding, only : c_ptr, c_loc, c_associated, c_null_ptr

        implicit none

        type(SMIOLF_decomp), pointer, intent(inout) :: decomp
        type(c_ptr) :: c_decomp = c_null_ptr

        interface
            function SMIOL_free_decomp(decomp) result(ierr) bind(C, name='SMIOL_free_decomp')
                use iso_c_binding, only : c_ptr, c_int
                type(c_ptr) :: decomp
                integer(kind=c_int) :: ierr
            end function
        end interface

        ierr = SMIOL_SUCCESS

        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        endif
        ierr = SMIOL_free_decomp(c_decomp)

        if (ierr == SMIOL_SUCCESS) then
            if (c_associated(c_decomp)) then
                ierr = SMIOL_FORTRAN_ERROR
            else
                nullify(decomp)
            end if
        else
            if (.not. c_associated(c_decomp)) then
                nullify(decomp)
            end if
        end if

    end function SMIOLf_free_decomp

end module SMIOLf
