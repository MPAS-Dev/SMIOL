#include "smiol_codes.inc"

program smiol_runner

    use iso_c_binding, only : c_size_t
    use SMIOLf
    use mpi

    implicit none

    integer :: ierr
    integer :: i
    real :: f
    integer :: my_proc_id
    integer :: test_log = 42
    integer(kind=c_size_t) :: n_compute_elements = 1
    integer :: num_io_tasks, io_stride
    integer(kind=SMIOL_offset_kind), dimension(:), pointer :: compute_elements
    type (SMIOLf_decomp), pointer :: decomp => null()
    type (SMIOLf_context), pointer :: context => null()
    type (SMIOLf_file), pointer :: file => null()
    character(len=16) :: log_fname
    character(len=32), dimension(2) :: dimnames
    integer(kind=SMIOL_offset_kind) :: dimsize
    integer :: ndims

    call MPI_Init(ierr)
    if (ierr /= MPI_SUCCESS) then
        write(0,'(a)') 'Error: MPI_Init failed'
        stop 1
    end if

    call MPI_Comm_rank(MPI_COMM_WORLD, my_proc_id, ierr)
    if (ierr /= MPI_SUCCESS) then
        write(0,'(a)') 'Error: MPI_Comm_rank failed'
        stop 1
    end if

    write(log_fname, '(a,i4.4,a)') "smiolf.", my_proc_id, ".test"
    open(unit=test_log, file=log_fname, status='replace')

    !
    ! Unit tests for SMIOL_init and SMIOL_finalize
    !
    ierr = test_init_finalize(test_log)
    if (ierr == 0) then
        write(test_log,'(a)') 'All tests PASSED!'
        write(test_log,'(a)') ''
    else
        write(test_log,'(i3,a)') ierr, ' tests FAILED!'
        write(test_log,'(a)') ''
    end if

    !
    ! Unit tests for SMIOL_open_file and SMIOL_close_file
    !
    ierr = test_open_close(test_log)
    if (ierr == 0) then
        write(test_log,'(a)') 'All tests PASSED!'
        write(test_log,'(a)') ''
    else
        write(test_log,'(i3,a)') ierr, ' tests FAILED!'
        write(test_log,'(a)') ''
    end if


    !
    ! Unit tests for dimensions
    !
    ierr = test_dimensions(test_log)
    if (ierr == 0) then
        write(test_log,'(a)') 'All tests PASSED!'
        write(test_log,'(a)') ''
    else
        write(test_log,'(i3,a)') ierr, ' tests FAILED!'
        write(test_log,'(a)') ''
    end if


    !
    ! Unit tests for variables
    !
    ierr = test_variables(test_log)
    if (ierr == 0) then
        write(test_log,'(a)') 'All tests PASSED!'
        write(test_log,'(a)') ''
    else
        write(test_log,'(i3,a)') ierr, ' tests FAILED!'
        write(test_log,'(a)') ''
    end if


    !
    ! Unit tests for SMIOL_create_decomp and SMIOL_free_decomp
    !
    ierr = test_decomp(test_log)
    if (ierr == 0) then
        write(test_log,'(a)') 'All tests PASSED!'
        write(test_log,'(a)') ''
    else
        write(test_log,'(i3,a)') ierr, ' tests FAILED!'
        write(test_log,'(a)') ''
    endif

    !
    ! Unit tests for SMIOL_sync_file
    !
    ierr = test_file_sync(test_log)
    if (ierr == 0) then
        write(test_log,'(a)') 'All tests PASSED!'
        write(test_log,'(a)') ''
    else
        write(test_log,'(i3,a)') ierr, ' tests FAILED!'
        write(test_log,'(a)') ''
    endif

    if (SMIOLf_init(MPI_COMM_WORLD, context) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_init' was not called successfully"
        stop 1
    endif 

    if (.not. associated(context)) then
        write(test_log,'(a)') 'Error: SMIOLf_init returned an unassociated context'
        stop 1
    end if

    n_compute_elements = 100
    allocate(compute_elements(n_compute_elements))
    compute_elements(:) = 0

    num_io_tasks = 16
    io_stride = 4

    if (SMIOLf_create_decomp(context, n_compute_elements, compute_elements, &
                             num_io_tasks, io_stride, decomp) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "Error: SMIOLf_create_decomp was not called successfully"
        stop 1
    endif

    deallocate(compute_elements)

    if (SMIOLf_free_decomp(decomp) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "Error: SMIOLf_free_decomp was not called successfully"
        stop 1
    endif

    if (SMIOLf_inquire() /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_inquire' was not called successfully"
        stop 1
    endif

    if (SMIOLf_open_file(context, "blahf.nc", SMIOL_FILE_CREATE, file) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_open_file' was not called successfully"
        stop 1
    endif

    if (SMIOLf_define_dim(file, 'Time', -1_SMIOL_offset_kind) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_define_dim' was not called successfully"
        stop 1
    endif

    if (SMIOLf_sync_file(file) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_sync_file' was not called successfully"
        stop 1
    endif

    if (SMIOLf_define_dim(file, 'nCells', 40962_SMIOL_offset_kind) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_define_dim' was not called successfully"
        stop 1
    endif

    if (SMIOLf_inquire_dim(file, 'nCells', dimsize) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_inquire_dim' was not called successfully"
        stop 1
    endif
    write(test_log,'(a,i6)') 'Size of nCells dimension is ', dimsize

    if (SMIOLf_sync_file(file) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_sync_file' was not called successfully"
        stop 1
    endif

    dimnames(1) = 'Time'
    dimnames(2) = 'nCells'
    if (SMIOLf_define_var(file, 'theta', SMIOL_REAL32, 2, dimnames) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_define_var' was not called successfully"
        stop 1
    endif

    if (SMIOLf_inquire_var(file, 'theta', ndims=ndims) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_inquire_var' was not called successfully"
        stop 1
    endif

    i = 2
    if (SMIOLf_define_att(file, 'theta', 'time_levels', i) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_define_att' was not called successfully"
        stop 1
    endif

    f = 3.14159265
    if (SMIOLf_define_att(file, '', 'pi', f) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_define_att' was not called successfully"
        stop 1
    endif

    if (SMIOLf_define_att(file, '', 'title', 'MPAS-Atmosphere v7.0') /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_define_att' was not called successfully"
        stop 1
    endif

    if (SMIOLf_close_file(file) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_close_file' was not called successfully"
        stop 1
    endif

    if (SMIOLf_put_var() /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_put_var' was not called successfully"
        stop 1
    endif

    if (SMIOLf_get_var() /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_get_var' was not called successfully"
        stop 1
    endif

    if (SMIOLf_inquire_att() /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_inquire_att' was not called successfully"
        stop 1
    endif

    if (SMIOLf_set_option() /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_set_option' was not called successfully"
        stop 1
    endif

    write(test_log,'(a)') "Testing SMIOLf_error_string success: ", trim(SMIOLf_error_string(SMIOL_SUCCESS))
    write(test_log,'(a)') "Testing SMIOLf_error_string unkown error: ", trim(SMIOLf_error_string(1))
    write(test_log,'(a)') "Testing SMIOLf_error_string malloc returned a null pointer: ", &
               trim(SMIOLf_error_string(SMIOL_MALLOC_FAILURE))
    write(test_log,'(a)') "Testing SMIOL_error_string test invalid subroutine argument: ", &
               trim(SMIOLf_error_string(SMIOL_INVALID_ARGUMENT))
    write(test_log,'(a)') "Testing SMIOL_error_string test internal MPI call failed: ", &
               trim(SMIOLf_error_string(SMIOL_MPI_ERROR))
    write(test_log,'(a)') "Testing SMIOL_error_string test Fortran wrapper detected an inconsistency in C return values: ", &
               trim(SMIOLf_error_string(SMIOL_FORTRAN_ERROR))
    write(test_log,'(a)') "Testing SMIOL_error_string: bad return code from a library call: ", &
               trim(SMIOLf_error_string(SMIOL_LIBRARY_ERROR))
    write(test_log,'(a)') "Testing SMIOL_lib_error_string: Could not find matching library for the source of the error: ", &
               trim(SMIOLf_lib_error_string(context))

    if (SMIOLf_finalize(context) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_finalize' was not called successfully"
        stop 1
    endif

    if (associated(context)) then
        write(test_log,'(a)') 'Error: SMIOLf_finalize returned an associated context'
        stop 1
    end if

    close(test_log)

    call MPI_Finalize(ierr)
    if (ierr /= MPI_SUCCESS) then
        write(0,'(a)') 'Error: MPI_Finalize failed'
        stop 1
    end if

    stop 0


contains


    function test_init_finalize(test_log) result(ierrcount)

        implicit none

        integer, intent(in) :: test_log
        integer :: ierrcount
        type (SMIOLf_context), pointer :: context
        type (SMIOLf_context), pointer :: context_temp

        write(test_log,'(a)') '********************************************************************************'
        write(test_log,'(a)') '************ SMIOLf_init / SMIOLf_finalize unit tests **************************'
        write(test_log,'(a)') ''

        ierrcount = 0

        ! Invalid MPI communicator, and with an associated context that should be nullified
        write(test_log,'(a)',advance='no') 'Invalid MPI communicator (SMIOLf_init): '
        allocate(context_temp)
        context => context_temp
        ierr = SMIOLf_init(MPI_COMM_NULL, context)
        deallocate(context_temp)
        if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was returned, when an error was expected'
            ierrcount = ierrcount + 1
        else if (associated(context)) then
            write(test_log,'(a)') 'FAIL - an error code was returned, but context was not nullified'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'PASS'
        end if

        ! Handle unassociated context in SMIOL_finalize
        write(test_log,'(a)',advance='no') 'Handle unassociated context (SMIOLf_finalize): '
        nullify(context)
        ierr = SMIOLf_finalize(context)
        if (ierr == SMIOL_SUCCESS .and. .not. associated(context)) then
            write(test_log,'(a)') 'PASS'
        else if (associated(context)) then
            write(test_log,'(a)') 'FAIL - context is associated'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - context is unassociated as expected, but SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if

        ! Everything OK for SMIOLf_init
        write(test_log,'(a)',advance='no') 'Everything OK (SMIOLf_init): '
        nullify(context)
        ierr = SMIOLf_init(MPI_COMM_WORLD, context)
        if (ierr == SMIOL_SUCCESS .and. associated(context)) then
            write(test_log,'(a)') 'PASS'
        else if (ierr == SMIOL_SUCCESS .and. .not. associated(context)) then    ! May not be possible at present...
            write(test_log,'(a)') 'FAIL - context is not associated, although SMIOL_SUCCESS was returned'
            ierrcount = ierrcount + 1
        else if (ierr /= SMIOL_SUCCESS .and. associated(context)) then
            write(test_log,'(a)') 'FAIL - context is associated as expected, but SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - context is not associated, and SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if

        ! Everything OK for SMIOLf_finalize
        write(test_log,'(a)',advance='no') 'Everything OK (SMIOLf_finalize): '
        ierr = SMIOLf_finalize(context)
        if (ierr == SMIOL_SUCCESS .and. .not. associated(context)) then
            write(test_log,'(a)') 'PASS'
        else if (associated(context)) then
            write(test_log,'(a)') 'FAIL - context is associated'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - context is not associated as expected, but SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if

        write(test_log,'(a)') ''

    end function test_init_finalize


    function test_open_close(test_log) result(ierrcount)
#if 0
        use iso_c_binding, only : c_loc
#endif

        implicit none

        integer, intent(in) :: test_log
        integer :: ierrcount
        type (SMIOLf_context), pointer :: context
        type (SMIOLf_file), pointer :: file

        write(test_log,'(a)') '********************************************************************************'
        write(test_log,'(a)') '************ SMIOL_open_file / SMIOL_close_file unit tests *********************'
        write(test_log,'(a)') ''

        ierrcount = 0


        ! Create a SMIOL context for testing file open/close routines
        nullify(context)
        ierr = SMIOLf_init(MPI_COMM_WORLD, context)
        if (ierr /= SMIOL_SUCCESS .or. .not. associated(context)) then
            ierrcount = -1
            return
        end if

        ! Try to open a file with an invalid mode
        write(test_log,'(a)',advance='no') 'Try to open a file with an invalid mode: '
        nullify(file)
        ierr = SMIOLf_open_file(context, 'smiol_invalid.nc', &
                                not(ior(ior(SMIOL_FILE_CREATE, SMIOL_FILE_WRITE), SMIOL_FILE_READ)), file)
        if (ierr == SMIOL_INVALID_ARGUMENT .and. .not. associated(file)) then
            write(test_log,'(a)') 'PASS'
        else
            write(test_log,'(a)') 'FAIL - expected error code of SMIOL_INVALID_ARGUMENT not returned, or file handle is associated'
            ierrcount = ierrcount + 1
        end if

#ifdef SMIOL_PNETCDF
        ! Try to create a file for which we should not have sufficient permissions
        write(test_log,'(a)',advance='no') 'Try to create a file with insufficient permissions: '
        nullify(file)
        ierr = SMIOLf_open_file(context, '/smiol_test.nc', SMIOL_FILE_CREATE, file)
        if (ierr == SMIOL_LIBRARY_ERROR .and. .not. associated(file)) then
            write(test_log,'(a)') 'PASS ('//trim(SMIOLf_lib_error_string(context))//')'
        else
            write(test_log,'(a)') 'FAIL - expected error code of SMIOL_LIBRARY_ERROR not returned, or file handle is associated'
            ierrcount = ierrcount + 1
        end if

        ! Try to open a file that does not exist
        write(test_log,'(a)',advance='no') 'Try to open a non-existent file: '
        nullify(file)
        ierr = SMIOLf_open_file(context, '/smiol_foobar.nc', SMIOL_FILE_READ, file)
        if (ierr == SMIOL_LIBRARY_ERROR .and. .not. associated(file)) then
            write(test_log,'(a)') 'PASS ('//trim(SMIOLf_lib_error_string(context))//')'
        else
            write(test_log,'(a)') 'FAIL - expected error code of SMIOL_LIBRARY_ERROR not returned, or file handle is associated'
            ierrcount = ierrcount + 1
        end if

#if 0
!
! This test will not work under most compilers:
! * The flang compiler complains that file % context requires a reference to a TYPE(C_PTR)
! * The Intel compiler generates a double-free error when SMIOL_close_file tries to free
!   the 'file' allocated by Fortran
!
        ! Try to close a file that was never opened
        write(test_log,'(a)',advance='no') 'Try to close a file that was never opened: '
        allocate(file)
        file % context = c_loc(context)
        ierr = SMIOLf_close_file(file)
        if (associated(file)) deallocate(file)
        if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'PASS ('//trim(SMIOLf_lib_error_string(context))//')'
        else
            write(test_log,'(a)') 'FAIL - expected error code of SMIOL_LIBRARY_ERROR not returned'
            ierrcount = ierrcount + 1
        end if
#endif

        ! Create a file to be closed and opened again
        write(test_log,'(a)',advance='no') 'Create a file to be closed and later re-opened: '
        nullify(file)
        ierr = SMIOLf_open_file(context, 'pnetcdf_test_f.nc', SMIOL_FILE_CREATE, file)
        if (ierr == SMIOL_SUCCESS .and. associated(file)) then
            write(test_log,'(a)') 'PASS'
        else
            write(test_log,'(a)') 'FAIL ('//trim(SMIOLf_lib_error_string(context))//') - failed to create a new file'
            ierrcount = ierrcount + 1
        end if

        ! Close the file that was just created
        write(test_log,'(a)',advance='no') 'Close the file that was just created: '
        ierr = SMIOLf_close_file(file)
        if (ierr == SMIOL_SUCCESS .and. .not. associated(file)) then
            write(test_log,'(a)') 'PASS'
        else
            write(test_log,'(a)') 'FAIL - file handle is associated or SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if

        ! Re-open the file with read access
        write(test_log,'(a)',advance='no') 'Re-open file with read access: '
        nullify(file)
        ierr = SMIOLf_open_file(context, 'pnetcdf_test_f.nc', SMIOL_FILE_READ, file)
        if (ierr == SMIOL_SUCCESS .and. associated(file)) then
            write(test_log,'(a)') 'PASS'
        else
            write(test_log,'(a)') 'FAIL ('//trim(SMIOLf_lib_error_string(context))//') - failed to re-open existing file'
            ierrcount = ierrcount + 1
        end if

        ! Close the file
        write(test_log,'(a)',advance='no') 'Close the file: '
        ierr = SMIOLf_close_file(file)
        if (ierr == SMIOL_SUCCESS .and. .not. associated(file)) then
            write(test_log,'(a)') 'PASS'
        else
            write(test_log,'(a)') 'FAIL - file handle is associated or SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if

        ! Re-open the file with write access
        write(test_log,'(a)',advance='no') 'Re-open file with write access: '
        nullify(file)
        ierr = SMIOLf_open_file(context, 'pnetcdf_test_f.nc', SMIOL_FILE_WRITE, file)
        if (ierr == SMIOL_SUCCESS .and. associated(file)) then
            write(test_log,'(a)') 'PASS'
        else
            write(test_log,'(a)') 'FAIL ('//trim(SMIOLf_lib_error_string(context))//') - failed to re-open existing file'
            ierrcount = ierrcount + 1
        end if

        ! Close the file
        write(test_log,'(a)',advance='no') 'Close the file: '
        ierr = SMIOLf_close_file(file)
        if (ierr == SMIOL_SUCCESS .and. .not. associated(file)) then
            write(test_log,'(a)') 'PASS'
        else
            write(test_log,'(a)') 'FAIL - file handle is associated or SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if
#endif

        ! Everything OK (SMIOLf_open_file)
        write(test_log,'(a)',advance='no') 'Everything OK (SMIOLf_open_file): '
        nullify(file)
        ierr = SMIOLf_open_file(context, 'test_fortran.nc', SMIOL_FILE_CREATE, file)
        if (ierr == SMIOL_SUCCESS .and. associated(file)) then
            write(test_log,'(a)') 'PASS'
        else
            write(test_log,'(a)') 'FAIL - file handle is not associated or SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if

        ! Everything OK (SMIOLf_close_file)
        write(test_log,'(a)',advance='no') 'Everything OK (SMIOLf_close_file): '
        ierr = SMIOLf_close_file(file)
        if (ierr == SMIOL_SUCCESS .and. .not. associated(file)) then
            write(test_log,'(a)') 'PASS'
        else
            write(test_log,'(a)') 'FAIL - file handle is associated or SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if

        ! Free the SMIOL context
        ierr = SMIOLf_finalize(context)
        if (ierr /= SMIOL_SUCCESS .or. associated(context)) then
            ierrcount = -1
            return
        end if

        write(test_log,'(a)') ''

    end function test_open_close


    function test_decomp(test_log) result(ierrcount)

        use iso_c_binding, only : c_size_t

        implicit none

        integer, intent(in) :: test_log
        integer :: ierrcount
        integer :: comm_size
        integer :: comm_rank
        integer(kind=c_size_t) :: n_compute_elements
        integer(kind=c_size_t) :: i
        integer(kind=SMIOL_offset_kind), dimension(:), pointer :: compute_elements
        type (SMIOLf_context), pointer :: context
        type (SMIOLf_decomp), pointer :: decomp => null()
        logical :: matched

        write(test_log,'(a)') '********************************************************************************'
        write(test_log,'(a)') '************ SMIOLf_create_decomp / SMIOLf_free_decomp tests *******************'
        write(test_log,'(a)') ''

        ierrcount = 0

        call MPI_Comm_rank(MPI_COMM_WORLD, comm_rank, ierr)
        if (ierr /= MPI_SUCCESS) then
            write(test_log, '(a)') 'Failed to get MPI rank...'
            ierrcount = -1
            return
        end if

        call MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierr)
        if (ierr /= MPI_SUCCESS) then
            write(test_log, '(a)') 'Failed to get MPI size...'
            ierrcount = -1
            return
        end if

        ! Create a SMIOL context for testing decomp routines
        nullify(context)
        ierr = SMIOLf_init(MPI_COMM_WORLD, context)
        if (ierr /= SMIOL_SUCCESS .or. .not. associated(context)) then
            ierrcount = -1
            return
        end if

        ! Test with 0 elements
        write(test_log,'(a)',advance='no') 'Everything OK for SMIOLf_create_decomp with 0 elements: '
        n_compute_elements = 0
        allocate(compute_elements(n_compute_elements))
        ierr = SMIOLf_create_decomp(context, n_compute_elements, compute_elements, comm_size, 1, decomp)
        if (ierr == SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "PASS"
        else
            write(test_log, '(a)') "FAIL - Either SMIOL_SUCCESS was not returned or decomp was not associated"
            ierrcount = ierrcount + 1
        endif

        deallocate(compute_elements)

        ! Free Decomp
        write(test_log,'(a)',advance='no') 'Everything OK for SMIOLf_free_decomp with 0 elements: '
        ierr = SMIOLf_free_decomp(decomp)
        if (ierr == SMIOL_SUCCESS .and. .not. associated(decomp)) then
            write(test_log,'(a)') "PASS"
        else if (ierr /= SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "FAIL - ierr did not return SMIOL_SUCCESS, and decomp was still associated"
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "FAIL - ierr returned SMIOL_SUCCESS but the decomp was associated"
            ierrcount = ierrcount + 1
        endif

        ! Small number of Compute and IO Elements
        write(test_log,'(a)',advance='no') 'Everything OK for SMIOLf_create_decomp 1 element: '
        n_compute_elements = 1
        allocate(compute_elements(n_compute_elements))
        compute_elements(:) = 0
        ierr = SMIOLf_create_decomp(context, n_compute_elements, compute_elements, comm_size, 1, decomp)
        if (ierr == SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "PASS"
        else
            write(test_log, '(a)') "FAIL - Either SMIOL_SUCCESS was not returned or decomp was not associated"
            ierrcount = ierrcount + 1
        endif

        deallocate(compute_elements)

        ! Free Decomp
        write(test_log,'(a)',advance='no') 'Everything OK for SMIOLf_free_decomp with 1 element: '
        ierr = SMIOLf_free_decomp(decomp)
        if (ierr == SMIOL_SUCCESS .and. .not. associated(decomp)) then
            write(test_log,'(a)') "PASS"
        else if (ierr /= SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "FAIL - ierr did not return SMIOL_SUCCESS, and decomp was still associated"
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "FAIL - ierr returned SMIOL_SUCCESS but the decomp was associated"
            ierrcount = ierrcount + 1
        endif

        ! Large number of Compute and IO Elements
        write(test_log,'(a)',advance='no') 'Everything OK for SMIOLf_create_decomp large number of elements: '
        n_compute_elements = 1000000
        allocate(compute_elements(n_compute_elements))
        compute_elements(:) = 0
        ierr = SMIOLf_create_decomp(context, n_compute_elements, compute_elements, comm_size, 1, decomp)
        if (ierr == SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "PASS"
        else
            write(test_log, '(a)') "FAIL - Either SMIOL_SUCCESS was not returned or decomp was not associated"
            ierrcount = ierrcount + 1
        endif

        deallocate(compute_elements)

        ! Free Decomp
        write(test_log,'(a)',advance='no') 'Everything OK for SMIOLf_free_decomp large number of elements: '
        ierr = SMIOLf_free_decomp(decomp)
        if (ierr == SMIOL_SUCCESS .and. .not. associated(decomp)) then
            write(test_log,'(a)') "PASS"
        else if (ierr /= SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "FAIL - ierr did not return SMIOL_SUCCESS, and decomp was still associated"
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "FAIL - ierr returned SMIOL_SUCCESS but the decomp was associated"
            ierrcount = ierrcount + 1
        endif

        ! Pass SMIOLf_free_decomp a decomp that has already been freed
        write(test_log,'(a)',advance='no') 'Everything OK for SMIOLf_free_decomp on an unassociated decomp: '
        ierr = SMIOLf_free_decomp(decomp)
        if (ierr == SMIOL_SUCCESS .and. .not. associated(decomp)) then
            write(test_log,'(a)') "PASS"
        else if (ierr /= SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "FAIL - ierr did not return SMIOL_SUCCESS, and decomp became associated..."
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "FAIL - ierr returned SMIOL_SUCCESS, but the decomp became associated..."
            ierrcount = ierrcount + 1
        endif

        !
        ! The following tests will only be run if there are exactly two MPI tasks.
        ! In principle, as long as there are at least two MPI ranks in MPI_COMM_WORLD,
        ! an intracommunicator with exactly two ranks could be created for these tests.
        !
        if (comm_size == 2) then

            ! Odd/even compute, half/half I/O
            write(test_log,'(a)',advance='no') 'Odd/even compute, half/half I/O: '
            n_compute_elements = 4
            allocate(compute_elements(n_compute_elements))

            if (comm_rank == 0) then
                do i = 1, n_compute_elements
                    compute_elements(i) = 2 * (i-1) + 1      ! Odd elements
                end do
            else
                do i = 1, n_compute_elements
                    compute_elements(i) = 2 * (i-1)          ! Even elements
                end do
            end if

            ierr = SMIOLf_create_decomp(context, n_compute_elements, compute_elements, 2, 1, decomp)
            if (ierr == SMIOL_SUCCESS .and. associated(decomp)) then

                ! The correct comp_list and io_list arrays, below, were verified manually
                if (comm_rank == 0) then
                    matched = compare_decomps(decomp, &
                                              [ integer(kind=SMIOL_offset_kind) :: 2, 0, 2, 0, 1, 1, 2, 2, 3 ], &
                                              [ integer(kind=SMIOL_offset_kind) :: 2, 0, 2, 1, 3, 1, 2, 0, 2 ])
                else
                    matched = compare_decomps(decomp, &
                                              [ integer(kind=SMIOL_offset_kind) :: 2, 0, 2, 0, 1, 1, 2, 2, 3 ], &
                                              [ integer(kind=SMIOL_offset_kind) :: 2, 0, 2, 1, 3, 1, 2, 0, 2 ])
                end if

                if (matched) then
                    write(test_log,'(a)') 'PASS'
                else
                    write(test_log,'(a)') 'FAIL - the decomp did not contain the expected values'
                    ierrcount = ierrcount + 1
                end if
            else
                write(test_log, '(a)') 'FAIL - Either SMIOL_SUCCESS was not returned or decomp was not associated'
                ierrcount = ierrcount + 1
            end if

            deallocate(compute_elements)

            ierr = SMIOLf_free_decomp(decomp)
            if (ierr /= SMIOL_SUCCESS .or. associated(decomp)) then
                write(test_log,'(a)') 'After previous unit test, SMIOL_free_decomp was unsuccessful: FAIL'
                ierrcount = ierrcount + 1
            end if


            ! Even/odd compute, all/nothing I/O
            write(test_log,'(a)',advance='no') 'Even/odd compute, all/nothing I/O: '
            n_compute_elements = 4
            allocate(compute_elements(n_compute_elements))

            if (comm_rank == 0) then
                do i = 1, n_compute_elements
                    compute_elements(i) = 2 * (i-1)          ! Even elements
                end do
            else
                do i = 1, n_compute_elements
                    compute_elements(i) = 2 * (i-1) + 1      ! Odd elements
                end do
            end if

            ierr = SMIOLf_create_decomp(context, n_compute_elements, compute_elements, 1, 2, decomp)
            if (ierr == SMIOL_SUCCESS .and. associated(decomp)) then

                ! The correct comp_list and io_list arrays, below, were verified manually
                if (comm_rank == 0) then
                    matched = compare_decomps(decomp, &
                                              [ integer(kind=SMIOL_offset_kind) :: 1, 0, 4, 0, 1, 2, 3 ], &
                                              [ integer(kind=SMIOL_offset_kind) :: 2, 0, 4, 0, 2, 4, 6, 1, 4, 1, 3, 5, 7 ])
                else
                    matched = compare_decomps(decomp, &
                                              [ integer(kind=SMIOL_offset_kind) :: 1, 0, 4, 0, 1, 2, 3 ], &
                                              [ integer(kind=SMIOL_offset_kind) :: 0 ])
                end if

                if (matched) then
                    write(test_log,'(a)') 'PASS'
                else
                    write(test_log,'(a)') 'FAIL - the decomp did not contain the expected values'
                    ierrcount = ierrcount + 1
                end if
            else
                write(test_log, '(a)') 'FAIL - Either SMIOL_SUCCESS was not returned or decomp was not associated'
                ierrcount = ierrcount + 1
            end if

            deallocate(compute_elements)

            ierr = SMIOLf_free_decomp(decomp)
            if (ierr /= SMIOL_SUCCESS .or. associated(decomp)) then
                write(test_log,'(a)') 'After previous unit test, SMIOL_free_decomp was unsuccessful: FAIL'
                ierrcount = ierrcount + 1
            end if


            ! Nothing/all compute, all/nothing I/O
            write(test_log,'(a)',advance='no') 'Nothing/all compute, all/nothing I/O: '
            if (comm_rank == 0) then
                n_compute_elements = 0
            else
                n_compute_elements = 8
            end if
            allocate(compute_elements(n_compute_elements))

            if (comm_rank == 0) then
                ! No compute elements
            else
                do i = 1, n_compute_elements
                    compute_elements(i) = (n_compute_elements - i)      ! All compute elements
                end do
            end if

            ierr = SMIOLf_create_decomp(context, n_compute_elements, compute_elements, 1, 2, decomp)
            if (ierr == SMIOL_SUCCESS .and. associated(decomp)) then

                ! The correct comp_list and io_list arrays, below, were verified manually
                if (comm_rank == 0) then
                    matched = compare_decomps(decomp, &
                                              [ integer(kind=SMIOL_offset_kind) :: 0 ], &
                                              [ integer(kind=SMIOL_offset_kind) :: 1, 1, 8, 0, 1, 2, 3, 4, 5, 6, 7 ])
                else
                    matched = compare_decomps(decomp, &
                                              [ integer(kind=SMIOL_offset_kind) :: 1, 0, 8, 7, 6, 5, 4, 3, 2, 1, 0 ], &
                                              [ integer(kind=SMIOL_offset_kind) :: 0 ])
                end if

                if (matched) then
                    write(test_log,'(a)') 'PASS'
                else
                    write(test_log,'(a)') 'FAIL - the decomp did not contain the expected values'
                    ierrcount = ierrcount + 1
                end if
            else
                write(test_log, '(a)') 'FAIL - Either SMIOL_SUCCESS was not returned or decomp was not associated'
                ierrcount = ierrcount + 1
            end if

            deallocate(compute_elements)

            ierr = SMIOLf_free_decomp(decomp)
            if (ierr /= SMIOL_SUCCESS .or. associated(decomp)) then
                write(test_log,'(a)') 'After previous unit test, SMIOL_free_decomp was unsuccessful: FAIL'
                ierrcount = ierrcount + 1
            end if
        else
            write(test_log, '(a)') '<<< Tests that require exactly 2 MPI tasks will not be run >>>'
        end if

        ! Free the SMIOL context
        ierr = SMIOLf_finalize(context)
        if (ierr /= SMIOL_SUCCESS .or. associated(context)) then
            ierrcount = -1
            return
        end if

        write(test_log,'(a)') ''

    end function test_decomp

    function test_dimensions(test_log) result(ierrcount)

        implicit none

        integer, intent(in) :: test_log
        integer :: ierrcount
        type (SMIOLf_context), pointer :: context
        type (SMIOLf_file), pointer :: file
#if 0
        type (SMIOLf_file), pointer :: null_file
#endif
        integer(kind=SMIOL_offset_kind) :: dimsize
        integer(kind=SMIOL_offset_kind) :: expected_dimsize

        write(test_log,'(a)') '********************************************************************************'
        write(test_log,'(a)') '************ SMIOL_define_dim / SMIOL_inquire_dim unit tests *******************'
        write(test_log,'(a)') ''

        ierrcount = 0


        ! Create a SMIOL context for testing file dimension routines
        nullify(context)
        ierr = SMIOLf_init(MPI_COMM_WORLD, context)
        if (ierr /= SMIOL_SUCCESS .or. .not. associated(context)) then
            write(test_log,'(a)') 'Failed to create SMIOL context...'
            ierrcount = -1
            return
        end if

        ! Create a SMIOL file for testing dimension routines
        nullify(file)
        ierr = SMIOLf_open_file(context, 'test_dims_fortran.nc', SMIOL_FILE_CREATE, file)
        if (ierr /= SMIOL_SUCCESS .or. .not. associated(file)) then
            write(test_log,'(a)') 'Failed to create SMIOL file...'
            ierrcount = -1
            return
        end if

#if 0
        ! Handle unassociated file handle
        write(test_log,'(a)',advance='no') 'Handle unassociated file handle (SMIOLf_define_dim): '
        nullify(null_file)
        ierr = SMIOLf_define_dim(null_file, 'invalid_dim', 42_SMIOL_offset_kind)
        if (ierr /= SMIOL_SUCCESS) then
            write(test_log,'(a)') 'PASS'
        else
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was returned, when an error was expected'
            ierrcount = ierrcount + 1
        end if
#endif

        ! Everything OK for SMIOL_define_dim, unlimited dimension
        write(test_log,'(a)',advance='no') 'Everything OK - unlimited dimension (SMIOLf_define_dim): '
        ierr = SMIOLf_define_dim(file, 'Time', -1_SMIOL_offset_kind)
        if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'PASS'
        else
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if

        ! Everything OK for SMIOL_define_dim, small non-record dimension
        write(test_log,'(a)',advance='no') 'Everything OK - small non-record dimension (SMIOLf_define_dim): '
        ierr = SMIOLf_define_dim(file, 'nCells', 40962_SMIOL_offset_kind)
        if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'PASS'
        else
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if

        ! Everything OK for SMIOL_define_dim, large non-record dimension
        write(test_log,'(a)',advance='no') 'Everything OK - large non-record dimension (SMIOLf_define_dim): '
        ierr = SMIOLf_define_dim(file, 'nElements', 99999999999_SMIOL_offset_kind)
        if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'PASS'
        else
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if

        ! Handle undefined dimension
        write(test_log,'(a)',advance='no') 'Handle undefined dimension (SMIOLf_inquire_dim): '
        ierr = SMIOLf_inquire_dim(file, 'foobar', dimsize)
#ifdef SMIOL_PNETCDF
        if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'PASS ('//trim(SMIOLf_lib_error_string(context))//')'
        else
            write(test_log,'(a)') 'FAIL - SMIOL_LIBRARY_ERROR was not returned'
            ierrcount = ierrcount + 1
        end if
#else
        if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'PASS'
        else
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was not returned for no-op implementation of SMIOLf_inquire_dim'
            ierrcount = ierrcount + 1
        end if
#endif

        ! Everything OK for SMIOL_inquire_dim, unlimited dimension
        write(test_log,'(a)',advance='no') 'Everything OK - unlimited dimension (SMIOLf_inquire_dim): '
        dimsize = 0_SMIOL_offset_kind
        ierr = SMIOLf_inquire_dim(file, 'Time', dimsize)
        if (ierr == SMIOL_SUCCESS) then
            if (dimsize == 0_SMIOL_offset_kind) then
                write(test_log,'(a)') 'PASS'
            else
                write(test_log,'(a,a,i11,a,i11)') 'FAIL - SMIOL_SUCCESS was returned, but the dimension size is wrong', &
                               ' (got ', dimsize, ', expected ', 0_SMIOL_offset_kind, ')'
                ierrcount = ierrcount + 1
            end if
        else
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if

        ! Everything OK for SMIOL_inquire_dim, small non-record dimension
        write(test_log,'(a)',advance='no') 'Everything OK - small non-record dimension (SMIOLf_inquire_dim): '
        dimsize = 0_SMIOL_offset_kind
#ifdef SMIOL_PNETCDF
        expected_dimsize = 40962_SMIOL_offset_kind
#else
        expected_dimsize = 0_SMIOL_offset_kind
#endif
        ierr = SMIOLf_inquire_dim(file, 'nCells', dimsize)
        if (ierr == SMIOL_SUCCESS) then
            if (dimsize == expected_dimsize) then
                write(test_log,'(a)') 'PASS'
            else
                write(test_log,'(a,a,i11,a,i11)') 'FAIL - SMIOL_SUCCESS was returned, but the dimension size is wrong', &
                               ' (got ', dimsize, ', expected ', expected_dimsize, ')'
                ierrcount = ierrcount + 1
            end if
        else
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if

        ! Everything OK for SMIOL_inquire_dim, large non-record dimension
        write(test_log,'(a)',advance='no') 'Everything OK - large non-record dimension (SMIOLf_inquire_dim): '
        dimsize = 0_SMIOL_offset_kind
#ifdef SMIOL_PNETCDF
        expected_dimsize = 99999999999_SMIOL_offset_kind
#else
        expected_dimsize = 0_SMIOL_offset_kind
#endif
        ierr = SMIOLf_inquire_dim(file, 'nElements', dimsize)
        if (ierr == SMIOL_SUCCESS) then
            if (dimsize == expected_dimsize) then
                write(test_log,'(a)') 'PASS'
            else
                write(test_log,'(a,a,i11,a,i11)') 'FAIL - SMIOL_SUCCESS was returned, but the dimension size is wrong', &
                               ' (got ', dimsize, ', expected ', expected_dimsize, ')'
                ierrcount = ierrcount + 1
            end if
        else
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if

        ! Close the SMIOL file
        ierr = SMIOLf_close_file(file)
        if (ierr /= SMIOL_SUCCESS .or. associated(file)) then
            write(test_log,'(a)') 'Failed to close SMIOL file...'
            ierrcount = -1
            return
        end if

        ! Re-open the SMIOL file
        nullify(file)
        ierr = SMIOLf_open_file(context, 'test_dims_fortran.nc', SMIOL_FILE_READ, file)
        if (ierr /= SMIOL_SUCCESS .or. .not. associated(file)) then
            write(test_log,'(a)') 'Failed to open existing SMIOL file...'
            ierrcount = -1
            return
        end if

        ! Existing file for SMIOL_inquire_dim, unlimited dimension
        write(test_log,'(a)',advance='no') 'Existing file - unlimited dimension (SMIOLf_inquire_dim): '
        dimsize = 0_SMIOL_offset_kind
        ierr = SMIOLf_inquire_dim(file, 'Time', dimsize)
        if (ierr == SMIOL_SUCCESS) then
            if (dimsize == 0_SMIOL_offset_kind) then
                write(test_log,'(a)') 'PASS'
            else
                write(test_log,'(a,a,i11,a,i11)') 'FAIL - SMIOL_SUCCESS was returned, but the dimension size is wrong', &
                               ' (got ', dimsize, ', expected ', 0_SMIOL_offset_kind, ')'
                ierrcount = ierrcount + 1
            end if
        else
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if

        ! Existing file for SMIOL_inquire_dim, small non-record dimension
        write(test_log,'(a)',advance='no') 'Existing file - small non-record dimension (SMIOLf_inquire_dim): '
        dimsize = 0_SMIOL_offset_kind
#ifdef SMIOL_PNETCDF
        expected_dimsize = 40962_SMIOL_offset_kind
#else
        expected_dimsize = 0_SMIOL_offset_kind
#endif
        ierr = SMIOLf_inquire_dim(file, 'nCells', dimsize)
        if (ierr == SMIOL_SUCCESS) then
            if (dimsize == expected_dimsize) then
                write(test_log,'(a)') 'PASS'
            else
                write(test_log,'(a,a,i11,a,i11)') 'FAIL - SMIOL_SUCCESS was returned, but the dimension size is wrong', &
                               ' (got ', dimsize, ', expected ', expected_dimsize, ')'
                ierrcount = ierrcount + 1
            end if
        else
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if

        ! Existing file for SMIOL_inquire_dim, large non-record dimension
        write(test_log,'(a)',advance='no') 'Existing file - large non-record dimension (SMIOLf_inquire_dim): '
        dimsize = 0_SMIOL_offset_kind
#ifdef SMIOL_PNETCDF
        expected_dimsize = 99999999999_SMIOL_offset_kind
#else
        expected_dimsize = 0_SMIOL_offset_kind
#endif
        ierr = SMIOLf_inquire_dim(file, 'nElements', dimsize)
        if (ierr == SMIOL_SUCCESS) then
            if (dimsize == expected_dimsize) then
                write(test_log,'(a)') 'PASS'
            else
                write(test_log,'(a,a,i11,a,i11)') 'FAIL - SMIOL_SUCCESS was returned, but the dimension size is wrong', &
                               ' (got ', dimsize, ', expected ', expected_dimsize, ')'
                ierrcount = ierrcount + 1
            end if
        else
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        end if

        ! Close the SMIOL file
        ierr = SMIOLf_close_file(file)
        if (ierr /= SMIOL_SUCCESS .or. associated(file)) then
            write(test_log,'(a)') 'Failed to close SMIOL file...'
            ierrcount = -1
            return
        end if

        ! Free the SMIOL context
        ierr = SMIOLf_finalize(context)
        if (ierr /= SMIOL_SUCCESS .or. associated(context)) then
            write(test_log,'(a)') 'Failed to free SMIOL context...'
            ierrcount = -1
            return
        end if

        write(test_log,'(a)') ''

    end function test_dimensions


    function test_variables(test_log) result(ierrcount)

        implicit none

        integer, intent(in) :: test_log

        integer :: ierrcount
        type (SMIOLf_context), pointer :: context
        type (SMIOLf_file), pointer :: file
        character(len=32), dimension(6) :: dimnames

        character(len=32), dimension(:), pointer :: dimnames_out
#ifdef SMIOL_PNETCDF
        integer :: ndims_out
        integer :: vartype_out
#endif

        write(test_log,'(a)') '********************************************************************************'
        write(test_log,'(a)') '************ SMIOL_define_var / SMIOL_inquire_var unit tests *******************'
        write(test_log,'(a)') ''

        ierrcount = 0


        ! Create a SMIOL context for testing file variable routines
        nullify(context)
        ierr = SMIOLf_init(MPI_COMM_WORLD, context)
        if (ierr /= SMIOL_SUCCESS .or. .not. associated(context)) then
            write(test_log,'(a)') 'Failed to create a context...'
            ierrcount = -1
            return
        end if

        ! Create a SMIOL file for testing variable routines
        nullify(file)
        ierr = SMIOLf_open_file(context, 'test_vars_fortran.nc', SMIOL_FILE_CREATE, file)
        if (ierr /= SMIOL_SUCCESS .or. .not. associated(file)) then
            write(test_log,'(a)') 'Failed to create a file...'
            ierrcount = -1
            return
        end if

        ! Define several dimensions in the file to be used when defining variables
        if (SMIOLf_define_dim(file, 'Time', -1_SMIOL_offset_kind) /= SMIOL_SUCCESS) then
            write(test_log,'(a)') 'Failed to create dimension Time...'
            ierrcount = -1
            return
        end if

        if (SMIOLf_define_dim(file, 'nCells', 40962_SMIOL_offset_kind) /= SMIOL_SUCCESS) then
            write(test_log,'(a)') 'Failed to create dimension nCells...'
            ierrcount = -1
            return
        end if

        if (SMIOLf_define_dim(file, 'nVertLevels', 55_SMIOL_offset_kind) /= SMIOL_SUCCESS) then
            write(test_log,'(a)') 'Failed to create dimension nVertLevels...'
            ierrcount = -1
            return
        end if

        if (SMIOLf_define_dim(file, 'maxEdges', 6_SMIOL_offset_kind) /= SMIOL_SUCCESS) then
            write(test_log,'(a)') 'Failed to create dimension maxEdges...'
            ierrcount = -1
            return
        end if

        if (SMIOLf_define_dim(file, 'TWO', 2_SMIOL_offset_kind) /= SMIOL_SUCCESS) then
            write(test_log,'(a)') 'Failed to create dimension TWO...'
            ierrcount = -1
            return
        end if

        if (SMIOLf_define_dim(file, 'nMonths', 12_SMIOL_offset_kind) /= SMIOL_SUCCESS) then
            write(test_log,'(a)') 'Failed to create dimension nMonths...'
            ierrcount = -1
            return
        end if

        if (SMIOLf_define_dim(file, 'StrLen', 512_SMIOL_offset_kind) /= SMIOL_SUCCESS) then
            write(test_log,'(a)') 'Failed to create dimension StrLen...'
            ierrcount = -1
            return
        end if

        ! Define a 32-bit real variable with zero dimensions
        write(test_log,'(a)',advance='no') 'Define a 32-bit real variable with zero dimensions: '
        allocate(dimnames_out(0))
        ierr = SMIOLf_define_var(file, 'r0', SMIOL_REAL32, 0, dimnames_out)
        deallocate(dimnames_out)
        if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'PASS'
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL - a library-specific error was returned ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - '//trim(SMIOLf_error_string(ierr))
            ierrcount = ierrcount + 1
        end if

        ! Define a 32-bit real variable with one dimension that is a record dimension
        write(test_log,'(a)',advance='no') 'Define a 32-bit real variable with only a record dimension: '
        dimnames(1) = 'Time'
        ierr = SMIOLf_define_var(file, 'r0_t', SMIOL_REAL32, 1, dimnames)
        if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'PASS'
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL - a library-specific error was returned ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - '//trim(SMIOLf_error_string(ierr))
            ierrcount = ierrcount + 1
        end if

        ! Define a 32-bit real variable with one dimension that is *not* a record dimension
        write(test_log,'(a)',advance='no') 'Define a 32-bit real variable with one non-record dimension: '
        dimnames(1) = 'nCells'
        ierr = SMIOLf_define_var(file, 'r1', SMIOL_REAL32, 1, dimnames)
        if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'PASS'
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL - a library-specific error was returned ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - '//trim(SMIOLf_error_string(ierr))
            ierrcount = ierrcount + 1
        end if

        ! Define a 32-bit real variable with one non-record dimension and a record dimension
        write(test_log,'(a)',advance='no') 'Define a 32-bit real variable with one non-record dimension and a record dimension: '
        dimnames(1) = 'Time'
        dimnames(2) = 'nCells'
        ierr = SMIOLf_define_var(file, 'r1_t', SMIOL_REAL32, 2, dimnames)
        if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'PASS'
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL - a library-specific error was returned ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - '//trim(SMIOLf_error_string(ierr))
            ierrcount = ierrcount + 1
        end if

        ! Define a 32-bit real variable with five dimensions, none of which is a record dimension
        write(test_log,'(a)',advance='no') 'Define a 32-bit real variable with five non-record dimension: '
        dimnames(1) = 'nCells'
        dimnames(2) = 'nVertLevels'
        dimnames(3) = 'maxEdges'
        dimnames(4) = 'TWO'
        dimnames(5) = 'nMonths'
        ierr = SMIOLf_define_var(file, 'r5', SMIOL_REAL32, 5, dimnames)
        if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'PASS'
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL - a library-specific error was returned ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - '//trim(SMIOLf_error_string(ierr))
            ierrcount = ierrcount + 1
        end if

        ! Define a 32-bit real variable with five non-record dimensions and a record dimension
        write(test_log,'(a)',advance='no') 'Define a 32-bit real variable with five non-record dimension and a record dimension: '
        dimnames(1) = 'Time'
        dimnames(2) = 'nCells'
        dimnames(3) = 'nVertLevels'
        dimnames(4) = 'maxEdges'
        dimnames(5) = 'TWO'
        dimnames(6) = 'nMonths'
        ierr = SMIOLf_define_var(file, 'r5_t', SMIOL_REAL32, 6, dimnames)
        if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'PASS'
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL - a library-specific error was returned ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - '//trim(SMIOLf_error_string(ierr))
            ierrcount = ierrcount + 1
        end if

        ! Define a 64-bit real variable with five non-record dimension and a record dimension
        write(test_log,'(a)',advance='no') 'Define a 64-bit real variable with five non-record dimension and a record dimension: '
        dimnames(1) = 'Time'
        dimnames(2) = 'nCells'
        dimnames(3) = 'nVertLevels'
        dimnames(4) = 'maxEdges'
        dimnames(5) = 'TWO'
        dimnames(6) = 'nMonths'
        ierr = SMIOLf_define_var(file, 'd5_t', SMIOL_REAL64, 6, dimnames)
        if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'PASS'
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL - a library-specific error was returned ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - '//trim(SMIOLf_error_string(ierr))
            ierrcount = ierrcount + 1
        end if

        ! Define a 32-bit int variable with five non-record dimension and a record dimension
        write(test_log,'(a)',advance='no') 'Define a 32-bit int variable with five non-record dimension and a record dimension: '
        dimnames(1) = 'Time'
        dimnames(2) = 'nCells'
        dimnames(3) = 'nVertLevels'
        dimnames(4) = 'maxEdges'
        dimnames(5) = 'TWO'
        dimnames(6) = 'nMonths'
        ierr = SMIOLf_define_var(file, 'i5_t', SMIOL_INT32, 6, dimnames)
        if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'PASS'
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL - a library-specific error was returned ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - '//trim(SMIOLf_error_string(ierr))
            ierrcount = ierrcount + 1
        end if

        ! Define a char variable with one non-record dimension and a record dimension
        write(test_log,'(a)',advance='no') 'Define a character variable with one non-record dimension and a record dimension: '
        dimnames(1) = 'Time'
        dimnames(2) = 'StrLen'
        ierr = SMIOLf_define_var(file, 'c1_t', SMIOL_CHAR, 2, dimnames)
        if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'PASS'
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL - a library-specific error was returned ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - '//trim(SMIOLf_error_string(ierr))
            ierrcount = ierrcount + 1
        end if

#ifdef SMIOL_PNETCDF
        ! Try to re-define a variable that already exists
        write(test_log,'(a)',advance='no') 'Try to re-define a variable that already exists: '
        dimnames(1) = 'Time'
        dimnames(2) = 'nCells'
        ierr = SMIOLf_define_var(file, 'c1_t', SMIOL_CHAR, 2, dimnames)
        if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'PASS ('//trim(SMIOLf_lib_error_string(context))//')'
        else if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was erroneously returned'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - a return code of SMIOL_LIBRARY_ERROR was expected'
            ierrcount = ierrcount + 1
        end if

        ! Try to define a variable with an undefined dimension
        write(test_log,'(a)',advance='no') 'Try to define a variable with an undefined dimension: '
        dimnames(1) = 'Time'
        dimnames(2) = 'foobar'
        dimnames(3) = 'nVertLevels'
        ierr = SMIOLf_define_var(file, 'should_not_exist', SMIOL_INT32, 3, dimnames)
        if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'PASS ('//trim(SMIOLf_lib_error_string(context))//')'
        else if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was erroneously returned'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - a return code of SMIOL_LIBRARY_ERROR was expected'
            ierrcount = ierrcount + 1
        end if

        ! Try to define a variable with an invalid type
        write(test_log,'(a)',advance='no') 'Try to define a variable with an invalid type: '
        dimnames(1) = 'Time'
        dimnames(2) = 'nCells'
        dimnames(3) = 'nVertLevels'
        ierr = SMIOLf_define_var(file, 'should_not_exist', &
                                 not(ior(SMIOL_REAL32, ior(SMIOL_REAL64, ior(SMIOL_INT32, SMIOL_CHAR)))), &
                                 3, dimnames)
        if (ierr == SMIOL_INVALID_ARGUMENT) then
            write(test_log,'(a)') 'PASS'
        else
            write(test_log,'(a)') 'FAIL - a return code of SMIOL_INVALID_ARGUMENT was expected'
            ierrcount = ierrcount + 1
        end if
#endif

        ! Close the SMIOL file
        ierr = SMIOLf_close_file(file)
        if (ierr /= SMIOL_SUCCESS .or. associated(file)) then
            write(test_log,'(a)') 'Failed to close the file...'
            ierrcount = -1
            return
        end if

        ! Re-open the SMIOL file
        nullify(file)
        ierr = SMIOLf_open_file(context, 'test_vars_fortran.nc', SMIOL_FILE_READ, file)
        if (ierr /= SMIOL_SUCCESS .or. .not. associated(file)) then
            write(test_log,'(a)') 'Failed to re-open the file...'
            ierrcount = -1
            return
        end if

        nullify(dimnames_out)

#ifdef SMIOL_PNETCDF
        ! Inquire about just the number of dimensions for a variable
        write(test_log,'(a)',advance='no') 'Inquire about just the number of dimensions for a variable: '
        ndims_out = -1
        ierr = SMIOLf_inquire_var(file, 'r0_t', ndims=ndims_out)
        if (ierr == SMIOL_SUCCESS .and. ndims_out == 1) then
            write(test_log,'(a)') 'PASS'
        else if (ierr == SMIOL_SUCCESS .and. ndims_out /= 1) then
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was returned, but the number of dimensions was wrong'
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - '//trim(SMIOLf_error_string(ierr))
            ierrcount = ierrcount + 1
        end if

        ! Inquire about just the type of a variable
        write(test_log,'(a)',advance='no') 'Inquire about just the type of a variable: '
        vartype_out = SMIOL_UNKNOWN_VAR_TYPE
        ierr = SMIOLf_inquire_var(file, 'r5_t', vartype=vartype_out)
        if (ierr == SMIOL_SUCCESS .and. vartype_out == SMIOL_REAL32) then
            write(test_log,'(a)') 'PASS'
        else if (ierr == SMIOL_SUCCESS .and. vartype_out /= SMIOL_REAL32) then
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was returned, but the variable type was wrong'
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - '//trim(SMIOLf_error_string(ierr))
            ierrcount = ierrcount + 1
        end if

        ! Inquire about just the dimension names for a variable
        write(test_log,'(a)',advance='no') 'Inquire about just the dimension names for a variable: '
        allocate(dimnames_out(2))
        dimnames_out(1) = '---------'
        dimnames_out(2) = '---------'
        ierr = SMIOLf_inquire_var(file, 'r1_t', dimnames=dimnames_out)
        if (ierr == SMIOL_SUCCESS .and. &
            trim(dimnames_out(1)) == 'Time' .and. &
            trim(dimnames_out(2)) == 'nCells') then
            write(test_log,'(a)') 'PASS'
        else if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was returned, but the dimension names were wrong'
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - '//trim(SMIOLf_error_string(ierr))
            ierrcount = ierrcount + 1
        end if
        deallocate(dimnames_out)

        allocate(dimnames_out(6))

        ! Inquire about all properties of a variable
        write(test_log,'(a)',advance='no') 'Inquire about all properties of a variable: '
        vartype_out = SMIOL_UNKNOWN_VAR_TYPE
        ndims_out = -1
        dimnames_out(1) = '---------'
        dimnames_out(2) = '---------'
        ierr = SMIOLf_inquire_var(file, 'c1_t', vartype=vartype_out, ndims=ndims_out, dimnames=dimnames_out)
        if (ierr == SMIOL_SUCCESS .and. &
            ndims_out == 2 .and. &
            vartype_out == SMIOL_CHAR .and. &
            trim(dimnames_out(1)) == 'Time' .and. &
            trim(dimnames_out(2)) == 'StrLen') then
            write(test_log,'(a)') 'PASS'
        else if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was returned, but one or more properties were wrong'
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - '//trim(SMIOLf_error_string(ierr))
            ierrcount = ierrcount + 1
        end if

        deallocate(dimnames_out)
#endif

        ! Inquire about none of the properties of a variable
        write(test_log,'(a)',advance='no') 'Inquire about none of the properties of a variable: '
        ierr = SMIOLf_inquire_var(file, 'i5_t')
        if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'PASS'
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - '//trim(SMIOLf_error_string(ierr))
            ierrcount = ierrcount + 1
        end if

#ifdef SMIOL_PNETCDF
        allocate(dimnames_out(6))

        ! Try to inquire about an undefined variable
        write(test_log,'(a)',advance='no') 'Try to inquire about an undefined variable: '
        ierr = SMIOLf_inquire_var(file, 'fooblaz', vartype=vartype_out, ndims=ndims_out, dimnames=dimnames_out)
        if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'PASS ('//trim(SMIOLf_lib_error_string(context))//')'
        else if (ierr == SMIOL_SUCCESS) then
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was erroneously returned'
            ierrcount = ierrcount + 1
        else
            write(test_log,'(a)') 'FAIL - a return code of SMIOL_LIBRARY_ERROR was expected'
            ierrcount = ierrcount + 1
        end if

        deallocate(dimnames_out)
#endif

        ! Close the SMIOL file
        ierr = SMIOLf_close_file(file)
        if (ierr /= SMIOL_SUCCESS .or. associated(file)) then
            write(test_log,'(a)') 'Failed to close the file...'
            ierrcount = -1
            return
        end if

        ! Free the SMIOL context
        ierr = SMIOLf_finalize(context)
        if (ierr /= SMIOL_SUCCESS .or. associated(context)) then
            write(test_log,'(a)') 'Failed to finalize the context...'
            ierrcount = -1
            return
        end if

        write(test_log,'(a)') ''

    end function test_variables


    function test_file_sync(test_log) result(ierrcount)

        implicit none

        integer, intent(in) :: test_log
        integer :: ierr
        integer :: ierrcount
        type (SMIOLf_context), pointer :: context => null()
        type (SMIOLf_file), pointer :: file => null()

        write(test_log,'(a)') '********************************************************************************'
        write(test_log,'(a)') '************************* SMIOLf_sync_file tests *******************************'
        write(test_log,'(a)') ''

        ierrcount = 0

        ! Create a SMIOL context for testing SMIOL_sync_file
        ierr = SMIOLf_init(MPI_COMM_WORLD, context)
        if (ierr /= SMIOL_SUCCESS .or. .not. associated(context)) then
            write(test_log,'(a)') 'Failed to initalize a SMIOL context'
            ierrcount = -1
            return
        end if

        ! Test sync file on a file that was created with SMIOL_FILE_CREATE
        ierr = SMIOLf_open_file(context, 'smiolf_sync_file.nc', SMIOL_FILE_CREATE, file)
        if (ierr /= SMIOL_SUCCESS .or. .not. associated(file)) then
            write(test_log,'(a)') "Failed to open the file 'smiolf_sync_file.nc' with SMIOL_FILE_CREATE"
            ierrcount = -1
            return
        end if

        ! Everything OK (SMIOLf_sync_file)
        write(test_log,'(a)',advance='no') 'Everything OK (SMIOLf_sync_file) with SMIOL_FILE_CREATE: '
        ierr = SMIOLf_sync_file(file)
        if (ierr == SMIOL_SUCCESS .and. associated(file)) then
            write(test_log,'(a)') 'PASS'
        else if (ierr /= SMIOL_SUCCESS .and. associated(file)) then
            write(test_log,'(a)') 'FAIL - File was associated but SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_SUCCESS .and. .not. associated(file)) then
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was returned but file was not associated'
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL - ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        end if

        ! Close file
        ierr = SMIOLf_close_file(file)
        if (ierr /= SMIOL_SUCCESS .or. associated(file)) then
            write(test_log, '(a)') "Failed to close `smiolf_sync_file.nc'"
            ierrcount = -1
            return
        endif

        ! Test sync file on a file that was created with SMIOL_FILE_WRITE
        ierr = SMIOLf_open_file(context, 'smiolf_sync_file.nc', SMIOL_FILE_WRITE, file)
        if (ierr /= SMIOL_SUCCESS .or. .not. associated(file)) then
            write(test_log,'(a)') "Failed to open the file 'smiolf_sync_file.nc' with SMIOL_FILE_WRITE"
            ierrcount = -1
            return
        end if

        ! Everything OK (SMIOLf_sync_file)
        write(test_log,'(a)',advance='no') 'Everything OK (SMIOLf_sync_file) with SMIOL_FILE_WRITE: '
        ierr = SMIOLf_sync_file(file)
        if (ierr == SMIOL_SUCCESS .and. associated(file)) then
            write(test_log,'(a)') 'PASS'
        else if (ierr /= SMIOL_SUCCESS .and. associated(file)) then
            write(test_log,'(a)') 'FAIL - File was associated but SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_SUCCESS .and. .not. associated(file)) then
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was returned but file was not associated'
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL - ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        end if

        ! Close file
        ierr = SMIOLf_close_file(file)
        if (ierr /= SMIOL_SUCCESS .or. associated(file)) then
            write(test_log, '(a)') "Failed to close `smiolf_sync_file.nc'"
            ierrcount = -1
            return
        endif

        ! Test sync file on a file that was created with SMIOL_FILE_READ
        ierr = SMIOLf_open_file(context, 'smiolf_sync_file.nc', SMIOL_FILE_READ, file)
        if (ierr /= SMIOL_SUCCESS .or. .not. associated(file)) then
            write(test_log,'(a)') "Failed to open the file 'smiolf_sync_file.nc' with SMIOL_FILE_READ"
            ierrcount = -1
            return
        end if

        ! Everything OK (SMIOLf_sync_file)
        write(test_log,'(a)',advance='no') 'Everything OK (SMIOLf_sync_file) with SMIOL_FILE_READ: '
        ierr = SMIOLf_sync_file(file)
        if (ierr == SMIOL_SUCCESS .and. associated(file)) then
            write(test_log,'(a)') 'PASS'
        else if (ierr /= SMIOL_SUCCESS .and. associated(file)) then
            write(test_log,'(a)') 'FAIL - File was associated but SMIOL_SUCCESS was not returned'
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_SUCCESS .and. .not. associated(file)) then
            write(test_log,'(a)') 'FAIL - SMIOL_SUCCESS was returned but file was not associated'
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_LIBRARY_ERROR) then
            write(test_log,'(a)') 'FAIL - ('//trim(SMIOLf_lib_error_string(context))//')'
            ierrcount = ierrcount + 1
        end if

        ! Close file
        ierr = SMIOLf_close_file(file)
        if (ierr /= SMIOL_SUCCESS .or. associated(file)) then
            write(test_log, '(a)') "Failed to close `smiolf_sync_file.nc'"
            ierrcount = -1
            return
        endif

#if 0
! This test will work for most compilers, but it wont work for the PGI and (most likely)
! the Flang compiler. It fails at file % contect = c_loc(context).
#ifdef SMIOL_PNETCDF
        ! Try to sync a file that was never opened
        write(test_log,'(a)',advance='no') 'Try to sync a file that was never opened: '
        allocate(file)
        file % context = c_loc(context)
        file % state = -42 ! Erroneous, currently unused file state
        ierr = SMIOLf_file_sync(file)
        if (ierr == SMIOL_LIBRARY_ERROR .or. .not. associated(file)) then
            write(test_log, '(a)') 'PASS ('//trim(SMIOLf_lib_error_string(context))//')'
        else
            write(test_log, '(a)') 'FAIL - Expected error code of SMIOL_LIBRARY_ERROR not returned or file was NULL'
            ierrcount = ierrcount + 1
        end if
        deallocate(file)
#endif
#endif

        nullify(file)
        ! SMIOL_sync_file with an unassociated file
        write(test_log,'(a)',advance='no') 'Testing SMIOLf_sync_file with NULL file handle: '
        ierr = SMIOLf_sync_file(file)
        if (ierr == SMIOL_INVALID_ARGUMENT .and. .not. associated(file)) then
            write(test_log,'(a)') 'PASS'
        else
            write(0,*) ierr
            write(test_log, '(a)') 'FAIL - Expected error code of SMIOL_INVALID_ARGUMENT not returned or file was associated'
        endif

        ! Free the SMIOL context
        ierr = SMIOLf_finalize(context)
        if (ierr /= SMIOL_SUCCESS .or. associated(context)) then
            ierrcount = -1
            return
        end if

        write(test_log,'(a)') ''

    end function test_file_sync


    !-----------------------------------------------------------------------
    !  routine compare_decomps
    !
    !> \brief Compare a SMIOLf_decomp against known-correct comp_list and io_list
    !> \details
    !>  Compares the comp_list and io_list members of a SMIOLf_decomp against
    !>  comp_list and io_list arrays that contain the correct (reference) values.
    !>
    !>  If all values in the comp_list_correct and decomp % comp_list match and
    !>  if all values in the io_list_correct and decomp % io_list match, a value
    !>  of .true. is returned; otherwise, .false. is returned.
    !>
    !>  Note: If the decomp % comp_list is not at least the size of comp_list_correct,
    !>        or the decomp % io_list is not at least the size of io_list_correct,
    !>        this routine may fail unpredictably (perhaps with a segfault), since
    !>        accessing the lists in the SMIOLf_decomp type requires the use of
    !>        c_f_pointer with an assumed size for these arrays.
    !
    !-----------------------------------------------------------------------
    function compare_decomps(decomp, comp_list_correct, io_list_correct) result(matched)

        use iso_c_binding, only : c_f_pointer

        implicit none

        ! Arguments
        type (SMIOLf_decomp), intent(in) :: decomp
        integer(kind=SMIOL_offset_kind), dimension(:), intent(in) :: comp_list_correct
        integer(kind=SMIOL_offset_kind), dimension(:), intent(in) :: io_list_correct

        ! Return value
        logical :: matched

        ! Local variables
        integer :: i
        integer(kind=SMIOL_offset_kind), dimension(:), pointer :: comp_list
        integer(kind=SMIOL_offset_kind), dimension(:), pointer :: io_list


        matched = .true.

        ! Get Fortran pointers to the comp_list and io_list members of decomp
        call c_f_pointer(decomp % comp_list, comp_list, shape=[size(comp_list_correct)])
        call c_f_pointer(decomp % io_list, io_list, shape=[size(io_list_correct)])

        ! Check comp_list
        do i = 1, size(comp_list_correct)
            if (comp_list(i) /= comp_list_correct(i)) then
                matched = .false.
                return
            end if
        end do

        ! Check io_list
        do i = 1, size(io_list_correct)
            if (io_list(i) /= io_list_correct(i)) then
                matched = .false.
                return
            end if
        end do

    end function compare_decomps

end program smiol_runner
