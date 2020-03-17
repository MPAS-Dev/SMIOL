#include "smiol_codes.inc"

program smiol_runner

    use iso_c_binding, only : c_size_t
    use SMIOLf
    use mpi

    implicit none

    integer :: ierr
    integer :: my_proc_id
    integer :: test_log = 42
    integer(kind=c_size_t) :: n_compute_elements = 1
    integer(kind=c_size_t) :: n_io_elements = 1
    integer(kind=SMIOL_offset_kind), dimension(:), pointer :: compute_elements
    integer(kind=SMIOL_offset_kind), dimension(:), pointer :: io_elements
    type (SMIOLf_decomp), pointer :: decomp => null()
    type (SMIOLf_context), pointer :: context => null()
    type (SMIOLf_file), pointer :: file => null()
    character(len=16) :: log_fname
    integer(kind=SMIOL_offset_kind) :: dimsize

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

    allocate(compute_elements(n_compute_elements))
    allocate(io_elements(n_io_elements))

    if (SMIOLf_create_decomp(context, n_compute_elements, compute_elements, &
                             n_io_elements, io_elements, decomp) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "Error: SMIOLf_create_decomp was not called successfully"
        stop 1
    endif

    deallocate(compute_elements)
    deallocate(io_elements)

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

    if (SMIOLf_close_file(file) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_close_file' was not called successfully"
        stop 1
    endif

    if (SMIOLf_define_var() /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_define_var' was not called successfully"
        stop 1
    endif

    if (SMIOLf_inquire_var() /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_inquire_var' was not called successfully"
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

    if (SMIOLf_define_att() /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_define_att' was not called successfully"
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
        integer(kind=c_size_t) :: n_compute_elements
        integer(kind=c_size_t) :: n_io_elements
        integer(kind=SMIOL_offset_kind), dimension(:), pointer :: compute_elements
        integer(kind=SMIOL_offset_kind), dimension(:), pointer :: io_elements
        type (SMIOLf_context), pointer :: context
        type (SMIOLf_decomp), pointer :: decomp => null()

        write(test_log,'(a)') '********************************************************************************'
        write(test_log,'(a)') '************ SMIOLf_create_decomp / SMIOLf_free_decomp tests *******************'
        write(test_log,'(a)') ''

        ierrcount = 0

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
        n_io_elements = 0
        allocate(compute_elements(n_compute_elements))
        allocate(io_elements(n_io_elements))
        ierr = SMIOLf_create_decomp(context, n_compute_elements, compute_elements, n_io_elements, io_elements, decomp)
        if (ierr == SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "PASS"
        else
            write(test_log, '(a)') "FAIL - Either SMIOL_SUCCESS was not returned or decomp was not associated"
            ierrcount = ierrcount + 1
        endif

        deallocate(compute_elements)
        deallocate(io_elements)

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
        n_io_elements = 1
        allocate(compute_elements(n_compute_elements))
        allocate(io_elements(n_io_elements))
        ierr = SMIOLf_create_decomp(context, n_compute_elements, compute_elements, n_io_elements, io_elements, decomp)
        if (ierr == SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "PASS"
        else
            write(test_log, '(a)') "FAIL - Either SMIOL_SUCCESS was not returned or decomp was not associated"
            ierrcount = ierrcount + 1
        endif

        deallocate(compute_elements)
        deallocate(io_elements)

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
        n_compute_elements = 10000000
        n_io_elements = 10000000
        allocate(compute_elements(n_compute_elements))
        allocate(io_elements(n_io_elements))
        ierr = SMIOLf_create_decomp(context, n_compute_elements, compute_elements, n_io_elements, io_elements, decomp)
        if (ierr == SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "PASS"
        else
            write(test_log, '(a)') "FAIL - Either SMIOL_SUCCESS was not returned or decomp was not associated"
            ierrcount = ierrcount + 1
        endif

        deallocate(compute_elements)
        deallocate(io_elements)

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

end program smiol_runner
