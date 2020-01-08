#include "smiol_codes.inc"

program smiol_runner

    use iso_c_binding, only : c_size_t, c_int64_t
    use SMIOLf
    use mpi

    implicit none

    integer :: ierr
    integer :: my_proc_id
    integer :: test_log = 42
    integer(c_size_t) :: n_compute_elements = 1
    integer(c_size_t) :: n_io_elements = 1
    integer(c_int64_t), dimension(:), pointer :: compute_elements
    integer(c_int64_t), dimension(:), pointer :: io_elements
    type (SMIOLf_decomp), pointer :: decomp => null()
    type (SMIOLf_context), pointer :: context => null()
    type (SMIOLf_file), pointer :: file => null()
    character(len=16) :: log_fname

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

    write(log_fname, '(a,I4.4,a)') "smiolf.", my_proc_id, ".test"
    open(unit=test_log, file=log_fname, status='replace')

    !
    ! Unit tests for SMIOL_init and SMIOL_finalize
    !
    ierr = test_init_finalize(test_log)
    if (ierr == 0) then
        write(test_log,'(a)') 'All tests PASSED!'
        write(test_log,'(a)') ''
    else
        write(test_log,'(a)') ierr, ' tests FAILED!'
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
        write(test_log,'(a)') ierr, ' tests FAILED!'
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
        write(test_log,'(a)') ierr, ' tests FAILED!'
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

    if (SMIOLf_create_decomp(n_compute_elements, n_io_elements, compute_elements, io_elements, decomp) /= SMIOL_SUCCESS) then
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

    if (SMIOLf_open_file(context, "blahf.nc", file) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_open_file' was not called successfully"
        stop 1
    endif

    if (SMIOLf_close_file(file) /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_close_file' was not called successfully"
        stop 1
    endif

    if (SMIOLf_define_dim() /= SMIOL_SUCCESS) then 
        write(test_log,'(a)') "ERROR: 'SMIOLf_define_dim' was not called successfully"
        stop 1
    endif

    if (SMIOLf_inquire_dim() /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_inquire_dim' was not called successfully"
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

    if (SMIOLf_file_sync() /= SMIOL_SUCCESS) then
        write(test_log,'(a)') "ERROR: 'SMIOLf_file_sync' was not called successfully"
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

        ! Everything OK (SMIOLf_open_file)
        write(test_log,'(a)',advance='no') 'Everything OK (SMIOLf_open_file): '
        nullify(file)
        ierr = SMIOLf_open_file(context, 'test_fortran.nc', file)
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

        use iso_c_binding, only : c_size_t, c_int64_t

        implicit none

        integer, intent(in) :: test_log
        integer :: ierrcount
        integer(c_size_t) :: n_compute_elements
        integer(c_size_t) :: n_io_elements
        integer(c_int64_t), dimension(:), pointer :: compute_elements
        integer(c_int64_t), dimension(:), pointer :: io_elements
        type (SMIOLf_decomp), pointer :: decomp => null()

        write(test_log,'(a)') '********************************************************************************'
        write(test_log,'(a)') '************ SMIOLf_create_decomp / SMIOLf_free_decomp tests *******************'
        write(test_log,'(a)') ''

        ierrcount = 0

        ! Test with 0 elements
        write(test_log,'(a)',advance='no') 'Everything OK for SMIOLf_create_decomp with 0 elements: '
        n_compute_elements = 0
        n_io_elements = 0
        allocate(compute_elements(n_compute_elements))
        allocate(io_elements(n_io_elements))
        ierr = SMIOLf_create_decomp(n_compute_elements, n_io_elements, compute_elements, io_elements, decomp)
        if (ierr == SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "PASS"
        else if (ierr /= SMIOL_SUCCESS .and. .not. associated(decomp)) then
            write(test_log,'(a)') "FAIL - SMIOLf_create_decomp returned an error and decomp was not associated"
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_SUCCESS .and. .not. associated(decomp)) then
            write(test_log,'(a)') "FAIL - ierr returned success but decomp was NOT associated when it should have been"
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
        ierr = SMIOLf_create_decomp(n_compute_elements, n_io_elements, compute_elements, io_elements, decomp)
        if (ierr == SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "PASS"
        else if (ierr /= SMIOL_SUCCESS .and. .not. associated(decomp)) then
            write(test_log,'(a)') "FAIL - SMIOLf_create_decomp returned an error and decomp was not associated"
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_SUCCESS .and. .not. associated(decomp)) then
            write(test_log,'(a)') "FAIL - ierr returned success but decomp was NOT associated when it should have been"
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
        ierr = SMIOLf_create_decomp(n_compute_elements, n_io_elements, compute_elements, io_elements, decomp)
        if (ierr == SMIOL_SUCCESS .and. associated(decomp)) then
            write(test_log,'(a)') "PASS"
        else if (ierr /= SMIOL_SUCCESS .and. .not. associated(decomp)) then
            write(test_log,'(a)') "FAIL - SMIOLf_create_decomp returned an error and decomp was not associated"
            ierrcount = ierrcount + 1
        else if (ierr == SMIOL_SUCCESS .and. .not. associated(decomp)) then
            write(test_log,'(a)') "FAIL - ierr returned success but decomp was NOT associated when it should have been"
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

        write(test_log,'(a)') ''

    end function test_decomp


end program smiol_runner
