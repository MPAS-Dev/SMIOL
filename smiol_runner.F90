#include "smiol_codes.inc"

program smiol_runner

    use SMIOLf

    implicit none

    if (SMIOLf_init() /= SMIOL_SUCCESS) then
        write(0,*) "ERROR: 'SMIOLf_init' was not called successfully"
        stop
    endif 

    if (SMIOLf_finalize() /= SMIOL_SUCCESS) then
        write(0,*) "ERROR: 'SMIOLf_finalize' was not called successfully"
        stop
    endif

    if (SMIOLf_inquire() /= SMIOL_SUCCESS) then
        write(0,*) "ERROR: 'SMIOLf_inquire' was not called successfully"
        stop
    endif

    if (SMIOLf_open_file() /= SMIOL_SUCCESS) then
        write(0,*) "ERROR: 'SMIOLf_open_file' was not called successfully"
        stop
    endif

    if (SMIOLf_close_file() /= SMIOL_SUCCESS) then
        write(0,*) "ERROR: 'SMIOLf_close_file' was not called successfully"
        stop
    endif

    if (SMIOLf_define_dim() /= SMIOL_SUCCESS) then 
        write(0,*) "ERROR: 'SMIOLf_define_dim' was not called successfully"
        stop
    endif

    if (SMIOLf_inquire_dim() /= SMIOL_SUCCESS) then
        write(0,*) "ERROR: 'SMIOLf_inquire_dim' was not called successfully"
        stop
    endif

    if (SMIOLf_define_var() /= SMIOL_SUCCESS) then
        write(0,*) "ERROR: 'SMIOLf_define_var' was not called successfully"
        stop
    endif

    if (SMIOLf_inquire_var() /= SMIOL_SUCCESS) then
        write(0,*) "ERROR: 'SMIOLf_inquire_var' was not called successfully"
        stop
    endif

    if (SMIOLf_put_var() /= SMIOL_SUCCESS) then
        write(0,*) "ERROR: 'SMIOLf_put_var' was not called successfully"
        stop
    endif

    if (SMIOLf_get_var() /= SMIOL_SUCCESS) then
        write(0,*) "ERROR: 'SMIOLf_get_var' was not called successfully"
        stop
    endif

    if (SMIOLf_define_att() /= SMIOL_SUCCESS) then
        write(0,*) "ERROR: 'SMIOLf_define_att' was not called successfully"
        stop
    endif

    if (SMIOLf_inquire_att() /= SMIOL_SUCCESS) then
        write(0,*) "ERROR: 'SMIOLf_inquire_att' was not called successfully"
        stop
    endif

    if (SMIOLf_file_sync() /= SMIOL_SUCCESS) then
        write(0,*) "ERROR: 'SMIOLf_file_sync' was not called successfully"
        stop
    endif

    if (SMIOLf_set_option() /= SMIOL_SUCCESS) then
        write(0,*) "ERROR: 'SMIOLf_set_option' was not called successfully"
        stop
    endif

    write(0,*) "Testing SMIOLf_error_string success: ", SMIOLf_error_string(0)
    write(0,*) "Testing SMIOLf_error_string unkown error: ", SMIOLf_error_string(1)
    write(0,*) "SUCCESS"

end program smiol_runner
