#include <stdio.h>
#include "smiol.h"
#include "smiol_codes.inc"

/*******************************************************************************
 * SMIOL C Runner - Take SMIOL out for a run!
 *******************************************************************************/

int main(int argc, char **argv)
{
	int ierr;

	if ((ierr = SMIOL_init()) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_init: %s ", SMIOL_error_string(ierr));
		return 1;
	} 

	if ((ierr = SMIOL_finalize()) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_finalize: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_inquire()) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_inquire: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_open_file()) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_open_file: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_close_file()) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_close_file: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_define_dim()) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_define_dim: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_inquire_dim()) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_inquire_dim: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_define_var()) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_define_var: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_put_var()) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_put_var: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_get_var()) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_get_var: %s ", SMIOL_error_string(ierr));
		return 1;
	}
		
	if ((ierr = SMIOL_define_att()) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_define_att: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_inquire_att()) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_inquire_att: %s ", SMIOL_error_string(ierr));
		return 1;
	}
	
	if ((ierr = SMIOL_file_sync()) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_file_sync: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_set_option()) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_set_option: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	printf("SMIOL_error_string test 'Unknown error': %s\n", SMIOL_error_string(-1));
	printf("SMIOL_error_string test 'Success!': %s\n", SMIOL_error_string(0));
	printf("Called all SMIOL functions successfully!\n");

	return 0;
}
