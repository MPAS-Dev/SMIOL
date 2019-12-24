#include <stdio.h>
#include <stdlib.h>
#include "smiol.h"

/*******************************************************************************
 * SMIOL C Runner - Take SMIOL out for a run!
 *******************************************************************************/

int main(int argc, char **argv)
{
	int ierr;
	size_t n_compute_elements = 1;
	size_t n_io_elements = 1;
	uint64_t *compute_elements;
	uint64_t *io_elements;
	struct SMIOL_decomp *decomp = NULL;
	struct SMIOL_context *context = NULL;

	if (argc == 2) {
		n_compute_elements = (size_t) atoi(argv[1]);
		n_io_elements = (size_t) atoi(argv[1]);
	}

	if (MPI_Init(&argc, &argv) != MPI_SUCCESS) {
		fprintf(stderr, "Error: MPI_Init failed.\n");
		return 1;
	}

	if ((ierr = SMIOL_init(MPI_COMM_WORLD, &context)) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_init: %s ", SMIOL_error_string(ierr));
		return 1;
	} 

	if (context == NULL) {
		fprintf(stderr, "SMIOL_init returned a NULL context\n");
		return 1;
	}

	// Create elements
	compute_elements = malloc(sizeof(uint64_t) * n_compute_elements);
	io_elements = malloc(sizeof(uint64_t) * n_io_elements);

	decomp = SMIOL_create_decomp(n_compute_elements, n_io_elements,
				compute_elements, io_elements);
	if (decomp == NULL) {
		printf("ERROR: SMIOL_create_decomp - Decomp was not allocated\n");
		return 1;
	}

	// Free local copy
	free(compute_elements);
	free(io_elements);

	if ((ierr = SMIOL_free_decomp(&decomp)) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_free_decomp: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	if (decomp != NULL) {
		printf("ERROR: SMIOL_free_decomp - Decomp not 'NULL' after free\n");
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
	printf("SMIOL_error_string test 'Success!': %s\n", SMIOL_error_string(SMIOL_SUCCESS));
	printf("SMIOL_error_string test 'malloc returned a null pointer': %s\n",
		SMIOL_error_string(SMIOL_MALLOC_FAILURE));

	if ((ierr = SMIOL_finalize(&context)) != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_finalize: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	if (context != NULL) {
		fprintf(stderr, "SMIOL_finalize returned a non-NULL context\n");
		return 1;
	}
	printf("Called all SMIOL functions successfully!\n");

	if (MPI_Finalize() != MPI_SUCCESS) {
		fprintf(stderr, "Error: MPI_Finalize failed.\n");
		return 1;
	}

	return 0;
}
