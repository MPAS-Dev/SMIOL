#include <stdio.h>
#include <stdlib.h>
#include "smiol.h"

/*******************************************************************************
 * SMIOL C Runner - Take SMIOL out for a run!
 *******************************************************************************/

int test_init_finalize(FILE *test_log);
int test_open_close(FILE *test_log);
int test_dimensions(FILE *test_log);
int test_decomp(FILE *test_log);
int test_file_sync(FILE *test_log);

int main(int argc, char **argv)
{
	int ierr;
	int my_proc_id;
	SMIOL_Offset dimsize;
	size_t n_compute_elements = 1;
	size_t n_io_elements = 1;
	int64_t *compute_elements;
	int64_t *io_elements;
	struct SMIOL_decomp *decomp = NULL;
	struct SMIOL_context *context = NULL;
	struct SMIOL_file *file = NULL;
	char log_fname[17];
	FILE *test_log = NULL;

	if (argc == 2) {
		n_compute_elements = (size_t) atoi(argv[1]);
		n_io_elements = (size_t) atoi(argv[1]);
	}

	if (MPI_Init(&argc, &argv) != MPI_SUCCESS) {
		fprintf(stderr, "Error: MPI_Init failed.\n");
		return 1;
	}

	if (MPI_Comm_rank(MPI_COMM_WORLD, &my_proc_id) != MPI_SUCCESS) {
		fprintf(stderr, "Error: MPI_Comm_rank failed.\n");
	}

	sprintf(log_fname, "smiol.%04d.test", my_proc_id);
	test_log = fopen(log_fname, "w");
	if (test_log == NULL) {
		fprintf(stderr, "Error: Could not open test log file\n");
		return 1;
	}

	/*
	 * Unit tests for SMIOL_init and SMIOL_finalize
	 */
	ierr = test_init_finalize(test_log);
	if (ierr == 0) {
		fprintf(test_log, "All tests PASSED!\n\n");
	}
	else {
		fprintf(test_log, "%i tests FAILED!\n\n", ierr);
	}


	/*
	 * Unit tests for SMIOL_open_file and SMIOL_close_file
	 */
	ierr = test_open_close(test_log);
	if (ierr == 0) {
		fprintf(test_log, "All tests PASSED!\n\n");
	}
	else {
		fprintf(test_log, "%i tests FAILED!\n\n", ierr);
	}


	/*
	 * Unit tests for dimensions
	 */
	ierr = test_dimensions(test_log);
	if (ierr == 0) {
		fprintf(test_log, "All tests PASSED!\n\n");
	}
	else {
		fprintf(test_log, "%i tests FAILED!\n\n", ierr);
	}


	/*
	 * Unit tests for SMIOL_create_decomp and SMIOL_free_decomp
	 */
	ierr = test_decomp(test_log);
	if (ierr == 0) {
		fprintf(test_log, "All tests PASSED!\n\n");
	}
	else {
		fprintf(test_log, "%i tests FAILED!\n\n", ierr);
	}

	/*
	 * Unit tests for SMIOL_sync_file
	 */
	ierr = test_file_sync(test_log);
	if (ierr == 0) {
		fprintf(test_log, "All tests PASSED!\n\n");
	}
	else {
		fprintf(test_log, "%i tests FAILED!\n\n", ierr);
	}

	if ((ierr = SMIOL_init(MPI_COMM_WORLD, &context)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_init: %s ", SMIOL_error_string(ierr));
		return 1;
	} 

	if (context == NULL) {
		fprintf(test_log, "SMIOL_init returned a NULL context\n");
		return 1;
	}

	// Create elements
	compute_elements = malloc(sizeof(int64_t) * n_compute_elements);
	io_elements = malloc(sizeof(int64_t) * n_io_elements);

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
		fprintf(test_log, "ERROR: SMIOL_free_decomp: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}

	if (decomp != NULL) {
		fprintf(test_log, "ERROR: SMIOL_free_decomp - Decomp not 'NULL' after free\n");
		return 1;
	}

	if ((ierr = SMIOL_inquire()) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_inquire: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_open_file(context, "blah.nc", SMIOL_FILE_CREATE, &file)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_open_file: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_define_dim(file, "Time", -1)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_define_dim: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_sync_file(file)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_sync_file: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_define_dim(file, "nCells", 40962)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_define_dim: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_inquire_dim(file, "nCells", &dimsize)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_inquire_dim: %s ", SMIOL_error_string(ierr));
		return 1;
	}
	fprintf(test_log, "Size of nCells dimension is %li\n", (long int)dimsize);

	if ((ierr = SMIOL_sync_file(file)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_sync_file: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_close_file(&file)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_close_file: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_define_var()) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_define_var: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_put_var()) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_put_var: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_get_var()) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_get_var: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}
		
	if ((ierr = SMIOL_define_att()) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_define_att: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_inquire_att()) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_inquire_att: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}
	
	if ((ierr = SMIOL_set_option()) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_set_option: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}

	fprintf(test_log, "SMIOL_error_string test 'Unknown error': %s\n",
						SMIOL_error_string(-1));
	fprintf(test_log, "SMIOL_error_string test 'Success!': %s\n",
						SMIOL_error_string(SMIOL_SUCCESS));
	fprintf(test_log, "SMIOL_error_string test 'malloc returned a null pointer': %s\n",
		SMIOL_error_string(SMIOL_MALLOC_FAILURE));
	fprintf(test_log, "SMIOL_error_string test 'invalid subroutine argument': %s\n",
		SMIOL_error_string(SMIOL_INVALID_ARGUMENT));
	fprintf(test_log, "SMIOL_error_string test 'internal MPI call failed': %s\n",
		SMIOL_error_string(SMIOL_MPI_ERROR));
	fprintf(test_log, "SMIOL_error_string test 'Fortran wrapper detected an inconsistency in C return values': %s\n",
		SMIOL_error_string(SMIOL_FORTRAN_ERROR));
	fprintf(test_log, "SMIOL_error_string test 'bad return code from a library call': %s\n",
		SMIOL_error_string(SMIOL_LIBRARY_ERROR));

	fprintf(test_log, "SMIOL_lib_error_string 'Could not find matching library for the source of the error': %s\n",
		SMIOL_lib_error_string(context));

	if ((ierr = SMIOL_finalize(&context)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_finalize: %s ", SMIOL_error_string(ierr));
		return 1;
	}

	if (context != NULL) {
		fprintf(test_log, "SMIOL_finalize returned a non-NULL context\n");
		return 1;
	}

	if (fclose(test_log) != 0) {
		fprintf(stderr, "Error: Could not close test_log\n");
		return 1;
	}

	fprintf(stderr, "Called all SMIOL functions successfully!\n");

	if (MPI_Finalize() != MPI_SUCCESS) {
		fprintf(stderr, "Error: MPI_Finalize failed.\n");
		return 1;
	}

	return 0;
}

int test_init_finalize(FILE *test_log)
{
	int ierr;
	int errcount;
	struct SMIOL_context *context;

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************ SMIOL_init / SMIOL_finalize unit tests ****************************\n");
	fprintf(test_log, "\n");

	errcount = 0;

	/* Null context pointer */
	fprintf(test_log, "Null pointer to context pointer (SMIOL_init): ");
	ierr = SMIOL_init(MPI_COMM_WORLD, NULL);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, when an error was expected\n");
		errcount++;
	}
	else {
		fprintf(test_log, "PASS\n");
	}

	/* Invalid MPI communicator, and with a non-NULL context that should be NULL on return */
	fprintf(test_log, "Invalid MPI communicator (SMIOL_init): ");
	context = (struct SMIOL_context *)NULL + 42;   /* Any non-NULL value should be fine... */
	ierr = SMIOL_init(MPI_COMM_NULL, &context);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, when an error was expected\n");
		errcount++;
	}
	else if (context != NULL) {
		fprintf(test_log, "FAIL - an error code was returned, but context was not NULL\n");
		errcount++;
	}
	else {
		fprintf(test_log, "PASS\n");
	}

	/* Handle NULL context in SMIOL_finalize */
	fprintf(test_log, "Handle NULL context (SMIOL_finalize): ");
	context = NULL;
	ierr = SMIOL_finalize(&context);
	if (ierr == SMIOL_SUCCESS && context == NULL) {
		fprintf(test_log, "PASS\n");
	}
	else if (context != NULL) {
		fprintf(test_log, "FAIL - context is not NULL\n");
		errcount++;
	}
	else {
		fprintf(test_log, "FAIL - context is NULL as expected, but SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Handle NULL pointer to context pointer in SMIOL_finalize */
	fprintf(test_log, "Handle NULL pointer to context pointer (SMIOL_finalize): ");
	ierr = SMIOL_finalize(NULL);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK for SMIOL_init */
	fprintf(test_log, "Everything OK (SMIOL_init): ");
	context = NULL;
	ierr = SMIOL_init(MPI_COMM_WORLD, &context);
	if (ierr == SMIOL_SUCCESS && context != NULL) {
		fprintf(test_log, "PASS\n");
	}
	else if (ierr == SMIOL_SUCCESS && context == NULL) {
		fprintf(test_log, "FAIL - context is NULL, although SMIOL_SUCCESS was returned\n");
		errcount++;
	}
	else if (ierr != SMIOL_SUCCESS && context != NULL) {
		fprintf(test_log, "FAIL - context is not NULL as expected, but SMIOL_SUCCESS was not returned\n");
		errcount++;
	}
	else {
		fprintf(test_log, "FAIL - context is NULL, and SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK for SMIOL_finalize */
	fprintf(test_log, "Everything OK (SMIOL_finalize): ");
	ierr = SMIOL_finalize(&context);
	if (ierr == SMIOL_SUCCESS && context == NULL) {
		fprintf(test_log, "PASS\n");
	}
	else if (context != NULL) {
		fprintf(test_log, "FAIL - context is not NULL\n");
		errcount++;
	}
	else {
		fprintf(test_log, "FAIL - context is NULL as expected, but SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	fflush(test_log);
	ierr = MPI_Barrier(MPI_COMM_WORLD);

	fprintf(test_log, "\n");

	return errcount;
}

int test_open_close(FILE *test_log)
{
	int ierr;
	int errcount;
	struct SMIOL_context *context;
	struct SMIOL_file *file;

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************ SMIOL_open_file / SMIOL_close_file unit tests *********************\n");
	fprintf(test_log, "\n");

	errcount = 0;

	/* Create a SMIOL context for testing file open/close routines */
	ierr = SMIOL_init(MPI_COMM_WORLD, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	/* Try to open a file with an invalid mode */
	fprintf(test_log, "Try to open a file with an invalid mode: ");
	file = NULL;
	ierr = SMIOL_open_file(context, "smiol_invalid.nc", ~(SMIOL_FILE_CREATE | SMIOL_FILE_WRITE | SMIOL_FILE_READ), &file);
	if (ierr == SMIOL_INVALID_ARGUMENT && file == NULL) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - expected error code of SMIOL_INVALID_ARGUMENT not returned, or file not NULL\n");
		errcount++;
	}

#ifdef SMIOL_PNETCDF
	/* Try to create a file for which we should not have sufficient permissions */
	fprintf(test_log, "Try to create a file with insufficient permissions: ");
	file = NULL;
	ierr = SMIOL_open_file(context, "/smiol_test.nc", SMIOL_FILE_CREATE, &file);
	if (ierr == SMIOL_LIBRARY_ERROR && file == NULL) {
		fprintf(test_log, "PASS (%s)\n", SMIOL_lib_error_string(context));
	}
	else {
		fprintf(test_log, "FAIL - expected error code of SMIOL_LIBRARY_ERROR not returned, or file not NULL\n");
		errcount++;
	}

	/* Try to open a file that does not exist */
	fprintf(test_log, "Try to open a non-existent file: ");
	file = NULL;
	ierr = SMIOL_open_file(context, "/smiol_foobar.nc", SMIOL_FILE_READ, &file);
	if (ierr == SMIOL_LIBRARY_ERROR && file == NULL) {
		fprintf(test_log, "PASS (%s)\n", SMIOL_lib_error_string(context));
	}
	else {
		fprintf(test_log, "FAIL - expected error code of SMIOL_LIBRARY_ERROR not returned, or file not NULL\n");
		errcount++;
	}

	/* Try to close a file that was never opened */
	fprintf(test_log, "Try to close a file that was never opened: ");
	file = (struct SMIOL_file *)malloc(sizeof(struct SMIOL_file));
	file->context = context;
	ierr = SMIOL_close_file(&file);
	free(file);
	if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "PASS (%s)\n", SMIOL_lib_error_string(context));
	}
	else {
		fprintf(test_log, "FAIL - expected error code of SMIOL_LIBRARY_ERROR not returned\n");
		errcount++;
	}

	/* Create a file to be closed and opened again */
	fprintf(test_log, "Create a file to be closed and later re-opened: ");
	file = NULL;
	ierr = SMIOL_open_file(context, "pnetcdf_test_c.nc", SMIOL_FILE_CREATE, &file);
	if (ierr == SMIOL_SUCCESS && file != NULL) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL (%s) - failed to create a new file\n", SMIOL_lib_error_string(context));
		errcount++;
	}

	/* Close the file that was just created */
	fprintf(test_log, "Close the file that was just created: ");
	ierr = SMIOL_close_file(&file);
	if (ierr == SMIOL_SUCCESS && file == NULL) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - context is not NULL or SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Re-open the file with read access */
	fprintf(test_log, "Re-open file with read access: ");
	file = NULL;
	ierr = SMIOL_open_file(context, "pnetcdf_test_c.nc", SMIOL_FILE_READ, &file);
	if (ierr == SMIOL_SUCCESS && file != NULL) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL (%s) - failed to re-open existing file\n", SMIOL_lib_error_string(context));
		errcount++;
	}

	/* Close the file */
	fprintf(test_log, "Close the file: ");
	ierr = SMIOL_close_file(&file);
	if (ierr == SMIOL_SUCCESS && file == NULL) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - context is not NULL or SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Re-open the file with write access */
	fprintf(test_log, "Re-open file with write access: ");
	file = NULL;
	ierr = SMIOL_open_file(context, "pnetcdf_test_c.nc", SMIOL_FILE_WRITE, &file);
	if (ierr == SMIOL_SUCCESS && file != NULL) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL (%s) - failed to re-open existing file\n", SMIOL_lib_error_string(context));
		errcount++;
	}

	/* Close the file */
	fprintf(test_log, "Close the file: ");
	ierr = SMIOL_close_file(&file);
	if (ierr == SMIOL_SUCCESS && file == NULL) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - context is not NULL or SMIOL_SUCCESS was not returned\n");
		errcount++;
	}
#endif

	/* Everything OK (SMIOL_open_file) */
	fprintf(test_log, "Everything OK (SMIOL_open_file): ");
	file = NULL;
	ierr = SMIOL_open_file(context, "test.nc", SMIOL_FILE_CREATE, &file);
	if (ierr == SMIOL_SUCCESS && file != NULL) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - context is NULL or SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK (SMIOL_close_file) */
	fprintf(test_log, "Everything OK (SMIOL_close_file): ");
	ierr = SMIOL_close_file(&file);
	if (ierr == SMIOL_SUCCESS && file == NULL) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - context is not NULL or SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Free the SMIOL context */
	ierr = SMIOL_finalize(&context);
	if (ierr != SMIOL_SUCCESS || context != NULL) {
		fprintf(test_log, "Failed to free SMIOL context...\n");
		return -1;
	}

	fflush(test_log);
	ierr = MPI_Barrier(MPI_COMM_WORLD);

	fprintf(test_log, "\n");

	return errcount;
}

int test_decomp(FILE *test_log)
{
	int ierr;
	int errcount = 0;
	size_t n_compute_elements, n_io_elements;
	int64_t *compute_elements = NULL, *io_elements = NULL;
	struct SMIOL_decomp *decomp = NULL;

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************ SMIOL_create_decomp / SMIOL_free_decomp unit tests ****************\n");
	fprintf(test_log, "\n");

	/* Create decomp with io_elements and compute_elements == NULL */
	fprintf(test_log, "Everything OK (SMIOL_create_decomp) with NULL elements: ");
	n_compute_elements = 0;
	n_io_elements = 0;
	decomp = SMIOL_create_decomp(n_compute_elements, n_io_elements,
					compute_elements, io_elements);
	if (decomp != NULL) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - decomp was returned as NULL\n");
	}

	fprintf(test_log, "Everything OK (SMIOL_free_decomp) with NULL elements: ");
	ierr = SMIOL_free_decomp(&decomp);
	if (ierr == SMIOL_SUCCESS && decomp == NULL) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_SUCCESS && decomp != NULL) {
		fprintf(test_log, "FAIL - Returned SMIOL_SUCCESS but, decomp was not NULL\n");
		errcount++;
	} else if (ierr != SMIOL_SUCCESS && decomp == NULL) {
		fprintf(test_log, "FAIL - decomp was NULL but did not returned SMIOL_SUCCESS\n");
		errcount++;
	}


	/* Create and Free Decomp with elements == 0 */
	fprintf(test_log, "Everything OK (SMIOL_create_decomp) with 0 elements: ");
	n_compute_elements = 0;
	n_io_elements = 0;
	compute_elements = malloc(sizeof(int64_t) * n_compute_elements);
	io_elements = malloc(sizeof(int64_t) * n_io_elements);
	decomp = SMIOL_create_decomp(n_compute_elements, n_io_elements,
				compute_elements, io_elements);
	if (decomp != NULL) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - decomp was returned as null\n");
		errcount++;
	}
	free(compute_elements);
	free(io_elements);

	fprintf(test_log, "Everything OK (SMIOL_free_decomp) with 0 elements: ");
	ierr = SMIOL_free_decomp(&decomp);
	if (ierr == SMIOL_SUCCESS && decomp == NULL) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_SUCCESS && decomp != NULL) {
		fprintf(test_log, "FAIL - Returned SMIOL_SUCCESS but, decomp was not NULL\n");
		errcount++;
	} else if (ierr != SMIOL_SUCCESS && decomp == NULL) {
		fprintf(test_log, "FAIL - decomp was NULL but did not returned SMIOL_SUCCESS\n");
		errcount++;
	}

	/* In case an error occured above, free the decomp to continue testing */
	if (decomp != NULL) {
		free(decomp);
		decomp = NULL;
	}

	/* Create and Free Decomp with elements == 1 */
	fprintf(test_log, "Everything OK (SMIOL_create_decomp) with 1 elements: ");
	n_compute_elements = 1;
	n_io_elements = 1;
	compute_elements = malloc(sizeof(int64_t) * n_compute_elements);
	io_elements = malloc(sizeof(int64_t) * n_io_elements);
	decomp = SMIOL_create_decomp(n_compute_elements, n_io_elements,
	                             compute_elements, io_elements);
	if (decomp != NULL) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - decomp was returned as NULL\n");
		errcount++;
	}
	free(compute_elements);
	free(io_elements);

	fprintf(test_log, "Everything OK (SMIOL_free_decomp) with 1 elements: ");
	ierr = SMIOL_free_decomp(&decomp);
	if (ierr == SMIOL_SUCCESS && decomp == NULL) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_SUCCESS && decomp != NULL) {
		fprintf(test_log, "FAIL - Returned SMIOL_SUCCESS but, decomp was not NULL\n");
		errcount++;
	} else if (ierr != SMIOL_SUCCESS && decomp == NULL) {
		fprintf(test_log, "FAIL - decomp was NULL but did not returned SMIOL_SUCCESS\n");
		errcount++;
	}

	if (decomp != NULL) {
		free(decomp);
		decomp = NULL;
	}

	/* Create and Free Decomp with large amount of elements */
	fprintf(test_log, "Everything OK (SMIOL_create_decomp) with large amount of elements: ");
	n_compute_elements = 100000000;
	n_io_elements = 100000000;
	compute_elements = malloc(sizeof(int64_t) * n_compute_elements);
	io_elements = malloc(sizeof(int64_t) * n_io_elements);
	decomp = SMIOL_create_decomp(n_compute_elements, n_io_elements,
	                             compute_elements, io_elements);
	if (decomp != NULL) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - decomp was returned as NULL\n");
		errcount++;
	}
	free(compute_elements);
	free(io_elements);

	fprintf(test_log, "Everything OK (SMIOL_free_decomp) with large amount of elements: ");
	ierr = SMIOL_free_decomp(&decomp);
	if (ierr == SMIOL_SUCCESS && decomp == NULL) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_SUCCESS && decomp != NULL) {
		fprintf(test_log, "FAIL - Returned SMIOL_SUCCESS but, decomp was not NULL\n");
		errcount++;
	} else if (ierr != SMIOL_SUCCESS && decomp == NULL) {
		fprintf(test_log, "FAIL - decomp was NULL but did not returned SMIOL_SUCCESS\n");
		errcount++;
	}

	if (decomp != NULL) {
		free(decomp);
		decomp = NULL;
	}

	/* Free a decomp that has already been freed */
	fprintf(test_log, "Everything OK (SMIOL_free_decomp) freeing a NULL decomp: ");
	ierr = SMIOL_free_decomp(&decomp);
	if (ierr == SMIOL_SUCCESS && decomp == NULL) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_SUCCESS && decomp != NULL) {
		fprintf(test_log, "FAIL - Returned SMIOL_SUCCESS but, decomp was not NULL\n");
		errcount++;
	} else if (ierr != SMIOL_SUCCESS && decomp == NULL) {
		fprintf(test_log, "FAIL - decomp was NULL but did not returned SMIOL_SUCCESS\n");
		errcount++;
	}

	fflush(test_log);
	ierr = MPI_Barrier(MPI_COMM_WORLD);

	fprintf(test_log, "\n");

	return errcount;
}

int test_dimensions(FILE *test_log)
{
	int ierr;
	int errcount;
	SMIOL_Offset dimsize;
	struct SMIOL_context *context;
	struct SMIOL_file *file;
	SMIOL_Offset expected_dimsize;

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************ SMIOL_define_dim / SMIOL_inquire_dim ******************************\n");
	fprintf(test_log, "\n");

	errcount = 0;

	/* Create a SMIOL context for testing dimension routines */
	context = NULL;
	ierr = SMIOL_init(MPI_COMM_WORLD, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	/* Create a SMIOL file for testing dimension routines */
	file = NULL;
	ierr = SMIOL_open_file(context, "test_dims.nc", SMIOL_FILE_CREATE, &file);
	if (ierr != SMIOL_SUCCESS || file == NULL) {
		fprintf(test_log, "Failed to create SMIOL file...\n");
		return -1;
	}

	/* Handle NULL file handle */
	fprintf(test_log, "Handle NULL file handle (SMIOL_define_dim): ");
	ierr = SMIOL_define_dim(NULL, "invalid_dim", 42);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, when an error was expected\n");
		errcount++;
	}

	/* Handle NULL dimension name */
	fprintf(test_log, "Handle NULL dimension name (SMIOL_define_dim): ");
	ierr = SMIOL_define_dim(file, NULL, 42);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, when an error was expected\n");
		errcount++;
	}

	/* Everything OK for SMIOL_define_dim, unlimited dimension */
	fprintf(test_log, "Everything OK - unlimited dimension (SMIOL_define_dim): ");
	ierr = SMIOL_define_dim(file, "Time", -1);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK for SMIOL_define_dim, small non-record dimension */
	fprintf(test_log, "Everything OK - small non-record dimension (SMIOL_define_dim): ");
	ierr = SMIOL_define_dim(file, "nCells", 40962);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK for SMIOL_define_dim, large non-record dimension */
	fprintf(test_log, "Everything OK - large non-record dimension (SMIOL_define_dim): ");
	ierr = SMIOL_define_dim(file, "nElements", 99999999999);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Handle NULL file handle */
	fprintf(test_log, "Handle NULL file handle (SMIOL_inquire_dim): ");
	ierr = SMIOL_inquire_dim(NULL, "invalid_dim", &dimsize);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, when an error was expected\n");
		errcount++;
	}

	/* Handle NULL dimension name */
	fprintf(test_log, "Handle NULL dimension name (SMIOL_inquire_dim): ");
	ierr = SMIOL_inquire_dim(file, NULL, &dimsize);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, when an error was expected\n");
		errcount++;
	}

	/* Handle NULL dimension size */
	fprintf(test_log, "Handle NULL dimension size (SMIOL_inquire_dim): ");
	ierr = SMIOL_inquire_dim(file, "nCells", NULL);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, when an error was expected\n");
		errcount++;
	}

	/* Handle undefined dimension */
	fprintf(test_log, "Handle undefined dimension (SMIOL_inquire_dim): ");
	ierr = SMIOL_inquire_dim(file, "foobar", &dimsize);
#ifdef SMIOL_PNETCDF
	if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "PASS (%s)\n", SMIOL_lib_error_string(context));
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_LIBRARY_ERROR was not returned\n");
		errcount++;
	}
#else
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned for no-op implementation of SMIOL_inquire_dim\n");
		errcount++;
	}
#endif

	/* Everything OK for SMIOL_inquire_dim, unlimited dimension */
	fprintf(test_log, "Everything OK - unlimited dimension (SMIOL_inquire_dim): ");
	dimsize = (SMIOL_Offset)0;
	ierr = SMIOL_inquire_dim(file, "Time", &dimsize);
	if (ierr == SMIOL_SUCCESS) {
		if (dimsize == (SMIOL_Offset)0) {
			fprintf(test_log, "PASS\n");
		}
		else {
			fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but the dimension size is wrong (got %li, expected %li)\n",
				(long int)dimsize, (long int)0);
			errcount++;
		}
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK for SMIOL_inquire_dim, small non-record dimension */
	fprintf(test_log, "Everything OK - small non-record dimension (SMIOL_inquire_dim): ");
	dimsize = (SMIOL_Offset)0;
#ifdef SMIOL_PNETCDF
	expected_dimsize = (SMIOL_Offset)40962;
#else
	expected_dimsize = (SMIOL_Offset)0;
#endif
	ierr = SMIOL_inquire_dim(file, "nCells", &dimsize);
	if (ierr == SMIOL_SUCCESS) {
		if (dimsize == expected_dimsize) {
			fprintf(test_log, "PASS\n");
		}
		else {
			fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but the dimension size is wrong (got %li, expected %li)\n",
				(long int)dimsize, (long int)expected_dimsize);
			errcount++;
		}
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK for SMIOL_inquire_dim, large non-record dimension */
	fprintf(test_log, "Everything OK - large non-record dimension (SMIOL_inquire_dim): ");
	dimsize = (SMIOL_Offset)0;
#ifdef SMIOL_PNETCDF
	expected_dimsize = (SMIOL_Offset)99999999999;
#else
	expected_dimsize = (SMIOL_Offset)0;
#endif
	ierr = SMIOL_inquire_dim(file, "nElements", &dimsize);
	if (ierr == SMIOL_SUCCESS) {
		if (dimsize == expected_dimsize) {
			fprintf(test_log, "PASS\n");
		}
		else {
			fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but the dimension size is wrong (got %li, expected %li)\n",
				(long int)dimsize, (long int)expected_dimsize);
			errcount++;
		}
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Close the SMIOL file */
	ierr = SMIOL_close_file(&file);
	if (ierr != SMIOL_SUCCESS || file != NULL) {
		fprintf(test_log, "Failed to close SMIOL file...\n");
		return -1;
	}

	/* Re-open the SMIOL file */
	file = NULL;
	ierr = SMIOL_open_file(context, "test_dims.nc", SMIOL_FILE_READ, &file);
	if (ierr != SMIOL_SUCCESS || file == NULL) {
		fprintf(test_log, "Failed to open existing SMIOL file...\n");
		return -1;
	}

	/* Existing file for SMIOL_inquire_dim, unlimited dimension */
	fprintf(test_log, "Existing file - unlimited dimension (SMIOL_inquire_dim): ");
	dimsize = (SMIOL_Offset)0;
	ierr = SMIOL_inquire_dim(file, "Time", &dimsize);
	if (ierr == SMIOL_SUCCESS) {
		if (dimsize == (SMIOL_Offset)0) {
			fprintf(test_log, "PASS\n");
		}
		else {
			fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but the dimension size is wrong (got %li, expected %li)\n",
				(long int)dimsize, (long int)0);
			errcount++;
		}
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Existing file for SMIOL_inquire_dim, small non-record dimension */
	fprintf(test_log, "Existing file - small non-record dimension (SMIOL_inquire_dim): ");
	dimsize = (SMIOL_Offset)0;
#ifdef SMIOL_PNETCDF
	expected_dimsize = (SMIOL_Offset)40962;
#else
	expected_dimsize = (SMIOL_Offset)0;
#endif
	ierr = SMIOL_inquire_dim(file, "nCells", &dimsize);
	if (ierr == SMIOL_SUCCESS) {
		if (dimsize == expected_dimsize) {
			fprintf(test_log, "PASS\n");
		}
		else {
			fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but the dimension size is wrong (got %li, expected %li)\n",
				(long int)dimsize, (long int)expected_dimsize);
			errcount++;
		}
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Existing file for SMIOL_inquire_dim, large non-record dimension */
	fprintf(test_log, "Existing file - large non-record dimension (SMIOL_inquire_dim): ");
	dimsize = (SMIOL_Offset)0;
#ifdef SMIOL_PNETCDF
	expected_dimsize = (SMIOL_Offset)99999999999;
#else
	expected_dimsize = (SMIOL_Offset)0;
#endif
	ierr = SMIOL_inquire_dim(file, "nElements", &dimsize);
	if (ierr == SMIOL_SUCCESS) {
		if (dimsize == expected_dimsize) {
			fprintf(test_log, "PASS\n");
		}
		else {
			fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but the dimension size is wrong (got %li, expected %li)\n",
				(long int)dimsize, (long int)expected_dimsize);
			errcount++;
		}
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Close the SMIOL file */
	ierr = SMIOL_close_file(&file);
	if (ierr != SMIOL_SUCCESS || file != NULL) {
		fprintf(test_log, "Failed to close SMIOL file...\n");
		return -1;
	}

	/* Free the SMIOL context */
	ierr = SMIOL_finalize(&context);
	if (ierr != SMIOL_SUCCESS || context != NULL) {
		fprintf(test_log, "Failed to free SMIOL context...\n");
		return -1;
	}

	fflush(test_log);
	ierr = MPI_Barrier(MPI_COMM_WORLD);

	fprintf(test_log, "\n");

	return errcount;
}

int test_file_sync(FILE *test_log)
{
	int ierr;
	int errcount;
	struct SMIOL_context *context = NULL;
	struct SMIOL_file *file = NULL;

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************************ SMIOL_sync_file unit tests ****************************\n");
	fprintf(test_log, "\n");

	errcount = 0;


	/* Create a SMIOL context for testing file sync routines */
	ierr = SMIOL_init(MPI_COMM_WORLD, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	/* Open a file for syncing */
	ierr = SMIOL_open_file(context, "smiol_sync_file.nc", SMIOL_FILE_CREATE, &file);
	if (ierr != SMIOL_SUCCESS || file == NULL) {
		fprintf(test_log, "Failed to open `smiol_sycn_file.nc\n");
		return -1;
	}

	/* Testing SMIOL_sync_file on a file opened with SMIOL_FILE_CREATE*/
	fprintf(test_log, "Everything OK (SMIOL_sync_file) with SMIOL_FILE_CREATE: ");
	ierr = SMIOL_sync_file(file);
	if (ierr == SMIOL_SUCCESS && file != NULL) {
		fprintf(test_log, "PASS\n");
	} else if (ierr != SMIOL_SUCCESS && file != NULL) {
		fprintf(test_log, "FAIL - file was not NULL, but SMIOL_SUCCESS was not returned\n");
		errcount++;
	} else if (ierr == SMIOL_SUCCESS && file == NULL) {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but file was NULL\n");
		errcount++;
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned and file was NULL\n");
		errcount++;
	}

	/* Close the file */
	ierr = SMIOL_close_file(&file);
	if (ierr != SMIOL_SUCCESS || file != NULL) {
		fprintf(test_log, "Failed to close 'smiol_sync_file.nc'\n");
		return -1;
	}


	/* Testing SMIOL_sync_file on a file opened with SMIOL_FILE_WRITE */
	ierr = SMIOL_open_file(context, "smiol_sync_file.nc", SMIOL_FILE_WRITE, &file);
	if (ierr != SMIOL_SUCCESS || file == NULL) {
		fprintf(test_log, "Failed to open `smiol_sync_file.nc in write mode\n");
		return -1;
	}

	/* Testing SMIOL_sync_file */
	fprintf(test_log, "Everything OK (SMIOL_sync_file) with SMIOL_FILE_WRITE: ");
	ierr = SMIOL_sync_file(file);
	if (ierr == SMIOL_SUCCESS && file != NULL) {
		fprintf(test_log, "PASS\n");
	} else if (ierr != SMIOL_SUCCESS && file != NULL) {
		fprintf(test_log, "FAIL - file was not NULL, but SMIOL_SUCCESS was not returned\n");
		errcount++;
	} else if (ierr == SMIOL_SUCCESS && file == NULL) {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but file was NULL\n");
		errcount++;
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned and file was NULL\n");
		errcount++;
	}

	/* Close the file */
	ierr = SMIOL_close_file(&file);
	if (ierr != SMIOL_SUCCESS || file != NULL) {
		fprintf(test_log, "Failed to close 'smiol_sync_file.nc'\n");
		return -1;
	}

	/* Testing SMIOL_sync_file on a file opened with SMIOL_FILE_READ*/
	ierr = SMIOL_open_file(context, "smiol_sync_file.nc", SMIOL_FILE_READ, &file);
	if (ierr != SMIOL_SUCCESS || file == NULL) {
		fprintf(test_log, "Failed to open `smiol_sync_file.nc with SMIOL_FILE_READ\n");
		return -1;
	}

	/* Testing SMIOL_sync_file */
	fprintf(test_log, "Everything OK (SMIOL_sync_file) with SMIOL_FILE_READ: ");
	ierr = SMIOL_sync_file(file);
	if (ierr == SMIOL_SUCCESS && file != NULL) {
		fprintf(test_log, "PASS\n");
	} else if (ierr != SMIOL_SUCCESS && file != NULL) {
		fprintf(test_log, "FAIL - file was not NULL, but SMIOL_SUCCESS was not returned\n");
		errcount++;
	} else if (ierr == SMIOL_SUCCESS && file == NULL) {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but file was NULL\n");
		errcount++;
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned and file was NULL\n");
		errcount++;
	}

	/* Close the file */
	ierr = SMIOL_close_file(&file);
	if (ierr != SMIOL_SUCCESS || file != NULL) {
		fprintf(test_log, "Failed to close 'smiol_sync_file.nc'\n");
		return -1;
	}


#ifdef SMIOL_PNETCDF
	/* Testing a file that was never opened */
	fprintf(test_log, "Try to sync a file that was never opened: ");
	file = (struct SMIOL_file *)malloc(sizeof(struct SMIOL_file));
	file->context = context;
	file->state = -42;	// Erroneous, currently unused, state
	ierr = SMIOL_sync_file(file);
	if (ierr == SMIOL_LIBRARY_ERROR && file != NULL) {
		fprintf(test_log, "PASS (%s)\n",
			SMIOL_lib_error_string(context));
	} else {
		fprintf(test_log, "FAIL - expected error code of SMIOL_LIBRARY_ERROR not returned, or file was not NULL\n");
		errcount++;
	}
	free(file);
#endif

	/* Testing SMIOL_sync_file with a NULL file pointer*/
	file = NULL;
	fprintf(test_log, "Testing SMIOL_sync_file with a NULL file pointer: ");
	ierr = SMIOL_sync_file(file);
	if (ierr == SMIOL_INVALID_ARGUMENT && file == NULL) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - Expected error code SMIOL_INVALID_ARGUMENT not returned or file was not NULL\n");
		errcount++;
	}

	ierr = SMIOL_finalize(&context);
	if (ierr != SMIOL_SUCCESS || context != NULL) {
		fprintf(test_log, "Failed to free SMIOL context...\n");
		return -1;
	}

	fflush(test_log);
	ierr = MPI_Barrier(MPI_COMM_WORLD);

	fprintf(test_log, "\n");

	return errcount;

}

