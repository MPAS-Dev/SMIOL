#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "smiol.h"
#include "smiol_utils.h"

/*******************************************************************************
 * SMIOL C Runner - Take SMIOL out for a run!
 *******************************************************************************/

int test_init_finalize(FILE *test_log);
int test_open_close(FILE *test_log);
int test_dimensions(FILE *test_log);
int test_variables(FILE *test_log);
int test_decomp(FILE *test_log);
int test_file_sync(FILE *test_log);
int test_utils(FILE *test_log);
int compare_decomps(struct SMIOL_decomp *decomp,
                    size_t n_comp_list, SMIOL_Offset *comp_list_correct,
                    size_t n_io_list, SMIOL_Offset *io_list_correct);

int main(int argc, char **argv)
{
	int ierr;
	int my_proc_id;
	SMIOL_Offset dimsize;
	size_t n_compute_elements = 1;
	size_t n_io_elements = 1;
	SMIOL_Offset *compute_elements;
	SMIOL_Offset *io_elements;
	struct SMIOL_decomp *decomp = NULL;
	struct SMIOL_context *context = NULL;
	struct SMIOL_file *file = NULL;
	char log_fname[17];
	FILE *test_log = NULL;
	char **dimnames;

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
	 * Unit tests for variables
	 */
	ierr = test_variables(test_log);
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

	/*
	 * Unit tests for SMIOL utilities
	 */
	ierr = test_utils(test_log);
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
	compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
	io_elements = malloc(sizeof(SMIOL_Offset) * n_io_elements);

	ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, n_io_elements, io_elements, &decomp);
	if (ierr != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_create_decomp - SMIOL_SUCCESS was not returned\n");
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

	dimnames = (char **)malloc((size_t)2 * sizeof(char *));
	dimnames[0] = (char *)malloc((size_t)64 * sizeof(char));
	dimnames[1] = (char *)malloc((size_t)64 * sizeof(char));
	snprintf(dimnames[0], 64, "Time");
	snprintf(dimnames[1], 64, "nCells");
	if ((ierr = SMIOL_define_var(file, "theta", SMIOL_REAL32, 2, (const char**)dimnames)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_define_var: %s ", SMIOL_error_string(ierr));
		return 1;
	}
	free(dimnames[0]);
	free(dimnames[1]);
	free(dimnames);

	if ((ierr = SMIOL_close_file(&file)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_close_file: %s ", SMIOL_error_string(ierr));
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
	int comm_rank;
	int comm_size;
	size_t i;
	size_t n_compute_elements, n_io_elements;
	SMIOL_Offset *compute_elements = NULL, *io_elements = NULL;
	struct SMIOL_context *context = NULL;
	struct SMIOL_decomp *decomp = NULL;

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************ SMIOL_create_decomp / SMIOL_free_decomp unit tests ****************\n");
	fprintf(test_log, "\n");

	ierr = MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);
	if (ierr != MPI_SUCCESS) {
		fprintf(test_log, "Failed to get MPI rank...\n");
		return -1;
	}

	ierr = MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	if (ierr != MPI_SUCCESS) {
		fprintf(test_log, "Failed to get MPI size...\n");
		return -1;
	}

	/* Create a SMIOL context for testing decomp routines */
	ierr = SMIOL_init(MPI_COMM_WORLD, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	/* Test create decomp with io_elements and compute_elements == NULL and
	 * n_io_elements and n_compute != 0 */
	fprintf(test_log, "Testing SMIOL_create_decomp with NULL elements and n_elements != 0: ");
	n_compute_elements = 1;
	n_io_elements = 1;
	ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, n_io_elements, io_elements, &decomp);
	if (ierr == SMIOL_INVALID_ARGUMENT && decomp == NULL) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - Either SMIOL_INVALID_ARGUMENT was not returned or DECOMP was not NULL\n");
		errcount++;
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
	compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
	io_elements = malloc(sizeof(SMIOL_Offset) * n_io_elements);
	ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, n_io_elements, io_elements, &decomp);
	if (ierr == SMIOL_SUCCESS && decomp != NULL) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or decomp was NULL\n");
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
	compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
	io_elements = malloc(sizeof(SMIOL_Offset) * n_io_elements);
	ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, n_io_elements, io_elements, &decomp);
	if (ierr == SMIOL_SUCCESS && decomp != NULL) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or decomp was NULL\n");
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
	n_compute_elements = 1000000;
	n_io_elements = 1000000;
	compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
	io_elements = malloc(sizeof(SMIOL_Offset) * n_io_elements);
	ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, n_io_elements, io_elements, &decomp);
	if (ierr == SMIOL_SUCCESS && decomp != NULL) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or decomp was NULL\n");
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

	/*
	 * The following tests will only be run if there are exactly two MPI tasks.
	 * In principle, as long as there are at least two MPI ranks in MPI_COMM_WORLD,
	 * an intracommunicator with exactly two ranks could be created for these tests.
	 */
	if (comm_size == 2) {

		/* Even/odd compute, half/half I/O */
		fprintf(test_log, "Even/odd compute, half/half I/O: ");
		n_compute_elements = 4;
		n_io_elements = 4;
		compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
		io_elements = malloc(sizeof(SMIOL_Offset) * n_io_elements);

		if (comm_rank == 0) {
			for (i = 0; i < n_compute_elements; i++) {
				compute_elements[i] = (SMIOL_Offset)(2 * i + 1);    /* Odd elements */
			}
			for (i = 0; i < n_io_elements; i++) {
				io_elements[i] = (SMIOL_Offset)i;                   /* First half of elements */
			}
		} else {
			for (i = 0; i < n_compute_elements; i++) {
				compute_elements[i] = (SMIOL_Offset)(2 * i);        /* Even elements */
			}
			for (i = 0; i < n_io_elements; i++) {
				io_elements[i] = (SMIOL_Offset)(4 + i);             /* Second half of elements */
			}
		}
		ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, n_io_elements, io_elements, &decomp);
		if (ierr == SMIOL_SUCCESS && decomp != NULL) {

			/* The correct comp_list and io_list arrays, below, were verified manually */
			if (comm_rank == 0) {
				SMIOL_Offset comp_list_correct[] = { 2, 0, 2, 0, 1, 1, 2, 2, 3 };
				SMIOL_Offset io_list_correct[] = { 2, 0, 2, 1, 3, 1, 2, 0, 2 };

				ierr = compare_decomps(decomp, (size_t)9, comp_list_correct, (size_t)9, io_list_correct);
			} else {
				SMIOL_Offset comp_list_correct[] = { 2, 0, 2, 0, 1, 1, 2, 2, 3 };
				SMIOL_Offset io_list_correct[] = { 2, 0, 2, 1, 3, 1, 2, 0, 2 };

				ierr = compare_decomps(decomp, (size_t)9, comp_list_correct, (size_t)9, io_list_correct);
			}

			if (ierr == 0) {
				fprintf(test_log, "PASS\n");
			} else {
				fprintf(test_log, "FAIL - the decomp did not contain the expected values\n");
				errcount++;
			}
		} else {
			fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or decomp was NULL\n");
			errcount++;
		}

		free(compute_elements);
		free(io_elements);

		ierr = SMIOL_free_decomp(&decomp);
		if (ierr != SMIOL_SUCCESS || decomp != NULL) {
			fprintf(test_log, "After previous unit test, SMIOL_free_decomp was unsuccessful: FAIL\n");
			errcount++;
		}


		/* Even/odd compute, nothing/all I/O */
		fprintf(test_log, "Even/odd compute, nothing/all I/O: ");
		n_compute_elements = 4;
		if (comm_rank == 0) {
			n_io_elements = 0;
		} else {
			n_io_elements = 8;
		}
		compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
		io_elements = malloc(sizeof(SMIOL_Offset) * n_io_elements);

		if (comm_rank == 0) {
			for (i = 0; i < n_compute_elements; i++) {
				compute_elements[i] = (SMIOL_Offset)(2 * i + 1);    /* Odd elements */
			}

			/* No I/O elements */
		} else {
			for (i = 0; i < n_compute_elements; i++) {
				compute_elements[i] = (SMIOL_Offset)(2 * i);        /* Even elements */
			}

			for (i = 0; i < n_io_elements; i++) {
				io_elements[i] = (SMIOL_Offset)i;                   /* All I/O elements */
			}
		}
		ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, n_io_elements, io_elements, &decomp);
		if (ierr == SMIOL_SUCCESS && decomp != NULL) {

			/* The correct comp_list and io_list arrays, below, were verified manually */
			if (comm_rank == 0) {
				SMIOL_Offset comp_list_correct[] = { 1, 1, 4, 0, 1, 2, 3 };
				SMIOL_Offset io_list_correct[] = { 0 };

				ierr = compare_decomps(decomp, (size_t)7, comp_list_correct, (size_t)1, io_list_correct);
			} else {
				SMIOL_Offset comp_list_correct[] = { 1, 1, 4, 0, 1, 2, 3 };
				SMIOL_Offset io_list_correct[] = { 2, 0, 4, 1, 3, 5, 7, 1, 4, 0, 2, 4, 6 };

				ierr = compare_decomps(decomp, (size_t)7, comp_list_correct, (size_t)13, io_list_correct);
			}

			if (ierr == 0) {
				fprintf(test_log, "PASS\n");
			} else {
				fprintf(test_log, "FAIL - the decomp did not contain the expected values\n");
				errcount++;
			}
		} else {
			fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or decomp was NULL\n");
			errcount++;
		}
		free(compute_elements);
		free(io_elements);

		ierr = SMIOL_free_decomp(&decomp);
		if (ierr != SMIOL_SUCCESS || decomp != NULL) {
			fprintf(test_log, "After previous unit test, SMIOL_free_decomp was unsuccessful: FAIL\n");
			errcount++;
		}


		/* All/nothing compute, nothing/all I/O */
		fprintf(test_log, "All/nothing compute, nothing/all I/O: ");
		if (comm_rank == 0) {
			n_compute_elements = 8;
			n_io_elements = 0;
		} else {
			n_compute_elements = 0;
			n_io_elements = 8;
		}
		compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
		io_elements = malloc(sizeof(SMIOL_Offset) * n_io_elements);

		if (comm_rank == 0) {
			for (i = 0; i < n_compute_elements; i++) {
				compute_elements[i] = (SMIOL_Offset)(n_compute_elements - 1 - i);    /* All compute elements */
			}

			/* No I/O elements */
		} else {
			/* No compute elements */

			for (i = 0; i < n_io_elements; i++) {
				io_elements[i] = (SMIOL_Offset)i;                   /* All I/O elements */
			}
		}
		ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, n_io_elements, io_elements, &decomp);
		if (ierr == SMIOL_SUCCESS && decomp != NULL) {

			/* The correct comp_list and io_list arrays, below, were verified manually */
			if (comm_rank == 0) {
				SMIOL_Offset comp_list_correct[] = { 1, 1, 8, 7, 6, 5, 4, 3, 2, 1, 0 };
				SMIOL_Offset io_list_correct[] = { 0 };

				ierr = compare_decomps(decomp, (size_t)11, comp_list_correct, (size_t)1, io_list_correct);
			} else {
				SMIOL_Offset comp_list_correct[] = { 0 };
				SMIOL_Offset io_list_correct[] = { 1, 0, 8, 0, 1, 2, 3, 4, 5, 6, 7 };

				ierr = compare_decomps(decomp, (size_t)1, comp_list_correct, (size_t)11, io_list_correct);
			}

			if (ierr == 0) {
				fprintf(test_log, "PASS\n");
			} else {
				fprintf(test_log, "FAIL - the decomp did not contain the expected values\n");
				errcount++;
			}
		} else {
			fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or decomp was NULL\n");
			errcount++;
		}
		free(compute_elements);
		free(io_elements);

		ierr = SMIOL_free_decomp(&decomp);
		if (ierr != SMIOL_SUCCESS || decomp != NULL) {
			fprintf(test_log, "After previous unit test, SMIOL_free_decomp was unsuccessful: FAIL\n");
			errcount++;
		}

	} else {
		fprintf(test_log, "<<< Tests that require exactly 2 MPI tasks will not be run >>>\n");
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

int test_variables(FILE *test_log)
{
	int errcount;
	int ierr;
	int i;
	struct SMIOL_context *context;
	struct SMIOL_file *file;
	char **dimnames;
	int ndims;
	int vartype;

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************ SMIOL_define_var / SMIOL_inquire_var ******************************\n");
	fprintf(test_log, "\n");

	errcount = 0;

	/* Create a SMIOL context for testing variable routines */
	context = NULL;
	ierr = SMIOL_init(MPI_COMM_WORLD, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	/* Create a SMIOL file for testing variable routines */
	file = NULL;
	ierr = SMIOL_open_file(context, "test_vars.nc", SMIOL_FILE_CREATE, &file);
	if (ierr != SMIOL_SUCCESS || file == NULL) {
		fprintf(test_log, "Failed to create SMIOL file...\n");
		return -1;
	}

        /* Define several dimensions in the file to be used when defining variables */
	ierr = SMIOL_define_dim(file, "Time", (SMIOL_Offset)-1);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create dimension Time...\n");
		return -1;
	}

	ierr = SMIOL_define_dim(file, "nCells", (SMIOL_Offset)40962);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create dimension nCells...\n");
		return -1;
	}

	ierr = SMIOL_define_dim(file, "nVertLevels", (SMIOL_Offset)55);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create dimension nVertLevels...\n");
		return -1;
	}

	ierr = SMIOL_define_dim(file, "maxEdges", (SMIOL_Offset)6);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create dimension maxEdges...\n");
		return -1;
	}

	ierr = SMIOL_define_dim(file, "TWO", (SMIOL_Offset)2);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create dimension TWO...\n");
		return -1;
	}

	ierr = SMIOL_define_dim(file, "nMonths", (SMIOL_Offset)12);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create dimension nMonths...\n");
		return -1;
	}

	ierr = SMIOL_define_dim(file, "StrLen", (SMIOL_Offset)512);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create dimension StrLen...\n");
		return -1;
	}

	dimnames = (char **)malloc(sizeof(char *) * (size_t)6);
	for (i = 0; i < 6; i++) {
		dimnames[i] = (char *)malloc(sizeof(char) * (size_t)32);
	}

	/* Define a 32-bit real variable with zero dimensions */
	fprintf(test_log, "Define a 32-bit real variable with zero dimensions: ");
	ierr = SMIOL_define_var(file, "r0", SMIOL_REAL32, 0, NULL);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "FAIL - a library-specific error was returned (%s)\n", SMIOL_lib_error_string(context));
		errcount++;
	} else {
		fprintf(test_log, "FAIL - %s\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Define a 32-bit real variable with one dimension that is a record dimension */
	fprintf(test_log, "Define a 32-bit real variable with only a record dimension: ");
	snprintf(dimnames[0], 32, "Time");
	ierr = SMIOL_define_var(file, "r0_t", SMIOL_REAL32, 1, (const char **)dimnames);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "FAIL - a library-specific error was returned (%s)\n", SMIOL_lib_error_string(context));
		errcount++;
	} else {
		fprintf(test_log, "FAIL - %s\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Define a 32-bit real variable with one dimension that is *not* a record dimension */
	fprintf(test_log, "Define a 32-bit real variable with one non-record dimension: ");
	snprintf(dimnames[0], 32, "nCells");
	ierr = SMIOL_define_var(file, "r1", SMIOL_REAL32, 1, (const char **)dimnames);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "FAIL - a library-specific error was returned (%s)\n", SMIOL_lib_error_string(context));
		errcount++;
	} else {
		fprintf(test_log, "FAIL - %s\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Define a 32-bit real variable with one non-record dimension and a record dimension */
	fprintf(test_log, "Define a 32-bit real variable with one non-record dimension and a record dimension: ");
	snprintf(dimnames[0], 32, "Time");
	snprintf(dimnames[1], 32, "nCells");
	ierr = SMIOL_define_var(file, "r1_t", SMIOL_REAL32, 2, (const char **)dimnames);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "FAIL - a library-specific error was returned (%s)\n", SMIOL_lib_error_string(context));
		errcount++;
	} else {
		fprintf(test_log, "FAIL - %s\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Define a 32-bit real variable with five dimensions, none of which is a record dimension */
	fprintf(test_log, "Define a 32-bit real variable with five non-record dimension: ");
	snprintf(dimnames[0], 32, "nCells");
	snprintf(dimnames[1], 32, "nVertLevels");
	snprintf(dimnames[2], 32, "maxEdges");
	snprintf(dimnames[3], 32, "TWO");
	snprintf(dimnames[4], 32, "nMonths");
	ierr = SMIOL_define_var(file, "r5", SMIOL_REAL32, 5, (const char **)dimnames);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "FAIL - a library-specific error was returned (%s)\n", SMIOL_lib_error_string(context));
		errcount++;
	} else {
		fprintf(test_log, "FAIL - %s\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Define a 32-bit real variable with five non-record dimensions and a record dimension */
	fprintf(test_log, "Define a 32-bit real variable with five non-record dimension and a record dimension: ");
	snprintf(dimnames[0], 32, "Time");
	snprintf(dimnames[1], 32, "nCells");
	snprintf(dimnames[2], 32, "nVertLevels");
	snprintf(dimnames[3], 32, "maxEdges");
	snprintf(dimnames[4], 32, "TWO");
	snprintf(dimnames[5], 32, "nMonths");
	ierr = SMIOL_define_var(file, "r5_t", SMIOL_REAL32, 6, (const char **)dimnames);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "FAIL - a library-specific error was returned (%s)\n", SMIOL_lib_error_string(context));
		errcount++;
	} else {
		fprintf(test_log, "FAIL - %s\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Define a 64-bit real variable with five non-record dimension and a record dimension */
	fprintf(test_log, "Define a 64-bit real variable with five non-record dimension and a record dimension: ");
	snprintf(dimnames[0], 32, "Time");
	snprintf(dimnames[1], 32, "nCells");
	snprintf(dimnames[2], 32, "nVertLevels");
	snprintf(dimnames[3], 32, "maxEdges");
	snprintf(dimnames[4], 32, "TWO");
	snprintf(dimnames[5], 32, "nMonths");
	ierr = SMIOL_define_var(file, "d5_t", SMIOL_REAL64, 6, (const char **)dimnames);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "FAIL - a library-specific error was returned (%s)\n", SMIOL_lib_error_string(context));
		errcount++;
	} else {
		fprintf(test_log, "FAIL - %s\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Define a 32-bit int variable with five non-record dimension and a record dimension */
	fprintf(test_log, "Define a 32-bit int variable with five non-record dimension and a record dimension: ");
	snprintf(dimnames[0], 32, "Time");
	snprintf(dimnames[1], 32, "nCells");
	snprintf(dimnames[2], 32, "nVertLevels");
	snprintf(dimnames[3], 32, "maxEdges");
	snprintf(dimnames[4], 32, "TWO");
	snprintf(dimnames[5], 32, "nMonths");
	ierr = SMIOL_define_var(file, "i5_t", SMIOL_INT32, 6, (const char **)dimnames);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "FAIL - a library-specific error was returned (%s)\n", SMIOL_lib_error_string(context));
		errcount++;
	} else {
		fprintf(test_log, "FAIL - %s\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Define a char variable with one non-record dimension and a record dimension */
	fprintf(test_log, "Define a character variable with one non-record dimension and a record dimension: ");
	snprintf(dimnames[0], 32, "Time");
	snprintf(dimnames[1], 32, "StrLen");
	ierr = SMIOL_define_var(file, "c1_t", SMIOL_CHAR, 2, (const char **)dimnames);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "FAIL - a library-specific error was returned (%s)\n", SMIOL_lib_error_string(context));
		errcount++;
	} else {
		fprintf(test_log, "FAIL - %s\n", SMIOL_error_string(ierr));
		errcount++;
	}

#ifdef SMIOL_PNETCDF
	/* Try to re-define a variable that already exists */
	fprintf(test_log, "Try to re-define a variable that already exists: ");
	snprintf(dimnames[0], 32, "Time");
	snprintf(dimnames[1], 32, "nCells");
	ierr = SMIOL_define_var(file, "c1_t", SMIOL_CHAR, 2, (const char **)dimnames);
	if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "PASS (%s)\n", SMIOL_lib_error_string(context));
	} else if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was erroneously returned\n");
		errcount++;
	} else {
		fprintf(test_log, "FAIL - a return code of SMIOL_LIBRARY_ERROR was expected\n");
		errcount++;
	}

	/* Try to define a variable with undefined dimension */
	fprintf(test_log, "Try to define a variable with an undefined dimension: ");
	snprintf(dimnames[0], 32, "Time");
	snprintf(dimnames[1], 32, "foobar");
	snprintf(dimnames[2], 32, "nVertLevels");
	ierr = SMIOL_define_var(file, "should_not_exist", SMIOL_INT32, 3, (const char **)dimnames);
	if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "PASS (%s)\n", SMIOL_lib_error_string(context));
	} else if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was erroneously returned\n");
		errcount++;
	} else {
		fprintf(test_log, "FAIL - a return code of SMIOL_LIBRARY_ERROR was expected\n");
		errcount++;
	}
#endif

	/* Try to define a variable with a NULL variable name argument */
	fprintf(test_log, "Try to define a variable with a NULL variable name argument: ");
	snprintf(dimnames[0], 32, "Time");
	snprintf(dimnames[1], 32, "nCells");
	snprintf(dimnames[2], 32, "nVertLevels");
	ierr = SMIOL_define_var(file, NULL, SMIOL_INT32, 3, (const char **)dimnames);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - a return code of SMIOL_INVALID_ARGUMENT was expected\n");
	}

	/* Try to define a variable with a NULL file argument */
	fprintf(test_log, "Try to define a variable with a NULL file argument: ");
	snprintf(dimnames[0], 32, "Time");
	snprintf(dimnames[1], 32, "nCells");
	snprintf(dimnames[2], 32, "nVertLevels");
	ierr = SMIOL_define_var(NULL, "should_not_exist", SMIOL_INT32, 3, (const char **)dimnames);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - a return code of SMIOL_INVALID_ARGUMENT was expected\n");
	}

	/* Try to define a variable with a NULL dimension list */
	fprintf(test_log, "Try to define a variable with a NULL dimension list: ");
	ierr = SMIOL_define_var(file, "should_not_exist", SMIOL_INT32, 3, NULL);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - a return code of SMIOL_INVALID_ARGUMENT was expected\n");
	}

#ifdef SMIOL_PNETCDF
	/* Try to define a variable with an invalid type */
	fprintf(test_log, "Try to define a variable with an invalid type: ");
	snprintf(dimnames[0], 32, "Time");
	snprintf(dimnames[1], 32, "nCells");
	snprintf(dimnames[2], 32, "nVertLevels");
	ierr = SMIOL_define_var(file, "should_not_exist",
	                        ~(SMIOL_REAL32 | SMIOL_REAL64 | SMIOL_INT32 | SMIOL_CHAR),
	                        3, (const char **)dimnames);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - a return code of SMIOL_INVALID_ARGUMENT was expected\n");
	}
#endif

	/* Close the SMIOL file */
	ierr = SMIOL_close_file(&file);
	if (ierr != SMIOL_SUCCESS || file != NULL) {
		fprintf(test_log, "Failed to close SMIOL file...\n");
		return -1;
	}

	/* Re-open the SMIOL file */
	file = NULL;
	ierr = SMIOL_open_file(context, "test_vars.nc", SMIOL_FILE_READ, &file);
	if (ierr != SMIOL_SUCCESS || file == NULL) {
		fprintf(test_log, "Failed to re-open SMIOL file...\n");
		return -1;
	}

#ifdef SMIOL_PNETCDF
	/* Inquire about just the number of dimensions for a variable */
	fprintf(test_log, "Inquire about just the number of dimensions for a variable: ");
	ndims = -1;
	ierr = SMIOL_inquire_var(file, "r0_t", NULL, &ndims, NULL);
	if (ierr == SMIOL_SUCCESS && ndims == 1) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_SUCCESS && ndims != 1) {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but the number of dimensions was wrong\n");
		errcount++;
	} else if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "FAIL (%s)\n", SMIOL_lib_error_string(context));
		errcount++;
	} else {
		fprintf(test_log, "FAIL - %s\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Inquire about just the type of a variable */
	fprintf(test_log, "Inquire about just the type of a variable: ");
	vartype = SMIOL_UNKNOWN_VAR_TYPE;
	ierr = SMIOL_inquire_var(file, "r5_t", &vartype, NULL, NULL);
	if (ierr == SMIOL_SUCCESS && vartype == SMIOL_REAL32) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_SUCCESS && vartype != SMIOL_REAL32) {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but the variable type was wrong\n");
		errcount++;
	} else if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "FAIL (%s)\n", SMIOL_lib_error_string(context));
		errcount++;
	} else {
		fprintf(test_log, "FAIL - %s\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Inquire about just the dimension names for a variable */
	fprintf(test_log, "Inquire about just the dimension names for a variable: ");
	snprintf(dimnames[0], 32, "----------");
	snprintf(dimnames[1], 32, "----------");
	ierr = SMIOL_inquire_var(file, "r1_t", NULL, NULL, dimnames);
	if (ierr == SMIOL_SUCCESS &&
	    strncmp(dimnames[0], "Time", 32) == 0 &&
	    strncmp(dimnames[1], "nCells", 32) == 0) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but the dimension names were wrong\n");
		errcount++;
	} else if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "FAIL (%s)\n", SMIOL_lib_error_string(context));
		errcount++;
	} else {
		fprintf(test_log, "FAIL - %s\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Inquire about all properties of a variable */
	fprintf(test_log, "Inquire about all properties of a variable: ");
	vartype = SMIOL_UNKNOWN_VAR_TYPE;
	ndims = -1;
	snprintf(dimnames[0], 32, "----------");
	snprintf(dimnames[1], 32, "----------");
	ierr = SMIOL_inquire_var(file, "c1_t", &vartype, &ndims, dimnames);
	if (ierr == SMIOL_SUCCESS &&
	    ndims == 2 &&
	    vartype == SMIOL_CHAR &&
	    strncmp(dimnames[0], "Time", 32) == 0 &&
	    strncmp(dimnames[1], "StrLen", 32) == 0) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but one or more properties were wrong\n");
		errcount++;
	} else if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "FAIL (%s)\n", SMIOL_lib_error_string(context));
		errcount++;
	} else {
		fprintf(test_log, "FAIL - %s\n", SMIOL_error_string(ierr));
		errcount++;
	}
#endif

	/* Inquire about none of the properties of a variable */
	fprintf(test_log, "Inquire about none of the properties of a variable: ");
	ierr = SMIOL_inquire_var(file, "i5_t", NULL, NULL, NULL);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "FAIL (%s)\n", SMIOL_lib_error_string(context));
		errcount++;
	} else {
		fprintf(test_log, "FAIL - %s\n", SMIOL_error_string(ierr));
		errcount++;
	}

#ifdef SMIOL_PNETCDF
	/* Try to inquire about an undefined variable */
	fprintf(test_log, "Try to inquire about an undefined variable: ");
	ierr = SMIOL_inquire_var(file, "fooblaz", &vartype, &ndims, dimnames);
	if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "PASS (%s)\n", SMIOL_lib_error_string(context));
	} else if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was erroneously returned\n");
		errcount++;
	} else {
		fprintf(test_log, "FAIL - a return code of SMIOL_LIBRARY_ERROR was expected\n");
		errcount++;
	}
#endif

	/* Try to inquire with NULL variable name argument */
	fprintf(test_log, "Try to inquire with a NULL variable name argument: ");
	ierr = SMIOL_inquire_var(file, NULL, &vartype, &ndims, dimnames);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - a return code of SMIOL_INVALID_ARGUMENT was expected\n");
		errcount++;
	}

	/* Try to inquire with NULL file argument */
	fprintf(test_log, "Try to inquire with a NULL file argument: ");
	ierr = SMIOL_inquire_var(NULL, "i5_t", &vartype, &ndims, dimnames);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - a return code of SMIOL_INVALID_ARGUMENT was expected\n");
		errcount++;
	}

	for (i = 0; i < 6; i++) {
		free(dimnames[i]);
	}
	free(dimnames);

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

int test_utils(FILE *test_log)
{
	int errcount;
	int entry;
	size_t uentry;
	size_t n_arr;
	SMIOL_Offset *arr;
	SMIOL_Offset key;
	SMIOL_Offset smallest, largest;
	SMIOL_Offset *res;
	size_t i;
	const SMIOL_Offset RANDMAX = (SMIOL_Offset)1000000000;


	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************************ SMIOL utilities unit tests ****************************\n");
	fprintf(test_log, "\n");

	errcount = 0;

	/*
	 * Test sorting and searching on each of the entries in a triplet
	 */
	for (entry = 0; entry < 3; entry++) {

		/*
		 * Some compilers will complain about change of signedness when
		 * using entry as an offset to a size_t, below, so uentry is
		 * used when indexing arr[]
		 */
		uentry = (size_t)entry;

		/* Testing sort of an empty list of triplets on entry N */
		fprintf(test_log, "Testing sort of an empty list of triplets on entry %i: ", entry);
		n_arr = (size_t)0;
		arr = NULL;
		sort_triplet_array(n_arr, arr, entry);
		if (arr == NULL) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL\n");
			errcount++;
		}

		/* Testing search of an empty list of triplets on entry N */
		fprintf(test_log, "Testing search of an empty list of triplets on entry %i: ", entry);
		n_arr = (size_t)0;
		arr = NULL;
		key = (SMIOL_Offset)42;
		res = search_triplet_array(key, n_arr, arr, entry);
		if (res == NULL) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL\n");
			errcount++;
		}

		n_arr = (size_t)1;
		arr = (SMIOL_Offset *)malloc(sizeof(SMIOL_Offset) * TRIPLET_SIZE);
		arr[TRIPLET_SIZE*0 + 0] = 1;
		arr[TRIPLET_SIZE*0 + 1] = 2;
		arr[TRIPLET_SIZE*0 + 2] = 3;

		smallest = arr[TRIPLET_SIZE*0 + uentry];

		/* Testing sort of a list of a single triplet on entry N */
		fprintf(test_log, "Testing sort of a list of a single triplet on entry %i: ", entry);
		sort_triplet_array(n_arr, arr, entry);
		if (arr[TRIPLET_SIZE*0 + uentry] == smallest) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL\n");
			errcount++;
		}

		/* Testing successful search of a list of a single triplet on entry N */
		fprintf(test_log, "Testing successful search of a list of a single triplet on entry %i: ", entry);
		key = smallest;
		res = search_triplet_array(key, n_arr, arr, entry);
		if (res == &arr[TRIPLET_SIZE*0]) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL (returned address was wrong)\n");
			errcount++;
		}

		/* Testing un-successful search of a list of a single triplet on entry N */
		fprintf(test_log, "Testing un-successful search of a list of a single triplet on entry %i: ", entry);
		key = 42;
		res = search_triplet_array(key, n_arr, arr, entry);
		if (res == NULL) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL (returned address not NULL)\n");
			errcount++;
		}

		free(arr);

		n_arr = (size_t)2;
		arr = (SMIOL_Offset *)malloc(n_arr * sizeof(SMIOL_Offset) * TRIPLET_SIZE);
		arr[TRIPLET_SIZE*0 + 0] = 7;
		arr[TRIPLET_SIZE*0 + 1] = 8;
		arr[TRIPLET_SIZE*0 + 2] = 9;
		arr[TRIPLET_SIZE*1 + 0] = 1;
		arr[TRIPLET_SIZE*1 + 1] = 2;
		arr[TRIPLET_SIZE*1 + 2] = 3;

		largest = arr[TRIPLET_SIZE*0 + uentry];
		smallest = arr[TRIPLET_SIZE*1 + uentry];

		/* Testing sort of a list of two triplets on entry N */
		fprintf(test_log, "Testing sort of a list of two triplets on entry %i: ", entry);
		sort_triplet_array(n_arr, arr, entry);
		if (arr[TRIPLET_SIZE*0 + uentry] == smallest && arr[TRIPLET_SIZE*1 + uentry] == largest) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL\n");
			errcount++;
		}

		/* Testing successful search of a list of two triplets on entry N */
		fprintf(test_log, "Testing successful search of a list of two triplets on entry %i: ", entry);
		key = largest;
		res = search_triplet_array(key, n_arr, arr, entry);
		if (res == &arr[TRIPLET_SIZE*1]) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL (returned address was wrong)\n");
			errcount++;
		}

		/* Testing un-successful search of a list of two triplets on entry N */
		fprintf(test_log, "Testing un-successful search of a list of two triplets on entry %i: ", entry);
		key = 42;
		res = search_triplet_array(key, n_arr, arr, entry);
		if (res == NULL) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL (returned address not NULL)\n");
			errcount++;
		}

		free(arr);

		n_arr = (size_t)10000;
		arr = (SMIOL_Offset *)malloc(n_arr * sizeof(SMIOL_Offset) * TRIPLET_SIZE);
		srand(42);
		for (i = 0; i < (TRIPLET_SIZE * n_arr); i++) {
			arr[i] = rand() % RANDMAX;
		}

		/* Testing sort of a list of 10000 triplets on entry N */
		fprintf(test_log, "Testing sort of a list of 10000 triplets on entry %i: ", entry);
		sort_triplet_array(n_arr, arr, entry);
		for (i = 1; i < n_arr; i++) {
			if (arr[TRIPLET_SIZE * i + uentry] < arr[TRIPLET_SIZE * (i - 1) + uentry]) {
				fprintf(test_log, "FAIL\n");
				errcount++;
				break;
			}
		}
		if (i == n_arr) {
			fprintf(test_log, "PASS\n");
		}

		/* Testing successful search of a list of 10000 triplets on entry N */
		fprintf(test_log, "Testing successful search of a list of 10000 triplets on entry %i: ", entry);
		key = arr[TRIPLET_SIZE * 4242 + uentry];
		res = search_triplet_array(key, n_arr, arr, entry);
		if (res == &arr[TRIPLET_SIZE * 4242]) {
			fprintf(test_log, "PASS\n");
		} else {
			/* NB: in principle, the search could have found another entry whose value is
			 *     the same as the entry at 4242, since we don't guarantee that all random
			 *     values are unique...
			 */
			fprintf(test_log, "FAIL (returned address was wrong)\n");
			errcount++;
		}

		/* Testing successful search for smallest element in a list of 10000 triplets on entry N */
		fprintf(test_log, "Testing successful search for smallest element in a list of 10000 triplets on entry %i: ", entry);
		key = arr[TRIPLET_SIZE * 0 + uentry];
		res = search_triplet_array(key, n_arr, arr, entry);
		if (res == &arr[TRIPLET_SIZE * 0]) {
			fprintf(test_log, "PASS\n");
		} else {
			/* NB: in principle, the search could have found another entry whose value is
			 *     the same as the entry at 0, since we don't guarantee that all random
			 *     values are unique...
			 */
			fprintf(test_log, "FAIL (returned address was wrong)\n");
			errcount++;
		}

		/* Testing successful search for largest element in a list of 10000 triplets on entry N */
		fprintf(test_log, "Testing successful search for largest element in a list of 10000 triplets on entry %i: ", entry);
		key = arr[TRIPLET_SIZE * (n_arr - 1) + uentry];
		res = search_triplet_array(key, n_arr, arr, entry);
		if (res == &arr[TRIPLET_SIZE * (n_arr - 1)]) {
			fprintf(test_log, "PASS\n");
		} else {
			/* NB: in principle, the search could have found another entry whose value is
			 *     the same as the entry at (n_arr-1), since we don't guarantee that all random
			 *     values are unique...
			 */
			fprintf(test_log, "FAIL (returned address was wrong)\n");
			errcount++;
		}

		/* Testing un-successful search of a list of 10000 triplets on entry N */
		fprintf(test_log, "Testing un-successful search of a list of 10000 triplets on entry %i: ", entry);
		key = RANDMAX;   /* Array should contain only values 0 through RANDMAX-1 */
		res = search_triplet_array(key, n_arr, arr, entry);
		if (res == NULL) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL (returned address not NULL)\n");
			errcount++;
		}

		free(arr);
	}

	fflush(test_log);
	if (MPI_Barrier(MPI_COMM_WORLD) != MPI_SUCCESS) {
		fprintf(stderr, "Error: MPI_Barrier failed.\n");
		return -1;
	}

	fprintf(test_log, "\n");

	return errcount;
}


/********************************************************************************
 *
 * compare_decomps
 *
 * Compare a SMIOL_decomp against known-correct comp_list and io_list
 *
 * Compares the comp_list and io_list members of a SMIOL_decomp against
 * comp_list and io_list arrays that contain the correct (reference) values.
 *
 * If all values in the comp_list_correct and decomp->comp_list match and
 * if all values in the io_list_correct and decomp->io_list match, a value
 * of 0 is returned; otherwise, a non-zero value is returned.
 *
 * Note: The length of the decomp->comp_list and comp_list_correct arrays must
 *       be at least n_comp_list, and the decomp->io_list and io_list_correct
 *       arrays must be at least n_io_list; otherwise, this routine may fail
 *       unpredictably (perhaps with a segfault).
 *
 ********************************************************************************/
int compare_decomps(struct SMIOL_decomp *decomp,
                    size_t n_comp_list, SMIOL_Offset *comp_list_correct,
                    size_t n_io_list, SMIOL_Offset *io_list_correct)
{
	size_t i;
	SMIOL_Offset *comp_list;
	SMIOL_Offset *io_list;

	comp_list = decomp->comp_list;
	io_list = decomp->io_list;

	/* Check comp_list */
	for (i = 0; i < n_comp_list; i++) {
		if (comp_list[i] != comp_list_correct[i]) {
			return 1;
		}
	}

	/* Check io_list */
	for (i = 0; i < n_io_list; i++) {
		if (io_list[i] != io_list_correct[i]) {
			return 1;
		}
	}

	return 0;
}
