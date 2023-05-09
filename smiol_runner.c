#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdint.h>
#include "smiol.h"
#include "smiol_utils.h"

/*******************************************************************************
 * SMIOL C Runner - Take SMIOL out for a run!
 *******************************************************************************/

int test_init_finalize(FILE *test_log);
int test_open_close(FILE *test_log);
int test_dimensions(FILE *test_log);
int test_variables(FILE *test_log);
int test_attributes(FILE *test_log);
int test_decomp(FILE *test_log);
int test_build_exch(FILE *test_log);
int test_transfer(FILE *test_log);
int test_file_sync(FILE *test_log);
int test_utils(FILE *test_log);
int test_io_decomp(FILE *test_log);
int test_aggregate_list(FILE *test_log);
int test_set_get_frame(FILE* test_log);
int test_put_get_vars(FILE *test_log);
int test_io_aggregation(FILE *test_log);
int test_buffered_io(FILE *test_log);
int compare_decomps(struct SMIOL_decomp *decomp,
                    size_t n_comp_list, SMIOL_Offset *comp_list_correct,
                    size_t n_io_list, SMIOL_Offset *io_list_correct);
size_t elements_covered(int comm_size, size_t *io_start, size_t *io_count);
int range_intersection(size_t start1, size_t count1, size_t start2, size_t count2);


/*******************************************************************************
 *
 * transfer test macro function
 *
 * Tests the transfer_field utility for a given SMIOL_decomp
 *
 * Given a SMIOL_decomp and the number of compute and I/O elements assumed by
 * the SMIOL_decomp, this function:
 * 1) Allocates a compute field and fills it with a pattern of values
 * 2) Transfers the field from compute to I/O tasks
 * 3) Zeros-out the compute field
 * 4) Adds 42 to the field on I/O tasks
 * 5) Transfers the field from I/O tasks back to compute tasks
 * 6) Checks the field on compute tasks.
 *
 * If the final values in the compute field are their original values plus 42,
 * a value of 0 is returned. Otherwise, 1 is returned.
 *
 *******************************************************************************/
#define transfer_type(NAME, TYPE) \
static int NAME(size_t n_compute_elements, size_t n_io_elements, \
                struct SMIOL_decomp *decomp) \
{ \
	size_t i; \
	TYPE *comp_field, *io_field; \
\
	comp_field = malloc(sizeof(TYPE) * n_compute_elements); \
	io_field = malloc(sizeof(TYPE) * n_io_elements); \
\
	/* Initialize compute field */ \
	for (i = 0; i < n_compute_elements; i++) { \
		comp_field[i] = (TYPE)i; \
	} \
\
	/* Transfer field from compute to I/O tasks */ \
	transfer_field(decomp, SMIOL_COMP_TO_IO, sizeof(TYPE), \
	               (const void *)comp_field, (void *)io_field); \
\
	/* Zero-out the compute field */ \
	for (i = 0; i < n_compute_elements; i++) { \
		comp_field[i] = (TYPE)0; \
	} \
\
	/* Add 42 to all elements of the I/O field */ \
	for (i = 0; i < n_io_elements; i++) { \
		io_field[i] = io_field[i] + (TYPE)42; \
	} \
\
	/* Transfer the modified field from I/O tasks to compute tasks */ \
	transfer_field(decomp, SMIOL_IO_TO_COMP, sizeof(TYPE), \
	               (const void *)io_field, (void *)comp_field); \
\
	free(io_field); \
\
	/* Verify values in the compute field */ \
	for (i = 0; i < n_compute_elements; i++) { \
		if (comp_field[i] != (TYPE)i + (TYPE)42) { \
			free(comp_field); \
			return 1; \
		} \
	} \
\
	free(comp_field); \
\
	return 0; \
}

transfer_type(transfer_float, float)
transfer_type(transfer_int, int)
transfer_type(transfer_double, double)
transfer_type(transfer_char, char)

int main(int argc, char **argv)
{
	int ierr;
	int i;
	float f;
	int my_proc_id;
	SMIOL_Offset dimsize;
	size_t n_compute_elements;
	SMIOL_Offset *compute_elements;
	int num_io_tasks, io_stride;
	struct SMIOL_decomp *decomp = NULL;
	struct SMIOL_context *context = NULL;
	struct SMIOL_file *file = NULL;
	char log_fname[17];
	FILE *test_log = NULL;
	char **dimnames;
	float *buf;

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
	 * Unit tests for attributes
	 */
	ierr = test_attributes(test_log);
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
	 * Unit tests for build_exchange
	 */
	ierr = test_build_exch(test_log);
	if (ierr == 0) {
		fprintf(test_log, "All tests PASSED!\n\n");
	}
	else {
		fprintf(test_log, "%i tests FAILED!\n\n", ierr);
	}


	/*
	 * Unit tests transfer_field
	 */
	ierr = test_transfer(test_log);
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

	/*
	 * Unit tests for decomposition of I/O elements
	 */
	ierr = test_io_decomp(test_log);
	if (ierr == 0) {
		fprintf(test_log, "All tests PASSED!\n\n");
	}
	else {
		fprintf(test_log, "%i tests FAILED!\n\n", ierr);
	}

	/*
	 * Unit tests for list aggregation utility function
	 */
	ierr = test_aggregate_list(test_log);
	if (ierr == 0) {
		fprintf(test_log, "All tests PASSED!\n\n");
	}
	else {
		fprintf(test_log, "%i tests FAILED!\n\n", ierr);
	}

	/*
	 * Unit tests for set, get frame
	 */
	ierr = test_set_get_frame(test_log);
	if (ierr == 0) {
		fprintf(test_log, "All tests PASSED!\n\n");
	}
	else {
		fprintf(test_log, "%i tests FAILED!\n\n", ierr);
	}

	/*
	 * Unit tests for writing and reading variables
	 */
	ierr = test_put_get_vars(test_log);
	if (ierr == 0) {
		fprintf(test_log, "All tests PASSED!\n\n");
	}
	else {
		fprintf(test_log, "%i tests FAILED!\n\n", ierr);
	}

	/*
	 * Test I/O aggregation
	 */
	ierr = test_io_aggregation(test_log);
	if (ierr == 0) {
		fprintf(test_log, "All tests PASSED!\n\n");
	}
	else {
		fprintf(test_log, "%i tests FAILED!\n\n", ierr);
	}

	/*
	 * Test buffered I/O
	 */
	ierr = test_buffered_io(test_log);
	if (ierr == 0) {
		fprintf(test_log, "All tests PASSED!\n\n");
	}
	else {
		fprintf(test_log, "%i tests FAILED!\n\n", ierr);
	}


	num_io_tasks = 16;
	io_stride = 4;

	if ((ierr = SMIOL_init(MPI_COMM_WORLD, num_io_tasks, io_stride, &context)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_init: %s ", SMIOL_error_string(ierr));
		return 1;
	} 

	if (context == NULL) {
		fprintf(test_log, "SMIOL_init returned a NULL context\n");
		return 1;
	}

	/* Create elements */
	n_compute_elements = 100;
	compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);

	memset((void *)compute_elements, 0, sizeof(SMIOL_Offset) * n_compute_elements);

	ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, 1,
	                           &decomp);
	if (ierr != SMIOL_SUCCESS) {
		printf("ERROR: SMIOL_create_decomp - SMIOL_SUCCESS was not returned\n");
		return 1;
	}

	/* Free local copy */
	free(compute_elements);

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

	if ((ierr = SMIOL_open_file(context, "blah.nc", SMIOL_FILE_CREATE, &file, (size_t)64000000)) != SMIOL_SUCCESS) {
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

	if ((ierr = SMIOL_inquire_dim(file, "nCells", &dimsize, NULL)) != SMIOL_SUCCESS) {
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

	i = 2;
	if ((ierr = SMIOL_define_att(file, "theta", "time_levels", SMIOL_INT32,
	                             (const void *)&i)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_define_att: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}

	f = (float)3.14159265;
	if ((ierr = SMIOL_define_att(file, NULL, "pi", SMIOL_REAL32,
	                             (const void *)&f)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_define_att: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_define_att(file, NULL, "title", SMIOL_CHAR,
	                             "MPAS-Atmosphere v7.0")) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_define_att: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_inquire_att(file, NULL, "title", NULL, NULL, NULL)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_inquire_att: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}

	buf = malloc(sizeof(float) * (size_t)40962);
	memset((void *)buf, 0, sizeof(float) * (size_t)40962);
	if ((ierr = SMIOL_put_var(file, "theta", NULL, buf)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_put_var: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}

	if ((ierr = SMIOL_get_var(file, "theta", NULL, buf)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_get_var: %s ",
			SMIOL_error_string(ierr));
		return 1;
	}
	free(buf);

	if ((ierr = SMIOL_close_file(&file)) != SMIOL_SUCCESS) {
		fprintf(test_log, "ERROR: SMIOL_close_file: %s ", SMIOL_error_string(ierr));
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
	fprintf(test_log, "SMIOL_error_string test 'argument is of the wrong type': %s\n",
		SMIOL_error_string(SMIOL_WRONG_ARG_TYPE));
	fprintf(test_log, "SMIOL_error_string test 'argument is of insufficient size': %s\n",
		SMIOL_error_string(SMIOL_INSUFFICIENT_ARG));

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
	int num_io_tasks;
	int io_stride;

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************ SMIOL_init / SMIOL_finalize unit tests ****************************\n");
	fprintf(test_log, "\n");

	errcount = 0;

	MPI_Comm_size(MPI_COMM_WORLD, &num_io_tasks);
	io_stride = 1;

	/* Null context pointer */
	fprintf(test_log, "Null pointer to context pointer (SMIOL_init): ");
	ierr = SMIOL_init(MPI_COMM_WORLD, num_io_tasks, io_stride, NULL);
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
	ierr = SMIOL_init(MPI_COMM_NULL, num_io_tasks, io_stride, &context);
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
	ierr = SMIOL_init(MPI_COMM_WORLD, num_io_tasks, io_stride, &context);
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
	int num_io_tasks;
	int io_stride;

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************ SMIOL_open_file / SMIOL_close_file unit tests *********************\n");
	fprintf(test_log, "\n");

	errcount = 0;

	MPI_Comm_size(MPI_COMM_WORLD, &num_io_tasks);
	io_stride = 1;

	/* Create a SMIOL context for testing file open/close routines */
	ierr = SMIOL_init(MPI_COMM_WORLD, num_io_tasks, io_stride, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	/* Try to open a file with an invalid mode */
	fprintf(test_log, "Try to open a file with an invalid mode: ");
	file = NULL;
	ierr = SMIOL_open_file(context, "smiol_invalid.nc", ~(SMIOL_FILE_CREATE | SMIOL_FILE_WRITE | SMIOL_FILE_READ), &file, (size_t)64000000);
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
	ierr = SMIOL_open_file(context, "/smiol_test.nc", SMIOL_FILE_CREATE, &file, (size_t)64000000);
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
	ierr = SMIOL_open_file(context, "/smiol_foobar.nc", SMIOL_FILE_READ, &file, (size_t)0);
	if (ierr == SMIOL_LIBRARY_ERROR && file == NULL) {
		fprintf(test_log, "PASS (%s)\n", SMIOL_lib_error_string(context));
	}
	else {
		fprintf(test_log, "FAIL - expected error code of SMIOL_LIBRARY_ERROR not returned, or file not NULL\n");
		errcount++;
	}

#if 0
/*
 * Since members of the SMIOL_file struct may be used in various ways within
 * SMIOL_close_file, and without being able to verify that the contents of the
 * SMIOL_file are valid, this test may lead to segfaults or other crashes.
 * Until we have a way of verifying the validity of a SMIOL_file before using
 * its contents, this test should probably just be omitted.
 */
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
#endif

	/* Create a file to be closed and opened again */
	fprintf(test_log, "Create a file to be closed and later re-opened: ");
	file = NULL;
	ierr = SMIOL_open_file(context, "pnetcdf_test_c.nc", SMIOL_FILE_CREATE, &file, (size_t)64000000);
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
	ierr = SMIOL_open_file(context, "pnetcdf_test_c.nc", SMIOL_FILE_READ, &file, (size_t)0);
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
	ierr = SMIOL_open_file(context, "pnetcdf_test_c.nc", SMIOL_FILE_WRITE, &file, (size_t)64000000);
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
	ierr = SMIOL_open_file(context, "test.nc", SMIOL_FILE_CREATE, &file, (size_t)64000000);
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
	size_t n_compute_elements;
	SMIOL_Offset *compute_elements = NULL;
	struct SMIOL_context *context = NULL;
	struct SMIOL_decomp *decomp = NULL;
	int num_io_tasks;
	int io_stride;

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

	num_io_tasks = comm_size;
	io_stride = 1;

	/* Create a SMIOL context for testing decomp routines */
	ierr = SMIOL_init(MPI_COMM_WORLD, num_io_tasks, io_stride, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	/* Test create decomp with compute_elements == NULL and n_compute != 0 */
	fprintf(test_log, "Testing SMIOL_create_decomp with NULL elements and n_elements != 0: ");
	n_compute_elements = 1;
	ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, 1, &decomp);
	if (ierr == SMIOL_INVALID_ARGUMENT && decomp == NULL) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - Either SMIOL_INVALID_ARGUMENT was not returned or decomp was not NULL\n");
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
	compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
	ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, 1, &decomp);
	if (ierr == SMIOL_SUCCESS && decomp != NULL) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or decomp was NULL\n");
		errcount++;
	}
	free(compute_elements);

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

	/* Check for error with negative aggregation factor */
	fprintf(test_log, "Check for error with negative aggregation factor: ");
	n_compute_elements = 0;
	compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
	ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, -1, &decomp);
	if (ierr == SMIOL_INVALID_ARGUMENT && decomp == NULL) {
		fprintf(test_log, "PASS\n");
	} else if (ierr != SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "FAIL - SMIOL_INVALID_ARGUMENT was not returned\n");
		errcount++;
	} else if (decomp != NULL) {
		fprintf(test_log, "FAIL - decomp was not NULL on return\n");
		errcount++;
	}
	free(compute_elements);
	if (decomp != NULL) {
		/*
		 * Since we do not expect decomp to be non-NULL, we have no way
		 * to know what state decomp is in, so just leak memory and nullify
		 * rather than calling SMIOL_free_decomp with unknown memory.
		 */
		decomp = NULL;
	}

	/* Create and Free Decomp with elements == 1 */
	fprintf(test_log, "Everything OK (SMIOL_create_decomp) with 1 elements: ");
	n_compute_elements = 1;
	compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
	memset((void *)compute_elements, 0, sizeof(SMIOL_Offset) * n_compute_elements);
	ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, 1, &decomp);
	if (ierr == SMIOL_SUCCESS && decomp != NULL) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or decomp was NULL\n");
		errcount++;
	}
	free(compute_elements);

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
	compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
	memset((void *)compute_elements, 0, sizeof(SMIOL_Offset) * n_compute_elements);
	ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, 1, &decomp);
	if (ierr == SMIOL_SUCCESS && decomp != NULL) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or decomp was NULL\n");
		errcount++;
	}
	free(compute_elements);

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

	/* Free the SMIOL context */
	ierr = SMIOL_finalize(&context);
	if (ierr != SMIOL_SUCCESS || context != NULL) {
		fprintf(test_log, "Failed to free SMIOL context...\n");
		return -1;
	}

	/*
	 * The following tests will only be run if there are exactly two MPI tasks.
	 * In principle, as long as there are at least two MPI ranks in MPI_COMM_WORLD,
	 * an intracommunicator with exactly two ranks could be created for these tests.
	 */
	if (comm_size == 2) {

		/* Create a SMIOL context for testing decomp routines */
		ierr = SMIOL_init(MPI_COMM_WORLD, 2, 1, &context);
		if (ierr != SMIOL_SUCCESS || context == NULL) {
			fprintf(test_log, "Failed to create SMIOL context...\n");
			return -1;
		}

		/* Odd/even compute, half/half I/O */
		fprintf(test_log, "Odd/even compute, half/half I/O: ");
		n_compute_elements = 4;
		compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);

		if (comm_rank == 0) {
			for (i = 0; i < n_compute_elements; i++) {
				compute_elements[i] = (SMIOL_Offset)(2 * i + 1);    /* Odd elements */
			}
		} else {
			for (i = 0; i < n_compute_elements; i++) {
				compute_elements[i] = (SMIOL_Offset)(2 * i);        /* Even elements */
			}
		}
		ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, 1, &decomp);
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

		ierr = SMIOL_free_decomp(&decomp);
		if (ierr != SMIOL_SUCCESS || decomp != NULL) {
			fprintf(test_log, "After previous unit test, SMIOL_free_decomp was unsuccessful: FAIL\n");
			errcount++;
		}

		/* Free the SMIOL context */
		ierr = SMIOL_finalize(&context);
		if (ierr != SMIOL_SUCCESS || context != NULL) {
			fprintf(test_log, "Failed to free SMIOL context...\n");
			return -1;
		}


		/* Create a SMIOL context for testing decomp routines */
		ierr = SMIOL_init(MPI_COMM_WORLD, 1, 2, &context);
		if (ierr != SMIOL_SUCCESS || context == NULL) {
			fprintf(test_log, "Failed to create SMIOL context...\n");
			return -1;
		}

		/* Even/odd compute, all/nothing I/O */
		fprintf(test_log, "Even/odd compute, all/nothing I/O: ");
		n_compute_elements = 4;
		compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);

		if (comm_rank == 0) {
			for (i = 0; i < n_compute_elements; i++) {
				compute_elements[i] = (SMIOL_Offset)(2 * i);        /* Even elements */
			}
		} else {
			for (i = 0; i < n_compute_elements; i++) {
				compute_elements[i] = (SMIOL_Offset)(2 * i + 1);    /* Odd elements */
			}
		}
		ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, 1, &decomp);
		if (ierr == SMIOL_SUCCESS && decomp != NULL) {

			/* The correct comp_list and io_list arrays, below, were verified manually */
			if (comm_rank == 0) {
				SMIOL_Offset comp_list_correct[] = { 1, 0, 4, 0, 1, 2, 3 };
				SMIOL_Offset io_list_correct[] = { 2, 0, 4, 0, 2, 4, 6, 1, 4, 1, 3, 5, 7 };

				ierr = compare_decomps(decomp, (size_t)7, comp_list_correct, (size_t)13, io_list_correct);
			} else {
				SMIOL_Offset comp_list_correct[] = { 1, 0, 4, 0, 1, 2, 3 };
				SMIOL_Offset io_list_correct[] = { 0 };

				ierr = compare_decomps(decomp, (size_t)7, comp_list_correct, (size_t)1, io_list_correct);
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

		ierr = SMIOL_free_decomp(&decomp);
		if (ierr != SMIOL_SUCCESS || decomp != NULL) {
			fprintf(test_log, "After previous unit test, SMIOL_free_decomp was unsuccessful: FAIL\n");
			errcount++;
		}

		/* Free the SMIOL context */
		ierr = SMIOL_finalize(&context);
		if (ierr != SMIOL_SUCCESS || context != NULL) {
			fprintf(test_log, "Failed to free SMIOL context...\n");
			return -1;
		}


		/* Create a SMIOL context for testing decomp routines */
		ierr = SMIOL_init(MPI_COMM_WORLD, 1, 2, &context);
		if (ierr != SMIOL_SUCCESS || context == NULL) {
			fprintf(test_log, "Failed to create SMIOL context...\n");
			return -1;
		}

		/* Nothing/all compute, all/nothing I/O */
		fprintf(test_log, "Nothing/all compute, all/nothing I/O: ");
		if (comm_rank == 0) {
			n_compute_elements = 0;
		} else {
			n_compute_elements = 8;
		}
		compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);

		if (comm_rank == 0) {
			/* No compute elements */
		} else {
			for (i = 0; i < n_compute_elements; i++) {
				compute_elements[i] = (SMIOL_Offset)(n_compute_elements - 1 - i);    /* All compute elements */
			}
		}
		ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, 1, &decomp);
		if (ierr == SMIOL_SUCCESS && decomp != NULL) {

			/* The correct comp_list and io_list arrays, below, were verified manually */
			if (comm_rank == 0) {
				SMIOL_Offset comp_list_correct[] = { 0 };
				SMIOL_Offset io_list_correct[] = { 1, 1, 8, 0, 1, 2, 3, 4, 5, 6, 7 };

				ierr = compare_decomps(decomp, (size_t)1, comp_list_correct, (size_t)11, io_list_correct);
			} else {
				SMIOL_Offset comp_list_correct[] = { 1, 0, 8, 7, 6, 5, 4, 3, 2, 1, 0 };
				SMIOL_Offset io_list_correct[] = { 0 };

				ierr = compare_decomps(decomp, (size_t)11, comp_list_correct, (size_t)1, io_list_correct);
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

		ierr = SMIOL_free_decomp(&decomp);
		if (ierr != SMIOL_SUCCESS || decomp != NULL) {
			fprintf(test_log, "After previous unit test, SMIOL_free_decomp was unsuccessful: FAIL\n");
			errcount++;
		}

		/* Free the SMIOL context */
		ierr = SMIOL_finalize(&context);
		if (ierr != SMIOL_SUCCESS || context != NULL) {
			fprintf(test_log, "Failed to free SMIOL context...\n");
			return -1;
		}

	} else {
		fprintf(test_log, "<<< Tests that require exactly 2 MPI tasks will not be run >>>\n");
	}

	/*
	 * The following tests will only be run if there are exactly four MPI tasks.
	 * In principle, as long as there are at least four MPI ranks in MPI_COMM_WORLD,
	 * an intracommunicator with exactly four ranks could be created for these tests.
	 */
	if (comm_size == 4) {
		/* Create a SMIOL context for testing decomp routines */
		ierr = SMIOL_init(MPI_COMM_WORLD, 4, 1, &context);
		if (ierr != SMIOL_SUCCESS || context == NULL) {
			fprintf(test_log, "Failed to create SMIOL context...\n");
			return -1;
		}

		/* Round-robin compute, stride 1, aggregate 2 */
		fprintf(test_log, "Round-robin compute, stride 1, aggregate 2: ");
		n_compute_elements = 4;
		compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);

		for (i = 0; i < n_compute_elements; i++) {
			compute_elements[i] = (SMIOL_Offset)(4 * i + comm_rank);
		}

		ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, 2, &decomp);
		if (ierr == SMIOL_SUCCESS && decomp != NULL) {

			/* The correct comp_list and io_list arrays, below, were verified manually */
			if (comm_rank == 0) {
				SMIOL_Offset comp_list_correct[] = { 4, 0, 2, 0, 4, 1, 2, 1, 5, 2, 2, 2, 6, 3, 2, 3, 7 };
				SMIOL_Offset io_list_correct[] = { 2, 0, 2, 0, 1, 2, 2, 2, 3 };
				ierr = compare_decomps(decomp, (sizeof(comp_list_correct) / sizeof(SMIOL_Offset)), comp_list_correct,
				                               (sizeof(io_list_correct) / sizeof(SMIOL_Offset)), io_list_correct);
			} else if (comm_rank == 1) {
				SMIOL_Offset comp_list_correct[] = { 0 };
				SMIOL_Offset io_list_correct[] = { 2, 0, 2, 0, 1, 2, 2, 2, 3 };
				ierr = compare_decomps(decomp, (sizeof(comp_list_correct) / sizeof(SMIOL_Offset)), comp_list_correct,
				                               (sizeof(io_list_correct) / sizeof(SMIOL_Offset)), io_list_correct);
			} else if (comm_rank == 2) {
				SMIOL_Offset comp_list_correct[] = { 4, 0, 2, 0, 4, 1, 2, 1, 5, 2, 2, 2, 6, 3, 2, 3, 7 };
				SMIOL_Offset io_list_correct[] = { 2, 0, 2, 0, 1, 2, 2, 2, 3 };
				ierr = compare_decomps(decomp, (sizeof(comp_list_correct) / sizeof(SMIOL_Offset)), comp_list_correct,
				                               (sizeof(io_list_correct) / sizeof(SMIOL_Offset)), io_list_correct);
			} else if (comm_rank == 3) {
				SMIOL_Offset comp_list_correct[] = { 0 };
				SMIOL_Offset io_list_correct[] = { 2, 0, 2, 0, 1, 2, 2, 2, 3 };
				ierr = compare_decomps(decomp, (sizeof(comp_list_correct) / sizeof(SMIOL_Offset)), comp_list_correct,
				                               (sizeof(io_list_correct) / sizeof(SMIOL_Offset)), io_list_correct);
			} else {
				ierr = 1;
				fprintf(stderr, "We seem to have too may ranks in the aggregation decomp unit tests...\n");
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

		ierr = SMIOL_free_decomp(&decomp);
		if (ierr != SMIOL_SUCCESS || decomp != NULL) {
			fprintf(test_log, "After previous unit test, SMIOL_free_decomp was unsuccessful: FAIL\n");
			errcount++;
		}

		/* Free the SMIOL context */
		ierr = SMIOL_finalize(&context);
		if (ierr != SMIOL_SUCCESS || context != NULL) {
			fprintf(test_log, "Failed to free SMIOL context...\n");
			return -1;
		}

		/* Create a SMIOL context for testing decomp routines */
		ierr = SMIOL_init(MPI_COMM_WORLD, 2, 2, &context);
		if (ierr != SMIOL_SUCCESS || context == NULL) {
			fprintf(test_log, "Failed to create SMIOL context...\n");
			return -1;
		}

		/* Round-robin compute, stride 2, aggregate 3 */
		fprintf(test_log, "Round-robin compute, stride 2, aggregate 3: ");
		n_compute_elements = 4;
		compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);

		for (i = 0; i < n_compute_elements; i++) {
			compute_elements[i] = (SMIOL_Offset)(4 * i + comm_rank);
		}

		ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, 3, &decomp);
		if (ierr == SMIOL_SUCCESS && decomp != NULL) {

			/* The correct comp_list and io_list arrays, below, were verified manually */
			if (comm_rank == 0) {
				SMIOL_Offset comp_list_correct[] = { 2, 0, 6, 0, 4, 8, 1, 5, 9, 2, 6, 2, 6, 10, 3, 7, 11 };
				SMIOL_Offset io_list_correct[] = { 2, 0, 6, 0, 1, 2, 4, 5, 6, 3, 2, 3, 7 };
				ierr = compare_decomps(decomp, (sizeof(comp_list_correct) / sizeof(SMIOL_Offset)), comp_list_correct,
				                               (sizeof(io_list_correct) / sizeof(SMIOL_Offset)), io_list_correct);
			} else if (comm_rank == 1) {
				SMIOL_Offset comp_list_correct[] = { 0 };
				SMIOL_Offset io_list_correct[] = { 0 };
				ierr = compare_decomps(decomp, (sizeof(comp_list_correct) / sizeof(SMIOL_Offset)), comp_list_correct,
				                               (sizeof(io_list_correct) / sizeof(SMIOL_Offset)), io_list_correct);
			} else if (comm_rank == 2) {
				SMIOL_Offset comp_list_correct[] = { 0 };
				SMIOL_Offset io_list_correct[] = { 2, 0, 6, 0, 1, 2, 4, 5, 6, 3, 2, 3, 7 };
				ierr = compare_decomps(decomp, (sizeof(comp_list_correct) / sizeof(SMIOL_Offset)), comp_list_correct,
				                               (sizeof(io_list_correct) / sizeof(SMIOL_Offset)), io_list_correct);
			} else if (comm_rank == 3) {
				SMIOL_Offset comp_list_correct[] = { 2, 0, 2, 0, 1, 2, 2, 2, 3 };
				SMIOL_Offset io_list_correct[] = { 0 };
				ierr = compare_decomps(decomp, (sizeof(comp_list_correct) / sizeof(SMIOL_Offset)), comp_list_correct,
				                               (sizeof(io_list_correct) / sizeof(SMIOL_Offset)), io_list_correct);
			} else {
				ierr = 1;
				fprintf(stderr, "We seem to have too may ranks in the aggregation decomp unit tests...\n");
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

		ierr = SMIOL_free_decomp(&decomp);
		if (ierr != SMIOL_SUCCESS || decomp != NULL) {
			fprintf(test_log, "After previous unit test, SMIOL_free_decomp was unsuccessful: FAIL\n");
			errcount++;
		}

		/* Round-robin compute, stride 2, aggregate 0 */
		fprintf(test_log, "Round-robin compute, stride 2, aggregate 0: ");
		ierr = SMIOL_create_decomp(context, n_compute_elements, compute_elements, 0, &decomp);
		if (ierr == SMIOL_SUCCESS && decomp != NULL) {
			/*
			 * Since the aggregation factor that will ultimately be chosen cannot in general
			 * be predicted, we cannot really verify the contents of the decomp.
			 */
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or decomp was NULL\n");
			errcount++;
		}

		ierr = SMIOL_free_decomp(&decomp);
		if (ierr != SMIOL_SUCCESS || decomp != NULL) {
			fprintf(test_log, "After previous unit test, SMIOL_free_decomp was unsuccessful: FAIL\n");
			errcount++;
		}

		free(compute_elements);

		/* Free the SMIOL context */
		ierr = SMIOL_finalize(&context);
		if (ierr != SMIOL_SUCCESS || context != NULL) {
			fprintf(test_log, "Failed to free SMIOL context...\n");
			return -1;
		}
	} else {
		fprintf(test_log, "<<< Tests that require exactly 4 MPI tasks will not be run >>>\n");
	}

	fflush(test_log);
	ierr = MPI_Barrier(MPI_COMM_WORLD);

	fprintf(test_log, "\n");

	return errcount;
}

int test_build_exch(FILE *test_log)
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
	int num_io_tasks;
	int io_stride;

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************************* build_exchange unit tests ****************************\n");
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

	num_io_tasks = comm_size;
	io_stride = 1;

	/* Create a SMIOL context for testing decomp routines */
	ierr = SMIOL_init(MPI_COMM_WORLD, num_io_tasks, io_stride, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	/* Test create decomp with io_elements and compute_elements == NULL and
	 * n_io_elements and n_compute != 0 */
	fprintf(test_log, "Testing build_exchange with NULL elements and n_elements != 0: ");
	n_compute_elements = 1;
	n_io_elements = 1;
	ierr = build_exchange(context, n_compute_elements, compute_elements, n_io_elements, io_elements, &decomp);
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
	fprintf(test_log, "Everything OK (build_exchange) with 0 elements: ");
	n_compute_elements = 0;
	n_io_elements = 0;
	compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
	io_elements = malloc(sizeof(SMIOL_Offset) * n_io_elements);
	ierr = build_exchange(context, n_compute_elements, compute_elements, n_io_elements, io_elements, &decomp);
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
	fprintf(test_log, "Everything OK (build_exchange) with 1 elements: ");
	n_compute_elements = 1;
	n_io_elements = 1;
	compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
	io_elements = malloc(sizeof(SMIOL_Offset) * n_io_elements);
	memset((void *)compute_elements, 0, sizeof(SMIOL_Offset) * n_compute_elements);
	memset((void *)io_elements, 0, sizeof(SMIOL_Offset) * n_io_elements);
	ierr = build_exchange(context, n_compute_elements, compute_elements, n_io_elements, io_elements, &decomp);
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
	fprintf(test_log, "Everything OK (build_exchange) with large amount of elements: ");
	n_compute_elements = 1000000;
	n_io_elements = 1000000;
	compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
	io_elements = malloc(sizeof(SMIOL_Offset) * n_io_elements);
	memset((void *)compute_elements, 0, sizeof(SMIOL_Offset) * n_compute_elements);
	memset((void *)io_elements, 0, sizeof(SMIOL_Offset) * n_io_elements);
	ierr = build_exchange(context, n_compute_elements, compute_elements, n_io_elements, io_elements, &decomp);
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
		ierr = build_exchange(context, n_compute_elements, compute_elements, n_io_elements, io_elements, &decomp);
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
		ierr = build_exchange(context, n_compute_elements, compute_elements, n_io_elements, io_elements, &decomp);
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
		ierr = build_exchange(context, n_compute_elements, compute_elements, n_io_elements, io_elements, &decomp);
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

int test_transfer(FILE *test_log)
{
	int ierr;
	int errcount = 0;
	int comm_rank;
	int comm_size;
	int j;
	size_t i;
	size_t n_compute_elements, n_io_elements;
	SMIOL_Offset *compute_elements = NULL, *io_elements = NULL;
	struct SMIOL_context *context = NULL;
	struct SMIOL_decomp *decomp = NULL;
	int num_io_tasks;
	int io_stride;

	/* Pointers to transfer tests for various element types */
	int (*testfun[4])(size_t,size_t,struct SMIOL_decomp *) = {
	    transfer_char,
	    transfer_int,
	    transfer_float,
	    transfer_double };

	/* Names of the element types tested by transfer test function */
	const char *testname[4] = {
	    "char",
	    "int",
	    "float",
	    "double" };

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************************* transfer_field unit tests ****************************\n");
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

	num_io_tasks = comm_size;
	io_stride = 1;

	/* Create a SMIOL context for testing decomp routines */
	ierr = SMIOL_init(MPI_COMM_WORLD, num_io_tasks, io_stride, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	/*
	 * The following tests will only be run if there are exactly two MPI
	 * tasks. In principle, as long as there are at least two MPI ranks in
	 * MPI_COMM_WORLD, an intracommunicator with exactly two ranks could be
	 * created for these tests.
	 */
	if (comm_size != 2) {
		fprintf(test_log, "<<< Tests that require exactly 2 MPI tasks will not be run >>>\n");

		/* Free the SMIOL context */
		ierr = SMIOL_finalize(&context);
		if (ierr != SMIOL_SUCCESS || context != NULL) {
			fprintf(test_log, "Failed to free SMIOL context...\n");
			return -1;
		}

		return 0;
	}

	/* Iterate over different test element types and associated functions */
	for (j = 0; j < 4; j++) {

		/* Even/odd compute, half/half I/O */
		fprintf(test_log, "Even/odd compute, half/half I/O (%s): ", testname[j]);
		n_compute_elements = 4;
		n_io_elements = 4;
		compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
		io_elements = malloc(sizeof(SMIOL_Offset) * n_io_elements);

		if (comm_rank == 0) {
			/* Odd elements */
			for (i = 0; i < n_compute_elements; i++) {
				compute_elements[i] = (SMIOL_Offset)(2 * i + 1);
			}

			/* First half of elements */
			for (i = 0; i < n_io_elements; i++) {
				io_elements[i] = (SMIOL_Offset)i;
			}
		} else {
			/* Even elements */
			for (i = 0; i < n_compute_elements; i++) {
				compute_elements[i] = (SMIOL_Offset)(2 * i);
			}

			/* Second half of elements */
			for (i = 0; i < n_io_elements; i++) {
				io_elements[i] = (SMIOL_Offset)(4 + i);
			}
		}
		ierr = build_exchange(context,
		                      n_compute_elements, compute_elements,
		                      n_io_elements, io_elements, &decomp);
		if (ierr != SMIOL_SUCCESS || decomp == NULL) {
			fprintf(test_log, "Failed to build exchange to test transfer_field...\n");
			return -1;
		}

		free(compute_elements);
		free(io_elements);

		if (testfun[j](n_compute_elements, n_io_elements, decomp) == 0) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL\n");
			errcount++;
		}

		ierr = SMIOL_free_decomp(&decomp);
		if (ierr != SMIOL_SUCCESS || decomp != NULL) {
			fprintf(test_log, "After previous unit test, SMIOL_free_decomp was unsuccessful...\n");
			return -1;
		}


		/* Even/odd compute, nothing/all I/O */
		fprintf(test_log, "Even/odd compute, nothing/all I/O (%s): ", testname[j]);
		n_compute_elements = 4;
		if (comm_rank == 0) {
			n_io_elements = 0;
		} else {
			n_io_elements = 8;
		}
		compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
		io_elements = malloc(sizeof(SMIOL_Offset) * n_io_elements);

		if (comm_rank == 0) {
			/* Odd elements */
			for (i = 0; i < n_compute_elements; i++) {
				compute_elements[i] = (SMIOL_Offset)(2 * i + 1);
			}

			/* No I/O elements */
		} else {
			/* Even elements */
			for (i = 0; i < n_compute_elements; i++) {
				compute_elements[i] = (SMIOL_Offset)(2 * i);
			}

			/* All I/O elements */
			for (i = 0; i < n_io_elements; i++) {
				io_elements[i] = (SMIOL_Offset)i;
			}
		}
		ierr = build_exchange(context,
		                      n_compute_elements, compute_elements,
		                      n_io_elements, io_elements, &decomp);
		if (ierr != SMIOL_SUCCESS || decomp == NULL) {
			fprintf(test_log, "Failed to build exchange to test transfer_field...\n");
			return -1;
		}

		free(compute_elements);
		free(io_elements);

		if (testfun[j](n_compute_elements, n_io_elements, decomp) == 0) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL\n");
			errcount++;
		}

		ierr = SMIOL_free_decomp(&decomp);
		if (ierr != SMIOL_SUCCESS || decomp != NULL) {
			fprintf(test_log, "After previous unit test, SMIOL_free_decomp was unsuccessful: FAIL\n");
			errcount++;
		}


		/* All/nothing compute, nothing/all I/O */
		fprintf(test_log, "All/nothing compute, nothing/all I/O (%s): ", testname[j]);
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
			/* All compute elements */
			for (i = 0; i < n_compute_elements; i++) {
				compute_elements[i] = (SMIOL_Offset)(n_compute_elements - 1 - i);
			}

			/* No I/O elements */
		} else {
			/* No compute elements */

			/* All I/O elements */
			for (i = 0; i < n_io_elements; i++) {
				io_elements[i] = (SMIOL_Offset)i;
			}
		}
		ierr = build_exchange(context,
		                      n_compute_elements, compute_elements,
		                      n_io_elements, io_elements, &decomp);
		if (ierr != SMIOL_SUCCESS || decomp == NULL) {
			fprintf(test_log, "Failed to build exchange to test transfer_field...\n");
			return -1;
		}

		free(compute_elements);
		free(io_elements);

		if (testfun[j](n_compute_elements, n_io_elements, decomp) == 0) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL\n");
			errcount++;
		}

		ierr = SMIOL_free_decomp(&decomp);
		if (ierr != SMIOL_SUCCESS || decomp != NULL) {
			fprintf(test_log, "After previous unit test, SMIOL_free_decomp was unsuccessful: FAIL\n");
			errcount++;
		}

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
	int is_unlimited;
	SMIOL_Offset dimsize;
	struct SMIOL_context *context;
	struct SMIOL_file *file;
	SMIOL_Offset expected_dimsize;
	int num_io_tasks;
	int io_stride;

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************ SMIOL_define_dim / SMIOL_inquire_dim ******************************\n");
	fprintf(test_log, "\n");

	errcount = 0;

	MPI_Comm_size(MPI_COMM_WORLD, &num_io_tasks);
	io_stride = 1;

	/* Create a SMIOL context for testing dimension routines */
	context = NULL;
	ierr = SMIOL_init(MPI_COMM_WORLD, num_io_tasks, io_stride, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	/* Create a SMIOL file for testing dimension routines */
	file = NULL;
	ierr = SMIOL_open_file(context, "test_dims.nc", SMIOL_FILE_CREATE, &file, (size_t)64000000);
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
	ierr = SMIOL_inquire_dim(NULL, "invalid_dim", NULL, NULL);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, when an error was expected\n");
		errcount++;
	}

	/* Handle NULL dimension name */
	fprintf(test_log, "Handle NULL dimension name (SMIOL_inquire_dim): ");
	ierr = SMIOL_inquire_dim(file, NULL, &dimsize, NULL);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, when an error was expected\n");
		errcount++;
	}

	/* Handle NULL dimension size */
	fprintf(test_log, "Handle NULL dimension size and NULL unlimited argument (SMIOL_inquire_dim): ");
	ierr = SMIOL_inquire_dim(file, "nCells", NULL, NULL);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	}
	else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, when an error was expected\n");
		errcount++;
	}

	/* Handle undefined dimension */
	fprintf(test_log, "Handle undefined dimension (SMIOL_inquire_dim): ");
	ierr = SMIOL_inquire_dim(file, "foobar", &dimsize, NULL);
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
	ierr = SMIOL_inquire_dim(file, "Time", &dimsize, NULL);
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
	ierr = SMIOL_inquire_dim(file, "nCells", &dimsize, NULL);
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
	ierr = SMIOL_inquire_dim(file, "nElements", &dimsize, NULL);
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

	/* Inquire about the unlimited dimension */
	fprintf(test_log, "Everything OK - checking if Time is the unlimited dimension (SMIOL_inquire_dim): ");
	ierr = SMIOL_inquire_dim(file, "Time", NULL, &is_unlimited);
	if (ierr == SMIOL_SUCCESS) {
#ifdef SMIOL_PNETCDF
		if (is_unlimited == 1) {
#else
		if (is_unlimited == 0) {
#endif
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "is_unlimited: %d\n", is_unlimited);
			fprintf(test_log, "FAIL - SMIOL_inquire_dim reported that the Time dim was not the unlimited dim\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	fprintf(test_log, "Everything OK - checking if nCells is not the unlimited dimension (SMIOL_inquire_dim): ");
	ierr = SMIOL_inquire_dim(file, "nCells", NULL, &is_unlimited);
	if (ierr == SMIOL_SUCCESS) {
		if (!is_unlimited) {
			fprintf(test_log, "PASS\n");
		}
		else {
			fprintf(test_log, "FAIL - SMIOL_inquire_dim reported that the nCells dim *was* the unlimited dim\n");
			errcount++;
		}
	} else {
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
	ierr = SMIOL_open_file(context, "test_dims.nc", SMIOL_FILE_READ, &file, (size_t)0);
	if (ierr != SMIOL_SUCCESS || file == NULL) {
		fprintf(test_log, "Failed to open existing SMIOL file...\n");
		return -1;
	}

	/* Existing file for SMIOL_inquire_dim, unlimited dimension */
	fprintf(test_log, "Existing file - unlimited dimension (SMIOL_inquire_dim): ");
	dimsize = (SMIOL_Offset)0;
	ierr = SMIOL_inquire_dim(file, "Time", &dimsize, NULL);
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
	ierr = SMIOL_inquire_dim(file, "nCells", &dimsize, NULL);
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
	ierr = SMIOL_inquire_dim(file, "nElements", &dimsize, NULL);
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
	int num_io_tasks;
	int io_stride;

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************ SMIOL_define_var / SMIOL_inquire_var ******************************\n");
	fprintf(test_log, "\n");

	errcount = 0;

	MPI_Comm_size(MPI_COMM_WORLD, &num_io_tasks);
	io_stride = 1;

	/* If more than one MPI rank, use an io_stride of 2 */
	if (num_io_tasks > 1) {
		io_stride = 2;
		num_io_tasks = num_io_tasks / io_stride;
	}

	/* Create a SMIOL context for testing variable routines */
	context = NULL;
	ierr = SMIOL_init(MPI_COMM_WORLD, num_io_tasks, io_stride, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	/* Create a SMIOL file for testing variable routines */
	file = NULL;
	ierr = SMIOL_open_file(context, "test_vars.nc", SMIOL_FILE_CREATE, &file, (size_t)64000000);
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
	ierr = SMIOL_open_file(context, "test_vars.nc", SMIOL_FILE_READ, &file, (size_t)0);
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

int test_attributes(FILE *test_log)
{
	int errcount;
	int ierr;
	int i;
	char **dimnames;
	struct SMIOL_context *context;
	struct SMIOL_file *file;
	float real32_att;
	double real64_att;
	int int32_att;
	char text_att[32];
	int num_io_tasks;
	int io_stride;


	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************ SMIOL_define_att / SMIOL_inquire_att ******************************\n");
	fprintf(test_log, "\n");

	errcount = 0;

	MPI_Comm_size(MPI_COMM_WORLD, &num_io_tasks);
	io_stride = 1;

	/* Create a SMIOL context for testing attribute routines */
	context = NULL;
	ierr = SMIOL_init(MPI_COMM_WORLD, num_io_tasks, io_stride, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	/* Create a SMIOL file for testing attribute routines */
	file = NULL;
	ierr = SMIOL_open_file(context, "test_atts.nc", SMIOL_FILE_CREATE, &file, (size_t)64000000);
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

	dimnames = (char **)malloc(sizeof(char *) * (size_t)2);
	for (i = 0; i < 2; i++) {
		dimnames[i] = (char *)malloc(sizeof(char) * (size_t)32);
	}

	/* Define a 32-bit real variable with one non-record dimension and a record dimension */
	snprintf(dimnames[0], 32, "Time");
	snprintf(dimnames[1], 32, "nCells");
	ierr = SMIOL_define_var(file, "surface_pressure", SMIOL_REAL32, 2, (const char **)dimnames);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create variable surface_pressure...\n");
		return -1;
	}

	/* Everything OK - Define a global REAL32 attribute */
	fprintf(test_log, "Everything OK - Define a global REAL32 attribute: ");
	real32_att = 3.14159;
	ierr = SMIOL_define_att(file, NULL, "pi", SMIOL_REAL32,
	                        (const void *)&real32_att);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK - Define a global REAL64 attribute */
	fprintf(test_log, "Everything OK - Define a global REAL64 attribute: ");
	real64_att = 2.718281828;
	ierr = SMIOL_define_att(file, NULL, "e", SMIOL_REAL64,
	                        (const void *)&real64_att);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK - Define another global REAL64 attribute */
	fprintf(test_log, "Everything OK - Define another global REAL64 attribute: ");
	real64_att = 1.0;
	ierr = SMIOL_define_att(file, NULL, "unity", SMIOL_REAL64,
	                        (const void *)&real64_att);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK - Define a global INT32 attribute */
	fprintf(test_log, "Everything OK - Define a global INT32 attribute: ");
	int32_att = 42;
	ierr = SMIOL_define_att(file, NULL, "grid_id", SMIOL_INT32,
	                        (const void *)&int32_att);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK - Define a global CHAR attribute */
	fprintf(test_log, "Everything OK - Define a global CHAR attribute: ");
	snprintf(text_att, 32, "Don't panic!");
	ierr = SMIOL_define_att(file, NULL, "Advice", SMIOL_CHAR,
	                        (const void *)text_att);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK - Define _FillValue variable attribute */
	fprintf(test_log, "Everything OK - Define _FillValue variable attribute: ");
	real32_att = 0.0;
	ierr = SMIOL_define_att(file, "surface_pressure", "_FillValue", SMIOL_REAL32,
	                        (const void *)&real32_att);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK - Define a variable REAL32 attribute */
	fprintf(test_log, "Everything OK - Define a variable REAL32 attribute: ");
	real32_att = 3.14159;
	ierr = SMIOL_define_att(file, "surface_pressure", "pi", SMIOL_REAL32,
	                        (const void *)&real32_att);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK - Define a variable REAL64 attribute */
	fprintf(test_log, "Everything OK - Define a variable REAL64 attribute: ");
	real64_att = 2.718281828;
	ierr = SMIOL_define_att(file, "surface_pressure", "e", SMIOL_REAL64,
	                        (const void *)&real64_att);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK - Define another variable REAL64 attribute */
	fprintf(test_log, "Everything OK - Define another variable REAL64 attribute: ");
	real64_att = -1.0;
	ierr = SMIOL_define_att(file, "surface_pressure", "missing_value", SMIOL_REAL64,
	                        (const void *)&real64_att);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK - Define a variable INT32 attribute */
	fprintf(test_log, "Everything OK - Define a variable INT32 attribute: ");
	int32_att = 42;
	ierr = SMIOL_define_att(file, "surface_pressure", "grid_id", SMIOL_INT32,
	                        (const void *)&int32_att);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

	/* Everything OK - Define a variable CHAR attribute */
	fprintf(test_log, "Everything OK - Define a variable CHAR attribute: ");
	snprintf(text_att, 32, "Don't panic!");
	ierr = SMIOL_define_att(file, "surface_pressure", "Advice", SMIOL_CHAR,
	                        (const void *)text_att);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}

#ifdef SMIOL_PNETCDF
	/* Try to define an attribute for a non-existent variable */
	fprintf(test_log, "Try to define an attribute for a non-existent variable: ");
	real64_att = 0.01;
	ierr = SMIOL_define_att(file, "foobar", "min_val", SMIOL_REAL64,
	                        (const void *)&real64_att);
	if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_LIBRARY_ERROR was not returned\n");
		errcount++;
	}

	/* Try to define an attribute with an invalid type */
	fprintf(test_log, "Try to define an attribute with an invalid type: ");
	real64_att = 0.01;
	ierr = SMIOL_define_att(file, NULL, "min_val", SMIOL_UNKNOWN_VAR_TYPE,
	                        (const void *)&real64_att);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_INVALID_ARGUMENT was not returned\n");
		errcount++;
	}
#endif

	/* Call SMIOL_define_att with NULL file pointer */
	fprintf(test_log, "Call SMIOL_define_att with NULL file pointer: ");
	real64_att = 0.01;
	ierr = SMIOL_define_att(NULL, NULL, "blah", SMIOL_REAL64,
	                        (const void *)&real64_att);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_INVALID_ARGUMENT was not returned\n");
		errcount++;
	}

	/* Call SMIOL_define_att with NULL attribute name */
	fprintf(test_log, "Call SMIOL_define_att with NULL attribute name: ");
	real64_att = 0.01;
	ierr = SMIOL_define_att(file, NULL, NULL, SMIOL_REAL64,
	                        (const void *)&real64_att);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_INVALID_ARGUMENT was not returned\n");
		errcount++;
	}

	/* Call SMIOL_define_att with NULL attribute pointer */
	fprintf(test_log, "Call SMIOL_define_att with NULL attribute pointer: ");
	real64_att = 0.01;
	ierr = SMIOL_define_att(file, NULL, "blah", SMIOL_REAL64, NULL);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_INVALID_ARGUMENT was not returned\n");
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
	ierr = SMIOL_open_file(context, "test_atts.nc", SMIOL_FILE_READ, &file, (size_t)0);
	if (ierr != SMIOL_SUCCESS || file == NULL) {
		fprintf(test_log, "Failed to re-open SMIOL file...\n");
		return -1;
	}

#ifdef SMIOL_PNETCDF
	/* Everything OK - Inquire about a global REAL32 attribute */
	fprintf(test_log, "Everything OK - Inquire about a global REAL32 attribute: ");
	real32_att = 0.0;
	ierr = SMIOL_inquire_att(file, NULL, "pi", NULL, NULL,
	                         (void *)&real32_att);
	if (ierr == SMIOL_SUCCESS && real32_att == (float)3.14159) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or attribute value was incorrect\n");
		errcount++;
	}

	/* Everything OK - Inquire about a global REAL64 attribute */
	fprintf(test_log, "Everything OK - Inquire about a global REAL64 attribute: ");
	real64_att = 0.0;
	ierr = SMIOL_inquire_att(file, NULL, "e", NULL, NULL,
	                         (void *)&real64_att);
	if (ierr == SMIOL_SUCCESS && real64_att == (double)2.718281828) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or attribute value was incorrect\n");
		errcount++;
	}

	/* Everything OK - Inquire about a global INT32 attribute */
	fprintf(test_log, "Everything OK - Inquire about a global INT32 attribute: ");
	int32_att = 0;
	ierr = SMIOL_inquire_att(file, NULL, "grid_id", NULL, NULL,
	                         (void *)&int32_att);
	if (ierr == SMIOL_SUCCESS && int32_att == 42) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or attribute value was incorrect\n");
		errcount++;
	}

	/* Everything OK - Inquire about a global CHAR attribute */
	fprintf(test_log, "Everything OK - Inquire about a global CHAR attribute: ");
	memset((void *)text_att, 0, 32);
	ierr = SMIOL_inquire_att(file, NULL, "Advice", NULL, NULL,
	                         (void *)text_att);
	if (ierr == SMIOL_SUCCESS && strncmp("Don't panic!", text_att, 32) == 0) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or attribute value was incorrect\n");
		fprintf(test_log, "%s\n", text_att);
		errcount++;
	}

	/* Everything OK - Inquire about _FillValue variable attribute */
	fprintf(test_log, "Everything OK - Inquire about _FillValue variable attribute: ");
	real32_att = -999.0;
	ierr = SMIOL_inquire_att(file, "surface_pressure", "_FillValue", NULL, NULL,
	                         (void *)&real32_att);
	if (ierr == SMIOL_SUCCESS && real32_att == 0.0) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or attribute value was incorrect\n");
		errcount++;
	}

	/* Everything OK - Inquire about a variable REAL32 attribute */
	fprintf(test_log, "Everything OK - Inquire about a variable REAL32 attribute: ");
	real32_att = 0.0;
	ierr = SMIOL_inquire_att(file, "surface_pressure", "pi", NULL, NULL,
	                         (void *)&real32_att);
	if (ierr == SMIOL_SUCCESS && real32_att == (float)3.14159) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or attribute value was incorrect\n");
		errcount++;
	}

	/* Everything OK - Inquire about a variable REAL64 attribute */
	fprintf(test_log, "Everything OK - Inquire about a variable REAL64 attribute: ");
	real64_att = 0.0;
	ierr = SMIOL_inquire_att(file, "surface_pressure", "e", NULL, NULL,
	                         (void *)&real64_att);
	if (ierr == SMIOL_SUCCESS && real64_att == (double)2.718281828) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or attribute value was incorrect\n");
		errcount++;
	}

	/* Everything OK - Inquire about a variable INT32 attribute */
	fprintf(test_log, "Everything OK - Inquire about a variable INT32 attribute: ");
	int32_att = 0;
	ierr = SMIOL_inquire_att(file, "surface_pressure", "grid_id", NULL, NULL,
	                         (void *)&int32_att);
	if (ierr == SMIOL_SUCCESS && int32_att == 42) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or attribute value was incorrect\n");
		errcount++;
	}

	/* Everything OK - Inquire about a variable CHAR attribute */
	fprintf(test_log, "Everything OK - Inquire about a variable CHAR attribute: ");
	memset((void *)text_att, 0, 32);
	ierr = SMIOL_inquire_att(file, "surface_pressure", "Advice", NULL, NULL,
	                         (void *)text_att);
	if (ierr == SMIOL_SUCCESS && strncmp("Don't panic!", text_att, 32) == 0) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned or attribute value was incorrect\n");
		errcount++;
	}

	/* Try to inquire about an attribute for a non-existent variable */
	fprintf(test_log, "Try to inquire about an attribute for a non-existent variable: ");
	real64_att = 0.0;
	ierr = SMIOL_inquire_att(file, "foobar", "e", NULL, NULL,
	                         (void *)&real64_att);
	if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_LIBRARY_ERROR was not returned\n");
		errcount++;
	}

	/* Try to inquire about a non-existent attribute */
	fprintf(test_log, "Try to inquire about a non-existent attribute: ");
	real64_att = 0.0;
	ierr = SMIOL_inquire_att(file, "surface_pressure", "blah", NULL, NULL,
	                         (void *)&real64_att);
	if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_LIBRARY_ERROR was not returned\n");
		errcount++;
	}
#endif

	/* Call SMIOL_inquire_att with NULL file pointer */
	fprintf(test_log, "Call SMIOL_inquire_att with NULL file pointer: ");
	real64_att = 0.0;
	ierr = SMIOL_inquire_att(NULL, "surface_pressure", "e", NULL, NULL,
	                         (void *)&real64_att);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_INVALID_ARGUMENT was not returned\n");
		errcount++;
	}

	/* Call SMIOL_inquire_att with NULL attribute name */
	fprintf(test_log, "Call SMIOL_inquire_att with NULL attribute name: ");
	real64_att = 0.0;
	ierr = SMIOL_inquire_att(file, "surface_pressure", NULL, NULL, NULL,
	                         (void *)&real64_att);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_INVALID_ARGUMENT was not returned\n");
		errcount++;
	}

	/* Close the SMIOL file */
	ierr = SMIOL_close_file(&file);
	if (ierr != SMIOL_SUCCESS || file != NULL) {
		fprintf(test_log, "Failed to close SMIOL file...\n");
		return -1;
	}

	for (i = 0; i < 2; i++) {
		free(dimnames[i]);
	}
	free(dimnames);

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
	int num_io_tasks;
	int io_stride;


	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************************ SMIOL_sync_file unit tests ****************************\n");
	fprintf(test_log, "\n");

	errcount = 0;

	MPI_Comm_size(MPI_COMM_WORLD, &num_io_tasks);
	io_stride = 1;

	/* Create a SMIOL context for testing file sync routines */
	ierr = SMIOL_init(MPI_COMM_WORLD, num_io_tasks, io_stride, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	/* Open a file for syncing */
	ierr = SMIOL_open_file(context, "smiol_sync_file.nc", SMIOL_FILE_CREATE, &file, (size_t)64000000);
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
	ierr = SMIOL_open_file(context, "smiol_sync_file.nc", SMIOL_FILE_WRITE, &file, (size_t)64000000);
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
	ierr = SMIOL_open_file(context, "smiol_sync_file.nc", SMIOL_FILE_READ, &file, (size_t)0);
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


#if 0
/*
 * Since members of the SMIOL_file struct may be used in various ways within
 * SMIOL_sync_file, and without being able to verify that the contents of the
 * SMIOL_file are valid, this test may lead to segfaults or other crashes.
 * Until we have a way of verifying the validity of a SMIOL_file before using
 * its contents, this test should probably just be omitted.
 */
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

int test_io_decomp(FILE *test_log)
{
	int errcount;
	int ierr;
	int comm_rank, comm_size;
	size_t n_io_elements;
	size_t io_start[16], io_count[16];

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "******************** SMIOL I/O decomposition unit tests ************************\n");
	fprintf(test_log, "\n");

	errcount = 0;

	/*
	 * Perform a few quick tests on the range_intersection function (unit tests within unit tests...)
	 */

	/* No overlap, second range after the first */
	if (range_intersection((size_t)0, (size_t)50, (size_t)50, (size_t)50)) {
		fprintf(test_log, "Function range_intersection broken (test 1)...\n");
		return -1;
	}

	/* No overlap, second range before the first */
	if (range_intersection((size_t)50, (size_t)50, (size_t)0, (size_t)50)) {
		fprintf(test_log, "Function range_intersection broken (test 2)...\n");
		return -1;
	}

	/* Overlap, but zero-sized first range */
	if (range_intersection((size_t)10, (size_t)0, (size_t)0, (size_t)50)) {
		fprintf(test_log, "Function range_intersection broken (test 3)...\n");
		return -1;
	}

	/* Overlap, but zero-sized second range */
	if (range_intersection((size_t)0, (size_t)50, (size_t)10, (size_t)0)) {
		fprintf(test_log, "Function range_intersection broken (test 4)...\n");
		return -1;
	}

	/* Overlap, identical ranges */
	if (!range_intersection((size_t)100, (size_t)50, (size_t)100, (size_t)50)) {
		fprintf(test_log, "Function range_intersection broken (test 5)...\n");
		return -1;
	}

	/* Overlap, second contained within first */
	if (!range_intersection((size_t)0, (size_t)50, (size_t)10, (size_t)1)) {
		fprintf(test_log, "Function range_intersection broken (test 6)...\n");
		return -1;
	}

	/* Overlap, first contained within second */
	if (!range_intersection((size_t)10, (size_t)1, (size_t)0, (size_t)50)) {
		fprintf(test_log, "Function range_intersection broken (test 7)...\n");
		return -1;
	}

	/* Overlap, second begins within first */
	if (!range_intersection((size_t)0, (size_t)50, (size_t)25, (size_t)50)) {
		fprintf(test_log, "Function range_intersection broken (test 8)...\n");
		return -1;
	}

	/* Overlap, first begins within second */
	if (!range_intersection((size_t)25, (size_t)50, (size_t)0, (size_t)50)) {
		fprintf(test_log, "Function range_intersection broken (test 9)...\n");
		return -1;
	}

	/* Overlap at one point, end of first, start of second */
	if (!range_intersection((size_t)0, (size_t)50, (size_t)49, (size_t)50)) {
		fprintf(test_log, "Function range_intersection broken (test 10)...\n");
		return -1;
	}

	/* Overlap at one point, start of first, end of second */
	if (!range_intersection((size_t)99, (size_t)50, (size_t)0, (size_t)100)) {
		fprintf(test_log, "Function range_intersection broken (test 11)...\n");
		return -1;
	}

	/* Testing for non-zero return with NULL io_start */
	fprintf(test_log, "Non-zero return code with NULL io_start: ");
	ierr = get_io_elements(0, 1, 1, 1, NULL, &io_count[0]);
	if (ierr != 0) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - A non-zero status was not returned\n");
		errcount++;
	}

	/* Testing for non-zero return with NULL io_count */
	fprintf(test_log, "Non-zero return code with NULL io_count: ");
	ierr = get_io_elements(0,     /* comm_rank */
	                       1, 1,  /* n_io_tasks, io_stride */
	                       1,     /* n_io_elements */
	                       &io_start[0], NULL);
	if (ierr != 0) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - A non-zero status was not returned\n");
		errcount++;
	}

	/* Test I/O decomp with zero I/O elements, one task */
	fprintf(test_log, "Test I/O decomp with zero I/O elements, one task: ");
	io_count[0] = (size_t)42;     /* any non-zero value will do... */
	ierr = get_io_elements(0,     /* comm_rank */
	                       1, 1,  /* n_io_tasks, io_stride */
	                       0,     /* n_io_elements */
	                       &io_start[0], &io_count[0]);
	if (ierr == 0) {
		if (io_count[0] == 0) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - Expected an io_count of 0 (%lu returned)\n",
			        (unsigned long)io_count[0]);
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - A non-zero value was returned\n");
		errcount++;
	}

	/* Test I/O decomp with one I/O elements, one task */
	fprintf(test_log, "Test I/O decomp with one I/O elements, one task: ");
	io_count[0] = (size_t)42;     /* any non-zero value will do... */
	ierr = get_io_elements(0,     /* comm_rank */
	                       1, 1,  /* n_io_tasks, io_stride */
	                       1,     /* n_io_elements */
	                       &io_start[0], &io_count[0]);
	if (ierr == 0) {
		if (io_count[0] == 1) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - Expected an io_count of 1 (%lu returned)\n",
			        (unsigned long)io_count[0]);
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - A non-zero value was returned\n");
		errcount++;
	}

	/* Test evenly divisible I/O element count, all tasks are I/O tasks */
	fprintf(test_log, "Test evenly divisible I/O element count, all tasks are I/O tasks: ");
	ierr = 0;
	comm_size = 4;
	n_io_elements = 100;
	for (comm_rank = 0; comm_rank < comm_size; comm_rank++) {
		ierr |= get_io_elements(comm_rank,
		                        comm_size, 1,  /* n_io_tasks, io_stride */
		                        n_io_elements,
		                        &io_start[comm_rank], &io_count[comm_rank]);
	}
	if (ierr == 0) {
		if (elements_covered(comm_size, io_start, io_count) == n_io_elements) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - Not all I/O elements covered by decomp\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - Non-zero return code for at least one rank\n");
		errcount++;
	}

	/* Test un-evenly divisible I/O element count, all tasks are I/O tasks */
	fprintf(test_log, "Test un-evenly divisible I/O element count, all tasks are I/O tasks: ");
	ierr = 0;
	comm_size = 4;
	n_io_elements = 103;
	for (comm_rank = 0; comm_rank < comm_size; comm_rank++) {
		ierr |= get_io_elements(comm_rank,
		                        comm_size, 1,  /* n_io_tasks, io_stride */
		                        n_io_elements,
		                        &io_start[comm_rank], &io_count[comm_rank]);
	}
	if (ierr == 0) {
		if (elements_covered(comm_size, io_start, io_count) == n_io_elements) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - Not all I/O elements covered by decomp\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - Non-zero return code for at least one rank\n");
		errcount++;
	}

	/* Test evenly divisible I/O element count, every second tasks is an I/O task */
	fprintf(test_log, "Test evenly divisible I/O element count, every second task is an I/O task: ");
	ierr = 0;
	comm_size = 4;
	n_io_elements = 100;
	for (comm_rank = 0; comm_rank < comm_size; comm_rank++) {
		ierr |= get_io_elements(comm_rank,
		                        comm_size/2, 2,  /* n_io_tasks, io_stride */
		                        n_io_elements,
		                        &io_start[comm_rank], &io_count[comm_rank]);
	}
	if (ierr == 0) {
		if (elements_covered(comm_size, io_start, io_count) == n_io_elements) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - Not all I/O elements covered by decomp\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - Non-zero return code for at least one rank\n");
		errcount++;
	}

	/* Test un-evenly divisible I/O element count, every second task is an I/O task */
	fprintf(test_log, "Test un-evenly divisible I/O element count, every second task is an I/O task: ");
	ierr = 0;
	comm_size = 4;
	n_io_elements = 103;
	for (comm_rank = 0; comm_rank < comm_size; comm_rank++) {
		ierr |= get_io_elements(comm_rank,
		                        comm_size/2, 2,  /* n_io_tasks, io_stride */
		                        n_io_elements,
		                        &io_start[comm_rank], &io_count[comm_rank]);
	}
	if (ierr == 0) {
		if (elements_covered(comm_size, io_start, io_count) == n_io_elements) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - Not all I/O elements covered by decomp\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - Non-zero return code for at least one rank\n");
		errcount++;
	}

	/* Test for more I/O tasks than I/O elements */
	fprintf(test_log, "Test more I/O tasks than I/O elements: ");
	ierr = 0;
	comm_size = 16;
	n_io_elements = 3;
	for (comm_rank = 0; comm_rank < comm_size; comm_rank++) {
		ierr |= get_io_elements(comm_rank,
		                        comm_size/2, 2,  /* n_io_tasks, io_stride */
		                        n_io_elements,
		                        &io_start[comm_rank], &io_count[comm_rank]);
	}
	if (ierr == 0) {
		if (elements_covered(comm_size, io_start, io_count) == n_io_elements) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - Not all I/O elements covered by decomp\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - Non-zero return code for at least one rank\n");
		errcount++;
	}

	/* Test more than 2^32 I/O elements */
	fprintf(test_log, "Test more than 2^32 I/O elements: ");
	ierr = 0;
	comm_size = 16;
	n_io_elements = 65536000002;
	for (comm_rank = 0; comm_rank < comm_size; comm_rank++) {
		ierr |= get_io_elements(comm_rank,
		                        comm_size/2, 2,       /* n_io_tasks, io_stride */
		                        n_io_elements,
		                        &io_start[comm_rank], &io_count[comm_rank]);
	}
	if (ierr == 0) {
		if (elements_covered(comm_size, io_start, io_count) == n_io_elements) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - Not all I/O elements covered by decomp\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - Non-zero return code for at least one rank\n");
		errcount++;
	}

	fflush(test_log);
	fprintf(test_log, "\n");

	return errcount;
}

int test_aggregate_list(FILE *test_log)
{
	int errcount;
	int ierr;
	int root;
	SMIOL_Offset *in_list, *out_list;
	SMIOL_Offset *out_list_expected;
	size_t n_in, n_out;
	size_t n_out_expected;
	size_t i, j;
	int *counts, *displs;
	int *counts_expected, *displs_expected;
	int world_rank, world_size;

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "********************* SMIOL list aggregation unit tests ************************\n");
	fprintf(test_log, "\n");

	errcount = 0;

	ierr = MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
	if (ierr != MPI_SUCCESS) {
		fprintf(test_log, "Failed to get MPI rank...\n");
		return -1;
	}

	ierr = MPI_Comm_size(MPI_COMM_WORLD, &world_size);
	if (ierr != MPI_SUCCESS) {
		fprintf(test_log, "Failed to get MPI size...\n");
		return -1;
	}

	root = 0;

	/* Aggregate a list across a communicator of size 1 (using MPI_COMM_SELF) */
	fprintf(test_log, "Testing aggregation of lists across communicator of size 1: ");
	n_in = 1000;
	n_out = SIZE_MAX;
	in_list = malloc(sizeof(SMIOL_Offset) * n_in);

	for (i = 0; i < n_in; i++) {
		in_list[i] = (SMIOL_Offset)(2 * i);   /* Initialize in_list with positive consecutive even integers */
	}

	ierr = aggregate_list(MPI_COMM_SELF, root, n_in, in_list,
	                      &n_out, &out_list,
	                      &counts, &displs);
	if (ierr == 0) {
		if (n_out != n_in) {
			fprintf(test_log, "FAIL - n_out != n_in\n");
			errcount++;
		} else if (counts == NULL || displs == NULL) {
			fprintf(test_log, "FAIL - either counts or displs not allocated\n");
			errcount++;
		} else if (counts[0] != (int)n_in) {
			fprintf(test_log, "FAIL - counts is incorrect\n");
			errcount++;
		} else if (displs[0] != 0) {
			fprintf(test_log, "FAIL - displs is incorrect\n");
			errcount++;
		} else if (out_list == NULL) {
			fprintf(test_log, "FAIL - out_list not allocated\n");
			errcount++;
		} else if (memcmp(in_list, out_list, sizeof(SMIOL_Offset) * n_in)) {
			fprintf(test_log, "FAIL - out_list differs from in_list\n");
			errcount++;
		} else {
			fprintf(test_log, "PASS\n");
			free(out_list);
			free(counts);
			free(displs);
		}
	} else {
		fprintf(test_log, "FAIL - non-zero return error code\n");
		errcount++;
	}
	free(in_list);

	/* Aggregate zero-sized lists */
	fprintf(test_log, "Testing aggregation of zero-sized lists on all tasks: ");
	n_in = 0;
	n_out = SIZE_MAX;
	in_list = malloc(sizeof(SMIOL_Offset) * n_in);
	counts_expected = malloc(sizeof(int) * (size_t)world_size);
	displs_expected = malloc(sizeof(int) * (size_t)world_size);

	counts_expected[0] = (int)n_in;
	displs_expected[0] = 0;
	for (i = 1; i < (size_t)world_size; i++) {
		counts_expected[i] = (int)n_in;
		displs_expected[i] = displs_expected[i-1] + counts_expected[i-1];
	}

	ierr = aggregate_list(MPI_COMM_WORLD, root, n_in, in_list,
	                      &n_out, &out_list,
	                      &counts, &displs);
	if (ierr == 0) {
		if (n_out != n_in) {
			fprintf(test_log, "FAIL - n_out != n_in\n");
			errcount++;
		} else if (world_rank == root && (counts == NULL || displs == NULL)) {
			fprintf(test_log, "FAIL - either counts or displs not allocated on root rank\n");
			errcount++;
		} else if (world_rank == root && out_list == NULL) {
			fprintf(test_log, "FAIL - out_list not allocated on root rank\n");
			errcount++;
		} else if (world_rank != root && (counts != NULL || displs != NULL)) {
			fprintf(test_log, "FAIL - either counts or displs is allocated on non-root rank\n");
			errcount++;
		} else if (world_rank != root && out_list != NULL) {
			fprintf(test_log, "FAIL - out_list is allocated on non-root rank\n");
			errcount++;
		} else if (world_rank == root && memcmp(counts, counts_expected, sizeof(int) * (size_t)world_size)) { /* Check counts on root rank */
			fprintf(test_log, "FAIL - counts contains incorrect values\n");
			errcount++;
		} else if (world_rank == root && memcmp(displs, displs_expected, sizeof(int) * (size_t)world_size)) { /* Check displs on root rank */
			fprintf(test_log, "FAIL - displs contains incorrect values\n");
			errcount++;
		} else {
			fprintf(test_log, "PASS\n");
			free(out_list);
			free(counts);
			free(displs);
		}
	} else {
		fprintf(test_log, "FAIL - non-zero return error code\n");
		errcount++;
	}
	free(in_list);
	free(counts_expected);
	free(displs_expected);

	for (root = 0; root < world_size; root++) {

		/* Aggregate one-sized lists */
		fprintf(test_log, "Testing aggregation to %i of one-sized lists on all tasks: ", root);
		n_in = 1;
		n_out = SIZE_MAX;
		if (world_rank == root) {
			n_out_expected = n_in * (size_t)world_size;
		} else {
			n_out_expected = 0;
		}
		out_list = NULL;
		counts = NULL;
		displs = NULL;
		in_list = malloc(sizeof(SMIOL_Offset) * n_in);
		out_list_expected = malloc(sizeof(SMIOL_Offset) * n_out_expected);
		counts_expected = malloc(sizeof(int) * (size_t)world_size);
		displs_expected = malloc(sizeof(int) * (size_t)world_size);

		in_list[0] = 100 * world_rank;

		if (world_rank == root) {
			for (i = 0; i < (size_t)world_size; i++) {
				out_list_expected[i] = (SMIOL_Offset)(100 * i);
			}
		}

		counts_expected[0] = (int)n_in;
		displs_expected[0] = 0;
		for (i = 1; i < (size_t)world_size; i++) {
			counts_expected[i] = (int)n_in;
			displs_expected[i] = displs_expected[i-1] + counts_expected[i-1];
		}

		ierr = aggregate_list(MPI_COMM_WORLD, root, n_in, in_list,
		                      &n_out, &out_list,
		                      &counts, &displs);
		if (ierr == 0) {
			if (world_rank == root && n_out != n_out_expected) {
				fprintf(test_log, "FAIL - n_out is incorrect on root rank\n");
				errcount++;
			} else if (world_rank != root && n_out != 0) {
				fprintf(test_log, "FAIL - n_out != 0 on non-root rank\n");
				errcount++;
			} else if (world_rank == root && (counts == NULL || displs == NULL)) {
				fprintf(test_log, "FAIL - either counts or displs not allocated on root rank\n");
				errcount++;
			} else if (world_rank == root && out_list == NULL) {
				fprintf(test_log, "FAIL - out_list not allocated on root rank\n");
				errcount++;
			} else if (world_rank != root && (counts != NULL || displs != NULL)) {
				fprintf(test_log, "FAIL - either counts or displs is allocated on non-root rank\n");
				errcount++;
			} else if (world_rank != root && out_list != NULL) {
				fprintf(test_log, "FAIL - out_list is allocated on non-root rank\n");
				errcount++;
			} else if (memcmp(out_list, out_list_expected, sizeof(SMIOL_Offset) * n_out_expected)) {      /* Check out_list values on all ranks */
				fprintf(test_log, "FAIL - out_list contains incorrect values\n");
				errcount++;
			} else if (world_rank == root && memcmp(counts, counts_expected, sizeof(int) * (size_t)world_size)) { /* Check counts on root rank */
				fprintf(test_log, "FAIL - counts contains incorrect values\n");
				errcount++;
			} else if (world_rank == root && memcmp(displs, displs_expected, sizeof(int) * (size_t)world_size)) { /* Check displs on root rank */
				fprintf(test_log, "FAIL - displs contains incorrect values\n");
				errcount++;
			} else {
				fprintf(test_log, "PASS\n");
				free(out_list);
				free(counts);
				free(displs);
			}
		} else {
			fprintf(test_log, "FAIL - non-zero return error code\n");
			errcount++;
		}
		free(in_list);
		free(out_list_expected);
		free(counts_expected);
		free(displs_expected);

		/* Aggregate N-sized lists */
		fprintf(test_log, "Testing aggregation to %i of N-sized lists on all tasks: ", root);
		n_in = 1000;
		n_out = SIZE_MAX;
		if (world_rank == root) {
			n_out_expected = n_in * (size_t)world_size;
		} else {
			n_out_expected = 0;
		}
		out_list = NULL;
		counts = NULL;
		displs = NULL;
		in_list = malloc(sizeof(SMIOL_Offset) * n_in);
		out_list_expected = malloc(sizeof(SMIOL_Offset) * n_out_expected);
		counts_expected = malloc(sizeof(int) * (size_t)world_size);
		displs_expected = malloc(sizeof(int) * (size_t)world_size);

		for (i = 0; i < n_in; i++) {
			in_list[i] = (SMIOL_Offset)((size_t)world_rank + 2 * i);   /* Initialize in_list with positive consecutive integers with offset */
		}

		if (world_rank == root) {
			for (j = 0; j < (size_t)world_size; j++) {
				for (i = 0; i < n_in; i++) {
					out_list_expected[j * n_in + i] = (SMIOL_Offset)(j + 2 * i);
				}
			}
		}

		counts_expected[0] = (int)n_in;
		displs_expected[0] = 0;
		for (i = 1; i < (size_t)world_size; i++) {
			counts_expected[i] = (int)n_in;
			displs_expected[i] = displs_expected[i-1] + counts_expected[i-1];
		}

		ierr = aggregate_list(MPI_COMM_WORLD, root, n_in, in_list,
		                      &n_out, &out_list,
		                      &counts, &displs);
		if (ierr == 0) {
			if (world_rank == root && n_out != n_out_expected) {                      /* Check n_out on root rank */
				fprintf(test_log, "FAIL - n_out is incorrect on root rank\n");
				errcount++;
			} else if (world_rank != root && n_out != 0) {                            /* Check n_out on other ranks */
				fprintf(test_log, "FAIL - n_out != 0 on non-root rank\n");
				errcount++;
			} else if (world_rank == root && (counts == NULL || displs == NULL)) {    /* Check non-NULL counts/displs on root rank */
				fprintf(test_log, "FAIL - either counts or displs not allocated on root rank\n");
				errcount++;
			} else if (world_rank == root && out_list == NULL) {                      /* Check non-NULL out_list on root rank */
				fprintf(test_log, "FAIL - out_list not allocated on root rank\n");
				errcount++;
			} else if (world_rank != root && (counts != NULL || displs != NULL)) {    /* Check NULL counts/displs on other ranks */
				fprintf(test_log, "FAIL - either counts or displs is allocated on non-root rank\n");
				errcount++;
			} else if (world_rank != root && out_list != NULL) {                      /* Check NULL out_list on other ranks */
				fprintf(test_log, "FAIL - out_list is allocated on non-root rank\n");
				errcount++;
			} else if (memcmp(out_list, out_list_expected, sizeof(SMIOL_Offset) * n_out_expected)) {      /* Check out_list values on all ranks */
				fprintf(test_log, "FAIL - out_list contains incorrect values\n");
				errcount++;
			} else if (world_rank == root && memcmp(counts, counts_expected, sizeof(int) * (size_t)world_size)) { /* Check counts on root rank */
				fprintf(test_log, "FAIL - counts contains incorrect values\n");
				errcount++;
			} else if (world_rank == root && memcmp(displs, displs_expected, sizeof(int) * (size_t)world_size)) { /* Check displs on root rank */
				fprintf(test_log, "FAIL - displs contains incorrect values\n");
				errcount++;
			} else {
				fprintf(test_log, "PASS\n");
				free(out_list);
				free(counts);
				free(displs);
			}
		} else {
			fprintf(test_log, "FAIL - non-zero return error code\n");
			errcount++;
		}
		free(in_list);
		free(out_list_expected);
		free(counts_expected);
		free(displs_expected);

		/* Aggregate mixed-sized lists -- tasks will have input list sizes of (N-1), 0, (N-2), 1, (N-3), 2, ... */
		fprintf(test_log, "Testing aggregation to %i of mixed-sized lists on all tasks: ", root);
		if (world_rank % 2 == 0) {
			n_in = (size_t)(10 * (world_size - world_rank / 2 - 1));
		} else {
			n_in = (size_t)(10 * (world_rank / 2));
		}
		n_out = SIZE_MAX;
		if (world_rank == root) {
			n_out_expected = (size_t)(10 * world_size * (world_size - 1) / 2);
		} else {
			n_out_expected = 0;
		}
		out_list = NULL;
		counts = NULL;
		displs = NULL;
		in_list = malloc(sizeof(SMIOL_Offset) * n_in);
		out_list_expected = malloc(sizeof(SMIOL_Offset) * n_out_expected);
		counts_expected = malloc(sizeof(int) * (size_t)world_size);
		displs_expected = malloc(sizeof(int) * (size_t)world_size);

		for (i = 0; i < n_in; i++) {
			in_list[i] = world_rank;   /* Initialize in_list with rank in global communicator */
		}

		for (i = 0; i < (size_t)world_size; i++) {
			if (i % 2 == 0) {
				counts_expected[i] = (int)(10 * ((size_t)world_size - i / 2 - 1));
			} else {
				counts_expected[i] = (int)(10 * (i / 2));
			}
		}

		displs_expected[0] = 0;
		for (i = 1; i < (size_t)world_size; i++) {
			displs_expected[i] = displs_expected[i-1] + counts_expected[i-1];
		}

		if (world_rank == root) {
			for (j = 0; j < (size_t)world_size; j++) {
				for (i = 0; i < (size_t)counts_expected[j]; i++) {
					out_list_expected[(size_t)displs_expected[j] + i] = (SMIOL_Offset)j;
				}
			}
		}

		ierr = aggregate_list(MPI_COMM_WORLD, root, n_in, in_list,
		                      &n_out, &out_list,
		                      &counts, &displs);
		if (ierr == 0) {
			if (world_rank == root && n_out != n_out_expected) {                      /* Check n_out on root rank */
				fprintf(test_log, "FAIL - n_out is incorrect on root rank %i %i\n", (int)n_out, (int)n_out_expected);
				errcount++;
			} else if (world_rank != root && n_out != 0) {                            /* Check n_out on other ranks */
				fprintf(test_log, "FAIL - n_out != 0 on non-root rank\n");
				errcount++;
			} else if (world_rank == root && (counts == NULL || displs == NULL)) {    /* Check non-NULL counts/displs on root rank */
				fprintf(test_log, "FAIL - either counts or displs not allocated on root rank\n");
				errcount++;
			} else if (world_rank == root && out_list == NULL) {                      /* Check non-NULL out_list on root rank */
				fprintf(test_log, "FAIL - out_list not allocated on root rank\n");
				errcount++;
			} else if (world_rank != root && (counts != NULL || displs != NULL)) {    /* Check NULL counts/displs on other ranks */
				fprintf(test_log, "FAIL - either counts or displs is allocated on non-root rank\n");
				errcount++;
			} else if (world_rank != root && out_list != NULL) {                      /* Check NULL out_list on other ranks */
				fprintf(test_log, "FAIL - out_list is allocated on non-root rank\n");
				errcount++;
			} else if (memcmp(out_list, out_list_expected, sizeof(SMIOL_Offset) * n_out_expected)) {      /* Check out_list values on all ranks */
				fprintf(test_log, "FAIL - out_list contains incorrect values\n");
				errcount++;
			} else if (world_rank == root && memcmp(counts, counts_expected, sizeof(int) * (size_t)world_size)) { /* Check counts on root rank */
				fprintf(test_log, "FAIL - counts contains incorrect values\n");
				errcount++;
			} else if (world_rank == root && memcmp(displs, displs_expected, sizeof(int) * (size_t)world_size)) { /* Check displs on root rank */
				fprintf(test_log, "FAIL - displs contains incorrect values\n");
				errcount++;
			} else {
				fprintf(test_log, "PASS\n");
				free(out_list);
				free(counts);
				free(displs);
			}
		} else {
			fprintf(test_log, "FAIL - non-zero return error code\n");
			errcount++;
		}
		free(in_list);
		free(out_list_expected);
		free(counts_expected);
		free(displs_expected);

	} /* root loop */

	fflush(test_log);
	fprintf(test_log, "\n");

	return errcount;
}

int test_set_get_frame(FILE* test_log)
{

	int errcount;
	int ierr;
	struct SMIOL_context *context;
	struct SMIOL_file *file;
	SMIOL_Offset frame;
	int num_io_tasks;
	int io_stride;


	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************************** SMIOL_set/get_frame tests ***************************\n");
	fprintf(test_log, "\n");

	errcount = 0;

	MPI_Comm_size(MPI_COMM_WORLD, &num_io_tasks);
	io_stride = 1;

	/* Create a SMIOL context for testing variable routines */
	context = NULL;
	ierr = SMIOL_init(MPI_COMM_WORLD, num_io_tasks, io_stride, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	/* See if the frame is set correctly when opening a file */
	fprintf(test_log, "Everything OK - Frame set correct on file open: ");
	file = NULL;
	ierr = SMIOL_open_file(context, "test_frame.nc", SMIOL_FILE_CREATE, &file, (size_t)64000000);
	if (ierr != SMIOL_SUCCESS || file == NULL) {
		fprintf(test_log, "Failed to create SMIOL file...\n");
		return -1;
	}
	if (file->frame != 0) {
		fprintf(test_log, "FAIL - Frame was not '0'\n");
		errcount++;
	} else {
		fprintf(test_log, "PASS\n");
	}


	/* Set the frame to 1 */
	fprintf(test_log, "Everything OK - Setting the frame to one: ");
	ierr = SMIOL_set_frame(file, (SMIOL_Offset) 1);
	if (ierr == SMIOL_SUCCESS && file->frame == 1) {
		fprintf(test_log, "PASS\n");
	} else if (ierr != SMIOL_SUCCESS && file->frame == 1) {
		fprintf(test_log, "FAIL - frame was 1, but SMIOL_SUCCESS was not returned\n");
		errcount++;
	} else if (ierr == SMIOL_SUCCESS && file->frame != 1) {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but frame was not 1\n");
		errcount++;
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned and frame was not 1\n");
	}

	fprintf(test_log, "Everything OK - Get frame returns 1: ");
	ierr = SMIOL_get_frame(file, &frame);
	if (ierr == SMIOL_SUCCESS && frame == 1) {
		fprintf(test_log, "PASS\n");
	} else if (ierr != SMIOL_SUCCESS && frame == 1) {
		fprintf(test_log, "FAIL - frame was 1, but SMIOL_SUCCESS was not returned\n");
		errcount++;
	} else if (ierr == SMIOL_SUCCESS && frame != 1) {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but frame was not 1\n");
		errcount++;
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned and frame was not 1\n");
	}

	/* Set the frame to a large number */
	fprintf(test_log, "Everything OK - Setting the frame to a large record number: ");
	ierr = SMIOL_set_frame(file, (SMIOL_Offset) 4300000000);
	if (ierr == SMIOL_SUCCESS && file->frame == (SMIOL_Offset) 4300000000) {
		fprintf(test_log, "PASS\n");
	} else if (ierr != SMIOL_SUCCESS && file->frame == (SMIOL_Offset) 4300000000) {
		fprintf(test_log, "FAIL - frame was 1, but SMIOL_SUCCESS was not returned\n");
		errcount++;
	} else if (ierr == SMIOL_SUCCESS && file->frame != (SMIOL_Offset) 430000000) {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but frame was not 4,300,000,000\n");
		errcount++;
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned and frame was not 4,300,000,000\n");
	}


	fprintf(test_log, "Everything OK - Get frame returns large record number: ");
	ierr = SMIOL_get_frame(file, &frame);
	if (ierr == SMIOL_SUCCESS && frame == (SMIOL_Offset) 4300000000) {
		fprintf(test_log, "PASS\n");
	} else if (ierr != SMIOL_SUCCESS && frame == (SMIOL_Offset) 4300000000) {
		fprintf(test_log, "FAIL - frame was 4,300,000,000, but SMIOL_SUCCESS was not returned\n");
		errcount++;
	} else if (ierr == SMIOL_SUCCESS && frame != (SMIOL_Offset) 4300000000) {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was returned, but frame was not 4,300,000,000\n");
		errcount++;
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned and frame was not 4,300,000,000\n");
	}

	fprintf(test_log, "Everything OK - Testing set frame with a NULL file: ");
	ierr = SMIOL_set_frame(NULL, 1);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - Set frame did not return SMIOL_INVALID_ARGUMENT\n");
	}

	fprintf(test_log, "Everything OK - Testing get frame with a NULL file: ");
	ierr = SMIOL_get_frame(NULL, &frame);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - Get frame did not return SMIOL_INVALID_ARGUMENT\n");
	}

	/* Close the file */
	ierr = SMIOL_close_file(&file);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to close file\n");
		return -1;
	}

	ierr = SMIOL_finalize(&context);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to free SMIOL context...\n");
		return -1;
	}

	fflush(test_log);
	fprintf(test_log, "\n");

	return errcount;
}

int test_put_get_vars(FILE *test_log)
{
	int errcount;
	int ierr;
	size_t i, j;
	int valid_comm_size;
	int comm_rank, comm_size;
	int num_io_tasks, io_stride;
	size_t n_compute_elements;
	SMIOL_Offset *compute_elements;
	SMIOL_Offset nCells, nVertLevels, strLen, nTasks;
	SMIOL_Offset compute_element;
	struct SMIOL_context *context;
	struct SMIOL_decomp *decomp;
	struct SMIOL_decomp *decomp2;
	struct SMIOL_file *file;
	char **dimnames;
	float vers;
	float vers_valid;
	float *foo;
	float *foo_valid;
	double *coeffs;
	double *coeffs_valid;
	int *pbl_mask;
	int *pbl_mask_valid;
	char *id_string;
	char *id_string_valid;
	double timestamp;
	double timestamp_valid;

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "************************ SMIOL_put_var / SMIOL_get_var *************************\n");
	fprintf(test_log, "\n");

	errcount = 0;

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

	/*
	 * Some of the tests, below, will only work for specific numbers of MPI
	 * tasks; determine here whether the number of tasks is appropriate
	 */
	if (floor((double)120 / (double)comm_size) == ceil((double)120 / (double)comm_size)) {
		valid_comm_size = 1;
	} else {
		valid_comm_size = 0;
	}

	strLen = 64;
	nCells = 120;
	nTasks = comm_size;
	nVertLevels = 10;

	num_io_tasks = comm_size / 2;
	num_io_tasks = (num_io_tasks <= 0) ? 1 : num_io_tasks;  /* Always use at least one I/O task */
	io_stride = 2;

	/* Create a SMIOL context */
	context = NULL;
	ierr = SMIOL_init(MPI_COMM_WORLD, num_io_tasks, io_stride, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	if (valid_comm_size) {
		/* Create a decomp for testing parallel I/O */
		n_compute_elements = (size_t)(nCells / comm_size);
		compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
		for (i = 0; i < n_compute_elements; i++) {
			compute_elements[i] = comm_rank * (nCells / comm_size) + (SMIOL_Offset)i;
		}
		ierr = SMIOL_create_decomp(context,
		                           n_compute_elements, compute_elements, 1,
		                           &decomp);
		if (ierr != SMIOL_SUCCESS) {
			fprintf(test_log, "Failed to create decomp...\n");
			return -1;
		}

		free(compute_elements);
	} else {
		n_compute_elements = 0;
		decomp = NULL;
	}

	/* Create a second decomp in which each task owns just one element */
	compute_element = comm_rank;
	ierr = SMIOL_create_decomp(context,
	                           1, &compute_element, 1,
	                           &decomp2);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create decomp2...\n");
		return -1;
	}

	/* Create a SMIOL file */
	file = NULL;
	ierr = SMIOL_open_file(context, "test_put_get_vars.nc", SMIOL_FILE_CREATE, &file, (size_t)64000000);
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

	ierr = SMIOL_define_dim(file, "strLen", strLen);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create dimension strLen...\n");
		return -1;
	}

	ierr = SMIOL_define_dim(file, "nTasks", nTasks);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create dimension nTasks...\n");
		return -1;
	}

	ierr = SMIOL_define_dim(file, "nCells", nCells);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create dimension nCells...\n");
		return -1;
	}

	ierr = SMIOL_define_dim(file, "nVertLevels", nVertLevels);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create dimension nVertLevels...\n");
		return -1;
	}

	dimnames = (char **)malloc(sizeof(char *) * (size_t)3);
	for (i = 0; i < (size_t)3; i++) {
		dimnames[i] = (char *)malloc(sizeof(char) * (size_t)32);
	}

	/* Define a float variable with no dimensions */
	ierr = SMIOL_define_var(file, "version", SMIOL_REAL32, 0, NULL);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create variable...\n");
		return -1;
	}

	/* Define a variable with only a record dimension */
	snprintf(dimnames[0], 32, "Time");
	ierr = SMIOL_define_var(file, "seconds_since_epoch", SMIOL_REAL64, 1, (const char **)dimnames);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create variable...\n");
		return -1;
	}

	/* Define a character variable with two non-record dimensions */
	snprintf(dimnames[0], 32, "nTasks");
	snprintf(dimnames[1], 32, "strLen");
	ierr = SMIOL_define_var(file, "id_string", SMIOL_CHAR, 2, (const char **)dimnames);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create variable...\n");
		return -1;
	}

	/* Define a 32-bit integer variable with one non-record dimensions and a record dimension*/
	snprintf(dimnames[0], 32, "Time");
	snprintf(dimnames[1], 32, "nVertLevels");
	ierr = SMIOL_define_var(file, "pbl_mask", SMIOL_INT32, 2, (const char **)dimnames);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create variable...\n");
		return -1;
	}

	/* Define a 64-bit real variable with one non-record dimensions */
	snprintf(dimnames[0], 32, "nVertLevels");
	ierr = SMIOL_define_var(file, "coeffs", SMIOL_REAL64, 1, (const char **)dimnames);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create variable...\n");
		return -1;
	}

	/* Define a 32-bit real variable with two non-record dimensions and a record dimension */
	snprintf(dimnames[0], 32, "Time");
	snprintf(dimnames[1], 32, "nCells");
	snprintf(dimnames[2], 32, "nVertLevels");
	ierr = SMIOL_define_var(file, "foo", SMIOL_REAL32, 3, (const char **)dimnames);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create variable...\n");
		return -1;
	}

	for (i = 0; i < (size_t)3; i++) {
		free(dimnames[i]);
	}
	free(dimnames);

	/* Allocate a non-decomposed variable */
	coeffs = malloc(sizeof(double) * (size_t)nVertLevels);
	coeffs[0] = 0.1;
	for (i = 1; i < (size_t)nVertLevels; i++) {
		coeffs[i] = coeffs[i-1] * 0.001;
	}

	/* Allocate another non-decomposed variable */
	pbl_mask = malloc(sizeof(int) * (size_t)nVertLevels);
	pbl_mask[0] = 1;
	for (i = 1; i < (size_t)nVertLevels; i++) {
		pbl_mask[i] = 0;
	}

	/* Allocate a variable decomposed by decomp */
	foo = malloc(sizeof(float) * n_compute_elements * (size_t)nVertLevels);
	for (i = 0; i < n_compute_elements; i++) {
		for (j = 0; j < (size_t)nVertLevels; j++) {
			foo[i*(size_t)nVertLevels + j] = (float)((size_t)comm_rank*n_compute_elements + i + j);
		}
	}

	/* Allocate a variable decomposed by decomp2 */
	id_string = malloc(sizeof(char) * (size_t)strLen);
	memset((void *)id_string, 0, sizeof(char) * (size_t)strLen);
	snprintf(id_string, (size_t)strLen, "MPI task %3.3i", comm_rank);

	/* Supply a NULL file argument */
	fprintf(test_log, "Supply a NULL file argument to SMIOL_put_var: ");
	ierr = SMIOL_put_var(NULL, "foo", decomp, foo);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_INVALID_ARGUMENT was not returned\n");
		errcount++;
	}

	/* Supply a NULL varname argument */
	fprintf(test_log, "Supply a NULL varname argument to SMIOL_put_var: ");
	ierr = SMIOL_put_var(file, NULL, decomp, foo);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_INVALID_ARGUMENT was not returned\n");
		errcount++;
	}

	/* Try to write to a non-existent variable */
	fprintf(test_log, "Try to write to a non-existent variable: ");
	ierr = SMIOL_put_var(file, "blargh", decomp, foo);
#ifdef SMIOL_PNETCDF
	if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "PASS (%s)\n", SMIOL_lib_error_string(context));
	} else {
		fprintf(test_log, "FAIL - SMIOL_LIBRARY_ERROR was not returned\n");
		errcount++;
	}
#else
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}
#endif

	/* Write a variable with no dimensions */
	fprintf(test_log, "Write a variable with no dimensions: ");
	vers = (float)1.0;
	ierr = SMIOL_put_var(file, "version", NULL, &vers);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Write a non-decomposed variable with no record dimension */
	fprintf(test_log, "Write a non-decomposed variable with no record dimension: ");
	ierr = SMIOL_put_var(file, "coeffs", NULL, coeffs);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Write a decomposed variable with no record dimension */
	fprintf(test_log, "Write a decomposed variable with no record dimension: ");
	ierr = SMIOL_put_var(file, "id_string", decomp2, id_string);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	if (valid_comm_size) {
		/* Write frame 0 of a decomposed variable with a record dimension */
		fprintf(test_log, "Write frame 0 of a decomposed variable with a record dimension: ");
		ierr = SMIOL_put_var(file, "foo", decomp, foo);
		if (ierr == SMIOL_SUCCESS) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
			errcount++;
		}
	}

	/* Write frame 0 of a non-decomposed variable with a record dimension */
	fprintf(test_log, "Write frame 0 of a non-decomposed variable with a record dimension: ");
	ierr = SMIOL_put_var(file, "pbl_mask", NULL, pbl_mask);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Write frame 0 of a 0-d variable with a record dimension */
	fprintf(test_log, "Write frame 0 of a 0-d variable with a record dimension: ");
	timestamp = 1594253412.75;
	ierr = SMIOL_put_var(file, "seconds_since_epoch", NULL, &timestamp);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	ierr = SMIOL_set_frame(file, (SMIOL_Offset)1);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to advance frame in file...\n");
		return -1;
	}

	for (i = 0; i < n_compute_elements; i++) {
		for (j = 0; j < (size_t)nVertLevels; j++) {
			foo[i*(size_t)nVertLevels + j] *= (float)10.0;
		}
	}

	pbl_mask[1] = 1;
	pbl_mask[2] = 1;
	pbl_mask[3] = 1;

	if (valid_comm_size) {
		/* Write frame 1 of a decomposed variable with a record dimension */
		fprintf(test_log, "Write frame 1 of a decomposed variable with a record dimension: ");
		ierr = SMIOL_put_var(file, "foo", decomp, foo);
		if (ierr == SMIOL_SUCCESS) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
			errcount++;
		}
	}

	/* Write frame 1 of a non-decomposed variable with a record dimension */
	fprintf(test_log, "Write frame 1 of a non-decomposed variable with a record dimension: ");
	ierr = SMIOL_put_var(file, "pbl_mask", NULL, pbl_mask);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Write frame 1 of a 0-d variable with a record dimension */
	fprintf(test_log, "Write frame 1 of a 0-d variable with a record dimension: ");
	timestamp = 1594253412.875;
	ierr = SMIOL_put_var(file, "seconds_since_epoch", NULL, &timestamp);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	ierr = SMIOL_set_frame(file, (SMIOL_Offset)2);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to advance frame in file...\n");
		return -1;
	}

	for (i = 0; i < n_compute_elements; i++) {
		for (j = 0; j < (size_t)nVertLevels; j++) {
			foo[i*(size_t)nVertLevels + j] *= (float)-1.0;
		}
	}

	pbl_mask[4] = 1;
	pbl_mask[5] = 1;
	pbl_mask[6] = 1;

	if (valid_comm_size) {
		/* Write frame 2 of a decomposed variable with a record dimension */
		fprintf(test_log, "Write frame 2 of a decomposed variable with a record dimension: ");
		ierr = SMIOL_put_var(file, "foo", decomp, foo);
		if (ierr == SMIOL_SUCCESS) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
			errcount++;
		}
	}

	/* Write frame 2 of a non-decomposed variable with a record dimension */
	fprintf(test_log, "Write frame 2 of a non-decomposed variable with a record dimension: ");
	ierr = SMIOL_put_var(file, "pbl_mask", NULL, pbl_mask);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Write frame 2 of a 0-d variable with a record dimension */
	fprintf(test_log, "Write frame 2 of a 0-d variable with a record dimension: ");
	timestamp = 1594253413.0;
	ierr = SMIOL_put_var(file, "seconds_since_epoch", NULL, &timestamp);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	free(id_string);
	free(foo);
	free(coeffs);
	free(pbl_mask);

	/* Close the SMIOL file */
	ierr = SMIOL_close_file(&file);
	if (ierr != SMIOL_SUCCESS || file != NULL) {
		fprintf(test_log, "Failed to close SMIOL file...\n");
		return -1;
	}

	ierr = SMIOL_free_decomp(&decomp);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to free decomp...\n");
		return -1;
	}

	if (!valid_comm_size) {
		fprintf(test_log, "<<< Some tests that require the number of MPI tasks to divide 120 were not run >>>\n");
	}


	/*****     Begin tests for SMIOL_get_var     *****/


	if (valid_comm_size) {
		/* Create a decomp for testing parallel I/O */
		n_compute_elements = (size_t)(nCells / comm_size);
		compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
		for (i = 0; i < n_compute_elements; i++) {
			compute_elements[i] = comm_rank + comm_size * (SMIOL_Offset)i;
		}
		ierr = SMIOL_create_decomp(context,
		                           n_compute_elements, compute_elements, 1,
		                           &decomp);
		if (ierr != SMIOL_SUCCESS) {
			fprintf(test_log, "Failed to create decomp...\n");
			return -1;
		}

		free(compute_elements);
	} else {
		n_compute_elements = 0;
		decomp = NULL;
	}

	/* Allocate a variable decomposed by decomp */
	foo = malloc(sizeof(float) * n_compute_elements * (size_t)nVertLevels);
	foo_valid = malloc(sizeof(float) * n_compute_elements * (size_t)nVertLevels);

	/* Allocate a variable decomposed by decomp2 */
	id_string = malloc(sizeof(char) * (size_t)strLen);
	id_string_valid = malloc(sizeof(char) * (size_t)strLen);

	/* Re-open the SMIOL file */
	file = NULL;
	ierr = SMIOL_open_file(context, "test_put_get_vars.nc", SMIOL_FILE_READ, &file, (size_t)0);
	if (ierr != SMIOL_SUCCESS || file == NULL) {
		fprintf(test_log, "Failed to create SMIOL file...\n");
		return -1;
	}

	/* Supply a NULL file argument */
	fprintf(test_log, "Supply a NULL file argument to SMIOL_get_var: ");
	ierr = SMIOL_get_var(NULL, "foo", decomp, foo);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_INVALID_ARGUMENT was not returned\n");
		errcount++;
	}

	/* Supply a NULL varname argument */
	fprintf(test_log, "Supply a NULL varname argument to SMIOL_get_var: ");
	ierr = SMIOL_get_var(file, NULL, decomp, foo);
	if (ierr == SMIOL_INVALID_ARGUMENT) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_INVALID_ARGUMENT was not returned\n");
		errcount++;
	}

	/* Try to read from a non-existent variable */
	fprintf(test_log, "Try to read from a non-existent variable: ");
	ierr = SMIOL_get_var(file, "blaz", decomp, foo);
#ifdef SMIOL_PNETCDF
	if (ierr == SMIOL_LIBRARY_ERROR) {
		fprintf(test_log, "PASS (%s)\n", SMIOL_lib_error_string(context));
	} else {
		fprintf(test_log, "FAIL - SMIOL_LIBRARY_ERROR was not returned\n");
		errcount++;
	}
#else
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned\n");
		errcount++;
	}
#endif

	/* Allocate a non-decomposed variable */
	coeffs = malloc(sizeof(double) * (size_t)nVertLevels);
	coeffs_valid = malloc(sizeof(double) * (size_t)nVertLevels);

	/* Allocate another non-decomposed variable */
	pbl_mask = malloc(sizeof(int) * (size_t)nVertLevels);
	pbl_mask_valid = malloc(sizeof(int) * (size_t)nVertLevels);

	/* Read a variable with no dimensions */
	fprintf(test_log, "Read a variable with no dimensions: ");
	vers = (float)0.0;
	ierr = SMIOL_get_var(file, "version", NULL, &vers);
	if (ierr == SMIOL_SUCCESS) {
		/* Set validation array */
#ifdef SMIOL_PNETCDF
		vers_valid = (float)1.0;
#else
		vers_valid = (float)0.0;
#endif

		/* Check correctness of read data */
		if (vers == vers_valid) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - values read from the file are not correct\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Read a non-decomposed variable with no record dimension */
	fprintf(test_log, "Read a non-decomposed variable with no record dimension: ");
	memset((void *)coeffs, 0, sizeof(double) * (size_t)nVertLevels);
	ierr = SMIOL_get_var(file, "coeffs", NULL, coeffs);
	if (ierr == SMIOL_SUCCESS) {
		/* Set validation array */
#ifdef SMIOL_PNETCDF
		coeffs_valid[0] = 0.1;
		for (i = 1; i < (size_t)nVertLevels; i++) {
			coeffs_valid[i] = coeffs_valid[i-1] * 0.001;
		}
#else
		memset((void *)coeffs_valid, 0, sizeof(double) * (size_t)nVertLevels);
#endif

		/* Check correctness of read data */
		if (memcmp(coeffs, coeffs_valid, sizeof(double) * (size_t)nVertLevels) == 0) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - values read from the file are not correct\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Read a decomposed variable with no record dimension */
	fprintf(test_log, "Read a decomposed variable with no record dimension: ");
	memset((void *)id_string, 0, sizeof(char) * (size_t)strLen);
	ierr = SMIOL_get_var(file, "id_string", decomp2, id_string);
	if (ierr == SMIOL_SUCCESS) {
		/* Set validation array */
		memset((void *)id_string_valid, 0, sizeof(char) * (size_t)strLen);
#ifdef SMIOL_PNETCDF
		snprintf(id_string_valid, (size_t)strLen, "MPI task %3.3i", comm_rank);
#endif

		/* Check correctness of read data */
		if (memcmp(id_string, id_string_valid, sizeof(char) * (size_t)strLen) == 0) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - values read from the file are not correct\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	if (valid_comm_size) {
		/* Read frame 0 of a decomposed variable with a record dimension */
		fprintf(test_log, "Read frame 0 of a decomposed variable with a record dimension: ");
		memset((void *)foo, 0, sizeof(float) * n_compute_elements * (size_t)nVertLevels);
		ierr = SMIOL_get_var(file, "foo", decomp, foo);
		if (ierr == SMIOL_SUCCESS) {
			/* Set validation array */
#ifdef SMIOL_PNETCDF
			for (i = 0; i < n_compute_elements; i++) {
				for (j = 0; j < (size_t)nVertLevels; j++) {
					foo_valid[i*(size_t)nVertLevels + j] = (float)((size_t)(comm_rank) + (size_t)comm_size * i + j);
				}
			}
#else
			memset((void *)foo_valid, 0, sizeof(float) * n_compute_elements * (size_t)nVertLevels);
#endif

			/* Check correctness of read data */
			if (memcmp(foo, foo_valid, sizeof(float) * n_compute_elements * (size_t)nVertLevels) == 0) {
				fprintf(test_log, "PASS\n");
			} else {
				fprintf(test_log, "FAIL - values read from the file are not correct\n");
				errcount++;
			}
		} else {
			fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
			errcount++;
		}
	}

	/* Read frame 0 of a non-decomposed variable with a record dimension */
	fprintf(test_log, "Read frame 0 of a non-decomposed variable with a record dimension: ");
	memset((void *)pbl_mask, 0, sizeof(int) * (size_t)nVertLevels);
	ierr = SMIOL_get_var(file, "pbl_mask", NULL, pbl_mask);
	if (ierr == SMIOL_SUCCESS) {
		/* Set validation array */
#ifdef SMIOL_PNETCDF
		pbl_mask_valid[0] = 1;
		for (i = 1; i < (size_t)nVertLevels; i++) {
			pbl_mask_valid[i] = 0;
		}
#else
		memset((void *)pbl_mask_valid, 0, sizeof(int) * (size_t)nVertLevels);
#endif

		/* Check correctness of read data */
		if (memcmp(pbl_mask, pbl_mask_valid, sizeof(int) * (size_t)nVertLevels) == 0) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - values read from the file are not correct\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Read frame 0 of a 0-d variable with a record dimension */
	fprintf(test_log, "Read frame 0 of a 0-d variable with a record dimension: ");
	timestamp = 0.0;
	ierr = SMIOL_get_var(file, "seconds_since_epoch", NULL, &timestamp);
	if (ierr == SMIOL_SUCCESS) {
		/* Set validation array */
#ifdef SMIOL_PNETCDF
		timestamp_valid = 1594253412.75;
#else
		timestamp_valid = 0.0;
#endif

		/* Check correctness of read data */
		if (timestamp == timestamp_valid) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - values read from the file are not correct\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	ierr = SMIOL_set_frame(file, (SMIOL_Offset)1);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to advance frame in file...\n");
		return -1;
	}

	if (valid_comm_size) {
		/* Read frame 1 of a decomposed variable with a record dimension */
		fprintf(test_log, "Read frame 1 of a decomposed variable with a record dimension: ");
		memset((void *)foo, 0, sizeof(float) * n_compute_elements * (size_t)nVertLevels);
		ierr = SMIOL_get_var(file, "foo", decomp, foo);
		if (ierr == SMIOL_SUCCESS) {
			/* Update validation array from previous frame */
#ifdef SMIOL_PNETCDF
			for (i = 0; i < n_compute_elements; i++) {
				for (j = 0; j < (size_t)nVertLevels; j++) {
					foo_valid[i*(size_t)nVertLevels + j] *= (float)10.0;
				}
			}
#endif

			/* Check correctness of read data */
			if (memcmp(foo, foo_valid, sizeof(float) * n_compute_elements * (size_t)nVertLevels) == 0) {
				fprintf(test_log, "PASS\n");
			} else {
				fprintf(test_log, "FAIL - values read from the file are not correct\n");
				errcount++;
			}
		} else {
			fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
			errcount++;
		}
	}

	/* Read frame 1 of a non-decomposed variable with a record dimension */
	fprintf(test_log, "Read frame 1 of a non-decomposed variable with a record dimension: ");
	memset((void *)pbl_mask, 0, sizeof(int) * (size_t)nVertLevels);
	ierr = SMIOL_get_var(file, "pbl_mask", NULL, pbl_mask);
	if (ierr == SMIOL_SUCCESS) {
		/* Update validation array from previous frame */
#ifdef SMIOL_PNETCDF
		pbl_mask_valid[1] = 1;
		pbl_mask_valid[2] = 1;
		pbl_mask_valid[3] = 1;
#endif

		/* Check correctness of read data */
		if (memcmp(pbl_mask, pbl_mask_valid, sizeof(int) * (size_t)nVertLevels) == 0) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - values read from the file are not correct\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Read frame 1 of a 0-d variable with a record dimension */
	fprintf(test_log, "Read frame 1 of a 0-d variable with a record dimension: ");
	timestamp = 0.0;
	ierr = SMIOL_get_var(file, "seconds_since_epoch", NULL, &timestamp);
	if (ierr == SMIOL_SUCCESS) {
		/* Update validation array from previous frame */
#ifdef SMIOL_PNETCDF
		timestamp_valid = 1594253412.875;
#endif

		/* Check correctness of read data */
		if (timestamp == timestamp_valid) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - values read from the file are not correct\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	ierr = SMIOL_set_frame(file, (SMIOL_Offset)2);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to advance frame in file...\n");
		return -1;
	}

	if (valid_comm_size) {
		/* Read frame 2 of a decomposed variable with a record dimension */
		fprintf(test_log, "Read frame 2 of a decomposed variable with a record dimension: ");
		memset((void *)foo, 0, sizeof(float) * n_compute_elements * (size_t)nVertLevels);
		ierr = SMIOL_get_var(file, "foo", decomp, foo);
		if (ierr == SMIOL_SUCCESS) {
			/* Update validation array from previous frame */
#ifdef SMIOL_PNETCDF
			for (i = 0; i < n_compute_elements; i++) {
				for (j = 0; j < (size_t)nVertLevels; j++) {
					foo_valid[i*(size_t)nVertLevels + j] *= (float)-1.0;
				}
			}
#endif

			/* Check correctness of read data */
			if (memcmp(foo, foo_valid, sizeof(float) * n_compute_elements * (size_t)nVertLevels) == 0) {
				fprintf(test_log, "PASS\n");
			} else {
				fprintf(test_log, "FAIL - values read from the file are not correct\n");
				errcount++;
			}
		} else {
			fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
			errcount++;
		}
	}

	/* Read frame 2 of a non-decomposed variable with a record dimension */
	fprintf(test_log, "Read frame 2 of a non-decomposed variable with a record dimension: ");
	memset((void *)pbl_mask, 0, sizeof(int) * (size_t)nVertLevels);
	ierr = SMIOL_get_var(file, "pbl_mask", NULL, pbl_mask);
	if (ierr == SMIOL_SUCCESS) {
		/* Update validation array from previous frame */
#ifdef SMIOL_PNETCDF
		pbl_mask_valid[4] = 1;
		pbl_mask_valid[5] = 1;
		pbl_mask_valid[6] = 1;
#endif

		/* Check correctness of read data */
		if (memcmp(pbl_mask, pbl_mask_valid, sizeof(int) * (size_t)nVertLevels) == 0) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - values read from the file are not correct\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	/* Read frame 2 of a 0-d variable with a record dimension */
	fprintf(test_log, "Read frame 2 of a 0-d variable with a record dimension: ");
	timestamp = 0.0;
	ierr = SMIOL_get_var(file, "seconds_since_epoch", NULL, &timestamp);
	if (ierr == SMIOL_SUCCESS) {
		/* Update validation array from previous frame */
#ifdef SMIOL_PNETCDF
		timestamp_valid = 1594253413.0;
#endif

		/* Check correctness of read data */
		if (timestamp == timestamp_valid) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - values read from the file are not correct\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
		errcount++;
	}

	free(id_string);
	free(id_string_valid);
	free(foo);
	free(foo_valid);
	free(coeffs);
	free(coeffs_valid);
	free(pbl_mask);
	free(pbl_mask_valid);

	/* Close the SMIOL file */
	ierr = SMIOL_close_file(&file);
	if (ierr != SMIOL_SUCCESS || file != NULL) {
		fprintf(test_log, "Failed to close SMIOL file...\n");
		return -1;
	}

	if (!valid_comm_size) {
		fprintf(test_log, "<<< Some tests that require the number of MPI tasks to divide 120 were not run >>>\n");
	}

	ierr = SMIOL_free_decomp(&decomp);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to free decomp...\n");
		return -1;
	}

	ierr = SMIOL_free_decomp(&decomp2);
	if (ierr != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to free decomp2...\n");
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

int test_io_aggregation(FILE *test_log)
{
	int errcount;
	int ierr;
	size_t i, j;
	struct SMIOL_context *context;
	int comm_rank, comm_size;
	int num_io_tasks, io_stride;

	size_t n_compute_elements, n_total, offset;
	SMIOL_Offset *compute_elements = NULL;
	struct SMIOL_decomp *decomp_noagg = NULL, *decomp_agg2 = NULL, *decomp_agg0 = NULL;
	struct SMIOL_file *file = NULL;
	char **dimnames = NULL;
	float *theta1 = NULL, *theta2 = NULL;
	int all_equal;

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "********************** SMIOL I/O aggregation unit tests ************************\n");
	fprintf(test_log, "\n");

	errcount = 0;

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

	num_io_tasks = comm_size;
	io_stride = 1;

	/* Create a SMIOL context for testing aggregation */
	context = NULL;
	ierr = SMIOL_init(MPI_COMM_WORLD, num_io_tasks, io_stride, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	/*
	 * Define compute elements for all tasks
	 */
	n_compute_elements = (size_t)(10 + comm_rank);    /* Give each task a different number... why not? */

	/* Total number of compute elements across all tasks */
	n_total = (size_t)(10 * comm_size + (comm_size * (comm_size - 1)) / 2);

	/* Offset for contiguous range of elements computed on this task */
	offset = (size_t)(10 * comm_rank + (comm_rank * (comm_rank - 1)) / 2);

	compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);

	/* Tasks compute contiguous ranges of elements in reverse order */
	for (i = 0; i < n_compute_elements; i++) {
		compute_elements[i] = (SMIOL_Offset)(n_total - (offset + i) - 1);
	}

	/*
	 * Create three decompositions: one that does not use aggregation, one that uses
	 * an aggregation factor of two, and one that specifies an aggregation factor of 0
	 */
	if (SMIOL_create_decomp(context, n_compute_elements, compute_elements, 1, &decomp_noagg) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create a decomp with aggregation_factor=1\n");
		return -1;
	}

	if (SMIOL_create_decomp(context, n_compute_elements, compute_elements, 2, &decomp_agg2) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create a decomp with aggregation_factor=2\n");
		return -1;
	}

	if (SMIOL_create_decomp(context, n_compute_elements, compute_elements, 0, &decomp_agg0) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create a decomp with aggregation_factor=0\n");
		return -1;
	}

	/*
	 * Create a new file, to which we will write using all three of the decompositions from above
	 */
	file = NULL;
	ierr = SMIOL_open_file(context, "test_agg.nc", SMIOL_FILE_CREATE, &file, (size_t)64000000);
	if (ierr != SMIOL_SUCCESS || file == NULL) {
		fprintf(test_log, "Failed to create a file for testing aggregation\n");
		return -1;
	}

	if (SMIOL_define_dim(file, "nCells", (SMIOL_Offset)n_total) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create dimension nCells...\n");
		return -1;
	}

	if (SMIOL_define_dim(file, "nVertLevels", (SMIOL_Offset)55) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create dimension nVertLevels...\n");
		return -1;
	}

	/* The theta_noagg variable will be written with no aggregation and later read with aggregation */
	dimnames = (char **)malloc((size_t)2 * sizeof(char *));
	dimnames[0] = (char *)malloc((size_t)64 * sizeof(char));
	dimnames[1] = (char *)malloc((size_t)64 * sizeof(char));
	snprintf(dimnames[0], 64, "nCells");
	snprintf(dimnames[1], 64, "nVertLevels");

	if (SMIOL_define_var(file, "theta_noagg", SMIOL_REAL32, 2, (const char **)dimnames) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create theta_noagg var...\n");
		return -1;
	}

	/* The theta_agg2 variable will be written with aggregation=2 and later read without aggregation */
	if (SMIOL_define_var(file, "theta_agg2", SMIOL_REAL32, 2, (const char **)dimnames) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create theta_agg2 var...\n");
		return -1;
	}

	/* The theta_agg0 variable will be written with aggregation=0 and later read with aggregation=2 */
	if (SMIOL_define_var(file, "theta_agg0", SMIOL_REAL32, 2, (const char **)dimnames) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create theta_agg0 var...\n");
		return -1;
	}

	free(dimnames[0]);
	free(dimnames[1]);
	free(dimnames);

	/*
	 * Write a simple pattern to the theta field
	 */
	theta1 = malloc(sizeof(float) * (size_t)55 * n_compute_elements);

	for (j = 0; j < n_compute_elements; j++) {
		for (i = 0; i < 55; i++) {
			theta1[j * 55 + i] = (float)(55 * compute_elements[j] + (SMIOL_Offset)i);
		}
	}

	free(compute_elements);
	compute_elements = NULL;

	/* Writing a field with a no-aggregation decomp */
	fprintf(test_log, "Write a field with a decomp that does not use aggregation: ");
	ierr = SMIOL_put_var(file, "theta_noagg", decomp_noagg, theta1);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned by SMIOL_put_var\n");
		errcount++;
	}

	/* Writing a field with aggregation=2 decomp */
	fprintf(test_log, "Write a field with a decomp that uses aggregation factor 2: ");
	ierr = SMIOL_put_var(file, "theta_agg2", decomp_agg2, theta1);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned by SMIOL_put_var\n");
		errcount++;
	}

	/* Writing a field with aggregation=0 decomp */
	fprintf(test_log, "Write a field with a decomp that uses aggregation factor 0: ");
	ierr = SMIOL_put_var(file, "theta_agg0", decomp_agg0, theta1);
	if (ierr == SMIOL_SUCCESS) {
		fprintf(test_log, "PASS\n");
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned by SMIOL_put_var\n");
		errcount++;
	}

	if (SMIOL_close_file(&file) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to close file for aggregregation tests\n");
		return -1;
	}

	file = NULL;
	ierr = SMIOL_open_file(context, "test_agg.nc", SMIOL_FILE_READ, &file, (size_t)0);
	if (ierr != SMIOL_SUCCESS || file == NULL) {
		fprintf(test_log, "Failed to open file that was created for testing aggregation\n");
		return -1;
	}

	theta2 = malloc(sizeof(float) * (size_t)55 * n_compute_elements);

	/* Read a field that was written with aggregation=2 using a no-aggregation decomp */
	fprintf(test_log, "Read field written with aggregation=2 using no aggregation: ");
	for (i = 0; i < (size_t)55 * n_compute_elements; i++) {
		theta2[i] = (float)-1.0;
	}
	ierr = SMIOL_get_var(file, "theta_agg2", decomp_noagg, theta2);
	if (ierr == SMIOL_SUCCESS) {
		/* Compare with theta1, which still contains the correct field */
		all_equal = 1;
		for (i = 0; i < (size_t)55 * n_compute_elements; i++) {
			if (theta1[i] != theta2[i]) {
				all_equal = 0;
				break;
			}
		}

		if (all_equal) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - The field was read with incorrect values\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned by SMIOL_get_var\n");
		errcount++;
	}

	/* Read a field that was written with no aggregation using an aggregation=0 decomp */
	fprintf(test_log, "Read field written with no aggregation using aggregation=0: ");
	for (i = 0; i < (size_t)55 * n_compute_elements; i++) {
		theta2[i] = (float)-1.0;
	}
	ierr = SMIOL_get_var(file, "theta_noagg", decomp_agg0, theta2);
	if (ierr == SMIOL_SUCCESS) {
		/* Compare with theta1, which still contains the correct field */
		all_equal = 1;
		for (i = 0; i < (size_t)55 * n_compute_elements; i++) {
			if (theta1[i] != theta2[i]) {
				all_equal = 0;
				break;
			}
		}

		if (all_equal) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - The field was read with incorrect values\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned by SMIOL_get_var\n");
		errcount++;
	}

	/* Read a field that was written with aggregation=0 using an aggregation=2 decomp */
	fprintf(test_log, "Read field written with aggregation=0 using aggregation=2: ");
	for (i = 0; i < (size_t)55 * n_compute_elements; i++) {
		theta2[i] = (float)-1.0;
	}
	ierr = SMIOL_get_var(file, "theta_agg0", decomp_agg2, theta2);
	if (ierr == SMIOL_SUCCESS) {
		/* Compare with theta1, which still contains the correct field */
		all_equal = 1;
		for (i = 0; i < (size_t)55 * n_compute_elements; i++) {
			if (theta1[i] != theta2[i]) {
				all_equal = 0;
				break;
			}
		}

		if (all_equal) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - The field was read with incorrect values\n");
			errcount++;
		}
	} else {
		fprintf(test_log, "FAIL - SMIOL_SUCCESS was not returned by SMIOL_get_var\n");
		errcount++;
	}

	if (SMIOL_close_file(&file) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to close file for aggregregation tests\n");
		return -1;
	}

	free(theta1);
	free(theta2);


	if (SMIOL_free_decomp(&decomp_noagg) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to free decomp_noagg\n");
		return -1;
	}

	if (SMIOL_free_decomp(&decomp_agg2) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to free decomp_agg2\n");
		return -1;
	}

	if (SMIOL_free_decomp(&decomp_agg0) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to free decomp_agg0\n");
		return -1;
	}

	/* Free the SMIOL context */
	ierr = SMIOL_finalize(&context);
	if (ierr != SMIOL_SUCCESS || context != NULL) {
		fprintf(test_log, "Failed to free SMIOL context...\n");
		return -1;
	}

	fflush(test_log);
	fprintf(test_log, "\n");

	return errcount;
}

int test_buffered_io(FILE *test_log)
{
	int ierr;
	int errcount;
	int comm_rank, comm_size;
	int num_io_tasks, io_stride;
	struct SMIOL_context *context;
	struct SMIOL_file *file = NULL;
	char filename[64];
	size_t bufsize;
	char **dimnames = NULL;
	size_t i, j;
	size_t n_compute_elements;
	SMIOL_Offset *compute_elements = NULL;
	struct SMIOL_decomp *small_decomp = NULL, *medium_decomp = NULL, *large_decomp = NULL;
	int *small_var = NULL, *medium_var = NULL, *large_var = NULL;

	fprintf(test_log, "********************************************************************************\n");
	fprintf(test_log, "********************** SMIOL buffered I/O unit tests ***************************\n");
	fprintf(test_log, "\n");

	errcount = 0;

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

	if (comm_size > 1) {
		num_io_tasks = comm_size / 2;
	} else {
		num_io_tasks = 1;
	}
	io_stride = 2;

	/* Create a SMIOL context for testing buffered I/O */
	context = NULL;
	ierr = SMIOL_init(MPI_COMM_WORLD, num_io_tasks, io_stride, &context);
	if (ierr != SMIOL_SUCCESS || context == NULL) {
		fprintf(test_log, "Failed to create SMIOL context...\n");
		return -1;
	}

	n_compute_elements = 1ul;
	compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
	for (i = 0; i < n_compute_elements; i++) {
		compute_elements[i] = (SMIOL_Offset)i + (SMIOL_Offset)(n_compute_elements * (size_t)comm_rank);
	}
	if (SMIOL_create_decomp(context, n_compute_elements, compute_elements, 1, &small_decomp) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create small_decomp\n");
		return -1;
	}
	free(compute_elements);

	n_compute_elements = 2048ul;
	compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
	for (i = 0; i < n_compute_elements; i++) {
		compute_elements[i] = (SMIOL_Offset)i + (SMIOL_Offset)(n_compute_elements * (size_t)comm_rank);
	}
	if (SMIOL_create_decomp(context, n_compute_elements, compute_elements, 1, &medium_decomp) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create medium_decomp\n");
		return -1;
	}
	free(compute_elements);

	n_compute_elements = 6000000ul;
	compute_elements = malloc(sizeof(SMIOL_Offset) * n_compute_elements);
	for (i = 0; i < n_compute_elements; i++) {
		compute_elements[i] = (SMIOL_Offset)i + (SMIOL_Offset)(n_compute_elements * (size_t)comm_rank);
	}
	if (SMIOL_create_decomp(context, n_compute_elements, compute_elements, 1, &large_decomp) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to create large_decomp\n");
		return -1;
	}
	free(compute_elements);

	for (bufsize = 0ul; bufsize <= (size_t)(4*1024*1024); bufsize += (size_t)(4*1024*1024)) {
		snprintf(filename, 64, "buffered_write_%dMB.nc", (int)(bufsize / 1024ul / 1024ul));

		file = NULL;
		ierr = SMIOL_open_file(context, filename, SMIOL_FILE_CREATE, &file, bufsize);
		if (ierr != SMIOL_SUCCESS || file == NULL) {
			fprintf(test_log, "Failed to create a file for testing buffered I/O\n");
			return -1;
		}

		if ((ierr = SMIOL_define_dim(file, "Time", (SMIOL_Offset)-1)) != SMIOL_SUCCESS) {
			fprintf(test_log, "ERROR: SMIOL_define_dim: %s ", SMIOL_error_string(ierr));
			return 1;
		}

		if ((ierr = SMIOL_define_dim(file, "small_dim", (SMIOL_Offset)(comm_size))) != SMIOL_SUCCESS) {
			fprintf(test_log, "ERROR: SMIOL_define_dim: %s ", SMIOL_error_string(ierr));
			return 1;
		}

		if ((ierr = SMIOL_define_dim(file, "medium_dim", (SMIOL_Offset)(2048 * comm_size))) != SMIOL_SUCCESS) {
			fprintf(test_log, "ERROR: SMIOL_define_dim: %s ", SMIOL_error_string(ierr));
			return 1;
		}

		if ((ierr = SMIOL_define_dim(file, "large_dim", (SMIOL_Offset)(6000000 * comm_size))) != SMIOL_SUCCESS) {
			fprintf(test_log, "ERROR: SMIOL_define_dim: %s ", SMIOL_error_string(ierr));
			return 1;
		}

		dimnames = (char **)malloc((size_t)2 * sizeof(char *));
		dimnames[0] = (char *)malloc((size_t)64 * sizeof(char));
		dimnames[1] = (char *)malloc((size_t)64 * sizeof(char));

		snprintf(dimnames[0], 64, "Time");
		snprintf(dimnames[1], 64, "small_dim");

		if (SMIOL_define_var(file, "small_var", SMIOL_INT32, 2, (const char **)dimnames) != SMIOL_SUCCESS) {
			fprintf(test_log, "Failed to create small_var variable...\n");
			return -1;
		}

		snprintf(dimnames[0], 64, "Time");
		snprintf(dimnames[1], 64, "medium_dim");

		if (SMIOL_define_var(file, "medium_var", SMIOL_INT32, 2, (const char **)dimnames) != SMIOL_SUCCESS) {
			fprintf(test_log, "Failed to create medium_var variable...\n");
			return -1;
		}

		snprintf(dimnames[0], 64, "large_dim");

		if (SMIOL_define_var(file, "large_var", SMIOL_INT32, 1, (const char **)dimnames) != SMIOL_SUCCESS) {
			fprintf(test_log, "Failed to create large_var variable...\n");
			return -1;
		}

		free(dimnames[0]);
		free(dimnames[1]);
		free(dimnames);

		small_var = malloc(sizeof(int) * 1ul);
		medium_var = malloc(sizeof(int) * 2048ul);
		large_var = malloc(sizeof(int) * 6000000ul);

		for (i = 0; i < 1ul; i++) {
			small_var[i] = (int)i + 1 * comm_rank;
		}

		for (i = 0; i < 2048ul; i++) {
			medium_var[i] = (int)i + 2048 * comm_rank;
		}

		for (i = 0; i < 6000000ul; i++) {
			large_var[i] = (int)i + 6000000 * comm_rank;
		}

		/* Write a single small variable */
		fprintf(test_log, "Bufsize = %lu, write a single small variable: ", bufsize);
		ierr = SMIOL_put_var(file, "small_var", small_decomp, small_var);
		if (ierr == SMIOL_SUCCESS) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
			errcount++;
		}

		/* Write more variables than available pending requests */
		fprintf(test_log, "Bufsize = %lu, write more variables than available pending requests: ", bufsize);
		for (i = 0; i < 300ul; i++) {
			ierr = SMIOL_set_frame(file, (SMIOL_Offset)i);
			if (ierr != SMIOL_SUCCESS) {
				break;
			}

			ierr = SMIOL_put_var(file, "small_var", small_decomp, small_var);
			if (ierr != SMIOL_SUCCESS) {
				break;
			}
		}
		if (ierr == SMIOL_SUCCESS) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
			errcount++;
		}

		/* Synchronize a file with buffered writes */
		fprintf(test_log, "Synchronize a file with buffered writes: ");
		ierr = SMIOL_sync_file(file);
		if (ierr == SMIOL_SUCCESS) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
			errcount++;
		}

		/* Write variables to simultaneously exceed all requests and buffer space */
		fprintf(test_log, "Bufsize = %lu, write variables to simultaneously exceed all requests and buffer space: ", bufsize);
		for (i = 0; i < 257ul; i++) {
			ierr = SMIOL_set_frame(file, (SMIOL_Offset)i);
			if (ierr != SMIOL_SUCCESS) {
				break;
			}

			ierr = SMIOL_put_var(file, "medium_var", medium_decomp, medium_var);
			if (ierr != SMIOL_SUCCESS) {
				break;
			}
		}
		if (ierr == SMIOL_SUCCESS) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
			errcount++;
		}

		/* Write a single variable that should exceed buffer space */
		fprintf(test_log, "Bufsize = %lu, write a single variable that should exceed buffer space: ", bufsize);
		ierr = SMIOL_put_var(file, "large_var", large_decomp, large_var);
		if (ierr == SMIOL_SUCCESS) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL - (%s)\n", SMIOL_error_string(ierr));
			errcount++;
		}

		free(small_var);
		free(medium_var);
		free(large_var);

#ifdef SMIOL_PNETCDF
		/* Read back and verify large variable */
		fprintf(test_log, "Read back and verify large variable: ");
		large_var = malloc(sizeof(int) * 6000000ul * (size_t)comm_size);
		memset((void *)large_var, 0, sizeof(int) * 6000000ul * (size_t)comm_size);
		ierr = SMIOL_get_var(file, "large_var", NULL, large_var);
		if (ierr == SMIOL_SUCCESS) {
			for (i = 0; i < 6000000ul * (size_t)comm_size; i++) {
				if (large_var[i] != (int)i) {
					ierr = ~SMIOL_SUCCESS;
					break;
				}
			}
		}
		free(large_var);

		if (ierr == SMIOL_SUCCESS) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL\n");
			errcount++;
		}

		/* Read back and verify medium variable */
		fprintf(test_log, "Read back and verify medium variable: ");
		medium_var = malloc(sizeof(int) * 2048ul * (size_t)comm_size);
		for (j = 0; j < 257ul; j++) {
			ierr = SMIOL_set_frame(file, (SMIOL_Offset)j);
			if (ierr != SMIOL_SUCCESS) break;

			memset((void *)medium_var, 0, sizeof(int) * 2048ul * (size_t)comm_size);
			ierr = SMIOL_get_var(file, "medium_var", NULL, medium_var);
			if (ierr == SMIOL_SUCCESS) {
				for (i = 0; i < 2048ul * (size_t)comm_size; i++) {
					if (medium_var[i] != (int)i) {
						ierr = ~SMIOL_SUCCESS;
						break;
					}
				}
			}
			if (ierr != SMIOL_SUCCESS) break;
		}
		free(medium_var);

		if (ierr == SMIOL_SUCCESS) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL\n");
			errcount++;
		}

		/* Read back and verify small variable */
		fprintf(test_log, "Read back and verify small variable: ");
		small_var = malloc(sizeof(int) * (size_t)comm_size);
		for (j = 0; j < 300ul; j++) {
			ierr = SMIOL_set_frame(file, (SMIOL_Offset)j);
			if (ierr != SMIOL_SUCCESS) break;

			memset((void *)small_var, 0, sizeof(int) * (size_t)comm_size);
			ierr = SMIOL_get_var(file, "small_var", NULL, small_var);
			if (ierr == SMIOL_SUCCESS) {
				for (i = 0; i < (size_t)comm_size; i++) {
					if (small_var[i] != (int)i) {
						ierr = ~SMIOL_SUCCESS;
						break;
					}
				}
			}
			if (ierr != SMIOL_SUCCESS) break;
		}
		free(small_var);

		if (ierr == SMIOL_SUCCESS) {
			fprintf(test_log, "PASS\n");
		} else {
			fprintf(test_log, "FAIL\n");
			errcount++;
		}
#endif

		if (SMIOL_close_file(&file) != SMIOL_SUCCESS) {
			fprintf(test_log, "Failed to close file for buffered I/O tests\n");
			return -1;
		}
	}

	if (SMIOL_free_decomp(&small_decomp) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to free small_decomp\n");
		return -1;
	}

	if (SMIOL_free_decomp(&medium_decomp) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to free medium_decomp\n");
		return -1;
	}

	if (SMIOL_free_decomp(&large_decomp) != SMIOL_SUCCESS) {
		fprintf(test_log, "Failed to free large_decomp\n");
		return -1;
	}

	/* Free the SMIOL context */
	ierr = SMIOL_finalize(&context);
	if (ierr != SMIOL_SUCCESS || context != NULL) {
		fprintf(test_log, "Failed to free SMIOL context...\n");
		return -1;
	}

	fflush(test_log);
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


/********************************************************************************
 *
 * elements_covered
 *
 * Return the number of I/O elements that would be read/written by all I/O tasks
 *
 * Given the io_start and io_count values for all I/O tasks, compute the total
 * number of I/O elements that are read/written.
 *
 * If any of the I/O ranges overlap, a value of 0 is returned. In principle,
 * there is enough information available to determine exactly how many elements
 * will be read/written, and to return that value, but at present it is
 * considered an error to have overlapping ranges of I/O elements.
 *
 * If none of the I/O ranges overlap, this routine returns the sum of all
 * io_count values.
 *
 * Note: This routine has computation cost O(n^2), for n = comm_size.
 *
 ********************************************************************************/
size_t elements_covered(int comm_size, size_t *io_start, size_t *io_count)
{
	int i, j;
	size_t n_elems;

	/*
	 * Check that no ranges are overlapping
	 */
	for (i = 0; i < comm_size; i++) {
		for (j = 0; j < comm_size; j++) {
			if (j == i) {
				continue;
			}

			/* Does the range of task i intersect the range of task j? */
			if (range_intersection(io_start[i], io_count[i],
			                       io_start[j], io_count[j])) {
				return (size_t)0;
			}
		}
	}

	/*
	 * Get total number of I/O elements across all tasks
	 */
	n_elems = 0;
	for (i = 0; i < comm_size; i++) {
		n_elems += io_count[i];
	}

	return n_elems;
}


/********************************************************************************
 *
 * range_intersection
 *
 * Return 1 if two ranges given by (start,count) pairs overlap,
 * and 0 if they do not.
 *
 ********************************************************************************/
int range_intersection(size_t start1, size_t count1, size_t start2, size_t count2)
{
	size_t s1, e1, s2, e2;

	/* If either range has zero size, no intersection is possible */
	if (count1 == (size_t)0 || count2 == (size_t)0) {
		return (size_t)0;
	}

	s1 = start1;
	e1 = start1 + count1 - 1;

	s2 = start2;
	e2 = start2 + count2 - 1;

	/* Is the end of the first range within the second? */
	if (e1 >= s2 && e1 <= e2) {
		return 1;
	}

	/* Is the start of the first range within the second? */
	if (s1 >= s2 && s1 <= e2) {
		return 1;
	}

	/* Is the end of the second range within the first? */
	if (e2 >= s1 && e2 <= e1) {
		return 1;
	}

	/* Is the start of the second range within the first? */
	if (s2 >= s1 && s2 <= e1) {
		return 1;
	}

	return (size_t)0;
}
