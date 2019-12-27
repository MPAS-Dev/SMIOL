#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include "smiol.h"


/********************************************************************************
 *
 * SMIOL_fortran_init
 *
 * Initialize a SMIOL context from Fortran.
 *
 * This function is a simply a wrapper for the SMOIL_init routine that is intended
 * to be called from Fortran. Accordingly, the first argument is of type MPI_Fint
 * (a Fortran integer) rather than MPI_Comm.
 *
 ********************************************************************************/
int SMIOL_fortran_init(MPI_Fint comm, struct SMIOL_context **context)
{
	return SMIOL_init(MPI_Comm_f2c(comm), context);
}


/********************************************************************************
 *
 * SMIOL_init
 *
 * Initialize a SMIOL context.
 *
 * Initializes a SMIOL context, within which decompositions may be defined and
 * files may be read and written. At present, the only input argument is an MPI
 * communicator.
 *
 * Upon successful return the context argument points to a valid SMIOL context;
 * otherwise, it is NULL and an error code other than MPI_SUCCESS is returned.
 *
 * Note: It is assumed that MPI_Init has been called prior to this routine, so
 *       that any use of the provided MPI communicator will be valid.
 *
 ********************************************************************************/
int SMIOL_init(MPI_Comm comm, struct SMIOL_context **context)
{
	/*
	 * Before dereferencing context below, ensure that the pointer
	 * the context pointer is not NULL
	 */
	if (context == NULL) {
		return -999;    /* Should we define an error code for this? */
	}

	/*
	 * We cannot check for every possible invalid argument for comm, but
	 * at least we can verify that the communicator is not MPI_COMM_NULL
	 */
	if (comm == MPI_COMM_NULL) {
		/* Nullifying (*context) here may result in a memory leak, but this
		 * seems better than disobeying the stated behavior of returning
		 * a NULL context upon failure
		 */
		(*context) = NULL;

		return -999;    /* Should we define an error code for this? */
	}

	*context = (struct SMIOL_context *)malloc(sizeof(struct SMIOL_context));
	if ((*context) == NULL) {
		return SMIOL_MALLOC_FAILURE;
	}

	/* TODO: should we MPI_Comm_dup ? */
	(*context)->fcomm = MPI_Comm_c2f(comm);

	if (MPI_Comm_size(comm, &((*context)->comm_size)) != MPI_SUCCESS) {
		free((*context));
		(*context) = NULL;
		return -998;    /* Should we define an error code for this? */
	}

	if (MPI_Comm_rank(comm, &((*context)->comm_rank)) != MPI_SUCCESS) {
		free((*context));
		(*context) = NULL;
		return -998;    /* Should we define an error code for this? */
	}

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_finalize
 *
 * Finalize a SMIOL context.
 *
 * Finalizes a SMIOL context and frees all memory in the SMIOL_context instance.
 * After this routine is called, no other SMIOL routines that make reference to
 * the finalized context should be called.
 *
 ********************************************************************************/
int SMIOL_finalize(struct SMIOL_context **context)
{
	/*
	 * If the pointer to the context pointer is NULL, assume we have nothing
	 * to do and declare success
	 */
	if (context == NULL) {
		return SMIOL_SUCCESS;
	}

	free((*context));
	(*context) = NULL;

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_inquire
 *
 * Inquire about a SMIOL context.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_inquire(void)
{
	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_open_file
 *
 * Opens a file within a SMIOL context.
 *
 * Creates or opens the file specified by filename within the provided SMIOL
 * context.
 *
 * Upon successful completion, SMIOL_SUCCESS is returned, and the file handle argument
 * will point to a valid file handle. Otherwise, the file handle is NULL and an error
 * code other than SMIOL_SUCCESS is returned.
 *
 ********************************************************************************/
int SMIOL_open_file(struct SMIOL_context *context, const char *filename, struct SMIOL_file **file)
{
	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_close_file
 *
 * Closes a file within a SMIOL context.
 *
 * Closes the file associated with the provided file handle. Upon successful
 * completion, SMIOL_SUCCESS is returned, the file will be closed, and all memory
 * that is uniquely associated with the file handle will be deallocated.
 * Otherwise, an error code other than SMIOL_SUCCESS will be returned.
 *
 ********************************************************************************/
int SMIOL_close_file(struct SMIOL_file **file)
{
	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_define_dim
 *
 * Defines a new dimension in a file.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_define_dim(void)
{
	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_inquire_dim
 *
 * Inquires about an existing dimension in a file.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_inquire_dim(void)
{
	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_define_var
 *
 * Defines a new variable in a file.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_define_var(void)
{
	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_inquire_var
 *
 * Inquires about an existing variable in a file.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_inquire_var(void)
{
	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_put_var
 *
 * Writes a variable to a file.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_put_var(void)
{
	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_get_var
 *
 * Reads a variable from a file.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_get_var(void)
{
	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_define_att
 *
 * Defines a new attribute in a file.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_define_att(void)
{
	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_inquire_att
 *
 * Inquires about an attribute in a file.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_inquire_att(void)
{
	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_file_sync
 *
 * Forces all in-memory data to be flushed to disk.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_file_sync(void)
{
	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_error_string
 *
 * Returns an error string for a specified error code.
 *
 * Detailed description.
 *
 ********************************************************************************/
const char *SMIOL_error_string(int errno)
{
	switch (errno) {
	case SMIOL_SUCCESS:
		return "Success!";
	case SMIOL_MALLOC_FAILURE:
		return "malloc returned a null pointer";
	case SMIOL_INVALID_ARGUMENT:
		return "invalid subroutine argument";
	case SMIOL_MPI_ERROR:
		return "internal MPI call failed";
	case SMIOL_FORTRAN_ERROR:
		return "Fortran wrapper detected an inconsistency in C return values";
	default:
		return "Unknown error";
	}
}


/********************************************************************************
 *
 * SMIOL_set_option
 *
 * Sets an option for the SMIOL library.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_set_option(void)
{
	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_create_decomp
 *
 * Creates a mapping between compute elements and I/O elements.
 *
 * Detailed description.
 *
 ********************************************************************************/
struct SMIOL_decomp *SMIOL_create_decomp(size_t n_compute_elements,
		size_t n_io_elements, uint64_t *compute_elements,
		uint64_t *io_elements)
{
	struct SMIOL_decomp *d;
	size_t i;

	d = malloc(sizeof(struct SMIOL_decomp));
	d->n_compute_elements = n_compute_elements;
	d->n_io_elements = n_io_elements;

	d->compute_elements = malloc(sizeof(uint64_t) * d->n_compute_elements);
	if (d->compute_elements == NULL) {
		fprintf(stderr, "ERROR: Could not malloc space for d->compute_elements\n");
		return NULL;
	}

	d->io_elements = malloc(sizeof(uint64_t) * d->n_io_elements);
	if (d->io_elements == NULL) {
		fprintf(stderr, "ERROR: Could not malloc space for d->io_elements\n");
		return NULL;
	}

	// Copy compute elements
	for (i = 0; i < d->n_compute_elements; i++) {
		d->compute_elements[i] = compute_elements[i];
	}

	// Copy io elements
	for (i = 0; i < d->n_io_elements; i++) {
		d->io_elements[i] = io_elements[i];
	}

	return d;
}


/********************************************************************************
 *
 * SMIOL_free_decomp
 *
 * Frees a mapping between compute elements and I/O elements.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_free_decomp(struct SMIOL_decomp **d)
{
	if ((*d) == NULL) {
		return SMIOL_SUCCESS;
	}

	free((*d)->compute_elements);
	free((*d)->io_elements);
	free((*d));
	*d = NULL;

	return SMIOL_SUCCESS;
}
