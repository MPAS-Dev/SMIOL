/*******************************************************************************
 * SMIOL -- The Simple MPAS I/O Library
 *******************************************************************************/

#include <stdint.h>
#include "mpi.h"

/*
 * Types
 */
struct SMIOL_context {
	MPI_Fint fcomm; /* Fortran handle to MPI communicator */
	int comm_size;  /* Size of MPI communicator */
	int comm_rank;  /* Rank within MPI communicator */

	int lib_ierr;   /* Library-specific error code */
	int lib_type;   /* From which library the error code originated */
};

struct SMIOL_file {
	struct SMIOL_context *context; /* Context for this file */
#ifdef SMIOL_PNETCDF
	int ncidp; /* parallel-netCDF file handle */
#endif
};

struct SMIOL_decomp {
	size_t n_compute_elements;
	size_t n_io_elements;
	int64_t *compute_elements;
	int64_t *io_elements;
};

/*
 * Return error codes
 */
#include "smiol_codes.inc"

/*
 * Library methods
 */
int SMIOL_fortran_init(MPI_Fint comm, struct SMIOL_context **context);
int SMIOL_init(MPI_Comm comm, struct SMIOL_context **context);
int SMIOL_finalize(struct SMIOL_context **context);
int SMIOL_inquire(void);

/*
 * File methods
 */
int SMIOL_open_file(struct SMIOL_context *context, const char *filename, int mode, struct SMIOL_file **file);
int SMIOL_close_file(struct SMIOL_file **file);

/*
 * Dimension methods
 */
int SMIOL_define_dim(struct SMIOL_file *file, const char *dimname, int64_t dimsize);
int SMIOL_inquire_dim(void);

/*
 * Variable methods
 */
int SMIOL_define_var(void);
int SMIOL_inquire_var(void);
int SMIOL_put_var(void);
int SMIOL_get_var(void);

/*
 * Attribute methods
 */
int SMIOL_define_att(void);
int SMIOL_inquire_att(void);

/*
 * Control methods
 */
int SMIOL_file_sync(void);
const char *SMIOL_error_string(int errno);
const char *SMIOL_lib_error_string(struct SMIOL_context *context);
int SMIOL_set_option(void);

/*
 * Decomposition methods
 */
struct SMIOL_decomp *SMIOL_create_decomp(size_t n_compute_elements,
		size_t n_io_elements, int64_t *compute_elements,
		int64_t *io_elements);
int SMIOL_free_decomp(struct SMIOL_decomp **d);
