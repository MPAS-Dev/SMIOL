#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include "smiol.h"
#include "smiol_utils.h"

#ifdef SMIOL_PNETCDF
#include "pnetcdf.h"
#define PNETCDF_DEFINE_MODE 0
#define PNETCDF_DATA_MODE 1
#endif


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
	MPI_Comm smiol_comm;

	/*
	 * Before dereferencing context below, ensure that the pointer
	 * the context pointer is not NULL
	 */
	if (context == NULL) {
		return SMIOL_INVALID_ARGUMENT;
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

		return SMIOL_INVALID_ARGUMENT;
	}

	*context = (struct SMIOL_context *)malloc(sizeof(struct SMIOL_context));
	if ((*context) == NULL) {
		return SMIOL_MALLOC_FAILURE;
	}

	/*
	 * Initialize context
	 */
	(*context)->lib_ierr = 0;
	(*context)->lib_type = SMIOL_LIBRARY_UNKNOWN;

	/*
	 * Make a duplicate of the MPI communicator for use by SMIOL
	 */
	if (MPI_Comm_dup(comm, &smiol_comm) != MPI_SUCCESS) {
		free((*context));
		(*context) = NULL;
		return SMIOL_MPI_ERROR;
	}
	(*context)->fcomm = MPI_Comm_c2f(smiol_comm);

	if (MPI_Comm_size(smiol_comm, &((*context)->comm_size)) != MPI_SUCCESS) {
		free((*context));
		(*context) = NULL;
		return SMIOL_MPI_ERROR;
	}

	if (MPI_Comm_rank(smiol_comm, &((*context)->comm_rank)) != MPI_SUCCESS) {
		free((*context));
		(*context) = NULL;
		return SMIOL_MPI_ERROR;
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
	MPI_Comm smiol_comm;

	/*
	 * If the pointer to the context pointer is NULL, assume we have nothing
	 * to do and declare success
	 */
	if (context == NULL) {
		return SMIOL_SUCCESS;
	}

	if ((*context) == NULL) {
		return SMIOL_SUCCESS;
	}

	smiol_comm = MPI_Comm_f2c((*context)->fcomm);
	if (MPI_Comm_free(&smiol_comm) != MPI_SUCCESS) {
		free((*context));
		(*context) = NULL;
		return SMIOL_MPI_ERROR;
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
 * Depending on the specified file mode, creates or opens the file specified
 * by filename within the provided SMIOL context.
 *
 * Upon successful completion, SMIOL_SUCCESS is returned, and the file handle argument
 * will point to a valid file handle. Otherwise, the file handle is NULL and an error
 * code other than SMIOL_SUCCESS is returned.
 *
 ********************************************************************************/
int SMIOL_open_file(struct SMIOL_context *context, const char *filename, int mode, struct SMIOL_file **file)
{
#ifdef SMIOL_PNETCDF
	int ierr;
#endif

	/*
	 * Before dereferencing file below, ensure that the pointer
	 * the file pointer is not NULL
	 */
	if (file == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Check that context is valid
	 */
	if (context == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	*file = (struct SMIOL_file *)malloc(sizeof(struct SMIOL_file));
	if ((*file) == NULL) {
		return SMIOL_MALLOC_FAILURE;
	}

	/*
	 * Save pointer to context for this file
	 */
	(*file)->context = context;

	if (mode & SMIOL_FILE_CREATE) {
#ifdef SMIOL_PNETCDF
		if ((ierr = ncmpi_create(MPI_Comm_f2c(context->fcomm), filename,
					(NC_64BIT_DATA | NC_CLOBBER), MPI_INFO_NULL,
					&((*file)->ncidp))) != NC_NOERR) {
			free((*file));
			(*file) = NULL;
			context->lib_type = SMIOL_LIBRARY_PNETCDF;
			context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		} else {
			(*file)->state = PNETCDF_DEFINE_MODE;
		}
#endif
	}
	else if (mode & SMIOL_FILE_WRITE) {
#ifdef SMIOL_PNETCDF
		if ((ierr = ncmpi_open(MPI_Comm_f2c(context->fcomm), filename,
				NC_WRITE, MPI_INFO_NULL, &((*file)->ncidp))) != NC_NOERR) {
			free((*file));
			(*file) = NULL;
			context->lib_type = SMIOL_LIBRARY_PNETCDF;
			context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		} else {
			(*file)->state = PNETCDF_DATA_MODE;
		}
#endif
	}
	else if (mode & SMIOL_FILE_READ) {
#ifdef SMIOL_PNETCDF
		if ((ierr = ncmpi_open(MPI_Comm_f2c(context->fcomm), filename,
				NC_NOWRITE, MPI_INFO_NULL, &((*file)->ncidp))) != NC_NOERR) {
			free((*file));
			(*file) = NULL;
			context->lib_type = SMIOL_LIBRARY_PNETCDF;
			context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		} else {
			(*file)->state = PNETCDF_DATA_MODE;
		}
#endif
	}
	else {
		free((*file));
		(*file) = NULL;
		return SMIOL_INVALID_ARGUMENT;
	}

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
#ifdef SMIOL_PNETCDF
	int ierr;
#endif

	/*
	 * If the pointer to the file pointer is NULL, assume we have nothing
	 * to do and declare success
	 */
	if (file == NULL) {
		return SMIOL_SUCCESS;
	}

#ifdef SMIOL_PNETCDF
	if ((ierr = ncmpi_close((*file)->ncidp)) != NC_NOERR) {
		((*file)->context)->lib_type = SMIOL_LIBRARY_PNETCDF;
		((*file)->context)->lib_ierr = ierr;
		free((*file));
		(*file) = NULL;
		return SMIOL_LIBRARY_ERROR;
	}
#endif

	free((*file));
	(*file) = NULL;

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_define_dim
 *
 * Defines a new dimension in a file.
 *
 * Defines a dimension with the specified name and size in the file associated
 * with the file handle. If a negative value is provided for the size argument,
 * the dimension will be defined as an unlimited or record dimension.
 *
 * Upon successful completion, SMIOL_SUCCESS is returned; otherwise, an error
 * code is returned.
 *
 ********************************************************************************/
int SMIOL_define_dim(struct SMIOL_file *file, const char *dimname, SMIOL_Offset dimsize)
{
#ifdef SMIOL_PNETCDF
	int dimidp;
	int ierr;
	MPI_Offset len;
#endif

	/*
	 * Check that file handle is valid
	 */
	if (file == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Check that dimension name is valid
	 */
	if (dimname == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

#ifdef SMIOL_PNETCDF
	/*
	 * The parallel-netCDF library does not permit zero-length dimensions
	 */
	if (dimsize == (SMIOL_Offset)0) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Handle unlimited / record dimension specifications
	 */
	if (dimsize < (SMIOL_Offset)0) {
		len = NC_UNLIMITED;
	}
	else {
		len = (MPI_Offset)dimsize;
	}

	/*
	 * If the file is in data mode, then switch it to define mode
	 */
	if (file->state == PNETCDF_DATA_MODE) {
		if ((ierr = ncmpi_redef(file->ncidp)) != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}
		file->state = PNETCDF_DEFINE_MODE;
	}

	if ((ierr = ncmpi_def_dim(file->ncidp, dimname, len, &dimidp)) != NC_NOERR) {
		file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
		file->context->lib_ierr = ierr;
		return SMIOL_LIBRARY_ERROR;
	}
#endif

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_inquire_dim
 *
 * Inquires about an existing dimension in a file.
 *
 * Inquires about the size of an existing dimension in a file. For record
 * dimensions, the current size of the dimension is returned; future writes of
 * additional records to a file can lead to different return sizes for record
 * dimensions.
 *
 * Upon successful completion, SMIOL_SUCCESS is returned; otherwise, an error
 * code is returned.
 *
 ********************************************************************************/
int SMIOL_inquire_dim(struct SMIOL_file *file, const char *dimname, SMIOL_Offset *dimsize)
{
#ifdef SMIOL_PNETCDF
	int dimidp;
	int ierr;
	MPI_Offset len;
#endif
	/*
	 * Check that file handle is valid
	 */
	if (file == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Check that dimension name is valid
	 */
	if (dimname == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Check that dimension size is not NULL
	 */
	if (dimsize == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	(*dimsize) = (int64_t)0;   /* Default dimension size if no library provides a value */

#ifdef SMIOL_PNETCDF
	if ((ierr = ncmpi_inq_dimid(file->ncidp, dimname, &dimidp)) != NC_NOERR) {
		(*dimsize) = (SMIOL_Offset)(-1);  /* TODO: should there be a well-defined invalid size? */
		file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
		file->context->lib_ierr = ierr;
		return SMIOL_LIBRARY_ERROR;
	}

	if ((ierr = ncmpi_inq_dimlen(file->ncidp, dimidp, &len)) != NC_NOERR) {
		(*dimsize) = (SMIOL_Offset)(-1);  /* TODO: should there be a well-defined invalid size? */
		file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
		file->context->lib_ierr = ierr;
		return SMIOL_LIBRARY_ERROR;
	}

	(*dimsize) = (SMIOL_Offset)len;
#endif

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_define_var
 *
 * Defines a new variable in a file.
 *
 * Defines a variable with the specified name, type, and dimensions in an open
 * file pointed to by the file argument. The varname and dimnames arguments
 * are expected to be null-terminated strings, except if the variable has zero
 * dimensions, in which case the dimnames argument may be a NULL pointer.
 *
 * Upon successful completion, SMIOL_SUCCESS is returned; otherwise, an error
 * code is returned.
 *
 ********************************************************************************/
int SMIOL_define_var(struct SMIOL_file *file, const char *varname, int vartype, int ndims, const char **dimnames)
{
#ifdef SMIOL_PNETCDF
	int *dimids;
	int ierr;
	int i;
	nc_type xtype;
	int varidp;
#endif

	/*
	 * Check that file handle is valid
	 */
	if (file == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Check that variable name is valid
	 */
	if (varname == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Check that the variable type is valid - handled below in a library-specific way...
	 */

	/*
	 * Check that variable dimension names are valid
	 */
	if (dimnames == NULL && ndims > 0) {
		return SMIOL_INVALID_ARGUMENT;
	}

#ifdef SMIOL_PNETCDF
	dimids = (int *)malloc(sizeof(int) * (size_t)ndims);
	if (dimids == NULL) {
		return SMIOL_MALLOC_FAILURE;
	}

	/*
	 * Build a list of dimension IDs
	 */
	for (i=0; i<ndims; i++) {
		if ((ierr = ncmpi_inq_dimid(file->ncidp, dimnames[i], &dimids[i])) != NC_NOERR) {
			free(dimids);
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}
	}

	/*
	 * Translate SMIOL variable type to parallel-netcdf type
	 */
	switch (vartype) {
		case SMIOL_REAL32:
			xtype = NC_FLOAT;
			break;
		case SMIOL_REAL64:
			xtype = NC_DOUBLE;
			break;
		case SMIOL_INT32:
			xtype = NC_INT;
			break;
		case SMIOL_CHAR:
			xtype = NC_CHAR;
			break;
		default:
			free(dimids);
			return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * If the file is in data mode, then switch it to define mode
	 */
	if (file->state == PNETCDF_DATA_MODE) {
		if ((ierr = ncmpi_redef(file->ncidp)) != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}
		file->state = PNETCDF_DEFINE_MODE;
	}

	/*
	 * Define the variable
	 */
	if ((ierr = ncmpi_def_var(file->ncidp, varname, xtype, ndims, dimids, &varidp)) != NC_NOERR) {
		free(dimids);
		file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
		file->context->lib_ierr = ierr;
		return SMIOL_LIBRARY_ERROR;
	}

	free(dimids);
#endif

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_inquire_var
 *
 * Inquires about an existing variable in a file.
 *
 * Inquires about a variable in a file, and optionally returns the type
 * of the variable, the dimensionality of the variable, and the names of
 * the dimensions of the variable. Which properties of the variable to return
 * (type, dimensionality, or dimension names) is indicated by the status of
 * the pointers for the corresponding properties: if the pointer is a non-NULL
 * pointer, the property will be set upon successful completion of this routine.
 *
 * If the names of a variable's dimensions are requested (by providing a non-NULL
 * actual argument for dimnames), the size of the dimnames array must be at least
 * the number of dimensions in the variable, and each character string pointed
 * to by an element of dimnames must be large enough to accommodate the corresponding
 * dimension name.
 *
 ********************************************************************************/
int SMIOL_inquire_var(struct SMIOL_file *file, const char *varname, int *vartype, int *ndims, char **dimnames)
{
#ifdef SMIOL_PNETCDF
	int *dimids;
	int varidp;
	int ierr;
	int i;
	int xtypep;
	int ndimsp;
#endif

	/*
	 * Check that file handle is valid
	 */
	if (file == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Check that variable name is valid
	 */
	if (varname == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * If all output arguments are NULL, we can return early
	 */
	if (vartype == NULL && ndims == NULL && dimnames == NULL) {
		return SMIOL_SUCCESS;
	}

#ifdef SMIOL_PNETCDF
	/*
	 * Get variable ID
	 */
	if ((ierr = ncmpi_inq_varid(file->ncidp, varname, &varidp)) != NC_NOERR) {
		file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
		file->context->lib_ierr = ierr;
		return SMIOL_LIBRARY_ERROR;
	}

	/*
	 * If requested, inquire about variable type
	 */
	if (vartype != NULL) {
		if ((ierr = ncmpi_inq_vartype(file->ncidp, varidp, &xtypep)) != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}

		/* Convert parallel-netCDF variable type to SMIOL variable type */
		switch (xtypep) {
			case NC_FLOAT:
				*vartype = SMIOL_REAL32;
				break;
			case NC_DOUBLE:
				*vartype = SMIOL_REAL64;
				break;
			case NC_INT:
				*vartype = SMIOL_INT32;
				break;
			case NC_CHAR:
				*vartype = SMIOL_CHAR;
				break;
			default:
				*vartype = SMIOL_UNKNOWN_VAR_TYPE;
		}
	}

	/*
	 * All remaining properties will require the number of dimensions
	 */
	if (ndims != NULL || dimnames != NULL) {
		if ((ierr = ncmpi_inq_varndims(file->ncidp, varidp, &ndimsp)) != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}
	}

	/*
	 * If requested, inquire about dimensionality
	 */
	if (ndims != NULL) {
		*ndims = ndimsp;
	}

	/*
	 * If requested, inquire about dimension names
	 */
	if (dimnames != NULL) {
		dimids = (int *)malloc(sizeof(int) * (size_t)ndimsp);
		if (dimids == NULL) {
			return SMIOL_MALLOC_FAILURE;
		}

		if ((ierr = ncmpi_inq_vardimid(file->ncidp, varidp, dimids)) != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			free(dimids);
			return SMIOL_LIBRARY_ERROR;
		}

		for (i = 0; i < ndimsp; i++) {
			if (dimnames[i] == NULL) {
				return SMIOL_INVALID_ARGUMENT;
			}
			if ((ierr = ncmpi_inq_dimname(file->ncidp, dimids[i], dimnames[i])) != NC_NOERR) {
				file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
				file->context->lib_ierr = ierr;
				free(dimids);
				return SMIOL_LIBRARY_ERROR;
			}
		}

		free(dimids);
	}
#endif

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
 * SMIOL_sync_file
 *
 * Forces all in-memory data to be flushed to disk.
 *
 * Upon success, all in-memory data for the file associatd with the file
 * handle will be flushed to the file system and SMIOL_SUCCESS will be
 * returned; otherwise, an error code is returned.
 *
 ********************************************************************************/
int SMIOL_sync_file(struct SMIOL_file *file)
{
#ifdef SMIOL_PNETCDF
	int ierr;
#endif

	/*
	 * Check that file is valid
	 */
	if (file == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

#ifdef SMIOL_PNETCDF
	/*
	 * If the file is in define mode then switch it into data mode
	 */
	if (file->state == PNETCDF_DEFINE_MODE) {
		if ((ierr = ncmpi_enddef(file->ncidp)) != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}
		file->state = PNETCDF_DATA_MODE;
	}

	if ((ierr = ncmpi_sync(file->ncidp)) != NC_NOERR) {
		file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
		file->context->lib_ierr = ierr;
		return SMIOL_LIBRARY_ERROR;
	}
#endif

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_error_string
 *
 * Returns an error string for a specified error code.
 *
 * Returns an error string corresponding to a SMIOL error code. If the error code is
 * SMIOL_LIBRARY_ERROR and a valid SMIOL context is available, the SMIOL_lib_error_string
 * function should be called instead. The error string is null-terminated, but it
 * does not contain a newline character.
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
	case SMIOL_LIBRARY_ERROR:
		return "bad return code from a library call";
	default:
		return "Unknown error";
	}
}


/********************************************************************************
 *
 * SMIOL_lib_error_string
 *
 * Returns an error string for a third-party library called by SMIOL.
 *
 * Returns an error string corresponding to an error that was generated by
 * a third-party library that was called by SMIOL. The library that was the source
 * of the error, as well as the library-specific error code, are retrieved from
 * a SMIOL context. If successive library calls resulted in errors, only the error
 * string for the last of these errors will be returned. The error string is
 * null-terminated, but it does not contain a newline character.
 *
 ********************************************************************************/
const char *SMIOL_lib_error_string(struct SMIOL_context *context)
{
	if (context == NULL) {
		return "SMIOL_context argument is a NULL pointer";
	}

	switch (context->lib_type) {
#ifdef SMIOL_PNETCDF
	case SMIOL_LIBRARY_PNETCDF:
		return ncmpi_strerror(context->lib_ierr);
#endif
	default:
		return "Could not find matching library for the source of the error";
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


/*******************************************************************************
 *
 * SMIOL_create_decomp
 *
 * Creates a mapping between compute elements and I/O elements.
 *
 * Given arrays of global element IDs that each task computes and global element
 * IDs that each task reads/writes, this routine works out a mapping of elements
 * between compute and I/O tasks.
 *
 * If all input arguments are determined to be valid and if the routine is
 * successful in working out a mapping, the decomp pointer is allocated and
 * given valid contents, and SMIOL_SUCCESS is returned; otherwise a non-success
 * error code is returned and the decomp pointer is NULL.
 *
 *******************************************************************************/
int SMIOL_create_decomp(struct SMIOL_context *context,
                        size_t n_compute_elements, SMIOL_Offset *compute_elements,
                        size_t n_io_elements, SMIOL_Offset *io_elements,
                        struct SMIOL_decomp **decomp)
{
	MPI_Comm comm;
	int comm_size;
	int comm_rank;
	int ierr;
	int i, j;
	int count;
	int nbuf_in, nbuf_out;
	SMIOL_Offset *compute_ids;
	SMIOL_Offset *io_ids;
	SMIOL_Offset *buf_in, *buf_out;
	SMIOL_Offset *io_list, *comp_list;
	SMIOL_Offset neighbor;
	MPI_Request req_in, req_out;
	size_t ii;
	size_t idx;
	size_t n_neighbors;
	size_t n_xfer;
	size_t n_xfer_total;
	size_t n_list;

	const SMIOL_Offset UNKNOWN_TASK = (SMIOL_Offset)(-1);


	if (context == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	if (compute_elements == NULL && n_compute_elements != 0) {
		return SMIOL_INVALID_ARGUMENT;
	}

	if (io_elements == NULL && n_io_elements != 0) {
		return SMIOL_INVALID_ARGUMENT;
	}


	comm = MPI_Comm_f2c(context->fcomm);
	comm_size = context->comm_size;
	comm_rank = context->comm_rank;


	/*
	 * Because the count argument to MPI_Isend and MPI_Irecv is an int, at
	 * most 2^31-1 elements can be transmitted at a time. In this routine,
	 * arrays of pairs of SMIOL_Offset values will be transmitted as arrays
	 * of bytes, so n_compute_elements and n_io_elements can be at most
	 * 2^31-1 / sizeof(SMIOL_Offset) / 2.
	 */
	i = 0;
	if (n_compute_elements > (((size_t)1 << 31) - 1)
	                         / sizeof(SMIOL_Offset)
	                         / (size_t)2) {
		i = 1;
	}
	if (n_io_elements > (((size_t)1 << 31) - 1)
	                    / sizeof(SMIOL_Offset)
	                    / (size_t)2) {
		i = 1;
	}

	ierr = MPI_Allreduce((const void *)&i, (void *)&j, 1, MPI_INT, MPI_MAX,
	                     comm);
	if (j > 0) {
		return SMIOL_INVALID_ARGUMENT;
	} else if (ierr != MPI_SUCCESS) {
		return SMIOL_MPI_ERROR;
	}


	/*
	 * Allocate an array, compute_ids, with three entries for each compute
	 * element
	 *    [0] - element global ID
	 *    [1] - element local ID
	 *    [2] - I/O task that reads/writes this element
	 */
	compute_ids = (SMIOL_Offset *)malloc(sizeof(SMIOL_Offset) * TRIPLET_SIZE
	                                     * n_compute_elements);
	if (compute_ids == NULL) {
		return SMIOL_MALLOC_FAILURE;
	}

	/*
	 * Fill in compute_ids array with global and local IDs; rank of I/O task
	 * is not yet known
	 */
	for (ii = 0; ii < n_compute_elements; ii++) {
		compute_ids[TRIPLET_SIZE*ii] = compute_elements[ii]; /* global ID */
		compute_ids[TRIPLET_SIZE*ii+1] = (SMIOL_Offset)ii;   /* local ID */
		compute_ids[TRIPLET_SIZE*ii+2] = UNKNOWN_TASK;       /* I/O task rank */
	}

	/*
	 * Sort the compute_ids array on global element ID
	 * (first entry for each element)
	 */
	sort_triplet_array(n_compute_elements, compute_ids, 0);

	/*
	 * Allocate buffer with two entries for each I/O element
	 *    [0] - I/O element global ID
	 *    [1] - task that computes this element
	 */
	nbuf_out = (int)n_io_elements;
	buf_out = (SMIOL_Offset *)malloc(sizeof(SMIOL_Offset) * (size_t)2
	                                 * (size_t)nbuf_out);
	if (buf_out == NULL) {
		return SMIOL_MALLOC_FAILURE;
	}

	/*
	 * Fill buffer with I/O element IDs; compute task is not yet known
	 */
	for (ii = 0; ii < n_io_elements; ii++) {
		buf_out[2*ii] = io_elements[ii];
		buf_out[2*ii+1] = UNKNOWN_TASK;
	}

	/*
	 * Iterate through all ranks in the communicator, receiving from "left"
	 * neighbor and sending to "right" neighbor in each iteration.
	 * The objective is to identify, for each I/O element, which MPI rank
	 * computes that element. At the end of iteration, each rank will have
	 * seen the I/O element list from all other ranks.
	 */
	for (i = 0; i < comm_size; i++) {
		/*
		 * Compute the rank whose buffer will be received this iteration
		 */
		SMIOL_Offset src_rank = (comm_rank - 1 - i + comm_size)
		                        % comm_size;

		/*
		 * Initiate send of outgoing buffer size and receive of incoming
		 * buffer size
		 */
		ierr = MPI_Irecv((void *)&nbuf_in, 1, MPI_INT,
		                 (comm_rank - 1 + comm_size) % comm_size,
		                 (comm_rank + i), comm, &req_in);

		ierr = MPI_Isend((const void *)&nbuf_out, 1, MPI_INT,
		                 (comm_rank + 1) % comm_size,
		                 ((comm_rank + 1) % comm_size + i), comm,
		                 &req_out);

		/*
		 * Wait until the incoming buffer size has been received
		 */
		ierr = MPI_Wait(&req_in, MPI_STATUS_IGNORE);

		/*
		 * Allocate incoming buffer
		 */
		buf_in = (SMIOL_Offset *)malloc(sizeof(SMIOL_Offset) * (size_t)2
		                                * (size_t)nbuf_in);

		/*
		 * Initiate receive of incoming buffer
		 */
		count = 2 * nbuf_in;
		count *= (int)sizeof(SMIOL_Offset);
		ierr = MPI_Irecv((void *)buf_in, count, MPI_BYTE,
		                 (comm_rank - 1 + comm_size) % comm_size,
		                 (comm_rank + i), comm, &req_in);

		/*
		 * Wait until the outgoing buffer size has been sent
		 */
		ierr = MPI_Wait(&req_out, MPI_STATUS_IGNORE);

		/*
		 * Initiate send of outgoing buffer
		 */
		count = 2 * nbuf_out;
		count *= (int)sizeof(SMIOL_Offset);
		ierr = MPI_Isend((const void *)buf_out, count, MPI_BYTE,
		                 (comm_rank + 1) % comm_size,
		                 ((comm_rank + 1) % comm_size + i), comm,
		                 &req_out);

		/*
		 * Wait until the incoming buffer has been received
		 */
		ierr = MPI_Wait(&req_in, MPI_STATUS_IGNORE);

		/*
		 * Loop through the incoming buffer, marking all elements that
		 * are computed on this task
		 */
		for (j = 0; j < nbuf_in; j++) {
			/*
			 * If I/O element does not yet have a computing task...
			 */
			if (buf_in[2*j+1] == UNKNOWN_TASK) {
				SMIOL_Offset *elem;

				/*
				 * and if this element is computed on this task...
				 */
				elem = search_triplet_array(buf_in[2*j],
				                            n_compute_elements,
				                            compute_ids, 0);
				if (elem != NULL) {
					/*
					 * then mark the element as being
					 * computed on this task
					 */
					buf_in[2*j+1] = (SMIOL_Offset)comm_rank;

					/*
					 * and note locally which task will
					 * read/write this element
					 */
					elem[2] = src_rank;
				}
			}
		}

		/*
		 * Wait until we have sent the outgoing buffer
		 */
		ierr = MPI_Wait(&req_out, MPI_STATUS_IGNORE);

		/*
		 * Free outgoing buffer and make the input buffer into
		 * the output buffer for next iteration
		 */
		free(buf_out);
		buf_out = buf_in;
		nbuf_out = nbuf_in;
	}

	/*
	 * The output buffer is now the initial buffer with the compute tasks
	 * for each I/O element identified
	 */

	/*
	 * Allocate an array, io_ids, with three entries for each I/O element
	 *    [0] - element global ID
	 *    [1] - element local ID
	 *    [2] - compute task that operates on this element
	 */
	io_ids = (SMIOL_Offset *)malloc(sizeof(SMIOL_Offset) * TRIPLET_SIZE
	                                * n_io_elements);
	if (io_ids == NULL) {
		return SMIOL_MALLOC_FAILURE;
	}

	/*
	 * Fill in io_ids array with global and local IDs, plus the rank of
	 * the task that computes each element
	 */
	for (ii = 0; ii < n_io_elements; ii++) {
		io_ids[TRIPLET_SIZE*ii] = buf_out[2*ii+0];    /* global ID */
		io_ids[TRIPLET_SIZE*ii+1] = (SMIOL_Offset)ii; /* local ID */
		io_ids[TRIPLET_SIZE*ii+2] = buf_out[2*ii+1];  /* computing task rank */
	}

	free(buf_out);

	/*
	 * Sort io_ids array on task ID (third entry for each element)
	 */
	sort_triplet_array(n_io_elements, io_ids, 2);

	*decomp = (struct SMIOL_decomp *)malloc(sizeof(struct SMIOL_decomp));
	if ((*decomp) == NULL) {
		return SMIOL_MALLOC_FAILURE;
	}

	(*decomp)->context = context;


	/*
	 * Scan through io_ids to determine number of unique neighbors that
	 * compute elements read/written on this task, and also determine
	 * the total number of elements
	 * computed on other tasks that are read/written on this task
	 */
	ii = 0;
	n_neighbors = 0;
	n_xfer_total = 0;
	while (ii < n_io_elements) {
		/* Task that computes this element */
		neighbor = io_ids[TRIPLET_SIZE*ii + 2];

		/* Number of elements to read/write for neighbor */
		n_xfer = 0;

		/*
		 * Since io_ids is sorted on task, as long as task is unchanged,
		 * increment n_xfer
		 */
		while (ii < n_io_elements
		       && io_ids[TRIPLET_SIZE*ii+2] == neighbor) {
			n_xfer++;
			ii++;
		}
		if (neighbor != UNKNOWN_TASK) {
			n_neighbors++;
			n_xfer_total += n_xfer;
		}
	}

	/*
	 * Based on number of neighbors and total number of elements to transfer
	 * allocate the io_list
	 */
	n_list = sizeof(SMIOL_Offset) * ((size_t)1
	                                 + (size_t)2 * n_neighbors
	                                 + n_xfer_total);
	(*decomp)->io_list = (SMIOL_Offset *)malloc(n_list);
	if ((*decomp)->io_list == NULL) {
		return SMIOL_MALLOC_FAILURE;
	}
	io_list = (*decomp)->io_list;

	/*
	 * Scan through io_ids a second time, filling in the io_list
	 */
	io_list[0] = (SMIOL_Offset)n_neighbors;
	idx = 1; /* Index in io_list where neighbor ID will be written, followed
	            by number of elements and element local IDs */

	ii = 0;
	while (ii < n_io_elements) {
		/* Task that computes this element */
		neighbor = io_ids[TRIPLET_SIZE*ii + 2];

		/* Number of elements to read/write for neighbor */
		n_xfer = 0;

		/*
		 * Since io_ids is sorted on task, as long as task is unchanged,
		 * increment n_xfer
		 */
		while (ii < n_io_elements
		       && io_ids[TRIPLET_SIZE*ii+2] == neighbor) {
			if (neighbor != UNKNOWN_TASK) {
				/* Save local element ID in list */
				io_list[idx+2+n_xfer] = io_ids[TRIPLET_SIZE*ii+1];
				n_xfer++;
			}
			ii++;
		}
		if (neighbor != UNKNOWN_TASK) {
			io_list[idx] = neighbor;
			io_list[idx+1] = (SMIOL_Offset)n_xfer;
			idx += (2 + n_xfer);
		}
	}

	free(io_ids);

	/*
	 * Sort compute_ids array on task ID (third entry for each element)
	 */
	sort_triplet_array(n_compute_elements, compute_ids, 2);

	/*
	 * Scan through compute_ids to determine number of unique neighbors that
	 * read/write elements computed on this task, and also determine
	 * the total number of elements read/written on other tasks that are
	 * computed on this task
	 */
	ii = 0;
	n_neighbors = 0;
	n_xfer_total = 0;
	while (ii < n_compute_elements) {
		/* Task that reads/writes this element */
		neighbor = compute_ids[TRIPLET_SIZE*ii + 2];

		/* Number of elements to compute for neighbor */
		n_xfer = 0;

		/*
		 * Since compute_ids is sorted on task, as long as task is
		 * unchanged, increment n_xfer
		 */
		while (ii < n_compute_elements
		       && compute_ids[TRIPLET_SIZE*ii+2] == neighbor) {
			n_xfer++;
			ii++;
		}
		if (neighbor != UNKNOWN_TASK) {
			n_neighbors++;
			n_xfer_total += n_xfer;
		}
	}

	/*
	 * Based on number of neighbors and total number of elements to transfer
	 * allocate the comp_list
	 */
	n_list = sizeof(SMIOL_Offset) * ((size_t)1
	                                 + (size_t)2 * n_neighbors
	                                 + n_xfer_total);
	(*decomp)->comp_list = (SMIOL_Offset *)malloc(n_list);
	if ((*decomp)->comp_list == NULL) {
		return SMIOL_MALLOC_FAILURE;
	}
	comp_list = (*decomp)->comp_list;

	/*
	 * Scan through compute_ids a second time, filling in the comp_list
	 */
	comp_list[0] = (SMIOL_Offset)n_neighbors;
	idx = 1; /* Index in compute_list where neighbor ID will be written,
	            followed by number of elements and element local IDs */

	ii = 0;
	while (ii < n_compute_elements) {
		/* Task that reads/writes this element */
		neighbor = compute_ids[TRIPLET_SIZE*ii + 2];

		/* Number of elements to compute for neighbor */
		n_xfer = 0;

		/*
		 * Since compute_ids is sorted on task, as long as task is
		 * unchanged, increment n_xfer
		 */
		while (ii < n_compute_elements
		       && compute_ids[TRIPLET_SIZE*ii+2] == neighbor) {
			if (neighbor != UNKNOWN_TASK) {
				/* Save local element ID in list */
				comp_list[idx+2+n_xfer] = compute_ids[TRIPLET_SIZE*ii+1];
				n_xfer++;
			}
			ii++;
		}
		if (neighbor != UNKNOWN_TASK) {
			comp_list[idx] = neighbor;
			comp_list[idx+1] = (SMIOL_Offset)n_xfer;
			idx += (2 + n_xfer);
		}
	}

	free(compute_ids);

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_free_decomp
 *
 * Frees a mapping between compute elements and I/O elements.
 *
 * Free all memory of a SMIOL_decomp and returns SMIOL_SUCCESS. If decomp
 * points to NULL, then do nothing and return SMIOL_SUCCESS. After this routine
 * is called, no other SMIOL routines should use the freed SMIOL_decomp.
 *
 ********************************************************************************/
int SMIOL_free_decomp(struct SMIOL_decomp **decomp)
{
	if ((*decomp) == NULL) {
		return SMIOL_SUCCESS;
	}

	free((*decomp)->comp_list);
	free((*decomp)->io_list);
	free((*decomp));
	*decomp = NULL;

	return SMIOL_SUCCESS;
}
