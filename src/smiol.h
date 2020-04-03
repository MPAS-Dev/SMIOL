/*******************************************************************************
 * SMIOL -- The Simple MPAS I/O Library
 *******************************************************************************/
#ifndef SMIOL_H
#define SMIOL_H

#include "smiol_types.h"


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
int SMIOL_define_dim(struct SMIOL_file *file, const char *dimname, SMIOL_Offset dimsize);
int SMIOL_inquire_dim(struct SMIOL_file *file, const char *dimname, SMIOL_Offset *dimsize);

/*
 * Variable methods
 */
int SMIOL_define_var(struct SMIOL_file *file, const char *varname, int vartype, int ndims, const char **dimnames);
int SMIOL_inquire_var(struct SMIOL_file *file, const char *varname, int *vartype, int *ndims, char **dimnames);
int SMIOL_put_var(struct SMIOL_file *file, struct SMIOL_decomp *decomp,
                  const char *varname, const void *buf);
int SMIOL_get_var(struct SMIOL_file *file, struct SMIOL_decomp *decomp,
                  const char *varname, const void *buf);

/*
 * Attribute methods
 */
int SMIOL_define_att(void);
int SMIOL_inquire_att(void);

/*
 * Control methods
 */
int SMIOL_sync_file(struct SMIOL_file *file);
const char *SMIOL_error_string(int errno);
const char *SMIOL_lib_error_string(struct SMIOL_context *context);
int SMIOL_set_option(void);

/*
 * Decomposition methods
 */
int SMIOL_create_decomp(struct SMIOL_context *context,
                        size_t n_compute_elements, SMIOL_Offset *compute_elements,
                        size_t n_io_elements, SMIOL_Offset *io_elements,
                        struct SMIOL_decomp **decomp);
int SMIOL_free_decomp(struct SMIOL_decomp **decomp);

#endif
