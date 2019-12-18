/*******************************************************************************
 * SMIOL -- The Simple MPAS I/O Library
 *******************************************************************************/

#include <stdint.h>

/*
 * Types
 */
struct SMIOL_context {
	int foo;
};

struct SMIOL_file {
	int foo;
};

struct SMIOL_decomp {
	size_t n_compute_elements;
	size_t n_io_elements;
	uint64_t *compute_elements;
	uint64_t *io_elements;
};

/*
 * Return error codes
 */
#include "smiol_codes.inc"

/*
 * Library methods
 */
int SMIOL_init(void);
int SMIOL_finalize(void);
int SMIOL_inquire(void);

/*
 * File methods
 */
int SMIOL_open_file(void);
int SMIOL_close_file(void);

/*
 * Dimension methods
 */
int SMIOL_define_dim(void);
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
int SMIOL_set_option(void);

/*
 * Decomposition methods
 */
struct SMIOL_decomp *SMIOL_create_decomp(size_t n_compute_elements,
		size_t n_io_elements, uint64_t *compute_elements,
		uint64_t *io_elements);
int SMIOL_free_decomp(struct SMIOL_decomp **d);
