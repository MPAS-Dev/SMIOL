#include <stddef.h>
#include "smiol.h"


/********************************************************************************
 *
 * SMIOL_init
 *
 * Initialize a SMIOL context.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_init(void)
{
	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_finalize
 *
 * Finalize a SMIOL context.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_finalize(void)
{
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
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_open_file(void)
{
	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_close_file
 *
 * Closes a file within a SMIOL context.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_close_file(void)
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
struct SMIOL_decomp *SMIOL_create_decomp(void)
{
	return NULL;
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
	return SMIOL_SUCCESS;
}
