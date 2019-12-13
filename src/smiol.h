/*******************************************************************************
 * SMIOL -- The Simple MPAS I/O Library
 *******************************************************************************/

/*
 * Types
 */
struct {
} SMIOL_context;

struct {
} SMIOL_file;

/*
 * Return error codes
 */
#define SMIOL_SUCCESS (0)

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
