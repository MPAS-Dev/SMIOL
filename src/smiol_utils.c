#include <stdlib.h>
#include <stdio.h>
#include "smiol_utils.h"

/*
 * Prototypes for functions used only internally by SMIOL utilities
 */
static int comp_sort_0(const void *a, const void *b);
static int comp_sort_1(const void *a, const void *b);
static int comp_sort_2(const void *a, const void *b);
static int comp_search_0(const void *a, const void *b);
static int comp_search_1(const void *a, const void *b);
static int comp_search_2(const void *a, const void *b);


/*******************************************************************************
 *
 * sort_triplet_array
 *
 * Sorts an array of triplets of SMIOL_Offset values in ascending order
 *
 * Given a pointer to an array of SMIOL_Offset triplets, sorts the array in
 * ascending order on the specified entry: 0 sorts on the first value in
 * the triplets, 1 sorts on the second value, and 2 sorts on the third.
 *
 * If the sort_entry is 1 or 2, the relative position of two triplets whose
 * values in that entry match will be determined by their values in the first
 * entry.
 *
 * The sort is not guaranteed to be stable.
 *
 *******************************************************************************/
void sort_triplet_array(size_t n_arr, SMIOL_Offset *arr, int sort_entry)
{
	size_t width = sizeof(SMIOL_Offset) * TRIPLET_SIZE;

	switch (sort_entry) {
	case 0:
		qsort((void *)arr, n_arr, width, comp_sort_0);
		break;
	case 1:
		qsort((void *)arr, n_arr, width, comp_sort_1);
		break;
	case 2:
		qsort((void *)arr, n_arr, width, comp_sort_2);
		break;
	}
}


/*******************************************************************************
 *
 * search_triplet_array
 *
 * Searches a sorted array of triplets of SMIOL_Offset values
 *
 * Given a pointer to a sorted array of SMIOL_Offset triplets, searches
 * the array on the specified entry for the key value. A search_entry value of
 * 0 searches for the key in the first entry of each triplet, 1 searches in
 * the second entry, and 2 searches in the third.
 *
 * If the key is found, the address of the triplet will be returned; otherwise,
 * a NULL pointer is returned.
 *
 * If the key occurs in more than one triplet at the specified entry, there is
 * no guarantee as to which triplet's address will be returned.
 *
 *******************************************************************************/
SMIOL_Offset *search_triplet_array(SMIOL_Offset key,
                                   size_t n_arr, SMIOL_Offset *arr,
                                   int search_entry)
{
	SMIOL_Offset *res;
	SMIOL_Offset key3[TRIPLET_SIZE];
	size_t width = sizeof(SMIOL_Offset) * TRIPLET_SIZE;

	key3[search_entry] = key;

	switch (search_entry) {
	case 0:
		res = (SMIOL_Offset *)bsearch((const void *)&key3,
		                              (const void *)arr, n_arr,
                                              width, comp_search_0);
		break;
	case 1:
		res = (SMIOL_Offset *)bsearch((const void *)&key3,
		                              (const void *)arr, n_arr,
                                              width, comp_search_1);
		break;
	case 2:
		res = (SMIOL_Offset *)bsearch((const void *)&key3,
		                              (const void *)arr, n_arr,
                                              width, comp_search_2);
		break;
	default:
		res = NULL;
	}

	return res;
}


/*******************************************************************************
 *
 * transfer_field
 *
 * Transfers a field between compute and I/O tasks
 *
 * Given a SMIOL_decomp and a direction, which determines whether the input
 * field is transferred from compute tasks to I/O tasks or from I/O tasks to
 * compute tasks, this function transfers the input field to the output field.
 *
 * The size in bytes of the elements in the field to be transferred is given by
 * element_size; for example, a single-precision field would set element_size
 * to sizeof(float).
 *
 * The caller must have already allocated the out_field argument with sufficient
 * space to contain the field.
 *
 * If no errors are detected in the input arguments or in the transfer of
 * the input field to the output field, SMIOL_SUCCESS is returned.
 *
 *******************************************************************************/
int transfer_field(const struct SMIOL_decomp *decomp, int dir,
                   size_t element_size, const void *in_field, void *out_field)
{
	MPI_Comm comm;
	int comm_rank;

	SMIOL_Offset *sendlist = NULL;
	SMIOL_Offset *recvlist = NULL;

	MPI_Request *send_reqs = NULL;
	MPI_Request *recv_reqs = NULL;

	uint8_t **send_bufs = NULL;
	uint8_t **recv_bufs = NULL;
	uint8_t *in_bytes = NULL;
	uint8_t *out_bytes = NULL;

	size_t ii, kk;
	size_t n_neighbors_send;
	size_t n_neighbors_recv;
	int64_t pos;
	int64_t pos_src = -1;
	int64_t pos_dst = -1;

	/*
	 * The following are ints because they correspond to MPI arguments
	 * that are ints, or they iterate over an int bound
	 */
	int taskid;
	int n_send, n_recv;
	int j;


	if (decomp == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	comm = MPI_Comm_f2c(decomp->context->fcomm);
	comm_rank = decomp->context->comm_rank;

	/*
	 * Throughout this function, operate on the fields as arrays of bytes
	 */
	in_bytes = (uint8_t *)in_field;
	out_bytes = (uint8_t *)out_field;

	/*
	 * Set send and recv lists based on exchange direction
	 */
	if (dir == SMIOL_COMP_TO_IO) {
		sendlist = decomp->comp_list;
		recvlist = decomp->io_list;
	} else if (dir == SMIOL_IO_TO_COMP) {
		sendlist = decomp->io_list;
		recvlist = decomp->comp_list;
	} else {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Determine how many other MPI tasks to communicate with, and allocate
	 * request lists and buffer pointers
	 */
	n_neighbors_send = (size_t)(sendlist[0]);
	n_neighbors_recv = (size_t)(recvlist[0]);

	/*
	 * Check that we have non-NULL in_field and out_field arguments
	 * in agreement with the number of neighbors to send/recv to/from
	 */
	if ((in_field == NULL && n_neighbors_send != 0)
	    || (out_field == NULL && n_neighbors_recv != 0)) {
		return SMIOL_INVALID_ARGUMENT;
	}

	send_reqs = (MPI_Request *)malloc(sizeof(MPI_Request)
	                                  * n_neighbors_send);
	recv_reqs = (MPI_Request *)malloc(sizeof(MPI_Request)
	                                  * n_neighbors_recv);

	send_bufs = (uint8_t **)malloc(sizeof(uint8_t *) * n_neighbors_send);
	recv_bufs = (uint8_t **)malloc(sizeof(uint8_t *) * n_neighbors_recv);

	/*
	 * Post receives
	 */
	pos = 1;
	for (ii = 0; ii < n_neighbors_recv; ii++) {
		taskid = (int)recvlist[pos++];
		n_recv = (int)recvlist[pos++];
		if (taskid != comm_rank) {
			recv_bufs[ii] = (uint8_t *)malloc(sizeof(uint8_t)
			                                  * element_size
			                                  * (size_t)n_recv);

			MPI_Irecv((void *)recv_bufs[ii],
			          n_recv * (int)element_size,
			          MPI_BYTE, taskid, comm_rank, comm,
			          &recv_reqs[ii]);
		}
		else {
			/*
			 * This is a receive from ourself - save position in
			 * recvlist for local copy, below
			 */
			pos_dst = pos - 1; /* Offset of n_recv */
			recv_bufs[ii] = NULL;
		}
		pos += n_recv;
	}

	/*
	 * Post sends
	 */
	pos = 1;
	for (ii = 0; ii < n_neighbors_send; ii++) {
		taskid = (int)sendlist[pos++];
		n_send = (int)sendlist[pos++];
		if (taskid != comm_rank) {
			send_bufs[ii] = (uint8_t *)malloc(sizeof(uint8_t)
			                                  * element_size
			                                  * (size_t)n_send);

			/* Pack send buffer */
			for (j = 0; j < n_send; j++) {
				size_t out_idx = (size_t)j
				                 * element_size;
				size_t in_idx = (size_t)sendlist[pos]
				                * element_size;

				for (kk = 0; kk < element_size; kk++) {
					send_bufs[ii][out_idx + kk] = in_bytes[in_idx + kk];
				}
				pos++;
			}

			MPI_Isend((void *)send_bufs[ii],
			          n_send * (int)element_size,
			          MPI_BYTE, taskid, taskid, comm,
			          &send_reqs[ii]);
		}
		else {
			/*
			 * This is a send to ourself - save position in
			 * sendlist for local copy, below
			 */
			pos_src = pos - 1; /* Offset of n_send */
			send_bufs[ii] = NULL;
			pos += n_send;
		}
	}

	/*
	 * Handle local copies
	 */
	if (pos_src >= 0 && pos_dst >= 0) {

		/* n_send and n_recv should actually be identical */
		n_send = (int)sendlist[pos_src++];
		n_recv = (int)recvlist[pos_dst++];

		for (j = 0; j < n_send; j++) {
			size_t out_idx = (size_t)recvlist[pos_dst]
			                 * element_size;
			size_t in_idx = (size_t)sendlist[pos_src]
			                * element_size;

			for (kk = 0; kk < element_size; kk++) {
				out_bytes[out_idx + kk] = in_bytes[in_idx + kk];
			}
			pos_dst++;
			pos_src++;
		}
	}

	/*
	 * Wait on receives
	 */
	pos = 1;
	for (ii = 0; ii < n_neighbors_recv; ii++) {
		taskid = (int)recvlist[pos++];
		n_recv = (int)recvlist[pos++];
		if (taskid != comm_rank) {
			MPI_Wait(&recv_reqs[ii], MPI_STATUS_IGNORE);

			/* Unpack receive buffer */
			for (j = 0; j < n_recv; j++) {
				size_t out_idx = (size_t)recvlist[pos]
				                 * element_size;
				size_t in_idx = (size_t)j
				                * element_size;

				for (kk = 0; kk < element_size; kk++) {
					out_bytes[out_idx + kk] = recv_bufs[ii][in_idx + kk];
				}
				pos++;
			}
		}
		else {
			/*
			 * A receive from ourself - just skip to next neighbor
			 * in the recvlist
			 */
			pos += n_recv;
		}

		/*
		 * The receive buffer for the current neighbor can now be freed
		 */
		if (recv_bufs[ii] != NULL) {
			free(recv_bufs[ii]);
		}
	}

	/*
	 * Wait on sends
	 */
	pos = 1;
	for (ii = 0; ii < n_neighbors_send; ii++) {
		taskid = (int)sendlist[pos++];
		n_send = (int)sendlist[pos++];
		if (taskid != comm_rank) {
			MPI_Wait(&send_reqs[ii], MPI_STATUS_IGNORE);
		}

		/*
		 * The send buffer for the current neighbor can now be freed
		 */
		if (send_bufs[ii] != NULL) {
			free(send_bufs[ii]);
		}

		pos += n_send;
	}

	/*
	 * Free request lists and buffer pointers
	 */
	free(send_reqs);
	free(recv_reqs);
	free(send_bufs);
	free(recv_bufs);

	return SMIOL_SUCCESS;
}


/*******************************************************************************
 *
 * get_io_elements
 *
 * Returns a contiguous range of I/O elements for an MPI task
 *
 * Given the rank of a task, a description of the I/O task arrangement --
 * the number of I/O tasks and the stride between I/O tasks -- as well as the
 * total number of elements to read or write, compute the offset of the first
 * I/O element as well as the number of elements to read or write for the task.
 *
 * If this routine is successful in producing a valid io_start and io_count,
 * a value of 0 is returned; otherwise, a non-zero value is returned.
 *
 *******************************************************************************/
int get_io_elements(int comm_rank, int num_io_tasks, int io_stride,
                    size_t n_io_elements, size_t *io_start, size_t *io_count)
{
	if (io_start == NULL || io_count == NULL) {
		return 1;
	}

	*io_start = 0;
	*io_count = 0;

	if (comm_rank % io_stride == 0) {
		size_t io_rank = (size_t)(comm_rank / io_stride);
		size_t elems_per_task = (n_io_elements / (size_t)num_io_tasks);

		*io_start = io_rank * elems_per_task;
		*io_count = elems_per_task;

		if (io_rank + 1 == (size_t)num_io_tasks) {
			size_t remainder = n_io_elements
			                   - (size_t)num_io_tasks * elems_per_task;
			*io_count += remainder;
		}
	}

	return 0;
}


/*******************************************************************************
 *
 * print_lists
 *
 * Writes the contents of comp_list and io_list arrays to a text file
 *
 * Given pointers to the comp_list and io_list arrays from a SMIOL_decomp
 * structure, writes the contents of these arrays to a text file in a human-
 * readable format.
 *
 * Because the comp_list and io_list arrays are unique to each MPI task, this
 * routine takes as an argument the MPI rank of the calling task. The output
 * text file is named list.NNNN.txt, where NNNN is the rank of the task.
 *
 *******************************************************************************/
void print_lists(int comm_rank, SMIOL_Offset *comp_list, SMIOL_Offset *io_list)
{
	char filename[14];
	FILE *f;
	SMIOL_Offset n_neighbors;
	SMIOL_Offset n_elems, neighbor;
	int i, j, k;

	snprintf(filename, 14, "list.%4.4i.txt", comm_rank);

	f = fopen(filename, "w");

	/*
	 * The lists below are structured as follows:
	 *   list[0] - the number of neighbors for which a task sends/recvs
	 *                                                                             |
	 *   list[n] - neighbor task ID                                                | repeated for
	 *   list[n+1] - number of elements, m, to send/recv to/from the neighbor      | each neighbor
	 *   list[n+2 .. n+2+m] - local element IDs to send/recv to/from the neighbor  |
	 *                                                                             |
	 */

	fprintf(f, "===== comp_list for MPI rank %i =====\n", comm_rank);
	fprintf(f, "Our compute elements are read/written on %i tasks\n",
	        (int)comp_list[0]);
	j = 0;
	n_neighbors = comp_list[j++];
	for (i = 0; i < n_neighbors; i++) {
		neighbor = comp_list[j++];
		n_elems = comp_list[j++];
		if (neighbor == comm_rank) {
			fprintf(f, "----- copy %i elements -----\n",
			        (int)n_elems);
		} else {
			fprintf(f, "----- send %i elements to %i -----\n",
			        (int)n_elems, (int)neighbor);
		}
		for (k = 0; k < n_elems; k++) {
			fprintf(f, "  %i\n", (int)comp_list[j+k]);
		}
		j += n_elems;
	}

	fprintf(f, "\n\n");
	fprintf(f, "===== io_list for MPI rank %i =====\n", comm_rank);
	fprintf(f, "Our I/O elements are computed on %i tasks\n",
	        (int)io_list[0]);
	j = 0;
	n_neighbors = io_list[j++];
	for (i = 0; i < n_neighbors; i++) {
		neighbor = io_list[j++];
		n_elems = io_list[j++];
		if (neighbor == comm_rank) {
			fprintf(f, "----- copy %i elements -----\n",
			        (int)n_elems);
		} else {
			fprintf(f, "----- recv %i elements from %i -----\n",
			        (int)n_elems, (int)neighbor);
		}
		for (k = 0; k < n_elems; k++) {
			fprintf(f, "  %i\n", (int)io_list[j+k]);
		}
		j += n_elems;
	}

	fclose(f);
}


/*******************************************************************************
 *
 * comp_sort_0
 *
 * Compares two SMIOL_Offset triplets based on their first entry, returning:
 *  1 if the first is larger than the second,
 *  0 if the two are equal, and
 * -1 if the first is less than the second.
 *
 *******************************************************************************/
static int comp_sort_0(const void *a, const void *b)
{
	return (((const SMIOL_Offset *)a)[0] > ((const SMIOL_Offset *)b)[0])
	     - (((const SMIOL_Offset *)a)[0] < ((const SMIOL_Offset *)b)[0]);
}


/*******************************************************************************
 *
 * comp_sort_1
 *
 * Compares two SMIOL_Offset triplets based on their second entry, returning:
 *  1 if the first is larger than the second,
 *  0 if the two are equal, and
 * -1 if the first is less than the second.
 *
 * If the triplets a and b have equal values in their second entry, the values
 * in their first entry will be used to determine the result of the comparison.
 *
 *******************************************************************************/
static int comp_sort_1(const void *a, const void *b)
{
	int res;

	res = (((const SMIOL_Offset *)a)[1] > ((const SMIOL_Offset *)b)[1])
	    - (((const SMIOL_Offset *)a)[1] < ((const SMIOL_Offset *)b)[1]);
	if (res == 0) {
		res = (((const SMIOL_Offset *)a)[0] > ((const SMIOL_Offset *)b)[0])
		    - (((const SMIOL_Offset *)a)[0] < ((const SMIOL_Offset *)b)[0]);
	}
	return res;
}


/*******************************************************************************
 *
 * comp_sort_2
 *
 * Compares two SMIOL_Offset triplets based on their third entry, returning:
 *  1 if the first is larger than the second,
 *  0 if the two are equal, and
 * -1 if the first is less than the second.
 *
 * If the triplets a and b have equal values in their third entry, the values
 * in their first entry will be used to determine the result of the comparison.
 *
 *******************************************************************************/
static int comp_sort_2(const void *a, const void *b)
{
	int res;

	res = (((const SMIOL_Offset *)a)[2] > ((const SMIOL_Offset *)b)[2])
	    - (((const SMIOL_Offset *)a)[2] < ((const SMIOL_Offset *)b)[2]);
	if (res == 0) {
		res = (((const SMIOL_Offset *)a)[0] > ((const SMIOL_Offset *)b)[0])
		    - (((const SMIOL_Offset *)a)[0] < ((const SMIOL_Offset *)b)[0]);
	}
	return res;
}


/*******************************************************************************
 *
 * comp_search_0
 *
 * Compares two SMIOL_Offset triplets based on their first entry, returning:
 *  1 if the first is larger than the second,
 *  0 if the two are equal, and
 * -1 if the first is less than the second.
 *
 *******************************************************************************/
static int comp_search_0(const void *a, const void *b)
{
	return (((const SMIOL_Offset *)a)[0] > ((const SMIOL_Offset *)b)[0])
	     - (((const SMIOL_Offset *)a)[0] < ((const SMIOL_Offset *)b)[0]);
}


/*******************************************************************************
 *
 * comp_search_1
 *
 * Compares two SMIOL_Offset triplets based on their second entry, returning:
 *  1 if the first is larger than the second,
 *  0 if the two are equal, and
 * -1 if the first is less than the second.
 *
 *******************************************************************************/
static int comp_search_1(const void *a, const void *b)
{
	return (((const SMIOL_Offset *)a)[1] > ((const SMIOL_Offset *)b)[1])
	     - (((const SMIOL_Offset *)a)[1] < ((const SMIOL_Offset *)b)[1]);
}


/*******************************************************************************
 *
 * comp_search_2
 *
 * Compares two SMIOL_Offset triplets based on their third entry, returning:
 *  1 if the first is larger than the second,
 *  0 if the two are equal, and
 * -1 if the first is less than the second.
 *
 *******************************************************************************/
static int comp_search_2(const void *a, const void *b)
{
	return (((const SMIOL_Offset *)a)[2] > ((const SMIOL_Offset *)b)[2])
	     - (((const SMIOL_Offset *)a)[2] < ((const SMIOL_Offset *)b)[2]);
}
