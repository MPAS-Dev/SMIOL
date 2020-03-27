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
