#include <stdlib.h>
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
 * Sorts an array of triplets of int64_t values in ascending order
 *
 * Given a pointer to an array of int64_t triplets, sorts the array in ascending
 * order on the specified entry: 0 sorts on the first value in the triplets,
 * 1 sorts on the second value, and 2 sorts on the third.
 *
 * If the sort_entry is 1 or 2, the relative position of two triplets whose
 * values in that entry match will be determined by their values in the first
 * entry.
 *
 * The sort is not guaranteed to be stable.
 *
 *******************************************************************************/
void sort_triplet_array(size_t n_arr, int64_t *arr, int sort_entry)
{
	switch (sort_entry) {
	case 0:
		qsort((void *)arr, n_arr, sizeof(int64_t) * TRIPLET_SIZE,
		      comp_sort_0);
		break;
	case 1:
		qsort((void *)arr, n_arr, sizeof(int64_t) * TRIPLET_SIZE,
		      comp_sort_1);
		break;
	case 2:
		qsort((void *)arr, n_arr, sizeof(int64_t) * TRIPLET_SIZE,
		      comp_sort_2);
		break;
	}
}


/*******************************************************************************
 *
 * search_triplet_array
 *
 * Searches a sorted array of triplets of int64_t values
 *
 * Given a pointer to a sorted array of int64_t triplets, searches the array on
 * the specified entry for the key value. A search_entry value of 0 searches for
 * the key in the first entry of each triplet, 1 searches in the second entry,
 * and 2 searches in the third.
 *
 * If the key is found, the address of the triplet will be returned; otherwise,
 * a NULL pointer is returned.
 *
 * If the key occurs in more than one triplet at the specified entry, there is
 * no guarantee as to which triplet's address will be returned.
 *
 *******************************************************************************/
int64_t *search_triplet_array(int64_t key, size_t n_arr, int64_t *arr,
                              int search_entry)
{
	int64_t *res;
	int64_t key3[TRIPLET_SIZE];

	key3[search_entry] = key;

	switch (search_entry) {
	case 0:
		res = (int64_t *)bsearch((const void *)&key3, (const void *)arr,
		                         n_arr, sizeof(int64_t) * TRIPLET_SIZE,
		                         comp_search_0);
		break;
	case 1:
		res = (int64_t *)bsearch((const void *)&key3, (const void *)arr,
		                         n_arr, sizeof(int64_t) * TRIPLET_SIZE,
		                         comp_search_1);
		break;
	case 2:
		res = (int64_t *)bsearch((const void *)&key3, (const void *)arr,
		                         n_arr, sizeof(int64_t) * TRIPLET_SIZE,
		                         comp_search_2);
		break;
	default:
		res = NULL;
	}

	return res;
}


/*******************************************************************************
 *
 * comp_sort_0
 *
 * Compares two int64_t triplets based on their first entry, returning:
 *  1 if the first is larger than the second,
 *  0 if the two are equal, and
 * -1 if the first is less than the second.
 *
 *******************************************************************************/
static int comp_sort_0(const void *a, const void *b)
{
	return (((const int64_t *)a)[0] > ((const int64_t *)b)[0])
	     - (((const int64_t *)a)[0] < ((const int64_t *)b)[0]);
}


/*******************************************************************************
 *
 * comp_sort_1
 *
 * Compares two int64_t triplets based on their second entry, returning:
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

	res = (((const int64_t *)a)[1] > ((const int64_t *)b)[1])
	    - (((const int64_t *)a)[1] < ((const int64_t *)b)[1]);
	if (res == 0) {
		res = (((const int64_t *)a)[0] > ((const int64_t *)b)[0])
		    - (((const int64_t *)a)[0] < ((const int64_t *)b)[0]);
	}
	return res;
}


/*******************************************************************************
 *
 * comp_sort_2
 *
 * Compares two int64_t triplets based on their third entry, returning:
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

	res = (((const int64_t *)a)[2] > ((const int64_t *)b)[2])
	    - (((const int64_t *)a)[2] < ((const int64_t *)b)[2]);
	if (res == 0) {
		res = (((const int64_t *)a)[0] > ((const int64_t *)b)[0])
		    - (((const int64_t *)a)[0] < ((const int64_t *)b)[0]);
	}
	return res;
}


/*******************************************************************************
 *
 * comp_search_0
 *
 * Compares two int64_t triplets based on their first entry, returning:
 *  1 if the first is larger than the second,
 *  0 if the two are equal, and
 * -1 if the first is less than the second.
 *
 *******************************************************************************/
static int comp_search_0(const void *a, const void *b)
{
	return (((const int64_t *)a)[0] > ((const int64_t *)b)[0])
	     - (((const int64_t *)a)[0] < ((const int64_t *)b)[0]);
}


/*******************************************************************************
 *
 * comp_search_1
 *
 * Compares two int64_t triplets based on their second entry, returning:
 *  1 if the first is larger than the second,
 *  0 if the two are equal, and
 * -1 if the first is less than the second.
 *
 *******************************************************************************/
static int comp_search_1(const void *a, const void *b)
{
	return (((const int64_t *)a)[1] > ((const int64_t *)b)[1])
	     - (((const int64_t *)a)[1] < ((const int64_t *)b)[1]);
}


/*******************************************************************************
 *
 * comp_search_2
 *
 * Compares two int64_t triplets based on their third entry, returning:
 *  1 if the first is larger than the second,
 *  0 if the two are equal, and
 * -1 if the first is less than the second.
 *
 *******************************************************************************/
static int comp_search_2(const void *a, const void *b)
{
	return (((const int64_t *)a)[2] > ((const int64_t *)b)[2])
	     - (((const int64_t *)a)[2] < ((const int64_t *)b)[2]);
}
