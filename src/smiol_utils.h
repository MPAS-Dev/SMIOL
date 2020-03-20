/*******************************************************************************
 * Utilities and helper functions for SMIOL
 *******************************************************************************/
#ifndef SMIOL_UTILS_H
#define SMIOL_UTILS_H

#include "smiol_types.h"


/*
 * Searching and sorting
 */
void sort_triplet_array(size_t n_arr, int64_t *arr, int sort_entry);
int64_t *search_triplet_array(int64_t key, size_t n_arr, int64_t *arr,
                              int search_entry);

/*
 * Debugging
 */
void print_lists(int comm_rank, int64_t *comp_list, int64_t *io_list);

#endif
