/*******************************************************************************
 * Utilities and helper functions for SMIOL
 *******************************************************************************/
#ifndef SMIOL_UTILS_H
#define SMIOL_UTILS_H

#include "smiol_types.h"


/*
 * Searching and sorting
 */
void sort_triplet_array(size_t n_arr, SMIOL_Offset *arr, int sort_entry);
SMIOL_Offset *search_triplet_array(SMIOL_Offset key,
                                   size_t n_arr, SMIOL_Offset *arr,
                                   int search_entry);

/*
 * Debugging
 */
void print_lists(int comm_rank, SMIOL_Offset *comp_list, SMIOL_Offset *io_list);

#endif
