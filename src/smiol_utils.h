/*******************************************************************************
 * Utilities and helper functions for SMIOL
 *******************************************************************************/

#include <stdint.h>

#define TRIPLET_SIZE ((size_t)3)

/*
 * Searching and sorting
 */
void sort_triplet_array(size_t n_arr, int64_t *arr, int sort_entry);
int64_t *search_triplet_array(int64_t key, size_t n_arr, int64_t *arr,
                              int search_entry);
