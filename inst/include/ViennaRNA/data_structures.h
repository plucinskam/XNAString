#ifndef VIENNA_RNA_PACKAGE_DATA_STRUCTURES_DEPRECATED_H
#define VIENNA_RNA_PACKAGE_DATA_STRUCTURES_DEPRECATED_H

/**
 *  @file ViennaRNA/data_structures.h
 *  @brief      Use ViennaRNA/datastructures/basic.h instead
 *  @deprecated Use ViennaRNA/datastructures/basic.h instead
 */
#ifdef __cplusplus
extern "C" {
#endif
  
#ifndef VRNA_DISABLE_BACKWARD_COMPATIBILITY
# ifdef VRNA_WARN_DEPRECATED
#warning "Including deprecated header file <ViennaRNA/data_structures.h>! Use <ViennaRNA/datastructures/basic.h> instead!"
# endif
#include <ViennaRNA/datastructures/basic.h>
#endif

#ifdef __cplusplus
}
#endif

#endif
