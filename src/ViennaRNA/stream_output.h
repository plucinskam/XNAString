#ifndef VIENNA_RNA_PACKAGE_DATA_STRUCTURES_STREAM_OUTPUT_DEPRECATED_H
#define VIENNA_RNA_PACKAGE_DATA_STRUCTURES_STREAM_OUTPUT_DEPRECATED_H

/**
 *  @file ViennaRNA/stream_output.h
 *  @brief      Use ViennaRNA/datastructures/stream_output.h instead
 *  @deprecated Use ViennaRNA/datastructures/stream_output.h instead
 */

#ifdef __cplusplus
extern "C" {
#endif
  
#ifndef VRNA_DISABLE_BACKWARD_COMPATIBILITY
# ifdef VRNA_WARN_DEPRECATED
#warning "Including deprecated header file <ViennaRNA/stream_output.h>! Use <ViennaRNA/datastructures/stream_output.h> instead!"
# endif
#include <ViennaRNA/datastructures/stream_output.h>
#endif

  
#ifdef __cplusplus
}
#endif

#endif
