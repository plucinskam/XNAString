#ifndef VIENNA_RNA_PACKAGE_PLOT_UTILS_DEPRECATED_H
#define VIENNA_RNA_PACKAGE_PLOT_UTILS_DEPRECATED_H

/**
 *  @file ViennaRNA/plot_utils.h
 *  @brief      Use ViennaRNA/plotting/utils.h instead
 *  @deprecated Use ViennaRNA/plotting/utils.h instead
 */

#ifdef __cplusplus
extern "C" {
#endif
  
#ifndef VRNA_DISABLE_BACKWARD_COMPATIBILITY
# ifdef VRNA_WARN_DEPRECATED
#warning "Including deprecated header file <ViennaRNA/plot_utils.h>! Use <ViennaRNA/plotting/utils.h> instead!"
# endif
#include <ViennaRNA/plotting/utils.h>
#endif

#ifdef __cplusplus
}
#endif

#endif
