#ifndef VIENNA_RNA_PACKAGE_PLOT_LAYOUTS_DEPRECATED_H
#define VIENNA_RNA_PACKAGE_PLOT_LAYOUTS_DEPRECATED_H

/**
 *  @file ViennaRNA/plot_layouts.h
 *  @brief      Use ViennaRNA/plotting/layouts.h instead
 *  @deprecated Use ViennaRNA/plotting/layouts.h instead
 */

#ifdef __cplusplus
extern "C" {
#endif
  
  
#ifndef VRNA_DISABLE_BACKWARD_COMPATIBILITY
# ifdef VRNA_WARN_DEPRECATED
#warning "Including deprecated header file <ViennaRNA/plot_layouts.h>! Use <ViennaRNA/plotting/layouts.h> instead!"
# endif
#include <ViennaRNA/plotting/layouts.h>
#endif

#ifdef __cplusplus
}
#endif

#endif
