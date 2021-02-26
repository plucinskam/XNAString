#ifndef VIENNA_RNA_PACKAGE_CONSTRAINTS_SOFT_DEPRECATED_H
#define VIENNA_RNA_PACKAGE_CONSTRAINTS_SOFT_DEPRECATED_H

/**
 *  @file constraints_soft.h
 *  @brief      Use ViennaRNA/constraints/soft.h instead
 *  @deprecated Use ViennaRNA/constraints/soft.h instead
 */

#ifdef __cplusplus
extern "C" {
#endif
  
#ifndef VRNA_DISABLE_BACKWARD_COMPATIBILITY
# ifdef VRNA_WARN_DEPRECATED
#warning "Including deprecated header file <ViennaRNA/constraints_soft.h>! Use <ViennaRNA/constraints/soft.h> instead!"
# endif
#include <ViennaRNA/constraints/soft.h>
#endif

#ifdef __cplusplus
}
#endif

#endif
