#ifndef VIENNA_RNA_PACKAGE_PARAMS_CONVERT_DEPRECATED_H
#define VIENNA_RNA_PACKAGE_PARAMS_CONVERT_DEPRECATED_H

/**
 *  @file ViennaRNA/convert_epars.h
 *  @brief      Use ViennaRNA/params/convert.h instead
 *  @deprecated Use ViennaRNA/params/convert.h instead
 */
#ifdef __cplusplus
extern "C" {
#endif
  
#ifndef VRNA_DISABLE_BACKWARD_COMPATIBILITY
# ifdef VRNA_WARN_DEPRECATED
#warning "Including deprecated header file <ViennaRNA/convert_epars.h>! Use <ViennaRNA/params/convert.h> instead!"
# endif
#include <ViennaRNA/params/convert.h>
#endif

#ifdef __cplusplus
}
#endif

#endif
