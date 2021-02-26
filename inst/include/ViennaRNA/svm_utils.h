#ifndef VIENNA_RNA_PACKAGE_UTILS_SVM_DEPRECATED_H
#define VIENNA_RNA_PACKAGE_UTILS_SVM_DEPRECATED_H

/**
 *  @file ViennaRNA/svm_utils.h
 *  @brief      Use ViennaRNA/utils/svm.h instead
 *  @deprecated Use ViennaRNA/utils/svm.h instead
 */

#ifdef __cplusplus
extern "C" {
#endif
  
  
#ifndef VRNA_DISABLE_BACKWARD_COMPATIBILITY
# ifdef VRNA_WARN_DEPRECATED
#warning "Including deprecated header file <ViennaRNA/svm_utils.h>! Use <ViennaRNA/utils/svm.h> instead!"
# endif
#include <ViennaRNA/utils/svm.h>
#endif

#ifdef __cplusplus
}
#endif

#endif
