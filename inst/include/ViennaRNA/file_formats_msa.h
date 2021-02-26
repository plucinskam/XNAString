#ifndef VIENNA_RNA_PACKAGE_FILE_FORMATS_MSA_DEPRECATED_H
#define VIENNA_RNA_PACKAGE_FILE_FORMATS_MSA_DEPRECATED_H

/**
 *  @file ViennaRNA/file_formats_msa.h
 *  @brief      Use ViennaRNA/io/file_formats_msa.h instead
 *  @deprecated Use ViennaRNA/io/file_formats_msa.h instead
 */

#ifdef __cplusplus
extern "C" {
#endif
  
#ifndef VRNA_DISABLE_BACKWARD_COMPATIBILITY
# ifdef VRNA_WARN_DEPRECATED
#warning "Including deprecated header file <ViennaRNA/file_formats_msa.h>! Use <ViennaRNA/io/file_formats_msa.h> instead!"
# endif
#include <ViennaRNA/io/file_formats_msa.h>
#endif

#ifdef __cplusplus
}
#endif

#endif
