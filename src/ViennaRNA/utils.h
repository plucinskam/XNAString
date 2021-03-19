#ifndef VIENNA_RNA_PACKAGE_UTILS_DEPRECATED_H
#define VIENNA_RNA_PACKAGE_UTILS_DEPRECATED_H

/**
 *  @file ViennaRNA/utils.h
 *  @brief      Use ViennaRNA/utils/basic.h instead
 *  @deprecated Use ViennaRNA/utils/basic.h instead
 */

#ifdef __cplusplus
extern "C" {
#endif
  
  
#ifndef VRNA_DISABLE_BACKWARD_COMPATIBILITY
# ifdef VRNA_WARN_DEPRECATED
#warning "Including deprecated header file <ViennaRNA/utils.h>! Use <ViennaRNA/utils/basic.h> instead!"
# endif
#include <ViennaRNA/utils/basic.h>
#include <ViennaRNA/utils/strings.h>
#include <ViennaRNA/utils/structures.h>
#include <ViennaRNA/io/utils.h>
#include <ViennaRNA/alphabet.h>
#endif

void
    vrna_message_verror(const char  *format,
                        va_list     args)  
void
    vrna_message_error(const char *format,
                       ...)

  
#ifdef __cplusplus
}
#endif

#endif
