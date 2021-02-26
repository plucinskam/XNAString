#ifndef VIENNA_RNA_PACKAGE_UTILS_FUN_H
#define VIENNA_RNA_PACKAGE_UTILS_FUN_H

#ifdef __cplusplus
extern "C" {
#endif
  
void
vrna_fun_dispatch_disable(void);


void
vrna_fun_dispatch_enable(void);


int
vrna_fun_zip_add_min(const int  *e1,
                     const int  *e2,
                     int        count);

#ifdef __cplusplus
}
#endif

#endif
