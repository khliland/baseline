#include<R_ext/Rdynload.h>
#ifndef R_R_H
#  include <R.h>
#endif


void F77_NAME(dgbsv)(int*, int*, int*, int*, double*, int*,
              int*, double*, int*, int*);

R_FortranMethodDef fortranMethods[] = {
  {"dgbsv", (DL_FUNC) &F77_SUB(dgbsv), 10},
  {NULL, NULL, 0}
};
void R_init_baseline(DllInfo *info) {
  R_registerRoutines(info, NULL, NULL, fortranMethods, NULL);
  R_useDynamicSymbols(info, FALSE);
}
