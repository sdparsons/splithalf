#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _splithalf_Speedloop(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_splithalf_Speedloop", (DL_FUNC) &_splithalf_Speedloop, 3},
    {NULL, NULL, 0}
};

void R_init_splithalf(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, TRUE);
}
