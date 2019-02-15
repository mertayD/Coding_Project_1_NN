#include "NN1toKmaxPredict.h"
#include <R.h> 
#include <R_ext/Rdynload.h>

void NN1toKmaxPredict_interface(
    const double *train_inputs_ptr,//n_train_observations, n_features
    const double *train_label_ptr, //n_train_observations
    const double *test_input_ptr, //n_train_observations, n_features
    const int *n_test_observations_ptr, 
    const int *n_train_observations_ptr, 
    const int *n_features_ptr, 
    const int *max_neighbours_ptr,
    double *test_predictions_ptr //n_test_observations, max_neighbors
){
  int status = NN1toKmaxPredict(train_inputs_ptr, train_label_ptr, test_input_ptr,
                                *n_test_observations_ptr,*n_train_observations_ptr,*n_features_ptr,
                                *max_neighbours_ptr, test_predictions_ptr);
  if(status != 0)
  {
    error("non-zero exit status from NN1toKmaxPredict");
  }
}

R_CMethodDef cMethods[] = {
  {"NN1toKmaxPredict_interface", (DL_FUNC) &NN1toKmaxPredict_interface, 8},
  {NULL,NULL,0}
};

extern "C" {
  void R_init_codungProject1(DllInfo *info){
    R_registerRoutines(info, cMethods, NULL, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
  }
}