int NN1toKmaxPredict(
    const double *train_inputs_ptr,//n_train_observations, n_features
    const double *train_label_ptr, //n_train_observations
    const double *test_input_ptr, //n_train_observations, n_features
    const int n_test_observations, 
    const int n_train_observations, 
    const int n_features, 
    const int max_neighbours,
    double *test_predictions_ptr //n_test_observations, max_neighbors
);