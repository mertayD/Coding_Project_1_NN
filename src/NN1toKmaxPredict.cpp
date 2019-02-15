#include <Eigen/Dense>

int NN1toKmaxPredict(
    const double *train_inputs_ptr,//n_train_observations x n_features
    const double *train_label_ptr, //n_train_observations
    const double *test_input_ptr, //n_train_observations
    const int n_test_observations, 
    const int n_train_observations, 
    const int n_features, 
    const int max_neighbours,
    double *test_predictions_ptr //n_test_observations, max_neighbors
){
  Eigen::Map <Eigen::MatrixXd > train_inputs_mat((double*)train_inputs_ptr, n_train_observations, n_features);
  Eigen::Map< Eigen::MatrixXd > test_input_mat(
      (double*)test_input_ptr, n_test_observations, n_features);
  
  Eigen::Map<Eigen::MatrixXd> test_predictions_mat((double*)test_predictions_ptr, n_test_observations, max_neighbours);
    
  Eigen::VectorXd dist_vec(n_train_observations);
  Eigen::VectorXi sorted_index_vec(n_train_observations);

  if(n_features > 0 && n_test_observations > 0 && n_train_observations > 0 && max_neighbours > 0 )
  {
    
    for(int j = 0; j < n_test_observations; j++ )
    {
      for(int i = 0; i< n_train_observations; i++){
        dist_vec(i) = (train_inputs_mat.row(i).transpose() - test_input_mat.row(j).transpose()).norm();
        sorted_index_vec(i) = i;
      }
      std::sort(
        sorted_index_vec.data(),
        sorted_index_vec.data()+sorted_index_vec.size(),
        [&dist_vec](int left, int right){
          return dist_vec(left) < dist_vec(right);
        }
      );
      double total = 0.0;
      for(int model_i = 0; model_i<max_neighbours; model_i++)
      {
        int neighbors = model_i + 1;
        int row_i = sorted_index_vec(model_i);
        total += train_label_ptr[row_i];
        test_predictions_mat(j,model_i) = total/neighbors;
      }
    }
    return 0;
  }
  return -1;
}