#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <functional>  // Ajout pour std::function

using namespace Rcpp;

class UncertaintyUtils {
public:
  static NumericMatrix absolute_residual_scores_cpp(NumericMatrix y_pred, NumericMatrix y_true) {
    int n = y_pred.nrow();
    int d = y_pred.ncol();

    // Vérification de la correspondance des dimensions
    if (n != y_true.nrow() || d != y_true.ncol()) {
      stop("Dimensions of y_pred and y_true do not match.");
    }

    NumericMatrix res(n, d);

    for (int i = 0; i < n; i++) {
      for (int j = 0; j < d; j++) {
        res(i, j) = std::abs(y_pred(i, j) - y_true(i, j));
      }
    }

    return res;
  }

  static double simultaneous_coverage_cpp(NumericMatrix y_true, NumericMatrix y_lower, NumericMatrix y_upper) {
    int n = y_true.nrow();
    int d = y_true.ncol();

    // Vérification de la correspondance des dimensions
    if (n != y_lower.nrow() || n != y_upper.nrow() || d != y_lower.ncol() || d != y_upper.ncol()) {
      stop("Dimensions of y_true, y_lower, and y_upper must match.");
    }

    int count = 0;
    for (int i = 0; i < n; i++) {
      bool inside = true;
      for (int j = 0; j < d; j++) {
        if (y_true(i, j) < y_lower(i, j) || y_true(i, j) > y_upper(i, j)) {
          inside = false;
          break;
        }
      }
      if (inside) count++;
    }
    return static_cast<double>(count) / n;
  }

  static NumericMatrix get_sorted_scores_cpp(NumericMatrix Scores) {
    int n = Scores.nrow();
    int d = Scores.ncol();

    // Vérification de la validité de la matrice Scores
    if (n <= 0 || d <= 0) {
      stop("Scores matrix must have positive dimensions.");
    }

    NumericMatrix sorted(n, d);

    for (int j = 0; j < d; j++) {
      std::vector<double> col(n);
      for (int i = 0; i < n; i++) {
        col[i] = Scores(i, j);
      }
      std::sort(col.begin(), col.end());

      for (int i = 0; i < n; i++) {
        sorted(i, j) = col[i];
      }
    }
    return sorted;
  }

  static NumericVector get_Beta_quantiles_cpp(NumericMatrix sorted_Scores, double Beta) {
    int n = sorted_Scores.nrow();
    int d = sorted_Scores.ncol();
    NumericVector quantiles(d);

    if (Beta < 0.0 || Beta > 1.0) {
      stop("Beta must be between 0 and 1.");
    }

    for (int i = 0; i < d; i++) {
      int rank = static_cast<int>(std::ceil((1 - Beta) * (n + 1))) - 1;
      rank = std::min(rank, n - 1);

      quantiles[i] = sorted_Scores(rank, i);
    }

    return quantiles;
  }



  static List get_prediction_bounds_cpp(NumericVector quantile, NumericMatrix y_pred) {
    int n = y_pred.nrow();
    int d = y_pred.ncol();

    // Vérification de la compatibilité des dimensions
    if (quantile.size() != d) {
      stop("Size of quantile vector must match number of columns in y_pred.");
    }

    NumericMatrix y_lower(n, d);
    NumericMatrix y_upper(n, d);

    for (int i = 0; i < n; i++) {
      for (int j = 0; j < d; j++) {
        y_lower(i, j) = y_pred(i, j) - quantile[j];
        y_upper(i, j) = y_pred(i, j) + quantile[j];
      }
    }

    return List::create(Named("y_lower") = y_lower,
                        Named("y_upper") = y_upper);
  }

  static NumericMatrix column_wise_rank_cpp(NumericMatrix mat) {
    int n = mat.nrow();
    int d = mat.ncol();

    // Vérification de la validité de la matrice
    if (n <= 0 || d <= 0) {
      stop("Matrix must have positive dimensions.");
    }

    NumericMatrix ranks(n, d);

    for (int j = 0; j < d; j++) {
      std::vector<std::pair<double, int>> col(n);
      for (int i = 0; i < n; i++) {
        col[i] = std::make_pair(mat(i, j), i);
      }
      std::sort(col.begin(), col.end());
      for (int i = 0; i < n; i++) {
        ranks(col[i].second, j) = i + 1;
      }
    }
    return ranks;
  }

  static NumericVector R_max_vector_cpp(NumericMatrix rank_matrix) {
    int n = rank_matrix.nrow();
    int d = rank_matrix.ncol();

    // Vérification de la validité de la matrice
    if (n <= 0 || d <= 0) {
      stop("Matrix must have positive dimensions.");
    }

    NumericVector r_max(n);

    for (int i = 0; i < n; i++) {
      // Initialiser le rang maximum avec la première dimension
      double max_rank = rank_matrix(i, 0);
      // Comparer avec les autres dimensions pour trouver le plus grand rang
      for (int j = 1; j < d; j++) {
        if (rank_matrix(i, j) > max_rank) {
          max_rank = rank_matrix(i, j);
        }
      }
      r_max[i] = max_rank;  // Affecter le rang maximum pour l'observation i
    }

    return r_max;
  }

  static int get_r_max_scalar_cpp(NumericVector R_max, double alpha) {
    int n = R_max.size();

    if (n <= 0) {
      stop("R_max vector must have positive size.");
    }

    // Trier les valeurs de R_max
    std::vector<double> sorted_Rmax(R_max.begin(), R_max.end());
    std::sort(sorted_Rmax.begin(), sorted_Rmax.end());

    // Calculer le rang à partir du quantile (1 - alpha)
    int rank = std::ceil((1 - alpha) * (n + 1)); // Calcul du rang à partir du quantile
    rank = std::min(rank, n);  // Assurer que le rang ne dépasse pas le nombre d'observations

    // Retourner la valeur correspondant à ce rang dans le vecteur trié
    return static_cast<int>(sorted_Rmax[rank - 1]);
  }

  static NumericVector max_rank_quantiles_cpp(int r_max, NumericMatrix sorted_score_matrix) {
    int d = sorted_score_matrix.ncol();
    int n = sorted_score_matrix.nrow();

    if (r_max > n) {
      stop("r_max exceeds number of rows in the score matrix.");
    }

    if (r_max > n) {
      stop("r_max exceeds number of rows in the score matrix.");
    }

    NumericVector quantiles(d);
    for (int j = 0; j < d; j++) {
      quantiles[j] = sorted_score_matrix(r_max - 1, j);
    }
    return quantiles;
  }

  static double simultaneous_coverage_bis_cpp(NumericVector R_max, double Beta) {
    int n = R_max.size();
    int threshold = std::ceil((1 - Beta) * n);
    int count = 0;

    for (int i = 0; i < n; i++) {
      if (R_max[i] <= threshold) {
        count++;
      }
    }

    return static_cast<double>(count) / n;
  }

  // Wrapper pour rendre simultaneous_coverage_bis compatible avec dichotomie_cpp
  static double coverage_wrapper_cpp(double Beta, NumericVector R_max) {
    // Vérification de la validité de R_max
    if (R_max.size() == 0) {
      stop("R_max vector cannot be empty.");
    }

    return UncertaintyUtils::simultaneous_coverage_bis_cpp(R_max, Beta);
  }

  // Fonction de dichotomie avec le wrapper pour rendre la fonction de couverture compatible
  static double dichotomie_cpp(std::function<double(double)> Eval_simultaneous_coverage_beta, double target, double xmin, double xmax, int n_iter, double epsilon) {

    // Vérification des paramètres d'entrée
    if (xmin >= xmax) {
      stop("xmin must be smaller than xmax.");
    }
    if (n_iter <= 0) {
      stop("n_iter must be greater than 0.");
    }
    if (epsilon <= 0) {
      stop("epsilon must be greater than 0.");
    }

    double a = xmin, b = xmax;
    double c, coverage;
    for (int i = 0; i < n_iter; ++i) {
      c = (a + b) / 2.0;
      coverage = Eval_simultaneous_coverage_beta(c);
      std::cout << "Beta = " << c << " => Coverage = " << coverage << std::endl;

      // Vérifier la convergence selon epsilon
      if (std::abs(b - a) < epsilon) {
        std::cout << "Convergence reached with Beta = " << c << std::endl;
        break;
      }

      if (std::abs(b - a) < epsilon) {
        break; // Convergence atteinte
      }

      // Ajuster a et b en fonction de la couverture
      if (coverage > target) {
        a = c;
      } else {
        b = c;
      }
    }
    return (a + b) / 2.0; // Retourne la meilleure valeur de Beta
  }
};

// [[Rcpp::export]]
NumericMatrix absolute_residual_scores_cpp(NumericMatrix y_pred, NumericMatrix y_true) {
  return UncertaintyUtils::absolute_residual_scores_cpp(y_pred, y_true);
}


// [[Rcpp::export]]
double simultaneous_coverage_cpp(NumericMatrix y_true, NumericMatrix y_lower, NumericMatrix y_upper) {
  return UncertaintyUtils::simultaneous_coverage_cpp(y_true, y_lower, y_upper);
}

// [[Rcpp::export]]
NumericMatrix get_sorted_scores_cpp(NumericMatrix Scores) {
  return UncertaintyUtils::get_sorted_scores_cpp(Scores);
}

// [[Rcpp::export]]
NumericVector get_Beta_quantiles_cpp(NumericMatrix sorted_Scores, double Beta) {
  return UncertaintyUtils::get_Beta_quantiles_cpp(sorted_Scores, Beta);
}

// [[Rcpp::export]]
List get_prediction_bounds_cpp(NumericVector quantile, NumericMatrix y_pred) {
  return UncertaintyUtils::get_prediction_bounds_cpp(quantile, y_pred);
}

// [[Rcpp::export]]
NumericMatrix column_wise_rank_cpp(NumericMatrix mat) {
  return UncertaintyUtils::column_wise_rank_cpp(mat);
}

// [[Rcpp::export]]
NumericVector R_max_vector_cpp(NumericMatrix rank_matrix) {
  return UncertaintyUtils::R_max_vector_cpp(rank_matrix);
}

// [[Rcpp::export]]
int get_r_max_scalar_cpp(NumericVector R_max, double alpha) {
  return UncertaintyUtils::get_r_max_scalar_cpp(R_max, alpha);
}

// [[Rcpp::export]]
NumericVector max_rank_quantiles_cpp(int r_max, NumericMatrix sorted_score_matrix) {
  return UncertaintyUtils::max_rank_quantiles_cpp(r_max, sorted_score_matrix);
}

// [[Rcpp::export]]
double dichotomie_wrapper_cpp(double target, double xmin, double xmax, int n_iter, NumericVector R_max) {
  // Créer une lambda pour encapsuler coverage_wrapper_cpp avec R_max
  auto Eval_simultaneous_coverage_beta = [&](double Beta) -> double {
    return UncertaintyUtils::simultaneous_coverage_bis_cpp(R_max, Beta);
  };

  // Appeler la méthode statique dichotomie_cpp
  return UncertaintyUtils::dichotomie_cpp(Eval_simultaneous_coverage_beta, target, xmin, xmax, n_iter, 1e-6);
}

// [[Rcpp::export]]
double simultaneous_coverage_bis_wrapper_cpp(NumericVector R_max, double Beta) {
  return UncertaintyUtils::simultaneous_coverage_bis_cpp(R_max, Beta);
}

// [[Rcpp::export]]
List fit_cpp(NumericMatrix y_pred_calib, NumericMatrix Y_calibration, std::string uncertainty_method, double Global_alpha) {
  int n = y_pred_calib.nrow();
  int d = y_pred_calib.ncol();

  // Vérifier que y_pred_calib et Y_calibration ont les mêmes dimensions
  if (y_pred_calib.nrow() != Y_calibration.nrow() || y_pred_calib.ncol() != Y_calibration.ncol()) {
    stop("The number of rows and columns of y_pred_calib must match with Y_calibration.");
  }

  // 1. Compute score matrix (directement en une seule fois)
  NumericMatrix Scores = UncertaintyUtils::absolute_residual_scores_cpp(y_pred_calib, Y_calibration);

  // 2. Sort scores
  NumericMatrix sorted_S = UncertaintyUtils::get_sorted_scores_cpp(Scores);

  NumericVector quantiles;
  double Beta_optim = NA_REAL;
  int r_max = -1;

  // ========================================================
  // Method: Beta_Optim
  // ========================================================
  if (uncertainty_method == "Beta_Optim") {
    auto Eval_simultaneous_coverage_beta = [&](double Beta) -> double {
      NumericVector Beta_quantile = UncertaintyUtils::get_Beta_quantiles_cpp(sorted_S, Beta);
      List bounds = UncertaintyUtils::get_prediction_bounds_cpp(Beta_quantile, y_pred_calib);
      NumericMatrix y_lower = bounds["y_lower"];
      NumericMatrix y_upper = bounds["y_upper"];
      return UncertaintyUtils::simultaneous_coverage_cpp(Y_calibration, y_lower, y_upper);
    };

    // Dichotomie
    double a = 0.0, b = Global_alpha, epsilon = 1e-6;
    int n_iter = 10;

    // Validation des bornes de la dichotomie
    if (b <= a) {
      stop("Global_alpha must be greater than 0 and smaller than 1.");
    }

    for (int i = 0; i < n_iter; ++i) {
      double c = (a + b) / 2.0;
      double coverage = Eval_simultaneous_coverage_beta(c);
      if (std::abs(b - a) < epsilon) break;
      if (coverage > 1 - Global_alpha) {
        a = c;
      } else {
        b = c;
      }
    }

    Beta_optim = (a + b) / 2.0;
    quantiles = UncertaintyUtils::get_Beta_quantiles_cpp(sorted_S, Beta_optim);

    // Affichage final propre
    List bounds = UncertaintyUtils::get_prediction_bounds_cpp(quantiles, y_pred_calib);
    NumericMatrix y_lower = bounds["y_lower"];
    NumericMatrix y_upper = bounds["y_upper"];
    double final_coverage = UncertaintyUtils::simultaneous_coverage_cpp(Y_calibration, y_lower, y_upper);

    Rcpp::Rcout << "Optimal Beta Found = " << Beta_optim << " => Coverage = " << final_coverage << "\n";
  }
  // ========================================================
  // Method 2: Max_Rank
  // ========================================================
  else if (uncertainty_method == "Max_Rank") {
    NumericMatrix rank_matrix = UncertaintyUtils::column_wise_rank_cpp(Scores);
    NumericVector R_max = UncertaintyUtils::R_max_vector_cpp(rank_matrix);

    r_max = UncertaintyUtils::get_r_max_scalar_cpp(R_max, Global_alpha);
    quantiles = UncertaintyUtils::max_rank_quantiles_cpp(r_max, sorted_S);

    Rcpp::Rcout << "Max Rank method fitted. r_max = " << r_max << "\n";
  }

  // ========================================================
  // Method 3: Fast_Optim
  // ========================================================
  else if (uncertainty_method == "Fast_Optim") {
    NumericMatrix rank_matrix = UncertaintyUtils::column_wise_rank_cpp(Scores);
    NumericVector R_max = UncertaintyUtils::R_max_vector_cpp(rank_matrix);

    // Fonction pour calculer la couverture pour un Beta donné
    auto Eval_simultaneous_coverage_beta_fast_optim = [&](double Beta) -> double {
      double coverage = UncertaintyUtils::simultaneous_coverage_bis_cpp(R_max, Beta);
      return coverage;
    };

    double a = 0.0, b = Global_alpha, epsilon = 1e-6;
    int n_iter = 10;

    // Validation des bornes de la dichotomie pour Fast_Optim
    if (b <= a) {
      stop("Global_alpha must be greater than 0 and smaller than 1.");
    }

    // Effectuer une recherche dichotomique pour trouver le Beta optimal
    for (int i = 0; i < n_iter; ++i) {
      double c = (a + b) / 2.0;
      double coverage = Eval_simultaneous_coverage_beta_fast_optim(c);

      // Si la différence est suffisamment petite, on arrête
      if (std::abs(b - a) < epsilon) break;

      // Ajuster les bornes en fonction de la couverture
      if (coverage > 1 - Global_alpha) {
        a = c;
      } else {
        b = c;
      }
    }

    Beta_optim = (a + b) / 2.0;

    // Calculer la couverture finale pour Beta_optim
    double final_coverage_fast = Eval_simultaneous_coverage_beta_fast_optim(Beta_optim);

    // Affichage de beta optimal ainsi que la couverture simultanée.
    Rcpp::Rcout << "Optimal Beta found: " << Beta_optim << "\n" << "Final Coverage for Beta_optim: " << final_coverage_fast << "\n";


    // Obtenir les quantiles pour Beta_optim
    quantiles = UncertaintyUtils::get_Beta_quantiles_cpp(sorted_S, Beta_optim);
  }


  // ========================================================
  // Catch unknown method
  // ========================================================
  else {
    stop("Unknown uncertainty method. Available methods: Beta_Optim, Max_Rank, Fast_Optim.");
  }

  return List::create(
    Named("quantiles") = quantiles,
    Named("Beta_optim") = Beta_optim,
    Named("r_max") = r_max
  );
}

// [[Rcpp::export]]
List predict_cpp(NumericMatrix y_pred_test, NumericVector quantiles) {
  // Vérification que quantiles n'est pas vide
  if (quantiles.size() == 0) {
    stop("Quantiles vector cannot be empty.");
  }

  return UncertaintyUtils::get_prediction_bounds_cpp(quantiles, y_pred_test);
}
