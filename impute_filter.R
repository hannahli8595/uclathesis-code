##############################################
# Impute & Filter Classes, All Waves
##############################################

#' Unified Missing Data Handler for All Waves
#'
#' Imputes missing values for each wave's data frame using either "simple" or "mice" imputation methods.
#' Returns a named list (V, W, X, Y), where each element is a class-level list of imputed data frames.
#'
#' @param PupilsV Data frame for wave V.
#' @param PupilsW Data frame for wave W.
#' @param PupilsX Data frame for wave X.
#' @param PupilsY Data frame for wave Y.
#' @param method Character, either "simple" or "mice".
#' @param output_folder Optional directory for MICE outputs (required if method = "mice").
#' @return Named list with elements V, W, X, Y; each is a list of imputed data frames per class.
handle_missing_all_waves <- function(PupilsV, PupilsW, PupilsX, PupilsY, 
                                     method = "simple",
                                     output_folder = NULL) {
  if (method == "mice") {
    if (is.null(output_folder)) {
      stop("you must supply `output_folder` when using method = 'mice'")
    }
    impute_fun <- function(dat) {
      handle_missing_mice(dat, output_folder = output_folder)
    }
  } else {
    impute_fun <- handle_missing_simple
  }
  list(
    V = impute_fun(PupilsV),
    W = impute_fun(PupilsW),
    X = impute_fun(PupilsX),
    Y = impute_fun(PupilsY)
  )
}

#' Impute, Filter, and Aggregate Class Data for a School (All Waves)
#'
#' For a specified school, imputes missing values for each wave and aggregates data for all classes,
#' ensuring only students present in all four waves are retained for analysis.
#'
#' @param PupilsV Data frame for wave V.
#' @param PupilsW Data frame for wave W.
#' @param PupilsX Data frame for wave X.
#' @param PupilsY Data frame for wave Y.
#' @param method Character, either "simple" or "mice".
#' @param output_folder Optional directory for MICE outputs (required if method = "mice").
#' @return A named list keyed by class_id. Each entry is a list containing:
#'         $friendship_matrix (list of adjacency matrices per wave),
#'         $delinquency_matrix (list of binary delinquency matrices per wave),
#'         $pupils_data (list of imputed class data per wave).
process_all_classes_for_school <- function(PupilsV, PupilsW, PupilsX, PupilsY, 
                                           method = "simple",
                                           output_folder = NULL) {
  # Impute all waves (returns: list with V, W, X, Y each as a class-level list)
  imputed_waves <- handle_missing_all_waves(PupilsV, PupilsW, PupilsX, PupilsY, 
                                            method = method,
                                            output_folder = output_folder)
  
  PupilsV <- imputed_waves$V
  PupilsW <- imputed_waves$W
  PupilsX <- imputed_waves$X
  PupilsY <- imputed_waves$Y
  
  class_ids <- as.character(unique(names(PupilsV)))
  class_arrays <- list()
  for (specified_class_id in class_ids) {
    # Extract class-level imputed data for each wave
    PupilsV_class <- PupilsV[[specified_class_id]]
    PupilsW_class <- PupilsW[[specified_class_id]]
    PupilsX_class <- PupilsX[[specified_class_id]]
    PupilsY_class <- PupilsY[[specified_class_id]]
    
    # Find students present in all four waves
    common_students <- Reduce(intersect, list(PupilsV_class$namenr, PupilsW_class$namenr, PupilsX_class$namenr, PupilsY_class$namenr))
    PupilsV_class <- PupilsV_class %>% filter(namenr %in% common_students)
    PupilsW_class <- PupilsW_class %>% filter(namenr %in% common_students)
    PupilsX_class <- PupilsX_class %>% filter(namenr %in% common_students)
    PupilsY_class <- PupilsY_class %>% filter(namenr %in% common_students)
    
    if (nrow(PupilsV_class) == 0) next
    # Build adjacency/delinquency matrices for all waves for this class
    friendship_matrix <- list(
      wave_V = create_adjacency_and_behavior_matrices(PupilsV_class)$friendship,
      wave_W = create_adjacency_and_behavior_matrices(PupilsW_class)$friendship,
      wave_X = create_adjacency_and_behavior_matrices(PupilsX_class)$friendship,
      wave_Y = create_adjacency_and_behavior_matrices(PupilsY_class)$friendship
    )
    
    delinquency_matrix <- list(
      wave_V = create_adjacency_and_behavior_matrices(PupilsV_class)$delinquency,
      wave_W = create_adjacency_and_behavior_matrices(PupilsW_class)$delinquency,
      wave_X = create_adjacency_and_behavior_matrices(PupilsX_class)$delinquency,
      wave_Y = create_adjacency_and_behavior_matrices(PupilsY_class)$delinquency
    )
    
    class_arrays[[specified_class_id]] <- list(
      friendship_matrix = friendship_matrix,
      delinquency_matrix = delinquency_matrix,
      pupils_data = list(V = PupilsV_class, W = PupilsW_class, X = PupilsX_class, Y = PupilsY_class)
    )
  }
  class_arrays
}
