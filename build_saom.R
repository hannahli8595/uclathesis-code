##############################################
# Build SAOM
##############################################

#' Prepare RSiena Data for a Class (Friendship + Delinquency SAOM)
#'
#' Constructs the RSiena sienaData object for modeling, combining
#' friendship and delinquency matrices (across 4 waves) and actor covariates.
#'
#' @param class_arrays List: Output of `process_all_classes_for_school` (contains matrices and data for each class).
#' @param class_id Character: The class ID to prepare.
#' @param output_folder Optional: Directory for output/logs (not used directly).
#' @return A `sienaData` object, ready for RSiena modeling.
prepare_siena_data_for_class <- function(class_arrays, class_id, output_folder = NULL) {
  # Extract per-class data
  class_data_list <- class_arrays[[class_id]]$pupils_data
  friend_list <- class_arrays[[class_id]]$friendship_matrix
  delinquency_list <- class_arrays[[class_id]]$delinquency_matrix
  
  friend_array <- adj_matrices_to_array(friend_list, wave_prefix = "Wave")
  delinquency_array <- adj_matrices_to_array(delinquency_list, wave_prefix = "Wave")
  
  Pupils_set <- sienaNodeSet(dim(friend_array)[1], nodeSetName = "Pupils", names = rownames(friend_array))
  Behaviors_set <- sienaNodeSet(dim(delinquency_array)[2], nodeSetName = "DelinquentBehaviors", names = colnames(delinquency_array))
  
  # Ensure consistent student ordering across waves
  ordered_ids <- class_data_list$V$namenr
  class_data_list <- lapply(class_data_list, function(df) df[match(ordered_ids, df$namenr), ])
  
  class_data_list <- lapply(class_data_list, function(df) clean_and_cast_variables(df, var_types))
  
  # --- Time-varying matrices ---
  timevars <- c("money", "isdrug", "isalcoh", "issmoke", "opalcoh", "opdrug", "opattent")
  
  timevar_matrices <- setNames(lapply(timevars, function(var) {
    do.call(cbind, lapply(class_data_list, function(df) df[[var]]))
  }), paste0(timevars, "_matrix"))
  
  trust_matrix_all <- do.call(cbind, lapply(class_data_list, compute_trust_composite))
  
  # --- Build sienaData Object ---
  siena_data <- sienaDataCreate(
    nodeSets = list(Pupils_set, Behaviors_set),
    
    # Network dependent variables
    friendship_siena = sienaDependent(friend_array, type = "oneMode", nodeSet = "Pupils"),
    delinquency_siena = sienaDependent(delinquency_array, type = "bipartite", nodeSet = c("Pupils", "DelinquentBehaviors")),
    
    # Actor constant covariates
    advice_siena = coCovar(as.numeric(class_data_list$V$advice), nodeSet = "Pupils"),
    age_siena = coCovar(as.numeric(class_data_list$V$age), nodeSet = "Pupils"),
    siblings_siena = coCovar(as.numeric(class_data_list$V$siblings), nodeSet = "Pupils"),
    relfath_siena = coCovar(as.numeric(class_data_list$X$relfath), nodeSet = "Pupils"),
    relmoth_siena = coCovar(as.numeric(class_data_list$X$relmoth), nodeSet = "Pupils"),

    # Actor time-varying covariates (3 periods)
    money_siena = varCovar(timevar_matrices$money_matrix[,1:3], nodeSet = "Pupils"),
    isdrug_siena = varCovar(timevar_matrices$isdrug_matrix[,1:3], nodeSet = "Pupils"),
    isalcoh_siena = varCovar(timevar_matrices$isalcoh_matrix[,1:3], nodeSet = "Pupils"),
    issmoke_siena = varCovar(timevar_matrices$issmoke_matrix[,1:3], nodeSet = "Pupils"),
    
    opdrug_siena = varCovar(timevar_matrices$opdrug_matrix[,1:3], nodeSet = "Pupils"),
    opalcoh_siena = varCovar(timevar_matrices$opalcoh_matrix[,1:3], nodeSet = "Pupils"),
    opattent_siena = varCovar(timevar_matrices$opattent_matrix[,1:3], nodeSet = "Pupils"),
    
    # Trust composite (time-varying)
    trust_siena = varCovar(trust_matrix_all[,1:3], nodeSet = "Pupils")
  )
  
  return(siena_data)
}

#' Run RSiena SAOM Models for All Classes in a School
#'
#' For each class in the specified school, this function imputes missing data (via the chosen method),
#' constructs a RSiena sienaData object, sets up model effects, runs the model, saves results, 
#' and finally runs diagnostic/GOF routines for all classes.
#'
#' @param PupilsV,PupilsW,PupilsX,PupilsY Data frames for each wave.
#' @param specified_school_id Character: School ID.
#' @param effect_function Function: User-supplied function to define effects for RSiena.
#' @param output_folder Character: Directory to save all results (default = "saom_results").
#' @param subset_classes Optional: Character vector of class IDs to subset.
#' @param method Character: Imputation method ("simple" or "mice").
#' @return None (side effect: saves results and plots to output_folder; returns invisible list of model fits).
run_saom_for_school_custom <- function(PupilsV, PupilsW, PupilsX, PupilsY, 
                                       specified_school_id, 
                                       effect_function, 
                                       output_folder = NULL, 
                                       subset_classes = NULL,
                                       method = "simple") {
  if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
  
  # Impute and structure data per class
  class_arrays <- process_all_classes_for_school(PupilsV, PupilsW, PupilsX, PupilsY, 
                                                 method = method,
                                                 output_folder = output_folder)
  
  # Optionally filter by class
  if (!is.null(subset_classes)) {
    class_arrays <- class_arrays[names(class_arrays) %in% subset_classes]
  }
  
  processed_results <- list()
  
  # Loop through each class and run SAOM
  for (class_id in names(class_arrays)) {
    cat("Processing SAOM for Class", class_id, "\n")
    
    # Create RSiena data object
    siena_data <- prepare_siena_data_for_class(class_arrays, class_id, output_folder)
    
    # Define effects
    effects <- effect_function(siena_data)
    
    # Set algorithm parameters
    alg <- sienaAlgorithmCreate(
      projname = paste0("SAOM_", specified_school_id, "_", class_id),
      n3 = 5000
    )
    
    # Run the model
    result <- siena07(
      alg, 
      data = siena_data, 
      effects = effects, 
      batch = TRUE, 
      returnDeps = TRUE, 
      silent = TRUE, 
      verbose = FALSE
    )
    
    # Save results
    save(result, file = file.path(output_folder, paste0("SAOM_", specified_school_id, "_", class_id, ".RData")))
    processed_results[[class_id]] <- result
    cat("SAOM completed for Class", class_id, "\n")
  }
  
  # Run convergence and GOF diagnostics (plots saved)
  run_saom_diagnostics(processed_results, output_folder = output_folder)
  cat("All SAOM models completed for School", specified_school_id, "\n")
}

