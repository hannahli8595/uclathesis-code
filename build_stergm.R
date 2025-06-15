##############################################
# Build STERGM - School/Class Pipeline
##############################################

#' Convert Array of Adjacency Matrices and Attributes to List of Network Objects
#'
#' For each wave, constructs a statnet/network object from an adjacency array
#' and assigns vertex attributes (including handling of haven_labelled types).
#'
#' @param friend_array 3D array: [students x students x waves], adjacency for each wave.
#' @param attributes Data frame: student attributes (must include "namenr").
#' @return Named list of network objects, one per wave, with all vertex attributes set.
array_to_network_list <- function(friend_array,
                                  attributes) {
  common_students <- rownames(friend_array[, , 1])
  attributes <- attributes %>%
    mutate(namenr = as.character(namenr)) %>%
    filter(namenr %in% common_students) %>%
    arrange(match(namenr, common_students))
  
  network_list <- vector("list", dim(friend_array)[3])
  
  for (t in seq_len(dim(friend_array)[3])) {
    
    net <- network(friend_array[, , t], directed = TRUE)
    
    for (v in setdiff(colnames(attributes), "namenr")) {
      vec <- attributes[[v]]
      # Coerce haven_labelled to base numeric/factor
      if (inherits(vec, "haven_labelled")) {
        vec <- as.numeric(vec)        # for truly numeric covariates
      }
      vec <- as.vector(unclass(vec))
      # Assignment check
      message("→ assigning “", v, "”: class=", paste(class(vec), collapse="/"),
              ", typeof=", typeof(vec),
              ", length=", length(vec),
              ", network.size=", network.size(net))
      network::set.vertex.attribute(net, v, vec)
    }
    
    network_list[[t]] <- net
  }
  names(network_list) <- paste0("Wave_", seq_len(dim(friend_array)[3]))
  network_list
}

#' Run STERGM for All Classes in a School (with Imputation)
#'
#' For each class in the school, imputes missing data (simple or MICE),
#' prepares adjacency and attribute data, builds statnet network objects,
#' fits a STERGM with supplied formation/dissolution formulas, and saves results.
#'
#' @param PupilsV,PupilsW,PupilsX,PupilsY Data frames for waves V, W, X, Y.
#' @param specified_school_id Character or numeric: school identifier.
#' @param formation Formula for STERGM formation model (e.g., ~edges + mutual + ...).
#' @param dissolution Formula for STERGM dissolution model.
#' @param output_folder Directory where results and diagnostics will be saved (default "stergm_results").
#' @param subset_classes Optional character vector of class IDs to run (default = all).
#' @param method "simple" or "mice" (must supply output_folder for "mice").
#' @return Named list of fitted STERGM models, keyed by class ID.
run_stergm_for_school_custom <- function(
    PupilsV, PupilsW, PupilsX, PupilsY,
    specified_school_id,
    formation,
    dissolution,
    output_folder      = "stergm_results",
    subset_classes     = NULL,
    method             = "simple"
) {
  if (!dir.exists(output_folder)) dir.create(output_folder, recursive=TRUE)
  
  # Impute data and split by class (across all waves)
  class_arrays <- process_all_classes_for_school(
    PupilsV, PupilsW, PupilsX, PupilsY,
    method        = method,
    output_folder = output_folder
  )
  
  # Subset classes if needed
  if (!is.null(subset_classes)) {
    class_arrays <- class_arrays[names(class_arrays) %in% subset_classes]
  }
  
  stergm_results <- list()
  for (cid in names(class_arrays)) {
    cat("\n Running STERGM for class", cid, "(School", specified_school_id, ")…\n")
    cas <- class_arrays[[cid]]
    cd  <- cas$pupils_data
    fl  <- cas$friendship_matrix
    
    # Friendship array [students x students x waves]
    friend_array <- adj_matrices_to_array(fl, wave_prefix = "")
    
    # Use Wave V attributes for all covariates
    Vdf   <- cd$V %>% mutate(namenr = as.character(namenr))
    tvec  <- compute_trust_composite(Vdf)
    
    # Calculate total delinquency (sum across acts), NAs → 0 for ergm stability
    del_list   <- create_adjacency_and_behavior_matrices(cd$V)$delinquency
    del_index  <- rowSums(del_list, na.rm=TRUE)
    
    attrs <- Vdf %>%
      mutate(
        trust       = tvec,
        actsteal    = ifelse(is.na(del_list$actsteal), 0, del_list$actsteal),
        actsmear    = ifelse(is.na(del_list$actsmear), 0, del_list$actsmear),
        actbreak    = ifelse(is.na(del_list$actbreak), 0, del_list$actbreak),
        actfight    = ifelse(is.na(del_list$actfight), 0, del_list$actfight),
        delinquency = ifelse(is.na(del_index),      0, del_index)
      ) %>%
      select(namenr, sex, money, age, advice, siblings, trust,
             opdrug, opalcoh, opattent, actsteal, actsmear, actbreak, actfight, 
             delinquency)
    
    # Build network object list for all four waves
    netlist <- array_to_network_list(friend_array, attrs)
    
    #  Fit the STERGM
    model <- tergm(
      netlist ~ Form(formation) + Diss(dissolution),
      estimate    = "CMLE",
      times = 0:3,
      control  = control.tergm(
        CMLE.ergm = control.ergm(
          MCMC.burnin    = 1000,
          MCMC.interval  = 500,
          MCMC.samplesize= 6500
        )
      )
    )
    
    # Save model + diagnostics PDF
    save(model, file = file.path(output_folder, paste0("STERGM_", specified_school_id, "_", cid, ".RData")))
    pdf(file.path(output_folder, paste0("STERGM_", specified_school_id, "_", cid, "_Diagnostics.pdf")),
        width=8, height=6)
    mcmc.diagnostics(model)
    dev.off()
    
    cat("Saved STERGM + diagnostics for class", cid, "\n")
    
    stergm_results[[cid]] <- model
  }
  
  invisible(stergm_results)
}
