##############################################
# Diagnostics Functions
##############################################

#' Compute Geodesic Distribution for SAOM GOF
#'
#' Calculates the distribution of geodesic (shortest path) distances for a given network.
#'
#' @param i Index for the current simulation or observed network.
#' @param data, sims, period, groupName, varName Parameters passed from sienaGOF.
#' @param levls Numeric vector: geodesic distance levels to report.
#' @param cumulative Logical: if TRUE, count all distances <= level; if FALSE, exact level only.
#' @return Named vector: geodesic counts for each level.
GeodesicDistribution <- function(i, data, sims, period, groupName, varName, levls = c(1:5, Inf), cumulative = TRUE, ...) {
  x <- networkExtraction(i, data, sims, period, groupName, varName)
  # Symmetrize (undirected) before computing distances
  x_sym <- sna::symmetrize(x)
  a <- sna::geodist(x_sym)$gdist
  if (cumulative) {
    gdi <- sapply(levls, function(lvl) sum(a <= lvl))
  } else {
    gdi <- sapply(levls, function(lvl) sum(a == lvl))
  }
  names(gdi) <- as.character(levls)
  gdi
}

#' Run Diagnostics and Save GOF Plots for RSiena (SAOM) Results
#'
#' Runs convergence diagnostics and goodness-of-fit for each class, saving plots for triad census,
#' geodesic, and outdegree distributions (for both friendship and delinquency).
#'
#' @param results_list Named list of fitted RSiena objects (one per class).
#' @param output_folder Folder where GOF plots are saved (default "saom_results").
#' @param specified_school_id Character or numeric school ID (for filename tags).
#' @return List of diagnostics summaries for each class (convergence, GOF objects).
run_saom_diagnostics <- function(results_list, output_folder = "saom_results") {
  if (!dir.exists(output_folder)) stop("Error: Output folder does not exist.")
  
  if (length(results_list) == 0) {
    cat("No SAOM results found for this school and these classes.\n")
    return(NULL)
  }  
  
  diagnostics_results <- list()
  
  for (class_id in names(results_list)) {
    result <- results_list[[class_id]]  
    cat("\n--- Diagnostics for Class", class_id, "---\n")
    
    max_tconv <- max(abs(result$tconv.max))
    cat("Max T-Convergence =", max_tconv, "\n")
    
    # Compute GOF for each relevant stat
    gof_TriadCensus_friendship <- sienaGOF(result, TriadCensus, varName = "friendship_siena", verbose = FALSE)
    gof_Geodesic_friendship <- sienaGOF(result, GeodesicDistribution, varName = "friendship_siena", verbose = FALSE)
    gof_Outdegree_friendship <- sienaGOF(result, OutdegreeDistribution, varName = "friendship_siena", verbose = FALSE)
    gof_Outdegree_delinquency <- sienaGOF(result, OutdegreeDistribution, varName = "delinquency_siena", verbose = FALSE)
    
    # Set up output filenames
    base_path <- file.path(getwd(), output_folder)
    plot_paths <- list(
      p1 = file.path(base_path, paste0("GOF_TriadCensus_Friendship_", specified_school_id, "_", class_id, ".png")),
      p2 = file.path(base_path, paste0("GOF_GeodesicDistribution_Friendship_", specified_school_id, "_", class_id, ".png")),      
      p3 = file.path(base_path, paste0("GOF_OutdegreeDistribution_Friendship_", specified_school_id, "_", class_id, ".png")),
      p4 = file.path(base_path, paste0("GOF_OutdegreeDistribution_Delinquency_", specified_school_id, "_", class_id, ".png"))
    )
    
    # GOF plotting
    for (info in list(
      list(obj = gof_TriadCensus_friendship, path = plot_paths$p3, title = "Triad Census"),
      list(obj = gof_Geodesic_friendship, path = plot_paths$p4, title = "Geodesic Distribution"),
      list(obj = gof_Outdegree_friendship, path = plot_paths$p5, title = "Outdegree Distribution"),
      list(obj = gof_Outdegree_delinquency, path = plot_paths$p6, title = "Outdegree Distribution")
    )) {
      cat("Saving GOF plot to:", info$path, "\n")
      
      png(info$path, res = 300, width = 8, height = 6, units = "in") 
      plot(info$obj,
           fontsize = 12, 
           position = 1,   
           cex.axis = 1.0,   
           cex.lab = 1.0,   
           cex.main = 0.01  
      )
      
      dev.off()
      Sys.sleep(0.5)
    }
    
    
    if (all(sapply(plot_paths, file.exists))) {
      cat("GOF plots saved for Class", class_id, "\n")
    } else {
      cat("ERROR: GOF plots NOT saved for Class", class_id, "\n")
    }
    
    # Save diagnostic summary
    diagnostics_results[[class_id]] <- list(
      convergence = result$tconv.max,
      gof_TriadCensus_friendship = gof_TriadCensus_friendship,
      gof_Geodesic_friendship = gof_Geodesic_friendship,
      gof_Outdegree_friendship = gof_Outdegree_friendship,
      gof_Outdegree_delinquency = gof_Outdegree_delinquency
    )
    
    cat("Diagnostics completed for Class", class_id, "\n")
  }
  
  cat("\nAll SAOM diagnostics completed for the School", "\n")
  diagnostics_results
}