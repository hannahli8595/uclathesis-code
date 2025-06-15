##############################################
# Data Preparation & Cleaning Utilities
##############################################

#' Generate Variable Name List for Numbered Variables (e.g., issteal1, issteal2, ...)
#'
#' @param base_v The base name for the first wave (e.g., "issteal").
#' @param base_other The base name for other waves, if different (default = base_v).
#' @param n The number of numbered variables (e.g., n = 8 for issteal1–8).
#' @param varname Optional: Override the prefix for standardized output names.
#' @return A named list of variable names by wave/number.
make_wave_vars_split_numbered_list <- function(base_v, base_other = base_v, n = 1, varname = NULL) {
  vars_list <- lapply(1:n, function(i) {
    c(paste0(base_v, i), paste0(base_other, i, "b"), paste0(base_other, i, "c"), paste0(base_other, i, "d"))
  })
  names(vars_list) <- paste0(ifelse(is.null(varname), base_v, varname), 1:n)
  return(vars_list)
}


#' Compute Composite Trust Score for One Wave
#'
#' @param data Data frame for one wave. Must include trust item columns ("trust1", "trust2", ...).
#' @return Numeric vector of composite trust scores (row means, after reversing specified items).
compute_trust_composite <- function(data) {
  trust_items <- grep("^trust[0-9]+$", names(data), value = TRUE)
  trust_data <- sapply(data[, trust_items, drop = FALSE], as.numeric)
  
  # Specify which items are negatively phrased and need reversing
  reverse_coded_items <- c("trust3", "trust7")  
  
  for (item in reverse_coded_items) {
    if (item %in% colnames(trust_data)) {
      trust_data[, item] <- 6 - trust_data[, item]
    }
  }
  
  rowMeans(trust_data, na.rm = TRUE)
}

#' Filter Wave Data by Class and Keep Students Present in All Four Waves
#'
#' @param PupilsV_raw, PupilsW_raw, PupilsX_raw, PupilsY_raw: Data frames for each wave.
#' @param specified_school_id: The school ID to filter for.
#' @return A named list of filtered data frames (waves V/W/X/Y), by class.
get_filtered_wave_data_by_class <- function(PupilsV_raw, PupilsW_raw, PupilsX_raw, PupilsY_raw, specified_school_id) {
  # Store wave datasets in a named list
  waves <- list(V = PupilsV_raw, W = PupilsW_raw, X = PupilsX_raw, Y = PupilsY_raw)
  
  # Get valid students present in all 4 waves
  valid_students <- get_students_present_all_waves(PupilsV_raw, PupilsW_raw, PupilsX_raw, PupilsY_raw, specified_school_id)
  
  # Filter and split by class for each wave
  filtered_waves <- map(waves, ~ .x %>%
                          filter(school_id == specified_school_id) %>%
                          semi_join(valid_students, by = c("namenr", "class_id")))
  
  return(filtered_waves)
}

##############################################
# Data Cleaning & Standardization
##############################################

#' Process and Clean Data for a Single Wave
#'
#' Loads raw data, splits school/class, standardizes variable names,
#' collapses multiple choice variables, and returns a cleaned data frame.
#'
#' @param file_name Filename for the wave (e.g., "PupilsV.sav").
#' @param data_dir Path to the data directory.
#' @param wave Character: "V", "W", "X", or "Y".
#' @param specified_school_id Optional: restrict to a single school.
#' @return Cleaned data frame for the specified wave and school.
process_data <- function(file_name, data_dir, wave, specified_school_id = NULL) {
  # Helper function: Load SPSS data
  load_data <- function(file_name, data_dir) {
    file_path <- file.path(data_dir, file_name)
    read_sav(file_path)
  }
  
  # Helper function: split schoolnr into school_id and class_id
  split_schoolnr <- function(data, col_name = "schoolnr") {
    data <- data %>%
      mutate(
        !!col_name := trimws(.data[[col_name]]),
        school_id = ifelse(grepl("^[0-9]+[a-zA-Z]$", .data[[col_name]]),
                           sub("[a-zA-Z].*", "", .data[[col_name]]),
                           sub("[^0-9]", "", .data[[col_name]])),
        class_id = ifelse(grepl("^[0-9]+[a-zA-Z]$", .data[[col_name]]),
                          sub("^[0-9]+", "", .data[[col_name]]),
                          sub("[^a-zA-Z]", "", .data[[col_name]]))
      ) %>%
      mutate(
        school_id = as.numeric(school_id),  
        class_id = tolower(class_id),
        class_id = as.factor(class_id)
      )
    return(data)
  }
  
  # Helper: standardize names for current wave using mapping
  # (mapping provided in the function body, omitted here for brevity)
  # Helper: collapse multiple-choice variables
  standardize_wave_names <- function(data, wave) {
    mapping <- c(
      # Demographics
      make_wave_vars_single_list("money"), 
      
      # Delinquent Acts
      make_wave_vars_single_list("actatten","actatte"), 
      make_wave_vars_single_list("actbreak","actbrea"), 
      make_wave_vars_single_list("actfight","actfigh"), 
      make_wave_vars_single_list("actsmear","actsmea"), 
      make_wave_vars_single_list("actsteal","actstea"), 
      
      # Opinions about delinquency
      make_wave_vars_single_list("opalcoh", "opalco"), 
      make_wave_vars_single_list("opbreak"),
      make_wave_vars_single_list("opcopy"),
      make_wave_vars_single_list("opdrug"), 
      make_wave_vars_single_list("opfight"),
      make_wave_vars_split_numbered_list("opsmoke", "opsmok", n = 3), 
      make_wave_vars_single_list("oplie"), 
      make_wave_vars_single_list("opsmear"),
      
      # School norms and behavior
      make_wave_vars_single_list("opattent", "opatte"), 
      
      # Parental/Mentor influence
      make_wave_vars_single_list("opmebest","opmbest"), 
      make_wave_vars_single_list("opmestea","opmstea"), 
      make_wave_vars_single_list("oppabest","oppbest"), 
      make_wave_vars_single_list("oppastea","oppstea"), 
      make_wave_vars_single_list("rmeatteb","rmatteb"), 
      make_wave_vars_single_list("rmeatteg","rmatteg"), 
      make_wave_vars_single_list("rmenattb","rmnattb"), 
      make_wave_vars_single_list("rmenattg","rmnattg"), 
      make_wave_vars_single_list("rpaatteb","rpatteb"), 
      make_wave_vars_single_list("rpaatteg","rpatteg"), 
      make_wave_vars_single_list("rpanattb","rpnattb"), 
      make_wave_vars_single_list("rpanattg","rpnattg"), 
      
      
      # Trust & reciprocity
      make_wave_vars_split_numbered_list("trust", "trust", n = 8),
      
      # Personality (big 5 items)
      make_wave_vars_split_numbered_list("bigfiv", "bigfi", n = 20),
      
      # Peer identity/taste
      make_wave_vars_single_list("actcloth","actclot"),
      make_wave_vars_single_list("opsamecl","opsamec"),
      make_wave_vars_single_list("hobby2lm","hobb2lm"),
      make_wave_vars_single_list("hobby3sh","hobb3sh"),
      make_wave_vars_single_list("hobby4sp","hobb4sp"),
      make_wave_vars_single_list("hobby5cg","hobb5cg"),
      make_wave_vars_single_list("hobby6ou","hobb6ou"),
      make_wave_vars_single_list("hobby7go","hobb7go"),
      make_wave_vars_single_list("hobby8tv","hobb8tv"),
      make_wave_vars_single_list("hobby9dr","hobb9dr"),
      make_wave_vars_single_list("hobby10r","hobb10r"),
      make_wave_vars_single_list("amoufria","amoufri"),
      make_wave_vars_single_list("impofria","impofri") 
    )
    wave_index <- match(wave, c("V", "W", "X", "Y"), nomatch = 1)
    
    for (std_name in names(mapping)) {
      possible_names <- mapping[[std_name]]
      current_name <- ifelse(length(possible_names) >= wave_index, possible_names[wave_index], possible_names[1])
      
      if (current_name %in% names(data)) {
        names(data)[names(data) == current_name] <- std_name
      }
    }
    return(data)
  }
  
  collapse_multiple_choice_vars <- function(df, base_var, n = 8) {
    vars <- paste0(base_var, 1:n)
    vars_present <- vars[vars %in% names(df)]
    if (length(vars_present) > 0) {
      df[[base_var]] <- apply(df[, vars_present], 1, function(row) {
        selected <- which(row == 1)
        if (length(selected) == 1) selected else NA
      })
      df <- df[, !(names(df) %in% vars_present)]
    }
    return(df)
  }
  
  # Load data
  data <- load_data(file_name, data_dir) %>%
    split_schoolnr() 
  
  # Filter school if specified
  if (!is.null(specified_school_id)) {
    data <- data %>% filter(school_id == specified_school_id)
  }
  
  # Standardize variable names
  data <- standardize_wave_names(data, wave)
  data <- standardize_wave_names(data, wave)
  data <- collapse_multiple_choice_vars(data, "isdrug", 8) 
  data <- collapse_multiple_choice_vars(data, "islie", 8) 
  data <- collapse_multiple_choice_vars(data, "isalcoh", 8) 
  data <- collapse_multiple_choice_vars(data, "issmoke", 8) 
  data <- collapse_multiple_choice_vars(data, "issteal", 8) 
  data <- collapse_multiple_choice_vars(data, "isatte", 8) 
  data <- collapse_multiple_choice_vars(data, "isnatt", 8) 
  data <- collapse_multiple_choice_vars(data, "isnerd", 8) 
  
  return(data)
}
##############################################
# Network Construction
##############################################

#' Create Adjacency Matrices for Friendship and Delinquent Behaviors
#'
#' @param class_data Data frame for a single class/wave.
#' @param delinquency_vars Character vector of delinquency variable names.
#' @return List: $friendship (adjacency matrix), $delinquency (matrix/data.frame)
create_adjacency_and_behavior_matrices <- function(class_data, delinquency_vars = c("actsteal", "actsmear", "actbreak", "actfight")) {
  class_data$namenr <- as.character(class_data$namenr)
  
  # Friendship matrix: senders (rows) nominate friends (cols)
  friendship_vars <- grep("^friend[0-9]+$", names(class_data), value = TRUE)
  student_ids <- class_data$namenr
  friend_matrix <- matrix(0, nrow = length(student_ids), ncol = length(student_ids),
                          dimnames = list(student_ids, student_ids))
  for (i in seq_len(nrow(class_data))) {
    sender <- student_ids[i]
    for (var in friendship_vars) {
      nominee <- as.character(class_data[[var]][i])
      if (!is.na(nominee) && nominee != "" && nominee %in% student_ids) {
        friend_matrix[sender, nominee] <- 1
      }
    }
  }
  
  # Delinquency matrix: thresholding applied to create binary indicators
  selected_vars <- delinquency_vars[delinquency_vars %in% names(class_data)]
  
  delinquency_matrix <- class_data[, selected_vars, drop = FALSE] %>% as.data.frame()
  for (var in selected_vars) {
    if (grepl("actfight", var)) {
      delinquency_matrix[[var]] <- ifelse(class_data[[var]] >= 3, 1, 0)
    } else {
      delinquency_matrix[[var]] <- ifelse(class_data[[var]] >= 2, 1, 0)
    }
  }
  rownames(delinquency_matrix) <- class_data$namenr
  
  list(friendship = friend_matrix, delinquency = delinquency_matrix)
}


#' Convert List of Matrices to 3D Array (e.g., for Multiple Waves)
#'
#' @param mat_list Named list of adjacency matrices (one per wave or type).
#' @param wave_prefix Optional prefix for naming.
#' @return 3D array: rows = senders, cols = receivers, slices = waves/types.
adj_matrices_to_array <- function(mat_list, wave_prefix = "Wave") {
  common_ids <- Reduce(intersect, lapply(mat_list, rownames))
  
  is_square <- all(sapply(mat_list, function(mat) nrow(mat) == ncol(mat)))
  
  mat_list <- lapply(mat_list, function(mat) {
    mat <- mat[common_ids, , drop = FALSE]
    if (is_square) {
      mat <- mat[, common_ids, drop = FALSE]
    }
    mat
  })
  
  arr_dim <- if (is_square) c(length(common_ids), length(common_ids), length(mat_list)) else c(length(common_ids), ncol(mat_list[[1]]), length(mat_list))
  arr_dimnames <- if (is_square) list(common_ids, common_ids, names(mat_list)) else list(common_ids, colnames(mat_list[[1]]), names(mat_list))
  
  arr <- array(unlist(mat_list), dim = arr_dim, dimnames = arr_dimnames)
  arr
}

