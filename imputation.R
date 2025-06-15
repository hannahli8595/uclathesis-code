##############################################
# Imputation - Simple and MICE
##############################################

# ---- Variable type vectors (for use in imputation) ----

#' Continuous numeric variables (to be mean-imputed or MICE-pmm)
numeric_continuous_vars <- c("money")

#' Discrete numeric variables (to be median-imputed or MICE-pmm, then rounded)
numeric_discrete_vars <- c("age", "siblings")

#' Unordered categorical variables (to be mode-imputed or MICE-polyreg)
categorical_unordered_vars <- c(
  "place1", "place2", "workfa1", "workmo1", "relfath", "relmoth", "amoufria", "impofria"
)

#' Ordered categorical variables (Likert or collapsed multiple-choice)
categorical_ordered_vars <- c(
  "advice", "smokemum", "smokedad", "smokesib",
  "opalcoh", "opdrug", paste0("opsmoke", 1:3), "oplie", "opattent", "actatten",
  "opmebest", "opmestea", "oppabest", "oppastea",
  "rmeatteb", "rmeatteg", "rmenattb", "rmenattg",
  "rpaatteb", "rpaatteg", "rpanattb", "rpanattg",
  paste0("trust", 1:8),
  paste0("bigfiv", 1:20),
  "actcloth", "opsamecl",
  # Collapsed variables (e.g., 1-8 options, now as ordered single var)
  "isdrug", "islie", "isalcoh", "issmoke", "issteal", "isatte", "isnatt", "isnerd"
)

#' Binary variables (to be mode-imputed or MICE-logreg)
binary_vars <- c(
  "sex",
  "hobby10r", "hobby2lm", "hobby3sh", "hobby4sp",
  "hobby5cg", "hobby6ou", "hobby7go", "hobby8tv", "hobby9dr"
)

#' Calculate the statistical mode
#'
#' @param x A vector (numeric, character, or factor).
#' @return The mode (most common value).
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Simple Per-Class Imputation
#'
#' For each class, impute missing values using mean (continuous), median (discrete), or mode (categorical/binary).
#'
#' @param data Data frame for a single wave (must include class_id).
#' @return Named list of imputed data frames (one per class).
handle_missing_simple <- function(data) {
  class_ids <- unique(data$class_id)
  imputed_class_data <- list()
  
  for (specified_class_id in class_ids) {
    class_data <- data %>% filter(class_id == specified_class_id)
    
    # Impute numeric continuous (mean)
    for (col in intersect(numeric_continuous_vars, names(class_data))) {
      mean_value <- mean(class_data[[col]], na.rm = TRUE)
      class_data[[col]][is.na(class_data[[col]])] <- mean_value
    }
    # Impute numeric discrete (median, rounded)
    for (col in intersect(numeric_discrete_vars, names(class_data))) {
      median_value <- round(median(class_data[[col]], na.rm = TRUE))
      class_data[[col]][is.na(class_data[[col]])] <- median_value
    }
    # Impute unordered categorical (mode)
    for (col in intersect(categorical_unordered_vars, names(class_data))) {
      mode_value <- Mode(class_data[[col]])
      class_data[[col]][is.na(class_data[[col]])] <- mode_value
    }
    # Impute ordered categorical (mode)
    for (col in intersect(categorical_ordered_vars, names(class_data))) {
      mode_value <- Mode(class_data[[col]])
      class_data[[col]][is.na(class_data[[col]])] <- mode_value
    }
    # Impute binary (mode)
    for (col in intersect(binary_vars, names(class_data))) {
      mode_value <- Mode(class_data[[col]])
      class_data[[col]][is.na(class_data[[col]])] <- mode_value
    }
    
    imputed_class_data[[specified_class_id]] <- class_data
  }
  
  return(imputed_class_data)
}

#' Per-Class Multiple Imputation by Chained Equations (MICE) Imputation
#'
#' Performs robust MICE imputation for each class, with safety checks for sparse or degenerate variables.
#'
#' @param data Data frame for one wave (must include class_id).
#' @param output_folder Directory for any outputs (optional, for logging).
#' @return Named list of imputed data frames (one per class).
handle_missing_mice <- function(data, output_folder) {
  imputed_class_data <- list()
  
  for (cid in unique(data$class_id)) {
    cat("\n", glue("Processing class {cid}..."), sep = "")
    class_data <- data %>% filter(class_id == cid)
    
    # Skip tiny classes (under 5)
    if (nrow(class_data) < 5) {
      cat("\n Skipping (too few rows).")
      imputed_class_data[[cid]] <- class_data
      next
    }
    
    # Pre-impute all numeric (continuous + discrete) with median to avoid MICE dropping them
    for (v in intersect(union(numeric_continuous_vars, numeric_discrete_vars),
                        names(class_data))) {
      med <- median(class_data[[v]], na.rm=TRUE)
      class_data[[v]][is.na(class_data[[v]])] <- med
    }
    
    # Select relevant variables
    vars <- intersect(c(numeric_continuous_vars, numeric_discrete_vars,
                        categorical_unordered_vars, categorical_ordered_vars, binary_vars),
                      names(class_data))
    df <- class_data[, vars, drop=FALSE]
    
    # Enforce factor/ordered types
    for (v in intersect(categorical_unordered_vars, names(df)))
      df[[v]] <- factor(df[[v]])
    for (v in intersect(categorical_ordered_vars, names(df)))
      df[[v]] <- ordered(df[[v]])
    for (v in intersect(binary_vars, names(df)))
      df[[v]] <- factor(df[[v]])
    
    # Drop sparse/degenerate ordered categorical (impute with mode, then remove from MICE)
    to_drop <- c()
    for (v in intersect(categorical_ordered_vars, names(df))) {
      cnt <- table(droplevels(df[[v]]))
      if (length(cnt) < 3 || any(cnt < 5)) {
        mv <- names(which.max(cnt))
        cat("\n", glue("{v} too sparse, mode→{mv} + dropping from MICE"), sep = "")
        class_data[[v]] <- as.character(class_data[[v]])
        class_data[[v]][is.na(class_data[[v]])] <- mv
        class_data[[v]] <- factor(class_data[[v]], levels=names(cnt))
        to_drop <- c(to_drop, v)
      }
    }
    if (length(to_drop)) df <- df[, setdiff(names(df), to_drop), drop=FALSE]
    
    # Drop degenerate categorical columns (all NAs or only one level)
    cat_vars <- intersect(names(df),
                          c(categorical_unordered_vars,
                            categorical_ordered_vars,
                            binary_vars))
    deg <- cat_vars[sapply(cat_vars, function(v) {
      length(unique(na.omit(df[[v]]))) <= 1
    })]
    if (length(deg)) {
      cat("Dropping degenerate categorical vars:", paste(deg, collapse=", "), "\n")
      df <- df[, setdiff(names(df), deg), drop=FALSE]
    }
    if (!ncol(df)) {
      cat("\n NOTHING to impute → skip.")
      imputed_class_data[[cid]] <- class_data
      next
    }
    
    # Remember original types for fallback
    orig_type <- sapply(df, function(x) {
      if (is.ordered(x))   "ordered"
      else if (is.factor(x)) "factor"
      else if (is.numeric(x)) "numeric"
      else class(x)[1]
    })
    
    # Set up MICE methods and predictor matrix
    meth <- rep("", ncol(df)); names(meth) <- names(df)
    for (v in intersect(numeric_continuous_vars, names(df))) meth[v] <- "pmm"
    for (v in intersect(numeric_discrete_vars,   names(df))) meth[v] <- "pmm"
    for (v in intersect(categorical_unordered_vars, names(df))) meth[v] <- "polyreg"
    for (v in intersect(binary_vars,             names(df))) meth[v] <- "logreg"
    for (v in intersect(categorical_ordered_vars,   names(df))) meth[v] <- "polr"
    pm <- quickpred(df, mincor=0.05, minpuc=0.25)
    
    # Try MICE, fallback to type-aware mode/median if MICE fails
    cat("\n Starting MICE...")
    tryCatch({
      imp <- mice(df, m=1, maxit=5, method=meth, predictorMatrix=pm,
                  seed=123, printFlag=FALSE)
      cat("\n MICE done.")
      
      comp <- complete(imp)
      
      # Round discrete
      for (v in intersect(numeric_discrete_vars, names(comp))) {
        cat("\n", glue("Rounding: {v}"), sep = "")
        comp[[v]] <- round(comp[[v]])
      }
      class_data[, names(comp)] <- comp
    }, error = function(e) {
      cat("\n", glue("MICE failed: {e$message}"), sep = "")
      for (v in names(df)) {
        miss_idx <- which(is.na(class_data[[v]]))
        if (!length(miss_idx)) next
        if (orig_type[v] == "numeric") {
          med <- median(class_data[[v]], na.rm=TRUE)
          class_data[[v]][miss_idx] <- med
        } else {
          tb <- table(class_data[[v]])
          mv <- names(which.max(tb))
          tmp <- as.character(class_data[[v]])
          tmp[miss_idx] <- mv
          if (orig_type[v] == "ordered")
            class_data[[v]] <- ordered(tmp, levels=levels(df[[v]]))
          else
            class_data[[v]] <- factor(tmp, levels=levels(df[[v]]))
        }
      }
    })
    
    imputed_class_data[[cid]] <- class_data
    cat("\n", glue("Class {cid} done."), sep = "")
  }
  return(imputed_class_data)
}
