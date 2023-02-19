unik <- function(df) {
  suppressWarnings({
    # numeric columns
    cat("#Numeric columns with 2 unique values:\n")
    numeric2 <- sapply(df, function(x) is.numeric(x) & length(unique(x)) == 2)
    if (sum(numeric2) == 0) {
      cat("None\n")
    } else {
      for (col in names(numeric2)[numeric2]) {
        cat(col, ": ", paste0(sort(unique(df[[col]])), collapse = "  "), "\n")
      }
    }

    cat("\n#Numeric columns with 3 unique values:\n")
    numeric3 <- sapply(df, function(x) is.numeric(x) & length(unique(x)) == 3)
    if (sum(numeric3) == 0) {
      cat("None\n")
    } else {
      for (col in names(numeric3)[numeric3]) {
        cat(col, ": ", paste0(sort(unique(df[[col]])), collapse = "  "), "\n")
      }
    }

    cat("\n#Numeric columns with 4 unique values:\n")
    numeric4 <- sapply(df, function(x) is.numeric(x) & length(unique(x)) == 4)
    if (sum(numeric4) == 0) {
      cat("None\n")
    } else {
      for (col in names(numeric4)[numeric4]) {
        cat(col, ": ", paste0(sort(unique(df[[col]])), collapse = "  "), "\n")
      }
    }

    cat("\n#Numeric columns with 5 unique values:\n")
    numeric5 <- sapply(df, function(x) is.numeric(x) & length(unique(x)) == 5)
    if (sum(numeric5) == 0) {
      cat("None\n")
    } else {
      for (col in names(numeric5)[numeric5]) {
        cat(col, ": ", paste0(sort(unique(df[[col]])), collapse = "  "), "\n")
      }
    }

    cat("\n#Numeric columns with >5 non-decimal unique values:\n")
    numeric_more <- sapply(df, function(x) is.numeric(x) & length(unique(x)) > 5 & all(x %% 1 == 0))
    if (sum(numeric_more) == 0) {
      cat("None\n")
    } else {
      for (col in names(numeric_more)[numeric_more]) {
        cat(col, "\n")
      }
    }

    cat("\n#Numeric columns with decimal values:\n")
    numeric_decimal <- sapply(df, function(x) is.numeric(x) & any(grepl("\\.", as.character(x))))
    if (sum(numeric_decimal) == 0) {
      cat("None\n")
    } else {
      for (col in names(numeric_decimal)[numeric_decimal]) {
        cat(col, " (mean=", mean(df[[col]]), ", min=", min(df[[col]]), ", max=", max(df[[col]]), ")\n", sep="")
      }
    }


    cat("\n#Factor columns:\n")
    factor_cols <- sapply(df, function(x) is.factor(x))
    if (sum(factor_cols) == 0) {
      cat("None\n")
    } else {
      for (col in names(factor_cols)[factor_cols]) {
        cat(col, ": ", paste0(levels(df[[col]]), collapse = "  "), "\n")
      }
    }
  })
}
