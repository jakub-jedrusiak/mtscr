# Utility Functions for MTSCR Shiny App ----

## Data Validation Functions ----

#' Validate data for MTS/STS analysis
#' @param data The input data frame
#' @param id_col The ID column name
#' @param score_col The score column name
#' @param item_col The item column name (optional)
#' @return List with validation result and message
validate_data <- function(data, id_col, score_col, item_col = NULL) {
    if (is.null(data) || nrow(data) == 0) {
        return(list(valid = FALSE, message = "No data provided"))
    }

    if (!id_col %in% colnames(data)) {
        return(list(valid = FALSE, message = "ID column not found in data"))
    }

    if (!score_col %in% colnames(data)) {
        return(list(valid = FALSE, message = "Score column not found in data"))
    }

    if (!is.null(item_col) && !item_col %in% colnames(data)) {
        return(list(valid = FALSE, message = "Item column not found in data"))
    }

    if (!is.numeric(data[[score_col]])) {
        return(list(valid = FALSE, message = "Score column must be numeric"))
    }

    return(list(valid = TRUE, message = "Data validation passed"))
}

#' Check if data has sufficient observations for analysis
#' @param data The input data frame
#' @param min_obs Minimum number of observations required
#' @return List with check result and message
check_sufficient_data <- function(data, min_obs = 10) {
    if (nrow(data) < min_obs) {
        return(list(
            sufficient = FALSE,
            message = paste("Warning: Only", nrow(data), "observations. Consider having at least", min_obs, "for reliable results.")
        ))
    }

    return(list(sufficient = TRUE, message = "Sufficient data for analysis"))
}

## Model Result Processing Functions ----

#' Format model summary for display
#' @param model The fitted model object
#' @param model_type The type of model ("MTS" or "STS")
#' @return Formatted data frame for display
format_model_summary <- function(model, model_type = "MTS") {
    if (model_type == "STS" && is.list(model) && !is.null(model$minimal)) {
        # For STS models, use rstatix summary stats on top columns
        summary_df <- rstatix::get_summary_stats(
            model$minimal,
            dplyr::matches("^top\\d+$")
        )
    } else {
        # For MTS models, use standard summary
        summary_df <- summary(model)
    }

    # Add any formatting here if needed
    # For example, rounding numeric columns
    numeric_cols <- sapply(summary_df, is.numeric)
    summary_df[numeric_cols] <- lapply(summary_df[numeric_cols], function(x) round(x, 4))

    return(summary_df)
}

#' Create downloadable filename with timestamp
#' @param prefix The filename prefix
#' @param extension The file extension (without dot)
#' @return Formatted filename string
create_filename <- function(prefix, extension) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    paste0(prefix, "_", timestamp, ".", extension)
}

## UI Helper Functions ----

#' Create a styled info box
#' @param title The box title
#' @param content The box content
#' @param status The box status (primary, info, success, warning, danger)
#' @return Shiny UI element
create_info_box <- function(title, content, status = "info") {
    div(
        class = paste("alert alert", status, sep = "-"),
        tags$strong(title),
        br(),
        content
    )
}

#' Create a loading spinner
#' @param id The spinner ID
#' @return Shiny UI element
create_loading_spinner <- function(id = "loading-spinner") {
    div(
        id = id,
        class = "text-center",
        style = "margin: 20px;",
        tags$i(class = "fa fa-spinner fa-spin fa-2x"),
        br(),
        "Processing..."
    )
}

## Error Handling Functions ----

#' Safe model generation with error handling
#' @param model_func The model generation function
#' @param inputs The input parameters
#' @return List with success status, model, and message
safe_model_generation <- function(model_func, inputs) {
    tryCatch(
        {
            # Validate inputs first
            validation <- validate_data(
                inputs$data,
                inputs$id_col,
                inputs$score_col,
                inputs$item_col
            )

            if (!validation$valid) {
                return(list(
                    success = FALSE,
                    model = NULL,
                    message = validation$message
                ))
            }

            # Check data sufficiency
            data_check <- check_sufficient_data(inputs$data)
            warning_msg <- if (!data_check$sufficient) data_check$message else NULL

            # Generate model
            model <- model_func(inputs)

            return(list(
                success = TRUE,
                model = model,
                message = "Model generated successfully",
                warning = warning_msg
            ))
        },
        error = function(e) {
            return(list(
                success = FALSE,
                model = NULL,
                message = paste("Error:", e$message)
            ))
        }
    )
}
