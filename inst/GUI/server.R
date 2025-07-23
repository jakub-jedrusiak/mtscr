require("shiny")
require("mtscr")
require("dplyr")
require("DT")
require("writexl")
require("datamods")
require("shinyWidgets")
require("rstatix")

# Source modules and functions
source("mts.R", local = TRUE)
source("sts.R", local = TRUE)
source("functions.R", local = TRUE)

# Model Generation Functions ----

## MTS Model Generation ----
generate_mts_model <- function(inputs) {
  # Extract inputs
  data <- inputs$data
  id_col <- inputs$id_col
  item_col <- inputs$item_col
  score_col <- inputs$score_col
  ties_method <- inputs$ties_method
  top <- inputs$top
  normalise <- inputs$normalise
  self_ranking <- inputs$self_ranking

  # Generate MTS model
  model <- mtscr::mtscr(
    data,
    {{ id_col }},
    {{ score_col }},
    {{ item_col }},
    top = top,
    ties_method = ties_method,
    normalise = normalise,
    self_ranking = {{ self_ranking }}
  )

  return(model)
}

## STS Model Generation ----
generate_sts_model <- function(inputs) {
  # Extract inputs
  data <- inputs$data
  id_col <- inputs$id_col
  item_col <- inputs$item_col
  score_col <- inputs$score_col
  top <- inputs$top
  by_item <- inputs$by_item
  na_if_less <- inputs$na_if_less

  model <- list(
    minimal = mtscr::top_scoring(
      data,
      {{ id_col }},
      {{ score_col }},
      {{ item_col }},
      top = top,
      by_item = by_item,
      na_if_less = na_if_less,
      append = FALSE
    ),
    whole = mtscr::top_scoring(
      data,
      {{ id_col }},
      {{ score_col }},
      {{ item_col }},
      top = top,
      by_item = by_item,
      na_if_less = na_if_less,
      append = TRUE
    )
  )

  return(model)
}

# Main Server Function ----
function(input, output, session) {
  # Data Import ----
  datamods::import_modal(
    id = "data_main",
    from = c("env", "file", "copypaste", "googlesheets", "url"),
    title = "Import data to be used in application"
  )

  observeEvent(input$import_window, {
    datamods::import_modal(
      id = "data_main",
      from = c("env", "file", "copypaste", "googlesheets", "url"),
      title = "Import data to be used in application"
    )
  })

  imported <- datamods::import_server("data_main", return_class = "tbl_df")

  # Module Servers ----
  mts_module <- mtsServer("mts_module", imported$data)
  sts_module <- stsServer("sts_module", imported$data)

  # Reactive values for storing results ----
  values <- reactiveValues(
    current_model = NULL,
    current_type = NULL,
    scored_data = NULL,
    scored_data_whole = NULL
  )

  # MTS Model Generation ----
  observeEvent(mts_module()$generate_clicked(), {
    req(imported$data())

    inputs <- mts_module()$get_inputs()
    result <- safe_model_generation(generate_mts_model, inputs)

    if (result$success) {
      # Store results
      values$current_model <- result$model
      values$current_type <- "MTS"
      values$scored_data <- predict(result$model, minimal = TRUE)
      values$scored_data_whole <- predict(result$model, minimal = FALSE)

      # Show success message
      shinyWidgets::show_alert(
        title = "Success!",
        text = result$message,
        type = "success"
      )

      # Show warning if any
      if (!is.null(result$warning)) {
        shinyWidgets::show_alert(
          title = "Warning",
          text = result$warning,
          type = "warning"
        )
      }
    } else {
      shinyWidgets::show_alert(
        title = "Error",
        text = result$message,
        type = "error"
      )
    }
  })

  # STS Model Generation ----
  observeEvent(sts_module()$generate_clicked(), {
    req(imported$data())

    inputs <- sts_module()$get_inputs()
    result <- safe_model_generation(generate_sts_model, inputs)

    if (result$success) {
      # Store results
      values$current_model <- result$model
      values$current_type <- "STS"
      values$scored_data <- result$model$minimal
      values$scored_data_whole <- result$model$whole

      # Show success message
      shinyWidgets::show_alert(
        title = "Success!",
        text = result$message,
        type = "success"
      )

      # Show warning if any
      if (!is.null(result$warning)) {
        shinyWidgets::show_alert(
          title = "Warning",
          text = result$warning,
          type = "warning"
        )
      }
    } else {
      shinyWidgets::show_alert(
        title = "Error",
        text = result$message,
        type = "error"
      )
    }
  })

  # Output Rendering ----

  ## Models summary ----
  output$models_summary_header <- renderUI({
    req(values$current_model)
    tags$b(paste(values$current_type, "Model Summary:"))
  })

  output$models_summary <- renderTable({
    req(values$current_model)
    format_model_summary(values$current_model, values$current_type)
  })

  ## Scored data table ----
  output$scored_data_header <- renderUI({
    req(values$scored_data)
    tags$b("Scored Data:")
  })

  output$scored_data <- DT::renderDataTable(
    {
      req(values$scored_data)
      values$scored_data
    },
    extensions = "Buttons",
    options = list(
      dom = "Bfrtip",
      buttons = c("csv", "excel"),
      scrollX = TRUE
    )
  )

  ## Download buttons ----
  output$download_buttons <- renderUI({
    req(values$scored_data)

    div(
      style = "margin-top: 20px;",
      card(
        card_header("Download Results"),
        div(
          tags$b("Download scores:"),
          br(),
          downloadButton(
            "scores_csv",
            "Scores (.csv)",
            class = "btn-outline-primary"
          ),
          " ",
          downloadButton(
            "scores_xlsx",
            "Scores (.xlsx)",
            class = "btn-outline-primary"
          ),
          br(),
          br(),
          tags$b("Download complete database with scores:"),
          br(),
          downloadButton(
            "whole_csv",
            "Complete (.csv)",
            class = "btn-outline-secondary"
          ),
          " ",
          downloadButton(
            "whole_xlsx",
            "Complete (.xlsx)",
            class = "btn-outline-secondary"
          )
        )
      )
    )
  })

  # Download Handlers ----
  output$scores_csv <- downloadHandler(
    filename = function() {
      create_filename(paste0(tolower(values$current_type), "_scores"), "csv")
    },
    content = function(file) {
      write.csv(values$scored_data, file, row.names = FALSE)
    }
  )

  output$scores_xlsx <- downloadHandler(
    filename = function() {
      create_filename(paste0(tolower(values$current_type), "_scores"), "xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(values$scored_data, file)
    }
  )

  output$whole_csv <- downloadHandler(
    filename = function() {
      create_filename(paste0(tolower(values$current_type), "_complete"), "csv")
    },
    content = function(file) {
      write.csv(values$scored_data_whole, file, row.names = FALSE)
    }
  )

  output$whole_xlsx <- downloadHandler(
    filename = function() {
      create_filename(paste0(tolower(values$current_type), "_complete"), "xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(values$scored_data_whole, file)
    }
  )
}
