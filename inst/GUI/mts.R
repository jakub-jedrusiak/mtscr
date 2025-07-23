# Multidimensional Top Scoring Module ----

## UI Module ----
mtsUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Column selectors
    uiOutput(ns("column_selectors")),

    # Model parameters
    div(
      style = "margin-top: 15px;",
      selectInput(
        ns("ties_method"),
        "Ties method:",
        choices = c(
          "random (better for ratings)" = "random",
          "average (better for continuous scores)" = "average"
        ),
        selected = "random"
      ),
      checkboxInput(
        ns("normalise"),
        "Normalise scores (recommended)",
        value = TRUE
      ),
      div(
        style = "margin-bottom: 10px;",
        actionButton(
          ns("self_ranking_info"),
          "What is self-ranking?",
          class = "btn-link btn-sm",
          style = "padding: 0; text-decoration: underline;"
        )
      ),
      selectInput(
        ns("self_ranking"),
        "Column with self-ranking:",
        choices = "no self-ranking"
      ),
      sliderInput(
        ns("top"),
        "Max number of top answers:",
        value = 1,
        min = 1,
        max = 10
      )
    ),

    # Generate button
    div(
      style = "margin-top: 20px;",
      actionButton(
        ns("generate_model"),
        "Generate MTS Model →",
        class = "btn-success",
        style = "width: 100%;"
      )
    )
  )
}

## Server Module ----
mtsServer <- function(id, imported_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Column selectors UI ----
    output$column_selectors <- renderUI({
      req(imported_data())
      data <- imported_data()

      tagList(
        selectInput(
          ns("id_column"),
          "ID column:",
          choices = colnames(data)
        ),
        selectInput(
          ns("score_column"),
          "Score column:",
          choices = colnames(
            dplyr::select(data, dplyr::where(is.numeric))
          )
        ),
        selectInput(
          ns("item_column"),
          "Item column:",
          choices = c("no item column", colnames(data))
        )
      )
    })

    # Update self-ranking choices when data changes ----
    observe({
      req(imported_data())
      data <- imported_data()
      numeric_cols <- colnames(dplyr::select(data, dplyr::where(is.numeric)))

      updateSelectInput(
        session,
        "self_ranking",
        choices = c("no self-ranking", numeric_cols)
      )
    })

    # Self-ranking info modal ----
    observeEvent(input$self_ranking_info, {
      shinyWidgets::show_alert(
        title = "Self-ranking Information",
        text = "Name of the column containing answers' self-ranking. Provide if model should be based on top answers self-chosen by the participant. Every item should have its own ranks. Preferably it should be a complete ranking (each answer with its own relative rank) starting with 1 for the best answer. Otherwise the top answers should have a value of 1, and the other answers should have a value of 0. In that case, the Top answers argument doesn't change anything so leave the slider at 1. Ties method is not used if self-ranking was provided. See mtscr_self_rank dataset for example.",
        type = "info"
      )
    })

    # Return reactive values for the main app ----
    reactive({
      list(
        generate_clicked = reactive({
          input$generate_model
        }),
        get_inputs = reactive({
          list(
            data = imported_data(),
            id_col = input$id_column,
            item_col = if (input$item_column == "no item column") NULL else input$item_column,
            score_col = input$score_column,
            ties_method = input$ties_method,
            top = seq(1, input$top),
            normalise = input$normalise,
            self_ranking = if (input$self_ranking == "no self-ranking") NULL else input$self_ranking
          )
        })
      )
    })
  })
}
