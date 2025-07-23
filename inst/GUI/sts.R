# Simple Top Scoring Module ----

## UI Module ----
stsUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Column selectors
    uiOutput(ns("column_selectors")),

    div(
      checkboxInput(
        ns("by_item"),
        "Separate scores for different items",
        value = FALSE
      ),
      checkboxInput(
        ns("na_if_less"),
        "Return NA if less ideas than expected top answers",
        value = FALSE
      ),
      
      # Info button for means calculation
      div(
        style = "margin-top: 10px; margin-bottom: 10px;",
        actionButton(
          ns("means_calculation_info"),
          "How are the means calculated?",
          class = "btn-link btn-sm",
          style = "padding: 0; text-decoration: underline;"
        )
      )
    ),

    # Model parameters
    div(
      style = "margin-top: 15px;",
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
        "Generate STS Model →",
        class = "btn-info",
        style = "width: 100%;"
      )
    )
  )
}

## Server Module ----
stsServer <- function(id, imported_data) {
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

    # Means calculation info modal ----
    observeEvent(input$means_calculation_info, {
      shinyWidgets::show_alert(
        title = "How are the means calculated?",
        text = HTML("
          <p><strong>If you provide the item column and don't check the \"By item\" tickbox:</strong><br/>
          best scores for each item → item means → grand mean</p>
          
          <p><strong>If you don't provide the item column:</strong><br/>
          best scores overall → grand mean</p>
          
          <p><strong>If the \"NA if less\" tickbox is not checked:</strong><br/>
          the scores will be calculated from all the available data.</p>
        "),
        type = "info",
        html = TRUE
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
            item_col = if (input$item_column == "no item column") {
              NULL
            } else {
              input$item_column
            },
            score_col = input$score_column,
            top = seq(1, input$top),
            by_item = input$by_item,
            na_if_less = input$na_if_less
          )
        })
      )
    })
  })
}
