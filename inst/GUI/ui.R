require("shiny")
require("bslib")

# Source modules and functions
source("mts.R", local = TRUE)
source("sts.R", local = TRUE)
source("functions.R", local = TRUE)

# UI ----
ui <- fluidPage(
  tag(
    "link",
    list(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css?family=Raleway"
    )
  ),
  includeCSS("./www/styles.css"),
  titlePanel("Multidimensional Top Scoring for Creativity Research"),
  ## Sidebar ----
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # hex sticker ----
      img(
        id = "hex-sticker",
        src = "https://raw.githubusercontent.com/jakub-jedrusiak/mtscr/main/man/figures/mtscr-hex.svg",
        alt = "mtscr hex sticker",
      ),
      hr(),

      # Data Import Card ----
      card(
        card_header("Data Import"),
        actionButton(
          "import_window",
          "Import data",
          class = "btn-primary",
          style = "width: 100%;"
        )
      ),
      br(),

      # Multidimensional Top Scoring Card ----
      card(
        card_header("Multidimensional Top Scoring", class = "nice-card-header"),
        mtsUI("mts_module")
      ),
      br(),

      # Simple Top Scoring Card ----
      card(
        card_header("Simple Top Scoring", class = "nice-card-header"),
        stsUI("sts_module")
      )
    ),
    ## Main panel ----
    mainPanel(
      width = 9,
      fluidRow(
        ### Model info ----
        uiOutput("models_summary_header"),
        tableOutput("models_summary"),
        ### Loading message ----
        conditionalPanel(
          condition = "$('html').hasClass('shiny-busy')",
          tags$div("Loading...", id = "loadmessage")
        )
      ),
      fluidRow(
        uiOutput("scored_data_header"),
        DT::dataTableOutput("scored_data", width = "95%")
      ),
      fluidRow(
        uiOutput("download_buttons")
      )
    )
  ),
  hr(),
  div(class = "footer", includeHTML("./www/article_citation.html"))
)
