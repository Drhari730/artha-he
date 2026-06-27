# =============================================================================
# Artha HE — Health Economics Workbench
# app.R : application entry point. Run with:  shiny::runApp("ArthaHE")
# =============================================================================
# Load libraries, helpers, modules and constants. Sourcing explicitly (rather
# than relying on Shiny's auto-load order) guarantees bslib is attached before
# the UI below is built.
if (!exists("APP_NAME")) source("global.R")

ui <- page_navbar(
  title = tags$span(APP_NAME, tags$small(style = "opacity:.6; font-weight:400;",
                                         paste0("  ", APP_TAGLINE))),
  theme = bs_theme(version = 5, bootswatch = "flatly",
                   primary = "#0b6e4f", secondary = "#16697a"),
  fillable = FALSE,

  nav_panel("Costing",    icon = icon("file-invoice-dollar"), costingUI("costing")),
  nav_panel("Evaluation", icon = icon("scale-balanced"),      evaluationUI("eval")),
  nav_panel("Modeling",   icon = icon("diagram-project"),     modelUI("model")),
  nav_panel("Sensitivity",icon = icon("chart-line"),          sensitivityUI("sens")),
  nav_panel("Budget impact", icon = icon("sack-dollar"),      biaUI("bia")),

  nav_spacer(),
  nav_item(tags$a(href = "#", paste0("v", APP_VERSION), style = "opacity:.5;")),
  nav_panel("About", icon = icon("circle-info"),
    card(card_header("About Artha HE"),
      card_body(
        tags$p(tags$strong(APP_NAME), " is a health-economics workbench for ",
               "researchers, teaching, and HTA/payer analysis."),
        tags$p("Modules: costing (micro & gross), economic evaluation (ICER, ",
               "CE plane), decision-analytic modeling, sensitivity analysis, ",
               "and budget impact analysis."),
        tags$p("Defaults are tuned for the Indian/LMIC context ",
               "(GDP-based WTP thresholds, 3% discounting). All configurable."),
        tags$p(tags$em("Reporting will follow the CHEERS 2022 checklist."))
      ))
  )
)

server <- function(input, output, session) {
  costingServer("costing")
  evaluationServer("eval")
  modelServer("model")
  sensitivityServer("sens")
  biaServer("bia")
}

shinyApp(ui, server)
