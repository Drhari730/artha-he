# =============================================================================
# mod_costing.R : Costing module (micro & gross costing)  — FULLY FUNCTIONAL
# =============================================================================

costingUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 340,
      title = "Costing inputs",
      radioButtons(ns("method"), "Costing method",
                   choices = c("Micro-costing (bottom-up)" = "micro",
                               "Gross-costing (top-down)"   = "gross"),
                   selected = "micro"),

      # --- Micro-costing controls ---
      conditionalPanel(
        sprintf("input['%s'] == 'micro'", ns("method")),
        fileInput(ns("file"), "Upload resource-use data (CSV or Excel)",
                  accept = c(".csv", ".xlsx")),
        helpText("Need columns: item, category, quantity, unit_cost, year. ",
                 "Leave empty to use the built-in example."),
        numericInput(ns("to_year"), "Express all costs in price year",
                     value = DEFAULTS$price_year, min = 2000, max = 2100, step = 1),
        sliderInput(ns("inflation"), "Annual inflation rate", min = 0, max = 0.15,
                    value = 0.05, step = 0.005),
        sliderInput(ns("discount"), "Annual discount rate", min = 0, max = 0.10,
                    value = 0, step = 0.005),
        helpText("Discounting applies only if your data has a 'period' column.")
      ),

      # --- Gross-costing controls ---
      conditionalPanel(
        sprintf("input['%s'] == 'gross'", ns("method")),
        numericInput(ns("total_cost"), "Total cost of the service (₹)",
                     value = 5000000, min = 0),
        numericInput(ns("output"), "Number of output units (e.g. patients)",
                     value = 1200, min = 1)
      ),

      actionButton(ns("run"), "Calculate", class = "btn-primary", icon = icon("calculator")),
      hr(),
      downloadButton(ns("dl_csv"),   "Download CSV"),
      downloadButton(ns("dl_xlsx"),  "Download Excel")
    ),

    # --- Main panel ---
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box("Total cost", textOutput(ns("vb_total")),  showcase = icon("indian-rupee-sign"),
                theme = "primary"),
      value_box("Cost per unit", textOutput(ns("vb_per")), showcase = icon("user"),
                theme = "secondary"),
      value_box("Cost lines", textOutput(ns("vb_n")),      showcase = icon("list"),
                theme = "secondary")
    ),
    navset_card_tab(
      nav_panel("Cost table",   DTOutput(ns("tbl"))),
      nav_panel("By category",  plotOutput(ns("plot_cat")), DTOutput(ns("tbl_cat")))
    )
  )
}

costingServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Reactive: load uploaded data, or fall back to the example
    raw_data <- reactive({
      if (is.null(input$file)) return(example_costing_data())
      ext <- tools::file_ext(input$file$name)
      switch(ext,
             csv  = utils::read.csv(input$file$datapath, stringsAsFactors = FALSE),
             xlsx = as.data.frame(readxl::read_excel(input$file$datapath)),
             validate("Please upload a .csv or .xlsx file"))
    })

    # Compute on button press (and once at startup)
    result <- eventReactive(input$run, {
      if (input$method == "micro") {
        res <- micro_cost(raw_data(), to_year = input$to_year,
                          inflation = input$inflation, discount = input$discount)
        list(method = "micro", lines = res$lines, by_cat = res$by_category,
             total = res$total, per_unit = NA_real_, n = nrow(res$lines))
      } else {
        per <- gross_cost(input$total_cost, input$output)
        list(method = "gross",
             lines = data.frame(metric = c("Total cost", "Output units", "Cost per unit"),
                                value  = c(input$total_cost, input$output, per)),
             by_cat = NULL, total = input$total_cost, per_unit = per, n = input$output)
      }
    }, ignoreNULL = FALSE)  # ignoreNULL=FALSE -> runs once on load with defaults

    rupee <- function(x) paste0(DEFAULTS$currency_symbol, format(round(x), big.mark = ",", scientific = FALSE))

    output$vb_total <- renderText(rupee(result()$total))
    output$vb_per   <- renderText({
      r <- result()
      if (r$method == "gross") rupee(r$per_unit) else "—"
    })
    output$vb_n     <- renderText(as.character(result()$n))

    output$tbl <- renderDT({
      datatable(result()$lines, options = list(pageLength = 15, dom = "tip"),
                rownames = FALSE) |>
        formatRound(columns = which(sapply(result()$lines, is.numeric)), digits = 2)
    })

    output$tbl_cat <- renderDT({
      bc <- result()$by_cat
      validate(need(!is.null(bc), "Category breakdown is available for micro-costing only."))
      datatable(bc, rownames = FALSE) |>
        formatRound("cost", 0) |> formatPercentage("share", 1)
    })

    output$plot_cat <- renderPlot({
      bc <- result()$by_cat
      validate(need(!is.null(bc), "Category breakdown is available for micro-costing only."))
      ggplot(bc, aes(x = reorder(category, cost), y = cost, fill = category)) +
        geom_col(width = 0.65, show.legend = FALSE) +
        coord_flip() +
        scale_y_continuous(labels = label_comma(prefix = DEFAULTS$currency_symbol)) +
        labs(x = NULL, y = "Cost", title = "Cost by category") +
        theme_minimal(base_size = 14)
    })

    output$dl_csv <- downloadHandler(
      filename = function() sprintf("artha_costing_%s.csv", Sys.Date()),
      content  = function(file) utils::write.csv(result()$lines, file, row.names = FALSE)
    )
    output$dl_xlsx <- downloadHandler(
      filename = function() sprintf("artha_costing_%s.xlsx", Sys.Date()),
      content  = function(file) {
        out <- list(cost_lines = result()$lines)
        if (!is.null(result()$by_cat)) out$by_category <- result()$by_cat
        writexl::write_xlsx(out, file)
      }
    )
  })
}
