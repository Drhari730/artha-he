# =============================================================================
# mod_evaluation.R : Economic evaluation (ICER, incremental, CE plane, NMB)
# Functional with a small editable strategy table.
# =============================================================================

evaluationUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 340,
      title = "Comparison inputs",
      helpText("Enter cost and effect (e.g. QALYs) for each strategy. ",
               "Edit cells directly in the table."),
      sliderInput(ns("wtp"), "Willingness-to-pay per QALY (₹)",
                  min = 0, max = 1000000, value = DEFAULTS$gdp_per_capita,
                  step = 25000),
      helpText(sprintf("1x GDP ≈ %s, 3x GDP ≈ %s",
                       format(DEFAULTS$gdp_per_capita, big.mark = ","),
                       format(DEFAULTS$gdp_per_capita * 3, big.mark = ","))),
      actionButton(ns("run"), "Analyse", class = "btn-primary", icon = icon("scale-balanced"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Strategies (editable)"),
           DTOutput(ns("input_tbl"))),
      card(card_header("Incremental analysis"),
           DTOutput(ns("inc_tbl")))
    ),
    card(card_header("Cost-effectiveness plane"),
         plotOutput(ns("ce_plane"), height = 420))
  )
}

evaluationServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    seed <- reactiveVal(data.frame(
      strategy = c("Standard care", "New drug A", "New drug B"),
      cost     = c(40000, 85000, 120000),
      effect   = c(3.5,   4.4,    4.7),
      stringsAsFactors = FALSE
    ))

    output$input_tbl <- renderDT(
      datatable(seed(), editable = TRUE, rownames = FALSE,
                options = list(dom = "t")),
      server = FALSE
    )
    observeEvent(input$input_tbl_cell_edit, {
      seed(editData(seed(), input$input_tbl_cell_edit, rownames = FALSE))
    })

    analysis <- eventReactive(input$run, he_incremental(seed()), ignoreNULL = FALSE)

    output$inc_tbl <- renderDT({
      a <- analysis()
      datatable(a, rownames = FALSE, options = list(dom = "t")) |>
        formatRound(c("cost", "inc_cost", "icer"), 0) |>
        formatRound(c("effect", "inc_effect"), 3)
    })

    output$ce_plane <- renderPlot({
      d <- seed()
      ref <- d[which.min(d$cost), ]
      d$inc_cost   <- d$cost   - ref$cost
      d$inc_effect <- d$effect - ref$effect
      ggplot(d, aes(inc_effect, inc_cost, label = strategy)) +
        geom_hline(yintercept = 0, colour = "grey70") +
        geom_vline(xintercept = 0, colour = "grey70") +
        geom_abline(slope = input$wtp, intercept = 0,
                    linetype = "dashed", colour = "steelblue") +
        geom_point(size = 4, colour = "#b3001b") +
        geom_text(vjust = -1, size = 4.5) +
        scale_y_continuous(labels = label_comma(prefix = DEFAULTS$currency_symbol)) +
        labs(x = "Incremental effect (QALYs) vs cheapest",
             y = "Incremental cost",
             subtitle = "Dashed line = willingness-to-pay threshold") +
        theme_minimal(base_size = 14)
    })
  })
}
