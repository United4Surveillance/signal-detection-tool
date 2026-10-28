#' tabpanel "Signal Line List" UI Function
#'
#' @description A shiny Module for a tab to display line list cases related to a
#' detected signal based on parameters inputs chosen. Ability to download data
#' to file.
#'
#' @param id Internal parameter for {shiny}, ensuring namespace coherency in sessions.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabpanel_linelist_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    title = "Signal Line List",
    icon = shiny::icon("magnifying-glass"),
    shiny::tags$style(shiny::HTML("
    .selected-signals-list {
    display: flex;
    flex-wrap: wrap;
    align-content: flex-start;
    gap: 6px;
    }

    .selected-signal {
    border: 1px solid #bdcae6;
    border-radius: 4px;
    background: #f6f8fc;
    color: #304794;
    padding: 5px 8px;
    font-size: 12px;
    line-height: 1.4;
    }

    .summary-card {
    color: #304794;
    }
    ")),
    shinybusy::add_busy_spinner(
      spin = "fading-circle",
      color = "#304794",
      position = "full-page",
      height = "100px",
      width = "100px"
    ),
    shiny::div(
      class = "content-container",
      shiny::div(
        class = "card-container",
        bslib::layout_column_wrap(
          width = 1,
          heights_equal = "row",
          shiny::uiOutput(ns("signal_linelist_tab_ui"))
        )
      ),
      footer_text
    )
  )
}


#' tabpanel "Signal Line List" Server Functions
#'
#' @noRd
mod_tabpanel_linelist_server <- function(
  id,
  filtered_data,
  errors_detected,
  number_of_time_units_input_valid,
  method,
  no_algorithm_possible,
  intervention_date,
  signals_padded
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # UI-portion of the tab below!
    # ensuring that content is only shown if data check returns no errors
    output$signal_linelist_tab_ui <- shiny::renderUI({
      if (errors_detected() == TRUE) {
        return(datacheck_error_message)
      } else if (!number_of_time_units_input_valid()) {
        return(ntime_units_error_message)
      } else if (no_algorithm_possible() == TRUE) {
        return(algorithm_error_message)
      } else {
        return(shiny::tagList(
          # CARD 1: Signals Overview
          bslib::card(
            id = ns("signal-selection"),
            min_height = "700px",
            shiny::h1("Signals Overview"),
            shiny::span("Click into the table to select the signals you want to further investigate."),
            shiny::span(
              paste0(
                "Detected signals using method '",
                get_name_by_value(method(), available_algorithms()), "':"
              ),
              style = "font-size:120%;font-weight: bold"
            ),
            DT::DTOutput(ns("show_signals_padded"))
          ),

          # CARD 2: Signal Analysis
          shiny::uiOutput(ns("signal_analysis_panel"))
        ))
      }
    })


    true_signals <- shiny::reactive({
      shiny::req(signals_padded())

      signals <- signals_padded() %>% dplyr::filter(alarms == TRUE)
      if (nrow(signals) > 0) {
        signals <- signals %>% dplyr::mutate(signal_id = 1:dplyr::n(), .before = 1)
      }
      return(signals)
    })

    # output padded signal data in table
    output$show_signals_padded <- DT::renderDT({
      shiny::req(!errors_detected())
      shiny::req(true_signals)

      signals <- true_signals()

      if (nrow(signals) == 0) {
        # Return a placeholder message
        build_empty_datatable("No signals found.")
      } else {
        # Render the actual data table
        build_signals_table(
          signals,
          format = "DataTable",
          dt_selection_type = "multiple"
        )
      }
    })

    cases_linelist <- shiny::reactive({
      shiny::req(filtered_data())
      shiny::req(true_signals())
      shiny::req(signals_padded())
      shiny::req(!is.na(input$show_signals_padded_rows_selected))

      selected_signal_ids <- sort(input$show_signals_padded_rows_selected)

      build_signal_and_comparison_linelist(
        selected_signal_ids = selected_signal_ids,
        true_signals = true_signals(),
        signals_padded = signals_padded(),
        filtered_data = filtered_data()
      )
    })

    render_analysis <- reactiveVal(FALSE)

    shiny::observeEvent(input$show_signals_padded_rows_selected,
      {
        if (isFALSE(render_analysis())) {
          if (length(input$show_signals_padded_rows_selected) > 0) {
            render_analysis(TRUE)
          }
        } else {
          if (is.null(input$show_signals_padded_rows_selected)) {
            render_analysis(FALSE)
          }
        }
      },
      ignoreNULL = FALSE
    )

    # render signal analysis only when cases_linelist exist
    output$signal_analysis_panel <- shiny::renderUI({
      shiny::req(render_analysis() == TRUE)

      bslib::card(
        bslib::card_title("Signal Analysis", container = shiny::h1),
        bslib::card(
          shiny::uiOutput(ns("selected_signals_summary")),
          fill = FALSE
        ),
        bslib::navset_card_pill(
          ## TAB: Analysis
          bslib::nav_panel(
            title = "Analysis",
            bslib::card(
              bslib::card_title("Comparisons table", container = shiny::h3),
              bslib::card_body(DT::DTOutput(ns("comparisons_tbl")), fillable = FALSE)
            ),
            bslib::card(
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  shiny::uiOutput(ns("epicurve_stratum_ui"))
                ),
                bslib::layout_columns(
                  bslib::card(
                    min_height = "50px",
                    full_screen = TRUE,
                    plotly::plotlyOutput(ns("epicurve"))
                  ),
                  bslib::card(
                    min_height = "50px",
                    full_screen = TRUE,
                    plotly::plotlyOutput(ns("age_comparison"))
                  )
                )
              )
            )
          ),
          ## TAB: Case Linelist
          bslib::nav_panel(
            title = "Case Linelist",
            bslib::card_title("Case Linelist for Selected Signals", container = shiny::h3),
            bslib::card_title("Export or review cases linked to the selected signals.", container = shiny::h4),
            bslib::card_body(DT::DTOutput(ns("linelist")), fillable = FALSE)
          )
        )
      )
    })

    show_all_signals <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$toggle_signals, {
      show_all_signals(!show_all_signals())
    })

    output$selected_signals_summary <- shiny::renderUI({

      rows <- sort(shiny::req(input$show_signals_padded_rows_selected))
      signals <- true_signals()
      shiny::req(all(rows >= 1L & rows <= nrow(signals)))

      selected <- dplyr::slice(signals, rows)
      cases <- cases_linelist()$cases

      # Period labels
      period <- vapply(seq_len(nrow(selected)), function(i) {
        switch(
          selected$time_unit[i],
          weekly = paste0(
            "Week ", selected$week[i], ", ", selected$year[i]
          ),
          biweekly = {
            start <- ISOweek::ISOweek2date(paste0(
              selected$year[i], "-W",
              sprintf("%02d", selected$week[i]), "-1"
            ))
            paste(start, start + 13, sep = " - ")
          },
          monthly = paste0(
            selected$year[i], "-",
            sprintf("%02d", selected$month[i])
          )
        )
      }, character(1))

      # Mean score
      scores <- selected$score
      scores <- scores[is.finite(scores)]

      mean_score <- if (length(scores)) {
        sprintf("%.2f", mean(scores))
      } else {
        "Not available"
      }

      # Cases in outbreaks
      outbreak_cases <- if (
        "outbreak_status" %in% names(cases) &&
        (nrow(cases) == 0L || any(!is.na(cases$outbreak_status)))
      ) {
        sum(cases$outbreak_status == "yes", na.rm = TRUE)
      } else {
        "Not available"
      }

      # Shared value box theme
      summary_theme <- bslib::value_box_theme(
        bg = "#ffffff",
        fg = "#304794"
      )

      # Create signal labels
      signal_items <- lapply(seq_len(nrow(selected)), function(i) {
        shiny::div(
          class = "selected-signal",
          shiny::tags$strong(paste0("S", selected$signal_id[i])),
          shiny::tags$br(),
          period[i],
          shiny::tags$br(),
          paste(selected$cases[i], "cases")
        )
      })

      bslib::layout_columns(
        col_widths = c(4, 2, 2, 2, 2),
        fill = FALSE,

        # Selected signals

        # Selected signals
        bslib::card(
          class = "summary-card",

          bslib::card_header(
            shiny::tags$strong("Selected signals"),
            shiny::span(
              class = "float-end",
              paste(
                nrow(selected),
                if (nrow(selected) == 1L) {
                  "signal selected"
                } else {
                  "signals selected"
                }
              )
            )
          ),

          bslib::card_body(
            fillable = FALSE,
            shiny::div(
              class = "selected-signals-list",

              if (show_all_signals()) {
                signal_items
              } else {
                head(signal_items, 4)
              }
            ),

            if (length(signal_items) > 4L) {
              shiny::actionLink(
                inputId = ns("toggle_signals"),
                label = if (show_all_signals()) {
                  "Show less"
                } else {
                  paste0("Show ", length(signal_items) - 4L, " more")
                }
              )
            }
          )
        ),

        # Summary metrics
        bslib::value_box(
          title = "Number of cases total",
          value = nrow(cases),
          theme = summary_theme,
          height = "180px",
          fill = FALSE,
          class = "border"
        ),

        bslib::value_box(
          title = "Number of cases in outbreaks",
          value = outbreak_cases,
          theme = summary_theme,
          height = "180px",
          fill = FALSE,
          class = "border"
        ),

        bslib::value_box(
          title = "Mean score",
          value = mean_score,
          theme = summary_theme,
          height = "180px",
          fill = FALSE,
          class = "border"
        ),

        # Back to selection
        shiny::div(
          class = "d-flex align-items-center h-100",
          shiny::tags$a(
            href = paste0("#", ns("signal-selection")),
            class = "btn btn-outline-primary w-100",
            "Change selection"
          )
        )
      )
    })

    # display age distribution graphic
    output$age_comparison <- plotly::renderPlotly({
      shiny::req(cases_linelist())
      shiny::req(true_signals())

      linelist_cases <- dplyr::bind_rows(
        cases_linelist()$cases %>% dplyr::mutate(signal = T),
        cases_linelist()$cases_comparison %>% dplyr::mutate(signal = F)
      )
      cases_agg <- linelist_cases %>%
        dplyr::count(signal, age_group) %>%
        dplyr::group_by(signal) %>%
        dplyr::mutate(
          total_n = sum(n),
          perc = round(n / total_n * 100)
        ) %>%
        dplyr::ungroup()

      plot_agegroup_comparison(cases_agg, unique(true_signals()$number_of_time_units), unique(true_signals()$time_unit))
    })

    # Generate epicurve plot
    epicurve_plot <- shiny::reactive({
      linelist <- shiny::req(cases_linelist())
      epicurve_stratum <- shiny::req(input$epicurve_stratum)

      cases_agg <- dplyr::bind_rows(
        linelist$cases %>%
          dplyr::mutate(signal = TRUE),
        linelist$cases_comparison %>%
          dplyr::mutate(signal = FALSE)
      )

      selected_stratum <- if (
        identical(epicurve_stratum, "None")
      ) {
        NULL
      } else {
        epicurve_stratum
      }

      plot_epicurve(
        cases_agg,
        stratum = selected_stratum
      )
    })

    # Display epicurve graphic
    output$epicurve <- plotly::renderPlotly({
      epicurve_plot()
    })

    # make selection of stratum possible
    output$epicurve_stratum_ui <- shiny::renderUI({
      shiny::req(cases_linelist())

      dat <- dplyr::bind_rows(
        cases_linelist()$cases,
        cases_linelist()$cases_comparison
      )

      stratum_choices <- dat %>%
        dplyr::select(where(is.character) | where(is.factor)) %>%
        dplyr::select(
          -dplyr::any_of(c("pathogen")),
          -dplyr::ends_with("_id")
        ) %>%
        names() %>%
        sort()

      shiny::selectInput(
        inputId = ns("epicurve_stratum"),
        label = "Select a stratum to display in the epicurve",
        choices = c("None", stratum_choices),
        selected = "None",
        multiple = FALSE,
        width = "100%"
      )
    })

    comparisons_table <- shiny::reactive({
      req(cases_linelist)

      build_comparison_summary(cases_linelist())
    })

    output$comparisons_tbl <- DT::renderDT({
      req(comparisons_table)

      DT::datatable(
        comparisons_table(),
        rownames = FALSE,
        colnames = c("Measure", "Cases in selected signals", "Rest of cases")
      )
    })

    # display linelist of selected signals
    output$linelist <- DT::renderDataTable({
      filename_download <- "signals_line_list"

      DT::datatable(
        cases_linelist()$cases,
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          buttons = list(
            "copy",
            list(extend = "csv", filename = filename_download),
            list(extend = "excel", filename = filename_download),
            list(extend = "pdf", filename = filename_download),
            "print"
          ),
          scrollX = TRUE
        )
      )
    })
  })
}
