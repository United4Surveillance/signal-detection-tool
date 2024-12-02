#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  options(shiny.sanitize.errors = TRUE)
  # bslib::bs_themer()
  mod_tabpanel_help_server("help")

  data_load_check_result <- mod_tabpanel_data_server("data")

  datinput <- mod_tabpanel_input_server("input",
    data = data_load_check_result$data,
    errors_detected = data_load_check_result$errors_detected
  )

  signals_output <- mod_tabpanel_signals_server("signals",
    filtered_data = datinput$filtered_data,
    number_of_weeks = datinput$n_weeks,
    number_of_weeks_input_valid = datinput$weeks_input_valid,
    strat_vars = datinput$strat_vars,
    errors_detected = data_load_check_result$errors_detected,
    method = datinput$method,
    no_algorithm_possible = datinput$no_algorithm_possible,
    intervention_date = datinput$intervention_date
  )

  mod_tabpanel_report_server("report",
    filtered_data = datinput$filtered_data,
    strat_vars = datinput$strat_vars,
    pathogen_vars = datinput$pathogen_vars,
    errors_detected = data_load_check_result$errors_detected,
    datinput$no_algorithm_possible,
    number_of_weeks_input_valid = datinput$weeks_input_valid,
    signals_padded = signals_output$signals_padded,
    signals_agg = signals_output$signals_agg,
    intervention_date = datinput$intervention_date
  )


  # Calculate tab-content size based on title-panel and footer -----------------
  rv <- reactiveValues(
    title_height = NULL,
    footer_height = NULL,
    navbar_height = NULL
  )

  # JavaScript to listen for window resize and recalculate heights
  shiny::observe({
    shinyjs::runjs("
      // Function to recalculate heights and update Shiny inputs
      function recalculateHeights() {
        var titlePanel = document.getElementById('title-panel');
        var footer = document.getElementById('footer');
        var navbar = document.querySelector('.nav-tabs'); // Select the navbar element
        // Get the actual height including padding and border
        var titleHeight = titlePanel ? titlePanel.getBoundingClientRect().height : 0;
        var footerHeight = footer ? footer.getBoundingClientRect().height : 0;
        var navbarHeight = navbar ? navbar.getBoundingClientRect().height : 0;
        // Send new height values to Shiny server
        Shiny.setInputValue('title_panel_height', titleHeight);
        Shiny.setInputValue('footer_height', footerHeight);
        Shiny.setInputValue('navbar_height', navbarHeight);
      }
      // Recalculate heights on window resize
      window.onresize = recalculateHeights;
      // Initial call to set heights on page load
      recalculateHeights();
    ")
  })

  # Update reactiveValues with the height inputs
  shiny::observeEvent(input$title_panel_height, {
    rv$title_height <- input$title_panel_height
  })
  shiny::observeEvent(input$footer_height, {
    rv$footer_height <- input$footer_height
  })
  shiny::observeEvent(input$navbar_height, {
    rv$navbar_height <- input$navbar_height
  })

  # Dynamically adjust the tab content max-height based on title, footer, and navbar heights
  shiny::observe({
    shiny::req(rv$title_height, rv$footer_height, rv$navbar_height)  # Ensure all heights are available

    # # Debugging: Print current heights to console
    # print(paste("Title Height:", rv$title_height))
    # print(paste("Footer Height:", rv$footer_height))
    # print(paste("Navbar Height:", rv$navbar_height))

    # Update the CSS for the tab content's maxHeight
    shinyjs::runjs(sprintf("
      var tabContent = document.querySelector('.tab-content');
      if (tabContent) {
        tabContent.style.maxHeight = 'calc(100vh - %fpx - %fpx - %fpx)';
      }
    ", rv$title_height, rv$footer_height, rv$navbar_height))
  })


}
