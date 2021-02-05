#' Map reliability calculator
#'
#' Takes a dataframe with estimate and MOE columns and launches an RStudio addin
#' to explore the reliability of various classifications of the estimate values for mapping.
#'
#' @param data Dataframe
#' @param est Estimate column
#' @param moe Margin of error column
#' @param viewer Where to display the gadget: `"dialog"`, `"pane"` or
#'   `"browser"` (see [shiny::viewer()]).
#'
#' @export
#'
#' @import shiny
#' @import rlang
#' @importFrom magrittr `%>%`
#'
#' @examples
#' \dontrun{
#'
#' if (interactive()) {
#'
#' bk_hh_income <- tidycensus::get_acs(
#'   geography = "tract",
#'   state = "NY",
#'   county = "Kings",
#'   year = 2018,
#'   survey = "acs5",
#'   variables = "B19013_001", # Median Household Income
#'   key = Sys.getenv("CENSUS_API")
#' )
#'
#' reliability_calculator(bk_hh_income, estimate, moe)
#'
#' }
#'
#' }
reliability_calculator <- function(data, est, moe, viewer = getOption(x = "mapreliability.viewer", default = "pane")) {
  input_data <- prep_input_data(data, as_label(enquo(est)), as_label(enquo(moe)))

  custom_placeholder_breaks <- input_data %>%
    reliability_table_equal(est, moe, 4, quiet = TRUE) %>%
    dplyr::pull(.data[["class_breaks"]]) %>%
    paste(collapse = ", ")


  ui <- miniUI::miniPage(

    miniUI::gadgetTitleBar(
      "Map Reliability Calculator",
      left = miniUI::miniTitleBarCancelButton(),
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),

    miniUI::miniContentPanel(
      fluidRow(
        column(12,
               h3("Input data"),
               p("Click and drag to restrict the observations used in the calculations"),
               plotly::plotlyOutput("plot")
        )
      ),
      fluidRow(
        column(6,
               h3("Equal Interval"),
               numericInput("num_classes_eq", NULL, 4, min = 2, step = 1, width = "70px"),
               DT::dataTableOutput("equal_table")
        ),
        column(6,
               h3("Quantile"),
               numericInput("num_classes_qu", NULL, 4, min = 2, step = 1, width = "70px"),
               DT::dataTableOutput("quant_table")
        )
      ),

      fluidRow(
        column(6,
               h3("Custom"),
               textInput("custom_breaks",
                         label = "Enter lower breaks separated by commas",
                         value = custom_placeholder_breaks),
               DT::dataTableOutput("custom_table")
        ),
        column(6,
               h3("Notes"),
               p('This tool assumes the margins of error for the input data use a 90% confidence level.'),
               p('The reliability column shows the likelihood, on average, that any given geography is erroneously classed.'),
               p('A classification is considered to be reliable if total reliability is less 10% and all individual classes are less than 20%.'),
               br(),
               h5('Suggested notation for maps that pass this reliability test:'),
               p("There is less than a 10% that chance any given geography in this map is misclassified due to sampling error."),
               p("For each individual category, there is less than a 20% chance that any given geography is misclassified due to sampling error.")
        )
      )
    )
  )

  server <- function(input, output, session) {

    output$plot <- plotly::renderPlotly({
      input_data %>%
        plotly::plot_ly(type = "scatter", mode = "markers", x = ~est, y = ~moe) %>%
        plotly::layout(
          dragmode = "select",
          xaxis = list(title = "Estimate"),
          yaxis = list(title = "Margin of Error (90%)")
        ) %>%
        plotly::event_register("plotly_selecting")
    })

    output$equal_table <- DT::renderDataTable({
      make_table(input_data, reliability_table_equal, n_classes = input$num_classes_eq)
    })

    output$quant_table <- DT::renderDataTable({
      make_table(input_data, reliability_table_quant, n_classes = input$num_classes_qu)
    })

    output$custom_table <- DT::renderDataTable({
      breaks <- stringr::str_split(input$custom_breaks, ",")[[1]] %>% readr::parse_number() %>% na.omit()
      make_table(input_data, reliability_table_custom, class_breaks_low = breaks)
    })
  }

  make_table <- function(default_data, breaks_fun, ...) {
    selected_data <- plotly::event_data("plotly_selected")

    if (is.null(selected_data)) {
      selected_data <- default_data
    } else {
      selected_data <- dplyr::select(selected_data, est = .data[["x"]], moe = .data[["y"]])
    }

    formatted_data <- selected_data %>%
      breaks_fun(est, moe, ..., quiet = TRUE) %>%
      dplyr::mutate(class_breaks = as.character(class_breaks)) %>%
      tibble::add_row(
        class_breaks = "TOTAL",
        count = .[["tot_count"]][[1]],
        reliability = .[["tot_reliability"]][[1]]
      ) %>%
      dplyr::select(-dplyr::starts_with("tot_"))


    DT::datatable(
      formatted_data,
      rownames = FALSE,
      colnames = c("Class Breaks" = "class_breaks", "Count" = "count", "Reliability" = "reliability"),
      options = list(
        paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE,
        scrollY = "600px", scrollCollapse = TRUE,
        rowCallback = DT::JS(
          'function(row, data) {
          // Color classes for reliability
          if (data[0] != "TOTAL") {
            if (parseFloat(data[2]) < 20.0)
              $("td:eq(2)", row).css("color", "green")
            else
              $("td:eq(2)", row).css("color", "red");
          }
          // Style total row
          if (data[0] == "TOTAL") {
            $("td:eq(0)", row).css("font-weight", "bold").css("text-decoration", "underline")
            $("td:eq(1)", row).css("font-weight", "bold").css("text-decoration", "underline")
            if (parseFloat(data[2]) < 10.0)
              $("td:eq(2)", row).css("color", "green").css("text-decoration", "underline")
            else
              $("td:eq(2)", row).css("color", "red").css("text-decoration", "underline");
          }
        }'
        )
      )
    ) %>%
      DT::formatStyle("Reliability", `font-weight` = "bold") %>%
      DT::formatRound("Reliability", 1)
  }


  if (viewer == "dialog") {
    viewer <- dialogViewer("Map Reliability Calculator")
  } else if (viewer == "browser") {
    viewer <- browserViewer()
  } else {
    viewer <- paneViewer(minHeight = 600)
  }

  runGadget(app = ui, server = server, viewer = viewer)

}
