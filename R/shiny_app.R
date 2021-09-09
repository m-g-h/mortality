#' Run a Shiny app displaying the survival model functions
#'
#' @param plot_resolution \code{Numeric scalar} giving the resolution for plots
#'
#' @return Runs a \code{shiny} app
#'
#'
#' @export
#'
shiny_app <- function(plot_resolution = 100) {

  # Fail gracefully if plot resolution is not a scalar
  stop_if_not_scalar(plot_resolution)

  # Fail gracefully if packages are not installed
  stop_if_package_is_missing("shiny")
  stop_if_package_is_missing("tibble")
  stop_if_package_is_missing("ggplot2")

  ui = function(){

    shiny::fluidPage(
      shiny::titlePanel("Models for Mortality, Survival Probability and Life Expectancy"),
      shiny::withMathJax(), # Nice LaTeX math
      shiny::tabsetPanel(
        # Gompertz-Makeham --------------------------------------------------------
        shiny::tabPanel(
          title = "Gompertz-Makeham",
          shiny::wellPanel(
            shiny::fluidRow(
              shiny::column(
                width = 3,
                shiny::h3("Parameters \\( \\mu(x) = A + R e^{\\alpha x} \\)"),
                shiny::sliderInput(inputId = "GM_A",
                                   "\\( A \\quad \\)Age-Unrelated Mortality / Makeham Intercept",
                                   min = 0.0, max = 0.01,
                                   value = 0,
                                   step = 0.00001),
                shiny::sliderInput(inputId = "GM_R",
                                   "\\( R \\quad \\)    Baseline Mortality / Gompertz Intercept",
                                   min = 0.000001, max = 0.0002,
                                   value = 0.000033,
                                   step = 0.000001),
                shiny::sliderInput(inputId = "GM_alpha",
                                   "\\( \\alpha \\quad \\)    Age-Increase in Mortality / Gompertz Slope",
                                   min = 0, max = 0.2,
                                   value = 0.1013,
                                   step = 0.0001)
              ),# End of column
              shiny::column(3,
                            shiny::h5("Force of Mortality \\( \\mu(x) \\)"),
                            shiny::plotOutput(outputId = "GM_plot_mu"),
                            shiny::uiOutput("GM_FOM_formula")
              ),# End of column
              shiny::column(3,
                            shiny::h5("Survival Probability \\( S(x) \\)"),
                            shiny::plotOutput(outputId = "GM_plot_sur")
                            #uiOutput("GM_surv_formula")
              ),# End of column
              shiny::column(3,
                            shiny::h5("Life Expectancy \\( e(x) \\)"),
                            shiny::plotOutput(outputId = "GM_plot_exp")
              )# End of column
            )# End of fluidRow
          )# End of wellPanel

        )# End of Gompertz-Makeham tabPanel

      ),# End of tabsetPanel

      # Author Info -------------------------------------------------------------
      shiny::wellPanel(
        shiny::h4("App Info"),
        shiny::tags$li("Author: Martin Georg Haas") ,
        shiny::tags$li(shiny::tags$a(href="mailto:haasmartin@live.de", "haasmartin@live.de")),
        shiny::tags$li(a("https://github.com/m-g-h"))
      )

    )# End of fluidPage

  }

  server = function(input, output){


    # Gompertz-Makeham --------------------------------------------------------

    # Debug values
    debug_mode = function(){
      input <<- list()

      input$GM_A <<- 0.00033
      input$GM_R <<- 0.00014
      input$GM_alpha <<- 0.11521

      data_GM <<- tibble::tibble(t = (0:1100)/10,
                                 `Force of Mortality` = force_of_mortality_GM(t = t,
                                                                              A = input$GM_A,
                                                                              R = input$GM_R,
                                                                              alpha = input$GM_alpha),
                                 `Survival Probability` = survival_function_GM(t = t,
                                                                               A = input$GM_A,
                                                                               R = input$GM_R,
                                                                               alpha = input$GM_alpha),
                                 `Life Expectancy` = life_expectancy_GM(t = t,
                                                                        A = input$GM_A,
                                                                        R = input$GM_R,
                                                                        alpha = input$GM_alpha)
      )
    }

    data_GM = shiny::reactiveVal()

    shiny::observeEvent({
      input$GM_A
      input$GM_R
      input$GM_alpha},
      {
        data_GM(
          tibble::tibble(
            t = 0:110,
            `Force of Mortality` = mortality::force_of_mortality_GM(t = t,
                                                                    A = input$GM_A,
                                                                    R = input$GM_R,
                                                                    alpha = input$GM_alpha),
            `Survival Probability` = mortality::survival_function_GM(t = t,
                                                                     A = input$GM_A,
                                                                     R = input$GM_R,
                                                                     alpha = input$GM_alpha),
            `Life Expectancy` = mortality::life_expectancy_GM(t = t,
                                                              A = input$GM_A,
                                                              R = input$GM_R,
                                                              alpha = input$GM_alpha)
          )
        )
      }
    )

    # Plot of Gompertz-Makeham Force of Mortality
    output$GM_plot_mu <- shiny::renderPlot(
      res = plot_resolution,
      {
        data_GM() %>%
          ggplot2::ggplot(mapping = ggplot2::aes(x = t, y = `Force of Mortality`)) +
          ggplot2::geom_line(size = 1) +
          ggplot2::scale_y_continuous(trans='log10',
                                      breaks = c(10^(-5:0)),
                                      limits = c(0.00001,1)) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "bottom")
      })

    # Plot of Gompertz-Makeham Survival Probability
    output$GM_plot_sur <- shiny::renderPlot(
      res = plot_resolution,
      {
        data_GM() %>%
          ggplot2::ggplot(mapping = ggplot2::aes(x = t, y = `Survival Probability`)) +
          ggplot2::geom_line(size = 1) +
          ggplot2::scale_y_continuous(breaks = (0:5)/5,
                                      limits = c(0,1)) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "bottom")
      })

    # Plot of Gompertz-Makeham Life Expectancy
    output$GM_plot_exp <- shiny::renderPlot(
      res = plot_resolution,
      {
        data_GM() %>%
          ggplot2::ggplot(mapping = ggplot2::aes(x = t, y = `Life Expectancy`)) +
          ggplot2::geom_line(size = 1) +
          ggplot2::scale_y_continuous(breaks = (0:7)*10,
                                      limits = c(0, 70)) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "bottom")
      })

  }

  shiny::shinyApp(ui = ui,
                  server = server)

}
