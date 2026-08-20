# ============================================================
# SERVER.R
# Policyholder Transition Model
# ============================================================

server <- function(input, output, session) {


  # ==========================================================
  # INPUT MATRIX / INITIAL DISTRIBUTION (Model Inputs tab)
  # ==========================================================

  output$input_matrix <- renderTable({

    format_matrix(base_matrix)

  }, striped = TRUE, bordered = FALSE)


  output$initial_table <- renderTable({

    data.frame(
      State = states,
      Initial_Probability = paste0(
        round(initial_distribution * 100, 2),
        "%"
      )
    )

  }, striped = TRUE, bordered = FALSE)


  # ==========================================================
  # DATA & ESTIMATION
  # ==========================================================

  output$assumed_matrix_display <- renderTable({

    format_matrix(base_matrix)

  }, striped = TRUE, bordered = FALSE)


  # Re-run the simulation only when the button is pressed (or on
  # first load), rather than on every keystroke in the numeric
  # inputs — keeps the app responsive for a task that can involve
  # thousands of simulated policyholder-years.
  simulated_data <- eventReactive(
    input$run_simulation,
    {

      validate(
        need(input$sim_n_policyholders >= 50, "Please simulate at least 50 policyholders."),
        need(input$sim_n_periods >= 1, "Please simulate at least 1 period.")
      )

      simulate_policyholder_paths(
        M = base_matrix,
        initial_dist = initial_distribution,
        n_policyholders = input$sim_n_policyholders,
        n_periods = input$sim_n_periods,
        seed = input$sim_seed
      )

    },
    ignoreNULL = FALSE
  )


  estimated_matrix <- reactive({

    estimate_transition_matrix(
      simulated_data(),
      state_names = states
    )

  })


  output$estimated_matrix_display <- renderTable({

    format_matrix(estimated_matrix())

  }, striped = TRUE, bordered = FALSE)


  output$estimation_note <- renderUI({

    est <- estimated_matrix()

    max_abs_diff <- max(abs(est - base_matrix))

    div(

      class = "info-box",

      p(
        strong("Largest absolute difference: "),
        strong(sprintf("%.4f", max_abs_diff)),
        " between any single assumed and estimated entry."
      ),

      p(
        "With more simulated policyholders and/or more observed periods, the estimated ",
        "matrix converges towards the assumed matrix — this is exactly what would happen ",
        "with a growing block of real policyholder experience."
      )
    )

  })


  # ==========================================================
  # PORTFOLIO PROJECTION
  # ==========================================================

  projection_distribution <- reactive({

    req(input$projection_years)

    validate(
      need(input$projection_years >= 0, "Projection horizon must be zero or greater.")
    )

    project_portfolio(
      base_matrix,
      input$projection_years
    )

  })


  # ----------------------------------------------------------
  # Projection plot
  # ----------------------------------------------------------

  portfolio_projection_data <- reactive({

    req(input$projection_years)

    horizon <- input$projection_years

    years <- 0:horizon

    projection_data <- data.frame()

    for (yr in years) {

      dist <- project_portfolio(
        base_matrix,
        yr
      )

      temp <- data.frame(
        Year = yr,
        State = names(dist),
        Probability = as.numeric(dist)
      )

      projection_data <- rbind(
        projection_data,
        temp
      )
    }

    projection_data

  })


  output$portfolio_plot <- renderPlot({

    projection_data <- portfolio_projection_data()

    ggplot(
      projection_data,
      aes(
        x = Year,
        y = Probability,
        group = State,
        color = State,
        linetype = State
      )
    ) +

      geom_line(
        linewidth = 1.2
      ) +

      geom_point(
        size = 2
      ) +

      scale_color_manual(
        values = state_colors
      ) +

      scale_y_continuous(
        labels = function(x) paste0(
          round(x * 100),
          "%"
        ),
        limits = c(0, 1)
      ) +

      scale_x_continuous(
        breaks = pretty(projection_data$Year)
      ) +

      labs(
        title = "Projected Portfolio Distribution",
        x = "Year",
        y = "Portfolio Distribution",
        color = "State",
        linetype = "State"
      ) +

      theme_minimal(base_size = 14) +

      theme(
        plot.title = element_text(
          face = "bold",
          color = "#123F3C"
        ),

        axis.title = element_text(
          color = "#123F3C",
          face = "bold"
        ),

        legend.title = element_text(
          face = "bold"
        ),

        panel.grid.minor = element_blank()
      )

  })


  # ----------------------------------------------------------
  # Projection table + download
  # ----------------------------------------------------------

  projection_table_data <- reactive({

    dist <- projection_distribution()

    data.frame(

      State = names(dist),

      Probability = paste0(
        round(dist * 100, 4),
        "%"
      ),

      Expected_Policyholders = round(
        dist * input$portfolio_size,
        2
      )

    )

  })


  output$projection_table <- renderTable({

    projection_table_data()

  }, striped = TRUE, bordered = FALSE)


  output$download_projection <- downloadHandler(

    filename = function() {
      paste0("portfolio_projection_year_", input$projection_years, ".csv")
    },

    content = function(file) {
      write.csv(projection_table_data(), file, row.names = FALSE)
    }
  )


  # ----------------------------------------------------------
  # Illustrative financial impact
  # ----------------------------------------------------------

  financial_projection <- reactive({

    req(input$projection_years, input$avg_claim_cost, input$portfolio_size)

    validate(
      need(input$avg_claim_cost >= 0, "Average claim cost cannot be negative.")
    )

    horizon <- input$projection_years
    years <- 0:horizon

    claim_prob <- sapply(
      years,
      function(yr) project_portfolio(base_matrix, yr)["Claim"]
    )

    expected_in_claim <- claim_prob * input$portfolio_size
    expected_cost <- expected_in_claim * input$avg_claim_cost

    data.frame(
      Year = years,
      Claim_Probability = paste0(round(claim_prob * 100, 3), "%"),
      Expected_Policyholders_In_Claim = round(expected_in_claim, 2),
      Expected_Cost = round(expected_cost, 2)
    )

  })


  output$financial_table <- renderTable({

    financial_projection()

  }, striped = TRUE, bordered = FALSE)


  output$total_expected_claim_cost <- renderText({

    total <- sum(financial_projection()$Expected_Cost)

    formatC(total, format = "f", digits = 0, big.mark = ",")

  })


  # ==========================================================
  # N-STEP TRANSITION MATRIX
  # ==========================================================

  output$n_step_matrix <- renderTable({

    n <- input$n_steps

    req(n)

    validate(
      need(n >= 0, "n must be zero or greater.")
    )

    Pn <- matrix_power(
      base_matrix,
      n
    )

    rownames(Pn) <- states
    colnames(Pn) <- states

    format_matrix(Pn)

  }, striped = TRUE, bordered = FALSE)


  # ----------------------------------------------------------
  # N-step interpretation
  # ----------------------------------------------------------

  output$transition_interpretation <- renderUI({

    n <- input$n_steps

    req(n)

    Pn <- matrix_power(
      base_matrix,
      n
    )

    rownames(Pn) <- states
    colnames(Pn) <- states

    active_to_closed <- Pn[1, 4]
    active_to_active <- Pn[1, 1]

    div(

      class = "info-box",

      p(
        strong(
          paste0(
            "After ",
            n,
            " transition(s): "
          )
        ),

        "a policyholder starting in the Active state has a ",

        strong(
          paste0(
            round(active_to_closed * 100, 2),
            "%"
          )
        ),

        " probability of being Closed."
      ),

      p(
        "The probability of still being Active is ",

        strong(
          paste0(
            round(active_to_active * 100, 2),
            "%"
          )
        ),

        "."
      )
    )

  })


  # ==========================================================
  # STATIONARY DISTRIBUTION
  # ==========================================================

  stationary_distribution <- reactive({

    calculate_stationary(
      base_matrix
    )

  })


  output$stationary_table <- renderTable({

    pi <- stationary_distribution()

    data.frame(

      State = names(pi),

      Stationary_Probability = paste0(
        sprintf(
          "%.4f",
          pi * 100
        ),
        "%"
      )

    )

  }, striped = TRUE, bordered = FALSE)


  output$stationary_interpretation <- renderUI({

    pi <- stationary_distribution()

    absorbing <- find_absorbing_states(
      base_matrix
    )

    if (length(absorbing) > 0) {

      div(

        class = "warning-box",

        strong("Interpretation: "),

        "The current transition structure contains absorbing ",
        "state(s): ",

        strong(
          paste(
            absorbing,
            collapse = ", "
          )
        ),

        ". Therefore, the long-run result should be interpreted ",
        "in the context of absorption rather than as a stable ",
        "business equilibrium between all states."
      )

    } else {

      div(

        class = "info-box",

        strong("Interpretation: "),

        "The stationary distribution represents the long-run ",
        "equilibrium distribution implied by the transition matrix."
      )

    }

  })


  # ==========================================================
  # ABSORBING STATES
  # ==========================================================

  output$absorbing_table <- renderTable({

    absorbing <- find_absorbing_states(
      base_matrix
    )

    data.frame(

      State = states,

      Absorbing = ifelse(
        states %in% absorbing,
        "Yes",
        "No"
      )

    )

  }, striped = TRUE, bordered = FALSE)


  output$absorbing_message <- renderUI({

    absorbing <- find_absorbing_states(
      base_matrix
    )

    if (length(absorbing) > 0) {

      div(

        class = "warning-box",

        strong(
          paste0(
            "Absorbing state(s): ",
            paste(absorbing, collapse = ", "),
            ". "
          )
        ),

        "Once a policyholder enters an absorbing state, ",
        "the model does not allow them to leave it."
      )

    }

  })


  # ==========================================================
  # EXPECTED TIME TO ABSORPTION  (NEW)
  # ==========================================================

  output$absorption_time_table <- renderTable({

    t_vec <- expected_time_to_absorption(base_matrix)

    data.frame(
      State = names(t_vec),
      Expected_Periods_To_Absorption = round(t_vec, 2)
    )

  }, striped = TRUE, bordered = FALSE)


  # ==========================================================
  # SCENARIO ANALYSIS
  # ==========================================================

  # Reset the three lapse-rate inputs back to their defaults.
  observeEvent(input$reset_scenarios, {

    updateNumericInput(session, "high_lapse", value = 0.15)
    updateNumericInput(session, "base_lapse", value = 0.08)
    updateNumericInput(session, "low_lapse", value = 0.03)

  })


  output$scenario_input_warning <- renderUI({

    req(input$high_lapse, input$base_lapse, input$low_lapse)

    max_lapse <- base_matrix["Active", "Lapsed"] + base_matrix["Active", "Active"]

    flagged <- c(
      if (input$high_lapse > max_lapse) "High lapse",
      if (input$base_lapse > max_lapse) "Base case",
      if (input$low_lapse > max_lapse) "Low lapse"
    )

    if (length(flagged) > 0) {

      div(
        class = "warning-box",

        strong("Note: "),

        paste(
          paste(flagged, collapse = ", "),
          "exceed(s) the maximum lapse rate this matrix structure supports"
        ),

        sprintf(" (%.2f). The value used has been capped at that maximum.", max_lapse)
      )

    }

  })


  scenario_matrices <- reactive({

    req(input$high_lapse, input$base_lapse, input$low_lapse)

    validate(
      need(input$high_lapse >= 0, "High lapse rate must be zero or greater."),
      need(input$base_lapse >= 0, "Base lapse rate must be zero or greater."),
      need(input$low_lapse >= 0, "Low lapse rate must be zero or greater.")
    )

    list(

      "Base Case" =
        scenario_matrix(
          input$base_lapse
        ),

      "High Lapse" =
        scenario_matrix(
          input$high_lapse
        ),

      "Low Lapse" =
        scenario_matrix(
          input$low_lapse
        )

    )

  })


  # ==========================================================
  # SCENARIO PLOT
  # ==========================================================

  scenario_plot_data <- reactive({

    scenarios <- scenario_matrices()

    horizon <- input$projection_years

    all_data <- data.frame()

    for (scenario_name in names(scenarios)) {

      M <- scenarios[[scenario_name]]

      for (yr in 0:horizon) {

        dist <- project_portfolio(
          M,
          yr
        )

        temp <- data.frame(

          Scenario = scenario_name,

          Year = yr,

          State = names(dist),

          Probability = as.numeric(dist)

        )

        all_data <- rbind(
          all_data,
          temp
        )
      }
    }

    subset(
      all_data,
      State %in% c(
        "Active",
        "Lapsed",
        "Closed"
      )
    )

  })


  output$scenario_plot <- renderPlot({

    plot_data <- scenario_plot_data()

    ggplot(
      plot_data,
      aes(
        x = Year,
        y = Probability,
        color = State,
        linetype = State
      )
    ) +

      geom_line(
        linewidth = 1.15
      ) +

      facet_wrap(
        ~Scenario,
        nrow = 1
      ) +

      scale_color_manual(
        values = state_colors
      ) +

      scale_y_continuous(
        labels = function(x) paste0(
          round(x * 100),
          "%"
        ),
        limits = c(0, 1)
      ) +

      labs(
        title = "Impact of Lapse Assumptions",
        x = "Year",
        y = "Portfolio Distribution",
        color = "State",
        linetype = "State"
      ) +

      theme_minimal(base_size = 14) +

      theme(

        plot.title = element_text(
          face = "bold",
          color = "#123F3C"
        ),

        strip.text = element_text(
          face = "bold",
          color = "#123F3C"
        ),

        axis.title = element_text(
          face = "bold",
          color = "#123F3C"
        ),

        panel.grid.minor = element_blank(),

        legend.position = "bottom"
      )

  })


  # ==========================================================
  # SCENARIO RESULTS + DOWNLOAD
  # ==========================================================

  scenario_results_data <- reactive({

    scenarios <- scenario_matrices()

    horizon <- input$projection_years

    results <- data.frame()

    for (scenario_name in names(scenarios)) {

      M <- scenarios[[scenario_name]]

      dist <- project_portfolio(
        M,
        horizon
      )

      temp <- data.frame(

        Scenario = scenario_name,

        State = names(dist),

        Probability = paste0(
          round(dist * 100, 4),
          "%"
        ),

        Expected_Policyholders = round(
          dist * input$portfolio_size,
          2
        )

      )

      results <- rbind(
        results,
        temp
      )
    }

    results

  })


  output$scenario_results <- renderTable({

    scenario_results_data()

  }, striped = TRUE, bordered = FALSE)


  output$download_scenarios <- downloadHandler(

    filename = function() {
      paste0("scenario_results_year_", input$projection_years, ".csv")
    },

    content = function(file) {
      write.csv(scenario_results_data(), file, row.names = FALSE)
    }
  )


  # ==========================================================
  # SCENARIO INTERPRETATION
  # ==========================================================

  output$scenario_interpretation <- renderUI({

    high_M <- scenario_matrix(
      input$high_lapse
    )

    base_M <- scenario_matrix(
      input$base_lapse
    )

    low_M <- scenario_matrix(
      input$low_lapse
    )


    high_dist <- project_portfolio(
      high_M,
      input$projection_years
    )

    base_dist <- project_portfolio(
      base_M,
      input$projection_years
    )

    low_dist <- project_portfolio(
      low_M,
      input$projection_years
    )


    div(

      class = "info-box",

      p(

        strong("High lapse scenario: "),

        "Increasing the Active → Lapsed transition probability ",
        "increases the proportion of policyholders expected to ",
        "lapse over the projection period."
      ),

      p(

        strong("Low lapse scenario: "),

        "Reducing the Active → Lapsed transition probability ",
        "allows a greater proportion of policyholders to remain ",
        "Active."
      ),

      p(

        strong("Actuarial interpretation: "),

        "Scenario analysis helps assess how sensitive portfolio ",
        "projections are to assumptions about policyholder behaviour."
      ),

      p(

        strong("At the selected horizon: "),

        "the Active proportion is ",

        strong(
          paste0(
            round(base_dist["Active"] * 100, 2),
            "%"
          )
        ),

        " under the base case, compared with ",

        strong(
          paste0(
            round(high_dist["Active"] * 100, 2),
            "%"
          )
        ),

        " under high lapse and ",

        strong(
          paste0(
            round(low_dist["Active"] * 100, 2),
            "%"
          )
        ),

        " under low lapse."
      )

    )

  })


  # ==========================================================
  # METHODOLOGY MATRIX
  # ==========================================================

  output$method_matrix <- renderTable({

    format_matrix(
      base_matrix
    )

  }, striped = TRUE, bordered = FALSE)

}
