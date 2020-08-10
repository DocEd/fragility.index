#' Calculate Average Treatment Effect
#' 
#' returns the average treatment effect given treatment assignment in a
#' randomised scenario
#'
#' @param t treatment assignment (must be 0 or 1)
#' @param y outcome (must be 0 or 1)
ate_calc <- function(t, y) {
  mean(y[t == 1]) - mean(y[t == 0])
}

#' Simulate Fragility Index
#'
#' This is the workhorse function of the study. It calculates the appropriate
#' number of subjects for a given effect size (specified as a control and
#' intervention mortality), power and alpha. Simulations are repeated a default
#' 1000 times (set by sims). Alternately, one can override this behaviour by
#' specifying the number of subjects directly (set by N).
#'
#' The `permit_negative` refers to whether or not you want the FI to return
#' values for when the starting p value is already > 0.05.
#'
#' A tibble is returned with list columns containing the original treatment
#' allocations and outcomes. A number of other metrics are returned, some
#' observed, some defined apriori. A column specifying `reverse_effect`
#' identifies cases where the reverse effect was present (i.e. harm observed in
#' the treatment arm). These cases should either be excluded, or flipped for
#' them to be valid and interpretable.
#' 
#' @importFrom rlang abort
#' @importFrom stats power.prop.test chisq.test fisher.test
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows bind_cols mutate if_else
#' @importFrom magrittr %>%
#' @importFrom purrr map2_dbl
#'
#' @param intervention_mort treatment arm mortality: scalar [0, 1]
#' @param power desired statistical power: scalar [0, 1]
#' @param control_mort control arm mortality: scalar [0, 1]
#' @param n manual selection of subjects in total (overrides other settings)
#' @param sims number of simulations to run: default 1000
#' @param alpha pre-fixed alpha level to determine a "positive" finding.
#' @param permit_negative allow negative fragility index: default FALSE
simulate_fragility <- function(intervention_mort,
                               power = 0.8,
                               control_mort,
                               n = NULL,
                               sims = 1000,
                               alpha = 0.05,
                               permit_negative = FALSE) {
  
  if (intervention_mort > control_mort) {
    rlang::abort(
    "Your intervention mortality is greater than your control
     mortality. The Fragility Index is not defined under these
     conditions"
    )
  }
  
  if (is.null(n)) {
    pwr <- power.prop.test(
      p1 = intervention_mort,
      p2 = control_mort,
      power = power)
    n <- as.integer(pwr$n)*2
  }
  
  # Ensure study uses even number of patients for balance.
  if (n%%2 == 1) {
    n <- n + 1L
  }
  
  # Set up objects to store values we are interested in capturing
  p_value <- rep(NA, sims)
  fragility <- rep(NA, sims)

  # Set up some list columns to store treatment allocation and outcomes
  df <- tibble(
    t = as.list(NULL),
    y = as.list(NULL)
  )
  
  # Loop over simulations
  for (i in seq_len(sims)){
    
    # Randomly assign half to the treatment, and half to control
    # I want exactly half in each group, so will use sample to
    # guarantee this behaviour
    treatment <- sample(1:n, size = n/2, replace = FALSE)
    
    # Vector for the treatment assignment
    t <- rep(as.integer(NA), n)
    
    # Randomly assign half to treatment, half to control
    t[treatment] <- 1L
    t[-treatment] <- 0L
    
    # vector for the outcome
    y <- rep(NA, n)
    
    # Outcome WITH treatment, note 1 = death, 0 = survive
    # Bernoilli trial with p = intervention_mortality
    y[treatment] <- rbinom(n = n/2, size = 1, prob = intervention_mort)
    oyt <- mean(y[t == 1])
    # Outcome WITHOUT treatment, note 1 = death, 0 = survive
    # Bernoilli trial with p = control_mortality
    y[-treatment] <- rbinom(n = n/2, size = 1, prob = control_mort)
    oyc <- mean(y[t == 0])
    # Test the difference in groups
    test <- chisq.test(y, t)
    p <- p_value[i] <- test$p.value
    
    # Start the fragility index counter at 0
    fi <- 0
    
    # Grab the salient parts of the contingency table
    # (convenience to make code easier to read)
    con_tbl <- as.matrix(table(t, y))
    intervention_death <- con_tbl[2, 2]
    intervention_survive <- con_tbl[2, 1]
    control_death <- con_tbl[1, 2]
    control_survive <- con_tbl[1, 1]
    
    # If "significant" start FI procedure.
    if (p <= alpha) {
      while(p < alpha) {
        # Change the outcome for someone in the intervention arm
        intervention_death <- intervention_death + 1
        intervention_survive <- intervention_survive - 1
        if (intervention_survive < 0) {
          abort(
          "negative people are being generated, you probably have too
          few people in your intervention arm for this to work.")
        }
        fi <- fi + 1
        mat <- matrix(
          c(control_survive,
            control_death,
            intervention_survive,
            intervention_death),
          nrow=2, byrow = TRUE)
        test <- fisher.test(mat)
        p <- test$p.value
      }
    } else {
      if (permit_negative) {
        while(p >= alpha) {
          # Change the outcome for someone in the intervention arm
          # The fragility index was not originally defined for "non-significant"
          # Studies... but since it is just a transformation of the p value
          # It is no less valid in this direction.
          intervention_death <- intervention_death - 1
          intervention_survive <- intervention_survive + 1
          if (intervention_death < 0) {
            abort(
            "negative people are being generated, you probably have too
             few people in your intervention arm for this to work.")
          }
          fi <- fi - 1
          mat <- matrix(
            c(control_survive,
              control_death,
              intervention_survive,
              intervention_death),
            nrow=2, byrow = TRUE)
          test <- fisher.test(mat)
          p <- test$p.value
        }
      } else {
        fi <- as.integer(NA)
      }
    }
    fragility[i] <- fi
    
    # Capture the treatment allocation and outcome for later.
    df <- bind_rows(
      df, tibble(t = list(t), y = list(y))
      )
  }
  
  # Bind the simulated study values to p and fragility values.
  df <- bind_cols(
    df, tibble(p_value, fragility)
  )
  
  # Occasionally, the reverse effect is seen (randomness). We need
  # to add a flag for this. There is nothing wrong with this, but it
  # creates a symmetrical discontinuity in the FI.
  df <- df %>%
    mutate(
      ratio = map2_dbl(.x = t, .y = y, .f = function(a, b) {
        con_tbl <- as.matrix(table(a, b))
        intervention_survive <- con_tbl[2, 1]
        control_survive <- con_tbl[1, 1]
        return(intervention_survive/control_survive)
        }),
      reverse_effect = if_else(
        ratio < 1, TRUE, FALSE
      )
    ) %>%
    select(-ratio) %>%
    mutate(power = power,
           c_y = control_mort,
           oc_y = oyc,
           t_y = intervention_mort,
           ot_y = oyt,
           arr = control_mort-intervention_mort,
           rrr = (control_mort-intervention_mort)/control_mort,
           n = n,
           ate = map2_dbl(t, y, ate_calc))

}
