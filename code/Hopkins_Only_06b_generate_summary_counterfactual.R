# Sarah Goldfarb
# 03/09/2026

# TODO BEFORE RUNNING: define which sites have contributed
sites <- c("Hopkins", "UCMC", "emory", "NU", "OHSU", 'UMN', "UCSF", "rush", "Michigan", "penn")
units_all <- c("icu", "ward", "stepdown")

{ # Setup
  
  { # Load needed packages
    packages <- c(
      "tidyverse",
      "yaml",
      "rprojroot",
      "metafor",
      "dplyr",
      "ggplot2",
      "forcats",
      "scales"
    )
    
    install_if_missing <- function(package) {
      if (!require(package, character.only = TRUE)) {
        install.packages(package, dependencies = TRUE)
        library(package, character.only = TRUE)
      }
    }
    
    sapply(packages, install_if_missing)
    rm(packages, install_if_missing)
  } # Load needed packages
  
  { # Load config to specify local paths
    # Find project root
    project_root <- find_root(rprojroot::has_dir("config"))
    
    # Read YAML config
    config <- yaml::read_yaml(file.path(project_root, "config", "config.yaml"))
    global_config <- yaml::read_yaml(file.path(project_root, "config", "global_config.yaml"))
    
    # Assign config values to R variables
    project_location <- config$project_location
    
    covariates <- global_config$covariates
    outcomes_binary <- global_config$outcomes_binary
    outcomes_counts <- global_config$outcomes_counts
    
    rm(config, global_config)
  } # Load config to specify local paths
  
  { # Create directories as needed
    
    output_dir <- paste0(project_location, "/compiled_final_results/")
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
    
  } # Create directories as needed
  
  { # Load covariate names and beta error
    cov_names <- read_csv(paste0(project_location, 
                                 "/global_model_outputs/cov_names.csv"),
                          show_col_types = FALSE)|>pull(term)
    # Global fixed effect of each covariate (determined in file 4b)
    global_coeff_vars <- read_csv(paste0(
      project_location,
      "/global_model_outputs/global_coeff_vars_by_hosp.csv"
    ), 
    show_col_types=FALSE)
    
    global_coeff_vars_secondary <- read_csv(paste0(
      project_location,
      "/global_model_outputs/global_coeff_vars_imc_icu_together.csv"
    ), 
    show_col_types=FALSE)
  } # Load covariate names
  
  { # Load all event rates
    
    # Initialize the dataframe of event rates
    all_event_rates = secondary_event_rates = data.frame(
      outcome=character(),
      unit=character(),
      local_hospital=character(),
      n=integer(),
      clif_hospital=character(),
      sum_risks=numeric(),
      sum_w=numeric(),
      stringsAsFactors = FALSE
    )
    
    all_xw = secondary_xw <- data.frame(
      outcome = character(),
      unit = character(),
      local_hospital = character(),
      clif_hospital = character(),
      setNames(
        replicate(length(cov_names), numeric(0), simplify = FALSE),
        cov_names
      ),
      check.names=FALSE
    )
    
    hospital_data <- data.frame(
      first_hospital_id = character(),
      n_patients=numeric(),
      imc_capable=numeric(),
      academic_community=character(),
      stringsAsFactors = FALSE
    )
    
    
    
    # Iterate over each site and read in the gamma values
    for(site in sites){
      all_event_rates <- all_event_rates |>
        add_row(read_csv(paste0(project_location, site,
                                "_project_output/local_model_outputs/", site,
                                "_all_event_rates.csv"),
                         show_col_types = FALSE) |>
                  mutate(local_hospital = as.character(local_hospital)))
      
      all_xw <- all_xw |>
        add_row(read_csv(paste0(project_location, site,
                                "_project_output/local_model_outputs/", site,
                                "_all_xw.csv"),
                         show_col_types = FALSE)|>
                  mutate(local_hospital = as.character(local_hospital)))
      
      hospital_data <- hospital_data |>
        add_row(read_csv(paste0(
            project_location,site, "_project_output/", site,"_hospital_data.csv"
            ),
            show_col_types =FALSE)|>
              mutate(first_hospital_id = as.character(first_hospital_id))
          )
      
      # Only read in from sites that have hospitals that are imc capable
      if(site %in% c("Hopkins", "UMN", "OHSU")){ # , "UCSF") & # Removed UCSF since ICU only, NU also ICU only
         #if(nrow(hospital_data|>filter(imc_capable==1))){
      
        secondary_event_rates <- secondary_event_rates |>
          add_row(read_csv(paste0(project_location, site,
                                  "_project_output/local_model_outputs/", site,
                                  "_secondary_event_rates.csv"),
                           show_col_types = FALSE))
        
        secondary_xw <- secondary_xw |>
          add_row(read_csv(paste0(project_location, site,
                                  "_project_output/local_model_outputs/", site,
                                  "_secondary_xw.csv"),
                           show_col_types = FALSE))
      }
      
    }
    
  } # Load all event rates
  
} # Setup


rser_ci_from_site_summaries <- function(
    N_total,
    sum_p,
    sum_w,
    sum_xw,
    var_beta, # covariance matrix of covariate coeffs
    z = qnorm(0.975)
    ) {       
  
  # RSER estimate is the weighted average across sites
  p_hat <- sum_p / N_total
  
  # gradients of RSER
  g_gamma <- sum_w / N_total
  g_beta  <- sum_xw / N_total
  
  # variance for gamma_hat
  I11 <- sum_w
  I12 <- sum_xw
  var_gamma <- as.numeric(1 / I11 + t(I12) %*% var_beta %*% I12 / (I11^2))
  
  # delta method variance for RSER
  var_rser <- as.numeric(g_gamma^2 * var_gamma + t(g_beta) %*% var_beta %*% g_beta)
  se_rser <- sqrt(var_rser)
  
  expit <- function(x){
    1/(1+exp(-x))
  }
  
  # Logit scale CI
  logit_hat <- log(p_hat/(1-p_hat))
  se_logit <- se_rser / (p_hat * (1-p_hat))
  lower <- expit(logit_hat - z * se_logit)
  upper <- expit(logit_hat + z * se_logit)
  
  outcome_df <- data.frame(p_hat = p_hat,
             var   = var_rser,
             se    = se_rser,
             lower_prob = p_hat - z * se_rser,
             upper_prob = p_hat + z * se_rser,
             lower_log = lower,
             upper_log = upper)
  
  outcome_df <- outcome_df|> mutate(
    width_prob = upper_prob-lower_prob,
    width_log = upper_log-lower_log
  )
  
  return(outcome_df)
}

{ # Calculate final event rate for each hospital at each level of care for each outcome
  
  calc_final_event_rate <- function(my_event_rates,
                                    my_xw,
                                    my_coeff_vars,
                                    units=units_all,
                                    incl_unit_in_colname = T){
    
    final_event_rate_combined <- data.frame(
      outcome=character(),
      unit=character(),
      clif_hospital = character(),
      event_rate = numeric(),
      n_patients=integer(),
      contributing_hospitals=character(),
      stringsAsFactors = FALSE
    )
    
    # rser_ci_results <- data.frame(
    #   outcome=character(),
    #   unit=character(),
    #   clif_hospital = character(),
    #   p_hat = numeric(),
    #   var = numeric(),
    #   se = numeric(),
    #   lower = numeric(),
    #   upper=numeric(),
    #   stringsAsFactors = FALSE
    # )   
    rser_ci_results <- data.frame(
      outcome=character(),
      unit=character(),
      clif_hospital = character(),
      p_hat = numeric(),
      var = numeric(),
      se = numeric(),
      lower_prob = numeric(),
      upper_prob=numeric(),
      lower_log = numeric(),
      upper_log=numeric(),
      width_prob = numeric(),
      width_log=numeric(),
      stringsAsFactors = FALSE
    )
    
    # Iterate over each outcome and unit
    for(outcome_i in outcomes_binary){
      for(unit_i in units){
        
        # Primary analysis
        if(incl_unit_in_colname == TRUE){
          # Saved variance of the covariate coefficient vector from prior file
          # Save as diagonal matrix
          var_beta <- diag(my_coeff_vars[[paste0(outcome_i, ".", unit_i)]])
        }
        # Secondary analysis
        else{
          var_beta <- diag(my_coeff_vars[[outcome_i]])
        }
        
        # Subset event rates to only for this outcome and unit
        event_rate <- my_event_rates |>
          filter(outcome==outcome_i, unit==unit_i) |>
          select(-c(outcome, unit)) |>
          # Summarize the event rate, grouped by counterfactual clif hospital
          group_by(clif_hospital) |>
          summarise(
            n = sum(n),
            sum_risks = sum(sum_risks),
            sum_w = sum(sum_w),
            contributing_hospitals = paste(sort(unique(local_hospital)), collapse = ", "),
            .groups = "drop"
          ) |>
          mutate(event_rate = sum_risks/n, 
                 outcome=outcome_i, unit=unit_i)
        
        # Subset xw (derivative) to only for this outcome and unit
        xw_i <- my_xw |> 
          filter(outcome==outcome_i, unit==unit_i) |>
          select(-c(outcome, unit)) |>
          # Summarize the xw (derivative), grouped by counterfactual clif hospital
          group_by(clif_hospital) |>
          summarize(
            across(all_of(cov_names), sum, na.rm = TRUE),
            .groups = "drop"
          )
        
        for(clif_hosp_j in unique(event_rate$clif_hospital)){
          
          event_rate_j <- event_rate|>filter(clif_hospital==clif_hosp_j)
          
          xw_j <- unlist((xw_i|>filter(clif_hospital==clif_hosp_j)|>select(-clif_hospital))[1,], use.names=TRUE)
          
          rser_ci_results <- rser_ci_results |>
            add_row(rser_ci_from_site_summaries(event_rate_j$n, 
                                      event_rate_j$sum_risks, 
                                      event_rate_j$sum_w,
                                      xw_j,
                                      var_beta) |>
                      mutate(
                        clif_hospital = clif_hosp_j,
                        unit=unit_i,
                        outcome=outcome_i
                      ))
        }
        
        final_event_rate_combined <- final_event_rate_combined |>
          add_row(event_rate |>
                    select(outcome, unit, clif_hospital, event_rate,
                           n_patients=n,
                           contributing_hospitals))
        
      }
    }
    
    if(nrow(final_event_rate_combined) != nrow(rser_ci_results)){
      stop("Error: final event rate and CI results do not have the same number of rows")
    }
    
    final_output <- final_event_rate_combined |>
      left_join(rser_ci_results, by=c("outcome", "unit", "clif_hospital"))
    
    if(!isTRUE(all.equal(final_output$event_rate, final_output$p_hat))){
      stop("Error: final event rate and CI results do not have the same number of rows")
    }
    
    return(final_output)
  }
  
  final_output <- calc_final_event_rate(all_event_rates,
                                                    all_xw,
                                                    global_coeff_vars)
  
  secondary_output <- calc_final_event_rate(secondary_event_rates,
                                            secondary_xw,
                                            global_coeff_vars_secondary,
                                            incl_unit_in_colname = F)
  
  write_csv(final_output, paste0(output_dir,"final_output.csv"))
  write_csv(final_output, paste0(output_dir,"secondary_output"))
  
} # Calculate final event rate for each hospital at each level of care for each outcome

{ # Group by IMC capable vs incapable
  outcome_comparisons_raw <- final_output |>
    select(outcome, unit, clif_hospital,
           estimate=p_hat,se, lower_prob, upper_prob)|>
    left_join(hospital_data |>
                select(first_hospital_id, hosp_size = n_patients, imc_capable, academic_community), 
              by=c("clif_hospital"= "first_hospital_id"))
  
  outcome_comparisons <- data.frame(
    outcome = character(),
    unit = character(),
    imc_capable = numeric(),
    estimate = numeric(),
    se = numeric(),
    lower_prob = numeric(),
    upper_prob = numeric(),
    total_size = numeric(),
    stringsAsFactors = F
  )
  
  for(outcome_i in outcomes_binary){
    for(unit_i in c("icu", "ward")){
      outcome_comparisons <- outcome_comparisons |>
        add_row(
          outcome_comparisons_raw |>
            filter(outcome==outcome_i & unit==unit_i) |>
            group_by(imc_capable) |>
            mutate(weight=hosp_size/sum(hosp_size, na.rm=T)) |>
            summarize(
              estimate = sum(weight * estimate, na.rm = TRUE),
              se = sqrt(sum(weight^2 * se^2, na.rm = TRUE)),
              lower_prob = estimate - 1.96 * se,
              upper_prob = estimate + 1.96 * se,
              total_size = sum(hosp_size, na.rm = TRUE)
            ) |>
            ungroup() |>
            mutate(outcome=outcome_i, unit=unit_i)
        )
    }
  }
  
  run_rd <- function(df) {
    if (length(unique(df$imc_capable)) < 2) return(NULL)   # need both groups present
    
    m  <- rma(yi = estimate, sei = se, mods = ~ imc_capable, data = df)
    cs <- as.data.frame(coef(summary(m)))
    rn <- intersect(c("imc_capable", "imc_capable1"), rownames(cs))[1]  # numeric or factor
    b  <- cs[rn, ]
    
    p0 <- predict(m, newmods = 0)   # fitted proportion, non-capable
    p1 <- predict(m, newmods = 1)   # fitted proportion, capable
    
    data.frame(
      outcome = df$outcome[1],
      unit    = df$unit[1],
      n_hosp  = nrow(df),
      pct_non = p0$pred  * 100,
      pct_cap = p1$pred  * 100,
      rd_pp   = b$estimate * 100,            # risk difference (capable - non), pp
      lcl_pp  = b$ci.lb   * 100,
      ucl_pp  = b$ci.ub   * 100,
      p_value = b$pval,
      tau2    = m$tau2,                       # residual between-hospital heterogeneity
      row.names = NULL
    )
  }
  
  results <- do.call(rbind,
                     lapply(split(outcome_comparisons_raw, ~ outcome + unit, drop = TRUE), run_rd))
  
  num <- c("pct_non","pct_cap","rd_pp","lcl_pp","ucl_pp")
  results[num]    <- round(results[num], 1)
  results$p_value <- signif(results$p_value, 3)
  results$tau2    <- signif(results$tau2, 3)
  
  print(results, row.names = FALSE)

} # Group by IMC capable vs incapable

{ # plot forest plots
  
  
  library(ggplot2)
  
  # `results` = your table; output_dir <- "/your/path/"  (end with a slash)
  
  make_forest <- function(df, outcome_key, outcome_title) {
    
    d <- df[df$outcome == outcome_key, ]
    
    d$unit <- factor(d$unit, levels = c("ward", "icu"),
                     labels = c("Ward", "ICU"))
    
    d$lab <- sprintf("%+.1f pp  (95%% CI %+.1f to %+.1f),  p = %.3f",
                     d$rd_pp, d$lcl_pp, d$ucl_pp, d$p_value)
    
    xmax  <- max(abs(c(d$lcl_pp, d$ucl_pp))) + 1
    xpad  <- xmax + 0.5
    x_lo  <- -xmax
    x_hi  <- xpad + xmax * 1.6
    
    y_arrow <- 0.55
    y_label <- 0.20
    
    # --- Arrow geometry: explicit, equal-length, near each panel edge ---
    arrow_len <- xmax * 0.45          # length of each arrow (half of before)
    edge_pad  <- xmax * 0.10          # gap between arrow tip and panel edge
    
    left_out  <- x_lo + edge_pad                 # left arrow tip (points left, to edge)
    left_in   <- left_out + arrow_len            # left arrow tail (toward center)
    right_out <- x_hi - edge_pad                 # right arrow tip (points right, to edge)
    right_in  <- right_out - arrow_len           # right arrow tail (toward center)
    
    ggplot(d, aes(x = rd_pp, y = unit)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      geom_errorbarh(aes(xmin = lcl_pp, xmax = ucl_pp), height = 0.15, linewidth = 0.7) +
      geom_point(aes(size = n_hosp), shape = 15, color = "#2c3e50") +
      geom_text(aes(x = xpad, label = lab), hjust = 0, size = 3.4) +
      
      # Equal-length arrows, one near each edge
      annotate("segment", x = left_in, xend = left_out, y = y_arrow, yend = y_arrow,
               arrow = arrow(length = unit(0.18, "cm")), color = "grey40") +
      annotate("segment", x = right_in, xend = right_out, y = y_arrow, yend = y_arrow,
               arrow = arrow(length = unit(0.18, "cm")), color = "grey40") +
      annotate("text", x = (left_in + left_out) / 2, y = y_label,
               label = "Favors IMC-capable", size = 3.1, color = "grey30") +
      annotate("text", x = (right_in + right_out) / 2, y = y_label,
               label = "Favors non-capable", size = 3.1, color = "grey30") +
      
      scale_size_continuous(range = c(3, 6), guide = "none") +
      scale_x_continuous(limits = c(x_lo, x_hi), breaks = scales::pretty_breaks(5)) +
      scale_y_discrete(expand = expansion(add = c(1.1, 0.6))) +
      labs(
        title = outcome_title,
        x = "Risk difference (percentage points)",
        y = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title         = element_text(face = "bold", hjust = 0.5),
        panel.grid.major.y = element_blank(),
        axis.text.y        = element_text(face = "bold"),
        plot.background    = element_rect(fill = "white", color = NA),
        panel.background   = element_rect(fill = "white", color = NA)
      )
  }
  
  p_death <- make_forest(results, "death_hospice",    "Death or Discharge to Hospice")
  p_organ <- make_forest(results, "organ_failure_yn", "Progressive Organ Failure")
  
  ggsave(paste0(output_dir, "forest_death_hospice.png"), p_death, width = 9, height = 3.4, dpi = 300)
  ggsave(paste0(output_dir, "forest_organ_failure.png"), p_organ, width = 9, height = 3.4, dpi = 300)
  
  p_death
  p_organ
  
} # plot forest plots

##### Plot caterpillar plots #####
for(outcome_i in outcomes_binary){
  for(unit_i in c("icu", "ward")){
    
    plot_data <- outcome_comparisons_raw |>
      filter(outcome == outcome_i, unit == unit_i) |>
      mutate(
        # Orders hospitals from lowest to highest estimate
        clif_hospital = fct_reorder(clif_hospital, estimate),
        
        imc_capable = factor(
          imc_capable,
          levels = c(0, 1),
          labels = c("Not IMC-capable", "IMC-capable")
        ),
        
        academic_community = factor(
          academic_community,
          levels = c("academic", "community"),
          labels = c("Academic", "Community")
        )
      )
    
    # Set upper y-axis limit to a nice rounded value above the max upper CI
    upper_lim <- ceiling(max(plot_data$upper_prob, na.rm = TRUE) / 0.05) * 0.05
    
    p <- ggplot(
      plot_data,
      aes(
        x = clif_hospital,
        y = estimate,
        color = imc_capable
      )
    ) +
      geom_errorbar(
        aes(
          ymin = lower_prob,
          ymax = upper_prob
        ),
        width = 0,
        linewidth = 0.6
      ) +
      geom_point(
        aes(shape = academic_community),
        size = 2.8
      ) +
      scale_y_continuous(
        labels = label_percent(accuracy = 1),
        limits = c(0, upper_lim),
        expand = expansion(mult = c(0, 0.02))
      ) +
      scale_shape_manual(
        values = c(
          "Academic" = 16,
          "Community" = 17
        )
      ) +
      labs(
        x = paste0("Hospital (", ifelse(unit_i=="icu", "ICU", "ward"),")"),
        y = paste0("Estimated probability of ", 
                   ifelse(outcome_i=="death_hospice", "death or hospice",
                          "progressive organ failure")),
        color = "IMC capability",
        shape = "Hospital type"
      ) +
      theme_classic() +
      theme(
        axis.text.x = element_blank(),   # keeps hospitals unlabelled
        axis.ticks.x = element_blank(),
        legend.position = "right"
      )
    
    ggsave(paste0(output_dir, paste0("caterpillar_", outcome_i, "_", unit_i, ".jpg")), 
           plot=p, width=8, height=5, dpi=600)
    
  }
}