library(MCMCglmm)
library(ggplot2)
library(dplyr)
library(coda)

plot_estimates <- function(model, model_name = "") {
  # extract fixed effects summary
  fixed_summary <- summary(model)$solutions
  
  # make df
  fixed_df <- data.frame(
    term = rownames(fixed_summary),
    mean = fixed_summary[, "post.mean"],
    lower = fixed_summary[, "l-95% CI"],
    upper = fixed_summary[, "u-95% CI"]
  )
  
  # effective sample size
  ess <- effectiveSize(model$Sol)
  fixed_df$ess <- ess[fixed_df$term]
  
  # append ESS to label
  fixed_df$term_label <- paste0(fixed_df$term, " (ESS: ", round(fixed_df$ess), ")")
  
  # order by estimate
  fixed_df$term_label <- factor(
    fixed_df$term_label,
    levels = fixed_df$term_label[order(fixed_df$mean)]
  )
  
  # significance: CI does not cross 0
  fixed_df$significant <- with(fixed_df, lower * upper > 0)
  
  # plot
  ggplot(fixed_df, aes(x = mean, y = term_label, color = significant)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
    geom_point(size = 3) +
    scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "darkolivegreen4")) +
    labs(
      x = "Posterior Mean Estimate",
      y = NULL,
      title = model_name,
      color = "Significant"
    ) +
    theme_minimal(base_size = 14)
}
plot_estimates(leaf_C_per_dry_mass_chain1, "")

#------ variance partioning stacked bar plot


var_plot <- function(model, model_name = "") {
  # Assuming get_info extracts variance proportions per iteration in $prop
  results <- get_info(model)   # You need to have get_info() defined
  
  #calculate mean variations from results
  mean_props <- colMeans(results$prop)
  
  #prep for plotting
  df <- data.frame(
    Component = names(mean_props),
    Proportion = as.numeric(mean_props)
  )
  
  #plot
  ggplot(df, aes(x = "", y = Proportion, fill = Component)) +
    geom_bar(stat = "identity", width = 0.6) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = model_name,
      x = NULL,
      y = "Proportion of Total Variance"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  }

var_plot(ln_NP_ratio_chain3, "N:P")
