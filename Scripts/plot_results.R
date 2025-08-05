library(MCMCglmm)
library(ggplot2)
library(dplyr)

plot_estimates <- function(model, model_name = "") {
  #extract fixed effects summary
  fixed_summary <- summary(model)$solutions
  
  #make df
  fixed_df <- data.frame(
    term = rownames(fixed_summary),
    mean = fixed_summary[, "post.mean"],
    lower = fixed_summary[, "l-95% CI"],
    upper = fixed_summary[, "u-95% CI"]
  )
  
  #effective sample size
  ess <- effectiveSize(model$Sol)
  fixed_df$ess <- ess[fixed_df$term]
  
  #append
  fixed_df$term_label <- paste0(fixed_df$term, " (ESS: ", round(fixed_df$ess), ")")
  
  #order
  fixed_df$term_label <- factor(fixed_df$term_label, levels = fixed_df$term_label[order(fixed_df$mean)])
  
  #set significance
  fixed_df$significant <- with(fixed_df, lower * upper > 0)
  
  #plot
  ggplot(fixed_df, aes(x = term_label, y = mean, fill = significant)) +
    geom_col(width = 0.6) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "grey60") +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_fill_manual(values = c("grey80", "darkolivegreen4")) +
    labs(
      x = NULL, y = "Posterior Mean Estimate",
      title = model_name,
      fill = "Significant"
    ) +
    theme_minimal(base_size = 14)
}


plot_estimates(CP1, "CP")

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
