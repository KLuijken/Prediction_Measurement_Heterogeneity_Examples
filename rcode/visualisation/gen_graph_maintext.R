#------------------------------------------------------------------------------#
# Predictor measurement heterogeneity empirical
# K Luijken
#
# Create plot main text
# In this script, an example for the calibration-in-the-large coefficient is 
# presented. Similar plots can be made for the c-statistic and Brier score
#------------------------------------------------------------------------------#

# Source code ----
#------------------------------------------------------------------------------#
source("./rcode/visualisation/helper/process_analysis_results.R")

# Calibration-in-the-large ----
#------------------------------------------------------------------------------#

perf_plot_calLarge <- function(){
  # Process raw results
  a <- process_results_heterogeneity(method = "ML",
                                      derivation_predictor = "X",
                                      validation_predictor = "W",
                                      data = data)
  
  b <- process_results_heterogeneity(method = "ML",
                                      derivation_predictor = "W",
                                      validation_predictor = "X",
                                      data = data)
  
  # Pre-process data
  plot_data <- data.frame(mean = c(a$point_estimates$cal_large,
                                    b$point_estimates$cal_large), 
                           lower = c(a$confidence_intervals_lower$cal_large,
                                     b$confidence_intervals_lower$cal_large),
                           upper = c(a$confidence_intervals_upper$cal_large,
                                     b$confidence_intervals_upper$cal_large))
  plot_data$scenario <- rep(1:nrow(a$point_estimates), each = 2)
  plot_data$group <- rep(c("a","b"), times = nrow(a$point_estimates))
  
  # Generate plot
  plot <- ggplot(data=plot_data,
               aes(x = group,y = mean, ymin = lower, ymax = upper ))+
    geom_pointrange(aes(shape=group))+  scale_shape_manual(values=c(8,16))+
    geom_hline(yintercept =0, linetype=2)+
    xlab('Scenario')+ ylab("Calibration-in-the-large coefficient")+
    geom_errorbar(aes(ymin=lower, ymax=upper),width=0.3,cex=0.5)+ 
    facet_wrap(~scenario,strip.position="left",nrow=10,scales = "free_y") +
    theme(plot.title=element_text(size=16,face="bold"),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_text(face="bold"),
          axis.title=element_text(size=12,face="bold"),
          strip.text.y = element_text(hjust=0,vjust = 0.5,angle=180,face="bold"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill="grey90"),
          legend.position="none")+
    #labs(shape="Scenario version")+
    coord_flip()
  
  print(plot)
}


