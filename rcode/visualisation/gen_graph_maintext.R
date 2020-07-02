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

# Process raw results
performancexw <- process_results_heterogeneity(method = "ML",
                                               derivation_predictor = "X",
                                               validation_predictor = "W",
                                               data = data)

performancewx <- process_results_heterogeneity(method = "ML",
                                               derivation_predictor = "W",
                                               validation_predictor = "X",
                                               data = data)

# Pre-process data
datacalitl <- data.frame(mean = c(performancexw$point_estimates$cal_large,
                                  performancewx$point_estimates$cal_large), 
                lower = c(performancexw$confidence_intervals_lower$cal_large,
                          performancewx$confidence_intervals_lower$cal_large),
                upper = c(performancexw$confidence_intervals_upper$cal_large,
                          performancewx$confidence_intervals_upper$cal_large))
datacalitl$scenario <- c(1,1)
datacalitl$group <- c("a","b")

# Generate plot
p1 <- ggplot(data=datacalitl,
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
p1
