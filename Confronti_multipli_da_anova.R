library(tidyverse)
Confronti_multipli_da_anova = function(anova_model, alpha = 0.05, bonferroni = FALSE)
{
  groups = factor(sort (unique(anova_model$xlevels)[[1]]))
  g = length(groups)
  confronti = g*(g-1) / 2
  observed_data = anova_model$model[[1]]
  observed_groups = anova_model$model[[2]]
  n = length(observed_groups)
  if(bonferroni){
    alpha_corr = alpha/confronti
  } else {
    alpha_corr = alpha
  }
  intervals = NULL
  names = NULL
  S = sum(anova_model$residuals ^ 2) / anova_model$df.residual
  
  for ( k in 1:(g-1))
  {
    group1 = groups[k]
    for(j in (k+1):g)
    {
      group2 = groups[j]
      temp = c(0,0,0,0)
      m1 = mean(observed_data[observed_groups == group1])
      m2 = mean(observed_data[observed_groups == group2])
      n1 = length(observed_data[observed_groups == group1])
      n2 = length(observed_data[observed_groups == group2])
      width = sqrt(S * (1/n1 + 1/n2)) * qt(1-alpha_corr/2, df = n - g)
      temp[2] = m1 - m2
      temp[1] = temp[2] - width
      temp[3] = temp[2] + width
      temp[4] = 2*(1-pt(abs(temp[2] )/sqrt(S * (1/n1 + 1/n2)),df = n - g  ))
      names = c(names, paste0(group1,"-",group2))
      intervals = rbind(intervals,temp)
    }
  }
  row.names(intervals) = names
  colnames(intervals) = c("lower","mean","upper", "pval")
  if(bonferroni){
    intervals[,4] = p.adjust(intervals[,4], method = "bonferroni") # Se si vogliono i pvalues corretti
  }
  intervals_to_plot = data.frame(intervals)
  intervals_to_plot$comparison = as.factor(names)
  plot = ggplot(intervals_to_plot, aes(x = comparison, y = mean))+
    geom_point(size = 2)+
    geom_errorbar(aes(x = comparison, ymin = lower, ymax = upper), linewidth = 0.5)+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.title.x = element_blank())
  plot
  return(list(intervals = intervals, plot = plot))
}