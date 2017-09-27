library(ggplot2)
library(gridExtra)

lambda = 0.2;

theoretical_mean <- 1/lambda
standard_error <- (1/lambda)/sqrt(40)


mns = NULL
for (i in 1 : 1000){
mns = c(mns, mean(rexp(40, lambda)))
}

average_means <- mean(mns)
sd_means <- sd(mns)
means_frame <- data.frame(mns)


exp_values <- rexp(1000,lambda)
exp_frame <- data.frame(exp_values)

exp_plot <- ggplot(exp_frame, aes(x=exp_values)) +
geom_histogram(alpha = .80, binwidth=1, boundary=1, colour = "black", 
fill = "limegreen",aes(y = ..density..))+
stat_function(fun = dexp, args = list(rate=lambda), size=2)+
geom_vline(xintercept = theoretical_mean, linetype = "solid",color="blue",
size=2,alpha = 0.8)+
labs(title="Sample Exponential Density Plot",x = "x",y="P(x)")+
theme(text = element_text(size=16))
  
  
  
  


sample_means_plot <- ggplot(means_frame, aes(x=mns)) +
geom_histogram(alpha = .80, bins = 15, colour = "black", 
fill = "orange",aes(y = ..density..))+
stat_function(fun = dnorm, args = list(mean = theoretical_mean, sd = standard_error)
, size=2)+
geom_vline(xintercept = average_means, linetype = "solid",color="steelblue",size=2,alpha = 0.7)+
geom_vline(xintercept = c(average_means-1.96*sd_means, average_means+1.96*sd_means),
linetype = "dashed",color="red",size=2,alpha=0.6)+
geom_vline(xintercept = c(theoretical_mean-1.96*standard_error, 
theoretical_mean+1.96*standard_error),
linetype = "solid",color="green",size=2,alpha=0.6)+
labs(title="Sample Mean Density Plot",x = "Sample Mean",y="Probability")+
theme(text = element_text(size=16))

print(sample_means_plot)
plot_CLT <- grid.arrange(exp_plot, sample_means_plot, ncol = 2)

