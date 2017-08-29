
set.seed(333)
lambda <- .2
n <- 40
nsims <- 1000

exp.sim <- function(nsims, n, lambda){
        means <- numeric()
        for(i in 1:nsims){
                means[i] <- mean(rexp(n, lambda))
        }
        means
}

means <- exp.sim(nsims, n, lambda)
sim.mean <- mean(means)
sim.sd <- sd(means)
sim.range <- range(means)
sequence <- seq(sim.range[1], sim.range[2], length = 1000)
norm.curve <- dnorm(sequence, sim.mean, sim.sd)

xblack <- c(sim.mean + .05, sim.mean + .5, sim.mean + .55)
xred <- c(1/lambda - .05, 1/lambda - .5, 1/lambda - .55)
yblack <- yred <- max(norm.curve) + .05


library(ggplot2)
ggplot() + geom_histogram(aes(means, ..density..), color = "black", fill = "black", alpha = .3) +
        geom_line(aes(sequence, norm.curve), size = 1.5, color = "black") +
        geom_vline(xintercept = 1/lambda, size = 2, color = "tomato", linetype = "dashed") +
        geom_vline(xintercept = sim.mean, color = "black", size = 2) +
        labs(x = "Averages",
             y = "Density",
             title = "Distribution of averages of 40 random exponential variables") +
        annotate("segment",
                 x = xblack[2],
                 xend = xblack[1],
                 y = yblack,
                 yend = yblack,
                 color = "black",
                 arrow = arrow(),
                 size = 2) +
        annotate("text",
                 x = xblack[3], 
                 y = yblack, 
                 label = "Observed mean", 
                 color = "black",
                 hjust = 0,
                 size = 6) +
        annotate("segment",
                 x = xred[2],
                 xend = xred[1],
                 y = yred,
                 yend = yred,
                 arrow = arrow(),
                 color = "tomato",
                 size = 2) +
        annotate("text",
                 x = xred[3],
                 y = yred, 
                 label = "Theoretical mean",
                 color = "tomato",
                 hjust = 1,
                 size = 6) +
        theme_bw() +
        theme(plot.title = element_text(size = 18, face = "bold"),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 15))

mean(means) # mean of samples of size 40
1/lambda    # theoretical mean of samples of size 40
sd(means)   # sd of samples of size 40
1/lambda/sqrt(40) # theoretical SE of samples of size 40