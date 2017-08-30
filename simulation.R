mean(means) # mean of samples of size 40
1/lambda    # theoretical mean of samples of size 40
sd(means)   # sd of samples of size 40
1/lambda/sqrt(40) # theoretical SE of samples of size 40

lambda <- .2
n <- 40
nsims <- 1000



#================
### SECTION 1 ###
#================
# Comparing a simulated distribution of samples of 40 random exponentials
# with its theoretical distribution.



set.seed(333)

# exp.sim() is a function that generates means of samples of
# 40 random exponentials
exp.sim <- function(nsims, n, lambda){
        means <- numeric()
        for(i in 1:nsims){
                means[i] <- mean(rexp(n, lambda))
        }
        means
}


means <- exp.sim(nsims, n, lambda) # create distribution of sample means
sim.mean <- mean(means) # calculate mean of sample means
sim.sd <- sd(means) # sd of sample means
sim.range <- range(means) # range of sample means

# Create vector of normal density values using sim.mean and sim.sd
# to plot on top of histogram
sequence <- seq(sim.range[1], sim.range[2], length = 1000)
norm.curve <- dnorm(sequence, sim.mean, sim.sd)

# (ggplot annotation information)
xblack <- c(sim.mean + .05, sim.mean + .5, sim.mean + .55)
xred <- c(1/lambda - .05, 1/lambda - .5, 1/lambda - .55)
yblack <- yred <- max(norm.curve) + .05


# Plot a histogram of the sample means with the normal density curve overlayed on
# top. Plot vertical lines showing the observed mean of the simulated samples and
# the theoretical mean of samples of size 40.
library(ggplot2)
ggplot() + geom_histogram(aes(means, ..density..), 
                          color = "black", 
                          fill = "black", 
                          alpha = .3,
                          boundary = 0) +
        geom_vline(xintercept = 1/lambda, size = 2, color = "tomato", linetype = "dashed") +
        geom_vline(xintercept = sim.mean, color = "black", size = 2) +
        labs(x = "Sample average",
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

# Print the observed and theoretical means of the distribution (they're really close!)
compare.means <- c("Observed mean" = sim.mean, "Theoretical mean" =  1/lambda)
compare.means
# Print the observed and theoretical variances of the distribution (they're also really close!)
compare.var <- c("Observed variance" = sim.sd^2, "Theoretical variance" = 1/lambda^2/n)
compare.var

# Draw the theoretical distribution
theory.curve <- dnorm(sequence, 1/lambda, 1/lambda/sqrt(40))

# annotate() information for plot
xblack2 <- c(3.75, 4.4)
yblack2 <- c(.475, .45)
xred2 <- c(6.1, 5.5)
yred2 <- c(.475, .45)

# Plot the theoretical and observed distributions with vertical lines
# at the means
ggplot() + geom_area(aes(sequence, theory.curve), color = "black", fill = "tomato", alpha = .3) +
        geom_density(aes(means), fill = "black", alpha = .3) +
        labs(x = "Sample average",
             y = "Density") +
        annotate("segment", x = xblack2[1], xend = xblack2[2], y = yblack2[1], yend = yblack2[2],
                 arrow = arrow(), size = 1.5) +
        annotate("text", label = "Observed \ndistribution", x = xblack2[1] - .1, y = yblack2[1],
                 hjust = 1, size = 5) +
        annotate("segment", x = xred2[1], xend = xred2[2], y = yred2[1], yend = yred2[2],
                 arrow = arrow(), size = 1.5, color = "tomato") +
        annotate("text", label = "Theoretical \ndistribution", x = xred2[1] + .1, y = yblack2[1],
                 hjust = 0, size = 5, color = "tomato") +
        theme_bw() +
        theme(axis.title = element_text(size = 15),
              axis.text = element_text(size = 12))




#================
### SECTION 2 ###
#================
# Show the difference between the distribution of values in one large sample
# vs. the distribution of sample averages.


# Plot the distribution of 1000 random exponentials and compare it
# to the distribution of 1000 means of 40 random exponentials.
library(ggplot2)
library(gridExtra)
set.seed(222)
sample <- rexp(nsims, lambda)
density.x <- seq(0, range(sample)[2], length = 1000)
density.y <- dexp(density.x, .2)
plot1 <- ggplot() + geom_histogram(aes(sample, ..density..),
                          bins = 25,
                          color = "black",
                          fill = "green",
                          alpha = .5,
                          boundary = 0) +
        geom_line(aes(density.x, density.y), color = "darkgreen", size = 1) +
        xlim(0, 20) +
        ylim(0, .5) +
        annotate("text", label = "Exponential distribution",
                 x = 10, y = .3, size = 5, color = "darkgreen") +
        labs(x = "Sample value (n = 1)", y = "Density") +
        theme_bw()
plot2 <- ggplot() + geom_histogram(aes(means, ..density..), 
                                   color = "black", fill = "black", alpha = .5, boundary = 0) +
        geom_line(aes(sequence, norm.curve), size = 1) +
        xlim(0, 20) +
        labs(x = "Sample average (n = 40)", y = "Density") +
        annotate("text", label = "Normal distribution!",
                 x = 10, y = .3, size = 5) +
        theme_bw()

grid.arrange(plot1, plot2, nrow = 2)





#================
### SECTION 3 ###
#================
# Simple exploratory analysis











