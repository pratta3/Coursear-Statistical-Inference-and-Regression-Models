


### Motor Trend data analysis ###
#================================



# Load the data and take a quick peek at it
data(mtcars)
head(mtcars)
str(mtcars)
sum(!complete.cases(mtcars))



# Load up libraries
library(ggplot2)
library(dplyr)
library(tidyr)


# Make am and vs factor variables instead of numeric
mtcars <- mtcars %>% mutate(am = factor(am, levels = c("0", "1"), labels = c("automatic", "manual")),
                            vs = factor(vs, c("0", "1"), c("V", "S")))

# ?mtcars provides some more information about the dataset.



#=======================================================
# Final Take

# Make some custom palettes just because I feel like seeing different colors.
color.palette <- scale_color_brewer(palette = "Dark2", guide = FALSE)
fill.palette <- scale_fill_brewer(palette = "Dark2", guide = FALSE)

# Make some theme elements to make the plots look nice and
# easy to read:

# plot.theme creates a simple that's easy to look at
plot.theme <- theme_bw() +
        theme(axis.title = element_text(size = 13),
              axis.text = element_text(size = 10, color = "black"),
              strip.text = element_text(size = 10))

# legend.top.right puts the legend in the top right corner of the plot
legend.top.right <- theme(legend.justification = c(1,1),
                          legend.position = c(1,1),
                          legend.background = element_rect(fill = "transparent"))

# First look at mpg vs. am
ggplot(mtcars, aes(am, mpg)) + 
        geom_boxplot(aes(fill = am), alpha = .8) +
        geom_jitter(aes(fill = am), shape = 21, size = 3, height = 0, width = .3, alpha = .7) +
        fill.palette +
        labs(x = "") +
        plot.theme


# So the problem with this dataset is that
# 1) it's small
# 2) the characteristics of manual vs. automatic
#    automatic transmissions are not the same across
#    the board! This means that you can't just compare
#    the gas mileage between the two types of
#    transmission without accounting for other
#    variables.
#
# Thus, I should set out to find out which variables might
# be affecting gas mileage that are clearly different
# between manual transmission cars and automatic
# transmission cars.
# 
# Which variables would you expect might possibly
# affect gas mileage?
#
# Number of cylinders: I would expect that the more
# cylinders a car has, A) the heaver it becomes
# (though maybe not heavier enough to make a
#  difference), and B) the less fuel efficient it
# becomes.
ggplot(mtcars, aes(cyl, mpg)) + 
        geom_jitter(aes(color = am), size = 3, alpha = .7, height = 0, width = .3) +
        labs(x = "Num. of cylinders") +
        scale_color_brewer(palette = "Dark2", name = "Transmission") +
        plot.theme

# It definitely looks like as you put more cylinders in
# the engine, fuel efficiency declines. Furthermore,
# there are more automatic transmission cars that have
# 8 cylinders and more manual transmission cars that have
# 4 cylinders. This could definitely affect our assessment
# of which type of transmission gets better gas mileage!

# Engine displacement. If my impression of cars serves me
# correctly, I would expect that cars with bigger engines
# get less fuel economy.
ggplot(mtcars, aes(disp, mpg)) +
        geom_point(aes(color = am), size = 3, alpha = .7) +
        scale_color_brewer(palette = "Dark2", name = "Transmission") +
        plot.theme

# And that's exactly what the data shows. It also shows that
# the displacement volumes of manual transmission cars in the
# dataset are smaller than those of automatic transmission cars.
# This could affect conclusions made about the difference in 
# fuel economy between manual transmission and automatic 
# transmission cars! Interestingly, it also looks like there
# is some curvature in the relationship between engine displacement
# and miles per gallon. ALSO not that the number of cylinders
# and the engine displacement are kind of measuring the
# same thing:
ggplot(mtcars, aes(cyl, disp)) + 
        geom_jitter(aes(color = am), size = 5, alpha = .7, height = 0, width = .2) +
        scale_color_brewer(palette = "Dark2", name = "Transmission") +
        plot.theme

# Next, horsepower. My guess is that more horsepower probably
# corresponds to less fuel efficiency. My experience tells me
# that muscle cars and sports cars with a lot of horse powers
# get fewer miles to the gallon.
ggplot(mtcars, aes(hp, mpg)) +
        geom_point(aes(color = am), size = 5, alpha = .7) +
        scale_color_brewer(palette = "Dark2", name = "Transmission") +
        plot.theme

# Clearly, more horsepower does correspond to less fuel
# efficiency. But its less clear whether or not manual
# transmission cars and automatic transmission cars in
# this data set differ in their overall horsepowers.
ggplot(mtcars, aes(am, hp)) +
        geom_jitter(aes(color = am), size = 5, alpha = .7, height = 0, width = .2) +
        scale_color_brewer(palette = "Dark2", name = "Transmission") +
        plot.theme

# This makes it look like there is maybe some difference
# between the horsepowers of the automatic vs. the
# manual transmission cars in the dataset. Thus,
# it may also be influencing the fuel efficiency
# of the two types of transmissions.

# Next: rear axle ratio. I did a little bit of reading
# and found that often times axle ratio is talked about
# in the context of pickup trucks. It is the ratio of the
# number of gear teeth on the rear axle gear to the number
# of teeth on the pinion gear of the driveshaft, which is
# turned by the transmission. When the pinion gear is
# turned by the transmission, the axle gear is subsequently
# turned by the pinion gear, causing the rear wheels to turn.
# So, if a car has a ratio of 4.11, for example, then it would
# take 4.11 revolutions of the pinion gear to turn the axle
# gear once. For trucks, the significance is this: the higher
# this ratio is, the more pulling power the truck has but the
# lower the top-speed of the car becomes. And the inverse is
# also true. What I expect to see in this dataset is that
# cars with higher ratios get fewer miles per gallon because
# it takes more energy to turn the rear axle gear the rear
# axle gear one revolution.
ggplot(mtcars, aes(drat, mpg)) +
        geom_point(aes(color = am), size = 3, alpha = .7) +
        scale_color_brewer(palette = "Dark2", name = "Transmission") +
        plot.theme
        

# Aha! The trend is actually the opposite. Why is this? To be
# honest, I'm not sure why this is. Everything I have found
# about axle ratios says that higher ratios should mean less
# fuel efficiency. What may be going on here is that there is
# a separate confounding variable. At any rate, it's also clear
# from this plot that the rear axle ratios of automatic vs.
# manual transmission cars are not the same! This could again
# potentially affect any conclusions made about the fuel efficiency
# of the two types of transmissions.

# Weight! I'm expecting to see that heavier cars get fewer
# miles per gallon than lighter cars.
ggplot(mtcars, aes(wt, mpg)) +
        geom_point(aes(color = am), size = 3, alpha = .7) +
        labs(x = "Weight (1000 lbs.)") +
        scale_color_brewer(palette = "Dark2", name = "Transmission") +
        plot.theme

# And that's exactly what the data shows. What it also shows is
# that the manual transmission cars in the dataset weight
# less than the automatic transmission cars in the dataset.

# V engines vs. straight engines. Straight engines, or inline
# engines, contain all the cylinders in one line. Because of
# this, engines with more than 6 cylinders can't be made to
# fit in a car's engine compartment. V engines, on the other
# hand, have their cylinders oriented in two rows. This means
# that they are wider but not as long as straight engines and
# they can therefore be used to fit more than 6 cylinders into
# the engine compartment. I'm not sure what to expect here, but
# what I did was plot the mpg of the two engine types faceted
# by the number of cylinders and colored the points according
# the type of transmission.
ggplot(mtcars, aes(vs, mpg)) + 
        geom_jitter(aes(color = am), size = 5, alpha = .7, height = 0, width = .3) +
        facet_wrap(~ cyl, nrow = 1) +
        scale_color_brewer(palette = "Dark2", name = "Transmission") +
        plot.theme

# What it seems to show is that A) the only case where there's
# anything to compare is in 6 cylinder engines, B) it's a little
# hard to compare in this case anyway because all the V engines
# are manual transmission and all the straight engines are
# automatic transmission, and C) if you are going to compare the
# two engine types in 6 cylinder engines, they don't appear to get
# different gas mileage.

# Number of forward gears? I'm not sure what to expect here. It
# could go either way or there could be nothing at all.
ggplot(mtcars, aes(gear, mpg)) +
        geom_jitter(aes(color = am), size = 5, alpha = .7, height = 0, width = .2) +
        scale_color_brewer(palette = "Dark2", name = "Transmission") +
        plot.theme

# Interesting. It looks like cars with only 3 forward gears are
# less fuel efficient than cars with 4 or 5 forward gears. Furthermore,
# All cars with only 3 forward gears have automatic transmission.

# Number of carburetors. Also not sure what to expect here.
ggplot(mtcars, aes(carb, mpg)) + 
        geom_jitter(aes(color = am), size = 5, alpha = .7, height = 0, width = .2) +
        scale_color_brewer(palette = "Dark2", name = "Transmission") +
        plot.theme

# It looks like fuel efficiency declines as the number of carburetors
# increases. It also looks like for each number of carburetors,
# automatic transmission cars get fewer miles per gallon than manual
# transmission cars.



# Model selection. 

# Start with a model containing all the variable you selected.
# For each model, select the largest p-value that doesn't
# correspond to "ammanual" and remove it from the data to create
# the next model. Keep doing this until you have only "Intercept"
# and "ammanual" remaining.
model.data <- list()
models <- list()
model.summaries <- list()
model.data[[1]] <- mtcars %>% select(mpg, cyl, disp, hp, drat, wt, am, gear, carb)
models[[1]] <- lm(mpg ~ ., model.data[[1]])
model.summaries[[1]] <- summary(models[[1]])
for(i in 1:(length(model.data[[1]])-2)){
        parms <- coef(model.summaries[[i]])
        amindex <- which(rownames(parms) == "ammanual")
        parms <- parms[-amindex,]
        var <- names(which.max(parms[,4]))
        varindex <- which(names(model.data[[i]]) == var)
        model.data[[i+1]] <- model.data[[i]][,-varindex]
        models[[i+1]] <- lm(mpg ~ ., model.data[[i+1]])
        model.summaries[[i+1]] <- summary(models[[i+1]])
}

anova(models[[8]], models[[7]], models[[6]],
      models[[5]], models[[4]], models[[3]],
      models[[2]], models[[1]])


final.model <- models[[6]]
summary(final.model)




# Visualize it

# Create new data containing predicted values from final.model 
# to mtcars dataset that extend slightly beyond the range of the
# data so the lines don't look stupid.
pred <- mtcars %>% group_by(am, cyl) %>% 
        mutate(lower = range(wt)[1]-1,
                  upper = range(wt)[2]+1) %>% 
        select(cyl, am, lower, upper) %>% 
        gather("bound", "wt", lower:upper) %>% 
        select(-bound) %>% 
        ungroup
pred$pred <- predict(final.model, newdata = pred)

# Plot mpg vs. weight, colored by number of cylinders
# Lines are predicted values from final.model
ggplot(mtcars, aes(wt, mpg)) +
        geom_line(data = pred, aes(wt, pred, color = factor(cyl)), size = 1.5, alpha = .7) +
        geom_point(aes(color = factor(cyl)), size = 5, alpha = .7) +
        scale_color_brewer(palette = "Dark2", name = "Num. of cylinders") +
        facet_wrap(~ am) +
        labs(x = "Weight (1000 lbs.)",
             y = "mpg") +
        plot.theme +
        legend.top.right


# Hmmm I don't know about that. Because I forced the lines to
# be parallel, they don't fit the data that well.
# Look at the residuals:
mtcars$resid <- resid(final.model)
mtcars <- mtcars %>% arrange(cyl) %>% mutate(index = 1:nrow(mtcars))
ggplot(mtcars, aes(index, resid)) +
        geom_hline(yintercept = 0, size = 2, alpha = .5) +
        geom_point(size = 5, aes(color = factor(cyl), shape = am)) +
        color.palette +
        scale_shape_discrete(guide = FALSE) +
        plot.theme

# See how the residuals don't all look balanced?




#=====================================================

# This time I'll fit a model that includes an interaction
# term between number of cylinders and car weight. The
# residuals look much better.
final.model2 <- lm(mpg ~ am + cyl*wt, mtcars)
summary(final.model2)
mtcars2 <- mtcars %>% mutate(resid = resid(final.model2)) %>% 
        arrange(cyl) %>% 
        mutate(index = 1:nrow(mtcars))

# Plot the residuals
ggplot(mtcars2, aes(index, resid)) +
        geom_hline(yintercept = 0, size = 2, alpha = .5) +
        geom_point(aes(color = factor(cyl), shape = am), size = 5) +
        color.palette +
        scale_shape_discrete(guide = FALSE) +
        plot.theme

# Visualized again
pred2 <- mtcars %>% group_by(am, cyl) %>% 
        mutate(lower = range(wt)[1]-1,
               upper = range(wt)[2]+1) %>% 
        select(cyl, am, lower, upper) %>% 
        gather("bound", "wt", lower:upper) %>% 
        select(-bound) %>% 
        ungroup
pred2$pred <- predict(final.model2, newdata = pred2)

ggplot(mtcars2, aes(wt, mpg)) +
        geom_line(data = pred2, aes(wt, pred, color = factor(cyl)), size = 1.5, alpha = .7) +
        geom_point(aes(color = factor(cyl)), size = 5, alpha = .7) +
        scale_color_brewer(palette = "Dark2", name = "Num. of cylinders") +
        facet_wrap(~ am) +
        labs(x = "Weight (1000 lbs.)",
             y = "mpg") +
        plot.theme +
        legend.top.right




#=============================================


# Of course, according to this, the type of transmission
# doesn't really make a difference. So an even better
# model should include only weight, number of cylinder,
# and their interaction as covariates.
final.model3 <- lm(mpg ~ wt*cyl, mtcars)
summary(final.model3)

mtcars3 <- mtcars %>% mutate(resid = resid(final.model3)) %>% 
        arrange(cyl) %>% 
        mutate(index = 1:nrow(mtcars))

# Plot the residuals
ggplot(mtcars3, aes(index, resid)) +
        geom_hline(yintercept = 0, size = 2, alpha = .5) +
        geom_point(aes(color = factor(cyl)), size = 5) +
        color.palette +
        scale_shape_discrete(guide = FALSE) +
        plot.theme

# Visualized again
pred3 <- mtcars %>% group_by(am, cyl) %>% 
        mutate(lower = range(wt)[1]-1,
               upper = range(wt)[2]+1) %>% 
        select(cyl, am, lower, upper) %>% 
        gather("bound", "wt", lower:upper) %>% 
        select(-bound) %>% 
        ungroup
pred3$pred <- predict(final.model3, newdata = pred3)

ggplot(mtcars3, aes(wt, mpg)) +
        geom_line(data = pred3, aes(wt, pred, color = factor(cyl)), size = 1.5, alpha = .7) +
        geom_point(aes(color = factor(cyl)), size = 5, alpha = .7) +
        scale_color_brewer(palette = "Dark2", name = "Num. of cylinders") +
        labs(x = "Weight (1000 lbs.)",
             y = "mpg") +
        plot.theme +
        legend.top.right



