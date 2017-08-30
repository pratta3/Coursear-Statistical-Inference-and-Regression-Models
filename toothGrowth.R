

#================
### SECTION 3 ###
#================
# Simple exploratory data analysis of the ToothGrowth dataset.

# len is the length of odontoblasts in micrometers

library(datasets)
data("ToothGrowth")
# help(ToothGrowth)

ToothGrowth$dose <- factor(ToothGrowth$dose)

theme <- theme(axis.text = element_text(size = 12),
               axis.title = element_text(size = 15),
               plot.title = element_text(size = 18, face = "bold"),
               legend.title = element_text(size = 12),
               legend.text = element_text(size = 12),
               legend.position = c(1,0),
               legend.justification = c(1,0))

library(RColorBrewer)
library(ggplot2)
ggplot(ToothGrowth, aes(dose, len)) +
        geom_boxplot(aes(fill = supp)) +
        scale_fill_brewer(palette = "Paired",
                          name = "Vitamin C source",
                          labels = c("orange juice", "ascorbic acid")) +
        labs(x = "Dosage (mg//day)",
             y = expression("Odontoblast length ("*mu*"m)"),
             title = "Odontoblast growth in response to two sources of vitamin C") +
        theme_bw() +
        theme

# Although there are more appropriate ways to analyze the data
# statistically, I will only look at the difference between the
# two supplement types at each separate dosage level.

library(dplyr)
doses <- c(.5, 1, 2)
crit.value <- rep(.05, 3)*1:3/3
p.value <- rep(NA, 3)
mean.diff <- rep(NA, 3)

for(i in 1:3){
        subset <- ToothGrowth %>% filter(dose == doses[i])
        test <- with(subset, t.test(len[supp == "OJ"], len[supp == "VC"], var.equal = TRUE))
        p.value[i] <- test$p.value
        mean.diff[i] <- with(test, estimate[1] - estimate[2])
}

p.values <- data.frame(doses, p.value, crit.value, mean.diff) %>% arrange(p.value)
p.values <- p.values %>% mutate(sig = ifelse(p.value <= crit.value, "REJECT", "ACCEPT"))
p.values




