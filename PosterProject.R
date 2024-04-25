install.packages("posterdown")
library(tidyverse)
library(posterdown)
library(stargazer)

mod1 <- lm(price ~ distance + source + surge_multiplier, data = cab_rides)


stargazer(mod1, type = "text", title = "Linear Regression Model: Impact of Distance on Ride-Sharing Pricing",
          covariate.labels = c("Distance", "Surge Multiplier", "Source",
          "Source: Boston University", 
          "Source: Fenway",
          "Source: Financial District",
          "Source: Haymarket Square",
          "Source: North End",
          "Source: North Station",
          "Source: Northeastern University",
          "Source: Theatre District",
          "Source: West End"),
          dep.var.labels = "Price (USD)",
          se = NULL, conf.int = TRUE)

uberplot=
  cab_rides|>
  filter(cab_type == "Uber")|>
  ggplot(aes(x=distance, y = price))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  labs(title = "Uber Pricing By Distance",
       x = "Distance (miles)",
       y = "Price (USD)")+
  scale_y_continuous(limits = c(0,100),
                     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
  scale_x_continuous(limits = c(0, 8),
                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8))+
  theme_bw()

uberplot


lyftplot=
  cab_rides|>
  filter(cab_type == "Lyft")|>
  ggplot(aes(x=distance, y = price))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  labs(title = "Lyft Pricing By Distance",
       x = "Distance (miles)",
       y = "Price (USD)")+
  scale_y_continuous(limits = c(0,100),
                     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
  scale_x_continuous(limits = c(0, 8),
                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8))+
  theme_bw()


lyftplot
