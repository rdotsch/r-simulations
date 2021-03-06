
# --- DESCRIPTION --------------------------------------------------------------

# Description: Generates animated plot showing p-curve depending on increasing sample size
# Output: Animated plot as pcurvebypower.gif in current working directory
# Author: Ron Dotsch (r.dotsch@uu.nl, http://ron.dotsch.org/)


# --- INSTALL AND LOAD DEPENDENCIES --------------------------------------------

# Run the following two lines to install necessary dependencies
# install.packages(c('devtools', 'tidyverse', 'cowplot', 'tweenr'))
# devtools::install_github("dgrtwo/gganimate")

library(tidyverse)
library(gganimate)
library(animation)

# --- SETTINGS -----------------------------------------------------------------

# Settings for simulation of one-sample t-test (test against H0: m = 0 )
m    <- 0.3    # Population mean
sd   <- 1      # Population standard deviation
N    <- 2:200  # Sequence of sample sizes
iter <- 10000  # Number of iterations for each sample size

# --- SIMULATION ---------------------------------------------------------------

results <- data.frame(i=rep(1:iter, times=length(N)), N=rep(N, each=iter)) %>%
  
  # Simulate data for each sample size N for each iteration i
  group_by(i, N) %>%
  mutate(
    # Compute p value for t.test
    p.value = t.test(rnorm(N, m, sd))$p.value,
    
    # Round p values upwards to two-digit bins
    p.bin = round(p.value + 0.01, 2)) %>%
  
  # Keep only significant results
  filter(p.bin <= 0.05) %>%
  
  # Compute percentage of p values that fall within a single bin for this sample size
  group_by(N, p.bin) %>%
  summarise(p.bin.percentage = 100 * n() / iter) %>%
  
  # Compute power for this sample size 
  group_by(N) %>%
  mutate(power = sum(p.bin.percentage))

# --- VISUALIZATION ------------------------------------------------------------

theme_set(theme_bw())

max.percentage <- max(results$p.bin.percentage)
p <- ggplot(results, aes(x=p.bin, y=p.bin.percentage, group=N, frame=N)) + 
  geom_line() + 
  geom_point() +
  geom_label(aes(label=paste0(round(p.bin.percentage, 0), '%')), nudge_y=max.percentage / 20) +
  geom_text(aes(label=paste0('N=', N, '; ', round(power, 2), '% Power'), x=0.03, y=max.percentage + 5)) +
  labs(x='p-values\n(rounded up)', y='% of p-values') 

ani.options(interval=0.2)
gganimate(p, filename = 'pcurvebypower.gif', title_frame = F)
