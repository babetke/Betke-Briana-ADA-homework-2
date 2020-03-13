#### Code for hw2
# challenge 1
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot()) # b/c no grids in my graphs pls!!
f <- "https://raw.githubusercontent.com/difiore/ADA-datasets/master/IMDB-movies.csv"
d <- read_csv(f, col_names = TRUE)

# Filter out movies b/w 1920-1979 and runtime of < 240 minutes.
# Then create a variable called decades for each decade of movies.
s <- filter(d, runtimeMinutes < 240 & startYear %in% 1920:1979) %>% 
  mutate(decade = if_else(startYear %in% 1920:1929,"20s",
                         if_else(startYear %in% 1930:1939,"30s",
                                 if_else(startYear %in% 1940:1949,"40s",
                                         if_else(startYear %in% 1950:1959,"50s",
                                                if_else(startYear %in% 1960:1969,"60s","70s"))))))

# create histograms of runtimes by decade.  
p <- ggplot(s,aes(x=runtimeMinutes))+
  geom_histogram()+
  facet_wrap(~decade)
p

# Calculate mean and standard deviations by decade and save to results.
results <- group_by(s, decade)%>%
  summarise(n_cases = n(),
            avg = mean(runtimeMinutes, na.rm = TRUE),
            popSD = sqrt(sum((runtimeMinutes - mean(runtimeMinutes))^2) / length(runtimeMinutes)))
results

# take samples of 100 from each decade, calculate mean, standard deviation and 
Sample_decades <- group_by(s, decade) %>% sample_n(100) 

outcome <- summarize(Sample_decades,
    avg = mean(runtimeMinutes, na.rm = TRUE),
    SD = sd(runtimeMinutes, na.rm = TRUE),
    se = SD/sqrt(100)
  )
outcome

# compare single sample results 
# calculate the SE for pop
results
(results$popSD)/sqrt(100)

library(mosaic)

# 10,000 samples of size 100 for each decade?
# probably the long way to do it but subset the decades and sample each one w/ a for loop?
just20s <- filter(s, decade == "20s")
just30s <- filter(s, decade == "30s")
just40s <- filter(s, decade == "40s")
just50s <- filter(s, decade == "50s")
just60s <- filter(s, decade == "60s")
just70s <- filter(s, decade == "70s")

k <- 10000 # number of samples
n <- 100  # size of each sample

samp20 <- list(length = k)
# samp30 <- list(length = k)
# samp40 <- list(length = k)
# samp50 <- list(length = k)
# samp60 <- list(length = k)
samp70 <- list(length = k)# create a dummy variable to hold each sample
for (i in 1:k) {
  samp20[[i]] <- sample(just20s$runtimeMinutes, size = n, replace = FALSE)
  # samp30[[i]] <- sample(just30s, size = n, replace = FALSE)
  # samp40[[i]] <- sample(just40s, size = n, replace = FALSE)
  # samp50[[i]] <- sample(just50s, size = n, replace = FALSE)
  # samp60[[i]] <- sample(just60s, size = n, replace = FALSE)
  # samp70[[i]] <- sample(just70s, size = n, replace = FALSE)
}
# if I head all of these, my rmarkdown will be way too long...


# caculating the means
m20 <- vector(length = k)
m30 <- vector(length = k)
m40 <- vector(length = k)
m50 <- vector(length = k)
m60 <- vector(length = k)
m70 <- vector(length = k)

for (i in 1:k) {
  m20[[i]] <- mean(samp20[[i]][["runtimeMinutes"]])
  m30[[i]] <- mean(samp30[[i]][["runtimeMinutes"]])
  m40[[i]] <- mean(samp40[[i]][["runtimeMinutes"]])
  m50[[i]] <- mean(samp50[[i]][["runtimeMinutes"]])
  m60[[i]] <- mean(samp60[[i]][["runtimeMinutes"]])
  m70[[i]] <- mean(samp70[[i]][["runtimeMinutes"]])
}

head(m20)
head(m30)
head(m40)
head(m50)
head(m60)
head(m60)
head(m70)

# Standard deviations
# Calculating sd
sample_sd20 <- vector(length = k)
sample_sd30 <- vector(length = k)
sample_sd40 <- vector(length = k)
sample_sd50 <- vector(length = k)
sample_sd60 <- vector(length = k)
sample_sd70 <- vector(length = k)
# create a dummy variable to hold the SD of each sample
for (i in 1:k) {
  sample_sd20[[i]] <- sd(samp20[[i]][["runtimeMinutes"]]) # a vector of SDs for each sample
  sample_sd30[[i]] <- sd(samp30[[i]][["runtimeMinutes"]])
  sample_sd40[[i]] <- sd(samp40[[i]][["runtimeMinutes"]])
  sample_sd50[[i]] <- sd(samp50[[i]][["runtimeMinutes"]])
  sample_sd60[[i]] <- sd(samp60[[i]][["runtimeMinutes"]])
  sample_sd70[[i]] <- sd(samp70[[i]][["runtimeMinutes"]])
}

head(sample_sd20)
head(sample_sd30)
head(sample_sd40)
head(sample_sd50)
head(sample_sd60)
head(sample_sd70)

# Not clear if we should calculate the sd for the distirbution to compare to pop se but 
# its right here just incase....
sd(m20)
sd(m30)
sd(m40)
sd(m50)
sd(m60)
sd(m70)


#Challenge 2
ppois(13, lambda = 18)
dpois(0, lambda = 18)
ppois(0, lambda = 18)
dpois(7, lambda = 18)
ppois(20, lambda = 18, lower.tail = FALSE)

# plot probability mass function 
library(mosaic)
l <- 18
pmf_poisson <-
  plotDist(
    "pois",
    lambda = l,
    xlim = c(0,40),
    main = paste0("Poisson Distribution\nwith lambda=", l),
    xlab = "x",
    ylab = "Pr(X=x)"
  )
pmf_poisson

# dample and graph with histogram()
q <- rpois(520, lambda = 18)
histgram(q,  xlim = c(0,40))


# challenge 3
# get that zombie data 
f <- "https://raw.githubusercontent.com/difiore/ADA-datasets/master/zombies.csv"
d <- read_csv(f, col_names = TRUE)

# calculating population mean and sd for each quatitative random variable
pop_var <- function(x) {
  sum((x - mean(x))^2) / (length(x))
}

pop_sd <- function(x) {
  sqrt(pop_var(x))
}

results2 <- summarise(d,
  avgH = mean(height, na.rm = TRUE),
  avgW = mean(weight, na.rm = TRUE),
  avgZ = mean(zombies_killed, na.rm = TRUE),
  avgE = mean(years_of_education,na.rm = TRUE),
  avgA = mean(age,na.rm = TRUE),
  sdH = pop_sd(height),
  sdW = pop_sd(weight),
  sdZ = pop_sd(zombies_killed),
  sdE = pop_sd(years_of_education),
  sdA = pop_sd(age)
)
results2

# scatter plot of weight and height
p1 <- ggplot(data = d, aes(
  x = height,
  y = age,)) +
  geom_point(na.rm = TRUE)
p1

p2 <- ggplot(data = d, aes(
  x = weight,
  y = age,)) +
  geom_point(na.rm = TRUE)
p2

plot_grid(p1, p2, labels = c("A", "B"), label_size = 12, nrow = 1)

# Histograms and qq plots of each variable.
par(mfrow=c(1,5))
hist(d$height)
hist(d$weight)
hist(d$zombies_killed)
hist(d$years_of_education)
hist(d$age)
par(mfrow=c(1,1))

par(mfrow=c(1,5))
qqnorm(d$height)
qqline(d$height, col = "red")

qqnorm(d$weight)
qqline(d$weight, col = "red")

qqnorm(d$zombies_killed)
qqline(d$zombies_killed, col = "red")

qqnorm(d$years_of_education)
qqline(d$years_of_education, col = "red")

qqnorm(d$age)
qqline(d$age, col = "red")
par(mfrow=c(1,1))
# the number of zombies killed and the years of education appear to not be normally
# distributed. Both are skewed right, as shown in the histograms.

# single sample
(Zombie30 <- sample_n(d, 30))

# calculations for height
(meanH <- mean(Zombie30$height))
(SD.h <- sd(Zombie30$height))
(seH <- sd(Zombie30$height) / sqrt(length(Zombie30$height)))
(ci_normH <- meanH + c(-1, 1) * qnorm(1 - 0.05 / 2) * seH)

# calcualtions for weight
(meanW <- mean(Zombie30$weight))
(SD.W <- sd(Zombie30$weight))
(seW <- sd(Zombie30$weight) / sqrt(length(Zombie30$weight)))
(ci_normW <- meanW + c(-1, 1) * qnorm(1 - 0.05 / 2) * seW)

# Calcualtions for zombies killed
(meanK <- mean(Zombie30$zombies_killed))
(SD.K <- sd(Zombie30$zombies_killed))
(seK <- sd(Zombie30$zombies_killed) / sqrt(length(Zombie30$zombies_killed)))
(ci_normW <- meanK + c(-1, 1) * qnorm(1 - 0.05 / 2) * seK)

# Calculations for years of education
(meanE <- mean(Zombie30$years_of_education))
(SD.E <- sd(Zombie30$years_of_education))
(seE <- sd(Zombie30$years_of_education) / sqrt(length(Zombie30$years_of_education)))
(ci_normE <- meanE + c(-1, 1) * qnorm(1 - 0.05 / 2) * seE)

# Calculations for age
(meanA <- mean(Zombie30$age))
(SD.A <- sd(Zombie30$years_of_education))
(seA <- sd(Zombie30$years_of_education) / sqrt(length(Zombie30$years_of_education)))
(ci_normA <- meanA + c(-1, 1) * qnorm(1 - 0.05 / 2) * seA)

# 99 samples of si sizr 30
k <- 99 # number of samples
n <- 30 # size of each sample
s <- list(length = k) # create a dummy variable to hold each sample
for (i in 1:k) {
  s[[i]] <- sample(d, size = n, replace = FALSE)
}
head(s)

# Calculating means
mH <- vector(length = k)
mW <- vector(length = k)
mK <- vector(length = k)
mE <- vector(length = k)
mA <- vector(length = k)
for (i in 1:k) {
  mH[[i]] <- mean(s[[i]][["height"]])
  mW[[i]] <- mean(s[[i]][["weight"]])
  mK[[i]] <- mean(s[[i]][["zombies_killed"]])
  mE[[i]] <- mean(s[[i]][["years_of_education"]])
  mA[[i]] <- mean(s[[i]][["age"]])
}
head(mH)
head(mW)
head(mK)
head(mE)
head(mA)

# mean of sampling distributions
# Height
mean(mH) # mean of sampling distirbution
results2$avgH # population mean

mean(mW) # mean of sampling distirbution
results2$avgW # population mean 

mean(mK) # mean of sampling distirbution
results2$avgZ # population mean 

mean(mE) # mean of sampling distirbution
results2$avgE # population mean 

mean(mA) # mean of sampling distirbution
results2$avgA # population mean 

# Calculating sd
sample_sdH <- vector(length = k)
sample_sdW <- vector(length = k)
sample_sdK <- vector(length = k)
sample_sdE <- vector(length = k)
sample_sdA <- vector(length = k)
# create a dummy variable to hold the SD of each sample
for (i in 1:k) {
  sample_sdH[[i]] <- sd(s[[i]][["height"]]) # a vector of SDs for each sample
  sample_sdW[[i]] <- sd(s[[i]][["weight"]])
  sample_sdK[[i]] <- sd(s[[i]][["zombies_killed"]])
  sample_sdE[[i]] <- sd(s[[i]][["years_of_education"]])
  sample_sdA[[i]] <- sd(s[[i]][["age"]])
}

head(sample_sdH)
head(sample_sdW)
head(sample_sdK)
head(sample_sdE)
head(sample_sdA)

# Comparison to population se?
sd(mH)

(sample_seH <- sample_sdH / sqrt(n))
mean(sample_seH)

(pop_se <- results2$sdH / (sqrt(n)))


sd(mW)
(pop_se <- results2$sdW / (sqrt(n)))

sd(mk)
(pop_se <- results2$sdk / (sqrt(n)))

sd(mE)
(pop_se <- results2$sdE / (sqrt(n)))

sd(mA)
(pop_se <- results2$sdA / (sqrt(n)))

# distributions of sample means
par(mfrow=c(1,5))
hist(mH)
hist(mW)
hist(mK)
hist(mE)
hist(mA)
par(mfrow=c(1,1))





