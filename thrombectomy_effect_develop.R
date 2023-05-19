# simulate data from randomized trials
# t is number of trials
# participants is vector with number of participants in each of the t trials
# groups is vector with the group each of the t trials belongs to
# a is log odds of outcome in control group
# b is treatment effect of intervention

simulate_data <- function(trials, participants, a, b, group) {
  # Set seed for reproducibility
  set.seed(123)

  # Initialize an empty list to store the simulated data for each trial
  data_list <- vector("list", length = length(trials))

  for (i in seq_along(1:trials)) {
    # Generate treatment assignment (0 for control, 1 for treatment)
    treatment <- sample(c(0, 1), size = participants[i], replace = TRUE)

    # Calculate probabilities of success for control and treatment groups
    prob_control <- plogis(a[i])
    prob_treatment <- plogis(a[i] + b[i])

    # Generate outcomes based on treatment assignment and probabilities
    outcome <- ifelse(treatment == 0, rbinom(participants[i], 1, prob_control), rbinom(participants[i], 1, prob_treatment))

    # Create a data frame with the simulated data for the current trial
    trial_data <- data.frame(trial = rep(i, participants[i]), group = rep(group[i], participants[i]), treatment = treatment, outcome = outcome)

    # Store the simulated data in the list
    data_list[[i]] <- trial_data
  }

  # Combine the data for all trials into a single data frame
  data <- do.call(rbind, data_list)

  return(data)
}

# Use above function to simulate outcomes for 4 groups of trials, each correspodning to different stroke types
set.seed(123)

# Define the number of trials and participants in each group
trials <- 3 + 9 + 4 + 4 # 3 large trials, 9 early, 4 late, 4 basilar
participants <- c(
  230+225, 100+102, 177+171, # large: angel, rescue, select
  164+146, 35+34, 233+265, 33+30, 111+111, 103+103, 97+94, 50+46, 200+202, # early: escape, extend, mrclean, piste, resilient, revascat, swift, therapy, thrace
  107+99, 91+91, 252+247, 12+21, # late: dawn, defuse3, mrclean late, positive
  226+114, 110+107, 134+146, 66+65 # basilar: attention, baoche, basics, best
)

# Define different values of a and b for each dataset
a <- c(
  rep(-1.5,3),
  rep(-1,9),
  rep(-1,4),
  rep(-1.5,4)
)
b <- rep(1,20)

# Define group membership for each dataset
group <- c(
  rep(1,3),
  rep(2,9),
  rep(3,4),
  rep(4,4)
)

simulated_data <- simulate_data(trials,participants,a,b,group)

# Run model
# Specifies how to code trimmed data list used in HMC
dat <- list(
  outcome = simulated_data$outcome,
  trial = as.factor(simulated_data$trial),
  treatment = simulated_data$treatment,
  group = simulated_data$group)

# partial pooling model with fixed effect for group and informed priors
# model should recover parameter values from the data generating process
set.seed(123)
model <- ulam(
  alist(
    outcome ~ bernoulli(p),
    logit(p) <- a[trial] + b[trial]*treatment + c[group],
    # define effects using other parameters
    save> vector[20]:a <<- abar + za*sigma,
    save> vector[20]:b <<- bbar + zb*tau,
    # z-scored effects
    vector[20]:za ~ normal(0,1),
    vector[20]:zb ~ normal(0,1),
    # hyper-priors
    abar ~ normal(-1,1),
    bbar ~ normal(0,0.5),
    sigma ~ half_normal(0,1),
    tau ~ half_normal(0,1),
    # fixed effect priors
    c[group] ~ normal(0,1)
    ), data=dat,chains=8,cores=4,control=list(adapt_delta=0.99))
