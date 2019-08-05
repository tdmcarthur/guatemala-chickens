



set.seed(100)

n.obs <- 500

#chickens.lamba <- 10 # 5
#chickens.treatment.nutrition.boost <- 0

chickens.treatment.nutrition.boost <-  0.225

mc.reps <- 10000

p.val.results.ls <- list()

for (rep.number in seq_len(mc.reps)) {
  
  
  #chicken.treatment.boost <- sample(0:1, size = n.obs, replace = TRUE, 
  #                                  prob = c(.5, .5)) # c(.75, .25)
  # i.e. average treatment effect is a boost of <X> chickens
  
  chicken.treatment.boost <- sample(c(0, 6), size = n.obs, replace = TRUE, 
                                    prob = c(.5, .5))
  
  treatment <- sample(0:1, size = n.obs, replace = TRUE)
  
  chickens.lamba <- rlnorm(n.obs, sdlog = 3)
  # Generate overdispered quasi-poisson
  # See https://stat.ethz.ch/pipermail/r-help/2006-June/107226.html
  # Overdispersion turns out to be crucial for our main result here.
  
  chickens.lamba[chickens.lamba > 50] <- 50
  
  y.chickens <- rpois(n.obs, lambda = chickens.lamba)
  
  y.chickens[is.na(y.chickens)] <- 0
  
  
  y.chickens <- y.chickens + treatment * chicken.treatment.boost 
  
  summary(y.chickens)
  
  chicken.boost.pval <- summary(lm(y.chickens ~ treatment))$coefficients["treatment", "Pr(>|t|)"]
  
  
  #girl.nutrition <- 50 + chickens.treatment.nutrition.boost * y.chickens + rnorm(n.obs, mean = 0, sd = 25)
  
  girl.nutrition <- 0.412 + 
    (mean(sqrt(y.chickens)) * chickens.treatment.nutrition.boost / mean(chicken.treatment.boost)) *  sqrt(y.chickens) + 
    rnorm(n.obs, mean = 0, sd = 1.5)
  # Decreasing marginal benefit of chickens
  
  summary(girl.nutrition)
  
  nutrition.boost.pval <- summary(lm(girl.nutrition ~ treatment))$coefficients["treatment", "Pr(>|t|)"]
  
    nutrition.boost.pval <- summary(lm(girl.nutrition ~ treatment))$coefficients["treatment", "Pr(>|t|)"]

  
  p.val.results.ls[[rep.number]] <- data.frame(
    chicken.boost.pval = chicken.boost.pval, 
    nutrition.boost.pval = nutrition.boost.pval )
  
}

p.val.results.dt <- data.table::rbindlist(p.val.results.ls)


summary(p.val.results.dt)

cor(p.val.results.dt)


effect.tab <- prop.table(table(
  chicken.boost = p.val.results.dt$chicken.boost.pval < .05,
  nutrition.boost = p.val.results.dt$nutrition.boost.pval < .05 ))


effect.tab

effect.tab[ "FALSE", "TRUE"] / (effect.tab[ "FALSE", "TRUE"] + effect.tab[ "TRUE", "TRUE"])


sum(effect.tab[, "TRUE"])

sum(effect.tab["TRUE", ])



# Lower effect size on chicken count --> higher probabilility of our result
# Lower effect size on nutrition --> higher probabilility of our result


.95





