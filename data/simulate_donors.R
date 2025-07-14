# Function to simulate data for Anova chapter

#SD in willingness to donate in donors.sav is 1.65633
simulate_data=function(Clooney_avg=1,ClooneyFemale_avg=Clooney_avg,ClooneyMale_avg=Clooney_avg,
                       Jolie_avg=1,JolieFemale_avg=Jolie_avg,JolieMale_avg=Jolie_avg,
                       Nobody_avg=1,NobodyFemale_avg=Nobody_avg,NobodyMale_avg=Nobody_avg, 
                       Clooney_sd=1.63920,ClooneyFemale_sd=Clooney_sd,ClooneyMale_sd=Clooney_sd,
                       Nobody_sd=1.47389,NobodyFemale_sd=Nobody_sd,NobodyMale_sd=Nobody_sd,
                       Jolie_sd=1.63033,JolieFemale_sd=Jolie_sd,JolieMale_sd=Jolie_sd,
                       seed = 123,N_per_endorser=50){
  set.seed(seed)
  
  d=tibble(
    endorser = rep(c("Nobody", "Clooney", "Jolie"), each = N_per_endorser),
    sex=rep(rep(c('female','male'),each= N_per_endorser/2), times = 3)
  ) %>% 
    mutate(
      willingness = case_when(
        endorser == "Nobody" & sex=='female'  ~ rnorm(n(), mean = NobodyFemale_avg, sd = NobodyFemale_sd),
        endorser == "Nobody" & sex=='male'  ~ rnorm(n(), mean = NobodyMale_avg, sd = NobodyMale_sd),
        endorser == "Clooney" & sex=='female' ~ rnorm(n(), mean = ClooneyFemale_avg, sd = ClooneyFemale_sd),
        endorser == "Clooney" & sex=='male'~ rnorm(n(), mean = ClooneyMale_avg, sd = ClooneyMale_sd),
        endorser == "Jolie" &  sex=='female' ~ rnorm(n(), mean = JolieFemale_avg, sd = JolieFemale_sd),
        endorser == "Jolie" & sex=='male' ~ rnorm(n(), mean = JolieMale_avg, sd = JolieMale_sd)
      ),
      # keep values within the stated 1–10 bounds
      willingness = pmin(pmax(willingness, 1), 10)
    )
  return(d)
}
