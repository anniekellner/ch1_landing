# Repro2
0 = no young or no data
1 = coy or yearling

# Repro 3
0 = anything but coy
1 = coy

# Repro4
0 = no data
1 = denning
2 = yearling
3 = coy


```{r}
# Repro 2
logreg$repro2 <- ifelse(logreg$repro == 2 | logreg$repro == 3, 1, 0) # 1 = coy OR yearling

# Repro 3
logreg$repro3 <- ifelse(logreg$repro == 2, 1, 0) # 1 = coy only

# Repro 4

logreg$repro4 <- 0

logreg <- logreg %>%
  mutate(repro4 = replace(repro4, repro == 1, 1)) %>%
  mutate(repro4 = replace(repro4, repro == 2, 3)) %>%
  mutate(repro4 = replace(repro4, repro == 3, 2))


repro  <- glm(start.swim ~ repro, data = logreg, family = binomial(link = 'logit'))
repro2 <- glm(start.swim ~ repro2, data = logreg, family = binomial(link = 'logit'))
repro3 <- glm(start.swim ~ repro3, data = logreg, family = binomial(link = 'logit'))
repro4 <- glm(start.swim ~ repro4, data = logreg, family = binomial(link = 'logit'))


aicc <- AICc(repro, repro2, repro3, repro4)

create_AICc_table(aicc)
aicc

summary(repro)
summary(repro2)
summary(repro3)
summary(repro4)
