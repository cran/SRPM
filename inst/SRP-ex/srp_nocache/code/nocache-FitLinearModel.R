###################################################
### chunk number 2: FitLinearModel
###################################################
data(airquality)
fit <- lm(Ozone ~ Temp + Wind + Solar.R, data = airquality)


