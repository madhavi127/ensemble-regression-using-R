# set the margins
tmpmar <- par("mar")
tmpmar[3] <- 0.5
par(mar=tmpmar)

# get underlying plot
#1. Import the csv file with past 3 months history for infy share trend
scrip.data <- read.csv("C:/Users/user/downloads/idea_90 (1).csv")


plot(scrip.data$Open.Price, scrip.data$High.Price)

y <- scrip.data$High.Price
x <- scrip.data$Open.Price

# basic straight line of fit
fit_ln_glm <- glm(y~x) # same as lm(y~x)
co <- coef(fit_ln_glm)
abline(fit_ln_glm, col="black", lwd=2)
summary(fit_ln_glm)
co

# exponential
f1 <- function(x,a,b) {a * exp(b * x)}
fit_exp_nls <- nls(log(y) ~ f1(log(x),a,b), start = c(a=1, b=1)) 
co <- coef(fit_exp_nls)
curve(f1(x, a=co[1], b=co[2]), add = TRUE, col='blue', lwd=2) 
summary(fit_exp_nls)
co

# logarithmic
f2 <- function(x,a,b) {a * log(x) + b}
fit_log_lns <- nls(y ~ f2(x,a,b), start = c(a=1, b=1)) 
co <- coef(fit_log_lns)
curve(f2(x, a=co[1], b=co[2]), add = TRUE, col="orange", lwd=2) 
summary(fit_log_lns)
co
# polynomial
f3 <- function(x,a,b,d) {(a*x^2) + (b*x) + d}
fit4_poly_nls <- nls(y ~ f3(x,a,b,d), start = c(a=1, b=1, d=1)) 
co <- coef(fit4_poly_nls)
curve(f3(x, a=co[1], b=co[2], d=co[3]), add = TRUE, col="red", lwd=2) 
summary(fit4_poly_nls)
co

#Add a descriptive legend:
# legend
legend("topleft",
       legend=c("linear","exponential","logarithmic","polynomial"),
       col=c("blue","green","orange","red"),
       lwd=2,
)

# use predict() function to prform prediction on the model using testdata
pred_fit_ln_glm <- predict(fit_ln_glm,newdata = scrip.data)


pred_fit_exp_nls <- predict(fit_exp_nls,newdata = scrip.data)
pred_fit_exp_nls <- as.numeric(lapply(pred_fit_exp_nls, function(x) {exp(x)}))


pred_fit_log_lns <- predict(fit_log_lns, newdata = scrip.data)

pred_fit4_poly_nls <- predict(fit4_poly_nls,newdata = scrip.data)

#view predicted values and actual values sid by side in data frame 
p1 <- data.frame(pred_fit_ln_glm, pred_fit_exp_nls, pred_fit_log_lns,  pred_fit4_poly_nls)

p1 <- data.frame(lapply(p1, function(x) { as.numeric(gsub(",", "", x)) }))

tp1 <- data.frame(t(p1))

tp1_mean <- lapply(tp1, function(x) { mean(x) })
tp1_median <- lapply(tp1, function(x) { median(x) })

tp1 <- rbind(tp1, Mean=tp1_mean)
tp1 <- rbind(tp1, Median=tp1_median)

p1 <- data.frame(t(tp1))


p1 <- cbind(p1, Date=scrip.data$Date)
p1 <- cbind(p1, High.Price=scrip.data$High.Price)
p1 <- cbind(p1, Open.Price=scrip.data$Open.Price)

View(p1)
