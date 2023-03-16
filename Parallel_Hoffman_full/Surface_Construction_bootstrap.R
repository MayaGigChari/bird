df_surf<- data.frame(read.csv("CI_mpd_output_bootstrap.csv", header = TRUE)) #read in the data

low <- df_surf[1,]
med <- df_surf[3,]
high<- df_surf[2,]

install.packages("data.table")
library(data.table)
tree_sizes<- c(10,50,100,200,1000)

t_low <-transpose(low)
t_med <- transpose(med)
t_high <-transpose(high) 

t_low2 = data.frame(t_low[-1,])
colnames(t_low2) <- c("mpd")
t_low2$mpd = as.double(t_low2$mpd)
t_low2["sizes"] = tree_sizes
X_low = t_low2$sizes
Y_low = t_low2$mpd
plot(t_low2$sizes, t_low2$mpd)

t_med2 = data.frame(t_med[-1,])
colnames(t_med2) <- c("mpd")
t_med2$mpd = as.double(t_med2$mpd)
t_med2["sizes"] = tree_sizes
X_med = t_med2$sizes
Y_med = t_med2$mpd
plot(t_med2$sizes, t_med2$mpd)

t_high2 = data.frame(t_high[-1,])
colnames(t_high2) <- c("mpd")
t_high2$mpd = as.double(t_high2$mpd)
t_high2["sizes"] = tree_sizes
X_high = t_high2$sizes
Y_high = t_high2$mpd
plot(t_high2$sizes, t_high2$mpd)


#models 
#choose MM.3 (best fit)

model_low <- drm(Y_low ~ X_low, fct = MM.2())
model_med <- drm(Y_med ~ X_med, fct = MM.2())
model_high <- drm(Y_high ~ X_high, fct = MM.2())

summary(model_low)
summary(model_med)
summary(model_high)

plot(model_low, log = "", main = "MM-estimated 95% CI for pd", ylab = "expected PD", xlab = "tree size",col = "red",
     ylim = c(210, 300))
par(new=TRUE)
plot(model_med, log = "", main = "", ylab = "",xlab = "", col = "blue",
     ylim = c(210, 300))
par(new = TRUE)
plot(model_high, log = "", main = "", ylab = "", xlab = "",col = "red",
     ylim = c(210, 300))

x <- seq(10, 200, 1)
y_low<- 187.43493 + (285.79165 -187.43493)/(1+ 4.80982/x)
y_med<- 181.75421 + (286.05480-181.75421)/(1+4.01521/x)
y_high<-297.4075 + (286.30946-167.86292)/(1+183.0482 /x)


low_pred<- function(sample_size)
{
  lower <- 187.43493 + (285.79165 -187.43493)/(1+ 4.80982/sample_size)
  upper<- 167.86292 + (286.30946-167.86292)/(1+3.06938/sample_size)
  return(c(lower,upper))
}
plot(x,y_low,type = "l", xlab = "tree size", ylab = "pd", main = "true PD for a 200 taxa sample compared to expected")
lines(x, y_high, type = "l", col = 2)
polygon(c(x, rev(x)), c(y_low, rev(y_high)),
        col = "#6BD7AF")+points(200,265)

plot(model_low,  main = "michaelis Menten") 
+ (model_high, main = "michaelis Menten")

lm_log = lm(mpd~log(sizes), 
  t_low2)
plot(mpd~sizes, 
     t_low2)
curve(coef(lm_log)[1] + 
        coef(lm_log)[2]*log(x), 
      add=TRUE, 
      col = "red")

summary(lm_log)
library(ggplot2)


fit2 <- lm(t_low2$mpd~poly(t_low2$sizes,2,raw=TRUE))



ggplot(t_low2,
       aes(sizes,mpd)) + 
  geom_point(shape = 1) + 
  stat_smooth(method = "lm", 
              formula = y ~ log(x), 
              col = "red")


#need to use k fold cross vaildation and fit a polynomial regression model. 

Y <- unlist(t_low2$mpd)
X <- unlist(t_low2$sizes)
install.packages("devtools")
library(devtools)
library(aomisc)
devtools::install_github("onofriAndreaPG/aomisc")
model <- nls(Y ~ NLS.asymReg(X, init, m, plateau) )
model <- drm(Y ~ X, fct = DRC.asymReg())
model <- drm(Y ~ X, fct = MM.3())

summary(model)
plot(model)

#michaelis menten 
#f(x,(c,d,e)) = c + (d-e)/(1+(e/x))
#parameters: lower limit c, set to 0 in this case?
#e = "dose" halfway between c and d , d is the upper horizontal asymptote. 

summary(model)
# in this case 
#model f(x,(d,e)) = (284.987186-0)/(1+1.237342/x)
# y = 284.987186/(1 +1.237342/x)
# The mpd saturates at a value of about 285. when x is sufficiently large, 1.237342/x becomes increasingly small and 
# then the value approaches but doesn't reach 284.98. 

#need some way to store this function 
summary(model)
plot(model, log="", main = "michaelis Menten")

slice_plot(284.987186/(1 +1.237342/x) ~ x, domain(x = range(0, 1000)))

polygon(c(x, rev(x)), c(y2, rev(y1)),
        col = "#6BD7AF")
y1 = 

curve(187.43493+((285.79165-187.43493)/(1 +(4.80982/x))), from=10, to=1000, xlab="xvalue", ylab="yvalue", 
        col="blue", lwd=2, main="Plot of (3x^2 + x)"  )
?MM.2
