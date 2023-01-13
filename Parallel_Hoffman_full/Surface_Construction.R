df_surf<- data.frame(read.csv("Sample_output.csv", header = TRUE)) #read in the data

low <- df_surf[1,]
med <- df_surf[2,]
high<- df_surf[3,]

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
Y_med = t_high2$mpd
plot(t_med2$sizes, t_med2$mpd)

t_high2 = data.frame(t_high[-1,])
colnames(t_high2) <- c("mpd")
t_high2$mpd = as.double(t_high2$mpd)
t_high2["sizes"] = tree_sizes
X_high = t_med2$sizes
Y_high = t_high2$mpd
plot(t_high2$sizes, t_high2$mpd)


#models 

model_low <- drm(Y_low ~ X_low, fct = MM.2())
model_med <- drm(Y_med ~ X_med, fct = MM.2())
model_high <- drm(Y_high ~ X_high, fct = MM.2())


plot(model_low, log = "", main = "Michaelis-Menten function", 
     ylim = c(250, 290))
par(new=TRUE)
plot(model_med, log = "", main = "Michaelis-Menten function", 
     ylim = c(250, 290))
par(new = TRUE)
plot(model_high, log = "", main = "Michaelis-Menten function", 
     ylim = c(250, 290))


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
model <- drm(Y ~ X, fct = MM.2())

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


model_mean = 
