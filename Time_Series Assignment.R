#Daily India Covid-19#

data = read.csv("covid.csv")

covid_new = data.frame(Date = data$Date, Daily_Conf = data$Daily.Confirmed)

covid = covid_new[48:137,]

yt = data.frame(Daily_C = covid$Daily_Conf)

#1) Model 1: (m = 7 point moving average)

T_hat_t = numeric(90)
T_hat_t[1] = NA
T_hat_t[2] = NA
T_hat_t[3] = NA

for (i in seq_along(covid$Daily_Conf)){
  
  T_hat_t[i+3] = (sum(yt[i,1],yt[i+1,1],yt[i+2,1],yt[i+3,1],yt[i+4,1],yt[i+5,1],yt[i+6,1]))/7 
}

D_Conf = data.frame(Date = covid$Date,yt, T_hat_t = T_hat_t[1:90])

#Calculation of Ephsilon_t_hat

Err_t_hat = data.frame(err_t_hat = D_Conf$Daily_C - D_Conf$T_hat_t) 
Err_t_hat_sq = data.frame(Err_t_hat**2)

m_avg_tab = data.frame(D_Conf,Err_t_hat,Err_t_hat_sq)

#Average of Eph_t_hat_sq values
Err_t_hat_sq_new = mean(na.omit(m_avg_tab$err_t_hat.1))

RMSE = sqrt(Err_t_hat_sq_new)

#Hence, the RMSE value is 363.033 for m-moving average procedure.

plot(seq(1,90,1), m_avg_tab$Daily_C, type="p", main = "Daily count of COVID-19 and Trend estimate (m-moving average)", xlab = "Days",ylab = "Count on each day")
lines(seq(1,90,1), T_hat_t[1:90], col= "green" )


#2)Method2: Exponential Smoothing

#calcultion of T_hat_t using: 
#T_hat_t = alpha*yt +(1-alpha)*T_hat_t-1
#T_hat_1 =y1
#consider alpha = 0.25

T_hat_t2 = numeric(90)
T_hat_t2[1] = 20

for (i in seq_along(covid$Daily_Conf)){
  
  T_hat_t2[i+1] = 0.25*(yt[i+1,1]) + (1-0.25)*(yt[i,1])
  
}

T_hat_t2_val = na.omit(T_hat_t2)

expo_sm_table = data.frame(covid,T_hat_t2_val)

# Calculation of ephsilon t and its square values.

Err_hat_t2 = expo_sm_table$Daily_Conf - expo_sm_table$T_hat_t2_val
Err_hat_t2_sq = Err_hat_t2 **2

expo_sm_tab = data.frame(expo_sm_table,Err_hat_t2,Err_hat_t2_sq)

avg_sq_error = mean(Err_hat_t2_sq)

## RMSE value
RMSE2 = sqrt(avg_sq_error)

#The RMSE value for Exponential Smoothing Process is 471.5124

plot(seq(1,90,1),expo_sm_tab$Daily_Conf, type="p", main = "Daily count of COVID-19 and Trend estimate (exponential smoothing)", xlab = "Days",ylab = "Count on each day")
lines(seq(1,90,1), expo_sm_tab$T_hat_t2_val[1:90], col="magenta")


#3) Curve fitting method


t = seq(1,90,1)

#Specific to general
#First order using yt= a + b*t + ut

#### using lm function for yt= a + b*t + ut. 
#H0: b = 0
#Ha: b!= 0

yt_linear =lm(covid$Daily_Conf~t) 
summary(yt_linear)

#Null gets rejected. Moving to higher order polynomial

### using lm function for second order, yt= a + b*t + c*t^2 + ut

#Second order
yt_quad=lm(covid$Daily_Conf~t+I(t^2))
summary(yt_quad)

#the p-value for the last co-efficient is less than 0.05. Null gets rejected. Moving to higher order polynomial

#Third order
yt_cube=lm(covid$Daily_Conf~t+I(t^2)+I(t^3))
summary(yt_cube)

#we see that the p-value for the last co-efficient is greater than 0.05 which means it is insignificant.

# Therefore, our data fits second order polynomial 

#yt = 373.53345 - 39.91378*t+(1.87282)*t^2

T_hat_t3 = c()
for (i in 1:90){
  T_hat_t3[i] = 373.53345 - (39.91378) * i+(1.87282)*(i^2)
}

#Calculating the error and square of error. 
Err_hat_t3 = yt - T_hat_t3
Err_hat_t3_sq = Err_hat_t3**2

#creating final dataframe for curve fitting model
Cf_tab = data.frame(covid,T_hat_t3, Err_hat_t3, Err_hat_t3_sq)
avg_sq_error3 = mean(Cf_tab$Daily_C.1)

#RMSE for curve fitting method
RMSE3 = sqrt(avg_sq_error3)

#plotting the trend

plot(seq(1,90,1),Cf_tab$Daily_Conf, type="p", main = "Daily count of COVID-19 and Trend estimate(CURVE FITTING)", xlab = "Days",ylab = "Count on each day")
lines(seq(1,90,1), Cf_tab$T_hat_t3, col="purple")