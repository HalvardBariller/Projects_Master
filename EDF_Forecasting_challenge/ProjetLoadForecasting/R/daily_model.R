#' Daily linear model with AIC penalty
#'
#' Fit a linear model for each day based on the equation found with AIC penalty
#'
#' @param dataset the dataset on which the models are fitted
#' @param covid_period the subset of train set during COVID and the test set
#'
#' @return the forecasts computed with the different models on COVID period
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export


daily_model <- function(dataset = Data0[sel_a,], covid_period = rbind(Data0[sel_b,-2],Data1[,-20])){
  mon <- dataset[which(dataset$WeekDays=="Monday"),]
  tue <- dataset[which(dataset$WeekDays=="Tuesday"),]
  wed <- dataset[which(dataset$WeekDays=="Wednesday"),]
  thu <- dataset[which(dataset$WeekDays=="Thursday"),]
  fri <- dataset[which(dataset$WeekDays=="Friday"),]
  sat <- dataset[which(dataset$WeekDays=="Saturday"),]
  sun <- dataset[which(dataset$WeekDays=="Sunday"),]
  mon_fit <- lm(Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
                  toy + BH + DLS + Summer_break + Christmas_break +
                  Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
                  Load.1:Temp_s99_max + Load.1:toy + Load.1:DLS +
                  Load.1:Summer_break + Load.1:Christmas_break + Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max + Load.7:DLS + Load.7:Summer_break +
                  Load.7:Christmas_break + Temp:Temp_s95 + Temp:Temp_s99_max +
                  Temp:DLS + Temp:Christmas_break + Temp_s95:Temp_s95_max +
                  Temp_s95:Temp_s99_max + Temp_s95:toy +
                  Temp_s95:DLS + Temp_s95:Summer_break + Temp_s95:Christmas_break +
                  Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                  Temp_s95_max:Christmas_break + Temp_s95_max:Time + Temp_s99_max:toy +
                  Temp_s99_max:BH + Temp_s99_max:DLS +
                  toy:BH + toy:DLS + toy:Summer_break + toy:Christmas_break +
                  BH:DLS + BH:Summer_break + BH:Christmas_break +
                  BH:Time + Summer_break:Time,
                data= subset(mon, select = -c(WeekDays)))
  tue_fit <- lm(Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
                  toy + BH + DLS + Summer_break + Christmas_break +
                  Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
                  Load.1:Temp_s99_max + Load.1:toy + Load.1:DLS +
                  Load.1:Summer_break + Load.1:Christmas_break + Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max + Load.7:DLS + Load.7:Summer_break +
                  Load.7:Christmas_break + Temp:Temp_s95 + Temp:Temp_s99_max +
                  Temp:DLS + Temp:Christmas_break + Temp_s95:Temp_s95_max +
                  Temp_s95:Temp_s99_max + Temp_s95:toy +
                  Temp_s95:DLS + Temp_s95:Summer_break + Temp_s95:Christmas_break +
                  Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                  Temp_s95_max:Christmas_break + Temp_s95_max:Time + Temp_s99_max:toy +
                  Temp_s99_max:BH + Temp_s99_max:DLS +
                  toy:BH + toy:DLS + toy:Summer_break + toy:Christmas_break +
                  BH:DLS + BH:Summer_break + BH:Christmas_break +
                  BH:Time + Summer_break:Time,
                data= subset(tue, select = -c(WeekDays)))
  wed_fit <- lm(Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
                  toy + BH + DLS + Summer_break + Christmas_break +
                  Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
                  Load.1:Temp_s99_max + Load.1:toy + Load.1:DLS +
                  Load.1:Summer_break + Load.1:Christmas_break + Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max + Load.7:DLS + Load.7:Summer_break +
                  Load.7:Christmas_break + Temp:Temp_s95 + Temp:Temp_s99_max +
                  Temp:DLS + Temp:Christmas_break + Temp_s95:Temp_s95_max +
                  Temp_s95:Temp_s99_max + Temp_s95:toy +
                  Temp_s95:DLS + Temp_s95:Summer_break + Temp_s95:Christmas_break +
                  Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                  Temp_s95_max:Christmas_break + Temp_s95_max:Time + Temp_s99_max:toy +
                  Temp_s99_max:BH + Temp_s99_max:DLS +
                  toy:BH + toy:DLS + toy:Summer_break + toy:Christmas_break +
                  BH:DLS + BH:Summer_break + BH:Christmas_break +
                  BH:Time + Summer_break:Time,
                data= subset(wed, select = -c(WeekDays)))
  thu_fit <- lm(Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
                  toy + BH + DLS + Summer_break + Christmas_break +
                  Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
                  Load.1:Temp_s99_max + Load.1:toy + Load.1:DLS +
                  Load.1:Summer_break + Load.1:Christmas_break + Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max + Load.7:DLS + Load.7:Summer_break +
                  Load.7:Christmas_break + Temp:Temp_s95 + Temp:Temp_s99_max +
                  Temp:DLS + Temp:Christmas_break + Temp_s95:Temp_s95_max +
                  Temp_s95:Temp_s99_max + Temp_s95:toy +
                  Temp_s95:DLS + Temp_s95:Summer_break + Temp_s95:Christmas_break +
                  Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                  Temp_s95_max:Christmas_break + Temp_s95_max:Time + Temp_s99_max:toy +
                  Temp_s99_max:BH + Temp_s99_max:DLS +
                  toy:BH + toy:DLS + toy:Summer_break + toy:Christmas_break +
                  BH:DLS + BH:Summer_break + BH:Christmas_break +
                  BH:Time + Summer_break:Time,
                data= subset(thu, select = -c(WeekDays)))
  fri_fit <- lm(Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
                  toy + BH + DLS + Summer_break + Christmas_break +
                  Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
                  Load.1:Temp_s99_max + Load.1:toy + Load.1:DLS +
                  Load.1:Summer_break + Load.1:Christmas_break + Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max + Load.7:DLS + Load.7:Summer_break +
                  Load.7:Christmas_break + Temp:Temp_s95 + Temp:Temp_s99_max +
                  Temp:DLS + Temp:Christmas_break + Temp_s95:Temp_s95_max +
                  Temp_s95:Temp_s99_max + Temp_s95:toy +
                  Temp_s95:DLS + Temp_s95:Summer_break + Temp_s95:Christmas_break +
                  Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                  Temp_s95_max:Christmas_break + Temp_s95_max:Time + Temp_s99_max:toy +
                  Temp_s99_max:BH + Temp_s99_max:DLS +
                  toy:BH + toy:DLS + toy:Summer_break + toy:Christmas_break +
                  BH:DLS + BH:Summer_break + BH:Christmas_break +
                  BH:Time + Summer_break:Time,
                data= subset(fri, select = -c(WeekDays)))
  sat_fit <- lm(Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
                  toy + BH + DLS + Summer_break + Christmas_break +
                  Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
                  Load.1:Temp_s99_max + Load.1:toy + Load.1:DLS +
                  Load.1:Summer_break + Load.1:Christmas_break + Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max + Load.7:DLS + Load.7:Summer_break +
                  Load.7:Christmas_break + Temp:Temp_s95 + Temp:Temp_s99_max +
                  Temp:DLS + Temp:Christmas_break + Temp_s95:Temp_s95_max +
                  Temp_s95:Temp_s99_max + Temp_s95:toy +
                  Temp_s95:DLS + Temp_s95:Summer_break + Temp_s95:Christmas_break +
                  Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                  Temp_s95_max:Christmas_break + Temp_s95_max:Time + Temp_s99_max:toy +
                  Temp_s99_max:BH + Temp_s99_max:DLS +
                  toy:BH + toy:DLS + toy:Summer_break + toy:Christmas_break +
                  BH:DLS + BH:Summer_break + BH:Christmas_break +
                  BH:Time + Summer_break:Time,
                data= subset(sat, select = -c(WeekDays)))
  sun_fit <- lm(Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
                  toy + BH + DLS + Summer_break + Christmas_break +
                  Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
                  Load.1:Temp_s99_max + Load.1:toy + Load.1:DLS +
                  Load.1:Summer_break + Load.1:Christmas_break + Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max + Load.7:DLS + Load.7:Summer_break +
                  Load.7:Christmas_break + Temp:Temp_s95 + Temp:Temp_s99_max +
                  Temp:DLS + Temp:Christmas_break + Temp_s95:Temp_s95_max +
                  Temp_s95:Temp_s99_max + Temp_s95:toy +
                  Temp_s95:DLS + Temp_s95:Summer_break + Temp_s95:Christmas_break +
                  Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                  Temp_s95_max:Christmas_break + Temp_s95_max:Time + Temp_s99_max:toy +
                  Temp_s99_max:BH + Temp_s99_max:DLS +
                  toy:BH + toy:DLS + toy:Summer_break + toy:Christmas_break +
                  BH:DLS + BH:Summer_break + BH:Christmas_break +
                  BH:Time + Summer_break:Time,
                data= subset(sun, select = -c(WeekDays)))
  mon_forecast <- cbind(predict(mon_fit, newdata = covid_period[which(covid_period$WeekDays=="Monday"),]),covid_period[which(covid_period$WeekDays=="Monday"),]$Time)
  tue_forecast <- cbind(predict(tue_fit, newdata = covid_period[which(covid_period$WeekDays=="Tuesday"),]),covid_period[which(covid_period$WeekDays=="Tuesday"),]$Time)
  wed_forecast <- cbind(predict(wed_fit, newdata = covid_period[which(covid_period$WeekDays=="Wednesday"),]),covid_period[which(covid_period$WeekDays=="Wednesday"),]$Time)
  thu_forecast <- cbind(predict(thu_fit, newdata = covid_period[which(covid_period$WeekDays=="Thursday"),]),covid_period[which(covid_period$WeekDays=="Thursday"),]$Time)
  fri_forecast <- cbind(predict(fri_fit, newdata = covid_period[which(covid_period$WeekDays=="Friday"),]),covid_period[which(covid_period$WeekDays=="Friday"),]$Time)
  sat_forecast <- cbind(predict(sat_fit, newdata = covid_period[which(covid_period$WeekDays=="Saturday"),]),covid_period[which(covid_period$WeekDays=="Saturday"),]$Time)
  sun_forecast <- cbind(predict(sun_fit, newdata = covid_period[which(covid_period$WeekDays=="Sunday"),]),covid_period[which(covid_period$WeekDays=="Sunday"),]$Time)
  forecast <- rbind(mon_forecast, tue_forecast, wed_forecast, thu_forecast, fri_forecast, sat_forecast,
                    sun_forecast)
  forecast <- forecast[order(forecast[,2]),1]
  return(forecast)
}




