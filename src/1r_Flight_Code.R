library(dplyr)
library(ggplot2)

#Plot Function--------------------------------------------------
generate_plot_case <- function(df, output) {
  cerulean_blue <- "#007BA7"
  minlogplot <- 0.4
  df[df == 0] <- minlogplot
  df <- df[order(df$casesbeforeimport.upper),]
  df <- df[1:100,]
  df <- df %>%
    mutate(Lower = casesbeforeimport.lower,
           Median = casesbeforeimport.median - casesbeforeimport.lower,
           Upper = casesbeforeimport.upper - casesbeforeimport.median,
           ymin_lower = minlogplot,
           ymax_lower = Lower,
           ymin_median = Lower,
           ymax_median = Lower + Median,
           ymin_upper = Lower + Median,
           ymax_upper = Lower + Median + Upper) %>%
    mutate(country = factor(country, levels = country[order(ymax_upper)]))
  df$X <- seq(1,100,1)
  plot_stack_layer <- function(df, ymin_col, ymax_col, color, fill) {
    geom_rect(aes(xmin = X - 0.3, xmax = X + 0.3,
                  ymin = !!sym(ymin_col), ymax = !!sym(ymax_col)),
              color = color, fill = fill, linewidth = 0.3)
  }

  p <- ggplot(df) +
    scale_y_log10(breaks = c(minlogplot, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500,
                             600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000,
                             8000, 9000, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000),
                  limits = c(minlogplot, 100000),
                  labels = c("0", "1", "", "", "", "", "", "", "", "", "10", "", "", "", "", "", "", "", "", "100",
                             "", "", "", "", "", "", "", "", "1000",
                             "", "", "", "", "", "", "", "", "10 000",
                             "", "", "", "", "", "", "", "", "100 000"),
                  expand = c(0, 0)) +
    scale_x_continuous(breaks = 1:length(unique(df$country)), labels = unique(df$country), expand = c(0, 0.2)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 12),
          axis.ticks.y = element_line(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "black"),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none") +
    plot_stack_layer(df, "ymin_lower", "ymax_lower", color = "white",
                     fill = ifelse(df$country %in% c("Sweden", "Thailand"), NA, "white")) +
    plot_stack_layer(df, "ymin_median", "ymax_median", color = "navy",
                     fill = ifelse(df$country %in% c("Sweden", "Thailand"), cerulean_blue, "white")) +
    plot_stack_layer(df, "ymin_upper", "ymax_upper", color = "navy",
                     fill = ifelse(df$country %in% c("Sweden", "Thailand"), cerulean_blue, "white"))
  p
  ggsave(out, plot = p, width = 9000, height = 3000, units = "px", dpi = 600)
}
generate_plot_country <- function(df, output) {
  df <- df[order(df$casesbeforeimport.upper),]
  df <- df[1:100,]
  df$X = seq(1,100,1)
  df <- df %>%
    mutate(Lower = countriesbeforeimport.lower,
           Median = countriesbeforeimport.median - countriesbeforeimport.lower,
           Upper = countriesbeforeimport.upper - countriesbeforeimport.median,
           ymin_lower = 0,
           ymax_lower = Lower,
           ymin_median = Lower,
           ymax_median = Lower + Median,
           ymin_upper = Lower + Median,
           ymax_upper = Lower + Median + Upper) %>%
    mutate(country = factor(country, levels = country[order(ymax_upper)]))

  plot_stack_layer <- function(df, ymin_col, ymax_col, color, fill) {
    geom_rect(aes(xmin = X - 0.3, xmax = X + 0.3,
                  ymin = !!sym(ymin_col), ymax = !!sym(ymax_col)),
              color = color, fill = fill, linewidth = 0.3)
  }

  p <- ggplot(df) +
    scale_x_continuous(breaks = 1:length(unique(df$country)), labels = unique(df$country), expand = c(0, 0.2)) +
    scale_y_continuous(breaks = seq(0, 170, by = 10), limits = c(0,170),expand = c(0, 0)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 12),
          axis.ticks.y = element_line(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "black"),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none") +
    plot_stack_layer(df, "ymin_lower", "ymax_lower", color = "white",
                     fill = ifelse(df$country %in% c("Sweden", "Thailand"), NA, "white")) +
    plot_stack_layer(df, "ymin_median", "ymax_median", color = "darkorange3",
                     fill = ifelse(df$country %in% c("Sweden", "Thailand"), "tan1", "white")) +
    plot_stack_layer(df, "ymin_upper", "ymax_upper", color = "darkorange3",
                     fill = ifelse(df$country %in% c("Sweden", "Thailand"), "tan1", "white"))
  p
  ggsave(output, plot = p, width = 9000, height = 3000, units = "px", dpi = 600)
}

#DRC Scenario 1---------------------------------------------------------------------------
raw = read.csv("./dt_sim_data/Scenario.csv")

data = data.frame(country=raw$Country,volume=raw$Volume)
data = data[data$volume>0,]

totalvolume = sum(data$volume)
data$prob=data$volume/totalvolume

size = 10000
result= t(sapply(1:nrow(data),function(r){
  cases_quantile = qgeom(c(0.025,0.5,0.975),data$prob[r])
  casesbeforeimport = rgeom(size,data$prob[r])
  othercountryprobs = data$prob[-r]
  countriesbeforeimport = sapply(casesbeforeimport,function(cases){
    sum(rmultinom(1, cases, othercountryprobs)>0)
  })
  countries_quantile = quantile(countriesbeforeimport,c(0.025,0.5,0.975),type=1)
  c(cases_quantile,countries_quantile)
}))
colnames(result)=c(paste("casesbeforeimport", c("lower", "median", "upper")),paste("countriesbeforeimport", c("lower", "median", "upper")))
data=cbind(data[,1:3], data.frame(result))
write.csv(data,"./res/results1.csv")
out = "./res/Cases_S1.png"
generate_plot_case(data,out)
out = "./res/Countries_S1.png"
generate_plot_country(data,out)

#DRC+Burundi Scenario 2------------------------------------------------------------------
raw = read.csv("./dt_flight/Scenario2.csv")

data = data.frame(country=raw$Country,volume=raw$Volume)
data = data[data$volume>0,]

totalvolume = sum(data$volume)
data$prob=data$volume/totalvolume

size = 10000
result= t(sapply(1:nrow(data),function(r){
  cases_quantile = qgeom(c(0.025,0.5,0.975),data$prob[r])
  casesbeforeimport = rgeom(size,data$prob[r])
  othercountryprobs = data$prob[-r]
  countriesbeforeimport = sapply(casesbeforeimport,function(cases){
    sum(rmultinom(1, cases, othercountryprobs)>0)
  })
  countries_quantile = quantile(countriesbeforeimport,c(0.025,0.5,0.975),type=1)
  c(cases_quantile,countries_quantile)
}))
colnames(result)=c(paste("casesbeforeimport", c("lower", "median", "upper")),paste("countriesbeforeimport", c("lower", "median", "upper")))

data=cbind(data[,1:3], data.frame(result))
write.csv(data,"./res/results2.csv")
out = "./res/Cases_S2.png"
generate_plot_case(data,out)
out = "./res/Countries_S2.png"
generate_plot_country(data,out)

#DRC+Burundi+Uganda+Kenya+Rwanda Scenario 3----------------------------------------------------
raw = read.csv("./dt_flight/Scenario3.csv")

data = data.frame(country=raw$Country,volume=raw$Volume)
data = data[data$volume>0,]

totalvolume = sum(data$volume)
data$prob=data$volume/totalvolume

size = 10000
result= t(sapply(1:nrow(data),function(r){
  cases_quantile = qgeom(c(0.025,0.5,0.975),data$prob[r])
  casesbeforeimport = rgeom(size,data$prob[r])
  othercountryprobs = data$prob[-r]
  countriesbeforeimport = sapply(casesbeforeimport,function(cases){
    sum(rmultinom(1, cases, othercountryprobs)>0)
  })
  countries_quantile = quantile(countriesbeforeimport,c(0.025,0.5,0.975),type=1)
  c(cases_quantile,countries_quantile)
}))
colnames(result)=c(paste("casesbeforeimport", c("lower", "median", "upper")),paste("countriesbeforeimport", c("lower", "median", "upper")))

data=cbind(data[,1:3], data.frame(result))
write.csv(data,"./res/results3.csv")
out = "./res/Cases_S3.png"
generate_plot_case(data,out)
out = "./res/Countries_S3.png"
generate_plot_country(data,out)

