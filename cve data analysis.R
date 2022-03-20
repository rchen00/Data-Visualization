# library("rjson")
# df2019 <- fromJSON("D:/CSTU/Data Visualization for Machine Learning/Project Submission/nvdcve-1.1-2019.json")
# df2020 <- fromJSON(file = "D:/CSTU/Data Visualization for Machine Learning/Project Submission/nvdcve-1.1-2020.json")
# df2021 <- fromJSON(file = "D:/CSTU/Data Visualization for Machine Learning/Project Submission/nvdcve-1.1-2021.json")
# df2022 <- fromJSON(file = "D:/CSTU/Data Visualization for Machine Learning/Project Submission/nvdcve-1.1-2022.json")


library(jsonlite)
library(tidyverse)
list2019 <- read_json("D:/CSTU/Data Visualization for Machine Learning/Project Submission/nvdcve-1.1-2019.json",simplifyVector = TRUE,flatten=TRUE)
list2020 <- read_json("D:/CSTU/Data Visualization for Machine Learning/Project Submission/nvdcve-1.1-2020.json",simplifyVector = TRUE,flatten=TRUE)
list2021 <- read_json("D:/CSTU/Data Visualization for Machine Learning/Project Submission/nvdcve-1.1-2021.json",simplifyVector = TRUE,flatten=TRUE)
list2022 <- read_json("D:/CSTU/Data Visualization for Machine Learning/Project Submission/nvdcve-1.1-2022.json",simplifyVector = TRUE,flatten=TRUE)

head(list2019)
str(list2019, max.level = 4)



df2019 <- as.data.frame(list2019)
df2020 <- as.data.frame(list2020)
df2021 <- as.data.frame(list2021)
df2022 <- as.data.frame(list2022)


cdf2019 <- df2019[,c(6,11,18:31)]
cdf2020 <- df2020[,c(6,11,18:31)]
cdf2021 <- df2021[,c(6,11,18:31)]
cdf2022 <- df2022[,c(6,11,18:31)]

cdf19_22 <- rbind(cdf2019,cdf2020,cdf2021,cdf2022)

str(cdf19_22)
summary(cdf19_22)
View(cdf19_22)

cdf19_22$Date <- as.Date(cdf19_22$CVE_Items.publishedDate)
cdf19_22$month <- as.Date(cut(cdf19_22$Date, breaks = 'month'))
cdf19_22$YM <- format(as.Date(cdf19_22$Date, format="%d/%m/%Y"),"%Y-%m")

library(scales)
ggplot(data = cdf19_22,
       aes(month, freq=CVE_Items.cve.CVE_data_meta.ID, )) +
  geom_bar() +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "3 month") + # custom x-axis labels
  ggtitle('Number of CVEs per month')+
  xlab('Published Month') +
  ylab('Number of CVEs')
ggsave('D:/CSTU/Data Visualization for Machine Learning/Project Submission/Number of CVEs per month.jpg')

ggplot(data = cdf19_22,
       aes(month, CVE_Items.impact.baseMetricV3.cvssV3.baseScore, group=month)) +
  geom_boxplot() +
  stat_summary(fun = mean)+ # adds up all observations for the month
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "3 month") + # custom x-axis labels
  ggtitle('CVSS3 Base Score')+
  xlab('Published Month') +
  ylab('BaseScore')
ggsave('D:/CSTU/Data Visualization for Machine Learning/Project Submission/CVSS3 Base Score boxplot.jpg')

ggplot(data = cdf19_22,
       aes(x = month, CVE_Items.impact.baseMetricV3.exploitabilityScore, group=month)) +
  geom_boxplot() +
  stat_summary(fun = mean) + # adds up all observations for the month
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "3 month") + # custom x-axis labels
  ggtitle('CVSS3 exploitabilityScore')+
  xlab('Published Month') +
  ylab('exploitabilityScore')
ggsave('D:/CSTU/Data Visualization for Machine Learning/Project Submission/exploitabilityScore.jpg')

ggplot(data = cdf19_22,
       aes(x = month, y = CVE_Items.impact.baseMetricV3.impactScore, group=month)) +
  geom_boxplot() +
  stat_summary(fun = mean) + # adds up all observations for the month
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "3 month") + # custom x-axis labels
  ggtitle('CVSS3 impactScore')+
  xlab('Published Month') +
  ylab('impactScore')
ggsave('D:/CSTU/Data Visualization for Machine Learning/Project Submission/impactScore.jpg')

ggplot(data = cdf19_22,
       aes(month, CVE_Items.impact.baseMetricV3.cvssV3.baseScore, )) +
  stat_summary(fun = mean, # adds up all observations for the month
               geom = "line") + # or "line"
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "3 month") + # custom x-axis labels
  ggtitle('CVSS3 Base Score - monthly mean')+
  xlab('Published Month') +
  ylab('BaseScore')
ggsave('D:/CSTU/Data Visualization for Machine Learning/Project Submission/Base Score mean line chart.jpg')

ggplot(cdf19_22,
       aes(month, freq=CVE_Items.impact.baseMetricV3.cvssV3.attackComplexity, fill = CVE_Items.impact.baseMetricV3.cvssV3.attackComplexity, )) +
  geom_bar(position = 'dodge') +
  scale_fill_manual(values=c("blue",
                             "red",
                             "gray")) +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "3 month") + # custom x-axis labels
  labs(fill='attackComplexity') +
  ggtitle('CVSS3 attackComplexity')+
  xlab('Published Month') +
  ylab('attackComplexity count') 
ggsave('D:/CSTU/Data Visualization for Machine Learning/Project Submission/attackComplexity.jpg')

ggplot(cdf19_22,
       aes(month, freq=CVE_Items.impact.baseMetricV3.cvssV3.privilegesRequired, fill = CVE_Items.impact.baseMetricV3.cvssV3.privilegesRequired, )) +
  geom_bar(position = 'dodge') +
  scale_fill_manual(values=c("blue",
                             'orange',
                             "red",
                             "gray")) +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "3 month") + # custom x-axis labels
  labs(fill='privilegesRequired') +
  ggtitle('CVSS3 privilegesRequired')+
  xlab('Published Month') +
  ylab('privilegesRequired count') 
ggsave('D:/CSTU/Data Visualization for Machine Learning/Project Submission/privilegesRequired.jpg')

ggplot(cdf19_22,
       aes(month, freq=CVE_Items.impact.baseMetricV3.cvssV3.userInteraction, fill = CVE_Items.impact.baseMetricV3.cvssV3.userInteraction, )) +
  geom_bar(position = 'dodge') +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "3 month") + # custom x-axis labels
  labs(fill='userInteraction') +
  ggtitle('CVSS3 userInteraction')+
  xlab('Published Month') +
  ylab('userInteraction count') 
ggsave('D:/CSTU/Data Visualization for Machine Learning/Project Submission/userInteraction.jpg')

ggplot(cdf19_22,
       aes(month, freq=CVE_Items.impact.baseMetricV3.cvssV3.confidentialityImpact, fill = CVE_Items.impact.baseMetricV3.cvssV3.confidentialityImpact, )) +
  geom_bar(position = 'dodge') +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "3 month") + # custom x-axis labels
  labs(fill='confidentialityImpact') +
  ggtitle('CVSS3 confidentialityImpact')+
  xlab('Published Month') +
  ylab('confidentialityImpact count') 
ggsave('D:/CSTU/Data Visualization for Machine Learning/Project Submission/confidentialityImpact.jpg')

ggplot(cdf19_22,
       aes(month, freq=CVE_Items.impact.baseMetricV3.cvssV3.integrityImpact, fill = CVE_Items.impact.baseMetricV3.cvssV3.integrityImpact, )) +
  geom_bar(position = 'dodge') +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "3 month") + # custom x-axis labels
  labs(fill='integrityImpact') +
  ggtitle('CVSS3 integrityImpact')+
  xlab('Published Month') +
  ylab('integrityImpact count') 
ggsave('D:/CSTU/Data Visualization for Machine Learning/Project Submission/integrityImpact.jpg')

ggplot(cdf19_22,
       aes(month, freq=CVE_Items.impact.baseMetricV3.cvssV3.availabilityImpact, fill = CVE_Items.impact.baseMetricV3.cvssV3.availabilityImpact, )) +
  geom_bar(position = 'dodge') +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "3 month") + # custom x-axis labels
  labs(fill='availabilityImpact') +
  ggtitle('CVSS3 availabilityImpact')+
  xlab('Published Month') +
  ylab('availabilityImpact count')
ggsave('D:/CSTU/Data Visualization for Machine Learning/Project Submission/availabilityImpact.jpg')

ggplot(cdf19_22,
       aes(month, freq=CVE_Items.impact.baseMetricV3.cvssV3.privilegesRequired, fill = CVE_Items.impact.baseMetricV3.cvssV3.privilegesRequired, group = CVE_Items.impact.baseMetricV3.cvssV3.privilegesRequired)) +
  geom_bar(position = 'dodge') +
 #   geom_density() +
  #  geom_histogram() +
  #  geom_smooth() +
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "3 month") + # custom x-axis labels
  labs(fill='privilegesRequired') +
  ggtitle('CVSS3 privilegesRequired')+
  xlab('Published Month') +
  ylab('privilegesRequired') 

