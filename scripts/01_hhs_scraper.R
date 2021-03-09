library(tidyverse)
library(rvest)
library(lubridate)

## code to scrape various HHS and CBP websites for data

#### FY 2021 ----

url <- "https://www.hhs.gov/programs/social-services/unaccompanied-children/latest-uc-data-fy2021/index.html?language=es"

df <- read_html(url) %>% 
  html_nodes("td , .color2 strong , th") %>% 
  html_text() %>% 
  data.frame()

colnames(df) <- "txt"

df$copy <- NA

for (i in 1:nrow(df)) {
  if (i!=1) {
    if (df$txt[i]==df$txt[i-1]) {
      df$copy[i] <- T
    } else {
      df$copy[i] <- F
    }
    
    if (grepl("\\d", df$txt[i])) {
      df$copy[i] <- F
    }
    
    if (df$txt[i]=="30-Day Average Number of UC Referrals:" &
        df$txt[i-1]=="30-Day Average Number of UC Referrals:") {
      df$copy[i] <- T
    }
    if (df$txt[i]=="30-Day Average UC Discharge Rate:" &
        df$txt[i-1]=="30-Day Average UC Discharge Rate:") {
      df$copy[i] <- T
    }
    
    
    if (df$txt[i]=="0-5" &
        df$txt[i-1]=="0-5") {
      df$copy[i] <- T
    }
    
    
    
    if (df$txt[i]=="6-12" &
        df$txt[i-1]=="6-12") {
      df$copy[i] <- T
    }
    
    
    
    if (df$txt[i]=="13-14" &
        df$txt[i-1]=="13-14") {
      df$copy[i] <- T
    }
    
    if (df$txt[i]=="15-16" &
        df$txt[i-1]=="15-16") {
      df$copy[i] <- T
    }
    
    if (df$txt[i]=="17+" &
        df$txt[i-1]=="17+") {
      df$copy[i] <- T
    }
    
    
    if (grepl("Category 1", df$txt[i]) &
        grepl("Category 1", df$txt[i-1])) {
      df$copy[i] <- T
    }
    
    if (grepl("Category 2", df$txt[i]) &
        grepl("Category 2", df$txt[i-1])) {
      df$copy[i] <- T
    }
    
    if (grepl("Category 3", df$txt[i]) &
        grepl("Category 3", df$txt[i-1])) {
      df$copy[i] <- T
    }
  }
}

df$copy <- ifelse(is.na(df$copy), F, df$copy)
df <- filter(df, copy!=T)
df <- df %>% 
  mutate(id = row_number())


df$copy <- ifelse(is.na(df$copy), F, df$copy)
df <- filter(df, copy!=T)
df <- df %>% 
  mutate(id = row_number())

df_len <- nrow(df)
df_wid <- 5
category <- seq(1, df_len, by=df_wid)
oct <- seq(2, df_len, by=df_wid)
nov <- seq(3, df_len, by=df_wid)
dec <- seq(4, df_len, by=df_wid)
jan<- seq(5, df_len, by=df_wid)

category <- filter(df,
                id %in% category) %>% 
  select(-copy, -id)
oct <- filter(df,
              id %in% oct)  %>% 
  select(-copy, -id)
nov <- filter(df,
              id %in% nov) %>% 
  select(-copy, -id)
dec <- filter(df,
              id %in% dec) %>% 
  select(-copy, -id)
jan <- filter(df,
              id %in% jan) %>% 
  select(-copy, -id)

fy2021 <- data.frame(category, oct, nov, dec, jan)
colnames(fy2021) <- c("category", "oct", "nov", "dec", "jan")


fy2021 <- filter(fy2021,
                 category!="In Care Statistics Calculated on the Last Day of the Month") 

fy2021$group <- NA

for (i in 1:nrow(fy2021)) {
  
  if (i==1) {
    group <- "Overall Data"
  }
  
  if (i==8) {
    group <- "Gender"
  }
  
  if (i==10) {
    group <- "Age"
  }
  
  if (i==15) {
    group <- "Tender Age (0-12) UC By Shelter Type"
  }
  
  if (i==22) {
    group <- "Total Monthly Discharges to Individual Sponsors Only (By Category)"
  }
  fy2021$group[i] <- group
  
}

#fixing_top <- fy2021[1:16,]
#fixing_bot <- fy2021[19:23,]
#fixing_mid <- data.frame(
#  category=c("Staff Secure", "Secure", "Therapeutic"),
#   oct=c(0,0,0),
#   nov=c(0,0,0),
#   dec=c(0,0,0),
#   group=c("Tender Age (0-12) UC By Shelter Type",
#           "Tender Age (0-12) UC By Shelter Type",
#           "Tender Age (0-12) UC By Shelter Type"))
# fy2021 <- rbind(fixing_top, fixing_bot, fixing_mid)

fy2021 <- fy2021 %>% 
  pivot_longer(cols="oct":"jan",
               names_to="month",
               values_to="value") %>% 
  select(group, category, month, value) %>% 
  mutate(number_type=case_when(
    grepl("%", value) ~ "Percent",
    TRUE ~ "Number"
  ))

fy2021 <- fy2021 %>% 
  mutate(value=gsub(",", "", value),
         value=gsub("%", "", value)) %>% 
  mutate(value=as.numeric(value)) %>% 
  mutate(month=case_when(
    month=="oct" ~ "10/1/20",
    month=="nov" ~ "11/1/20",
    month=="dec" ~ "12/1/20",
    month=="jan" ~ "01/1/21"
  )) %>% 
  mutate(month=mdy(month))


#### FY 2020 ----



url <- "https://www.hhs.gov/programs/social-services/unaccompanied-children/latest-uc-data-fy2020/index.html"
df <- read_html(url) %>% 
  html_nodes("td , .color2 strong , th") %>% 
  html_text() %>% 
  data.frame()


colnames(df) <- "txt"

df$copy <- NA

for (i in 1:nrow(df)) {
  if (i!=1) {
    if (df$txt[i]==df$txt[i-1]) {
      df$copy[i] <- T
    } else {
      df$copy[i] <- F
    }
    
    if (grepl("\\d", df$txt[i])) {
      df$copy[i] <- F
    }
    
    if (df$txt[i]=="30-Day Average Number of UC Referrals:" &
        df$txt[i-1]=="30-Day Average Number of UC Referrals:") {
      df$copy[i] <- T
    }
    if (df$txt[i]=="30-Day Average UC Discharge Rate:" &
        df$txt[i-1]=="30-Day Average UC Discharge Rate:") {
      df$copy[i] <- T
    }
    
    
    if (df$txt[i]=="0-5" &
        df$txt[i-1]=="0-5") {
      df$copy[i] <- T
    }
    
    
    
    if (df$txt[i]=="6-12" &
        df$txt[i-1]=="6-12") {
      df$copy[i] <- T
    }
    
    
    
    if (df$txt[i]=="13-14" &
        df$txt[i-1]=="13-14") {
      df$copy[i] <- T
    }
    
    if (df$txt[i]=="15-16" &
        df$txt[i-1]=="15-16") {
      df$copy[i] <- T
    }
    
    if (df$txt[i]=="17+" &
        df$txt[i-1]=="17+") {
      df$copy[i] <- T
    }
    
    
    if (grepl("Category 1", df$txt[i]) &
        grepl("Category 1", df$txt[i-1])) {
      df$copy[i] <- T
    }
    
    if (grepl("Category 2", df$txt[i]) &
        grepl("Category 2", df$txt[i-1])) {
      df$copy[i] <- T
    }
    
    if (grepl("Category 3", df$txt[i]) &
        grepl("Category 3", df$txt[i-1])) {
      df$copy[i] <- T
    }
  }
}

df$copy <- ifelse(is.na(df$copy), F, df$copy)
df <- filter(df, copy!=T)
df <- df %>% 
  mutate(id = row_number())

category <- seq(1, 390, by=13)
oct <- seq(2, 390, by=13)
nov <- seq(3, 390, by=13)
dec <- seq(4, 390, by=13)
jan <- seq(5, 390, by=13)
feb <- seq(6, 390, by=13)
mar <- seq(7, 390, by=13)
apr <- seq(8, 390, by=13)
may <- seq(9, 390, by=13)
jun <- seq(10, 390, by=13)
jul <- seq(11, 390, by=13)
aug <- seq(12, 390, by=13)
sep <- seq(13, 390, by=13)

category <- filter(df,
                   id %in% category) %>% 
  select(-copy, -id)
oct <- filter(df,
              id %in% oct)  %>% 
  select(-copy, -id)
nov <- filter(df,
              id %in% nov) %>% 
  select(-copy, -id)
dec <- filter(df,
              id %in% dec) %>% 
  select(-copy, -id)
jan <- filter(df,
              id %in% jan) %>% 
  select(-copy, -id)
feb <- filter(df,
              id %in% feb) %>% 
  select(-copy, -id)
mar <- filter(df,
              id %in% mar) %>% 
  select(-copy, -id)
apr <- filter(df,
              id %in% apr) %>% 
  select(-copy, -id)
may <- filter(df,
              id %in% may) %>% 
  select(-copy, -id)
jun <- filter(df,
              id %in% jun) %>% 
  select(-copy, -id)
jul <- filter(df,
              id %in% jul) %>% 
  select(-copy, -id)
aug <- filter(df,
              id %in% aug) %>% 
  select(-copy, -id)
sep <- filter(df,
              id %in% sep) %>% 
  select(-copy, -id)


fy2020 <- data.frame(category, oct, nov, dec,
                     jan, feb, mar, apr, may,
                     jun, jul, aug, sep)
colnames(fy2020) <- c("category", "oct", "nov", "dec",
                      "jan", "feb", "mar", "apr", "may",
                      "jun", "jul", "aug", "sep")


fy2020 <- filter(fy2020,
                 category!="In Care Statistics Calculated on the Last Day of the Month") 

fy2020$group <- NA

for (i in 1:nrow(fy2020)) {
  
  if (i==1) {
    group <- "Overall Data"
  }
  
  if (i==8) {
    group <- "Gender"
  }
  
  if (i==10) {
    group <- "Age"
  }
  
  if (i==15) {
    group <- "Tender Age (0-12) UC By Shelter Type"
  }
  
  if (i==22) {
    group <- "Total Monthly Discharges to Individual Sponsors Only (By Category)"
  }
  fy2020$group[i] <- group
  
}


fy2020 <- fy2020 %>% 
  pivot_longer(cols="oct":"sep",
               names_to="month",
               values_to="value") %>% 
  select(group, category, month, value) %>% 
  mutate(number_type=case_when(
    grepl("%", value) ~ "Percent",
    TRUE ~ "Number"
  ))

fy2020 <- fy2020 %>% 
  mutate(value=gsub(",", "", value),
         value=gsub("%", "", value)) %>% 
  mutate(value=as.numeric(value)) %>% 
  mutate(month=case_when(
    month=="oct" ~ "10/1/19",
    month=="nov" ~ "11/1/19",
    month=="dec" ~ "12/1/19",
    month=="jan" ~ "1/1/20",
    month=="feb" ~ "2/1/20",
    month=="mar" ~ "3/1/20",
    month=="apr" ~ "4/1/20",
    month=="may" ~ "5/1/20",
    month=="jun" ~ "6/1/20",
    month=="jul" ~ "7/1/20",
    month=="aug" ~ "8/1/20",
    month=="sep" ~ "9/1/20",
  )) %>% 
  mutate(month=mdy(month))

#### FY 2019 ----



url <- "https://www.hhs.gov/programs/social-services/unaccompanied-children/latest-uc-data-fy2019/index.html"
df <- read_html(url) %>% 
  html_nodes("td , .color2 strong , th") %>% 
  html_text() %>% 
  data.frame()


colnames(df) <- "txt"

df$copy <- NA

for (i in 1:nrow(df)) {
  if (i!=1) {
    if (df$txt[i]==df$txt[i-1]) {
      df$copy[i] <- T
    } else {
      df$copy[i] <- F
    }
    
    if (grepl("\\d", df$txt[i])) {
      df$copy[i] <- F
    }
    
    if (df$txt[i]=="30-Day Average Number of UC Referrals:" &
        df$txt[i-1]=="30-Day Average Number of UC Referrals:") {
      df$copy[i] <- T
    }
    if (df$txt[i]=="30-Day Average UC Discharge Rate:" &
        df$txt[i-1]=="30-Day Average UC Discharge Rate:") {
      df$copy[i] <- T
    }
    
    
    if (df$txt[i]=="0-5" &
        df$txt[i-1]=="0-5") {
      df$copy[i] <- T
    }
    
    
    
    if (df$txt[i]=="6-12" &
        df$txt[i-1]=="6-12") {
      df$copy[i] <- T
    }
    
    
    
    if (df$txt[i]=="13-14" &
        df$txt[i-1]=="13-14") {
      df$copy[i] <- T
    }
    
    if (df$txt[i]=="15-16" &
        df$txt[i-1]=="15-16") {
      df$copy[i] <- T
    }
    
    if (df$txt[i]=="17" &
        df$txt[i-1]=="17") {
      df$copy[i] <- T
    }
    
    
    if (grepl("Category 1", df$txt[i]) &
        grepl("Category 1", df$txt[i-1])) {
      df$copy[i] <- T
    }
    
    if (grepl("Category 2", df$txt[i]) &
        grepl("Category 2", df$txt[i-1])) {
      df$copy[i] <- T
    }
    
    if (grepl("Category 3", df$txt[i]) &
        grepl("Category 3", df$txt[i-1])) {
      df$copy[i] <- T
    }
  }
}

df$copy <- ifelse(is.na(df$copy), F, df$copy)
df <- filter(df, copy!=T)
df <- df %>% 
  mutate(id = row_number())

category <- seq(1, 390, by=13)
oct <- seq(2, 390, by=13)
nov <- seq(3, 390, by=13)
dec <- seq(4, 390, by=13)
jan <- seq(5, 390, by=13)
feb <- seq(6, 390, by=13)
mar <- seq(7, 390, by=13)
apr <- seq(8, 390, by=13)
may <- seq(9, 390, by=13)
jun <- seq(10, 390, by=13)
jul <- seq(11, 390, by=13)
aug <- seq(12, 390, by=13)
sep <- seq(13, 390, by=13)

category <- filter(df,
                   id %in% category) %>% 
  select(-copy, -id)
oct <- filter(df,
              id %in% oct)  %>% 
  select(-copy, -id)
nov <- filter(df,
              id %in% nov) %>% 
  select(-copy, -id)
dec <- filter(df,
              id %in% dec) %>% 
  select(-copy, -id)
jan <- filter(df,
              id %in% jan) %>% 
  select(-copy, -id)
feb <- filter(df,
              id %in% feb) %>% 
  select(-copy, -id)
mar <- filter(df,
              id %in% mar) %>% 
  select(-copy, -id)
apr <- filter(df,
              id %in% apr) %>% 
  select(-copy, -id)
may <- filter(df,
              id %in% may) %>% 
  select(-copy, -id)
jun <- filter(df,
              id %in% jun) %>% 
  select(-copy, -id)
jul <- filter(df,
              id %in% jul) %>% 
  select(-copy, -id)
aug <- filter(df,
              id %in% aug) %>% 
  select(-copy, -id)
sep <- filter(df,
              id %in% sep) %>% 
  select(-copy, -id)


fy2019 <- data.frame(category, oct, nov, dec,
                     jan, feb, mar, apr, may,
                     jun, jul, aug, sep)
colnames(fy2019) <- c("category", "oct", "nov", "dec",
                      "jan", "feb", "mar", "apr", "may",
                      "jun", "jul", "aug", "sep")


fy2019 <- filter(fy2019,
                 !grepl("In Care Statistics Calculated on the Last Day of the Month", category))

fy2019$group <- NA

for (i in 1:nrow(fy2019)) {
  
  if (i==1) {
    group <- "Overall Data"
  }
  
  if (i==8) {
    group <- "Gender"
  }
  
  if (i==10) {
    group <- "Age"
  }
  
  if (i==15) {
    group <- "Tender Age (0-12) UC By Shelter Type"
  }
  
  if (i==22) {
    group <- "Total Monthly Discharges to Individual Sponsors Only (By Category)"
  }
  fy2019$group[i] <- group
  
}


fy2019 <- fy2019 %>% 
  pivot_longer(cols="oct":"sep",
               names_to="month",
               values_to="value") %>% 
  select(group, category, month, value) %>% 
  mutate(number_type=case_when(
    grepl("%", value) ~ "Percent",
    TRUE ~ "Number"
  ))

fy2019 <- fy2019 %>% 
  mutate(value=gsub(",", "", value),
         value=gsub("%", "", value)) %>% 
  mutate(value=as.numeric(value)) %>% 
  mutate(month=case_when(
    month=="oct" ~ "10/1/18",
    month=="nov" ~ "11/1/18",
    month=="dec" ~ "12/1/18",
    month=="jan" ~ "1/1/19",
    month=="feb" ~ "2/1/19",
    month=="mar" ~ "3/1/19",
    month=="apr" ~ "4/1/19",
    month=="may" ~ "5/1/19",
    month=="jun" ~ "6/1/19",
    month=="jul" ~ "7/1/19",
    month=="aug" ~ "8/1/19",
    month=="sep" ~ "9/1/19",
  )) %>% 
  mutate(month=mdy(month))



#### FY 2018 ----



url <- "https://www.hhs.gov/programs/social-services/unaccompanied-children/latest-uc-data-fy2018/index.html"
df <- read_html(url) %>% 
  html_nodes("td , .color2 strong , th") %>% 
  html_text() %>% 
  data.frame()


colnames(df) <- "txt"

df$copy <- NA

for (i in 1:nrow(df)) {
  if (i!=1) {
    if (df$txt[i]==df$txt[i-1]) {
      df$copy[i] <- T
    } else {
      df$copy[i] <- F
    }
    
    if (grepl("\\d", df$txt[i])) {
      df$copy[i] <- F
    }
    
    if (df$txt[i]=="30-Day Average Number of UC Referrals:" &
        df$txt[i-1]=="30-Day Average Number of UC Referrals:") {
      df$copy[i] <- T
    }
    if (df$txt[i]=="30-Day Average UC Discharge Rate:" &
        df$txt[i-1]=="30-Day Average UC Discharge Rate:") {
      df$copy[i] <- T
    }
    
    
    if (df$txt[i]=="0-5" &
        df$txt[i-1]=="0-5") {
      df$copy[i] <- T
    }
    
    
    
    if (df$txt[i]=="6-12" &
        df$txt[i-1]=="6-12") {
      df$copy[i] <- T
    }
    
    
    
    if (df$txt[i]=="13-14" &
        df$txt[i-1]=="13-14") {
      df$copy[i] <- T
    }
    
    if (df$txt[i]=="15-16" &
        df$txt[i-1]=="15-16") {
      df$copy[i] <- T
    }
    
    if (df$txt[i]=="17" &
        df$txt[i-1]=="17") {
      df$copy[i] <- T
    }
    
    
    if (grepl("Category 1", df$txt[i]) &
        grepl("Category 1", df$txt[i-1])) {
      df$copy[i] <- T
    }
    
    if (grepl("Category 2", df$txt[i]) &
        grepl("Category 2", df$txt[i-1])) {
      df$copy[i] <- T
    }
    
    if (grepl("Category 3", df$txt[i]) &
        grepl("Category 3", df$txt[i-1])) {
      df$copy[i] <- T
    }
  }
}

df$copy <- ifelse(is.na(df$copy), F, df$copy)
df <- filter(df, copy!=T)
df <- df %>% 
  mutate(id = row_number())

category <- seq(1, 390, by=13)
oct <- seq(2, 390, by=13)
nov <- seq(3, 390, by=13)
dec <- seq(4, 390, by=13)
jan <- seq(5, 390, by=13)
feb <- seq(6, 390, by=13)
mar <- seq(7, 390, by=13)
apr <- seq(8, 390, by=13)
may <- seq(9, 390, by=13)
jun <- seq(10, 390, by=13)
jul <- seq(11, 390, by=13)
aug <- seq(12, 390, by=13)
sep <- seq(13, 390, by=13)

category <- filter(df,
                   id %in% category) %>% 
  select(-copy, -id)
oct <- filter(df,
              id %in% oct)  %>% 
  select(-copy, -id)
nov <- filter(df,
              id %in% nov) %>% 
  select(-copy, -id)
dec <- filter(df,
              id %in% dec) %>% 
  select(-copy, -id)
jan <- filter(df,
              id %in% jan) %>% 
  select(-copy, -id)
feb <- filter(df,
              id %in% feb) %>% 
  select(-copy, -id)
mar <- filter(df,
              id %in% mar) %>% 
  select(-copy, -id)
apr <- filter(df,
              id %in% apr) %>% 
  select(-copy, -id)
may <- filter(df,
              id %in% may) %>% 
  select(-copy, -id)
jun <- filter(df,
              id %in% jun) %>% 
  select(-copy, -id)
jul <- filter(df,
              id %in% jul) %>% 
  select(-copy, -id)
aug <- filter(df,
              id %in% aug) %>% 
  select(-copy, -id)
sep <- filter(df,
              id %in% sep) %>% 
  select(-copy, -id)


fy2018 <- data.frame(category, oct, nov, dec,
                     jan, feb, mar, apr, may,
                     jun, jul, aug, sep)
colnames(fy2018) <- c("category", "oct", "nov", "dec",
                      "jan", "feb", "mar", "apr", "may",
                      "jun", "jul", "aug", "sep")


fy2018 <- filter(fy2018,
                 !grepl("In Care Statistics Calculated on the Last Day of the Month", category))

fy2018$group <- NA

for (i in 1:nrow(fy2018)) {
  
  if (i==1) {
    group <- "Overall Data"
  }
  
  if (i==8) {
    group <- "Gender"
  }
  
  if (i==10) {
    group <- "Age"
  }
  
  if (i==15) {
    group <- "Tender Age (0-12) UC By Shelter Type"
  }
  
  if (i==22) {
    group <- "Total Monthly Discharges to Individual Sponsors Only (By Category)"
  }
  fy2018$group[i] <- group
  
}


fy2018 <- fy2018 %>% 
  pivot_longer(cols="oct":"sep",
               names_to="month",
               values_to="value") %>% 
  select(group, category, month, value) %>% 
  mutate(number_type=case_when(
    grepl("%", value) ~ "Percent",
    TRUE ~ "Number"
  ))

fy2018 <- fy2018 %>% 
  mutate(value=gsub(",", "", value),
         value=gsub("%", "", value)) %>% 
  mutate(value=as.numeric(value)) %>% 
  mutate(month=case_when(
    month=="oct" ~ "10/1/17",
    month=="nov" ~ "11/1/17",
    month=="dec" ~ "12/1/17",
    month=="jan" ~ "1/1/18",
    month=="feb" ~ "2/1/18",
    month=="mar" ~ "3/1/18",
    month=="apr" ~ "4/1/18",
    month=="may" ~ "5/1/18",
    month=="jun" ~ "6/1/18",
    month=="jul" ~ "7/1/18",
    month=="aug" ~ "8/1/18",
    month=="sep" ~ "9/1/18",
  )) %>% 
  mutate(month=mdy(month))


#### combine ----

fy <- rbind(fy2021, fy2020, fy2019, fy2018)
write_csv(fy, "output_data/fy2018_fy2021.csv")

#### Unaccompanied Alien Children Released to Sponsors By State ----

url <- "https://www.acf.hhs.gov/orr/grant-funding/unaccompanied-alien-children-released-sponsors-state"

df <- read_html(url) %>% 
  html_table()

df <- df[1] %>% data.frame()

colnames(df) <- c("state", "fy15", "fy16", "fy17", "fy18", "fy19",
                  "fy20", "fy21_so_far") 
df <- pivot_longer(df, cols=2:8, names_to="year", values_to="values")

df <- df %>% 
  mutate(values=gsub(",", "", values)) %>% 
  mutate(values=as.numeric(values))

df <- df %>% 
  pivot_wider(names_from="year", values_from="values")

df <- df %>% 
  mutate(state=gsub("DC", "District of Columbia", state))

write_csv(df, "output_data/states_children.csv", na="")


#### top counties ----


url <- "https://www.acf.hhs.gov/orr/grant-funding/unaccompanied-alien-children-released-sponsors-county"


df <- read_html(url) %>% 
  html_table()

df <- df[1] %>% data.frame()

colnames(df) <- c("state", "county", "fy21_so_far")
write_csv(df, "output_data/county_50_fy21.csv", na="")

#### U.S. Border Patrol Southwest Border Apprehensions by Sector Fiscal Year ----

absfy<- function(x, a="fy18", b="fy19", c="pc1819", type="type") {
  df <- x
  df <- df[2:nrow(df),]
  colnames(df) <- c("sector", a, b, c)
  df$type <- type
  return(df)
}

sbecfy <- function(x, a="fy14", b="fy15", c="fy16",
                 d = "fy17", e="fy18", f="fy19", g="fy20", type="type") {
  df <- x
  df <- df[2:nrow(df),]
  if(ncol(df)==5) {
    colnames(df) <- c("sector", a, b, c, d)
  } else if (ncol(df)==6) {
    colnames(df) <- c("sector", a, b, c, d, e)
  } else if (ncol(df)==7) {
  colnames(df) <- c("sector", a, b, c, d, e, f)
  } else if (ncol(df)==8) {
    colnames(df) <- c("sector", a, b, c, d, e, f, g)
  }
  df$type <- type
  return(df)
}

secmon <- function(x, month="Jan", year=2019) {
  df <- x
  df <- df[2:nrow(df),]
  colnames(df) <- c("sector", "fmua", "uac", "sa", "total")
  df$month <- month
  df$year <- year
  return(df)
}

# 2019
url <- "https://www.cbp.gov/newsroom/stats/sw-border-migration/usbp-sw-border-apprehensions-fy2019"

df <- read_html(url) %>% 
  html_table()

df1 <- df[1] %>% data.frame()
df1 <- absfy(df1, a="fy18", b="fy19", c="pc1819", type="uac")

df2 <- df[2] %>% data.frame()
df2<- absfy(df2, a="fy18", b="fy19", c="pc1819", type="family unit")

df3 <- df[3] %>% data.frame()
df3$Var.5 <- NULL
df3<- absfy(df3, a="fy18", b="fy19", c="pc1819", type="single adult")


sector_apprehensions <- rbind(df1, df2, df3)
sector_apprehensions19 <- sector_apprehensions

for (i in 2:3){
  sector_apprehensions19[,i] <- as.numeric(gsub(",", "", sector_apprehensions19[,i]))
}
  
df4 <- df[4] %>% data.frame() %>% 
  sbecfy(a="fy14", b="fy15", c="fy16",
         d = "fy17", e="fy18", f="fy19", 
         type="uac")
df4 <- df4 %>% 
  pivot_longer(cols=2:7, names_to="year",
               values_to="encounters") %>% 
  mutate(encounters=as.numeric(gsub(",", "", encounters)))

df5 <- df[5] %>% data.frame() %>% 
  sbecfy(a="fy16", b="fy17", c="fy18", d="fy19", 
         type="family unit")
df5 <- df5 %>% 
  pivot_longer(cols=2:5, names_to="year",
               values_to="encounters") %>% 
  mutate(encounters=as.numeric(gsub(",", "", encounters)))

df6 <- df[6] %>% data.frame()  %>% 
  sbecfy(a="fy16", b="fy17", c="fy18", d="fy19", 
         type="single adult")
df6 <- df6 %>% 
  pivot_longer(cols=2:5, names_to="year",
               values_to="encounters") %>% 
  mutate(encounters=as.numeric(gsub(",", "", encounters)))

sbecfydf <- rbind(df4, df5, df6)
sbecfydf19 <- sbecfydf

df7 <- df[7] %>% data.frame() %>% 
  secmon(month="TOTAL", year=2019)
df8 <- df[8] %>% data.frame() %>% 
  secmon(month="Oct", year=2018)
df9 <- df[9] %>% data.frame() %>% 
  secmon(month="Nov", year=2018) 
df10 <- df[10] %>% data.frame() %>% 
  secmon(month="Dec", year=2018)
df11 <- df[11] %>% data.frame() %>% 
  secmon(month="Jan", year=2019)
df12 <- df[12] %>% data.frame() %>% 
  secmon(month="Feb", year=2019) 
df13 <- df[13] %>% data.frame() %>% 
  secmon(month="Mar", year=2019)
df14 <- df[14] %>% data.frame() %>% 
  secmon(month="Apr", year=2019)
df15 <- df[15] %>% data.frame() %>% 
  secmon(month="Jun", year=2019)
df16 <- df[16] %>% data.frame() %>% 
  secmon(month="Jul", year=2019)
df17 <- df[17] %>% data.frame() %>% 
  secmon(month="Aug", year=2019)
df18 <- df[18] %>% data.frame() %>% 
  secmon(month="Sep", year=2019)
df19 <- df[19] %>% data.frame() %>% 
  secmon(month="Oct", year=2019)

secmondf <- rbind(df8, df9, df10,
                  df11, df12, df13,
                  df14, df15, df16,
                  df17, df18, df19)

for (i in 2:5){
  secmondf[,i] <- as.numeric(gsub(",", "", secmondf[,i]))
}

secmondf19 <- secmondf

# 2020
url <- "https://www.cbp.gov/newsroom/stats/sw-border-migration/usbp-sw-border-apprehensions-fy2020"


df <- read_html(url) %>% 
  html_table()

df1 <- df[1] %>% data.frame()
df1 <- absfy(df1, a="fy19", b="fy20", c="pc1920", type="uac")

df2 <- df[2] %>% data.frame()
df2<- absfy(df2, a="fy19", b="fy20", c="pc1920", type="family unit")

df3 <- df[3] %>% data.frame()
df3<- absfy(df3, a="fy19", b="fy20", c="pc1920", type="single adult")


sector_apprehensions <- rbind(df1, df2, df3)
sector_apprehensions20 <- sector_apprehensions

for (i in 2:3){
  sector_apprehensions20[,i] <- as.numeric(gsub(",", "", sector_apprehensions20[,i]))
}

df4 <- df[4] %>% data.frame() %>% 
  sbecfy(a="fy14", b="fy15", c="fy16",
         d = "fy17", e="fy18", f="fy19", 
         g="fy20",
         type="uac")
df4 <- df4 %>% 
  pivot_longer(cols=2:8, names_to="year",
               values_to="encounters") %>% 
  mutate(encounters=as.numeric(gsub(",", "", encounters)))

df5 <- df[5] %>% data.frame() %>% 
  sbecfy(a="fy16", b="fy17", c="fy18", d="fy19", e="fy20",
         type="family unit")
df5 <- df5 %>% 
  pivot_longer(cols=2:6, names_to="year",
               values_to="encounters") %>% 
  mutate(encounters=as.numeric(gsub(",", "", encounters)))

df6 <- df[6] %>% data.frame()  %>% 
  sbecfy(a="fy16", b="fy17", c="fy18", d="fy19", e="fy20",
         type="single adult")
df6 <- df6 %>% 
  pivot_longer(cols=2:6, names_to="year",
               values_to="encounters") %>% 
  mutate(encounters=as.numeric(gsub(",", "", encounters)))

sbecfydf <- rbind(df4, df5, df6)
sbecfydf20 <- sbecfydf

df7 <- df[7] %>% data.frame() %>% 
  secmon(month="Oct", year=2019)
df8 <- df[8] %>% data.frame() %>% 
  secmon(month="Nov", year=2019) 
df9 <- df[9] %>% data.frame() %>% 
  secmon(month="Dec", year=2019)
df10 <- df[10] %>% data.frame() %>% 
  secmon(month="Jan", year=2020)
df11 <- df[11] %>% data.frame() %>% 
  secmon(month="Feb", year=2020) 
df12 <- df[12] %>% data.frame() %>% 
  secmon(month="Mar", year=2020)
df13 <- df[13] %>% data.frame() %>% 
  secmon(month="Apr", year=2020)
df14 <- df[14] %>% data.frame() %>% 
  secmon(month="Jun", year=2020)
df15 <- df[15] %>% data.frame() %>% 
  secmon(month="Jul", year=2020)
df16 <- df[16] %>% data.frame() %>% 
  secmon(month="Aug", year=2020)
df17 <- df[17] %>% data.frame() %>% 
  secmon(month="Sep", year=2020)
df18 <- df[18] %>% data.frame() %>% 
  secmon(month="Oct", year=2020)

secmondf <- rbind(df7, df8, df9, df10,
                  df11, df12, df13,
                  df14, df15, df16,
                  df17, df18)

for (i in 2:5){
  secmondf[,i] <- as.numeric(gsub(",", "", secmondf[,i]))
}

secmondf20 <- secmondf


# 2021
url <- "https://www.cbp.gov/newsroom/stats/sw-border-migration/usbp-sw-border-apprehensions"

df <- read_html(url) %>% 
  html_table()

df1 <- df[1] %>% data.frame()
df1 <- absfy(df1, a="fy20", b="fy21", c="pc2021", type="uac")

df2 <- df[2] %>% data.frame()
df2<- absfy(df2, a="fy20", b="fy21", c="pc2021", type="family unit")

df3 <- df[3] %>% data.frame()
df3<- absfy(df3, a="fy20", b="fy21", c="pc2021", type="single adult")


sector_apprehensions <- rbind(df1, df2, df3)
sector_apprehensions21 <- sector_apprehensions

for (i in 2:3){
  sector_apprehensions21[,i] <- as.numeric(gsub(",", "", sector_apprehensions21[,i]))
}

df4 <- df[4] %>% data.frame() %>% 
  sbecfy(a="fy15", b="fy16", c="fy17",
         d = "fy18", e="fy19", f="fy20", 
         g="fy21",
         type="uac")
df4 <- df4 %>% 
  pivot_longer(cols=2:8, names_to="year",
               values_to="encounters") %>% 
  mutate(encounters=as.numeric(gsub(",", "", encounters)))

df5 <- df[5] %>% data.frame() %>% 
  sbecfy(a="fy16", b="fy17", c="fy18", d="fy19", e="fy20",
         f="fy21",
         type="family unit")
df5 <- df5 %>% 
  pivot_longer(cols=2:7, names_to="year",
               values_to="encounters") %>% 
  mutate(encounters=as.numeric(gsub(",", "", encounters)))

df6 <- df[6] %>% data.frame()  %>% 
  sbecfy(a="fy16", b="fy17", c="fy18", d="fy19", e="fy20",
         f="fy21",
         type="single adult")
df6 <- df6 %>% 
  pivot_longer(cols=2:7, names_to="year",
               values_to="encounters") %>% 
  mutate(encounters=as.numeric(gsub(",", "", encounters)))

sbecfydf <- rbind(df4, df5, df6)
sbecfydf21 <- sbecfydf


df7 <- df[7] %>% data.frame() %>% 
  secmon(month="Oct", year=2020)
df8 <- df[8] %>% data.frame() %>% 
  secmon(month="Nov", year=2020) 
df9 <- df[9] %>% data.frame() %>% 
  secmon(month="Dec", year=2020)
df10 <- df[10] %>% data.frame() %>% 
  secmon(month="Jan", year=2021)

secmondf <- rbind(df7, df8, df9, df10)

for (i in 2:5){
  secmondf[,i] <- as.numeric(gsub(",", "", secmondf[,i]))
}

secmondf21 <- secmondf

### overall again ---

sector_apprehensions19 
sbecfydf19 
secmondf19 

sector_apprehensions20  <- sector_apprehensions20 %>% 
  mutate(fy19=case_when(
    fy19==2.931 ~ 2931,
    TRUE ~ fy19
  ))
sbecfydf20
secmondf20 

sector_apprehensions21
sbecfydf21
secmondf21 

sector_apprehensions <- full_join(sector_apprehensions19,
                                  sector_apprehensions20) %>% 
  select(sector, type, fy18, fy19)

sector_apprehensions <- full_join(sector_apprehensions,
                                  sector_apprehensions21) %>% 
  select(-pc2021)

write_csv(sector_apprehensions, "output_data/sector_apprehensions_20210303.csv", na="")

sbecfydf <- rbind(sbecfydf19, sbecfydf20, sbecfydf21)

sbecfydf <- unique(sbecfydf)

write_csv(sbecfydf, "output_data/sbecfydf_20210303.csv", na="" )


secmondf <- rbind(secmondf19, secmondf20, secmondf21 )
write_csv(secmondf, "output_data/secmondf_20210303.csv", na="" )
