setwd("D:/ICMR/Data/Dataset")

list.files()

library(readxl)
library(dplyr)

### Import file
hpdf1 <- read_excel("STI_ST_Blood_Urine_Result (updated) 17 September 2026.xlsx",sheet = "Himachal Pradesh")

#hpdf1 <- read_excel("HP Interview data of 12-clusters UPDATED  04 May 2026.xlsx",sheet = "sample sheet")

names(hpdf1)
## Rename
hpdf1 <- hpdf1 %>% rename (# 'Household Number' = 'Household\r\n Number',
                           # 'Blood sample ID'= 'Blood\r\n sample ID',
                           # 'Urine sample ID'='Urine \r\nsample \r\nID',
                           'Ureaplasma Urealyticum' = 'Ureaplasma Urealyticum (UU)',
                           'Neisseria Gonorrhoea' = 'Neisseria Gonorrhoea (NG)',
                           'Mycoplasma Hominis' = 'Mycoplasma Hominis (MH)',
                           'Mycoplasma Genitalium'= 'Mycoplasma Genitalium (MG)',
                           'Ureaplasma Parvum' = 'Ureaplasma Parvum (UP)',
                           'Chlamydia Trachomatis' = 'Chlamydia Trachomatis (CT)',
                           'Trichomonas Vaginalis' = 'Trichomonas Vaginalis (TV)')

### Recode

varh<- c ("HIV","HCV","HBsAg", "HSV-1-IgG", "HSV-2-IgG","HSV-1&2-IgM","Syphilis","Ureaplasma Urealyticum",
          "Neisseria Gonorrhoea","Mycoplasma Hominis","Mycoplasma Genitalium","Ureaplasma Parvum",
          "Chlamydia Trachomatis","Trichomonas Vaginalis")

hpdf2<- hpdf1%>% mutate(across(all_of(varh), ~ recode(.,
                                                      "NON-REACTIVE" = "Negative",
                                                      "REACTIVE"= "Positive",
                                                      "POSITIVE" = "Positive",
                                                      "NEGATIVE" = "Negative")))
table(hpdf2$HIV)

#### removing observations with 0 tests done

hpdf3 <- hpdf2 %>% filter(rowSums(!is.na(across(all_of(varh))))>0)

hpdf3 <- hpdf3 %>% arrange(`Cluster ID`,`Household Number`)

View(hpdf3)

hpdf3 <- hpdf3 %>% select(State,State_ID,Cluster,`Cluster ID`, `Household Number`,Name,Age,Gender,`Blood sample ID`,
                          HIV,HCV,HBsAg, `HSV-1-IgG`, `HSV-2-IgG`,`HSV-1&2-IgM`,Syphilis,
                          `Urine sample ID`, `Ureaplasma Urealyticum`,`Neisseria Gonorrhoea`,
                          `Mycoplasma Hominis`,`Mycoplasma Genitalium`,`Ureaplasma Parvum`,
                          `Chlamydia Trachomatis`, `Trichomonas Vaginalis`)
table(hpdf3$HIV)

### Export

write.csv(hpdf3,"D:/ICMR/Data/Outputs/Himachal_Pradesh_Site.csv",row.names = FALSE, na="") 

####################### Merging with interview data ##########################

### Import interview data
df1<- read.csv("STI_Tribal_Interview.csv")

dfs1<- df1 %>% filter(state == 1)

count(dfs1) 

## Renaming
dfs1 <- dfs1 %>% rename(State_ID = state,
                        'Cluster ID' = clusterid,
                        'Household Number'= hh_no,
                        Age = Q01_age,
                        Gender = Q02_gender,
                        Marital_status = Q03_marital_status,
                        Education = Q07_education,
                        Occupation = Q08_occupation,
                        SES = Q09_soc_econ_status)

dfs1 <- dfs1 %>% mutate(Gender = case_when( Gender== 1 ~ "Male",
                                            Gender== 2~ "Female",
                                            TRUE~ NA_character_))

### Checking duplicate observation 

dfs1 %>% count(State_ID, `Cluster ID`, `Household Number`,Gender) %>%  filter(n > 1)
hpdf3  %>% count(State_ID, `Cluster ID`, `Household Number`,Gender) %>%  filter(n > 1)


#### Joining datasets

hpdf3$`Household Number` <- as.character(hpdf3$`Household Number`)

hpdfs<- hpdf3 %>% left_join(dfs1 %>% 
                              select(State_ID,`Cluster ID`,`Household Number`,Gender,
                                     Marital_status,Education,Occupation,SES),
                              by= c("State_ID","Cluster ID", "Household Number",'Gender'))

### Checking for missing values
sum(is.na(hpdf3$Gender))
sum(is.na(dfs1$Gender))
sum(is.na(dfs1$Occupation))
sum(is.na(dfs1$Marital_status))
sum(is.na(dfs1$Education))
colSums(is.na(hpdfs))

View(hpdfs)

hpdfs <- hpdfs %>% select(State,State_ID,Cluster,`Cluster ID`, `Household Number`,Name,
                          Age,Gender,Marital_status, Education,Occupation,SES,everything())


write.csv(hpdfs,"D:/ICMR/Data/Outputs/SES_Himachal.csv",row.names = FALSE, na="")     


####################################################################################
#### Master dataset 
hpdfm <- hpdf3 %>% left_join(dfs1 %>% 
                               select(-Age),
                             by=c ("State_ID","Cluster ID", "Household Number",'Gender'))

### Rearanging the variables

hpdfm <- hpdfm %>%
  select(State, State_ID, Cluster, `Cluster ID`, `Household Number`,
         Name, Age, Gender, Marital_status, Education, Occupation, SES,everything())

hpdfm <- hpdfm %>% 
  relocate(`Blood sample ID`,
           HIV,HCV,HBsAg, `HSV-1-IgG`, `HSV-2-IgG`,`HSV-1&2-IgM`,Syphilis,
           `Urine sample ID`, `Ureaplasma Urealyticum`,`Neisseria Gonorrhoea`,
           `Mycoplasma Hominis`,`Mycoplasma Genitalium`,`Ureaplasma Parvum`,
           `Chlamydia Trachomatis`, `Trichomonas Vaginalis`,
           .after= last_col())
names(hpdfm)
write.csv(hpdfm,"D:/ICMR/Data/Outputs/Himachal.csv",row.names = FALSE, na="")
write.xlsx(hpdfm,"D:/ICMR/Data/Outputs/Himachal.xlsx",rowNames = FALSE,keepNA=FALSE)


################################### Descriptive  ####################################

#### Descriptive 

table(hpdf3$`Cluster ID`)

table(hpdfs$State_ID, hpdfs$HIV)

table(hpdfs$State_ID, hpdfs$HCV)

table(hpdf3$State_ID, hpdf3$HBsAg)

table(hpdf3$State_ID,hpdf3$`HSV-1-IgG`)

table(hpdf3$State_ID,hpdf3$`HSV-2-IgG`)

table(hpdf3$State_ID,hpdf3$`Urine sample`)

table(hpdf3$State_ID,hpdf3$`Ureaplasma Urealyticum`)

table(hpdf3$State_ID,hpdf3$`Neisseria Gonorrhoea`)

table(hpdf3$State_ID,hpdf3$`Mycoplasma Hominis`)

table(hpdf3$State_ID,hpdf3$`Mycoplasma Genitalium`)

table(hpdf3$State_ID,hpdf3$`Ureaplasma Parvum`)

table(hpdf3$State_ID,hpdf3$`Chlamydia Trachomatis`)

table(hpdf3$State_ID,hpdf3$`Trichomonas Vaginalis`)
