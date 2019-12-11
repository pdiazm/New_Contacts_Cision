#Survey Responses Processor

library(tidyverse)

#load data
new_contacts <- read_csv("~/R Projects/Cision/Survey_Uploads/New_survey_responses.csv")

#Update observations with list names conditional on the language chosen by the contact
clean_contacts <- new_contacts %>%
  mutate(`All news releases` = case_when(English == 1 & `All news releases` == 1 ~ "OECD Eng All News Releases",
                                         French == 1 & `All news releases` == 1 ~ "OECD Fre All news releases"),
         `Agriculture and food` = case_when(English == 1 & `Agriculture and food` == 1 ~ "OECD Eng Agriculture and food",
                                            French == 1 & `Agriculture and food` == 1 ~ "OECD Fre Agriculture and food"),
         `Cities and regions` = case_when(English == 1 & `Cities and regions` == 1 ~ "OECD Eng Cities and regions",
                                          French == 1 & `Cities and regions` == 1 ~ "OECD Fre Cities and regions"),
         `Climate change` = case_when(English == 1 & `Climate change` == 1 ~ "OECD ENG Environment",
                                      French == 1 & `Climate change` == 1 ~ "OECD FR Environment"),
         `Corruption and money laundering` = case_when(English == 1 & `Corruption and money laundering` == 1 ~ "OECD Eng Corruption and money laundering",
                                                       French == 1 & `Corruption and money laundering` == 1 ~ "OECD Fre Corruption and money laundering"),
         Development = case_when(English == 1 & Development == 1 ~ "OECD Development Eng",
                                 French == 1 & Development == 1 ~ "OECD Development Fre"),
         `Digital Economy and technology` = case_when(English == 1 & `Digital Economy and technology` == 1 ~ "OECD Digital Eng",
                                                      French == 1 & `Digital Economy and technology` == 1 ~ "OECD Digital Fre"),
         Economy = case_when(English == 1 & Economy == 1 ~ "OECD Eng Economy",
                             French == 1 & Economy == 1 ~ "OECD Fre Economy"),
         Education = case_when(English == 1 & Education == 1 ~ "OECD Education Eng",
                               French == 1 & Education == 1 ~ "OECD Education French"),
         Employment = case_when(English == 1 & Employment == 1 ~ "OECD Eng Employment",
                                French == 1 & Employment == 1 ~ "OECD Fre Employment"),
         Environment = case_when(English == 1 & Environment == 1 ~ "OECD ENG Environment",
                                 French == 1 & Environment == 1 ~ "OECD FR Environment"),
         `Finance and investment` = case_when(English == 1 & `Finance and investment` == 1 ~ "OECD Eng Finance and Investment",
                                              French == 1 & `Finance and investment` == 1 ~ "OECD Fre Finance and Investment"),
         Health = case_when(English == 1 & Health == 1 ~ "OECD Eng Health",
                            French == 1 & Health == 1 ~ "OECD Fr Health"),
         `Industry and entrepreneurship` = case_when(English == 1 & `Industry and entrepreneurship` == 1 ~ "OECD Eng Industry and Entrepreneurship",
                                                     French == 1 & `Industry and entrepreneurship` == 1 ~ "OECD Fre Industry and Entrepreneurship"),
         `Media Events Calendar` = case_when(`Media Events Calendar` == 1 ~ "OECD Media Calendar"),
         Migration = case_when(English == 1 & Migration == 1 ~ "OECD Eng Migration",
                               French == 1 & Migration == 1 ~ "OECD Fre Migration"),
         `Public governance and regulation` = case_when(English == 1 & `Public governance and regulation` == 1 ~ "OECD Public Governance Eng",
                                                        French == 1 & `Public governance and regulation` == 1 ~ "OECD Public Governance Fre"),
         `Science and innovation` = case_when(English == 1 & `Science and innovation` == 1 ~ "OECD Eng Science and innovation",
                                              French == 1 & `Science and innovation` == 1 ~ "OECD Fre Science and innovation"),
         `Social affairs and well-being` = case_when(English == 1 & `Social affairs and well-being` == 1 ~ "OECD Social Affairs ENG",
                                                     French == 1 & `Social affairs and well-being` == 1 ~ "OECD Social Affairs FR"),
         Statistics = case_when(English == 1 & Statistics == 1 ~ "OECD Statistics ENG",
                                French == 1 & Statistics == 1 ~ "OECD Statistics FR"),
         Tax = case_when(English == 1 & Tax == 1 ~ "OECD TAX ENGLISH",
                         French == 1 & Tax == 1 ~ "OECD TAX FRENCH"),
         Trade = case_when(English == 1 & Trade == 1 ~ "OECD Eng Trade",
                           French == 1 & Trade == 1 ~ "OECD Fre Trade"),
         Transport = case_when(English == 1 & Transport == 1 ~ "OECD Eng Transport",
                               French == 1 & Transport == 1 ~ "OECD Fre Transport")) %>%
  unite(col = "lists", `All news releases`:Transport, sep = ";") %>%
  mutate(lists = str_replace_all(lists, "NA", ""),
         lists = str_replace_all(lists, ";;*", ";")) %>%
  select(`First Name`, `Last Name`, Country, lists, `Contact Email`) %>%
  mutate(country = Country, `List(s)` = lists) %>%
  select(`First Name`, `Last Name`, `Contact Email`, country, `List(s)`) %>%
  rename(Country = country, Email = `Contact Email`) %>%
  mutate(Country = if_else(grepl("Korea", Country), "South Korea", Country),
         Country = if_else(grepl("UK", Country), "United Kingdom", Country),
         Country = if_else(grepl("England", Country), "United Kingdom", Country),
         Country = if_else(grepl("Türkiye", Country), "Turkey", Country),
         Country = if_else(grepl("US", Country), "United States", Country),
         Country = if_else(grepl("Norge", Country), "Norway", Country)) %>%
  distinct()

# Write dataframe to csv file, ready for uploading to Cision
write_csv(clean_contacts, "~/R Projects/Cision/Survey_Uploads/New_Contacts_Upload.csv")

