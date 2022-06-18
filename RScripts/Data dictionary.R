## Loading libraries
library(tidyverse)
library(here)
library(dataMeta)

# Importing collected data
collected_data <- read_csv(here("Rawdata","CollectedDataLOSFinal.csv"))

# Inspecting collected data

head(collected_data)
glimpse(collected_data)

variable_description <- c("Index column allows identification of the observation regarding the original LOS dataset in the raw folder.",
                          "The number of days spend in Hospital", "Age of the individual", "Outcome of the hospitalization",
                          "Consent from the end-user to process and share the data collected with the data capture tool")
variable_type <- c(0,0,0,1,1)

linker <- build_linker(collected_data,variable_description,variable_type)
print(linker)

data_dictionary <- build_dict(my.data = collected_data,linker = linker)

glimpse(data_dictionary)

write_csv(data_dictionary,here("Rawdata","Collected_dataLOS_datadictionary.csv"))



# Appending data dictionary to collected data

main_string <- "This data is artificially created and fabricates a patient dataset with age, lenght of stay and death status for 300 patients across 10 hospitals"

main_string

complete_collectedLOSdata <- incorporate_attr(my.data=collected_data,data.dictionary = data_dictionary,main_string = main_string)

attributes(complete_collectedLOSdata)$author[1] <- "First Last Name"

complete_collectedLOSdata

attributes(complete_collectedLOSdata)

save_it(complete_collectedLOSdata,here("Rawdata","CollectedDataLOSFinal"))


