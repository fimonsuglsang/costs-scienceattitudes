

library(tidyverse)

#reading data
read_csv("data/qolcost.csv") %>% 
  filter(att_1 == 2 & att_2 == 2 & att_3 == 2 ) %>% 
  mutate(id = 1:nrow(.)) -> rawqol

#reordering qualtrics data
qol <- plyr::join_all(
  
  list(
    
    #extracting attitude items
    
    rawqol %>% 
      select(id, matches("c\\d[tc]_\\d")) %>% 
      
      pivot_longer(2:25) %>% 
      drop_na() %>% 
      
      mutate(
        case = str_extract(name, "^.."),
        condition = str_extract(name, "(?<=^..)."),
        item = str_extract(name, "(?<=_).")
      ) %>% 
      select(-name) %>%
      mutate(
        item = case_when(
          item == "1" ~ "con" ,
          item == "2" ~ "qual" ,
          item == "3" ~ "mot" 
        )
      ) %>% 
      pivot_wider(names_from = item, values_from = value) %>% 
      mutate(index = (con+qual+mot)/3),
    
    #extracting completion time
    rawqol %>% 
      select(id, matches("c\\d[tc]_time_Page Submit")) %>% 
      
      pivot_longer(2:9) %>% 
      drop_na() %>% 
      
      mutate(
        case = str_extract(name, "^.."),
        condition = str_extract(name, "(?<=^..).")
      ) %>% 
      rename(time = value) %>% 
      select(-name),
    
    #extracting manipulation check
    rawqol %>% 
      select(id, matches("c\\d[tc]_man")) %>% 
      
      pivot_longer(2:9) %>% 
      drop_na() %>% 
      
      mutate(
        case = str_extract(name, "^.."),
        condition = str_extract(name, "(?<=^..).")
      ) %>% 
      select(-name) %>%
      rename(mancheck = value)
  )
) %>% 
  tibble() %>% 
  
  #background vars
  left_join(
    rawqol %>% 
      select(id, gender:sci_1) %>% 
      rename_all(str_remove, "_1") %>% 
      mutate(
        gender = case_when(
          gender == 1 ~ "Woman",
          gender == 2 ~ "Man"
        )
      ) 
  ) %>% 
  
  #question order
  left_join(
    rawqol %>% 
      select(id, order = FL_7_DO) %>% 
      separate(order, c("n1", "n2", "n3"), "\\|") %>% 
      pivot_longer(2:4) %>% 
      mutate(value = paste0("c", as.numeric(str_remove(value, "FL_"))-13)) %>% 
      mutate(name = as.numeric(str_remove(name, "n"))) %>% 
      rename(order = name, case = value)
  ) %>% 
  mutate(order = case_when(is.na(order) == T ~ 4, .default = order)
  ) %>% 
  
  #standardizing outcome
  mutate(
    across(
      c(index, con, qual, mot, mancheck),
      \(x) (x-mean(x, na.rm = T))/sd(x, na.rm = T)
    ),
    stime = (time-mean(time))/sd(time)
  ) 




