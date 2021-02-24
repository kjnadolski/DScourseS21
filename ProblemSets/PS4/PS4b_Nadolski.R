#### PS4b_Nadolski.R
#### Karley Nadolski
#### ECON 5253

# Using Spark through R

library(sparklyr)
library(tidyverse)

# 4. Set up a connection to Spark by issuing the following commands: 
    spark_install(version = "3.0.0")
    sc <- spark_connect(master = "local")

# 5. Create a tibble called df1 that uses the iris data
       df1 <- as_tibble(iris)
       
# 6. Copy the table into Spark calling it
       df <- copy_to(sc, df1)
       
# 7. Check the class of df and df1
      class(df1) # "tbl_df", "tbl", "data.frame"
      class(df) # "tbl_spark", "tbl_sql", "tbl_lazy", "tbl"
      
# 9. Apply common RDD/SQL operations
      # select: 
      df %>% select(Sepal_Length, Species) %>% head %>% print
# 10. Apply filter
      df %>% filter(Sepal_Length>5.5) %>% head %>% print
# 11. Combine filter and select commands
      df %>% filter(Sepal_Length>5.5) %>% select(Sepal_Length, Species) %>% head %>% print
# 12. Use "group_by"
      df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>% head %>% print
# 13. Use a common RDD operation to sort the result in ascending order by species
      df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>% head %>% print
      df2 %>% arrange(Species) %>% head %>% print
    
    