library(stringr)

data <- read_excel("C:/Users/hp/Desktop/data.xlsx")

Data_Frame <- data.frame(
  name = character(),
  count = numeric(),
  stringsAsFactors = FALSE  # Avoid factor conversion
)

View(data)

colCount <- dim(data)[1]
for (i in 1:colCount) {
  str <- as.character(data[i, 7])
  print(str)
  strList <- strsplit(str, ",")
  strLen <- length(strList[[1]])
  print(strLen)
  
  for (ii in 1:strLen) {
    value <- gsub(" ", "", strList[[1]][ii])
    lenDf <- length(Data_Frame$name)
    
    if (lenDf == 0) {
      # If Data_Frame is empty, add the first row
      Data_Frame <- rbind(Data_Frame, data.frame(name = value, count = 1, stringsAsFactors = FALSE))
    } else {
      # Check if the value already exists in Data_Frame
      match_idx <- match(value, Data_Frame$name)
      
      if (!is.na(match_idx)) {
        # If the value exists, increment the count
        Data_Frame$count[match_idx] <- Data_Frame$count[match_idx] + 1
      } else {
        # If the value doesn't exist, add a new row
        Data_Frame <- rbind(Data_Frame, data.frame(name = value, count = 1, stringsAsFactors = FALSE))
      }
    }
    
    print(Data_Frame)
  }
}





barplot(Data_Frame$count, names.arg = Data_Frame$name, horiz = TRUE)






Data_Frame