library(stringi)
setwd("F://PvD//Studying//Programming//CTRRAssignment//CTRR_BTL")
dataRaw <- read.csv2("owid-covid-data.csv", header = TRUE, sep = ",")
setwd("F://PvD//Studying//Programming//CTRRAssignment//CTRR_BTL//task1")
#MADE 4263
  x <- length(dataRaw$continent)

#(1) Years of research
  curYear <- "2000"                 #current Year
  cntYear <- 0                      #counter Year (0 default)
  Years <- {}                       #Years listing
  iCountry <- dataRaw$iso_code[1]   #checking Years analyzed in 1st country
  for (i in 1 : 1000)
  {
    if (iCountry != dataRaw$iso_code[i])
      break;
    if (curYear != stri_sub(dataRaw$date[i], -4))
    {
      cntYear <- cntYear + 1
      Years[cntYear] <- stri_sub(dataRaw$date[i], -4)
      curYear <- stri_sub(dataRaw$date[i], -4)
    }
  }
  write.table(Years, file = "task1.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")

#(2) Print first 10 countries
{
    cntCountry <- 0                 #counter Counties
    curCountry <- "0"               #current Countries (0 default)
    Country <- {}                   #Countries listing
    Iso_code <- {}                  #Iso_code listing
    for (i in 1 : (x))
    {
      if (curCountry != dataRaw$iso_code[i] && stri_sub(dataRaw$iso_code[i], 0, 4) != "OWID")
      {
        cntCountry <- cntCountry + 1
        curCountry <- dataRaw$iso_code[i]
        Country[cntCountry] <- dataRaw$location[i]
        Iso_code[cntCountry] <- curCountry
      }
      if (cntCountry == 10)
        break
    }
    list_first10Country <- data.frame(Iso_code, Country)
    write.table(list_first10Country, file="task1.csv", row.names = FALSE, quote = FALSE, sep = ",", append = TRUE)
}

#(2) Counting number of Countries
  {
    cntCountry <- 1     #counter Counties
    curCountry <- "AFG" #current Country
    for (i in 1 : x)
    {
      if (curCountry != dataRaw$iso_code[i] && stri_sub(dataRaw$iso_code[i], 0, 4) != "OWID")
      {
        cntCountry <- cntCountry + 1
        curCountry <- dataRaw$iso_code[i]
      }
    }
    write.table(cntCountry, file = "task1.csv", row.names = FALSE, col.names = FALSE, append = TRUE)
  }
  
#(3) Data saved each continent
{
  curCon <- "Mars"    #current Continent
  Continent <- {}     #Continent list
  cntCon <- 0         #Continent counter
  
    for (i in 1 : x)
    {
      k <- stri_sub(dataRaw$iso_code[i], 0, 4)
      if (k != "OWID")
      {
        isValid <- 1; 
        if (cntCon > 0)
        for (j in 1 : (cntCon))
        {
          if (dataRaw$continent[i] == Continent[j])
          {
            isValid <- 0; break
          }
        }
        if (isValid != 0) 
        {
          curCon <- dataRaw$continent[i]
          cntCon <- cntCon + 1
          Continent[cntCon] <- curCon
        }
      }
    }
  
  write.table(paste("Continents:", cntCon), file = "task1.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  
  cntCon_Vie <- 0
  Continent_Vie <- {}
  for (i in 1 : cntCon)
  {
    if (Continent[i] == "Africa")
    {
      cntCon_Vie <- cntCon_Vie + 1
      Continent_Vie[cntCon_Vie] <- "Châu Phi"
    }
    if (Continent[i] == "Asia")
    {
      cntCon_Vie <- cntCon_Vie + 1
      Continent_Vie[cntCon_Vie] <- "Châu Á"
    }
    if (Continent[i] == "Europe")
    {
      cntCon_Vie <- cntCon_Vie + 1
      Continent_Vie[cntCon_Vie] <- "Châu Âu"
    }
    if (Continent[i] == "South America")
    {
      cntCon_Vie <- cntCon_Vie + 1
      Continent_Vie[cntCon_Vie] <- "Nam Mỹ"
    }
    if (Continent[i] == "North America")
    {
      cntCon_Vie <- cntCon_Vie + 1
      Continent_Vie[cntCon_Vie] <- "Bắc Mỹ"
    }
    if (Continent[i] == "Oceania")
    {
      cntCon_Vie <- cntCon_Vie + 1
      Continent_Vie[cntCon_Vie] <- "Châu Đại Dương"
    }
  }
  write.table(data.frame(Continent, Continent_Vie), file = "task1.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE, sep = ",")

  vector1 <- c(0,0,0,0,0,0)
  Observations <- array(vector1, dim <- 6)  #Data counter
}
#(4) data counted continents
{
  x <- length(dataRaw$continent)
    for (i in 1 : x)
    {
      k <- stri_sub(dataRaw$iso_code[i], 0, 4)
      if (k != "OWID")
      {
        for (j in 1 : (cntCon))
        {
          if (dataRaw$continent[i] == Continent[j])
          {
            Observations[j] <- Observations[j] + 1; break;
          }
        }
      }
    }
  
  write.table(data.frame(Continent, Observations), file = "task1.csv", row.names = FALSE, quote = FALSE, append = TRUE, sep = ",")
  sumData <- 0
  for (j in 1 : (cntCon))
  {
    sumData <- sumData + Observations[j]
  }
  write.table(paste("Tổng:", sumData), file = "task1.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
}

#(6,7) fewest and most data counted continent
{
  fewestcontinent <- 1000000
  posfewestcontinent <- 0
  mostcontinent <- 0
  posmostcontinent <- 0
  for (j in 1 : (cntCon))
  {
    {
      if (Observations[j] < fewestcontinent)
      {
        fewestcontinent <- Observations[j]
        posfewestcontinent <- j
      }
      if (Observations[j] > mostcontinent)
      {
        mostcontinent <- Observations[j]
        posmostcontinent <- j
      }
    }
  }
  write.table(paste("Châu lục ít dữ liệu nhất:", Continent_Vie[posfewestcontinent], fewestcontinent), file = "task1.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  write.table(paste("Châu lục nhiều dữ liệu nhất:", Continent_Vie[posmostcontinent], mostcontinent), file = "task1.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
}

#(5) data counted countries
{
  x <- length(dataRaw$continent)
  curCountry <- "VietnamNumber1"
  cntCountry <- 0
  Countries <- {}
  for (i in 1 : 10)
  Observations[i] <- 0
    for (i in x : 1)
    {
      k <- stri_sub(dataRaw$iso_code[i], 0, 4)
      if (k != "OWID")
      {
        if (dataRaw$location[i] != curCountry)
        {
          if (cntCountry == 10) break
          cntCountry <- cntCountry + 1
          curCountry <- dataRaw$location[i]
          Countries[11 - cntCountry] <- dataRaw$location[i]
          Observations[11 - cntCountry] <- 0
        }
        Observations[11 - cntCountry] <- Observations[11 - cntCountry] + 1
      }
    }
  write.table(data.frame(Countries, Observations), file = "task1.csv", row.names = FALSE, quote = FALSE, append = TRUE, sep = ",")
}
{
  x <- length(dataRaw$continent)
  curCountry <- "VietnamNumber1"
  cntCountry <- 0
  Countries <- {}
  for (i in 1 : 10)
  Observations[i] <- 0
    for (i in 1 : x)
    {
      k <- stri_sub(dataRaw$iso_code[i], 0, 4)
      if (k != "OWID")
      {
        if (dataRaw$location[i] != curCountry)
        {
          cntCountry <- cntCountry + 1
          curCountry <- dataRaw$location[i]
          Countries[cntCountry] <- dataRaw$location[i]
          Observations[cntCountry] <- 0
        }
        Observations[cntCountry] <- Observations[cntCountry] + 1
      }
    }
  sumData <- 0
  for (j in 1 : cntCountry)
  {
    sumData <- sumData + Observations[j]
  }
  write.table(paste("Tổng:", sumData), file = "task1.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
}

#(8,9) fewest and most data counted countries
{
  fewestcountry <- 1000000
  posfewestcountry <- 0
  mostcountry <- 0
  posmostcountry <- 0
  for (j in 1 : (10))
  {
    {
      if (Observations[j] < fewestcountry)
      {
        fewestcountry <- Observations[j]
        posfewestcountry <- j
      }
      if (Observations[j] > mostcountry)
      {
        mostcountry <- Observations[j]
        posmostcountry <- j
      }
    }
  }

  write.table(paste("Nước ít dữ liệu nhất:", Countries[posfewestcountry], fewestcountry), file = "task1.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)

  write.table(paste("Nước nhiều dữ liệu nhất:", Countries[posmostcountry], mostcountry), file = "task1.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
}

#(10,11) datee
{
  x <- length(dataRaw$date)
  curdate <- "VietnamNumber1"
  cntdate <- 0
  date <- {}
  Observations <- {}
  basedate <- strptime("1/1/2020", format = "%m/%d/%Y")
  everyday <- seq(as.Date("2020-1-1"), as.Date("2022-8-1"), by="days")
  for (i in 1 : 1000)
  Observations[i] <- 0
  for (i in 1 : x)
  {
    k <- as.integer(strptime(dataRaw$date[i], format = "%m/%d/%Y") - basedate)
    Observations[k] <- Observations[k] + 1
  }
  fewestdate <- 1000000
  posfewestdate <- 0
  mostdate <- 0
  cntmdate <- 0
  firstmdate <- 0
  lastmdate <- 0
  for (j in 1 : 1000)
  {
    {
      if (Observations[j] < fewestdate && Observations[j] > 0)
      {
        fewestdate <- Observations[j]
        posfewestdate <- j
      }
      if (Observations[j] > mostdate)
      {
        mostdate <- Observations[j]
      }
    }
  }
  for (j in 2 : 943)
  {
    k <- as.integer(strptime(everyday[j], format = "%Y-%m-%d") - basedate)
    if (Observations[k] == fewestdate)
    {
      posfewestdate <- everyday[j]
    }
    if (Observations[k] == mostdate)
    {
      if (cntmdate == 0)
      {firstmdate <- everyday[j]}
      cntmdate <- 1
    }
    else
    {
      if (cntmdate == 1)
      {
        lastmdate <- everyday[j-1]
        break
      }
    }
  }
  
  write.table(paste("Ngày ít dữ liệu nhất:", fewestdate), file = "task1.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  write.table((posfewestdate), file = "task1.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  write.table(paste("Ngày nhiều dữ liệu nhất:", mostdate), file = "task1.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  write.table(seq(firstmdate, lastmdate, by="days"), file = "task1.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  
}