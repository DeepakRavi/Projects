library(RCurl)
library(XML)

#---------------------------------------Web Scraping-----------------------------------------
GetAllListings <- function()
{
base_url <- "https://www.airbnb.com/s/"
page_url <- "?page="
pg <- 1
Area <- c("Boston--MA--United-States","Cambridge--MA--United-States","Quincy--MA--United-States",
          "Worcester--MA--United-States","Lowell--MA--United-States","Providence--MA--United-States")
assimilated.listing <- NULL
  
      for(listing.city in 1 : length(Area))
      {
      
            for(listing.page in 1 : pg)
            {
              new_url <- paste(base_url, Area[listing.city], page_url, listing.page , sep = "")              
              Sys.sleep(sample(0:2,1))
              webpage <- getURL(new_url,cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
              tc <- textConnection(webpage)
              webpage <- readLines(tc)
              close(tc)
              pagetree <- htmlTreeParse(webpage, useInternalNodes = TRUE)
              
              listings <- unlist(xpathApply(pagetree, "//*/div[@class='listing']", xmlValue))
              
              #Latitude of the accommodation
              lat <- unlist(xpathApply(pagetree, "//div[@class = 'listing']", xmlGetAttr,'data-lat'))
              lat <- data.frame(lat)
              
              #Longitude of the accommodation
              lon <- unlist(xpathApply(pagetree, "//div[@class = 'listing']", xmlGetAttr,'data-lng'))
              lon <- data.frame(lon)
              
              #Title of the accommodation
              title <- unlist(xpathApply(pagetree, "//div[@class = 'listing']", xmlGetAttr,'data-name'))
              title <- data.frame(title)
              
              #Url of every listing on the page
              url <- unlist(xpathApply(pagetree, "//div[@class = 'listing']", xmlGetAttr,'data-url'))
              url <- data.frame(url)
              
              #user number of every listing 
              user.num <- unlist(xpathApply(pagetree, "//div[@class = 'listing']", xmlGetAttr,'data-user'))
              user.num <- data.frame(user.num)
              
              #user id of every listing
              user.id <- unlist(xpathApply(pagetree, "//div[@class = 'listing']", xmlGetAttr,'data-id'))
              user.id <- data.frame(user.id)        
                            
              desc <- unlist(xpathApply(pagetree, "//div[@class = 'listing']/div[@class = 'panel-image listing-img']/div/div", xmlValue))
              
              #Combining all data frames into one
              individual.listing <- data.frame(cbind(user.id,user.num,title,lat,lon,url))
              
              #COmbining listings from various pages
              assimilated.listing <- rbind(assimilated.listing, individual.listing)          
              
            }    
      }

write.csv(assimilated.listing, "assimilated.listing.csv", row.names = FALSE)
}

#Scraping more information from each of these listings

ListingsDetails <- function(assimilated.listing)
{
base_url <- "https://www.airbnb.com/rooms/"
space.info <- NULL
price.info <- NULL
review.info <- NULL
    
    for(iterate.listings in 1: length(assimilated.listing$user.id))
    {
      new_url <- paste(base_url, as.character(assimilated.listing$user.id[iterate.listings]), sep = "")         
      Sys.sleep(sample(1:2,1))    
      webpage <- getURL(new_url, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
      tc <- textConnection(webpage)
      webpage <- readLines(tc)
      close(tc)
      pagetree <- htmlTreeParse(webpage, useInternalNodes = TRUE)  
      
      
      listings.price <- unlist(xpathApply(pagetree, "//*/div[@class='row']/div[@class = 'col-md-9']/div/div[@class = 'col-md-6']/div", xmlValue))
      listings.reviews <- unlist(xpathApply(pagetree, "//div[@id = 'host-profile']/div/div[@class = 'row']/div[@class = 'col-lg-8']/div[@class = 'row']/div[@class = 'col-md-9']/div[@class = 'row row-condensed space-2']/div", xmlValue))
      listings.overallprice <- unlist(xpathApply(pagetree, "//*/div[@id = 'room']/div[@id = 'summary']/div/div/div[@class = 'col-lg-4']/div[@class = 'book-it js-book-it']/div[@class = 'book-it__price js-price']/div", xmlValue))          
      listings.overallreviews <- unlist(xpathApply(pagetree, "//*[@id='host-profile']/div/div/div/div[3]/div[2]/div/div[1]/a/div/div[1]/span", xmlValue))      
      listings.starratings <- unlist(xpathApply(pagetree, "//*[@id='display-address']/a[2]/div/div[1]/meta", xmlGetAttr,"content"))
      
      room.type = bed.type = property.type = accommodates = num.bedrooms = num.bathrooms = num.beds = check.in =check.out = pet.owner = NA
      
      if(is.null(listings.price) == TRUE)
      {
          user.id <- as.character(assimilated.listing$user.id[iterate.listings])
          
          room.type = bed.type = property.type = accommodates = num.bedrooms = num.bathrooms = num.beds = check.in =check.out = pet.owner = NA
          temp.space <- data.frame(cbind(user.id, room.type, bed.type, property.type, accommodates, num.bedrooms, num.bathrooms, num.beds, check.in, check.out, pet.owner))
          space.info <- rbind(space.info,temp.space)
          
          extra.people = cancellation = clean.fee = weekly.fee = monthly.fee = security.deposit = price.pernight = NA
          temp.price <- data.frame(cbind(user.id, price.pernight, weekly.fee, monthly.fee, security.deposit, extra.people, clean.fee, cancellation ))
          price.info <- rbind(price.info, temp.price)
           
      }
      else
      { 
        room.type = bed.type = property.type = accommodates = num.bedrooms = num.bathrooms = num.beds = check.in =check.out = pet.owner = NA      
        extra.people = cancellation = clean.fee = weekly.fee = monthly.fee = security.deposit = price.pernight = NA  
        
        for(iterate.detail in 1:  length(listings.price))
          {
            str <- unlist(strsplit(listings.price[iterate.detail],":"))
            
            if(str[1] == "Room type")
            {
              room.type <- str[2]
            }
            else if(str[1] == "Bed type")
            {
              bed.type <- str[2]
            }
            else if(str[1] == "Property type")
            {
              property.type <- str[2]
            }
            else if(str[1] == "Accommodates")
            {
              accommodates <- str[2]
            }
            else if(str[1] == "Bedrooms")
            {
              num.bedrooms <- str[2]
            }
            else if(str[1] == "Bathrooms")
            {
              num.bathrooms <- str[2]
            }
            else if(str[1] == "Beds")
            {
              num.beds <- str[2]
            }
            else if(str[1] == "Check In")
            {
              check.in <- str[2]
            }
            else if(str[1] == "Check Out")
            {
              check.out <- str[2]
            }
            else if(str[1] == "Pet Owner")
            {
              pet.owner <- str[2]
            }
            else if(str[1] == "Extra people")
            {
              extra.people <- str[2]
            }
            else if(str[1] == "Cancellation")
            {
              cancellation <- str[2]
            }
            else if(str[1] == "Cleaning Fee")
            {
              clean.fee <- str[2]
            }
            else if(str[1] == "Weekly Price")
            {
              weekly.fee <- str[2]
            }
            else if(str[1] == "Security Deposit")
            {
              security.deposit <- str[2]
            }
            else if(str[1] == "Monthly Price")
            {
              monthly.fee <- str[2]
            }
            
          }    
      
      user.id <- as.character(assimilated.listing$user.id[iterate.listings])    
      temp.space <- data.frame(cbind(user.id, room.type, bed.type, property.type, accommodates, num.bedrooms, num.bathrooms, num.beds, check.in, check.out, pet.owner))
      space.info <- rbind(space.info,temp.space)
      
      price.pernight <- unlist(strsplit(listings.overallprice[1], "\n"))
      price.pernight <- grep(pattern = "[0-9]", price.pernight, value = TRUE)
      
      temp.price <- data.frame(cbind(user.id, price.pernight, weekly.fee, monthly.fee, security.deposit, extra.people, clean.fee, cancellation ))
      price.info <- rbind(price.info, temp.price)     
                
      }
      
      #Collecting data about host-profile and reviews
      
      if(is.null(listings.reviews)  == TRUE)
      {
        member.since <- NA
        response.rate <- NA
      }
      else
      {
        member.since <- as.numeric(gsub("\\D","",listings.reviews[1])) 
        response.rate <- as.numeric(gsub("%","",gsub(" ","",unlist(strsplit(listings.reviews[2],"\n"))[3])))
      }
      if(is.null(listings.overallreviews) == TRUE)
      {
       overall.review <- NA 
      }
      else
      {
        overall.review <- as.numeric(gsub("\\D","",listings.overallreviews))   
      }
            
      if(is.null(listings.starratings) == TRUE)
      {
        star.ratings <- NA
        
      }
      else
      {
        star.ratings <- as.numeric(listings.starratings)        
      }
      
      user.id <- as.character(assimilated.listing$user.id[iterate.listings])
      temp.review <- data.frame(cbind(user.id, member.since, response.rate, overall.review, star.ratings))
      review.info <- rbind(review.info,temp.review)                 
            
    }

write.csv(space.info, "space.info.csv", row.names = FALSE)
write.csv(price.info, "price.info.csv", row.names = FALSE)
write.csv(review.info, "review.info.csv", row.names = FALSE)
  
}

#-------------------------------------Data Cleaning------------------------------------------

DataCleaning <- function()
{
  assimilated.listing <- read.csv("assimilated.listing.csv", header = TRUE)
  price.info <- read.csv("price.info.csv", header = TRUE)
  space.info <- read.csv("space.info.csv", header = TRUE)
  review.info <- read.csv("review.info.csv", header = TRUE)
  
  #----------------------------------cleaning price.info table----------------------------------
  
  #Cleaning price.pernight column
  
  for(i in 1 : length(price.info$price.pernight))
  {
    if(is.na(price.info$price.pernight[i]) == TRUE)
    {
      price.info$price.pernight[i] <- 0
    }
    
  } 
  price.info$price.pernight <- as.numeric(gsub("\\$","", gsub(" ", "",as.character(price.info$price.pernight))))
  
  #Cleaning weekly.fee column
  
  for(i in 1 : length(price.info$weekly.fee))
  {
    if(is.na(price.info$weekly.fee[i]) == TRUE)
    {
      price.info$weekly.fee[i] <- 0
    }
    
  }
  price.info$weekly.fee <- as.numeric(gsub("\\D","",gsub("/","",gsub("\\$","",as.character(price.info$weekly.fee)))))
  
  #Cleaning monthly.fee column  
  
  for(i in 1 : length(price.info$monthly.fee))
  {
    if(is.na(price.info$monthly.fee[i]) == TRUE)
    {
      price.info$monthly.fee[i] <- 0
    }
    
  }
  price.info$monthly.fee <- as.numeric(gsub("\\D","",gsub("/","",gsub("\\$","",as.character(price.info$monthly.fee)))))
  
  #Cleaning security.deposit column
  
  price.info$security.deposit <- as.character(price.info$security.deposit)
  
  for(i in 1 : length(price.info$security.deposit))
  {
    if(is.na(price.info$security.deposit[i]) == TRUE)
    {
      price.info$security.deposit[i] <- 0
    }
    
  }
  price.info$security.deposit  <- as.numeric(gsub("\\$","",price.info$security.deposit))
  
  #Cleaning clean.fee column
  
  price.info$clean.fee <- as.character(price.info$clean.fee)
  
  for(i in 1 : length(price.info$clean.fee))
  {
    if(is.na(price.info$clean.fee[i]) == TRUE)
    {
      price.info$clean.fee[i] <- 0
    }
    
  }
  price.info$clean.fee  <- as.numeric(gsub("\\$","",price.info$clean.fee))
  
  #cleaning extra.people column
  
  price.info$extra.people <- as.character(price.info$extra.people)
  for(i in 1 : length(price.info$extra.people))
  {
    if(is.na(price.info$extra.people[i]) == TRUE || price.info$extra.people[i] == " No Charge")
    {
      price.info$extra.people[i] <- 0
    }
    
  }
  for(i in 1: length(price.info$extra.people))
  {
    price.info$extra.people[i] <- as.numeric(gsub("\\D","",unlist(strsplit(price.info$extra.people[i], "/"))[1]))
  }  
  
  
  #----------------------------------cleaning review.info table----------------------------------
  
  #cleaning all columns of NA
  
  for(i in 1 : length(review.info$star.ratings))
  {
    if(is.na(review.info$star.ratings[i]) == TRUE)
    {
      review.info$star.ratings[i] <- 0
    }   
    
  } 
  for(i in 1 : length(review.info$member.since))
  {
    if(is.na(review.info$member.since[i]) == TRUE)
    {
      review.info$member.since[i] <- 0
    }
    
  }
  for(i in 1 : length(review.info$response.rate))
  {
    if(is.na(review.info$response.rate[i]) == TRUE)
    {
      review.info$response.rate[i] <- 0
    }
    
  }
  for(i in 1 : length(review.info$overall.review))
  {
    if(is.na(review.info$overall.review[i]) == TRUE)
    {
      review.info$overall.review[i] <- 0
    }    
  }  
 
  #-------------------------------------space.info table--------------------------------------
    
  #writing the cleaned files back into csv
  
  write.csv(space.info, "space.info.csv", row.names = FALSE)
  write.csv(price.info, "price.info.csv", row.names = FALSE)
  write.csv(review.info, "review.info.csv", row.names = FALSE)  
  
}

#---------------------------------------Main Script---------------------------------------

#This function scrapes the main page of AirBnB and stores all the data as a csv file
#in the working directory
GetAllListings()

#Reading the scraped csv file from the previous step
assimilated.listing <- read.csv("assimilated.listing.csv", header = TRUE)

#This function further scrapes each listing for more information about the space,
#price and reviews
ListingsDetails(assimilated.listing)

#This function cleans all the scraped data for analysis
DataCleaning()









