



# Loading data and feature selection,

# Libraries needed for both processing and visualizations

library('readr')
library('dplyr')
library('ggplot2')
library('stringi')
library('ggplot2')
library('ggthemes')
library('shiny')
library('shinydashboard')
library('shinyWidgets')
library('DT')
library('scales')
library('plotly')
library('ggrepel')




# Loading dataset with interesting variables

interested <- c('_id','name','country','language','genre',
                'publicationDate','dateRelease','id_artist',
                'title', 'deezerFans')


#wasabi_albums <- read_csv("wasabi_albums.csv")[,interested]
data<- readRDS("albums_all_artists_3000 (1).rds")
my_data<-readRDS("albums_all_artists_3000 (1).rds")[,interested]


# Exploring analysis

colnames(my_data)
str(my_data)
head(my_data)


# Missing values identification and treatment

## Na's

colSums(is.na(my_data))

sum(apply(my_data, 1, anyNA))/nrow(my_data)

# % Na's

round(colSums(is.na(my_data))/nrow(my_data),digits = 3)

table(my_data$genre)

# Genres and Countries field
genre <- count(my_data,genre, sort = TRUE)
View(genre)

countries <- count(my_data,country, sort = TRUE)

View(countries)


####

# Time feature treatment,

# Here I do some investigation on the columns related to the time, before
# taking any action. Conclusion: The first component of every date in dateRelease is the year
# so we can safely pick the 4 first letters of the field to get the year



### Investigating pubdate and daterelease

## Checking length of pubdate and daterelease

datadate <- my_data %>%
  select(c('publicationDate','dateRelease')) %>%
  mutate(pubdate_L = nchar(publicationDate),
         daterel_L = nchar(dateRelease)) %>%
  count(pubdate_L,daterel_L)


## See what's going on with daterel_l != 10 
dateInvest <- my_data %>%
  select(c('publicationDate','dateRelease')) %>%
  mutate(pubdate_L = nchar(publicationDate),
         daterel_L = nchar(dateRelease)) %>%
  filter(daterel_L == 7)


# Conclusion: nchar != 10 means it doesn't have month or day

## Check number of characters until '-' on dateRelease


 

# Conclusion: The first component of every date in dateRelease is the year
# so we can safely pick the 4 first letters of the field to get the year

#####

### Mixing dates column into 'Year'

df1 <- my_data %>%
  mutate(dateRelease = substr(x = publicationDate,start = 1,stop = 4),
         year = ifelse(is.na(publicationDate),dateRelease,publicationDate)) %>%
  mutate(year = as.Date(year,format = '%Y'),
         year = as.numeric(substr(x = year,start = 1,stop = 4))) %>%
  select(!c(dateRelease,publicationDate))
attach(df)




### Joining the albums dataset with artists dataset

wasa_artists <- readRDS('wasabi_all_artists_3000.rds')[,interest]



colnames(wasa_artists)
colnames(df)
attach(wasa_artists)
attach(df1)
interest <- c('_id','genres')
wasa_full_artists <- readRDS('wasabi_all_artists_3000.rds')
wasa_artists <- readRDS('wasabi_all_artists_3000.rds')[,interest]


df <- df1 %>%
  left_join(wasa_artists,by = c('id_artist' = '_id'),na_matches = "never") %>%
  select(-c('_id','genres'))

colnames(df)


#### Clustering genres: ####

Music_Art <- c('classical','contemporary')

Country <- c('country')

Pop <- c('pop')

Electronic <- c('electronic','bass','disco','drum','dubstep','edm',
                'garage','hardcore','house','techno',
                'trance','ebm')

Funk <- c('funk rock','avant funk','go-go','boogie','electro funk','funk metal',
                'g-funk','timba funk','funk jam')

Hip_hop <- c('hip hop','trap','rap')


Jazz <- c('jazz')


Latin <- c('latin','salsa','bachata','mexican','tango','merengue',
                'latin pop','urban','reggaeton','brazilian')

Punk <- c('punk','industrial')

Rock <- c('rock')

Metal <- c('metal','nwobhm','Djent')

Soul <- c('soul','funk','soca','boogaloo')


# Clustering on the dataset
start_time <- Sys.time()
df2 <- df %>%
 
  mutate(genre = tolower(genre)) %>%
  mutate(cluster = sapply(genre, function(x) {
    case_when(
      any(stri_detect_fixed(str = x, pattern = Rock)) ~ 'Rock',
      any(stri_detect_fixed(str = x, pattern = Pop)) ~ 'Pop',
      any(stri_detect_fixed(str = x, pattern = Electronic)) ~ 'Electronic',
      any(stri_detect_fixed(str = x, pattern = Music_Art)) ~ 'Music art',
      any(stri_detect_fixed(str = x, pattern = Country)) ~ 'Country',
      any(stri_detect_fixed(str = x, pattern = Funk)) ~ 'Funk',
      any(stri_detect_fixed(str = x, pattern = Jazz)) ~ 'Jazz',
      any(stri_detect_fixed(str = x, pattern = Latin)) ~ 'Latin',
      any(stri_detect_fixed(str = x, pattern = Punk)) ~ 'Punk',
      any(stri_detect_fixed(str = x, pattern = Metal)) ~ 'Metal',
      any(stri_detect_fixed(str = x, pattern = Soul)) ~ 'Soul',
      is.na(x) ~ x,
      TRUE ~ 'Other'
    )
  }))
end_time <- Sys.time()
end_time - start_time
start_time <- Sys.time()
df2

table(df2$cluster)







write_csv(df2,'Data')




# Data with no char missing values:


myData <- read.csv('Data')
Data <- na.omit(myData) 
write_csv(Data,"C:\\Users\\Madzella\\OneDrive\\Documents\\iadata.csv")
getwd()
