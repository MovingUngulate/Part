# Part <img src="man/figures/PartLog.jpg" align="right" height="200" width="120" />

## Overview

Part is a suite of tools to aid in modeling and predicting ungulate parturition both in real time
and on historic or previously collected datasets.

## Installation

### Currently, the only version of part is on GitHub:

To install:

```{r}
install.packages("devtools")

devtools::install_github("MovingUngulates/Part")
```

To uninstall current version and get newest version:

```{r}
remove.packages("Part", lib="~/R/win-library/3.4")

devtools::install_github('MovingUngulate/Part')
```

You also need tools to build markdown files and manipulate pdfs. This is done via miktex and PDFtk

sudo apt-get install pdftk
https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/


Will not email on Unix like systems. Need to set email='no' and email another way. as of V0.4


## General Thoughts - RealTime (scroll to bottom for "historic" predictions)

There are a number of ways to use Part. Are you trying to use code to replace Vaginal Implant Transmitters (VITs)? Are you trying to look back at past movement datasets to determine if you can identify parturition. Do you have a new species which we could add to the growing list of base species models?

Part is best used in collaboration across those of us working on ungulate parturition questions. Many species have evolved life-history strategies around their parturition behavior, mule deer, for instance, give birth and then hide their young who've adapted to remain motionless for some time. Elk typically split off from a herd, give birth in seclusion, leave a young calf in a central spot and forage out from that location, often returning to feed their youngster. We can exploit this knowledge and predict when and animal will give birth based on movement data and, potentially, avoid expensive and costly endeavors to track parturition timing in other ways.

## Usage

To use the automated reports, you need to install sendEmail. This is a lightweight command line program that is used to email the reports to users. 

Get the program zip file from here: http://caspian.dotconf.net/menu/Software/SendEmail/sendEmail-v156.zip

Simply extract that to wherever you please. Then, this become the progpath in your call to FawnMark or CalfMark.

Usage is fairly simple and is boiled down to a single function which downloads all your movement data, does all the manipulations, creates all the plots, creates all the pages of the report, merges all the report pages together and then, finally, emails you the results.

Lookup Table: You need a lookup table with a few columns of data. To see an example:

```{r}

exampLookUp<-data("ExampleLookup",package='Part')

```


Usage:

```{r}

#vecpath path to vectronic data folder
#ATSUsers character vector of ATS user names
#ATSPass character vector of ATS passwords
#tempdir temporary folder for downloading data
#spp Species of animal
#ncpu number of CPU cores for multithreaded work
#lookup path to lookup table
#mortvec character vector of mortalities 
CalfMark(    ATSUser=c('ATSLogin'),
             ATSPass=c('ATSPassword'),
             tempdir='C:/Users/mhayes1/Desktop/ElkFFS/',
             ncpu=6, #depends on threads on your machine
             lookup='F:/Box Sync/DEER/Summer Field Work/PregData.csv',
             mortvec=c(''),
             from='mhayes1@uwyo.edu',
             to=c('mhayes1@uwyo.edu','khuggler@uwyo.edu'),
             subject='Test Calf Mark',
             SP="smtp.office365.com:587",
             progpath='C:/Users/mhayes1/sendEmail/sendEmail.exe',
             username='email',
             password='password',
             email='yes')

```

# Example of what parturition looks like using this method (mule deer)

<img src="man/figures/ExamplePart.jpg" align="left" height="1000" width="900" />

- - -

This shows several different plots to help detect parturition in real-time. 

 - Top Left: Vit Status
 - Top Right: Movement Rate
 - Middle Left: dynamic bivariate gaussian bridge metrics
 - Middle Right: First Passage Time 50m radius
 - Bottom Left: First Passage Time 100m radius
 - Bottom Right: First Passage Time 150m radius

- - -

- - -

# Historic Model Building/Predictions

Models are initially built using a dataset with GPS data and known birth dates from VIT data.

## To Create historic models and validate internally:

```{r}

time.zone = 'Etc/GMT-7'
proj = '+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'

parts<-Part::PartWrap(dat=dat,
                      projstring=proj,
                      time.zone=time.zone,
                      dataset="AKMoose",
                      ncpus=75,
                      folder='/home/puma/Desktop/RunningPart/Results/AKMoose',
                      mean_date=148,
                      bday_dat=bday_dat,
                      sampsize=80,
                      finBoots=100)
```
Things are fairly simple here.

- dat = GPS data as a data.frame
  - Columns are:
    - UAID - character of unique animal id_year ('201_2016'; this is animal ID 201 for 2016)
    - TelemDate - POSIXct time/date field
    - x - x coordinate (numeric)
    - y - y coordinate (numeric)
  
- proj = projection of your data (MUST be a UTM/meter projection)
- time.zone = time.zone for the TelemDate column in the format above (eg. 'Etc/GMT-7' for mountain time)
- dataset = a name for your dataset. Files will be created with this name attached
- ncpus = how many cpus for parallel functions
- folder = the folder you'd like to save everything in. WILL BE DELETED EACH TIME YOU RUN THE FUNCTION
- mean_date = the average julian day of parturition in your dataset (the data is clipped +- around this to cut down on data)
- bday_dat = data.frame with two columns
    - Columns are:
      - UAID - character of unique animal id_year ('201_2016'; this is animal ID 201 for 2016)
      - Date.of.Birth - POSIXct formatted date of parturition
- sampsize = what proportion of animals to use in each bootstrap (eg. 80 = 80%)
- finBoots = how many bootstraps to conduct



## To predict to new data from historic models and/or validate externally:
```{r}
preds<-Part::Part_PredWrap(rfmod =           readRDS('/home/puma/Desktop/RunningPart/Results/DEERElk/Results/RFModsForPredict_20180926.RDS'),
                           dat = dat,
                           mean_date = 148,
                           projstring = proj,
                           dataset = 'Starkey_Elk',
                           time.zone = time.zone,
                           ncpus = 80,
                           folder = '/home/puma/Desktop/RunningPart',
                           imp=FALSE)
```

Things are fairly simple here.

- rfmod = the previously saved RFModsForPredict that you got from building models originally
- dat = New GPS data as a data.frame
  - Columns are:
    - UAID - character of unique animal id_year ('201_2016'; this is animal ID 201 for 2016)
    - TelemDate - POSIXct time/date field
    - x - x coordinate (numeric)
    - y - y coordinate (numeric)
  
- mean_date = the average julian day of parturition in your dataset (the data is clipped +- around this to cut down on data)
- proj = projection of your data (MUST be a UTM/meter projection)
- dataset = a name for your dataset. Files will be created with this name attached
- time.zone = time.zone for the TelemDate column in the format above (eg. 'Etc/GMT-7' for mountain time)
- ncpus = how many cpus for parallel functions
- folder = the folder you'd like to save everything in. WILL BE DELETED EACH TIME YOU RUN THE FUNCTION
- imp = TRUE or FALSE for if missing data should be imputed using missForest. Should only be set to true if models do not predict as expected.