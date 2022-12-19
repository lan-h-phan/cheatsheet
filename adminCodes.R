##################################
#Code to update Github token in R
###################################

#install.packages("gitcreds)
library(gitcreds)
gitcreds_set()

#three choices should appear in the console



#Note: If you get an error message "invalid active developer path"
#especially after you upgrade MacOS
#open Terminal and run  
#xcode-select --install
#or sudo xcode-select --reset if the above doesn't work




####################################
#Updating R without losing packages
####################################

#credit to this post: 
#https://www.linkedin.com/pulse/3-methods-update-r-rstudio-windows-mac-woratana-ngarmtrakulchol/

#install.packages('devtools') 

library(devtools)

install_github('andreacirilloac/updateR')

library(updateR)

updateR(admin_password = 'Admin user password')

###### Moving previously installed R packages

#1. Move all folders from your old R version to new R version.

#/Library/Frameworks/R.framework/Versions/x.xx/Resources/library

#Replace x.xx with the old and new R version at a time.

#Note that you have to move only the packages that are not currently in the destination folder (because those are the base packages, and you don’t want to ruin them). But if you already did replaced everything, the next step will solve this for you.

#If you cannot find the proper path, you can run this command to check: installed.packages()

#2. Update the moved packages

#Run the following command in R. Type ‘y’ for every question that popped up.

#update.packages(checkBuilt=TRUE)

#3. Type the following command in R to check if everything went well

#version

#packageStatus()

#That’s it! 



##############################################
#Sync an existing project with a github repo
###############################################

#Open a terminal

# move to the project directory
#cd Projects/website

# initiate the upstream tracking of the project on the GitHub repo
#git remote add origin https://github.com/hansenjohnson/website.git

# pull all files from the GitHub repo (typically just readme, license, gitignore)
#git pull origin master

# set up GitHub repo to track changes on local machine
#git push -u origin master


