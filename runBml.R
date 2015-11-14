rm(list = ls())
library(dplyr)
library(animation)
library(ggplot2)
library(ggthemes)
source("BML.R")

createGrid <- createBMLGrid(200,200,.35)
runProcess <- runBMLGrid(createGrid, 50)

newBML <- function(prop, newGIF = TRUE, nSteps = 130){
	newGrid <- createBMLGrid(140,140, prop)
	newName <- paste0(c("bmlX_p2_", prop, ".gif"), collapse = "")
	runBMLGrid(newGrid, nSteps, nameVal = newName, getGIF = newGIF)
}

diffProp  <- c(.20, .35, .45, .55, .65, .80)
sapply(diffProp, newBML)

#Velocity Viz
stepCount <- 2000
velData <- lapply(diffProp, newBML, newGIF = FALSE, nSteps = stepCount)
velData <- data.frame(Steps      = rep(seq(stepCount), 6), 
		      Velocity   = unlist(velData), 
		      Proportion = rep(diffProp, each = stepCount)) 


jpeg("CVbV.jpg")
m = ggplot(velData, aes(Steps, Velocity, color = factor(Proportion)))
m + stat_smooth(size = 1.5)+scale_colour_tableau()+theme_pander()+
	labs(title = "Change in Average Velocity",colour = "Proportion")
dev.off()
