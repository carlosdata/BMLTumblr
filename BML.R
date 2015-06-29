createBMLGrid   <- function(r, c,p){
  	mapAxis <- expand.grid( "rAxis" = seq(r), "cAxis" = seq(c))
  
	N         <- round(r*c*p)
	carSample <- sample( c('red', 'blue'),N, replace = T) %>% data.frame() 
    	carIndex  <-  sample(nrow(mapAxis), N) 
  	
	names(carSample) <- 'carType'
	mainCol          <- c('rAxis', 'cAxis')
	carDataFrame     <-  as.matrix(mapAxis[carIndex, mainCol])

	finalList        <-  list(indMatrix = carDataFrame, cars = carSample, r = r, c = c)
 	class(finalList) <-  'BMLdata'
  	finalList
}




moveDot    <- function(mainMatrix, carColor, carList, mainColumn, wrapTest){
	carIndex <-  which(carList  == carColor)
  
  	dotVal   <- getTestFrame(mainMatrix, carIndex, mainColumn, wrapTest)
  
  	blockedDot <- frontTest(mainMatrix, dotVal[carIndex,])
  	moveIndex  <-  carIndex[!blockedDot]
 	 mainMatrix[moveIndex, mainColumn] <- dotVal[moveIndex, mainColumn]
  	list(updatedMatrix = mainMatrix, moveVelocity = mean(!blockedDot))
}


frontTest <- function(origMatrix, movedMatrix){
  	bindMatrix  <- rbind(origMatrix, movedMatrix)
  	findBlocked <-  duplicated(bindMatrix)
  	findBlocked[ -(1:nrow(origMatrix)) ]
}

getTestFrame <- function(newMatrix, carInd, testCol, wrapVal){
  	newMatrix[carInd, testCol]    <- newMatrix[carInd, testCol] + 1
  	wrapIndex <-  newMatrix[, testCol] > wrapVal
  	newMatrix[wrapIndex, testCol] <-  1
  	newMatrix
}



runBMLGrid  <- function(someList, steps = 10, nameVal = "bml.gif", speedVal = 0.05, 
			getGIF = TRUE){
	if(getGIF){
		saveGIF(loopBML(someList, steps, getGIF), movie.name = nameVal, 
			interval = speedVal)
		return(invisible(NULL))
	}else{
		runValue <- loopBML(someList, steps, getGIF)	
		return(runValue)
	}
}


loopBML <- function(carValues, numSteps, getPlot){
	carMatrix <- carValues$indMatrix
	carList   <- carValues$cars
  	velocity  <-  c()
	  
  	for(i in seq(numSteps)){
    		if(i %% 2)
      			carMatrix <- moveDot(carMatrix, 'blue', carList, 'rAxis', carValues$r)
    		else
      			carMatrix <- moveDot(carMatrix, 'red', carList, 'cAxis', carValues$c)
  
    		velocity  <- c(velocity, carMatrix$moveVelocity)
    		carMatrix <- carMatrix$updatedMatrix
  		if(getPlot)
			plotBML(carMatrix, carList, carValues$r, carValues$c)
	}
 	return(velocity) 	
}	


plotBML <- function(newValues, carColor, gridR, gridC){
  	gridMap <- rep(0, gridR*gridC) %>% matrix(ncol = gridC)
  	gridMap[newValues] <-  c(carColor[,1])
	#par()$mar 5.1 4.1 4.1 2.1
	par(mar=c(0.5, 0.5, 0.5, 0.5)) 
	image(t(gridMap), col = c('white', 'blue', 'red'), axes= FALSE)
}


plotVelocity  <- function(ind, movedVelocity, len){
	tempLabel <- paste('Time-Steps: ', ind - 1)
    
	plot(seq(ind), movedVelocity[seq(ind)], col = c('red', 'blue'),
       	pch = '*', ylim = c(0,1), xlim = c(0,len),
       	ylab = 'Velocity Proportion', xlab = tempLabel)
}


