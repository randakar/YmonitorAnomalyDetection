#' Integrating Anomaly Detection with Ymonitor RDEC (https://www.ymonitor.nl/rdec)
#'
#'
#' @seealso \code{\link{AnomalyDetectionTs}}
#' @export
#'

AnomalyDetectionInRDECCSV <- function(input_csv = 'example.csv', max_anoms = 0.10, direction = 'pos',
                               alpha = 0.05, only_last = NULL, threshold = 'None',
                               e_value = FALSE, longterm = FALSE, piecewise_median_period_weeks = 2, plot = FALSE,
                               y_log = FALSE, xlabel = '', ylabel = 'count',
                               title = NULL, verbose=FALSE, na.rm = FALSE){
	
	## Read datafile (downloaded from RDEC)
	inputdata <- read.table(input_csv, header = TRUE, sep = "," , dec = "." , colClasses = "character", comment.char = "")
	
	return ( AnomalyDetectionRDECwrapper(inputdata, max_anoms, direction, alpha, only_last, threshold, e_value, longterm, piecewise_median_period_weeks, plot, y_log, xlabel, ylabel, title, verbose, na.rm) )
}


	
AnomalyDetectionRDECwrapper <- function(inputdata, max_anoms = 0.10, direction = 'pos',
                               alpha = 0.05, only_last = NULL, threshold = 'None',
                               e_value = FALSE, longterm = FALSE, piecewise_median_period_weeks = 2, plot = FALSE,
                               y_log = FALSE, xlabel = '', ylabel = 'count',
                               title = NULL, verbose=FALSE, na.rm = FALSE){

	print(inputdata$class);
	
	## Rename columns
	names(inputdata)[names(inputdata)=="Date"] <- "timestamp"
	names(inputdata)[names(inputdata)=="Duration"] <- "count"

	## Filter unwanted columns and reorder them
	inputdata <- subset(inputdata, select=c("timestamp", "count"))

	## Clean up the data itself
	#inputdata$timestamp <- strptime(mydata$timestamp,"%Y-%m-%d %H:%M:%S")
	inputdata$timestamp <- as.POSIXlt(inputdata$timestamp,format='%d/%m/%Y %H:%M:%S')
	inputdata$count <- as.numeric(inputdata$count)
	
	## Debugging
	##print(inputdata);
	
	return ( AnomalyDetectionTs(inputdata, max_anoms, direction, alpha, only_last, threshold, e_value, longterm, piecewise_median_period_weeks, plot, y_log, xlabel, ylabel, title, verbose, na.rm) )
}
