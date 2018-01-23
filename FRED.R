# ========================================================================== #
# FRED
# ========================================================================== #

# You must install.packages("rjson") and get your own API key (a 32 char
# string) by registering an account on:
# https://research.stlouisfed.org/useraccount/register

.FRED_api_key <- "your_api_key"

# -------------------------------------------------------------------------- #
# qFRED
# -------------------------------------------------------------------------- #

# Retrieves times series from FRED as matrices.

# sid     A single & valid series ID given as string
# from    Start date as Date object or "YYYY-MM-DD" formatted string.
#         Defaults to 1776-07-04.
# to      End date as Date object or "YYYY-MM-DD" formatted string.
#         Defaults to Sys.Date().
# units   String. One of:
#         lin ... Levels (Default)
#         chg ... Change
#         ch1 ... Change from Year Ago
#         pch ... Percent Change
#         pc1 ... Percent Change from Year Ago
#         pca ... Compounded Annual Rate of Change
#         cch ... Continuously Compounded Rate of Change
#         cca ... Continuously Compounded Annual Rate of Change
#         log ... Natural Log
# freq    Frequency as string. One of:
#         d ..... Daily (Default)
#         w ..... Weekly
#         bw .... Bi-Weekly
#         m ..... Monthly
#         q ..... Quarterly
#         sa .... Semiannual
#         a ..... Annual
#         wef ... Weekly, Ending Friday
#         weth .. Weekly, Ending Thursday
#         wew ... Weekly, Ending Wednesday
#         wetu .. Weekly, Ending Tuesday
#         wem ... Weekly, Ending Monday
#         wesu .. Weekly, Ending Sunday
#         wesa .. Weekly, Ending Saturday
#         bwew .. Bi-Weekly, Ending Wednesday
#         bwem .. Bi-Weekly, Ending Monday
# aggreg  Aggregation method (when freq is used) as string. One of:
#         eop ... End of Period (Default)
#         avg ... Average
#         sum ... Sum
# na.rm   Logical, should NAs be removed?

qFRED = function(sid, from = "1776-07-04", to = Sys.Date(), units = "lin",
	freq = "d", aggreg = "eop", na.rm = FALSE) {
	if(! "rjson" %in% .packages(1)) stop("You must install *rjson* first!")
	if(! exists(".FRED_api_key")) stop("Couln't find .FRED_api_key!")
	if(nchar(.FRED_api_key) != 32) stop("Invalid .FRED_api_key!")
	from <- try(as.Date(from), silent = 1)
	if(class(from) == "try-error") stop("Invalid date format in 'from'")
	to <- try(as.Date(to), silent = 1)
	if(class(to) == "try-error") stop("Invalid date format in 'to'")
	if(to < from) stop("to < from")
	a <- c("lin", "chg", "ch1", "pch", "pc1", "pca", "cch", "cca", "log")
	if(! units %in% a) stop("'units' must be one of: ", toString(a))
	b <- c("d", "w", "bw", "m", "q", "sa", "a", "wef", "weth", "wew",
		"wetu", "wem", "wesu", "wesa", "bwew", "bwem")
	if(! freq %in% b) stop("'freq' must be one of: ", toString(b))
	c <- c("eop", "avg", "sum")
	if(! aggreg %in% c) stop("'aggreg' must be one of: ", toString(c))
	options(warn = -1)
	tmp <- setInternet2(use = TRUE)
	options(warn = 0)
	u0 <- "https://api.stlouisfed.org/fred/series/observations?"
	u1 <- paste("series_id", sid, sep = "=")
	u2 <- paste("&api_key", .FRED_api_key, sep = "=")
	u3 <- paste("&observation_start", as.character(from), sep = "=")
	u4 <- paste("&observation_end", as.character(to), sep = "=")
	u5 <- paste("&units", units, sep = "=")
	u6 <- paste("&frequency", freq, sep = "=")
	u7 <- paste("&aggregation_method", aggreg, sep = "=")
	u8 <- "&file_type=json"
	url <- paste(u0, u1, u2, u3, u4, u5, u6, u7, u8, sep = "")	
	ans <- try(rjson:::fromJSON(file = url), silent = 1)
	if(class(ans) == "try-error") {
		stop("Could not query. Check ", sid, " is a valid ID.")
	}
	dta <- ans$observations
	if(is.null(dta)) stop("Ooops! is.null(dta)...")
	d <- sapply(dta, "[[", "date")
	tmp <- sapply(dta, "[[", "value")
	tmp[tmp == "."] <- NA
	e <- as.numeric(tmp)
	if(na.rm) {
		ix <- ! is.na(e)
		e <- e[ix]
		d <- d[ix]
	}	
	res <- as.matrix(e)
	dimnames(res) <- list(d, sid)
	return(res)
}

# -------------------------------------------------------------------------- #
# iFRED
# -------------------------------------------------------------------------- #

# Returns information for a given series ID as a list.

iFRED = function(sid) {
	if(! "rjson" %in% .packages(1)) stop("You must install *rjson* first!")
	if(! exists(".FRED_api_key")) stop("Couln't find .FRED_api_key!")
	if(nchar(.FRED_api_key) != 32) stop("Invalid .FRED_api_key!")
	u0 <- "https://api.stlouisfed.org/fred/series?"
	u1 <- paste("series_id", sid, sep = "=")
	u2 <- paste("&api_key", .FRED_api_key, sep = "=")
	u8 <- "&file_type=json"
	url <- paste(u0, u1, u2, u8, sep = "")	
	ans <- try(rjson:::fromJSON(file = url), silent = 1)
	if(class(ans) == "try-error") {
		stop("Could not query. Check ", sid, " is a valid ID.")
	}
	res <- ans$seriess[[1]]
	return(res)
}
