##Data downloading=group
##year=number 2016
##month=selection;1;2;3;4;5;6;7;8;9;10;11;12
##day=selection;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31
##hour=selection;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24
##Output=output vector

library(XML)
library(sp)
library(rgdal)

Sys.setlocale(locale="Japanese_Japan.932")



jma_list <- data.frame(
prec_no = c(44, 45, 45, 45, 45, 46, 43, 43),
block_no = c(47662, 47682, 47648, 47672, 47674, 47670, 47641, 47626),
name = c("“Œ‹ž", "ç—t", "’¶Žq", "ŠÚŽR", "Ÿ‰Y", "‰¡•l", "’•ƒ", "ŒF’J"),
lon = c(139.75, 140.1033, 140.8567, 139.865, 140.3117, 139.6517, 139.0733, 139.38),
lat = c(35.69167, 35.60167, 35.73833, 34.98667, 35.15, 35.43833, 35.99, 36.15))



header.text <- c("time", "p", "p_sea", "prcp", "t", "due", "vpr_p",
                 "humid", "w_spd", "w_dir", "sun_hrs",
                 "rad", "s_fall", "s_ac", "weather", "clowd", "view")

direction.list <- c("–k", "–k–k“Œ", "–k“Œ", "“Œ–k“Œ",
                    "“Œ", "“Œ“ì“Œ", "“ì“Œ", "“ì“ì“Œ",
                    "“ì", "“ì“ì¼", "“ì¼", "¼“ì¼",
                    "¼", "¼–k¼", "–k¼", "–k–k¼")

					
n.aws <- nrow(jma_list)			
n.header <- length(header.text)
					
					
parse2df <- function(prec_no, block_no, year, month, day, hour){

url <- paste0("http://www.data.jma.go.jp/obd/stats/etrn/view/hourly_s1.php?",
              "prec_no=", prec_no, "&",
              "block_no=", block_no, "&",
              "year=", year, "&",
              "month=", month, "&",
              "day=", day)

doc = htmlParse(url)
tableNodes = getNodeSet(doc, "//table")
d = readHTMLTable(tableNodes[[5]], skip=1)
colnames(d) <- header.text

# Data cleaning for processing
d[,1] <- as.POSIXct(paste(year, month, day, as.character(d[,1])), format="%Y %m %d %H")
d[,2] <- as.numeric(as.character(d[,2]))
d[,3] <- as.numeric(as.character(d[,3]))
d[,4] <- as.numeric(as.character(sub("--", 0, d[,4])))
d[,5] <- as.numeric(as.character(d[,5]))
d[,6] <- as.numeric(as.character(d[,6]))
d[,7] <- as.numeric(as.character(d[,7]))
d[,8] <- as.numeric(as.character(d[,8]))
d[,9] <- as.numeric(as.character(d[,9]))

d[,10] <- factor(d[,10], levels = direction.list)
d[,11] <- as.numeric(as.character(d[,11]))
d[,12] <- as.numeric(as.character(d[,12]))

d[,13] <- as.numeric(as.character(sub("~", NA, sub("--", 0, d[,13]))))
d[,14] <- as.numeric(as.character(sub("~", NA, sub("--", 0, d[,14]))))
d[,15] <- as.character(d[,15])
d[,16] <- as.numeric(as.character(d[,16]))
d[,17] <- as.numeric(as.character(d[,17]))

# Conversion from text to clockwise degree from North.
d[,10] <- (as.numeric(d[,10]) - 1) * 22.5

d <- d[hour,]

return(d)
}


tmp.d <- as.data.frame(matrix(NA, n.aws, n.header))
colnames(tmp.d) <- header.text
tmp.d[,1] <- as.POSIXct(tmp.d[,1])


for(i in 1:n.aws){
  tmp.d[i,] <- parse2df(jma_list$prec_no[i], jma_list$block_no[i], year, month, day, hour)
}

sp.dfm <- cbind(jma_list, tmp.d)


pj <- CRS("+proj=longlat +datum=WGS84")

sp <- SpatialPoints(jma_list[,4:5], proj4string=pj)
Output <- SpatialPointsDataFrame(sp,data=sp.dfm)
