# Copyright 2015 Metamarkets Group, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

power_fname <- "frequencyScaling.power.log"
power_nrows <- length(readLines(power_fname)) - 12
power_table <- read.csv(power_fname, nrows = power_nrows)


timing_fname <- "frequencyScaling.test.log"
timing_table <- read.csv(timing_fname)
timing_table$StartTimeUTC <- sapply(
  timing_table$startTime,
  function(d){
    as.POSIXct(d/1000, origin="1970-01-01", tz = "GMT")
  }
)
timing_table$EndTimeUTC <- sapply(
  timing_table$stopTime,
  function(d){
    as.POSIXct(d/1000, origin="1970-01-01", tz = "GMT")
  }
)

power_table$TimeUTC <- sapply(power_table$System.Time, function(d) {
  h <- strtoi(substr(d,1,2))
  m <- strtoi(substr(d,4,5))
  s <- strtoi(substr(d,7,8))
  ms <- strtoi(substr(d,10,12))
  as.POSIXct(paste0("2015/01/22 ",d), format="%Y/%m/%d %H:%M:%OS", tz="America/Los_Angeles")
}
)


prior_benchmark_fname <- "frequencyScaling.control.log"
prior_benchmark <- read.csv(prior_benchmark_fname)

prior_benchmark <- prior_benchmark[,'timeMS']


getLowerIndex <- function(time){
  which.min(power_table$TimeUTC < time) - 1
}
getUpperIndex <- function(time){
  which.max(power_table$TimeUTC >= time)
}

timing_table$consumedCycles <- apply(
  timing_table[,c('StartTimeUTC','EndTimeUTC')], 1,
  function(r){
    startTime <- r['StartTimeUTC']
    endTime <- r['EndTimeUTC']
    ixl <- getLowerIndex(startTime)
    ixu <- getUpperIndex(endTime)
    cycles <- 0
    if(ixu - ixl > 2){
      secs <- c( startTime, power_table$TimeUTC[(ixl + 1):(ixu - 1)], endTime)
      mhz <- c(power_table$CPU.Frequency_0.MHz.[ixl], power_table$CPU.Frequency_0.MHz.[(ixl + 1):(ixu - 1)], power_table$CPU.Frequency_0.MHz.[ixu -1])
    }else{
      secs <- c(startTime, endTime)
      mhz <- c(power_table$CPU.Frequency_0.MHz.[ixl], power_table$CPU.Frequency_0.MHz.[ixl])
    }
    idx <- 2:length(secs)
    result <- as.double( (secs[idx] - secs[idx - 1]) %*% (mhz[idx] + mhz[idx - 1]))/2
    result * 1e6
  }
)

norm_prior <- prior_benchmark / median(prior_benchmark)
norm_raw <- timing_table$elapsedTime / median(timing_table$elapsedTime)
norm_adjusted <- timing_table$consumedCycles / median(timing_table$consumedCycles)

norm_prior_density <- density(norm_prior)

ymax <- 6
xmin <- 0.5
pointMax <- round(max(norm_prior_density$y))
plot(1, type="n", main = "Query Time Density", xlim = c(xmin,1.5), ylim = c(0, ymax), ylab="Density", xlab="Normalized Wall Time")
lines(norm_prior_density, col="red", lwd=5)
lines(density(norm_raw), col="blue", lwd=5)
lines(density(norm_adjusted), col="darkgreen", lwd=5)
legend(xmin,ymax, col=c("red", "blue", "darkgreen"), legend=c("EC2", "MBP Raw", "MBP Adjusted"), lwd=5)
arrows(1.08,5, y1=6.2, lwd=5)
text(1.11,5.1,labels=paste0("max = ",pointMax), srt=90)

summary_raw <- summary(norm_raw)
summary_adjusted <- summary(norm_adjusted)

var_raw <- var(norm_raw)
var_adjusted <- var(norm_adjusted)

xlims <- c(min(power_table$TimeUTC),max(power_table$TimeUTC))

class(power_table$TimeUTC) <- 'POSIXct'
plot(power_table$TimeUTC, power_table$CPU.Frequency_0.MHz., xaxt="n", xlab="Test timestamp (M:S)", ylab="CPU frequency (MHz)", xlim=xlims)
axis.POSIXct(1,power_table$TimeUTC)
title("Query Performance Vs CPU Frequency")
par(new=T)
plot((timing_table$StartTimeUTC + timing_table$EndTimeUTC)/2, timing_table$elapsedTime, type="p", col="red",pch=16, axes=F, xlab="", ylab="", xlim=xlims)
par(new=F)