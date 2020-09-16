lyft <- fread('data/AlanJennFinal.csv')
lyft[,time_parsed:=ymd_hms(pickup_timestamp)]
lyft[,day:=as.Date(time_parsed)]
lyft[,minute.start:=hour(time_parsed)*60+minute(time_parsed)]
lyft[,minute.end:=minute.start+ceiling(distance_miles)]
lyft[,energy:=distance_miles*.3]
lyft[,minutes.travel:=minute.end-minute.start+1]
lyft[,hour.start:=hour(time_parsed)+1]

tt.sd.data <- fread('data/sd_counties_tract_travel_times.csv')
tt.sd.data[,from_census_tract:=as.numeric(substr(as.character(F_GEOID),5,10))]
tt.sd.data[,to_census_tract:=as.numeric(substr(as.character(T_GEOID),5,10))]
tt.sd.data[tt_minutes_network<1,tt_minutes_network:=1]

tt.la.data <- fread('data/la_counties_tract_travel_times.csv')
tt.la.data[,from_census_tract:=as.numeric(substr(as.character(F_GEOID),5,10))]
tt.la.data[,to_census_tract:=as.numeric(substr(as.character(T_GEOID),5,10))]
tt.la.data[tt_minutes_network<1,tt_minutes_network:=1]
tt.la.data <- tt.la.data[!(F_GEOID%in%as.integer64(c(6037599100,6111980000,6083980100,6111003612,6037599000))|T_GEOID%in%as.integer64(c(6037599100,6111980000,6083980100,6111003612,6037599000)))]

tt.sf.data <- fread('data/sf_counties_tract_travel_times.csv')
tt.sf.data[,from_census_tract:=as.numeric(substr(as.character(F_GEOID),5,10))]
tt.sf.data[,to_census_tract:=as.numeric(substr(as.character(T_GEOID),5,10))]
tt.sf.data[tt_minutes_network<1,tt_minutes_network:=1]
tt.sf.data <- tt.sf.data[!(F_GEOID%in%as.integer64(6075980401)|T_GEOID%in%as.integer64(6075980401))]

census.tracts.ca <- st_read(file.path('data/cb_2017_06_tract_500k','cb_2017_06_tract_500k.shp'))

aggregateRegions <- function(input) {
	dist <- input[,.(Origin=F_GEOID,Destination=T_GEOID,Distance=euclidean_distance_m)]
	dist <- dcast.data.table(dist,Origin~Destination,value.var='Distance')
	row.names(dist) <- dist$Origin
	dist[,Origin:=NULL]

	dist.matrix <- as.dist(dist,diag=TRUE)
	cluster.results <- hclust(dist.matrix)
	memb <- cutree(cluster.results,k=500)
	membership <- data.table(GEOID=as.integer64(as.numeric(names(memb))),new.id=memb)

	tt.agg <- merge(x=input,y=membership[,.(GEOID=GEOID,F_new.id=new.id)],by.x='F_GEOID',by.y='GEOID')
	tt.agg <- merge(x=tt.agg,y=membership[,.(GEOID=GEOID,T_new.id=new.id)],by.x='T_GEOID',by.y='GEOID')

	key <- unique(tt.agg[,.(GEOID=F_GEOID,pickup_census_tract=from_census_tract,new.id=F_new.id)])

	tt.agg <- tt.agg[,.(tt_minutes_network=mean(tt_minutes_network)),by=.(F_new.id,T_new.id)]

	new.region.geo <- census.tracts.ca[census.tracts.ca$GEOID%in%paste0('0',unique(key$GEOID)),]
	new.region.geo <- merge(x=new.region.geo,y=membership[,.(GEOID=paste0('0',GEOID),new.id)],by='GEOID')
	new.region.geo <- group_by(new.region.geo,new.id) %>%
		summarise(do_union=TRUE)

	return(list(tt.agg,key,new.region.geo))
}

sd.data <- aggregateRegions(tt.sd.data)
la.data <- aggregateRegions(tt.la.data)
sf.data <- aggregateRegions(tt.sf.data)

tt.sd.agg <- sd.data[[1]]
tt.la.agg <- la.data[[1]]
tt.sf.agg <- sf.data[[1]]

sd.key <- sd.data[[2]]
la.key <- la.data[[2]]
sf.key <- sf.data[[2]]

regions.sd <- sd.data[[3]]
regions.la <- la.data[[3]]
regions.sf <- sf.data[[3]]

region.centroids.sd <- st_centroid(regions.sd)
region.centroids.la <- st_centroid(regions.la)
region.centroids.sf <- st_centroid(regions.sf)

lyft.ev.sd <- lyft[is_ev==1&pickup_census_tract%in%unique(sd.key$pickup_census_tract)]
lyft.ev.la <- lyft[is_ev==1&pickup_census_tract%in%unique(la.key$pickup_census_tract)]
lyft.ev.sf <- lyft[is_ev==1&pickup_census_tract%in%unique(sf.key$pickup_census_tract)]

prep.inputs.traveltime <- function(numVehicles,scenario,homeCharging,city) {
	if(city=='sd') {
		tt.data <- tt.sd.agg
	} else if(city=='la') {
		tt.data <- tt.la.agg
	} else if(city=='sf') {
		tt.data <- tt.sf.agg
	}

	inputs <- list()
	inputs$sets <- list()
	inputs$parameters <- list()

	inputs$sets$r <- unique(tt.data$F_new.id)
		
	travel.simulations <- dailyTravel.simulations(numVehicles,scenario,homeCharging,city)
	inputs$parameters$energyDemand <- travel.simulations
	inputs$sets$small <- unique(inputs$parameters$energyDemand$small)

	travelTime <- data.table(small=tt.data[,F_new.id],r=tt.data[,T_new.id],value=tt.data[,tt_minutes_network])
	inputs$parameters$travelTime <- travelTime[small%in%inputs$sets$small,]

	return(inputs)
}

dailyTravel.simulations <- function(numVehicles,scenario,homeCharging,city) {
	if(city=='sd') {
		tt.data <- tt.sd.agg
		lyft.ev <- merge(x=lyft.ev.sd,y=sd.key,by='pickup_census_tract')
		regions <- regions.sd
		key <- sd.key
	} else if(city=='la') {
		tt.data <- tt.la.agg
		lyft.ev <- merge(x=lyft.ev.la,y=la.key,by='pickup_census_tract')
		regions <- regions.la
		key <- la.key
	} else if(city=='sf') {
		tt.data <- tt.sf.agg
		lyft.ev <- merge(x=lyft.ev.sf,y=sf.key,by='pickup_census_tract')
		regions <- regions.sf
		key <- sf.key
	}

	full <- data.table()
	for(d in 1:90) {
		hold <- lyft.ev[sample(1:nrow(lyft.ev),size=numVehicles,replace=TRUE),.(id,day)]
		hold[,index:=1:nrow(hold)]
		hold <- merge(x=hold,y=lyft.ev[,.(new.id,minute.start,minute.end,minutes.travel,energy,id,day,hour.start)],by=c('id','day'))
		if(homeCharging==TRUE) {
			hold <- homeCharging.procedure(hold)
		}
		hold <- hold[,.(value=sum(energy)),by=new.id]
		hold[,day:=d]
		full <- rbind(full,hold)
	}
	names(full) <- c('small','value','day')
	fwrite(full,paste0('runFiles/',scenario,'/dailySimulations.csv'),row.names=FALSE)
	output <- full[,.(value=mean(value)),by=small]
	return(output)
}

homeCharging.procedure <- function(input) {
	input[,total.energy:=cumsum(energy),by=index]
	input[total.energy<48,energy:=0]
	input[,total.energy:=NULL]
	return(input)
}