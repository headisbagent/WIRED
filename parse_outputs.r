### Comments Here ###

#####################
census.tracts.ca <- st_read(file.path('data/cb_2017_06_tract_500k','cb_2017_06_tract_500k.shp'))
# census.tracts.sd <- census.tracts.ca[census.tracts.ca$GEOID%in%paste0('0',unique(tt.sd.data$F_GEOID)),]
# census.tracts.la <- census.tracts.ca[census.tracts.ca$GEOID%in%paste0('0',unique(tt.la.data$F_GEOID)),]
# census.tracts.sf <- census.tracts.ca[census.tracts.ca$GEOID%in%paste0('0',unique(tt.sf.data$F_GEOID)),]

# census.tracts.centroids.sd <- st_centroid(census.tracts.sd)
# census.tracts.centroids.la <- st_centroid(census.tracts.la)
# census.tracts.centroids.sf <- st_centroid(census.tracts.sf)

parseResults <- function(scenario,city) {
	if(city=='sd') {
		tt.data <- tt.sd.agg
		regions <- regions.sd
		regions.centroids <- region.centroids.sd
		key <- sd.key
	} else if(city=='la') {
		tt.data <- tt.la.agg
		regions <- regions.la
		regions.centroids <- region.centroids.la
		key <- la.key
	} else if(city=='sf') {
		tt.data <- tt.sf.agg
		regions <- regions.sf
		regions.centroids <- region.centroids.sf
		key <- sf.key
	}

	energyDemand <- data.table(gdx(paste0('runFiles/',scenario,'/outputs.gdx'))['energyDemand'])

	energy <- merge(x=regions,y=energyDemand[,.(small,ENERGY=value)],by.x='new.id',by.y='small')

	stations <- data.table(gdx(paste0('runFiles/',scenario,'/outputs.gdx'))['installed'])
	stations <- stations[value>0,]
	charging <- data.table(gdx(paste0('runFiles/',scenario,'/outputs.gdx'))['chargeAmount'])
	charging <- charging[,.(CHARGING=sum(value)),by=.(i,r)]

	other <- merge(x=regions.centroids,y=stations[,.(i,new.id=r,STATIONS=value)],by='new.id')
	other <- merge(x=other,y=charging[,.(i,new.id=r,CHARGING)],by=c('i','new.id'),all.x=TRUE)

	plot.SummaryMap(regions,energy,other,scenario)
	plot.Simulation(scenario,city)
}

plot.SummaryMap <- function(regions,energy,other,scenario) {
	p.master <- ggplot()+
		geom_sf(data=regions,color='black',fill=NA)+
		geom_sf(data=energy,aes(fill=ENERGY),color='black')+
		geom_sf(data=other,aes(size=CHARGING),color='forestgreen',alpha=0.4)+
		scale_size_continuous(name='Charging Amount\n(kWh)',range=c(2,10))+
		new_scale('size')+
		geom_sf(data=other,aes(size=factor(STATIONS),color=i),alpha=0.8)+
		scale_size_discrete(name='# of\nCharger Plugs',range=c(.5,2))+
		scale_color_brewer(name='Station Type',palette='Set1')+
		coord_sf()+
		scale_fill_viridis_c(name='Energy Demand\n(kWh)',option='plasma',trans='sqrt')+
		theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),legend.direction='vertical',legend.box='horizontal',legend.position='none')

	p.energy <- ggplot()+
		geom_sf(data=energy,aes(fill=ENERGY),color='black')+
		scale_fill_viridis_c(name='Energy Demand\n(kWh)',option='plasma',trans='sqrt')+
		coord_sf()+
		theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),legend.direction='vertical',legend.box='horizontal')

	p.charging <- ggplot()+
		geom_sf(data=other,aes(size=CHARGING),color='forestgreen',alpha=0.4)+
		scale_size_continuous(name='Charging Amount\n(kWh)',range=c(2,10))+
		theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),legend.direction='vertical',legend.box='horizontal')

	p.stationSize <- ggplot()+
		geom_sf(data=other,aes(size=factor(STATIONS)),alpha=0.8)+
		scale_size_discrete(name='# of\nCharger Plugs',range=c(.5,2))+
		coord_sf()+
		theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),legend.direction='vertical',legend.box='horizontal')

	p.stationColor <- ggplot()+
		geom_sf(data=other,aes(color=i),alpha=0.8)+
		scale_color_brewer(name='Station Type',palette='Set1')+
		coord_sf()+
		theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),legend.direction='vertical',legend.box='horizontal')

	leg.energy <- get_legend(p.energy)
	leg.charging <- get_legend(p.charging)
	leg.stationSize <- get_legend(p.stationSize)
	leg.stationColor <- get_legend(p.stationColor)

	blank_p <- plot_spacer()+theme_void()

	leg12 <- plot_grid(blank_p,leg.energy,leg.charging,blank_p,nrow=4)
	leg34 <- plot_grid(blank_p,leg.stationSize,leg.stationColor,blank_p,nrow=4)
	leg.comb <- plot_grid(leg12,leg34,ncol=2)

	plotSave <- plot_grid(p.master,leg.comb,nrow=1,align='h',axis='t',rel_widths=c(.7,.15))
	ggsave(plotSave,file=paste0('figures/',scenario,'/summary_map.pdf'),height=7,width=11)
	ggsave(plotSave,file=paste0('figures/',scenario,'/summary_map.png'),height=7,width=11)
}

plot.Simulation <- function(scenario,city) {
	if(city=='sd') {
		regions <- regions.sd
		regions.centroids <- region.centroids.sd
		key <- sd.key
	} else if(city=='la') {
		regions <- regions.la
		regions.centroids <- region.centroids.la
		key <- la.key
	} else if(city=='sf') {
		regions <- regions.sf
		regions.centroids <- region.centroids.sf
		key <- sf.key
	}

	full <- fread(paste0('runFiles/',scenario,'/dailySimulations.csv'))

	forPlot.animate <- merge(x=full[,.(new.id=small,ENERGY=value,day)],y=regions,by='new.id')
	st_geometry(forPlot.animate) <- forPlot.animate$geometry

	# print('Beginning animation plotting ...')

	# plotSave.animation <- ggplot()+
	# 	geom_sf(data=regions,color='black',fill=NA)+
	# 	geom_sf(data=forPlot.animate,aes(fill=ENERGY),color='black')+
	# 	scale_fill_viridis_c(name='Energy Demand\n(kWh)',option='plasma',trans='sqrt')+
	# 	coord_sf()+
	# 	theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank())+
	# 	labs(title='Simulation Day: {frame_time}')+
	# 	transition_time(day)
	# anim_save(plotSave.animation,file=paste0('figures/',scenario,'/dailyDemand_90day_simulation.gif'),height=600,width=900,nframes=90)

	output <- full[,.(value=mean(value)),by=small]
	forPlot.mean <- merge(x=output[,.(new.id=small,ENERGY=value)],y=regions,by='new.id')
	st_geometry(forPlot.mean) <- forPlot.mean$geometry

	plotSave.mean <- ggplot()+
		geom_sf(data=regions,color='black',fill=NA)+
		geom_sf(data=forPlot.mean,aes(fill=ENERGY),color='black')+
		scale_fill_viridis_c(name='Energy Demand\n(kWh)',option='plasma',trans='sqrt')+
		coord_sf()+
		theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank())
	ggsave(plotSave.mean,file=paste0('figures/',scenario,'/dailyDemand_avgDay_simulation.pdf'),height=6,width=9)
	ggsave(plotSave.mean,file=paste0('figures/',scenario,'/dailyDemand_avgDay_simulation.png'),height=6,width=9)
}

parse.allResults <- function() {
	scenarios <- list.files('runFiles')
	for(s in scenarios) {
		if('outputs.gdx'%in%list.files(paste0('runFiles/',s))&length(list.files(paste0('figures/',s)))<5) {
			print(paste0('Beginning ',s))
			city <- substring(s,1,2)
			parseResults(s,city)
		}
	}
}

extractScenario.stats <- function(city,numVeh,w1,w2,hc) {
	stationCost <- data.table(gdx(paste0('runFiles/',city,'_veh',numVeh,'_w1-',w1,'_w2-',w2,hc,'/outputs.gdx'))['stationCost'])
	chrgRate <-data.table(gdx(paste0('runFiles/',city,'_veh',numVeh,'_w1-',w1,'_w2-',w2,hc,'/outputs.gdx'))['chargeRate'])
	stations <- data.table(gdx(paste0('runFiles/',city,'_veh',numVeh,'_w1-',w1,'_w2-',w2,hc,'/outputs.gdx'))['installed'])
	chargeAmount <- data.table(gdx(paste0('runFiles/',city,'_veh',numVeh,'_w1-',w1,'_w2-',w2,hc,'/outputs.gdx'))['chargeAmount'])
	tt <- data.table(gdx(paste0('runFiles/',city,'_veh',numVeh,'_w1-',w1,'_w2-',w2,hc,'/outputs.gdx'))['travelTime'])

	if(w1=='10') {
		w1.index <- 'high'
	}else if(w1=='001') {
		w1.index <- 'low'
	}

	if(w2=='1000') {
		w2.index <- 'low'
	}else if(w2=='1e+05') {
		w2.index <- 'high'
	}

	if(hc=='') {
		hc.index <- 'no'
	}else if(hc=='_hc') {
		hc.index <- 'yes'
	}

	avg.tt <- merge(x=chargeAmount[,.(kWh=sum(value)),by=.(r,small)],y=tt[,.(small,r,tt=value)],by=c('r','small'))
	avg.tt <- avg.tt[,.(tt.kwh=sum(tt*kWh)/sum(kWh))]
	avg.chrgRate <- merge(x=chargeAmount[,.(kWh=sum(value)),by=i],y=chrgRate,by='i')
	avg.chrgRate <- avg.chrgRate[,.(chrgRate=sum(kWh*value)/sum(kWh))]
	tot.stationCost <- merge(x=stations[,.(stations=sum(value)),by=i],y=stationCost,by='i')
	tot.stationCost <- tot.stationCost[,.(cost=sum(stations*value))]
	tot.stations <- stations[,.(stations=sum(value)),by=i]

	full.scenario <- data.table(city=city,veh=numVeh,w1=w1.index,w2=w2.index,hc=hc.index,avgtt=avg.tt,avgchrgrte=avg.chrgRate,totcost=tot.stationCost,l1Count=tot.stations$stations[tot.stations$i=='L1'],l2Count=tot.stations$stations[tot.stations$i=='L2'],dcCount=tot.stations$stations[tot.stations$i=='DC'])

	return(full.scenario)
}

extractAll.scenarios <- function() {
	output <- data.table()
	for(city in c('sd','la','sf')){
		for(numVeh in c(100,1000,10000)){
			for(w1 in c('10','001')) {
				for(w2 in c('1000','1e+05')) {
					for(hc in c('','_hc')) {
						hold <- extractScenario.stats(city,numVeh,w1,w2,hc)
						output <- rbind(output,hold,fill=TRUE)
					}
				}
			}
		}
	}
	return(output)
}

all.scenarios <- extractAll.scenarios()
all.scenarios <- all.scenarios[complete.cases(all.scenarios)]
all.scenarios[,avgcost:=totcost.cost/(l1Count+l2Count+dcCount)]

forPlot <- all.scenarios[hc=='no',]
forPlot[,w1:=ifelse(w1=='high','TT(+)','TT(-)')]
forPlot[,w2:=ifelse(w2=='high','ChrgRte(+)','ChrgRte(-)')]
forPlot[,weights:=paste(w1,w2,sep=',')]
forPlot[,totalChargers:=l1Count+l2Count+dcCount]

plotSave <- ggplot(data=forPlot,aes(x=totalChargers,y=avgtt.tt.kwh,size=totcost.cost/10^6,color=city,shape=weights))+
	geom_point()+
	xlab('Number of Chargers')+
	ylab('Average travel time to charger (min)')+
	scale_size_continuous(name='Total Cost\n(millions of $)')+
	scale_color_brewer(name='City',palette='Set1')+
	theme_bw()
ggsave(plotSave,file='figures/cost_summary.pdf',height=6,width=9)
ggsave(plotSave,file='figures/cost_summary.png',height=6,width=9)

plotSave <- ggplot(data=all.scenarios,aes(x=hc,y=totcost.cost/10^6))+
	geom_boxplot()+
	xlab('Maximize overnight charging')+
	ylab('Total public infrastructure cost (millions of $)')+
	theme_bw()
ggsave(plotSave,file='figures/overnight_comparison.pdf',height=5.5,width=4)
ggsave(plotSave,file='figures/overnight_comparison.png',height=5.5,width=4)