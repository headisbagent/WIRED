$title tncinfrastructure

$offlisting
options
	limrow = 0,
	limcol = 0,
	solprint = off,
	sysout = off,
	profile = 1
	;

sets
	i 		Station type
	j		Vehicle index
	t 		Time period
	r 		Region of travel
	;

alias(r,o);

parameters
	travelTime(r,o)
	existingStations(i,r)
	chargeRate(i)
	energyDemand(j)
	stationCost(i)
	travelIndicator(j,r,t)
	;

scalar
	weight /.05/
	;

positive variable
	locationDiff(j,r,o,t)
	occupied(j,i,t,r) #binary
	location(j,r,t) #binary
	installed(i,r) #integer
	;

variable
	downtime
	;

$gdxin inputs
$load i j t r travelTime existingStations chargeRate energyDemand stationCost travelIndicator
$gdxin

equations
	obj					Objective function minimizing downtime of drivers
	linearization1		Linearizes product of binary variables
	linearization2		Linearizes product of binary variables
	linearization3		Linearizes product of binary variables
	constraint1			Vehicle must occupy a location
	constraint2 		Vehicle travel location must match
	constraint3 		Vehicle charge location must match
	constraint4			Cannot charge and travel at the same time
	constraint5			Each vehicle can only occupy one charger in a given time period
	constraint6 		Vehicle can only occupy existing chargers
	constraint7 		Daily charging demand must be fulfilled
	;

*Variable limits
	locationDiff.up(j,r,o,t) = 1;
	locationDiff.lo(j,r,o,t) = 0;
	location.up(j,r,t) = 1;
	occupied.up(j,i,t,r) = 1;

obj..
	downtime =e= sum((j,i,t,r),occupied(j,i,t,r))+sum((j,r,o,t),locationDiff(j,r,o,t)*travelTime(r,o))+weight*sum((i,r),installed(i,r)*stationCost(i));

linearization1(j,r,o,t)..
	locationDiff(j,r,o,t)-location(j,r,t)-location(j,o,t-1)+1 =g= 0;

linearization2(j,r,o,t)..
	locationDiff(j,r,o,t)-location(j,r,t) =l= 0;

linearization3(j,r,o,t)..
	locationDiff(j,r,o,t)-location(j,o,t-1) =l= 0;

constraint1(j,t)..
	sum(r,location(j,r,t))-1 =e= 0;

constraint2(j,r,t)..
	location(j,r,t)-travelIndicator(j,r,t) =g= 0;	

constraint3(j,r,t)..
	location(j,r,t)-sum(i,occupied(j,i,t,r)) =g= 0;

constraint4(j,r,t)..
	sum(i,occupied(j,i,t,r))+travelIndicator(j,r,t)-1 =l= 0;

constraint5(j,t)..
	sum((i,r),occupied(j,i,t,r))-1 =l= 0;

constraint6(i,t,r)..
	sum(j,occupied(j,i,t,r))-installed(i,r)+existingStations(i,r) =l= 0;

constraint7(j)..
	sum((i,t,r),occupied(j,i,t,r)*chargeRate(i))-energyDemand(j) =g= 0;

model
	tncinfrastructure /obj,linearization1,linearization2,linearization3,constraint1,constraint2,constraint3,constraint4,constraint5,constraint6,constraint7/
	;

options
	mip = cplex
	solvelink = 0
	reslim = 50000
	;

$onecho > cplex.opt
workmem 32000
aggind 10
names no
memoryemphasis 1
threads=4
$offecho

tncinfrastructure.optFile = 1;
tncinfrastructure.holdfixed = 1;

solve
	tncinfrastructure
	using mip
	minimizing downtime
	;

display
	downtime.l
	occupied.l
	installed.l
	location.l
	;

Execute_Unload "outputs.gdx"
