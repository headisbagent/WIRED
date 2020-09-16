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
	i 			Station type
	r 			Region of travel
	small		Subset of regions for energy demand
	;

parameters
	travelTime(small,r)
	existingStations(i,r)
	chargeRate(i)
	energyDemand(small)
	stationCost(i)
	chargePrice(i)
	;

scalar
	weight1 /w1Here/
	weight2 /w2Here/
	numVeh /substituteHere/
	;

integer variable
	installed(i,r)
	;

positive variable
	chargeAmount(i,r,small)
	;

variable
	totalCost
	;

$gdxin inputs
$load i r small travelTime existingStations chargeRate energyDemand stationCost chargePrice
$gdxin

equations
	obj					Objective function minimizing cost and time
	constraint1			Total charging demand must be fulfilled
	constraint2 		Charging in each period cannot exceed charging capacity
	constraint3			Allocate charging to original demand locations
	;

*Variable limits

obj..
	totalCost =e= sum((i,r),installed(i,r)*stationCost(i))+sum((i,r,small),chargeAmount(i,r,small)*chargePrice(i))+weight1*sum((small,r,i),energyDemand(small)*travelTime(small,r)*chargeAmount(i,r,small))+weight2*sum((i,r,small),chargeAmount(i,r,small)/chargeRate(i));

constraint1..
	sum((i,r,small),chargeAmount(i,r,small))-sum((small),energyDemand(small)) =g= 0;

constraint2(i,r)..
	(installed(i,r)+existingStations(i,r))*chargeRate(i)*12-sum(small,chargeAmount(i,r,small)) =g= 0;

constraint3(small)..
	sum((i,r),chargeAmount(i,r,small))-energyDemand(small) =g= 0;	

model
	tncinfrastructure /obj,constraint1,constraint2,constraint3/
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
	minimizing totalCost
	;

display
	installed.l
	chargeAmount.l
	;

Execute_Unload "outputs.gdx"
