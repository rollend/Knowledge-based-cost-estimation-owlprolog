buildingcomponents(ifcwall_15478).
buildingcomponents(wall2).
buildingcomponents(finishes1).
buildingcomponents(finishes2).
buildingcomponents(door1).
buildingcomponents(window1).
material(bricks).
material(ceramic_tile).
units(area,square_meter).
units(length,meter).
units(length,millimeter).
has_material(ifcwall_15478,bricks).
has_material(finishes1,ceramic_tile).
has_property(bricks, dimension, 240*240).
has_property(ceramic_tile, size, 0.33*0.33).
has_property(window1,height,1.3).
has_property(window1,width,2.4).
has_property(wall2,width,0.5).
has_property(wall2,height,0.5).
has_opening(ifcwall_15478,door1).
has_opening(ifcwall_15478,window1).
has_units(area,area,square_meter).
has_units(dimension,length, millimeter).
has_units(size,length,meter).
has_units(height,length,meter).
has_units(width,length,meter).
has_covering(ifcwall_15478,finishes1).
has_covering(wall2,finishes2).
has_quantity(ifcwall_15478,ifcquantityarea_33501).
has_quantity(ifcwall_15478,ifcquantitylength_33505).
has_quantity(ifcwall_15478,ifcquantitylength_33506).
has_quantity(ifcwall_15478,ifcquantitylength_33507).
area(ifcquantityarea_33501).
height(ifcquantitylength_33505).
length(ifcquantitylength_33506).
width(ifcquantitylength_33507).
has_value(ifcquantitylength_33506,5).
has_value(ifcquantitylength_33505,3.5).
has_value(ifcquantityarea_33501,17.5).
has_value(ifcquantitylength_33507,0.5).
has_unit(ifcquantitylength_33507,meter).
has_unit(ifcquantitylength_33506,meter).
has_unit(ifcquantityarea_33501, square_meter).
has_unit(ifcquantitylength_33505,meter).

:- dynamic costitem/1.
hasmaterial_costitem(Buildingcomponents):-
	has_material(Buildingcomponents,X),
	tab(2),
	write(X),
	nl,fail.
hasmaterial_costitem(_).

hasproperty_material(Material):-
	has_property(Material,Y,Z),
	tab(2),
	write(Y),
	write(Z),
	nl,
	fail.
hasproperty_material(_).

state(Buildingcomponents) :-
	write('CostItemDescription:'),
	has_material(Buildingcomponents,X),has_property(X,Y,Z),
	tab(1),
	write(X),tab(1),
	write(Y),tab(1),
	write(Z),tab(1),
	fail.
state(_).

measuredunits_length(C) :- C =< 500.
measuredunits_area(C):- C>500.

select(Buildingcomponents) :-
	write('Measured Unit:'),
	has_quantity(Buildingcomponents,Y), width(Y),
	has_value(Y,Z),
	C is Z*1000,
	C >500,
	units(area,A),
	write(A),
	fail.

select(Buildingcomponents) :-
	has_quantity(Buildingcomponents,Y), width(Y),
	has_value(Y,Z),
	C is Z*1000,
	C =<500,
	write(meter),
	fail.
select(_).

