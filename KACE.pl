wall(ifcwall_15478).
has_type(ifcwall_15478, exteranl_wall).
has_elevation(ifcspace_1234568,'+-1.10').
has_spacefunction(ifcwall_15478,ifcspace_1234568).
window(ifcwindow_18136).
spacefunction(ifcspace_1234568).
labourcost(recordedlabourcost47).
materialcost(recordedmaterialcost47).
waterproof(ifcfinishes_12345).
costlineitem(lineitem_123456936).
material(adobe_brick).
type(semi-stabilized).
size('254mm x 102mm x 356mm').
constructionmethod(cement_mortar).

has_itemdescription(lineitem_123456930,adobe_brick).
has_itemdescription(lineitem_123456930,semi-stabilized).
has_itemdescription(lineitem_123456930,'254mm x 102mm x 356mm').
has_itemdescription(lineitem_123456930,cement_mortar).
has_itemdescription(lineitem_123456931,adobe_brick).
has_itemdescription(lineitem_123456931,semi-stabilized).
has_itemdescription(lineitem_123456931,'305mm x 102mm x 406mm').
has_itemdescription(lineitem_123456931,cement_mortar).
has_itemdescription(lineitem_123456932,adobe_brick).
has_itemdescription(lineitem_123456932,semi-stabilized).
has_itemdescription(lineitem_123456932,'254mm x 102mm x 406mm').
has_itemdescription(lineitem_123456932,cement_mortar).
has_itemdescription(lineitem_123456933,adobe_brick).
has_itemdescription(lineitem_123456933,semi-stabilized).
has_itemdescription(lineitem_123456933,'203mm x 102mm x 406mm').
has_itemdescription(lineitem_123456933,cement_mortar).
has_itemdescription(lineitem_123456934,adobe_brick).
has_itemdescription(lineitem_123456934,semi-stabilized).
has_itemdescription(lineitem_123456934,'102mm x 102mm x 406mm').
has_itemdescription(lineitem_123456934,cement_mortar).
has_itemdescription(lineitem_123456935,adobe_brick).
has_itemdescription(lineitem_123456935,semi-stabilized).
has_itemdescription(lineitem_123456935,'152mm x 102mm x 406mm').
has_itemdescription(lineitem_123456935,cement_mortar).
has_itemdescription(lineitem_123456936,adobe_brick).
has_itemdescription(lineitem_123456936,semi-stabilized).
has_itemdescription(lineitem_123456936,'102mm x 102mm x 305mm').
has_itemdescription(lineitem_123456936,cement_mortar).
has_itemdescription(lineitem_123456937,adobe_brick).
has_itemdescription(lineitem_123456937,semi-stabilized).
has_itemdescription(lineitem_123456937,'203mm x 102mm x 305mm').
has_itemdescription(lineitem_123456937,cement_mortar).
has_itemdescription(lineitem_123456938,common_brick).
has_itemdescription(lineitem_123456938,semi-stabilized).
has_itemdescription(lineitem_123456938,'102mm x 102mm x 305mm').
has_itemdescription(lineitem_123456938,mortar).



has_price(lineitem_123456930,recordedmaterialcost41).
has_price(lineitem_123456930,recordedlabourcost41).
has_price(lineitem_123456931,recordedmaterialcost42).
has_price(lineitem_123456931,recordedlabourcost42).
has_price(lineitem_123456932,recordedmaterialcost43).
has_price(lineitem_123456932,recordedlabourcost43).
has_price(lineitem_123456933,recordedmaterialcost44).
has_price(lineitem_123456933,recordedlabourcost44).
has_price(lineitem_123456934,recordedmaterialcost45).
has_price(lineitem_123456934,recordedlabourcost45).
has_price(lineitem_123456935,recordedmaterialcost46).
has_price(lineitem_123456935,recordedlabourcost46).
has_price(lineitem_123456936,recordedmaterialcost47).
has_price(lineitem_123456936,recordedlabourcost47).
has_price(lineitem_123456937,recordedmaterialcost48).
has_price(lineitem_123456937,recordedlabourcost48).

has_value(recordedmaterialcost47,4.56).
has_date(recordedmaterialcost47,'Year_2013_Quarter_2').
has_unit(recordedmaterialcost47,squaremeter).
has_currency(recordedmaterialcost47,dollor).
has_value(recordedlabourcost47,3.17).
has_date(recordedlabourcost47,'Year_2013_Quarter 2').
has_unit(recordedlabourcost47,squaremeter).
has_labourtype(recordedlabourcost47,standard).
has_currency(recordedlabourcost47,dollor).


design_material(brick1).


has_constructionworkresult(ifcwall_15478,brick1).
has_constructionworkresult(ifcwindow_18136, windowsize1).

has_property(brick1, '102mm x 102mm x 305mm').
has_property(brick1, cement_mortar).
has_property(brick1, adobe_brick).
has_property(windowsize1, 2100*2400-mm*mm).

has_opening(ifcwall_15478,ifcwindow_18136).



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
has_value(windowsize1, 5.04).


has_unit(ifcquantitylength_33507,meter).
has_unit(ifcquantitylength_33506,meter).
has_unit(ifcquantityarea_33501, square_meter).
has_unit(ifcquantitylength_33505,meter).
has_unit(windowsize1, square_meter).
measurementrule(decompose_rule_1).
componentfeatures(adjust_rule_1).

:- dynamic costitem/1.
:- dynamic decompose/2.
:- dynamic constructionproduct/1.

decompose_rule_1(Buildingelement) :-
	write('Construction Product:'),
	wall(Buildingelement),
	has_constructionworkresult(Buildingelement,Y),findall(Z,(has_property(Y,Z)),A),
	write(A),
	assert(decompose(decompose_rule_1,A)),
	assert(constructionproduct(A)),
	fail.


select_rule_1(Buildingelement) :-
	write('Measured Unit:'),
	waterproof(Buildingelement),
	has_quantity(Buildingelement,Y), width(Y),
	has_value(Y,Z),
	C is Z*1000,
	C >500,
	write(area),
	fail.

select_rule_2(Buildingelement) :-
	waterproof(Buildingelement),
	has_quantity(Buildingelement,Y), width(Y),
	has_value(Y,Z),
	C is Z*1000,
	C =<500,
	write(meter),
	fail.

select_rule_3(Buildingelement) :-
	wall(Buildingelement)->
	write(square_meter),
	fail.

select_rule_4(Buildingelement) :-
	window(Buildingelement)->
	write(number),
	fail.

costitemdescription(Buildingelement) :-
	decompose_rule_1(Buildingelement), tab(1),
	wall(Buildingelement)->
	select_rule_3(Buildingelement),tab(1).

create_rule_1(Buildingelement) :-
	wall(Buildingelement),
	has_opening(Buildingelement,X), window(X),
	has_constructionworkresult(X,Y), has_value(Y,Z), has_unit(Y, A),
	A = square_meter,
	Z > 0.5 ->
	write('New-Cost-Item-Creating'),
	fail.


criterion(Buildingelement,Criteria):-
	has_constructionworkresult(Buildingelement,B),has_property(B,Criteria).
criteria(Buildingelement):-
	findall(X,criterion(Buildingelement,X),Y)->
	write(Y).

match(Criteria,Candidatesolution) :-
	has_itemdescription(Candidatesolution,Criteria).

match_list(PList,Candidatesolution):-
	foreach(
	    member(P,PList),
	    match(P,Candidatesolution)).
identify_rule_1(Buildingelement, Candidatesolution):-
	findall(X,criterion(Buildingelement,X),Y),
	match_list(Y,Candidatesolution).


measure_rule_1(Buildingelement):-
	wall(Buildingelement),
	has_opening(Buildingelement,X), window(X),
	has_constructionworkresult(X,Y), has_value(Y,Z), has_unit(Y, A),
	A = square_meter,
	Z > 0.5,
	has_quantity(Buildingelement,B),area(B),has_value(B,C)->
	D is C - Z,
	write(D),tab(1),write(A);
	has_quantity(Buildingelement,B),area(B),has_value(B,C),has_unit(B,D),
	write(C), tab(1),write(D).

:- dynamic  costitemdescrip/1.
state(Buildingelement):-
	wall(Buildingelement),
	identify_rule_1(Buildingelement, X),
	findall(Y,has_itemdescription(X,Y),Y),
	findall(A,has_type(Buildingelement,A),A),
	has_spacefunction(Buildingelement,B),
	findall(D,has_elevation(B,D),D),
	append(Y,A,C),
	append(C,D,E),
	write(E),
	assert(costitemdescrip(E)).

form_rule_1(Buildingelement) :-
	aggregate_all(count,identify_rule_1(Buildingelement,X),Count),Count=1 ->
	identify_rule_1(Buildingelement,X),
	has_price(X,Y),materialcost(Y),has_value(Y,Z),
	has_unit(Y,A),has_currency(Y,B),
	write('Material Cost:'), tab(1),
	write(Z), tab(1),
	write(B), write('/'),write(A),nl,
	has_price(X,C),labourcost(C),has_value(C,D),
	has_unit(C,E),has_currency(C,F),
	write('Labour Cost:'), tab(1),
	write(D), tab(1),
	write(F), write('/'),write(E).

adjust_rule_1(Buildingelement) :-
	wall(Buildingelement),has_quantity(Buildingelement,Y),height(Y),has_value(Y,Z),Z>2.74,Z<3.96->
	identify_rule_1(Buildingelement,X),
	has_price(X,C),labourcost(C),has_value(C,D),
	has_unit(C,E),has_currency(C,F),
	write('Descrease Labour Cost: 30%'),nl,
	write('Scoped Labour Cost:'),
	G is D*0.7,
	write(G),tab(1),
	write(F), write('/'),write(E).

unitprice(Buildingelement):-
	wall(Buildingelement)->
	form_rule_1(Buildingelement),nl,
	adjust_rule_1(Buildingelement).

costitemwithquantity(Buildingelement):-
	wall(Buildingelement)->
	write('Cost Item Description:'),state(Buildingelement),nl,
	write('Measured Quantity:'),
	measure_rule_1(Buildingelement).

costitem(Buildingelement):-
	costitemwithquantity(Buildingelement),nl,
	unitprice(Buildingelement).
