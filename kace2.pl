:- abolish(walls/1).
:- abolish(decompose/2).
:- abolish(has_name/2).
:- abolish(material/2).
:- abolish(thickness/2).
:-abolish(measuredby/2).

:-abolish(roofcoveringsandwallcoverings/1).
:-abolish(boundarywork/1).
:-abolish(surveybasedquantity/1).
:-abolish(has_surveybasedquantity/2).
:-abolish(has_formedmaterialcost/2).
:-abolish(has_formedlabourcost/2).
:- dynamic(walls/1).
:- dynamic(roofcoveringsandwallcoverings/1).
:- dynamic(boundarywork/1).
:- dynamic(decompose/2).

wall(ifcwall_15478).
finishes(ifccovering_14583).
has_walltype(ifcwall_15478, exteranl_wall).
has_covering(ifcwall_15478,ifccovering_14583).
has_elevation(ifcspace_1234568,'+-1.10').
has_spacefunction(ifcwall_15478,ifcspace_1234568).
window(ifcwindow_18136).
spacefunction(ifcspace_1234568).
labourcost(recordedlabourcost47).
labourcost(recordedlabourcost50).
materialcost(recordedmaterialcost47).
materialcost(recordedmaterialcost50).
dampproof(ifcfinishes_12345).
costlineitem(lineitem_123456936).
material('adobe brick').
type('semi-stabilized').
size('254mm x 102mm x 356mm').
constructionmethod('cement mortar').
material('ceramic tile 15.24cm x 15.24cm').
has_itemdescription(lineitem_123456930,'adobe brick').
has_itemdescription(lineitem_123456930,'semi-stabilized').
has_itemdescription(lineitem_123456930,'254mm x 102mm x 356mm').
has_itemdescription(lineitem_123456930,'cement mortar').
has_itemdescription(lineitem_123456931,'adobe brick').
has_itemdescription(lineitem_123456931,'semi-stabilized').
has_itemdescription(lineitem_123456931,'305mm x 102mm x 406mm').
has_itemdescription(lineitem_123456931,'cement_mortar').
has_itemdescription(lineitem_123456932,'adobe_brick').
has_itemdescription(lineitem_123456932,'semi-stabilized').
has_itemdescription(lineitem_123456932,'254mm x 102mm x 406mm').
has_itemdescription(lineitem_123456932,'cement mortar').
has_itemdescription(lineitem_123456933,'adobe brick').
has_itemdescription(lineitem_123456933,'semi-stabilized').
has_itemdescription(lineitem_123456933,'203mm x 102mm x 406mm').
has_itemdescription(lineitem_123456933,'cement mortar').
has_itemdescription(lineitem_123456934,'adobe brick').
has_itemdescription(lineitem_123456934,'semi-stabilized').
has_itemdescription(lineitem_123456934,'102mm x 102mm x 406mm').
has_itemdescription(lineitem_123456934,'cement mortar').
has_itemdescription(lineitem_123456935,'adobe brick').
has_itemdescription(lineitem_123456935,'semi-stabilized').
has_itemdescription(lineitem_123456935,'152mm x 102mm x 406mm').
has_itemdescription(lineitem_123456935,'cement mortar').
has_itemdescription(lineitem_123456936,'adobe brick').
has_itemdescription(lineitem_123456936,'semi-stabilized').
has_itemdescription(lineitem_123456936,'102mm x 102mm x 305mm').
has_itemdescription(lineitem_123456936,'cement mortar').
has_itemdescription(lineitem_123456937,'adobe brick').
has_itemdescription(lineitem_123456937,'semi-stabilized').
has_itemdescription(lineitem_123456937,'203mm x 102mm x 305mm').
has_itemdescription(lineitem_123456937,'cement mortar').
has_itemdescription(lineitem_123456938,'common brick').
has_itemdescription(lineitem_123456938,'semi-stabilized').
has_itemdescription(lineitem_123456938,'102mm x 102mm x 305mm').
has_itemdescription(lineitem_123456938,'mortar').
has_itemdescription(lineitem_123456846,'ceramic tile 10.8cm x 10.8cm').
has_itemdescription(lineitem_123456846,'exterior walls').
has_itemdescription(lineitem_123456846,'mud set').

has_itemdescription(lineitem_123456847,'ceramic tile 3.5cm x 3.5cm').
has_itemdescription(lineitem_123456847,'exterior walls').
has_itemdescription(lineitem_123456847,'mud set').
has_itemdescription(lineitem_123456848,'ceramic tile 10.8cm x 10.8cm').
has_itemdescription(lineitem_123456848,'exterior walls').
has_itemdescription(lineitem_123456848,'mud set').
has_itemdescription(lineitem_123456848,'plain crystalline glazed').
has_itemdescription(lineitem_123456849,'ceramic tile 10.8cm x 10.8cm').
has_itemdescription(lineitem_123456849,'exterior walls').
has_itemdescription(lineitem_123456849,'mud set').
has_itemdescription(lineitem_123456849,'scored crystalline glazed').
has_itemdescription(lineitem_123456850,'ceramic tile 15.24cm x 15.24cm').
has_itemdescription(lineitem_123456850,'exterior walls').
has_itemdescription(lineitem_123456850,'mud set').
has_itemdescription(lineitem_123456850,'plain crystalline glazed').

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
has_price(lineitem_123456850,recordedmaterialcost50).
has_price(lineitem_123456850,recordedlabourcost50).



has_value(recordedmaterialcost47,4.56).
has_date(recordedmaterialcost47,'Year_2013_Quarter_2').
has_unit(recordedmaterialcost47,squaremeter).
has_currency(recordedmaterialcost47,dollor).
has_value(recordedlabourcost47,3.17).
has_date(recordedlabourcost47,'Year_2013_Quarter 2').
has_unit(recordedlabourcost47,squaremeter).
has_labourtype(recordedlabourcost47,standard).
has_currency(recordedlabourcost47,dollor).

has_value(recordedmaterialcost50,66.2).
has_date(recordedmaterialcost50,'Year_2013_Quarter_2').
has_unit(recordedmaterialcost50,'squaremeter').
has_currency(recordedmaterialcost50,'dollor').
has_value(recordedlabourcost50,68.35).
has_date(recordedlabourcost50,'Year_2013_Quarter 2').
has_unit(recordedlabourcost50,'squaremeter').
has_labourtype(recordedlabourcost50,'standard').
has_currency(recordedlabourcost50,'dollor').



design_material(ifcmateriallayer_19034).
design_material(ifcmateriallayer_18990).
workingmethods(ifctask_16034).
workingmethods(ifctask_16080).
has_method(ifctask_16034,'M10 cement mortar').
has_method(ifctask_16080,'mud set').

has_type(ifcwall_15478,ifcmateriallayer_19034).
has_type(ifccovering_14583,ifcmateriallayer_18990).

has_working_method(ifcmateriallayer_19034,ifctask_16034).
has_working_method(ifcmateriallayer_18990,ifctask_16080).
layerthickness(ifcmateriallayer_19034, '102mm x 102mm x 305mm').
layerthickness(ifcmateriallayer_18990,'100mm').
has_material(ifcmateriallayer_19034, 'adobe brick').
has_material(ifcmateriallayer_18990,'ceramic tile 15.24cm x 15.24cm').

overallheight(ifcwindow_18136, 2100).
overallwidth(ifcwindow_18136,2400).

has_opening(ifcwall_15478,ifcwindow_18136).
has_opening(ifccovering_14583,ifcwindow_18136).


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
has_unit(windowsize1, square_meter).
measurementrule(decompose_rule_1).
componentfeatures(adjust_rule_1).

:- dynamic costitem/1.

:- dynamic constructionproduct/1.


decomposing(Buildingelement) :-
	wall(Buildingelement),
	has_type(Buildingelement,Y),
	has_material(Y,A),
	layerthickness(Y,B),
	has_working_method(Y,C),
	has_method(C,D),
	member(A,['brick','adobe brick'])
	->
	E=[A,B,D],
	assert(has_name(constructionproduct1,E)),
	assert(decompose(Buildingelement,constructionproduct1)),
	assert(walls(constructionproduct1)),
	assert(material(constructionproduct1,A)),
	assert(thickness(constructionproduct1,B)).

decomposing(Buildingelement):-
	finishes(Buildingelement),
	has_opening(Buildingelement,A),
	overallwidth(A,B),overallheight(A,C),
	D is B*C/1000000, D>1,
	has_type(Buildingelement,Y),has_material(Y,E),member(E,['tile','ceramic tile','ceramic tile 15.24cm x 15.24cm']),has_working_method(Y,F),has_method(F,G),layerthickness(Y,I)
	->
	H=[E,I,G],
	assert(decompose(Buildingelement,constructionproduct2)),
	assert(decompose(Buildingelement,constructionproduct3)),
	assert(roofcoveringsandwallcoverings(constructionproduct2)),
	assert(boundarywork(constructionproduct3)),
	assert(has_name(constructionproduct2,H)),
	assert(material(constructionproduct2,E)),
	assert(thickness(constructionproduct2,I)),
	assert(has_name(constructionproduct3,H)),
	assert(material(constructionproduct3,E)),
	assert(thickness(constructionproduct3,I)).



selecting(ConstructionProduct) :-
	dampproof(ConstructionProduct),
	has_quantity(ConstructionProduct,Y), width(Y),
	has_value(Y,Z),
	C is Z*1000,
	C >300->
	write(area),
	assert(measuredby(ConstructionProduct,'squaremeter')),
	fail.

selecting(ConstructionProduct) :-
	dampproof(ConstructionProduct),
	has_quantity(ConstructionProduct,Y), width(Y),
	has_value(Y,Z),
	C is Z*1000,
	C =<500,
	write(meter),
	fail.

selecting(ConstructionProduct) :-
	walls(ConstructionProduct)
	->assert(measuredby(ConstructionProduct,'squaremeter')).


selecting(ConstructionProduct) :-
	window(ConstructionProduct)->
	write(number),
	fail.

selecting(ConstructionProduct) :-
	roofcoveringsandwallcoverings(ConstructionProduct)->
	assert(measuredby(ConstructionProduct,'squaremeter')).

selecting(ConstructionProduct) :-
	boundarywork(ConstructionProduct)->
	assert(measuredby(ConstructionProduct,'meter')).



costitemdescription(Buildingelement) :-
	decomposing(Buildingelement)
	->
	decompose(Buildingelement, X),
	selecting(X).





criterion(ConstructionProduct,Criteria):-
	walls(ConstructionProduct)-> material(ConstructionProduct,Criteria).
criterion(ConstructionProduct,Criteria):-
	walls(ConstructionProduct)-> thickness(ConstructionProduct,Criteria).
criterion(ConstructionProduct,Criteria):-
	roofcoveringsandwallcoverings(ConstructionProduct)->material(ConstructionProduct,Criteria).
criterion(ConstructionProduct,Criteria):-
	boundarywork(ConstructionProduct)->material(ConstructionProduct,Criteria).

criteria(Buildingelement):-
	findall(X,criterion(Buildingelement,X),Y)->
	write(Y).

match(Criteria,Candidatesolution) :-
	has_itemdescription(Candidatesolution,Criteria).

match_list(PList,Candidatesolution):-
	foreach(
	    member(P,PList),
	    match(P,Candidatesolution)).
identifying(ConstructionProduct, Candidatesolution):-
	findall(X,criterion(ConstructionProduct,X),Y),
	match_list(Y,Candidatesolution).


measuring(ConstructionProduct):-
	walls(ConstructionProduct),
	measuredby(ConstructionProduct,'squaremeter'),
	decompose(Buildingelement,ConstructionProduct),
	has_opening(Buildingelement,X),
	window(X),
	overallheight(X,Y),
	overallwidth(X,Z),
	A is Y*Z/1000000,
	A > 0.5,
	has_quantity(Buildingelement,B),
	area(B),has_value(B,C)
	->
	D is C - A,
	write(D),tab(1),write(squaremeter).
measuring(ConstructionProduct):-
	roofcoveringsandwallcoverings(ConstructionProduct),
	measuredby(ConstructionProduct,'squaremeter'),
	decompose(Buildingelement,ConstructionProduct),
	has_opening(Buildingelement,X), window(X),
	overallheight(X,Y),overallwidth(X,Z),
	A is Y*Z/1000000,
	A > 0.5,
	has_covering(E,Buildingelement),
	has_quantity(E,B),area(B),has_value(B,C)
	->
	D is C - A,
	write(D),tab(1),write(squaremeter).
measuring(ConstructionProduct):-
	boundarywork(ConstructionProduct),
	measuredby(ConstructionProduct,'meter'),
	decompose(Buildingelement,ConstructionProduct),
	has_opening(Buildingelement,X), window(X),
	overallheight(X,Y),overallwidth(X,Z),Y>1000,Z>1000
	->
	A is Y*2/1000+Z*2/1000,
	assert(surveybasedquantity(A)),
	assert(has_surveybasedquantity(ConstructionProduct,A)),write(A),tab(1),write(meter).

establishing(ConstructionProduct):-
	roofcoveringsandwallcoverings(ConstructionProduct),
	identifying(ConstructionProduct, X),aggregate_all(count,identifying(ConstructionProduct,X),Count),Count=1,
	findall(Y,has_itemdescription(X,Y),Y),

	write(Y).

establishing(ConstructionProduct):-
	boundarywork(ConstructionProduct),
aggregate_all(count,identifying(ConstructionProduct,X),Count),Count=1,
	identifying(ConstructionProduct,X),findall(Y,has_itemdescription(X,Y),Y),
	C = ['Boundary Work'],
	append(Y,C,D),
	write(D).
establishing(ConstructionProduct):-
	walls(ConstructionProduct),decompose(Buildingelement,ConstructionProduct),
	identifying(ConstructionProduct, X),
	findall(Y,has_itemdescription(X,Y),Y),
	has_spacefunction(Buildingelement,B),
	findall(D,has_elevation(B,D),D),
	append(Y,D,C),
	write(C).

forming(ConstructionProduct) :-
	aggregate_all(count,identifying(ConstructionProduct,X),Count),Count=1,
	identifying(ConstructionProduct,X),
	has_price(X,Y),materialcost(Y),has_value(Y,Z),measuredby(ConstructionProduct,G),
	has_unit(Y,G),has_currency(Y,B),has_price(X,C),labourcost(C),has_value(C,D),	has_unit(C,G),has_currency(C,F)
	,
	write('Material Cost:'), tab(1),
	write(Z), tab(1),
	write(B), write('/'),write(G),nl,

	write('Labour Cost:'), tab(1),
	write(D), tab(1),
	write(F), write('/'),write(G),assert(has_formedmaterialcost(ConstructionProduct,Z)),assert(has_formedlabourcost(ConstructionProduct,D));
	boundarywork(ConstructionProduct),aggregate_all(count,identifying(ConstructionProduct,X),Count),Count=1,
	identifying(ConstructionProduct,X),
	has_price(X,Y),materialcost(Y),has_value(Y,Z),measuredby(ConstructionProduct,G),
	has_unit(Y,A),has_currency(Y,B),has_price(X,C),labourcost(C),has_value(C,D),	has_unit(C,E),has_currency(C,F)
	->
has_surveybasedquantity(ConstructionProduct,H),I is H*0.5,J is I*Z/H,Q is I*D/H,
	write('Oringinal Material Cost:'), tab(1),
	write(Z), tab(1),
	write(B), write('/'),write(A),nl,
	write('Oringinal Labour Cost:'), tab(1),
	write(D), tab(1),
	write(F), write('/'),write(E),nl,

	write('Converted Material Cost:'), tab(1),
	write(J), tab(1),
	write(B), write('/'),write(G),nl,
	write('Converted Labour Cost:'), tab(1),
	write(Q), tab(1),
	write(F), write('/'),write(G),assert(has_formedmaterialcost(ConstructionProduct,J)),assert(has_formedlabourcost(ConstructionProduct,Q)).

adjusting(ConstructionProduct) :-
	decompose(Buildingelement, ConstructionProduct),wall(Buildingelement),has_quantity(Buildingelement,Y),height(Y),has_value(Y,Z),Z>2.74,Z<3.96->
	identifying(ConstructionProduct,X),
	has_price(X,C),labourcost(C),has_value(C,D),
	has_unit(C,E),has_currency(C,F),
	write('Descrease Labour Cost: 30%'),nl,
	write('Scoped Labour Cost:'),
	G is D*0.7,
	write(G),tab(1),
	write(F), write('/'),write(E).
adjusting(ConstructionProduct) :-
	decompose(Buildingelement, ConstructionProduct),finishes(Buildingelement),has_covering(Mainelement,Buildingelement),has_quantity(Mainelement,Y),height(Y),has_value(Y,Z),Z>2.74,Z<3.96->
	has_formedlabourcost(ConstructionProduct,D),measuredby(ConstructionProduct,E),
	write('Descrease Labour Cost: 30%'),nl,
	write('Scoped Labour Cost:'),
	G is D*0.7,
	write(G),tab(1),
	write(dollor), write('/'),write(E).



unitprice(Buildingelement):-
	decompose(Buildingelement,ConstructionProduct),
	forming(ConstructionProduct),nl,
	adjusting(ConstructionProduct).

costitemwithquantity(Buildingelement):-
	costitemdescription(Buildingelement),decompose(Buildingelement,ConstructionProduct),
	write('Cost Item Description:'),establishing(ConstructionProduct),nl,
	write('Measured Quantity:'),
	measuring(ConstructionProduct).

quantitysurveying(Buildingelement):-
	wall(Buildingelement),costitemwithquantity(Buildingelement),nl,
	unitprice(Buildingelement);

	finishes(Buildingelement),decomposing(Buildingelement)->aggregate_all(count,decompose(Buildingelement,X),Count),write('There are'),tab(1),write(Count),tab(1),write('cost items derived'),nl.
quantitysurveying(Buildingelement):-
	aggregate_all(count,decompose(Buildingelement,X),Count),Count=2,decompose(Buildingelement,constructionproduct2),
	write('First Cost Item:'),selecting(constructionproduct2),nl,
	write('Candidate Solution'),tab(1),identifying(constructionproduct2,Y),write(Y),nl,
	write('Cost Item Description'),tab(1),establishing(constructionproduct2),nl,
	write('Meausred Quantity'),tab(1),measuring(constructionproduct2),nl,
	forming(constructionproduct2),nl,adjusting(constructionproduct2),nl,
	write('Second Cost Item:'),selecting(constructionproduct3),nl,
	write('Candidate Solution'),tab(1),identifying(constructionproduct3,Y),write(Y),nl,
	write('Cost Item Description'),tab(1),establishing(constructionproduct3),nl,
	write('Meausred Quantity'),tab(1),measuring(constructionproduct3),nl,
	forming(constructionproduct3),nl,adjusting(constructionproduct3).
