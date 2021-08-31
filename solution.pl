% demet yayla
% 2019400105
% compiling: yes
% complete: yes
:- encoding(utf8).
:- ['load.pro'].

%form_wo_weight(+List1, +List2, -Sum)

%Finds the sum of the squares of the differences between glanian1_feature[i] and 
%glanian2_expected_feature[i]. Doesn' take wieghts into account. Helper method for glanian_distance
form_wo_weight([],[],0).
form_wo_weight([Hf|Tf], [He|Te], Sum) :- 
    form_wo_weight(Tf, Te, SquaSums),
    (   (He \= -1, Sum is SquaSums + ((He-Hf)*(He-Hf))); (He =:= -1), Sum is SquaSums	).



%glanian_distance(+Name1, +Name2, -Distance)

%Given two glanians Name1 and Name2, this predicate will return the distance from Name1 to Name2.
glanian_distance(Name1, Name2, Distance) :-
    glanian(Name2,_, FeatureList),
    expects(Name1,_, ExpectList),
    form_wo_weight(FeatureList, ExpectList , SquaSums),
    Distance is sqrt(SquaSums).



%form_w_weight(+List1, +List2, +List3 , -Sum)

%Finds the sum of the squares of the differences between glanian1_feature[i] and glanian2_expected_feature[i] 
%times weight index of the feature, so takes wieghts into account. Helper method for weighted_glanian_distance
form_w_weight([],[],[], 0).
form_w_weight([Hf|Tf], [He|Te], [Hw|Tw],Sum) :- 
    form_w_weight(Tf, Te, Tw, SquaSums),
    (   (He \= -1, Sum is SquaSums + (He-Hf)*(He-Hf)*Hw); (He =:= -1), Sum is SquaSums	).



%weighted_glanian_distance(+Name1, +Name2, -Distance)

%Given two glanians Name1 and Name2, this predicate will return the weighted distance from Name1 to Name2.
weighted_glanian_distance(Name1, Name2, Distance) :-
    glanian(Name2,_, FeatureList),
    expects(Name1,_, ExpectList),
    weight(Name1, WeightList),
    form_w_weight(FeatureList, ExpectList, WeightList, SquaWeightedSums),
    Distance is sqrt(SquaWeightedSums).    



%find_possible_cities(+Name, -CityList)

%Returns the union of Name's current city and liked cities
find_possible_cities(Name, CityList) :-
    city(CityName, HabitantList, _ ),
    member(Name, HabitantList),
    likes(Name, _ , LikedCities),
    CityList = [CityName|LikedCities].



%merge_without_duplicate(+List1,+List2,-List3)

%merges List1 and List2 without duplicates
merge_without_duplicate([],List2,List3) :-
    List3 = List2.
merge_without_duplicate(List1,[], List3) :-
    List3 = List1.

merge_without_duplicate([H1|T1], List4, List3) :-
    not(member(H1,List4)), !, merge_without_duplicate(T1,List4, List2), append([H1], List2, List3).
merge_without_duplicate([H1|T1],List2,List3) :-
    delete(List2,H1, List4),merge_without_duplicate(T1,List4,List5), append([H1],List5,List3).



%merge_possible_cities(+Name1, +Name2, -CityList)

%merges Name1's and Name2's find_possible_cities'
merge_possible_cities(Name1, Name2, CityList) :-
    find_possible_cities(Name1, CityList1),
    find_possible_cities(Name2, CityList2),
    merge_without_duplicate(CityList1, CityList2, CityList).



%collectMutual(-ActivityList, +List1, +List2)

%intersects List1 and List2 (Collects the mutual elements for two lists).
collectMutual([], [],_).
collectMutual(ActivityList, [H|T], List2) :-
    collectMutual(Activity, T, List2),
    (   member(H,List2), ActivityList = [H|Activity] ,!;
    ActivityList = Activity
    ).



%find_mutual_activities(+Name1, +Name2, -ActivityList)

%Finds Name1 and Name2's mutually liked activities.
find_mutual_activities(Name1, Name2, ActivityList) :-
    likes(Name1, LikedAct1 , _),
    likes(Name2, LikedAct2, _),
    collectMutual(ActivityList, LikedAct1, LikedAct2).



%seperate_lists(-ListD, -ListT, +List)

%given a list of couples, seperates those two indexed elements into two lists, ListD and ListT.
seperate_lists([],[],[]).
seperate_lists(ListD, ListT, [[H|T]|Ts]) :-
    seperate_lists(ListD2, ListT2, Ts),
    append([H], ListD2, ListD),
    append(T, ListT2, ListT).



%list_distances(+Name, +List, -Distances)

% Distances[i] is the non-weighted distance between Name and List[i]
list_distances(_,[], []).
list_distances(Name, [H|T], Distances) :-
    list_distances(Name, T, Distance),
    glanian_distance(Name, H, Dist),
    append([[Dist, H]], Distance,Distances).



%find_possible_targets(+Name, -Distances, -TargetList)

%This predicate will return a list of possible glanians sorted by their distances as possible matching
%targets for Name. Distances should be a sorted list that contains glanian distance from Name to
%glanians in TargetList. The gender of each glanian in TargetList should be in ExpectedGenders of Name.
find_possible_targets(Name, Distances, TargetList) :-
    expects(Name, ExpGenderList, _),
    (   expGenderList =@= [],TargetList = [], Distances = [], !;
        findall(Na, (glanian(Na,Gender,_), Na \== Name ,member(Gender, ExpGenderList)), List),
        list_distances(Name,List, Distance),
        sort(Distance, Sorted),
        seperate_lists(Distances, TargetList, Sorted)
    ),!.



%list_weighted_distances(+Name, +List1, -List2)

%only difference between list_distances is that this uses weighted_glanian_distance instead of glanian_distance
list_weighted_distances(_,[], []).
list_weighted_distances(Name, [H|T], Distances) :-
    list_weighted_distances(Name, T, Distance),
    weighted_glanian_distance(Name, H, Dist),
    append([[Dist, H]], Distance,Distances).



%find_weighted_targets(+Name, -Distances, -TargetList)

%This predicate will return a list of possible glanians sorted by their weighted distances as possible
%matching targets for Name. It is the same as find_possible_targets except that the distances are
%calculated with weighted glanian distance(Name, Target, D).
find_weighted_targets(Name, Distances, TargetList) :-
    expects(Name, ExpGenderList, _),
    (   expGenderList =@= [],TargetList = [], Distances = [], !;
        findall(Na, (glanian(Na,Gender,_), Na \== Name, member(Gender, ExpGenderList)), List),
        list_weighted_distances(Name,List, Distance),
        sort(Distance, Sorted),
        seperate_lists(Distances, TargetList, Sorted)
    ),!.



%tolerable(+List1, +List2)

%checks if for all i, List2[i] is within the boundries stated in List1[i].
tolerable([], []).
tolerable([H|Td], [Hf|Tf]) :-
    tolerable(Td, Tf),
    (   H =@= [],!;
        [A,B] = H,
        Hf @> A,
        Hf @< B
        ).



%target_is_appropriate(+Me, +Friend)

%checks Friend for the conditions s/he should comply except for gender for below predicates.
target_is_appropriate(Me, Friend) :-
    not(old_relation([Me,Friend])),
    not(old_relation([Friend, Me])),
    glanian(Friend,_, Features),
    dislikes(Me,_, _,Dislikes),
    tolerable(Dislikes, Features),
    dislikes(Me, DislikedActivities, _,_),
    likes(Friend, LikedActivities,_),
    findall(X, (member(X,DislikedActivities), member(X, LikedActivities)), CommonList),
    length(CommonList, L),
    L @< 3.



%seperate_quadruple_lists(-List1, -List2, -List3, -List4, +List5)

%seperates a list of quadruples into four lists according to indexes.
seperate_quadruple_lists([],[],[],[],[]).
seperate_quadruple_lists(List1, List2, List3, List4, [[A,B,C,D]|Ts]) :-
    seperate_quadruple_lists(List1_2, List2_2, List3_2, List4_2, Ts),
    append([A], List1_2, List1),
    append([B], List2_2, List2),
    append([C], List3_2, List3),
    append([D], List4_2, List4).



%find_my_best_target(+Name, -Distances, -ActivityList, -CityList,-TargetList)

%This predicate will use all the other restrictions to nd possible matching targets together with
%possible activities in possible cities. So in the end, a glanian will enter her name and will get a list
%of distances to her matching targets, and activities that can be done in possible cities.
find_my_best_target(Name, Distances, ActivityList, CityList,TargetList) :-
    findall(N, (expects(Name, ExpGenderList,_),glanian(N,Gender,_), N \== Name ,member(Gender, ExpGenderList), target_is_appropriate(Name, N)), PossibleMatches),
    findall([Dist, Activity, City, Name2], (member(Name2, PossibleMatches), weighted_glanian_distance(Name, Name2, Dist), merge_possible_cities(Name, Name2, MergedCities),likes(Name, LA,_),
        (find_possible_cities(Name, PosCity), member(City, PosCity), city(City, _, CLA), member(Activity, CLA), not(member(Activity, LA)); city(City, _, AL),member(Activity,AL),member(Activity,LA)),
        dislikes(Name, DislikedActivities,DislikedCities, _), not(member(Activity, DislikedActivities)),not(member(City, DislikedCities)), member(City, MergedCities)), Solutions),
    sort(Solutions, Sorted),
    seperate_quadruple_lists(Distances, ActivityList, CityList, TargetList, Sorted).



%avg_weighted_glanian_distance(+Name1, +Name2, -Distance)

%finds the average of Name1 and Name2's weighted_glanian_distance
avg_weighted_glanian_distance(Name1, Name2, Distance) :-
    weighted_glanian_distance(Name1, Name2, Dis1),
    weighted_glanian_distance(Name2, Name1, Dis2),
    Distance is (Dis1+Dis2)/2.    
    
    
    
%find_my_best_match(+Name, -Distances, -ActivityList, -CityList, -TargetList)

%This predicate is similar to the previous predicate with some additional constraints. In this predicate,
%we also take the matching target's preferences into account.
find_my_best_match(Name, Distances, ActivityList, CityList, TargetList) :-
    findall(N, (expects(Name, ExpGenderList,_),glanian(N,Gender,_), N \== Name,member(Gender, ExpGenderList), target_is_appropriate(Name, N), expects(N, Exp,_), glanian(Name, Gen,_), member(Gen, Exp), target_is_appropriate(N, Name)), PossibleMatches),
    findall([Dist, Activity, City, Name2], (member(Name2, PossibleMatches), avg_weighted_glanian_distance(Name, Name2, Dist), merge_possible_cities(Name, Name2, MergedCities),likes(Name, LA,_),likes(Name2, L,_),
        (find_possible_cities(Name, PosCity), member(City, PosCity), city(City, _, CLA), member(Activity, CLA), not(member(Activity, LA)); city(City, _, AL),member(Activity,AL),member(Activity,LA)),
        (find_possible_cities(Name2, PosCit), member(City, PosCit), city(City, _, CL), member(Activity, CL), not(member(Activity, L)); city(City, _, A),member(Activity,A),member(Activity,L)),
        dislikes(Name, DislikedActivities,DislikedCities, _), not(member(Activity, DislikedActivities)),not(member(City, DislikedCities)), member(City, MergedCities),
        dislikes(Name2, DislikedActiviti,DislikedCiti, _), not(member(Activity, DislikedActiviti)),not(member(City, DislikedCiti))), Solutions),
    sort(Solutions, Sorted),
    seperate_quadruple_lists(Distances, ActivityList, CityList, TargetList, Sorted).



%trim(+List, +N, -Trimmed)

%trimms List to size N
trim(List, N, Trimmed) :-
    findall(E, (nth1(I,List,E), I =< N), Trimmed).



%find_top_ten_matches()

%the couple has both ways amiable genders, tolerable features; among couples going along with these, no prior relation 
%and clashing tastes in activities no more than 3. Couple with least weighted glanian distance has priority.
find_top_ten_matches() :-
    findall([W,A,B], (glanian(A,G1,_), glanian(B,G2,_), A \=B,expects(A,EG1,_),expects(B,EG2,_),member(G1,EG2), member(G2,EG1),target_is_appropriate(A,B),target_is_appropriate(B,A), weighted_glanian_distance(A,B,W)), List),
    sort(List, Sorted),
    trim(Sorted, 10, Trimmed),
    absolute_file_name('top10.txt',Abs),
    open(Abs,write,Out),
    forall(member([W,E,K],Trimmed), (write(Out, E), write(Out, ' - '), write(Out, K) , nl(Out))),
    close(Out).