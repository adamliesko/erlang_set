-module(set).
-export([show/1, newSet/0, toList/1, toSet/1, insert/2, delete/2, prec/2, succ/2, equals/2, min/1, max/1, card/1]).
-export([diff/2, union/2, intersect/2]).
-export([isin/2, any/2, all/2, product/3, map/2, filter/2, foldl/3]).
%% vytvorí prázdnu množinu
newSet() ->
  [].

%% vráti zoznam prvkov množiny ako usporiadaný zoznam
toList(Set) ->
  Set.

%% vytvorí množinu z prvkov zoznamu List
toSet(List) when is_list(List) ->
  p_toSet(List, []).

p_toSet([Ele | List], Acc) when is_integer(Ele) ->
  Acc1 = insert(Acc, Ele),
  p_toSet(List, Acc1);
p_toSet([Ele | List], Acc) when not is_integer(Ele) ->
  p_toSet(List, Acc);
p_toSet([], Acc) ->
  Acc.

%% do množiny Set pridá prvok Ele

insert(Set, Ele) when is_integer(Ele), is_list(Set) ->
  reverse(p_insert(Set, Ele, [])).
p_insert([H | Set], Ele, Acc) when Ele > H -> %%ideme doprava
  p_insert(Set, Ele, [H | Acc]);
p_insert([H | R], Ele, Acc) when Ele < H ->
  p_insert(R, H, [Ele | Acc]);
p_insert([_ | R], Ele, Acc) when Acc /= [] ->
  p_insert(R, Ele, Acc);
p_insert(Set, Ele, Acc) when Acc == [], Set == [] ->
  reverse([Ele | Set]);
p_insert(Set, _, Acc) when Acc == [], Set /= [] ->
  reverse(Set);
p_insert(_, Ele, Acc) ->
  [Ele | Acc].


%% z množiny Set odoberie prvok Ele
%% pri tail call alebo body rekurzii vieme skoncit skor a naslednej spojit tie 2 mnoziny - ale proces spojenia 2 mnzoin, aj ked je spravne vhodne cez ++ , kde pridavam k prvemu prvku celu mnozinu,
%% nebude efektivnejsi ako obycajny scan cez filter a nasledne vyhodenie prvku, ktory sa rovna Ele
delete(Set, Ele) when is_integer(Ele), is_list(Set) ->
 filter(Set, fun(X) -> X /= Ele end).

%% vráti prvok, ktorý sa v usporiadanej množine nachádza pred Ele

prec([H | _], Ele) when is_integer(Ele), Ele < H ->
  nil;
prec([H | Set], Ele) when is_integer(Ele), Ele > H ->
  [H1 | R] = Set,
  if
    H1 == Ele -> H;
    R == [] -> nil;
    true -> prec(Set, Ele)
  end;
prec([H | _], Ele) when is_integer(Ele), Ele == H ->
  nil;
prec([], Ele) when is_integer(Ele) ->
  nil.

%% vráti prvok, ktorý sa v usporiadanej množine nachádza za Ele

succ([T], Ele) when is_integer(Ele), Ele == T ->
  nil;
succ([T | Set], Ele) when is_integer(Ele), Ele == T, Set == [] ->
  nil;
succ([T], Ele) when is_integer(Ele), Ele /= T ->
  nil;
succ([H | Set], Ele) when is_integer(Ele), Ele > H ->
  succ(Set, Ele);
succ([H | Set], Ele) when is_integer(Ele), Ele < H ->
  succ(Set, Ele);
succ([], Ele) when is_integer(Ele) ->
  nil;
succ([H | Set], Ele) when is_integer(Ele), Ele == H ->
  [R | _] = Set,
  R.

%%vypíše prvky množiny (io:format)
%%todo, make it more pro?
show(Set) when not (Set == []) ->
  io:format("set contains:"),
  printList(Set),
  io:format("~n"),
  ok;
show([]) ->
  io:format("set contains: ~n"),
  ok.

printList([H | Set]) when Set /= [] ->
  io:format("~p, ", [H]),
  printList(Set);
printList([T]) ->
  io:format("~p", [T]);
printList([]) ->
  nil.


%% porovná obsah dvoch množín
equals([H1 | R1] = S1, [H2 | R2] = S2) when S1 /= [], S2 /= [] ->
  if
    H1 /= H2 -> false;
    true -> equals(R1, R2)
  end;
equals([], []) ->
  true;
equals(_, _) ->
  false.


%% vráti najväčší prvok množiny Set
max(Set) when Set /= [] ->
  [H | T] = Set,
  if
    T /= [] -> max(T);
    true -> H
  end;
max([]) ->
  nil.


%% vráti najmenší prvok množiny Set
min(Set) when Set /= [] ->
  [H | _] = Set,
  H;
min([]) ->
  nil.


%% vráti počet prvkov množiny
card(Set) ->
  cardTail(Set, 0).

cardTail([_ | T] = Set, Count) ->
  if
    Set == [] -> Count;
    true -> cardTail(T, Count + 1)
  end;
cardTail([], Count) ->
  Count.

%% otestuje, či sa prvok Ele nachádza v množine Set
isin([H | R], Ele) ->
  if
    Ele == H -> true;
    true -> isin(R, Ele)
  end;
isin([], _) ->
  false.


%% test, či aspoň jeden prvok množiny Set spĺňa predikát Fnc
any([H | R], Fnc) ->
  Res = Fnc(H),
  if
    Res == true -> true;
    true -> any(R, Fnc)
  end;
any([], _) ->
  false.


%% test, či všetky prvky množiny Set vyhovujú predikátu Fnc
all([H | R], Fnc) ->
  Res = Fnc(H),
  if
    Res == false -> false;
    true -> all(R, Fnc)
  end;
all([], _Fnc) ->
  true.

%%  produkt kartézskeho súčinu množín, kde výsledok súčinu dvoch prvkov a∈Set1, b∈Set2 bude Fnc(a,b)
product(Set1, Set2, Fnc) ->
  toSet([Fnc(X, Y) || X <- Set1, Y <- Set2]).

%% aplikuje funkciu Fnc na každý prvok množiny Set a vrati zoznam prvkov množiny po aplikovaní funkcie Fnc
map(Set1, Fnc) ->
  [Fnc(X) || X <- Set1].

%% vyberie prvky z množiny Set na základe predikátu Fnc
filter(Set1, Fnc) ->
  [X || X <- Set1, Fnc(X)].

%%  urobí fold na množine Set aplikovaním funkcie Fnc, kde inicializačná hodnota bude Acc0
foldl([H | Set], Fnc, Acc0) when Set /= [] ->
  Acc = Fnc(H, Acc0),
  if
    Set == [] -> Acc;
    true -> foldl(Set, Fnc, Acc)
  end;
foldl([H | Set], Fnc, Acc0) when Set == [] ->
  Fnc(H, Acc0);
foldl([], _Fnc, Acc0) ->
  Acc0.



%%  spraví spojenie dvoch množín
union(Set1, Set2) when is_list(Set1), is_list(Set2) -> %% ak je v 1. mnozine vacsi prvok, tak si ho necham na neskor - celu mnozinu a pridam prvy prvok z druhej mnoziny
  p_union(Set1, Set2, []).
p_union([H1 | _] = Set1, [H2 | R2], Acc) when is_integer(H1), is_integer(H2), H1 > H2 -> %% ak je v 1. mnozine vacsi prvok, tak si ho necham na neskor - celu mnozinu a pridam prvy prvok z druhej mnoziny
  p_union(Set1, R2, [H2 | Acc]);                                                          %% POZOR otocim listy pretoze predpokladam ze sa to moze opakovat
p_union([H1 | R1], [H2 | _] = Set2, Acc) when is_integer(H1), is_integer(H2), H1 < H2 -> %% ak je v 1. mnozine mensi prvok, tak ho pridam do vysledku, odrezem z 1. mnoziny, a druhu mnozinu pouzijem celu
  p_union(R1, Set2, [H1 | Acc]);
p_union([H1 | Set1], [H2 | Set2], Acc) when is_integer(H1), is_integer(H2) -> %% ak mam rovnaky prvok - tak ho odrezem z oboch mnozin, pouzijem napr H1 -mozem aj H2
  p_union(Set1, Set2, [H1 | Acc]);
p_union([], Set, []) ->
  Set;
p_union(Set, [], []) ->
  Set;
p_union([H1 | R1] = Set, [], Acc) when Set /= [] ->
  p_union(R1, [], [H1 | Acc]);
p_union([], [H1 | R1] = Set, Acc) when Set /= [] ->
  p_union(R1, [], [H1 | Acc]);
p_union([], [], Acc) ->
  reverse(Acc).

%% spraví prienik dvoch množín
%%intersect(Set1, Set2) when is_list(Set1), is_list(Set2) ->
%%filter(Set1, fun(X) -> isin(Set2, X) end).

%% spraví prienik dvoch množín
intersect(Set1,Set2) when is_list(Set1), is_list(Set2) ->
reverse(p_intersect(Set1, Set2, [])).
p_intersect([H1 | _] = Set1, [H2 | R2], Acc) when is_integer(H1), is_integer(H2), H1 > H2 -> %% pripad kedy osekavam prvu mnozinu, je tam mensi prvok
  p_intersect(Set1, R2, Acc);                                                                %% switch lebo to porastie
p_intersect([H1 | R1], [H2 | _] = Set2, Acc) when is_integer(H1), is_integer(H2), H1 < H2 -> %% pripad kedy osekavam druhu mnozinu, je tam mensi prvok
  p_intersect(R1, Set2, Acc);
p_intersect([H1 | Set1], [H2 | Set2], Acc) when is_integer(H1), is_integer(H2) -> %% pripad kedy osekavam obe mnoziny a pridam H1 do vysledku
  p_intersect(Set1, Set2, [H1 | Acc]);
p_intersect([], _, Acc) ->
  Acc;
p_intersect(_, [], Acc) ->
  Acc.

%% spraví rozdiel množín Set1 - Set2
%%diff(Set1, Set2) when is_list(Set1), is_list(Set2) ->
%%  filter(Set1, fun(X) -> not isin(Set2, X) end).

%% spraví rozdiel množín Set1 - Set2
diff(Set1, Set2) when is_list(Set1), is_list(Set2) ->
  reverse(p_diff(Set1, Set2, [])).
p_diff([H1 | R1], [H2 | R2], Acc) when is_integer(H1), is_integer(H2), H1 > H2 -> %% odsekavam z prvej, je tam mensi prvok
  p_diff(R1, R2, [H1 | Acc]);
p_diff([H1 | R1], [H2 | _] = Set2, Acc) when is_integer(H1), is_integer(H2), H1 < H2 -> %% odsekavam z druhej, tam je mensi prvok
  p_diff(R1, Set2, [H1 | Acc]);
p_diff([_ | R1], [_ | R2], Acc) ->
  p_diff(R1, R2, Acc);
p_diff( Set1, [], Acc) ->
  reverse(union(Set1, reverse(Acc)));
p_diff([], _, Acc) ->
  Acc.

reverse(List) ->
  reverse(List, []).
reverse([H | T], Acc) ->
  reverse(T, [H | Acc]);
reverse([], Acc) ->
  Acc.







