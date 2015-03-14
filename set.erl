-module(set).
-export([show/1, newSet/0, toList/1, toSet/1, insert/2, delete/2, prec/2, succ/2, equals/2, min/1, max/1, card/1, isin/2, any/2, all/2, product/3, map/2, filter/2, foldl/3, diff/2, union/2, intersection/2]).

%% vytvorí prázdnu množinu
newSet() ->
  [].

%% vráti zoznam prvkov množiny ako usporiadaný zoznam
toList(Set) ->
  Set.

%% vytvorí množinu z prvkov zoznamu List
toSet([Ele | List]) when is_integer(Ele) ->
  insert(toSet(List), Ele);
toSet([Ele | List]) when not is_integer(Ele) ->
  toSet(List);
toSet([]) ->
  [].

%% do množiny Set pridá prvok Ele
insert([H | Set], Ele) when is_integer(Ele), Ele > H ->
  [H | insert(Set, Ele)];
insert([H | _] = Set, Ele) when is_integer(Ele), Ele < H ->
  [Ele | Set];
insert([H | _] = Set, Ele) when is_integer(Ele), Ele == H ->
  Set;
insert([], Ele) when is_integer(Ele) ->
  [Ele].

%% z množiny Set odoberie prvok Ele
delete([H | Set], Ele) when is_integer(Ele), Ele > H ->
  [H | delete(Set, Ele)];
delete([H | _] = Set, Ele) when is_integer(Ele), Ele < H ->
  Set;
delete([H | Set], Ele) when is_integer(Ele), Ele == H ->
  Set;
delete([], Ele) when is_integer(Ele) ->
  [].

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
union([H1 | _] = Set1, [H2 | Set2]) when H1 > H2 ->
  [H2 | union(Set1, Set2)];
union([H1 | Set1], [H2 | _] = Set2) when H1 < H2 ->
  [H1 | union(Set1, Set2)];
union([H1 | Set1], [H2 | Set2]) when H1 == H2 ->
  [H1 | union(Set1, Set2)];
union([], Set) ->
  Set;
union(Set, []) ->
  Set.

%% spraví prienik dvoch množín
intersection([H1 | _] = Set1, [H2 | Set2]) when is_integer(H1), is_integer(H2), H1 > H2 ->
  intersection(Set1, Set2);
intersection([H1 | Set1], [H2 | _] = Set2) when is_integer(H1), is_integer(H2), H1 < H2 ->
  intersection(Set1, Set2);
intersection([H1 | Set1], [H2 | Set2]) when is_integer(H1), is_integer(H2), H1 == H2 ->
  [H1 | intersection(Set1, Set2)];
intersection([], _) ->
  [];
intersection(_, []) ->
  [].

%% spraví rozdiel množín Set1 - Set2
diff([H1 | R1], [H2 | _] = Set2) when is_integer(H1), is_integer(H2), H1 < H2 ->
  [H1 | diff(R1, Set2)];
diff([H1 | _] = Set1, [H2 | R2]) when is_integer(H1), is_integer(H2), H1 > H2 ->
  diff(Set1, R2);
diff([_ | R1], [_ | R2]) ->
  diff(R1, R2);
diff(R1, []) ->
  R1;
diff([], _) ->
  [].







