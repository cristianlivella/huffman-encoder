%%%% -*- Mode: Prolog -*-

extract_symbol_weight([Symbol, Weight], Symbol, Weight).

extract_symbols_weights([], [], []).
extract_symbols_weights([Pair | Tail], [Symbol | TailSymbols], [Weight | TailWeights]) :-
	extract_symbol_weight(Pair, Symbol, Weight),
	extract_symbols_weights(Tail, TailSymbols, TailWeights).

find_symbol_weight([[Symbol, Weight] | _], Symbol, Weight).
find_symbol_weight([_ | Tail], Symbol, Weight) :- find_symbol_weight(Tail, Symbol, Weight).

% contain_symbol([Symbol | _], Symbol).
% contain_symbol([SymbolFound | TailSymbols], Symbol) :- contain_symbol(TailSymbols, Symbol).

list_contain([ToFind | _], ToFind) :- !.
list_contain([_ | Tail], ToFind) :- list_contain(Tail, ToFind).

get_pairs_by_sorted_weights(_, _, [], _).

get_pairs_by_sorted_weights(OriginalSymbolsAndWeights, Symbols, [Weight | WeightTail], [[Symbol, Weight] | TailNewSymbolAndWeights]) :-
	find_symbol_weight(OriginalSymbolsAndWeights, Symbol, Weight),
	list_contain(Symbols, Symbol),
	delete(Symbols, Symbol, NewSymbols),
	get_pairs_by_sorted_weights(OriginalSymbolsAndWeights, NewSymbols, WeightTail, TailNewSymbolAndWeights),
	!.

%extract_weights([], []).
%extract_weights([Pair | Tail], [Weight | TailWeights]) :- extract_weight(Pair, Weight), extract_weights(Tail, TailWeights).

sort_symbols_and_weights(SymbolsAndWeights, SortedSymbolAndWeights) :-
	extract_symbols_weights(SymbolsAndWeights, Symbols, Weights),
	sort(0, '@=<', Weights, SortedWeights),
	get_pairs_by_sorted_weights(SymbolsAndWeights, Symbols, SortedWeights, SortedSymbolAndWeights).
	%extract_symbols_weights(SortedSymbolAndWeights, Symbols, SortedWeights).

% sort_symbols_and_weights([[Symbol | Weight] | Tail], [_ | SortedWeigt] | SortedTail]) :- 

%create_nodes([], _).
%create_nodes([[Symbol, Weight] | Tail], [[Weight, [Symbol]] | NodesTail]) :- create_nodes(Tail, NodesTail).

create_nodes([[Symbol, Weight]], [[Weight, [Symbol]]]).
create_nodes([[Symbol, Weight] | Tail], [[Weight, [Symbol]] | NodesTail]) :- create_nodes(Tail, NodesTail).

he_generate_huffman_tree(SymbolsAndWeights, HuffmanTree) :-
	sort_symbols_and_weights(SymbolsAndWeights, SortedSymbolAndWeights),
	%create_nodes(SortedSymbolAndWeights, Nodes),
	HuffmanTree = SortedSymbolAndWeights,
	!.
	

%%%% end of file -- huffman-codes.pl --