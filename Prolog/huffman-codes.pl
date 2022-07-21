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


find_node_weight([[Weight, Symbol] | _], Symbol, Weight).
find_node_weight([_ | Tail], Symbol, Weight) :- find_node_weight(Tail, Symbol, Weight).

% get_nodes_with_sorted_weights(_, _, [], _).

get_nodes_with_sorted_weights(OriginalSymbolsAndWeights, Symbols, [Weight | WeightTail], [[Weight, Symbol] | TailNewSymbolAndWeights]) :-
	find_node_weight(OriginalSymbolsAndWeights, Symbol, Weight),
	list_contain(Symbols, Symbol),
	delete(Symbols, Symbol, NewSymbols),
	get_nodes_with_sorted_weights(OriginalSymbolsAndWeights, NewSymbols, WeightTail, TailNewSymbolAndWeights),
	!.

get_nodes_with_sorted_weights(OriginalSymbolsAndWeights, [Symbol], [Weight], [[Weight, Symbol]]) :- !.

extract_weight_node([Symbol, Weight], Symbol, Weight).

extract_weights_nodes([], [], []).
extract_weights_nodes([Node | Tail], [Weight | TailWeights], [Nodes | TailNodes]) :-
	extract_weight_node(Node, Weight, Nodes),
	extract_weights_nodes(Tail, TailWeights, TailNodes).

sort_nodes(Nodes, SortedNodes) :-
	extract_weights_nodes(Nodes, Weights, SubNodes),
	sort(0, '@=<', Weights, SortedWeights),
	get_nodes_with_sorted_weights(Nodes, SubNodes, SortedWeights, SortedNodes).

create_new_node(FirstW, FirstS, SecondW, SecondS, SumWeight, Tail, [[SumWeight, [[FirstW, FirstS], [SecondW, SecondS]]] | Tail]).
%create_new_node(FirstW, FirstS, SecondW, SecondS, SumWeight, [[SumWeight, [[FirstW, FirstS], [SecondW, SecondS]]]]).

%real_generate_huffman([[FirstW, FirstS], [SecondW, SecondS]], Node) :-
%	SumWeight is FirstW + SecondW,
%	create_node_node(FirstW, FirstS, SecondW, SecondS, SumWeight, Node).

real_generate_huffman([[FirstW, FirstS], [SecondW, SecondS] | Rest], Node) :-
	SumWeight is FirstW + SecondW,
	create_new_node(FirstW, FirstS, SecondW, SecondS, SumWeight, Rest, Node).

final_generate_huffman([Node], Node).

final_generate_huffman(Nodes, Final) :-
	sort_nodes(Nodes, SortedNodes),
	real_generate_huffman(SortedNodes, Partial),
	final_generate_huffman(Partial, Final).

he_generate_huffman_tree(SymbolsAndWeights, HuffmanTree) :-
	%sort_symbols_and_weights(SymbolsAndWeights, SortedSymbolAndWeights),
	create_nodes(SymbolsAndWeights, Nodes),
	sort_nodes(Nodes, SortedNodes),
	final_generate_huffman(SortedNodes, HuffmanTree),
	!.

union([], [], []).
union([L1 | L1T], L2, [L1 | L3T]) :- union(L1T, L2, L3T), !.
union([], [L2 | L2T], [L2 | L3T]) :- union([], L2T, L3T).

subnode_generate_symbol_bits([NodeA], Prefix, [[NodeA, Prefix]]) :-
	atom(NodeA).
	%union([Prefix], [], NodeABits),
	%write(Prefix),
	%write('\n'),
	%write(NodeABits),
	%write('\n\n').
	%string_concat(Prefix, '', NodeABits).

%subnode_generate_symbol_bits([NodeA, NodeB], Prefix, [[[NodeA, NodeABits], [NodeB, NodeBBits]]], _, _) :-
%	atom(NodeA),
%	atom(NodeB),
%	string_concat(Prefix, '', NodeABits),
%	string_concat(Prefix, '', NodeBBits).

%subnode_generate_symbol_bits([_, [[_, SubNodesA], [_, SubNodesB]]], Prefix, _, _, _) :-
%	write('A \n').

subnode_generate_symbol_bits([[_, SubNodesA], [_, SubNodesB]], Prefix, Solution) :-
	%string_concat(Prefix, '0', NodeAPrefix),
	union(Prefix, ['0'], NodeAPrefix),
	union(Prefix, ['1'], NodeBPrefix),
	%string_concat(Prefix, '1', NodeBPrefix),
	subnode_generate_symbol_bits(SubNodesA, NodeAPrefix, Res1),
	subnode_generate_symbol_bits(SubNodesB, NodeBPrefix, Res2),
	union(Res1, Res2, Solution), !.
				    
	
% real_generate_symbol_bits_table([_, SubNodes], Prefix, SymbolBitsTable) :-

get_bits_for_symbol([[FirstSymbol, FirstBits] | TailSymbolBitsTable], FirstSymbol, FirstBits) :- !.
get_bits_for_symbol([FirstSymbolBitsTable | TailSymbolBitsTable], Symbol, Bits) :- get_bits_for_symbol(TailSymbolBitsTable, Symbol, Bits).
	

encode_real([], _, []).

encode_real([Symbol | TailMessage], BitsTable, TailBits) :-
	get_bits_for_symbol(BitsTable, Symbol, Bits),
	encode_real(TailMessage, BitsTable, TailBits2),
	write(Bits),
	write('\n'),
	write(TailBits2),
	write('\n'),
	write(TailBits),
	write('\n\n'),
	union(Bits, TailBits2, TailBits).


decode_real([Bit | Tail], OtherBits, BitsTable, [Symbol | Message]) :- write('\nA: '), write(Bit), write('  '), write(OtherBits), union( OtherBits, [Bit], Res), get_bits_for_symbol(BitsTable, Symbol, Res), decode_real(Tail, [], BitsTable, Message), !.
decode_real([Bit | Tail], OtherBits, BitsTable, Message) :- write('\nB: '), union(OtherBits, [Bit], Res), decode_real(Tail, Res, BitsTable, Message), !.
decode_real(Bits, [], BitsTable, [Symbol | []]) :- write('\nC: '), get_bits_for_symbol(BitsTable, Symbol, Bits).

read_line(Stream, List) :-
	read_line_to_codes(Stream, Line),
	atom_codes(List, Line).
	%atomic_list_concat(As, ' ' , A),
	%maplist(string_chars, As, List).

read_file(Stream,[]) :-
    at_end_of_stream(Stream), !.

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line(Stream,X),
    read_file(Stream,L).

lines_to_chars([], []).

lines_to_chars([Line | LineTail], [Chars | CharsTail]) :-
	string_chars(Line, Chars),
	lines_to_chars(LineTail, CharsTail).

%flat_chars_list(_, _).
flat_chars_list([], []).
%flat_chars_list([Item], Item).
%flat_chars_list(Line1, Line1).
flat_chars_list([Line1, Line2 | LineTail], Final) :-
	union(Line1, ['\n'], Partial1),
	union(Partial1, Line2, Partial2),
	%write(Partial2),
	%write('-y'),
	union([Partial2], LineTail, Partial4),
	%write('-z'),
	%write(Partial4),
	flat_chars_list(Partial4, Partial3),
	%write(Partial3),
	%write('-x'),
				%union(Partial2, Partial3, Final),
	Final = Partial3, !.
	%write('***'),
	%write(Final),
	%write('***'), !.
	%flat_chars_list(Line1, Line1).

flat_chars_list([Line1], Line1).
main2 :-
    open('file.txt', read, Str),
    read_file(Str,Lines),
    close(Str),
    lines_to_chars(Lines, Chars),
    flat_chars_list(Chars, Chars2),
    write(Chars2), nl.

he_encode_file(BitTable, Res) :-
	open('file.txt', read, Str),
	read_file(Str,Lines),
	close(Str),
	lines_to_chars(Lines, Chars),
	flat_chars_list(Chars, Chars2),
	encode_real(Chars2, BitTable, Res).

%he_encode(Message, HuffmanTree, Bits) :-
	

% he_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable) :-
	

%%%% end of file -- huffman-codes.pl --