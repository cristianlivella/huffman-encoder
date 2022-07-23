%%%% -*- Mode: Prolog -*-

%%% 866169 Cristian Livella
%%% Simple huffman encoder library

%%% UTILS

%%% find_symbol_weight/3
%%% find_symbol_weight(SymbolsAndWeights, Symbol, Weight)
%%% Find the SymbolAndWeights couples that match the given Symbol or Weight.

find_symbol_weight([(Symbol, Weight) | _], Symbol, Weight).
find_symbol_weight([_ | Tail], Symbol, Weight) :- find_symbol_weight(Tail, Symbol, Weight).

%%% list_contain/2
%%% list_contains(Haystack, Needle) means 'Needle is contained in Haystack')

list_contain([ToFind | _], ToFind) :- !.
list_contain([_ | Tail], ToFind) :- list_contain(Tail, ToFind).

%%% create_nodes/2
%%% create_nodes(SymbolAndWeights, Nodes)
%%% Nodes is a list of nodes (leaves) created from SymbolAndWeights.

create_nodes([(Symbol, Weight)], [[Weight, [Symbol]]]).
create_nodes([(Symbol, Weight) | Tail], [[Weight, [Symbol]] | NodesTail]) :- create_nodes(Tail, NodesTail).

%%% find_node_weight/3
%%% find_node_weight(Nodes, Symbol, Weight)
%%% The list Nodes contains a node with wheight = Weight and children = Symbol.

find_node_weight([[Weight, Symbol] | _], Symbol, Weight).
find_node_weight([_ | Tail], Symbol, Weight) :- find_node_weight(Tail, Symbol, Weight).

%%% get_nodes_with_sorted_weights/4
%%% get_nodes_with_sorted_weights(OriginalSymbolsAndWeights, Symbols, Weights, SortedSymbolsAndWeights)
%%% SortedSymbolsAndWeights is a list that contain the same couples
%%% contained in OriginalSymbolAndWeights, ordered by weights
%%% according to Weights list.

get_nodes_with_sorted_weights(OriginalSymbolsAndWeights, Symbols, [Weight | WeightTail], [[Weight, Symbol] | TailNewSymbolAndWeights]) :-
	find_node_weight(OriginalSymbolsAndWeights, Symbol, Weight),
	list_contain(Symbols, Symbol),
	delete(Symbols, Symbol, NewSymbols),
	get_nodes_with_sorted_weights(OriginalSymbolsAndWeights, NewSymbols, WeightTail, TailNewSymbolAndWeights),
	!.

get_nodes_with_sorted_weights(OriginalSymbolsAndWeights, [Symbol], [Weight], [[Weight, Symbol]]) :- !.

%%% extract_weight_node/3
%%% extract_weight_node(Node, Weight, Symbol)
%%% Node has weight = Weight and contains Symbol.

extract_weight_node([Weight, Symbol], Weight, Symbol).

%%% extract_weights_nodes/3
%%% extract_weights_nodes(Nodes, Weights, Children)
%%% The list of nodes SymbolsAndWeights is splitted
%%% in a list of Weights and a list of Children.

extract_weights_nodes([], [], []).
extract_weights_nodes([Node | Tail], [Weight | TailWeights], [Nodes | TailNodes]) :-
	extract_weight_node(Node, Weight, Nodes),
	extract_weights_nodes(Tail, TailWeights, TailNodes).

%%% sort_nodes/2
%%% sort_nodes(Nodes, SortedNodes)
%%% SortedNodes contains the same nodes
%%% contained in Nodes, order by weight.

sort_nodes(Nodes, SortedNodes) :-
	extract_weights_nodes(Nodes, Weights, SubNodes),
	sort(0, '@=<', Weights, SortedWeights),
	get_nodes_with_sorted_weights(Nodes, SubNodes, SortedWeights, SortedNodes).

%%% prepend_new_node/7
%%% prepend_new_node(FirstW, FirstC, SecondW, SecondC, SumWeight, Tail, NewNodes).
%%% NewNodes contains the nodes in Tail, prepended with a new node
%%% with weight = SumWeight and with children [FirstW, FirstC] and [SecondW, SecondC].

create_new_node(FirstW, FirstS, SecondW, SecondS, SumWeight, Tail, [[SumWeight, [[FirstW, FirstS], [SecondW, SecondS]]] | Tail]).
%create_new_node(FirstW, FirstS, SecondW, SecondS, SumWeight, [[SumWeight, [[FirstW, FirstS], [SecondW, SecondS]]]]).

%%% real_generate_huffman/3
%%% real_generate_huffman(Nodes, NewNodes).
%%% NewNodes contains the same nodes as in Nodes,
%%% but with the first two nodes moved as children of a new prepended node.

real_generate_huffman([[FirstW, FirstS], [SecondW, SecondS] | Rest], Node) :-
	SumWeight is FirstW + SecondW,
	create_new_node(FirstW, FirstS, SecondW, SecondS, SumWeight, Rest, Node).

%%% final_generate_huffman/2
%%% final_generate_huffman(Nodes, Tree)
%%% Tree is a huffman tree generated from Nodes.

final_generate_huffman([Node], Node).

final_generate_huffman(Nodes, Final) :-
	sort_nodes(Nodes, SortedNodes),
	real_generate_huffman(SortedNodes, Partial),
	final_generate_huffman(Partial, Final).

%%% union/3
%%% union(L1, L2, L3)
%%% L3 is the union of L1, L2.

union([], [], []).
union([L1 | L1T], L2, [L1 | L3T]) :- union(L1T, L2, L3T), !.
union([], [L2 | L2T], [L2 | L3T]) :- union([], L2T, L3T).

%%% generate_symbol_bits_table/3
%%% generate_symbol_bits_table(Nodes, Prefix, SymbolBitsTable)

generate_symbol_bits_table([Node], Prefix, [(Node, Prefix)]) :-
	atom(Node).

generate_symbol_bits_table([[_, SubNodesA], [_, SubNodesB]], Prefix, Solution) :-
	union(Prefix, [0], NodeAPrefix),
	union(Prefix, [1], NodeBPrefix),
	generate_symbol_bits_table(SubNodesA, NodeAPrefix, Res1),
	generate_symbol_bits_table(SubNodesB, NodeBPrefix, Res2),
	union(Res1, Res2, Solution), !.

% real_generate_symbol_bits_table([_, SubNodes], Prefix, SymbolBitsTable) :-

get_bits_for_symbol([(FirstSymbol, FirstBits) | TailSymbolBitsTable], FirstSymbol, FirstBits) :- !.
get_bits_for_symbol([FirstSymbolBitsTable | TailSymbolBitsTable], Symbol, Bits) :- get_bits_for_symbol(TailSymbolBitsTable, Symbol, Bits).

encode([], _, []).

encode([Symbol | TailMessage], BitsTable, TailBits) :-
	get_bits_for_symbol(BitsTable, Symbol, Bits),
	encode(TailMessage, BitsTable, TailBits2),
	union(Bits, TailBits2, TailBits).

decode([Bit | Tail], OtherBits, BitsTable, [Symbol | Message]) :-
	union( OtherBits, [Bit], Res),
	get_bits_for_symbol(BitsTable, Symbol, Res),
	decode(Tail, [], BitsTable, Message),
	!.

decode([Bit | Tail], OtherBits, BitsTable, Message) :-
	union(OtherBits, [Bit], Res),
	decode(Tail, Res, BitsTable, Message),
	!.

decode(Bits, [], BitsTable, [Symbol | []]) :-
	get_bits_for_symbol(BitsTable, Symbol, Bits).

read_line(Stream, List) :-
	read_line_to_codes(Stream, Line),
	atom_codes(List, Line).

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

flat_chars_list([], []).
flat_chars_list([Line1, Line2 | LineTail], Final) :-
	union(Line1, ['\n'], Partial1),
	union(Partial1, Line2, Partial2),
	union([Partial2], LineTail, Partial4),
	flat_chars_list(Partial4, Final),
	!.

flat_chars_list([Line1], Line1).

write_spaces(0).

write_spaces(Count) :-
	Count > 0,
	NewCount is Count - 1,
	write(' '),
	write_spaces(NewCount),
	!.

write_tabs(Count) :-
	SpaceCount is Count * 4,
	write_spaces(SpaceCount).

print_first_half_line(IndentLevel, Weight) :-
	write_tabs(IndentLevel),
	write('('),
	write(Weight),
	write(')').

he_decode(Bits, HuffmanTree, Message) :-
	he_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable),
	decode(Bits, [], SymbolBitsTable, Message).

he_encode(Message, HuffmanTree, Bits) :-
	he_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable),
	encode(Message, SymbolBitsTable, Bits).

he_encode_file(BitTable, Res) :-
	open('file.txt', read, Str),
	read_file(Str,Lines),
	close(Str),
	lines_to_chars(Lines, Chars),
	flat_chars_list(Chars, Chars2),
	encode_real(Chars2, BitTable, Res).

%%% he_generate_huffman_tree/2
%%% he_generate_huffman_tree(SymbolsAndWeights, HuffmanTree)
%%% HuffmanTree is a huffman tree generated from SymbolsAndWeights.

he_generate_huffman_tree(SymbolsAndWeights, HuffmanTree) :-
	create_nodes(SymbolsAndWeights, Nodes),
	sort_nodes(Nodes, SortedNodes),
	final_generate_huffman(SortedNodes, HuffmanTree),
	!.

he_generate_symbol_bits_table([_, Nodes], SymbolBitsTable) :-
	generate_symbol_bits_table(Nodes, [], SymbolBitsTable).

he_print_huffman_tree([Weight, [Child1, Child2]], IndentLevel) :-
	NextIndentLevel is IndentLevel + 1,
	print_first_half_line(IndentLevel, Weight),
	write('\n'),
	he_print_huffman_tree(Child1, NextIndentLevel),
	he_print_huffman_tree(Child2, NextIndentLevel),
	!.

he_print_huffman_tree([Weight, [Child]], IndentLevel) :-
	print_first_half_line(IndentLevel, Weight),
	write(' '),
	write(Child),
	write('\n'),
	!.

%%%% end of file -- huffman-codes.pl --
