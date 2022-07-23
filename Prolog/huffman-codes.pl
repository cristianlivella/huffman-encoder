%%%% -*- Mode: Prolog -*-

%%% 866169 Cristian Livella
%%% Simple huffman encoder library

%%% UTILS

%%% list_contain/2
%%% list_contains(Haystack, Needle) means 'Needle is contained in Haystack'.

list_contain([ToFind | _], ToFind) :- !.
list_contain([_ | Tail], ToFind) :- list_contain(Tail, ToFind).

%%% create_nodes/2
%%% create_nodes(SymbolAndWeights, Nodes)
%%% Nodes is a list of nodes (leaves) created from SymbolAndWeights.

create_nodes([(Symbol, Weight)], [(Weight, [Symbol])]).
create_nodes([(Symbol, Weight) | Tail], [(Weight, [Symbol]) | NodesTail]) :- create_nodes(Tail, NodesTail).

%%% find_node_weight/3
%%% find_node_weight(Nodes, Children, Weight)
%%% The list Nodes contains a node with weight = Weight and children = Children.

find_node_weight([(Weight, Children) | _], Children, Weight).
find_node_weight([_ | Tail], Children, Weight) :- find_node_weight(Tail, Children, Weight).

%%% get_nodes_with_sorted_weights/4
%%% get_nodes_with_sorted_weights(OriginalNodes, ChildrenList, Weights, SortedNodes)
%%% SortedNodes is a list that contain the same couples
%%% contained in OriginalNodes, ordered by weights according to Weights list.

get_nodes_with_sorted_weights(OriginalNodes, ChildrenList, [Weight | WeightTail], [(Weight, Children) | TailSortedNodes]) :-
	find_node_weight(OriginalNodes, Children, Weight),
	list_contain(ChildrenList, Children),
	delete(ChildrenList, Children, NewChildrenList),
	get_nodes_with_sorted_weights(OriginalNodes, NewChildrenList, WeightTail, TailSortedNodes),
	!.

get_nodes_with_sorted_weights(_, [Children], [Weight], [(Weight, Children)]) :- !.

%%% extract_weight_node/3
%%% extract_weight_node(Node, Weight, Children)
%%% Node has weight = Weight and contains Children.

extract_weight_node((Weight, Children), Weight, Children).

%%% extract_weights_children/3
%%% extract_weights_children(Nodes, Weights, Children)
%%% The list of nodes SymbolsAndWeights is splitted
%%% in a list of Weights and a list of Children.

extract_weights_children([], [], []).
extract_weights_children([Node | Tail], [Weight | TailWeights], [Nodes | TailNodes]) :-
	extract_weight_node(Node, Weight, Nodes),
	extract_weights_children(Tail, TailWeights, TailNodes).

%%% sort_nodes/2
%%% sort_nodes(Nodes, SortedNodes)
%%% SortedNodes contains the same nodes
%%% contained in Nodes, order by weight.

sort_nodes(Nodes, SortedNodes) :-
	extract_weights_children(Nodes, Weights, SubNodes),
	sort(0, '@=<', Weights, SortedWeights),
	get_nodes_with_sorted_weights(Nodes, SubNodes, SortedWeights, SortedNodes).

%%% prepend_new_node/5
%%% prepend_new_node(FirstNode, SecondNode, SumWeight, Tail, NewNodes).
%%% NewNodes contains the nodes in Tail, prepended with a new node
%%% with weight = SumWeight and with children FirstNode and SecondNode.

prepend_new_node(FirstNode, SecondNode, SumWeight, Tail, [(SumWeight, [FirstNode, SecondNode]) | Tail]).

%%% group_first_two_nodes/3
%%% group_first_two_nodes(Nodes, NewNodes).
%%% NewNodes contains the same nodes as in Nodes,
%%% but with the first two nodes moved as children of a new prepended node.

group_first_two_nodes([(FirstW, FirstC), (SecondW, SecondC) | Rest], Node) :-
	SumWeight is FirstW + SecondW,
	prepend_new_node((FirstW, FirstC), (SecondW, SecondC), SumWeight, Rest, Node).

%%% generate_huffman_tree/2
%%% generate_huffman_tree(Nodes, Tree)
%%% Tree is a huffman tree generated from Nodes.

generate_huffman_tree([Node], Node).

generate_huffman_tree(Nodes, Final) :-
	sort_nodes(Nodes, SortedNodes),
	group_first_two_nodes(SortedNodes, Partial),
	generate_huffman_tree(Partial, Final).

%%% union/3
%%% union(L1, L2, L3)
%%% L3 is the union of L1, L2.

union([], [], []).
union([L1 | L1T], L2, [L1 | L3T]) :- union(L1T, L2, L3T), !.
union([], [L2 | L2T], [L2 | L3T]) :- union([], L2T, L3T).

%%% generate_symbol_bits_table/3
%%% generate_symbol_bits_table(Nodes, Prefix, SymbolBitsTable)
%%% SymbolBitsTable is a list containg couples (Symbol, Bits).

generate_symbol_bits_table([Node], Prefix, [(Node, Prefix)]) :-
	atom(Node).

generate_symbol_bits_table([(_, ChildrenA), (_, ChildrenB)], Prefix, Solution) :-
	union(Prefix, [0], NodeAPrefix),
	union(Prefix, [1], NodeBPrefix),
	generate_symbol_bits_table(ChildrenA, NodeAPrefix, Res1),
	generate_symbol_bits_table(ChildrenB, NodeBPrefix, Res2),
	union(Res1, Res2, Solution), !.

%%% get_bits_for_symbol/3
%%% get_bits_for_symbol(SymbolBitsTable, Symbol, Bits)
%%% Find the Bits of a given Symbol (or the Symbol of a given Bits list)
%%% by looking up in the SymbolBitsTable.

get_bits_for_symbol([(FirstSymbol, FirstBits) | _], FirstSymbol, FirstBits) :- !.
get_bits_for_symbol([_ | TailSymbolBitsTable], Symbol, Bits) :- get_bits_for_symbol(TailSymbolBitsTable, Symbol, Bits).

%%% decode/4
%%% decode(Bits, HeadBits, SymbolBitsTable, Message)
%%% Message is the decoded Bits, using the provided SymbolBitsTable.
%%% HeadBits is used internally, it's initially an empty list.

decode([Bit | Tail], OtherBits, SymbolBitsTable, [Symbol | Message]) :-
	union( OtherBits, [Bit], Res),
	get_bits_for_symbol(SymbolBitsTable, Symbol, Res),
	decode(Tail, [], SymbolBitsTable, Message),
	!.

decode([Bit | Tail], OtherBits, SymbolBitsTable, Message) :-
	union(OtherBits, [Bit], Res),
	decode(Tail, Res, SymbolBitsTable, Message),
	!.

decode(Bits, [], SymbolBitsTable, [Symbol | []]) :-
	get_bits_for_symbol(SymbolBitsTable, Symbol, Bits).

%%% encode/3
%%% encode(Message, SymbolBitsTable, Bits)
%%% Bits is the encoded Message, using the provided SymbolBitsTable.

encode([], _, []).

encode([Symbol | TailMessage], SymbolBitsTable, TailBits) :-
	get_bits_for_symbol(SymbolBitsTable, Symbol, Bits),
	encode(TailMessage, SymbolBitsTable, TailBits2),
	union(Bits, TailBits2, TailBits).

%%% read_line/2
%%% read_line(Stream List)
%%% List is the list of symbols read in a line from the stream.

read_line(Stream, List) :-
	read_line_to_codes(Stream, Line),
	atom_codes(String, Line),
	string_chars(String, List).

%%% read_stream/2
%%% read_stream(Stream, List)
%%% List is a list containing the list of symbols of the file lines.

read_stream(Stream, []) :-
    at_end_of_stream(Stream), !.

read_stream(Stream, List) :-
    read_line(Stream, FirstList),
    read_stream(Stream, OtherList),
	union(FirstList, ['\n'], Partial1),
	union(Partial1, OtherList, List).

%%% write_spaces/1
%%% write_spaces(Count)
%%% Write on screen the given number of spaces.

write_spaces(0).

write_spaces(Count) :-
	Count > 0,
	NewCount is Count - 1,
	write(' '),
	write_spaces(NewCount),
	!.

%%% print_first_half_line/2
%%% print_first_half_line(IndentLevel, Weight)
%%% Print the first part of a line of the huffman tree rappresentation.

print_first_half_line(IndentLevel, Weight) :-
	Spaces is IndentLevel * 4,
	write_spaces(Spaces),
	write('('),
	write(Weight),
	write(')').

%%% print_huffman_tree/2
%%% print_huffman_tree(HuffmanTree, IndentLevel)
%%% Print the HuffmanTree provided, with the IndentLevel specified.

print_huffman_tree((Weight, [Child1, Child2]), IndentLevel) :-
	NextIndentLevel is IndentLevel + 1,
	print_first_half_line(IndentLevel, Weight),
	write('\n'),
	print_huffman_tree(Child1, NextIndentLevel),
	print_huffman_tree(Child2, NextIndentLevel),
	!.

print_huffman_tree((Weight, [Child]), IndentLevel) :-
	print_first_half_line(IndentLevel, Weight),
	write(' '),
	write(Child),
	write('\n'),
	!.

%%% he_decode/3
%%% he_decode(Bits, HuffmanTree, Message)
%%% Message is the decoded Bits, using the provided HuffmanTree.

he_decode(Bits, HuffmanTree, Message) :-
	he_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable),
	decode(Bits, [], SymbolBitsTable, Message).

%%% he_encode/3
%%% he_encode(Message, HuffmanTree, Bits)
%%% Bits is the encoded Message, using the provided HuffmanTree.

he_encode(Message, HuffmanTree, Bits) :-
	he_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable),
	encode(Message, SymbolBitsTable, Bits).

%%% he_encode_file(File, HuffmanTree, Bits)
%%% Bits is the encoded content of the file named File,
%%% using the provided HuffmanTree.

he_encode_file(File, HuffmanTree, Bits) :-
	open(File, read, Stream),
	read_stream(Stream, Message),
	close(Stream),
	he_encode(Message, HuffmanTree, Bits).

%%% he_generate_huffman_tree/2
%%% he_generate_huffman_tree(SymbolsAndWeights, HuffmanTree)
%%% HuffmanTree is a huffman tree generated from SymbolsAndWeights.

he_generate_huffman_tree(SymbolsAndWeights, HuffmanTree) :-
	create_nodes(SymbolsAndWeights, Nodes),
	sort_nodes(Nodes, SortedNodes),
	generate_huffman_tree(SortedNodes, HuffmanTree),
	!.

%%% he_generate_symbol_bits_table/2
%%% he_generate_huffman_tree(HuffmanTree, SymbolBitsTable)
%%% SymbolBitsTable is a list containg couples (Symbol, Bits),
%%% generated from the HuffmanTree provided.

he_generate_symbol_bits_table((_, Nodes), SymbolBitsTable) :-
	generate_symbol_bits_table(Nodes, [], SymbolBitsTable).

%%% he_print_huffman_tree/1
%%% he_print_huffman_tree(HuffmanTree)
%%% Print the given HuffmanTree.

he_print_huffman_tree(HuffmanTree) :-
	print_huffman_tree(HuffmanTree, 0).

%%%% end of file -- huffman-codes.pl --
