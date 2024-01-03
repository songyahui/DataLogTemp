from symlog.shortcuts import (
    SymbolicConstant,
    String,
    Literal,
    Variable,
    Rule,
    Fact,
    SymbolicSign,
    symex,
)
from symlog.souffle import SYM

# the reachability rules
edge_relation1 = Literal(
    "edge", [Variable("x"), Variable("y")]
)  # default sign is positive
reachable_relation1 = Literal("reachable", [Variable("x"), Variable("y")])

edge_relation2 = Literal("edge", [Variable("x"), Variable("y")])
reachable_relation2 = Literal("reachable", [Variable("x"), Variable("z")])
reachable_relation3 = Literal("reachable", [Variable("y"), Variable("z")])

rules = [
    Rule(reachable_relation1, [edge_relation1]),  # reachable(x, y) :- edge(x, y).
    Rule(
        reachable_relation2, [edge_relation2, reachable_relation3]
    ),  # reachable(x, z) :- edge(x, y), reachable(y, z).
]

# the edge facts
edge_fact1 = SymbolicSign(
    Fact("edge", [SymbolicConstant("alpha", type=SYM), String("b")])
)
edge_fact2 = SymbolicSign(Fact("edge", [String("b"), String("c")]))  # edge('b', 'c').
edge_fact3 = Fact("edge", [String("c"), String("b")])  # edge('c', 'b').
edge_fact4 = Fact("edge", [String("c"), String("d")])  # edge('c', 'd').

facts = [edge_fact1, edge_fact2, edge_fact3, edge_fact4]

interested_fact = Fact("reachable", [String("a"), String("b")])

# get constraints
constraints = symex(rules, facts, {interested_fact})

print(constraints)
