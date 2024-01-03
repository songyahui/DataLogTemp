from symlog.shortcuts import (
    SymbolicConstant,
    String,
    Fact,
    SymbolicSign,
    symex,
    load_facts,
    parse,
    substitute,
)
from symlog.souffle import SYM

# the reachability rules
rules = parse("example/reachability.dl")

# the edge facts
facts = load_facts("example/", rules.declarations)

alpha = SymbolicConstant("alpha", SYM)

facts = set(
    map(
        lambda fact: SymbolicSign(substitute(fact, {String("a"): alpha})),
        facts,
    )
)

facts = {SymbolicSign(f) if str(f) == 'edge("b", "c").' else f for f in facts}

interested_fact = Fact("reachable", [String("a"), String("b")])

# get constraints
constraints = symex(rules, facts, {interested_fact})

print(constraints)

# {reachable("a", "b").: And(alpha == "a", edge("a", "b").)}
