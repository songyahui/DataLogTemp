from symlog.souffle import NUM, SYM
from symlog.shortcuts import (
    Rule,
    Fact,
    String,
    Variable,
    Literal,
    SymbolicSign,
    SymbolicConstant,
    Number,
    symex,
    parse,
    load_facts,
)

from z3 import And, Bool, Const, StringSort, BoolVal, simplify
import time


def test_symex_with_sym_sign():
    rule = Rule(
        Literal("t", [Variable("X"), Variable("Z")], True),
        [
            Literal("r", [Variable("X"), Variable("Y")], True),
            Literal("s", [Variable("Y"), Variable("Z")], True),
        ],
    )

    facts = [
        SymbolicSign(Fact("r", [String("a"), String("b")])),
        Fact("r", [String("b"), String("c")]),
        Fact("s", [String("b"), String("c")]),
        SymbolicSign(Fact("s", [String("c"), String("d")])),
    ]

    interested_output_facts = set([Fact("t", [String("a"), String("c")])])

    constraints = symex([rule], facts, interested_output_facts)
    constraints = {k: v.to_z3() for k, v in constraints.items()}

    answer = {Fact("t", [String("a"), String("c")]): simplify(Bool('r("a", "b").'))}

    assert constraints == answer


def test_symex_with_sym_const_sign():
    rule = Rule(
        Literal("t", [Variable("X"), Variable("Z")], True),
        [
            Literal("r", [Variable("X"), Variable("Y")], True),
            Literal("s", [Variable("Y"), Variable("Z")], True),
        ],
    )

    facts = [
        Fact("r", [SymbolicConstant("alpha", type=SYM), String("b")]),
        Fact("r", [String("b"), String("c")]),
        Fact("s", [String("b"), String("c")]),
        SymbolicSign(Fact("s", [String("c"), String("d")])),
    ]

    target_outputs = {Fact("t", [String("b"), String("d")])}

    constraints = symex([rule], facts, target_outputs)

    constraints = {k: v.to_z3() for k, v in constraints.items()}

    answer = {Fact("t", [String("b"), String("d")]): simplify(Bool('s("c", "d").'))}

    assert constraints == answer


def test_symex_with_multiple_sym_sign():
    rule = Rule(
        Literal("t", [Variable("X"), Variable("Z")], True),
        [
            Literal("r", [Variable("X"), Variable("Y")], True),
            Literal("s", [Variable("Y"), Variable("Z")], True),
        ],
    )

    facts = [
        SymbolicSign(Fact("r", [SymbolicConstant("alpha", type=SYM), String("b")])),
        Fact("r", [String("b"), String("c")]),
        Fact("s", [String("b"), String("c")]),
        SymbolicSign(Fact("s", [String("c"), String("d")])),
    ]

    target_outputs = {Fact("t", [String("a"), String("c")])}

    constraints = symex([rule], facts, target_outputs)
    constraints = {k: v.to_z3() for k, v in constraints.items()}
    answer = {
        Fact("t", [String("a"), String("c")]): simplify(
            And(
                [
                    Const("alpha", StringSort()) == "a",
                    Bool('r("a", "b").'),
                ]
            )
        )
    }

    assert constraints == answer


def test_symex_with_multiple_target_outputs():
    rule = Rule(
        Literal("t", [Variable("X"), Variable("Z")], True),
        [
            Literal("r", [Variable("X"), Variable("Y")], True),
            Literal("s", [Variable("Y"), Variable("Z")], True),
        ],
    )

    facts = [
        Fact("r", [SymbolicConstant("alpha", type=SYM), String("b")]),
        Fact("r", [String("b"), String("c")]),
        Fact("s", [String("b"), String("c")]),
        SymbolicSign(Fact("s", [String("c"), String("d")])),
    ]

    target_outputs = {
        Fact("t", [String("a"), String("c")]),
        Fact("t", [String("e"), String("c")]),
    }

    constraints = symex([rule], facts, target_outputs)

    updated_constraints = {k: v.to_z3() for k, v in constraints.items()}

    answer = {
        Fact("t", [String("a"), String("c")]): simplify(
            Const("alpha", StringSort()) == "a"
        ),
        Fact("t", [String("e"), String("c")]): simplify(
            Const("alpha", StringSort()) == "e"
        ),
    }

    assert updated_constraints == answer


def test_symex_with_nothing():
    rule = Rule(
        Literal("t", [Variable("X"), Variable("Z")], True),
        [
            Literal("r", [Variable("X"), Variable("Y")], True),
            Literal("s", [Variable("Y"), Variable("Z")], True),
        ],
    )

    facts = [
        Fact("r", [String("a"), String("b")]),
        Fact("r", [String("b"), String("c")]),
        Fact("s", [String("b"), String("c")]),
        Fact("s", [String("c"), String("d")]),
    ]

    target_outputs = {
        Fact("t", [String("a"), String("c")]),
        Fact("t", [String("e"), String("c")]),
    }

    constraints = symex([rule], facts, target_outputs)

    updated_constraints = {k: v.to_z3() for k, v in constraints.items()}

    answer = {
        Fact("t", [String("a"), String("c")]): BoolVal(True),
    }

    assert updated_constraints == answer


def test_symex_with_number_types():
    rule = Rule(
        Literal("t", [Variable("X"), Variable("Z")], True),
        [
            Literal("r", [Variable("X"), Variable("Y")], True),
            Literal("s", [Variable("Y"), Variable("Z")], True),
        ],
    )

    facts = [
        Fact("r", [SymbolicConstant("alpha", type=SYM), Number(1)]),
        Fact("r", [String("1"), Number(3)]),
        Fact("s", [Number(1), Number(2)]),
        Fact("s", [Number(2), Number(3)]),
    ]

    target_outputs = {
        Fact("t", [String("1"), Number(2)]),
    }

    constraints = symex([rule], facts, target_outputs)

    updated_constraints = {k: v.to_z3() for k, v in constraints.items()}

    answer = {
        Fact("t", [String("1"), Number(2)]): simplify(
            And(
                [
                    Const("alpha", StringSort()) == "1",
                ]
            )
        ),
    }

    assert updated_constraints == answer


def test_chart_4():
    the_program_path = "tmp/stratum_2.dl"

    the_program = parse(the_program_path)

    # TODO: load facts with decls
    the_fact_path = "tmp/chart-4"
    facts = load_facts(the_fact_path, the_program.declarations, the_program.inputs)

    target_fact = Fact(
        "MustNonNullAt",
        [
            Number(92),
            String(
                "<org.jfree.chart.plot.XYPlot: org.jfree.data.Range"
                " getDataRange(org.jfree.chart.axis.ValueAxis)>/org.jfree.chart.renderer.xy.XYItemRenderer.getAnnotations/0"
            ),
            String(
                "<org.jfree.chart.plot.XYPlot: org.jfree.data.Range"
                " getDataRange(org.jfree.chart.axis.ValueAxis)>/r#_4473"
            ),
            String(
                "<org.jfree.chart.plot.XYPlot: org.jfree.data.Range"
                " getDataRange(org.jfree.chart.axis.ValueAxis)>"
            ),
        ],
    )

    a1 = SymbolicConstant("a1", type=SYM)

    added_facts = {
        Fact(
            "OperatorAt",
            [a1, SymbolicConstant("a2", type=SYM)],
        ),
        Fact(
            "If_Var",
            [
                a1,
                String("_"),
                SymbolicConstant("a3", type=SYM),
            ],
        ),
        Fact(
            "If_Constant",
            [
                a1,
                String("_"),
                SymbolicConstant("a4", type=SYM),
            ],
        ),
        Fact(
            "JumpTarget",
            [SymbolicConstant("a5", type=SYM), a1],
        ),
        Fact("Instruction_Next", [a1, SymbolicConstant("a6", type=SYM)]),
    }

    facts = facts.union(added_facts)
    # symbolise the sign of the facts
    facts = {SymbolicSign(f) for f in facts}

    start = time.time()
    constraints = symex(the_program, facts, {target_fact})

    print(f"Time taken: {time.time() - start}")

    # dump the constraints
    with open("tmp/chart-4-constraints-6-sym_consts.txt", "w") as f:
        for k, v in constraints.items():
            f.write(f"{k} -> {v.to_z3().sexpr()}\n")


def test_Bears_56():
    the_program_path = "tmp/stratum_2.dl"

    the_program = parse(the_program_path)

    # TODO: load facts with decls
    the_fact_path = "tmp/Bears-56"
    facts = load_facts(the_fact_path, the_program.declarations, the_program.inputs)

    target_fact = Fact(
        "MustNonNullAt",
        [
            Number(4),
            String(
                "<spoon.reflect.visitor.ImportScannerImpl: boolean isTypeInCollision(spoon.reflect.reference.CtReference,boolean)>/spoon.reflect.reference.CtTypeReference.getSimpleName/0"
            ),
            String(
                "<spoon.reflect.visitor.ImportScannerImpl: boolean isTypeInCollision(spoon.reflect.reference.CtReference,boolean)>/$stack10"
            ),
            String(
                "<spoon.reflect.visitor.ImportScannerImpl: boolean isTypeInCollision(spoon.reflect.reference.CtReference,boolean)>"
            ),
        ],
    )

    a1 = SymbolicConstant("a1", type=SYM)

    added_facts = {
        Fact(
            "OperatorAt",
            [a1, SymbolicConstant("a2", type=SYM)],
        ),
        Fact(
            "If_Var",
            [
                a1,
                String("_"),
                SymbolicConstant("a3", type=SYM),
            ],
        ),
        Fact(
            "If_Constant",
            [
                a1,
                String("_"),
                SymbolicConstant("a4", type=SYM),
            ],
        ),
        Fact(
            "JumpTarget",
            [SymbolicConstant("a5", type=SYM), a1],
        ),
        # Fact("Instruction_Next", [a1, SymbolicConstant("a6", type=SYM)]),
    }

    facts = facts.union(added_facts)
    # symbolise the sign of the facts
    facts = {SymbolicSign(f) for f in facts}

    start = time.time()
    constraints = symex(the_program, facts, {target_fact})

    print(f"Time taken: {time.time() - start}")

    # dump the constraints
    with open("tmp/Bears-56-constraints-5-sym_consts.txt", "w") as f:
        for k, v in constraints.items():
            f.write(f"{k} -> {v.to_z3().sexpr()}\n")


def test_GoldenGnu_jeveassets():
    the_program_path = "tmp/stratum_2.dl"

    the_program = parse(the_program_path)

    # TODO: load facts with decls
    the_fact_path = "tmp/GoldenGnu-jeveassets-f35ccd9_4a3c507"
    facts = load_facts(the_fact_path, the_program.declarations, the_program.inputs)

    target_fact = Fact(
        "MustNonNullAt",
        [
            Number(8),
            String(
                "<net.nikr.eve.jeveasset.io.esi.AbstractEsiGetter: void run()>/net.nikr.eve.jeveasset.data.api.accounts.EsiOwner.isInvalid/0"
            ),
            String(
                "<net.nikr.eve.jeveasset.io.esi.AbstractEsiGetter: void run()>/$stack8"
            ),
            String("<net.nikr.eve.jeveasset.io.esi.AbstractEsiGetter: void run()>"),
        ],
    )

    a1 = SymbolicConstant("a1", type=SYM)

    added_facts = {
        Fact(
            "OperatorAt",
            [a1, SymbolicConstant("a2", type=SYM)],
        ),
        Fact(
            "If_Var",
            [
                a1,
                String("_"),
                SymbolicConstant("a3", type=SYM),
            ],
        ),
        Fact(
            "If_Constant",
            [
                a1,
                String("_"),
                SymbolicConstant("a4", type=SYM),
            ],
        ),
        Fact(
            "JumpTarget",
            [SymbolicConstant("a5", type=SYM), a1],
        ),
        # Fact("Instruction_Next", [a1, SymbolicConstant("a6", type=SYM)]),
    }

    facts = facts.union(added_facts)
    # symbolise the sign of the facts
    facts = {SymbolicSign(f) for f in facts}

    start = time.time()
    constraints = symex(the_program, facts, {target_fact})

    print(f"Time taken: {time.time() - start}")

    # dump the constraints
    with open(
        "tmp/GoldenGnu-jeveassets-f35ccd9_4a3c507-constraints-5-sym_consts.txt", "w"
    ) as f:
        for k, v in constraints.items():
            f.write(f"{k} -> {v.to_z3().sexpr()}\n")


def test_HydAu_Camel():
    the_program_path = "tmp/stratum_2.dl"

    the_program = parse(the_program_path)

    # TODO: load facts with decls
    the_fact_path = "tmp/HydAu-Camel-597883f"
    facts = load_facts(the_fact_path, the_program.declarations, the_program.inputs)

    target_fact = Fact(
        "MustNonNullAt",
        [
            Number(47),
            String(
                "<org.apache.camel.component.bean.BeanInfo: org.apache.camel.component.bean.MethodInfo chooseBestPossibleMethodInfo(org.apache.camel.Exchange,java.util.Collection,java.lang.Object,java.util.List,java.util.List,java.util.List)>/java.lang.Class.isInstance/0"
            ),
            String(
                "<org.apache.camel.component.bean.BeanInfo: org.apache.camel.component.bean.MethodInfo chooseBestPossibleMethodInfo(org.apache.camel.Exchange,java.util.Collection,java.lang.Object,java.util.List,java.util.List,java.util.List)>/$stack33"
            ),
            String(
                "<org.apache.camel.component.bean.BeanInfo: org.apache.camel.component.bean.MethodInfo chooseBestPossibleMethodInfo(org.apache.camel.Exchange,java.util.Collection,java.lang.Object,java.util.List,java.util.List,java.util.List)>"
            ),
        ],
    )

    a1 = SymbolicConstant("a1", type=SYM)

    added_facts = {
        Fact(
            "OperatorAt",
            [a1, SymbolicConstant("a2", type=SYM)],
        ),
        Fact(
            "If_Var",
            [
                a1,
                String("_"),
                SymbolicConstant("a3", type=SYM),
            ],
        ),
        Fact(
            "If_Constant",
            [
                a1,
                String("_"),
                SymbolicConstant("a4", type=SYM),
            ],
        ),
        Fact(
            "JumpTarget",
            [SymbolicConstant("a5", type=SYM), a1],
        ),
        # Fact("Instruction_Next", [a1, SymbolicConstant("a6", type=SYM)]),
    }

    facts = facts.union(added_facts)
    # symbolise the sign of the facts
    facts = {SymbolicSign(f) for f in facts}

    start = time.time()
    constraints = symex(the_program, facts, {target_fact})

    print(f"Time taken: {time.time() - start}")

    # dump the constraints
    with open("tmp/HydAu-Camel-597883f-constraints-5-sym_consts.txt", "w") as f:
        for k, v in constraints.items():
            f.write(f"{k} -> {v.to_z3().sexpr()}\n")


def test_Adobe_Consulting_374231969():
    the_program_path = "tmp/stratum_2.dl"

    the_program = parse(the_program_path)

    # TODO: load facts with decls
    the_fact_path = "tmp/Adobe-Consulting-Services-acs-aem-commons-374231969"
    facts = load_facts(the_fact_path, the_program.declarations, the_program.inputs)

    target_fact = Fact(
        "MustNonNullAt",
        [
            Number(35),
            String(
                "<com.adobe.acs.commons.mcp.impl.processes.asset.UrlAssetImport: com.adobe.acs.commons.mcp.impl.processes.asset.FileOrRendition extractFile(java.util.Map)>/com.adobe.acs.commons.mcp.util.CompositeVariant.toString/1"
            ),
            String(
                "<com.adobe.acs.commons.mcp.impl.processes.asset.UrlAssetImport: com.adobe.acs.commons.mcp.impl.processes.asset.FileOrRendition extractFile(java.util.Map)>/$stack19"
            ),
            String(
                "<com.adobe.acs.commons.mcp.impl.processes.asset.UrlAssetImport: com.adobe.acs.commons.mcp.impl.processes.asset.FileOrRendition extractFile(java.util.Map)>"
            ),
        ],
    )

    a1 = SymbolicConstant("a1", type=SYM)

    added_facts = {
        Fact(
            "OperatorAt",
            [a1, SymbolicConstant("a2", type=SYM)],
        ),
        Fact(
            "If_Var",
            [
                a1,
                String("_"),
                SymbolicConstant("a3", type=SYM),
            ],
        ),
        Fact(
            "If_Constant",
            [
                a1,
                String("_"),
                SymbolicConstant("a4", type=SYM),
            ],
        ),
        Fact(
            "JumpTarget",
            [SymbolicConstant("a5", type=SYM), a1],
        ),
        # Fact("Instruction_Next", [a1, SymbolicConstant("a6", type=SYM)]),
    }

    facts = facts.union(added_facts)
    # symbolise the sign of the facts
    facts = {SymbolicSign(f) for f in facts}

    start = time.time()
    constraints = symex(the_program, facts, {target_fact})

    print(f"Time taken: {time.time() - start}")

    # dump the constraints
    with open(
        "tmp/Adobe-Consulting-Services-acs-aem-commons-374231969-constraints-5-sym_consts.txt",
        "w",
    ) as f:
        for k, v in constraints.items():
            f.write(f"{k} -> {v.to_z3().sexpr()}\n")


def test_Adobe_Consulting_374231969_2():
    the_program_path = "tmp/stratum_2.dl"

    the_program = parse(the_program_path)

    # TODO: load facts with decls
    the_fact_path = "tmp/Adobe-Consulting-Services-acs-aem-commons-374231969-2"
    facts = load_facts(the_fact_path, the_program.declarations, the_program.inputs)

    target_fact = Fact(
        "MustNonNullAt",
        [
            Number(5),
            String(
                "<com.adobe.acs.commons.mcp.impl.processes.asset.UrlAssetImport: java.lang.String getTargetFolder(java.util.Map)>/com.adobe.acs.commons.mcp.util.CompositeVariant.toString/0"
            ),
            String(
                "<com.adobe.acs.commons.mcp.impl.processes.asset.UrlAssetImport: java.lang.String getTargetFolder(java.util.Map)>/$stack5"
            ),
            String(
                "<com.adobe.acs.commons.mcp.impl.processes.asset.UrlAssetImport: java.lang.String getTargetFolder(java.util.Map)>"
            ),
        ],
    )

    a1 = SymbolicConstant("a1", type=SYM)

    added_facts = {
        Fact(
            "OperatorAt",
            [a1, SymbolicConstant("a2", type=SYM)],
        ),
        Fact(
            "If_Var",
            [
                a1,
                String("_"),
                SymbolicConstant("a3", type=SYM),
            ],
        ),
        Fact(
            "If_Constant",
            [
                a1,
                String("_"),
                SymbolicConstant("a4", type=SYM),
            ],
        ),
        Fact(
            "JumpTarget",
            [SymbolicConstant("a5", type=SYM), a1],
        ),
        # Fact("Instruction_Next", [a1, SymbolicConstant("a6", type=SYM)]),
    }

    facts = facts.union(added_facts)
    # symbolise the sign of the facts
    facts = {SymbolicSign(f) for f in facts}

    start = time.time()
    constraints = symex(the_program, facts, {target_fact})

    print(f"Time taken: {time.time() - start}")

    # dump the constraints
    with open(
        "tmp/Adobe-Consulting-Services-acs-aem-commons-374231969-2-constraints-5-sym_consts.txt",
        "w",
    ) as f:
        for k, v in constraints.items():
            f.write(f"{k} -> {v.to_z3().sexpr()}\n")


def test_fastjson_7c05c6f():
    the_program_path = "tmp/stratum_2.dl"

    the_program = parse(the_program_path)

    # TODO: load facts with decls
    the_fact_path = "tmp/fastjson_7c05c6f"
    facts = load_facts(the_fact_path, the_program.declarations, the_program.inputs)

    target_fact = Fact(
        "MustNonNullAt",
        [
            Number(27),
            String(
                "<com.alibaba.fastjson.parser.deserializer.ArrayListTypeFieldDeserializer: void parseField(com.alibaba.fastjson.parser.DefaultJSONParser,java.lang.Object,java.lang.reflect.Type,java.util.Map)>/java.util.Map.put/0"
            ),
            String(
                "<com.alibaba.fastjson.parser.deserializer.ArrayListTypeFieldDeserializer: void parseField(com.alibaba.fastjson.parser.DefaultJSONParser,java.lang.Object,java.lang.reflect.Type,java.util.Map)>/fieldValues#_0"
            ),
            String(
                "<com.alibaba.fastjson.parser.deserializer.ArrayListTypeFieldDeserializer: void parseField(com.alibaba.fastjson.parser.DefaultJSONParser,java.lang.Object,java.lang.reflect.Type,java.util.Map)>"
            ),
        ],
    )

    a1 = SymbolicConstant("a1", type=SYM)

    added_facts = {
        Fact(
            "OperatorAt",
            [a1, SymbolicConstant("a2", type=SYM)],
        ),
        Fact(
            "If_Var",
            [
                a1,
                String("_"),
                SymbolicConstant("a3", type=SYM),
            ],
        ),
        Fact(
            "If_Constant",
            [
                a1,
                String("_"),
                SymbolicConstant("a4", type=SYM),
            ],
        ),
        Fact(
            "JumpTarget",
            [SymbolicConstant("a5", type=SYM), a1],
        ),
        # Fact("Instruction_Next", [a1, SymbolicConstant("a6", type=SYM)]),
    }

    facts = facts.union(added_facts)
    # symbolise the sign of the facts
    facts = {SymbolicSign(f) for f in facts}

    start = time.time()
    constraints = symex(the_program, facts, {target_fact})

    print(f"Time taken: {time.time() - start}")

    # dump the constraints
    with open(
        "tmp/fastjson_7c05c6f-constraints-5-sym_consts.txt",
        "w",
    ) as f:
        for k, v in constraints.items():
            f.write(f"{k} -> {v.to_z3().sexpr()}\n")


def test_karaf_5965290():
    the_program_path = "tmp/stratum_2.dl"

    the_program = parse(the_program_path)

    # TODO: load facts with decls
    the_fact_path = "tmp/karaf_5965290"
    facts = load_facts(the_fact_path, the_program.declarations, the_program.inputs)

    target_fact = Fact(
        "MustNonNullAt",
        [
            Number(4),
            String(
                "<org.apache.karaf.shell.impl.console.commands.help.wikidoc.WikiParser: void parseHeading(org.apache.karaf.shell.impl.console.commands.help.wikidoc.WikiParser$Tokenizer)>/java.lang.String.matches/0"
            ),
            String(
                "(org.apache.karaf.shell.impl.console.commands.help.wikidoc.WikiParser$Tokenizer)>/level#_102"
            ),
            String(
                "<org.apache.karaf.shell.impl.console.commands.help.wikidoc.WikiParser: void parseHeading(org.apache.karaf.shell.impl.console.commands.help.wikidoc.WikiParser$Tokenizer)>"
            ),
        ],
    )

    a1 = SymbolicConstant("a1", type=SYM)

    added_facts = {
        Fact(
            "OperatorAt",
            [a1, SymbolicConstant("a2", type=SYM)],
        ),
        Fact(
            "If_Var",
            [
                a1,
                String("_"),
                SymbolicConstant("a3", type=SYM),
            ],
        ),
        Fact(
            "If_Constant",
            [
                a1,
                String("_"),
                SymbolicConstant("a4", type=SYM),
            ],
        ),
        Fact(
            "JumpTarget",
            [SymbolicConstant("a5", type=SYM), a1],
        ),
        # Fact("Instruction_Next", [a1, SymbolicConstant("a6", type=SYM)]),
    }

    facts = facts.union(added_facts)
    # symbolise the sign of the facts
    facts = {SymbolicSign(f) for f in facts}

    start = time.time()
    constraints = symex(the_program, facts, {target_fact})

    print(f"Time taken: {time.time() - start}")

    # dump the constraints
    with open(
        "tmp/karaf_5965290-constraints-5-sym_consts.txt",
        "w",
    ) as f:
        for k, v in constraints.items():
            f.write(f"{k} -> {v.to_z3().sexpr()}\n")


def test_api_analyser():
    api_analyser_path = "/root/Symlog-Dev/tmp/api_analyser.dl"
    api_analyser_program = parse(api_analyser_path)
    fact_path = "/root/Symlog-Dev/tmp/ushahidi"
    facts = load_facts(
        fact_path, api_analyser_program.declarations, api_analyser_program.inputs
    )

    a1 = SymbolicConstant("a1", type=SYM)
    a2 = SymbolicConstant("a2", type=NUM)
    a3 = SymbolicConstant("a3", type=SYM)
    a4 = SymbolicConstant("a4", type=SYM)
    a5 = SymbolicConstant("a5", type=NUM)
    added_facts = {
        Fact(
            "call_name",
            [
                a1,
                a2,
                a3,
                a4,
            ],
        ),
        # Fact(
        #     "call_name",
        #     [
        #         a1,
        #         a5,
        #         a3,
        #         a4,
        #     ],
        # ),
    }
    facts = facts.union(added_facts)
    # symbolise the sign of the facts
    facts = {SymbolicSign(f) for f in facts}

    # target_fact = Fact(
    #     "correct_usage",
    #     [
    #         String("android.database.sqlite.SQLiteDatabase.query"),
    #         Number(3),
    #         String(
    #             "com.ushahidi.android.app.database.OpenGeoSmsDao.getReportState(long)"
    #         ),
    #         Number(1),
    #     ],
    # )

    target_fact = Fact(
        "must_call_name_followed_before_exit",
        [
            String("android.database.sqlite.SQLiteDatabase.query"),
            Number(3),
            String(
                "com.ushahidi.android.app.database.OpenGeoSmsDao.getReportState(long)"
            ),
            String("android.database.Cursor.close"),
            Number(1),
        ],
    )

    start = time.time()
    constraints = symex(api_analyser_program, facts, {target_fact})
    print(f"Time taken: {time.time() - start}")
    # dump the constraints
    with open("tmp/ushahidi-constraints-sym_consts.txt", "w") as f:
        for k, v in constraints.items():
            f.write(f"{k} -> {v.to_z3().sexpr()}\n")


def tmp_test():
    api_analyser_path = "/root/Symlog-Dev/tmp/api_analyser.dl"
    api_analyser_program = parse(api_analyser_path)
    fact_path = "/root/Symlog-Dev/tmp/raw_1"
    facts = load_facts(
        fact_path, api_analyser_program.declarations, api_analyser_program.inputs
    )

    target_fact = Fact(
        "must_call_name_followed_before_exit",
        [
            String("android.database.sqlite.SQLiteDatabase.query"),
            Number(3),
            String(
                "com.ushahidi.android.app.database.OpenGeoSmsDao.getReportState(long)"
            ),
            String("android.database.Cursor.close"),
            Number(1),
        ],
    )

    start = time.time()
    constraints = symex(api_analyser_program, facts, {target_fact})
    print(f"Time taken: {time.time() - start}")


if __name__ == "__main__":
    test_api_analyser()
