from symlog.transformer import (
    transform_negation,
    transform_program,
    Program,
    Literal,
    Rule,
    Variable,
    Number,
    Unification,
)

from symlog.shortcuts import load_facts, parse, SymbolicConstant, SymbolicSign, Fact
from symlog.souffle import NUM, SYM
from symlog.program_builder import ProgramBuilder


def test_transform_negation():
    api_analyser_path = "/root/Symlog-Dev/tmp/api_analyser.dl"
    api_analyser_program = parse(api_analyser_path)
    fact_path = "/root/Symlog-Dev/tmp/ushahidi"
    facts = load_facts(
        fact_path, api_analyser_program.declarations, api_analyser_program.inputs
    )

    program = ProgramBuilder.preprocess_parsed_program(
        api_analyser_program, facts, outputs=api_analyser_program.outputs
    )

    # Call the transform_negation function
    transformed_program = transform_negation(program)

    # Assert that the transformed_program is a Program instance
    assert isinstance(transformed_program, Program)


def test_transform_program():
    api_analyser_path = "/root/Symlog-Dev/tmp/api_analyser.dl"
    api_analyser_program = parse(api_analyser_path)
    fact_path = "/root/Symlog-Dev/tmp/ushahidi"
    facts = load_facts(
        fact_path, api_analyser_program.declarations, api_analyser_program.inputs
    )

    added_facts = {
        Fact(
            "call",
            [
                SymbolicConstant("a1", type=SYM),
                SymbolicConstant("a2", type=NUM),
                SymbolicConstant("a3", type=SYM),
                SymbolicConstant("a4", type=SYM),
            ],
        )
    }
    facts = facts.union(added_facts)
    # symbolise the sign of the facts
    facts = {SymbolicSign(f) for f in facts}

    program = ProgramBuilder.preprocess_parsed_program(
        api_analyser_program, facts, outputs=api_analyser_program.outputs
    )

    transformed_program = transform_program(
        program,
    )
    # Assert that the transformed_program is a Program instance
    assert isinstance(transformed_program, Program)


if __name__ == "__main__":
    test_transform_program()
