from symlog.souffle import (
    Program,
    Fact,
    Literal,
    String,
    Number,
    compile_and_run,
    pprint,
)
from symlog.utils import is_sublist
from symlog.program_builder import ProgramBuilder
from symlog.common import NEG_PREFIX

from subprocess import Popen, PIPE
from tempfile import NamedTemporaryFile
import json
import re
from typing import List, Set, FrozenSet
from itertools import combinations
from more_itertools import unique_everseen
from collections import namedtuple, defaultdict


ProgramTargetKey = namedtuple("ProgramTargetKey", ["program", "target"])

Axiom = namedtuple("Axiom", ["axiom"])
AxiomSet = namedtuple("AxiomSet", ["axioms", "positive"])

AxiomFactSet = namedtuple("AxiomFactSet", ["facts", "positive"])


def filter_out_excluded_items(source_list, exclusion_list):
    # convert exclusion_list to a set for faster look-up
    exclusion_set = set(exclusion_list)

    # remove elements from source_list that are in exclusion_set
    return [item for item in source_list if item not in exclusion_set]


def combinations_and_complements(lst: list) -> list[tuple[tuple, tuple]]:
    all_combs_and_complements = []

    def get_complement(lst, comb):
        temp = lst - frozenset(comb)
        return tuple(temp)

    for comb in combinations(lst, len(lst) - 1):
        complement = get_complement(lst, comb)
        all_combs_and_complements.append((comb, complement))

    # remove duplicate combinations and complements
    all_combs_and_complements = list(unique_everseen(all_combs_and_complements))

    return all_combs_and_complements


class Provenancer:
    def __init__(self):
        self._provenance_cache_bv = defaultdict(list)
        self._provenance_cache = defaultdict(list)

    def _extract_and_map_axioms_to_facts(self, node, program):
        """Recursively extract axioms from a proof node."""
        if "axiom" in node:
            return [self._axiom_or_premises_to_fact(node["axiom"], program)]
        elif "children" in node:
            assert "premises" in node, "Bug?"
            if node["premises"].startswith(NEG_PREFIX):
                return [self._axiom_or_premises_to_fact(node["premises"], program)]
            else:
                return [
                    axiom
                    for child in node["children"]
                    for axiom in self._extract_and_map_axioms_to_facts(child, program)
                ]
        return []

    def _extract_tuple_info(self, tuple_str):
        # Regular expression to match the tuple name and its arguments
        pattern = r"(\w+)\((.*)\)"
        match = re.match(pattern, tuple_str)

        if not match:
            return None

        tuple_name = match.group(1)
        arguments_str = match.group(2)

        # Function to correctly split the arguments considering nested structures
        def split_args(args_str):
            args = []
            nested_level = 0
            current_arg = ""

            for char in args_str:
                if char == "(":
                    nested_level += 1
                    current_arg += char
                elif char == ")":
                    nested_level -= 1
                    current_arg += char
                elif char == "," and nested_level == 0:
                    args.append(current_arg.strip())
                    current_arg = ""
                else:
                    current_arg += char

            # Add the last argument
            if current_arg:
                args.append(current_arg.strip())
            # For each arg, remove the double quotes if present
            args = [arg.strip('"') for arg in args]
            return args

        arguments = split_args(arguments_str)

        return tuple_name, arguments

    def _axiom_or_premises_to_fact(self, node_content, program):
        """Convert axiom or premises to fact."""
        relation_name, arguments = self._extract_tuple_info(node_content)

        # wrap the arguments with corresponding types
        decl_types = program.declarations[relation_name]
        wrapped_args = [
            String(arg) if decl_type == "symbol" else Number(int(arg))
            for arg, decl_type in zip(arguments, decl_types)
        ]

        # TODO: use better logic to handle the negative facts
        # ignore the symbolic sign of IDB facts at this moment
        if relation_name.startswith(NEG_PREFIX):
            return Fact(Literal(relation_name, wrapped_args, positive=True), [], False)
        else:
            # for EBD facts, find the matched fact in the program
            for fact in program.facts:
                if fact.head.name == relation_name and fact.head.args == wrapped_args:
                    return fact
            assert False, "Bug?"

    def _provenance(
        self, bare_program: Program, target_fact: Fact, input_facts: List[Fact]
    ) -> FrozenSet[Fact]:
        """Returns the dependent facts of the given fact in the given bare program and input facts."""

        # quick check if the target fact is derivable
        derived_facts = compile_and_run(bare_program, facts=input_facts)

        if target_fact not in derived_facts:
            return []

        program = ProgramBuilder.update_program(bare_program, facts=input_facts)

        dep_facts = self._base_provenance(program, target_fact)

        return dep_facts

    def _escape_invalid_json_chars(self, json_str):
        return json_str.replace("\\;", "\\\\;")

    def _base_provenance(self, program: Program, target_fact: Fact) -> FrozenSet[Fact]:
        """Returns the provenance of the given fact in the given program."""

        try:
            with NamedTemporaryFile() as datalog_script:
                datalog_script.write(pprint(program).encode())
                datalog_script.flush()

                cmd = [
                    "souffle",
                    "-t",
                    "explain",
                    datalog_script.name,
                ]

                try:
                    interactive_process = Popen(
                        cmd,
                        stdin=PIPE,
                        stdout=PIPE,
                        stderr=PIPE,
                    )

                    commands = [
                        f"setdepth {1e9}",  # set a large number
                        "format json",
                        "explain "
                        + "".join(
                            pprint(target_fact).rsplit(".", 1)
                        ).strip(),  # remove the '.' for identifying a fact and \n.
                    ]
                    output, errors = interactive_process.communicate(
                        "\n".join(commands).encode()
                    )

                    if interactive_process.returncode != 0:
                        raise RuntimeError(
                            f"'souffle' command failed with error: {errors.decode()}"
                        )
                except Exception as e:
                    print(f"Error executing external command: {e}")
                    raise

                try:
                    output = self._escape_invalid_json_chars(output.decode())
                    data = json.loads(output)
                except json.JSONDecodeError:
                    print("Error parsing JSON output from Souffle")
                    return None

                assert "proof" in data, "Bug in Souffle?"

                # debug: dump the json output to a file
                with open("output.json", "w") as f:
                    json.dump(data, f, indent=4)

                if "Tuple not found" in str(
                    data["proof"]
                ) or "Relation not found" in str(data["proof"]):
                    return None

                facts = self._extract_and_map_axioms_to_facts(data["proof"], program)

        except IOError as e:
            print(f"File operation error: {e}")
            raise

        return frozenset(facts)

    def _previous_computed_result(
        self, program_target_key: ProgramTargetKey, input_data: Set[Fact]
    ):
        for prev_result in unique_everseen(
            self._provenance_cache[program_target_key], key=tuple
        ):
            if is_sublist(prev_result, input_data):
                return prev_result
        return None

    def monotonic_all(
        self, bare_program: Program, target_output: Fact, input_facts: Set[Fact]
    ) -> list:
        """Final all minimized input facts that can produce the target output"""

        program_target_key = ProgramTargetKey(bare_program, target_output)

        def dfs(current_input_facts):
            results = []

            # try to reuse the minimized input computed previously
            minimized_input = self._previous_computed_result(
                program_target_key, current_input_facts
            )
            # if not, compute the minimal input
            if not minimized_input:
                minimized_input = self._provenance(
                    bare_program, target_output, current_input_facts
                )

            if not minimized_input:
                return results

            results.append(minimized_input)

            if not minimized_input in self._provenance_cache[program_target_key]:
                self._provenance_cache[program_target_key].append(minimized_input)

            # debug: save minimized input to a file
            with open("minimized_input.json", "w") as f:
                json.dump(
                    {
                        f"{str(target_output)}": [
                            pprint(fact) for fact in minimized_input
                        ]
                    },
                    f,
                    indent=4,
                )

            # get combinations of the minimized_input
            i = 0
            for comb, complement in combinations_and_complements(minimized_input):
                i += 1
                comb = list(comb)  # convert tuple to list
                # skip the complement which starts with 'symlog_neg_'
                if complement[0].head.name.startswith(NEG_PREFIX):
                    continue
                # new_input = comb + filter_out_excluded_items(
                #     current_input_facts, complement
                # )
                new_input = filter_out_excluded_items(current_input_facts, complement)

                # print(
                #     f"Combination {i}, old input length:{len(current_input_facts)}, new input length: {len(new_input)}"
                # )

                # print("set new input length: ", len(set(new_input)))

                results.extend(dfs(new_input))

            return results

        # execute dfs and retrieve results
        all_results = dfs(input_facts)

        # deduplicate results
        unique_results = list(unique_everseen(all_results, key=tuple))

        # debug: save all results to a file
        with open("all_results.json", "w") as f:
            json.dump(
                {
                    f"{str(target_output)}": [
                        [pprint(fact) for fact in result] for result in unique_results
                    ]
                },
                f,
                indent=4,
            )
        return unique_results
