make it

./amla run ./test/fail/syntax_error/expected_expression.amla
./amla run ./test/fail/syntax_error/expected_token.amla
./amla run ./test/fail/syntax_error/expected_top_level_statement.amla
./amla run ./test/fail/syntax_error/expected_type.amla
./amla run ./test/fail/syntax_error/numeric_literal_exceeds_u64_max.amla
./amla run ./test/fail/syntax_error/unrecognized_character.amla
./amla run ./test/fail/syntax_error/unterminated_character_literal.amla
./amla run ./test/fail/syntax_error/unterminated_string_literal.amla

./amla run ./test/fail/type_error/incorrect_argument_count1.amla
./amla run ./test/fail/type_error/incorrect_argument_count2.amla
./amla run ./test/fail/type_error/invalid_assignment.amla
./amla run ./test/fail/type_error/not_a_function.amla
./amla run ./test/fail/type_error/print_wrong_number_of_format_arguments.amla
./amla run ./test/fail/type_error/type_missmatch.amla
./amla run ./test/fail/type_error/undeclared_identifier.amla

./amla run ./test/pass/print.amla

