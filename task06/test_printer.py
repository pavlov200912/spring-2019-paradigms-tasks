import pytest
from printer import *
import textwrap


def test_number():
    assert (Number(1).accept(PrettyPrinter())) == '1;'


def test_conditional_true():
    test_command = """\
    if (42) {
        12;
        27;
    }"""
    assert Conditional(
        Number(42),
        [
            Number(12), Number(27)
        ],
        []).accept(PrettyPrinter()) == (textwrap.dedent(test_command))


def test_conditional_true_and_false():
    test_command = """\
    if (42) {
        12;
        27;
    } else {
        if (42) {
            12;
            27;
        }
    }"""
    assert Conditional(
        Number(42),
        [
            Number(12), Number(27)
        ],
        [
            Conditional(Number(42), [Number(12), Number(27)], [])
        ]).accept(PrettyPrinter()) == textwrap.dedent(test_command)


def test_function_definition():
    test_command = """\
    def foo(arg1, arg2) {
        if (42) {
            12;
            27;
        } else {
            if (42) {
                12;
                27;
            }
        }
    }"""
    assert FunctionDefinition(
        "foo",
        Function(['arg1', 'arg2'],
                 [
                     Conditional(
                         Number(42),
                         [
                             Number(12), Number(27)
                         ],
                         [
                             Conditional(
                                 Number(42),
                                 [Number(12), Number(27)],
                                 [])
                         ])
                 ]
                 )
    ).accept(PrettyPrinter()) == textwrap.dedent(test_command)


def test_print():
    assert Print(Number(42)).accept(PrettyPrinter()) == 'print 42;'


def test_read():
    assert Read('var').accept(PrettyPrinter()) == 'read var;'


def test_reference():
    assert Reference('var').accept(PrettyPrinter()) == 'var;'


def test_function_call():
    assert FunctionCall(
        Reference('foo'),
        [Number(1), Number(2), Number(3)]
    ).accept(PrettyPrinter()) == 'foo(1, 2, 3);'


def test_function_call_hard():
    assert FunctionCall(
        Reference('foo'),
        [
            FunctionCall(
                Reference('foo'),
                [Number(1), Number(2), Number(3)]
            ),
            Number(2), Number(3)
        ]).accept(PrettyPrinter()) == 'foo(foo(1, 2, 3), 2, 3);'


def test_binary_operation():
    assert BinaryOperation(
        Number(1),
        '*',
        BinaryOperation(
            Number(2),
            '+',
            Number(3),
        )
    ).accept(PrettyPrinter()) == '(1 * (2 + 3));'


def test_binary_expression_as_command():
    test_command = """\
    if (42) {
        ((2 + 2) == 2);
    }"""
    assert Conditional(
        Number(42),
        [
            BinaryOperation(
                BinaryOperation(Number(2), '+', Number(2)),
                '==',
                Number(2)
            )
        ],
        []
    ).accept(PrettyPrinter()) == textwrap.dedent(test_command)


def test_unary_operation():
    assert UnaryOperation('-',
                          UnaryOperation('-',
                                         UnaryOperation('!',
                                                        Number(42)
                                                        )
                                         )
                          ).accept(PrettyPrinter()) == '(-(-(!42)));'


def test_unary_expression_as_command():
    test_command = """\
    if (42) {
        (-2);
    }"""
    assert Conditional(
        Number(42),
        [UnaryOperation('-', Number(2))],
        []
    ).accept(PrettyPrinter()) == textwrap.dedent(test_command)


def test_end_to_end(capsys):
    test_command = """\
    def main(arg1) {
        read x;
        print x;
        if ((2 == 3)) {
            if (1) {
            }
        } else {
            exit((-arg1));
        }
    }
    """
    pretty_print(FunctionDefinition('main', Function(['arg1'], [
        Read('x'),
        Print(Reference('x')),
        Conditional(
            BinaryOperation(Number(2), '==', Number(3)),
            [
                Conditional(Number(1), [], [])
            ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('arg1'))
                ])
            ],
        ),
    ])))
    out, err = capsys.readouterr()
    assert not err
    assert out == textwrap.dedent(test_command)


if __name__ == "__main__":
    pytest.main()
