import pytest
from printer import *


def test_number():
    assert (Number(1).accept(PrettyPrinter())) == '1;'


def test_conditional_true():
    """
    if (42) {
        12;
        27;
    }
    """
    assert Conditional(
        Number(42),
        [
            Number(12), Number(27)
        ],
        []).accept(PrettyPrinter()) == 'if (42) {\n    12;\n    27;\n}'


def test_conditional_true_and_false():
    """
    if (42) {
        12;
        27;
    } else {
        if (42) {
            12;
            27;
        }
    }
    """
    assert Conditional(
        Number(42),
        [
            Number(12), Number(27)
        ],
        [
            Conditional(Number(42), [Number(12), Number(27)], [])
        ]).accept(PrettyPrinter()) == 'if (42) {\n    12;\n    27;\n} ' \
                                      'else {\n    if (42) {\n        12;\n' \
                                      '        27;\n    }\n}'


def test_fuction_defenition():
    """
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
    }
    """
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
    ).accept(PrettyPrinter()) == 'def foo(arg1, arg2) {\n    ' \
                                 'if (42) {\n        12;\n        27;\n    } '\
                                 'else {\n        ' \
                                 'if (42) {\n            12;\n' \
                                 '            27;\n        }\n    }\n}'


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


def test_binary_opeartion():
    assert BinaryOperation(
        Number(1),
        '*',
        BinaryOperation(
            Number(2),
            '+',
            Number(3),
        )
    ).accept(PrettyPrinter()) == '(1 * (2 + 3));'


def test_unary_operation():
    assert UnaryOperation('-',
                          UnaryOperation('-',
                                         UnaryOperation('!',
                                                        Number(42)
                                                        )
                                         )
                          ).accept(PrettyPrinter()) == '(-(-(!42)));'


def test_end_to_end(capsys):
    """
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
    assert out == 'def main(arg1) {\n    read x;\n    print x;\n  ' \
                  '  if ((2 == 3)) {\n        if (1) {\n        }\n    }' \
                  ' else {\n        exit((-arg1));\n    }\n}'


if __name__ == "__main__":
    pytest.main()
