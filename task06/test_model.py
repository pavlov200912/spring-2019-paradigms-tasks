#!/usr/bin/env python3
import pytest
import sys
from io import StringIO
from model import Scope
from model import Reference
from model import Function
from model import FunctionCall
from model import FunctionDefinition
from model import BinaryOperation
from model import Conditional
from model import Print
from model import Number
from model import Read
from model import UnaryOperation


def test_scope_local():
    var1 = object()
    scope = Scope()
    scope['var1'] = var1
    assert scope['var1'] == var1


def test_scope_parent():
    var1 = object()
    var2 = object()
    parent = Scope()
    parent['var1'] = var1
    scope = Scope(parent)
    scope['var2'] = var2
    assert scope['var1'] == var1
    assert scope['var2'] == var2
    scope['var1'] = var2
    assert scope['var1'] == var2


def test_scope_grandparent():
    var1 = object()
    var2 = object()
    var3 = object()
    grand_parent = Scope()
    grand_parent['var2'] = var3
    grand_parent['var3'] = var3
    parent = Scope(grand_parent)
    parent['var1'] = var1
    scope = Scope(parent)
    scope['var2'] = var2
    assert scope['var1'] == var1
    assert scope['var2'] == var2
    assert scope['var3'] == var3
    scope['var1'] = var2
    assert scope['var1'] == var2


def test_scope_exception():
    var1 = object()
    scope = Scope()
    scope['var1'] = var1
    with pytest.raises(KeyError):
        scope['var']


def test_end_to_end(capsys):

    yat_fac = FunctionDefinition('fac', Function(['n'], [
        Conditional(
            BinaryOperation(Reference('n'), '==', Number(0)),
            [Number(1)],
            [
                BinaryOperation(
                    Reference('n'),
                    '*',
                    FunctionCall(Reference('fac'), [
                        BinaryOperation(
                            Reference('n'),
                            '-',
                            Number(1)
                        )
                    ])
                )
            ]
        )
    ]))
    s = Scope()
    yat_fac.evaluate(s)
    a = Print(FunctionCall(Reference('fac'), [Number(5)])).evaluate(s)
    out, err = capsys.readouterr()
    assert a == Number(120)
    assert out == '120\n'
    assert err == ''


def test_function_definition():
    scope = Scope()
    func = Function(
        ['a', 'b'],
        [BinaryOperation(Reference('a'), '+', Reference('b'))]
    )
    func_def = FunctionDefinition('sum', func)
    assert func_def.evaluate(scope) == func
    assert scope['sum'] == func


def test_conditional():
    true_conditional = Conditional(
        Number(1),
        [Number(1)],
        [Print(Number(2))]
    ).evaluate(Scope())
    false_conditional = Conditional(
        Number(0),
        [Print(Number(1))],
        [Number(3), Number(2)]
    ).evaluate(Scope())
    assert true_conditional.evaluate(Scope()) == Number(1)
    assert false_conditional.evaluate(Scope()) == Number(2)


def test_print(capsys):
    value = Print(Number(17)).evaluate(Scope())
    out, err = capsys.readouterr()
    assert value == Number(17)
    assert out == '17\n'
    assert err == ''


def test_read(monkeypatch):
    monkeypatch.setattr(sys, 'stdin', StringIO('12'))
    scope = Scope()
    value = Read('x').evaluate(scope)
    assert value == Number(12)
    assert scope['x'] == Number(12)


def test_function_call():
    scope = Scope()
    function = Function(
        ['a'],
        [Reference('a')]
    )
    scope['function'] = function
    function_call = FunctionCall(
        Reference('function'),
        [Number(8)]
    )
    assert function_call.evaluate(scope) == Number(8)


def test_reference():
    scope = Scope()
    scope['var1'] = Number(1)
    scope['var2'] = Number(0)
    assert Reference('var1').evaluate(scope) == Number(1)
    assert Reference('var2').evaluate(scope) == Number(0)


def test_binary_operation():
    assert BinaryOperation(
        Number(5),
        '+',
        Number(6)
    ).evaluate(Scope) == Number(11)
    assert BinaryOperation(
        Number(5),
        '-',
        Number(6)
    ).evaluate(Scope) == Number(-1)
    assert BinaryOperation(
        Number(5),
        '*',
        Number(6)
    ).evaluate(Scope) == Number(30)
    assert BinaryOperation(
        Number(11),
        '/',
        Number(6)
    ).evaluate(Scope) == Number(1)
    assert BinaryOperation(
        Number(17),
        '%',
        Number(6)
    ).evaluate(Scope) == Number(5)
    assert BinaryOperation(
        Number(5),
        '==',
        Number(6)
    ).evaluate(Scope) == Number(0)
    assert BinaryOperation(
        Number(5),
        '!=',
        Number(6)
    ).evaluate(Scope) == Number(1)
    assert BinaryOperation(
        Number(5),
        '<',
        Number(6)
    ).evaluate(Scope) == Number(1)
    assert BinaryOperation(
        Number(5),
        '>',
        Number(6)
    ).evaluate(Scope) == Number(0)
    assert BinaryOperation(
        Number(5),
        '<=',
        Number(5)
    ).evaluate(Scope) == Number(1)
    assert BinaryOperation(
        Number(5),
        '>=',
        Number(6)
    ).evaluate(Scope) == Number(0)
    assert BinaryOperation(
        Number(5),
        '&&',
        Number(0)
    ).evaluate(Scope) == Number(0)
    assert BinaryOperation(
        Number(5),
        '&&',
        Number(2)
    ).evaluate(Scope) == Number(1)
    assert BinaryOperation(
        Number(1),
        '&&',
        Number(2)
    ).evaluate(Scope) == Number(1)
    assert BinaryOperation(
        Number(5),
        '||',
        Number(0)
    ).evaluate(Scope) == Number(1)
    assert BinaryOperation(
        Number(0),
        '||',
        Number(0)
    ).evaluate(Scope) == Number(0)
    assert BinaryOperation(
        Number(0),
        '||',
        Number(1)
    ).evaluate(Scope) == Number(1)


def test_unary_operation():
    assert UnaryOperation(
        '-',
        Number(8)
    ).evaluate(Scope()) == Number(-8)
    assert UnaryOperation(
        '-',
        Number(-7)
    ).evaluate(Scope()) == Number(7)
    assert UnaryOperation(
        '!',
        Number(18)
    ).evaluate(Scope()) == Number(0)
    assert UnaryOperation(
        '!',
        Number(0)
    ).evaluate(Scope()) == Number(1)


if __name__ == "__main__":
    pytest.main()
