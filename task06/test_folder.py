import pytest
from folder import *


def test_number_and_number():
    assert fold_constants(BinaryOperation(
        Number(21), '+', Number(10))) == Number(31)
    assert fold_constants(BinaryOperation(
        Number(7), '*', Number(8))) == Number(56)
    assert fold_constants(BinaryOperation(
        Number(22), '==', Number(3))) == Number(0)
    assert fold_constants(BinaryOperation(
        Number(222), '/', Number(2))) == Number(111)


def test_zero_multiply_reference():
    assert fold_constants(BinaryOperation(
        Number(0), '*', Reference('x'))) == Number(0)


def test_reference_multiply_zero():
    assert fold_constants(BinaryOperation(
        Reference('x'), '*', Number(0))) == Number(0)


def test_reference_substract_reference():
    assert fold_constants(BinaryOperation(
        Reference('x'), '-', Reference('x'))) == Number(0)


def test_reference_plus_reference():
    result = fold_constants(BinaryOperation(
        Reference('x'), '+', Reference('x')))
    assert isinstance(result, BinaryOperation)
    assert isinstance(result.lhs, Reference)
    assert isinstance(result.rhs, Reference)
    assert result.lhs.name == result.rhs.name == 'x'
    assert result.op == '+'


def test_unary_operation_and_number():
    assert fold_constants(UnaryOperation('!', Number(0))) == Number(1)
    assert fold_constants(UnaryOperation('!', Number(1))) == Number(0)
    assert fold_constants(UnaryOperation('-', Number(7))) == Number(-7)


def test_end_to_end():
    assert fold_constants(
        BinaryOperation(
            Number(10),
            '-',
            UnaryOperation(
                '-',
                BinaryOperation(
                    Number(3),
                    '+',
                    BinaryOperation(
                        Reference('x'),
                        '-',
                        Reference('x')
                    )
                )
            )
        )
    ) == Number(13)
