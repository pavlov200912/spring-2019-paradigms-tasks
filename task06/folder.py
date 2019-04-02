from model import *


class ConstantFolder(ASTNodeVisitor):
    def visit_conditional(self, conditional):
        conditional.condition = conditional.condition.accept(self)
        conditional.if_true = \
            [expr.accept(self) for expr in conditional.if_true]
        conditional.if_false = \
            [expr.accept(self) for expr in conditional.if_false]
        return conditional

    def visit_function_definition(self, function_definition):
        function_definition.function = \
            function_definition.function.accept(self)
        return function_definition

    def visit_print(self, print_object):
        print_object.expr = print_object.expr.accept(self)
        return print_object

    def visit_read(self, read):
        read.expr = read.expr.accept(self)
        return read

    def visit_number(self, number):
        return number

    def visit_reference(self, reference):
        return reference

    def visit_binary_operation(self, binary_operation):
        binary_operation.lhs = binary_operation.lhs.accept(self)
        binary_operation.rhs = binary_operation.rhs.accept(self)
        if isinstance(binary_operation.lhs, Number) and \
                isinstance(binary_operation.rhs, Number):
            return binary_operation.evaluate(Scope())
        if isinstance(binary_operation.lhs, Reference) and \
                isinstance(binary_operation.rhs, Number) and \
                binary_operation.rhs == Number(0):
            return Number(0)
        if isinstance(binary_operation.rhs, Reference) and \
                isinstance(binary_operation.lhs, Number) and \
                binary_operation.lhs == Number(0):
            return Number(0)
        if isinstance(binary_operation.lhs, Reference) and \
                isinstance(binary_operation.rhs, Reference) and \
                binary_operation.rhs.name == binary_operation.lhs.name and \
                binary_operation.op == '-':
            return Number(0)
        return binary_operation

    def visit_unary_operation(self, unary_operation):
        unary_operation.expr = unary_operation.expr.accept(self)
        if isinstance(unary_operation.expr, Number):
            return unary_operation.evaluate(Scope())
        return unary_operation

    def visit_function_call(self, function_call):
        function_call.fun_expr = function_call.fun_expr.accept(self)
        function_call.args = [expr.accept(self) for expr in function_call.args]
        return function_call

    def visit_function(self, function):
        function.args = [expr.accept(self) for expr in function.args]
        function.body = [expr.accept(self) for expr in function.body]
        return function


def fold_constants(program):
    return program.accept(ConstantFolder())
