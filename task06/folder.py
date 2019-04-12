from model import *


class ConstantFolder(ASTNodeVisitor):
    def visit_conditional(self, conditional):
        new_condition = conditional.condition.accept(self)
        new_if_true = [expr.accept(self)
                       for expr in conditional.if_true or []]
        new_if_false = [expr.accept(self)
                        for expr in conditional.if_false or []]
        return Conditional(new_condition, new_if_true, new_if_false)

    def visit_function_definition(self, function_definition):
        return FunctionDefinition(
            function_definition.name,
            function_definition.function.accept(self)
        )

    def visit_print(self, print_object):
        return Print(print_object.expr.accept(self))

    def visit_read(self, read):
        return Read(read.name)

    def visit_number(self, number):
        return Number(number.value)

    def visit_reference(self, reference):
        return Reference(reference.name)

    def visit_binary_operation(self, binary_operation):
        new_lhs = binary_operation.lhs.accept(self)
        new_rhs = binary_operation.rhs.accept(self)
        if isinstance(new_lhs, Number) and isinstance(new_rhs, Number):
            return BinaryOperation(new_lhs, binary_operation.op,
                                   new_rhs).evaluate(Scope())
        if (isinstance(new_lhs, Number) and new_lhs == Number(0) and
                isinstance(new_rhs, Reference) and binary_operation.op == '*'):
            return Number(0)
        if (isinstance(new_rhs, Number) and new_rhs == Number(0) and
                isinstance(new_lhs, Reference) and binary_operation.op == '*'):
            return Number(0)
        if (isinstance(new_lhs, Reference) and isinstance(new_rhs, Reference)
                and new_lhs.name == new_rhs.name
                and binary_operation.op == '-'):
            return Number(0)
        return BinaryOperation(new_lhs, binary_operation.op, new_rhs)

    def visit_unary_operation(self, unary_operation):
        new_expr = unary_operation.expr.accept(self)
        if isinstance(new_expr, Number):
            return UnaryOperation(
                unary_operation.op, new_expr
            ).evaluate(Scope())
        return UnaryOperation(unary_operation.op, new_expr)

    def visit_function_call(self, function_call):
        new_fun_expr = function_call.fun_expr.accept(self)
        new_args = [expr.accept(self) for expr in function_call.args]
        return FunctionCall(new_fun_expr, new_args)

    def visit_function(self, function):
        new_args = function.args.copy()
        new_body = [expr.accept(self) for expr in function.body]
        return Function(new_args, new_body)


def fold_constants(program):
    return program.accept(ConstantFolder())
