from model import *
import textwrap


class ExpressionPrinter(ASTNodeVisitor):
    def visit_number(self, number):
        return str(number.value)

    def visit_function(self, function):
        pass

    def visit_function_definition(self, function_definition):
        pass

    def visit_conditional(self, conditional):
        pass

    def visit_print(self, print_object):
        pass

    def visit_read(self, read):
        pass

    def visit_function_call(self, function_call):
        result = function_call.fun_expr.accept(self) + '('
        result += ', '.join(x.accept(self) for x in function_call.args) + ')'
        return result

    def visit_reference(self, reference):
        return reference.name

    def visit_binary_operation(self, binary_operation):
        return ('(' + binary_operation.lhs.accept(self) + ' ' +
                binary_operation.op + ' '
                + binary_operation.rhs.accept(self) + ')')

    def visit_unary_operation(self, unary_operation):
        return ('(' + unary_operation.op +
                unary_operation.expr.accept(self) + ')')


class PrettyPrinter(ASTNodeVisitor):
    INDENT = '    '

    def __init__(self, expression_printer):
        self.expression_printer = expression_printer

    def visit_expression(self, node):
        return node.accept(self.expression_printer)

    def visit_block(self, block):
        result = ''
        for expr in block or []:
            result += expr.accept(self) + '\n'
        return textwrap.indent(result, self.INDENT)

    def visit_number(self, number):
        return self.visit_expression(number) + ';'

    def visit_function(self, function):
        pass

    def visit_function_definition(self, function_definition):
        result = 'def ' + function_definition.name + '('
        result += ', '.join(function_definition.function.args)
        result += ') {\n'
        result += self.visit_block(function_definition.function.body)
        result += '}'
        return result

    def visit_conditional(self, conditional):
        result = 'if ('
        result += self.visit_expression(conditional.condition)
        result += ') {\n'
        result += self.visit_block(conditional.if_true)
        result += '}'
        if conditional.if_false:
            result += ' else {\n'
            result += self.visit_block(conditional.if_false)
            result += '}'
        return result

    def visit_print(self, print_object):
        return 'print ' + self.visit_expression(print_object.expr) + ';'

    def visit_read(self, read):
        return 'read ' + read.name + ';'

    def visit_reference(self, reference):
        return self.visit_expression(reference) + ';'

    def visit_function_call(self, function_call):
        return self.visit_expression(function_call) + ';'

    def visit_binary_operation(self, binary_operation):
        return self.visit_expression(binary_operation) + ';'

    def visit_unary_operation(self, unary_operation):
        return self.visit_expression(unary_operation) + ';'


def pretty_print(program):
    result = program.accept(PrettyPrinter(ExpressionPrinter()))
    print(result)
