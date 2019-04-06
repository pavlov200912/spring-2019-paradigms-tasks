from model import *


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self):
        self.enclosure_counter = 0
        self.deep_counter = 0

    def calc_tabs(self):
        return self.enclosure_counter * '    '

    def visit_number(self, number):
        result = str(number.value)
        if self.deep_counter:
            return result
        return self.calc_tabs() + result + ';'

    def visit_function(self, function):
        pass

    def visit_function_definition(self, function_definition):
        result = self.calc_tabs() + 'def ' + function_definition.name + '('
        result += ', '.join(function_definition.function.args)
        result += ') {\n'
        self.enclosure_counter += 1
        for expr in function_definition.function.body:
            result += expr.accept(self) + '\n'
        self.enclosure_counter -= 1
        result += self.calc_tabs() + '}'
        return result

    def visit_conditional(self, conditional):
        result = self.calc_tabs() + 'if ('
        self.deep_counter += 1
        result += conditional.condition.accept(self)
        self.deep_counter -= 1
        result += ') {\n'
        self.enclosure_counter += 1
        for expr in conditional.if_true or []:
            result += expr.accept(self) + '\n'
        self.enclosure_counter -= 1
        result += self.calc_tabs() + '}'
        if conditional.if_false:
            result += ' else {\n'
            self.enclosure_counter += 1
            for expr in conditional.if_false:
                result += expr.accept(self) + '\n'
            self.enclosure_counter -= 1
            result += self.calc_tabs() + '}'
        return result

    def visit_print(self, print_object):
        self.deep_counter += 1
        result = self.calc_tabs() + 'print ' + print_object.expr.accept(self)
        self.deep_counter -= 1
        return result + ';'

    def visit_read(self, read):
        return self.calc_tabs() + 'read ' + read.name + ';'

    def visit_reference(self, reference):
        if self.deep_counter:
            return reference.name
        return self.calc_tabs() + reference.name + ';'

    def visit_function_call(self, function_call):
        self.deep_counter += 1
        result = function_call.fun_expr.accept(self) + '('
        result += ', '.join([x.accept(self) for x in function_call.args]) + ')'
        self.deep_counter -= 1
        if self.deep_counter:
            return result
        return self.calc_tabs() + result + ';'

    def visit_binary_operation(self, binary_operation):
        self.deep_counter += 1
        result = ('(' + binary_operation.lhs.accept(self) + ' ' +
                  binary_operation.op + ' '
                  + binary_operation.rhs.accept(self) + ')')
        self.deep_counter -= 1
        if self.deep_counter:
            return result
        return result + ';'

    def visit_unary_operation(self, unary_operation):
        self.deep_counter += 1
        result = '(' + unary_operation.op + \
                 unary_operation.expr.accept(self) + ')'
        self.deep_counter -= 1
        if self.deep_counter:
            return result
        return result + ';'


def pretty_print(program):
    result = program.accept(PrettyPrinter())
    print(result)
