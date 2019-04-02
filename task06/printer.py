from model import *


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self):
        self.enclosure_counter = 0
        self.deep_counter = 0

    def calc_tabs(self):
        return self.enclosure_counter * '    '

    def visit_conditional(self, acceptor):
        result = self.calc_tabs() + 'if ('
        self.deep_counter += 1
        result += acceptor.condition.accept(self)
        self.deep_counter -= 1
        result += ') {\n'
        self.enclosure_counter += 1
        for expr in acceptor.if_true or []:
            result += expr.accept(self) + '\n'
        self.enclosure_counter -= 1
        result += self.calc_tabs() + '}'
        if acceptor.if_false:
            result += ' else {\n'
            self.enclosure_counter += 1
            for expr in acceptor.if_false:
                result += expr.accept(self) + '\n'
            self.enclosure_counter -= 1
            result += self.calc_tabs() + '}'
        return result

    def visit_function_definition(self, acceptor):
        result = self.calc_tabs() + 'def ' + acceptor.name + '('
        result += ', '.join(acceptor.function.args)
        result += ') {\n'
        self.enclosure_counter += 1
        for expr in acceptor.function.body:
            result += expr.accept(self) + '\n'
        self.enclosure_counter -= 1
        result += self.calc_tabs() + '}'
        return result

    def visit_print(self, acceptor):
        self.deep_counter += 1
        result = self.calc_tabs() + 'print ' + acceptor.expr.accept(self)
        self.deep_counter -= 1
        return result + ';'

    def visit_read(self, acceptor):
        return self.calc_tabs() + 'read ' + acceptor.name + ';'

    def visit_number(self, acceptor):
        result = str(acceptor.value)
        if self.deep_counter:
            return result
        return self.enclosure_counter * '    ' + result + ';'

    def visit_reference(self, acceptor):
        if self.deep_counter:
            return acceptor.name
        return self.calc_tabs() + acceptor.name + ';'

    def visit_binary_operation(self, acceptor):
        self.deep_counter += 1
        result = '(' + acceptor.lhs.accept(self) + ' ' + acceptor.op + ' '\
                 + acceptor.rhs.accept(self) + ')'
        self.deep_counter -= 1
        if self.deep_counter:
            return result
        return result + ';'

    def visit_unary_operation(self, acceptor):
        self.deep_counter += 1
        result = '(' + acceptor.op + acceptor.expr.accept(self) + ')'
        self.deep_counter -= 1
        if self.deep_counter:
            return result
        return result + ';'

    def visit_function_call(self, acceptor):
        self.deep_counter += 1
        result = acceptor.fun_expr.accept(self) + '('
        result += ', '.join([x.accept(self) for x in acceptor.args]) + ')'
        self.deep_counter -= 1
        if self.deep_counter:
            return result
        return self.calc_tabs() + result + ';'

    def visit_function(self, acceptor):
        pass


def pretty_print(program):
    result = program.accept(PrettyPrinter())
    print(result)


pretty_print()