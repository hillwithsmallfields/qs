#!/usr/bin/python3

import functools
import sexpdata
import traceback
import operator
import finfuns

finlisp_forms = {}

def def_finlisp_form(fname, fimpl):
    finlisp_forms[fname] = fimpl

def finlisp_var_lookup(context, varname):
    for frame in context['bindings']:
        if varname in frame:
            return frame[varname], True
    return None, False

def finlisp_var_value(context, varname):
    value, found = finlisp_var_lookup(context, varname)
    return value if found else None

def finlisp_setq(context, args):
    while args:
        key = args[0]
        value = args[1]
        for frame in context['bindings']:
            if key in frame:
                frame[key] = value
                return value
        bindings[-1][key] = value
        args = args[2:]
    return value

def_finlisp_form('setq', finlisp_setq)

def finlisp_progn(context, *bodyforms):
    result = None
    for body_form in bodyforms:
        result = finlisp_eval(context, body_form)
    return result

def_finlisp_form('progn', finlisp_progn)

def finlisp_if(context, condition, then_form, *else_forms):
    if finlisp_eval(context, condition):
        return finlisp_eval(context, then_form)
    else:
        result = None
        for else_form in else_forms:
            result = finlisp_eval(context, else_form)
        return result

def_finlisp_form('if', finlisp_if)

def finlisp_when(context, condition, *when_forms):
    if finlisp_eval(context, condition):
        result = None
        for when_form in when_forms:
            result = finlisp_eval(context, when_form)
        return result
    else:
        return None

def_finlisp_form('when', finlisp_when)

def finlisp_unless(context, condition, *unless_forms):
    if not finlisp_eval(context, condition):
        result = None
        for unless_form in unless_forms:
            result = finlisp_eval(context, unless_form)
        return result
    else:
        return None

def_finlisp_form('unless', finlisp_unless)

def finlisp_let(context, bindings, *bodyforms):
    new_context = context.copy()
    new_context['bindings'] = [{binding[0]._val: finlisp_eval(context, binding[1])
                                for binding in bindings}] + context['bindings']
    result = None
    for body_form in bodyforms:
        result = finlisp_eval(new_context, body_form)
    return result

def_finlisp_form('let', finlisp_let)

def finlisp_letstar(context, bindings, *bodyforms):
    new_context = context.copy()
    new_context['bindings'] = [{}] + context['bindings'].copy()
    for binding in bindings:
        new_context['bindings'][0][binding[0]._val] = finlisp_eval(new_context, binding[1])
    result = None
    for body_form in bodyforms:
        result = finlisp_eval(new_context, body_form)
    return result

def_finlisp_form('let*', finlisp_letstar)

def finlisp_dolist(context, control_binding, *bodyforms):
    new_context = context.copy()
    control_var_name = control_binding[0]._val
    control_list = finlisp_eval(context, control_binding[1])
    our_binding = {control_var_name: None}
    new_context['bindings'] = [our_binding] + context['bindings']
    result = None
    for list_item in control_list:
        our_binding[control_var_name] = list_item
        for body_form in bodyforms:
            result = finlisp_eval(new_context, body_form)
    return result

def_finlisp_form('dolist', finlisp_dolist)

def finlisp_for_each_row(context, sheet, row_var, forward, *bodyforms):
    """Iterate over rows, going forward or backward in time."""
    sheet = finlisp_eval(context, sheet)
    new_context = context.copy()
    row_var_name = row_var._val
    new_context['bindings'] = [{row_var_name: None}] + context['bindings']
    result = None
    for timestamp in sorted(sheet.rows.keys(), reverse=not forward):
        row = sheet.rows[timestamp]
        new_context['bindings'][0][row_var_name] = row
        for body_form in bodyforms:
            result = finlisp_eval(new_context, body_form)
    return result

def_finlisp_form('for-each-row', finlisp_for_each_row)

def finlisp_defun(context, fname, fargs, *fbody):
    context['bindings'][0][fname._val] = ['lambda', fargs] + list(fbody)

def_finlisp_form('defun', finlisp_defun)

finlisp_functions = {}

def def_finlisp_fn(fname, fimpl):
    finlisp_functions[fname] = fimpl

def def_finlisp_wrapped_fn(fname, basefn):
    finlisp_functions[fname] = lambda context, *args: basefn(*args)

def def_finlisp_wrapped_nargs_fn(fname, basefn):
    finlisp_functions[fname] = lambda context, *args: functools.reduce(basefn, args)

for fname, basefn in {'<': operator.lt,
                      '<=': operator.le,
                      '==': operator.eq,
                      '!=': operator.ne,
                      '>': operator.gt,
                      '>=': operator.ge}.items():
    def_finlisp_wrapped_fn(fname, basefn)

for fname, basefn in {'+': operator.add,
                      '-': operator.sub,
                      '*': operator.mul,
                      '/': operator.floordiv,
                      '%': operator.mod}.items():
    def_finlisp_wrapped_nargs_fn(fname, basefn)

def finlisp_extend_stack(context, form):
    new_context = context.copy()
    new_stack = new_context['eval-stack'].copy()
    new_context['eval-stack'] = new_stack
    new_stack.append(form)
    return new_context

class UndefinedName(BaseException):

    def __init__(self, function_name, value):
        self.function_name = function_name

class InvalidFunction(BaseException):

    def __init__(self, form):
        self.form = form
        
class EvalError(BaseException):

    def __init__(self, form):
        self.form = form

def finlisp_eval_list(context, expr):
    if len(expr) == 0:
        return expr             # nil / empty list
    fun_name = sexpdata.car(expr)._val
    if fun_name in finlisp_forms:
        return finlisp_forms[fun_name](context,
                                       *sexpdata.cdr(expr))
    elif fun_name in finlisp_functions:
        return finlisp_functions[fun_name](context,
                                           *[finlisp_eval(context, x)
                                             for x in sexpdata.cdr(expr)])
    else:
        user_fn, found = finlisp_var_lookup(context, fun_name)
        if found:
            if isinstance(user_fn, list) and user_fn[0] == 'lambda' and len(user_fn) >= 3:
                new_context = context.copy()
                new_context['bindings'] = [{binding[0]._val: finlisp_eval(context, binding[1])
                                            for binding in zip(user_fn[1], expr[1:])}] + context['bindings']
                result = None
                for body_form in user_fn[2:]:
                    result = finlisp_eval(new_context, body_form)
                return result
            else:
                raise InvalidFunction(fun_name)
        else:
            raise UndefinedName(fun_name)

def finlisp_eval_symbol(context, expr):
    result, found = finlisp_var_lookup(context, expr._val)
    if found:
        return result
    print("Name", expr._val, "not defined")
    raise(UndefinedName, expr)

def finlisp_eval_quoted(context, expr):
    return expr._val

def finlisp_eval_literal(context, expr):
    return expr

finlisp_type_evaluators = {
    list: finlisp_eval_list,
    sexpdata.Symbol: finlisp_eval_symbol,
    sexpdata.Quoted: finlisp_eval_quoted
}

def finlisp_eval(context, expr):
    try:
        return finlisp_type_evaluators.get(type(expr),
                                           finlisp_eval_literal)(finlisp_extend_stack(context,
                                                                                      expr),
                                                                 expr)
    except Exception as e:
        print("Error in evaluating", expr)
        traceback.print_exc()
        for frame in context['eval-stack']:
            print("eval:", sexpdata.dumps(frame))
        for binding_frame in context['bindings']:
            print("bindings:")
            for varname, varval in binding_frame.items():
                print("    " + varname + ":", varval)
        raise(EvalError, expr)

def lisp_to_string(value):
    try:
        return sexpdata.dumps(value)
    except:
        return str(value)

def finlisp_load_file(context, filename):
    with open(filename) as instream:
        parser = sexpdata.Parser(instream.read())
        _, sexps = parser.parse_sexp(0)
        for sexp in sexps:
            result = finlisp_eval(context, sexp)
            print("==>", lisp_to_string(result))
