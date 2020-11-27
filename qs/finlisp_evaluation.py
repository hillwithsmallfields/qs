#!/usr/bin/python3

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
    key = args[0]
    value = args[1]
    for frame in context['bindings']:
        if key in frame:
            frame[key] = value
            return value
    bindings[-1][key] = value
    return value

def_finlisp_form('setq', finlisp_setq)

def finlisp_progn(context, *bodyforms):
    result = None
    for body_form in bodyforms:
        result = finlisp_eval(context, body_form)
    return result

def_finlisp_form('progn', finlisp_progn)

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

finlisp_functions = {}

def def_finlisp_fn(fname, fimpl):
    finlisp_functions[fname] = fimpl

def def_finlisp_wrapped_fn(fname, basefn):
    finlisp_functions[fname] = lambda context, *args: basefn(*args)

for fname, basefn in {'+': operator.add,
                      '-': operator.sub,
                      '*': operator.mul,
                      '/': operator.floordiv,
                      '%': operator.mod}.items():
    def_finlisp_wrapped_fn(fname, basefn)

def finlisp_extend_stack(context, form):
    new_context = context.copy()
    new_stack = new_context['eval-stack'].copy()
    new_context['eval-stack'] = new_stack
    new_stack.append(form)
    return new_context

class UndefinedName(Exception):
    pass

    def __init__(self, function_name, value):
        self.function_name = function_name

class EvalError(Exception):
    pass

    def __init__(self, form):
        self.form = form

def finlisp_eval_list(context, expr):
    fun_name = sexpdata.car(expr)._val
    if fun_name in finlisp_forms:
        return finlisp_forms[fun_name](context,
                                       *sexpdata.cdr(expr))
    elif fun_name in finlisp_functions:
        return finlisp_functions[fun_name](context,
                                           *[finlisp_eval(context, x)
                                             for x in sexpdata.cdr(expr)])
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
            print("eval:", frame)
        raise(EvalError, expr)

def finlisp_load_file(context, filename):
    with open(filename) as instream:
        parser = sexpdata.Parser(instream.read())
        _, sexps = parser.parse_sexp(0)
        for sexp in sexps:
            result = finlisp_eval(context, sexp)
            print("==>", result)
