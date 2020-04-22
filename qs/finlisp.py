#!/usr/bin/python3

import argparse
import operator
import qsutils
import sexpdata

finlisp_forms = {}

def def_finlisp_form(fname, fimpl):
    finlisp_forms[fname] = fimpl

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

def finlisp_let(context, bindings, *bodyforms):
    new_context = context.copy()
    new_context['bindings'] = [{binding[0]._val: finlisp_eval(binding[1], context)
                                for binding in bindings}] + context['bindings']
    result = None
    for body_form in bodyforms:
        result = finlisp_eval(body_form, new_context)
    return result

def_finlisp_form('let', finlisp_let)

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

class UndefinedName(Exception):
    pass

    def __init__(self, function_name, value):
        self.function_name = function_name

def finlisp_eval_list(expr, context):
    fun_name = sexpdata.car(expr)._val
    if fun_name in finlisp_forms:
        return finlisp_forms[fun_name](context,
                                       *sexpdata.cdr(expr))
    elif fun_name in finlisp_functions:
        return finlisp_functions[fun_name](context,
                                           *[finlisp_eval(x, context)
                                             for x in sexpdata.cdr(expr)])
    else:
        raise(UndefinedName, fun_name)

def finlisp_eval_symbol(expr, context):
    symname = expr._val
    for frame in context['bindings']:
        if symname in frame:
            return frame[symname]
    raise(UndefinedName, expr)

def finlisp_eval_literal(expr, context):
    return expr

finlisp_type_evaluators = {
    list: finlisp_eval_list,
    sexpdata.Symbol: finlisp_eval_symbol
}

def finlisp_eval(expr, context={'bindings': []}):
    return finlisp_type_evaluators.get(type(expr),
                                       finlisp_eval_literal)(expr,
                                                             context)

def finlisp_load_file(filename):
    with open(filename) as instream:
        parser = sexpdata.Parser(instream.read())
        _, sexps = parser.parse_sexp(0)
        global_context = {'bindings': [{}]}
        for sexp in sexps:
            # try:
                result = finlisp_eval(sexp, global_context)
                print("==>", result)
            # except Exception as e:
            #     print("error or eof", e)

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("script_files", nargs='*')
    args = parser.parse_args()

    config = qsutils.program_load_config(args)

    for filename in args.script_files:
        finlisp_load_file(filename)

if __name__ == "__main__":
    main()
