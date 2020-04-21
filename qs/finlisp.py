#!/usr/bin/python3

import argparse
import qsutils
import sexpdata

finlisp_forms = {}

def def_finlisp_form(fname, fimpl):
    finlisp_forms[sexpdata.Symbol(fname)] = fimpl

def finlisp_setq(context, args):
    key = args[0]
    value = args[1]
    bindings = context['bindings']
    for frame in bindings:
        if key in frame:
            frame[key] = value
            return value
    bindings[-1][key] = value
    return value

def_finlisp_form('setq', finlisp_setq)

def finlisp_let(context, args):
    new_binding_form = args[0]
    body_forms = args[1:]
    # todo: take copy of context, set new bindings list in it, fill in, eval body forms

def_finlisp_form('let', finlisp_let)

finlisp_functions = {}

def def_finlisp_fn(fname, fimpl):
    finlisp_functions[sexpdata.Symbol(fname)] = fimpl

class UndefinedName(Exception):
    pass

    def __init__(self, function_name, value):
        self.function_name = function_name

def finlisp_eval_list(expr, context):
    fun_name = sexpdata.car(expr)
    if fun_name in finlisp_forms:
        finlisp_forms[fun_name](context, *sexpdata.cdr(expr))
    elif fun_name in finlisp_functions:
        finlisp_functions[fun_name](context, *[finlisp_eval(x, context)
                                               for x in sexpdata.cdr(expr)])
    else:
        raise(UndefinedName, fun_name)

def finlisp_eval_symbol(expr, context):
    for frame in context['bindings']:
        if expr in frame:
            return frame[expr]
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
        while True:
            try:
                contents = sexpdata.load(instream)
                result = finlisp_eval(contents, None)
                print("==> ", result)
            except Exception as e:
                print("error or eof", e)
                break

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("script_files", nargs='*')
    args = parser.parse_args()

    config = qsutils.program_load_config(args)

    for filename in args.script_files:
        print("reading", filename)
        finlisp_load_file(filename)

if __name__ == "__main__":
    main()
