#!/usr/bin/python3

import argparse
import qsutils
import sexpdata

finlisp_functions = {}

def def_finlisp_fn(fname, fimpl):
    finlisp_functions[sexpdata.Symbol(fname)] = fimpl

def finlisp_eval_list(expr, context):
    fun_name = sexpdata.car(expr)
    fun_args = [finlisp_eval(x, context) for x in sexpdata.cdr(expr)]
    print("got function", fun_name, "and args", fun_args)
    return expr

def finlisp_eval_symbol(expr, context):
    return expr

def finlisp_eval_literal(expr, context):
    return expr

finlisp_type_evaluators = {
    list: finlisp_eval_list,
    sexpdata.Symbol: finlisp_eval_symbol
}

def finlisp_eval(expr, context):
    return finlisp_type_evaluators.get(type(expr), finlisp_eval_literal)(expr, context)

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
