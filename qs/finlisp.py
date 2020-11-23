#!/usr/bin/python3

import argparse
import operator
import os

import sexpdata

import account
import canonical_sheet
import csv_sheet
import finfuns
import qsutils

finlisp_forms = {}

def finlisp_var_lookup(context, varname):
    for frame in context['bindings']:
        if varname in frame:
            return frame[varname], True
    return None, False

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

for fname in finfuns.functions:
    def_finlisp_fn(fname.replace('_', '-'), eval("finfuns." + fname))

def finlisp_read_canonical(context, csvname):
    return canonical_sheet.canonical_sheet(context['config'],
                                           input_sheet=csvname,
                                           # verbose=True,
                                           convert_all=True)

def_finlisp_fn('read-canonical', finlisp_read_canonical)

def finlisp_format_string(_, fmt_string, *fmt_args):
    return fmt_string % fmt_args

def_finlisp_fn('format', finlisp_format_string)

def finlisp_getenv(_, varname, default_value=None):
    return os.getenv(varname, default_value)

def_finlisp_fn('getenv', finlisp_getenv)

def finlisp_account(context, name, base_sheet):
    print("Making account from base_sheet", base_sheet)
    result = account.account(name, transactions=base_sheet, config=context['config'])
    print("Account made from base_sheet is", result)
    return result

def_finlisp_fn('account', finlisp_account)

# covered in the bulk import
# def finlisp_add_sheet(context, base, addendum):
#     print("Adding", addendum, "to base", base)
#     result = base.add_sheet(addendum)
#     print("Result of adding", addendum, "to base", base, "is", result)
#     return result
#
# def_finlisp_fn('add-sheet', finlisp_add_sheet)

class NotApplicable(BaseException):
    pass

    def __init__(self, action, value):
        self.action = action
        self.value = value

def finlisp_headers(_, x):
    return x.column_names_list()

def_finlisp_fn('headers', finlisp_headers)

def finlisp_payees(_, x):
    if isinstance(x, account.account):
        return sorted(x.payees.keys())
    else:
        raise NotApplicable("headers", x)

def_finlisp_fn('payees', finlisp_payees)

def finlisp_length(_, x):
    return len(x)

def_finlisp_fn('length', finlisp_length)

def finlisp_print(_, *values):
    print(*values)

def_finlisp_fn('print', finlisp_print)

def finlisp_builtins(_):
    return sorted(finlisp_functions.keys())

def_finlisp_fn('builtins', finlisp_builtins)

class UndefinedName(BaseException):
    pass

    def __init__(self, function_name, value):
        self.function_name = function_name

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
    return finlisp_type_evaluators.get(type(expr),
                                       finlisp_eval_literal)(context,
                                                             expr)

def finlisp_load_file(context, filename):
    with open(filename) as instream:
        parser = sexpdata.Parser(instream.read())
        _, sexps = parser.parse_sexp(0)
        for sexp in sexps:
            # try:
                result = finlisp_eval(context, sexp)
                print("==>", result)
            # except Exception as e:
            #     print("error or eof", e)

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("script_files", nargs='*')
    args = parser.parse_args()

    context = {
        'config': qsutils.program_load_config(args),
        'bindings': [{}]
    }

    for filename in args.script_files:
        finlisp_load_file(context, filename)

if __name__ == "__main__":
    main()
