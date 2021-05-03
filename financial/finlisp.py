#!/usr/bin/python3

import argparse
import os

import sexpdata

import account
import canonical_sheet
import csv_sheet
import finfuns
import finlisp_evaluation
import qsutils

def finlisp_read(context, csvname):
    return csv_sheet.csv_sheet(context['config'],
                               input_filename=csvname)

finlisp_evaluation.def_finlisp_fn('read', finlisp_read)

def finlisp_read_canonical(context, csvname):
    return canonical_sheet.canonical_sheet(context['config'],
                                           input_sheet=csvname,
                                           # verbose=True,
                                           convert_all=True)

finlisp_evaluation.def_finlisp_fn('read-canonical', finlisp_read_canonical)

def finlisp_format_string(_, fmt_string, *fmt_args):
    return fmt_string % fmt_args

finlisp_evaluation.def_finlisp_fn('format', finlisp_format_string)

def finlisp_concat(_, *strings):
    return "".join(strings)

finlisp_evaluation.def_finlisp_fn('concat', finlisp_concat)

def finlisp_getenv(_, varname, default_value=None):
    return os.getenv(varname, default_value)

finlisp_evaluation.def_finlisp_fn('getenv', finlisp_getenv)

def finlisp_append(_, *lists):
    result = lists[0]
    for onelist in lists[1:]:
        result = result + onelist
    return result

finlisp_evaluation.def_finlisp_fn('append', finlisp_append)

def finlisp_cons(_, a, b):
    return [a] + b

finlisp_evaluation.def_finlisp_fn('cons', finlisp_cons)

def finlisp_car(_, a):
    return a[0]

finlisp_evaluation.def_finlisp_fn('car', finlisp_car)

def finlisp_cdr(_, a):
    return a[1:]

finlisp_evaluation.def_finlisp_fn('cdr', finlisp_cdr)

def make_empty_dir(_, dirname):
    os.makedirs(dirname, exist_ok=True)
    for filename in os.listdir(dirname):
        os.remove(os.path.join(dirname, filename))

finlisp_evaluation.def_finlisp_fn('make-empty-dir', make_empty_dir)

def finlisp_account(context, name, base_sheet):
    print("Making account from base_sheet", base_sheet)
    result = account.account(name, transactions=base_sheet, config=context['config'])
    print("Account made from base_sheet is", result)
    return result

finlisp_evaluation.def_finlisp_fn('account', finlisp_account)

# covered in the bulk import
# def finlisp_add_sheet(context, base, addendum):
#     print("Adding", addendum, "to base", base)
#     result = base.add_sheet(addendum)
#     print("Result of adding", addendum, "to base", base, "is", result)
#     return result
#
# finlisp_evaluation.def_finlisp_fn('add-sheet', finlisp_add_sheet)

class NotApplicable(Exception):
    pass

    def __init__(self, action, value):
        self.action = action
        self.value = value

def finlisp_headers(_, x):
    return x.column_names_list()

finlisp_evaluation.def_finlisp_fn('headers', finlisp_headers)

def finlisp_payees(_, x):
    if isinstance(x, account.account):
        return sorted(x.payees.keys())
    else:
        raise NotApplicable("headers", x)

finlisp_evaluation.def_finlisp_fn('payees', finlisp_payees)

def finlisp_get(_, dictionary, key):
    return dictionary.get(key, None)

finlisp_evaluation.def_finlisp_fn('get', finlisp_get)

def finlisp_in(_, dictionary, key):
    return key in dictionary

finlisp_evaluation.def_finlisp_fn('in', finlisp_in)

def finlisp_length(_, x):
    return len(x)

finlisp_evaluation.def_finlisp_fn('length', finlisp_length)

def finlisp_print(_, *values):
    print(*values)
    return values

finlisp_evaluation.def_finlisp_fn('print', finlisp_print)

def finlisp_builtins(_):
    return sorted(finlisp_evaluation.finlisp_functions.keys())

finlisp_evaluation.def_finlisp_fn('builtins', finlisp_builtins)

for fname in finfuns.functions:
    finlisp_evaluation.def_finlisp_fn(fname.replace('_', '-'), eval("finfuns." + fname))

def lisp_debug(context, *args):
    config = context['config'] if len(args) < 2 else args[0].config
    label = args[-1]
    if config and 'formats' in config:
        print("at", label, "financisto column sequence is", len(config['formats']['financisto']['column-sequence']), "columns", config['formats']['financisto']['column-sequence'])

finlisp_evaluation.def_finlisp_fn('debug', lisp_debug)

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("--bind", "-D",
                        nargs=2,
                        action='append',
                        help="""Preset a Lisp variable.""")
    parser.add_argument("--output-dir", "-O",
                        help="""Set the default output directory.""")
    parser.add_argument("script_files", nargs='*')
    args = parser.parse_args()

    finlisp_main(args.script_files,
                 args.output_dir,
                 qsutils.program_load_config(args, quiet=True),
                 args.verbose,
                 {binding[0]: binding[1]
                  for binding in args.bind})

def finlisp_main(script_files, output_dir, config, verbose, bindings):

    initial_bindings = {
        'output-dir': output_dir or ".",
        'verbose': verbose,
        't': True,
        'nil': False}

    if bindings:
        initial_bindings.update(bindings)

    # print("finlisp_main using config formats", config['formats'])
    for fname, fdef in config['formats'].items():
        print("fname", fname, "binds", sorted(fdef.keys()))

    for filename in script_files:
        finlisp_evaluation.finlisp_load_file({'config': config,
                                              'project_source': os.path.dirname(os.path.dirname(os.path.realpath(__file__))),
                                              'bindings': [initial_bindings],
                                              'eval-stack': []},
                                             filename)

if __name__ == "__main__":
    main()
