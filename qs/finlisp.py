#!/usr/bin/python3

import argparse
import operator
import os

import sexpdata

import account
import canonical_sheet
import csv_sheet
import finfuns
import finlisp_evaluation
import qsutils

def finlisp_read_canonical(context, csvname):
    return canonical_sheet.canonical_sheet(context['config'],
                                           input_sheet=csvname,
                                           # verbose=True,
                                           convert_all=True)

finlisp_evaluation.def_finlisp_fn('read-canonical', finlisp_read_canonical)

def finlisp_format_string(_, fmt_string, *fmt_args):
    return fmt_string % fmt_args

finlisp_evaluation.def_finlisp_fn('format', finlisp_format_string)

def finlisp_getenv(_, varname, default_value=None):
    return os.getenv(varname, default_value)

finlisp_evaluation.def_finlisp_fn('getenv', finlisp_getenv)

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

class NotApplicable(BaseException):
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

def finlisp_length(_, x):
    return len(x)

finlisp_evaluation.def_finlisp_fn('length', finlisp_length)

def finlisp_print(_, *values):
    print(*values)

finlisp_evaluation.def_finlisp_fn('print', finlisp_print)

def finlisp_builtins(_):
    return sorted(finlisp_functions.keys())

finlisp_evaluation.def_finlisp_fn('builtins', finlisp_builtins)

for fname in finfuns.functions:
    finlisp_evaluation.def_finlisp_fn(fname.replace('_', '-'), eval("finfuns." + fname))

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("script_files", nargs='*')
    args = parser.parse_args()

    context = {
        'config': qsutils.program_load_config(args, quiet=True),
        'bindings': [{'output-dir': "."}],
        'eval-stack': []
    }

    for filename in args.script_files:
        finlisp_evaluation.finlisp_load_file(context, filename)

if __name__ == "__main__":
    main()
