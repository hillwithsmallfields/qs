# Financial spreadsheet functions

import re

functions = ['list_accounts', 'write_accounts']

functions_regexp = re.compile(r"\b(" + "|".join(functions) + r")\(")

def add_package_prefixes(command):
    """Add package prefixes to a Python string."""
    return functions_regexp.sub(r"finfuns.\1(variables, ", command)

def list_accounts(variables, filename):
    with open(filename, 'w') as outfile:
        for name in sorted(variables.keys()):
            outfile.write(name + "\n")
