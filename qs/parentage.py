#!/usr/bin/python3

import yaml

def add_parentage(tree, table, ancestry):
    # print("  tree", tree, "ancestry", ancestry)
    for parent, children in tree.items():
        # print("    parent", parent, "children", children, "ancestry", ancestry)
        if len(children) == 0:
            # print("    registering", parent, "as having ancestry", ancestry)
            table[parent] = ancestry
        else:
            # print("    recursing with", children, "and ancestry", ancestry + [parent])
            add_parentage(children, table, ancestry + [parent])

def read_parentage_table(parentage_filename):
    if parentage_filename.endswith(".yaml"):
        with open(parentage_filename) as instream:
            tree = yaml.safe_load(instream)
    else:
        print("don't know how to read parentage from", parentage_filename)
        return None
    table = {}
    add_parentage(tree, table, [])
    print("read parentage table", table)
    return table

def main():
    """Test program for read_parentage_table."""
    for key, value in read_parentage_table("cats.yaml").items():
        print(key, value)
            
if __name__ == "__main__":
    main()
    
