"""Example panel to copy and base new ones on."""

from collections import defaultdict
import datetime

import channels.panels as panels
import coimealta.inventory.storage as storage
from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_section, SectionalPage

def items_table(items):
    items_by_type = dict()
    for item in items.values():
        subtype = item.get('Subtype')
        key = ("%s (%s)" % (item['Type'], subtype)) if subtype else (item['Type'])
        if key in items_by_type:
            items_by_type[key].append(item)
        else:
            items_by_type[key] = [item]
    return T.div(class_='inventory_list')[
        T.table[[T.tr[T.td[T.span(class_='overview')[key,
                                                     T.div(class_='details')[T.ul[[[T.li[x['Item']]]
                                                                                   for x in items_by_type[key]]]]]],
                      T.td[len(items_by_type[key])]]
                 for key in sorted(items_by_type.keys(),
                                   reverse=True,
                                   key=lambda k: len(items_by_type[k]))]]]

class InventoryPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.locations = None
        self.items = None
        self.stock = None
        self.project_parts = None
        self.media = None
        self.media_by_type = None
        self.latest_book = None
        self.volume = None
        self.bookshelf_length = None
        self.other_length = None
        self.area = None

    def name(self):
        return "inventory"

    def label(self):
        return "Inventory"

    def update(self, verbose=False, messager=None, **kwargs):
        """Update the cached data."""
        self.locations = storage.read_locations("$SYNCED/org/storage.csv")
        self.items = storage.read_inventory("$SYNCED/org/inventory.csv")
        self.stock = storage.read_inventory("$SYNCED/org/stock.csv")
        self.project_parts = storage.read_inventory("$SYNCED/org/project-parts.csv")
        self.media = storage.read_books("$SYNCED/org/books.csv")
        self.media_by_type = defaultdict(list)
        for medium in self.media.values():
            self.media_by_type[medium['MediaType']].append(medium)

        books_with_acquisition_date = sorted([book
                                              for book in self.media_by_type['Book']
                                              if book.get('Acquired', None) is not None],
                                             key=lambda b: b['Acquired'])
        self.latest_book = books_with_acquisition_date[-1]

        _, self.volume, self.bookshelf_length, self.other_length, self.area = storage.calculate_capacities(self.locations)
        self.updated = datetime.datetime.now()
        super().update(verbose, messager)
        return self

    def html(self, _messager=None):
        """Generate an expressionive HTML structure from the cached data."""
        return T.div(class_='inventory')[
            T.div[wrap_box(
                T.div[T.h3["Media"],
                      T.div(class_='inventory_list')[
                          T.table[
                              [T.tr[
                                  T.td[mtype],
                                  T.td[str(len(self.media_by_type[mtype]))]]
                               for mtype in sorted(self.media_by_type)]]]],
                T.div[T.h3["General possessions"], items_table(self.items)],
                T.div[T.h3["Project parts"], items_table(self.project_parts)],
                T.div[T.h3["Stock"], items_table(self.stock)],
                T.div[T.h3["Storage"],
                      T.div(class_='inventory_list')[T.table[
                          T.tr[T.td["Container volume"], T.td["%g litres" % self.volume]],
                          T.tr[T.td["Bookshelf length"], T.td["%g metres" % self.bookshelf_length]],
                          T.tr[T.td["Other shelf length"], T.td["%g metres" % self.other_length]]]]])]]
