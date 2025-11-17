import datetime
import glob
import os
import re

import channels.panels as panels
from expressionive.expressionive import htmltags as T
from orgbookchapterverse.orgbookchapterverse import TextCollection, interlinear_chapter

GOSPEL_LENGTHS = (
    ("Matthew", 28),
    ("Mark", 16),
    ("Luke", 24),
    ("John", 21))

def gospel_chapter(n):
    """Return the Nth chapter of the gospels."""
    gospel = 0
    begin = 1
    end = 0
    for gospel in GOSPEL_LENGTHS:
        end += gospel[1]
        if begin <= n and n <= end:
            return "%s %d" % (gospel[0], (n - begin) + 1)
        begin = end + 1
    return None

def chapter_name_hack(chapter):
    return "Psalms " + chapter.split(" ")[1] if chapter.startswith("Psalm ") else chapter

def emphasize_word(text):
    """Convert markdown-style bolding to expressionive."""
    result = []
    while "**" in text:
        try:
            before, between, after = text.split("**", 2)
            result.append(before)
            result.append(T.span(class_='word')[between])
            text = after
        except:
            # there was an unmatched "**" in the text
            result.append(text)
            return result
    result.append(text)
    return result

def spanify_verse(verse):
    """Put span markers into a numbered verse."""
    number, text = verse.strip(' ').split(' ', 1)
    return [T.span(class_='verse_number')[number], T.span(class_='verse_text')[emphasize_word(text)]]

def chapter_html(bible, chapter):
    print("chapter_html for chapter", chapter)
    """Return the expressionive structure for a Bible chapter."""
    return T.div(class_="bible_chapter")[[T.p(class_="bible_verse")[spanify_verse(line)]
                                          for line in bible.chapter(chapter_name_hack(chapter)).lines()
                                          if line]]

def chapters_html(bible, chapters):
    """Return the expressionive structure for a list of Bible chapters."""
    return [[T.h3[chapter], chapter_html(bible, chapter)]
            for chapter in chapters]

def interlinear_verse(number, verse):
    return [T.tr[[T.th[str(number)],
                  [[T.td[text]
                    for text in verse]]
                  ]]]

def chapter_interlinear_html(versions, chapter):
    """Return the expressionive structure for an interlinear text."""
    book_name, chapter_number = chapter.rsplit(" ", -1)
    list_of_verses = interlinear_chapter(versions, book_name, chapter_number)
    return T.table()[
        [[T.tr[[T.th(class_="verse_number")[str(vnumber)],
                [[T.td(class_="verse_text")[emphasize_word(text)]
                  for text in verse]]
                ]]]
         for vnumber, verse in enumerate(list_of_verses, start=1)]]

def chapters_interlinear_html(versions, chapters):
    """Return the expressionive structure for a list of Bible chapters."""
    return [[T.h3[chapter], chapter_interlinear_html(versions, chapter)]
            for chapter in chapters]

def psalm_titles(day_of_month, pluralise=False):
    """Return the psalms for today, as a list."""
    start = min(day_of_month, 30) * 5
    return [("Psalms %d" if pluralise else "Psalm %d") % (start + offset)
            for offset in range(5)]

def psalm_titles_string(day_of_month):
    """Return the psalms for today, as a string."""
    return ", ".join(psalm_titles(day_of_month))

def proverbs_titles(day_of_month):
    """Return the proverbs for today, as a list."""
    return ["Proverbs %d" % day_of_month]

def proverbs_titles_string(day_of_month):
    """Return the proverbs for today, as a string."""
    return ", ".join(proverbs_titles(day_of_month))

def gospel_titles(day_of_month):
    """Return the gospels for today, as a list."""
    base = day_of_month*3
    return [gospel_chapter(i+1) for i in range(base, base+3)]

def gospel_titles_string(day_of_month):
    """Return the gospels for today, as a string."""
    return ", ".join(gospel_titles(day_of_month))

class BiblePanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.bible = TextCollection(os.path.expandvars("$BIBLE/kj.org"), "KJV")
        self.bible_al = TextCollection(os.path.expandvars("$BIBLE/al.org"), "Shqip")
        # self.bible_pl = TextCollection(os.path.expandvars("$BIBLE/pl.org"), "Polska")
        self.versions = [
            self.bible,
            self.bible_al,
            # self.bible_pl,
        ]

    def name(self):
        return "bible"

    def label(self):
        return "Bible"

    # def update(self, verbose=False, messager=None, **kwargs):
    #     """Update the cached data."""
    #     super().update(verbose, messager)
    #     return self

    def html(self, _messager=None):
        """Generate an expressionive HTML structure for the Bible readings."""
        day_of_month = datetime.date.today().day
        interlinear = True
        return T.div(class_='bible')[
            T.h2["Psalms: ", psalm_titles_string(day_of_month)],
            (chapters_interlinear_html(self.versions, psalm_titles(day_of_month, pluralise=True))
             if interlinear
             else chapters_html(self.bible, psalm_titles(day_of_month))),
            T.h2["Proverbs: ", proverbs_titles_string(day_of_month)],
            (chapters_interlinear_html(self.versions, proverbs_titles(day_of_month))
             if interlinear
             else chapters_html(self.bible, proverbs_titles(day_of_month))),
            T.h2["Gospels: ", gospel_titles_string(day_of_month)],
            (chapters_interlinear_html(self.versions, gospel_titles(day_of_month))
             if interlinear
             else chapters_html(self.bible, gospel_titles(day_of_month))),
        ]
