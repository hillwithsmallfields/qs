import datetime
import glob
import os
import re

import channels.panels as panels
from expressionive.expressionive import htmltags as T
from orgbookchapterverse.orgbookchapterverse import TextCollection, interlinear_chapter

# 32 chapters to add to the 150 psalms gives 182 chapters, which is
# twice the 91 days of the nearly-quarter-year cycle, so we get Old
# Testament two readings a day, mostly from the Psalms.
OLD_TESTAMENT_LENGTHS = (
    ("Ecclesiastes", 12),
    ("Song of Solomon", 8),
    ("Ecclesiastes", 12),
)

GOSPEL_LENGTHS = (
    ("Matthew", 28),
    ("Mark", 16),
    ("Luke", 24),
    ("John", 21))

def book_sequence_chapter(book_sequence, n):
    """Return the Nth chapter from a sequence of books."""
    book = 0
    begin = 1
    end = 0
    for book in book_sequence:
        end += book[1]
        if begin <= n and n <= end:
            return "%s %d" % (book[0], (n - begin) + 1)
        begin = end + 1
    return None

def old_testament_chapter(n):
    """Return the Nth chapter from selected Old Testament books."""
    return book_sequence_chapter(OLD_TESTAMENT_LENGTHS, n)

def gospel_chapter(n):
    """Return the Nth chapter of the gospels."""
    return book_sequence_chapter(GOSPEL_LENGTHS, n)

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

def psalm_titles(day_of_cycle, pluralise=False):
    """Return the psalms for today, as a list."""
    double_day = day_of_cycle * 2
    return (["Psalms %d" % (double_day - 1),
             "Psalms %d" % double_day]
            if double_day <= 150
            else [old_testament_chapter(double_day - 151),
                  old_testament_chapter(double_day - 150)])

def psalm_titles_string(day_of_cycle):
    """Return the psalms for today, as a string."""
    return ", ".join(psalm_titles(day_of_cycle))

def proverbs_titles(day_of_cycle):
    """Return the proverbs for today, as a list."""
    return [("Proverbs %d" % ((day_of_cycle-1) % 31 + 1))
             if day_of_cycle <= 89
             else ("Proverbs 28"
                   if day_of_cycle == 90
                   else "Proverbs 30")]

def proverbs_titles_string(day_of_cycle):
    """Return the proverbs for today, as a string."""
    return ", ".join(proverbs_titles(day_of_cycle))

def gospel_titles(day_of_cycle):
    """Return the gospels for today, as a list."""
    return [gospel_chapter(day_of_cycle)
            if day_of_cycle <= 89
            else ('Proverbs 29'
                  if day_of_cycle == 90
                  else 'Proverbs 31')]

def gospel_titles_string(day_of_cycle):
    """Return the gospels for today, as a string."""
    return ", ".join(gospel_titles(day_of_cycle))

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
        today = datetime.date.today()
        day_of_cycle = (today - today.replace(month=1, day=1)).days % 91 + 1
        interlinear = True
        return T.div(class_='bible')[
            T.h2["Psalms: ", psalm_titles_string(day_of_cycle)],
            (chapters_interlinear_html(self.versions, psalm_titles(day_of_cycle, pluralise=True))
             if interlinear
             else chapters_html(self.bible, psalm_titles(day_of_cycle))),
            T.h2["Proverbs: ", proverbs_titles_string(day_of_cycle)],
            (chapters_interlinear_html(self.versions, proverbs_titles(day_of_cycle))
             if interlinear
             else chapters_html(self.bible, proverbs_titles(day_of_cycle))),
            T.h2["Gospels: ", gospel_titles_string(day_of_cycle)],
            (chapters_interlinear_html(self.versions, gospel_titles(day_of_cycle))
             if interlinear
             else chapters_html(self.bible, gospel_titles(day_of_cycle))),
        ]
