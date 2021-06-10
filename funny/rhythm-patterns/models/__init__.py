from fractions import gcd
from itertools import cycle, islice
from collections import OrderedDict
from xml.etree import ElementTree as trees


class Rhythm:
    def __init__(self, name, *args):
        self.name = name
        self.braids = args

    def as_tree(self):
        rhythm = trees.Element('rhythm', name=self.name)
        rhythm.extend([braid.as_tree() for braid in self.braids])
        return rhythm

    def to_html(self):
        rtree = self.as_tree()

        rtree.tag = "div"
        rtree.attrib["class"] = "rhythm"

        for section in rtree:
            section_to_html(section)
        header = trees.Element("h2")
        header.text = rtree.attrib.pop("name")
        rtree.insert(0, header)

        return (
            "{% extends 'index.html' %}\n{% block body %}\n" +
            trees.tostring(rtree).decode("utf-8") +
            "\n{% endblock %}\n"
        )


class Section:
    def __init__(self, name, *args):
        self.name = name

        self.voices = OrderedDict()
        for voice in args:
            self.voices.setdefault(voice.name, Voice())
            self.voices[voice.name] += voice

    def as_tree(self):
        section = trees.Element('section', name=self.name)
        section.extend([voice.as_tree() for voice in self.voices.values()])
        return section


class Voice:
    def __init__(self, name=None, notes=""):
        self.name = name
        self.notes = "".join(note for note in notes if not note.isspace())

    def as_tree(self):
        voice = trees.Element('voice', name=self.name)
        voice.text = self.notes
        return voice

    def __add__(self, another):
        return Voice(another.name, self.notes + another.notes)


def make_node(tag, attributes, *args):
    result = trees.Element(tag, attributes)
    if isinstance(args[0], str):
        result.text = ''.join(args)
    else:
        result.extend(args)
    return result


def lcm(sequence):
    result = 1
    for item in sequence:
        result = int(result * item / gcd(result, item))
    return result


def section_to_html(stree):
    stree.tag = "div"
    stree.attrib["class"] = "section"
    stree[0].attrib["section_name"] = stree.attrib.pop("name")
    children = []
    lcm_len = lcm(len(voice.text) for voice in stree)
    for voice in stree:
        voice.text = ''.join(islice(cycle(voice.text), lcm_len))
    for voices in zip(*[
        voice_to_html(voice)
        for voice in stree
    ]):
        row = trees.Element("div", {"class": "row"})
        row.extend(voices)
        children.append(row)
    for x in list(stree):
        stree.remove(x)
    stree.extend(children)


def voice_to_html(vtree):
    return [make_node("div", {"class": "voice"}, *row_to_html(vtree.text[row:row + 64], vtree.attrib["name"], vtree.attrib.get("section_name") if not row else None)) for row in range(0, len(vtree.text), 64)]


def row_to_html(notes, name, section_name):
    notes = notes \
        .replace('x', '✕') \
        .replace('_', '·') \
        .replace('b', 'B') \
        .replace('w', 'ж') \
        .replace('.', '•')
    return \
            [make_node("span", {"class": "left"}, section_name if section_name is not None else " ")] \
            + [make_node("span", {"class": "bar"}, notes[bar:bar + 4]) for bar in range(0, len(notes), 4)] \
            + [make_node("span", {"class": "right"}, name)]
