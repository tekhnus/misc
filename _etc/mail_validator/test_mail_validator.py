import unittest
import re
from mail_validator import domain, name, is_valid_email


domain_regex = re.compile(r'({})\Z'.format(domain))
name_regex = re.compile(r'({})\Z'.format(name))


def is_valid_domain(text):
    return bool(domain_regex.match(text))


def is_valid_name(text):
    return bool(name_regex.match(text))


class TestDomainValidator(unittest.TestCase):

    def test_border_dash_fails(self):
        self.assertFalse(is_valid_domain('one.two-.three'))
        self.assertFalse(is_valid_domain('one.-two.three'))
        self.assertFalse(is_valid_domain('one.-.three'))

    def test_inner_dash_accepted(self):
        self.assertTrue(is_valid_domain('one.two-three'))
        self.assertTrue(is_valid_domain('one.two--three'))

    def test_empty_domain_parts_fails(self):
        self.assertFalse(is_valid_domain('.'))
        self.assertFalse(is_valid_domain('.foo'))
        self.assertFalse(is_valid_domain('bar.'))
        self.assertFalse(is_valid_domain('hello..world'))

    def test_quotes_fails(self):
        self.assertFalse(is_valid_domain('"hello"'))

    def test_valid_accepted(self):
        self.assertTrue(is_valid_domain('hello.world'))
        self.assertTrue(is_valid_domain('hello-world'))
        self.assertTrue(is_valid_domain('hello_world'))
        self.assertTrue(is_valid_domain('hello0world'))


class TestNameValidator(unittest.TestCase):

    def test_odd_quote_number_fails(self):
        self.assertFalse(is_valid_name('"'))
        self.assertFalse(is_valid_name('"""'))

    def test_specials_accepted_in_quotes_only(self):
        for s in '!,:':
            self.assertTrue(is_valid_name('"bang{s}"'.format(s=s)))
            self.assertTrue(is_valid_name('"bang{s}""bang{s}"'.format(s=s)))
            self.assertFalse(is_valid_name('"bang{s}"{s}"bang{s}"'.format(s=s)))

    def test_valid_accepted(self):
        self.assertTrue(is_valid_name('alex'))
        self.assertTrue(is_valid_name('alex"!"'))
        self.assertTrue(is_valid_name('dot.dot.dot'))
        self.assertTrue(is_valid_name('x'))


class TestMailValidator(unittest.TestCase):

    def test_empty_fails(self):
        self.assertFalse(is_valid_email(''))

    def test_empty_domain_or_name_fails(self):
        self.assertFalse(is_valid_email('@'))
        self.assertFalse(is_valid_email('@example.com'))
        self.assertFalse(is_valid_email('alex@'))

    def test_long_fails(self):
        self.assertTrue(is_valid_email('u' * 128 + '@' + 'v' * 256))
        self.assertFalse(is_valid_email('u' * 128 + '@' + 'v' * 257))
        self.assertFalse(is_valid_email('u' * 129 + '@' + 'v' * 256))

    def test_short_fails(self):
        self.assertTrue(is_valid_email('z@x.y'))
        self.assertTrue(is_valid_email('z@xoy'))
        self.assertFalse(is_valid_email('z@xy'))

    def test_no_ats_fails(self):
        self.assertFalse(is_valid_email('useraexample.com'))

    def test_multiple_ats_fails(self):
        self.assertFalse(is_valid_email('@@@@@@@@'))
        self.assertFalse(is_valid_email('g@@gle.ru'))
        self.assertFalse(is_valid_email('alex@nder@gmail.com'))
        self.assertFalse(is_valid_email('wwww@@@@@@'))

    def test_double_dots_fails(self):
        self.assertFalse(is_valid_email('..@example'))
        self.assertFalse(is_valid_email('a..def@example'))
        self.assertFalse(is_valid_email('abcdef@ex..ple'))
        self.assertFalse(is_valid_email('..cdef@example'))
        self.assertFalse(is_valid_email('abcd..@example'))

    def test_valid_accepted(self):
        self.assertTrue(is_valid_email('z.pilipchuk@gmail.com'))
        self.assertTrue(is_valid_email('"hel:lo!"@wo-rld'))
        self.assertTrue(is_valid_email('"a!"and":b"@x.y'))


if __name__ == '__main__':
    unittest.main()
