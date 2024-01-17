import re


# According to the problem statement, domain name can contain
# symbols [a-z 0-9_-] -- I assume the space is unwanted here.
# Also, I guess the name shouldn't be empty.


char = r'[a-z0-9_]'
char_or_dash = r'[a-z0-9_-]'
name_char = r'[a-z0-9_.-]'
quoted_char = r'[a-z0-9_.!,:-]'
domain_part = r'{ch}({chdash}*{ch})?'.format(ch=char, chdash=char_or_dash)
# Domain and name regexps do not include length checking.
# Name regexp also doesn't include double-dot checking.
# It's done in final email regexp.
domain = r'({part})(\.({part}))*'.format(part=domain_part)
name = r'({name_char}|\"{quoted_char}*\")*'.format(name_char=name_char,
                                                   quoted_char=quoted_char)
unchecked_email = r'({name})@({domain})'.format(name=name, domain=domain)
checks = r'(?!.*\.\.)(?=.{1,128}@.{3,256}\Z)'
email = r'({checks})({mail})'.format(checks=checks, mail=unchecked_email)

email_regex = re.compile(r'({})\Z'.format(email))


def is_valid_email(text):
    return bool(email_regex.match(text))


__all__ = ['is_valid_email']


if __name__ == '__main__':
    print(email)

