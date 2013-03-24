
from pygments.lexer import RegexLexer, include, bygroups
from pygments.token import *

class JanusLexer(RegexLexer):
    """
    For the reversible programming language Janus.

    """
    name = 'Janus'
    aliases = ['janus']
    filenames = ['*.ja']

    keywords = [
        'if', 'then', 'else', 'fi',
        'from', 'do', 'loop', 'until',
        'push', 'pop',
        'local', 'delocal',
        'skip'
    ]

    operators = r'(\+=?|-=?|\^=?|\*|/|&&?|\|\|?|<=?|>=?|!?=)'

    builtin = [
        'top', 'empty'
    ]

    identifier = '[a-zA-Z][a-zA-Z0-9]*'

    tokens = {
        'whitespace': [
            (r'\s+', Text),
            (r'\(\*', Comment.Multiline, 'comment'),
            (r'#.*?$', Comment.Single),
        ],
        'root': [
            include('whitespace'),

            (r'\b(procedure)(\s+)(%s)' % identifier, bygroups(Keyword, Text, Name.Function)),
            (r'\b(call|uncall)(\s+)(%s)' % identifier, bygroups(Keyword, Text, Name.Function)),
            (r'\b(%s)\b' % '|'.join(keywords), Keyword),
            (r'\b(%s)\b' % '|'.join(builtin), Name.Builtin),
            (r'\b(int|stack)\b', Keyword.Type),
            (r'\b(nil)\b', Name.Constant),
            (operators, Operator),
            (identifier, Name.Variable),
            include('number'),
            (r'[()\[\],]', Punctuation),
            (r'.', Error),
        ],
        'number': [
            (r'[+-]?\d+', Number.Integer),
        ],
        'comment': [
            (r'[^(*)]', Comment.Multiline),
            (r'\(\*', Comment.Multiline, '#push'),
            (r'\*\)', Comment.Multiline, '#pop'),
            (r'[(*)]', Comment.Multiline),
        ],
    }
