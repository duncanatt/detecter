""" Lexer for maxHML script.

Copyright (c) 2022, Duncan Paul Attard <duncanatt@gmail.com>

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <https://www.gnu.org/licenses/>.
"""


import re

from pygments.lexer import RegexLexer, bygroups, words, include
from pygments.token import Generic, Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation

__all__ = ['MaxHmlLexer']

line_re = re.compile('.*?\n')


class MaxHmlLexer(RegexLexer):
    """
    Pygments lexer class for the maxHML scripting language.
    """

    name = 'maxHML'
    aliases = ['maxhml']
    filenames = ['*.maxhml']
    mimetypes = ['text/x-maxhml']

    keywords = (
        'with', 'when', 'monitor',  # Meta operators.
        'max', 'and', 'or', 'ff', 'tt'  # maxHML keywords.
    )

    special_variables_1 = (
      'maxHML', 'P', 'C', 'φ', 'φ₁', 'φₙ', 'φᵢ'
    )

    special_variables_2 = (
      'Act', 'α', 'α₁', 'αₙ', 'αᵢ'
    )

    builtins = (
        'is_alive', 'is_boolean', 'is_builtin', 'is_constant', 'is_function',
        'is_number', 'is_float', 'is_integer', 'is_list', 'is_pid', 'is_port',
        'is_record', 'is_reference', 'is_tuple', 'is_binary', 'is_bitstring',
        'tuple_size', 'bit_size', 'byte_size', 'length', 'map_size'
    )

    operators = r'(\+\+?|--?|\*|/|<|>|/=|=:=|=/=|=<|>=|==?|<-|!|\?)'
    word_operators = (
        'and', 'or', 'andalso', 'orelse', 'not', 'xor',  # Boolean operators.
        'band', 'bor', 'bnot', 'bxor', 'bsl', 'bsr',  # Bitwise operators.
        'div', 'rem'  # Math operators.
    )

    atom_re = r"(?:[a-z]\w*|'[^\n']*[^\\]')"

    variable_re = r'(?:[A-Z_]\w*)'

    esc_char_re = r'[bdefnrstv\'"\\]'
    esc_octal_re = r'[0-7][0-7]?[0-7]?'
    esc_hex_re = r'(?:x[0-9a-fA-F]{2}|x\{[0-9a-fA-F]+\})'
    esc_ctrl_re = r'\^[a-zA-Z]'
    escape_re = r'(?:\\(?:' + esc_char_re + r'|' + esc_octal_re + r'|' + \
                esc_hex_re + r'|' + esc_ctrl_re + r'))'

    macro_re = r'(?:' + variable_re + r'|' + atom_re + r')'

    base_re = r'(?:[2-9]|[12][0-9]|3[0-6])'

    tokens = {
        'root': [
            (words(special_variables_1, suffix=r'\b'), Name.Function),
            (words(special_variables_2, suffix=r'\b'), String.Char),
            # (r'ff', Number.Integer),
            # (r'tt', String.Char),
            # (r'(Act|α|(α₁)|(αₙ)|(αᵢ))', String.Char),
            (r'\(\d+\)', Comment), # Bubble numbering.
            (r'\s+', Text),
            (r'%.*\n', Comment),
            (words(keywords, suffix=r'\b'), Keyword),
            (words(builtins, suffix=r'\b'), Name.Builtin),
            (words(word_operators, suffix=r'\b'), Operator.Word),
            (r'^-', Punctuation, 'directive'),
            (operators, Operator),
            (r'"', String, 'string'),
            (r'<<', Name.Label),
            (r'>>', Name.Label),
            ('(' + atom_re + ')(:)', bygroups(Name.Namespace, Punctuation)),
            ('(?:^|(?<=:))(' + atom_re + r')(\s*)(\()',
             bygroups(Name.Function, Text, Punctuation)),
            (r'[+-]?' + base_re + r'#[0-9a-zA-Z]+', Number.Integer),
            (r'[+-]?\d+', Number.Integer),
            (r'[+-]?\d+.\d+', Number.Float),
            (r'[]\[:_@\".{}()|;,]', Punctuation),
            (variable_re, Name.Variable),
            (atom_re, Name),
            (r'\?' + macro_re, Name.Constant),
            (r'\$(?:' + escape_re + r'|\\[ %]|[^\\])', String.Char),
            (r'#' + atom_re + r'(:?\.' + atom_re + r')?', Name.Label),

            # Erlang script shebang
            (r'\A#!.+\n', Comment.Hashbang),

            # EEP 43: Maps
            # http://www.erlang.org/eeps/eep-0043.html
            (r'#\{', Punctuation, 'map_key'),
        ],
        'string': [
            (escape_re, String.Escape),
            (r'"', String, '#pop'),
            (r'~[0-9.*]*[~#+BPWXb-ginpswx]', String.Interpol),
            (r'[^"\\~]+', String),
            (r'~', String),
        ],
        'directive': [
            (r'(define)(\s*)(\()(' + macro_re + r')',
             bygroups(Name.Entity, Text, Punctuation, Name.Constant), '#pop'),
            (r'(record)(\s*)(\()(' + macro_re + r')',
             bygroups(Name.Entity, Text, Punctuation, Name.Label), '#pop'),
            (atom_re, Name.Entity, '#pop'),
        ],
        'map_key': [
            include('root'),
            (r'=>', Punctuation, 'map_val'),
            (r':=', Punctuation, 'map_val'),
            (r'\}', Punctuation, '#pop'),
        ],
        'map_val': [
            include('root'),
            (r',', Punctuation, '#pop'),
            (r'(?=\})', Punctuation, '#pop'),
        ],
    }
