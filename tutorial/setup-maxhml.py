#!/usr/bin/env python
"""Setup maxhml-lexer.

Copyright (c) 2021, Duncan Paul Attard <duncanatt@gmail.com>

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
from setuptools import setup, find_packages

entry_points = '''
[pygments.lexers]
maxhml=maxhml_lexer:MaxHmlLexer
'''

setup(
    name='maxhml-lexer',
    version='0.9.0',
    description='Pygments lexer package for maxhml-script v0.9+',
    author='Duncan Paul Attard',
    author_email='duncanatt[at]gmail.com',
    url='https://github.com/detecter',
    packages=find_packages(),
    entry_points=entry_points,
    install_requires=[
        'Pygments>=2.3.1'
    ],
    zip_safe=True,
    license='GPL3 License',
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Environment :: Console',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: GPL3 License',
        'Operating System :: OS Independent',
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.2',
        'Programming Language :: Python :: 3.3',
        'Programming Language :: Python :: 3.4',
        'Topic :: Internet :: WWW/HTTP :: Dynamic Content',
        'Topic :: Software Development :: Libraries :: Python Modules',
        'Topic :: Text Processing :: Filters',
        'Topic :: Text Processing :: Markup :: HTML'
    ]
)