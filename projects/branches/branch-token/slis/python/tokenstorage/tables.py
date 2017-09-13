from __future__ import print_function

import sqlite3
from collections import OrderedDict

__author__ = 'Jerzy Gawad'
__copyright__ = 'KU Leuven'
__status__ = 'Prototype'

USERS_SQL_STMT = '''CREATE TABLE Users(
    id     INTEGER PRIMARY KEY,
    name   TEXT NOT NULL,
    key    TEXT UNIQUE NOT NULL)'''

TOKENS_SQL_STMT = '''CREATE TABLE Tokens(
    id     INTEGER PRIMARY KEY,
    user_id INTEGER NOT NULL,
    expiry TEXT NOT NULL,
    activation_date TEXT,
    key   TEXT NOT NULL,
    data  BLOB NOT NULL,
    FOREIGN KEY(user_id) REFERENCES Users(id))
'''

#ACTIVATIONS_SQL_STMT = '''CREATE TABLE Activations(
#    id      INTEGER PRIMARY KEY,
#    token_id INTEGER,
#    activation_date TEXT,
#    FOREIGN KEY(token_id) REFERENCES Tokens(id))
#'''

TABLES = OrderedDict([('Users', USERS_SQL_STMT),
                      ('Tokens', TOKENS_SQL_STMT)])
#                     ('Activations', ACTIVATIONS_SQL_STMT)])

def setup(connection):
    cursor = connection.cursor()
    # Make sure we can drop tables in any order
    cursor.execute('PRAGMA foreign_keys = OFF')
    #
    tables = cursor.execute('SELECT name FROM sqlite_master').fetchall()
    tables = set(t[0] for t in tables)

    # Cleanup & build
    for table, statement in TABLES.items():
        if table in tables:
            cursor.execute('DROP TABLE {}'.format(table))
        cursor.execute(statement)
    connection.commit()
    cursor.execute('PRAGMA foreign_keys = ON')