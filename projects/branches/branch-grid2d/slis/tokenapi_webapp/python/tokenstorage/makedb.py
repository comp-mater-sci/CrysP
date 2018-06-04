
from __future__ import print_function

__author__ = 'Jerzy Gawad'
__copyright__ = 'KU Leuven'
__status__ = 'Prototype'

import sqlite3
import csv
import os

from os import path
from io import open
import tables

# The last field looks like an SHA3-256 hash: [a-z0-9]{64}
USERS = [(1, "Mitsubishi Materials Corporation", "334f5b64c11bcadecd5bf1680ddea626c20c9a2cb07906580944a5a13806dc2e")]

def populate_users(db):
    with db:
        for user in USERS:
            db.execute('INSERT INTO Users VALUES(?,?,?)', user)

def populate_tokens(db, user_id, metafile, dir_path='.'):

    #
    # Insert statement
    #
    # Remaining fields '?' are:
    # expiry TEXT,
    # key   TEXT,
    # data  BLOB,
    INSERT_TOKEN_SQL_STMT = 'INSERT INTO Tokens VALUES(NULL, {user_id}, ?, NULL, ?, ?)'.format(user_id=user_id)

    with open(path.join(dir_path, metafile), 'r', newline='') as inp:
        reader = csv.DictReader(inp)
        for row in reader:
            print(row)
            
            with open(path.join(dir_path, row['fname']), 'rb') as token_file:
                data = token_file.read()
                record = (row['expiry_date'], row['id'], data)
                db.execute(INSERT_TOKEN_SQL_STMT, record)
                db.commit()


def makedb(dbfile, metafile, datadir):
    with sqlite3.connect(dbfile) as connection:
        cursor = connection.cursor()
        cursor.execute('PRAGMA foreign_keys = ON')
    
        tables.setup(connection)
    
        populate_users(connection)
    
        # set up tokens
        for user_id, _, _ in USERS:
            populate_tokens(connection, user_id, metafile, datadir)

if __name__ == '__main__':

    dbfile='database.db'
    metafile = 'tokens.csv'
    datadir = 'testdata'

    makedb(dbfile, metafile, datadir)



