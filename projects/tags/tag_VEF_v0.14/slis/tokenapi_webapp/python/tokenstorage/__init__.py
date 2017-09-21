
import sqlite3


def connect(db_path):
    '''Connect to sqlite database and configure connection. Returns connection.
    '''

    # Dubious trick/workaround: check_same_thread:
    # https://stackoverflow.com/questions/393554/python-sqlite3-and-concurrency
    # The "right" solution would be:
    # http://flask.pocoo.org/docs/0.12/patterns/sqlite3/
    #
    db = sqlite3.connect(db_path, check_same_thread=False)
    db.execute('PRAGMA foreign_keys = ON')
    return db