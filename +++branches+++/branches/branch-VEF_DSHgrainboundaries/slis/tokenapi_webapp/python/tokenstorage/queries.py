
__author__ = 'Jerzy Gawad'
__copyright__ = 'KU Leuven'
__status__ = 'Prototype'

# TODO: consider using "named style" in queries:

# Return one or zero records: non-activated token OR empty
# Parameter: {user_id}
_PICK_TOKEN_SQL_STMT = '''
SELECT * FROM Tokens 
WHERE user_id == {user_id} AND activation_date IS NULL AND expiry >= date("now")
LIMIT 1
'''

# Parameter: {user_id}
_COUNT_TOKENS_SQL_STMT = '''
SELECT Count() 
FROM Tokens 
WHERE activation_date IS NULL AND user_id == {user_id} AND expiry >= date("now")
'''

_COUNT_ALL_USERS_TOKENS_SQL_STMT = '''
SELECT Count() 
FROM Tokens 
WHERE user_id == {user_id} AND expiry >= date("now")
'''

# Parameters: {id}, {user_id}
_ACTIVATE_TOKEN_SQL_STMT = '''
UPDATE Tokens 
SET activation_date=date("now") 
WHERE id == {id} AND user_id == {user_id}
'''

# Parameters: {user_key}
_RESOLVE_USERKEY_SQL_STMT = '''
SELECT id from USERS
WHERE key == "{user_key}"
'''


def acquireToken(db, user_id):
    '''
    Retrieve unused token belonging to user_id from the database db.

    Arguments
    ---------
    db: connection to sqlite3 SQL database
    user_id: id of user (owner of token)

    Returns
    -------
    Either: None if no token is available or error conditions were reached,
    OR
    tuple (expiry_date, key, token_data)
    '''
    try:
        with db:
            # Get either one row or None
            row = db.execute(_PICK_TOKEN_SQL_STMT.format(user_id=user_id)).fetchone()
            if row:
                # FIXME: this unpacking highly depends on the schema
                id, _, expiry, _, key, data = row
                db.execute(_ACTIVATE_TOKEN_SQL_STMT.format(id=id, user_id=user_id)).fetchone()
                return (expiry, key, data)
    except sqlite3.OperationalError:
        pass # Silence the error, but the function returns None


def getTokenCount(connection, user_id, total=False):
    '''Get the number of tokens owned by user user_id
    '''
    query = _COUNT_TOKENS_SQL_STMT if not total else _COUNT_ALL_USERS_TOKENS_SQL_STMT
    count = connection.execute(query.format(user_id=user_id)).fetchone()
    return int(0 if count is None else count[0])


def getUserId(connection, user_key):
    '''Resolve user_key to user_id. Returns user_id (int) on success or None 
    if user_key does not exist in the database.
    '''
    query = _RESOLVE_USERKEY_SQL_STMT.format(user_key=user_key)
    user_id = connection.execute(query).fetchone()
    return user_id[0] if user_id else None

